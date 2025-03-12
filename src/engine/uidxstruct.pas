unit uidxstruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIdxStruct = class;

  TIdxEntry = class
    private
      iName: String;
      iNumber: Word;
      iSrf: Word;
      iSubOffset: Integer;
    public
      property Name: String read iName write iName;
      property FileNumber: Word read iNumber write iNumber;
      property LinkedSrf: Word read iSrf write iSrf;
      property SubOffset: Integer read iSubOffset write iSubOffset;
    end;

  TIdxStruct = class
    private
      fList: TList;
      fSrcFileName: TFileName;
      fSrfCount: Word;
      function GetCount: Integer;
      function GetEntry(Index: Integer): TIdxEntry;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(var IdxEntry: TIdxEntry);
      procedure LoadFromFile(const FileName: TFileName);
      procedure SaveToFile(const FileName: TFileName);
      procedure CopyTo(var IdxStruct: TIdxStruct);
      property FileName: TFileName read fSrcFileName write fSrcFileName;
      property Count: Integer read GetCount;
      property SrfCount: Word read fSrfCount write fSrfCount;
      property Items[Index: Integer]: TIdxEntry read GetEntry; Default;
      procedure Delete(const Index: Integer);
      procedure Clear;
  end;

const
  _SizeOf_Integer = SizeOf(Integer);
  _SizeOf_Word = SizeOf(Word);

implementation

constructor TIdxStruct.Create;
begin
  fList := TList.Create;
  fSrfCount := 0;
end;

destructor TIdxStruct.Destroy;
var
  i: Integer;
begin
  for i:=0 to fList.Count-1 do begin
    TIdxEntry(fList[i]).Free;
  end;
  fList.Free;
  inherited;
end;

procedure TIdxStruct.Add(var IdxEntry: TIdxEntry);
begin
  fList.Add(IdxEntry);
end;

function TIdxStruct.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TIdxStruct.GetEntry(Index: Integer): TIdxEntry;
begin
  Result := TIdxEntry(fList[Index]);
end;

procedure TIdxStruct.Delete(const Index: Integer);
begin
  TIdxEntry(fList[Index]).Free;
  fList.Delete(Index);
end;

procedure TIdxStruct.Clear;
var
  i: Integer;
begin
  for i:=0 to fList.Count-1 do begin
    TIdxEntry(fList[i]).Free;
  end;
  fList.Clear;
  fSrcFileName := '';
end;

procedure TIdxStruct.CopyTo(var IdxStruct: TIdxStruct);
var
  i: Integer;
  inEntry, outEntry: TIdxEntry;
begin
  //Copy one structure to another... not pointing...
  IdxStruct.fSrcFileName := fSrcFileName;
  IdxStruct.fSrfCount := fSrfCount;

  for i:=0 to Count-1 do begin
    inEntry := Items[i];
    outEntry := TIdxEntry.Create;

    outEntry.iName := inEntry.iName;
    outEntry.iNumber := inEntry.iNumber;
    outEntry.iSrf := inEntry.iSrf;
    outEntry.iSubOffset := inEntry.iSubOffset;
    IdxStruct.Add(outEntry);
  end;
end;

procedure TIdxStruct.LoadFromFile(const FileName: TFileName);
const
  IDX_SIGN = 'IDX0';
var
  F: File;
  IdxEntry: TIdxEntry;
  strBuf: String;
  i: Integer;
  eCount: Word;
begin
  Clear;
  fSrcFileName := FileName; //Storing source filename

  //Opening source file
  AssignFile(F, FileName);
  {$I-}Reset(F, 1);{$I+}
  if IOResult <> 0 then Exit;

  //Reading file header
  SetLength(strBuf, 4);
  BlockRead(F, Pointer(strBuf)^, Length(strBuf));
  if strBuf = IDX_SIGN then begin
    BlockRead(F, fSrfCount, _SizeOf_Word); //Entry count with SRF
    BlockRead(F, eCount, _SizeOf_Word); //Entry count without SRF
    Dec(fSrfCount, eCount);

    Seek(F, FilePos(F)+12); //Seeking to entries list

    for i:=0 to eCount-1 do begin
      IdxEntry := TIdxEntry.Create;

      //Entry name
      SetLength(strBuf, 12);
      BlockRead(F, Pointer(strBuf)^, Length(strBuf));
      IdxEntry.Name := Trim(strBuf);

      BlockRead(F, IdxEntry.iNumber, _SizeOf_Word); //File number
      BlockRead(F, IdxEntry.iSrf, _SizeOf_Word); //Linked SRF number
      BlockRead(F, IdxEntry.iSubOffset, _SizeOf_Integer); //Offset in SRF
      Add(IdxEntry);
    end;
  end;

  CloseFile(F);
end;

procedure TIdxStruct.SaveToFile(const FileName: TFileName);
var
  F: File;
  IdxEntry: TIdxEntry;
  strBuf: String;
  i: Integer;
  wordBuf: Word;

  function TruncateStr(const Str: string; const StrLength: Integer): String;
  var
    strBuf: String;
  begin
    if Length(Str) <= StrLength then begin
      Result := Str;
    end
    else begin
      strBuf := Str;
      System.Delete(strBuf, StrLength+1, Length(strBuf)-StrLength);
      Result := strBuf;
    end;
  end;

  procedure WritePadding(var F_out: File; PaddingSize: Integer);
  const
    WORK_BUFFER_SIZE = 16384;
  var
    Buf: array[0..WORK_BUFFER_SIZE-1] of Byte;
    i, j, BufSize: Integer;
    _Last_BufSize_Entry: Integer;
  begin
    FillByte(Buf, SizeOf(Buf), 0);
    BufSize := SizeOf(Buf);
    _Last_BufSize_Entry := PaddingSize mod BufSize;
    j := PaddingSize div BufSize;
    for i:=0 to j-1 do begin
      BlockWrite(F_out, Buf, SizeOf(Buf), BufSize);
    end;
    BlockWrite(F_out, Buf, _Last_BufSize_Entry);
  end;
begin
  //Creating the new file
  AssignFile(F, FileName);
  ReWrite(F, 1);

  //Writing header
  strBuf := 'IDX0';
  BlockWrite(F, Pointer(strBuf)^, Length(strBuf));

  //Entry count
  wordBuf := fSrfCount+Count;
  BlockWrite(F, wordBuf, _SizeOf_Word);
  wordBuf := Count;
  BlockWrite(F, wordBuf, _SizeOf_Word);

  //Padding before first file
  WritePadding(F, 12);

  //Indexed files list
  for i:=0 to Count-1 do begin
    IdxEntry := Items[i];
    strBuf := TruncateStr(IdxEntry.iName, 12);
    BlockWrite(F, Pointer(strBuf)^, Length(strBuf)); //Filename
    WritePadding(F, 12-Length(strBuf));

    BlockWrite(F, IdxEntry.iNumber, _SizeOf_Word); //File number
    BlockWrite(F, IdxEntry.iSrf, _SizeOf_Word); //SRF file number
    BlockWrite(F, IdxEntry.iSubOffset, _SizeOf_Integer); //Subtitle offset
  end;

  CloseFile(F);
end;

end.

