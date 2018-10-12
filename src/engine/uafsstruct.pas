unit uafsstruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSrfStruct = class;

  TSrfEntry = class
    private
      sName: String;
      sText: String;
      sData: TMemoryStream;
      sOffset: Integer;
    public
      property CharName: String read sName write sName;
      property Text: String read sText write sText;
      property Data: TMemoryStream read sData
      property Offset: Integer read sOffset
  end;

  TSrfStruct = class
    private
      fSrcFileName: TFileName;
      fList: TList;
      function GetCount: Integer;
      function GetEntry(Index: Integer): TSrfEntry;
      procedure LoadFile(const FileName: TFileName; const Offset, Size: Integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile(const FileName: TFileName); overload;
      procedure LoadFromFile(const FileName: TFileName; const Offset, Size: Integer); overload;
      procedure Add(var SrfEntry: TSrfEntry);
      procedure SaveToFile(const FileName: TFileName);
      property FileName: TFileName read fSrcFileName write fSrcFileName;
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TSrfEntry read GetEntry; Default;
      procedure Delete(const Index: Integer);
      procedure Clear;
  end;

const
  _SizeOf_Integer = SizeOf(Integer);
  _SizeOf_Byte = SizeOf(Byte);

implementation

constructor TSrfStruct.Create;
begin
  fList := TList.Create;
end;

destructor TSrfStruct.Destroy;
var
  i: Integer;
begin
  for i := 0 to fList.Count-1 do begin
    TSrfEntry(fList[i]).sData.Free;
    TSrfEntry(fList[i]).Free;
  end;
  fList.Free;
  inherited;
end;

procedure TSrfStruct.Add(var SrfEntry: TSrfEntry);
begin
  fList.Add(SrfEntry);
end;

procedure TSrfStruct.Delete(const Index: Integer);
begin
  TSrfEntry(fList[Index]).sData.Free;
  TSrfEntry(fList[Index]).Free;
  fList.Delete(Index);
end;

procedure TSrfStruct.Clear;
var
  i: Integer;
begin
  for i:=0 to fList.Count-1 do begin
    TSrfEntry(fList[i]).sData.Free;
    TSrfEntry(fList[i]).Free;
  end;
  fList.Clear;
  fSrcFileName := '';
end;

function TSrfStruct.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TSrfStruct.GetEntry(Index: Integer): TSrfEntry;
begin
  Result := TSrfEntry(fList[Index]);
end;

procedure TSrfStruct.LoadFromFile(const FileName: TFileName);
begin
  LoadFile(FileName, -1, -1);
end;

procedure TSrfStruct.LoadFromFile(const FileName: TFileName; const Offset, Size: Integer);
begin
  LoadFile(FileName, Offset, Size);
end;

procedure TSrfStruct.LoadFile(const FileName: TFileName; const Offset, Size: Integer);
var
  F: File;
  SrfEntry: TSrfEntry;
  i, intBuf, fSize, fOffset: Integer;
  strBuf: String;
  Buf: Byte;
begin
  Clear;
  fSrcFileName := FileName; //Keeping filename

  //Opening file
  AssignFile(F, FileName);
  {$I-}Reset(F, 1);{$I+}
  if IOResult <> 0 then Exit;

  if (Offset <> -1) and (Size <> -1) then begin
    fOffset := Offset;
    fSize := Size;
  end
  else begin
    fOffset := 0;
    fSize := FileSize(F);
  end;

  Seek(F, fOffset);

  while FilePos(F) <> (fOffset+fSize) do begin
      //Reading first value
      BlockRead(F, intBuf, _SizeOf_Integer);
      if intBuf = 8 then begin
        SrfEntry := TSrfEntry := TSrfEntry.Create;
        SrfEntry.sOffset := (FilePos(F)-fOffset)-4;

        //Reading filename
        SetLength(strBuf, intBuf-4);
        BlockRead(F, Pointer(strBuf)^, intBuf-4);
        SrfEntry.CharName :=  Trim(strBuf);

        //Reading subtitle length
        //If length is 4, there's no text
        strBuf := '';
        BlockRead(F, intBuf, _SizeOf_Integer);
        if intBuf > 4 then begin
          SetLength(strBuf, intBuf-4);
          BlockRead(F, Pointer(strBuf)^, intBuf-4);
        end;
        SrfEntry.Text := Trim(strBuf);

        //Initializing memory stream and ready data
        SrfEntry.sData := TMemoryStream.Create;
        BlockRead(F, intBuf, _SizeOf_Integer); //Data size
        for i:=0 to (intBuf-4)-1 do begin
          BlockRead(F, Buf, _SizeOf_Byte);
          SrfEntry.sData.Write(Buf, _SizeOf_Byte); //Copy to MemoryStream
        end;
        Add(SrfEntry);
      end
      else begin
          Seek(F, FilePos(F)-3);
      end;
  end;

  CloseFile(F);
end;

procedure TSrfStruct.SaveToFile(const FileName: TFileName);
var
  F: File;
  SrfEntry: TSrfEntry;
  i, j, intBuf, subTotal, bSize, bNum: Integer;
  Buf: Byte;

  function NullByteLength(const DataSize: Integer): Integer;
  var
    currNum, totalNull: Integer;
  begin
    //Text padding for Shenmue 1/2 SRF
    currNum := 0;
    totalNull := 4;
    while currNum <> DataSize do begin
      if totalNull = 1 then begin
        totalNull := 4;
      end
      else begin
        Dec(totalNull);
      end;
      Inc(currNum);
    end;
    Result := totalNull;
  end;

  procedure WritePadding(var F_out: File; PaddingSize: Integer);
  const
    WORK_BUFFER_SIZE = 16384;
  var
    Buf: array[0..WORK_BUFFER_SIZE-1] of Byte;
    i, j, bufSize: Integer;
    _Last_bufSize_Entry: Integer;
  begin
    FillByte(Buf, SizeOf(Buf), 0);
    bufSize := SizeOf(Buf);
    _Last_bufSize_Entry := PaddingSize mod bufSize;
    j := PaddingSize div bufSize;
    for i := 0 to j - 1 do begin
      BlockWrite(F_out, Buf, SizeOf(Buf), bufSize);
    end;
    BlockWrite(F_out, Buf, _Last_bufSize_Entry);
  end;
begin
  //Assigning output file
  AssignFile(F, FileName);
  ReWrite(F, 1);

  bNum := 1;
  bSize := 2048;

  for i:=0 to Count-1 do begin
    SrfEntry := Items[i];

    //Calculating section total size
    subTotal := 0;
    Inc(subTotal, 12+Length(SrfEntry.sName));
    Inc(subTotal, Length(SrfEntry.sText)+SrfEntry.sData.Size);
    if Length(SrfEntry.sText) > 0 then begin
      Inc(subTotal, NullByteLength(Length(SrfEntry.sText)));
    end;

    //Verifying remaining space in the current block
    if (FileSize(F)+subTotal) > (bSize*bNum) then begin
      WritePadding(F, (bSize*bNum)-FileSize(F));
    end;

    //Writing character name
  end;
end;

end.

