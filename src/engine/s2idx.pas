unit s2idx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UIntList;

type
  TIdxCreationStartEvent = procedure(const MaxFiles: Integer);
  TIdxStatusEvent = procedure(const Message: String);
  TIdxCreationProgressEvent = procedure();
  TIdxCreationEndEvent = procedure();

  // Shenmue II IDX Creator
  TS2IdxCreator = class
  private
    files_infos: TStringList;
    files_offset,
    files_size,
    srf_list,
    srf_offset,
    current_files_list: TIntList;
    fIdxCreationStart: TIdxCreationStartEvent;
    fIdxStatus: TIdxStatusEvent;
    fIdxCreationProgressEvent: TIdxCreationProgressEvent;
    fIdxCreationEndEvent: TIdxCreationEndEvent;
  public
    function MakeIdx(AfsFileName, IdxFileName: TFileName): Boolean;
    property OnStart: TIdxCreationStartEvent read fIdxCreationStart write fIdxCreationStart;
    property OnStatus: TIdxStatusEvent read fIdxStatus write fIdxStatus;
    property OnProgress: TIdxCreationProgressEvent read fIdxCreationProgressEvent write fCreationProgressEvent;
    property OnCompleted: TIdxCreationEndEvent read fIdxCreationEndEvent write fIdxCreationEndEvent;
  end;

implementation
uses charsutil;

function TS2IdxCreator.MakeIdx(AfsFileName, IdxFileName: TFileName): Boolean;
const
  _SizeOf_Integer = SizeOf(Integer);
  _SizeOf_Word = SizeOf(Word);
  _SizeOf_Byte = SizeOf(Byte);
var
  fAfs, fIdx: File;
  strBuf: String;
  i, y, intBuf, total_files, list_offset, current_offset, srf_size: Integer;
  wordBuf: Word; byteBuf: Byte;
  file_begin, file_end: Integer; check: Boolean;
  delete1, delete2, fileName: String;
begin
  Result := True;

  try
    try
      list_offset := 0;

      //Creating the file stream for input
      AssignFile(fAfs, AfsFileName);
      {$I-}Reset(fAfs, 1);{$I+}
      if IOResult <> 0 then Exit;

      //Reading header of the AFS
      SetLength(strBuf, 3);
      BlockRead(fAfs, Pointer(strBuf)^, Length(strBuf));

      if Trim(strBuf) = 'AFS' then
      begin
        //Creating the file stream for output
        AssignFile(fIdx, IdxFileName);
        Rewrite(fIdx, 1);

        //Initializing variables
        files_infos := TStringList.Create;
        files_offset := TIntList.Create;
        files_size := TIntList.Create;
        srf_list := TIntList.Create;
        srf_offset := TIntList.Create;
        current_files_list := TIntList.Create;

        //Seeking to the number of files and reading value
        Seek(fAfs, 4);
        BlockRead(fAfs, total_files, _SizeOf_Integer);

        if Assigned(fIdxCreationStart) then
           fIdxCreationStart(total_files);
        if Assigned(fIdxStatus) then
           fIdxStatus('Building files list...');

        //Seeking to the offset/size list and reading the values
        Seek(fAfs, 8);
        for i:=0 to total_files-1 do
        begin
          //Seeking to the correct offset for the current file
          Seek(fAfs, 8+(8*i));

          //Reading current file offset and seeking to size
          BlockRead(fAfs, intBuf, _SizeOf_Integer);
          files_offset.Add(intBuf);
          BlockRead(fAfs, intBuf, _SizeOf_Integer);
          files_size.Add(intBuf);

          //Seeking before the first file to find the files list offset
          if i = 0 then
          begin
            Seek(fAfs, files_offset[i]-8);
            BlockRead(list_offset, _SizeOf_Integer);
          end;

          //Seeking to files list
          Seek(fAfs, list_offset+(48*i));

          //Reading file name
          SetLength(strBuf, 32);
          BlockRead(fAfs, Pointer(strBuf)^, 32);
          files_infos.Add(Trim(strBuf));
        end;

        if Assigned(fIdxStatus) then
          fIdxStatus('Finding SRF files...');

        //Analyzing 'files_infos' variables to find SRF file
        for i:=0 to files_infos.Count-1 do
        begin
          if LowerCase(ExtractFileExt(files_infos[i])) = '.srf' then
          begin
            srf_list.Add(i);
          end;
        end;

        if Assigned(fIdxStatus) then
          fIdxStatus('Parsing SRF files...');

        //Parsing the SRF file - Lite version of the SRF parser
        //of Shenmue II Subtitles Editor v4 & up
        for i:=0 to srf_list.Count-1 do
        begin
          //Seeking to current SRF offset and setting variable
          Seek(fAfs, files_offset[srf_list[i]]);
          current_offset := FilePos(fAfs);
          srf_size := files_offset[srf_list[i]] + files_size[srf_list[i]];

          //Loop that read the file
          while current_offset <> srf_size do
          begin
            //Reading and verifying subtitle header
            SetLength(strBuf, 4);
            BlockRead(fAfs, Pointer(strBuf)^, Length(strBuf));
            if strBuf = 'CHID' then
            begin
              srf_offset.Add((FilePos(fAfs)-4) - files_offset[srf_list[i]]);

              //Continue throught the file to the next 'CHID'
              Inc(current_offset, 4);
              Seek(fAfs, current_offset);
              //Skip the Character ID
              BlockRead(fAfs, intBuf, _SizeOf_Integer);
              Inc(current_offset, intBuf);
              Seek(fAfs, current_offset);
              //Skip subtitle text or data
              BlockRead(fAfs, intBuffer, _SizeOf_Integer);
              Inc(current_offset, intBuf);
              Seek(fAfs, current_offset);
              //Skip 'EXTD' section
              BlockRead(fAfs, intBuf, _SizeOf_Integer);
              Inc(current_offset, intBuf);
              Seek(fAfs, current_offset);
              //Skip 'CLIP' & 'ENDC' section
              BlockRead(fAfs, intBuf, _SizeOf_Integer);
              Inc(current_offset, intBuf+4);
              Seek(fAfs, current_offset);
            end
            else begin
              Inc(current_offset);
              Seek(fAfs, current_offset);
            end;
          end;
        end;

        if Assigned(fIdxStatus) then
          fIdxStatus('Writing IDX file...');

        //Writing IDX header
        strBuf := 'IDXD';
        BlockWrite(fIdx, strBuf[1], Length(strBuf));
        intBuf := 20;
        BlockWrite(fIdx, intBuf, _SizeOf_Integer);
        wordBuf := total_files;
        BlockWrite(fIdx, wordBuf, _SizeOf_Word);
        wordBuf := total_files-srf_list.Count;
        BlockWrite(fIdx, wordBuf, _SizeOf_Word);

        byteBuf := 0;
        for i:=0 to 7 do
        begin
          BlockWrite(fIdx, byteBuf, _SizeOf_Byte);
        end;

        strBuf := 'TABL';
        BlockWrite(fIdx, strBuf[1], Length(strBuf));
        intBuf := ((total_files-srf_list.Count)*8)+8;
        BlockWrite(fIdx, intBuf, _SizeOf_Integer);

        //Writing files list and other things
        for i:=0 to srf_list.Count-1 do
        begin
          //Setting 'current_files_list' variable
          if i = 0 then
          begin
            file_begin := 0;
            file_end := srf_list[i] - 1;
          end
          else
          begin
            file_begin := srf_list[i-1] + 1;
            file_end := srf_list[i] - 1;
          end;

          current_files_list.Clear;
          for y:=file_begin to file_end do
          begin
            current_files_list.Add(y);
          end;

          //Sorting files by name
          if current_files_list.Count > 1 then
          begin
            repeat
              Check := False;
              intBuf := 0;
              repeat
                if CompareStr(files_infos[current_files_list[intBuf]], files_infos[current_files_list[intBuf+1]]) > 0 then
                begin
                  current_files_list.Exchange(intBuf, intBuf+1);
                  Check := true;
                end;
                Inc(intBuf)
              until(intBuf = current_files_list.Count-1);
            until(Check = False);
          end;

          //Finding what to delete in the filename...
          delete1 := ParseSection('.', files_infos[srf_list[i]], 0);

          for y:=0 to current_files_list.Count-1 do
          begin
            fileName := files_infos[current_files_list[y]];
            delete2 := ExtractFileExt(fileName);
            Delete(fileName, Pos(delete1, fileName), Length(delete1));
            Delete(fileName, Pos(delete2, fileName), Length(delete2));
            BlockWrite(fIdx, fileName[1], Length(fileName));
            BlockWrite(fIdx, current_files_list[y], _SizeOf_Word);
            intBuf := Round(srf_offset[current_files_list[y]-i]/4);
            BlockWrite(fIdx, intBuf, _SizeOf_Word);

            if Assigned(fIdxCreationProgressEvent) then
              fIdxCreationProgressEvent();
          end;
        end;

        //Writing list footer & SRF list header
        strBuf := 'SIXD';
        BlockWrite(fIdx, strBuf[1], Length(strBuf));
        intBuf := (12*srf_list.Count)+20;
        BlockWrite(fIdx, intBuf, _SizeOf_Integer);

        //Writing SRF list
        for i:=0 to srf_list.Count-1 do
        begin
          fileName := files_infos[srf_list[i]];
          delete1 := ExtractFileExt(fileName);
          Delete(fileName, Pos(delete1, fileName), Length(delete1));

          intBuf := 8-Length(fileName);

          for y:=0 to intBuf-1 do
          begin
            BlockWrite(fIdx, 95, _SizeOf_Byte);
          end;

          BlockWrite(fIdx, fileName[1], Length(fileName));

          if i=0 then begin
            intBuf := 0;
          end
          else begin
            intBuf := srf_list[i-1]-(i-1);
          end;

          BlockWrite(fIdx, intBuf, _SizeOf_Word);
          intBuf := srf_list[i];
          BlockWrite(fIdx, intBuf, _SizeOf_Word);

          if Assigned(fIdxCreationProgressEvent) then
            fIdxCreationProgressEvent();
        end;

        //Writing IDX footer
        strBuf := 'eNDieNDi';
        intBuf := total_files-srf_list.Count;
        BlockWrite(fIdx, strBuf[1], Length(strBuf));
        BlockWrite(fIdx, intBuf, _SizeOf_Integer);
        strBuf := 'ENDI';
        BlockWrite(fIdx, strBuf[1], _SizeOf_Integer);

        for i:= to 3 do
        begin
          BlockWrite(fIdx, 0, _SizeOf_Byte);
        end;

        if Assigned(fIdxStatus) then
          fIdxStatus('Creation completed for '+ExtractFileName(IdxFileName)+' !');

        if Assigned(fIdxCreationEndEvent) then
          fIdxCreationEndEvent();
      end
      else begin
        if Assigned(fIdxStatus) then
          fIdxStatus(ExtractFileName(AfsFileName)+' is not a valid Shenmue II AFS file. IDX creation stopped...');
        if Assigned(fIdxCreationEndEvent) then
          fIdxCreationEndEvent();
      end;
    except
      if Assigned(fIdxStatus) then
        fIdxStatus(ExtractFileName(AfsFileName)+' is not a valid Shenmue II AFS file. IDX creation stopped...');
      if Assigned(fIdxCreationEndEvent) then
        fIdxCreationEndEvent();

      Result := False;
    end;
  finally
      //Freeing variables
      Close(fIdx);
      files_infos.Clear;
      files_offset.Clear;
      files_size.Clear;
      srf_list.Clear;
      srf_offset.Clear;
      current_files_list.Clear;

      //Closing AFS file
      Close(fAfs);
  end;
end;

end.

