//
// This unit is part of the GLScene Project, http://glscene.org
//
{ @HTML ( GLFileZLIB

  @HTML (
  <p>
   ZLib compression support for TDGLArchiveManager
  </p>

  <p>
  <b>History: </b><font size=-1><ul>
  <li>01/01/16 - JD - Imported from GLScene
  </ul></font></p>
  )
}

unit DGLFileZLIB;

{$I DGLEngine.inc}

interface

uses
  System.Classes, System.SysUtils, System.AnsiStrings,System.ZLib,

  DGLSArchiveManager;

const
   SIGN = 'ZLIB'; //Signature for compressed zlib.

Type
   TZLibHeader = record
      Signature: array[0..3] of AnsiChar;
      DirOffset: integer;
      DirLength: integer;
   end;

   TFileSection = record
      FileName: array[0..119] of AnsiChar;
      FilePos: integer;
      FileLength: integer;
      CbrMode: TCompressionLevel;
   end;

  { TDGLZLibArchive }

  TDGLZLibArchive=class(TDGLBaseArchive)
    private
      FHeader: TZLibHeader;
      FStream: TFileStream;

      function GetContentCount: integer;
      procedure MakeContentList;
    public
      property ContentCount: integer Read GetContentCount;
      destructor Destroy; override;

      procedure LoadFromFile(const FileName: string); override;

      procedure Clear; override;
      function ContentExists(ContentName: string): boolean;override;

      function GetContent(aStream: TStream; index: integer): TStream; override;
      function GetContent(index: integer): TStream; override;
      function GetContent(ContentName: string): TStream; override;

      function GetContentSize(index: integer): integer; override;
      function GetContentSize(ContentName: string): integer; override;

      procedure AddFromStream(ContentName, Path: string; FS: TStream);override;
      procedure AddFromFile(FileName, Path: string); override;

      procedure RemoveContent(index: integer); overload; override;
      procedure RemoveContent(ContentName: string); overload;override;

      procedure Extract(index: integer; NewName: string); override;
      procedure Extract(ContentName, NewName: string); override;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

var
   Dir: TFileSection;

{ TDGLZLibArchive }


function TDGLZLibArchive.GetContentCount: integer;
begin
   Result := FHeader.DirLength div SizeOf(TFileSection);
end;

procedure TDGLZLibArchive.MakeContentList;
var
   I: integer;
begin
   FStream.Seek(FHeader.DirOffset, soFromBeginning);
   FContentList.Clear;
   for i := 0 to ContentCount - 1 do
   begin
      FStream.ReadBuffer(Dir, SizeOf(TFileSection));
      FContentList.Add(string(Dir.FileName));
   end;
end;

destructor TDGLZLibArchive.Destroy;
begin
  inherited Destroy;
end;

procedure TDGLZLibArchive.LoadFromFile(const FileName: string);
begin
   FFileName := FileName;
    FStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
   if FStream.Size = 0 then
   begin
    FHeader.Signature := SIGN;
    FHeader.DirOffset := SizeOf(TZLibHeader);
    FHeader.DirLength := 0;

    FStream.WriteBuffer(FHeader, SizeOf(TZlibHeader));
    FStream.Position := 0;
   end;

   FStream.ReadBuffer(FHeader, SizeOf(TZlibHeader));
   if FHeader.Signature <> SIGN    then
   begin
      FreeAndNil(FStream); // nil it too to avoid own Clear() giving AV
      raise Exception.Create(FileName+' - This is not ZLIB file');
      Exit;
   end;
   if ContentCount <> 0 then
      MakeContentList;
end;

procedure TDGLZLibArchive.Clear;
begin
   FContentList.Clear;
   If FStream <> nil then FStream.Free;
end;

function TDGLZLibArchive.ContentExists(ContentName: string): boolean;
begin
   Result := (FContentList.IndexOf(ContentName) > -1);
end;

function TDGLZLibArchive.GetContent(aStream: TStream; index: integer): TStream;
var
  tempStream: TMemoryStream;
  decompr : TZDecompressionStream;
begin
      Result := nil;
      If FStream = nil then exit;
      Result := aStream;

      //Find file
      FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
      FStream.Read(Dir, SizeOf(TFileSection));
      FStream.Seek(Dir.FilePos, soFromBeginning);

      //Copy file to temp stream
      tempStream := TMemoryStream.Create;
      tempStream.CopyFrom(FStream, Dir.FileLength);
      tempStream.Position := 0;

      //decompress
       decompr := TZDecompressionStream.Create(tempStream);
       try
         Result.CopyFrom(decompr, 0);
       finally
        decompr.Free;
         tempStream.Free;
       end;
      Result.Position := 0;
end;

function TDGLZLibArchive.GetContent(index: integer): TStream;
begin
   Result:=GetContent(TMemoryStream.Create,index);
end;

function TDGLZLibArchive.GetContent(ContentName: string): TStream;
begin
   Result := nil;
   if ContentExists(ContentName) then
      Result := GetContent(FContentList.IndexOf(ContentName));
end;

function TDGLZLibArchive.GetContentSize(index: integer): integer;
begin
   Result := -1;
   If FStream = nil then exit;
   FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
   FStream.Read(Dir, SizeOf(Dir));
   Result := Dir.FileLength;
end;

function TDGLZLibArchive.GetContentSize(ContentName: string): integer;
begin
   Result := -1;
   if ContentExists(ContentName) then
      Result := GetContentSize(FContentList.IndexOf(ContentName));
end;

procedure TDGLZLibArchive.AddFromStream(ContentName, Path: string; FS: TStream);
var
   Temp, compressed: TMemoryStream;
   FCompressor: TZCompressionStream;
   LCompLevel : TZCompressionLevel;
begin
   if (FStream = nil) or ContentExists(ContentName) then exit;

   FStream.Position := FHeader.DirOffset;
   //???
   if FHeader.DirLength > 0 then
   begin
      Temp := TMemoryStream.Create;
      Temp.CopyFrom(FStream, FHeader.DirLength);
      Temp.Position    := 0;
      FStream.Position := FHeader.DirOffset;
   end
   else
     Temp := nil;
   Dir.FilePos := FHeader.DirOffset;
   Dir.CbrMode := compressionLevel;

   compressed := TMemoryStream.Create;

   // Archive data to stream
   case CompressionLevel of
     clNone    : LCompLevel := zcNone;
     clFastest : LCompLevel := zcFastest;
     clMax     : LCompLevel := zcMax;
   else
     LCompLevel := zcDefault;
   end;
   FCompressor := TZCompressionStream.Create(compressed, LCompLevel, 15);

   FCompressor.CopyFrom(FS,   FS.Size);
   FCompressor.Free;

   // Copy results
   FStream.CopyFrom(compressed, 0);
   Dir.FileLength := compressed.Size;
   Compressed .Free;

   // ???
   FHeader.DirOffset := FStream.Position;
   if FHeader.DirLength > 0 then
   begin
      FStream.CopyFrom(Temp, 0);
      Temp.Free;
   end;
   System.AnsiStrings.StrPCopy(Dir.FileName, AnsiString(Path + ExtractFileName(ContentName)));
   FStream.WriteBuffer(Dir, SizeOf(TFileSection));
   FHeader.DirLength := FHeader.DirLength + SizeOf(TFileSection);
   FStream.Position  := 0;
   FStream.WriteBuffer(FHeader, SizeOf(TZLibHeader));
   FContentList.Add(string(Dir.FileName));
end;

procedure TDGLZLibArchive.AddFromFile(FileName, Path: string);
var
   FS: TFileStream;
begin
   if not FileExists(FileName) then
      exit;
   FS := TFileStream.Create(FileName, fmOpenRead);
   try
      AddFromStream(FileName, Path, FS);
   finally
      FS.Free;
   end;
end;

procedure TDGLZLibArchive.RemoveContent(index: integer);
var
   Temp: TMemoryStream;
   i:    integer;
   f:    TFileSection;
begin

   Temp := TMemoryStream.Create;
   FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
   FStream.ReadBuffer(Dir, SizeOf(TFileSection));
   FStream.Seek(Dir.FilePos + Dir.FileLength, soFromBeginning);

   Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
   FStream.Position := Dir.FilePos;
   FStream.CopyFrom(Temp, 0);
   FHeader.DirOffset := FHeader.DirOffset - dir.FileLength;
   Temp.Clear;
   for i := 0 to ContentCount - 1 do
      if i > index then
      begin
         FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * i, soFromBeginning);
         FStream.ReadBuffer(f, SizeOf(TFileSection));
         FStream.Position := FStream.Position - SizeOf(TFileSection);
         f.FilePos := f.FilePos - dir.FileLength;
         FStream.WriteBuffer(f, SizeOf(TFileSection));
      end;

   i := FHeader.DirOffset + SizeOf(TFileSection) * index;
   FStream.Position := Cardinal(i + SizeOf(TFileSection));
   if FStream.Position < FStream.Size then
   begin
      Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
      FStream.Position := i;
      FStream.CopyFrom(Temp, 0);
   end;
   Temp.Free;
   FHeader.DirLength := FHeader.DirLength - SizeOf(TFileSection);
   FStream.Position  := 0;
   FStream.WriteBuffer(FHeader, SizeOf(TZLibHeader));
   FStream.Size := FStream.Size - dir.FileLength - SizeOf(TFileSection);
   MakeContentList;
end;

procedure TDGLZLibArchive.RemoveContent(ContentName: string);
begin
   if ContentExists(ContentName) then
      RemoveContent(FContentList.IndexOf(ContentName));
end;

procedure TDGLZLibArchive.Extract(index: integer; NewName: string);
var
   vExtractFileStream: TFileStream;
   vTmpStream: Tstream;
begin
   if NewName = '' then
      Exit;
   if (index < 0) or (index >= ContentCount) then
      exit;
   vExtractFileStream := TFileStream.Create(NewName, fmCreate);
   vTmpStream := GetContent(index);
   vExtractFileStream.CopyFrom(vTmpStream, 0);
   vTmpStream.Free;
   vExtractFileStream.Free;
end;

procedure TDGLZLibArchive.Extract(ContentName, NewName: string);
begin
   if ContentExists(ContentName) then
      Extract(FContentList.IndexOf(ContentName), NewName);
end;

initialization
  RegisterArchiveFormat('zlib', 'GLScene file uses the zlib compression algorithm', TDGLZLibArchive);

end.
