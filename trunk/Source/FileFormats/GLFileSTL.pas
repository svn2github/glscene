//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Support-code to load STL Files into TGLFreeForm-Components in GLScene. 
  Note that you must manually add this unit to one of your project's uses
  to enable support for STL files at run-time. 

  History :
  17/10/02 - EG - Created from split of GLVectorFileObjects,
  ASCII STL support (Adem)
  The whole history is logged in previous version of the unit
}
unit GLFileSTL;

interface

uses
  System.Classes,
  System.SysUtils,

  GLVectorTypes,
  GLVectorGeometry,
  GLVectorLists,
  GLVectorFileObjects,
  GLApplicationFileIO,
  GLUtils, GLCrossPlatform;

type
  TSTLHeader = packed record
    dummy: array [0 .. 79] of byte;
    nbFaces: Longint;
  end;

  TSTLFace = packed record
    normal: TAffineVector; // facet surface normal
    v1: TAffineVector; // vertex 1
    v2: TAffineVector; // vertex 2
    v3: TAffineVector; // vertex 3
    padding: array [0 .. 1] of byte;
  end;

type
  {  The STL vector file (stereolithography format).
    It is a list of the triangular surfaces that describe a computer generated
    solid model. This is the standard input for most rapid prototyping machines.
    There are two flavors of STL, the "text" and the "binary", this class
    reads both, but exports only the "binary" version.
    Original Binary importer code by Paul M. Bearne, Text importer by Adem. }
  TGLSTLVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cSOLID_LABEL = 'SOLID';
  cFACETNORMAL_LABEL = 'FACET NORMAL ';
  cOUTERLOOP_LABEL = 'OUTER LOOP';
  cVERTEX_LABEL = 'VERTEX';
  cENDLOOP_LABEL = 'ENDLOOP';
  cENDFACET_LABEL = 'ENDFACET';
  cENDSOLID_LABEL = 'ENDSOLID';
  cFULL_HEADER_LEN = 84;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

class function TGLSTLVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLSTLVectorFile.LoadFromStream(aStream: TStream);
var
  sl: TStringList;

  procedure DecodeSTLNormals(const aString: String; var aNormal: TAffineVector);
  begin
    sl.CommaText := aString;
    if sl.Count <> 5 then
      raise Exception.Create('Invalid Normal')
    else
    begin
      aNormal.X := GLUtils.StrToFloatDef(sl[2], 0);
      aNormal.Y := GLUtils.StrToFloatDef(sl[3], 0);
      aNormal.Z := GLUtils.StrToFloatDef(sl[4], 0);
    end;
  end;

  procedure DecodeSTLVertex(const aString: String; var aVertex: TAffineVector);
  begin
    sl.CommaText := aString;
    if (sl.Count <> 4) or (CompareText(sl[0], cVERTEX_LABEL) <> 0) then
      raise Exception.Create('Invalid Vertex')
    else
    begin
      aVertex.X := GLUtils.StrToFloatDef(sl[1], 0);
      aVertex.Y := GLUtils.StrToFloatDef(sl[2], 0);
      aVertex.Z := GLUtils.StrToFloatDef(sl[3], 0);
    end;
  end;

  function ReadLine(Stream: TStream; var Line: String): boolean;
  var
    ch: AnsiChar;
    StartPos, LineLen: integer;
    LineBuf: AnsiString;
  begin
    result := False;
    StartPos := Stream.Position;
    ch := #0;
    while (Stream.Read( ch, 1) = 1) and (ch <> #13) and (ch <> #10) do;
    LineLen := Stream.Position - StartPos;
    Stream.Position := StartPos;
    SetString(LineBuf, NIL, LineLen);
    Stream.ReadBuffer(LineBuf[1], LineLen);
    Line:= String(LineBuf);
    if (ch = #13) or (ch = #10) then
      begin
        result := True;
        if (Stream.Read( ch, 1) = 1) and (ch <> #10) then
          Stream.Seek(-1, soCurrent) // unread it if not LF character.
      end
  end;

var
  isBinary: Boolean;
  ASCIIHeaderBuf: Array[0..5] of AnsiChar;
  positionBackup, StreamSize: Int64;
  fileContent: TStringList;
  curLine: String;
  i: Integer;
  mesh: TMeshObject;
  header: TSTLHeader;
  dataFace: TSTLFace;
  calcNormal: TAffineVector;
begin
  positionBackup := aStream.Position;
  StreamSize:= aStream.Size;

  // check format is ASCII or binary:
  // see https://stackoverflow.com/questions/26171521/verifying-that-an-stl-file-is-ascii-or-binary
  isBinary := True;

  // Minimal STL must be at least 16 Bytes
  if StreamSize < 16 then
    raise Exception.CreateFmt('The STL file is not long enough (%d bytes).', [StreamSize]);

  // test for valid ASCII format
  aStream.Read(ASCIIHeaderBuf[0], 6);
  if ASCIIHeaderBuf = 'solid ' then
    begin
      // test for ASCII format
      if ReadLine(aStream, curLine) then
        if ReadLine(aStream, curLine) then
          if (Pos('facet ', curLine) > 0) or (Pos('endsolid ', curLine) > 0) then
            begin
              isBinary := False;
              aStream.Position := positionBackup;
            end;
    end;

  // test for valid binary
  if isBinary then
    begin
      // test for binary format
      if StreamSize < 84 then
        raise Exception.CreateFmt('The STL file is not long enough (%d bytes).', [StreamSize]);
      aStream.Position:= positionBackup;
      aStream.Read(header, SizeOf(TSTLHeader));
      // Verify that file size equals the sum of header + nTriangles value + all triangles
      if StreamSize < (84 + (header.nbFaces * 50)) then
        raise Exception.CreateFmt('The STL file is not long enough (%d bytes).', [StreamSize]);
    end;


  mesh := TMeshObject.CreateOwned(Owner.MeshObjects);
  try
    mesh.Mode := momTriangles;
    if isBinary then
    begin
      aStream.Read(header, SizeOf(TSTLHeader));
      for i := 0 to header.nbFaces - 1 do
      begin
        aStream.Read(dataFace, SizeOf(TSTLFace));
        with dataFace, mesh do
        begin
          // STL faces have a normal, but do not necessarily follow the winding rule,
          // so we must first determine if the triangle is properly oriented
          // and rewind it properly if not...
          calcNormal := CalcPlaneNormal(v1, v2, v3);
          if VectorDotProduct(calcNormal, normal) > 0 then
            Vertices.Add(v1, v2, v3)
          else
            Vertices.Add(v3, v2, v1);
          Normals.Add(normal, normal, normal);
        end;
      end;

    end
    else
    begin
      fileContent := TStringList.Create;
      sl := TStringList.Create;
      try
        fileContent.LoadFromStream(aStream);
        i := 0;
        curLine := Trim(UpperCase(fileContent[i]));
        if Pos(cSOLID_LABEL, curLine) = 1 then
        begin
          mesh.Vertices.Capacity := (fileContent.Count - 2) div 7;
          mesh.Normals.Capacity := (fileContent.Count - 2) div 7;
          Inc(i);
          curLine := Trim(UpperCase(fileContent[i]));
          while i < fileContent.Count do
          begin
            if Pos(cFACETNORMAL_LABEL, curLine) = 1 then
            begin
              DecodeSTLNormals(curLine, dataFace.normal);
              Inc(i);
              curLine := Trim(UpperCase(fileContent[i]));
              if Pos(cOUTERLOOP_LABEL, curLine) = 1 then
              begin
                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, dataFace.v1);

                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, dataFace.v2);

                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, dataFace.v3);
              end;
              Inc(i);
              curLine := Trim(UpperCase(fileContent[i]));
              if Pos(cENDLOOP_LABEL, curLine) <> 1 then
                raise Exception.Create('End of Loop Not Found')
              else
              begin
                calcNormal := CalcPlaneNormal(dataFace.v1, dataFace.v2,
                  dataFace.v3);
                if VectorDotProduct(calcNormal, dataFace.normal) > 0 then
                  mesh.Vertices.Add(dataFace.v1, dataFace.v2, dataFace.v3)
                else
                  mesh.Vertices.Add(dataFace.v3, dataFace.v2, dataFace.v1);
                mesh.Normals.Add(dataFace.normal, dataFace.normal,
                  dataFace.normal);
              end;
            end;
            Inc(i);
            curLine := Trim(UpperCase(fileContent[i]));
            if Pos(cENDFACET_LABEL, curLine) <> 1 then
              raise Exception.Create('End of Facet Not found');
            Inc(i);
            curLine := Trim(UpperCase(fileContent[i]));
            if Pos(cENDSOLID_LABEL, curLine) = 1 then
              Break;
          end;
        end;
      finally
        sl.Free;
        fileContent.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      mesh.Free;
    end;
  end;
end;

procedure TGLSTLVectorFile.SaveToStream(aStream: TStream);
var
  i: Integer;
  header: TSTLHeader;
  dataFace: TSTLFace;
  list: TAffineVectorList;
const
  cHeaderTag = 'GLScene STL export';
begin
  list := Owner.MeshObjects.ExtractTriangles;
  try
    FillChar(header.dummy[0], SizeOf(header.dummy), 0);
    Move(cHeaderTag, header.dummy[0], Length(cHeaderTag));
    header.nbFaces := list.Count div 3;
    aStream.Write(header, SizeOf(header));
    i := 0;
    while i < list.Count do
    begin
      dataFace.normal := CalcPlaneNormal(list[i], list[i + 1], list[i + 2]);
      dataFace.v1 := list[i];
      dataFace.v2 := list[i + 1];
      dataFace.v3 := list[i + 2];
      aStream.Write(dataFace, SizeOf(dataFace));
      Inc(i, 3);
    end;
  finally
    list.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterVectorFileFormat('stl', 'Stereolithography files', TGLSTLVectorFile);

end.
