{-----------------------------------------------------------------------------
 Unit Name: GLFileMX2 mesh import
 Project: Virtual Studio project - Research and Development Division
 Purpose: Mesh,Texture and Animation import
 Address: stefan.bazelkov@gmail.com rd@evrocom.net
 Eurocom Bulgaria Television Ltd. - Europe, Bulgaria, Plovdiv
-----------------------------------------------------------------------------}


unit GLFileMX2;

interface

uses

  Classes, SysUtils, GLVectorFileObjects, GLColor, GLMaterial;

type
  TGLMX2VectorFile = class(TVectorFile)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;
  TMx2MessageType = (mx2mtInformation, mx2mtWarning, mx2mtError);
  TOnMx2Message = procedure(Mt: TMx2MessageType; const S: String) of Object;

var
  OnMx2Message: TOnMx2Message = nil;

implementation

uses
  GLVectorGeometry, GLVectorLists, GLTexture, GLVectorTypes;

type
  TMxSignature = array[0..3] of AnsiChar;
  TMxReader = class
  public
    function Version: Integer; virtual; abstract;
    function ReadSignature: TMxSignature; virtual; abstract;
    function ReadInteger: Integer; virtual; abstract;
    function ReadFloat: Single; virtual; abstract;
    function ReadString: String; virtual; abstract;
  end;

  TMxTextReader = class(TMxReader)
  private
    FStream: TStream;
    FVersion: Integer;
    FLine: Integer;
  public
    constructor Create(Stream: TStream);
    function Version: Integer; override;
    function ReadSignature: TMxSignature; override;
    function ReadInteger: Integer; override;
    function ReadFloat: Single; override;
    function ReadString: String; override;
  private
    function ReadChar: AnsiChar;
    function ReadWord: String;
    procedure Error(const S: String);
  end;


  TMxBinaryReader = class(TMxReader)
  private
    FStream: TStream;
    FVersion: Integer;
  public
    constructor Create(Stream: TStream);
    function Version: Integer; override;
    function ReadSignature: TMxSignature; override;
    function ReadInteger: Integer; override;
    function ReadFloat: Single; override;
    function ReadString: String; override;
  end;

procedure Message(Mt: TMx2MessageType; const S: String);
begin
  if Assigned(OnMx2Message) then OnMx2Message(mt, S);
end;

procedure Error(const S: String);
begin
  Message(mx2mtError, S);
  raise EInvalidVectorFile.Create('MX2 Import Error: ' + S);
end;

procedure textureNotFound(const texturePath: String; ignoreMissing: Boolean);
var
  Msg: String;

begin
  Msg := 'Texture ''' + texturePath + ''' not found';
  if ignoreMissing then
    Message(mx2mtWarning, Msg)
  else
  begin
    Message(mx2mtError, Msg);
    raise ETexture.Create(Msg);
  end;
end;

function isPrintable(C: AnsiChar): Boolean;
begin
  Result := (Ord(C) > 31) and (Ord(C) < 128);
end;

function makePrintable(C: AnsiChar; N: AnsiChar): AnsiChar;
begin
  if( isPrintable(C) ) then
    Result := C
  else
    Result := N;
end;

function CheckMx2Format(Stream: TStream): TMxReader;
var
  Buffer: array[0..3] of AnsiChar;
  i: Integer;
begin
  Result := nil; // silence warning
//  Stream.ReadBuffer(Buffer, 4);
  Buffer := 'MX2A';
  if(Buffer = 'MX2A') then
    Result := TMxTextReader.Create(Stream)
  else if(Buffer = 'MX2B') then
    Result := TMxBinaryReader.Create(Stream)
  else
  begin
    for i := 0 to 3 do Buffer[i] := makePrintable(Buffer[i], '?');
    Error('Invalid Signature (' + Buffer + ')');
  end;
end;

function MeshAvailable(Reader: TMxReader): Boolean;
var
  Sig: TMxSignature;
  i: Integer;

begin
  Sig := Reader.ReadSignature;
  if(Sig = 'MESH') then
    Result := true
  else if(Sig = 'END.') then
    Result := false
  else
  begin
    for i := 0 to 3 do Sig[i] := makePrintable(Sig[i], '?');
    Error('Invalid Subsection (' + Sig + ')');
    Result := false; // silence warning
  end;
end;

function ReadVertex(Reader: TMxReader): TAffineVector;
begin
  Result[0] := Reader.ReadFloat;
  Result[1] := Reader.ReadFloat;
  Result[2] := Reader.ReadFloat;
end;

function ReadNormal(Reader: TMxReader): TAffineVector;
begin
  Result[0] := Reader.ReadFloat;
  Result[1] := Reader.ReadFloat;
  Result[2] := Reader.ReadFloat;
end;

function ReadTexCoord(Reader: TMxReader): TAffineVector;
begin
  Result[0] := Reader.ReadFloat;
  Result[1] := Reader.ReadFloat;
  Result[2] := 0.0;
end;

type TElementReader = function(Reader: TMxReader): TAffineVector;

procedure Read(Reader: TMxReader; V: TAffineVectorList; F: TElementReader);
var
  i, n: Integer;

begin
  n := Reader.ReadInteger;
  V.Capacity := n;
  for i := 1 to n do
    V.Add( F(Reader) );
end;

type
  Float = Single;
  TMxMaterial = record
    Diffuse: TColorVector;
    Transparency: Float;
    Ambient: TColorVector;
    Emission: TColorVector;
    Specular: TColorVector;
    Snininess: Float;
    Texture: AnsiString;
  end;

function ReadColor(Reader: TMxReader): TColorVector;
begin
  Result[0] := Reader.ReadFloat;
  Result[1] := Reader.ReadFloat;
  Result[2] := Reader.ReadFloat;
  Result[3] := 1;
end;

function ReadMaterial(Reader: TMxReader): TMxMaterial;
begin
  Result.Diffuse := ReadColor(Reader);
  Result.Transparency := Reader.ReadFloat;
  Result.Ambient := ReadColor(Reader);
  Result.Emission := ReadColor(Reader);
  Result.Specular := ReadColor(Reader);
  Result.Snininess := Reader.ReadFloat;
  Result.Texture := Reader.ReadString;
end;


// SysUtils.ExtractFileName doesn't support / as a path delimiter
function extractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('/\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function getTexturePath(const Texture: String; const SearchPath: String): String;
begin
  if FileExists(Texture) then
    Result := Texture
  else
    Result := FileSearch(extractFileName(Texture), SearchPath);
end;

procedure assignColor(Source: TColorVector; Dest: TGLColor);
begin
  if( Source[0] >= 0.0 ) then
    Dest.Color := Source;
end;

procedure ImportMaterial(Reader: TMxReader; Owner: TGLBaseMesh; const Name: String);
var
  mx: TMxMaterial;
  lm: TGLMaterialLibrary;
  tp: String;
  ml: TGLLibMaterial;

begin
  mx := ReadMaterial(Reader);
  lm := Owner.MaterialLibrary;
  if Assigned(lm) and (lm.LibMaterialByName(Name) = nil) then
  begin
    if mx.Texture <> '' then
    begin
      tp := getTexturePath(mx.Texture, lm.TexturePaths);
      if tp = '' then
        begin
          tp := mx.Texture;
          textureNotFound(tp, Owner.IgnoreMissingTextures);
        end;
      ml := lm.AddTextureMaterial(Name, tp, false);
      assignColor(mx.Diffuse, ml.Material.FrontProperties.Diffuse);
    end
    else
    begin
      ml := lm.Materials.Add;
      ml.Name := Name;
      with ml.Material.FrontProperties do
      begin
        assignColor(mx.Diffuse, Diffuse);
        if mx.Transparency > 0.0 then
        begin
          Diffuse.Alpha := 1.0 - mx.Transparency;
          ml.Material.BlendingMode := bmTransparency;
        end;
      end;
    end;
    // common for textured and uintextured
    with ml.Material.FrontProperties do
    begin
      assignColor(mx.Ambient, Ambient);
      assignColor(mx.Emission, Emission);
      assignColor(mx.Specular, Specular);
      if (mx.Snininess >= 0.0) and (mx.Snininess < 128.5) then Shininess := Round(mx.Snininess);
    end;
  end;
end;

procedure ImportFaceGroup(Reader: TMxReader; Owner: TGLBaseMesh; Mesh: TGLMeshObject);
var
  fgName: String;
  faceGroup: TFGVertexNormalTexIndexList;
  i, n: Integer;
begin
  fgName := Reader.ReadString;
  ImportMaterial(Reader, Owner, fgName);
  faceGroup := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  faceGroup.MaterialName := fgName;
  faceGroup.Mode := fgmmTriangles;
  n := Reader.ReadInteger;
  faceGroup.VertexIndices.Capacity := n;
  faceGroup.NormalIndices.Capacity := n;
  faceGroup.TexCoordIndices.Capacity := n;
  Message( mx2mtInformation, Format('Mesh ''%s'': importing face group ''%s'' (%d triangles)', [Mesh.Name, fgName, n div 3]) );
  for i := 1 to n do
  begin
    faceGroup.VertexIndices.Add(Reader.ReadInteger);
    faceGroup.NormalIndices.Add(Reader.ReadInteger);
    faceGroup.TexCoordIndices.Add(Reader.ReadInteger);
  end;
end;

procedure AddAnimation(Animations: TActorAnimations; Name: String; startFrame, endFrame: Integer);
var
  anim: TActorAnimation;
begin
  if(Animations.FindName(Name) = nil) then
  begin
    anim := Animations.Add;
    anim.Name := Name;
    anim.Reference := aarMorph;
    anim.StartFrame := startFrame;
    anim.EndFrame := endFrame;
  end;
end;

procedure ImportMesh(Reader: TMxReader; Owner: TGLBaseMesh);
var

  meshName: String;
  meshFrames: Integer;

  mesh: TGLMeshObject;
  mesh2: TMorphableMeshObject;
  morphTarget: TMeshMorphTarget;
  i, fgCount: Integer;

begin
  meshName := Reader.ReadString;
  meshFrames := Reader.ReadInteger;
  if(meshFrames = 1) then
  begin
    mesh := TGLMeshObject.CreateOwned(Owner.MeshObjects);
    mesh2 := nil;
  end
  else
  begin
    mesh2 := TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
    mesh := mesh2;
  end;

  mesh.Name := meshName;
  mesh.Mode := momFaceGroups;

  if(meshFrames = 1) then
  begin
    Message( mx2mtInformation, Format('Mesh ''%s'': importing vertices', [meshName]) );
    Read(Reader, mesh.Vertices, ReadVertex);
    Message( mx2mtInformation, Format('Mesh ''%s'': importing normals', [meshName]) );
    Read(Reader, mesh.Normals, ReadNormal);
  end
  else
  begin
    for i := 1 to meshFrames do
    begin
      morphTarget := TMeshMorphTarget.CreateOwned(mesh2.MorphTargets);
      morphTarget.Name := 'Frame' + IntToStr(i);
      Message( mx2mtInformation, Format('Mesh ''%s'', frame %d: importing vertices', [meshName, i]) );
      Read(Reader, morphTarget.Vertices, ReadVertex);
      Message( mx2mtInformation, Format('Mesh ''%s'', frame %d: importing normals', [meshName, i]) );
      Read(Reader, morphTarget.Normals, ReadNormal);
    end;
    mesh2.MorphTo(0);
    if( Owner is TGLActor ) then
    begin
      AddAnimation(TGLActor(Owner).Animations, 'default_playback', 0, meshFrames - 1);
    end;
  end;

  Message( mx2mtInformation, Format('Mesh ''%s'': importing texture coordinates', [meshName]) );
  Read(Reader, mesh.TexCoords, ReadTexCoord);

  // face groups
  fgCount := Reader.ReadInteger;
  for i := 1 to fgCount do
    ImportFaceGroup(Reader, Owner, mesh);
end;

procedure TGLMX2VectorFile.LoadFromStream(Stream: TStream);
var
  reader: TMxReader;

begin
  Message( mx2mtInformation, 'Importing ' + Self.ResourceName);
  reader := CheckMx2Format(Stream);
  try
    while MeshAvailable(reader) do
    begin
      ImportMesh(reader, Owner);
    end;
  finally
    reader.Free;
  end;
  Message( mx2mtInformation, Self.ResourceName + ' imported successfully');
end;

{ TMxBinaryReader }

constructor TMxBinaryReader.Create(Stream: TStream);
begin
  FStream := Stream;
  FVersion := ReadInteger;
end;

function TMxBinaryReader.ReadFloat: Single;
begin
  FStream.ReadBuffer(Result, 4);
end;

function TMxBinaryReader.ReadInteger: Integer;
begin
  FStream.ReadBuffer(Result, 4);
end;

function TMxBinaryReader.ReadSignature: TMxSignature;
begin
  FStream.ReadBuffer(Result, 4);
end;

function TMxBinaryReader.ReadString: String;
var n: Integer;
begin
  n := ReadInteger;
  SetLength(Result, n);
  FStream.ReadBuffer(Result[1], n);
end;

function TMxBinaryReader.Version: Integer;
begin
  Result := FVersion;
end;

{ TMxTextReader }

constructor TMxTextReader.Create(Stream: TStream);
begin
  FStream := Stream;
  FLine := 1;
  FVersion := ReadInteger;
end;

procedure TMxTextReader.Error(const S: String);
begin
  GLFileMX2.Error(S + ' at line ' + IntToStr(FLine));
end;

function TMxTextReader.ReadChar: AnsiChar;
begin
  FStream.ReadBuffer(Result, 1);
  if( Result = #10 ) then Inc(FLine);
end;

function TMxTextReader.ReadFloat: Single;
begin
  try
    Result := StrToFloat(ReadWord);
  except
    on E: EConvertError do
    begin
      Error(E.Message);
      Result := 0; // silence warning
    end;
  end;
end;

function TMxTextReader.ReadInteger: Integer;
begin
  try
    Result := StrToInt(ReadWord);
  except
    on E: EConvertError do
    begin
      Error(E.Message);
      Result := 0; // silence warning
    end;
  end;
end;

function TMxTextReader.ReadSignature: TMxSignature;
var
  tmp: AnsiString;
  i: Integer;

begin
  tmp := ReadWord;
  if( length(tmp) <> 4 ) then Error('Invalid signature: ''' + tmp + '''');
  for i := 0 to 3 do Result[i] := tmp[i+1];
end;

function TMxTextReader.ReadString: String;
var
  C, Q: AnsiChar;

begin
  Result := '';
  repeat
    C := ReadChar;
  until not (C in [' ', #9, #10, #13]);
  if not ( C in ['''', '"'] ) then Error('String expected');
  Q := C;
  repeat
    C := ReadChar;
    if(C = Q) then Break;
    Result := Result + C;
  until false;
end;

function TMxTextReader.ReadWord: String;
var
  C: AnsiChar;

begin
  Result := '';
  repeat
    C := ReadChar;
  until not (C in [' ', #9, #10, #13]);
  repeat
    Result := Result + C;
    C := ReadChar;
  until C in [' ', #9, #10, #13];
end;

function TMxTextReader.Version: Integer;
begin
  Result := FVersion;
end;

initialization

   RegisterVectorFileFormat('mx2', 'mx2', TGLMX2VectorFile);
   RegisterVectorFileFormat('mx2a', 'mx2a', TGLMX2VectorFile);
   RegisterVectorFileFormat('mx2b', 'mx2b', TGLMX2VectorFile);

end.

