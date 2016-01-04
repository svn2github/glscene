//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : GLObjects

  @HTML (
  <p>Implementation of basic scene objects plus some management routines.</p>

  <p>All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.<br>

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical like SuperEllipsoid and pqTorus
  objects can be found in DGLGeomObjects.</p>

  <p>You'll find specialized Meshes and Actors Objects in DGLMeshObject unit</p>

  <b>History : </b><font size=-1><ul>
    <li>03/01/16 - JD - Created</li>
  </ul></font>
  <p>
  <b>Status : </b>In Progress<br>
  <b>Todo : </b>
  <ul>
      <li></li>
  </ul></p> )
}
unit DGLVectorObjects;

interface

{$I DGLEngine.inc}

uses
  Classes, SysUtils,
  // DGLE
  DGLSLog, DGLCrossPlatform, dglOpenGL, DGLContext, DGLContextHandles, DGLRenderContextInfo,
  DGLBaseClasses, DGLPersistentClasses, DGLResStrings, DGLState,  DGLTypes,
  DGLCoordinates, DGLVectorMaths, DGLVectorTypes, DGLVectorLists,DGLApplicationFileIO,
  DGLScene,DGLSilhouette, DGLColor,DGLShader;

type

  {: TDGLMeshMode
     - mmStatic  : Data Not often Updated
     - mmDynamic : Data often Updated (many time by second but not each frame)
     - mmStream  : Date Always Updated (each Frame) }
  TDGLMeshMode = (mmStatic, mmDynamic,mmStream);

  // ****************************************************************************************
  // TGLAbstractBaseSceneObject
  //
  // TBaseMeshObject
  //
  {: A base class for mesh Datas Objects.<p>
     The class introduces a set of vertices,normals and TexCoords by default for the object.
     You can choose to use Colors, Tangents and Binormals.
     The Object does no rendering of its own. }
  TDGLMeshDataObject = class(TDGLPersistentObject)
  private
    { Private Declarations }
    FName: string;
    FVertices: TDGLAffineVectorList;
    FNormals: TDGLAffineVectorList;
    FTexCoords :TDGLAffineVectorList;

    FTangents: TDGLAffineVectorList;
    FBiNormals: TDGLAffineVectorList;
    FColors: TDGLVectorList;

    FVisible: Boolean;

    FUseColors,
    FUseTangents,
    FUseBiNormals : Boolean;

    FVerticesSize,
    FNormalsSize,
    FTexCoordsSize,
    FTangentsSize,
    FBiNormalsSize,
    FColorsSize,
    FTotalSize:Integer;
  protected
    { Protected Declarations }
    procedure SetVertices(const val: TDGLAffineVectorList);
    procedure SetNormals(const val: TDGLAffineVectorList);
    procedure SetTexCoords(const val: TDGLAffineVectorList);
    procedure SetTangents(const val: TDGLAffineVectorList);
    procedure SetBiNormals(const val: TDGLAffineVectorList);
    procedure SetColors(const val: TDGLVectorList);

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    function GetVerticesDataSize:Integer;
    function GetNormalsDataSize:Integer;
    function GetTangentsDataSize:Integer;
    function GetBiNormalsDataSize:Integer;
    function GetColorsDataSize:Integer;
    function GetTexCoordsDataSize:Integer;
    function GetTotalDataSize:Integer;

    function VerticesCount:Integer;
    function FacesCount:Integer;

    procedure AddVertice(x,y,z:single);
    procedure AddNormal(x,y,z:single);
    procedure AddTexCoord(x,y,z:Single);

//    procedure BuildNormals(vertexIndices: TIntegerList; mode: TMeshObjectMode;normalIndices: TIntegerList = nil);
//    procedure BuildTangentAndBiNormals;

    {: Clears all mesh object data, submeshes, facegroups, etc. }
    procedure Clear; dynamic;

    property Name: string read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Vertices: TDGLAffineVectorList read FVertices write SetVertices;
    property Normals: TDGLAffineVectorList read FNormals write SetNormals;
    property TexCoords: TDGLAffineVectorList read FTexCoords write SetTexCoords;
    property Tangents: TDGLAffineVectorList read FTangents write SetTangents;
    property BiNormals: TDGLAffineVectorList read FBiNormals write SetBiNormals;
    property Colors: TDGLVectorList read FColors write SetColors;

    property UseColors : Boolean read FUseColors Write FUseColors;
    property UseTangents : Boolean read FUseTangents Write FUseTangents;
    property UseBiNormals : Boolean read FUseBiNormals Write FUseBiNormals;

  end;


  // ****************************************************************************************
  // TDGLBaseVextexBufferObject
  //
  {: Base of Rendering Mesh Objects. It use TDGLMeshDataObject for storing vertices, normals, texcoords ect...
     This Object use  VAO abd VBO so OpenGL 3.3 minimum is required.
     At DesignTime it use his own default Shader and at Runtime it use your own shader if he's assigned.
  }
  TDGLBaseVertexBufferedObject = class(TDGLBaseSceneObject, IDGLShaderLibrarySupported)
  private
    FVBOID : TGLUInt;
    FVAOID : TGLUInt;
    FVBOHandle:  TDGLVBOArrayBufferHandle;
    FVAOHandle : TDGLVertexArrayHandle;

    FMeshData : TDGLMeshDataObject;
    FMeshMode : TDGLMeshMode;
    FNeedBuild : Boolean;

    FShaderLib: TDGLShaderLibrary;
    FShaderName: TDGLLibShaderName;
    FShader : TDGLLibShader;

    procedure SetShaderName(aValue: TDGLLibShaderName);
    procedure SetShaderLib(Value:TDGLShaderLibrary);
  protected
    function GetShaderLibrary: TDGLAbstractShaderLibrary;
    function GetOpenGLMeshMode:GLUInt;
    property MeshData : TDGLMeshDataObject read FMeshData write FMeshData;
  public

    Constructor Create(AOWner:TComponent);override;
    Destructor Destroy;override;

    procedure NotifyChange(Sender:TObject);override;

    procedure UpdateAll;
//    procedure Update(DataType,DataSize,NewData:Integer);
    property NeedBuild : Boolean read FNeedBuild write FNeedBuild;

    procedure BuildMesh;virtual;
    function getShader:TDGLLibShader;
    procedure Build(ARci:TRenderContextInfo);
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;

    property ShaderLibrary: TDGLShaderLibrary read FShaderLib write SetShaderLib;
    property ShaderName: TDGLLibShaderName read FShaderName write SetShaderName;
    property MeshMode : TDGLMeshMode read FMeshMode Write FMeshMode;
  end;

  // ****************************************************************************************
  // TDGLPlane
  //
  TDGLPlane = class(TDGLBaseVertexBufferedObject)
  private
    FWidth, FHeight: TGLFloat;

    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);

  public
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;

    procedure BuildMesh;override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function ScreenRect(aBuffer: TDGLSceneBuffer): TDGLRect;
    function PointDistance(const aPoint: TVector): Single;

  published
    { Public Declarations }
    property Height: TGLFloat read FHeight write SetHeight;
    property Width: TGLFloat read FWidth write SetWidth;
    property ShaderLibrary;
    property ShaderName;
    property MeshMode;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;

  end;

  // ****************************************************************************************
  // TDGLCube
  //
  {: Setup and Draw Cube }
  TCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TCubeParts = set of TCubePart;
  TDGLCube = class(TDGLBaseVertexBufferedObject)
  private
    { Private Declarations }
    FCubeSize:        TAffineVector;
    FParts:           TCubeParts;
    FNormalDirection: TNormalDirection;
    procedure SetCubeSize(const Index:Integer;const aValue: Single);
    function GetCubeSize(const Index:Integer):Single;
    procedure SetParts(aValue: TCubeParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    Constructor Create(AOWner:TComponent);override;
//    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure BuildMesh;override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette; override;

  published
    { Published Declarations }
    property CubeWidth:       Single index 0 read GetCubeSize write SetCubeSize stored False;
    property CubeHeight:      Single index 1 read GetCubeSize write SetCubeSize stored False;
    property CubeDepth:       Single index 2 read GetCubeSize write SetCubeSize stored False;
    property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
    property Parts:           TCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];

    property ShaderLibrary;
    property ShaderName;
    property MeshMode;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
//    property Hint;
  end;


implementation

uses
  Math;

var
  DefaultShader: TDGLProgramHandle = nil;

Const
  DefaultShader_vp: AnsiString =
    '#version 330' + #10#13 +

    'layout(location=0) in vec3 Position;' + #10#13 +
    'layout(location=1) in vec3 Normal;' + #10#13 +
    'layout(location=2) in vec2 TexCoord0;' + #10#13 +

    'out float diffuse;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +

    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec4 LightSourcePos;' + #10#13 +

    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vec4 vertex    = ModelMatrix*vec4(Position, 1.0);' + #10#13 +

    '	vec4 direction = normalize(LightSourcePos - vertex);' + #10#13 +
    '	vec3 normal = normalize(mat3(ModelMatrix) * Normal);' + #10#13 +
    '	diffuse = clamp(dot(normal, direction.xyz), 0.1, 1.0);' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vertex;' + #10#13 +
    '}';

  DefaultShader_fp: AnsiString =
    '#version 330' + #10#13 +
    'in float diffuse;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 tc = fract(texcoord);' + #10#13 +
    ' float df = sign(diffuse+0.01)*tc.s;' + #10#13 +
    '	FragColor = vec4(df*tc.s, df*tc.t, df,1.0);' + #10#13 +
    '}';

  DefaultShader_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec3 Normal;' + #10#13 +
    'in vec2 TexCoord0;' + #10#13 +
    'out float diffuse;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec4 LightSourcePos;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vec4 vertex    = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    '	vec4 direction = normalize(LightSourcePos - vertex);' + #10#13 +
    '	vec3 normal = normalize(mat3(ModelMatrix) * Normal);' + #10#13 +
    '	diffuse = clamp(dot(normal, direction.xyz), 0.0, 1.0);' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vertex;' + #10#13 +
    '}';

  DefaultShader_fp150: AnsiString =
    '#version 150' + #10#13 +
    'in float diffuse;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 tc = fract(texcoord);' + #10#13 +
    ' float df = sign(diffuse+0.01);' + #10#13 +
    '	FragColor = vec4(tc.s*df, tc.t*df, df, 1.0);' + #10#13 +
    '}';


// ------------------
{ TDGLMeshDataObject }
{$IFDEF GLS_REGION}{$REGION 'TDGLMeshDataObject'}{$ENDIF}

constructor TDGLMeshDataObject.Create;
begin
  FVertices := TDGLAffineVectorList.Create;
  FNormals := TDGLAffineVectorList.Create;
  FTexCoords:=TDGLAffineVectorList.Create;
  FBiNormals:=TDGLAffineVectorList.Create;
  FTangents:=TDGLAffineVectorList.Create;
  FColors:=TDGLVectorList.Create;
  FVisible := True;
  FTotalSize:=-1;
  FVerticesSize:=-1;
  FNormalsSize:=-1;
  FTexCoordsSize:=-1;
  FBiNormalsSize:=-1;
  FTangentsSize:=-1;
  FColorsSize:=-1;
  FuseColors := False;
  FUseTangents:=False;
  FUseBiNormals:=False;

  inherited Create;
end;

destructor TDGLMeshDataObject.Destroy;
begin
  FColors.Free;
  FTangents.Free;
  FBiNormals.Free;
  FTexCoords.Free;
  FNormals.Free;
  FVertices.Free;
  inherited;
end;

procedure TDGLMeshDataObject.Assign(Source: TPersistent);
begin
  if Source is TDGLMeshDataObject then
  begin
    FName := TDGLMeshDataObject(Source).Name;
    FVertices.Assign(TDGLMeshDataObject(Source).FVertices);
    FNormals.Assign(TDGLMeshDataObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

procedure TDGLMeshDataObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1, added FVisible
    WriteString(FName);
    FVertices.WriteToFiler(writer);
    FNormals.WriteToFiler(writer);
    WriteBoolean(FVisible);
  end;
end;

procedure TDGLMeshDataObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..1] then
    with reader do
    begin
      FName := ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion >= 1 then
        FVisible := ReadBoolean
      else
        FVisible := True;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TDGLMeshDataObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
  FTexCoords.Clear;
  FColors.Clear;
  FTangents.Clear;
  FBiNormals.Clear;
end;

procedure TDGLMeshDataObject.SetVertices(const val: TDGLAffineVectorList);
begin
  FVertices.Assign(val);
end;

procedure TDGLMeshDataObject.SetNormals(const val: TDGLAffineVectorList);
begin
  FNormals.Assign(val);
end;

procedure TDGLMeshDataObject.SetTexCoords(const val: TDGLAffineVectorList);
begin
  FtexCoords.Assign(val);
end;

procedure TDGLMeshDataObject.SetBiNormals(const val: TDGLAffineVectorList);
begin
  FBiNormals.Assign(val);
end;

procedure TDGLMeshDataObject.SetTangents(const val: TDGLAffineVectorList);
begin
  FTangents.Assign(val);
end;

procedure TDGLMeshDataObject.SetColors(const val: TDGLVectorList);
begin
  FColors.Assign(val);
end;

procedure TDGLMeshDataObject.AddVertice(x,y,z:single);
begin
 FVertices.Add(x,y,z);
end;

procedure TDGLMeshDataObject.AddNormal(x,y,z:single);
begin
 FNormals.Add(x,y,z);
end;

procedure TDGLMeshDataObject.AddTexCoord(x,y,z:Single);
begin
  FTexCoords.Add(x,y,z);
end;

function TDGLMeshDataObject.GetVerticesDataSize:Integer;
begin
 if (FVerticesSize=-1) then FVerticesSize := Vertices.Count*3*sizeof(Single);
 DGLSLogger.LogInfo('-----> Vertices Size : '+inttostr(FVerticesSize)+' Bytes');
 Result:=FVerticesSize;
end;

function TDGLMeshDataObject.GetNormalsDataSize:Integer;
begin
 if (FNormalsSize=-1) then FNormalsSize := Normals.Count*3*sizeof(Single);
 DGLSLogger.LogInfo('-----> Normals Size : '+inttostr(FNormalsSize)+' Bytes');
 Result:=FNormalsSize;
end;

function TDGLMeshDataObject.GetTangentsDataSize:Integer;
begin
 if (FTangentsSize=-1) then FTangentsSize := Tangents.Count*3*sizeof(Single);
 DGLSLogger.LogInfo('-----> Tangents Size : '+inttostr(FTangentsSize)+' Bytes');
 Result:=FTangentsSize;
end;

function TDGLMeshDataObject.GetBiNormalsDataSize:Integer;
begin
 if (FBiNormalsSize=-1) then FBiNormalsSize := BiNormals.Count*3*sizeof(Single);
 DGLSLogger.LogInfo('-----> BiNormalss Size : '+inttostr(FBiNormalsSize)+' Bytes');
 Result:=FBiNormalsSize;
end;

function TDGLMeshDataObject.GetColorsDataSize:Integer;
begin
 if (FColorsSize=-1) then FColorsSize := Colors.Count*4*sizeof(Single);
 DGLSLogger.LogInfo('-----> Colors Size : '+inttostr(FColorsSize)+' Bytes');
 Result:=FColorsSize;
end;

function TDGLMeshDataObject.GetTexCoordsDataSize:Integer;
begin
 if (FTexCoordsSize=-1) then FTexCoordsSize := TexCoords.Count*3*sizeof(Single);
 DGLSLogger.LogInfo('-----> TexCoords Size : '+inttostr(FTexCoordsSize)+' Bytes');
 Result:=FTexCoordsSize;
end;

function TDGLMeshDataObject.GetTotalDataSize:Integer;
begin
 if (FTotalSize=-1) then
 begin
  FTotalSize := GetVerticesDataSize+GetNormalsDataSize+GetTexCoordsDataSize;
  if FUseColors then FTotalSize := FTotalSize+GetColorsDataSize;
  if FUseBiNormals then FTotalSize := FTotalSize+GetBiNormalsDataSize;
  if FUseTangents then FTotalSize := FTotalSize+GetTangentsDataSize;
 end;
 DGLSLogger.LogInfo('------> Total Size : '+inttostr(FTotalSize)+' Bytes');
 Result:=FTotalSize;
end;

function TDGLMeshDataObject.VerticesCount:Integer;
begin
  DGLSLogger.LogInfo('-----> Vertices Count : '+inttostr(Vertices.Count));
  result:=Vertices.Count;
end;

function TDGLMeshDataObject.FacesCount:Integer;
begin
// Assert((VerticesCount mod 3)<>0,'Wrong Number Vertices');
 DGLSLogger.LogInfo('-----> Faces Count : '+inttostr(VerticesCount div 3));
 result:=VerticesCount div 3;
end;

//procedure TDGLMeshDataObject.ContributeToBarycenter(var currentSum: TAffineVector;var nb: Integer);
//begin
//  AddVector(currentSum, FVertices.Sum);
//  nb := nb + FVertices.Count;
//end;

//procedure TDGLMeshDataObject.Translate(const delta: TAffineVector);
//begin
//  FVertices.Translate(delta);
//end;

//procedure TDGLMeshDataObject.BuildNormals(vertexIndices: TIntegerList; mode:
//  TMeshObjectMode;
//  normalIndices: TIntegerList = nil);
//var
//  i, base: Integer;
//  n: TAffineVector;
//  newNormals: TIntegerList;
//
//  function TranslateNewNormal(vertexIndex: Integer; const delta:
//    TAffineVector): Integer;
//  var
//    pv: PAffineVector;
//  begin
//    result := newNormals[vertexIndex];
//    if result < base then
//    begin
//      result := Normals.Add(NullVector);
//      newNormals[vertexIndex] := result;
//    end;
//    pv := @Normals.List[result];
//    AddVector(pv^, delta);
//  end;
//
//begin
//  if not Assigned(normalIndices) then
//  begin
//    // build bijection
//    Normals.Clear;
//    Normals.Count := Vertices.Count;
//    case mode of
//      momTriangles:
//        begin
//          i := 0;
//          while i <= vertexIndices.Count - 3 do
//            with Normals do
//            begin
//              with Vertices do
//              begin
//                CalcPlaneNormal(Items[vertexIndices[i + 0]],
//                  Items[vertexIndices[i + 1]],
//                  Items[vertexIndices[i + 2]], n);
//              end;
//              with Normals do
//              begin
//                TranslateItem(vertexIndices[i + 0], n);
//                TranslateItem(vertexIndices[i + 1], n);
//                TranslateItem(vertexIndices[i + 2], n);
//              end;
//              Inc(i, 3);
//            end;
//        end;
//      momTriangleStrip:
//        begin
//          i := 0;
//          while i <= vertexIndices.Count - 3 do
//            with Normals do
//            begin
//              with Vertices do
//              begin
//                if (i and 1) = 0 then
//                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
//                    Items[vertexIndices[i + 1]],
//                    Items[vertexIndices[i + 2]], n)
//                else
//                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
//                    Items[vertexIndices[i + 2]],
//                    Items[vertexIndices[i + 1]], n);
//              end;
//              with Normals do
//              begin
//                TranslateItem(vertexIndices[i + 0], n);
//                TranslateItem(vertexIndices[i + 1], n);
//                TranslateItem(vertexIndices[i + 2], n);
//              end;
//              Inc(i, 1);
//            end;
//        end;
//    else
//      Assert(False);
//    end;
//    Normals.Normalize;
//  end
//  else
//  begin
//    // add new normals
//    base := Normals.Count;
//    newNormals := TIntegerList.Create;
//    newNormals.AddSerie(-1, 0, Vertices.Count);
//    case mode of
//      momTriangles:
//        begin
//          i := 0;
//          while i <= vertexIndices.Count - 3 do
//          begin
//            with Vertices do
//            begin
//              CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i
//                + 1]],
//                Items[vertexIndices[i + 2]], n);
//            end;
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
//            Inc(i, 3);
//          end;
//        end;
//      momTriangleStrip:
//        begin
//          i := 0;
//          while i <= vertexIndices.Count - 3 do
//          begin
//            with Vertices do
//            begin
//              if (i and 1) = 0 then
//                CalcPlaneNormal(Items[vertexIndices[i + 0]],
//                  Items[vertexIndices[i + 1]],
//                  Items[vertexIndices[i + 2]], n)
//              else
//                CalcPlaneNormal(Items[vertexIndices[i + 0]],
//                  Items[vertexIndices[i + 2]],
//                  Items[vertexIndices[i + 1]], n);
//            end;
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
//            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
//            Inc(i, 1);
//          end;
//        end;
//    else
//      Assert(False);
//    end;
//    for i := base to Normals.Count - 1 do
//      NormalizeVector(Normals.List^[i]);
//    newNormals.Free;
//  end;
//end;

//function TDGLMeshDataObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
//  normals: TAffineVectorList = nil): TAffineVectorList;
//begin
//  Result := TAffineVectorList.Create;
//  if (Vertices.Count mod 3) = 0 then
//  begin
//    Result.Assign(Vertices);
//    if Assigned(normals) then
//      normals.Assign(Self.Normals);
//  end;
//end;



{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseVertexBufferedObject }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseVertexBufferedObject'}{$ENDIF}

Constructor TDGLBaseVertexBufferedObject.Create(AOWner:TComponent);
begin
  inherited;
  FMeshData := TDGLMeshDataObject.Create;
  FNeedBuild := True;
//  FShaderLib:=TDGLShaderLibrary.Create(self);
  FVBOID:=0;
  FVBOHandle  := TDGLVBOArrayBufferHandle.Create;
  FVAOHandle  := TDGLVertexArrayHandle.Create;
  FVAOID:=0;
end;

function TDGLBaseVertexBufferedObject.GetOpenGLMeshMode:GLUInt;
begin
  result := GL_STATIC_DRAW;
  Case FMeshMode of
    mmStatic : result := GL_STATIC_DRAW;
    mmDynamic : result:= GL_DYNAMIC_DRAW;
    mmStream : result:= GL_STREAM_DRAW;
  End;
end;

procedure TDGLBaseVertexBufferedObject.Build(ARci :TRenderContextInfo);
var
 BufferIndex:Integer;
 BufferOffset:Integer;
begin
  DGLSLogger.LogInfo('TDGLBaseVertexBufferedObject : Build');

//  if (glIsBuffer(FVBOID) = GL_TRUE) then
//  begin
//    DGLSLogger.LogInfo('---> Delete Old VBO Buffer');
//    glDeleteBuffers(1, @FVBOID);
//  end;
//
  DGLSLogger.LogInfo('---> Generate New VBO Buffer');
//  glGenBuffers(1, @FVBOID);
  ARci.GLStates.ArrayBufferBinding:=0;
  if FVBOHandle.Handle = 0 then
  begin
    FVBOHandle.AllocateHandle;
  end;
  DGLSLogger.LogInfo('----> Lock VBO Buffer');
  FVBOHandle.Bind;

    BufferOffset:=0;

    DGLSLogger.LogInfo('-----> Allocate VBO Buffer and Set Vertices Buffer');
    FVBOHandle.BufferData(FMeshData.Vertices,MeshData.GetTotalDataSize,GetOpenGLMeshMode);
    BufferOffset:= BufferOffset + FMeshData.GetVerticesDataSize;

    DGLSLogger.LogInfo('-----> Set Normals VBO SubBuffer');
    FVBOHandle.BufferSubData(BufferOffset,FMeshData.GetNormalsDataSize,FMeshData.Normals);
    BufferOffset:= BufferOffset + FMeshData.GetNormalsDataSize;

    DGLSLogger.LogInfo('-----> Set TexCoords VBO SubBuffer');
    FVBOHandle.BufferSubData(BufferOffset,FMeshData.GetTexCoordsDataSize,FMeshData.TexCoords);
    BufferOffset:= BufferOffset + FMeshData.GetTexCoordsDataSize;

    if FMeshData.UseColors then
    begin
      DGLSLogger.LogInfo('-----> Set Colors VBO SubBuffer');
      FVBOHandle.BufferSubData(BufferOffset,FMeshData.GetColorsDataSize,FMeshData.Colors);
      BufferOffset := BufferOffset + FMeshData.GetColorsDataSize;
    end;

    if FMeshData.UseTangents then
    begin
      DGLSLogger.LogInfo('-----> Set Tangents VBO SubBuffer');
      FVBOHandle.BufferSubData(BufferOffset,FMeshData.GetTangentsDataSize,FMeshData.Tangents);
      BufferOffset := BufferOffset + FMeshData.GetTangentsDataSize;
    end;

    if FMeshData.UseBiNormals then
    begin
      DGLSLogger.LogInfo('-----> Set BiNormals VBO SubBuffer');
      FVBOHandle.BufferSubData(BufferOffset,FMeshData.GetBiNormalsDataSize,FMeshData.BiNormals);
    end;

  DGLSLogger.LogInfo('----> UnLock VBO Buffer');
  FVBOHandle.UnBind;

  DGLSLogger.LogInfo('---> Generate New VAO Buffer');
  ARci.GLStates.VertexArrayBinding := 0;
  if FVAOHandle.Handle = 0 then
  begin
    FVAOHandle.AllocateHandle;
  end;
//  if(glIsVertexArray(FVAOID) = GL_TRUE) then
//  begin
//    DGLSLogger.LogInfo('---> Delete Old VAO Buffer');
//    glDeleteVertexArrays(1, @FVAOID);
//  end;
//


  DGLSLogger.LogInfo('---> Lock VAO Buffer');
  FVAOHandle.Bind;

  DGLSLogger.LogInfo('----> Lock VBO Buffer');
  FVBOHandle.Bind;


    DGLSLogger.LogInfo('-----> Access to vertices VAO Buffer');
    BufferOffset:=0;
    BufferIndex:=0;

    glVertexAttribPointer(BufferIndex, 3, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
    glEnableVertexAttribArray(BufferIndex);
    BufferOffset:= BufferOffset + FMeshData.GetVerticesDataSize;
    inc(BufferIndex);

    DGLSLogger.LogInfo('-----> Access to Normals VAO Buffer');
    glVertexAttribPointer(BufferIndex, 3, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
    glEnableVertexAttribArray(BufferIndex);
    BufferOffset:= BufferOffset + FMeshData.GetNormalsDataSize;
    inc(BufferIndex);

    DGLSLogger.LogInfo('-----> Access to TexCoords VAO Buffer');
    glVertexAttribPointer(BufferIndex, 3, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
    glEnableVertexAttribArray(BufferIndex);
    BufferOffset:= BufferOffset + FMeshData.GetTexCoordsDataSize;
    inc(BufferIndex);

    if FMeshData.UseColors then
    begin
      DGLSLogger.LogInfo('-----> Access to Colors VAO Buffer');
      glVertexAttribPointer(BufferIndex, 4, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
      glEnableVertexAttribArray(BufferIndex);
      inc(BufferIndex);
      BufferOffset := BufferOffset + FMeshData.GetColorsDataSize;
    end;

    if FMeshData.UseTangents then
    begin
      DGLSLogger.LogInfo('-----> Access to Tangents VAO Buffer');
      glVertexAttribPointer(BufferIndex, 3, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
      glEnableVertexAttribArray(BufferIndex);
      inc(BufferIndex);
      BufferOffset := BufferOffset + FMeshData.GetTangentsDataSize;
    end;

    if FMeshData.UseBiNormals then
    begin
      DGLSLogger.LogInfo('-----> Access to BiNormals VAO Buffer');
      glVertexAttribPointer(BufferIndex, 3, GL_FLOAT, GL_FALSE, 0, Pointer(BufferOffset));
      glEnableVertexAttribArray(BufferIndex);
//      BufferOffset := BufferOffset + FMeshData.GetBiNormalsDataSize;
    end;

 DGLSLogger.LogInfo('----> UnLock VBO Buffer');
 FVBOHandle.UnBind;
 //glBindBuffer(GL_ARRAY_BUFFER,0);
 //ARci.GLStates.ArrayBufferBinding:=0;
 DGLSLogger.LogInfo('---> UnLock VAO Buffer');
 FVAOHandle.UnBind;
 //ARci.GLStates.VertexArrayBinding:=0;
 //glBindVertexArray(0);

 FNeedBuild := False;
end;

procedure TDGLBaseVertexBufferedObject.BuildMesh;
begin
  //Do Nothing;
end;

procedure TDGLBaseVertexBufferedObject.UpdateAll;
var
 VBOAddr : PGLVoid;
begin
 DGLSLogger.LogInfo('TDGLBaseVertexBufferedObject : UpdateAll');

 DGLSLogger.LogInfo('---> Lock VBO Buffer');
 glBindBuffer(GL_ARRAY_BUFFER,FVBOID);

 DGLSLogger.LogInfo('----> Get VBO Buffer Pointer');
 VBOAddr := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);

 DGLSLogger.LogInfo('-----> Update VBO Buffer Datas');
 //Move(Attr.List^, PByte(Integer(HostVertexMap) + Integer(offset))^, size)

 DGLSLogger.LogInfo('----> Free VBO Buffer Pointer');
 glUnmapBuffer(GL_ARRAY_BUFFER);
 VBOAddr:= nil;

 DGLSLogger.LogInfo('---> UnLock VBO Buffer');
 glBindBuffer(GL_ARRAY_BUFFER,0);
end;

procedure TDGLBaseVertexBufferedObject.NotifyChange(Sender:TObject);
begin
  //TDGLBaseVertexBufferedObject.UpdateAll;
end;

procedure DisplayMatrix(Value:TMatrix;Name:String);
var
i,j:Integer;
tmp:String;
begin
  DGLSLogger.LogInfo('Display '+Name);
  for i:=0 to 3  do
  begin
    tmp:='[ ';
    for J := 0 to 3 do
    begin
      tmp:=tmp+ FloatToStrF(Value.V[I].V[J],ffFixed,5,3);
      if (J<3) then tmp:=tmp+', ';
    end;
    tmp:=tmp+' ]';
    DGLSLogger.LogInfo(tmp);
  end;

end;

procedure TDGLBaseVertexBufferedObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
Var
 USeDefaultShader:Boolean;
 SB: TDGLSceneBuffer;
// LS: TDGLLightSource;
// BufferIndex:Integer;
// BufferOffset:Integer;
 ModelMatrix,MVP:       TMatrix;
begin
   DGLSLogger.LogInfo('TDGLBaseVertexBufferedObject : DoRender');
  if (FNeedBuild) then Build(ARci);
  if ARenderSelf then
  begin
    UseDefaultShader := true;

    // if runtime
    if not(csDesigning in ComponentState) then
    begin
      DGLSLogger.LogInfo('---> RunTime Mode Activated');
      if Assigned(FShader) then UseDefaultShader := False;
    end
    else
      UseDefaultShader := true;

    DGLSLogger.LogInfo('---> Render Object');

    DGLSLogger.LogInfo('----> Lock VAO Buffer');
    FVAOHandle.Bind;
    //glBindVertexArray(FVAOID);
    //ARci.GLStates.VertexArrayBinding:=FVAOID;

    if Not(USeDefaultShader) then DGLSLogger.LogInfo('-----> Use Custom Shader')
    else DGLSLogger.LogInfo('-----> Use Default Shader');

    if UseDefaultShader then
    begin
      if (DefaultShader.Handle = 0) then
      begin
        DefaultShader.AllocateHandle;
        with DefaultShader do
        begin
          DGLSLogger.LogInfo('------> Assign Vertex and Fragment Shader');
          AddShader(TDGLVertexShaderHandle, string(DefaultShader_vp150), true);
          AddShader(TDGLFragmentShaderHandle, string(DefaultShader_fp150), true);

          
          BindAttribLocation(0, 'Position');
          BindAttribLocation(1, 'Normal');
          BindAttribLocation(2, 'TexCoord0');

          if (not(LinkProgram) or not(ValidateProgram)) then
          begin
            DGLSLogger.LogError('Error Default Shader, not initialized correctly');
            UseDefaultShader := False;
            Assert(false,'Error Default Shader, not initialized correctly');
            abort;
          end;
          DGLSLogger.LogInfo(InfoLog);
        end;
      end;
      with DefaultShader do
      begin
        DGLSLogger.LogInfo('-----> Apply Default Shader');
        UseProgramObject;
        SB := TDGLSceneBuffer(ARci.buffer);
        DGLSLogger.LogInfo('------> Assign Default Shader''s uniforms');
        if Assigned(SB) then
        begin
          // Attribs Shader Uniforms
          //  if ocTransformation in Changes then RebuildMatrix;
            DGLSLogger.LogInfo('-------> Assign Model Matrix From SceneBuffer');
            UniformMatrix4fv['ModelMatrix']          := SB.ModelMatrix;
            DisplayMatrix(SB.ModelMatrix,'ModelMatrix');
            DGLSLogger.LogInfo('-------> Assign ViewProjection Matrix From SceneBuffer');
            //MVP:=MatrixMultiply(SB.ModelMatrix,MatrixMultiply(SB.ViewMatrix, SB.ProjectionMatrix));
            MVP:=MatrixMultiply(SB.ViewMatrix, SB.ProjectionMatrix);
            DisplayMatrix(MVP,'ViewProjectionMatrix');
            UniformMatrix4fv['ViewProjectionMatrix'] := MVP;
        end
        else
        begin
          if ocTransformation in Changes then RebuildMatrix;
          DGLSLogger.LogInfo('-------> Assign Model and ViewProjection Matrix From Current Object');
          ModelMatrix                              := LocalMatrix^;
          UniformMatrix4fv['ModelMatrix']          := ModelMatrix;
          UniformMatrix4fv['ViewProjectionMatrix'] := MatrixMultiply(vDefaultViewMatrix, vDefaultProjectionMatrix);
        end;

        if CurrentDGLContext.GLStates.LightNumber>0 then
        begin
          DGLSLogger.LogInfo('-------> Assign Light position From Current Rendering Context');
          Uniform4f['LightSourcePos'] := CurrentDGLContext.GLStates.LightPosition[0];
        end
        else
        begin
          DGLSLogger.LogInfo('-------> Assign Default Light position');
          Uniform4f['LightSourcePos'] := vDefaultLightSourcePosition;
        end;
      end;
    end
    else
    begin
     DGLSLogger.LogInfo('-----> Apply Custom Shader');
     FShader.Apply(ARci);
    end;

    DGLSLogger.LogInfo('------> Draw Faces');

    glDrawArrays(GL_TRIANGLES, 0, FMeshData.VerticesCount);

    DGLSLogger.LogInfo('----> UnLock VAO Buffer');
    FVAOHandle.UnBind;
    //glBindVertexArray(0);
    //ARci.GLStates.VertexArrayBinding:=0;

    if UseDefaultShader then
    begin
      DGLSLogger.LogInfo('----> UnApply Default Shader');
      DefaultShader.EndUseProgramObject
    end
    else
    begin
      DGLSLogger.LogInfo('----> UnApply Custom Shader');
      FShader.UnApply(ARci);
    end;
  end;
end;

function TDGLBaseVertexBufferedObject.GetShader : TDGLLibShader;
begin
  if (FShaderName='') then
  begin
   // Assert(not(Assigned(FShaderLib)),'You must define a Shader Library First');
    Assert(FShaderName='','You must define a Shader from the Shader Library First');
    abort;
  end;
  result:=FShaderLib.Shaders.GetLibShaderByName(FShaderName);
end;

function TDGLBaseVertexBufferedObject.GetShaderLibrary: TDGLAbstractShaderLibrary;
begin
  Result:=nil;
  if Assigned(FShaderLib) then
  begin
     Result := TDGLAbstractShaderLibrary(FShaderLib);
  end;
end;

procedure TDGLBaseVertexBufferedObject.SetShaderLib(Value:TDGLShaderLibrary);
begin
  if FShaderLib = Value then exit;
  FShaderLib:=Value;
  NotifyChange(Self);
end;

procedure TDGLBaseVertexBufferedObject.SetShaderName(aValue: TDGLLibShaderName);
begin
  Assert(not(Assigned(FShaderLib)),'You must define a Shader Library First');
  if FShaderName=aValue then exit;
  FShaderName:=aValue;
  FShader:=GetShader;
  NotifyChange(Self);
end;

Destructor TDGLBaseVertexBufferedObject.Destroy;
begin
 if FVBOID<>0 then glDeleteBuffers(1, @FVBOID);
 if FVAOID<>0 then glDeleteVertexArrays(1, @FVAOID);
 FMeshData.Free;
 inherited;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLPlane }
{$IFDEF GLS_REGION}{$REGION 'TDGPlane'}{$ENDIF}

constructor TDGLPlane.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  BuildMesh;
end;

procedure TDGLPlane.BuildMesh;
var
  hw, hh:Single;
begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;

  With MeshData do
  begin
    Clear;
    AddVertice(hw,hh,0);
    AddNormal(0,0,1);
    AddTexCoord(1,1,0);

    AddVertice(hw,-hh,0);
    AddNormal(0,0,1);
    AddTexCoord(1,0,0);

    AddVertice(-hw,-hh,0);
    AddNormal(0,0,1);
    AddTexCoord(0,0,0);

    AddVertice(-hw,-hh,0);
    AddNormal(0,0,-1);
    AddTexCoord(0,0,0);

    AddVertice(-hw,hh,0);
    AddNormal(0,0,-1);
    AddTexCoord(0,1,0);

    AddVertice(hw,hh,0);
    AddNormal(0,0,-1);
    AddTexCoord(1,1,0);

  end;
  inherited;
end;

procedure TDGLPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDGLPlane) then
  begin
    FWidth := TDGLPlane(Source).FWidth;
    FHeight := TDGLPlane(Source).FHeight;
  end;
  inherited Assign(Source);
  BuildMesh;
end;

function TDGLPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.V[0] := 0.5 * Abs(FWidth);
  Result.V[1] := 0.5 * Abs(FHeight);
  Result.V[2] := 0;
end;

procedure TDGLPlane.SetWidth(const aValue: Single);
begin
  if aValue <> FWidth then
  begin
    FWidth := aValue;
    BuildMesh;
    NeedBuild:=True;
    StructureChanged;
  end;
end;

function TDGLPlane.ScreenRect(aBuffer: TDGLSceneBuffer): TDGLRect;
var
  v: array[0..3] of TVector;
  buf: TDGLSceneBuffer;
  hw, hh: TGLFloat;
begin
  buf := aBuffer;
  if Assigned(buf) then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    v[0] := LocalToAbsolute(PointMake(-hw, -hh, 0));
    v[1] := LocalToAbsolute(PointMake(hw, -hh, 0));
    v[2] := LocalToAbsolute(PointMake(hw, hh, 0));
    v[3] := LocalToAbsolute(PointMake(-hw, hh, 0));
    buf.WorldToScreen(@v[0], 4);
    Result.Left := Round(MinFloat([v[0].V[0], v[1].V[0], v[2].V[0], v[3].V[0]]));
    Result.Right := Round(MaxFloat([v[0].V[0], v[1].V[0], v[2].V[0], v[3].V[0]]));
    Result.Top := Round(MinFloat([v[0].V[1], v[1].V[1], v[2].V[1], v[3].V[1]]));
    Result.Bottom := Round(MaxFloat([v[0].V[1], v[1].V[1], v[2].V[1], v[3].V[1]]));
  end
  else
    FillChar(Result, SizeOf(TDGLRect), 0);
end;

function TDGLPlane.PointDistance(const aPoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),AbsoluteDirection);
end;

procedure TDGLPlane.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    BuildMesh;
    NeedBuild:=True;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCube }
{$IFDEF GLS_REGION}{$REGION 'TDGLCube'}{$ENDIF}

constructor TDGLCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCubeSize        := XYZVector;
  FParts           := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
  BuildMesh;
end;

procedure TDGLCube.BuildMesh;
var
  hw, hh, hd, nd: Single;
begin
  if FNormalDirection = ndInside then nd := -1
  else nd := 1;

  hw   := FCubeSize.X * 0.5;
  hh   := FCubeSize.Y * 0.5;
  hd   := FCubeSize.Z * 0.5;

  With MeshData do
  begin
    Clear;
    if cpBack in FParts then
    begin
      //1
      AddNormal(0,0, -nd);
      AddTexCoord(1,0,0);
      AddVertice(-hw, -hh, -hd); //6
      //2
      AddNormal(0,0, -nd);
      AddTexCoord(0,0,0);
      AddVertice(hw, -hh, -hd); //7
      //3
      AddNormal(0,0, -nd);
      AddTexCoord(0,1,0);
      AddVertice(hw, hh, -hd); //3

      //3
      AddNormal(0,0, -nd);
      AddTexCoord(0,1,0);
      AddVertice( -hw, -hh, -hd); //3
      //4
      AddNormal(0,0, -nd);
      AddTexCoord(1,1,0);
      AddVertice(-hw, hh, -hd);  //2
      //1
      AddNormal(0,0, -nd);
      AddTexCoord(1,0,0);
      AddVertice(hw, hh, -hd);  //6
    end;

    if cpLeft in FParts then
    begin

      AddNormal(nd, 0,0);
      AddTexCoord(0,0,0);
      AddVertice(hw, -hh, hd); // 4
      //2
      AddNormal(nd, 0,0);
      AddTexCoord(1,0,0);
      AddVertice(hw, -hh, -hd);  //5
      //3
      AddNormal(nd, 0,0);
      AddTexCoord(1,1,0);
      AddVertice( hw, hh, -hd); // 1

      //3
      AddNormal(nd, 0,0);
      AddTexCoord(1,1,0);
      AddVertice(hw, -hh, hd); // 1
      //4
      AddNormal(nd, 0,0);
      AddTexCoord(0,1,0);
      AddVertice(hw, hh, hd); // 0
      //1
      AddNormal(nd, 0,0);
      AddTexCoord(0,0,0);
      AddVertice( hw, hh, -hd); // 4
    end;

    if cpBottom in FParts then
    begin
      //1
      AddNormal(0,-nd,0);
      AddTexCoord(1,0,0);
      AddVertice(-hw, -hh, hd); // 7
      //2
      AddNormal(0,-nd,0);
      AddTexCoord(0,0,0);
      AddVertice(hw, -hh, hd); // 5
      //3
      AddNormal(0,-nd,0);
      AddTexCoord(0,1,0);
      AddVertice(hw, -hh, -hd); // 1

      //3
      AddNormal(0,-nd,0);
      AddTexCoord(0,1,0);
      AddVertice( -hw, -hh, hd); // 1
      //4
      AddNormal(0,-nd,0);
      AddTexCoord(1,1,0);
      AddVertice(-hw, -hh, -hd); // 3
      //1
      AddNormal(0,-nd,0);
      AddTexCoord(1,0,0);
      AddVertice( hw, -hh, -hd); // 7
    end;


    if cpFront in FParts then
    begin
      //1
      AddNormal(0,0,nd);
      AddTexCoord(1,0,0);
      AddVertice(-hw, -hh, hd); // 4
      //2
      AddNormal(0,0,nd);
      AddTexCoord(0,0,0);
      AddVertice( hw, -hh, hd); // 6
      //3
      AddNormal(0,0,nd);
      AddTexCoord(0,1,0);
      AddVertice(hw, hh, hd); // 2

      //3
      AddNormal(0,0,nd);
      AddTexCoord(0,1,0);
      AddVertice( -hw, -hh, hd); // 2
      //4
      AddNormal(0,0,nd);
      AddTexCoord(1,1,0);
      AddVertice(-hw, hh, hd); // 0
      //1
      AddNormal(0,0,nd);
      AddTexCoord(1,0,0);
      AddVertice(hw, hh, hd); // 4
    end;

    if cpLeft in FParts then
    begin
      //1
      AddNormal(-nd,0,0);
      AddTexCoord(1,0,0);
      AddVertice(-hw, -hh, -hd); // 2
      //2
      AddNormal(-nd,0,0);
      AddTexCoord(0,0,0);
      AddVertice(-hw, -hh, hd); // 3
      //3
      AddNormal(-nd,0,0);
      AddTexCoord(0,1,0);
      AddVertice(-hw, hh, hd); // 1

      //3
      AddNormal(-nd,0,0);
      AddTexCoord(0,1,0);
      AddVertice(-hw, -hh, -hd); // 1
      //4
      AddNormal(-nd,0,0);
      AddTexCoord(1,1,0);
      AddVertice(-hw, hh, -hd); // 0
      //1
      AddNormal(-nd,0,0);
      AddTexCoord(1,0,0);
      AddVertice(-hw, hh, hd); // 2
    end;
    if cpTop in FParts then
    begin
      //1
      AddNormal(0,nd,0);
      AddTexCoord(1,0,0);
      AddVertice( -hw, hh, hd); // 6
      //2
      AddNormal(0,nd,0);
      AddTexCoord(0,0,0);
      AddVertice(hw, hh, hd); // 7
      //3
      AddNormal(0,nd,0);
      AddTexCoord(0,1,0);
      AddVertice(hw, hh, -hd); // 5

      //3
      AddNormal(0,nd,0);
      AddTexCoord(0,1,0);
      AddVertice(-hw, hh, hd); // 5
      //4
      AddNormal(0,nd,0);
      AddTexCoord(1,1,0);
      AddVertice(-hw, hh, -hd); // 4
      //1
      AddNormal(0,nd,0);
      AddTexCoord(1,0,0);
      AddVertice(hw, hh, -hd); // 6
    end;
  end;
  inherited;
end;

function TDGLCube.GetCubeSize(const Index: Integer):Single;
begin
  result:=FCubeSize.V[Index];
end;

procedure TDGLCube.SetCubeSize(const Index:Integer;const aValue: Single);
begin
  if aValue <> FCubeSize.V[Index] then
  begin
    FCubeSize.V[Index] := aValue;
    StructureChanged;
  end;
end;

procedure TDGLCube.SetParts(aValue: TCubeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TDGLCube.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TDGLCube.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDGLCube) then
  begin
    FCubeSize        := TDGLCube(Source).FCubeSize;
    FParts           := TDGLCube(Source).FParts;
    FNormalDirection := TDGLCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
  BuildMesh;
end;

function TDGLCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := FCubeSize.X * 0.5;
  Result.Y := FCubeSize.Y * 0.5;
  Result.Z := FCubeSize.Z * 0.5;
  Result.W := 0;
end;

function TDGLCube.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  p:     array [0 .. 5] of TVector;
  rv:    TVector;
  rs, r: TVector;
  i:     Integer;
  t, e:  Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  e        := 0.5 + 0.0001; // Small value for floating point imprecisions
  eSize.X := FCubeSize.X * e;
  eSize.Y := FCubeSize.Y * e;
  eSize.Z := FCubeSize.Z * e;
  p[0]     := XHmgVector;
  p[1]     := YHmgVector;
  p[2]     := ZHmgVector;
  SetVector(p[3], -1, 0, 0);
  SetVector(p[4], 0, -1, 0);
  SetVector(p[5], 0, 0, -1);
  for i := 0 to 5 do
  begin
    if VectorDotProduct(p[i], rv) > 0 then
    begin
      t := -(p[i].X * rs.X + p[i].Y * rs.Y + p[i].Z * rs.Z + 0.5 * FCubeSize.V[i mod 3]) / (p[i].X * rv.X + p[i].Y * rv.Y + p[i].Z * rv.Z);
      MakePoint(r, rs.X + t * rv.X, rs.Y + t * rv.Y, rs.Z + t * rv.Z);
      if (Abs(r.X) <= eSize.X) and (Abs(r.Y) <= eSize.Y) and (Abs(r.Z) <= eSize.Z) and (VectorDotProduct(VectorSubtract(r, rs), rv) > 0) then
      begin
        if Assigned(intersectPoint) then
          MakePoint(intersectPoint^, LocalToAbsolute(r));
        if Assigned(intersectNormal) then
          MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
        Result := true;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TDGLCube.GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette;
var
  hw, hh, hd:   TGLFloat;
  connectivity: TConnectivity;
  sil:         TDGLSilhouette;
begin
  connectivity := TConnectivity.Create(true);

  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  if cpFront in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd), AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, -hh, hd), AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, -hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd), AffineVectorMake(hw, -hh, hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, hh, hd), AffineVectorMake(hw, hh, hd), AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, -hh, hd), AffineVectorMake(-hw, -hh, hd));
  end;

  sil := nil;
  connectivity.CreateSilhouette(silhouetteParameters, sil, False);
  Result := sil;
  connectivity.Free;
end;

procedure TDGLCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData, (FCubeSize.X <> 1) or (FCubeSize.Y <> 1) or (FCubeSize.Z <> 1));
end;

procedure TDGLCube.ReadData(Stream: TStream);
begin
  Stream.Read(FCubeSize, SizeOf(TAffineVector));
end;

procedure TDGLCube.WriteData(Stream: TStream);
begin
  Stream.Write(FCubeSize, SizeOf(TAffineVector));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

  RegisterClasses([TDGLPlane, TDGLCube]);
  //TDGLSprite, TDGLCube, TDGLSphere, TDGLGeoSphere, TDGLDisk, TDGLBilletMesh]);

  DefaultShader := TDGLProgramHandle.Create;


finalization

  DefaultShader.Destroy;
  DefaultShader := nil;

end.
