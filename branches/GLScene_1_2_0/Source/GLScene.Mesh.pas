//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_Mesh<p>

   <b>History : </b><font size=-1><ul>
    <li>25/05/11 - Yar - Added TInstancesChain
    <li>18/05/11 - Yar - Added RayCastIntersect
    <li>16/04/11 - Yar - Rewriten
    <li>15/10/10 - Yar - Creation
 </ul></font>
}

unit GLScene.Mesh;

interface

{$I GLScene.inc}
{$M-}

uses
  Classes,
  GLScene.Platform,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.Vector.Lists,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.Context,
  GLScene.Shader.Parameter,
  GLScene.Base.GLStateMachine,
  GLScene.Base.GeometryBB,
  GLScene.Base.Transformation,
  SyncObjs;

type

  TAttribLocation =
    (
    attrPosition = 0,
    attrNormal = 1,
    attrColor = 2,
    attrTangent = 3,
    attrBinormal = 4,
    attrTexCoord0 = 5,
    attrTexCoord1 = 6,
    attrTexCoord2 = 7,
    attrTexCoord3 = 8,
    attrTexCoord4 = 9,
    attrTexCoord5 = 10,
    attrTexCoord6 = 11,
    attrTexCoord7 = 12,
    attrCustom0 = 13,
    attrCustom1 = 14,
    attrCustom2 = 15
    );

  TMeshState = (mmsDefault, mmsAssembling, mmsPrimitives, mmsIgnoring);

  // TInstancesChain
  //
  TInstancesChain = class(TObject)
  private
    { Private Declarations }
{$IFDEF GLS_MULTITHREAD}
    FLock: TCriticalSection;
{$ENDIF}
    FBuildingState: TMeshState;
    FValid: Boolean;
    FLastRevision: Cardinal;
    FInstanceCount: Integer;
    function GetValid: Boolean;
    function GetAttributeDivisor(Attribs: TAttribLocation): TGLint;
    function GetAttributes(Attribs: TAttribLocation): Boolean;
    function GetAttributesType(Attribs: TAttribLocation): TGLSLDataType;
    procedure SetAttributeDivisor(Attribs: TAttribLocation;
      const Value: TGLint);
    procedure SetAttributes(Attribs: TAttribLocation; const Value: Boolean);
    procedure SetAttributesType(Attribs: TAttribLocation;
      const Value: TGLSLDataType);
    function GetAttributeLists(Attribs: TAttribLocation): T4ByteList;
    procedure SetAttributeLists(Attribs: TAttribLocation;
      const Value: T4ByteList);
    procedure SetTransformationEnabled(const Value: Boolean);
  protected
    FAttributes: array[TAttribLocation] of Boolean;
    FType: array[TAttribLocation] of TGLSLDataType;
    FAttributeArrays: array[TAttribLocation] of T4ByteList;
    FAttributeDivisor: array[TAttribLocation] of TGLint;
    FTransformationEnabled: Boolean;
    FTransformations: TList;
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    {: Begins working. }
    procedure Lock; virtual;
    {: Clear content. }
    procedure Clear;
    {: Ends working with mesh. }
    procedure UnLock; virtual;
    {: Current validation state. }
    property IsValid: Boolean read GetValid;

    {: Direct access to content. }
    property Attributes[Attribs: TAttribLocation]: Boolean
    read GetAttributes write SetAttributes;
    property AttributesType[Attribs: TAttribLocation]: TGLSLDataType
    read GetAttributesType write SetAttributesType;
    property AttributeDivisor[Attribs: TAttribLocation]: TGLint
    read GetAttributeDivisor write SetAttributeDivisor;
    property AttributeLists[Attribs: TAttribLocation]: T4ByteList
    read GetAttributeLists write SetAttributeLists;
    property TransformationEnabled: Boolean read FTransformationEnabled
      write SetTransformationEnabled;
    property Transformations: TList read FTransformations;
    property InstanceCount: Integer read FInstanceCount;
  end;

  TMeshExtra = (mesTangents, mesAdjacency, mesFastWireFrame, mesOctreeRayCast);
  TMeshExtras = set of TMeshExtra;

  //  TMeshPurpose = (fgpCommon, fgpInterior, fgpExterior, fgpOcluder,
  //    fgpControlCage);

  // TMeshAtom
  //
  TMeshAtom = class(TObject)
  private
    { Private Declarations }
    FAABB: TAABB;
    FDLO: TGLListHandle;
    FVAO_BuildIn: TGLVertexArrayHandle;
    FVAO_Generic: TGLVertexArrayHandle;
    FOwner: TComponent;
    function GetAttributes(Attribs: TAttribLocation): Boolean;
    function GetAttributesType(Attribs: TAttribLocation): TGLSLDataType;
    procedure SetAttributes(Attribs: TAttribLocation; const Value: Boolean);
    procedure SetAttributesType(Attribs: TAttribLocation;
      const Value: TGLSLDataType);
    procedure SetElements(const Value: T4ByteList);
    function GetValid: Boolean;
    procedure SetPrimitive(const Value: TGLMeshPrimitive);
    procedure SetOwner(const Value: TComponent);
  protected
    { Protected Declarations }
{$IFDEF GLS_MULTITHREAD}
    FLock: TCriticalSection;
{$ENDIF}
    FBuildingState: TMeshState;
    FCurrentAttribValue: array[TAttribLocation, 0..15] of T4ByteData;
    FRemoveLastElement: Boolean;
    FValid: Boolean;
    FTagName: string;
    FTagInteger: Integer;
    FRevisionNum: Integer;

    FPrimitive: TGLMeshPrimitive;
    FAttributes: array[TAttribLocation] of Boolean;
    FType: array[TAttribLocation] of TGLSLDataType;
    FTypeComponentCount: array[TAttribLocation] of Byte;
    FAttributeArrays: array[TAttribLocation] of T4ByteList;
    FVertexCount: Integer;

    FHasIndices: Boolean;
    FElements: T4ByteList;
    FAdjacencyElements: T4ByteList;
    FTrianglesElements: T4ByteList;
    FRestartIndex: TGLuint;
    FRestartVertex: TIntegerList;
    FStripCounts: TIntegerList;

    FBufferRevision: Integer;
    FArraySectorIndex: Integer;
    FElementSectorIndex: Integer;

    function GetAttributeCount: Integer;
    function GetAABB: TAABB;
    function GetAttributeIndex(Attrib: TAttribLocation;
      AType: TGLSLDataType): Boolean; {$IFDEF GLS_INLINE}inline;{$ENDIF}
    procedure ComputeBoundingBox;
    procedure DoOnPrepare(Sender: TGLContext);
    function GetDLO: TGLListHandle;
    function GetVAO_BuildIn: TGLVertexArrayHandle;
    function GetVAO_Generic: TGLVertexArrayHandle;
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(AMesh: TMeshAtom); virtual;

    {: Begins storing a piece of geometry }
    procedure Lock; virtual;
    {: Begins gathering information about the given type of primitives. }
    procedure BeginAssembly(APrimitiveType: TGLMeshPrimitive);
    {: Declare attibute and it type for use in mesh. }
    procedure DeclareAttribute(Attrib: TAttribLocation; AType: TGLSLDataType);
    {: Specifies a new value for the attribute with the given name. }
    procedure Attribute1f(Attrib: TAttribLocation; a1: GLfloat);
    procedure Attribute2f(Attrib: TAttribLocation; a1, a2: GLfloat);
      overload;
    procedure Attribute2f(Attrib: TAttribLocation; const a: TVector2f);
      overload;
    procedure Attribute2f(Attrib: TAttribLocation; const a: TTexPoint);
      overload;
    procedure Attribute3f(Attrib: TAttribLocation; a1, a2, a3: GLfloat);
      overload;
    procedure Attribute3f(Attrib: TAttribLocation; const a: TVector3f);
      overload;
    procedure Attribute4f(Attrib: TAttribLocation; a1, a2, a3, a4: GLfloat);
      overload;
    procedure Attribute4f(Attrib: TAttribLocation; const a: TVector4f);
      overload;
    procedure Attribute1i(Attrib: TAttribLocation; a1: GLint);
    procedure Attribute2i(Attrib: TAttribLocation; a1, a2: GLint);
      overload;
    procedure Attribute2i(Attrib: TAttribLocation; const a: TVector2i);
      overload;
    procedure Attribute3i(Attrib: TAttribLocation; a1, a2, a3: GLint);
      overload;
    procedure Attribute3i(Attrib: TAttribLocation; const a: TVector3i);
      overload;
    procedure Attribute4i(Attrib: TAttribLocation; a1, a2, a3, a4: GLint);
      overload;
    procedure Attribute4i(Attrib: TAttribLocation; const a: TVector4i);
      overload;
    procedure Attribute1ui(Attrib: TAttribLocation; a1: GLuint);
    procedure Attribute2ui(Attrib: TAttribLocation; a1, a2: GLuint);
      overload;
    procedure Attribute2ui(Attrib: TAttribLocation; const a: TVector2ui);
      overload;
    procedure Attribute3ui(Attrib: TAttribLocation; a1, a2, a3: GLuint);
      overload;
    procedure Attribute3ui(Attrib: TAttribLocation; const a: TVector3ui);
      overload;
    procedure Attribute4ui(Attrib: TAttribLocation; a1, a2, a3, a4: GLuint);
      overload;
    procedure Attribute4ui(Attrib: TAttribLocation; const a: TVector4ui);
      overload;
    procedure AttributeList(Attrib: TAttribLocation; AList: T4ByteList);
      overload;
    procedure AttributeList(Attrib: TAttribLocation; AList: TAffineVectorList);
      overload;
    procedure AttributeList(Attrib: TAttribLocation; AList: TVectorList);
      overload;
    {: Specifies a new vertex of a primitive. }
    procedure EmitVertex;
    {: Reserve space for vertices. They value is undefined.
       Main purpose is to allocate space for feedback. }
    procedure EmitVertices(ANumber: LongWord);
    {: Restart strip by hardware feature or degenerate primitive }
    procedure RestartStrip;
    {: Ends gathering information about the primitives. }
    procedure EndAssembly;
    {: Validate mesh after custom changes. }
    procedure Validate;
    {: Clear mesh content. }
    procedure Clear;
    {: Merge vertices of other mesh, leaving only same name attributes. }
    procedure Merge(AMesh: TMeshAtom);
    {: Flip face side, front <-> back, negate normals.
       True for TwoSide to duplicate triangles froom front side to back. }
    procedure FlipFaces(ATwoSide: Boolean = False);
    {: Weld equivalent vertices, rebuild element buffer. }
    procedure WeldVertices;
    {: Slit equivalent vertices, every element becomes unique. }
    procedure SplitVertices;
    {: Update element buffer where pure triangle indexing. }
    procedure MakeTriangleElements;
    {: Cast strip and fans of triangles to simple triangles. }
    procedure Triangulate;
    {: Cast strip and loop of lines to simple lines. }
    procedure LineSegmentation;
    {: Make additional element buffer with triangle adjacency (. }
    procedure MakeAdjacencyElements;
    {: Compute triangle's normals. }
    procedure ComputeNormals(ASmooth: Boolean = True);
    {: Compute triangle's texture coordinate of 0 channel. }
    procedure ComputeTexCoords;
    {: Compute triangle's normals. }
    procedure ComputeTangents;
    {: Transform positions, normals. }
    procedure Transform(const AMatrix: TMatrix);
    {: Rescales and alignes mesh based on bounding box. }
    procedure Rescale(ARadius: Single = 1.0);
    {: Intersection with a casted ray.
       Given coordinates & vector are in local coordinate system. }
    function RayCastIntersect(const ARayStart, ARayVector: TVector;
      out AnIntersectPoint, AnIntersectNormal: TVector): Boolean;
    {: Ends working with mesh. }
    procedure UnLock; virtual;
    {: Current validation state. }
    property IsValid: Boolean read GetValid;
    property Primitive: TGLMeshPrimitive read FPrimitive write SetPrimitive;
    property VertexCount: Integer read FVertexCount;
    property AttributeCount: Integer read GetAttributeCount;
    {: Direct asscess to content. }
    property Attributes[Attribs: TAttribLocation]: Boolean
    read GetAttributes write SetAttributes;
    property AttributesType[Attribs: TAttribLocation]: TGLSLDataType
    read GetAttributesType write SetAttributesType;
    property Elements: T4ByteList read FElements write SetElements;

    property TagName: string read FTagName write FTagName;
    property TagInteger: Integer read FTagInteger write FTagInteger;
    property Owner: TComponent read FOwner write SetOwner;
    property RestartStripIndex: TGLuint read FRestartIndex;

    {: This property returns the points defining the axis-
       aligned bounding box containing the model. }
    property AABB: TAABB read GetAABB;
  end;

  I4ByteListToVectorList = interface(IInterface)
    function Count: Integer;
    function Get(AIndex: Integer): TVector3f;
    function Get4f(AIndex: Integer): TVector4f;
    procedure Put(AIndex: Integer; const Value: TVector3f);
    property Items[Index: Integer]: TVector3f read Get write Put; default;
  end;

  T4ByteListToVectorList = class(TInterfacedObject, I4ByteListToVectorList)
  private
    FSource: P4ByteArrayList;
    FType: TGLSLDataType;
    FSize: Integer;
    FCount: Integer;
    function Get(AIndex: Integer): TVector3f;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
    procedure Put(AIndex: Integer; const Value: TVector3f);
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
    function Get4f(AIndex: Integer): TVector4f;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  public
    constructor Create(AList: T4ByteList; AType: TGLSLDataType);
    function Count: Integer;
  end;

function GetDefaultMesh: TMeshAtom;

var
  vAttributeNames: array[TAttribLocation] of AnsiString =
    (
    'Position',
    'Normal',
    'Color',
    'Tangent',
    'Binormal',
    'TexCoord0',
    'TexCoord1',
    'TexCoord2',
    'TexCoord3',
    'TexCoord4',
    'TexCoord5',
    'TexCoord6',
    'TexCoord7',
    'Custom0',
    'Custom1',
    'Custom2'
    );
  vUniformInstanceIDName: AnsiString = 'InstanceID';

  vUsePrimitiveRestart: Boolean = True;

implementation

uses
{$IFDEF FPC}
  LCLVersion,
  LCLType,
{$ENDIF}
{$IFNDEF GLS_DELPHI_2007_DOWN}
  GLScene.Base.RedBlackTree,
{$ENDIF}
  SysUtils,
  GLScene.Base.Log,
  GLScene.Base.Strings;

{$IFDEF GLS_DELPHI_2007_DOWN}
{$I OldDelphiSpike.inc}
{$ELSE}

{$IFDEF FPC}
{$IF (LCL_RELEASE < 31)}
{$DEFINE GLS_GENERIC_PREFIX}
{$IFEND}
{$ENDIF}

type
  TVertexHashMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GRedBlackTree < Double, Integer > ;

  TIntIntRBT = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GRedBlackTree < Integer, Integer > ;
{$ENDIF}

{$IFNDEF GLS_MULTITHREAD}
var
{$ELSE}
threadvar
{$ENDIF}
  vDefaultMesh: TMeshAtom;
  vTempMesh: TMeshAtom;

{$IFDEF GLS_REGION}{$REGION 'Helper functions'}{$ENDIF}

function GetDefaultMesh: TMeshAtom;
begin
  if not Assigned(vDefaultMesh) then
    vDefaultMesh := TMeshAtom.Create;
  Result := vDefaultMesh;
end;

procedure ReleaseMemory;
begin
  FreeAndNil(vDefaultMesh);
end;

function CompareVertexKey(const Item1, Item2: Double): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
    exit;
  end
  else if Item1 = Item2 then
  begin
    Result := -0;
    exit;
  end
  else
  begin
    Result := 1;
    exit;
  end
end;

function CompareVertex(const Item1, Item2: Integer): Boolean;
var
  A: TAttribLocation;
  Size: Integer;
  p1, p2: Pointer;
  BD: T4ByteData;
  Idx1, Idx2: Integer;
begin
  if Item1 <> Item2 then
  begin
    with vTempMesh do
    begin
      BD := FElements[Item1];
      Idx1 := Integer(BD.UInt.Value);
      BD := FElements[Item2];
      Idx2 := Integer(BD.UInt.Value);
      for a := High(TAttribLocation) downto Low(TAttribLocation) do
        if FAttributes[a] then
        begin
          size := FTypeComponentCount[a]+1;
          p1 := @FAttributeArrays[a].List[Idx1 * size];
          p2 := @FAttributeArrays[a].List[Idx2 * size];
          if not CompareMem(p1, p2, size * SizeOf(T4ByteData)) then
          begin
            Result := False;
            exit;
          end;
        end;
    end;
  end;
  Result := True;
end;

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
    exit;
  end
  else if Item1 = Item2 then
  begin
    Result := 0;
    exit;
  end
  else
  begin
    Result := 1;
    exit;
  end
end;

function CompareInteger_(const Item1, Item2: Integer): Boolean;
begin
  Result := Item1 = Item2;
end;

function VectorEquals_(const V1, V2: TAffineVector): Boolean;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
const
  EPSILON3 = 1e-3;
begin
  Result := (Abs(v1[0] - v2[0]) < EPSILON3)
    and (Abs(v1[1] - v2[1]) < EPSILON3)
    and (Abs(v1[2] - v2[2]) < EPSILON3);
end;

type
  TTriangleEdgeInfo = record
    adjacentTriangle: array[0..2] of LongWord;
    // Bits 0:1 is edge number of adjacent triangle 0
    // Bits 2:3 is edge number of adjacent triangle 1
    // Bits 4:5 is edge number of adjacent triangle 2
    adjacentTriangleEdges: Byte;
    openEdgeMask: Byte;
  end;

  TTriangleEdgeInfoArray = array of TTriangleEdgeInfo;

  TTriangleBoundary = record
    vertexIndex: LongWord;
    triangle: LongWord;
    edge: LongWord;
    prev: LongWord;
    next: array[0..2] of LongWord;
    active: LongWord;
    maxSqArea: Single;
  end;

  TTriangleBoundaryArray = array of TTriangleBoundary;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TInstancesChain'}{$ENDIF}

procedure TInstancesChain.Clear;
var
  A: TAttribLocation;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  for A := High(TAttribLocation) downto Low(TAttribLocation) do
  begin
    FAttributes[A] := False;
    FType[A] := GLSLTypeVoid;
    FAttributeArrays[A].Clear;
    FAttributeDivisor[A] := 0;
  end;

  FTransformations.Clear;
  FTransformationEnabled := False;
  FValid := False;
  FInstanceCount := 0;
end;

constructor TInstancesChain.Create;
var
  A: TAttribLocation;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock := SyncObjs.TCriticalSection.Create;
{$ENDIF}
  FValid := False;
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    FAttributeArrays[A] := T4ByteList.Create;
  FLastRevision := 0;
  FInstanceCount := 0;
  FTransformations := TList.Create;
  FTransformationEnabled := False;
end;

destructor TInstancesChain.Destroy;
var
  A: TAttribLocation;
begin
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    FreeAndNil(FAttributeArrays[A]);
  FTransformations.Destroy;
{$IFDEF GLS_MULTITHREAD}
  FLock.Destroy;
{$ENDIF}
  inherited;
end;

function TInstancesChain.GetAttributeDivisor(
  Attribs: TAttribLocation): TGLint;
begin
  Result := FAttributeDivisor[Attribs];
end;

function TInstancesChain.GetAttributeLists(
  Attribs: TAttribLocation): T4ByteList;
begin
  Result := FAttributeArrays[Attribs];
end;

function TInstancesChain.GetAttributes(Attribs: TAttribLocation): Boolean;
begin
  Result := FAttributes[Attribs];
end;

function TInstancesChain.GetAttributesType(
  Attribs: TAttribLocation): TGLSLDataType;
begin
  Result := FType[Attribs];
end;

function TInstancesChain.GetValid: Boolean;
var
  A: TAttribLocation;
begin
  Result := FValid and (FBuildingState = mmsDefault);
  if Result then
  begin
    for A := High(TAttribLocation) downto Low(TAttribLocation) do
      if FAttributeArrays[A].Revision <> FLastRevision then
      begin
        Result := False;
        exit;
      end;

    if FTransformationEnabled then
      if (FInstanceCount = 1) and (FTransformations.Count = 0) then
        Result := False
      else if (FInstanceCount > 1) and (FTransformations.Count <> FInstanceCount) then
        Result := False;
  end;
end;

procedure TInstancesChain.Lock;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock.Enter;
{$ENDIF}
  if not (FBuildingState in [mmsDefault, mmsIgnoring]) then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    exit;
  end;

  FBuildingState := mmsAssembling;
end;

procedure TInstancesChain.SetAttributeDivisor(Attribs: TAttribLocation;
  const Value: TGLint);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FAttributeDivisor[Attribs] := MaxInteger(Value, 0);
  FValid := False;
end;

procedure TInstancesChain.SetAttributeLists(Attribs: TAttribLocation;
  const Value: T4ByteList);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FAttributeArrays[Attribs].Assign(Value);
  FValid := False;
end;

procedure TInstancesChain.SetAttributes(Attribs: TAttribLocation;
  const Value: Boolean);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FAttributes[Attribs] := Value;
  FValid := False;
end;

procedure TInstancesChain.SetAttributesType(Attribs: TAttribLocation;
  const Value: TGLSLDataType);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FType[Attribs] := Value;
  FValid := False;
end;

procedure TInstancesChain.SetTransformationEnabled(const Value: Boolean);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FTransformationEnabled := Value;
  FValid := False;
end;

procedure TInstancesChain.UnLock;
var
  A: TAttribLocation;
  C, E: Integer;
  S: TMeshState;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock.Leave;
{$ENDIF}
  Inc(FLastRevision);
  FValid := False;
  S := FBuildingState;
  FBuildingState := mmsDefault;

  if S = mmsIgnoring then
    exit;

  if S <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [ClassName]);
    exit;
  end;

  E := 0;
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
  begin
    FAttributeArrays[A].Revision := FLastRevision;
    if FAttributes[A] then
    begin
      C := FAttributeArrays[A].Count;
      if C = 0 then
      begin
        FAttributes[A] := False;
        continue;
      end;
      C := C div GLSLTypeComponentCount(FType[A]);
      if FAttributeDivisor[A] > 1 then
        C := C div FAttributeDivisor[A];

      if E = 0 then
        E := C
      else if E <> C then
      begin
        GLSLogger.LogErrorFmt(glsMeshInvalidArraySize,
          [vAttributeNames[A], ClassName]);
        exit;
      end;
    end;
  end;

  if E > 0 then
  begin
    FInstanceCount := E;
    if FTransformationEnabled and (FTransformations.Count <> E) then
      exit;
    FValid := True;
  end
  else
  begin
    FInstanceCount := FTransformations.Count;
    FValid := FTransformationEnabled and (FInstanceCount > 0);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TMeshAtom'}{$ENDIF}

// ------------------
// ------------------ TMeshAtom ------------------
// ------------------

constructor TMeshAtom.Create;
var
  A: TAttribLocation;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock := SyncObjs.TCriticalSection.Create;
{$ENDIF}
  FValid := False;
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    FAttributeArrays[A] := T4ByteList.Create;
  FHasIndices := False;
  FRestartIndex := $FFFFFFFF;
  FElements := T4ByteList.Create;
  FAdjacencyElements := T4ByteList.Create;
  FTrianglesElements := T4ByteList.Create;
  FRestartVertex := TIntegerList.Create;
  FRestartVertex.Add(0);
  FStripCounts := TIntegerList.Create;
  FRevisionNum := 0;
  FBufferRevision := -1;
  FArraySectorIndex := -1;
  FElementSectorIndex := -1;
  FTagName := 'Nameless';

  FVAO_BuildIn := TGLVertexArrayHandle.Create;
  FVAO_BuildIn.OnPrapare := DoOnPrepare;
  FVAO_Generic := TGLVertexArrayHandle.Create;
  FVAO_Generic.OnPrapare := DoOnPrepare;
end;

destructor TMeshAtom.Destroy;
var
  A: TAttribLocation;
begin
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    FreeAndNil(FAttributeArrays[A]);
  FreeAndNil(FElements);
  FreeAndNil(FTrianglesElements);
  FreeAndNil(FAdjacencyElements);
  FreeAndNil(FRestartVertex);
  FreeAndNil(FStripCounts);
  FreeAndNil(FDLO);
  FreeAndNil(FVAO_BuildIn);
  FreeAndNil(FVAO_Generic);
{$IFDEF GLS_MULTITHREAD}
  FLock.Destroy;
{$ENDIF}
  inherited;
end;

function TMeshAtom.GetDLO: TGLListHandle;
begin
  if not Assigned(FDLO) then
  begin
    FDLO := TGLListHandle.Create;
    FDLO.OnPrapare := DoOnPrepare;
  end;
  Result := FDLO;
end;

function TMeshAtom.GetVAO_BuildIn: TGLVertexArrayHandle;
begin
  if not Assigned(FVAO_BuildIn) then
  begin
    FVAO_BuildIn := TGLVertexArrayHandle.Create;
    FVAO_BuildIn.OnPrapare := DoOnPrepare;
  end;
  Result := FVAO_BuildIn;
end;

function TMeshAtom.GetVAO_Generic: TGLVertexArrayHandle;
begin
  if not Assigned(FVAO_Generic) then
  begin
    FVAO_Generic := TGLVertexArrayHandle.Create;
    FVAO_Generic.OnPrapare := DoOnPrepare;
  end;
  Result := FVAO_Generic;
end;

procedure TMeshAtom.DoOnPrepare(Sender: TGLContext);

  function IsHandleNull(AHandle: TGLContextHandle): Boolean;
  begin
    Result := not Assigned(AHandle);
    if not Result then
      Result := AHandle.Handle = 0;
  end;

begin
  if IsHandleNull(FDLO)
    and IsHandleNull(FVAO_BuildIn)
    and IsHandleNull(FVAO_Generic) then
  begin
    FBufferRevision := -1;
    FArraySectorIndex := -1;
    FElementSectorIndex := -1;
  end;
end;

function TMeshAtom.GetAABB: TAABB;
const
  cZeroAABB: TAABB = (min: (0, 0, 0); max: (0, 0, 0); revision: 0);
begin
  if FAttributes[attrPosition] then
  begin
    if FAttributeArrays[attrPosition].Revision <> FAABB.revision then
      ComputeBoundingBox;
    Result := FAABB;
  end
  else
    Result := cZeroAABB;
end;

function TMeshAtom.GetAttributeCount: Integer;
var
  A: TAttribLocation;
  C: Integer;
begin
  C := 0;
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    if FAttributes[A] then
      Inc(C);
  Result := C;
end;

function TMeshAtom.GetAttributes(Attribs: TAttribLocation): Boolean;
begin
  Result := FAttributes[Attribs];
end;

function TMeshAtom.GetAttributesType(
  Attribs: TAttribLocation): TGLSLDataType;
begin
  Result := FType[Attribs];
end;

function TMeshAtom.GetValid: Boolean;
begin
  Result := FValid and (FBuildingState = mmsDefault);
end;

procedure TMeshAtom.SetAttributes(Attribs: TAttribLocation;
  const Value: Boolean);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FAttributes[Attribs] := Value;
  FValid := False;
end;

procedure TMeshAtom.SetAttributesType(Attribs: TAttribLocation;
  const Value: TGLSLDataType);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FType[Attribs] := Value;
  FValid := False;
end;

procedure TMeshAtom.SetElements(const Value: T4ByteList);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FElements := Value;
  FValid := False;
end;

procedure TMeshAtom.SetOwner(const Value: TComponent);
begin
  FOwner := Value;
end;

procedure TMeshAtom.SetPrimitive(const Value: TGLMeshPrimitive);
begin
  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FPrimitive := Value;
  FValid := False;
end;

procedure TMeshAtom.Lock;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock.Enter;
{$ENDIF}
  if not (FBuildingState in [mmsDefault, mmsIgnoring]) then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    exit;
  end;
  // Default mesh can be rebuilded
  if Self = vDefaultMesh then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;
  FBuildingState := mmsAssembling;
end;

procedure TMeshAtom.Clear;
var
  A: TAttribLocation;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  for A := High(TAttribLocation) downto Low(TAttribLocation) do
  begin
    FAttributes[A] := False;
    FType[A] := GLSLTypeVoid;
    FAttributeArrays[A].Clear;
  end;

  FHasIndices := False;
  FElements.Clear;
  FAdjacencyElements.Clear;
  FRestartVertex.Clear;
  FRestartVertex.Add(0);
  FPrimitive := mpNOPRIMITIVE;
  FVertexCount := 0;
  FValid := False;
end;

procedure TMeshAtom.Merge(AMesh: TMeshAtom);
var
  A: TAttribLocation;
  bSuccess: Boolean;
  I: Integer;
  E, J, C: Cardinal;
  BD: T4ByteData;
  Ptr: P4ByteArrayList;
begin
  if (FBuildingState = mmsIgnoring)
    or (AMesh.FBuildingState = mmsIgnoring) then
    exit;

  if (FBuildingState <> mmsAssembling)
    or (AMesh.FBuildingState <> mmsAssembling) then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if FPrimitive = mpNOPRIMITIVE then
  begin
    FPrimitive := AMesh.FPrimitive;
    FAttributes := AMesh.FAttributes;
    FType := AMesh.FType;
    FTypeComponentCount := AMesh.FTypeComponentCount;
    for A := high(TAttribLocation) downto low(TAttribLocation)  do
    begin
      FAttributeArrays[A].Assign(AMesh.FAttributeArrays[A]);
    end;
    FVertexCount := AMesh.FVertexCount;
    FHasIndices := AMesh.FHasIndices;
    FElements.Assign(AMesh.FElements);
    FAdjacencyElements.Assign(AMesh.FAdjacencyElements);
    FTrianglesElements.Assign(AMesh.FTrianglesElements);
    FRestartIndex := AMesh.FRestartIndex;
    FRestartVertex.Assign(AMesh.FRestartVertex);
    FStripCounts.Assign(AMesh.FStripCounts);
    FAABB := AMesh.FAABB;
    FValid := AMesh.FValid;
    Exit;
  end
  else if FPrimitive <> AMesh.FPrimitive then
  begin
    GLSLogger.LogWarningFmt(glsMeshNEquPrimitive, [TagName, AMesh.TagName]);
    exit;
  end;

  bSuccess := False;
  // Merge attributes
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
  begin
    if FAttributes[A]
      and AMesh.FAttributes[A]
      and (FType[A] = AMesh.FType[A]) then
    begin
      bSuccess := True;
    end
    else
    begin
      FAttributes[A] := False;
      FAttributeArrays[A].Flush;
      FType[A] := GLSLTypeVoid;
    end;
  end;
  // Append data
  if bSuccess then
  begin
    for I := 0 to AMesh.FElements.Count - 1 do
    begin
      BD := AMesh.FElements[I];
      E := BD.UInt.Value;
      if E = AMesh.RestartStripIndex then
      begin
        FElements.Add(FRestartIndex);
        FRestartVertex.Add(FVertexCount);
        continue;
      end;
      for A := High(TAttribLocation) downto Low(TAttribLocation) do
        if FAttributes[A] then
        begin
          C := FTypeComponentCount[A];
          Ptr := @AMesh.FAttributeArrays[A].List[(C+1)*E];
          for J := 0 to C do
            FAttributeArrays[A].Add(Ptr[J]);
        end;
      FElements.Add(FVertexCount);
      Inc(FVertexCount);
    end;
  end;
end;

procedure TMeshAtom.BeginAssembly(APrimitiveType: TGLMeshPrimitive);
begin
  if FBuildingState = mmsIgnoring then
    exit;
  Assert(APrimitiveType <> mpNOPRIMITIVE);

  if (FPrimitive <> mpNOPRIMITIVE)
    and (FPrimitive <> APrimitiveType) then
  begin
    GLSLogger.LogWarning(glsMeshMismatchPrimitive);
    FBuildingState := mmsIgnoring;
    exit;
  end;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if GetAttributeCount = 0 then
  begin
    GLSLogger.LogWarning(glsMeshNoAttrDeclar);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  FBuildingState := mmsPrimitives;
  FPrimitive := APrimitiveType;
end;

procedure TMeshAtom.EndAssembly;
var
  A: TAttribLocation;
  C: Integer;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsPrimitives then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;
  FBuildingState := mmsAssembling;
  FValid := False;

  if FVertexCount = 0 then
    exit;

  // Check attribute arrays equability
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    if FAttributes[A] then
    begin
      C := FAttributeArrays[A].Count;
      if C = 0 then
      begin
        FAttributes[A] := False;
        continue;
      end;
      C := C div GLSLTypeComponentCount(FType[A]);
      if FVertexCount <> C then
      begin
        GLSLogger.LogErrorFmt(glsMeshInvalidArraySize,
          [vAttributeNames[A], TagName]);
        exit;
      end;
    end;

  // Remove excesive restart index
  if FRemoveLastElement then
  begin
    if not vUsePrimitiveRestart then
      FElements.Pop;
    FElements.Pop;
    FRestartVertex.Pop;
  end;

  FHasIndices := False;

  case FPrimitive of
    mpTRIANGLES: FValid := (FVertexCount mod 3 = 0) and (FVertexCount > 2);
    mpTRIANGLE_STRIP, mpTRIANGLE_FAN: FValid := FVertexCount > 2;
    mpPOINTS: FValid := FVertexCount > 0;
    mpLINES: FValid := (FVertexCount mod 2 = 0) and (FVertexCount > 1);
    mpLINE_STRIP, mpLINE_LOOP: FValid := FVertexCount > 1;
    mpLINES_ADJACENCY: FValid := (FVertexCount mod 4 = 0) and (FVertexCount > 3);
    mpLINE_STRIP_ADJACENCY: FValid := FVertexCount > 4;
    mpTRIANGLES_ADJACENCY: FValid := (FVertexCount mod 6 = 0) and (FVertexCount > 5);
    mpTRIANGLE_STRIP_ADJACENCY: FValid := FVertexCount > 4;
    mpPATCHES: FValid := FVertexCount > 1;
  end;

  if not FValid then
    GLSLogger.LogError(glsInvalidNumberOfVertex);
end;

procedure TMeshAtom.FlipFaces(ATwoSide: Boolean = False);
var
  I, C: Integer;
  BD: T4ByteData;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if FPrimitive = mpTRIANGLES then
  begin
    if ATwoSide then
    begin
      I := 0;
      C := FElements.Count;
      while I < C do
      begin
        BD := FElements[I + 1];
        FElements.Add(BD);
        BD := FElements[I];
        FElements.Add(BD);
        BD := FElements[I+2];
        FElements.Add(BD);
        Inc(I, 3);
      end;
      SplitVertices;
      if FAttributes[attrNormal] and (FType[attrNormal] = GLSLType3f) then
        for I := FAttributeArrays[attrNormal].Count div 2 to FAttributeArrays[attrNormal].Count - 1 do
        begin
          BD := FAttributeArrays[attrNormal][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrNormal][I] := BD;
        end;
      if FAttributes[attrTangent] and (FType[attrTangent] = GLSLType3f) then
        for I := FAttributeArrays[attrTangent].Count div 2 to FAttributeArrays[attrTangent].Count - 1 do
        begin
          BD := FAttributeArrays[attrTangent][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrTangent][I] := BD;
        end;
      if FAttributes[attrBinormal] and (FType[attrBinormal] = GLSLType3f) then
        for I := FAttributeArrays[attrTangent].Count div 2 to FAttributeArrays[attrBinormal].Count - 1 do
        begin
          BD := FAttributeArrays[attrBinormal][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrBinormal][I] := BD;
        end;
    end
    else
    begin
      I := 0;
      while I < FElements.Count do
      begin
        BD := FElements[I];
        FElements[I] := FElements[I + 1];
        FElements[I + 1] := BD;
        Inc(I, 3);
      end;
      SplitVertices;
      if FAttributes[attrNormal] and (FType[attrNormal] = GLSLType3f) then
        for I := 0 to FAttributeArrays[attrNormal].Count - 1 do
        begin
          BD := FAttributeArrays[attrNormal][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrNormal][I] := BD;
        end;
      if FAttributes[attrTangent] and (FType[attrTangent] = GLSLType3f) then
        for I := 0 to FAttributeArrays[attrTangent].Count - 1 do
        begin
          BD := FAttributeArrays[attrTangent][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrTangent][I] := BD;
        end;
      if FAttributes[attrBinormal] and (FType[attrBinormal] = GLSLType3f) then
        for I := 0 to FAttributeArrays[attrBinormal].Count - 1 do
        begin
          BD := FAttributeArrays[attrBinormal][I];
          BD.Float.Value := -BD.Float.Value;
          FAttributeArrays[attrBinormal][I] := BD;
        end;
    end;
  end;
end;

procedure TMeshAtom.UnLock;
var
  S: TMeshState;
  I: Integer;
begin
{$IFDEF GLS_MULTITHREAD}
  FLock.Leave;
{$ENDIF}
  S := FBuildingState;
  FBuildingState := mmsDefault;

  if FValid then
  begin
    if S = mmsIgnoring then
    begin
      FValid := False;
      exit;
    end;

    if S <> mmsAssembling then
    begin
      FValid := False;
      GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
      exit;
    end;

    if not FHasIndices then
    begin
      FStripCounts.Count := FRestartVertex.Count;
      for I := 0 to FRestartVertex.Count - 2 do
      begin
        FStripCounts[I] := FRestartVertex[I+1] - FRestartVertex[I];
      end;
      FStripCounts[FRestartVertex.Count - 1] := FVertexCount - FRestartVertex[FRestartVertex.Count - 1];
    end;

    Inc(FRevisionNum);
    if not IsMainThread then
      ComputeBoundingBox;

    if Assigned(FDLO) then
      FDLO.NotifyChangesOfData;
    if Assigned(FVAO_BuildIn) then
      FVAO_BuildIn.NotifyChangesOfData;
    if Assigned(FVAO_Generic) then
      FVAO_Generic.NotifyChangesOfData;
  end;
end;

procedure TMeshAtom.Validate;
var
  C: Integer;
begin
  FValid := False;
  C := FElements.Count;
  case FPrimitive of
    mpTRIANGLES: FValid := (C mod 3 = 0) and (C > 2);
    mpTRIANGLE_STRIP, mpTRIANGLE_FAN: FValid := C > 2;
    mpPOINTS: FValid := C > 0;
    mpLINES: FValid := (C mod 2 = 0) and (C > 1);
    mpLINE_STRIP, mpLINE_LOOP: FValid := C > 2;
    mpLINES_ADJACENCY: FValid := (C mod 4 = 0) and (C > 3);
    mpLINE_STRIP_ADJACENCY: FValid := C > 4;
    mpTRIANGLES_ADJACENCY: FValid := (C mod 6 = 0) and (C > 5);
    mpTRIANGLE_STRIP_ADJACENCY: FValid := C > 4;
    mpPATCHES: FValid := C > 1;
  end;
end;

procedure TMeshAtom.DeclareAttribute(Attrib: TAttribLocation;
  AType: TGLSLDataType);
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if AType <> GLSLTypeVoid then
    if FAttributes[Attrib] then
    begin
      GLSLogger.LogErrorFmt(glsMeshExcesAttrib, [TagName]);
      FBuildingState := mmsIgnoring;
      exit;
    end;

  // Enable attribute
  FAttributes[Attrib] := True;
  FType[Attrib] := AType;
  FTypeComponentCount[Attrib] := GLSLTypeComponentCount(AType) - 1;
end;

function TMeshAtom.GetAttributeIndex(
  Attrib: TAttribLocation; AType: TGLSLDataType): Boolean;
begin
  if FBuildingState = mmsPrimitives then
  begin
    if FAttributes[Attrib] then
    begin
      // Check attribute type
      if (AType <> GLSLTypeVoid) and (FType[Attrib] <> AType) then
        GLSLogger.LogError(glsWrongAttrType);
      Result := True;
      exit;
    end
    else
      GLSLogger.LogErrorFmt(glsUnknownAttrib, [vAttributeNames[Attrib],
        TagName]);
  end;
  Result := False;
end;

procedure TMeshAtom.Assign(AMesh: TMeshAtom);
var
  A: TAttribLocation;
begin
  if Assigned(AMesh) and (Self <> AMesh) then
  begin
    if (FBuildingState <> mmsDefault) or
      (AMesh.FBuildingState <> mmsDefault) then
    begin
      GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
      exit;
    end;

    Lock;
    AMesh.Lock;
    try
      FPrimitive := AMesh.FPrimitive;
      FAttributes := AMesh.FAttributes;
      FType := AMesh.FType;
      FTypeComponentCount := AMesh.FTypeComponentCount;
      for A := high(TAttribLocation) downto low(TAttribLocation)  do
      begin
        FAttributeArrays[A].Assign(AMesh.FAttributeArrays[A]);
      end;
      FVertexCount := AMesh.FVertexCount;
      FHasIndices := AMesh.FHasIndices;
      FElements.Assign(AMesh.FElements);
      FAdjacencyElements.Assign(AMesh.FAdjacencyElements);
      FTrianglesElements.Assign(AMesh.FTrianglesElements);
      FRestartIndex := AMesh.FRestartIndex;
      FRestartVertex.Assign(AMesh.FRestartVertex);
      FStripCounts.Assign(AMesh.FStripCounts);
      FAABB := AMesh.FAABB;
      FValid := AMesh.FValid;
    finally
      UnLock;
      AMesh.UnLock;
    end;
  end;
end;

procedure TMeshAtom.Attribute1f(Attrib: TAttribLocation; a1:
  GLfloat);
begin
  if GetAttributeIndex(Attrib, GLSLType1F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a1;
  end;
end;

procedure TMeshAtom.Attribute2f(Attrib: TAttribLocation; a1, a2:
  GLfloat);
begin
  if GetAttributeIndex(Attrib, GLSLType2F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a1;
    FCurrentAttribValue[Attrib, 1].Float.Value := a2;
  end;
end;

procedure TMeshAtom.Attribute2f(Attrib: TAttribLocation;
  const a: TVector2f);
begin
  if GetAttributeIndex(Attrib, GLSLType2F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Float.Value := a[1];
  end;
end;

procedure TMeshAtom.Attribute2f(Attrib: TAttribLocation;
  const a: TTexPoint);
begin
  if GetAttributeIndex(Attrib, GLSLType2F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a.S;
    FCurrentAttribValue[Attrib, 1].Float.Value := a.T;
  end;
end;

procedure TMeshAtom.Attribute3f(Attrib: TAttribLocation;
  a1, a2, a3: GLfloat);
begin
  if GetAttributeIndex(Attrib, GLSLType3F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a1;
    FCurrentAttribValue[Attrib, 1].Float.Value := a2;
    FCurrentAttribValue[Attrib, 2].Float.Value := a3;
  end;
end;

procedure TMeshAtom.Attribute3f(Attrib: TAttribLocation;
  const a: TVector3f);
begin
  if GetAttributeIndex(Attrib, GLSLType3F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Float.Value := a[1];
    FCurrentAttribValue[Attrib, 2].Float.Value := a[2];
  end;
end;

procedure TMeshAtom.Attribute4f(Attrib: TAttribLocation;
  a1, a2, a3, a4: GLfloat);
begin
  if GetAttributeIndex(Attrib, GLSLType4F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a1;
    FCurrentAttribValue[Attrib, 1].Float.Value := a2;
    FCurrentAttribValue[Attrib, 2].Float.Value := a3;
    FCurrentAttribValue[Attrib, 3].Float.Value := a4;
  end;
end;

procedure TMeshAtom.Attribute4f(Attrib: TAttribLocation;
  const a: TVector4f);
begin
  if GetAttributeIndex(Attrib, GLSLType4F) then
  begin
    FCurrentAttribValue[Attrib, 0].Float.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Float.Value := a[1];
    FCurrentAttribValue[Attrib, 2].Float.Value := a[2];
    FCurrentAttribValue[Attrib, 3].Float.Value := a[3];
  end;
end;

procedure TMeshAtom.Attribute1i(Attrib: TAttribLocation;
  a1: GLint);
begin
  if GetAttributeIndex(Attrib, GLSLType1I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a1;
  end;
end;

procedure TMeshAtom.Attribute2i(Attrib: TAttribLocation; a1, a2:
  GLint);
begin
  if GetAttributeIndex(Attrib, GLSLType2I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a1;
    FCurrentAttribValue[Attrib, 1].Int.Value := a2;
  end;
end;

procedure TMeshAtom.Attribute2i(Attrib: TAttribLocation;
  const a: TVector2i);
begin
  if GetAttributeIndex(Attrib, GLSLType2I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Int.Value := a[1];
  end;
end;

procedure TMeshAtom.Attribute3i(Attrib: TAttribLocation;
  a1, a2, a3: GLint);
begin
  if GetAttributeIndex(Attrib, GLSLType3I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a1;
    FCurrentAttribValue[Attrib, 1].Int.Value := a2;
    FCurrentAttribValue[Attrib, 2].Int.Value := a3;
  end;
end;

procedure TMeshAtom.Attribute3i(Attrib: TAttribLocation;
  const a: TVector3i);
begin
  if GetAttributeIndex(Attrib, GLSLType3I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Int.Value := a[1];
    FCurrentAttribValue[Attrib, 2].Int.Value := a[2];
  end;
end;

procedure TMeshAtom.Attribute4i(Attrib: TAttribLocation;
  a1, a2, a3, a4: GLint);
begin
  if GetAttributeIndex(Attrib, GLSLType4I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a1;
    FCurrentAttribValue[Attrib, 1].Int.Value := a2;
    FCurrentAttribValue[Attrib, 2].Int.Value := a3;
    FCurrentAttribValue[Attrib, 3].Int.Value := a4;
  end;
end;

procedure TMeshAtom.Attribute4i(Attrib: TAttribLocation;
  const a: TVector4i);
begin
  if GetAttributeIndex(Attrib, GLSLType4I) then
  begin
    FCurrentAttribValue[Attrib, 0].Int.Value := a[0];
    FCurrentAttribValue[Attrib, 1].Int.Value := a[1];
    FCurrentAttribValue[Attrib, 2].Int.Value := a[2];
    FCurrentAttribValue[Attrib, 3].Int.Value := a[3];
  end;
end;

procedure TMeshAtom.Attribute1ui(Attrib: TAttribLocation;
  a1: GLuint);
begin
  if GetAttributeIndex(Attrib, GLSLType1UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a1;
  end;
end;

procedure TMeshAtom.Attribute2ui(Attrib: TAttribLocation; a1, a2:
  GLuint);
begin
  if GetAttributeIndex(Attrib, GLSLType2UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a1;
    FCurrentAttribValue[Attrib, 1].UInt.Value := a2;
  end;
end;

procedure TMeshAtom.Attribute2ui(Attrib: TAttribLocation;
  const a: TVector2ui);
begin
  if GetAttributeIndex(Attrib, GLSLType2UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a[0];
    FCurrentAttribValue[Attrib, 1].UInt.Value := a[1];
  end;
end;

procedure TMeshAtom.Attribute3ui(Attrib: TAttribLocation;
  a1, a2, a3: GLuint);
begin
  if GetAttributeIndex(Attrib, GLSLType3UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a1;
    FCurrentAttribValue[Attrib, 1].UInt.Value := a2;
    FCurrentAttribValue[Attrib, 2].UInt.Value := a3;
  end;
end;

procedure TMeshAtom.Attribute3ui(Attrib: TAttribLocation;
  const a: TVector3ui);
begin
  if GetAttributeIndex(Attrib, GLSLType3UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a[0];
    FCurrentAttribValue[Attrib, 1].UInt.Value := a[1];
    FCurrentAttribValue[Attrib, 2].UInt.Value := a[2];
  end;
end;

procedure TMeshAtom.Attribute4ui(Attrib: TAttribLocation;
  a1, a2, a3, a4: GLuint);
begin
  if GetAttributeIndex(Attrib, GLSLType4UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a1;
    FCurrentAttribValue[Attrib, 1].UInt.Value := a2;
    FCurrentAttribValue[Attrib, 2].UInt.Value := a3;
    FCurrentAttribValue[Attrib, 3].UInt.Value := a4;
  end;
end;

procedure TMeshAtom.Attribute4ui(Attrib: TAttribLocation;
  const a: TVector4ui);
begin
  if GetAttributeIndex(Attrib, GLSLType4UI) then
  begin
    FCurrentAttribValue[Attrib, 0].UInt.Value := a[0];
    FCurrentAttribValue[Attrib, 1].UInt.Value := a[1];
    FCurrentAttribValue[Attrib, 2].UInt.Value := a[2];
    FCurrentAttribValue[Attrib, 3].UInt.Value := a[3];
  end;
end;

procedure TMeshAtom.AttributeList(Attrib: TAttribLocation; AList:
  TAffineVectorList);
var
  AA: T4ByteList;
  Last: Integer;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if GetAttributeIndex(Attrib, GLSLTypeVoid) then
  begin
    if FType[Attrib] <> GLSLType3F then
    begin
      GLSLogger.LogWarning(glsWrongAttrType);
      FBuildingState := mmsIgnoring;
      exit;
    end;

    AA := FAttributeArrays[Attrib];
    Last := AA.Count;
    AA.Count := Last + 3 * AList.Count;
    System.Move(AList.List^, AA.List[Last], AList.DataSize);
    FVertexCount := AA.Count div 3;
  end;
end;

procedure TMeshAtom.AttributeList(Attrib: TAttribLocation; AList: TVectorList);
var
  AA: T4ByteList;
  Last: Integer;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if GetAttributeIndex(Attrib, GLSLTypeVoid) then
  begin
    if FType[Attrib] <> GLSLType4F then
    begin
      GLSLogger.LogWarning(glsWrongAttrType);
      FBuildingState := mmsIgnoring;
      exit;
    end;

    AA := FAttributeArrays[Attrib];
    Last := AA.Count;
    AA.Count := Last + 4 * AList.Count;
    System.Move(AList.List^, AA.List[Last], AList.DataSize);
    FVertexCount := AA.Count div 4;
  end;
end;

procedure TMeshAtom.AttributeList(Attrib: TAttribLocation; AList:
  T4ByteList);
var
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if GetAttributeIndex(Attrib, GLSLTypeVoid) then
  begin
    Valid := false;
    case FType[Attrib] of
      GLSLType1F: Valid := true;
      GLSLType2F: Valid := (AList.Count mod 2 = 0);
      GLSLType3F: Valid := (AList.Count mod 3 = 0);
      GLSLType4F: Valid := (AList.Count mod 4 = 0);
    end;
    if not Valid then
    begin
      GLSLogger.LogWarning(glsWrongAttrType);
      FBuildingState := mmsIgnoring;
      exit;
    end;

    AA := FAttributeArrays[Attrib];
    Last := AA.Count;
    AA.Count := Last + AList.Count;
    System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
    FVertexCount := AA.Count div GLSLTypeComponentCount(FType[Attrib]);
  end;
end;

procedure TMeshAtom.EmitVertex;
var
  A: TAttribLocation;
  C: Integer;
  Ptr: P4ByteArrayList;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsPrimitives then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  // Push vertex attributes into lists
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    if FAttributes[A] then
    begin
      Ptr := @FCurrentAttribValue[A][0];
      for C := 0 to FTypeComponentCount[A] do
        FAttributeArrays[A].Add(Ptr[C]);
    end;

  FElements.Add(FVertexCount);
  Inc(FVertexCount);
  FRemoveLastElement := False;
end;

procedure TMeshAtom.EmitVertices(ANumber: LongWord);
var
  A: TAttribLocation;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsPrimitives then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;
  // Increase vertex attributes lists
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    if FAttributes[A] then
    begin
      FAttributeArrays[A].Count := FAttributeArrays[A].Count +
        (FTypeComponentCount[A]+1) * Integer(ANumber);
    end;
  FElements.AddNulls(ANumber);
  Inc(FVertexCount, ANumber);
end;

procedure TMeshAtom.WeldVertices;
var
  StoreAttribArrays: array[TAttribLocation] of T4ByteList;
  StoreElementBuffer: T4ByteList;
  VertexIndex: LongWord;

  function SameVertex(const Item1, Item2: LongWord): Boolean;
  var
    AA: TAttribLocation;
    size: Integer;
    p1, p2: Pointer;
  begin
    if Item1 <> Item2 then
      for AA := High(TAttribLocation) downto Low(TAttribLocation) do
        if FAttributes[AA] then
        begin
          size := FTypeComponentCount[AA]+1;
          p1 := @StoreAttribArrays[AA].List[Integer(Item1) * size];
          p2 := @StoreAttribArrays[AA].List[Integer(Item2) * size];
          if not CompareMem(p1, p2, size * SizeOf(T4ByteData)) then
          begin
            Result := False;
            exit;
          end;
        end;
    Result := True;
  end;

  procedure CopyVertex(N: Integer);
  var
    AA: TAttribLocation;
    C, size: Integer;
    Ptr: P4ByteArrayList;
  begin
    for AA := High(TAttribLocation) downto Low(TAttribLocation) do
      if FAttributes[AA] then
      begin
        size := FTypeComponentCount[AA];
        Ptr := @StoreAttribArrays[AA].List[N * (size+1)];
        for C := 0 to size do
          FAttributeArrays[AA].Add(Ptr[C]);
      end;
    Inc(VertexIndex);
  end;

var
  A: TAttribLocation;
  I, J, Size: Integer;
  E, E_: LongWord;
  vertexKey: Double;
  VertexHashMap: TVertexHashMap;
  VertexHashKey: TDoubleList;
  BD: T4ByteData;
  bFind: Boolean;

  procedure CalcHashKay;
  var
    BD2: T4ByteData;
    pKey: ^Word;
    K: Integer;
  begin
    vertexKey := 0;
    pKey := @vertexKey;
    J := Integer(E) * Size;
    for K := Size - 1 downto 0 do
    begin
      BD2 := StoreAttribArrays[attrPosition][J + K];
      pKey^ := BD2.Word.Value[1];
      Inc(pKey);
    end;
    VertexHashKey[I] := vertexKey;
  end;

var
  HasHash: Boolean;

begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  vTempMesh := Self;
  E_ := 0; // Drop compilator warning

  // Add vertex to hash tree
  if FAttributes[attrPosition] then
  begin
    for A := High(TAttribLocation) downto Low(TAttribLocation) do
      StoreAttribArrays[A] := FAttributeArrays[A];

    // Calculate hash keys
    VertexHashMap := TVertexHashMap.Create(CompareVertexKey, CompareVertex);
    VertexHashKey := TDoubleList.Create;
    VertexHashKey.Count := FElements.Count;
    Size := FTypeComponentCount[attrPosition] + 1;
    for I := 0 to FElements.Count - 1 do
    begin
      E := FElements[I].UInt.Value;
      if E = FRestartIndex then
        continue;
      CalcHashKay;
      if VertexHashMap.Find(vertexKey, J) then
      begin
        BD := FElements[J];
        E_ := BD.UInt.Value;
        HasHash := (E_ >= E) or not SameVertex(E, E_);
      end
      else
        HasHash := True;
      if HasHash then
        VertexHashMap.Add(vertexKey, I);
    end;

    for A := High(TAttribLocation) downto Low(TAttribLocation) do
      FAttributeArrays[A] := T4ByteList.Create;
    StoreElementBuffer := T4ByteList.Create;
    StoreElementBuffer.Assign(FElements);

    // Remap element buffer, fill new attributes list
    VertexIndex := 0;
    for I := 0 to FElements.Count - 1 do
    begin
      BD := FElements[I];
      E := BD.UInt.Value;
      if E = FRestartIndex then
        continue;

      bFind := False;
      vertexKey := VertexHashKey[I];
      if VertexHashMap.Find(vertexKey, J) then
      begin
        repeat
          BD := StoreElementBuffer[J];
          E_ := BD.UInt.Value;
          bFind := SameVertex(E, E_);
          if bFind then
            break;
        until not VertexHashMap.NextDublicate(J);
      end;
      if not bFind then
      begin
        GLSLogger.LogWarningFmt(glsMeshHashingError,
          [ClassName, TagName]);
        continue;
      end;

      if E_ >= E then
      begin
        BD.UInt.Value := VertexIndex;
        FElements[I] := BD;
        CopyVertex(E);
      end
      else
      begin
        FElements[I] := FElements[J];
      end;
    end;

    // Free unpacked arrays
    for A := High(TAttribLocation) downto Low(TAttribLocation) do
      StoreAttribArrays[A].Destroy;
    StoreElementBuffer.Destroy;

    FHasIndices := True;
    FRestartIndex := FRestartIndex;

    VertexHashMap.Destroy;
    VertexHashKey.Destroy;
  end;
end;

procedure TMeshAtom.SplitVertices;
var
  StoreAttribArrays: array[TAttribLocation] of T4ByteList;
  StoreElementBuffer: T4ByteList;
  I, C, Size: Integer;
  A: TAttribLocation;
  E: LongWord;
  BD: T4ByteData;
  Ptr: P4ByteArrayList;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  // create new arrays
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
  begin
    StoreAttribArrays[A] := FAttributeArrays[A];
    FAttributeArrays[A] := T4ByteList.Create;
    FAttributeArrays[A].Capacity := StoreAttribArrays[A].Count;
  end;
  StoreElementBuffer := FElements;
  FElements := T4ByteList.Create;
  FElements.Capacity := StoreElementBuffer.Count;

  // unpack arrays
  for I := 0 to StoreElementBuffer.Count - 1 do
  begin
    BD := StoreElementBuffer[I];
    E := BD.UInt.Value;
    for A := High(TAttribLocation) downto Low(TAttribLocation) do
      if FAttributes[A] then
      begin
        size := FTypeComponentCount[A];
        Ptr := @StoreAttribArrays[A].List[Integer(E) * (Size+1)];
        for C := 0 to Size do
          FAttributeArrays[A].Add(Ptr[C]);
      end;
    FElements.Add(I);
  end;

  // Free packed arrays
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    StoreAttribArrays[A].Destroy;
  StoreElementBuffer.Destroy;

  FVertexCount := FElements.Count;

  FHasIndices := False;
end;

procedure TMeshAtom.Triangulate;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if FPrimitive in [mpTRIANGLE_STRIP, mpTRIANGLE_FAN] then
  begin
    if FTrianglesElements.Revision <> FElements.Revision then
      MakeTriangleElements;
    FElements.Assign(FTrianglesElements);
    FTrianglesElements.Revision := FElements.Revision;
    FPrimitive := mpTRIANGLES;
    // Triangles no need restarting
    FRestartVertex.Clear;
    FRestartVertex.Add(0);
    FHasIndices := True;
  end;
end;

procedure TMeshAtom.LineSegmentation;
var
  I: Integer;
  LElemets: T4ByteList;
  BD: T4ByteData;
  bRst: Boolean;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if FPrimitive = mpLINE_LOOP then
  begin
    LElemets := T4ByteList.Create;
    LElemets.Add(FElements[0]);
    for I := 1 to FElements.Count - 1 do
    begin
      BD := FElements[I];
      LElemets.Add(BD);
      LElemets.Add(BD);
    end;
    LElemets.Add(FElements[0]);
    FElements.Destroy;
    FElements := LElemets;
    FPrimitive := mpLINES;
  end
  else if FPrimitive = mpLINE_STRIP then
  begin
    LElemets := T4ByteList.Create;
    LElemets.Add(FElements[0]);
    bRst := False;
    for I := 1 to FElements.Count - 1 do
    begin
      BD := FElements[I];
      if BD.UInt.Value = RestartStripIndex then
      begin
        LElemets.Pop;
        bRst := True;
        continue;
      end;
      LElemets.Add(BD);
      if not bRst then
        LElemets.Add(BD);
      bRst := False;
    end;
    FElements.Destroy;
    FElements := LElemets;
    FPrimitive := mpLINES;
    FRestartVertex.Clear;
    FRestartVertex.Add(0);
  end
end;

procedure TMeshAtom.MakeTriangleElements;
var
  J: Integer;
  Index, prevIndex1, prevIndex2, stripCount: LongWord;
  centerIndex, prevIndex, fansCount: LongWord;
  degenerate: Boolean;
  BD: T4ByteData;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  case FPrimitive of
    mpTRIANGLES:
      begin
        FTrianglesElements.Assign(FElements);
      end;

    mpTRIANGLE_STRIP:
      begin
        FTrianglesElements.Clear;
        stripCount := 0;
        prevIndex1 := 0;
        prevIndex2 := 0;
        for J := 0 to FElements.Count - 1 do
        begin
          BD := FElements[J];
          Index := BD.UInt.Value;
          if stripCount > 2 then
          begin
            // Check for restart index
            if Index = FRestartIndex then
            begin
              stripCount := 0;
              continue;
            end
              // Check for degenerate triangles
            else if Index = prevIndex1 then
            begin
              continue;
            end
            else if prevIndex1 = prevIndex2 then
            begin
              stripCount := 0;
              continue;
            end;
            if (stripCount and 1) = 0 then
            begin
              FTrianglesElements.Add(prevIndex2);
              FTrianglesElements.Add(prevIndex1);
            end
            else
            begin
              FTrianglesElements.Add(prevIndex1);
              FTrianglesElements.Add(prevIndex2);
            end;
          end
          else if stripCount = 2 then
          begin
            FTrianglesElements.Add(Index);
            BD.UInt.Value := prevIndex1;
            FTrianglesElements.Items[FTrianglesElements.Count - 2] := BD;
            prevIndex2 := prevIndex1;
            prevIndex1 := Index;
            Inc(stripCount);
            continue;
          end;
          FTrianglesElements.Add(Index);
          prevIndex2 := prevIndex1;
          prevIndex1 := Index;
          Inc(stripCount);
        end;
      end;

    mpTRIANGLE_FAN:
      begin
        FTrianglesElements.Clear;
        fansCount := 0;
        prevIndex := 0;
        degenerate := False;
        BD := FElements[0];
        centerIndex := BD.UInt.Value;
        for J := 0 to FElements.Count - 1 do
        begin
          BD := FElements[J];
          Index := BD.UInt.Value;
          if fansCount > 2 then
          begin
            // Check for restart index
            if Index = FRestartIndex then
            begin
              fansCount := 0;
              continue;
            end
              // Check for degenerate triangles
            else if Index = prevIndex then
            begin
              degenerate := true;
              continue;
            end
            else if degenerate then
            begin
              degenerate := false;
              fansCount := 0;
              continue;
            end;
            FTrianglesElements.Add(centerIndex);
            FTrianglesElements.Add(prevIndex);
          end
          else if fansCount = 0 then
            centerIndex := Index;
          FTrianglesElements.Add(Index);
          prevIndex := Index;
          Inc(fansCount);
        end;
      end;
  else
    FTrianglesElements.Clear;
  end; // of case
  FTrianglesElements.Revision := FElements.Revision;
end;

procedure TMeshAtom.MakeAdjacencyElements;
var
  Positions: I4ByteListToVectorList;
  LElements: T4ByteList;
var
  edgeInfo: TTriangleEdgeInfoArray;
  triangleNum: Integer;

  function sameVertex(i0, i1: LongWord): Boolean;
  begin
    Result := VectorEquals_(Positions[i0], Positions[i1]);
  end;

  procedure joinTriangles(
    tri1: Integer; edge1: Cardinal;
    tri2: Integer; edge2: Cardinal);
  begin
    Assert((edge1 < 3) and (edge2 < 3),
      'joinTriangles: Multiple edge detected.');

    edgeInfo[tri1].adjacentTriangle[edge1] := tri2;
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1].adjacentTriangleEdges
      and not (3 shl (2 * edge1));
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1].adjacentTriangleEdges
      or (edge2 shl (2 * edge1));

    edgeInfo[tri2].adjacentTriangle[edge2] := tri1;
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2].adjacentTriangleEdges
      and not (3 shl (2 * edge2));
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2].adjacentTriangleEdges
      or (edge1 shl (2 * edge2));
  end;

  procedure matchWithTriangleSharingEdge(triangle, edge, v0, v1, otherv:
    LongWord);
  var
    i: Integer;
    doubleTri: Integer;
    otherEdge: Integer;
    vertexIndex: ^TVector4ui;
  begin
    doubleTri := -1;
    otherEdge := 0;
    // Match shared edges based on vertex numbers (relatively fast).
    for i := triangle + 1 to triangleNum - 1 do
    begin
      vertexIndex := @LElements.List[i * 3];

      if vertexIndex[0] = v0 then
        if vertexIndex[2] = v1 then
          if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
            if vertexIndex[1] = otherv then
            begin
              if (doubleTri < 0) then
              begin
                doubleTri := i;
                otherEdge := 2;
              end;
            end
            else
            begin
              joinTriangles(i, 2, triangle, edge);
              Exit;
            end;

      if vertexIndex[1] = v0 then
        if vertexIndex[0] = v1 then
          if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
            if vertexIndex[2] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 0;
              end;
            end
            else
            begin
              joinTriangles(i, 0, triangle, edge);
              Exit;
            end;

      if vertexIndex[2] = v0 then
        if vertexIndex[1] = v1 then
          if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
            if vertexIndex[0] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 1;
              end;
            end
            else
            begin
              joinTriangles(i, 1, triangle, edge);
              Exit;
            end;
    end;

    // Only connect a triangle to a triangle with the exact
    // same three vertices as a last resort.
    if doubleTri >= 0 then
      joinTriangles(doubleTri, otherEdge, triangle, edge);
  end;

  procedure CheckForBogusAdjacency;

    function AdjacentEdge(x, n: Integer): Integer;
    begin
      Result := (x shr (2 * n)) and 3;
    end;

  var
    i, j: Integer;
    adjacentTriangle, adjacentTriangleSharedEdge: LongWord;
  begin
    for i := 0 to triangleNum - 1 do
      for j := 0 to 2 do
      begin
        adjacentTriangleSharedEdge :=
          AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, j);
        adjacentTriangle := edgeInfo[i].adjacentTriangle[j];
        if adjacentTriangle <> $FFFFFFFF then
        begin
          Assert(adjacentTriangleSharedEdge < 3);
          Assert(edgeInfo[adjacentTriangle].adjacentTriangle[adjacentTriangleSharedEdge] = LongWord(i));
          Assert(AdjacentEdge(edgeInfo[adjacentTriangle].adjacentTriangleEdges,
            adjacentTriangleSharedEdge) = j);
        end
        else
          Assert(adjacentTriangleSharedEdge = 3);
      end;
  end;

  function AdjacentEdge(x, n: Integer): Integer;
  begin
    Result := (x shr (2 * n)) and 3;
  end;

var
  I, J: Integer;
  vertexIndex: ^TVector4ui;
  N, ii, jj: LongWord;
  tri, adjtri: ^TVector3ui;
  BD: T4ByteData;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if not FHasIndices then
    WeldVertices;
  if FTrianglesElements.Revision <> FElements.Revision then
    MakeTriangleElements;

  FAdjacencyElements.Clear;

  if FAttributes[attrPosition] and (FTrianglesElements.Count > 0) then
  begin
    Positions :=
      T4ByteListToVectorList.Create(FAttributeArrays[attrPosition],
      FType[attrPosition]);

    // Lets make element list with position based welded vertices
    LElements := T4ByteList.Create;
    try
      for I := 1 to FTrianglesElements.Count - 1 do
      begin
        BD := FTrianglesElements[I];
        ii := BD.UInt.Value;
        for J := 0 to I - 1 do
        begin
          BD := FTrianglesElements[J];
          jj := BD.UInt.Value;
          if ii = jj then
            continue;
          if sameVertex(ii, jj) then
          begin
            FTrianglesElements[I] := BD;
            break;
          end;
        end;
      end;

      // Remove degenerate triangles
      triangleNum := 0;
      for I := 0 to FTrianglesElements.Count div 3 - 1 do
      begin
        vertexIndex :=  @FTrianglesElements.List[I * 3];
        if (vertexIndex[0] = vertexIndex[1])
          or (vertexIndex[0] = vertexIndex[2])
          or (vertexIndex[1] = vertexIndex[2]) then
          continue;
        LElements.Add(vertexIndex[0], vertexIndex[1], vertexIndex[2]);
        Inc(triangleNum);
      end;

      // Initialize edge information as if all triangles are fully disconnected.
      SetLength(edgeInfo, triangleNum);
      for I := 0 to triangleNum - 1 do
      begin
        edgeInfo[i].adjacentTriangle[0] := $FFFFFFFF; // Vertex 0,1 edge
        edgeInfo[i].adjacentTriangle[1] := $FFFFFFFF; // Vertex 1,2 edge
        edgeInfo[i].adjacentTriangle[2] := $FFFFFFFF; // Vertex 2,0 edge
        edgeInfo[i].adjacentTriangleEdges := (3 shl 0) or (3 shl 2) or (3 shl
          4);
        edgeInfo[i].openEdgeMask := 0;
      end;

      try
        for I := 0 to triangleNum - 1 do
        begin
          vertexIndex := @LElements.List[I * 3];
          if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
            matchWithTriangleSharingEdge(i, 0,
              vertexIndex[0], vertexIndex[1], vertexIndex[2]);
          if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
            matchWithTriangleSharingEdge(i, 1,
              vertexIndex[1], vertexIndex[2], vertexIndex[0]);
          if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
            matchWithTriangleSharingEdge(i, 2,
              vertexIndex[2], vertexIndex[0], vertexIndex[1]);
        end;

        CheckForBogusAdjacency;

        FAdjacencyElements.SetCountResetsMemory := False;
        FAdjacencyElements.Capacity := 2 * LElements.Count;

        for i := 0 to triangleNum - 1 do
        begin
          N := 3 * i;
          tri := @LElements.List[N];
          for j := 0 to 2 do
          begin
            FAdjacencyElements.Add(tri^[j]);
            N := edgeInfo[i].adjacentTriangle[j];
            if N = $FFFFFFFF then
            begin
              jj := (j + 2) mod 3;
              FAdjacencyElements.Add(tri^[jj]);
            end
            else
            begin
              N := 3 * N;
              adjtri := @LElements.List[N];
              ii := (AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, j) + 2) mod
                3;
              FAdjacencyElements.Add(adjtri^[ii]);
            end;
          end;
        end;
      except
        FAdjacencyElements.Clear;
        FAdjacencyElements.Revision := FElements.Revision;
      end;
    finally
      LElements.Destroy;
    end;
  end;
  FAdjacencyElements.Revision := FElements.Revision;
end;

procedure TMeshAtom.ComputeNormals(ASmooth: Boolean);
var
  Positions: I4ByteListToVectorList;

  function sameVertex(i0, i1: Integer): Boolean;
  begin
    Result := VectorEquals_(Positions[i0], Positions[i1]);
  end;

var
  T, I, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, fNormal, nNormal, cNormal: TVector3f;
  newNormals: TAffineVectorList;
  newNormalIndices: TIntegerList;
  collisionMap: TIntIntRBT;
  Agrees: Boolean;
  pV: ^TVector3ui;
  BD: T4ByteData;
begin

  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  Triangulate;
  // Clear old normals
  FAttributes[attrNormal] := False;
  FAttributeArrays[attrNormal].Clear;
  Positions :=
    T4ByteListToVectorList.Create(FAttributeArrays[attrPosition],
    FType[attrPosition]);

  if ASmooth then
  begin
    if not FHasIndices then
    begin
      // Lets make element list with position based welded vertices
      MakeTriangleElements;
      for I := 1 to FTrianglesElements.Count - 1 do
      begin
        BD := FTrianglesElements[I];
        E := BD.Int.Value;
        for J := 0 to I - 1 do
        begin
          BD := FTrianglesElements[J];
          E_ := BD.Int.Value;
          if E = E_ then
            continue;
          if sameVertex(E, E_) then
          begin
            FTrianglesElements[I] := BD;
            break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    if FHasIndices then
      SplitVertices;
    MakeTriangleElements;
  end;

  // Allocate and initialize the normal values
  newNormals := TAffineVectorList.Create;
  newNormals.SetCountResetsMemory := True;
  newNormals.Count := FTrianglesElements.Count;
  newNormalIndices := TIntegerList.Create;

  // The collision map records any alternate locations for the normals
  if ASmooth then
  begin
    collisionMap := TIntIntRBT.Create(CompareInteger, CompareInteger_);
    collisionMap.DuplicateKeys := True;
  end
  else
    collisionMap := nil;

  // Iterate over the faces, computing the face normal and summing it them
  for T := 0 to FTrianglesElements.Count div 3 - 1 do
  begin
    E := 3 * T;
    pV := @FTrianglesElements.List[E];
    p0 := Positions[pV^[0]];
    p1 := Positions[pV^[1]];
    p2 := Positions[pV^[2]];

    // Compute the edge vectors
    dp0 := VectorSubtract(p1, p0);
    dp1 := VectorSubtract(p2, p0);

    // Compute the face normal
    fNormal := VectorCrossProduct(dp0, dp1);

    if not ASmooth then
    begin
      newNormals[E] := fNormal;
      newNormalIndices.Add(E);
      Inc(E);
      newNormals[E] := fNormal;
      newNormalIndices.Add(E);
      Inc(E);
      newNormals[E] := fNormal;
      newNormalIndices.Add(E);
      continue;
    end;

    // Compute a normalized normal
    nNormal := VectorNormalize(fNormal);

    // Iterate over the vertices, adding the face normal influence to each
    for J := 0 to 2 do
    begin
      // Get the current normal from the default location (index shared with position)
      EJ := pV^[J];
      cNormal := newNormals[EJ];

      // Check to see if this normal has not yet been touched
      if VectorIsNull(cNormal) then
      begin
        // First instance of this index, just store it as is
        newNormals[EJ] := fNormal;
        newNormalIndices.Add(EJ);
      end
      else
      begin
        // Check for agreement
        NormalizeVector(cNormal);

        if VectorDotProduct(cNormal, nNormal) >= cos(3.1415926 * 0.333333) then
        begin
          // Normal agrees, so add it
          newNormals[EJ] := VectorAdd(newNormals[EJ], fNormal);
          newNormalIndices.Add(EJ);
        end
        else
        begin
          // Normals disagree, this vertex must be along a facet edge
          Agrees := False;
          if collisionMap.Find(EJ, E_) then
          begin
            // Loop through all hits on this index, until one agrees
            repeat
              cNormal := newNormals[E_];
              NormalizeVector(cNormal);
              if VectorDotProduct(cNormal, nNormal) >= cos(3.1415926 * 0.333333) then
              begin
                Agrees := True;
                break;
              end;
            until not collisionMap.NextDublicate(E_);
          end;

          // Check for agreement with an earlier collision
          if Agrees then
          begin
            // Found agreement with an earlier collision, use that one
            newNormals[E_] := VectorAdd(newNormals[E_], fNormal);
            newNormalIndices.Add(E_);
          end
          else
          begin
            // We have a new collision, create a new normal
            newNormalIndices.Add(newNormals.Count);
            collisionMap.Add(EJ, newNormals.Count);
            newNormals.Add(fNormal);
          end;
        end; // else ( if normal agrees)
      end; // else (if normal is uninitialized)
    end; // for each vertex in triangle
  end; // for each face

  //now normalize all the normals
  newNormals.Normalize;

  // Place new normals
  if ASmooth then
    SplitVertices;

  FAttributes[attrNormal] := True;
  FAttributeArrays[attrNormal].Count := 3 * newNormalIndices.Count;
  FType[attrNormal] := GLSLType3F;
  for I := 0 to newNormalIndices.Count - 1 do
  begin
    E := newNormalIndices[I];
    BD := FElements[I];
    E_ := 3 * BD.Int.Value;
    Move(newNormals.List[E], FAttributeArrays[attrNormal].List[E_],
      SizeOf(TVector3f));
  end;
  WeldVertices;

  newNormals.Destroy;
  newNormalIndices.Destroy;
  collisionMap.Free;
end;

procedure TMeshAtom.ComputeTexCoords;
var
  Positions: I4ByteListToVectorList;
  I, T, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector3f;
  TBN: TMatrix3f;
  newTexCoords: TTexPointList;
  BD: T4ByteData;
  TT: TTexPoint;
  pV: ^TVector3ui;
begin
  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  // Clear old texture coordinates
  FAttributes[attrTexCoord0] := False;
  FAttributeArrays[attrTexCoord0].Clear;

  if not FHasIndices then
    WeldVertices;

  Triangulate;

  Positions := T4ByteListToVectorList.Create(FAttributeArrays[attrPosition], FType[attrPosition]);

  // Allocate and initialize the tangent values
  newTexCoords := TTexPointList.Create;
  newTexCoords.SetCountResetsMemory := True;
  newTexCoords.Count := FElements.Count;

  EJ := 0;
  for T := 0 to FElements.Count div 3 - 1 do
  begin
    E := 3 * T;
    pV := @FElements.List[E];
    p0 := Positions[pV^[0]];
    p1 := Positions[pV^[1]];
    p2 := Positions[pV^[2]];

    // Compute the edge vectors
    dp0 := VectorSubtract(p1, p0);
    dp1 := VectorSubtract(p2, p0);

    // Compute the face TBN
    fNormal := VectorCrossProduct(dp0, dp1);
    NormalizeVector(fNormal);
    fTangent := dp0;
    NormalizeVector(fTangent);
    fBinormal := VectorCrossProduct(fNormal, fTangent);
    TBN[0] := fTangent;
    TBN[1] := fBinormal;
    TBN[2] := fNormal;
    InvertMatrix(TBN);

    p0 := VectorTransform(p0, TBN);
    TT.S := p0[0];
    TT.T := p0[1];
    newTexCoords[EJ] := TT;
    Inc(EJ);

    p1 := VectorTransform(p1, TBN);
    TT.S := p1[0];
    TT.T := p1[1];
    newTexCoords[EJ] := TT;
    Inc(EJ);

    p2 := VectorTransform(p2, TBN);
    TT.S := p2[0];
    TT.T := p2[1];
    newTexCoords[EJ] := TT;
    Inc(EJ);
  end;

  // Place new texture coordinates
  SplitVertices;
  FAttributes[attrTexCoord0] := True;
  FAttributeArrays[attrTexCoord0].Count := 2 * newTexCoords.Count;
  FType[attrTexCoord0] := GLSLType2F;
  for I := 0 to newTexCoords.Count - 1 do
  begin
    BD := FElements[I];
    E_ := 2 * BD.Int.Value;
    TT := newTexCoords[I];
    BD.Float.Value := TT.S;
    FAttributeArrays[attrTexCoord0].Items[E_ + 0] := BD;
    BD.Float.Value := TT.T;
    FAttributeArrays[attrTexCoord0].Items[E_ + 1] := BD;
  end;

  newTexCoords.Destroy;
end;

procedure TMeshAtom.ComputeTangents;
var
  Positions: I4ByteListToVectorList;
  TexCoords: I4ByteListToVectorList;

  function sameVertex(i0, i1: Integer): Boolean;
  begin
    Result := VectorEquals_(Positions[i0], Positions[i1]);
  end;

var
  T, I, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1,
    st0, st1, st2, dst0, dst1,
    fTangent, nTangent, cTangent: TVector3f;
  factor: Single;
  newTangents: TAffineVectorList;
  newTangentIndices: TIntegerList;
  collisionMap: TIntIntRBT;
  Agrees: Boolean;
  BD: T4ByteData;
  pV: ^TVector3ui;
begin
  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  if not FAttributes[attrTexCoord0] then
  begin
    ComputeTexCoords;
    if not FAttributes[attrTexCoord0] then
      exit; // Force Majeure
  end;

  Triangulate;

  Positions := T4ByteListToVectorList.Create(FAttributeArrays[attrPosition], FType[attrPosition]);

  TexCoords := T4ByteListToVectorList.Create(FAttributeArrays[attrTexCoord0], FType[attrTexCoord0]);

  // Clear old tangents
  FAttributes[attrTangent] := False;
  FAttributeArrays[attrTangent].Clear;

  if not FHasIndices then
  begin
    // Lets make element list with position based welded vertices
    MakeTriangleElements;
    for I := 1 to FTrianglesElements.Count - 1 do
    begin
      BD := FTrianglesElements[I];
      E := BD.Int.Value;
      for J := 0 to I - 1 do
      begin
        BD := FTrianglesElements[J];
        E_ := BD.Int.Value;
        if E = E_ then
          continue;
        if sameVertex(E, E_) then
        begin
          FTrianglesElements[I] := BD;
          break;
        end;
      end;
    end;
  end;

  Triangulate;

  if FTrianglesElements.Revision <> FElements.Revision then
    MakeTriangleElements;

  // Allocate and initialize the tangent values
  newTangents := TAffineVectorList.Create;
  newTangents.SetCountResetsMemory := True;
  newTangents.Count := FTrianglesElements.Count;
  newTangentIndices := TIntegerList.Create;

  // The collision map records any alternate locations for the tangent
  collisionMap := TIntIntRBT.Create(CompareInteger, CompareInteger_);
  collisionMap.DuplicateKeys := True;

  // Iterate over the faces, computing the face normal and summing it them
  for T := 0 to FTrianglesElements.Count div 3 - 1 do
  begin
    E := 3 * T;
    pV := @FTrianglesElements.List[E];
    p0 := Positions[pV^[0]];
    p1 := Positions[pV^[1]];
    p2 := Positions[pV^[2]];
    st0 := TexCoords[pV^[0]];
    st1 := TexCoords[pV^[1]];
    st2 := TexCoords[pV^[2]];

    // Compute the edge and tc differentials
    dp0 := VectorSubtract(p1, p0);
    dp1 := VectorSubtract(p2, p0);
    dst0 := VectorSubtract(st1, st0);
    dst1 := VectorSubtract(st2, st0);

    factor := 1.0 / (dst0[0] * dst1[1] - dst1[0] * dst0[1]);

    //compute fTangent
    fTangent[0] := dp0[0] * dst1[1] - dp1[0] * dst0[1];
    fTangent[1] := dp0[1] * dst1[1] - dp1[1] * dst0[1];
    fTangent[2] := dp0[2] * dst1[1] - dp1[2] * dst0[1];
    ScaleVector(fTangent, factor);

    //should this really renormalize?
    nTangent := VectorNormalize(fTangent);

    // Iterate over the vertices, adding the face normal influence to each
    for J := 0 to 2 do
    begin
      // Get the current normal from the default location (index shared with position)
      EJ := pV^[J];
      cTangent := newTangents[EJ];

      // Check to see if this normal has not yet been touched
      if VectorIsNull(cTangent) then
      begin
        // First instance of this index, just store it as is
        newTangents[EJ] := fTangent;
        newTangentIndices.Add(EJ);
      end
      else
      begin
        // Check for agreement
        NormalizeVector(cTangent);

        if VectorDotProduct(cTangent, nTangent) >= cos(3.1415926 * 0.333333) then
        begin
          // Normal agrees, so add it
          newTangents[EJ] := VectorAdd(newTangents[EJ], fTangent);
          newTangentIndices.Add(EJ);
        end
        else
        begin
          // Normals disagree, this vertex must be along a facet edge
          Agrees := False;
          if collisionMap.Find(EJ, E_) then
          begin
            // Loop through all hits on this index, until one agrees
            repeat
              cTangent := newTangents[E_];
              NormalizeVector(cTangent);
              if VectorDotProduct(cTangent, nTangent) >= cos(3.1415926 *
                0.333333) then
              begin
                Agrees := True;
                break;
              end;
            until not collisionMap.NextDublicate(E_);
          end;

          // Check for agreement with an earlier collision
          if Agrees then
          begin
            // Found agreement with an earlier collision, use that one
            newTangents[E_] := VectorAdd(newTangents[E_], fTangent);
            newTangentIndices.Add(E_);
          end
          else
          begin
            // We have a new collision, create a new normal
            newTangentIndices.Add(newTangents.Count);
            collisionMap.Add(EJ, newTangents.Count);
            newTangents.Add(fTangent);
          end;
        end; // else ( if tangent agrees)
      end; // else (if tangent is uninitialized)
    end; // for each vertex in triangle
  end; // for each face

  //now normalize all the normals
  newTangents.Normalize;

  // Place new tangent
  SplitVertices;

  FAttributes[attrTangent] := True;
  FAttributeArrays[attrTangent].Count := 3 * newTangentIndices.Count;
  FType[attrTangent] := GLSLType3F;
  if FAttributes[attrNormal] then
  begin
    FAttributes[attrBinormal] := True;
    FAttributeArrays[attrBinormal].Count := 3 * newTangentIndices.Count;
    FType[attrBinormal] := GLSLType3F;
    for I := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[I];
      BD := FElements[I];
      E_ := 3 * BD.Int.Value;
      Move(newTangents.List[E], FAttributeArrays[attrTangent].List[E_],
        SizeOf(TVector3f));
      PAffineVector(@FAttributeArrays[attrBinormal].List[E_])^ :=
        VectorCrossProduct(
        PAffineVector(@FAttributeArrays[attrNormal].List[E_])^,
        PAffineVector(@FAttributeArrays[attrTangent].List[E_])^);
    end;
  end
  else
    for I := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[I];
      BD := FElements[I];
      E_ := 3 * BD.Int.Value;
      Move(newTangents.List[E], FAttributeArrays[attrTangent].List[E_],
        SizeOf(TVector3f));
    end;

  newTangents.Destroy;
  newTangentIndices.Destroy;
  collisionMap.Destroy;
end;

procedure TMeshAtom.ComputeBoundingBox;
var
  Positions: I4ByteListToVectorList;
  I: Integer;
  min, max, p: TVector3f;

begin
  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  Positions :=
    T4ByteListToVectorList.Create(FAttributeArrays[attrPosition],
    FType[attrPosition]);

  min := Vector3fMake(1e10, 1e10, 1e10);
  max := Vector3fMake(-1e10, -1e10, -1e10);

  for I := Positions.Count - 1 downto 0 do
  begin
    p := Positions[I];
    MinVector(min, p);
    MaxVector(max, p);
  end;

  FAABB.min := min;
  FAABB.max := max;
  FAABB.revision := FRevisionNum;
end;

function TMeshAtom.RayCastIntersect(const ARayStart, ARayVector: TVector;
  out AnIntersectPoint, AnIntersectNormal: TVector): Boolean;
var
  Positions: I4ByteListToVectorList;
  Normals: I4ByteListToVectorList;

var
  Dis, minDis: Single;
  pE: ^TVector3i;
  I: Integer;
  V1, V2, V3: TVector3f;
  V4: TVector4f;
  iPoint, iNormal: TVector;
begin
  Result := False;

  if FBuildingState = mmsIgnoring then
    exit;

  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  Positions := T4ByteListToVectorList.Create(FAttributeArrays[attrPosition], FType[attrPosition]);

  if FAttributes[attrNormal] then
    Normals := T4ByteListToVectorList.Create(FAttributeArrays[attrNormal], FType[attrNormal])
  else
    Normals := nil;

  minDis := -1;

  case FPrimitive of
    mpTRIANGLES, mpTRIANGLE_STRIP, mpTRIANGLE_FAN:
      begin
        if FTrianglesElements.Revision <> FElements.Revision then
          MakeTriangleElements;
        for I := FTrianglesElements.Count div 3 - 1 downto 0 do
        begin
          pE := @FTrianglesElements.List[3 * I];
          V1 := Positions[pE^[0]];
          V2 := Positions[pE^[1]];
          V3 := Positions[pE^[2]];
          if RayCastTriangleIntersect(
            ARayStart, ARayVector,
            V1, V2, V3,
            @iPoint, @iNormal) then
          begin
            Dis := VectorDistance2(ARayStart, iPoint);
            if (Dis < minDis) or (minDis < 0) then
            begin
              minDis := Dis;
              AnIntersectPoint := iPoint;
              AnIntersectNormal := iNormal;
            end;
          end;
        end;
      end;

    mpPOINTS:
      begin
        for I := FVertexCount - 1 downto 0 do
        begin
          V4 := Positions.Get4f(I);
          iNormal := VectorSubtract(V4, ARayStart);
          NormalizeVector(iNormal);
          if VectorEquals(iNormal, ARayVector) then
          begin
            Dis := VectorDistance2(ARayStart, V4);
            if (Dis < minDis) or (minDis < 0) then
            begin
              minDis := Dis;
              AnIntersectPoint := V4;
              if Assigned(Normals) then
                AnIntersectNormal := Normals.Get4f(I)
              else
                AnIntersectNormal := ARayVector;
            end;
          end;
        end;
      end;
  end;

  Result := (minDis >= 0);
end;

procedure TMeshAtom.Transform(const AMatrix: TMatrix);

  function IsMatrixIdentity: Boolean;
  var R, C: Integer;
  begin
    Result := True;
    for R := 0 to 3 do
      for C := 0 to 3 do
        if R = C then
        begin
          if AMatrix[R,C] <> 1.0 then
          begin
            Result := False;
            exit;
          end;
        end
        else
          if AMatrix[R,C] <> 0.0 then
          begin
            Result := False;
            exit;
          end;
  end;

  var
    Positions: I4ByteListToVectorList;
    Normals: I4ByteListToVectorList;
    Tangents: I4ByteListToVectorList;
    Binormals: I4ByteListToVectorList;
    NMatrix: TMatrix;
    I: Integer;
    p: TVector3f;

begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  Positions := T4ByteListToVectorList.Create(FAttributeArrays[attrPosition], FType[attrPosition]);
  Normals := nil;
  Tangents := nil;
  Binormals := nil;
  if not IsMatrixIdentity then
  begin
    NMatrix := AMatrix;
    NormalizeMatrix(NMatrix);
    if FAttributes[attrNormal] then
      Normals := T4ByteListToVectorList.Create(FAttributeArrays[attrNormal], FType[attrNormal]);
    if FAttributes[attrTangent] then
      Tangents := T4ByteListToVectorList.Create(FAttributeArrays[attrTangent], FType[attrTangent]);
    if FAttributes[attrBinormal] then
      Binormals := T4ByteListToVectorList.Create(FAttributeArrays[attrBinormal], FType[attrBinormal]);
  end;

  for I := Positions.Count - 1 downto 0 do
  begin
    p := Positions[I];
    p := VectorTransform(p, AMatrix);
    Positions[I] := p;

    if Assigned(Normals) then
    begin
      p := Normals[I];
      p := VectorTransform(p, NMatrix);
      Normals[I] := p;
    end;

    if Assigned(Tangents) then
    begin
      p := Tangents[I];
      p := VectorTransform(p, NMatrix);
      Tangents[I] := p;
    end;

    if Assigned(Binormals) then
    begin
      p := Binormals[I];
      p := VectorTransform(p, NMatrix);
      Binormals[I] := p;
    end;
  end;
end;

procedure TMeshAtom.Rescale(ARadius: Single);
var
  Positions: I4ByteListToVectorList;
  I: Integer;
  min, max, p, r, center: TVector3f;
  oldRadius, scale: Single;

begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsAssembling then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  ComputeBoundingBox;
  if not FAttributes[attrPosition] then
    exit; // Nothing todo

  Positions := T4ByteListToVectorList.Create(FAttributeArrays[attrPosition], FType[attrPosition]);

  min := FAABB.min;
  max := FAABB.max;
  r := VectorSubtract(max, min);
  ScaleVector(r, 0.5);
  center := VectorAdd(min, r);
  oldRadius := MaxFloat(r[0], MaxFloat(r[1], r[2]));
  scale := ARadius / oldRadius;
  min := Vector3fMake(1e10, 1e10, 1e10);
  max := Vector3fMake(-1e10, -1e10, -1e10);

  for I := Positions.Count - 1 downto 0 do
  begin
    p := Positions[I];
    p := VectorSubtract(p, center);
    ScaleVector(p, scale);
    Positions[I] := p;
    MinVector(min, p);
    MaxVector(max, p);
  end;
  FAABB.min := min;
  FAABB.max := max;
  FAABB.revision := FRevisionNum;
end;

procedure TMeshAtom.RestartStrip;
var
  I: T4ByteData;
begin
  if FBuildingState = mmsIgnoring then
    exit;
  if FBuildingState <> mmsPrimitives then
  begin
    GLSLogger.LogWarningFmt(glsMeshWrongCall, [TagName]);
    FBuildingState := mmsIgnoring;
    exit;
  end;

  if not (FPrimitive in [mpTRIANGLE_STRIP, mpTRIANGLE_FAN, mpLINE_STRIP]) then
  begin
    GLSLogger.LogWarningFmt(glsMeshNoNeedRestart, [ClassName]);
    exit;
  end;

  FRestartVertex.Add(FVertexCount);
  if vUsePrimitiveRestart then
  begin
    FElements.Add(FRestartIndex);
  end
  else
  begin
    I := FElements[FElements.Count - 1];
    FElements.Add(I);
    I.UInt.Value := I.UInt.Value + 1;
    FElements.Add(I);
  end;
  FRemoveLastElement := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TMeshAtom'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'T4ByteListToVectorList'}{$ENDIF}

function T4ByteListToVectorList.Count: Integer;
begin
  Result := FCount;
end;

constructor T4ByteListToVectorList.Create(AList: T4ByteList; AType:
  TGLSLDataType);
begin
  FSource := AList.List;
  FType := AType;
  FSize := GLSLTypeComponentCount(AType);
  FCount := AList.Count div FSize;
end;

function T4ByteListToVectorList.Get(AIndex: Integer): TVector3f;
var
  AsVec2f: TVector2f absolute Result;
begin
  case FType of
    GLSLType1F:
      begin
        Result[0] := PSingle(@FSource[AIndex])^;
        Result[1] := 0.0;
        Result[2] := 0.0;
      end;
    GLSLType2F:
      begin
        AsVec2f := PVector2f(@FSource[FSize * AIndex])^;
        Result[2] := 0.0;
      end;
    GLSLType3F, GLSLType4F:
      begin
        Result := PVector3f(@FSource[FSize * AIndex])^;
      end;
    GLSLType1I: ;
    GLSLType2I: ;
    GLSLType3I: ;
    GLSLType4I: ;
    GLSLType1UI: ;
    GLSLType2UI: ;
    GLSLType3UI: ;
    GLSLType4UI: ;
    GLSLTypeMat2F: ;
    GLSLTypeMat3F: ;
    GLSLTypeMat4F: ;
  end;
end;

function T4ByteListToVectorList.Get4f(AIndex: Integer): TVector4f;
var
  AsVec2f: TVector2f absolute Result;
  AsVec3f: TVector3f absolute Result;
begin
  case FType of
    GLSLType1F:
      begin
        Result[0] := PSingle(@FSource[AIndex])^;
        Result[1] := 0.0;
        Result[2] := 0.0;
        Result[3] := 0.0;
      end;
    GLSLType2F:
      begin
        AsVec2f := PVector2f(@FSource[FSize * AIndex])^;
        Result[2] := 0.0;
        Result[3] := 0.0;
      end;
    GLSLType3F:
      begin
        AsVec3f := PVector3f(@FSource[FSize * AIndex])^;
        Result[3] := 0.0;
      end;
    GLSLType4F:
      begin
        Result := PVector4f(@FSource[FSize * AIndex])^;
      end;
    GLSLType1I: ;
    GLSLType2I: ;
    GLSLType3I: ;
    GLSLType4I: ;
    GLSLType1UI: ;
    GLSLType2UI: ;
    GLSLType3UI: ;
    GLSLType4UI: ;
    GLSLTypeMat2F: ;
    GLSLTypeMat3F: ;
    GLSLTypeMat4F: ;
  end;
end;

procedure T4ByteListToVectorList.Put(AIndex: Integer; const Value:
  TVector3f);
var
  AsVec2f: TVector2f absolute Value;
begin
  case FType of
    GLSLType1F:
      begin
        PSingle(@FSource[AIndex])^ := Value[0];
      end;
    GLSLType2F:
      begin
        PVector2f(@FSource[FSize * AIndex])^ := AsVec2f
      end;
    GLSLType3F, GLSLType4F:
      begin
        PVector3f(@FSource[FSize * AIndex])^ := Value;
      end;
    GLSLType1I: ;
    GLSLType2I: ;
    GLSLType3I: ;
    GLSLType4I: ;
    GLSLType1UI: ;
    GLSLType2UI: ;
    GLSLType3UI: ;
    GLSLType4UI: ;
    GLSLTypeMat2F: ;
    GLSLTypeMat3F: ;
    GLSLTypeMat4F: ;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

finalization

  ReleaseMemory;

end.

