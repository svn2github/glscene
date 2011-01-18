//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLMeshManager<p>

   <b>History : </b><font size=-1><ul>
    <li>15/10/10 - Yar - Creation
 </ul></font>
}

// TODO: MeshBuilder.MakeAdjacency

unit GL3xMesh;

interface

{$I GLScene.inc}

uses
{$IFDEF FPC}
  LCLVersion,
  LCLType,
  LResources,
{$ENDIF}
  Classes,
  SysUtils,
  SyncObjs,
  BaseClasses,
  PersistentClasses,
  GLCrossPlatform,
  ApplicationFileIO,
  GLContext,
  GLState,
  GLShaderManager,
  VectorGeometry,
  VectorTypes,
  VectorLists,
  GeometryBB,
  OpenGLTokens,
  GLSGenerics,
  GLSRedBlackTree,
  GL3xMaterialTokens;

type

  TGLMeshPrimitive = (
    mpNOPRIMITIVE,
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES
    );

  TGLMeshPrimitives = set of TGLMeshPrimitive;

const
  cAllMeshPrimitive = [
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES];

{$IFDEF FPC}
{$IF (LCL_RELEASE < 31)}
{$DEFINE GLS_GENERIC_PREFIX}
{$IFEND}
{$ENDIF}

type

  TGLMeshAttributeRequest = (arqNormal, arqTexCoord, arqTangent);
  TGLMeshAttributeRequests = set of TGLMeshAttributeRequest;

  TGL3xMeshName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
  end;

  PFaceGroupDesc = ^TFaceGroupDesc;
  TFaceGroupDesc = record
    Name: string;
    PrimitiveType: TGLMeshPrimitive;
    VertexOffset: TGLint;
    VertexCount: TGLuint;
    ElementOffset, ElementCount: TGLuint;
    TriOnlyElementOffset, TriOnlyElementCount: TGLuint;
    Material: IGLName;
  end;

  TGLAbstractMesh = class(TDataFile)
  protected
    { Protected Declarations }
    FName: TGL3xMeshName;
    FRevisionNum: Integer;
    FRequest: TGLMeshAttributeRequests;

    FBlank: Boolean;
    FAttributes: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLAttribute;
    FDataFormat: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLDataType;
    FAttributeArrays: array[0..GLS_VERTEX_ATTR_NUM - 1] of T4ByteList;
    FAttributeDivisor: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLuint;
    FLODFaceGroupMap: array[Byte] of array of TFaceGroupDesc;

    FHasIndices: Boolean;
    FElementBuffer: T4ByteList;
    FElementBufferWithAdjacency: T4ByteList;
    FElementBufferTrianglesOnly: T4ByteList;
    FTriOnlyBufferNeedUpdate: Boolean;
    FRestartIndex: TGLuint;

    FAABB: TAABB;

    FTaskFinishEvent: TFinishTaskEvent;
    function NewEvent: TFinishTaskEvent;

    function GetAttributeCount: Integer;
    function GetBuilder: TObject; virtual; abstract;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure WaitParallelTask;

    procedure ImportFromFile(const fileName: string); virtual; abstract;
    procedure ExportToFile(const fileName: string); virtual; abstract;

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    class function Capabilities: TDataFileCapabilities; override;

    property Name: TGL3xMeshName read FName;
    property AttributeCount: Integer read GetAttributeCount;
  end;

  TVertexHashMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GRedBlackTree < Double, Integer > ;

  { Note:
      Element buffer allways present, filled during mesh assembling.

  }

  TGLAbstractMeshBuilder = class(TObject)
  protected
    { Protected Declarations }
    FState: (mmsDefault, mmsAssembling, mmsPrimitives);
    FMesh: TGLAbstractMesh;
    FCurrentPrimitive: TGLMeshPrimitive;
    FCurrentAttribValue: array[0..GLS_VERTEX_ATTR_NUM - 1, 0..15] of T4ByteData;
    FCurrentLOD: Byte;
    FCurrentBatch: Integer;
    FVertexPositionSlot: Integer;
    FRemoveLastElement: Boolean;
    function GetAttributeIndex(Attrib: TGLSLAttribute; AType: TGLSLDataType): GLint;
    function GetBlank: Boolean;
    procedure SetBlank(AValue: Boolean);
    function FindSlot(const Attr: TGLSLAttribute): Integer; inline;
    function FindOrOccupySlot(const Attr: TGLSLAttribute): Integer; inline;
    procedure MakeTriangleOnlyBuffer;
    procedure ComputeBoundingBox;
    function GetAABB: TAABB;
    function GetFaceGroupName: string;
    procedure SetFaceGroupName(const AValue: string);
    function GetFaceGroupMaterialName: IGLName;
    procedure SetFaceGroupMaterialName(const AValue: IGLName);
  public
    { Public Declarations }
    constructor Create(AOwner: TGLAbstractMesh); virtual;
    {: Begins storing a piece of geometry }
    procedure BeginMeshAssembly; virtual;
    {: Begins gathering information about the given type of primitives. }
    procedure BeginBatch(APrimitiveType: TGLMeshPrimitive; ALOD: Byte = 0);
    {: Declare attibute and it type for use in mesh. }
    procedure DeclareAttribute(Attrib: TGLSLAttribute; AType: TGLSLDataType);
    {: Specifies a new value for the attribute with the given name. }
    procedure Attribute1f(Attrib: TGLSLAttribute; a1: GLfloat);
    procedure Attribute2f(Attrib: TGLSLAttribute; a1, a2: GLfloat); overload;
    procedure Attribute2f(Attrib: TGLSLAttribute; const a: TVector2f); overload;
    procedure Attribute3f(Attrib: TGLSLAttribute; a1, a2, a3: GLfloat); overload;
    procedure Attribute3f(Attrib: TGLSLAttribute; const a: TVector3f); overload;
    procedure Attribute4f(Attrib: TGLSLAttribute; a1, a2, a3, a4: GLfloat); overload;
    procedure Attribute4f(Attrib: TGLSLAttribute; const a: TVector4f); overload;
    procedure Attribute1i(Attrib: TGLSLAttribute; a1: GLint);
    procedure Attribute2i(Attrib: TGLSLAttribute; a1, a2: GLint); overload;
    procedure Attribute2i(Attrib: TGLSLAttribute; const a: TVector2i); overload;
    procedure Attribute3i(Attrib: TGLSLAttribute; a1, a2, a3: GLint); overload;
    procedure Attribute3i(Attrib: TGLSLAttribute; const a: TVector3i); overload;
    procedure Attribute4i(Attrib: TGLSLAttribute; a1, a2, a3, a4: GLint); overload;
    procedure Attribute4i(Attrib: TGLSLAttribute; const a: TVector4i); overload;
    procedure Attribute1ui(Attrib: TGLSLAttribute; a1: GLuint);
    procedure Attribute2ui(Attrib: TGLSLAttribute; a1, a2: GLuint); overload;
    procedure Attribute2ui(Attrib: TGLSLAttribute; const a: TVector2ui); overload;
    procedure Attribute3ui(Attrib: TGLSLAttribute; a1, a2, a3: GLuint); overload;
    procedure Attribute3ui(Attrib: TGLSLAttribute; const a: TVector3ui); overload;
    procedure Attribute4ui(Attrib: TGLSLAttribute; a1, a2, a3, a4: GLuint); overload;
    procedure Attribute4ui(Attrib: TGLSLAttribute; const a: TVector4ui); overload;
    procedure AttributeList(Attrib: TGLSLAttribute; AList: T4ByteList);
    {: Specifies a new vertex of a primitive. }
    procedure EmitVertex;
    {: Reserve space for vertices. Thay value is undefined.
       Main purpose is to allocate space for feedback. }
    procedure EmitVertices(ANumber: LongWord);
    {: Restart strip by hardware feature or degenerate primitive }
    procedure RestartStrip;
    {: Ends gathering information about the primitives in batch. }
    procedure EndBatch;
    {: Clear mesh content. }
    procedure Clear;
    {: Weld equivalent vertices, rebuild element buffer. }
    procedure WeldVertices;
    {: Slit equivalent vertices, every element becomes unique. }
    procedure SplitVertices;
    {: Cast strip and fans of triangles to simple triangles. }
    procedure Triangulate;
    {: Make additional element buffer with triangle adjacency. }
    procedure MakeAdjacency;
    {: Compute triangle's normals. }
    procedure ComputeNormals(ASmooth: Boolean = True);
    {: Compute triangle's texture coordinate of 0 channel. }
    procedure ComputeTexCoords;
    {: Compute triangle's normals. }
    procedure ComputeTangents;
    {: Rescales and alignes mesh based on bounding box. }
    procedure Rescale(ARadius: Single = 1.0);
    {: Ends assembling of mesh. }
    procedure EndMeshAssembly; virtual;
    {: Specify an empty mesh for which video memory only allocated, but not uploaded. }
    property Blank: Boolean read GetBlank write SetBlank;
    {: This property returns the points defining the axis-
       aligned bounding box containing the model. }
    property AABB: TAABB read GetAABB;
    {: Access to primitive group name aka batch. }
    property FaceGroupName: string read GetFaceGroupName write SetFaceGroupName;
    property FaceGroupMaterialName: IGLName read GetFaceGroupMaterialName write SetFaceGroupMaterialName;
  end;

  TGLAbstractMeshClass = class of TGLAbstractMesh;
  TGLAbstractMeshBuilderClass = class of TGLAbstractMeshBuilder;

  MeshManager = class(TGLSAbstractManager)
  protected
    { Protected Declarations }
    class procedure Initialize; override;
    class procedure Finalize; override;
    class procedure LoadResources;
    class procedure SaveResources;
    class procedure ClearResources;
    class procedure PushMesh(AMesh: TGLAbstractMesh);
    class function GetMesh(const AName: IGLName): TGLAbstractMesh;
    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
    class procedure NotifyContextCreated; override;
    class procedure NotifyBeforeCompile; override;
    class function Priority: Byte; override;
  public
    { Public Declarations }
    class function FillResourceList(AList: TStringList): Boolean; override;
    class procedure MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass); override;

    class procedure FillMeshNameList(var AList: TStringList);

    class function CreateMesh(AName: string; AClass: TGLAbstractMeshClass;
      const AFileName, AImportFile: string): IGLName;
    class function GetMeshName(const AName: string): IGLName;
    class function GetMeshBuilder(const AName: IGLName): TGLAbstractMeshBuilder;
  end;

  TGLAbstractMeshFileIO = class(TPersistent)
  public
    { Public Declarations }

    {: Describes what the TGLAbstractMeshFileIO is capable of.<p>
       Default value is [dfcRead]. }
    class function Capabilities: TDataFileCapabilities; virtual;
    class procedure LoadFromStream(AStream: TStream; ABuilder: TGLAbstractMeshBuilder); virtual;
    class procedure SaveToStream(AStream: TStream; AMesh: TGLAbstractMesh); virtual;
  end;

  TGLAbstractMeshFileIOClass = class of TGLAbstractMeshFileIO;

  // TVectorFileFormat
  //
  TVectorFileFormat = class
  public
    VectorFileClass: TGLAbstractMeshFileIOClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // TVectorFileFormatsList
  //
  {: Stores registered vector file formats. }
  TVectorFileFormatsList = class(TPersistentObjectList)
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: Integer; AClass:
      TGLAbstractMeshFileIOClass);
    function FindExt(ext: string): TGLAbstractMeshFileIOClass;
    function FindFromFileName(const fileName: string): TGLAbstractMeshFileIOClass;
    procedure Remove(AClass: TGLAbstractMeshFileIOClass);
    procedure BuildFilterStrings(VectorFileClass: TGLAbstractMeshFileIOClass;
      out descriptions, filters: string;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(AIndex: Integer;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidVectorFile = class(Exception);

  //: Read access to the list of registered vector file formats
function GetVectorFileFormats: TVectorFileFormatsList;
//: A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter: string;
//: A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter: string;
{: Returns an extension by its index in the vector files dialogs filter.<p>
   Use VectorFileFormatsFilter to obtain the filter. }
function VectorFileFormatExtensionByIndex(AIndex: Integer): string;

procedure RegisterVectorFileFormat(const AExtension, ADescription: string;
  AClass: TGLAbstractMeshFileIOClass);
procedure UnregisterVectorFileClass(AClass: TGLAbstractMeshFileIOClass);

implementation

uses
  GLSLog,
  GLStrings,
  VectorGeometryEXT;

const
  cRestartIndex: TGLuint = $FFFFFFFF;

type
  TIntIntRBT = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GRedBlackTree < Integer, Integer > ;

resourcestring
  glsMeshBuilderWrongCall = 'MeshBuilder: wrong method call - ignored';
  glsMeshBuilderNoPrimitive = 'MeshBuilder: no primitive for batch - ignored';
  glsMeshBuilderNoAttrDeclar = 'MeshBuilder: no declaration of attributes';
  glsMeshBuilderExcesAttrib = '%s: Excessive attribute declaration.';
  glsMeshBuilderInvalidArraySize = 'Invalid array size of attribute "%s" of ' +
    'mesh "%s"';
  glsMeshBuilderHashingError = '%s: hashing error during vertex welding in m' +
    'esh %s';
  glsMeshBuilderNoNeedRestart = '%s: This primitive type does not need to re' +
    'start.';

var
  Meshes: array of TGLAbstractMesh;
  MeshBuilder: TGLAbstractMeshBuilder;
  UsePrimitiveRestart: Boolean = True;
  VectorFileFormats: TVectorFileFormatsList;

{$REGION 'Helper functions'}

function CompareVertexKey(const Item1, Item2: Double): Integer;
begin
  if Item1 < Item2 then
    exit(-1)
  else if Item1 = Item2 then
    exit(0)
  else
    exit(1);
end;

function CompareVertex(const Item1, Item2: Integer): Boolean;
var
  a, size: Integer;
  p1, p2: Pointer;
  BD: T4ByteData;
  Idx1, Idx2: Integer;
begin
  if Item1 <> Item2 then
  begin
    with MeshBuilder.FMesh do
    begin
      BD := FElementBuffer[Item1];
      Idx1 := Integer(BD.UInt.Value);
      BD := FElementBuffer[Item2];
      Idx2 := Integer(BD.UInt.Value);
      for a := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        if Assigned(FAttributes[a]) then
        begin
          size := GLSLTypeComponentCount(FDataFormat[a]);
          p1 := @FAttributeArrays[a].List[Idx1 * size];
          p2 := @FAttributeArrays[a].List[Idx2 * size];
          if not CompareMem(p1, p2, size * SizeOf(T4ByteData)) then
            exit(False);
        end;
    end;
  end;
  Result := True;
end;

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 < Item2 then
    exit(-1)
  else if Item1 = Item2 then
    exit(0)
  else
    exit(1);
end;

function CompareInteger_(const Item1, Item2: Integer): Boolean;
begin
  Result := Item1 = Item2;
end;

{$ENDREGION}

{$REGION 'Vector file registry'}

// GetVectorFileFormats
//

function GetVectorFileFormats: TVectorFileFormatsList;
begin
  if not Assigned(VectorFileFormats) then
    VectorFileFormats := TVectorFileFormatsList.Create;
  Result := VectorFileFormats;
end;

// VectorFileFormatsFilter
//

function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLAbstractMeshFileIO, Result, f);
end;

// VectorFileFormatsSaveFilter
//

function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLAbstractMeshFileIO, Result, f, False, True);
end;

// RegisterVectorFileFormat
//

procedure RegisterVectorFileFormat(const AExtension, ADescription: string;
  AClass: TGLAbstractMeshFileIOClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterVectorFileClass
//

procedure UnregisterVectorFileClass(AClass: TGLAbstractMeshFileIOClass);
begin
  if Assigned(VectorFileFormats) then
    VectorFileFormats.Remove(AClass);
end;

// VectorFileFormatExtensionByIndex
//

function VectorFileFormatExtensionByIndex(AIndex: Integer): string;
begin
  Result := GetVectorFileFormats.FindExtByIndex(AIndex);
end;

// TVectorFileFormatsList.Destroy
//

destructor TVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

// Add
//

procedure TVectorFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TGLAbstractMeshFileIOClass);
var
  newRec: TVectorFileFormat;
begin
  newRec := TVectorFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    VectorFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

// FindExt
//

function TVectorFileFormatsList.FindExt(ext: string): TGLAbstractMeshFileIOClass;
var
  i: Integer;
begin
  ext := AnsiLowerCase(ext);
  for i := Count - 1 downto 0 do
    with TVectorFileFormat(Items[I]) do
    begin
      if Extension = ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

// FindFromFileName
//

function TVectorFileFormatsList.FindFromFileName(const fileName: string):
  TGLAbstractMeshFileIOClass;
var
  ext: string;
begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(glsUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

// Remove
//

procedure TVectorFileFormatsList.Remove(AClass: TGLAbstractMeshFileIOClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

// BuildFilterStrings
//

procedure TVectorFileFormatsList.BuildFilterStrings(
  VectorFileClass: TGLAbstractMeshFileIOClass;
  out descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(VectorFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and (dfcRead in
      p.VectorFileClass.Capabilities))
      or (formatsThatCanBeSaved and (dfcWrite in
      p.VectorFileClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s',
          [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s',
      [glsAllFilter, filters, descriptions]);
end;

// FindExtByIndex
//

function TVectorFileFormatsList.FindExtByIndex(AIndex: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TVectorFileFormat;
begin
  Result := '';
  if AIndex > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities))
        or (formatsThatCanBeSaved and (dfcWrite in
        p.VectorFileClass.Capabilities)) then
      begin
        if AIndex = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(AIndex);
      end;
    end;
  end;
end;

class function TGLAbstractMeshFileIO.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

class procedure TGLAbstractMeshFileIO.LoadFromStream(AStream: TStream; ABuilder: TGLAbstractMeshBuilder);
begin
end;

class procedure TGLAbstractMeshFileIO.SaveToStream(AStream: TStream; AMesh: TGLAbstractMesh);
begin
end;

{$ENDREGION}

{$REGION 'TGLAbstractMesh'}
// ------------------
// ------------------ TGLAbstractMeshBuilder ------------------
// ------------------

function TGL3xMeshName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGL3xMeshName.GetManager: TGLSAbstractManagerClass;
begin
  Result := MeshManager;
end;

constructor TGLAbstractMesh.Create(AOwner: TPersistent);
var
  A: Integer;
begin
  inherited Create(AOwner);
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    FAttributeArrays[A] := T4ByteList.Create;
  FBlank := False;
  FHasIndices := False;
  FRestartIndex := cRestartIndex;
  FElementBuffer := T4ByteList.Create;
  FElementBufferWithAdjacency := T4ByteList.Create;
  FElementBufferTrianglesOnly := T4ByteList.Create;
  FTriOnlyBufferNeedUpdate := True;
  FName := TGL3xMeshName.Create;
  FName._AddRef;
  FRevisionNum := 0;
end;

destructor TGLAbstractMesh.Destroy;
var
  A: Integer;
begin
  WaitParallelTask;
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    FAttributeArrays[A].Destroy;
  FElementBuffer.Destroy;
  FElementBufferWithAdjacency.Destroy;
  FElementBufferTrianglesOnly.Destroy;
  FName.SetIndex(-1);
  FName._Release;
  inherited;
end;

function TGLAbstractMesh.NewEvent: TFinishTaskEvent;
begin
  if IsMainThread then
    FTaskFinishEvent := TFinishTaskEvent.Create;
  Result := FTaskFinishEvent;
end;

procedure TGLAbstractMesh.WaitParallelTask;
begin
  if Assigned(FTaskFinishEvent) and IsMainThread then
  begin
    CheckSynchronize;

    case FTaskFinishEvent.WaitFor(60000) of
      wrTimeout:
        begin
          // Recursively continue waiting
          GLSLogger.LogDebug('Sevice thread timeout');
          WaitParallelTask;
        end;

      wrAbandoned:

        begin
          GLSLogger.LogError('Sevice thread abandoned');
        end;

      wrError:

        begin
          GLSLogger.LogError('Sevice thread error');
          FTaskFinishEvent.Free;
          FTaskFinishEvent := nil;
          exit;
        end;

    end;
    FTaskFinishEvent.Free;
    FTaskFinishEvent := nil;
  end;
end;

function TGLAbstractMesh.GetAttributeCount: Integer;
var
  A: Integer;
begin
  Result := 0;
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if Assigned(FAttributes[A]) then
      Inc(Result);
end;

// LoadFromFile
//

procedure TGLAbstractMesh.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := fileName;
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end
  else
  begin
    GLSLogger.LogErrorFmt('File %s not found', [fileName]);
    ImportFromFile('');
  end;
end;

// SaveToFile
//

procedure TGLAbstractMesh.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  ResourceName := fileName;
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

class function TGLAbstractMesh.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

{$ENDREGION 'TGLAbstractMesh'}

{$REGION 'TGLAbstractMeshBuilder'}
// ------------------
// ------------------ TGLAbstractMeshBuilder ------------------
// ------------------

constructor TGLAbstractMeshBuilder.Create(AOwner: TGLAbstractMesh);
begin
  FMesh := AOwner;
end;

procedure TGLAbstractMeshBuilder.BeginMeshAssembly;
begin
  FMesh.WaitParallelTask;
  if FState <> mmsDefault then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  // Default mesh can be rebuilded
  if FMesh.FName.IndexInManagerArray = 0 then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  FState := mmsAssembling;
  FVertexPositionSlot := -1;
end;

procedure TGLAbstractMeshBuilder.Clear;
var
  A: Integer;
  L: Byte;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  with FMesh do
  begin
    for L := 0 to 255 do
      FLODFaceGroupMap[L] := nil;
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    begin
      FAttributes[A] := nil;
      FDataFormat[A] := GLSLTypeVoid;
      FAttributeArrays[A].Clear;
      FAttributeDivisor[A] := 0;
    end;
    FHasIndices := False;
    FElementBuffer.Clear;
    FElementBufferWithAdjacency.Clear;
    FTriOnlyBufferNeedUpdate := True;
  end;
end;

procedure TGLAbstractMeshBuilder.BeginBatch(
  APrimitiveType: TGLMeshPrimitive; ALOD: Byte);
begin
  if APrimitiveType = mpNOPRIMITIVE then
  begin
    GLSLogger.LogWarning(glsMeshBuilderNoPrimitive);
    exit;
  end;
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  if FMesh.AttributeCount = 0 then
  begin
    GLSLogger.LogWarning(glsMeshBuilderNoAttrDeclar);
    exit;
  end;

  FState := mmsPrimitives;
  FCurrentPrimitive := APrimitiveType;
  FCurrentLOD := ALOD;
  FCurrentBatch := Length(FMesh.FLODFaceGroupMap[ALOD]);
  SetLength(FMesh.FLODFaceGroupMap[ALOD], FCurrentBatch + 1);
  with FMesh.FLODFaceGroupMap[ALOD][FCurrentBatch] do
  begin
    Name := Format('FaceGroup%d', [FCurrentBatch]);
    if FCurrentBatch > 0 then
      VertexOffset := TGLint(FMesh.FLODFaceGroupMap[ALOD][FCurrentBatch - 1].VertexCount)
    else
      VertexOffset := 0;
    VertexCount := 0;
    ElementOffset := FMesh.FElementBuffer.Count;
    PrimitiveType := FCurrentPrimitive;
    Material := nil;
  end;
end;

procedure TGLAbstractMeshBuilder.EndBatch;
var
  Valid: Boolean;
  A, C, E: Integer;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  FState := mmsAssembling;

  with FMesh do
  begin
    E := 0;
    for A := 0 to High(FLODFaceGroupMap[FCurrentLOD]) do
      Inc(E, FLODFaceGroupMap[FCurrentLOD][A].VertexCount);

    // Check attribute arrays equability
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(FAttributes[A]) then
      begin
        C := FAttributeArrays[A].Count;
        if C = 0 then
        begin
          FAttributes[A] := nil;
          continue;
        end;
        C := C div GLSLTypeComponentCount(FDataFormat[A]);
        if E <> C then
        begin
          GLSLogger.LogErrorFmt(glsMeshBuilderInvalidArraySize,
            [FAttributes[A].Name, Name.GetValue]);
          Abort;
        end;
      end;

    // Remove empty batch
    if E = 0 then
    begin
      FLODFaceGroupMap[FCurrentLOD] := nil;
      exit;
    end;

    // Remove excesive restart index
    if FRemoveLastElement then
    begin
      if not UsePrimitiveRestart then
        FElementBuffer.Pop;
      FElementBuffer.Pop;
    end;

    FHasIndices := False;
    FTriOnlyBufferNeedUpdate := True;
    Valid := False;

    case FCurrentPrimitive of
      mpTRIANGLES: Valid := (E mod 3 = 0) and (E > 2);
      mpTRIANGLE_STRIP, mpTRIANGLE_FAN: Valid := E > 2;
      mpPOINTS: Valid := True;
      mpLINES: Valid := (E mod 2 = 0) and (E > 1);
      mpLINE_STRIP, mpLINE_LOOP: Valid := E > 2;
      mpLINES_ADJACENCY: Valid := (E mod 4 = 0) and (E > 3);
      mpLINE_STRIP_ADJACENCY: Valid := E > 4;
      mpTRIANGLES_ADJACENCY: Valid := (E mod 6 = 0) and (E > 5);
      mpTRIANGLE_STRIP_ADJACENCY: Valid := E > 4;
      mpPATCHES: Valid := True;
    end;

    if not Valid then
    begin
      GLSLogger.LogError(glsInvalidNumberOfVertex);
      FLODFaceGroupMap[FCurrentLOD] := nil;
      exit;
    end;

    with FLODFaceGroupMap[FCurrentLOD][FCurrentBatch] do
      ElementCount := Cardinal(FElementBuffer.Count) - ElementOffset;
  end;
end;

procedure TGLAbstractMeshBuilder.EndMeshAssembly;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  ComputeBoundingBox;
  FState := mmsDefault;
  Inc(FMesh.FRevisionNum);
end;

procedure TGLAbstractMeshBuilder.DeclareAttribute(Attrib: TGLSLAttribute; AType: TGLSLDataType);
var
  I: Integer;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  if AType <> GLSLTypeVoid then
  begin
    for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if FMesh.FAttributes[I] = Attrib then
      begin
        GLSLogger.LogErrorFmt(glsMeshBuilderExcesAttrib, [ClassName]);
        exit;
      end;

    for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if not Assigned(FMesh.FAttributes[I]) then
      begin
        // Record new attribute in uses set
        FMesh.FAttributes[I] := Attrib;
        FMesh.FDataFormat[I] := AType;
        if Attrib = attrPosition then
          FVertexPositionSlot := I;
        exit;
      end;
    GLSLogger.LogError(glsOutOfMaxAttrib);
    Abort;
  end
end;

function TGLAbstractMeshBuilder.GetAttributeIndex(
  Attrib: TGLSLAttribute; AType: TGLSLDataType): GLint;
var
  I: Integer;
begin
  Result := -1;
  if FState <> mmsPrimitives then
    exit;

  for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if FMesh.FAttributes[I] = Attrib then
    begin
      // Check attribute type
      if (AType <> GLSLTypeVoid) and (FMesh.FDataFormat[I] <> AType) then
        GLSLogger.LogError(glsWrongAttrType);
      exit(I);
    end;
  if Attrib.TagObject <> Self then
  begin
    Attrib.TagObject := Self;
    GLSLogger.LogErrorFmt(glsUnknownAttrib, [Attrib.Name, FMesh.Name.GetValue]);
  end;
end;

function TGLAbstractMeshBuilder.FindSlot(const Attr: TGLSLAttribute): Integer;
begin
  for Result := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if FMesh.FAttributes[Result] = Attr then
      exit;
  Result := -1;
end;

function TGLAbstractMeshBuilder.FindOrOccupySlot(const Attr: TGLSLAttribute): Integer;
begin
  for Result := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if FMesh.FAttributes[Result] = Attr then
      exit;
  for Result := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if FMesh.FAttributes[Result] = nil then
      exit;
  Result := -1;
end;

function TGLAbstractMeshBuilder.GetBlank: Boolean;
begin
  Result := FMesh.FBlank;
end;

procedure TGLAbstractMeshBuilder.SetBlank(AValue: Boolean);
begin
  Assert(FState = mmsAssembling);
  FMesh.FBlank := AValue;
end;

procedure TGLAbstractMeshBuilder.Attribute1f(Attrib: TGLSLAttribute; a1:
  GLfloat);
var
  loc: Integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1);
    end
    else
      FCurrentAttribValue[loc, 0].Float.Value := a1;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2f(Attrib: TGLSLAttribute; a1, a2:
  GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a1;
      FCurrentAttribValue[loc, 1].Float.Value := a2;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2f(Attrib: TGLSLAttribute;
  const a: TVector2f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a[0];
      FCurrentAttribValue[loc, 1].Float.Value := a[1];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3f(Attrib: TGLSLAttribute;
  a1, a2, a3: GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a1;
      FCurrentAttribValue[loc, 1].Float.Value := a2;
      FCurrentAttribValue[loc, 2].Float.Value := a3;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3f(Attrib: TGLSLAttribute;
  const a: TVector3f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a[0];
      FCurrentAttribValue[loc, 1].Float.Value := a[1];
      FCurrentAttribValue[loc, 2].Float.Value := a[2];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4f(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a1;
      FCurrentAttribValue[loc, 1].Float.Value := a2;
      FCurrentAttribValue[loc, 2].Float.Value := a3;
      FCurrentAttribValue[loc, 3].Float.Value := a4;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4f(Attrib: TGLSLAttribute;
  const a: TVector4f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4F);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Float.Value := a[0];
      FCurrentAttribValue[loc, 1].Float.Value := a[1];
      FCurrentAttribValue[loc, 2].Float.Value := a[2];
      FCurrentAttribValue[loc, 3].Float.Value := a[3];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute1i(Attrib: TGLSLAttribute;
  a1: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a1;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2i(Attrib: TGLSLAttribute; a1, a2:
  GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a1;
      FCurrentAttribValue[loc, 1].Int.Value := a2;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2i(Attrib: TGLSLAttribute;
  const a: TVector2i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a[0];
      FCurrentAttribValue[loc, 1].Int.Value := a[1];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3i(Attrib: TGLSLAttribute;
  a1, a2, a3: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a1;
      FCurrentAttribValue[loc, 1].Int.Value := a2;
      FCurrentAttribValue[loc, 2].Int.Value := a3;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3i(Attrib: TGLSLAttribute;
  const a: TVector3i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a[0];
      FCurrentAttribValue[loc, 1].Int.Value := a[1];
      FCurrentAttribValue[loc, 2].Int.Value := a[2];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4i(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a1;
      FCurrentAttribValue[loc, 1].Int.Value := a2;
      FCurrentAttribValue[loc, 2].Int.Value := a3;
      FCurrentAttribValue[loc, 3].Int.Value := a4;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4i(Attrib: TGLSLAttribute;
  const a: TVector4i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4I);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].Int.Value := a[0];
      FCurrentAttribValue[loc, 1].Int.Value := a[1];
      FCurrentAttribValue[loc, 2].Int.Value := a[2];
      FCurrentAttribValue[loc, 3].Int.Value := a[3];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute1ui(Attrib: TGLSLAttribute;
  a1: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a1;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2ui(Attrib: TGLSLAttribute; a1, a2: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a1;
      FCurrentAttribValue[loc, 1].UInt.Value := a2;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute2ui(Attrib: TGLSLAttribute;
  const a: TVector2ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a[0];
      FCurrentAttribValue[loc, 1].UInt.Value := a[1];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3ui(Attrib: TGLSLAttribute;
  a1, a2, a3: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a1;
      FCurrentAttribValue[loc, 1].UInt.Value := a2;
      FCurrentAttribValue[loc, 2].UInt.Value := a3;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute3ui(Attrib: TGLSLAttribute;
  const a: TVector3ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a[0];
      FCurrentAttribValue[loc, 1].UInt.Value := a[1];
      FCurrentAttribValue[loc, 2].UInt.Value := a[2];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4ui(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a1;
      FCurrentAttribValue[loc, 1].UInt.Value := a2;
      FCurrentAttribValue[loc, 2].UInt.Value := a3;
      FCurrentAttribValue[loc, 3].UInt.Value := a4;
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.Attribute4ui(Attrib: TGLSLAttribute;
  const a: TVector4ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4UI);
  if loc > -1 then
  begin
    if FMesh.FAttributeDivisor[loc] > 0 then
    begin
      FMesh.FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      FCurrentAttribValue[loc, 0].UInt.Value := a[0];
      FCurrentAttribValue[loc, 1].UInt.Value := a[1];
      FCurrentAttribValue[loc, 2].UInt.Value := a[2];
      FCurrentAttribValue[loc, 3].UInt.Value := a[3];
    end;
  end;
end;

procedure TGLAbstractMeshBuilder.AttributeList(Attrib: TGLSLAttribute; AList: T4ByteList);
var
  loc: integer;
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLTypeVoid);
  if loc > -1 then
  begin
    Valid := false;
    case FMesh.FDataFormat[loc] of
      GLSLType1F: Valid := true;
      GLSLType2F: Valid := (AList.Count mod 2 = 0);
      GLSLType3F: Valid := (AList.Count mod 3 = 0);
      GLSLType4F: Valid := (AList.Count mod 4 = 0);
    end;
    if not Valid then
    begin
      GLSLogger.LogWarning(glsWrongAttrType);
      Abort;
    end;

    AA := FMesh.FAttributeArrays[loc];
    Last := AA.Count;
    AA.Count := Last + AList.Count;
    System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
  end;
end;

procedure TGLAbstractMeshBuilder.EmitVertex;
var
  A, C, Size: Integer;
  I: LongWord;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  // Push vertex attributes into lists
  with FMesh do
  begin
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(FAttributes[A]) and not (FAttributeDivisor[A] > 0) then
      begin
        Size := GLSLTypeComponentCount(FDataFormat[A]);
        for C := 0 to Size - 1 do
          FAttributeArrays[A].Push(FCurrentAttribValue[A][C]);
      end;

    with FLODFaceGroupMap[FCurrentLOD][FCurrentBatch] do
    begin
      I := ElementOffset + VertexCount;
      FElementBuffer.Add(I);
      Inc(VertexCount);
    end;
  end;
  FRemoveLastElement := False;
end;

procedure TGLAbstractMeshBuilder.EmitVertices(ANumber: LongWord);
var
  A, Size: Integer;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  // Increase vertex attributes lists
  with FMesh do
  begin
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(FAttributes[A]) and not (FAttributeDivisor[A] > 0) then
      begin
        Size := GLSLTypeComponentCount(FDataFormat[A]);
        FAttributeArrays[A].Count := FAttributeArrays[A].Count + Size * Integer(ANumber);
      end;
    FElementBuffer.AddNulls(ANumber);
    Inc(FLODFaceGroupMap[FCurrentLOD][FCurrentBatch].VertexCount, ANumber);
  end;
end;

procedure TGLAbstractMeshBuilder.WeldVertices;
var
  StoreAttribArrays: array[0..GLS_VERTEX_ATTR_NUM - 1] of T4ByteList;
  StoreElementBuffer: T4ByteList;
  VertexIndex: LongWord;

  function SameVertex(const Item1, Item2: LongWord): Boolean;
  var
    AA, size: Integer;
    p1, p2: Pointer;
  begin
    if Item1 <> Item2 then
      with FMesh do
      begin
        for AA := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
          if Assigned(FAttributes[AA]) then
          begin
            size := GLSLTypeComponentCount(FDataFormat[AA]);
            p1 := @StoreAttribArrays[AA].List[Integer(Item1) * size];
            p2 := @StoreAttribArrays[AA].List[Integer(Item2) * size];
            if not CompareMem(p1, p2, size * SizeOf(T4ByteData)) then
              exit(False);
          end;
      end;
    Result := True;
  end;

  procedure CopyVertex(N: Integer);
  var
    AA, C, size: Integer;
  begin
    for AA := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(FMesh.FAttributes[AA]) then
      begin
        size := GLSLTypeComponentCount(FMesh.FDataFormat[AA]);
        for C := 0 to size - 1 do
          FMesh.FAttributeArrays[AA].Add(StoreAttribArrays[AA].Items[N * size + C]);
      end;
    Inc(VertexIndex);
  end;

var
  A, I, J, Size: Integer;
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
      BD2 := StoreAttribArrays[FVertexPositionSlot][J + K];
      pKey^ := BD2.Word.Value[1];
      Inc(pKey);
    end;
    VertexHashKey[I] := vertexKey;
  end;

var
  HasHash: Boolean;

begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  E_ := 0; // Drop compilator warning

  // Add vertex to hash tree
  if FVertexPositionSlot > -1 then
    with FMesh do
    begin
      for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        StoreAttribArrays[A] := FAttributeArrays[A];

      // Calculate hash keys
      VertexHashMap := TVertexHashMap.Create(CompareVertexKey, CompareVertex);
      VertexHashKey := TDoubleList.Create;
      VertexHashKey.Count := FElementBuffer.Count;
      Size := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
      MeshBuilder := Self;
      for I := 0 to FElementBuffer.Count - 1 do
      begin
        E := FElementBuffer[I].UInt.Value;
        if E = cRestartIndex then
          continue;
        CalcHashKay;
        if VertexHashMap.Find(vertexKey, J) then
        begin
          BD := FElementBuffer[J];
          E_ := BD.UInt.Value;
          HasHash := (E_ >= E) or not SameVertex(E, E_);
        end
        else
          HasHash := True;
        if HasHash then
          VertexHashMap.Add(vertexKey, I);
      end;

      for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        FAttributeArrays[A] := T4ByteList.Create;
      StoreElementBuffer := T4ByteList.Create;
      StoreElementBuffer.Assign(FElementBuffer);

      // Remap element buffer, fill new attributes list
      VertexIndex := 0;
      for I := 0 to FElementBuffer.Count - 1 do
      begin
        BD := FElementBuffer[I];
        E := BD.UInt.Value;
        if E = cRestartIndex then
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
          GLSLogger.LogWarningFmt(glsMeshBuilderHashingError,
            [ClassName, FMesh.FName.Value]);
          continue;
        end;

        if E_ >= E then
        begin
          BD.UInt.Value := VertexIndex;
          FElementBuffer[I] := BD;
          CopyVertex(E);
        end
        else
        begin
          FElementBuffer[I] := FElementBuffer[J];
        end;
      end;

      // Free unpacked arrays
      for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        StoreAttribArrays[A].Destroy;
      StoreElementBuffer.Destroy;

      FHasIndices := True;
      FRestartIndex := cRestartIndex;
      FTriOnlyBufferNeedUpdate := True;

      VertexHashMap.Destroy;
      VertexHashKey.Destroy;
    end;
end;

procedure TGLAbstractMeshBuilder.SplitVertices;
var
  StoreAttribArrays: array[0..GLS_VERTEX_ATTR_NUM - 1] of T4ByteList;
  StoreElementBuffer: T4ByteList;
  I, A, C, Size: Integer;
  E: LongWord;
  BD: T4ByteData;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  with FMesh do
  begin
    if not FHasIndices then
      exit;
    // create new arrays
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    begin
      StoreAttribArrays[A] := FAttributeArrays[A];
      FAttributeArrays[A] := T4ByteList.Create;
      FAttributeArrays[A].Capacity := StoreAttribArrays[A].Count;
    end;
    StoreElementBuffer := FElementBuffer;
    FElementBuffer := T4ByteList.Create;
    FElementBuffer.Capacity := StoreElementBuffer.Count;

    // unpack arrays
    for I := 0 to StoreElementBuffer.Count - 1 do
    begin
      BD := StoreElementBuffer[I];
      E := BD.UInt.Value;
      for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        if Assigned(FAttributes[A]) then
        begin
          size := GLSLTypeComponentCount(FMesh.FDataFormat[A]);
          for C := 0 to Size - 1 do
            FAttributeArrays[A].Add(StoreAttribArrays[A].Items[Integer(E) * Size + C]);
        end;
      FElementBuffer.Add(I);
    end;

    // Free packed arrays
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      StoreAttribArrays[A].Destroy;
    StoreElementBuffer.Destroy;

    for I := 0 to High(FLODFaceGroupMap[FCurrentLOD]) do
      with FLODFaceGroupMap[FCurrentLOD][I] do
      begin
        VertexOffset := ElementOffset;
        VertexCount := ElementCount;
      end;

    FHasIndices := False;
    FTriOnlyBufferNeedUpdate := True;
  end;
end;

procedure TGLAbstractMeshBuilder.Triangulate;
var
  I, J: Integer;
  StoreElementBuffer: T4ByteList;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  with FMesh do
  begin
    if FTriOnlyBufferNeedUpdate then
      MakeTriangleOnlyBuffer;

    StoreElementBuffer := FElementBuffer;
    FElementBuffer := T4ByteList.Create;

    for I := 0 to High(FLODFaceGroupMap[FCurrentLOD]) do
      with FLODFaceGroupMap[FCurrentLOD][I] do
      begin
        ElementOffset := FElementBuffer.Count;
        case PrimitiveType of
          mpTRIANGLE_STRIP, mpTRIANGLE_FAN:
            begin
              // Replace strips and fans to common triangles
              for J := TriOnlyElementOffset to TriOnlyElementOffset + TriOnlyElementCount - 1 do
                FElementBuffer.Add(FElementBufferTrianglesOnly[J]);
              PrimitiveType := mpTRIANGLES;
            end;
        else
          begin
            // For other primitives just copy elements
            for J := ElementOffset to ElementOffset + ElementCount - 1 do
              FElementBuffer.Add(StoreElementBuffer[J]);
          end;
        end;
        ElementCount := Cardinal(FElementBuffer.Count) - ElementOffset;
      end;

    StoreElementBuffer.Destroy;
  end;

end;

procedure TGLAbstractMeshBuilder.MakeTriangleOnlyBuffer;
var
  I, J: Integer;
  Index, prevIndex1, prevIndex2, stripCount: LongWord;
  centerIndex, prevIndex, fansCount: LongWord;
  degenerate: Boolean;
  BD: T4ByteData;
begin
  with FMesh do
  begin
    FElementBufferTrianglesOnly.Clear;
    for I := 0 to High(FLODFaceGroupMap[FCurrentLOD]) do
      with FLODFaceGroupMap[FCurrentLOD][I] do
      begin
        TriOnlyElementOffset := FElementBufferTrianglesOnly.Count;
        case PrimitiveType of
          mpTRIANGLES:
            begin
              for J := ElementOffset to ElementOffset + ElementCount - 1 do
                FElementBufferTrianglesOnly.Add(FElementBuffer[J]);
            end;

          mpTRIANGLE_STRIP:
            begin
              stripCount := 0;
              prevIndex1 := 0;
              prevIndex2 := 0;
              for J := ElementOffset to ElementOffset + ElementCount - 1 do
              begin
                BD := FElementBuffer[J];
                Index := BD.UInt.Value;
                if stripCount > 2 then
                begin
                  // Check for restart index
                  if Index = cRestartIndex then
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
                    FElementBufferTrianglesOnly.Add(prevIndex2);
                    FElementBufferTrianglesOnly.Add(prevIndex1);
                  end
                  else
                  begin
                    FElementBufferTrianglesOnly.Add(prevIndex1);
                    FElementBufferTrianglesOnly.Add(prevIndex2);
                  end;
                end
                else if stripCount = 2 then
                begin
                  FElementBufferTrianglesOnly.Add(Index);
                  BD.UInt.Value := prevIndex1;
                  FElementBufferTrianglesOnly.Items[FElementBufferTrianglesOnly.Count - 2] := BD;
                  prevIndex2 := prevIndex1;
                  prevIndex1 := Index;
                  Inc(stripCount);
                  continue;
                end;
                FElementBufferTrianglesOnly.Add(Index);
                prevIndex2 := prevIndex1;
                prevIndex1 := Index;
                Inc(stripCount);
              end;
            end;

          mpTRIANGLE_FAN:
            begin
              fansCount := 0;
              prevIndex := 0;
              degenerate := False;
              BD := FElementBuffer[0];
              centerIndex := BD.UInt.Value;
              for J := ElementOffset to ElementOffset + ElementCount - 1 do
              begin
                BD := FElementBuffer[J];
                Index := BD.UInt.Value;
                if fansCount > 2 then
                begin
                  // Check for restart index
                  if Index = cRestartIndex then
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
                  FElementBufferTrianglesOnly.Add(centerIndex);
                  FElementBufferTrianglesOnly.Add(prevIndex);
                end
                else if fansCount = 0 then
                  centerIndex := Index;
                FElementBufferTrianglesOnly.Add(Index);
                prevIndex := Index;
                Inc(fansCount);
              end;
            end;
        else
          continue;
        end; // of case
        TriOnlyElementCount := Cardinal(FElementBufferTrianglesOnly.Count) - TriOnlyElementOffset;
      end;
    FTriOnlyBufferNeedUpdate := False;
  end;
end;

procedure TGLAbstractMeshBuilder.MakeAdjacency;
begin
  if FState <> mmsAssembling then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  if not FMesh.FHasIndices then
    WeldVertices;
  // TODO
end;

procedure TGLAbstractMeshBuilder.ComputeNormals(ASmooth: Boolean);
var
  Positions: T4ByteList;
  PosSize, MoveSize: Integer;

  function GetPosition(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(Positions.List[Index * PosSize], Result.V[0], MoveSize);
  end;

var
  N, T, I, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, fNormal, nNormal, cNormal: TVector3fEXT;
  newNormals: TAffineVectorList;
  newNormalIndices: TIntegerList;
  collisionMap: TIntIntRBT;
  Agrees: Boolean;
  BD: T4ByteData;
begin

  with FMesh do
  begin
    FVertexPositionSlot := FindSlot(attrPosition);
    if FVertexPositionSlot < 0 then
      exit; // Nothing todo
    Positions := FAttributeArrays[FVertexPositionSlot];
    PosSize := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
    MoveSize := MinInteger(PosSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    N := FindOrOccupySlot(attrNormal);
    if N < 0 then
      exit; // No free slot

    // Clear old normals
    FAttributes[N] := nil;
    FAttributeArrays[N].Clear;

    if not FHasIndices then
      WeldVertices;

    Triangulate;

    if FTriOnlyBufferNeedUpdate then
      MakeTriangleOnlyBuffer;

    // Allocate and initialize the normal values
    newNormals := TAffineVectorList.Create;
    newNormals.SetCountResetsMemory := True;
    newNormals.Count := FElementBufferTrianglesOnly.Count;
    newNormalIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the normals
    collisionMap := TIntIntRBT.Create(CompareInteger, CompareInteger_);
    collisionMap.DuplicateKeys := True;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to FElementBufferTrianglesOnly.Count div 3 - 1 do
    begin
      E := 3 * T;
      BD := FElementBufferTrianglesOnly[E + 0];
      p0 := GetPosition(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 1];
      p1 := GetPosition(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 2];
      p2 := GetPosition(BD.Int.Value);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face normal
      fNormal := dp0.Cross(dp1);

      if not ASmooth then
      begin
        BD := FElementBufferTrianglesOnly[E + 0];
        EJ := BD.Int.Value;
        newNormals[EJ] := fNormal;
        newNormalIndices.Add(EJ);
        BD := FElementBufferTrianglesOnly[E + 1];
        EJ := BD.Int.Value;
        newNormals[EJ] := fNormal;
        newNormalIndices.Add(EJ);
        BD := FElementBufferTrianglesOnly[E + 2];
        EJ := BD.Int.Value;
        newNormals[EJ] := fNormal;
        newNormalIndices.Add(EJ);
        continue;
      end;

      // Compute a normalized normal
      nNormal := fNormal.Normal;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        BD := FElementBufferTrianglesOnly[E + J];
        EJ := BD.Int.Value;
        cNormal := newNormals[EJ];

        // Check to see if this normal has not yet been touched
        if cNormal.IsNull then
        begin
          // First instance of this index, just store it as is
          newNormals[EJ] := fNormal;
          newNormalIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement
          cNormal.Normalize;

          if cNormal.Dot(nNormal) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            newNormals[EJ] := newNormals[EJ] + fNormal;
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
                cNormal.Normalize;
                if cNormal.Dot(nNormal) >= cos(3.1415926 * 0.333333) then
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
              newNormals[E_] := newNormals[E_] + fNormal;
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
    SplitVertices;
    MakeTriangleOnlyBuffer;
    FAttributes[N] := attrNormal;
    FAttributeArrays[N].Count := 3 * newNormalIndices.Count;
    FDataFormat[N] := GLSLType3F;
    FAttributeDivisor[N] := 0;
    for I := 0 to newNormalIndices.Count - 1 do
    begin
      E := newNormalIndices[I];
      BD := FElementBufferTrianglesOnly[I];
      E_ := 3 * BD.Int.Value;
      BD.Float.Value := newNormals[E][0];
      FAttributeArrays[N].Items[E_ + 0] := BD;
      BD.Float.Value := newNormals[E][1];
      FAttributeArrays[N].Items[E_ + 1] := BD;
      BD.Float.Value := newNormals[E][2];
      FAttributeArrays[N].Items[E_ + 2] := BD;
    end;
    WeldVertices;

    newNormals.Destroy;
    newNormalIndices.Destroy;
    collisionMap.Destroy;
  end;
end;

procedure TGLAbstractMeshBuilder.ComputeTexCoords;
var
  Positions: T4ByteList;
  PosSize, PosMoveSize: Integer;

  function GetPosition(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(Positions.List[Index * PosSize], Result.V[0], PosMoveSize);
  end;

var
  N, I, T, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector3fEXT;
  TBN: TMatrix3fEXT;
  newTexCoords: TTexPointList;
  BD: T4ByteData;
  TT: TTexPoint;
begin
  with FMesh do
  begin
    FVertexPositionSlot := FindSlot(attrPosition);
    if FVertexPositionSlot < 0 then
      exit; // Nothing todo
    Positions := FAttributeArrays[FVertexPositionSlot];
    PosSize := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
    PosMoveSize := MinInteger(PosSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    N := FindOrOccupySlot(attrTexCoord0);
    if N < 0 then
      exit; // No free slot

    // Clear old texture coordinates
    FAttributes[N] := nil;
    FAttributeArrays[N].Clear;

    if not FHasIndices then
      WeldVertices;

    Triangulate;

    if FTriOnlyBufferNeedUpdate then
      MakeTriangleOnlyBuffer;

    // Allocate and initialize the tangent values
    newTexCoords := TTexPointList.Create;
    newTexCoords.SetCountResetsMemory := True;
    newTexCoords.Count := FElementBufferTrianglesOnly.Count;

    EJ := 0;
    for T := 0 to FElementBufferTrianglesOnly.Count div 3 - 1 do
    begin
      E := 3 * T;
      BD := FElementBufferTrianglesOnly[E + 0];
      p0 := GetPosition(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 1];
      p1 := GetPosition(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 2];
      p2 := GetPosition(BD.Int.Value);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face TBN
      fNormal := dp0.Cross(dp1);
      fNormal.Normalize;
      fTangent := dp0;
      fTangent.Normalize;
      fBinormal := fNormal.Cross(fTangent);
      TBN.Row[0] := fTangent;
      TBN.Row[1] := fBinormal;
      TBN.Row[2] := fNormal;
      TBN := TBN.Inverse;

      p0 := TBN * p0;
      TT.S := p0.X;
      TT.T := p0.Y;
      newTexCoords[EJ] := TT;
      Inc(EJ);

      p1 := TBN * p1;
      TT.S := p1.X;
      TT.T := p1.Y;
      newTexCoords[EJ] := TT;
      Inc(EJ);

      p2 := TBN * p2;
      TT.S := p2.X;
      TT.T := p2.Y;
      newTexCoords[EJ] := TT;
      Inc(EJ);
    end;

    // Place new texture coordinates
    SplitVertices;
    MakeTriangleOnlyBuffer;
    FAttributes[N] := attrTexCoord0;
    FAttributeArrays[N].Count := 2 * newTexCoords.Count;
    FDataFormat[N] := GLSLType2F;
    FAttributeDivisor[N] := 0;
    for I := 0 to newTexCoords.Count - 1 do
    begin
      BD := FElementBufferTrianglesOnly[I];
      E_ := 3 * BD.Int.Value;
      TT := newTexCoords[I];
      BD.Float.Value := TT.S;
      FAttributeArrays[N].Items[E_ + 0] := BD;
      BD.Float.Value := TT.T;
      FAttributeArrays[N].Items[E_ + 1] := BD;
    end;
    WeldVertices;

    newTexCoords.Destroy;
  end;
end;

procedure TGLAbstractMeshBuilder.ComputeTangents;
var
  Positions: T4ByteList;
  PosSize, PosMoveSize: Integer;
  TexCoords: T4ByteList;
  TexCoordSize, TexCoordMoveSize: Integer;

  function GetPosition(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(Positions.List[Index * PosSize], Result.V[0], PosMoveSize);
  end;

  function GetTexCoord(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(TexCoords.List[Index * TexCoordSize], Result.V[0], TexCoordMoveSize);
  end;

var
  N, T, I, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1,
    st0, st1, st2, dst0, dst1,
    fTangent, nTangent, cTangent: TVector3fEXT;
  factor: Single;
  newTangents: TAffineVectorList;
  newTangentIndices: TIntegerList;
  collisionMap: TIntIntRBT;
  Agrees: Boolean;
  BD: T4ByteData;
begin

  with FMesh do
  begin
    FVertexPositionSlot := FindSlot(attrPosition);
    if FVertexPositionSlot < 0 then
      exit; // Nothing todo
    Positions := FAttributeArrays[FVertexPositionSlot];
    PosSize := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
    PosMoveSize := MinInteger(PosSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    N := FindSlot(attrTexCoord0);
    if N < 0 then
    begin
      ComputeTexCoords;
      N := FindSlot(attrTexCoord0);
      if N < 0 then
        exit;
    end;

    TexCoords := FAttributeArrays[N];
    TexCoordSize := GLSLTypeComponentCount(FDataFormat[N]);
    TexCoordMoveSize := MinInteger(TexCoordSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    N := FindOrOccupySlot(attrTangent);
    if N < 0 then
      exit; // No free slot

    // Clear old tangents
    FAttributes[N] := nil;
    FAttributeArrays[N].Clear;

    if not FHasIndices then
      WeldVertices;

    Triangulate;

    if FTriOnlyBufferNeedUpdate then
      MakeTriangleOnlyBuffer;

    // Allocate and initialize the tangent values
    newTangents := TAffineVectorList.Create;
    newTangents.SetCountResetsMemory := True;
    newTangents.Count := FElementBufferTrianglesOnly.Count;
    newTangentIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the tangent
    collisionMap := TIntIntRBT.Create(CompareInteger, CompareInteger_);
    collisionMap.DuplicateKeys := True;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to FElementBufferTrianglesOnly.Count div 3 - 1 do
    begin
      E := 3 * T;
      BD := FElementBufferTrianglesOnly[E + 0];
      p0 := GetPosition(BD.Int.Value);
      st0 := GetTexCoord(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 1];
      p1 := GetPosition(BD.Int.Value);
      st1 := GetTexCoord(BD.Int.Value);
      BD := FElementBufferTrianglesOnly[E + 2];
      p2 := GetPosition(BD.Int.Value);
      st2 := GetTexCoord(BD.Int.Value);

      // Compute the edge and tc differentials
      dp0 := p1 - p0;
      dp1 := p2 - p0;
      dst0 := st1 - st0;
      dst1 := st2 - st0;

      factor := 1.0 / (dst0.X * dst1.Y - dst1.X * dst0.Y);

      //compute fTangent
      fTangent.X := dp0.X * dst1.Y - dp1.X * dst0.Y;
      fTangent.Y := dp0.Y * dst1.Y - dp1.Y * dst0.Y;
      fTangent.Z := dp0.Z * dst1.Y - dp1.Z * dst0.Y;
      fTangent := fTangent * factor;

      //should this really renormalize?
      nTangent := fTangent.Normal;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        BD := FElementBufferTrianglesOnly[E + J];
        EJ := BD.Int.Value;
        cTangent := newTangents[EJ];

        // Check to see if this normal has not yet been touched
        if cTangent.IsNull then
        begin
          // First instance of this index, just store it as is
          newTangents[EJ] := fTangent;
          newTangentIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement
          cTangent.Normalize;

          if cTangent.Dot(nTangent) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            newTangents[EJ] := newTangents[EJ] + fTangent;
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
                cTangent.Normalize;
                if cTangent.Dot(nTangent) >= cos(3.1415926 * 0.333333) then
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
              newTangents[E_] := newTangents[E_] + fTangent;
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
    MakeTriangleOnlyBuffer;
    FAttributes[N] := attrTangent;
    FAttributeArrays[N].Count := 3 * newTangentIndices.Count;
    FDataFormat[N] := GLSLType3F;
    FAttributeDivisor[N] := 0;
    for I := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[I];
      BD := FElementBufferTrianglesOnly[I];
      E_ := 3 * BD.Int.Value;
      BD.Float.Value := newTangents[E][0];
      FAttributeArrays[N].Items[E_ + 0] := BD;
      BD.Float.Value := newTangents[E][1];
      FAttributeArrays[N].Items[E_ + 1] := BD;
      BD.Float.Value := newTangents[E][2];
      FAttributeArrays[N].Items[E_ + 2] := BD;
    end;
    WeldVertices;

    newTangents.Destroy;
    newTangentIndices.Destroy;
    collisionMap.Destroy;
  end;

end;

procedure TGLAbstractMeshBuilder.ComputeBoundingBox;
var
  Positions: T4ByteList;
  PosSize, PosMoveSize: Integer;
  I: Integer;
  min, max, p: TVector3fEXT;
  BD: T4ByteData;

  function GetPosition(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(Positions.List[Index * PosSize], Result.V[0], PosMoveSize);
  end;

begin
  with FMesh do
  begin
    FVertexPositionSlot := FindSlot(attrPosition);
    if FVertexPositionSlot < 0 then
      exit; // Nothing todo

    Positions := FAttributeArrays[FVertexPositionSlot];
    PosSize := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
    PosMoveSize := MinInteger(PosSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    min := VectorMakeEXT(1e10, 1e10, 1e10);
    max := -min;

    for I := 0 to FElementBuffer.Count - 1 do
    begin
      if FElementBuffer[I].UInt.Value = FRestartIndex then
        continue;
      BD := FElementBuffer[I];
      p := GetPosition(BD.Int.Value);
      min.Min(p);
      max.Max(p);
    end;

    FAABB.min := min;
    FAABB.max := max;
  end;
end;

function TGLAbstractMeshBuilder.GetAABB: TAABB;
begin
  Result := FMesh.FAABB;
end;

function TGLAbstractMeshBuilder.GetFaceGroupName: string;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  Result := FMesh.FLODFaceGroupMap[FCurrentLOD][FCurrentBatch].Name;
end;

procedure TGLAbstractMeshBuilder.SetFaceGroupName(const AValue: string);
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  FMesh.FLODFaceGroupMap[FCurrentLOD][FCurrentBatch].Name := AValue;
end;

function TGLAbstractMeshBuilder.GetFaceGroupMaterialName: IGLName;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  Result := FMesh.FLODFaceGroupMap[FCurrentLOD][FCurrentBatch].Material;
end;

procedure TGLAbstractMeshBuilder.SetFaceGroupMaterialName(const AValue: IGLName);
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;
  FMesh.FLODFaceGroupMap[FCurrentLOD][FCurrentBatch].Material := AValue;
end;

procedure TGLAbstractMeshBuilder.Rescale(ARadius: Single);
var
  Positions: T4ByteList;
  PosSize, PosMoveSize: Integer;
  I, E: Integer;
  min, max, p, r, center: TVector3fEXT;
  oldRadius, scale: Single;
  BD: T4ByteData;

  function GetPosition(Index: Integer): TVector3fEXT;
  begin
    Result := NullVector;
    Move(Positions.List[Index * PosSize], Result.V[0], PosMoveSize);
  end;

  procedure SetPosition(Index: Integer; const Value: TVector3fEXT);
  begin
    Move(Value.V[0], Positions.List[Index * PosSize], PosMoveSize);
  end;

begin
  with FMesh do
  begin
    ComputeBoundingBox;
    if FVertexPositionSlot < 0 then
      exit; // Nothing todo

    Positions := FAttributeArrays[FVertexPositionSlot];
    PosSize := GLSLTypeComponentCount(FDataFormat[FVertexPositionSlot]);
    PosMoveSize := MinInteger(PosSize * SizeOf(T4ByteData), 3 * SizeOf(Single));

    min := FAABB.min;
    max := FAABB.max;
    r := max - min;
    r := r * 0.5;
    center := min + r;
    oldRadius := MaxFloat(r.X, MaxFloat(r.Y, r.Z));
    scale := ARadius / oldRadius;
    min := VectorMakeEXT(1e10, 1e10, 1e10);
    max := -min;

    for I := 0 to FElementBuffer.Count - 1 do
    begin
      BD := FElementBuffer[I];
      if BD.UInt.Value = FRestartIndex then
        continue;
      E := BD.Int.Value;
      p := GetPosition(E);
      p := p - center;
      p := p * scale;
      SetPosition(E, p);
      min.Min(p);
      max.Max(p);
    end;
    FAABB.min := min;
    FAABB.max := max;
  end;
end;

procedure TGLAbstractMeshBuilder.RestartStrip;
var
  I: T4ByteData;
begin
  if FState <> mmsPrimitives then
  begin
    GLSLogger.LogWarning(glsMeshBuilderWrongCall);
    exit;
  end;

  if not (FCurrentPrimitive in
    [mpTRIANGLE_STRIP, mpTRIANGLE_FAN, mpLINE_STRIP]) then
  begin
    GLSLogger.LogWarningFmt(glsMeshBuilderNoNeedRestart, [ClassName]);
    exit;
  end;

  if UsePrimitiveRestart then
  begin
    FMesh.FElementBuffer.Add(cRestartIndex);
  end
  else
  begin
    I := FMesh.FElementBuffer[FMesh.FElementBuffer.Count - 1];
    FMesh.FElementBuffer.Add(I);
    I.UInt.Value := I.UInt.Value + 1;
    FMesh.FElementBuffer.Add(I);
  end;
  FRemoveLastElement := True;
end;
{$ENDREGION 'TGLAbstractMeshBuilder'}

{$REGION 'MeshManager'}
// ------------------
// ------------------ MeshManager ------------------
// ------------------

class procedure MeshManager.Initialize;
begin
  LoadResources;
end;

class procedure MeshManager.Finalize;
begin
  ClearResources;
end;

class procedure MeshManager.ClearResources;
var
  I: Integer;
begin
  for I := 0 to High(Meshes) do
    FreeAndNil(Meshes[I]);
  Meshes := nil;
end;

class procedure MeshManager.NotifyContextCreated;
begin
end;

class procedure MeshManager.NotifyBeforeCompile;
begin
  SaveResources;
end;

class function MeshManager.Priority: Byte;
begin
  Result := 252;
end;

class function MeshManager.FillResourceList(AList: TStringList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Meshes) > 1 then
  begin
    AList.Add('[STATIC MESHES]');
    for I := 1 to High(Meshes) do
    begin
      if Assigned(Meshes[I]) and (Length(Meshes[I].ResourceName) > 0) then
      begin
        AList.Add(Format('%s=%s', [Meshes[I].Name.Value, ExtractFileName(Meshes[I].ResourceName)]));
        Result := True;
      end;
    end;
  end;
end;

class procedure MeshManager.SaveResources;
var
  I: Integer;

  procedure SaveRes(ARes: TDataFile);
  begin
    if Assigned(ARes) and (Length(ARes.ResourceName) > 0) then
      ARes.SaveToFile(ARes.ResourceName);
  end;

begin
  for I := 1 to High(Meshes) do
    SaveRes(Meshes[I]);
end;

class procedure MeshManager.NotifyProjectOpened;
begin
  if IsDesignTime then
  begin
    ClearResources;
    LoadResources;
  end;
end;

class procedure MeshManager.NotifyProjectClosed;
begin
  if IsDesignTime then
  begin
    SaveResources;
    ClearResources;
  end;
end;

class procedure MeshManager.LoadResources;
var
  I: Integer;
  rStream: TGLSResourceStream;
  ResList: TStringList;
  RT, RT_: TGLSApplicationResource;
  line, rName, rFile: string;
  StaticMeshClass: TGLAbstractMeshClass;
  newMesh: TGLAbstractMesh;

  procedure GetNameAndFile;
  var
    p: Integer;
  begin
    p := Pos('=', line);
    rName := Copy(line, 1, p - 1);
    rFile := Copy(line, p + 1, Length(line) - p + 1);
  end;

begin
  try
    BeginWork;
    StaticMeshClass := TGLAbstractMeshClass(FindClass('TGL3xStaticMesh'));

    if Length(Meshes) = 0 then
    begin
      newMesh := StaticMeshClass.Create(nil);
      newMesh.Name.Value := glsDEFAULTMESHNAME;
      newMesh.ImportFromFile('');
      PushMesh(newMesh);
    end;

    // Load materials and textures info from application resource
    GLSLogger.Enabled := False;
    ResList := TStringList.Create;

    if IsDesignTime then
    begin
      ResList.Text := vManagersResourceList;
    end
    else
    begin
      rStream := CreateResourceStream(glsResourceInfo, GLS_RC_String_Type);
      if Assigned(rStream) then
      begin
        ResList.LoadFromStream(rStream);
        rStream.Destroy;
      end;
    end;

    GLSLogger.Enabled := True;

    RT := aresNone;
    SetExeDirectory;

    for I := 0 to ResList.Count - 1 do
    begin
      line := ResList.Strings[I];
      RT_ := StrToGLSResType(line);
      if (RT_ <> aresNone) and (RT <> RT_) then
      begin
        RT := RT_;
        continue;
      end;
      case RT of
        aresMesh:
          begin
            GetNameAndFile;
            if FileStreamExists(rFile) then
            begin
              newMesh := StaticMeshClass.Create(nil);
              newMesh.Name.Value := rName;
              newMesh.LoadFromFile(rFile);
              PushMesh(newMesh);
            end
            else
              GLSLogger.LogWarningFmt(glsMissingResource, [StaticMeshClass.ClassName, rName]);
          end;
      end;
    end;
    ResList.Destroy;
  finally
    EndWork;
  end;
end;

class function MeshManager.GetMeshName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[i]);

  for I := 0 to High(Meshes) do
  begin
    if Assigned(Meshes[I]) then
    begin
      if (Meshes[I].FName.HashCode = H)
        and (Meshes[I].FName.Value = AName) then
        exit(Meshes[I].FName);
    end;
  end;

  Result := Meshes[0].FName;
end;

class function MeshManager.GetMeshBuilder(const AName: IGLName): TGLAbstractMeshBuilder;
var
  mesh: TGLAbstractMesh;
begin
  mesh := GetMesh(AName);
  Result := TGLAbstractMeshBuilder(mesh.GetBuilder);
end;

class procedure MeshManager.PushMesh(AMesh: TGLAbstractMesh);
var
  I: Integer;
begin
  SetLength(Meshes, Length(Meshes) + 1);
  I := High(Meshes);
  AMesh.Name.SetIndex(I);
  Meshes[I] := AMesh;
end;

class function MeshManager.GetMesh(const AName: IGLName): TGLAbstractMesh;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGL3xMeshName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(Meshes[I]);
  end;
  Result := Meshes[0];
end;

class procedure MeshManager.MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass);
const
  cMeshName = 'StaticMesh';
var
  N: Integer;
begin
  CheckCall;

  if AClass = TGL3xMeshName then
  begin
    if Length(Meshes) < 1 then
      exit;
    if (Length(AName) = 0)
      or (AName = glsDEFAULTMESHNAME) then
      AName := cMeshName;
    N := 1;
    while GetMeshName(AName).GetValue <> glsDEFAULTMESHNAME do
    begin
      AName := AName + IntToHex(N, 2);
      Inc(N);
    end;
  end
end;

class procedure MeshManager.FillMeshNameList(var AList: TStringList);
var
  I: Integer;
begin
  try
    BeginWork;
    if not Assigned(AList) then
      AList := TStringList.Create;
    AList.Clear;
    AList.Add(Meshes[0].Name.Value);
    for I := 1 to High(Meshes) do
      if Assigned(Meshes[I]) and (Length(Meshes[I].ResourceName) > 0) then
        AList.Add(Meshes[I].Name.Value);
  finally
    EndWork;
  end;
end;

class function MeshManager.CreateMesh(AName: string;
  AClass: TGLAbstractMeshClass; const AFileName, AImportFile: string): IGLName;
var
  newMesh: TGLAbstractMesh;
begin
  CheckCall;
  MakeUniqueItemName(AName, TGL3xMeshName);
  newMesh := AClass.Create(nil);
  newMesh.Name.Value := AName;
  PushMesh(newMesh);
  if Length(AImportFile) > 0 then
    newMesh.ImportFromFile(AImportFile);
  if Length(AFileName) > 0 then
    newMesh.SaveToFile(AFileName);
  Result := newMesh.Name;
end;

{$ENDREGION 'MeshManager'}

{$IFDEF GLS_EXPERIMENTAL}

initialization

  RegisterGLSceneManager(MeshManager);

finalization

  MeshManager.Finalize;
  FreeAndNil(VectorFileFormats);

{$ENDIF GLS_EXPERIMENTAL}

end.

