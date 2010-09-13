//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLVBOManagers<p>

   Base unit to drawing geometry data in vertex buffer objects.<p>
   Require OpenGL 2.1.<p>
   Try to not change buffer usage during runtime because static
   and stream data not deleted from video memory until application running.<p>

   <b>History : </b><font size=-1><ul>
    <li>13/09/10 - Yar - Now static manager create big pool for vertices and indices
                         and can upload geometry anytime
    <li>09/09/10 - Yar - Added patch primitives and VerticesInPatch property
    <li>20/07/10 - Yar - Improved VAO refreshing
    <li>14/04/10 - Yar - Vertex array object per program request
    <li>29/03/10 - Yar - Added multicontext and multithreading support
    <li>24/03/10 - Yar - Creation
 </ul></font>
}

// TODO: Matrix attribute
// TODO: Bindless graphic
// TODO: Auto-normalization for attribute

unit GLVBOManager;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  // GLScene
  GLCrossPlatform,
  BaseClasses,
  OpenGLTokens,
  GLContext,
  GLState,
  GLShaderManager,
  VectorTypes,
  VectorLists,
  GLSRedBlackTree,
  GLSLog;

const
  GLVBOM_MAX_DIFFERENT_PRIMITIVES = 8;
  GLVBOM_STATIC_POOL = 32 * 1024 * 1024; // Used when no memory info avaible

type

  TGLVBOMState = (GLVBOM_DEFAULT, GLVBOM_OBJECT, GLVBOM_PRIMITIVE);

  TGLVBOMEnum = (
    GLVBOM_NOPRIMITIVE,
    GLVBOM_TRIANGLES,
    GLVBOM_TRIANGLE_STRIP,
    GLVBOM_TRIANGLE_FAN,
    GLVBOM_QUADS, // OpenGL3x deprecated !!
    GLVBOM_QUAD_STRIP, // OpenGL3x deprecated !!
    GLVBOM_POINTS,
    GLVBOM_LINES,
    GLVBOM_LINE_LOOP,
    GLVBOM_LINE_STRIP,
    GLVBOM_POLYGON, // OpenGL3x deprecated !!
    GLVBOM_LINES_ADJACENCY,
    GLVBOM_LINE_STRIP_ADJACENCY,
    GLVBOM_TRIANGLES_ADJACENCY,
    GLVBOM_TRIANGLE_STRIP_ADJACENCY,
    GLVBOM_PATCHES
    );

  TGLBufferUsage = (buStatic, buDynamic, buStream);

  // Generics classes
  //
  TGLVAOHandleTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < TGLProgramHandle, TGLVertexArrayHandle > ;

  TGLBaseVBOManager = class;
  TGLBaseVBOManagerClass = class of TGLBaseVBOManager;
  TGLBuiltProperties = class;
  // Information about object graphic data (something like DIP)
  PGLRenderPacket = ^TGLRenderPacket;
  TGLRenderPacket = record
    StateHandle: TGLVAOHandleTree;
    ArrayHandle: TGLVBOArrayBufferHandle;
    ElementHandle: TGLVBOElementArrayHandle;

    ArrayBufferOffset: PtrUInt;
    ElementBufferOffset: PtrUInt;
    IndexType: TGLenum; // UByte, UShort or UInt
    RestartIndex: TGLuint;
    VertexCount: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    IndexCount: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    PrimitiveType: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of TGLVBOMEnum;

    Attributes: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLAttribute;
    Divisor: array[0..GLS_VERTEX_ATTR_NUM - 1] of GLuint;
    DataFormat: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLDataType;
    DataSize: array[0..GLS_VERTEX_ATTR_NUM - 1] of LongWord;
    VerticesInPatch: Cardinal;
    BuiltProp: TGLBuiltProperties;
    LastTimeWhenRendered: Double;
    RelativeSize: Single;
    DataSizeInArrayPool: Cardinal;
    DataSizeInElementPool: Cardinal;
  end;

  TBuildRequestEvent = procedure(Sender: TGLBaseVBOManager)
    of object;

  TGLBuiltProperties = class(TPersistent)
  private
    { Private declarations }
    Owner: TComponent;
    ID: Longword;
    FUsage: TGLBufferUsage;
    FVertexWelding: Boolean;
    FTriangleAdjacency: Boolean;
    FInstancesNumber: Integer;
    FStructureChanged: Boolean;
    FOnBuildRequest: TBuildRequestEvent;
    FOwnerNotifyChange: TNotifyEvent;
    procedure SetUsage(const Value: TGLBufferUsage);
    procedure SetVertexWelding(const Value: Boolean);
    procedure SetTriangleAdjacency(const Value: Boolean);
    procedure SetInstancesNumber(Value: Integer);
    function GetVertexBufferHandle: GLUInt;
    function GetIndexBufferHandle: GLUInt;
    function GetVertexNumber: LongWord;
    function GetManager: TGLBaseVBOManager;
  protected
    { Protected Declarations }
    VertexBufferCapacity: LongWord;
    IndexBufferCapacity: LongWord;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    procedure StructureChanged;
    property VertexBufferHandle: GLUInt read GetVertexBufferHandle;
    property IndexBufferHandle: GLUInt read GetIndexBufferHandle;

    property Manager: TGLBaseVBOManager read GetManager;
    property OwnerNotifyChange: TNotifyEvent read FOwnerNotifyChange
      write FOwnerNotifyChange;
    property OnBuildRequest: TBuildRequestEvent read FOnBuildRequest write
      FOnBuildRequest;
    property VertexNumber: LongWord read GetVertexNumber;
  published
    { Published Declarations }
    property Usage: TGLBufferUsage read fUsage write SetUsage default buStatic;
    property VertexWelding: Boolean read fVertexWelding write SetVertexWelding
      default True;
    property TriangleAdjacency: Boolean read fTriangleAdjacency write
      SetTriangleAdjacency default False;
    property InstancesNumber: Integer read fInstancesNumber write
      SetInstancesNumber default 0;
  end;

  // TGLBaseVBOManager
  //
  TGLBaseVBOManager = class
  private
    { Private declarations }
    FArrayHandle: TGLVBOArrayBufferHandle;
    FElementHandle: TGLVBOElementArrayHandle;
    FClientList: TList;
    CurrentClient: PGLRenderPacket;
    FState: TGLVBOMState;
{$IFDEF GLS_MULTITHREAD}
    FLock: TRTLCriticalSection;
{$ENDIF}
    FAttributeArrays: array[0..GLS_VERTEX_ATTR_NUM - 1] of T4ByteList;
    FObjectVertexCount: Integer; // From BeginObject to EndObject
    FHostIndexBuffer: TLongWordList;
    FObjectIndexCount: LongWord;
    FDoubling: Boolean;
    FPrimitiveTypeCount: Integer;
    FWasUsedList: Boolean;
  protected
    { Protected declarations }
    function GetAttributeIndex(Attrib: TGLSLAttribute;
      eType: TGLSLDataType): GLint;
    procedure ResetAttribArray(idx: integer);
    procedure InitClient(var AClient: PGLRenderPacket);
    procedure FreeClient(const AClient: PGLRenderPacket);
    procedure BindVertexArray(out hVAO: TGLVertexArrayHandle);
    procedure RenderCurrentClient;
    function DoMakeAdjacency: Boolean;
    procedure BuildErrorModel(const BuiltProp: TGLBuiltProperties);
    class function Usage: TGLEnum; virtual; abstract;
    function GetVecticesInPatch: Cardinal;
    procedure SetVecticesInPatch(Value: Cardinal);
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeginWork; inline;
    procedure EndWork; inline;

    {: Begins storing a piece of geometry }
    procedure BeginObject(const BuiltProp: TGLBuiltProperties);
      virtual;
    {: Ends a piece of geometry. }
    procedure EndObject;
      virtual;
    {: Begins gathering information about the given type of primitives.
       An object can only consist of a set of primitives of the same type. }
    procedure BeginPrimitives(eType: TGLVBOMEnum);
      virtual;
    {: Ends gathering information about the primitive. }
    procedure EndPrimitives;
      virtual;
    {: Specifies a new value for the attribute with the given name. }
    procedure Attribute1f(Attrib: TGLSLAttribute; a1: GLfloat);
    procedure Attribute2f(Attrib: TGLSLAttribute; a1, a2: GLfloat);
      overload;
    procedure Attribute2f(Attrib: TGLSLAttribute; const a: TVector2f);
      overload;
    procedure Attribute3f(Attrib: TGLSLAttribute; a1, a2, a3: GLfloat);
      overload;
    procedure Attribute3f(Attrib: TGLSLAttribute; const a: TVector3f);
      overload;
    procedure Attribute4f(Attrib: TGLSLAttribute;
      a1, a2, a3, a4: GLfloat);
      overload;
    procedure Attribute4f(Attrib: TGLSLAttribute; const a: TVector4f);
      overload;
    procedure Attribute1i(Attrib: TGLSLAttribute; a1: GLint);
    procedure Attribute2i(Attrib: TGLSLAttribute; a1, a2: GLint);
      overload;
    procedure Attribute2i(Attrib: TGLSLAttribute; const a: TVector2i);
      overload;
    procedure Attribute3i(Attrib: TGLSLAttribute; a1, a2, a3: GLint);
      overload;
    procedure Attribute3i(Attrib: TGLSLAttribute; const a: TVector3i);
      overload;
    procedure Attribute4i(Attrib: TGLSLAttribute;
      a1, a2, a3, a4: GLint); overload;
    procedure Attribute4i(Attrib: TGLSLAttribute; const a: TVector4i);
      overload;
    procedure Attribute1ui(Attrib: TGLSLAttribute; a1: GLuint);
    procedure Attribute2ui(Attrib: TGLSLAttribute; a1, a2: GLuint);
      overload;
    procedure Attribute2ui(Attrib: TGLSLAttribute; const a: TVector2ui);
      overload;
    procedure Attribute3ui(Attrib: TGLSLAttribute; a1, a2, a3: GLuint);
      overload;
    procedure Attribute3ui(Attrib: TGLSLAttribute; const a: TVector3ui);
      overload;
    procedure Attribute4ui(Attrib: TGLSLAttribute;
      a1, a2, a3, a4: GLuint); overload;
    procedure Attribute4ui(Attrib: TGLSLAttribute; const a: TVector4ui);
      overload;
    {: Specifies a attrubute divisor for instanced drawing. }
    procedure AttributeDivisor(Attrib: TGLSLAttribute; Value: GLuint);
    {: Takes a full list of attribute values,
       but does not determine its type, so you must use AttributeXX
       between BeginObject and BeginPrimitives }
    procedure AttributeList(Attrib: TGLSLAttribute; const AList:
      TSingleList);
      overload;
    procedure AttributeList(Attrib: TGLSLAttribute; const AList:
      TIntegerList);
      overload;
    {: Specifies a new vertex of a primitive. }
    procedure EmitVertex;
    {: Provides a feature to create an empty buffer
       from the task markup attributes.
       This is useful for transformfeedback or copying operations. }
    procedure EmitVertices(VertexNumber: LongWord; Indexed: Boolean);
      dynamic;
    {: Restart strip by GL_NV_primitive_restart or degenerate primitive }
    procedure RestartStrip;
    {: If during the storing geometry popup an error
       use discard to remove last stored object's data }
    procedure Discard;
    {: Store time }
    procedure DoProgress(const progressTime: TProgressTimes);
      dynamic;
    {: Rendering sender }
    procedure RenderClient(const BuiltProp: TGLBuiltProperties);
      dynamic;
    {: Notify about program is relinked and need to redefine VAO states }
    procedure NotifyProgramChanged(const AProg: TGLProgramHandle);

    property VecticesInPatch: Cardinal read GetVecticesInPatch write SetVecticesInPatch;
  end;

  // TGLStaticVBOManager
  //
  TGLStaticVBOManager = class(TGLBaseVBOManager)
  private
    { Private declarations }
    FArrayBufferSize: TGLuint;
    FElementBufferSize: TGLuint;
    FArrayBufferFreeZone: TGLuint;
    FElementBufferFreeZone: TGLuint;
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties);
      override;
    procedure EndObject; override;
    procedure AllocateBuffers;
    procedure RenderClient(const BuiltProp: TGLBuiltProperties); override;
    {: Returns the portion of the buffer that is used during the time interval. }
    function UsageStatistic(const TimeInterval: Double): Single;
    class function Usage: TGLenum; override;
  end;

  // TGLDynamicVBOManager
  //
  TGLDynamicVBOManager = class(TGLBaseVBOManager)
  private
    { Private declarations }
    VertexBufferCapacity: LongWord;
    IndexBufferCapacity: LongWord;
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties); override;
    procedure EndObject; override;
    procedure RenderClient(const BuiltProp: TGLBuiltProperties); override;
    class function Usage: TGLenum; override;
  end;

  // TGLStreamVBOManager
  //
  TGLStreamVBOManager = class(TGLBaseVBOManager)
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties); override;
    procedure EndObject; override;
    procedure EmitVertices(VertexNumber: LongWord; Indexed: Boolean); override;
    procedure RenderClient(const BuiltProp: TGLBuiltProperties); override;
    class function Usage: TGLenum; override;
  end;

var
  vUseMappingForOftenBufferUpdate: Boolean = False;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ENDIF}
  vCurrentTime: Double;
  vCurrentAttribValue: array[0..GLS_VERTEX_ATTR_NUM - 1, 0..15] of T4ByteData;

function StaticVBOManager: TGLStaticVBOManager;
function DynamicVBOManager: TGLDynamicVBOManager;
function StreamVBOManager: TGLStreamVBOManager;

implementation

uses
  VectorGeometry,
  GLUtils,
  GLStrings,
  MeshUtils;

const
  cPrimitiveType: array[GLVBOM_TRIANGLES..GLVBOM_PATCHES] of
    GLenum =
    (
    GL_TRIANGLES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_QUADS,
    GL_QUAD_STRIP,
    GL_POINTS,
    GL_LINES,
    GL_LINE_LOOP,
    GL_LINE_STRIP,
    GL_POLYGON,
    GL_LINES_ADJACENCY,
    GL_LINE_STRIP_ADJACENCY,
    GL_TRIANGLES_ADJACENCY,
    GL_TRIANGLE_STRIP_ADJACENCY,
    GL_PATCHES
    );

type
  TVBOProgramGetter = class(TCurrentProgramGetter);

procedure ArrayHandleDestroyer(
  k: TGLProgramHandle;
  AHandle: TGLVertexArrayHandle;
  out Flag: Boolean);
begin
  AHandle.Destroy;
  Flag := True;
end;

procedure ArrayHandleNotifyChange(
  k: TGLProgramHandle;
  AHandle: TGLVertexArrayHandle;
  out Flag: Boolean);
begin
  AHandle.NotifyChangesOfData;
  Flag := True;
end;

var
  vMaxElementsIndices: GLUInt;
  vStaticVBOManager: TGLStaticVBOManager;
  vDynamicVBOManager: TGLDynamicVBOManager;
  vStreamVBOManager: TGLStreamVBOManager;

function StaticVBOManager: TGLStaticVBOManager;
begin
  if vStaticVBOManager = nil then
    vStaticVBOManager := TGLStaticVBOManager.Create;
  Result := vStaticVBOManager;
end;

function DynamicVBOManager: TGLDynamicVBOManager;
begin
  if vDynamicVBOManager = nil then
    vDynamicVBOManager := TGLDynamicVBOManager.Create;
  Result := vDynamicVBOManager;
end;

function StreamVBOManager: TGLStreamVBOManager;
begin
  if vStreamVBOManager = nil then
    vStreamVBOManager := TGLStreamVBOManager.Create;
  Result := vStreamVBOManager;
end;

procedure FreeVBOManagers;
begin
  vStaticVBOManager.Free;
  vDynamicVBOManager.Free;
  vStreamVBOManager.Free;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLBuiltProperties'}{$ENDIF}
// ------------------
// ------------------ TGLBuiltProperties ------------------
// ------------------

constructor TGLBuiltProperties.Create(AOwner: TComponent);
begin
  inherited Create;
  Owner := AOwner;
  ID := 0;
  FUsage := buStatic;
  FVertexWelding := True;
  FTriangleAdjacency := False;
  FInstancesNumber := 0;
  FStructureChanged := True;
end;

procedure TGLBuiltProperties.SetUsage(const Value: TGLBufferUsage);
begin
  if Value <> fUsage then
  begin
    fUsage := Value;
    StructureChanged;
  end;
end;

procedure TGLBuiltProperties.StructureChanged;
begin
  if Assigned(FOwnerNotifyChange) and not (csLoading in Owner.ComponentState) then
    FOwnerNotifyChange(Self);
  FStructureChanged := True;
end;

function TGLBuiltProperties.GetManager: TGLBaseVBOManager;
begin
  if (FUsage = buStatic) and (csDesigning in Owner.ComponentState) then
    Result := DynamicVBOManager
  else
    case FUsage of
      buStatic: Result := StaticVBOManager;
      buDynamic: Result := DynamicVBOManager;
      buStream: Result := StreamVBOManager;
    else
      begin
        Assert(False, glsErrorEx + glsUnknownType);
        Result := DynamicVBOManager;
      end;
    end;
end;

procedure TGLBuiltProperties.SetVertexWelding(const Value: Boolean);
begin
  if Value <> fVertexWelding then
  begin
    FVertexWelding := Value;
    StructureChanged;
  end;
end;

procedure TGLBuiltProperties.SetTriangleAdjacency(const Value: Boolean);
begin
  if Value <> fTriangleAdjacency then
  begin
    FTriangleAdjacency := Value;
    StructureChanged;
  end;
end;

procedure TGLBuiltProperties.SetInstancesNumber(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fInstancesNumber then
  begin
    FInstancesNumber := Value;
    StructureChanged;
  end;
end;

function TGLBuiltProperties.GetVertexBufferHandle: GLUInt;
var
  Client: PGLRenderPacket;
begin
  Result := 0;
  case fUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TGLStaticVBOManager(Manager).FClientList.Items[ID - 1];
        if Assigned(Client.ArrayHandle) then
          Result := Client.ArrayHandle.Handle;
      end;
    buDynamic:
      Result := TGLDynamicVBOManager(Manager).FArrayHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).FClientList.Items[ID - 1];
        if Assigned(Client.ArrayHandle) then
          Result := Client.ArrayHandle.Handle;
      end;
  end;
end;

function TGLBuiltProperties.GetIndexBufferHandle: GLUInt;
var
  Client: PGLRenderPacket;
begin
  Result := 0;
  case fUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TGLStaticVBOManager(Manager).FClientList.Items[ID - 1];
        if Assigned(Client.ElementHandle) then
          Result := Client.ElementHandle.Handle;
      end;
    buDynamic:
      Result := TGLDynamicVBOManager(Manager).FElementHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).FClientList.Items[ID - 1];
        if Assigned(Client.ElementHandle) then
          Result := Client.ElementHandle.Handle;
      end;
  end;
end;

function TGLBuiltProperties.GetVertexNumber: LongWord;
var
  Client: PGLRenderPacket;
  p: Integer;
begin
  Result := 0;
  case fUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TGLStaticVBOManager(Manager).FClientList.Items[ID - 1];
        for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
          Inc(Result, Client.VertexCount[p]);
      end;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).FClientList.Items[ID - 1];
        for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
          Inc(Result, Client.VertexCount[p]);
      end;
  end;
end;

procedure TGLBuiltProperties.Assign(Source: TPersistent);
begin
  if Source is TGLBuiltProperties then
  begin
    Usage := TGLBuiltProperties(Source).fUsage;
    VertexWelding := TGLBuiltProperties(Source).fVertexWelding;
    TriangleAdjacency := TGLBuiltProperties(Source).fTriangleAdjacency;
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLBaseVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLBaseVBOManager ------------------
// ------------------

constructor TGLBaseVBOManager.Create;
var
  i: Integer;
begin
  FArrayHandle := TGLVBOArrayBufferHandle.Create;
  FElementHandle := TGLVBOElementArrayHandle.Create;
  FHostIndexBuffer := TLongWordList.Create;
  FHostIndexBuffer.SetCountResetsMemory := false;
  FState := GLVBOM_DEFAULT;
  FWasUsedList := False;

  for i := 0 to High(FAttributeArrays) do
    FAttributeArrays[i] := T4ByteList.Create;
{$IFDEF GLS_MULTITHREAD}
  InitializeCriticalSection(FLock);
{$ENDIF}
end;

destructor TGLBaseVBOManager.Destroy;
var
  i: Integer;
begin
  FArrayHandle.Destroy;
  FElementHandle.Destroy;
  for i := 0 to High(FAttributeArrays) do
    FAttributeArrays[i].Destroy;
  FHostIndexBuffer.Destroy;
{$IFDEF GLS_MULTITHREAD}
  DeleteCriticalSection(FLock);
{$ENDIF}
end;

procedure TGLBaseVBOManager.BeginWork;
begin
{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TGLBaseVBOManager.EndWork;
begin
{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

function TGLBaseVBOManager.GetVecticesInPatch: Cardinal;
begin
  if (FState = GLVBOM_OBJECT) or (FState = GLVBOM_PRIMITIVE) then
    Result := CurrentClient.VerticesInPatch
  else
    Result := 0;
end;

procedure TGLBaseVBOManager.SetVecticesInPatch(Value: Cardinal);
begin
  Assert(Value > 0);
  if FState = GLVBOM_OBJECT then
    CurrentClient.VerticesInPatch := Value
  else
  begin
    GLSLogger.LogError(glsWrongCallEnd);
    Abort;
  end;
end;

procedure TGLBaseVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
begin
  if FState <> GLVBOM_DEFAULT then
  begin
    GLSLogger.LogError(glsWrongCallBegin);
    Abort;
  end;
end;

procedure TGLBaseVBOManager.EndObject;
begin
end;

procedure TGLBaseVBOManager.Discard;
var
  i, p: integer;
begin
  FState := GLVBOM_DEFAULT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(i);

  if Assigned(CurrentClient) then
    for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
      for i := 0 to Integer(CurrentClient.IndexCount[p]) - 1 do
        FHostIndexBuffer.Pop;

  if Self is TGLStaticVBOManager then
  begin
    FreeClient(CurrentClient);
    Dispose(CurrentClient);
  end;
  CurrentClient := nil;
end;

procedure TGLBaseVBOManager.ResetAttribArray(idx: integer);
begin
  if idx >= GLS_VERTEX_ATTR_NUM then
  begin
    GLSLogger.LogError('ResetAttribArray: Array index out of bound.');
    Abort;
  end;
  FAttributeArrays[idx].Flush;
end;

procedure TGLBaseVBOManager.InitClient(var AClient: PGLRenderPacket);
var
  I: Integer;
begin
  if not Assigned(AClient) then
  begin
    New(AClient);
    FillChar(AClient^, SizeOf(TGLRenderPacket), 0);
    if Self is TGLStreamVBOManager then
    begin
      AClient.ArrayHandle := TGLVBOArrayBufferHandle.Create;
      AClient.ElementHandle := TGLVBOElementArrayHandle.Create;
    end;
    AClient.StateHandle := TGLVAOHandleTree.Create(CompareProgram, nil);
  end
  else
  begin
    AClient.StateHandle.ForEach(ArrayHandleNotifyChange);

    for I := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    begin
      AClient.PrimitiveType[I] := GLVBOM_NOPRIMITIVE;
      AClient.IndexCount[I] := 0;
      AClient.VertexCount[I] := 0;
    end;
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    begin
      AClient.Attributes[I] := nil;
      AClient.Divisor[I] := 0;
      AClient.DataSize[I] := 0;
      AClient.DataFormat[I] := GLSLTypeUndefined;
    end;
  end;
  AClient.IndexType := GL_UNSIGNED_INT;
  AClient.RestartIndex := $FFFFFFFF;
  AClient.VerticesInPatch := 1;
end;

procedure TGLBaseVBOManager.FreeClient(const AClient: PGLRenderPacket);
begin
  if Assigned(AClient) then
  begin
    AClient.StateHandle.ForEach(ArrayHandleDestroyer);
    AClient.StateHandle.Free;
    if Self is TGLStreamVBOManager then
    begin
      FreeAndNil(AClient.ArrayHandle);
      FreeAndNil(AClient.ElementHandle);
    end;
  end;
end;

procedure TGLBaseVBOManager.BeginPrimitives(eType: TGLVBOMEnum);
var
  lastState: TGLVBOMState;
begin
  lastState := FState;
  FState := GLVBOM_PRIMITIVE;
  if lastState <> GLVBOM_OBJECT then
  begin
    GLSLogger.LogError(glsWrongCallBeginPrim);
    Abort;
  end;

  if not ((eType >= GLVBOM_TRIANGLES) and (eType <=
    GLVBOM_PATCHES)) then
  begin
    GLSLogger.LogError(glsInvalidPrimType);
    Abort;
  end;

  if CurrentClient.PrimitiveType[FPrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(FPrimitiveTypeCount);
    if FPrimitiveTypeCount >= GLVBOM_MAX_DIFFERENT_PRIMITIVES then
    begin
      GLSLogger.LogError(glsTooMachDiffPrim);
      Abort;
    end;
  end;

  CurrentClient.PrimitiveType[FPrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[FPrimitiveTypeCount] := 0;
end;

procedure TGLBaseVBOManager.EndPrimitives;
var
  lastState: TGLVBOMState;
  Valid: Boolean;
  count: LongWord;
begin
  lastState := FState;
  FState := GLVBOM_OBJECT;
  if lastState <> GLVBOM_PRIMITIVE then
  begin
    GLSLogger.LogError(glsWrongCallEndPrim);
    Abort;
  end;

  if FDoubling then
  begin
    FHostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[FPrimitiveTypeCount]);
    FDoubling := false;
  end;

  Valid := false;
  count := CurrentClient.VertexCount[FPrimitiveTypeCount];
  case CurrentClient.PrimitiveType[FPrimitiveTypeCount] of
    GLVBOM_TRIANGLES: Valid := (count mod 3 = 0) and
      (count > 2);
    GLVBOM_TRIANGLE_STRIP,
      GLVBOM_TRIANGLE_FAN: Valid := count > 2;
    GLVBOM_QUADS: Valid := (count mod 4 = 0) and (count > 3);
    GLVBOM_QUAD_STRIP: Valid := count > 3;
    GLVBOM_POINTS: Valid := count > 0;
    GLVBOM_LINES: Valid := (count mod 2 = 0) and (count > 1);
    GLVBOM_LINE_STRIP,
      GLVBOM_LINE_LOOP: Valid := count > 2;
    GLVBOM_POLYGON: Valid := count > 2;
    GLVBOM_LINES_ADJACENCY: Valid := (count mod 4 = 0) and (count > 3);
    GLVBOM_LINE_STRIP_ADJACENCY: Valid := count > 4;
    GLVBOM_TRIANGLES_ADJACENCY: Valid := (count mod 6 = 0) and (count > 5);
    GLVBOM_TRIANGLE_STRIP_ADJACENCY: Valid := count > 4;
    GLVBOM_PATCHES: Valid := (count mod CurrentClient.VerticesInPatch = 0) and (count > 0);
  end;

  if not Valid then
  begin
    GLSLogger.LogError(glsInvalidNumberOfVertex);
    Abort;
  end;

  // Make trinagles with adjancency
  if Assigned(CurrentClient.BuiltProp)
    and CurrentClient.BuiltProp.TriangleAdjacency then
    if not DoMakeAdjacency then
    begin
      GLSLogger.LogError(glsDoMakeAdjFail);
      Abort;
    end;
end;

procedure TGLBaseVBOManager.EmitVertex;
var
  a, v, I, etalon, count, dsize: integer;
  AA: T4ByteList;
  weld: Boolean;
begin
  if FState <> GLVBOM_PRIMITIVE then
  begin
    GLSLogger.LogError(glsWrongCallEmit);
    Abort;
  end;

  if FWasUsedList then
  begin
    // Emit List of Vertex
    etalon := -1;
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if Assigned(CurrentClient.Attributes[a])
        and (CurrentClient.Divisor[a] = 0) then
      begin
        AA := FAttributeArrays[a];
        dsize := GLSLTypeComponentCount(CurrentClient.DataFormat[a]);
        count := AA.Count div dsize;
        if etalon < 0 then
        begin
          etalon := count;
          continue;
        end
        else
          Assert(etalon = count,
            'EmitVertex: Lists of attributes do not match the length');
      end;
    {: Indexing verteces not complited }
    //    for i := 0 to etalon - 1 do
    //    begin
    //      HostIndexBuffer.Push(ObjectIndex);
    //      Inc(ObjectIndex);
    //    end;
    //
    //    Inc(CurrentClient.IndexCount[PrimitiveTypeCount], etalon);
    Inc(CurrentClient.VertexCount[FPrimitiveTypeCount], etalon);
    Inc(FObjectVertexCount, etalon);
    FWasUsedList := False;
  end
    // Emit single vertex
  else
  begin
    I := -1;
    weld := false;
    if Assigned(CurrentClient.BuiltProp)
      and CurrentClient.BuiltProp.VertexWelding then
    begin
      for v := 0 to FObjectVertexCount - 1 do
      begin
        weld := true;
        for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
          if Assigned(CurrentClient.Attributes[a])
            and (CurrentClient.Divisor[a] = 0) then
          begin
            AA := FAttributeArrays[a];
            dsize := GLSLTypeComponentCount(CurrentClient.DataFormat[a]);
            if not CompareMem(
              @AA.List[v * dsize],
              @vCurrentAttribValue[a][0],
              dsize * SizeOf(T4ByteData)) then
            begin
              weld := false;
              break;
            end;
          end; // of for a

        if weld then
        begin
          I := v;
          break;
        end;
      end; // of for v
    end;

    if not weld then
    begin
      for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
        if Assigned(CurrentClient.Attributes[a])
          and (CurrentClient.Divisor[a] = 0) then
        begin
          AA := FAttributeArrays[a];
          dsize := GLSLTypeComponentCount(CurrentClient.DataFormat[a]);
          for v := 0 to dsize - 1 do
            AA.Push(vCurrentAttribValue[a][v]);
          I := 0;
        end;

      if I > -1 then
      begin
        FHostIndexBuffer.Push(FObjectIndexCount);
        Inc(CurrentClient.IndexCount[FPrimitiveTypeCount]);
        Inc(CurrentClient.VertexCount[FPrimitiveTypeCount]);
        if FDoubling then
        begin
          FHostIndexBuffer.Push(FObjectIndexCount);
          Inc(CurrentClient.IndexCount[FPrimitiveTypeCount]);
          FDoubling := false;
        end;
        Inc(FObjectIndexCount);
        Inc(FObjectVertexCount);
      end;
    end
    else if I > -1 then
    begin
      // Push this offer index of repetitive vertex
      FHostIndexBuffer.Push(I);
      Inc(CurrentClient.IndexCount[FPrimitiveTypeCount]);
      Inc(CurrentClient.VertexCount[FPrimitiveTypeCount]);
      if FDoubling then
      begin
        FHostIndexBuffer.Push(I);
        Inc(CurrentClient.IndexCount[FPrimitiveTypeCount]);
        FDoubling := false;
      end;
    end;
  end;
end;

procedure TGLBaseVBOManager.RestartStrip;
begin
  if FWasUsedList then
    exit;
  if FState <> GLVBOM_PRIMITIVE then
  begin
    GLSLogger.LogError('RestartStrip must be called between Begin / End.');
    Abort;
  end;
  if FDoubling then
  begin
    GLSLogger.LogError('RestartStrip: Excessive call.');
    Abort;
  end;
  if not (CurrentClient.PrimitiveType[FPrimitiveTypeCount] in
    [GLVBOM_TRIANGLE_STRIP, GLVBOM_TRIANGLE_FAN,
    GLVBOM_QUAD_STRIP, GLVBOM_LINE_STRIP, GLVBOM_PATCHES]) then
  begin
    GLSLogger.LogError('RestartStrip: This primitive type does not need to restart.');
    Abort;
  end;

  if GL.NV_primitive_restart then
    FHostIndexBuffer.Push($FFFFFFFF)
  else
  begin
    // Create degenerate primitive
    FHostIndexBuffer.Push(FObjectIndexCount - 1);
    FDoubling := true;
  end;
  Inc(CurrentClient.IndexCount[FPrimitiveTypeCount]);
end;

function TGLBaseVBOManager.GetAttributeIndex(
  Attrib: TGLSLAttribute; eType: TGLSLDataType): GLint;
var
  I: Integer;
begin
  Result := -1;
  if FWasUsedList and (eType <> GLSLTypeVoid) then
  begin
    GLSLogger.LogError(glsBadAttrCombination);
    Abort;
  end;

  if (FState = GLVBOM_OBJECT) and (eType <> GLSLTypeVoid) then
  begin
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if CurrentClient.Attributes[I] = Attrib then
      begin
        GLSLogger.LogError('Excessive attribute declaration.');
        Abort;
      end;
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if not Assigned(CurrentClient.Attributes[I]) then
      begin
        // Record new attribute in uses set
        CurrentClient.Attributes[I] := Attrib;
        CurrentClient.DataFormat[I] := eType;
        Result := I;
        exit;
      end;
    GLSLogger.LogError(glsOutOfMaxAttrib);
  end

  else
  begin
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if CurrentClient.Attributes[I] = Attrib then
      begin
        // Check attribute type
        if (eType <> GLSLTypeVoid) and (CurrentClient.DataFormat[I] <> eType) then
          GLSLogger.LogError(glsWrongAttrType);
        Result := I;
        exit;
      end;
    GLSLogger.LogError(glsUnknownAttrib);
  end;
end;

procedure TGLBaseVBOManager.Attribute1f(Attrib: TGLSLAttribute; a1:
  GLfloat);
var
  loc: Integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1);
    end
    else
      vCurrentAttribValue[loc, 0].Float.Value := a1;
  end;
end;

procedure TGLBaseVBOManager.Attribute2f(Attrib: TGLSLAttribute; a1, a2:
  GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a1;
      vCurrentAttribValue[loc, 1].Float.Value := a2;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute2f(Attrib: TGLSLAttribute;
  const a: TVector2f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a[0];
      vCurrentAttribValue[loc, 1].Float.Value := a[1];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3f(Attrib: TGLSLAttribute;
  a1, a2, a3: GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a1;
      vCurrentAttribValue[loc, 1].Float.Value := a2;
      vCurrentAttribValue[loc, 2].Float.Value := a3;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3f(Attrib: TGLSLAttribute;
  const a: TVector3f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a[0];
      vCurrentAttribValue[loc, 1].Float.Value := a[1];
      vCurrentAttribValue[loc, 2].Float.Value := a[2];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4f(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLfloat);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a1;
      vCurrentAttribValue[loc, 1].Float.Value := a2;
      vCurrentAttribValue[loc, 2].Float.Value := a3;
      vCurrentAttribValue[loc, 3].Float.Value := a4;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4f(Attrib: TGLSLAttribute;
  const a: TVector4f);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4F);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Float.Value := a[0];
      vCurrentAttribValue[loc, 1].Float.Value := a[1];
      vCurrentAttribValue[loc, 2].Float.Value := a[2];
      vCurrentAttribValue[loc, 3].Float.Value := a[3];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute1i(Attrib: TGLSLAttribute;
  a1: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a1;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute2i(Attrib: TGLSLAttribute; a1, a2:
  GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a1;
      vCurrentAttribValue[loc, 1].Int.Value := a2;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute2i(Attrib: TGLSLAttribute;
  const a: TVector2i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a[0];
      vCurrentAttribValue[loc, 1].Int.Value := a[1];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3i(Attrib: TGLSLAttribute;
  a1, a2, a3: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a1;
      vCurrentAttribValue[loc, 1].Int.Value := a2;
      vCurrentAttribValue[loc, 2].Int.Value := a3;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3i(Attrib: TGLSLAttribute;
  const a: TVector3i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a[0];
      vCurrentAttribValue[loc, 1].Int.Value := a[1];
      vCurrentAttribValue[loc, 2].Int.Value := a[2];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4i(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a1;
      vCurrentAttribValue[loc, 1].Int.Value := a2;
      vCurrentAttribValue[loc, 2].Int.Value := a3;
      vCurrentAttribValue[loc, 3].Int.Value := a4;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4i(Attrib: TGLSLAttribute;
  const a: TVector4i);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4I);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].Int.Value := a[0];
      vCurrentAttribValue[loc, 1].Int.Value := a[1];
      vCurrentAttribValue[loc, 2].Int.Value := a[2];
      vCurrentAttribValue[loc, 3].Int.Value := a[3];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute1ui(Attrib: TGLSLAttribute;
  a1: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType1UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a1;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute2ui(Attrib: TGLSLAttribute; a1, a2:
  GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a1;
      vCurrentAttribValue[loc, 1].UInt.Value := a2;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute2ui(Attrib: TGLSLAttribute;
  const a: TVector2ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType2UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a[0];
      vCurrentAttribValue[loc, 1].UInt.Value := a[1];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3ui(Attrib: TGLSLAttribute;
  a1, a2, a3: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a1;
      vCurrentAttribValue[loc, 1].UInt.Value := a2;
      vCurrentAttribValue[loc, 2].UInt.Value := a3;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute3ui(Attrib: TGLSLAttribute;
  const a: TVector3ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType3UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a[0];
      vCurrentAttribValue[loc, 1].UInt.Value := a[1];
      vCurrentAttribValue[loc, 2].UInt.Value := a[2];
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4ui(Attrib: TGLSLAttribute;
  a1, a2, a3, a4: GLuint);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a1, a2, a3, a4);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a1;
      vCurrentAttribValue[loc, 1].UInt.Value := a2;
      vCurrentAttribValue[loc, 2].UInt.Value := a3;
      vCurrentAttribValue[loc, 3].UInt.Value := a4;
    end;
  end;
end;

procedure TGLBaseVBOManager.Attribute4ui(Attrib: TGLSLAttribute;
  const a: TVector4ui);
var
  loc: integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLType4UI);
  if loc > -1 then
  begin
    if CurrentClient.Divisor[loc] > 0 then
    begin
      FAttributeArrays[loc].Add(a[0], a[1], a[2], a[3]);
    end
    else
    begin
      vCurrentAttribValue[loc, 0].UInt.Value := a[0];
      vCurrentAttribValue[loc, 1].UInt.Value := a[1];
      vCurrentAttribValue[loc, 2].UInt.Value := a[2];
      vCurrentAttribValue[loc, 3].UInt.Value := a[3];
    end;
  end;
end;

procedure TGLBaseVBOManager.AttributeDivisor(Attrib: TGLSLAttribute; Value: GLuint);
var
  loc: integer;
begin
  if FState <> GLVBOM_OBJECT then
  begin
    GLSLogger.LogError(glsWrongCallEnd);
    Abort;
  end;

  loc := GetAttributeIndex(Attrib, GLSLTypeVoid);
  if loc > -1 then
    CurrentClient.Divisor[loc] := Value
  else
  begin
    GLSLogger.LogError(glsUnknownAttrib);
    Abort;
  end;
end;

procedure TGLBaseVBOManager.AttributeList(Attrib: TGLSLAttribute; const
  AList: TSingleList);
var
  loc: Integer;
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLTypeVoid);
  if loc = -1 then
    exit;

  Valid := false;
  case Attrib.DataFormat of
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

  AA := FAttributeArrays[loc];
  Last := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
  FWasUsedList := True;
end;

procedure TGLBaseVBOManager.AttributeList(Attrib: TGLSLAttribute; const
  AList: TIntegerList);
var
  loc: Integer;
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  loc := GetAttributeIndex(Attrib, GLSLTypeVoid);
  if loc = -1 then
    exit;

  Valid := false;
  case Attrib.DataFormat of
    GLSLType1I: Valid := true;
    GLSLType2I: Valid := (AList.Count mod 2 = 0);
    GLSLType3I: Valid := (AList.Count mod 3 = 0);
    GLSLType4I: Valid := (AList.Count mod 4 = 0);
  end;

  if not Valid then
  begin
    GLSLogger.LogWarning(glsWrongAttrType);
    Abort;
  end;

  AA := FAttributeArrays[loc];
  Last := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
  FWasUsedList := True;
end;

procedure TGLBaseVBOManager.EmitVertices(VertexNumber: LongWord; Indexed:
  Boolean);
begin
end;

procedure TGLBaseVBOManager.DoProgress(const progressTime: TProgressTimes);
begin
end;

procedure TGLBaseVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties);
begin
end;

procedure TGLBaseVBOManager.NotifyProgramChanged(const AProg: TGLProgramHandle);
var
  I: Integer;
  pClient: PGLRenderPacket;
  hVAO: TGLVertexArrayHandle;
begin
  if Assigned(FClientList) then
  begin
    for I := 0 to FClientList.Count - 1 do
    begin
      pClient := FClientList[I];
      if pClient.StateHandle.Find(AProg, hVAO) then
        hVAO.NotifyChangesOfData;
    end;
  end
  else if not CurrentClient.StateHandle.Find(AProg, hVAO) then
    hVAO.NotifyChangesOfData;
end;

procedure TGLBaseVBOManager.BindVertexArray(out hVAO: TGLVertexArrayHandle);
var
  Prog: TGLProgramHandle;
  I, L: Integer;
  Offset: PtrUInt;
  EnabledLocations: array[0..GLS_VERTEX_ATTR_NUM - 1] of Boolean;
begin
  Prog := TVBOProgramGetter.CurrentProgram;
  if Prog = nil then
  begin
    GLSLogger.LogError(glsNoShader);
    Abort;
  end;

  if not CurrentClient.StateHandle.Find(Prog, hVAO) then
  begin
    hVAO := TGLVertexArrayHandle.Create;
    CurrentClient.StateHandle.Add(Prog, hVAO);
  end;
  hVAO.AllocateHandle;

  if hVAO.IsDataNeedUpdate then
  begin
//    if Self is TGLStreamVBOManager then
//      sleep(0);
    // Uniting all the states and buffers in one vertex array object
    hVAO.Bind;

    // Need to direct bind array buffer for correctly VertexAttribPointer set up
    if CurrentGLcontext.GLStates.ArrayBufferBinding = CurrentClient.ArrayHandle.Handle then
      GL.BindBuffer(GL_ARRAY_BUFFER, CurrentClient.ArrayHandle.Handle)
    else
      CurrentClient.ArrayHandle.Bind;
    CurrentClient.ElementHandle.Bind; // Index handle can be zero

    Offset := CurrentClient.ArrayBufferOffset;
    // Predisable attributes
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      EnabledLocations[I] := False;

    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    begin
      if Assigned(CurrentClient.Attributes[I]) then
      begin
        L := CurrentClient.Attributes[I].Location;
        if (L > -1)
          and (CurrentClient.Attributes[I].DataFormat = CurrentClient.DataFormat[I]) then
        begin
          EnabledLocations[L] := True;
          GL.VertexAttribDivisor(L, CurrentClient.Divisor[I]);
          // Setup Client Attributes pointer
          case CurrentClient.DataFormat[I] of
            GLSLType1F:
              GL.VertexAttribPointer(L, 1, GL_FLOAT, false, 0, pointer(Offset));
            GLSLType2F:
              GL.VertexAttribPointer(L, 2, GL_FLOAT, false, 0, pointer(Offset));
            GLSLType3F:
              GL.VertexAttribPointer(L, 3, GL_FLOAT, false, 0, pointer(Offset));
            GLSLType4F:
              GL.VertexAttribPointer(L, 4, GL_FLOAT, false, 0, pointer(Offset));
            GLSLType1I:
              GL.VertexAttribIPointer(L, 1, GL_INT, 0, pointer(Offset));
            GLSLType2I:
              GL.VertexAttribIPointer(L, 2, GL_INT, 0, pointer(Offset));
            GLSLType3I:
              GL.VertexAttribIPointer(L, 3, GL_INT, 0, pointer(Offset));
            GLSLType4I:
              GL.VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, pointer(Offset));
            GLSLType1UI:
              GL.VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, pointer(Offset));
            GLSLType2UI:
              GL.VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, pointer(Offset));
            GLSLType3UI:
              GL.VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, pointer(Offset));
            GLSLType4UI:
              GL.VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, pointer(Offset));
          else
            Assert(False, glsErrorEx + glsUnknownType);
          end; // of case
        end;
        Offset := Offset + CurrentClient.DataSize[I];
      end;
    end;
    // Enable engagement attributes array
    with CurrentGLContext.GLStates do
    begin
      for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
        CurrentGLContext.GLStates.EnableVertexAttribArray[I] :=
          EnabledLocations[I];
    end;
    hVAO.NotifyDataUpdated;
  end
  else
    hVAO.Bind;

  with CurrentGLContext.GLStates do
  begin
    if Assigned(CurrentClient.BuiltProp) then
      EnablePrimitiveRestart := CurrentClient.BuiltProp.VertexWelding
    else
      EnablePrimitiveRestart := False;
    PrimitiveRestartIndex := CurrentClient.RestartIndex;
  end;
end;

procedure TGLBaseVBOManager.RenderCurrentClient;
var
  p, n, fullPartCount: Integer;
  pType: TGLEnum;
  restPart: LongWord;
  IndexStart, IndexFinish, VertexStart, typeSize: LongWord;
  Offset: PtrUInt;

  function IsPromitiveSupported: Boolean;
  begin
    if (CurrentClient.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY)
      and (CurrentClient.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
      Result := GL.EXT_gpu_shader4
    else if CurrentClient.PrimitiveType[p] = GLVBOM_PATCHES then
      Result := GL.ARB_tessellation_shader
    else
      Result := True;
  end;

  procedure HardwareInstancing;
  begin
    if CurrentClient.IndexCount[p] > 0 then
      GL.DrawElementsInstanced(
        pType,
        CurrentClient.IndexCount[p],
        CurrentClient.IndexType,
        Pointer(offset),
        CurrentClient.BuiltProp.InstancesNumber)
    else
      GL.DrawArraysInstanced(
        pType,
        VertexStart,
        CurrentClient.VertexCount[p],
        CurrentClient.BuiltProp.InstancesNumber);
  end;

  procedure PseudoInstancing;
  var
    uniform: GLInt;
    i: Integer;
  begin
    uniform := GL.GetUniformLocation(
      CurrentGLContext.GLStates.CurrentProgram,
      PGLChar(AnsiString(uniformInstanceID.Name)));
    if CurrentClient.IndexCount[p] > 0 then
    begin
      for i := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
      begin
        if uniform >= 0 then
          GL.Uniform1i(uniform, i);
        GL.DrawElements(
          pType,
          CurrentClient.IndexCount[p],
          CurrentClient.IndexType,
          Pointer(offset));
      end;
      if uniform >= 0 then
        GL.Uniform1i(uniform, 0);
    end
    else
    begin
      if uniform >= 0 then
        GL.Uniform1i(uniform, 0);
      for i := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
      begin
        if uniform >= 0 then
          GL.Uniform1i(uniform, i);
        GL.DrawArrays(
          pType,
          VertexStart,
          CurrentClient.VertexCount[p]);
      end;
      if uniform >= 0 then
        GL.Uniform1i(uniform, 0);
    end;
  end;

begin
  if vMaxElementsIndices = 0 then
    GL.Getintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  case CurrentClient.IndexType of
    GL_UNSIGNED_BYTE: typeSize := SizeOf(GLUByte);
    GL_UNSIGNED_SHORT: typeSize := SizeOf(GLUShort);
    GL_UNSIGNED_INT: typeSize := SizeOf(GLUInt);
  else
    begin
      Assert(False, glsErrorEx + glsUnknownType);
      exit;
    end;
  end;
  CurrentClient.LastTimeWhenRendered := vCurrentTime;

  offset := CurrentClient.ElementBufferOffset;
  IndexStart := 0;
  IndexFinish := vMaxElementsIndices - 1;
  VertexStart := 0;
  for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
  begin
    if CurrentClient.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if not IsPromitiveSupported then
        continue;
      {: Primitives without adjacency should not be drawn with
         primitives with adjacency }
      if Assigned(CurrentClient.BuiltProp)
        and CurrentClient.BuiltProp.TriangleAdjacency
        and not ((CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY)
        or (CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then
        continue;

      if CurrentClient.PrimitiveType[p] = GLVBOM_PATCHES then
        GL.PatchParameteri(GL_PATCH_VERTICES, CurrentClient.VerticesInPatch);

      pType := cPrimitiveType[CurrentClient.PrimitiveType[p]];

      if Assigned(CurrentClient.BuiltProp) and
        (CurrentClient.BuiltProp.InstancesNumber > 0) then
      begin
        if GL.EXT_draw_instanced or GL.ARB_draw_instanced then
          HardwareInstancing
        else
          PseudoInstancing;
      end
        // Simple drawing vertex array
      else if CurrentClient.IndexCount[p] = 0 then
      begin
        GL.DrawArrays(
          pType,
          VertexStart,
          CurrentClient.VertexCount[p]);
      end
        // Simple drawing with frendly to vertex buffer mapping
      else if GL.EXT_draw_range_elements then
      begin
        fullPartCount := CurrentClient.IndexCount[p] div vMaxElementsIndices;
        restPart := CurrentClient.IndexCount[p] mod vMaxElementsIndices;
        for n := 0 to fullPartCount - 1 do
        begin
          GL.DrawRangeElements(
            pType,
            IndexStart,
            IndexFinish,
            vMaxElementsIndices,
            CurrentClient.IndexType,
            Pointer(offset));
          Inc(IndexStart, vMaxElementsIndices);
          Inc(IndexFinish, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          GL.DrawRangeElements(
            pType,
            IndexStart,
            IndexStart + restPart - 1,
            restPart,
            CurrentClient.IndexType,
            Pointer(offset));
          Inc(IndexStart, restPart);
        end;
      end
      else
        GL.DrawElements(
          pType,
          CurrentClient.IndexCount[p],
          CurrentClient.IndexType,
          Pointer(offset));

      Inc(offset, typeSize * CurrentClient.IndexCount[p]);
      Inc(VertexStart, CurrentClient.VertexCount[p]);
    end
    else
      break;
  end; // for p
end;

function TGLBaseVBOManager.DoMakeAdjacency: Boolean;
var
  p: Integer;
  IndicesList, adjIndicesList: TLongWordList;
  start, count: LongWord;
begin
  Result := False;
  if not GL.EXT_gpu_shader4 then
    exit;

  start := CurrentClient.ElementBufferOffset;
  for p := 0 to FPrimitiveTypeCount - 1 do
    start := start + CurrentClient.IndexCount[p];
  count := CurrentClient.IndexCount[FPrimitiveTypeCount];

  if CurrentClient.PrimitiveType[FPrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP then
  begin
    // Convert strips to independent triangles
    IndicesList := ConvertStripToList(
      @FHostIndexBuffer.List[start], count, CurrentClient.RestartIndex);
    if Assigned(IndicesList) then
    begin
      FHostIndexBuffer.Count := start;
      count := IndicesList.Count;
      FHostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), count);
      CurrentClient.IndexCount[FPrimitiveTypeCount] := count;
      CurrentClient.PrimitiveType[FPrimitiveTypeCount] := GLVBOM_TRIANGLES;
      IndicesList.Free;
    end;
  end
  else if CurrentClient.PrimitiveType[FPrimitiveTypeCount] = GLVBOM_TRIANGLE_FAN then
  begin
    // Convert fans to independent triangles
    IndicesList := ConvertFansToList(
      @FHostIndexBuffer.List[start], count, CurrentClient.RestartIndex);
    if Assigned(IndicesList) then
    begin
      FHostIndexBuffer.Count := start;
      count := IndicesList.Count;
      FHostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), count);
      CurrentClient.IndexCount[FPrimitiveTypeCount] := count;
      CurrentClient.PrimitiveType[FPrimitiveTypeCount] := GLVBOM_TRIANGLES;
      IndicesList.Free;
    end;
  end;

  if CurrentClient.PrimitiveType[FPrimitiveTypeCount] <> GLVBOM_TRIANGLES then
    exit;

  if FHostIndexBuffer.Capacity < Integer(start + 2 * count +
    vEdgeInfoReserveSize) then
    FHostIndexBuffer.Capacity :=
      Integer(start + 2 * count + vEdgeInfoReserveSize);

  adjIndicesList := MakeTriangleAdjacencyList(
    @FHostIndexBuffer.List[start],
    count,
    @FAttributeArrays[0].List[0]);

  if Assigned(adjIndicesList) then
  begin
    FHostIndexBuffer.Count := start;
    FHostIndexBuffer.AddLongWords(PLongWord(adjIndicesList.List),
      adjIndicesList.Count);
    CurrentClient.IndexCount[FPrimitiveTypeCount] := adjIndicesList.Count;
    CurrentClient.PrimitiveType[FPrimitiveTypeCount] :=
      GLVBOM_TRIANGLES_ADJACENCY;
    FObjectIndexCount := FHostIndexBuffer.Items[FHostIndexBuffer.Count - 1] + 1;
    adjIndicesList.Free;
  end;
  Result := True;
end;

procedure TGLBaseVBOManager.BuildErrorModel(
  const BuiltProp: TGLBuiltProperties);
const
  hw = 0.25;
  hh = 0.25;
  hd = 0.25;
  nd = 1.0;
begin

  BeginObject(BuiltProp);
  Attribute3f(attrPosition, 0, 0, 0);
  Attribute3f(attrNormal, 0, 0, 0);
  Attribute2f(attrTexCoord0, 0, 0);
  BeginPrimitives(GLVBOM_TRIANGLES);
  // Front

  Attribute3f(attrNormal, 0, 0, nd);
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, -hw * nd, hh * nd, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, -hw, -hh, hd);
  EmitVertex;
  Attribute3f(attrPosition, -hw, -hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, hw * nd, -hh * nd, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;

  // Back

  Attribute3f(attrNormal, 0, 0, -nd);
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, hw, hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, hw * nd, -hh * nd, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, -hw * nd, hh * nd, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, hw, hh, -hd);
  EmitVertex;

  // Left

  Attribute3f(attrNormal, -nd, 0, 0);
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, -hw, hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, -hw, hh * nd, -hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, -hw, -hh * nd, hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, -hw, hh, hd);
  EmitVertex;

  // Right

  Attribute3f(attrNormal, nd, 0, 0);
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, hw, -hh * nd, hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, hw, -hh, -hd);
  EmitVertex;
  Attribute3f(attrPosition, hw, -hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, hw, hh * nd, -hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;

  // Top

  Attribute3f(attrNormal, 0, nd, 0);
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, -hw, hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, -hw * nd, hh, hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;
  Attribute3f(attrPosition, hw, hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, hw * nd, hh, -hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, -hw, hh, -hd);
  EmitVertex;

  // Bottom

  Attribute3f(attrNormal, 0, -nd, 0);
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 0);
  Attribute3f(attrPosition, hw * nd, -hh, -hd * nd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 1, 1);
  Attribute3f(attrPosition, hw, -hh, hd);
  EmitVertex;
  Attribute3f(attrPosition, hw, -hh, hd);
  EmitVertex;
  Attribute2f(attrTexCoord0, 0, 1);
  Attribute3f(attrPosition, -hw * nd, -hh, hd * nd);
  EmitVertex;
  Attribute3f(attrNormal, 0, -nd, 0);
  Attribute2f(attrTexCoord0, 0, 0);
  Attribute3f(attrPosition, -hw, -hh, -hd);
  EmitVertex;

  EndPrimitives;
  EndObject;

end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLDynamicVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLDynamicVBOManager ------------------
// ------------------

constructor TGLDynamicVBOManager.Create;
begin
  inherited;
end;

destructor TGLDynamicVBOManager.Destroy;
begin
  FreeClient(CurrentClient);
  Dispose(CurrentClient);
  inherited;
end;

procedure TGLDynamicVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
var
  i: integer;
begin
  inherited;

  InitClient(CurrentClient);
  CurrentClient.ArrayHandle := FArrayHandle;
  CurrentClient.ElementHandle := FElementHandle;
  CurrentClient.ArrayBufferOffset := 0;
  CurrentClient.ElementBufferOffset := 0;
  CurrentClient.BuiltProp := BuiltProp;
  FState := GLVBOM_OBJECT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(i);

  FObjectVertexCount := 0;
  FObjectIndexCount := 0;
  FHostIndexBuffer.Flush;
  FDoubling := false;
  FPrimitiveTypeCount := 0;
end;

procedure TGLDynamicVBOManager.EndObject;
var
  a: Integer;
  Attr: T4ByteList;
  offset, size: PtrUInt;
  HostVertexMap: Pointer;
  HostIndexMap: Pointer;
  hVAO: TGLVertexArrayHandle;
begin
  if FState <> GLVBOM_OBJECT then
  begin
    GLSLogger.LogError(glsWrongCallEnd);
    Abort;
  end;

  FState := GLVBOM_DEFAULT;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataSize[a] := FAttributeArrays[a].Count * SizeOf(T4ByteData);
    Inc(CurrentClient.DataSizeInArrayPool, CurrentClient.DataSize[a]);
  end;

  // Vertex buffer managment
  FArrayHandle.AllocateHandle;
  if FArrayHandle.IsDataNeedUpdate then
  begin
    VertexBufferCapacity := CurrentClient.DataSizeInArrayPool;
    if VertexBufferCapacity < 1024 then
      VertexBufferCapacity := 1024;
    FArrayHandle.BindBufferData(nil, VertexBufferCapacity, Usage);
    FArrayHandle.NotifyDataUpdated;
  end
  else
  begin
    FArrayHandle.Bind;
    if VertexBufferCapacity < CurrentClient.DataSizeInArrayPool then
    begin
      VertexBufferCapacity := RoundUpToPowerOf2(CurrentClient.DataSizeInArrayPool);
      FArrayHandle.BufferData(nil, VertexBufferCapacity, Usage);
    end;
  end;

  FElementHandle.AllocateHandle;
  if FHostIndexBuffer.Count > 0 then
  begin
    // Index buffer managment
    if FElementHandle.IsDataNeedUpdate then
    begin
      IndexBufferCapacity := LongWord(FHostIndexBuffer.Count) * SizeOf(GLUInt);
      if IndexBufferCapacity < 1024 then
        IndexBufferCapacity := 1024;
      FElementHandle.BindBufferData(FHostIndexBuffer.List, IndexBufferCapacity,
        Usage);
      FElementHandle.NotifyDataUpdated;
    end
    else
    begin
      FElementHandle.Bind;
      size := LongWord(FHostIndexBuffer.Count) * SizeOf(GLUInt);
      if IndexBufferCapacity < size then
      begin
        IndexBufferCapacity := RoundUpToPowerOf2(size);
        FElementHandle.BufferData(nil, IndexBufferCapacity, Usage);
      end;
      if vUseMappingForOftenBufferUpdate then
      begin
        HostIndexMap := FElementHandle.MapBuffer(GL_WRITE_ONLY);
        Move(FHostIndexBuffer.List^, PLongWord(HostIndexMap)^, size);
        FElementHandle.UnmapBuffer;
      end
      else
        FElementHandle.BufferSubData(0, size, FHostIndexBuffer.List);
    end;
  end;

  // upload each attribute array one after another
  offset := 0;
  if vUseMappingForOftenBufferUpdate then
  begin
    if GL.ARB_map_buffer_range then
      HostVertexMap := FArrayHandle.MapBufferRange(0,
        CurrentClient.DataSizeInArrayPool,
        GL_MAP_WRITE_BIT or
        GL_MAP_INVALIDATE_BUFFER_BIT or
        GL_MAP_UNSYNCHRONIZED_BIT or
        GL_MAP_FLUSH_EXPLICIT_BIT)
    else
      HostVertexMap := FArrayHandle.MapBuffer(GL_WRITE_ONLY);
  end
  else
    HostVertexMap := nil;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if Assigned(CurrentClient.Attributes[a]) then
    begin
      Attr := FAttributeArrays[a];

      if vUseMappingForOftenBufferUpdate then
        Move(
          Attr.List^,
          PByte(PtrUInt(HostVertexMap) + PtrUInt(offset))^,
          CurrentClient.DataSize[a])
      else
        FArrayHandle.BufferSubData(offset, CurrentClient.DataSize[a],
          Attr.List);
      Offset := Offset + CurrentClient.DataSize[a];
    end;
  end;

  if vUseMappingForOftenBufferUpdate then
  begin
    if GL.ARB_map_buffer_range then
      FArrayHandle.Flush(0, CurrentClient.DataSizeInArrayPool);
    FArrayHandle.UnmapBuffer;
  end;
  // Fast rendering without build properties
  if not Assigned(CurrentClient.BuiltProp) then
  begin
    BindVertexArray(hVAO);
    RenderCurrentClient;
    hVAO.UnBind;
  end;

  inherited;
end;

procedure TGLDynamicVBOManager.RenderClient(const BuiltProp:
  TGLBuiltProperties);
var
  hVAO: TGLVertexArrayHandle;
begin
{$IFDEF GLS_MULTITHREAD}
  try
    Lock;
{$ENDIF}

    if Assigned(BuiltProp) then
    begin
      try
        BuiltProp.OnBuildRequest(Self);
      except
        Discard;
        BuildErrorModel(BuiltProp);
        GLSLogger.LogError(glsErrorBuildModel);
      end;

      BindVertexArray(hVAO);
      RenderCurrentClient;
      hVAO.UnBind;
    end;

{$IFDEF GLS_MULTITHREAD}
  finally
    UnLock;
  end;
{$ENDIF}
end;

class function TGLDynamicVBOManager.Usage: TGLenum;
begin
  Result := GL_DYNAMIC_DRAW;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLStaticVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLStaticVBOManager ------------------
// ------------------

constructor TGLStaticVBOManager.Create;
begin
  inherited;
  FClientList := TList.Create;
end;

destructor TGLStaticVBOManager.Destroy;
var
  i: integer;
  Client: PGLRenderPacket;
  MemoryUsed: Double;
begin
  MemoryUsed := (FArrayBufferFreeZone + FElementBufferFreeZone) / (FArrayBufferSize + FElementBufferSize) * 100;
  GLSLogger.LogInfo(Format('Static buffer pools used on - %f %%', [MemoryUsed]));

  // Clear clients info
  for i := 0 to FClientList.Count - 1 do
  begin
    Client := FClientList.Items[i];
    FreeClient(Client);
    Dispose(Client);
  end;
  FClientList.Destroy;
  inherited;
end;

procedure TGLStaticVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
var
  i: integer;
begin
  inherited;
  AllocateBuffers;

  if BuiltProp.ID > 0 then
    CurrentClient := FClientList.Items[BuiltProp.ID - 1];

  InitClient(CurrentClient);
  CurrentClient.ArrayHandle := FArrayHandle;
  CurrentClient.ElementHandle := FElementHandle;
  CurrentClient.ArrayBufferOffset := FArrayBufferFreeZone;
  CurrentClient.ElementBufferOffset := FElementBufferFreeZone;
  CurrentClient.BuiltProp := BuiltProp;

  FState := GLVBOM_OBJECT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(i);
  FHostIndexBuffer.Flush;

  FObjectVertexCount := 0;
  FObjectIndexCount := 0;
  FDoubling := false;
  FPrimitiveTypeCount := 0;
end;

procedure TGLStaticVBOManager.EndObject;
var
  a, I: Integer;
  size, zone, maxIndexValue: Cardinal;
  tempIndexBuffer: Pointer;
  BuiltProp: TGLBuiltProperties;
begin
  if FState <> GLVBOM_OBJECT then
  begin
    GLSLogger.LogError(glsWrongCallEnd);
    Abort;
  end;

  if FObjectVertexCount = 0 then
  begin
    // No one vertex not recorded
    FreeClient(CurrentClient);
    Dispose(CurrentClient);
    Abort;
  end;

  // Calculate size of array
  size := 0;
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if Assigned(CurrentClient.Attributes[a]) then
    begin
      CurrentClient.DataSize[a] := FAttributeArrays[a].Count * SizeOf(T4ByteData);
      Inc(size, CurrentClient.DataSize[a]);
    end;

  // choose place in pool to upload data
  if (CurrentClient.DataSizeInArrayPool > 0) and (size <= CurrentClient.DataSizeInArrayPool) then
  begin
    // Rewrite data in pool if it's size small or equal of current
    zone := CurrentClient.ArrayBufferOffset;
  end
  else
  begin
    zone := FArrayBufferFreeZone;
    if FArrayBufferFreeZone + size > FArrayBufferSize then
    begin
      GLSLogger.LogError('Static vertex array pool is full');
      Abort;
    end;
    Inc(FArrayBufferFreeZone, size);
    CurrentClient.DataSizeInArrayPool := size;
  end;

  // upload each attribute array one after another
  FArrayHandle.Bind;
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if Assigned(CurrentClient.Attributes[a]) then
    begin
      FArrayHandle.BufferSubData(
        zone,
        CurrentClient.DataSize[a],
        FAttributeArrays[a].List);
      Inc(zone, CurrentClient.DataSize[a]);
    end;

  if FHostIndexBuffer.Count > 0 then
  begin

    // Find maxumum index value, skip restart index
    I := FHostIndexBuffer.Count - 1;
    repeat
      maxIndexValue := FHostIndexBuffer[I];
      Dec(I);
    until maxIndexValue < $FFFFFFFF;
    // Adjust index type according it's number
    if maxIndexValue + 1 < $10000 then
    begin
      if (maxIndexValue + 1 < $100) and not GL.VERSION_3_0 then
      begin
        CurrentClient.IndexType := GL_UNSIGNED_BYTE;
        CurrentClient.RestartIndex := $FF;
        size := FHostIndexBuffer.Count * SizeOf(TGLubyte);
      end
      else
      begin
        CurrentClient.IndexType := GL_UNSIGNED_SHORT;
        CurrentClient.RestartIndex := $FFFF;
        size := FHostIndexBuffer.Count * SizeOf(TGLushort);
      end;
    end
    else
    begin
      CurrentClient.IndexType := GL_UNSIGNED_INT;
      CurrentClient.RestartIndex := $FFFFFFFF;
      size := FHostIndexBuffer.Count * SizeOf(TGLuint);
    end;

    // choose place in pool to upload data
    if (CurrentClient.DataSizeInElementPool > 0) and (size <= CurrentClient.DataSizeInElementPool) then
    begin
      // Rewrite data in pool if it's size small or equal of current
      zone := CurrentClient.ElementBufferOffset;
    end
    else
    begin
      zone := FElementBufferFreeZone;
      if FElementBufferFreeZone + size > FElementBufferSize then
      begin
        GLSLogger.LogError('Static element array is full');
        Abort;
      end;
      Inc(FElementBufferFreeZone, size);
      CurrentClient.DataSizeInElementPool := size;
    end;

    FElementHandle.Bind;
    case CurrentClient.IndexType of
      GL_UNSIGNED_BYTE:
        begin
          GetMem(tempIndexBuffer, SizeOf(TGLubyte) * FHostIndexBuffer.Count);
          for I := FHostIndexBuffer.Count - 1 downto 0 do
            PByteArray(tempIndexBuffer)[i] := Byte(FHostIndexBuffer.Items[i]);
          FElementHandle.BufferSubData(zone, size, tempIndexBuffer);
          FreeMem(tempIndexBuffer, FHostIndexBuffer.Count);
        end;

      GL_UNSIGNED_SHORT:
        begin
          GetMem(tempIndexBuffer, SizeOf(TGLushort) * FHostIndexBuffer.Count);
          for I := FHostIndexBuffer.Count - 1 downto 0 do
            PWordVector(tempIndexBuffer)[i] := Word(FHostIndexBuffer.Items[i]);
          FElementHandle.BufferSubData(zone, size, tempIndexBuffer);
          FreeMem(tempIndexBuffer, FHostIndexBuffer.Count);
        end;

      GL_UNSIGNED_INT:
        begin
          FElementHandle.BufferSubData(FElementBufferFreeZone, size, FHostIndexBuffer.List);
        end;
    end;
  end;

  // add and setup client
  BuiltProp := CurrentClient.BuiltProp;
  if BuiltProp.ID = 0 then
  begin
    FClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := Longword(FClientList.Count);
  end;
  CurrentClient := nil;
  FState := GLVBOM_DEFAULT;

  inherited;
end;

procedure TGLStaticVBOManager.AllocateBuffers;
var
  VBOFreeMem: TVector4ui;
  VBOPool: Cardinal;
begin
  FArrayHandle.AllocateHandle;
  FElementHandle.AllocateHandle;
  if FArrayHandle.IsDataNeedUpdate or FElementHandle.IsDataNeedUpdate then
  begin
    if FArrayBufferSize = 0 then
    begin
      if GL.ATI_meminfo then
      begin
        GL.GetIntegerv(GL_VBO_FREE_MEMORY_ATI, @VBOFreeMem[0]);
        GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM', [VBOFreeMem[1] div 1024]));
        VBOPool := VBOFreeMem[1] * 1024 div 4;
      end
      else if GL.NVX_gpu_memory_info then
      begin
        GL.GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, @VBOFreeMem[1]);
        GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM', [VBOFreeMem[1] div 1024]));
        VBOPool := VBOFreeMem[1] * 1024 div 4;
      end
      else
      begin
        VBOPool := GLVBOM_STATIC_POOL;
        GLSLogger.LogInfo('Can''t get info about graphic memory');
      end;
      FArrayBufferSize := 3 * VBOPool div 4;
      FElementBufferSize := VBOPool - FArrayBufferSize;
      GLSLogger.LogInfo(Format('Allocated static vertex buffer pool - %dM', [FArrayBufferSize div $100000]));
      GLSLogger.LogInfo(Format('Allocated static element buffer pool - %dM', [FElementBufferSize div $100000]));
    end;

    FArrayBufferFreeZone := 0;
    FElementBufferFreeZone := 0;

    FArrayHandle.BindBufferData(nil, FArrayBufferSize, Usage);
    FElementHandle.BindBufferData(nil, FElementBufferSize, Usage);
    FArrayHandle.NotifyDataUpdated;
    FElementHandle.NotifyDataUpdated;
  end;
end;

procedure TGLStaticVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties);
var
  hVAO: TGLVertexArrayHandle;
begin
{$IFDEF GLS_MULTITHREAD}
  try
    Lock;
{$ENDIF}

    if FArrayHandle.IsDataNeedUpdate or BuiltProp.FStructureChanged
      or (BuiltProp.ID = 0) then
    begin
      // seems to be has changed context and need to rebuild buffers
      try
        BuiltProp.OnBuildRequest(Self);
        BuiltProp.FStructureChanged := False;
      except
        Discard;
        BuildErrorModel(BuiltProp);
        GLSLogger.LogError(glsErrorBuildModel);
      end;
    end;

    if BuiltProp.ID > 0 then
    begin
      CurrentClient := FClientList.Items[BuiltProp.ID - 1];
      BindVertexArray(hVAO);
      RenderCurrentClient;
      hVAO.UnBind;
      CurrentClient := nil;
    end;

{$IFDEF GLS_MULTITHREAD}
  finally
    UnLock;
  end;
{$ENDIF}
end;

function TGLStaticVBOManager.UsageStatistic(const TimeInterval: Double): Single;
var
  c: Integer;
  Client: PGLRenderPacket;
  portion: Single;
begin
  portion := 0;
  for c := 0 to FClientList.Count - 1 do
  begin
    Client := FClientList.Items[c];
    if Client.LastTimeWhenRendered > vCurrentTime - TimeInterval then
      portion := portion + Client.RelativeSize;
  end;
  Result := portion;
end;

class function TGLStaticVBOManager.Usage: TGLenum;
begin
  Result := GL_STATIC_DRAW;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLStreamVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLStreamVBOManager ------------------
// ------------------

constructor TGLStreamVBOManager.Create;
begin
  inherited Create;
  FClientList := TList.Create;
end;

destructor TGLStreamVBOManager.Destroy;
var
  i: integer;
  Client: PGLRenderPacket;
begin
  // Clear clients info
  for i := 0 to FClientList.Count - 1 do
  begin
    Client := FClientList.Items[i];
    FreeClient(Client);
    Dispose(Client);
  end;
  FClientList.Destroy;
  inherited;
end;

procedure TGLStreamVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
var
  i: integer;
begin
  inherited;

  if BuiltProp.ID <> 0 then
    CurrentClient := FClientList.Items[BuiltProp.ID - 1];

  InitClient(CurrentClient);
  CurrentClient.ArrayBufferOffset := 0;
  CurrentClient.ElementBufferOffset := 0;
  CurrentClient.BuiltProp := BuiltProp;

  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(i);

  FObjectVertexCount := 0;
  FObjectIndexCount := 0;
  FHostIndexBuffer.Flush;
  FDoubling := false;
  FPrimitiveTypeCount := 0;
  FState := GLVBOM_OBJECT;
end;

procedure TGLStreamVBOManager.EndObject;
var
  a: Integer;
  offset, size: PtrUInt;
  HostVertexMap: Pointer;
  HostIndexMap: Pointer;
  BuiltProp: TGLBuiltProperties;
begin
  if FState <> GLVBOM_OBJECT then
  begin
    GLSLogger.LogError(glsWrongCallEnd);
    Abort;
  end;

  FState := GLVBOM_DEFAULT;
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataSize[a] := FAttributeArrays[a].Count * SizeOf(T4ByteData);
    Inc(CurrentClient.DataSizeInArrayPool, CurrentClient.DataSize[a]);
  end;
  if CurrentClient.DataSizeInArrayPool = 0 then
    exit;

  CurrentClient.ArrayHandle.AllocateHandle;
  CurrentClient.ElementHandle.AllocateHandle;

  // Vertex buffer managment
  if CurrentClient.ArrayHandle.IsDataNeedUpdate then
  begin
    CurrentClient.ArrayHandle.AllocateHandle;
    CurrentClient.BuiltProp.VertexBufferCapacity := CurrentClient.DataSizeInArrayPool;
    CurrentClient.ArrayHandle.BindBufferData(nil,
      CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    CurrentClient.ArrayHandle.NotifyDataUpdated;
  end
  else
  begin
    CurrentClient.ArrayHandle.Bind;
    if CurrentClient.BuiltProp.VertexBufferCapacity < CurrentClient.DataSizeInArrayPool then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(CurrentClient.DataSizeInArrayPool);
      CurrentClient.ArrayHandle.BufferData(nil,
        CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;

  // Index buffer managment
  if CurrentClient.ElementHandle.IsDataNeedUpdate then
  begin
    CurrentClient.ElementHandle.AllocateHandle;
    CurrentClient.BuiltProp.IndexBufferCapacity :=
      LongWord(FHostIndexBuffer.Count) * SizeOf(GLUInt);
    CurrentClient.ElementHandle.BindBufferData(FHostIndexBuffer.List,
      CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    CurrentClient.ElementHandle.NotifyDataUpdated;
  end
  else
  begin
    CurrentClient.ElementHandle.Bind;
    size := LongWord(FHostIndexBuffer.Count) * SizeOf(GLUInt);
    if CurrentClient.BuiltProp.IndexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.ElementHandle.BufferData(nil,
        CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    end;
    if vUseMappingForOftenBufferUpdate then
    begin
      HostIndexMap := FElementHandle.MapBuffer(GL_WRITE_ONLY);
      Move(FHostIndexBuffer.List^, PLongWord(HostIndexMap)^, size);
      CurrentClient.ElementHandle.UnmapBuffer;
    end
    else
      CurrentClient.ElementHandle.BufferSubData(0, size, FHostIndexBuffer.List);
  end;

  // upload each attribute array one after another
  offset := 0;
  if vUseMappingForOftenBufferUpdate then
  begin
    HostVertexMap := CurrentClient.ArrayHandle.MapBuffer(GL_WRITE_ONLY);
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if Assigned(CurrentClient.Attributes[a]) then
      begin
        Move(FAttributeArrays[a].List^, PByte(PtrUInt(HostVertexMap) + offset)^,
          CurrentClient.DataSize[a]);
        Inc(offset, CurrentClient.DataSize[a]);
      end;
    CurrentClient.ArrayHandle.UnmapBuffer;
  end
  else
  begin
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if Assigned(CurrentClient.Attributes[a]) then
      begin
        CurrentClient.ArrayHandle.BufferSubData(offset,
          CurrentClient.DataSize[a], FAttributeArrays[a].List);
        Inc(offset, CurrentClient.DataSize[a]);
      end;
  end;

  // add and setup client
  BuiltProp := CurrentClient.BuiltProp;
  if BuiltProp.ID = 0 then
  begin
    FClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := Longword(FClientList.Count);
  end;
  CurrentClient := nil;
  inherited;
end;

procedure TGLStreamVBOManager.EmitVertices(VertexNumber: LongWord; Indexed:
  Boolean);
var
  a: Integer;
  //  offset,
  size: LongWord;
begin
  if FState <> GLVBOM_PRIMITIVE then
  begin
    GLSLogger.LogError(glsWrongCallEmit);
    Abort;
  end;

  if VertexNumber = 0 then
  begin
    GLSLogger.LogError(glsInvalidNumberOfVertex);
    Abort;
  end;

  FState := GLVBOM_DEFAULT;
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataSize[a] := FAttributeArrays[a].Count * SizeOf(T4ByteData);
    Inc(CurrentClient.DataSizeInArrayPool, CurrentClient.DataSize[a]);
  end;
  CurrentClient.ArrayHandle.AllocateHandle;

  // Vertex buffer managment
  if CurrentClient.ArrayHandle.IsDataNeedUpdate then
  begin
    CurrentClient.BuiltProp.VertexBufferCapacity := CurrentClient.DataSizeInArrayPool;
    CurrentClient.ArrayHandle.BindBufferData(nil,
      CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    CurrentClient.ArrayHandle.NotifyChangesOfData;
  end
  else
  begin
    CurrentClient.ArrayHandle.Bind;
    if CurrentClient.BuiltProp.VertexBufferCapacity < CurrentClient.DataSizeInArrayPool then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(CurrentClient.DataSizeInArrayPool);
      CurrentClient.ArrayHandle.BufferData(nil,
        CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;

  if Indexed then
  begin
    // Index buffer managment
    CurrentClient.ElementHandle.AllocateHandle;
    if CurrentClient.ElementHandle.IsDataNeedUpdate then
    begin
      CurrentClient.BuiltProp.IndexBufferCapacity :=
        VertexNumber * SizeOf(GLUInt);
      CurrentClient.ElementHandle.BindBufferData(nil,
        CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
      CurrentClient.ElementHandle.NotifyDataUpdated;
    end
    else
    begin
      CurrentClient.ElementHandle.Bind;
      size := VertexNumber * SizeOf(GLUInt);
      if CurrentClient.BuiltProp.IndexBufferCapacity < size then
      begin
        CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
        CurrentClient.ElementHandle.BufferData(nil,
          CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
      end;
    end;
  end
  else if CurrentClient.ElementHandle.Handle <> 0 then
    CurrentClient.ElementHandle.DestroyHandle;

  // add and setup client
  if CurrentClient.BuiltProp.ID = 0 then
  begin
    FClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := Longword(FClientList.Count);
  end;
  CurrentClient.VertexCount[FPrimitiveTypeCount] := VertexNumber;
  CurrentClient := nil;
end;

procedure TGLStreamVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties);
var
  hVAO: TGLVertexArrayHandle;
  NeedBuild: Boolean;
begin
{$IFDEF GLS_MULTITHREAD}
  try
    Lock;
{$ENDIF}
    NeedBuild := (BuiltProp.ID = 0) or BuiltProp.FStructureChanged;
    if (BuiltProp.ID > 0) then
    begin
      CurrentClient := FClientList.Items[BuiltProp.ID - 1];
      NeedBuild := NeedBuild or CurrentClient.ArrayHandle.IsDataNeedUpdate;
    end;


    if NeedBuild then
    begin
      try
        BuiltProp.OnBuildRequest(Self);
        BuiltProp.FStructureChanged := False;
      except
        Discard;
        BuildErrorModel(BuiltProp);
        GLSLogger.LogError(glsErrorBuildModel);
      end;
    end;

    if (BuiltProp.ID > 0) then
    begin
      CurrentClient := FClientList.Items[BuiltProp.ID - 1];
      BindVertexArray(hVAO);
      RenderCurrentClient;
      hVAO.UnBind;
      CurrentClient := nil;
    end;

{$IFDEF GLS_MULTITHREAD}
  finally
    UnLock;
  end;
{$ENDIF}
end;

class function TGLStreamVBOManager.Usage: TGLenum;
begin
  Result := GL_STREAM_DRAW;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

finalization

  FreeVBOManagers;

end.

