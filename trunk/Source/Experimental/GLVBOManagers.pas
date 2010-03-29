//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLVBOManagers<p>

   Base unit to drawing geometry data in vertex buffer objects.<p>
   Require OpenGL 2.1.<p>
   Try to not change buffer usage during runtime because static <p>
   and stream data not removed from video memory until application running.<p>

   <b>History : </b><font size=-1><ul>

    <li>29/03/10 - Yar - Added multicontext and multithreading support
    <li>24/03/10 - Yar - Creation
 </ul></font>
}

unit GLVBOManagers;

interface

{$I GLScene.inc}

uses
  Classes,
  // GLScene
  BaseClasses, OpenGL1x, GLContext, GLRenderContextInfo, GLState,
  GL3xShadersManager, VectorTypes, VectorLists,
  GLSLog;

const
  GLVBOM_MAX_DIFFERENT_PRIMITIVES = 8;

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

    GLVBOM_NODATA,
    GLVBOM_CUSTOM_DATA,
    GLVBOM_1F,
    GLVBOM_2F,
    GLVBOM_3F,
    GLVBOM_4F,
    GLVBOM_1I,
    GLVBOM_2I,
    GLVBOM_3I,
    GLVBOM_4I,
    GLVBOM_4UB
    );

  TGLBufferUsage = (buStatic, buDynamic, buStream);

  TGLBaseVBOManager = class;
  TGLBaseVBOManagerClass = class of TGLBaseVBOManager;
  TGLBuiltProperties = class;

  // Information about object graphic data (something like DIP)
  PGLRenderPacket = ^TGLRenderPacket;
  TGLRenderPacket = record
    VertexHandle: TGLVBOArrayBufferHandle;
    IndexHandle: TGLVBOElementArrayHandle;
    ArrayHandle: TGLVertexArrayHandle;
    FirstVertex: LongWord;
    FirstIndex: LongWord;
    VertexCount: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    IndexCount: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    PrimitiveType: array[0..GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of TGLVBOMEnum;

    AttributeID: array[0..GLS_VERTEX_ATTR_NUM - 1] of Integer;
    AttributesInUse: array[0..GLS_VERTEX_ATTR_NUM - 1] of Boolean;
    DataSize: array[0..GLS_VERTEX_ATTR_NUM - 1] of LongWord;
    DataType: array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLVBOMEnum;
    BuiltProp: TGLBuiltProperties;
    LastTimeWhenRendered: Double;
    RelativeSize: Single;
  end;

  TGLBuiltProperties = class(TPersistent)
  private
    { Private declarations }
    Owner: TComponent;
    ID: Longword;
    FUsage: TGLBufferUsage;
    FVertexWelding: Boolean;
    FTriangleAdjacency: Boolean;
    FInstancesNumber: Integer;
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
    property VertexBufferHandle: GLUInt read GetVertexBufferHandle;
    property IndexBufferHandle: GLUInt read GetIndexBufferHandle;

    property Manager: TGLBaseVBOManager read GetManager;
    property OwnerNotifyChange: TNotifyEvent read FOwnerNotifyChange
      write FOwnerNotifyChange;
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
    FRenderingContext: TGLContext;
    FVertexHandle: TGLVBOArrayBufferHandle;
    FIndexHandle: TGLVBOElementArrayHandle;
    FBuilded: Boolean;
    Usage: LongWord;
    CurrentClient: PGLRenderPacket;
    GLVBOMState: TGLVBOMState;
    AttributeArrays: array[0..GLS_VERTEX_ATTR_NUM - 1] of T4ByteList;
    CurrentValue: array[0..GLS_VERTEX_ATTR_NUM - 1, 0..3] of T4ByteData;
    OneVertexDataSize: LongWord;
    ObjectVertexCount: Integer; // From BeginObject to EndObject
    HostIndexBuffer: TLongWordList;
    ObjectIndex: LongWord;
    Doubling: Boolean;
    RestartIndex: GLUInt;
    MaxIndexValue: LongWord;
    PrimitiveTypeCount: Integer;
    WasUsedList: Boolean;
    function GetAttributeLocation(const AttrName: TGLSLAttribute;
      eType: TGLVBOMEnum): GLint;

    procedure ResetAttribArray(idx: integer);
    procedure InitClient(var AClient: PGLRenderPacket);
    procedure FreeClient(const AClient: PGLRenderPacket);
    procedure EnablePrimitiveRestart(const rci: TRenderContextInfo);
  public
    { Public Declarations }
    constructor Create(const AContext: TGLContext); virtual;
    destructor Destroy; override;

    {: Begins storing a piece of geometry }
    procedure BeginObject(const BuiltProp: TGLBuiltProperties);
      virtual; abstract;
    {: Ends a piece of geometry. }
    procedure EndObject(const rci: TRenderContextInfo);
      virtual; abstract;
    {: Begins gathering information about the given type of primitives.
       An object can only consist of a set of primitives of the same type. }
    procedure BeginPrimitives(eType: TGLVBOMEnum);
      virtual; abstract;
    {: Ends gathering information about the primitive. }
    procedure EndPrimitives;
      virtual; abstract;
    {: Specifies a new value for the attribute with the given name. }
    procedure Attribute1f(const AttrName: TGLSLAttribute; a1: GLfloat);
    procedure Attribute2f(const AttrName: TGLSLAttribute; a1, a2: GLfloat);
      overload;
    procedure Attribute2f(const AttrName: TGLSLAttribute; const a: TVector2f);
      overload;
    procedure Attribute3f(const AttrName: TGLSLAttribute; a1, a2, a3: GLfloat);
      overload;
    procedure Attribute3f(const AttrName: TGLSLAttribute; const a: TVector3f);
      overload;
    procedure Attribute4f(const AttrName: TGLSLAttribute;
      a1, a2, a3, a4: GLfloat);
      overload;
    procedure Attribute4f(const AttrName: TGLSLAttribute; const a: TVector4f);
      overload;
    procedure Attribute1i(const AttrName: TGLSLAttribute; a1: GLint);
    procedure Attribute2i(const AttrName: TGLSLAttribute; a1, a2: GLint);
      overload;
    procedure Attribute2i(const AttrName: TGLSLAttribute; const a: TVector2i);
      overload;
    procedure Attribute3i(const AttrName: TGLSLAttribute; a1, a2, a3: GLint);
      overload;
    procedure Attribute3i(const AttrName: TGLSLAttribute; const a: TVector3i);
      overload;
    procedure Attribute4i(const AttrName: TGLSLAttribute;
      a1, a2, a3, a4: GLint); overload;
    procedure Attribute4i(const AttrName: TGLSLAttribute; const a: TVector4i);
      overload;
    procedure Attribute4ub(const AttrName: TGLSLAttribute;
      a1, a2, a3, a4: GLubyte);
    {: Takes a full list of attribute values,
       but does not determine its type, so you must use AttributeXX
       between BeginObject and BeginPrimitives }
    procedure AttributeList(const AttrName: TGLSLAttribute; const AList:
      TSingleList);
      overload;
    procedure AttributeList(const AttrName: TGLSLAttribute; const AList:
      TIntegerList);
      overload;
    {: Count of list should be a multiple of four }
    procedure AttributeList(const AttrName: TGLSLAttribute; const AList:
      TByteList);
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
    {: Execute build stage }
    procedure BuildBuffer(const rci: TRenderContextInfo);
      dynamic;
    {: Store time }
    procedure DoProgress(const progressTime: TProgressTimes);
      dynamic;
    {: Rendering sender }
    procedure RenderClient(const BuiltProp: TGLBuiltProperties; const rci:
      TRenderContextInfo);
      dynamic;
    {: Return true if buffer is uploaded to video memory.
       Dynamic VBO always return false }
    property IsBuilded: Boolean read fBuilded;
    {: Rendering context to which it belongs manager. }
    property RenderingContext: TGLContext read FRenderingContext;
  end;

  // TGLStaticVBOManager
  //
  TGLStaticVBOManager = class(TGLBaseVBOManager)
  private
    { Private declarations }
    ClientList: TList;
    HostVertexBuffer: T4ByteList;
    indexType: GLenum; // UByte, UShort or UInt
  public
    { Public Declarations }
    constructor Create(const AContext: TGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties);
      override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
    procedure BuildBuffer(const rci: TRenderContextInfo); override;
    procedure RenderClient(const BuiltProp: TGLBuiltProperties; const rci:
      TRenderContextInfo); override;
    {: Returns the portion of the buffer that is used during the time interval. }
    function UsageStatistic(const TimeInterval: Double): Single;
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
    constructor Create(const AContext: TGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties); override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
  end;

  // TGLStreamVBOManager
  //
  TGLStreamVBOManager = class(TGLBaseVBOManager)
  private
    { Private declarations }
    ClientList: TList;
  public
    { Public Declarations }
    constructor Create(const AContext: TGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TGLBuiltProperties); override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
    procedure EmitVertices(VertexNumber: LongWord; Indexed: Boolean); override;
    procedure RenderClient(const BuiltProp: TGLBuiltProperties; const rci:
      TRenderContextInfo); override;
  end;

var
  vUseMappingForOftenBufferUpdate: Boolean = False;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ENDIF}
  vCurrentTime: Double;

function StaticVBOManager: TGLStaticVBOManager;
function DynamicVBOManager: TGLDynamicVBOManager;
function StreamVBOManager: TGLStreamVBOManager;

implementation

uses
  VectorGeometry, GLUtils, GLStrings, MeshUtils;

const
  FourByteZero: T4ByteData = (Int: (Value: 0));
  cPrimitiveType: array[GLVBOM_TRIANGLES..GLVBOM_TRIANGLE_STRIP_ADJACENCY] of
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
    GL_TRIANGLE_STRIP_ADJACENCY
    );

resourcestring
  glsCanNotRebuild =
    'Static object can not be rebuilded';
  glsBadAttrCombination =
    'Single and list attributes can not be combined';
  glsWrongAttrType =
    'An attribute was used with different type than previously or bad list size';
  glsWrongCallBegin =
    'This function cannot be called again before EndPart has been called.';
  glsWrongCallEnd =
    'This function must be called after BeginObject ... EndPrimitive.';
  glsWrongCallEmit =
    'This function must be called after BeginPrimitive ... EndPrimitive.';
  glsNoShader =
    'Currently no shader is bound.';
  glsInvalidNumberOfVertex =
    'The number of primitives to render is invalid. You need to construct complete primitives.';
  glsWrongCallBeginPrim =
    'This function cannot be called recursively or before BeginObject.';
  glsInvalidPrimType =
    'Invalid primitive type.';
  glsWrongCallEndPrim =
    'Before calling this function Begin must have been called.';
  glsTooMachDiffPrim =
    'Too mach different primitive types in one object.';
  glsAlreadyDefined =
    'Geometric data of the object already identified.';
  glsNoActiveRC =
    'Using the VBO manager without the active rendeering context.';

var
  vMaxElementsIndices: GLUInt;
{$IFNDEF GLS_MULTITHREAD}
  vVBOManagerList: TList;
{$ELSE}
  vVBOManagerList: TThreadList;
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Global Managment'}{$ENDIF}

function GetVBOManager(const AClass: TGLBaseVBOManagerClass): TGLBaseVBOManager;
var
  I: Integer;
  RC: TGLContext;
begin
  RC := CurrentGLContext;
  if RC = nil then
    Log.LogWarning(glsNoActiveRC);
  // Find suitable manager
{$IFNDEF GLS_MULTITHREAD}
  for I := 0 to vVBOManagerList.Count - 1 do
  begin
    Result := TGLBaseVBOManager(vVBOManagerList[I]);
    if (Result is AClass)
      and ((Result.RenderingContext = RC) or (RC = nil)) then
      exit;
  end;
  // Create new
  Result := AClass.Create(RC);
  vVBOManagerList.Add(Result);
{$ELSE}
  with vVBOManagerList.LockList do
  begin
    for I := 0 to Count - 1 do
    begin
      Result := TGLBaseVBOManager(Items[I]);
      if (Result is AClass)
        and ((Result.RenderingContext = RC) or (RC = nil)) then
        exit;
    end;
    // Create new
    Result := AClass.Create(RC);
    Add(Result);
  end;
{$ENDIF}
end;

function StaticVBOManager: TGLStaticVBOManager;
begin
  Result := GetVBOManager(TGLStaticVBOManager) as TGLStaticVBOManager;
end;

function DynamicVBOManager: TGLDynamicVBOManager;
begin
  Result := GetVBOManager(TGLDynamicVBOManager) as TGLDynamicVBOManager;
end;

function StreamVBOManager: TGLStreamVBOManager;
begin
  Result := GetVBOManager(TGLStreamVBOManager) as TGLStreamVBOManager;
end;

procedure FreeVBOManagers;
var
  I: Integer;
  vManager: TGLBaseVBOManager;
begin
{$IFNDEF GLS_MULTITHREAD}
  for I := 0 to vVBOManagerList.Count - 1 do
  begin
    vManager := TGLBaseVBOManager(vVBOManagerList[I]);
    vManager.Free;
  end;
{$ELSE}
  with vVBOManagerList.LockList do
  begin
    for I := 0 to Count - 1 do
    begin
      vManager := TGLBaseVBOManager(Items[I]);
      vManager.Free;
    end;
  end;
{$ENDIF}
  vVBOManagerList.Free;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

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
  FVertexWelding := true;
  FTriangleAdjacency := false;
  FInstancesNumber := 0;
end;

procedure TGLBuiltProperties.SetUsage(const Value: TGLBufferUsage);
begin
  if Value <> fUsage then
  begin
    fUsage := Value;
    ID := 0;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
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
    fVertexWelding := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

procedure TGLBuiltProperties.SetTriangleAdjacency(const Value: Boolean);
begin
  if Value <> fTriangleAdjacency then
  begin
    fTriangleAdjacency := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

procedure TGLBuiltProperties.SetInstancesNumber(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fInstancesNumber then
  begin
    fInstancesNumber := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
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
        Client := TGLStaticVBOManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.VertexHandle) then
          Result := Client.VertexHandle.Handle;
      end;
    buDynamic:
      Result := TGLDynamicVBOManager(Manager).fVertexHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.VertexHandle) then
          Result := Client.VertexHandle.Handle;
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
        Client := TGLStaticVBOManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.IndexHandle) then
          Result := Client.IndexHandle.Handle;
      end;
    buDynamic:
      Result := TGLDynamicVBOManager(Manager).fIndexHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.IndexHandle) then
          Result := Client.IndexHandle.Handle;
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
        Client := TGLStaticVBOManager(Manager).ClientList.Items[ID - 1];
        for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
          Inc(Result, Client.VertexCount[p]);
      end;
    buStream:
      if ID > 0 then
      begin
        Client := TGLStreamVBOManager(Manager).ClientList.Items[ID - 1];
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

constructor TGLBaseVBOManager.Create(const AContext: TGLContext);
var
  i: Integer;
begin
  FRenderingContext := AContext;
  fVertexHandle := TGLVBOArrayBufferHandle.Create;
  fIndexHandle := TGLVBOElementArrayHandle.Create;
  HostIndexBuffer := TLongWordList.Create;
  HostIndexBuffer.SetCountResetsMemory := false;
  GLVBOMState := GLVBOM_DEFAULT;
  OneVertexDataSize := 0;
  WasUsedList := False;
  for i := 0 to High(AttributeArrays) do
    AttributeArrays[i] := T4ByteList.Create;
  RestartIndex := $FFFFFFFF;
end;

destructor TGLBaseVBOManager.Destroy;
var
  i: Integer;
begin
  fVertexHandle.Destroy;
  fIndexHandle.Destroy;
  for i := 0 to High(AttributeArrays) do
    AttributeArrays[i].Destroy;
  HostIndexBuffer.Destroy;
end;

procedure TGLBaseVBOManager.Discard;
var
  i, p: integer;
begin
  GLVBOMState := GLVBOM_DEFAULT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(i);

  if Assigned(CurrentClient) then
    for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
      for i := 0 to Integer(CurrentClient.IndexCount[p]) - 1 do
        HostIndexBuffer.Pop;

  if Self is TGLStaticVBOManager then
  begin
    FreeClient(CurrentClient);
    Dispose(CurrentClient);
  end;
  CurrentClient := nil;
end;

procedure TGLBaseVBOManager.ResetAttribArray(idx: integer);
begin
  Assert(idx < GLS_VERTEX_ATTR_NUM,
    'ResetAttribArray: Array index out of bound.');
  AttributeArrays[idx].Flush;
  CurrentValue[idx][0] := FourByteZero;
  CurrentValue[idx][1] := FourByteZero;
  CurrentValue[idx][2] := FourByteZero;
  CurrentValue[idx][3] := FourByteZero;
end;

procedure TGLBaseVBOManager.InitClient(var AClient: PGLRenderPacket);
var
  i: Integer;
begin
  if not Assigned(AClient) then
  begin
    New(AClient);
    AClient.VertexHandle := nil;
    AClient.IndexHandle := nil;
    AClient.ArrayHandle := nil;
    AClient.LastTimeWhenRendered := 0;
    AClient.RelativeSize := 0;
  end;
  for i := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
  begin
    AClient.PrimitiveType[i] := GLVBOM_NOPRIMITIVE;
    AClient.IndexCount[i] := 0;
    AClient.VertexCount[i] := 0;
  end;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    AClient.AttributesInUse[i] := False;
    AClient.AttributeID[i] := 0;
    AClient.DataType[0] := GLVBOM_NODATA;
  end;
end;

procedure TGLBaseVBOManager.FreeClient(const AClient: PGLRenderPacket);
begin
  if Assigned(AClient) then
  begin
    AClient.ArrayHandle.Free;
    AClient.ArrayHandle := nil;
    if Self is TGLStreamVBOManager then
    begin
      AClient.VertexHandle.Free;
      AClient.VertexHandle := nil;
      AClient.IndexHandle.Free;
      AClient.IndexHandle := nil;
    end;
  end;
end;

procedure TGLBaseVBOManager.EnablePrimitiveRestart(const rci:
  TRenderContextInfo);
begin
  if rci.GLStates.ForwardContext then
  begin
    rci.GLStates.EnablePrimitiveRestart := True;
    rci.GLStates.PrimitiveRestartIndex := RestartIndex;
  end
  else if GL_NV_primitive_restart then
  begin
    glEnableClientState(GL_PRIMITIVE_RESTART_NV);
    rci.GLStates.PrimitiveRestartIndex := RestartIndex;
  end;
end;

procedure TGLBaseVBOManager.EmitVertex;
var
  a, v, I, etalon, count: integer;
  AA: T4ByteList;
  dt: TGLVBOMEnum;
  weld: Boolean;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEmit);

  if WasUsedList then
  begin
    // Emit List of Vertex
    etalon := -1;
    count := 0;
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if CurrentClient.AttributesInUse[a] then
      begin
        AA := AttributeArrays[a];

        case CurrentClient.DataType[a] of
          GLVBOM_1I,
            GLVBOM_1F: count := AA.Count;
          GLVBOM_2I,
            GLVBOM_2F: count := AA.Count div 2;
          GLVBOM_3I,
            GLVBOM_3F: count := AA.Count div 3;
          GLVBOM_4UB,
            GLVBOM_4I,
            GLVBOM_4F: count := AA.Count div 4;
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
        if etalon < 0 then
        begin
          etalon := count;
          continue;
        end
        else
          Assert(etalon = count,
            'EmitVertex: Lists of attributes do not match the length');
      end;

    //    for i := 0 to etalon - 1 do
    //    begin
    //      HostIndexBuffer.Push(ObjectIndex);
    //      Inc(ObjectIndex);
    //    end;
    //
    //    Inc(CurrentClient.IndexCount[PrimitiveTypeCount], etalon);
    Inc(CurrentClient.VertexCount[PrimitiveTypeCount], etalon);
    Inc(ObjectVertexCount, etalon);
    if MaxIndexValue < ObjectIndex then
      MaxIndexValue := ObjectIndex;
    WasUsedList := False;
  end
    // Emit single vertex
  else
  begin
    I := -1;
    weld := false;
    if CurrentClient.BuiltProp.VertexWelding then
    begin
      for v := 0 to ObjectVertexCount - 1 do
      begin
        weld := true;
        for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
          if CurrentClient.AttributesInUse[a] then
          begin
            AA := AttributeArrays[a];
            dt := CurrentClient.DataType[a];
            if (dt = GLVBOM_1I) or
              (dt = GLVBOM_1F) or
              (dt = GLVBOM_4UB) then
            begin
              if AA.Items[v].Int.Value <>
                CurrentValue[a][0].Int.Value then
              begin
                weld := false;
                break;
              end;
            end
            else if (dt = GLVBOM_2I) or
              (dt = GLVBOM_2F) then
            begin
              if (AA.Items[v * 2 + 0].Int.Value <>
                CurrentValue[a][0].Int.Value)
                or (AA.Items[v * 2 + 1].Int.Value <>
                CurrentValue[a][1].Int.Value) then
              begin
                weld := false;
                break;
              end;
            end
            else if (dt = GLVBOM_3I) or
              (dt = GLVBOM_3F) then
            begin
              if (AA.Items[v * 3 + 0].Int.Value <>
                CurrentValue[a][0].Int.Value)
                or (AA.Items[v * 3 + 1].Int.Value <>
                CurrentValue[a][1].Int.Value)
                or (AA.Items[v * 3 + 2].Int.Value <>
                CurrentValue[a][2].Int.Value) then
              begin
                weld := false;
                break;
              end;
            end
            else if (dt = GLVBOM_4I) or
              (dt = GLVBOM_4F) then
            begin
              if (AA.Items[v * 3 + 0].Int.Value <>
                CurrentValue[a][0].Int.Value)
                or (AA.Items[v * 3 + 1].Int.Value <>
                CurrentValue[a][1].Int.Value)
                or (AA.Items[v * 3 + 2].Int.Value <>
                CurrentValue[a][2].Int.Value)
                or (AA.Items[v * 3 + 3].Int.Value <>
                CurrentValue[a][3].Int.Value) then
              begin
                weld := false;
                break;
              end;
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
        if CurrentClient.AttributesInUse[a] then
        begin
          AA := AttributeArrays[a];
          dt := CurrentClient.DataType[a];
          if (dt = GLVBOM_1I) or
            (dt = GLVBOM_1F) or
            (dt = GLVBOM_4UB) then
          begin
            AA.Push(CurrentValue[a][0]);
          end
          else if (dt = GLVBOM_2I) or
            (dt = GLVBOM_2F) then
          begin
            AA.Push(CurrentValue[a][0]);
            AA.Push(CurrentValue[a][1]);
          end
          else if (dt = GLVBOM_3I) or
            (dt = GLVBOM_3F) then
          begin
            AA.Push(CurrentValue[a][0]);
            AA.Push(CurrentValue[a][1]);
            AA.Push(CurrentValue[a][2]);
          end
          else if (dt = GLVBOM_4I) or
            (dt = GLVBOM_4F) then
          begin
            AA.Push(CurrentValue[a][0]);
            AA.Push(CurrentValue[a][1]);
            AA.Push(CurrentValue[a][2]);
            AA.Push(CurrentValue[a][3]);
          end
          else
            Assert(False, 'Vertex: Data-Type is invalid');
        end;

      HostIndexBuffer.Push(ObjectIndex);
      Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
      Inc(CurrentClient.VertexCount[PrimitiveTypeCount]);
      if Doubling then
      begin
        HostIndexBuffer.Push(ObjectIndex);
        Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
        Doubling := false;
      end;
      Inc(ObjectIndex);
      Inc(ObjectVertexCount);
      if MaxIndexValue < ObjectIndex then
        MaxIndexValue := ObjectIndex;
    end
    else
    begin
      // Push this offer index of repetitive vertex
      HostIndexBuffer.Push(I);
      Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
      Inc(CurrentClient.VertexCount[PrimitiveTypeCount]);
      if Doubling then
      begin
        HostIndexBuffer.Push(I);
        Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
        Doubling := false;
      end;
    end;
  end;
end;

procedure TGLBaseVBOManager.RestartStrip;
begin
  if WasUsedList then
    exit;
  Assert(GLVBOMState = GLVBOM_PRIMITIVE,
    'RestartStrip: This function must be called between Begin / End.');
  Assert(not Doubling, 'RestartStrip: Excessive call.');
  Assert(
    (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP)
    or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP)
    or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_FAN)
    or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_QUAD_STRIP)
    or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_LINE_STRIP),
    'RestartStrip: This primitive type does not need to restart.');

  if GL_NV_primitive_restart then
  begin
    HostIndexBuffer.Push(RestartIndex);
  end
  else
  begin
    // Create degenerate primitive
    HostIndexBuffer.Push(ObjectIndex - 1);
    Doubling := true;
  end;
  Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
end;

function TGLBaseVBOManager.GetAttributeLocation(const AttrName: TGLSLAttribute;
  eType: TGLVBOMEnum): GLint;
var
  a: integer;
  Finded: Boolean;
  prog: GLUint;
  nm: PGLChar;
begin
  // if the attribute has never been used before
  Result := -1;
  Assert(AttrName.ID > 0, 'GetAttributeLocation: attribute not registered');
  Finded := false;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if CurrentClient.AttributeID[a] = AttrName.ID then
    begin
      Result := a;
      Finded := true;
      break;
    end;

  if not Finded then
  begin
    if GLVBOMState <> GLVBOM_OBJECT then
      exit;
    prog := FRenderingContext.GLStates.CurrentProgram;
    nm := PGLChar(TGLString(AttrName.Name));
    Result := glGetAttribLocation(prog, nm);

    Assert(Result < GLS_VERTEX_ATTR_NUM,
      'GetAttributeLocation: Location is out of bound.');

    if Result <> -1 then
    begin
      CurrentClient.AttributeID[Result] := AttrName.ID;
      CurrentClient.DataType[Result] := eType;
      CurrentClient.AttributesInUse[Result] := True;

      if (eType = GLVBOM_1F) or
        (eType = GLVBOM_1I) or
        (eType = GLVBOM_4UB) then
        Inc(OneVertexDataSize, 1 * sizeof(T4ByteData))
      else if (eType = GLVBOM_2F) or
        (eType = GLVBOM_2I) then
        Inc(OneVertexDataSize, 2 * sizeof(T4ByteData))
      else if (eType = GLVBOM_3F) or
        (eType = GLVBOM_3I) then
        Inc(OneVertexDataSize, 3 * sizeof(T4ByteData))
      else if (eType = GLVBOM_4F) or
        (eType = GLVBOM_4I) then
        Inc(OneVertexDataSize, 4 * sizeof(T4ByteData));
    end
    else
      exit;
  end;

  if eType <> GLVBOM_CUSTOM_DATA then
    Assert(CurrentClient.DataType[Result] = eType,
      'GetAttributeLocation: An attribute was used with different type than previously.');
end;

procedure TGLBaseVBOManager.Attribute1f(const AttrName: TGLSLAttribute; a1:
  GLfloat);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_1F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a1;
end;

procedure TGLBaseVBOManager.Attribute2f(const AttrName: TGLSLAttribute; a1, a2:
  GLfloat);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
end;

procedure TGLBaseVBOManager.Attribute2f(const AttrName: TGLSLAttribute;
  const a: TVector2f);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a[0];
  CurrentValue[Location, 1].Float.Value := a[1];
end;

procedure TGLBaseVBOManager.Attribute3f(const AttrName: TGLSLAttribute; a1, a2,
  a3:
  GLfloat);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
  CurrentValue[Location, 2].Float.Value := a3;
end;

procedure TGLBaseVBOManager.Attribute3f(const AttrName: TGLSLAttribute;
  const a: TVector3f);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a[0];
  CurrentValue[Location, 1].Float.Value := a[1];
  CurrentValue[Location, 2].Float.Value := a[2];
end;

procedure TGLBaseVBOManager.Attribute4f(const AttrName: TGLSLAttribute; a1, a2,
  a3,
  a4: GLfloat);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
  CurrentValue[Location, 2].Float.Value := a3;
  CurrentValue[Location, 3].Float.Value := a4;
end;

procedure TGLBaseVBOManager.Attribute4f(const AttrName: TGLSLAttribute;
  const a: TVector4f);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4F);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Float.Value := a[0];
  CurrentValue[Location, 1].Float.Value := a[1];
  CurrentValue[Location, 2].Float.Value := a[2];
  CurrentValue[Location, 3].Float.Value := a[3];
end;

procedure TGLBaseVBOManager.Attribute1i(const AttrName: TGLSLAttribute; a1:
  GLint);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_1I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a1;
end;

procedure TGLBaseVBOManager.Attribute2i(const AttrName: TGLSLAttribute; a1, a2:
  GLint);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
end;

procedure TGLBaseVBOManager.Attribute2i(const AttrName: TGLSLAttribute;
  const a: TVector2i);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a[0];
  CurrentValue[Location, 1].Int.Value := a[1];
end;

procedure TGLBaseVBOManager.Attribute3i(const AttrName: TGLSLAttribute; a1, a2,
  a3:
  GLint);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
  CurrentValue[Location, 2].Int.Value := a3;
end;

procedure TGLBaseVBOManager.Attribute3i(const AttrName: TGLSLAttribute;
  const a: TVector3i);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a[0];
  CurrentValue[Location, 1].Int.Value := a[1];
  CurrentValue[Location, 2].Int.Value := a[2];
end;

procedure TGLBaseVBOManager.Attribute4i(const AttrName: TGLSLAttribute; a1, a2,
  a3,
  a4: GLint);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
  CurrentValue[Location, 2].Int.Value := a3;
  CurrentValue[Location, 3].Int.Value := a4;
end;

procedure TGLBaseVBOManager.Attribute4i(const AttrName: TGLSLAttribute;
  const a: TVector4i);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4I);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Int.Value := a[0];
  CurrentValue[Location, 1].Int.Value := a[1];
  CurrentValue[Location, 2].Int.Value := a[2];
  CurrentValue[Location, 3].Int.Value := a[3];
end;

procedure TGLBaseVBOManager.Attribute4ub(const AttrName: TGLSLAttribute; a1, a2,
  a3,
  a4: GLubyte);
var
  Location: integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4UB);
  if Location = -1 then
    Exit;
  CurrentValue[Location, 0].Bytes.Value[0] := a1;
  CurrentValue[Location, 0].Bytes.Value[1] := a2;
  CurrentValue[Location, 0].Bytes.Value[2] := a3;
  CurrentValue[Location, 0].Bytes.Value[3] := a4;
end;

procedure TGLBaseVBOManager.AttributeList(const AttrName: TGLSLAttribute; const
  AList: TSingleList);
var
  Location: integer;
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then
    exit;
  Valid := false;
  case CurrentClient.DataType[Location] of
    GLVBOM_1F: Valid := true;
    GLVBOM_2F: Valid := (AList.Count mod 2 = 0);
    GLVBOM_3F: Valid := (AList.Count mod 3 = 0);
    GLVBOM_4F: Valid := (AList.Count mod 4 = 0);
  end;
  Assert(Valid, glsWrongAttrType);

  AA := AttributeArrays[Location];
  Last := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
  WasUsedList := True;
end;

procedure TGLBaseVBOManager.AttributeList(const AttrName: TGLSLAttribute; const
  AList: TIntegerList);
var
  Location: integer;
  Valid: Boolean;
  AA: T4ByteList;
  Last: Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then
    Exit;
  Valid := false;
  case CurrentClient.DataType[Location] of
    GLVBOM_1I: Valid := true;
    GLVBOM_2I: Valid := (AList.Count mod 2 = 0);
    GLVBOM_3I: Valid := (AList.Count mod 3 = 0);
    GLVBOM_4I: Valid := (AList.Count mod 4 = 0);
  end;
  Assert(Valid, glsWrongAttrType);

  AA := AttributeArrays[Location];
  Last := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * SizeOf(T4ByteData));
  WasUsedList := True;
end;

procedure TGLBaseVBOManager.AttributeList(const AttrName: TGLSLAttribute; const
  AList: TByteList);
var
  Location: integer;
  AA: T4ByteList;
  Last: Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then
    Exit;

  Assert((CurrentClient.DataType[Location] = GLVBOM_4UB)
    and (AList.Count mod 4 = 0), glsWrongAttrType);
  AA := AttributeArrays[Location];
  Last := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count);
  WasUsedList := True;
end;

procedure TGLBaseVBOManager.EmitVertices(VertexNumber: LongWord; Indexed:
  Boolean);
begin
end;

procedure TGLBaseVBOManager.BuildBuffer(const rci: TRenderContextInfo);
begin
end;

procedure TGLBaseVBOManager.DoProgress(const progressTime: TProgressTimes);
begin
end;

procedure TGLBaseVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties;
  const rci: TRenderContextInfo);
begin
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLDynamicVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLDynamicVBOManager ------------------
// ------------------

constructor TGLDynamicVBOManager.Create(const AContext: TGLContext);
begin
  inherited Create(AContext);
  Usage := GL_DYNAMIC_DRAW;
  fBuilded := false;
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
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  InitClient(CurrentClient);
  CurrentClient.VertexHandle := fVertexHandle;
  CurrentClient.FirstVertex := 0;
  CurrentClient.FirstIndex := 0;
  CurrentClient.BuiltProp := BuiltProp;
  GLVBOMState := GLVBOM_OBJECT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[i] := GLVBOM_NODATA;
    ResetAttribArray(i);
  end;
  OneVertexDataSize := 0;
  ObjectVertexCount := 0;
  ObjectIndex := 0;
  HostIndexBuffer.Flush;
  Doubling := false;
  MaxIndexValue := 0;
  PrimitiveTypeCount := 0;
end;

procedure TGLDynamicVBOManager.EndObject(const rci: TRenderContextInfo);
var
  a, i, p, n, fullPartCount: Integer;
  Attr: T4ByteList;
  offset, size: LongWord;
  start, restPart, IndexStart, VertexStart: LongWord;
  pType: TGLEnum;
  uniform: GLInt;
  HostVertexMap: Pointer;
  HostIndexMap: Pointer;
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallEnd);
  GLVBOMState := GLVBOM_DEFAULT;

  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  // make sure no VAO is bound
  rci.GLStates.VertexArrayBinding := 0;
  // Vertex buffer managment
  if fVertexHandle.Handle = 0 then
  begin
    fVertexHandle.AllocateHandle;
    VertexBufferCapacity := LongWord(ObjectVertexCount) * OneVertexDataSize;
    fVertexHandle.BindBufferData(nil, VertexBufferCapacity, Usage);
  end
  else
  begin
    fVertexHandle.Bind;
    size := LongWord(ObjectVertexCount) * OneVertexDataSize;
    if VertexBufferCapacity < size then
    begin
      VertexBufferCapacity := RoundUpToPowerOf2(size);
      fVertexHandle.BufferData(nil, VertexBufferCapacity, Usage);
    end;
  end;

  if HostIndexBuffer.Count > 0 then
  begin
    // Index buffer managment
    if fIndexHandle.Handle = 0 then
    begin
      fIndexHandle.AllocateHandle;
      IndexBufferCapacity := LongWord(HostIndexBuffer.Count) * SizeOf(GLUInt);
      fIndexHandle.BindBufferData(HostIndexBuffer.List, IndexBufferCapacity,
        Usage);
    end
    else
    begin
      fIndexHandle.Bind;
      size := LongWord(HostIndexBuffer.Count) * SizeOf(GLUInt);
      if IndexBufferCapacity < size then
      begin
        IndexBufferCapacity := RoundUpToPowerOf2(size);
        fIndexHandle.BufferData(nil, IndexBufferCapacity, Usage);
      end;
      if vUseMappingForOftenBufferUpdate then
      begin
        HostIndexMap := fIndexHandle.MapBuffer(GL_WRITE_ONLY);
        Move(HostIndexBuffer.List^, PLongWord(HostIndexMap)^, size);
        fIndexHandle.UnmapBuffer;
      end
      else
        fIndexHandle.BufferSubData(0, size, HostIndexBuffer.List);
    end;
  end;

  // upload each attribute array one after another
  offset := 0;
  if vUseMappingForOftenBufferUpdate then
  begin
    if GL_ARB_map_buffer_range then
      HostVertexMap := fVertexHandle.MapBufferRange(0,
        LongWord(ObjectVertexCount) * OneVertexDataSize,
        GL_MAP_WRITE_BIT or
        GL_MAP_INVALIDATE_BUFFER_BIT or
        GL_MAP_UNSYNCHRONIZED_BIT or
        GL_MAP_FLUSH_EXPLICIT_BIT)
    else
      HostVertexMap := fVertexHandle.MapBuffer(GL_WRITE_ONLY);
  end
  else
    HostVertexMap := nil;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if CurrentClient.AttributesInUse[a] then
    begin
      Attr := AttributeArrays[a];
      size := Attr.Count * SizeOf(T4ByteData);

      if vUseMappingForOftenBufferUpdate then
        Move(Attr.List^, PByte(Integer(HostVertexMap) + Integer(offset))^, size)
      else
        fVertexHandle.BufferSubData(offset, size, Attr.List);

      glEnableVertexAttribArray(a);
      case CurrentClient.DataType[a] of
        GLVBOM_1F: glVertexAttribPointer(a, 1,
            GL_FLOAT, false, 0, pointer(Offset));

        GLVBOM_2F: glVertexAttribPointer(a, 2,
            GL_FLOAT, false, 0, pointer(Offset));

        GLVBOM_3F: glVertexAttribPointer(a, 3,
            GL_FLOAT, false, 0, pointer(Offset));

        GLVBOM_4F: glVertexAttribPointer(a, 4,
            GL_FLOAT, false, 0, pointer(Offset));

        GLVBOM_1I: glVertexAttribIPointer(a, 1, GL_INT, 0, pointer(Offset));

        GLVBOM_2I: glVertexAttribIPointer(a, 2, GL_INT, 0, pointer(Offset));

        GLVBOM_3I: glVertexAttribIPointer(a, 3, GL_INT, 0, pointer(Offset));

        GLVBOM_4I: glVertexAttribIPointer(a, 4, GL_INT, 0, pointer(Offset));

        GLVBOM_4UB: glVertexAttribPointer(a, 4,
            GL_UNSIGNED_BYTE, true, 0, pointer(Offset));
      else
        Assert(False, glsErrorEx + glsUnknownType);
      end;
      Offset := Offset + size;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  if vUseMappingForOftenBufferUpdate then
  begin
    if GL_ARB_map_buffer_range then
      fVertexHandle.Flush(0, LongWord(ObjectVertexCount) * OneVertexDataSize);
    fVertexHandle.UnmapBuffer;
  end;

  EnablePrimitiveRestart(rci);
  offset := 0;
  IndexStart := 0;
  VertexStart := 0;
  for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    if CurrentClient.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if (CurrentClient.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY)
        and (CurrentClient.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY)
          then
        if not GL_EXT_gpu_shader4 then
          continue;
      {: Primitives without adjacency should not be drawn with
         primitives with adjacency }
      if CurrentClient.BuiltProp.TriangleAdjacency then
        if not ((CurrentClient.PrimitiveType[p] =
          GLVBOM_TRIANGLE_STRIP_ADJACENCY)
          or (CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then
          continue;

      pType := cPrimitiveType[CurrentClient.PrimitiveType[p]];

      if CurrentClient.BuiltProp.InstancesNumber > 0 then
      begin
        if GL_EXT_draw_instanced then
        begin
          // HW instancing
          if CurrentClient.IndexCount[p] > 0 then
            glDrawElementsInstancedEXT(pType, CurrentClient.IndexCount[p],
              GL_UNSIGNED_INT, Pointer(offset),
              CurrentClient.BuiltProp.InstancesNumber)
          else
            glDrawArraysInstancedEXT(pType, VertexStart,
              CurrentClient.VertexCount[p],
              CurrentClient.BuiltProp.InstancesNumber);
        end
        else
        begin
          // Pseudo-Instancing
          uniform := glGetUniformLocationARB(
            FRenderingContext.GLStates.CurrentProgram,
            PGLChar(AnsiString(uniformInstanceID.Name)));
          if CurrentClient.IndexCount[p] > 0 then
          begin
            for i := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
            begin
              if uniform >= 0 then
                glUniform1iARB(uniform, i);
              glDrawElements(pType, CurrentClient.IndexCount[p],
                GL_UNSIGNED_INT,
                Pointer(offset));
            end;
          end
          else
          begin
            if uniform >= 0 then
              glUniform1iARB(uniform, 0);
            for i := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
            begin
              if uniform >= 0 then
                glUniform1iARB(uniform, i);
              glDrawArrays(pType, VertexStart, CurrentClient.VertexCount[p]);
            end;
            if uniform >= 0 then
              glUniform1iARB(uniform, 0);
          end;
        end;
      end
        // Simple drawing with frendly to vertex buffer mapping
      else if CurrentClient.IndexCount[p] = 0 then
      begin
        glDrawArrays(pType, VertexStart, CurrentClient.VertexCount[p]);
      end
      else if GL_EXT_draw_range_elements then
      begin
        fullPartCount := CurrentClient.IndexCount[p] div vMaxElementsIndices;
        restPart := CurrentClient.IndexCount[p] mod vMaxElementsIndices;
        for n := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(
            pType,
            IndexStart,
            IndexStart + vMaxElementsIndices - 1,
            vMaxElementsIndices,
            GL_UNSIGNED_INT,
            Pointer(offset));
          Inc(IndexStart, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(
            pType,
            IndexStart,
            IndexStart + restPart - 1,
            restPart,
            GL_UNSIGNED_INT,
            Pointer(offset));
          Inc(IndexStart, restPart);
        end;
      end
      else
        glDrawElements(pType, CurrentClient.IndexCount[p], GL_UNSIGNED_INT,
          Pointer(offset));

      Inc(offset, SizeOf(GLUInt) * CurrentClient.IndexCount[p]);
      Inc(VertexStart, CurrentClient.VertexCount[p]);
    end
    else
      break;
end;

procedure TGLDynamicVBOManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <=
    GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  GLVBOMState := GLVBOM_PRIMITIVE;
  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES,
      glsTooMachDiffPrim);
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount] := 0;
end;

procedure TGLDynamicVBOManager.EndPrimitives;
var
  Valid: Boolean;
  count: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsInvalidPrimType);
  GLVBOMState := GLVBOM_OBJECT;

  if Doubling then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := false;
  end;

  Valid := false;
  count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
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
  end;

  Assert(Valid, glsInvalidNumberOfVertex);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLStaticVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLStaticVBOManager ------------------
// ------------------

constructor TGLStaticVBOManager.Create(const AContext: TGLContext);
begin
  inherited Create(AContext);
  Usage := GL_STATIC_DRAW;
  HostVertexBuffer := T4ByteList.Create;
  ClientList := TList.Create;
  fBuilded := false;
  MaxIndexValue := 0;
end;

destructor TGLStaticVBOManager.Destroy;
var
  i: integer;
  Client: PGLRenderPacket;
begin
  HostVertexBuffer.Destroy;
  // Clear clients info
  for i := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[i];
    FreeClient(Client);
    Dispose(Client);
  end;
  ClientList.Destroy;
  inherited;
end;

procedure TGLStaticVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
var
  i: integer;
begin
  Assert(not fBuilded, glsCanNotRebuild);
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  Assert(BuiltProp.ID = 0, glsAlreadyDefined);

  InitClient(CurrentClient);
  CurrentClient.VertexHandle := fVertexHandle;
  CurrentClient.IndexHandle := fIndexHandle;
  CurrentClient.FirstVertex := HostVertexBuffer.Count * SizeOf(T4ByteData);
  CurrentClient.FirstIndex := HostIndexBuffer.Count;
  CurrentClient.BuiltProp := BuiltProp;

  GLVBOMState := GLVBOM_OBJECT;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[i] := GLVBOM_NODATA;
    ResetAttribArray(i);
  end;

  ObjectVertexCount := 0;
  ObjectIndex := 0;
  OneVertexDataSize := 0;
  Doubling := false;
  PrimitiveTypeCount := 0;
end;

procedure TGLStaticVBOManager.EndObject(const rci: TRenderContextInfo);
var
  a: integer;
  Attr: T4ByteList;
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallEnd);
  GLVBOMState := GLVBOM_DEFAULT;

  if ObjectIndex = 0 then
  begin
    // No one vertex not recorded
    FreeClient(CurrentClient);
    Dispose(CurrentClient);
    exit;
  end;

  // upload each attribute array one after another
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if CurrentClient.AttributesInUse[a] then
    begin
      Attr := AttributeArrays[a];
      HostVertexBuffer.Add(Attr);
      CurrentClient.DataSize[a] := Attr.Count * SizeOf(T4ByteData);
    end;

  ClientList.Add(CurrentClient);
  CurrentClient.BuiltProp.ID := Longword(ClientList.Count);
  CurrentClient := nil;
end;

procedure TGLStaticVBOManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  GLVBOMState := GLVBOM_PRIMITIVE;
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <=
    GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES,
      'BeginPrimitives: Too mach different primitive types in one object.');
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount] := 0;
end;

procedure TGLStaticVBOManager.EndPrimitives;
var
  Valid: Boolean;
  Index: LongWord;
  p: Integer;
  IndicesList, adjIndicesList: TLongWordList;
  start, count: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEndPrim);
  GLVBOMState := GLVBOM_OBJECT;

  Index := HostIndexBuffer.Items[HostIndexBuffer.Count - 1];
  if Doubling or (Index = RestartIndex) then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := false;
  end;

  Valid := false;
  count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
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
  end;

  Assert(Valid, glsInvalidNumberOfVertex);

  // Make trinagles with adjancency
  if CurrentClient.BuiltProp.TriangleAdjacency then
  begin
    start := CurrentClient.FirstIndex;
    for p := 0 to PrimitiveTypeCount - 1 do
      start := start + CurrentClient.IndexCount[p];
    count := CurrentClient.IndexCount[PrimitiveTypeCount];

    if CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP
      then
    begin
      // Convert strips to independent triangles
      IndicesList := ConvertStripToList(
        @HostIndexBuffer.List[start], count, RestartIndex);
      if Assigned(IndicesList) then
      begin
        HostIndexBuffer.Count := start;
        count := IndicesList.Count;
        HostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), count);
        CurrentClient.IndexCount[PrimitiveTypeCount] := count;
        CurrentClient.PrimitiveType[PrimitiveTypeCount] := GLVBOM_TRIANGLES;
        IndicesList.Free;
      end;
    end
    else if CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_FAN
      then
    begin
      // Convert fans to independent triangles
      IndicesList := ConvertFansToList(
        @HostIndexBuffer.List[start], count, RestartIndex);
      if Assigned(IndicesList) then
      begin
        HostIndexBuffer.Count := start;
        count := IndicesList.Count;
        HostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), count);
        CurrentClient.IndexCount[PrimitiveTypeCount] := count;
        CurrentClient.PrimitiveType[PrimitiveTypeCount] := GLVBOM_TRIANGLES;
        IndicesList.Free;
      end;
    end;

    if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_TRIANGLES then
      exit;

    if HostIndexBuffer.Capacity < Integer(start + 2 * count +
      vEdgeInfoReserveSize) then
      HostIndexBuffer.Capacity :=
        Integer(start + 2 * count + vEdgeInfoReserveSize);

    adjIndicesList := MakeTriangleAdjacencyList(
      @HostIndexBuffer.List[start],
      count,
      @AttributeArrays[0].List[0]);

    if Assigned(adjIndicesList) then
    begin
      HostIndexBuffer.Count := start;
      HostIndexBuffer.AddLongWords(PLongWord(adjIndicesList.List),
        adjIndicesList.Count);
      CurrentClient.IndexCount[PrimitiveTypeCount] := adjIndicesList.Count;
      CurrentClient.PrimitiveType[PrimitiveTypeCount] :=
        GLVBOM_TRIANGLES_ADJACENCY;
      ObjectIndex := HostIndexBuffer.Items[HostIndexBuffer.Count - 1] + 1;
      adjIndicesList.Free;
    end;
  end;

end;

procedure TGLStaticVBOManager.BuildBuffer(const rci: TRenderContextInfo);
var
  a, c, i: Integer;
  offset: LongWord;
  Client: PGLRenderPacket;
  tempIndexBuffer: Pointer;
begin
  if fBuilded then
    exit;
  if (HostVertexBuffer.Count = 0) or (HostIndexBuffer.Count = 0) then
    exit;

  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  fVertexHandle.AllocateHandle;
  fVertexHandle.Bind;
  // Upload all vertices data in one buffer
  fVertexHandle.BufferData(HostVertexBuffer.List, HostVertexBuffer.Count *
    SizeOf(T4ByteData), Usage);
  // Upload all indices data in one buffer
  fIndexHandle.AllocateHandle;
  fIndexHandle.Bind;
  // Adjust index type according its number
  indexType := GL_UNSIGNED_INT;
  tempIndexBuffer := nil;
  RestartIndex := $FFFFFF;
  if MaxIndexValue + 1 < $10000 then
  begin
    if MaxIndexValue + 1 < $100 then
    begin
      GetMem(tempIndexBuffer, HostIndexBuffer.Count);
      for i := 0 to HostIndexBuffer.Count - 1 do
        PByteArray(tempIndexBuffer)[i] := Byte(HostIndexBuffer.Items[i]);
      fIndexHandle.BufferData(tempIndexBuffer, HostIndexBuffer.Count, Usage);
      indexType := GL_UNSIGNED_BYTE;
      RestartIndex := $FF;
    end
    else
    begin
      GetMem(tempIndexBuffer, SizeOf(Word) * HostIndexBuffer.Count);
      for i := 0 to HostIndexBuffer.Count - 1 do
        PWordVector(tempIndexBuffer)[i] := Word(HostIndexBuffer.Items[i]);
      fIndexHandle.BufferData(tempIndexBuffer, SizeOf(Word) *
        HostIndexBuffer.Count, Usage);
      indexType := GL_UNSIGNED_SHORT;
      RestartIndex := $FFFF;
    end;
  end
  else
    fIndexHandle.BufferData(HostIndexBuffer.List, SizeOf(Integer) *
      HostIndexBuffer.Count, Usage);

  for c := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[c];
    // Uniting all the states and buffers in one vertex array object
    Client.ArrayHandle := TGLVertexArrayHandle.CreateAndAllocate;
    Client.ArrayHandle.Bind;
    fVertexHandle.Bind;
    offset := Client.FirstVertex;
    // Setup Client Attributes pointer
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    begin
      if Client.AttributesInUse[a] then
      begin
        glEnableVertexAttribArray(a);
        case Client.DataType[a] of
          GLVBOM_1F: glVertexAttribPointer(a, 1,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_2F: glVertexAttribPointer(a, 2,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_3F: glVertexAttribPointer(a, 3,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_4F: glVertexAttribPointer(a, 4,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_1I: glVertexAttribIPointer(a, 1, GL_INT, 0, pointer(Offset));

          GLVBOM_2I: glVertexAttribIPointer(a, 2, GL_INT, 0, pointer(Offset));

          GLVBOM_3I: glVertexAttribIPointer(a, 3, GL_INT, 0, pointer(Offset));

          GLVBOM_4I: glVertexAttribIPointer(a, 4, GL_INT, 0, pointer(Offset));

          GLVBOM_4UB: glVertexAttribPointer(a, 4,
              GL_UNSIGNED_BYTE, true, 0, pointer(Offset));
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end; // of case
        Offset := Offset + Client.DataSize[a];
      end
      else
        glDisableVertexAttribArray(a)
    end;

    Client.RelativeSize :=
      (offset - Client.FirstVertex) / (HostVertexBuffer.Count *
      SizeOf(T4ByteData));

    // Add index buffer to array
    fIndexHandle.Bind;

    Client.ArrayHandle.UnBind;
  end; // for c

  fBuilded := true;

  if Assigned(tempIndexBuffer) then
    FreeMem(tempIndexBuffer);
  HostVertexBuffer.Clear;
  HostIndexBuffer.Clear;
end;

procedure TGLStaticVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties;
  const rci: TRenderContextInfo);
var
  p, i, n, fullPartCount: Integer;
  Client: PGLRenderPacket;
  pType: TGLEnum;
  offset, typeSize: LongWord;
  uniform: GLInt;
  start, restPart: LongWord;
begin
  if not fBuilded then
    exit;

  Client := ClientList.Items[BuiltProp.ID - 1];
  offset := Client.FirstIndex;
  typeSize := SizeOf(GLUByte);
  case indexType of
    GL_UNSIGNED_SHORT: typeSize := SizeOf(GLUShort);
    GL_UNSIGNED_INT: typeSize := SizeOf(GLUInt);
  end;
  offset := offset * typeSize;

  Client.LastTimeWhenRendered := vCurrentTime;
  Client.ArrayHandle.Bind;
  EnablePrimitiveRestart(rci);

  start := 0;
  for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    if Client.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if (Client.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY)
        and (Client.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
        if not GL_EXT_gpu_shader4 then
          continue;
      {: Primitives without adjacency should not be drawn with
         primitives with adjacency }
      if BuiltProp.TriangleAdjacency then
        if not ((Client.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY)
          or (Client.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then
          continue;

      pType := cPrimitiveType[Client.PrimitiveType[p]];

      if Client.BuiltProp.InstancesNumber > 0 then
      begin
        if GL_EXT_draw_instanced then
          // HW instancing
          glDrawElementsInstancedEXT(pType, Client.IndexCount[p], indexType,
            Pointer(offset), Client.BuiltProp.InstancesNumber)
        else
        begin
          // Pseudo-Instancing
          if FRenderingContext.GLStates.CurrentProgram > 0 then
          begin
            uniform := glGetUniformLocationARB(
              FRenderingContext.GLStates.CurrentProgram,
              PGLChar(AnsiString(uniformInstanceID.Name)));
            for i := 0 to Client.BuiltProp.InstancesNumber - 1 do
            begin
              if uniform >= 0 then
                glUniform1iARB(uniform, i);
              glDrawElements(pType, Client.IndexCount[p], indexType,
                Pointer(offset));
            end;
            if uniform >= 0 then
              glUniform1iARB(uniform, 0);
          end;
        end;
      end
        // Simple drawing with pre-TnL cashing
      else if GL_EXT_draw_range_elements then
      begin
        fullPartCount := Client.IndexCount[p] div vMaxElementsIndices;
        restPart := Client.IndexCount[p] mod vMaxElementsIndices;
        for n := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(pType, start, start + vMaxElementsIndices - 1,
            vMaxElementsIndices, indexType, Pointer(offset));
          Inc(start, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(pType, start, start + restPart - 1, restPart,
            indexType, Pointer(offset));
          Inc(start, restPart);
        end;
      end;
      Inc(offset, Client.IndexCount[p] * typeSize);

    end
    else
      break;

  Client.ArrayHandle.UnBind;
end;

function TGLStaticVBOManager.UsageStatistic(const TimeInterval: Double): Single;
var
  c: Integer;
  Client: PGLRenderPacket;
  portion: Single;
begin
  portion := 0;
  for c := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[c];
    if Client.LastTimeWhenRendered > vCurrentTime - TimeInterval then
      portion := portion + Client.RelativeSize;
  end;
  Result := portion;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLStreamVBOManager'}{$ENDIF}
// ------------------
// ------------------ TGLStreamVBOManager ------------------
// ------------------

constructor TGLStreamVBOManager.Create(const AContext: TGLContext);
begin
  inherited Create(AContext);
  Usage := GL_STREAM_DRAW;
  fBuilded := false;
  ClientList := TList.Create;
end;

destructor TGLStreamVBOManager.Destroy;
var
  i: integer;
  Client: PGLRenderPacket;
begin
  // Clear clients info
  for i := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[i];
    FreeClient(Client);
    Dispose(Client);
  end;
  ClientList.Destroy;
  inherited;
end;

procedure TGLStreamVBOManager.BeginObject(const BuiltProp: TGLBuiltProperties);
var
  i: integer;
begin
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  if BuiltProp.ID <> 0 then
    CurrentClient := ClientList.Items[BuiltProp.ID - 1];

  InitClient(CurrentClient);
  CurrentClient.FirstVertex := 0;
  CurrentClient.FirstIndex := 0;
  CurrentClient.BuiltProp := BuiltProp;
  GLVBOMState := GLVBOM_OBJECT;

  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[i] := GLVBOM_NODATA;
    ResetAttribArray(i);
  end;
  OneVertexDataSize := 0;
  ObjectVertexCount := 0;
  ObjectIndex := 0;
  HostIndexBuffer.Flush;
  Doubling := false;
  MaxIndexValue := 0;
  PrimitiveTypeCount := 0;
end;

procedure TGLStreamVBOManager.EndObject(const rci: TRenderContextInfo);
var
  a: Integer;
  Attr: T4ByteList;
  offset, size: LongWord;
  HostVertexMap: Pointer;
  HostIndexMap: Pointer;
  BuiltProp: TGLBuiltProperties;
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallEnd);
  GLVBOMState := GLVBOM_DEFAULT;

  if ObjectIndex = 0 then
    exit;

  if not Assigned(CurrentClient.ArrayHandle) then
  begin
    CurrentClient.ArrayHandle := TGLVertexArrayHandle.CreateAndAllocate;
    CurrentClient.ArrayHandle.Bind;
    CurrentClient.VertexHandle := TGLVBOArrayBufferHandle.Create;
    CurrentClient.IndexHandle := TGLVBOElementArrayHandle.Create;
  end;

  // Vertex buffer managment
  if CurrentClient.VertexHandle.Handle = 0 then
  begin
    CurrentClient.VertexHandle.AllocateHandle;
    CurrentClient.BuiltProp.VertexBufferCapacity :=
      LongWord(ObjectVertexCount) * OneVertexDataSize;
    CurrentClient.VertexHandle.BindBufferData(nil,
      CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.VertexHandle.Bind;
    size := LongWord(ObjectVertexCount) * OneVertexDataSize;
    if CurrentClient.BuiltProp.VertexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.VertexHandle.BufferData(nil,
        CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;
  // Index buffer managment
  if CurrentClient.IndexHandle.Handle = 0 then
  begin
    CurrentClient.IndexHandle.AllocateHandle;
    CurrentClient.BuiltProp.IndexBufferCapacity :=
      LongWord(HostIndexBuffer.Count) * SizeOf(GLUInt);
    CurrentClient.IndexHandle.BindBufferData(HostIndexBuffer.List,
      CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.IndexHandle.Bind;
    size := LongWord(HostIndexBuffer.Count) * SizeOf(GLUInt);
    if CurrentClient.BuiltProp.IndexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.IndexHandle.BufferData(nil,
        CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    end;
    if vUseMappingForOftenBufferUpdate then
    begin
      HostIndexMap := fIndexHandle.MapBuffer(GL_WRITE_ONLY);
      Move(HostIndexBuffer.List^, PLongWord(HostIndexMap)^, size);
      CurrentClient.IndexHandle.UnmapBuffer;
    end
    else
      CurrentClient.IndexHandle.BufferSubData(0, size, HostIndexBuffer.List);
  end;

  // upload each attribute array one after another
  offset := 0;
  if vUseMappingForOftenBufferUpdate then
    HostVertexMap := CurrentClient.VertexHandle.MapBuffer(GL_WRITE_ONLY)
  else
    HostVertexMap := nil;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if CurrentClient.AttributesInUse[a] then
    begin
      Attr := AttributeArrays[a];
      size := Attr.Count * SizeOf(T4ByteData);

      if vUseMappingForOftenBufferUpdate then
        Move(Attr.List^, PByte(Integer(HostVertexMap) + Integer(offset))^, size)
      else
        CurrentClient.VertexHandle.BufferSubData(offset, size, Attr.List);

      glEnableVertexAttribArray(a);
      if a > 0 then
        case CurrentClient.DataType[a] of
          GLVBOM_1F: glVertexAttribPointer(a, 1,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_2F: glVertexAttribPointer(a, 2,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_3F: glVertexAttribPointer(a, 3,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_4F: glVertexAttribPointer(a, 4,
              GL_FLOAT, false, 0, pointer(Offset));

          GLVBOM_1I: glVertexAttribIPointer(a, 1, GL_INT, 0, pointer(Offset));

          GLVBOM_2I: glVertexAttribIPointer(a, 2, GL_INT, 0, pointer(Offset));

          GLVBOM_3I: glVertexAttribIPointer(a, 3, GL_INT, 0, pointer(Offset));

          GLVBOM_4I: glVertexAttribIPointer(a, 4, GL_INT, 0, pointer(Offset));

          GLVBOM_4UB: glVertexAttribPointer(a, 4,
              GL_UNSIGNED_BYTE, true, 0, pointer(Offset));
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
      Offset := Offset + size;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  if vUseMappingForOftenBufferUpdate then
    CurrentClient.VertexHandle.UnmapBuffer;
  // set the pointer for position last
  glVertexAttribPointer(0, 3, GL_FLOAT, false, 0, nil);
  CurrentClient.ArrayHandle.UnBind;

  // add and setup client
  BuiltProp := CurrentClient.BuiltProp;
  if BuiltProp.ID = 0 then
  begin
    ClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := Longword(ClientList.Count);
  end;
  CurrentClient := nil;
  RenderClient(BuiltProp, rci);
end;

procedure TGLStreamVBOManager.EmitVertices(VertexNumber: LongWord; Indexed:
  Boolean);
var
  a: Integer;
  offset, size: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEmit);
  Assert(VertexNumber > 0, glsInvalidNumberOfVertex);
  GLVBOMState := GLVBOM_DEFAULT;

  if not Assigned(CurrentClient.ArrayHandle) then
  begin
    CurrentClient.ArrayHandle := TGLVertexArrayHandle.CreateAndAllocate;
    CurrentClient.ArrayHandle.Bind;
    CurrentClient.VertexHandle := TGLVBOArrayBufferHandle.Create;
    CurrentClient.IndexHandle := TGLVBOElementArrayHandle.Create;
  end;

  // Vertex buffer managment
  if CurrentClient.VertexHandle.Handle = 0 then
  begin
    CurrentClient.VertexHandle.AllocateHandle;
    CurrentClient.BuiltProp.VertexBufferCapacity :=
      VertexNumber * OneVertexDataSize;
    CurrentClient.VertexHandle.BindBufferData(nil,
      CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.VertexHandle.Bind;
    size := VertexNumber * OneVertexDataSize;
    if CurrentClient.BuiltProp.VertexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.VertexHandle.BufferData(nil,
        CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;

  if Indexed then
  begin
    // Index buffer managment
    if CurrentClient.IndexHandle.Handle = 0 then
    begin
      CurrentClient.IndexHandle.AllocateHandle;
      CurrentClient.BuiltProp.IndexBufferCapacity :=
        VertexNumber * SizeOf(GLUInt);
      CurrentClient.IndexHandle.BindBufferData(nil,
        CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    end
    else
    begin
      CurrentClient.IndexHandle.Bind;
      size := VertexNumber * SizeOf(GLUInt);
      if CurrentClient.BuiltProp.IndexBufferCapacity < size then
      begin
        CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
        CurrentClient.IndexHandle.BufferData(nil,
          CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
      end;
    end;
  end
  else if CurrentClient.IndexHandle.Handle <> 0 then
    CurrentClient.IndexHandle.DestroyHandle;

  // upload each attribute array one after another
  offset := 0;
  size := 0;
  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if CurrentClient.AttributesInUse[a] then
    begin
      glEnableVertexAttribArray(a);
      case CurrentClient.DataType[a] of
        GLVBOM_1F:
          begin
            glVertexAttribPointer(a, 1,
              GL_FLOAT, false, 0, pointer(Offset));
            size := SizeOf(GLFloat);
          end;

        GLVBOM_2F:
          begin
            glVertexAttribPointer(a, 2,
              GL_FLOAT, false, 0, pointer(Offset));
            size := 2 * SizeOf(GLFloat);
          end;

        GLVBOM_3F:
          begin
            glVertexAttribPointer(a, 3,
              GL_FLOAT, false, 0, pointer(Offset));
            size := 3 * SizeOf(GLFloat);
          end;

        GLVBOM_4F:
          begin
            glVertexAttribPointer(a, 4,
              GL_FLOAT, false, 0, pointer(Offset));
            size := 4 * SizeOf(GLFloat);
          end;

        GLVBOM_1I:
          begin
            glVertexAttribIPointer(a, 1, GL_INT, 0, pointer(Offset));
            size := SizeOf(GLInt);
          end;

        GLVBOM_2I:
          begin
            glVertexAttribIPointer(a, 2, GL_INT, 0, pointer(Offset));
            size := 2 * SizeOf(GLInt);
          end;

        GLVBOM_3I:
          begin
            glVertexAttribIPointer(a, 3, GL_INT, 0, pointer(Offset));
            size := 3 * SizeOf(GLInt);
          end;

        GLVBOM_4I:
          begin
            glVertexAttribIPointer(a, 4, GL_INT, 0, pointer(Offset));
            size := 4 * SizeOf(GLInt);
          end;

        GLVBOM_4UB:
          begin
            glVertexAttribPointer(a, 4,
              GL_UNSIGNED_BYTE, true, 0, pointer(Offset));
            size := 4 * SizeOf(GLUByte);
          end
      else
        Assert(False, glsErrorEx + glsUnknownType);
      end;
      Offset := Offset + size * VertexNumber;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  CurrentClient.ArrayHandle.UnBind;

  // add and setup client
  if CurrentClient.BuiltProp.ID = 0 then
  begin
    ClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := Longword(ClientList.Count);
  end;
  CurrentClient.VertexCount[PrimitiveTypeCount] := VertexNumber;
  CurrentClient.BuiltProp.ID := Longword(ClientList.Count);
  CurrentClient := nil;
end;

procedure TGLStreamVBOManager.RenderClient(const BuiltProp: TGLBuiltProperties;
  const rci: TRenderContextInfo);
var
  p, i, n, fullPartCount: Integer;
  Client: PGLRenderPacket;
  pType: TGLEnum;
  offset, typeSize: LongWord;
  uniform: GLInt;
  IndexStart, VertexStart, restPart: LongWord;
begin
  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  Client := ClientList.Items[BuiltProp.ID - 1];
  offset := Client.FirstIndex;
  typeSize := SizeOf(GLUInt);
  //  case indexType of
  //    GL_UNSIGNED_SHORT: typeSize := SizeOf(GLUShort);
  //    GL_UNSIGNED_INT: typeSize := SizeOf(GLUInt);
  //  end;
  offset := offset * typeSize;

  //  Client.LastTimeWhenRendered := fCurrentTime;
  Client.ArrayHandle.Bind;
  EnablePrimitiveRestart(rci);

  IndexStart := 0;
  VertexStart := 0;
  for p := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    if Client.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if (Client.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY)
        and (Client.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
        if not GL_EXT_gpu_shader4 then
          continue;
      {: Primitives without adjacency should not be drawn with
         primitives with adjacency }
      if BuiltProp.TriangleAdjacency then
        if not ((Client.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY)
          or (Client.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then
          continue;

      pType := cPrimitiveType[Client.PrimitiveType[p]];

      if Client.BuiltProp.InstancesNumber > 0 then
      begin
        if GL_EXT_draw_instanced then
        begin
          // HW instancing
          if Client.IndexCount[p] > 0 then
            glDrawElementsInstancedEXT(pType, Client.IndexCount[p],
              GL_UNSIGNED_INT,
              Pointer(offset), Client.BuiltProp.InstancesNumber)
          else
            glDrawArraysInstancedEXT(pType, VertexStart, Client.VertexCount[p],
              Client.BuiltProp.InstancesNumber);
        end
        else
        begin
          // Pseudo-Instancing
          if FRenderingContext.GLStates.CurrentProgram > 0 then
          begin
            uniform := glGetUniformLocationARB(
              FRenderingContext.GLStates.CurrentProgram,
              PGLChar(AnsiString(uniformInstanceID.Name)));
            if Client.IndexCount[p] > 0 then
            begin
              for i := 0 to Client.BuiltProp.InstancesNumber - 1 do
              begin
                if uniform >= 0 then
                  glUniform1iARB(uniform, i);
                glDrawElements(pType, Client.IndexCount[p], GL_UNSIGNED_INT,
                  Pointer(offset));
              end;
              if uniform >= 0 then
                glUniform1iARB(uniform, 0);
            end
            else
            begin
              for i := 0 to Client.BuiltProp.InstancesNumber - 1 do
              begin
                if uniform >= 0 then
                  glUniform1iARB(uniform, i);
                glDrawArrays(pType, VertexStart, Client.VertexCount[p]);
              end;
              if uniform >= 0 then
                glUniform1iARB(uniform, 0);
            end;
          end;
        end;
      end
        // Simple drawing with frendly to vertex buffer mapping
      else if Client.IndexCount[p] = 0 then
      begin
        glDrawArrays(pType, VertexStart, Client.VertexCount[p]);
      end
      else if GL_EXT_draw_range_elements then
      begin
        fullPartCount := Client.IndexCount[p] div vMaxElementsIndices;
        restPart := Client.IndexCount[p] mod vMaxElementsIndices;
        for n := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(pType, IndexStart,
            IndexStart + vMaxElementsIndices - 1,
            vMaxElementsIndices, GL_UNSIGNED_INT, Pointer(offset));
          Inc(IndexStart, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(pType, IndexStart,
            IndexStart + restPart - 1, restPart,
            GL_UNSIGNED_INT, Pointer(offset));
          Inc(IndexStart, restPart);
        end;
      end;
      Inc(offset, Client.IndexCount[p] * typeSize);
      Inc(VertexStart, Client.VertexCount[p]);
    end
    else
      break;

  Client.ArrayHandle.UnBind;
end;

procedure TGLStreamVBOManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <=
    GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  GLVBOMState := GLVBOM_PRIMITIVE;
  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES,
      glsTooMachDiffPrim);
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount] := 0;
end;

procedure TGLStreamVBOManager.EndPrimitives;
var
  Valid: Boolean;
  count: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsInvalidPrimType);
  GLVBOMState := GLVBOM_OBJECT;

  if Doubling then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := false;
  end;

  Valid := false;
  count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
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
  end;

  Assert(Valid, glsInvalidNumberOfVertex);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

  vMaxElementsIndices := 0;
{$IFNDEF GLS_MULTITHREAD}
  vVBOManagerList := TList.Create;
{$ELSE}
  vVBOManagerList := TThreadList.Create;
{$ENDIF}

finalization

  FreeVBOManagers;

end.

