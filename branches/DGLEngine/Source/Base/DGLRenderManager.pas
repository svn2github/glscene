//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLRenderManagers
  @HTML (
  <p>
  Base unit to drawing geometry data with Vertex Array Object (VAO) and Vertex Buffer Objects.(VBO) </p>
  Require OpenGL 3.3.<br>
  Try to not change buffer usage during runtime because static
  and stream data not removed from video memory until application running.</p>
  <p>
  <b>History : </b><font size=-1><ul>
    <li>24/12/15 - JD - Creation  - Base on codes from Yar and Phantom, Thanks</li>
  </ul></font></p>

   )
}
unit DGLRenderManager;

interface

{$I DGLEngine.inc}

uses
  Classes,
  // DGLE
  DGLSLog,
  DGLBaseClasses, DGLTypes, dglOpenGL, DGLContext, DGLContextHandles, DGLRenderContextInfo, DGLState,
  DGLVectorTypes, DGLVectorLists;


const
  GLVBOM_MAX_DIFFERENT_PRIMITIVES = 8;

type
  // ****************************************************************************************
  TDGLBaseRenderManager      = class;
  TDGLBaseRenderManagerClass = class of TDGLBaseRenderManager;
  TDGLBuiltProperties        = class;

  // ****************************************************************************************
  // TGLRenderPacket
  { @HTML ( Information about object graphic data (something like DIP) }
  TGLRenderPacket = record
    VertexHandle: TDGLVBOArrayBufferHandle;
    IndexHandle: TDGLVBOElementArrayHandle;
    ArrayHandle: TDGLVertexArrayHandle;
    FirstVertex: LongWord;
    FirstIndex: LongWord;
    VertexCount: array [0 .. GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    IndexCount: array [0 .. GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of LongWord;
    PrimitiveType: array [0 .. GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1] of TGLVBOMEnum;

    AttributeID: array [0 .. GLS_VERTEX_ATTR_NUM - 1] of Integer;
    AttributesInUse: array [0 .. GLS_VERTEX_ATTR_NUM - 1] of Boolean;
    DataSize: array [0 .. GLS_VERTEX_ATTR_NUM - 1] of LongWord;
    DataType: array [0 .. GLS_VERTEX_ATTR_NUM - 1] of TGLVBOMEnum;
    BuiltProp: TDGLBuiltProperties;
    LastTimeWhenRendered: Double;
    RelativeSize: Single;
  end;
  PGLRenderPacket = ^TGLRenderPacket;

  // ****************************************************************************************
  // TDGLBuiltProperties
  //
  TDGLBuiltProperties = class(TPersistent)
  private
    { Private declarations }
    Owner:              TComponent;
    ID:                 LongWord;
    FUsage:             TGLBufferUsage;
    FVertexWelding:     Boolean;
    FTriangleAdjacency: Boolean;
    FInstancesNumber:   Integer;
    FOwnerNotifyChange: TNotifyEvent;
    procedure SetUsage(const Value: TGLBufferUsage);
    procedure SetVertexWelding(const Value: Boolean);
    procedure SetTriangleAdjacency(const Value: Boolean);
    procedure SetInstancesNumber(Value: Integer);
    function GetVertexBufferHandle: GLUInt;
    function GetIndexBufferHandle: GLUInt;
    function GetVertexNumber: LongWord;
    function GetManager: TDGLBaseRenderManager;
  protected
    { Protected Declarations }
    VertexBufferCapacity: LongWord;
    IndexBufferCapacity:  LongWord;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    property VertexBufferHandle: GLUInt read GetVertexBufferHandle;
    property IndexBufferHandle: GLUInt read GetIndexBufferHandle;

    property Manager: TDGLBaseRenderManager read GetManager;
    property OwnerNotifyChange: TNotifyEvent read FOwnerNotifyChange write FOwnerNotifyChange;
    property VertexNumber: LongWord read GetVertexNumber;
  published
    { Published Declarations }
    property Usage:             TGLBufferUsage read FUsage write SetUsage default buStatic;
    property VertexWelding:     Boolean read FVertexWelding write SetVertexWelding default True;
    property TriangleAdjacency: Boolean read FTriangleAdjacency write SetTriangleAdjacency default False;
    property InstancesNumber:   Integer read FInstancesNumber write SetInstancesNumber default 0;
  end;

  // ****************************************************************************************
  // TDGLBaseRenderManager
  //
  TDGLBaseRenderManager = class
  private
    { Private declarations }
    FRenderingContext:  TDGLContext;
    FVertexHandle:      TDGLVBOArrayBufferHandle;
    FIndexHandle:       TDGLVBOElementArrayHandle;
    FBuilded:           Boolean;
    Usage:              LongWord;
    CurrentClient:      PGLRenderPacket;
    GLVBOMState:        TGLVBOMState;
    AttributeArrays:    array [0 .. GLS_VERTEX_ATTR_NUM - 1] of TDGL4ByteList;
    CurrentValue:       array [0 .. GLS_VERTEX_ATTR_NUM - 1, 0 .. 3] of TDGL4ByteData;
    OneVertexDataSize:  LongWord;
    ObjectVertexCount:  Integer; // From BeginObject to EndObject
    HostIndexBuffer:    TDGLLongWordList;
    ObjectIndex:        LongWord;
    Doubling:           Boolean;
    RestartIndex:       GLUInt;
    MaxIndexValue:      LongWord;
    PrimitiveTypeCount: Integer;
    WasUsedList:        Boolean;

    procedure ResetAttribArray(idx: Integer);
    procedure InitClient(var AClient: PGLRenderPacket);
    procedure FreeClient(const AClient: PGLRenderPacket);
    procedure EnablePrimitiveRestart(const rci: TRenderContextInfo);
    function GetAttributeLocation(const AttrName: TDGLSLAttribute; eType: TGLVBOMEnum): GLint;
  public
    { Public Declarations }
    constructor Create(const AContext: TDGLContext); virtual;
    destructor Destroy; override;

    { : Begins storing a piece of geometry }
    procedure BeginObject(const BuiltProp: TDGLBuiltProperties); virtual; abstract;
    { : Ends a piece of geometry. }
    procedure EndObject(const rci: TRenderContextInfo); virtual; abstract;
    { : Begins gathering information about the given type of primitives.
      An object can only consist of a set of primitives of the same type. }
    procedure BeginPrimitives(eType: TGLVBOMEnum); virtual; abstract;
    { : Ends gathering information about the primitive. }
    procedure EndPrimitives; virtual; abstract;
    { : Specifies a new value for the attribute with the given name. }
    procedure Attribute1f(const AttrName: TDGLSLAttribute; a1: GLfloat);
    procedure Attribute2f(const AttrName: TDGLSLAttribute; a1, a2: GLfloat); overload;
    procedure Attribute2f(const AttrName: TDGLSLAttribute; const a: TVector2f); overload;
    procedure Attribute3f(const AttrName: TDGLSLAttribute; a1, a2, a3: GLfloat); overload;
    procedure Attribute3f(const AttrName: TDGLSLAttribute; const a: TVector3f); overload;
    procedure Attribute4f(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLfloat); overload;
    procedure Attribute4f(const AttrName: TDGLSLAttribute; const a: TVector4f); overload;
    procedure Attribute1i(const AttrName: TDGLSLAttribute; a1: GLint);
    procedure Attribute2i(const AttrName: TDGLSLAttribute; a1, a2: GLint); overload;
    procedure Attribute2i(const AttrName: TDGLSLAttribute; const a: TVector2i); overload;
    procedure Attribute3i(const AttrName: TDGLSLAttribute; a1, a2, a3: GLint); overload;
    procedure Attribute3i(const AttrName: TDGLSLAttribute; const a: TVector3i); overload;
    procedure Attribute4i(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLint); overload;
    procedure Attribute4i(const AttrName: TDGLSLAttribute; const a: TVector4i); overload;
    procedure Attribute4ub(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLubyte);
    { : Takes a full list of attribute values,
      but does not determine its type, so you must use AttributeXX
      between BeginObject and BeginPrimitives }
    procedure AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLSingleList); overload;
    procedure AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLIntegerList); overload;
    { : Count of list should be a multiple of four }
    procedure AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLByteList); overload;
    { : Specifies a new vertex of a primitive. }
    procedure EmitVertex;
    { : Provides a feature to create an empty buffer
      from the task markup attributes.
      This is useful for transformfeedback or copying operations. }
    procedure EmitVertices(VertexNumber: LongWord; Indexed: Boolean); dynamic;
    { : Restart strip by GL_NV_primitive_restart or degenerate primitive }
    procedure RestartStrip;
    { : If during the storing geometry popup an error
      use discard to remove last stored object's data }
    procedure Discard;
    { : Execute build stage }
    procedure BuildBuffer(const rci: TRenderContextInfo); dynamic;
    { : Store time }
    procedure DoProgress(const progressTime: TProgressTimes); dynamic;
    { : Rendering sender }
    procedure RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo); dynamic;
    { : Return true if buffer is uploaded to video memory.
      Dynamic VBO always return false }
    property IsBuilded: Boolean read FBuilded;
    { : Rendering context to which it belongs manager. }
    property RenderingContext: TDGLContext read FRenderingContext;
  end;

  // ****************************************************************************************
  // TDGLStaticRenderManager
  //
  TDGLStaticRenderManager = class(TDGLBaseRenderManager)
  private
    { Private declarations }
    ClientList:       TList;
    HostVertexBuffer: TDGL4ByteList;
    indexType:        GLenum; // UByte, UShort or UInt
  public
    { Public Declarations }
    constructor Create(const AContext: TDGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TDGLBuiltProperties); override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
    procedure BuildBuffer(const rci: TRenderContextInfo); override;
    procedure RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo); override;
    { : Returns the portion of the buffer that is used during the time interval. }
    function UsageStatistic(const TimeInterval: Double): Single;
  end;

  // ****************************************************************************************
  // TDGLDynamicRenderManager
  //
  TDGLDynamicRenderManager = class(TDGLBaseRenderManager)
  private
    { Private declarations }
    VertexBufferCapacity: LongWord;
    IndexBufferCapacity:  LongWord;
  public
    { Public Declarations }
    constructor Create(const AContext: TDGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TDGLBuiltProperties); override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
  end;

  // ****************************************************************************************
  // TDGLStreamRenderManager
  //
  TDGLStreamRenderManager = class(TDGLBaseRenderManager)
  private
    { Private declarations }
    ClientList: TList;
  public
    { Public Declarations }
    constructor Create(const AContext: TDGLContext); override;
    destructor Destroy; override;
    procedure BeginObject(const BuiltProp: TDGLBuiltProperties); override;
    procedure EndObject(const rci: TRenderContextInfo); override;
    procedure BeginPrimitives(eType: TGLVBOMEnum); override;
    procedure EndPrimitives; override;
    procedure EmitVertices(VertexNumber: LongWord; Indexed: Boolean); override;
    procedure RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo); override;
  end;

// ****************************************************************************************

var
  vUseMappingForOftenBufferUpdate: Boolean = False;

function StaticVBOManager: TDGLStaticRenderManager;
function DynamicVBOManager: TDGLDynamicRenderManager;
function StreamVBOManager: TDGLStreamRenderManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  DGLVectorMaths, DGLUtils, DGLresStrings, DGLMeshUtils;

const
  FourByteZero: TDGL4ByteData  = (Int: (Value: 0));
  cPrimitiveType: array [GLVBOM_TRIANGLES .. GLVBOM_TRIANGLE_STRIP_ADJACENCY] of GLenum = (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_POINTS, GL_LINES, GL_LINE_LOOP, GL_LINE_STRIP,  GL_LINES_ADJACENCY,
    GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY);

var
  vMaxElementsIndices: GLUInt;

  vVBOManagerList: TThreadList;

threadvar
  vCurrentTime: Double;

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

function GetVBOManager(const AClass: TDGLBaseRenderManagerClass): TDGLBaseRenderManager;
var
  I:  Integer;
  RC: TDGLContext;
begin
  RC := CurrentDGLContext;
  if RC = nil then
    DGLSLogger.LogWarning(glsNoActiveRC);
  // Find suitable manager

  with vVBOManagerList.LockList do
  begin
    for I := 0 to Count - 1 do
    begin
      Result := TDGLBaseRenderManager(Items[I]);
      if (Result is AClass) and ((Result.RenderingContext = RC) or (RC = nil)) then
        exit;
    end;
    // Create new
    Result := AClass.Create(RC);
    Add(Result);
  end;

end;

function StaticVBOManager: TDGLStaticRenderManager;
begin
  Result := GetVBOManager(TDGLStaticRenderManager) as TDGLStaticRenderManager;
end;

function DynamicVBOManager: TDGLDynamicRenderManager;
begin
  Result := GetVBOManager(TDGLDynamicRenderManager) as TDGLDynamicRenderManager;
end;

function StreamVBOManager: TDGLStreamRenderManager;
begin
  Result := GetVBOManager(TDGLStreamRenderManager) as TDGLStreamRenderManager;
end;

procedure FreeVBOManagers;
var
  I:        Integer;
  vManager: TDGLBaseRenderManager;
begin

  with vVBOManagerList.LockList do
  begin
    for I := 0 to Count - 1 do
    begin
      vManager := TDGLBaseRenderManager(Items[I]);
      vManager.Free;
    end;
  end;

  vVBOManagerList.Free;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBuiltProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLBuiltProperties'}{$ENDIF}

constructor TDGLBuiltProperties.Create(AOwner: TComponent);
begin
  inherited Create;
  Owner              := AOwner;
  ID                 := 0;
  FUsage             := buStatic;
  FVertexWelding     := True;
  FTriangleAdjacency := False;
  FInstancesNumber   := 0;
end;

procedure TDGLBuiltProperties.SetUsage(const Value: TGLBufferUsage);
begin
  if Value <> FUsage then
  begin
    FUsage := Value;
    ID     := 0;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

function TDGLBuiltProperties.GetManager: TDGLBaseRenderManager;
begin
  if (FUsage = buStatic) and (csDesigning in Owner.ComponentState) then
    Result := DynamicVBOManager
  else
    case FUsage of
      buStatic:
        Result := StaticVBOManager;
      buDynamic:
        Result := DynamicVBOManager;
      buStream:
        Result := StreamVBOManager;
    else
      begin
        Assert(False, glsErrorEx + glsUnknownType);
        Result := DynamicVBOManager;
      end;
    end;
end;

procedure TDGLBuiltProperties.SetVertexWelding(const Value: Boolean);
begin
  if Value <> FVertexWelding then
  begin
    FVertexWelding := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

procedure TDGLBuiltProperties.SetTriangleAdjacency(const Value: Boolean);
begin
  if Value <> FTriangleAdjacency then
  begin
    FTriangleAdjacency := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

procedure TDGLBuiltProperties.SetInstancesNumber(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FInstancesNumber then
  begin
    FInstancesNumber := Value;
    if Assigned(FOwnerNotifyChange) then
      FOwnerNotifyChange(Self);
  end;
end;

function TDGLBuiltProperties.GetVertexBufferHandle: GLUInt;
var
  Client: PGLRenderPacket;
begin
  Result := 0;
  case FUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TDGLStaticRenderManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.VertexHandle) then
          Result := Client.VertexHandle.Handle;
      end;
    buDynamic:
      Result := TDGLDynamicRenderManager(Manager).FVertexHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TDGLStreamRenderManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.VertexHandle) then
          Result := Client.VertexHandle.Handle;
      end;
  end;
end;

function TDGLBuiltProperties.GetIndexBufferHandle: GLUInt;
var
  Client: PGLRenderPacket;
begin
  Result := 0;
  case FUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TDGLStaticRenderManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.IndexHandle) then
          Result := Client.IndexHandle.Handle;
      end;
    buDynamic:
      Result := TDGLDynamicRenderManager(Manager).FIndexHandle.Handle;
    buStream:
      if ID > 0 then
      begin
        Client := TDGLStreamRenderManager(Manager).ClientList.Items[ID - 1];
        if Assigned(Client.IndexHandle) then
          Result := Client.IndexHandle.Handle;
      end;
  end;
end;

function TDGLBuiltProperties.GetVertexNumber: LongWord;
var
  Client: PGLRenderPacket;
  p:      Integer;
begin
  Result := 0;
  case FUsage of
    buStatic:
      if ID > 0 then
      begin
        Client := TDGLStaticRenderManager(Manager).ClientList.Items[ID - 1];
        for p  := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
          Inc(Result, Client.VertexCount[p]);
      end;
    buStream:
      if ID > 0 then
      begin
        Client := TDGLStreamRenderManager(Manager).ClientList.Items[ID - 1];
        for p  := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
          Inc(Result, Client.VertexCount[p]);
      end;
  end;
end;

procedure TDGLBuiltProperties.Assign(Source: TPersistent);
begin
  if Source is TDGLBuiltProperties then
  begin
    Usage             := TDGLBuiltProperties(Source).FUsage;
    VertexWelding     := TDGLBuiltProperties(Source).FVertexWelding;
    TriangleAdjacency := TDGLBuiltProperties(Source).FTriangleAdjacency;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseRenderManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseRenderManager'}{$ENDIF}

constructor TDGLBaseRenderManager.Create(const AContext: TDGLContext);
var
  I: Integer;
begin
  FRenderingContext                    := AContext;
  FVertexHandle                        := TDGLVBOArrayBufferHandle.Create;
  FIndexHandle                         := TDGLVBOElementArrayHandle.Create;
  HostIndexBuffer                      := TDGLLongWordList.Create;
  HostIndexBuffer.SetCountResetsMemory := False;
  GLVBOMState                          := GLVBOM_DEFAULT;
  OneVertexDataSize                    := 0;
  WasUsedList                          := False;
  for I                                := 0 to High(AttributeArrays) do
    AttributeArrays[I]                 := TDGL4ByteList.Create;
  RestartIndex                         := $FFFFFFFF;
end;

destructor TDGLBaseRenderManager.Destroy;
var
  I: Integer;
begin
  FVertexHandle.Destroy;
  FIndexHandle.Destroy;
  for I := 0 to High(AttributeArrays) do
    AttributeArrays[I].Destroy;
  HostIndexBuffer.Destroy;
end;

procedure TDGLBaseRenderManager.Discard;
var
  I, p: Integer;
begin
  GLVBOMState := GLVBOM_DEFAULT;
  for I       := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    ResetAttribArray(I);

  if Assigned(CurrentClient) then
    for p   := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
      for I := 0 to Integer(CurrentClient.IndexCount[p]) - 1 do
        HostIndexBuffer.Pop;

  if Self is TDGLStaticRenderManager then
  begin
    FreeClient(CurrentClient);
    Dispose(CurrentClient);
  end;
  CurrentClient := nil;
end;

procedure TDGLBaseRenderManager.ResetAttribArray(idx: Integer);
begin
  Assert(idx < GLS_VERTEX_ATTR_NUM, 'ResetAttribArray: Array index out of bound.');
  AttributeArrays[idx].Flush;
  CurrentValue[idx][0] := FourByteZero;
  CurrentValue[idx][1] := FourByteZero;
  CurrentValue[idx][2] := FourByteZero;
  CurrentValue[idx][3] := FourByteZero;
end;

procedure TDGLBaseRenderManager.InitClient(var AClient: PGLRenderPacket);
var
  I: Integer;
begin
  if not Assigned(AClient) then
  begin
    New(AClient);
    AClient.VertexHandle         := nil;
    AClient.IndexHandle          := nil;
    AClient.ArrayHandle          := nil;
    AClient.LastTimeWhenRendered := 0;
    AClient.RelativeSize         := 0;
  end;
  for I := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
  begin
    AClient.PrimitiveType[I] := GLVBOM_NOPRIMITIVE;
    AClient.IndexCount[I]    := 0;
    AClient.VertexCount[I]   := 0;
  end;
  for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    AClient.AttributesInUse[I] := False;
    AClient.AttributeID[I]     := 0;
    AClient.DataType[0]        := GLVBOM_NODATA;
  end;
end;

procedure TDGLBaseRenderManager.FreeClient(const AClient: PGLRenderPacket);
begin
  if Assigned(AClient) then
  begin
    AClient.ArrayHandle.Free;
    AClient.ArrayHandle := nil;
    if Self is TDGLStreamRenderManager then
    begin
      AClient.VertexHandle.Free;
      AClient.VertexHandle := nil;
      AClient.IndexHandle.Free;
      AClient.IndexHandle := nil;
    end;
  end;
end;

procedure TDGLBaseRenderManager.EnablePrimitiveRestart(const rci: TRenderContextInfo);
begin
  // if GL_NV_primitive_restart then
  // begin
  // glEnableVariantClientState(GL_PRIMITIVE_RESTART_NV);
  // rci.GLStates.PrimitiveRestartIndex := RestartIndex;
  // end;
end;

procedure TDGLBaseRenderManager.EmitVertex;
var
  a, v, I, etalon, Count: Integer;
  AA:                     TDGL4ByteList;
  dt:                     TGLVBOMEnum;
  weld:                   Boolean;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEmit);

  if WasUsedList then
  begin
    // Emit List of Vertex
    etalon := -1;
    Count  := 0;
    for a  := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      if CurrentClient.AttributesInUse[a] then
      begin
        AA := AttributeArrays[a];

        case CurrentClient.DataType[a] of
          GLVBOM_1I, GLVBOM_1F:
            Count := AA.Count;
          GLVBOM_2I, GLVBOM_2F:
            Count := AA.Count div 2;
          GLVBOM_3I, GLVBOM_3F:
            Count := AA.Count div 3;
          GLVBOM_4UB, GLVBOM_4I, GLVBOM_4F:
            Count := AA.Count div 4;
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
        if etalon < 0 then
        begin
          etalon := Count;
          continue;
        end
        else
          Assert(etalon = Count, 'EmitVertex: Lists of attributes do not match the length');
      end;

     for i := 0 to etalon - 1 do
     begin
     HostIndexBuffer.Push(ObjectIndex);
     Inc(ObjectIndex);
     end;

     Inc(CurrentClient.IndexCount[PrimitiveTypeCount], etalon);
    Inc(CurrentClient.VertexCount[PrimitiveTypeCount], etalon);
    Inc(ObjectVertexCount, etalon);
    if MaxIndexValue < ObjectIndex then
      MaxIndexValue := ObjectIndex;
    WasUsedList     := False;
  end
  // Emit single vertex
  else
  begin
    I    := -1;
    weld := False;
    if CurrentClient.BuiltProp.VertexWelding then
    begin
      for v := 0 to ObjectVertexCount - 1 do
      begin
        weld  := True;
        for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
          if CurrentClient.AttributesInUse[a] then
          begin
            AA := AttributeArrays[a];
            dt := CurrentClient.DataType[a];
            if (dt = GLVBOM_1I) or (dt = GLVBOM_1F) or (dt = GLVBOM_4UB) then
            begin
              if AA.Items[v].Int.Value <> CurrentValue[a][0].Int.Value then
              begin
                weld := False;
                break;
              end;
            end
            else if (dt = GLVBOM_2I) or (dt = GLVBOM_2F) then
            begin
              if (AA.Items[v * 2 + 0].Int.Value <> CurrentValue[a][0].Int.Value) or (AA.Items[v * 2 + 1].Int.Value <> CurrentValue[a][1].Int.Value) then
              begin
                weld := False;
                break;
              end;
            end
            else if (dt = GLVBOM_3I) or (dt = GLVBOM_3F) then
            begin
              if (AA.Items[v * 3 + 0].Int.Value <> CurrentValue[a][0].Int.Value) or (AA.Items[v * 3 + 1].Int.Value <> CurrentValue[a][1].Int.Value) or (AA.Items[v * 3 + 2].Int.Value <> CurrentValue[a][2].Int.Value) then
              begin
                weld := False;
                break;
              end;
            end
            else if (dt = GLVBOM_4I) or (dt = GLVBOM_4F) then
            begin
              if (AA.Items[v * 3 + 0].Int.Value <> CurrentValue[a][0].Int.Value) or (AA.Items[v * 3 + 1].Int.Value <> CurrentValue[a][1].Int.Value) or (AA.Items[v * 3 + 2].Int.Value <> CurrentValue[a][2].Int.Value) or
                (AA.Items[v * 3 + 3].Int.Value <> CurrentValue[a][3].Int.Value) then
              begin
                weld := False;
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
          if (dt = GLVBOM_1I) or (dt = GLVBOM_1F) or (dt = GLVBOM_4UB) then
          begin
            AA.Push(CurrentValue[a][0]);
          end
          else if (dt = GLVBOM_2I) or (dt = GLVBOM_2F) then
          begin
            AA.Push(CurrentValue[a][0]);
            AA.Push(CurrentValue[a][1]);
          end
          else if (dt = GLVBOM_3I) or (dt = GLVBOM_3F) then
          begin
            AA.Push(CurrentValue[a][0]);
            AA.Push(CurrentValue[a][1]);
            AA.Push(CurrentValue[a][2]);
          end
          else if (dt = GLVBOM_4I) or (dt = GLVBOM_4F) then
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
        Doubling := False;
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
        Doubling := False;
      end;
    end;
  end;
end;

procedure TDGLBaseRenderManager.RestartStrip;
begin
  if WasUsedList then
    exit;
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, 'RestartStrip: This function must be called between Begin / End.');
  Assert(not Doubling, 'RestartStrip: Excessive call.');
  Assert((CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP) or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP) or (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_FAN) or
    (CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_LINE_STRIP), 'RestartStrip: This primitive type does not need to restart.');

  if GL_NV_primitive_restart then
  begin
    HostIndexBuffer.Push(RestartIndex);
  end
  else
  begin
    // Create degenerate primitive
    HostIndexBuffer.Push(ObjectIndex - 1);
    Doubling := True;
  end;
  Inc(CurrentClient.IndexCount[PrimitiveTypeCount]);
end;

function TDGLBaseRenderManager.GetAttributeLocation(const AttrName: TDGLSLAttribute; eType: TGLVBOMEnum): GLint;
var
  a:      Integer;
  Finded: Boolean;
  prog:   GLUInt;
  nm:     PGLChar;
begin
  // if the attribute has never been used before
  Result := -1;
  Assert(AttrName.ID > 0, 'GetAttributeLocation: attribute '+AttrName.Name+' not registered');
  Finded := False;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if CurrentClient.AttributeID[a] = AttrName.ID then
    begin
      Result := a;
      Finded := True;
      break;
    end;

  if not Finded then
  begin
    if GLVBOMState <> GLVBOM_OBJECT then
      exit;
    prog   := FRenderingContext.GLStates.CurrentProgram;
    nm     := PGLChar(TGLString(AttrName.Name));
    Result := glGetAttribLocation(prog, nm);

    Assert(Result < GLS_VERTEX_ATTR_NUM, 'GetAttributeLocation: Location is out of bound.');

    if Result <> -1 then
    begin
      CurrentClient.AttributeID[Result]     := AttrName.ID;
      CurrentClient.DataType[Result]        := eType;
      CurrentClient.AttributesInUse[Result] := True;

      if (eType = GLVBOM_1F) or (eType = GLVBOM_1I) or (eType = GLVBOM_4UB) then
        Inc(OneVertexDataSize, 1 * sizeof(TDGL4ByteData))
      else if (eType = GLVBOM_2F) or (eType = GLVBOM_2I) then
        Inc(OneVertexDataSize, 2 * sizeof(TDGL4ByteData))
      else if (eType = GLVBOM_3F) or (eType = GLVBOM_3I) then
        Inc(OneVertexDataSize, 3 * sizeof(TDGL4ByteData))
      else if (eType = GLVBOM_4F) or (eType = GLVBOM_4I) then
        Inc(OneVertexDataSize, 4 * sizeof(TDGL4ByteData));
    end
    else
      exit;
  end;

  if eType <> GLVBOM_CUSTOM_DATA then
    Assert(CurrentClient.DataType[Result] = eType, 'GetAttributeLocation: An attribute was used with different type than previously.');
end;

procedure TDGLBaseRenderManager.Attribute1f(const AttrName: TDGLSLAttribute; a1: GLfloat);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_1F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a1;

end;

procedure TDGLBaseRenderManager.Attribute2f(const AttrName: TDGLSLAttribute; a1, a2: GLfloat);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
end;

procedure TDGLBaseRenderManager.Attribute2f(const AttrName: TDGLSLAttribute; const a: TVector2f);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a.V[0];
  CurrentValue[Location, 1].Float.Value := a.V[1];
end;

procedure TDGLBaseRenderManager.Attribute3f(const AttrName: TDGLSLAttribute; a1, a2, a3: GLfloat);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
  CurrentValue[Location, 2].Float.Value := a3;
end;

procedure TDGLBaseRenderManager.Attribute3f(const AttrName: TDGLSLAttribute; const a: TVector3f);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a.V[0];
  CurrentValue[Location, 1].Float.Value := a.V[1];
  CurrentValue[Location, 2].Float.Value := a.V[2];
end;

procedure TDGLBaseRenderManager.Attribute4f(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLfloat);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a1;
  CurrentValue[Location, 1].Float.Value := a2;
  CurrentValue[Location, 2].Float.Value := a3;
  CurrentValue[Location, 3].Float.Value := a4;
end;

procedure TDGLBaseRenderManager.Attribute4f(const AttrName: TDGLSLAttribute; const a: TVector4f);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4F);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Float.Value := a.V[0];
  CurrentValue[Location, 1].Float.Value := a.V[1];
  CurrentValue[Location, 2].Float.Value := a.V[2];
  CurrentValue[Location, 3].Float.Value := a.V[3];
end;

procedure TDGLBaseRenderManager.Attribute1i(const AttrName: TDGLSLAttribute; a1: GLint);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_1I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a1;
end;

procedure TDGLBaseRenderManager.Attribute2i(const AttrName: TDGLSLAttribute; a1, a2: GLint);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
end;

procedure TDGLBaseRenderManager.Attribute2i(const AttrName: TDGLSLAttribute; const a: TVector2i);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_2I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a.V[0];
  CurrentValue[Location, 1].Int.Value := a.V[1];
end;

procedure TDGLBaseRenderManager.Attribute3i(const AttrName: TDGLSLAttribute; a1, a2, a3: GLint);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
  CurrentValue[Location, 2].Int.Value := a3;
end;

procedure TDGLBaseRenderManager.Attribute3i(const AttrName: TDGLSLAttribute; const a: TVector3i);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_3I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a.V[0];
  CurrentValue[Location, 1].Int.Value := a.V[1];
  CurrentValue[Location, 2].Int.Value := a.V[2];
end;

procedure TDGLBaseRenderManager.Attribute4i(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLint);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a1;
  CurrentValue[Location, 1].Int.Value := a2;
  CurrentValue[Location, 2].Int.Value := a3;
  CurrentValue[Location, 3].Int.Value := a4;
end;

procedure TDGLBaseRenderManager.Attribute4i(const AttrName: TDGLSLAttribute; const a: TVector4i);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4I);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Int.Value := a.V[0];
  CurrentValue[Location, 1].Int.Value := a.V[1];
  CurrentValue[Location, 2].Int.Value := a.V[2];
  CurrentValue[Location, 3].Int.Value := a.V[3];
end;

procedure TDGLBaseRenderManager.Attribute4ub(const AttrName: TDGLSLAttribute; a1, a2, a3, a4: GLubyte);
var
  Location: Integer;
begin
  Assert(not WasUsedList, glsBadAttrCombination);
  Location := GetAttributeLocation(AttrName, GLVBOM_4UB);
  if Location = -1 then exit;
  CurrentValue[Location, 0].Bytes.Value[0] := a1;
  CurrentValue[Location, 0].Bytes.Value[1] := a2;
  CurrentValue[Location, 0].Bytes.Value[2] := a3;
  CurrentValue[Location, 0].Bytes.Value[3] := a4;
end;

procedure TDGLBaseRenderManager.AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLSingleList);
var
  Location: Integer;
  Valid:    Boolean;
  AA:       TDGL4ByteList;
  Last:     Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then exit;
  Valid := False;
  case CurrentClient.DataType[Location] of
    GLVBOM_1F:
      Valid := True;
    GLVBOM_2F:
      Valid := (AList.Count mod 2 = 0);
    GLVBOM_3F:
      Valid := (AList.Count mod 3 = 0);
    GLVBOM_4F:
      Valid := (AList.Count mod 4 = 0);
  end;
  Assert(Valid, glsWrongAttrType);

  AA       := AttributeArrays[Location];
  Last     := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * sizeof(TDGL4ByteData));
  WasUsedList := True;
end;

procedure TDGLBaseRenderManager.AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLIntegerList);
var
  Location: Integer;
  Valid:    Boolean;
  AA:       TDGL4ByteList;
  Last:     Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then exit;
  Valid := False;
  case CurrentClient.DataType[Location] of
    GLVBOM_1I:
      Valid := True;
    GLVBOM_2I:
      Valid := (AList.Count mod 2 = 0);
    GLVBOM_3I:
      Valid := (AList.Count mod 3 = 0);
    GLVBOM_4I:
      Valid := (AList.Count mod 4 = 0);
  end;
  Assert(Valid, glsWrongAttrType);

  AA       := AttributeArrays[Location];
  Last     := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count * sizeof(TDGL4ByteData));
  WasUsedList := True;
end;

procedure TDGLBaseRenderManager.AttributeList(const AttrName: TDGLSLAttribute; const AList: TDGLByteList);
var
  Location: Integer;
  AA:       TDGL4ByteList;
  Last:     Integer;
begin
  Location := GetAttributeLocation(AttrName, GLVBOM_CUSTOM_DATA);
  if Location = -1 then exit;

  Assert((CurrentClient.DataType[Location] = GLVBOM_4UB) and (AList.Count mod 4 = 0), glsWrongAttrType);
  AA       := AttributeArrays[Location];
  Last     := AA.Count;
  AA.Count := Last + AList.Count;
  System.Move(AList.List^, AA.List[Last], AList.Count);
  WasUsedList := True;
end;

procedure TDGLBaseRenderManager.EmitVertices(VertexNumber: LongWord; Indexed: Boolean);
begin
end;

procedure TDGLBaseRenderManager.BuildBuffer(const rci: TRenderContextInfo);
begin
end;

procedure TDGLBaseRenderManager.DoProgress(const progressTime: TProgressTimes);
begin
end;

procedure TDGLBaseRenderManager.RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo);
begin
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLDynamicRenderManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLDynamicRenderManager'}{$ENDIF}

constructor TDGLDynamicRenderManager.Create(const AContext: TDGLContext);
begin
  inherited Create(AContext);
  Usage    := GL_DYNAMIC_DRAW;
  FBuilded := False;
end;

destructor TDGLDynamicRenderManager.Destroy;
begin
  FreeClient(CurrentClient);
  Dispose(CurrentClient);
  inherited;
end;

procedure TDGLDynamicRenderManager.BeginObject(const BuiltProp: TDGLBuiltProperties);
var
  I: Integer;
begin
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  InitClient(CurrentClient);
  CurrentClient.VertexHandle := FVertexHandle;
  CurrentClient.FirstVertex  := 0;
  CurrentClient.FirstIndex   := 0;
  CurrentClient.BuiltProp    := BuiltProp;
  GLVBOMState                := GLVBOM_OBJECT;
  for I                      := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[I] := GLVBOM_NODATA;
    ResetAttribArray(I);
  end;
  OneVertexDataSize := 0;
  ObjectVertexCount := 0;
  ObjectIndex       := 0;
  HostIndexBuffer.Flush;
  Doubling           := False;
  MaxIndexValue      := 0;
  PrimitiveTypeCount := 0;
end;

procedure TDGLDynamicRenderManager.EndObject(const rci: TRenderContextInfo);
var
  a, I, p, n, fullPartCount:                Integer;
  Attr:                                     TDGL4ByteList;
  offset, size:                             LongWord;
  //start,
  restPart, IndexStart, VertexStart: LongWord;
  pType:                                    TGLEnum;
  uniform:                                  GLint;
  HostVertexMap:                            Pointer;
  HostIndexMap:                             Pointer;
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallEnd);
  GLVBOMState := GLVBOM_DEFAULT;

  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  // make sure no VAO is bound
  rci.GLStates.VertexArrayBinding := 0;
  // Vertex buffer managment
  if FVertexHandle.Handle = 0 then
  begin
    FVertexHandle.AllocateHandle;
    VertexBufferCapacity := LongWord(ObjectVertexCount) * OneVertexDataSize;
    FVertexHandle.BindBufferData(nil, VertexBufferCapacity, Usage);
  end
  else
  begin
    FVertexHandle.Bind;
    size := LongWord(ObjectVertexCount) * OneVertexDataSize;
    if VertexBufferCapacity < size then
    begin
      VertexBufferCapacity := RoundUpToPowerOf2(size);
      FVertexHandle.BufferData(nil, VertexBufferCapacity, Usage);
    end;
  end;

  if HostIndexBuffer.Count > 0 then
  begin
    // Index buffer managment
    if FIndexHandle.Handle = 0 then
    begin
      FIndexHandle.AllocateHandle;
      IndexBufferCapacity := LongWord(HostIndexBuffer.Count) * sizeof(GLUInt);
      FIndexHandle.BindBufferData(HostIndexBuffer.List, IndexBufferCapacity, Usage);
    end
    else
    begin
      FIndexHandle.Bind;
      size := LongWord(HostIndexBuffer.Count) * sizeof(GLUInt);
      if IndexBufferCapacity < size then
      begin
        IndexBufferCapacity := RoundUpToPowerOf2(size);
        FIndexHandle.BufferData(nil, IndexBufferCapacity, Usage);
      end;
      if vUseMappingForOftenBufferUpdate then
      begin
        HostIndexMap := FIndexHandle.MapBuffer(GL_WRITE_ONLY);
        Move(HostIndexBuffer.List^, PLongWord(HostIndexMap)^, size);
        FIndexHandle.UnmapBuffer;
      end
      else
        FIndexHandle.BufferSubData(0, size, HostIndexBuffer.List);
    end;
  end;

  // upload each attribute array one after another
  offset := 0;
  if vUseMappingForOftenBufferUpdate then
  begin
    if GL_ARB_map_buffer_range then
      HostVertexMap := FVertexHandle.MapBufferRange(0, LongWord(ObjectVertexCount) * OneVertexDataSize, GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_BUFFER_BIT or GL_MAP_UNSYNCHRONIZED_BIT or GL_MAP_FLUSH_EXPLICIT_BIT)
    else
      HostVertexMap := FVertexHandle.MapBuffer(GL_WRITE_ONLY);
  end
  else
    HostVertexMap := nil;

  for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if CurrentClient.AttributesInUse[a] then
    begin
      Attr := AttributeArrays[a];
      size := Attr.Count * sizeof(TDGL4ByteData);

      if vUseMappingForOftenBufferUpdate then
        Move(Attr.List^, PByte(Integer(HostVertexMap) + Integer(offset))^, size)
      else
        FVertexHandle.BufferSubData(offset, size, Attr.List);

      glEnableVertexAttribArray(a);
      case CurrentClient.DataType[a] of
        GLVBOM_1F:
          glVertexAttribPointer(a, 1, GL_FLOAT, False, 0, Pointer(offset));

        GLVBOM_2F:
          glVertexAttribPointer(a, 2, GL_FLOAT, False, 0, Pointer(offset));

        GLVBOM_3F:
          glVertexAttribPointer(a, 3, GL_FLOAT, False, 0, Pointer(offset));

        GLVBOM_4F:
          glVertexAttribPointer(a, 4, GL_FLOAT, False, 0, Pointer(offset));

        GLVBOM_1I:
          glVertexAttribIPointer(a, 1, GL_INT, 0, Pointer(offset));

        GLVBOM_2I:
          glVertexAttribIPointer(a, 2, GL_INT, 0, Pointer(offset));

        GLVBOM_3I:
          glVertexAttribIPointer(a, 3, GL_INT, 0, Pointer(offset));

        GLVBOM_4I:
          glVertexAttribIPointer(a, 4, GL_INT, 0, Pointer(offset));

        GLVBOM_4UB:
          glVertexAttribPointer(a, 4, GL_UNSIGNED_BYTE, True, 0, Pointer(offset));
      else
        Assert(False, glsErrorEx + glsUnknownType);
      end;
      offset := offset + size;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  if vUseMappingForOftenBufferUpdate then
  begin
    if GL_ARB_map_buffer_range then
      FVertexHandle.Flush(0, LongWord(ObjectVertexCount) * OneVertexDataSize);
    FVertexHandle.UnmapBuffer;
  end;

  EnablePrimitiveRestart(rci);
  offset      := 0;
  IndexStart  := 0;
  VertexStart := 0;
  for p       := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    if CurrentClient.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if (CurrentClient.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY) and (CurrentClient.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
        if not dglCheckExtension('GL_EXT_gpu_shader4') then continue;
      { : Primitives without adjacency should not be drawn with
        primitives with adjacency }
      if CurrentClient.BuiltProp.TriangleAdjacency then
        if not((CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY) or (CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then continue;

      pType := cPrimitiveType[CurrentClient.PrimitiveType[p]];

      if CurrentClient.BuiltProp.InstancesNumber > 0 then
      begin
        if GL_EXT_draw_instanced then
        begin
          // HW instancing
          if CurrentClient.IndexCount[p] > 0 then
            glDrawElementsInstancedEXT(pType, CurrentClient.IndexCount[p], GL_UNSIGNED_INT, Pointer(offset), CurrentClient.BuiltProp.InstancesNumber)
          else
            glDrawArraysInstancedEXT(pType, VertexStart, CurrentClient.VertexCount[p], CurrentClient.BuiltProp.InstancesNumber);
        end
        else
        begin
          // // Pseudo-Instancing
           uniform := glGetUniformLocationARB(FRenderingContext.GLStates.CurrentProgram, PGLChar(AnsiString(FRenderingContext.GLStates.uniformInstanceID.Name)));
          if CurrentClient.IndexCount[p] > 0 then
          begin
            for I := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
            begin
               if uniform >= 0 then glUniform1iARB(uniform, I);
              glDrawElements(pType, CurrentClient.IndexCount[p], GL_UNSIGNED_INT, Pointer(offset));
            end;
          end
          else
          begin
             if uniform >= 0 then glUniform1iARB(uniform, 0);
            for I := 0 to CurrentClient.BuiltProp.InstancesNumber - 1 do
            begin
              if uniform >= 0 then glUniform1iARB(uniform, I);
              glDrawArrays(pType, VertexStart, CurrentClient.VertexCount[p]);
            end;
             if uniform >= 0 then glUniform1iARB(uniform, 0);
          end;
        end;
      end
      // Simple drawing with frendly to vertex buffer mapping
      else if CurrentClient.IndexCount[p] = 0 then
      begin
        glDrawArrays(pType, VertexStart, CurrentClient.VertexCount[p]);
      end
      else if dglCheckExtension('GL_EXT_draw_range_elements') then
      begin
        fullPartCount := CurrentClient.IndexCount[p] div vMaxElementsIndices;
        restPart      := CurrentClient.IndexCount[p] mod vMaxElementsIndices;
        for n         := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(pType, IndexStart, IndexStart + vMaxElementsIndices - 1, vMaxElementsIndices, GL_UNSIGNED_INT, Pointer(offset));
          Inc(IndexStart, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(pType, IndexStart, IndexStart + restPart - 1, restPart, GL_UNSIGNED_INT, Pointer(offset));
          Inc(IndexStart, restPart);
        end;
      end
      else
        glDrawElements(pType, CurrentClient.IndexCount[p], GL_UNSIGNED_INT, Pointer(offset));

      Inc(offset, sizeof(GLUInt) * CurrentClient.IndexCount[p]);
      Inc(VertexStart, CurrentClient.VertexCount[p]);
    end
    else
      break;
end;

procedure TDGLDynamicRenderManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <= GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  GLVBOMState := GLVBOM_PRIMITIVE;
  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES, glsTooMachDiffPrim);
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount]   := 0;
end;

procedure TDGLDynamicRenderManager.EndPrimitives;
var
  Valid: Boolean;
  Count: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsInvalidPrimType);
  GLVBOMState := GLVBOM_OBJECT;

  if Doubling then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := False;
  end;

  Valid := False;
  Count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
    GLVBOM_TRIANGLES:
      Valid := (Count mod 3 = 0) and (Count > 2);
    GLVBOM_TRIANGLE_STRIP, GLVBOM_TRIANGLE_FAN:
      Valid := Count > 2;
    // GLVBOM_QUADS:
    // Valid := (Count mod 4 = 0) and (Count > 3);
    // GLVBOM_QUAD_STRIP:
    // Valid := Count > 3;
    GLVBOM_POINTS:
      Valid := Count > 0;
    GLVBOM_LINES:
      Valid := (Count mod 2 = 0) and (Count > 1);
    GLVBOM_LINE_STRIP, GLVBOM_LINE_LOOP:
      Valid := Count > 2;
    // GLVBOM_POLYGON:
    // Valid := Count > 2;
    GLVBOM_LINES_ADJACENCY:
      Valid := (Count mod 4 = 0) and (Count > 3);
    GLVBOM_LINE_STRIP_ADJACENCY:
      Valid := Count > 4;
    GLVBOM_TRIANGLES_ADJACENCY:
      Valid := (Count mod 6 = 0) and (Count > 5);
    GLVBOM_TRIANGLE_STRIP_ADJACENCY:
      Valid := Count > 4;
  end;

  Assert(Valid, glsInvalidNumberOfVertex);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLStaticRenderManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLStaticRenderManager'}{$ENDIF}

constructor TDGLStaticRenderManager.Create(const AContext: TDGLContext);
begin
  inherited Create(AContext);
  Usage            := GL_STATIC_DRAW;
  HostVertexBuffer := TDGL4ByteList.Create;
  ClientList       := TList.Create;
  FBuilded         := False;
  MaxIndexValue    := 0;
end;

destructor TDGLStaticRenderManager.Destroy;
var
  I:      Integer;
  Client: PGLRenderPacket;
begin
  HostVertexBuffer.Destroy;
  // Clear clients info
  for I := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[I];
    FreeClient(Client);
    Dispose(Client);
  end;
  ClientList.Destroy;
  inherited;
end;

procedure TDGLStaticRenderManager.BeginObject(const BuiltProp: TDGLBuiltProperties);
var
  I: Integer;
begin
  DGLSLogger.LogInfo('TDGLStaticRenderManager : BeginObject');
  Assert(not FBuilded, glsCanNotRebuild);
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
//  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  Assert(BuiltProp.ID = 0, glsAlreadyDefined);

  InitClient(CurrentClient);
  CurrentClient.VertexHandle := FVertexHandle;
  CurrentClient.IndexHandle  := FIndexHandle;
  CurrentClient.FirstVertex  := HostVertexBuffer.Count * sizeof(TDGL4ByteData);
  CurrentClient.FirstIndex   := HostIndexBuffer.Count;
  CurrentClient.BuiltProp    := BuiltProp;

  GLVBOMState := GLVBOM_OBJECT;
  for I       := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[I] := GLVBOM_NODATA;
    ResetAttribArray(I);
  end;

  ObjectVertexCount  := 0;
  ObjectIndex        := 0;
  OneVertexDataSize  := 0;
  Doubling           := False;
  PrimitiveTypeCount := 0;
end;

procedure TDGLStaticRenderManager.EndObject(const rci: TRenderContextInfo);
var
  a:    Integer;
  Attr: TDGL4ByteList;
begin
  DGLSLogger.LogInfo('TDGLStaticRenderManager : EndObject');
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
      CurrentClient.DataSize[a] := Attr.Count * sizeof(TDGL4ByteData);
    end;

  ClientList.Add(CurrentClient);
  CurrentClient.BuiltProp.ID := LongWord(ClientList.Count);
  CurrentClient              := nil;
end;

procedure TDGLStaticRenderManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  DGLSLogger.LogInfo('TDGLStaticRenderManager : BeginPrimitive');
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  GLVBOMState := GLVBOM_PRIMITIVE;
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <= GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES, 'BeginPrimitives: Too much different primitive types in one object.');
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount]   := 0;
end;

procedure TDGLStaticRenderManager.EndPrimitives;
var
  Valid:                       Boolean;
  Index:                       LongWord;
  p:                           Integer;
  IndicesList, adjIndicesList: TDGLLongWordList;
  start, Count:                LongWord;
begin
  DGLSLogger.LogInfo('TDGLStaticRenderManager : EndPrimitive');
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEndPrim);
  GLVBOMState := GLVBOM_OBJECT;

  Index := HostIndexBuffer.Items[HostIndexBuffer.Count - 1];
  if Doubling or (Index = RestartIndex) then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := False;
  end;

  Valid := False;
  Count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
    GLVBOM_TRIANGLES:
      Valid := (Count mod 3 = 0) and (Count > 2);
    GLVBOM_TRIANGLE_STRIP, GLVBOM_TRIANGLE_FAN:
      Valid := Count > 2;
    // GLVBOM_QUADS:
    // Valid := (Count mod 4 = 0) and (Count > 3);
    // GLVBOM_QUAD_STRIP:
    // Valid := Count > 3;
    GLVBOM_POINTS:
      Valid := Count > 0;
    GLVBOM_LINES:
      Valid := (Count mod 2 = 0) and (Count > 1);
    GLVBOM_LINE_STRIP, GLVBOM_LINE_LOOP:
      Valid := Count > 2;
    // GLVBOM_POLYGON:
    // Valid := Count > 2;
    GLVBOM_LINES_ADJACENCY:
      Valid := (Count mod 4 = 0) and (Count > 3);
    GLVBOM_LINE_STRIP_ADJACENCY:
      Valid := Count > 4;
    GLVBOM_TRIANGLES_ADJACENCY:
      Valid := (Count mod 6 = 0) and (Count > 5);
    GLVBOM_TRIANGLE_STRIP_ADJACENCY:
      Valid := Count > 4;
  end;

  Assert(Valid, glsInvalidNumberOfVertex);

  // Make trinagles with adjancency
  if CurrentClient.BuiltProp.TriangleAdjacency then
  begin
    start   := CurrentClient.FirstIndex;
    for p   := 0 to PrimitiveTypeCount - 1 do
      start := start + CurrentClient.IndexCount[p];
    Count   := CurrentClient.IndexCount[PrimitiveTypeCount];

    if CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_STRIP then
    begin
      // Convert strips to independent triangles
      IndicesList := ConvertStripToList(@HostIndexBuffer.List[start], Count, RestartIndex);
      if Assigned(IndicesList) then
      begin
        HostIndexBuffer.Count := start;
        Count                 := IndicesList.Count;
        HostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), Count);
        CurrentClient.IndexCount[PrimitiveTypeCount]    := Count;
        CurrentClient.PrimitiveType[PrimitiveTypeCount] := GLVBOM_TRIANGLES;
        IndicesList.Free;
      end;
    end
    else if CurrentClient.PrimitiveType[PrimitiveTypeCount] = GLVBOM_TRIANGLE_FAN then
    begin
      // Convert fans to independent triangles
      IndicesList := ConvertFansToList(@HostIndexBuffer.List[start], Count, RestartIndex);
      if Assigned(IndicesList) then
      begin
        HostIndexBuffer.Count := start;
        Count                 := IndicesList.Count;
        HostIndexBuffer.AddLongWords(PLongWord(IndicesList.List), Count);
        CurrentClient.IndexCount[PrimitiveTypeCount]    := Count;
        CurrentClient.PrimitiveType[PrimitiveTypeCount] := GLVBOM_TRIANGLES;
        IndicesList.Free;
      end;
    end;

    if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_TRIANGLES then
      exit;

    if HostIndexBuffer.Capacity < Integer(start + 2 * Count + vEdgeInfoReserveSize) then
      HostIndexBuffer.Capacity := Integer(start + 2 * Count + vEdgeInfoReserveSize);

    adjIndicesList := MakeTriangleAdjacencyList(@HostIndexBuffer.List[start], Count, @AttributeArrays[0].List[0]);

    if Assigned(adjIndicesList) then
    begin
      HostIndexBuffer.Count := start;
      HostIndexBuffer.AddLongWords(PLongWord(adjIndicesList.List), adjIndicesList.Count);
      CurrentClient.IndexCount[PrimitiveTypeCount]    := adjIndicesList.Count;
      CurrentClient.PrimitiveType[PrimitiveTypeCount] := GLVBOM_TRIANGLES_ADJACENCY;
      ObjectIndex                                     := HostIndexBuffer.Items[HostIndexBuffer.Count - 1] + 1;
      adjIndicesList.Free;
    end;
  end;

end;

procedure TDGLStaticRenderManager.BuildBuffer(const rci: TRenderContextInfo);
var
  a, c, I:         Integer;
  offset:          LongWord;
  Client:          PGLRenderPacket;
  tempIndexBuffer: Pointer;
begin
  if FBuilded then exit;
  DGLSLogger.LogInfo('TDGLStaticRenderManager : BuildBuffer');
  if (HostVertexBuffer.Count = 0) or (HostIndexBuffer.Count = 0) then exit;

  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  FVertexHandle.AllocateHandle;
  FVertexHandle.Bind;
  // Upload all vertices data in one buffer
  FVertexHandle.BufferData(HostVertexBuffer.List, HostVertexBuffer.Count * sizeof(TDGL4ByteData), Usage);
  // Upload all indices data in one buffer
  FIndexHandle.AllocateHandle;
  FIndexHandle.Bind;
  // Adjust index type according its number
  indexType       := GL_UNSIGNED_INT;
  tempIndexBuffer := nil;
  RestartIndex    := $FFFFFF;
  if MaxIndexValue + 1 < $10000 then
  begin
    if MaxIndexValue + 1 < $100 then
    begin
      GetMem(tempIndexBuffer, HostIndexBuffer.Count);
      for I                            := 0 to HostIndexBuffer.Count - 1 do
        PByteArray(tempIndexBuffer)[I] := Byte(HostIndexBuffer.Items[I]);
      FIndexHandle.BufferData(tempIndexBuffer, HostIndexBuffer.Count, Usage);
      indexType    := GL_UNSIGNED_BYTE;
      RestartIndex := $FF;
    end
    else
    begin
      GetMem(tempIndexBuffer, sizeof(Word) * HostIndexBuffer.Count);
      for I                             := 0 to HostIndexBuffer.Count - 1 do
        PWordVector(tempIndexBuffer)[I] := Word(HostIndexBuffer.Items[I]);
      FIndexHandle.BufferData(tempIndexBuffer, sizeof(Word) * HostIndexBuffer.Count, Usage);
      indexType    := GL_UNSIGNED_SHORT;
      RestartIndex := $FFFF;
    end;
  end
  else
    FIndexHandle.BufferData(HostIndexBuffer.List, sizeof(Integer) * HostIndexBuffer.Count, Usage);

  for c := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[c];
    // Uniting all the states and buffers in one vertex array object
    Client.ArrayHandle := TDGLVertexArrayHandle.CreateAndAllocate;
    Client.ArrayHandle.Bind;
    FVertexHandle.Bind;
    offset := Client.FirstVertex;
    // Setup Client Attributes pointer
    for a := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    begin
      if Client.AttributesInUse[a] then
      begin
        glEnableVertexAttribArray(a);
        case Client.DataType[a] of
          GLVBOM_1F:
            glVertexAttribPointer(a, 1, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_2F:
            glVertexAttribPointer(a, 2, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_3F:
            glVertexAttribPointer(a, 3, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_4F:
            glVertexAttribPointer(a, 4, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_1I:
            glVertexAttribIPointer(a, 1, GL_INT, 0, Pointer(offset));

          GLVBOM_2I:
            glVertexAttribIPointer(a, 2, GL_INT, 0, Pointer(offset));

          GLVBOM_3I:
            glVertexAttribIPointer(a, 3, GL_INT, 0, Pointer(offset));

          GLVBOM_4I:
            glVertexAttribIPointer(a, 4, GL_INT, 0, Pointer(offset));

          GLVBOM_4UB:
            glVertexAttribPointer(a, 4, GL_UNSIGNED_BYTE, True, 0, Pointer(offset));
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end; // of case
        offset := offset + Client.DataSize[a];
      end
      else
        glDisableVertexAttribArray(a)
    end;

    Client.RelativeSize := (offset - Client.FirstVertex) / (HostVertexBuffer.Count * sizeof(TDGL4ByteData));

    // Add index buffer to array
    FIndexHandle.Bind;

    Client.ArrayHandle.UnBind;
  end; // for c

  FBuilded := True;

  if Assigned(tempIndexBuffer) then
    FreeMem(tempIndexBuffer);
  HostVertexBuffer.Clear;
  HostIndexBuffer.Clear;
end;

procedure TDGLStaticRenderManager.RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo);
var
  p, I, n, fullPartCount: Integer;
  Client:                 PGLRenderPacket;
  pType:                  TGLEnum;
  offset, typeSize:       LongWord;
  uniform:                GLint;
  start, restPart:        LongWord;
begin
  if not FBuilded then exit;
  DGLSLogger.LogInfo('TDGLStaticRenderManager : RenderClient');
  Client   := ClientList.Items[BuiltProp.ID - 1];
  offset   := Client.FirstIndex;
  typeSize := sizeof(GLubyte);
  case indexType of
    GL_UNSIGNED_SHORT:
      typeSize := sizeof(GLUShort);
    GL_UNSIGNED_INT:
      typeSize := sizeof(GLUInt);
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
      if (Client.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY) and (Client.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
        if not dglCheckExtension('GL_EXT_gpu_shader4') then continue;
      { : Primitives without adjacency should not be drawn with
        primitives with adjacency }
      if BuiltProp.TriangleAdjacency then
        if not((Client.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY) or (Client.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then continue;

      pType := cPrimitiveType[Client.PrimitiveType[p]];

      if Client.BuiltProp.InstancesNumber > 0 then
      begin
        if dglCheckExtension('GL_EXT_draw_instanced') then
          // HW instancing
          glDrawElementsInstancedEXT(pType, Client.IndexCount[p], indexType, Pointer(offset), Client.BuiltProp.InstancesNumber)
        else
        begin
          // Pseudo-Instancing
          if FRenderingContext.GLStates.CurrentProgram > 0 then
          begin
             uniform := glGetUniformLocationARB(FRenderingContext.GLStates.CurrentProgram, PGLChar(AnsiString(FRenderingContext.GLStates.uniformInstanceID.Name)));
            for I := 0 to Client.BuiltProp.InstancesNumber - 1 do
            begin
               if uniform >= 0 then glUniform1iARB(uniform, I);
              glDrawElements(pType, Client.IndexCount[p], indexType, Pointer(offset));
            end;
             if uniform >= 0 then glUniform1iARB(uniform, 0);
          end;
        end;
      end
      // Simple drawing with pre-TnL cashing
      else if dglCheckExtension('GL_EXT_draw_range_elements') then
      begin
        fullPartCount := Client.IndexCount[p] div vMaxElementsIndices;
        restPart      := Client.IndexCount[p] mod vMaxElementsIndices;
        for n         := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(pType, start, start + vMaxElementsIndices - 1, vMaxElementsIndices, indexType, Pointer(offset));
          Inc(start, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(pType, start, start + restPart - 1, restPart, indexType, Pointer(offset));
          Inc(start, restPart);
        end;
      end;
      Inc(offset, Client.IndexCount[p] * typeSize);

    end
    else
      break;

  Client.ArrayHandle.UnBind;
end;

function TDGLStaticRenderManager.UsageStatistic(const TimeInterval: Double): Single;
var
  c:       Integer;
  Client:  PGLRenderPacket;
  portion: Single;
begin
  portion := 0;
  for c   := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[c];
    if Client.LastTimeWhenRendered > vCurrentTime - TimeInterval then
      portion := portion + Client.RelativeSize;
  end;
  Result := portion;
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLStreamRenderManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLStreamRenderManager'}{$ENDIF}

constructor TDGLStreamRenderManager.Create(const AContext: TDGLContext);
begin
  inherited Create(AContext);
  Usage      := GL_STREAM_DRAW;
  FBuilded   := False;
  ClientList := TList.Create;
end;

destructor TDGLStreamRenderManager.Destroy;
var
  I:      Integer;
  Client: PGLRenderPacket;
begin
  // Clear clients info
  for I := 0 to ClientList.Count - 1 do
  begin
    Client := ClientList.Items[I];
    FreeClient(Client);
    Dispose(Client);
  end;
  ClientList.Destroy;
  inherited;
end;

procedure TDGLStreamRenderManager.BeginObject(const BuiltProp: TDGLBuiltProperties);
var
  I: Integer;
begin
  Assert(GLVBOMState = GLVBOM_DEFAULT, glsWrongCallBegin);
  Assert(FRenderingContext.GLStates.CurrentProgram > 0, glsNoShader);

  if BuiltProp.ID <> 0 then
    CurrentClient := ClientList.Items[BuiltProp.ID - 1];

  InitClient(CurrentClient);
  CurrentClient.FirstVertex := 0;
  CurrentClient.FirstIndex  := 0;
  CurrentClient.BuiltProp   := BuiltProp;
  GLVBOMState               := GLVBOM_OBJECT;

  for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    CurrentClient.DataType[I] := GLVBOM_NODATA;
    ResetAttribArray(I);
  end;
  OneVertexDataSize := 0;
  ObjectVertexCount := 0;
  ObjectIndex       := 0;
  HostIndexBuffer.Flush;
  Doubling           := False;
  MaxIndexValue      := 0;
  PrimitiveTypeCount := 0;
end;

procedure TDGLStreamRenderManager.EndObject(const rci: TRenderContextInfo);
var
  a:             Integer;
  Attr:          TDGL4ByteList;
  offset, size:  LongWord;
  HostVertexMap: Pointer;
  HostIndexMap:  Pointer;
  BuiltProp:     TDGLBuiltProperties;
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallEnd);
  GLVBOMState := GLVBOM_DEFAULT;

  if ObjectIndex = 0 then
    exit;

  if not Assigned(CurrentClient.ArrayHandle) then
  begin
    CurrentClient.ArrayHandle := TDGLVertexArrayHandle.CreateAndAllocate;
    CurrentClient.ArrayHandle.Bind;
    CurrentClient.VertexHandle := TDGLVBOArrayBufferHandle.Create;
    CurrentClient.IndexHandle  := TDGLVBOElementArrayHandle.Create;
  end;

  // Vertex buffer managment
  if CurrentClient.VertexHandle.Handle = 0 then
  begin
    CurrentClient.VertexHandle.AllocateHandle;
    CurrentClient.BuiltProp.VertexBufferCapacity := LongWord(ObjectVertexCount) * OneVertexDataSize;
    CurrentClient.VertexHandle.BindBufferData(nil, CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.VertexHandle.Bind;
    size := LongWord(ObjectVertexCount) * OneVertexDataSize;
    if CurrentClient.BuiltProp.VertexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.VertexHandle.BufferData(nil, CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;
  // Index buffer managment
  if CurrentClient.IndexHandle.Handle = 0 then
  begin
    CurrentClient.IndexHandle.AllocateHandle;
    CurrentClient.BuiltProp.IndexBufferCapacity := LongWord(HostIndexBuffer.Count) * sizeof(GLUInt);
    CurrentClient.IndexHandle.BindBufferData(HostIndexBuffer.List, CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.IndexHandle.Bind;
    size := LongWord(HostIndexBuffer.Count) * sizeof(GLUInt);
    if CurrentClient.BuiltProp.IndexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.IndexHandle.BufferData(nil, CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    end;
    if vUseMappingForOftenBufferUpdate then
    begin
      HostIndexMap := FIndexHandle.MapBuffer(GL_WRITE_ONLY);
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
      size := Attr.Count * sizeof(TDGL4ByteData);

      if vUseMappingForOftenBufferUpdate then
        Move(Attr.List^, PByte(Integer(HostVertexMap) + Integer(offset))^, size)
      else
        CurrentClient.VertexHandle.BufferSubData(offset, size, Attr.List);

      glEnableVertexAttribArray(a);
      if a > 0 then
        case CurrentClient.DataType[a] of
          GLVBOM_1F:
            glVertexAttribPointer(a, 1, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_2F:
            glVertexAttribPointer(a, 2, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_3F:
            glVertexAttribPointer(a, 3, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_4F:
            glVertexAttribPointer(a, 4, GL_FLOAT, False, 0, Pointer(offset));

          GLVBOM_1I:
            glVertexAttribIPointer(a, 1, GL_INT, 0, Pointer(offset));

          GLVBOM_2I:
            glVertexAttribIPointer(a, 2, GL_INT, 0, Pointer(offset));

          GLVBOM_3I:
            glVertexAttribIPointer(a, 3, GL_INT, 0, Pointer(offset));

          GLVBOM_4I:
            glVertexAttribIPointer(a, 4, GL_INT, 0, Pointer(offset));

          GLVBOM_4UB:
            glVertexAttribPointer(a, 4, GL_UNSIGNED_BYTE, True, 0, Pointer(offset));
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
      offset := offset + size;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  if vUseMappingForOftenBufferUpdate then
    CurrentClient.VertexHandle.UnmapBuffer;
  // set the pointer for position last
  glVertexAttribPointer(0, 3, GL_FLOAT, False, 0, nil);
  CurrentClient.ArrayHandle.UnBind;

  // add and setup client
  BuiltProp := CurrentClient.BuiltProp;
  if BuiltProp.ID = 0 then
  begin
    ClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := LongWord(ClientList.Count);
  end;
  CurrentClient := nil;
  RenderClient(BuiltProp, rci);
end;

procedure TDGLStreamRenderManager.EmitVertices(VertexNumber: LongWord; Indexed: Boolean);
var
  a:            Integer;
  offset, size: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsWrongCallEmit);
  Assert(VertexNumber > 0, glsInvalidNumberOfVertex);
  GLVBOMState := GLVBOM_DEFAULT;

  if not Assigned(CurrentClient.ArrayHandle) then
  begin
    CurrentClient.ArrayHandle := TDGLVertexArrayHandle.CreateAndAllocate;
    CurrentClient.ArrayHandle.Bind;
    CurrentClient.VertexHandle := TDGLVBOArrayBufferHandle.Create;
    CurrentClient.IndexHandle  := TDGLVBOElementArrayHandle.Create;
  end;

  // Vertex buffer managment
  if CurrentClient.VertexHandle.Handle = 0 then
  begin
    CurrentClient.VertexHandle.AllocateHandle;
    CurrentClient.BuiltProp.VertexBufferCapacity := VertexNumber * OneVertexDataSize;
    CurrentClient.VertexHandle.BindBufferData(nil, CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
  end
  else
  begin
    CurrentClient.VertexHandle.Bind;
    size := VertexNumber * OneVertexDataSize;
    if CurrentClient.BuiltProp.VertexBufferCapacity < size then
    begin
      CurrentClient.BuiltProp.VertexBufferCapacity := RoundUpToPowerOf2(size);
      CurrentClient.VertexHandle.BufferData(nil, CurrentClient.BuiltProp.VertexBufferCapacity, Usage);
    end;
  end;

  if Indexed then
  begin
    // Index buffer managment
    if CurrentClient.IndexHandle.Handle = 0 then
    begin
      CurrentClient.IndexHandle.AllocateHandle;
      CurrentClient.BuiltProp.IndexBufferCapacity := VertexNumber * sizeof(GLUInt);
      CurrentClient.IndexHandle.BindBufferData(nil, CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
    end
    else
    begin
      CurrentClient.IndexHandle.Bind;
      size := VertexNumber * sizeof(GLUInt);
      if CurrentClient.BuiltProp.IndexBufferCapacity < size then
      begin
        CurrentClient.BuiltProp.IndexBufferCapacity := RoundUpToPowerOf2(size);
        CurrentClient.IndexHandle.BufferData(nil, CurrentClient.BuiltProp.IndexBufferCapacity, Usage);
      end;
    end;
  end
  else if CurrentClient.IndexHandle.Handle <> 0 then
    CurrentClient.IndexHandle.DestroyHandle;

  // upload each attribute array one after another
  offset := 0;
  size   := 0;
  for a  := 0 to GLS_VERTEX_ATTR_NUM - 1 do
  begin
    if CurrentClient.AttributesInUse[a] then
    begin
      glEnableVertexAttribArray(a);
      case CurrentClient.DataType[a] of
        GLVBOM_1F:
          begin
            glVertexAttribPointer(a, 1, GL_FLOAT, False, 0, Pointer(offset));
            size := sizeof(GLfloat);
          end;

        GLVBOM_2F:
          begin
            glVertexAttribPointer(a, 2, GL_FLOAT, False, 0, Pointer(offset));
            size := 2 * sizeof(GLfloat);
          end;

        GLVBOM_3F:
          begin
            glVertexAttribPointer(a, 3, GL_FLOAT, False, 0, Pointer(offset));
            size := 3 * sizeof(GLfloat);
          end;

        GLVBOM_4F:
          begin
            glVertexAttribPointer(a, 4, GL_FLOAT, False, 0, Pointer(offset));
            size := 4 * sizeof(GLfloat);
          end;

        GLVBOM_1I:
          begin
            glVertexAttribIPointer(a, 1, GL_INT, 0, Pointer(offset));
            size := sizeof(GLint);
          end;

        GLVBOM_2I:
          begin
            glVertexAttribIPointer(a, 2, GL_INT, 0, Pointer(offset));
            size := 2 * sizeof(GLint);
          end;

        GLVBOM_3I:
          begin
            glVertexAttribIPointer(a, 3, GL_INT, 0, Pointer(offset));
            size := 3 * sizeof(GLint);
          end;

        GLVBOM_4I:
          begin
            glVertexAttribIPointer(a, 4, GL_INT, 0, Pointer(offset));
            size := 4 * sizeof(GLint);
          end;

        GLVBOM_4UB:
          begin
            glVertexAttribPointer(a, 4, GL_UNSIGNED_BYTE, True, 0, Pointer(offset));
            size := 4 * sizeof(GLubyte);
          end
      else
        Assert(False, glsErrorEx + glsUnknownType);
      end;
      offset := offset + size * VertexNumber;
    end
    else
      glDisableVertexAttribArray(a);
  end; // of attribute cycle

  CurrentClient.ArrayHandle.UnBind;

  // add and setup client
  if CurrentClient.BuiltProp.ID = 0 then
  begin
    ClientList.Add(CurrentClient);
    CurrentClient.BuiltProp.ID := LongWord(ClientList.Count);
  end;
  CurrentClient.VertexCount[PrimitiveTypeCount] := VertexNumber;
  CurrentClient.BuiltProp.ID                    := LongWord(ClientList.Count);
  CurrentClient                                 := nil;
end;

procedure TDGLStreamRenderManager.RenderClient(const BuiltProp: TDGLBuiltProperties; const rci: TRenderContextInfo);
var
  p, I, n, fullPartCount:            Integer;
  Client:                            PGLRenderPacket;
  pType:                             TGLEnum;
  offset, typeSize:                  LongWord;
  uniform:                           GLint;
  IndexStart, VertexStart, restPart: LongWord;
begin
  if vMaxElementsIndices = 0 then
    glGetintegerv(GL_MAX_ELEMENTS_INDICES, @vMaxElementsIndices);

  Client   := ClientList.Items[BuiltProp.ID - 1];
  offset   := Client.FirstIndex;
  typeSize := sizeof(GLUInt);
  // case indexType of
  //  GL_UNSIGNED_SHORT: typeSize := SizeOf(GLUShort);
  // GL_UNSIGNED_INT: typeSize := SizeOf(GLUInt);
  // end;
  offset := offset * typeSize;

  // Client.LastTimeWhenRendered := fCurrentTime;
  Client.ArrayHandle.Bind;
  EnablePrimitiveRestart(rci);

  IndexStart  := 0;
  VertexStart := 0;
  for p       := 0 to GLVBOM_MAX_DIFFERENT_PRIMITIVES - 1 do
    if Client.PrimitiveType[p] <> GLVBOM_NOPRIMITIVE then
    begin
      // Check the HW support of primitives
      if (Client.PrimitiveType[p] >= GLVBOM_LINES_ADJACENCY) and (Client.PrimitiveType[p] <= GLVBOM_TRIANGLE_STRIP_ADJACENCY) then
        if not dglCheckExtension('GL_EXT_gpu_shader4') then continue;
      { : Primitives without adjacency should not be drawn with
        primitives with adjacency }
      if BuiltProp.TriangleAdjacency then
        if not((Client.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY) or (Client.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then continue;

      pType := cPrimitiveType[Client.PrimitiveType[p]];

      if Client.BuiltProp.InstancesNumber > 0 then
      begin
        if GL_EXT_draw_instanced then
        begin
          // HW instancing
          if Client.IndexCount[p] > 0 then
            glDrawElementsInstancedEXT(pType, Client.IndexCount[p], GL_UNSIGNED_INT, Pointer(offset), Client.BuiltProp.InstancesNumber)
          else
            glDrawArraysInstancedEXT(pType, VertexStart, Client.VertexCount[p], Client.BuiltProp.InstancesNumber);
        end
        else
        begin
          // Pseudo-Instancing
          if FRenderingContext.GLStates.CurrentProgram > 0 then
          begin
            uniform := glGetUniformLocationARB(FRenderingContext.GLStates.CurrentProgram, PGLChar(AnsiString(FRenderingContext.GLStates.uniformInstanceID.Name)));
            if Client.IndexCount[p] > 0 then
            begin
              for I := 0 to Client.BuiltProp.InstancesNumber - 1 do
              begin
                if uniform >= 0 then glUniform1iARB(uniform, I);
                glDrawElements(pType, Client.IndexCount[p], GL_UNSIGNED_INT, Pointer(offset));
              end;
              if uniform >= 0 then glUniform1iARB(uniform, 0);
            end
            else
            begin
              for I := 0 to Client.BuiltProp.InstancesNumber - 1 do
              begin
                if uniform >= 0 then glUniform1iARB(uniform, I);
                glDrawArrays(pType, VertexStart, Client.VertexCount[p]);
              end;
               if uniform >= 0 then glUniform1iARB(uniform, 0);
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
        restPart      := Client.IndexCount[p] mod vMaxElementsIndices;
        for n         := 0 to fullPartCount - 1 do
        begin
          glDrawRangeElements(pType, IndexStart, IndexStart + vMaxElementsIndices - 1, vMaxElementsIndices, GL_UNSIGNED_INT, Pointer(offset));
          Inc(IndexStart, vMaxElementsIndices);
        end;
        if restPart > 0 then
        begin
          glDrawRangeElements(pType, IndexStart, IndexStart + restPart - 1, restPart, GL_UNSIGNED_INT, Pointer(offset));
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

procedure TDGLStreamRenderManager.BeginPrimitives(eType: TGLVBOMEnum);
begin
  Assert(GLVBOMState = GLVBOM_OBJECT, glsWrongCallBeginPrim);
  Assert((eType >= GLVBOM_TRIANGLES) and (eType <= GLVBOM_TRIANGLE_STRIP_ADJACENCY), glsInvalidPrimType);

  GLVBOMState := GLVBOM_PRIMITIVE;
  if CurrentClient.PrimitiveType[PrimitiveTypeCount] <> GLVBOM_NOPRIMITIVE then
  begin
    Inc(PrimitiveTypeCount);
    Assert(PrimitiveTypeCount < GLVBOM_MAX_DIFFERENT_PRIMITIVES, glsTooMachDiffPrim);
  end;

  CurrentClient.PrimitiveType[PrimitiveTypeCount] := eType;
  CurrentClient.VertexCount[PrimitiveTypeCount]   := 0;
end;

procedure TDGLStreamRenderManager.EndPrimitives;
var
  Valid: Boolean;
  Count: LongWord;
begin
  Assert(GLVBOMState = GLVBOM_PRIMITIVE, glsInvalidPrimType);
  GLVBOMState := GLVBOM_OBJECT;

  if Doubling then
  begin
    HostIndexBuffer.Pop;
    Dec(CurrentClient.IndexCount[PrimitiveTypeCount]);
    Doubling := False;
  end;

  Valid := False;
  Count := CurrentClient.VertexCount[PrimitiveTypeCount];
  case CurrentClient.PrimitiveType[PrimitiveTypeCount] of
    GLVBOM_TRIANGLES:
      Valid := (Count mod 3 = 0) and (Count > 2);
    GLVBOM_TRIANGLE_STRIP, GLVBOM_TRIANGLE_FAN:
      Valid := Count > 2;
    // GLVBOM_QUADS: ---> DEPRECATED SINCE OPENGL 3.3
    // Valid := (Count mod 4 = 0) and (Count > 3);
    // GLVBOM_QUAD_STRIP:
    // Valid := Count > 3;
    GLVBOM_POINTS:
      Valid := Count > 0;
    GLVBOM_LINES:
      Valid := (Count mod 2 = 0) and (Count > 1);
    GLVBOM_LINE_STRIP, GLVBOM_LINE_LOOP:
      Valid := Count > 2;
    // GLVBOM_POLYGON:
    // Valid := Count > 2;
    GLVBOM_LINES_ADJACENCY:
      Valid := (Count mod 4 = 0) and (Count > 3);
    GLVBOM_LINE_STRIP_ADJACENCY:
      Valid := Count > 4;
    GLVBOM_TRIANGLES_ADJACENCY:
      Valid := (Count mod 6 = 0) and (Count > 5);
    GLVBOM_TRIANGLE_STRIP_ADJACENCY:
      Valid := Count > 4;
  end;

  Assert(Valid, glsInvalidNumberOfVertex);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

vMaxElementsIndices := 0;

vVBOManagerList := TThreadList.Create;

finalization

FreeVBOManagers;

end.
