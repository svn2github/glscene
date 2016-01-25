//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//

unit GLS.CUDAGraphics;

interface

{$I cuda.inc}

uses
  System.Classes,
  GLS.CrossPlatform,
  GLS.CUDAAPI,
  GLS.CUDA,
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.State,
  GLS.Scene,
  GLS.Graphics,
  GLS.Material,
  GLS.Texture,
  GLSL.Shader,
  GLSL.Parameter,
  GLS.RenderContextInfo;

type

  TVKVertexAttribute = class;
  TVKVertexAttributes = class;

  TOnBeforeKernelLaunch = procedure(Sender: TVKVertexAttribute) of object;

  // TVKVertexAttribute
  //

  TVKVertexAttribute = class(TCollectionItem)
  private
    { Private declarations }
    FName: string;
    FType: TVKSLDataType;
    FFunc: TCUDAFunction;
    FLocation: TGLint;
    FOnBeforeKernelLaunch: TOnBeforeKernelLaunch;
    procedure SetName(const AName: string);
    procedure SetType(AType: TVKSLDataType);
    procedure SetFunc(AFunc: TCUDAFunction);
    function GetLocation: TGLint;
    function GetOwner: TVKVertexAttributes; reintroduce;
  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    procedure NotifyChange(Sender: TObject);
    property Location: TGLint read GetLocation;
  published
    { Published Declarations }
    property Name: string read FName write SetName;
    property GLSLType: TVKSLDataType read FType write SetType;
    property KernelFunction: TCUDAFunction read FFunc write SetFunc;
    property OnBeforeKernelLaunch: TOnBeforeKernelLaunch read
      FOnBeforeKernelLaunch write FOnBeforeKernelLaunch;
  end;

  // TVKVertexAttributes
  //

  TVKVertexAttributes = class(TOwnedCollection)
  private
    { Private declarations }
    procedure SetItems(Index: Integer; const AValue: TVKVertexAttribute);
    function GetItems(Index: Integer): TVKVertexAttribute;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    procedure NotifyChange(Sender: TObject);
    function MakeUniqueName(const ANameRoot: string): string;
    function GetAttributeByName(const AName: string): TVKVertexAttribute;
    function Add: TVKVertexAttribute;
    property Attributes[Index: Integer]: TVKVertexAttribute read GetItems
      write SetItems; default;
  end;

  TFeedBackMeshPrimitive = (fbmpPoint, fbmpLine, fbmpTriangle);
  TFeedBackMeshLaunching = (fblCommon, fblOnePerAtttribute);

  // TVKFeedBackMesh
  //

  TVKCustomFeedBackMesh = class(TVKBaseSceneObject)
  private
    { Private declarations }
    FGeometryResource: TCUDAGraphicResource;
    FAttributes: TVKVertexAttributes;
    FVAO: TVKVertexArrayHandle;
    FVBO: TVKVBOArrayBufferHandle;
    FEBO: TVKVBOElementArrayHandle;
    FPrimitiveType: TFeedBackMeshPrimitive;
    FVertexNumber: Integer;
    FElementNumber: Integer;
    FShader: TVKGLSLShader;
    FCommonFunc: TCUDAFunction;
    FLaunching: TFeedBackMeshLaunching;
    FBlend: Boolean;
    procedure SetAttributes(AValue: TVKVertexAttributes);
    procedure SetPrimitiveType(AValue: TFeedBackMeshPrimitive);
    procedure SetVertexNumber(AValue: Integer);
    procedure SetElementNumber(AValue: Integer);
    procedure SetShader(AShader: TVKGLSLShader);
    procedure SetCommonFunc(AFunc: TCUDAFunction);
  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RefreshAttributes;
    procedure AllocateHandles;
    procedure LaunchKernels;
  protected
    { Protected Declarations }
    property Attributes: TVKVertexAttributes read FAttributes write SetAttributes;
    { GLSL shader as material. If it absent or disabled - nothing be drawen. }
    property Shader: TVKGLSLShader read FShader write SetShader;
    { Primitive type. }
    property PrimitiveType: TFeedBackMeshPrimitive read FPrimitiveType
      write SetPrimitiveType default fbmpPoint;
    { Number of vertexes in array buffer. }
    property VertexNumber: Integer read FVertexNumber
      write SetVertexNumber default 1;
    { Number of indexes in element buffer. Zero to disable. }
    property ElementNumber: Integer read FElementNumber
      write SetElementNumber default 0;
    { Used for all attributes and elements if Launching = fblCommon
       otherwise used own attribute function and this for elements. }
    property CommonKernelFunction: TCUDAFunction read FCommonFunc
      write SetCommonFunc;
    { Define mode of manufacturer launching:
      fblCommon - single launch for all,
      flOnePerAtttribute - one launch per attribute and elements }
    property Launching: TFeedBackMeshLaunching read FLaunching
      write FLaunching default fblCommon;
    { Defines if the object uses blending for object
       sorting purposes. }
    { Defines if the object uses blending for object
       sorting purposes. }
    property Blend: Boolean read FBlend write FBlend default False;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    property ArrayBufferHandle: TVKVBOArrayBufferHandle read FVBO;
    property ElementArrayHandle: TVKVBOElementArrayHandle read FEBO;
  end;

  TVKFeedBackMesh = class(TVKCustomFeedBackMesh)
  published
    { Published Declarations }
    property Attributes;
    property Shader;
    property PrimitiveType;
    property VertexNumber;
    property ElementNumber;
    property CommonKernelFunction;
    property Launching;
    property Blend;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
  end;

  // TCUDAGLImageResource
  //

  TCUDAGLImageResource = class(TCUDAGraphicResource)
  private
    { Private declarations }
    fMaterialLibrary: TVKMaterialLibrary;
    fTextureName: TVKLibMaterialName;
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    procedure SetTextureName(const Value: TVKLibMaterialName);
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MapResources; override;
    procedure UnMapResources; override;
    procedure BindArrayToTexture(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LOngWord); override;
  published
    { Published declarations }
    property TextureName: TVKLibMaterialName read fTextureName write
      SetTextureName;
    property MaterialLibrary: TVKMaterialLibrary read fMaterialLibrary write
      SetMaterialLibrary;
    property Mapping;
  end;

  TCUDAGLGeometryResource = class(TCUDAGraphicResource)
  private
    { Private declarations }
    FFeedBackMesh: TVKCustomFeedBackMesh;
    procedure SetFeedBackMesh(const Value: TVKCustomFeedBackMesh);
    function GetAttribArraySize(AAttr: TVKVertexAttribute): LongWord;
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetAttributeArraySize(const AName: string): LongWord; override;
    function GetAttributeArrayAddress(const AName: string): Pointer; override;
    function GetElementArrayDataSize: LongWord; override;
    function GetElementArrayAddress: Pointer; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MapResources; override;
    procedure UnMapResources; override;

    property AttributeDataSize[const AttribName: string]: LongWord read
      GetAttributeArraySize;
    property AttributeDataAddress[const AttribName: string]: Pointer read
      GetAttributeArrayAddress;
    property IndexDataSize: LongWord read GetElementArrayDataSize;
    property IndexDataAddress: Pointer read GetElementArrayAddress;
  published
    { Published declarations }
    property FeedBackMesh: TVKCustomFeedBackMesh read FFeedBackMesh write
      SetFeedBackMesh;
    property Mapping;
  end;

implementation

uses
  System.SysUtils,
  GLS.Strings,
  GLS.TextureFormat
  {$IFDEF GLS_LOGGING}, GLSLog {$ENDIF};

resourcestring
  cudasFailToBindArrayToTex = 'Unable to bind CUDA array to OpenGL unmaped t' +
  'exture';
  cudasOutOfAttribSize = 'The amount of device''s data less then size of att' +
  'ribute''s data.';
  cudasOutOfElementSize = 'The amount of device''s data less then size of in' +
  'dexes data.';

{$IFDEF GLS_REGION}{$REGION 'TCUDAGLImageResource'}{$ENDIF}
// ------------------
// ------------------ TCUDAGLImageResource ------------------
// ------------------

constructor TCUDAGLImageResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle[0] := nil;
  fResourceType := rtTexture;
  FGLContextHandle := TVKVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAGLImageResource.Destroy;
begin
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAGLImageResource.SetMaterialLibrary(const Value:
  TVKMaterialLibrary);
begin
  if fMaterialLibrary <> Value then
  begin
    if Assigned(fMaterialLibrary) then
      fMaterialLibrary.RemoveFreeNotification(Self);
    fMaterialLibrary := Value;
    if Assigned(fMaterialLibrary) then
    begin
      fMaterialLibrary.FreeNotification(Self);
      if fMaterialLibrary.TextureByName(fTextureName) <> nil then
        DestroyHandles;
    end;
  end;
end;

procedure TCUDAGLImageResource.SetTextureName(const Value: TVKLibMaterialName);
begin
  if fTextureName <> Value then
  begin
    fTextureName := Value;
    DestroyHandles;
  end;
end;

procedure TCUDAGLImageResource.UnMapResources;
begin
  if FMapCounter > 0 then
    Dec(FMapCounter);

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      FStatus := cuGraphicsUnMapResources(1, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
end;

procedure TCUDAGLImageResource.AllocateHandles;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);
var
  LTexture: TVKTexture;
  glHandle: GLUInt;
begin
  FGLContextHandle.AllocateHandle;

  if FGLContextHandle.IsDataNeedUpdate
    and Assigned(FMaterialLibrary)
    and (Length(FTextureName) > 0) then
  begin
    inherited;

    LTexture := FMaterialLibrary.TextureByName(FTextureName);
    if Assigned(LTexture) then
    begin
      glHandle := LTexture.AllocateHandle;
      if glHandle = 0 then
        Abort;

      Context.Requires;
      DestroyHandles;

      FStatus := cuGraphicsGLRegisterImage(
        FHandle[0],
        glHandle,
        DecodeGLTextureTarget(LTexture.Image.NativeTextureTarget),
        cMapping[fMapping]);

      Context.Release;

      if FStatus <> CUDA_SUCCESS then
        Abort;

      FGLContextHandle.NotifyDataUpdated;
    end;
  end;
end;

procedure TCUDAGLImageResource.DestroyHandles;
begin
  if Assigned(FHandle[0]) then
  begin
    inherited;
    Context.Requires;
    FStatus := cuGraphicsUnregisterResource(FHandle[0]);
    Context.Release;
    FHandle[0] := nil;
    FGLContextHandle.NotifyChangesOfData;
  end;
end;

procedure TCUDAGLImageResource.MapResources;
begin
  AllocateHandles;

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      FStatus := cuGraphicsMapResources(1, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAGLImageResource.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (AComponent = fMaterialLibrary) and (Operation = opRemove) then
  begin
    fMaterialLibrary := nil;
    fTextureName := '';
    DestroyHandles;
  end;
end;

procedure TCUDAGLImageResource.BindArrayToTexture(var cudaArray: TCUDAMemData;
  ALeyer, ALevel: LOngWord);
var
  LTexture: TVKTexture;
  newArray: PCUarray;
begin
  if FMapCounter = 0 then
  begin
    GLSLogger.LogError(cudasFailToBindArrayToTex);
    Abort;
  end;

  Context.Requires;
  FStatus := cuGraphicsSubResourceGetMappedArray(
    newArray, FHandle[0], ALeyer, ALevel);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  LTexture := FMaterialLibrary.TextureByName(FTextureName);
  SetArray(cudaArray, newArray, True, LTexture.TexDepth > 0);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TCUDAGLGeometryResource'}{$ENDIF}
// ------------------
// ------------------ TCUDAGLGeometryResource ------------------
// ------------------

constructor TCUDAGLGeometryResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle[0] := nil;
  FHandle[1] := nil;
  FResourceType := rtBuffer;
  FMapCounter := 0;
  FGLContextHandle := TVKVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAGLGeometryResource.Destroy;
begin
  FeedBackMesh := nil;
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAGLGeometryResource.SetFeedBackMesh(const Value:
  TVKCustomFeedBackMesh);
begin
  if FFeedBackMesh <> Value then
  begin
    if Assigned(FFeedBackMesh) then
    begin
      FFeedBackMesh.RemoveFreeNotification(Self);
      FFeedBackMesh.FGeometryResource := nil;
    end;
    FFeedBackMesh := Value;
    if Assigned(FFeedBackMesh) then
    begin
      FFeedBackMesh.FreeNotification(Self);
      FFeedBackMesh.FGeometryResource := Self;
    end;
    DestroyHandles;
  end;
end;

procedure TCUDAGLGeometryResource.AllocateHandles;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);

begin
  inherited;
  FGLContextHandle.AllocateHandle;
  if FGLContextHandle.IsDataNeedUpdate then
  begin
    if FFeedBackMesh.FVBO.IsDataNeedUpdate then
      FFeedBackMesh.AllocateHandles;

    Context.Requires;

    DestroyHandles;

    // Register vertex array
    FStatus := cuGraphicsGLRegisterBuffer(
      FHandle[0],
      FFeedBackMesh.FVBO.Handle,
      cMapping[FMapping]);

    // Register element array
    if FFeedBackMesh.ElementNumber > 0 then
      CollectStatus(
        cuGraphicsGLRegisterBuffer(
          FHandle[1],
          FFeedBackMesh.FEBO.Handle,
          cMapping[FMapping]));

    Context.Release;

    if FStatus <> CUDA_SUCCESS then
      Abort;

    FGLContextHandle.NotifyDataUpdated;
  end;
end;

procedure TCUDAGLGeometryResource.DestroyHandles;
begin
  if Assigned(fHandle[0]) or Assigned(fHandle[1]) then
  begin
    inherited;

    Context.Requires;

    while FMapCounter > 0 do
      UnMapResources;

    FStatus := CUDA_SUCCESS;

    if Assigned(fHandle[0]) then
    begin
      CollectStatus(cuGraphicsUnregisterResource(fHandle[0]));
      fHandle[0] := nil;
    end;

    if Assigned(fHandle[1]) then
    begin
      CollectStatus(cuGraphicsUnregisterResource(fHandle[1]));
      fHandle[1] := nil;
    end;

    Context.Release;
    FGLContextHandle.NotifyChangesOfData;
  end;
end;

procedure TCUDAGLGeometryResource.Notification(AComponent: TComponent;
  Operation:
  TOperation);
begin
  inherited;
  if (AComponent = FFeedBackMesh) and (Operation = opRemove) then
  begin
    FeedBackMesh := nil;
    DestroyHandles;
  end;
end;

procedure TCUDAGLGeometryResource.MapResources;
var
  count: Integer;
begin
  AllocateHandles;

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      FStatus := cuGraphicsMapResources(count, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAGLGeometryResource.UnMapResources;
var
  count: Integer;
begin
  if FMapCounter > 0 then
    Dec(FMapCounter);

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      FStatus := cuGraphicsUnMapResources(count, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
end;

function TCUDAGLGeometryResource.GetAttribArraySize(AAttr: TVKVertexAttribute): LongWord;
var
  typeSize: LongWord;
begin
  case AAttr.GLSLType of
    GLSLType1F: typeSize := SizeOf(GLFloat);
    GLSLType2F: typeSize := 2 * SizeOf(GLFloat);
    GLSLType3F: typeSize := 3 * SizeOf(GLFloat);
    GLSLType4F: typeSize := 4 * SizeOf(GLFloat);
    GLSLType1I: typeSize := SizeOf(GLInt);
    GLSLType2I: typeSize := 2 * SizeOf(GLInt);
    GLSLType3I: typeSize := 3 * SizeOf(GLInt);
    GLSLType4I: typeSize := 4 * SizeOf(GLInt);
    GLSLType1UI: typeSize := SizeOf(GLInt);
    GLSLType2UI: typeSize := 2 * SizeOf(GLInt);
    GLSLType3UI: typeSize := 3 * SizeOf(GLInt);
    GLSLType4UI: typeSize := 4 * SizeOf(GLInt);
    GLSLTypeMat2F: typeSize := 4 * SizeOf(GLFloat);
    GLSLTypeMat3F: typeSize := 9 * SizeOf(GLFloat);
    GLSLTypeMat4F: typeSize := 16 * SizeOf(GLFloat);
  else
    begin
      Assert(False, vksErrorEx + vksUnknownType);
      typeSize := 0;
    end;
  end;
  Result := Cardinal(FFeedBackMesh.VertexNumber) * typeSize;
end;

function TCUDAGLGeometryResource.GetAttributeArraySize(
  const AName: string): LongWord;
var
  LAttr: TVKVertexAttribute;
begin
  Result := 0;
  LAttr := FFeedBackMesh.Attributes.GetAttributeByName(AName);
  if not Assigned(LAttr) then
    exit;
  if LAttr.GLSLType = GLSLTypeUndefined then
    exit;
  Result := GetAttribArraySize(LAttr);
end;

function TCUDAGLGeometryResource.GetAttributeArrayAddress(
  const AName: string): Pointer;
var
  i: Integer;
  Size: Cardinal;
  MapPtr: Pointer;
  LAttr: TVKVertexAttribute;
begin
  Result := nil;
  if FMapCounter = 0 then
    exit;
  LAttr := FFeedBackMesh.Attributes.GetAttributeByName(AName);
  if not Assigned(LAttr) then
    exit;

  for i := 0 to LAttr.Index - 1 do
    Inc(PByte(Result), GetAttribArraySize(FFeedBackMesh.Attributes[i]));

  Context.Requires;
  MapPtr := nil;
  FStatus := cuGraphicsResourceGetMappedPointer(
    MapPtr, Size, FHandle[0]);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  if PtrUInt(Result) + GetAttribArraySize(LAttr) > Size then
  begin
    GLSLogger.LogError(cudasOutOfAttribSize);
    Abort;
  end;

  Inc(Pbyte(Result), PtrUInt(MapPtr));
end;

function TCUDAGLGeometryResource.GetElementArrayDataSize: LongWord;
begin
  Result := FFeedBackMesh.ElementNumber * SizeOf(GLUInt);
end;

function TCUDAGLGeometryResource.GetElementArrayAddress: Pointer;
var
  Size: Cardinal;
  MapPtr: Pointer;
begin
  Result := nil;
  if (FHandle[1] = nil) and (FMapCounter = 0) then
    exit;

  Context.Requires;
  MapPtr := nil;
  FStatus := cuGraphicsResourceGetMappedPointer(MapPtr, Size, FHandle[1]);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  if GetElementArrayDataSize > Size then
  begin
    GLSLogger.LogError(cudasOutOfElementSize);
    Abort;
  end;

  Inc(Pbyte(Result), PtrUInt(MapPtr));
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// -----------------------
// ----------------------- TVKVertexAttribute -------------------
// -----------------------

{$IFDEF GLS_REGION}{$REGION 'TVKVertexAttribute'}{$ENDIF}

constructor TVKVertexAttribute.Create(ACollection: TCollection);
begin
  inherited;
  FName := GetOwner.MakeUniqueName('Attrib');
  FType := GLSLTypeUndefined;
  FLocation := -1;
end;

procedure TVKVertexAttribute.SetFunc(AFunc: TCUDAFunction);
var
  LMesh: TVKCustomFeedBackMesh;
begin
  LMesh := TVKCustomFeedBackMesh(GetOwner.GetOwner);
  if Assigned(FFunc) then
    FFunc.RemoveFreeNotification(LMesh);
  FFunc := AFunc;
  if Assigned(FFunc) then
    FFunc.FreeNotification(LMesh);
end;

procedure TVKVertexAttribute.SetName(const AName: string);
begin
  if AName <> FName then
  begin
    FName := '';
    FName := GetOwner.MakeUniqueName(AName);
    NotifyChange(Self);
  end;
end;

procedure TVKVertexAttribute.SetType(AType: TVKSLDataType);
begin
  if AType <> FType then
  begin
    FType := AType;
    NotifyChange(Self);
  end;
end;

function TVKVertexAttribute.GetLocation: TGLint;
begin
  if FLocation < 0 then
    FLocation := GL.GetAttribLocation(
      CurrentGLContext.GLStates.CurrentProgram,
      PGLChar(TGLString(FName)));
  Result := FLocation;
end;

function TVKVertexAttribute.GetOwner: TVKVertexAttributes;
begin
  Result := TVKVertexAttributes(Collection);
end;

procedure TVKVertexAttribute.NotifyChange(Sender: TObject);
begin
  GetOwner.NotifyChange(Self);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// -----------------------
// ----------------------- TVKVertexAttributes -------------------
// -----------------------

{$IFDEF GLS_REGION}{$REGION 'TVKVertexAttributes'}{$ENDIF}

function TVKVertexAttributes.Add: TVKVertexAttribute;
begin
  Result := (inherited Add) as TVKVertexAttribute;
end;

constructor TVKVertexAttributes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVKVertexAttribute);
end;

function TVKVertexAttributes.GetAttributeByName(
  const AName: string): TVKVertexAttribute;
var
  I: Integer;
  A: TVKVertexAttribute;
begin
  // Brute-force, there no need optimization
  for I := 0 to Count - 1 do
  begin
    A := TVKVertexAttribute(Items[i]);
    if A.Name = AName then
      Exit(A);
  end;
  Result := nil;
end;

function TVKVertexAttributes.GetItems(Index: Integer): TVKVertexAttribute;
begin
  Result := TVKVertexAttribute(inherited Items[index]);
end;

function TVKVertexAttributes.MakeUniqueName(const ANameRoot: string): string;
var
  I: Integer;
begin
  Result := ANameRoot;
  I := 1;
  while GetAttributeByName(Result) <> nil do
  begin
    Result := ANameRoot + IntToStr(I);
    Inc(I);
  end;
end;

procedure TVKVertexAttributes.NotifyChange(Sender: TObject);
begin
  TVKCustomFeedBackMesh(GetOwner).NotifyChange(Self);
end;

procedure TVKVertexAttributes.SetItems(Index: Integer;
  const AValue: TVKVertexAttribute);
begin
  inherited Items[index] := AValue;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// -----------------------
// ----------------------- TVKCustomFeedBackMesh -------------------
// -----------------------

{$IFDEF GLS_REGION}{$REGION 'TVKCustomFeedBackMesh'}{$ENDIF}

// AllocateHandles
//

procedure TVKCustomFeedBackMesh.AllocateHandles;
var
  I, L: Integer;
  Size, Offset: Cardinal;
  GR: TCUDAGLGeometryResource;
  EnabledLocations: array[0..GLS_VERTEX_ATTR_NUM - 1] of Boolean;
begin
  FVAO.AllocateHandle;
  FVBO.AllocateHandle;
  FEBO.AllocateHandle;

  if Assigned(FGeometryResource) then
  begin
    GR := TCUDAGLGeometryResource(FGeometryResource);
    size := 0;
    for I := 0 to Attributes.Count - 1 do
      Inc(size, GR.GetAttribArraySize(Attributes[I]));

    FVAO.Bind;
    FVBO.BindBufferData(nil, size, GL_STREAM_DRAW);
    if FElementNumber > 0 then
      FEBO.BindBufferData(nil, GR.GetElementArrayDataSize, GL_STREAM_DRAW)
    else
      FEBO.UnBind; // Just in case

    // Predisable attributes
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      EnabledLocations[I] := false;

    Offset := 0;
    for I := 0 to Attributes.Count - 1 do
    begin
      L := Attributes[I].Location;
      if L > -1 then
      begin
        EnabledLocations[I] := True;
        case Attributes[I].GLSLType of

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
              GL.VertexAttribIPointer(L, 4, GL_INT, 0, pointer(Offset));

            GLSLType1UI:
              GL.VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType2UI:
              GL.VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType3UI:
              GL.VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType4UI:
              GL.VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLTypeMat2F:
              GL.VertexAttribPointer(L, 4, GL_FLOAT, false, 0, pointer(Offset));

            GLSLTypeMat3F:
              GL.VertexAttribPointer(L, 9, GL_FLOAT, false, 0, pointer(Offset));

            GLSLTypeMat4F:
              GL.VertexAttribPointer(L, 16, GL_FLOAT, false, 0, pointer(Offset));

        end; // of case
      end;
      Inc(Offset, GR.GetAttribArraySize(Attributes[I]));
    end;

    // Enable engagement attributes array
    with GL do
    begin
      for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        if EnabledLocations[I] then
          EnableVertexAttribArray(I)
        else
          DisableVertexAttribArray(I);
    end;

    FVAO.UnBind;
    FVAO.NotifyDataUpdated;
  end;
end;

// Create
//

constructor TVKCustomFeedBackMesh.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FAttributes := TVKVertexAttributes.Create(Self);
  FVAO := TVKVertexArrayHandle.Create;
  FVBO := TVKVBOArrayBufferHandle.Create;
  FEBO := TVKVBOElementArrayHandle.Create;
  FPrimitiveType := fbmpPoint;
  FLaunching := fblCommon;
  FVertexNumber := 1;
  FElementNumber := 0;
  FBlend := False;
end;

// Destroy
//

destructor TVKCustomFeedBackMesh.Destroy;
begin
  Shader := nil;
  FAttributes.Destroy;
  FVAO.Destroy;
  FVBO.Destroy;
  FEBO.Destroy;
  inherited;
end;

// LaunchKernels
//

procedure TVKCustomFeedBackMesh.LaunchKernels;
var
  i: Integer;
  GR: TCUDAGLGeometryResource;
//  IR: TCUDAGLImageResource;
begin

  if Assigned(FGeometryResource) then
  begin
    // Produce geometry resource
    GR := TCUDAGLGeometryResource(FGeometryResource);
    GR.MapResources;
    // Produce vertex attributes
    case Launching of
      fblCommon:
        begin
          for I := 0 to FAttributes.Count - 1 do
            with FAttributes.Attributes[I] do
              if Assigned(OnBeforeKernelLaunch) then
                OnBeforeKernelLaunch(FAttributes.Attributes[I]);
          if Assigned(FCommonFunc) then
            FCommonFunc.Launch;
        end;
      fblOnePerAtttribute:
        begin
          for I := 0 to FAttributes.Count - 1 do
            with FAttributes.Attributes[I] do
            begin
              if Assigned(OnBeforeKernelLaunch) then
                OnBeforeKernelLaunch(FAttributes.Attributes[I]);
              if Assigned(KernelFunction) then
                KernelFunction.Launch;
            end;
        end;
    else
      Assert(False, vksErrorEx + vksUnknownType);
    end;
    // Produce indexes
    if (GR.GetElementArrayDataSize > 0)
      and Assigned(FCommonFunc) then
        FCommonFunc.Launch;

    GR.UnMapResources;
  end;
end;
//    // Produce image resource
//  else if FGLResource is TCUDAGLImageResource then
//  begin
//    IR := TCUDAGLImageResource(FGLResource);
//    IR.MapResources;
//    if Assigned(FBeforeLaunch) then
//      FBeforeLaunch(Self, 0);
//    if Assigned(FManufacturer) then
//      FManufacturer.Launch;
//    IR.UnMapResources;
//  end;

procedure TVKCustomFeedBackMesh.DoRender(var ARci: TVKRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
const
  cPrimitives: array[TFeedBackMeshPrimitive] of TGLenum =
    (GL_POINTS, GL_LINES, GL_TRIANGLES);
begin
  if ARenderSelf
    and not (csDesigning in ComponentState)
    and Assigned(FShader)
    and Assigned(FGeometryResource) then
    try
      FShader.Apply(ARci, Self);
      if FVAO.IsDataNeedUpdate then
        AllocateHandles;

      // Produce mesh data
      LaunchKernels;
      // Draw mesh
      FVAO.Bind;
      // Multipass Shader Loop
      repeat
        // Render mesh
        if FElementNumber > 0 then
        begin
          GL.DrawElements(
            cPrimitives[FPrimitiveType],
            FElementNumber,
            GL_UNSIGNED_INT,
            nil);
        end
        else
        begin
          GL.DrawArrays(
            cPrimitives[FPrimitiveType],
            0,
            FVertexNumber);
        end;
      until not FShader.UnApply(ARci);
      FVAO.UnBind;
    except
      Visible := False;
    end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TVKCustomFeedBackMesh.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Operation = opRemove then
  begin
    if AComponent = Shader then
      Shader := nil
    else if AComponent = FCommonFunc then
      CommonKernelFunction := nil
    else if AComponent is TCUDAFunction then
    begin
      for I := 0 to FAttributes.Count - 1  do
        if FAttributes[I].KernelFunction = AComponent then
          FAttributes[I].KernelFunction := nil;
    end;
  end;
  inherited;
end;

procedure TVKCustomFeedBackMesh.RefreshAttributes;
var
  I: Integer;
  AttribInfo: TVKActiveAttribArray;
begin
  if Assigned(FShader) and FShader.Enabled then
  begin
    FShader.FailedInitAction := fiaSilentDisable;
    Scene.CurrentBuffer.RenderingContext.Activate;
    try
      AttribInfo := FShader.GetActiveAttribs;
    except
      FShader.Enabled := False;
      Scene.CurrentBuffer.RenderingContext.Deactivate;
      exit;
    end;
    Scene.CurrentBuffer.RenderingContext.Deactivate;
    FAttributes.Clear;
    for I := 0 to High(AttribInfo) do
    begin
      with FAttributes.Add do
      begin
        Name := AttribInfo[I].Name;
        GLSLType := AttribInfo[I].AType;
        FLocation := AttribInfo[I].Location;
      end;
    end;
    FVAO.NotifyChangesOfData;
  end;
end;

procedure TVKCustomFeedBackMesh.SetAttributes(AValue: TVKVertexAttributes);
begin
  FAttributes.Assign(AValue);
end;

procedure TVKCustomFeedBackMesh.SetCommonFunc(AFunc: TCUDAFunction);
begin
  if AFunc <> FCommonFunc then
  begin
    if Assigned(FCommonFunc) then
      FCommonFunc.RemoveFreeNotification(Self);
    FCommonFunc := AFunc;
    if Assigned(FCommonFunc) then
      FCommonFunc.FreeNotification(Self);
  end;
end;

procedure TVKCustomFeedBackMesh.SetElementNumber(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  FElementNumber := AValue;
  FVAO.NotifyChangesOfData;
end;

procedure TVKCustomFeedBackMesh.SetPrimitiveType(AValue: TFeedBackMeshPrimitive);
begin
  FPrimitiveType := AValue;
end;

procedure TVKCustomFeedBackMesh.SetShader(AShader: TVKGLSLShader);
begin
  if AShader <> FShader then
  begin
    if Assigned(FShader) then
      FShader.RemoveFreeNotification(Self);
    FShader := AShader;
    if Assigned(FShader) then
      FShader.FreeNotification(Self);
    if not (csLoading in ComponentState) then
      RefreshAttributes;
  end;
end;

procedure TVKCustomFeedBackMesh.SetVertexNumber(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  FVertexNumber := AValue;
  FVAO.NotifyChangesOfData;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses([TCUDAGLImageResource, TCUDAGLGeometryResource,
    TVKCustomFeedBackMesh, TVKFeedBackMesh]);

end.

