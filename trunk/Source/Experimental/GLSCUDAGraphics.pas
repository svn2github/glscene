//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDAGraphics<p>

   <b>History : </b><font size=-1><ul>
      <li>01/04/10 - Yar - Creation
   </ul></font><p>
}

unit GLSCUDAGraphics;

interface

{$I cuda.inc}

uses
  Classes, GLS_CUDA_API, OpenGL1x, GLContext, GLState, GLSCUDA,
  GLGraphics, GLMaterial, GLTexture, GLRenderContextInfo,
  GL3xMaterial, GL3xObjects, GL3xShadersManager, GLVBOManagers, GL3xFactory;

type

  TCUDAGLImageResource = class(TCUDAGraphicResource)
  private
    { Private declarations }
    fMaterialLibrary: TGLMaterialLibrary;
    fTextureName: TGLLibMaterialName;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetTextureName(const Value: TGLLibMaterialName);
  protected
    { Protected declaration }
    procedure AllocateHandle; override;
    procedure DestroyHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MapResources; override;
    procedure UnMapResources; override;
    procedure BindArrayToImage(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LOngWord); override;
  published
    { Published declarations }
    property TextureName: TGLLibMaterialName read fTextureName write
      SetTextureName;
    property MaterialLibrary: TGLMaterialLibrary read fMaterialLibrary write
      SetMaterialLibrary;
    property Mapping;

  end;

  TCUDAGLGeometryResource = class(TCUDAGraphicResource)
  private
    { Private declarations }
    FSceneObject: TGL3xBaseSceneObject;
    FAttrInfoReady: Boolean;
    FAttributes: TGLSLAttributeArray;
    procedure SetSceneObject(const Value: TGL3xBaseSceneObject);
  protected
    { Protected declaration }
    procedure AllocateHandle; override;
    procedure DestroyHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function GetAttribute(Attr: Integer): TGLSLAttribute;
    function GetAttributeDataSize(Attr: Integer): LongWord; override;
    function GetAttributeDataAddress(Attr: Integer): Pointer; override;
    function GetIndexDataSize: LongWord; override;
    function GetIndexDataAddress: Pointer; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MapResources; override;
    procedure UnMapResources; override;

    property Attribute[Index: Integer]: TGLSLAttribute read GetAttribute;
    property AttributeDataSize[Index: Integer]: LongWord read
    GetAttributeDataSize;
    property AttributeDataAddress[Index: Integer]: Pointer read
    GetAttributeDataAddress;
    property IndexDataSize: LongWord read GetIndexDataSize;
    property IndexDataAddress: Pointer read GetIndexDataAddress;
  published
    { Published declarations }
    property SceneObject: TGL3xBaseSceneObject read fSceneObject write
      SetSceneObject;
    property Mapping;
  end;

  TCUDAFactoryBeforeLaunch =
    procedure(Sender: TObject; AttrIndex: Integer) of object;

  // TGLSCUDAFactory
  //
  TGLSCUDAFactory = class(TGL3xBaseFactory)
  private
    { Private declarations }
    FManufacturer: TCUDAFunction;
    FGLResource: TCUDAGraphicResource;
    FBeforeLaunch: TCUDAFactoryBeforeLaunch;

    procedure SetManufacturer(Value: TCUDAFunction);
    procedure SetOpenGLResource(Value: TCUDAGraphicResource);
  protected
    { Protected declaration }
    procedure DoProduce(Sender: TObject; var ARci: TRenderContextInfo);
      override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    {: CUDA's function that will produce vertex data }
    property Manufacturer: TCUDAFunction read FManufacturer
      write SetManufacturer;

    property OpenGLResource: TCUDAGraphicResource read FGLResource
      write SetOpenGLResource;
    {: Call before every Manufacturer launch  }
    property BeforeLaunch: TCUDAFactoryBeforeLaunch
      read FBeforeLaunch write FBeforeLaunch;
    {: Acivity switch }
    property Active;
    {: Define mode of manufacturer launching:
      flSingle - single launch,
      flOnePerAtttribute - one launch per attribute and indexes }
    property Launching;
  end;

implementation

uses
  GLStrings;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAGLImageResource'}{$ENDIF}
// ------------------
// ------------------ TCUDAGLImageResource ------------------
// ------------------

constructor TCUDAGLImageResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle[0] := nil;
  fResourceType := rtImage;
  FGLContextHandle := TGLVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAGLImageResource.Destroy;
begin
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAGLImageResource.SetMaterialLibrary(const Value:
  TGLMaterialLibrary);
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
        DestroyHandle;
    end;
  end;
end;

procedure TCUDAGLImageResource.SetTextureName(const Value: TGLLibMaterialName);
begin
  if fTextureName <> Value then
  begin
    fTextureName := Value;
    DestroyHandle;
  end;
end;

procedure TCUDAGLImageResource.AllocateHandle;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);
var
  texture: TGLTexture;
  status: TCUresult;
  texHandle: GLUInt;
begin
  if Assigned(FMaterialLibrary) and (Length(FTextureName) > 0) then
  begin
    texture := FMaterialLibrary.TextureByName(FTextureName);
    if Assigned(texture) then
    begin
      texHandle := texture.Handle;
      if texHandle = 0 then
        raise
          EGLS_CUDA.Create('TCUDAGLImageResource.AllocateHandle: OpenGL texture is not created.');
      DestroyHandle;
      Context.Requires;
      status := cuGraphicsGLRegisterImage(FHandle[0], texHandle,
        texture.Image.NativeTextureTarget, cMapping[fMapping]);
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAGLImageResource.AllocateHandle: ' +
          GetCUDAAPIerrorString(status));
      if FGLContextHandle.Handle = 0 then
        FGLContextHandle.AllocateHandle;
      FRegistered := true;
    end;
  end;
end;

procedure TCUDAGLImageResource.DestroyHandle;
var
  status: TCUresult;
begin
  if Assigned(FHandle[0]) then
  begin
    Context.Requires;
    status := cuGraphicsUnregisterResource(FHandle[0]);
    Context.Release;
    if status <> CUDA_SUCCESS then
    begin
      raise EGLS_CUDA.Create('TCUDAGLImageResource.DestroyHandle: ' +
        GetCUDAAPIerrorString(status));
    end;
    FHandle[0] := nil;
  end;
  FRegistered := false;
end;

procedure TCUDAGLImageResource.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (AComponent = fMaterialLibrary) and (Operation = opRemove) then
  begin
    fMaterialLibrary := nil;
    fTextureName := '';
    DestroyHandle;
  end;
end;

procedure TCUDAGLImageResource.MapResources;
var
  status: TCUresult;
begin
  if not FRegistered then
    AllocateHandle;

  if FMapCounter=0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      status := cuGraphicsMapResources(1, @FHandle[0], nil);
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAGLImageResource.MapResources: ' +
          GetCUDAAPIerrorString(status));
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAGLImageResource.UnMapResources;
var
  status: TCUresult;
begin
  if FMapCounter>0 then
    Dec(FMapCounter);

  if FMapCounter=0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      status := cuGraphicsUnMapResources(1, @FHandle[0], nil);
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAGLImageResource.UnMapResources: ' +
          GetCUDAAPIerrorString(status));
    end;
  end;
end;

procedure TCUDAGLImageResource.BindArrayToImage(var cudaArray: TCUDAMemData;
  ALeyer, ALevel: LOngWord);
var
  status: TCUresult;
  texture: TGLTexture;
  newArray: PCUarray;
begin
  if FMapCounter=0 then
    exit;
  Context.Requires;
  status := cuGraphicsSubResourceGetMappedArray(
    newArray, FHandle[0], ALeyer, ALevel);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise
      EGLS_CUDA.Create('TCUDAGLGeometryResource.GetAttributeDataAddress: ' +
      GetCUDAAPIerrorString(status));

  texture := FMaterialLibrary.TextureByName(FTextureName);
  SetArray(cudaArray, newArray, True, texture.TexDepth > 0);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAGLGeometryResource'}{$ENDIF}
// ------------------
// ------------------ TCUDAGLGeometryResource ------------------
// ------------------

constructor TCUDAGLGeometryResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle[0] := nil;
  FHandle[1] := nil;
  FResourceType := rtBuffer;
  FAttrInfoReady := false;
  FMapCounter := 0;
  FRegistered := false;
  FGLContextHandle := TGLVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAGLGeometryResource.Destroy;
begin
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAGLGeometryResource.SetSceneObject(const Value:
  TGL3xBaseSceneObject);
begin
  if fSceneObject <> Value then
  begin
    if Assigned(fSceneObject) then
      fSceneObject.RemoveFreeNotification(Self);
    fSceneObject := Value;
    if Assigned(fSceneObject) then
      fSceneObject.FreeNotification(Self);
    DestroyHandle;
  end;
end;

procedure TCUDAGLGeometryResource.AllocateHandle;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);
var
  status: TCUresult;
  buffer: GLUInt;

begin
  buffer := FSceneObject.BuiltProperties.VertexBufferHandle;
  if buffer = 0 then
    exit;
  FAttrInfoReady := FSceneObject.Material.GetAttributes(FAttributes);

  Context.Requires;
  status := cuGraphicsGLRegisterBuffer(FHandle[0], buffer, cMapping[FMapping]);
  buffer := FSceneObject.BuiltProperties.IndexBufferHandle;
  if buffer <> 0 then
    ErrorCollect(status, cuGraphicsGLRegisterBuffer(FHandle[1], buffer,
      cMapping[FMapping]));
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAGLGeometryResource.AllocateHandle: ' +
      GetCUDAAPIerrorString(status));

  if FGLContextHandle.Handle = 0 then
    FGLContextHandle.AllocateHandle;
  FRegistered := true;
end;

procedure TCUDAGLGeometryResource.DestroyHandle;
var
  status: TCUresult;
begin
  while FMapCounter>0 do
    UnMapResources;

  if Assigned(fHandle[0]) then
  begin
    Context.Requires;
    status := cuGraphicsUnregisterResource(fHandle[0]);
    if Assigned(fHandle[1]) then
      ErrorCollect(status, cuGraphicsUnregisterResource(fHandle[1]));
    Context.Release;
    if status <> CUDA_SUCCESS then
      raise EGLS_CUDA.Create('TCUDAGLGeometryResource.DestroyHandle: ' +
        GetCUDAAPIerrorString(status));
    fHandle[0] := nil;
    fHandle[1] := nil;
  end;
  FAttrInfoReady := false;
  FRegistered := false;
end;

procedure TCUDAGLGeometryResource.Notification(AComponent: TComponent;
  Operation:
  TOperation);
begin
  inherited;
  if (AComponent = FSceneObject) and (Operation = opRemove) then
  begin
    FSceneObject := nil;
    DestroyHandle;
  end;
end;

procedure TCUDAGLGeometryResource.MapResources;
var
  status: TCUresult;
  count: Integer;
begin
  if not FRegistered then
    AllocateHandle;

  if FMapCounter=0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      status := cuGraphicsMapResources(count, @FHandle[0], nil);
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAGLGeometryResource.MapResources: ' +
          GetCUDAAPIerrorString(status));
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAGLGeometryResource.UnMapResources;
var
  status: TCUresult;
  count: Integer;
begin
  if FMapCounter>0 then
    Dec(FMapCounter);

  if FMapCounter=0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      status := cuGraphicsUnMapResources(count, @FHandle[0], nil);
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAGLGeometryResource.UnMapResources: ' +
          GetCUDAAPIerrorString(status));
    end;
  end;
end;

function TCUDAGLGeometryResource.GetAttribute(Attr: Integer): TGLSLAttribute;
begin
  Result := FAttributes[Attr];
end;

function TCUDAGLGeometryResource.GetAttributeDataSize(Attr: Integer): LongWord;
var
  typeSize: LongWord;
begin
  Result := 0;
  if not FAttrInfoReady then
  begin
    FAttrInfoReady := FSceneObject.Material.GetAttributes(FAttributes);
    if not FAttrInfoReady then
      exit;
  end;
  if FAttributes[Attr].DataType = GLSLTypeUndefined then
    exit;
  case FAttributes[Attr].DataType of
    GLSLType1F: typeSize := SizeOf(GLFloat);
    GLSLType2F: typeSize := 2 * SizeOf(GLFloat);
    GLSLType3F: typeSize := 3 * SizeOf(GLFloat);
    GLSLType4F: typeSize := 4 * SizeOf(GLFloat);
    GLSLType1I: typeSize := SizeOf(GLInt);
    GLSLType2I: typeSize := 2 * SizeOf(GLInt);
    GLSLType3I: typeSize := 3 * SizeOf(GLInt);
    GLSLType4I: typeSize := 4 * SizeOf(GLInt);
    GLSLType4UB: typeSize := 4 * SizeOf(GLUbyte);
  else
    begin
      Assert(False, glsErrorEx + glsUnknownType);
      typeSize := 0;
    end;
  end;
  Result := LongWord(FSceneObject.BuiltProperties.VertexNumber) * typeSize;
end;

function TCUDAGLGeometryResource.GetAttributeDataAddress(Attr: Integer):
  Pointer;
var
  i: Integer;
  Size: Cardinal;
  DevPtr: Pointer;
  status: TCUresult;
begin
  Result := nil;
  if FMapCounter=0 then
    exit;
  for i := 0 to Attr - 1 do
    Inc(PByte(Result), GetAttributeDataSize(i));
  Context.Requires;
  DevPtr := nil;
  status := cuGraphicsResourceGetMappedPointer(DevPtr, Size, FHandle[0]);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise
      EGLS_CUDA.Create('TCUDAGLGeometryResource.GetAttributeDataAddress: ' +
      GetCUDAAPIerrorString(status));

  if Cardinal(Result) + GetAttributeDataSize(Attr) > Size then
    raise
      EGLS_CUDA.Create('TCUDAGLGeometryResource.GetAttributeDataAddress: The amount of device''s data less then size of attribute''s data.');

  Inc(Pbyte(Result), Cardinal(DevPtr));
end;

function TCUDAGLGeometryResource.GetIndexDataSize: LongWord;
begin
  if FSceneObject.BuiltProperties.IndexBufferHandle <> 0 then
    Result := FSceneObject.BuiltProperties.VertexNumber * SizeOf(GLUInt)
  else
    Result := 0;
end;

function TCUDAGLGeometryResource.GetIndexDataAddress: Pointer;
var
  Size: Cardinal;
  DevPtr: Pointer;
  status: TCUresult;
begin
  Result := nil;
  if (FHandle[1] = nil) and (FMapCounter=0) then
    exit;
  Context.Requires;
  DevPtr := nil;
  status := cuGraphicsResourceGetMappedPointer(DevPtr, Size, FHandle[1]);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAGLGeometryResource.GetIndexDataAddress: ' +
      GetCUDAAPIerrorString(status));

  if GetIndexDataSize > Size then
    raise
      EGLS_CUDA.Create('TCUDAGLGeometryResource.GetIndexDataAddress: The amount of device''s data less then size of indexes data.');

  Inc(Pbyte(Result), Cardinal(DevPtr));
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSCUDAFactory'}{$ENDIF}
// ------------------
// ------------------ TGLSCUDAFactory ------------------
// ------------------

constructor TGLSCUDAFactory.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TGLSCUDAFactory.Destroy;
begin
  SetManufacturer(nil);
  inherited;
end;

procedure TGLSCUDAFactory.DoProduce(Sender: TObject; var ARci:
  TRenderContextInfo);
var
  i: Integer;
  GR: TCUDAGLGeometryResource;
  IR: TCUDAGLImageResource;

  procedure ProdeuceData;
  begin
    if Assigned(FBeforeLaunch) then
      FBeforeLaunch(Self, i);
    if Assigned(FManufacturer) then
      FManufacturer.Launch;
  end;

begin
  if Sender = nil then
    exit;

  if FGLResource is TCUDAGLGeometryResource then
  begin
    // Produce geomtry resource
    GR := TCUDAGLGeometryResource(FGLResource);
    if Cardinal(Sender) <>
       Cardinal(GR.FSceneObject) then
      exit;
    GR.MapResources;
    // Produce vertex attributes
    case Launching of
      flSingle:
        begin
          FProducedAttribute := @GR.FAttributes[0];
          ProdeuceData;
        end;
      flOnePerAtttribute:
        begin
          for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
            if GR.FAttributes[i].DataType <> GLSLTypeUndefined then
            begin
              FProducedAttribute := @GR.FAttributes[i];
              ProdeuceData;
            end;
        end;
    else
      Assert(False, glsErrorEx + glsUnknownType);
    end;
    // Produce indexes
    if GR.GetIndexDataSize > 0 then
    begin
      FProducedAttribute := @attrIndex;
      ProdeuceData;
    end;
    GR.UnMapResources;
    FProducedAttribute := nil;
  end
  // Produce image resource
  else if FGLResource is TCUDAGLImageResource then
  begin
    IR := TCUDAGLImageResource(FGLResource);
    IR.MapResources;
    if Assigned(FBeforeLaunch) then
      FBeforeLaunch(Self, 0);
    if Assigned(FManufacturer) then
      FManufacturer.Launch;
    IR.UnMapResources;
  end;

end;

procedure TGLSCUDAFactory.SetManufacturer(Value: TCUDAFunction);
begin
  if Value <> FManufacturer then
  begin
    if Assigned(FManufacturer) then
      FManufacturer.RemoveFreeNotification(Self);
    FManufacturer := Value;
    if Assigned(FManufacturer) then
      FManufacturer.FreeNotification(Self);
  end;
end;

procedure TGLSCUDAFactory.SetOpenGLResource(Value:
  TCUDAGraphicResource);
begin
  if Value <> FGLResource then
  begin
    if Assigned(FGLResource) then
      FGLResource.RemoveFreeNotification(Self);
    FGLResource := Value;
    if Assigned(FGLResource) then
      FGLResource.FreeNotification(Self);
  end;
end;

procedure TGLSCUDAFactory.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FManufacturer then
      FManufacturer := nil
    else if AComponent = FGLResource then
      FGLResource := nil;
  inherited;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TCUDAGLImageResource, TCUDAGLGeometryResource,
    TGLSCUDAFactory]);

end.

