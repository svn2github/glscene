//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    This shader allows to apply multiple textures, gathering them from existing materials.
    This allows saving resources, since you can reference the textures of any material in
    any materialLibrary.
    Note that actually the component references a Material (not a texture) but
    it uses that material's texture. The referenced material settings will be ignored,
    but the texture's settings (like TextureMode, ImageGamma, ImageBrightness) will be used.
    Instead the local material settings (listed in the collection) will be used.
            
 }

unit VKS.TextureSharingShader;

interface

uses
  System.Classes, System.SysUtils,
  
  VKS.Scene, VKS.VectorGeometry, VKS.Color, VKS.Material, VKS.Strings,
  VKS.VectorFileObjects, VKS.XOpenGL, VKS.State, VKS.PersistentClasses,
  VKS.CrossPlatform, VKS.Coordinates, VKS.RenderContextInfo;

type
  TVKTextureSharingShader = class;

  TVKTextureSharingShaderMaterial = class(TVKInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
    FTextureMatrix: TMatrix;
    FNeedToUpdateTextureMatrix: Boolean;
    FTextureMatrixIsUnitary: Boolean;

    FLibMaterial: TVKLibMaterial;
    FTexOffset: TVKCoordinates2;
    FTexScale: TVKCoordinates2;
    FBlendingMode: TBlendingMode;
    FSpecular: TVKColor;
    FAmbient: TVKColor;
    FDiffuse: TVKColor;
    FEmission: TVKColor;
    FShininess: TShininess;
    FMaterialLibrary: TVKMaterialLibrary;
    FLibMaterialName: TVKLibMaterialName;

    procedure SetAmbient(const Value: TVKColor);
    procedure SetDiffuse(const Value: TVKColor);
    procedure SetEmission(const Value: TVKColor);
    procedure SetShininess(const Value: TShininess);
    procedure SetSpecular(const Value: TVKColor);
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    procedure SetLibMaterialName(const Value: TVKLibMaterialName);
    procedure SetBlendingMode(const Value: TBlendingMode);
    procedure SetLibMaterial(const Value: TVKLibMaterial);
    procedure SetTexOffset(const Value: TVKCoordinates2);
    procedure SetTexScale(const Value: TVKCoordinates2);

    function GetTextureMatrix: TMatrix;
    function GetTextureMatrixIsUnitary: Boolean;
  protected
    procedure coordNotifychange(Sender: TObject);
    procedure OtherNotifychange(Sender: TObject);

    function GetDisplayName: string; override;
    function GetTextureSharingShader: TVKTextureSharingShader;

    // Implementing IVKMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary; virtual;

  public
    procedure Apply(var rci: TVKRenderContextInfo);
    procedure UnApply(var rci: TVKRenderContextInfo);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property LibMaterial: TVKLibMaterial read FLibMaterial write SetLibMaterial;

    property TextureMatrix: TMatrix read GetTextureMatrix;
    property TextureMatrixIsUnitary: Boolean read GetTextureMatrixIsUnitary;
  published

    property TexOffset: TVKCoordinates2 read FTexOffset write SetTexOffset;
    property TexScale: TVKCoordinates2 read FTexScale write SetTexScale;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode;
    property Emission: TVKColor read FEmission write SetEmission;
    property Ambient: TVKColor read FAmbient write SetAmbient;
    property Diffuse: TVKColor read FDiffuse write SetDiffuse;
    property Specular: TVKColor read FSpecular write SetSpecular;
    property Shininess: TShininess read FShininess write SetShininess;
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TVKLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end;

  TVKTextureSharingShaderMaterials = class(TOwnedCollection)
  protected
    function GetItems(const AIndex: Integer): TVKTextureSharingShaderMaterial;
    procedure SetItems(const AIndex: Integer; const Value: TVKTextureSharingShaderMaterial);
    function GetParent: TVKTextureSharingShader;
  public
    function Add: TVKTextureSharingShaderMaterial;
    constructor Create(AOwner: TVKTextureSharingShader);
    property Items[const AIndex: Integer]: TVKTextureSharingShaderMaterial read GetItems write SetItems; default;
  end;

  TVKTextureSharingShader = class(TVKShader)
  private
    FMaterials: TVKTextureSharingShaderMaterials;
    FCurrentPass: Integer;
    procedure SetMaterials(const Value: TVKTextureSharingShaderMaterials);
  protected
    procedure DoApply(var rci: TVKRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLibMaterial(const ALibMaterial: TVKLibMaterial): TVKTextureSharingShaderMaterial;
    function FindLibMaterial(const ALibMaterial: TVKLibMaterial): TVKTextureSharingShaderMaterial;
  published
    property Materials: TVKTextureSharingShaderMaterials read FMaterials write SetMaterials;
  end;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
implementation
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

{ TVKTextureSharingShaderMaterial }

procedure TVKTextureSharingShaderMaterial.Apply(var rci: TVKRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;
  xgl.BeginUpdate;
  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
      ssReplace:
      begin
        FLibMaterial.Shader.Apply(rci, FLibMaterial);
        Exit;
      end;
    end;
  end;
  if not FLibMaterial.Material.Texture.Disabled then
  begin
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.VKStates.SetTextureMatrix(TextureMatrix);
    end;
  end;

  if moNoLighting in FLibMaterial.Material.MaterialOptions then
    rci.VKStates.Disable(stLighting);

  if stLighting in rci.VKStates.States then
  begin
    rci.VKStates.SetMaterialColors(cmFront,
      Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, Shininess);
    rci.VKStates.PolygonMode :=FLibMaterial.Material.PolygonMode;
  end
  else
    FLibMaterial.Material.FrontProperties.ApplyNoLighting(rci, cmFront);
  if (stCullFace in rci.VKStates.States) then
  begin
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault: if not rci.bufferFaceCull then
        begin
          rci.VKStates.Disable(stCullFace);
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
        end;
      fcCull: ; // nothing to do
      fcNoCull:
      begin
        rci.VKStates.Disable(stCullFace);
        FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      else
        Assert(False);
    end;
  end
  else
  begin
    // currently NOT culling
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault:
      begin
        if rci.bufferFaceCull then
          rci.VKStates.Enable(stCullFace)
        else
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      fcCull: rci.VKStates.Enable(stCullFace);
      fcNoCull: FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      else
        Assert(False);
    end;
  end;

  // Apply Blending mode
  if not rci.ignoreBlendingRequests then
    case BlendingMode of
      bmOpaque:
      begin
        rci.VKStates.Disable(stBlend);
        rci.VKStates.Disable(stAlphaTest);
      end;
      bmTransparency:
      begin
        rci.VKStates.Enable(stBlend);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      bmAdditive:
      begin
        rci.VKStates.Enable(stBlend);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
      bmAlphaTest50:
      begin
        rci.VKStates.Disable(stBlend);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetAlphaFunction(cfGEqual, 0.5);
      end;
      bmAlphaTest100:
      begin
        rci.VKStates.Disable(stBlend);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetAlphaFunction(cfGEqual, 1.0);
      end;
      bmModulate:
      begin
        rci.VKStates.Enable(stBlend);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetBlendFunc(bfDstColor, bfZero);
      end;
      else
        Assert(False);
    end;
  // Fog switch
  if moIgnoreFog in FLibMaterial.Material.MaterialOptions then
  begin
    if stFog in rci.VKStates.States then
    begin
      rci.VKStates.Disable(stFog);
      Inc(rci.fogDisabledCounter);
    end;
  end;

  if not Assigned(FLibMaterial.Material.TextureEx) then
  begin
    if Assigned(FLibMaterial.Material.Texture) then
      FLibMaterial.Material.Texture.Apply(rci);
  end
  else
  begin
    if Assigned(FLibMaterial.Material.Texture) and not FLibMaterial.Material.TextureEx.IsTextureEnabled(0) then
      FLibMaterial.Material.Texture.Apply(rci)
    else
    if FLibMaterial.Material.TextureEx.Count > 0 then
      FLibMaterial.Material.TextureEx.Apply(rci);
  end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
    end;
  end;
  xgl.EndUpdate;
end;

procedure TVKTextureSharingShaderMaterial.coordNotifychange(Sender: TObject);
begin
  FNeedToUpdateTextureMatrix := True;
  GetTextureSharingShader.NotifyChange(Self);
end;

constructor TVKTextureSharingShaderMaterial.Create(Collection: TCollection);
begin
  inherited;
  FSpecular := TVKColor.Create(Self);
  FSpecular.OnNotifyChange := OtherNotifychange;
  FAmbient := TVKColor.Create(Self);
  FAmbient.OnNotifyChange := OtherNotifychange;
  FDiffuse := TVKColor.Create(Self);
  FDiffuse.OnNotifyChange := OtherNotifychange;
  FEmission := TVKColor.Create(Self);
  FEmission.OnNotifyChange := OtherNotifychange;

  FTexOffset := TVKCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2d);
  FTexOffset.OnNotifyChange := coordNotifychange;

  FTexScale := TVKCoordinates2.CreateInitialized(Self, XYZHmgVector, csPoint2d);
  FTexScale.OnNotifyChange := coordNotifychange;
  FNeedToUpdateTextureMatrix := True;
end;

destructor TVKTextureSharingShaderMaterial.Destroy;
begin
  FSpecular.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FTexOffset.Free;
  FTexScale.Free;
  inherited;
end;


function TVKTextureSharingShaderMaterial.GetDisplayName: string;
var
  st: string;
begin
  if Assigned(MaterialLibrary) then
    st := MaterialLibrary.Name
  else
    st := '';
  Result := '[' + st + '.' + Self.LibMaterialName + ']';
end;

function TVKTextureSharingShaderMaterial.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TVKTextureSharingShaderMaterial.GetTextureMatrix: TMatrix;
begin
  if FNeedToUpdateTextureMatrix then
  begin
    if not (TexOffset.Equals(NullHmgVector) and TexScale.Equals(XYZHmgVector)) then
    begin
      FTextureMatrixIsUnitary := False;
      FTextureMatrix := CreateScaleAndTranslationMatrix(TexScale.AsVector, TexOffset.AsVector)
    end
    else
      FTextureMatrixIsUnitary := True;
    FNeedToUpdateTextureMatrix := False;
  end;
  Result := FTextureMatrix;
end;

function TVKTextureSharingShaderMaterial.GetTextureMatrixIsUnitary: Boolean;
begin
  if FNeedToUpdateTextureMatrix then
    GetTextureMatrix;
  Result := FTextureMatrixIsUnitary;
end;

function TVKTextureSharingShaderMaterial.GetTextureSharingShader: TVKTextureSharingShader;
begin
  if Collection is TVKTextureSharingShaderMaterials then
    Result := TVKTextureSharingShaderMaterials(Collection).GetParent
  else
    Result := nil;
end;

procedure TVKTextureSharingShaderMaterial.OtherNotifychange(Sender: TObject);
begin
  GetTextureSharingShader.NotifyChange(Self);
end;

procedure TVKTextureSharingShaderMaterial.SetAmbient(const Value: TVKColor);
begin
  FAmbient.Assign(Value);
end;

procedure TVKTextureSharingShaderMaterial.SetBlendingMode(const Value: TBlendingMode);
begin
  FBlendingMode := Value;
end;

procedure TVKTextureSharingShaderMaterial.SetDiffuse(const Value: TVKColor);
begin
  FDiffuse.Assign(Value);
end;

procedure TVKTextureSharingShaderMaterial.SetEmission(const Value: TVKColor);
begin
  FEmission.Assign(Value);
end;

procedure TVKTextureSharingShaderMaterial.SetLibMaterialName(const Value: TVKLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TVKTextureSharingShaderMaterial.SetLibMaterial(const Value: TVKLibMaterial);
begin
  FLibMaterial := Value;
  if FLibMaterial <> nil then
  begin
    FLibMaterialName := FLibMaterial.DisplayName;
    FMaterialLibrary := TVKMaterialLibrary(TVKLibMaterials(Value.Collection).Owner);
    if not (csloading in GetTextureSharingShader.ComponentState) then
    begin
      FTexOffset.Assign(FLibMaterial.TextureOffset);
      FTexScale.Assign(FLibMaterial.TextureScale);
      FBlendingMode := FLibMaterial.Material.BlendingMode;
      fEmission.Assign(FLibMaterial.Material.FrontProperties.Emission);
      fAmbient.Assign(FLibMaterial.Material.FrontProperties.Ambient);
      fDiffuse.Assign(FLibMaterial.Material.FrontProperties.Diffuse);
      fSpecular.Assign(FLibMaterial.Material.FrontProperties.Specular);
      fShininess := FLibMaterial.Material.FrontProperties.Shininess;
    end;
  end;
end;


procedure TVKTextureSharingShaderMaterial.SetMaterialLibrary(const Value: TVKMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TVKTextureSharingShaderMaterial.SetShininess(const Value: TShininess);
begin
  FShininess := Value;
end;

procedure TVKTextureSharingShaderMaterial.SetSpecular(const Value: TVKColor);
begin
  FSpecular.Assign(Value);
end;

procedure TVKTextureSharingShaderMaterial.SetTexOffset(const Value: TVKCoordinates2);
begin
  FTexOffset.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TVKTextureSharingShaderMaterial.SetTexScale(const Value: TVKCoordinates2);
begin
  FTexScale.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TVKTextureSharingShaderMaterial.UnApply(var rci: TVKRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.UnApply(rci);
      ssReplace:
      begin
        FLibMaterial.Shader.UnApply(rci);
        Exit;
      end;
    end;
  end;

  FLibMaterial.Material.UnApply(rci);

  if not FLibMaterial.Material.Texture.Disabled then
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.VKStates.ResetTextureMatrix;
    end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.UnApply(rci);
    end;
  end;
end;

{ TVKTextureSharingShader }

function TVKTextureSharingShader.AddLibMaterial(const ALibMaterial: TVKLibMaterial): TVKTextureSharingShaderMaterial;
begin
  Result := FMaterials.Add;
  Result.SetLibMaterial(ALibMaterial);
end;

constructor TVKTextureSharingShader.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVKTextureSharingShaderMaterials.Create(Self);
  ShaderStyle := ssReplace;
end;

destructor TVKTextureSharingShader.Destroy;
begin
  FMaterials.Free;
  inherited;
end;

procedure TVKTextureSharingShader.DoApply(var rci: TVKRenderContextInfo; Sender: TObject);
begin
  if Materials.Count > 0 then
  begin
    rci.VKStates.Enable(stDepthTest);
    rci.VKStates.DepthFunc := cfLEqual;
    Materials[0].Apply(rci);
    FCurrentPass := 1;
  end;
end;

function TVKTextureSharingShader.DoUnApply(var rci: TVKRenderContextInfo): Boolean;
begin
  Result := False;
  if Materials.Count > 0 then
  begin
    Materials[FCurrentPass - 1].UnApply(rci);
    if FCurrentPass < Materials.Count then
    begin
      Materials[FCurrentPass].Apply(rci);
      Inc(FCurrentPass);
      Result := True;
    end
    else
    begin
      rci.VKStates.DepthFunc := cfLess;
      rci.VKStates.Disable(stBlend);
      rci.VKStates.Disable(stAlphaTest);
      FCurrentPass := 0;
    end;
  end;
end;

function TVKTextureSharingShader.FindLibMaterial(const ALibMaterial: TVKLibMaterial): TVKTextureSharingShaderMaterial;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMaterials.Count - 1 do
    if FMaterials[I].FLibMaterial = ALibMaterial then
    begin
      Result := FMaterials[I];
      Break;
    end;
end;

procedure TVKTextureSharingShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TVKMaterialLibrary then
    begin
      for I := 0 to Materials.Count - 1 do
      begin
        if Materials.Items[I].MaterialLibrary = AComponent then
          Materials.Items[I].MaterialLibrary := nil;
      end;
    end;
  end;
end;

procedure TVKTextureSharingShader.SetMaterials(const Value: TVKTextureSharingShaderMaterials);
begin
  FMaterials.Assign(Value);
end;

{ TVKTextureSharingShaderMaterials }

function TVKTextureSharingShaderMaterials.Add: TVKTextureSharingShaderMaterial;
begin
  Result := (inherited Add) as TVKTextureSharingShaderMaterial;
end;

constructor TVKTextureSharingShaderMaterials.Create(AOwner: TVKTextureSharingShader);
begin
  inherited Create(AOwner, TVKTextureSharingShaderMaterial);
end;

function TVKTextureSharingShaderMaterials.GetItems(const AIndex: Integer): TVKTextureSharingShaderMaterial;
begin
  Result := (inherited Items[AIndex]) as TVKTextureSharingShaderMaterial;
end;

function TVKTextureSharingShaderMaterials.GetParent: TVKTextureSharingShader;
begin
  Result := TVKTextureSharingShader(GetOwner);
end;

procedure TVKTextureSharingShaderMaterials.SetItems(const AIndex: Integer; const Value: TVKTextureSharingShaderMaterial);
begin
  inherited Items[AIndex] := Value;
end;


initialization
  RegisterClasses([TVKTextureSharingShader, TVKTextureSharingShaderMaterials,
                   TVKTextureSharingShaderMaterial]);

end.
