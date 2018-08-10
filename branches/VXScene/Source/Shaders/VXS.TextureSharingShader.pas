//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
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

unit VXS.TextureSharingShader;

interface

uses
  System.Classes,
  System.SysUtils,

  VXS.XOpenGL,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.Color,
  VXS.Material,
  VXS.Strings,
  VXS.VectorFileObjects,
  VXS.State,
  VXS.PersistentClasses,
  VXS.Coordinates,
  VXS.RenderContextInfo;

type
  TVXTextureSharingShader = class;

  TVXTextureSharingShaderMaterial = class(TVXInterfacedCollectionItem, IVXMaterialLibrarySupported)
  private
    FTextureMatrix: TMatrix;
    FNeedToUpdateTextureMatrix: Boolean;
    FTextureMatrixIsUnitary: Boolean;
    FLibMaterial: TVXLibMaterial;
    FTexOffset: TVXCoordinates2;
    FTexScale: TVXCoordinates2;
    FBlendingMode: TBlendingMode;
    FSpecular: TVXColor;
    FAmbient: TVXColor;
    FDiffuse: TVXColor;
    FEmission: TVXColor;
    FShininess: TShininess;
    FMaterialLibrary: TVXMaterialLibrary;
    FLibMaterialName: TVXLibMaterialName;
    procedure SetAmbient(const Value: TVXColor);
    procedure SetDiffuse(const Value: TVXColor);
    procedure SetEmission(const Value: TVXColor);
    procedure SetShininess(const Value: TShininess);
    procedure SetSpecular(const Value: TVXColor);
    procedure SetMaterialLibrary(const Value: TVXMaterialLibrary);
    procedure SetLibMaterialName(const Value: TVXLibMaterialName);
    procedure SetBlendingMode(const Value: TBlendingMode);
    procedure SetLibMaterial(const Value: TVXLibMaterial);
    procedure SetTexOffset(const Value: TVXCoordinates2);
    procedure SetTexScale(const Value: TVXCoordinates2);
    function GetTextureMatrix: TMatrix;
    function GetTextureMatrixIsUnitary: Boolean;
  protected
    procedure coordNotifychange(Sender: TObject);
    procedure OtherNotifychange(Sender: TObject);
    function GetDisplayName: string; override;
    function GetTextureSharingShader: TVXTextureSharingShader;
    // Implementing IVKMaterialLibrarySupported.
    function GetMaterialLibrary: TVXAbstractMaterialLibrary; virtual;
  public
    procedure Apply(var rci: TVXRenderContextInfo);
    procedure UnApply(var rci: TVXRenderContextInfo);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property LibMaterial: TVXLibMaterial read FLibMaterial write SetLibMaterial;
    property TextureMatrix: TMatrix read GetTextureMatrix;
    property TextureMatrixIsUnitary: Boolean read GetTextureMatrixIsUnitary;
  published
    property TexOffset: TVXCoordinates2 read FTexOffset write SetTexOffset;
    property TexScale: TVXCoordinates2 read FTexScale write SetTexScale;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode;
    property Emission: TVXColor read FEmission write SetEmission;
    property Ambient: TVXColor read FAmbient write SetAmbient;
    property Diffuse: TVXColor read FDiffuse write SetDiffuse;
    property Specular: TVXColor read FSpecular write SetSpecular;
    property Shininess: TShininess read FShininess write SetShininess;
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TVXLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end;

  TVXTextureSharingShaderMaterials = class(TOwnedCollection)
  protected
    function GetItems(const AIndex: Integer): TVXTextureSharingShaderMaterial;
    procedure SetItems(const AIndex: Integer; const Value: TVXTextureSharingShaderMaterial);
    function GetParent: TVXTextureSharingShader;
  public
    function Add: TVXTextureSharingShaderMaterial;
    constructor Create(AOwner: TVXTextureSharingShader);
    property Items[const AIndex: Integer]: TVXTextureSharingShaderMaterial read GetItems write SetItems; default;
  end;

  TVXTextureSharingShader = class(TVXShader)
  private
    FMaterials: TVXTextureSharingShaderMaterials;
    FCurrentPass: Integer;
    procedure SetMaterials(const Value: TVXTextureSharingShaderMaterials);
  protected
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLibMaterial(const ALibMaterial: TVXLibMaterial): TVXTextureSharingShaderMaterial;
    function FindLibMaterial(const ALibMaterial: TVXLibMaterial): TVXTextureSharingShaderMaterial;
  published
    property Materials: TVXTextureSharingShaderMaterials read FMaterials write SetMaterials;
  end;

//=======================================================================
implementation
//=======================================================================

//----------------------------------------------------------------------------
{ TVXTextureSharingShaderMaterial }
//----------------------------------------------------------------------------

procedure TVXTextureSharingShaderMaterial.Apply(var rci: TVXRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;
  xglBeginUpdate;
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
      rci.VXStates.SetTextureMatrix(TextureMatrix);
    end;
  end;

  if moNoLighting in FLibMaterial.Material.MaterialOptions then
    rci.VXStates.Disable(stLighting);

  if stLighting in rci.VXStates.States then
  begin
    rci.VXStates.SetMaterialColors(cmFront,
      Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, Shininess);
    rci.VXStates.PolygonMode :=FLibMaterial.Material.PolygonMode;
  end
  else
    FLibMaterial.Material.FrontProperties.ApplyNoLighting(rci, cmFront);
  if (stCullFace in rci.VXStates.States) then
  begin
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault: if not rci.bufferFaceCull then
        begin
          rci.VXStates.Disable(stCullFace);
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
        end;
      fcCull: ; // nothing to do
      fcNoCull:
      begin
        rci.VXStates.Disable(stCullFace);
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
          rci.VXStates.Enable(stCullFace)
        else
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      fcCull: rci.VXStates.Enable(stCullFace);
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
        rci.VXStates.Disable(stBlend);
        rci.VXStates.Disable(stAlphaTest);
      end;
      bmTransparency:
      begin
        rci.VXStates.Enable(stBlend);
        rci.VXStates.Enable(stAlphaTest);
        rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      bmAdditive:
      begin
        rci.VXStates.Enable(stBlend);
        rci.VXStates.Enable(stAlphaTest);
        rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
      bmAlphaTest50:
      begin
        rci.VXStates.Disable(stBlend);
        rci.VXStates.Enable(stAlphaTest);
        rci.VXStates.SetAlphaFunction(cfGEqual, 0.5);
      end;
      bmAlphaTest100:
      begin
        rci.VXStates.Disable(stBlend);
        rci.VXStates.Enable(stAlphaTest);
        rci.VXStates.SetAlphaFunction(cfGEqual, 1.0);
      end;
      bmModulate:
      begin
        rci.VXStates.Enable(stBlend);
        rci.VXStates.Enable(stAlphaTest);
        rci.VXStates.SetBlendFunc(bfDstColor, bfZero);
      end;
      else
        Assert(False);
    end;
  // Fog switch
  if moIgnoreFog in FLibMaterial.Material.MaterialOptions then
  begin
    if stFog in rci.VXStates.States then
    begin
      rci.VXStates.Disable(stFog);
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
  xglEndUpdate;
end;

procedure TVXTextureSharingShaderMaterial.coordNotifychange(Sender: TObject);
begin
  FNeedToUpdateTextureMatrix := True;
  GetTextureSharingShader.NotifyChange(Self);
end;

constructor TVXTextureSharingShaderMaterial.Create(Collection: TCollection);
begin
  inherited;
  FSpecular := TVXColor.Create(Self);
  FSpecular.OnNotifyChange := OtherNotifychange;
  FAmbient := TVXColor.Create(Self);
  FAmbient.OnNotifyChange := OtherNotifychange;
  FDiffuse := TVXColor.Create(Self);
  FDiffuse.OnNotifyChange := OtherNotifychange;
  FEmission := TVXColor.Create(Self);
  FEmission.OnNotifyChange := OtherNotifychange;

  FTexOffset := TVXCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2d);
  FTexOffset.OnNotifyChange := coordNotifychange;

  FTexScale := TVXCoordinates2.CreateInitialized(Self, XYZHmgVector, csPoint2d);
  FTexScale.OnNotifyChange := coordNotifychange;
  FNeedToUpdateTextureMatrix := True;
end;

destructor TVXTextureSharingShaderMaterial.Destroy;
begin
  FSpecular.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FTexOffset.Free;
  FTexScale.Free;
  inherited;
end;


function TVXTextureSharingShaderMaterial.GetDisplayName: string;
var
  st: string;
begin
  if Assigned(MaterialLibrary) then
    st := MaterialLibrary.Name
  else
    st := '';
  Result := '[' + st + '.' + Self.LibMaterialName + ']';
end;

function TVXTextureSharingShaderMaterial.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TVXTextureSharingShaderMaterial.GetTextureMatrix: TMatrix;
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

function TVXTextureSharingShaderMaterial.GetTextureMatrixIsUnitary: Boolean;
begin
  if FNeedToUpdateTextureMatrix then
    GetTextureMatrix;
  Result := FTextureMatrixIsUnitary;
end;

function TVXTextureSharingShaderMaterial.GetTextureSharingShader: TVXTextureSharingShader;
begin
  if Collection is TVXTextureSharingShaderMaterials then
    Result := TVXTextureSharingShaderMaterials(Collection).GetParent
  else
    Result := nil;
end;

procedure TVXTextureSharingShaderMaterial.OtherNotifychange(Sender: TObject);
begin
  GetTextureSharingShader.NotifyChange(Self);
end;

procedure TVXTextureSharingShaderMaterial.SetAmbient(const Value: TVXColor);
begin
  FAmbient.Assign(Value);
end;

procedure TVXTextureSharingShaderMaterial.SetBlendingMode(const Value: TBlendingMode);
begin
  FBlendingMode := Value;
end;

procedure TVXTextureSharingShaderMaterial.SetDiffuse(const Value: TVXColor);
begin
  FDiffuse.Assign(Value);
end;

procedure TVXTextureSharingShaderMaterial.SetEmission(const Value: TVXColor);
begin
  FEmission.Assign(Value);
end;

procedure TVXTextureSharingShaderMaterial.SetLibMaterialName(const Value: TVXLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TVXTextureSharingShaderMaterial.SetLibMaterial(const Value: TVXLibMaterial);
begin
  FLibMaterial := Value;
  if FLibMaterial <> nil then
  begin
    FLibMaterialName := FLibMaterial.DisplayName;
    FMaterialLibrary := TVXMaterialLibrary(TVXLibMaterials(Value.Collection).Owner);
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


procedure TVXTextureSharingShaderMaterial.SetMaterialLibrary(const Value: TVXMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TVXTextureSharingShaderMaterial.SetShininess(const Value: TShininess);
begin
  FShininess := Value;
end;

procedure TVXTextureSharingShaderMaterial.SetSpecular(const Value: TVXColor);
begin
  FSpecular.Assign(Value);
end;

procedure TVXTextureSharingShaderMaterial.SetTexOffset(const Value: TVXCoordinates2);
begin
  FTexOffset.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TVXTextureSharingShaderMaterial.SetTexScale(const Value: TVXCoordinates2);
begin
  FTexScale.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TVXTextureSharingShaderMaterial.UnApply(var rci: TVXRenderContextInfo);
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
      rci.VXStates.ResetTextureMatrix;
    end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.UnApply(rci);
    end;
  end;
end;

{ TVXTextureSharingShader }

function TVXTextureSharingShader.AddLibMaterial(const ALibMaterial: TVXLibMaterial): TVXTextureSharingShaderMaterial;
begin
  Result := FMaterials.Add;
  Result.SetLibMaterial(ALibMaterial);
end;

constructor TVXTextureSharingShader.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVXTextureSharingShaderMaterials.Create(Self);
  ShaderStyle := ssReplace;
end;

destructor TVXTextureSharingShader.Destroy;
begin
  FMaterials.Free;
  inherited;
end;

procedure TVXTextureSharingShader.DoApply(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  if Materials.Count > 0 then
  begin
    rci.VXStates.Enable(stDepthTest);
    rci.VXStates.DepthFunc := cfLEqual;
    Materials[0].Apply(rci);
    FCurrentPass := 1;
  end;
end;

function TVXTextureSharingShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
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
      rci.VXStates.DepthFunc := cfLess;
      rci.VXStates.Disable(stBlend);
      rci.VXStates.Disable(stAlphaTest);
      FCurrentPass := 0;
    end;
  end;
end;

function TVXTextureSharingShader.FindLibMaterial(const ALibMaterial: TVXLibMaterial): TVXTextureSharingShaderMaterial;
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

procedure TVXTextureSharingShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TVXMaterialLibrary then
    begin
      for I := 0 to Materials.Count - 1 do
      begin
        if Materials.Items[I].MaterialLibrary = AComponent then
          Materials.Items[I].MaterialLibrary := nil;
      end;
    end;
  end;
end;

procedure TVXTextureSharingShader.SetMaterials(const Value: TVXTextureSharingShaderMaterials);
begin
  FMaterials.Assign(Value);
end;

{ TVXTextureSharingShaderMaterials }

function TVXTextureSharingShaderMaterials.Add: TVXTextureSharingShaderMaterial;
begin
  Result := (inherited Add) as TVXTextureSharingShaderMaterial;
end;

constructor TVXTextureSharingShaderMaterials.Create(AOwner: TVXTextureSharingShader);
begin
  inherited Create(AOwner, TVXTextureSharingShaderMaterial);
end;

function TVXTextureSharingShaderMaterials.GetItems(const AIndex: Integer): TVXTextureSharingShaderMaterial;
begin
  Result := (inherited Items[AIndex]) as TVXTextureSharingShaderMaterial;
end;

function TVXTextureSharingShaderMaterials.GetParent: TVXTextureSharingShader;
begin
  Result := TVXTextureSharingShader(GetOwner);
end;

procedure TVXTextureSharingShaderMaterials.SetItems(const AIndex: Integer; const Value: TVXTextureSharingShaderMaterial);
begin
  inherited Items[AIndex] := Value;
end;

//----------------------------------------------------------------------------
initialization
//----------------------------------------------------------------------------

  RegisterClasses([TVXTextureSharingShader, TVXTextureSharingShaderMaterials,
                   TVXTextureSharingShaderMaterial]);

end.
