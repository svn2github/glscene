//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  A collection of components that generate post effects. 
 
}
unit VXS.PostEffects;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,
  Winapi.OpenGL, Winapi.OpenGLext,
  
  VXS.Scene, VXS.Texture, VXS.Graphics, VXS.Strings,
  VXS.CustomShader, VXS.Context, VXS.VectorGeometry, VXS.RenderContextInfo,
  VXS.Material, VXS.TextureFormat;

type
  EGLPostShaderHolderException = class(Exception);
  TVXPostShaderHolder = class;

  TVXPostShaderCollectionItem = class(TCollectionItem)
  private
    FShader: TVXShader;
    FPostShaderInterface: IGLPostShader;
    procedure SetShader(const Value: TVXShader);
  protected
    function GetRealOwner: TVXPostShaderHolder;
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TVXShader read FShader write SetShader;
  end;

  TVXPostShaderCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TVXPostShaderCollectionItem;
    procedure SetItems(const Index: Integer;
      const Value: TVXPostShaderCollectionItem);
  public
    procedure Remove(const Item: TVXShader);
    function Add: TVXPostShaderCollectionItem;

    property Items[const Index: Integer]: TVXPostShaderCollectionItem read GetItems write SetItems; default;
  end;

  { A class that allows several post-shaders to be applied to the scene,
    one after another. It does not provide any optimizations related to
    multi-shader rendering, just a convenient interface. }
  TVXPostShaderHolder = class(TVXBaseSCeneObject)
  private
    FShaders: TVXPostShaderCollection;
    FTempTexture: TVXTextureHandle;
    FPreviousViewportSize: TVXSize;
    FTempTextureTarget: TVXTextureTarget;
    procedure SetShaders(const Value: TVXPostShaderCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci : TVXRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
  published
    property TempTextureTarget: TVXTextureTarget read FTempTextureTarget write FTempTextureTarget default ttTexture2d;
    property Shaders: TVXPostShaderCollection read FShaders write SetShaders;

    // Publish some stuff from TVXBaseSceneObject.
    property Visible;
    property OnProgress;
  end;


  TVXPostEffectColor = record
    R, G, B, A: GLubyte;
  end;

  TVXPostEffectBuffer = array of TVXPostEffectColor;

  TVXOnCustomPostEffectEvent = procedure(Sender: TObject; var rci : TVXRenderContextInfo; var Buffer: TVXPostEffectBuffer) of object;

  { Some presets for TVXPostEffect:
       pepNone - does nothing.
       pepGray - makes picture gray.
       pepNegative - inverts all colors.
       pepDistort - simulates shaky TV image.
       pepNoise - just adds random niose.
       pepNightVision - simulates nightvision goggles.
       pepBlur - blurs the scene.
       pepCustom - calls the OnCustomEffect event.
  }
  TVXPostEffectPreset = (pepNone, pepGray, pepNegative, pepDistort, pepNoise,
                         pepNightVision, pepBlur, pepCustom);

  { Provides a simple way to producing post-effects without shaders. 
     It is slow as hell, but it's worth it in some cases.}
  TVXPostEffect = class(TVXBaseSCeneObject)
  private
    FOnCustomEffect: TVXOnCustomPostEffectEvent;
    FPreset: TVXPostEffectPreset;
    FRenderBuffer: TVXPostEffectBuffer;
  protected
    // May be should be private...
    procedure MakeGrayEffect; virtual;
    procedure MakeNegativeEffect; virtual;
    procedure MakeDistortEffect; virtual;
    procedure MakeNoiseEffect; virtual;
    procedure MakeNightVisionEffect; virtual;
    procedure MakeBlurEffect(var rci : TVXRenderContextInfo); virtual;
    procedure DoOnCustomEffect(var rci : TVXRenderContextInfo; var Buffer: TVXPostEffectBuffer); virtual;
  public
    procedure DoRender(var rci : TVXRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Preset: TVXPostEffectPreset read FPreset write FPreset default pepNone;
    // User creates this effect.
    property OnCustomEffect: TVXOnCustomPostEffectEvent read FOnCustomEffect write FOnCustomEffect;
    // Publish some stuff from TVXBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;

//-----------------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------------

{ TVXPostEffect }

procedure TVXPostEffect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVXPostEffect then
  begin
    FPreset := TVXPostEffect(Source).FPreset;
  end;
end;

procedure TVXPostEffect.DoOnCustomEffect(
  var rci : TVXRenderContextInfo; var Buffer: TVXPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, rci, Buffer);
end;

procedure TVXPostEffect.DoRender(var rci : TVXRenderContextInfo;
                                      renderSelf, renderChildren : Boolean);
var
  NewScreenSize: Integer;
begin
  if (not rci.ignoreMaterials) and (FPreset <> pepNone) and (rci.drawState <> dsPicking) then
  begin
    NewScreenSize := rci.viewPortSize.cx * rci.viewPortSize.cy;
    if NewScreenSize <> Length(FRenderBuffer) then
      SetLength(FRenderBuffer, NewScreenSize);

    glReadPixels(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
    case FPreset of
      // pepNone is handled in the first line.
      pepGray:        MakeGrayEffect;
      pepNegative:    MakeNegativeEffect;
      pepDistort:     MakeDistortEffect;
      pepNoise:       MakeNoiseEffect;
      pepNightVision: MakeNightVisionEffect;
      pepBlur:        MakeBlurEffect(rci);
      pepCustom:      DoOnCustomEffect(rci, FRenderBuffer);
    else
      Assert(False, strErrorEx + strUnknownType);
    end;
    glDrawPixels(rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
  end;

  // Start rendering children (if any).
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TVXPostEffect.MakeGrayEffect;
var
  I:    Longword;
  gray: GLubyte;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    gray := Round((0.30 * FRenderBuffer[I].r) +
                  (0.59 * FRenderBuffer[I].g) +
                  (0.11 * FRenderBuffer[I].b));
    FRenderBuffer[I].r := gray;
    FRenderBuffer[I].g := gray;
    FRenderBuffer[I].b := gray;
  end;
end;

procedure TVXPostEffect.MakeNegativeEffect;
var
  I: Longword;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    FRenderBuffer[I].r := 255 - FRenderBuffer[I].r;
    FRenderBuffer[I].g := 255 - FRenderBuffer[I].g;
    FRenderBuffer[I].b := 255 - FRenderBuffer[I].b;
  end;
end;

procedure TVXPostEffect.MakeDistortEffect;
var
  I: Integer;
  lMaxLength: Integer;
  lNewIndex: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(10) - 5));
    FRenderBuffer[I].r := FRenderBuffer[lNewIndex].r;
    FRenderBuffer[I].g := FRenderBuffer[lNewIndex].g;
    FRenderBuffer[I].b := FRenderBuffer[lNewIndex].b;
  end;
end;

procedure TVXPostEffect.MakeNoiseEffect;
var
  I:   Longword;
  rnd: Single;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    rnd := 0.25 + Random(75)/100;

    FRenderBuffer[I].r := Round(FRenderBuffer[I].r * rnd);
    FRenderBuffer[I].g := Round(FRenderBuffer[I].g * rnd);
    FRenderBuffer[I].b := Round(FRenderBuffer[I].b * rnd);
  end;
end;

procedure TVXPostEffect.MakeNightVisionEffect;
var
   gray: Single;
   I: Integer;
   lNewIndex, lMaxLength: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(20) - 10));

    gray := 60 + (0.30 * FRenderBuffer[lNewIndex].r) +
                 (0.59 * FRenderBuffer[lNewIndex].g) +
                 (0.11 * FRenderBuffer[lNewIndex].b);

    FRenderBuffer[I].r := Round(gray * 0.25);
    FRenderBuffer[I].g := Round((gray + 4) * 0.6);
    FRenderBuffer[I].b := Round((gray + 4) * 0.11);
  end;
end;

procedure TVXPostEffect.MakeBlurEffect(var rci : TVXRenderContextInfo);
const
  lOffset: Integer = 2;
var
  I: Integer;
  lUp: Integer;
begin
  lUp := rci.viewPortSize.cx * lOffset;
  for I := lUp to High(FRenderBuffer) - lUp do
  begin
    FRenderBuffer[I].r := (FRenderBuffer[I].r + FRenderBuffer[I - lOffset].r +
        FRenderBuffer[I + lOffset].r + FRenderBuffer[I - lUp].r +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].g := (FRenderBuffer[I].g + FRenderBuffer[I - lOffset].g +
        FRenderBuffer[I + lOffset].g + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].b := (FRenderBuffer[I].b + FRenderBuffer[I - lOffset].b +
        FRenderBuffer[I + lOffset].b + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
  end;
end;

{ TVXPostShaderCollectionItem }

procedure TVXPostShaderCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TVXPostShaderCollectionItem then
  begin
    SetShader(TVXPostShaderCollectionItem(Source).FShader);
  end
  else
    inherited; // Die!!!
end;

function TVXPostShaderCollectionItem.GetDisplayName: string;
begin
  if FShader = nil then
    Result := ''
  else
  begin
    if FShader.Name <> '' then
      Result := FShader.Name
    else
      Result := FShader.ClassName;
  end;
end;

type
  // Required for Delphi5 compatibility.
  THackCollection = class(TOwnedCollection)end;

function TVXPostShaderCollectionItem.GetRealOwner: TVXPostShaderHolder;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TVXPostShaderHolder(THackCollection(Collection).GetOwner);
end;

procedure TVXPostShaderCollectionItem.SetShader(const Value: TVXShader);
var
  RealOwner: TVXPostShaderHolder;
begin
  if FShader = Value then Exit;
  RealOwner := GetRealOwner;

  if FShader <> nil then
      FShader.RemoveFreeNotification(RealOwner);

  if not Supports(TObject(Value), IGLPostShader, FPostShaderInterface) then
    raise EGLPostShaderHolderException.Create('Shader must support interface IGLPostShader!');

  if RealOwner <> nil then
    if FPostShaderInterface.GetTextureTarget <> RealOwner.TempTextureTarget then
      raise EGLPostShaderHolderException.Create(strErrorEx + 'TextureTarget is not compatible!');
  // If RealOwner = nil, we ignore this case and hope it will turn out ok...

  FShader := Value;

  if FShader <> nil then
    if RealOwner <> nil then
      FShader.FreeNotification(RealOwner);
end;

{ TVXPostShaderHolder }

procedure TVXPostShaderHolder.Assign(Source: TPersistent);
begin
  if Source is TVXPostShaderHolder then
  begin
    FShaders.Assign(TVXPostShaderHolder(Source).FShaders);
    FTempTextureTarget := TVXPostShaderHolder(Source).FTempTextureTarget;
  end;
  inherited;
end;

constructor TVXPostShaderHolder.Create(Owner: TComponent);
begin
  inherited;
  FTempTexture := TVXTextureHandle.Create;
  FTempTextureTarget :=ttTexture2D;
  FShaders := TVXPostShaderCollection.Create(Self, TVXPostShaderCollectionItem);
end;

destructor TVXPostShaderHolder.Destroy;
begin
  FShaders.Destroy;
  FTempTexture.Destroy;
  inherited;
end;

procedure TVXPostShaderHolder.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I: Integer;
begin
  if not (rci.ignoreMaterials) and not (csDesigning in ComponentState) and
         (rci.drawState <> dsPicking) then
  begin
    if (FPreviousViewportSize.cx <> rci.viewPortSize.cx) or
       (FPreviousViewportSize.cy <> rci.viewPortSize.cy) then
    begin
      InitTexture(FTempTexture.Handle, rci.viewPortSize,
        FTempTextureTarget);
      FPreviousViewportSize := rci.viewPortSize;
    end;

    if FShaders.Count <> 0 then
    begin
      for I := 0 to FShaders.Count - 1 do
      begin
        Assert(Assigned(FShaders[I].FShader));
        if FShaders[I].FShader.Enabled then
        begin
          rci.VXStates.ActiveTextureEnabled[FTempTextureTarget] := True;
          FShaders[I].FShader.Apply(rci, Self);
          repeat
            CopyScreenToTexture(rci.viewPortSize, DecodeTextureTarget(FTempTextureTarget));
            FShaders[I].FPostShaderInterface.DoUseTempTexture(FTempTexture, FTempTextureTarget);
            DrawTexturedScreenQuad5(rci.viewPortSize);
          until not FShaders[I].FShader.UnApply(rci);
          rci.VXStates.ActiveTextureEnabled[FTempTextureTarget] := False;
        end;
      end;
    end;
  end;
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TVXPostShaderHolder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TVXShader then
      FShaders.Remove(TVXShader(AComponent));
  end;
end;

procedure TVXPostShaderHolder.SetShaders(
  const Value: TVXPostShaderCollection);
begin
  FShaders.Assign(Value);
end;

{ TVXPostShaderCollection }

function TVXPostShaderCollection.Add: TVXPostShaderCollectionItem;
begin
  Result := TVXPostShaderCollectionItem(inherited Add);
end;

function TVXPostShaderCollection.GetItems(
  const Index: Integer): TVXPostShaderCollectionItem;
begin
  Result := TVXPostShaderCollectionItem(GetItem(Index));
end;

procedure TVXPostShaderCollection.Remove(
  const Item: TVXShader);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := Count - 1 downto 0 do
      if GetItems(I).FShader = Item then
        Delete(I);
  // Don't exit because the same shader might be applied more than once.
end;

procedure TVXPostShaderCollection.SetItems(const Index: Integer;
  const Value: TVXPostShaderCollectionItem);
begin
  GetItems(Index).Assign(Value);
end;

initialization
  RegisterClasses([TVXPostEffect, TVXPostShaderHolder,
                   TVXPostShaderCollection, TVXPostShaderCollectionItem]);

end.
