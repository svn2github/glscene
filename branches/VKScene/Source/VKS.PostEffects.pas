//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  A collection of components that generate post effects. 
 
}
unit VKS.PostEffects;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  Winapi.OpenGL, Winapi.OpenGLext,
  //VKS
  VKS.Scene, VKS.Texture, VKS.Graphics, VKS.Strings,
  VKS.CustomShader, VKS.Context, VKS.VectorGeometry, VKS.RenderContextInfo,
  VKS.Material, VKS.TextureFormat;

type
  EGLPostShaderHolderException = class(Exception);
  TVKPostShaderHolder = class;

  TVKPostShaderCollectionItem = class(TCollectionItem)
  private
    FShader: TVKShader;
    FPostShaderInterface: IGLPostShader;
    procedure SetShader(const Value: TVKShader);
  protected
    function GetRealOwner: TVKPostShaderHolder;
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TVKShader read FShader write SetShader;
  end;

  TVKPostShaderCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TVKPostShaderCollectionItem;
    procedure SetItems(const Index: Integer;
      const Value: TVKPostShaderCollectionItem);
  public
    procedure Remove(const Item: TVKShader);
    function Add: TVKPostShaderCollectionItem;

    property Items[const Index: Integer]: TVKPostShaderCollectionItem read GetItems write SetItems; default;
  end;

  { A class that allows several post-shaders to be applied to the scene,
    one after another. It does not provide any optimizations related to
    multi-shader rendering, just a convenient interface. }
  TVKPostShaderHolder = class(TVKBaseSCeneObject)
  private
    FShaders: TVKPostShaderCollection;
    FTempTexture: TVKTextureHandle;
    FPreviousViewportSize: TVKSize;
    FTempTextureTarget: TVKTextureTarget;
    procedure SetShaders(const Value: TVKPostShaderCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci : TVKRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
  published
    property TempTextureTarget: TVKTextureTarget read FTempTextureTarget write FTempTextureTarget default ttTexture2d;
    property Shaders: TVKPostShaderCollection read FShaders write SetShaders;

    //: Publish some stuff from TVKBaseSceneObject.
    property Visible;
    property OnProgress;
  end;


  TVKPostEffectColor = record
    R, G, B, A: GLubyte;
  end;

  TVKPostEffectBuffer = array of TVKPostEffectColor;

  TVKOnCustomPostEffectEvent = procedure(Sender: TObject; var rci : TVKRenderContextInfo; var Buffer: TVKPostEffectBuffer) of object;

  { Some presets for TVKPostEffect:
       pepNone - does nothing.
       pepGray - makes picture gray.
       pepNegative - inverts all colors.
       pepDistort - simulates shaky TV image.
       pepNoise - just adds random niose.
       pepNightVision - simulates nightvision goggles.
       pepBlur - blurs the scene.
       pepCustom - calls the OnCustomEffect event.
  }
  TVKPostEffectPreset = (pepNone, pepGray, pepNegative, pepDistort, pepNoise,
                         pepNightVision, pepBlur, pepCustom);

  { Provides a simple way to producing post-effects without shaders. 
     It is slow as hell, but it's worth it in some cases.}
  TVKPostEffect = class(TVKBaseSCeneObject)
  private
    FOnCustomEffect: TVKOnCustomPostEffectEvent;
    FPreset: TVKPostEffectPreset;
    FRenderBuffer: TVKPostEffectBuffer;
  protected
    //: May be should be private...
    procedure MakeGrayEffect; virtual;
    procedure MakeNegativeEffect; virtual;
    procedure MakeDistortEffect; virtual;
    procedure MakeNoiseEffect; virtual;
    procedure MakeNightVisionEffect; virtual;
    procedure MakeBlurEffect(var rci : TVKRenderContextInfo); virtual;
    procedure DoOnCustomEffect(var rci : TVKRenderContextInfo; var Buffer: TVKPostEffectBuffer); virtual;
  public
    procedure DoRender(var rci : TVKRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Preset: TVKPostEffectPreset read FPreset write FPreset default pepNone;
    //: User creates this effect.
    property OnCustomEffect: TVKOnCustomPostEffectEvent read FOnCustomEffect write FOnCustomEffect;
    //: Publish some stuff from TVKBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;

//-----------------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------------

{ TVKPostEffect }

procedure TVKPostEffect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKPostEffect then
  begin
    FPreset := TVKPostEffect(Source).FPreset;
  end;
end;

procedure TVKPostEffect.DoOnCustomEffect(
  var rci : TVKRenderContextInfo; var Buffer: TVKPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, rci, Buffer);
end;

procedure TVKPostEffect.DoRender(var rci : TVKRenderContextInfo;
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

procedure TVKPostEffect.MakeGrayEffect;
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

procedure TVKPostEffect.MakeNegativeEffect;
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

procedure TVKPostEffect.MakeDistortEffect;
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

procedure TVKPostEffect.MakeNoiseEffect;
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

procedure TVKPostEffect.MakeNightVisionEffect;
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

procedure TVKPostEffect.MakeBlurEffect(var rci : TVKRenderContextInfo);
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

{ TVKPostShaderCollectionItem }

procedure TVKPostShaderCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TVKPostShaderCollectionItem then
  begin
    SetShader(TVKPostShaderCollectionItem(Source).FShader);
  end
  else
    inherited; // Die!!!
end;

function TVKPostShaderCollectionItem.GetDisplayName: string;
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

function TVKPostShaderCollectionItem.GetRealOwner: TVKPostShaderHolder;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TVKPostShaderHolder(THackCollection(Collection).GetOwner);
end;

procedure TVKPostShaderCollectionItem.SetShader(const Value: TVKShader);
var
  RealOwner: TVKPostShaderHolder;
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

{ TVKPostShaderHolder }

procedure TVKPostShaderHolder.Assign(Source: TPersistent);
begin
  if Source is TVKPostShaderHolder then
  begin
    FShaders.Assign(TVKPostShaderHolder(Source).FShaders);
    FTempTextureTarget := TVKPostShaderHolder(Source).FTempTextureTarget;
  end;
  inherited;
end;

constructor TVKPostShaderHolder.Create(Owner: TComponent);
begin
  inherited;
  FTempTexture := TVKTextureHandle.Create;
  FTempTextureTarget :=ttTexture2D;
  FShaders := TVKPostShaderCollection.Create(Self, TVKPostShaderCollectionItem);
end;

destructor TVKPostShaderHolder.Destroy;
begin
  FShaders.Destroy;
  FTempTexture.Destroy;
  inherited;
end;

procedure TVKPostShaderHolder.DoRender(var rci: TVKRenderContextInfo;
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
          rci.VKStates.ActiveTextureEnabled[FTempTextureTarget] := True;
          FShaders[I].FShader.Apply(rci, Self);
          repeat
            CopyScreenToTexture(rci.viewPortSize, DecodeTextureTarget(FTempTextureTarget));
            FShaders[I].FPostShaderInterface.DoUseTempTexture(FTempTexture, FTempTextureTarget);
            DrawTexturedScreenQuad5(rci.viewPortSize);
          until not FShaders[I].FShader.UnApply(rci);
          rci.VKStates.ActiveTextureEnabled[FTempTextureTarget] := False;
        end;
      end;
    end;
  end;
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TVKPostShaderHolder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TVKShader then
      FShaders.Remove(TVKShader(AComponent));
  end;
end;

procedure TVKPostShaderHolder.SetShaders(
  const Value: TVKPostShaderCollection);
begin
  FShaders.Assign(Value);
end;

{ TVKPostShaderCollection }

function TVKPostShaderCollection.Add: TVKPostShaderCollectionItem;
begin
  Result := TVKPostShaderCollectionItem(inherited Add);
end;

function TVKPostShaderCollection.GetItems(
  const Index: Integer): TVKPostShaderCollectionItem;
begin
  Result := TVKPostShaderCollectionItem(GetItem(Index));
end;

procedure TVKPostShaderCollection.Remove(
  const Item: TVKShader);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := Count - 1 downto 0 do
      if GetItems(I).FShader = Item then
        Delete(I);
  // Don't exit because the same shader might be applied more than once.
end;

procedure TVKPostShaderCollection.SetItems(const Index: Integer;
  const Value: TVKPostShaderCollectionItem);
begin
  GetItems(Index).Assign(Value);
end;

initialization
  RegisterClasses([TVKPostEffect, TVKPostShaderHolder,
                   TVKPostShaderCollection, TVKPostShaderCollectionItem]);

end.
