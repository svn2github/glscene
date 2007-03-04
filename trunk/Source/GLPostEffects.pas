//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPostEffects<p>

  A collection of components that generate post effects.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/03/07 - DaStr - Added TGLPostShaderHolder
      <li>02/03/07 - DaStr - TGLOnCustomPostEffectEvent now passes rci
                             pepNone preset does not call gl[Read/Draw]Pixels
      <li>23/02/07 - DaStr - Initial version of TGLPostEffect
                                                (based on OldCity demo by FedeX)

}
unit GLPostEffects;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  GLScene, GLTexture, OpenGL1x, GLStrings, GLCustomShader, GLContext;

type
  EGLPostShaderHolderException = class(Exception);
  TGLPostShaderHolder = class;

  TGLPostShaderCollectionItem = class(TCollectionItem)
  private
    FShader: TGLShader;
    FPostShaderInterface: IGLPostShader;
    procedure SetShader(const Value: TGLShader);
  protected
    function GetRealOwner: TGLPostShaderHolder;
  public
    procedure Assign(Source: TPersistent); override;  
  published
    property Shader: TGLShader read FShader write SetShader;
  end;

  TGLPostShaderCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TGLPostShaderCollectionItem;
    procedure SetItems(const Index: Integer;
      const Value: TGLPostShaderCollectionItem);
  public
    procedure Remove(const Item: TGLShader);
    function Add: TGLPostShaderCollectionItem;

    property Items[const Index: Integer]: TGLPostShaderCollectionItem read GetItems write SetItems; default;
  end;

  {: A class that allows several post-shaders to be applied to the scene
     without a need to read the contents of the buffer to a temp texture in
     every applied shader. }
  TGLPostShaderHolder = class(TGLBaseSCeneObject)
  private
    FShaders: TGLPostShaderCollection;
    FTempTexture: TGLTextureHandle;
    FPreviousViewportSize: TGLSize;
    FTempTextureTarget: Cardinal;
    procedure SetShaders(const Value: TGLPostShaderCollection);
    procedure SetTempTextureTarget(const Value: TGLTextureTarget);
    function GetTempTextureTarget: TGLTextureTarget;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci : TRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
  published
    property TempTextureTarget: TGLTextureTarget read GetTempTextureTarget write SetTempTextureTarget default ttTexture2d;
    property Shaders: TGLPostShaderCollection read FShaders write SetShaders;

    //: Publish some stuff from TGLBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;


  TGLPostEffectColor = record
    R, G, B, A: TGLubyte;
  end;

  TGLPostEffectBuffer = array of TGLPostEffectColor;

  TGLOnCustomPostEffectEvent = procedure(Sender: TObject; var rci : TRenderContextInfo; var Buffer: TGLPostEffectBuffer) of object;

  {: Some presets. None - does nothing, Custom - calls the OnCustomEffect event. }
  TGLPostEffectPreset = (pepNone, pepGray, pepNegative, pepWeird, pepRedNoise, pepCustom);

  {: Provides a simple way to producing post-effects without shaders.<p>
     It is slow as hell, but it's worth it in some cases.}
  TGLPostEffect = class(TGLBaseSCeneObject)
  private
    FOnCustomEffect: TGLOnCustomPostEffectEvent;
    FPreset: TGLPostEffectPreset;
    FRenderBuffer: TGLPostEffectBuffer;
  protected
    //: May be should be private...
    procedure MakeGrayEffect; virtual;
    procedure MakeNegativeEffect; virtual;
    procedure MakeWeirdEffect; virtual;
    procedure MakeWeirdEffect2; virtual;
    procedure DoOnCustomEffect(var rci : TRenderContextInfo; var Buffer: TGLPostEffectBuffer); virtual;
  public
    destructor Destroy; override;
    procedure DoRender(var rci : TRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Preset: TGLPostEffectPreset read FPreset write FPreset default pepNone;
    //: User creates this effect.
    property OnCustomEffect: TGLOnCustomPostEffectEvent read FOnCustomEffect write FOnCustomEffect;
    //: Publish some stuff from TGLBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;

implementation

{ TGLPostEffect }

procedure TGLPostEffect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLPostEffect then
  begin
    FPreset := TGLPostEffect(Source).FPreset;
  end;
end;

destructor TGLPostEffect.Destroy;
begin
  inherited;
end;

procedure TGLPostEffect.DoOnCustomEffect(
  var rci : TRenderContextInfo; var Buffer: TGLPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, rci, Buffer);
end;

procedure TGLPostEffect.DoRender(var rci : TRenderContextInfo;
                                      renderSelf, renderChildren : Boolean);
var
  NewScreenSize: Integer;
begin
  if (not rci.ignoreMaterials) and (FPreset <> pepNone) then
  begin
    NewScreenSize := rci.viewPortSize.cx * rci.viewPortSize.cy;
    if NewScreenSize <> Length(FRenderBuffer) then
      SetLength(FRenderBuffer, NewScreenSize);

    glReadPixels(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
     case FPreset of
       // pepNone is handled in the first line.
       pepGray:     MakeGrayEffect;
       pepNegative: MakeNegativeEffect;
       pepWeird:    MakeWeirdEffect;
       pepRedNoise:    MakeWeirdEffect2;
       pepCustom:   DoOnCustomEffect(rci, FRenderBuffer);
     else
       Assert(False, glsUnknownType);
     end;
    glDrawPixels(rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
  end;

   // start rendering children (if any)
   if renderChildren then
      Self.RenderChildren(0, Count - 1, rci);
end;

{$IFOPT R+}
  {$R-}
  {$DEFINE NEED_TO_RESTORE_RANGE_CHECK}
{$ENDIF}
procedure TGLPostEffect.MakeGrayEffect;
var
  I:    Longword;
  gray: TGLubyte;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    gray := Round((0.30 * TGLPostEffectColor(FRenderBuffer[I]).r) +
      (0.59 * TGLPostEffectColor(FRenderBuffer[I]).g) +
      (0.11 * TGLPostEffectColor(FRenderBuffer[I]).b));
    TGLPostEffectColor(FRenderBuffer[I]).r := gray;
    TGLPostEffectColor(FRenderBuffer[I]).g := gray;
    TGLPostEffectColor(FRenderBuffer[I]).b := gray;
  end;
end;

procedure TGLPostEffect.MakeNegativeEffect;
var
  I:   Longword;
  red: TGLubyte;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    red := TGLPostEffectColor(FRenderBuffer[I]).r;
    TGLPostEffectColor(FRenderBuffer[I]).r := TGLPostEffectColor(FRenderBuffer[I]).b;
    TGLPostEffectColor(FRenderBuffer[I]).b := red;
  end;
end;

procedure TGLPostEffect.MakeWeirdEffect;
var
  I: Longword;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    TGLPostEffectColor(FRenderBuffer[I]).r := round(TGLPostEffectColor(FRenderBuffer[I + 5]).r * 2);
    TGLPostEffectColor(FRenderBuffer[I]).g := round(TGLPostEffectColor(FRenderBuffer[I]).g * 1.5);
    TGLPostEffectColor(FRenderBuffer[I]).b := round(TGLPostEffectColor(FRenderBuffer[I + 5]).b * 1.5);
  end;
end;

procedure TGLPostEffect.MakeWeirdEffect2;
var
  I:      Longword;
  r, rnd: Single;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    rnd := random + 1;
    r := TGLPostEffectColor(FRenderBuffer[I]).r * 1.5 * rnd;
    if r > 255 then
      r := 255;
    TGLPostEffectColor(FRenderBuffer[I]).r := round(r);

    TGLPostEffectColor(FRenderBuffer[I]).g := round(TGLPostEffectColor(FRenderBuffer[I]).g * rnd);

    TGLPostEffectColor(FRenderBuffer[I]).b := round(TGLPostEffectColor(FRenderBuffer[I]).b * rnd);
  end;
end;
{$IFDEF NEED_TO_RESTORE_RANGE_CHECK}
  {$R+}
  {$UNDEF NEED_TO_RESTORE_RANGE_CHECK}
{$ENDIF}

{ TGLPostShaderCollectionItem }

procedure TGLPostShaderCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TGLPostShaderCollectionItem then
  begin
    SetShader(TGLPostShaderCollectionItem(Source).FShader);
  end
  else
    inherited; // Die!!!
end;

function TGLPostShaderCollectionItem.GetRealOwner: TGLPostShaderHolder;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TGLPostShaderHolder(Collection.Owner);
end;

procedure TGLPostShaderCollectionItem.SetShader(const Value: TGLShader);
var
  RealOwner: TGLPostShaderHolder;
begin
  if FShader = Value then Exit;
  RealOwner := GetRealOwner;

  if FShader <> nil then
      FShader.RemoveFreeNotification(RealOwner);

  if not Supports(Value, IGLPostShader, FPostShaderInterface) then
    raise EGLPostShaderHolderException.Create('Shader must support interface IGLPostShader!');

  if RealOwner <> nil then
    if FPostShaderInterface.GetTextureTarget <> RealOwner.GetTempTextureTarget then
      raise EGLPostShaderHolderException.Create(glsErrorEx + 'TextureTarget is not compatible!');
  // If RealOwner = nil, we ignore this case and hope it will turn out ok...

  FShader := Value;

  if FShader <> nil then
    if RealOwner <> nil then
      FShader.FreeNotification(RealOwner);
end;

{ TGLPostShaderHolder }

constructor TGLPostShaderHolder.Create(Owner: TComponent);
begin
  inherited;
  FTempTexture := TGLTextureHandle.Create;
  FTempTextureTarget := GL_TEXTURE_2D;
  FShaders := TGLPostShaderCollection.Create(Self, TGLPostShaderCollectionItem);
end;

destructor TGLPostShaderHolder.Destroy;
begin
  FShaders.Destroy;
  FTempTexture.Destroy;
  inherited;
end;

procedure TGLPostShaderHolder.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I: Integer;
  NeedToContinue: Boolean;
begin
  if not rci.ignoreMaterials and not (csDesigning in ComponentState) then
  begin

    if (FPreviousViewportSize.cx <> rci.viewPortSize.cx) or
       (FPreviousViewportSize.cy <> rci.viewPortSize.cy) then
    begin
      InitTexture(FTempTexture.Handle, rci.viewPortSize, FTempTextureTarget);
      FPreviousViewportSize := rci.viewPortSize;
    end;

    if FShaders.Count <> 0 then
    begin
      glEnable(FTempTextureTarget);
      CopyScreentoTexture(rci.viewPortSize, FTempTextureTarget);
      for I := 0 to FShaders.Count - 1 do
      begin
        Assert(Assigned(FShaders[I].FShader));
        FShaders[I].FShader.Apply(rci, Self);
        repeat
          FShaders[I].FPostShaderInterface.DoUseTempTexture(FTempTexture, FTempTextureTarget);
          DrawTexturedScreenQuad3;
          NeedToContinue := FShaders[I].FShader.UnApply(rci);
        until
          NeedToContinue = False;
      end;
      glDisable(FTempTextureTarget);
    end;
  end;
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

function TGLPostShaderHolder.GetTempTextureTarget: TGLTextureTarget;
begin
  Result := EncodeGLTextureTarget(FTempTextureTarget);
end;

procedure TGLPostShaderHolder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TGLShader then
      FShaders.Remove(TGLShader(AComponent));
  end;
end;

procedure TGLPostShaderHolder.SetShaders(
  const Value: TGLPostShaderCollection);
begin
  FShaders.Assign(Value);
end;

procedure TGLPostShaderHolder.SetTempTextureTarget(
  const Value: TGLTextureTarget);
begin
  FTempTextureTarget := DecodeGLTextureTarget(Value);
end;

{ TGLPostShaderCollection }

function TGLPostShaderCollection.Add: TGLPostShaderCollectionItem;
begin
  Result := TGLPostShaderCollectionItem(inherited Add);
end;

function TGLPostShaderCollection.GetItems(
  const Index: Integer): TGLPostShaderCollectionItem;
begin
  Result := TGLPostShaderCollectionItem(GetItem(Index));
end;

procedure TGLPostShaderCollection.Remove(
  const Item: TGLShader);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := Count - 1 downto 0 do
      if GetItems(I).FShader = Item then
        Delete(I);
  // Don't exit because the same shader might be applied more than once.
end;

procedure TGLPostShaderCollection.SetItems(const Index: Integer;
  const Value: TGLPostShaderCollectionItem);
begin
  GetItems(Index).Assign(Value);
end;

initialization
  RegisterClasses([TGLPostEffect, TGLPostShaderHolder,
                   TGLPostShaderCollection, TGLPostShaderCollectionItem]);

end.
