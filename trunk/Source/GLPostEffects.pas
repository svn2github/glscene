//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPostEffects<p>

  Provides a simple way to producing post-effects without shaders.<p>
  They are slow as hell, but it's worth it in some cases.<p>

	<b>History : </b><font size=-1><ul>
      <li>02/03/07 - DaStr - TGLOnCustomPostEffectEvent now passes rci
                             pepNone preset does not call gl[Read/Draw]Pixels
      <li>23/02/07 - DaStr - Initial version (based on OldCity demo by FedeX)

}
unit GLPostEffects;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,

  // GLScene
  GLScene, GLTexture, OpenGL1x, GLStrings;

type
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

initialization
  RegisterClass(TGLPostEffect);

end.
