//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPostEffects<p>

  Provides a simple way to producing post-effects without shaders.<p>
  They are slow as hell, but it's worth it in some cases.<p>

	<b>History : </b><font size=-1><ul>
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

  TGLOnCustomPostEffectEvent = procedure(Sender: TObject; var Buffer: TGLPostEffectBuffer) of object;

  {: Note, that even if you use pepNone, object still calls glReadPixels and
    glDrawPixels and wastes your FPS. So if you waant to disable this effect
    completely, set Visible to False.
  }
  TGLPostEffectPreset = (pepNone, pepGray, pepNegative, pepWeird, pepWeird2, pepCustom);

  {: Provides a simple way to producing post-effects without shaders.<p>
     It is slow as hell, but it's worth it in some cases.}
  TGLPostEffect = class(TGLBaseSCeneObject)
  private
    FOnCustomEffect: TGLOnCustomPostEffectEvent;
    FPreset: TGLPostEffectPreset;
    FRenderBuffer: TGLPostEffectBuffer;
  protected
    //: May be should be private...
    procedure MakeGrayEffect(var Buffer: TGLPostEffectBuffer); virtual;
    procedure MakeNegativeEffect(var Buffer: TGLPostEffectBuffer); virtual;
    procedure MakeWeirdEffect(var Buffer: TGLPostEffectBuffer); virtual;
    procedure MakeWeirdEffect2(var Buffer: TGLPostEffectBuffer); virtual;
    procedure DoOnCustomEffect(var Buffer: TGLPostEffectBuffer); virtual;
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
  var Buffer: TGLPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, Buffer);
end;

procedure TGLPostEffect.DoRender(var rci : TRenderContextInfo;
                                      renderSelf, renderChildren : Boolean);
var
  NewScreenSize: Integer;
begin
  if not rci.ignoreMaterials then
  begin
    NewScreenSize := rci.viewPortSize.cx * rci.viewPortSize.cy;
    if NewScreenSize <> Length(FRenderBuffer) then
      SetLength(FRenderBuffer, NewScreenSize);

    glReadPixels(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
     case FPreset of
       pepNone:     ;
       pepGray:     MakeGrayEffect(FRenderBuffer);
       pepNegative: MakeNegativeEffect(FRenderBuffer);
       pepWeird:    MakeWeirdEffect(FRenderBuffer);
       pepWeird2:   MakeWeirdEffect2(FRenderBuffer);
       pepCustom:   DoOnCustomEffect(FRenderBuffer);
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
procedure TGLPostEffect.MakeGrayEffect(
  var Buffer: TGLPostEffectBuffer);
var
  I:    Longword;
  gray: TGLubyte;
begin
  for I := 0 to High(Buffer) do
  begin
    gray := Round((0.30 * TGLPostEffectColor(Buffer[I]).r) +
      (0.59 * TGLPostEffectColor(Buffer[I]).g) +
      (0.11 * TGLPostEffectColor(Buffer[I]).b));
    TGLPostEffectColor(Buffer[I]).r := gray;
    TGLPostEffectColor(Buffer[I]).g := gray;
    TGLPostEffectColor(Buffer[I]).b := gray;
  end;
end;

procedure TGLPostEffect.MakeNegativeEffect(
  var Buffer: TGLPostEffectBuffer);
var
  I:   Longword;
  red: TGLubyte;
begin
  for I := 0 to High(Buffer) do
  begin
    red := TGLPostEffectColor(Buffer[I]).r;
    TGLPostEffectColor(Buffer[I]).r := TGLPostEffectColor(Buffer[I]).b;
    TGLPostEffectColor(Buffer[I]).b := red;
  end;
end;

procedure TGLPostEffect.MakeWeirdEffect(
  var Buffer: TGLPostEffectBuffer);
var
  I: Longword;
begin
  for I := 0 to High(Buffer) do
  begin
    TGLPostEffectColor(Buffer[I]).r := round(TGLPostEffectColor(Buffer[I + 5]).r * 2);
    TGLPostEffectColor(Buffer[I]).g := round(TGLPostEffectColor(Buffer[I]).g * 1.5);
    TGLPostEffectColor(Buffer[I]).b := round(TGLPostEffectColor(Buffer[I + 5]).b * 1.5);
  end;
end;

procedure TGLPostEffect.MakeWeirdEffect2(
  var Buffer: TGLPostEffectBuffer);
var
  I:      Longword;
  r, rnd: Single;
begin
  for I := 0 to High(Buffer) do
  begin
    rnd := random + 1;
    r := TGLPostEffectColor(Buffer[I]).r * 1.5 * rnd;
    if r > 255 then
      r := 255;
    TGLPostEffectColor(Buffer[I]).r := round(r);

    TGLPostEffectColor(Buffer[I]).g := round(TGLPostEffectColor(Buffer[I]).g * rnd);

    TGLPostEffectColor(Buffer[I]).b := round(TGLPostEffectColor(Buffer[I]).b * rnd);
  end;
end;
{$IFDEF NEED_TO_RESTORE_RANGE_CHECK}
  {$R+}
  {$UNDEF NEED_TO_RESTORE_RANGE_CHECK}
{$ENDIF}

initialization
  RegisterClass(TGLPostEffect);

end.
