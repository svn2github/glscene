//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
 Applies a blur effect over the viewport.<p>
}

unit GLS.Blur;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils, System.UITypes, Winapi.OpenGL,
  FMX.Graphics,
  //GLS 
  GLS.Scene, GLS.VectorGeometry, GLS.Objects, GLS.BitmapFont, GLS.Texture, GLS.Material,
  GLS.HudObjects, GLS.Color, GLS.Graphics, GLS.Context, GLS.OpenGLTokens,
  GLS.XOpenGL, GLS.State, GLS.TextureFormat, GLS.BaseClasses, GLS.RenderContextInfo;

type

  TVKBlurPreset = (pNone, pGlossy, pBeastView, pOceanDepth, pDream, pOverBlur, pAdvancedBlur);
  TVKBlurkind = (bNone, bSimple, bAdvanced);
  TRGBPixel = record
    R, G, B: GLubyte;
  end;
  TRGBPixelBuffer = array of TRGBPixel;
  TVKAdvancedBlurImagePrepareEvent = procedure(Sender: TObject; BMP32: TVKBitmap32; var DoBlur: boolean) of object;

  EGLMotionBlurException = class(Exception);

  TVKBlur = class(TVKHUDSprite)
  private
    FViewer: TVKMemoryViewer;
    OldTime: Double;
    FDoingMemView: boolean;
    FBlurDeltaTime: Double;
    FBlurTop: Single;
    FBlurBottom: Single;
    FBlurLeft: Single;
    FBlurRight: Single;
    FRenderHeight: Integer;
    FRenderWidth: Integer;
    FPreset: TVKBlurPreset;
    FTargetObject: TVKbaseSceneObject;
    FOnAdvancedBlurImagePrepareEvent: TVKAdvancedBlurImagePrepareEvent;
    FBlur: TVKBlurKind;
    Pixelbuffer: TRGBPixelBuffer;
    FAdvancedBlurPasses: integer;
    FOnAfterTargetRender: TNotifyEvent;
    FOnBeforeTargetRender: TNotifyEvent;
    FAdvancedBlurAmp: single;
    FBlurSelf: boolean;
    FAdvancedBlurLowClamp: byte;
    FAdvancedBlurHiClamp: byte;
    FRenderBackgroundColor: TColor;
    procedure DoMemView(baseObject: TVKBaseSceneObject);
    procedure SetRenderHeight(const Value: Integer);
    procedure SetRenderWidth(const Value: Integer);
    procedure UpdateImageSettings;
    procedure SetPreset(const Value: TVKBlurPreset);

    function StoreBlurBottom: Boolean;
    function StoreBlurDeltaTime: Boolean;
    function StoreBlurRight: Boolean;
    function StoreBlurTop: Boolean;
    function StoreBlurLeft: Boolean;
    procedure SetTargetObject(const Value: TVKbaseSceneObject);
    procedure SetOnAdvancedBlurImagePrepareEvent(const Value: TVKAdvancedBlurImagePrepareEvent);
    procedure SetBlur(const Value: TVKBlurKind);
    procedure SetAdvancedBlurPasses(const Value: integer);
    procedure SetOnAfterTargetRender(const Value: TNotifyEvent);
    procedure SetOnBeforeTargetRender(const Value: TNotifyEvent);
    procedure SetAdvancedBlurAmp(const Value: single);
    procedure SetBlurSelf(const Value: boolean);
    procedure SetAdvancedBlurLowClamp(const Value: byte);
    procedure SetAdvancedBlurHiClamp(const Value: byte);
    procedure SetRenderBackgroundColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Blur: TVKBlurKind read FBlur write SetBlur;
    property BlurDeltaTime: Double read FBlurDeltaTime write FBlurDeltaTime stored StoreBlurDeltaTime;
    property BlurLeft: Single read FBlurLeft write FBlurLeft stored StoreBlurLeft;
    property BlurTop: Single read FBlurTop write FBlurTop stored StoreBlurTop;
    property BlurRight: Single read FBlurRight write FBlurRight stored StoreBlurRight;
    property BlurBottom: Single read FBlurBottom write FBlurBottom stored StoreBlurBottom;
    property RenderWidth: Integer read FRenderWidth write SetRenderWidth default 256;
    property RenderHeight: Integer read FRenderHeight write SetRenderHeight default 256;
    property Preset: TVKBlurPreset read FPreset write SetPreset stored false;
    property TargetObject: TVKbaseSceneObject read FTargetObject write SetTargetObject;
    property AdvancedBlurPasses: integer read FAdvancedBlurPasses write SetAdvancedBlurPasses;
    property AdvancedBlurAmp: single read FAdvancedBlurAmp write SetAdvancedBlurAmp;
    property AdvancedBlurLowClamp: byte read FAdvancedBlurLowClamp write SetAdvancedBlurLowClamp;
    property AdvancedBlurHiClamp: byte read FAdvancedBlurHiClamp write SetAdvancedBlurHiClamp;
    property BlurSelf: boolean read FBlurSelf write SetBlurSelf;
    property RenderBackgroundColor: TColor read FRenderBackgroundColor write SetRenderBackgroundColor;
    property OnAdvancedBlurImagePrepareEvent: TVKAdvancedBlurImagePrepareEvent read FOnAdvancedBlurImagePrepareEvent write SetOnAdvancedBlurImagePrepareEvent;
    property OnBeforeTargetRender: TNotifyEvent read FOnBeforeTargetRender write SetOnBeforeTargetRender;
    property OnAfterTargetRender: TNotifyEvent read FOnAfterTargetRender write SetOnAfterTargetRender;
  end;

  {
    This component blurs everything thatis rendered BEFORE it. So if you want part
    of your scene blured, the other not blured, make sure that the other part is
    rendered after this component. It is fast and does not require shaders.

    Note: it is FPS-dependant. Also also can produce a "blury trail effect", which
    stays on the screen until something new is rendered over it. It can be overcome
    by changing the Material.FrontProperties.Diffuse property. This, however, also
    has a drawback - the picture becomes more blured altogether. For example, if
    your backgroud color is Black, set the Material.FrontProperties.Diffuse to White.
    If it is White, set Material.FrontProperties.Diffuse to Black. I haven't tried
    any others, but I hope you get the idea ;)

    I've seen this effect in different Bruring components, even in shaders, but if
    anyone knows another way to fix this issue - please post it on the glscene
    newsgroup.
  }
  TVKMotionBlur = class(TVKCustomSceneObject, IGLInitializable)
  private
    FIntensity: Single;
    function StoreIntensity: Boolean;
  protected
    procedure DoOnAddedToParent; override;
    procedure InitializeObject(ASender: TObject; const ARci: TVKRenderContextInfo); virtual;
  public
    { This function is only valid AFTER OpenGL has been initialized. }
    function SupportsRequiredExtensions: Boolean;
    procedure DoRender(var ARci: TVKRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    constructor Create(aOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    // The more the intensity, the more blur you have.
    property Intensity: Single read FIntensity write FIntensity stored StoreIntensity;

    // From TVKBaseSceneObject.
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
    property Hint;
  end;

implementation

const
  EPS = 0.001;

constructor TVKBlur.Create(AOwner: TComponent);
begin
  inherited;
  FBlurDeltaTime := 0.02;
  FBlurTop := 0.01;
  FBlurLeft := 0.01;
  FBlurRight := 0.01;
  FBlurBottom := 0.01;
  FRenderHeight := 256;
  FRenderWidth := 256;
  FViewer := TVKMemoryViewer.Create(Self);
  FPreset := pNone;
  Material.Texture.Disabled := False;
  FAdvancedBlurPasses := 1;
  FAdvancedBlurAmp := 1.1;
  FBlurSelf := true;
  FAdvancedBlurLowClamp := 0;
  FAdvancedBlurHiClamp := 255;
  FRenderBackgroundColor := TColors.Black;
end;

destructor TVKBlur.Destroy;
begin
  FViewer.Free;
  inherited;
end;

procedure TVKBlur.UpdateImageSettings;
var
  B: TBitmap;
begin
  if Material.Texture.Image is TVKBlankImage then
    with TVKBlankImage(Material.Texture.Image) do
    begin
      Width := RenderWidth;
      Height := Renderheight;
    end
  else if Material.Texture.Image is TVKPersistentImage then
  begin
    B := TVKPersistentImage(Material.Texture.Image).Picture.Bitmap;
    B.Width := 0;
    B.Height := 0;
    B.Width := RenderWidth;
    B.Height := RenderHeight;
  end;

  with FViewer do
  begin
    Width := RenderWidth;
    Height := Renderheight;
  end;

  SetLength(Pixelbuffer, RenderWidth * RenderHeight);
end;

procedure TVKBlur.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  if self.Visible and (progressTime.newTime - OldTime > FBlurDeltaTime) then
  begin
    OldTime := progressTime.newTime;
    if TargetObject <> nil then
      DoMemView(TargetObject);
  end;

end;

procedure TVKBlur.DoMemView(baseObject: TVKBaseSceneObject);
var
  OldFocalLength: single;
  refsiz: single;
  BMP: TVKBitmap32;
  x, y: integer;
  line: PGLPixel32Array;
  by: Integer;
  bp: Integer;
  DoBlur: Boolean;

  procedure ApplyBlur(const passes: integer);
  var
    t: integer;
    x, y: integer;
    lin, linu, lind, linuu, lindd: PGLPixel32Array;
    r, g, b: single;
    ir, ig, ib: Smallint;

    procedure ApplyBlurClampAndSetPixel;
    begin
      // 0.1111 = 1/7 (where 7 is the times each pixel is summed with neighbours or self)
      ir := round(r * FAdvancedBlurAmp * 0.1111);
      ig := round(g * FAdvancedBlurAmp * 0.1111);
      ib := round(b * FAdvancedBlurAmp * 0.1111);

      // Hi Clamp
      if ir > FAdvancedBlurHiClamp then
        ir := FAdvancedBlurHiClamp;
      if ig > FAdvancedBlurHiClamp then
        ig := FAdvancedBlurHiClamp;
      if ib > FAdvancedBlurHiClamp then
        ib := FAdvancedBlurHiClamp;

      lin^[x].r := ir;
      lin^[x].g := ig;
      lin^[x].b := ib;
    end;

  begin

    for t := 0 to passes do
    begin
      for y := 2 to BMP.Height - 3 do
      begin
        linuu := BMP.ScanLine[y - 2];
        linu := BMP.ScanLine[y - 1];
        lin := BMP.ScanLine[y];
        lind := BMP.ScanLine[y + 1];
        lindd := BMP.ScanLine[y + 2];
        by := y * BMP.Height;
        // X = 0 PART:
        x := 0;
        r := lin^[x].r + lin^[x + 1].r + lin^[x + 2].r + linu^[x].r + lind^[x].r + linuu^[x].r + lindd^[x].r;
        g := lin^[x].g + lin^[x + 1].g + lin^[x + 2].g + linu^[x].g + lind^[x].g + linuu^[x].g + lindd^[x].g;
        b := lin^[x].b + lin^[x + 1].b + lin^[x + 2].b + linu^[x].b + lind^[x].b + linuu^[x].b + lindd^[x].b;
        ApplyBlurClampAndSetPixel;
        // X = 1 PART:
        x := 1;
        r := lin^[x].r + lin^[x + 1].r + lin^[x - 1].r + lin^[x + 2].r + linu^[x].r + lind^[x].r + linuu^[x].r + lindd^[x].r;
        g := lin^[x].g + lin^[x + 1].g + lin^[x - 1].g + lin^[x + 2].g + linu^[x].g + lind^[x].g + linuu^[x].g + lindd^[x].g;
        b := lin^[x].b + lin^[x + 1].b + lin^[x - 1].b + lin^[x + 2].b + linu^[x].b + lind^[x].b + linuu^[x].b + lindd^[x].b;
        ApplyBlurClampAndSetPixel;
        // ALL X IN MIDDLE PART:
        for x := 2 to BMP.Width - 3 do
        begin
          r := lin^[x].r + lin^[x + 1].r + lin^[x - 1].r + lin^[x + 2].r + lin^[x - 2].r + linu^[x].r + lind^[x].r + linuu^[x].r + lindd^[x].r;
          g := lin^[x].g + lin^[x + 1].g + lin^[x - 1].g + lin^[x + 2].g + lin^[x - 2].g + linu^[x].g + lind^[x].g + linuu^[x].g + lindd^[x].g;
          b := lin^[x].b + lin^[x + 1].b + lin^[x - 1].b + lin^[x + 2].b + lin^[x - 2].b + linu^[x].b + lind^[x].b + linuu^[x].b + lindd^[x].b;
          ApplyBlurClampAndSetPixel;
        end;
        //X = NEXT TO LAST PART:
        x := BMP.Width - 2;
        r := lin^[x].r + lin^[x + 1].r + lin^[x - 1].r + lin^[x - 2].r + linu^[x].r + lind^[x].r + linuu^[x].r + lindd^[x].r;
        g := lin^[x].g + lin^[x + 1].g + lin^[x - 1].g + lin^[x - 2].g + linu^[x].g + lind^[x].g + linuu^[x].g + lindd^[x].g;
        b := lin^[x].b + lin^[x + 1].b + lin^[x - 1].b + lin^[x - 2].b + linu^[x].b + lind^[x].b + linuu^[x].b + lindd^[x].b;
        ApplyBlurClampAndSetPixel;
        //X = LAST PART:
        x := BMP.Width - 1;
        r := lin^[x].r + lin^[x - 1].r + lin^[x - 2].r + linu^[x].r + lind^[x].r + linuu^[x].r + lindd^[x].r;
        g := lin^[x].g + lin^[x - 1].g + lin^[x - 2].g + linu^[x].g + lind^[x].g + linuu^[x].g + lindd^[x].g;
        b := lin^[x].b + lin^[x - 1].b + lin^[x - 2].b + linu^[x].b + lind^[x].b + linuu^[x].b + lindd^[x].b;
        ApplyBlurClampAndSetPixel;
      end;
    end;
  end;

begin
  if FViewer.Camera <> Scene.CurrentGLCamera then
    FViewer.Camera := Scene.CurrentGLCamera;

  if FViewer.Camera <> nil then
  begin
    FDoingMemView := true;

    //Scene.RenderScene(FViewer.Buffer,FViewer.Width,FViewer.Height,dsRendering,baseObject);
    FViewer.Camera.BeginUpdate;

    OldFocalLength := FViewer.Camera.FocalLength;

    // CALCULATE SCALED FOCAL LENGTH FOR VIEWER
    if SCene.CurrentBuffer.Width > SCene.CurrentBuffer.height then
      refsiz := Scene.CurrentBuffer.Width
    else
      refsiz := Scene.CurrentBuffer.height;

    FViewer.Camera.FocalLength := FViewer.Camera.FocalLength * FViewer.Buffer.Width / refsiz;

    if FViewer.Buffer.BackgroundColor <> FRenderBackgroundColor then
      FViewer.Buffer.BackgroundColor := FRenderBackgroundColor;

    try
      case FBlur of
        bNone:
          begin
            // do nothing
          end;
        bSimple:
          begin
            if Assigned(FOnBeforeTargetRender) then
              FOnBeforeTargetRender(self);
            // RENDER
            FViewer.Render(baseObject);
            // Copy to texture (unfortunatelly, after this, the bitmap cannot be red back from the hardware.. i think)
            FViewer.CopyToTexture(Material.Texture);
            if Assigned(FOnAfterTargetRender) then
              FOnAfterTargetRender(self);
          end;
        bAdvanced:
          begin
            if Assigned(FOnBeforeTargetRender) then
              FOnBeforeTargetRender(self);

            // RENDER
            FViewer.Render(baseObject);
            // Read pixels from buffer. This is slow, but ok with reasonably small render size.
            FViewer.Buffer.RenderingContext.Activate;
            try
              glReadPixels(0, 0, FViewer.Buffer.Width, FViewer.Buffer.Height, GL_RGB, GL_UNSIGNED_BYTE, Pixelbuffer);
            except
              FViewer.Buffer.RenderingContext.Deactivate;
            end;
            if Assigned(FOnAfterTargetRender) then
              FOnAfterTargetRender(self);

            BMP := Material.Texture.Image.GetBitmap32;
            BMP.Narrow;
            FViewer.Buffer.RenderingContext.Deactivate;
            // FILLS THE BITMAP with the pixelbuffer captured from the internal memoryViewer
            for y := 0 to RenderHeight - 1 do
            begin
              line := BMP.ScanLine[y];
              by := y * RenderHeight;
              for x := 0 to RenderWidth - 1 do
              begin
                bp := x + by;
                line^[x].r := Pixelbuffer[bp].R;
                line^[x].g := Pixelbuffer[bp].G;
                line^[x].b := Pixelbuffer[bp].B;

                // Low clamp
                if line^[x].r < FAdvancedBlurLowClamp then
                  line^[x].r := 0;
                if line^[x].g < FAdvancedBlurLowClamp then
                  line^[x].g := 0;
                if line^[x].b < FAdvancedBlurLowClamp then
                  line^[x].b := 0;
              end;
            end;

            DoBlur := true;
            if Assigned(FOnAdvancedBlurImagePrepareEvent) then
            begin
              FOnAdvancedBlurImagePrepareEvent(self, BMP, DoBlur);
            end;

            if DoBlur then
              ApplyBlur(FAdvancedBlurPasses);

            Material.Texture.Image.NotifyChange(self);

          end;
      end;
    finally
      FViewer.Camera.FocalLength := OldFocalLength;
      FViewer.Camera.EndUpdate;
      FDoingMemView := false;
    end;
  end;
end;

{$WARNINGS Off} //Suppress "unsafe" warning

procedure TVKBlur.DoRender(var ARci: TVKRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  vx, vy, vx1, vy1, f: Single;
  offsx, offsy: single;
  MaxMeasure: integer;
begin
  if FDoingMemView and (FBlurSelf = false) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    if Count > 0 then
      Self.RenderChildren(0, Count - 1, ARci);
    exit;
  end;
  if ARci.ignoreMaterials then
    Exit;
  GL.CheckError;
  Material.Apply(ARci);
  GL.CheckError;
  repeat
    if AlphaChannel <> 1 then
      ARci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
    // Prepare matrices
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrixf(@TVKSceneBuffer(ARci.buffer).BaseProjectionMatrix);
    if ARci.renderDPI = 96 then
      f := 1
    else
      f := ARci.renderDPI / 96;
    GL.Scalef(2 / ARci.viewPortSize.cx, 2 / ARci.viewPortSize.cy, 1);

    // center of viewport:
    GL.Translatef(0, 0, Position.Z);

    if Rotation <> 0 then
      GL.Rotatef(Rotation, 0, 0, 1);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    ARci.GLStates.Disable(stDepthTest);
    ARci.GLStates.DepthWriteMask := False;

    // calculate offsets in order to keep the quad a square centered in the view
    if ARci.viewPortSize.cx > ARci.viewPortSize.cy then
    begin
      offsx := 0;
      offsy := (ARci.viewPortSize.cx - ARci.viewPortSize.cy) * 0.5;
      MaxMeasure := ARci.viewPortSize.cx;
    end
    else
    begin
      offsx := (ARci.viewPortSize.cy - ARci.viewPortSize.cx) * 0.5;
      offsy := 0;
      MaxMeasure := ARci.viewPortSize.cy;
    end;

    // precalc coordinates
    vx := -ARci.viewPortSize.cx * 0.5 * f;
    vx1 := vx + ARci.viewPortSize.cx * f;
    vy := +ARci.viewPortSize.cy * 0.5 * f;
    vy1 := vy - ARci.viewPortSize.cy * f;

    vx := vx - offsx;
    vx1 := vx1 + offsx;
    vy := vy + offsy;
    vy1 := vy1 - offsy;

    // Cause the radial scaling
    if FDoingMemView then
    begin
      vx := vx - FBlurLeft * MaxMeasure;
      vx1 := vx1 + FBlurRight * MaxMeasure;
      vy := vy + FBlurTop * MaxMeasure;
      vy1 := vy1 - FBlurBottom * MaxMeasure;
    end;

    // issue quad
    glBegin(GL_QUADS);
    GL.Normal3fv(@YVector);
    glTexCoord2f(0, 0);
    glVertex2f(vx, vy1);
    glTexCoord2f(XTiles, 0);
    glVertex2f(vx1, vy1);
    glTexCoord2f(XTiles, YTiles);
    glVertex2f(vx1, vy);
    glTexCoord2f(0, YTiles);
    glVertex2f(vx, vy);
    glEnd;
    // restore state
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  until not Material.UnApply(ARci);
  if Count > 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TVKBlur.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FTargetObject then
      FTargetObject := nil;
  inherited;
end;

{$WARNINGS On}

procedure TVKBlur.SetRenderBackgroundColor(const Value: TColor);
begin
  FRenderBackgroundColor := Value;
end;

procedure TVKBlur.SetRenderHeight(const Value: integer);
begin
  FRenderHeight := Value;
  UpdateImageSettings;
end;

procedure TVKBlur.SetRenderWidth(const Value: integer);
begin
  FRenderWidth := Value;
  UpdateImageSettings;
end;

procedure TVKBlur.SetTargetObject(const Value: TVKbaseSceneObject);
begin
  FTargetObject := Value;
end;

procedure TVKBlur.SetAdvancedBlurAmp(const Value: single);
begin
  FAdvancedBlurAmp := Value;
end;

procedure TVKBlur.SetAdvancedBlurHiClamp(const Value: byte);
begin
  FAdvancedBlurHiClamp := Value;
end;

procedure TVKBlur.SetAdvancedBlurLowClamp(const Value: byte);
begin
  FAdvancedBlurLowClamp := Value;
end;

procedure TVKBlur.SetAdvancedBlurPasses(const Value: integer);
begin
  FAdvancedBlurPasses := Value;
end;

procedure TVKBlur.SetBlur(const Value: TVKBlurKind);
begin
  if FBlur <> Value then
  begin
    case Value of
      bnone:
        begin
          // do Nothing
        end;
      bSimple:
        begin
          Material.Texture.ImageClassName := TVKBlankImage.ClassName;
        end;
      bAdvanced:
        begin
          Material.Texture.ImageClassName := TVKPersistentImage.ClassName;
        end;
    end;
    UpdateImageSettings;
  end;
  FBlur := Value;
end;

procedure TVKBlur.SetBlurSelf(const Value: boolean);
begin
  FBlurSelf := Value;
end;

procedure TVKBlur.SetOnAdvancedBlurImagePrepareEvent(const Value: TVKAdvancedBlurImagePrepareEvent);
begin
  FOnAdvancedBlurImagePrepareEvent := Value;
end;

procedure TVKBlur.SetOnAfterTargetRender(const Value: TNotifyEvent);
begin
  FOnAfterTargetRender := Value;
end;

procedure TVKBlur.SetOnBeforeTargetRender(const Value: TNotifyEvent);
begin
  FOnBeforeTargetRender := Value;
end;

procedure TVKBlur.SetPreset(const Value: TVKBlurPreset);
begin
  FPreset := Value;

  case FPreset of
    pNone:
      begin
        // do nothing
      end;
    pAdvancedBlur:
      begin
        Blur := bAdvanced;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 1);
        BlurTop := 0;
        BlurLeft := 0;
        BlurRight := 0;
        BlurBottom := 0;
        BlurDeltaTime := 0;
        BlurSelf := false;
        AdvancedBlurPasses := 1;
        AdvancedBlurAmp := 1.2;
        RenderWidth := 64;
        RenderHeight := 64;
      end;
    pGlossy:
      begin
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.7);
        BlurTop := 0.02;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.02;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pBeastView:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 0, 0, 0.8);
        BlurTop := 0.001;
        BlurLeft := 0.03;
        BlurRight := 0.03;
        BlurBottom := 0.001;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pOceanDepth:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmTransparency;
        Material.FrontProperties.Diffuse.SetColor(0.2, 0.2, 1, 0.99);
        BlurTop := 0.04;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.04;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pDream:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmTransparency;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.992);
        BlurTop := 0.02;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.02;
        BlurDeltaTime := 0.1;
        BlurSelf := true;
      end;
    pOverBlur:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(0.95, 0.95, 0.95, 0.98);
        BlurTop := 0.01;
        BlurLeft := 0.01;
        BlurRight := 0.01;
        BlurBottom := 0.01;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
  end;

end;

function TVKBlur.StoreBlurBottom: Boolean;
begin
  Result := Abs(FBlurBottom - 0.01) > EPS;
end;

function TVKBlur.StoreBlurDeltaTime: Boolean;
begin
  Result := Abs(FBlurDeltaTime - 0.02) > EPS;
end;

function TVKBlur.StoreBlurLeft: Boolean;
begin
  Result := Abs(FBlurLeft - 0.01) > EPS;
end;

function TVKBlur.StoreBlurRight: Boolean;
begin
  Result := Abs(FBlurRight - 0.01) > EPS;
end;

function TVKBlur.StoreBlurTop: Boolean;
begin
  Result := Abs(FBlurTop - 0.01) > EPS;
end;

{ TVKMotionBlur }

procedure TVKMotionBlur.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKMotionBlur then
  begin
    FIntensity := TVKMotionBlur(Source).FIntensity;
  end;
end;

constructor TVKMotionBlur.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Material.FrontProperties.Diffuse.Initialize(clrBlack);
  Material.MaterialOptions := [moNoLighting, moIgnoreFog];
  Material.Texture.Disabled := False;
  Material.BlendingMode := bmTransparency;
  FIntensity := 0.975;
end;

procedure TVKMotionBlur.DoOnAddedToParent;
begin
  inherited;
  // Request to be initialized on next render.
  if Scene <> nil then
    Scene.InitializableObjects.Add(Self);
end;

procedure TVKMotionBlur.DoRender(var ARci: TVKRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if not (ARci.ignoreMaterials or (csDesigning in ComponentState) or
    (ARci.drawState = dsPicking)) then
  with ARci.GLStates do
  begin
    ARci.ignoreDepthRequests := True;
    Material.Apply(ARci);
    ActiveTextureEnabled[ttTextureRect] := True;
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    GL.Ortho(0, ARci.viewPortSize.cx, ARci.viewPortSize.cy, 0, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadIdentity;
    Disable(stDepthTest);
    DepthWriteMask := False;

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, ARci.viewPortSize.cy);
    glVertex2f(0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, ARci.viewPortSize.cy);
    glTexCoord2f(ARci.viewPortSize.cx, 0.0);
    glVertex2f(ARci.viewPortSize.cx, ARci.viewPortSize.cy);
    glTexCoord2f(ARci.viewPortSize.cx, ARci.viewPortSize.cy);
    glVertex2f(ARci.viewPortSize.cx, 0);
    glEnd;

    glPopMatrix;
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    ActiveTextureEnabled[ttTextureRect] := False;
    Material.UnApply(ARci);
    ARci.ignoreDepthRequests := False;

    glCopyTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB, 0, 0, ARci.viewPortSize.cx, ARci.viewPortSize.cy, 0);

    Material.FrontProperties.Diffuse.Alpha := FIntensity;
  end;

  if Count <> 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TVKMotionBlur.InitializeObject(ASender: TObject;
  const ARci: TVKRenderContextInfo);
begin
  // If extension is not supported, silently disable this component.
  if not (csDesigning in ComponentState) then
    if not SupportsRequiredExtensions then
      Visible := False;
end;

function TVKMotionBlur.StoreIntensity: Boolean;
begin
  Result := Abs(FIntensity - 0.975) > EPS;
end;

function TVKMotionBlur.SupportsRequiredExtensions: Boolean;
begin
  Result :=
    GL.ARB_texture_rectangle or GL_EXT_texture_rectangle or GL.NV_texture_rectangle;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

     // class registrations
  RegisterClass(TVKBlur);
  RegisterClass(TVKMotionBlur);

end.

