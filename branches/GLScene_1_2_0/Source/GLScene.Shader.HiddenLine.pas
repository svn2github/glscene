{ : GLHiddenLineShader<p>

  A shader that renders hidden (back-faced) lines differently from visible
  (front) lines. Polygon offset is used to displace fragments depths a little
  so that there is no z-fighting in rendering the same geometry multiple times.<p>

  <b>History : </b><font size=-1><ul>
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>05/03/10 - DanB - More state added to TGLStateCache
  <li>06/06/07 - DaStr - Added $I GLScene.inc
  Added GLColor to uses (BugtrackerID = 1732211)
  <li>25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
  <li>25/09/04 - NelC - Fixed bug of disabled blend (thx Carlos)
  <li>05/02/04 - NelC - Fixed memory leak in TGLHiddenLineShader.Destroy (thx Achim Hammes)
  <li>13/12/03 - NelC - Added SurfaceLit, ShadeModel
  <li>05/12/03 - NelC - Added ForceMaterial
  <li>03/12/03 - NelC - Creation. Modified from the HiddenLineShader in
  the multipass demo.
  </ul></font>
}
unit GLScene.Shader.HiddenLine;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene.Material,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Platform,
  GLScene.Core,
  GLScene.Base.Color,
  GLScene.Base.Classes,
  GLScene.Base.Context.Info,
  GLScene.Base.GLStateMachine;

type
  TGLLineSettings = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FColor: TGLColor;
    FWidth: Single;
    FPattern: TGLushort;

    FForceMaterial: Boolean;

    procedure SetPattern(const value: TGLushort);
    procedure SetColor(const v: TGLColor);
    procedure SetWidth(const value: Single);
    procedure SetForceMaterial(v: Boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TRenderContextInfo);
    procedure UnApply(var rci: TRenderContextInfo);

  published
    { Published Declarations }
    property Width: Single read FWidth write SetWidth;
    property Color: TGLColor read FColor write SetColor;
    property Pattern: TGLushort read FPattern write SetPattern default $FFFF;
    { : Set ForceMaterial to true to enforce the application of the line settings
      for objects that sets their own color, line width and pattern. }
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TGLHiddenLineShader = class(TGLShader)
  private
    FPassCount: integer;

    FLineSmooth: Boolean;
    FSolid: Boolean;

    FBackGroundColor: TGLColor;

    FFrontLine: TGLLineSettings;
    FBackLine: TGLLineSettings;

    FLighting: Boolean;
    FShadeModel: TGLShadeModel;

    procedure SetlineSmooth(v: Boolean);
    procedure SetSolid(v: Boolean);
    procedure SetBackgroundColor(AColor: TGLColor);
    procedure SetLighting(v: Boolean);
    procedure SetShadeModel(const val: TGLShadeModel);

  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property FrontLine: TGLLineSettings read FFrontLine write FFrontLine;
    property BackLine: TGLLineSettings read FBackLine write FBackLine;
    { : Line smoothing control }
    property LineSmooth: Boolean read FLineSmooth write SetlineSmooth
      default false;
    { : Solid controls if you can see through the front-line wireframe. }
    property Solid: Boolean read FSolid write SetSolid default false;
    { : Color used for solid fill. }
    property BackgroundColor: TGLColor read FBackGroundColor
      write SetBackgroundColor;
    { : When Solid is True, determines if lighting or background color is used. }
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    { : Shade model.<p>
      Default is "Smooth".<p> }
    property ShadeModel: TGLShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
uses
  GLScene.Base.Context;
// ------------------
// ------------------ TGLLineSettings ------------------
// ------------------

// Create
//

constructor TGLLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TGLColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;

// Destroy
//

destructor TGLLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

// SetPattern
//

procedure TGLLineSettings.SetPattern(const value: TGLushort);
begin
  if FPattern <> value then
  begin
    FPattern := value;
    NotifyChange(Self);
  end;
end;

// SetColor
//

procedure TGLLineSettings.SetColor(const v: TGLColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

// SetWidth
//

procedure TGLLineSettings.SetWidth(const value: Single);
begin
  FWidth := value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: Boolean;

  // Apply
  //

procedure TGLLineSettings.Apply(var rci: TRenderContextInfo);
begin
  rci.GLStates.LineWidth := Width;
  GL.Color4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.GLStates.Enable(stLineStipple);
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := Pattern;
  end
  else
    rci.GLStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;

// UnApply
//

procedure TGLLineSettings.UnApply(var rci: TRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;

// SetForceMaterial
//

procedure TGLLineSettings.SetForceMaterial(v: Boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLHiddenLineShader ------------------
// ------------------

// Create
//

constructor TGLHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TGLLineSettings.Create(Self);
  FBackLine := TGLLineSettings.Create(Self);
  FSolid := false;

  FBackGroundColor := TGLColor.Create(Self);
  FBackGroundColor.Initialize(clrBtnFace);

  FLineSmooth := false;
  FLighting := true;
  FShadeModel := smDefault;
end;

// Destroy
//

destructor TGLHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackGroundColor.Free;
  inherited;
end;

// DoApply
//

procedure TGLHiddenLineShader.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  FPassCount := 1;

  if Solid then
    with rci.GLStates do
    begin
      // draw filled front faces in first pass
      PolygonMode := pmFill;
      CullFaceMode := cmBack;

      if FLighting then
      begin
        case ShadeModel of
          smDefault, smSmooth:
            GL.ShadeModel(GL_SMOOTH);
          smFlat:
            GL.ShadeModel(GL_FLAT);
        end
      end
      else
      begin
        Disable(stLighting);
        GL.Color4fv(FBackGroundColor.AsAddress); // use background color
      end;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetFill);
    end
  else
    with rci.GLStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.GLStates.SetPolygonOffset(1, 2);
end;

// DoUnApply
//

function TGLHiddenLineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.GLStates do
    begin
      LineStippleFactor := 1;
      LineStipplePattern := $FFFF;
      if LineSmooth then
      begin
        LineSmoothHint := hintNicest;
        Enable(stLineSmooth);
      end
      else
        Disable(stLineSmooth);

      if LineSmooth or (FBackLine.FColor.Alpha < 1) or
        (FFrontLine.FColor.Alpha < 1) then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end
      else
        Disable(stBlend);
    end;
  end;

begin
  case FPassCount of
    1:
      with rci.GLStates do
      begin
        // draw front line in 2nd pass
        FPassCount := 2;

        FBackLine.UnApply(rci);
        FFrontLine.Apply(rci);

        SetLineSmoothBlend;

        if Solid and FLighting then
          Disable(stLighting);

        PolygonMode := pmLines;
        CullFaceMode := cmBack;

        if Solid then
          rci.GLStates.Disable(stPolygonOffsetFill)
        else
          rci.GLStates.Disable(stPolygonOffsetLine);

        Result := true;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.GLStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(false);
    Result := false;
  end;
end;

// SetBackgroundColor
//

procedure TGLHiddenLineShader.SetBackgroundColor(AColor: TGLColor);
begin
  FBackGroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;

// SetlineSmooth
//

procedure TGLHiddenLineShader.SetlineSmooth(v: Boolean);
begin
  if FLineSmooth <> v then
  begin
    FLineSmooth := v;
    NotifyChange(Self);
  end;
end;

// SetLighting
//

procedure TGLHiddenLineShader.SetLighting(v: Boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(Self);
  end;
end;

// SetSolid
//

procedure TGLHiddenLineShader.SetSolid(v: Boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(Self);
  end;
end;

// SetShadeModel
//

procedure TGLHiddenLineShader.SetShadeModel(const val: TGLShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

end.
