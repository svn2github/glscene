//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialPreview<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialPreview;

interface

uses
  // vcl
{$IFDEF MSWINDOWS}
  Windows, GLWin32Context,
{$ENDIF}
{$IFDEF UNIX}
  GLWidgetContext,
{$ENDIF}
  GLCrossPlatform, SysUtils, Classes, Forms, Controls,
{$IFDEF FPC}
  LResources, LMessages,
{$ELSE}
  Messages, VectorTypes,
{$ENDIF}
  // GLScene
  OpenGLTokens, GLContext, GLState, GLColor, GLRenderContextInfo,
  GLVBOManager, GL3xMaterial, GL3xObjects, VectorGeometry, VectorGeometryEXT;

type
  TMaterialPreviewForm = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
{$IFDEF MSWINDOWS}
    FDC: HDC;
{$ENDIF}
{$IFDEF UNIX}
    FDC: Cardinal;
{$ENDIF}
    FRenderingContext: TGLContext;
    FRenderContextInfo: TRenderContextInfo;
    Model: TGL3xBaseSceneObject;
    Sphere: TGL3xSphere;
    Cube: TGL3xCube;
    Plane: TGL3xPlane;

    mx, my: Integer;
    CameraPosition: TVector;
    ProjectionMatrix: TMatrixEXT;
  protected
    { Protected declarations }
{$IFDEF FPC}
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure LMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
{$ELSE}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
{$ENDIF}
  public
    { Public declarations }
    NotifyClose: TNotifyEvent;
    Docked: Boolean;
    LastChangePosByMainForm: Boolean;
    procedure DoPaint;
    procedure PrepareContext;

    property RenderingContext: TGLContext read FRenderingContext;
    procedure SetModel(I: Integer);
  end;

implementation

{$I GLScene.inc}
{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  OpenGL1x,
  GLSpecializedUniforms,
  GL3xMaterialEditor;

procedure TMaterialPreviewForm.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  FDC := GetDC(Handle);
{$ENDIF}
{$IFDEF LINUX}
  FDC := Handle;
{$ENDIF}

  FRenderingContext := GLContextManager.CreateContext;
  if not Assigned(FRenderingContext) then
    exit;

  with FRenderingContext do
  begin
    Acceleration := chaHardware;
    Options := [rcoDoubleBuffered];
    ColorBits := 24;
    DepthBits := 24;
    StencilBits := 0;
    AlphaBits := 0;
    AccumBits := 0;
    AuxBuffers := 0;
    AntiAliasing := aa2x;
    GLStates.ForwardContext := True;
  end;
  try
    FRenderingContext.CreateContext(FDC);
  except
    FreeAndNil(FRenderingContext);
  end;
  if not Assigned(FRenderingContext) then
    exit;
  Sphere := TGL3xSphere.Create(Self);
  Sphere.BuiltProperties.Usage := buStream;
  Cube := TGL3xCube.Create(Self);
  Cube.BuiltProperties.Usage := buStream;
  Plane := TGL3xPlane.Create(Self);
  Plane.BuiltProperties.Usage := buStream;
  Model := Sphere;
end;

procedure TMaterialPreviewForm.PrepareContext;
begin
  if Assigned(FRenderingContext) then
  begin
    ProjectionMatrix.Perspective(45, 1, 0.1, 100);
    SetVector(CameraPosition, 2, 0.2, 0.2, 1);

    FRenderingContext.Activate;
    with FRenderingContext.GLStates do
    begin
      ColorClearValue := clrGray10;
      Enable(stDepthTest);
      Disable(stCullFace);
      FrontFace := fwCounterClockWise;
      ViewPort := Vector4iMake(0, 0, ClientWidth, ClientHeight);
      FRenderingContext.PipelineTransformation.ProjectionMatrix := ProjectionMatrix;
      LightEnabling[0] := True;
      LightPosition[0] := VectorMake(4, 6, 8, 1);
    end;
    FillChar(FRenderContextInfo, SizeOf(FRenderContextInfo), 0);
    FRenderContextInfo.ignoreMaterials := True;
    FRenderContextInfo.GLStates := FRenderingContext.GLStates;
    FRenderingContext.Deactivate;
  end;
end;

procedure TMaterialPreviewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FRenderingContext) then
  begin
    FreeAndNil(FRenderingContext);
    FreeAndNil(Sphere);
    FreeAndNil(Cube);
    FreeAndNil(Plane);
  end;
end;

procedure TMaterialPreviewForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  NotifyClose(Self);
end;

{$IFDEF FPC}

procedure TMaterialPreviewForm.LMPaint(var Message: TLMPaint);
begin
end;
{$ELSE}

procedure TMaterialPreviewForm.WMPaint(var Msg: TWMPaint);
begin
  DoPaint;
  inherited;
end;
{$ENDIF}

procedure TMaterialPreviewForm.DoPaint;
var
{$IFDEF MSWINDOWS}
  ps: TPaintStruct;
{$ENDIF}
  LM: TMatrixEXT;
begin
  if Assigned(FRenderingContext) and Visible then
  begin
    {$IFDEF MSWINDOWS}
    BeginPaint(Handle, ps);
    {$ENDIF}
    with FRenderingContext do
    begin
      Activate;
      try
        GL.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

        PipelineTransformation.CameraPosition := CameraPosition;
        PipelineTransformation.ModelMatrix := IdentityHmgMatrix;
        LM.LookAt(
          CameraPosition,
          Sphere.AbsolutePosition,
          YHmgVector);
        PipelineTransformation.ViewMatrix := LM;
        PipelineTransformation.ProjectionMatrix := ProjectionMatrix;

        if Model.Visible then
          with MaterialEditorForm.MaterialGraph.Material do
          begin
            if IsReadytoWork then
              repeat
                Apply;
                Model.DoRender(FRenderContextInfo, True, False);
              until UnApply;
          end;

        FRenderingContext.SwapBuffers;
      finally
        Deactivate;
        {$IFDEF MSWINDOWS}
        EndPaint(Handle, ps);
        {$ENDIF}
      end;
    end;
  end;
end;

{$IFDEF FPC}

procedure TMaterialPreviewForm.LMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if Assigned(FRenderingContext) then
    Message.Result := 1
  else
    inherited;
end;

procedure TMaterialPreviewForm.WMMove(var Message: TLMMove);
{$ELSE}
procedure TMaterialPreviewForm.WMMove(var Message: TWMMove);
{$ENDIF}
begin
  if LastChangePosByMainForm then
    LastChangePosByMainForm := False
  else
    Docked := False;
  inherited;
end;

procedure TMaterialPreviewForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TMaterialPreviewForm.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  originalT2C, normalT2C, normalCameraRight: TVector;
  pitchNow, dist, pitchDelta, turnDelta: Single;
begin
  if Shift = [ssLeft] then
  begin
    pitchDelta := my - Y;
    turnDelta := mx - X;
    originalT2C := VectorSubtract(CameraPosition, Sphere.AbsolutePosition);
    SetVector(normalT2C, originalT2C);
    dist := VectorLength(normalT2C);
    NormalizeVector(normalT2C);
    normalCameraRight := VectorCrossProduct(YHmgVector, normalT2C);
    if VectorLength(normalCameraRight) < 0.001 then
      SetVector(normalCameraRight, XVector)
    else
      NormalizeVector(normalCameraRight);
    pitchNow := ArcCos(VectorDotProduct(YHmgVector, normalT2C));
    pitchNow := ClampValue(pitchNow + DegToRad(pitchDelta), 0 + 0.025, PI -
      0.025);
    SetVector(normalT2C, YHmgVector);
    RotateVector(normalT2C, normalCameraRight, -pitchNow);
    RotateVector(normalT2C, YHmgVector, -DegToRad(turnDelta));
    ScaleVector(normalT2C, dist);
    CameraPosition := VectorAdd(CameraPosition,
      VectorSubtract(normalT2C, originalT2C));
    mx := X;
    my := Y;
  end;
end;

procedure TMaterialPreviewForm.SetModel(I: Integer);
begin
  case I of
    1: Model := Cube;
    2: Model := Plane;
    else Model := Sphere;
  end;
end;

end.
