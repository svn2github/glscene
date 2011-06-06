{: GLCameraController demo.<p>

   This demo shows how the TGLCameraController can be used to control the
   camera's movement around a target using minimal code.

}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLWin32Viewer, GLScene, GLObjects,
  GLVectorFileObjects, GLCameraController, GLFile3ds, GLGeomObjects, GLTexture,
  GLCadencer, StdCtrls, ComCtrls, GLMaterial, GLCoordinates, GLCrossPlatform,
  BaseClasses, VectorGeometry, GLNavigator, GLSmoothNavigator, GLGraph;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eDestX: TEdit;
    eDestY: TEdit;
    eDestZ: TEdit;
    Label5: TLabel;
    GLScene1: TGLScene;
    GLDummyCube1: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLCylinder1: TGLCylinder;
    GLSphere2: TGLSphere;
    GLCamera: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLSphere3: TGLSphere;
    Panel3: TPanel;
    Label6: TLabel;
    Label9: TLabel;
    eDistance: TEdit;
    Panel4: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    eSafeDistance: TEdit;
    eTimeToSafePlacement: TEdit;
    eTimeToOrbit: TEdit;
    Label12: TLabel;
    eTimeToZoomBackIn: TEdit;
    Panel5: TPanel;
    Label13: TLabel;
    eTime: TEdit;
    Label14: TLabel;
    btnMoveToPos: TButton;
    btnZoomToDistance: TButton;
    btnOrbitToPos: TButton;
    btnSafeOrbitAndZoomToPos: TButton;
    pImg: TPanel;
    Image1: TImage;
    Label4: TLabel;
    Panel6: TPanel;
    Label15: TLabel;
    btnOrbitToPosAdv: TButton;
    Timer1: TTimer;
    GLCameraController1: TGLCameraController;
    Panel8: TPanel;
    Label20: TLabel;
    Panel7: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    camDirX: TEdit;
    camDirY: TEdit;
    camDirZ: TEdit;
    Panel9: TPanel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    camUpX: TEdit;
    camUpY: TEdit;
    camUpZ: TEdit;
    btSmoothOrbit: TButton;
    GLSmoothNavigator: TGLSmoothNavigator;
    dcDebugGUI: TGLDummyCube;
    ArrowLine: TGLArrowLine;
    XYZGrid: TGLXYZGrid;
    GLPlane1: TGLPlane;
    UpAxis: TCheckBox;
    btSmoothOrbitToPosAdv: TButton;
    btSmoothOrbitAndZoom: TButton;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnMoveToPosClick(Sender: TObject);
    procedure btnZoomToDistanceClick(Sender: TObject);
    procedure btnOrbitToPosClick(Sender: TObject);
    procedure btnSafeOrbitAndZoomToPosClick(Sender: TObject);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure btnOrbitToPosAdvClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btSmoothOrbitToPosAdvClick(Sender: TObject);
    procedure btSmoothOrbitClick(Sender: TObject);
    procedure btSmoothOrbitAndZoomClick(Sender: TObject);
  private
    { Private declarations }
    FGLCameraController : TGLCameraController;
    DextX, DextY, DextZ, Time, ZoomDistance: double;
    mx, my : Integer;
    FCameraSmoothAnimator: TGLNavigatorSmoothChangeVector;
    procedure GetInput(Sender:TButton);
    function OnGetCameraPosition(const ASender: TGLNavigatorSmoothChangeVector): TVector;
    procedure OnSetCameraPosition(const ASender: TGLNavigatorSmoothChangeVector; const AValue: TVector);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.dfm}

procedure TForm1.GetInput(Sender:TButton);
begin
  FCameraSmoothAnimator.Enabled := False;

  if (Sender = btnMoveToPos) or
     (Sender = btnOrbitToPos) or
     (Sender = btnOrbitToPosAdv) or
     (Sender = btnSafeOrbitAndZoomToPos) or
     (Sender = btSmoothOrbit) or
     (Sender = btSmoothOrbitAndZoom) or
     (Sender = btSmoothOrbitToPosAdv) then
  begin
    DextX:=strtofloat(eDestX.text);
    DextY:=strtofloat(eDestY.text);
    DextZ:=strtofloat(eDestZ.text);
  end;
  if (Sender = btnMoveToPos) or
     (Sender.Name='btnZoomToDistance') or
     (Sender.Name='btnOrbitToPosAdv') or
     (Sender.Name='btnOrbitToPos') then
  begin
    Time:= strtofloat(eTime.text);
  end;
  if (Sender.Name='btnZoomToDistance')then
  begin
    ZoomDistance:= strtofloat(eDistance.text)
  end;
  if (Sender.Name='btnSafeOrbitAndZoomToPos')then
  begin
    FGLCameraController.soSafeDistance := strtofloat(eSafeDistance.text);
    FGLCameraController.soTimeToSafePlacement := strtofloat(eTimeToSafePlacement.text);
    FGLCameraController.soTimeToOrbit := strtofloat(eTimeToOrbit.text);
    FGLCameraController.soTimeToZoomBackIn := strtofloat(eTimeToZoomBackIn.text);
  end;
end;

//MoveToPos Usage
procedure TForm1.btnMoveToPosClick(Sender: TObject);
begin
  GetInput(TButton(Sender));
  FGLCameraController.MoveToPos(DextX, DextY, DextZ, Time);
end;

//ZoomToDistance Usage
procedure TForm1.btnZoomToDistanceClick(Sender: TObject);
begin
  GetInput(TButton(Sender));
  FGLCameraController.ZoomToDistance(ZoomDistance,Time);
end;

//OrbitToPos Usage
procedure TForm1.btnOrbitToPosClick(Sender: TObject);
begin
  GetInput(TButton(Sender));
  FGLCameraController.OrbitToPos(DextX, DextY, DextZ, Time);
end;

procedure TForm1.btnOrbitToPosAdvClick(Sender: TObject);
begin
  GetInput(TButton(Sender));
  FGLCameraController.OrbitToPosAdvanced(DextX, DextY, DextZ, Time, UpAxis.Checked);
end;


//SafeOrbitAndZoomToPos Usage
procedure TForm1.btnSafeOrbitAndZoomToPosClick(Sender: TObject);
begin
  GetInput(TButton(Sender));
  FGLCameraController.SafeOrbitAndZoomToPos(DextX, DextY, DextZ);
end;

//GUI Implementation - Pay attention to FGLCameraController.AllowUserAction!

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FCameraSmoothAnimator.Enabled := False;
	GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
//  GLCamera.DepthOfView:=2*GLCamera.DistanceToTarget+2*GLcamera.TargetObject.BoundingSphereRadius;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // For btSmoothOrbitAndZoomClick Order of these commands is important.
  GLSmoothNavigator.AdjustDistanceToTarget(0, deltaTime);

  FGLCameraController.Step(deltaTime, newtime);

  // This component requires FixedDeltaTime higher than FMaxExpectedDeltatime.
  GLSmoothNavigator.AnimateCustomItems(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCameraSmoothAnimator.Enabled := False;
  FGLCameraController.StopMovement;
   if Shift=[ssLeft] then
   begin
     mx:=x; my:=y;
   end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

  if Shift=[ssLeft] then
  begin
    GLCamera.MoveAroundTarget(my-y, mx-x);
    mx:=x; my:=y;
    caption:= 'CameraController Demo - camera position = ' +
      formatfloat('0.##',glcamera.position.x)+'/'+
      formatfloat('0.##',glcamera.position.y)+'/'+
      formatfloat('0.##',glcamera.position.z);
  end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  caption:= 'CameraController Demo';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  camDirX.Text := Format('%.4f',[GLCamera.Direction.X]);
  camDirY.Text := Format('%.4f',[GLCamera.Direction.Y]);
  camDirZ.Text := Format('%.4f',[GLCamera.Direction.Z]);

  camUpX.Text := Format('%.4f',[GLCamera.Up.X]);
  camUpY.Text := Format('%.4f',[GLCamera.Up.Y]);
  camUpZ.Text := Format('%.4f',[GLCamera.Up.Z]);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGLCameraController := GLCameraController1;

  FCameraSmoothAnimator := TGLNavigatorSmoothChangeVector.Create(GLSmoothNavigator.CustomAnimatedItems);
  FCameraSmoothAnimator.Enabled := False;
  FCameraSmoothAnimator.Inertia := 0.6;
  FCameraSmoothAnimator.Speed := 1;
  FCameraSmoothAnimator.SpeedLimit := 5000;
  FCameraSmoothAnimator.Cutoff := 0.0001;
  FCameraSmoothAnimator.OnGetCurrentValue := OnGetCameraPosition;
  FCameraSmoothAnimator.OnSetCurrentValue := OnSetCameraPosition;

  GLSmoothNavigator.MovingObject := GLCamera;
  GLSmoothNavigator.MoveAroundParams.TargetObject := GLCamera.TargetObject;
end;


procedure TForm1.btSmoothOrbitClick(Sender: TObject);
var
  lAngle: Single; // In radians.
  lTime: Single;
  lNeedToRecalculateZoom: Boolean;
begin
  GetInput(TButton(Sender));
  lAngle := ArcCos(VectorAngleCosine(
    VectorNormalize(VectorSubtract(GLSphere1.AbsoluteAffinePosition, GLCamera.AbsoluteAffinePosition)),
    VectorNormalize(VectorSubtract(GLSphere1.AbsoluteAffinePosition, Vector3fMake(DextX, DextY, DextZ))
    )));

  // The final look and feel of smooth animation is affected by
  //  FCameraSmoothAnimator's propperties and this value.
  lTime := lAngle * 2;
  FCameraSmoothAnimator.TargetValue.DirectVector := GLCamera.AbsolutePosition;
  FCameraSmoothAnimator.Enabled := True;

  if Sender = btSmoothOrbit then
    lNeedToRecalculateZoom := False
  else if Sender = btSmoothOrbitAndZoom then
    lNeedToRecalculateZoom := True
  else
  begin
    lNeedToRecalculateZoom := False;
    Assert(False);
  end;

  FGLCameraController.OrbitToPosSmooth(DextX, DextY, DextZ, lTime, FCameraSmoothAnimator, lNeedToRecalculateZoom);
end;

procedure TForm1.btSmoothOrbitAndZoomClick(Sender: TObject);
begin
  btSmoothOrbitClick(btSmoothOrbitAndZoom);
  GLSmoothNavigator.AdjustDistanceParams.AddImpulse( Sign(Random - 0.5) * 10);
end;

procedure TForm1.btSmoothOrbitToPosAdvClick(Sender: TObject);
var
  lAngle: Single; // In radians.
  lTime: Single;
begin
  GetInput(TButton(Sender));
  lAngle := ArcCos(VectorAngleCosine(
    VectorNormalize(VectorSubtract(GLSphere1.AbsoluteAffinePosition, GLCamera.AbsoluteAffinePosition)),
    VectorNormalize(VectorSubtract(GLSphere1.AbsoluteAffinePosition, Vector3fMake(DextX, DextY, DextZ))
    )));

  lTime := lAngle; // Speed can be controled by applying a multiplier here.

  FCameraSmoothAnimator.TargetValue.DirectVector := GLCamera.AbsolutePosition;
  FCameraSmoothAnimator.Enabled := True;
  FGLCameraController.OrbitToPosAdvancedSmooth(DextX, DextY, DextZ, lTime, FCameraSmoothAnimator);
end;

function TForm1.OnGetCameraPosition(const ASender: TGLNavigatorSmoothChangeVector): TVector;
begin
  Result := GLCamera.AbsolutePosition;
end;

procedure TForm1.OnSetCameraPosition(const ASender: TGLNavigatorSmoothChangeVector; const AValue: TVector);
begin
  GLCamera.AbsolutePosition := AValue;
end;


initialization
  DecimalSeparator := '.';

end.
