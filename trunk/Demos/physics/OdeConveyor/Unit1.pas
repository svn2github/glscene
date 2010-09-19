{ : ODE Conveyor belt demo.<p>

  This demo demontrates how to use the Motion1 parameter of ODE.

  Surface settings applied when a collision occured. In this situation, the ODE
  Manager uses surface settings of both objects:
  eg: muFinal = (muObject1 + muObject2)*0.5

  To choose the direction of the motion, we are changing the FDir-1 parameter of
  the collision contact.

  For a full explanation take a look at:
  http://opende.sourceforge.net/wiki/index.php/Manual_(All)#Contact

  Approximate coefficients of friction (from http://en.wikipedia.org/wiki/Friction)
  ------------------------------------
  Materials 	    Static friction (µs)
  ---------------------------------------------------
  Dry & clean 	Lubricated
  Aluminum 	      Steel 	  0.61
  Copper 	        Steel 	  0.53
  Brass 	        Steel 	  0.51
  Cast iron 	    Copper 	  1.05
  Cast iron 	    Zinc 	    0.85
  Concrete (wet) 	Rubber 	  0.30
  Concrete (dry) 	Rubber 	  1.0
  Concrete 	      Wood 	    0.62[7]
  Copper 	        Glass 	  0.68
  Glass 	        Glass 	  0.94
  Metal 	        Wood 	    0.2-0.6[7] 	  0.2 (wet)[7]
  Polythene 	    Steel 	  0.2[8] 	      0.2[8]
  Steel 	        Steel 	  0.80[8] 	    0.16[8]
  Steel 	        Teflon  	0.04[8] 	    0.04[8]
  Teflon 	        Teflon 	  0.04[8] 	    0.04[8]
  Wood 	          Wood 	    0.25-0.5[7] 	0.2 (wet)[7]

  <b>History : </b><font size=-1><ul>
  <li>19/09/10 - YP - Created by Yann Papouin
  </ul>
}

unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLNGDManager,
  GLODEManager,
  odeimport,
  GLCadencer,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLCrossPlatform,
  BaseClasses,
  GLWin32Viewer,
  VectorGeometry,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  GLSimpleNavigation;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    ConveyorBelt1: TGLCube;
    GLCadencer1: TGLCadencer;
    GLODEManager1: TGLODEManager;
    GLRenderPoint1: TGLRenderPoint;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Panel1: TPanel;
    Label1: TLabel;
    TrackBarMotionSpeed: TTrackBar;
    Label2: TLabel;
    Friction: TEdit;
    FrictionFeedback: TLabel;
    FDirX: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    FDirY: TEdit;
    Label6: TLabel;
    FDirZ: TEdit;
    NormZ: TLabel;
    NormY: TLabel;
    NormX: TLabel;
    AddODECube: TButton;
    SpawnPoint: TGLDummyCube;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormShow(Sender: TObject);
    procedure GLODEManager1Collision(Sender, Object1, Object2: TObject;
      var Contact: TdContact; var HandleCollision: Boolean);
    procedure TrackBarMotionSpeedChange(Sender: TObject);
    procedure FrictionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FDirChange(Sender: TObject);
    procedure AddODECubeClick(Sender: TObject);

  private
    { Déclarations privées }
    FUserDirection: TVector;
    FDirectionVector: TVector;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLKeyboard;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize default values from the one of DesignTime;
  with GetOrCreateOdeStatic(ConveyorBelt1) do
  begin
    TrackBarMotionSpeed.Position := Round(Surface.Motion1);
    Friction.Text := FloatToStr(Surface.Mu);
  end;

  FDirX.Text := '0';
  FDirY.Text := '0';
  FDirZ.Text := '1';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  GLCadencer1.Enabled := true;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLODEManager1.Step(deltaTime);
end;

procedure TForm1.GLODEManager1Collision(Sender, Object1, Object2: TObject;
  var Contact: TdContact; var HandleCollision: Boolean);
begin
  if Object2 = GetOrCreateOdeStatic(ConveyorBelt1) then
  begin
    Contact.fdir1[0] := FDirectionVector[0]; // x
    Contact.fdir1[1] := FDirectionVector[1]; // y
    Contact.fdir1[2] := FDirectionVector[2]; // z
    Contact.fdir1[3] := FDirectionVector[3]; // not used
  end;
end;

procedure TForm1.TrackBarMotionSpeedChange(Sender: TObject);
begin
  with GetOrCreateOdeStatic(ConveyorBelt1) do
  begin
    Surface.Motion1 := TrackBarMotionSpeed.Position;
  end;
end;

procedure TForm1.FrictionChange(Sender: TObject);
begin
  with GetOrCreateOdeStatic(ConveyorBelt1) do
  begin
    Surface.Mu := StrToFloatDef(Friction.Text, Surface.Mu);
    FrictionFeedback.Caption := Format('µs = %.2f', [Surface.Mu]);
  end;
end;

procedure TForm1.AddODECubeClick(Sender: TObject);
var
  ACube: TGLCube;
  AODEDynamic: TGLODEDynamic;
  AODEElementBox: TODEElementBox;
begin

  // Create a new GLScene cube and add it to the current GLScene1
  ACube := TGLCube.Create(GLScene1.Objects);
  with ACube do
  begin
    Parent := GLScene1.Objects;
    Position.Assign(SpawnPoint.Position);
    Material.FrontProperties.Diffuse.RandomColor;
  end;

  // Add ODE Dynamic behaviour on it
  AODEDynamic := GetOrCreateOdeDynamic(ACube);
  AODEDynamic.Manager := GLODEManager1;

  // Set µs value to 1 (default=1000), just uses the one from the conveyor
  with AODEDynamic.Surface do
  begin
    Mu := 1;
  end;

  // Finally create physical data in this behaviour
  AODEElementBox := TODEElementBox(AODEDynamic.AddNewElement(TODEElementBox));
  if Assigned(AODEElementBox) then
    with AODEElementBox do
    begin
      BoxWidth := ACube.CubeWidth;
      BoxHeight := ACube.CubeHeight;
      BoxDepth := ACube.CubeDepth;
    end;

  // The new camera target is the last added cube
  GLCamera1.TargetObject := ACube;

  // The spawn position is increased
  SpawnPoint.Position.Y := SpawnPoint.Position.Y + 1;
end;

procedure TForm1.FDirChange(Sender: TObject);
begin
  // Get back user data from GUI
  FUserDirection[0] := StrToFloatDef(FDirX.Text, FUserDirection[0]); // x
  FUserDirection[1] := StrToFloatDef(FDirY.Text, FUserDirection[0]); // y
  FUserDirection[2] := StrToFloatDef(FDirZ.Text, FUserDirection[0]); // z
  FUserDirection[3] := 0; // not used

  // Copy user data and normalized it
  FDirectionVector := FUserDirection;
  NormalizeVector(FDirectionVector);

  // Now returned normalized data to user to understand the 1-unit thing
  NormX.Caption := Format('Norm(X) = %.3f', [FDirectionVector[0]]);
  NormY.Caption := Format('Norm(Y) = %.3f', [FDirectionVector[1]]);
  NormZ.Caption := Format('Norm(Z) = %.3f', [FDirectionVector[2]]);
end;

end.
