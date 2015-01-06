//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRMaterialPreview<p>

   Material Preview frame.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>05/01/14 - PW - Converted to FMX
      <li>12/07/07 - DaStr - Improved Cross-Platform compatibility
                             (Bugtracker ID = 1684432)
      <li>06/06/07 - DaStr - Added GLS.Color to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>16/12/06 - DaStr - Editor enhanced
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}

unit FRMaterialPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math.Vectors,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Viewport3D, FMX.ListBox, FMX.Types3D, FMX.Controls3D,
  FMX.Objects3D, FMX.MaterialSources,

  GLS.Scene, GLS.SceneViewer, GLS.Material, GLS.Teapot;

type
  TRMaterialPreview = class(TFrame)
    CBObject: TComboBox;
    Camera: TCameraComponent;
    Cube: TCube;
    Sphere: TSphere;
    LightSource: TLight;
    CBBackground: TComboBox;
    GLSceneViewer: TViewport3D;
    LightMaterialSource1: TLightMaterialSource;
    Cone: TCone;
    World: TDummy;
    Light: TDummy;
    FireSphere: TSphere;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    FLibMaterial: TGLAbstractLibMaterial;
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
    function GetLibMaterial: TGLAbstractLibMaterial;
    procedure SetLibMaterial(const Value: TGLAbstractLibMaterial);
    { Private declarations }
  public
    { Public declarations }
    GLMaterialLibrary: TGLMaterialLibrary;
    constructor Create(AOwner : TComponent); override;
    property Material : TGLMaterial read GetMaterial
      write SetMaterial;
    property LibMaterial : TGLAbstractLibMaterial read GetLibMaterial
      write SetLibMaterial;
  end;

implementation

{$R *.fmx}

var
  MX, MY: Integer;

{ TRMaterialPreview }

constructor TRMaterialPreview.Create(AOwner: TComponent);
begin
  inherited;
   BackGroundSprite.Position.X := GLSceneViewer.Width div 2;
   BackGroundSprite.Position.Y := GLSceneViewer.Height div 2;
   BackGroundSprite.Width := GLSceneViewer.Width;
   BackGroundSprite.Height := GLSceneViewer.Height;

   CBObject.ItemIndex:=0;       CBObjectChange(Self);
   CBBackground.ItemIndex:=0;   CBBackgroundChange(Self);
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Cube.Visible   := I = 0;
   Sphere.Visible := I = 1;
   Cone.Visible   := I = 2;
   Teapot.Visible := I = 3;
end;

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
   bgColor : TColor;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:=clWhite;
      2 : bgColor:=clBlack;
      3 : bgColor:=clBlue;
      4 : bgColor:=clRed;
      5 : bgColor:=clGreen;
   else
      bgColor:=clNone;
   end;
   with BackGroundSprite.Material do begin
      Texture.Disabled:=(bgColor<>clNone);
      FrontProperties.Diffuse.Color:=ConvertWinColor(bgColor);
   end;
end;

procedure TRMaterialPreview.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);
  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.GLSceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

function TRMaterialPreview.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TRMaterialPreview.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value.GetActualPrimaryMaterial);
end;

function TRMaterialPreview.GetLibMaterial: TGLAbstractLibMaterial;
begin
  Result := FLibMaterial;
end;

procedure TRMaterialPreview.SetLibMaterial(const Value: TGLAbstractLibMaterial);
begin
  FLibMaterial := Value;
  if Assigned(FLibMaterial) then
  begin
    with GLMaterialLibrary.Materials[0] do
    begin
      Material.MaterialLibrary := FLibMaterial.MaterialLibrary;
      Material.LibMaterialName := FLibMaterial.Name
    end;
  end
  else
  with GLMaterialLibrary.Materials[0] do
  begin
    Material.MaterialLibrary := nil;
    Material.LibMaterialName := '';
  end;
end;

end.
