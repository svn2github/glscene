{: FMaterialEditorForm<p>

   Editor window for a material (with preview).<p>

   <b>Historique : </b><font size=-1><ul>

   </ul></font>
}
unit FMaterialEditorFormLCL;

interface
{$MODE DELPHI}
{$i GLScene.inc}

uses

  {$IFDEF MSWINDOWS} Windows,{$ENDIF} FRTrackBarEditLCL, Forms,
  FRColorEditorLCL, ComCtrls, FRFaceEditorLCL, StdCtrls, Controls,
  Graphics,
  Classes, Buttons, TypInfo, FRTextureEditLCL, 
  GLScene, GLObjects, GLTexture, GLHUDObjects, GLTeapot,
  GLGeomObjects, GLColor, GLLCLViewer, GLCoordinates,
  GLCrossPlatform, BaseClasses, GLMaterial
  ,lresources;

type
  TMaterialEditorForm = class(TForm)
    PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    GroupBox1: TGroupBox;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TGLCube;
    Sphere: TGLSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    BackGroundSprite: TGLHUDSprite;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    FireSphere: TGLSphere;
    GLMaterialLibrary: TGLMaterialLibrary;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnMaterialChanged(Sender : TObject);
  private
    { Diclarations privies }
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
  public
    { Diclarations publiques }
    constructor Create(AOwner : TComponent); override;
    function Execute(var amaterial : TGLMaterial) : Boolean;
    property Material : TGLMaterial read GetMaterial write SetMaterial;
  end;

function MaterialEditorForm : TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

implementation

var
	vMaterialEditorForm : TMaterialEditorForm;
        FEFront: TRFaceEditor;
        FEBack: TRFaceEditor;
        MX, MY: Integer;

function MaterialEditorForm : TMaterialEditorForm;
begin
	if not Assigned(vMaterialEditorForm) then
	   vMaterialEditorForm:=TMaterialEditorForm.Create(nil);
	Result:=vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
	if Assigned(vMaterialEditorForm) then begin
	   vMaterialEditorForm.Free; vMaterialEditorForm:=nil;
	end;
end;

// Create
//
constructor TMaterialEditorForm.Create(AOwner : TComponent);
var
  I: Integer;
begin
	inherited;
  for i := 0 to Integer(High(TBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TBlendingMode), i));
    FEFront:= TRFaceEditor.Create(self);
    FEFront.Parent := TSFront;
    FEFront.Name:='FEFront';
    FEFront.Align:=alClient;

    FEBack:= TRFaceEditor.Create(self);
    FEBack.Parent := TSBack;
    FEBack.Name:='FEBack';
    FEBack.Align:=alClient;

    FEFront.OnChange:=OnMaterialChanged;
    FEBack.OnChange:=OnMaterialChanged;
    RTextureEdit.OnChange:=OnMaterialChanged;

    BackGroundSprite.Position.X := SceneViewer.Width div 2;
    BackGroundSprite.Position.Y := SceneViewer.Height div 2;
    BackGroundSprite.Width := SceneViewer.Width;
    BackGroundSprite.Height := SceneViewer.Height;

    CBObject.ItemIndex:=0;     CBObjectChange(Self);
    CBBackground.ItemIndex:=0; CBBackgroundChange(Self);
end;

// Execute
//
function TMaterialEditorForm.Execute(var amaterial : TGLMaterial) : Boolean;
begin
   with amaterial do begin
      FEFront.FaceProperties:=FrontProperties;
		FEBack.FaceProperties:=BackProperties;
		RTextureEdit.Texture:=Texture;
      CBBlending.ItemIndex:=Integer(BlendingMode);
	end;
	Self.Material:=material;
	Result:=(ShowModal=mrOk);
	if Result then with material do begin
		FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
end;

// OnMaterialChanged
//
procedure TMaterialEditorForm.OnMaterialChanged(Sender : TObject);
begin
   with Self.Material do begin
      FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
 // MPPreview.Render;
end;

procedure TMaterialEditorForm.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Cube.Visible   := I = 0;
   Sphere.Visible := I = 1;
   Cone.Visible   := I = 2;
   Teapot.Visible := I = 3;
end;

procedure TMaterialEditorForm.CBBackgroundChange(Sender: TObject);
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

procedure TMaterialEditorForm.SceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);

  MX := X;
  MY := Y;
end;

procedure TMaterialEditorForm.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TMaterialEditorForm.SceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

function TMaterialEditorForm.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TMaterialEditorForm.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value);
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {$i FMaterialEditorFormLCL.lrs}

finalization

   ReleaseMaterialEditorForm;

end.



