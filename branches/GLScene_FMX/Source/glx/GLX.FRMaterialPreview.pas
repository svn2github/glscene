//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRMaterialPreview<p>

   Material Preview frame.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>14/07/23 - PW - Changed to support FireMonkey platform
      <li>12/07/07 - DaStr - Improved Cross-Platform compatibility
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>16/12/06 - DaStr - Editor enhanced
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit GLX.FRMaterialPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  //FMX
  FMX.Types, FMX.Graphics, FMX.Colors, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox,

  //GLS
  GLX.CrossPlatform, GLX.VectorTypes,
  //Need to be changed with GLS.* units
  GLMaterial,  BaseClasses, GLScene, GLTeapot,
  GLGeomObjects, GLObjects, GLHUDObjects, GLCoordinates, GLCrossPlatform,
  GLScene.FMX.Viewer;

type
  TRMaterialPreview = class(TFrame)
    CBObject: TComboBox;
    CBBackground: TComboBox;
    GLScene: TGLScene;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLSceneViewport: TGLSceneViewport;
    GLCamera: TGLCamera;
    BackgroundSprite: TGLHUDSprite;
    LightSource: TGLLightSource;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    Cube: TGLCube;
    Sphere: TGLSphere;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    FireSphere: TGLSphere;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

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
   aColor : Integer;
   ColorVector : TVector4f;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:= TColors.White; // $00FFFFFF;  //clWhite;
      2 : bgColor:= TColors.Black; // $00000000;  //clBlack;
      3 : bgColor:= TColors.Blue;  // $00FF0000;  //clBlue;
      4 : bgColor:= TColors.Red;   // $000000FF;  //clRed;
      5 : bgColor:= TColors.Green; // $0000FF00;  //clGreen;
   else
      bgColor:= TColors.Null; // $1FFFFFFF; //clNone;
   end;
   with BackGroundSprite.Material do
   begin
      Texture.Disabled:=(bgColor<>$1FFFFFFF);
      aColor := TColors.ColorToRGB(bgColor);
      ColorVector.X := (aColor and $FF) * (1 / 255);
      ColorVector.Y := ((aColor shr 8) and $FF) * (1 / 255);
      ColorVector.Z := ((aColor shr 16) and $FF) * (1 / 255);
      ColorVector.W := 1; //Alpha;
      /// FrontProperties.Diffuse.Color:= ColorVector;
   end;
end;

end.
