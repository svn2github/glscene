{: Using the GLBumpShader for object space bump mapping.<p>

   The bump shader runs an ambient light pass and then a
   pass for each light shining in the scene. Currently there
   is only the dot3 texture combiner method available, but I
   will be adding some fragment program alternatives soon
   that will support specular lighting and possibly process
   multiple lights in one pass.<p>

   For the shader to work you need to have the normal map
   as the primary texture. For now the secondary texture is
   disabled, that will change with the next update.<p>
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLObjects, GLTexture, GLBumpShader, GLScene,
  GLVectorFileObjects, GLMisc, GLCadencer, GLWin32Viewer, JPEG, ExtCtrls,
  StdCtrls, AsyncTimer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Camera: TGLCamera;
    WhiteLight: TGLLightSource;
    Bunny: TGLFreeForm;
    RedLight: TGLLightSource;
    BlueLight: TGLLightSource;
    GLBumpShader1: TGLBumpShader;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    ColorDialog1: TColorDialog;
    DCLights: TGLDummyCube;
    AsyncTimer1: TAsyncTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBoxClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my, dx, dy : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

  // Load the bunny mesh and scale for viewing
  Bunny.LoadFromFile('bunny.glsm');
  Bunny.Scale.Scale(2/Bunny.BoundingSphereRadius);

  // Load the normal map
  with GLMaterialLibrary1.Materials[0].Material.Texture.Image do
    LoadFromFile('bunnynormals.jpg');

  // Link the lights to their toggles
  CheckBox1.Tag:=Integer(WhiteLight);
  CheckBox2.Tag:=Integer(RedLight);
  CheckBox3.Tag:=Integer(BlueLight);
  Shape1.Tag:=Integer(WhiteLight);
  Shape2.Tag:=Integer(RedLight);
  Shape3.Tag:=Integer(BlueLight);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // Orbit the camera
  if (dx<>0) or (dy<>0) then
    Camera.MoveAroundTarget(dy, dx);

  // Rotate the light sources
  DCLights.Turn(deltaTime*10);
end;

procedure TForm1.CheckBoxClick(Sender: TObject);
begin
  // Light Shining CheckBox
  TGLLightSource(TCheckBox(Sender).Tag).Shining:=TCheckBox(Sender).Checked;
end;

procedure TForm1.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Light Color Dialog
  ColorDialog1.Color:=TShape(Sender).Brush.Color;
  if ColorDialog1.Execute then begin
    TShape(Sender).Brush.Color:=ColorDialog1.Color;
    with TGLLightSource(TShape(Sender).Tag) do
      Diffuse.AsWinColor:=ColorDialog1.Color;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex = 0 then
    Bunny.Material.LibMaterialName:=''
  else
    Bunny.Material.LibMaterialName:='Bump';
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
  dx:=0;
  dy:=0;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    dx:=dx+(mx-x);
    dy:=dy+(my-y);
  end else begin
    dx:=0;
    dy:=0;
  end;
  mx:=x;
  my:=y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:='GLBumpShader Demo - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
