unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,

  GLWin32Viewer,
  GLMaterial, GLScene,
  GLCadencer, GLTexture, GLObjects,
  GLSkydome,
  GLColor,
  GLUtils,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;


type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Materials1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Cube1: TGLCube;
    Panel1: TPanel;
    Edit1: TEdit;
    SkyDome1: TGLSkyDome;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,newTime: Double);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure AssignBitmapToTexture;
  private
     
  public
     
  end;

var
  Form1   : TForm1;
  b       : Tpicture;
  EditMem : string = 'Edit1';

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  b := Tpicture.create;
  With b.bitmap do
  begin
    PixelFormat:=pf24bit;
    Width  := 640;  // set texture width
    height := 480;  // set texture height
    Canvas.Font.Name   :='Arial';
    Canvas.Font.Size   :=  36;
    Canvas.Font.Color  :=  clLime;
    Canvas.pen.color   :=  $00FFFF80;
    canvas.brush.color :=  $00FF9900;
    canvas.Rectangle(rect(2,2,b.Bitmap.Width-2,b.Bitmap.height-2));
    canvas.brush.color := $00FF9900;
    Canvas.textout(4,10,Edit1.text);
  end;

  Materials1.AddTextureMaterial('Blank',b.graphic);

  with Materials1.Materials[Materials1.Materials.Count-1].Material do
  begin
    Texture.Assign(b.Graphic);
    Texture.TextureMode            := tmModulate;
    Texture.MinFilter              := miLinear;
    Texture.MagFilter              := maLinear;
    BlendingMode                   := bmTransparency;
    Texture.ImageAlpha             := tiaSuperBlackTransparent;
    FrontProperties.Diffuse.Alpha  := 0.8;
    FrontProperties.Emission.color := clrWhite;
  end;

  with Cube1.Material do
  begin
    MaterialLibrary     := Materials1;
    LibMaterialName     := 'Blank';
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  b.free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Cube1.turnangle := Cube1.turnangle + 0.1;
  if EditMem <> Edit1.text then begin AssignBitmapToTexture;EditMem := Edit1.text;end;
end;

procedure TForm1.AssignBitmapToTexture;
begin
  With b.bitmap.canvas do
  begin
    brush.color := $00FF9900;
    Rectangle(rect(2,2,b.Bitmap.Width-2,b.Bitmap.height-2));
    brush.color := $00FF9900;
    textout(4,10,Edit1.text);
  end;
  Materials1.Materials.GetLibMaterialByName('Blank').Material.Texture.Image.assign(b.Graphic);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then close;
end;

end.
