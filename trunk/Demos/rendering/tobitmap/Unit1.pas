unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLObjects, GLScene, ExtCtrls, GLMisc, GLWin32Viewer,
  GLHUDObjects, StdCtrls;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Panel1: TPanel;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Plane1: TPlane;
    Sphere1: TSphere;
    GLCadencer1: TGLCadencer;
    DummyCube1: TDummyCube;
    HUDSprite1: THUDSprite;
    BUCapture: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Sphere1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BUCaptureClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Jpeg, Unit2, GLGraphics;

procedure ViewBitmap(aBitmap : TBitmap; caption : String);
var
   f : TForm2;
begin
   Application.CreateForm(TForm2, f);
   f.Image1.Picture.Bitmap:=aBitmap;
   f.Image1.Width:=aBitmap.Width;
   f.Image1.Height:=aBitmap.Height;
   f.Caption:=caption;
   f.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   HUDSprite1.Material.Texture.Image.LoadFromFile('..\..\media\ashwood.jpg');
   Plane1.Material.Texture.Image.LoadFromFile('..\..\media\marbletiles.jpg');
   Sphere1.Material.Texture.Image.LoadFromFile('..\..\media\marbletiles.jpg');
end;

procedure TForm1.Sphere1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   h : Single;
begin
   h:=2.5+2*Sin(newTime*4);
   Sphere1.Position.Y:=h;
   if h<1 then
      Sphere1.Scale.Y:=h
   else Sphere1.Scale.Y:=1;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   with HUDSprite1 do begin
      Width:=GLSceneViewer1.Width;
      Position.X:=Width*0.5;
      Height:=GLSceneViewer1.Height;
      Position.Y:=Height*0.5;
   end;
end;

procedure TForm1.BUCaptureClick(Sender: TObject);
var
   bmp32 : TGLBitmap32;
   bmp : TBitmap;
begin
   bmp32:=GLSceneViewer1.Buffer.CreateSnapShot;
   bmp:=bmp32.Create32BitsBitmap;
   ViewBitmap(bmp, 'SnapShot - ');
   bmp.Free;
   bmp32.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
   bmp : TBitmap;
begin
   bmp:=TBitmap.Create;
   bmp.PixelFormat:=pf24bit;
   bmp.Width:=GLSceneViewer1.Width;
   bmp.Height:=GLSceneViewer1.Height;
   GLSceneViewer1.Buffer.RenderToBitmap(bmp, 0);
   ViewBitmap(bmp, 'RenderToBitmap - ');
   bmp.Free;
end;

end.
