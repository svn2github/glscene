unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLObjects, GLGraph, GLScene, GLMisc, GLWin32Viewer,
  VectorGeometry, GLTilePlane, GLTexture, GLCadencer, Jpeg, StdCtrls,
  OpenGL1x;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    GLXYZGrid: TGLXYZGrid;
    Panel1: TPanel;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLLightSource: TGLLightSource;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    CBMaterial: TComboBox;
    GLTilePlane: TGLTilePlane;
    GLDirectOpenGL: TGLDirectOpenGL;
    DCSelection: TGLDummyCube;
    GLLines1: TGLLines;
    BUPack: TButton;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLDirectOpenGLRender(var rci: TRenderContextInfo);
    procedure BUPackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    tileX, tileY : Integer;
    mip, translateOffset : TVector;
    translating : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Keyboard;

procedure TForm1.FormCreate(Sender: TObject);
var
   i, j : Integer;
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

   RandSeed:=0;
   for i:=-20 to 20 do for j:=-20 to 20 do
      GLTilePlane.Tiles[i, j]:=Random(GLMaterialLibrary.Materials.Count-1)+1;

   for i:=1 to GLMaterialLibrary.Materials.Count-1 do
      with GLMaterialLibrary.Materials[i] do begin
         Material.Texture.FilteringQuality:=tfAnisotropic;
         CBMaterial.Items.Add(Name);
      end;
   CBMaterial.ItemIndex:=0;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
   if Shift=[ssLeft] then begin
      GLTilePlane.Tiles[tileX, tileY]:=CBMaterial.ItemIndex+1;
      GLTilePlane.StructureChanged;
   end else if Shift=[ssRight] then begin
      GLTilePlane.Tiles[tileX, tileY]:=0;
      GLTilePlane.StructureChanged;
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   ip : TVector;
   mp : TPoint;
   shiftDown : Boolean;
begin
   shiftDown:=(IsKeyDown(VK_LSHIFT) or IsKeyDown(VK_RSHIFT));
   DCSelection.Visible:=not shiftDown;
   if DCSelection.Visible then
      GLSceneViewer1.Cursor:=crDefault
   else GLSceneViewer1.Cursor:=crHandPoint;

   GetCursorPos(mp);
   mp:=GLSceneViewer1.ScreenToClient(mp);
   if PtInRect(GLSceneViewer1.ClientRect, mp) then begin
      GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXY(
              VectorMake(mp.x, GLSceneViewer1.Height-mp.y, 0), 0, ip);
      tileX:=Round(ip[0]-0.5);
      tileY:=Round(ip[1]-0.5);
      DCSelection.Position.SetPoint(tileX, tileY, 0);

      if shiftDown then begin
         if IsKeyDown(VK_LBUTTON) then begin
            if not translating then begin
               translateOffset:=ip;
               translating:=True;
            end;
            DCTarget.Position.Translate(VectorAdd(VectorSubtract(mip, ip), translateOffset))
         end else translating:=False;
         if IsKeyDown(VK_RBUTTON) then begin
            GLCamera.MoveAroundTarget((my-mp.y)*0.5, (mx-mp.x)*0.5);
         end;
      end else begin
         translating:=False;
         if IsKeyDown(VK_LBUTTON) then begin
            GLTilePlane.Tiles[tileX, tileY]:=CBMaterial.ItemIndex+1;
            GLTilePlane.StructureChanged;
         end;
         if IsKeyDown(VK_RBUTTON) then begin
            GLTilePlane.Tiles[tileX, tileY]:=0;
            GLTilePlane.StructureChanged;
         end;
      end;
//      mip:=ip;
      mx:=mp.x;
      my:=mp.y;
   end;

   GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLDirectOpenGLRender(var rci: TRenderContextInfo);
begin
   glClear(GL_DEPTH_BUFFER_BIT);
end;

procedure TForm1.BUPackClick(Sender: TObject);
begin
   GLTilePlane.Tiles.Pack;
end;

end.
