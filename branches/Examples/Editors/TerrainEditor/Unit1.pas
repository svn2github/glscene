unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  //GLS
  GLScene,
  GLObjects,
  GLCadencer,
  GLTexture,
  GLWin32Viewer,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo,
  //
  TerrainEngine;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCone1: TGLCone;
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    FTerrain: TTerrain;
    GPoint: TPoint;

    FBrush: TTerrainBrush;
    FModifier: TTerrainHeightModifier;

    procedure UpdateCursor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Align:= alClient;
  GLSceneViewer1.Cursor:= crNone;

  FTerrain:= TTerrain.Create('debug.bmp', 0.05, 0.5);

  GPoint.X:= FTerrain.Data.XDim div 2;
  GPoint.Y:= FTerrain.Data.YDim div 2;

  UpdateCursor;

  FBrush:= TSmoothCircularBrush.Create;

  FModifier:= TElevateModifier.Create(FTerrain.Data);  
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBrush.Free;
  FModifier.Free;
  FTerrain.Free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  GLMaterialLibrary1.ApplyMaterial('TerrainMat', rci);

  FTerrain.Render;

  GLMaterialLibrary1.UnApplyMaterial(rci);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  GPoint.X:= round(((X / ClientWidth - 0.2) * 1.5) * FTerrain.Data.XDim);
  GPoint.Y:= round(((Y / ClientHeight - 0.2) * 1.5) * FTerrain.Data.YDim);

  UpdateCursor;

  if ssLeft in Shift then
  begin
    // increase elevation
    FModifier.Modify(GPoint.X, GPoint.Y, FBrush, 0.1);
    FTerrain.Data.CalculateNormals;
  end
  else if ssRight in Shift then
  begin
    // decrease elevation
    FModifier.Modify(GPoint.X, GPoint.Y, FBrush, -0.1);
    FTerrain.Data.CalculateNormals;
  end;
end;

procedure TForm1.UpdateCursor;
begin
  GLCone1.Position.AsAffineVector:= FTerrain.GridPointPosition(GPoint.X, GPoint.Y);
  GLCone1.Position.Y:= GLCone1.Position.Y + GLCone1.Height / 2;
end;

end.
