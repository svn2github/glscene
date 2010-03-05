{: Shows how to use 1D palettes to visualize "intensity" on a mesh.<p>

   Typical application is interactive representation of FEA (Finite Element
   Analysis), the palette texture can be used with discrete colors
   (use "nearest" filtering) or continuous colors (use "linear" filtering).
   Palette scale and offset can then be adjusted via texture scaling/off.<p>

   Representation uses a simple multipass shader to overlay the model's
   wireframe using smoothed lines.<p>

   Sample Data represents the Von Mises Stress (usually is used to predict
   steel yelding) over the strucutre of a tubular tower for electrical
   power lines. That stress is the result of the loads from cables,
   transversal wind and self weigth.

   (Sample Data contributed by Carlos Ferreira)
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, GLScene,
  GLVectorFileObjects, GLWin32Viewer, GLMesh, GLTexture, GLUserShader,
  StdCtrls, GLHUDObjects, VectorGeometry, OpenGL1x, ComCtrls, GLObjects,
  ExtCtrls, GLBitmapFont, GLWindowsFont, GLUtils, GLMaterial, GLCoordinates,
  GLCrossPlatform, BaseClasses, GLRenderContextInfo, GLGraphics;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLUserShader: TGLUserShader;
    HSPalette: TGLHUDSprite;
    GLWindowsBitmapFont: TGLWindowsBitmapFont;
    HTPaletteLeft: TGLHUDText;
    HTPaletteRight: TGLHUDText;
    Panel1: TPanel;
    CBWireFrame: TCheckBox;
    CBSmooth: TCheckBox;
    TBScale: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLUserShaderDoUnApply(Sender: TObject; Pass: Integer;
      var rci: TRenderContextInfo; var Continue: Boolean);
    procedure CBSmoothClick(Sender: TObject);
    procedure CBWireFrameClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TBScaleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GLState;

type
   // Structures used in our binary file
   // The structure is quite simplified here, original data came from a FEM
   // package and was in (huge) text files, and parsing text files is not the
   // purpose of this demo, so data was simplified ;)
   TDataNode = record
      X, Y, Z : Single;
      Intensity : Single;
   end;
   TDataPrimitive = record
      Node1, Node2, Node3, Node4 : Word;  // if Node4 is $FFFF, codes a triangle
   end;
var
   DataNodes : array of TDataNode;
   DataPrimitives : array of TDataPrimitive;

procedure TForm1.FormCreate(Sender: TObject);
var
   mo : TMeshObject;
   fgQuads, fgTris : TFGVertexIndexList;
   i : Integer;
   str : TFileStream;
begin
   // load our raw data
   str:=TFileStream.Create('IntensityMesh.data', fmOpenRead);
   str.Read(i, 4);
   SetLength(DataNodes, i);
   str.Read(i, 4);
   SetLength(DataPrimitives, i);
   str.Read(DataNodes[0], Length(DataNodes)*SizeOf(TDataNode));
   str.Read(DataPrimitives[0], Length(DataPrimitives)*SizeOf(TDataPrimitive));
   str.Free;

   // fill the freeform with our data

   // first create a mesh object
   mo:=TMeshObject.CreateOwned(GLFreeForm.MeshObjects);
   mo.Mode:=momFaceGroups;
   // Specify vertex and texcoords data (intensity is stored a texcoord)
   for i:=0 to High(DataNodes) do begin
      mo.Vertices.Add(DataNodes[i].X, DataNodes[i].Y, DataNodes[i].Z);
      mo.TexCoords.Add(DataNodes[i].Intensity*0.001, 0);
   end;
   // Then create the facegroups that will hold our quads and triangles
   fgQuads:=TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   fgQuads.Mode:=fgmmQuads;
   fgTris:=TFGVertexIndexList.CreateOwned(mo.FaceGroups);
   fgTris.Mode:=fgmmTriangles;
   // and fill them with our primitives
   for i:=1 to High(DataPrimitives) do with DataPrimitives[i] do begin
      if Node4<>$FFFF then begin
         fgQuads.VertexIndices.Add(Node1, Node2);
         fgQuads.VertexIndices.Add(Node4, Node3);
      end else begin
         fgTris.VertexIndices.Add(Node1, Node2, Node3);
      end;
   end;
   // auto center
   GLFreeForm.PerformAutoCentering;
   // and initialize scale
   TBScaleChange(Self);
end;

procedure TForm1.GLUserShaderDoUnApply(Sender: TObject; Pass: Integer;
  var rci: TRenderContextInfo; var Continue: Boolean);
begin
   if not CBWireFrame.Checked then
      Pass:=2; // skip wireframe pass
   case Pass of
      1 : begin
         // 2nd pass is a wireframe pass (two-sided)
         rci.GLStates.PushAttrib([sttEnable]);  // backup states
         rci.GLStates.Disable(stTexture2D);
         rci.GLStates.Enable(stLineSmooth); // setup smoothed lines
         rci.GLStates.Enable(stBlend);
         rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
         rci.GLStates.LineWidth := 0.5;
         rci.GLStates.PolygonMode := pmLines;
         rci.GLStates.SetPolygonOffset(-1, -1);
         rci.GLStates.Enable(stPolygonOffsetLine);
         glColor3f(0, 0, 0);
         Continue:=True;
      end;
   else
      // restore states or mark them dirty
      if CBWireFrame.Checked then
         rci.GLStates.PopAttrib;
      rci.GLStates.ResetGLPolygonMode;
      Continue:=False;
   end;
end;

procedure TForm1.CBSmoothClick(Sender: TObject);
var
   tex : TGLTexture;
begin
   // switch between linear and nearest filtering
   tex:=GLMaterialLibrary1.Materials[0].Material.Texture;
   if CBSmooth.Checked then begin
      tex.MagFilter:=maLinear;
      tex.MinFilter:=miLinear;
   end else begin
      tex.MagFilter:=maNearest;
      tex.MinFilter:=miNearest;
   end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
   GLSceneViewer1.SetFocus;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera.MoveAroundTarget(my-y, mx-x);
   if ssRight in Shift then begin
      DCTarget.Position.AddScaledVector((mx-x)/30, GLCamera.AbsoluteRightVectorToTarget);
      DCTarget.Position.AddScaledVector((y-my)/30, GLCamera.AbsoluteUpVectorToTarget);
   end;
   mx:=x; my:=y;
end;

procedure TForm1.TBScaleChange(Sender: TObject);
begin
   with GLMaterialLibrary1.Materials[0] do
      TextureScale.X:=TBScale.Position/100;
   HTPaletteRight.Text:=Format('%d', [TBScale.Position*10]);
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera.AdjustDistanceToTarget(Power(1.03, WheelDelta/120));
end;

procedure TForm1.CBWireFrameClick(Sender: TObject);
begin
   GLSceneViewer1.Invalidate;
end;

end.
