unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GLScene, GLVectorFileObjects, GLObjects, GLTexture,
  GLWin32Viewer, GLMisc, VectorLists, ComCtrls, ExtCtrls, GLCadencer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Button1: TButton;
    BUSubdivide: TButton;
    TrackBar1: TTrackBar;
    RBWireFrame: TRadioButton;
    RBSolid: TRadioButton;
    CBAnimate: TCheckBox;
    GLActor1: TGLActor;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BUSubdivideClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RBWireFrameClick(Sender: TObject);
    procedure RBSolidClick(Sender: TObject);
    procedure CBAnimateClick(Sender: TObject);
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

uses MeshUtils, Geometry, Jpeg, TGA, GLFileObj;

procedure TForm1.Button1Click(Sender: TObject);
begin
   GLMaterialLibrary1.TexturePaths:='..\..\media';

//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\polyhedron.3ds');
//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\mushroom.3ds');
//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\trinityrage.smd');

   GLActor1.LoadFromFile('..\..\media\waste.md2');
   GLActor1.Material.Texture.Image.LoadFromFile('..\..\media\waste.jpg');
   GLActor1.Material.Texture.Enabled:=True;
   GLActor1.SwitchToAnimation(GLActor1.Animations[0]);

//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\HighPolyObject.3ds');

   CBAnimateClick(Self);
end;

procedure TForm1.BUSubdivideClick(Sender: TObject);
var
   i : Integer;
   tris, norms, tex, buf : TAffineVectorList;
   indices, texIndices : TIntegerlist;
begin
   for i:=0 to GLActor1.MeshObjects.Count-1 do begin
      tex:=TAffineVectorList.Create;
      with GLActor1.MeshObjects[i] do begin
         tris:=ExtractTriangles(tex);
      end;
      indices:=BuildVectorCountOptimizedIndices(tris);

      RemapAndCleanupReferences(tris, indices);

      norms:=BuildNormals(tris, indices);

      // subdivide geometry
      SubdivideTriangles(TrackBar1.Position*0.1, tris, indices, norms);

      texIndices:=BuildVectorCountOptimizedIndices(tex);
      RemapAndCleanupReferences(tex, texIndices);

      // subdivide texture space
      SubdivideTriangles(0, tex, texIndices);

      // Re-expand everything
      buf:=TAffineVectorList.Create;
      try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
      finally
         buf.Free;
      end;

      // Pack & Optimize the expanded stuff
      indices.Free;
      indices:=BuildVectorCountOptimizedIndices(tris, norms, tex);
      RemapReferences(norms, indices);
      RemapReferences(tex, indices);
      RemapAndCleanupReferences(tris, indices);

      IncreaseCoherency(indices, 13);

      with GLActor1.MeshObjects[i] as TMorphableMeshObject do begin
         Vertices:=tris;
         Normals:=norms;
         TexCoords:=tex;
         FaceGroups.Clear;
         with TFGVertexIndexList.CreateOwned(FaceGroups) do begin
            VertexIndices:=indices;
            Mode:=fgmmTriangles;
         end;
      end;

      tex.Free;
      indices.Free;
      norms.Free;
      tris.Free;
   end;
   GLActor1.StructureChanged;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   end else if Shift=[ssRight] then begin
      GLCamera1.RotateTarget(my-y, mx-x);
   end;
   mx:=x;
   my:=y;
end;

procedure TForm1.RBWireFrameClick(Sender: TObject);
begin
   GLActor1.Material.FrontProperties.PolygonMode:=pmLines;
   GLActor1.Material.BackProperties.PolygonMode:=pmLines;
end;

procedure TForm1.RBSolidClick(Sender: TObject);
begin
   GLActor1.Material.FrontProperties.PolygonMode:=pmFill;
   GLActor1.Material.BackProperties.PolygonMode:=pmFill;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS -  %d Triangles',
                   [GLSceneViewer1.FramesPerSecond,
                    GLActor1.MeshObjects.TriangleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.CBAnimateClick(Sender: TObject);
begin
   if CBAnimate.Checked then begin
      GLActor1.AnimationMode:=aamLoop;
      GLActor1.ObjectStyle:=GLActor1.ObjectStyle+[osDirectDraw];
      GLActor1.Reference:=aarMorph;
   end else begin
      GLActor1.AnimationMode:=aamNone;
      GLActor1.MeshObjects.MorphTo(0);
      GLActor1.Reference:=aarNone;
      GLActor1.StructureChanged;
      GLActor1.ObjectStyle:=GLActor1.ObjectStyle-[osDirectDraw];
   end;
end;

end.
