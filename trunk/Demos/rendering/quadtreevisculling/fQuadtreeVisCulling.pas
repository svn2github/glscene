{: Using the TOctreeSpacePartition for visibility culling.<p>

  Demo by HRLI slightly reworked to be a quadtree demo and committed by MF.<p>
}

unit fQuadtreeVisCulling;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLWin32Viewer, GLSkydome, GLMisc, GLObjects,  jpeg, keyboard,
  GLHeightData, GLTerrainRenderer, GLTexture, GLCadencer, GLNavigator,
  SpatialPartitioning,VectorGeometry, ExtCtrls, GLBitmapFont, GeometryBB,
  GLWindowsFont, GLHUDObjects, StdCtrls, ComCtrls, OpenGL1x;

type
  TfrmQuadtreeVisCulling = class(TForm)
    GLScene1: TGLScene;
    trees: TGLDummyCube;
    GLSkyDome1: TGLSkyDome;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTerrainRenderer1: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    queryVisible: TGLDirectOpenGL;
    Timer1: TTimer;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLDirectOpenGL1: TGLDirectOpenGL;
    cbUseQuadtree: TCheckBox;
    Panel1: TPanel;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    cbShowQuadtree: TCheckBox;
    GLDirectOpenGL2: TGLDirectOpenGL;
    tree: TGLSprite;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure queryVisibleRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure cbShowQuadtreeClick(Sender: TObject);
    procedure GLDirectOpenGL2Render(Sender: TObject;
      var rci: TRenderContextInfo);
  private
    { Private declarations }
    cullingMode: string;
    visiblecount,treecount: integer;
    SpacePartition: TSectoredSpacePartition;
    procedure CreateTrees;
  public

  end;

var
  frmQuadtreeVisCulling: TfrmQuadtreeVisCulling;

implementation

uses GLSpatialPartitioning;

{$R *.dfm}

procedure TfrmQuadtreeVisCulling.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
  var
   speed : Single;
begin
  GLUserInterface1.MouseLook;
  GLUserInterface1.MouseUpdate;
   if IsKeyDown(VK_SHIFT) then
      speed:=3000*deltaTime
   else speed:=1000*deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(87) then
         GLNavigator1.MoveForward(speed);
      if IsKeyDown(83) then
         GLNavigator1.MoveForward(-speed);
      if IsKeyDown(65) then
         GLNavigator1.StrafeHorizontal(-speed);
      if IsKeyDown(68) then
         GLNavigator1.StrafeHorizontal(speed);
      if IsKeyDown(VK_ESCAPE) then Close;
   end;
   with GLCamera1.Position do
      Y:=glTerrainRenderer1.InterpolatedHeight(AsVector)+80;//+FCamHeight;
   GLHUDText1.Text := cullingMode+ 'visible tree count: '+inttostr(visiblecount)+' / Total:'+inttostr(treecount)+
     #13#10+ ' press ''V'' to Change quadtree query visible or visiblity culling'+
     #13#10+ ' press ''esc'' to quit';
end;

procedure TfrmQuadtreeVisCulling.FormCreate(Sender: TObject);
begin
  SpacePartition := TQuadSpacePartition.Create;
  SpacePartition.LeafThreshold := 50;
  SpacePartition.MaxTreeDepth := 10;//}

  tree.visible := false;
  trees.ObjectsSorting := osRenderFarthestFirst;

  GLBitmapHDS1.Picture.LoadFromFile('..\..\media\terrain.bmp');
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('..\..\media\snow512.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('..\..\media\detailmap.jpg');
  tree.Material.Texture.Image.LoadFromFile('..\..\media\tree1.bmp');
  Show;
  CreateTrees;
  cullingMode := 'quadtree ';
  GLUserInterface1.MouseLookActivate;
end;

procedure TfrmQuadtreeVisCulling.CreateTrees;
var
  i,j: integer;
  obj: TGLProxyObject;
begin
  glscene1.BeginUpdate;

  ProgressBar1.Max := 80*80;
  Label1.Refresh;

  for i := -40 to 40 do
  for j := -40 to 40 do
  begin
    inc(treecount);
    ProgressBar1.Position := TreeCount;
    obj := TGLProxyObject(trees.AddNewChild(TGLProxyObject));
    obj.MasterObject := tree;
    obj.Position.AsAffineVector := AffineVectorMake(i*500+random(200),0,j*500+random(200));
    with obj.Position do
      y := GLTerrainRenderer1.InterpolatedHeight(obj.AbsolutePosition)+150;
    TSceneObj.CreateObj(SpacePartition,obj);

    Label2.Caption := Format('Leaves = %d, Nodes = %d, NodesInRoot = %d',[
      SpacePartition.Leaves.Count,
      SpacePartition.GetNodeCount,
      SpacePartition.RootNode.Leaves.Count]);

    Label2.Refresh;

  end;
  Panel1.Free;
  glscene1.EndUpdate;
end;

procedure TfrmQuadtreeVisCulling.FormDestroy(Sender: TObject);
begin
  SpacePartition.Free;
end;

procedure TfrmQuadtreeVisCulling.queryVisibleRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
  i: integer;
begin
  if not cbUseQuadtree.Checked then exit;

  glscene1.BeginUpdate;
  for i := 0 to trees.Count - 1 do
    trees.Children[i].Visible := false;

  // Query the Quadtree for objects that intersect the frustum
  SpacePartition.QueryFrustum(rci.rcci.frustum);
  visiblecount := SpacePartition.QueryResult.Count;
  Label2.Caption := Format('Visible = %d, ObjTests = %d, NodeTests = %d, TotalNodes = %d',[
    SpacePartition.QueryResult.Count,
    SpacePartition.QueryInterObjectTests,
    SpacePartition.QueryNodeTests,
    SpacePartition.GetNodeCount]);

  for i := 0 to SpacePartition.QueryResult.Count - 1 do begin
    TSceneObj(SpacePartition.QueryResult[i]).Obj.Visible := true;
  end;
  glscene1.EndUpdate;
end;

procedure TfrmQuadtreeVisCulling.Timer1Timer(Sender: TObject);
begin
  caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;


procedure TfrmQuadtreeVisCulling.FormKeyPress(Sender: TObject; var Key: Char);
var
  i: integer;
begin
  if key = 'v' then
  begin
    cbUseQuadtree.Checked := not cbUseQuadtree.Checked;

    if cbUseQuadtree.Checked then
    begin
      cullingMode := ' Quadtree';
      trees.VisibilityCulling := vcNone;
    end else
    begin
      cullingMode := 'visibility culling ';
      for i := 0 to trees.Count -1 do
        trees.Children[i].Visible := true;
      trees.VisibilityCulling := vcObjectBased;
    end;
  end;
end;

procedure TfrmQuadtreeVisCulling.cbShowQuadtreeClick(Sender: TObject);
begin
  GLDirectOpenGL2.Visible := cbShowQuadtree.Checked;
end;

procedure TfrmQuadtreeVisCulling.GLDirectOpenGL2Render(
  Sender: TObject; var rci: TRenderContextInfo);
begin
  RenderSpatialPartitioning(SpacePartition);
end;
end.
