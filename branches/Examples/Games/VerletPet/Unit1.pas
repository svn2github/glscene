unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.JPEG,
   
  GLWin32Viewer,
  GLScene,
  GLCadencer,
  GLObjects,
  GLVectorFileObjects,
  GLTexture,
  GLFileSMD,
  GLFileTGA,
  GLKeyboard, GLFile3DS, GLVectorTypes, GLTree,
  GLVectorGeometry, GLVerletTypes,GLVerletSkeletonColliders, GLVerletClothify,
  GLGeomObjects,GLBehaviours, GLExtrusion, GLHUDObjects, GLRenderContextInfo,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    Timer1: TTimer;
    Actor1: TGLActor;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    DummyActor: TGLDummyCube;
    ODECube: TGLDummyCube;
    GLDisk1: TGLDisk;
    Tela: TGLActor;
    CentroCube: TGLDummyCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLSphere1: TGLSphere;
    DummyCamara: TGLDummyCube;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    MaterialArbol: TGLMaterialLibrary;
    CheckBox2: TCheckBox;
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLDirectOpenGL1Render(var rci: TGLRenderContextInfo);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
     
    procedure ActorHace(accion:string; Realizarla:boolean);
    procedure ChecarTeclas(const deltaTime: Double);
  public
     
    GLTree1:TGLTree;
    proxy : TGLProxyObject;
    mx,my, mx2, my2: Integer;
    VerletWorld : TGLVerletWorld;
    EdgeDetector : TEdgeDetector;
    VCSphere: TVCSphere;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ActorHace(accion:string; Realizarla:boolean);
begin
   if ((Accion<>Actor1.CurrentAnimation)and(Realizarla=True)) then
      Actor1.SwitchToAnimation(accion);
   if ((Accion=Actor1.CurrentAnimation)and(Realizarla=False)) then
      Actor1.SwitchToAnimation('idle');
end;

procedure TForm1.ChecarTeclas(const deltaTime: Double);
const
   VELOCIDAD_ROTACION = 100;
   VELOCIDAD_MOVIMIENTO = 70;
begin
 if IsKeyDown(VK_LEFT) or IsKeyDown(VK_RIGHT) then
 begin
   if IsKeyDown(VK_LEFT)    then   //a la izquierda
     DummyActor.Turn(-VELOCIDAD_ROTACION*deltaTime);
   if IsKeyDown(VK_RIGHT)   then   //a la derecha
     DummyActor.Turn(VELOCIDAD_ROTACION*deltaTime);
   GLSCeneViewer1.SetFocus;
 end;

 if IsKeyDown(VK_UP) or IsKeyDown(VK_DOWN) then
 begin
    if IsKeyDown(VK_UP) then    //Si es hacia adelante
    begin
     DummyActor.Move(VELOCIDAD_MOVIMIENTO*deltaTime);
     If DummyActor.DistanceTo(CentroCube) >  250 then //Checamos que no se salga del circulo
         DummyActor.Move(-VELOCIDAD_MOVIMIENTO*deltaTime);
    end
    else
    begin
        DummyActor.Move(-VELOCIDAD_MOVIMIENTO*deltaTime); //si es hacia atras
        If DummyActor.DistanceTo(CentroCube) >  250 then //Checamos que no se salga del circulo
           DummyActor.Move(VELOCIDAD_MOVIMIENTO*deltaTime);
    end;
    DummyCamara.Position.Assign(DummyActor.Position);
    ActorHace('run',True);   //Cambiamos las animaciones
    GLSCeneViewer1.SetFocus;
 end
   else ActorHace('run',False);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS  Integración Verlet', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
   mx2:=x; my2:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 //Usamos esta técnica de mover el objeto para no interferir con la animación...
   if ssLeft in Shift then
   begin
      mx2:=x; my2:=y;
   end 
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
 //Permitimos mover la cámara...
 if ((mx<>mx2)or(my<>my2)) then begin
    GLCamera1.MoveAroundTarget(my-my2, mx-mx2);
    mx:=mx2; my:=my2;
 end;
  ChecarTeclas(deltaTime);
  VCSphere.Location := DummyActor.Position.AsAffineVector;

  //Actualizamos el Mundo
  VerletWorld.Progress(deltaTime, newTime);
  DummyCamara.Position.Assign(DummyActor.Position);
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:Integer;
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName)+'\media\');
  Actor1.LoadFromFile('qtz_ref.smd');
  Actor1.AddDataFromFile('idle.smd');
  Actor1.AddDataFromFile('run.smd');
  Actor1.Animations[0].MakeSkeletalTranslationStatic;
  Actor1.Animations[1].MakeSkeletalTranslationStatic;
  Actor1.SwitchToAnimation('idle');

  Tela.LoadFromFile('capa.3ds');
  Tela.ObjectStyle:=Tela.ObjectStyle+[osDirectDraw];
  Tela.Scale.SetVector(3,3,3);
  Tela.BuildSilhouetteConnectivityData;
  Tela.Material.Texture.Image.LoadFromFile('tallo.jpg');
  Tela.Material.Texture.Disabled := False;

  GLDisk1.Material.Texture.Image.LoadFromFile('Grass3.jpg');
  GLDisk1.Material.Texture.Disabled := False;

  GLSphere1.Material.Texture.Image.LoadFromFile('aura.bmp');

  VerletWorld := TGLVerletWorld.Create;
  VerletWorld.CreateOctree(
    AffineVectorMake(0,0,0),
    AffineVectorMake(0,0,0), 10, 6);

  VerletWorld.UpdateSpacePartion := uspEveryFrame;
  VerletWorld.Iterations := 3;

  // Asignamos los vertices de la tela al mundo Verlet
  EdgeDetector:=TEdgeDetector.Create(Tela);
  EdgeDetector.ProcessMesh;
  EdgeDetector.AddEdgesAsSticks(VerletWorld, 0.15);
  EdgeDetector.AddEdgesAsSolidEdges(VerletWorld);
  for i := 0 to Pred(VerletWorld.Nodes.Count) do
  begin
     VerletWorld.Nodes.Items[i].Weight := 50;
  end;

  for i := 110 to 120 do
     VerletWorld.Nodes.Items[i].NailedDown := True;

  // Iniciamos la gravedad y el suelo del mundo
  with TVFGravity.Create(VerletWorld) do
    Gravity:=AffineVectorMake(0,-98.1,0);
  with TVCFloor.Create(VerletWorld) do
  begin
    Normal:=GLDisk1.Direction.AsAffineVector;
    Location:=VectorAdd(GLDisk1.Position.AsAffineVector,
                        VectorScale(GLDisk1.Direction.AsAffineVector,1));
  end;

   // CreaCola;

   //Creamos el aura repelente
     VCSphere := TVCSphere.Create(VerletWorld);
     VCSphere.FrictionRatio := 0.05;
     VCSphere.Radius :=   Actor1.BoundingSphereRadiusUnscaled/2;
     VCSphere.Location := Actor1.Position.AsAffineVector;
     GLSphere1.Radius := VCSphere.Radius;
     GLSphere1.Position.AsAffineVector := VCSPhere.Location;

   //Ponemos un árbol feliz en nuestro mundo...
   GLTree1:=TGLTree(GLScene1.Objects.AddNewChild(TGLTree));
    with GLTree1 do begin
      with MaterialArbol.AddTextureMaterial('Tallo','tallo.jpg') do
         Material.Texture.TextureMode:=tmModulate;
      with MaterialArbol.AddTextureMaterial('Hoja','hoja.tga') do begin
        Material.BlendingMode:=bmAlphaTest50;
        Material.Texture.TextureMode:=tmModulate;
        Material.Texture.TextureFormat:=tfRGBA;
      end;
      MaterialLibrary:=MaterialArbol;
      LeafMaterialName:='Hoja';
      LeafBackMaterialName:='Hoja';
      BranchMaterialName:='Tallo';
      Depth:=9;
      LeafSize:=10;
      BranchRadius:=8;
      BranchSize := 40;
      BranchNoise:=0.5;
      Position.AsVector := VectorMake(-150,10,150);
      Direction.AsAffineVector := affineVectorMake(0,1,0);
      Randomize;
      Seed:=Round((2*Random-1)*(MaxInt-1));
    end;

    //Y le ponemos un hermanito en otra posición...
     proxy:=TGLProxyObject(GLScene1.Objects.AddNewChild(TGLProxyObject));
      with proxy do begin
         ProxyOptions:=[pooObjects];
         MasterObject:=GLTree1;
         Direction:=GLTree1.Direction;
         Up:=GLTree1.Up;
         Position.SetPoint(GLTree1.Position.x+200,
                           GLTree1.Position.y,
                           GLTree1.Position.z-300);
      end;
  CheckBox2Click(Sender);

  Label1.Caption := Format(' Nodos=%d, Restricciones=%d',[VerletWorld.Nodes.Count, VerletWorld.Constraints.Count]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 VerletWorld.Free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.GLDirectOpenGL1Render(var rci: TGLRenderContextInfo);
begin
 if CheckBox1.Checked then
    EdgeDetector.RenderEdges(rci);
end;

procedure TForm1.Button1Click(Sender: TObject);
var i:Integer;
begin
  TrackBar1.Position := 0;
  TrackBar1.Enabled := False;
  for i := 110 to 120 do
     VerletWorld.Nodes.Items[i].NailedDown := False;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var i:Integer;
begin
 for i := 0 to Pred(VerletWorld.Nodes.Count) do
  begin
     VerletWorld.Nodes.Items[i].Radius := TrackBar1.Position;
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
 GLTree1.Visible := CheckBox2.Checked;
 Proxy.Visible := CheckBox2.Checked;
end;

end.
