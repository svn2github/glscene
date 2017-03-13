unit fBuoyancy;
{
  ********************************************************
  Buoyancy Demo, by Mattias Fagerlund
    ( mattias@cambrianlabs.com)

  See http://www.cambrianlabs.com/Mattias/DelphiODE/BuoyancyParticles.htm
    for details on this demo.

}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg,
   
  GLScene, GLObjects, GLCadencer, GLWin32Viewer, ODEGL,
  ODEImport, GLTexture, GLzBuffer, GLShadowPlane, uBobbyClasses,
  GLMirror, GLVectorTypes, GLVectorGeometry, {GR32_Image,}
  GLVectorFileObjects, GLGeomObjects, GLShadowVolume, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

const
  GRAVITY = 9.81;	// the global gravity to use

type
  TfrmBuoyancy = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    DC_MirrorAndReflect: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    OceanFloor: TGLPlane;
    OceanLevel: TGLPlane;
    Label_FPS: TLabel;
    Timer1: TTimer;
    ScrollBar_WaterDensity: TScrollBar;
    Label_Density: TLabel;
    Label_Submerged: TLabel;
    ScrollBar_Drag: TScrollBar;
    Label_DragConstant: TLabel;
    Button_Lift: TButton;
    GLMaterialLibrary1: TGLMaterialLibrary;
    ScrollBar_TiltOcean: TScrollBar;
    Label1: TLabel;
    CheckBox_StormySeas: TCheckBox;
    Label_Comment: TLabel;
    Button_LoadBobbyFile: TButton;
    OpenDialog_BobbyFile: TOpenDialog;
    Button_Clear: TButton;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    Button_RainCubes: TButton;
    CheckBox_StepFast: TCheckBox;
    GLShadowVolume1: TGLShadowVolume;
    CheckBox_Shadows: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure ScrollBar_WaterDensityChange(Sender: TObject);
    procedure ScrollBar_DragChange(Sender: TObject);
    procedure Button_LiftClick(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ScrollBar_TiltOceanChange(Sender: TObject);
    procedure Button_LoadBobbyFileClick(Sender: TObject);
    procedure CheckBox_StormySeasClick(Sender: TObject);
    procedure Button_ClearClick(Sender: TObject);
    procedure Button_RainCubesClick(Sender: TObject);
    procedure CheckBox_ShadowsClick(Sender: TObject);
  private
     
  public
     

    // dynamics and collision objects
    world : PdxWorld;
    space : PdxSpace;
    body : PdxBody;
    contactgroup : TdJointGroupID;
    ground : PdxGeom;

    PhysicsTime : single;

    BobbyHandler : TBobbyHandler;

    GeomList : TGeomList;
    BodyList : TBodyList;

    procedure LoadBobbyFile(FileName : string; px, py, pz : single);
    procedure SetupOceanFloor(Depth : single);

    procedure GetDepthAndNormal(Bobby : TBobby);
  end;

var
  frmBuoyancy: TfrmBuoyancy;

implementation

uses StrFunctions;

{$R *.dfm}

procedure TfrmBuoyancy.FormCreate(Sender: TObject);
begin
  if FileExists('Baller.jpg') then
    GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('Baller.jpg')
  else
    GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('..\Media\Baller.jpg');

  // create world
  world := dWorldCreate();
  dWorldSetQuickStepNumIterations(world, 3);
  space := dHashSpaceCreate(nil);
  dWorldSetGravity (world,0,0,-GRAVITY);

  contactgroup := dJointGroupCreate (0);
  ground := CreateODEPlaneFromGLPlane(OceanFloor, space);

  BobbyHandler := TBobbyHandler.Create;
  BobbyHandler.OnGetDepthAndNormal := GetDepthAndNormal;
  BobbyHandler.Gravity := world.gravity;

  GeomList := TGeomList.Create;
  BodyList := TBodyList.Create;

  CreateGeomFromCube(GLCube1, Space);
  CreateGeomFromCube(GLCube2, Space);
  CreateGeomFromCube(GLCube3, Space);
  CreateGeomFromCube(GLCube4, Space);

  Button_RainCubes.Click;

  ScrollBar_WaterDensityChange(nil);
  ScrollBar_DragChange(nil);
end;

procedure TfrmBuoyancy.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;

  Application.ProcessMessages;

  dJointGroupDestroy (contactgroup);
  dSpaceDestroy (space);
  dWorldDestroy (world);
end;

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
var
  i : integer;
  b1, b2 : PdxBody;
  numc : integer;
  contact : array[0..2] of TdContact;
  c : TdJointID;
begin
  // exit without doing anything if the two bodies are connected by a joint
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (assigned(b1) and assigned(b2) and (dAreConnected (b1,b2)<>0)) then
    exit;//}

  for i :=0 to 2 do
  begin
    contact[i].surface.mode := dContactBounce or dContactApprox1; //dContactMu2;

    // This determines friction, play around with it!
    contact[i].surface.mu := 1000; //dInfinity; SHOULD BE INFINITY!
    contact[i].surface.mu2 := 0;
    contact[i].surface.bounce := 0.15;
    contact[i].surface.bounce_vel := 0.1;
  end;

  numc := dCollide (o1,o2,3,contact[0].geom,sizeof(TdContact));
  if (numc>0) then
  begin
    // dMatrix3 RI;
    // dRSetIdentity (RI);
    // const dReal ss[3] = {0.02,0.02,0.02};
    for i := 0 to numc-1 do
    begin
      c := dJointCreateContact (frmBuoyancy.world,frmBuoyancy.contactgroup, @contact[i]);
      dJointAttach (c,b1,b2);
      // dsDrawBox (contact[i].geom.pos,RI,ss);
    end;
  end;
end;

const
  cDELTA_TIME = 1/100;
procedure TfrmBuoyancy.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

  procedure WorldStep;
  begin
    dSpaceCollide (space,nil,nearCallback);

    // Calculate and apply bobby forces!
    BobbyHandler.UpdateBobbies(cDELTA_TIME);

    if CheckBox_StepFast.Checked then
      dWorldQuickStep (world,cDELTA_TIME)
    else
      dWorldStep (world,cDELTA_TIME);

    // remove all contact joints
    dJointGroupEmpty (contactgroup);
  end;
begin
  if CheckBox_StormySeas.Checked then
  begin
    OceanLevel.PitchAngle := (sin(newTime*2))*15;
    OceanLevel.TurnAngle := (sin(newTime*3))*5;
  end;

  while newTime>PhysicsTime do
  begin
    WorldStep;
    PhysicsTime := PhysicsTime + cDELTA_TIME;
  end;

  RenderGeomList(GeomList);

  //Caption := Format('(%f, %f, %f)',[GLSphere1.Position.X, GLSphere1.Position.Y, GLSphere1.Position.Z]);
  if BobbyHandler.Bobbies.Count>0 then
  begin
    //Caption := Format('Water depth = %f',[BobbyHandler.Bobbies[0].CenterDepth]);
  end;
end;

const
  cGPB = 100;

procedure TfrmBuoyancy.LoadBobbyFile(FileName : string; px, py, pz : single);
var
  i : integer;
  TempGeomList : TGeomList;
  BobbyList : TBobbyList;
  TotalMass : TdMass;
  dpos : array[0..cGPB-1, 0..2] of TdReal;
  StringList : TStringList;
  c : char;
  MyCube : TGLCube;
  mx, my, mz : single;
  LastGeom : PdxGeom;

  procedure AddBobby(Body : PdxBody; x,y,z,r : single);
  var
    Bobby : TBobby;
  begin
    // Setup the bobby
    Bobby := TBobby.Create(BobbyHandler);
    Bobby.Body := Body;
    Bobby.Radius := r; 
    Bobby.Position[0] := x;
    Bobby.Position[1] := y;
    Bobby.Position[2] := z;
    BobbyList.Add(Bobby);//}

    {GLSphere := TGLSphere(MyCube.AddNewChild(TGLSphere));
    GLSphere.Position.x := x;
    GLSphere.Position.y := y;
    GLSphere.Position.z := z;
    with GLSphere do
    begin
      Material.MaterialLibrary := GLMaterialLibrary1;
      Radius := r*1.3;
      Stacks := 8;
      Slices := 8;
    end;//}
  end;

  procedure BobbyGrid(Body : PdxBody; x,y,z, sizex, sizey, sizez : single; Res : integer);
  var
    r : single;
    gx, gy, gz : integer;
  begin
    // Calculate a sphere radius that will give the same volume as the cube
    r := power(sizex*sizey*sizez*3/(pi * res * res * res * 4), 1/3);

    // Reduce size of box so that boddies don't stick out
    Sizex := Sizex-2*r;
    Sizey := Sizey-2*r;
    Sizez := Sizez-2*r;

    //Assert(sizex*sizey*sizez = res * res * res * r * r * r * pi * 4 / 3);

    for gx := 0 to res-1 do
      for gy := 0 to res-1 do
        for gz := 0 to res-1 do
        begin
          mx := x + (gx)/(res-1)*(sizex) - sizex/2;
          my := y + (gy)/(res-1)*(sizey) - sizey/2;
          mz := z + (gz)/(res-1)*(sizez) - sizez/2;//}

          AddBobby(Body, mx, my, mz, r);

          {GLSphere := TGLSphere(MyCube.AddNewChild(TGLSphere));
          GLSphere.Position.x := mx;
          GLSphere.Position.y := my;
          GLSphere.Position.z := mz;
          with GLSphere do
          begin
            Material.MaterialLibrary := GLMaterialLibrary1;
            Radius := r*1.5;
            Stacks := 8;
            Slices := 8;
          end;//}
        end;
  end;

  procedure AddGeomSphere(Body : PdxBody; x,y,z,r : single; Density : single=1);
  var
    SphereMass : TdMass;
    Transform, Sphere : PdxGeom;
    GLSphere : TGLSphere;
  begin
    // Create the transform
    Transform := dCreateGeomTransform (space);
    LastGeom := Transform;
    dGeomSetBody(Transform, Body);
    GeomList.Add(Transform);
    dGeomTransformSetCleanup (Transform,1);

    // Adding the transform will suffice, if we add the Sphere geom things will
    // break down
    dpos[TempGeomList.Count, 0] := x;
    dpos[TempGeomList.Count, 1] := y;
    dpos[TempGeomList.Count, 2] := z;

    // Create
    Sphere := dCreateSphere (nil, r); TempGeomList.Add(Sphere);
    dMassSetSphere (SphereMass,Density,r);

    dGeomTransformSetGeom (Transform, Sphere);

    dGeomSetPosition(Sphere, x,y,z);
    dMassTranslate(SphereMass, x,y,z);

    // Create the sphere
    GLSphere := TGLSphere(DC_MirrorAndReflect.AddNewChild(TGLSphere));
    with GLSphere do
    begin
      Material.MaterialLibrary := GLMaterialLibrary1;
      Material.LibMaterialName := 'Ballmaterial';
      Radius := r;
      Stacks := 8;
      Slices := 8;
    end;
    Transform.Data := GLSphere;

    GLShadowVolume1.Occluders.AddCaster(GLSphere);
    //PositionSceneObject(GLSphere, Transform);

    // add to the total mass
    dMassAdd (TotalMass,SphereMass);
  end;

  procedure SphereGrid(Body : PdxBody; x,y,z, sizex, sizey, sizez : single; Res : integer);
  var
    r : single;
    gx, gy, gz : integer;
  begin
    // Calculate a sphere radius that will give the same volume as the cube
    r := power(sizex*sizey*sizez*3/(pi * res * res * res * 4), 1/3);

    //Assert(sizex*sizey*sizez = res * res * res * r * r * r * pi * 4 / 3);

    // Reduce size of box so that boddies don't stick out
    Sizex := Sizex-2*r;
    Sizey := Sizey-2*r;
    Sizez := Sizez-2*r;

    for gx := 0 to res-1 do
      for gy := 0 to res-1 do
        for gz := 0 to res-1 do
        begin
          mx := x + (gx)/(res-1)*(sizex) - sizex/2;
          my := y + (gy)/(res-1)*(sizey) - sizey/2;
          mz := z + (gz)/(res-1)*(sizez) - sizez/2;//}

          AddGeomSphere(Body, mx, my, mz, r);
          AddBobby(Body, mx, my, mz, r);
        end;
  end;

  procedure AddGeomBox(Body : PdxBody; x,y,z, sizex, sizey, sizez : single);
  var
    BoxMass : TdMass;
    Transform, Box : PdxGeom;
    GLCube : TGLCube;
  begin
    // Create the transform
    Transform := dCreateGeomTransform (space);
    LastGeom := Transform;

    dGeomSetBody(Transform, Body);
    GeomList.Add(Transform);
    dGeomTransformSetCleanup (Transform,1);

    // Adding the transform will suffice, if we add the Sphere geom things will
    // break down
    dpos[TempGeomList.Count, 0] := x;
    dpos[TempGeomList.Count, 1] := y;
    dpos[TempGeomList.Count, 2] := z;

    // Create
    Box := dCreateBox (nil, sizex, sizey, sizez); TempGeomList.Add(Box);
    dMassSetBox (BoxMass,1,sizex, sizey, sizez);

    dGeomTransformSetGeom (Transform, Box);

    dGeomSetPosition(Box, x,y,z);
    dMassTranslate(BoxMass, x,y,z);

    // Create the sphere
    GLCube := TGLCube(DC_MirrorAndReflect.AddNewChild(TGLCube));
    with GLCube do
    begin
      Material.MaterialLibrary := GLMaterialLibrary1;
      Material.LibMaterialName := 'Ballmaterial';
      CubeWidth:=sizex;
      CubeHeight:=sizey;
      CubeDepth:=sizez;
    end;
    MyCube := GLCube;
    Transform.Data := GLCube;

    GLShadowVolume1.Occluders.AddCaster(GLCube);
    //PositionSceneObject(GLSphere, Transform);

    // add to the total mass
    dMassAdd (TotalMass,BoxMass);//}
  end;

  procedure ParseBobbyFile;
  var
    i : integer;
    s : string;
    command, args : string;
    FreeForm : TGLFreeForm;
  begin
    for i := 0 to StringList.Count-1 do
    begin
      s := Trim(StringList[i]);

      if (Copy(s,1,2)='//') or (s='') then
        continue;

      Command := GetBefore(' ', s+' ');
      Args := GetAfter(' ', s)+',';

      if SameText('GeomSphere', Command) then
      begin
        // GeomSphere x, y, z, radius, Bobby(0=false, 1=true), Density

        if GetNTh(',', Args, 5)<>'' then
          AddGeomSphere(Body, GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2), GetNThAsSingle(',', Args, 3), GetNThAsSingle(',', Args, 5))
        else
          AddGeomSphere(Body, GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2), GetNThAsSingle(',', Args, 3));

        if (Trim(GetNTh(',', Args, 4))='1') then
          AddBobby(Body, GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2), GetNThAsSingle(',', Args, 3));
      end else
      if SameText('GeomBox', Command) then
      begin
        // GeomBox x,y,z, sizex, sizey, sizez, BobbyResolution (0= no bobbies)
        AddGeomBox(Body,
          GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2),
          GetNThAsSingle(',', Args, 3), GetNThAsSingle(',', Args, 4), GetNThAsSingle(',', Args, 5));

        if (Trim(GetNTh(',', Args, 6))<>'0') then
        begin
          // Create a grid of spheres
          BobbyGrid(
            Body,
            GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2),
            GetNThAsSingle(',', Args, 3), GetNThAsSingle(',', Args, 4), GetNThAsSingle(',', Args, 5),
            StrToInt(GetNTh(',', Args, 6)));
        end;
      end else
      if SameText('SphereGrid', Command) then
      begin
        // SphereGrid  x,y,z, sizex, sizey, sizez, BobbyResolution (0= no bobbies)
        // Create a grid of spheres
        SphereGrid(
          Body,
          GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2),
          GetNThAsSingle(',', Args, 3), GetNThAsSingle(',', Args, 4), GetNThAsSingle(',', Args, 5),
          StrToInt(GetNTh(',', Args, 6)));
      end else
      if SameText('Bobby', Command) then
      begin
        // Bobby x, y, z, radius
        AddBobby(Body, GetNThAsSingle(',', Args, 0), GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2), GetNThAsSingle(',', Args, 3));
      end else
      if SameText('Mesh', Command) then
      begin
        FreeForm := TGLFreeForm(DC_MirrorAndReflect.AddNewChild(TGLFreeForm));

        FreeForm.LoadFromFile(GetBetween('''', GetNTh(',', Args, 0)));
        FreeForm.Scale.SetVector(GetNThAsSingle(',', Args, 1), GetNThAsSingle(',', Args, 2), GetNThAsSingle(',', Args, 3), 0);
        FreeForm.Up.SetVector(0,0,1);
        FreeForm.TurnAngle := 90;

        LastGeom.data := FreeForm;
      end else
      if SameText('WaterDepth', Command) then
      begin
        SetupOceanFloor(GetNThAsSingle(',', Args, 0));
      end else
      if SameText('Comment', Command) then
      begin
        Label_Comment.Caption := Args
      end else
        Assert(false, Format('Command %s not recogniezed!',[Command]));
    end;
  end;
begin
  // Prepare the mass sum
  dMassSetZero (TotalMass);

  // Create the body for this creation
  body := dBodyCreate (world);
  BodyList.Add(Body);

  // Create a temporay storage for all our geoms
  TempGeomList := TGeomList.Create;

  c := FormatSettings.DecimalSeparator;

  Label_Comment.Caption := 'No comment for this file';

  SetupOceanFloor(-1);

  try
    FormatSettings.DecimalSeparator := '.';

    // Bobbyfile
    StringList := TStringList.Create;
    StringList.LoadFromFile(FileName);

    // Keep track of the bobbies we create
    BobbyList := TBobbyList.Create;

    // Parse the file
    ParseBobbyFile;

    // Now update all geoms (and bobbies) so that the gravity center is 0,0,0
    for i := 0 to TempGeomList.Count-1 do
    begin
      dGeomSetPosition (TempGeomList[i],
        dpos[i,0]-TotalMass.c[0],
        dpos[i,1]-TotalMass.c[1],
        dpos[i,2]-TotalMass.c[2]);
    end;//}

    for i := 0 to BobbyList.Count-1 do
    begin
      BobbyList[i].Position[0] := BobbyList[i].Position[0] - TotalMass.c[0];
      BobbyList[i].Position[1] := BobbyList[i].Position[1] - TotalMass.c[1];
      BobbyList[i].Position[2] := BobbyList[i].Position[2] - TotalMass.c[2];
    end;//}

    // Center the mass
    dMassTranslate (TotalMass,-TotalMass.c[0],-TotalMass.c[1],-TotalMass.c[2]);

    // Assign mass to body
    dBodySetMass (Body, @TotalMass);

    // Move the body into position
    dBodySetPosition(Body, px, py, pz);
  finally
    FormatSettings.DecimalSeparator := c;
    FreeAndNil(TempGeomList);
    FreeAndNil(BobbyList);
    FreeAndNil(StringList);
  end;
end;

procedure TfrmBuoyancy.GetDepthAndNormal(Bobby: TBobby);
var
  Up : TVector4f;
begin
  //Bobby.CenterDepth := OceanLevel.PointDistance(ConvertdVector3ToVector4f(Bobby.WorldPosition));
  //Bobby.CenterDepth := OceanLevel.PointDistance(GLSphere1.Position.AsVector);

  Up := OceanLevel.AbsoluteDirection;

  // If the bobby is outside the ocean, then the depth is negative!
  if (abs(Bobby.WorldPosition[0])>OceanLevel.Width/2) or
     (abs(Bobby.WorldPosition[1])>OceanLevel.Width/2) then
  begin
    Bobby.CenterDepth := 10e5;
  end else
  begin
    Bobby.CenterDepth :=
      PointPlaneDistance(
      ConvertdVector3ToVector4f(Bobby.WorldPosition),
      OceanLevel.AbsolutePosition,
      Up);//}

    Bobby.WaterNormal := ConvertVector4fTodVector3(Up);
  end;
  //Bobby.CenterDepth := Bobby.WorldPosition[2]-OceanLevel.Position.z;
end;

procedure TfrmBuoyancy.Timer1Timer(Sender: TObject);
begin
  Label_Submerged.Caption := Format('%f%% submerged',[BobbyHandler.GetSubmergedAmount * 100]);

  Label_FPS.Caption := Format('%d Bodies, %d geoms, %d bobbies, %f fps',[world.nb, dSpaceGetNumGeoms (Space), BobbyHandler.Bobbies.Count, GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmBuoyancy.ScrollBar_WaterDensityChange(Sender: TObject);
begin
  BobbyHandler.LiquidDensity := ScrollBar_WaterDensity.Position/100;
  Label_Density.Caption := Format('Water density : %f',[BobbyHandler.LiquidDensity]);
end;

procedure TfrmBuoyancy.ScrollBar_DragChange(Sender: TObject);
var
  i : integer;
begin
  if BobbyHandler.Bobbies.Count>0 then
  begin
    for i := 0 to BobbyHandler.Bobbies.Count - 1 do
      BobbyHandler.Bobbies[i].DragCoefficient := ScrollBar_Drag.Position / 100;
    Label_DragConstant.Caption := Format('Drag coeff : %f',[BobbyHandler.Bobbies[0].DragCoefficient]);
  end;
end;

procedure TfrmBuoyancy.Button_LiftClick(Sender: TObject);
var
  c : single;
begin
  c := 15;
  dBodySetPosition(Body, 0, 0, 5);
  dBodySetLinearVel(Body, 0, 0, 0);
  dBodySetAngularVel(Body, c * (random-0.5), c * (random-0.5), c * (random-0.5));
end;

var
  FoldMouseX : integer;
  FoldMouseY : integer;
procedure TfrmBuoyancy.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(FoldMouseY-Y, FoldMouseX-X);

  FoldMouseX := X;
  FoldMouseY := Y;
end;

procedure TfrmBuoyancy.ScrollBar_TiltOceanChange(Sender: TObject);
begin
  OceanLevel.PitchAngle := ScrollBar_TiltOcean.Position;
end;

procedure TfrmBuoyancy.Button_LoadBobbyFileClick(Sender: TObject);
begin
  OpenDialog_BobbyFile.FileName := '*.bob';
  if OpenDialog_BobbyFile.Execute then
    LoadBobbyFile(OpenDialog_BobbyFile.FileName, 0,0,5 );
end;

procedure TfrmBuoyancy.SetupOceanFloor(Depth: single);
begin
  OceanFloor.Position.Z := Depth;

  if Ground<>nil then
    dGeomDestroy(ground);

  ground := CreateODEPlaneFromGLPlane(OceanFloor, space);
end;

procedure TfrmBuoyancy.CheckBox_StormySeasClick(Sender: TObject);
begin
  if CheckBox_StormySeas.Checked = false then
    OceanLevel.PitchAngle := 0;
end;

procedure TfrmBuoyancy.Button_ClearClick(Sender: TObject);
begin
  BobbyHandler.ClearBobbies;

{  for i := 0 to GeomList.Count-1 do
    GLShadowVolume1.Occluders.RemoveCaster(TGLBaseSceneObject(dGeomGetData(GeomList[i])));//}

  GeomList.DeleteAllGeoms(true);
  BodyList.DeleteAllBodies;
end;

procedure TfrmBuoyancy.Button_RainCubesClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to 20 do
    LoadBobbyFile('Grid2.bob', 0,0,15+i*3);
end;

procedure TfrmBuoyancy.CheckBox_ShadowsClick(Sender: TObject);
begin
  if CheckBox_Shadows.Checked then
    GLShadowVolume1.Mode := svmDarkening
  else
    GLShadowVolume1.Mode := svmOff;
end;
(*
Hi Ted here is the code so far.

void BoxGeom::ApplyHydrodynamicForces(dReal linear_viscosity, dReal
angular_viscosity, dReal density, Vector3 flow)
    {
        const dReal *lvel = dBodyGetLinearVel(this->_body->Id);
        const dReal *avel = dBodyGetAngularVel(this->_body->Id);
        const dReal *R = dBodyGetRotation(this->_body->Id);

        dVector3 compFlow;
        compFlow[0] = lvel[0] - flow.x;
        compFlow[1] = lvel[1] - flow.y;
        compFlow[2] = lvel[2] - flow.z;

        dReal ss[3];
        dGeomBoxGetLengths(this->_id, ss);

        dReal AreaX = ss[1] * ss[2];
        dReal AreaY = ss[0] * ss[2];
        dReal AreaZ = ss[0] * ss[1];

        dReal nx = (R[0] * compFlow[0] + R[4] * compFlow[1] + R[8] * 
compFlow[2]) * AreaX;
        dReal ny = (R[1] * compFlow[0] + R[5] * compFlow[1] + R[9] * 
compFlow[2]) * AreaY;
        dReal nz = (R[2] * compFlow[0] + R[6] * compFlow[1] + R[10] * 
compFlow[2]) * AreaZ;

        dReal temp = -nx*linear_viscosity;
        dBodyAddForce(this->_body->Id, temp*R[0], temp*R[4], temp*R[8]);

        temp = -ny*linear_viscosity;
        dBodyAddForce(this->_body->Id, temp*R[1], temp*R[5], temp*R[9]);

        temp = -nz*linear_viscosity;
        dBodyAddForce(this->_body->Id, temp*R[2], temp*R[6], temp*R[10]);

        nx = (R[0] * avel[0] + R[4] * avel[1] + R[8] * avel[2]) * (AreaY + 
AreaZ);
        ny = (R[1] * avel[0] + R[5] * avel[1] + R[9] * avel[2]) * (AreaX + 
AreaZ);
        nz = (R[2] * avel[0] + R[6] * avel[1] + R[10] * avel[2]) * (AreaX + 
AreaY);

        temp = -nx * angular_viscosity;
        dBodyAddTorque(this->_body->Id, temp * R[0], temp * R[4], temp * 
R[8]);

        temp = -ny * angular_viscosity;
        dBodyAddTorque(this->_body->Id, temp * R[1], temp * R[5], temp * 
R[9]);

        temp = -nz * angular_viscosity;
        dBodyAddTorque(this->_body->Id, temp * R[2], temp * R[6], temp * 
R[10]);

        dReal gravity[3];
        dWorldGetGravity(this->_body->get_WorldId(), gravity);

        temp = -density * ss[0] * ss[1] * ss[2];
        dBodyAddForce(this->_body->Id, temp*gravity[0], temp*gravity[1], 
temp*gravity[2]);

        //which, unless you have a non-axis-aligned gravity, is probably 
just:
        //dBodyAddForce(body, 0, temp*gravity[1], 0);

    }


Ander
*)
end.

