{ Cloned from Stuarts ClickMove,
  Built ON the GLScene Basic terrain rendering demo.
  Much deleted to Simplify : See Originals for Comments, etc...
  The terrain HeightData is provided by a TGLBitmapHDS
  (HDS stands for "Height Data Source"),
  and displayed by a TGLTerrainRenderer.

  Controls:<ul>
  [Ctrl] Right Click a Point to move the little guy there
  <li>Direction keys move the camera around (shift to speedup)
  <li>PageUp/PageDown move the camera up and down (Vertically)
  <li>Orient the camera freely by holding down the left button
  <li>Toggle wireframe mode with 'w'
  <li>Increase/decrease the viewing distance with '+'/'-'.
  <li>Increase/decrease CLOD precision with '*' and '/'.
  <li>Increase/decrease QualityDistance with '9' and '8'. }

unit ATerrainFrm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.jpeg,

  GLScene,
  GLState,
  GLTerrainRenderer,
  GLObjects,
  GLHeightData,
  GLCadencer,
  GLTexture,
  GLHUDObjects,
  GLBitmapFont, // HudText
  GLSkydome,
  GLWin32Viewer, GLVectorGeometry,
  GLCrossPlatform, // PrecisionTimer
  GLVectorFileObjects,
  GLFileMD2,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses;

type
  TWayPoint = record
    Position: TAffineVector;
    Active: Boolean;
  end;

  TPathPoint = record
    Position: TAffineVector;
    Active: Boolean;
  end;

  TATerrainForm = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    CameraCube: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    SkyDome1: TGLSkyDome;
    Marker: TGLDummyCube;
    ArrowLine1: TGLArrowLine;
    Torus1: TGLTorus;
    ActorBase0: TGLDummyCube;
    Actor0: TGLActor;
    BitmapFont1: TGLBitmapFont;
    HUDText1: TGLHUDText;
    StatusBar1: TStatusBar;
    ActorsxDC: TGLDummyCube;
    ActorBasex1: TGLDummyCube;
    ActorBasex2: TGLDummyCube;
    ActorBasex3: TGLDummyCube;
    ActorBasex4: TGLDummyCube;
    ActorBasex5: TGLDummyCube;
    ActorBasex6: TGLDummyCube;
    GLActor1: TGLActor;
    Targetx1: TGLCube;
    Targetx2: TGLCube;
    GLActor2: TGLActor;
    Targetx3: TGLCube;
    GLActor3: TGLActor;
    Targetx4: TGLCube;
    GLActor4: TGLActor;
    Targetx5: TGLCube;
    GLActor5: TGLActor;
    Targetx6: TGLCube;
    GLActor6: TGLActor;
    ActorsDC: TGLDummyCube;
    BaseDC: TGLDummyCube;
    BunkerDC: TGLDummyCube;
    PathLines: TGLLines;
    PathDC: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure DoActorMovement;
    procedure DoScenario1;
    procedure DoScenario2;
    procedure DoScenario3;
    procedure DoScenario4;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure CreatePathLines(PathUnit: Integer);
    procedure DoSavePath;
  private
    { Déclarations privées }
    // fullScreen,
    Testing: Boolean;
    SomeMagicNumber, Scenario, mx, my: Integer;
    theta: single;
    FCamHeight: single;
    WayPoint: TWayPoint;
    PathPoint: TPathPoint;
    currentanim: string;
  public
    { Déclarations publiques }

  end;

var
  ATerrainForm: TATerrainForm;

implementation

{$R *.DFM}

uses GLKeyboard, OpenGL1x, GLVectorTypes,
  AStarBlitzCode, AStarBlitzCodeH, AStarBlitzUnit,
  // AProjectOptionsFrm
  AStarGlobals;

procedure TATerrainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  ActorsDC.DeleteChildren;
  BunkerDC.DeleteChildren;
  BaseDC.DeleteChildren;
  PathLines.Visible := False;
  PathLines.Nodes.Clear;
  // Setlength the Arrays
  SetLength(island, 0);
  SetLength(claimedNode, 0);
  SetLength(nearByPath, 0);
  SetLength(tempUnwalkability, 0);
  SetLength(MapAttribute, 0);

  SetLength(UnitpathBank[1], 0);
  SetLength(UnitpathBank[2], 0);
  EndPathfinder;
end;

procedure TATerrainForm.FormCreate(Sender: TObject);
begin
  If ProjectFilename = '' then
    Testing := True
  else
    Testing := False;
  Scenario := 1;
  gGameTime := 0;
  ActiveUnitNumber := 1;
end;

procedure TATerrainForm.FormShow(Sender: TObject);
var
  F: File of Integer;
  min, max: TAffineVector;
  ActorInstance: TGLActor;
  // CubeInstance:TGLCube;
  CubeInstance: TGLCubeEX; // in AStarBlitzUnit to make Cubes be Texture Wrapped
  DCDummy: TGLDummyCube;
  X, Y, z: Integer;
begin // 'F:\vrgisAStar\Projects'
  SetCurrentDir(ProjectDirectory); // So things get found
  // 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;
  // Could've been done at design time, but it the, it hurts the eyes ;)
  GLSceneViewer1.Buffer.BackgroundColor := clWhite;
  StatusBar1.Panels[6].Text := 'Loading';
  Application.ProcessMessages;
  // Load ProjectFilename
  If Testing then
  begin
    // specify height map data
    GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
    // load the texture maps
    GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
      ('snow512.jpg');
    GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
      ('detailmap512.jpg');
    GLBitmapHDS1.InfiniteWrap := True; // False;
    // apply texture map scale (Texture 1024 / our heightmap size is 256)
    TerrainRenderer1.TilesPerTexture := 256 / 32;
    // 256/TerrainRenderer1.TileSize;
    // 256/32
    // Move camera starting point to an interesting hand-picked location
    // CameraCube.Position.X:=570;
    // CameraCube.Position.Z:=-385;
    CameraCube.Turn(90);
    // Initial camera height offset (controled with pageUp/pageDown)
    FCamHeight := 10;
  end
  else
  begin
    // showmessage('Step 1');
    StatusBar1.Panels[6].Text := 'Loading  File';
    Application.ProcessMessages;
    LoadProjectFile(ProjectFilename);
    If ((Length(ProjectRecord.MAGValuesFile) > 0) and
      (ProjectRecord.MAGValuesFile <> 'NoName')) then
      if FileExists(ProjectRecord.MAGValuesFile) then
        LoadMAGValues
      else
        ResetMAGDefaults;

    // showmessage('Step 2');
    // mapWidth:= ImageWidth div tileSize;
    // mapHeight:= ImageHeight div tileSize;
    mapWidth := ProjectRecord.mapWidth;
    mapHeight := ProjectRecord.mapHeight;
    InitializePathfinder;
    SetLength(island, ProjectRecord.mapWidth + 1, ProjectRecord.mapHeight + 1);
    SetLength(claimedNode, ProjectRecord.mapWidth + 1,
      ProjectRecord.mapHeight + 1);
    SetLength(nearByPath, ProjectRecord.mapWidth + 1,
      ProjectRecord.mapHeight + 1);
    SetLength(tempUnwalkability, ProjectRecord.mapWidth + 1,
      ProjectRecord.mapHeight + 1);
    SetLength(MapAttribute, ProjectRecord.mapWidth + 1,
      ProjectRecord.mapHeight + 1);

    SetLength(UnitpathBank[1], ProjectRecord.NumberofActiveUnits + 1 +
      ProjectRecord.NumberofActiveEnemyPatrol);
    SetLength(UnitpathBank[2], ProjectRecord.NumberofActiveUnits + 1 +
      ProjectRecord.NumberofActiveEnemyPatrol);

    // GCostFileName  FileAgfEdit//GCost  LoadGCostValues
    // SetLength(GCostData,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);
    // G Cost loaded by LoadProjectFile This adds Slope to it
    If FileExists(ProjectRecord.ElevationFile) then
    begin // Slope
      // showmessage('Step 3 '+AProjectOptionsForm.FileAefEdit.Text);
      AssignFile(F, ProjectRecord.ElevationFile);
      Reset(F);
      Read(F, z);
      If (z <> ProjectRecord.mapWidth) then;
      Read(F, z);
      If (z <> ProjectRecord.mapHeight) then;
      for Y := 0 to ProjectRecord.mapHeight - 1 do
        for X := 0 to ProjectRecord.mapWidth - 1 do
        begin
          Read(F, z);
          GCostData[X, Y] := GCostData[X, Y] + z;
        end;
      CloseFile(F);
    end
    else
      showmessage('Step 3 No Slope.aef');
    // Done so  walkability gets Set even if NO Slopes
    for Y := 0 to ProjectRecord.mapHeight - 1 do
      for X := 0 to ProjectRecord.mapWidth - 1 do
      begin
        If GCostData[X, Y] > ProjectRecord.TerrainMagNoGoD then
          walkability[X][Y] := unwalkable
        else
          walkability[X][Y] := walkable;
      end;
    // IdentifyIslands is done when data changes ONLY
    // () ;update islands() array.
    // If ProjectRecord.ProcessNoGoIslands then IdentifyIslands;

    StatusBar1.Panels[6].Text := 'Loading Height';
    Application.ProcessMessages;
    // specify height map data
    // showmessage('Step 4 '+AProjectOptionsForm.OpenHeightDataEdit.Text);
    GLBitmapHDS1.Picture.LoadFromFile(ProjectRecord.HeightDataFile);
    // load the texture maps
    // showmessage('Step 5 '+AProjectOptionsForm.FileImageTextureEdit.Text);
    GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
      (ExtractFileName(ProjectRecord.ImageTextureFile));
    GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
      ('detailmap512.jpg');
    GLBitmapHDS1.InfiniteWrap := False;
    // True;
    // TerrainRenderer1.TileSize is 32
    // apply texture map scale (Texture 512 / our heightmap size is 256)
    // 256/32=8 ? 8x512=4096  ? 3D is 1024
    TerrainRenderer1.TilesPerTexture := ProjectRecord.HeightfieldSize / 32;
    // Move camera starting point to an interesting hand-picked location
    CameraCube.Position.X := -50;
    CameraCube.Position.z := 50; // Y is 8
    CameraCube.Turn(90); //
    // CameraCube is the Target of the camera
    // GLCamera1.Position.X=5, Y=10,
    GLCamera1.Position.z := -25; // 25
    // Initial camera height offset (controled with pageUp/pageDown)
    FCamHeight := 10;
  end;

  StatusBar1.Panels[6].Text := 'Loading Actors';
  Application.ProcessMessages;
  SomeMagicNumber := 2;
  If Testing then
  begin
    // Load Actor into GLScene    ExtractFilePath(ParamStr(0))+'Actors\'
    // UnitRecordArray[6].Md2Name  UnitRecordArray[6].Md2TextureName
    Actor0.LoadFromFile(ExtractFilePath(ParamStr(0)) +
      'Actors\waste\waste.MD2'); // waste.md2  waste.jpg
    Actor0.Material.Texture.Image.LoadFromFile(ExtractFilePath(ParamStr(0)) +
      'Actors\waste\waste.jpg');
    Actor0.Animations.LoadFromFile(ExtractFilePath(ParamStr(0)) +
      'Actors\Quake2Animations.aaf');

    // Scale Actor for put in the Scene
    Actor0.Scale.SetVector(0.04, 0.04, 0.04, 0);
    Actor0.GetExtents(min, max);
    ScaleVector(min, Actor0.Scale.Y);
    Actor0.Position.AsAffineVector := min;
    Actor0.Position.X := 0;
    Actor0.Position.Y := -Actor0.Position.Y + 0.1;
    Actor0.Position.z := 0;
    Actor0.SwitchToAnimation('stand', True);
    Actor0.AnimationMode := aamLoop;
  end
  else
  begin // Need to Create Actors and  ActorBase1
    // showmessage('Step Actors # of '+AProjectOptionsForm.UnitsActiveEdit.Text);
    for X := 1 to (ProjectRecord.NumberofActiveUnits { NumberofUnits } -
      ProjectRecord.NumberofActiveEnemyPatrol) do
    begin
      // showmessage('Step Actors '+inttostr(x));
      CubeInstance := TGLCubeEX(BaseDC.AddNewChild(TGLCubeEX));
      CubeInstance.Name := 'Target' + Inttostr(X);
      CubeInstance.CubeDepth := UnitRecordArray[X].TargetSize;
      CubeInstance.CubeHeight := UnitRecordArray[X].TargetSize;
      CubeInstance.CubeWidth := UnitRecordArray[X].TargetSize;
      If FileExists(UnitRecordArray[X].FlagFile) then
      begin // 384x64    6 panels 64x64                              //ExtractFileName
        CubeInstance.Material.Texture.Image.LoadFromFile
          ((UnitRecordArray[X].FlagFile));
        CubeInstance.Material.Texture.Disabled := False;
      end
      else // showmessage(UnitRecordArray[X].FlagFile);
        CubeInstance.Material.FrontProperties.Diffuse.AsWinColor :=
          TargetGoalColorArray[X];
      { CubeInstance.Direction.Y:=1;
        CubeInstance.Direction.X:=0;
        CubeInstance.Direction.Z:=0;   //1
        CubeInstance.Up.X:=1;
        CubeInstance.Up.Z:=0;
        CubeInstance.Up.Y:=0; }        // 1
      CubeInstance.Position.X := -UnitRecordArray[X].targetX * SomeMagicNumber;
      // 0;
      CubeInstance.Position.Y := 0;
      CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.5;
      CubeInstance.Position.z := UnitRecordArray[X].targetY * SomeMagicNumber;
      // 0;
      CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
        (CubeInstance.Position.AsVector) + (UnitRecordArray[X].TargetSize / 2);
      // 0.1;

      // Actor
      DCDummy := TGLDummyCube(ActorsDC.AddNewChild(TGLDummyCube));
      DCDummy.Name := 'ActorBase' + Inttostr(X);
      // Load Actor into GLScene
      ActorInstance := TGLActor(DCDummy.AddNewChild(TGLActor));
      ActorInstance.Direction.Y := 1;
      ActorInstance.Direction.X := 0;
      ActorInstance.Direction.z := 0;
      ActorInstance.Up.X := 1;
      ActorInstance.Up.z := 0;
      ActorInstance.Up.Y := 0; // ExtractFileName
      // UnitRecordArray[6].Md2Name  UnitRecordArray[6].Md2TextureName
      ActorInstance.LoadFromFile((UnitRecordArray[X].Md2Name)); // 'waste.md2'
      ActorInstance.Material.Texture.Image.LoadFromFile
        ((UnitRecordArray[X].Md2TextureName)); // 'waste.jpg'
      ActorInstance.Material.Texture.Disabled := False;
      ActorInstance.Name := 'Actor' + Inttostr(X);
      ActorInstance.Animations.LoadFromFile(ExtractFilePath(ParamStr(0)) +
        'Actors\Quake2Animations.aaf');

      // Scale Actor for put in the Scene
      ActorInstance.Scale.SetVector(UnitRecordArray[X].ActorScale,
        UnitRecordArray[X].ActorScale, UnitRecordArray[X].ActorScale, 0);
      // 0.04, 0.04, 0.04, 0);
      ActorInstance.GetExtents(min, max);
      ScaleVector(min, ActorInstance.Scale.Y);
      ActorInstance.Position.AsAffineVector := min;
      ActorInstance.Position.X := -UnitRecordArray[X].startXLoc *
        SomeMagicNumber; // 0;
      ActorInstance.Position.Y := -ActorInstance.Position.Y + 0.1;
      ActorInstance.Position.z := UnitRecordArray[X].startYLoc *
        SomeMagicNumber; // 0;
      ActorInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
        (ActorInstance.Position.AsVector) + 0.5;
      UnitRecordArray[X].CurrentpathBank := 1;
      ActorInstance.SwitchToAnimation('stand', True);
      ActorInstance.AnimationMode := aamLoop;
    End;
    // ENEMY
    If (ProjectRecord.NumberofActiveEnemyPatrol > 0) then
    begin
      If (ProjectRecord.NumberofActiveEnemy > 0) then
      begin
        for X := 1 to UnitRecordArray[6].Members do
        begin
          CubeInstance := TGLCubeEX(BunkerDC.AddNewChild(TGLCubeEX));
          CubeInstance.Name := 'EnemyBunker' + Inttostr(X);
          CubeInstance.CubeDepth := UnitRecordArray[6].TargetSize;
          CubeInstance.CubeHeight := UnitRecordArray[6].TargetSize;
          CubeInstance.CubeWidth := UnitRecordArray[6].TargetSize;
          If FileExists(UnitRecordArray[6].FlagFile) then
          begin // ExtractFileName
            CubeInstance.Material.Texture.Image.LoadFromFile
              ((UnitRecordArray[6].FlagFile));
            CubeInstance.Material.Texture.Disabled := False;
          end
          else
            CubeInstance.Material.FrontProperties.Diffuse.AsWinColor :=
              EnemyPositionsColor;
          { CubeInstance.Direction.Y:=1;
            CubeInstance.Direction.X:=0;
            CubeInstance.Direction.Z:=0;
            CubeInstance.Up.X:=1;
            CubeInstance.Up.Z:=0;
            CubeInstance.Up.Y:=0; }
          Case X of
            1:
              Begin
                CubeInstance.Position.X := -UnitRecordArray[6].Members1X *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
                CubeInstance.Position.z := UnitRecordArray[6].Members1Y *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
                  (CubeInstance.Position.AsVector) +
                  (UnitRecordArray[6].TargetSize / 2); // 0.1;
              End;
            2:
              Begin
                CubeInstance.Position.X := -UnitRecordArray[6].Members2X *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
                CubeInstance.Position.z := UnitRecordArray[6].Members2Y *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
                  (CubeInstance.Position.AsVector) +
                  (UnitRecordArray[6].TargetSize / 2); // 0.1;
              End;
            3:
              Begin
                CubeInstance.Position.X := -UnitRecordArray[6].Members3X *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
                CubeInstance.Position.z := UnitRecordArray[6].Members3Y *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
                  (CubeInstance.Position.AsVector) +
                  (UnitRecordArray[6].TargetSize / 2); // 0.1;
              End;
            4:
              Begin
                CubeInstance.Position.X := -UnitRecordArray[6].Members4X *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
                CubeInstance.Position.z := UnitRecordArray[6].Members4Y *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
                  (CubeInstance.Position.AsVector) +
                  (UnitRecordArray[6].TargetSize / 2); // 0.1;
              End;
            5:
              Begin
                CubeInstance.Position.X := -UnitRecordArray[6].Members5X *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
                CubeInstance.Position.z := UnitRecordArray[6].Members5Y *
                  SomeMagicNumber; // 0;
                CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
                  (CubeInstance.Position.AsVector) +
                  (UnitRecordArray[6].TargetSize / 2); // 0.1;
              End;
          End; // Case Bunkers
        end; // Bunkers

        // Target
        CubeInstance := TGLCubeEX(BunkerDC.AddNewChild(TGLCubeEX));
        CubeInstance.Name := 'EnemyTarget';
        CubeInstance.CubeDepth := UnitRecordArray[6].TargetSize;
        CubeInstance.CubeHeight := UnitRecordArray[6].TargetSize;
        CubeInstance.CubeWidth := UnitRecordArray[6].TargetSize;
        If FileExists(UnitRecordArray[6].FlagFile) then
        begin // ExtractFileName
          CubeInstance.Material.Texture.Image.LoadFromFile
            ((UnitRecordArray[6].FlagFile));
          CubeInstance.Material.Texture.Disabled := False;
        end
        else
          CubeInstance.Material.FrontProperties.Diffuse.AsWinColor :=
            EnemyPositionsColor;
        CubeInstance.Position.X := -UnitRecordArray[6].targetX *
          SomeMagicNumber; // 0;
        CubeInstance.Position.Y := -CubeInstance.Position.Y + 0.05;
        CubeInstance.Position.z := UnitRecordArray[6].targetY * SomeMagicNumber;
        // 0;
        CubeInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
          (CubeInstance.Position.AsVector) +
          (UnitRecordArray[6].TargetSize / 2); // 0.1;

        DCDummy := TGLDummyCube(ActorsDC.AddNewChild(TGLDummyCube));
        DCDummy.Name := 'Enemy0'; // ALREADY MADE ???
        // Load Actor into GLScene
        ActorInstance := TGLActor(DCDummy.AddNewChild(TGLActor));
        ActorInstance.Direction.Y := 1;
        ActorInstance.Direction.X := 0;
        ActorInstance.Direction.z := 0;
        ActorInstance.Up.X := 1;
        ActorInstance.Up.z := 0;
        ActorInstance.Up.Y := 0;
        // UnitRecordArray[6].Md2Name  UnitRecordArray[6].Md2TextureName
        ActorInstance.LoadFromFile((UnitRecordArray[6].Md2Name)); // 'waste.md2'
        ActorInstance.Material.Texture.Image.LoadFromFile
          ((UnitRecordArray[6].Md2TextureName)); // 'waste.jpg'
        ActorInstance.Material.Texture.Disabled := False;
        ActorInstance.Name := 'Actor' + Inttostr(6);
        ActorInstance.Animations.LoadFromFile(ExtractFilePath(ParamStr(0)) +
          'Actors\Quake2Animations.aaf');

        // Scale Actor for put in the Scene
        ActorInstance.Scale.SetVector(UnitRecordArray[6].ActorScale,
          UnitRecordArray[6].ActorScale, UnitRecordArray[6].ActorScale, 0);
        // 0.04, 0.04, 0.04, 0);
        ActorInstance.GetExtents(min, max);
        ScaleVector(min, ActorInstance.Scale.Y);
        ActorInstance.Position.AsAffineVector := min;
        ActorInstance.Position.X := -UnitRecordArray[6].startXLoc *
          SomeMagicNumber; // 0;
        ActorInstance.Position.Y := -ActorInstance.Position.Y + 0.1;
        ActorInstance.Position.z := UnitRecordArray[6].startYLoc *
          SomeMagicNumber; // 0;
        ActorInstance.Position.Y := TerrainRenderer1.InterpolatedHeight
          (ActorInstance.Position.AsVector) + 0.5;
        UnitRecordArray[6].CurrentpathBank := 1;
        ActorInstance.SwitchToAnimation('stand', True);
        ActorInstance.AnimationMode := aamLoop;
      end; // Enemy
    end;
  end; // Actors
  StatusBar1.Panels[6].Text := 'Done';
  Application.ProcessMessages;
  WayPoint.Active := False;
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
end;

procedure TATerrainForm.DoActorMovement;
var
  angle, dist, left, right: single;
  v1, v2, Original: TAffineVector;
  sign: Integer;
begin
  if WayPoint.Active then
  begin
    ActorBase0.Move(0.5);

    Original := ActorBase0.Direction.AsAffineVector;
    VectorSubtract(WayPoint.Position, ActorBase0.Position.AsAffineVector, v1);
    v1.Y := 0;
    v2 := ActorBase0.Direction.AsAffineVector;
    v2.Y := 0;
    angle := VectorAngleCosine(v1, v2);

    // Which way should the actor turn?
    // This is a cheap trick, but it does work. Basically we're just
    // turning the actor both ways and checking which direction is the
    // closest to the direction we want.
    ActorBase0.Turn(1);
    v2 := ActorBase0.Direction.AsAffineVector;
    v2.Y := 0;
    left := VectorAngleCosine(v1, v2);
    ActorBase0.Turn(-1);
    v2 := ActorBase0.Direction.AsAffineVector;
    v2.Y := 0;
    right := VectorAngleCosine(v1, v2);
    sign := 0;
    if right <> left then
    begin
      if right > left then
        sign := -1
      else
        sign := 1;
    end;
    // Return the actor to his original direction
    ActorBase0.Direction.AsAffineVector := Original;

    // This determines how fast the actor should turn
    // Once he is close enough we just set him in the proper direction
    if angle < 0.99 then
      if angle < 0.5 then
        angle := sign * 30
      else if angle < 0.9 then
        angle := sign * 10
      else if angle < 0.95 then
        angle := sign * 5
      else
      begin
        angle := 0;
        ActorBase0.Direction.AsAffineVector := v1;
      end;
    ActorBase0.Turn(angle);

    if currentanim <> 'run' then
    begin
      Actor0.SwitchToAnimation('run', True);
      currentanim := 'run';
    end;
    Actor0.NextFrame;
  end
  else
  begin
    if currentanim <> 'stand' then
    begin
      Actor0.SwitchToAnimation('stand', True);
      currentanim := 'stand';
    end;
  end;

  // Set the actor to the terrain height
  ActorBase0.Position.Y := TerrainRenderer1.InterpolatedHeight
    (ActorBase0.Position.AsVector) + 0.1;

  // If the waypoint is active then the actor moves towards it
  // Once within a distance from the waypoint, deactivate the waypoint
  if WayPoint.Active then
  begin
    WayPoint.Position := Marker.Position.AsAffineVector;
    dist := VectorDistance(WayPoint.Position,
      ActorBase0.Position.AsAffineVector);
    if abs(dist) > 1 then
    begin
      WayPoint.Active := True;
    end
    else
    begin
      WayPoint.Active := False;
    end;
    Marker.Visible := WayPoint.Active;
  end;
end;

// Do Actual Pathfinding..This is called Every Cadence
// Scenario:=1 //Pathfinding ClickMove
// Scenario:=2 else //Move and Deploy (Selected Unit)
// Scenario:=3;     //Move, Deploy, (All Units w/Collision), Target BOSS fight
procedure TATerrainForm.DoScenario1;
Begin
  if PathPoint.Active then
  begin
    CreatePathLines(ActiveUnitNumber);
    PathPoint.Active := False;
  end;
  // (SelectedObject as TGLActor).Tag=1
  // (ActorsDC.Children[1] as TGLActor).SwitchToAnimation('run',True);

  { ActorsDC
    CubeInstance.Name:='Target'+Inttostr(X);
    DCDummy.Name:='ActorBase'+Inttostr(X);
    ActorInstance.Name:='Actor'+Inttostr(X);

    'EnemyBunker'+Inttostr(X);
    DCDummy.Name:='Enemy0';
    ActorInstance.Name:='Actor'+Inttostr(6);
  }
  { For i:= 0 to ActorsDC.Count do
    begin    //'EnemyBunker'+Inttostr(X); 'Target'+Inttostr(X);
    If ActorsDC.Children[i].Name ='Target'+Inttostr(i) then
    begin
    with (ActorsDC.Children[i] as TGLCubeEx) do
    begin
    If TurnAngle >359 then TurnAngle:=0;
    TurnAngle:= TurnAngle+1;
    end; }
end;

procedure TATerrainForm.DoScenario2;
Begin
  if PathPoint.Active then
  begin
    CreatePathLines(ActiveUnitNumber);
    PathPoint.Active := False;
  end;
  {
    //I only want them to Disperse at Target
    //.. NOT have 5 individual Paths
    //ChooseGroupLocations()

    //gGameStarted = False
    //For unit.unit = Each unit
    //unit\pathStatus = notstarted
    4:
    Begin
    For xx := 0 To mapWidth-1 Do
    For yy := 0 To mapHeight-1 Do
    claimedNode[xx,yy] :=0;
    for xx := 1 to 3 do
    begin
    //unit\targetX = unit\xLoc : unit\targetY = unit\yLoc ;required
    xPath[xx] := startXLocArray[xx];
    yPath[xx] := startYLocArray[xx];
    ClearNodes(xx);
    pathStatus[xx] := notstarted;
    end;
    StatusBar1.Panels[5].Text:='0';
    colCount:=3;//NumberofUnits  for when there are many units...
    t:=StartPrecisionTimer;
    //Past:=Now;
    for xx := 1 to 3 do
    pathStatus[xx]:=FindPath(xx,
    //startX*50,startY*50,targetX*50,targetY*50);
    startXLocArray[xx]*tileSize,
    startYLocArray[xx]*tileSize,
    targetXLocArray[xx]*tileSize,
    targetYLocArray[xx]*tileSize);

    //Present:=Now;
    //DecodeTime(Present-Past, Hour, Min, Sec, MSec);
    colTotalTime:=colTotalTime+StopPrecisionTimer(t);
    ts:=colTotalTime*1000/colCount;
    StatusBar1.Panels[1].Text:= //IntToStr(MSec)+' ms';
    Format('%.3f / %d = %.3f ms',[colTotalTime*1000,colCount ,ts ]);

    StatusBar1.Panels[5].Text:='2';

    end;
  }
end;

procedure TATerrainForm.DoScenario3;
// var i:Integer;
Begin
  if PathPoint.Active then
  begin
    CreatePathLines(ActiveUnitNumber);
    PathPoint.Active := False;
  end;
end;

procedure TATerrainForm.DoScenario4;
var
  i: Integer;
Begin
  // Make Cubes Spin When Pathfinding
  For i := 0 to BaseDC.Count - 1 do
  begin // 'EnemyBunker'+Inttostr(X); 'Target'+Inttostr(X);
    { SName:= Copy(ActorsDC.Children[i].Name,0,length(ActorsDC.Children[i].Name)-1);
      If SName ='Target' then
      begin }
    with (BaseDC.Children[i] as TGLCubeEX) do
    begin // TurnAngle  PitchAngle   RollAngle
      If TurnAngle > 359 then
        TurnAngle := 0;
      TurnAngle := TurnAngle + 1;
    end;
    // end;
  end;
end;

procedure TATerrainForm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: single;
  // SName:String;    i:Integer;
begin
  if IsKeyDown(VK_ESCAPE) then
    Close;
  // handle keypresses  to Move Camera
  if IsKeyDown(VK_SHIFT) then
    speed := 5 * deltaTime
  else
    speed := deltaTime;
  with GLCamera1.Position do
  begin
    if IsKeyDown(VK_UP) then
      CameraCube.Translate(z * speed, 0, -X * speed);
    if IsKeyDown(VK_DOWN) then
      CameraCube.Translate(-z * speed, 0, X * speed);
    if IsKeyDown(VK_LEFT) then
      CameraCube.Translate(-X * speed, 0, -z * speed);
    if IsKeyDown(VK_RIGHT) then
      CameraCube.Translate(X * speed, 0, z * speed);
    if IsKeyDown(VK_PRIOR) then
      FCamHeight := FCamHeight + 10 * speed;
    if IsKeyDown(VK_NEXT) then
      FCamHeight := FCamHeight - 10 * speed;
  end;

  If (Testing) then
    DoActorMovement
  else // WayPoint.Active toggles some things, others NEED to be done
    If (not EditModeActive) then
      Case Scenario of
        1:
          DoScenario1;
        2:
          DoScenario2;
        3:
          DoScenario3;
      End; // Case

  // don't drop through terrain!
  CameraCube.Position.Y := TerrainRenderer1.InterpolatedHeight
    (CameraCube.Position.AsVector) + FCamHeight;

  // Animate the arrowlines
  if WayPoint.Active then
  Begin
    ArrowLine1.Position.Y := 0.5 * sin(theta) + 1;
    theta := theta + 1;
  End;
end;

// Standard mouse rotation & FPS code below

procedure TATerrainForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TVector;
  // pathfinderID:Integer;
begin
  if (ssRight in Shift) then // ssCtrl ssLeft
    GLSceneViewer1.Cursor := crHandPoint
  else
    GLSceneViewer1.Cursor := crDefault;
  // red green yellow Red ?nene?
  // ssLeft is used to Move around
  if (ssRight in Shift) then // Place the Gopher Target
  begin
    If (Testing) then
    begin
      // Code from the raycasting demo
      SetVector(rayStart, GLCamera1.AbsolutePosition);
      SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector
        (AffineVectorMake(X, GLSceneViewer1.Height - Y, 0)));
      NormalizeVector(rayVector);
      if TerrainRenderer1.RayCastIntersect(rayStart, rayVector, @iPoint,
        @iNormal) then
      begin
        Marker.Position.AsVector := iPoint;
        WayPoint.Active := True;
      end;
    end
    else If (not EditModeActive) then
    begin
      PathLines.Visible := False;
      PathLines.Nodes.Clear;
      Case Scenario of
        1:
          Begin // Pathfinding ClickMove
            // Code from the raycasting demo
            SetVector(rayStart, GLCamera1.AbsolutePosition);
            SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector
              (AffineVectorMake(X, GLSceneViewer1.Height - Y, 0)));
            NormalizeVector(rayVector);
            if TerrainRenderer1.RayCastIntersect(rayStart, rayVector, @iPoint,
              @iNormal) then
            begin
              // pathfinderID:=ActiveUnitNumber;
              // Check LOCATION it MUST be VALID
              // WHY is it TWICE the expected value?
              IF ((ProjectRecord.mapWidth >
                (-1 * (Trunc(iPoint.X / SomeMagicNumber)))) and
                (Trunc((iPoint.X / SomeMagicNumber)) < 0) and
                (ProjectRecord.mapHeight > ((Trunc(iPoint.Z / SomeMagicNumber))
                )) and (Trunc((iPoint.Z / SomeMagicNumber)) > 0)) then
              begin
                UnitRecordArray[ActiveUnitNumber].targetX :=
                  (-1 * Trunc(iPoint.X / SomeMagicNumber));
                UnitRecordArray[ActiveUnitNumber].targetY := Trunc(iPoint.Z);
                UnitRecordArray[ActiveUnitNumber].targetZ :=
                  Trunc(iPoint.Y / SomeMagicNumber);
                { } showmessage('Target X: ' +
                  Inttostr(UnitRecordArray[ActiveUnitNumber].targetX) + ' Y: ' +
                  Inttostr(UnitRecordArray[ActiveUnitNumber].targetY) + ' Z: ' +
                  Inttostr(UnitRecordArray[ActiveUnitNumber].targetZ));
                { }
                gLoopCount := 1;
                gGameTime := 0;
                gStartTime := StartPrecisionTimer;
                UnitRecordArray[ActiveUnitNumber].pathStatus :=
                  BlitzFindPathH(ActiveUnitNumber,
                  UnitRecordArray[ActiveUnitNumber].startXLoc *
                  ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber]
                  .startYLoc * ProjectRecord.tileSize,
                  // ?? The Pathfinder divides by Tilesize to convert
                  UnitRecordArray[ActiveUnitNumber].targetX *
                  ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber]
                  .targetY * ProjectRecord.tileSize, normal);
                gLoopTime := gLoopTime + StopPrecisionTimer(gStartTime);
                gGameTime := gLoopTime * 1000 / gLoopCount;
                // PathPoint.Position :=iPoint;
                PathPoint.Position.X := iPoint.X;
                PathPoint.Position.Y := iPoint.Y;
                PathPoint.Position.Z := iPoint.Z;
                If (UnitRecordArray[ActiveUnitNumber].pathStatus = found) then
                  PathPoint.Active := True
                else
                begin // NO PATH
                  gGameTime := -1;
                  PathPoint.Active := False;
                end;
              end
              else
              begin // Invalid Target
                gGameTime := -2;
                PathPoint.Active := False;
              end;
            end;
          end;
        2:
          Begin // PathFind THE Selected  Unit
            gLoopCount := 1;
            gGameTime := 0;
            gStartTime := StartPrecisionTimer;
            UnitRecordArray[ActiveUnitNumber].pathStatus :=
              BlitzFindPathH(ActiveUnitNumber, UnitRecordArray[ActiveUnitNumber]
              .startXLoc * ProjectRecord.tileSize,
              UnitRecordArray[ActiveUnitNumber].startYLoc *
              ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber].targetX
              * ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber]
              .targetY * ProjectRecord.tileSize, normal);
            gLoopTime := gLoopTime + StopPrecisionTimer(gStartTime);
            gGameTime := gLoopTime * 1000 / gLoopCount;
            // PathPoint.Position :=iPoint;
            PathPoint.Position.X := iPoint.X;
            PathPoint.Position.Y := iPoint.Y;
            PathPoint.Position.Z := iPoint.Z;
            If (UnitRecordArray[ActiveUnitNumber].pathStatus = found) then
              PathPoint.Active := True
            else
            begin
              gGameTime := -1;
              PathPoint.Active := False;
            end;

          End;
        3:
          Begin // Pathfind 5 Units +1 Enemy    ActiveEnemyNumber:=1
            gLoopCount := 1;
            gGameTime := 0;
            gStartTime := StartPrecisionTimer;
            UnitRecordArray[ActiveUnitNumber].pathStatus :=
              BlitzFindPath(ActiveUnitNumber, UnitRecordArray[ActiveUnitNumber]
              .startXLoc * ProjectRecord.tileSize,
              UnitRecordArray[ActiveUnitNumber].startYLoc *
              ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber].targetX
              * ProjectRecord.tileSize, UnitRecordArray[ActiveUnitNumber]
              .targetY * ProjectRecord.tileSize, normal);
            gLoopTime := gLoopTime + StopPrecisionTimer(gStartTime);
            gGameTime := gLoopTime * 1000 / gLoopCount;
            // PathPoint.Position :=iPoint;
            PathPoint.Position.X := iPoint.X;
            PathPoint.Position.Y := iPoint.Y;
            PathPoint.Position.Z := iPoint.Z;
            If (UnitRecordArray[ActiveUnitNumber].pathStatus = found) then
              PathPoint.Active := True
            else
            begin
              gGameTime := -1;
              PathPoint.Active := False;
            end;
          end;
        4:
          Begin
            SetVector(rayStart, GLCamera1.AbsolutePosition);
            SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector
              (AffineVectorMake(X, GLSceneViewer1.Height - Y, 0)));
            NormalizeVector(rayVector);
            if TerrainRenderer1.RayCastIntersect(rayStart, rayVector, @iPoint,
              @iNormal) then
            begin
              PathPoint.Position.X := iPoint.X;
              PathPoint.Position.Y := iPoint.Y;
              PathPoint.Position.Z := iPoint.Z;
              { } showmessage('Target X: ' +
                Inttostr(Trunc(PathPoint.Position.X)) + ' Y: ' +
                Inttostr(Trunc(PathPoint.Position.Z)) + ' Z: ' +
                Inttostr(Trunc(PathPoint.Position.Y)));
              { }
            end;
          End;

      End; // Case
    end;
  end;
  mx := X;
  my := Y;
end;

procedure TATerrainForm.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then // ssCtrl
    GLSceneViewer1.Cursor := crHandPoint
  else
  begin
    GLSceneViewer1.Cursor := crDefault;
    if ssLeft in Shift then
    begin
      GLCamera1.MoveAroundTarget(my - Y, mx - X);
    end;
  end;
  mx := X;
  my := Y;
  StatusBar1.Panels[4].Text := 'X: ' +
    Inttostr(Trunc(-1 * CameraCube.Position.X)) + ' Y: ' +
    Inttostr(Trunc(CameraCube.Position.z));
  StatusBar1.Panels[5].Text := 'X: ' +
    Inttostr(Trunc((((-1 * CameraCube.Position.X) / ProjectRecord.tileSize) /
    SomeMagicNumber) + 1)) + ' Y: ' +
    Inttostr(Trunc(((CameraCube.Position.z / ProjectRecord.tileSize) /
    SomeMagicNumber + 1)));
end;

procedure TATerrainForm.Timer1Timer(Sender: TObject);
begin // Caption   //HUDText1.Text
  StatusBar1.Panels[6].Text := Format('%.1f FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
  If (not EditModeActive) then
    StatusBar1.Panels[1].Text := Format('%.3f ms', [gGameTime]);
end;

procedure TATerrainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'w', 'W':
      with GLMaterialLibrary1.Materials[0].Material do
      begin
        if PolygonMode = pmLines then
          PolygonMode := pmFill
        else
          PolygonMode := pmLines;
      end;
    'r', 'R':
      FCamHeight := 10;
    '+':
      if GLCamera1.DepthOfView < 2000 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView * 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd * 1.2;
          FogStart := FogStart * 1.2;
        end;
      end;
    '-':
      if GLCamera1.DepthOfView > 300 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView / 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd / 1.2;
          FogStart := FogStart / 1.2;
        end;
      end;
    '*':
      with TerrainRenderer1 do
        if CLODPrecision > 20 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    '8':
      with TerrainRenderer1 do
        if QualityDistance > 40 then
          QualityDistance := Round(QualityDistance * 0.8);
    '9':
      with TerrainRenderer1 do
        if QualityDistance < 1000 then
          QualityDistance := Round(QualityDistance * 1.2);

    's', 'S':
      DoSavePath;
  end;
  Key := #0;
end;

procedure TATerrainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Cancel AStar pathfinding loop
  if ssCtrl in Shift then
  begin
    If (Key = VK_F2) then
      LostinaLoop := False;
    If (Key = VK_F9) then
      AstarUnitsRunning := (not AstarUnitsRunning);
  End;
  if (Key = VK_ESCAPE) then
    Close
  else // if Key = #27 then Close;
    if (Key = VK_RETURN) then
    begin
      // Enter key Toggles smileyActivated
      // If smileyActivated then CreateMapImage() else EditMap();
      EditModeActive := (not EditModeActive);
      If EditModeActive then
        StatusBar1.Panels[0].Text := 'Edit Mode'
      else
        StatusBar1.Panels[0].Text := 'Path Mode';
    end
    else if ssCtrl in Shift then
    begin // CTRL : Enemy
      if ((Key = $36) and (ProjectRecord.NumberofActiveEnemyPatrol > 0)) then
      begin
        ActiveUnitNumber := 6; // 6;//?
        StatusBar1.Panels[2].Text := 'Enemy Unit ';
        // +'Unit # '+Inttostr(ActiveUnitNumber);
      end
      else If ProjectRecord.NumberofActiveEnemy > 0 then
      begin
        if (Key = $31) then
          ActiveEnemyNumber := 1
        else if (Key = $32) then
          ActiveEnemyNumber := 2
        else if (Key = $33) then
          ActiveEnemyNumber := 3
        else if (Key = $34) then
          ActiveEnemyNumber := 4
        else if (Key = $35) then
          ActiveEnemyNumber := 5;
        If ActiveEnemyNumber > ProjectRecord.NumberofActiveEnemy then
          ActiveEnemyNumber := ProjectRecord.NumberofActiveEnemy;
        StatusBar1.Panels[2].Text := 'Enemy # ' + Inttostr(ActiveEnemyNumber);
      end;
    end
    else
    begin // Ctrl not required
      // 5 Units possible  + 1 Enemy
      if (Key = $31) then
        ActiveUnitNumber := 1
      else if (Key = $32) then
        ActiveUnitNumber := 2
      else if (Key = $33) then
        ActiveUnitNumber := 3
      else if (Key = $34) then
        ActiveUnitNumber := 4
      else if (Key = $35) then
        ActiveUnitNumber := 5;
      If ActiveUnitNumber > ProjectRecord.NumberofActiveUnits then
        ActiveUnitNumber := ProjectRecord.NumberofActiveUnits;
      StatusBar1.Panels[2].Text := 'Unit # ' + Inttostr(ActiveUnitNumber);

      If EditModeActive then // Change ONLY in Edit mode
      begin // (Key=VK_F2)  IsKeyDown(VK_F1)
        if (Key = VK_F1) then
          Scenario := 1
        else // Move IAW
          if (Key = VK_F2) then
            Scenario := 2
          else // Move and Deploy (Unit 1)
            if (Key = VK_F3) then
              Scenario := 3
            else // Move, Deploy, Target BOSS fight
              if (Key = VK_F4) then
                Scenario := 4;
        StatusBar1.Panels[3].Text := 'Scenario # ' + Inttostr(Scenario);
      end;
    end;
end;

procedure TATerrainForm.CreatePathLines(PathUnit: Integer);
var
  i, ii: Integer;
  NodeX, NodeY, NodeZ: Double;
begin
  If (UnitRecordArray[PathUnit].pathStatus = found) then
  // If ProjectRecord.PathDisplayed then
  begin
    PathLines.Visible := False;
    PathLines.Nodes.Clear;
    // TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);
    PathLines.NodesAspect := lnaCube;
    PathLines.NodeSize := 12; // 0.1;  //1;
    PathLines.LineWidth := 5; // 1;
    // ShowMessage('pathLength: '+Inttostr(UnitRecordArray[PathUnit].pathLength));
    // ?
    begin
      PathDC.Position.X := -1 * UnitRecordArray[PathUnit]
        .startXLoc { * SomeMagicNumber };
      PathDC.Position.Y := 0;
      PathDC.Position.z := UnitRecordArray[PathUnit]
        .startYLoc { * SomeMagicNumber };
      PathDC.Position.Y := TerrainRenderer1.InterpolatedHeight
        (PathDC.Position.AsVector) + 6;
      PathLines.Position.X := PathDC.Position.X;
      PathLines.Position.z := PathDC.Position.z;
      PathLines.Position.Y := PathDC.Position.Y;
      NodeX := PathDC.Position.X;
      NodeZ := PathDC.Position.z;
      NodeY := PathDC.Position.Y;
      { ShowMessage('start X: '+floattostr(PathDC.Position.X)+
        ' Y: '+floattostr(PathDC.Position.Y)+
        ' Z: '+floattostr(PathDC.Position.Z));
      }
      PathLines.Visible := True;
      PathLines.Nodes.AddNode(NodeX, NodeY, NodeZ);
      ii := UnitRecordArray[PathUnit].pathLength;
      For i := 0 to ii - 1 do
      begin
        // ActorInstance.Position.Y:=TerrainRenderer1.InterpolatedHeight(ActorInstance.Position.AsVector)+0.1;
        // Dir Z=1  Up Y=1
        UnitRecordArray[PathUnit].pathLocation :=
          (UnitRecordArray[PathUnit].pathLocation + 1);
        UnitRecordArray[PathUnit].xPath :=
          BlitzReadPathX(PathUnit, UnitRecordArray[PathUnit].pathLocation);
        UnitRecordArray[PathUnit].yPath :=
          BlitzReadPathY(PathUnit, UnitRecordArray[PathUnit].pathLocation);

        PathDC.Position.X := -1 * UnitRecordArray[PathUnit].xPath *
          SomeMagicNumber;
        PathDC.Position.Y := 0;
        PathDC.Position.z := UnitRecordArray[PathUnit].yPath * SomeMagicNumber;
        PathDC.Position.Y := TerrainRenderer1.InterpolatedHeight
          (PathDC.Position.AsVector) + 6;
        NodeX := PathDC.Position.X;
        NodeY := PathDC.Position.Y;
        NodeZ := PathDC.Position.z;
        PathLines.Nodes.AddNode(NodeX, NodeY, NodeZ);
        { ShowMessage(Inttostr(I)+' X: '+floattostr(PathDC.Position.X)+
          ' Y: '+floattostr(PathDC.Position.Y)+
          ' Z: '+floattostr(PathDC.Position.Z));
        }

      end;
      PathDC.Position.X := -1 * UnitRecordArray[PathUnit].targetX *
        SomeMagicNumber;
      PathDC.Position.Y := 0;
      PathDC.Position.z := UnitRecordArray[PathUnit].targetY * SomeMagicNumber;
      PathDC.Position.Y := TerrainRenderer1.InterpolatedHeight
        (PathDC.Position.AsVector) + 6;
      NodeX := PathDC.Position.X;
      NodeY := PathDC.Position.Y;
      NodeZ := PathDC.Position.z;
      PathLines.Nodes.AddNode(NodeX, NodeY, NodeZ);
      { ShowMessage('target X: '+floattostr(PathDC.Position.X)+
        ' Y: '+floattostr(PathDC.Position.Y)+
        ' Z: '+floattostr(PathDC.Position.Z)); }

    end;
  end;
end;

procedure TATerrainForm.DoSavePath;
var
  F: TextFile;
  S, s2: string;
  X1, Y1, ii, iii: Integer;
Begin // ActiveUnitNumber
  If (UnitRecordArray[ActiveUnitNumber].pathStatus = found) then
  begin
    AssignFile(F, ProjectDirectory + 'PathData.txt');
    Rewrite(F);
    Writeln(F, 'AStar Path File for ' + Inttostr(ActiveUnitNumber));

    X1 := UnitRecordArray[ActiveUnitNumber].startXLoc;
    Y1 := UnitRecordArray[ActiveUnitNumber].startYLoc;
    S := Inttostr(X1);
    s2 := Inttostr(Y1);
    Writeln(F, 'start X ' + S + ' Y ' + s2);
    iii := UnitRecordArray[ActiveUnitNumber].pathLength;
    // Restart to the First one
    UnitRecordArray[ActiveUnitNumber].pathLocation := 0;
    For ii := 0 to iii - 1 do
    begin
      UnitRecordArray[ActiveUnitNumber].pathLocation :=
        (UnitRecordArray[ActiveUnitNumber].pathLocation + 1);
      UnitRecordArray[ActiveUnitNumber].xPath :=
        BlitzReadPathX(ActiveUnitNumber, UnitRecordArray[ActiveUnitNumber]
        .pathLocation);
      UnitRecordArray[ActiveUnitNumber].yPath :=
        BlitzReadPathY(ActiveUnitNumber, UnitRecordArray[ActiveUnitNumber]
        .pathLocation);
      X1 := UnitRecordArray[ActiveUnitNumber].xPath; // +ProjectRecord.tileSize;
      Y1 := UnitRecordArray[ActiveUnitNumber].yPath; // +ProjectRecord.tileSize;
      S := Inttostr(X1);
      s2 := Inttostr(Y1);
      Writeln(F, Inttostr(ii) + ' X ' + S + ' Y ' + s2);
    end;
    X1 := UnitRecordArray[ActiveUnitNumber].targetX;
    Y1 := UnitRecordArray[ActiveUnitNumber].targetY;
    S := Inttostr(X1);
    s2 := Inttostr(Y1);
    Writeln(F, 'target X ' + S + ' Y ' + s2);

    CloseFile(F);
    Application.ProcessMessages;
    if FileExists(ProjectDirectory + 'PathData.txt') then
      ExecuteFile('PathData.txt', '', ProjectDirectory, SW_SHOW)
  end;
End;

end.
