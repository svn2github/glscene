unit AStarFrm;

{ [1] AStar Cube..Data Display Demo Directions
  [2] Demo #1 Smiley
  [3] PATHFINDING DEMO #2 - Smiley & Chaser
  [4] PATHFINDING DEMO #3 - Smiley & Chaser (4-way)
  [5] ;PATHFINDING DEMO #4a - Collision Avoidance (loop-based)
  [6] ;PATHFINDING DEMO #4b - Collision Avoidance (time-based)
  [7] ;PATHFINDING DEMO #5 - Group Movement }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Printers, Menus, ImgList,
  Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls, System.ImageList;

type
  TAStarForm = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    RunMenu: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    OnHelp1: TMenuItem;
    Main1: TMenuItem;
    About1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Run1: TMenuItem;
    Walk1: TMenuItem;
    Crawl1: TMenuItem;
    Pause1: TMenuItem;
    New1: TMenuItem;
    View1: TMenuItem;
    ProjectOptions1: TMenuItem;
    Terrain3D1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    OpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SetupPrinter1: TMenuItem;
    SearchMenu: TMenuItem;
    Dijkstra1: TMenuItem;
    ProjectMenu: TMenuItem;
    AStarDataDisplayMenu: TMenuItem;
    ComegetsomeMenu: TMenuItem;
    GoGowishuponAStarMenu: TMenuItem;
    KingAstaroftheMistyValleyMenu: TMenuItem;
    KingAstarDynamicChaseMenu: TMenuItem;
    CollisionAvoidancetimebased1: TMenuItem;
    GroupMovement1: TMenuItem;
    ScrollBox1: TScrollBox;
    AStarImage: TImage;
    N3: TMenuItem;
    SizeMenu: TMenuItem;
    Size256: TMenuItem;
    Size512: TMenuItem;
    Size650x500: TMenuItem;
    Size800x600: TMenuItem;
    Timer1: TTimer;
    Size1024x1024: TMenuItem;
    N4: TMenuItem;
    Tools1: TMenuItem;
    GridLines1: TMenuItem;
    PathDisplay1: TMenuItem;
    Size1280x1280: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Size640x640: TMenuItem;
    PrintDialog1: TPrintDialog;
    Memo1: TMemo;
    MemoActive: TMenuItem;
    ImageList1: TImageList;
    ImageList0: TImageList;
    Size2560x2560: TMenuItem;
    BFS1: TMenuItem;
    Label1: TLabel;
    Unit1Status: TLabel;
    Unit2Status: TLabel;
    Unit3Status: TLabel;
    Unit4Status: TLabel;
    Unit5Status: TLabel;
    EnemyUnitStatus: TLabel;
    Unit1Time: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    N7: TMenuItem;
    TileSizeMenu: TMenuItem;
    TileSize50x50: TMenuItem;
    TileSize10x10: TMenuItem;
    TileSize32x32: TMenuItem;
    TileSize1x1: TMenuItem;
    ProgressBar1: TProgressBar;
    Diagonal1: TMenuItem;
    Manhattan1: TMenuItem;
    GR32Viewer1: TMenuItem;
    Trot1: TMenuItem;
    Jog1: TMenuItem;
    Gallop1: TMenuItem;
    Glide1: TMenuItem;
    Trundle1: TMenuItem;
    Toddle1: TMenuItem;
    TieBreakersMenu: TMenuItem;
    TBNone1: TMenuItem;
    TBStraight1: TMenuItem;
    TBClose1: TMenuItem;
    TBFar1: TMenuItem;
    SplashScreen1: TMenuItem;
    AutoloadObstacleFile1: TMenuItem;
    N9: TMenuItem;
    ClearObstacles1: TMenuItem;
    SetRandomObstacles1: TMenuItem;
    Euclidean1: TMenuItem;
    N10: TMenuItem;
    TileSize16x16: TMenuItem;
    TileSize8x8: TMenuItem;
    Size2048x2048: TMenuItem;
    Size4096x4096: TMenuItem;
    TileSize2x2: TMenuItem;
    TileSize4x4: TMenuItem;
    N11: TMenuItem;
    SaveUnitLocationsauf1: TMenuItem;
    ProjectLandscaper1: TMenuItem;
    N12: TMenuItem;
    ProjectMapPainter1: TMenuItem;
    UseOriginal1: TMenuItem;
    N8: TMenuItem;
    ProgramOptions1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure DisplayPOFSettings;
    procedure ShowHint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure AutoloadObstacleFile1Click(Sender: TObject);
    procedure ClearObstacles1Click(Sender: TObject);
    procedure SetRandomObstacles1Click(Sender: TObject);
    procedure SaveUnitLocationsauf1Click(Sender: TObject);
    procedure ProjectLandscaper1Click(Sender: TObject);
    procedure ProjectMapPainter1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure OnHelp1Click(Sender: TObject);
    procedure Main1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure GridLines1Click(Sender: TObject);
    procedure PathDisplay1Click(Sender: TObject);
    procedure MemoActiveClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure LoadObstacleData(fileName: String);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure SetupPrinter1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure TileSizeMenuClick(Sender: TObject);
    procedure TieBreakersMenuClick(Sender: TObject);
    procedure SplashScreen1Click(Sender: TObject);
    procedure GR32Viewer1Click(Sender: TObject);
    procedure ProjectOptions1Click(Sender: TObject);
    procedure Terrain3D1Click(Sender: TObject);
    procedure SizeMenuClick(Sender: TObject);
    procedure ResizeImage; // (Width,Height:Integer)
    procedure LoadUnitData;
    procedure RedrawData;
    procedure ProjectMenuClick(Sender: TObject);
    procedure RunMenuClick(Sender: TObject);
    procedure SearchMenuClick(Sender: TObject);
    procedure AStarImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure AStarImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure DoSingleStepDrawing(Step: Integer);
    procedure RunAStarProject;
    procedure PaintAStarPath(ID: Integer);
    procedure UseOriginal1Click(Sender: TObject);
    procedure ProgramOptions1Click(Sender: TObject);
  private
    { Private declarations }
  public
    UseOriginal: Boolean;
    FPSCount: Integer;
    AStarObstacleFileName: String;
  end;

var
  AStarForm: TAStarForm;

//==================================================================
implementation
//==================================================================

uses
  AStarGlobals,
  AStarCode,
  AStarCodeH,
  AStarAboutFrm, // About
  AProjectOptionsFrm, // Options
  fLandscape, // Terrain Creation
  AProjectMapMakerFrm, // Terrain painter
  AGr32ViewerFrm, // Gr32 Viewer: Bitmap sprites
  ATerrainFrm, // 3D GLScene Viewer
  GLVectorGeometry, // math atan2
  GLCrossPlatform, // PrecisionTimer
  JPeg, // Load textures
  GLKeyboard, AProgramOptionsFrm; // User Interface

{$R *.DFM}

procedure TAStarForm.FormCreate(Sender: TObject);
var
  dc: HDC;
  temp: Integer;
begin
  if FileExists(ExtractFilePath(ParamStr(0)) + 'AStar.pof') then
  begin
    DoLoader;
  end
  else
  begin
    ResetDefaults;
  end;
  dc := GetDc(0);
  if not(GetDeviceCaps(dc, BITSPIXEL) = 32) then
  begin
    ShowMessage('32 bit Color Required' + #13#10 +
      'Application Termination Suggested' + #13#10 + 'No idea what happens next'
      + #13#10 + 'All bets are off, AI is Offline');

  end; // DisplaySplashScreen;
  temp := ReleaseDc(0, dc); { Give back the screen dc }
  if temp <> 1 then
    Application.Terminate; // something REALLY failed

  // Set Variables Not set by  POF
  EditModeActive := True;
  LostinaLoop := False;
  AstarUnitsRunning := False;
  UseOriginal := False;
  AStarObstacleFileName := '';
  ProjectFilename := '';
  // ProjectDirectory
  ActiveEnemyNumber := 0;
  gScreenCaptureNumber := 0;
  FPSCount := 0;
  left := AStarFormX;
  top := AStarFormY;
  // Makes Viewer: W650 x H500
  Width := 767;
  Height := 569;
  SplashScreen1.Checked := SplashScreenDisplayed;
  AutoloadObstacleFile1.Checked := AutoloadObstacleFile;

  case SearchMode of
    0:
      begin
        Dijkstra1.Checked := True;
      End;
    1:
      begin
        Diagonal1.Checked := True;
      End;
    2:
      begin
        Manhattan1.Checked := True;
      End;
    3:
      begin
        BFS1.Checked := True;
      End;
    4:
      begin
        Euclidean1.Checked := True;
      End;
  End; // case

  case TieBreakerMode of
    0:
      begin
        TBNone1.Checked := True;
      End;
    1:
      begin
        TBStraight1.Checked := True;
      End;
    2:
      begin
        TBClose1.Checked := True;
      End;
    3:
      begin
        TBFar1.Checked := True;
      End;
  End; // case

  case TileSizeMode of
    0:
      begin
        TileSize50x50.Checked := True;
        tileSize := 50;
      End;
    1:
      begin
        TileSize32x32.Checked := True;
        tileSize := 32;
      End;
    2:
      begin
        TileSize16x16.Checked := True;
        tileSize := 16;
      End;
    3:
      begin
        TileSize10x10.Checked := True;
        tileSize := 10;
      End;
    4:
      begin
        TileSize8x8.Checked := True;
        tileSize := 8;
      End;
    5:
      begin
        TileSize4x4.Checked := True;
        tileSize := 4;
      End;
    6:
      begin
        TileSize2x2.Checked := True;
        tileSize := 2;
      End;
    7:
      begin
        TileSize1x1.Checked := True;
        tileSize := 1;
      End;
  end;

  case ImageSizeMode of
    0:
      begin
        Size650x500.Checked := True;
        ImageWidth := 650;
        ImageHeight := 500;
      End;
    1:
      begin
        Size800x600.Checked := True;
        ImageWidth := 800;
        ImageHeight := 600;
      End;
    2:
      begin
        Size640x640.Checked := True;
        ImageWidth := 640;
        ImageHeight := 640;
      End;
    3:
      begin
        Size1280x1280.Checked := True;
        ImageWidth := 1280;
        ImageHeight := 1280;
      End;
    4:
      begin
        Size2560x2560.Checked := True;
        ImageWidth := 2560;
        ImageHeight := 2560;
      End;
    5:
      begin
        Size256.Checked := True;
        ImageWidth := 256;
        ImageHeight := 256;
      End;
    6:
      begin
        Size512.Checked := True;
        ImageWidth := 512;
        ImageHeight := 512;
      End;
    7:
      begin
        Size1024x1024.Checked := True;
        ImageWidth := 1024;
        ImageHeight := 1024;
      End;
    8:
      begin
        Size2048x2048.Checked := True;
        ImageWidth := 2048;
        ImageHeight := 1024;
      End;
    9:
      begin
        Size4096x4096.Checked := True;
        ImageWidth := 4096;
        ImageHeight := 1024;
      End;
  End; // case
  case AstarMode of
    1: AStarDataDisplayMenu.Click;
    2: ComegetsomeMenu.Click;
    3: GoGowishuponAStarMenu.Click;
    4: KingAstarDynamicChaseMenu.Click;
    5: KingAstaroftheMistyValleyMenu.Click;
    6: CollisionAvoidancetimebased1.Click;
    7: GroupMovement1.Click;
  End; // case
  // InitializePathfinder;
  // Set any Changes..Defaults from POF
  DisplayPOFSettings;
end;

procedure TAStarForm.FormActivate(Sender: TObject);
begin
  DisplayPOFSettings;
end;

procedure TAStarForm.DisplayPOFSettings;
Begin
  GridLines1.Checked := GridLinesDisplayed;
  PathDisplay1.Checked := PathDisplayed;
End;

procedure TAStarForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[5].Text := Application.Hint;
end;

procedure TAStarForm.FormShow(Sender: TObject);
begin
  Application.OnHint := AStarForm.ShowHint;
  // StatusBar1.Panels[5].Text := Application.Hint;
  // Tip: To further customize the hint window,
  // create a custom descendant of THintWindow
  // and assign it to the global HintWindowClass variable.
end;

{ //ScreenCapture by pressing print screen or F12. Successive screen
  //captures during the same program run will be saved separately.
  Function ScreenCapture()
  If KeyHit(88) Or KeyHit(183)
  SaveBuffer(BackBuffer(),"screenshot"+gScreenCaptureNumber+".bmp")
  gScreenCaptureNumber = gScreenCaptureNumber+1 ;global enables multiple captures
  EndIf
  End Function }
procedure TAStarForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Cancel AStar pathfinding loop
  if ssCtrl in Shift then
  begin
    If (Key = VK_F2) then
      LostinaLoop := False;
    If (Key = VK_F9) then
      AstarUnitsRunning := (not AstarUnitsRunning);
    // If KeyHit(88) Or KeyHit(183)
    If (Key = VK_F12) then
      AStarImage.picture.savetofile(ProjectDirectory + AStarObstacleFileName +
        'screenshot' + inttostr(gScreenCaptureNumber) + '.bmp');
    gScreenCaptureNumber := gScreenCaptureNumber + 1;
    // global enables multiple captures
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
      If AstarMode = 7 then
      begin { This should be in Mouse Event
          gGameStarted = False
          For unit.unit = Each unit
          unit\pathStatus = notstarted
          //gDrawMore is toggled by Menu DrawPaths
          ;Toggle drawing more (Path Line) by pressing space bar
          If KeyHit(57) Then gDrawMore = 1-gDrawMore
        }
      end;
    end
    else if ssCtrl in Shift then
    begin
      If ActiveEnemyNumber > 0 then
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
        if (Key = $36) then
        begin
          ActiveEnemyNumber := 6; // 6;//?
          StatusBar1.Panels[2].Text := 'Enemy Unit ';
          // +'Unit # '+Inttostr(ActiveUnitNumber);
        end
        else
          StatusBar1.Panels[2].Text := 'Enemy # ' + inttostr(ActiveEnemyNumber);
      end;
    end
    else
    begin
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
      If ActiveUnitNumber > NumberofUnits then
        ActiveUnitNumber := NumberofUnits;
      StatusBar1.Panels[2].Text := 'Unit # ' + inttostr(ActiveUnitNumber);
      if (Key = $39) then
        DoSingleStepDrawing(9); // key 9 Start SingleStep Drawing
      if (Key = $30) then
        DoSingleStepDrawing(0); // key 0 Finish Drawing
    end;
end;

procedure TAStarForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AStarFormX := AStarForm.left;
  AStarFormY := AStarForm.top;
  // showmessage('what');
  DoSaver;
  Application.ProcessMessages;
end;

procedure TAStarForm.FormDestroy(Sender: TObject);
begin
  SetLength(MAGColorValueArray, 0);
  EndPathfinder;
end;

procedure TAStarForm.Contents1Click(Sender: TObject);
begin
  // Application.HelpFile := 'MYHELP.HLP';
  // Application.HelpCommand(HELP_FINDER, 0);
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TAStarForm.OnHelp1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TAStarForm.Main1Click(Sender: TObject);
// Application.HelpContext(234);
var
  PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0));
  if FileExists(PathS + 'astar1.html') then
    ExecuteFile('astar1.html', '', PathS, SW_SHOW)
end;

procedure TAStarForm.About1Click(Sender: TObject);
begin
  // AStarAboutForm.Show;
  AStarAboutForm := TAStarAboutForm.Create(Application);
  AStarAboutForm.ShowModal;
  AStarAboutForm.Free;
end;

procedure TAStarForm.New1Click(Sender: TObject);
var
  X, Y: Integer;
begin
  // Reset whatever
  EditModeActive := True;
  AStarObstacleFileName := '';
  ProjectFilename := '';
  // initialize the map to completely walkable
  for X := 0 to mapWidth do
    for Y := 0 to mapHeight do
      walkability[X][Y] := walkable;
  // Repaint the Image
  ResizeImage;
end;

procedure TAStarForm.Open1Click(Sender: TObject);

begin
  // astar|*.dat|jpeg|*.jpg|bitmap|*.bmp|png|*.png  ;*.dat
  OpenDialog.Filter := 'astar Obstacle files|*.aof'; //
  OpenDialog.InitialDir := ProjectDirectory;
  // Clear or set anything previous
  OpenDialog.fileName := ''; // '*.aof'; //'tile10x80x60.aof';
  if OpenDialog.Execute then
  Begin
    // ProjectDirectory:=ExtractFilePath(OpenDialog.FileName);
    Application.ProcessMessages;
    { if uppercase(extractfileext(OpenDialog.filename))='.APF' then
      LoadProjectData(OpenDialog.FileName) else }
    if uppercase(extractfileext(OpenDialog.fileName)) = '.AOF' then
      LoadObstacleData(OpenDialog.fileName);
  End;
end;

procedure TAStarForm.LoadObstacleData(fileName: String);
var
  // F: TextFile;   S: string;
  F: File of Integer; // byte;
  z, // :byte;
  intilesize, inmapwidth, inmapHeight, X, Y: Integer;
Begin
  Application.ProcessMessages;
  AStarObstacleFileName := extractfilename(fileName);
  AStarForm.Caption := 'AStar: ' + AStarObstacleFileName;
  AssignFile(F, fileName);
  Reset(F);
  Read(F, intilesize);
  Read(F, inmapwidth);
  Read(F, inmapHeight);
  If intilesize <= tileSize then
  begin
    for X := 0 to inmapwidth - 1 do
      for Y := 0 to inmapHeight - 1 do
      begin
        Read(F, z);
        walkability[X][Y] := z;
        // Read(F, walkability [x][y]);
        if (walkability[X][Y] > 1) then
          walkability[X][Y] := 0;
      end;
  end
  else
    ShowMessage('in tilesize: ' + inttostr(intilesize) + ' is larger than ' +
      inttostr(intilesize));
  CloseFile(F);
  RedrawData;
  // Called when Moused... Things Might change... island
  // If  ProcessNoGoIslands then IdentifyIslands;
End;

procedure TAStarForm.Save1Click(Sender: TObject);
var
  F: File of Integer;
  X, Y, z: Integer;
begin
  If AStarObstacleFileName = '' then
    SaveAs1Click(Sender)
  else
  begin
    Application.ProcessMessages;
    AssignFile(F, ProjectDirectory + AStarObstacleFileName);
    Rewrite(F);
    write(F, tileSize);
    write(F, mapWidth);
    write(F, mapHeight);
    for X := 0 to mapWidth - 1 do
      for Y := 0 to mapHeight - 1 do
      begin // walkable is 0
        if (walkability[X][Y] <> 1) then
          walkability[X][Y] := 0;
        z := walkability[X][Y];
        write(F, z);
      end;
    CloseFile(F);
  end;
end;

procedure TAStarForm.SaveAs1Click(Sender: TObject);
begin
  // astar|*.dat|jpeg|*.jpg|bitmap|*.bmp|png|*.png
  SaveDialog1.Filter := 'astar.aof or bitmap.bmp|*.aof|bitmap|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.fileName := AStarObstacleFileName;
  if SaveDialog1.Execute then
  Begin
    if uppercase(extractfileext(SaveDialog1.fileName)) = '.AOF' then
    begin
      // ProjectDirectory:=ExtractFilePath(savedialog1.FileName);
      AStarObstacleFileName := extractfilename(SaveDialog1.fileName);
      Save1Click(Sender);
    end
    else if uppercase(extractfileext(SaveDialog1.fileName)) = '.BMP' then
    begin
      AStarImage.picture.savetofile(SaveDialog1.fileName);
    end;
  end;
end;

procedure TAStarForm.ProgramOptions1Click(Sender: TObject);
begin
  AProgramOptionsForm.Show;
  { AProgramOptionsForm := TAProgramOptionsForm.Create(Application);
    AProgramOptionsForm.ShowModal;
    AProgramOptionsForm.Free;
    DisplayPOFSettings; }
end;

procedure TAStarForm.SetupPrinter1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TAStarForm.Print1Click(Sender: TObject);
// var    p:TPoint;
begin
  If PrintDialog1.Execute then
    with Printer do
    begin
      // p:=getscale(pagewidth-1,pageheight-1);
      BeginDoc;
      // printer.canvas.stretchDraw(rect(1,1,p.x-2,p.y-2),AStarImage.picture.bitmap);
      Canvas.Draw((PageWidth - AStarImage.picture.bitmap.Width) div 2,
        (PageHeight - AStarImage.picture.bitmap.Height) div 2,
        AStarImage.picture.bitmap);
      EndDoc;
    end;
end;

procedure TAStarForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TAStarForm.ProjectOptions1Click(Sender: TObject);
begin
  AProjectOptionsForm.ShowModal; // Options
  { AProjectOptionsForm := TAProjectOptionsForm.Create(Application);
    AProjectOptionsForm.ShowModal;
    AProjectOptionsForm.Free;
    DisplayPOFSettings; }
end;

procedure TAStarForm.ProjectLandscaper1Click(Sender: TObject);
begin
  // frmLandscape.show;
  frmLandscape := TfrmLandscape.Create(Application);
  frmLandscape.ShowModal;
  frmLandscape.Free;
end;

procedure TAStarForm.ProjectMapPainter1Click(Sender: TObject);
begin
  AProjectMapMakerForm := TAProjectMapMakerForm.Create(Application);
  AProjectMapMakerForm.ShowModal;
  AProjectMapMakerForm.Free;
end;

procedure TAStarForm.GR32Viewer1Click(Sender: TObject);
begin
  OpenDialog.Filter := 'astar projects|*.apf';
  OpenDialog.InitialDir := ProjectDirectory;
  OpenDialog.fileName := '*.apf';
  if OpenDialog.Execute then
  begin
    Application.ProcessMessages;
    if uppercase(extractfileext(OpenDialog.fileName)) = '.APF' then
    begin // All inside as No Test mode...
      ProjectFilename := OpenDialog.fileName;
      AGr32ViewerForm := TAGr32ViewerForm.Create(Application);
      AGr32ViewerForm.ShowModal;
      AGr32ViewerForm.Free;
      ProjectFilename := '';
    end;
  end;
end;

procedure TAStarForm.Terrain3D1Click(Sender: TObject);
begin
  ProjectFilename := ''; // to enable Testing
  OpenDialog.Filter := 'astar projects|*.apf';
  OpenDialog.InitialDir := ProjectDirectory;
  OpenDialog.fileName := '*.apf';
  if OpenDialog.Execute then
  begin
    Application.ProcessMessages;
    if uppercase(extractfileext(OpenDialog.fileName)) = '.APF' then
    begin
      ProjectFilename := OpenDialog.fileName;
    end;
  end;
  // ATerrainForm 3D Viewer
  ATerrainForm := TATerrainForm.Create(Application);
  ATerrainForm.ShowModal;
  ATerrainForm.Free;
  ProjectFilename := '';
end;

procedure TAStarForm.ClearObstacles1Click(Sender: TObject);
var
  X, Y: Integer;
begin
  for X := 0 to mapWidth - 1 do
    for Y := 0 to mapHeight - 1 do
      walkability[X][Y] := 0;
  RedrawData;
end;

procedure TAStarForm.SetRandomObstacles1Click(Sender: TObject);
var
  X, Y, z: Integer;
begin
  for X := 0 to mapWidth - 1 do
    for Y := 0 to mapHeight - 1 do
    begin //
      z := Random(100);
      if (walkability[X][Y] <> 1) then
        If z > 92 then
          walkability[X][Y] := 1; // obstacle created
    end;
  RedrawData;
end;

procedure TAStarForm.SaveUnitLocationsauf1Click(Sender: TObject);
begin
  If NumberofUnits >= 5 then
  Begin
    // Load the file, then Change the Unit Locations.
    OpenDialog.Filter := 'astar Units|*.auf';
    OpenDialog.InitialDir := ProjectDirectory;
    OpenDialog.fileName := '*.auf';
    if OpenDialog.Execute then
    begin
      Application.ProcessMessages;
      // FileAufEdit.Text:= OpenDialog.FileName;
      LoadUnitsFile(OpenDialog.fileName);
    end;
    // startXLocArray,startYLocArray, //Set seeker location
    // UnitRecordArray[1].startXLoc
    // targetXLocArray,targetYLocArray,//Set initial target location.
    // UnitRecordArray[1].targetX
    // speedArray :  Array [1..5]of Integer;
    // UnitRecordArray[1].speed

    // AProjectOptionsForm.SaveaufBtn.Click
  End
  else
    ShowMessage('Select a Project having All 5 units Active');
end;

procedure TAStarForm.GridLines1Click(Sender: TObject);
begin
  GridLines1.Checked := (not GridLines1.Checked);
  GridLinesDisplayed := GridLines1.Checked;
end;

procedure TAStarForm.PathDisplay1Click(Sender: TObject);
begin
  PathDisplay1.Checked := (not PathDisplay1.Checked);
  PathDisplayed := PathDisplay1.Checked;
end;

procedure TAStarForm.SplashScreen1Click(Sender: TObject);
begin
  SplashScreen1.Checked := (not SplashScreen1.Checked);
  SplashScreenDisplayed := SplashScreen1.Checked;
end;

procedure TAStarForm.AutoloadObstacleFile1Click(Sender: TObject);
begin
  AutoloadObstacleFile1.Checked := (not AutoloadObstacleFile1.Checked);
  AutoloadObstacleFile := AutoloadObstacleFile1.Checked;
end;

procedure TAStarForm.MemoActiveClick(Sender: TObject);
begin
  MemoActive.Checked := (not MemoActive.Checked);
  Memo1.Enabled := MemoActive.Checked;
end;

//-----------------------------------------------------------------------

procedure TAStarForm.RunMenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of

    // Key P Pause
    // Number of Pixels along Path to move
    // maybe Run is 1 ..Amount of Time * ? Application.ProcessMessages
    1:
      begin
        Run1.Checked := True;
        speedArray[ActiveUnitNumber] := 9;
      End;
    2:
      begin
        Gallop1.Checked := True;
        speedArray[ActiveUnitNumber] := 8;
      End;
    3:
      begin
        Jog1.Checked := True;
        speedArray[ActiveUnitNumber] := 7;
      End;
    4:
      begin
        Trot1.Checked := True;
        speedArray[ActiveUnitNumber] := 6;
      End;
    5:
      begin
        Walk1.Checked := True;
        speedArray[ActiveUnitNumber] := 5;
      End;
    6:
      begin
        Trundle1.Checked := True;
        speedArray[ActiveUnitNumber] := 4;
      End;
    7:
      begin
        Glide1.Checked := True;
        speedArray[ActiveUnitNumber] := 3;
      End;
    8:
      begin
        Toddle1.Checked := True;
        speedArray[ActiveUnitNumber] := 2;
      End;
    9:
      begin
        Crawl1.Checked := True;
        speedArray[ActiveUnitNumber] := 1;
      End;
    10:
      begin
        Pause1.Checked := True;
        speedArray[ActiveUnitNumber] := 0;
      End;
  End; // case
end;

procedure TAStarForm.SearchMenuClick(Sender: TObject);
begin
  SearchMode := TComponent(Sender).Tag;
  case TComponent(Sender).Tag of
    0:
      begin
        Dijkstra1.Checked := True;
      End;
    1:
      begin
        Diagonal1.Checked := True;
      End;
    2:
      begin
        Manhattan1.Checked := True;
      End;
    3:
      begin
        BFS1.Checked := True;
      End;
    4:
      begin
        Euclidean1.Checked := True;
      End;
  End; // case
end;

//-----------------------------------------------------------------------

procedure TAStarForm.UseOriginal1Click(Sender: TObject);
begin
  UseOriginal1.Checked := (not UseOriginal1.Checked);
  UseOriginal := UseOriginal1.Checked;
end;

procedure TAStarForm.TieBreakersMenuClick(Sender: TObject);
begin
  TieBreakerMode := TComponent(Sender).Tag;
  case TComponent(Sender).Tag of
    0:
      begin
        TBNone1.Checked := True;
      End;
    1:
      begin
        TBStraight1.Checked := True;
      End;
    2:
      begin
        TBClose1.Checked := True;
      End;
    3:
      begin
        TBFar1.Checked := True;
      End;
  End; // case
end;

//-----------------------------------------------------------------------

procedure TAStarForm.ProjectMenuClick(Sender: TObject);
begin
  EditModeActive := True;
  StatusBar1.Panels[0].Text := 'Edit Mode';
  // Application.ProcessMessages;
  // ResizeImage;
  // tileSize:=10;
  case TComponent(Sender).Tag of
    1:
      begin
        AStarDataDisplayMenu.Checked := True;
        TileSizeMode := 0;
        NumberofUnits := 1;
      End;
    2:
      begin
        ComegetsomeMenu.Checked := True;
        NumberofUnits := 1;
        TileSizeMode := 3;
      End;
    3:
      begin
        GoGowishuponAStarMenu.Checked := True;
        NumberofUnits := 5;
        TileSizeMode := 3;
      End;
    4:
      begin
        KingAstarDynamicChaseMenu.Checked := True;
        NumberofUnits := 3;
        TileSizeMode := 3;
      End;
    5:
      begin
        KingAstaroftheMistyValleyMenu.Checked := True;
        NumberofUnits := 4;
        TileSizeMode := 3;
      End;
    6:
      begin
        CollisionAvoidancetimebased1.Checked := True;
        NumberofUnits := 5;
        TileSizeMode := 3;
      End;
    7:
      begin
        GroupMovement1.Checked := True;
        NumberofUnits := 5;
        /// /6;  5or6
        TileSizeMode := 3;
      End; // Enemy is 6
  End; // case
  AstarMode := TComponent(Sender).Tag;
  case TileSizeMode of
    0:
      begin
        TileSize50x50.Checked := True;
        tileSize := 50;
      End;
    1:
      begin
        TileSize32x32.Checked := True;
        tileSize := 32;
      End;
    2:
      begin
        TileSize16x16.Checked := True;
        tileSize := 16;
      End;
    3:
      begin
        TileSize10x10.Checked := True;
        tileSize := 10;
      End;
    4:
      begin
        TileSize8x8.Checked := True;
        tileSize := 8;
      End;
    5:
      begin
        TileSize4x4.Checked := True;
        tileSize := 4;
      End;
    6:
      begin
        TileSize2x2.Checked := True;
        tileSize := 2;
      End;
    7:
      begin
        TileSize1x1.Checked := True;
        tileSize := 1;
      End;
  end;
  ResizeImage;
end;

//-----------------------------------------------------------------------

procedure TAStarForm.SizeMenuClick(Sender: TObject);
begin
  EditModeActive := True;
  StatusBar1.Panels[0].Text := 'Edit Mode';
  case TComponent(Sender).Tag of
    0:
      begin
        Size650x500.Checked := True;
        ImageWidth := 650;
        ImageHeight := 500;
      End;
    1:
      begin
        Size800x600.Checked := True;
        ImageWidth := 800;
        ImageHeight := 600;
      End;
    2:
      begin
        Size640x640.Checked := True;
        ImageWidth := 640;
        ImageHeight := 640;
      End;
    3:
      begin
        Size1280x1280.Checked := True;
        ImageWidth := 1280;
        ImageHeight := 1280;
      End;
    4:
      begin
        Size2560x2560.Checked := True;
        ImageWidth := 2560;
        ImageHeight := 2560;
      End;
    5:
      begin
        Size256.Checked := True;
        ImageWidth := 256;
        ImageHeight := 256;
      End;
    6:
      begin
        Size512.Checked := True;
        ImageWidth := 512;
        ImageHeight := 512;
      End;
    7:
      begin
        Size1024x1024.Checked := True;
        ImageWidth := 1024;
        ImageHeight := 1024;
      End;
    8:
      begin
        Size2048x2048.Checked := True;
        ImageWidth := 2048;
        ImageHeight := 1024;
      End;
    9:
      begin
        Size4096x4096.Checked := True;
        ImageWidth := 4096;
        ImageHeight := 1024;
      End;
  End; // case
  ImageSizeMode := TComponent(Sender).Tag;
  ResizeImage;
end;

//-----------------------------------------------------------------------

procedure TAStarForm.TileSizeMenuClick(Sender: TObject);
begin
  // TileSize50x50   TileSize25x25  TileSize10x10
  case TComponent(Sender).Tag of
    0:
      begin
        TileSize50x50.Checked := True;
        tileSize := 50;
      End;
    1:
      begin
        TileSize32x32.Checked := True;
        tileSize := 32;
      End;
    2:
      begin
        TileSize16x16.Checked := True;
        tileSize := 16;
      End;
    3:
      begin
        TileSize10x10.Checked := True;
        tileSize := 10;
      End;
    4:
      begin
        TileSize8x8.Checked := True;
        tileSize := 8;
      End;
    5:
      begin
        TileSize4x4.Checked := True;
        tileSize := 4;
      End;
    6:
      begin
        TileSize2x2.Checked := True;
        tileSize := 2;
      End;
    7:
      begin
        TileSize1x1.Checked := True;
        tileSize := 1;
      End;
  end;
  TileSizeMode := TComponent(Sender).Tag;
  ResizeImage;
end;

// This is a Cluster of things
// RedrawData is similar but does NOT ..
// EXPECT a set of things IAW  AstarMode
// InitializePathfinder
// load the Map data  LoadObstacleData
// Reset Unit data    LoadUnitData
procedure TAStarForm.ResizeImage;
var
  BitMap1: TBitMap;
  X, Y: Integer;
Begin // ResizeImage(256,256);
  AStarImage.Width := ImageWidth;
  AStarImage.Height := ImageHeight;
  BitMap1 := TBitMap.Create;
  try
    BitMap1.Width := ImageWidth;
    BitMap1.Height := ImageHeight;
    BitMap1.Canvas.Brush.Color := BackGroundColor; //
    BitMap1.Canvas.Brush.Style := bsSolid;
    BitMap1.Canvas.FillRect(Rect(0, 0, ImageWidth, ImageHeight));
    AStarImage.picture.bitmap.Assign(BitMap1);
  finally
    BitMap1.Free;
  end;
  mapWidth := ImageWidth div tileSize;
  mapHeight := ImageHeight div tileSize;
  // EndPathfinder;
  InitializePathfinder;
  // Clear the map
  For X := 0 to mapWidth - 1 do
    For Y := 0 to mapHeight - 1 do
      walkability[X][Y] := walkable;
  // mapWidth = 16, mapHeight = 12, tileSize = 50, numberPeople = 1;
  // 80              60             10
  If AutoloadObstacleFile then
    Case AstarMode of // tile50x13x10.aof  //tile10x65x50.aof
      1:
        If FileExists(ProjectDirectory + 'tile50x13x10.aof') then
          LoadObstacleData(ProjectDirectory + 'tile50x13x10.aof');
    else
      If FileExists(ProjectDirectory + 'tile10x65x50.aof') then
        LoadObstacleData(ProjectDirectory + 'tile10x65x50.aof');
    end;
  // Draw Grid
  AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //
  AStarImage.picture.bitmap.Canvas.Brush.Style := bsSolid;
  If GridLinesDisplayed then
    AStarImage.picture.bitmap.Canvas.Pen.Color := GridColor
  else
    AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor; //
  { CreateMapImage }
  For X := 0 to mapWidth - 1 do
    For Y := 0 to mapHeight - 1 do
    begin
      if (walkability[X][Y] = unwalkable) then
        AStarImage.picture.bitmap.Canvas.Brush.Color := ObstacleColor
      else
        AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //

      AStarImage.picture.bitmap.Canvas.Rectangle
        (Rect(X * tileSize, Y * tileSize, X * tileSize + tileSize,
        Y * tileSize + tileSize));

    end;
  LoadUnitData;

  For X := 1 to NumberofUnits do
  Begin // Circles in Squares
    walkability[startXLocArray[X] div tileSize][startYLocArray[X] div tileSize]
      := walkable;
    walkability[targetXLocArray[X] div tileSize]
      [targetXLocArray[X] div tileSize] := walkable;
    // So the Units can have Base and Target the same Color
    // and the Demo still has the Classic colors
    If AstarMode = 1 then
      AStarImage.picture.bitmap.Canvas.Brush.Color := clGreen
    else
      AStarImage.picture.bitmap.Canvas.Brush.Color := BaseStartColorArray[X];
    // Base is Square..Make ? smaller than Block  Ellipse
    AStarImage.picture.bitmap.Canvas.Rectangle
      (Rect(startXLocArray[X] * tileSize, startYLocArray[X] * tileSize,
      startXLocArray[X] * tileSize + tileSize, startYLocArray[X] * tileSize +
      tileSize));
    If AstarMode = 1 then
      AStarImage.picture.bitmap.Canvas.Brush.Color := clRed
    else
      AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray[X];
    AStarImage.picture.bitmap.Canvas.Ellipse(Rect(targetXLocArray[X] * tileSize,
      targetYLocArray[X] * tileSize, (targetXLocArray[X] * tileSize) + tileSize,
      (targetYLocArray[X] * tileSize) + tileSize));
  End;
  If ActiveEnemyNumber > 0 then
  begin
    walkability[EnemystartXLoc div tileSize][EnemystartYLoc div tileSize]
      := walkable;

    // ActiveEnemyPosition  Need Random Generator ?
    AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyUnitBaseColor;
    // Base is Square..Make ? smaller than Block
    AStarImage.picture.bitmap.Canvas.Rectangle(Rect(EnemystartXLoc * tileSize,
      EnemystartYLoc * tileSize, EnemystartXLoc * tileSize + tileSize,
      EnemystartYLoc * tileSize + tileSize));

    AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyUnitGoalTargetColor;
    AStarImage.picture.bitmap.Canvas.Ellipse(Rect(EnemytargetXLoc * tileSize,
      EnemytargetYLoc * tileSize, EnemytargetXLoc * tileSize + tileSize,
      EnemytargetYLoc * tileSize + tileSize));

    For X := 1 to 5 do
    Begin // Circles in Squares
      walkability[EnemyBaseXLocArray[X] div tileSize]
        [EnemyBaseYLocArray[X] div tileSize] := walkable;

      AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyPositionsColor;
      // Base is Square..Make ? smaller than Block
      AStarImage.picture.bitmap.Canvas.Rectangle
        (Rect(EnemyBaseXLocArray[X] * tileSize, EnemyBaseYLocArray[X] *
        tileSize, EnemyBaseXLocArray[X] * tileSize + tileSize,
        EnemyBaseYLocArray[X] * tileSize + tileSize));
    end;
  end;
  // to set the image canvas font.. Must do every time cause it forgets.
  AStarImage.picture.bitmap.Canvas.Font.Assign(AStarForm.Font);
  StatusBar1.Panels[5].Text := 'Size X: ' + inttostr(ImageWidth) + ' Y: ' +
    inttostr(ImageHeight) + ' Tilesize: ' + inttostr(tileSize);
End;

//-----------------------------------------------------------------------

procedure TAStarForm.RedrawData;
var
  X, Y: Integer;
Begin
  AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //
  AStarImage.picture.bitmap.Canvas.Brush.Style := bsSolid;
  AStarImage.picture.bitmap.Canvas.FillRect(Rect(0, 0, ImageWidth,
    ImageHeight));
  // Draw Grid
  AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //
  AStarImage.picture.bitmap.Canvas.Brush.Style := bsSolid;
  If GridLinesDisplayed then
    AStarImage.picture.bitmap.Canvas.Pen.Color := GridColor
  else
    AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor; //
  { CreateMapImage }
  For X := 0 to mapWidth - 1 do
    For Y := 0 to mapHeight - 1 do
    begin
      if (walkability[X][Y] = unwalkable) then
        AStarImage.picture.bitmap.Canvas.Brush.Color := ObstacleColor
      else
        AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //

      AStarImage.picture.bitmap.Canvas.Rectangle
        (Rect(X * tileSize, Y * tileSize, X * tileSize + tileSize,
        Y * tileSize + tileSize));
    end;

  For X := 1 to NumberofUnits do
  Begin // Circles in Squares
    walkability[startXLocArray[X] div tileSize][startYLocArray[X] div tileSize]
      := walkable;
    walkability[targetXLocArray[X] div tileSize]
      [targetXLocArray[X] div tileSize] := walkable;

    If AstarMode = 1 then
      AStarImage.picture.bitmap.Canvas.Brush.Color := clGreen
    else
      AStarImage.picture.bitmap.Canvas.Brush.Color := BaseStartColorArray[X];
    // Base is Square..Make ? smaller than Block
    AStarImage.picture.bitmap.Canvas.Rectangle
      (Rect(startXLocArray[X] * tileSize, startYLocArray[X] * tileSize,
      startXLocArray[X] * tileSize + tileSize, startYLocArray[X] * tileSize +
      tileSize));

    If AstarMode = 1 then
      AStarImage.picture.bitmap.Canvas.Brush.Color := clRed
    else
      AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray[X];
    AStarImage.picture.bitmap.Canvas.Ellipse(Rect(targetXLocArray[X] * tileSize,
      targetYLocArray[X] * tileSize, (targetXLocArray[X] * tileSize) + tileSize,
      (targetYLocArray[X] * tileSize) + tileSize));
  End;
  If ActiveEnemyNumber > 0 then
  begin
    walkability[EnemystartXLoc div tileSize][EnemystartYLoc div tileSize]
      := walkable;

    // ActiveEnemyPosition  Need Random Generator
    AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyUnitBaseColor;
    // Base is Square..Make ? smaller than Block
    AStarImage.picture.bitmap.Canvas.Rectangle(Rect(EnemystartXLoc * tileSize,
      EnemystartYLoc * tileSize, EnemystartXLoc * tileSize + tileSize,
      EnemystartYLoc * tileSize + tileSize));

    AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyUnitGoalTargetColor;
    AStarImage.picture.bitmap.Canvas.Ellipse(Rect(EnemytargetXLoc * tileSize,
      EnemytargetYLoc * tileSize, EnemytargetXLoc * tileSize + tileSize,
      EnemytargetYLoc * tileSize + tileSize));

    For X := 1 to 5 do
    Begin // Circles in Squares
      walkability[EnemyBaseXLocArray[X] div tileSize]
        [EnemyBaseYLocArray[X] div tileSize] := walkable;

      AStarImage.picture.bitmap.Canvas.Brush.Color := EnemyPositionsColor;
      // Base is Square..Make ? smaller than Block
      AStarImage.picture.bitmap.Canvas.Rectangle
        (Rect(EnemyBaseXLocArray[X] * tileSize, EnemyBaseYLocArray[X] *
        tileSize, EnemyBaseXLocArray[X] * tileSize + tileSize,
        EnemyBaseYLocArray[X] * tileSize + tileSize));
    end;
  end;
End;
// -----------------------------------------------------------------------------
// Name: LoadUnitData
// Desc: Initialize unit-related data
// -----------------------------------------------------------------------------

procedure TAStarForm.LoadUnitData;
Begin
  ActiveEnemyNumber := 0;
  Case AstarMode of
    1: // CubeDataDisplay1
      Begin
        NumberofUnits := 1;
        ActiveUnitNumber := 1;
        startXLocArray[ActiveUnitNumber] := // 3;
          Trunc(((3) / 800) * ImageWidth);
        startYLocArray[ActiveUnitNumber] := // 6;
          Trunc(((5) / 600) * ImageHeight);
        // *50  120x240     2x5
        targetXLocArray[ActiveUnitNumber] := // 12;
          Trunc(((11) / 800) * ImageWidth);
        targetYLocArray[ActiveUnitNumber] := // 6;
          Trunc(((5) / 600) * ImageHeight);
        speedArray[ActiveUnitNumber] := 9;
      End;
    2: // Smiley1
      Begin // 800x600    ImageWidth,ImageHeight
        NumberofUnits := 1;
        ActiveUnitNumber := 1;
        startXLocArray[1] := Round((125 / 800) * ImageWidth / tileSize);
        startYLocArray[1] := Round((225 / 600) * ImageHeight / tileSize);
        // initial smiley location
        targetXLocArray[1] := Round((725 / 800) * ImageWidth / tileSize);
        targetYLocArray[1] := Round((525 / 600) * ImageHeight / tileSize);
        // initial smiley location
        speedArray[1] := 9; // smiley speed
      End;
    3: // Star
      Begin
        NumberofUnits := 5;
        ActiveUnitNumber := 1;
        startXLocArray[1] := { 46; } Round((462 / 650) * ImageWidth / tileSize);
        startYLocArray[1] := { 20; } Round((202 / 500) * ImageHeight /
          tileSize); // initial smiley location
        targetXLocArray[1] := { 19; } Round((192 / 650) * ImageWidth /
          tileSize);
        targetYLocArray[1] := { 20;// } Round((202 / 500) * ImageHeight /
          tileSize); // initial smiley location

        startXLocArray[2] := { 19; } Round((192 / 650) * ImageWidth / tileSize);
        startYLocArray[2] := { 20; } Round((202 / 500) * ImageHeight /
          tileSize); // initial chaser location
        targetXLocArray[2] := { 44; } Round((444 / 650) * ImageWidth /
          tileSize);
        targetYLocArray[2] := { 34; } Round((345 / 500) * ImageHeight /
          tileSize); // initial smiley location

        startXLocArray[3] := { 44; } Round((444 / 650) * ImageWidth / tileSize);
        startYLocArray[3] := { 34; } Round((345 / 500) * ImageHeight /
          tileSize); // initial chaser location
        targetXLocArray[3] := { 33; } Round((333 / 650) * ImageWidth /
          tileSize);
        targetYLocArray[3] := { 13; } Round((132 / 500) * ImageHeight /
          tileSize); // initial smiley location

        startXLocArray[4] := { 33; } Round((333 / 650) * ImageWidth / tileSize);
        startYLocArray[4] := { 13; } Round((132 / 500) * ImageHeight /
          tileSize); // initial chaser location
        targetXLocArray[4] := { 23; } Round((230 / 650) * ImageWidth /
          tileSize);
        targetYLocArray[4] := { 34;// } Round((340 / 500) * ImageHeight /
          tileSize); // initial smiley location
        startXLocArray[5] := { 23;// } Round((230 / 650) * ImageWidth /
          tileSize);
        startYLocArray[5] := { 34;// } Round((340 / 500) * ImageHeight /
          tileSize); // initial chaser location
        targetXLocArray[5] := { 46;// } Round((462 / 650) * ImageWidth /
          tileSize);
        targetYLocArray[5] := { 20;// } Round((202 / 500) * ImageHeight /
          tileSize); // initial smiley location

        speedArray[1] := 9; // smiley speed
        speedArray[2] := 9; // chaser
        speedArray[3] := 9; // chaser
        speedArray[4] := 9; // chaser
        speedArray[5] := 9; // chaser

      End;
    4: // King Astar : Dynamic Chase
      Begin
        NumberofUnits := 3;
        ActiveUnitNumber := 1;
        startXLocArray[1] := Round((112.5 / 800) * ImageWidth / tileSize);
        startYLocArray[1] := Round((337.5 / 600) * ImageHeight / tileSize);
        // initial smiley location
        targetXLocArray[1] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[1] := Round((525 / 600) * ImageHeight / tileSize);
        // initial smiley location

        startXLocArray[2] := Round((737.5 / 800) * ImageWidth / tileSize);
        startYLocArray[2] := Round((337.5 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[2] := Round((125 / 800) * ImageWidth / tileSize);
        targetYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[3] := Round((437.5 / 800) * ImageWidth / tileSize);
        startYLocArray[3] := Round((237.5 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[3] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[3] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        speedArray[1] := 5; // smiley speed
        speedArray[2] := 4; // chaser
        speedArray[3] := 3; // chaser
      End;
    5: // King Astar of the Misty Valley
      Begin
        NumberofUnits := 4;
        ActiveUnitNumber := 1;
        startXLocArray[1] := Round((125 / 800) * ImageWidth / tileSize);
        startYLocArray[1] := Round((225 / 600) * ImageHeight / tileSize);
        // initial smiley location
        targetXLocArray[1] := Round((725 / 800) * ImageWidth / tileSize);
        targetYLocArray[1] := Round((525 / 600) * ImageHeight / tileSize);
        // initial smiley location

        startXLocArray[2] := Round((725 / 800) * ImageWidth / tileSize);
        startYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[2] := Round((125 / 800) * ImageWidth / tileSize);
        targetYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[3] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[3] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[3] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[3] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[4] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[4] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[4] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[4] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location

        speedArray[1] := 9; // smiley speed 9..0
        speedArray[2] := 9; // chaser
        speedArray[3] := 9; // chaser
        speedArray[4] := 9; // chaser
      End;

    6: // CollisionAvoidancetimebased1
      Begin
        NumberofUnits := 5;
        ActiveUnitNumber := 1;
        startXLocArray[1] := Round((125 / 800) * ImageWidth / tileSize);
        startYLocArray[1] := Round((225 / 600) * ImageHeight / tileSize);
        // initial smiley location
        targetXLocArray[1] := Round((725 / 800) * ImageWidth / tileSize);
        targetYLocArray[1] := Round((525 / 600) * ImageHeight / tileSize);
        // initial smiley location

        startXLocArray[2] := Round((725 / 800) * ImageWidth / tileSize);
        startYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[2] := Round((125 / 800) * ImageWidth / tileSize);
        targetYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[3] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[3] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[3] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[3] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[4] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[4] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[4] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[4] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[5] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[5] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[5] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[5] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location

        speedArray[1] := 5; // smiley speed
        speedArray[2] := 4; // chaser
        speedArray[3] := 4; // chaser
        speedArray[4] := 4; // chaser
        speedArray[5] := 4; // chaser
      End;
    7: // Group Movement (w/Enemy)
      Begin
        NumberofUnits := 5;
        ActiveUnitNumber := 1;
        startXLocArray[1] := Round((125 / 800) * ImageWidth / tileSize);
        startYLocArray[1] := Round((225 / 600) * ImageHeight / tileSize);
        // initial smiley location
        targetXLocArray[1] := Round((725 / 800) * ImageWidth / tileSize);
        targetYLocArray[1] := Round((525 / 600) * ImageHeight / tileSize);
        // initial smiley location

        startXLocArray[2] := Round((725 / 800) * ImageWidth / tileSize);
        startYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[2] := Round((125 / 800) * ImageWidth / tileSize);
        targetYLocArray[2] := Round((325 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[3] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[3] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[3] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[3] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[4] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[4] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[4] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[4] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        startXLocArray[5] := Round((365 / 800) * ImageWidth / tileSize);
        startYLocArray[5] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        targetXLocArray[5] := Round((775 / 800) * ImageWidth / tileSize);
        targetYLocArray[5] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        speedArray[1] := 5; // smiley speed
        speedArray[2] := 5; // chaser
        speedArray[3] := 5; // chaser
        speedArray[4] := 5; // chaser
        speedArray[5] := 5; // chaser
        // ALWAYS 5 blocks and 1 EnemyBase..Target
        // Setting to > 1 makes for some action
        ActiveEnemyNumber := 1;
        EnemystartXLoc := Round((765 / 800) * ImageWidth / tileSize);
        EnemystartYLoc := Round((123 / 600) * ImageWidth / tileSize);
        EnemytargetXLoc := Round((123 / 800) * ImageWidth / tileSize);
        EnemytargetYLoc := Round((456 / 600) * ImageWidth / tileSize);

        EnemyBaseXLocArray[1] := Round((365 / 800) * ImageWidth / tileSize);
        EnemyBaseYLocArray[1] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        EnemyBaseXLocArray[2] := Round((775 / 800) * ImageWidth / tileSize);
        EnemyBaseYLocArray[2] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        EnemyBaseXLocArray[3] := Round((365 / 800) * ImageWidth / tileSize);
        EnemyBaseYLocArray[3] := Round((145 / 600) * ImageHeight / tileSize);
        // initial chaser location
        EnemyBaseXLocArray[4] := Round((775 / 800) * ImageWidth / tileSize);
        EnemyBaseYLocArray[4] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
        EnemyBaseXLocArray[5] := Round((775 / 800) * ImageWidth / tileSize);
        EnemyBaseYLocArray[5] := Round((425 / 600) * ImageHeight / tileSize);
        // initial smiley location
      End;
  End;
End;

procedure TAStarForm.AStarImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  StatusBar1.Panels[3].Text := 'X: ' + inttostr(X) + ' Y: ' + inttostr(Y);
  StatusBar1.Panels[4].Text := 'X: ' + inttostr((X div tileSize) + 1) + ' Y: ' +
    inttostr((Y div tileSize) + 1);
end;

procedure TAStarForm.AStarImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  colCount, xx, yy, AStarx, AStary: Integer;
  t: Int64;
  colTotalTime, ts: Double; // Single;
  // Past,Present: TDateTime;
  // Hour, Min, Sec, MSec: Word;
begin
  // All action starts at the Mouse Click
  // Depends on the Current AstarMode  Project demo type
  // Turn ON the timer which Calls The AStar Runner
  // this allows the mouse to be 'done' and still get something Activated
  AStarx := X div tileSize;
  AStary := Y div tileSize;
  If EditModeActive then
  begin
    if ssLeft in Shift then { Base }
    begin
      if IsKeyDown('O') then // ObstacleColor
      begin
        walkability[AStarx][AStary] := 1 - walkability[AStarx][AStary];
        if (walkability[AStarx][AStary] = unwalkable) then
          AStarImage.picture.bitmap.Canvas.Brush.Color := ObstacleColor
        else
          AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //
      end
      else
        // ActiveEnemyPosition
        if IsKeyDown('E') then // EnemyPositionsColor
        begin
          If ActiveEnemyNumber = 6 then
          begin // EnemystartXLoc
            EnemystartXLoc := AStarx;
            EnemystartYLoc := AStary;
          end
          else
          begin // 1..5
            EnemyBaseXLocArray[ActiveEnemyNumber] := AStarx;
            EnemyBaseYLocArray[ActiveEnemyNumber] := AStary;
            walkability[AStarx][AStary] := 1 - walkability[AStarx][AStary];
            if (walkability[AStarx][AStary] = unwalkable) then
              AStarImage.picture.bitmap.Canvas.Brush.Color :=
                EnemyPositionsColor
            else
              AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor;
            //
          end;
        end
        else
        begin
          startXLocArray[ActiveUnitNumber] := AStarx;
          startYLocArray[ActiveUnitNumber] := AStary;
          AStarImage.picture.bitmap.Canvas.Brush.Color := BaseStartColorArray
            [ActiveUnitNumber];
        end;
    end
    else if ssRight in Shift then { Target }
    begin
      if IsKeyDown('E') then // EnemyPositionsColor
      begin
        If ActiveEnemyNumber = 6 then
        begin // EnemystartXLoc
          EnemytargetXLoc := AStarx;
          EnemytargetYLoc := AStary;
        end
      end
      else
      begin
        targetXLocArray[ActiveUnitNumber] := AStarx;
        targetYLocArray[ActiveUnitNumber] := AStary;
        AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray
          [ActiveUnitNumber];
      end;
    end;
    // Fill with whatever color is Set
    If GridLinesDisplayed then
      AStarImage.picture.bitmap.Canvas.Pen.Color := GridColor
    else
      AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor; //

    AStarImage.picture.bitmap.Canvas.Rectangle(Rect(AStarx * tileSize,
      AStary * tileSize, AStarx * tileSize + tileSize,
      AStary * tileSize + tileSize));

    RedrawData;
  end
  else
  begin // Pathfinding Mode
    if ssLeft in Shift then { Base }
    begin
      if IsKeyDown('O') then // ObstacleColor
      begin
        walkability[AStarx][AStary] := 1 - walkability[AStarx][AStary];
        if (walkability[AStarx][AStary] = unwalkable) then
          AStarImage.picture.bitmap.Canvas.Brush.Color := ObstacleColor
        else
          AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor; //
      end
      else
      begin
        startXLocArray[ActiveUnitNumber] := AStarx;
        startYLocArray[ActiveUnitNumber] := AStary;
        AStarImage.picture.bitmap.Canvas.Brush.Color := BaseStartColorArray
          [ActiveUnitNumber];
      end;
      RedrawData;
    end
    else if ssRight in Shift then { Target }
    begin
      targetXLocArray[ActiveUnitNumber] := AStarx;
      targetYLocArray[ActiveUnitNumber] := AStary;
      AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray
        [ActiveUnitNumber];
    end;
    // either draws a box
    If GridLinesDisplayed then
      AStarImage.picture.bitmap.Canvas.Pen.Color := GridColor
    else
      AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor; //

    AStarImage.picture.bitmap.Canvas.Rectangle(Rect(AStarx * tileSize,
      AStary * tileSize, AStarx * tileSize + tileSize,
      AStary * tileSize + tileSize));

    RedrawData;
    // Start A* pathfinding search if Right Click is hit
    if ssRight in Shift then
    begin
      // Reset after finishing A* search if pressing mouse button or "1" or enter key
      // Meanwhile pressing 9 or 0 keys will Single Step Draw the Path
      if (pathStatus[ActiveUnitNumber] <> notfinished) then // if path is done
      // if (MouseDown(1)==1 || MouseDown(2)==1 || KeyHit(49) || KeyHit(13)) then
      begin
        for xx := 0 to mapWidth - 1 do
          for yy := 0 to mapHeight - 1 do
            whichList[xx][yy] := 0;
        for xx := 1 to 5 do
          pathStatus[xx] := notfinished;
      end;
      if (pathStatus[ActiveUnitNumber] = notfinished) then
      // if path not searched
      Begin // ActiveUnitNumber pathfinderID
        // so you know if it gets stuck
        // If you see this ? something wrong?
        StatusBar1.Panels[1].Text := 'Work';
        If (UseOriginal and (SearchMode = 3) and (TieBreakerMode = 1)) then
          StatusBar1.Panels[5].Text := 'AStar Normal'
        else
          StatusBar1.Panels[5].Text := 'AStar H: ' + inttostr(SearchMode) +
            ' : ' + inttostr(TieBreakerMode);
        Application.ProcessMessages; // to show it
        colTotalTime := 0;

        Case AstarMode of
          1 .. 2: // CubeDataDisplay1
            Begin
              colCount := 1; // NumberofUnits  for when there are many units...
              t := StartPrecisionTimer;
              // Past:=Now;
              If (UseOriginal and (SearchMode = 3) and (TieBreakerMode = 1))
              then
                pathStatus[ActiveUnitNumber] := FindPath(ActiveUnitNumber,
                  // startX*50,startY*50,targetX*50,targetY*50);
                  startXLocArray[ActiveUnitNumber] * tileSize,
                  startYLocArray[ActiveUnitNumber] * tileSize,
                  targetXLocArray[ActiveUnitNumber] * tileSize,
                  targetYLocArray[ActiveUnitNumber] * tileSize)
              else
                pathStatus[ActiveUnitNumber] := FindPathH(ActiveUnitNumber,
                  // startX*50,startY*50,targetX*50,targetY*50);
                  startXLocArray[ActiveUnitNumber] * tileSize,
                  startYLocArray[ActiveUnitNumber] * tileSize,
                  targetXLocArray[ActiveUnitNumber] * tileSize,
                  targetYLocArray[ActiveUnitNumber] * tileSize);
              // Present:=Now;
              // DecodeTime(Present-Past, Hour, Min, Sec, MSec);
              colTotalTime := colTotalTime + StopPrecisionTimer(t);
              // don't highlight the start square (aesthetics)
              whichList // [startX][startY]
                [startXLocArray[ActiveUnitNumber],
                startYLocArray[ActiveUnitNumber]] := 0;
              ts := colTotalTime * 1000 / colCount;
              StatusBar1.Panels[1].Text := // IntToStr(MSec)+' ms';
                Format('%.3f ms', [ts]);
            end;
          3 .. 6: // Smiley and Chasers
            Begin // 5;
              colCount := NumberofUnits; // for when there are many units...
              t := StartPrecisionTimer;
              // Past:=Now;
              for xx := 1 to 5 do
              begin
                If (UseOriginal and (SearchMode = 3) and (TieBreakerMode = 1))
                then
                  pathStatus[xx] := FindPath(xx,
                    // startX*50,startY*50,targetX*50,targetY*50);
                    startXLocArray[xx] * tileSize,
                    startYLocArray[xx] * tileSize, targetXLocArray[xx] *
                    tileSize, targetYLocArray[xx] * tileSize)
                else
                  pathStatus[xx] := FindPathH(xx,
                    // startX*50,startY*50,targetX*50,targetY*50);
                    startXLocArray[xx] * tileSize,
                    startYLocArray[xx] * tileSize, targetXLocArray[xx] *
                    tileSize, targetYLocArray[xx] * tileSize);
              end;
              // Present:=Now;
              // DecodeTime(Present-Past, Hour, Min, Sec, MSec);
              colTotalTime := colTotalTime + StopPrecisionTimer(t);
              ts := colTotalTime * 1000 / colCount;
              StatusBar1.Panels[1].Text := // IntToStr(MSec)+' ms';
                Format('%.3f / %d = %.3f ms',
                [colTotalTime * 1000, colCount, ts]);
            end;

        End; // Case;
      end;
      // This will call the object movements
      Timer1.Enabled := True;
    end;
  end;
end;

procedure TAStarForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  RunAStarProject;
end;

// Called when a number 9 or 0 is presssed
procedure TAStarForm.DoSingleStepDrawing(Step: Integer);
Begin // DoSingleStepDrawing(0);
  If (pathStatus[ActiveUnitNumber] = found) then
  begin
    If Step = 0 then
      RunAStarProject
    else // Do All the rest
      If Step = 9 then // RunASingleStep; //1
      begin
        // Draw 1 Step.. MUST keep track of Current X and Y
        // If the First time then Draw the Base stuff.

        // procedure TAStarForm.PaintAStarPath(X,Y:Integer);
        // procedure TAStarForm.PaintLinePath(X,Y:Integer);
      end;
  end;
End;

procedure TAStarForm.RunAStarProject;
var
  // x,y, Z,ID,
  i: Integer;
  Procedure Runit(ID: Integer);
  var
    ii, xx: Integer;
  Begin
    If PathDisplayed then
      AStarImage.picture.bitmap.Canvas.Pen.Color := TargetGoalColorArray[ID]
    else
      AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor;
    // The Findpath calls ReadPath the does +1 so Have to reset back 1
    // pathLocation[ID] := (pathLocation[ID] - 1);
    For ii := 0 to pathLength[ID] - 1 do
    begin
      pathLocation[ID] := (pathLocation[ID] + 1);
      xPath[ID] := ReadPathX(ID, pathLocation[ID]);
      yPath[ID] := ReadPathY(ID, pathLocation[ID]);
      AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray[ID];
      AStarImage.picture.bitmap.Canvas.Ellipse
        (Rect(xPath[ID] + (2), yPath[ID] + (2), xPath[ID] + tileSize - (2),
        yPath[ID] + tileSize - (2)));
      // Move the Sprites IAW their Speed
      // Call ____ times to allow time to SEE the Sprite move...
      For xx := 0 to 1000000 * (10 - speedArray[ID]) do
        Application.ProcessMessages;
      // Paint over the Dot to 'erase'
      AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor;
      AStarImage.picture.bitmap.Canvas.Ellipse
        (Rect(xPath[ID] + (2), yPath[ID] + (2), xPath[ID] + tileSize - (2),
        yPath[ID] + tileSize - (2)));
    end;
    // Change locations to have it go from present to next
    If AstarMode = 2 then
    begin
      startXLocArray[1] := xPath[1] div tileSize;
      startYLocArray[1] := yPath[1] div tileSize;
      targetXLocArray[1] := xPath[1] div tileSize;
      targetYLocArray[1] := yPath[1] div tileSize;
    end;
    // Repaint Last dot.. since it was 'erased'
    AStarImage.picture.bitmap.Canvas.Brush.Color := TargetGoalColorArray[ID];
    AStarImage.picture.bitmap.Canvas.Ellipse
      (Rect(xPath[ID] + (2), yPath[ID] + (2), xPath[ID] + tileSize - (2),
      yPath[ID] + tileSize - (2)));
  End;
  Procedure RunAll;
  var
    AllID, xx: Integer;
    a1, a2, a3, a4, a5: Boolean;
  Begin
    a1 := True; // False  True
    a2 := True;
    a3 := True;
    a4 := True;
    a5 := True;
    // The Findpath calls ReadPath the does +1 so Have to reset back 1
    For AllID := 1 to NumberofUnits Do
    begin
      // pathLocation[AllID] := (pathLocation[AllID] - 1);
      Case AllID of
        1:
          a1 := False;
        2:
          a2 := False;
        3:
          a3 := False;
        4:
          a4 := False;
        5:
          a5 := False;
      end;
    end;
    Repeat
    Begin
      For AllID := 1 to NumberofUnits Do
      Begin
        StatusBar1.Panels[5].Text := '';
        If PathDisplayed then
          AStarImage.picture.bitmap.Canvas.Pen.Color :=
            TargetGoalColorArray[AllID]
        else
          AStarImage.picture.bitmap.Canvas.Pen.Color := BackGroundColor;
        // If  NONE set it back False..Then All done...
        // For Ii:= 0 to pathLength[AllID]-1 do
        If (xPath[AllID] div tileSize) <> (targetXLocArray[AllID]) then
        begin
          StatusBar1.Panels[5].Text := 'Processing: ' + inttostr(AllID);
          pathLocation[AllID] := (pathLocation[AllID] + 1);
          xPath[AllID] := ReadPathX(AllID, pathLocation[AllID]);
          yPath[AllID] := ReadPathY(AllID, pathLocation[AllID]);
          AStarImage.picture.bitmap.Canvas.Brush.Color :=
            TargetGoalColorArray[AllID];
          AStarImage.picture.bitmap.Canvas.Ellipse
            (Rect(xPath[AllID] + (2), yPath[AllID] + (2),
            xPath[AllID] + tileSize - (2), yPath[AllID] + tileSize - (2)));
          // Move the Sprites IAW their Speed
          // Call ____ times to allow time to SEE the Sprite move...
          For xx := 0 to 1000000 * (10 - speedArray[AllID]) do
            Application.ProcessMessages;
          // Paint over the Dot to 'erase'
          AStarImage.picture.bitmap.Canvas.Brush.Color := BackGroundColor;
          AStarImage.picture.bitmap.Canvas.Ellipse
            (Rect(xPath[AllID] + (2), yPath[AllID] + (2),
            xPath[AllID] + tileSize - (2), yPath[AllID] + tileSize - (2)));

          // Repaint Last dot.. since it was 'erased'
          AStarImage.picture.bitmap.Canvas.Brush.Color :=
            TargetGoalColorArray[AllID];
          AStarImage.picture.bitmap.Canvas.Ellipse
            (Rect(xPath[AllID] + (2), yPath[AllID] + (2),
            xPath[AllID] + tileSize - (2), yPath[AllID] + tileSize - (2)));
        end
        else
        begin
          Case AllID of
            1:
              a1 := True;
            2:
              a2 := True;
            3:
              a3 := True;
            4:
              a4 := True;
            5:
              a5 := True;
          end;
        end;
      End;
    End;
    Until ((a1 = True) and (a2 = True) and (a3 = True) and (a4 = True) and
      (a5 = True));
    StatusBar1.Panels[5].Text := 'All Paths Finished';
  End;

begin
  { CubeDataDisplay1
    GOGO
    star
    King valley
    King chase
    CollisionAvoidancetimebased1
    GroupMovement1 }
  { If (path = found)then ReadPath(ActiveUnitNumber,
    startXLocArray[ActiveUnitNumber],
    startYLocArray[ActiveUnitNumber],1 );
  }
  Case AstarMode of
    1: // CubeDataDisplay1
      Begin
        If (pathStatus[1] = found) then
        begin
          Memo1.Enabled := True;
          Memo1.Lines.Clear;
          Memo1.Lines.Add('(path = found)');
          If pathLength[1] > 0 then // showmessage(
            Memo1.Lines.Add('Location ' + inttostr(pathLocation[1]));
          Memo1.Lines.Add('Length ' + inttostr(pathLength[1]));
          // ID:=1;
          Memo1.Lines.Add(inttostr(startXLocArray[1]) + ' start x..y ' +
            inttostr(startYLocArray[1]));
          // Memo1.Lines.Add('onClosedList: '+inttostr(onClosedList));
          // The Findpath calls ReadPath the does +1 so Have to reset back 1
          // pathLocation[1] := (pathLocation[1] - 1);
          For i := 0 to pathLength[1] - 1 do
          begin
            pathLocation[1] := (pathLocation[1] + 1);
            xPath[1] := ReadPathX(1, pathLocation[1]);
            yPath[1] := ReadPathY(1, pathLocation[1]);
            Memo1.Lines.Add(inttostr(xPath[1] div tileSize) + '..' +
              inttostr(yPath[1] div tileSize));
            // Add 1 to make the Path get Displayed different than Closedlist
            whichList[xPath[1] div tileSize][yPath[1] div tileSize] :=
              whichList[xPath[1] div tileSize][yPath[1] div tileSize] + 1;
          end;
          { Z:= onClosedList-2;
            For X:= 0 to mapWidth-1 do
            For Y:= 0 to mapHeight-1 do
            Memo1.Lines.Add(inttostr(whichList[x][y])+ '..-z '
            + inttostr((whichList[x][y])-Z) ); }
          Memo1.Lines.Add(inttostr(targetXLocArray[1]) + '..' +
            inttostr(targetYLocArray[1]));
          Memo1.Lines.Add('eol');
          // Draw the Path  ,startXLocArray[ID],startYLocArray[ID]
          PaintAStarPath(1);
        end;
      End;
    2: // one after another
      Begin
        // ID:=1;
        Runit(1);
      End;
    3: // Star
      Begin
        RunAll; // for I:= 1 to 5 do  Runit(I);
      End;
    // Blitz code projects
    4: // King  chase  [3]
      Begin
        FPSCount := 0;
        // for I:= 1 to 3 do  Runit(I);
        // AstarUnitsRunning:=True;
        // RunUnitLoop;// until ???? [Ctrl] F9 Toggles AstarUnitsRunning
      End;
    5: // King valley   [4]
      Begin
        // AstarUnitsRunning:=True;
        // RunUnitLoop;// until ???? [Ctrl] F9 Toggles AstarUnitsRunning
      End;
    6: // CollisionAvoidance [5]
      Begin
        //
      End;
    7: // GroupMovement1   [5+1=6]
      Begin
        //
        // ;Draw unit claimed nodes
        // DrawClaimedNodes(True);see shared functions.bb include file
      End;
  End;
end;

procedure TAStarForm.PaintAStarPath(ID: Integer);
var
  dx1, dx2, dy1, dy2, cross, // Tiebreakers
  XDistance, YDistance, HDistance, // Heuristic options
  G, F, H, X, Y, z: Integer;
  Procedure DrawImage(Image, X, Y: Integer);
  begin
    // Dta Display Demo IGNORES the Base .. Target Colors
    Case Image of // 123: Draw a Color Rectangle
      1:
        begin
          AStarImage.picture.bitmap.Canvas.Brush.Color := clGreen;
          AStarImage.picture.bitmap.Canvas.FrameRect
            (Rect(X, Y, X + tileSize, Y + tileSize));
        end;
      2:
        begin
          AStarImage.picture.bitmap.Canvas.Brush.Color := clBlue;
          AStarImage.picture.bitmap.Canvas.FrameRect
            (Rect(X, Y, X + tileSize, Y + tileSize));
        end;
      3:
        begin // Paint the Base Red Dot;
          AStarImage.picture.bitmap.Canvas.Brush.Color := clYellow;
          AStarImage.picture.bitmap.Canvas.FrameRect
            (Rect(X, Y, X + tileSize, Y + tileSize));
          AStarImage.picture.bitmap.Canvas.Brush.Color := clRed;
          AStarImage.picture.bitmap.Canvas.Ellipse
            (Rect(X + (12), Y + (12), X + tileSize - (12),
            Y + tileSize - (12)));
          AStarImage.picture.bitmap.Canvas.Brush.Color := clYellow;
        End;
    end; // case
    Case Image of
      10 .. 17:
        begin // Draw the ImageList image  directional arrows
          If AStarDataDisplayArrowsColor = 0 then
            ImageList0.Draw(AStarImage.picture.bitmap.Canvas, X + (12),
              Y + (12), Image - 10)
          else
            ImageList1.Draw(AStarImage.picture.bitmap.Canvas, X + (12),
              Y + (12), Image - 10);
        end;
    end; // case
  end; // DrawImage

Begin
  // Cloned from RenderScreen
  // onClosedList := onClosedList+2;
  // onOpenList := onClosedList-1;
  z := onClosedList - 2;
  For X := 0 to mapWidth - 1 do
    For Y := 0 to mapHeight - 1 do
    Begin
      // Draw cells on open list (green), closed list (blue) and
      // part of the found path, if any (red dots).
      if ((whichList[X][Y]) - z = 1) then
        DrawImage(1 { greengrid } , X * 50, Y * 50)
      else if ((whichList[X][Y]) - z = 2) then
        DrawImage(2 { bluegrid } , X * 50, Y * 50)
      else if ((whichList[X][Y]) - z = 3) then
        DrawImage(3 { dottedPath } , X * 50, Y * 50);

      // Draw arrows that point to each square's parent.
      if (whichList[X][Y] - z = 1) or (whichList[X][Y] - z = 2) then
      begin
        If (parentX[X][Y] = X) and (parentY[X][Y] = Y - 1) then
          DrawImage(10 { parentArrow[1] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X + 1) and (parentY[X][Y] = Y - 1) then
          DrawImage(11 { parentArrow[2] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X + 1) and (parentY[X][Y] = Y) then
          DrawImage(12 { parentArrow[3] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X + 1) and (parentY[X][Y] = Y + 1) then
          DrawImage(13 { parentArrow[4] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X) and (parentY[X][Y] = Y + 1) then
          DrawImage(14 { parentArrow[5] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X - 1) and (parentY[X][Y] = Y + 1) then
          DrawImage(15 { parentArrow[6] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X - 1) and (parentY[X][Y] = Y) then
          DrawImage(16 { parentArrow[7] } , X * 50, Y * 50)
        else if (parentX[X][Y] = X - 1) and (parentY[X][Y] = Y - 1) then
          DrawImage(17 { parentArrow[8] } , X * 50, Y * 50);
      end;

      // Print F, G and H costs in cells
      if (whichList[X][Y] - z > 0) then
      // if an open, closed, or path cell.
      begin
        G := Gcost[X][Y];
        H := 0; // compiler shutup
        // Heuristic Options
        // H := 10*(abs(x - targetXLocArray[ID]) + abs(y - targetYLocArray[ID]));
        Case SearchMode of
          0:
            H := 0; // Dijkstra
          1: // Diagonal
            Begin
              XDistance := abs(X - targetXLocArray[ID]);
              YDistance := abs(Y - targetYLocArray[ID]);
              If XDistance > YDistance then
                HDistance := ((14 * YDistance) + (10 * (XDistance - YDistance)))
              else
                HDistance :=
                  ((14 * XDistance) + (10 * (YDistance - XDistance)));
              H := HDistance;
            End;
          2: // Manhattan
            H := 10 * (abs(X - targetXLocArray[ID]) +
              abs(Y - targetYLocArray[ID]));
          3: // BestFirst..Overestimated h
            H := 10 * ((X - targetXLocArray[ID]) * (X - targetXLocArray[ID]) +
              (Y - targetYLocArray[ID]) * (Y - targetYLocArray[ID]));
          4: // Euclidian
            H := Trunc
              (10 * SQRT(((X - targetXLocArray[ID]) * (X - targetXLocArray[ID])
              + (Y - targetYLocArray[ID]) * (Y - targetYLocArray[ID]))));

        End;
        F := G + H; // compiler shutup
        Case TieBreakerMode of
          0:
            F := G + H; // None
          1:
            begin // straight
              dx1 := (X - targetXLocArray[ID]);
              dx2 := (Y - targetYLocArray[ID]);
              dy1 := (startXLocArray[ID] - targetXLocArray[ID]);
              dy2 := (startYLocArray[ID] - targetYLocArray[ID]);
              cross := abs((dx1 * dy2) - (dx2 * dy1));
              // or Trunc ?
              F := Round(G + H + (cross * 0.001));
            end;
          2:
            F := Trunc(G + H - (0.1 * H)); // close
          3:
            F := Trunc(G + H + (0.1 * H)); // far
        end;

        // Print F in upper left corner of cell (F=G+H)
        AStarImage.picture.bitmap.Canvas.textout(X * tileSize + 2,
          Y * tileSize + 2, inttostr(F));
        // Print G in bottom left corner of cell (G=distance from start)
        AStarImage.picture.bitmap.Canvas.textout(X * tileSize + 2,
          Y * tileSize + 36, inttostr(G));
        // Print H in bottom right corner of cell (H=distance to target)
        if (H < 100) then
          AStarImage.picture.bitmap.Canvas.textout(X * tileSize + 37,
            Y * tileSize + 36, inttostr(H))
        else
          AStarImage.picture.bitmap.Canvas.textout((X * tileSize) + 30,
            Y * tileSize + 36, inttostr(H));
      end; // 32
    End;
End;

end.
