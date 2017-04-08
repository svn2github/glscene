unit AStarGlobals;

interface

uses
  Windows,
  Classes,{TStringList}
  FileCtrl, Sysutils{Filestuff},
    GLUtils,//Strtofloatdef
  Dialogs{MessageDlg},
  Shellapi,{ShellExecute}
  Forms,{App}
  Graphics{Graphics};

  //const  //x:type=value is like an initialized variable
  const  // path-related constants

  //path is Checked Against the following:
  //They Are constant..this is Set to one of their values
  //path: Integer = 0; // replaced by pathStatus array
  //pathStatus constants
  notStarted: Integer = 0;
  notfinished: Integer = 0;  //not used?
  found: Integer = 1;
  nonexistent: Integer = 2;
  tempStopped: Integer = 3;  //not H   claimedNode
  stopped: Integer = 3;      //not H   UpdatePath
  Targetunwalkable : Integer =4;
  RedirectFailed : Integer =5;
  //CheckRedirect returns these constants
  CheckRedirectSucceeded: Integer =1;
  CheckRedirectFailed: Integer = -1;

  //BlitzFindPath() mode constants	Const
  normal: Integer = 0;
  randomMove: Integer = 1;
  PatrolMove: Integer = 2;  //Not Implemented yet
  
  // walkability array constants  onOpenList := onClosedList-1;
  onClosedList : Integer = 10;
  walkable: Integer = 0;
  unwalkable: Integer = 1;
  tempUnwalkable: Integer = 1;  //tempUnwalkable := onClosedList-2;
     //PenalizeAdjacentPaths
  penalized: Integer = 1;       //penalized := onClosedList-3;

  var
  mapWidth,mapHeight,
  tileSize, ImageWidth, ImageHeight:Integer;

  //Create needed arrays : SOME are Word to save memory
  //1 dimensional array holding ID# of open list items
  openList,//:array of Integer;//[mapWidth*mapHeight+2];
  //1d array stores the x location of an item on the open list
  openX,//:array of Integer;//[mapWidth*mapHeight+2];
  //1d array stores the y location of an item on the open list
  openY//:array of Integer;//[mapWidth*mapHeight+2];
  //1d array to store F cost of a cell on the open list
       :array of Word; //Word is smaller than  Integer


  walkability: array of array of Byte;//[mapWidth][mapHeight];
  //These (_Cost) Must be Integer..
  //they can be Negative?.. Straight Tiebreaker goes bonkers
  Fcost,//:array of Integer;//[mapWidth*mapHeight+2];
  //1d array to store H cost of a cell on the open list
  Hcost:array of Integer;//[mapWidth*mapHeight+2];
  //2d array to store G cost for each cell.
  Gcost: array of array of Integer;//[mapWidth+1][mapHeight+1];

  //2 dimensional array used to record
  //whether a cell is on the open list or on the closed list.
  whichList,//: array of array of Integer;//[mapWidth+1][mapHeight+1];
  //2d array to store parent of each cell (x)
  parentX,//: array of array of Integer;//[mapWidth+1][mapHeight+1];
  //2d array to store parent of each cell (y)
  parentY//: array of array of Integer;//[mapWidth+1][mapHeight+1];
        :array of array of Word;

//Units : Blitz only
  MapAttribute,//:array of array of Integer;
  GCostData//:array of array of Integer;
        : array of array of Byte;
  //;array that holds info about adjacent units
  tempUnwalkability,//:array of array of Integer;//(mapWidth+1,mapHeight+1)
  nearByPath,//:array of array of Integer;//(mapWidth+1,mapHeight+1)
  //; array that stores claimed nodes
  claimedNode,//:array of array of Integer;//.unit(mapWidth+1,mapHeight+1)
  //SetLength in IdentifyIslands
  island:array of array of Word;//Integer;//(mapWidth+1,mapHeight+1)
  //Set by Program...Destroyed Setlength 0 in App Destroy
  MAGColorValueArray:  Array of Array[1..2] of Integer;

  //THESE are DEMO ONLY : Units have their OWN
  //stores length of the found path for critter
  pathLength:array of Integer;//[numberPeople+1];
  //stores current position along the chosen path for critter
  pathLocation:array of Integer;//[numberPeople+1];
  //Path reading variables
  pathStatus:array of Integer;//[numberPeople+1];
  xPath:array of Integer;//[numberPeople+1];
  yPath:array of Integer;//[numberPeople+1];
  //int*   // [numberPeople+1] [xy pairs stored  ]
  //Unit,X and Y Path Locations

  pathBank:array of array of Integer;//Setlength in FindPath


  //New stuff
  gGameStarted,
  gDiagonalBlockage:Boolean;
  gScreenCaptureNumber, gLoops,
  gInt1, gInt2:Integer; //global variables.
  //gPathCost:Integer;//  PathCost:Integer;//
  savedClockTime:array [0..20]of Double;
  gLoopCount,savedClockCount :Integer;
  //gStartTime:=StartPrecisionTimer;
  gStartTime : Int64;
  //gLoopTime:=gLoopTime+StopPrecisionTimer(gStartTime);
  gLoopTime,
  //gGameTime:=gLoopTime*1000/gLoopCount;
  gGameTime:Double;

  //Mixed or Demo only?
  EnemyPositionsColor,
  EnemyUnitBaseColor,  EnemyUnitGoalTargetColor:Integer;
  BaseStartColorArray, TargetGoalColorArray:  Array [1..5]of Integer;
  //1 Enemy Base..Target .. 5 Bases,  5 Units Base..Target
  EnemystartXLoc,EnemystartYLoc,
  EnemytargetXLoc,EnemytargetYLoc:Integer;
  ActiveEnemyNumber:Integer;
  EnemyBaseXLocArray, EnemyBaseYLocArray:  Array [1..5]of Integer;
  ActiveUnitNumber, NumberofUnits, NumberofUnitPeople:Integer;
  startXLocArray,startYLocArray, //Set seeker location
  targetXLocArray,targetYLocArray,//Set initial target location.
  speedArray :  Array [1..5]of Integer;

Type ProjectRecordData = record             //IsEnemyPatrolActive,
  NumberofActiveUnits, NumberofActiveEnemy, NumberofActiveEnemyPatrol,
  RatioSizeMode, RatioSize, TileSizeMode, tileSize,
  HeightfieldSizeMode, HeightfieldSize,
  mapWidth,mapHeight,
  ImageSizeMode, ImageWidth, ImageHeight:Integer;
  GridLinesDisplayed, PathDisplayed,
//  UseTerrainValueAlpha, UseTurningpenalty,PenalizeAdjacentPaths
  ProcessNoGoIslands, ClaimedNodesDisplayed:Boolean;
  AdjacentPathPenaltyD,
  TerrainMagNoGoD, TerrainValueAlphaD,
  AverageTerrainD, TurningpenaltyD:Double;
  //Filenames
  HeightDataFile,ImageTextureFile,MapAttributesFile,
  ElevationFile, VegetationFile, SoilsFile, HydrologyFile,
  TransportFile, UrbanFile, NOGOFile, ObstaclesFile,
  InfluenceMapFile, GCostFile, MAGValuesFile,
  //ColorsFile,
  UnitsFile,TerrainFile:String;
End;   //Trunc(20*AdjacentPathPenaltyD)
var ProjectRecord: ProjectRecordData;

//pathBank1, pathBank2,:CurrentpathBank, Unit,[pathX pathY]
UnitpathBank:Array[1..2]of Array of array of Integer;//Setlength in FindPath

Type UnitRecord = record
  CurrentpathBank, pathStatus, pathLength, pathLocation,
  targetunit, unitCollidingWith:Integer;
  distanceToNextNode:Double;
  startNewPath:Boolean; //;used for delayed-action pathfinding

//BaseColor,targetColor,
{sprite, red, green, blue, selected,}
xDistanceFromGroupCenter, yDistanceFromGroupCenter,
actualAngleFromGroupCenter, assignedAngleFromGroupCenter:Integer;
xPath, yPath,  //;in pixels
startXLoc, startYLoc, startZLoc,//xLoc, yLoc,
targetX, targetY, targetZ:Integer;
Md2WpnName,Md2WpnTextureName,
Md2Name,Md2TextureName,FlagFile:String;
TargetSize,ActorScale:Double;
//;speed is in pixels/second
ID, pathAI,
OperatingOrders, SearchMode,   TieBreakerMode,
speed, Manueverability, armor,
Members, Members1X, Members1Y, Members2X, Members2Y,
Members3X, Members3Y,Members4X, Members4Y,Members5X, Members5Y
:Integer;
End;                     //5Units + 1 Enemy
var UnitRecordArray: Array [1..6]of UnitRecord;
(*
Type unit
	Field ID, xLoc#, yLoc#, speed# ;speed is in pixels/second
	Field pathAI, pathStatus, pathLength, pathLocation
	Field pathBank, pathBank1, pathBank2
	Field xPath#, yPath#, distanceToNextNode# ;in pixels
	Field targetX#, targetY#, target.unit, unitCollidingWith.unit
	Field startNewPath ;used for delayed-action pathfinding
	Field sprite, red, green, blue
	Field selected
	Field xDistanceFromGroupCenter#, yDistanceFromGroupCenter#
	Field actualAngleFromGroupCenter, assignedAngleFromGroupCenter
End Type
*)

type
  PrefRecord = record
  //PMAGValuesFileName, PGCostFileName,
  PProjectDirectory: string[255];
  PSplashScreenDisplayed,
  PAutoloadObstacleFile,

  PGridLinesDisplayed, PPathDisplayed:Boolean;
  PAProjectOptionsFormX,PAProjectOptionsFormY,
  PAStarFormX,PAStarFormY,
  PATerrainFormX,PATerrainFormY,
  PAStarAboutFormX,PAStarAboutFormY,
  PAGr32ViewerFormX,PAGr32ViewerFormY,
  PAProjectMapMakerFormX,PAProjectMapMakerFormY,
  PfrmLandscapeX,PfrmLandscapeY,
  PSearchMode, PTieBreakerMode,
    PAStarDataDisplayArrowsColor,
    PEnemyPositionsColor,   PEnemyUnitBaseColor,
    PEnemyUnitGoalTargetColor,
    PBackGroundColor, PGridColor, PPathColor, PObstacleColor,
    PAstarMode,PImageSizeMode,PTileSizeMode:Integer;
    PActiveUnitNumber,PNumberofUnits,PNumberofUnitPeople,
    PmapWidth,PmapHeight:Integer;
    PtileSize, PImageWidth,  PImageHeight:Integer;
    PBaseStartColorArray:  Array [1..5]of Integer;
    PTargetGoalColorArray:  Array [1..5]of Integer;
  end;
  PrefFile = file of PrefRecord;


var
  PreRcd: PrefRecord;
  //Global 'temp' data
  AstarUnitsRunning,
  EditModeActive, LostinaLoop:Boolean;
  ProjectFilename:String;
  //POF file: Stored data    ProgramPath:String;
//  MAGValuesFileName, GCostFileName,
  ProjectDirectory:String;
  SplashScreenDisplayed,
  AutoloadObstacleFile:Boolean;
  GridLinesDisplayed, PathDisplayed:Boolean;
  AProjectOptionsFormX,AProjectOptionsFormY,
  AStarFormX,AStarFormY,
  ATerrainFormX,ATerrainFormY,
  AStarAboutFormX,AStarAboutFormY,
  AGr32ViewerFormX,AGr32ViewerFormY,
  AProjectMapMakerFormX,AProjectMapMakerFormY,
  frmLandscapeX,frmLandscapeY:Integer;
  SearchMode,   TieBreakerMode,
  AStarDataDisplayArrowsColor,
  GridColor,  ObstacleColor, BackGroundColor, PathColor,
  AstarMode,ImageSizeMode,TileSizeMode:Integer;



Procedure InitializePathfinder;
Procedure EndPathfinder;
function ExecuteFile(const FileName, Params, DefaultDir: string;
                     ShowCmd: Integer): THandle;
procedure LoadProjectFile(Filename:String);
procedure LoadColorsFile(Filename:String);
procedure LoadUnitsFile(Filename:String);
procedure LoadTerrainFile(Filename:String);

procedure  ResetDefaults;
procedure  LoadGCostValues;
procedure  SaveGCostValues(mapWidth,mapHeight:Integer);
procedure  ResetMAGDefaults;
procedure  LoadMAGValues;
procedure  SaveMAGValues;
procedure DoLoader;
procedure SetPreferences;
procedure DoSaver;
procedure GetPreferences;

implementation


//---------------------------------------------------------------------------
// Name: InitializePathfinder
// Desc: Allocates memory for the pathfinder.
//--------------------------------------------------------------------------
Procedure InitializePathfinder;
Begin
  //Create needed arrays
  SetLength(walkability,mapWidth,mapHeight);//[mapWidth][mapHeight];
  SetLength(openList,mapWidth*mapHeight+2);//[mapWidth*mapHeight+2];
  //whether a cell is on the open list or on the closed list.
  SetLength(openX,mapWidth*mapHeight+2);//[mapWidth*mapHeight+2];
  SetLength(openY,mapWidth*mapHeight+2);//[mapWidth*mapHeight+2];
  SetLength(whichList,mapWidth+1,mapHeight+1);//[mapWidth+1][mapHeight+1];
  SetLength(parentX,mapWidth+1,mapHeight+1);//[mapWidth+1][mapHeight+1];
  SetLength(parentY,mapWidth+1,mapHeight+1);//[mapWidth+1][mapHeight+1];
  SetLength(Fcost,mapWidth*mapHeight+2);//[mapWidth*mapHeight+2];
  SetLength(Hcost,mapWidth*mapHeight+2);//[mapWidth*mapHeight+2];
  SetLength(Gcost,mapWidth+1,mapHeight+1);//[mapWidth+1][mapHeight+1];
//Demo only   //Path reading variables
 // [numberPeople+1] [xy pairs]
  SetLength(pathBank,NumberofUnits+1);
  SetLength(pathLength,NumberofUnits+1);//[numberPeople+1];
  SetLength(pathLocation,NumberofUnits+1);//[numberPeople+1];
  SetLength(pathStatus,NumberofUnits+1);//[numberPeople+1];
  SetLength(xPath,NumberofUnits+1);//[numberPeople+1];
  SetLength(yPath,NumberofUnits+1);//[numberPeople+1];
end;


//---------------------------------------------------------------------------
// Name: EndPathfinder
// Desc: Frees memory used by the pathfinder.
//---------------------------------------------------------------------------
Procedure EndPathfinder;
Begin
  //Create needed arrays
  SetLength(walkability,0);
  SetLength(openList,0);
  SetLength(openX,0);
  SetLength(openY,0);
  SetLength(whichList,0);
  SetLength(parentX,0);
  SetLength(parentY,0);
  SetLength(Fcost,0);
  SetLength(Gcost,0);
  SetLength(Hcost,0);
  //for (int x = 0; x < numberPeople+1; x++)
  //Path reading variables for Demos
  SetLength(pathBank,0);
  SetLength(pathLength,0);
  SetLength(pathLocation,0);
  SetLength(pathStatus,0);
  SetLength(xPath,0);
  SetLength(yPath,0);
  //Should be zeroed by using Unit form
  SetLength(island,0);
  SetLength(claimedNode,0);
  SetLength(nearByPath,0);
  SetLength(tempUnwalkability,0);
  SetLength(GCostData,0);
  SetLength(MapAttribute,0);
  SetLength(UnitpathBank[1],0);
  SetLength(UnitpathBank[2],0);
  //UnitpathBank:Array[1..2]of Array of array of Integer;//Setlength in FindPath
end;


function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..255] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

procedure LoadProjectFile(Filename:String);
var
  F: TextFile;
  S: string;
  Version:Integer;
Const NoName='NoName';
Begin
  // Load Base data  THEN THE SECTION FILENAMES:
  //Display Color,Units,Terrain File
  //(Each SECTION Loads their OWN stuff)
    AssignFile(F, FileName);
    Reset(F);
    Readln(F,S);If S='AStar Project File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);
      //Read the Base Information
      Readln(F,S);
      ProjectRecord.HeightfieldSizeMode:=Strtoint(S);

  case ProjectRecord.HeightfieldSizeMode of
  0:ProjectRecord.HeightfieldSize:=256;
  1:ProjectRecord.HeightfieldSize:=512;
  2:ProjectRecord.HeightfieldSize:=1024;
  end;
      Readln(F,S);
      ProjectRecord.RatioSizeMode:=Strtoint(S);

  case ProjectRecord.RatioSizeMode of
  0:ProjectRecord.RatioSize:=1;
  1:ProjectRecord.RatioSize:=2;
  2:ProjectRecord.RatioSize:=4;
  3:ProjectRecord.RatioSize:= 8;
  end;
      Readln(F,S);
      ProjectRecord.ImageSizeMode:=Strtoint(S);

      //Case into ImageWidth, ImageHeight
  case ProjectRecord.ImageSizeMode of
  0:begin ProjectRecord.ImageWidth:=256;  ProjectRecord.ImageHeight:=256;  End;
  1:begin ProjectRecord.ImageWidth:=512;  ProjectRecord.ImageHeight:=512; End;
  2:begin ProjectRecord.ImageWidth:=1024;  ProjectRecord.ImageHeight:=1024;  End;
  3:begin ProjectRecord.ImageWidth:=2048;  ProjectRecord.ImageHeight:=2048;  End;
  4:begin ProjectRecord.ImageWidth:=4096;  ProjectRecord.ImageHeight:=4096;  End;
  End;//case
      Readln(F,S);
      ProjectRecord.TileSizeMode:=Strtoint(S);

      //Case into  tileSize
  case ProjectRecord.TileSizeMode of
  0:begin ProjectRecord.tileSize:=32; End;
  1:begin ProjectRecord.tileSize:=16;  End;
  2:begin ProjectRecord.tileSize:=8;  End;
  3:begin ProjectRecord.tileSize:=4;  End;
  4:begin ProjectRecord.tileSize:=2;  End;
  5:begin ProjectRecord.tileSize:=1;  End;
  end;
  ProjectRecord.mapWidth :=
    ProjectRecord.ImageWidth div ProjectRecord.tileSize;
  ProjectRecord.mapHeight :=
    ProjectRecord.ImageWidth div ProjectRecord.tileSize;
      Readln(F,S);
        ProjectRecord.TerrainValueAlphaD :=StrtoFloatdef(S);
      Readln(F,S);
        ProjectRecord.AverageTerrainD:=StrtoFloatdef(S);
      Readln(F,S);
        ProjectRecord.TerrainMagNoGoD:=StrtoFloatdef(S);
      Readln(F,S);
        ProjectRecord.AdjacentPathPenaltyD:=StrtoFloatdef(S);
      Readln(F,S);
        ProjectRecord.TurningpenaltyD:=StrtoFloatdef(S);
      Readln(F,S);
        ProjectRecord.GridLinesDisplayed:= (S='True');
      Readln(F,S);
        ProjectRecord.PathDisplayed:= (S='True');
      Readln(F,S);
        ProjectRecord.ClaimedNodesDisplayed := (S='True');
      Readln(F,S);
        ProjectRecord.ProcessNoGoIslands:= (S='True');
      //Read the Section File Names
       {Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.ColorsFile:=S;
       If Length(S) > 0 then LoadColorsFile(S); }
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.UnitsFile:=S;
       If Length(S) > 0 then LoadUnitsFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.TerrainFile:=S;
       If Length(S) > 0 then LoadTerrainFile(S);

      //Future Proofing
      If Version > 1 then
      begin
      end;

    End else showmessage('Not AStar Project File');
    CloseFile(F);
End;

procedure LoadColorsFile(Filename:String);
var
  F: TextFile;
  S: string;
  Version:Integer;
Begin
    AssignFile(F, FileName);
    Reset(F);
    Readln(F,S);If S='AStar Colors File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);

      Readln(F,S); GridColor:=Strtoint(S);
      Readln(F,S); ObstacleColor:=Strtoint(S);
      Readln(F,S); BackGroundColor:=Strtoint(S);
      Readln(F,S); PathColor:=Strtoint(S);
      Readln(F,S); AStarDataDisplayArrowsColor:=Strtoint(S);

      Readln(F,S); BaseStartColorArray[1]:=Strtoint(S);
      Readln(F,S); BaseStartColorArray[2]:=Strtoint(S);
      Readln(F,S); BaseStartColorArray[3]:=Strtoint(S);
      Readln(F,S); BaseStartColorArray[4]:=Strtoint(S);
      Readln(F,S); BaseStartColorArray[5]:=Strtoint(S);
      Readln(F,S); TargetGoalColorArray[1]:=Strtoint(S);
      Readln(F,S); TargetGoalColorArray[2]:=Strtoint(S);
      Readln(F,S); TargetGoalColorArray[3]:=Strtoint(S);
      Readln(F,S); TargetGoalColorArray[4]:=Strtoint(S);
      Readln(F,S); TargetGoalColorArray[5]:=Strtoint(S);
      Readln(F,S); EnemyPositionsColor:=Strtoint(S);
      Readln(F,S); EnemyUnitBaseColor:=Strtoint(S);
      Readln(F,S); EnemyUnitGoalTargetColor:=Strtoint(S);

      //Future Proofing
      If Version > 1 then
      begin
      end;

    End;
    CloseFile(F);
End;

procedure LoadUnitsFile(Filename:String);
var
  F: TextFile;
  S: string;
  Version:Integer;
Const NoName='NoName';
Begin
    AssignFile(F, FileName);
    Reset(F);
    Readln(F,S);If S='AStar Units File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);

      Readln(F,S);
        If  (S='True') then
        ProjectRecord.NumberofActiveEnemyPatrol:=1 else
        ProjectRecord.NumberofActiveEnemyPatrol:=0;

      Readln(F,S); //NumberofUnits:=Strtoint(S);

        ProjectRecord.NumberofActiveUnits :=
        ProjectRecord.NumberofActiveEnemyPatrol+Strtoint(S);

//Unit1
      Readln(F,S); UnitRecordArray[1].startXLoc:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].startYLoc:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].targetX:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].targetY:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].speed:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Manueverability:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].armor:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members1X:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members1Y:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members2X:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members2Y:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members3X:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members3Y:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members4X:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members4Y:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members5X:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].Members5Y:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].OperatingOrders:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].SearchMode:=Strtoint(S);
      Readln(F,S); UnitRecordArray[1].TieBreakerMode:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[1].ActorScale:=StrtoFloatdef(S);

      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
       UnitRecordArray[1].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

//Unit2
      Readln(F,S); UnitRecordArray[2].startXLoc:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].startYLoc:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].targetX:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].targetY:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].speed:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].Manueverability:=Strtoint(S);
//        Unit2ManEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].armor:=Strtoint(S);
//        Unit2ArmorEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members:=Strtoint(S);
//        Unit2MEdit.Text:=S;

      Readln(F,S); UnitRecordArray[2].Members1X:=Strtoint(S);
//        Unit2M1XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members1Y:=Strtoint(S);
//        Unit2M1YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members2X:=Strtoint(S);
//        Unit2M2XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members2Y:=Strtoint(S);
//        Unit2M2YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members3X:=Strtoint(S);
//        Unit2M3XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members3Y:=Strtoint(S);
//        Unit2M3YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members4X:=Strtoint(S);
//        Unit2M4XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members4Y:=Strtoint(S);
//        Unit2M4YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members5X:=Strtoint(S);
//        Unit2M5XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[2].Members5Y:=Strtoint(S);
//        Unit2M5YEdit.Text:=S;

      Readln(F,S); UnitRecordArray[2].OperatingOrders:=Strtoint(S);
//        Unit2OperatingOrdersRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].SearchMode:=Strtoint(S);
 //       Unit2SearchRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[2].TieBreakerMode:=Strtoint(S);
//        Unit2TieBreakerRG.ItemIndex:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAff2Edit.Text:=S;
       UnitRecordArray[2].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       TargetSizeEdit2.Text:=S;
       UnitRecordArray[2].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAmd2Edit.Text:=S;
       UnitRecordArray[2].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAjp2Edit.Text:=S;
       UnitRecordArray[2].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[2].ActorScale:=StrtoFloatdef(S);
//      Unit2ActorScaleEdit.Text:=S;
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnEdit2.Text:=S;
       UnitRecordArray[2].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnTextureEdit2.Text:=S;
       UnitRecordArray[2].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

//Unit3
      Readln(F,S); UnitRecordArray[3].startXLoc:=Strtoint(S);
//        Unit3StartXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].startYLoc:=Strtoint(S);
//        Unit3StartYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].targetX:=Strtoint(S);
//        Unit3TargetXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].targetY:=Strtoint(S);
///        Unit3TargetYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].speed:=Strtoint(S);
//        Unit3SpeedEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Manueverability:=Strtoint(S);
//        Unit3ManEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].armor:=Strtoint(S);
//        Unit3ArmorEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members:=Strtoint(S);
//        Unit3MEdit.Text:=S;

      Readln(F,S); UnitRecordArray[3].Members1X:=Strtoint(S);
//        Unit3M1XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members1Y:=Strtoint(S);
//        Unit3M1YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members2X:=Strtoint(S);
//        Unit3M2XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members2Y:=Strtoint(S);
//        Unit3M2YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members3X:=Strtoint(S);
//        Unit3M3XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members3Y:=Strtoint(S);
//        Unit3M3YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members4X:=Strtoint(S);
//        Unit3M4XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members4Y:=Strtoint(S);
//        Unit3M4YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members5X:=Strtoint(S);
//        Unit3M5XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[3].Members5Y:=Strtoint(S);
//        Unit3M5YEdit.Text:=S;

      Readln(F,S); UnitRecordArray[3].OperatingOrders:=Strtoint(S);
//        Unit3OperatingOrdersRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[3].SearchMode:=Strtoint(S);
//        Unit3SearchRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[3].TieBreakerMode:=Strtoint(S);
//        Unit3TieBreakerRG.ItemIndex:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAff3Edit.Text:=S;
       UnitRecordArray[3].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       TargetSizeEdit3.Text:=S;
       UnitRecordArray[3].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAmd3Edit.Text:=S;
       UnitRecordArray[3].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAjp3Edit.Text:=S;
       UnitRecordArray[3].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[3].ActorScale:=StrtoFloatdef(S);
//      Unit3ActorScaleEdit.Text:=S;
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnEdit3.Text:=S;
       UnitRecordArray[3].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnTextureEdit3.Text:=S;
       UnitRecordArray[3].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

//Unit4
      Readln(F,S); UnitRecordArray[4].startXLoc:=Strtoint(S);
//        Unit4StartXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].startYLoc:=Strtoint(S);
//        Unit4StartYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].targetX:=Strtoint(S);
//        Unit4TargetXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].targetY:=Strtoint(S);
//        Unit4TargetYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].speed:=Strtoint(S);
//        Unit4SpeedEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Manueverability:=Strtoint(S);
//        Unit4ManEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].armor:=Strtoint(S);
//        Unit4ArmorEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members:=Strtoint(S);
//        Unit4MEdit.Text:=S;

      Readln(F,S); UnitRecordArray[4].Members1X:=Strtoint(S);
//        Unit4M1XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members1Y:=Strtoint(S);
//        Unit4M1YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members2X:=Strtoint(S);
//        Unit4M2XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members2Y:=Strtoint(S);
//        Unit4M2YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members3X:=Strtoint(S);
//        Unit4M3XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members3Y:=Strtoint(S);
//        Unit4M3YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members4X:=Strtoint(S);
//        Unit4M4XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members4Y:=Strtoint(S);
//        Unit4M4YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members5X:=Strtoint(S);
//        Unit4M5XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[4].Members5Y:=Strtoint(S);
//        Unit4M5YEdit.Text:=S;

      Readln(F,S); UnitRecordArray[4].OperatingOrders:=Strtoint(S);
//        Unit4OperatingOrdersRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[4].SearchMode:=Strtoint(S);
//        Unit4SearchRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[4].TieBreakerMode:=Strtoint(S);
//        Unit4TieBreakerRG.ItemIndex:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAff4Edit.Text:=S;
       UnitRecordArray[4].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       TargetSizeEdit4.Text:=S;
       UnitRecordArray[4].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAmd4Edit.Text:=S;
       UnitRecordArray[4].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAjp4Edit.Text:=S;
       UnitRecordArray[4].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[4].ActorScale:=StrtoFloatdef(S);
//      Unit4ActorScaleEdit.Text:=S;
            Readln(F,S); If (S=  NoName) then S:='';
//       WpnEdit4.Text:=S;
       UnitRecordArray[4].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
 //      WpnTextureEdit4.Text:=S;
       UnitRecordArray[4].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

//Unit5
      Readln(F,S); UnitRecordArray[5].startXLoc:=Strtoint(S);
//        Unit5StartXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].startYLoc:=Strtoint(S);
//        Unit5StartYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].targetX:=Strtoint(S);
//        Unit5TargetXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].targetY:=Strtoint(S);
//        Unit5TargetYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].speed:=Strtoint(S);
//        Unit5SpeedEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Manueverability:=Strtoint(S);
//        Unit5ManEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].armor:=Strtoint(S);
//        Unit5ArmorEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members:=Strtoint(S);
//        Unit5MEdit.Text:=S;

      Readln(F,S); UnitRecordArray[5].Members1X:=Strtoint(S);
//        Unit5M1XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members1Y:=Strtoint(S);
 //       Unit5M1YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members2X:=Strtoint(S);
 //       Unit5M2XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members2Y:=Strtoint(S);
//        Unit5M2YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members3X:=Strtoint(S);
//        Unit5M3XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members3Y:=Strtoint(S);
//        Unit5M3YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members4X:=Strtoint(S);
///        Unit5M4XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members4Y:=Strtoint(S);
//        Unit5M4YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members5X:=Strtoint(S);
//        Unit5M5XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[5].Members5Y:=Strtoint(S);
//        Unit5M5YEdit.Text:=S;

      Readln(F,S); UnitRecordArray[5].OperatingOrders:=Strtoint(S);
 //       Unit5OperatingOrdersRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[5].SearchMode:=Strtoint(S);
//        Unit5SearchRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[5].TieBreakerMode:=Strtoint(S);
//        Unit5TieBreakerRG.ItemIndex:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAff5Edit.Text:=S;
       UnitRecordArray[5].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       TargetSizeEdit5.Text:=S;
       UnitRecordArray[5].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAmd5Edit.Text:=S;
       UnitRecordArray[5].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAjp5Edit.Text:=S;
       UnitRecordArray[5].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[5].ActorScale:=StrtoFloatdef(S);
//      Unit5ActorScaleEdit.Text:=S;
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnEdit5.Text:=S;
       UnitRecordArray[5].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnTextureEdit5.Text:=S;
       UnitRecordArray[5].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

//Unit6 Enemy
      Readln(F,S); UnitRecordArray[6].startXLoc:=Strtoint(S);
 //       Unit6StartXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].startYLoc:=Strtoint(S);
//        Unit6StartYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].targetX:=Strtoint(S);
//        Unit6TargetXEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].targetY:=Strtoint(S);
 //       Unit6TargetYEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].speed:=Strtoint(S);
 //       Unit6SpeedEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Manueverability:=Strtoint(S);
//        Unit6ManEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].armor:=Strtoint(S);
//        Unit6ArmorEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members:=Strtoint(S);
//        Unit6MEdit.Text:=S;
        ProjectRecord.NumberofActiveEnemy:= Strtoint(S);
        //ProjectRecord.NumberofActiveEnemy+
      Readln(F,S); UnitRecordArray[6].Members1X:=Strtoint(S);
//        Unit6M1XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members1Y:=Strtoint(S);
//        Unit6M1YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members2X:=Strtoint(S);
//        Unit6M2XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members2Y:=Strtoint(S);
 //       Unit6M2YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members3X:=Strtoint(S);
//        Unit6M3XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members3Y:=Strtoint(S);
//        Unit6M3YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members4X:=Strtoint(S);
 //       Unit6M4XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members4Y:=Strtoint(S);
 //       Unit6M4YEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members5X:=Strtoint(S);
 //       Unit6M5XEdit.Text:=S;
      Readln(F,S); UnitRecordArray[6].Members5Y:=Strtoint(S);
 //       Unit6M5YEdit.Text:=S;

      Readln(F,S); UnitRecordArray[6].OperatingOrders:=Strtoint(S);
//        Unit6OperatingOrdersRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[6].SearchMode:=Strtoint(S);
//        Unit6SearchRG.ItemIndex:=Strtoint(S);
      Readln(F,S); UnitRecordArray[6].TieBreakerMode:=Strtoint(S);
 //       Unit6TieBreakerRG.ItemIndex:=Strtoint(S);
      Readln(F,S); If (S=  NoName) then S:='';
 //      FileAff6Edit.Text:=S;
       UnitRecordArray[6].FlagFile:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       TargetSizeEdit6.Text:=S;
       UnitRecordArray[6].TargetSize:=StrtoFloatdef(S);
      Readln(F,S); If (S=  NoName) then S:='';
 //      FileAmd6Edit.Text:=S;
       UnitRecordArray[6].Md2Name:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
//       FileAjp6Edit.Text:=S;
       UnitRecordArray[6].Md2TextureName:=(S);
      Readln(F,S); UnitRecordArray[6].ActorScale:=StrtoFloatdef(S);
//      Unit6ActorScaleEdit.Text:=S;
      Readln(F,S); If (S=  NoName) then S:='';
//       WpnEdit6.Text:=S;
       UnitRecordArray[6].Md2WpnName:=(S);
      Readln(F,S); If (S=  NoName) then S:='';
 //      WpnTextureEdit6.Text:=S;
       UnitRecordArray[6].Md2WpnTextureName:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;

      //Future Proofing
      If Version > 1 then
      begin
      end;
    End;
    CloseFile(F);
End;


procedure LoadTerrainFile(Filename:String);
var
  F: TextFile;
  S: string;
  Version:Integer;
Const NoName='NoName';
Begin
    AssignFile(F, FileName);
    Reset(F);
    Readln(F,S);If S='AStar Terrain File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.HeightDataFile:=S;
       If Length(S) > 0 then ;//LoadImageFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.ImageTextureFile:=S;
       If Length(S) > 0 then ;//LoadImageFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.MapAttributesFile:=S;
       If Length(S) > 0 then ;//
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.ElevationFile:=S;
       If Length(S) > 0 then ;//LoadElevationFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.VegetationFile:=S;
       If Length(S) > 0 then ;//LoadVegetationFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.SoilsFile:=S;
       If Length(S) > 0 then ;//LoadSoilsFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.HydrologyFile:=S;
       If Length(S) > 0 then ;//LoadHydrologyFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.TransportFile:=S;
       If Length(S) > 0 then ;//LoadTransportFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.UrbanFile:=S;
       If Length(S) > 0 then ;//LoadUrbanFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.NOGOFile:=S;
       If Length(S) > 0 then ;//LoadNOGOFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.ObstaclesFile:=S;
       If Length(S) > 0 then ;//LoadObstaclesFile(S);
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.InfluenceMapFile:=S;
       If Length(S) > 0 then ;//
       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.GCostFile:=S;
       If Length(S) > 0 then
       begin
         //GCostFileName:=S;
         LoadGCostValues;//LoadGCostFile(S);
       end;

       Readln(F,S); If (S=  NoName) then S:='';
       ProjectRecord.MAGValuesFile:=S;
       If Length(S) > 0 then
       begin
         //MAGValuesFileName:=S;
         LoadMAGValues;//LoadMAGValuesFile(S);
       end;
      //Future Proofing
      If Version > 1 then
      begin
      end;
    End;
    CloseFile(F);
End;

//This could be a file of integers, instead of Readln strings...
procedure  LoadGCostValues;
var
  F: TextFile;
  S: string;
  Version,x,y,xi,yi,ii:Integer;
Const NoName='NoName';
Begin
    AssignFile(F, ProjectRecord.GCostFile);
    Reset(F);
    Readln(F,S);If S='AStar GCost File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);
      Readln(F,S); xi:=Strtoint(S);
      Readln(F,S); yi:=Strtoint(S);
      //SetLength(AttributeTileArray,mapWidth+1,mapHeight+1);
      SetLength(GCostData,xi+1,yi+1);
      For y:=0 to yi-1 do
      For x:=0 to xi-1 do
      begin
        Readln(F,S); ii:=Strtoint(S);
        GCostData[x][y] :=ii;
      end;
      //Future Proofing
      If Version > 1 then
      begin
      end;
    End;
    CloseFile(F);
End;

procedure  SaveGCostValues(mapWidth,mapHeight:Integer);
var
  F: TextFile;
  S: string;
  Version,x,y,ii:Integer;
Const NoName='NoName';
Begin
    AssignFile(F, ProjectRecord.GCostFile);
    Rewrite(F);
    Writeln(F,'AStar GCost File');
    Begin
      Version:=1;
      S:=Inttostr(Version); Writeln(F,S);
      S:=Inttostr(mapWidth); Writeln(F,S);
      S:=Inttostr(mapHeight); Writeln(F,S);
      For y:=0 to mapHeight-1 do
      For x:=0 to mapWidth-1 do
      begin
        ii:=GCostData[x][y];
        S:=Inttostr(ii); Writeln(F,S);
      end;
      //Future Proofing
      If Version > 1 then  begin    end;
    End;
    CloseFile(F);
End;

procedure  LoadMAGValues;
var
  F: TextFile;
  S: string;
  Version,i,ii,iii:Integer;
Const NoName='NoName';
Begin
    AssignFile(F, ProjectRecord.MAGValuesFile);
    Reset(F);
    Readln(F,S);If S='AStar MAG File' then
    Begin
      Readln(F,S); Version:=Strtoint(S);
       //OpenHeightDataEdit.Text:=S;
       // MAGValuesFileName   SetLength(MAGColorValueArray,0);
      Readln(F,S); iii:=Strtoint(S);
      //showmessage(S);
      SetLength(MAGColorValueArray,iii);
      //MAGColorValueArray:  Array of Array[1..2] of Integer;
      For I:=0 to iii-1 do
      begin
        Readln(F,S); ii:=Strtoint(S);
        MAGColorValueArray[I][1] :=ii;
        Readln(F,S); ii:=Strtoint(S);
        MAGColorValueArray[I][2] :=ii;
      end;
      //Future Proofing
      If Version > 1 then
      begin
      end;
    End;
    CloseFile(F);
End;
procedure  SaveMAGValues;
var
  F: TextFile;
  S: string;
  i,ii:Integer;
Const NoName='NoName';
Begin
//  MAGValuesFileName  SetLength(MAGColorValueArray,0);
  //showmessage(MAGValuesFileName);
      AssignFile(F, ProjectRecord.MAGValuesFile);
      Rewrite(F);
      writeln(F,'AStar MAG File');
      S:=Inttostr(1); writeln(F,S);
      ii:= Length(MAGColorValueArray);
      S:=Inttostr(ii); writeln(F,S);
      //showmessage(s);
      For I:=0 to ii-1 do
      begin
        S:=Inttostr(MAGColorValueArray[I][1]); writeln(F,S);
        S:=Inttostr(MAGColorValueArray[I][2]); writeln(F,S);
      end;
     //Future Proofing
      //If Version > 1 then  begin       end;
    CloseFile(F);
End;

procedure  ResetMAGDefaults;
Begin
  SetLength(MAGColorValueArray,27);  //0..26:27
  //Transportation  0.4
    MAGColorValueArray[0][1]:= RGB(0,0,0);
    MAGColorValueArray[0][2]:=30;  //S-Bare
    MAGColorValueArray[1][1]:= RGB(175,0,0);
    MAGColorValueArray[1][2]:=5;  //T-Hiway
    MAGColorValueArray[2][1]:= RGB(159,0,0);
    MAGColorValueArray[2][2]:=10; //T-Road
    MAGColorValueArray[3][1]:= RGB(150,0,0);
    MAGColorValueArray[3][2]:=20; //T-Street
    MAGColorValueArray[4][1]:= RGB(145,0,0);
    MAGColorValueArray[4][2]:=20;  //T-Dirt Trail
    //Soils    5..12 Bare Ground is #0
    MAGColorValueArray[5][1]:= RGB(160,175,160);
    MAGColorValueArray[5][2]:=40;  //S-Bare
    MAGColorValueArray[6][1]:= RGB(207,239,207);  //veg dead crops
    MAGColorValueArray[6][2]:=50;   //S-Plowed field
    MAGColorValueArray[7][1]:= RGB(191,175,191);
    MAGColorValueArray[7][2]:=65;   //S-Rocky Boulders

    MAGColorValueArray[8][1]:= RGB(191,175,91);
    MAGColorValueArray[8][2]:=75;   //S-Muck
    MAGColorValueArray[9][1]:= RGB(91,175,91);
    MAGColorValueArray[9][2]:=85;   //S-Swamp
    MAGColorValueArray[10][1]:= RGB(91,175,191);
    MAGColorValueArray[10][2]:=110;   //S-Swamp Lake
    MAGColorValueArray[11][1]:= RGB(207,191,207);
    MAGColorValueArray[11][2]:=40;  //S-Sand  hard packed
    MAGColorValueArray[12][1]:= RGB(223,207,223);
    MAGColorValueArray[12][2]:=60;  //S-Sand Dunes
    //Vegetation   13..17
    MAGColorValueArray[13][1]:= RGB(254,191,0);
    MAGColorValueArray[13][2]:=45; // V-Grass
    MAGColorValueArray[14][1]:= RGB(127,159,127);
    MAGColorValueArray[14][2]:=50;  //V-Mixed
    MAGColorValueArray[15][1]:= RGB(159,95,0);
    MAGColorValueArray[15][2]:=55;  //V-Shrub
    MAGColorValueArray[16][1]:= RGB(0,255,0);
    MAGColorValueArray[16][2]:=65;  //V-Forest
    MAGColorValueArray[17][1]:= RGB(0,111,0);
    MAGColorValueArray[17][2]:=80;  //V-Jungle

    //Hydrology  18..23
    MAGColorValueArray[18][1]:= RGB(0,254,254);
    MAGColorValueArray[18][2]:=65;   //H-Stream (fordable)
    MAGColorValueArray[19][1]:= RGB(0,0,254);
    MAGColorValueArray[19][2]:=110;   //H-River
    MAGColorValueArray[20][1]:= RGB(0,0,254);
    MAGColorValueArray[20][2]:=110;   //H-Lake
    MAGColorValueArray[21][1]:= RGB(254,254,254);
    MAGColorValueArray[21][2]:=75;   //H-Snow<1 ft
    MAGColorValueArray[22][1]:= RGB(207,207,207);
    MAGColorValueArray[22][2]:=110;   //H-Snow>1 ft
    MAGColorValueArray[23][1]:= RGB(211,211,211);
    MAGColorValueArray[23][2]:=110;   //H-Ice

    //Urban   24 25
    MAGColorValueArray[24][1]:= RGB(225,0,0);
    MAGColorValueArray[24][2]:=65;  //U-Housing
    MAGColorValueArray[25][1]:= RGB(254,0,0);
    MAGColorValueArray[25][2]:=75; //U-Urban areas
    //Obstacle Nogo  26
    MAGColorValueArray[26][1]:= RGB(100,100,100);
    MAGColorValueArray[26][2]:=110; //O-NoGo areas
End;
{Reset Directories and all other defaults}
procedure  ResetDefaults;
Begin
  ProjectDirectory:= ExtractFilePath(ParamStr(0))+'Projects';
  //MAGValuesFileName
  ProjectRecord.MAGValuesFile:=ProjectDirectory+'\MagDefault.abf';
  //showmessage(ProjectDirectory+'\MagDefault.abf');
  ResetMAGDefaults;
  AProjectOptionsFormX:=200;
  AProjectOptionsFormY:=120;
  AStarFormX:=20;
  AStarFormY:=0;
  ATerrainFormX:=10;
  ATerrainFormY:=10;
  AGr32ViewerFormX:=10;
  AGr32ViewerFormY:=10;
  AStarAboutFormX:=280;
  AStarAboutFormY:=80;
  AProjectMapMakerFormX:=20;
  AProjectMapMakerFormY:=20;
  frmLandscapeX:=30;
  frmLandscapeY:=30;
  EditModeActive:=True;
  SplashScreenDisplayed:=True;
  AutoloadObstacleFile:=False;

  TileSizeMode:=0;
  tileSize := 50;
  ImageSizeMode:=0;
  ImageWidth:=650;
  ImageHeight:=500;

  mapWidth := 65;//80;
  mapHeight := 50;//60;
    SearchMode:=2;
    TieBreakerMode:=0;
    GridLinesDisplayed:=True;
    PathDisplayed:=True;



    AStarDataDisplayArrowsColor:=0;
    BackGroundColor:=clBlack;
    GridColor:=clBlue;
    ObstacleColor:=clBlue;
    PathColor:=clLime;
    EnemyPositionsColor:=clTeal;
    EnemyUnitBaseColor:=clFuchsia;
    EnemyUnitGoalTargetColor:=clFuchsia;
    BaseStartColorArray[1]:=clRed;
    TargetGoalColorArray[1]:=clRed;
    BaseStartColorArray[2]:=clYellow;
    TargetGoalColorArray[2]:=clYellow;
    BaseStartColorArray[3]:=clAqua;
    TargetGoalColorArray[3]:=clAqua;
    BaseStartColorArray[4]:=clLime;
    TargetGoalColorArray[4]:=clLime;
    BaseStartColorArray[5]:=clSilver;
    TargetGoalColorArray[5]:=clSilver;


  //Set Data Demo location
  AstarMode:=1;
  //pathfinderID is used TOO MUCH as Variable
  ActiveUnitNumber:=1;
  NumberofUnits := 1;
 startXLocArray[ActiveUnitNumber] := 3;
 startYLocArray[ActiveUnitNumber] := 6;
 //Set initial target location. This can
 //be changed by right-clicking on the map.
 targetXLocArray[ActiveUnitNumber] := 12;
 targetYLocArray[ActiveUnitNumber] := 6;
 speedArray[ActiveUnitNumber] := 5;
 End;




procedure DoLoader;
var P_File: PrefFile;
var PathS: string;
begin {}
  PathS := ExtractFilePath(ParamStr(0)) + 'AStar.pof';
  if FileExists(PathS) then
  begin
    AssignFile(P_File, PathS);
    Reset(P_File);
    if IoResult <> 0 then ;//DoMessages(39984);
    Read(P_File, PreRcd);
    CloseFile(P_File);
    SetPreferences;
  end; //else DoMessages(39985);
  {ProjectRecord.MAGValuesFile :=ProjectDirectory+'\MagDefault.abf';
  if FileExists(MAGValuesFileName) then  LoadMAGValues
    else ResetMAGDefaults;}
end;


procedure SetPreferences;
var i:Integer;
begin {after loading}
  with PreRcd do
  begin
 // GCostFileName:=PGCostFileName;
  //ProjectRecord.MAGValuesFile:=PMAGValuesFileName;
    ProjectDirectory:=PProjectDirectory;
  AProjectOptionsFormX:=PAProjectOptionsFormX;
  AProjectOptionsFormY:=PAProjectOptionsFormY;
  AStarFormX:=PAStarFormX;
  AStarFormY:=PAStarFormY;
  ATerrainFormX:=PATerrainFormX;
  ATerrainFormY:=PATerrainFormY;
  AStarAboutFormX:=PAStarAboutFormX;
  AStarAboutFormY:=PAStarAboutFormY;
  AGr32ViewerFormX:=PAGr32ViewerFormX;
  AGr32ViewerFormY:=PAGr32ViewerFormY;
  AProjectMapMakerFormX:=PAProjectMapMakerFormX;
  AProjectMapMakerFormY:=PAProjectMapMakerFormY;
  frmLandscapeX:=PfrmLandscapeX;
  frmLandscapeY:=PfrmLandscapeY;
  SplashScreenDisplayed:=PSplashScreenDisplayed;
  AutoloadObstacleFile:=PAutoloadObstacleFile;
    GridLinesDisplayed:=PGridLinesDisplayed;
    PathDisplayed:=PPathDisplayed;
    SearchMode:=PSearchMode;
    TieBreakerMode:=PTieBreakerMode;
    AStarDataDisplayArrowsColor:=PAStarDataDisplayArrowsColor;
    BackGroundColor:=PBackGroundColor;
    GridColor:=PGridColor;
    PathColor:=PPathColor;
    ObstacleColor:=PObstacleColor;
    EnemyPositionsColor:=PEnemyPositionsColor;
    EnemyUnitBaseColor:=PEnemyUnitBaseColor;
    EnemyUnitGoalTargetColor:=PEnemyUnitGoalTargetColor;
    AstarMode:=PAstarMode;
    TileSizeMode:=PTileSizeMode;
    ImageSizeMode:=PImageSizeMode;
    ActiveUnitNumber:=PActiveUnitNumber;
    NumberofUnits:=PNumberofUnits;
    NumberofUnitPeople:=PNumberofUnitPeople;
    mapWidth:=PmapWidth;
    mapHeight:=PmapHeight;
    tileSize:=PtileSize;
    ImageWidth:=PImageWidth;
    ImageHeight:=PImageHeight;
    For i:=1 to 5 do
    BaseStartColorArray[i]:= PBaseStartColorArray[i];
    For i:=1 to 5 do
    TargetGoalColorArray[i]:=PTargetGoalColorArray[i];
  end;
end;
{---------------------------------------------------------------------}

procedure DoSaver;
var P_File: PrefFile;
var PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0)) + 'AStar.pof';
  if (not FileExists(PathS)) then ;//DoMessages(14{39987});
  GetPreferences;
  AssignFile(P_File, PathS);
  Rewrite(P_File);
  if IoResult <> 0 then
  begin
    //DoMessages(39986);
  end;
  //ProjectRecord.MAGValuesFile :=ProjectDirectory+'\MagDefault.abf';
  write(P_File, PreRcd);
  CloseFile(P_File);
  //SaveMAGValues;
end;
{---------------------------------------------------------------------}

procedure GetPreferences;
var i:Integer;
begin {before saving}
  with PreRcd do
  begin
    //PGCostFileName:=ProjectRecord.GCostFile;
    //PMAGValuesFileName:=ProjectRecord.MAGValuesFile;
    PProjectDirectory:=ProjectDirectory;
  PAProjectOptionsFormX:=AProjectOptionsFormX;
  PAProjectOptionsFormY:=AProjectOptionsFormY;
  PAStarFormX:=AStarFormX;
  PAStarFormY:=AStarFormY;
  PATerrainFormX:=ATerrainFormX;
  PATerrainFormY:=ATerrainFormY;
  PAStarAboutFormX:=AStarAboutFormX;
  PAStarAboutFormY:=AStarAboutFormY;
  PAGr32ViewerFormX:=AGr32ViewerFormX;
  PAGr32ViewerFormY:=AGr32ViewerFormY;
  PAProjectMapMakerFormX:=AProjectMapMakerFormX;
  PAProjectMapMakerFormY:=AProjectMapMakerFormY;
  PfrmLandscapeX:=frmLandscapeX;
  PfrmLandscapeY:=frmLandscapeY;
  PSplashScreenDisplayed:=SplashScreenDisplayed;
  PAutoloadObstacleFile:=AutoloadObstacleFile;
    PGridLinesDisplayed:=GridLinesDisplayed;
    PPathDisplayed:=PathDisplayed;
    PSearchMode:=SearchMode;
    PTieBreakerMode:=TieBreakerMode;
    PAStarDataDisplayArrowsColor:=AStarDataDisplayArrowsColor;
    PBackGroundColor:=BackGroundColor;
    PGridColor:=GridColor;
    PPathColor:=PathColor;
    PObstacleColor:=ObstacleColor;
    PEnemyPositionsColor:=EnemyPositionsColor;
    PEnemyUnitBaseColor:=EnemyUnitBaseColor;
    PEnemyUnitGoalTargetColor:=EnemyUnitGoalTargetColor;
    PAstarMode:=AstarMode;
    PTileSizeMode:=TileSizeMode;
    PImageSizeMode:=ImageSizeMode;
    PActiveUnitNumber:=ActiveUnitNumber;
    PNumberofUnits:=NumberofUnits;
    PNumberofUnitPeople:=NumberofUnitPeople;
    PmapWidth:=mapWidth;
    PmapHeight:=mapHeight;
    PtileSize:=tileSize;
    PImageWidth:=ImageWidth;
    PImageHeight:=ImageHeight;
    For i:=1 to 5 do
    PBaseStartColorArray[i]:= BaseStartColorArray[i];
    For i:=1 to 5 do
    PTargetGoalColorArray[i]:=TargetGoalColorArray[i];

  end;
end;

(*
    For i:=0 to 18 do
    AttributeColorArray[i]:=PAttributeColorArray[i];
    For i:=0 to 18 do
    AttributeValueArray[i]:=PAttributeValueArray[i];


     For i:=0 to 18 do
    AttributeColorArray[i]:=PAttributeColorArray[i];
    For i:=0 to 18 do
    AttributeValueArray[i]:=PAttributeValueArray[i];
*)
(*//Unit1   
      Read(F,S); UnitRecordArray[1].OperatingOrders:=Strtoint(S);
        :=Strtoint(S);
      Read(F,S); UnitRecordArray[1].SearchMode:=Strtoint(S);
        :=Strtoint(S);
      Read(F,S); UnitRecordArray[1].TieBreakerMode:=Strtoint(S);
        :=Strtoint(S);
      Read(F,S); If (S=  NoName) then S:='';
       FileAff1Edit.Text:=S;
       UnitRecordArray[1].FlagFile:=(S);
       //This is ONLY Loaded by the Using Graphics
       //If Length(S) > 0 then Load;


*)


end.
