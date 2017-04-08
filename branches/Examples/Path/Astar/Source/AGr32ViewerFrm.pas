unit AGr32ViewerFrm;
{ * The Original Code is Graphics32
 * The Initial Developer of the Original Code is
 * Alex A. Denisov}

 //Sprites_Ex demo  move sprites around.. faster than repainting stuff...


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AppEvnts, ComCtrls, ExtCtrls, ExtDlgs, Buttons,
  GLCrossPlatform,//PrecisionTimer
  GR32_Image,
  GR32, GR32_Transforms,  GR32_Layers;

type
  TPathPoint = record
    //Position : TAffineVector;
    Active   : Boolean;
  end;

  TAGr32ViewerForm = class(TForm)
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image32: TImage32;
    Panel1: TPanel;
    Label1: TLabel;
    Unit1Status: TLabel;
    Unit2Status: TLabel;
    Unit3Status: TLabel;
    Unit4Status: TLabel;
    Unit5Status: TLabel;
    Unit6Status: TLabel;
    Unit1Time: TLabel;
    Unit2Time: TLabel;
    Unit3Time: TLabel;
    Unit4Time: TLabel;
    Unit5Time: TLabel;
    Unit6Time: TLabel;
    LayerAlphaTB: TTrackBar;
    TerrainValueAlphaValueTB: TTrackBar;
    TerrainValueAlphaValueLabel: TLabel;
    LayerLoadBtn: TSpeedButton;
    OpenDialog: TOpenPictureDialog;
    GrPathPanel: TPanel;
    ColorDialog1: TColorDialog;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    TVAMinLabel: TLabel;
    TVAMaxLabel: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Unit1BaseYLabel: TLabel;
    Unit1BaseXLabel: TLabel;
    Unit2BaseYLabel: TLabel;
    Unit2BaseXLabel: TLabel;
    Unit3BaseYLabel: TLabel;
    Unit3BaseXLabel: TLabel;
    Unit4BaseYLabel: TLabel;
    Unit4BaseXLabel: TLabel;
    Unit5BaseYLabel: TLabel;
    Unit5BaseXLabel: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Unit6BaseXLabel: TLabel;
    Unit6BaseYLabel: TLabel;
    Unit1TargetXLabel: TLabel;
    Unit1TargetYLabel: TLabel;
    Unit2TargetXLabel: TLabel;
    Unit2TargetYLabel: TLabel;
    Unit3TargetXLabel: TLabel;
    Unit3TargetYLabel: TLabel;
    Unit4TargetXLabel: TLabel;
    Unit4TargetYLabel: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Unit5TargetXLabel: TLabel;
    Unit5TargetYLabel: TLabel;
    Unit6TargetXLabel: TLabel;
    Unit6TargetYLabel: TLabel;
    GridSizeRG: TRadioGroup;
    GridColorPanel: TPanel;
    GridTB: TTrackBar;
    GridPaintBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
procedure LoadFlags;

    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
procedure DisplayUnitStatus(InUnitNumber:Integer);

procedure DoScenario1;
procedure DoScenario2;
procedure DoScenario3;
procedure DoScenario4;
procedure DoSavePath;
procedure CreatePathLines(PathUnit:Integer);
    procedure LayerAlphaTBChange(Sender: TObject);
    procedure TerrainValueAlphaValueTBChange(Sender: TObject);
    procedure LayerLoadBtnClick(Sender: TObject);
    procedure GrPathPanelClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GridColorPanelClick(Sender: TObject);
    procedure GridPaintBtnClick(Sender: TObject);
    procedure GridTBChange(Sender: TObject);
  private
    { Private declarations }
    Testing : Boolean;
//    mx, my,
    MinValue,MaxValue,
    Scenario : Integer;
    PathPoint:TPathPoint;
  public
    { Public declarations }
    GridColor,GrPathColor:TColor32;
  end;

var
  AGr32ViewerForm: TAGr32ViewerForm;

implementation

uses
   AStarBlitzCode, AStarBlitzCodeH, AStarBlitzUnit,
   AStarGlobals,
   //AProjectOptionsFrm,
   GLKeyboard;  //User Interface

{$R *.DFM}


procedure TAGr32ViewerForm.FormCreate(Sender: TObject);
begin
  left := AGr32ViewerFormX;
  top := AGr32ViewerFormY;
  Width:=696;
  Height:=564;
  Scenario:=1;
  gGameTime:=0;
  ActiveUnitNumber:=1;
  GrPathColor:=clBlack32;
  GridColor:=clRed32;
  GridColorPanel.Color:=WinColor(GridColor);
  If ProjectFilename ='' then Testing:=True
  else Testing:=False;
end;

procedure TAGr32ViewerForm.LayerLoadBtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'ehh? (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir:=ProjectDirectory; //media\heightmaps
  OpenDialog.FileName:='';
  if OpenDialog.Execute then
  begin
    with TBitmapLayer(Image32.Layers[0]) do
    bitmap.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TAGr32ViewerForm.GrPathPanelClick(Sender: TObject);
begin
  ColorDialog1.Color:= WinColor(GrPathColor);
  if ColorDialog1.Execute then
  begin
    GrPathPanel.Color:=ColorDialog1.Color;
    GrPathColor:=Color32(ColorDialog1.Color);
  end;
end;

procedure TAGr32ViewerForm.LayerAlphaTBChange(Sender: TObject);
begin
 with TBitmapLayer(Image32.Layers[0]) do
  Bitmap.MasterAlpha := LayerAlphaTB.Position;//0..254;
end;

procedure TAGr32ViewerForm.TerrainValueAlphaValueTBChange(Sender: TObject);
begin
  ProjectRecord.TerrainValueAlphaD:= (TerrainValueAlphaValueTB.Position/100);
  TerrainValueAlphaValueLabel.Caption:=Format('%.4f',[ProjectRecord.TerrainValueAlphaD]);
end;

procedure TAGr32ViewerForm.FormShow(Sender: TObject);
var
  F: File of Integer;
  X,y,z:Integer;
 BitMap1 : TBitMap;
  ALayer: TBitmapLayer;
  L: TFloatRect;
begin
  Image32.Bitmap.BeginUpdate;
  Image32.Bitmap.Clear(clWhite32);  //clBlack32
  Image32.Bitmap.EndUpdate;
  Image32.Bitmap.Changed;
  Image32.Refresh; // force repaint
  If Testing then
  begin
  end else
  begin
  //Load ProjectFilename
    StatusBar1.Panels[6].Text:='Loading';
    Application.ProcessMessages;
    LoadProjectFile(ProjectFilename);
 If ((Length(ProjectRecord.MAGValuesFile)>0)and
 (ProjectRecord.MAGValuesFile<>'NoName')) then
 if FileExists(ProjectRecord.MAGValuesFile)then LoadMAGValues
    else ResetMAGDefaults;
        
    mapWidth:=ProjectRecord.mapWidth;
    mapHeight:=ProjectRecord.mapHeight;
    InitializePathfinder;
    TerrainValueAlphaValueTB.Position:= Trunc(ProjectRecord.TerrainValueAlphaD*100);
    TerrainValueAlphaValueLabel.Caption:=Format('%.4f',[ProjectRecord.TerrainValueAlphaD]);

    SetLength(island,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);
    SetLength(claimedNode,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);
    SetLength(nearByPath,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);
    SetLength(tempUnwalkability,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);
    SetLength(MapAttribute,ProjectRecord.mapWidth+1,ProjectRecord.mapHeight+1);

    setlength(UnitpathBank[1],ProjectRecord.NumberofActiveUnits+1+ProjectRecord.NumberofActiveEnemyPatrol);
    setlength(UnitpathBank[2],ProjectRecord.NumberofActiveUnits+1+ProjectRecord.NumberofActiveEnemyPatrol);

    If FileExists(ProjectRecord.ElevationFile) then
    begin //Slope
      //showmessage('Step 3 '+AProjectOptionsForm.FileAefEdit.Text);
      AssignFile(F, ProjectRecord.ElevationFile);
      Reset(F);
      Read(F,Z); If (Z<> ProjectRecord.mapWidth) then ;
      Read(F,Z); If (Z<> ProjectRecord.mapHeight) then ;
      for y := 0 to ProjectRecord.mapHeight-1 do
      for x := 0 to ProjectRecord.mapWidth-1 do
      begin
        Read(F,Z);
        GCostData[x,y]:= GCostData[x,y]+z;
      end;
      CloseFile(F);
    end else showmessage('Step 3 No Slope.aef');
    //Done so  walkability gets Set even if NO Slopes
    MinValue:=255;  MaxValue:=0;
    for y := 0 to ProjectRecord.mapHeight-1 do
    for x := 0 to ProjectRecord.mapWidth-1 do
    begin
      If GCostData[x,y] > ProjectRecord.TerrainMagNoGoD then
        walkability[x][y] :=unwalkable else
        walkability[x][y] := walkable;
        If GCostData[x,y] < MinValue then MinValue:= GCostData[x,y];
        If GCostData[x,y] > MaxValue then MaxValue:= GCostData[x,y];
    end;
    TVAMinLabel.Caption:= inttostr(MinValue);
    TVAMaxLabel.Caption:= inttostr(MaxValue);
  Image32.Layers.Clear;
  Image32.BeginUpdate;
  Image32.bitmap.LoadFromFile(ExtractFileName(ProjectRecord.ImageTextureFile));
  BitMap1 := TBitMap.Create;
  try
    BitMap1.Width:= Image32.Width;
    BitMap1.Height:= Image32.Height;
    BitMap1.Pixelformat :=pf24bit;
    BitMap1.Canvas.Brush.Color := clWhite;
    BitMap1.Canvas.Pen.Color := clWhite;
    BitMap1.Canvas.Brush.Style := bsSolid;
    BitMap1.Canvas.FillRect(Rect(0,0,Image32.Width,Image32.Height));
      //This ONLY works if data and Texture are the same size
    for y := 0 to ProjectRecord.mapHeight-1 do
    for x := 0 to ProjectRecord.mapWidth-1 do
    begin
      If  walkability[x][y] =unwalkable then
        BitMap1.Canvas.pixels[x,y]:=clRed;// clBlack else
        //BitMap1.Canvas.pixels[x,y]:= clRed;
    end;
      BitMap1.SaveToFile(ProjectDirectory+'nogo.bmp');

      ALayer := TBitmapLayer.Create(Image32.Layers);
       with ALayer do
      begin
        Bitmap.Assign(BitMap1);
        Bitmap.DrawMode := dmBlend;
        Bitmap.MasterAlpha := 255;
        // put it somethere
        L.Left := 0;//Random(Image32.Width);
        L.Top := 0;//Random(Image32.Height);
        L.Right := Image32.Width;//L.Left + Bitmap.Width;
        L.Bottom :=Image32.Height;// L.Top + Bitmap.Height;
        ALayer.Location := L;
      end;
      //Reuse it
      BitMap1.Canvas.FillRect(Rect(0,0,Image32.Width,Image32.Height));
      ALayer := TBitmapLayer.Create(Image32.Layers);
       with ALayer do
      begin
        Bitmap.Assign(BitMap1);
        Bitmap.DrawMode := dmBlend;
        Bitmap.MasterAlpha := 255;
        // put it somethere
        L.Left := 0;//Random(Image32.Width);
        L.Top := 0;//Random(Image32.Height);
        L.Right := Image32.Width;//L.Left + Bitmap.Width;
        L.Bottom :=Image32.Height;// L.Top + Bitmap.Height;
        ALayer.Location := L;
      end;
  finally
    BitMap1.Free;
    end;
    with TBitmapLayer(Image32.Layers[0]) do
    Bitmap.MasterAlpha :=0;
    with TBitmapLayer(Image32.Layers[1]) do
    Bitmap.MasterAlpha :=0;

   Image32.EndUpdate;
  Image32.Changed;

    LoadFlags;
    StatusBar1.Panels[6].Text:='Done';
  end;
end;
//Target Texture .. Actor Texture
procedure TAGr32ViewerForm.LoadFlags;
var
  X: Integer;
  ALayer: TBitmapLayer;
  L: TFloatRect;
begin
  Image32.BeginUpdate;
  for X := 1 to ProjectRecord.NumberofActiveUnits do
  begin
     //Create the Base: the Player
     UnitRecordArray[X].CurrentpathBank:=1;
     DisplayUnitStatus(X);
    // create a new layer...
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap.LoadfromFile(UnitRecordArray[X].Md2TextureName);
      //Bitmap32List.Bitmaps[X].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := 255;
      // put it somethere
      L.Left :=((UnitRecordArray[X].startXLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[X].startYLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[X].startXLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[X].startYLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
    end;
    //Create the Target
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap.LoadfromFile(UnitRecordArray[X].FlagFile);
      //Bitmap32List.Bitmaps[X].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := 255;
      // put it somethere
      L.Left :=((UnitRecordArray[X].targetX div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[X].targetY div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[X].targetX div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[X].targetY div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
    end;
  end;
    If (ProjectRecord.NumberofActiveEnemyPatrol>0) then
    begin
    If (ProjectRecord.NumberofActiveEnemy>0) then
    begin
    UnitRecordArray[6].CurrentpathBank:=1;
      for X := 1 to UnitRecordArray[6].Members do
      begin
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap.LoadfromFile(UnitRecordArray[6].FlagFile);
      //Bitmap32List.Bitmaps[X].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := 255;

        Case x of
        1:Begin
      // put it somethere
      L.Left :=((UnitRecordArray[6].Members1X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].Members1Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].Members1X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].Members1Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
        End;
        2:Begin
      L.Left :=((UnitRecordArray[6].Members2X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].Members2Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].Members2X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].Members2Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
        End;
        3:Begin
      L.Left :=((UnitRecordArray[6].Members3X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].Members3Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].Members3X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].Members3Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
        End;
        4:Begin
      L.Left :=((UnitRecordArray[6].Members4X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].Members4Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].Members4X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].Members4Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
        End;
        5:Begin
      L.Left :=((UnitRecordArray[6].Members5X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].Members5Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].Members5X div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].Members5Y div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
        End;
        End;//Case Bunkers
        end;//layer
      end; //Bunkers

     UnitRecordArray[6].CurrentpathBank:=1;
     DisplayUnitStatus(6);
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap.LoadfromFile(UnitRecordArray[6].Md2TextureName);
      //Bitmap32List.Bitmaps[X].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := 255;
      // put it somethere
      L.Left :=((UnitRecordArray[6].startXLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].startYLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].startXLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].startYLoc div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
    end;
    //Create the Target
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap.LoadfromFile(UnitRecordArray[6].FlagFile);
      //Bitmap32List.Bitmaps[X].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := 255;
      // put it somethere
      L.Left :=((UnitRecordArray[6].targetX div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Top := ((UnitRecordArray[6].targetY div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))- (Bitmap.Width div 2);
      L.Right := ((UnitRecordArray[6].targetX div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      L.Bottom := ((UnitRecordArray[6].targetY div ProjectRecord.tilesize)+(ProjectRecord.tilesize div 2))+ (Bitmap.Width div 2);
      ALayer.Location := L;
    end;
    end;
  end;
  Image32.EndUpdate;
  Image32.Changed;
End;

procedure TAGr32ViewerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  AGr32ViewerFormX := AGr32ViewerForm.left;
  AGr32ViewerFormY := AGr32ViewerForm.top;
   //Setlength the Arrays
    SetLength(island,0);
    SetLength(claimedNode,0);
    SetLength(nearByPath,0);
    SetLength(tempUnwalkability,0);
    SetLength(MapAttribute,0);

    setlength(UnitpathBank[1],0);
    setlength(UnitpathBank[2],0);

   EndPathfinder;
end;

procedure TAGr32ViewerForm.FormDestroy(Sender: TObject);
begin
  Image32.Layers.Clear;
end;

procedure TAGr32ViewerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Cancel AStar pathfinding loop
  if ssCtrl in Shift then
  begin
    If (Key=VK_F2) then LostinaLoop :=False;
    If (Key=VK_F9) then AstarUnitsRunning:=(not AstarUnitsRunning);
  End;
  if (Key=VK_ESCAPE) then Close else //if Key = #27 then Close;
  if (Key=VK_RETURN) then
  begin   //Enter key Toggles Pathfinding
    EditModeActive:=(not EditModeActive);
    If EditModeActive then
      StatusBar1.Panels[0].Text:= 'Edit Mode' else
      StatusBar1.Panels[0].Text:= 'Path Mode';
  end else   //Return key pressed
  if ssCtrl in Shift then
  begin   //CTRL : Enemy
    if ((Key=$36)and (ProjectRecord.NumberofActiveEnemyPatrol>0)) then
      begin
        ActiveUnitNumber:=6;  //6;//?
        StatusBar1.Panels[2].Text:= 'Enemy Unit ';//+'Unit # '+Inttostr(ActiveUnitNumber);
      end else
    If ProjectRecord.NumberofActiveEnemy>0 then
    begin
      if (Key=$31) then ActiveEnemyNumber:=1 else
      if (Key=$32) then ActiveEnemyNumber:=2 else
      if (Key=$33) then ActiveEnemyNumber:=3 else
      if (Key=$34) then ActiveEnemyNumber:=4 else
      if (Key=$35) then ActiveEnemyNumber:=5;
      If ActiveEnemyNumber>ProjectRecord.NumberofActiveEnemy then
         ActiveEnemyNumber:= ProjectRecord.NumberofActiveEnemy;
      StatusBar1.Panels[2].Text:= 'Enemy # '+Inttostr(ActiveEnemyNumber);
    end;
  end else
  begin  //Ctrl not required
     //5 Units possible  + 1 Enemy
    if (Key=$31) then ActiveUnitNumber:=1 else
    if (Key=$32) then ActiveUnitNumber:=2 else
    if (Key=$33) then ActiveUnitNumber:=3 else
    if (Key=$34) then ActiveUnitNumber:=4 else
    if (Key=$35) then ActiveUnitNumber:=5;
    if ((Key=$31) or (Key=$32) or (Key=$33) or (Key=$34) or (Key=$35)) then
    begin
      If ActiveUnitNumber>ProjectRecord.NumberofActiveUnits then ActiveUnitNumber:= ProjectRecord.NumberofActiveUnits;
      StatusBar1.Panels[2].Text:= 'Unit # '+Inttostr(ActiveUnitNumber);
    end;

    If EditModeActive then //Change ONLY in Edit mode
    begin  //(Key=VK_F2)  IsKeyDown(VK_F1)
      if (Key=VK_F1) then Scenario:=1 else //Move IAW
      if (Key=VK_F2) then Scenario:=2 else //Move and Deploy (Unit 1)
      if (Key=VK_F3) then Scenario:=3 else //Move, Deploy, Target BOSS fight
      if (Key=VK_F4) then Scenario:=4;
      StatusBar1.Panels[3].Text:= 'Scenario # '+Inttostr(Scenario);
    end;
  end;
end;
procedure TAGr32ViewerForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
    's','S':DoSavePath;
    end;
end;

procedure TAGr32ViewerForm.Image32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var   AStarx,AStary:Integer;
begin
  AStarx := X div ProjectRecord.tileSize;
  AStary := Y div ProjectRecord.tileSize;
  if (ssLeft in Shift) then  { Base }
  begin
      if IsKeyDown('O') then     //ObstacleColor
      begin
        walkability[AStarx][AStary] := 1-walkability[AStarx][AStary];
      end else
      //ActiveEnemyPosition
      if IsKeyDown('E') then     //EnemyPositionsColor
      begin
        If ActiveEnemyNumber=6 then
        begin  //EnemystartXLoc
        EnemystartXLoc:=AStarx;
        EnemystartYLoc:=AStary;
        end else
        begin     //1..5
        EnemyBaseXLocArray[ActiveEnemyNumber]:=AStarx;
        EnemyBaseYLocArray[ActiveEnemyNumber]:=AStary;
        walkability[AStarx][AStary] := 1-walkability[AStarx][AStary];
        {if (walkability[AStarx][AStary] = unwalkable)
        then AStarImage.Picture.Bitmap.Canvas.Brush.Color := EnemyPositionsColor
        else AStarImage.Picture.Bitmap.Canvas.Brush.Color := BackGroundColor;}
        end;
      end else
      begin
      startXLocArray[ActiveUnitNumber]:=AStarx;
      startYLocArray[ActiveUnitNumber]:=AStary;
      {AStarImage.Picture.Bitmap.Canvas.Brush.Color :=
        BaseStartColorArray[ActiveUnitNumber];}
      end;

  End else
  if (ssRight in Shift) then  //Place the Gopher Target
  begin
    If (EditModeActive) then
    Begin
      if IsKeyDown('E') then     //EnemyPositionsColor
      begin
        If ActiveEnemyNumber=6 then
        begin  //EnemystartXLoc
        EnemytargetXLoc:=AStarx;
        EnemytargetYLoc:=AStary;
        end
        end else
        begin
      targetXLocArray[ActiveUnitNumber]:=AStarx;
      targetYLocArray[ActiveUnitNumber]:=AStary;
      {AStarImage.Picture.Bitmap.Canvas.Brush.Color :=
        TargetGoalColorArray[ActiveUnitNumber]; }
        end;

    End else
    begin
      Case Scenario of
      1:Begin
          //  UnitRecordArray[ActiveUnitNumber].targetX:=(-1*Trunc(iPoint[0]/2));
          // UnitRecordArray[ActiveUnitNumber].targetY:=Trunc(iPoint[2]);
          //UnitRecordArray[ActiveUnitNumber].targetZ:=Trunc(iPoint[1]/2 );

        end;
      2:Begin    //PathFind THE Selected  Unit
             gLoopCount:=1;
             gGameTime:=0; gLoopTime:=0;
             gStartTime:=StartPrecisionTimer;
             UnitRecordArray[ActiveUnitNumber].pathStatus:=BlitzFindPathH(ActiveUnitNumber,
                UnitRecordArray[ActiveUnitNumber].startXLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].startYLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetX*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetY*ProjectRecord.tileSize,
                normal);
             gLoopTime:=gLoopTime+StopPrecisionTimer(gStartTime);
             gGameTime:=gLoopTime*1000/gLoopCount;
             DisplayUnitStatus(ActiveUnitNumber);
             If (UnitRecordArray[ActiveUnitNumber].pathStatus = found)then
             begin
             StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
             PathPoint.Active:=True;
             DoScenario2;
             end else
             begin
               gGameTime:=-1;
               PathPoint.Active:=False;
               StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
             end;
        End;
      3:Begin//Pathfind 5 Units +1 Enemy    ActiveEnemyNumber:=1
             gLoopCount:=1;
             gGameTime:=0; gLoopTime:=0;
             gStartTime:=StartPrecisionTimer;
             UnitRecordArray[ActiveUnitNumber].pathStatus:=BlitzFindPath(ActiveUnitNumber,
                UnitRecordArray[ActiveUnitNumber].startXLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].startYLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetX*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetY*ProjectRecord.tileSize,
                normal);
             gLoopTime:=gLoopTime+StopPrecisionTimer(gStartTime);
             gGameTime:=gLoopTime*1000/gLoopCount;
             DisplayUnitStatus(ActiveUnitNumber);
             If (UnitRecordArray[ActiveUnitNumber].pathStatus = found)then
             begin
               StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
               PathPoint.Active:=True;
               DoScenario3;
             end else
             begin
               gGameTime:=-1;
               PathPoint.Active:=False;     //Display Time as Failure
               StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
             end;
        end;
      4:Begin//Pathfind 5 Units +1 Enemy    ActiveEnemyNumber:=1
             gLoopCount:=1;
             gGameTime:=0; gLoopTime:=0;
             gStartTime:=StartPrecisionTimer;
             UnitRecordArray[ActiveUnitNumber].pathStatus:=BlitzFindPath(ActiveUnitNumber,
                UnitRecordArray[ActiveUnitNumber].startXLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].startYLoc*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetX*ProjectRecord.tileSize,
                UnitRecordArray[ActiveUnitNumber].targetY*ProjectRecord.tileSize,
                normal);
             gLoopTime:=gLoopTime+StopPrecisionTimer(gStartTime);
             gGameTime:=gLoopTime*1000/gLoopCount;
             DisplayUnitStatus(ActiveUnitNumber);
             If (UnitRecordArray[ActiveUnitNumber].pathStatus = found)then
             begin
               StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
               PathPoint.Active:=True;
               DoScenario3;
             end else
             begin
               gGameTime:=-1;
               PathPoint.Active:=False;     //Display Time as Failure
               StatusBar1.Panels[1].Text:= Format('%.3f ms',[gGameTime]);
             end;
        end;
      end;//case
    end;//  (not EditModeActive)
  end; // (ssRight in Shift)
end;

procedure TAGr32ViewerForm.DisplayUnitStatus(InUnitNumber:Integer);
var Status:Integer;
Begin  //Patrolling   Deployed  Lost
  Status:= UnitRecordArray[InUnitNumber].pathStatus;
  Case InUnitNumber of
  1:Begin
      Unit1Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit1BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit1BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit1TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit1TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit1Status.Caption:='notStarted'else
      If (Status=found) then  Unit1Status.Caption:='found' else
      If (Status=nonexistent) then Unit1Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit1Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit1Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit1Status.Caption:='RedirectFailed' else
       Unit1Status.Caption:='OBE';
    End;
  2:Begin
      Unit2Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit2BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit2BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit2TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit2TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit2Status.Caption:='notStarted'else
      If (Status=found) then  Unit2Status.Caption:='found' else
      If (Status=nonexistent) then Unit2Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit2Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit2Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit2Status.Caption:='RedirectFailed' else
         Unit2Status.Caption:='OBE';
    End;
  3:Begin
      Unit3Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit3BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit3BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit3TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit3TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit3Status.Caption:='notStarted'else
      If (Status=found) then  Unit3Status.Caption:='found' else
      If (Status=nonexistent) then Unit3Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit3Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit3Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit3Status.Caption:='RedirectFailed' else
         Unit3Status.Caption:='OBE';
    End;
  4:Begin
      Unit4Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit4BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit4BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit4TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit4TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit4Status.Caption:='notStarted'else
      If (Status=found) then  Unit4Status.Caption:='found' else
      If (Status=nonexistent) then Unit4Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit4Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit4Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit4Status.Caption:='RedirectFailed' else
         Unit4Status.Caption:='OBE';
    End;
  5:Begin
      Unit5Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit5BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit5BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit5TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit5TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit5Status.Caption:='notStarted'else
      If (Status=found) then  Unit5Status.Caption:='found' else
      If (Status=nonexistent) then Unit5Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit5Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit5Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit5Status.Caption:='RedirectFailed' else
         Unit5Status.Caption:='OBE';
    End;
  6:Begin
      Unit6Time.Caption:= Format('%.3f',[gGameTime]);   //*ProjectRecord.tileSize
      Unit6BaseXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startXLoc);
      Unit6BaseYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].startYLoc);
      Unit6TargetXLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetX);
      Unit6TargetYLabel.Caption:=Inttostr(UnitRecordArray[InUnitNumber].targetY);
      If (Status=notStarted) then Unit6Status.Caption:='notStarted'else
      If (Status=found) then  Unit6Status.Caption:='found' else
      If (Status=nonexistent) then Unit6Status.Caption:='nonexistent' else
      If (Status=stopped) then Unit6Status.Caption:='stopped' else
      If (Status=Targetunwalkable) then Unit6Status.Caption:='Targetunwalkable' else
      If (Status=RedirectFailed) then Unit6Status.Caption:='RedirectFailed' else
         Unit6Status.Caption:='OBE';
    End;
  End;//Case
End;

procedure TAGr32ViewerForm.Image32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
// Only do the Simple stuff.. Retarget the Path
  StatusBar1.Panels[4].Text :='X: '+Inttostr(X)+' Y: '+Inttostr(Y);
  StatusBar1.Panels[5].Text :='X: '+Inttostr((X div ProjectRecord.tilesize)+1)
                            +' Y: '+Inttostr((Y  div ProjectRecord.tilesize)+1);
end;

procedure TAGr32ViewerForm.DoScenario1;
var
  I: Integer;
  R: TFloatRect;
Begin
  if PathPoint.Active then
  begin
    PathPoint.Active:=False;
    Image32.BeginUpdate;
    for I := 0 to Image32.Layers.Count - 1 do
    begin
      with TBitmapLayer(Image32.Layers[I]) do
      begin
        R := Location;

        Location := R;
      end;
    end;
    Image32.EndUpdate;
    Image32.Invalidate;
    //  If ProjectRecord.PathDisplayed then
    CreatePathLines(ActiveUnitNumber);
  end;
end;
procedure TAGr32ViewerForm.DoScenario2;
Begin
  if PathPoint.Active then
  begin
    PathPoint.Active:=False;
  //  If ProjectRecord.PathDisplayed then
    CreatePathLines(ActiveUnitNumber);
  end;
End;
procedure TAGr32ViewerForm.DoScenario3;
//var i:Integer;
Begin
  if PathPoint.Active then
  begin
    PathPoint.Active:=False;
  //  If ProjectRecord.PathDisplayed then
    CreatePathLines(ActiveUnitNumber);
  end;
end;

procedure TAGr32ViewerForm.DoScenario4;
//var i:Integer;
Begin
  if PathPoint.Active then
  begin
    PathPoint.Active:=False;
  //  If ProjectRecord.PathDisplayed then
    CreatePathLines(ActiveUnitNumber);
  end;
end;

procedure TAGr32ViewerForm.DoSavePath;
var
  F: TextFile;
  S,s2: string;
  X1, Y1, Ii: Integer;
Begin  //ActiveUnitNumber
  If (UnitRecordArray[ActiveUnitNumber].pathStatus = found)then
  begin
    AssignFile(F, ProjectDirectory+'PathData.txt');
    Rewrite(F);
    Writeln(F,'AStar Path File for '+Inttostr(ActiveUnitNumber));

    X1:=UnitRecordArray[ActiveUnitNumber].startXLoc;
    Y1:=UnitRecordArray[ActiveUnitNumber].startYLoc;
    S:=Inttostr(X1); S2:=Inttostr(Y1);Writeln(F,'start X '+S+' Y '+S2);
    //Restart to the First one
    UnitRecordArray[ActiveUnitNumber].pathLocation :=0;
    For Ii:= 0 to UnitRecordArray[ActiveUnitNumber].pathLength-1 do
    begin
          UnitRecordArray[ActiveUnitNumber].pathLocation := (UnitRecordArray[ActiveUnitNumber].pathLocation + 1);
          UnitRecordArray[ActiveUnitNumber].xPath := BlitzReadPathX(ActiveUnitNumber,UnitRecordArray[ActiveUnitNumber].pathLocation);
          UnitRecordArray[ActiveUnitNumber].yPath := BlitzReadPathY(ActiveUnitNumber,UnitRecordArray[ActiveUnitNumber].pathLocation);
    X1:=UnitRecordArray[ActiveUnitNumber].xPath;//+ProjectRecord.tileSize;
    Y1:=UnitRecordArray[ActiveUnitNumber].yPath;//+ProjectRecord.tileSize;
    S:=Inttostr(X1); S2:=Inttostr(Y1);Writeln(F,Inttostr(ii)+' X '+S+' Y '+S2);
    end;
    X1:=UnitRecordArray[ActiveUnitNumber].targetX;
    Y1:=UnitRecordArray[ActiveUnitNumber].targetY;
    S:=Inttostr(X1); S2:=Inttostr(Y1);Writeln(F,'target X '+S+' Y '+S2);

    CloseFile(F);
    Application.ProcessMessages;
    if FileExists(ProjectDirectory+'PathData.txt') then
      ExecuteFile('PathData.txt', '', ProjectDirectory, SW_SHOW)
  end;
End;

procedure TAGr32ViewerForm.CreatePathLines(PathUnit:Integer);
var
X, Y1, X2, Y2,Ii: Integer;
//i,ii : Integer;
//  NodeX,NodeY,NodeZ:Double;

begin
  If (UnitRecordArray[PathUnit].pathStatus = found)then
  begin
    Image32.BeginUpdate;
    //ColorDialog1.Color:= WinColor(GridColor);
    //GridColor:=Color32(ColorDialog1.Color);
    //GrPathColor:=clBlack32;//Color32(PathColor);
    X:=UnitRecordArray[PathUnit].startXLoc;
    Y1:=UnitRecordArray[PathUnit].startYLoc;
        For Ii:= 0 to UnitRecordArray[PathUnit].pathLength-1 do
        begin
        StatusBar1.Panels[6].Text :=Inttostr(Ii);
          UnitRecordArray[PathUnit].pathLocation := (UnitRecordArray[PathUnit].pathLocation + 1);
          UnitRecordArray[PathUnit].xPath := BlitzReadPathX(PathUnit,UnitRecordArray[PathUnit].pathLocation);
          UnitRecordArray[PathUnit].yPath := BlitzReadPathY(PathUnit,UnitRecordArray[PathUnit].pathLocation);
//        AStarImage.Picture.Bitmap.Canvas.Brush.Color :=TargetGoalColorArray[ID];

    X2:=UnitRecordArray[PathUnit].xPath+ProjectRecord.tileSize-(2);
    Y2:=UnitRecordArray[PathUnit].yPath+ProjectRecord.tileSize-(2);
    Image32.Bitmap.LineS(X, Y1, X2, Y2, GrPathColor,True);
//    For i:= 0 to 1000000*(10-UnitRecordArray[PathUnit].speed) do       Application.ProcessMessages;
    X:=X2;
    Y1:=Y2;
    end;

    X2:=UnitRecordArray[PathUnit].targetX;
    Y2:=UnitRecordArray[PathUnit].targetY;
    Image32.Bitmap.LineS(X, Y1, X2, Y2, GrPathColor,True);
    Image32.EndUpdate;
    Image32.Invalidate;
    StatusBar1.Panels[6].Text :='Done';
  end;
end;
(*       XDistance,YDistance,HDistance,
ClaimedNodesDisplayed    SplashScreenDisplayed

island  claimedNode  nearByPath  tempUnwalkability

unitCollidingWith pathAI  startNewPath  gGroupUnit



procedure TForm1.IdleHandler(Sender: TObject; var Done: Boolean);
var
  I: Integer;
  R: TFloatRect;
begin
  if Image32.Layers.Count = 0 then Exit;
  Image32.BeginUpdate;
  for I := 0 to Image32.Layers.Count - 1 do
  begin
    with TBitmapLayer(Image32.Layers[I]) do
    begin
      //Bitmap.MasterAlpha := (Bitmap.MasterAlpha + 1) mod 256;
      R := Location;
      with Velocities[I] do
      begin
        OffsetRectF(R, X, Y);
        X := X + (Random - 0.5) * 0.1;
        Y := Y + (Random - 0.5) * 0.1;
        if (R.Left < 0) and (X < 0) then X := 1;
        if (R.Top < 0) and (Y < 0) then Y := 1;
        if (R.Right > Image32.Width) and (X > 0) then X := -1;
        if (R.Bottom > Image32.Height) and (Y > 0) then Y := -1;
      end;
      Location := R;
    end;
  end;
  Image32.EndUpdate;
  Image32.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image32.Layers.Clear;
  Velocities := nil;
  Edit1.Text := '0 layers';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := IdleHandler;
end;
*)


















procedure TAGr32ViewerForm.GridColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color:= WinColor(GridColor);
  if ColorDialog1.Execute then
  begin
    GridColorPanel.Color:=ColorDialog1.Color;
    GridColor:=Color32(ColorDialog1.Color);
  end;
end;

procedure TAGr32ViewerForm.GridPaintBtnClick(Sender: TObject);
var x,y,z,zz:Integer;
begin
//GridLineSizeRG
 case GridSizeRG.itemindex of
 0:z:=64;
 1:z:=32;
 2:z:=20;
 3:z:=16;
 4:z:=10;
 else z:=8;
 end;
 zz:=0;
  For X:= 0 to Image32.Width-1 do
  Begin
   inc(zz);  
   If (zz mod z = 0)then
    begin
     with TBitmapLayer(Image32.Layers[1]) do
    {Image32.}Bitmap.Line(X, 0, X, Image32.Height,
                             GridColor,True);
    zz:=0;
    end;
  End;
  zz:=0;
  For Y:= 0 to Image32.Height-1 do
  Begin
   inc(zz);  
   If (zz mod z = 0)then
    begin
     with TBitmapLayer(Image32.Layers[1]) do
    {Image32.}Bitmap.Line(0, y,Image32.Width, y,
                             GridColor,True);
    zz:=0;
    end;
  End;
end;

procedure TAGr32ViewerForm.GridTBChange(Sender: TObject);
begin
 with TBitmapLayer(Image32.Layers[1]) do
  Bitmap.MasterAlpha := GridTB.Position;//0..254;
end;

end.
