unit fLandscape;

// This is cloned from Mattias Landscape MapMaker
// Made Variables to change the textures
// Button_CreateBitmapClick
interface

{ \\0111004-gx240\Server Disc\User\Mattias\Delphi\MapMaker\ }

// Make the heightmap curved /\  bubble as second line
// cup males it 'flatter' :  \/
// the Memo tends to erase itself.. sometimes so this is a backup
{
  clear
  cup
  n|4|0.5
  n|8|0.25
  n|16|0.125
  n|32|0.061
  n|64|0.0305
  n|128|0.0150
  rescale
}
uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  // math,
  // mmsystem,
  StdCtrls,
  ComCtrls,
  Buttons,
  ExtDlgs,
  Spin,
  Vcl.Imaging.jpeg,
  GLUtils,
  GLScene,
  GLVectorGeometry,
  GLVectorTypes,
  GR32,
  GR32_Image,
  GR32_Layers,
  uHeightmapClasses;

// const  BASE_DIR = 'Projects\';//'Output\';

type
  TfrmLandscape = class(TForm)
    OpenDialogData: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ColorDialog1: TColorDialog;
    OpenDialog: TOpenPictureDialog;
    PageControl1: TPageControl;
    TerrainTabSheet: TTabSheet;
    TerrainAttributesTabSheet: TTabSheet;
    TerrainAttributesScrollBox: TScrollBox;
    TerrainAttributesPanel: TPanel;
    AttributeSetBtn: TSpeedButton;
    AttributeImageSizeRG: TRadioGroup;
    AttributeNewImageBtn: TButton;
    AttributeSaveImageBtn: TButton;
    AttributeLoadBackgroundBtn: TButton;
    AttributeEdit: TEdit;
    AttributeColorPanel: TPanel;
    TerrainPanel: TPanel;
    CheckBox_RenderWater: TCheckBox;
    Button_RenderNormal: TButton;
    Button_Rescale: TButton;
    Button_MakeIsland: TButton;
    Button_GO: TButton;
    Memo_Octaves: TMemo;
    Button_Clear: TButton;
    ImageTA32: TImage32;
    TerrainAttributesTileSizeRG: TRadioGroup;
    StatusBar1: TStatusBar;
    LayerAlphaTB: TTrackBar;
    TextureTabSheet: TTabSheet;
    TextureScrollBox: TScrollBox;
    Image_Bitmap: TImage32;
    TexturePanel: TPanel;
    SlopeBtn: TSpeedButton;
    SnowBtn: TSpeedButton;
    RockBtn: TSpeedButton;
    GrassBtn: TSpeedButton;
    SandBtn: TSpeedButton;
    Label5: TLabel;
    SandEdit1: TEdit;
    SandEdit2: TEdit;
    SandEdit3: TEdit;
    SandEdit4: TEdit;
    SandPanel: TPanel;
    ImageTextureSizeRG: TRadioGroup;
    Button_SaveBitmap: TButton;
    Button_CreateBitmap: TButton;
    CheckBox_Shaded: TCheckBox;
    Edit_Ambient: TEdit;
    GrassEdit3: TEdit;
    GrassEdit4: TEdit;
    GrassEdit1: TEdit;
    GrassEdit2: TEdit;
    GrassPanel: TPanel;
    RockEdit3: TEdit;
    RockEdit4: TEdit;
    RockPanel: TPanel;
    SnowPanel: TPanel;
    RockEdit1: TEdit;
    RockEdit2: TEdit;
    SnowEdit1: TEdit;
    SnowEdit2: TEdit;
    SnowEdit3: TEdit;
    SnowEdit4: TEdit;
    TSlopeTab: TTabSheet;
    TSlopeScrollBox: TScrollBox;
    Image32_Misc: TImage32;
    Button_LoadTerrain: TButton;
    Button_SaveTerrain: TButton;
    Button_SubDivide: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBar_Radius: TScrollBar;
    ScrollBar_Depth: TScrollBar;
    CheckBox_Box: TCheckBox;
    LandscapeSizeRG: TRadioGroup;
    TerrainScrollBox: TScrollBox;
    Image32_Terrain: TImage32;
    TSlopePanel: TPanel;
    ConvertSlopesBtn: TSpeedButton;
    SaveMiscBitmapBtn: TButton;
    Button_Shadow: TButton;
    Button_Shaded: TButton;
    SlopeMaxLabel: TLabel;
    SlopeMinLabel: TLabel;
    SlopeEdit: TEdit;
    Button_Slopes: TButton;
    TSlopeSizeRG: TRadioGroup;
    Button_AATerrain: TButton;
    Button_AASweep: TButton;
    ForestBtn: TSpeedButton;
    ForestPanel: TPanel;
    ForestEdit1: TEdit;
    ForestEdit2: TEdit;
    ForestEdit3: TEdit;
    ForestEdit4: TEdit;
    PaintMapCB: TCheckBox;
    AttributeSaveBackgroundBtn: TButton;
    DepthLabel: TLabel;
    RadiusLabel: TLabel;
    ToDoTabsheet: TTabSheet;
    ToDoPanel: TPanel;
    ToDoPageControl: TPageControl;
    TabSheet7: TTabSheet;
    SaveagfBtn: TSpeedButton;
    OpenagfBtn: TSpeedButton;
    FileAgfEdit: TEdit;
    TabSheet9: TTabSheet;
    OpenanfBtn: TSpeedButton;
    SaveanfBtn: TSpeedButton;
    FileAnfEdit: TEdit;
    TabSheet10: TTabSheet;
    OpenabfBtn: TSpeedButton;
    SaveabfBtn: TSpeedButton;
    FileAbfEdit: TEdit;
    TabSheet18: TTabSheet;
    OpenaefBtn: TSpeedButton;
    SaveaefBtn: TSpeedButton;
    FileAefEdit: TEdit;
    TabSheet19: TTabSheet;
    OpenavfBtn: TSpeedButton;
    SaveavfBtn: TSpeedButton;
    FileAvfEdit: TEdit;
    CheckBox1: TCheckBox;
    TabSheet20: TTabSheet;
    OpenasfBtn: TSpeedButton;
    SaveasfBtn: TSpeedButton;
    FileAsfEdit: TEdit;
    CheckBox7: TCheckBox;
    TabSheet21: TTabSheet;
    OpenahfBtn: TSpeedButton;
    SaveahfBtn: TSpeedButton;
    Label59: TLabel;
    FileAhfEdit: TEdit;
    CheckBox6: TCheckBox;
    TabSheet6: TTabSheet;
    OpenatfBtn: TSpeedButton;
    SaveatfBtn: TSpeedButton;
    FileAtfEdit: TEdit;
    CheckBox5: TCheckBox;
    TabSheet22: TTabSheet;
    OpenaafBtn: TSpeedButton;
    SaveaafBtn: TSpeedButton;
    FileAafEdit: TEdit;
    CheckBox4: TCheckBox;
    TabSheet23: TTabSheet;
    OpenaofBtn: TSpeedButton;
    SaveaofBtn: TSpeedButton;
    FileAofEdit: TEdit;
    CheckBox3: TCheckBox;
    TabSheet5: TTabSheet;
    OpenaifBtn: TSpeedButton;
    SaveaifBtn: TSpeedButton;
    FileAifEdit: TEdit;
    TabSheet3: TTabSheet;
    OpenamfBtn: TSpeedButton;
    SaveamfBtn: TSpeedButton;
    FileAmfEdit: TEdit;
    CheckBox2: TCheckBox;
    PageControl3: TPageControl;
    TabSheet1: TTabSheet;
    agfSaveBtn: TSpeedButton;
    agfOpenBtn: TSpeedButton;
    agfEdit: TEdit;
    TabSheet4: TTabSheet;
    abfOpenBtn: TSpeedButton;
    abfSaveBtn: TSpeedButton;
    abfEdit: TEdit;
    TabSheet24: TTabSheet;
    amfOpenBtn: TSpeedButton;
    amfSaveBtn: TSpeedButton;
    amfEdit: TEdit;
    TAGroupBox: TGroupBox;
    TAConvertBtn: TSpeedButton;
    TALabel: TLabel;
    TABigBrushCB: TCheckBox;
    TAScrollBar: TScrollBar;
    Label3: TLabel;
    TARadiusLabel: TLabel;
    TARePaintBtn: TSpeedButton;
    AttributeLoadImageBtn: TButton;
    ResetDefaultMemoBtn: TButton;
    AttributeNewBaseImageBtn: TButton;
    AttributeRG: TRadioGroup;
    SlopeCheckBox: TCheckBox;
    LayerTwixTB: TTrackBar;
    AttributeLoadTwixBtn: TButton;
    WaterEdit: TEdit;
    IslandEdit: TEdit;
    GridPaintBtn: TSpeedButton;
    GridSizeRG: TRadioGroup;
    GridColorPanel: TPanel;
    FlipnSaveBtn: TButton;
    LoadTextureButton: TButton;
    Slope2ColorPanel: TPanel;
    Slope2Edit: TEdit;
    Slope15Edit: TEdit;
    Slope15ColorPanel: TPanel;
    Slope45Edit: TEdit;
    Slope45ColorPanel: TPanel;
    Slope99Edit: TEdit;
    Slope99ColorPanel: TPanel;
    Slope7Edit: TEdit;
    Slope7ColorPanel: TPanel;
    Label4: TLabel;
    AttributeLoadUnitBtn: TButton;
    LayerUnitTB: TTrackBar;
    LayerUnitSizeTB: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure Button_ClearClick(Sender: TObject);
    procedure Button_GOClick(Sender: TObject);
    procedure Button_ClampClick(Sender: TObject);
    procedure Button_MakeIslandClick(Sender: TObject);
    procedure Button_RescaleClick(Sender: TObject);
    procedure Button_SlopesClick(Sender: TObject);
    procedure Button_RenderNormalClick(Sender: TObject);
    procedure Button_SaveTerrainClick(Sender: TObject);
    procedure Button_SaveBitmapClick(Sender: TObject);
    procedure Button_CreateBitmapClick(Sender: TObject);
    procedure Button_ShadedClick(Sender: TObject);
    procedure Button_LoadTerrainClick(Sender: TObject);
    procedure Image32_TerrainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32_TerrainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure Button_ShadowClick(Sender: TObject);
    procedure Button_AATerrainClick(Sender: TObject);
    procedure Button_SubDivideClick(Sender: TObject);
    procedure Button_AASweepClick(Sender: TObject);
    procedure SaveMiscBitmapBtnClick(Sender: TObject);
    procedure SandBtnClick(Sender: TObject);
    procedure GrassBtnClick(Sender: TObject);
    procedure RockBtnClick(Sender: TObject);
    procedure SnowBtnClick(Sender: TObject);
    procedure SlopeBtnClick(Sender: TObject);
    procedure SandPanelClick(Sender: TObject);
    procedure ImageTextureSizeRGClick(Sender: TObject);
    procedure GrassPanelClick(Sender: TObject);
    procedure RockPanelClick(Sender: TObject);
    procedure SnowPanelClick(Sender: TObject);
    procedure AttributeNewImageBtnClick(Sender: TObject);
    procedure AttributeLoadBackgroundBtnClick(Sender: TObject);
    procedure AttributeSaveImageBtnClick(Sender: TObject);
    procedure AttributeImageSizeRGClick(Sender: TObject);

    procedure AttributeRGClick(Sender: TObject);
    procedure AttributeColorPanelClick(Sender: TObject);
    procedure AttributeSetBtnClick(Sender: TObject);
    procedure ConvertSlopesBtnClick(Sender: TObject);
    procedure ImageTA32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageTA32MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure ImageTA32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure TAScrollBarChange(Sender: TObject);
    procedure PaintSpot(X, Y: Integer; Radius: single);

    procedure TerrainAttributesTileSizeRGClick(Sender: TObject);
    procedure LayerAlphaTBChange(Sender: TObject);
    procedure LandscapeSizeRGClick(Sender: TObject);
    procedure TSlopeSizeRGClick(Sender: TObject);
    procedure ForestPanelClick(Sender: TObject);
    procedure ForestBtnClick(Sender: TObject);
    procedure AttributeSaveBackgroundBtnClick(Sender: TObject);
    procedure ScrollBar_DepthChange(Sender: TObject);
    procedure ScrollBar_RadiusChange(Sender: TObject);

    procedure abfOpenBtnClick(Sender: TObject);
    procedure abfSaveBtnClick(Sender: TObject);
    procedure agfOpenBtnClick(Sender: TObject);
    procedure agfSaveBtnClick(Sender: TObject);
    procedure amfOpenBtnClick(Sender: TObject);
    procedure amfSaveBtnClick(Sender: TObject);
    procedure TAConvertBtnClick(Sender: TObject);
    procedure TARePaintBtnClick(Sender: TObject);
    procedure AttributeLoadImageBtnClick(Sender: TObject);
    procedure ResetDefaultMemoBtnClick(Sender: TObject);
    procedure AttributeNewBaseImageBtnClick(Sender: TObject);
    procedure AttributeLoadTwixBtnClick(Sender: TObject);
    procedure LayerTwixTBChange(Sender: TObject);
    procedure GridPaintBtnClick(Sender: TObject);
    procedure GridColorPanelClick(Sender: TObject);
    procedure Image_BitmapMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure FlipnSaveBtnClick(Sender: TObject);
    procedure LoadTextureButtonClick(Sender: TObject);
    procedure Slope2ColorPanelClick(Sender: TObject);
    procedure Slope7ColorPanelClick(Sender: TObject);
    procedure Slope15ColorPanelClick(Sender: TObject);
    procedure Slope45ColorPanelClick(Sender: TObject);
    procedure Slope99ColorPanelClick(Sender: TObject);
    procedure AttributeLoadUnitBtnClick(Sender: TObject);
    procedure LayerUnitTBChange(Sender: TObject);

  private
    SandImageName, GrassImageName, ForestImageName, RockImageName,
      SnowImageName, SlopeImageName: string;
    Slope2Color, Slope7Color, Slope15Color, Slope45Color, Slope99Color,
    // clBlack32,clBlue32,clGreen32,clYellow32,clRed32
    GridColor, SandColor, GrassColor, ForestColor, RockColor,
      SnowColor: TColor32;
    TAtileSize, CurrentAttribute: Integer;
    function GetBitmapToUse: TBitmap32;
    { Private declarations }

  public
    { Public declarations }
    HeightMap: THeightMap;
    Mousing: Boolean;

    procedure DoRender;
    procedure SetStatus(s: string);
  end;

var
  frmLandscape: TfrmLandscape;
  StartPath: string;

implementation

uses StrFunctions, AStarGlobals;

{$R *.dfm}
{ TfrmLandscape }

procedure TfrmLandscape.FormCreate(Sender: TObject);
begin
  left := frmLandscapeX;
  top := frmLandscapeY;
  Mousing := False;
  Randomize;
end;

procedure TfrmLandscape.FormShow(Sender: TObject);
begin
  // Show;
  If ((Length(ProjectRecord.MAGValuesFile) > 0) and
    (ProjectRecord.MAGValuesFile <> 'NoName')) then
    if FileExists(ProjectRecord.MAGValuesFile) then
      LoadMAGValues
    else
      ResetMAGDefaults;

  CurrentAttribute := 18; // 0...?
  AttributeColorPanel.Color := MAGColorValueArray[CurrentAttribute][1];
  AttributeEdit.Text := Inttostr(MAGColorValueArray[CurrentAttribute][2]);
  TAtileSize := 1;

  Image32_Terrain.Bitmap.SetSize(Image32_Terrain.Width, Image32_Terrain.Height);
  Image32_Misc.Bitmap.SetSize(Image32_Misc.Width, Image32_Misc.Height);
  Image_Bitmap.Bitmap.SetSize(Image_Bitmap.Width, Image_Bitmap.Height);
  ImageTA32.Bitmap.SetSize(ImageTA32.Width, ImageTA32.Height);

  PageControl1.ActivePageIndex := 0;
  TerrainTabSheet.DoubleBuffered := true;
  HeightMap := THeightMap.Create(Image32_Terrain.Width, Image32_Terrain.Height);
  Button_GO.Click;

  StartPath := ExtractFilePath(ParamStr(0));
  // showmessage(StartPath);
  // StartPath := GetCurrentDir+'\';
  If FileExists(StartPath + 'Textures\ssand.jpg') then
    SandImageName := StartPath + 'Textures\ssand.jpg'
  else
    SandImageName := '';
  If FileExists(StartPath + 'Textures\vgrass.jpg') then
    GrassImageName := StartPath + 'Textures\vgrass.jpg'
  else
    GrassImageName := '';
  If FileExists(StartPath + 'Textures\vforest.jpg') then
    ForestImageName := StartPath + 'Textures\vforest.jpg'
  else
    ForestImageName := '';
  If FileExists(StartPath + 'Textures\srock.jpg') then
    RockImageName := StartPath + 'Textures\srock.jpg'
  else
    RockImageName := '';
  If FileExists(StartPath + 'Textures\hsnow.jpg') then
    SnowImageName := StartPath + 'Textures\hsnow.jpg'
  else
    SnowImageName := '';
  If FileExists(StartPath + 'Textures\srocky.jpg') then
    SlopeImageName := StartPath + 'Textures\srocky.jpg'
  else
    SlopeImageName := '';

  GridColor := clBlack32;
  GridColorPanel.Color := WinColor(GridColor);

  Slope2Color := clBlack32;
  Slope2ColorPanel.Color := WinColor(Slope2Color);
  Slope7Color := clBlue32;
  Slope7ColorPanel.Color := WinColor(Slope7Color);
  Slope15Color := clGreen32;
  Slope15ColorPanel.Color := WinColor(Slope15Color);
  Slope45Color := clYellow32;
  Slope45ColorPanel.Color := WinColor(Slope45Color);
  Slope99Color := clRed32;
  Slope99ColorPanel.Color := WinColor(Slope99Color);

  SandColor := clYellow32;
  SandPanel.Color := WinColor(SandColor);
  GrassColor := clLime32;
  GrassPanel.Color := WinColor(GrassColor);
  ForestColor := clGreen32;
  ForestPanel.Color := WinColor(ForestColor);
  RockColor := clRed32;
  RockPanel.Color := WinColor(RockColor);
  SnowColor := clWhite32;
  SnowPanel.Color := WinColor(SnowColor);
  DoRender;

  AttributeNewImageBtn.Click; // Make the Layers
  StatusBar1.Panels[3].Text := 'X ' + Inttostr(ImageTA32.Width) + ' Y ' +
    Inttostr(ImageTA32.Height) + ' T ' + Inttostr(TAtileSize);
end;

procedure TfrmLandscape.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Mousing := False;
  frmLandscapeX := frmLandscape.left;
  frmLandscapeY := frmLandscape.top;
end;

procedure TfrmLandscape.FormDestroy(Sender: TObject);
begin
  ImageTA32.Layers.Clear;
end;

procedure TfrmLandscape.Button_ClearClick(Sender: TObject);
begin
  HeightMap.Height.Clear;
  DoRender;
end;

procedure TfrmLandscape.Button_GOClick(Sender: TObject);
var
  s: string;
  i: Integer;
  f, a: single;
  X, Y: Integer;
  maxd: single;
  d: single;
begin
  SetStatus('Rendering...');
  Application.ProcessMessages;
  for i := 0 to Memo_Octaves.Lines.Count - 1 do
  begin
    s := Memo_Octaves.Lines[i];

    if s = 'clear' then
    begin
      HeightMap.Height.Clear;
      Continue;
    end;

    if s = 'rescale' then
    begin
      HeightMap.Rescale(0, 1);
      Continue;
    end;

    // Make the heightmap curved /\  bubble as second line
    if s = 'bubble' then
    begin
      maxd := sqrt(sqr(HeightMap.SizeX / 2)); // sqr(HeightMap.SizeY/2));
      for X := 0 to HeightMap.SizeX - 1 do
        for Y := 0 to HeightMap.SizeY - 1 do
        begin
          d := sqrt(sqr(X - HeightMap.SizeX / 2) +
            sqr(Y - HeightMap.SizeY / 2));

          if d > maxd then
            HeightMap.Height[X, Y] := 0
          else
            HeightMap.Height[X, Y] := 2 - 2 * d / maxd;
        end;

      // HeightMap.Rescale(0,1);
      Continue;
    end;

    if s = 'cup' then
    begin
      maxd := sqrt(sqr(HeightMap.SizeX / 2)); // sqr(HeightMap.SizeY/2));
      for X := 0 to HeightMap.SizeX - 1 do
        for Y := 0 to HeightMap.SizeY - 1 do
        begin
          d := sqrt(sqr(X - HeightMap.SizeX / 2) +
            sqr(Y - HeightMap.SizeY / 2));

          if d < maxd then
            HeightMap.Height[X, Y] := 0
          else
            HeightMap.Height[X, Y] := 2 - 2 * d / maxd;
        end;

      // HeightMap.Rescale(0,1);
      Continue;
    end;

    if GetBefore('|', s) = 's' then
    begin
      s := GetAfter('|', s);
      f := StrToFloat(GetBefore('|', s));
      a := StrToFloat(GetAfter('|', s));
      HeightMap.Subdivide(trunc(f), a);
      Continue;
    end;

    if GetBefore('|', s) = 'n' then
    begin
      s := GetAfter('|', s);
      f := StrToFloat(GetBefore('|', s));
      a := StrToFloat(GetAfter('|', s));
      HeightMap.AddNoise(f, a);
      Continue;
    end;

    if GetBefore('|', s) = 'mi' then
    begin
      HeightMap.MakeIsland(StrToFloat(GetAfter('|', s)));
      Continue;
    end;
  end;
  DoRender;
  SetStatus('Done');
  Application.ProcessMessages;
end;

procedure TfrmLandscape.Button_ClampClick(Sender: TObject);
begin
  HeightMap.ClampToLevel(0);
  DoRender;
end;

procedure TfrmLandscape.ResetDefaultMemoBtnClick(Sender: TObject);
begin
  Memo_Octaves.Clear;
  Memo_Octaves.Lines.Add('clear');
  Memo_Octaves.Lines.Add('cup');
  Memo_Octaves.Lines.Add('n|4|0.5');
  Memo_Octaves.Lines.Add('n|8|0.25');
  Memo_Octaves.Lines.Add('n|16|0.125');
  Memo_Octaves.Lines.Add('n|32|0.061');
  Memo_Octaves.Lines.Add('n|64|0.0305');
  Memo_Octaves.Lines.Add('n|128|0.0150');
  Memo_Octaves.Lines.Add('rescale');
end;

procedure TfrmLandscape.Button_MakeIslandClick(Sender: TObject);
var
  Island: single;
begin
  HeightMap.Rescale(0, 1);
  Island := Strtofloatdef(IslandEdit.Text); // 0.1
  HeightMap.MakeIsland(Island);
  HeightMap.Rescale(0, 1);
  // HeightMap.ClampToLevel(0.5);
  DoRender;
end;

procedure TfrmLandscape.Button_RescaleClick(Sender: TObject);
begin
  HeightMap.Rescale(0, 1);
  DoRender;
end;

procedure TfrmLandscape.Button_RenderNormalClick(Sender: TObject);
begin
  DoRender;
end;

procedure TfrmLandscape.Button_SaveTerrainClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Terrain .bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory; // StartPath + BASE_DIR
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
  begin
    HeightMap.RenderTo(Image32_Terrain.Bitmap, False, 0.5);
    Image32_Terrain.Bitmap.SaveToFile(SaveDialog1.FileName);
    // StartPath + BASE_DIR+'terrain.bmp');
  end;
end;

procedure TfrmLandscape.LoadTextureButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Texture .bmp|*.bmp';
  OpenDialog.InitialDir := ProjectDirectory; // media\heightmaps
  OpenDialog.FileName := '';
  if OpenDialog.execute then
  begin
    Image_Bitmap.Bitmap.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TfrmLandscape.Button_SaveBitmapClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Texture .bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory; // StartPath + BASE_DIR
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
    Image_Bitmap.Bitmap.SaveToFile(SaveDialog1.FileName);
  // StartPath + BASE_DIR+'bitmap.bmp');
end;

procedure TfrmLandscape.FlipnSaveBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Texture Flipped.bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory; // StartPath + BASE_DIR
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
  begin
    Image_Bitmap.Bitmap.FlipVert; // FlipHorz
    // Rotate90(Dst: TBitmap32);   180  270
    Image_Bitmap.Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmLandscape.Image_BitmapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  If ((X < 0) or (X > Image_Bitmap.Width) or (Y < 0) or
    (Y > Image_Bitmap.Height)) then
    exit;
  StatusBar1.Panels[1].Text := 'X: ' + Inttostr(X) + ' Y: ' + Inttostr(Y);
end;

procedure TfrmLandscape.GridPaintBtnClick(Sender: TObject);
var
  X, Y, z, zz: Integer;
begin
  // GridLineSizeRG
  case GridSizeRG.itemindex of
    0:
      z := 64;
    1:
      z := 32;
    2:
      z := 20;
    3:
      z := 16;
    4:
      z := 10;
  else
    z := 8;
  end;
  zz := 0;
  For X := 0 to Image_Bitmap.Width - 1 do
  Begin
    inc(zz);
    If (zz mod z = 0) then
    begin
      Image_Bitmap.Bitmap.Line(X, 0, X, Image_Bitmap.Height, GridColor, true);
      zz := 0;
    end;
  End;
  zz := 0;
  For Y := 0 to Image_Bitmap.Height - 1 do
  Begin
    inc(zz);
    If (zz mod z = 0) then
    begin
      Image_Bitmap.Bitmap.Line(0, Y, Image_Bitmap.Width, Y, GridColor, true);
      zz := 0;
    end;
  End;
end;

procedure TfrmLandscape.GridColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(GridColor);
  if ColorDialog1.execute then
  begin
    GridColorPanel.Color := ColorDialog1.Color;
    GridColor := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.SaveMiscBitmapBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Misc bitmap.bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory;
  // StartPath + BASE_DIR
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
    Image32_Misc.Bitmap.SaveToFile(SaveDialog1.FileName);
  // StartPath + BASE_DIR+'Misc.bmp');
end;

procedure TfrmLandscape.LandscapeSizeRGClick(Sender: TObject);
begin
  Case LandscapeSizeRG.itemindex of
    0:
      Begin
        Image32_Terrain.Width := 256;
        Image32_Terrain.Height := 256;
      End;
    1:
      Begin
        Image32_Terrain.Width := 512;
        Image32_Terrain.Height := 512;
      End;
    2:
      Begin
        Image32_Terrain.Width := 1024;
        Image32_Terrain.Height := 1024;
      End;
    3:
      Begin
        Image32_Terrain.Width := 2048;
        Image32_Terrain.Height := 2048;
      End;
    4:
      Begin
        Image32_Terrain.Width := 4096;
        Image32_Terrain.Height := 4096;
      End;
  End;
  // Remake the HeightField...
  Image32_Terrain.Bitmap.SetSize(Image32_Terrain.Width, Image32_Terrain.Height);
  HeightMap := THeightMap.Create(Image32_Terrain.Width, Image32_Terrain.Height);
  Button_GO.Click;
end;

procedure TfrmLandscape.TSlopeSizeRGClick(Sender: TObject);
begin
  Case TSlopeSizeRG.itemindex of
    0:
      Begin
        Image32_Misc.Width := 256;
        Image32_Misc.Height := 256;
      End;
    1:
      Begin
        Image32_Misc.Width := 512;
        Image32_Misc.Height := 512;
      End;
    2:
      Begin
        Image32_Misc.Width := 1024;
        Image32_Misc.Height := 1024;
      End;
    3:
      Begin
        Image32_Misc.Width := 2048;
        Image32_Misc.Height := 2048;
      End;
    4:
      Begin
        Image32_Misc.Width := 4096;
        Image32_Misc.Height := 4096;
      End;
  End;
  Image32_Misc.Bitmap.SetSize(Image32_Misc.Width, Image32_Misc.Height);
end;

procedure TfrmLandscape.ImageTextureSizeRGClick(Sender: TObject);
begin
  Case ImageTextureSizeRG.itemindex of
    0:
      Begin
        Image_Bitmap.Width := 512;
        Image_Bitmap.Height := 512;
      End;
    1:
      Begin
        Image_Bitmap.Width := 1024;
        Image_Bitmap.Height := 1024;
      End;
    2:
      Begin
        Image_Bitmap.Width := 2048;
        Image_Bitmap.Height := 2048;
      End;
    3:
      Begin
        Image_Bitmap.Width := 4096;
        Image_Bitmap.Height := 4096;
      End;
  End;
  Image_Bitmap.Bitmap.SetSize(Image_Bitmap.Width, Image_Bitmap.Height);
end;

procedure TfrmLandscape.Button_CreateBitmapClick(Sender: TObject);
var
  ColorModifierHandler: TColorModifierHandler;
  CMSand, CMGrass, CMForest, CMRock, CMSnow: TColorModifierHeight;
  CMSlope: TColorModifierSlope;
  // Bitmap_Output  : TBitmap32;
  // x,y : integer;
  // hx,hy, Height, Slope : single;
  In1, In2, In3, In4: single;
begin
  SetStatus('Rendering...');
  Application.ProcessMessages;
  HeightMap.Rescale(0, 1);

  ColorModifierHandler := TColorModifierHandler.Create;
  ColorModifierHandler.HeightMap := HeightMap;

  // PageControl1.ActivePageIndex := 1;

  /// SandImageName:=StartPath+'Textures\terrain0.jpg'
  /// GrassImageName:=StartPath+'Textures\terrain1.jpg'
  /// DirtImageName:=StartPath+'Textures\terrain2.jpg'
  /// SnowImageName:=StartPath+'Textures\terrain3.jpg'
  /// SlopeImageName:=StartPath+''Textures\rock.jpg''
  CMSand := TColorModifierHeight.Create(SandImageName);
  CMSand.SingleColor := SandColor; // clYellow32;
  In1 := StrtoFloatDef(SandEdit1.Text);
  In2 := StrtoFloatDef(SandEdit2.Text);
  In3 := StrtoFloatDef(SandEdit3.Text);
  In4 := StrtoFloatDef(SandEdit4.Text);
  CMSand.SetupCurve(In1, In2, In3, In4); // -2,-1,0.2,0.3);
  ColorModifierHandler.Add(CMSand);
  // CMSand.SingleColor := clYellow32;  CMSand.SetupCurve(-2,-1,0.2,0.3);
  // (StartHeight, FullOnHeight, StopHeight, FullOffHeight: single);
  CMGrass := TColorModifierHeight.Create(GrassImageName);
  CMGrass.SingleColor := GrassColor; // clGreen32;
  In1 := StrtoFloatDef(GrassEdit1.Text);
  In2 := StrtoFloatDef(GrassEdit2.Text);
  In3 := StrtoFloatDef(GrassEdit3.Text);
  In4 := StrtoFloatDef(GrassEdit4.Text);
  CMGrass.SetupCurve(In1, In2, In3, In4); // 0.2,0.3,0.5,0.6);
  ColorModifierHandler.Add(CMGrass);
  // CMGrass.SingleColor := clGreen32;  CMGrass.SetupCurve(0.2,0.3,0.5,0.6);

  CMForest := TColorModifierHeight.Create(ForestImageName);
  CMForest.SingleColor := ForestColor; // clRed32;
  In1 := StrtoFloatDef(ForestEdit1.Text);
  In2 := StrtoFloatDef(ForestEdit2.Text);
  In3 := StrtoFloatDef(ForestEdit3.Text);
  In4 := StrtoFloatDef(ForestEdit4.Text);
  CMForest.SetupCurve(In1, In2, In3, In4); // 0.5,0.6,0.9,0.95);
  ColorModifierHandler.Add(CMForest);

  CMRock := TColorModifierHeight.Create(RockImageName);
  CMRock.SingleColor := RockColor; // clRed32;
  In1 := StrtoFloatDef(RockEdit1.Text);
  In2 := StrtoFloatDef(RockEdit2.Text);
  In3 := StrtoFloatDef(RockEdit3.Text);
  In4 := StrtoFloatDef(RockEdit4.Text);
  CMRock.SetupCurve(In1, In2, In3, In4); // 0.5,0.6,0.9,0.95);
  ColorModifierHandler.Add(CMRock);
  // CMRock.SingleColor := clRed32;  CMRock.SetupCurve(0.5,0.6,0.9,0.95);

  CMSnow := TColorModifierHeight.Create(SnowImageName);
  CMSnow.SingleColor := SnowColor; // clWhite32;
  In1 := StrtoFloatDef(SnowEdit1.Text);
  In2 := StrtoFloatDef(SnowEdit2.Text);
  In3 := StrtoFloatDef(SnowEdit3.Text);
  In4 := StrtoFloatDef(SnowEdit4.Text);
  CMSnow.SetupCurve(In1, In2, In3, In4); // 0.9,0.95,2,2);
  ColorModifierHandler.Add(CMSnow);
  // CMSnow.SingleColor := clWhite32;  CMSnow.SetupCurve(0.9,0.95,2,2);

  CMSlope := TColorModifierSlope.Create(SlopeImageName);
  ColorModifierHandler.Add(CMSlope); //

  // Bitmap_Output := Image_Bitmap.Bitmap;

  Image_Bitmap.Bitmap.SetSize(Image_Bitmap.Width, Image_Bitmap.Height);

  // ColorModifierHandler.NoTexture := true;
  ColorModifierHandler.Ambient := StrToFloat(Edit_Ambient.Text);
  ColorModifierHandler.Shaded := CheckBox_Shaded.Checked;
  if ColorModifierHandler.Shaded then
  begin
    SetStatus('Creating slopes...');
    HeightMap.SetupSlope(0.0);

    SetStatus('Creating shading...');
    HeightMap.SetupShade;

    SetStatus('Creating shadows...');
    HeightMap.SetupShadows;
  end;

  SetStatus('Rendering...');
  ColorModifierHandler.RenderToBitmap(Image_Bitmap.Bitmap);
  SetStatus('Done');

  ColorModifierHandler.Free;
end;

procedure TfrmLandscape.DoRender;
var
  InWater: single;
begin
  SetStatus('Rendering...');
  Application.ProcessMessages;
  InWater := StrtoFloatDef(WaterEdit.Text);
  HeightMap.RenderTo(Image32_Terrain.Bitmap, // 0.2
    CheckBox_RenderWater.Checked, InWater);
  SetStatus('Done');
  Application.ProcessMessages;
end;

function TfrmLandscape.GetBitmapToUse: TBitmap32;
begin
  // result := Image32_Misc.Bitmap
  if SlopeCheckBox.Checked then
    result := Image32_Misc.Bitmap
  else
  begin
    PageControl1.ActivePageIndex := 2;
    result := Image_Bitmap.Bitmap;
  end;
end;

procedure TfrmLandscape.Button_SlopesClick(Sender: TObject);
begin
  SetStatus('Rendering...');
  Application.ProcessMessages;
  HeightMap.SetupSlope(StrtoFloatDef(SlopeEdit.Text) { 0.3 } );
  HeightMap.RenderSlopesTo(GetBitmapToUse);
  SlopeMaxLabel.Caption := Floattostr(HeightMap.GetSlopeMax);
  SlopeMinLabel.Caption := Floattostr(HeightMap.GetSlopeMin);
  SetStatus('Done');
  Application.ProcessMessages;
end;

procedure TfrmLandscape.Button_ShadedClick(Sender: TObject);
begin
  HeightMap.SetupShade;
  HeightMap.RenderShadedTo(GetBitmapToUse, StrToFloat(Edit_Ambient.Text));
end;

procedure TfrmLandscape.Button_AATerrainClick(Sender: TObject);
begin
  HeightMap.Height.AntiAlias(0.1);
  DoRender;
  Button_Shaded.Click;
end;

procedure TfrmLandscape.Button_AASweepClick(Sender: TObject);
begin
  HeightMap.Height.AntiAliasSweep;
  DoRender;
  Button_Shaded.Click;
end;

procedure TfrmLandscape.Button_ShadowClick(Sender: TObject);
begin
  SetStatus('Creating shadows...');
  HeightMap.SetupShadows;

  SetStatus('Rendering shadows...');
  HeightMap.RenderShadowTo(GetBitmapToUse);

  SetStatus('Done');
end;

procedure TfrmLandscape.Button_LoadTerrainClick(Sender: TObject);
begin // OpenDialog_Terrain
  OpenDialog.Filter := 'Height Maps (bmp jpg)|*.bmp;*.jpg';
  // OpenDialog.Filter:= 'Height Maps|*.jpg';
  OpenDialog.InitialDir := ProjectDirectory; // media\heightmaps
  OpenDialog.FileName := '';
  if OpenDialog.execute then
  begin
    // HeightMap.LoadFromFile('media\HeightMap.jpg');
    HeightMap.LoadFromFile(OpenDialog.FileName);
    HeightMap.Rescale(0, 1);
    DoRender;
  end;
end;

procedure TfrmLandscape.Image32_TerrainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Mult: Integer;
begin
  if ssRight in Shift then
    Mult := 1
  else
    Mult := -1;

  if CheckBox_Box.Checked then
    HeightMap.BoxLower(X, Y, Mult * ScrollBar_Depth.Position / 100,
      ScrollBar_Radius.Position)
  else
    HeightMap.CircleLower(X, Y, Mult * ScrollBar_Depth.Position / 100,
      ScrollBar_Radius.Position);

  DoRender;
end;

{ var
  oldX : integer=-100;
  oldY : integer=-100; }
procedure TfrmLandscape.Image32_TerrainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Mult: Integer;
  // r : integer;
begin
  if ssRight in Shift then
    Mult := 1
  else
    Mult := -1;

  if (ssLeft in Shift) or (ssRight in Shift) then
  begin
    HeightMap.CircleLower(X, Y, Mult * ScrollBar_Depth.Position / 100,
      ScrollBar_Radius.Position);
    DoRender;
  end
  else
  begin
    // r := ScrollBar_Radius.Position;
  end;
end;

procedure TfrmLandscape.SetStatus(s: string);
begin
  StatusBar1.Panels[3].Text := s;
end;

procedure TfrmLandscape.ScrollBar_DepthChange(Sender: TObject);
begin
  DepthLabel.Caption := Inttostr(ScrollBar_Depth.Position);
end;

procedure TfrmLandscape.ScrollBar_RadiusChange(Sender: TObject);
begin
  RadiusLabel.Caption := Inttostr(ScrollBar_Radius.Position);
end;

procedure TfrmLandscape.Button_SubDivideClick(Sender: TObject);
begin
  SetStatus('Subdividing...');
  HeightMap.Subdivide(500, 0.001);
  DoRender;
  SetStatus('Done');
end;

/// SandImageName:=StartPath+'Textures\terrain0.jpg'
/// GrassImageName:=StartPath+'Textures\terrain1.jpg'
/// DirtImageName:=StartPath+'Textures\terrain2.jpg'
/// SnowImageName:=StartPath+'Textures\terrain3.jpg'
/// SlopeImageName:=StartPath+''Textures\rock.jpg''
procedure TfrmLandscape.SandBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Sand Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := SandImageName;
  if OpenDialog.execute then
    SandImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.SandPanelClick(Sender: TObject);
begin
  // function WinColor(Color32: TColor32): TColor;
  // function Color32(WinColor: TColor): TColor32; overload;
  ColorDialog1.Color := WinColor(SandColor);
  if ColorDialog1.execute then
  begin
    SandPanel.Color := ColorDialog1.Color;
    SandColor := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.GrassBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Grass Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := GrassImageName;
  if OpenDialog.execute then
    GrassImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.ForestBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Forest Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := ForestImageName;
  if OpenDialog.execute then
    ForestImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.RockBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Rock Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := RockImageName;
  if OpenDialog.execute then
    RockImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.SnowBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Snow Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := SnowImageName;
  if OpenDialog.execute then
    SnowImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.SlopeBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Slope Image (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := StartPath + 'Textures';
  OpenDialog.FileName := SlopeImageName;
  if OpenDialog.execute then
    SlopeImageName := OpenDialog.FileName;
end;

procedure TfrmLandscape.GrassPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(GrassColor);
  if ColorDialog1.execute then
  begin
    GrassPanel.Color := ColorDialog1.Color;
    GrassColor := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.ForestPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(ForestColor);
  if ColorDialog1.execute then
  begin
    ForestPanel.Color := ColorDialog1.Color;
    ForestColor := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.RockPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(RockColor);
  if ColorDialog1.execute then
  begin
    RockPanel.Color := ColorDialog1.Color;
    RockColor := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.SnowPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(SnowColor);
  if ColorDialog1.execute then
  begin
    SnowPanel.Color := ColorDialog1.Color;
    SnowColor := Color32(ColorDialog1.Color);
  end;
end;

/// /////////////////////////////////////////////////
/// /////////////////////////////////////////////////
procedure TfrmLandscape.Slope2ColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(Slope2Color);
  if ColorDialog1.execute then
  begin
    Slope2ColorPanel.Color := ColorDialog1.Color;
    Slope2Color := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.Slope7ColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(Slope7Color);
  if ColorDialog1.execute then
  begin
    Slope7ColorPanel.Color := ColorDialog1.Color;
    Slope7Color := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.Slope15ColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(Slope15Color);
  if ColorDialog1.execute then
  begin
    Slope15ColorPanel.Color := ColorDialog1.Color;
    Slope15Color := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.Slope45ColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(Slope45Color);
  if ColorDialog1.execute then
  begin
    Slope45ColorPanel.Color := ColorDialog1.Color;
    Slope45Color := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.Slope99ColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := WinColor(Slope99Color);
  if ColorDialog1.execute then
  begin
    Slope99ColorPanel.Color := ColorDialog1.Color;
    Slope99Color := Color32(ColorDialog1.Color);
  end;
end;

procedure TfrmLandscape.ConvertSlopesBtnClick(Sender: TObject);
var
  f: File of Integer; // Byte;
  b: byte;
  Slope2, Slope7, Slope15, Slope45, Slope99, X, Y, z, c: Integer;
  s: String;
  // SlopeArray:Array of Array of Byte;
begin
  Slope2 := Strtoint(Slope2Edit.Text);
  Slope7 := Strtoint(Slope7Edit.Text);
  Slope15 := Strtoint(Slope15Edit.Text);
  Slope45 := Strtoint(Slope45Edit.Text);
  Slope99 := Strtoint(Slope99Edit.Text);

  SaveDialog1.Filter := 'astar Elevation Slopes.aef|*.aef';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then // Image32_Misc
  begin
    AssignFile(f, SaveDialog1.FileName);
    Rewrite(f);
    write(f, Image32_Misc.Bitmap.Width);
    write(f, Image32_Misc.Bitmap.Height);
    // Setlength(SlopeArray,Image32_Misc.Bitmap.Width ,Image32_Misc.Bitmap.Height );
    for Y := 0 to Image32_Misc.Bitmap.Height - 1 do
      for X := 0 to Image32_Misc.Bitmap.Width - 1 do
      begin
        z := WinColor(Image32_Misc.Bitmap.Pixels[X, Y]);
        b := GetRValue(z);
        Case b of
          0 .. 29:
            begin
              c := Slope2;
              Image32_Misc.Bitmap.Pixel[X, Y] := Slope2Color;
            end;
          30 .. 79:
            begin
              c := Slope7;
              Image32_Misc.Bitmap.Pixel[X, Y] := Slope7Color;
            end;
          80 .. 110:
            begin
              c := Slope15;
              Image32_Misc.Bitmap.Pixel[X, Y] := Slope15Color;
            end;
          111 .. 180:
            begin
              c := Slope45;
              Image32_Misc.Bitmap.Pixels[X, Y] := Slope45Color;
            end;
        else
          begin
            c := Slope99;
            Image32_Misc.Bitmap.Pixels[X, Y] := Slope99Color;
          end;
        end;
        // Slope2Color,Slope7Color,Slope15Color,Slope45Color,Slope99Color
        // clBlack32,clBlue32,clGreen32,clYellow32,clRed32
        // SlopeArray[x,y]:= c;
        write(f, c);
      end;
  end;
  CloseFile(f);
  // Redraw the new colors
  Image32_Misc.Bitmap.Changed;
  // Save the data image
  s := copy(SaveDialog1.FileName, 0, Length(SaveDialog1.FileName) - 4);
  s := s + 'aef.bmp';
  Image32_Misc.Bitmap.SaveToFile(s);
  // Setlength(SlopeArray,0,0);
end;

/// /////////////////////////////////////////////////
/// /////////////////////////////////////////////////
procedure TfrmLandscape.AttributeNewImageBtnClick(Sender: TObject);
var
  BitMap1: TBitMap;
  ALayer: TBitmapLayer;
  L: TFloatRect;
  i: Integer;
begin
  ImageTA32.Layers.Clear;
  ImageTA32.BeginUpdate;
  BitMap1 := TBitMap.Create;
  try
    BitMap1.Width := ImageTA32.Width;
    BitMap1.Height := ImageTA32.Height;
    BitMap1.Pixelformat := pf32bit;
    BitMap1.Canvas.Brush.Color := clBlack;
    BitMap1.Canvas.Pen.Color := clBlack;
    BitMap1.Canvas.Brush.Style := bsSolid;
    BitMap1.Canvas.FillRect(Rect(0, 0, ImageTA32.Width, ImageTA32.Height));
    ImageTA32.Bitmap.Assign(BitMap1);
    { ImageTA32.bitmap.Canvas.Brush.Color := MAGColorValueArray[CurrentAttribute][1];
      ImageTA32.bitmap.Canvas.Pen.Color := MAGColorValueArray[CurrentAttribute][1];
      ImageTA32.bitmap.Canvas.Brush.Style := bsSolid; }

    for i := 0 to 2 do // Map , Twix,Unit overlays //6 layers
    begin
      // create a new layer...
      ALayer := TBitmapLayer.Create(ImageTA32.Layers);
      with ALayer do
      begin
        Bitmap.Assign(BitMap1);
        Bitmap.DrawMode := dmBlend;
        If i = 1 then
          Bitmap.MasterAlpha := 0
        else
          Bitmap.MasterAlpha := 254;
        // put it somethere
        L.left := 0; // Random(Image32.Width);
        L.top := 0; // Random(Image32.Height);
        L.Right := ImageTA32.Width; // L.Left + Bitmap.Width;
        L.Bottom := ImageTA32.Height; // L.Top + Bitmap.Height;
        ALayer.Location := L;
      end;
    end;
    ImageTA32.EndUpdate;
    ImageTA32.Changed;
  finally
    BitMap1.Free;
  end;
  LayerAlphaTB.Position := 254;
  LayerTwixTB.Position := 0;
  LayerUnitTB.Position := 0;
  Setlength(GCostData, (ImageTA32.Width div TAtileSize) + 1,
    (ImageTA32.Height div TAtileSize) + 1);
  Setlength(MapAttribute, (ImageTA32.Width div TAtileSize) + 1,
    (ImageTA32.Height div TAtileSize) + 1);
  { mapWidth:= ImageTA32.Width div TAtileSize;
    mapHeight:= ImageTA32.Height div TAtileSize;
    EndPathfinder;
    InitializePathfinder; }
  StatusBar1.Panels[3].Text := 'X ' + Inttostr(ImageTA32.Width) + ' Y ' +
    Inttostr(ImageTA32.Height) + ' T ' + Inttostr(TAtileSize);
end;

procedure TfrmLandscape.AttributeLoadBackgroundBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Texture (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := ProjectDirectory;
  OpenDialog.FileName := '';
  if OpenDialog.execute then
  begin //
    ImageTA32.Bitmap.LoadFromFile(OpenDialog.FileName);
    // ImageTA32.bitmap.Pixelformat :=pf24bit;
    ImageTA32.Width := ImageTA32.Bitmap.Width;
    ImageTA32.Height := ImageTA32.Bitmap.Height;
    { ImageTA32.bitmap.Canvas.Brush.Color := MAGColorValueArray[CurrentAttribute][1];
      ImageTA32.bitmap.Canvas.Pen.Color := MAGColorValueArray[CurrentAttribute][1];
      ImageTA32.bitmap.Canvas.Brush.Style := bsSolid; }
    Case ImageTA32.Width of
      256:
        AttributeImageSizeRG.itemindex := 0;
      512:
        AttributeImageSizeRG.itemindex := 1;
      1024:
        AttributeImageSizeRG.itemindex := 2;
      2048:
        AttributeImageSizeRG.itemindex := 3;
      4096:
        AttributeImageSizeRG.itemindex := 4;
    End;
    TBitmapLayer(ImageTA32.Layers[0 { I } ]).Bitmap.Width := ImageTA32.Width;
    TBitmapLayer(ImageTA32.Layers[0 { I } ]).Bitmap.Height := ImageTA32.Height;
    ImageTA32.Invalidate;
    ImageTA32.Bitmap.LoadFromFile(OpenDialog.FileName);
    Setlength(GCostData, ImageTA32.Width div TAtileSize,
      ImageTA32.Height div TAtileSize);
    Setlength(MapAttribute, ImageTA32.Width div TAtileSize,
      ImageTA32.Height div TAtileSize);
    { mapWidth:= ImageTA32.Width div TAtileSize;
      mapHeight:= ImageTA32.Height div TAtileSize;
      EndPathfinder;
      InitializePathfinder; }
    StatusBar1.Panels[3].Text := 'X ' + Inttostr(ImageTA32.Width) + ' Y ' +
      Inttostr(ImageTA32.Height) + ' T ' + Inttostr(TAtileSize);
  end;
end;

procedure TfrmLandscape.AttributeSaveBackgroundBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Texture .bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
  begin
    ImageTA32.Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmLandscape.AttributeLoadImageBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Texture (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := ProjectDirectory;
  OpenDialog.FileName := '';
  if OpenDialog.execute then
    with TBitmapLayer(ImageTA32.Layers[0]) do
      Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmLandscape.AttributeSaveImageBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Attribute .bmp|*.bmp';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then
  begin
    with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
      Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmLandscape.AttributeLoadTwixBtnClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Texture (bmp jpg)|*.bmp;*.jpg';
  OpenDialog.InitialDir := ProjectDirectory;
  OpenDialog.FileName := '';
  if OpenDialog.execute then
    with TBitmapLayer(ImageTA32.Layers[1]) do
      Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmLandscape.LayerTwixTBChange(Sender: TObject);
begin
  with TBitmapLayer(ImageTA32.Layers[1]) do
    Bitmap.MasterAlpha := LayerTwixTB.Position; // 254;
end;

procedure TfrmLandscape.AttributeLoadUnitBtnClick(Sender: TObject);
var
  PaintColor: TColor32;
  X, z: Integer;
begin
  z := trunc(LayerUnitSizeTB.Position / 2);
  For X := 1 to ProjectRecord.NumberofActiveUnits do
  Begin
    PaintColor := Color32(BaseStartColorArray[X]);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[X].startXLoc * TAtileSize) - (z),
        (UnitRecordArray[X].startYLoc * TAtileSize) - (z),
        (UnitRecordArray[X].startXLoc * TAtileSize) + (z),
        (UnitRecordArray[X].startYLoc * TAtileSize) + (z), PaintColor);

    PaintColor := Color32(TargetGoalColorArray[X]);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[X].targetX * TAtileSize) - (z),
        (UnitRecordArray[X].targetY * TAtileSize) - (z),
        (UnitRecordArray[X].targetX * TAtileSize) + (z),
        (UnitRecordArray[X].targetY * TAtileSize) + (z), PaintColor);
  end;
  If ProjectRecord.NumberofActiveEnemy > 0 then
  begin
    PaintColor := Color32(EnemyUnitBaseColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].startXLoc * TAtileSize) - (z),
        (UnitRecordArray[6].startYLoc * TAtileSize) - (z),
        (UnitRecordArray[6].startXLoc * TAtileSize) + (z),
        (UnitRecordArray[6].startYLoc * TAtileSize) + (z), PaintColor);
    PaintColor := Color32(EnemyUnitGoalTargetColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].targetX * TAtileSize) - (z),
        (UnitRecordArray[6].targetY * TAtileSize) - (z),
        (UnitRecordArray[6].targetX * TAtileSize) + (z),
        (UnitRecordArray[6].targetY * TAtileSize) + (z), PaintColor);

    PaintColor := Color32(EnemyPositionsColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].Members1X * TAtileSize) - (z),
        (UnitRecordArray[6].Members1Y * TAtileSize) - (z),
        (UnitRecordArray[6].Members1X * TAtileSize) + (z),
        (UnitRecordArray[6].Members1Y * TAtileSize) + (z), PaintColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].Members2X * TAtileSize) - (z),
        (UnitRecordArray[6].Members2Y * TAtileSize) - (z),
        (UnitRecordArray[6].Members2X * TAtileSize) + (z),
        (UnitRecordArray[6].Members2Y * TAtileSize) + (z), PaintColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].Members3X * TAtileSize) - (z),
        (UnitRecordArray[6].Members3Y * TAtileSize) - (z),
        (UnitRecordArray[6].Members3X * TAtileSize) + (z),
        (UnitRecordArray[6].Members3Y * TAtileSize) + (z), PaintColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].Members4X * TAtileSize) - (z),
        (UnitRecordArray[6].Members4Y * TAtileSize) - (z),
        (UnitRecordArray[6].Members4X * TAtileSize) + (z),
        (UnitRecordArray[6].Members4Y * TAtileSize) + (z), PaintColor);
    with TBitmapLayer(ImageTA32.Layers[2]) do
      Bitmap.FillRectS((UnitRecordArray[6].Members5X * TAtileSize) - (z),
        (UnitRecordArray[6].Members5Y * TAtileSize) - (z),
        (UnitRecordArray[6].Members5X * TAtileSize) + (z),
        (UnitRecordArray[6].Members5Y * TAtileSize) + (z), PaintColor);
  end;
end;

procedure TfrmLandscape.LayerUnitTBChange(Sender: TObject);
begin
  with TBitmapLayer(ImageTA32.Layers[2]) do
    Bitmap.MasterAlpha := LayerUnitTB.Position; // 0..254;
end;

procedure TfrmLandscape.AttributeImageSizeRGClick(Sender: TObject);
begin
  Case AttributeImageSizeRG.itemindex of
    0:
      Begin
        ImageTA32.Width := 256;
        ImageTA32.Height := 256;
      End;
    1:
      Begin
        ImageTA32.Width := 512;
        ImageTA32.Height := 512;
      End;
    2:
      Begin
        ImageTA32.Width := 1024;
        ImageTA32.Height := 1024;
      End;
    3:
      Begin
        ImageTA32.Width := 2048;
        ImageTA32.Height := 2048;
      End;
    4:
      Begin
        ImageTA32.Width := 4096;
        ImageTA32.Height := 4096;
      End;
  End;
  AttributeNewImageBtn.Click;
end;

procedure TfrmLandscape.TerrainAttributesTileSizeRGClick(Sender: TObject);
begin
  // TileSize50x50   TileSize25x25  TileSize10x10
  case TerrainAttributesTileSizeRG.itemindex of
    0:
      begin
        TAtileSize := 32;
      End;
    1:
      begin
        TAtileSize := 16;
      End;
    2:
      begin
        TAtileSize := 8;
      End;
    3:
      begin
        TAtileSize := 4;
      End;
    4:
      begin
        TAtileSize := 2;
      End;
    5:
      begin
        TAtileSize := 1;
      End;
  end; // ResizeImage;       //TAtileSize
  Setlength(GCostData, ImageTA32.Width div TAtileSize,
    ImageTA32.Height div TAtileSize);
  Setlength(MapAttribute, ImageTA32.Width div TAtileSize,
    ImageTA32.Height div TAtileSize);

  StatusBar1.Panels[3].Text := 'X ' + Inttostr(ImageTA32.Width) + ' Y ' +
    Inttostr(ImageTA32.Height) + ' T ' + Inttostr(TAtileSize);
end;

// Image1MouseUp   If Mousing then Mousing:=False;
procedure TfrmLandscape.AttributeRGClick(Sender: TObject);
var
  s: String;
begin // RGB(0,0,255);
  CurrentAttribute := AttributeRG.itemindex;
  AttributeEdit.Text := Inttostr(MAGColorValueArray[CurrentAttribute][2]);
  AttributeColorPanel.Color := MAGColorValueArray[CurrentAttribute][1];
  Case AttributeRG.itemindex of
    0 .. 3:
      s := 'Trans Layer';
    4 .. 11:
      s := 'Soils Layer';
    12 .. 16:
      s := 'Veg Layer';
    17 .. 22:
      s := 'Hydro Layer';
    23 .. 24:
      s := 'Urban Layer';
  else
    s := 'Obstacle Layer';
  end;
  StatusBar1.Panels[0].Text := s; // TAtileSize
end;

procedure TfrmLandscape.AttributeColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := AttributeColorPanel.Color;
  if ColorDialog1.execute then
  begin
    AttributeColorPanel.Color := ColorDialog1.Color;
  end;
end;

procedure TfrmLandscape.AttributeSetBtnClick(Sender: TObject);
begin // Case AttributeRG.Itemindex of
  MAGColorValueArray[CurrentAttribute][2] := Strtoint(AttributeEdit.Text);
  MAGColorValueArray[CurrentAttribute][1] := AttributeColorPanel.Color;
  // 17 items   AttributeColorArray   AttributeValueArray
end;

procedure TfrmLandscape.LayerAlphaTBChange(Sender: TObject);
begin
  with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
    Bitmap.MasterAlpha := LayerAlphaTB.Position; // 254;
end;

procedure TfrmLandscape.ImageTA32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
// var i:Integer;
begin
  If Mousing then
    exit;
  If ((X < 0) or (X > ImageTA32.Width) or (Y < 0) or (Y > ImageTA32.Height))
  then
    exit;

  If Button = mbLeft then // mbRight
  begin
    Mousing := true;
    If TABigBrushCB.Checked then
      PaintSpot(X, Y, TAScrollBar.Position)
    else
    Begin
      If PaintMapCB.Checked then
      begin
        GCostData[X div TAtileSize, Y div TAtileSize] := MAGColorValueArray
          [CurrentAttribute][2];
        MapAttribute[X div TAtileSize, Y div TAtileSize] := CurrentAttribute;
        // Draw Box at Tile location IN the CORRECT LAYER
        // ImageTA32.BeginUpdate;
        { Case CurrentAttribute of
          0..3:I:=0;
          4..11:I:=1;
          12..16:I:=2;
          17..22:I:=3;
          23..24:I:=4;
          else i:=5;
          End;//Case }
        with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
          { ImageTA32. }
          Bitmap.FillRectS(((X div TAtileSize) * TAtileSize),
            ((Y div TAtileSize) * TAtileSize),
            (((X div TAtileSize) * TAtileSize) + TAtileSize),
            (((Y div TAtileSize) * TAtileSize) + TAtileSize),
            Color32(MAGColorValueArray[CurrentAttribute][1]));
        // ImageTA32.EndUpdate;
        // ImageTA32.Invalidate;
        // Mousing:=False;
      end
      else
        ImageTA32.Bitmap.FillRectS(((X div TAtileSize) * TAtileSize),
          ((Y div TAtileSize) * TAtileSize),
          (((X div TAtileSize) * TAtileSize) + TAtileSize),
          (((Y div TAtileSize) * TAtileSize) + TAtileSize),
          Color32(MAGColorValueArray[CurrentAttribute][1]));
    end;
  end; // WinColor

end;

procedure TfrmLandscape.ImageTA32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
  If ((X < 0) or (X > ImageTA32.Width) or (Y < 0) or (Y > ImageTA32.Height))
  then
    exit;

  StatusBar1.Panels[1].Text := 'X: ' + Inttostr(X) + ' Y: ' + Inttostr(Y);
  StatusBar1.Panels[2].Text := 'X: ' + Inttostr((X div TAtileSize) + 1) + ' Y: '
    + Inttostr((Y div TAtileSize) + 1);

  // If Mousing  then exit;
  If Mousing { Button=mbLeft } then // mbRight
  begin
    If TABigBrushCB.Checked then
      PaintSpot(X, Y, TAScrollBar.Position)
    else
    Begin
      If PaintMapCB.Checked then
      begin
        GCostData[X div TAtileSize, Y div TAtileSize] := MAGColorValueArray
          [CurrentAttribute][2];
        MapAttribute[X div TAtileSize, Y div TAtileSize] := CurrentAttribute;
        // Draw Box at Tile location IN the CORRECT LAYER
        // ImageTA32.BeginUpdate;
        { Case CurrentAttribute of
          0..3:I:=0;
          4..11:I:=1;
          12..16:I:=2;
          17..22:I:=3;
          23..24:I:=4;
          else i:=5;
          End;//Case }
        with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
          { ImageTA32. }
          Bitmap.FillRectS(((X div TAtileSize) * TAtileSize),
            ((Y div TAtileSize) * TAtileSize),
            (((X div TAtileSize) * TAtileSize) + TAtileSize),
            (((Y div TAtileSize) * TAtileSize) + TAtileSize),
            Color32(MAGColorValueArray[CurrentAttribute][1]));
        // ImageTA32.EndUpdate;
        // ImageTA32.Invalidate;
        // Mousing:=False;
      end
      else
        ImageTA32.Bitmap.FillRectS(((X div TAtileSize) * TAtileSize),
          ((Y div TAtileSize) * TAtileSize),
          (((X div TAtileSize) * TAtileSize) + TAtileSize),
          (((Y div TAtileSize) * TAtileSize) + TAtileSize),
          Color32(MAGColorValueArray[CurrentAttribute][1]));
    End;
  end; // WinColor
end;

procedure TfrmLandscape.ImageTA32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  Mousing := False;
end;

procedure TfrmLandscape.TAScrollBarChange(Sender: TObject);
begin
  TARadiusLabel.Caption := Inttostr(TAScrollBar.Position);
end;

procedure TfrmLandscape.PaintSpot(X, Y: Integer; Radius: single);
var
  gx, gy, iRadius: Integer;
  dist2: single;
  Radius2: single;
begin
  Radius2 := sqr(Radius);
  iRadius := trunc(Radius);
  // ImageTA32.BeginUpdate;
  for gx := X - iRadius to X + iRadius do
  begin
    if gx < 0 then
      Continue; // Skip it for this value
    if gx >= ImageTA32.Width then
      Continue;
    for gy := Y - iRadius to Y + iRadius do
    begin
      if gy < 0 then
        Continue;
      if gy >= ImageTA32.Height then
        Continue;
      dist2 := sqr(X - gx) + sqr(Y - gy);
      // Paint a Circle NOT a Square
      if dist2 >= Radius2 then
        Continue;

      If PaintMapCB.Checked then
      begin

        with TBitmapLayer(ImageTA32.Layers[0]) do
        begin
          Bitmap.BeginUpdate;
          Bitmap[gx, gy] := Color32(MAGColorValueArray[CurrentAttribute][1]);
          Bitmap.EndUpdate;
          Bitmap.Changed;
        end
      end
      else
        ImageTA32.Bitmap[gx, gy] :=
          Color32(MAGColorValueArray[CurrentAttribute][1]);

    end;
    // ImageTA32.EndUpdate;
    // ImageTA32.Invalidate;
  end;
end;

/// ////////////////////////////////////////
procedure TfrmLandscape.abfOpenBtnClick(Sender: TObject);
begin
  OpenDialogData.Filter := 'astar MAG Values|*.abf';
  OpenDialogData.InitialDir := ProjectDirectory;
  OpenDialogData.FileName := '*.abf';
  if OpenDialogData.execute then
  begin // FileAbfEdit
    Application.ProcessMessages;
    FileAbfEdit.Text := OpenDialogData.FileName;
    ProjectRecord.MAGValuesFile := OpenDialogData.FileName;
    LoadMAGValues;
  end;
end;

procedure TfrmLandscape.abfSaveBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'astar MAG B|*.abf';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := ''; // 'myTerrainData.aof';
  if SaveDialog1.execute then
  Begin
    if uppercase(extractfileext(SaveDialog1.FileName)) = '.ABF' then
    begin
      Application.ProcessMessages;
      ProjectDirectory := ExtractFilePath(SaveDialog1.FileName);
      FileAbfEdit.Text := SaveDialog1.FileName;
      ProjectRecord.MAGValuesFile := SaveDialog1.FileName;
      SaveMAGValues;
    End;
  end;
end;

procedure TfrmLandscape.agfOpenBtnClick(Sender: TObject);
begin
  // AttributeTileArray    FileAdfEdit
  OpenDialogData.Filter := 'astar GCost Values|*.agf';
  OpenDialogData.InitialDir := ProjectDirectory;
  OpenDialogData.FileName := '*.agf';
  if OpenDialogData.execute then
  begin
    Application.ProcessMessages;
    FileAgfEdit.Text := OpenDialogData.FileName;
    ProjectRecord.GCostFile := OpenDialogData.FileName;
    LoadGCostValues;
    // Redisplay it...
    // No it is just Cost.. Map Attributes are Visible
  end;
end;

procedure TfrmLandscape.agfSaveBtnClick(Sender: TObject);
begin
  // AttributeTileArray
  // mapWidth:= ImageTA32.Width div TAtileSize;
  // mapHeight:= ImageTA32.Height div TAtileSize;
  SaveDialog1.Filter := 'astar GCost Values|*.agf';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := ''; // 'myTerrainData.aof';
  if SaveDialog1.execute then
  Begin
    if uppercase(extractfileext(SaveDialog1.FileName)) = '.AGF' then
    begin
      Application.ProcessMessages;
      ProjectDirectory := ExtractFilePath(SaveDialog1.FileName);
      FileAgfEdit.Text := SaveDialog1.FileName;
      ProjectRecord.GCostFile := SaveDialog1.FileName;
      SaveGCostValues(ImageTA32.Width div TAtileSize,
        ImageTA32.Height div TAtileSize);
    End;
  end;
end;

procedure TfrmLandscape.amfOpenBtnClick(Sender: TObject);
var
  f: TextFile;
  // i,
  X, xx, Y, yy, z: Integer;
  s: string;
begin
  OpenDialogData.Filter := 'astar Map Attributes .amf|*.amf';
  OpenDialogData.InitialDir := ProjectDirectory;
  OpenDialogData.FileName := '*.amf';
  if OpenDialogData.execute then
  begin // FileAbfEdit
    Application.ProcessMessages;
    FileAmfEdit.Text := OpenDialogData.FileName;
    AssignFile(f, OpenDialogData.FileName);
    Reset(f);
    Readln(f, s);
    xx := Strtoint(s);
    Readln(f, s);
    yy := Strtoint(s);
    // showmessage(inttostr(xx) + ' : '+ inttostr(yy));
    for Y := 0 to yy - 1 do
      for X := 0 to xx - 1 do
      begin
        Readln(f, s);
        z := Strtoint(s);
        MapAttribute[X, Y] := z;
        // //Redisplay it
        { For i:= 0 to Length(MAGColorValueArray)-1 do
          If z= MAGColorValueArray[i][2] then
          with TBitmapLayer(ImageTA32.Layers[0]) do
          bitmap.FillRectS(
          ((x div TAtileSize)*TAtileSize),((y div TAtileSize)*TAtileSize),
          (((x div TAtileSize)*TAtileSize)+TAtileSize),(((y div TAtileSize)*TAtileSize)+TAtileSize),
          Color32(MAGColorValueArray[i][1])); }
      end;
    CloseFile(f);
    TARePaintBtn.Click;
    // with TBitmapLayer(ImageTA32.Layers[0{I}]) do bitmap.changed;
  end;
end;

procedure TfrmLandscape.amfSaveBtnClick(Sender: TObject);
var
  f: TextFile;
  X, xx, Y, yy, z: Integer;
  s: string;
begin
  SaveDialog1.Filter := 'astar Map Attributes .amf|*.amf';
  SaveDialog1.InitialDir := ProjectDirectory;
  SaveDialog1.FileName := '';
  if SaveDialog1.execute then // Image32_Misc
  begin
    AssignFile(f, SaveDialog1.FileName);
    Rewrite(f);
    // MapAttribute[x div TAtileSize,y div TAtileSize]:= CurrentAttribute;
    xx := ImageTA32.Width div TAtileSize;
    yy := ImageTA32.Height div TAtileSize;
    s := Inttostr(xx);
    writeln(f, s);
    s := Inttostr(yy);
    writeln(f, s);
    // showmessage(inttostr(xx) + ' : '+ inttostr(yy));
    for Y := 0 to yy - 1 do
      for X := 0 to xx - 1 do
      begin
        z := MapAttribute[X, Y];
        s := Inttostr(z);
        writeln(f, s);
      end;
    CloseFile(f);
  end;
end;

procedure TfrmLandscape.TAConvertBtnClick(Sender: TObject);
var
  i, X, xx, Y, yy, z: Integer;
  Confused: Boolean;
begin
  for Y := 0 to ImageTA32.Height - 1 do
    for X := 0 to ImageTA32.Width - 1 do
    begin
      xx := X div TAtileSize;
      yy := Y div TAtileSize;
      Confused := true;
      with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
        z := WinColor(Bitmap[X, Y]);
      For i := 0 to Length(MAGColorValueArray) - 1 do
        If z = MAGColorValueArray[i][1] then
        begin
          GCostData[xx, yy] := MAGColorValueArray[i][2];
          MapAttribute[xx, yy] := i; // MAGColorValueArray[i][1];
          Confused := False;
        end;
      If Confused then
        showmessage(Inttostr(z));
    end;
end;

procedure TfrmLandscape.TARePaintBtnClick(Sender: TObject);
var
  X, xx, Y, yy, z: Integer;
begin
  xx := ImageTA32.Width div TAtileSize;
  yy := ImageTA32.Height div TAtileSize;
  for Y := 0 to yy - 1 do
    for X := 0 to xx - 1 do
    begin
      z := MapAttribute[X, Y];
      with TBitmapLayer(ImageTA32.Layers[0 { I } ]) do
        Bitmap.FillRectS(((X) * TAtileSize), ((Y) * TAtileSize),
          (((X) * TAtileSize) + TAtileSize), (((Y) * TAtileSize) + TAtileSize),
          Color32(MAGColorValueArray[z][1]));
    end;
end;

// Cloned to make a Terrain Attributes BASE map
// Using Attributes Colors
procedure TfrmLandscape.AttributeNewBaseImageBtnClick(Sender: TObject);
var
  ColorModifierHandler: TColorModifierHandler;
  CMSand, CMGrass, CMForest, CMRock, CMSnow: TColorModifierHeight;
  CMSlope: TColorModifierSlope;
  // Bitmap_Output  : TBitmap32;
  // x,y : integer;
  // hx,hy, Height, Slope : single;
  In1, In2, In3, In4: single;
begin
  SetStatus('Rendering...');
  Application.ProcessMessages;
  HeightMap.Rescale(0, 1);

  ColorModifierHandler := TColorModifierHandler.Create;
  ColorModifierHandler.HeightMap := HeightMap;

  // PageControl1.ActivePageIndex := 1;

  CMSand := TColorModifierHeight.Create('');
  CMSand.SingleColor := Color32(MAGColorValueArray[11][1]);
  // SandColor;//clYellow32;
  In1 := StrtoFloatDef(SandEdit1.Text);
  In2 := StrtoFloatDef(SandEdit2.Text);
  In3 := StrtoFloatDef(SandEdit3.Text);
  In4 := StrtoFloatDef(SandEdit4.Text);
  CMSand.SetupCurve(In1, In2, In3, In4); // -2,-1,0.2,0.3);
  ColorModifierHandler.Add(CMSand);
  // CMSand.SingleColor := clYellow32;  CMSand.SetupCurve(-2,-1,0.2,0.3);
  // (StartHeight, FullOnHeight, StopHeight, FullOffHeight: single);
  CMGrass := TColorModifierHeight.Create('');
  CMGrass.SingleColor := Color32(MAGColorValueArray[13][1]);
  // GrassColor;//clGreen32;
  In1 := StrtoFloatDef(GrassEdit1.Text);
  In2 := StrtoFloatDef(GrassEdit2.Text);
  In3 := StrtoFloatDef(GrassEdit3.Text);
  In4 := StrtoFloatDef(GrassEdit4.Text);
  CMGrass.SetupCurve(In1, In2, In3, In4); // 0.2,0.3,0.5,0.6);
  ColorModifierHandler.Add(CMGrass);
  // CMGrass.SingleColor := clGreen32;  CMGrass.SetupCurve(0.2,0.3,0.5,0.6);

  CMForest := TColorModifierHeight.Create('');
  CMForest.SingleColor := Color32(MAGColorValueArray[16][1]);
  // ForestColor;//clRed32;
  In1 := StrtoFloatDef(ForestEdit1.Text);
  In2 := StrtoFloatDef(ForestEdit2.Text);
  In3 := StrtoFloatDef(ForestEdit3.Text);
  In4 := StrtoFloatDef(ForestEdit4.Text);
  CMForest.SetupCurve(In1, In2, In3, In4); // 0.5,0.6,0.9,0.95);
  ColorModifierHandler.Add(CMForest);

  CMRock := TColorModifierHeight.Create('');
  CMRock.SingleColor := Color32(MAGColorValueArray[12][1]);
  // RockColor;//clRed32;
  In1 := StrtoFloatDef(RockEdit1.Text);
  In2 := StrtoFloatDef(RockEdit2.Text);
  In3 := StrtoFloatDef(RockEdit3.Text);
  In4 := StrtoFloatDef(RockEdit4.Text);
  CMRock.SetupCurve(In1, In2, In3, In4); // 0.5,0.6,0.9,0.95);
  ColorModifierHandler.Add(CMRock);
  // CMRock.SingleColor := clRed32;  CMRock.SetupCurve(0.5,0.6,0.9,0.95);

  CMSnow := TColorModifierHeight.Create('');
  CMSnow.SingleColor := Color32(MAGColorValueArray[22][1]);
  // SnowColor;//clWhite32;
  In1 := StrtoFloatDef(SnowEdit1.Text);
  In2 := StrtoFloatDef(SnowEdit2.Text);
  In3 := StrtoFloatDef(SnowEdit3.Text);
  In4 := StrtoFloatDef(SnowEdit4.Text);
  CMSnow.SetupCurve(In1, In2, In3, In4); // 0.9,0.95,2,2);
  ColorModifierHandler.Add(CMSnow);
  // CMSnow.SingleColor := clWhite32;  CMSnow.SetupCurve(0.9,0.95,2,2);

  CMSlope := TColorModifierSlope.Create('');
  ColorModifierHandler.Add(CMSlope); //

  // Bitmap_Output := ImageTA32.Bitmap;
  with TBitmapLayer(ImageTA32.Layers[0]) do
    Bitmap.SetSize(ImageTA32.Width, ImageTA32.Height);

  // ColorModifierHandler.NoTexture := true;
  ColorModifierHandler.Ambient := StrToFloat(Edit_Ambient.Text);
  ColorModifierHandler.Shaded := False; // CheckBox_Shaded.Checked;

  SetStatus('Rendering...');
  ColorModifierHandler.RenderToBitmap(ImageTA32.Bitmap);
  // Attempt to make a Simple Colors only layer
  // with TBitmapLayer(ImageTA32.Layers[0]) do
  // ColorModifierHandler.RenderBaseToBitmap(bitmap);
  SetStatus('Done');

  ColorModifierHandler.Free;
end;

end.
