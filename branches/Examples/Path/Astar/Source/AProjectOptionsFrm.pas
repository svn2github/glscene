unit AProjectOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  GLUtils,//Strtofloatdef
  Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  TAProjectOptionsForm = class(TForm)
    AMainPageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    ExitBtn: TBitBtn;
    TabSheet4: TTabSheet;
    AUnitsPageControl: TPageControl;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    Unit1M5YEdit: TEdit;
    Unit1M4YEdit: TEdit;
    Unit1M3YEdit: TEdit;
    Unit1M2YEdit: TEdit;
    Unit1M1YEdit: TEdit;
    Unit1M5XEdit: TEdit;
    Unit1M4XEdit: TEdit;
    Unit1M3XEdit: TEdit;
    Unit1M2XEdit: TEdit;
    Unit1M1XEdit: TEdit;
    Label8: TLabel;
    Unit1MEdit: TEdit;
    Label9: TLabel;
    Unit1TargetXEdit: TEdit;
    Unit1TargetYEdit: TEdit;
    Unit1StartYEdit: TEdit;
    Unit1StartXEdit: TEdit;
    Label7: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    Unit2MEdit: TEdit;
    Label11: TLabel;
    Unit2TargetXEdit: TEdit;
    Unit2TargetYEdit: TEdit;
    Unit2StartYEdit: TEdit;
    Unit2StartXEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Unit3MEdit: TEdit;
    Label15: TLabel;
    Unit3TargetXEdit: TEdit;
    Unit3TargetYEdit: TEdit;
    Unit3StartYEdit: TEdit;
    Unit3StartXEdit: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Unit4M5YEdit: TEdit;
    Unit4M4YEdit: TEdit;
    Unit4M3YEdit: TEdit;
    Unit4M2YEdit: TEdit;
    Unit4M1YEdit: TEdit;
    Unit4M5XEdit: TEdit;
    Unit4M4XEdit: TEdit;
    Unit4M3XEdit: TEdit;
    Unit4M2XEdit: TEdit;
    Unit4M1XEdit: TEdit;
    Label18: TLabel;
    Unit4MEdit: TEdit;
    Label19: TLabel;
    Unit4TargetXEdit: TEdit;
    Unit4TargetYEdit: TEdit;
    Unit4StartYEdit: TEdit;
    Unit4StartXEdit: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Unit5M5YEdit: TEdit;
    Unit5M4YEdit: TEdit;
    Unit5M3YEdit: TEdit;
    Unit5M2YEdit: TEdit;
    Unit5M1YEdit: TEdit;
    Unit5M5XEdit: TEdit;
    Unit5M4XEdit: TEdit;
    Unit5M3XEdit: TEdit;
    Unit5M2XEdit: TEdit;
    Unit5M1XEdit: TEdit;
    Label22: TLabel;
    Unit5MEdit: TEdit;
    Label23: TLabel;
    Unit5TargetXEdit: TEdit;
    Unit5TargetYEdit: TEdit;
    Unit5StartYEdit: TEdit;
    Unit5StartXEdit: TEdit;
    Label24: TLabel;
    Label25: TLabel;
    FileAdfEdit: TEdit;
    OpenadfBtn: TSpeedButton;
    Label1: TLabel;
    Unit1SpeedEdit: TEdit;
    Unit2SpeedEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Unit2ArmorEdit: TEdit;
    Unit3SpeedEdit: TEdit;
    Label5: TLabel;
    Label35: TLabel;
    Unit3ArmorEdit: TEdit;
    Unit4SpeedEdit: TEdit;
    Label36: TLabel;
    Label37: TLabel;
    Unit4ArmorEdit: TEdit;
    Unit5SpeedEdit: TEdit;
    Label38: TLabel;
    Label39: TLabel;
    Unit5ArmorEdit: TEdit;
    TabSheet24: TTabSheet;
    Unit6M5YEdit: TEdit;
    Unit6M5XEdit: TEdit;
    Unit6M4XEdit: TEdit;
    Unit6M4YEdit: TEdit;
    Unit6M3YEdit: TEdit;
    Unit6M3XEdit: TEdit;
    Unit6M2XEdit: TEdit;
    Unit6M2YEdit: TEdit;
    Unit6M1YEdit: TEdit;
    Unit6M1XEdit: TEdit;
    Label27: TLabel;
    Label29: TLabel;
    Unit6TargetXEdit: TEdit;
    Unit6TargetYEdit: TEdit;
    Unit6StartYEdit: TEdit;
    Unit6StartXEdit: TEdit;
    Label28: TLabel;
    Label26: TLabel;
    ATerrainPageControl: TPageControl;
    TabSheet8: TTabSheet;
    OpenaufBtn: TSpeedButton;
    FileAufEdit: TEdit;
    OpenapfBtn: TSpeedButton;
    FileApfEdit: TEdit;
    SaveapfBtn: TSpeedButton;
    SaveaufBtn: TSpeedButton;
    SaveadfBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Unit1ManEdit: TEdit;
    Label2: TLabel;
    Unit1ArmorEdit: TEdit;
    Label52: TLabel;
    Unit2ManEdit: TEdit;
    Label53: TLabel;
    Unit3ManEdit: TEdit;
    Label54: TLabel;
    Unit4ManEdit: TEdit;
    Label55: TLabel;
    Unit5ManEdit: TEdit;
    Label56: TLabel;
    Unit1OperatingOrdersRG: TRadioGroup;
    Unit2OperatingOrdersRG: TRadioGroup;
    Unit3OperatingOrdersRG: TRadioGroup;
    Unit4OperatingOrdersRG: TRadioGroup;
    Unit5OperatingOrdersRG: TRadioGroup;
    Unit6OperatingOrdersRG: TRadioGroup;
    Unit2M1XEdit: TEdit;
    Unit2M2XEdit: TEdit;
    Unit2M3XEdit: TEdit;
    Unit2M4XEdit: TEdit;
    Unit2M5XEdit: TEdit;
    Unit2M5YEdit: TEdit;
    Unit2M4YEdit: TEdit;
    Unit2M3YEdit: TEdit;
    Unit2M2YEdit: TEdit;
    Unit2M1YEdit: TEdit;
    Unit6SpeedEdit: TEdit;
    Label61: TLabel;
    Label62: TLabel;
    Unit6ManEdit: TEdit;
    Unit6ArmorEdit: TEdit;
    Label63: TLabel;
    Openaff1Btn: TSpeedButton;
    FileAff1Edit: TEdit;
    Unit3M1XEdit: TEdit;
    Unit3M1YEdit: TEdit;
    Unit3M2XEdit: TEdit;
    Unit3M2YEdit: TEdit;
    Unit3M3XEdit: TEdit;
    Unit3M3YEdit: TEdit;
    Unit3M4XEdit: TEdit;
    Unit3M4YEdit: TEdit;
    Unit3M5XEdit: TEdit;
    Unit3M5YEdit: TEdit;
    FileAff2Edit: TEdit;
    FileAff3Edit: TEdit;
    FileAff4Edit: TEdit;
    FileAff5Edit: TEdit;
    FileAff6Edit: TEdit;
    Label67: TLabel;
    TabSheet7: TTabSheet;
    TabSheet14: TTabSheet;
    OpenImageTextureBtn: TSpeedButton;
    FileImageTextureEdit: TEdit;
    Label68: TLabel;
    OpenanfBtn: TSpeedButton;
    FileAnfEdit: TEdit;
    SaveanfBtn: TSpeedButton;
    Label69: TLabel;
    Openaff2Btn: TSpeedButton;
    Openaff3Btn: TSpeedButton;
    Openaff4Btn: TSpeedButton;
    Openaff5Btn: TSpeedButton;
    Openaff6Btn: TSpeedButton;
    GridLinesDisplayedCB: TCheckBox;
    PathDisplayedCB: TCheckBox;
    ClaimedNodesDisplayedCB: TCheckBox;
    TerrainValueAlphaEdit: TEdit;
    ProcessNoGoIslandsCB: TCheckBox;
    TurningpenaltyEdit: TEdit;
    Label72: TLabel;
    TabSheet16: TTabSheet;
    UnitsAITaskListRG: TRadioGroup;
    AverageTerrainEdit: TEdit;
    Label74: TLabel;
    Unit1SearchRG: TRadioGroup;
    Unit1TieBreakerRG: TRadioGroup;
    Unit2SearchRG: TRadioGroup;
    Unit2TieBreakerRG: TRadioGroup;
    Unit3SearchRG: TRadioGroup;
    Unit3TieBreakerRG: TRadioGroup;
    Unit4SearchRG: TRadioGroup;
    Unit4TieBreakerRG: TRadioGroup;
    Unit5SearchRG: TRadioGroup;
    Unit5TieBreakerRG: TRadioGroup;
    Unit6SearchRG: TRadioGroup;
    Unit6TieBreakerRG: TRadioGroup;
    Label76: TLabel;
    Unit6MEdit: TEdit;
    TextureSizeRG: TRadioGroup;
    TileSizeRG: TRadioGroup;
    Panel1: TPanel;
    Label47: TLabel;
    Label46: TLabel;
    Label45: TLabel;
    Label44: TLabel;
    Label43: TLabel;
    Label42: TLabel;
    Label41: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Unit6CB: TCheckBox;
    NumberofActiveUnitsEdit: TEdit;
    Label81: TLabel;
    Label82: TLabel;
    OpenagfBtn: TSpeedButton;
    FileAgfEdit: TEdit;
    SaveagfBtn: TSpeedButton;
    TabSheet15: TTabSheet;
    Label83: TLabel;
    OpenamfBtn: TSpeedButton;
    FileAmfEdit: TEdit;
    SaveamfBtn: TSpeedButton;
    TabSheet17: TTabSheet;
    OpenHeightDataBtn: TSpeedButton;
    OpenHeightDataEdit: TEdit;
    Label71: TLabel;
    Label73: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Openamd1Btn: TSpeedButton;
    FileAmd1Edit: TEdit;
    FileAjp1Edit: TEdit;
    Openajp1Btn: TSpeedButton;
    FileAjp2Edit: TEdit;
    Openajp2Btn: TSpeedButton;
    FileAmd2Edit: TEdit;
    Openamd2Btn: TSpeedButton;
    FileAjp3Edit: TEdit;
    Openajp3Btn: TSpeedButton;
    FileAmd3Edit: TEdit;
    Openamd3Btn: TSpeedButton;
    FileAjp4Edit: TEdit;
    Openajp4Btn: TSpeedButton;
    FileAmd4Edit: TEdit;
    Openamd4Btn: TSpeedButton;
    FileAjp5Edit: TEdit;
    Openajp5Btn: TSpeedButton;
    FileAmd5Edit: TEdit;
    Openamd5Btn: TSpeedButton;
    Label30: TLabel;
    FileAjp6Edit: TEdit;
    Openajp6Btn: TSpeedButton;
    FileAmd6Edit: TEdit;
    Openamd6Btn: TSpeedButton;
    TabSheet25: TTabSheet;
    Label70: TLabel;
    OpenaifBtn: TSpeedButton;
    FileAifEdit: TEdit;
    SaveaifBtn: TSpeedButton;
    PageControl1: TPageControl;
    TabSheet18: TTabSheet;
    Label50: TLabel;
    OpenaefBtn: TSpeedButton;
    SaveaefBtn: TSpeedButton;
    Label51: TLabel;
    FileAefEdit: TEdit;
    TabSheet19: TTabSheet;
    OpenavfBtn: TSpeedButton;
    SaveavfBtn: TSpeedButton;
    Label58: TLabel;
    FileAvfEdit: TEdit;
    TabSheet20: TTabSheet;
    OpenasfBtn: TSpeedButton;
    SaveasfBtn: TSpeedButton;
    Label60: TLabel;
    FileAsfEdit: TEdit;
    TabSheet21: TTabSheet;
    OpenahfBtn: TSpeedButton;
    SaveahfBtn: TSpeedButton;
    Label59: TLabel;
    FileAhfEdit: TEdit;
    TabSheet6: TTabSheet;
    OpenatfBtn: TSpeedButton;
    SaveatfBtn: TSpeedButton;
    Label75: TLabel;
    FileAtfEdit: TEdit;
    TabSheet22: TTabSheet;
    OpenaafBtn: TSpeedButton;
    SaveaafBtn: TSpeedButton;
    Label49: TLabel;
    FileAafEdit: TEdit;
    TabSheet23: TTabSheet;
    OpenaofBtn: TSpeedButton;
    SaveaofBtn: TSpeedButton;
    Label57: TLabel;
    FileAofEdit: TEdit;
    TabSheet5: TTabSheet;
    Label31: TLabel;
    OpenabfBtn: TSpeedButton;
    FileAbfEdit: TEdit;
    SaveabfBtn: TSpeedButton;
    TerrainMagNoGoEdit: TEdit;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label88: TLabel;
    MAGResettoDefaultsBtn: TSpeedButton;
    Unit1ActorScaleEdit: TEdit;
    Label90: TLabel;
    Label91: TLabel;
    Unit2ActorScaleEdit: TEdit;
    Label92: TLabel;
    Unit3ActorScaleEdit: TEdit;
    Label93: TLabel;
    Unit4ActorScaleEdit: TEdit;
    Label94: TLabel;
    Unit5ActorScaleEdit: TEdit;
    Label95: TLabel;
    Unit6ActorScaleEdit: TEdit;
    HeightfieldSizeRG: TRadioGroup;
    Label89: TLabel;
    Label87: TLabel;
    OpenTexture1Btn: TSpeedButton;
    OpenWpn1Btn: TSpeedButton;
    WpnEdit1: TEdit;
    WpnTextureEdit1: TEdit;
    WpnTextureEdit2: TEdit;
    WpnEdit2: TEdit;
    OpenWpn2Btn: TSpeedButton;
    OpenTexture2Btn: TSpeedButton;
    WpnTextureEdit3: TEdit;
    WpnEdit3: TEdit;
    OpenWpn3Btn: TSpeedButton;
    OpenTexture3Btn: TSpeedButton;
    WpnTextureEdit4: TEdit;
    WpnEdit4: TEdit;
    OpenWpn4Btn: TSpeedButton;
    OpenTexture4Btn: TSpeedButton;
    WpnTextureEdit5: TEdit;
    WpnEdit5: TEdit;
    OpenWpn5Btn: TSpeedButton;
    OpenTexture5Btn: TSpeedButton;
    WpnTextureEdit6: TEdit;
    WpnEdit6: TEdit;
    OpenWpn6Btn: TSpeedButton;
    OpenTexture6Btn: TSpeedButton;
    Label40: TLabel;
    TargetSizeEdit1: TEdit;
    Label48: TLabel;
    TargetSizeEdit2: TEdit;
    Label64: TLabel;
    TargetSizeEdit3: TEdit;
    Label65: TLabel;
    TargetSizeEdit4: TEdit;
    Label66: TLabel;
    TargetSizeEdit5: TEdit;
    Label96: TLabel;
    TargetSizeEdit6: TEdit;
    PrintaufUnitsBtn: TSpeedButton;
    PrintabfBtn: TSpeedButton;
    Label97: TLabel;
    Label98: TLabel;
    RatioRG: TRadioGroup;
    TerrainAPPAEdit: TEdit;
    Label99: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OpenapfBtnClick(Sender: TObject);
//procedure LoadProjectFile(Filename:String);
//procedure LoadColorsFile(Filename:String);
//procedure LoadUnitsFile(Filename:String);
//procedure LoadTerrainFile(Filename:String);
procedure DisplayProjectFile;
procedure DisplayUnitsFile;
procedure DisplayTerrainFile;

    procedure SaveapfBtnClick(Sender: TObject);
    procedure OpenaffBtnClick(Sender: TObject);
    procedure OpenaufBtnClick(Sender: TObject);
    procedure SaveaufBtnClick(Sender: TObject);
    procedure OpenadfBtnClick(Sender: TObject);
    procedure SaveadfBtnClick(Sender: TObject);
    procedure OpenaefBtnClick(Sender: TObject);
    procedure SaveaefBtnClick(Sender: TObject);
    procedure SaveasfBtnClick(Sender: TObject);
    procedure SaveahfBtnClick(Sender: TObject);
    procedure SaveavfBtnClick(Sender: TObject);
    procedure SaveatfBtnClick(Sender: TObject);
    procedure SaveaofBtnClick(Sender: TObject);

    procedure SaveaifBtnClick(Sender: TObject);
    procedure SaveanfBtnClick(Sender: TObject);
    procedure Openaff1BtnClick(Sender: TObject);
    procedure OpenasfBtnClick(Sender: TObject);
    procedure OpenahfBtnClick(Sender: TObject);
    procedure OpenavfBtnClick(Sender: TObject);
    procedure OpenatfBtnClick(Sender: TObject);
    procedure OpenaofBtnClick(Sender: TObject);
    procedure OpenaifBtnClick(Sender: TObject);
    procedure OpenImageTextureBtnClick(Sender: TObject);
    procedure OpenanfBtnClick(Sender: TObject);
    procedure OpenaafBtnClick(Sender: TObject);
    procedure SaveaafBtnClick(Sender: TObject);
    procedure OpenagfBtnClick(Sender: TObject);
    procedure SaveagfBtnClick(Sender: TObject);
    procedure OpenamfBtnClick(Sender: TObject);
    procedure SaveamfBtnClick(Sender: TObject);
    procedure OpenHeightDataBtnClick(Sender: TObject);
    procedure Openamd1BtnClick(Sender: TObject);
    procedure Openajp1BtnClick(Sender: TObject);
    procedure OpenabfBtnClick(Sender: TObject);
    procedure SaveabfBtnClick(Sender: TObject);
    procedure MAGResettoDefaultsBtnClick(Sender: TObject);
    procedure OpenTexture6BtnClick(Sender: TObject);
    procedure OpenWpn1BtnClick(Sender: TObject);
    procedure PrintaufUnitsBtnClick(Sender: TObject);
    procedure PrintabfBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AProjectOptionsForm: TAProjectOptionsForm;

implementation

uses AStarGlobals;

{$R *.DFM}

procedure TAProjectOptionsForm.FormCreate(Sender: TObject);
begin
  left := AProjectOptionsFormX;
  top := AProjectOptionsFormY;
end;

procedure TAProjectOptionsForm.FormShow(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.FormActivate(Sender: TObject);
begin      
  //SetOptions;  //Only from a Project file
{  ScreenSizeRG.ItemIndex:=ImageSizeMode;
  TileSizeRG.ItemIndex:=TileSizeMode;
  GridLinesDisplayedCB.Checked:=GridLinesDisplayed;
  PathDisplayedCB.Checked:=PathDisplayed;}
end;

procedure TAProjectOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AProjectOptionsFormX := AProjectOptionsForm.left;
  AProjectOptionsFormY := AProjectOptionsForm.top;
end;


procedure TAProjectOptionsForm.OpenapfBtnClick(Sender: TObject);
begin //FileApfEdit
  OpenDialog.Filter:= 'astar Project|*.apf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.apf';
  if OpenDialog.Execute then
  begin
    Application.ProcessMessages;
    FileApfEdit.Text:= OpenDialog.FileName;
    ProjectFilename:=  OpenDialog.FileName;
    ProjectDirectory:=ExtractFilePath(OpenDialog.FileName);
    LoadProjectFile(OpenDialog.FileName);
    DisplayProjectFile;    
    //SetColors;//Sets Colors
    DisplayUnitsFile;
    DisplayTerrainFile;
  end;
end;


procedure TAProjectOptionsForm.DisplayProjectFile;
Begin
HeightfieldSizeRG.Itemindex:=(ProjectRecord.HeightfieldSizeMode);
RatioRG.Itemindex:=(ProjectRecord.RatioSizeMode);
TextureSizeRG.Itemindex:=(ProjectRecord.ImageSizeMode);
TileSizeRG.Itemindex:=(ProjectRecord.TileSizeMode);
TerrainValueAlphaEdit.Text:=Floattostr(ProjectRecord.TerrainValueAlphaD);
AverageTerrainEdit.Text:=Floattostr(ProjectRecord.AverageTerrainD);
TerrainMagNoGoEdit.Text:=Floattostr(ProjectRecord.TerrainMagNoGoD);
TerrainAPPAEdit.Text:=Floattostr(ProjectRecord.AdjacentPathPenaltyD);
TurningpenaltyEdit.Text:=Floattostr(ProjectRecord.TurningpenaltyD);

GridLinesDisplayedCB.Checked:= ProjectRecord.GridLinesDisplayed;
PathDisplayedCB.Checked:= ProjectRecord.PathDisplayed;
ClaimedNodesDisplayedCB.Checked:= ProjectRecord.ClaimedNodesDisplayed;
ProcessNoGoIslandsCB.checked:= ProjectRecord.ProcessNoGoIslands;

//FileAffEdit.Text:=ProjectRecord.ColorsFile;
FileAufEdit.Text:=ProjectRecord.UnitsFile;
FileAdfEdit.Text  :=ProjectRecord.TerrainFile;
End;

procedure TAProjectOptionsForm.DisplayUnitsFile;
Begin
Unit6CB.Checked:= (ProjectRecord.NumberofActiveEnemyPatrol=1);
NumberofActiveUnitsEdit.Text:=
inttostr(ProjectRecord.NumberofActiveUnits -ProjectRecord.NumberofActiveEnemyPatrol);

Unit1StartXEdit.Text:=inttostr(UnitRecordArray[1].startXLoc);
Unit1StartYEdit.Text:=inttostr(UnitRecordArray[1].startYLoc);
Unit1TargetXEdit.Text:=inttostr(UnitRecordArray[1].targetX);
Unit1TargetYEdit.Text:=inttostr(UnitRecordArray[1].targetY);
Unit1SpeedEdit.Text:=inttostr(UnitRecordArray[1].speed);
Unit1ManEdit.Text:=inttostr(UnitRecordArray[1].Manueverability);
Unit1ArmorEdit.Text:=inttostr(UnitRecordArray[1].armor);
Unit1MEdit.Text:=inttostr(UnitRecordArray[1].Members);
Unit1M1XEdit.Text:=Inttostr(UnitRecordArray[1].Members1X);
Unit1M1YEdit.Text:=Inttostr(UnitRecordArray[1].Members1Y);
Unit1M2XEdit.Text:=Inttostr(UnitRecordArray[1].Members2X);
Unit1M2YEdit.Text:=Inttostr(UnitRecordArray[1].Members2Y);
Unit1M3XEdit.Text:=Inttostr(UnitRecordArray[1].Members3X);
Unit1M3YEdit.Text:=Inttostr(UnitRecordArray[1].Members3Y);
Unit1M4XEdit.Text:=Inttostr(UnitRecordArray[1].Members4X);
Unit1M4YEdit.Text:=Inttostr(UnitRecordArray[1].Members4Y);
Unit1M5XEdit.Text:=Inttostr(UnitRecordArray[1].Members5X);
Unit1M5YEdit.Text:=Inttostr(UnitRecordArray[1].Members5Y);
Unit1OperatingOrdersRG.ItemIndex:=UnitRecordArray[1].OperatingOrders;
Unit1SearchRG.ItemIndex:=UnitRecordArray[1].SearchMode;
Unit1TieBreakerRG.ItemIndex:=UnitRecordArray[1].TieBreakerMode;
FileAff1Edit.Text:=UnitRecordArray[1].FlagFile;
TargetSizeEdit1.Text:=floattostr(UnitRecordArray[1].TargetSize);
FileAmd1Edit.Text:=UnitRecordArray[1].Md2Name;
FileAjp1Edit.Text:=UnitRecordArray[1].Md2TextureName;
Unit1ActorScaleEdit.Text:=floattostr(UnitRecordArray[1].ActorScale);
WpnEdit1.Text:=UnitRecordArray[1].Md2WpnName;
WpnTextureEdit1.Text:=UnitRecordArray[1].Md2WpnTextureName;

Unit2StartXEdit.Text:=inttostr(UnitRecordArray[2].startXLoc);
Unit2StartYEdit.Text:=inttostr(UnitRecordArray[2].startYLoc);
Unit2TargetXEdit.Text:=inttostr(UnitRecordArray[2].targetX);
Unit2TargetYEdit.Text:=inttostr(UnitRecordArray[2].targetY);
Unit2SpeedEdit.Text:=inttostr(UnitRecordArray[2].speed);
Unit2ManEdit.Text:=inttostr(UnitRecordArray[2].Manueverability);
Unit2ArmorEdit.Text:=inttostr(UnitRecordArray[2].armor);
Unit2MEdit.Text:=inttostr(UnitRecordArray[2].Members);
Unit2M1XEdit.Text:=Inttostr(UnitRecordArray[2].Members1X);
Unit2M1YEdit.Text:=Inttostr(UnitRecordArray[2].Members1Y);
Unit2M2XEdit.Text:=Inttostr(UnitRecordArray[2].Members2X);
Unit2M2YEdit.Text:=Inttostr(UnitRecordArray[2].Members2Y);
Unit2M3XEdit.Text:=Inttostr(UnitRecordArray[2].Members3X);
Unit2M3YEdit.Text:=Inttostr(UnitRecordArray[2].Members3Y);
Unit2M4XEdit.Text:=Inttostr(UnitRecordArray[2].Members4X);
Unit2M4YEdit.Text:=Inttostr(UnitRecordArray[2].Members4Y);
Unit2M5XEdit.Text:=Inttostr(UnitRecordArray[2].Members5X);
Unit2M5YEdit.Text:=Inttostr(UnitRecordArray[2].Members5Y);
Unit2OperatingOrdersRG.ItemIndex:=UnitRecordArray[2].OperatingOrders;
Unit2SearchRG.ItemIndex:=UnitRecordArray[2].SearchMode;
Unit2TieBreakerRG.ItemIndex:=UnitRecordArray[2].TieBreakerMode;
FileAff2Edit.Text:=UnitRecordArray[2].FlagFile;
TargetSizeEdit2.Text:=floattostr(UnitRecordArray[2].TargetSize);
FileAmd2Edit.Text:=UnitRecordArray[2].Md2Name;
FileAjp2Edit.Text:=UnitRecordArray[2].Md2TextureName;
Unit2ActorScaleEdit.Text:=floattostr(UnitRecordArray[2].ActorScale);
WpnEdit2.Text:=UnitRecordArray[2].Md2WpnName;
WpnTextureEdit2.Text:=UnitRecordArray[2].Md2WpnTextureName;

Unit3StartXEdit.Text:=inttostr(UnitRecordArray[3].startXLoc);
Unit3StartYEdit.Text:=inttostr(UnitRecordArray[3].startYLoc);
Unit3TargetXEdit.Text:=inttostr(UnitRecordArray[3].targetX);
Unit3TargetYEdit.Text:=inttostr(UnitRecordArray[3].targetY);
Unit3SpeedEdit.Text:=inttostr(UnitRecordArray[3].speed);
Unit3ManEdit.Text:=inttostr(UnitRecordArray[3].Manueverability);
Unit3ArmorEdit.Text:=inttostr(UnitRecordArray[3].armor);
Unit3MEdit.Text:=inttostr(UnitRecordArray[3].Members);
Unit3M1XEdit.Text:=Inttostr(UnitRecordArray[3].Members1X);
Unit3M1YEdit.Text:=Inttostr(UnitRecordArray[3].Members1Y);
Unit3M2XEdit.Text:=Inttostr(UnitRecordArray[3].Members2X);
Unit3M2YEdit.Text:=Inttostr(UnitRecordArray[3].Members2Y);
Unit3M3XEdit.Text:=Inttostr(UnitRecordArray[3].Members3X);
Unit3M3YEdit.Text:=Inttostr(UnitRecordArray[3].Members3Y);
Unit3M4XEdit.Text:=Inttostr(UnitRecordArray[3].Members4X);
Unit3M4YEdit.Text:=Inttostr(UnitRecordArray[3].Members4Y);
Unit3M5XEdit.Text:=Inttostr(UnitRecordArray[3].Members5X);
Unit3M5YEdit.Text:=Inttostr(UnitRecordArray[3].Members5Y);
Unit3OperatingOrdersRG.ItemIndex:=UnitRecordArray[3].OperatingOrders;
Unit3SearchRG.ItemIndex:=UnitRecordArray[3].SearchMode;
Unit3TieBreakerRG.ItemIndex:=UnitRecordArray[3].TieBreakerMode;
FileAff3Edit.Text:=UnitRecordArray[3].FlagFile;
TargetSizeEdit3.Text:=floattostr(UnitRecordArray[3].TargetSize);
FileAmd3Edit.Text:=UnitRecordArray[3].Md2Name;
FileAjp3Edit.Text:=UnitRecordArray[3].Md2TextureName;
Unit3ActorScaleEdit.Text:=floattostr(UnitRecordArray[3].ActorScale);
WpnEdit3.Text:=UnitRecordArray[3].Md2WpnName;
WpnTextureEdit3.Text:=UnitRecordArray[3].Md2WpnTextureName;

Unit4StartXEdit.Text:=inttostr(UnitRecordArray[4].startXLoc);
Unit4StartYEdit.Text:=inttostr(UnitRecordArray[4].startYLoc);
Unit4TargetXEdit.Text:=inttostr(UnitRecordArray[4].targetX);
Unit4TargetYEdit.Text:=inttostr(UnitRecordArray[4].targetY);
Unit4SpeedEdit.Text:=inttostr(UnitRecordArray[4].speed);
Unit4ManEdit.Text:=inttostr(UnitRecordArray[4].Manueverability);
Unit4ArmorEdit.Text:=inttostr(UnitRecordArray[4].armor);
Unit4MEdit.Text:=inttostr(UnitRecordArray[4].Members);
Unit4M1XEdit.Text:=Inttostr(UnitRecordArray[4].Members1X);
Unit4M1YEdit.Text:=Inttostr(UnitRecordArray[4].Members1Y);
Unit4M2XEdit.Text:=Inttostr(UnitRecordArray[4].Members2X);
Unit4M2YEdit.Text:=Inttostr(UnitRecordArray[4].Members2Y);
Unit4M3XEdit.Text:=Inttostr(UnitRecordArray[4].Members3X);
Unit4M3YEdit.Text:=Inttostr(UnitRecordArray[4].Members3Y);
Unit4M4XEdit.Text:=Inttostr(UnitRecordArray[4].Members4X);
Unit4M4YEdit.Text:=Inttostr(UnitRecordArray[4].Members4Y);
Unit4M5XEdit.Text:=Inttostr(UnitRecordArray[4].Members5X);
Unit4M5YEdit.Text:=Inttostr(UnitRecordArray[4].Members5Y);
Unit4OperatingOrdersRG.ItemIndex:=UnitRecordArray[4].OperatingOrders;
Unit4SearchRG.ItemIndex:=UnitRecordArray[4].SearchMode;
Unit4TieBreakerRG.ItemIndex:=UnitRecordArray[4].TieBreakerMode;
FileAff4Edit.Text:=UnitRecordArray[4].FlagFile;
TargetSizeEdit4.Text:=floattostr(UnitRecordArray[4].TargetSize);
FileAmd4Edit.Text:=UnitRecordArray[4].Md2Name;
FileAjp4Edit.Text:=UnitRecordArray[4].Md2TextureName;
Unit4ActorScaleEdit.Text:=floattostr(UnitRecordArray[4].ActorScale);
WpnEdit4.Text:=UnitRecordArray[4].Md2WpnName;
WpnTextureEdit4.Text:=UnitRecordArray[4].Md2WpnTextureName;

Unit5StartXEdit.Text:=inttostr(UnitRecordArray[5].startXLoc);
Unit5StartYEdit.Text:=inttostr(UnitRecordArray[5].startYLoc);
Unit5TargetXEdit.Text:=inttostr(UnitRecordArray[5].targetX);
Unit5TargetYEdit.Text:=inttostr(UnitRecordArray[5].targetY);
Unit5SpeedEdit.Text:=inttostr(UnitRecordArray[5].speed);
Unit5ManEdit.Text:=inttostr(UnitRecordArray[5].Manueverability);
Unit5ArmorEdit.Text:=inttostr(UnitRecordArray[5].armor);
Unit5MEdit.Text:=inttostr(UnitRecordArray[5].Members);
Unit5M1XEdit.Text:=Inttostr(UnitRecordArray[5].Members1X);
Unit5M1YEdit.Text:=Inttostr(UnitRecordArray[5].Members1Y);
Unit5M2XEdit.Text:=Inttostr(UnitRecordArray[5].Members2X);
Unit5M2YEdit.Text:=Inttostr(UnitRecordArray[5].Members2Y);
Unit5M3XEdit.Text:=Inttostr(UnitRecordArray[5].Members3X);
Unit5M3YEdit.Text:=Inttostr(UnitRecordArray[5].Members3Y);
Unit5M4XEdit.Text:=Inttostr(UnitRecordArray[5].Members4X);
Unit5M4YEdit.Text:=Inttostr(UnitRecordArray[5].Members4Y);
Unit5M5XEdit.Text:=Inttostr(UnitRecordArray[5].Members5X);
Unit5M5YEdit.Text:=Inttostr(UnitRecordArray[5].Members5Y);
Unit5OperatingOrdersRG.ItemIndex:=UnitRecordArray[5].OperatingOrders;
Unit5SearchRG.ItemIndex:=UnitRecordArray[5].SearchMode;
Unit5TieBreakerRG.ItemIndex:=UnitRecordArray[5].TieBreakerMode;
FileAff5Edit.Text:=UnitRecordArray[5].FlagFile;
TargetSizeEdit5.Text:=floattostr(UnitRecordArray[5].TargetSize);
FileAmd5Edit.Text:=UnitRecordArray[5].Md2Name;
FileAjp5Edit.Text:=UnitRecordArray[5].Md2TextureName;
Unit5ActorScaleEdit.Text:=floattostr(UnitRecordArray[5].ActorScale);
WpnEdit5.Text:=UnitRecordArray[5].Md2WpnName;
WpnTextureEdit5.Text:=UnitRecordArray[5].Md2WpnTextureName;

Unit6StartXEdit.Text:=inttostr(UnitRecordArray[6].startXLoc);
Unit6StartYEdit.Text:=inttostr(UnitRecordArray[6].startYLoc);
Unit6TargetXEdit.Text:=inttostr(UnitRecordArray[6].targetX);
Unit6TargetYEdit.Text:=inttostr(UnitRecordArray[6].targetY);
Unit6SpeedEdit.Text:=inttostr(UnitRecordArray[6].speed);
Unit6ManEdit.Text:=inttostr(UnitRecordArray[6].Manueverability);
Unit6ArmorEdit.Text:=inttostr(UnitRecordArray[6].armor);
Unit6MEdit.Text:=inttostr(UnitRecordArray[6].Members);
Unit6M1XEdit.Text:=Inttostr(UnitRecordArray[6].Members1X);
Unit6M1YEdit.Text:=Inttostr(UnitRecordArray[6].Members1Y);
Unit6M2XEdit.Text:=Inttostr(UnitRecordArray[6].Members2X);
Unit6M2YEdit.Text:=Inttostr(UnitRecordArray[6].Members2Y);
Unit6M3XEdit.Text:=Inttostr(UnitRecordArray[6].Members3X);
Unit6M3YEdit.Text:=Inttostr(UnitRecordArray[6].Members3Y);
Unit6M4XEdit.Text:=Inttostr(UnitRecordArray[6].Members4X);
Unit6M4YEdit.Text:=Inttostr(UnitRecordArray[6].Members4Y);
Unit6M5XEdit.Text:=Inttostr(UnitRecordArray[6].Members5X);
Unit6M5YEdit.Text:=Inttostr(UnitRecordArray[6].Members5Y);
Unit6OperatingOrdersRG.ItemIndex:=UnitRecordArray[6].OperatingOrders;
Unit6SearchRG.ItemIndex:=UnitRecordArray[6].SearchMode;
Unit6TieBreakerRG.ItemIndex:=UnitRecordArray[6].TieBreakerMode;
FileAff6Edit.Text:=UnitRecordArray[6].FlagFile;
TargetSizeEdit6.Text:=floattostr(UnitRecordArray[6].TargetSize);
FileAmd6Edit.Text:=UnitRecordArray[6].Md2Name;
FileAjp6Edit.Text:=UnitRecordArray[6].Md2TextureName;
Unit6ActorScaleEdit.Text:=floattostr(UnitRecordArray[6].ActorScale);
WpnEdit6.Text:=UnitRecordArray[6].Md2WpnName;
WpnTextureEdit6.Text:=UnitRecordArray[6].Md2WpnTextureName;

End;

procedure TAProjectOptionsForm.DisplayTerrainFile;
Begin
 OpenHeightDataEdit.Text:=ProjectRecord.HeightDataFile;
FileImageTextureEdit.Text:=ProjectRecord.ImageTextureFile;
FileAmfEdit.Text:=ProjectRecord.MapAttributesFile;
FileAefEdit.Text:=ProjectRecord.ElevationFile;
FileAvfEdit.Text:=ProjectRecord.VegetationFile;
FileAsfEdit.Text:=ProjectRecord.SoilsFile;
FileAhfEdit.Text:=ProjectRecord.HydrologyFile;
FileAtfEdit.Text:=ProjectRecord.TransportFile;
FileAafEdit.Text:=ProjectRecord.UrbanFile;
FileAnfEdit.Text:=ProjectRecord.NOGOFile;
FileAofEdit.Text:=ProjectRecord.ObstaclesFile;

FileAifEdit.Text:= ProjectRecord.InfluenceMapFile;
FileAgfEdit.Text:=ProjectRecord.GCostFile;
FileAbfEdit.Text:=ProjectRecord.MAGValuesFile;
End;



procedure TAProjectOptionsForm.SaveapfBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
Const NoName='NoName';
Begin
  SaveDialog1.Filter:= 'astar Project|*.apf';
  SaveDialog1.InitialDir:=ProjectDirectory;
  SaveDialog1.FileName:='';//'myTerrainData.aof';
  if savedialog1.execute then
  Begin
    if uppercase(extractfileext(savedialog1.filename))='.APF' then
    begin
      Application.ProcessMessages;
      ProjectDirectory:=ExtractFilePath(savedialog1.FileName);
      AssignFile(F, savedialog1.filename);
      Rewrite(F);
      Writeln(F,'AStar Project File');
      S:=Inttostr(1); Writeln(F,S);
  // Load Base data  THEN THE SECTION FILENAMES:
  //Display Color,Units,Terrain File
  //(Each SECTION Loads their OWN stuff)
      //Read the Base Information
      S:=Inttostr(HeightfieldSizeRG.Itemindex); Writeln(F,S);
      S:=Inttostr(RatioRG.Itemindex); Writeln(F,S);
      S:=Inttostr(TextureSizeRG.Itemindex); Writeln(F,S);
      S:=Inttostr(TileSizeRG.Itemindex); Writeln(F,S);

      S:=TerrainValueAlphaEdit.Text; Writeln(F,S);
      S:=AverageTerrainEdit.Text; Writeln(F,S);
      S:=TerrainMagNoGoEdit.Text; Writeln(F,S);
      S:=TerrainAPPAEdit.Text; Writeln(F,S);
      S:=TurningpenaltyEdit.Text; Writeln(F,S);
      If GridLinesDisplayedCB.Checked then S:='True' else S:='False';
       Writeln(F,S);
      If PathDisplayedCB.Checked then S:='True' else S:='False';
       Writeln(F,S);
      If ClaimedNodesDisplayedCB.Checked then S:='True' else S:='False';
       Writeln(F,S);
      If ProcessNoGoIslandsCB.checked then S:='True' else S:='False';
       Writeln(F,S);


      //Read the Section File Names
       {S:=FileAffEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S); }
       S:=FileAufEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAdfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      //Future Proofing
      //If Version > 1 then  begin       end;
    CloseFile(F);
    End;
  end;
end;

procedure TAProjectOptionsForm.OpenaffBtnClick(Sender: TObject);
begin//FileAcfEdit

end;

procedure TAProjectOptionsForm.OpenaufBtnClick(Sender: TObject);
begin  //FileAufEdit
  OpenDialog.Filter:= 'astar Units|*.auf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.auf';
  if OpenDialog.Execute then
  begin
  Application.ProcessMessages;
  FileAufEdit.Text:= OpenDialog.FileName;
  LoadUnitsFile(OpenDialog.FileName);
  DisplayUnitsFile;
  end;
end;
procedure TAProjectOptionsForm.PrintaufUnitsBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string; //  PathS: string;
Const NoName='NoName';
begin
  //PathS:=ProjectDirectory+'UnitPrint.txt';
  showmessage(ProjectDirectory+'UnitPrint.txt');
  AssignFile(F, ProjectDirectory+'UnitPrint.txt');
  Rewrite(F);
  Writeln(F,'AStar Units File');
  S:=NumberofActiveUnitsEdit.Text;//Inttostr(NumberofUnits);
  Writeln(F,'NumberofUnits: '+S);

  Writeln(F,'Unit1');//Unit1
      S:=Unit1StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit1StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit1TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit1TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit1MEdit.Text; Writeln(F,'Members '+S);      
      S:=FileAff1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit1.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit1ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit1.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit1.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

  Writeln(F,'Unit2');//Unit1
      S:=Unit2StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit2StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit2TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit2TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit2MEdit.Text; Writeln(F,'Members '+S);
      S:=FileAff2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit2.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit2ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit2.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit2.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

  Writeln(F,'Unit3');//Unit1
      S:=Unit3StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit3StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit3TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit3TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit3MEdit.Text; Writeln(F,'Members '+S);
      S:=FileAff3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit3.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit3ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit3.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit3.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

  Writeln(F,'Unit4');//Unit1
      S:=Unit4StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit4StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit4TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit4TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit4MEdit.Text; Writeln(F,'Members '+S);
      S:=FileAff4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit4.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit4ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit4.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit4.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

  Writeln(F,'Unit5');//Unit1
      S:=Unit5StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit5StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit5TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit5TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit5MEdit.Text; Writeln(F,'Members '+S);
      S:=FileAff5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit5.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit5ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit5.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit5.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

  If Unit6CB.Checked then S:='True' else S:='False';
  Writeln(F,'Enemy Active: '+S);
  Writeln(F,'Unit6 : Enemy');//Unit1
      S:=Unit6StartXEdit.Text; Writeln(F,'StartX '+S);
      S:=Unit6StartYEdit.Text; Writeln(F,'StartY '+S);
      S:=Unit6TargetXEdit.Text; Writeln(F,'TargetX '+S);
      S:=Unit6TargetYEdit.Text; Writeln(F,'TargetY '+S);
      S:=Unit6MEdit.Text; Writeln(F,'Members '+S);
      S:=FileAff6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Target Texture '+S);
      S:=TargetSizeEdit6.Text; Writeln(F,'TargetSize '+S);
      S:=FileAmd6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor.md2 '+S);
      S:=FileAjp6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Actor Texture '+S);
      S:=Unit6ActorScaleEdit.Text; Writeln(F,'ActorScale '+S);
      S:=WpnEdit6.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'Wpn '+S);
      S:=WpnTextureEdit6.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,'WpnTexture '+S);

      CloseFile(F);
      Application.ProcessMessages;
  if FileExists(ProjectDirectory+'UnitPrint.txt') then
    ExecuteFile('UnitPrint.txt', '', ProjectDirectory, SW_SHOW)
end;
procedure TAProjectOptionsForm.SaveaufBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
Const NoName='NoName';
Begin
  SaveDialog1.Filter:= 'astar Units|*.auf';
  SaveDialog1.InitialDir:=ProjectDirectory;
  SaveDialog1.FileName:='';//'myTerrainData.aof';
  if savedialog1.execute then
  Begin
    if uppercase(extractfileext(savedialog1.filename))='.AUF' then
    begin
      Application.ProcessMessages;
      AssignFile(F, savedialog1.filename);
      Rewrite(F);
      Writeln(F,'AStar Units File');
      S:=Inttostr(1); Writeln(F,S);

      If Unit6CB.Checked then S:='True' else S:='False';
       Writeln(F,S);
      S:=NumberofActiveUnitsEdit.Text;//Inttostr(NumberofUnits);
      Writeln(F,S);
//Unit1
      S:=Unit1StartXEdit.Text; Writeln(F,S);
      S:=Unit1StartYEdit.Text; Writeln(F,S);
      S:=Unit1TargetXEdit.Text; Writeln(F,S);
      S:=Unit1TargetYEdit.Text; Writeln(F,S);
      S:=Unit1SpeedEdit.Text; Writeln(F,S);
      S:=Unit1ManEdit.Text; Writeln(F,S);
      S:=Unit1ArmorEdit.Text; Writeln(F,S);
      S:=Unit1MEdit.Text; Writeln(F,S);
      S:=Unit1M1XEdit.Text; Writeln(F,S);
      S:=Unit1M1YEdit.Text; Writeln(F,S);
      S:=Unit1M2XEdit.Text; Writeln(F,S);
      S:=Unit1M2YEdit.Text; Writeln(F,S);
      S:=Unit1M3XEdit.Text; Writeln(F,S);
      S:=Unit1M3YEdit.Text; Writeln(F,S);
      S:=Unit1M4XEdit.Text; Writeln(F,S);
      S:=Unit1M4YEdit.Text; Writeln(F,S);
      S:=Unit1M5XEdit.Text; Writeln(F,S);
      S:=Unit1M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit1OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit1SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit1TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit1.Text; Writeln(F,S);
      S:=FileAmd1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp1Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit1ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit1.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit1.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
//Unit2
      S:=Unit2StartXEdit.Text; Writeln(F,S);
      S:=Unit2StartYEdit.Text; Writeln(F,S);
      S:=Unit2TargetXEdit.Text; Writeln(F,S);
      S:=Unit2TargetYEdit.Text; Writeln(F,S);
      S:=Unit2SpeedEdit.Text; Writeln(F,S);
      S:=Unit2ManEdit.Text; Writeln(F,S);
      S:=Unit2ArmorEdit.Text; Writeln(F,S);
      S:=Unit2MEdit.Text; Writeln(F,S);
      S:=Unit2M1XEdit.Text; Writeln(F,S);
      S:=Unit2M1YEdit.Text; Writeln(F,S);
      S:=Unit2M2XEdit.Text; Writeln(F,S);
      S:=Unit2M2YEdit.Text; Writeln(F,S);
      S:=Unit2M3XEdit.Text; Writeln(F,S);
      S:=Unit2M3YEdit.Text; Writeln(F,S);
      S:=Unit2M4XEdit.Text; Writeln(F,S);
      S:=Unit2M4YEdit.Text; Writeln(F,S);
      S:=Unit2M5XEdit.Text; Writeln(F,S);
      S:=Unit2M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit2OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit2SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit2TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit2.Text; Writeln(F,S);
      S:=FileAmd2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp2Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit2ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit2.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit2.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
//Unit3
      S:=Unit3StartXEdit.Text; Writeln(F,S);
      S:=Unit3StartYEdit.Text; Writeln(F,S);
      S:=Unit3TargetXEdit.Text; Writeln(F,S);
      S:=Unit3TargetYEdit.Text; Writeln(F,S);
      S:=Unit3SpeedEdit.Text; Writeln(F,S);
      S:=Unit3ManEdit.Text; Writeln(F,S);
      S:=Unit3ArmorEdit.Text; Writeln(F,S);
      S:=Unit3MEdit.Text; Writeln(F,S);
      S:=Unit3M1XEdit.Text; Writeln(F,S);
      S:=Unit3M1YEdit.Text; Writeln(F,S);
      S:=Unit3M2XEdit.Text; Writeln(F,S);
      S:=Unit3M2YEdit.Text; Writeln(F,S);
      S:=Unit3M3XEdit.Text; Writeln(F,S);
      S:=Unit3M3YEdit.Text; Writeln(F,S);
      S:=Unit3M4XEdit.Text; Writeln(F,S);
      S:=Unit3M4YEdit.Text; Writeln(F,S);
      S:=Unit3M5XEdit.Text; Writeln(F,S);
      S:=Unit3M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit3OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit3SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit3TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit3.Text; Writeln(F,S);
      S:=FileAmd3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp3Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit3ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit3.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit3.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
//Unit4
      S:=Unit4StartXEdit.Text; Writeln(F,S);
      S:=Unit4StartYEdit.Text; Writeln(F,S);
      S:=Unit4TargetXEdit.Text; Writeln(F,S);
      S:=Unit4TargetYEdit.Text; Writeln(F,S);
      S:=Unit4SpeedEdit.Text; Writeln(F,S);
      S:=Unit4ManEdit.Text; Writeln(F,S);
      S:=Unit4ArmorEdit.Text; Writeln(F,S);
      S:=Unit4MEdit.Text; Writeln(F,S);
      S:=Unit4M1XEdit.Text; Writeln(F,S);
      S:=Unit4M1YEdit.Text; Writeln(F,S);
      S:=Unit4M2XEdit.Text; Writeln(F,S);
      S:=Unit4M2YEdit.Text; Writeln(F,S);
      S:=Unit4M3XEdit.Text; Writeln(F,S);
      S:=Unit4M3YEdit.Text; Writeln(F,S);
      S:=Unit4M4XEdit.Text; Writeln(F,S);
      S:=Unit4M4YEdit.Text; Writeln(F,S);
      S:=Unit4M5XEdit.Text; Writeln(F,S);
      S:=Unit4M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit4OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit4SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit4TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit4.Text; Writeln(F,S);
      S:=FileAmd4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp4Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit4ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit4.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit4.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
//Unit5
      S:=Unit5StartXEdit.Text; Writeln(F,S);
      S:=Unit5StartYEdit.Text; Writeln(F,S);
      S:=Unit5TargetXEdit.Text; Writeln(F,S);
      S:=Unit5TargetYEdit.Text; Writeln(F,S);
      S:=Unit5SpeedEdit.Text; Writeln(F,S);
      S:=Unit5ManEdit.Text; Writeln(F,S);
      S:=Unit5ArmorEdit.Text; Writeln(F,S);
      S:=Unit5MEdit.Text; Writeln(F,S);
      S:=Unit5M1XEdit.Text; Writeln(F,S);
      S:=Unit5M1YEdit.Text; Writeln(F,S);
      S:=Unit5M2XEdit.Text; Writeln(F,S);
      S:=Unit5M2YEdit.Text; Writeln(F,S);
      S:=Unit5M3XEdit.Text; Writeln(F,S);
      S:=Unit5M3YEdit.Text; Writeln(F,S);
      S:=Unit5M4XEdit.Text; Writeln(F,S);
      S:=Unit5M4YEdit.Text; Writeln(F,S);
      S:=Unit5M5XEdit.Text; Writeln(F,S);
      S:=Unit5M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit5OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit5SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit5TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit5.Text; Writeln(F,S);
      S:=FileAmd5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp5Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit5ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit5.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit5.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
//Unit6
      S:=Unit6StartXEdit.Text; Writeln(F,S);
      S:=Unit6StartYEdit.Text; Writeln(F,S);
      S:=Unit6TargetXEdit.Text; Writeln(F,S);
      S:=Unit6TargetYEdit.Text; Writeln(F,S);
      S:=Unit6SpeedEdit.Text; Writeln(F,S);
      S:=Unit6ManEdit.Text; Writeln(F,S);
      S:=Unit6ArmorEdit.Text; Writeln(F,S);
      S:=Unit6MEdit.Text; Writeln(F,S);
      S:=Unit6M1XEdit.Text; Writeln(F,S);
      S:=Unit6M1YEdit.Text; Writeln(F,S);
      S:=Unit6M2XEdit.Text; Writeln(F,S);
      S:=Unit6M2YEdit.Text; Writeln(F,S);
      S:=Unit6M3XEdit.Text; Writeln(F,S);
      S:=Unit6M3YEdit.Text; Writeln(F,S);
      S:=Unit6M4XEdit.Text; Writeln(F,S);
      S:=Unit6M4YEdit.Text; Writeln(F,S);
      S:=Unit6M5XEdit.Text; Writeln(F,S);
      S:=Unit6M5YEdit.Text; Writeln(F,S);
      S:=Inttostr(Unit6OperatingOrdersRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit6SearchRG.ItemIndex); Writeln(F,S);
      S:=Inttostr(Unit6TieBreakerRG.ItemIndex); Writeln(F,S);
      S:=FileAff6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=TargetSizeEdit6.Text; Writeln(F,S);
      S:=FileAmd6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=FileAjp6Edit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=Unit6ActorScaleEdit.Text; Writeln(F,S);
      S:=WpnEdit6.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      S:=WpnTextureEdit6.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);          
      //Future Proofing
      //If Version > 1 then       begin       end;
      CloseFile(F);
    end;
  End;
end;


procedure TAProjectOptionsForm.OpenadfBtnClick(Sender: TObject);
begin  //FileAdfEdit
  OpenDialog.Filter:= 'astar Terrain Data|*.adf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.adf';
  if OpenDialog.Execute then
  begin
  Application.ProcessMessages;
  FileAdfEdit.Text:= OpenDialog.FileName;
  LoadTerrainFile(OpenDialog.FileName);
  DisplayTerrainFile;
  end;
end;

procedure TAProjectOptionsForm.SaveadfBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
Const NoName='NoName';  
Begin
  SaveDialog1.Filter:= 'astar Terrain Data|*.adf';
  SaveDialog1.InitialDir:=ProjectDirectory;
  SaveDialog1.FileName:='';//'myTerrainData.aof';
  if savedialog1.execute then
  Begin
    if uppercase(extractfileext(savedialog1.filename))='.ADF' then
    begin
      Application.ProcessMessages;
      AssignFile(F, savedialog1.filename);
      Rewrite(F);
      Writeln(F,'AStar Terrain File');
      S:=Inttostr(1); Writeln(F,S);
       S:=OpenHeightDataEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileImageTextureEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAmfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAefEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAvfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAsfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAhfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAtfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAafEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAnfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAofEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAifEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAgfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
       S:=FileAbfEdit.Text;
       If Length(S) = 0 then S:=  NoName; Writeln(F,S);
      //Future Proofing
      //If Version > 1 then       begin       end;
      CloseFile(F);
    end;
  End;
end;

procedure TAProjectOptionsForm.OpenaefBtnClick(Sender: TObject);
begin  //FileAefEdit
  OpenDialog.Filter:= 'astar Elevation|*.aef';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.aef';
  if OpenDialog.Execute then
  FileAefEdit.Text:= OpenDialog.FileName;
end;

procedure TAProjectOptionsForm.SaveaefBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenasfBtnClick(Sender: TObject);
begin   // FileAsfEdit
  OpenDialog.Filter:= 'astar Soils|*.asf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.asf';
  if OpenDialog.Execute then
  FileAsfEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveasfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenahfBtnClick(Sender: TObject);
begin   // FileAhfEdit
  OpenDialog.Filter:= 'astar Hydrology|*.ahf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.ahf';
  if OpenDialog.Execute then
  FileAhfEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveahfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenavfBtnClick(Sender: TObject);
begin       //  FileAvfEdit
  OpenDialog.Filter:= 'astar Vegetation|*.avf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.avf';
  if OpenDialog.Execute then
  FileAvfEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveavfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenatfBtnClick(Sender: TObject);
begin    //  FileAtfEdit
  OpenDialog.Filter:= 'astar Transportation|*.atf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.atf';
  if OpenDialog.Execute then
  FileAtfEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveatfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenaafBtnClick(Sender: TObject);
begin   //FileAafEdit
  OpenDialog.Filter:= 'astar Urban Areas|*.aaf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.aaf';
  if OpenDialog.Execute then
  FileAafEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveaafBtnClick(Sender: TObject);
begin
  //
end;

procedure TAProjectOptionsForm.OpenaofBtnClick(Sender: TObject);
begin   //  FileAofEdit
  OpenDialog.Filter:= 'astar Obstacles|*.aof';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.aof';
  if OpenDialog.Execute then
  FileAofEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveaofBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenaifBtnClick(Sender: TObject);
begin   // FileAmfEdit
  OpenDialog.Filter:= 'astar Influence Map|*.aif';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.aif';
  if OpenDialog.Execute then
  FileAifEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveaifBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenHeightDataBtnClick(Sender: TObject);
begin //OpenHeightDataEdit
  //OpenDialog.Filter:= 'astar Image (bmp or htf)|*.bmp;*.htf';
  OpenDialog.Filter:= 'astar HeightData (bmp)|*.bmp';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.bmp';
  if OpenDialog.Execute then
  OpenHeightDataEdit.Text:= OpenDialog.FileName;
end;

procedure TAProjectOptionsForm.OpenImageTextureBtnClick(Sender: TObject);
begin   // FileAifEdit
  OpenDialog.Filter:= 'astar Texture Image (bmp)|*.bmp';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.bmp';
  if OpenDialog.Execute then
  FileImageTextureEdit.Text:= OpenDialog.FileName;
end;

procedure TAProjectOptionsForm.OpenagfBtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar G Cost|*.agf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.agf';
  if OpenDialog.Execute then
  begin
  Application.ProcessMessages;
  FileAgfEdit.Text:= OpenDialog.FileName;
  ProjectRecord.GCostFile:=OpenDialog.FileName;
  LoadGCostValues;
  end;
end;
procedure TAProjectOptionsForm.SaveagfBtnClick(Sender: TObject);
begin
 //
end;

procedure TAProjectOptionsForm.OpenabfBtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar MAG Values|*.abf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.abf';
  if OpenDialog.Execute then
  begin
  Application.ProcessMessages;
  FileAbfEdit.Text:= OpenDialog.FileName;
  ProjectRecord.MAGValuesFile:=OpenDialog.FileName;
  LoadMAGValues;
  end;
end;

procedure TAProjectOptionsForm.SaveabfBtnClick(Sender: TObject);
Begin
  SaveDialog1.Filter:= 'astar MAG B|*.abf';
  SaveDialog1.InitialDir:=ProjectDirectory;
  SaveDialog1.FileName:='';//'myTerrainData.aof';
  if savedialog1.execute then
  Begin
    if uppercase(extractfileext(savedialog1.filename))='.ABF' then
    begin
      Application.ProcessMessages;
      ProjectDirectory:=ExtractFilePath(savedialog1.FileName);
      FileAbfEdit.Text:= savedialog1.FileName;
      ProjectRecord.MAGValuesFile:=savedialog1.FileName;
      SaveMAGValues;
    End;
  end;
end;
procedure TAProjectOptionsForm.MAGResettoDefaultsBtnClick(Sender: TObject);
begin
  ResetMAGDefaults;
end;
procedure TAProjectOptionsForm.PrintabfBtnClick(Sender: TObject);
var
  F: TextFile;
  SAll,S,Sname,Sr,Sg,Sb: string;
  i,ii:Integer;
Const NoName='NoName';
Begin
//  MAGValuesFileName  SetLength(MAGColorValueArray,0);
  //showmessage(MAGValuesFileName);
  showmessage(ProjectDirectory+'MAGValuesPrint.txt');
  AssignFile(F, ProjectDirectory+'MAGValuesPrint.txt');
      //AssignFile(F, MAGValuesFileName);
      Rewrite(F);
      writeln(F,'AStar MAG File');
      S:=Inttostr(1); writeln(F,'Version '+S);
      ii:= Length(MAGColorValueArray);
      S:=Inttostr(ii); writeln(F,'Number of Values '+S);
      //showmessage(s);
      For I:=0 to ii-1 do
      begin
        Sr:=format( '%.3d',[GetRValue(MAGColorValueArray[I][1])]);
        Sg:=format( '%.3d',[GetRValue(MAGColorValueArray[I][1])]);
        Sb:=format( '%.3d',[GetRValue(MAGColorValueArray[I][1])]);
        S:=format( '%.3d',[MAGColorValueArray[I][2]]);
Case I of
 0: Sname:='x-Base...........';
 1: Sname:='T-Hiway..........';
 2: Sname:='T-Road...........';
 3: Sname:='T-Street.........';
 4: Sname:='T-Dirt Trail.....';
 5: Sname:='S-Bare...........';
 6: Sname:='S-Plowed field...';
 7: Sname:='S-Rocky Boulders.';
 8: Sname:='S-Muck...........';
 9: Sname:='S-Swamp..........';
10: Sname:='S-Swamp Lake.....';
11: Sname:='S-Sand  hard pack';
12: Sname:='S-Sand Dunes.....';
13: Sname:='V-Grass..........';
14: Sname:='V-Mixed..........';
15: Sname:='V-Shrub..........';
16: Sname:='V-Forest.........';
17: Sname:='V-Jungle.........';
18: Sname:='H-Stream fordable';
19: Sname:='H-River..........';
20: Sname:='H-Lake...........';
21: Sname:='H-Snow<1 ft......';
22: Sname:='H-Snow>1 ft......';
23: Sname:='H-Ice............';
24: Sname:='U-Housing........';
25: Sname:='U-Urban areas....';
26: Sname:='O-NoGo areas.....';
end;
        SAll:='MAG '+format( '%.2d',[I])+ ' '+Sname+
              ' Value '+S+
              ' Red '+(Sr)+' Green ' +(Sr)+' Blue '+(Sb)
              ;
        writeln(F,SAll);
      end;
    CloseFile(F);
      Application.ProcessMessages;
  if FileExists(ProjectDirectory+'MAGValuesPrint.txt') then
    ExecuteFile('MAGValuesPrint.txt', '', ProjectDirectory, SW_SHOW)
End;


procedure TAProjectOptionsForm.OpenamfBtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar Map Attributes|*.amf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.amf';
  if OpenDialog.Execute then
  FileAmfEdit.Text:= OpenDialog.FileName;
end;

procedure TAProjectOptionsForm.SaveamfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.OpenanfBtnClick(Sender: TObject);
begin   // FileAnfEdit
  OpenDialog.Filter:= 'astar NOGO|*.anf';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.anf';
  if OpenDialog.Execute then
  FileAnfEdit.Text:= OpenDialog.FileName;
end;
procedure TAProjectOptionsForm.SaveanfBtnClick(Sender: TObject);
begin
//
end;

procedure TAProjectOptionsForm.Openaff1BtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar unit flag jpg bmp|*.jpg;*.bmp';
  OpenDialog.InitialDir:=//ProjectDirectory;
          ExtractFilePath(Application.ExeName)+'Flags';
  OpenDialog.FileName:='';//'*.bmp';
  if OpenDialog.Execute then
  case TComponent(Sender).Tag of
  1:begin FileAff1Edit.Text:= OpenDialog.FileName; End;
  2:begin FileAff2Edit.Text:= OpenDialog.FileName; End;
  3:begin FileAff3Edit.Text:= OpenDialog.FileName; End;
  4:begin FileAff4Edit.Text:= OpenDialog.FileName; End;
  5:begin FileAff5Edit.Text:= OpenDialog.FileName; End;
  6:begin FileAff6Edit.Text:= OpenDialog.FileName; End;
  End;//case
end;

procedure TAProjectOptionsForm.Openamd1BtnClick(Sender: TObject);
begin                     //actors
  OpenDialog.Filter:= 'astar Actor md2|*.md2';
  OpenDialog.InitialDir:=//ProjectDirectory;
          ExtractFilePath(Application.ExeName)+'Actors';
  OpenDialog.FileName:='*.md2';
  if OpenDialog.Execute then
  case TComponent(Sender).Tag of
  1:begin FileAmd1Edit.Text:= OpenDialog.FileName; End;
  2:begin FileAmd2Edit.Text:= OpenDialog.FileName; End;
  3:begin FileAmd3Edit.Text:= OpenDialog.FileName; End;
  4:begin FileAmd4Edit.Text:= OpenDialog.FileName; End;
  5:begin FileAmd5Edit.Text:= OpenDialog.FileName; End;
  6:begin FileAmd6Edit.Text:= OpenDialog.FileName; End;
  End;//case
end;

procedure TAProjectOptionsForm.Openajp1BtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar Actor jpg bmp|*.jpg;*.bmp';
  OpenDialog.InitialDir:=//ProjectDirectory;
          ExtractFilePath(Application.ExeName)+'Actors';
  OpenDialog.FileName:='';
  if OpenDialog.Execute then
  case TComponent(Sender).Tag of
  1:begin FileAjp1Edit.Text:= OpenDialog.FileName; End;
  2:begin FileAjp2Edit.Text:= OpenDialog.FileName; End;
  3:begin FileAjp3Edit.Text:= OpenDialog.FileName; End;
  4:begin FileAjp4Edit.Text:= OpenDialog.FileName; End;
  5:begin FileAjp5Edit.Text:= OpenDialog.FileName; End;
  6:begin FileAjp6Edit.Text:= OpenDialog.FileName; End;
  End;//case
end;





procedure TAProjectOptionsForm.OpenTexture6BtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar Actor Weapon Texture jpg bmp|*.jpg;*.bmp';
  OpenDialog.InitialDir:=//ProjectDirectory;
          ExtractFilePath(Application.ExeName)+'Actors';
  OpenDialog.FileName:='';
  if OpenDialog.Execute then
  case TComponent(Sender).Tag of
  1:begin WpnTextureEdit1.Text:= OpenDialog.FileName; End;
  2:begin WpnTextureEdit2.Text:= OpenDialog.FileName; End;
  3:begin WpnTextureEdit3.Text:= OpenDialog.FileName; End;
  4:begin WpnTextureEdit4.Text:= OpenDialog.FileName; End;
  5:begin WpnTextureEdit5.Text:= OpenDialog.FileName; End;
  6:begin WpnTextureEdit6.Text:= OpenDialog.FileName; End;
  End;//case
end;

procedure TAProjectOptionsForm.OpenWpn1BtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar Actor Weapon md2|*.md2';
  OpenDialog.InitialDir:=//ProjectDirectory;
          ExtractFilePath(Application.ExeName)+'Actors';
  OpenDialog.FileName:='*.md2';
  if OpenDialog.Execute then
  case TComponent(Sender).Tag of
  1:begin WpnEdit1.Text:= OpenDialog.FileName; End;
  2:begin WpnEdit2.Text:= OpenDialog.FileName; End;
  3:begin WpnEdit3.Text:= OpenDialog.FileName; End;
  4:begin WpnEdit4.Text:= OpenDialog.FileName; End;
  5:begin WpnEdit5.Text:= OpenDialog.FileName; End;
  6:begin WpnEdit6.Text:= OpenDialog.FileName; End;
  End;//case
end;





end.
