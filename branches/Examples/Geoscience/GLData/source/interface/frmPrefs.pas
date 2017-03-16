{-----------------------------------------------------------------------------
 Unit Name: frmPrefs
 Author:    HochwimmerA
 Purpose:   glData Preferences
 $Id: frmPrefs.pas,v 1.37 2004/07/08 09:50:58 hochwimmera Exp $
-----------------------------------------------------------------------------}
unit frmPrefs;

interface

uses
  Windows,Messages,SysUtils,Variants,Classes,Graphics,Controls,Forms,Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, ImgList,

// gldata Component Library
  geFloatEdit,geIntegerEdit;

type
  TFormPreferences = class(TForm)
    bCancel: TBitBtn;
    bLoadArcInfoCLR: TButton;
    bLoadDataCLR: TButton;
    bLoadSurferCLR: TButton;
    bLoadVectorDataCLR: TButton;
    bOK: TBitBtn;
    bResetScale: TButton;
    bUpdate: TBitBtn;
    cbArcInfoColourMode: TComboBox;
    cbArcInfoPolygonMode: TComboBox;
    cbCameraStyle: TComboBox;
    cbLineMode: TComboBox;
    cbSurferColourMode : TComboBox;
    cbSurferPolygonMode : TComboBox;

    cbxAutoCenterPoints: TCheckBox;
    cbxArcInfoTwoSided: TCheckBox;
    cbxAutoCenterGrids: TCheckBox;
    cbxAutoProcess: TCheckBox;
    cbxColourScaleBorder: TCheckBox;
    cbxColourScaleContinous: TCheckBox;
    cbxColourScaleShowContours: TCheckBox;
    cbxDisplayAxes: TCheckBox;
    cbxDisplayLine: TCheckBox;
    cbxDisplayMarkers: TCheckBox;
    cbxDisplayPoints: TCheckBox;
    cbxFocusStatusBar: TCheckBox;
    cbxLightingShining: TCheckBox;
    cbxPointWireFrame: TCheckBox;
    cbxPromptTexArcInfo: TCheckBox;
    cbxPromptTexSurfer: TCheckBox;
    cbxShowColourScale: TCheckBox;
    cbxShowHUDScale: TCheckBox;
    cbxShowPipes: TCheckBox;
    cbxSurferTwoSided: TCheckBox;
    cbxTwoSideLighting: TCheckBox;
    ColourDialog: TColorDialog;
    ebArcInfoCLRFile: TEdit;
    ebDataCLRFile: TEdit;
    ebSurferCLRFile: TEdit;
    geArcInfoAlpha: TGEFloatEdit;
    geColourScaleAlpha: TGEFloatEdit;
    geDepthOfView: TGEFloatEdit;
    geFocalLength: TGEFloatEdit;
    geScaleX: TGEFloatEdit;
    geScaleY: TGEFloatEdit;
    geScaleZ: TGEFloatEdit;
    geSurferAlpha: TGEFloatEdit;
    geTileArcInfoX: TGEIntegerEdit;
    geTileArcInfoY: TGEIntegerEdit;
    geTileSurferX: TGEIntegerEdit;
    geTileSurferY: TGEIntegerEdit;
    geVectorMinLength: TGEFloatEdit;
    ImageList: TImageList;
    lblArcInfoBaseMap: TLabel;
    lblArcInfoBlankedColour: TLabel;
    lblArcInfoCLRFile: TLabel;
    lblArcInfoMaxColour: TLabel;
    lblArcInfoMinColour: TLabel;
    lblArcInfoPalette: TLabel;
    lblArcInfoPolygonMode: TLabel;
    lblArcInfoSingleColour: TLabel;
    lblCameraDepthOfView: TLabel;
    lblCameraStyle: TLabel;
    lblColourLine: TLabel;
    lblColourPoint: TLabel;
    lblDataCLRFile: TLabel;
    lblFocalLength: TLabel;
    lblLineMode: TLabel;
    lblPipeRadius: TLabel;
    lblPointColour: TLabel;
    lblPointNullColour: TLabel;
    lblPointMaxColour: TLabel;
    lblPointMinColour: TLabel;
    lblPointRadius: TLabel;
    lblScalarOptions: TLabel;
    lblScaleX: TLabel;
    lblScaleY: TLabel;
    lblScaleZ: TLabel;
    lblSurferAlpha: TLabel;
    lblSurferBaseMap: TLabel;
    lblSurferBlankedColour: TLabel;
    lblSurferCLRFile: TLabel;
    lblSurferMaxColour: TLabel;
    lblSurferMinColour: TLabel;
    lblSurferPalette: TLabel;
    lblSurferPolygonMode: TLabel;
    lblSurferSingleColour: TLabel;
    lblTileArcInfoY: TLabel;
    lblTileArcInfoX: TLabel;
    lblTileSurferX: TLabel;
    lblTileSurferY: TLabel;
    lblVectorDataCLRFile: TLabel;
    OpenDialogCLR: TOpenDialog;
    pcPreferences: TPageControl;
    pnlArcInfoBlankedColour: TPanel;
    pnlArcInfoColour: TPanel;
    pnlArcInfoMaxColour: TPanel;
    pnlArcInfoMinColour: TPanel;
    pnlBackgroundColour: TPanel;
    pnlColourLine: TPanel;
    pnlColourSetting: TPanel;
    pnlLeft: TPanel;
    pnlMaxPointColour: TPanel;
    pnlMinPointColour: TPanel;
    pnlNullPointColour: TPanel;
    pnlOptions: TPanel;
    pnlSurferBlankedColour: TPanel;
    pnlSurferColour: TPanel;
    pnlSurferMinColour: TPanel;
    pnlSurferMaxColour: TPanel;
    rgArcInfoOptions: TRadioGroup;
    rgPointOptions: TRadioGroup;
    rgSurferOptions: TRadioGroup;
    tsArcInfo: TTabSheet;
    tsAxes: TTabSheet;
    tsCamera: TTabSheet;
    tsData: TTabSheet;
    tsDisplay: TTabSheet;
    tsGeneral: TTabSheet;
    tsGrids: TTabSheet;
    tsHUD: TTabSheet;
    tsInterface: TTabSheet;
    tsLighting: TTabSheet;
    tsLines: TTabSheet;
    tsMarkers: TTabSheet;
    tsPipes: TTabSheet;
    tsScale: TTabSheet;
    tsSurfer: TTabSheet;
    tvPrefs: TTreeView;
    lblColourScaleAlpha: TLabel;
    pnlSinglePointColour: TPanel;
    lblPointSingleColour: TLabel;
    cbColourScaleType: TComboBox;
    ebScaleLabelFormat: TEdit;
    lblScaleLabelFormat: TLabel;
    cbxSurferSilentImport: TCheckBox;
    lblImportSurfer: TLabel;
    cbxSurferSilentLoad: TCheckBox;
    tsVectorData: TTabSheet;
    lblVectorOptions: TLabel;
    rgVectorDataOptions: TRadioGroup;
    lblVectorMaxColour: TLabel;
    lblVectorMinColour: TLabel;
    lblVectorSingleColour: TLabel;
    pnlSingleVectorColour: TPanel;
    pnlMinVectorColour: TPanel;
    pnlMaxVectorColour: TPanel;
    ebVectorDataCLRFile: TEdit;
    cbxRenderNullPoints: TCheckBox;
    cbxInvertMouseWheel: TCheckBox;
    cbxCameraStatusBar: TCheckBox;
    lblVectorScaleOptions: TLabel;
    bArrowLengthSync: TButton;
    lblVectorMinLength: TLabel;
    lblVectorMaxLength: TLabel;
    lblVectorMinArrowRadius: TLabel;
    lblVectorMaxArrowRadius: TLabel;
    bArrowHeadRadiusSync: TButton;
    lblVectorArrowHeadRadius: TLabel;
    lblVectorMinRadius: TLabel;
    lblVectorMaxRadius: TLabel;
    bArrowRadiusSync: TButton;
    Label4: TLabel;
    lblVectorMaxArrowLength: TLabel;
    lblVectorMinArrowLength: TLabel;
    Label5: TLabel;
    bArrowHeadLengthSync: TButton;
    Label2: TLabel;
    geMarkerRadius: TGEFloatEdit;
    gePipeRadius: TGEFloatEdit;
    geVectorMaxLength: TGEFloatEdit;
    geVectorMinRadius: TGEFloatEdit;
    geVectorMaxRadius: TGEFloatEdit;
    geVectorMaxArrowLength: TGEFloatEdit;
    geVectorMinArrowLength: TGEFloatEdit;
    geVectorMinArrowRadius: TGEFloatEdit;
    geVectorMaxArrowRadius: TGEFloatEdit;
    geVectorSlices: TGEIntegerEdit;
    geVectorStacks: TGEIntegerEdit;
    lblVectorSlices: TLabel;
    lblVectorStacks: TLabel;
    geCameraNearPlaneBias: TGEFloatEdit;
    lblCameraNearPlaneBias: TLabel;
    cbPointStyle: TComboBox;
    lblPointStyle: TLabel;
    tsGeothermal: TTabSheet;
    rgGeothermalOptions: TRadioGroup;
    bLoadGeothermalCLR: TButton;
    lblGeothermalCLR: TLabel;
    ebGeothermalCLRFile: TEdit;
    lblGeothermalMaxColour: TLabel;
    pnlGeothermalMaxColour: TPanel;
    lblGeothermalMinColour: TLabel;
    pnlGeothermalMinColour: TPanel;
    lblGeothermalSingleColour: TLabel;
    pnlGeothermalColour: TPanel;
    lblImportArcInfo: TLabel;
    cbxArcInfoSilentImport: TCheckBox;
    cbxArcInfoSilentLoad: TCheckBox;
    cbAntialiasing: TComboBox;
    lblAntialiasing: TLabel;
    sbModified: TStatusBar;
    tsScalarBox: TTabSheet;
    tsVectorBox: TTabSheet;
    lblScalarBoundingColour: TLabel;
    pnlScalarBoundingColour: TPanel;
    lblScalarBB: TLabel;
    pnlVectorBoundingColour: TPanel;
    lblVectorBoxLineColour: TLabel;
    lblVectorBB: TLabel;
    cbxVectorAA: TCheckBox;
    cbxVectorSmooth: TCheckBox;
    lblVectorBoxLinePattern: TLabel;
    geVectorLinePattern: TGEIntegerEdit;
    cbxScalarAA: TCheckBox;
    cbxScalarSmooth: TCheckBox;
    lblScalarBoxLinePattern: TLabel;
    geScalarLinePattern: TGEIntegerEdit;
    cbxSurferCreateVisible: TCheckBox;
    cbxLighting2Shining: TCheckBox;
    cbxHUDScaleSteps: TCheckBox;
    geHUDPoints: TGEIntegerEdit;
    cbShadeModel: TComboBox;
    lblShadeModel: TLabel;

    procedure bCancelClick(Sender: TObject);
    procedure bLoadArcInfoCLRClick(Sender: TObject);
    procedure bLoadDataCLRClick(Sender: TObject);
    procedure bLoadSurferCLRClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bResetScaleClick(Sender: TObject);
    procedure bUpdateClick(Sender: TObject);

    procedure cbxArcInfoTwoSidedClick(Sender: TObject);
    procedure cbxSurferTwoSidedClick(Sender: TObject);
    procedure ebArcInfoCLRFileChange(Sender: TObject);
    procedure ebSurferCLRFileChange(Sender: TObject);
    procedure pnlArcInfoBlankedColourClick(Sender: TObject);
    procedure pnlArcInfoColourClick(Sender: TObject);
    procedure pnlArcInfoMaxColourClick(Sender: TObject);
    procedure pnlArcInfoMinColourClick(Sender: TObject);
    procedure pnlBackgroundColourClick(Sender: TObject);
    procedure pnlColourLineClick(Sender: TObject);
    procedure pnlColourSettingClick(Sender: TObject);
    procedure pnlMaxPointColourClick(Sender: TObject);
    procedure pnlMinPointColourClick(Sender: TObject);
    procedure pnlNullPointColourClick(Sender: TObject);
    procedure pnlSurferColourClick(Sender: TObject);
    procedure pnlSurferMaxColourClick(Sender: TObject);
    procedure pnlSurferMinColourClick(Sender: TObject);
    procedure pnlSurferBlankedColourClick(Sender: TObject);
    procedure tvPrefsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure pnlSinglePointColourClick(Sender: TObject);
    procedure ebDataCLRFileChange(Sender: TObject);
    procedure pnlSingleVectorColourClick(Sender: TObject);
    procedure pnlMinVectorColourClick(Sender: TObject);
    procedure pnlMaxVectorColourClick(Sender: TObject);
    procedure bArrowLengthSyncClick(Sender: TObject);
    procedure bArrowHeadRadiusSyncClick(Sender: TObject);
    procedure bArrowRadiusSyncClick(Sender: TObject);
    procedure bArrowHeadLengthSyncClick(Sender: TObject);



    procedure UpdateModifiedOnly(Sender: TObject);
    procedure UpdateGeothermalModified(Sender:TObject);
    procedure UpdatedModifiedOnly(Sender: TObject);
    procedure UpdatedSurferModified(Sender: TObject);
    procedure bLoadVectorDataCLRClick(Sender: TObject);
    procedure ebVectorDataCLRFileChange(Sender: TObject);

    procedure bLoadGeothermalCLRClick(Sender: TObject);
    procedure ebGeothermalCLRFileChange(Sender: TObject);
    procedure pnlGeothermalColourClick(Sender: TObject);
    procedure pnlGeothermalMinColourClick(Sender: TObject);
    procedure pnlGeothermalMaxColourClick(Sender: TObject);
    procedure pnlScalarBoundingColourClick(Sender: TObject);
    procedure pnlVectorBoundingColourClick(Sender: TObject);
  private
    bModifiedArcInfo : boolean;
    bModifiedSurfer: boolean;
    bModifiedGeothermal : boolean;
    bUpdateMe : boolean;
    procedure SetArcInfoModified;
    procedure SetGeothermalModified;
    procedure SetSurferModified;
    procedure SetModified;
    procedure UpdateColour(pnl:TPanel;iMode:integer);
  public
    bContinue : boolean;
    bLoading : boolean;
    function IsArcInfoModified:boolean;
    function IsGeothermalModified:boolean;
    function IsSurferModified:boolean;
  end;

implementation

{$R *.dfm}
// ----- TformPreferences.UpdateColour -----------------------------------------
procedure TformPreferences.UpdateColour(pnl:TPanel;iMode:integer);

begin
  with ColourDialog do
  begin
    Color := pnl.Color;
    if Execute then
    begin
      pnl.Color := Color;
      case iMode of
        1: SetModified;
        2: SetSurferModified;
        3: SetArcInfoModified;
        4: SetGeothermalModified;
      end;
    end;
  end;
end;
// ----- TformPreferences.bCancelClick -----------------------------------------
procedure TformPreferences.bCancelClick(Sender: TObject);

begin
  bContinue := false;
end;
// ----- TformPreferences.bLoadArcInfoCLRClick ---------------------------------
procedure TformPreferences.bLoadArcInfoCLRClick(Sender: TObject);

begin
  if OpenDialogCLR.Execute then
    ebArcInfoCLRFile.Text := OpenDialogCLR.FileName;
end;
// ----- TformPreferences.bLoadDataCLRClick ------------------------------------
procedure TformPreferences.bLoadDataCLRClick(Sender: TObject);

begin
  if OpenDialogCLR.Execute then
    ebDataCLRFile.Text := OpenDialogCLR.FileName;
end;
// ----- TformPreferences.bLoadSurferCLRClick ----------------------------------
procedure TformPreferences.bLoadSurferCLRClick(Sender: TObject);

begin
  if OpenDialogCLR.Execute then
    ebSurferCLRFile.Text := OpenDialogCLR.FileName;
end;
// ----- TformPreferences.bOKClick ---------------------------------------------
procedure TformPreferences.bOKClick(Sender: TObject);
begin
  bContinue := false;
end;
// ----- TformPreferences.bResetScaleClick -------------------------------------
procedure TformPreferences.bResetScaleClick(Sender: TObject);

begin
{
  geScaleX.Value := 1.0;
  geScaleY.Value := 1.0;
  geScaleZ.Value := 1.0;
}
end;
// ----- TformPreferences.bUpdateClick -----------------------------------------
procedure TformPreferences.bUpdateClick(Sender: TObject);

begin
  bContinue := true;
  bUpdateMe := true;
end;
// ----- TformPreferences.cbxArcInfoTwoSidedClick ------------------------------
procedure TformPreferences.cbxArcInfoTwoSidedClick(Sender: TObject);
begin
  SetArcInfoModified;
end;
// ----- TformPreferences.cbxSurferTwoSidedClick -------------------------------
procedure TformPreferences.cbxSurferTwoSidedClick(Sender: TObject);
begin
  SetSurferModified;
end;
// ----- TformPreferences.ebArcInfoCLRFileChange -------------------------------
procedure TformPreferences.ebArcInfoCLRFileChange(Sender: TObject);
begin
  if (not FileExists(ebArcInfoCLRFile.Text)) then
    ebArcInfoCLRFile.Color := clInfoBk
  else
    ebArcInfoCLRFile.Color := clWindow;
  SetArcInfoModified;
end;
// ----- TformPreferences.ebSurferCLRFileChange --------------------------------
procedure TformPreferences.ebSurferCLRFileChange(Sender: TObject);
begin
  if not FileExists(ebSurferCLRFile.Text) then
    ebSurferCLRFile.Color := clInfoBk
  else
    ebSurferCLRFile.Color := clWindow;
  SetSurferModified;
end;
// ----- TformPreferences.pnlArcInfoBlankedColourClick -------------------------
procedure TformPreferences.pnlArcInfoBlankedColourClick(Sender: TObject);

begin
  UpdateColour(pnlArcInfoBlankedColour,3);
end;
// ----- TformPreferences.pnlArcInfoColourClick --------------------------------
procedure TformPreferences.pnlArcInfoColourClick(Sender: TObject);

begin
  UpdateColour(pnlArcInfoColour,3);
end;
// ----- TformPreferences.pnlArcInfoMaxColourClick -----------------------------
procedure TformPreferences.pnlArcInfoMaxColourClick(Sender: TObject);

begin
  UpdateColour(pnlArcInfoMaxColour,3);
end;
// ----- TformPreferences.pnlArcInfoMinColourClick -----------------------------
procedure TformPreferences.pnlArcInfoMinColourClick(Sender: TObject);

begin
  UpdateColour(pnlArcInfoMinColour,3);
end;
// ----- TformPreferences.pnlBackgroundColourClick -----------------------------
procedure TformPreferences.pnlBackgroundColourClick(Sender: TObject);

begin
  UpdateColour(pnlBackgroundColour,1);
end;
// ----- TformPreferences.pnlColourLineClick -----------------------------------
procedure TformPreferences.pnlColourLineClick(Sender: TObject);

begin
  UpdateColour(pnlColourLine,1);
end;
// ----- TformPreferences.pnlColourSettingClick --------------------------------
procedure TformPreferences.pnlColourSettingClick(Sender: TObject);

begin
  UpdateColour(pnlColourSetting,1);
end;
// ----- TformPreferences.pnlMaxPointColourClick -------------------------------
procedure TformPreferences.pnlMaxPointColourClick(Sender: TObject);

begin
  UpdateColour(pnlMaxPointColour,1);
end;
// ----- TformPreferences.pnlMinPointColourClick -------------------------------
procedure TformPreferences.pnlMinPointColourClick(Sender: TObject);

begin
  UpdateColour(pnlMinPointColour,1);
end;
// ----- TformPreferences.pnlNullPointColourClick ------------------------------
procedure TformPreferences.pnlNullPointColourClick(Sender: TObject);

begin
  UpdateColour(pnlNullPointColour,1);
end;
// ----- TformPreferences.pnlSinglePointColourClick ----------------------------
procedure TformPreferences.pnlSinglePointColourClick(Sender: TObject);

begin
  UpdateColour(pnlSinglePointColour,1);
end;
// ----- TformPreferences.pnlSurferBlankedColourClick --------------------------
procedure TformPreferences.pnlSurferBlankedColourClick(Sender: TObject);

begin
  UpdateColour(pnlSurferBlankedColour,2);
end;
// ----- TformPreferences.pnlSurferColourClick ---------------------------------
procedure TformPreferences.pnlSurferColourClick(Sender: TObject);

begin
  UpdateColour(pnlSurferColour,2);
end;
// ----- TformPreferences.pnlSurferMaxColourClick ------------------------------
procedure TformPreferences.pnlSurferMaxColourClick(Sender: TObject);

begin
  UpdateColour(pnlSurferMaxColour,2);
end;
// ----- TformPreferences.pnlSurferMinColourClick ------------------------------
procedure TformPreferences.pnlSurferMinColourClick(Sender: TObject);

begin
  UpdateColour(pnlSurferMinColour,2);
end;
// ----- TformPreferences.tvPrefsClick -----------------------------------------
procedure TformPreferences.tvPrefsClick(Sender: TObject);

begin
  if (tvPrefs.Selected.Text = 'General') then
    pcPreferences.ActivePage := tsGeneral
  else if (tvPrefs.Selected.Text = 'Interface') then
    pcPreferences.ActivePage := tsInterface
  else if (tvPrefs.Selected.Text = 'Display') then
    pcPreferences.ActivePage := tsDisplay
  else if (tvPrefs.Selected.Text = 'Axes') then
    pcPreferences.ActivePage := tsAxes
  else if (tvPrefs.Selected.Text = 'Markers') then
    pcPreferences.ActivePage := tsMarkers
  else if (tvPrefs.Selected.Text = 'Pipe') then
    pcPReferences.ActivePage := tsPipes
  else if (tvPrefs.Selected.Text = 'Lines') then
    pcPreferences.ActivePage := tsLines
  else if (tvPrefs.Selected.Text = 'Grids') then
    pcPreferences.ActivePage := tsGrids
  else if (tvPrefs.Selected.Text = 'Surfer') then
    pcPreferences.ActivePage := tsSurfer
  else if (tvPrefs.Selected.Text = 'ESRI ArcInfo') then
    pcPreferences.ActivePage := tsArcInfo
  else if (tvPrefs.Selected.Text = 'Camera') then
    pcPreferences.ActivePage := tsCamera
  else if (tvPrefs.Selected.Text = 'Scalar Data') then
    pcPreferences.ActivePage := tsData
  else if (tvPrefs.Selected.Text = 'Vector Data') then
    pcPreferences.ActivePage := tsVectorData
  else if (tvPrefs.Selected.Text = 'Lighting') then
    pcPreferences.ActivePage := tsLighting
  else if (tvPrefs.Selected.Text = 'HUD') then
    pcPreferences.ActivePage := tsHUD
  else if (tvPrefs.Selected.Text = 'Scale') then
    pcPreferences.ActivePage := tsScale
  else if (tvPrefs.Selected.Text = 'Geothermal') then
    pcPreferences.ActivePage := tsGeothermal
  else if (tvPrefs.Selected.text = 'Scalar Box') then
    pcPreferences.ActivePage := tsScalarBox
  else if (tvPRefs.Selected.Text ='Vector Box') then
    pcPreferences.ActivePage := tsVectorBox;
end;
// ----- TformPreferences.FormClose --------------------------------------------
procedure TformPreferences.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
// check for null entries for Edits
{
  if (gePipeRadius.IsNull or
    geSurferAlpha.IsNull or geTileSurferX.IsNull or geTileSurferY.IsNull or
    geVectorMinLength.IsNull or geVectorMaxLength.IsNull or
    geVectorMinRadius.IsNull or geVectorMaxRadius.IsNull or
    geVectorMinArrowLength.IsNull or geVectorMaxArrowLength.IsNull or
    geVectorMinArrowRadius.IsNull or geVectorMaxArrowRadius.IsNull) then
  begin
    MessageDlg('NULL entries for numerical edit boxes are not allowed!',mtError,
      [mbOK],0);
    Action := caNone;
  end else
  begin
    if (geVectorMinLength.Value > geVectorMaxLength.Value) then
      geVectorMinLength.Value := geVectorMaxLength.Value;
    if (geVectorMinRadius.Value > geVectorMaxRadius.Value) then
      geVectorMinRadius.Value := geVectorMaxRadius.Value;
    if (geVectorMinArrowLength.Value > geVectorMaxArrowLength.Value) then
      geVectorMinArrowLength.Value := geVectorMaxArrowLength.Value;
    if (geVectorMinArrowRadius.Value > geVectorMaxArrowRadius.Value) then
      geVectorMinArrowRadius.Value := geVectorMaxArrowRadius.Value;

    if not bUpdateMe then
      bContinue := false;
  end;
  }
end;
// ----- TformPreferences.FormShow ---------------------------------------------
procedure TformPreferences.FormShow(Sender: TObject);

var
  sExePath,sCLRPath : string;
  i:integer;

begin
  bLoading := true;

// set up the spectrum path - where the CLR files should be
  sExePath := ExtractFilePath(Application.ExeName);
  sCLRpath := sExePath + '\spectrums';
  if DirectoryExists(sCLRPath) then
    OpenDialogCLR.InitialDir := sCLRPath
  else
    OpenDialogCLR.InitialDir := sExePath;

  bModifiedSurfer := false;
  bModifiedArcInfo := false;
  bModifiedGeothermal := false;
  bUpdateMe := false;

  with sbModified do
  begin
    Panels[0].Text := '';
    Panels[1].Text := '';
    Panels[2].text := '';
    Panels[3].Text := '';
  end;

  tvPrefs.FullExpand;

  for i:=0 to tvPrefs.Items.Count-1 do
  begin
    tvPrefs.Items[i].ImageIndex := 0;
    tvPrefs.Items[i].SelectedIndex := 1;
  end;

// set the colour of the path accordingly
  ebSurferCLRFileChange(nil);
  ebArcInfoCLRFileChange(nil);
  ebDataCLRFileChange(nil);
  ebGeothermalCLRFileChange(nil);

  bLoading := false;
end;
// ----- TformPreferences.UpdatedArcInfoModified -------------------------------
procedure TformPreferences.UpdateModifiedOnly(Sender:TObject);
begin
  SetArcInfoModified;
end;
// ----- TformPreferences.UpdateGeothermalModified -----------------------------
procedure TformPreferences.UpdateGeothermalModified(Sender:TObject);

begin
  SetGeothermalModified;
end;
// ----- TformPreferences.UpdatedModifiedOnly ----------------------------------
procedure TformPreferences.UpdatedModifiedOnly(Sender: TObject);

begin
  SetModified;
end;
// ----- TformPreferences.UpdatedSurferModified --------------------------------
procedure TformPreferences.UpdatedSurferModified(Sender: TObject);

begin
  SetSurferModified;
end;
// ----- TformPreferences.SetArcInfoModified -----------------------------------
procedure TFormPreferences.SetArcInfoModified;
begin
  if not (bLoading) and not bModifiedArcInfo then
  begin
    sbModified.Panels[0].text := 'Modified';
    sbModified.Panels[2].text := 'ArcInfo Modified';
    bModifiedArcInfo := true;
    tvPrefs.Items[15].ImageIndex := 2;
    tvPrefs.Items[15].SelectedIndex := 3;
    tvPrefs.Items[17].ImageIndex := 2;
    tvPrefs.Items[17].SelectedIndex := 3;
    tvPrefs.Invalidate;
  end;
end;
// ----- TformPreferences.SetGeothermalModified --------------------------------
procedure TFormPreferences.SetGeothermalModified;

begin
  if not (bLoading) and not bModifiedArcInfo then
  begin
    sbModified.Panels[0].text := 'Modified';
    sbModified.Panels[3].text := 'Geothermal Modified';
    bModifiedGeothermal := true;
    tvPrefs.Items[18].ImageIndex := 2;
    tvPrefs.Items[18].SelectedIndex := 3;
    tvPrefs.Invalidate;
  end;
end;
// ----- TformPreferences.SetSurferModified ------------------------------------
procedure TFormPreferences.SetSUrferModified;
var
  i:integer;

begin
  if not (bLoading) and not bModifiedSurfer then
  begin
    sbModified.Panels[0].text := 'Modified';
    sbModified.Panels[1].text := 'Surfer Modified';
    bModifiedSurfer := true;
    for i:=15 to 16 do
    begin
      tvPrefs.Items[i].ImageIndex := 2;
      tvPrefs.Items[i].SelectedIndex := 3;
    end;
    tvPrefs.Invalidate;
  end;
end;
// ----- TformPreferences.SetModified ------------------------------------------
procedure TFormPreferences.SetModified;

begin
  if not bLoading then
    sbModified.Panels[0].Text := 'Modified';
end;
// ----- TFormPreferences.IsArcInfoModified ------------------------------------
function TFormPreferences.IsArcInfoModified:boolean;
begin
  result := bModifiedArcInfo;
end;
// ----- TformPreferences.IsGeothermalModified ---------------------------------
function TformPreferences.IsGeothermalModified:boolean;
begin
  result := bModifiedGeothermal;
end;
// ----- TFormPreferences.IsSurferModified -------------------------------------
function TFormPreferences.IsSurferModified:boolean;

begin
  result := bModifiedSurfer;
end;
// ----- TformPreferences.ebDataCLRFileChange ----------------------------------
procedure TformPreferences.ebDataCLRFileChange(Sender: TObject);
begin
  if not FileExists(ebDataCLRFile.Text) then
    ebDataCLRFile.Color := clInfoBk
  else
    ebDataCLRFile.Color := clWindow;
end;
// ----- TformPreferences.pnlSingleVectorColourClick ---------------------------
procedure TformPreferences.pnlSingleVectorColourClick(Sender: TObject);
begin
  UpdateColour(pnlSingleVectorColour,1);
end;
// ----- TformPreferences.pnlMinVectorColourClick ------------------------------
procedure TformPreferences.pnlMinVectorColourClick(Sender: TObject);
begin
  UpdateColour(pnlMinVectorColour,1);
end;
// ----- TformPreferences.pnlMaxVectorColourClick ------------------------------
procedure TformPreferences.pnlMaxVectorColourClick(Sender: TObject);
begin
  UpdateColour(pnlMaxVectorColour,1);
end;
// ----- TformPreferences.bLengthSyncClick -------------------------------------
procedure TformPreferences.bArrowLengthSyncClick(Sender: TObject);

begin
  ///geVectorMinlength.Value := geVectorMaxLength.Value;
end;
// ----- TformPreferences.bArrowRadiusSyncClick --------------------------------
procedure TformPreferences.bArrowRadiusSyncClick(Sender: TObject);

begin
  ///geVectorMinRadius.Value := geVectorMaxRadius.Value;
end;
// ----- TformPreferences.bArrowHeadRadiusSyncClick ----------------------------
procedure TformPreferences.bArrowHeadRadiusSyncClick(Sender: TObject);
begin
  geVectorMinArrowRadius.Value := geVectorMaxArrowRadius.Value;
end;
// ----- TformPreferences.bArrowHeadLengthSyncClick ----------------------------
procedure TformPreferences.bArrowHeadLengthSyncClick(Sender: TObject);
begin
  ///geVectorMinArrowLength.Value := geVectorMaxArrowLength.Value;
end;
// ----- TformPreferences.bLoadVectorDataCLRClick ------------------------------
procedure TformPreferences.bLoadVectorDataCLRClick(Sender: TObject);
begin
  if OpenDialogCLR.Execute then
    ebVectorDataCLRFile.Text := OpenDialogCLR.FileName;
end;
// ----- TformPreferences.ebVectorDataCLRFileChange ----------------------------
procedure TformPreferences.ebVectorDataCLRFileChange(Sender: TObject);
begin
  if not FileExists(ebVectorDataCLRFile.Text) then
    ebVectorDataCLRFile.Color := clInfoBk
  else
    ebVectorDataCLRFile.Color := clWindow;
end;
// ----- TformPreferences.bLoadGeothermalCLRClick ------------------------------
procedure TformPreferences.bLoadGeothermalCLRClick(Sender: TObject);
begin
  if OpenDialogCLR.Execute then
    ebGeothermalCLRFile.Text := OpenDialogCLR.FileName;
end;
// ----- TformPreferences.ebGeothermalCLRFileChange ----------------------------
procedure TformPreferences.ebGeothermalCLRFileChange(Sender: TObject);
begin
  if not FileExists(ebGeothermalCLRFile.Text) then
    ebGeothermalCLRFile.Color := clInfoBk
  else
    ebGeothermalCLRFile.Color := clWindow;
  SetGeothermalModified;
end;
// ----- TformPreferences.pnlGeothermalColourClick -----------------------------
procedure TformPreferences.pnlGeothermalColourClick(Sender: TObject);

begin
  UpdateColour(pnlGeothermalColour,4);
end;
// ----- TformPreferences.pnlGeothermalMinColourClick --------------------------
procedure TformPreferences.pnlGeothermalMinColourClick(Sender: TObject);
begin
  UpdateColour(pnlGeothermalMinColour,4);
end;
// ----- TformPreferences.pnlGeothermalMaxColourClick --------------------------
procedure TformPreferences.pnlGeothermalMaxColourClick(Sender: TObject);
begin
  UpdateColour(pnlGeothermalMaxColour,4);
end;
// ----- TformPreferences.pnlScalarBoundingColourClick -------------------------
procedure TformPreferences.pnlScalarBoundingColourClick(Sender: TObject);
begin
  UpdateColour(pnlScalarBoundingColour,1);
end;
// ----- TformPreferences.pnlVectorBoundingColourClick -------------------------
procedure TformPreferences.pnlVectorBoundingColourClick(Sender: TObject);
begin
  UpdateColour(pnlVectorBoundingColour,1);
end;
// =============================================================================
end.







