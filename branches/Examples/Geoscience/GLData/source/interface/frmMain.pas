{ Unit Name: frmMain
  Author:    HochwimmerA
  Purpose:   Main form of 'gldata'
  Id: frmMain.pas,v 1.80 2004/07/08 09:50:58 hochwimmera Exp
  ------------------------------------------------------------------------------- }
unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.Shellapi, System.UITypes,
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  DB, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.ImgList,
  Vcl.clipbrd, Vcl.ActnCtrls, Vcl.ActnMan, Vcl.ActnMenus, Vcl.ActnList,
  Vcl.ToolWin, Vcl.StdActns, Vcl.Imaging.jpeg, Vcl.XPStyleActnCtrls,
  Vcl.CheckLst, Vcl.ExtDlgs, 

  {** 3rd Party}
  {kbmMemTable,}
  ///GR32_RangeBars,
  {,XDOM_3_1}

   
  GLTexture, GLBitmapFont, GLVectorFileObjects,
  GLWindowsFont, GLHUDObjects, GLObjects, GLGraph, GLScene, GLContext,
  GLWin32Viewer, GLVectorGeometry, GLGraphics, GLVectorTypes,
  GLMesh, GLExtrusion, GLColor, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLBaseClasses,

  {** glData Units}
  cISOSurfaceMC,
  frmsort, geImportFile, geExportFile,
  cColourSpectrum, cContourtypes, cGridImportFn, cStructured3DDataSet,
  cUtilities,
  cUsersSettings, frmAbout, frmPrefs, cGLGridImportFn, frmGeoSimGrid,
  cGLSimulationGRids, frmProcess, geTipofDay, geIntegerEdit, geFloatEdit,
  frmColourTool, GLAVIRecorder, cGLCoordinateAxes, System.ImageList;

type
  TformMain = class(TForm)
    acAbout: TAction;
    acArcInfoAnimation: TAction;
    acArcInfoDefault: TAction;
    acArcInfoDefaultAll: TAction;
    acArcInfoDelete: TAction;
    acArcInfoDeleteAll: TAction;
    acCenterOnPoints: TAction;
    acEmptyOriginal: TAction;
    acEmptyProcessed: TAction;
    acExit: TAction;
    acExport: TAction;
    acExportProcessedClipboard: TAction;
    acExportProcessedCSV: TAction;
    acExportProcessedLaTeX: TAction;
    acExportProcessedSSV: TAction;
    acExportProcessedTSV: TAction;
    acExportProcessedXML: TAction;
    acFocusOnArcInfoGrid: TAction;
    acFocusProcessed: TAction;
    acFocusSurfer: TAction;
    acHelp: TAction;
    acImport: TAction;
    acImportArcInfo: TAction;
    acImportClipboard: TAction;
    acImportCSV: TAction;
    acImportFixedFormat: TAction;
    acImportSSV: TAction;
    acImportSurfer: TAction;
    acImportTSV: TAction;
    acMoveMode: TAction;
    acOpenVTK: TAction;
    acPickMode: TAction;
    acPreferences: TAction;
    acProcess: TAction;
    acRenderBitmap: TAction;
    acRenderBitmap2: TAction;
    acRenderBitmap300: TAction;
    acRenderBitmap600: TAction;
    acSaveGridMeta: TAction;
    acSnapShot: TAction;
    acSortOriginal: TAction;
    acSortProcessed: TAction;
    acSurferAnimation: TAction;
    acSurferDefault: TAction;
    acSurferDefaultAll: TAction;
    acSurferDelete: TAction;
    acSurferDeleteAll: TAction;
    acTipOfDay: TAction;
    acViewMode: TAction;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    ActionTool_Grids: TActionToolBar;
    ActionToolBar_Modes: TActionToolBar;
    ActionToolBar_Original: TActionToolBar;
    ActionToolBar_Processed: TActionToolBar;
    ActionToolBarArcInfo: TActionToolBar;
    ActionToolBarSurfer: TActionToolBar;
    bLoadSurferBaseMap: TButton;
    cbxCutXY: TCheckBox;
    cbxCutXZ: TCheckBox;
    cbCutYZ: TCheckBox;
    cbSurferColourMode: TComboBox;
    cbSurferPolygonMode: TComboBox;
    cbxSurferBaseMap: TCheckBox;
    cbxSurferTwoSided: TCheckBox;
    cbxGridBoundingBox: TCheckBox;
    clbArcInfoGrids: TCheckListBox;
    clbSurferGrids: TCheckListBox;
    dbArcInfoGridInfo: TDBGrid;
    dbgData1: TDBGrid;
    dbgData2: TDBGrid;
    dbSurferGridInfo: TDBGrid;
    dsArcInfoGridInfo: TDataSource;
    dsData1: TDataSource;
    dsData2: TDataSource;
    dsSurferGridInfo: TDataSource;
    ebSurferBaseMap: TEdit;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    GLCamera: TGLCamera;
    GLDArcInfoGrids: TGLDummyCube;
    GLDCubeGrid: TGLDummyCube;
    GLDSurferGrids: TGLDummyCube;
    GLDummyCube: TGLDummyCube;
    GLDummyCubeObjects: TGLDummyCube;
    GLHUDColourScale: TGLHUDSprite;
    GLHUDMovePosition: TGLHUDText;
    GLHUDTextScale: TGLHUDText;
    GLLightSource: TGLLightSource;
    GLLines: TGLLines;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLPipe: TGLPipe;
    GLPoints: TGLPoints;
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLSphere1: TGLSphere;
    GLWindowsBitmapFont: TGLWindowsBitmapFont;
    glBoundingBoxVector: TGLXYZGrid;
    ImageList: TImageList;
    lblSurferAlpha: TLabel;
    lblSurferPolygonMode: TLabel;
    lblTileSurferX: TLabel;
    lblTileSurferY: TLabel;
    LightCube: TGLDummyCube;
    memGridDetails: TMemo;
    OpenArcInfo: TOpenDialog;
    OpenDialogGridMeta: TOpenDialog;
    OpenSurfer: TOpenDialog;
    OpenVTK: TOpenDialog;
    pcArcInfoDetails: TPageControl;
    pcMain: TPageControl;
    pcSurferDetails: TPageControl;
    pnlData: TPanel;
    pnlGLScene: TPanel;
    pnlNoArcInfo: TPanel;
    pnlNoSurfer: TPanel;
    SaveDialog: TSaveDialog;
    SaveDialogGridMeta: TSaveDialog;
    sbCamera: TStatusBar;
    sbFocus: TStatusBar;
    splitterArcInfo: TSplitter;
    splittermain: TSplitter;
    splitterSurfer: TSplitter;
    TimerAnimationSurfer: TTimer;
    tsArcInfo: TTabSheet;
    tsArcInfoGridInfo: TTabSheet;
    tsNoArcInfo: TTabSheet;
    tsNoSurfer: TTabSheet;
    tsOriginalData: TTabSheet;
    tsProcessed: TTabSheet;
    tsStructureGrids: TTabSheet;
    tsSurfer: TTabSheet;
    tsSurferDisplay: TTabSheet;
    tsSurferGridInfo: TTabSheet;
    tsArcInfoDisplay: TTabSheet;
    cbArcInfoColourMode: TComboBox;
    lblArcInfoAlpha: TLabel;
    lblArcInfoPolygonMode: TLabel;
    cbArcInfoPolygonMode: TComboBox;
    cbxArcInfoTwoSided: TCheckBox;
    bLoadArcInfoBaseMap: TButton;
    ebArcInfoBaseMap: TEdit;
    cbxArcInfoBaseMap: TCheckBox;
    lblArcInfoTileX: TLabel;
    lblArcInfoTileY: TLabel;
    GLDVectorData: TGLDummyCube;
    tsGeothermal: TTabSheet;
    ActionToolBarGeothermal: TActionToolBar;
    acGeothermalGrid: TAction;
    GLDGeoSim: TGLDummyCube;
    pnlGeoTop: TPanel;
    dsGeothermalLayers: TDataSource;
    cbDisplay: TComboBox;
    dbgGeothermalLayers: TDBGrid;
    cbxGeoAutoScale: TCheckBox;
    ebgeoMinValue: TEdit;
    ebgeoMaxValue: TEdit;
    acSortGeoLayers: TAction;
    bGeoSyncLimits: TButton;
    acColourTool: TAction;
    sbOriginal: TStatusBar;
    sbProcessed: TStatusBar;
    acBenchmark: TAction;
    glsLocator: TGLSphere;
    acOpenGLContext: TAction;
    glBoundingBox: TGLXYZGrid;
    acBoundingBox: TAction;
    acBoundingBoxVector: TAction;
    lblSurferGridName: TLabel;
    ebSurferGridName: TEdit;
    lblArcInfoGridName: TLabel;
    ebArcInfoGridName: TEdit;
    GLDAxes: TGLDummyCube;
    AxesFont: TGLWindowsBitmapFont;
    Image1: TImage;
    acLocator: TAction;
    acAxes: TAction;
    GLLightSourceReverse: TGLLightSource;
    acWorldView: TAction;
    acGeothermalTETRAD: TAction;
    PageControl1: TPageControl;
    tsRowsCols: TTabSheet;
    clbColumns: TCheckListBox;
    clbRows: TCheckListBox;
    tsIsoSurfaces: TTabSheet;
    tbIso: TTrackBar;
    tsBlockSettings: TTabSheet;
    cbxInActiveBlocks: TCheckBox;
    GLAVIRecorder: TGLAVIRecorder;
    procedure acAboutExecute(Sender: TObject);
    procedure acArcInfoDeleteAllUpdate(Sender: TObject);
    procedure acArcInfoDeleteAllExecute(Sender: TObject);
    procedure acArcInfoDeleteExecute(Sender: TObject);
    procedure acArcInfoDeleteUpdate(Sender: TObject);
    procedure acCenterOnPointsExecute(Sender: TObject);
    procedure acEmptyOriginalExecute(Sender: TObject);
    procedure acEmptyOriginalUpdate(Sender: TObject);
    procedure acEmptyProcessedExecute(Sender: TObject);
    procedure acEmptyProcessedUpdate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acExportHint(var HintStr: String; var CanShow: Boolean);
    procedure acExportProcessedClipboardExecute(Sender: TObject);
    procedure acExportProcessedClipboardUpdate(Sender: TObject);
    procedure acExportProcessedCSVExecute(Sender: TObject);
    procedure acExportProcessedCSVUpdate(Sender: TObject);
    procedure acExportProcessedLaTeXExecute(Sender: TObject);
    procedure acExportProcessedLaTeXUpdate(Sender: TObject);
    procedure acExportProcessedSSVExecute(Sender: TObject);
    procedure acExportProcessedSSVUpdate(Sender: TObject);
    procedure acExportProcessedTSVExecute(Sender: TObject);
    procedure acExportProcessedTSVUpdate(Sender: TObject);
    procedure acExportUpdate(Sender: TObject);
    procedure acExportProcessedXMLExecute(Sender: TObject);
    procedure acExportProcessedXMLUpdate(Sender: TObject);
    procedure acFocusOnArcInfoGridExecute(Sender: TObject);
    procedure acFocusOnArcInfoGridUpdate(Sender: TObject);
    procedure acFocusProcessedExecute(Sender: TObject);
    procedure acFocusProcessedUpdate(Sender: TObject);
    procedure acFocusSurferExecute(Sender: TObject);
    procedure acFocusSurferUpdate(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acHelpUpdate(Sender: TObject);
    procedure acImportArcInfoExecute(Sender: TObject);
    procedure acImportClipboardExecute(Sender: TObject);
    procedure acImportClipboardUpdate(Sender: TObject);
    procedure acImportCSVExecute(Sender: TObject);
    procedure acImportCSVUpdate(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acImportFixedFormatExecute(Sender: TObject);
    procedure acImportFixedFormatUpdate(Sender: TObject);
    procedure acImportHint(var HintStr: String; var CanShow: Boolean);
    procedure acImportSSVExecute(Sender: TObject);
    procedure acImportSSVUpdate(Sender: TObject);
    procedure acImportSurferExecute(Sender: TObject);
    procedure acImportTSVExecute(Sender: TObject);
    procedure acImportTSVUpdate(Sender: TObject);
    procedure acLoadGridMetaExecute(Sender: TObject);
    procedure acMoveModeExecute(Sender: TObject);
    procedure acOpenVTKExecute(Sender: TObject);
    procedure acPickModeExecute(Sender: TObject);
    procedure acPreferencesExecute(Sender: TObject);
    procedure acProcessExecute(Sender: TObject);
    procedure acProcessUpdate(Sender: TObject);
    procedure acRenderBitmapExecute(Sender: TObject);
    procedure acRenderBitmap2Execute(Sender: TObject);
    procedure acRenderBitmap300Execute(Sender: TObject);
    procedure acRenderBitmap600Execute(Sender: TObject);
    procedure acSaveGridMetaExecute(Sender: TObject);
    procedure acSaveGridMetaUpdate(Sender: TObject);
    procedure acSnapShotExecute(Sender: TObject);
    procedure acSortOriginalExecute(Sender: TObject);
    procedure acSortOriginalUpdate(Sender: TObject);
    procedure acSortProcessedExecute(Sender: TObject);
    procedure acSortProcessedUpdate(Sender: TObject);
    procedure acSurferAnimationExecute(Sender: TObject);
    procedure acSurferAnimationUpdate(Sender: TObject);
    procedure acSurferDefaultExecute(Sender: TObject);
    procedure acSurferDefaultUpdate(Sender: TObject);
    procedure acSurferDefaultAllUpdate(Sender: TObject);
    procedure acSurferDefaultAllExecute(Sender: TObject);
    procedure acSurferDeleteExecute(Sender: TObject);
    procedure acSurferDeleteUpdate(Sender: TObject);
    procedure acSurferDeleteAllExecute(Sender: TObject);
    procedure acSurferDeleteAllUpdate(Sender: TObject);
    procedure acTipOfDayExecute(Sender: TObject);
    procedure acViewModeExecute(Sender: TObject);
    procedure cbSurferColourModeChange(Sender: TObject);
    procedure cbSurferPolygonModeChange(Sender: TObject);
    procedure cbxCutXYClick(Sender: TObject);
    procedure cbxCutXZClick(Sender: TObject);
    procedure cbxGridBoundingBoxClick(Sender: TObject);
    procedure cbxSurferBaseMapClick(Sender: TObject);
    procedure cbxSurferTwoSidedClick(Sender: TObject);
    procedure clbArcInfoGridsClick(Sender: TObject);
    procedure clbSurferGridsClick(Sender: TObject);
    procedure clbSurferGridsClickCheck(Sender: TObject);
    procedure dbgData2DblClick(Sender: TObject);
    procedure ebSurferBaseMapChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure gbCutXYChange(Sender: TObject);
    procedure gbCutXZChange(Sender: TObject);
    procedure gbCutYZChange(Sender: TObject);
    procedure GEImportFileAfterImport(Sender: TObject);
    procedure GLSceneViewerMouseEnter(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure kbmData2AfterDelete(DataSet: TDataSet);
    procedure kbmData2AfterInsert(DataSet: TDataSet);
    procedure kbmData2AfterPost(DataSet: TDataSet);
    procedure pcSurferDetailsChange(Sender: TObject);
    procedure TimerAnimationSurferTimer(Sender: TObject);
    procedure cbArcInfoColourModeChange(Sender: TObject);
    procedure cbArcInfoPolygonModeChange(Sender: TObject);
    procedure cbxArcInfoTwoSidedClick(Sender: TObject);
    procedure bLoadArcInfoBaseMapClick(Sender: TObject);
    procedure bLoadSurferBaseMapClick(Sender: TObject);
    procedure ebArcInfoBaseMapChange(Sender: TObject);
    procedure cbxArcInfoBaseMapClick(Sender: TObject);
    procedure clbArcInfoGridsClickCheck(Sender: TObject);
    procedure acArcInfoDefaultExecute(Sender: TObject);
    procedure acArcInfoDefaultUpdate(Sender: TObject);
    procedure acArcInfoDefaultAllExecute(Sender: TObject);
    procedure acArcInfoDefaultAllUpdate(Sender: TObject);
    procedure acGeothermalGridExecute(Sender: TObject);
    procedure cbDisplayChange(Sender: TObject);
    procedure kbmGeothermalLayersAfterPost(DataSet: TDataSet);
    procedure dbgGeothermalLayersDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure cbxGeoAutoScaleClick(Sender: TObject);
    procedure ebgeoMinValueExit(Sender: TObject);
    procedure ebgeoMaxValueExit(Sender: TObject);
    procedure acSortGeoLayersUpdate(Sender: TObject);
    procedure acSortGeoLayersExecute(Sender: TObject);
    procedure bGeoSyncLimitsClick(Sender: TObject);
    procedure geTileSurferXExit(Sender: TObject);
    procedure geTileSurferYExit(Sender: TObject);
    procedure geTileArcInfoXExit(Sender: TObject);
    procedure geTileArcInfoYExit(Sender: TObject);
    procedure geSurferAlphaExit(Sender: TObject);
    procedure geArcInfoAlphaExit(Sender: TObject);
    procedure acColourToolExecute(Sender: TObject);
    procedure kbmData1AfterInsert(DataSet: TDataSet);
    procedure kbmData1AfterDelete(DataSet: TDataSet);
    procedure kbmData1AfterPost(DataSet: TDataSet);
    procedure acBenchmarkExecute(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure acImportSurferUpdate(Sender: TObject);
    procedure acArcInfoAnimationExecute(Sender: TObject);
    procedure acArcInfoAnimationUpdate(Sender: TObject);
    procedure acOpenGLContextExecute(Sender: TObject);
    procedure acBoundingBoxExecute(Sender: TObject);
    procedure acBoundingBoxVectorExecute(Sender: TObject);
    procedure ebSurferGridNameExit(Sender: TObject);
    procedure ebArcInfoGridNameExit(Sender: TObject);
    procedure acBenchmarkUpdate(Sender: TObject);
    procedure acLocatorUpdate(Sender: TObject);
    procedure acLocatorExecute(Sender: TObject);
    procedure acAxesExecute(Sender: TObject);
    procedure acAxesUpdate(Sender: TObject);
    /// procedure DomImplementationRequestXPathVariable(
    /// const sender: TXPathExpression; const namespaceURI,
    /// localName: WideString; var value: TdomXPathCustomResult);
    procedure acWorldViewUpdate(Sender: TObject);
    procedure acWorldViewExecute(Sender: TObject);
    procedure acGeothermalTETRADExecute(Sender: TObject);
    procedure cbxInActiveBlocksClick(Sender: TObject);
    procedure clbColumnsClickCheck(Sender: TObject);
    procedure clbRowsClickCheck(Sender: TObject);
    procedure geISOExit(Sender: TObject);
    procedure tbIsoChange(Sender: TObject);
  private
    iArcInfoIndex, iSurferIndex, mdx, mdy: Integer;
    Contourbm, Vectorbm: array of TBookmark;

    VectorField: TVectorField;
    Grid3d: T3DStructuredDataSet;

    moveproxy: TGLProxyObject;
    LastMouseWorldPos: TVector;
    bMoveOnZ: Boolean;
    bLoadingGeo: Boolean;
    procedure LoadAxesProperties;
    procedure ApplyUserSettingsToObjects;
    procedure DrawConnectionLines;
    procedure DrawGLPipe;
    procedure DrawGLPoints;
    procedure GenerateHUDColourScale;
    procedure GenerateVectorColourScale;
    procedure GenerateGeoColourScale;
    procedure GenerateSurferColourScale;
    procedure GenerateArcInfocolourScale;
    procedure GenerateLocalYCameraTranslation(dx: Integer; dy: Integer;
      zoomcoeff: double; var v: TVector);
    procedure GenerateLocalZCameraTranslation(dx: Integer; dy: Integer;
      zoomcoeff: double; var v: TVector);
    function HasHelp: Boolean;
    function MouseWorldPos(X, Y: Integer): TVector;
    procedure ProcessSurferGrid(sFileName: string; bLoad: Boolean;
      dBlankSub: double);
    procedure ProcessArcInfoGrid(sFileName: string; bLoad: Boolean;
      dBlankSub: double);
    procedure RenderHUDScale(sID: string; aPal: TColourSpectrum);
    procedure RenderProcessed;
    procedure RenderToBitMap(scale: single);
    procedure LoadGridMetaXML(sMetaFile: string);
    procedure SaveGridMetaXML;
    procedure ShowHelp;
    procedure UpdateCameraFocusPositions;
    procedure UpdateNewPosition;

    procedure UpdateScaleHUD;
    procedure UpdateStatusOriginal;
    procedure UpdateStatusProcessed;

    procedure UpdateSurferBounds;
    procedure UpdateArcInfoGrid(iIndex: Integer);
    procedure UpdateArcInfoScale(iIndex: Integer);
    procedure UpdateSurferGrid(iIndex: Integer);
    procedure UpdateSurferScale(iIndex: Integer);
  public
    Geosimgrid: TGLSimulationgrid;

    contourpoints: TContourPointCollection;
    usersettings: TGLDataUserSettings;
    Axes: TGLCoordinateAxes;
    bProcessing: Boolean;
    procedure TransformCamera;
  end;

var
  formMain: TformMain;

implementation

uses
  {cXML,} frmBenchmark, frmOpenGL, {frmLocator,} frmAxes, frmWorld, frmBlock;

{$R *.dfm}

// ----- TformMain.TransformCamera ---------------------------------------------
procedure TformMain.TransformCamera;

begin
  GLCamera.TransformationChanged;
  UpdateCameraFocusPositions;
end;

// ----- TformMain.acAboutExecute ----------------------------------------------
procedure TformMain.acAboutExecute(Sender: TObject);
var
  about: TformAbout;

begin
  about := TformAbout.Create(nil);
  about.ShowModal;
  about.Release;
end;

// ----- TformMain.acCenterOnPointsExecute -------------------------------------
procedure TformMain.acCenterOnPointsExecute(Sender: TObject);

var
  dleft, dright, dBack, dForward, dBottom, dtop: double;
  v: TAffineVector;

begin
  if contourpoints.Count > 0 then
  begin
    contourpoints.CalcBounds(dleft, dright, dBack, dForward, dBottom, dtop);
    MakeVector(v, usersettings.ScaleX * 0.5 * (dleft + dright),
      usersettings.ScaleY * 0.5 * (dBack + dForward), usersettings.ScaleZ * 0.5
      * (dBottom + dtop));
  end
  else
    v := NullVector;

  GLDummyCube.Position.AsAffineVector := v;

  GLCamera.TransformationChanged;
  UpdateCameraFocusPositions;
end;

// ----- TformMain.acEmptyOriginalExecute --------------------------------------
procedure TformMain.acEmptyOriginalExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to empty the original data table?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    /// kbmData1.EmptyTable;
end;

// ----- TformMain.acEmptyOriginalUpdate ---------------------------------------
procedure TformMain.acEmptyOriginalUpdate(Sender: TObject);

begin
  /// acEmptyOriginal.Enabled := (kbmData1.RecordCount > 0);
end;

// ----- TformMain.acEmptyProcessedExecute -------------------------------------
procedure TformMain.acEmptyProcessedExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to empty the processed data table?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    /// kbmData2.EmptyTable;
    RenderProcessed;
end;

// ----- TformMain.acEmptyProcessedUpdate --------------------------------------
procedure TformMain.acEmptyProcessedUpdate(Sender: TObject);

begin
  /// acEmptyProcessed.Enabled := kbmData2.RecordCount > 0;
end;

// ----- TformMain.acExitExecute -----------------------------------------------
procedure TformMain.acExitExecute(Sender: TObject);

begin
  Close;
end;

// ----- TformMain.acExportExecute ---------------------------------------------
procedure TformMain.acExportExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportHint ------------------------------------------------
procedure TformMain.acExportHint(var HintStr: String; var CanShow: Boolean);
begin
  /// HintStr := GEExportFile.Hint;
  CanShow := true;
end;

// ----- TformMain.acExportProcessedClipboardExecute ---------------------------
procedure TformMain.acExportProcessedClipboardExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efClipboard;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedClipboardUpdate ----------------------------
procedure TformMain.acExportProcessedClipboardUpdate(Sender: TObject);

begin
  /// acExportProcessedClipboard.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedClipboard.Checked := (GEExportFIle.ExportFileType = efClipboard);
end;

// ----- TformMain.acExportProcessedCSVExecute ---------------------------------
procedure TformMain.acExportProcessedCSVExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efCSV;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedCSVUpdate ----------------------------------
procedure TformMain.acExportProcessedCSVUpdate(Sender: TObject);
begin
  /// acExportProcessedCSV.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedCSV.Checked := (GEExportFIle.ExportFileType = efCSV);
end;

// ----- TformMain.acExportProcessedLaTeXExecute -------------------------------
procedure TformMain.acExportProcessedLaTeXExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efLaTeX;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedLaTeXUpdate --------------------------------
procedure TformMain.acExportProcessedLaTeXUpdate(Sender: TObject);

begin
  /// acExportProcessedLaTex.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedLaTex.Checked := (GEExportFIle.ExportFileType = efLaTeX);
end;

// ----- TformMain.acExportProcessedSSVExecute ---------------------------------
procedure TformMain.acExportProcessedSSVExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efSSV;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedSSVUpdate ----------------------------------
procedure TformMain.acExportProcessedSSVUpdate(Sender: TObject);

begin
  /// acExportProcessedSSV.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedSSV.Checked := (GEExportFIle.ExportFileType = efSSV);
end;

// ----- TformMain.acExportProcessedTSVExecute ---------------------------------
procedure TformMain.acExportProcessedTSVExecute(Sender: TObject);
begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efTSV;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedTSVUpdate ----------------------------------
procedure TformMain.acExportProcessedTSVUpdate(Sender: TObject);
begin
  /// acExportProcessedTSV.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedTSV.Checked := (GEExportFIle.ExportFileType = efTSV);
end;

// ----- TformMain.acExportProcessedXMLExecute ---------------------------------
procedure TformMain.acExportProcessedXMLExecute(Sender: TObject);

begin
  /// GEExportFile.Identifier := 'processed';
  /// GEExportFile.ExportFileType := efXML;
  /// GEExportFile.Source := kbmData2;
  /// GEExportFile.Execute;
end;

// ----- TformMain.acExportProcessedXMLUpdate ----------------------------------
procedure TformMain.acExportProcessedXMLUpdate(Sender: TObject);

begin
  /// acExportProcessedXML.Enabled := (kbmData2.RecordCount > 0);
  /// acExportProcessedXML.Checked := (GEExportFIle.ExportFileType = efXML);
end;

// ----- TformMain.acExportUpdate ----------------------------------------------
procedure TformMain.acExportUpdate(Sender: TObject);
begin
  /// acExport.Enabled := (kbmData2.RecordCount > 0);
end;

// ----- TformMain.acFocusOnArcInfoGridExecute ---------------------------------
procedure TformMain.acFocusOnArcInfoGridExecute(Sender: TObject);

var
  dOffSet: double;

begin
  with TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]) do
  begin
    // center the focus cube on the surfergrid minpoint
    GLDummyCube.Position.X := grid.Position.X + 0.5 *
      usersettings.ScaleX * xrange;
    GLDummyCube.Position.Y := grid.Position.Y + 0.5 *
      usersettings.ScaleY * yrange;
    GLDummyCube.Position.z := grid.Position.z + 0.5 *
      usersettings.ScaleZ * zrange;
    dOffSet := usersettings.ScaleX * xrange;

    if (usersettings.ScaleY * yrange) > dOffSet then
      dOffSet := usersettings.ScaleY * yrange;

    if (usersettings.ScaleZ * zrange) > dOffSet then
      dOffSet := usersettings.ScaleZ * zrange;

    GLCamera.Position.z := GLDummyCube.Position.z + dOffSet;
    // this could be cleaned up and made user customisable
    GLCamera.DepthOfView := 5 * dOffSet;
    GLCamera.TransformationChanged;
    UpdateCameraFocusPositions;
  end;
end;

// ----- TformMain.acFocusOnArcInfoGridUpdate ----------------------------------
procedure TformMain.acFocusOnArcInfoGridUpdate(Sender: TObject);

begin
  acFocusOnArcInfoGrid.Enabled := (clbArcInfoGrids.ItemIndex <> -1);
end;

// ----- TformMain.acFocusProcessedExecute -------------------------------------
procedure TformMain.acFocusProcessedExecute(Sender: TObject);

begin
  // set the focus cube to the object - this will work for either scalar or
  // vector data.
  /// GLDummyCube.Position.X := usersettings.ScaleX*kbmData2.FieldByName('x').AsFloat;
  /// GLDummyCube.Position.y := usersettings.ScaleY*kbmData2.FieldByName('y').AsFloat;
  /// GLDummyCube.Position.z := usersettings.ScaleZ*kbmData2.FieldByName('z').AsFloat;
  /// GLCamera.TransformationChanged;
  /// UpdateCameraFocusPositions;
end;

// ----- TformMain.acFocusProcessedUpdate --------------------------------------
procedure TformMain.acFocusProcessedUpdate(Sender: TObject);
begin
  /// acFocusProcessed.Enabled := ((kbmData2.RecNo >= 1) and
  /// (kbmData2.RecNo <= kbmData2.RecordCount))
end;

// ----- TformMain.acHelpExecute -----------------------------------------------
procedure TformMain.acHelpExecute(Sender: TObject);

begin
  ShowHelp;
end;

// ----- TformMain.acHelpUpdate ------------------------------------------------
procedure TformMain.acHelpUpdate(Sender: TObject);

begin
  acHelp.Enabled := HasHelp;
end;

// ----- TformMain.acImportClipboardExecute ------------------------------------
procedure TformMain.acImportClipboardExecute(Sender: TObject);

begin
  /// GEImportFile.ImportFileType := ifClipboard;
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportClipboardUpdate -------------------------------------
procedure TformMain.acImportClipboardUpdate(Sender: TObject);
begin
  /// acImportClipboard.Checked := (GEImportFile.ImportFileType = ifClipboard);
end;

// ----- TformMain.acImportCSVExecute ------------------------------------------
procedure TformMain.acImportCSVExecute(Sender: TObject);

begin
  /// GEImportFile.ImportFileType := ifCSV;
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportCSVUpdate -------------------------------------------
procedure TformMain.acImportCSVUpdate(Sender: TObject);
begin
  /// acImportCSV.Checked := (GEImportFile.ImportFileType = ifCSV);
end;

// ----- TformMain.acImportExecute ---------------------------------------------
// import the default file type
procedure TformMain.acImportExecute(Sender: TObject);

begin
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportFixedFormatExecute ----------------------------------
procedure TformMain.acImportFixedFormatExecute(Sender: TObject);
begin
  /// GEImportFile.ImportFileType := ifFixedFormatSpace;
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportFixedFormatUpdate -----------------------------------
procedure TformMain.acImportFixedFormatUpdate(Sender: TObject);
begin
  /// acImportFixedFormat.Checked := (GEImportFile.ImportFileType =
  /// ifFixedFormatSpace);
end;

// ----- TformMain.acImportHint ------------------------------------------------
procedure TformMain.acImportHint(var HintStr: String; var CanShow: Boolean);
begin
  /// HintStr := GEImportFile.Hint;
  /// CanShow := true;
end;

// ----- TformMain.acImportSSVExecute ------------------------------------------
procedure TformMain.acImportSSVExecute(Sender: TObject);

begin
  /// GEImportFile.ImportFileType := ifSSV;
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportSSVUpdate -------------------------------------------
procedure TformMain.acImportSSVUpdate(Sender: TObject);

begin
  /// acImportSSV.Checked := (GEImportFile.ImportFileType = ifSSV);
end;

// ----- TformMain.acImportSurferExecute ---------------------------------------
procedure TformMain.acImportSurferExecute(Sender: TObject);

begin
  if OpenSurfer.Execute then
  begin
    Application.ProcessMessages;
    ProcessSurferGrid(OpenSurfer.FileName, false, 0.0);
    TGLContourGridData(clbSurferGrids.Items.Objects[clbSurferGrids.Count - 1])
      .Visible := usersettings.SurferGrid.CreateVisible;
    clbSurferGrids.Checked[clbSurferGrids.Count - 1] :=
      usersettings.SurferGrid.CreateVisible;
    Application.ProcessMessages;
    clbSurferGridsClick(nil);
    UpdateSurferBounds;
  end;
end;

// ----- TformMain.ProcessSurferGrid ------------------------------------------
procedure TformMain.ProcessSurferGrid(sFileName: string; bLoad: Boolean;
  dBlankSub: double);

var
  bOK: Boolean;
  GLGrid: TGLContourGridData;

begin
  GLGrid := TGLContourGridData.Create(GLDSurferGrids);
  GLGrid.GridOptions := usersettings.SurferGrid;
  if bLoad then
  begin
    if usersettings.SurferGrid.SilentLoad then
      GLGrid.SilentImport(sFileName, dBlankSub, 0, bOK)
    else
      GLGrid.Import(sFileName, dBlankSub, 0, bOK);
  end
  else
  begin
    if usersettings.SurferGrid.SilentImport then
      GLGrid.SilentImport(sFileName, dBlankSub, 0, bOK)
    else
      GLGrid.Import(sFileName, dBlankSub, 0, bOK);
  end;

  if bOK then
  begin
    clbSurferGrids.Items.AddObject(sFileName, GLGrid);

    // option to render grid by default
    GLGrid.Visible := false;

    clbSurferGrids.ItemIndex := clbSurferGrids.Count - 1;

    UpdateSurferGrid(clbSurferGrids.Count - 1);
    UpdateSurferScale(clbSurferGrids.Count - 1);

    if usersettings.AutoFocusGrids then
      acFocusSurfer.Execute;

    if (usersettings.ColourScaleOptions.ColourScaleType = 2) then
      GenerateSurferColourScale;
    usersettings.SurferGrid.DefaultDir := ExtractFilePath(sFileName);
  end
  else
    GLGrid.Free;
end;

// ----- TformMain.ProcessArcInfoGrid ------------------------------------------
procedure TformMain.ProcessArcInfoGrid(sFileName: string; bLoad: Boolean;
  dBlankSub: double);

var
  bOK: Boolean;
  GLGrid: TGLContourGridData;

begin
  GLGrid := TGLContourGridData.Create(GLDArcInfoGrids);
  GLGrid.GridOptions := usersettings.ArcInfoGrid;

  if bLoad then
  begin
    if usersettings.ArcInfoGrid.SilentLoad then
      GLGrid.SilentImport(sFileName, dBlankSub, 1, bOK)
    else
      GLGrid.Import(sFileName, dBlankSub, 1, bOK);
  end
  else
  begin
    if usersettings.ArcInfoGrid.SilentImport then
      GLGrid.SilentImport(sFileName, dBlankSub, 1, bOK)
    else
      GLGrid.Import(sFileName, dBlankSub, 1, bOK);
  end;

  if bOK then
  begin
    clbArcInfoGrids.Items.AddObject(sFileName, GLGrid);
    clbArcInfoGrids.Checked[clbArcInfoGrids.Count - 1] := GLGrid.Visible;
    UpdateArcInfoGrid(clbArcInfoGrids.Count - 1);
    UpdateArcInfoScale(clbArcInfoGrids.Count - 1);

    clbArcInfoGrids.ItemIndex := clbArcInfoGrids.Count - 1;
    clbArcInfoGridsClick(nil);
    if usersettings.AutoFocusGrids then
      acFocusOnArcInfoGrid.Execute;

    if (usersettings.ColourScaleOptions.ColourScaleType = 3) then
      GenerateArcInfocolourScale;
    usersettings.ArcInfoGrid.DefaultDir := ExtractFilePath(sFileName);
  end
  else
    GLGrid.Free;
end;

// ----- TformMain.RenderHUDScale ----------------------------------------------
procedure TformMain.RenderHUDScale(sID: string; aPal: TColourSpectrum);

begin
  // general from here - make a procedure passing in the sID name
  with GLHUDColourScale do
  begin
    Visible := usersettings.ColourScaleOptions.Visible;
    Width := aPal.Scalepalette.Width;
    Height := aPal.Scalepalette.Height;
    Position.Y := 0.5 * aPal.Scalepalette.Height + 40;
    Position.X := 40;
  end;

  if Assigned(GLMaterialLibrary.LibMaterialByName(sID)) then
  begin
    GLMaterialLibrary.LibMaterialByName(sID).Material.Texture.Image.Assign
      (aPal.Scalepalette);
  end
  else
    GLMaterialLibrary.AddTextureMaterial(sID, aPal.Scalepalette);

  with GLMaterialLibrary.LibMaterialByName(sID).Material do
  begin
    MaterialOptions := MaterialOptions + [moNoLighting];
    FrontProperties.Diffuse.AsWinColor := GLSceneViewer.Buffer.BackgroundColor;
    FrontProperties.Diffuse.Alpha := usersettings.ColourScaleOptions.Alpha;
    BlendingMode := bmTransparency;
  end;
  GLHUDColourScale.Material.LibMaterialName := sID;
  GLHUDColourScale.StructureChanged;
end;

// ----- TformMain.acImportTSVExecute ------------------------------------------
procedure TformMain.acImportTSVExecute(Sender: TObject);

begin
  /// GEImportFile.ImportFileType := ifTSV;
  /// GEImportFile.Execute;
end;

// ----- TformMain.acImportTSVUpdate -------------------------------------------
procedure TformMain.acImportTSVUpdate(Sender: TObject);

begin
  /// acImportTSV.Checked := (GEImportFile.ImportFileType = ifTSV);
end;

// ----- TformMain.acMoveModeExecute -------------------------------------------
procedure TformMain.acMoveModeExecute(Sender: TObject);
begin
  acMoveMode.Checked := not acMoveMode.Checked;
  acViewMode.Checked := not acMoveMode.Checked;
  acPickMode.Checked := not acMoveMode.Checked;
  GLSceneViewer.Cursor := crDrag;
end;

// ----- TformMain.acOpenVTKExecute --------------------------------------------
procedure TformMain.acOpenVTKExecute(Sender: TObject);

begin
  if OpenVTK.Execute then
  begin
    Grid3d.ImportVTKFile(OpenVTK.FileName);
    with memGridDetails do
    begin
      Lines.Clear;
      Lines.Add('Grid imported: ' + DateTimeToStr(Now));
      Lines.Add('File: ' + OpenVTK.FileName);
      Lines.Add(Grid3d.Description);
      Lines.Add('NX: ' + IntToStr(Grid3d.NX));
      Lines.Add('NY: ' + IntToStr(Grid3d.NY));
      Lines.Add('NZ: ' + IntToStr(Grid3d.NZ));
      Lines.Add(' ');
      Lines.Add('Xlo: ' + FloatToStr(Grid3d.xlo));
      Lines.Add('Ylo: ' + FloatToStr(Grid3d.ylo));
      Lines.Add('Zlo: ' + FloatToStr(Grid3d.zlo));
      Lines.Add('Xhi: ' + FloatToStr(Grid3d.xhi));
      Lines.Add('Yhi: ' + FloatToStr(Grid3d.yhi));
      Lines.Add('Zhi: ' + FloatToStr(Grid3d.zhi));
      Lines.Add('X delta: ' + FloatToStr(Grid3d.xDelta));
      Lines.Add('Y delta: ' + FloatToStr(Grid3d.yDelta));
      Lines.Add('Z delta: ' + FloatToStr(Grid3d.zDelta));

      (*
        gbCutXY.Min := 0;
        gbCutXY.Max := grid3d.NZ;
        gbCutXY.Position := 0;

        gbCutXZ.Min := 0;
        gbCutXZ.Max := grid3d.NY;
        gbCutXZ.Position := 0;

        gbCutYZ.Min := 0;
        gbCutYZ.Max := grid3d.NX;
        gbCutYZ.Position := 0;
      *)
    end;
  end;
end;

// ----- TformMain.acPickModeExecute -------------------------------------------
procedure TformMain.acPickModeExecute(Sender: TObject);
begin
  acPickMode.Checked := not acPickMode.Checked;
  acViewMode.Checked := not acPickMode.Checked;
  acMoveMode.Checked := not acPickMode.Checked;
  GLSceneViewer.Cursor := crHandPoint;
end;

// ----- TformMain.acPreferencesExecute ----------------------------------------
procedure TformMain.acPreferencesExecute(Sender: TObject);

var
  pf: TFormPreferences;

begin
  pf := TFormPreferences.Create(nil);
  pf.bContinue := true;
  pf.bLoading := true;

  with usersettings do
  begin
    // general
    pf.cbxAutoProcess.Checked := AutoProcess;
    // interface
    pf.cbxInvertMouseWheel.Checked := InvertMouseWheel;
    pf.cbxFocusStatusBar.Checked := FocusStatus;
    pf.cbxCameraStatusBar.Checked := CameraStatus;
    // display
    pf.pnlBackgroundColour.Color := BackGroundColour;
    pf.cbxTwoSideLighting.Checked := TwoSideLighting;
    pf.cbAntialiasing.ItemIndex := AntiAliasing;
    pf.cbShadeModel.ItemIndex := ShadeModel;
    // display/axes
    pf.cbxDisplayAxes.Checked := DisplayAxes;
    // display/camera
    pf.cbCameraStyle.ItemIndex := CameraStyle;
    pf.geFocalLength.Value := CameraFocalLength;
    pf.geDepthOfView.Value := CameraDepthOfView;
    pf.geCameraNearPlaneBias.Value := CameraNearPlaneBias;

    // display/hud
    pf.cbxShowColourScale.Checked := ColourScaleOptions.Visible;
    pf.cbxColourScaleContinous.Checked := ColourScaleOptions.Continuous;
    pf.cbxColourScaleBorder.Checked := ColourScaleOptions.Border;
    pf.cbxColourScaleShowContours.Checked :=
      ColourScaleOptions.ShowContourPoints;
    pf.geColourScaleAlpha.Value := ColourScaleOptions.Alpha;
    pf.cbColourScaleType.ItemIndex := ColourScaleOptions.ColourScaleType;
    pf.ebScaleLabelFormat.Text := ColourScaleOptions.Scaleformat;
    pf.cbxHUDScaleSteps.Checked := ColourScaleOptions.ScaleSteps;
    pf.geHUDPoints.Value := ColourScaleOptions.Points;

    // display/lighting
    pf.cbxLightingShining.Checked := Lighting;
    pf.cbxLighting2Shining.Checked := Lighting2;

    // display/scale
    pf.geScaleX.Value := ScaleX;
    pf.geScaleY.Value := ScaleY;
    pf.geScaleZ.Value := ScaleZ;

    pf.cbxShowHUDScale.Checked := ScaleHUD;

    // data - points
    pf.cbxRenderNullPoints.Checked := RenderNullPoints;

    // data/markers
    pf.cbxScalarAA.Checked := MarkerBoxLineAA;
    pf.pnlScalarBoundingColour.Color := MarkerBoxLineColour;
    pf.geScalarLinePattern.Value := MarkerBoxLinePattern;
    pf.cbxScalarSmooth.Checked := MarkerBoxLineSmooth;

    pf.cbxAutoCenterGrids.Checked := AutoFocusGrids;
    pf.cbxAutoCenterPoints.Checked := AutoFocusPoints;
    pf.cbxDisplayMarkers.Checked := DisplayMarkers;
    pf.cbxDisplayPoints.Checked := DisplayPoints;
    pf.cbxPointWireFrame.Checked := MarkerWireFrame;
    pf.geMarkerRadius.Value := MarkerRadius;

    pf.cbPointStyle.ItemIndex := PointStyle;

    pf.pnlColourSetting.Color := MarkerColour;
    pf.cbxDisplayLine.Checked := DisplayLines;
    pf.pnlColourLine.Color := LineColour;
    pf.cbLineMode.ItemIndex := LineMode;
    pf.cbxShowPipes.Checked := DisplayPipe;
    pf.gePipeRadius.Value := PipeRadius;
    pf.pnlNullPointColour.Color := PointColourNull.AsWinColor;
    pf.pnlSinglePointColour.Color := colpalette.SingleColour;
    pf.pnlMinPointColour.Color := colpalette.MinColour;
    pf.pnlMaxPointColour.Color := colpalette.MaxColour;
    pf.rgPointOptions.ItemIndex := colpalette.SpectrumMode;
    pf.ebDataCLRFile.Text := colpalette.SpectrumFile;
  end;
  // vector data
  with usersettings.VectorFieldOptions do
  begin
    // bounding box
    pf.cbxVectorAA.Checked := BoxLineAA;
    pf.pnlVectorBoundingColour.Color := BoxLineColour;
    pf.cbxVectorSmooth.Checked := BoxLineSmooth;
    pf.geVectorLinePattern.Value := BoxLinePattern;
    // colour:
    pf.rgVectorDataOptions.ItemIndex := colpalette.SpectrumMode;
    pf.pnlSingleVectorColour.Color := colpalette.SingleColour;
    pf.pnlMinVectorColour.Color := colpalette.MinColour;
    pf.pnlMaxVectorColour.Color := colpalette.MaxColour;
    pf.ebVectorDataCLRFile.Text := colpalette.SpectrumFile;
    // arrow dimensions:
    pf.geVectorMaxArrowRadius.Value := MaxArrowHeadRadius;
    pf.geVectorMinArrowRadius.Value := MinArrowHeadRadius;
    pf.geVectorMaxArrowLength.Value := MaxArrowHeadHeight;
    pf.geVectorMinArrowLength.Value := MinArrowHeadHeight;
    pf.geVectorMaxLength.Value := MaxArrowLength;
    pf.geVectorMinLength.Value := MinArrowLength;
    pf.geVectorMaxRadius.Value := MaxArrowRadius;
    pf.geVectorMinRadius.Value := MinArrowRadius;
    // arrow detail
    pf.geVectorSlices.Value := Slices;
    pf.geVectorStacks.Value := Stacks;
  end;

  // grids/ArcInfo
  with usersettings.ArcInfoGrid do
  begin
    pf.geArcInfoAlpha.Value := Alpha;
    pf.geTileArcInfoX.Value := TileX;
    pf.geTileArcInfoY.Value := TileY;
    pf.cbArcInfoColourMode.ItemIndex := ColourMode;
    pf.cbxPromptTexArcInfo.Checked := PromptTexture;
    pf.cbxArcInfoTwoSided.Checked := TwoSidedMesh;
    pf.cbArcInfoPolygonMode.ItemIndex := PolygonMode;
    pf.pnlArcInfoBlankedColour.Color := ColsBlank.AsWinColor;
    pf.cbxArcInfoSilentImport.Checked := SilentImport;
    pf.cbxArcInfoSilentLoad.Checked := SilentLoad;
  end;
  with usersettings.ArcInfoGrid.colpalette do
  begin
    pf.ebArcInfoCLRFile.Text := SpectrumFile;
    pf.pnlArcInfoColour.Color := SingleColour;
    pf.pnlArcInfoMinColour.Color := MinColour;
    pf.pnlArcInfoMaxColour.Color := MaxColour;
    pf.rgArcInfoOptions.ItemIndex := SpectrumMode;
  end;
  // grids/Surfer
  with usersettings.SurferGrid do
  begin
    pf.geSurferAlpha.Value := Alpha;
    pf.geTileSurferX.Value := TileX;
    pf.geTileSurferY.Value := TileY;
    pf.cbSurferColourMode.ItemIndex := ColourMode;
    pf.cbxPromptTexSurfer.Checked := PromptTexture;
    pf.cbxSurferTwoSided.Checked := TwoSidedMesh;
    pf.cbSurferPolygonMode.ItemIndex := PolygonMode;
    pf.pnlSurferBlankedColour.Color := ColsBlank.AsWinColor;
    pf.cbxSurferSilentImport.Checked := SilentImport;
    pf.cbxSurferSilentLoad.Checked := SilentLoad;
    pf.cbxSurferCreateVisible.Checked := CreateVisible;
  end;
  with usersettings.SurferGrid.colpalette do
  begin
    pf.ebSurferCLRFile.Text := SpectrumFile;
    pf.pnlSurferColour.Color := SingleColour;
    pf.pnlSurferMinColour.Color := MinColour;
    pf.pnlSurferMaxColour.Color := MaxColour;
    pf.rgSurferOptions.ItemIndex := SpectrumMode;
  end;
  // geothermal
  with usersettings.geocolpalette do
  begin
    pf.ebGeothermalCLRFile.Text := SpectrumFile;
    pf.pnlGeothermalColour.Color := SingleColour;
    pf.pnlGeothermalMinColour.Color := MinColour;
    pf.pnlGeothermalMaxColour.Color := MaxColour;
    pf.rgGeothermalOptions.ItemIndex := SpectrumMode;
  end;

  while pf.bContinue do
  begin
    if pf.ShowModal = mrOK then
    begin
      Application.ProcessMessages; // to redraw...
      Screen.Cursor := crHourGlass;
      with usersettings do
      begin
        // display/general
        AutoProcess := pf.cbxAutoProcess.Checked;
        // display/interface
        InvertMouseWheel := pf.cbxInvertMouseWheel.Checked;
        FocusStatus := pf.cbxFocusStatusBar.Checked;
        CameraStatus := pf.cbxCameraStatusBar.Checked;
        // display
        AntiAliasing := pf.cbAntialiasing.ItemIndex;
        BackGroundColour := pf.pnlBackgroundColour.Color;
        TwoSideLighting := pf.cbxTwoSideLighting.Checked;
        ShadeModel := pf.cbShadeModel.ItemIndex;
        // display/axes
        DisplayAxes := pf.cbxDisplayAxes.Checked;
        // display/camera
        CameraStyle := pf.cbCameraStyle.ItemIndex;
        CameraFocalLength := pf.geFocalLength.Value;
        CameraDepthOfView := pf.geDepthOfView.Value;
        CameraNearPlaneBias := pf.geCameraNearPlaneBias.Value;
        // display/lighting
        Lighting := pf.cbxLightingShining.Checked;
        Lighting2 := pf.cbxLighting2Shining.Checked;
        // display/scale
        ScaleX := pf.geScaleX.Value;
        ScaleY := pf.geScaleY.Value;
        ScaleZ := pf.geScaleZ.Value;
        ScaleHUD := pf.cbxShowHUDScale.Checked;

        // display/colourscaleoptions
        ColourScaleOptions.Visible := pf.cbxShowColourScale.Checked;
        ColourScaleOptions.Continuous := pf.cbxColourScaleContinous.Checked;
        ColourScaleOptions.Border := pf.cbxColourScaleBorder.Checked;
        ColourScaleOptions.ShowContourPoints :=
          pf.cbxColourScaleShowContours.Checked;
        ColourScaleOptions.Alpha := pf.geColourScaleAlpha.Value;
        ColourScaleOptions.Scaleformat := pf.ebScaleLabelFormat.Text;
        ColourScaleOptions.ColourScaleType := pf.cbColourScaleType.ItemIndex;

        ColourScaleOptions.ScaleSteps := pf.cbxHUDScaleSteps.Checked;
        ColourScaleOptions.Points := pf.geHUDPoints.Value;

        RenderNullPoints := pf.cbxRenderNullPoints.Checked;
        DisplayMarkers := pf.cbxDisplayMarkers.Checked;
        DisplayPoints := pf.cbxDisplayPoints.Checked;

        MarkerBoxLineAA := pf.cbxScalarAA.Checked;
        MarkerBoxLineColour := pf.pnlScalarBoundingColour.Color;
        MarkerBoxLinePattern := pf.geScalarLinePattern.Value;
        MarkerBoxLineSmooth := pf.cbxScalarSmooth.Checked;

        MarkerRadius := pf.geMarkerRadius.Value;
        PointStyle := pf.cbPointStyle.ItemIndex;
        AutoFocusPoints := pf.cbxAutoCenterPoints.Checked;
        AutoFocusGrids := pf.cbxAutoCenterGrids.Checked;
        MarkerColour := pf.pnlColourSetting.Color;
        MarkerWireFrame := pf.cbxPointWireFrame.Checked;
        DisplayLines := pf.cbxDisplayLine.Checked;
        LineColour := pf.pnlColourLine.Color;
        LineMode := pf.cbLineMode.ItemIndex;
        DisplayPipe := pf.cbxShowPipes.Checked;
        PipeRadius := pf.gePipeRadius.Value;
        colpalette.MinColour := pf.pnlMinPointColour.Color;
        colpalette.MaxColour := pf.pnlMaxPointColour.Color;
        colpalette.SingleColour := pf.pnlSinglePointColour.Color;
        PointColourNull.AsWinColor := pf.pnlNullPointColour.Color; // deprecated
        colpalette.SpectrumFile := pf.ebDataCLRFile.Text;
        colpalette.SpectrumMode := pf.rgPointOptions.ItemIndex;
      end;

      // vector data (checks for min/max done in preferences)
      with usersettings.VectorFieldOptions do
      begin
        // bounding box
        BoxLineAA := pf.cbxVectorAA.Checked;
        BoxLineColour := pf.pnlVectorBoundingColour.Color;
        BoxLineSmooth := pf.cbxVectorSmooth.Checked;
        BoxLinePattern := pf.geVectorLinePattern.Value;
        // arrow colour:
        with colpalette do
        begin
          SpectrumFile := pf.ebVectorDataCLRFile.Text;
          SingleColour := pf.pnlSingleVectorColour.Color;
          MinColour := pf.pnlMinVectorColour.Color;
          MaxColour := pf.pnlMaxVectorColour.Color;
          SpectrumMode := pf.rgVectorDataOptions.ItemIndex;
        end;
        // arrow geometry:
        MaxArrowLength := pf.geVectorMaxLength.Value;
        MinArrowLength := pf.geVectorMinLength.Value;
        MaxArrowRadius := pf.geVectorMaxRadius.Value;
        MinArrowRadius := pf.geVectorMinRadius.Value;
        MaxArrowHeadHeight := pf.geVectorMaxArrowLength.Value;
        MinArrowHeadHeight := pf.geVectorMinArrowLength.Value;
        MaxArrowHeadRadius := pf.geVectorMaxArrowRadius.Value;
        MinArrowHeadRadius := pf.geVectorMinArrowRadius.Value;
        // arrow detail
        Slices := pf.geVectorSlices.Value;
        Stacks := pf.geVectorStacks.Value;
      end;

      // grids/ArcInfo
      with usersettings.ArcInfoGrid do
      begin
        Alpha := pf.geArcInfoAlpha.Value;
        TileX := pf.geTileArcInfoX.Value;
        TileY := pf.geTileArcInfoY.Value;
        PromptTexture := pf.cbxPromptTexArcInfo.Checked;
        ColourMode := pf.cbArcInfoColourMode.ItemIndex;
        ColsBlank.AsWinColor := pf.pnlArcInfoBlankedColour.Color;
        TwoSidedMesh := pf.cbxArcInfoTwoSided.Checked;
        SilentImport := pf.cbxArcInfoSilentImport.Checked;
        SilentLoad := pf.cbxArcInfoSilentLoad.Checked;
        PolygonMode := pf.cbArcInfoPolygonMode.ItemIndex;
        Modified := pf.IsArcInfoModified;
        with colpalette do
        begin
          SpectrumFile := pf.ebArcInfoCLRFile.Text;
          SingleColour := pf.pnlArcInfoColour.Color;
          MinColour := pf.pnlArcInfoMinColour.Color;
          MaxColour := pf.pnlArcInfoMaxColour.Color;
          SpectrumMode := pf.rgArcInfoOptions.ItemIndex;
        end;
      end;

      // grids/Surfer
      with usersettings.SurferGrid do
      begin
        Alpha := pf.geSurferAlpha.Value;
        TileX := pf.geTileSurferX.Value;
        TileY := pf.geTileSurferY.Value;
        PromptTexture := pf.cbxPromptTexSurfer.Checked;
        ColourMode := pf.cbSurferColourMode.ItemIndex;
        ColsBlank.AsWinColor := pf.pnlSurferBlankedColour.Color;
        TwoSidedMesh := pf.cbxSurferTwoSided.Checked;
        SilentImport := pf.cbxSurferSilentImport.Checked;
        SilentLoad := pf.cbxSurferSilentLoad.Checked;
        PolygonMode := pf.cbSurferPolygonMode.ItemIndex;
        CreateVisible := pf.cbxSurferCreateVisible.Checked;
        Modified := pf.IsSurferModified;
        with colpalette do
        begin
          SpectrumFile := pf.ebSurferCLRFile.Text;
          SingleColour := pf.pnlSurferColour.Color;
          MinColour := pf.pnlSurferMinColour.Color;
          MaxColour := pf.pnlSurferMaxColour.Color;
          SpectrumMode := pf.rgSurferOptions.ItemIndex;
        end;
      end;
      // geothermal
      usersettings.geomodified := pf.IsGeothermalModified;
      with usersettings.geocolpalette do
      begin
        SpectrumFile := pf.ebGeothermalCLRFile.Text;
        SingleColour := pf.pnlGeothermalColour.Color;
        MinColour := pf.pnlGeothermalMinColour.Color;
        MaxColour := pf.pnlGeothermalMaxColour.Color;
        SpectrumMode := pf.rgGeothermalOptions.ItemIndex;
      end;

      ApplyUserSettingsToObjects;
      UpdateScaleHUD;

      Screen.Cursor := crDefault;
    end;
  end;
  pf.Release;
end;

// ----- TformMain.acProcessExecute --------------------------------------------
procedure TformMain.acProcessExecute(Sender: TObject);

var
  pf: TformProcess;
  /// kbmBuffer : TkbmMemTable;

begin
  /// kbmBuffer := TkbmMemTable.Create(nil);
  /// kbmBuffer.LoadFromDataSet(kbmData2,[mtcpoStructure]);
  pf := TformProcess.Create(nil);
  if (pf.ShowModal = mrOK) then
  begin
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    RenderProcessed; // on ok we render
    if usersettings.AutoFocusPoints then
      acCenterOnPoints.Execute;
    Screen.Cursor := crDefault;
  end
  else
  begin
    // revert to the buffer data :0
    bProcessing := true;
    (*
      kbmData2.DisableControls;
      kbmData2.Edit;
      kbmData2.LoadFromDataSet(kbmBuffer,[mtcpoStructure]);
      kbmData2.EnableControls;
    *)
    bProcessing := false;
  end;

  /// kbmBuffer.Free;
  pf.Release;
end;

// ----- TformMain.acProcessUpdate ---------------------------------------------
procedure TformMain.acProcessUpdate(Sender: TObject);

begin
  /// acProcess.Enabled := kbmData1.RecordCount > 0;
end;

// ----- TformMain.acRenderBitmapExecute ---------------------------------------
procedure TformMain.acRenderBitmapExecute(Sender: TObject);

begin
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'photos';
  // Render at viewer resolution (scale = 1, DPI = 96)
  RenderToBitMap(1.0);
end;

// ----- TformMain.acRenderBitmap2Execute --------------------------------------
procedure TformMain.acRenderBitmap2Execute(Sender: TObject);

begin
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'photos';
  // Render at twice viewer resolution (scale = 2, DPI = 192 = 96x2)
  RenderToBitMap(2.0);
end;

// ----- TformMain.acRenderBitmap300Execute ------------------------------------
procedure TformMain.acRenderBitmap300Execute(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'photos';
  // Screen is "magic" 96 dpi, this gives us our scale
  RenderToBitMap(300 / 96);
end;

// ----- TformMain.acRenderBitmap600Execute ------------------------------------
procedure TformMain.acRenderBitmap600Execute(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'photos';
  // Screen is "magic" 96 dpi, this gives us our scale
  RenderToBitMap(600 / 96);
end;

// ----- TformMain.acSnapShotExecute -------------------------------------------
procedure TformMain.acSnapShotExecute(Sender: TObject);

var
  bmp32: TGLBitmap32;
  bmp: TBitmap;

begin
  // CreateSnapShot returns a TGLBitmap32, which is a low-level data buffer.
  // However TGLBitmap32 can spawn a regular TBitmap, which we use here }
  bmp32 := GLSceneViewer.Buffer.CreateSnapShot;
  bmp := bmp32.Create32BitsBitmap;

  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'photos';
  if SaveDialog.Execute then
    bmp.SaveToFile(SaveDialog.FileName);

  bmp.Free;
  bmp32.Free;
end;

// ----- TformMain.acSortOriginalExecute ---------------------------------------
procedure TformMain.acSortOriginalExecute(Sender: TObject);
var
  sf: TformSort;
  sl: TStringlist;
  /// buffer : TkbmMemTable;
  i: Integer;

begin
  sl := TStringlist.Create;
  (*
    for i:=0 to kbmData1.FieldDefs.Count-1 do
    sl.Add(kbmData1.Fields[i].FieldName);
  *)
  sf := TformSort.Create(nil);
  sf.LoadFields(sl);

  if sf.ShowModal = mrOK then
  begin
    Application.ProcessMessages; // to redraw...
    Screen.Cursor := crHourGlass;
    (*
      buffer := TkbmMemTable.Create(nil);
      buffer.LoadFromDataSet(kbmData1,[mtcpoStructure]);
      buffer.SortOn(sf.SortString,[]);

      kbmData1.DisableControls;
      kbmData1.Close;
      kbmData1.LoadFromDataSet(buffer,[mtcpoStructure]);
      kbmData1.EnableControls;
      buffer.Free;
    *)
    Screen.Cursor := crDefault;
  end;
  sf.Release;
  sl.Free;
end;

// ----- TformMain.acSortOriginalUpdate ----------------------------------------
procedure TformMain.acSortOriginalUpdate(Sender: TObject);

begin
  /// acSortOriginal.Enabled := kbmData1.RecordCount > 0;
end;

// ----- TformMain.acSortProcessedExecute --------------------------------------
procedure TformMain.acSortProcessedExecute(Sender: TObject);

var
  sf: TformSort;
  sl: TStringlist;
  /// buffer : TkbmMemTable;
  i: Integer;

begin
  sl := TStringlist.Create;
  (*
    for i:=0 to kbmData2.FieldCount-1 do
    sl.Add(kbmData2.Fields[i].FieldName);
  *)
  sf := TformSort.Create(nil);
  sf.LoadFields(sl);
  if sf.ShowModal = mrOK then
  begin
    Application.ProcessMessages; // to redraw...
    Screen.Cursor := crHourGlass;
    (*
      buffer := TkbmMemTable.Create(nil);
      buffer.LoadFromDataSet(kbmData2,[mtcpoStructure]);
      buffer.SortOn(sf.SortString,[]);

      bProcessing := true;     // disable auto editing
      kbmData2.DisableControls;
      kbmData2.Close;
      kbmData2.LoadFromDataSet(buffer,[mtcpoStructure]);
      kbmData2.EnableControls;
    *)
    RenderProcessed;
    bProcessing := false;

    /// buffer.Free;
    Screen.Cursor := crDefault;
  end;
  sl.Free;
  sf.Release;
end;

// ----- TformMain.acSortProcessedUpdate ---------------------------------------
procedure TformMain.acSortProcessedUpdate(Sender: TObject);
begin
  /// acSortProcessed.Enabled := kbmData2.RecordCount > 0;
end;

// ----- TformMain.acSurferAnimationExecute ------------------------------------
procedure TformMain.acSurferAnimationExecute(Sender: TObject);
var
  i: Integer;

begin
  TimerAnimationSurfer.Tag := 0;
  if acSurferAnimation.Checked then
  begin
    GLAVIRecorder.CloseAVIFile(false);
  end
  else
  begin

    for i := 0 to clbSurferGrids.Count - 1 do
    begin
      clbSurferGrids.Checked[i] := false;
      clbSurferGrids.ItemIndex := i;
      clbSurferGridsClickCheck(nil);
    end;

    // assume there is at least two entries - handle in update
    clbSurferGrids.Checked[0] := true;
    clbSurferGrids.ItemIndex := 0;
    clbSurferGridsClickCheck(nil);

    // need a check for movie making...
    if not GLAVIRecorder.CreateAVIFile then
      Exit;
    GLAVIRecorder.Width := GLSceneViewer.Width;
    GLAVIRecorder.Height := GLSceneViewer.Height;
    // if AVIRecorder.filename is empty, a dialog box will appear asking
    // for the filename. CreateAVIFile() will return a bool
    // indicating if user presses "cancel" in the dialog box.
    TimerAnimationSurfer.Enabled := true;
  end;
  acSurferAnimation.Checked := not acSurferAnimation.Checked;
end;

// ----- TformMain.acSurferAnimationUpdate -------------------------------------
procedure TformMain.acSurferAnimationUpdate(Sender: TObject);

begin
  acSurferAnimation.Enabled := (clbSurferGrids.ItemIndex <> -1) and
    (clbSurferGrids.Count > 1);
end;

// ----- TformMain.acTipOfDayExecute -------------------------------------------
procedure TformMain.acTipOfDayExecute(Sender: TObject);

begin
  /// GETipOfDay.Execute;
end;

// ----- TformMain.acViewModeExecute -------------------------------------------
procedure TformMain.acViewModeExecute(Sender: TObject);
begin
  acViewMode.Checked := not acViewMode.Checked;
  acPickMode.Checked := not acViewMode.Checked;
  acMoveMode.Checked := not acViewMode.Checked;
  GLSceneViewer.Cursor := crSizeAll;
end;

// ----- TformMain.cbxCutXYClick -----------------------------------------------
procedure TformMain.cbxCutXYClick(Sender: TObject);
begin
  T3DStructuredScalar(Grid3d.ScalarData.Objects[0]).CutXY.Visible :=
    cbxCutXY.Checked;
end;

// ----- TformMain.cbxCutXZClick -----------------------------------------------
procedure TformMain.cbxCutXZClick(Sender: TObject);
begin
  T3DStructuredScalar(Grid3d.ScalarData.Objects[0]).CutXZ.Visible :=
    cbxCutXZ.Checked;
end;

// ----- TformMain.cbxGridBoundingBoxClick -------------------------------------
procedure TformMain.cbxGridBoundingBoxClick(Sender: TObject);

begin
  Grid3d.Box.Visible := cbxGridBoundingBox.Checked;
end;

// ----- TformMain.clbSurferGridsClick -----------------------------------------
procedure TformMain.clbSurferGridsClick(Sender: TObject);

// ~~~~~ PostIntToGrid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  procedure PostIntToGrid(sProperty: string; iInt: Integer);

  begin
    (*
      with kbmSurferGridInfo do
      begin
      Append;
      FieldByName('Property').AsString := sProperty;
      FieldByName('Integer').AsInteger := iInt;
      Post;
      end;
    *)
  end;
// ~~~~~ PostFloatToGrid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  procedure PostFloatToGrid(sProperty: string; dFloat: double);

  begin
    (*
      with kbmSurferGridInfo do
      begin
      Append;
      FieldByName('Property').AsString := sProperty;
      FieldByName('Float').AsFloat:= dFloat;
      Post;
      end;
    *)
  end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
begin
  if (clbSurferGrids.ItemIndex <> -1) then
  begin
    tsNoSurfer.TabVisible := false;
    tsSurferDisplay.TabVisible := true;
    tsSurferGridInfo.TabVisible := true;
    if pcSurferDetails.Tag > 0 then
      pcSurferDetails.ActivePageIndex := pcSurferDetails.Tag
    else
      pcSurferDetails.ActivePageIndex := 1;

    iSurferIndex := clbSurferGrids.Items.IndexOf
      (clbSurferGrids.Items[clbSurferGrids.ItemIndex]);
    with TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]) do
    begin
      (*
        kbmSurferGridInfo.DisableControls;
        kbmSurferGridInfo.EmptyTable;
        kbmSurferGridInfo.First;
      *)
      PostIntToGrid('X Grid Nodes', NX);
      PostIntToGrid('Y Grid Nodes', NY);
      PostIntToGrid('Total Nodes', NX * NY);
      PostIntToGrid('Blank Nodes', nblanks);
      PostIntToGrid('Triangle Count', TriangleCount);
      PostFloatToGrid('X Low', xlo);
      PostFloatToGrid('X High', xhi);
      PostFloatToGrid('X Range', xrange);
      PostFloatToGrid('X Step', dx);
      PostFloatToGrid('Y Low', ylo);
      PostFloatToGrid('Y High', yhi);
      PostFloatToGrid('Y Range', yrange);
      PostFloatToGrid('Y Step', dy);
      PostFloatToGrid('Z Low', zlo);
      PostFloatToGrid('Z High', zhi);
      PostFloatToGrid('Z Range', zrange);
      /// kbmSurferGridInfo.EnableControls;
      // load display options
      cbSurferColourMode.ItemIndex := ColourMode;
      cbSurferPolygonMode.ItemIndex := PolygonMode;
      /// geSurferAlpha.Value := Alpha;
      cbxSurferTwoSided.Checked := TwoSided;
      ebSurferGridName.Text := GridName;
      // base maps
      cbxSurferBaseMap.Checked := EnableBaseMap;
      ebSurferBaseMap.Text := BaseMapPath;
      /// geTileSurferX.Value := TileX;
      /// geTileSurferY.Value := TileY;
    end;
  end
  else
  begin
    tsNoSurfer.TabVisible := true;
    tsSurferDisplay.TabVisible := false;
    tsSurferGridInfo.TabVisible := false;
    pcSurferDetails.ActivePageIndex := 0;
    /// kbmSurferGridInfo.EmptyTable;
  end;
end;

// ----- TformMain.clbSurferGridsClickCheck ------------------------------------
procedure TformMain.clbSurferGridsClickCheck(Sender: TObject);

var
  i: Integer;
  sItem: string;

begin
  sItem := clbSurferGrids.Items[clbSurferGrids.ItemIndex];
  i := clbSurferGrids.Items.IndexOf(sItem);
  TGLContourGridData(clbSurferGrids.Items.Objects[i]).Visible :=
    clbSurferGrids.Checked[i];
end;

// ----- TformMain.dbgData2DblClick --------------------------------------------
procedure TformMain.dbgData2DblClick(Sender: TObject);
begin
  acFocusProcessed.Execute;
end;

// ----- TFormmain.FormDestroy -------------------------------------------------
procedure TformMain.FormDestroy(Sender: TObject);

var
  i: Integer;

begin
  contourpoints.ClearAll;
  contourpoints.Free;

  VectorField.ClearAll;
  VectorField.Free;

  Grid3d.Free;
  geosimgrid.Free;
  axes.Free;

  for i := Low(Contourbm) to High(Contourbm) do
    /// kbmData2.FreeBookmark(contourbm[i]);
    SetLength(Contourbm, 0);
  for i := Low(Vectorbm) to High(Vectorbm) do
    /// kbmData2.FreeBookmark(vectorbm[i]);
    SetLength(Vectorbm, 0);

  usersettings.SaveToRegistry;
  usersettings.Free;
end;

// ----- TformMain.FormMouseWheel ----------------------------------------------
procedure TformMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio}
  if usersettings.InvertMouseWheel then
    GLCamera.AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120))
  else
    GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  UpdateCameraFocusPositions;
end;

// ----- TformMain.LoadAxesProperties ------------------------------------------
procedure TformMain.LoadAxesProperties;

begin

  axes.ScaleX := usersettings.ScaleX;
  axes.ScaleY := usersettings.ScaleY;
  axes.ScaleZ := usersettings.ScaleZ;
  formAxes.SetAxesProperties(axes);
end;

// ----- TFormmain.FormShow ----------------------------------------------------
procedure TformMain.FormShow(Sender: TObject);

begin
  // preps for movie making
  GLSceneViewer.ResetPerformanceMonitor;
  GLAVIRecorder.Width := GLSceneViewer.Width;
  GLAVIRecorder.Height := GLSceneViewer.Height;

  // scalar and vector points
  contourpoints := TContourPointCollection.Create(GLDummyCubeObjects,
    GLSphere1);
  VectorField := TVectorField.Create(GLDVectorData);
  // load and apply registry settings
  usersettings := TGLDataUserSettings.Create;
  usersettings.LoadFromRegistry;

  Grid3d := T3DStructuredDataSet.Create(GLDCubeGrid);

  axes := TGLCoordinateAxes.Create(GLDAxes);
  axes.LabelFont := AxesFont; // assign a font for drawing and stuff...
  axes.GenerateXLabels;
  axes.GenerateYlabels;
  axes.GenerateZLabels;
  LoadAxesProperties;

  geosimgrid := TGLSimulationgrid.CreateWithDummy(GLDGeoSim);

  clbArcInfoGridsClick(nil);
  clbSurferGridsClick(nil);

  VectorField.colpalette := usersettings.VectorFieldOptions.colpalette;
  geosimgrid.colpalette := usersettings.geocolpalette;

  GLSceneViewer.Cursor := crSizeAll;
  UpdateCameraFocusPositions;

  bProcessing := false;
  /// GEImportFile.Identifier := 'original';
  UpdateScaleHUD;

  ApplyUserSettingsToObjects;
  (*
    if geTipOfDay.ShowAtStart then
    geTipOfDay.Execute;
    geISO.Value := 200;
  *)
end;

// ----- TformMain.gbCutXYChange -----------------------------------------------
procedure TformMain.gbCutXYChange(Sender: TObject);
begin
  (*
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).ZLevel := gbCutXY.Position;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).CutXY.Position.Z :=
    grid3d.zlo + gbCutXY.Position*grid3d.zdelta;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).CutXY.Structurechanged;
  *)
end;

// ----- TformMain.gbCutXZChange -----------------------------------------------
procedure TformMain.gbCutXZChange(Sender: TObject);
begin
  (*
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).YLevel := gbCutXZ.Position;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).CutXZ.Position.Y :=
    grid3d.ylo + gbCutXZ.Position*grid3d.ydelta;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).CutXZ.Structurechanged;
  *)
end;

// ----- TformMain.gbCutYZChange -----------------------------------------------
procedure TformMain.gbCutYZChange(Sender: TObject);
begin
  (*
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).XLevel := gbCutYZ.Position;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).Cutyz.Position.x :=
    grid3d.xlo + gbCutYZ.Position*grid3d.xdelta;
    T3DStructuredScalar(grid3d.ScalarData.Objects[0]).CutYZ.Structurechanged;
  *)
end;

// ----- TformMain.GLSceneViewerMouseEnter -------------------------------------
procedure TformMain.GLSceneViewerMouseEnter(Sender: TObject);
begin
  GLSceneViewer.SetFocus;
end;

// ----- TformMain.GEImportFileAfterImport -------------------------------------
procedure TformMain.GEImportFileAfterImport(Sender: TObject);
begin
  if usersettings.AutoProcess then
    acProcess.Execute;
end;

// ----- TFormmain.GLSceneViewerMouseDown --------------------------------------
procedure TformMain.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pick: TGLCustomSceneObject;
  pickproxy: TGLProxyObject;
  iIndex, iLayer, iPos: Integer;
  sBlock: string;
  fb: TformBlock;

begin
  // store mouse coordinates when a button went down
  mdx := X;
  mdy := Y;
  if acPickMode.Checked then
  begin
    if GLSceneViewer.Buffer.GetPickedObject(X, Y) = nil then
      Exit;

    // assumes proxies are markers only - needs extending
    if (GLSceneViewer.Buffer.GetPickedObject(X, Y) is TGLProxyObject) then
    begin
      pickproxy := (GLSceneViewer.Buffer.GetPickedObject(X, Y)
        as TGLProxyObject);
      GLDummyCube.Position.AsVector := pickproxy.AbsolutePosition;
      GLCamera.TransformationChanged;
      /// kbmData2.DisableControls;
      // obtains the appropriate bookmark from the proxy name
      iIndex := contourpoints.PointList.IndexOf(pickproxy.Name);
      (*
        if iIndex <> -1 then
        kbmData2.GotoBookmark(TContourPoint(ContourPoints.PointList.
        Objects[iIndex]).BookMark);
        kbmData2.EnableControls;
      *)
      // geothermal blocks
    end
    else if (Pos('Block_', GLSceneViewer.Buffer.GetPickedObject(X, Y).Name) = 1)
    then
    begin
      fb := TformBlock.Create(nil);
      sBlock := GLSceneViewer.Buffer.GetPickedObject(X, Y).Name;
      Delete(sBlock, 1, 6);
      iPos := Pos('_Layer_', sBlock);
      iLayer := StrToInt(Copy(sBlock, iPos + 7, Length(sBlock)));
      sBlock := Copy(sBlock, 1, iPos - 1);

      fb.lebName.Text := sBlock;
      fb.lebLayer.Text := IntToStr(iLayer);

      // obtain the correct layer
      with TGLGridLayer(geosimgrid.GridLayers.GetGLLayerByName
        (IntToStr(iLayer))) do
      begin
        if (GetBlockByName(sBlock) <> nil) then
          with GetBlockByName(sBlock) do
          begin
            fb.lebNX.Text := IntToStr(Row);
            fb.lebNY.Text := IntToStr(Column);
            fb.gePorosity.Value := Porosity;
            fb.gePermX.Value := PermeabilityX;
            fb.gePermY.Value := PermeabilityY;
            fb.gePermZ.Value := PermeabilityZ;
            fb.lebPressure.Text := FloatToStr(Pr);
            fb.lebTemperature.Text := FloatToStr(Te);
            fb.lebSaturation.Text := FloatToStr(Sv);
            GLDummyCube.Position.X := LocationE;
            GLDummyCube.Position.Y := LocationN;
            GLDummyCube.Position.z := Elevation;
            UpdateCameraFocusPositions;
            GLCamera.TransformationChanged;

            fb.cbxActiveBlock.Checked := not Inactive;
            // post the rock changes back :)
            if (fb.ShowModal = mrOK) then
            begin
              Inactive := not fb.cbxActiveBlock.Checked;

              with geosimgrid.GetRockTypeByName(sBlock) do
              begin
                Porosity := fb.gePorosity.Value;
                PermeabilityX := fb.gePermX.Value;
                PermeabilityY := fb.gePermY.Value;
                PermeabilityZ := fb.gePermZ.Value;
              end;

              ResetAllBlocks;
            end;
          end;
      end;

      fb.Release;

    end
    else if (Pos('vect', GLSceneViewer.Buffer.GetPickedObject(X, Y).Name) = 1)
    then
    begin
      // vector field
      pick := (GLSceneViewer.Buffer.GetPickedObject(X, Y)
        as TGLCustomSceneObject);
      GLDummyCube.Position.AsVector := pick.AbsolutePosition;
      UpdateCameraFocusPositions;
      GLCamera.TransformationChanged;
      /// kbmData2.DisableControls;
      // obtains the appropriate bookmark from the proxy name
      iIndex := VectorField.PointList.IndexOf(pick.Name);
      (*
        if iIndex <> -1 then
        kbmData2.GotoBookmark(TVectorPoint(VectorField.PointList.
        Objects[iIndex]).BookMark);
        kbmData2.EnableControls;
      *)
    end
    else
    begin
      pick := (GLSceneViewer.Buffer.GetPickedObject(X, Y)
        as TGLCustomSceneObject);
      if Assigned(pick) then
      begin
        // set the focus cube to the object
        GLDummyCube.Position.AsVector := pick.AbsolutePosition;
        UpdateCameraFocusPositions;
        GLCamera.TransformationChanged;
      end;
    end;

    // object movement
  end
  else if acMoveMode.Checked then
  begin
    if (GLSceneViewer.Buffer.GetPickedObject(X, Y) is TGLProxyObject) then
      pickproxy := (GLSceneViewer.Buffer.GetPickedObject(X, Y)
        as TGLProxyObject)
    else
      pickproxy := nil;
    if (pickproxy <> moveproxy) then
      moveproxy := pickproxy;
    if Assigned(moveproxy) then
    begin
      GLHUDMovePosition.Visible := true;
      lastmouseworldpos := MouseWorldPos(X, Y);
    end;
  end;
end;

// ----- TFormmain.GLSceneViewerMouseMove --------------------------------------
procedure TformMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

const
  zoomcoeff = 0.12;

var
  dx, dy: Integer;
  v, newpos: TVector;

begin
  if acViewMode.Checked then
  begin
    // calculate delta since last move or last mousedown
    dx := mdx - X;
    dy := mdy - Y;
    mdx := X;
    mdy := Y;
    if ssLeft in Shift then
    begin
      if (ssShift in Shift) then
        GLCamera.AdjustDistanceToTarget(IntPower(1.05, dy))
      else
        GLCamera.MoveAroundTarget(dy, dx)
    end
    else if (ssRight in Shift) then
    begin
      if (ssShift in Shift) then
        GenerateLocalYCameraTranslation(dx, dy, zoomcoeff, v)
      else
        GenerateLocalZCameraTranslation(dx, dy, zoomcoeff, v);
      // move the focus cube and the camera will follow
      GLDummyCube.Position.Translate(v);
    end;
    UpdateCameraFocusPositions;
  end
  else if acMoveMode.Checked then
  begin
    if ssLeft in Shift then
    begin
      // needs fixing
      // if (ssSHift in SHift) <> bMoveOnZ then
      // begin
      // bMoveOnZ := (ssShift in SHift);
      // lastmouseworldpos := MouseworldPos(x,y);
      // end;
      newpos := MouseWorldPos(X, Y);

      // live updating - experimental
      if Assigned(moveproxy) and (VectorNorm(lastmouseworldpos) <> 0) then
      begin
        moveproxy.Position.Translate(VectorSubtract(newpos, lastmouseworldpos));
        UpdateNewPosition;
      end;
      lastmouseworldpos := newpos;
    end;
  end;
end;

// ----- TformMain.GLSceneViewerMouseUp ----------------------------------------
procedure TformMain.GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  iIndex: Integer;
  v: TVector3f;
begin

  if acMoveMode.Checked and Assigned(moveproxy) then
  begin
    /// kbmData2.DisableControls;

    { obtains the appropriate bookmark from the proxy name }
    iIndex := contourpoints.PointList.IndexOf(moveproxy.Name);
    (*
      kbmData2.GotoBookmark(TContourPoint(ContourPoints.PointList.
      Objects[iIndex]).BookMark);
      kbmData2.Edit;

      // easy way of updating this datapoint without processing the lot?
      kbmData2.FieldByName('X').AsFloat := moveproxy.Position.X/usersettings.scaleX;
      kbmData2.FieldByName('Y').AsFloat := moveproxy.Position.Y/usersettings.scaleY;
      kbmData2.FieldByName('Z').AsFloat := moveproxy.Position.Z/usersettings.scaleZ;

      GLLines.Nodes[kbmData2.RecNo-1].X := moveproxy.Position.X;
      GLLines.Nodes[kbmData2.RecNo-1].Y := moveproxy.Position.Y;
      GLLines.Nodes[kbmData2.RecNo-1].Z := moveproxy.Position.Z;
    *)
    // update the GLPoints position
    v.v[0] := moveproxy.Position.X;
    v.v[1] := moveproxy.Position.Y;
    v.v[2] := moveproxy.Position.z;

    (*
      GLPoints.Positions.Delete(kbmData2.RecNo-1);
      GLPoints.Positions.Insert(kbmData2.RecNo-1,v);

      kbmData2.Post;
      kbmData2.EnableControls;
    *)
    GLHUDMovePosition.Visible := false;
    moveproxy := nil;
  end;
end;

// ----- TformMain.kbmData2AfterDelete -----------------------------------------
procedure TformMain.kbmData2AfterDelete(DataSet: TDataSet);
begin
  UpdateStatusProcessed;
  if (not bProcessing) then
    RenderProcessed;
end;

// ----- TformMain.kbmData2AfterInsert -----------------------------------------
procedure TformMain.kbmData2AfterInsert(DataSet: TDataSet);

begin
  UpdateStatusProcessed;
  if (not bProcessing) then
    RenderProcessed;
end;

// ----- TformMain.kbmData2AfterPost -------------------------------------------
procedure TformMain.kbmData2AfterPost(DataSet: TDataSet);

var
  iPos: Integer;

begin
  UpdateStatusProcessed;
  if (not bProcessing) then
  begin
    /// iPos := kbmData2.RecNo;
    // sub-optimal ?
    RenderProcessed;

    /// kbmData2.RecNo := iPos;
  end;
end;

// ----- TformMain.TimerAnimationTimer -----------------------------------------
procedure TformMain.TimerAnimationSurferTimer(Sender: TObject);

begin
  if (pcMain.ActivePage = tsSurfer) then
  begin
    if TimerAnimationSurfer.Tag < clbSurferGrids.Count - 1 then
    begin
      GLAVIRecorder.AddAVIFrame;
      clbSurferGrids.Checked[TimerAnimationSurfer.Tag] := false;
      clbSurferGrids.ItemIndex := TimerAnimationSurfer.Tag;
      clbSurferGridsClickCheck(nil); // turn this off
      TimerAnimationSurfer.Tag := TimerAnimationSurfer.Tag + 1;
      clbSurferGrids.Checked[TimerAnimationSurfer.Tag] := true;
      clbSurferGrids.ItemIndex := TimerAnimationSurfer.Tag;
      clbSurferGridsClickCheck(nil); // turn this on
      if (TimerAnimationSurfer.Tag = clbSurferGrids.Count - 1) then
      begin
        TimerAnimationSurfer.Enabled := false;
        GLAVIRecorder.AddAVIFrame; // add the last frame
        Application.ProcessMessages;
        acSurferAnimation.Execute;
      end;
      Application.ProcessMessages;
    end;
  end;
end;

// ----- TformMain.ApplyUserSettingsToObjects ----------------------------------
procedure TformMain.ApplyUserSettingsToObjects;

var
  sApp: string;
  i: Integer;

begin
  with usersettings do
  begin
    glBoundingBox.AntiAliased := MarkerBoxLineAA;
    glBoundingBox.LineColor.AsWinColor := MarkerBoxLineColour;
    glBoundingBox.LinePattern := MarkerBoxLinePattern;
    glBoundingBox.LinesSmoothing := MarkerBoxLineSmooth;

    sbFocus.Visible := FocusStatus;
    sbCamera.Visible := CameraStatus;
    GLLightSource.Shining := Lighting;
    GLLightSourceReverse.Shining := Lighting2;
    contourpoints.Shown := DisplayMarkers;
    GLLines.Visible := DisplayLines;
    GLLines.LineColor.AsWinColor := LineColour;
    case LineMode of
      0:
        GLLines.SplineMode := lsmCubicSpline;
      1:
        GLLines.SplineMode := lsmLines;
    end;
    GLPipe.Visible := DisplayPipe;
    GLPipe.Radius := PipeRadius;
  end;

  DrawGLPoints;
  DrawGLPipe;

  GLDAxes.Position.X := formAxes.geAxesXOrigin.Value * usersettings.ScaleX;
  GLDAxes.Position.Y := formAxes.geAxesYOrigin.Value * usersettings.ScaleY;
  GLDAxes.Position.z := formAxes.geAxesZOrigin.Value * usersettings.ScaleZ;
  axes.ScaleX := usersettings.ScaleX;
  axes.ScaleY := usersettings.ScaleY;
  axes.ScaleZ := usersettings.ScaleZ;

  geosimgrid.freeform.scale.X := usersettings.ScaleX;
  geosimgrid.freeform.scale.Y := usersettings.ScaleY;
  geosimgrid.freeform.scale.z := usersettings.ScaleZ;

  // apply vector-field settings
  VectorField.ScaleX := usersettings.ScaleX;
  VectorField.ScaleY := usersettings.ScaleY;
  VectorField.ScaleZ := usersettings.ScaleZ;
  with VectorField do
  begin
    glBoundingBoxVector.AntiAliased :=
      usersettings.VectorFieldOptions.BoxLineAA;
    glBoundingBoxVector.LineColor.AsWinColor :=
      usersettings.VectorFieldOptions.BoxLineColour;
    glBoundingBoxVector.LinesSmoothing :=
      usersettings.VectorFieldOptions.BoxLineSmooth;
    glBoundingBoxVector.LinePattern :=
      usersettings.VectorFieldOptions.BoxLinePattern;

    // set scale here...
    MaxArrowHeadRadius := usersettings.VectorFieldOptions.MaxArrowHeadRadius;
    MinArrowHeadRadius := usersettings.VectorFieldOptions.MinArrowHeadRadius;
    MaxArrowHeadHeight := usersettings.VectorFieldOptions.MaxArrowHeadHeight;
    MinArrowHeadHeight := usersettings.VectorFieldOptions.MinArrowHeadHeight;
    MinArrowLength := usersettings.VectorFieldOptions.MinArrowLength;
    MaxArrowLength := usersettings.VectorFieldOptions.MaxArrowLength;
    MinArrowRadius := usersettings.VectorFieldOptions.MinArrowRadius;
    MaxArrowRadius := usersettings.VectorFieldOptions.MaxArrowRadius;
    Slices := usersettings.VectorFieldOptions.Slices;
    Stacks := usersettings.VectorFieldOptions.Stacks;
    RenderField;
  end;

  GLSceneViewer.Buffer.BackgroundColor := usersettings.BackGroundColour;
  GLDummyCube.ShowAxes := usersettings.DisplayAxes;
  case usersettings.AntiAliasing of
    0:
      GLSceneViewer.Buffer.AntiAliasing := aaNone;
    1:
      GLSceneViewer.Buffer.AntiAliasing := aaDefault;
    2:
      GLSceneViewer.Buffer.AntiAliasing := aa2x;
    3:
      GLSceneViewer.Buffer.AntiAliasing := aa2xHQ;
    4:
      GLSceneViewer.Buffer.AntiAliasing := aa4x;
    5:
      GLSceneViewer.Buffer.AntiAliasing := aa4xHQ;
  else
    GLSceneViewer.Buffer.AntiAliasing := aaDefault;
  end;

  case usersettings.ShadeModel of
    0:
      GLSceneViewer.Buffer.ShadeModel := smDefault;
    1:
      GLSceneViewer.Buffer.ShadeModel := smFlat;
    2:
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
  else
    GLSceneViewer.Buffer.ShadeModel := smDefault;
  end;

  if usersettings.TwoSideLighting then
    GLSceneViewer.Buffer.ContextOptions := GLSceneViewer.Buffer.ContextOptions +
      [roTwoSideLighting]
  else
    GLSceneViewer.Buffer.ContextOptions := GLSceneViewer.Buffer.ContextOptions -
      [roTwoSideLighting];
  case usersettings.CameraStyle of
    0:
      GLCamera.CameraStyle := csOrthogonal;
    1:
      GLCamera.CameraStyle := csOrtho2D;
    2:
      GLCamera.CameraStyle := csPerspective;
  end;
  GLCamera.NearPlaneBias := usersettings.CameraNearPlaneBias;
  GLCamera.FocalLength := usersettings.CameraFocalLength;
  GLCamera.DepthOfView := usersettings.CameraDepthOfView;
  GLCamera.TransformationChanged;

  GLHUDTextScale.Visible := usersettings.ScaleHUD;

  contourpoints.Radius := usersettings.MarkerRadius;
  contourpoints.Colour := usersettings.MarkerColour;
  contourpoints.WireFrame := usersettings.MarkerWireFrame;

  { ** apply scale factors }
  contourpoints.ScaleX := usersettings.ScaleX;
  GLLines.scale.X := usersettings.ScaleX;
  GLPoints.scale.X := usersettings.ScaleX;
  GLPipe.scale.X := usersettings.ScaleX;
  geosimgrid.ScaleX := usersettings.ScaleX; // doesn't update
  glBoundingBox.scale.X := usersettings.ScaleX;
  glBoundingBoxVector.scale.X := usersettings.ScaleX;

  contourpoints.ScaleY := usersettings.ScaleY;
  GLLines.scale.Y := usersettings.ScaleY;
  GLPoints.scale.Y := usersettings.ScaleY;
  GLPipe.scale.Y := usersettings.ScaleY;
  geosimgrid.ScaleY := usersettings.ScaleY;
  glBoundingBox.scale.Y := usersettings.ScaleY;
  glBoundingBoxVector.scale.Y := usersettings.ScaleY;

  contourpoints.ScaleZ := usersettings.ScaleZ;
  GLPoints.scale.z := usersettings.ScaleZ;
  GLLines.scale.z := usersettings.ScaleZ;
  GLPipe.scale.z := usersettings.ScaleZ;
  geosimgrid.ScaleZ := usersettings.ScaleZ;
  glBoundingBox.scale.z := usersettings.ScaleZ;
  glBoundingBoxVector.scale.z := usersettings.ScaleZ;

  if usersettings.SurferGrid.Modified then
  begin
    sApp := ExtractFilePath(Application.ExeName);
    if DirectoryExists(usersettings.SurferGrid.DefaultDir) then
      OpenSurfer.InitialDir := usersettings.SurferGrid.DefaultDir
    else if DirectoryExists(sApp + 'samples\surfer') then
      OpenSurfer.InitialDir := sApp + 'samples\surfer\'
    else
      OpenSurfer.InitialDir := sApp;
  end;

  for i := 0 to clbSurferGrids.Count - 1 do
    UpdateSurferScale(i);

  if usersettings.ArcInfoGrid.Modified then
  begin
    sApp := ExtractFilePath(Application.ExeName);
    if DirectoryExists(usersettings.ArcInfoGrid.DefaultDir) then
      OpenArcInfo.InitialDir := usersettings.ArcInfoGrid.DefaultDir
    else if DirectoryExists(sApp + 'samples\arcinfo') then
      OpenArcInfo.InitialDir := sApp + 'samples\arcinfo\'
    else
      OpenArcInfo.InitialDir := sApp;
    usersettings.ArcInfoGrid.Modified := false;
  end;

  for i := 0 to clbArcInfoGrids.Count - 1 do
    UpdateArcInfoScale(i);

  // update geothermal
  if usersettings.geomodified then
  begin
    geosimgrid.colpalette := usersettings.geocolpalette;
    geosimgrid.GridLayers.RenderAll;
    usersettings.geomodified := false;
  end;

  // choose which palette we are using...
  case usersettings.ColourScaleOptions.ColourScaleType of
    0:
      GenerateHUDColourScale;
    1:
      GenerateVectorColourScale;
    2:
      GenerateSurferColourScale;
    3:
      GenerateArcInfocolourScale;
    4:
      GenerateGeoColourScale;
  end;
end;

// ----- TformMain.UpdateArcInfoGrid -------------------------------------------
// to be made a method...
procedure TformMain.UpdateArcInfoGrid(iIndex: Integer);

begin
  with TGLContourGridData(clbArcInfoGrids.Items.Objects[iIndex]) do
  begin
    grid.Visible := false;
    TwoSided := usersettings.ArcInfoGrid.TwoSidedMesh;
    ColourMode := usersettings.ArcInfoGrid.ColourMode;
    PolygonMode := usersettings.ArcInfoGrid.PolygonMode;
    Alpha := usersettings.ArcInfoGrid.Alpha;
    TileX := usersettings.ArcInfoGrid.TileX;
    TileY := usersettings.ArcInfoGrid.TileY;
    grid.Visible := clbArcInfoGrids.Checked[iIndex];
  end;
end;

procedure TformMain.UpdateSurferBounds;

var
  i: Integer;
  zmin, zmax: double;

begin
  zmin := MAXDOUBLE;
  zmax := MINDOUBLE;
  // one pass to find bounds
  for i := 0 to clbSurferGrids.Count - 1 do
  begin
    with TGLContourGridData(clbSurferGrids.Items.Objects[i]) do
    begin
      if zmin > zlo then
        zmin := zlo;
      if zmax < zhi then
        zmax := zhi;
    end;
  end;
  // one pass to apply them
  usersettings.SurferGrid.colpalette.MinValue := zmin;
  usersettings.SurferGrid.colpalette.MaxValue := zmax;
  for i := 0 to clbSurferGrids.Count - 1 do
    UpdateSurferGrid(i);
  // update the HUD scale
  if (usersettings.ColourScaleOptions.ColourScaleType = 2) then
    GenerateSurferColourScale;
end;

// ----- TformMain.UpdateSurferGrid --------------------------------------------
procedure TformMain.UpdateSurferGrid(iIndex: Integer);

begin
  with TGLContourGridData(clbSurferGrids.Items.Objects[iIndex]) do
  begin
    grid.Visible := false;
    TwoSided := usersettings.SurferGrid.TwoSidedMesh;
    ColourMode := usersettings.SurferGrid.ColourMode;
    PolygonMode := usersettings.SurferGrid.PolygonMode;
    Alpha := usersettings.SurferGrid.Alpha;
    TileX := usersettings.SurferGrid.TileX;
    TileY := usersettings.SurferGrid.TileY;
    grid.Visible := clbSurferGrids.Checked[iIndex];
  end;
end;

// ----- TformMain.UpdateSurferScale -------------------------------------------
procedure TformMain.UpdateSurferScale(iIndex: Integer);

begin
  with TGLContourGridData(clbSurferGrids.Items.Objects[iIndex]) do
  begin
    ScaleX := usersettings.ScaleX;
    ScaleY := usersettings.ScaleY;
    ScaleZ := usersettings.ScaleZ;
  end;
end;

// ----- TformMain.UpdateArcInfoScale ------------------------------------------
procedure TformMain.UpdateArcInfoScale(iIndex: Integer);

begin
  with TGLContourGridData(clbArcInfoGrids.Items.Objects[iIndex]) do
  begin
    ScaleX := usersettings.ScaleX;
    ScaleY := usersettings.ScaleY;
    ScaleZ := usersettings.ScaleZ;
  end;
end;

// ----- TformMain.DrawConnectionLines -----------------------------------------
procedure TformMain.DrawConnectionLines;

var
  i: Integer;

begin
  { ** render the connectivity lines }
  GLLines.Nodes.Clear;
  for i := 0 to contourpoints.Count - 1 do
    GLLines.Nodes.AddNode(TContourPoint(contourpoints.PointList.Objects[i]).X,
      TContourPoint(contourpoints.PointList.Objects[i]).Y,
      TContourPoint(contourpoints.PointList.Objects[i]).z);
end;

// ----- TformMain.DrawGLPipe --------------------------------------------------
procedure TformMain.DrawGLPipe;

var
  i: Integer;
  dRat: double;
  col: TColorVector;

begin
  GLPipe.Nodes.Clear;
  GLPipe.BeginUpdate;
  for i := 0 to contourpoints.Count - 1 do
  begin
    GLPipe.Nodes.AddNode(TContourPoint(contourpoints.PointList.Objects[i]).X,
      TContourPoint(contourpoints.PointList.Objects[i]).Y,
      TContourPoint(contourpoints.PointList.Objects[i]).z);
    GLPipe.NodesColorMode := pncmEmission;

    if TContourPoint(contourpoints.PointList.Objects[i]).IsNull then
      col := usersettings.PointColourNull.Color
    else
    begin
      dRat := (TContourPoint(contourpoints.PointList.Objects[i]).Value -
        contourpoints.MinValue) /
        (contourpoints.MaxValue - contourpoints.MinValue);
      col := usersettings.GetScalarSpectrum(dRat,
        TContourPoint(contourpoints.PointList.Objects[i]).Value);
    end;
    TGLPipeNode(GLPipe.Nodes[i]).Color.Color := col;
  end;
  GLPipe.EndUpdate;
end;

// ----- TformMain.DrawGLPoints ------------------------------------------------
procedure TformMain.DrawGLPoints;

var
  i: Integer;
  col: TColorVector;
  dRat: double;

begin
  GLPoints.Positions.Clear;
  GLPoints.Colors.Clear;
  if contourpoints.Count > 0 then
    GLPoints.Visible := usersettings.DisplayPoints;

  case usersettings.PointStyle of
    0:
      GLPoints.Style := psRound;
    1:
      GLPoints.Style := psSmooth;
    2:
      GLPoints.Style := psSmoothAdditive;
    3:
      GLPoints.Style := psSquare;
    4:
      GLPoints.Style := psSquareAdditive;
  end;

  for i := 0 to contourpoints.Count - 1 do
  begin
    GLPoints.Positions.Add(TContourPoint(contourpoints.PointList.Objects[i]).X,
      TContourPoint(contourpoints.PointList.Objects[i]).Y,
      TContourPoint(contourpoints.PointList.Objects[i]).z);
    if TContourPoint(contourpoints.PointList.Objects[i]).IsNull then
      col := usersettings.PointColourNull.Color
    else
    begin
      dRat := (TContourPoint(contourpoints.PointList.Objects[i]).Value -
        contourpoints.MinValue) /
        (contourpoints.MaxValue - contourpoints.MinValue);
      col := usersettings.GetScalarSpectrum(dRat,
        TContourPoint(contourpoints.PointList.Objects[i]).Value);
    end;
    GLPoints.Colors.Add(col)
  end;
end;

// ----- TformMain.GenerateHUDColourScale ------------------------------------
procedure TformMain.GenerateHUDColourScale;

begin
  if not Assigned(usersettings.colpalette) then
    Exit;
  usersettings.UpdatePalette(0);
  RenderHUDScale('scalehudpoints', usersettings.colpalette);
end;

// ----- TformMain.GenerateVectorColourScale -----------------------------------
procedure TformMain.GenerateVectorColourScale;

begin
  if not Assigned(usersettings.VectorFieldOptions.colpalette) then
    Exit;
  usersettings.UpdatePalette(1);
  RenderHUDScale('scalehudvector', usersettings.VectorFieldOptions.colpalette);
end;

// ----- TformMain.GenerateSurferColourScale -----------------------------------
procedure TformMain.GenerateSurferColourScale;

begin
  if not Assigned(usersettings.SurferGrid.colpalette) then
    Exit;
  usersettings.UpdatePalette(2);
  RenderHUDScale('scalehudsurfer', usersettings.SurferGrid.colpalette);
end;

// ----- TformMain.GenerateArcInfocolourScale ----------------------------------
procedure TformMain.GenerateArcInfocolourScale;

begin
  if not Assigned(usersettings.ArcInfoGrid.colpalette) then
    Exit;
  usersettings.UpdatePalette(3);
  RenderHUDScale('scalehudarcinfo', usersettings.ArcInfoGrid.colpalette);
end;

// ----- TformMain.GenerateGeoColourScale --------------------------------------
procedure TformMain.GenerateGeoColourScale;

begin
  if not Assigned(usersettings.geocolpalette) then
    Exit;
  usersettings.UpdatePalette(4);
  RenderHUDScale('scalehudgeothermal', usersettings.geocolpalette);
end;

// ----- TFormmain.GenerateLocalYCameraTranslation -----------------------------
procedure TformMain.GenerateLocalYCameraTranslation(dx: Integer; dy: Integer;
  zoomcoeff: double; var v: TVector);

var
  X, Y: Integer;
  dist, localX, localY, localZ: TVector;

begin
  X := dx;
  Y := dy;

  dist := VectorSubtract(GLCamera.AbsolutePosition,
    GLCamera.TargetObject.AbsolutePosition);
  NormalizeVector(dist);
  localX := VectorCrossProduct(GLCamera.Up.AsVector, dist);
  NormalizeVector(localX);
  localZ := ZHmgVector;
  localY := VectorCrossProduct(localZ, localX);
  NormalizeVector(localY);
  if GLCamera.Position.z < 0 then
  begin
    X := -X;
    Y := -Y;
  end;
  v := GLCamera.ScreenDeltaToVector(X, Y, zoomcoeff * GLCamera.DistanceToTarget
    / GLCamera.FocalLength, localY);
end;

// ----- TFormmain.GenerateLocalZCameraTranslation -----------------------------
procedure TformMain.GenerateLocalZCameraTranslation(dx: Integer; dy: Integer;
  zoomcoeff: double; var v: TVector);

var
  X, Y: Integer;

begin
  X := dx;
  Y := -dy;

  v := GLCamera.ScreenDeltaToVectorXY(X, Y,
    zoomcoeff * GLCamera.DistanceToTarget / GLCamera.FocalLength);
end;

// ----- TformMain.HasHelp -----------------------------------------------------
function TformMain.HasHelp: Boolean;

begin
  result := Fileexists(ExtractFilePath(Application.ExeName) + 'glData.chm');
end;

// ----- TformMain.MouseWorldPos -----------------------------------------------
function TformMain.MouseWorldPos(X, Y: Integer): TVector;

var
  v: TVector;

begin
  Y := GLSceneViewer.Height - Y;
  if Assigned(moveproxy) then
  begin
    if bMoveOnZ then
    begin
      // broken ?
      // SetVector(v,x,y,0);
       ceneViewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v,moveproxy.Position.X,
      // result)
    end
    else
    begin
      SetVector(v, X, Y, 0);
      GLSceneViewer.Buffer.ScreenVectorIntersectWithPlaneXY(v,
        moveproxy.Position.z, result);
    end;
  end
  else
    SetVector(result, NullVector);
end;

// ----- TformMain.RenderProcessed ---------------------------------------------
procedure TformMain.RenderProcessed;

var
  i, j, iPos: Integer;
  dx, dy, dz, val, ux, uy, uz, mag: double;
  bIsnull, bNullPosition, bNullVector: Boolean;
  sLabel: string;
  dleft, dright, dBack, dForward, dBottom, dtop: double;

begin
  /// kbmData2.DisableControls;
  contourpoints.ClearAll;
  VectorField.ClearAll;
  (*
    for i:=Low(contourbm) to High(contourbm) do
    kbmData2.FreeBookmark(contourbm[i]);
    SetLength(contourbm,0);
    for i:=Low(vectorbm) to High(vectorbm) do
    kbmData2.FreeBookmark(vectorbm[i]);
    SetLength(vectorbm,0);

    with kbmData2 do
    begin
    iPos := RecNo;
    First;
    i:=0;
    j:=0;
    while not eof do
    begin
    dx := FieldByName('x').AsFloat;
    dy := FieldByName('y').AsFloat;
    dz := FieldByName('z').AsFloat;
    bNullPosition := FieldByName('x').IsNull or FIeldByName('y').IsNull or
    FieldByName('z').IsNull;
    bNullVector := FieldByName('ux').IsNull or FIeldByName('uy').IsNull or
    FieldByName('uz').IsNull;

    bIsNull := FieldByName('Value').IsNull;


    if (not bNullPosition) then
    begin
    val := FieldByName('value').AsFloat;
    sLabel := IntToStr(i);

    if not (bIsNull and not usersettings.RenderNullPoints) then
    begin
    SetLength(contourbm,i+1);
    contourbm[i] := GetBookMark;
    ContourPoints.Insert(dx,dy,dz,val,bIsNull,sLabel,contourbm[i]);
    end;

    i := i+1;
    if (not bNullVector) then
    begin
    SetLength(vectorbm,j+1);
    vectorbm[j] := GetBookMark;
    ux := FieldByName('ux').AsFloat;
    uy := FieldByName('uy').AsFloat;
    uz := FieldByName('uz').AsFloat;
    mag := FieldByName('magnitude').AsFloat;
    vectorfield.Insert(dx,dy,dz,mag,ux,uy,uz,IntToStr(j),vectorbm[j]);
    j := j+1;
    end;
    end;

    Next;
    end;
    end;

    // bounding box for scalar
    ContourPoints.CalcBounds(dleft,dright,dBack,dForward,dBottom,dtop);
    with glBoundingBox.XSamplingScale do
    begin
    Origin := dLeft;
    Min := dLeft;
    Max := dRight;
    Step := (dRight-dLeft);
    end;
    with glBoundingBox.YSamplingScale do
    begin
    Origin := dBack;
    Min := dBack;
    Max := dForward;
    Step := (dForward-dBack);
    end;
    with glBoundingBox.ZSamplingScale do
    begin
    Origin := dBottom;
    Min := dBottom;
    Max := dTop;
    Step := (dTop-dBottom);
    end;
    glBoundingBox.Scale.X := usersettings.ScaleX;
    glBoundingBox.Scale.Y := usersettings.ScaleY;
    glBoundingBox.Scale.Z := usersettings.ScaleZ;


    // bounding box for vector
    VectorField.CalcBounds(dleft,dright,dBack,dForward,dBottom,dtop);
    with glBoundingBoxVector.XSamplingScale do
    begin
    Origin := dLeft;
    Min := dLeft;
    Max := dRight;
    Step := (dRight-dLeft);
    end;
    with glBoundingBoxVector.YSamplingScale do
    begin
    Origin := dBack;
    Min := dBack;
    Max := dForward;
    Step := (dForward-dBack);
    end;
    with glBoundingBoxVector.ZSamplingScale do
    begin
    Origin := dBottom;
    Min := dBottom;
    Max := dTop;
    Step := (dTop-dBottom);
    end;
    glBoundingBoxVector.Scale.X := usersettings.ScaleX;
    glBoundingBoxVector.Scale.Y := usersettings.ScaleY;
    glBoundingBoxVector.Scale.Z := usersettings.ScaleZ;

    kbmData2.RecNo := iPos;
    kbmData2.EnableControls;

    usersettings.colpalette.MinValue := contourpoints.MinValue;
    usersettings.colpalette.MaxValue := contourpoints.MaxValue;

    // choose which palette we are using...
    if (usersettings.ColourScaleOptions.ColourScaleType = 0) then
    GenerateHUDColourScale;

    DrawConnectionLines;
    DrawGLPoints;
    DrawGLPipe;
    vectorfield.RenderField;

    contourpoints.ScaleX := usersettings.ScaleX;
    contourpoints.Scaley := userSettings.ScaleY;
    contourpoints.Scalez := usersettings.ScaleZ;
  *)
end;

// ----- TformMain.RenderToBitMap ----------------------------------------------
procedure TformMain.RenderToBitMap(scale: single);

var
  bmp: TBitmap;

begin
  // Rendering to a bitmap requires an existing bitmap, so we create and size
  // a new one }
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;

  bmp.Width := Round(GLSceneViewer.Width * scale);
  bmp.Height := Round(GLSceneViewer.Height * scale);

  // Here we just request a render
  // The second parameter specifies DPI (Dots Per Inch), which is
  // linked to the bitmap's scaling
  // "96" is the "magic" DPI scale of the screen under windows }
  GLSceneViewer.Buffer.RenderToBitMap(bmp, Round(96 * scale));

  if SaveDialog.Execute then
    bmp.SaveToFile(SaveDialog.FileName);
  bmp.Free;
end;

// ----- TformMain.LoadGridMetaXML ---------------------------------------------
procedure TformMain.LoadGridMetaXML(sMetaFile: string);

var
  /// dd,dd2 : TDOMDocument;
  i, i2: Integer;
  dBlankSub: double;
  sPath, sItem: string;
  GLGrid: TGLContourGridData;

begin
  // delete any existing surfer grids
  if (clbSurferGrids.Count > 0) then
    acSurferDeleteAll.Execute;
  // delete any arc/info grids
  if (clbArcInfoGrids.Count > 0) then
    acArcInfoDeleteAll.Execute;
  (*
    // parsing namespace?
    dd := XMLToDOMParser.FileToDOM(sMetaFile);
    dd2 := dd.DOMImplementation.CreateDocument('dummy',nil);
    dd2.Clear;
    XMLDomBuilder.ReferenceNode := dd2;

    try
    XMLStandardDOMReader.Parse(dd);

    // obtain axes
    with XPathExpression do
    begin
    // context node is the namespace aware
    ContextNode := dd2.firstChild;

    // first obtain axes information
    Expression := '/metafile/axes';
    Evaluate;
    if (resultLength > 0) then
    begin
    for i:=0 to Pred(ResultLength) do
    begin
    if resultnode(i).hasChildNodes then
    ObtainAxesProperties(resultNode(i),axes,GLDAxes);
    end;
    end;
    LoadAxesProperties;

    // next obtain Surfer Grid information:
    Expression := '/metafile/surfergrid';
    Evaluate;
    if (resultLength > 0) then
    begin
    // all grids (the ith /metafile/surfergrid)
    for i:= 0 to Pred(ResultLength) do
    begin
    if resultnode(i).hasChildNodes then
    begin
    // obtain the grid file name and blank substitution value. This is required for
    // loading the grid
    ObtainGridFileNameAndBlankSub(resultnode(i),sPath,dBlankSub);
    if FileExists(sPath) then
    begin
    ProcessSurferGrid(sPath,true,dBlankSub);
    sItem := clbSurferGrids.Items[clbSurferGrids.ItemIndex];
    i2 := clbSurferGrids.Items.IndexOf(sItem);
    glGrid := TGLContourGridData(clbSurferGrids.Items.Objects[i2]);
    ObtainGridProperties(resultnode(i),glGrid);
    clbSurferGrids.Checked[i2] := GLGrid.Visible;
    Application.ProcessMessages;
    UpdateSurferScale(i2);
    end;
    end;
    clbSurferGridsClick(nil);
    end;
    end;

    // next obtain ArcInfo Grid Information
    Expression := '/metafile/arcinfogrid';
    Evaluate;
    if (resultLength > 0) then
    begin
    // all grids (the ith /metafile/arcinfogrid)
    for i:= 0 to Pred(ResultLength) do
    begin
    if resultnode(i).hasChildNodes then
    begin
    // obtain the grid file name and blank substitution value. This is required for
    // loading the grid
    ObtainGridFileNameAndBlankSub(resultnode(i),sPath,dBlankSub);
    if FileExists(sPath) then
    begin
    ProcessArcInfoGrid(sPath,true,dBlankSub);
    sItem := clbArcInfoGrids.Items[clbSurferGrids.ItemIndex];
    i2 := clbArcInfoGrids.Items.IndexOf(sItem);
    glGrid := TGLContourGridData(clbArcInfoGrids.Items.Objects[i2]);
    ObtainGridProperties(resultnode(i),glGrid);
    clbArcInfoGrids.Checked[i2] := GLGrid.Visible;
    Application.ProcessMessages;
    UpdateArcInfoScale(i2);
    end;
    end;
    clbArcInfoGridsClick(nil);
    end;
    end;
    end;

    finally
    dd2.DOMImplementation.FreeDocument(dd2);
    dd.DOMImplementation.FreeDocument(dd);
    end;
  *)
end;

// ----- TformMain.SaveGridMetaXML ---------------------------------------------
procedure TformMain.SaveGridMetaXML;

var
  /// Comment : TDOMComment;
  /// DD : TDOMDocument;
  i: Integer;
  /// RElement : TDOMelement;
  Stream: TFileStream;

begin
  (*
    if SaveDialogGridMeta.Execute then
    begin
    DD := DOMImplementation.CreateDocument('metafile',nil);
    // root element
    RElement := DD.DocumentElement;
    Comment := DD.createComment('Grid MetaFile Created ' + DateTimeToStr(Now));
    RElement.AppendChild(Comment);
    // save axes settings
    SaveAxesProperties(DD,RElement,axes,GLDAxes);
    // surfer grids
    for i:=0 to clbSurferGrids.Count-1 do
    SaveGridProperties(DD,Relement,0,clbSurferGrids.Items.Strings[i],
    TGLContourGridData(clbSurferGrids.Items.Objects[i]));
    // arcinfo grids
    for i:=0 to clbArcInfoGrids.Count-1 do
    SaveGridProperties(DD,RElement,1,clbArcInfoGrids.Items.Strings[i],
    TGLContourGridData(clbArcInfoGrids.Items.Objects[i]));
    stream := TFileStream.Create(SaveDialogGridMeta.FileName,
    fmCreate or fmOpenWrite);
    // default to a UTF-8
    DOMToXMLParser.WriteToStream(DD,'UTF-8',Stream);
    Stream.Free;
    DOMImplementation.FreeDocument(dd);
    end;
  *)
end;

// ----- TformMain.ShowHelp ----------------------------------------------------
procedure TformMain.ShowHelp;
// runs the compressed html file (chm) as as a separate program
var
  sFile: string;
begin
  sFile := ExtractFilePath(Application.ExeName) + 'glData.chm';
  if Fileexists(sFile) then
    ShellExecute(formMain.Handle, pchar('open'), pchar(sFile), nil, nil,
      SW_NORMAL)
  else
    MessageDlg('Cannot find glData Help File', mtError, [mbOK], 0)
end;

// ----- TformMain.UpdateCameraFocusPositions ----------------------------------
procedure TformMain.UpdateCameraFocusPositions;

begin
  if sbCamera.Visible then
  begin
    sbCamera.Panels[1].Text :=
      Format('%8.4g ', [GLCamera.AbsolutePosition.v[0] / usersettings.ScaleX]);
    sbCamera.Panels[2].Text :=
      Format('%8.4g ', [GLCamera.AbsolutePosition.v[1] / usersettings.ScaleY]);
    sbCamera.Panels[3].Text :=
      Format('%8.4g ', [GLCamera.AbsolutePosition.v[2] / usersettings.ScaleZ]);
    Application.ProcessMessages;
  end;
  if sbFocus.Visible then
  begin
    sbFocus.Panels[1].Text :=
      Format('%8.4g ', [GLDummyCube.AbsolutePosition.v[0] /
      usersettings.ScaleX]);
    sbFocus.Panels[2].Text :=
      Format('%8.4g ', [GLDummyCube.AbsolutePosition.v[1] /
      usersettings.ScaleY]);
    sbFocus.Panels[3].Text :=
      Format('%8.4g ', [GLDummyCube.AbsolutePosition.v[2] /
      usersettings.ScaleZ]);
    Application.ProcessMessages;
  end;
end;

// ----- TformMain.UpdateNewPosition -------------------------------------------
procedure TformMain.UpdateNewPosition;

var
  sPos: string;

begin
  if acMoveMode.Checked and Assigned(moveproxy) then
  begin
    sPos := '[ X = ' + Format('%8.4g ',
      [moveproxy.Position.X / usersettings.ScaleX]) + ', ' + 'Y = ' +
      Format('%8.4g ', [moveproxy.Position.Y / usersettings.ScaleY]) + ', ' +
      'Z = ' + Format('%8.4g ',
      [moveproxy.Position.z / usersettings.ScaleZ]) + ' ]';
    GLHUDMovePosition.Text := sPos;
  end;
end;

// ----- TformMain.UpdateScaleHUD ----------------------------------------------
procedure TformMain.UpdateScaleHUD;

begin
  GLHUDTextScale.Text := 'Position Scale [ X = ' +
    FloatToStrF(usersettings.ScaleX, ffFixed, 7, 3) + ', Y = ' +
    FloatToStrF(usersettings.ScaleY, ffFixed, 7, 3) + ', Z = ' +
    FloatToStrF(usersettings.ScaleZ, ffFixed, 7, 3) + ' ]';
end;

// ----- TformMain.UpdateStatusOriginal ----------------------------------------
procedure TformMain.UpdateStatusOriginal;

begin
  /// if kbmData1.Active then
  /// sbOriginal.Panels[0].Text := IntToStr(kbmData1.RecordCount);
end;

// ----- Tformmain.UpdateStatusProcessed ---------------------------------------
procedure TformMain.UpdateStatusProcessed;

begin
  /// if kbmData2.Active then
  /// sbProcessed.Panels[0].Text := IntToStr(kbmData2.RecordCount);
end;

// ----- TformMain.acDeleteSurferExecute ---------------------------------------
procedure TformMain.acSurferDeleteExecute(Sender: TObject);

var
  sItem: string;
  i: Integer;

begin
  sItem := clbSurferGrids.Items[clbSurferGrids.ItemIndex];
  i := clbSurferGrids.Items.IndexOf(sItem);
  TGLContourGridData(clbSurferGrids.Items.Objects[i]).Free;
  clbSurferGrids.DeleteSelected;
  clbSurferGridsClick(nil);
  UpdateSurferBounds;
end;

// ----- TformMain.acDeleteSurferUpdate ----------------------------------------
procedure TformMain.acSurferDeleteUpdate(Sender: TObject);
begin
  acSurferDelete.Enabled := (clbSurferGrids.ItemIndex <> -1) and
    (not acSurferAnimation.Checked);
end;

// ----- TformMain.acFocusSurferExecute ----------------------------------------
procedure TformMain.acFocusSurferExecute(Sender: TObject);

var
  dOffSet: double;

begin
  with TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]) do
  begin
    // center the focus cube on the surfergrid minpoint
    GLDummyCube.Position.X := grid.Position.X + 0.5 *
      usersettings.ScaleX * xrange;
    GLDummyCube.Position.Y := grid.Position.Y + 0.5 *
      usersettings.ScaleY * yrange;
    GLDummyCube.Position.z := grid.Position.z + 0.5 *
      usersettings.ScaleZ * zrange;
    dOffSet := usersettings.ScaleX * xrange;

    if (usersettings.ScaleY * yrange) > dOffSet then
      dOffSet := usersettings.ScaleY * yrange;

    if (usersettings.ScaleZ * zrange) > dOffSet then
      dOffSet := usersettings.ScaleZ * zrange;

    GLCamera.Position.z := GLDummyCube.Position.z + dOffSet;
    // this could be cleaned up and made user customisable
    GLCamera.DepthOfView := 5 * dOffSet;
    GLCamera.TransformationChanged;
    UpdateCameraFocusPositions;
  end;
end;

// ----- TformMain.acFocusSurferUpdate -----------------------------------------
procedure TformMain.acFocusSurferUpdate(Sender: TObject);
begin
  acFocusSurfer.Enabled := (clbSurferGrids.ItemIndex <> -1) and
    (not acSurferAnimation.Checked);
end;

// ----- TformMain.acSurferDeleteAllUpdate -------------------------------------
procedure TformMain.acSurferDeleteAllUpdate(Sender: TObject);

begin
  acSurferDeleteAll.Enabled := (clbSurferGrids.Count > 0) and
    (not acSurferAnimation.Checked);
end;

// ----- TformMain.acSurferDeleteAllExecute ------------------------------------
procedure TformMain.acSurferDeleteAllExecute(Sender: TObject);

begin
  while clbSurferGrids.Count > 0 do
  begin
    clbSurferGrids.ItemIndex := 0;
    acSurferDelete.Execute;
  end;
end;

// ----- TformMain.acImportArcInfoExecute --------------------------------------
procedure TformMain.acImportArcInfoExecute(Sender: TObject);

begin
  if OpenArcInfo.Execute then
  begin
    Application.ProcessMessages;
    ProcessArcInfoGrid(OpenArcInfo.FileName, false, 0.0);
  end;
end;

// ----- TformMain.clbArcInfoGridsClick -----------------------------------------
procedure TformMain.clbArcInfoGridsClick(Sender: TObject);

// ~~~~~ PostIntToGrid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  procedure PostIntToGrid(sProperty: string; iInt: Integer);

  begin
    (*
      with kbmArcInfoGridInfo do
      begin
      Append;
      FieldByName('Property').AsString := sProperty;
      FieldByName('Integer').AsInteger := iInt;
      Post;
      end;
    *)
  end;
// ~~~~~ PostFloatToGrid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  procedure PostFloatToGrid(sProperty: string; dFloat: double);

  begin
    (*
      with kbmArcInfoGridInfo do
      begin
      Append;
      FieldByName('Property').AsString := sProperty;
      FieldByName('Float').AsFloat:= dFloat;
      Post;
      end;
    *)
  end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
begin
  if (clbArcInfoGrids.ItemIndex <> -1) then
  begin
    tsNoArcInfo.TabVisible := false;
    tsArcInfoDisplay.TabVisible := true;
    tsArcInfoGridInfo.TabVisible := true;
    if (pcArcInfoDetails.Tag > 0) then
      pcArcInfoDetails.ActivePageIndex := pcArcInfoDetails.Tag
    else
      pcArcInfoDetails.ActivePageIndex := 1;

    iArcInfoIndex := clbArcInfoGrids.Items.IndexOf
      (clbArcInfoGrids.Items[clbArcInfoGrids.ItemIndex]);

    with TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]) do
    begin
      /// kbmArcInfoGridInfo.DisableControls;
      /// kbmArcInfoGridInfo.EmptyTable;
      /// kbmArcInfoGridInfo.First;
      PostIntToGrid('X Grid Nodes', NX);
      PostIntToGrid('Y Grid Nodes', NY);
      PostIntToGrid('Total Nodes', NX * NY);
      PostIntToGrid('Blank Nodes', nblanks);
      ColourMode := ColourMode; // force tricount
      PostIntToGrid('Triangle Count', TriangleCount);
      PostFloatToGrid('X Low', xlo);
      PostFloatToGrid('X High', xhi);
      PostFloatToGrid('X Range', xrange);
      PostFloatToGrid('X Step', dx);
      PostFloatToGrid('Y Low', ylo);
      PostFloatToGrid('Y High', yhi);
      PostFloatToGrid('Y Range', yrange);
      PostFloatToGrid('Y Step', dy);
      PostFloatToGrid('Z Low', zlo);
      PostFloatToGrid('Z High', zhi);
      PostFloatToGrid('Z Range', zrange);
      /// kbmArcInfoGridInfo.EnableControls;
      // load display obtions
      cbArcInfoColourMode.ItemIndex := ColourMode;
      cbArcInfoPolygonMode.ItemIndex := PolygonMode;
      /// geArcInfoAlpha.Value := Alpha;
      cbxArcInfoTwoSided.Checked := TwoSided;
      // base map
      ebArcInfoGridName.Text := GridName;
      cbxArcInfoBaseMap.Checked := EnableBaseMap;
      ebArcInfoBaseMap.Text := BaseMapPath;
      /// geTileArcInfoX.Value := TileX;
      /// geTileArcInfoY.Value := TileY;
    end;
  end
  else
  begin
    /// kbmArcInfoGridInfo.EmptyTable;
    tsNoArcInfo.TabVisible := true;
    tsArcInfoDisplay.TabVisible := false;
    tsArcInfoGridInfo.TabVisible := false;
    pcArcInfoDetails.ActivePageIndex := 0;
  end;
end;
// =============================================================================

// ----- TformMain.cbSurferPolygonModeChange -----------------------------------
procedure TformMain.cbSurferPolygonModeChange(Sender: TObject);

begin
  TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).PolygonMode :=
    cbSurferPolygonMode.ItemIndex;
end;

// ----- TformMain.acSaveGridMetaExecute ---------------------------------------
procedure TformMain.acSaveGridMetaExecute(Sender: TObject);

begin
  SaveGridMetaXML;
end;

// ----- TformMain.acLoadGridMetaExecute ---------------------------------------
procedure TformMain.acLoadGridMetaExecute(Sender: TObject);
begin
  if OpenDialogGridMeta.Execute then
    LoadGridMetaXML(OpenDialogGridMeta.FileName);
end;

// ----- TformMain.acSurferDefaultExecute --------------------------------------
procedure TformMain.acSurferDefaultExecute(Sender: TObject);

begin
  UpdateSurferGrid(iSurferIndex);
  clbSurferGridsClick(nil);
end;

// ----- TformMain.acSurferDefaultUpdate ---------------------------------------
procedure TformMain.acSurferDefaultUpdate(Sender: TObject);
begin
  acSurferDefault.Enabled := (clbSurferGrids.ItemIndex <> -1) and
    (not acSurferAnimation.Checked);
end;

// ----- TformMain.acSurferDefaultAllUpdate ------------------------------------
procedure TformMain.acSurferDefaultAllUpdate(Sender: TObject);

begin
  acSurferDefaultAll.Enabled := (clbSurferGrids.Items.Count > 0) and
    (not acSurferAnimation.Checked);
end;

// ----- TformMain.acDefaultAllSurferExecute -----------------------------------
procedure TformMain.acSurferDefaultAllExecute(Sender: TObject);

var
  i: Integer;

begin
  for i := 0 to clbSurferGrids.Count - 1 do
  begin
    clbSurferGrids.ItemIndex := i;
    acSurferDefault.Execute;
  end;
end;

// ----- TformMain.cbSurferColourModeChange ------------------------------------
procedure TformMain.cbSurferColourModeChange(Sender: TObject);

begin
  TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).ColourMode :=
    cbSurferColourMode.ItemIndex;
end;

procedure TformMain.pcSurferDetailsChange(Sender: TObject);
begin
  pcSurferDetails.Tag := pcSurferDetails.ActivePageIndex;
end;

// ----- TformMain.cbxSurferTwoSidedClick --------------------------------------
procedure TformMain.cbxSurferTwoSidedClick(Sender: TObject);

begin
  TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).TwoSided :=
    cbxSurferTwoSided.Checked;
end;

// ----- TformMain.ebSurferCLRFileChange ---------------------------------------
procedure TformMain.ebSurferBaseMapChange(Sender: TObject);
begin
  if not Fileexists(ebSurferBaseMap.Text) then
  begin
    ebSurferBaseMap.Color := clInfoBk;
    cbxSurferBaseMap.Checked := false;
  end
  else
  begin
    ebSurferBaseMap.Color := clWindow;
    TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).BaseMapPath
      := ebSurferBaseMap.Text;
  end;
end;

// ----- TformMain.cbxSurferBaseMapClick ---------------------------------------
procedure TformMain.cbxSurferBaseMapClick(Sender: TObject);
begin
  TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).EnableBaseMap
    := cbxSurferBaseMap.Checked;
end;

// ----- TformMain.acSaveGridMetaUpdate ----------------------------------------
procedure TformMain.acSaveGridMetaUpdate(Sender: TObject);
begin
  acSaveGridMeta.Enabled := true;
end;

// ----- TformMain.acArcInfoDeleteAllUpdate ------------------------------------
procedure TformMain.acArcInfoDeleteAllUpdate(Sender: TObject);
begin
  acArcInfoDeleteAll.Enabled := (clbArcInfoGrids.Count > 0);
end;

// ----- TformMain.acArcInfoDeleteAllExecute -----------------------------------
procedure TformMain.acArcInfoDeleteAllExecute(Sender: TObject);
begin
  while clbArcInfoGrids.Count > 0 do
  begin
    clbArcInfoGrids.ItemIndex := 0;
    acArcInfoDelete.Execute;
  end;
end;

// ----- TformMain.acArcInfoDeleteExecute --------------------------------------
procedure TformMain.acArcInfoDeleteExecute(Sender: TObject);

var
  sItem: string;
  i: Integer;

begin
  sItem := clbArcInfoGrids.Items[clbArcInfoGrids.ItemIndex];
  i := clbArcInfoGrids.Items.IndexOf(sItem);
  TGLContourGridData(clbArcInfoGrids.Items.Objects[i]).Free;
  clbArcInfoGrids.DeleteSelected;
  clbArcInfoGridsClick(nil);
end;

// ----- TformMain.acArcInfoDeleteUpdate ---------------------------------------
procedure TformMain.acArcInfoDeleteUpdate(Sender: TObject);
begin
  acArcInfoDelete.Enabled := (clbArcInfoGrids.ItemIndex <> -1);
end;

// ----- TformMain.cbArcInfoColourModeChange -----------------------------------
procedure TformMain.cbArcInfoColourModeChange(Sender: TObject);
begin
  TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).ColourMode :=
    cbArcInfoColourMode.ItemIndex;
end;

// ----- TformMain.cbArcInfoPolygonModeChange ----------------------------------
procedure TformMain.cbArcInfoPolygonModeChange(Sender: TObject);
begin
  TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).PolygonMode
    := cbArcInfoPolygonMode.ItemIndex;
end;

// ----- TformMain.cbxArcInfoTwoSidedClick -------------------------------------
procedure TformMain.cbxArcInfoTwoSidedClick(Sender: TObject);
begin
  TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).TwoSided :=
    cbxArcInfoTwoSided.Checked;
end;

// ----- TformMain.bLoadArcInfoBaseMapClick ------------------------------------
procedure TformMain.bLoadArcInfoBaseMapClick(Sender: TObject);
begin
  with TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]) do
  begin
    if OpenTexture.Execute then
      ebArcInfoBaseMap.Text := OpenTexture.FileName;
  end;
end;

// ----- TformMain.bLoadSurferBaseMapClick -------------------------------------
procedure TformMain.bLoadSurferBaseMapClick(Sender: TObject);
begin
  with TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]) do
  begin
    if OpenTexture.Execute then
      ebSurferBaseMap.Text := OpenTexture.FileName;
  end;
end;

// ----- TformMain.ebArcInfoBaseMapChange --------------------------------------
procedure TformMain.ebArcInfoBaseMapChange(Sender: TObject);
begin
  if not Fileexists(ebArcInfoBaseMap.Text) then
  begin
    ebArcInfoBaseMap.Color := clInfoBk;
    cbxArcInfoBaseMap.Checked := false;
  end
  else
  begin
    ebArcInfoBaseMap.Color := clWindow;
    TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).BaseMapPath
      := ebArcInfoBaseMap.Text;
  end;
end;

// ----- TformMain.cbxArcInfoBaseMapClick --------------------------------------
procedure TformMain.cbxArcInfoBaseMapClick(Sender: TObject);
begin
  TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).EnableBaseMap
    := cbxArcInfoBaseMap.Checked;
end;

// ----- TformMain.clbArcInfoGridsClickCheck -----------------------------------
procedure TformMain.clbArcInfoGridsClickCheck(Sender: TObject);
var
  i: Integer;
  sItem: string;

begin
  sItem := clbArcInfoGrids.Items[clbArcInfoGrids.ItemIndex];
  i := clbArcInfoGrids.Items.IndexOf(sItem);
  TGLContourGridData(clbArcInfoGrids.Items.Objects[i]).Visible :=
    clbArcInfoGrids.Checked[i];
end;

// ----- TformMain.acArcInfoDefaultExecute -------------------------------------
procedure TformMain.acArcInfoDefaultExecute(Sender: TObject);
begin
  UpdateArcInfoGrid(iArcInfoIndex);
  clbSurferGridsClick(nil);
end;

// ----- TformMain.acArcInfoDefaultUpdate --------------------------------------
procedure TformMain.acArcInfoDefaultUpdate(Sender: TObject);
begin
  acArcInfoDefault.Enabled := (clbArcInfoGrids.ItemIndex <> -1);
end;

// ----- TformMain.acArcInfoDefaultAllExecute ----------------------------------
procedure TformMain.acArcInfoDefaultAllExecute(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to clbArcInfoGrids.Count - 1 do
  begin
    clbArcInfoGrids.ItemIndex := i;
    acArcInfoDefault.Execute;
  end;
end;

// ----- TformMain.acArcInfoDefaultAllUpdate -----------------------------------
procedure TformMain.acArcInfoDefaultAllUpdate(Sender: TObject);
begin
  acArcInfoDefaultAll.Enabled := (clbArcInfoGrids.Items.Count > 0);
end;

// ----- TformMain.acGeothermalGridExecute -------------------------------------
// temp testing methods for geothermal grids
procedure TformMain.acGeothermalGridExecute(Sender: TObject);

var
  bl: TStringlist;
  gf: TformGeoGrid;
  i: Integer;

begin
  bl := TStringlist.Create;
  geosimgrid.ClearAll;
  geosimgrid.GridLayers.ClearLayers; // override and extend gridlayers

  /// kbmGeothermalLayers.EmptyTable;
  /// kbmGeothermalLayers.First;
  /// kbmGeothermalLayers.DisableControls;

  gf := TformGeoGrid.Create(nil);
  gf.gm := gmTOUGH;
  if (gf.ShowModal = mrOK) then
  begin
    Application.ProcessMessages;
    Screen.Cursor := crHourGlass;
    gf.SaveGrid(geosimgrid, bl);
    // geosimgrid.GridLayers.RenderVertices;
    for i := 0 to geosimgrid.GridLayers.LayerList.Count - 1 do
    begin
      /// kbmGeothermalLayers.Append;

      with geosimgrid.GridLayers.GLLayer[i] do
      begin
        (*
          kbmGeothermalLayers.FieldByName('Layer').AsString := LayerName;
          kbmGeothermalLayers.FieldByName('Elevation').AsFloat := Elevation;
          kbmGeothermalLayers.FieldByName('Thickness').AsFloat := Thickness;
          kbmGeothermalLayers.FieldByName('Show Block').AsBoolean := false;
          kbmGeothermalLayers.FieldByName('Show Block').AsBoolean := false;
          kbmGeothermalLayers.FieldByName('Block Top').AsBoolean := true;
          kbmGeothermalLayers.FieldByName('Block Bottom').AsBoolean := true;
          kbmGeothermalLayers.FieldByName('Block Outside').AsBoolean := true;
          kbmGeothermalLayers.FieldByName('Alpha').AsFloat := 1.0;
          kbmGeothermalLayers.FieldByName('Top Lines').AsBoolean := false;
          kbmGeothermalLayers.FieldByName('Base Lines').AsBoolean := false;
          kbmGeothermalLayers.FieldByName('Vert Lines').AsBoolean := false;
          kbmGeothermalLayers.Post;
        *)
      end;
      TGLGridLayer(geosimgrid.GridLayers.LayerList.Objects[i]).RenderAllBlocks;
    end;

    // sort in descending elevation order - for transparency
    /// kbmGeothermalLayers.SortOn('Elevation:D',[]);
    /// kbmGeothermalLayers.EnableControls;

    GLDummyCube.Position.X := geosimgrid.Vertex[0].LocationE *
      usersettings.ScaleX;
    GLDummyCube.Position.Y := geosimgrid.Vertex[0].LocationN *
      usersettings.ScaleY;
    GLDummyCube.Position.z := 0.0;
    GLCamera.TransformationChanged;
    Screen.Cursor := crDefault;
  end;
  gf.Release;
  bl.Free;
end;

// ----- TformMain.cbDisplayChange ---------------------------------------------
procedure TformMain.cbDisplayChange(Sender: TObject);
begin
  geosimgrid.GridLayers.ColourMode := cbDisplay.ItemIndex;

  if (usersettings.ColourScaleOptions.ColourScaleType = 4) then
    GenerateGeoColourScale;
end;

// ----- TformMain.kbmGeothermalLayersAfterPost --------------------------------
procedure TformMain.kbmGeothermalLayersAfterPost(DataSet: TDataSet);

var
  iIndex: Integer;
  sLayer: string;

begin
  if bProcessing then
    Exit;

  /// sLayer := kbmGeothermalLayers.FIeldByName('Layer').AsString;
  iIndex := geosimgrid.GridLayers.LayerList.IndexOf(sLayer);

  with TGLGridLayer(geosimgrid.GridLayers.GLLayer[iIndex]) do
  begin
    (*
      if kbmGeothermalLayers.FieldByName('Show Block').AsBoolean then
      ShowAllBlocks
      else
      HideAllBlocks;

      BlockTop := kbmGeothermalLayers.FieldByName('Block Top').AsBoolean;
      BlockBottom := kbmGeothermalLayers.FieldByName('Block Bottom').AsBoolean;
      BlockOutside := kbmGeothermalLayers.FieldByName('Block Outside').AsBoolean;
      Alpha := kbmGeothermalLayers.FieldByName('Alpha').AsFloat;
    *)
  end;
end;

// ----- TformMain.dbgGeothermalLayersDrawColumnCell ---------------------------
procedure TformMain.dbgGeothermalLayersDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

var
  a, b: Integer;
  tt: string;

begin
  if (gdFocused in State) then
    dbgGeothermalLayers.DefaultDrawColumnCell(Rect, DataCol, Column, State)
  else
  begin
    if (Column.Field.DataType = ftBoolean) then
    begin
      if Column.Field.AsBoolean then
        dbgGeothermalLayers.Canvas.Brush.Color := clInfoBk
      else
        dbgGeothermalLayers.Canvas.Brush.Color := clWhite;
      dbgGeothermalLayers.Canvas.FillRect(Rect);

      tt := trim(Column.Field.Text);
      case Column.Alignment of
        taLeftJustify:
          dbgGeothermalLayers.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, tt);
        taCenter:
          begin
            a := dbgGeothermalLayers.Canvas.TextWidth(tt) div 2;
            b := (Rect.Left + Rect.Right) div 2;
            dbgGeothermalLayers.Canvas.TextOut(b - a, Rect.Top + 2, tt);
          end;
        taRightJustify:
          begin
            a := dbgGeothermalLayers.Canvas.TextWidth(tt);
            dbgGeothermalLayers.Canvas.TextOut(Rect.Right - a - 2,
              Rect.Top + 2, tt);
          end;
      end;

    end
    else
      dbgGeothermalLayers.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

// ----- TformMain.cbxGeoAutoScaleClick ----------------------------------------
procedure TformMain.cbxGeoAutoScaleClick(Sender: TObject);

begin
  geosimgrid.GridLayers.ManualLimits := not cbxGeoAutoScale.Checked;
  ebgeoMinValue.Enabled := geosimgrid.GridLayers.ManualLimits;
  ebgeoMaxValue.Enabled := geosimgrid.GridLayers.ManualLimits;
  if (usersettings.ColourScaleOptions.ColourScaleType = 4) then
    GenerateGeoColourScale;
end;

// ----- TformMain.ebgeoMinValueExit -------------------------------------------
procedure TformMain.ebgeoMinValueExit(Sender: TObject);

var
  s: string;

begin
  try
    s := ebgeoMinValue.Text;
    // now premultiply the number if no mantissa present
    if Pos('E', Uppercase(s)) = 1 then
      insert('1', s, 1);
    if Pos('-E', Uppercase(s)) = 1 then
      insert('1', s, 2);
    geosimgrid.GridLayers.MinPaletteValue := StrToFloat(ebgeoMinValue.Text);
    GenerateGeoColourScale;
  except
    ebgeoMinValue.Text := FloatToStr(geosimgrid.GridLayers.MinPaletteValue);
  end;
end;

// ----- TformMain.ebgeoMaxValueExit -------------------------------------------
procedure TformMain.ebgeoMaxValueExit(Sender: TObject);
var
  s: string;

begin
  try
    s := ebgeoMinValue.Text;
    // now premultiply the number if no mantissa present
    if Pos('E', Uppercase(s)) = 1 then
      insert('1', s, 1);
    if Pos('-E', Uppercase(s)) = 1 then
      insert('1', s, 2);
    geosimgrid.GridLayers.MaxPaletteValue := StrToFloat(ebgeoMaxValue.Text);
    GenerateGeoColourScale;
  except
    ebgeoMaxValue.Text := FloatToStr(geosimgrid.GridLayers.MaxPaletteValue);
  end;
end;

// ----- TformMain.acSortGeoLayersUpdate ---------------------------------------
procedure TformMain.acSortGeoLayersUpdate(Sender: TObject);
begin
  /// acSortGeoLayers.Enabled := kbmGeothermalLayers.RecordCount > 0;
end;

// ----- TformMain.acSortGeoLayersExecute --------------------------------------
procedure TformMain.acSortGeoLayersExecute(Sender: TObject);

var
  sf: TformSort;
  sl: TStringlist;
  /// buffer : TkbmMemTable;
  i: Integer;

begin
  sl := TStringlist.Create;
  (*
    for i:=0 to kbmGeothermalLayers.FieldCount-1 do
    sl.Add(kbmGeothermalLayers.Fields[i].FieldName);
  *)
  sf := TformSort.Create(nil);
  sf.LoadFields(sl);
  if sf.ShowModal = mrOK then
  begin
    (*
      buffer := TkbmMemTable.Create(nil);
      buffer.LoadFromDataSet(kbmGeothermalLayers,[mtcpoStructure]);
      buffer.SortOn(sf.SortString,[]);
      bProcessing := true;     // disable auto editing
      kbmGeothermalLayers.DisableControls;
      kbmGeothermalLayers.Close;
      kbmGeothermalLayers.LoadFromDataSet(buffer,[mtcpoStructure]);
      kbmGeothermalLayers.EnableControls;
      RenderProcessed;
      bProcessing := false;

      buffer.Free;
    *)
  end;
  sl.Free;
  sf.Release;
end;

// ----- TformMain.bGeoSyncLimitsClick -----------------------------------------
procedure TformMain.bGeoSyncLimitsClick(Sender: TObject);
begin
  ebgeoMinValue.Text := FloatToStr(geosimgrid.colpalette.MinValue);
  ebgeoMaxValue.Text := FloatToStr(geosimgrid.colpalette.MaxValue);
end;

// ----- TformMain.geTileSurferXExit -------------------------------------------
procedure TformMain.geTileSurferXExit(Sender: TObject);
begin
  /// TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).TileX :=
  /// geTileSurferX.Value;
end;

// ----- TformMain.geTileSurferYExit -------------------------------------------
procedure TformMain.geTileSurferYExit(Sender: TObject);
begin
  /// TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).TileY :=
  /// geTileSurferY.Value;
end;

// ----- TformMain.geTileArcInfoXExit ------------------------------------------
procedure TformMain.geTileArcInfoXExit(Sender: TObject);
begin
  /// TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).TileX :=
  /// geTileArcInfoX.Value;
end;

// ----- TformMain.geTileArcInfoYExit ------------------------------------------
procedure TformMain.geTileArcInfoYExit(Sender: TObject);
begin
  /// TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).TileY :=
  /// geTileArcInfoY.Value;
end;

// ----- TformMain.geSurferAlphaExit -------------------------------------------
procedure TformMain.geSurferAlphaExit(Sender: TObject);
begin
  /// TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).Alpha :=
  /// geSurferAlpha.Value;
end;

// ----- TformMain.geArcInfoAlphaExit ------------------------------------------
procedure TformMain.geArcInfoAlphaExit(Sender: TObject);
begin
  /// TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).Alpha :=
  /// geArcInfoAlpha.Value;
end;

// ----- TformMain.acColourToolExecute -----------------------------------------
procedure TformMain.acColourToolExecute(Sender: TObject);
var
  frmColourEdit: TformColourEdit;

begin
  frmColourEdit := TformColourEdit.Create(nil);
  frmColourEdit.ShowModal;
  frmColourEdit.Release;
end;

// ----- TformMain.kbmData1AfterInsert -----------------------------------------
procedure TformMain.kbmData1AfterInsert(DataSet: TDataSet);
begin
  UpdateStatusOriginal;
end;

// ----- TformMain.kbmData1AfterDelete -----------------------------------------
procedure TformMain.kbmData1AfterDelete(DataSet: TDataSet);
begin
  UpdateStatusOriginal;
end;

// ----- TformMain.kbmData1AfterPost -------------------------------------------
procedure TformMain.kbmData1AfterPost(DataSet: TDataSet);
begin
  UpdateStatusOriginal;
end;

// ----- TformMain.acBenchmarkExecute ------------------------------------------
procedure TformMain.acBenchmarkExecute(Sender: TObject);

begin
  if formBenchMark.Visible then
    formBenchMark.Hide
  else
    formBenchMark.Show;
end;

// ----- TformMain.pcMainChange ------------------------------------------------
procedure TformMain.pcMainChange(Sender: TObject);
begin
  if acSurferAnimation.Checked then
    acSurferAnimation.Execute;
end;

// ----- TformMain.acImportSurferUpdate ----------------------------------------
procedure TformMain.acImportSurferUpdate(Sender: TObject);
begin
  acImportSurfer.Enabled := not acSurferAnimation.Checked;
end;

// ----- TformMain.acArcInfoAnimationExecute -----------------------------------
procedure TformMain.acArcInfoAnimationExecute(Sender: TObject);
begin
  MessageDlg('ArcInfo Animation', mtWarning, [mbOK], 0);
end;

// ----- TformMain.acArcInfoAnimationUpdate ------------------------------------
procedure TformMain.acArcInfoAnimationUpdate(Sender: TObject);
begin
  acArcInfoAnimation.Enabled := (clbArcInfoGrids.ItemIndex <> -1) and
    (clbArcInfoGrids.Count > 1);
end;

// ----- TformMain.acOpenGLContextExecute --------------------------------------
procedure TformMain.acOpenGLContextExecute(Sender: TObject);

var
  formOpenGL: TformOpenGL;
begin
  formOpenGL := TformOpenGL.Create(nil);
  with formOpenGL do
  begin
    GetInfoFrom(GLSceneViewer.Buffer);
    ShowModal;
    Release;
  end;
end;

// ----- TformMain.acBoundingBoxExecute ----------------------------------------
procedure TformMain.acBoundingBoxExecute(Sender: TObject);
begin
  glBoundingBox.Visible := acBoundingBox.Checked;
end;

// ----- TformMain.acBoundingBoxVectorExecute ----------------------------------
procedure TformMain.acBoundingBoxVectorExecute(Sender: TObject);
begin
  glBoundingBoxVector.Visible := acBoundingBoxVector.Checked;
end;

// ----- TformMain.ebSurferGridNameExit ----------------------------------------
procedure TformMain.ebSurferGridNameExit(Sender: TObject);

begin
  TGLContourGridData(clbSurferGrids.Items.Objects[iSurferIndex]).GridName :=
    ebSurferGridName.Text;
end;

// ----- TformMain.ebArcInfoGridNameExit ---------------------------------------
procedure TformMain.ebArcInfoGridNameExit(Sender: TObject);
begin
  TGLContourGridData(clbArcInfoGrids.Items.Objects[iArcInfoIndex]).GridName :=
    ebArcInfoGridName.Text;
end;

// ----- TformMain.acBenchmarkUpdate -------------------------------------------
procedure TformMain.acBenchmarkUpdate(Sender: TObject);
begin
  acBenchmark.Checked := formBenchMark.Visible;
end;

// ----- TformMain.acLocatorExecute --------------------------------------------
procedure TformMain.acLocatorExecute(Sender: TObject);

begin
  {
    if formLocator.Visible then
    formLocator.Hide
    else
    formLocator.Show;
  }
end;

// ----- TformMain.acLocatorUpdate ---------------------------------------------
procedure TformMain.acLocatorUpdate(Sender: TObject);
begin
  // acLocator.Checked := formLocator.Visible;
end;

// ----- TformMain.acAxesExecute -----------------------------------------------
procedure TformMain.acAxesExecute(Sender: TObject);

begin
  if formAxes.Visible then
    formAxes.Hide
  else
    formAxes.Show;
end;

// ----- TformMain.acAxesUpdate ------------------------------------------------
procedure TformMain.acAxesUpdate(Sender: TObject);

begin
  acAxes.Checked := formAxes.Visible;
end;

(*
  // ----- TformMain.DomImplementationRequestXPathVariable -----------------------
  procedure TformMain.DomImplementationRequestXPathVariable(
  const sender: TXPathExpression; const namespaceURI,
  localName: WideString; var value: TdomXPathCustomResult);
  begin
  if (NamespaceURI = '') and (Localname = 'XDOM-version') then
  Value := TdomXPathStringResult.Create(DomImplementation.XdomVersion);
  end;
*)
// ----- TformMain.acWorldViewUpdate -------------------------------------------
procedure TformMain.acWorldViewUpdate(Sender: TObject);

begin
  acWorldView.Enabled := false;
  // acWorldView.Checked := formWorld.Visible;
end;

// ----- TformMain.acWorldViewExecute ------------------------------------------
procedure TformMain.acWorldViewExecute(Sender: TObject);

begin
  if formWorld.Visible then
    formWorld.Hide
  else
    formWorld.Show;
end;

// =============================================================================
procedure TformMain.acGeothermalTETRADExecute(Sender: TObject);

var
  bl: TStringlist;
  gf: TformGeoGrid;
  i: Integer;
  xmin, xmax, ymin, ymax, zmin, zmax, isovalue: double;
  NX, NY, NZ, isoMode: Integer;

begin

  bl := TStringlist.Create;
  geosimgrid.ClearAll;
  geosimgrid.GridLayers.ClearLayers; // override and extend gridlayers
  (*
    ///  kbmGeothermalLayers.EmptyTable;
    ///  kbmGeothermalLayers.First;
    ///  kbmGeothermalLayers.DisableControls;
  *)
  gf := TformGeoGrid.Create(nil);
  gf.gm := gmTETRAD;

  if (gf.ShowModal = mrOK) then
  begin
    Application.ProcessMessages;
    Screen.Cursor := crHourGlass;

    gf.SaveGrid(geosimgrid, bl);
    (*
      //    geosimgrid.GridLayers.RenderVertices;
      for i:= 0 to geosimgrid.GridLayers.LayerList.Count-1 do
      begin
      kbmGeothermalLayers.Append;
      with geosimgrid.GridLayers.GLLayer[i] do
      begin
      kbmGeothermalLayers.FieldByName('Layer').AsString := LayerName;
      kbmGeothermalLayers.FieldByName('Show Block').AsBoolean := false;
      kbmGeothermalLayers.FieldByName('Show Block').AsBoolean := false;
      kbmGeothermalLayers.FieldByName('Block Top').AsBoolean := true;
      kbmGeothermalLayers.FieldByName('Block Bottom').AsBoolean := true;
      kbmGeothermalLayers.FieldByName('Block Outside').AsBoolean := true;
      kbmGeothermalLayers.FieldByName('Alpha').AsFloat := 1.0;

      // should be moved into the extrusion class directly...
      kbmGeothermalLayers.FieldByName('Top Lines').AsBoolean := false;
      kbmGeothermalLayers.FieldByName('Base Lines').AsBoolean := false;
      kbmGeothermalLayers.FieldByName('Vert Lines').AsBoolean := false;
      kbmGeothermalLayers.Post;
      end;
      TGLGridLayer(geosimgrid.GridLayers.LayerList.Objects[i]).RenderAllBlocks;

      end;
      Screen.Cursor := crDefault;
      kbmGeothermalLayers.EnableControls;

      for i:=1 to gf.iRow do
      begin
      clbRows.Items.Add(IntToStr(i));
      clbRows.Checked[i-1] := true;
      geosimgrid.GridLayers.RowList.Add(IntToStr(i));
      end;

      for i:=1 to gf.iCol do
      begin
      clbColumns.Items.Add(IntToStr(i));
      clbColumns.Checked[i-1] := true;
      geosimgrid.GridLayers.ColumnList.Add(IntToSTr(i));
      end;
    *)
  end;

  with geosimgrid do
  begin
    EvaluateGridBounds;
    SetupIsoArray;
    GenerateISOSurfaceMC(tbIso.Position);
    freeform.StructureChanged;
  end;

  OptimiseIsoSurfaceMesh;
  geosimgrid.freeform.StructureChanged;

  gf.Release;
  bl.Free;

end;

// ----- TformMain.cbxInActiveBlocksClick --------------------------------------
procedure TformMain.cbxInActiveBlocksClick(Sender: TObject);
begin
  geosimgrid.GridLayers.ShowInActiveBlocks := cbxInActiveBlocks.Checked;
end;

// ----- TformMain.clbRowsClickCheck -------------------------------------------
procedure TformMain.clbColumnsClickCheck(Sender: TObject);

var
  i: Integer;

begin
  geosimgrid.GridLayers.ColumnList.Clear;
  for i := 0 to clbColumns.Items.Count - 1 do
  begin
    if clbColumns.Checked[i] then
      geosimgrid.GridLayers.ColumnList.Add(IntToStr(i + 1));
  end;

  geosimgrid.GridLayers.ResetAllBlocks;
end;

// ----- TformMain.clbRowsClickCheck -------------------------------------------
procedure TformMain.clbRowsClickCheck(Sender: TObject);
var
  i: Integer;

begin
  geosimgrid.GridLayers.RowList.Clear;
  for i := 0 to clbRows.Items.Count - 1 do
  begin
    if clbRows.Checked[i] then
      geosimgrid.GridLayers.RowList.Add(IntToStr(i + 1));
  end;
  geosimgrid.GridLayers.ResetAllBlocks;
end;

// ----- TformMain.geISOExit ---------------------------------------------------
procedure TformMain.geISOExit(Sender: TObject);
var
  cv: TColorVector;

begin
  /// geosimgrid.GenerateIsoSurfaceMC(geIso.Value);
  // usersettings.geocolpalette.

  /// cv := usersettings.geocolpalette.GetColourVector(geISO.Value,true);

  with geosimgrid.freeform.Material do
  begin
    FrontProperties.Ambient.AsWinColor := ConvertColorVector(cv);
    FrontProperties.Diffuse.AsWinColor := ConvertColorVector(cv);
    FrontProperties.Specular.AsWinColor := ConvertColorVector(cv);
    BackProperties.Ambient.AsWinColor := ConvertColorVector(cv);
    BackProperties.Diffuse.AsWinColor := ConvertColorVector(cv);
    BackProperties.Specular.AsWinColor := ConvertColorVector(cv);
  end;

  geosimgrid.freeform.StructureChanged;
  OptimiseIsoSurfaceMesh;
  geosimgrid.freeform.StructureChanged;
end;

// ----- TformMain.tbIsoChange -------------------------------------------------
procedure TformMain.tbIsoChange(Sender: TObject);
begin
  /// geIso.Value := tbIso.Position;
  /// geIsoExit(nil);
end;

// =============================================================================
end.
