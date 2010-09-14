{DONE Deleting Conections}
{DONE Deleting Nodes}
{DONE Cross Delphi/Lazarus Development}
{DONE Material Preview}
{TODO Togle Realtime Expresion Preview}
{DONE Refresh All}

unit GL3xMaterialEditor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I GLScene.inc}

uses
  // vcl
{$IFDEF MSWINDOWS}
  Windows,
  GLWin32Context,
{$ENDIF}
{$IFDEF UNIX}
  GLWidgetContext,
{$ENDIF}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
{$IFDEF FPC}
  LCLType,
  ComponentEditors,
  PropEdits,
  FileUtil,
  LResources,
  LMessages,
{$ELSE}
  Messages,
  ButtonGroup,
  DesignIntf,
{$ENDIF}
  // GLScene
  GLContext,
  GL3xMaterialGraph,
  VectorTypes,
  GLCrossPlatform,
  GLState,
  GL3xMaterial,
  GLFileDDS,
  GLFileJPEG,
  GLFilePNG;

type

  { TMaterialEditorForm }

  TMaterialEditorForm = class(TForm)
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    File1: TMenuItem;
    CloseItem: TMenuItem;
    Cancel1: TMenuItem;
    View1: TMenuItem;
    TransparensyMenuItem: TMenuItem;
    DeleteSelected: TMenuItem;
    NodePaletteMenuItem: TMenuItem;
    BreakLink: TMenuItem;
    BreakAllLinks: TMenuItem;
    RefreshAllMenuItem: TMenuItem;
    MaterialPreviewMenuItem: TMenuItem;
    RealTimePreviewMenuItem: TMenuItem;
    MaterialCodeMenuItem: TMenuItem;
    Cadencer: TTimer;
    Model1: TMenuItem;
    SphereMenuItem: TMenuItem;
    CubeMenuItem: TMenuItem;
    PlaneMenuItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseItemClick(Sender: TObject);
    procedure TransparensyMenuItemClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ACViewNodePalleteExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Cadence(Sender: TObject);
    procedure BreakLinkClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure BreakAllLinksClick(Sender: TObject);
    procedure DeleteSelectedClick(Sender: TObject);
    procedure ACViewMaterialPreviewExecute(Sender: TObject);
    procedure ACViewMaterialCodeExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ACRefreshAllExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ModelMenuItemClick(Sender: TObject);
  protected
    { Protected declarations }
{$IFDEF FPC}
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure LMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
{$ELSE}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
{$ENDIF}

  private
    { Private declarations }
{$IFDEF MSWINDOWS}
    FDC: HDC;
{$ENDIF}
{$IFDEF UNIX}
    FDC: HDC;
{$ENDIF}
{$IFNDEF FPC}
    FCurrentDesigner: IDesigner;
{$ELSE}
    FSelection: TPersistentSelectionList;
{$ENDIF}
    FRenderingContext: TGLContext;
    CounterFrequency: Int64;

    FMaterialGraph: TMaterialGraph;
    smx, smy, mx, my: Integer;
    FZoomFactor: Single;
    OldClientWidth, OldClientHeight: Integer;
    Panning: Boolean;
    AfterPopup: Boolean;
    FClosing: Boolean;
    FStoredMaterial: TGL3xMaterialName;
    procedure SetMaterial(Value: TGL3xMaterialName);
    function GetCoordInGraphSpace(const X, Y: Integer): TVector2s;
    procedure DoPaint;
    procedure SelectNode(ANode: TPersistent; GroupSelection: Boolean);
    procedure UnSelectAll;
  public
    { Public declarations }
    procedure PrepareContext;
{$IFNDEF FPC}
    property Designer: IDesigner write FCurrentDesigner;
{$ENDIF}
    property MaterialGraph: TMaterialGraph read FMaterialGraph;
    property Material: TGL3xMaterialName write SetMaterial;
  end;

function MaterialEditorForm: TMaterialEditorForm;

implementation

{$IFDEF FPC}
{.$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  OpenGLTokens,
  VectorGeometry,
  GLVBOManager,
{$IFDEF FPC}
  GL3xMaterialNodePaletteLCL,
{$ELSE}
  GL3xMaterialNodePalette,
{$ENDIF}
  GL3xMaterialPreview,
  GL3xMaterialCode;

var
  vMaterialEditorForm: TMaterialEditorForm;
  vNodePaletteForm: TNodePaletteForm;
  vMaterialPreviewForm: TMaterialPreviewForm;
  vMaterialCodeForm: TMaterialCodeForm;

function MaterialEditorForm: TMaterialEditorForm;
begin
  if not Assigned(vMaterialEditorForm) then
  begin
    vMaterialEditorForm := TMaterialEditorForm.Create({$IFNDEF FPC}nil{$ELSE}Application{$ENDIF});
    vNodePaletteForm := TNodePaletteForm.Create(vMaterialEditorForm);
    vMaterialPreviewForm := TMaterialPreviewForm.Create(vMaterialEditorForm);
    vMaterialCodeForm := TMaterialCodeForm.Create(vMaterialEditorForm);
    vNodePaletteForm.NotifyClose := vMaterialEditorForm.ACViewNodePalleteExecute;
    vMaterialPreviewForm.NotifyClose := vMaterialEditorForm.ACViewMaterialPreviewExecute;
    vMaterialCodeForm.NotifyClose := vMaterialEditorForm.ACViewMaterialCodeExecute;
    vMaterialPreviewForm.PrepareContext;
    vMaterialEditorForm.PrepareContext;
    vMaterialEditorForm.OnActivate(Application);
    vNodePaletteForm.Visible := True;
    vMaterialPreviewForm.Visible := True;
    vMaterialCodeForm.Visible := True;
  end;
  Result := vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
  vMaterialPreviewForm.Free;
  vMaterialCodeForm.Free;
  vMaterialEditorForm.Free;
  vMaterialEditorForm.Free;
end;

procedure TMaterialEditorForm.FormActivate(Sender: TObject);
begin
  if not Assigned(FRenderingContext) then
    Close;
  if Assigned(vNodePaletteForm)
    and Assigned(vMaterialPreviewForm)
    and Assigned(vMaterialCodeForm) then
  begin
    SetFocus;
{$IFNDEF FPC}
    vNodePaletteForm.Left := Left + Width + 5;
    vNodePaletteForm.Top := Top + 5;
    vMaterialPreviewForm.Left := Left - vMaterialPreviewForm.Width - 7;
    vMaterialPreviewForm.Top := Top + 5;
    vMaterialCodeForm.Left := Left + 5;
    vMaterialCodeForm.Top := Top + Height + 5;
{$ELSE}
    vNodePaletteForm.FormMouseLeave(Self);
    vNodePaletteForm.Left := Left + Width + 20;
    vNodePaletteForm.Top := Top + 5;
    vMaterialPreviewForm.Left := Left - vMaterialPreviewForm.Width - 10;
    vMaterialPreviewForm.Top := Top + 5;
    vMaterialCodeForm.Left := Left + 5;
    vMaterialCodeForm.Top := Top + Height + 40;
{$ENDIF}
    vNodePaletteForm.LastChangePosByMainForm := True;
    vMaterialPreviewForm.LastChangePosByMainForm := True;
    vMaterialCodeForm.LastChangePosByMainForm := True;
    vNodePaletteForm.Docked := True;
    vMaterialPreviewForm.Docked := True;
    vMaterialCodeForm.Docked := True;
    if Assigned(vMaterialCodeForm.CodePad) then
      FMaterialGraph.ShaderCodePad := vMaterialCodeForm.CodePad.Lines;

    vNodePaletteForm.Visible := NodePaletteMenuItem.Checked;
    vMaterialPreviewForm.Visible := MaterialPreviewMenuItem.Checked;
    vMaterialCodeForm.Visible := MaterialCodeMenuItem.Checked;
  end;
end;

procedure TMaterialEditorForm.FormCreate(Sender: TObject);
begin
  FMaterialGraph := TMaterialGraph.Create(Self);

{$IFDEF MSWINDOWS}
  FDC := GetDC(Handle);
{$ENDIF}
{$IFDEF LINUX}
  FDC := Handle;
{$ENDIF}

  FRenderingContext := GLContextManager.CreateContext;
  if not Assigned(FRenderingContext) then
    exit;

  with FRenderingContext do
  begin
    Acceleration := chaHardware;
    Options := [rcoDoubleBuffered, rcoDebug];
    ColorBits := 24;
    DepthBits := 24;
    StencilBits := 0;
    AlphaBits := 0;
    AccumBits := 0;
    AuxBuffers := 0;
    AntiAliasing := aa2x;
    GLStates.ForwardContext := True;
  end;
  try
    FRenderingContext.CreateContext(FDC);
  except
    FreeAndNil(FRenderingContext);
  end;
  if not Assigned(FRenderingContext) then
    exit;

  FZoomFactor := 1;

  FMaterialGraph.ScreenCenter := Vector2sMake(ClientWidth div 2, ClientHeight div 2);

  OldClientWidth := ClientWidth;
  OldClientHeight := ClientHeight;

  Panning := false;
  AfterPopup := false;
  FMaterialGraph.PullingMode := False;

  QueryPerformanceFrequency(CounterFrequency);
{$IFDEF GLS_DELPHI_XE_UP}
  FormatSettings.
{$ENDIF}
  DecimalSeparator := '.';
  TransparensyMenuItem.Enabled := False;
{$IFDEF FPC}
  FSelection := TPersistentSelectionList.Create;
{$ENDIF}
end;

procedure TMaterialEditorForm.PrepareContext;
begin
  if Assigned(FRenderingContext) then
  begin
    FRenderingContext.Activate;
    //  FRenderingContext.ShareLists(MaterialPreviewForm.RenderingContext);
    with FRenderingContext.GLStates do
    begin
      Disable(stDepthTest);
      Disable(stCullFace);
      FrontFace := fwClockWise;
      LineSmoothHint := hintNicest;
      Enable(stLineSmooth);
      LineWidth := 1.0;
      Gl.Enable(GL_TEXTURE_RECTANGLE);
      ViewPort := Vector4iMake(0, 0, ClientWidth, ClientHeight);
    end;
    FRenderingContext.Deactivate;

    Cadencer.Enabled := true;
  end;
end;

procedure TMaterialEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{$IFNDEF MSWINDOWS}
const
  idYes = idButtonYes;
  idNo = idButtonNo;
  idCancel = idButtonCancel;
  {$ENDIF}
var
  SaveReq: Integer;
begin
  if not Assigned(FRenderingContext) or not Visible then
  begin
    CanClose := true;
    exit;
  end;
  UnSelectAll;

  CanClose := True;

  SaveReq := MessageDlg('Save changes of material?', mtConfirmation, mbYesNoCancel, 0);
  case SaveReq of
    idYes:
    with FMaterialGraph do
    begin
      Save;
      if Length(Material.ResourceName) = 0 then
        Material.ResourceName := FStoredMaterial+'_material.xml';
      Material.SaveToFile(Material.ResourceName);
      Material.AssignTo(FStoredMaterial);
    end;
    idNo: CanClose := True;
    idCancel: CanClose := False;
  end;
end;

procedure TMaterialEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FClosing := True;
  if NodePaletteMenuItem.Checked then
    vNodePaletteForm.Close;
  if MaterialPreviewMenuItem.Checked then
    vMaterialPreviewForm.Close;
  if MaterialCodeMenuItem.Checked then
    vMaterialCodeForm.Close;
  FClosing := False;
end;

procedure TMaterialEditorForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FRenderingContext) then
  begin
    FreeAndNil(FMaterialGraph);
    FreeAndNil(FRenderingContext);
    {$IFDEF FPC}
    FreeAndNil(FSelection);
    {$ENDIF}
  end;
  vMaterialEditorForm := nil;
  vNodePaletteForm := nil;
  vMaterialPreviewForm := nil;
  vMaterialCodeForm := nil;
end;

procedure TMaterialEditorForm.FormDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  pos: Tvector2s;
begin
{$IFDEF FPC}
  if (Source is TPanel)
{$ELSE}
  if (Source is TButtonGroup)
{$ENDIF}
  and (vNodePaletteForm.NodeClass <> TCustomMaterialGraphNode) then
  begin
    pos := GetCoordInGraphSpace(X, Y);
    FMaterialGraph.AddNode(vNodePaletteForm.NodeClass,
      pos[0] - 50, pos[1] - 50, 100, 100);
  end;
end;

procedure TMaterialEditorForm.FormDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
{$IFDEF FPC}
  Accept := Source is TPanel;
{$ELSE}
  Accept := Source is TButtonGroup;
{$ENDIF}
end;

procedure TMaterialEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TMaterialEditorForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (TGLMouseButton(Button) = mbLeft)
    and FMaterialGraph.IsCursorOverJoint
    and not AfterPopup then
    FMaterialGraph.PullingMode := True
  else
    Panning := true;
  smx := X;
  smy := Y;
end;

procedure TMaterialEditorForm.FormMouseLeave(Sender: TObject);
begin
  Panning := false;
  FMaterialGraph.PullingMode := False;
  Cursor := crDefault;
end;

procedure TMaterialEditorForm.FormMouseMove(Sender: TObject; Shift:
  TShiftState;
  X, Y: Integer);
var
  delta: TVector2s;
begin
  if AfterPopup then
    AfterPopup := false
  else
  begin
    if (mx - smx <> 0) or (my - smy <> 0) then
    begin
      if Panning then
      begin
        delta := Vector2sMake(
          Round((mx - X) * FZoomFactor),
          Round((Y - my) * FZoomFactor));

        if ssCtrl in Shift then
        begin
          Cursor := crDrag;
          FMaterialGraph.MoveSelectedNodes(delta);
        end
        else
        begin
          Cursor := crSizeAll;
          FMaterialGraph.ScreenCenter := Vector2sMake(
            FMaterialGraph.ScreenCenter[0] + delta[0],
            FMaterialGraph.ScreenCenter[1] + delta[1]);
        end;
      end;
    end;
  end;
  mx := X;
  my := Y;
  FMaterialGraph.SetCursorCoords(GetCoordInGraphSpace(X, Y));
end;

procedure TMaterialEditorForm.FormMouseUp(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  bGroupSelection: Boolean;
begin
  if (smx - mx <> 0) or (smy - my <> 0) then
  begin
    if Panning then
      Cursor := crDefault;
    if FMaterialGraph.PullingMode then
      FMaterialGraph.CreateLink;
  end
  else
  begin
    if TGLMouseButton(Button) = mbRight then
      PopupMenu1.Popup(Left + X, Top + Y)
    else if TGLMouseButton(Button) = mbLeft then
    begin
      FMaterialGraph.SetCursorCoords(GetCoordInGraphSpace(X, Y));
      bGroupSelection := ssShift in Shift;
      SelectNode(FMaterialGraph.Pick(bGroupSelection), bGroupSelection);
    end;
  end;
  Panning := false;
  FMaterialGraph.PullingMode := false;
end;

procedure TMaterialEditorForm.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  delta: Integer;
begin
  delta := sign(WheelDelta);
  if delta > 0 then
  begin
    FZoomFactor := FZoomFactor * 1.1;
  end
  else if delta < 0 then
  begin
    FZoomFactor := FZoomFactor / 1.1;
  end;

  if FZoomFactor < 1 then
    FZoomFactor := 1;
  if FZoomFactor > 8 then
    FZoomFactor := 8;

  FMaterialGraph.ScreenZoom := FZoomFactor;
end;

procedure TMaterialEditorForm.FormResize(Sender: TObject);
var
  ScreenCenterOffset: TVector2s;
begin
  if Assigned(FMaterialGraph) then
  begin
    ScreenCenterOffset := Vector2sMake(
      Round(0.5 * (OldClientWidth - ClientWidth)),
      Round(0.5 * (OldClientHeight - ClientHeight)));
    FMaterialGraph.ScreenCenter := Vector2sMake(
      FMaterialGraph.ScreenCenter[0] + ScreenCenterOffset[0],
      FMaterialGraph.ScreenCenter[1] + ScreenCenterOffset[1]);
    FMaterialGraph.ScreenSize := Vector2sMake(ClientWidth, ClientHeight);
    OldClientWidth := ClientWidth;
    OldClientHeight := ClientHeight;
  end;
end;

procedure TMaterialEditorForm.Cadence(Sender: TObject);
begin
{$IFNDEF FPC}
  InvalidateRect(Handle, nil, False);
  InvalidateRect(vMaterialPreviewForm.Handle, nil, False);
{$ELSE}
  DoPaint;
  vMaterialPreviewForm.DoPaint;
{$ENDIF}
end;

procedure TMaterialEditorForm.TransparensyMenuItemClick(Sender: TObject);
begin
  TransparensyMenuItem.Checked := not TransparensyMenuItem.Checked;
  AlphaBlend := TransparensyMenuItem.Checked;
end;

{$IFDEF FPC}

procedure TMaterialEditorForm.LMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if Assigned(FRenderingContext) then
    Message.Result := 1
  else
    inherited;
end;

procedure TMaterialEditorForm.LMPaint(var Message: TLMPaint);
begin
end;
{$ELSE}

procedure TMaterialEditorForm.WMPaint(var Msg: TWMPaint);
begin
  DoPaint;
  inherited;
end;
{$ENDIF}

procedure TMaterialEditorForm.DoPaint;
var
  {$IFDEF MSWINDOWS}
  ps: Windows.TPaintStruct;
  {$ENDIF}
  counter: Int64;
begin
  if Assigned(FRenderingContext) then
  begin
    {$IFDEF MSWINDOWS}
    BeginPaint(Handle, ps);
    {$ENDIF}
    FRenderingContext.Activate;
    try
      if Assigned(FMaterialGraph) then
      begin
        QueryPerformanceCounter(counter);
        GLVBOManager.vCurrentTime := counter / CounterFrequency;
        FRenderingContext.GLStates.ViewPort :=
          Vector4iMake(0, 0, ClientWidth, ClientHeight);
        FMaterialGraph.Render;
      end;
      FRenderingContext.SwapBuffers;
    finally
      FRenderingContext.Deactivate;
      {$IFDEF MSWINDOWS}
      EndPaint(Handle, ps);
      {$ENDIF}
    end;
  end;
end;

{$IFDEF FPC}
procedure TMaterialEditorForm.WMMove(var Message: TLMMove);
{$ELSE}
procedure TMaterialEditorForm.WMMove(var Message: TWMMove);
{$ENDIF}
begin
  if Assigned(vNodePaletteForm)
    and vNodePaletteForm.Docked
    and Self.Active then
  begin
{$IFNDEF FPC}
    vNodePaletteForm.Left := Message.XPos + Width;
    vNodePaletteForm.Top := Message.YPos - 41;
    vMaterialPreviewForm.Left := Message.XPos - vMaterialPreviewForm.Width - 15;
    vMaterialPreviewForm.Top := vNodePaletteForm.Top;
    vMaterialCodeForm.Left := Message.XPos - 3;
    vMaterialCodeForm.Top := Message.YPos + Height - 39;
{$ELSE}
    vNodePaletteForm.Left := Message.XPos + Width + 20;
    vNodePaletteForm.Top := Message.YPos + 5;
    vMaterialPreviewForm.Left := Message.XPos - vMaterialPreviewForm.Width - 7;
    vMaterialPreviewForm.Top := vNodePaletteForm.Top;
    vMaterialCodeForm.Left := Message.XPos + 5;
    vMaterialCodeForm.Top := Message.YPos + Height + 40;
{$ENDIF}
    vNodePaletteForm.LastChangePosByMainForm := True;
    vNodePaletteForm.Docked := True;
    vMaterialPreviewForm.LastChangePosByMainForm := True;
    vMaterialPreviewForm.Docked := True;
    vMaterialCodeForm.LastChangePosByMainForm := True;
    vMaterialCodeForm.Docked := True;
  end;
  inherited;
end;

procedure TMaterialEditorForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

function TMaterialEditorForm.GetCoordInGraphSpace(const X, Y: Integer):
  TVector2s;
begin
  Result := Vector2sMake(
    Round(FZoomFactor * (X - ClientWidth div 2)),
    Round(FZoomFactor * (ClientHeight div 2 - Y)));
  Result[0] := Result[0] + FMaterialGraph.ScreenCenter[0];
  Result[1] := Result[1] + FMaterialGraph.ScreenCenter[1];
end;

procedure TMaterialEditorForm.ACViewNodePalleteExecute(Sender: TObject);
var
  flag: Boolean;
begin
  if FClosing then
    exit;
  flag := vNodePaletteForm.Visible;
  flag := not flag;
  NodePaletteMenuItem.Checked := flag;
  vNodePaletteForm.Visible := flag;
end;

procedure TMaterialEditorForm.ACViewMaterialPreviewExecute(Sender: TObject);
var
  flag: Boolean;
begin
  if FClosing then
    exit;
  flag := vMaterialPreviewForm.Visible;
  flag := not flag;
  MaterialPreviewMenuItem.Checked := flag;
  vMaterialPreviewForm.Visible := flag;
end;

procedure TMaterialEditorForm.ACViewMaterialCodeExecute(Sender: TObject);
var
  flag: Boolean;
begin
  if FClosing then
    exit;
  flag := vMaterialCodeForm.Visible;
  flag := not flag;
  MaterialCodeMenuItem.Checked := flag;
  vMaterialCodeForm.Visible := flag;
end;

procedure TMaterialEditorForm.ACRefreshAllExecute(Sender: TObject);
begin
  FMaterialGraph.Refresh;
end;

procedure TMaterialEditorForm.PopupMenu1Popup(Sender: TObject);
begin
  BreakLink.Visible := FMaterialGraph.IsCursorOverJoint;
  DeleteSelected.Visible := FMaterialGraph.GetSelectedNodeNumber > 0;
  BreakAllLinks.Visible := DeleteSelected.Visible;
  Panning := false;
  AfterPopup := true;
end;

procedure TMaterialEditorForm.BreakLinkClick(Sender: TObject);
begin
  FMaterialGraph.BreakLink;
end;

procedure TMaterialEditorForm.BreakAllLinksClick(Sender: TObject);
begin
  FMaterialGraph.BreakAllLinks;
end;

procedure TMaterialEditorForm.SelectNode(ANode: TPersistent; GroupSelection: Boolean);
{$IFDEF FPC}
var
  vSelected: TPersistent;
{$ENDIF}
begin
  if not GroupSelection then
{$IFNDEF FPC}
    FCurrentDesigner.NoSelection;
  FCurrentDesigner.SelectComponent(FMaterialGraph.Pick(GroupSelection));
  FCurrentDesigner.Modified;
{$ELSE}
  FSelection.Clear;
  vSelected := FMaterialGraph.Pick(GroupSelection);
  if Assigned(vSelected) then
    FSelection.Add(vSelected);
  GlobalDesignHook.SetSelection(FSelection);
{$ENDIF}
end;

procedure TMaterialEditorForm.UnSelectAll;
begin
{$IFNDEF FPC}
  FCurrentDesigner.NoSelection;
  FCurrentDesigner.Modified;
{$ELSE}
  FSelection.Clear;
  GlobalDesignHook.SetSelection(FSelection);
  GlobalDesignHook.Modified(Self);
{$ENDIF}
end;

procedure TMaterialEditorForm.DeleteSelectedClick(Sender: TObject);
begin
  UnSelectAll;
  FMaterialGraph.DeleteSelected;
end;

procedure TMaterialEditorForm.SetMaterial(Value: TGL3xMaterialName);
begin
  FStoredMaterial := Value;
  FMaterialGraph.Material.AssignFrom(Value);
  FMaterialGraph.Load;
end;

procedure TMaterialEditorForm.ModelMenuItemClick(Sender: TObject);
begin
  vMaterialPreviewForm.SetModel(TMenuItem(Sender).Tag);
end;

initialization
{$IFDEF FPC}
  {$I GL3xMaterialEditorForms.lrs}
{$ENDIF}
finalization

  ReleaseMaterialEditorForm;



end.

