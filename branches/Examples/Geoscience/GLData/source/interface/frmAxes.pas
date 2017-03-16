{-------------------------------------------------------------------------------
 Unit Name: frmAxes
 Author:    HochwimmerA
 Purpose:   Axes information...
 $Id: frmAxes.pas,v 1.5 2003/09/21 19:07:43 hochwimmera Exp $
-------------------------------------------------------------------------------}
unit frmAxes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  StdCtrls, geFloatEdit, ExtCtrls,cGLCoordinateAxes;

type
  TformAxes = class(TForm)
    ActionManager1: TActionManager;
    ActionToolBar_Axes: TActionToolBar;
    ImageList: TImageList;
    acAxesVisible: TAction;
    acAxesX: TAction;
    acAxesY: TAction;
    acAxesZ: TAction;
    acAxesXYGrid: TAction;
    acAxesYZGrid: TAction;
    acAxesXZGrid: TAction;
    pnlAxesSettings: TPanel;
    lblAxesXOrigin: TLabel;
    lblAxesYOrigin: TLabel;
    lblAxesZOrigin: TLabel;
    lblAxesXLength: TLabel;
    lblAxesYLength: TLabel;
    lblAxesZLength: TLabel;
    cbxAxesXN: TCheckBox;
    cbxAxesYN: TCheckBox;
    cbxAxesZN: TCheckBox;
    cbxAxesZP: TCheckBox;
    cbxAxesYP: TCheckBox;
    cbxAxesXP: TCheckBox;
    geAxesXOrigin: TGEFloatEdit;
    geAxesYOrigin: TGEFloatEdit;
    geAxesZOrigin: TGEFloatEdit;
    geAxesXLength: TGEFloatEdit;
    geAxesYLength: TGEFloatEdit;
    geAxesZLength: TGEFloatEdit;
    pnlAxesLabels: TPanel;
    lblAxesLabels: TLabel;
    lblAxesXStart: TLabel;
    lblAxesXStep: TLabel;
    lblAxesXStop: TLabel;
    lblAxesYStart: TLabel;
    lblAxesYStep: TLabel;
    lblAxesYStop: TLabel;
    lblAxesZStart: TLabel;
    lblAxesZStep: TLabel;
    lblAxesZStop: TLabel;
    geAxesXStart: TGEFloatEdit;
    geAxesXStep: TGEFloatEdit;
    geAxesXStop: TGEFloatEdit;
    bAxesUpdate: TButton;
    geAxesYStart: TGEFloatEdit;
    geAxesYStep: TGEFloatEdit;
    geAxesYStop: TGEFloatEdit;
    geAxesZStart: TGEFloatEdit;
    geAxesZStep: TGEFloatEdit;
    geAxesZStop: TGEFloatEdit;
    geAxesXRadius: TGEFloatEdit;
    lblAxesXRadius: TLabel;
    lblAxesYRadius: TLabel;
    geAxesYRadius: TGEFloatEdit;
    lblAxesZRadius: TLabel;
    geAxesZRadius: TGEFloatEdit;
    pnlXAxesColour: TPanel;
    lblAxesXColour: TLabel;
    ColourDialog: TColorDialog;
    lblAxesYColour: TLabel;
    pnlYAxesColour: TPanel;
    Label1: TLabel;
    pnlZAxesColour: TPanel;
    Label2: TLabel;
    pnlXLabelColour: TPanel;
    pnlYLabelColour: TPanel;
    pnlZLabelColour: TPanel;
    procedure acAxesVisibleExecute(Sender: TObject);
    procedure acAxesXExecute(Sender: TObject);
    procedure acAxesYExecute(Sender: TObject);
    procedure acAxesZExecute(Sender: TObject);
    procedure acAxesXYGridExecute(Sender: TObject);
    procedure acAxesYZGridExecute(Sender: TObject);
    procedure acAxesXZGridExecute(Sender: TObject);
    procedure cbxAxesXNClick(Sender: TObject);
    procedure cbxAxesXPClick(Sender: TObject);
    procedure cbxAxesYNClick(Sender: TObject);
    procedure cbxAxesYPClick(Sender: TObject);
    procedure cbxAxesZNClick(Sender: TObject);
    procedure cbxAxesZPClick(Sender: TObject);
    procedure geAxesXOriginExit(Sender: TObject);
    procedure geAxesYOriginExit(Sender: TObject);
    procedure geAxesZOriginExit(Sender: TObject);
    procedure geAxesXLengthExit(Sender: TObject);
    procedure geAxesYLengthExit(Sender: TObject);
    procedure geAxesZLengthExit(Sender: TObject);
    procedure bAxesUpdateClick(Sender: TObject);
    procedure geAxesXRadiusExit(Sender: TObject);
    procedure geAxesYRadiusExit(Sender: TObject);
    procedure geAxesZRadiusExit(Sender: TObject);
    procedure pnlXAxesColourClick(Sender: TObject);
    procedure pnlYAxesColourClick(Sender: TObject);
    procedure pnlZAxesColourClick(Sender: TObject);
    procedure pnlXLabelColourClick(Sender: TObject);
    procedure pnlYLabelColourClick(Sender: TObject);
    procedure pnlZLabelColourClick(Sender: TObject);
  private
     
  public
    procedure SetAxesProperties(aaxes:TGLCoordinateAxes);
     
  end;

var
  formAxes: TformAxes;

implementation

uses
  frmMain;

{$R *.dfm}

procedure TformAxes.SetAxesProperties(aaxes:TGLCoordinateAxes);

begin

  geAxesXOrigin.Value := formMain.GLDAxes.Position.X/aaxes.scalex;
  geAxesYOrigin.Value := formMain.GLDAxes.Position.Y/aaxes.scaley;
  geAxesZOrigin.Value := formMain.GLDAxes.Position.Z/aaxes.scalez;

  geAxesXLength.Value := aaxes.XLength;
  geAxesXRadius.Value := aaxes.XRadius;
  pnlXAxesColour.Color := aaxes.XAxisColour;

  geAxesYLength.Value := aaxes.YLength;
  geAxesYRadius.Value := aaxes.YRadius;
  pnlYAxesColour.Color := aaxes.YAxisColour;

  geAxesZLength.Value := aaxes.ZLength;
  geAxesZRadius.Value := aaxes.ZRadius;
  pnlZAxesColour.Color := aaxes.ZAxisColour;

  geAxesXStart.Value := aaxes.XLabelStart;
  geAxesXStep.Value := aaxes.XLabelStep;
  geAxesXStop.Value := aaxes.XLabelStop;
  geAxesYStart.Value := aaxes.YLabelStart;
  geAxesYStep.Value := aaxes.YLabelStep;
  geAxesYStop.Value := aaxes.YLabelStop;
  geAxesZStart.Value := aaxes.ZLabelStart;
  geAxesZStep.Value := aaxes.ZLabelStep;
  geAxesZStop.Value := aaxes.ZLabelStop;

  acAxesVisible.Checked := aaxes.IsVisible;
  acAxesX.Checked := aaxes.XLabelVisible;
  acAxesY.Checked := aaxes.YLabelVisible;
  acAxesZ.Checked := aaxes.ZLabelVisible;

  acAxesXYGrid.Checked := aaxes.XYGrid.Visible;
  acAxesYZGrid.Checked := aaxes.YZGrid.Visible;
  acAxesXZGrid.Checked := aaxes.XZGrid.Visible;

// axes parts:
  cbxAxesXN.Checked := apXN in aaxes.Parts;
  cbxAxesXP.Checked := apXP in aaxes.Parts;
  cbxAxesYN.Checked := apYN in aaxes.Parts;
  cbxAxesYP.Checked := apYP in aaxes.Parts;
  cbxAxesZN.Checked := apZN in aaxes.Parts;
  cbxAxesZP.Checked := apZP in aaxes.Parts;
  bAxesUpdateClick(nil);
end;
// ----- TformAxes.acAxesVisibleExecute ----------------------------------------
procedure TformAxes.acAxesVisibleExecute(Sender: TObject);
begin
  formMain.axes.ShowAxes(acAxesVisible.Checked);
end;
// ----- TformAxes.acAxesXExecute ----------------------------------------------
procedure TformAxes.acAxesXExecute(Sender: TObject);
begin
  formmain.axes.XLabelVisible := acAxesX.Checked;
end;
// ----- TformAxes.acAxesYExecute ----------------------------------------------
procedure TformAxes.acAxesYExecute(Sender: TObject);
begin
  formMain.axes.YLabelVisible := acAxesY.Checked;
end;
// ----- TformAxes.acAxesZExecute ----------------------------------------------
procedure TformAxes.acAxesZExecute(Sender: TObject);
begin
  formMain.axes.ZLabelVisible := acAxesZ.Checked;
end;
// ----- TformAxes.acAxesXYGridExecute -----------------------------------------
procedure TformAxes.acAxesXYGridExecute(Sender: TObject);

begin
  formMain.axes.XYGrid.Visible := acAxesXYGrid.Checked;
end;
// ----- TformAxes.acAxesYZGridExecute -----------------------------------------
procedure TformAxes.acAxesYZGridExecute(Sender: TObject);

begin
  formMain.axes.YZGrid.Visible := acAxesYZGrid.Checked;
end;
// ----- TformAxes.acAxesXZGridExecute -----------------------------------------
procedure TformAxes.acAxesXZGridExecute(Sender: TObject);

begin
  formMain.axes.XZGrid.Visible := acAxesXZGrid.Checked;
end;
// ----- TformAxes.cbxAxesXNClick ----------------------------------------------
procedure TformAxes.cbxAxesXNClick(Sender: TObject);

begin
  formmain.axes.XN := cbxAxesXN.Checked;
end;
// ----- TformAxes.cbxAxesXPClick ----------------------------------------------
procedure TformAxes.cbxAxesXPClick(Sender: TObject);

begin
  formmain.axes.XP := cbxAxesXP.Checked;
end;
// ----- TformAxes.cbxAxesYNClick ----------------------------------------------
procedure TformAxes.cbxAxesYNClick(Sender: TObject);

begin
  formmain.axes.YN := cbxAxesYN.Checked;
end;
// ----- TformAxes.cbxAxesYPClick ----------------------------------------------
procedure TformAxes.cbxAxesYPClick(Sender: TObject);

begin
  formmain.axes.YP := cbxAxesYP.Checked;
end;
// ----- TformAxes.cbxAxesZNClick ----------------------------------------------
procedure TformAxes.cbxAxesZNClick(Sender: TObject);

begin
  formmain.axes.ZN := cbxAxesZN.Checked;
end;
// ----- TformAxes.cbxAxesZPClick ----------------------------------------------
procedure TformAxes.cbxAxesZPClick(Sender: TObject);

begin
  formmain.axes.ZP := cbxAxesZP.Checked;
end;
// ----- TformAxes.geAxesXOriginExit -------------------------------------------
procedure TformAxes.geAxesXOriginExit(Sender: TObject);

begin
  with formMain do
  begin
    GLDAxes.Position.X := geAxesXOrigin.Value*usersettings.ScaleX;
    axes.GenerateXLabels;
  end;
end;
// ----- TformAxes.geAxesYOriginExit -------------------------------------------
procedure TformAxes.geAxesYOriginExit(Sender: TObject);

begin
  with formMain do
  begin
    GLDAxes.Position.Y := geAxesYOrigin.Value*usersettings.ScaleY;
    axes.GenerateYLabels;
  end;
end;
// ----- TformAxes.geAxesZOriginExit -------------------------------------------
procedure TformAxes.geAxesZOriginExit(Sender: TObject);
begin
  with formMain do
  begin
    GLDAxes.Position.Z := geAxesZOrigin.Value*usersettings.ScaleZ;
    axes.GenerateZLabels;
  end;
end;
// ----- TformAxes.geAxesXLengthExit -------------------------------------------
procedure TformAxes.geAxesXLengthExit(Sender: TObject);

begin
  formMain.axes.XLength := geAxesXLength.Value;
end;
// ----- TformAxes.geAxesYLengthExit -------------------------------------------
procedure TformAxes.geAxesYLengthExit(Sender: TObject);

begin
  formMain.axes.YLength := geAxesYLength.Value;
end;
// ----- TformAxes.geAxesZLengthExit -------------------------------------------
procedure TformAxes.geAxesZLengthExit(Sender: TObject);

begin
  formmain.axes.ZLength := geAxesZLength.Value;
end;
// ----- TformAxes.bAxesUpdateClick --------------------------------------------
procedure Tformaxes.bAxesUpdateClick(Sender: TObject);
begin
  with formMain do
  begin
    axes.XLabelStart := geAxesXStart.Value;
    axes.XLabelStep := geAxesXStep.Value;
    axes.XLabelStop := geAxesXStop.Value;
    axes.GenerateXLabels;
    axes.YLabelStart := geAxesYStart.Value;
    axes.YLabelStep := geAxesYStep.Value;
    axes.YLabelStop := geAxesYStop.Value;
    axes.GenerateYLabels;
    axes.ZLabelStart := geAxesZStart.Value;
    axes.ZLabelStep := geAxesZStep.Value;
    axes.ZLabelStop := geAxesZStop.Value;
    axes.GenerateZLabels;
  end;
end;
// ----- TformAxes.geAxesXRadiusExit -------------------------------------------
procedure TformAxes.geAxesXRadiusExit(Sender: TObject);
begin
  formMain.axes.XRadius := geAxesXRadius.Value;
end;
// ----- TformAxes.geAxesYRadiusExit -------------------------------------------
procedure TformAxes.geAxesYRadiusExit(Sender: TObject);
begin
  formMain.axes.YRadius := geAxesYRadius.Value;
end;
// ----- TformAxes.geAxesZRadiusExit -------------------------------------------
procedure TformAxes.geAxesZRadiusExit(Sender: TObject);
begin
  formMain.axes.ZRadius := geAxesZRadius.Value;
end;
// ----- TformAxes.pnlXAxesColourClick -----------------------------------------
procedure TformAxes.pnlXAxesColourClick(Sender: TObject);

begin
  with ColourDialog do
  begin
    Color := pnlXAxesColour.Color;
    if Execute then
    begin
      pnlXAxesColour.Color := Color;
      formMain.axes.XAxisColour := Color;
    end;
  end;
end;
// ----- TformAxes.pnlYAxesColourClick -----------------------------------------
procedure TformAxes.pnlYAxesColourClick(Sender: TObject);
begin
  with ColourDialog do
  begin
    Color := pnlYAxesColour.Color;
    if Execute then
    begin
      pnlYAxesColour.Color := Color;
      formMain.axes.YAxisColour := Color;
    end;
  end;
end;
// ----- TformAxes.pnlZAxesColourClick -----------------------------------------
procedure TformAxes.pnlZAxesColourClick(Sender: TObject);
begin
  with ColourDialog do
  begin
    Color := pnlZAxesColour.Color;
    if Execute then
    begin
      pnlZAxesColour.Color := Color;
      formMain.axes.ZAxisColour := Color;
    end;
  end;
end;
// ----- TformAxes.pnlXLabelColourClick ----------------------------------------
procedure TformAxes.pnlXLabelColourClick(Sender: TObject);
begin
  with ColourDialog do
  begin
    Color := pnlXLabelColour.Color;
    if Execute then
    begin
      pnlXLabelColour.Color := Color;
      formMain.axes.XLabelColour := Color;
    end;
  end;
end;
// ----- TformAxes.pnlYLabelColourClick ----------------------------------------
procedure TformAxes.pnlYLabelColourClick(Sender: TObject);
begin
  with ColourDialog do
  begin
    Color := pnlYLabelColour.Color;
    if Execute then
    begin
      pnlYLabelColour.Color := Color;
      formMain.axes.YLabelColour := Color;
    end;
  end;
end;
// ----- TformAxes.pnlZLabelColourClick ----------------------------------------
procedure TformAxes.pnlZLabelColourClick(Sender: TObject);
begin
  with ColourDialog do
  begin
    Color := pnlZLabelColour.Color;
    if Execute then
    begin
      pnlZLabelColour.Color := Color;
      formMain.axes.ZLabelColour := Color;
    end;
  end;
end;
// =============================================================================
end.

