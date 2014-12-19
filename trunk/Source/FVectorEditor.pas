//
// This unit is part of the GLScene Project, http://glscene.org
//

{ : FVectorEditor<p>

  Editor for a vector.<p>

  <b>Historique : </b><font size=-1><ul>
  <li>05/10/08 - DanB - Removed Kylix support
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>03/07/04 - LR - Make change for Linux
  <li>?/?/? -  - Creation
  </ul></font>
}
unit FVectorEditor;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  System.Classes, System.SysUtils, VCL.Forms, VCL.ComCtrls, VCL.StdCtrls,
  VCL.ToolWin,
  VCL.ExtCtrls, VCL.Buttons, VCL.Graphics, VCL.Controls,
{$ELSE}
    Classes, Forms, SysUtils, ComCtrls, StdCtrls, ToolWin, ExtCtrls, Buttons,
  Graphics, Controls,
{$ENDIF}
  //GLS
  GLVectorGeometry, GLUtils, GLVectorTypes;

type
  TVectorEditorForm = class(TForm)
    EDx: TEdit;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    EDy: TEdit;
    EDz: TEdit;
    BBok: TBitBtn;
    BBcancel: TBitBtn;
    IMx: TImage;
    IMy: TImage;
    IMz: TImage;
    SBPlusX: TSpeedButton;
    SBMinusX: TSpeedButton;
    SBPlusY: TSpeedButton;
    SBMinusY: TSpeedButton;
    SBPlusZ: TSpeedButton;
    SBMinusZ: TSpeedButton;
    SBNull: TSpeedButton;
    SBUnit: TSpeedButton;
    SBNormalize: TSpeedButton;
    Bevel1: TBevel;
    SBInvert: TSpeedButton;
    procedure EDxChange(Sender: TObject);
    procedure EDyChange(Sender: TObject);
    procedure EDzChange(Sender: TObject);
    procedure SBMinusXClick(Sender: TObject);
    procedure SBMinusYClick(Sender: TObject);
    procedure SBMinusZClick(Sender: TObject);
    procedure SBUnitClick(Sender: TObject);
    procedure SBNormalizeClick(Sender: TObject);
    procedure SBInvertClick(Sender: TObject);
    procedure SBPlusXClick(Sender: TObject);
    procedure SBPlusYClick(Sender: TObject);
    procedure SBPlusZClick(Sender: TObject);
    procedure SBNullClick(Sender: TObject);
  private
    { Private declarations }
    vx, vy, vz: Single;
    procedure TestInput(edit: TEdit; imError: TImage; var dest: Single);
  public
    { Public declarations }
    function Execute(var x, y, z: Single): Boolean;
  end;

function VectorEditorForm: TVectorEditorForm;
procedure ReleaseVectorEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

var
  vVectorEditorForm: TVectorEditorForm;

function VectorEditorForm: TVectorEditorForm;
begin
  if not Assigned(vVectorEditorForm) then
    vVectorEditorForm := TVectorEditorForm.Create(nil);
  Result := vVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
  if Assigned(vVectorEditorForm) then
  begin
    vVectorEditorForm.Free;
    vVectorEditorForm := nil;
  end;
end;

// Execute
//
function TVectorEditorForm.Execute(var x, y, z: Single): Boolean;
begin
  // setup dialog fields
  vx := x;
  vy := y;
  vz := z;
  EDx.Text := FloatToStr(vx);
  EDy.Text := FloatToStr(vy);
  EDz.Text := FloatToStr(vz);
  // show the dialog
  Result := (ShowModal = mrOk);
  if Result then
  begin
    x := vx;
    y := vy;
    z := vz;
  end;
end;

procedure TVectorEditorForm.TestInput(edit: TEdit; imError: TImage;
  var dest: Single);
begin
  if Visible then
  begin
    try
      dest := StrToFloat(edit.Text);
      imError.Visible := False;
    except
      imError.Visible := True;
    end;
    BBok.Enabled := not(IMx.Visible or IMy.Visible or IMz.Visible);
  end;
end;

procedure TVectorEditorForm.SBPlusXClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVectorEditorForm.SBPlusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '1';
  EDz.Text := '0';
end;


procedure TVectorEditorForm.SBPlusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '1';
end;

procedure TVectorEditorForm.SBNullClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVectorEditorForm.EDxChange(Sender: TObject);
begin
  TestInput(EDx, IMx, vx);
end;

procedure TVectorEditorForm.EDyChange(Sender: TObject);
begin
  TestInput(EDy, IMy, vy);
end;

procedure TVectorEditorForm.EDzChange(Sender: TObject);
begin
  TestInput(EDz, IMz, vz);
end;

procedure TVectorEditorForm.SBMinusXClick(Sender: TObject);
begin
  EDx.Text := '-1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVectorEditorForm.SBMinusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '-1';
  EDz.Text := '0';
end;

procedure TVectorEditorForm.SBMinusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '-1';
end;

procedure TVectorEditorForm.SBUnitClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '1';
  EDz.Text := '1';
end;

procedure TVectorEditorForm.SBNormalizeClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, GLUtils.StrToFloatDef(EDx.Text, 0),
    GLUtils.StrToFloatDef(EDy.Text, 0), GLUtils.StrToFloatDef(EDz.Text, 0));
  if VectorLength(v) = 0 then
    v := NullVector
  else
    NormalizeVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TVectorEditorForm.SBInvertClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, GLUtils.StrToFloatDef(EDx.Text, 0),
    GLUtils.StrToFloatDef(EDy.Text, 0), GLUtils.StrToFloatDef(EDz.Text, 0));
  NegateVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

ReleaseVectorEditorForm;

end.
