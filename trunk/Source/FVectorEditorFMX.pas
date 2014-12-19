//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : FVectorEditorFMX<p>

  Editor for a vector.<p>

  <b>History : </b><font size=-1><ul>
  <li>19/12/14 - PW - Upgraded to support FMX
  <li>05/10/08 - DanB - Removed Kylix support
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>03/07/04 - LR - Make change for Linux
  <li>02/03/04 -  - Creation
  </ul></font>
}

unit FVectorEditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects,
  //GLS
  GLVectorGeometry, GLVectorTypes, GLUtils;

type
  TVectorEditorForm = class(TForm)
    SBPlusX: TSpeedButton;
    SBPlusY: TSpeedButton;
    SBPlusZ: TSpeedButton;
    SBMinusX: TSpeedButton;
    SBMinusY: TSpeedButton;
    SBMinusZ: TSpeedButton;
    SBNull: TSpeedButton;
    SBUnit: TSpeedButton;
    SBNormalize: TSpeedButton;
    SBInvert: TSpeedButton;
    EdZ: TEdit;
    EdY: TEdit;
    EdX: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ImX: TImage;
    ImY: TImage;
    ImZ: TImage;
    procedure SBPlusXClick(Sender: TObject);
    procedure SBPlusYClick(Sender: TObject);
    procedure SBPlusZClick(Sender: TObject);
    procedure SBNullClick(Sender: TObject);
    procedure SBNormalizeClick(Sender: TObject);
    procedure SBMinusXClick(Sender: TObject);
    procedure SBMinusYClick(Sender: TObject);
    procedure SBMinusZClick(Sender: TObject);
    procedure SBUnitClick(Sender: TObject);
    procedure SBInvertClick(Sender: TObject);
  private
    { Private declarations }
    vx, vy, vz: Single;
    procedure TestInput(edit: TEdit; imError: TImage; var dest: Single);
  public
    { Public declarations }
    function Execute(var x, y, z: Single): Boolean;
  end;

var
  vVectorEditorForm: TVectorEditorForm;

function VectorEditorForm: TVectorEditorForm;
procedure ReleaseVectorEditorForm;


implementation

{$R *.fmx}

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


{ TVectorEditorForm }

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

procedure TVectorEditorForm.SBUnitClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '1';
  EDz.Text := '1';
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

procedure TVectorEditorForm.SBNullClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '0';
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
    ButtonOk.Enabled := not(IMx.Visible or IMy.Visible or IMz.Visible);
  end;
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
