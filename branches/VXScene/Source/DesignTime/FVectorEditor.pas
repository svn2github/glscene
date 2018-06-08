//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Editor for a vector. 
   
}

unit FVectorEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects,
   
  VXS.VectorGeometry, VXS.VectorTypes, VXS.Utils;

type
  TVXVectorEditorForm = class(TForm)
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
    ImageOK: TImage;
    ImageCancel: TImage;
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
    
    vx, vy, vz: Single;
    procedure TestInput(edit: TEdit; imError: TImage; var dest: Single);
  public
    
    function Execute(var x, y, z: Single): Boolean;
  end;

var
  vVectorEditorForm: TVXVectorEditorForm;

function GLVectorEditorForm: TVXVectorEditorForm;
procedure ReleaseVectorEditorForm;


implementation

{$R *.fmx}

function GLVectorEditorForm: TVXVectorEditorForm;
begin
  if not Assigned(vVectorEditorForm) then
    vVectorEditorForm := TVXVectorEditorForm.Create(nil);
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

function TVXVectorEditorForm.Execute(var x, y, z: Single): Boolean;
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

procedure TVXVectorEditorForm.SBPlusXClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVXVectorEditorForm.SBPlusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '1';
  EDz.Text := '0';
end;

procedure TVXVectorEditorForm.SBPlusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '1';
end;

procedure TVXVectorEditorForm.SBUnitClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '1';
  EDz.Text := '1';
end;

procedure TVXVectorEditorForm.SBInvertClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, VXS.Utils.StrToFloatDef(EDx.Text, 0),
    VXS.Utils.StrToFloatDef(EDy.Text, 0), VXS.Utils.StrToFloatDef(EDz.Text, 0));
  NegateVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TVXVectorEditorForm.SBMinusXClick(Sender: TObject);
begin
  EDx.Text := '-1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVXVectorEditorForm.SBMinusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '-1';
  EDz.Text := '0';
end;

procedure TVXVectorEditorForm.SBMinusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '-1';
end;

procedure TVXVectorEditorForm.SBNormalizeClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, VXS.Utils.StrToFloatDef(EDx.Text, 0),
    VXS.Utils.StrToFloatDef(EDy.Text, 0), VXS.Utils.StrToFloatDef(EDz.Text, 0));
  if VectorLength(v) = 0 then
    v := NullVector
  else
    NormalizeVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TVXVectorEditorForm.SBNullClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVXVectorEditorForm.TestInput(edit: TEdit; imError: TImage;
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
