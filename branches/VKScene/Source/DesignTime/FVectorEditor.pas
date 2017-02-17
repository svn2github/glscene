//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
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
   
  VKS.VectorGeometry, VKS.VectorTypes, VKS.Utils;

type
  TVKVectorEditorForm = class(TForm)
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
  vGLVectorEditorForm: TVKVectorEditorForm;

function GLVectorEditorForm: TVKVectorEditorForm;
procedure ReleaseVectorEditorForm;


implementation

{$R *.fmx}

function GLVectorEditorForm: TVKVectorEditorForm;
begin
  if not Assigned(vGLVectorEditorForm) then
    vGLVectorEditorForm := TVKVectorEditorForm.Create(nil);
  Result := vGLVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
  if Assigned(vGLVectorEditorForm) then
  begin
    vGLVectorEditorForm.Free;
    vGLVectorEditorForm := nil;
  end;
end;


{ TVectorEditorForm }

function TVKVectorEditorForm.Execute(var x, y, z: Single): Boolean;
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

procedure TVKVectorEditorForm.SBPlusXClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVKVectorEditorForm.SBPlusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '1';
  EDz.Text := '0';
end;

procedure TVKVectorEditorForm.SBPlusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '1';
end;

procedure TVKVectorEditorForm.SBUnitClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '1';
  EDz.Text := '1';
end;

procedure TVKVectorEditorForm.SBInvertClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, VKS.Utils.StrToFloatDef(EDx.Text, 0),
    VKS.Utils.StrToFloatDef(EDy.Text, 0), VKS.Utils.StrToFloatDef(EDz.Text, 0));
  NegateVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TVKVectorEditorForm.SBMinusXClick(Sender: TObject);
begin
  EDx.Text := '-1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVKVectorEditorForm.SBMinusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '-1';
  EDz.Text := '0';
end;

procedure TVKVectorEditorForm.SBMinusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '-1';
end;

procedure TVKVectorEditorForm.SBNormalizeClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, VKS.Utils.StrToFloatDef(EDx.Text, 0),
    VKS.Utils.StrToFloatDef(EDy.Text, 0), VKS.Utils.StrToFloatDef(EDz.Text, 0));
  if VectorLength(v) = 0 then
    v := NullVector
  else
    NormalizeVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TVKVectorEditorForm.SBNullClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TVKVectorEditorForm.TestInput(edit: TEdit; imError: TImage;
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
