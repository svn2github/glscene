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
   
  GLX.VectorGeometry, GLX.VectorTypes, GLX.Utils;

type
  TGLVectorEditorForm = class(TForm)
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
  vGLVectorEditorForm: TGLVectorEditorForm;

function GLVectorEditorForm: TGLVectorEditorForm;
procedure ReleaseVectorEditorForm;


implementation

{$R *.fmx}

function GLVectorEditorForm: TGLVectorEditorForm;
begin
  if not Assigned(vGLVectorEditorForm) then
    vGLVectorEditorForm := TGLVectorEditorForm.Create(nil);
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

function TGLVectorEditorForm.Execute(var x, y, z: Single): Boolean;
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

procedure TGLVectorEditorForm.SBPlusXClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TGLVectorEditorForm.SBPlusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '1';
  EDz.Text := '0';
end;

procedure TGLVectorEditorForm.SBPlusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '1';
end;

procedure TGLVectorEditorForm.SBUnitClick(Sender: TObject);
begin
  EDx.Text := '1';
  EDy.Text := '1';
  EDz.Text := '1';
end;

procedure TGLVectorEditorForm.SBInvertClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, GLX.Utils.StrToFloatDef(EDx.Text, 0),
    GLX.Utils.StrToFloatDef(EDy.Text, 0), GLX.Utils.StrToFloatDef(EDz.Text, 0));
  NegateVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TGLVectorEditorForm.SBMinusXClick(Sender: TObject);
begin
  EDx.Text := '-1';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TGLVectorEditorForm.SBMinusYClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '-1';
  EDz.Text := '0';
end;

procedure TGLVectorEditorForm.SBMinusZClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '-1';
end;

procedure TGLVectorEditorForm.SBNormalizeClick(Sender: TObject);
var
  v: TAffineVector;
begin
  SetVector(v, GLX.Utils.StrToFloatDef(EDx.Text, 0),
    GLX.Utils.StrToFloatDef(EDy.Text, 0), GLX.Utils.StrToFloatDef(EDz.Text, 0));
  if VectorLength(v) = 0 then
    v := NullVector
  else
    NormalizeVector(v);
  EDx.Text := FloatToStr(v.x);
  EDy.Text := FloatToStr(v.y);
  EDz.Text := FloatToStr(v.z);
end;

procedure TGLVectorEditorForm.SBNullClick(Sender: TObject);
begin
  EDx.Text := '0';
  EDy.Text := '0';
  EDz.Text := '0';
end;

procedure TGLVectorEditorForm.TestInput(edit: TEdit; imError: TImage;
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
