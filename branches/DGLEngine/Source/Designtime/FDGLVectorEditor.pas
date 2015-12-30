//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{: FVectorEditor<p>

   Editor for a vector.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>05/10/08 - JD - Imported from GLScene
   </ul></font>
}
unit FDGLVectorEditor;

interface

{$i DGLEngine.inc}

uses
  System.Classes, System.SysUtils,
  VCL.Forms, VCL.ComCtrls, VCL.StdCtrls, VCL.ToolWin,
  VCL.ExtCtrls, VCL.Buttons, VCL.Graphics, VCL.Controls,

  DGLVectorMaths, DGLUtils, DGLVectorTypes;

type
  TDGLVectorEditorForm = class(TForm)
    EDx: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EDy: TEdit;
    EDz: TEdit;
    BBok: TBitBtn;
    BBcancel: TBitBtn;
    IMx: TImage;
    IMy: TImage;
    IMz: TImage;
    SpeedButton1: TSpeedButton;
    SBmX: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SBmY: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SBmZ: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SBUnit: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Bevel1: TBevel;
    SBInvert: TSpeedButton;
    procedure TBxClick(Sender: TObject);
    procedure TByClick(Sender: TObject);
    procedure TBzClick(Sender: TObject);
    procedure TBnullClick(Sender: TObject);
    procedure EDxChange(Sender: TObject);
    procedure EDyChange(Sender: TObject);
    procedure EDzChange(Sender: TObject);
    procedure SBmXClick(Sender: TObject);
    procedure SBmYClick(Sender: TObject);
    procedure SBmZClick(Sender: TObject);
    procedure SBUnitClick(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SBInvertClick(Sender: TObject);
  private
    { Déclarations privées }
    vx, vy, vz : Single;
    procedure TestInput(edit : TEdit; imError : TImage; var dest : Single);
  public
    { Déclarations publiques }
    function Execute(var x, y, z : Single) : Boolean;
  end;

function DGLVectorEditorForm : TDGLVectorEditorForm;
procedure ReleaseVectorEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

var
	vVectorEditorForm : TDGLVectorEditorForm;

function DGLVectorEditorForm : TDGLVectorEditorForm;
begin
	if not Assigned(vVectorEditorForm) then
      vVectorEditorForm:=TDGLVectorEditorForm.Create(nil);
	Result:=vVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
	if Assigned(vVectorEditorForm) then
   begin
	   vVectorEditorForm.Free; vVectorEditorForm:=nil;
	end;
end;

// Execute
//
function TDGLVectorEditorForm.Execute(var x, y, z : Single) : Boolean;
begin
   // setup dialog fields
   vx:=x;
   vy:=y;
   vz:=z;
   EDx.Text:=FloatToStr(vx);
   EDy.Text:=FloatToStr(vy);
   EDz.Text:=FloatToStr(vz);
   // show the dialog
   Result:=(ShowModal=mrOk);
   if Result then
   begin
      x:=vx;
      y:=vy;
      z:=vz;
   end;
end;

procedure TDGLVectorEditorForm.TestInput(edit : TEdit; imError : TImage; var dest : Single);
begin
   if Visible then
   begin
      try
         dest:=StrToFloat(edit.Text);
         imError.Visible:=False;
      except
         imError.Visible:=True;
      end;
      BBOk.Enabled:=not (IMx.Visible or IMy.Visible or IMz.Visible);
   end;
end;

procedure TDGLVectorEditorForm.TBxClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TDGLVectorEditorForm.TByClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='1'; EDz.Text:='0';
end;

procedure TDGLVectorEditorForm.TBzClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='1';
end;

procedure TDGLVectorEditorForm.TBnullClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TDGLVectorEditorForm.EDxChange(Sender: TObject);
begin
   TestInput(EDx, IMx, vx);
end;

procedure TDGLVectorEditorForm.EDyChange(Sender: TObject);
begin
   TestInput(EDy, IMy, vy);
end;

procedure TDGLVectorEditorForm.EDzChange(Sender: TObject);
begin
   TestInput(EDz, IMz, vz);
end;

procedure TDGLVectorEditorForm.SBmXClick(Sender: TObject);
begin
   EDx.Text:='-1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TDGLVectorEditorForm.SBmYClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='-1'; EDz.Text:='0';
end;

procedure TDGLVectorEditorForm.SBmZClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='-1';
end;

procedure TDGLVectorEditorForm.SBUnitClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='1'; EDz.Text:='1';
end;

procedure TDGLVectorEditorForm.SpeedButton9Click(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, DGLUtils.StrToFloatDef(EDx.Text, 0), DGLUtils.StrToFloatDef(EDy.Text, 0), DGLUtils.StrToFloatDef(EDz.Text, 0));
   if VectorLength(v)=0 then
      v:=NullVector
   else NormalizeVector(v);
   EDx.Text:=FloatToStr(v.V[0]);
   EDy.Text:=FloatToStr(v.V[1]);
   EDz.Text:=FloatToStr(v.V[2]);
end;

procedure TDGLVectorEditorForm.SBInvertClick(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, DGLUtils.StrToFloatDef(EDx.Text, 0),DGLUtils.StrToFloatDef(EDy.Text, 0), DGLUtils.StrToFloatDef(EDz.Text, 0));
   NegateVector(v);
   EDx.Text:=FloatToStr(v.V[0]);
   EDy.Text:=FloatToStr(v.V[1]);
   EDz.Text:=FloatToStr(v.V[2]);
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



