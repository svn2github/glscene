//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//

unit FShaderUniformEditor;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.ListBox,
  FMX.StdCtrls,
  FMX.Controls.Presentation,

  VXS.GLSLParameter,
  VXS.TextureFormat,
  VXS.VectorGeometry,
  VXS.Strings;

type
  TVXShaderUniformEditorForm = class(TForm)
    Label1: TLabel;
    LBUniforms: TListBox;
    Label2: TLabel;
    AutoSetBox: TComboBox;
    SamplerBox: TComboBox;
    RedGroup: TGroupBox;
    GreenGroup: TGroupBox;
    BlueGroup: TGroupBox;
    AlphaGroup: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    TextureBox: TComboBox;
    Label5: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton13: TRadioButton;
    RadioButton14: TRadioButton;
    RadioButton15: TRadioButton;
    RadioButton16: TRadioButton;
    RadioButton17: TRadioButton;
    RadioButton18: TRadioButton;
    RadioButton19: TRadioButton;
    RadioButton20: TRadioButton;
    RadioButton21: TRadioButton;
    RadioButton22: TRadioButton;
    RadioButton23: TRadioButton;
    RadioButton24: TRadioButton;
    procedure FormDestroy(Sender: TObject);
    procedure LBUniformsClick(Sender: TObject);
    procedure ColorGroupClick(Sender: TObject);
    procedure AutoSetBoxChange(Sender: TObject);
    procedure TextureBoxChange(Sender: TObject);
    procedure SamplerBoxChange(Sender: TObject);
    procedure LBUniformsKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    FUniformList: array of IVXShaderParameter;
  public
    procedure Clear;
    procedure AddTextureName(const S: string);
    procedure AddSamplerName(const S: string);
    procedure AddUniform(AValue: IVXShaderParameter);
    procedure Execute;
  end;

function GLShaderUniformEditorForm: TVXShaderUniformEditorForm;
procedure ReleaseShaderUniformEditor;

//==================================================================
implementation
//==================================================================

{$R *.fmx}

var
  vVKShaderUniformEditor: TVXShaderUniformEditorForm;

  function GLShaderUniformEditorForm: TVXShaderUniformEditorForm;
begin
  if not Assigned(vVKShaderUniformEditor) then
    vVKShaderUniformEditor := TVXShaderUniformEditorForm.Create(nil);
  Result := vVKShaderUniformEditor;
end;

procedure ReleaseShaderUniformEditor;
begin
  if Assigned(vVKShaderUniformEditor) then
  begin
    vVKShaderUniformEditor.Free;
    vVKShaderUniformEditor := nil;
  end;
end;

//-----------------------------------------------------------
{ TVXShaderUniformEditor }
//-----------------------------------------------------------

procedure TVXShaderUniformEditorForm.AddUniform(AValue: IVXShaderParameter);
begin
  if AValue <> nil then
  begin
    SetLength(FUniformList, Length(FUniformList)+1);
    FUniformList[High(FUniformList)] := AValue;
  end;
end;

procedure TVXShaderUniformEditorForm.AutoSetBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].AutoSetMethod := AutoSetBox.Items[AutoSetBox.ItemIndex];
  end;
end;

procedure TVXShaderUniformEditorForm.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FUniformList) do
    FUniformList[I] := nil;
  SetLength(FUniformList, 0);
  LBUniforms.Items.Clear;
  LBUniforms.ItemIndex := -1;
  AutoSetBox.Items.Clear;
  TextureBox.Items.Clear;
  SamplerBox.Items.Clear;
  AutoSetBox.Items.Add(strNothing);
  TextureBox.Items.Add(strNothing);
  SamplerBox.Items.Add(strNothing);
  AutoSetBox.ItemIndex := 0;
  TextureBox.ItemIndex := 0;
  SamplerBox.ItemIndex := 0;
  RedGroup.Index := -1;
  GreenGroup.Index := -1;
  BlueGroup.Index := -1;
  AlphaGroup.Index := -1;
end;

procedure TVXShaderUniformEditorForm.Execute;
var
  I: Integer;
  str: AnsiString;
begin
  for I := 0 to High(FUniformList) do
  begin
    if FUniformList[I].GLSLType <> GLSLTypeUndefined then
      str := cGLSLTypeString[FUniformList[I].GLSLType];
    if FUniformList[I].GLSLSamplerType <> GLSLSamplerUndefined then
      str := cGLSLSamplerString[FUniformList[I].GLSLSamplerType];
    LBUniforms.Items.Add(FUniformList[I].Name+': '+string(str));
  end;
  ShowModal;
end;

procedure TVXShaderUniformEditorForm.FormDestroy(Sender: TObject);
begin
  FUniformList := nil;
end;

procedure TVXShaderUniformEditorForm.LBUniformsClick(Sender: TObject);
var
  SV: TSwizzleVector;
  IParam: IVXShaderParameter;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    AutoSetBox.Items.Clear;
    AutoSetBox.Items.Add(strNothing);
    IParam := FUniformList[LBUniforms.ItemIndex];
    if IParam.GLSLSamplerType <> GLSLSamplerUndefined then
    begin
      FillUniformAutoSetMethodList(AutoSetBox.Items, IParam.GLSLSamplerType);
      AutoSetBox.ItemIndex :=
        MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      TextureBox.Enabled := True;
      SamplerBox.Enabled := True;
      TextureBox.ItemIndex :=
        MaxInteger(TextureBox.Items.IndexOf(IParam.TextureName), 0);
      SamplerBox.ItemIndex :=
        MaxInteger(SamplerBox.Items.IndexOf(IParam.SamplerName), 0);
      SV := IParam.GetTextureSwizzle;
      RedGroup.Index := Ord(SV[0]);
      GreenGroup.Index := Ord(SV[1]);
      BlueGroup.Index := Ord(SV[2]);
      AlphaGroup.Index := Ord(SV[3]);
    end
    else
    begin
      TextureBox.Enabled := False;
      SamplerBox.Enabled := False;
      FillUniformAutoSetMethodList(AutoSetBox.Items, IParam.GLSLType);
      AutoSetBox.ItemIndex :=
        MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      RedGroup.Index := -1;
      GreenGroup.Index := -1;
      BlueGroup.Index := -1;
      AlphaGroup.Index := -1;
    end;
  end;
end;

procedure TVXShaderUniformEditorForm.LBUniformsKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  LBUniformsClick(Self);
end;

procedure TVXShaderUniformEditorForm.SamplerBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].SamplerName :=
      SamplerBox.Items[SamplerBox.ItemIndex];
  end;
end;

procedure TVXShaderUniformEditorForm.TextureBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].TextureName :=
      TextureBox.Items[TextureBox.ItemIndex];
  end;
end;

procedure TVXShaderUniformEditorForm.ColorGroupClick(Sender: TObject);
var
  SV: TSwizzleVector;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    if FUniformList[LBUniforms.ItemIndex].GLSLSamplerType = GLSLSamplerUndefined then
      exit;
    SV := FUniformList[LBUniforms.ItemIndex].GetTextureSwizzle;
    SV[TGroupBox(Sender).Tag] := TVXTextureSwizzle(TGroupBox(Sender).Index);
    FUniformList[LBUniforms.ItemIndex].SetTextureSwizzle(SV);
  end;
end;

procedure TVXShaderUniformEditorForm.AddTextureName(const S: string);
begin
  TextureBox.Items.Add(S);
end;

procedure TVXShaderUniformEditorForm.AddSamplerName(const S: string);
begin
  SamplerBox.Items.Add(S);
end;

//-----------------------------------------------------------
initialization
//-----------------------------------------------------------

finalization
  ReleaseShaderUniformEditor;

end.
