//
// This unit is part of the GLScene Project   
//
{: FShaderUniformEditor<p>

	<b>History : </b><font size=-1><ul>
    <li>06/01/15 - PW - Converted to FMX
    <li>23/03/11 - Yar - Creation
  </ul>
}

unit FShaderUniformEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls,

  GLSL.Parameter, GLS.TextureFormat, GLS.VectorGeometry;

type
  TShaderUniformEditor = class(TForm)
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
    { Private declarations }
    FUniformList: array of IShaderParameter;
  public
    { Public declarations }
    procedure Clear;
    procedure AddTextureName(const S: string);
    procedure AddSamplerName(const S: string);
    procedure AddUniform(AValue: IShaderParameter);

    procedure Execute;
  end;


function ShaderUniformEditor: TShaderUniformEditor;
procedure ReleaseShaderUniformEditor;

implementation

{$R *.fmx}

var
  vShaderUniformEditor: TShaderUniformEditor;

  function ShaderUniformEditor: TShaderUniformEditor;
begin
  if not Assigned(vShaderUniformEditor) then
    vShaderUniformEditor := TShaderUniformEditor.Create(nil);
  Result := vShaderUniformEditor;
end;

procedure ReleaseShaderUniformEditor;
begin
  if Assigned(vShaderUniformEditor) then
  begin
    vShaderUniformEditor.Free;
    vShaderUniformEditor := nil;
  end;
end;

{ TShaderUniformEditor }

procedure TShaderUniformEditor.AddUniform(AValue: IShaderParameter);
begin
  if AValue <> nil then
  begin
    SetLength(FUniformList, Length(FUniformList)+1);
    FUniformList[High(FUniformList)] := AValue;
  end;
end;

procedure TShaderUniformEditor.AutoSetBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].AutoSetMethod := AutoSetBox.Items[AutoSetBox.ItemIndex];
  end;
end;

procedure TShaderUniformEditor.Clear;
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
  AutoSetBox.Items.Add(rstrNothing);
  TextureBox.Items.Add(rstrNothing);
  SamplerBox.Items.Add(rstrNothing);
  AutoSetBox.ItemIndex := 0;
  TextureBox.ItemIndex := 0;
  SamplerBox.ItemIndex := 0;
  RedGroup.Index := -1;
  GreenGroup.Index := -1;
  BlueGroup.Index := -1;
  AlphaGroup.Index := -1;
end;

procedure TShaderUniformEditor.Execute;
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

procedure TShaderUniformEditor.FormDestroy(Sender: TObject);
begin
  FUniformList := nil;
end;

procedure TShaderUniformEditor.LBUniformsClick(Sender: TObject);
var
  SV: TSwizzleVector;
  IParam: IShaderParameter;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    AutoSetBox.Items.Clear;
    AutoSetBox.Items.Add(rstrNothing);
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

procedure TShaderUniformEditor.LBUniformsKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  LBUniformsClick(Self);
end;

procedure TShaderUniformEditor.SamplerBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].SamplerName :=
      SamplerBox.Items[SamplerBox.ItemIndex];
  end;
end;

procedure TShaderUniformEditor.TextureBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].TextureName :=
      TextureBox.Items[TextureBox.ItemIndex];
  end;
end;

procedure TShaderUniformEditor.ColorGroupClick(Sender: TObject);
var
  SV: TSwizzleVector;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    if FUniformList[LBUniforms.ItemIndex].GLSLSamplerType = GLSLSamplerUndefined then
      exit;
    SV := FUniformList[LBUniforms.ItemIndex].GetTextureSwizzle;
    SV[TGroupBox(Sender).Tag] := TGLTextureSwizzle(TGroupBox(Sender).Index);
    FUniformList[LBUniforms.ItemIndex].SetTextureSwizzle(SV);
  end;
end;

procedure TShaderUniformEditor.AddTextureName(const S: string);
begin
  TextureBox.Items.Add(S);
end;

procedure TShaderUniformEditor.AddSamplerName(const S: string);
begin
  SamplerBox.Items.Add(S);
end;

initialization

finalization
  ReleaseShaderUniformEditor;

end.
