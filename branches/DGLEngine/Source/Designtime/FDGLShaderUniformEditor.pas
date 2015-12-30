//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : FShaderUniformEditor<p>

  <b>History : </b><font size=-1><ul>
  <li>23/03/11 - Yar - Creation
  </ul>
}

unit FDGLShaderUniformEditor;

interface

{$I GLScene.inc}

uses
  System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.Buttons,
  // GLS
  DGLResStrings, DGLTypes, DGLSLParameters, DGLTextureFormat, DGLVectorMaths;

type
  TDGLShaderUniformEditor = class(TForm)
    LBUniforms: TListBox;
    Labe1: TLabel;
    AutoSetBox: TComboBox;
    SamplerBox: TComboBox;
    Panel1: TPanel;
    RedGroup: TRadioGroup;
    GreenGroup: TRadioGroup;
    BlueGroup: TRadioGroup;
    AlphaGroup: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TextureBox: TComboBox;
    Button1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure LBUniformsClick(Sender: TObject);
    procedure ColorGroupClick(Sender: TObject);
    procedure AutoSetBoxChange(Sender: TObject);
    procedure TextureBoxChange(Sender: TObject);
    procedure SamplerBoxChange(Sender: TObject);
    procedure LBUniformsKeyPress(Sender: TObject; var Key: Char);
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

function DGLShaderUniformEditor: TDGLShaderUniformEditor;
procedure ReleaseShaderUniformEditor;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

{$R *.dfm}

var
  vShaderUniformEditor: TDGLShaderUniformEditor;

function DGLShaderUniformEditor: TDGLShaderUniformEditor;
begin
  if not Assigned(vShaderUniformEditor) then
    vShaderUniformEditor := TDGLShaderUniformEditor.Create(nil);
  Result                 := vShaderUniformEditor;
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

procedure TDGLShaderUniformEditor.AddUniform(AValue: IShaderParameter);
begin
  if AValue <> nil then
  begin
    SetLength(FUniformList, Length(FUniformList) + 1);
    FUniformList[High(FUniformList)] := AValue;
  end;
end;

procedure TDGLShaderUniformEditor.AutoSetBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].AutoSetMethod := AutoSetBox.Items[AutoSetBox.ItemIndex];
  end;
end;

procedure TDGLShaderUniformEditor.Clear;
var
  I: Integer;
begin
  for I             := 0 to High(FUniformList) do
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
  RedGroup.ItemIndex   := -1;
  GreenGroup.ItemIndex := -1;
  BlueGroup.ItemIndex  := -1;
  AlphaGroup.ItemIndex := -1;
end;

procedure TDGLShaderUniformEditor.Execute;
var
  I:   Integer;
  str: AnsiString;
begin
  for I := 0 to High(FUniformList) do
  begin
    if FUniformList[I].GLSLType <> GLSLTypeUndefined then
      str := cGLSLTypeString[FUniformList[I].GLSLType];
    if FUniformList[I].GLSLSamplerType <> GLSLSamplerUndefined then
      str := cGLSLSamplerString[FUniformList[I].GLSLSamplerType];
    LBUniforms.Items.Add(FUniformList[I].Name + ': ' + string(str));
  end;
  ShowModal;
end;

procedure TDGLShaderUniformEditor.FormDestroy(Sender: TObject);
begin
  FUniformList := nil;
end;

procedure TDGLShaderUniformEditor.LBUniformsClick(Sender: TObject);
var
  SV:     TSwizzleVector;
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
      AutoSetBox.ItemIndex := MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      TextureBox.Enabled   := True;
      SamplerBox.Enabled   := True;
      TextureBox.ItemIndex := MaxInteger(TextureBox.Items.IndexOf(IParam.TextureName), 0);
      SamplerBox.ItemIndex := MaxInteger(SamplerBox.Items.IndexOf(IParam.SamplerName), 0);
      SV                   := IParam.GetTextureSwizzle;
      RedGroup.ItemIndex   := Ord(SV.V[0]);
      GreenGroup.ItemIndex := Ord(SV.V[1]);
      BlueGroup.ItemIndex  := Ord(SV.V[2]);
      AlphaGroup.ItemIndex := Ord(SV.V[3]);
    end
    else
    begin
      TextureBox.Enabled := False;
      SamplerBox.Enabled := False;
      FillUniformAutoSetMethodList(AutoSetBox.Items, IParam.GLSLType);
      AutoSetBox.ItemIndex := MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      RedGroup.ItemIndex   := -1;
      GreenGroup.ItemIndex := -1;
      BlueGroup.ItemIndex  := -1;
      AlphaGroup.ItemIndex := -1;
    end;
  end;
end;

procedure TDGLShaderUniformEditor.LBUniformsKeyPress(Sender: TObject; var Key: Char);
begin
  LBUniformsClick(Self);
end;

procedure TDGLShaderUniformEditor.SamplerBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].SamplerName := SamplerBox.Items[SamplerBox.ItemIndex];
  end;
end;

procedure TDGLShaderUniformEditor.TextureBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].TextureName := TextureBox.Items[TextureBox.ItemIndex];
  end;
end;

procedure TDGLShaderUniformEditor.ColorGroupClick(Sender: TObject);
var
  SV: TSwizzleVector;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    if FUniformList[LBUniforms.ItemIndex].GLSLSamplerType = GLSLSamplerUndefined then
      exit;
    SV                          := FUniformList[LBUniforms.ItemIndex].GetTextureSwizzle;
    SV.V[TRadioGroup(Sender).Tag] := TDGLTextureSwizzle(TRadioGroup(Sender).ItemIndex);
    FUniformList[LBUniforms.ItemIndex].SetTextureSwizzle(SV);
  end;
end;

procedure TDGLShaderUniformEditor.AddTextureName(const S: string);
begin
  TextureBox.Items.Add(S);
end;

procedure TDGLShaderUniformEditor.AddSamplerName(const S: string);
begin
  SamplerBox.Items.Add(S);
end;

initialization

finalization

ReleaseShaderUniformEditor;

end.
