unit FShaderMemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo,

  GLS.Memo;

type
  TShaderMemoForm = class(TForm)
    ToolBar1: TToolBar;
    Panel1: TPanel;
    ButtonOK: TButton;
    ImageOK: TImage;
    ButtonCancel: TButton;
    ImageCancel: TImage;
    CheckButton: TSpeedButton;
    CompilatorLog: TMemo;
    SBOpen: TSpeedButton;
    Image1: TImage;
    SBSave: TSpeedButton;
    Image2: TImage;
    SBHelp: TSpeedButton;
    Image3: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GLShaderEditorForm: TShaderMemoForm;
procedure ReleaseGLShaderEditor;


implementation

{$R *.fmx}

const
  cRegistryKey = 'Software\GLScene\GLSceneShaderEdit';

type
  TFriendlyMemo = class(TGLSSynHiMemo);

var
  vGLShaderEditor: TShaderMemoForm;

function GLShaderEditorForm: TShaderMemoForm;
begin
  if not Assigned(vGLShaderEditor) then
    vGLShaderEditor := TShaderMemoForm.Create(nil);
  Result := vGLShaderEditor;
end;

procedure ReleaseGLShaderEditor;
begin
  if Assigned(vGLShaderEditor) then
  begin
    vGLShaderEditor.Free;
    vGLShaderEditor := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;


procedure TShaderMemoForm.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  No: Integer;
  item: TMenuItem;
begin
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 500);
      Height := ReadRegistryInteger(reg, 'Height', 640);
    end;
  finally
    reg.Free;
  end;

  No := GLSLMemo.Styles.Add(clRed, clWhite, []);
  GLSLMemo.AddWord(No, GLSLDirectives);

  No := GLSLMemo.Styles.Add(clPurple, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLQualifiers);

  No := GLSLMemo.Styles.Add(clBlue, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLTypes);

  No := GLSLMemo.Styles.Add(clGray, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLBuildIn);

  No := GLSLMemo.Styles.Add(clGreen, clWhite, [fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  No := GLSLMemo.Styles.Add(clYellow, clSilver, [fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  FLightLineStyle := GLSLMemo.Styles.Add(clBlack, clLtGray, []);

  GLSLMemo.MultiCommentLeft := '/*';
  GLSLMemo.MultiCommentRight := '*/';
  GLSLMemo.LineComment := '//';

  GLSLMemo.CaseSensitive := True;
  GLSLMemo.DelErase := True;

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 5;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 0;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program with TBN pass', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 1;
  GLSL120.Add(item);

  item := NewItem('Basic fragment program, Phong lighting', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 2;
  GLSL120.Add(item);

  item := NewItem('Fragment program, normal mapping', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 3;
  GLSL120.Add(item);

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 6;
  GLSL330.Add(item);

  item := NewItem('Geometry program, edge detection', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 4;
  GLSL330.Add(item);
end;

end.
