unit FGLOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  IniFiles,

  //
  DGLSViewer, FGLForm, FGLDialog;

type
  TfmOptions = class(TGLDialog)
    CheckBoxAxis: TCheckBox;
    Label1: TLabel;
    RadioGroupLanguage: TRadioGroup;
    PanelBackground: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroupLanguageClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure PanelBackgroundClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurLangID : Word;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmOptions: TfmOptions;

implementation

{$R *.dfm}

uses
  GNUgettext, FMain;


procedure TfmOptions.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmOptions.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      CheckBoxAxis.Checked := ReadBool(Name, CheckBoxAxis.Name, True);
      PanelBackground.Color  := ReadInteger(Name, PanelBackground.Name, 0);
      LangID := ReadInteger(Name, RadioGroupLanguage.Name, 0);
      case LangID of
        LANG_ENGLISH : RadioGroupLanguage.ItemIndex := 0;
        LANG_RUSSIAN : RadioGroupLanguage.ItemIndex := 1;
        LANG_SPANISH : RadioGroupLanguage.ItemIndex := 2;
        LANG_FRENCH  : RadioGroupLanguage.ItemIndex := 3;
        LANG_GERMAN  : RadioGroupLanguage.ItemIndex := 4;
        LANG_ITALIAN : RadioGroupLanguage.ItemIndex := 5;
        else
          RadioGroupLanguage.ItemIndex := 0;
      end;
    finally
      IniFile.Free;
    end;
end;

procedure TfmOptions.RadioGroupLanguageClick(Sender: TObject);
begin
  case RadioGroupLanguage.ItemIndex of
    0: CurLangID := LANG_ENGLISH;
    1: CurLangID := LANG_RUSSIAN;
    2: CurLangID := LANG_SPANISH;
    3: CurLangID := LANG_FRENCH;
    4: CurLangID := LANG_GERMAN;
    5: CurLangID := LANG_ITALIAN;
    else
      CurLangID := LANG_ENGLISH;
  end;
end;

procedure TfmOptions.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      WriteBool(Name, CheckBoxAxis.Name, CheckBoxAxis.Checked);
      WriteInteger(Name, RadioGroupLanguage.Name, CurLangID);
      WriteInteger(Name, PanelBackground.Name, PanelBackground.Color);
    finally
      IniFile.Free;
    end;
  inherited;
end;

procedure TfmOptions.PanelBackgroundClick(Sender: TObject);
var
   bmp : TBitmap;
   col : TColor;

begin
   dmGLSViewer.ColorDialog.Color := PanelBackground.Color;
   if dmGLSViewer.ColorDialog.Execute then
   begin
     PanelBackground.Color :=  dmGLSViewer.ColorDialog.Color;
     Main.ApplyBgColor;
   end;
end;

procedure TfmOptions.ButtonOKClick(Sender: TObject);
var
  FileName: TFileName;
begin
  if CurLangID <> LangID then
  begin
    MessageDlg(_('Reload to change language'),
      mtInformation, [mbOK], 0);
    FileName := ChangeFileExt(Application.ExeName, '.ini');
    if FileExists(UpperCase(FileName)) then
      DeleteFile(UpperCase(FileName)); //to exclude dublicated sections for each language
  end;
end;

end.
