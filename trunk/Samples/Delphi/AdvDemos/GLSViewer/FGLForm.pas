//------------------------------------------------------------------------------
// This unit is part of the GLSceneViewer, http://sourceforge.net/projects/glscene
//------------------------------------------------------------------------------
{! The FGLForm unit for TGLForm class as parent for all child forms of GLSceneViewer project}
{
  History :
     01/04/09 - PW - Created
}

unit FGLForm;

interface

uses
  Windows,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.Actnlist,

  SysUtils,
  IniFiles;

type
  TGLForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IniFile : TIniFile;
    procedure ReadIniFile; virtual;
    procedure WriteIniFile; virtual;
    procedure SetLanguage;
  published
    //
  protected
  end;

var
  GLForm: TGLForm;

var
  LangID : Word;

implementation

uses
  Gnugettext;

{$R *.dfm}

//Here goes the translation of all component strings
//
procedure TGLForm.FormCreate(Sender: TObject);
begin
  SetLanguage;
  TranslateComponent(Self);
end;

procedure TGLForm.SetLanguage;
var
  I : Integer;
  GLScenePath : TFileName;
  IniFile : TIniFile;

begin
  GLScenePath := ExtractFilePath(ParamStr(0)); // Path to GLSViewer
  I := Pos(UpperCase('glscene'), UpperCase(GLScenePath));
  if (I <> 0) then
  begin
    Delete(GLScenePath, I+7, Length(GLScenePath)-I);
    GLScenePath := GLScenePath + PathDelim + 'Locale' + PathDelim;
  end;

  Textdomain('glscene');
  BindTextDomain ('glscene', GLScenePath);

  ReadIniFile;
  if (LangID <> LANG_ENGLISH) then
  begin
    case LangID of
      LANG_RUSSIAN:
      begin
        UseLanguage('ru');
        Application.HelpFile := UpperCase(GLScenePath + 'ru'+ PathDelim+'GLScene.chm');
      end;
      LANG_SPANISH:
      begin
        UseLanguage('es');
        Application.HelpFile := UpperCase(GLScenePath + 'es'+ PathDelim+'GLScene.chm');
      end;
      LANG_GERMAN:
      begin
        UseLanguage('ru');
        Application.HelpFile := UpperCase(GLScenePath + 'ru'+ PathDelim+'GLScene.chm');
      end;
      LANG_FRENCH:
      begin
        UseLanguage('ru');
        Application.HelpFile := UpperCase(GLScenePath + 'ru'+ PathDelim+'GLScene.chm');
      end
      else
      begin
        UseLanguage('en');
        Application.HelpFile := UpperCase(GLScenePath + 'en'+ PathDelim+'GLScene.chm');
      end;
    end;
  end
  else
  begin
    UseLanguage('en');
    Application.HelpFile := UpperCase(GLScenePath + 'en'+ PathDelim+'GLScene.chm');
  end;
  TP_IgnoreClass(TFont);
end;



procedure TGLForm.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      LangID := ReadInteger('fmOptions', 'RadioGroupLanguage', 0);
    finally
      IniFile.Free;
    end;
end;


procedure TGLForm.WriteIniFile;
begin
  //
end;

end.
