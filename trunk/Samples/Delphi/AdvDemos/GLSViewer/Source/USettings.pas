//
//  uSettings for GLSViewer
//

unit uSettings;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Win.Registry, System.IniFiles, System.SysUtils,
  Vcl.Forms, Vcl.Graphics, Vcl.ActnList,
  //
  GNUGettext;

procedure InitGeneralRegistry;
procedure InitLanguage;


//-------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------

uses
  uGlobals;

procedure InitGeneralRegistry;
var
  RegIni:      TRegistryIniFile;
  FileVersion: cardinal;
begin
  GeneralSection := RegGLSViewer + 'General';
  FileVersion := GetFileVersion(Application.ExeName);
  ExePath := ExtractFilePath(Application.ExeName);
  RegIni := TRegistryIniFile.Create(GeneralSection);
  try
    with RegIni do
    begin
      if not RegIni.SectionExists(GeneralSection) then
      begin
        WriteString(GeneralSection, 'ExePath', ExePath);  //Don't translate the strings
        WriteInteger(GeneralSection, 'FileVersion', FileVersion);
        WriteString(GeneralSection, 'Licensee', 'MPL');
        WriteInteger(GeneralSection, 'Language', LANG_ENGLISH); //9, Default installation
        Language := LANG_ENGLISH;
      end
      else
      begin
        ExePath  := ReadString(GeneralSection, 'ExePath', ExePath);
        Language := ReadInteger(GeneralSection, 'Language', LANG_ENGLISH);
        //9, LANG_ENGLISH - Default
      end;
    end;
  finally
    RegIni.Free;
  end;

  if RegIni.ValueExists(GeneralSection,'SplashStart') then
    SplashStart := RegIni.ReadBool(GeneralSection,'SplashStart',False)
  else
    SplashStart := True;

  if RegIni.ValueExists(GeneralSection,'TipOfTheDay') then
    TipOfTheDay := RegIni.ReadBool(GeneralSection,'TipOfTheDay', True)
  else
    TipOfTheDay := False;

end;

procedure InitLanguage;
begin
  if Language <> LANG_ENGLISH then
  begin
    Textdomain('GLSceneVAV');
    AddDomainForResourceString('delphi');
    AddDomainForResourceString('language');
    TP_GlobalIgnoreClass(TFont);
    //TP_GlobalIgnoreClass(TGLLibMaterial);
    //TP_GlobalIgnoreClass(TGLMaterialLibrary);
    //TP_GlobalIgnoreClass(TListBox);
    TP_GlobalIgnoreClassProperty(TAction, 'Category');
    // Removing the upper line will cause long loading but Action.Category translation
  end;
  case Language of
    LANG_GERMAN:   //1
    begin
      UseLanguage('de'); //or de_DE
      Application.HelpFile := UpperCase(ExePath + 'Help' + PathDelim +
        'Ge' + PathDelim + 'GLSViewer.chm');
    end;
    LANG_ENGLISH: //2
    begin
      UseLanguage('en');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'En' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
    LANG_SPANISH:  //3
    begin
      UseLanguage('es');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'Es' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
    LANG_FRENCH:  //4
    begin
      UseLanguage('fr');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'Fr' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
    LANG_ITALIAN:  //5
    begin
      UseLanguage('it');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'Fr' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
    LANG_RUSSIAN: //6
    begin
      UseLanguage('ru');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'Ru' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
    else
    begin
      UseLanguage('en');
      Application.HelpFile := UpperCase(ExePath + 'Locale' + PathDelim +
        'En' + PathDelim + 'Help' + PathDelim + 'GLSViewer.chm');
    end;
  end;
  //LoadNewResourceModule(Language);//on using ITE DELPHI, ENU for English USA
end;

initialization
  InitGeneralRegistry;
end.
