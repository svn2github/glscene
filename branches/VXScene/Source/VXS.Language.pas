//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  En:
  Language created to localize your application.
  In Delphi, the text is encoded using Ansi cp1251 and can not be encoded \ decoding.
  In Lazarus has the ability to upload text from any encoding.
  Ru:
  TVXLanguage создан для локализации вашего приложения
  В Delphi текст имеет кодировку Ansi cp1251 и не подлежит кодировке\декодировке.
  В Lazarus можно загружать текст любой кодировки
   
}
unit VXS.Language;

interface

{$I VXScene.inc}

uses
  System.Classes, System.IniFiles, System.SysUtils;

type

  TVXLanguageEntry = record
    ID: String; // **< identifier
    Text: String; // **< translation
  end;

  TVXLanguageEntryArray = array of TVXLanguageEntry;

  { TVXLanguage }
  { **
    * Eng
    *   Class TVXLanguage is used for downloading and translation, as in the final product it's no need for text processing.
    * Ru
    *   Класс TVXLanguage используется толко для загрузки и перевода текста, так как в конечном
    *   продукте нет необходимости в обработке текста.
    * }
  TVXLanguage = class
  private
    FCurrentLanguageFile: String;
    Entry: TVXLanguageEntryArray; // **< Entrys of Chosen Language
  public
    function FindID(const ID: String): integer;
    function Translate(const ID: String): String;
    procedure LoadLanguageFromFile(const Language: String);
    property CurrentLanguageFile: String read FCurrentLanguageFile;
  end;

  { **
    * Eng
    *   Advanced class is designed for loading and processing, will be useful for the editors of language.
    * Ru
    *   Расширенный класс созданный для загрузки и обработки текста, будет полезен для редакторов языка.
    * }
  TVXLanguageExt = class(TVXLanguage)
  private
    function GetEntry(Index: integer): TVXLanguageEntry;
    procedure SetEntry(Index: integer; aValue: TVXLanguageEntry);
    function GetCount: integer;
  public
    procedure AddConst(const ID: String; const Text: String);
    procedure AddConsts(aValues: TStrings);
    procedure ChangeConst(const ID: String; const Text: String);
    property Items[Index: integer]: TVXLanguageEntry read GetEntry write SetEntry;
    property Count: integer read GetCount;
    procedure SaveLanguageFromFile(const Language: String); overload;
    procedure SaveLanguageFromFile; overload;
  end;

  { TVXSLanguage }

  { Abstract class for control Language.  }
  { Абстрактный класс,  для палитры компонентов  }
  TVXSLanguage = class(TComponent)
  private
    FLanguage: TVXLanguageExt;
    FLanguageList: TStrings;
    procedure SetLanguage(aValue: TVXLanguageExt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguageFromFile(const Language: String);
    procedure SaveLanguageFromFile(const Language: String); overload;
    procedure SaveLanguageFromFile; overload;
    function Translate(const ID: String): String;
    property Language: TVXLanguageExt read FLanguage write SetLanguage;
  end;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

uses
  VXS.CrossPlatform;

{ TVXLanguage }

{ **
  * Load the specified LanguageFile
  * }
procedure TVXLanguage.LoadLanguageFromFile(const Language: String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
  S: TStringList;
  I: integer;
begin
  If Language = '' then
    Exit;
  if not FileExists(string(Language)) then
  begin
{$IFDEF USE_LOGGING}
    GLSLogger.LogFatalError(ExtractFileName(string(Language)) +
      ' Languagefile missing!');
{$ENDIF}
    Exit;
  end;
  SetLength(Entry, 0);
  FCurrentLanguageFile := Language;
  IniFile := TMemIniFile.Create(string(Language));
  S := TStringList.Create;

  IniFile.ReadSectionValues('Text', S);

  // Problem Solving with symbols wrap (#13#10)
  I := 0;
  for E := 0 to S.Count - 1 do
  begin
    If S.Names[E] = '' then
    begin
      S.Strings[I] := S.Strings[I] + #13#10 + GetValueFromStringsIndex(S, E);
    end
    else
      I := E;
  end;

  SetLength(Entry, S.Count);
  for E := 0 to high(Entry) do
    If S.Names[E] <> '' then
    begin
      Entry[E].ID := S.Names[E];
      Entry[E].Text := GetValueFromStringsIndex(S, E);
    end;
  S.Free;
  IniFile.Free;
end;

{ **
  * Find the index of ID an array of language entry.
  * @returns the index on success, -1 otherwise.
  * }
function TVXLanguage.FindID(const ID: String): integer;
var
  Index: integer;
begin
  for Index := 0 to High(Entry) do
  begin
    if UpperCase(string(ID)) = UpperCase(string(Entry[Index].ID)) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result := -1;
end;

{ **
  * Translate the Text.
  * If Text is an ID, text will be translated according to the current language
  * setting. If Text is not a known ID, it will be returned as is.
  * @param Text either an ID or an UTF-8 encoded string
  * }
function TVXLanguage.Translate(const ID: String): String;
var
  EntryIndex: integer;
begin
  // fallback result in case Text is not a known ID
  Result := ID;

  // Check if ID exists

  EntryIndex := FindID(ID);
  if (EntryIndex >= 0) then
  begin
    Result := Entry[EntryIndex].Text;
    Exit;
  end;
end;

{ TVXLanguageExt }

{ **
  * Add a Constant ID that will be Translated but not Loaded from the LanguageFile
  * }
procedure TVXLanguageExt.AddConst(const ID: String; const Text: String);
begin
  SetLength(Entry, Length(Entry) + 1);
  Entry[high(Entry)].ID := ID;
  Entry[high(Entry)].Text := Text;
end;

procedure TVXLanguageExt.AddConsts(aValues: TStrings);
var
  I: integer;
begin
  if aValues <> nil then
    for I := 0 to aValues.Count - 1 do
      If aValues.Names[I] <> '' then
        AddConst(aValues.Names[I],GetValueFromStringsIndex(aValues, I));
end;

{ **
  * Change a Constant Value by ID
  * }
procedure TVXLanguageExt.ChangeConst(const ID: String;
  const Text: String);
var
  I: integer;
begin
  for I := 0 to high(Entry) do
  begin
    if Entry[I].ID = ID then
    begin
      Entry[I].Text := Text;
      Break;
    end;
  end;
end;

function TVXLanguageExt.GetEntry(Index: integer): TVXLanguageEntry;
begin
  Result := Entry[Index];
end;

procedure TVXLanguageExt.SetEntry(Index: integer; aValue: TVXLanguageEntry);
begin
  Entry[Index] := aValue;
end;

function TVXLanguageExt.GetCount: integer;
begin
  Result := high(Entry) + 1;
end;

{ **
  * Save Update Language File
  * }
procedure TVXLanguageExt.SaveLanguageFromFile(const Language: String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
begin
  if Language = '' then
    Exit;

  IniFile := TMemIniFile.Create(string(Language));

  for E := 0 to Count - 1 do
  begin
    IniFile.WriteString('Text', string(Items[E].ID), string(Items[E].Text));
  end;
  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TVXLanguageExt.SaveLanguageFromFile;
begin
  SaveLanguageFromFile(CurrentLanguageFile);
end;

{ TVXSLanguage }

constructor TVXSLanguage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguage := TVXLanguageExt.Create;
  FLanguageList := TStringList.Create;
end;

destructor TVXSLanguage.Destroy;
begin
  FLanguage.Free;
  FLanguageList.Free;
  inherited Destroy;
end;

procedure TVXSLanguage.LoadLanguageFromFile(const Language: String);
begin
  FLanguage.LoadLanguageFromFile(Language);
end;

procedure TVXSLanguage.SetLanguage(aValue: TVXLanguageExt);
begin
  if aValue <> nil then
    FLanguage := aValue;
end;

procedure TVXSLanguage.SaveLanguageFromFile(const Language: String);
begin
  if Language = '' then
    Exit;

  FLanguage.SaveLanguageFromFile(Language);
end;

procedure TVXSLanguage.SaveLanguageFromFile;
begin
  FLanguage.SaveLanguageFromFile;
end;

function TVXSLanguage.Translate(const ID: String): String;
begin
  Result := FLanguage.Translate(ID);
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
initialization

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

RegisterClass(TVXSLanguage);

end.
