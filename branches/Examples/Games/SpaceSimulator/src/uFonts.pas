unit uFonts;

interface

uses
  Classes, Windows,
  Contnrs,
  GLScene, GLWindowsFont;

const
  C_FONT_1 = 'data\BankGothic RUSS Medium.ttf';
  C_FONT_2 = 'data\Journal Regular.ttf';

function AddFont(aOwner: TComponent; aFontPath: String): Integer;
function GetFont(aIndex: Integer): TGLWindowsBitmapFont; overload;
function GetFont(aFontPath: String): TGLWindowsBitmapFont; overload;

implementation

uses
  SysUtils,
  uLog;

type
  TdfFontRec = class
    GLFont: TGLWindowsBitmapFont;
    fontPath: String;
  end;

var
  Fonts: TObjectList;

function AddFont(aOwner: TComponent; aFontPath: String): Integer;
var
  i, a: Integer;
  rec: TdfFontRec;
begin
  for i := 0 to Fonts.Count - 1 do
    if TdfFontRec(Fonts[i]).fontPath = aFontPath then
    begin
      Result := i;
      Exit;
    end;
  a := AddFontResourceExW(PWideChar(aFontPath), FR_PRIVATE, nil);
  if a = 0 then
  begin
    logWriteError('uFonts: Error while adding font from ' + aFontPath);
    Result := -1;
    Exit;
  end
  else
  begin
    logWriteMessage('uFonts: Font added from ' + aFontPath);
    rec := TdfFontRec.Create;
    rec.GLFont := TGLWindowsBitmapFont.Create(aOwner);
    rec.fontPath := aFontPath;
    rec.GLFont.Font.Name := Copy(ExtractFileName(aFontPath), 0, Length(ExtractFileName(aFontPath)) - 4);
    rec.GLFont.Font.Charset := RUSSIAN_CHARSET;
    rec.GLFont.Ranges.Add(#32, #255);
    Result := Fonts.Add(rec);
  end;
end;

function GetFont(aIndex: Integer): TGLWindowsBitmapFont;
begin
  if aIndex < Fonts.Count then
    Result := TdfFontRec(Fonts[aIndex]).GLFont
  else
  begin
    Result := nil;
    logWriteError('uFonts: No font with index ' + IntToStr(aIndex)
     + '. Last index is ' + IntToStr(Fonts.Count - 1));
  end;
end;

function GetFont(aFontPath: String): TGLWindowsBitmapFont; overload;
var
  i: Integer;
begin
  for i := 0 to Fonts.Count - 1 do
    if TdfFontRec(Fonts[i]).fontPath = aFontPath then
    begin
      Result := TdfFontRec(Fonts[i]).GLFont;
      Exit;
    end;
  Result := nil;
  logWriteError('uFonts: Thre is no font from ' + aFontPath + ' in font list!');
end;

initialization
  Fonts := TObjectList.Create;

finalization
  Fonts.Free;

end.
