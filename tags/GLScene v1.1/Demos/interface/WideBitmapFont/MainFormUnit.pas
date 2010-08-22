unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLBitmapFont, GLWindowsFont, GLScene, StdCtrls,
  GLWideBitmapFont, GLHUDObjects, GLWin32Viewer, GLSpaceText, GLCadencer,
  GLCrossPlatform, GLCoordinates, BaseClasses;

type
  TMainForm = class(TForm)
    GLScene1: TGLScene;
    GLWideBitmapFont1: TGLWideBitmapFont;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLHUDText1: TGLHUDText;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
Var
  Loader : TMemoryStream;
  WString : WideString;

begin
  Loader := TMemoryStream.Create;
  Loader.LoadFromFile('FontCharacters.txt');
  Loader.Seek(2,soFromBeginning	); // does unicode strings allways have a prefix?
  SetLength(WString, (Loader.Size-2) div 2);
  Loader.Read(WString[1], Loader.Size-2);
  GLWideBitmapFont1.WideCharacters := WString;

  Loader.LoadFromFile('HUDText.txt');
  Loader.Seek(2,soFromBeginning	); // does unicode strings allways have a prefix?
  SetLength(WString, (Loader.Size-2) div 2);
  Loader.Read(WString[1], Loader.Size-2);
  Loader.Free;
  GLHUDText1.Text := GLWideBitmapFont1.Encode(WString);
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
Var
  Luminance : Single;

begin
  Luminance := frac(newTime/10);
  Luminance := Sin(Luminance*pi);
  GLHUDText1.ModulateColor.SetColor(Luminance, Luminance, Luminance);
  GLHUDText1.Rotation := newTime*40;
  GLSceneViewer1.Invalidate;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  GLHUDText1.Position.X := GLSceneViewer1.Width/2;
  GLHUDText1.Position.Y := GLSceneViewer1.Height/2;
end;

end.
