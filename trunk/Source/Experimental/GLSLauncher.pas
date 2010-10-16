unit GLSLauncher;

interface

{$I GLScene.inc}

uses
  Windows,
  Classes,
  Forms,
  SysUtils,
  JPEG,
  ExtCtrls,
  Controls,
  Graphics,
  BaseClasses,
  GLSArchiveManager;

type

  TUpdateResourceProc = procedure(Data: Pointer; Size: Integer) of object;
  TGetConfigDataStreamFunc = function(): TStream of object;

  TGLSLauncher = class(TComponent)
  private
    FShowSplashScreen: Boolean;
    FArchManager: TGLSArchiveManager;
    procedure SetShowSplashScreen(Value: Boolean);
    function GetWaitInterval: Integer;
    procedure SetWaitInterval(Value: Integer);
    procedure SetArchManager(Value: TGLSArchiveManager);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSplashScreenImage(AFileName: string);
  published
    property ShowSplashScreen: Boolean read FShowSplashScreen
      write SetShowSplashScreen default False;
    property WaitInterval: Integer read GetWaitInterval write SetWaitInterval default 0;
    property ArchiveManager: TGLSArchiveManager read FArchManager write SetArchManager;
  end;

  LaunchManager = class(TGLSAbstractManager)
  public
    { Public Declarations }
    class procedure NotifyContextCreated; override;
    class function FillResourceList(AList: TStringList): Boolean; override;
    class function FirstOne: Boolean; override;
    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
  end;

implementation

uses
  ApplicationFileIO,
  GLCrossPlatform,
  GLSLog,
  GLContext,
  GLStrings;

var
  vLauncher: TGLSLauncher;
  SplashImage: TJPEGImage;
  WaitInterval: Integer;

class procedure LaunchManager.NotifyContextCreated;
var
  SplashWindow: TForm;
  SplashCanvas: TImage;
begin
  SplashWindow := GetServiceWindow;
  if not IsDesignTime and Assigned(SplashWindow) and Assigned(SplashImage) then
  begin
    SplashCanvas := TImage.Create(SplashWindow);
    SplashCanvas.Parent := SplashWindow;
    SplashWindow.Width := SplashImage.Width;
    SplashWindow.Height := SplashImage.Height;
    SplashCanvas.Width := SplashImage.Width;
    SplashCanvas.Height := SplashImage.Height;
    SplashCanvas.Canvas.Draw(0, 0, SplashImage);
    SplashWindow.Show;
    SplashWindow.Repaint;
    Sleep(WaitInterval);
  end;
end;

class function LaunchManager.FillResourceList(AList: TStringList): Boolean;
var
  temp: string;
  mStream: TMemoryStream;
begin
  Result := Assigned(vLauncher);
  if Result then
  begin
    AList.Add('[SPLASH]');
    AList.Add(Format('WAIT = %d', [WaitInterval]));
    if vLauncher.FShowSplashScreen then
    begin
      mStream := TMemoryStream.Create;
      SplashImage.SaveToStream(mStream);
      SetLength(temp, 2 * mStream.Size);
      BinToHex(mStream.Memory^, PChar(temp), Integer(mStream.Size));
      mStream.Destroy;
      AList.Add(Format('SPLASH = %s', [temp]));
    end;
  end;
end;

class function LaunchManager.FirstOne: Boolean;
begin
  Result := True;
end;

class procedure LaunchManager.NotifyProjectOpened;
var
  I, E: Integer;
  rStream: TGLSResourceStream;
  mStream: TMemoryStream;
  eResType: TGLSApplicationResource;
  ResList: TStringList;
  line, rName, rFile: string;

  procedure GetNameAndFile;
  var
    p: Integer;
  begin
    p := Pos('=', line);
    rName := Copy(line, 1, p - 1);
    rFile := Copy(line, p + 1, Length(line) - p + 1);
  end;

begin
  GLSLogger.Enabled := False;
  if IsDesignTime then
  begin
    if Assigned(vAFIOGetAppResourceStream) then
      rStream := TGLSResourceStream(vAFIOGetAppResourceStream())
    else
      rStream := nil;
  end
  else
    rStream := CreateResourceStream(glsResourceInfo, GLS_RC_String_Type);
  GLSLogger.Enabled := True;

  if Assigned(rStream) then
  begin
    ResList := TStringList.Create;
    ResList.LoadFromStream(rStream);
    rStream.Destroy;
    eResType := aresNone;
    SetExeDirectory;
    for I := 0 to ResList.Count - 1 do
    begin
      line := ResList.Strings[I];
      if line = '[SPLASH]' then
      begin
        eResType := aresSplash;
        continue;
      end
      else
        case eResType of
          aresSplash:
            begin
              GetNameAndFile;
              if rName = 'Wait' then
              begin
                WaitInterval := 0;
                Val(rFile, WaitInterval, E);
              end
              else if rName = 'SPLASH' then
              begin
                mStream := TMemoryStream.Create;
                mStream.SetSize(Length(rFile) div 2);
                HexToBin(PChar(rFile), mStream.Memory^, Integer(mStream.Size));
                SplashImage := TJPEGImage.Create;
                SplashImage.LoadFromStream(mStream);
                mStream.Destroy;
              end;
            end;
        end;
    end;
    ResList.Destroy;
  end;
end;

class procedure LaunchManager.NotifyProjectClosed;
begin
  FreeAndNil(SplashImage);
end;

// ------------------
// ------------------ TGLSLauncher ------------------
// ------------------

constructor TGLSLauncher.Create(AOwner: TComponent);
begin
  if vLauncher = nil then
    vLauncher := Self;
  inherited;
end;

destructor TGLSLauncher.Destroy;
begin
  if vLauncher = Self then
    vLauncher := nil;
  inherited;
end;

procedure TGLSLauncher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FArchManager) then
  begin
    FArchManager := nil;
  end;
end;

procedure TGLSLauncher.SetShowSplashScreen(Value: Boolean);
begin
  if Value <> FShowSplashScreen then
  begin
    FShowSplashScreen := Value;
  end;
end;

function TGLSLauncher.GetWaitInterval: Integer;
begin
  Result := WaitInterval;
end;

procedure TGLSLauncher.SetWaitInterval(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> WaitInterval then
  begin
    WaitInterval := Value;
  end;
end;

procedure TGLSLauncher.SetArchManager(Value: TGLSArchiveManager);
begin
  if FArchManager <> nil then
    FArchManager.RemoveFreeNotification(Self);
  FArchManager := Value;
  if FArchManager <> nil then
    FArchManager.FreeNotification(Self);
end;

procedure TGLSLauncher.LoadSplashScreenImage(AFileName: string);
begin
  if not (csDesigning in ComponentState) then
    exit;
  if not Assigned(SplashImage) then
    SplashImage := TJPEGImage.Create;
  SplashImage.LoadFromFile(AFileName);
end;

initialization

  RegisterGLSceneManager(LaunchManager);

finalization

  FreeAndNil(SplashImage);

end.

