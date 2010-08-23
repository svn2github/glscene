unit GLSLauncher;

interface

{$I GLScene.inc}

uses
  Windows,
  Classes, Forms, SysUtils, JPEG, ExtCtrls, Controls, Graphics,
  ApplicationFileIO, GLSCrossXML, GLSArchiveManager;

type

  TUpdateResourceProc = procedure(Data: Pointer; Size: Integer) of object;
  TGetConfigDataStreamFunc = function(): TStream of object;

  TGLSLauncher = class(TComponent)
  private
    FSplashScreen: TJPEGImage;
    FShowSplashScreen: Boolean;
    FWaitInterval: Integer;
    FArchManager: TGLSArchiveManager;
    procedure SetShowSplashScreen(Value: Boolean);
    procedure SetWaitInterval(Value: Integer);
    procedure SetArchManager(Value: TGLSArchiveManager);
    procedure CreateConfig;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSplashScreenImage(AFileName: string);
  published
    property ShowSplashScreen: Boolean read FShowSplashScreen
      write SetShowSplashScreen default False;
    property WaitInterval: Integer read FWaitInterval write SetWaitInterval default 0;
    property ArchiveManager: TGLSArchiveManager read FArchManager write SetArchManager;
  end;

var
  vUpdateResourceProc: TUpdateResourceProc;
  vGetConfigDataStreamFunc: TGetConfigDataStreamFunc;

implementation

uses
  GLSLog, GLContext, GL3xMaterial, GLStrings;

var
  OldInitProc: Pointer;
  FWindowHandle: HWND;
  FDC: HDC;
  FRenderingContext: TGLContext;

procedure LauncherInitProc;
var
  rStream: TGLSResourceStream;
  Config: GLSXMLDocument;
  XMLConfig: GLSXMLNode;
  img: TJPEGImage;
  form: TForm;
  ACanvas: TImage;
  sConfig: WideString;
  temp: string;
  vWait: Cardinal;
  len, err: Integer;
begin
  if Assigned(OldInitProc) then
    TProcedure(OldInitProc);
  FRenderingContext := GLContextManager.CreateResourceContext;
  if not Assigned(FRenderingContext) then
  begin
    GLSLogger.LogError('GLSStarter: can''t create rendering context');
    exit;
  end;
//  FWindowHandle := Classes.AllocateHWnd(nil);
  FDC := GetDC(GetForegroundWindow);
  if FDC = 0 then
  begin
    GLSLogger.LogError('GLSStarter: can''t get device context');
    exit;
  end;
  try
    FRenderingContext.CreateMemoryContext(FDC, 1, 1, 1);
  except
    on EGLContext do
    begin
      GLSLogger.LogError('GLSStarter: can''t initialize rendering context');
    end
    else raise;
  end;
  GLSLogger.LogNotice('Resource context successfuly initialized');

  form := nil;
  vWait := 0;

  rStream := CreateResourceStream(glsLauncherData, GLS_RC_XML_Type);
  if Assigned(rStream) then
  begin
    rStream.Read(len, SizeOf(Integer));
    SetLength(sConfig, len);
    rStream.Read(sConfig[1], len*SizeOf(Char));
    Config := GLSNewXMLDocument;
    Config.LoadFromXML(sConfig);

    XMLConfig := Config.DocumentElement;
    if GetXMLAttribute(XMLConfig, 'SplashScreen', temp) then
    begin
      img := TJPEGImage.Create;
      img.LoadFromStream(rStream);
    end
    else
      img := nil;

    if GetXMLAttribute(XMLConfig, 'Wait', temp) then
      Val(temp, vWait, err);

  {$IFDEF FPC}
    FConfig.Free;
  {$ENDIF}
    rStream.Free;
    GLSLogger.LogNotice('Launcher successful loaded config');

    if Assigned(img) then
    begin
      form := TForm.Create(Application);
      form.Position := poScreenCenter;
      form.Width := img.Width;
      form.Height := img.Height;
      form.BorderStyle := bsNone;
      form.FormStyle := fsStayOnTop;
      form.Color := clBlack;
      ACanvas := TImage.Create(form);
      ACanvas.Parent := form;
      ACanvas.Width := img.Width;
      ACanvas.Height := img.Height;
      ACanvas.Canvas.Draw(0, 0, img);
      form.Show;
      form.Repaint;
      img.Free;
      GLSLogger.LogNotice('Splash-screen successful raised');
    end;
  end;

  FRenderingContext.Activate;
  MaterialManager.Initialize;
  FRenderingContext.Deactivate;
  Sleep(vWait);
  form.Free;
end;

// ------------------
// ------------------ TGLSLauncher ------------------
// ------------------

constructor TGLSLauncher.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TGLSLauncher.Destroy;
begin
  FSplashScreen.Free;
  inherited;
end;

procedure TGLSLauncher.Loaded;
var
  rStream: TStream;
  len: Integer;
  temp: string;
  sConfig: WideString;
  Config: GLSXMLDocument;
  XMLConfig: GLSXMLNode;
begin
  if Assigned(vGetConfigDataStreamFunc) then
  begin
    rStream := vGetConfigDataStreamFunc();
    if Assigned(rStream) then
    begin
      rStream.Read(len, SizeOf(Integer));
      SetLength(sConfig, len);
      rStream.Read(sConfig[1], len*SizeOf(WideChar));
      Config := GLSNewXMLDocument;
      Config.LoadFromXML(sConfig);
      XMLConfig := Config.DocumentElement;
      if GetXMLAttribute(XMLConfig, 'SplashScreen', temp) then
      begin
        FSplashScreen := TJPEGImage.Create;
        FSplashScreen.LoadFromStream(rStream);
      end;
{$IFDEF FPC}
      Config.Free;
{$ENDIF}
      rStream.Free;
    end;
  end;
  inherited;
end;

procedure TGLSLauncher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FArchManager) then
  begin
    FArchManager := nil;
    CreateConfig;
  end;
end;

procedure TGLSLauncher.SetShowSplashScreen(Value: Boolean);
begin
  if Value <> FShowSplashScreen then
  begin
    FShowSplashScreen := Value;
    CreateConfig;
  end;
end;

procedure TGLSLauncher.SetWaitInterval(Value: Integer);
begin
  if Value<0 then
    Value := 0;
  if Value <> FWaitInterval then
  begin
    FWaitInterval := Value;
    CreateConfig;
  end;
end;

procedure TGLSLauncher.SetArchManager(Value: TGLSArchiveManager);
begin
  if FArchManager <> nil then
    FArchManager.RemoveFreeNotification(Self);
  FArchManager := Value;
  if FArchManager <> nil then
    FArchManager.FreeNotification(Self);
  CreateConfig;
end;

procedure TGLSLauncher.LoadSplashScreenImage(AFileName: string);
begin
  if not(csDesigning in ComponentState) then
    exit;
  if not Assigned(FSplashScreen) then
    FSplashScreen := TJPEGImage.Create;
  FSplashScreen.LoadFromFile(AFileName);
  CreateConfig;
end;

procedure TGLSLauncher.CreateConfig;
var
  Config: GLSXMLDocument;
  XMLConfig: GLSDOMNode;
  sConfig: WideString;
  mStream: TMemoryStream;
  len: Integer;
begin
  if not(csDesigning in ComponentState) or (csLoading in ComponentState) then
    exit;

  Config := GLSNewXMLDocument;
  XMLConfig := Config.DOMDocument.CreateElement('StarterConfig');
  Config.DOMDocument.AppendChild(XMLConfig);
  if FShowSplashScreen and Assigned(FSplashScreen) then
    SetXMLAttribute(XMLConfig, 'SplashScreen', 'True');
  SetXMLAttribute(XMLConfig, 'Wait', IntToStr(FWaitInterval));
  Config.SaveToXML(sConfig);
  len := Length(sConfig);
  mStream := TMemoryStream.Create;
  try
    mStream.Write(len, SizeOf(Integer));
    mStream.Write(sConfig[1], len*SizeOf(WideChar));
    if FShowSplashScreen and Assigned(FSplashScreen) then
      FSplashScreen.SaveToStream(mStream);
    mStream.Seek(0, soBeginning);
    if Assigned(vUpdateResourceProc) then
      vUpdateResourceProc(mStream.Memory, Integer(mStream.Size));
  finally
{$IFDEF FPC}
    FConfig.Free;
{$ENDIF}
    mStream.Free;
  end;
end;

initialization

  OldInitProc := InitProc;
  InitProc := @LauncherInitProc;

finalization

  FreeAndNil(FRenderingContext);
  if FWindowHandle <> 0 then
    Classes.DeallocateHWnd(FWindowHandle);

end.
