{Меню в игровом процессе}
unit uGameScreen.InGameMenu;

interface

uses
  Windows,

  GLScene, GLObjects, GLWin32Viewer, GLHUDObjects, GLMaterial, GLTexture,
  GLFilePNG, GLFileDDS, GLVectorGeometry, GLVectorTypes, GLRenderContextInfo,
  GLKeyboard,

  uGameScreen, uGUIButton;

const
  C_SCREEN_NAME = 'InGame Menu';
  C_INGAMEMENU_PATH = 'data\menu\';

  C_PANEL_ALPHA = 0.9;
  C_PANEL_X = 0;
  C_PANEL_Y = 0;

  C_BUTTON_SCALE = 1.0;

  C_BUTTON_BACKTOGAME_X = 0;
  C_BUTTON_BACKTOGAME_Y = -50;

  C_BUTTON_BACKTOMENU_X = 0;
  C_BUTTON_BACKTOMENU_Y = 0;

  C_BUTTON_EXIT_X = 0;
  C_BUTTON_EXIT_Y = 50;

  C_INGAMEMENU_FADEIN_TIME  = 1.0;
  C_INGAMEMENU_FADEOUT_TIME = 0.8;

type
  TdfInGameMenu = class (TdfGameScreen)
  private
    FRoot: TGLDummyCube;

    FDir: TGLDirectOpenGL;

    Ft: Double;

    FGame, FMainMenu: TdfGameScreen;

    FPanel: TGLHUDSprite;
    FReturnButton, FMainMenuButton, FExitButton: TdfGUIButton;

    c_green_vec, c_white_vec: TVector;
    procedure OnRender(Sender: TObject; var rci: TGLRenderContextInfo);

    procedure FadeInComplete();
    procedure FadeOutComplete();
    procedure AddButtonMaterial(aObject: TdfGUIButton; aTextureName: String);
    procedure AddPanelMaterial(aObject: TGLHUDSprite; aTextureName: String);

    //Общие коллбэки
    procedure MouseMove(Sender: TdfGUIButton; OldMS, NewMS: TdfMouseState);
    procedure MouseClick(Sender: TdfGUIButton; MouseButton: TdfMouseButton);

    //Колббэки для отдельных кнопок
    procedure MouseClickToGame(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    procedure MouseClickToMenu(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    procedure MouseClickExit(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
  protected
    procedure FadeIn(deltaTime: Double); override;
    procedure FadeOut(deltaTime: Double); override;
    procedure SetStatus(const aStatus: TdfGameSceneStatus); override;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Load(); override;
    procedure Unload(); override;

    procedure Update(deltaTime: Double; X, Y: Integer); override;

    procedure SetGameScreens(aGame, aMainMenu: TdfGameScreen);
  end;

implementation

uses
  uGLSceneObjects, uTweener, dfKeyboardExt;

{ TdfInGameMenu }

procedure TdfInGameMenu.AddButtonMaterial(aObject: TdfGUIButton;  aTextureName: String);
var
  w, h: Single;
begin
  with aObject.Material do
  begin
    if aTextureName <> '' then
    begin
      Texture.Image.LoadFromFile(C_INGAMEMENU_PATH + aTextureName);
      Texture.Enabled := True;
      Texture.TextureMode := tmModulate;
      w := Texture.Image.Width * C_BUTTON_SCALE;
      h := Texture.Image.Height * C_BUTTON_SCALE;
      Texture.TextureWrap := twNone;
    end
    else
    begin
      w := 200;
      h := 30;
    end;
    BlendingMode := bmTransparency;
    FrontProperties.Diffuse.SetColor(1, 1, 1, 0);
    MaterialOptions := [moIgnoreFog, moNoLighting];
  end;
  aObject.SetSize(w, h);
end;

procedure TdfInGameMenu.AddPanelMaterial(aObject: TGLHUDSprite;
  aTextureName: String);
var
  w, h: Single;
begin
  with aObject.Material do
  begin
    if aTextureName <> '' then
    begin
      Texture.Image.LoadFromFile(C_INGAMEMENU_PATH + aTextureName);
      Texture.Enabled := True;
      Texture.TextureMode := tmModulate;
      Texture.TextureWrap := twNone;
      w := Texture.Image.Width * C_BUTTON_SCALE;
      h := Texture.Image.Height * C_BUTTON_SCALE;
    end
    else
    begin
      w := 200;
      h := 30;
    end;
    BlendingMode := bmTransparency;
    FrontProperties.Diffuse.SetColor(1, 1, 1, 0.0);
    MaterialOptions := [moIgnoreFog, moNoLighting];
  end;
  aObject.SetSize(w, h);
end;

constructor TdfInGameMenu.Create();
begin
  inherited;
  FName := C_SCREEN_NAME;
  FRoot := TGLDummyCube.CreateAsChild(dfGLSceneObjects.Scene.Objects);
  FRoot.Visible := False;

  FDir := TGLDirectOpenGL.CreateAsChild(FRoot);
  FDir.OnRender := OnRender;

  FPanel := TGLHUDSprite.CreateAsChild(FRoot);
  FExitButton := TdfGUIButton.CreateAsChild(FRoot);
  FReturnButton := TdfGUIButton.CreateAsChild(FRoot);
  FMainMenuButton := TdfGUIButton.CreateAsChild(FRoot);


  AddPanelMaterial(FPanel, 'ingamemenu_panel1.png');
  AddButtonMaterial(FReturnButton, 'btnReturnToGame1.png');
  AddButtonMaterial(FExitButton, 'btnExit1_ingame.png');
  AddButtonMaterial(FMainMenuButton, 'btnReturnToMainMenu1.png');


  with dfGLSceneObjects.Viewer do
  begin
    FPanel.Position.SetPoint(Width / 2 + C_PANEL_X, Height / 2 + C_PANEL_Y, 0);
    FReturnButton.Position.SetPoint(Width / 2 + C_BUTTON_BACKTOGAME_X,
      Height / 2 +  + C_BUTTON_BACKTOGAME_Y, 0);
    FMainMenuButton.Position.SetPoint(Width / 2 + C_BUTTON_BACKTOMENU_X,
      Height / 2 + C_BUTTON_BACKTOMENU_Y, 0);
    FExitButton.Position.SetPoint(Width / 2 + C_BUTTON_EXIT_X,
      Height / 2 + C_BUTTON_EXIT_Y, 0);
  end;

  FReturnButton.OnMouseClick := Self.MouseClickToGame;
  FReturnButton.OnMouseEvent := Self.MouseMove;
  FMainMenuButton.OnMouseClick := Self.MouseClickToMenu;
  FMainMenuButton.OnMouseEvent := Self.MouseMove;
  FExitButton.OnMouseClick := Self.MouseClickExit;
  FExitButton.OnMouseEvent := Self.MouseMove;

  c_green_vec := VectorMake(0,1,0,1);
  c_white_vec := VectorMake(1,1,1,1);

  FPanel.Visible := False;

//  FPanel.MoveFirst;
  Ft := 0;
  FGame := nil;
  FMainMenu := nil;
end;

destructor TdfInGameMenu.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TdfInGameMenu.FadeIn(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FPanel.Material.FrontProperties.Diffuse.Alpha := C_PANEL_ALPHA * (Ft / C_INGAMEMENU_FADEIN_TIME);
  FReturnButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_INGAMEMENU_FADEIN_TIME;
  FMainMenuButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_INGAMEMENU_FADEIN_TIME;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_INGAMEMENU_FADEIN_TIME;
  if Ft >= C_INGAMEMENU_FADEIN_TIME then
    inherited;
end;

procedure TdfInGameMenu.FadeInComplete;
begin
  FPanel.Material.FrontProperties.Diffuse.Alpha := C_PANEL_ALPHA;
  FReturnButton.Material.FrontProperties.Diffuse.Alpha := 1;
  FMainMenuButton.Material.FrontProperties.Diffuse.Alpha := 1;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := 1;
end;

procedure TdfInGameMenu.FadeOut(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FPanel.Material.FrontProperties.Diffuse.Alpha := C_PANEL_ALPHA - (Ft / C_INGAMEMENU_FADEOUT_TIME) * C_PANEL_ALPHA;
  FReturnButton.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_INGAMEMENU_FADEOUT_TIME;
  FMainMenuButton.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_INGAMEMENU_FADEOUT_TIME;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_INGAMEMENU_FADEOUT_TIME;
  if Ft >= C_INGAMEMENU_FADEOUT_TIME then
    inherited;
end;

procedure TdfInGameMenu.FadeOutComplete;
begin
  FRoot.Visible := False;
end;

procedure TdfInGameMenu.Load;
begin
  inherited;
  if FLoaded then
    Exit;

  //*

  FLoaded := True;
end;

procedure TdfInGameMenu.MouseClick(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
//  with Tweener, Sender.Material.FrontProperties.Diffuse do
//  begin
//    AddTweenPSingle(@Green, ts_ExpoEaseIn, 1, 0.5, 1.0);
//    AddTweenPSingle(@Blue, ts_ExpoEaseIn, 1, 0.5, 1.0);
//    AddTweenPSingle(@Green, ts_ExpoEaseIn, 0.5, 1, 2.5);
//    AddTweenPSingle(@Blue, ts_ExpoEaseIn, 0.5, 1, 2.5);
//  end;
end;

procedure TdfInGameMenu.MouseClickExit(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  MouseClick(Sender, MouseButton);
  OnNotify(Self, naQuitGame);
end;

procedure TdfInGameMenu.MouseClickToGame(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  MouseClick(Sender, MouseButton);
  OnNotify(FGame, naSwitchTo);
end;

procedure TdfInGameMenu.MouseClickToMenu(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  MouseClick(Sender, MouseButton);
  FGame.Status := gssFadeOut;
  OnNotify(FMainMenu, naSwitchTo);
end;

procedure TdfInGameMenu.MouseMove(Sender: TdfGUIButton; OldMS,
  NewMS: TdfMouseState);
begin
  with Sender.Material.FrontProperties.Diffuse do
    if (OldMS = msOut) and (NewMS = msIn) then
      Tweener.AddTweenPVector(@Color, ts_ExpoEaseIn, Color, c_green_vec, 3.0, 0)
    else if (OldMS = msIn) and (NewMS = msOut) then
      Tweener.AddTweenPVector(@Color, ts_ExpoEaseIn, Color, c_white_vec, 3.0, 0);
end;

procedure TdfInGameMenu.OnRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  FPanel.DoRender(rci, true, false);
end;

procedure TdfInGameMenu.SetGameScreens(aGame, aMainMenu: TdfGameScreen);
begin
  FGame := aGame;
  FMainMenu := aMainMenu;
end;

procedure TdfInGameMenu.SetStatus(const aStatus: TdfGameSceneStatus);
begin
  inherited;
  case aStatus of
    gssNone           : Exit;
    gssReady          : Exit;
    gssFadeIn         :
    begin
      FRoot.Visible := True;
      FRoot.MoveLast;
      Ft := 0;
    end;
    gssFadeInComplete : FadeInComplete();
    gssFadeOut        : Ft := 0;
    gssFadeOutComplete: FadeOutComplete();
    gssPaused         : Exit;
  end;
end;

procedure TdfInGameMenu.Unload;
begin
  inherited;
  if not FLoaded then
    Exit;

  //*

  FLoaded := False;
end;

procedure TdfInGameMenu.Update(deltaTime: Double; X, Y: Integer);
begin
  inherited;
  case FStatus of
    gssNone: Exit;
    gssFadeIn: FadeIn(deltaTime);
    gssFadeInComplete: Exit;
    gssFadeOut: FadeOut(deltaTime);
    gssFadeOutComplete: Exit;
    gssPaused: Exit;

    gssReady:
    begin
      if IsKeyDown(VK_ESCAPE) then
        FReturnButton.OnMouseClick(FReturnButton, mbLeft);

      if IsMouseClicked(VK_LBUTTON) then
      begin
        FReturnButton.UpdateMouseInfo(X, Y, mbLeft);
        FMainMenuButton.UpdateMouseInfo(X, Y, mbLeft);
        FExitButton.UpdateMouseInfo(X, Y, mbLeft);
      end
      else if IsMouseClicked(VK_RBUTTON) then
      begin
        FReturnButton.UpdateMouseInfo(X, Y, mbRight);
        FMainMenuButton.UpdateMouseInfo(X, Y, mbRight);
        FExitButton.UpdateMouseInfo(X, Y, mbRight);
      end
      else if IsMouseClicked(VK_MBUTTON) then
      begin
        FReturnButton.UpdateMouseInfo(X, Y, mbMiddle);
        FMainMenuButton.UpdateMouseInfo(X, Y, mbMiddle);
        FExitButton.UpdateMouseInfo(X, Y, mbMiddle);
      end
      else
      begin
        FReturnButton.UpdateMouseInfo(X, Y, mbNone);
        FMainMenuButton.UpdateMouseInfo(X, Y, mbNone);
        FExitButton.UpdateMouseInfo(X, Y, mbNone);
      end;
    end;
  end;
end;

end.
