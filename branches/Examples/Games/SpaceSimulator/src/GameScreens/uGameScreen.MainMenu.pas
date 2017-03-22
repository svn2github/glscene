unit uGameScreen.MainMenu;

{Главное меню}

interface

uses
  Winapi.Windows,

  GLScene,
  GLObjects,
  GLMaterial,
  GLTexture,
  GLFilePNG,
  GLHUDObjects,
  GLWin32Viewer,
  GLKeyboard,
  GLVectorGeometry,
  GLVectorTypes,
  DDSImage,

  uGameScreen,
  uGUIButton;

const
  C_SCREEN_NAME = 'Main Menu';
  C_BUTTONS_ORIGINPOSITION_X = 150;
  C_BUTTONS_ORIGINPOSITION_Y = 350;
  C_BUTTONS_STRAFE_X = 0;
  C_BUTTONS_STRAFE_Y = 35;
  C_BUTTONS_IMAGESCALE = 1/1;
  C_MENU_PATH = 'data\menu\';
  C_MENU_FADEIN_TIME = 2.5;
  C_MENU_FADEOUT_TIME = 1.5;

type

  TdfMainMenu = class(TdfGameScreen)
  private
    FRoot: TGLDummyCube;
    Ft: Double;
    FNewGameButton, FCreditsButton, FExitButton: TdfGUIButton;
    FBackground: TGLHUDSprite;
    FBlankBack: TGLHUDSprite;
    FGLSceneLogo: TGLHUDSprite;
    FViewerW, FViewerH: Integer;
    FNewGame, FCredits: TdfGameScreen;
    c_green_vec, c_white_vec, c_red_vec: TVector;
    procedure AddButtonMaterial(var aObject: TdfGUIButton; const aTextureName: String);
    procedure AddSpriteMaterial(var aObject: TGLHUDSprite; const aTextureName: String);
    procedure FadeInComplete();
    procedure FadeOutComplete();
  protected
    //Общие коллбэки
    procedure MouseMove(Sender: TdfGUIButton; OldMS, NewMS: TdfMouseState);
    procedure MouseClick(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    //Колббэки для отдельных кнопок
    procedure MouseClickNewGame(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    procedure MouseClickCredits(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    procedure MouseClickExit(Sender: TdfGUIButton; MouseButton: TdfMouseButton);
    procedure FadeIn(deltaTime: Double); override;
    procedure FadeOut(deltaTime: Double); override;
    procedure SetStatus(const aStatus: TdfGameSceneStatus); override;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Load(); override;
    procedure Unload(); override;
    procedure Update(deltaTime: Double; X, Y: Integer); override;
    procedure SetGameScreens(aNewGame, aCredits: TdfGameScreen);
  end;

//  TSuperHero = class
//  public
//    procedure SaveTheWorld(ShouldSaveMankind: Boolean); virtual; abstract;
//    procedure KillBadGuys(ShouldKickTheirAsses: Boolean); virtual; abstract;
//    procedure MakePatrioticSpeech(ItsMyJob,
//      IWillKickTheirAsses, WeWillFight: Boolean); virtual; abstract;
//  end;

//================================================================
implementation
//================================================================

{ TdfMainMenu }

uses
  dfKeyboardExt, uDebugInfo,
  uTweener, uGLSceneObjects;

//var
//  c_button_w, c_button_h: Single;

procedure TdfMainMenu.AddSpriteMaterial(var aObject: TGLHUDSprite;
  const aTextureName: String);
begin
  with aObject.Material do
  begin
    Texture.Image.LoadFromFile(C_MENU_PATH + aTextureName);
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    Texture.TextureWrap := twNone;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(1,1,1, 0);
  end;
end;

procedure TdfMainMenu.AddButtonMaterial(var aObject: TdfGUIButton;
  const aTextureName: String);
begin
  with aObject.Material do
  begin
    Texture.Image.LoadFromFile(C_MENU_PATH + aTextureName);
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    Texture.TextureWrap := twNone;
    BlendingMode := bmTransparency;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(1,1,1, 0);
  end;
  aObject.Width := aObject.Material.Texture.Image.Width * C_BUTTONS_IMAGESCALE;
  aObject.Height := aObject.Material.Texture.Image.Height * C_BUTTONS_IMAGESCALE;
end;

constructor TdfMainMenu.Create();
begin
  inherited;
  FRoot := TGLDummyCube.CreateAsChild(dfGLSceneObjects.Scene.Objects);
  FRoot.Visible := False;
  Name := C_SCREEN_NAME;

  FViewerW := dfGLSceneObjects.Viewer.Width;
  FViewerH := dfGLSceneObjects.Viewer.Height;

  FBlankBack := TGLHUDSprite.CreateAsChild(FRoot);
  with FBlankBack do
  begin
    Material.Texture.Enabled := False;
    Material.MaterialOptions := [moIgnoreFog, moNoLighting];
    Material.BlendingMode := bmTransparency;
    Material.FrontProperties.Diffuse.SetColor(0,0,0);
    Position.SetPoint(FViewerW / 2, FViewerH / 2, 0);
    Width := FViewerW;
    Height := FViewerH;
  end;

  FBackground := TGLHUDSprite.CreateAsChild(FRoot);
  with FBackground do
  begin
    Position.SetPoint(FViewerW / 2, FViewerH / 2, -1);
    Width := FViewerW;
    Height := FViewerH;
    AddSpriteMaterial(FBackground, 'background1.png');
  end;

  FGLSceneLogo := TGLHUDSprite.CreateAsChild(FRoot);
  AddSpriteMaterial(FGLSceneLogo, 'logo_ba_u.dds');
  FGLSceneLogo.Material.BlendingMode := bmTransparency;
  FGLSceneLogo.Width := FGLSceneLogo.Material.Texture.Image.Width * 1.2;
  FGLSceneLogo.Height := FGLSceneLogo.Material.Texture.Image.Height * 1.2;
  FGLSceneLogo.Position.SetPoint({FViewerW - }FGLSceneLogo.Width / 2, FViewerH - FGLSceneLogo.Height / 2, 0);

  FNewGameButton := TdfGUIButton.CreateAsChild(FRoot);
  with FNewGameButton do
  begin
    Position.SetPoint(C_BUTTONS_ORIGINPOSITION_X, C_BUTTONS_ORIGINPOSITION_Y, 0);
    OnMouseClick := MouseClickNewGame;
    OnMouseEvent := MouseMove;
    AddButtonMaterial(FNewGameButton, 'btnNewGame1.png');
  end;

  FCreditsButton := TdfGUIButton.CreateAsChild(FRoot);
  with FCreditsButton do
  begin
    Position.SetPoint(FNewGameButton.Position.X + C_BUTTONS_STRAFE_X,
                      FNewGameButton.Position.Y + FNewGameButton.Height / 2
                         + C_BUTTONS_STRAFE_Y, 0);
    OnMouseClick := MouseClickCredits;
    OnMouseEvent := MouseMove;
    AddButtonMaterial(FCreditsButton, 'btnCredits1.png');
  end;

  FExitButton := TdfGUIButton.CreateAsChild(FRoot);
  with FExitButton do
  begin
    Position.SetPoint(FCreditsButton.Position.X + C_BUTTONS_STRAFE_X,
                      FCreditsButton.Position.Y + FCreditsButton.Height / 2
                         + C_BUTTONS_STRAFE_Y, 0);
    OnMouseEvent := MouseMove;
    OnMouseClick := MouseClickExit;
    AddButtonMaterial(FExitButton, 'btnExit1.png');
  end;

//  c_button_w := FNewGameButton.Width + 1;
//  c_button_h := FNewGameButton.Height + 1;

  c_green_vec := VectorMake(0, 1, 0, 1);
  c_white_vec := VectorMake(1, 1, 1, 1);
  c_red_vec   := VectorMake(1, 0, 0, 1);


//  FBackground.MoveLast;
  FBlankBack.MoveFirst;
end;

destructor TdfMainMenu.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TdfMainMenu.FadeIn(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBlankBack.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_MENU_FADEIN_TIME;
  FNewGameButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_MENU_FADEIN_TIME;
  FCreditsButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_MENU_FADEIN_TIME;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := Ft / C_MENU_FADEIN_TIME;
  FGLSceneLogo.Material.FrontProperties.Diffuse.Alpha := Ft / C_MENU_FADEIN_TIME;
  if Ft >= C_MENU_FADEIN_TIME then
    inherited; //FadeInComplete
end;

procedure TdfMainMenu.FadeInComplete;
begin
  FBlankBack.Material.FrontProperties.Diffuse.Alpha := 0.0;
  FBlankBack.Visible := False;
  FNewGameButton.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FCreditsButton.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FGLSceneLogo.Material.FrontProperties.Diffuse.Alpha := 1.0;
end;

procedure TdfMainMenu.FadeOut(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBlankBack.Material.FrontProperties.Diffuse.Alpha := Ft / C_MENU_FADEOUT_TIME;
  FNewGameButton.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_MENU_FADEOUT_TIME;
  FCreditsButton.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_MENU_FADEOUT_TIME;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := 1 -Ft / C_MENU_FADEOUT_TIME;
  FGLSceneLogo.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_MENU_FADEOUT_TIME;
  if Ft >= C_MENU_FADEOUT_TIME then
    inherited; //FadeOutComplete
end;


procedure TdfMainMenu.FadeOutComplete;
begin
  FBlankBack.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FNewGameButton.Material.FrontProperties.Diffuse.Alpha := 0.0;
  FCreditsButton.Material.FrontProperties.Diffuse.Alpha := 0.0;
  FExitButton.Material.FrontProperties.Diffuse.Alpha := 0.0;
  FGLSceneLogo.Material.FrontProperties.Diffuse.Alpha := 0.0;

  FRoot.Visible := False;
  FStatus := gssNone;
end;

procedure TdfMainMenu.Load();
begin
  inherited;
  if FLoaded then
    Exit;

  FLoaded := True;
end;

procedure TdfMainMenu.MouseClickNewGame(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  //Goto newgame gamescene
  MouseClick(Sender, MouseButton);
  if Assigned(FNewGame) then
    OnNotify(FNewGame, naSwitchTo);
end;

procedure TdfMainMenu.MouseClickCredits(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  //Goto credits gamescene
  MouseClick(Sender, MouseButton);
  if Assigned(FCredits) then
    OnNotify(FCredits, naShowModal);
end;

procedure TdfMainMenu.MouseClickExit(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  //Goto exit gamescene or simple exit the game
  MouseClick(Sender, MouseButton);
  OnNotify(Self, naQuitGame);
end;

procedure TdfMainMenu.MouseMove(Sender: TdfGUIButton; OldMS,
  NewMS: TdfMouseState);
begin
  with Sender.Material.FrontProperties.Diffuse do
    if (OldMS = msOut) and (NewMS = msIn) then
      Tweener.AddTweenPVector(@Color, ts_ExpoEaseIn, Color, c_green_vec, 3.0, 0)
    else if (OldMS = msIn) and (NewMS = msOut) then
      Tweener.AddTweenPVector(@Color, ts_ExpoEaseIn, Color, c_white_vec, 3.0, 0);

//  if (OldMS = msOut) and (NewMS = msIn) then
//  begin
//    Tweener.AddTweenPSingle(@Sender.Width, ts_ElasticEaseIn, Sender.Width,
//                            c_button_w * 1.2, 1.2);
//    Tweener.AddTweenPSingle(@Sender.Height, ts_ElasticEaseIn, Sender.Height,
//                            c_button_h * 1.2, 1.2);
//  end
//  else if (OldMS = msIn) and (NewMS = msOut) then
//  begin
//    Tweener.AddTweenPSingle(@Sender.Width, ts_ElasticEaseIn, Sender.Width,
//                            c_button_w * 1.0, 1.2);
//    Tweener.AddTweenPSingle(@Sender.Height, ts_ElasticEaseIn, Sender.Height,
//                            c_button_h * 1.0, 1.2);
//  end;
end;

procedure TdfMainMenu.MouseClick(Sender: TdfGUIButton;
  MouseButton: TdfMouseButton);
begin
  with Sender.Material.FrontProperties.Diffuse do
    Tweener.AddTweenPVector(@Color, ts_ExpoEaseIn, c_green_vec, c_red_vec, 1.0, 0)

//  with Tweener, Sender.Material.FrontProperties.Diffuse do
//  begin
//    AddTweenPSingle(@Green, ts_ExpoEaseIn, 1, 0.5, 1.0);
//    AddTweenPSingle(@Blue, ts_ExpoEaseIn, 1, 0.5, 1.0);
//    AddTweenPSingle(@Green, ts_ExpoEaseIn, 0.5, 1, 2.5);
//    AddTweenPSingle(@Blue, ts_ExpoEaseIn, 0.5, 1, 2.5);
//  end;
end;

procedure TdfMainMenu.SetGameScreens(aNewGame, aCredits: TdfGameScreen);
begin
  FNewGame := aNewGame;
  FCredits := aCredits;
end;

procedure TdfMainMenu.SetStatus(const aStatus: TdfGameSceneStatus);
begin
  inherited;
  case aStatus of
    gssNone: Exit;

    gssReady: Exit;

    gssFadeIn:
    begin
      dfDebugInfo.Visible := False;
      FBlankBack.Visible := True;
      FRoot.Visible := True;
      Ft := 0;
    end;

    gssFadeInComplete: FadeInComplete();

    gssFadeOut:
    begin
      FBlankBack.Visible := True;
      Ft := 0;
    end;

    gssFadeOutComplete: FadeOutComplete();
  end;
end;

procedure TdfMainMenu.Unload;
begin
  inherited;
  if not FLoaded then
    Exit;

  FLoaded := False;
end;

procedure TdfMainMenu.Update(deltaTime: Double; X, Y: Integer);
begin
  inherited;
  case FStatus of
    gssNone           : Exit;
    gssFadeIn         : FadeIn(deltaTime);
    gssFadeInComplete : Exit;
    gssFadeOut        : FadeOut(deltaTime);
    gssFadeOutComplete: Exit;

    gssReady:
    begin
      if IsMouseClicked(VK_LBUTTON) then
      begin
        FNewGameButton.UpdateMouseInfo(X, Y, mbLeft);
        FCreditsButton.UpdateMouseInfo(X, Y, mbLeft);
        FExitButton.UpdateMouseInfo(X, Y, mbLeft)
      end
      else if IsMouseClicked(VK_RBUTTON) then
      begin
        FNewGameButton.UpdateMouseInfo(X, Y, mbRight);
        FCreditsButton.UpdateMouseInfo(X, Y, mbRight);
        FExitButton.UpdateMouseInfo(X, Y, mbRight)
      end
      else if IsMouseClicked(VK_MBUTTON) then
      begin
        FNewGameButton.UpdateMouseInfo(X, Y, mbMiddle);
        FCreditsButton.UpdateMouseInfo(X, Y, mbMiddle);
        FExitButton.UpdateMouseInfo(X, Y, mbMiddle)
      end
      else
      begin
        FNewGameButton.UpdateMouseInfo(X, Y, mbNone);
        FCreditsButton.UpdateMouseInfo(X, Y, mbNone);
        FExitButton.UpdateMouseInfo(X, Y, mbNone);
      end;
    end;
  end;
end;

end.
