unit uGameScreen.Credits;

interface

uses
  Windows,

  GLScene, GLObjects, GLWin32Viewer, GLHUDObjects, GLMaterial, GLTexture,
  GLKeyboard, GLFilePNG, GLState,

  uGameScreen,
  dfKeyboardExt;

const
  C_SCREEN_NAME = 'Credits';

  C_CREDITS_TEXTUREPATH = 'data\menu\';
  C_CREDITS_IMAGESCALE = 1/1;

  C_BACK_SCALE_X = 1.0;
  C_BACK_SCALE_Y = 1.0;
  C_BACK_STRAFE_X = 0;
  C_BACK_STRAFE_Y = 0;
  C_BACK_ALPHA = 0.5;

  C_CREDITS_ORIGIN_STRAFE_X = 150;
  C_CREDITS_ORIGIN_STRAFE_Y = -50;

  C_PROGRAMMING_STRAFE_X = 0;
  C_PROGRAMMING_STRAFE_Y = 0;

  C_ART_STRAFE_X = 0;
  C_ART_STRAFE_Y = 150;

  C_GLSCENERU_STRAFE_X = 0;
  C_GLSCENERU_STRAFE_Y = 250;

  C_CREDITS_FADEIN_TIME = 1.5;
  C_CREDITS_FADEOUT_TIME = 0.8;
type
  TdfCredits = class (TdfGameScreen)
  private
    FRoot: TGLDummyCube;
    FViewerW, FViewerH: Integer;

    FBackground: TGLHUDSprite;

    FProgramming, FArtModels, FForGLSceneRu: TGLHUDSprite;

    FMainMenu: TdfGameScreen;
    Ft: Double;
    procedure AddCreditsMaterial(var aObject: TGLHUDSprite; const aTextureName: String);
    procedure AddBackgroundMaterial(var aBack: TGLHUDSprite);
    procedure FadeInComplete();
    procedure FadeOutComplete();
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

    procedure SetGameScreens(aMainMenu: TdfGameScreen);
  end;

implementation

uses
  uGLSceneObjects;

{ TdfCredits }

procedure TdfCredits.AddBackgroundMaterial(var aBack: TGLHUDSprite);
begin
  with aBack.Material do
  begin
    Texture.Enabled := False;
    Texture.TextureMode := tmModulate;
    BlendingMode := bmTransparency;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(0,0,0, C_BACK_ALPHA);
  end;
end;

procedure TdfCredits.AddCreditsMaterial(var aObject: TGLHUDSprite;
  const aTextureName: String);
begin
  with aObject.Material do
  begin
    Texture.Image.LoadFromFile(C_CREDITS_TEXTUREPATH + aTextureName);
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    BlendingMode := bmTransparency;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(1,1,1);
    BlendingParams.BlendFuncSFactor := bfOne;
    BlendingParams.BlendFuncDFactor := bfOne;
  end;
  aObject.Width := aObject.Material.Texture.Image.Width * C_CREDITS_IMAGESCALE;
  aObject.Height := aObject.Material.Texture.Image.Height * C_CREDITS_IMAGESCALE;
end;

constructor TdfCredits.Create();
begin
  inherited;
  FName := C_SCREEN_NAME;
  FRoot := TGLDummyCube.CreateAsChild(dfGLSceneObjects.Scene.Objects);
  FRoot.Visible := False;

  FViewerW := dfGLSceneObjects.Viewer.Width;
  FViewerH := dfGLSceneObjects.Viewer.Height;

  FBackground := TGLHUDSprite.CreateAsChild(FRoot);
  AddBackgroundMaterial(FBackground);
  with FBackground do
  begin
    Position.SetPoint(FViewerW / 2 + C_BACK_STRAFE_X, FViewerH / 2 + C_BACK_STRAFE_Y, -0.9);
    Width := FViewerW * C_BACK_SCALE_X;
    Height := FViewerH * C_BACK_SCALE_Y;
  end;

  FProgramming := TGLHUDSprite.CreateAsChild(FRoot);
  AddCreditsMaterial(FProgramming, 'credit_program.png');
  FProgramming.Position.SetPoint(FViewerW / 2 + C_CREDITS_ORIGIN_STRAFE_X + C_PROGRAMMING_STRAFE_X,
                                 FViewerH / 2 + C_CREDITS_ORIGIN_STRAFE_Y + C_PROGRAMMING_STRAFE_Y, -0.5);

  FArtModels := TGLHUDSprite.CreateAsChild(FRoot);
  AddCreditsMaterial(FArtModels, 'credit_artmodel.png');
  FArtModels.Position.SetPoint(FViewerW / 2 + C_CREDITS_ORIGIN_STRAFE_X + C_ART_STRAFE_X,
                               FViewerH / 2 + C_CREDITS_ORIGIN_STRAFE_Y + C_ART_STRAFE_Y, -0.5);

  FForGLSceneRu := TGLHUDSprite.CreateAsChild(FRoot);
  AddCreditsMaterial(FForGLSceneRu, 'credit_glsceneru.png');
  FForGLSceneRu.Position.SetPoint(FViewerW / 2 + C_CREDITS_ORIGIN_STRAFE_X + C_GLSCENERU_STRAFE_X,
                               FViewerH / 2 + C_CREDITS_ORIGIN_STRAFE_Y + C_GLSCENERU_STRAFE_Y, -0.5);
//  FBackground.MoveFirst;
end;

destructor TdfCredits.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TdfCredits.FadeIn(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBackground.Material.FrontProperties.Diffuse.Alpha := (Ft / C_CREDITS_FADEIN_TIME) * C_BACK_ALPHA ;
  FProgramming.Material.FrontProperties.Diffuse.Alpha := Ft / C_CREDITS_FADEIN_TIME;
  FArtModels.Material.FrontProperties.Diffuse.Alpha := Ft / C_CREDITS_FADEIN_TIME;
  FForGLSceneRu.Material.FrontProperties.Diffuse.Alpha := Ft / C_CREDITS_FADEIN_TIME;
  if FT >= C_CREDITS_FADEIN_TIME then
    inherited;
end;

procedure TdfCredits.FadeInComplete;
begin
  FBackground.Material.FrontProperties.Diffuse.Alpha := C_BACK_ALPHA;
  FProgramming.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FArtModels.Material.FrontProperties.Diffuse.Alpha := 1.0;
  FForGLSceneRu.Material.FrontProperties.Diffuse.Alpha := 1.0;
end;

procedure TdfCredits.FadeOut(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBackground.Material.FrontProperties.Diffuse.Alpha := C_BACK_ALPHA - (Ft / C_CREDITS_FADEOUT_TIME) * C_BACK_ALPHA ;
  FProgramming.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_CREDITS_FADEOUT_TIME;
  FArtModels.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_CREDITS_FADEOUT_TIME;
  FForGLSceneRu.Material.FrontProperties.Diffuse.Alpha := 1 - Ft / C_CREDITS_FADEOUT_TIME;
  if FT >= C_CREDITS_FADEOUT_TIME then
    inherited;
end;

procedure TdfCredits.FadeOutComplete;
begin
  FBackground.Material.FrontProperties.Diffuse.Alpha := 0;
  FProgramming.Material.FrontProperties.Diffuse.Alpha := 0;
  FArtModels.Material.FrontProperties.Diffuse.Alpha := 0;
  FForGLSceneRu.Material.FrontProperties.Diffuse.Alpha := 0;

  FRoot.Visible := False;
  FStatus := gssNone;
end;

procedure TdfCredits.Load;
begin
  inherited;
  if FLoaded then
    Exit;
  //*
  FLoaded := True;
end;

procedure TdfCredits.SetGameScreens(aMainMenu: TdfGameScreen);
begin
  FMainMenu := aMainMenu;
end;

procedure TdfCredits.SetStatus(const aStatus: TdfGameSceneStatus);
begin
  inherited;
  case aStatus of
    gssNone           : Exit;
    gssReady          : Exit;
    gssFadeIn         :
    begin
      FRoot.Visible := True;
      Ft := 0;
    end;
    gssFadeInComplete : FadeInComplete();
    gssFadeOut        : Ft := 0;
    gssFadeOutComplete: FadeOutComplete();
  end;
end;

procedure TdfCredits.Unload;
begin
  inherited;
  if not FLoaded then
    Exit;
  //*
  FLoaded := False;
end;

procedure TdfCredits.Update(deltaTime: Double; X, Y: Integer);
begin
  inherited;
  case FStatus of
    gssNone: Exit;

    gssReady:
    begin
      //Надо что-то придумать с VK_LBUTTON. Нужен перехват именно кликов
      //UPD: сделано
      if IsMouseClicked(VK_LBUTTON) then
        if Assigned(FMainMenu) then
          OnNotify(FMainMenu, naSwitchTo);
    end;

    gssFadeIn: FadeIn(deltaTime);

    gssFadeInComplete: Exit;

    gssFadeOut: FadeOut(deltaTime);

    gssFadeOutComplete: Exit;

    gssPaused: Exit;
  end;

end;

end.
