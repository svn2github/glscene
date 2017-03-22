{Показывается ролик, еще что-то, выполняется один раз
 Его цикл - загрузится при нажатии NewGame, что-то показать (связанное с
 новой игрой) и передать управление экрану Game}
unit uGameScreen.NewGame;

interface

uses
  Windows,
  GLScene, GLObjects, GLWin32Viewer, GLHUDObjects, GLMaterial, GLTexture,
  GLCrossPlatform,

  uGameScreen, uHint, uFonts, dfKeyboardExt;

const
  C_SCREEN_NAME = 'New Game';
  C_NEWGAME_TEXTUREPATH = 'data\gameintro\';
  C_NEWGAME_FADEIN_TIME = 2.5;
  C_NEWGAME_FADEOUT_TIME = 1.5;

  //Welcome изначально показывается по центру + смещение
  C_WELCOME_STRAFE_X = 60;
  C_WELCOME_STRAFE_Y = 0;

  C_STAGES_COUNT = 12;

  //Текст показывается по центру только по оси Y, по X - от левого края
  C_NEWGAME_TEXT_STRAFE_X = 40;
  C_NEWGAME_TEXT_STRAFE_Y = 0;

  C_NEWGAME_TEXT1_STRAFE_X = 100;
  C_NEWGAME_TEXT1_STRAFE_Y = 0;

  C_STAGES_TIME: array[0..C_STAGES_COUNT - 1] of Single =
    (2.5, 1.0, 2.0, //Появление, задержка и исчезновение Welcome
     1.0, 1.5, 1.0, //text1
     1.5, 5.0, 1.5, //text2
     1.5, 4.5, 1.5  //text3
     );

  C_NEWGAME_TEXT_1 = 'Ой, извините, не тот текст!';
  C_NEWGAME_TEXT_2 = 'В 2035 году началось активное освоение' +#13#10+
                     'человечеством Ближнего Внеземелья ' + #151 + #13#10 +
                     'Марса, Венеры и Юпитера';
  C_NEWGAME_TEXT_3 = 'Объект: Алексей Тобольский' + #13#10 +
                     'Дата: 2150 год' + #13#10 +
                     'Место: Ганимед, Юпитер, Солнечная Система ' + #13#10 + #13#10 +
                     'Экзамен по пилотированию';

type
  TdfNewGame = class (TdfGameScreen)
  private
    FRoot: TGLDummyCube;
    FViewerW, FViewerH: Integer;

    FBackground: TGLHUDSprite;

    FWelcome, FText1, FText2, FText3: TGLHUDText;

    Ft: Double;
    FStage: Byte;

    FGame: TdfGameScreen;

    FHint: TdfHint;
    procedure AddBackgroundMaterial(var aBack: TGLHUDSprite;
      const aTexture: String);
    procedure AddFont(var aObject: TGLHUDText; const aFontPath, aText: String);
    procedure FadeInComplete();
    procedure FadeOutComplete();
    procedure DoIntroUpdate(deltaTime: Double);
    procedure SetStage(const Value: Byte);
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

    procedure SetGameScreens(aGame: TdfGameScreen);

    property Stage: Byte read FStage write SetStage;
  end;

implementation

uses
  uGLSceneObjects;

{ TdfNewGame }

procedure TdfNewGame.AddBackgroundMaterial(var aBack: TGLHUDSprite;
  const aTexture: String);
begin
  with aBack.Material do
  begin
    Texture.Image.LoadFromFile(C_NEWGAME_TEXTUREPATH + aTexture);
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    Texture.TextureWrap := twNone;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(0,0,0);
  end;
end;


procedure TdfNewGame.AddFont(var aObject: TGLHUDText; const aFontPath,
  aText: String);
begin
  aObject.BitmapFont := GetFont(aFontPath);
  aObject.Text := aText;
  GetFont(aFontPath).EnsureString(aText);
  aObject.ModulateColor.SetColor(1, 221 / 255, 170 / 255, 0);
end;

constructor TdfNewGame.Create();
begin
  inherited;
  FName := C_SCREEN_NAME;
  FRoot := TGLDummyCube.CreateAsChild(dfGLSceneObjects.Scene.Objects);
  FRoot.Visible := False;

  FViewerW := dfGLSceneObjects.Viewer.Width;
  FViewerH := dfGLSceneObjects.Viewer.Height;

  FBackground := TGLHUDSprite.CreateAsChild(FRoot);
  AddBackgroundMaterial(FBackground, 'newgame_background.png');
  FBackground.Position.SetPoint(FViewerW / 2, FViewerH / 2, 0.9);
  FBackground.SetSize(FViewerW, FViewerH);

  FWelcome := TGLHUDText.CreateAsChild(FRoot);
  AddFont(FWelcome, C_FONT_1, 'A long time ago in a Galaxy' + #13#10 +
                              'far, far away...');
  FWelcome.Layout := tlCenter;
  FWelcome.Position.SetPoint(C_WELCOME_STRAFE_X,
                             FViewerH / 2 + C_WELCOME_STRAFE_Y, 0.3);

  FText1 := TGLHUDText.CreateAsChild(FRoot);
  AddFont(FText1, C_FONT_1, C_NEWGAME_TEXT_1);
  FText1.Layout := tlCenter;
  FText1.Position.SetPoint(C_NEWGAME_TEXT1_STRAFE_X,
                           FViewerH / 2 + C_NEWGAME_TEXT1_STRAFE_Y, 0.4);

  FText2 := TGLHUDText.CreateAsChild(FRoot);
  AddFont(FText2, C_FONT_1, C_NEWGAME_TEXT_2);
  FText2.Layout := tlCenter;
  FText2.Position.SetPoint(C_NEWGAME_TEXT_STRAFE_X,
                           FViewerH / 2 + C_NEWGAME_TEXT_STRAFE_Y, 0.4);
  FText2.Scale.SetVector(0.7, 0.7, 0.7);

  FText3 := TGLHUDText.CreateAsChild(FRoot);
  AddFont(FText3, C_FONT_1, C_NEWGAME_TEXT_3);
  FText3.Layout := tlCenter;
  FText3.Position.SetPoint(C_NEWGAME_TEXT_STRAFE_X,
                           FViewerH / 2 + C_NEWGAME_TEXT_STRAFE_Y, 0.4);
  FText3.Scale.SetVector(0.7, 0.7, 0.7);

  FHint := TdfHint.CreateAsChild(FRoot);
  FHint.Init('Пропустить', GetFont(C_FONT_1), 'mouse_lbutton.png', haTop,
             0, 0, 0.5, 0.5);
  FHint.HintText.ModulateColor.SetColor(0.9, 0.4, 0.4);
  FHint.SetPosition(FViewerW - FHint.Width / 2 - 60,
                    FViewerH - FHint.Height / 2 - 40);

  FStage := 0;
  Ft := 0;
  FGame := nil;
end;

destructor TdfNewGame.Destroy;
begin
  FRoot.Free;
  inherited;
end;

{Основной "скрипт" ролика}
procedure TdfNewGame.DoIntroUpdate(deltaTime: Double);
begin
  if FStage >= C_STAGES_COUNT then
    if Assigned(FGame) then
      OnNotify(FGame, naSwitchTo);

  Ft := Ft + deltaTime;
  case FStage of
    {Первый этап: показ FWelcome}
    0: FWelcome.ModulateColor.Alpha := Ft / C_STAGES_TIME[FStage];
    {Второй этап - держим FWelcome}
    1: ;
    {Третий этап - убираем FWelcome}
    2: FWelcome.ModulateColor.Alpha := 1 - Ft / C_STAGES_TIME[FStage];
    {}
    3: FText1.ModulateColor.Alpha := Ft / C_STAGES_TIME[FStage];
    4: ;
    5: FText1.ModulateColor.Alpha := 1 - Ft / C_STAGES_TIME[FStage];

    6: FText2.ModulateColor.Alpha := Ft / C_STAGES_TIME[FStage];
    7: ;
    8: FText2.ModulateColor.Alpha := 1 - Ft / C_STAGES_TIME[FStage];

    9 : FText3.ModulateColor.Alpha := Ft / C_STAGES_TIME[FStage];
    10: ;
    11: FText3.ModulateColor.Alpha := 1 - Ft / C_STAGES_TIME[FStage];
  end;

  if Ft >= C_STAGES_TIME[FStage] then
    Stage := Stage + 1;

  if IsMouseClicked(VK_LBUTTON) then
  begin
    Stage := Stage + 1;
  end;
end;

procedure TdfNewGame.FadeIn(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBackground.Material.FrontProperties.Diffuse.SetColor
    (Ft / C_NEWGAME_FADEIN_TIME, Ft / C_NEWGAME_FADEIN_TIME, Ft / C_NEWGAME_FADEIN_TIME);
  if Ft >= C_NEWGAME_FADEIN_TIME then
    inherited;

end;

procedure TdfNewGame.FadeInComplete;
begin
  FBackground.Material.FrontProperties.Diffuse.SetColor(1,1,1);
end;

procedure TdfNewGame.FadeOut(deltaTime: Double);
begin
  Ft := Ft + deltaTime;
  FBackground.Material.FrontProperties.Diffuse.SetColor
    (1 - Ft / C_NEWGAME_FADEOUT_TIME, 1 - Ft / C_NEWGAME_FADEOUT_TIME, 1 - Ft / C_NEWGAME_FADEOUT_TIME);
  if Ft >= C_NEWGAME_FADEOUT_TIME then
    inherited;
end;

procedure TdfNewGame.FadeOutComplete;
begin
  FRoot.Visible := False;
end;

procedure TdfNewGame.Load;
begin
  inherited;
  if FLoaded then
    Exit;

  FStage := 0;
  Ft := 0;

  //*
  FLoaded := True;
end;

procedure TdfNewGame.SetGameScreens(aGame: TdfGameScreen);
begin
  FGame := aGame;
end;

procedure TdfNewGame.SetStage(const Value: Byte);
begin
  case Value of
    1, 2: FWelcome.ModulateColor.Alpha := 1;
    3: FWelcome.ModulateColor.Alpha := 0;

    4, 5: FText1.ModulateColor.Alpha := 1;
    6: FText1.ModulateColor.Alpha := 0;

    7,8: FText2.ModulateColor.Alpha := 1;
    9: FText2.ModulateColor.Alpha := 0;

    10, 11: FText3.ModulateColor.Alpha := 1;
    12: FText3.ModulateColor.Alpha := 0;
  end;
  Ft := 0;
  FStage := Value;
end;

procedure TdfNewGame.SetStatus(const aStatus: TdfGameSceneStatus);
begin
  inherited;
  case aStatus of
    gssNone           : Exit;
    gssReady          : Ft := 0;
    gssFadeIn         :
    begin
      FRoot.Visible := True;
      Ft := 0;
    end;
    gssFadeInComplete : FadeInComplete();
    gssFadeOut        : Ft := 0;
    gssFadeOutComplete: FadeOutComplete();
    gssPaused         : Exit;
  end;
end;

procedure TdfNewGame.Unload;
begin
  inherited;
  if not FLoaded then
    Exit;
  //*
  FLoaded := False;
end;

procedure TdfNewGame.Update(deltaTime: Double; X, Y: Integer);
begin
  inherited;
  case FStatus of
    gssNone: Exit;
    gssReady: DoIntroUpdate(deltaTime);
    gssFadeIn: FadeIn(deltaTime);
    gssFadeInComplete: Exit;
    gssFadeOut: FadeOut(deltaTime);
    gssFadeOutComplete: Exit;
    gssPaused: Exit;
  end;
end;

end.
