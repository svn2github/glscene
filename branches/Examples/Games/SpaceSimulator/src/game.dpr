program game;

uses
  Forms,
  SysUtils,
  uMain in 'uMain.pas' {frmMain},
  uGame in 'uGame.pas',
  uLog in 'uLog.pas',
  uFonts in 'uFonts.pas',
  dfHEngine in 'common\dfHEngine.pas',
  dfList in 'common\dfList.pas',
  dfLogger in 'common\dfLogger.pas',
  dfKeyboardExt in 'common\dfKeyboardExt.pas',
  uTweener in 'common\uTweener.pas',
  uGUIButton in 'GUI\uGUIButton.pas',
  uGameScreen in 'GameScreens\uGameScreen.pas',
  uGameScreen.MainMenu in 'GameScreens\uGameScreen.MainMenu.pas',
  uGameScreen.Credits in 'GameScreens\uGameScreen.Credits.pas',
  uGameScreen.NewGame in 'GameScreens\uGameScreen.NewGame.pas',
  uGameScreen.InGameMenu in 'GameScreens\uGameScreen.InGameMenu.pas',
  uGameScreen.MainGame in 'GameScreens\uGameScreen.MainGame.pas',
  uHint in 'GUI\uHint.pas',
  uSpacefighter in 'GameObjects\uSpacefighter.pas',
  uFighterControl.User in 'GameControls\uFighterControl.User.pas',
  uEngine in 'GameObjects\uEngine.pas',
  uWeapon in 'GameObjects\uWeapon.pas',
  uDebugInfo in 'GUI\uDebugInfo.pas',
  uAsteroidField in 'GameObjects\uAsteroidField.pas',
  uSimplePhysics in 'uSimplePhysics.pas',
  uBulletAccum in 'GameObjects\uBulletAccum.pas',
  uGLSceneObjects in 'uGLSceneObjects.pas',
  uTarget in 'GUI\uTarget.pas',
  uFighterControl.AI in 'GameControls\uFighterControl.AI.pas',
  uTutorial in 'uTutorial.pas',
  uGameObject in 'GameObjects\uGameObject.pas',
  uFighterControl in 'GameControls\uFighterControl.pas',
  uSpeedIndicator in 'GUI\uSpeedIndicator.pas',
  uBoomAccum in 'GameObjects\uBoomAccum.pas',
  uGameObjects in 'uGameObjects.pas',
  uFakeSpacefighter in 'GameObjects\uFakeSpacefighter.pas',
  uGameScreen.AlphaVersion in 'GameScreens\uGameScreen.AlphaVersion.pas';

{$R *.res}

begin
  FormatSettings.DecimalSeparator := '.';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
