{

  + BUG: Аккумулятор не удаляется при переходе в меню - патроны видны поверх

  + BUG: Не реализовано различное поведение при столкновении в зависимости от
         UserType в TdfPhys. Реализация в TdfMainGame.OnCollision

  + BUG: Не исчезает "Осталось целей" при новой игре

  + BUG: При новой игре следует отключать искусственный интеллект у противников
         У первых он остается после последнего этапа

  + BUG: Newgame пролистывается на второй раз

  + BUG: Не исчезает "осталось целей" при окончании этапа

  + BUG: Оставить счетчик по окончании 10-го этапа

  + BUG: Планета не вращается вместе с камерой

  + BUG: При загрузке новой игры не сбрасываются параметры.
      +  Перенести соответствующий функционал в Load()
      +  ПРОВЕРИТЬ

  ? TODO: Перерисовать хинт для мыши

  + TODO: Допилить InGameMenu, оно же PauseMenu. Базовый функционал:
          "вернуться в игру" и "в главное меню"

  + TODO: "Стрелочки" в HUD, указывающие направление движения корабля
  +       Перенести из прототипа

  + TODO: Spacefighter - загрузка текстуры через MatLib с проверкой на
          существование текстуры

  + TODO: Создание "врага" с полоской жизни, реализация повреждений
  +       Перенести из прототипа

  + TODO: Реализация "Текущий объект", рамка, направление, крестик движения
  +       Перенести из прототипа.
  +       Перенесено, не проверено.
  +       Внести в класс UserControl

  + TODO: При отключении flight mode переводить игрока в главную плоскость
          и возвращать камеру на нормальную позицию.

  + TODO: Индикатор скорости игрока

  + TODO: Перерисовать direction для target

  + TODO: Эффекты при столкновении снарядов с астероидами или разрушаемыми
          объектами
  + TODO: Индикатор расстояния до цели

    TODO: Надо бы комментарии по коду расставить, для себя

  + TODO: Туториал - стрельба по резко двигающимя целям. Цели имеют здоровье
          Нельзя отпускать цели далеко от себя.

        + TODO: Реализация списка объектов для выделения
                У каждого Spacefighter есть список его врагов, нейтралов и друзей
                Определяется по group_id, "клану". Например
                0 - нейтральные объекты
                1 - дружественные игроку (и сам игрок)
                2 - враждебные игроку

        +       При добавлении объекта в AllObjects требуется Notify, который
                добавляет этот объект в списки у spacefighter-ов.

        + TODO: Реализация класс туториала, у него должен быть
                доступ к mainGame для управления
                Решено через uGameObjects.pas

        + TODO: Реализация ИИ тренировочной мишени

        + TODO: Графическая реализация мишени - полупрозрачный корабль
                синего или иного цвета (аналог голограммы, псевдонастоящий
                корабль)

        + TODO: Обучающие надписи. Возможно, стоит создать игровой экран
                паузы с подсказками?

        + TODO: Сценарий обучения, тексты

        + TODO: Добавить иконку таймера, допилить его появление и исчезновение

        + TODO: Реплей при неуничтожении всех объектов

        + TODO: Игровой экран "Увы, в альфа версии больше нечего делать" и кнопки
                "Выйти" и "Я еще полетаю".

        + TODO: Общая шлифовка туториала

        + TODO: Добавить число оставшихся мишеней

  + TODO: Допилить NewGame - выравнять тексты, поправить тексты

  + TODO: Добавить FadeIn для MainGame.

1   TODO: Нормально реализовать класс планет и других удаленных объектов

1   TODO: FadeIn вызывает Load(), FadeOutComplete - Unload(). Регламентировать
          для всех GameScreen-ов, сделать Load и UnLoad защищенными (protected)

1   TODO: Реализовать класс таймера отдельно

2   TODO: Физика на боксах, возможность задать несколько боксов или сфер
          для объекта. Предпроверка коллизий по баундинг сферам

3   TODO: Создание режима гонки

?   TODO: Переделать титры, добавить им анимацию (уезжаем типа вверх?)
          Добавить "спасибо" для Fantom-а, Веон-а, Yar-а и сообщества glscene.ru
          Предусмотреть место под тестеров

  - TODO: Статусы gssNone, gssFadeOutComplete - регламентировать вызовы

  + TODO: Альтернатива IsKeyDown(VK_LBUTTON), иначе в credits-ах нереально что-то
          посмотреть. Продумать систему MouseDown / MouseClick
    +-Частично решено в dfKeyboardExt
    + Решено, работает

  + TODO: Допилить кредитсы, добавить надпись для сайта GLScene.ru
    +-Частично сделано
    + Сделано

  + TODO: Разобраться со статусами, load-ами и действиями. Уже каша
    +-Частично решено. Остается вопрос о том, что gssFadeOutComplete дублирует
      gssNone и gssFadeInComplete дублирует gssReady

  + TODO: доделать NewGame
    +- Пока сойдет, можно потом попилить
    +  Допилено

  + TODO: Перенести игровую логику из прототипа

  + TODO: Добавить надпись left mouse button to skip it!

  Roadmap на техническую часть, рефакторинг:
  Дата начала работ: после альфы проекта
  + 1. Создание единого предка для UserControl и AIControl
  + 2. Единый предок для всех видимых трехмерных объектов игры
       Общие свойства: "жизнь", "разрушаемость", физическая сущность,
       процедура апдейта
    3. Разгрестись с Load-Unload игровых экранов. Load должен вызывать
       настройку начальных параметров, UnLoad - выгружать ненужные данные
    4. Унификация загрузки - возможность загружать большую часть ресурсов и
       параметров из файлов. Вероятно, рассмотреть вопрос о поддержке XML
       вместо ini. Упаковка в архивы, либо перегонка в бинарные файлы
    5. Редактор мира для расстановки объектов
    6. У TdfTarget Chaser может быть только объектом с оружием. Значит, следует
       создать потомка TdfGameObject, который может обладать оружием и
       интеллектом
    7. У TdfGameObject следует создать Enabled, при Enabled=False -
       запрещать Update

  Roadmap на геймплейную часть:
  Дата начала работ: после альфы проекта
    1. Решить статус игры: sandbox с фриплеем, или жестко заданные миссии
    2. Далее - какие составляющие будут в игре:
       * Квесты (помимо основных), и как они будут выглядеть
       * Экономика (покупка кораблей, вооружения, оборудования)
       * Экономика 2 (продажа хлама, добыча руды из астероидов)
       * РПГ-составляющие - отыгрыш роли, навыки, репутация
       * Стратегические составляющие - экономика на планетах


}

unit uGame;

interface

uses
  Classes, Controls, Windows, SysUtils, Contnrs, Forms, IniFiles,

  GLScene, GLWin32Viewer, GLCadencer, GLObjects, GLRenderContextInfo,
  GLCrossPlatform, GLMaterial, GLParticleFX, GLPerlinPFX,

  uGameScreen, uLog;

const
  C_WAIT_BEFORE_EXIT = 0.3;

  C_SYSTEM_FILE = 'data\system.ini';

  C_MAIN_SECTION   = 'Main';
  C_CAMERA_SECTION = 'Camera';
  C_BUFFER_SECTION = 'Buffer';

type
  TdfGame = class
  private
    FOwner: TComponent;

    FScene: TGLScene;
    FViewer: TGLSceneViewer;
    FCadencer: TGLCadencer;
    FMatLib: TGLMaterialLibrary;
    FEngineFX, FBoomFX, FBigBoomFX: TGLPerlinPFXManager;

    FCamera: TGLCamera;
    FLight: TGLLightSource;
    FGameScreens: TObjectList;
    FActiveScreen: TdfGameScreen;

    FSubject: TdfGameScreen;
    FAction: TdfNotifyAction;

    FmX, FmY: Integer; //позиция курсора мыши

    FWait: Double;

    FFPSInd: Integer;

    t: Double;

//    FGLSceneInfo: TdfGLSceneInfo;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Progress(Sender: TObject; const deltaTime, newTime: Double);
    function GetActiveScene: TdfGameScreen;
    procedure SetActiveScene(const aScene: TdfGameScreen);
  public
    constructor Create(aOwner: TComponent); virtual;
    destructor Destroy; override;

    function AddGameScene(aGameSceneClass: TdfGameSceneClass): TdfGameScreen;


    procedure NotifyGameScenes(Subject: TdfGameScreen; Action: TdfNotifyAction);

    property ActiveScreen: TdfGameScreen read GetActiveScene write SetActiveScene;

    procedure Start;
    procedure Stop;
  end;

implementation

uses
  uTweener, uFonts, uDebugInfo, uGLSceneObjects,
  GLVectorGeometry;

{ TdfGame }

procedure TdfGame.Progress(Sender: TObject; const deltaTime, newTime: Double);
var
  i: Integer;
begin
  FViewer.Invalidate;
  Tweener.Update(deltaTime);
  t := t + deltaTime;
  dfDebugInfo.UpdateParam(FFPSInd, FViewer.FramesPerSecond);
  if t >= 1 then
  begin
    FViewer.ResetPerformanceMonitor;
    t := 0;
  end;
  for i := 0 to FGameScreens.Count - 1 do
    TdfGameScreen(FGameScreens[i]).Update(deltaTime, FmX, FmY);

  if FActiveScreen.Status = gssFadeInComplete then
  begin
    logWriteMessage('TdfGame: "' + FActiveScreen.Name + '" status gssFadeInComplete');
    FActiveScreen.Status := gssReady;
  end;

  if Assigned(FSubject) then
    case FAction of
      naNone: Exit;

      naSwitchTo:
        if (FActiveScreen.Status = gssFadeOutComplete) or
           (FActiveScreen.Status = gssNone) then
        begin
          logWriteMessage('TdfGame: "' + FActiveScreen.Name + '" status gssFadeOutComplete');
          FActiveScreen.Status := gssNone;
          FActiveScreen.Unload;
          FSubject.Load();
          if FSubject.Status = gssPaused then
            FSubject.Status := gssReady
          else
            FSubject.Status := gssFadeIn;
          FActiveScreen := FSubject;
          FSubject := nil;
        end;

      naSwitchToQ:
      begin
        FActiveScreen.Status := gssFadeOutComplete;
        FActiveScreen.Unload;
        FSubject.Load;
        FSubject.Status := gssReady;
        FActiveScreen := FSubject;
        FSubject := nil;
      end;

      naPreload: Exit;


      //Показываем Subject поверх текущей ActiveScene
      naShowModal:
      begin
        FActiveScreen.Status := gssPaused;
        FSubject.Load();
        FSubject.Status := gssFadeIn;
        FActiveScreen := FSubject;
        FSubject := nil;
      end;
//      else
//      begin
//        FSubject.Update(deltaTime, FmX, FmY);
//      end;
    end;

  if FAction = naQuitGame then
  begin
    FWait := FWait + deltaTime;
    if FWait >= C_WAIT_BEFORE_EXIT then
    begin
      PostQuitMessage(0);
      Stop();
      Free;
    end;
  end;
end;


function TdfGame.GetActiveScene: TdfGameScreen;
begin
  if Assigned(FActiveScreen) then
    Result := FActiveScreen
  else
  begin
    Result := nil;
    logWriteError('TdfGame: GetActiveScene - nil at FActiveScene!');
  end;
end;

procedure TdfGame.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FmX := X;
  FmY := Y;
end;

procedure TdfGame.SetActiveScene(const aScene: TdfGameScreen);
begin
  if Assigned(aScene) then
    if not Assigned(FActiveScreen) then
    begin
      FActiveScreen := aScene;
      FActiveScreen.Load();
      FActiveScreen.Status := gssFadeIn;
    end
    else
      if aScene <> FActiveScreen then
        NotifyGameScenes(aScene, naSwitchTo);
end;

procedure TdfGame.Start;
begin
//  dfGLSceneObjects.HUDDummy.MoveLast;
  dfDebugInfo.MoveLast;
  FCadencer.Enabled := True;
end;

procedure TdfGame.Stop;
begin
  FCadencer.Enabled := False;
end;

function TdfGame.AddGameScene(aGameSceneClass: TdfGameSceneClass): TdfGameScreen;
begin
  Result := aGameSceneClass.Create();
  Result.OnNotify := Self.NotifyGameScenes;
  logWriteMessage('TdfGame: Added "' + Result.Name + '" game scene with index: ' + IntToStr(FGameScreens.Add(Result)));
end;

constructor TdfGame.Create(aOwner: TComponent);
var
  ind: Integer;
  Ini: TIniFile;
begin
  inherited Create();
  if FileExists(C_SYSTEM_FILE) then
  begin
    Ini := TIniFile.Create(C_SYSTEM_FILE);
    FOwner := aOwner;

    FScene := TGLScene.Create(FOwner);
    FScene.VisibilityCulling := vcNone;
//    FScene.ObjectsSorting := osInherited;
//    FScene.ObjectsSorting :=

    FCadencer := TGLCadencer.Create(FOwner);
    FCadencer.Enabled := False;
    FCadencer.OnProgress := Self.Progress;
    FCadencer.Scene := FScene;

    FCamera := TGLCamera.CreateAsChild(FScene.Objects);
    FCamera.DepthOfView := Ini.ReadFloat(C_CAMERA_SECTION, 'DepthOfView', 1000);
    FCamera.Direction.SetVector(0,0,1);
    FCamera.FocalLength := 90;

    FViewer := TGLSceneViewer.Create(FOwner);
    FViewer.Buffer.DepthPrecision := dp24bits;
    FViewer.Camera := FCamera;
    FViewer.Align := alClient;
    FViewer.Parent := TWinControl(aOwner);
    FViewer.OnMouseMove := MouseMove;

    FMatLib := TGLMaterialLibrary.Create(FOwner);

    FEngineFX := TGLPerlinPFXManager.Create(FOwner);
    FEngineFX.Cadencer := FCadencer;

    {+debug}
    FEngineFX.ColorInner.SetColor(0.5, 0.5, 0.9, 1.0);
    FEngineFX.ColorOuter.SetColor(0.1, 0.1, 0.8, 0.5);
    FEngineFX.ParticleSize := 2.1;
    with FEngineFX.LifeColors.Add do
    begin
      LifeTime := 0.3;
    end;
    FBoomFX := TGLPerlinPFXManager.Create(FOwner);
    FBoomFX.Cadencer := FCadencer;
    FBoomFX.ColorInner.SetColor(0.9, 0.5, 0.5, 1.0);
    FBoomFX.ColorOuter.SetColor(0.7, 0.5, 0.5, 0.5);
    FBoomFX.ParticleSize := 1.4;

    with FBoomFX.LifeColors.Add do
    begin
      LifeTime := 0.4;
    end;

    FBigBoomFX := TGLPerlinPFXManager.Create(FOwner);
    FBigBoomFX.Cadencer := FCadencer;
    FBigBoomFX.ColorInner.SetColor(0.9, 0.7, 0.7, 1.0);
    FBigBoomFX.ColorOuter.SetColor(0.8, 0.4, 0.4, 0.5);
    FBigBoomFX.ParticleSize := 5.5;
    with FBigBoomFX.LifeColors.Add do
    begin
      LifeTime := 1.0;
    end;
    {-debug}

    FGameScreens :=TObjectList.Create(True);
    FActiveScreen := nil;
    FSubject := nil;

    FLight := TGLLightSource.CreateAsChild(FCamera);
    FLight.Diffuse.SetColor(1, 1, 1);

    FViewer.Buffer.BackgroundColor := RGB(
      Ini.ReadInteger(C_BUFFER_SECTION, 'backR', 0),
      Ini.ReadInteger(C_BUFFER_SECTION, 'backG', 0),
      Ini.ReadInteger(C_BUFFER_SECTION, 'backB', 0));

    {+debug}
    ind := uFonts.AddFont(aOwner, C_FONT_1);
    with GetFont(ind) do
      Font.Size := 24;

    with GetFont(AddFont(aOwner, C_FONT_2)) do
      Font.Size := 10;

    {-debug}

    //Заполняем синглтоны
    with dfGLSceneObjects do
    begin
      Scene := FScene;
      Viewer := FViewer;
      Cadencer := FCadencer;
      MatLibrary := FMatLib;
      EnginesFX[0] := FEngineFX;
      BoomFX[0] := FBoomFX;
      BoomFX[1] := FBigBoomFX;
      Camera := FCamera;
    end;

    dfDebugInfo := TdfDebugInfo.CreateAsChild(FScene.Objects);
    dfDebugInfo.Position.Z := -0.5;
    dfDebugInfo.BitmapFont := GetFont(C_FONT_2);
    dfDebugInfo.Layout := tlTop;
    dfDebugInfo.Alignment := taLeftJustify;
    dfDebugInfo.Visible := Ini.ReadBool(C_MAIN_SECTION, 'Debug', False);
    FFPSInd := dfDebugInfo.AddNewString('FPS');

    Ini.Free;
  end
  else
  begin
    logWriteError('TdfGame: File ' + C_SYSTEM_FILE + ' not founded', True, True, True);
    Free;
  end;
end;

destructor TdfGame.Destroy;
begin
  //Так как компоненты GLScene, GLViewer и GLCadencer были созданы с родителем
  //То их освободит родитель, в нашем случае - сама форма
  FGameScreens.Free;
  inherited;
end;

procedure TdfGame.NotifyGameScenes(Subject: TdfGameScreen; Action: TdfNotifyAction);
begin
  if FAction = naQuitGame then
    Exit;
  FSubject := Subject;
  FAction := Action;

  case Action of
    naSwitchTo:
    begin
//      logWriteMessage('TdfGame: Notified action: switch to "' + Subject.Name + '" game scene');
      if Assigned(FActiveScreen) then
        FActiveScreen.Status := gssFadeOut
      else
      begin
        FSubject.Status := gssFadeIn;
      end;
    end;
    naSwitchToQ: Exit;
    naPreload: Subject.Load();
    naQuitGame: FWait := 0;
    naShowModal: Exit;
  end;
end;

end.
