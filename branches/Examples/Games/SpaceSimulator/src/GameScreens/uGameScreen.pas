{Концепция GameScreen - некоторый интерактивный экран, являющийся активным у юзверя.
 Говоря проще, это базовый класс для главного меню, самой игры и различных ее режимов
 Данные сущности (меню, сама игра) реализуются в потомках класса}
unit uGameScreen;

interface

uses
  GLScene, GLWin32Viewer, GLMaterial, GLParticleFX;

type
  TdfGameScreen = class;

  //Действия, которые экран может отсылать классу TdfGame
  //Нет действия, переключиться, переключиться без показа FadeIn и FadeOut,
  //Показать поверх, предзагрузка, выйти из игры
  TdfNotifyAction = (naNone, naSwitchTo, naSwitchToQ, naShowModal, naPreload, naQuitGame);

  //Процедура уведомления. Уведомляем о том, что хотим сделать
  // с Subject действие Action. Предыдущая сцена, т.е. Sender записана в TdfGame-е
  TdfNotifyProc = procedure(Subject: TdfGameScreen; Action: TdfNotifyAction) of object;

  //Статус текущего экрана
  //gssNone - нет статуса. не update-ится и не показывается
  //gssReady - активный на данный момент, Update выполняется, реакция на действия
  //gssFadeIn - идет процесс показа (появления)
  //gssFadeInComplete - процесс появления завершен, автоматически переключается в gssReady
  //gssFadeOut - идет процесс скрывания с экрана
  //gssFadeOutComplete - процесс скрытия завершен, автоматически переключается в gssNone
  //gssPaused - пауза, экран продолжает отрисовываться, но не Update-ится
  TdfGameSceneStatus = (gssNone, gssReady, gssFadeIn, gssFadeInComplete,
                        gssFadeOut, gssFadeOutComplete, gssPaused);

  {О правилах загрузки:
   Create - содержит данные, которые загрязсят один раз и будут висеть в памяти
            на протяжении всей игры

   Load   - Непосредственная подгрузка при активации данного экрана

   Рекомендуется: Создавать все GL-объекты в Create, но не помещать их в
   рендер-узел GLScene.Objects сразу, а делать это при Load. Также при Load
   следует подгрухать игровые данные}
  TdfGameScreen = class
  private
  protected
    FLoaded: Boolean;
    FName: String;
    FStatus: TdfGameSceneStatus;
    FNotifyProc: TdfNotifyProc;

    procedure FadeIn(deltaTime: Double); virtual;
    procedure FadeOut(deltaTime: Double); virtual;
    procedure SetName(const Value: String); virtual;

    function GetStatus: TdfGameSceneStatus; virtual;
    procedure SetStatus(const aStatus: TdfGameSceneStatus); virtual;
    function GetLoaded: Boolean; virtual;
  public
    constructor Create(); virtual; abstract;
    destructor Destroy; override; abstract;

    procedure Load(); virtual; abstract;
    procedure Unload(); virtual; abstract;

    procedure Update(deltaTime: Double; X, Y: Integer); virtual; abstract;

    property OnNotify: TdfNotifyProc read FNotifyProc write FNotifyProc;
    property Status: TdfGameSceneStatus read GetStatus write SetStatus;
    property IsLoaded: Boolean read GetLoaded;
    property Name: String read FName write SetName;
  end;

  TdfGameSceneClass = class of TdfGameScreen;

implementation

{ TdfGameScene }

procedure TdfGameScreen.FadeIn(deltaTime: Double);
begin
  Status := gssFadeInComplete;
end;

procedure TdfGameScreen.FadeOut(deltaTime: Double);
begin
  Status := gssFadeOutComplete;
end;

function TdfGameScreen.GetLoaded: Boolean;
begin
  Result := FLoaded;
end;

function TdfGameScreen.GetStatus: TdfGameSceneStatus;
begin
  Result := FStatus;
end;

procedure TdfGameScreen.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TdfGameScreen.SetStatus(const aStatus: TdfGameSceneStatus);
begin
  FStatus := aStatus;
end;

end.
