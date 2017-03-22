unit uTutorial;

interface

uses
  Classes, SysUtils,

  GLScene, GLObjects, GLHUDObjects, GLWindowsFont, GLCrossPlatform,
  GLVectorGeometry, GLVectorTypes, GLFilePNG, GLMaterial, GLTexture,

  uGameObject, uSimplePhysics, uFakeSpacefighter, uFighterControl.AI;


const
  C_HUD_PATH = 'data\hud\';

  C_STAGES_COUNT = 13;
  C_TEXTS: array[0..C_STAGES_COUNT - 1] of String =
    ('Добро пожаловать, кадет. Четко следуйте инструкциям,'+ #13#10 +
     ' и вы сдадите экзамен.',

     'Чтобы начать экзамен, включите активный режим полета'+ #13#10 +
     'с помощью клавиши "ПРОБЕЛ".',

     'Подлетите к обозначенной точке, используя мышь для ' + #13#10 +
     'направления и клавиши W и S для управления тягой двигателя.',

     'Отлично! Запомните эти приемы!',

     'Теперь потренируемся в стрельбе: используйте мышь чтобы' + #13#10 +
     'повернуть корабль и его орудия в сторону цели. Затем'  + #13#10 +
     'нажмите "ПРАВУЮ КНОПКУ МЫШИ", чтобы открыть огонь из' + #13#10 +
     'орудий корабля по учебной мишени.',

     'Отлично, цель уничтожена!',

     'Когда цель находится вне зоны видимости, на экране'  + #13#10 +
     'отображается индикатор направления разворота на цель.' + #13#10 +
     'Поверните корабль на цель и откройте огонь.',

     'Еще одна учебная мишень уничтожена!',

     'Усложним задачу: теперь цель будет двигаться, и чтобы' + #13#10 +
     'ее поразить потребуется стрелять на упреждение.' + #13#10 +
     'Упреждение вычисляется бортовым компьютером'+#13#10+
     'и отображается на экране красным перекрестием.',

     'Вы отлично справились!',

     'Теперь целей будет несколько, и ваша задача -' + #13#10 +
     'поразить их в отведенное время! Используйте' + #13#10 +
     'клавишу "R", чтобы переключать цели',

     'Отлично, учебные цели поражены!',

     'Вы успешно сдали экзамен, кадет! У вас есть ' + #13#10 +
     'полтора часа полетного времени, затем вам' + #13#10 +
     'следует вернуться на базу для получения' + #13#10 +
     'дальнейших инструкций.'
    );

  C_SUCCESS_TEXTS: set of Byte = [0, 3, 5, 7, 9, 11];
  C_SUCCESS_TEXT_DELAY = 5.0;
  C_SUCCESS_TEXT_DELAY_T = '5.0';
  C_RETRY_TEXT = 'Вы не справились с задачей, перезапуск '+ #13#10 +
                 'попытки через ' + C_SUCCESS_TEXT_DELAY_T + ' секунд';

  C_TEXT_SCALE = 0.5;

  C_MAX_TARGETS = 10;

  C_TARGETS_LEFT_TEXT = 'Осталось целей: ';

  C_TIME_TO_ELIMINATE = 300.0;

  C_WAYPOINT_BBRAD = 20.0;

type
  TdfSimpleNotify = procedure of object;

  TdfGameObjectNotifyAction = (naAdded, naRemoved, naChanged);

  TdfGameObjectNotify = procedure(aObj: TdfGameObject; aAction: TdfGameObjectNotifyAction) of object;

  TdfTutorialMission = class
  private
    FEnabled: Boolean;
    FT, FT2: Single;
    FGONotify: TdfGameObjectNotify;
    FOnEnd: TdfSimpleNotify;
    FMissionText, FCounterText, FTargetsLeft: TGLHUDText;
    FCounterTimer: TGLHUDSprite;
    FStage: Integer;

    {Stages}
    FPlayerBBRad: Single;
    FWaypoint0: TdfGameObject;
    FFighters: array[0..C_MAX_TARGETS - 1] of TdfFakeSpaceFighter;
    FAIControls: array[0..C_MAX_TARGETS - 1] of TdfAIControl;

    FFightersDestroyed: Integer;

    procedure SetEnabled(const Value: Boolean);
    procedure SetStage(const Value: Integer);
    procedure OnDie(Sender: TdfGameObject);
  protected
    {StageChecks - простейшие триггеры}
    function StageCheck1: Boolean; //Включить динамичный режим полета
    function StageCheck2: Boolean; //Подлететь к заданной точке
    function StageCheck4: Boolean; //Первая цель уничтожена
    function StageCheck6: Boolean; //Вторая цель уничтожена
    function StageCheck8: Boolean; //Третья подвижная цель уничтожена
    function StageCheck10: Boolean; //Несколько целей в отведенное время

    procedure PrepareStage2;
    procedure PrepareStage4;
    procedure PrepareStage6;
    procedure PrepareStage8;
    procedure PrepareStage10;
    procedure PrepareStage12;
    procedure PrepareFailStage;

  public
    constructor Create(aObjectsRoot, aHUDRoot: TGLBaseSceneObject;
      aFont: TGLWindowsBitmapFont); virtual;
    destructor Destroy; override;

    procedure Start();
    procedure Stop();

    procedure Update(deltaTime: Double);

    property OnGameObjectNotify: TdfGameObjectNotify read FGONotify write FGONotify;
    property OnMissionEnd: TdfSimpleNotify read FOnEnd write FOnEnd;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Stage: Integer read FStage write SetStage;
  end;

implementation

uses
  uGLSceneObjects, uGameObjects;

{ TdfTutorialMission }


procedure TdfTutorialMission.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  FMissionText.Visible := Value;
end;


procedure TdfTutorialMission.SetStage(const Value: Integer);
begin
  FStage := Value;
  FT := 0;
  if (FStage < C_STAGES_COUNT) and (FStage > -1) then
    FMissionText.Text := C_TEXTS[FStage];
  case FStage of
   -1: PrepareFailStage;
    2: PrepareStage2;
    4: PrepareStage4;
    6: PrepareStage6;
    8: PrepareStage8;
    10: PrepareStage10;
    12: PrepareStage12;
  end;

  if FStage = C_STAGES_COUNT - 1 then
    OnMissionEnd;
end;

constructor TdfTutorialMission.Create(aObjectsRoot, aHUDRoot: TGLBaseSceneObject;
  aFont: TGLWindowsBitmapFont);
var
  x, y: Single;
  i: Integer;
begin
  inherited Create;

  FMissionText := TGLHUDText.CreateAsChild(aHUDRoot);
  FMissionText.BitmapFont := aFont;
  FMissionText.Scale.Scale(C_TEXT_SCALE);
  FMissionText.ModulateColor.SetColor(1.0, 0.8, 0.8, 1);

  x := dfGLSceneObjects.Viewer.Width div 2;
  y := 15;
  FMissionText.Position.SetPoint(x, y, 0.5);
  FMissionText.Alignment := taCenter;
  FMissionText.Layout := tlTop;
  aFont.EnsureChars('А', 'я');

  FTargetsLeft := TGLHUDText.CreateAsChild(aHUDRoot);
  FTargetsLeft.BitmapFont := aFont;
  x := 15;
  y := dfGLSceneObjects.Viewer.Height - 50;
  FTargetsLeft.Position.SetPoint(x, y, 0.2);
  FTargetsLeft.Alignment := taLeftJustify;
  FTargetsLeft.Layout := tlCenter;
  FTargetsLeft.Visible := False;
  FTargetsLeft.Scale.Scale(0.7);
  FTargetsLeft.Text := C_TARGETS_LEFT_TEXT;
  FTargetsLeft.ModulateColor.SetColor(0.5, 1.0, 0.5);

  {+debug - надо переделать в отдельный класс таймера}
  FCounterText := TGLHUDText.CreateAsChild(aHUDRoot);
  FCounterText.BitmapFont := aFont;
  FCounterText.ModulateColor.SetColor(0.5, 1.0, 0.5);
  x := dfGLSceneObjects.Viewer.Width div 2;
  y := dfGLSceneObjects.Viewer.Height - 50;
  FCounterText.Position.SetPoint(x, y, 0.5);
  FCounterText.Alignment := taCenter;
  FCounterText.Layout := tlCenter;
  FCounterText.Visible := False;
  FCounterText.Scale.Scale(0.7);

  FCounterTimer := TGLHUDSprite.CreateAsChild(aHUDRoot);
  FCounterTimer.Position.SetPoint(x, y, 0.2);
  with FCounterTimer.Material do
  begin
    Texture.Image.LoadFromFile(C_HUD_PATH + 'timer.png');
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    Texture.TextureWrap := twNone;
    BlendingMode := bmTransparency;
    MaterialOptions := [moIgnoreFog, moNoLighting];
    FrontProperties.Diffuse.SetColor(0.5, 1.0, 0.5);
  end;
  FCounterTimer.SetSize(FCounterTimer.Material.Texture.Image.Width * 1.5,
    FCounterTimer.Material.Texture.Image.Height * 1.5);
  FCounterTimer.Visible := False;

  {-debug}

  {Stages}
  FPlayerBBRad := dfGameObjects.Player.Fighter.BoundingSphereRadius;
  FWaypoint0 := TdfGameObject.CreateAsChild(aObjectsRoot);
  FWaypoint0.Position.SetPoint(100, -100, 500);
  FWaypoint0.GroupID := C_GROUP_OBJECT;
  with dfPhysics.AddPhysToObject(FWaypoint0, psSphere) do
  begin
    IsStatic := True;
    UserType := C_PHYS_INVINCIBLE;
  end;

  for i := 0 to C_MAX_TARGETS - 1 do
  begin
    FFighters[i] := TdfFakeSpaceFighter.CreateAsChild(aObjectsRoot);
    FFighters[i].LoadFromFile('sfFake.ini');
    FFighters[i].GroupID := C_GROUP_ENEMIES;
    FFighters[i].Position.SetPoint(100 * (i + 1), 20 * (i + 1), 300 * (i + 1));
    FFighters[i].Visible := False;
    FFighters[i].Phys.Enabled := False;
    FFighters[i].OnDie := Self.OnDie;

    FAIControls[i] := TdfAIControl.Create(FFighters[i]);
    FAIControls[i].Enabled := False;
  end;


  Enabled := False;
end;

destructor TdfTutorialMission.Destroy;
var
  i: Integer;
begin
  for i := 0 to C_MAX_TARGETS - 1 do
  begin
    FAIControls[i].Free;
    FFighters[i].Free;
  end;
  FCounterText.Free;
  FCounterTimer.Free;
  FTargetsLeft.Free;
  FMissionText.Free;
  FWaypoint0.Free;
  inherited;
end;

procedure TdfTutorialMission.OnDie(Sender: TdfGameObject);
begin
  dfGameObjects.BigBoom.SetBoom(AffineVectorMake(Sender.AbsolutePosition), 1);
  TdfFakeSpaceFighter(Sender).Phys.Enabled := False;
  Sender.Visible := False;
  Inc(FFightersDestroyed);
  OnGameObjectNotify(Sender, naRemoved);
  if FStage = 10 then
    FTargetsLeft.Text := C_TARGETS_LEFT_TEXT + IntToStr(C_MAX_TARGETS - FFightersDestroyed);
end;

{$REGION ' Stages - говнокод, так как нет скриптов'}

procedure TdfTutorialMission.PrepareStage2;
begin
  dfGameObjects.UserControl.Target.Target := FWaypoint0;
end;

procedure TdfTutorialMission.PrepareStage4;
begin

  FFightersDestroyed := 0;

  with FFighters[0] do
  begin
    ResetParams();
    Position.AsAffineVector := VectorScale(dfGameObjects.Player.Direction.AsAffineVector, 300);
    Phys.Enabled := True;
    Visible := True;
  end;

  FAIControls[0].Enabled := False;

  FGONotify(FFighters[0], naAdded);

  dfGameObjects.UserControl.Target.Target := FFighters[0];
end;

procedure TdfTutorialMission.PrepareStage6;
begin
  FFightersDestroyed := 0;
  with FFighters[0] do
  begin
    ResetParams();
    Position.AsAffineVector := VectorScale(dfGameObjects.Player.Direction.AsAffineVector, -200);
    Phys.Enabled := True;
    Visible := True;
  end;

  FAIControls[0].Enabled := False;
  FGONotify(FFighters[0], naAdded);
  dfGameObjects.UserControl.Target.Target := FFighters[0];
end;

procedure TdfTutorialMission.PrepareStage8;
var
  dir: TAffineVector;
begin
  FFightersDestroyed := 0;
  with FFighters[0] do
  begin
    ResetParams();
    Position.AsAffineVector := VectorScale(dfGameObjects.Player.Direction.AsAffineVector, 800);
    RandomPointOnSphere(dir);
    Direction.AsAffineVector := dir;
    Phys.Enabled := True;
    Visible := True;
  end;

  FGONotify(FFighters[0], naAdded);
  dfGameObjects.UserControl.Target.Target := FFighters[0];
  FAIControls[0].Enabled := True;
  FFighters[0].Speed := FFighters[0].MaxSpeed * 2 / 3;
end;

procedure TdfTutorialMission.PrepareFailStage;
begin
  FMissionText.Text := C_RETRY_TEXT;
  FT := 0;
end;

procedure TdfTutorialMission.PrepareStage10;
var
  i: Integer;
  dir, origin: TAffineVector;
begin
  FT2 := 0;
  FFightersDestroyed := 0;
  origin := AffineVectorMake(0, 0, 800);
  for i := 0 to C_MAX_TARGETS - 1 do
  begin
    with FFighters[i] do
    begin
      ResetParams();
      RandomPointOnSphere(dir);
      Position.AsAffineVector := VectorScale(dir, 300);
      RandomPointOnSphere(dir);
      Direction.AsAffineVector := dir;
      Phys.Enabled := True;
      Visible := True;
      Speed := MaxSpeed * 2/3;
    end;
    FGONotify(FFighters[i], naAdded);
    FAIControls[i].Enabled := True;
  end;
  dfGameObjects.UserControl.Target.Target := FFighters[0];
  FCounterText.Visible := True;
  FCounterTimer.Visible := True;
  FTargetsLeft.Visible := True;
  FTargetsLeft.Text := C_TARGETS_LEFT_TEXT + IntToStr(C_MAX_TARGETS);
end;

procedure TdfTutorialMission.PrepareStage12;
begin
  FCounterText.Visible := False;
  FCounterTimer.Visible := False;
  FTargetsLeft.Visible := False;
end;

function TdfTutorialMission.StageCheck1: Boolean;
begin
  Result := dfGameObjects.UserControl.MouseMode;
end;

function TdfTutorialMission.StageCheck2: Boolean;
begin
  Result := VectorDistance(FWaypoint0.Position.AsAffineVector,
    dfGameObjects.Player.Position.AsAffineVector) < FPlayerBBRad + C_WAYPOINT_BBRAD;
  if Result then
    dfGameObjects.UserControl.Target.Target := nil;
end;

function TdfTutorialMission.StageCheck4: Boolean;
begin
  Result := FFightersDestroyed >= 1;
  if Result then
    dfGameObjects.UserControl.Target.Target := nil;
end;

function TdfTutorialMission.StageCheck6: Boolean;
begin
  Result := FFightersDestroyed >= 1;
  if Result then
    dfGameObjects.UserControl.Target.Target := nil;
end;

function TdfTutorialMission.StageCheck8: Boolean;
begin
  Result := FFightersDestroyed >= 1;
  if Result then
    dfGameObjects.UserControl.Target.Target := nil;
end;

function TdfTutorialMission.StageCheck10: Boolean;
begin
  Result := FFightersDestroyed >= C_MAX_TARGETS;
  if Result then
  begin
    dfGameObjects.UserControl.Target.Target := nil;
//    FCounterText.Visible := False;
//    FCounterTimer.Visible := False;
  end;
end;

{$ENDREGION}

procedure TdfTutorialMission.Start;
begin
  Enabled := True;
  FT := 0;
  FT2 := 0;
  FCounterText.Visible := False;
  FCounterTimer.Visible := False;
  FTargetsLeft.Visible := False;
  Stage := 0;
end;

procedure TdfTutorialMission.Stop;
begin
  Enabled := False;
  FCounterText.Visible := False;
  FCounterTimer.Visible := False;
end;

procedure TdfTutorialMission.Update(deltaTime: Double);
var
  i: Integer;
begin
  if not Enabled then
    Exit;

  for i := 0 to C_MAX_TARGETS - 1 do
  begin
    FAIControls[i].Update(deltaTime, 0, 0);
    if FFighters[i].Visible then
      FFighters[i].Update(deltaTime);
  end;

  if FStage in C_SUCCESS_TEXTS then
  begin
    FT := FT + deltaTime;
    if FT >= C_SUCCESS_TEXT_DELAY then
      Stage := Stage + 1;
  end
  else
    case FStage of
     -1:
     begin
       FT := FT + deltaTime;
       if FT >= C_SUCCESS_TEXT_DELAY then
         Stage := 10;

     end;
      1: if StageCheck1 then Stage := Stage + 1;
      2: if StageCheck2 then Stage := Stage + 1;
      4: if StageCheck4 then Stage := Stage + 1;
      6: if StageCheck6 then Stage := Stage + 1;
      8: if StageCheck8 then Stage := Stage + 1;
      10:
      begin
        FT2 := FT2 + deltaTime;
        FCounterTimer.Rotation := FCounterTimer.Rotation + (360 / 3.14)*deltaTime;
        FCounterText.Text := IntToStr(Round(C_TIME_TO_ELIMINATE - FT2));
        if StageCheck10 then
          Stage := Stage + 1
        else
          if FT2 > C_TIME_TO_ELIMINATE then
          begin
            {Неудача, перезапуск этапа}
            for i := 0 to C_MAX_TARGETS - 1 do
            begin
              FFighters[i].Visible := False;
              FFighters[i].Phys.Enabled := False;
              FAIControls[i].Enabled := False;
            end;
            FCounterText.Visible := False;
            FCounterTimer.Visible := False;
            FTargetsLeft.Visible := False;
            dfGameObjects.UserControl.Target.Target := nil;
            Stage := -1;
          end;
      end;
    end;
end;

end.
