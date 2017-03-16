unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Math,
  Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLObjects, GLAsyncTimer,
  GLWin32Viewer, GLGeomObjects, GLTexture, GLHUDObjects,
  GLVectorTypes, GLSpaceText, GLVectorGeometry,
  GLParticleFX, GLPerlinPFX, GLKeyboard, GLBitmapFont, GLWindowsFont,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    AsyncTimer1: TGLAsyncTimer;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    txt_gl: TGLSpaceText;
    txt_scene: TGLSpaceText;
    GLPointLightPFXManager1: TGLPointLightPFXManager;
    GLPointLightPFXManager2: TGLPointLightPFXManager;
    DC_emitter: TGLDummyCube;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure CharType(ntype:integer);
  end;

var
  Form1:TForm1;
  _type:integer=0;      // effect type - rain/snow/fog
  _shift:boolean=false; // indicator of any mouse button pressing
  _mx:Integer;          // prev mouse button
  _zoom:single=0;       //

implementation
{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  glHUDtext1.Text:='1 rain'#13+'2 snow'#13+'3 fog';
  CharType(0);
end;

procedure TForm1.CharType(ntype:integer);
var a:integer;
begin
  _type:=ntype; // remember selected effect
  for a:=0 to 2 do
  // switch off unnecessary effects and switch on selected one
  with TGLSourcePFXEffect(GLDummyCube1.Effects[a]) do
    if _type=a then
      Enabled:=true
    else
      Enabled:=false;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  _zoom:=WheelDelta/120; // remember position of mouse wheel
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if _shift then
     GLDummyCube1.Turn(_mx-mouse.CursorPos.x) // rotating camera by mouse
  else
      GLDummyCube1.Turn(deltatime*10);       // othrwise auto rotation

    _mx:=Mouse.CursorPos.x; // запоминаем координату

  GLCamera1.AdjustDistanceToTarget(Power(1.1, _zoom)); // приближаемся-удаляемся
    _zoom:=0;

  if IsKeyDown('1')then CharType(0); // rain
  if IsKeyDown('2')then CharType(1); // snow
  if IsKeyDown('3')then CharType(2); // fog
  if IsKeyDown(VK_ESCAPE)then close;

  case _type of
    // подстройки "дождика" к вращению камеры
    0:with TGLSourcePFXEffect(GLDummyCube1.Effects[0])do
    begin
        // координаты эмиттера
        InitialPosition.AsVector:=dc_emitter.AbsolutePosition;
        // направление к центру
        InitialVelocity.AsVector:=VectorScale(VectorNormalize(VectorNegate(dc_emitter.AbsolutePosition)),10);
        // поворачиваем частицы к центру
        GLPointLightPFXManager1.Rotation:=-arctan(InitialVelocity.X/InitialVelocity.Y);
    end;
    // подстройки "снежка" относительно вращения камеры
    1:with TGLSourcePFXEffect(GLDummyCube1.Effects[1])do
    begin
        // координаты эмиттера
        InitialPosition.AsVector:=dc_emitter.AbsolutePosition;
        // направляем к центру
        InitialVelocity.AsVector:=VectorScale(VectorNormalize(VectorNegate(dc_emitter.AbsolutePosition)),4);
    end;
  end;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption := vp.FramesPerSecondText(2); // выводим количество кадров в секунду
  vp.ResetPerformanceMonitor;         // и обнуляем счётчик
end;

procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _shift := true; // pressed button
end;

procedure TForm1.vpMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _shift := false; // кнопка отжата
end;

end.


