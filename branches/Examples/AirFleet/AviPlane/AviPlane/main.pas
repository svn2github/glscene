unit main;

interface

uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Menus,
    Dialogs,
    ExtCtrls,
    StdCtrls,
    Math,

    //GLScene
    GLScene,  GLObjects, GLWin32Viewer, GLCadencer, GLKeyboard, glAviPlane,
    GLTexture, GLMaterial, GLCoordinates,  GLCrossPlatform,
    GLBaseClasses;

type
    TForm1 = class(TForm)
        GLScene1: TGLScene;
        CamH: TGLDummyCube;
        CamV: TGLDummyCube;
        Cam: TGLCamera;
        GLCadencer1: TGLCadencer;
        Timer1: TTimer;
        MainMenu1: TMainMenu;
        Openavi1: TMenuItem;
        File1: TMenuItem;
        Quit1: TMenuItem;
        Panel1: TPanel;
        GLSceneViewer1: TGLSceneViewer;
        ScrollBar1: TScrollBar;
        Label1: TLabel;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        Label2: TLabel;
        GLCube1: TGLCube;
        GLLightSource1: TGLLightSource;
        FPSScrollBar: TScrollBar;
        CheckBox1: TCheckBox;
        Label3: TLabel;
        PosScrollBar: TScrollBar;
        procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
            newTime: Double);
        procedure FormCreate(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure Openavi1Click(Sender: TObject);
        procedure Quit1Click(Sender: TObject);
        procedure ScrollBar1Change(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure RadioButton1Click(Sender: TObject);
        procedure FPSScrollBarChange(Sender: TObject);
        procedure CheckBox1Click(Sender: TObject);
        procedure PosScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
            var ScrollPos: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    private
         
        mdpos: TPoint;
        mousedown,tracking: boolean;
        procedure AVIPlaneUpdate(Sender: TObject; FrameIndex: integer);
    public
         
        aviplane: TGLAviPlane;
    end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if Button = TMouseButton(mbRight) then
    begin
        mousedown := true;
        screen.cursor := crNone;
    end;
    GetCursorPos(mdpos);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if Button = TMouseButton(mbRight) then
    begin
        mouseDown := false;
        screen.cursor := crDefault;
    end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
    newTime: Double);
var
    deltax, deltay: single;
    pt: TPoint;
begin
    if mousedown then
    begin
        GetCursorPos(pt);
        deltax := (mdpos.x - pt.x) / 5;
        deltay := (mdpos.Y - pt.y) / 5;

        if isKeyDown(vk_shift) then //zoom
        begin
            deltax := -((deltax + deltay) / 20);
            cam.move(deltax);
            if (cam.Position.z < -200) or (cam.Position.z > 0) then
                cam.move(-deltax);
        end
        else
        begin //rotate
            CamH.turnangle := CamH.turnangle + deltax;
            if CamH.turnangle >= 360 then
                CamH.turnangle := CamH.turnangle - 360;
            if CamH.turnangle < 0 then
                CamH.turnangle := CamH.turnangle + 360;
            if (CamV.pitchangle + deltay < 89) and (CamV.pitchangle + deltay > -89) then
                CamV.pitchangle := CamV.pitchangle + deltay;
        end;

        if (pt.x <> mdpos.x) or (pt.y <> mdpos.y) then
            SetCursorPos(mdpos.x, mdpos.y);
    end;

    if iskeydown('w') then
    begin
        camh.Move(2 * deltatime);
    end;

    if iskeydown('s') then
    begin
        camh.Move(-2 * deltatime);
    end;

    if iskeydown('d') then
    begin
        camh.slide(2 * deltatime);
    end;

    if iskeydown('a') then
    begin
        camh.slide(-2 * deltatime);
    end;

    if iskeydown('r') then
    begin
        camh.lift(2 * deltatime);
    end;

    if iskeydown('f') then
    begin
        camh.lift(-2 * deltatime);
    end;
    glcube1.CubeWidth := aviplane.width * 1.1;
    glcube1.Cubeheight := aviplane.height * 1.1;
    glsceneviewer1.Refresh;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    AviPlane := TGLAviPlane.Create(GLScene1);
    aviplane.Material.MaterialOptions := [moNoLighting];
    aviplane.OnUpdate := AVIPlaneUpdate;
    glscene1.Objects.AddChild(AviPlane);
    aviplane.UserFrameRate := 25;
    FPSScrollBar.position := 25;
    label3.caption := '0 FPS';
    tracking:=false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
    caption := format('Viewer:%.2f FPS Current Video:%d FPS Actual Video: %.2f FPS', [glsceneviewer1.FramesPerSecond, AviPlane.CurrentFrameRate, AviPlane.TargetFrameRate]);
    glsceneviewer1.ResetPerformanceMonitor;
end;

procedure TForm1.Openavi1Click(Sender: TObject);
begin
    with TOpendialog.Create(self) do
    begin
        filter := 'Avi files (*.avi)|*.avi';
        if execute then
            AviPlane.filename := filename;
        CheckBox1Click(self);
        PosScrollBar.Min := AviPlane.FirstFrame;
        PosScrollBar.Max := AviPlane.LastFrame;
        PosScrollBar.Position := 0;
        free;
    end;
end;

procedure TForm1.Quit1Click(Sender: TObject);
begin
    application.Terminate;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
    aviplane.Quality := ScrollBar1.Position;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    //  glscene1.Objects.DeleteChildren;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
    aviplane.Rendermode := TRendermode(TRadioButton(sender).tag);
end;

procedure TForm1.FPSScrollBarChange(Sender: TObject);
begin
    aviplane.UserFrameRate := FPSScrollBar.position;
    label3.caption := format('%.d FPS', [FPSScrollBar.position]);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
    AviPlane.autoFramerate := checkbox1.checked;
    FPSScrollbar.visible := not checkbox1.checked;
    if checkbox1.checked then
        label3.caption := format('%.d FPS', [round(Aviplane.TargetFrameRate)])
    else
        label3.caption := format('%.d FPS', [FPSScrollBar.position]);
end;

procedure TForm1.PosScrollBarScroll(Sender: TObject;
    ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
    if Aviplane.FrameIndex <> ScrollPos then
        Aviplane.FrameIndex := ScrollPos;
    case ScrollCode of
        scEndScroll:
                tracking := false;
        scTrack:
                tracking := true;
    end;
end;

procedure TForm1.AVIPlaneUpdate(Sender: TObject; FrameIndex: integer);
begin
    if not tracking then
    begin
        PosScrollBar.Position := Aviplane.FrameIndex;
    end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Cam.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

end.

