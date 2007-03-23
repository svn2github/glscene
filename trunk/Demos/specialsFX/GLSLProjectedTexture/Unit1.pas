unit Unit1;

interface

uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    GLScene,
    GLObjects,
    GLMisc,
    GLTexture,
    GLWin32Viewer,
    TGA,
    GLCadencer,
    GLVectorFileObjects,
    GLFileObj,
    JPeg,
    StdCtrls,
    GLShadowVolume,
    Math,
    GLPhongShader,
    GLSLProjectedTextures,
    GLGeomObjects,
    ExtCtrls,
    GLUtils,
    GLFile3ds,
    GLFileLMTS,
    opengl1x,
    GLContext,
    vectorgeometry,
    GLUserShader,
    GLProjectedTextures;

type
    TForm1 = class(TForm)
        GLScene1: TGLScene;
        GLSceneViewer1: TGLSceneViewer;
        GLCamera1: TGLCamera;
        GLCadencer1: TGLCadencer;
        GLLightSource2: TGLLightSource;
        GLArrowLine1: TGLArrowLine;
        Timer1: TTimer;
        GLDummyCube3: TGLDummyCube;
        GLMaterialLibrary1: TGLMaterialLibrary;
    GLSLTextureEmitter1: TGLSLTextureEmitter;
    GLSLProjectedTextures1: TGLSLProjectedTextures;
    GLFreeForm1: TGLFreeForm;
    GLCube1: TGLCube;
    GLSLTextureEmitter2: TGLSLTextureEmitter;
        procedure GLCamera1CustomPerspective(const viewport: TRectangle; width,
            height, DPI: Integer; var viewPortRadius: Single);
        procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
            newTime: Double);
        procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
            WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure GLSceneViewer1MouseDown(Sender: TObject;
            Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure Timer1Timer(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        mx, my: integer;
        sdir: integer;
    public
    end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
    newTime: Double);
var
    i: integer;
    oldvis:boolean;
begin
    for i := 1 to glslProjectedTextures1.Emitters.count - 1 do
        glslProjectedTextures1.Emitters[i].Emitter.turn(deltatime * (i + 1) * 10);

    GLSceneViewer1.invalidate;
    GLArrowLine1.position.y :=  GLArrowLine1.position.y + sdir * deltatime;
    if  GLArrowLine1.position.y > 20 then
    begin
        GLArrowLine1.position.y := 20;
        sdir := -10;
    end;
    if  GLArrowLine1.position.y < 10 then
    begin
        GLArrowLine1.position.y := 10;
        sdir := 10;
    end;

end;

procedure TForm1.GLCamera1CustomPerspective(const viewport: TRectangle; width,
    height, DPI: Integer; var viewPortRadius: Single);
begin
    gluPerspective(glcamera1.FocalLength, width / height, glcamera1.NearPlaneBias, glcamera1.DepthOfView);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
    if ssLeft in Shift then
    begin
        GLCamera1.MoveAroundTarget(my - y, mx - x);
        mx := x;
        my := y;
    end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    mx := x;
    my := y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
    i: integer;
begin
    Caption := GLSceneViewer1.FramesPerSecondText();
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    i: Integer;
begin
    randomize;
    sdir := -10;
    glcamera1.CameraStyle := cscustom;

    SetCurrentDir(extractfilepath(paramstr(0))+'..\..\media\');
    glslProjectedTextures1.Material.Texture.Image.LoadFromFile('flare1.bmp');
    glslProjectedTextures1.Material.Texture.Disabled := False;
    glslProjectedTextures1.Material.Texture.TextureWrap := twNone;
    glslProjectedTextures1.Material.Texture.MinFilter := miLinear;
    glslProjectedTextures1.Material.Texture.MagFilter := maLinear;
    glslProjectedTextures1.UseLightmaps := true;
    glcube1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
    glcube1.Material.Texture.Disabled := False;


    glFreeform1.LoadFromFile('groundtest.lmts');
    for i := 0 to GLMaterialLibrary1.Materials.Count - 1 do
        GLMaterialLibrary1.Materials.Items[i].Material.MaterialOptions := [moNoLighting];

end;

end.

