{: Scene-wide Particles FX bench.<p>

   Originally planned for the specials FX, but become a bench due to lack of
   time to improve graphics ;)<br>
   This is quite a brute-force situation for the Particles FX Renderer, two
   systems are present (Red an Blue) but Red contains the bulk of the particles.<p>

   Benchmark results (default win size, "Inferno" mode, ie. approx 7000 particles):<p>

   CPU               Graphics          Colors      FPS         Sort Time

   TBird 1.1GHz      GeForce2 Pro      32 bits     98.5        2.60 msec
   Duron 800MHz      TNT2 M64          32 bits     16.7        4.01 msec
   --- 23/01/02 - ZWrite=False in the PFX Renderer, minor optims
   TBird 1.1GHz      GeForce2 Pro      32 bits     91.7        2.86 msec
   Duron 800MHz      TNT2 M64          32 bits     12.2        4.45 msec
   --- 20/01/02 - Optimized PFX (sort) and TGLPolygonPFXManager (rendering)
   TBird 1.1Ghz      GeForce2 Pro      32 bits     65.5        3.66 msec
   Duron 800MHz      Voodoo3 NT4       16 bits      7.4        5.52 msec
   --- 09/09/01 - Created Benchmark

}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLParticleFX, GLCadencer, ExtCtrls,
  GLBehaviours, StdCtrls, GLWin32Viewer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DCVolcano: TDummyCube;
    PFXVolcano: TGLPolygonPFXManager;
    GLCadencer1: TGLCadencer;
    PFXRenderer: TGLParticleFXRenderer;
    Timer1: TTimer;
    Sphere1: TSphere;
    GLLightSource1: TGLLightSource;
    PFXBlue: TGLPolygonPFXManager;
    DCCamera: TDummyCube;
    RadioGroup1: TRadioGroup;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS - %3d Particles - Depth Sort: %.2f msec',
                   [GLSceneViewer1.FramesPerSecond,
                    PFXVolcano.Particles.ItemCount+PFXBlue.Particles.ItemCount,
                    PFXRenderer.LastSortTime]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
   source : TGLSourcePFXEffect;
begin
   source:=GetOrCreateSourcePFX(DCVolcano);
   case RadioGroup1.ItemIndex of
      0 : source.ParticleInterval:=0.1;
      1 : source.ParticleInterval:=0.05;
      2 : source.ParticleInterval:=0.02;
      3 : source.ParticleInterval:=0.01;
      4 : source.ParticleInterval:=0.005;
      5 : source.ParticleInterval:=0.001;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   RadioGroup1Click(Self);
end;

end.
 