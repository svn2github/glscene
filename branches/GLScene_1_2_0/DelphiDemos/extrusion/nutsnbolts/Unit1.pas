{ : A Nut and Bolt sample, 100% defined at design-time.<p>

  Both use two revolution solids, one for the head/pans, another for the thread.
  Additionnally, a cylinder and an annulus help finish up by providing the
  shafts.<p>

  The head/pans are defined by simple rotated on 360° in 6 steps, the threads
  are a simpler curve : two segments in triangular shape, that are rotated and
  extruded in the y axis.<p>

  Smoothing is used in the head to make smoother edges (along with a bevel in
  the curve), while the threads are unsmoothed, to get a sharp edge effect.
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,

  GLScene_SimpleNavigation,
  GLScene_Core,
  GLScene_Objects,
  GLScene_ObjectsEx,
  GLScene_Objects_Extrusion,
  GLScene_Base_Coordinates,
  GLScene_Platform,
  GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    RSBoltThreads: TGLRevolutionSolid;
    CYBoltShaft: TGLCylinder;
    RSBoltHead: TGLRevolutionSolid;
    Bolt: TGLDummyCube;
    Nut: TGLDummyCube;
    RSNutThreads: TGLRevolutionSolid;
    RSNutPans: TGLRevolutionSolid;
    Annulus1: TGLAnnulus;
    GLSimpleNavigation1: TGLSimpleNavigation;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
