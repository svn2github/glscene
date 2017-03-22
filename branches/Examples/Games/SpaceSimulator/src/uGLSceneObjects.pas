{Набор синглтонов, преимущественно объектов формы

  Освобождаются при непосредственном уничтожении формы, либо классом TdfGame}
unit uGLSceneObjects;

interface

uses
  GLScene, GLWin32Viewer, GLCadencer, GLMaterial, GLParticleFX, GLObjects;

const
  C_ENGINEFX_COUNT = 1;
  C_BOOMFX_COUNT   = 2;

var
  dfGLSceneObjects: record
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    MatLibrary: TGLMaterialLibrary;
    EnginesFX: array[0..C_ENGINEFX_COUNT - 1] of TGLParticleFXManager;
    BoomFX: array[0..C_BOOMFX_COUNT - 1] of TGLParticleFXManager;
//    HUDDummy: TGLDummyCube;
  end;

implementation

end.
