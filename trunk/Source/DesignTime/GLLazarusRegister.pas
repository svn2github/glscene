unit GLLazarusRegister;

interface

uses
   Classes,
   GLObjectManager, ComponentEditors, PropEdits, LResources;

type

   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class (TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;

         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

implementation

uses
   SysUtils, Graphics,
   // GLScene units
   VectorGeometry, GLScene, GLViewer, {$IFDEF MSWINDOWS}{GLWin32FullScreenViewer,}{$ENDIF}
   GLStrings, GLCoordinates, GLColor, GLTexture, GLMaterial,
   GLCadencer, GLCrossPlatform,
   // GLScene - basic geometry
   GLObjects, GLGeomObjects, GLPolyhedron,
   // GLScene - advanced geometry
   GLAnimatedSprite, GLExtrusion, GLMultiPolygon,
   // GLScene - mesh
   GLVectorFileObjects, GLMesh, GLTilePlane, GLPortal,
   // GLScene - terrain
   GLTerrainRenderer, GLHeightData, GLHeightTileFileHDS, GLBumpmapHDS, GLPerlin,
   GLTexturedHDS, GLAsyncHDS, GLShadowHDS,
   // GLScene - graph plotting
   GLBitmapFont, GLGraph, GLWindowsFont, {$IFDEF MSWINDOWS}GLWideBitmapFont,{$ENDIF}
   // GLScene - particles
   GLParticles, GLParticleFX, GLPerlinPFX, GLLinePFX, GLFireFX, GLThorFX,
   GLEParticleMasksManager,
   // GLScene - environment
   GLSkydome, GLSkyBox, GLAtmosphere,
   // GLScene - hud
   GLHUDObjects, GLGameMenu, {$IFDEF MSWINDOWS}GLConsole,{$ENDIF}
   // GLScene - gui
   GLWindows, GLGui,
   // GLScene - special
   GLLensFlare, GLTexLensFlare, GLMirror, GLShadowPlane, GLShadowVolume,
   GLzBuffer, GLSLProjectedTextures, GLProjectedTextures, GLBlur,
   {$IFDEF MSWINDOWS} GLSpaceText, {$ENDIF}
   GLTrail, GLPostEffects,
   // GLScene - doodad
   GLTeapot, GLTree, GLWaterPlane,
   // GLScene - proxy
   GLProxyObjects, GLMultiProxy, GLMaterialMultiProxy,
   // GLScene - shaders
   GLTexCombineShader, GLPhongShader, GLUserShader, GLSLShader,
   GLHiddenLineShader, GLCelShader, GLOutlineShader, GLMultiMaterialShader,
   GLBumpShader, GLSLDiffuseSpecularShader, GLSLBumpShader, GLSLPostBlurShader,
   GLAsmShader, GLShaderCombiner, GLTextureSharingShader,
   // GLScene - other
   GLImposter, GLFeedback, GLCollision, GLScriptBase, AsyncTimer, GLDCE,
   GLFPSMovement, GLMaterialScript, GLNavigator, GLSmoothNavigator,
   GLTimeEventsMgr, ApplicationFileIO, GLVfsPAK, GLSimpleNavigation,
   GLCameraController, {$IFDEF MSWINDOWS}GLGizmo, GLSound, GLSMWaveOut,
   {GLAVIRecorder,} Joystick, ScreenSaver,{$ENDIF}

   // Property editor forms
   GLLazarusSceneEdit, FVectorEditor;

var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create(nil);
   Result:=vObjectManager;
end;

//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb
//
procedure TGLSceneViewerEditor.ExecuteVerb(Index : Integer);
var
  source : TGLSceneViewer;
begin
  source:=Component as TGLSceneViewer;
  case Index of
    0 : source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//
function TGLSceneViewerEditor.GetVerb(Index : Integer) : String;
begin
  case Index of
    0 : Result:='Show context info';
  end;
end;

// GetVerbCount
//
function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;


//----------------- TGLSceneEditor ---------------------------------------------

// Edit
//
procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
      Show;
   end;
end;

// ExecuteVerb
//
procedure TGLSceneEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLSceneEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Scene Editor';
   end;
end;

// GetVerbCount
//
function TGLSceneEditor.GetVerbCount: Integer;
begin
   Result:=1;
end;


//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
   x:=glc.x;
   y:=glc.y;
   z:=glc.z;
   if VectorEditorForm.Execute(x, y, z) then begin
      glc.AsVector:=VectorMake(x, y, z);
      Modified;
   end;
end;


procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene,
                       TGLSceneViewer, TGLMemoryViewer,
                       TGLMaterialLibrary,
                       TGLCadencer,
                       TGLGuiLayout,
                       TGLBitmapFont, TGLWindowsBitmapFont, TGLStoredBitmapFont,
                       TGLScriptLibrary
                       {$ifdef MSWINDOWS}
                       ,TGLWideBitmapFont, TGLSoundLibrary, TGLSMWaveOut{,
                       TGLFullScreenViewer}
                       {$endif}
                      ]);

   RegisterComponents('GLScene PFX',
                      [
                       TGLCustomPFXManager,
                       TGLPolygonPFXManager, TGLPointLightPFXManager,
                       TGLCustomSpritePFXManager,
                       TGLPerlinPFXManager, TGLLinePFXManager,
                       TGLFireFXManager, TGLThorFXManager,
                       TGLEParticleMasksManager
                      ]);

   RegisterComponents('GLScene Utils',
                      [TAsyncTimer, TGLStaticImposterBuilder,
                       TCollisionManager, TGLAnimationControler,
                       TGLDCEManager, TGLFPSMovementManager,
                       TGLMaterialScripter, TGLUserInterface, TGLNavigator,
                       TGLSmoothNavigator, TGLSmoothUserInterface,
                       TGLTimeEventsMGR, TApplicationFileIO, TGLVfsPAK,
                       TGLSimpleNavigation, TGLCameraController
                       {$IFDEF MSWINDOWS}
                       {,TAVIRecorder}, TGLGizmo, TJoystick, TScreenSaver
                       {$ENDIF}
                      ]);

   RegisterComponents('GLScene Terrain',
                      [TGLBitmapHDS, TGLCustomHDS, TGLHeightTileFileHDS,
                       TGLBumpmapHDS, TGLPerlinHDS, TGLTexturedHDS, TGLAsyncHDS,
                       TGLShadowHDS
                      ]);

   RegisterComponents('GLScene Shaders',
                      [ TGLTexCombineShader, TGLPhongShader, TGLUserShader,
                        TGLHiddenLineShader, TGLCelShader, TGLOutlineShader,
                        TGLMultiMaterialShader, TGLBumpShader,
                        TGLSLShader, TGLSLDiffuseSpecularShader, TGLSLBumpShader,
                        TGLAsmShader,TGLShaderCombiner,TGLTextureSharingShader,
                        TGLSLPostBlurShader
                      ]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);

   RegisterClasses([TGLCoordinates]);

   RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);
end;

initialization

   {$I GLSceneVCL.lrs}

   GLColor.vUseDefaultColorSets:=True;
   GLCoordinates.vUseDefaultCoordinateSets:=True;

   //ReadVideoModes;

   with ObjectManager do begin
      RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
      RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
      RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

      // Basic Geometry
      RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCapsule, 'Capsule', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry, HInstance);

      //Advanced geometry
      RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

      //Mesh objects
      RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects, HInstance);

      //Graph-plotting objects
      RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects, HInstance);

      //Particle systems
      RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems, HInstance);
      RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', glsOCParticleSystems, HInstance);

      //Environment objects
      RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects, HInstance);

      // HUD objects.
      RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLResolutionIndependantHUDText, 'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
      {$ifdef MSWINDOWS}
      RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects, HInstance);
      {$ENDIF}

      // GUI objects.
      RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects, HInstance);

      //Special objects
      RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects, HInstance);
      {$ifdef MSWINDOWS}
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad, HInstance);
      {$endif}
      RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder', glsOCSpecialObjects, HInstance);

      // Doodad objects.
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

      // Proxy objects.
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy', glsOCProxyObjects, HInstance);

      // Other objects.
      RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
      RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
      RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
      RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '', HInstance);
   end;

finalization

   ObjectManager.Free;

end.
