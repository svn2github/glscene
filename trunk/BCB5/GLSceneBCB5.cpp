//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLSceneBCB5.res");
USERES("..\Source\DesignTime\nonGLSceneVCL.dcr");
USERES("..\Source\DesignTime\GLSceneObjects.dcr");
USERES("..\Source\DesignTime\GLSceneVCL.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEUNIT("..\Source\Base\PersistentClasses.pas");
USEUNIT("..\Source\Base\ApplicationFileIO.pas");
USEUNIT("..\Source\Base\SpatialPartitioning.pas");
USEUNIT("..\Source\Base\AsyncTimer.pas");
USEUNIT("..\Source\Platform\GLCrossPlatform.pas");
USEUNIT("..\Source\Base\CurvesAndSurfaces.pas");
USEUNIT("..\Source\Base\GeometryBB.pas");
USEUNIT("..\Source\Base\GLTextureCombiners.pas");
USEUNIT("..\Source\Base\OpenGL1x.pas");
USEUNIT("..\Source\GLContext.pas");
USEUNIT("..\Source\Base\HeightTileFile.pas");
USEUNIT("..\Source\Base\MeshUtils.pas");
USEUNIT("..\Source\Base\Joystick.pas");
USEUNIT("..\Source\Base\Keyboard.pas");
USEUNIT("..\Source\Base\Octree.pas");
USEUNIT("..\Source\Base\Polynomials.pas");
USEUNIT("..\Source\Base\ScreenSaver.pas");
USEUNIT("..\Source\Base\VectorTypes.pas");
USEUNIT("..\Source\Base\Spline.pas");
USEUNIT("..\Source\Base\VectorGeometry.pas");
USEUNIT("..\Source\Base\VectorLists.pas");
USEUNIT("..\Source\Base\VerletClasses.pas");
USEUNIT("..\Source\Base\XCollection.pas");
USEUNIT("..\Source\Base\XOpenGL.pas");
USEUNIT("..\Source\GLState.pas");
USEUNIT("..\Source\GLMisc.pas");
USEUNIT("..\Source\GLScreen.pas");
USEUNIT("..\Source\GLTexture.pas");
USEUNIT("..\Source\GLScene.pas");
USEUNIT("..\Source\GLMirror.pas");
USEUNIT("..\Source\GLGraphics.pas");
USEUNIT("..\Source\VideoAPIs\vfw.pas");
USEUNIT("..\Source\Platform\GLWin32Context.pas");
USEUNIT("..\Source\Platform\GLWin32Viewer.pas");
USEUNIT("..\Source\Platform\GLWin32FullScreenViewer.pas");
USEUNIT("..\Source\GLAVIRecorder.pas");
USEUNIT("..\Source\GLBaseMeshSilhouette.pas");
USEUNIT("..\Source\GLBehaviours.pas");
USEUNIT("..\Source\GLBitmapFont.pas");
USEUNIT("..\Source\GLBSP.pas");
USEUNIT("..\Source\GLExplosionFx.pas");
USEUNIT("..\Source\GLExtrusion.pas");
USEUNIT("..\Source\GLFireFX.pas");
USEUNIT("..\Source\GLGeomObjects.pas");
USEUNIT("..\Source\GLGraph.pas");
USEUNIT("..\Source\GLHeightData.pas");
USEUNIT("..\Source\GLHeightTileFileHDS.pas");
USEUNIT("..\Source\GLHUDObjects.pas");
USEUNIT("..\Source\GLLensFlare.pas");
USEUNIT("..\Source\GLMeshCSG.pas");
USEUNIT("..\Source\GLMovement.pas");
USEUNIT("..\Source\GLMultiProxy.pas");
USEUNIT("..\Source\GLNavigator.pas");
USEUNIT("..\Source\GLParametricSurfaces.pas");
USEUNIT("..\Source\GLParticleFX.pas");
USEUNIT("..\Source\GLParticles.pas");
USEUNIT("..\Source\GLPolyhedron.pas");
USEUNIT("..\Source\GLPortal.pas");
USEUNIT("..\Source\GLProcTextures.pas");
USEUNIT("..\Source\GLProxyObjects.pas");
USEUNIT("..\Source\GLROAMPatch.pas");
USEUNIT("..\Source\GLShadowPlane.pas");
USEUNIT("..\Source\GLShadowVolume.pas");
USEUNIT("..\Source\GLSkyBox.pas");
USEUNIT("..\Source\GLSkydome.pas");
USEUNIT("..\Source\FileFormats\GLStarRecord.pas");
USEUNIT("..\Source\GLSound.pas");
USEUNIT("..\Source\GLSoundFileObjects.pas");
USEUNIT("..\Source\GLSpaceText.pas");
USEUNIT("..\Source\GLTeapot.pas");
USEUNIT("..\Source\GLTerrainRenderer.pas");
USEUNIT("..\Source\GLThorFX.pas");
USEUNIT("..\Source\GLTimeEventsMgr.pas");
USEUNIT("..\Source\GLTree.pas");
USEUNIT("..\Source\GLVerletClothify.pas");
USEUNIT("..\Source\GLVerletSkeletonColliders.pas");
USEUNIT("..\Source\GLWindowsFont.pas");
USEUNIT("..\Source\GLzBuffer.pas");
USEUNIT("..\Source\GLGui.pas");
USEUNIT("..\Source\GLCollision.pas");
USEUNIT("..\Source\GLWindows.pas");
USEUNIT("..\Source\FileFormats\Const3DS.pas");
USEUNIT("..\Source\FileFormats\File3DS.pas");
USEUNIT("..\Source\FileFormats\FileMD2.pas");
USEUNIT("..\Source\FileFormats\FileMD3.pas");
USEUNIT("..\Source\FileFormats\FileNMF.pas");
USEUNIT("..\Source\FileFormats\FileOCT.pas");
USEUNIT("..\Source\FileFormats\GLFile3DS.pas");
USEUNIT("..\Source\FileFormats\GLFileGTS.pas");
USEUNIT("..\Source\FileFormats\GLFileMD2.pas");
USEUNIT("..\Source\FileFormats\GLFileMD3.pas");
USEUNIT("..\Source\FileFormats\GLFileMS3D.pas");
USEUNIT("..\Source\FileFormats\GLFileTIN.pas");
USEUNIT("..\Source\FileFormats\GLFileOBJ.pas");
USEUNIT("..\Source\FileFormats\GLFileOCT.pas");
USEUNIT("..\Source\FileFormats\GLFilePLY.pas");
USEUNIT("..\Source\FileFormats\GLFileQ3BSP.pas");
USEUNIT("..\Source\FileFormats\GLFileSMD.pas");
USEUNIT("..\Source\FileFormats\GLFileSTL.pas");
USEUNIT("..\Source\FileFormats\TGA.pas");
USEUNIT("..\Source\FileFormats\Q3MD3.pas");
USEUNIT("..\Source\GLUtils.pas");
USEUNIT("..\Source\GLStrings.pas");
USEUNIT("..\Source\GLCadencer.pas");
USEUNIT("..\Source\GLMesh.pas");
USEUNIT("..\Source\GLMultiPolygon.pas");
USEUNIT("..\Source\GLObjects.pas");
USEUNIT("..\Source\GLSilhouette.pas");
USEUNIT("..\Source\GLVectorFileObjects.pas");
USEUNIT("..\Source\Base\GLCanvas.pas");
USEUNIT("..\Source\Base\RegisterXCollection.pas");
USEUNIT("..\Source\FileFormats\GLFileNMF.pas");
USEUNIT("..\Source\FileFormats\GLFileNurbs.pas");
USEUNIT("..\Source\FileFormats\Q3BSP.pas");
USEUNIT("..\Source\FileFormats\Utils3DS.pas");
USEUNIT("..\Source\FileFormats\Types3DS.pas");
USEUNIT("..\Source\FileFormats\TypesMD2.pas");
USEUNIT("..\Source\FileFormats\TypesMS3D.pas");
USEUNIT("..\Source\FileFormats\TypesSTL.pas");
USEUNIT("..\Source\DesignTime\GLTextureImageEditors.pas");
USEUNIT("..\Source\DesignTime\GLSceneRegister.pas");
USEUNIT("..\Source\Shaders\GLHiddenLineShader.pas");
USEUNIT("..\Source\Shaders\GLMultiMaterialShader.pas");
USEUNIT("..\Source\Shaders\GLOutlineShader.pas");
USEUNIT("..\Source\Shaders\GLTexCombineShader.pas");
USEUNIT("..\Source\Shaders\GLUserShader.pas");
USEUNIT("..\Source\PlugIn\PlugInIntf.pas");
USEUNIT("..\Source\PlugIn\PlugInManager.pas");
USEUNIT("..\Source\GLTilePlane.pas");
USEUNIT("..\Source\GLBumpmapHDS.pas");
USEUNIT("..\Source\GLWaterPlane.pas");
USEUNIT("..\Source\Base\BumpMapping.pas");
USEUNIT("..\Source\FileFormats\GLVfsPAK.pas");
USEUNIT("..\Source\GLMeshOptimizer.pas");
USEUNIT("..\Source\FileFormats\TypesMDC.pas");
USEUNIT("..\Source\FileFormats\GLFileMDC.pas");
USEUNIT("..\Source\Shaders\GLCelShader.pas");
USEUNIT("..\Source\GLFPSMovement.pas");
USEUNIT("..\Source\Shaders\GLBumpShader.pas");
USEUNIT("..\Source\Base\PerlinNoise.pas");
USEUNIT("..\Source\GLTexLensFlare.pas");
USEUNIT("..\Source\GLPerlinPFX.pas");
USEUNIT("..\Source\GLImposter.pas");
USEUNIT("..\Source\GLFeedback.pas");
USEUNIT("..\Source\GLAnimatedSprite.pas");
USEUNIT("..\Source\Base\VerletHairClasses.pas");
USEUNIT("..\Source\GLVerletClasses.pas");
USEUNIT("..\Source\GLGLUTesselation.pas");
USEUNIT("..\Source\GLMeshBuilder.pas");
USEUNIT("..\Source\FileFormats\DDS.pas");
USEUNIT("..\Source\FileFormats\DXTC.pas");
USEUNIT("..\Source\GLMaterialScript.pas");
USEUNIT("..\Source\GLDCEMisc.pas");
USEUNIT("..\Source\GLEllipseCollision.pas");
USEUNIT("..\Source\GLDCE.pas");
USEUNIT("..\Source\GLProjectedTextures.pas");
USEUNIT("..\Source\GLBlur.pas");
USEUNIT("..\Source\GLPerlinBase.pas");
USEUNIT("..\Source\GLPerlin.pas");
USEUNIT("..\Source\Base\ARBProgram.pas");
USEUNIT("..\Source\Shaders\GLPhongShader.pas");
USEUNIT("..\Source\GLTrail.pas");
USEUNIT("..\Source\GLScriptBase.pas");
USEUNIT("..\Source\FileFormats\GLFileMD5.pas");
USEUNIT("..\Source\GLGameMenu.pas");
USEUNIT("..\Source\Base\PictureRegisteredFormats.pas");
USEUNIT("..\Source\GLSpatialPartitioning.pas");
USEFORMNS("..\Source\DesignTime\FRColorEditor.pas", Frcoloreditor, RColorEditor); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRTrackBarEdit.pas", Frtrackbaredit, RTrackBarEdit); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRFaceEditor.pas", Frfaceeditor, RFaceEditor); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FRTextureEdit.pas", Frtextureedit, RTextureEdit); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FVectorEditor.pas", Fvectoreditor, VectorEditorForm);
USEFORMNS("..\Source\DesignTime\GLSceneEdit.pas", Glsceneedit, GLSceneEditorForm);
USEFORMNS("..\Source\DesignTime\Info.pas", Info, InfoForm);
USEFORMNS("..\Source\PlugIn\PlugInManagerPropEditor.pas", Pluginmanagerpropeditor, PlugInManagerPropForm);
USEFORMNS("..\Source\DesignTime\FXCollectionEditor.pas", Fxcollectioneditor, XCollectionEditor);
USEFORMNS("..\Source\DesignTime\FRMaterialPreview.pas", Frmaterialpreview, RMaterialPreview); /* TFrame: File Type */
USEFORMNS("..\Source\DesignTime\FMaterialEditorForm.pas", Fmaterialeditorform, MaterialEditorForm);
USEFORMNS("..\Source\DesignTime\FLibMaterialPicker.pas", Flibmaterialpicker, LibMaterialPicker);
USEFORMNS("..\Source\DesignTime\GuiSkinEditorFormUnit.pas", Guiskineditorformunit, GUISkinEditor);
USEUNIT("..\Source\GLLinePFX.pas");
USEUNIT("..\Source\FileFormats\GLFileVRML.pas");
USEUNIT("..\Source\FileFormats\VRMLParser.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
