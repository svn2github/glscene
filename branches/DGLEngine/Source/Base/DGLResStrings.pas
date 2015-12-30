//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{: DGLResStrings<p>

   Reference all needed internal strings for messages.
   Pleas keep this unit updated if your unit use ResourceString.<p>

 <b>History : </b><font size=-1><ul>
      <li>21/12/15 - JD - Created
   </ul></font><p>
}
unit DGLResStrings;

interface

resourcestring
  // General
  glsDot = '.';
  glsError = 'Error!';
  glsErrorEx = 'Error: ';

  // CONTEXT
  cCannotAlterAnActiveContext = 'Cannot alter an active context';
  cInvalidContextRegistration = 'Invalid context registration';
  cInvalidNotificationRemoval = 'Invalid notification removal';
  cContextAlreadyCreated = 'Context already created';
  cContextNotCreated = 'Context not created';
  cUnbalancedContexActivations = 'Unbalanced context activations';
  cIncompatibleContexts = 'Incompatible contexts';
  cDeleteContextFailed = 'Delete context failed';
  cContextActivationFailed = 'Context activation failed: %X, %s';
  cContextDeactivationFailed = 'Context deactivation failed';
  cUnableToCreateLegacyContext = 'Unable to create legacy context';
  cNoActiveRC = 'No active rendering context';
  glsFailedToShare = 'DoCreateContext - Failed to share contexts';
  rstrOpenGLError = 'OpenGL error - %s';
  cBackwardContextFailed = 'Can not create backward compatible context: #%X, %s';
  cFailHWRC = 'Unable to create rendering context with hardware acceleration - down to software';
  glsTmpRC_Created = 'Temporary rendering context created';
  glsDriverNotSupportFRC = 'Driver not support creating of forward context';
  glsDriverNotSupportOESRC = 'Driver not support creating of OpenGL ES 2.0 context';
  glsDriverNotSupportDebugRC = 'Driver not support creating of debug context';
  glsOESvsForwardRC = 'OpenGL ES 2.0 context incompatible with Forward context - flag ignored';
  glsFRC_created = 'Forward core context seccussfuly created';
  glsOESRC_created = 'OpenGL ES 2.0 context seccussfuly created';
  glsPBufferRC_created = 'Backward compatible core PBuffer context successfully created';

  // common messages
  glsUnknownArchive = '%s : unknown archive version %d';
  glsOutOfMemory = 'Fatal: Out of memory';
  glsFileNotFound = 'File %s not found';
  glsFailedOpenFile = 'Could not open file: %s';
  glsFailedOpenFileFromCurrentDir = 'Could not open file: %s'#13#10'(Current directory is %s)';
  glsNoDescriptionAvailable = 'No description available';
  glsUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  glsUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support ' + 'unit to your uses? (%s?)';
  glsMissingResource = 'Missing application resource: %s: %s';
  glsIncompatibleTypes = 'Incompatible types!';
  glsUnknownType = 'Unknown type!';
  glsUnsupportedType = 'Unsupported type!';
  glsUnknownParam = 'Unknown %s "%s" for "%s" or program not in use';
  glsCantConvertImg = '%s: can''t convert image to RGBA8 format';
  cInvalidFileSignature = 'Invalid file signature';
  cUnknownArchiveVersion = ' : unknown archive version ';
  cBrokenObjectListArchive = 'Broken ObjectList archive';
  cListIndexError = 'Invalid list index';
  CsVectorHelp  = 'If you are getting assertions here, consider using the SetPoint procedure';
  CsPointHelp   = 'If you are getting assertions here, consider using the SetVector procedure';
  CsPoint2DHelp = 'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';


  // RenderManager
  glsCanNotRebuild = 'Static object can not be rebuilded';
  glsBadAttrCombination = 'Single and list attributes can not be combined';
  glsWrongAttrType = 'An attribute was used with different type than previously or bad list size';
  glsWrongCallBegin = 'This function cannot be called again before EndPart has been called.';
  glsWrongCallEnd = 'This function must be called after BeginObject ... EndPrimitive.';
  glsWrongCallEmit = 'This function must be called after BeginPrimitive ... EndPrimitive.';
  glsNoShader = 'Currently no shader is bound.';
  glsInvalidNumberOfVertex = 'The number of primitives to render is invalid. You need to construct complete primitives.';
  glsWrongCallBeginPrim = 'This function cannot be called recursively or before BeginObject.';
  glsInvalidPrimType = 'Invalid primitive type.';
  glsWrongCallEndPrim = 'Before calling this function Begin must have been called.';
  glsTooMachDiffPrim = 'Too mach different primitive types in one object.';
  glsAlreadyDefined = 'Geometric data of the object already identified.';
  glsNoActiveRC = 'Using the VBO manager without the active rendering context.';

  // SceneViewer
  glsNoRenderingContext = 'Could not create a rendering context';
  glsWrongVersion       = 'Need at least OpenGL version 3.3';
  glsTooManyLights      = 'Too many lights in the scene';
  glsDisplayList        = 'Failed to create a new display list for object ''%s''';
  glsWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  glsWrongPrinter       = 'Couldn''t render to printer';
  glsAlreadyRendering   = 'Already rendering';
  glsSceneViewerNotDefined = 'SceneViewer not defined!';

  // GLCadencer
  glsCadencerNotDefined   = 'Cadencer not defined!';
  glsCadencerNotDefinedEx = 'Cadencer not defined for  the ''%s'' component';

  // Shaders
  glsShaderNeedsAtLeastOneLightSource   = 'This shader needs at least one LightSource!';
  glsShaderNeedsAtLeastOneLightSourceEx = 'Shader ''%s'' needs at least one LightSource!';

  // GLTree
  glsSceneRoot  = 'Scene root';
  glsObjectRoot = 'Scene objects';
  glsCameraRoot = 'Cameras';
  glsCamera     = 'Camera';

  // object categories
  glsOCBasicGeometry = 'Basic geometry';
  glsOCAdvancedGeometry = 'Advanced geometry';
  glsOCMeshObjects = 'Mesh objects';
  glsOCParticleSystems = 'Particle systems';
  glsOCEnvironmentObjects = 'Environment objects';
  glsOCSpecialObjects = 'Special objects';
  glsOCGraphPlottingObjects = 'Graph-plotting objects';
  glsOCDoodad = 'Doodad objects';
  glsOCHUDObjects = 'HUD objects';
  glsOCGuiObjects = 'GUI objects';
  glsOCProxyObjects = 'Proxy objects';
  glsOCExperimental = 'Experimental objects';

  // GLTexture
  glsImageInvalid = 'Could not load texture, image is invalid';
  glsNoNewTexture = 'Could not get new texture name';

  // GLMaterials
  glsMatLibNotDefined = 'Material Library not defined!';
  glsMaterialNotFoundInMatlib = 'Material "%s" not found in current Material Library!';

  // GLShaders
  glsShaderLibNotDefined = 'Shader Library not defined!';
  glsShaderNotFoundInMatlib = 'Shader "%s" not found in current Shader Library!';



implementation

end.
