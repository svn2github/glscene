//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLStrings<p>

	String constants that are used in many GLScene units<p>

	<b>History :</b><font size=-1><ul>
      23/02/07 - DaStr - Added glsDot, glsUnsupportedType, glsUncompatibleTypes,
                         glsUnknownType, glsShaderNeedsAtLeastOneLightSource(Ex),
                         glsCadencerNotDefined(Ex), glsSceneViewerNotDefined
      16/02/07 - DaStr - Added glsOCProxyObjects, glsError, glsErrorEx,
                         glsMatLibNotDefined, glsMaterialNotFoundInMatlib(Ex)
      26/08/02 - EG - Added missing header, added glsUnknownExtension
	</ul></font>
}
unit GLStrings;

interface

resourcestring
  // General
  glsDot = '.';
  glsError   = 'Error!';
  glsErrorEx = 'Error: ';

  // SceneViewer
  glsNoRenderingContext = 'Could not create a rendering context';
  glsWrongVersion       = 'Need at least OpenGL version 1.1';
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

  // GLTexture
  glsImageInvalid = 'Could not load texture, image is invalid';
  glsNoNewTexture = 'Could not get new texture name';

  // GLMaterials
  glsMatLibNotDefined = 'Material Library not defined!';
  glsMaterialNotFoundInMatlib = 'Material not found in current Material Library!';
  glsMaterialNotFoundInMatlibEx = 'Material "%s" not found in current Material Library!';

  // GLObjects
  glsSphereTopBottom = 'The top angle must be higher than the bottom angle';
  glsSphereStartStop = 'The start angle must be smaller than then stop angle';
  glsMaterialNotFound = 'Loading failed: could not find material %s';
  glsInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';

  // common messages
  glsOutOfMemory = 'Fatal: Out of memory';
  glsFailedOpenFile = 'Could not open file: %s';
  glsFailedOpenFileFromCurrentDir = 'Could not open file: %s'#13#10'(Current directory is %s)';
  glsNoDescriptionAvailable = 'No description available';
  glsUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  glsUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;

  glsIncompatibleTypes = 'Incompatible types!';
  glsUnknownType       = 'Unknown type!';
  glsUnsupportedType   = 'Unsupported type!';

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

  // VBO and Shader managers messages
  glsCanNotRebuild =
    'Static object can not be rebuilded';
  glsBadAttrCombination =
    'Single and list attributes can not be combined';
  glsWrongAttrType =
    'An attribute was used with different type than previously or bad list size';
  glsWrongCallBegin =
    'This function cannot be called before EndObject has been called.';
  glsWrongCallEnd =
    'This function must be called between BeginObject ... EndPrimitive.';
  glsWrongCallEmit =
    'This function must be called between BeginPrimitive ... EndPrimitive.';
  glsNoShader =
    'Currently no shader is bound.';
  glsInvalidNumberOfVertex =
    'The number of primitives to render is invalid. You need to construct complete primitives.';
  glsWrongCallBeginPrim =
    'This function cannot be called after BeginObject.';
  glsInvalidPrimType =
    'Invalid primitive type.';
  glsWrongCallEndPrim =
    'Before calling this function Begin must have been called.';
  glsTooMachDiffPrim =
    'Too mach different primitive types in one object. Necessary to increase the constant GLVBOM_MAX_DIFFERENT_PRIMITIVES';
  glsAlreadyDefined =
    'Geometric data of the object already identified.';
  glsUnknownAttrib =
    'Used not declared attribute. Declare attribute between BeginObject ... BeginPrimitive.';
  glsErrorBuildModel =
    'Error occurred when model was builded';
  glsDoMakeAdjFail =
    'Unable to convert usualy primitives to primitives whith adjacency';
  glsOutOfMaxAttrib =
    'Necessary to increase the constant GLS_VERTEX_ATTR_NUM';
  glsOutOfMaxShader =
    'Necessary to increase the constant GLS_MAX_SHADER_PROGRAM';

  // Resources
  glsDEFAULTMATERIALNAME = 'DEFAULTMATERIAL';
  glsDIFFUSEMAP = 'DEFAULTDIFFUSEMAP';
  glsNORMALMAP = 'DEFAULTNORMALMAP';
  glsLauncherData = 'LAUNCHERDATA';
  glsMaterialManagerData = 'MATERIALMANAGERDATA';

implementation

end.

