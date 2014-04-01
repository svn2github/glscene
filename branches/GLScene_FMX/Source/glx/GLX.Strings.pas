//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLX.Strings<p>

	String constants that are used in many GLX.Scene units<p>

	<b>History :</b><font size=-1><ul>
      <li>01/04/14 - PW - Updated in alphabetic order and added new glsStrings
      <li>16/09/10 - YP - Added glsUnknownParam
      <li>23/02/07 - DaStr - Added glsDot, glsUnsupportedType, glsUncompatibleTypes,
                         glsUnknownType, glsShaderNeedsAtLeastOneLightSource(Ex),
                         glsCadencerNotDefined(Ex), glsSceneViewerNotDefined
      <li>16/02/07 - DaStr - Added glsOCProxyObjects, glsError, glsErrorEx,
                         glsMatLibNotDefined, glsMaterialNotFoundInMatlib(Ex)
      <li>26/08/02 - EG - Added missing header, added glsUnknownExtension
	</ul></font>
}
unit GLX.Strings;

interface

resourcestring
  // General
  glsDot = '.';
  glsError   = 'Error!';
  glsErrorEx = 'Error: ';

  // Common messages
  glsComputing = 'Computing';
  glsFileNotFound = 'File %s not found';
  glsFailedOpenFile = 'Could not open file: %s';
  glsFailedOpenFileFromCurrentDir = 'Could not open file: %s'#13#10'(Current directory is %s)';
  glsIncompatibleTypes = 'Incompatible types!';
  glsInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';
  glsMissingResource = 'Missing application resource: %s: %s';
  glsNoDescriptionAvailable = 'No description available';
  glsSphereTopBottom = 'The top angle must be higher than the bottom angle';
  glsSphereStartStop = 'The start angle must be smaller than then stop angle';
  glsOutOfMemory = 'Fatal: Out of memory';
  glsUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  glsUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;
  glsUnknownArchive = '%s : unknown archive version %d';
  glsUnknownType       = 'Unknown type!';
  glsUnsupportedType   = 'Unsupported type!';
  glsUnknownParam =  'Unknown %s "%s" for "%s" or program not in use';

  // GLX.Cadencer
  glsCadencerNotDefined   = 'Cadencer not defined!';
  glsCadencerNotDefinedEx = 'Cadencer not defined for  the ''%s'' component';

  // GLX.Materials
  glsMaterialNotFound = 'Loading failed: could not find material %s';
  glsMatLibNotDefined = 'Material Library not defined!';
  glsMaterialNotFoundInMatlib = 'Material not found in current Material Library!';
  glsMaterialNotFoundInMatlibEx = 'Material "%s" not found in current Material Library!';

  // GLX.SceneViewer
  glsAlreadyRendering   = 'Already rendering';
  glsDisplayList        = 'Failed to create a new display list for object ''%s''';
  glsNoRenderingContext = 'Could not create a rendering context';
  glsWrongVersion       = 'Need at least OpenGL version 1.1';
  glsTooManyLights      = 'Too many lights in the scene';
  glsSceneViewerNotDefined = 'SceneViewer not defined!';
  glsWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  glsWrongPrinter       = 'Couldn''t render to printer';

  // GLX.Shaders
  glsShaderNeedsAtLeastOneLightSource   = 'This shader needs at least one LightSource!';
  glsShaderNeedsAtLeastOneLightSourceEx = 'Shader ''%s'' needs at least one LightSource!';

  // GLX.Texture
  glsImageInvalid = 'Could not load texture, image is invalid';
  glsNoNewTexture = 'Could not get new texture name';

   // GLX.Tree
  glsCamera     = 'Camera';
  glsCameraRoot = 'Cameras';
  glsObjectRoot = 'Scene objects';
  glsSceneRoot  = 'Scene root';

  // GLX.Object categories
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
  glsOCDirectOpenGL = 'DirectOpenGL';
  glsOCRenderPoint = 'Render point';
  glsOCImposterSprite = 'Imposter sprite';

implementation

end.

