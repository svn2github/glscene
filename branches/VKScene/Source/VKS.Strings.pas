//
// This unit is part of the VKScene Project
//
{: VKS.Strings<p>
      String constants that are used in many VKScene units<p>

   <b>History :</b><font size=-1><ul>
      <li>02/01/16 - PW - Converted from GLScene
   </ul></font>
}
unit VKS.Strings;

interface

resourcestring
  // VKS.General
  vksDot = '.';
  vksError   = 'Error!';
  vksErrorEx = 'Error: ';

  // VKS.SceneViewer
  vksNoRenderingContext = 'Could not create a rendering context';
  vksWrongVersion       = 'Need at least OpenGL version 1.1';
  vksTooManyLights      = 'Too many lights in the scene';
  vksDisplayList        = 'Failed to create a new display list for object ''%s''';
  vksWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  vksWrongPrinter       = 'Couldn''t render to printer';
  vksAlreadyRendering   = 'Already rendering';
  vksSceneViewerNotDefined = 'SceneViewer not defined!';

  // VKS.Cadencer
  vksCadencerNotDefined   = 'Cadencer not defined!';
  vksCadencerNotDefinedEx = 'Cadencer not defined for  the ''%s'' component';

  // Shaders
  vksShaderNeedsAtLeastOneLightSource   = 'This shader needs at least one LightSource!';
  vksShaderNeedsAtLeastOneLightSourceEx = 'Shader ''%s'' needs at least one LightSource!';

  // VKTree
  vksSceneRoot  = 'Scene root';
  vksObjectRoot = 'Scene objects';
  vksCameraRoot = 'Cameras';
  vksCamera     = 'Camera';

  // VKS.Textures
  vksImageInvalid = 'Could not load texture, image is invalid';
  vksNoNewTexture = 'Could not get new texture name';

  // VKMaterials
  vksMatLibNotDefined = 'Material Library not defined!';
  vksMaterialNotFoundInMatlib = 'Material not found in current Material Library!';
  vksMaterialNotFoundInMatlibEx = 'Material "%s" not found in current Material Library!';

  // VKS.Objects
  vksSphereTopBottom = 'The top angle must be higher than the bottom angle';
  vksSphereStartStop = 'The start angle must be smaller than then stop angle';
  vksMaterialNotFound = 'Loading failed: could not find material %s';
  vksInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';

  // Common messages
  vksUnknownArchive = '%s : unknown archive version %d';
  vksOutOfMemory = 'Fatal: Out of memory';
  vksFileNotFound = 'File %s not found';
  vksFailedOpenFile = 'Could not open file: %s';
  vksFailedOpenFileFromCurrentDir = 'Could not open file: %s'#13#10'(Current directory is %s)';
  vksNoDescriptionAvailable = 'No description available';
  vksUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  vksUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;
  vksMissingResource = 'Missing application resource: %s: %s';

  vksIncompatibleTypes = 'Incompatible types!';
  vksUnknownType       = 'Unknown type!';
  vksUnsupportedType   = 'Unsupported type!';

  // Object categories
  vksOCBasicGeometry = 'Basic geometry';
  vksOCAdvancedGeometry = 'Advanced geometry';
  vksOCMeshObjects = 'Mesh objects';
  vksOCParticleSystems = 'Particle systems';
  vksOCEnvironmentObjects = 'Environment objects';
  vksOCSpecialObjects = 'Special objects';
  vksOCGraphPlottingObjects = 'Graph-plotting objects';
  vksOCDoodad = 'Doodad objects';
  vksOCHUDObjects = 'HUD objects';
  vksOCGuiObjects = 'GUI objects';
  vksOCProxyObjects = 'Proxy objects';
  vksOCExperimental = 'Experimental objects';

  vksUnknownParam =
    'Unknown %s "%s" for "%s" or program not in use';

implementation

end.

