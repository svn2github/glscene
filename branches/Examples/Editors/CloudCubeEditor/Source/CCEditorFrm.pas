unit CCEditorFrm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi, // to launch Help html

  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Menus,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.pngimage,

  OpenGL1X,

  GLFileTGA,
  // GR32_Image,
  GLGraphics, // PFX TGLGraphic32
  GLKeyboard, // Cadencer Keys for Zooming w/o scrollwheel
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLTexture,
  GLState,
  GLGizmoArv,
  GLGeomObjects,
  GLGraph,
  GLCadencer,
  GLVectorTypes,
  GLSkydome,
  GlUtils, // StrtoFloatdef
  GLVectorGeometry,
  // GLVectorFileObjects has changes in it
  GLVectorFileObjects,
  // Enable other Shape object file types
  // GLFileSMD, GLFileMD2, GLFile3DS, GLFilePLY, GLFileTIN,
  GLFileDXF,
  // Particle and Sprite makers
  GLImposter,
  GLParticleFX,
  GLParticles,
  GLPerlinPFX,
  GLProcTextures, // Clouds
  GLEParticleMasksManager,
  GLBitmapFont,
  GLWindowsFont,
  GLLensFlare,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo;

type
  TMovingAxis = (maAxisX, maAxisY, maAxisZ, maAxisXY, maAxisXZ, maAxisYZ);

  // The Not used yet things are for Movement and Time
  TCSPMTexture = Record
    Selected: Boolean;
    Permin, PerMax: Integer; // Byte ? 0..100
  end;

  TCSTexture = Record
    Selected: Boolean;
  end;

  TCSprite = Record // Generated..Current Generation
    Position: TVector;
    Size: TVector; // ? why..No Y (Depth).. maybe later
    // This rotation is of the Texture?or Sprite or
    // Not used?Cannot rotate the Texture here?
    Rotation: TVector;
    // Not used yet: Speed, Direction:
    // Not data here... computed per Cube no need to store?
    R, G, B, A: Double; // MaterialLibrary GLCloudsMatLib
    // To change Color during time... someday...
    AlbedoNumber: Integer;
    TextureName: String; // LibMaterialName
    TextureType, // how many images per file.. to know how to use TextureNumber
    // to get which Image to use when >1 per file
    TextureNumber: Integer;
  end;

  TCCube = Record
    Position: TVector; // Click //Need to get Position FROM Gizmo when moved
    Size: TVector; // CTRL //and Store in Record
    Rotation: TVector; // Shift
    SpeedX, SpeedY, SpeedZ: Double; // Not used yet
    ShapeShowObject, ShapeSizeCube, ShapeUpNegative, ShapeSpriteSize: Boolean;
    ShapeResolution, Upxyzw: Integer;
    // PLMask
    PLPSize, PLPLife, PLPInterval, PLScale: Double;
    PLMaskString: String;
    PLMaskStringSize, PLDepth, PLMaskXOffset, PLMaskYOffset, PLMaskZOffset,
      PLMaskImageSize, PLMaskType, PLMaskMenuNumber: Integer;
    PLColor: TColor;
    PLXImageFileName: String;
    PLPitch, PLRoll, PLTurn: Double;
    // Not saved in File:
    // Used in computation to control Each Sprite to avoid Clumpy movement
    CurrentMovingSprite: Integer;
    Generated: Boolean;
    GroupNumber: Integer;
    GroupColor: TColor;
    // Sprite info  ..Input data
    { * } SpriteCount: Integer;
    // Do 5..30..200 FIT in the Cube?
    // Sprite  Size Range  is Z required? All are planes ..No Depth
    XMax, XMin, YMax, YMin, ZMax, ZMin: Double;
    // Rotate Around to alter appearance
    // TurnAngle -180..180  PitchAngle:Fore..Aft  RollAngle:Left..Right
    RMax, RMin: Integer;
    // Texture Set parameters

    // There can be Several Combined files..
    // they will limit the number of Single files available..
    // 24 File maximum of Any type
    // 1 Per (24) File(s)
    // 4 per
    // 16 Set per (1) file.. ONLY 1 SET allowed in all 24
    // 16 random (1) //Should be the same type as they will be used randomly
    // 64 random (1)
    // Stratus Blurs, Wispy, Cumulus clumps, Flat Bottomed Rain
    TextureType,
    // Used in Generation Positioning..someday
    SpriteType, CloudType, CloudShape, CloudPattern: Integer;

    // Albedo1,Albedo2,Albedo3,Albedo4,Albedo5 :TColor;
    // TColor not needed:Computed during Setting
    // Set Sprite IAW Cloud Z in its Cube
    Albedo1R, Albedo1G, Albedo1B, Albedo1A, Albedo2R, Albedo2G, Albedo2B,
      Albedo2A, Albedo3R, Albedo3G, Albedo3B, Albedo3A, Albedo4R, Albedo4G,
      Albedo4B, Albedo4A, Albedo5R, Albedo5G, Albedo5B, Albedo5A: Double;
    // Not used yet
    // Formation.. Dissipation is Changed during day..Time
    // Start is That Cubes starting Value
    AlphaStart, AlphaRate, AlphaFormation, AlphaDissipation: Double;
    Sprites: Array of TCSprite; // Dynamic
    // The Image Usage parameters may Change.. the NAMES,Type,AlphaSet do NOT
    // Used IAW TextureType
    TexturesRecord: Array [1 .. 24] of TCSPMTexture;
    TextureSetRecord: Array [1 .. 16] of TCSTexture;
    TextureSet64Record: Array [1 .. 64] of TCSTexture;
  end;

  // GPFNodes: TGlobalData;
  // Global data AND Array of Cubes of Array of Sprites;
  TGlobalData = Record
    // Cloud data
    // Make like Trees in Forest Demo?
    // 1..20 Real ones.. Others are pictures..Whole Cube turned into 1 Sprite
    // Render Each and Save into 8x8 or 4x4 Combined image  Or Imposter
    // To do: Make this a Class or Component
    CloudsGenerated: Boolean;
    { * } CCubeCount: Integer;
    CCubes: Array of TCCube; // Dynamic
    // Texture.. Type .. IAW TextureType
    // This stores the names, Matlib settings, TextureType (# images per file)
    // The Cube stores the Usage parameters..as changed per cube
    TextureNames: Array [1 .. 24] of String;
    TextureAlpha: Array [1 .. 24] of Array [1 .. 4] of Integer;
    TextureType: Array [1 .. 24] of Integer;
    ShapeCount, ShapeMadeCount, ShapeCurrentCount: Integer;
    ShapeNames: Array of String; // Dynamic
    // Sun data : See EarthSkyDome Parameters
    // Not used yet  :All below
    SunPosition: TVector;
    SunMoving, LensFlare: Boolean;
    CurrentTime: TDateTime;
    DawnR, DawnG, DawnB, DawnA: Double;
    DuskR, DuskG, DuskB, DuskA: Double;
    // String; to load parameters.. maybe later
    // GLEarthSkyDome1: Sky, Haze, Deep,Dawn,Zenith,Night:Color,
    // Turbidity SunElevation Slices Stacks  [Stars] [Bands]
    SkyDomeUsed: Integer;
    SkyDomeName: String;
    ProDomeUsed: Integer;
    ProDomeName: String;
    RingRadius: Integer;
    RingRadiusName: String; // Filename of Pan image?
    CirrusType: Integer;
    CirrusTypeName: String; // Filename of ??? IAW type
  end;

  TACloudDemoForm = class(TForm)
    GLScene1: TGLScene;
    Scn: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLSunSphere: TGLSphere;
    GLLightSource1: TGLLightSource;
    GLGitzmoDC: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLCameraTargetDC: TGLDummyCube;
    GLXYZGrid1: TGLXYZGrid;
    AAUIHolderPanel: TPanel;
    StatusBar: TStatusBar;
    GLCloudCube: TGLCube;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet4: TTabSheet;
    AddBtn: TSpeedButton;
    TotalCubesLabel: TLabel;
    AddBlueBtn: TSpeedButton;
    AddAtLastBtn: TSpeedButton;
    Label3: TLabel;
    PositionXEdit: TEdit;
    PositionYEdit: TEdit;
    PositionZEdit: TEdit;
    RollEdit: TEdit;
    TurnEdit: TEdit;
    PitchEdit: TEdit;
    Label2: TLabel;
    WidthEdit: TEdit;
    DepthYEdit: TEdit;
    HeightZEdit: TEdit;
    Label1: TLabel;
    NewBtn: TSpeedButton;
    LoadBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    TabSheet2: TTabSheet;
    ColorDialog1: TColorDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpritesRG: TRadioGroup;
    Label15: TLabel;
    GenerateBtn: TSpeedButton;
    GLCloudsMatLib: TGLMaterialLibrary;
    CloudCubeDC: TGLDummyCube;
    DeleteBtn: TSpeedButton;
    ResetBtn: TSpeedButton;
    CurrentCubeUpDown: TUpDown;
    GroupColorPanel: TPanel;
    ExitBtn: TSpeedButton;
    GroupColorEdit: TEdit;
    Label31: TLabel;
    ImageCaptureRG: TRadioGroup;
    GLSkyDome1: TGLSkyDome;
    CurrentCubesLabel: TLabel;
    PageControl3: TPageControl;
    TabSheet12: TTabSheet;
    TabSheet14: TTabSheet;
    TabSheet15: TTabSheet;
    Label13: TLabel;
    Label10: TLabel;
    PartsEdit: TEdit;
    Label12: TLabel;
    RotationMaxEdit: TEdit;
    Label11: TLabel;
    RotationMinEdit: TEdit;
    SIZERANGEGroupBox: TGroupBox;
    Label9: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    ZMinEdit: TEdit;
    ZMaxEdit: TEdit;
    YMinEdit: TEdit;
    YMaxEdit: TEdit;
    XMinEdit: TEdit;
    XMaxEdit: TEdit;
    AlphaDissipationEdit: TEdit;
    Label19: TLabel;
    AlphaFormationEdit: TEdit;
    Label16: TLabel;
    AlphaInitialEdit: TEdit;
    Label18: TLabel;
    AlbedoPanel5: TPanel;
    AlbedoPanel4: TPanel;
    AlbedoPanel3: TPanel;
    AlbedoPanel2: TPanel;
    AlbedoPanel1: TPanel;
    Label71: TLabel;
    Albedo5AEdit: TEdit;
    Albedo5BEdit: TEdit;
    Albedo5GEdit: TEdit;
    Albedo5REdit: TEdit;
    Albedo4REdit: TEdit;
    Albedo4GEdit: TEdit;
    Albedo4BEdit: TEdit;
    Albedo4AEdit: TEdit;
    Albedo3AEdit: TEdit;
    Albedo3BEdit: TEdit;
    Albedo3GEdit: TEdit;
    Albedo3REdit: TEdit;
    Albedo2REdit: TEdit;
    Albedo2GEdit: TEdit;
    Albedo2BEdit: TEdit;
    Albedo2AEdit: TEdit;
    Albedo1AEdit: TEdit;
    Albedo1BEdit: TEdit;
    Albedo1GEdit: TEdit;
    Albedo1REdit: TEdit;
    Label70: TLabel;
    Label14: TLabel;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    Label21: TLabel;
    Label20: TLabel;
    TextureBtn1: TSpeedButton;
    Label23: TLabel;
    Label24: TLabel;
    TextureBtn2: TSpeedButton;
    Label26: TLabel;
    Label25: TLabel;
    TextureBtn3: TSpeedButton;
    Label28: TLabel;
    Label27: TLabel;
    TextureBtn4: TSpeedButton;
    Label30: TLabel;
    Label29: TLabel;
    TextureBtn5: TSpeedButton;
    Label32: TLabel;
    Label33: TLabel;
    TextureBtn6: TSpeedButton;
    TextureMinEdit1: TEdit;
    TextureMaxEdit1: TEdit;
    TextureFilename1: TEdit;
    TextureMinEdit2: TEdit;
    TextureMaxEdit2: TEdit;
    TextureFilename2: TEdit;
    TextureMinEdit3: TEdit;
    TextureMaxEdit3: TEdit;
    TextureFilename3: TEdit;
    TextureMinEdit4: TEdit;
    TextureMaxEdit4: TEdit;
    TextureFilename4: TEdit;
    TextureMinEdit5: TEdit;
    TextureMaxEdit5: TEdit;
    TextureFilename5: TEdit;
    TextureCB1: TCheckBox;
    TextureCB2: TCheckBox;
    TextureCB3: TCheckBox;
    TextureCB4: TCheckBox;
    TextureCB5: TCheckBox;
    TextureCB6: TCheckBox;
    TextureMinEdit6: TEdit;
    TextureMaxEdit6: TEdit;
    TextureFilename6: TEdit;
    TabSheet6: TTabSheet;
    Label34: TLabel;
    Label35: TLabel;
    TextureBtn7: TSpeedButton;
    Label40: TLabel;
    Label41: TLabel;
    TextureBtn8: TSpeedButton;
    Label42: TLabel;
    Label43: TLabel;
    TextureBtn9: TSpeedButton;
    Label44: TLabel;
    Label45: TLabel;
    TextureBtn10: TSpeedButton;
    Label46: TLabel;
    Label47: TLabel;
    TextureBtn11: TSpeedButton;
    Label48: TLabel;
    Label49: TLabel;
    TextureBtn12: TSpeedButton;
    TextureCB7: TCheckBox;
    TextureMaxEdit7: TEdit;
    TextureMinEdit7: TEdit;
    TextureFilename7: TEdit;
    TextureCB8: TCheckBox;
    TextureMaxEdit8: TEdit;
    TextureMinEdit8: TEdit;
    TextureFilename8: TEdit;
    TextureCB9: TCheckBox;
    TextureMaxEdit9: TEdit;
    TextureMinEdit9: TEdit;
    TextureFilename9: TEdit;
    TextureCB10: TCheckBox;
    TextureMaxEdit10: TEdit;
    TextureMinEdit10: TEdit;
    TextureFilename10: TEdit;
    TextureCB11: TCheckBox;
    TextureMaxEdit11: TEdit;
    TextureMinEdit11: TEdit;
    TextureFilename11: TEdit;
    TextureCB12: TCheckBox;
    TextureMaxEdit12: TEdit;
    TextureMinEdit12: TEdit;
    TextureFilename12: TEdit;
    TabSheet7: TTabSheet;
    Label36: TLabel;
    Label37: TLabel;
    TextureBtn13: TSpeedButton;
    Label50: TLabel;
    Label51: TLabel;
    TextureBtn14: TSpeedButton;
    Label52: TLabel;
    Label53: TLabel;
    TextureBtn15: TSpeedButton;
    Label54: TLabel;
    Label55: TLabel;
    TextureBtn16: TSpeedButton;
    Label56: TLabel;
    Label57: TLabel;
    TextureBtn17: TSpeedButton;
    Label58: TLabel;
    Label59: TLabel;
    TextureBtn18: TSpeedButton;
    TextureCB13: TCheckBox;
    TextureMaxEdit13: TEdit;
    TextureMinEdit13: TEdit;
    TextureFilename13: TEdit;
    TextureCB14: TCheckBox;
    TextureMaxEdit14: TEdit;
    TextureMinEdit14: TEdit;
    TextureFilename14: TEdit;
    TextureCB15: TCheckBox;
    TextureMaxEdit15: TEdit;
    TextureMinEdit15: TEdit;
    TextureFilename15: TEdit;
    TextureCB16: TCheckBox;
    TextureMaxEdit16: TEdit;
    TextureMinEdit16: TEdit;
    TextureFilename16: TEdit;
    TextureCB17: TCheckBox;
    TextureMaxEdit17: TEdit;
    TextureMinEdit17: TEdit;
    TextureFilename17: TEdit;
    TextureCB18: TCheckBox;
    TextureMaxEdit18: TEdit;
    TextureMinEdit18: TEdit;
    TextureFilename18: TEdit;
    TabSheet8: TTabSheet;
    Label38: TLabel;
    Label39: TLabel;
    TextureBtn19: TSpeedButton;
    Label60: TLabel;
    Label61: TLabel;
    TextureBtn20: TSpeedButton;
    Label62: TLabel;
    Label63: TLabel;
    TextureBtn21: TSpeedButton;
    Label64: TLabel;
    Label65: TLabel;
    TextureBtn22: TSpeedButton;
    Label66: TLabel;
    Label67: TLabel;
    TextureBtn23: TSpeedButton;
    Label68: TLabel;
    Label69: TLabel;
    TextureBtn24: TSpeedButton;
    TextureCB19: TCheckBox;
    TextureMaxEdit19: TEdit;
    TextureMinEdit19: TEdit;
    TextureFilename19: TEdit;
    TextureCB20: TCheckBox;
    TextureMaxEdit20: TEdit;
    TextureMinEdit20: TEdit;
    TextureFilename20: TEdit;
    TextureCB21: TCheckBox;
    TextureMaxEdit21: TEdit;
    TextureMinEdit21: TEdit;
    TextureFilename21: TEdit;
    TextureCB22: TCheckBox;
    TextureMaxEdit22: TEdit;
    TextureMinEdit22: TEdit;
    TextureFilename22: TEdit;
    TextureCB23: TCheckBox;
    TextureMaxEdit23: TEdit;
    TextureMinEdit23: TEdit;
    TextureFilename23: TEdit;
    TextureCB24: TCheckBox;
    TextureMaxEdit24: TEdit;
    TextureMinEdit24: TEdit;
    TextureFilename24: TEdit;
    RegenerateBtn: TSpeedButton;
    SpriteCubeDC: TGLDummyCube;
    HelpBtn: TSpeedButton;
    PFXRenderer: TGLParticleFXRenderer;
    PFXCSpriteManager: TGLCustomSpritePFXManager;
    Timer1: TTimer;
    GLHiddenColorCube: TGLCube;
    PFXCubeDC: TGLDummyCube;
    GLParticles1: TGLParticles;
    ParticleSprite: TGLSprite;
    GridOffCB: TCheckBox;
    Label109: TLabel;
    SpeedXEdit: TEdit;
    SpeedYEdit: TEdit;
    SpeedZEdit: TEdit;
    TabSheet11: TTabSheet;
    TextureTypeRG: TRadioGroup;
    ImageTypeGetSettingsRG: TRadioGroup;
    ImageTypeSetSettingsRG: TRadioGroup;
    TabSheet13: TTabSheet;
    ImageImageAlphaRG: TRadioGroup;
    ImageAlphaGetSettingsRG: TRadioGroup;
    ImageAlphaSetSettingsRG: TRadioGroup;
    ImageTextureFormatRG: TRadioGroup;
    ImageTextureModeRG: TRadioGroup;
    ImageBlendingModeRG: TRadioGroup;
    GLFreeFormx: TGLFreeForm;
    ShapeProgressBar: TProgressBar;
    ShapeSizeSpriteCB: TCheckBox;
    TabSheet3: TTabSheet;
    FreeFormDC: TGLDummyCube;
    TimeOnCB: TCheckBox;
    Panel1: TPanel;
    AddShapeBtn: TSpeedButton;
    ShapeSizeCubeCB: TCheckBox;
    ShapeShowObjectCB: TCheckBox;
    ShapeUpxyzRG: TRadioGroup;
    PFXPolyFogManager: TGLPolygonPFXManager;
    ShapeUpNegativeCB: TCheckBox;
    AlphaRateEdit: TEdit;
    Label110: TLabel;
    ShapeResolutionEdit: TEdit;
    ScnOnCB: TCheckBox;
    PFXPLMaskManager: TGLPointLightPFXManager;
    MatLib: TGLMaterialLibrary;
    GLEParticleMasksManager1: TGLEParticleMasksManager;
    WinFont: TGLWindowsBitmapFont;
    BlueCubeOffCB: TCheckBox;
    PLMaskPopupMenu: TPopupMenu;
    Torus1: TMenuItem;
    Sphere1: TMenuItem;
    Tube1: TMenuItem;
    Cube1: TMenuItem;
    TabSheet17: TTabSheet;
    PLPitchEdit: TEdit;
    PLRollEdit: TEdit;
    PLTurnEdit: TEdit;
    PLPIntervalEdit: TEdit;
    Label118: TLabel;
    Label115: TLabel;
    Label114: TLabel;
    Label113: TLabel;
    PLCharEdit: TEdit;
    Label111: TLabel;
    Label112: TLabel;
    PLDepthEdit: TEdit;
    PLCharacterSizeEdit: TEdit;
    PLScaleEdit: TEdit;
    Label117: TLabel;
    ZImage: TImage;
    YImage: TImage;
    PlLoadXBtn: TSpeedButton;
    XImage: TImage;
    CloudTypeRG: TRadioGroup;
    CloudShapeRG: TRadioGroup;
    CloudPatternRG: TRadioGroup;
    SpriteTypeRG: TRadioGroup;
    PLMaskTypeRG: TRadioGroup;
    PLMaskMaskXOffsetTB: TTrackBar;
    PLMaskMaskYOffsetTB: TTrackBar;
    PLMaskMaskZOffsetTB: TTrackBar;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    LabelImageSize: TLabel;
    TV1: TMenuItem;
    Star1: TMenuItem;
    Label116: TLabel;
    PLPSizeEdit: TEdit;
    PLPLifeEdit: TEdit;
    Label119: TLabel;
    TabSheet10: TTabSheet;
    ValidateAndSetTexturesBtn: TSpeedButton;
    SingleStepBtn: TSpeedButton;
    PLColorPanel: TPanel;
    PFX16SetGroupBox: TGroupBox;
    Texture16_13CB: TCheckBox;
    Texture16_14CB: TCheckBox;
    Texture16_15CB: TCheckBox;
    Texture16_16CB: TCheckBox;
    Texture16_12CB: TCheckBox;
    Texture16_11CB: TCheckBox;
    Texture16_10CB: TCheckBox;
    Texture16_9CB: TCheckBox;
    Texture16_5CB: TCheckBox;
    Texture16_6CB: TCheckBox;
    Texture16_7CB: TCheckBox;
    Texture16_8CB: TCheckBox;
    Texture16_4CB: TCheckBox;
    Texture16_3CB: TCheckBox;
    Texture16_2CB: TCheckBox;
    Texture16_1CB: TCheckBox;
    PFX64SetGroupBox: TGroupBox;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Label126: TLabel;
    Label127: TLabel;
    Label128: TLabel;
    Label129: TLabel;
    Label130: TLabel;
    Label131: TLabel;
    Label132: TLabel;
    Label133: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Texture64_1CB: TCheckBox;
    Texture64_2CB: TCheckBox;
    Texture64_3CB: TCheckBox;
    Texture64_4CB: TCheckBox;
    Texture64_5CB: TCheckBox;
    Texture64_6CB: TCheckBox;
    Texture64_7CB: TCheckBox;
    Texture64_8CB: TCheckBox;
    Texture64_9CB: TCheckBox;
    Texture64_10CB: TCheckBox;
    Texture64_11CB: TCheckBox;
    Texture64_12CB: TCheckBox;
    Texture64_13CB: TCheckBox;
    Texture64_14CB: TCheckBox;
    Texture64_15CB: TCheckBox;
    Texture64_16CB: TCheckBox;
    Texture64_17CB: TCheckBox;
    Texture64_18CB: TCheckBox;
    Texture64_19CB: TCheckBox;
    Texture64_20CB: TCheckBox;
    Texture64_21CB: TCheckBox;
    Texture64_22CB: TCheckBox;
    Texture64_23CB: TCheckBox;
    Texture64_24CB: TCheckBox;
    Texture64_25CB: TCheckBox;
    Texture64_26CB: TCheckBox;
    Texture64_27CB: TCheckBox;
    Texture64_28CB: TCheckBox;
    Texture64_29CB: TCheckBox;
    Texture64_30CB: TCheckBox;
    Texture64_31CB: TCheckBox;
    Texture64_32CB: TCheckBox;
    Texture64_33CB: TCheckBox;
    Texture64_34CB: TCheckBox;
    Texture64_35CB: TCheckBox;
    Texture64_36CB: TCheckBox;
    Texture64_37CB: TCheckBox;
    Texture64_38CB: TCheckBox;
    Texture64_39CB: TCheckBox;
    Texture64_40CB: TCheckBox;
    Texture64_41CB: TCheckBox;
    Texture64_42CB: TCheckBox;
    Texture64_43CB: TCheckBox;
    Texture64_44CB: TCheckBox;
    Texture64_45CB: TCheckBox;
    Texture64_46CB: TCheckBox;
    Texture64_47CB: TCheckBox;
    Texture64_48CB: TCheckBox;
    Texture64_49CB: TCheckBox;
    Texture64_50CB: TCheckBox;
    Texture64_51CB: TCheckBox;
    Texture64_52CB: TCheckBox;
    Texture64_53CB: TCheckBox;
    Texture64_54CB: TCheckBox;
    Texture64_55CB: TCheckBox;
    Texture64_56CB: TCheckBox;
    Texture64_57CB: TCheckBox;
    Texture64_58CB: TCheckBox;
    Texture64_59CB: TCheckBox;
    Texture64_60CB: TCheckBox;
    Texture64_61CB: TCheckBox;
    Texture64_62CB: TCheckBox;
    Texture64_63CB: TCheckBox;
    Texture64_64CB: TCheckBox;
    Label136: TLabel;
    Label137: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    PLRegenerateDupedBtn: TSpeedButton;
    PLResetDupedBtn: TSpeedButton;
    PLAddDupedBtn: TSpeedButton;
    PLMaskImageSizeTB: TTrackBar;
    ImposterDC: TGLDummyCube;
    ImposterDirectOGL: TGLDirectOpenGL;
    CloudSphere: TGLSphere;
    PageControl4: TPageControl;
    TabSheet9: TTabSheet;
    TabSheet16: TTabSheet;
    SunGroupBox: TGroupBox;
    Label17: TLabel;
    SunTimeLabel: TLabel;
    Label107: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label108: TLabel;
    SunXEdit: TEdit;
    SunYEdit: TEdit;
    SunZEdit: TEdit;
    SunMovingCB: TCheckBox;
    DawnPanel: TPanel;
    DuskPanel: TPanel;
    SunLensFlareCB: TCheckBox;
    SunDawnREdit: TEdit;
    SunDawnGEdit: TEdit;
    SunDawnBEdit: TEdit;
    SunDawnAEdit: TEdit;
    SunDuskAEdit: TEdit;
    SunDuskBEdit: TEdit;
    SunDuskGEdit: TEdit;
    SunDuskREdit: TEdit;
    LightsOnCB: TCheckBox;
    Edit1: TEdit;
    DomeGroupBox: TGroupBox;
    ProDomeCB: TCheckBox;
    SkyDomeCB: TCheckBox;
    SkyDomeFilenameEdit: TEdit;
    ProDomeFilenameEdit: TEdit;
    TabSheet19: TTabSheet;
    TabSheet20: TTabSheet;
    MFxMakerBtn: TSpeedButton;
    NanxMakerBtn: TSpeedButton;
    PCxMakerBtn: TSpeedButton;
    TexEditorBtn: TSpeedButton;
    TexCombineBtn: TSpeedButton;
    Label22: TLabel;
    RingRadiusEdit: TEdit;
    RingRadiusFileNameEdit: TEdit;
    ProDomeFilenameBtn: TSpeedButton;
    SkyDomeFilenameBtn: TSpeedButton;
    CirrusRG: TRadioGroup;
    CirrusFilenameEdit: TEdit;
    CirrusFilenameOpenBtn: TSpeedButton;
    CirrusFilenameSaveBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    UseCloudsCB: TCheckBox;
    UseCloudsSettingCB: TCheckBox;
    CloudImageSizeUsedEdit: TEdit;
    CloudMinUsedEdit: TEdit;
    Label73: TLabel;
    Label72: TLabel;
    CloudMinRangeUsedEdit: TEdit;
    UseCloudsRangeCB: TCheckBox;
    CloudSharpUsedEdit: TEdit;
    Label74: TLabel;
    CloudSharpRangeUsedEdit: TEdit;
    UseCloudsSRangeCB: TCheckBox;
    CloudRandomSeedUsedEdit: TEdit;
    Label75: TLabel;
    UseCloudFileCB: TCheckBox;
    CloudFileUsedEdit: TEdit;
    CloudFileOpenBtn: TSpeedButton;
    CloudChangeAmountEdit: TEdit;
    CloudChangeMoreBtn: TSpeedButton;
    CloudChangeLessBtn: TSpeedButton;
    Label76: TLabel;
    CloudAnimeEdit: TEdit;
    CloudsAnimeFasterBtn: TSpeedButton;
    CloudsAnimeSlowerBtn: TSpeedButton;
    CloudsAnimeMuchSlowerBtn: TSpeedButton;
    Label77: TLabel;
    ViewerBackgroundColorPanel: TPanel;
    GLLensFlare1: TGLLensFlare;
    GLEarthSkyDome1: TGLEarthSkyDome;
    procedure FormCreate(Sender: TObject);
    procedure ClearImageXYZ;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);

    procedure ScnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    // Clouds Panel stuff
    procedure NewBtnClick(Sender: TObject);
    procedure ClearAll;

    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);

    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TimeOnCBClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure SpritesRGClick(Sender: TObject); // On/Off/Both
    // Generation and its Code
    procedure GenerateBtnClick(Sender: TObject);
    procedure RegenerateBtnClick(Sender: TObject);
    procedure GenerateClouds(Regenerate: Boolean);
    procedure GenerateSprites(Regenerate: Boolean; I: Integer);
    procedure GenerateParticles(Regenerate: Boolean; I: Integer);
    { procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject); }
    procedure GeneratePFXPolyFog(Regenerate: Boolean; I: Integer);
    procedure GenerateImageSprites(Regenerate: Boolean; I: Integer);
    procedure GenerateImposter(Regenerate: Boolean; I: Integer);

    procedure GeneratePFXCSprite(Regenerate: Boolean; I: Integer);
    procedure PFXCSpriteManagerPrepareTextureImage(Sender: TObject;
      destBmp32: TGLBitmap32; var texFormat: Integer);
    procedure PFXCSpriteManagerCreateParticle(Sender: TObject;
      aParticle: TGLParticle);
    procedure GeneratePFXPLMask(Regenerate: Boolean; I: Integer);
    procedure PFXPLSetMask(CubeNumber: Integer);
    procedure PlLoadXBtnClick(Sender: TObject);
    procedure PLMaskPopupMenu1Click(Sender: TObject);
    procedure PLMaskMenuSet(MaskMenuNumber, XOffset, YOffset, ZOffset,
      ImageSize: Integer);
    procedure PLColorPanelClick(Sender: TObject);
    procedure PLMaskMaskXOffsetTBChange(Sender: TObject);
    procedure PLMaskMaskYOffsetTBChange(Sender: TObject);
    procedure PLMaskMaskZOffsetTBChange(Sender: TObject);
    procedure PLMaskImageSizeTBChange(Sender: TObject);
    procedure PLMaskImageSizeChange(NewSize: Integer);
    procedure PFXPLMaskManagerCreateParticle(Sender: TObject;
      aParticle: TGLParticle);
    procedure GenerateNinianeClouds(Regenerate: Boolean; I: Integer);
    procedure GenerateRealSlow(Regenerate: Boolean; I: Integer);
    // Generation and its Code

    procedure AddShapeBtnClick(Sender: TObject);
    procedure LoadMakeShape(CubeNumber, J: Integer; FileName: String);
    procedure BuildGrid(I, J: Integer);
    procedure AddBtnClick(Sender: TObject);
    procedure AddBlueBtnClick(Sender: TObject);
    procedure AddAtLastBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure GroupColorPanelClick(Sender: TObject);
    procedure CurrentCubeUpDownChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);

    procedure MakeABlank(I, Where: Integer);
    procedure SetNodeCubeToGPFCube(I: Integer);
    procedure SetGPFCubeToNodeCube(I: Integer);
    procedure SetGPFCubeToText(I: Integer);
    procedure SetTextToGPFCube(I: Integer);

    procedure NanxMakerBtnClick(Sender: TObject);
    procedure MFxMakerBtnClick(Sender: TObject);
    procedure PCxMakerBtnClick(Sender: TObject);
    procedure TexEditorBtnClick(Sender: TObject);
    procedure TexCombineBtnClick(Sender: TObject);

    procedure ImageCaptureRGClick(Sender: TObject);
    procedure GridOffCBClick(Sender: TObject);
    procedure BlueCubeOffCBClick(Sender: TObject);
    procedure SingleStepBtnClick(Sender: TObject);
    // Cubes panel
    // Size and Type panels have no Actions, only inputs read during Make Generation

    // Image All
    procedure ValidateAndSetTexturesBtnClick(Sender: TObject);

    // Image File //Texture panel
    procedure TextureBtn1Click(Sender: TObject);
    procedure TextureBtn2Click(Sender: TObject);
    procedure TextureBtn3Click(Sender: TObject);
    procedure TextureBtn4Click(Sender: TObject);
    procedure TextureBtn5Click(Sender: TObject);
    procedure TextureBtn6Click(Sender: TObject);
    procedure TextureBtn7Click(Sender: TObject);
    procedure TextureBtn8Click(Sender: TObject);
    procedure TextureBtn9Click(Sender: TObject);
    procedure TextureBtn10Click(Sender: TObject);
    procedure TextureBtn11Click(Sender: TObject);
    procedure TextureBtn12Click(Sender: TObject);
    procedure TextureBtn13Click(Sender: TObject);
    procedure TextureBtn14Click(Sender: TObject);
    procedure TextureBtn15Click(Sender: TObject);
    procedure TextureBtn16Click(Sender: TObject);
    procedure TextureBtn17Click(Sender: TObject);
    procedure TextureBtn18Click(Sender: TObject);
    procedure TextureBtn19Click(Sender: TObject);
    procedure TextureBtn20Click(Sender: TObject);
    procedure TextureBtn21Click(Sender: TObject);
    procedure TextureBtn22Click(Sender: TObject);
    procedure TextureBtn23Click(Sender: TObject);
    procedure TextureBtn24Click(Sender: TObject);
    // Image Alpha
    procedure ImageAlphaSetSettingsRGClick(Sender: TObject);
    procedure ImageAlphaGetSettingsRGClick(Sender: TObject);
    // Image Type
    procedure ImageTypeGetSettingsRGClick(Sender: TObject);
    procedure ImageTypeSetSettingsRGClick(Sender: TObject);
    // Image Mask.. all that stuff is in with the Generation procedures above

    // Albedo
    procedure AlbedoPanel1Click(Sender: TObject);
    procedure AlbedoPanel2Click(Sender: TObject);
    procedure AlbedoPanel3Click(Sender: TObject);
    procedure AlbedoPanel4Click(Sender: TObject);
    procedure AlbedoPanel5Click(Sender: TObject);

    // Global Panel stuff
    procedure DawnPanelClick(Sender: TObject);
    procedure DuskPanelClick(Sender: TObject);
    procedure SkyDomeCBClick(Sender: TObject);
    procedure ProDomeCBClick(Sender: TObject);
    procedure CirrusRGClick(Sender: TObject);
    procedure LightsOnCBClick(Sender: TObject);
    procedure ViewerBackgroundColorPanelClick(Sender: TObject);
    procedure ImposterDirectOGLRender(Sender: TObject;
      var rci: TGLRenderContextInfo);

    /// //
    procedure SkyDomeFilenameBtnClick(Sender: TObject);
    procedure ProDomeFilenameBtnClick(Sender: TObject);
    procedure CirrusFilenameOpenBtnClick(Sender: TObject);
    procedure LoadCirrusCloudData;
    procedure SETCirrusCloudData;
    procedure GetCirrusCloudData;
    procedure CirrusFilenameSaveBtnClick(Sender: TObject);
    procedure CloudFileOpenBtnClick(Sender: TObject);
    procedure CloudChangeMoreBtnClick(Sender: TObject);
    procedure CloudChangeLessBtnClick(Sender: TObject);
    procedure CloudsAnimeMuchSlowerBtnClick(Sender: TObject);
    procedure CloudsAnimeFasterBtnClick(Sender: TObject);
    procedure CloudsAnimeSlowerBtnClick(Sender: TObject);
    procedure UseCloudsCBClick(Sender: TObject);
    procedure SunLensFlareCBClick(Sender: TObject);

  private
    { Private declarations }
    function MouseWorldPos(X, Y: Integer): TVector;
  public
    { Public declarations }
    // Imposter Runtime creation
    impBuilder: TGLStaticImposterBuilder;
    renderPoint: TGLRenderPoint;

    // Gizmo stuff
    MovingAxis: TMovingAxis;
    pick, // : TGLCustomSceneObject;
    SelectedObject, // : TGLCustomSceneObject;
    OldCursorPick: TGLBaseSceneObject;
    // So it wont crash on picking weird stuff
    // TGLCustomSceneObject;
    GizmoX, GizmoY, GizmoZ: TGLGizmoArrow;
    GizmoCornerXY, GizmoCornerXZ, GizmoCornerYZ: TGLGizmoCorner;

    // Global Cirrus Clouds..   {show Global Cirrus Clouds}
    // Sky Clouds..NOT Storm clouds
    CloudMinUsed, CloudMinRangeUsed, CloudImageSizeUsed, CloudRanger { }
      : Integer;
    CloudAnime, CloudChangeAmount, CloudSharpUsed, CloudSharpRangeUsed,
      CloudSharpRanger { } , CloudChange { } , CloudNewTime { } : Double;
    CloudRandomSeedUsed: Longint;
    CloudFileUsed: String;
    UseCloudsSRange, UseCloudsRange, UseCloudFile, UseClouds,
      UseCloudsSetting: Boolean;

    // other stuff
    JustLoaded, // Used in Loading to Make w/o Regeneration
    AnyTexturesLoaded, // Even if not used, There Must be at least 1
    FirstParticle, // First Particle.. the rest are Children
    FirstTime: Boolean; // Used to do Onshow only once
    ImageSprites, // Used to get a Image Name
    ImposterCube, // The ImposterCube number
    PfxCSOnlyOne, // The CS Cube number
    PfxCSTotals, ParticleCountTotal, TotalSprites, // Used in Timer for Display
    PLMaskMenuNumberStored, // To keep the Menu number Until Generated
    SunMultiplier,
    // Used to make the Sun Go back and forth, rather than Go and Skip back
    // Mouse moving
    mx, my: Integer;
    lastMouseWorldPos: TVector;
    // Global data AND Array of Cubes of Sprites;
    GPFNodes: TGlobalData;
    procedure UpdateGizmo;
  end;

var
  ACloudDemoForm: TACloudDemoForm;
  ImagePath: String; // Here so other forms Know where images are

implementation

uses
  CCTextureFrm, CCATextureEditorFrm,
  CCProceduralCloudsFrm, CCNanMakerFrm, CCMFMakerFrm;

{$R *.dfm}

function TACloudDemoForm.MouseWorldPos(X, Y: Integer): TVector;
var
  v: TVector;
begin
  Y := Scn.Height - Y;
  if Assigned(SelectedObject) then
  begin
    SetVector(v, X, Y, 0);

    case MovingAxis of
      maAxisX:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneXZ(v,
            SelectedObject.Position.Y, Result);
        end;
      maAxisY:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneYZ(v,
            SelectedObject.Position.X, Result);
        end;
      maAxisZ:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneYZ(v,
            SelectedObject.Position.X, Result);
        end;
      maAxisXY:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneXY(v,
            SelectedObject.Position.Z, Result);
        end;
      maAxisXZ:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneXZ(v,
            SelectedObject.Position.Y, Result);
        end;
      maAxisYZ:
        begin
          Scn.Buffer.ScreenVectorIntersectWithPlaneYZ(v,
            SelectedObject.Position.X, Result);
        end;
    end;

  end
  else
    SetVector(Result, NullVector);
end;

procedure TACloudDemoForm.FormCreate(Sender: TObject);
begin
  FirstTime := True;
  JustLoaded := False;
  SunMultiplier := 1;
  // Cirrus Clouds
  CloudChange := 0; // CloudChange  CloudNewTime
  CloudNewTime := 0;
  UseClouds := False;
  UseCloudsSetting := False;
  //
  GPFNodes.CloudsGenerated := False;
  GPFNodes.CCubeCount := 0;
  GPFNodes.ShapeCount := 0;
  GPFNodes.ShapeCurrentCount := 0;
  GPFNodes.ShapeMadeCount := 0;
  SpriteTypeRG.ItemIndex := 0;
  PLMaskMenuNumberStored := 0;
  ImposterCube := 0;
  TotalSprites := 0;
  ParticleCountTotal := 0;
  ImageSprites := 0;
  FirstParticle := True;
  AnyTexturesLoaded := False;
  Randomize;
  ImagePath := ExtractFilePath(ParamStr(0)) + 'Images';
end;

procedure TACloudDemoForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  If FirstTime then
  begin
    FirstTime := False;
    GizmoX := TGLGizmoArrow.Create(GLScene1);
    GizmoX.GizmoType := gtAxisX;
    GizmoX.Name := 'GizmoX';
    GizmoX.Height := 0.5;

    GLGitzmoDC.AddChild(GizmoX);

    GizmoY := TGLGizmoArrow.Create(GLScene1);
    GizmoY.GizmoType := gtAxisY;
    GizmoY.Name := 'GizmoY';
    GizmoY.Height := 0.5;

    GLGitzmoDC.AddChild(GizmoY);

    GizmoZ := TGLGizmoArrow.Create(GLScene1);
    GizmoZ.GizmoType := gtAxisZ;
    GizmoZ.Name := 'GizmoZ';
    GizmoZ.Height := 0.5;

    GLGitzmoDC.AddChild(GizmoZ);

    GizmoCornerXY := TGLGizmoCorner.Create(GLScene1);
    GizmoCornerXY.GizmoType := gtPlaneXY;
    GizmoCornerXY.Name := 'GizmoXY';
    GizmoCornerXY.Height := 0.2;
    GizmoCornerXY.Distance := 0.5;

    GLGitzmoDC.AddChild(GizmoCornerXY);

    GizmoCornerXZ := TGLGizmoCorner.Create(GLScene1);
    GizmoCornerXZ.GizmoType := gtPlaneXZ;
    GizmoCornerXZ.Name := 'GizmoXZ';
    GizmoCornerXZ.Height := 0.2;
    GizmoCornerXZ.Distance := 0.5;

    GLGitzmoDC.AddChild(GizmoCornerXZ);

    GizmoCornerYZ := TGLGizmoCorner.Create(GLScene1);
    GizmoCornerYZ.GizmoType := gtPlaneYZ;
    GizmoCornerYZ.Name := 'GizmoYZ';
    GizmoCornerYZ.Height := 0.2;
    GizmoCornerYZ.Distance := 0.5;

    GLGitzmoDC.AddChild(GizmoCornerYZ);

    // Set the Colors for > Windows range
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(Albedo1REdit.text),
      StrToFloatDef(Albedo1GEdit.text), StrToFloatDef(Albedo1BEdit.text));
    AlbedoPanel1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(Albedo2REdit.text),
      StrToFloatDef(Albedo2GEdit.text), StrToFloatDef(Albedo2BEdit.text));
    AlbedoPanel2.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(Albedo3REdit.text),
      StrToFloatDef(Albedo3GEdit.text), StrToFloatDef(Albedo3BEdit.text));
    AlbedoPanel3.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(Albedo4REdit.text),
      StrToFloatDef(Albedo4GEdit.text), StrToFloatDef(Albedo4BEdit.text));
    AlbedoPanel4.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(Albedo5REdit.text),
      StrToFloatDef(Albedo5GEdit.text), StrToFloatDef(Albedo5BEdit.text));
    AlbedoPanel5.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;

    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(SunDuskREdit.text),
      StrToFloatDef(SunDuskGEdit.text), StrToFloatDef(SunDuskBEdit.text));
    DuskPanel.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
      PointMake(StrToFloatDef(SunDawnREdit.text),
      StrToFloatDef(SunDawnGEdit.text), StrToFloatDef(SunDawnBEdit.text));
    DawnPanel.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
      AsWinColor;
    // AlbedoPanel5.Color;
    // Make the Matlib Materials for the Clouds
    For I := 1 to 24 do
      GLCloudsMatLib.Materials.Add;
    ClearImageXYZ; // Clear the PL Mask Image
    ClearAll; // ??????? to rest stuff..or it does too much?
    GLCadencer1.Enabled := True;
  end;
end;

procedure TACloudDemoForm.ClearImageXYZ;
Begin
  XImage.Canvas.Pen.Color := clBlack;
  XImage.Canvas.Pen.Style := psSolid;
  XImage.Canvas.Brush.Color := clBlack;
  XImage.Canvas.Brush.Style := bsSolid;
  XImage.Canvas.FillRect(Rect(0, 0, XImage.Width, XImage.Height));
  YImage.Canvas.Pen.Color := clBlack;
  YImage.Canvas.Pen.Style := psSolid;
  YImage.Canvas.Brush.Color := clBlack;
  YImage.Canvas.Brush.Style := bsSolid;
  YImage.Canvas.FillRect(Rect(0, 0, XImage.Width, XImage.Height));
  ZImage.Canvas.Pen.Color := clBlack;
  ZImage.Canvas.Pen.Style := psSolid;
  ZImage.Canvas.Brush.Color := clBlack;
  ZImage.Canvas.Brush.Style := bsSolid;
  ZImage.Canvas.FillRect(Rect(0, 0, XImage.Width, XImage.Height));
End;

procedure TACloudDemoForm.FormResize(Sender: TObject);
begin
  // change focal so the view will shrink and not just get clipped
  // This lines take cares of auto-zooming.
  // magic numbers explanation :
  // 333 is a form width where things looks good when focal length is 50,
  // ie. when form width is 333, uses 50 as focal length,
  // when form is 666, uses 100, etc...
  // GLCamera1.FocalLength:=Width*50/333;
  GLCamera1.FocalLength := 50 * Width / 705;
end;

procedure TACloudDemoForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TACloudDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: Integer;
begin
  GLCadencer1.Enabled := False;
  Timer1.Enabled := False;
  GPFNodes.CloudsGenerated := False;
  // Clear the Data
  FreeFormDC.DeleteChildren;
  For I := 0 to GPFNodes.CCubeCount - 1 do
  begin
    If GPFNodes.CCubes[I].Generated then
      Setlength(GPFNodes.CCubes[I].Sprites, 0);
  end;
  // Free the Freeform data //FreeFormDC   ShapeNames
  Setlength(GPFNodes.ShapeNames, 0);
  Setlength(GPFNodes.CCubes, 0);
  GLCloudsMatLib.Materials.Clear;
end;

procedure TACloudDemoForm.HelpBtnClick(Sender: TObject);
begin // +'help'
  ShellExecute(Application.Handle, // handle to parent window
    'open', // pointer to string that specifies operation to perform
    PChar(ExtractFilePath(ParamStr(0)) + 'CloudsEditor.htm'),
    // pointer to filename or folder name string
    '', // pointer to string that specifies executable-file parameters
    PChar(ExtractFilePath(ParamStr(0))),
    // pointer to string that specifies default directory
    SW_SHOWNORMAL);
end;

procedure TACloudDemoForm.ScnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;

  if Shift = [ssLeft] then
  begin
    pick := (Scn.Buffer.GetPickedObject(X, Y) as TGLBaseSceneObject);
    // TGLCustomSceneObject);

    if Assigned(pick) then
    begin
      if pick.Name = 'GizmoX' then
      begin
        MovingAxis := maAxisX;
      end
      else if pick.Name = 'GizmoY' then
      begin
        MovingAxis := maAxisY;
      end
      else if pick.Name = 'GizmoZ' then
      begin
        MovingAxis := maAxisZ;
      end
      else if pick.Name = 'GizmoXY' then
      begin
        MovingAxis := maAxisXY;
      end
      else if pick.Name = 'GizmoXZ' then
      begin
        MovingAxis := maAxisXZ;
      end
      else if pick.Name = 'GizmoYZ' then
      begin
        MovingAxis := maAxisYZ;
      end
      else if pick.Tag = 0 then
      begin // do not even SELECT a sprite
        // If Pos('spr', Pick.Name)> 0 then begin end else
        begin
          SelectedObject := pick;
          GLGitzmoDC.Visible := True;
          UpdateGizmo;
        end;
      end;

      lastMouseWorldPos := MouseWorldPos(X, Y);
    end
    else
    begin
      GLGitzmoDC.Visible := False;
      SelectedObject := nil;
      UpdateGizmo;
    end;
  end;
end;

procedure TACloudDemoForm.ScnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  vec1, vec2, newPos, NewSize: TVector;
  CursorPick: TGLBaseSceneObject;
  // TGLCustomSceneObject;
  S2, SS: String;
  GSprite, GCube: Integer;
begin
  if ssRight in Shift then // Shift=[ssRight]
  begin
    if ssCtrl in Shift then
    begin // Reset GLCameraTargetDC
      GLCameraTargetDC.Position.X := 0;
      GLCameraTargetDC.Position.Y := 0;
      GLCameraTargetDC.Position.Z := 0;
    end;
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
    UpdateGizmo;
  end;

  if // (Shift=[ssLeft])
    (ssLeft in Shift) and (SelectedObject <> nil) then
  begin
    newPos := MouseWorldPos(X, Y);
    if (VectorNorm(lastMouseWorldPos) <> 0) then
    begin
      vec1 := newPos;
      vec2 := lastMouseWorldPos;

      case MovingAxis of
        maAxisX:
          begin
            vec1.Y := 0;
            vec1.Z := 0;
            vec1.W := 0;
            vec2.Y := 0;
            vec2.Z := 0;
            vec2.W := 0;
          end;
        maAxisY:
          begin
            vec1.X := 0;
            vec1.Z := 0;
            vec1.W := 0;
            vec2.X := 0;
            vec2.Z := 0;
            vec2.W := 0;
          end;
        maAxisZ:
          begin
            vec1.X := 0;
            vec1.Y := 0;
            vec1.W := 0;
            vec2.X := 0;
            vec2.Y := 0;
            vec2.W := 0;
          end;
      end; // Change Size... but Not a standard name for X,Y,Z
      if ssCtrl in Shift then
      begin
        If Pos('_', SelectedObject.Name) > 0 then
        begin // CloudCube_xxx
          SS := SelectedObject.Name;
          SS := Copy(SS, Pos('_', SS) + 1, Length(SS));
          GCube := Strtoint(SS); // SelectedObject.Size.Translate(
          NewSize := VectorSubtract(vec1, vec2);
          GPFNodes.CCubes[GCube].Size.X := GPFNodes.CCubes[GCube].Size.X +
            NewSize.X;
          GPFNodes.CCubes[GCube].Size.Y := GPFNodes.CCubes[GCube].Size.Y +
            NewSize.Y;
          GPFNodes.CCubes[GCube].Size.Z := GPFNodes.CCubes[GCube].Size.Z +
            NewSize.Z;
          (SelectedObject as TGLCube).CubeWidth :=
            GPFNodes.CCubes[GCube].Size.X;
          (SelectedObject as TGLCube).CubeHeight :=
            GPFNodes.CCubes[GCube].Size.Y;
          (SelectedObject as TGLCube).CubeDepth :=
            GPFNodes.CCubes[GCube].Size.Z;
          WidthEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.X);
          DepthYEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.Y);
          HeightZEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.Z);
        end
        else If Pos('spr', SelectedObject.Name) > 0 then
        begin // spr.name:='spr'+InttoStr(i)+'spr'+InttoStr(j);  ~
          SS := SelectedObject.Name;
          SS := Copy(SS, Pos('spr', SS) + 3, Length(SS));
          S2 := SS;
          SS := Copy(SS, 0, Pos('spr', SS) - 1);
          GCube := Strtoint(SS);
          S2 := Copy(S2, Pos('spr', S2) + 3, Length(S2));
          GSprite := Strtoint(S2);
          NewSize := VectorSubtract(vec1, vec2);
          GPFNodes.CCubes[GCube].Sprites[GSprite].Size.X :=
            GPFNodes.CCubes[GCube].Sprites[GSprite].Size.X + NewSize.X;
          GPFNodes.CCubes[GCube].Sprites[GSprite].Size.Y :=
            GPFNodes.CCubes[GCube].Sprites[GSprite].Size.Y + NewSize.Y;
          GPFNodes.CCubes[GCube].Sprites[GSprite].Size.Z :=
            GPFNodes.CCubes[GCube].Sprites[GSprite].Size.Z + NewSize.Z;
          // Dont know what happened to Y and Z but NEED a switch here
          (SelectedObject as TGLSprite).Width := GPFNodes.CCubes[GCube].Sprites
            [GSprite].Size.X;
          (SelectedObject as TGLSprite).Height := GPFNodes.CCubes[GCube].Sprites
            [GSprite].Size.Z;
        end;
      end
      else
        SelectedObject.Position.Translate(VectorSubtract(vec1, vec2));
    end;
    lastMouseWorldPos := newPos;
    UpdateGizmo;
  end
  else if Shift = [] then
  begin

    CursorPick := (Scn.Buffer.GetPickedObject(X, Y) as TGLBaseSceneObject);
    // TGLCustomSceneObject);//Get some errors on Picking stuff...
    if OldCursorPick <> CursorPick then
    begin
      if (CursorPick <> nil) and (Pos('Gizmo', CursorPick.Name) = 1) then
      begin
        Scn.Cursor := crSizeAll;

        if CursorPick is TGLGizmoArrow then
          (CursorPick as TGLGizmoArrow).Selected := True;

        if CursorPick is TGLGizmoCorner then
        begin
          (CursorPick as TGLGizmoCorner).Selected := True;
          if CursorPick.Name = 'GizmoXY' then
          begin
            GizmoX.Selected := True;
            GizmoY.Selected := True;
          end
          else if CursorPick.Name = 'GizmoXZ' then
          begin
            GizmoX.Selected := True;
            GizmoZ.Selected := True;
          end
          else if CursorPick.Name = 'GizmoYZ' then
          begin
            GizmoY.Selected := True;
            GizmoZ.Selected := True;
          end;
        end;
      end
      else
      begin
        Scn.Cursor := crDefault;
      end;

      if (OldCursorPick is TGLGizmoArrow) then
        (OldCursorPick as TGLGizmoArrow).Selected := False;

      if (OldCursorPick is TGLGizmoCorner) then
      begin
        (OldCursorPick as TGLGizmoCorner).Selected := False;
        if OldCursorPick.Name = 'GizmoXY' then
        begin
          GizmoX.Selected := False;
          GizmoY.Selected := False;
        end
        else if OldCursorPick.Name = 'GizmoXZ' then
        begin
          GizmoX.Selected := False;
          GizmoZ.Selected := False;
        end
        else if OldCursorPick.Name = 'GizmoYZ' then
        begin
          GizmoY.Selected := False;
          GizmoZ.Selected := False;
        end;
      end;
      OldCursorPick := CursorPick;
    end;
  end;
end;

procedure TACloudDemoForm.ScnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pick := nil; // SelectedObject := nil;
end;

procedure TACloudDemoForm.UpdateGizmo;
var
  absDir: TVector;
  S2, SS: String;
  GSprite, GCube: Integer;
begin
  if SelectedObject = nil then
  begin
    StatusBar.Panels[1].text := 'X:';
    StatusBar.Panels[2].text := 'Y:';
    StatusBar.Panels[3].text := 'Z:';
    Exit;
  end;
  absDir := VectorSubtract(SelectedObject.absolutePosition,
    GLCamera1.absolutePosition);
  NormalizeVector(absDir);

  ScaleVector(absDir, 4);

  absDir := VectorAdd(GLCamera1.absolutePosition, absDir);

  // GLCameraDC
  GLGitzmoDC.Position.AsVector := absDir;

  StatusBar.Panels[0].text := SelectedObject.Name;
  StatusBar.Panels[1].text := Format('X: %.2f', [SelectedObject.Position.X]);
  StatusBar.Panels[2].text := Format('Y: %.2f', [SelectedObject.Position.Y]);
  StatusBar.Panels[3].text := Format('Z: %.2f', [SelectedObject.Position.Z]);
  // IF a Cloud Cube..Change the data in the Record to reflect the NEW Position

  If Pos('_', SelectedObject.Name) > 0 then
  begin // CloudCube_xxx
    SS := SelectedObject.Name;
    SS := Copy(SS, Pos('_', SS) + 1, Length(SS));
    GCube := Strtoint(SS);
    GPFNodes.CCubes[GCube].Position.X := SelectedObject.Position.X;
    GPFNodes.CCubes[GCube].Position.Y := SelectedObject.Position.Y;
    GPFNodes.CCubes[GCube].Position.Z := SelectedObject.Position.Z;
    PositionXEdit.text := Floattostr(GPFNodes.CCubes[GCube].Position.X);
    PositionYEdit.text := Floattostr(GPFNodes.CCubes[GCube].Position.Y);
    PositionZEdit.text := Floattostr(GPFNodes.CCubes[GCube].Position.Z);
    WidthEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.X);
    DepthYEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.Y);
    HeightZEdit.text := Floattostr(GPFNodes.CCubes[GCube].Size.Z);
  end;
  // Moving Sprites works..
  If Pos('spr', SelectedObject.Name) > 0 then
  begin
    // spr.name:='spr'+InttoStr(i)+'spr'+InttoStr(j);  ~
    SS := SelectedObject.Name;
    SS := Copy(SS, Pos('spr', SS) + 3, Length(SS));
    S2 := SS;
    SS := Copy(SS, 0, Pos('spr', SS) - 1);
    GCube := Strtoint(SS);
    S2 := Copy(S2, Pos('spr', S2) + 3, Length(S2));
    GSprite := Strtoint(S2);
    GPFNodes.CCubes[GCube].Sprites[GSprite].Position.X :=
      SelectedObject.Position.X;
    GPFNodes.CCubes[GCube].Sprites[GSprite].Position.Y :=
      SelectedObject.Position.Y;
    GPFNodes.CCubes[GCube].Sprites[GSprite].Position.Z :=
      SelectedObject.Position.Z;
  end;
end;

/// /////////////////////////////////
/// //////////////////////////////////
procedure TACloudDemoForm.TimeOnCBClick(Sender: TObject);
begin
  Timer1.Enabled := TimeOnCB.checked;
end;

/// //////////////////////////////////
procedure TACloudDemoForm.Timer1Timer(Sender: TObject);
var // Present: TDateTime;   // Year, Month, Day,
  Hour, Min, Sec, MSec: Word;
begin // Timer turned on by Checkbox on Global page..Normally OFF
  If SunMovingCB.checked then
  begin // CurrentTime
    DecodeTime(Time { Present Now } , Hour, Min, Sec, MSec);
    If Sec = 0 then
      SunMultiplier := SunMultiplier * -1;
    GLSunSphere.Position.X := // Sin
      (((Sec / 60) - 0.5) * 10) * SunMultiplier; // newtime
    SunTimeLabel.Caption := TimeToStr(Time); // DateTimeToStr
  end;

  // update FPS count Caption and reset counter
  StatusBar.Panels[4].text :=
    Format('%.1f FPS - %d S - %d P - %3d CS - %3d F - %3d PL - Depth Sort: %.2f msec',
    [Scn.FramesPerSecond, TotalSprites, GLParticles1.Count - 1,
    PFXCSpriteManager.Particles.ItemCount,
    PFXPolyFogManager.Particles.ItemCount, PFXPLMaskManager.Particles.ItemCount,
    PFXRenderer.LastSortTime]);
  Scn.ResetPerformanceMonitor;
end;

procedure TACloudDemoForm.SingleStepBtnClick(Sender: TObject);
begin
  Scn.Invalidate;
end;

procedure TACloudDemoForm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  I: Integer;
begin
  // For Non-Wheel Mice    or IsKeyDown(Word('W'))
  if IsKeyDown(VK_ESCAPE) then
    Close
  else if IsKeyDown(VK_UP) or IsKeyDown('Z') then // Camera.Move(speed)   else
    GLCamera1.AdjustDistanceToTarget(Power(1.1, -11 / 120))
  else if IsKeyDown(VK_DOWN) or IsKeyDown('C') then
  // Camera.Move(-speed);  else
    GLCamera1.AdjustDistanceToTarget(Power(1.1, 11 / 120))
  else
    // move GLCameraTargetDC
    if IsKeyDown('W') then // Up
      GLCameraTargetDC.Position.Z := GLCameraTargetDC.Position.Z + 0.1
    else if IsKeyDown('S') then
      GLCameraTargetDC.Position.Z := GLCameraTargetDC.Position.Z - 0.1
    else if IsKeyDown('Q') then // Up
      GLCameraTargetDC.Position.Y := GLCameraTargetDC.Position.Y + 0.1
    else if IsKeyDown('E') then
      GLCameraTargetDC.Position.Y := GLCameraTargetDC.Position.Y - 0.1
    else if IsKeyDown('A') then
      GLCameraTargetDC.Position.X := GLCameraTargetDC.Position.X + 0.1
    else if IsKeyDown('D') then
      GLCameraTargetDC.Position.X := GLCameraTargetDC.Position.X - 0.1;

  If UseClouds then
  begin
    CloudNewTime := CloudNewTime + deltaTime;
    if ((CloudNewTime) > CloudAnime) then
    begin // CloudChangeAmount:=0.0001;
      If UseCloudsRange then
      begin
        // Make bigger number
        If CloudRanger < (CloudMinUsed + CloudMinRangeUsed) then
        begin
          CloudRanger := CloudRanger + 1;
          TGLProcTextureNoise(CloudSphere.Material.Texture.Image).MinCut :=
            Round(CloudRanger);
        end
        else // check that it is too small..Reset so above catches
          If TGLProcTextureNoise(CloudSphere.Material.Texture.Image).MinCut <
            (CloudMinUsed - CloudMinRangeUsed) then
          begin
            CloudRanger := TGLProcTextureNoise
              (CloudSphere.Material.Texture.Image).MinCut;
          end
          else
          begin // Reduce it till Caught by above
            TGLProcTextureNoise(CloudSphere.Material.Texture.Image).MinCut :=
              TGLProcTextureNoise(CloudSphere.Material.Texture.Image)
              .MinCut - 1;
          end;
      end;
      If UseCloudsSRange then
      begin
        // Make bigger number
        If CloudSharpRanger < (CloudSharpUsed + CloudSharpRangeUsed) then
        begin
          CloudSharpRanger := CloudSharpRanger + CloudChangeAmount;
          TGLProcTextureNoise(CloudSphere.Material.Texture.Image).NoiseSharpness
            := CloudSharpRanger;
        end
        else // check that it is too small..Reset so above catches
          If TGLProcTextureNoise(CloudSphere.Material.Texture.Image)
            .NoiseSharpness < (CloudSharpUsed - CloudSharpRangeUsed) then
          begin
            CloudSharpRanger := TGLProcTextureNoise
              (CloudSphere.Material.Texture.Image).NoiseSharpness;
          end
          else
          begin // Reduce it till Caught by above
            TGLProcTextureNoise(CloudSphere.Material.Texture.Image)
              .NoiseSharpness := TGLProcTextureNoise
              (CloudSphere.Material.Texture.Image).NoiseSharpness -
              CloudChangeAmount;
          end;
      end;
      // dtmHdsForm.Caption:= floattostr(CloudNewTime) + ' , '+ floattostr(CloudChange);
      CloudChange := CloudChange + CloudChangeAmount;
      CloudNewTime := 0;
      TGLProcTextureNoise(CloudSphere.Material.Texture.Image)
        .NoiseAnimate(CloudChange);
    end;
  end;

  // PFX Renderer uses this?..also Sun Does too, maybe someday
  // Here or after the below?
  If ScnOnCB.checked then
    Scn.Invalidate;

  If (PfxCSTotals > PFXCSpriteManager.Particles.ItemCount) then
    For I := 0 to GPFNodes.CCubeCount - 1 do
    begin
      If GPFNodes.CCubes[I].SpriteType = 3 then
        PFXCSpriteManager.CreateParticles(GPFNodes.CCubes[I].SpriteCount);
    end;
  UpdateGizmo;
end;

procedure TACloudDemoForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  UpdateGizmo;
end;

/// //////////////////////////////////

/// /////////////////////////////////

procedure TACloudDemoForm.NewBtnClick(Sender: TObject);
begin
  ClearAll;
  ACloudDemoForm.Caption := 'Gls Cloud Cube Editor';
end;

procedure TACloudDemoForm.ClearAll;
var
  I: Integer;
  S: String;
begin
  GLCadencer1.Enabled := False;
  GLSunSphere.Position.X := -5;
  GLSunSphere.Position.Y := -5;
  GLSunSphere.Position.Z := 5;
  S := '';
  GPFNodes.RingRadiusName := S;
  GPFNodes.CirrusTypeName := S;
  GPFNodes.ProDomeName := S;
  GPFNodes.SkyDomeName := S;

  // Make GLCloudCube1 and SET all the file stuff
  GLCloudCube.Position.X := 0;
  GLCloudCube.Position.Y := 0;
  GLCloudCube.Position.Z := 0;

  // Clear ALL from SpriteCubeDC and all the other Cloud makers
  ImposterCube := 0;

  CloudCubeDC.DeleteChildren;
  SpriteCubeDC.DeleteChildren;
  PLMaskMenuNumberStored := 0;
  PFXPLMaskManager.Cadencer := nil; // Remove to make the sprites go away
  PFXPolyFogManager.Cadencer := nil;
  PFXCSpriteManager.Cadencer := nil;
  PFXCubeDC.DeleteChildren;
  PfxCSTotals := 0;
  PfxCSOnlyOne := 0;
  ImageSprites := 0;
  GLParticles1.ParticlePoolSize := 0;
  GLParticles1.KillParticles;
  FirstParticle := True;
  ParticleSprite.DeleteChildren;
  FreeFormDC.DeleteChildren;
  TotalSprites := 0;
  ParticleCountTotal := 0;
  GPFNodes.ShapeCount := 0; //
  GPFNodes.ShapeCurrentCount := 0;
  GPFNodes.ShapeMadeCount := 0; //
  GPFNodes.CloudsGenerated := False;
  // Clear the Data
  For I := 0 to GPFNodes.CCubeCount - 1 do
  begin
    If GPFNodes.CCubes[I].Generated then
      Setlength(GPFNodes.CCubes[I].Sprites, 0);
    Setlength(GPFNodes.ShapeNames, 0);
  end; // FreeFormDC   ShapeNames
  Setlength(GPFNodes.CCubes, 0);
  GPFNodes.CCubeCount := 0;
  TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  CurrentCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  CurrentCubeUpDown.Position := 0;
  SpriteTypeRG.ItemIndex := 0;
  SpritesRG.ItemIndex := 0;
  SpritesRGClick(self);
  For I := 1 to 24 do
  begin
    GPFNodes.TextureAlpha[I, 1] := 0;
    GPFNodes.TextureAlpha[I, 2] := 0;
    GPFNodes.TextureAlpha[I, 3] := 0;
    GPFNodes.TextureAlpha[I, 4] := 0;
  end;
  ImageBlendingModeRG.ItemIndex := GPFNodes.TextureAlpha[1, 1];
  ImageTextureModeRG.ItemIndex := GPFNodes.TextureAlpha[1, 2];
  ImageTextureFormatRG.ItemIndex := GPFNodes.TextureAlpha[1, 3];
  ImageImageAlphaRG.ItemIndex := GPFNodes.TextureAlpha[1, 4];
  ImageAlphaGetSettingsRG.ItemIndex := 0;
  ImageAlphaSetSettingsRG.ItemIndex := 0;
  For I := 1 to 24 do
    GPFNodes.TextureType[I] := 0;
  ImageTypeGetSettingsRG.ItemIndex := 0;
  ImageTypeSetSettingsRG.ItemIndex := 0;
  TextureTypeRG.ItemIndex := 0;
  PFXPLMaskManager.Cadencer := GLCadencer1; // Put back Cadencers
  PFXPolyFogManager.Cadencer := GLCadencer1;
  PFXCSpriteManager.Cadencer := GLCadencer1;
  UpdateGizmo;
  Scn.Invalidate;
end;

procedure TACloudDemoForm.LoadBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
  I, J, Version: Integer;
  procedure MakeALoad(Count: Integer);
  var
    NewObject: TGLCube;
    // j:Integer; Tempname:String;
  begin
    // Make a New NodeCube_   GLCloudsDC   NodeCubeDC
    NewObject := (CloudCubeDC.AddNewChild(TGLCube) as TGLCube);
    NewObject.Name := 'CloudCube_' + inttostr(Count);
    // Change to GROUP Color   AsWinColor
    NewObject.Material.FrontProperties.Diffuse.AsWinColor :=
      GPFNodes.CCubes[Count].GroupColor;
    // NewObject.Tag:=strtoint(GroupColorEdit.text);
    { NewObject.Material.FrontProperties.Diffuse.Red  :=1;
      NewObject.Material.FrontProperties.Diffuse.Green:= 0.5;
      NewObject.Material.FrontProperties.Diffuse.Blue := 0; }
    NewObject.Material.FrontProperties.Diffuse.Alpha := 1;
    // The blue one is semitransparent when Gizmoed..
    // trying to make other cubes see through.. not working
    NewObject.Material.Texture.EnvColor.Red := 0;
    NewObject.Material.Texture.EnvColor.Green := 0;
    NewObject.Material.Texture.EnvColor.Blue := 0;
    NewObject.Material.Texture.EnvColor.Alpha := 1; // 0;
    NewObject.Material.Texture.Disabled := True;
    NewObject.Up := CloudCubeDC.Up;
    CloudCubeDC.Children[Count].ResetAndPitchTurnRoll(0, 0, 0);
    NewObject.CubeWidth := (GPFNodes.CCubes[Count].Size.X);
    NewObject.CubeHeight := (GPFNodes.CCubes[Count].Size.Y);
    NewObject.CubeDepth := (GPFNodes.CCubes[Count].Size.Z);
    NewObject.Position.X := GPFNodes.CCubes[Count].Position.X;
    NewObject.Position.Y := GPFNodes.CCubes[Count].Position.Y;
    NewObject.Position.Z := GPFNodes.CCubes[Count].Position.Z;
    CloudCubeDC.Children[I].ResetAndPitchTurnRoll
      (GPFNodes.CCubes[Count].Rotation.X, GPFNodes.CCubes[Count].Rotation.Y,
      GPFNodes.CCubes[Count].Rotation.Z);
  End;

begin
  OpenDialog1.Filter := 'Cloud data (*.dat)|*.dat';
  OpenDialog1.DefaultExt := 'dat';
  OpenDialog1.FileName := '*.dat';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    ClearAll;
    // Load data
    AssignFile(F, OpenDialog1.FileName); { File selected in dialog box }
    Reset(F);
    Readln(F, S);
    If 'Cloud Cubes Version' = S then
    begin
      Readln(F, S);
      Version := Strtoint(S);
      ACloudDemoForm.Caption := 'Gls Cloud Cube Editing: ' +
        ExtractFileName(OpenDialog1.FileName);
      // CloudsGenerated:Boolean;
      GPFNodes.CloudsGenerated := True;
      Readln(F, S);
      GPFNodes.CCubeCount := Strtoint(S);
      Setlength(GPFNodes.CCubes, GPFNodes.CCubeCount);
      // 24 of these
      Readln(F, S);
      GPFNodes.TextureNames[1] := S;
      TextureFilename1.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[2] := S;
      TextureFilename2.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[3] := S;
      TextureFilename3.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[4] := S;
      TextureFilename4.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[5] := S;
      TextureFilename5.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[6] := S;
      TextureFilename6.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[7] := S;
      TextureFilename7.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[8] := S;
      TextureFilename8.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[9] := S;
      TextureFilename9.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[10] := S;
      TextureFilename10.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[11] := S;
      TextureFilename11.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[12] := S;
      TextureFilename12.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[13] := S;
      TextureFilename13.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[14] := S;
      TextureFilename14.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[15] := S;
      TextureFilename15.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[16] := S;
      TextureFilename16.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[17] := S;
      TextureFilename17.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[18] := S;
      TextureFilename18.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[19] := S;
      TextureFilename19.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[20] := S;
      TextureFilename20.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[21] := S;
      TextureFilename21.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[22] := S;
      TextureFilename22.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[23] := S;
      TextureFilename23.text := S;
      Readln(F, S);
      GPFNodes.TextureNames[24] := S;
      TextureFilename24.text := S;

      For I := 1 to 24 do
      begin
        Readln(F, S);
        GPFNodes.TextureAlpha[I, 1] := Strtoint(S);
        Readln(F, S);
        GPFNodes.TextureAlpha[I, 2] := Strtoint(S);
        Readln(F, S);
        GPFNodes.TextureAlpha[I, 3] := Strtoint(S);
        Readln(F, S);
        GPFNodes.TextureAlpha[I, 4] := Strtoint(S);
      end;
      ImageBlendingModeRG.ItemIndex := GPFNodes.TextureAlpha[1, 1];
      ImageTextureModeRG.ItemIndex := GPFNodes.TextureAlpha[1, 2];
      ImageTextureFormatRG.ItemIndex := GPFNodes.TextureAlpha[1, 3];
      ImageImageAlphaRG.ItemIndex := GPFNodes.TextureAlpha[1, 4];
      ImageAlphaGetSettingsRG.ItemIndex := 0;
      ImageAlphaSetSettingsRG.ItemIndex := 0;

      For I := 1 to 24 do
      begin
        Readln(F, S);
        GPFNodes.TextureType[I] := Strtoint(S);
      end;
      TextureTypeRG.ItemIndex := GPFNodes.TextureType[1];
      ImageTypeGetSettingsRG.ItemIndex := 0;
      ImageTypeSetSettingsRG.ItemIndex := 0;

      // Sun data   ...  Not used yet
      // SunPosition: TVector;
      Readln(F, S);
      GPFNodes.SunPosition.X := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.SunPosition.Y := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.SunPosition.Z := StrToFloatDef(S);
      // SunMoving, LensFlare:Boolean;     CurrentTime:TDateTime;
      // RGBA data values needed?   Dawn,Dusk :TColor;
      Readln(F, S);
      GPFNodes.DawnR := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DawnG := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DawnB := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DawnA := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DuskR := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DuskG := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DuskB := StrToFloatDef(S);
      Readln(F, S);
      GPFNodes.DuskA := StrToFloatDef(S);

      // RingRadius : Integer;            CirrusType : Integer;
      Readln(F, S);
      GPFNodes.RingRadius := Strtoint(S);
      RingRadiusEdit.text := inttostr(GPFNodes.RingRadius);
      Readln(F, S);
      GPFNodes.RingRadiusName := S;
      RingRadiusFileNameEdit.text := S;

      Readln(F, S);
      GPFNodes.CirrusType := Strtoint(S);

      Readln(F, S);
      GPFNodes.CirrusTypeName := S;
      CirrusFilenameEdit.text := S;
      CirrusRG.ItemIndex := GPFNodes.CirrusType;
      // Here to load a name if required
      // Should this SET the stuff? ? the itemindex is SET? Does it Do anything?
      // CirrusRG.Itemindex:=GPFNodes.CirrusType;
      // GPFNodes.CirrusTypeName   CirrusFilenameEdit.Text

      // SkyDomeUsed, ProDomeUsed:Integer;
      Readln(F, S);
      GPFNodes.SkyDomeUsed := Strtoint(S);
      Readln(F, S);
      GPFNodes.SkyDomeName := S;
      SkyDomeFilenameEdit.text := S;

      Readln(F, S);
      GPFNodes.ProDomeUsed := Strtoint(S);
      Readln(F, S);
      GPFNodes.ProDomeName := S;
      ProDomeFilenameEdit.text := S;

      For I := 0 to GPFNodes.CCubeCount - 1 do
      begin
        Readln(F, S);
        GPFNodes.CCubes[I].Position.X := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Position.Y := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Position.Z := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Position.W := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Size.X := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Size.Y := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Size.Z := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Size.W := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Rotation.X := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Rotation.Y := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Rotation.Z := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Rotation.W := StrToFloatDef(S);
        // Speed
        Readln(F, S);
        GPFNodes.CCubes[I].SpeedX := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].SpeedY := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].SpeedZ := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Upxyzw := Strtoint(S);
        Readln(F, S);
        If S = 'True' then
          GPFNodes.CCubes[I].ShapeShowObject := True
        else
          GPFNodes.CCubes[I].ShapeShowObject := False;
        Readln(F, S);
        If S = 'True' then
          GPFNodes.CCubes[I].ShapeSizeCube := True
        else
          GPFNodes.CCubes[I].ShapeSizeCube := False;
        Readln(F, S);
        If S = 'True' then
          GPFNodes.CCubes[I].ShapeUpNegative := True
        else
          GPFNodes.CCubes[I].ShapeUpNegative := False;
        Readln(F, S);
        If S = 'True' then
          GPFNodes.CCubes[I].ShapeSpriteSize := True
        else
          GPFNodes.CCubes[I].ShapeSpriteSize := False;
        // GPFNodes.CCubes[i].ShapeResolution :=Strtoint(ShapeResolutionEdit.text);
        Readln(F, S);
        GPFNodes.CCubes[I].ShapeResolution := Strtoint(S);
        // Generated :Boolean;//When Reloaded..will need to MAKE Sprites
        // But NOT Generate the data.. so they can be saved in a DESIRED pattern
        GPFNodes.CCubes[I].Generated := True;
        Readln(F, S);
        GPFNodes.CCubes[I].GroupNumber := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].GroupColor := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].SpriteCount := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].XMax := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].XMin := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].YMax := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].YMin := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].ZMax := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].ZMin := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].RMax := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].RMin := Strtoint(S);

        Readln(F, S);
        GPFNodes.CCubes[I].TextureType := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].CloudType := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].CloudShape := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].CloudPattern := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].SpriteType := Strtoint(S);
        // Not used yet :    Albedo... 5 RGBA sets
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo1R := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo1G := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo1B := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo1A := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo2R := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo2G := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo2B := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo2A := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo3R := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo3G := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo3B := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo3A := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo4R := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo4G := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo4B := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo4A := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo5R := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo5G := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo5B := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].Albedo5A := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].AlphaStart := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].AlphaRate := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].AlphaFormation := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].AlphaDissipation := StrToFloatDef(S);
        //
        Readln(F, S);
        GPFNodes.CCubes[I].PLPSize := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLPLife := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLPInterval := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLDepth := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLScale := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskString := S;
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskStringSize := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskType := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskImageSize := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskXOffset := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskYOffset := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskZOffset := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLMaskMenuNumber := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLColor := Strtoint(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLXImageFileName := S;
        Readln(F, S);
        GPFNodes.CCubes[I].PLPitch := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLRoll := StrToFloatDef(S);
        Readln(F, S);
        GPFNodes.CCubes[I].PLTurn := StrToFloatDef(S);

        // TexturesRecord: Array[1..24]
        For J := 1 to 24 do
        begin
          Readln(F, S);
          If S = 'True' then
            GPFNodes.CCubes[I].TexturesRecord[J].Selected := True
          else
            GPFNodes.CCubes[I].TexturesRecord[J].Selected := False;
          Readln(F, S);
          GPFNodes.CCubes[I].TexturesRecord[J].Permin := Strtoint(S);
          Readln(F, S);
          GPFNodes.CCubes[I].TexturesRecord[J].PerMax := Strtoint(S);
        end;
        // TextureSetRecord: Array[1..16]
        For J := 1 to 16 do
        begin
          Readln(F, S);
          If S = 'True' then
            GPFNodes.CCubes[I].TextureSetRecord[J].Selected := True
          else
            GPFNodes.CCubes[I].TextureSetRecord[J].Selected := False;
        end;
        For J := 1 to 64 do
        begin
          Readln(F, S);
          If S = 'True' then
            GPFNodes.CCubes[I].TextureSet64Record[J].Selected := True
          else
            GPFNodes.CCubes[I].TextureSet64Record[J].Selected := False;
        end;

        Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
        For J := 0 to GPFNodes.CCubes[I].SpriteCount - 1 do
        begin // GPFNodes.CCubes[i].Sprites[J]
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Position.X := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Position.Y := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Position.Z := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Position.W := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Size.X := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Size.Y := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Size.Z := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Size.W := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Rotation.X := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Rotation.Y := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Rotation.Z := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].Rotation.W := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].R := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].G := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].B := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].A := StrToFloatDef(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].TextureName := S;
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].TextureType := Strtoint(S);
          Readln(F, S);
          GPFNodes.CCubes[I].Sprites[J].TextureNumber := Strtoint(S);
        end; // of Sprite
        // Create the CUBE     TextureTypeRG  TextureType   TextureNumber
        MakeALoad(I);
      end;
      Readln(F, S);
      GPFNodes.ShapeCount := Strtoint(S);
      Setlength(GPFNodes.ShapeNames, GPFNodes.ShapeCount);
      For J := 0 to GPFNodes.ShapeCount - 1 do
      begin // FreeFormDC   ShapeNames
        Readln(F, S);
        GPFNodes.ShapeNames[J] := S;
        // Load the File...into the freeform   //FreeFormDC   ShapeNames
        // LoadMakeShape(I,S);
      end;
      // Generate The Clouds.. Using the Old data  False  True
      JustLoaded := True; // Turned on/off Here..Nowhere else, except Create
      ValidateAndSetTexturesBtnClick(self); // Set the Textures
      // i dunno, maybe reset these
      For I := 0 to GPFNodes.CCubeCount - 1 do
        If (GPFNodes.CCubes[I].SpriteType = 5) then
        begin
          PLPSizeEdit.text := Floattostr(GPFNodes.CCubes[I].PLPSize);
          PLPLifeEdit.text := Floattostr(GPFNodes.CCubes[I].PLPLife);
          PLPIntervalEdit.text := Floattostr(GPFNodes.CCubes[I].PLPInterval);
          PLDepthEdit.text := Floattostr(GPFNodes.CCubes[I].PLDepth);
          PLScaleEdit.text := Floattostr(GPFNodes.CCubes[I].PLScale);
          PLCharEdit.text := GPFNodes.CCubes[I].PLMaskString;
          PLCharacterSizeEdit.text :=
            inttostr(GPFNodes.CCubes[I].PLMaskStringSize);
          PLMaskTypeRG.ItemIndex := GPFNodes.CCubes[I].PLMaskType;
          PLMaskImageSizeTB.Position := GPFNodes.CCubes[I].PLMaskImageSize;
          PLMaskImageSizeChange(GPFNodes.CCubes[I].PLMaskImageSize);
          PLMaskMaskXOffsetTB.Position := GPFNodes.CCubes[I].PLMaskXOffset;
          LabelX.Caption := inttostr(GPFNodes.CCubes[I].PLMaskXOffset);
          PLMaskMaskYOffsetTB.Position := GPFNodes.CCubes[I].PLMaskYOffset;
          LabelY.Caption := inttostr(GPFNodes.CCubes[I].PLMaskYOffset);
          PLMaskMaskZOffsetTB.Position := GPFNodes.CCubes[I].PLMaskZOffset;
          LabelZ.Caption := inttostr(GPFNodes.CCubes[I].PLMaskZOffset);
          PLMaskMenuNumberStored := GPFNodes.CCubes[I].PLMaskMenuNumber;
          PLColorPanel.Color := GPFNodes.CCubes[I].PLColor;
          PlLoadXBtn.Caption := GPFNodes.CCubes[I].PLXImageFileName;
          PLPitchEdit.text := Floattostr(GPFNodes.CCubes[I].PLPitch);
          PLRollEdit.text := Floattostr(GPFNodes.CCubes[I].PLRoll);
          PLTurnEdit.text := Floattostr(GPFNodes.CCubes[I].PLTurn);
          If (GPFNodes.CCubes[I].PLMaskType = 1) then
            XImage.Picture.Bitmap.Loadfromfile
              (GPFNodes.CCubes[I].PLXImageFileName)
          else If (GPFNodes.CCubes[I].PLMaskType = 2) then
            // PLMaskMenuSet(MaskMenuNumber,XOffset,YOffset,ZOffset,ImageSize:Integer);
            PLMaskMenuSet(GPFNodes.CCubes[I].PLMaskMenuNumber,
              GPFNodes.CCubes[I].PLMaskXOffset,
              GPFNodes.CCubes[I].PLMaskYOffset,
              GPFNodes.CCubes[I].PLMaskZOffset,
              GPFNodes.CCubes[I].PLMaskImageSize);
        end;
      GenerateBtnClick(self); // GenerateClouds(False);
      Application.ProcessMessages;
      If Version = 2 then // Next Version additions
      begin
      end;
    end;
    CloseFile(F);
    // Set things
    TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
    CurrentCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
    CurrentCubeUpDown.Position := GPFNodes.CCubeCount;
    SetGPFCubeToText(GPFNodes.CCubeCount - 1);
    SetGPFCubeToNodeCube(GPFNodes.CCubeCount - 1);
    GLCadencer1.Enabled := True;
    JustLoaded := False;
  end;
end;

procedure TACloudDemoForm.SaveBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
  I, J: Integer;
begin
  SaveDialog1.Filter := 'Cloud data (*.dat)|*.dat';
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt := 'dat';
  SaveDialog1.FileName := '*.dat';
  if SaveDialog1.Execute then { Display SaveDialog1 box }
  begin // Save data
    ACloudDemoForm.Caption := 'Gls Cloud Cube Editing: ' +
      ExtractFileName(SaveDialog1.FileName);
    GenerateClouds(False); // Make sure Everything has Something
    AssignFile(F, SaveDialog1.FileName); { File selected in dialog box }
    Rewrite(F);
    S := 'Cloud Cubes Version';
    Writeln(F, S);
    S := '1';
    Writeln(F, S); // Version 1

    S := inttostr(GPFNodes.CCubeCount);
    Writeln(F, S);
    For I := 1 to 24 do
    begin
      S := GPFNodes.TextureNames[I];
      Writeln(F, S);
    end;
    For I := 1 to 24 do
    begin
      S := inttostr(GPFNodes.TextureAlpha[I, 1]);
      Writeln(F, S);
      S := inttostr(GPFNodes.TextureAlpha[I, 2]);
      Writeln(F, S);
      S := inttostr(GPFNodes.TextureAlpha[I, 3]);
      Writeln(F, S);
      S := inttostr(GPFNodes.TextureAlpha[I, 4]);
      Writeln(F, S);
    end;
    For I := 1 to 24 do
    begin
      S := inttostr(GPFNodes.TextureType[I]);
      Writeln(F, S);
    end;

    // Sun data   ...  Not used yet
    // SunPosition: TVector;
    S := Floattostr(GPFNodes.SunPosition.X);
    Writeln(F, S);
    S := Floattostr(GPFNodes.SunPosition.Y);
    Writeln(F, S);
    S := Floattostr(GPFNodes.SunPosition.Z);
    Writeln(F, S);
    // SunMoving, LensFlare:Boolean;     CurrentTime:TDateTime;
    // RGBA data values needed?   Dawn,Dusk :TColor;
    S := Floattostr(GPFNodes.DawnR);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DawnG);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DawnB);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DawnA);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DuskR);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DuskG);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DuskB);
    Writeln(F, S);
    S := Floattostr(GPFNodes.DuskA);
    Writeln(F, S);

    // RingRadius : Integer;            CirrusType : Integer;
    GPFNodes.RingRadius := Strtoint(RingRadiusEdit.text);
    S := inttostr(GPFNodes.RingRadius);
    Writeln(F, S);
    GPFNodes.RingRadiusName := RingRadiusFileNameEdit.text;
    S := GPFNodes.RingRadiusName;
    Writeln(F, S);

    GPFNodes.CirrusType := (CirrusRG.ItemIndex);
    S := inttostr(GPFNodes.CirrusType);
    Writeln(F, S);
    GPFNodes.CirrusTypeName := CirrusFilenameEdit.text;
    S := GPFNodes.CirrusTypeName;
    Writeln(F, S);
    // SkyDomeUsed, ProDomeUsed:Integer;
    GPFNodes.SkyDomeUsed := 1;
    S := inttostr(GPFNodes.SkyDomeUsed);
    Writeln(F, S);
    GPFNodes.SkyDomeName := SkyDomeFilenameEdit.text;
    S := GPFNodes.SkyDomeName;
    Writeln(F, S);
    GPFNodes.ProDomeUsed := 1;
    S := inttostr(GPFNodes.ProDomeUsed);
    Writeln(F, S);
    GPFNodes.ProDomeName := ProDomeFilenameEdit.text;
    S := GPFNodes.ProDomeName;
    Writeln(F, S);

    For I := 0 to GPFNodes.CCubeCount - 1 do
    begin
      S := Floattostr(GPFNodes.CCubes[I].Position.X);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Position.Y);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Position.Z);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Position.W);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Size.X);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Size.Y);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Size.Z);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Size.W);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Rotation.X);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Rotation.Y);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Rotation.Z);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Rotation.W);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].SpeedX);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].SpeedY);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].SpeedZ);
      Writeln(F, S);
      // GPFNodes.CCubes[i].Upxyz:=UpxyzRG.itemindex;
      S := inttostr(GPFNodes.CCubes[I].Upxyzw);
      Writeln(F, S);

      If GPFNodes.CCubes[I].ShapeShowObject = True then
        S := 'True'
      else
        S := 'False';
      Writeln(F, S);

      If GPFNodes.CCubes[I].ShapeSizeCube = True then
        S := 'True'
      else
        S := 'False';
      Writeln(F, S);

      If GPFNodes.CCubes[I].ShapeUpNegative = True then
        S := 'True'
      else
        S := 'False';
      Writeln(F, S);

      If GPFNodes.CCubes[I].ShapeSpriteSize = True then
        S := 'True'
      else
        S := 'False';
      Writeln(F, S);
      // GPFNodes.CCubes[i].ShapeResolution :=Strtoint(ShapeResolutionEdit.text);
      S := inttostr(GPFNodes.CCubes[I].ShapeResolution);
      Writeln(F, S);
      // Generated :Boolean;
      // GPFNodes.CCubes[i].Generated:=True;
      //
      S := inttostr(GPFNodes.CCubes[I].GroupNumber);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].GroupColor);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].SpriteCount);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].XMax);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].XMin);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].YMax);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].YMin);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].ZMax);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].ZMin);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].RMax);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].RMin);
      Writeln(F, S);

      S := inttostr(GPFNodes.CCubes[I].TextureType);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].CloudType);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].CloudShape);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].CloudPattern);
      Writeln(F, S);

      S := inttostr(GPFNodes.CCubes[I].SpriteType);
      Writeln(F, S);
      // Albedo... 5 RGBA sets
      S := Floattostr(GPFNodes.CCubes[I].Albedo1R);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo1G);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo1B);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo1A);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo2R);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo2G);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo2B);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo2A);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo3R);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo3G);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo3B);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo3A);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo4R);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo4G);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo4B);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo4A);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo5R);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo5G);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo5B);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].Albedo5A);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].AlphaStart);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].AlphaRate);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].AlphaFormation);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].AlphaDissipation);
      Writeln(F, S);
      //
      S := Floattostr(GPFNodes.CCubes[I].PLPSize);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLPLife);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLPInterval);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLDepth);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLScale);
      Writeln(F, S);
      S := (GPFNodes.CCubes[I].PLMaskString);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskStringSize);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskType);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskImageSize);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskXOffset);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskYOffset);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskZOffset);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLMaskMenuNumber);
      Writeln(F, S);
      S := inttostr(GPFNodes.CCubes[I].PLColor);
      Writeln(F, S);
      S := (GPFNodes.CCubes[I].PLXImageFileName);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLPitch);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLRoll);
      Writeln(F, S);
      S := Floattostr(GPFNodes.CCubes[I].PLTurn);
      Writeln(F, S);

      // TexturesRecord: Array[1..24]
      For J := 1 to 24 do
      begin
        If GPFNodes.CCubes[I].TexturesRecord[J].Selected = True then
          S := 'True'
        else
          S := 'False';
        Writeln(F, S);
        S := inttostr(GPFNodes.CCubes[I].TexturesRecord[J].Permin);
        Writeln(F, S);
        S := inttostr(GPFNodes.CCubes[I].TexturesRecord[J].PerMax);
        Writeln(F, S);
      end;
      // TextureSetRecord: Array[1..16]
      For J := 1 to 16 do
      begin // S='True' then
        If GPFNodes.CCubes[I].TextureSetRecord[J].Selected = True then
          S := 'True'
        else
          S := 'False';
        Writeln(F, S);
      end;
      For J := 1 to 64 do
      begin // S='True' then
        If GPFNodes.CCubes[I].TextureSet64Record[J].Selected = True then
          S := 'True'
        else
          S := 'False';
        Writeln(F, S);
      end;
      // Setlength(GPFNodes.CCubes[i].Sprites,GPFNodes.CCubes[i].SpriteCount);
      For J := 0 to GPFNodes.CCubes[I].SpriteCount - 1 do
      begin // GPFNodes.CCubes[i].Sprites[J]
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Position.X);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Position.Y);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Position.Z);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Position.W);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Size.X);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Size.Y);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Size.Z);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Size.W);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Rotation.X);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Rotation.Y);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Rotation.Z);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].Rotation.W);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].R);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].G);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].B);
        Writeln(F, S);
        S := Floattostr(GPFNodes.CCubes[I].Sprites[J].A);
        Writeln(F, S);
        S := GPFNodes.CCubes[I].Sprites[J].TextureName;
        Writeln(F, S);
        S := inttostr(GPFNodes.CCubes[I].Sprites[J].TextureType);
        Writeln(F, S);
        S := inttostr(GPFNodes.CCubes[I].Sprites[J].TextureNumber);
        Writeln(F, S);
      end; // of Sprite
      // Create the CUBE         MakeABlank(i);
    end; // Cubes
    S := inttostr(GPFNodes.ShapeCount);
    Writeln(F, S);
    For J := 0 to GPFNodes.ShapeCount - 1 do
    begin
      S := GPFNodes.ShapeNames[J];
      Writeln(F, S);
    end;
    // Next Version additions
    { begin
      end; }
    CloseFile(F);
  end;
end;

procedure TACloudDemoForm.SpritesRGClick(Sender: TObject);
begin // Display them...
  Case SpritesRG.ItemIndex of
    1:
      Begin // Turn Off Cubes.. Turn on Sprites
        CloudCubeDC.Visible := False;
        SpriteCubeDC.Visible := True;
        PFXCubeDC.Visible := True;
      end;
    2:
      Begin // Both ON
        CloudCubeDC.Visible := True;
        SpriteCubeDC.Visible := True;
        PFXCubeDC.Visible := True;
      end;
  else
    Begin
      // 0: None  Turn On Cubes.. Turn Off Sprites
      SpriteCubeDC.Visible := False;
      PFXCubeDC.Visible := False;
      CloudCubeDC.Visible := True;
    End;
  End; // Case
end;

procedure TACloudDemoForm.GenerateBtnClick(Sender: TObject);
begin
  GPFNodes.CloudsGenerated := False; // to turn off sprites
  GenerateClouds(False); // Only do New ones...
  GPFNodes.CloudsGenerated := True;
end;

procedure TACloudDemoForm.RegenerateBtnClick(Sender: TObject);
begin
  GPFNodes.CloudsGenerated := False; // to turn off sprites
  GenerateClouds(True); // Redo ALL...
  GPFNodes.CloudsGenerated := True;
end;

// Needed a way to not have to Redo Matlib loading Every time...
procedure TACloudDemoForm.ValidateAndSetTexturesBtnClick(Sender: TObject);
var
  I: Integer;
  S, picName: String;
  bmp32: TGLBitmap32;
begin
  AnyTexturesLoaded := False;
  bmp32 := TGLBitmap32.Create;
  // bmp32.Width:=128;   bmp32.Height:=128;
  For I := 1 to 24 do
  begin
    S := ImagePath + '\' + GPFNodes.TextureNames[I];

    picName := ExtractFileName(S);
    picName := Copy(picName, 0, Length(picName) - 4);
    // Matlib gets 24 in Create.. so these can be by Number, no matter what
    // GLCloudsMatLib.Materials.Add;    //AddTextureMaterial;
    // AddTextureMaterial(picName ,S);
    If FileExists(S) then
    begin // these are  0..XXX The list is 1..24
      AnyTexturesLoaded := True;
      GLCloudsMatLib.Materials[I - 1].Material.Texture.Image.Loadfromfile(S);
      GLCloudsMatLib.Materials[I - 1].Name := picName;
      // ImageBlendingModeRG  ImageTextureModeRG
      // ImageTextureFormatRG    ImageImageAlphaRG
      Case GPFNodes.TextureAlpha[I, 1] of
        0:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode :=
            bmTransparency;
        1:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode := bmAdditive;
          // turns all white
        2:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode :=
            bmAlphaTest50;
        3:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode :=
            bmAlphaTest100;
        4:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode := bmModulate;
        5:
          GLCloudsMatLib.Materials[I - 1].Material.blendingmode := bmOpaque;
      end;
      Case GPFNodes.TextureAlpha[I, 2] of
        0:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureMode :=
            tmReplace; // better image
        1:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureMode :=
            tmModulate; // tint change
        2:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureMode
            := tmDecal;
        3:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureMode
            := tmBlend;
      end;
      Case GPFNodes.TextureAlpha[I, 3] of
        0:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfRGBA; // better image
        1:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfAlpha; // tint change
        2:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat
            := tfRGBA16;
        3:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfIntensity;
        4:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfLuminance;
        5:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfLuminanceAlpha;
        6:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureFormat :=
            tfDefault;
      end;
      { TGLTextureImageAlpha = (tiaDefault, tiaAlphaFromIntensity,
        tiaSuperBlackTransparent, tiaLuminance,
        tiaLuminanceSqrt, tiaOpaque,
        tiaTopLeftPointColorTransparent,
        tiaInverseLuminance, tiaInverseLuminanceSqrt); }
      Case GPFNodes.TextureAlpha[I, 4] of
        0:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaAlphafromIntensity; // better image
        1:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaLuminance;
        2:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaLuminanceSqrt;
        3:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaInverseLuminance;
        4:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaInverseLuminanceSqrt;
        5:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaOpaque;
        6:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaSuperBlackTransparent;
        7:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaTopLeftPointColorTransparent;
        8:
          GLCloudsMatLib.Materials[I - 1].Material.Texture.imageAlpha :=
            tiaDefault;
      end;
      GLCloudsMatLib.Materials[I - 1].Material.Texture.TextureWrap := twNone;
      // twBoth;
      GLCloudsMatLib.Materials[I - 1].Material.Texture.Disabled := False;
      // SprMat:=GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[PfxCSOnlyOne].Sprites[Token].TextureName);
      bmp32.Assign(GLCloudsMatLib.Materials[I - 1].Material.Texture.Image.
        GetBitmap32);
      // prepare AlphaChannel
      case GPFNodes.TextureAlpha[I, 4] of
        0 { tiaAlphaFromIntensity } :
          bmp32.SetAlphaFromIntensity;
        1 { tiaLuminance } :
          bmp32.SetAlphaFromIntensity;
        2 { tiaLuminanceSqrt } :
          begin
            bmp32.SetAlphaFromIntensity;
            bmp32.SqrtAlpha;
          end;
        3 { tiaInverseLuminance } :
          begin
            bmp32.SetAlphaFromIntensity;
            bmp32.InvertAlpha;
          end;
        4 { tiaInverseLuminanceSqrt } :
          begin
            bmp32.SetAlphaFromIntensity;
            bmp32.SqrtAlpha;
            bmp32.InvertAlpha;
          end;
        5 { tiaOpaque } :
          bmp32.SetAlphaToValue(255);
        6 { tiaSuperBlackTransparent } :
          bmp32.SetAlphaTransparentForColor($000000);
        7 { tiaTopLeftPointColorTransparent } :
          bmp32.SetAlphaTransparentForColor(bmp32.Data[0]);
        8 { tiaDefault } :
          ; // nothing to do
      end;
      // bmp32.SetAlphaFromIntensity;
      GLCloudsMatLib.Materials[I - 1].Material.Texture.Image.Assign(bmp32);
    end;
  end;
  bmp32.free;
  If (not AnyTexturesLoaded) then
  begin
    Showmessage('A Texture file must be selected');
    Exit;
  end;
end;

// False: Generate using previous data.. If Available
// True: ALL NEW .. Ignore anything there
// MAKE Data Here.. MAKE Sprite OR Particle or ___ in Their Generation
procedure TACloudDemoForm.GenerateClouds(Regenerate: Boolean);
var
  SetAll: array [1 .. 24] of Integer;
  CubeTexCountTotal, kk, kkk, I, J, K, TexTotal: Integer;
  PositionStep, TempDistance, StepHeight, MaxHeight, MinHeight, TempPosDouble,
    TempPosDouble2, TempPosDouble3, TempDouble, TempDouble2,
    TempDouble3: Double;
  S, picName: String;
  CubeTexUsedArray: Array [1 .. 24] of Double;
  CubeTexCountArray: Array [1 .. 24] of Integer;

begin
  If (not AnyTexturesLoaded) then
  begin
    Showmessage('A Texture file must be selected');
    Exit;
  end;
  GPFNodes.CloudsGenerated := True;
  GLCadencer1.Enabled := False;
  // Delete All the particles for All the Cloud Cubes
  If Regenerate then
  begin
    // CloudCubeDC Holds the Cubes.. Delete ONLY in Clear
    // Delete ONLY on Clear, the Sprites are under SpriteCubeDC
    /// FreeFormDC.DeleteChildren;
    SpriteCubeDC.DeleteChildren;
    PFXCubeDC.DeleteChildren;
    PfxCSTotals := 0;
    PfxCSOnlyOne := 0;
    ImageSprites := 0;
    PFXPLMaskManager.Cadencer := GLCadencer1;
    PFXPolyFogManager.Cadencer := GLCadencer1;
    PFXCSpriteManager.Cadencer := GLCadencer1;
    GLParticles1.ParticlePoolSize := 0;
    ParticleCountTotal := 0;
    TotalSprites := 0;
    GLParticles1.KillParticles;
    ParticleSprite.DeleteChildren;
    FirstParticle := True;
    GPFNodes.ShapeCurrentCount := 0;
    For I := 0 to GPFNodes.CCubeCount - 1 do
    begin
      GPFNodes.CCubes[I].Generated := False;
      Setlength(GPFNodes.CCubes[I].Sprites, 0);
    end;
  end;

  For I := 0 to GPFNodes.CCubeCount - 1 do
  begin
    TexTotal := 0;
    // (not JustLoaded) or  Would make it Redo when Loaded after saved
    If ((Regenerate) or ((not Regenerate) and
      (not GPFNodes.CCubes[I].Generated))) then
    begin // MAKE ALL or [Generate]Make the New ones
      If GPFNodes.CCubes[I].CloudPattern = 4 then // Open File Shapes
        GPFNodes.CCubes[I].SpriteCount :=
          (GPFNodes.CCubes[I].ShapeResolution * GPFNodes.CCubes[I]
          .ShapeResolution * GPFNodes.CCubes[I].ShapeResolution); // 6*6*6=216;
      Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
      // generated  but Not Made yet
      // GPFNodes.CCubes[i].Generated:=True;
      // These are done here to avoid computing Every sprite
      MinHeight := GPFNodes.CCubes[I].Position.Z - GPFNodes.CCubes[I].Size.Z;
      MaxHeight := GPFNodes.CCubes[I].Position.Z + GPFNodes.CCubes[I].Size.Z;
      StepHeight := (MaxHeight - MinHeight) / 5;
      // Make sure 1 has 100 as Max value..
      // it should work if All total to > 100
      For K := 1 to 24 do
        SetAll[K] := -1;
      For K := 1 to 24 do
      begin
        If (GPFNodes.CCubes[I].TexturesRecord[K].Selected and
          (GPFNodes.CCubes[I].TexturesRecord[K].Permin > 0)) then
        begin
          TexTotal := TexTotal + GPFNodes.CCubes[I].TexturesRecord[K].PerMax;
          SetAll[K] := 1;
        end;
        CubeTexUsedArray[K] := 0;
        CubeTexCountArray[K] := 0;
      end;
      If TexTotal < 100 then
        For K := 1 to 24 do
          GPFNodes.CCubes[I].TexturesRecord[K].PerMax := 100;
      CubeTexCountTotal := 0;

      // This makes the data.. the Actual SPrite Particle is Made later
      For J := 0 to GPFNodes.CCubes[I].SpriteCount - 1 do
      begin // GPFNodes.CCubes[i].Sprites[J]
        inc(ParticleCountTotal);
        // Rotation is of TurnAngle ONLY.. Position is Buggered if Cube is rotated...
        TempDouble := GPFNodes.CCubes[I].RMin + Random;
        If (TempDouble > GPFNodes.CCubes[I].RMax) then
          TempDouble := GPFNodes.CCubes[I].RMax;
        GPFNodes.CCubes[I].Sprites[J].Rotation.X := 0;
        GPFNodes.CCubes[I].Sprites[J].Rotation.Y := TempDouble;
        GPFNodes.CCubes[I].Sprites[J].Rotation.Z := 0;
        // X:Pitch Front.back   Y:TurnAngle:Around   Z:Roll:LeftRight

        // If X and Y are NOT equal... makes thin or flat
        If (GPFNodes.CCubes[I].XMin = GPFNodes.CCubes[I].XMax) then
          TempDouble := GPFNodes.CCubes[I].XMax
        else
        begin
          TempDouble := GPFNodes.CCubes[I].XMin + Random;
          If (TempDouble > GPFNodes.CCubes[I].XMax) then
            TempDouble := GPFNodes.CCubes[I].XMax;
        end;
        GPFNodes.CCubes[I].Sprites[J].Size.X := TempDouble;
        If (GPFNodes.CCubes[I].YMin = GPFNodes.CCubes[I].YMax) then
          TempDouble := GPFNodes.CCubes[I].YMax
        else
        begin
          TempDouble := GPFNodes.CCubes[I].YMin + Random;
          If (TempDouble > GPFNodes.CCubes[I].YMax) then
            TempDouble := GPFNodes.CCubes[I].YMax;
        end;
        GPFNodes.CCubes[I].Sprites[J].Size.Y := TempDouble;
        // GPFNodes.CCubes[Count].Size.Z:= ?
        // Depth  Sprite NOT SET...NO DEPTH
        If (GPFNodes.CCubes[I].ZMin = GPFNodes.CCubes[I].ZMax) then
          TempDouble := GPFNodes.CCubes[I].ZMax
        else
        begin
          TempDouble := GPFNodes.CCubes[I].ZMin + Random;
          If (TempDouble > GPFNodes.CCubes[I].ZMax) then
            TempDouble := GPFNodes.CCubes[I].ZMax;
        end;
        GPFNodes.CCubes[I].Sprites[J].Size.Z := TempDouble;

        // CloudTypeRG.. Ignored as this is HeightLevels Determined by User
        // CloudShapeRG ..User should make the Cubes in Proper arrangement
        // CloudPatternRG
        // Random Full
        // Linear XorY
        // Flat XxY
        // Rounded Mass
        // Shapes
        // This is for ALL sprites in a Cube.. but checked per Sprite...?
        Case GPFNodes.CCubes[I].CloudPattern of
          0: // Random
            Begin
              // Position is Buggered if Cube is rotated...
              // Position                to make a +or- number    (CubeSize/2)
              TempDouble := (GPFNodes.CCubes[I].Position.X) +
                ((Random - Random) * (GPFNodes.CCubes[I].Size.X / 2));
              If (GPFNodes.CCubes[I].Position.X >= 0) then
                If (TempDouble > ((GPFNodes.CCubes[I].Position.X) +
                  (GPFNodes.CCubes[I].Size.X / 2))) then
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.X) +
                    (GPFNodes.CCubes[I].Size.X / 2))
                else If (GPFNodes.CCubes[I].Position.X <= 0) then
                  If (TempDouble < ((GPFNodes.CCubes[I].Position.X) -
                    (GPFNodes.CCubes[I].Size.X / 2))) then
                    TempDouble :=
                      ((GPFNodes.CCubes[I].Position.X) -
                      (GPFNodes.CCubes[I].Size.X / 2));
              GPFNodes.CCubes[I].Sprites[J].Position.X := TempDouble;

              TempDouble := (GPFNodes.CCubes[I].Position.Y) +
                ((Random - Random) * (GPFNodes.CCubes[I].Size.Y / 2));
              If (GPFNodes.CCubes[I].Position.Y >= 0) then
                If (TempDouble > ((GPFNodes.CCubes[I].Position.Y) +
                  (GPFNodes.CCubes[I].Size.Y / 2))) then
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.Y) +
                    (GPFNodes.CCubes[I].Size.Y / 2))
                else If (GPFNodes.CCubes[I].Position.Y <= 0) then
                  If (TempDouble < ((GPFNodes.CCubes[I].Position.Y) -
                    (GPFNodes.CCubes[I].Size.Y / 2))) then
                    TempDouble :=
                      ((GPFNodes.CCubes[I].Position.Y) -
                      (GPFNodes.CCubes[I].Size.Y / 2));
              GPFNodes.CCubes[I].Sprites[J].Position.Y := TempDouble;

              TempDouble := (GPFNodes.CCubes[I].Position.Z) +
                ((Random - Random) * (GPFNodes.CCubes[I].Size.Z / 2));
              If (GPFNodes.CCubes[I].Position.Z >= 0) then
                If (TempDouble > ((GPFNodes.CCubes[I].Position.Z) +
                  (GPFNodes.CCubes[I].Size.Z / 2))) then
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.Z) +
                    (GPFNodes.CCubes[I].Size.Z / 2))
                else If (GPFNodes.CCubes[I].Position.Z <= 0) then
                  If (TempDouble < ((GPFNodes.CCubes[I].Position.Z) -
                    (GPFNodes.CCubes[I].Size.Z / 2))) then
                    TempDouble :=
                      ((GPFNodes.CCubes[I].Position.Z) -
                      (GPFNodes.CCubes[I].Size.Z / 2));
              GPFNodes.CCubes[I].Sprites[J].Position.Z := TempDouble;

            End;
          1: // Linear XorY: Check Length..Width Do Longest
            Begin // Position is CENTER: so it needs: Pos-(Size/2)
              If GPFNodes.CCubes[I].Size.X > GPFNodes.CCubes[I].Size.Y then
              begin
                PositionStep := GPFNodes.CCubes[I].Size.X / GPFNodes.CCubes[I]
                  .SpriteCount;
                If (GPFNodes.CCubes[I].Position.X >= 0) then
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.X -
                    (GPFNodes.CCubes[I].Size.X / 2)) + (PositionStep * J))
                else
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.X +
                    (GPFNodes.CCubes[I].Size.X / 2)) - (PositionStep * J));
                GPFNodes.CCubes[I].Sprites[J].Position.X := TempDouble;
                GPFNodes.CCubes[I].Sprites[J].Position.Y :=
                  GPFNodes.CCubes[I].Position.Y +
                  ((Random - Random) * (Random - Random));
                GPFNodes.CCubes[I].Sprites[J].Position.Z :=
                  GPFNodes.CCubes[I].Position.Z;
              end
              else
              begin // GPFNodes.CCubes[i].Size.Y
                PositionStep := GPFNodes.CCubes[I].Size.Y / GPFNodes.CCubes[I]
                  .SpriteCount;
                If (GPFNodes.CCubes[I].Position.Y >= 0) then
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.Y -
                    (GPFNodes.CCubes[I].Size.Y / 2)) + (PositionStep * J))
                else
                  TempDouble :=
                    ((GPFNodes.CCubes[I].Position.Y +
                    (GPFNodes.CCubes[I].Size.Y / 2)) - (PositionStep * J));
                GPFNodes.CCubes[I].Sprites[J].Position.Y := TempDouble;
                GPFNodes.CCubes[I].Sprites[J].Position.X :=
                  GPFNodes.CCubes[I].Position.X +
                  ((Random - Random) * (Random - Random));
                GPFNodes.CCubes[I].Sprites[J].Position.Z :=
                  GPFNodes.CCubes[I].Position.Z;
              end;
            End;
          2: // Flat XxY : Make SqRoot to decide density
            Begin
              PositionStep :=
                (((GPFNodes.CCubes[I].Size.X + GPFNodes.CCubes[I].Size.Y) / 2)
                / GPFNodes.CCubes[I].SpriteCount);
              // Need to make them Fill Cube area
              If (GPFNodes.CCubes[I].Position.Y >= 0) then
                TempDouble :=
                  ((GPFNodes.CCubes[I].Position.Y
                  { -(GPFNodes.CCubes[i].Size.Y/2) } ) +
                  ((Random - Random) * PositionStep * J))
              else
                TempDouble :=
                  ((GPFNodes.CCubes[I].Position.Y
                  { +(GPFNodes.CCubes[i].Size.Y/2) } ) -
                  ((Random - Random) * PositionStep * J));
              GPFNodes.CCubes[I].Sprites[J].Position.Y := TempDouble;
              If (GPFNodes.CCubes[I].Position.X >= 0) then
                TempDouble :=
                  ((GPFNodes.CCubes[I].Position.X
                  { -(GPFNodes.CCubes[i].Size.X/2) } ) +
                  ((Random - Random) * PositionStep * J))
              else
                TempDouble :=
                  ((GPFNodes.CCubes[I].Position.X
                  { +(GPFNodes.CCubes[i].Size.X/2) } ) -
                  ((Random - Random) * PositionStep * J));
              GPFNodes.CCubes[I].Sprites[J].Position.X := TempDouble;
              // Leave Height at Level of the Cube
              GPFNodes.CCubes[I].Sprites[J].Position.Z :=
                GPFNodes.CCubes[I].Position.Z;
            End;
          3: // Rounded Mass: Use Radius to check.. else make it at max
            Begin // GPFNodes.CCubes[i].SpriteCount
              PositionStep :=
                ((GPFNodes.CCubes[I].Size.X + GPFNodes.CCubes[I].Size.Y +
                GPFNodes.CCubes[I].Size.Z) / 3) / 2;
              TempDouble := (GPFNodes.CCubes[I].Position.X) +
                ((Random - Random) *
                PositionStep { (GPFNodes.CCubes[i].Size.X/2) } );
              If (GPFNodes.CCubes[I].Position.X >= 0) then
                TempPosDouble :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.X - TempDouble))
              else
                TempPosDouble :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.X + TempDouble));
              TempDouble2 := (GPFNodes.CCubes[I].Position.Y) +
                ((Random - Random) *
                PositionStep { (GPFNodes.CCubes[i].Size.Y/2) } );
              If (GPFNodes.CCubes[I].Position.Y >= 0) then
                TempPosDouble2 :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.Y - TempDouble2))
              else
                TempPosDouble2 :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.Y + TempDouble2));
              TempDouble3 := (GPFNodes.CCubes[I].Position.Z) +
                ((Random - Random) *
                PositionStep { (GPFNodes.CCubes[i].Size.Z/2 } );
              If (GPFNodes.CCubes[I].Position.Y >= 0) then
                TempPosDouble3 :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.Z - TempDouble3))
              else
                TempPosDouble3 :=
                  trunc(sqr(GPFNodes.CCubes[I].Position.Z + TempDouble3));

              TempDistance := trunc(sqr(TempPosDouble) + sqr(TempPosDouble2) +
                sqr(TempPosDouble3));
              if TempDistance >= PositionStep then
              begin // Showmessage('TOO Big.. Reset to MINimum');
                TempPosDouble := GPFNodes.CCubes[I].Position.X;
                TempPosDouble2 := GPFNodes.CCubes[I].Position.Y;
                TempPosDouble3 := GPFNodes.CCubes[I].Position.Z;
              end;
              GPFNodes.CCubes[I].Sprites[J].Position.X := TempPosDouble;
              // TempDouble;
              GPFNodes.CCubes[I].Sprites[J].Position.Y := TempPosDouble2;
              // TempDouble2;
              GPFNodes.CCubes[I].Sprites[J].Position.Z := TempPosDouble3;
              // TempDouble3;
            End;
          4: // Fill Shape...
            Begin
              // This load a freeform called later to place the sprites
              // LoadMakeShape(GPFNodes.CCubeCount-1,GPFNodes.CCubes[i].ShapeNames[J]);
              // BuildGrid(i); called later to place the sprites
            End;
        end; // Case  CloudPattern

        // Randomly select one of the available Textures
        For K := 1 to 24 do
        begin
          kk := Random(25); // 0..24
          If SetAll[kk] = 1 then
            break
        end; // until K in SetAll;
        If kk < 1 then
          kk := 1; // eliminate 0
        If SetAll[kk] = 1 then
          K := kk
        else
        begin // Get a  valid number if all else fails
          For kkk := 1 to 24 do
            If SetAll[kkk] = 1 then
              K := kkk;
        end;

        { CubeTexUsedArray:Array[1..24] of Double;
          CubeTexCountArray:Array[1..24] of Integer; }
        inc(CubeTexCountTotal);
        CubeTexCountArray[K] := CubeTexCountArray[K] + 1;
        CubeTexUsedArray[K] :=
          (CubeTexCountArray[K] / (GPFNodes.CCubes[I].SpriteCount));
        If (CubeTexUsedArray[K] > (GPFNodes.CCubes[I].TexturesRecord[K].PerMax /
          100)) then
          SetAll[K] := 0;
        GPFNodes.CCubes[I].Sprites[J].TextureName := GLCloudsMatLib.Materials
          [K - 1].Name;
        GPFNodes.CCubes[I].Sprites[J].TextureType := GPFNodes.CCubes[I]
          .TextureType;

        case GPFNodes.CCubes[I].TextureType of // TextureTypeRG
          0: // 1 Per (24) File(s)
            Begin
              GPFNodes.CCubes[I].Sprites[J].TextureNumber := 1;
            End;
          1: // 4 Per (24) File(s)  K is which Texture is Current
            Begin
              If CubeTexCountTotal > 4 then
                GPFNodes.CCubes[I].Sprites[J].TextureNumber := ((J mod 4) + 1)
              else
                GPFNodes.CCubes[I].Sprites[J].TextureNumber :=
                  (((J + CubeTexCountTotal) mod 4) + 1);
            End;
          2: // 16 Set per (1) file..Requires TextureSetRecord #
            Begin // Not really done yet...
              // TexturesRecord: Array[1..24] of TCSTexture;
              // Used ONLY for : 16 Set per (1) file..ONLY ??? uses it
              // TextureSetRecord: Array[1..16] of TCSTexture;
              Repeat
                kkk := (Random(16) + 1);
              until GPFNodes.CCubes[I].TextureSetRecord[kkk].Selected { =True };
              GPFNodes.CCubes[I].Sprites[J].TextureNumber := kkk;
            End;
          3: // 16 random (1)
            Begin
              GPFNodes.CCubes[I].Sprites[J].TextureNumber := Random(16) + 1;
            End;
          4: // SET 64 Set per (1) file..Requires TextureSetRecord #
            Begin
              Repeat
                kkk := Random(64) + 1; // 1..64
              until GPFNodes.CCubes[I].TextureSet64Record[kkk].Selected;
              GPFNodes.CCubes[I].Sprites[J].TextureNumber := kkk;
            End;
          5: // 64 random (1)}
            Begin
              GPFNodes.CCubes[I].Sprites[J].TextureNumber := Random(64) + 1;
            End;
        End; // case

        // Need Height to Choose Which Albedo 1..5 to use
        // spr.Position.Z
        // StepHeight, MaxHeight, MinHeight,
        If (GPFNodes.CCubes[I].Sprites[J].Position.Z < (MinHeight + StepHeight))
        then
        begin
          GPFNodes.CCubes[I].Sprites[J].R := GPFNodes.CCubes[I].Albedo5R;
          GPFNodes.CCubes[I].Sprites[J].G := GPFNodes.CCubes[I].Albedo5G;
          GPFNodes.CCubes[I].Sprites[J].B := GPFNodes.CCubes[I].Albedo5B;
          GPFNodes.CCubes[I].Sprites[J].A := GPFNodes.CCubes[I].Albedo5A;
          // to enable changing Color later
          GPFNodes.CCubes[I].Sprites[J].AlbedoNumber := 5;
        end
        else If (GPFNodes.CCubes[I].Sprites[J].Position.Z <
          (MinHeight + (2 * StepHeight))) then
        begin
          GPFNodes.CCubes[I].Sprites[J].R := GPFNodes.CCubes[I].Albedo4R;
          GPFNodes.CCubes[I].Sprites[J].G := GPFNodes.CCubes[I].Albedo4G;
          GPFNodes.CCubes[I].Sprites[J].B := GPFNodes.CCubes[I].Albedo4B;
          GPFNodes.CCubes[I].Sprites[J].A := GPFNodes.CCubes[I].Albedo4A;
          GPFNodes.CCubes[I].Sprites[J].AlbedoNumber := 4;
        end
        else If (GPFNodes.CCubes[I].Sprites[J].Position.Z <
          (MinHeight + (3 * StepHeight))) then
        begin
          GPFNodes.CCubes[I].Sprites[J].R := GPFNodes.CCubes[I].Albedo3R;
          GPFNodes.CCubes[I].Sprites[J].G := GPFNodes.CCubes[I].Albedo3G;
          GPFNodes.CCubes[I].Sprites[J].B := GPFNodes.CCubes[I].Albedo3B;
          GPFNodes.CCubes[I].Sprites[J].A := GPFNodes.CCubes[I].Albedo3A;
          GPFNodes.CCubes[I].Sprites[J].AlbedoNumber := 3;
        end
        else If (GPFNodes.CCubes[I].Sprites[J].Position.Z <
          (MinHeight + (4 * StepHeight))) then
        begin
          GPFNodes.CCubes[I].Sprites[J].R := GPFNodes.CCubes[I].Albedo2R;
          GPFNodes.CCubes[I].Sprites[J].G := GPFNodes.CCubes[I].Albedo2G;
          GPFNodes.CCubes[I].Sprites[J].B := GPFNodes.CCubes[I].Albedo2B;
          GPFNodes.CCubes[I].Sprites[J].A := GPFNodes.CCubes[I].Albedo2A;
          GPFNodes.CCubes[I].Sprites[J].AlbedoNumber := 2;
        end
        else
        begin // MaxHeight
          GPFNodes.CCubes[I].Sprites[J].R := GPFNodes.CCubes[I].Albedo1R;
          GPFNodes.CCubes[I].Sprites[J].G := GPFNodes.CCubes[I].Albedo1G;
          GPFNodes.CCubes[I].Sprites[J].B := GPFNodes.CCubes[I].Albedo1B;
          GPFNodes.CCubes[I].Sprites[J].A := GPFNodes.CCubes[I].Albedo1A;
          GPFNodes.CCubes[I].Sprites[J].AlbedoNumber := 1;
          // GPFNodes.CCubes[i].StartAlpha;
        end;
      end; // of Sprite

      If ((GPFNodes.CCubes[I].CloudPattern = 4) and
        (GPFNodes.CCubes[I].SpriteType = 2)) then
      begin
        // This loads a freeform
        // Need to Count the Number used..  GPFNodes.ShapeCurrentCount
        If (GPFNodes.ShapeCount > GPFNodes.ShapeMadeCount)
        // Only make New ones if not made yet, use the ones made
          and (GPFNodes.ShapeCurrentCount = GPFNodes.ShapeMadeCount) then
        begin
          { showmessage('x1: '+inttostr(i) + ','+
            inttostr(GPFNodes.ShapeCount) + ','+
            inttostr(GPFNodes.ShapeMadeCount)+ ','+
            inttostr(GPFNodes.ShapeCurrentCount) + ','+
            GPFNodes.ShapeNames[GPFNodes.ShapeMadeCount]); }
          LoadMakeShape(I, GPFNodes.ShapeMadeCount,
            GPFNodes.ShapeNames[GPFNodes.ShapeMadeCount]);
          BuildGrid(I, GPFNodes.ShapeMadeCount);
          inc(GPFNodes.ShapeMadeCount);
          inc(GPFNodes.ShapeCurrentCount);
        end
        else // already made..so use...
        begin
          { showmessage('x2: '+inttostr(i) + ','+
            inttostr(GPFNodes.ShapeCount) + ','+
            inttostr(GPFNodes.ShapeMadeCount) + ','+
            inttostr(GPFNodes.ShapeCurrentCount) + ','+
            GPFNodes.ShapeNames[GPFNodes.ShapeCurrentCount]); }
          BuildGrid(I, GPFNodes.ShapeCurrentCount);
          inc(GPFNodes.ShapeCurrentCount);
        end;
      end; // of CloudPattern
    end; // Regen or not
    // Regenerate checked in Each, they need to do something,
    // even when Regenerate is false
    { 0 Individual Sprites
      1 Particle System
      2 Open File Shapes...
      3 PolyPFX Fog
      4 Image Sprites
      5 PL Mask [only 1]
      6 CSprite [only 1]
      7 Imposter [1?]
      8 Procedural Clouds
      9 Real [Slow] }
    Case GPFNodes.CCubes[I].SpriteType of
      0:
        GenerateSprites(Regenerate, I); // Individual Sprites
      1:
        begin
          GLParticles1.ParticlePoolSize := ParticleCountTotal;
          GenerateParticles(Regenerate, I); // Particle System
        end;
      // Positions Set:  CloudPattern=4  SpriteType=2
      2:
        GenerateSprites(Regenerate, I); // Individual Sprites used for this too
      3:
        GeneratePFXPolyFog(Regenerate, I); // PFX   GeneratePFXFog
      4:
        GenerateImageSprites(Regenerate, I);
      5:
        Begin // procedure TACloudDemoForm.PFXPLSetMask;
          PFXPLSetMask(I);
          GeneratePFXPLMask(Regenerate, I); // GeneratePFXPLMask
        end;
      6:
        GeneratePFXCSprite(Regenerate, I); // PFX
      7:
        GenerateImposter(Regenerate, I);
        // Imposterdemo clone//Forest demo clone
      8:
        GenerateNinianeClouds(Regenerate, I);
      9:
        GenerateRealSlow(Regenerate, I);
    End; // Case
  end; // Cubes//Individual Sprites
  Scn.Invalidate; // Make the changes happen..
  // Invalidate might be Off in Cadencer
  GLCadencer1.Enabled := True;
end;

{ 0 Individual Sprites
  1 Particle System
  2 Open File Shapes...
  3 PolyPFX Fog
  4 Image Sprites
  5 PL Mask [only 1]
  6 CSprite [only 1]
  7 Imposter [1?]
  8 Imposter LOD
  9 Real [Slow] }

// #0 and #2
// False: Generate using previous data.. If Available
// True: ALL NEW .. Ignore anything there
procedure TACloudDemoForm.GenerateSprites(Regenerate: Boolean; I: Integer);
var
  // I,
  J: Integer;
  spr: TGLSprite;
begin
  // For I:=0 to GPFNodes.CCubeCount-1 do
  begin
    // TexTotal:=0;
    If (JustLoaded or (Regenerate) or ((not Regenerate) and
      (not GPFNodes.CCubes[I].Generated))) then
    begin // Make Sprites FROM Info
      // showmessage('allready gone '+Inttostr(i));
      Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
      GPFNodes.CCubes[I].Generated := True;
      For J := 0 to GPFNodes.CCubes[I].SpriteCount - 1 do
      begin
        inc(TotalSprites);
        spr := TGLSprite(SpriteCubeDC.AddNewChild(TGLSprite));
        // Named to allow Gizmo Repositioning  Special symbols
        spr.Name := 'spr' + inttostr(I) + 'spr' + inttostr(J);
        spr.Position.X := GPFNodes.CCubes[I].Sprites[J].Position.X;
        spr.Position.Y := GPFNodes.CCubes[I].Sprites[J].Position.Y;
        spr.Position.Z := GPFNodes.CCubes[I].Sprites[J].Position.Z;
        // GPFNodes.CCubes[i].Sprites[J].Position.W;
        spr.Width := GPFNodes.CCubes[I].Sprites[J].Size.X;
        spr.Height := GPFNodes.CCubes[I].Sprites[J].Size.Z;
        // GPFNodes.CCubes[i].Sprites[J].Size.Z;
        // GPFNodes.CCubes[i].Sprites[J].Size.W;
        spr.ResetAndPitchTurnRoll(GPFNodes.CCubes[I].Sprites[J].Rotation.X,
          GPFNodes.CCubes[I].Sprites[J].Rotation.Y,
          GPFNodes.CCubes[I].Sprites[J].Rotation.Z);
        // GPFNodes.CCubes[i].Sprites[J].Rotation.W;

        spr.Material.MaterialLibrary := GLCloudsMatLib;
        spr.Material.LibMaterialName := GPFNodes.CCubes[I].Sprites[J]
          .TextureName;
        // spr.Material.LibMaterialName:=GLCloudsMatLib.Materials[0].Name;

        // GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[i].Sprites[J].TextureName);
        { spr.Material.FrontProperties.Emission.Color:=PointMake(
          GPFNodes.CCubes[i].Sprites[J].R,
          GPFNodes.CCubes[i].Sprites[J].G,
          GPFNodes.CCubes[i].Sprites[J].B); }
        spr.Material.FrontProperties.Diffuse.Alpha := GPFNodes.CCubes[I]
          .Sprites[J].A;
        spr.Material.FrontProperties.Diffuse.Red := GPFNodes.CCubes[I]
          .Sprites[J].R;
        spr.Material.FrontProperties.Diffuse.Green := GPFNodes.CCubes[I]
          .Sprites[J].G;
        spr.Material.FrontProperties.Diffuse.Blue := GPFNodes.CCubes[I]
          .Sprites[J].B;
        // GPFNodes.CCubes[i].Sprites[J].B:=GPFNodes.CCubes[i].Sprites[J].B;
        spr.TagFloat := GPFNodes.CCubes[I].Sprites[J].AlbedoNumber;
      end;
    end;
  end;
end;

// #1 The OBJECT (DummyCube or Sphere, etc) has an Effects:
// a dialog of a lot of parameters
procedure TACloudDemoForm.GenerateParticles(Regenerate: Boolean; I: Integer);
var
  // K,TexTotal,
  // I,
  J: Integer;
  // TempDouble :Double;
  // S, picName : String;
  // spr : TGLSprite;
begin
  // Make DummyCubes and Sprites for All the Cloud Cubes
  // Make 1 Base and ALL THE REST under IT   GLParticles1
  // If (JustLoaded or Regenerate) then   FirstOne:=True else FirstOne:=False;
  begin
    // For I:=0 to GPFNodes.CCubeCount-1 do
    begin
      If (JustLoaded or (Regenerate) or ((not Regenerate) and
        (not GPFNodes.CCubes[I].Generated))) then
      begin // Make Sprites FROM Info
        Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
        GPFNodes.CCubes[I].Generated := True;
        For J := 0 to GPFNodes.CCubes[I].SpriteCount - 1 do
        begin
          If FirstParticle
          // and ((I=GPFNodes.CCubeCount-1)and (J =GPFNodes.CCubes[GPFNodes.CCubeCount-1].SpriteCount-1))
          then
            with TGLSprite(GLParticles1.CreateParticle) do
            begin
              FirstParticle := False; // ParticleSprite
              ParticleSprite.Position.X := GPFNodes.CCubes[I].Sprites[J]
                .Position.X;
              ParticleSprite.Position.Y := GPFNodes.CCubes[I].Sprites[J]
                .Position.Y;
              ParticleSprite.Position.Z := GPFNodes.CCubes[I].Sprites[J]
                .Position.Z;
              ParticleSprite.Width := GPFNodes.CCubes[I].Sprites[J].Size.X;
              ParticleSprite.Height := GPFNodes.CCubes[I].Sprites[J].Size.Z;
              // 1 is Depth
              ParticleSprite.Material.MaterialLibrary := GLCloudsMatLib;
              ParticleSprite.Material.LibMaterialName := GPFNodes.CCubes[I]
                .Sprites[J].TextureName;
              { ParticleSprite.Material.FrontProperties.Emission.Color:=PointMake(
                GPFNodes.CCubes[i].Sprites[J].R,
                GPFNodes.CCubes[i].Sprites[J].G,
                GPFNodes.CCubes[i].Sprites[J].B); }
              ParticleSprite.Material.FrontProperties.Diffuse.Alpha :=
                GPFNodes.CCubes[I].Sprites[J].A;
              ParticleSprite.Material.FrontProperties.Diffuse.Red :=
                GPFNodes.CCubes[I].Sprites[J].R;
              ParticleSprite.Material.FrontProperties.Diffuse.Green :=
                GPFNodes.CCubes[I].Sprites[J].G;
              ParticleSprite.Material.FrontProperties.Diffuse.Blue :=
                GPFNodes.CCubes[I].Sprites[J].B;
              ParticleSprite.TagFloat := GPFNodes.CCubes[I].Sprites[J]
                .AlbedoNumber;
            end
          else
            with TGLSprite(GLParticles1.CreateParticle) do
            begin
              // spr:=TGLSprite(ParticleSprite.AddNewChild(TGLSprite));

              // Named to allow Gizmo Repositioning  Special symbols
              name := 'spr' + inttostr(I) + 'spr' + inttostr(J);
              Position.X := GPFNodes.CCubes[I].Sprites[J].Position.X;
              Position.Y := GPFNodes.CCubes[I].Sprites[J].Position.Y;
              Position.Z := GPFNodes.CCubes[I].Sprites[J].Position.Z;
              // GPFNodes.CCubes[i].Sprites[J].Position.W;
              Width := GPFNodes.CCubes[I].Sprites[J].Size.X;
              Height := GPFNodes.CCubes[I].Sprites[J].Size.Y;
              // GPFNodes.CCubes[i].Sprites[J].Size.Z;
              // GPFNodes.CCubes[i].Sprites[J].Size.W;
              ResetAndPitchTurnRoll(GPFNodes.CCubes[I].Sprites[J].Rotation.X,
                GPFNodes.CCubes[I].Sprites[J].Rotation.Y,
                GPFNodes.CCubes[I].Sprites[J].Rotation.Z);
              // GPFNodes.CCubes[i].Sprites[J].Rotation.W;

              Material.MaterialLibrary := GLCloudsMatLib;
              Material.LibMaterialName := GPFNodes.CCubes[I].Sprites[J]
                .TextureName;
              // spr.Material.LibMaterialName:=GLCloudsMatLib.Materials[0].Name;

              // GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[i].Sprites[J].TextureName);
              { Material.FrontProperties.Emission.Color:=PointMake(
                GPFNodes.CCubes[i].Sprites[J].R,
                GPFNodes.CCubes[i].Sprites[J].G,
                GPFNodes.CCubes[i].Sprites[J].B); }
              Material.FrontProperties.Diffuse.Alpha := GPFNodes.CCubes[I]
                .Sprites[J].A;
              Material.FrontProperties.Diffuse.Red := GPFNodes.CCubes[I]
                .Sprites[J].R;
              Material.FrontProperties.Diffuse.Green := GPFNodes.CCubes[I]
                .Sprites[J].G;
              Material.FrontProperties.Diffuse.Blue := GPFNodes.CCubes[I]
                .Sprites[J].B;
              // GPFNodes.CCubes[i].Sprites[J].B:=GPFNodes.CCubes[i].Sprites[J].B;
              TagFloat := GPFNodes.CCubes[I].Sprites[J].AlbedoNumber;
            end;
        end;
      end;
    end;
  end;
End;
(* Part of the TGLParticales Events
  procedure TACloudDemoForm.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
  begin
  // this event is called when a particle is activated,
  // ie. just before it will be rendered
  {   with TGLSprite(particle) do begin
  with Material.FrontProperties do begin
  // we pick a random color
  Emission.Color:=PointMake(Random, Random, Random);
  // our halo starts transparent
  Diffuse.Alpha:=0.5;
  end;
  // this is our "birth time"
  //      TagFloat:=GLCadencer1.CurrentTime;
  end;}
  end;
*)

// #3 PolyPFX Fog
// Steam
// Steam:X=1.75,Y=0.05,Z=-2.7,SX=1.5,SZ=1,TimeOffset=0,TimeOn=5,TimeOff=1,Strength=15
// Steam:X=1,Y=0.05,Z=2.5,SX=0.5,SZ=0.5,TimeOffset=0.1,TimeOn=2,TimeOff=8,Strength=60
// Steam:X=1,Y=0.05,Z=3.5,SX=0.5,SZ=0.5,TimeOffset=2.2,TimeOn=2,TimeOff=8,Strength=60
// Steam:X=1,Y=0.05,Z=4.5,SX=0.5,SZ=0.5,TimeOffset=3.9,TimeOn=2,TimeOff=8,Strength=60
procedure TACloudDemoForm.GeneratePFXPolyFog(Regenerate: Boolean; I: Integer);
var
  // J,//  I:Integer;
  source: TGLSourcePFXEffect;
  sprCube: TGLDummyCube;
begin
  If (JustLoaded or (Regenerate) or ((not Regenerate) and
    (not GPFNodes.CCubes[I].Generated))) then
  begin // Make Sprites FROM Info
    GPFNodes.CCubes[I].Generated := True;
    sprCube := TGLDummyCube(PFXCubeDC.AddNewChild(TGLDummyCube));
    PFXPolyFogManager.LifeColors[0].LifeTime := 2;
    // smaller=faster, larger=keeps making more
    PFXPolyFogManager.ParticleSize := 0.7;
    PFXPolyFogManager.Cadencer := GLCadencer1;
    PFXPolyFogManager.Renderer := PFXRenderer;
    // PFXPolyFogManager.Acceleration.Z:=-1;
    source := GetOrCreateSourcePFX(sprCube, 'PFXPolyFogSource' + inttostr(I));
    // PFX Source
    source.Manager := PFXPolyFogManager;
    source.Enabled := True;
    source.ParticleInterval := 0.03; // density distance of particles
    source.InitialPosition.X := GPFNodes.CCubes[I].Position.X;
    source.InitialPosition.Y := GPFNodes.CCubes[I].Position.Y;
    source.InitialPosition.Z := GPFNodes.CCubes[I].Position.Z;
    source.PositionDispersion := VectorLength(GPFNodes.CCubes[I].Size) * 0.5;
    // 0.3;//smaller=all in 1 place
    // source.VelocityDispersion:=VectorLength(GPFNodes.CCubes[i].Size)*0.0007;
    // source.InitialVelocity.Y:=0;//Sqrt(GPFNodes.CCubes[i].SpeedY);
    { Setlength(GPFNodes.CCubes[i].Sprites,GPFNodes.CCubes[i].SpriteCount);
      GPFNodes.CCubes[i].Generated:=True;
      PfxCSTotal:=PfxTotal+GPFNodes.CCubes[i].SpriteCount;
      GLCustomSpritePFXManager1.CreateParticles(GPFNodes.CCubes[i].SpriteCount); }
  end;
end;

// #4 GenerateImageSprites(Regenerate,I);
procedure TACloudDemoForm.GenerateImageSprites(Regenerate: Boolean; I: Integer);
var
  scale1, scaled: single;
  scalex, scaley, scalexi, scaleyi, scaler, scalei: Integer;
  sprCount, X, Y: Integer;
  spr: TGLSprite;
  bmp: TBitmap;
  bmp32: TGLBitmap32;
  SprMat: TGLLibMaterial; // TGLLibMaterial
begin // ImageSprites
  If (JustLoaded or (Regenerate) or ((not Regenerate) and
    (not GPFNodes.CCubes[I].Generated))) then
  begin // Make Sprites FROM Info
    inc(ImageSprites); // used to get the Image name
    // bmp := tbitmap.Create;
    bmp32 := TGLBitmap32.Create;
    SprMat := GLCloudsMatLib.LibMaterialByName
      (GPFNodes.CCubes[I].Sprites[0].TextureName);
    bmp32.Assign(SprMat.Material.Texture.Image.GetBitmap32);
    bmp := bmp32.Create32BitsBitmap;
    // bmp.LoadFromFile(ImagePath+'\'+ GPFNodes.TextureNames[ImageSprites]);
    // Scale it all somehow
    scaler := // 30;     //25*25=625  50=2500 75=5625 100=10,000
      GPFNodes.CCubes[I].ShapeResolution;
    // Better to Resize the Bitmap and just use it?
    If bmp.Width > bmp.Height then
    begin
      scale1 := scaler / bmp.Width;
      scalei := bmp.Width;
    end
    else
    begin
      scale1 := scaler / bmp.Height;
      scalei := bmp.Height;
    end;
    scalex := Round(bmp.Width * scale1);
    scaley := Round(bmp.Height * scale1);
    scaled := (scale1 / scaler) * (scaler / 2); // sqrt(scaler);
    sprCount := 0;
    Setlength(GPFNodes.CCubes[I].Sprites, scaler * scaler);
    ShapeProgressBar.Max := scaley - 1;
    for Y := 0 to scaley - 1 do
    begin
      ShapeProgressBar.Position := Y;
      Application.ProcessMessages;
      scaleyi := Round(Y / scaler * scalei);
      for X := 0 to scalex - 1 do
      begin
        scalexi := Round((X / scaler * scalei));
        If GetRValue(bmp.Canvas.Pixels[scalexi, (bmp.Height - 1) - scaleyi]) > 0
        then
        begin
          inc(TotalSprites);
          inc(sprCount);
          spr := TGLSprite(SpriteCubeDC.AddNewChild(TGLSprite));
          spr.Name := 'spr' + inttostr(I) + 'spr' + inttostr(sprCount - 1);
          spr.Material.MaterialOptions := [moNoLighting];
          spr.Material.FrontProperties.Diffuse.AsWinColor :=
            bmp.Canvas.Pixels[scalexi, (bmp.Height - 1) - scaleyi];
          GPFNodes.CCubes[I].Sprites[sprCount - 1].R :=
            spr.Material.FrontProperties.Diffuse.Red;
          GPFNodes.CCubes[I].Sprites[sprCount - 1].G :=
            spr.Material.FrontProperties.Diffuse.Green;
          GPFNodes.CCubes[I].Sprites[sprCount - 1].B :=
            spr.Material.FrontProperties.Diffuse.Blue;
          spr.Position.X := GPFNodes.CCubes[I].Position.X -
            (GPFNodes.CCubes[I].Size.X / 2) + (X / scaler); // ((1/x+1));
          GPFNodes.CCubes[I].Sprites[sprCount - 1].Position.X :=
            spr.Position.X;
          spr.Position.Z := GPFNodes.CCubes[I].Position.Z -
            (GPFNodes.CCubes[I].Size.Z / 2) + (Y / scaler); // ((1/y+1));
          GPFNodes.CCubes[I].Sprites[sprCount - 1].Position.Z :=
            spr.Position.Z;
          spr.Position.Y := GPFNodes.CCubes[I].Position.Y
          { +(1/(spr.Material.FrontProperties.Diffuse.Red+1)) };
          GPFNodes.CCubes[I].Sprites[sprCount - 1].Position.Y :=
            spr.Position.Y;
          spr.Scale.SetVector(scaled, scaled, scaled);
          GPFNodes.CCubes[I].Sprites[sprCount - 1].Size.X := spr.Width;
          GPFNodes.CCubes[I].Sprites[sprCount - 1].Size.Y := spr.Height;
        end;
      end; // x
    end; // y
    GPFNodes.CCubes[I].SpriteCount := sprCount + 1;
    Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
    GPFNodes.CCubes[I].Generated := True;
    bmp.free;
    bmp32.free;
  end; // if regen ..do it
  ShapeProgressBar.Position := 0;
  ShapeProgressBar.Max := 100;
end;

// #5 PL Mask
procedure TACloudDemoForm.GeneratePFXPLMask(Regenerate: Boolean; I: Integer);
var
  source: TGLSourcePFXEffect;
  sprCube: TGLDummyCube;
begin // PLPSize  PLPLife  PLPInterval  PLMaskImageSize
  If (JustLoaded or (Regenerate) or ((not Regenerate) and
    (not GPFNodes.CCubes[I].Generated))) then
  begin // Make Sprites FROM Info
    // If JustLoaded then showmessage('it is JustLoaded');
    GPFNodes.CCubes[I].Generated := True;
    sprCube := TGLDummyCube(PFXCubeDC.AddNewChild(TGLDummyCube));
    sprCube.Position.X := GPFNodes.CCubes[I].Position.X;
    sprCube.Position.Y := GPFNodes.CCubes[I].Position.Y;
    sprCube.Position.Z := GPFNodes.CCubes[I].Position.Z;
    // These should be Saved..Create
    // PFXPLMaskManager
    // GLEParticleMasksManager1
    // ParticleMasks[].TGLEParticleMask
    // Name    mask ie ParticleMask1
    // Set a MaterialLibrary : MatLib
    // these are in MatLib: XMask, YMask, ZMask
    PFXPLMaskManager.ParticleSize := GPFNodes.CCubes[I].PLPSize;
    // StrToFloatDef(PLPSizeEdit.Text,0.3);
    // Color Inner <0.700 0.700 0.900 0.500>
    // LifeColors Inner clrNeonBlue  clrLightBlue //1  0.7  PLPLifeEdit
    PFXPLMaskManager.LifeColors[0].LifeTime := GPFNodes.CCubes[I].PLPLife;
    // StrToFloatDef(PLPLifeEdit.Text,1.0);
    PFXPLMaskManager.ColorOuter.AsWinColor := GPFNodes.CCubes[I].PLColor;
    // PLColorPanel.Color;
    source := GetOrCreateSourcePFX(sprCube, 'PFXPLMaskSource' + inttostr(I));
    // PFX Source
    source.Manager := PFXPLMaskManager;
    source.Enabled := True;
    source.ParticleInterval := GPFNodes.CCubes[I].PLPInterval;
    // StrToFloatDef(PLPIntervalEdit.Text,0.003);  //0.03;     0.7;  //0.0009;

    { source.InitialPosition.X:=GPFNodes.CCubes[i].Position.X;
      source.InitialPosition.Y:=GPFNodes.CCubes[i].Position.Y;
      source.InitialPosition.Z:=GPFNodes.CCubes[i].Position.Z; }

    // source.PositionDispersion:=VectorLength(GPFNodes.CCubes[i].Size)*0.2;
    // source.VelocityDispersion:=VectorLength(GPFNodes.CCubes[i].Size)*0.7;
    // source.InitialVelocity.Y:=Sqrt(GPFNodes.CCubes[i].SpeedY);

    { Setlength(GPFNodes.CCubes[i].Sprites,GPFNodes.CCubes[i].SpriteCount);
      GPFNodes.CCubes[i].Generated:=True;
      PfxCSTotal:=PfxTotal+GPFNodes.CCubes[i].SpriteCount;
      PFXPLMaskManager.CreateParticles(GPFNodes.CCubes[i].SpriteCount); }
  end;
end;

// GLEParticleMasksManager1:TGLEParticleMasksManager;
procedure TACloudDemoForm.PFXPLMaskManagerCreateParticle(Sender: TObject;
  aParticle: TGLParticle);
var
  I: Integer;
  Particle: TGLParticle;
begin
  if PFXPLMaskManager.Particles.ItemCount > 0 then
    for I := 0 to PFXPLMaskManager.Particles.ItemCount - 1 do
    begin
      Particle := PFXPLMaskManager.Particles.Items[I];
      // first we find the particles that are tagged (since before particle creation, the position is overridden)
      if Particle.Tag = 1 then
      begin
        { if CheckBox1.Checked then
          GLEParticleMasksManager1.SetParticlePositionFromMaskTarget(Particle, 'mask', Sphere)
          else }
        GLEParticleMasksManager1.SetParticlePositionFromMask(Particle, 'mask');
        { GLEParticleMasksManager1.SetParticlePositionFromMaskxyz(Particle, 'mask',
          GPFNodes.CCubes[0].Position.X,
          GPFNodes.CCubes[0].Position.Y,
          GPFNodes.CCubes[0].Position.Z ); }
        Particle.Tag := 0;
      end;
    end;
  // we tag the new particle for when another particle is made so we know this one needs updating aswell
  aParticle.Tag := 1;
end;

// Get info data and SET to Mask
// Called AFTER a Cube is Set
procedure TACloudDemoForm.PFXPLSetMask(CubeNumber: Integer);
var
  Rect: TRect;
  Mat: TGLLibMaterial;
  MaskString: String; // Char;
  PLScale: single;
  Scalar, Depth, MaskStringSize: Integer;
begin
  Scalar := GPFNodes.CCubes[CubeNumber].PLMaskImageSize;
  // PLMaskImageSizeTB.Position;
  Depth := GPFNodes.CCubes[CubeNumber].PLDepth;
  // StrToIntDef(PLDepthEdit.Text, 3);
  Case PLMaskTypeRG.ItemIndex of
    0: // Alphabet
      Begin
        MaskString := GPFNodes.CCubes[CubeNumber].PLMaskString;
        // PLCharEdit.Text;
        MaskStringSize := GPFNodes.CCubes[CubeNumber].PLMaskStringSize;
        // StrToIntDef(PLCharacterSizeEdit.Text, 16);
        Rect.Left := 0;
        Rect.Top := 0;
        Rect.Bottom := Scalar;
        Rect.Right := Scalar;
        XImage.Canvas.Font.Name := 'Arial';
        XImage.Canvas.Font.Size := MaskStringSize;
        // 16;
        XImage.Canvas.Font.Color := clWhite;
        XImage.Canvas.Pen.Color := clBlack;
        XImage.Canvas.Pen.Style := psSolid;
        XImage.Canvas.Brush.Color := clBlack;
        XImage.Canvas.Brush.Style := bsSolid;

        XImage.Canvas.FillRect(Rect);
        XImage.Canvas.TextOut
          (Round((Scalar - XImage.Canvas.TextWidth(MaskString)) / 2),
          Round((Scalar - XImage.Canvas.TextHeight(MaskString) - 2) / 2),
          MaskString);
        Mat := MatLib.LibMaterialByName('XMask');
        with Mat.Material.Texture.Image as TGLPersistentImage do
        begin
          Picture.Bitmap.Height := Scalar;
          Picture.Bitmap.Width := Scalar;
          Picture.Bitmap.Canvas.Draw(0, 0, XImage.Picture.Graphic);
        end;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.X :=
          GPFNodes.CCubes[CubeNumber].Position.X;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Y :=
          GPFNodes.CCubes[CubeNumber].Position.Y;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Z :=
          GPFNodes.CCubes[CubeNumber].Position.Z;
        // PLScale  PLPitch PLRoll PLTurn
        PLScale := GPFNodes.CCubes[CubeNumber].PLScale;
        // StrToFloatDef(PLScaleEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.X := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Y := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Z := PLScale;

        GLEParticleMasksManager1.ParticleMaskByName('mask').PitchAngle :=
          GPFNodes.CCubes[CubeNumber].PLPitch;
        // StrToFloatDef(PLPitchEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').RollAngle :=
          GPFNodes.CCubes[CubeNumber].PLRoll;
        // StrToFloatDef(PLRollEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').TurnAngle :=
          GPFNodes.CCubes[CubeNumber].PLTurn;
        // StrToFloatDef(PLTurnEdit.Text, 0);
      end;
    1: // Loaded Image
      begin
        Mat := MatLib.LibMaterialByName('XMask');
        with Mat.Material.Texture.Image as TGLPersistentImage do
        begin
          Picture.Bitmap.Height := XImage.Height; // Scalar;
          Picture.Bitmap.Width := XImage.Width; // Scalar;
          Picture.Bitmap.Canvas.Draw(0, 0, XImage.Picture.Graphic);
        end;
        PLScale := GPFNodes.CCubes[CubeNumber].PLScale;
        // StrToFloatDef(PLScaleEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.X := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Y := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Z := PLScale;

        GLEParticleMasksManager1.ParticleMaskByName('mask').PitchAngle :=
          GPFNodes.CCubes[CubeNumber].PLPitch;
        // StrToFloatDef(PLPitchEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').RollAngle :=
          GPFNodes.CCubes[CubeNumber].PLRoll;
        // StrToFloatDef(PLRollEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').TurnAngle :=
          GPFNodes.CCubes[CubeNumber].PLTurn;
        // StrToFloatDef(PLTurnEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.X :=
          GPFNodes.CCubes[CubeNumber].Position.X;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Y :=
          GPFNodes.CCubes[CubeNumber].Position.Y;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Z :=
          GPFNodes.CCubes[CubeNumber].Position.Z;
      End;
    2: // Popup Menu painted..
      begin
        Mat := MatLib.LibMaterialByName('XMask');
        with Mat.Material.Texture.Image as TGLPersistentImage do
        begin
          Picture.Bitmap.Height := XImage.Height; // Scalar;
          Picture.Bitmap.Width := XImage.Width; // Scalar;
          Picture.Bitmap.Canvas.Draw(0, 0, XImage.Picture.Graphic);
        end;

        PLScale := GPFNodes.CCubes[CubeNumber].PLScale;
        // StrToFloatDef(PLScaleEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.X := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Y := PLScale;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Scale.Z := PLScale;

        GLEParticleMasksManager1.ParticleMaskByName('mask').PitchAngle :=
          GPFNodes.CCubes[CubeNumber].PLPitch;
        // StrToFloatDef(PLPitchEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').RollAngle :=
          GPFNodes.CCubes[CubeNumber].PLRoll;
        // StrToFloatDef(PLRollEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').TurnAngle :=
          GPFNodes.CCubes[CubeNumber].PLTurn;
        // StrToFloatDef(PLTurnEdit.Text, 0);
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.X :=
          GPFNodes.CCubes[CubeNumber].Position.X;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Y :=
          GPFNodes.CCubes[CubeNumber].Position.Y;
        GLEParticleMasksManager1.ParticleMaskByName('mask').Position.Z :=
          GPFNodes.CCubes[CubeNumber].Position.Z;
      End;
  End; // case
  // this is a very recent implementation, the ability to generate other masks from 1 mask, so it satisfies
  // the requirements for the particle mask manager. useful for text and making basic shapes (cylinders etc)
  GLEParticleMasksManager1.ParticleMaskByName('mask').GenerateMaskFromProjection
    (pptXMask, pptYMask, Depth);
  GLEParticleMasksManager1.ParticleMaskByName('mask').GenerateMaskFromProjection
    (pptXMask, pptZMask, Depth);
  Mat := MatLib.LibMaterialByName('YMask');
  with Mat.Material.Texture.Image as TGLPersistentImage do
  begin
    Picture.Bitmap.Height := XImage.Height; // Scalar;
    Picture.Bitmap.Width := XImage.Width; // Scalar;
    YImage.Canvas.Draw(0, 0, Picture.Graphic);
  end;

  Mat := MatLib.LibMaterialByName('ZMask');
  with Mat.Material.Texture.Image as TGLPersistentImage do
  begin
    Picture.Bitmap.Height := XImage.Height; // Scalar;
    Picture.Bitmap.Width := XImage.Width; // Scalar;
    ZImage.Canvas.Draw(0, 0, Picture.Graphic);
  end;
end;

// The rest  of PL Mask is Interactive :Before being Added to a cube
procedure TACloudDemoForm.PLColorPanelClick(Sender: TObject);
begin
  GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
    PLColorPanel.Color;
  ColorDialog1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
    AsWinColor;
  if ColorDialog1.Execute then
  Begin
    PLColorPanel.Color := ColorDialog1.Color;
  End;
end;

procedure TACloudDemoForm.PLMaskImageSizeTBChange(Sender: TObject);
begin
  PLMaskImageSizeChange(PLMaskImageSizeTB.Position);
end;

procedure TACloudDemoForm.PLMaskImageSizeChange(NewSize: Integer);
begin
  XImage.Picture.Bitmap.Width := NewSize;
  XImage.Picture.Bitmap.Height := NewSize;
  YImage.Picture.Bitmap.Width := NewSize;
  YImage.Picture.Bitmap.Height := NewSize;
  ZImage.Picture.Bitmap.Width := NewSize;
  ZImage.Picture.Bitmap.Height := NewSize;
  LabelImageSize.Caption := inttostr(NewSize);
  // GPFNodes.CCubes[i].PLMaskImageSize:=PLMaskImageSizeTB.Position;
  ClearImageXYZ;
  XImage.Refresh;
  YImage.Refresh;
  ZImage.Refresh;
end;

procedure TACloudDemoForm.PLMaskMaskXOffsetTBChange(Sender: TObject);
begin
  LabelX.Caption := inttostr(PLMaskMaskXOffsetTB.Position);
end;

procedure TACloudDemoForm.PLMaskMaskYOffsetTBChange(Sender: TObject);
begin
  LabelY.Caption := inttostr(PLMaskMaskYOffsetTB.Position);
end;

procedure TACloudDemoForm.PLMaskMaskZOffsetTBChange(Sender: TObject);
begin
  LabelZ.Caption := inttostr(PLMaskMaskZOffsetTB.Position);
end;

procedure TACloudDemoForm.PlLoadXBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter :=
    'Cloud Images|*.bmp;*.tga|32 bits BMP|*.bmp|32 bits TGA|*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    XImage.Picture.Bitmap.Loadfromfile(OpenDialog1.FileName);
    PlLoadXBtn.Caption := OpenDialog1.FileName;
    // PLXImageFileName
  end;
end;

procedure TACloudDemoForm.PLMaskPopupMenu1Click(Sender: TObject);
Begin
  PLMaskMenuNumberStored := TComponent(Sender).Tag;
  PLMaskMenuSet(TComponent(Sender).Tag, PLMaskMaskXOffsetTB.Position,
    PLMaskMaskYOffsetTB.Position, PLMaskMaskZOffsetTB.Position,
    PLMaskImageSizeTB.Position);
End;

procedure TACloudDemoForm.PLMaskMenuSet(MaskMenuNumber, XOffset, YOffset,
  ZOffset, ImageSize: Integer);
var
  PLRect: TRect;
  // XOffset,YOffset,ZOffset, ImageSize : Integer;
begin // Make mask on the image.. will be Y and Z automasked by program later
  // XOffset:=PLMaskMaskXOffsetTB.Position;
  // YOffset:=PLMaskMaskYOffsetTB.Position; //Top bottom:X Left right Center +-Y
  // ZOffset:=PLMaskMaskZOffsetTB.Position;
  // ImageSize:=PLMaskImageSizeTB.Position;//Image Size
  ClearImageXYZ;
  XImage.Canvas.Pen.Width := 1;
  PLRect.Left := ZOffset;
  PLRect.Top := XOffset;
  PLRect.Bottom := ImageSize - XOffset;
  PLRect.Right := ImageSize - ZOffset;
  XImage.Canvas.Pen.Color := clWhite;
  XImage.Canvas.Brush.Color := clWhite;
  Case MaskMenuNumber of
    2: // Torus //XImage  ZImage.Picture.  YImage
      Begin // Short Cylinder Hollow
        XImage.Canvas.Pen.Width := YOffset;
        XImage.Canvas.Brush.Style := bsClear;
        XImage.Canvas.Ellipse(PLRect);
        XImage.Canvas.Pen.Width := 1;
      End;
    3: // Cube //XImage  ZImage.Picture.  YImage
      Begin // Short Bar
        XImage.Canvas.Brush.Style := bsSolid;
        XImage.Canvas.FillRect(PLRect);
      End;
    4: // Tube //XImage  ZImage.Picture.  YImage
      Begin // Short Tube   Hollow
        XImage.Canvas.Pen.Width := YOffset;
        XImage.Canvas.Brush.Style := bsClear;
        XImage.Canvas.Rectangle(PLRect);
        XImage.Canvas.Pen.Width := 1;
      End;
    5:
      Begin // RoundRect
        XImage.Canvas.Pen.Width := YOffset;
        XImage.Canvas.Brush.Style := bsClear;
        // RoundRect(X1, Y1, X2, Y2, X3, Y3
        // (X1,Y1), (X2,Y1), (X2,Y2), (X1,Y2),     RoundRect
        XImage.Canvas.RoundRect(ZOffset, XOffset, (ImageSize - ZOffset),
          (ImageSize - XOffset),
          // curve of the rounded corners
          // curvature of an ellipse with width X3 and height Y3.
          (ImageSize * 2), (ImageSize div 2));
        XImage.Canvas.Pen.Width := 1;
      End;
    6:
      Begin // Polygon  Star
        XImage.Canvas.Pen.Width := YOffset;
        XImage.Canvas.Brush.Style := bsClear;
        XImage.Canvas.Polygon([
          // Left start point
          Point(XOffset, ((ImageSize div 2) - XOffset)),
          // Left inner
          Point(((ImageSize div 2) - XOffset), ((ImageSize div 2) - XOffset)),
          // top
          Point((ImageSize div 2), XOffset),
          // Right inner
          Point(((ImageSize div 2) + XOffset), (((ImageSize div 2)) - XOffset)),
          // Right outer
          Point(((ImageSize) - XOffset), (((ImageSize div 2)) - XOffset)),
          // Should go to a lower inner, lower right outer, middle inner,
          // left outer, left upper inner..and back to start
          // But this skips that ..
          // bottom
          Point((ImageSize div 2), ImageSize - XOffset),
          // start again
          Point(XOffset, ((ImageSize div 2) - XOffset))]);
        XImage.Canvas.Pen.Width := 1;
      End;
  else
    // Relocated to Cacth loading errors
    // 1://Rod //XImage  ZImage.Picture.  YImage
    Begin
      XImage.Canvas.Brush.Style := bsSolid;
      XImage.Canvas.Ellipse(PLRect);
    End;
  End; // Case
end;

// #6 CSprite
// False: Generate using previous data.. If Available
// True: ALL NEW .. Ignore anything there
{ CubeTexUsedArray:Array[1..24] of Double;
  CubeTexCountArray:Array[1..24] of Integer; }
procedure TACloudDemoForm.GeneratePFXCSprite(Regenerate: Boolean; I: Integer);
var
  // J,
  // I:Integer;
  source: TGLSourcePFXEffect;
  sprCube: TGLDummyCube;
begin
  if (PfxCSOnlyOne > 0) then
    Exit; // ONLY 1 allowed... First one is 0
  PfxCSOnlyOne := I;
  { Case GPFNodes.TextureType[19] of
    0:PFXCSpriteManager.SpritesPerTexture:=sptOne;
    1:PFXCSpriteManager.SpritesPerTexture:=sptFour;
    end; }
  PFXCSpriteManager.ColorMode := // scmOuter;  //not visible
  // scmInner;  //can become invisible..Not Transparent Textured
    scmNone; // Solid.. but Not transparent
  // scmFade;  //not visible
  // For I:=0 to GPFNodes.CCubeCount-1 do
  begin
    If (JustLoaded or (Regenerate) or ((not Regenerate) and
      (not GPFNodes.CCubes[I].Generated))) then
    begin // Make Sprites FROM Info
      GPFNodes.CCubes[I].Generated := True;
      sprCube := TGLDummyCube(PFXCubeDC.AddNewChild(TGLDummyCube));
      source := GetOrCreateSourcePFX(sprCube, 'PFXCSSource' + inttostr(I));
      // PFX Source
      source.Manager := PFXCSpriteManager;
      source.Enabled := True;
      source.ParticleInterval := 0.1;
      source.InitialPosition.X := GPFNodes.CCubes[I].Position.X;
      source.InitialPosition.Y := GPFNodes.CCubes[I].Position.Y;
      source.InitialPosition.Z := GPFNodes.CCubes[I].Position.Z;
      Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
      GPFNodes.CCubes[I].Generated := True;
      PfxCSTotals := PfxCSTotals + GPFNodes.CCubes[I].SpriteCount;
      PFXCSpriteManager.CreateParticles(GPFNodes.CCubes[I].SpriteCount);

      { For I:=0 to GPFNodes.CCubeCount-1 do
        For J:=0 to GPFNodes.CCubes[i].SpriteCount-1 do
        begin
        spr:=TGLSprite(SpriteCubeDC.AddNewChild(TGLSprite));
        //Named to allow Gizmo Repositioning  Special symbols
        spr.name:='spr'+InttoStr(i)+'spr'+InttoStr(j);
        spr.Position.X:=  GPFNodes.CCubes[i].Sprites[J].Position.X;
        spr.Position.Y:= GPFNodes.CCubes[i].Sprites[J].Position.Y;
        spr.Position.Z:=GPFNodes.CCubes[i].Sprites[J].Position.Z;
        //GPFNodes.CCubes[i].Sprites[J].Position.W;
        spr.Width:= GPFNodes.CCubes[i].Sprites[J].Size.X;
        spr.Height:= GPFNodes.CCubes[i].Sprites[J].Size.Z;
        //GPFNodes.CCubes[i].Sprites[J].Size.Z;
        //GPFNodes.CCubes[i].Sprites[J].Size.W;
        spr.ResetAndPitchTurnRoll(
        GPFNodes.CCubes[i].Sprites[J].Rotation.X,
        GPFNodes.CCubes[i].Sprites[J].Rotation.Y,
        GPFNodes.CCubes[i].Sprites[J].Rotation.Z);
        //GPFNodes.CCubes[i].Sprites[J].Rotation.W;

        spr.Material.MaterialLibrary:=GLCloudsMatLib;
        spr.Material.LibMaterialName:=GPFNodes.CCubes[i].Sprites[J].TextureName;
        //  spr.Material.LibMaterialName:=GLCloudsMatLib.Materials[0].Name;

        //  GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[i].Sprites[J].TextureName);
        spr.Material.FrontProperties.Diffuse.Alpha:=
        GPFNodes.CCubes[i].Sprites[J].A;
        spr.Material.FrontProperties.Diffuse.Red:=GPFNodes.CCubes[i].Sprites[J].R;
        spr.Material.FrontProperties.Diffuse.Green:=GPFNodes.CCubes[i].Sprites[J].G;
        spr.Material.FrontProperties.Diffuse.Blue:=GPFNodes.CCubes[i].Sprites[J].B;
        //GPFNodes.CCubes[i].Sprites[J].B:=GPFNodes.CCubes[i].Sprites[J].B;
        spr.TagFloat:=GPFNodes.CCubes[i].Sprites[J].AlbedoNumber;
        end; }
    end;
  end;
end;

procedure TACloudDemoForm.PFXCSpriteManagerCreateParticle(Sender: TObject;
  aParticle: TGLParticle);
var
  I, Temp, Tempj: Integer;
begin // Some way to know WHICH particle is BEING Made...
  Temp := aParticle.ID;
  I := PfxCSOnlyOne;
  { If Temp >PfxCSTotal  then //it increases the id constantly
    StatusBar.Panels[4].Text:=inttostr(Temp)+' id invalid'; }
  If Temp > GPFNodes.CCubes[I].SpriteCount then
  begin
    Temp := Temp mod GPFNodes.CCubes[I].SpriteCount;
  end;
  Tempj := Temp;
  aParticle.PosX := GPFNodes.CCubes[I].Sprites[Tempj].Position.X;
  aParticle.PosY := GPFNodes.CCubes[I].Sprites[Tempj].Position.Y;
  aParticle.PosZ := GPFNodes.CCubes[I].Sprites[Tempj].Position.Z;
end;

{ //TGLParticleFXRenderer
  BlendingMode:= bmTransparency;//  bmAdditive bmAlphaTest50  bmModulate   bmTransparency
  Name:= PFXRenderer;
  Visible:=True;
  ZCull:=True;
  ZMaxDistance:=0;   //If zero, camera's DepthOfView is used
  ZSortAccuracy:=saHigh;  //saLow  saOneHalf   saOneTenth  saOneThird
  ZTest:=True;
  ZWrite:=False; }

{ GLCustomSpritePFXManager1.Cadencer:=GLCadencer1;
  GLCustomSpritePFXManager1.Renderer:=PFXRenderer; }
// SET THESE..BUT they Apply to ALL Sprites..
// individual parameters Must be done in the ___ Methods
// GLCustomSpritePFXManager1.AspectRatio
// BlendingMode   bmAdditive bmAlphaTest50  bmModulate   bmTransparency
// ColorInner
// ColorMode    scmInner  scmOuter scmNone  scmFade
// ColorOuter
// LifeColors
// ParticleSize
// ?Rotation.. NO Must MAKE the images rotated to do that...
// GLCustomSpritePFXManager1.SpritesPerTexture     sptOne   sptFour
// aParticle.ID
/// GLCustomSpritePFXManager1CreateParticle
/// GLCustomSpritePFXManager1PrepareTextureImage

// BindTexture line 2820
// texFormat:=GL_LUMINANCE_ALPHA;
procedure TACloudDemoForm.PFXCSpriteManagerPrepareTextureImage(Sender: TObject;
  destBmp32: TGLBitmap32; var texFormat: Integer);
var
  bmp32: TGLBitmap32;
  SprMat: TGLLibMaterial; // TGLLibMaterial
  Token: Integer;
begin
  { Case GPFNodes.TextureType[19] of
    0:GLCustomSpritePFXManager1.SpritesPerTexture:=sptOne;
    1:GLCustomSpritePFXManager1.SpritesPerTexture:=sptFour;
    end; }
  // texFormat:=GL_LUMINANCE_ALPHA;//GL_RGBA GL_RGB  GL_ALPHA}
  texFormat := // GL_ALPHA;
  // GL_RGBA; //
    GL_LUMINANCE_ALPHA;
  // 1 or 4 Textures per image MUST be set for !ALL!
  // spr.Material.MaterialLibrary:=GLCloudsMatLib;
  // spr.Material.LibMaterialName:=
  // PfxCSOnlyOne
  // GPFNodes.CCubes[PfxCSOnlyOne].Sprites[J].TextureName;
  bmp32 := TGLBitmap32.Create;
  bmp32.Width := 128;
  bmp32.Height := 128;
  // GPFNodes.CCubes[PfxCSOnlyOne].Sprites[Tempj].Position.X;
  // spr.Material.MaterialLibrary:=GLCloudsMatLib;
  // spr.Material.LibMaterialName:=;
  Token := Random(GPFNodes.CCubes[PfxCSOnlyOne].SpriteCount - 1);
  // Only called once ? for ALL the particles!..even after different loads.
  // showmessage(inttostr(GPFNodes.CCubes[PfxCSOnlyOne].SpriteCount)+' : '+inttostr(Token));
  SprMat := GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[PfxCSOnlyOne]
    .Sprites[Token].TextureName);
  bmp32.Assign(
    // GLCloudsMatLib.Materials[0].Material
    SprMat.Material.Texture.Image.GetBitmap32);
  bmp32.SetAlphaFromIntensity;
  destBmp32.Assign(bmp32);
end;

// #7 Imposter
// ImposterDC
// ImposterDirectOGL     ImposterDirectOGLRender
procedure TACloudDemoForm.GenerateImposter(Regenerate: Boolean; I: Integer);
var
  source: TGLSourcePFXEffect;
  sprCube: TGLDummyCube;
Begin
  If (JustLoaded or (Regenerate) or ((not Regenerate) and
    (not GPFNodes.CCubes[I].Generated))) then
  begin // Make Sprites FROM Info
    GPFNodes.CCubes[I].Generated := True;
    // renderPoint is at TOP of Hierarchy to render against Nothing
    renderPoint := TGLRenderPoint(ImposterDC.AddNewChild(TGLRenderPoint));

    sprCube := TGLDummyCube(ImposterDC.AddNewChild(TGLDummyCube));
    PFXPolyFogManager.LifeColors[0].LifeTime := 2;
    // smaller=faster, larger=keeps making more
    PFXPolyFogManager.ParticleSize := 0.7;
    PFXPolyFogManager.Cadencer := GLCadencer1;
    PFXPolyFogManager.Renderer := PFXRenderer;
    // PFXPolyFogManager.Acceleration.Z:=-1;
    source := GetOrCreateSourcePFX(sprCube, 'PFXPolyFogSource' + inttostr(I));
    // PFX Source
    source.Manager := PFXPolyFogManager;
    source.Enabled := True;
    source.ParticleInterval := 0.03; // density distance of particles
    source.InitialPosition.X := 0; // GPFNodes.CCubes[i].Position.X;
    source.InitialPosition.Y := 0; // GPFNodes.CCubes[i].Position.Y;
    source.InitialPosition.Z := 0; // GPFNodes.CCubes[i].Position.Z;
    source.PositionDispersion := VectorLength(GPFNodes.CCubes[I].Size) * 0.5;
    // 0.3;//smaller=all in 1 place

    impBuilder := TGLStaticImposterBuilder.Create(self);
    impBuilder.SampleSize := 64;
    impBuilder.SamplingRatioBias := 1.3;
    impBuilder.Coronas.Items[0].Samples := 32;
    impBuilder.Coronas.Add(15, 24);
    impBuilder.Coronas.Add(30, 24);
    impBuilder.Coronas.Add(45, 16);
    impBuilder.Coronas.Add(60, 16);
    impBuilder.Coronas.Add(85, 16);
    impBuilder.renderPoint := renderPoint;
    // Must Make the Fog Cloud FIRST
    ImposterCube := I;
    // ImposterDC
    impBuilder.RequestImposterFor(ImposterDC); // GLTeapot1
    // Only visible AFTER something there at ImposterDC
    Application.ProcessMessages;
    ImposterDirectOGL.Visible := True; // When visible it will begin rendering
    // GLTeapot1.Visible:=CBShowTeapot.Checked;
  end;
End;

procedure TACloudDemoForm.ImposterDirectOGLRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  camPos, Pos: TVector;
  imp: TImposter;
  J: Integer;
begin // impostoredObject : TGLBaseSceneObject
  imp := impBuilder.ImposterFor(ImposterDC);
  if (imp = nil) or (imp.Texture.Handle = 0) then
    Exit;

  imp.BeginRender(rci);
  // 3721 imposters   for x:=-30 to 30 do for y:=-30 to 30 do
  For J := 0 to GPFNodes.CCubes[ImposterCube].SpriteCount - 1 do
  begin // x*5, 0, y*4);
    MakePoint(Pos, GPFNodes.CCubes[ImposterCube].Sprites[J].Position.X,
      GPFNodes.CCubes[ImposterCube].Sprites[J].Position.Y,
      GPFNodes.CCubes[ImposterCube].Sprites[J].Position.Z);
    camPos := VectorSubtract(rci.cameraPosition, Pos);
    // const objPos, localCameraPos : TVector; size : Single);
    imp.Render(rci, Pos, camPos, GPFNodes.CCubes[ImposterCube].Sprites[J]
      .Size.X); // 1);
  end;
  imp.EndRender(rci);
end;

// 8 GenerateProceduralClouds
procedure TACloudDemoForm.GenerateNinianeClouds(Regenerate: Boolean;
  I: Integer);
Begin
  ShellExecute(Application.Handle, // handle to parent window
    'open', // pointer to string that specifies operation to perform
    PChar(ExtractFilePath(ParamStr(0)) + 'GenerateNinianeClouds.htm'),
    // pointer to filename or folder name string
    '', // pointer to string that specifies executable-file parameters
    PChar(ExtractFilePath(ParamStr(0))),
    // pointer to string that specifies default directory
    SW_SHOWNORMAL);
end;

// 9 Real [Slow]
procedure TACloudDemoForm.GenerateRealSlow(Regenerate: Boolean; I: Integer);
Begin
  ShellExecute(Application.Handle, // handle to parent window
    'open', // pointer to string that specifies operation to perform
    PChar(ExtractFilePath(ParamStr(0)) + 'RealSlow.htm'),
    // pointer to filename or folder name string
    '', // pointer to string that specifies executable-file parameters
    PChar(ExtractFilePath(ParamStr(0))),
    // pointer to string that specifies default directory
    SW_SHOWNORMAL);
end;
/// //////////////////////////////////
/// //////////////////////////////////

/// //////////////////////////////////
/// //////////////////////////////////
procedure TACloudDemoForm.AddShapeBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud shape data (*.glsm)|*.glsm;*.dxf';
  OpenDialog1.FileName := ''; // '*.glsm' ;//'*.glsm' ;
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    Application.ProcessMessages; // Let the dialog go away...
    ShapeProgressBar.Position := 10;
    Application.ProcessMessages;
    // Have it generate Cubedata.. then make the sprites as others do
    // Add to Data and Create a Cloud Cube Shape at Center
    inc(GPFNodes.CCubeCount);
    TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
    MakeABlank(GPFNodes.CCubeCount - 1, 0);
    // Need to Redo stuff...  Make sure it is doing Simple Sprites...
    // It must Add a Cube with Certain parameters: 216 Sprites: 6x6x6=216
    // SQUARE CUBE  ... but the size is mostly ignored.
    // SpriteTypeRG.ItemIndex;
    GPFNodes.CCubes[GPFNodes.CCubeCount - 1].SpriteType := 2;
    // CloudPatternRG 4
    GPFNodes.CCubes[GPFNodes.CCubeCount - 1].CloudPattern := 4;
    // CloudPatternRG.Itemindex;
    // 216 is Set then reset after number of Sprites actually used is determined
    // GPFNodes.CCubes[GPFNodes.CCubeCount-1].SpriteCount:=216;// Strtoint(PartsEdit.text);
    CurrentCubeUpDown.Position := GPFNodes.CCubeCount; // calls Set data to Text
    ShapeProgressBar.Position := 20;
    Application.ProcessMessages;
    inc(GPFNodes.ShapeCount);
    Setlength(GPFNodes.ShapeNames, GPFNodes.ShapeCount);
    GPFNodes.ShapeNames[GPFNodes.ShapeCount - 1] := OpenDialog1.FileName;
    // FreeFormDC   ShapeNames                                        //.ShapeName
    ShapeProgressBar.Position := 0;
    Application.ProcessMessages;
    // Called during Generation
    // LoadMakeShape(GPFNodes.CCubeCount-1,OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.LoadMakeShape(CubeNumber, J: Integer;
  FileName: String);
var
  NewObject: TGLFreeForm;
begin
  ShapeProgressBar.Position := 10;
  Application.ProcessMessages;
  NewObject := (FreeFormDC.AddNewChild(TGLFreeForm) as TGLFreeForm);
  NewObject.Name := 'FreeForm' + inttostr(J);
  // Give it a Tag.. so Gizmo cant catch it?
  NewObject.Material.FaceCulling := fcNoCull;
  /// /GPFNodes.CCubes[i].Upxyz:=UpxyzRG.itemindex;
  Case GPFNodes.CCubes[CubeNumber].Upxyzw of
    0:
      Begin
        If GPFNodes.CCubes[CubeNumber].ShapeUpNegative then
          NewObject.Up.X := -1
        else
          NewObject.Up.X := 1;
        NewObject.Up.Z := 0;
        NewObject.Up.Y := 0;
      end;
    1:
      Begin
        If GPFNodes.CCubes[CubeNumber].ShapeUpNegative then
          NewObject.Up.Y := -1
        else
          NewObject.Up.Y := 1;
        NewObject.Up.Z := 0;
        NewObject.Up.X := 0;
      end;
    2:
      Begin
        If GPFNodes.CCubes[CubeNumber].ShapeUpNegative then
          NewObject.Up.Z := -1
        else
          NewObject.Up.Z := 1;
        NewObject.Up.Y := 0;
        NewObject.Up.X := 0;
      end;
  End; // case

  NewObject.UseMeshMaterials := False;
  NewObject.Visible := GPFNodes.CCubes[CubeNumber].ShapeShowObject; // False;
  NewObject.Loadfromfile(FileName);
  ShapeProgressBar.Position := 20;
  Application.ProcessMessages;
  // Rescale the Freeform to FIT a Cube size of 1x1x1
  // or use the Input size from the form...
  If GPFNodes.CCubes[CubeNumber].ShapeSizeCube then
    NewObject.Scale.X := GPFNodes.CCubes[CubeNumber].Size.X /
      NewObject.BoundingSphereRadius
  else
    NewObject.Scale.X := 1 / NewObject.BoundingSphereRadius;
  If GPFNodes.CCubes[CubeNumber].ShapeSizeCube then
    NewObject.Scale.Y := GPFNodes.CCubes[CubeNumber].Size.Y /
      NewObject.BoundingSphereRadius
  else
    NewObject.Scale.Y := 1 / NewObject.BoundingSphereRadius;
  If GPFNodes.CCubes[CubeNumber].ShapeSizeCube then
    NewObject.Scale.Z := GPFNodes.CCubes[CubeNumber].Size.Z /
      NewObject.BoundingSphereRadius
  else
    NewObject.Scale.Z := 1 / NewObject.BoundingSphereRadius;
  NewObject.StructureChanged;
  ShapeProgressBar.Position := 30;
  Application.ProcessMessages;
  NewObject.BuildOctree;
  // Now called from Generate After Sprites are Made..for That Cube
  // BuildGrid;//(nil);
  ShapeProgressBar.Position := 0;
  Application.ProcessMessages;
end;

// This is culled from Mattias PointInMesh demo
procedure TACloudDemoForm.BuildGrid(I, J: Integer); // (Sender : TObject);
// const  cResolution = 6;//32;
// 3=27, 4=64, 5=125, 6x6x6=216, 7=343, 8=512, 9=729, 10=1000
// 32 = 32768  If filling a Square Cube
var
  Hits, cResolution, X, Y, Z: Integer;
  step: single;
  Point: TVector;
  brad: single;
begin
  // Note - this could be speeded up enourmously by using a proprietary method
  // instead of OctreePointInMesh - which is recalculated for each node where
  // it could be calculated once per column (cutting down run time by aprox.
  // 1/cResolution).

  // Increasing the bounding sphere radius to generate a box that's guaranteed
  // to encompas the entire freeform   //GLFreeFormNA
  // maybe make an Array of Freeforms so more than 1 can be used...
  // GPFNodes.CCubes[i].ShapeResolution :=Strtoint(ShapeResolutionEdit.text);
  cResolution := // Strtoint(ShapeResolutionEdit.text);
    GPFNodes.CCubes[I].ShapeResolution;
  brad := (FreeFormDC.Children[J] as TGLFreeForm).BoundingSphereRadius * 1.42;
  step := brad / (cResolution + 1);
  ShapeProgressBar.Max := cResolution - 1;
  Hits := 0;
  GPFNodes.CCubes[I].SpriteCount := (cResolution * cResolution * cResolution);
  // 216;
  Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
  for X := 0 to cResolution - 1 do
  begin
    ShapeProgressBar.Position := X; // Range here is o..5: 6
    for Y := 0 to cResolution - 1 do
      for Z := 0 to cResolution - 1 do
      begin
        Point := VectorAdd((FreeFormDC.Children[J] as TGLFreeForm)
          .Position.AsVector, VectorMake(X * step - brad / 2,
          Y * step - brad / 2, Z * step - brad / 2));
        // Iterate over the X,Y,Z of area.. if IN area then Make a New object
        if (FreeFormDC.Children[J] as TGLFreeForm).OctreePointInMesh(Point) then
        begin
          // AsVector := Point;
          GPFNodes.CCubes[I].Sprites[Hits].Position.X := GPFNodes.CCubes[I]
            .Position.X + Point.X;
          GPFNodes.CCubes[I].Sprites[Hits].Position.Y := GPFNodes.CCubes[I]
            .Position.Y + Point.Y;
          GPFNodes.CCubes[I].Sprites[Hits].Position.Z := GPFNodes.CCubes[I]
            .Position.Z + Point.Z;
          If GPFNodes.CCubes[I].ShapeSpriteSize then
          begin
            { use those set per input range }
          end
          else
          begin // else use default
            GPFNodes.CCubes[I].Sprites[Hits].Size.X := step * 1.42;
            /// 2;
            GPFNodes.CCubes[I].Sprites[Hits].Size.Z := step * 1.42;
            /// 2;
          end;
          // Ignore the Rotation set...
          GPFNodes.CCubes[I].Sprites[Hits].Rotation.X := 0;
          GPFNodes.CCubes[I].Sprites[Hits].Rotation.Y := 0;
          GPFNodes.CCubes[I].Sprites[Hits].Rotation.Z := 0;
          inc(Hits);
        end;
      end;
  end;
  GPFNodes.CCubes[I].SpriteCount := Hits;
  // Showmessage('Hits: '+inttostr(Hits));
  Setlength(GPFNodes.CCubes[I].Sprites, GPFNodes.CCubes[I].SpriteCount);
  ShapeProgressBar.Position := 0;
  ShapeProgressBar.Max := 100;
end;

/// //////////////////////////////////
// Need a way to Color the Cubes... By 'Group' or User Color selection...
// NodeCubeDC      SpriteCubeDC
procedure TACloudDemoForm.AddBtnClick(Sender: TObject);
begin
  // Add to Data and Create a Cloud Cube   at Center
  inc(GPFNodes.CCubeCount);
  TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  MakeABlank(GPFNodes.CCubeCount - 1, 0);
  CurrentCubeUpDown.Position := GPFNodes.CCubeCount;
  // SetGPFCubeToText(GPFNodes.CCubeCount-1);//  SetGPFCubeToNodeCube(i);  SetGPFCubeToNode(i);
end;

procedure TACloudDemoForm.AddBlueBtnClick(Sender: TObject);
begin
  // Add to Data and Create a Cloud Cube   at Blue Cube
  inc(GPFNodes.CCubeCount);
  TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  MakeABlank(GPFNodes.CCubeCount - 1, 1);
  CurrentCubeUpDown.Position := GPFNodes.CCubeCount;
  // SetGPFCubeToText(GPFNodes.CCubeCount-1);
end;

procedure TACloudDemoForm.AddAtLastBtnClick(Sender: TObject);
begin
  // Add to Data and Create a Cloud Cube   at Latest Cube
  inc(GPFNodes.CCubeCount);
  TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  MakeABlank(GPFNodes.CCubeCount - 1, 2);
  CurrentCubeUpDown.Position := GPFNodes.CCubeCount;
  // SetGPFCubeToText(GPFNodes.CCubeCount-1);
end;

procedure TACloudDemoForm.CurrentCubeUpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin // btNext
  // CurrentCubeUpDown
  // Select WHICH CloudCube to be Focused
  // Requires Changing ALL the Data Edits TOO
  // 0  ???  -1
  If ((NewValue > 0) and (NewValue <= GPFNodes.CCubeCount)) then
  begin // UpDown1.Position
    CurrentCubesLabel.Caption := inttostr(NewValue);
    SetGPFCubeToText(NewValue - 1);
    AllowChange := True;
  end
  else
    AllowChange := False;
end;

procedure TACloudDemoForm.DeleteBtnClick(Sender: TObject);
begin
  // Delete the LAST Cube
  dec(GPFNodes.CCubeCount);
  TotalCubesLabel.Caption := inttostr(GPFNodes.CCubeCount);
  // MakeABlank(GPFNodes.CCubeCount-1,2);
  // Delete the GPFNodes.CCube
  If GPFNodes.CCubes[GPFNodes.CCubeCount].Generated then
    Setlength(GPFNodes.CCubes[GPFNodes.CCubeCount].Sprites, 0);
  Setlength(GPFNodes.CCubes, GPFNodes.CCubeCount);
  // Display something
  SetGPFCubeToText(GPFNodes.CCubeCount - 1);
  SetGPFCubeToNodeCube(GPFNodes.CCubeCount - 1);
end;

procedure TACloudDemoForm.ResetBtnClick(Sender: TObject);
// Reset the Focused Cube to Current Edit data
var
  I: Integer;
begin
  I := Strtoint(CurrentCubesLabel.Caption) - 1;
  SetTextToGPFCube(I);
  SetGPFCubeToNodeCube(I);
end;

/// //////////////////////////////////
// CODE start
{ procedure TACloudDemoForm.MakeABlank(i,Where:Integer);
  procedure TACloudDemoForm.SetNodeCubeToGPFCube(i:Integer);
  procedure TACloudDemoForm.SetGPFCubeToNodeCube(i:Integer);
  procedure TACloudDemoForm.SetGPFCubeToText(i:Integer);
  procedure TACloudDemoForm.SetTextToGPFCube(i:Integer); }
/// //////////////////////////////////
// Node  NodeCube_0  GPFNodes[0]
procedure TACloudDemoForm.MakeABlank(I, Where: Integer);
var
  NewObject: TGLCube;
  // j:Integer; Tempname:String;
begin
  Setlength(GPFNodes.CCubes, GPFNodes.CCubeCount);
  // Make a New NodeCube_   GLCloudsDC   NodeCubeDC
  NewObject := (CloudCubeDC.AddNewChild(TGLCube) as TGLCube);
  NewObject.Name := 'CloudCube_' + inttostr(I);
  // Change to GROUP Color   AsWinColor
  NewObject.Material.FrontProperties.Diffuse.AsWinColor :=
    GroupColorPanel.Color;
  // NewObject.Tag:=strtoint(GroupColorEdit.text);
  { NewObject.Material.FrontProperties.Diffuse.Red  :=1;
    NewObject.Material.FrontProperties.Diffuse.Green:= 0.5;
    NewObject.Material.FrontProperties.Diffuse.Blue := 0; }
  NewObject.Material.FrontProperties.Diffuse.Alpha := 1;
  // The blue one is semitransparent when Gizmoed..
  // trying to make other cubes see through.. not working
  NewObject.Material.Texture.EnvColor.Red := 1; // 0;
  NewObject.Material.Texture.EnvColor.Green := 1; // 0;
  NewObject.Material.Texture.EnvColor.Blue := 1; // 0;
  NewObject.Material.Texture.EnvColor.Alpha := 0.1;
  NewObject.Material.Texture.Disabled := True;
  NewObject.Up := CloudCubeDC.Up; // 0,0,0
  CloudCubeDC.Children[I].ResetAndPitchTurnRoll
    (StrToFloatDef(PitchEdit.text), StrToFloatDef(RollEdit.text),
    StrToFloatDef(TurnEdit.text));
  NewObject.CubeWidth := StrToFloatDef(WidthEdit.text); // 1;
  NewObject.CubeHeight := StrToFloatDef(DepthYEdit.text); // 1;
  NewObject.CubeDepth := StrToFloatDef(HeightZEdit.text); // 1;
  Case Where of
    0: // Center
      begin
        NewObject.Position.X := 0;
        NewObject.Position.Y := 0;
        NewObject.Position.Z := 0;
      end;
    1: // Blue Cube Location
      begin
        NewObject.Position.X := GLCloudCube.Position.X;
        NewObject.Position.Y := GLCloudCube.Position.Y;
        NewObject.Position.Z := GLCloudCube.Position.Z;
      end;
    2: // Last Cube Location
      begin
        NewObject.Position.X := GPFNodes.CCubes[GPFNodes.CCubeCount - 2]
          .Position.X;
        NewObject.Position.Y := GPFNodes.CCubes[GPFNodes.CCubeCount - 2]
          .Position.Y;
        NewObject.Position.Z := GPFNodes.CCubes[GPFNodes.CCubeCount - 2]
          .Position.Z;
        { CloudCubeDC.Children[i].ResetAndPitchTurnRoll(
          GPFNodes.CCubes[GPFNodes.CCubeCount-2].Rotation.X,
          GPFNodes.CCubes[GPFNodes.CCubeCount-2].Rotation.Y,
          GPFNodes.CCubes[GPFNodes.CCubeCount-2].Rotation.Z); }
        // Make new ones per Size IAW Text input.. Above
        { NewObject.CubeWidth:=GPFNodes.CCubes[GPFNodes.CCubeCount-2].Size.X;
          NewObject.CubeHeight:=GPFNodes.CCubes[GPFNodes.CCubeCount-2].Size.Y;
          NewObject.CubeDepth:=GPFNodes.CCubes[GPFNodes.CCubeCount-2].Size.Z; }
        // CurrentRotationSpeed:=GPFNodes.CCubes[GPFNodes.CCubeCount-2].Speed;
      end;
  End; // Case
  GPFNodes.CCubes[I].Position.X := NewObject.Position.X;
  GPFNodes.CCubes[I].Position.Y := NewObject.Position.Y;
  GPFNodes.CCubes[I].Position.Z := NewObject.Position.Z;
  GPFNodes.CCubes[I].Rotation.X := NewObject.Rotation.X;
  GPFNodes.CCubes[I].Rotation.Y := NewObject.Rotation.Y;
  GPFNodes.CCubes[I].Rotation.Z := NewObject.Rotation.Z;
  GPFNodes.CCubes[I].Size.X := NewObject.CubeWidth;
  GPFNodes.CCubes[I].Size.Y := NewObject.CubeHeight;
  GPFNodes.CCubes[I].Size.Z := NewObject.CubeDepth;
  GPFNodes.CCubes[I].SpeedX := StrToFloatDef(SpeedXEdit.text); // 1;
  GPFNodes.CCubes[I].SpeedY := StrToFloatDef(SpeedYEdit.text); // 1;
  GPFNodes.CCubes[I].SpeedZ := StrToFloatDef(SpeedZEdit.text); // 1;
  GPFNodes.CCubes[I].CurrentMovingSprite := 0;
  GPFNodes.CCubes[I].Upxyzw := ShapeUpxyzRG.ItemIndex;
  GPFNodes.CCubes[I].ShapeShowObject := ShapeShowObjectCB.checked;
  GPFNodes.CCubes[I].ShapeSizeCube := ShapeSizeCubeCB.checked;
  GPFNodes.CCubes[I].ShapeUpNegative := ShapeUpNegativeCB.checked;
  GPFNodes.CCubes[I].ShapeSpriteSize := ShapeSizeSpriteCB.checked;
  GPFNodes.CCubes[I].ShapeResolution := Strtoint(ShapeResolutionEdit.text);
  GPFNodes.CCubes[I].Generated := False;
  GPFNodes.CCubes[I].GroupColor := GroupColorPanel.Color;
  GPFNodes.CCubes[I].GroupNumber := Strtoint(GroupColorEdit.text);
  GPFNodes.CCubes[I].SpriteCount := Strtoint(PartsEdit.text);
  GPFNodes.CCubes[I].XMax := StrToFloatDef(XMaxEdit.text);
  GPFNodes.CCubes[I].XMin := StrToFloatDef(XMinEdit.text);
  GPFNodes.CCubes[I].YMax := StrToFloatDef(YMaxEdit.text);
  GPFNodes.CCubes[I].YMin := StrToFloatDef(YMinEdit.text);
  GPFNodes.CCubes[I].ZMax := StrToFloatDef(ZMaxEdit.text);
  GPFNodes.CCubes[I].ZMin := StrToFloatDef(ZMinEdit.text);
  GPFNodes.CCubes[I].RMax := Strtoint(RotationMaxEdit.text);
  GPFNodes.CCubes[I].RMin := Strtoint(RotationMinEdit.text);

  GPFNodes.CCubes[I].SpriteType := SpriteTypeRG.ItemIndex;
  GPFNodes.CCubes[I].TextureType := TextureTypeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudType := CloudTypeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudShape := CloudShapeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudPattern := CloudPatternRG.ItemIndex;
  // inc(GPFNodes.ShapeCount);

  GPFNodes.CCubes[I].Albedo1R := StrToFloatDef(Albedo1REdit.text);
  GPFNodes.CCubes[I].Albedo1G := StrToFloatDef(Albedo1GEdit.text);
  GPFNodes.CCubes[I].Albedo1B := StrToFloatDef(Albedo1BEdit.text);
  GPFNodes.CCubes[I].Albedo1A := StrToFloatDef(Albedo1AEdit.text);
  GPFNodes.CCubes[I].Albedo2R := StrToFloatDef(Albedo2REdit.text);
  GPFNodes.CCubes[I].Albedo2G := StrToFloatDef(Albedo2GEdit.text);
  GPFNodes.CCubes[I].Albedo2B := StrToFloatDef(Albedo2BEdit.text);
  GPFNodes.CCubes[I].Albedo2A := StrToFloatDef(Albedo2AEdit.text);
  GPFNodes.CCubes[I].Albedo3R := StrToFloatDef(Albedo3REdit.text);
  GPFNodes.CCubes[I].Albedo3G := StrToFloatDef(Albedo3GEdit.text);
  GPFNodes.CCubes[I].Albedo3B := StrToFloatDef(Albedo3BEdit.text);
  GPFNodes.CCubes[I].Albedo3A := StrToFloatDef(Albedo3AEdit.text);
  GPFNodes.CCubes[I].Albedo4R := StrToFloatDef(Albedo4REdit.text);
  GPFNodes.CCubes[I].Albedo4G := StrToFloatDef(Albedo4GEdit.text);
  GPFNodes.CCubes[I].Albedo4B := StrToFloatDef(Albedo4BEdit.text);
  GPFNodes.CCubes[I].Albedo4A := StrToFloatDef(Albedo4AEdit.text);
  GPFNodes.CCubes[I].Albedo5R := StrToFloatDef(Albedo5REdit.text);
  GPFNodes.CCubes[I].Albedo5G := StrToFloatDef(Albedo5GEdit.text);
  GPFNodes.CCubes[I].Albedo5B := StrToFloatDef(Albedo5BEdit.text);
  GPFNodes.CCubes[I].Albedo5A := StrToFloatDef(Albedo5AEdit.text);
  GPFNodes.CCubes[I].AlphaStart := StrToFloatDef(AlphaInitialEdit.text);
  GPFNodes.CCubes[I].AlphaRate := StrToFloatDef(AlphaRateEdit.text);
  GPFNodes.CCubes[I].AlphaFormation :=
    StrToFloatDef(AlphaFormationEdit.text);
  GPFNodes.CCubes[I].AlphaDissipation :=
    StrToFloatDef(AlphaDissipationEdit.text);

  GPFNodes.CCubes[I].PLPSize := StrToFloatDef(PLPSizeEdit.text, 0.3);
  GPFNodes.CCubes[I].PLPLife := StrToFloatDef(PLPLifeEdit.text, 1.0);
  GPFNodes.CCubes[I].PLPInterval := StrToFloatDef(PLPIntervalEdit.text, 0.003);
  GPFNodes.CCubes[I].PLDepth := StrToIntDef(PLDepthEdit.text, 3);
  GPFNodes.CCubes[I].PLScale := StrToFloatDef(PLScaleEdit.text, 1);
  GPFNodes.CCubes[I].PLMaskString := PLCharEdit.text;
  GPFNodes.CCubes[I].PLMaskStringSize :=
    StrToIntDef(PLCharacterSizeEdit.text, 16);
  GPFNodes.CCubes[I].PLMaskType := PLMaskTypeRG.ItemIndex;
  GPFNodes.CCubes[I].PLMaskImageSize := PLMaskImageSizeTB.Position;
  // PLMaskImageSizeChange(NewSize:Integer);  //On load
  GPFNodes.CCubes[I].PLMaskXOffset := PLMaskMaskXOffsetTB.Position;
  LabelX.Caption := inttostr(GPFNodes.CCubes[I].PLMaskXOffset);
  GPFNodes.CCubes[I].PLMaskYOffset := PLMaskMaskYOffsetTB.Position;
  LabelY.Caption := inttostr(GPFNodes.CCubes[I].PLMaskYOffset);
  GPFNodes.CCubes[I].PLMaskZOffset := PLMaskMaskZOffsetTB.Position;
  LabelZ.Caption := inttostr(GPFNodes.CCubes[I].PLMaskZOffset);
  GPFNodes.CCubes[I].PLMaskMenuNumber := PLMaskMenuNumberStored;
  // PLMaskMenuSet(MaskMenuNumber,XOffset,YOffset,ZOffset,ImageSize:Integer);
  GPFNodes.CCubes[I].PLColor := PLColorPanel.Color;
  GPFNodes.CCubes[I].PLXImageFileName := PlLoadXBtn.Caption;
  GPFNodes.CCubes[I].PLPitch := StrToFloatDef(PLPitchEdit.text, -90);
  GPFNodes.CCubes[I].PLRoll := StrToFloatDef(PLRollEdit.text, 0);
  GPFNodes.CCubes[I].PLTurn := StrToFloatDef(PLTurnEdit.text, 0);

  // 24
  GPFNodes.CCubes[I].TexturesRecord[1].Selected := TextureCB1.checked;
  GPFNodes.CCubes[I].TexturesRecord[1].Permin := Strtoint(TextureMinEdit1.text);
  GPFNodes.CCubes[I].TexturesRecord[1].PerMax := Strtoint(TextureMaxEdit1.text);
  GPFNodes.CCubes[I].TexturesRecord[2].Selected := TextureCB2.checked;
  GPFNodes.CCubes[I].TexturesRecord[2].Permin := Strtoint(TextureMinEdit2.text);
  GPFNodes.CCubes[I].TexturesRecord[2].PerMax := Strtoint(TextureMaxEdit2.text);
  GPFNodes.CCubes[I].TexturesRecord[3].Selected := TextureCB3.checked;
  GPFNodes.CCubes[I].TexturesRecord[3].Permin := Strtoint(TextureMinEdit3.text);
  GPFNodes.CCubes[I].TexturesRecord[3].PerMax := Strtoint(TextureMaxEdit3.text);
  GPFNodes.CCubes[I].TexturesRecord[4].Selected := TextureCB4.checked;
  GPFNodes.CCubes[I].TexturesRecord[4].Permin := Strtoint(TextureMinEdit4.text);
  GPFNodes.CCubes[I].TexturesRecord[4].PerMax := Strtoint(TextureMaxEdit4.text);
  GPFNodes.CCubes[I].TexturesRecord[5].Selected := TextureCB5.checked;
  GPFNodes.CCubes[I].TexturesRecord[5].Permin := Strtoint(TextureMinEdit5.text);
  GPFNodes.CCubes[I].TexturesRecord[5].PerMax := Strtoint(TextureMaxEdit5.text);
  GPFNodes.CCubes[I].TexturesRecord[6].Selected := TextureCB6.checked;
  GPFNodes.CCubes[I].TexturesRecord[6].Permin := Strtoint(TextureMinEdit6.text);
  GPFNodes.CCubes[I].TexturesRecord[6].PerMax := Strtoint(TextureMaxEdit6.text);
  GPFNodes.CCubes[I].TexturesRecord[7].Selected := TextureCB7.checked;
  GPFNodes.CCubes[I].TexturesRecord[7].Permin := Strtoint(TextureMinEdit7.text);
  GPFNodes.CCubes[I].TexturesRecord[7].PerMax := Strtoint(TextureMaxEdit7.text);
  GPFNodes.CCubes[I].TexturesRecord[8].Selected := TextureCB8.checked;
  GPFNodes.CCubes[I].TexturesRecord[8].Permin := Strtoint(TextureMinEdit8.text);
  GPFNodes.CCubes[I].TexturesRecord[8].PerMax := Strtoint(TextureMaxEdit8.text);
  GPFNodes.CCubes[I].TexturesRecord[9].Selected := TextureCB9.checked;
  GPFNodes.CCubes[I].TexturesRecord[9].Permin := Strtoint(TextureMinEdit9.text);
  GPFNodes.CCubes[I].TexturesRecord[9].PerMax := Strtoint(TextureMaxEdit9.text);
  GPFNodes.CCubes[I].TexturesRecord[10].Selected := TextureCB10.checked;
  GPFNodes.CCubes[I].TexturesRecord[10].Permin :=
    Strtoint(TextureMinEdit10.text);
  GPFNodes.CCubes[I].TexturesRecord[10].PerMax :=
    Strtoint(TextureMaxEdit10.text);
  GPFNodes.CCubes[I].TexturesRecord[11].Selected := TextureCB11.checked;
  GPFNodes.CCubes[I].TexturesRecord[11].Permin :=
    Strtoint(TextureMinEdit11.text);
  GPFNodes.CCubes[I].TexturesRecord[11].PerMax :=
    Strtoint(TextureMaxEdit11.text);
  GPFNodes.CCubes[I].TexturesRecord[12].Selected := TextureCB12.checked;
  GPFNodes.CCubes[I].TexturesRecord[12].Permin :=
    Strtoint(TextureMinEdit12.text);
  GPFNodes.CCubes[I].TexturesRecord[12].PerMax :=
    Strtoint(TextureMaxEdit12.text);
  GPFNodes.CCubes[I].TexturesRecord[13].Selected := TextureCB13.checked;
  GPFNodes.CCubes[I].TexturesRecord[13].Permin :=
    Strtoint(TextureMinEdit13.text);
  GPFNodes.CCubes[I].TexturesRecord[13].PerMax :=
    Strtoint(TextureMaxEdit13.text);
  GPFNodes.CCubes[I].TexturesRecord[14].Selected := TextureCB14.checked;
  GPFNodes.CCubes[I].TexturesRecord[14].Permin :=
    Strtoint(TextureMinEdit14.text);
  GPFNodes.CCubes[I].TexturesRecord[14].PerMax :=
    Strtoint(TextureMaxEdit14.text);
  GPFNodes.CCubes[I].TexturesRecord[15].Selected := TextureCB15.checked;
  GPFNodes.CCubes[I].TexturesRecord[15].Permin :=
    Strtoint(TextureMinEdit15.text);
  GPFNodes.CCubes[I].TexturesRecord[15].PerMax :=
    Strtoint(TextureMaxEdit15.text);
  GPFNodes.CCubes[I].TexturesRecord[16].Selected := TextureCB16.checked;
  GPFNodes.CCubes[I].TexturesRecord[16].Permin :=
    Strtoint(TextureMinEdit16.text);
  GPFNodes.CCubes[I].TexturesRecord[16].PerMax :=
    Strtoint(TextureMaxEdit16.text);
  GPFNodes.CCubes[I].TexturesRecord[17].Selected := TextureCB17.checked;
  GPFNodes.CCubes[I].TexturesRecord[17].Permin :=
    Strtoint(TextureMinEdit17.text);
  GPFNodes.CCubes[I].TexturesRecord[17].PerMax :=
    Strtoint(TextureMaxEdit17.text);
  GPFNodes.CCubes[I].TexturesRecord[18].Selected := TextureCB18.checked;
  GPFNodes.CCubes[I].TexturesRecord[18].Permin :=
    Strtoint(TextureMinEdit18.text);
  GPFNodes.CCubes[I].TexturesRecord[18].PerMax :=
    Strtoint(TextureMaxEdit18.text);
  GPFNodes.CCubes[I].TexturesRecord[19].Selected := TextureCB19.checked;
  GPFNodes.CCubes[I].TexturesRecord[19].Permin :=
    Strtoint(TextureMinEdit19.text);
  GPFNodes.CCubes[I].TexturesRecord[19].PerMax :=
    Strtoint(TextureMaxEdit19.text);
  GPFNodes.CCubes[I].TexturesRecord[20].Selected := TextureCB20.checked;
  GPFNodes.CCubes[I].TexturesRecord[20].Permin :=
    Strtoint(TextureMinEdit20.text);
  GPFNodes.CCubes[I].TexturesRecord[20].PerMax :=
    Strtoint(TextureMaxEdit20.text);
  GPFNodes.CCubes[I].TexturesRecord[21].Selected := TextureCB21.checked;
  GPFNodes.CCubes[I].TexturesRecord[21].Permin :=
    Strtoint(TextureMinEdit21.text);
  GPFNodes.CCubes[I].TexturesRecord[21].PerMax :=
    Strtoint(TextureMaxEdit21.text);
  GPFNodes.CCubes[I].TexturesRecord[22].Selected := TextureCB22.checked;
  GPFNodes.CCubes[I].TexturesRecord[22].Permin :=
    Strtoint(TextureMinEdit22.text);
  GPFNodes.CCubes[I].TexturesRecord[22].PerMax :=
    Strtoint(TextureMaxEdit22.text);
  GPFNodes.CCubes[I].TexturesRecord[23].Selected := TextureCB23.checked;
  GPFNodes.CCubes[I].TexturesRecord[23].Permin :=
    Strtoint(TextureMinEdit23.text);
  GPFNodes.CCubes[I].TexturesRecord[23].PerMax :=
    Strtoint(TextureMaxEdit23.text);
  GPFNodes.CCubes[I].TexturesRecord[24].Selected := TextureCB24.checked;
  GPFNodes.CCubes[I].TexturesRecord[24].Permin :=
    Strtoint(TextureMinEdit24.text);
  GPFNodes.CCubes[I].TexturesRecord[24].PerMax :=
    Strtoint(TextureMaxEdit24.text);

  // 16
  GPFNodes.CCubes[I].TextureSetRecord[1].Selected := Texture16_1CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[2].Selected := Texture16_2CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[3].Selected := Texture16_3CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[4].Selected := Texture16_4CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[5].Selected := Texture16_5CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[6].Selected := Texture16_6CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[7].Selected := Texture16_7CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[8].Selected := Texture16_8CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[9].Selected := Texture16_9CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[10].Selected := Texture16_10CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[11].Selected := Texture16_11CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[12].Selected := Texture16_12CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[13].Selected := Texture16_13CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[14].Selected := Texture16_14CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[15].Selected := Texture16_15CB.checked;
  GPFNodes.CCubes[I].TextureSetRecord[16].Selected := Texture16_16CB.checked;
  // 64
  GPFNodes.CCubes[I].TextureSet64Record[1].Selected := Texture64_1CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[2].Selected := Texture64_2CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[3].Selected := Texture64_3CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[4].Selected := Texture64_4CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[5].Selected := Texture64_5CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[6].Selected := Texture64_6CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[7].Selected := Texture64_7CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[8].Selected := Texture64_8CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[9].Selected := Texture64_9CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[10].Selected := Texture64_10CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[11].Selected := Texture64_11CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[12].Selected := Texture64_12CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[13].Selected := Texture64_13CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[14].Selected := Texture64_14CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[15].Selected := Texture64_15CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[16].Selected := Texture64_16CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[17].Selected := Texture64_17CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[18].Selected := Texture64_18CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[19].Selected := Texture64_19CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[20].Selected := Texture64_20CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[21].Selected := Texture64_21CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[22].Selected := Texture64_22CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[23].Selected := Texture64_23CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[24].Selected := Texture64_24CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[25].Selected := Texture64_25CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[26].Selected := Texture64_26CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[27].Selected := Texture64_27CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[28].Selected := Texture64_28CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[29].Selected := Texture64_29CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[30].Selected := Texture64_30CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[31].Selected := Texture64_31CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[32].Selected := Texture64_32CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[33].Selected := Texture64_33CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[34].Selected := Texture64_34CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[35].Selected := Texture64_35CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[36].Selected := Texture64_36CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[37].Selected := Texture64_37CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[38].Selected := Texture64_38CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[39].Selected := Texture64_39CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[40].Selected := Texture64_40CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[41].Selected := Texture64_41CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[42].Selected := Texture64_42CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[43].Selected := Texture64_43CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[44].Selected := Texture64_44CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[45].Selected := Texture64_45CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[46].Selected := Texture64_46CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[47].Selected := Texture64_47CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[48].Selected := Texture64_48CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[49].Selected := Texture64_49CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[50].Selected := Texture64_50CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[51].Selected := Texture64_51CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[52].Selected := Texture64_52CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[53].Selected := Texture64_53CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[54].Selected := Texture64_54CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[55].Selected := Texture64_55CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[56].Selected := Texture64_56CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[57].Selected := Texture64_57CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[58].Selected := Texture64_58CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[59].Selected := Texture64_59CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[60].Selected := Texture64_60CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[61].Selected := Texture64_61CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[61].Selected := Texture64_62CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[62].Selected := Texture64_63CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[64].Selected := Texture64_64CB.checked;

  SelectedObject := NewObject; // Makes the New one selected
end;

procedure TACloudDemoForm.SetNodeCubeToGPFCube(I: Integer);
begin // TGLDummyCube  .. TGLBaseSceneObject
  GPFNodes.CCubes[I].Position.X := CloudCubeDC.Children[I].Position.X;
  GPFNodes.CCubes[I].Position.Y := CloudCubeDC.Children[I].Position.Y;
  GPFNodes.CCubes[I].Position.Z := CloudCubeDC.Children[I].Position.Z;
  GPFNodes.CCubes[I].Rotation.X := CloudCubeDC.Children[I].Rotation.X;
  GPFNodes.CCubes[I].Rotation.Y := CloudCubeDC.Children[I].Rotation.Y;
  GPFNodes.CCubes[I].Rotation.Z := CloudCubeDC.Children[I].Rotation.Z;
  GPFNodes.CCubes[I].Size.X := TGLCube(CloudCubeDC.Children[I]).CubeWidth;
  GPFNodes.CCubes[I].Size.Y := TGLCube(CloudCubeDC.Children[I]).CubeHeight;
  GPFNodes.CCubes[I].Size.Z := TGLCube(CloudCubeDC.Children[I]).CubeDepth;
  GPFNodes.CCubes[I].GroupColor := TGLCube(CloudCubeDC.Children[I])
    .Material.FrontProperties.Diffuse.AsWinColor;
  // GPFNodes.CCubes[i].GroupNumber:= CloudCubeDC.Children[i].Tag;
  { GPFNodes.CCubes[i].Speed:= CurrentRotationSpeed; }
end;

procedure TACloudDemoForm.SetGPFCubeToNodeCube(I: Integer);
begin
  CloudCubeDC.Children[I].Position.X := GPFNodes.CCubes[I].Position.X;
  CloudCubeDC.Children[I].Position.Y := GPFNodes.CCubes[I].Position.Y;
  CloudCubeDC.Children[I].Position.Z := GPFNodes.CCubes[I].Position.Z;
  // CloudCubeDC.Children[i].Rotation.X:=GPFNodes[i].Rotation.X;
  // CloudCubeDC.Children[i].Rotation.Y:=GPFNodes[i].Rotation.Y;
  // CloudCubeDC.Children[i].Rotation.Z:=GPFNodes[i].Rotation.Z;
  CloudCubeDC.Children[I].ResetAndPitchTurnRoll(GPFNodes.CCubes[I].Rotation.X,
    GPFNodes.CCubes[I].Rotation.Y, GPFNodes.CCubes[I].Rotation.Z);

  TGLCube(CloudCubeDC.Children[I]).CubeWidth := GPFNodes.CCubes[I].Size.X;
  TGLCube(CloudCubeDC.Children[I]).CubeHeight := GPFNodes.CCubes[I].Size.Y;
  TGLCube(CloudCubeDC.Children[I]).CubeDepth := GPFNodes.CCubes[I].Size.Z;
  TGLCube(CloudCubeDC.Children[I]).Material.FrontProperties.Diffuse.AsWinColor
    := GPFNodes.CCubes[I].GroupColor;
  // Tag is used by Gizmo
  // CloudCubeDC.Children[i].Tag:=GPFNodes.CCubes[i].GroupNumber;

  { CurrentRotationSpeed:=GPFNodes.CCubes[i].Speed; }
end;

procedure TACloudDemoForm.SetGPFCubeToText(I: Integer);
// var   j:Integer;    Tempname:String;
begin
  PositionXEdit.text := Floattostr(GPFNodes.CCubes[I].Position.X);
  PositionYEdit.text := Floattostr(GPFNodes.CCubes[I].Position.Y);
  PositionZEdit.text := Floattostr(GPFNodes.CCubes[I].Position.Z);
  PitchEdit.text := Floattostr(GPFNodes.CCubes[I].Rotation.X);
  TurnEdit.text := Floattostr(GPFNodes.CCubes[I].Rotation.Y);
  RollEdit.text := Floattostr(GPFNodes.CCubes[I].Rotation.Z);
  WidthEdit.text := Floattostr(GPFNodes.CCubes[I].Size.X);
  DepthYEdit.text := Floattostr(GPFNodes.CCubes[I].Size.Y);
  HeightZEdit.text := Floattostr(GPFNodes.CCubes[I].Size.Z);

  SpeedXEdit.text := Floattostr(GPFNodes.CCubes[I].SpeedX);
  SpeedYEdit.text := Floattostr(GPFNodes.CCubes[I].SpeedY);
  SpeedZEdit.text := Floattostr(GPFNodes.CCubes[I].SpeedZ);
  ShapeUpxyzRG.ItemIndex := GPFNodes.CCubes[I].Upxyzw;
  ShapeShowObjectCB.checked := GPFNodes.CCubes[I].ShapeShowObject;
  ShapeSizeCubeCB.checked := GPFNodes.CCubes[I].ShapeSizeCube;
  ShapeUpNegativeCB.checked := GPFNodes.CCubes[I].ShapeUpNegative;
  ShapeSizeSpriteCB.checked := GPFNodes.CCubes[I].ShapeSpriteSize;
  ShapeResolutionEdit.text := inttostr(GPFNodes.CCubes[I].ShapeResolution);
  GroupColorPanel.Color := GPFNodes.CCubes[I].GroupColor;
  GroupColorEdit.text := inttostr(GPFNodes.CCubes[I].GroupNumber);
  // GPFNodes.CCubes[i].Generated:= False;
  PartsEdit.text := inttostr(GPFNodes.CCubes[I].SpriteCount);
  XMaxEdit.text := Floattostr(GPFNodes.CCubes[I].XMax);
  XMinEdit.text := Floattostr(GPFNodes.CCubes[I].XMin);
  YMaxEdit.text := Floattostr(GPFNodes.CCubes[I].YMax);
  YMinEdit.text := Floattostr(GPFNodes.CCubes[I].YMin);
  ZMaxEdit.text := Floattostr(GPFNodes.CCubes[I].ZMax);
  ZMinEdit.text := Floattostr(GPFNodes.CCubes[I].ZMin);
  RotationMaxEdit.text := inttostr(GPFNodes.CCubes[I].RMax);
  RotationMinEdit.text := inttostr(GPFNodes.CCubes[I].RMin);

  TextureTypeRG.ItemIndex := GPFNodes.CCubes[I].TextureType;
  CloudTypeRG.ItemIndex := GPFNodes.CCubes[I].CloudType;
  CloudShapeRG.ItemIndex := GPFNodes.CCubes[I].CloudShape;
  CloudPatternRG.ItemIndex := GPFNodes.CCubes[I].CloudPattern;
  SpriteTypeRG.ItemIndex := GPFNodes.CCubes[I].SpriteType;

  Albedo1REdit.text := Floattostr(GPFNodes.CCubes[I].Albedo1R);
  Albedo1GEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo1G);
  Albedo1BEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo1B);
  Albedo1AEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo1A);
  Albedo2REdit.text := Floattostr(GPFNodes.CCubes[I].Albedo2R);
  Albedo2GEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo2G);
  Albedo2BEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo2B);
  Albedo2AEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo2A);
  Albedo3REdit.text := Floattostr(GPFNodes.CCubes[I].Albedo3R);
  Albedo3GEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo3G);
  Albedo3BEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo3B);
  Albedo3AEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo3A);
  Albedo4REdit.text := Floattostr(GPFNodes.CCubes[I].Albedo4R);
  Albedo4GEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo4G);
  Albedo4BEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo4B);
  Albedo4AEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo4A);
  Albedo5REdit.text := Floattostr(GPFNodes.CCubes[I].Albedo5R);
  Albedo5GEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo5G);
  Albedo5BEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo5B);
  Albedo5AEdit.text := Floattostr(GPFNodes.CCubes[I].Albedo5A);
  AlphaInitialEdit.text := Floattostr(GPFNodes.CCubes[I].AlphaStart);
  AlphaRateEdit.text := Floattostr(GPFNodes.CCubes[I].AlphaRate);
  AlphaFormationEdit.text := Floattostr(GPFNodes.CCubes[I].AlphaFormation);
  AlphaDissipationEdit.text := Floattostr(GPFNodes.CCubes[I].AlphaDissipation);

  PLPSizeEdit.text := Floattostr(GPFNodes.CCubes[I].PLPSize);
  PLPLifeEdit.text := Floattostr(GPFNodes.CCubes[I].PLPLife);
  PLPIntervalEdit.text := Floattostr(GPFNodes.CCubes[I].PLPInterval);
  PLDepthEdit.text := Floattostr(GPFNodes.CCubes[I].PLDepth);
  PLScaleEdit.text := Floattostr(GPFNodes.CCubes[I].PLScale);
  PLCharEdit.text := GPFNodes.CCubes[I].PLMaskString;
  PLCharacterSizeEdit.text := inttostr(GPFNodes.CCubes[I].PLMaskStringSize);
  PLMaskTypeRG.ItemIndex := GPFNodes.CCubes[I].PLMaskType;
  PLMaskImageSizeTB.Position := GPFNodes.CCubes[I].PLMaskImageSize;
  // PLMaskImageSizeChange(NewSize:Integer);  //On load
  PLMaskMaskXOffsetTB.Position := GPFNodes.CCubes[I].PLMaskXOffset;
  LabelX.Caption := inttostr(GPFNodes.CCubes[I].PLMaskXOffset);
  PLMaskMaskYOffsetTB.Position := GPFNodes.CCubes[I].PLMaskYOffset;
  LabelY.Caption := inttostr(GPFNodes.CCubes[I].PLMaskYOffset);
  PLMaskMaskZOffsetTB.Position := GPFNodes.CCubes[I].PLMaskZOffset;
  LabelZ.Caption := inttostr(GPFNodes.CCubes[I].PLMaskZOffset);
  PLMaskMenuNumberStored := GPFNodes.CCubes[I].PLMaskMenuNumber;
  // PLMaskMenuSet(MaskMenuNumber,XOffset,YOffset,ZOffset,ImageSize:Integer);
  PLColorPanel.Color := GPFNodes.CCubes[I].PLColor;
  PlLoadXBtn.Caption := GPFNodes.CCubes[I].PLXImageFileName;
  PLPitchEdit.text := Floattostr(GPFNodes.CCubes[I].PLPitch);
  PLRollEdit.text := Floattostr(GPFNodes.CCubes[I].PLRoll);
  PLTurnEdit.text := Floattostr(GPFNodes.CCubes[I].PLTurn);

  TextureCB1.checked := GPFNodes.CCubes[I].TexturesRecord[1].Selected;
  TextureMinEdit1.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[1].Permin);
  TextureMaxEdit1.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[1].PerMax);
  TextureCB2.checked := GPFNodes.CCubes[I].TexturesRecord[2].Selected;
  TextureMinEdit2.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[2].Permin);
  TextureMaxEdit2.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[2].PerMax);
  TextureCB3.checked := GPFNodes.CCubes[I].TexturesRecord[3].Selected;
  TextureMinEdit3.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[3].Permin);
  TextureMaxEdit3.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[3].PerMax);
  TextureCB4.checked := GPFNodes.CCubes[I].TexturesRecord[4].Selected;
  TextureMinEdit4.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[4].Permin);
  TextureMaxEdit4.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[4].PerMax);
  TextureCB5.checked := GPFNodes.CCubes[I].TexturesRecord[5].Selected;
  TextureMinEdit5.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[5].Permin);
  TextureMaxEdit5.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[5].PerMax);
  TextureCB6.checked := GPFNodes.CCubes[I].TexturesRecord[6].Selected;
  TextureMinEdit6.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[6].Permin);
  TextureMaxEdit6.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[6].PerMax);
  TextureCB7.checked := GPFNodes.CCubes[I].TexturesRecord[7].Selected;
  TextureMinEdit7.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[7].Permin);
  TextureMaxEdit7.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[7].PerMax);
  TextureCB8.checked := GPFNodes.CCubes[I].TexturesRecord[8].Selected;
  TextureMinEdit8.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[8].Permin);
  TextureMaxEdit8.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[8].PerMax);
  TextureCB9.checked := GPFNodes.CCubes[I].TexturesRecord[9].Selected;
  TextureMinEdit9.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[9].Permin);
  TextureMaxEdit9.text := inttostr(GPFNodes.CCubes[I].TexturesRecord[9].PerMax);
  TextureCB10.checked := GPFNodes.CCubes[I].TexturesRecord[10].Selected;
  TextureMinEdit10.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[10].Permin);
  TextureMaxEdit10.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[10].PerMax);
  TextureCB11.checked := GPFNodes.CCubes[I].TexturesRecord[11].Selected;
  TextureMinEdit11.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[11].Permin);
  TextureMaxEdit11.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[11].PerMax);
  TextureCB12.checked := GPFNodes.CCubes[I].TexturesRecord[12].Selected;
  TextureMinEdit12.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[12].Permin);
  TextureMaxEdit12.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[12].PerMax);
  TextureCB13.checked := GPFNodes.CCubes[I].TexturesRecord[13].Selected;
  TextureMinEdit13.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[13].Permin);
  TextureMaxEdit13.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[13].PerMax);
  TextureCB14.checked := GPFNodes.CCubes[I].TexturesRecord[14].Selected;
  TextureMinEdit14.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[14].Permin);
  TextureMaxEdit14.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[14].PerMax);
  TextureCB15.checked := GPFNodes.CCubes[I].TexturesRecord[15].Selected;
  TextureMinEdit15.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[15].Permin);
  TextureMaxEdit15.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[15].PerMax);
  TextureCB16.checked := GPFNodes.CCubes[I].TexturesRecord[16].Selected;
  TextureMinEdit16.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[16].Permin);
  TextureMaxEdit16.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[16].PerMax);
  TextureCB17.checked := GPFNodes.CCubes[I].TexturesRecord[17].Selected;
  TextureMinEdit17.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[17].Permin);
  TextureMaxEdit17.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[17].PerMax);
  TextureCB18.checked := GPFNodes.CCubes[I].TexturesRecord[18].Selected;
  TextureMinEdit18.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[18].Permin);
  TextureMaxEdit18.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[18].PerMax);
  TextureCB19.checked := GPFNodes.CCubes[I].TexturesRecord[19].Selected;
  TextureMinEdit19.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[19].Permin);
  TextureMaxEdit19.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[19].PerMax);
  TextureCB20.checked := GPFNodes.CCubes[I].TexturesRecord[20].Selected;
  TextureMinEdit20.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[20].Permin);
  TextureMaxEdit20.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[20].PerMax);
  TextureCB21.checked := GPFNodes.CCubes[I].TexturesRecord[21].Selected;
  TextureMinEdit21.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[21].Permin);
  TextureMaxEdit21.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[21].PerMax);
  TextureCB22.checked := GPFNodes.CCubes[I].TexturesRecord[22].Selected;
  TextureMinEdit22.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[22].Permin);
  TextureMaxEdit22.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[22].PerMax);
  TextureCB23.checked := GPFNodes.CCubes[I].TexturesRecord[23].Selected;
  TextureMinEdit23.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[23].Permin);
  TextureMaxEdit23.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[23].PerMax);
  TextureCB24.checked := GPFNodes.CCubes[I].TexturesRecord[24].Selected;
  TextureMinEdit24.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[24].Permin);
  TextureMaxEdit24.text :=
    inttostr(GPFNodes.CCubes[I].TexturesRecord[24].PerMax);

  Texture16_1CB.checked := GPFNodes.CCubes[I].TextureSetRecord[1].Selected;
  Texture16_2CB.checked := GPFNodes.CCubes[I].TextureSetRecord[2].Selected;
  Texture16_3CB.checked := GPFNodes.CCubes[I].TextureSetRecord[3].Selected;
  Texture16_4CB.checked := GPFNodes.CCubes[I].TextureSetRecord[4].Selected;
  Texture16_5CB.checked := GPFNodes.CCubes[I].TextureSetRecord[5].Selected;
  Texture16_6CB.checked := GPFNodes.CCubes[I].TextureSetRecord[6].Selected;
  Texture16_7CB.checked := GPFNodes.CCubes[I].TextureSetRecord[7].Selected;
  Texture16_8CB.checked := GPFNodes.CCubes[I].TextureSetRecord[8].Selected;
  Texture16_9CB.checked := GPFNodes.CCubes[I].TextureSetRecord[9].Selected;
  Texture16_10CB.checked := GPFNodes.CCubes[I].TextureSetRecord[10].Selected;
  Texture16_11CB.checked := GPFNodes.CCubes[I].TextureSetRecord[11].Selected;
  Texture16_12CB.checked := GPFNodes.CCubes[I].TextureSetRecord[12].Selected;
  Texture16_13CB.checked := GPFNodes.CCubes[I].TextureSetRecord[13].Selected;
  Texture16_14CB.checked := GPFNodes.CCubes[I].TextureSetRecord[14].Selected;
  Texture16_15CB.checked := GPFNodes.CCubes[I].TextureSetRecord[15].Selected;
  Texture16_16CB.checked := GPFNodes.CCubes[I].TextureSetRecord[16].Selected;

  Texture64_1CB.checked := GPFNodes.CCubes[I].TextureSet64Record[1].Selected;
  Texture64_2CB.checked := GPFNodes.CCubes[I].TextureSet64Record[2].Selected;
  Texture64_3CB.checked := GPFNodes.CCubes[I].TextureSet64Record[3].Selected;
  Texture64_4CB.checked := GPFNodes.CCubes[I].TextureSet64Record[4].Selected;
  Texture64_5CB.checked := GPFNodes.CCubes[I].TextureSet64Record[5].Selected;
  Texture64_6CB.checked := GPFNodes.CCubes[I].TextureSet64Record[6].Selected;
  Texture64_7CB.checked := GPFNodes.CCubes[I].TextureSet64Record[7].Selected;
  Texture64_8CB.checked := GPFNodes.CCubes[I].TextureSet64Record[8].Selected;
  Texture64_9CB.checked := GPFNodes.CCubes[I].TextureSet64Record[9].Selected;
  Texture64_10CB.checked := GPFNodes.CCubes[I].TextureSet64Record[10].Selected;
  Texture64_11CB.checked := GPFNodes.CCubes[I].TextureSet64Record[11].Selected;
  Texture64_12CB.checked := GPFNodes.CCubes[I].TextureSet64Record[12].Selected;
  Texture64_13CB.checked := GPFNodes.CCubes[I].TextureSet64Record[13].Selected;
  Texture64_14CB.checked := GPFNodes.CCubes[I].TextureSet64Record[14].Selected;
  Texture64_15CB.checked := GPFNodes.CCubes[I].TextureSet64Record[15].Selected;
  Texture64_16CB.checked := GPFNodes.CCubes[I].TextureSet64Record[16].Selected;
  Texture64_17CB.checked := GPFNodes.CCubes[I].TextureSet64Record[17].Selected;
  Texture64_18CB.checked := GPFNodes.CCubes[I].TextureSet64Record[18].Selected;
  Texture64_19CB.checked := GPFNodes.CCubes[I].TextureSet64Record[19].Selected;
  Texture64_20CB.checked := GPFNodes.CCubes[I].TextureSet64Record[20].Selected;
  Texture64_21CB.checked := GPFNodes.CCubes[I].TextureSet64Record[21].Selected;
  Texture64_22CB.checked := GPFNodes.CCubes[I].TextureSet64Record[22].Selected;
  Texture64_23CB.checked := GPFNodes.CCubes[I].TextureSet64Record[23].Selected;
  Texture64_24CB.checked := GPFNodes.CCubes[I].TextureSet64Record[24].Selected;
  Texture64_25CB.checked := GPFNodes.CCubes[I].TextureSet64Record[25].Selected;
  Texture64_26CB.checked := GPFNodes.CCubes[I].TextureSet64Record[26].Selected;
  Texture64_27CB.checked := GPFNodes.CCubes[I].TextureSet64Record[27].Selected;
  Texture64_28CB.checked := GPFNodes.CCubes[I].TextureSet64Record[28].Selected;
  Texture64_29CB.checked := GPFNodes.CCubes[I].TextureSet64Record[29].Selected;
  Texture64_30CB.checked := GPFNodes.CCubes[I].TextureSet64Record[30].Selected;
  Texture64_31CB.checked := GPFNodes.CCubes[I].TextureSet64Record[31].Selected;
  Texture64_32CB.checked := GPFNodes.CCubes[I].TextureSet64Record[32].Selected;
  Texture64_33CB.checked := GPFNodes.CCubes[I].TextureSet64Record[33].Selected;
  Texture64_34CB.checked := GPFNodes.CCubes[I].TextureSet64Record[34].Selected;
  Texture64_35CB.checked := GPFNodes.CCubes[I].TextureSet64Record[35].Selected;
  Texture64_36CB.checked := GPFNodes.CCubes[I].TextureSet64Record[36].Selected;
  Texture64_37CB.checked := GPFNodes.CCubes[I].TextureSet64Record[37].Selected;
  Texture64_38CB.checked := GPFNodes.CCubes[I].TextureSet64Record[38].Selected;
  Texture64_39CB.checked := GPFNodes.CCubes[I].TextureSet64Record[39].Selected;
  Texture64_40CB.checked := GPFNodes.CCubes[I].TextureSet64Record[40].Selected;
  Texture64_41CB.checked := GPFNodes.CCubes[I].TextureSet64Record[41].Selected;
  Texture64_42CB.checked := GPFNodes.CCubes[I].TextureSet64Record[42].Selected;
  Texture64_43CB.checked := GPFNodes.CCubes[I].TextureSet64Record[43].Selected;
  Texture64_44CB.checked := GPFNodes.CCubes[I].TextureSet64Record[44].Selected;
  Texture64_45CB.checked := GPFNodes.CCubes[I].TextureSet64Record[45].Selected;
  Texture64_46CB.checked := GPFNodes.CCubes[I].TextureSet64Record[46].Selected;
  Texture64_47CB.checked := GPFNodes.CCubes[I].TextureSet64Record[47].Selected;
  Texture64_48CB.checked := GPFNodes.CCubes[I].TextureSet64Record[48].Selected;
  Texture64_49CB.checked := GPFNodes.CCubes[I].TextureSet64Record[49].Selected;
  Texture64_50CB.checked := GPFNodes.CCubes[I].TextureSet64Record[50].Selected;
  Texture64_51CB.checked := GPFNodes.CCubes[I].TextureSet64Record[51].Selected;
  Texture64_52CB.checked := GPFNodes.CCubes[I].TextureSet64Record[52].Selected;
  Texture64_53CB.checked := GPFNodes.CCubes[I].TextureSet64Record[53].Selected;
  Texture64_54CB.checked := GPFNodes.CCubes[I].TextureSet64Record[54].Selected;
  Texture64_55CB.checked := GPFNodes.CCubes[I].TextureSet64Record[55].Selected;
  Texture64_56CB.checked := GPFNodes.CCubes[I].TextureSet64Record[56].Selected;
  Texture64_57CB.checked := GPFNodes.CCubes[I].TextureSet64Record[57].Selected;
  Texture64_58CB.checked := GPFNodes.CCubes[I].TextureSet64Record[58].Selected;
  Texture64_59CB.checked := GPFNodes.CCubes[I].TextureSet64Record[59].Selected;
  Texture64_60CB.checked := GPFNodes.CCubes[I].TextureSet64Record[60].Selected;
  Texture64_61CB.checked := GPFNodes.CCubes[I].TextureSet64Record[61].Selected;
  Texture64_62CB.checked := GPFNodes.CCubes[I].TextureSet64Record[62].Selected;
  Texture64_63CB.checked := GPFNodes.CCubes[I].TextureSet64Record[63].Selected;
  Texture64_64CB.checked := GPFNodes.CCubes[I].TextureSet64Record[64].Selected;

  { SpeedEdit.Text:=Floattostr(GPFNodes.CCubes[i].Speed); }
  { CurrentRotationSpeed :=GPFNodes[i].Speed;
    NumberEdit.Text:= Inttostr(GPFNodes[i].NodeNumber); }
end;

// Rotation.X means PitchAngle;
// Rotation.Y means TurnAngle;
// Rotation.Z means RollAngle;
procedure TACloudDemoForm.SetTextToGPFCube(I: Integer);
begin
  GPFNodes.CCubes[I].Position.X := StrToFloatDef(PositionXEdit.text);
  GPFNodes.CCubes[I].Position.Y := StrToFloatDef(PositionYEdit.text);
  GPFNodes.CCubes[I].Position.Z := StrToFloatDef(PositionZEdit.text);
  GPFNodes.CCubes[I].Rotation.X := StrToFloatDef(PitchEdit.text);
  GPFNodes.CCubes[I].Rotation.Y := StrToFloatDef(TurnEdit.text);
  GPFNodes.CCubes[I].Rotation.Z := StrToFloatDef(RollEdit.text);
  GPFNodes.CCubes[I].Size.X := StrToFloatDef(WidthEdit.text);
  GPFNodes.CCubes[I].Size.Y := StrToFloatDef(DepthYEdit.text);
  GPFNodes.CCubes[I].Size.Z := StrToFloatDef(HeightZEdit.text);
  GPFNodes.CCubes[I].SpeedX := StrToFloatDef(SpeedXEdit.text); // 1;
  GPFNodes.CCubes[I].SpeedY := StrToFloatDef(SpeedYEdit.text); // 1;
  GPFNodes.CCubes[I].SpeedZ := StrToFloatDef(SpeedZEdit.text); // 1;
  GPFNodes.CCubes[I].Upxyzw := ShapeUpxyzRG.ItemIndex;
  GPFNodes.CCubes[I].ShapeShowObject := ShapeShowObjectCB.checked;
  GPFNodes.CCubes[I].ShapeSizeCube := ShapeSizeCubeCB.checked;
  GPFNodes.CCubes[I].ShapeUpNegative := ShapeUpNegativeCB.checked;
  GPFNodes.CCubes[I].ShapeSpriteSize := ShapeSizeSpriteCB.checked;
  GPFNodes.CCubes[I].ShapeResolution := Strtoint(ShapeResolutionEdit.text);
  GPFNodes.CCubes[I].GroupColor := GroupColorPanel.Color;
  GPFNodes.CCubes[I].GroupNumber := Strtoint(GroupColorEdit.text);
  // GPFNodes.CCubes[i].Generated:= False;
  { GPFNodes.CCubes[i].Speed:=StrtoFloat(SpeedEdit.Text);
    CurrentRotationSpeed:=GPFNodes.CCubes[i].Speed; }
  GPFNodes.CCubes[I].SpriteCount := Strtoint(PartsEdit.text);
  GPFNodes.CCubes[I].XMax := StrToFloatDef(XMaxEdit.text);
  GPFNodes.CCubes[I].XMin := StrToFloatDef(XMinEdit.text);
  GPFNodes.CCubes[I].YMax := StrToFloatDef(YMaxEdit.text);
  GPFNodes.CCubes[I].YMin := StrToFloatDef(YMinEdit.text);
  GPFNodes.CCubes[I].ZMax := StrToFloatDef(ZMaxEdit.text);
  GPFNodes.CCubes[I].ZMin := StrToFloatDef(ZMinEdit.text);
  GPFNodes.CCubes[I].RMax := Strtoint(RotationMaxEdit.text);
  GPFNodes.CCubes[I].RMin := Strtoint(RotationMinEdit.text);
  GPFNodes.CCubes[I].SpriteType := SpriteTypeRG.ItemIndex;
  GPFNodes.CCubes[I].TextureType := TextureTypeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudType := CloudTypeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudShape := CloudShapeRG.ItemIndex;
  GPFNodes.CCubes[I].CloudPattern := CloudPatternRG.ItemIndex;
  GPFNodes.CCubes[I].Albedo1R := StrToFloatDef(Albedo1REdit.text);
  GPFNodes.CCubes[I].Albedo1G := StrToFloatDef(Albedo1GEdit.text);
  GPFNodes.CCubes[I].Albedo1B := StrToFloatDef(Albedo1BEdit.text);
  GPFNodes.CCubes[I].Albedo1A := StrToFloatDef(Albedo1AEdit.text);
  GPFNodes.CCubes[I].Albedo2R := StrToFloatDef(Albedo2REdit.text);
  GPFNodes.CCubes[I].Albedo2G := StrToFloatDef(Albedo2GEdit.text);
  GPFNodes.CCubes[I].Albedo2B := StrToFloatDef(Albedo2BEdit.text);
  GPFNodes.CCubes[I].Albedo2A := StrToFloatDef(Albedo2AEdit.text);
  GPFNodes.CCubes[I].Albedo3R := StrToFloatDef(Albedo3REdit.text);
  GPFNodes.CCubes[I].Albedo3G := StrToFloatDef(Albedo3GEdit.text);
  GPFNodes.CCubes[I].Albedo3B := StrToFloatDef(Albedo3BEdit.text);
  GPFNodes.CCubes[I].Albedo3A := StrToFloatDef(Albedo3AEdit.text);
  GPFNodes.CCubes[I].Albedo4R := StrToFloatDef(Albedo4REdit.text);
  GPFNodes.CCubes[I].Albedo4G := StrToFloatDef(Albedo4GEdit.text);
  GPFNodes.CCubes[I].Albedo4B := StrToFloatDef(Albedo4BEdit.text);
  GPFNodes.CCubes[I].Albedo4A := StrToFloatDef(Albedo4AEdit.text);
  GPFNodes.CCubes[I].Albedo5R := StrToFloatDef(Albedo5REdit.text);
  GPFNodes.CCubes[I].Albedo5G := StrToFloatDef(Albedo5GEdit.text);
  GPFNodes.CCubes[I].Albedo5B := StrToFloatDef(Albedo5BEdit.text);
  GPFNodes.CCubes[I].Albedo5A := StrToFloatDef(Albedo5AEdit.text);
  GPFNodes.CCubes[I].AlphaStart := StrToFloatDef(AlphaInitialEdit.text);
  GPFNodes.CCubes[I].AlphaRate := StrToFloatDef(AlphaRateEdit.text);
  GPFNodes.CCubes[I].AlphaFormation :=
    StrToFloatDef(AlphaFormationEdit.text);
  GPFNodes.CCubes[I].AlphaDissipation :=
    StrToFloatDef(AlphaDissipationEdit.text);

  GPFNodes.CCubes[I].PLPSize := StrToFloatDef(PLPSizeEdit.text, 0.3);
  GPFNodes.CCubes[I].PLPLife := StrToFloatDef(PLPLifeEdit.text, 1.0);
  GPFNodes.CCubes[I].PLPInterval := StrToFloatDef(PLPIntervalEdit.text, 0.003);
  GPFNodes.CCubes[I].PLDepth := StrToIntDef(PLDepthEdit.text, 3);
  GPFNodes.CCubes[I].PLScale := StrToFloatDef(PLScaleEdit.text, 1);
  GPFNodes.CCubes[I].PLMaskString := PLCharEdit.text;
  GPFNodes.CCubes[I].PLMaskStringSize :=
    StrToIntDef(PLCharacterSizeEdit.text, 16);
  GPFNodes.CCubes[I].PLMaskType := PLMaskTypeRG.ItemIndex;
  GPFNodes.CCubes[I].PLMaskImageSize := PLMaskImageSizeTB.Position;
  // PLMaskImageSizeChange(NewSize:Integer);  //On load
  GPFNodes.CCubes[I].PLMaskXOffset := PLMaskMaskXOffsetTB.Position;
  LabelX.Caption := inttostr(GPFNodes.CCubes[I].PLMaskXOffset);
  GPFNodes.CCubes[I].PLMaskYOffset := PLMaskMaskYOffsetTB.Position;
  LabelY.Caption := inttostr(GPFNodes.CCubes[I].PLMaskYOffset);
  GPFNodes.CCubes[I].PLMaskZOffset := PLMaskMaskZOffsetTB.Position;
  LabelZ.Caption := inttostr(GPFNodes.CCubes[I].PLMaskZOffset);
  GPFNodes.CCubes[I].PLMaskMenuNumber := PLMaskMenuNumberStored;
  // PLMaskMenuSet(MaskMenuNumber,XOffset,YOffset,ZOffset,ImageSize:Integer);
  GPFNodes.CCubes[I].PLColor := PLColorPanel.Color;
  GPFNodes.CCubes[I].PLXImageFileName := PlLoadXBtn.Caption;
  GPFNodes.CCubes[I].PLPitch := StrToFloatDef(PLPitchEdit.text, -90);
  GPFNodes.CCubes[I].PLRoll := StrToFloatDef(PLRollEdit.text, 0);
  GPFNodes.CCubes[I].PLTurn := StrToFloatDef(PLTurnEdit.text, 0);

  GPFNodes.CCubes[I].TexturesRecord[1].Selected := TextureCB1.checked;
  GPFNodes.CCubes[I].TexturesRecord[1].Permin := Strtoint(TextureMinEdit1.text);
  GPFNodes.CCubes[I].TexturesRecord[1].PerMax := Strtoint(TextureMaxEdit1.text);
  GPFNodes.CCubes[I].TexturesRecord[2].Selected := TextureCB2.checked;
  GPFNodes.CCubes[I].TexturesRecord[2].Permin := Strtoint(TextureMinEdit2.text);
  GPFNodes.CCubes[I].TexturesRecord[2].PerMax := Strtoint(TextureMaxEdit2.text);
  GPFNodes.CCubes[I].TexturesRecord[3].Selected := TextureCB3.checked;
  GPFNodes.CCubes[I].TexturesRecord[3].Permin := Strtoint(TextureMinEdit3.text);
  GPFNodes.CCubes[I].TexturesRecord[3].PerMax := Strtoint(TextureMaxEdit3.text);
  GPFNodes.CCubes[I].TexturesRecord[4].Selected := TextureCB4.checked;
  GPFNodes.CCubes[I].TexturesRecord[4].Permin := Strtoint(TextureMinEdit4.text);
  GPFNodes.CCubes[I].TexturesRecord[4].PerMax := Strtoint(TextureMaxEdit4.text);
  GPFNodes.CCubes[I].TexturesRecord[5].Selected := TextureCB5.checked;
  GPFNodes.CCubes[I].TexturesRecord[5].Permin := Strtoint(TextureMinEdit5.text);
  GPFNodes.CCubes[I].TexturesRecord[5].PerMax := Strtoint(TextureMaxEdit5.text);
  GPFNodes.CCubes[I].TexturesRecord[6].Selected := TextureCB6.checked;
  GPFNodes.CCubes[I].TexturesRecord[6].Permin := Strtoint(TextureMinEdit6.text);
  GPFNodes.CCubes[I].TexturesRecord[6].PerMax := Strtoint(TextureMaxEdit6.text);
  GPFNodes.CCubes[I].TexturesRecord[7].Selected := TextureCB7.checked;
  GPFNodes.CCubes[I].TexturesRecord[7].Permin := Strtoint(TextureMinEdit7.text);
  GPFNodes.CCubes[I].TexturesRecord[7].PerMax := Strtoint(TextureMaxEdit7.text);
  GPFNodes.CCubes[I].TexturesRecord[8].Selected := TextureCB8.checked;
  GPFNodes.CCubes[I].TexturesRecord[8].Permin := Strtoint(TextureMinEdit8.text);
  GPFNodes.CCubes[I].TexturesRecord[8].PerMax := Strtoint(TextureMaxEdit8.text);
  GPFNodes.CCubes[I].TexturesRecord[9].Selected := TextureCB9.checked;
  GPFNodes.CCubes[I].TexturesRecord[9].Permin := Strtoint(TextureMinEdit9.text);
  GPFNodes.CCubes[I].TexturesRecord[9].PerMax := Strtoint(TextureMaxEdit9.text);
  GPFNodes.CCubes[I].TexturesRecord[10].Selected := TextureCB10.checked;
  GPFNodes.CCubes[I].TexturesRecord[10].Permin :=
    Strtoint(TextureMinEdit10.text);
  GPFNodes.CCubes[I].TexturesRecord[10].PerMax :=
    Strtoint(TextureMaxEdit10.text);
  GPFNodes.CCubes[I].TexturesRecord[11].Selected := TextureCB11.checked;
  GPFNodes.CCubes[I].TexturesRecord[11].Permin :=
    Strtoint(TextureMinEdit11.text);
  GPFNodes.CCubes[I].TexturesRecord[11].PerMax :=
    Strtoint(TextureMaxEdit11.text);
  GPFNodes.CCubes[I].TexturesRecord[12].Selected := TextureCB12.checked;
  GPFNodes.CCubes[I].TexturesRecord[12].Permin :=
    Strtoint(TextureMinEdit12.text);
  GPFNodes.CCubes[I].TexturesRecord[12].PerMax :=
    Strtoint(TextureMaxEdit12.text);
  GPFNodes.CCubes[I].TexturesRecord[13].Selected := TextureCB13.checked;
  GPFNodes.CCubes[I].TexturesRecord[13].Permin :=
    Strtoint(TextureMinEdit13.text);
  GPFNodes.CCubes[I].TexturesRecord[13].PerMax :=
    Strtoint(TextureMaxEdit13.text);
  GPFNodes.CCubes[I].TexturesRecord[14].Selected := TextureCB14.checked;
  GPFNodes.CCubes[I].TexturesRecord[14].Permin :=
    Strtoint(TextureMinEdit14.text);
  GPFNodes.CCubes[I].TexturesRecord[14].PerMax :=
    Strtoint(TextureMaxEdit14.text);
  GPFNodes.CCubes[I].TexturesRecord[15].Selected := TextureCB15.checked;
  GPFNodes.CCubes[I].TexturesRecord[15].Permin :=
    Strtoint(TextureMinEdit15.text);
  GPFNodes.CCubes[I].TexturesRecord[15].PerMax :=
    Strtoint(TextureMaxEdit15.text);
  GPFNodes.CCubes[I].TexturesRecord[16].Selected := TextureCB16.checked;
  GPFNodes.CCubes[I].TexturesRecord[16].Permin :=
    Strtoint(TextureMinEdit16.text);
  GPFNodes.CCubes[I].TexturesRecord[16].PerMax :=
    Strtoint(TextureMaxEdit16.text);
  GPFNodes.CCubes[I].TexturesRecord[17].Selected := TextureCB17.checked;
  GPFNodes.CCubes[I].TexturesRecord[17].Permin :=
    Strtoint(TextureMinEdit17.text);
  GPFNodes.CCubes[I].TexturesRecord[17].PerMax :=
    Strtoint(TextureMaxEdit17.text);
  GPFNodes.CCubes[I].TexturesRecord[18].Selected := TextureCB18.checked;
  GPFNodes.CCubes[I].TexturesRecord[18].Permin :=
    Strtoint(TextureMinEdit18.text);
  GPFNodes.CCubes[I].TexturesRecord[18].PerMax :=
    Strtoint(TextureMaxEdit18.text);
  GPFNodes.CCubes[I].TexturesRecord[19].Selected := TextureCB19.checked;
  GPFNodes.CCubes[I].TexturesRecord[19].Permin :=
    Strtoint(TextureMinEdit19.text);
  GPFNodes.CCubes[I].TexturesRecord[19].PerMax :=
    Strtoint(TextureMaxEdit19.text);
  GPFNodes.CCubes[I].TexturesRecord[20].Selected := TextureCB20.checked;
  GPFNodes.CCubes[I].TexturesRecord[20].Permin :=
    Strtoint(TextureMinEdit20.text);
  GPFNodes.CCubes[I].TexturesRecord[20].PerMax :=
    Strtoint(TextureMaxEdit20.text);
  GPFNodes.CCubes[I].TexturesRecord[21].Selected := TextureCB21.checked;
  GPFNodes.CCubes[I].TexturesRecord[21].Permin :=
    Strtoint(TextureMinEdit21.text);
  GPFNodes.CCubes[I].TexturesRecord[21].PerMax :=
    Strtoint(TextureMaxEdit21.text);
  GPFNodes.CCubes[I].TexturesRecord[22].Selected := TextureCB22.checked;
  GPFNodes.CCubes[I].TexturesRecord[22].Permin :=
    Strtoint(TextureMinEdit22.text);
  GPFNodes.CCubes[I].TexturesRecord[22].PerMax :=
    Strtoint(TextureMaxEdit22.text);
  GPFNodes.CCubes[I].TexturesRecord[23].Selected := TextureCB23.checked;
  GPFNodes.CCubes[I].TexturesRecord[23].Permin :=
    Strtoint(TextureMinEdit23.text);
  GPFNodes.CCubes[I].TexturesRecord[23].PerMax :=
    Strtoint(TextureMaxEdit23.text);
  GPFNodes.CCubes[I].TexturesRecord[24].Selected := TextureCB24.checked;
  GPFNodes.CCubes[I].TexturesRecord[24].Permin :=
    Strtoint(TextureMinEdit24.text);
  GPFNodes.CCubes[I].TexturesRecord[24].PerMax :=
    Strtoint(TextureMaxEdit24.text);

  // 16
  GPFNodes.CCubes[I].TextureSetRecord[1].Selected := Texture16_1CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[1].Permin:= Strtoint(Texture16_1MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[1].PerMax:= Strtoint(Texture16_1MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[2].Selected := Texture16_2CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[2].Permin:= Strtoint(Texture16_2MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[2].PerMax:= Strtoint(Texture16_2MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[3].Selected := Texture16_3CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[3].Permin:= Strtoint(Texture16_3MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[3].PerMax:= Strtoint(Texture16_3MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[4].Selected := Texture16_4CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[4].Permin:= Strtoint(Texture16_4MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[4].PerMax:= Strtoint(Texture16_4MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[5].Selected := Texture16_5CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[5].Permin:= Strtoint(Texture16_5MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[5].PerMax:= Strtoint(Texture16_5MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[6].Selected := Texture16_6CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[6].Permin:= Strtoint(Texture16_6MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[6].PerMax:= Strtoint(Texture16_6MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[7].Selected := Texture16_7CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[7].Permin:= Strtoint(Texture16_7MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[7].PerMax:= Strtoint(Texture16_7MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[8].Selected := Texture16_8CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[8].Permin:= Strtoint(Texture16_8MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[8].PerMax:= Strtoint(Texture16_8MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[9].Selected := Texture16_9CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[9].Permin:= Strtoint(Texture16_9MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[9].PerMax:= Strtoint(Texture16_9MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[10].Selected := Texture16_10CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[10].Permin:= Strtoint(Texture16_10MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[10].PerMax:= Strtoint(Texture16_10MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[11].Selected := Texture16_11CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[11].Permin:= Strtoint(Texture16_11MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[11].PerMax:= Strtoint(Texture16_11MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[12].Selected := Texture16_12CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[12].Permin:= Strtoint(Texture16_12MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[12].PerMax:= Strtoint(Texture16_12MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[13].Selected := Texture16_13CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[13].Permin:= Strtoint(Texture16_13MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[13].PerMax:= Strtoint(Texture16_13MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[14].Selected := Texture16_14CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[14].Permin:= Strtoint(Texture16_14MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[14].PerMax:= Strtoint(Texture16_14MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[15].Selected := Texture16_15CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[15].Permin:= Strtoint(Texture16_15MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[15].PerMax:= Strtoint(Texture16_15MaxEdit.text); }
  GPFNodes.CCubes[I].TextureSetRecord[16].Selected := Texture16_16CB.checked;
  { GPFNodes.CCubes[i].TextureSetRecord[16].Permin:= Strtoint(Texture16_16MinEdit.text);
    GPFNodes.CCubes[i].TextureSetRecord[16].PerMax:= Strtoint(Texture16_16MaxEdit.text); }
  // 64
  GPFNodes.CCubes[I].TextureSet64Record[1].Selected := Texture64_1CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[2].Selected := Texture64_2CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[3].Selected := Texture64_3CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[4].Selected := Texture64_4CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[5].Selected := Texture64_5CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[6].Selected := Texture64_6CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[7].Selected := Texture64_7CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[8].Selected := Texture64_8CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[9].Selected := Texture64_9CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[10].Selected := Texture64_10CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[11].Selected := Texture64_11CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[12].Selected := Texture64_12CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[13].Selected := Texture64_13CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[14].Selected := Texture64_14CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[15].Selected := Texture64_15CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[16].Selected := Texture64_16CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[17].Selected := Texture64_17CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[18].Selected := Texture64_18CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[19].Selected := Texture64_19CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[20].Selected := Texture64_20CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[21].Selected := Texture64_21CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[22].Selected := Texture64_22CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[23].Selected := Texture64_23CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[24].Selected := Texture64_24CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[25].Selected := Texture64_25CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[26].Selected := Texture64_26CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[27].Selected := Texture64_27CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[28].Selected := Texture64_28CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[29].Selected := Texture64_29CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[30].Selected := Texture64_30CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[31].Selected := Texture64_31CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[32].Selected := Texture64_32CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[33].Selected := Texture64_33CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[34].Selected := Texture64_34CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[35].Selected := Texture64_35CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[36].Selected := Texture64_36CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[37].Selected := Texture64_37CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[38].Selected := Texture64_38CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[39].Selected := Texture64_39CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[40].Selected := Texture64_40CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[41].Selected := Texture64_41CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[42].Selected := Texture64_42CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[43].Selected := Texture64_43CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[44].Selected := Texture64_44CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[45].Selected := Texture64_45CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[46].Selected := Texture64_46CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[47].Selected := Texture64_47CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[48].Selected := Texture64_48CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[49].Selected := Texture64_49CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[50].Selected := Texture64_50CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[51].Selected := Texture64_51CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[52].Selected := Texture64_52CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[53].Selected := Texture64_53CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[54].Selected := Texture64_54CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[55].Selected := Texture64_55CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[56].Selected := Texture64_56CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[57].Selected := Texture64_57CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[58].Selected := Texture64_58CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[59].Selected := Texture64_59CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[60].Selected := Texture64_60CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[61].Selected := Texture64_61CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[62].Selected := Texture64_62CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[63].Selected := Texture64_63CB.checked;
  GPFNodes.CCubes[I].TextureSet64Record[64].Selected := Texture64_64CB.checked;
end;

procedure TACloudDemoForm.GroupColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := GroupColorPanel.Color;
  if ColorDialog1.Execute then
  Begin
    GroupColorPanel.Color := ColorDialog1.Color;
  End;
end;
/// //////////////////////////////////
// CODE End
/// /////////////////////////////////

/// //////////////////////////////////
procedure TACloudDemoForm.PCxMakerBtnClick(Sender: TObject);
begin
  ProceduralCloudsForm.Show;
end;

procedure TACloudDemoForm.MFxMakerBtnClick(Sender: TObject);
begin
  MFMakerForm.Show;
end;

procedure TACloudDemoForm.NanxMakerBtnClick(Sender: TObject);
begin
  NanMakerForm.Show;
end;

procedure TACloudDemoForm.TexEditorBtnClick(Sender: TObject);
begin
  ATextureEditorForm.Show;
end;

procedure TACloudDemoForm.TexCombineBtnClick(Sender: TObject);
begin
  ATextureCombinerForm.Show;
end;

procedure TACloudDemoForm.ViewerBackgroundColorPanelClick(Sender: TObject);
begin
  GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
    ViewerBackgroundColorPanel.Color;
  ColorDialog1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
    AsWinColor;
  if ColorDialog1.Execute then
  Begin
    ViewerBackgroundColorPanel.Color := ColorDialog1.Color;
    Scn.Buffer.BackGroundColor := ColorDialog1.Color;
    // GLLightSource1.Diffuse.Red
    Scn.Buffer.AmbientColor.AsWinColor := ColorDialog1.Color;
    Scn.Buffer.AmbientColor.Alpha := 1; // 0;
  End;
end;

procedure TACloudDemoForm.ImageCaptureRGClick(Sender: TObject);
var
  bmp: TBitmap;
  { Stolen from GLScene: Render To Bitmap sample. }
  procedure RenderToBitmap(Scale: single);
  begin
    // Rendering to a bitmap requires an existing bitmap,
    // so we create and size a new one
    bmp := TBitmap.Create;
    // Don't forget to specify a PixelFormat, or current screen pixel format
    // will be used, which may not suit your purposes!
    bmp.PixelFormat := pf24bit;
    // pf32bit;
    bmp.Width := Round(Scn.Width * Scale);
    bmp.Height := Round(Scn.Height * Scale);
    // Here we just request a render
    // The second parameter specifies DPI (Dots Per Inch), which is
    // linked to the bitmap's scaling
    // "96" is the "magic" DPI scale of the screen under windows
    // Actual dpi recorded is 96..the SIZE changes
    Scn.Buffer.RenderToBitmap(bmp, Round(96 * Scale));
  end;

begin
  Case ImageCaptureRG.ItemIndex of
    // None, As is, 256, 512
    1: // Create a snapshot directly from the viewer content
      bmp := Scn.CreateSnapShotBitmap;
    2:
      RenderToBitmap(1); // a 32 bit image
    3:
      Begin
        // Render at twice viewer resolution (scale = 2, DPI = 192 = 96x2)
        RenderToBitmap(2);
      End;
    4:
      Begin
        // Screen is "magic" 96 dpi, this gives us our scale
        // 300/96  3  Scn.Width*(512/96)
        RenderToBitmap((512 / Scn.Width) { /(96) } );
        // (3);
      End;
  end;
  SaveDialog1.Filter := 'Cloud Cube Editor (*.bmp)|*.bmp';
  SaveDialog1.InitialDir := ImagePath; // ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt := 'bmp';
  SaveDialog1.FileName := 'CCE.bmp';
  If SaveDialog1.Execute then
    bmp.SaveToFile(SaveDialog1.FileName);
  // Release the bitmap
  bmp.free;
end;

procedure TACloudDemoForm.GridOffCBClick(Sender: TObject);
begin
  GLXYZGrid1.Visible := (not GridOffCB.checked);
end;

procedure TACloudDemoForm.BlueCubeOffCBClick(Sender: TObject);
begin
  GLCloudCube.Visible := (not GridOffCB.checked);
  GLCameraTargetDC.ShowAxes := (not GridOffCB.checked);
end;

/// //////////////////////////////////

/// //////////////////////////////////
procedure TACloudDemoForm.AlbedoPanel1Click(Sender: TObject);
begin
  ColorDialog1.Color := AlbedoPanel1.Color;
  if ColorDialog1.Execute then
  Begin
    AlbedoPanel1.Color := ColorDialog1.Color;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
      ColorDialog1.Color;
    Albedo1REdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Red);
    Albedo1GEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Green);
    Albedo1BEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Blue);
  End;
end;

procedure TACloudDemoForm.AlbedoPanel2Click(Sender: TObject);
begin
  ColorDialog1.Color := AlbedoPanel2.Color;
  if ColorDialog1.Execute then
  Begin
    AlbedoPanel2.Color := ColorDialog1.Color;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
      ColorDialog1.Color;
    Albedo2REdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Red);
    Albedo2GEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Green);
    Albedo2BEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Blue);
  End;
end;

procedure TACloudDemoForm.AlbedoPanel3Click(Sender: TObject);
begin
  ColorDialog1.Color := AlbedoPanel3.Color;
  if ColorDialog1.Execute then
  Begin
    AlbedoPanel3.Color := ColorDialog1.Color;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
      ColorDialog1.Color;
    Albedo3REdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Red);
    Albedo3GEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Green);
    Albedo3BEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Blue);
  End;
end;

procedure TACloudDemoForm.AlbedoPanel4Click(Sender: TObject);
begin
  ColorDialog1.Color := AlbedoPanel4.Color;
  if ColorDialog1.Execute then
  Begin
    AlbedoPanel4.Color := ColorDialog1.Color;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
      ColorDialog1.Color;
    Albedo4REdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Red);
    Albedo4GEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Green);
    Albedo4BEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Blue);
  End;
end;

procedure TACloudDemoForm.AlbedoPanel5Click(Sender: TObject);
begin
  GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
    PointMake(StrToFloatDef(Albedo5REdit.text),
    StrToFloatDef(Albedo5GEdit.text), StrToFloatDef(Albedo5BEdit.text));
  ColorDialog1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
    AsWinColor;
  // AlbedoPanel5.Color;
  if ColorDialog1.Execute then
  Begin
    AlbedoPanel5.Color := ColorDialog1.Color;
    GLHiddenColorCube.Material.FrontProperties.Diffuse.AsWinColor :=
      ColorDialog1.Color;
    Albedo5REdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Red);
    Albedo5GEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Green);
    Albedo5BEdit.text :=
      Floattostr(GLHiddenColorCube.Material.FrontProperties.Diffuse.Blue);
  End;
end;

/// /////////////////////////////////
procedure TACloudDemoForm.ImageAlphaSetSettingsRGClick(Sender: TObject);
begin // TextureSet: Array[1..24] of Array[1..4] of Integer;
  // ImageBlendingModeRG  ImageTextureModeRG
  // ImageTextureFormatRG    ImageImageAlphaRG
  If ImageAlphaSetSettingsRG.ItemIndex > 0 then
  begin
    GPFNodes.TextureAlpha[ImageAlphaSetSettingsRG.ItemIndex, 1] :=
      ImageBlendingModeRG.ItemIndex;
    GPFNodes.TextureAlpha[ImageAlphaSetSettingsRG.ItemIndex, 2] :=
      ImageTextureModeRG.ItemIndex;
    GPFNodes.TextureAlpha[ImageAlphaSetSettingsRG.ItemIndex, 3] :=
      ImageTextureFormatRG.ItemIndex;
    GPFNodes.TextureAlpha[ImageAlphaSetSettingsRG.ItemIndex, 4] :=
      ImageImageAlphaRG.ItemIndex;
  end;
end;

procedure TACloudDemoForm.ImageAlphaGetSettingsRGClick(Sender: TObject);
begin
  If ImageAlphaGetSettingsRG.ItemIndex > 0 then
  begin
    ImageBlendingModeRG.ItemIndex := GPFNodes.TextureAlpha
      [ImageAlphaGetSettingsRG.ItemIndex, 1];
    ImageTextureModeRG.ItemIndex := GPFNodes.TextureAlpha
      [ImageAlphaGetSettingsRG.ItemIndex, 2];
    ImageTextureFormatRG.ItemIndex := GPFNodes.TextureAlpha
      [ImageAlphaGetSettingsRG.ItemIndex, 3];
    ImageImageAlphaRG.ItemIndex := GPFNodes.TextureAlpha
      [ImageAlphaGetSettingsRG.ItemIndex, 4];
  end;
end;

procedure TACloudDemoForm.ImageTypeGetSettingsRGClick(Sender: TObject);
begin
  If ImageTypeGetSettingsRG.ItemIndex > 0 then
    TextureTypeRG.ItemIndex := GPFNodes.TextureType
      [ImageTypeGetSettingsRG.ItemIndex];
end;

procedure TACloudDemoForm.ImageTypeSetSettingsRGClick(Sender: TObject);
begin
  If ImageTypeSetSettingsRG.ItemIndex > 0 then
    GPFNodes.TextureType[ImageTypeSetSettingsRG.ItemIndex] :=
      TextureTypeRG.ItemIndex;
end;

/// //////////////////////////////////
procedure TACloudDemoForm.TextureBtn1Click(Sender: TObject);
begin
  OpenDialog1.Filter :=
    'Cloud Images|*.bmp;*.tga|32 bits BMP|*.bmp|32 bits TGA|*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename1.text := ExtractFileName(OpenDialog1.FileName);
    GPFNodes.TextureNames[1] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn2Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename2.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[2] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn3Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename3.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[3] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn4Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename4.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[4] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn5Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename5.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[5] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn6Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename6.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[6] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn7Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename7.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[7] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn8Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename8.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[8] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn9Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename9.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[9] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn10Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename10.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[10] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn11Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename11.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[11] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn12Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename12.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[12] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn13Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename13.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[13] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn14Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename14.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[14] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn15Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename15.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[15] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn16Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename16.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[16] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn17Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename17.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[17] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn18Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename18.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[18] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn19Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename19.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[19] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn20Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename20.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[20] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn21Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename21.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[21] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn22Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename22.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[22] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn23Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename23.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[23] := ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TACloudDemoForm.TextureBtn24Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud Images (*.bmp)|*.bmp;*.tga';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.FileName := ''; // '*.bmp' ;
  OpenDialog1.InitialDir := ImagePath;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    TextureFilename24.text := ExtractFileName(OpenDialog1.FileName);
    // Load data//Set things
    GPFNodes.TextureNames[24] := ExtractFileName(OpenDialog1.FileName);
  end;
end;
/// //////////////////////////////////
/// //////////////////////////////////

/// //////////////////////////////////
/// /////////////////////////////////
// This was thought to allow a Plane (Sphere) at high level
// to be used for All Cirrus clouds...
procedure TACloudDemoForm.CirrusFilenameOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter :=
    'Cloud Images or File|*.bmp;*.tga;*.ccf|Cloud Cirrus File *.ccf)|*.ccf|32 bits BMP|*.bmp|32 bits TGA|*.tga';
  OpenDialog1.Filter := 'Cloud Cirrus File (*.ccf)|*.ccf';
  OpenDialog1.InitialDir := ImagePath;
  OpenDialog1.FileName := ''; // ChangeFileExt(ProjectName,'.clb' );
  if OpenDialog1.Execute then
  begin
    CirrusFilenameEdit.text := OpenDialog1.FileName;
    // Read the data from file
    If ExtractFileExt(OpenDialog1.FileName) = '.ccf' then
    begin
      LoadCirrusCloudData;
      SETCirrusCloudData;
    end;
  end;
end;

(* //Sky Clouds..NOT Storm clouds
  CloudMinUsed, CloudMinRangeUsed, CloudImageSizeUsed,
  CloudRanger{}: Integer;
  CloudAnime, CloudChangeAmount,
  CloudSharpUsed, CloudSharpRangeUsed,
  CloudSharpRanger{},
  CloudChange{},  CloudNewTime{}:Double;
  CloudRandomSeedUsed:Longint;
  CloudFileUsed:String;
  UseCloudsSRange, UseCloudsRange,
  UseCloudFile,  UseClouds,UseCloudsSetting:Boolean; *)
procedure TACloudDemoForm.LoadCirrusCloudData;
Begin
  // a File name IS THERE!
end;

procedure TACloudDemoForm.SETCirrusCloudData;
// var code: Integer;
Begin
  UseCloudsCB.checked := UseClouds;
  UseCloudsSRangeCB.checked := UseCloudsSRange;
  CloudFileUsedEdit.text := CloudFileUsed;
  UseCloudsRangeCB.checked := UseCloudsRange;
  UseCloudFileCB.checked := UseCloudFile;
  CloudAnimeEdit.text := Floattostr(CloudAnime);
  CloudChangeAmountEdit.text := Floattostr(CloudChangeAmount);
  CloudSharpUsedEdit.text := Floattostr(CloudSharpUsed);
  CloudSharpRangeUsedEdit.text := Floattostr(CloudSharpRangeUsed);
  CloudRandomSeedUsedEdit.text := inttostr(CloudRandomSeedUsed);
  CloudImageSizeUsedEdit.text := inttostr(CloudImageSizeUsed);
  CloudMinUsedEdit.text := inttostr(CloudMinUsed);
  CloudMinRangeUsedEdit.text := inttostr(CloudMinRangeUsed);
  UseCloudsSettingCB.checked := UseCloudsSetting;
end;

procedure TACloudDemoForm.GetCirrusCloudData;
var
  code: Integer;
Begin
  UseClouds := UseCloudsCB.checked;
  UseCloudsSetting := UseCloudsSettingCB.checked;
  val(CloudChangeAmountEdit.text, CloudChangeAmount, code);
  CloudAnime := Strtoint(CloudAnimeEdit.text);
  val(CloudSharpUsedEdit.text, CloudSharpUsed, code);
  val(CloudSharpRangeUsedEdit.text, CloudSharpRangeUsed, code);
  UseCloudsSRange := UseCloudsSRangeCB.checked;
  CloudMinUsed := Strtoint(CloudMinUsedEdit.text);
  CloudMinRangeUsed := Strtoint(CloudMinRangeUsedEdit.text);
  UseCloudsRange := UseCloudsRangeCB.checked;
  CloudRandomSeedUsed := Strtoint(CloudRandomSeedUsedEdit.text);
  CloudImageSizeUsed := Strtoint(CloudImageSizeUsedEdit.text);

  CloudFileUsed := CloudFileUsedEdit.text;
  UseCloudFile := UseCloudFileCB.checked;
end;

procedure TACloudDemoForm.CirrusFilenameSaveBtnClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Cloud Cirrus File (*.ccf)|*.ccf';
  SaveDialog1.InitialDir := ImagePath; // ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt := 'ccf';
  SaveDialog1.FileName := CirrusFilenameEdit.text;
  If SaveDialog1.Execute then
  begin
    GetCirrusCloudData;
    // Make the file

  end;
end;

// FormShowDown for clouds
procedure TACloudDemoForm.CirrusRGClick(Sender: TObject);
var
  I: Integer;
  aPERM: array [0 .. 255] of Byte;
  outfile: TextFile;
  S: string;
begin
  Showmessage('got here: CirrusRGClick');
  If CirrusRG.ItemIndex = 0 then
  begin
    UseCloudsCB.checked := False;
    UseClouds := UseCloudsCB.checked;
    CloudSphere.Visible := UseCloudsCB.checked;
  end
  else If FileExists(CirrusFilenameEdit.text) then
  begin
    Case CirrusRG.ItemIndex of
      1: // Place image on Sphere : Static Image
        Begin
          with CloudSphere.Material.Texture do
          begin
            CloudSphere.Material.Texture.ImageClassName := 'Persistent Image';
            CloudSphere.Material.Texture.Image.Loadfromfile(S);
          end;
        End;
      2:
        ; // Place image on Sphere : Moving ?what moves it
      3: // Procedural
        Begin
          LoadCirrusCloudData;
          SETCirrusCloudData;
          CloudSphere.Material.Texture.ImageClassName := 'Procedural Noise';
          If (UseClouds or UseCloudsSetting) then
          begin
            { [dark FULL] s99m30 .. s98 m60..90
              [50/50]    97..97
              [lighter]  95.120
              [10/10]    90 160 }
            with CloudSphere.Material.Texture do
            begin
              If (UseCloudFile and (FileExists(CloudFileUsed))) then
              begin
                Try
                  AssignFile(outfile, CloudFileUsed);
                  { File selected in dialog box }
                  Reset(outfile);
                  Readln(outfile, S { 'DTM Cloud Base V1.0' } );
                  For I := 0 to 255 do
                  begin
                    Readln(outfile, S);
                    aPERM[I] := Strtoint(S);
                  end;
                Finally
                  CloseFile(outfile);
                End;
                TGLProcTextureNoise(Image).SetPermFromData(aPERM);
              end
              else
                TGLProcTextureNoise(Image).SetPermToDefault;
              // TextureFormat:=tfRGBA;//TGLTextureFormat(Integer(tfRGB)+CBFormat.ItemIndex);
              // Compression:=TGLTextureCompression(Integer(tcNone)+CBCompression.ItemIndex);
              TGLProcTextureNoise(Image).Height := CloudImageSizeUsed;
              TGLProcTextureNoise(Image).Width := CloudImageSizeUsed;
              TGLProcTextureNoise(Image).MinCut := CloudMinUsed;
              CloudRanger := CloudMinUsed;
              TGLProcTextureNoise(Image).NoiseSharpness := CloudSharpUsed;
              CloudSharpRanger := CloudSharpUsed;
              TGLProcTextureNoise(Image).NoiseRandSeed := CloudRandomSeedUsed;
              TGLProcTextureNoise(Image).Seamless := True; // 1 is seamless
            end;
          end;
        End; // #3
    End; // Case
    CloudSphere.Visible := True; // Other 3 have it ready to be on
  end
  else
  begin // No file so reset
    CirrusRG.ItemIndex := 0;
    UseCloudsCB.checked := False;
    UseClouds := UseCloudsCB.checked;
    CloudSphere.Visible := UseCloudsCB.checked;
  end;
end;

procedure TACloudDemoForm.UseCloudsCBClick(Sender: TObject);
begin
  UseClouds := UseCloudsCB.checked;
  // CloudSphere.Visible:=UseCloudsCB.Checked;
end;

procedure TACloudDemoForm.CloudFileOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  OpenDialog1.InitialDir := ImagePath;
  OpenDialog1.FileName := ''; // ChangeFileExt(ProjectName,'.clb' );
  if OpenDialog1.Execute then
  begin
    CloudFileUsedEdit.text := OpenDialog1.FileName;
  end;
end;

procedure TACloudDemoForm.CloudChangeMoreBtnClick(Sender: TObject);
begin
  CloudChangeAmount := CloudChangeAmount + 0.001;
end;

procedure TACloudDemoForm.CloudChangeLessBtnClick(Sender: TObject);
begin
  CloudChangeAmount := CloudChangeAmount - 0.001;
end;

procedure TACloudDemoForm.CloudsAnimeFasterBtnClick(Sender: TObject);
begin
  CloudAnime := CloudAnime - 1;
  If (CloudAnime < 0) then
    CloudAnime := 0;
  CloudNewTime := 0;
  CloudChange := 0;
  // GLSphere1.TurnAngle:= GLSphere1.TurnAngle+ CloudChangeAmount;//deltaTime;
end;

procedure TACloudDemoForm.CloudsAnimeSlowerBtnClick(Sender: TObject);
begin
  // CloudAnime:=6;
  CloudAnime := CloudAnime + 1;
  CloudNewTime := 0;
  CloudChange := 0;
end;

procedure TACloudDemoForm.CloudsAnimeMuchSlowerBtnClick(Sender: TObject);
begin
  CloudAnime := CloudAnime + 10;
  CloudNewTime := 0;
  CloudChange := 0;
end;

/// //////////////////////////////////
procedure TACloudDemoForm.SkyDomeCBClick(Sender: TObject);
begin // Toggle Display
  GLSkyDome1.Visible := SkyDomeCB.checked;
end;

procedure TACloudDemoForm.SkyDomeFilenameBtnClick(Sender: TObject);
begin
  //
end;

procedure TACloudDemoForm.ProDomeFilenameBtnClick(Sender: TObject);
begin
  //
end;

procedure TACloudDemoForm.ProDomeCBClick(Sender: TObject);
begin // Toggle Display
  GLEarthSkyDome1.Visible := ProDomeCB.checked;
end;
/// //////////////////////////////////
/// //////////////////////////////////

/// /////////////////Sun     Arise
// SUN   Directional Colors
procedure TACloudDemoForm.DawnPanelClick(Sender: TObject);
begin
  GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
    PointMake(StrToFloatDef(SunDawnREdit.text),
    StrToFloatDef(SunDawnGEdit.text), StrToFloatDef(SunDawnBEdit.text));
  ColorDialog1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
    AsWinColor;
  if ColorDialog1.Execute then
  Begin
    DawnPanel.Color := ColorDialog1.Color;
    GLLightSource1.Diffuse.AsWinColor := ColorDialog1.Color;
    SunDawnREdit.text := Floattostr(GLLightSource1.Diffuse.Red);
    SunDawnGEdit.text := Floattostr(GLLightSource1.Diffuse.Green);
    SunDawnBEdit.text := Floattostr(GLLightSource1.Diffuse.Blue);
    GLLightSource1.Diffuse.Alpha := StrToFloatDef(SunDawnAEdit.text);
  End;
end;

procedure TACloudDemoForm.DuskPanelClick(Sender: TObject);
begin
  GLHiddenColorCube.Material.FrontProperties.Diffuse.Color :=
    PointMake(StrToFloatDef(SunDuskREdit.text),
    StrToFloatDef(SunDuskGEdit.text), StrToFloatDef(SunDuskBEdit.text));
  ColorDialog1.Color := GLHiddenColorCube.Material.FrontProperties.Diffuse.
    AsWinColor;
  if ColorDialog1.Execute then
  Begin
    DuskPanel.Color := ColorDialog1.Color;
    GLLightSource1.Diffuse.AsWinColor := ColorDialog1.Color;
    SunDuskREdit.text := Floattostr(GLLightSource1.Diffuse.Red);
    SunDuskGEdit.text := Floattostr(GLLightSource1.Diffuse.Green);
    SunDuskBEdit.text := Floattostr(GLLightSource1.Diffuse.Blue);
    GLLightSource1.Diffuse.Alpha := StrToFloatDef(SunDuskAEdit.text);
  End;
end;

/// //////////////////////////////////
procedure TACloudDemoForm.LightsOnCBClick(Sender: TObject);
begin
  GLLightSource1.Shining := LightsOnCB.checked;
end;

procedure TACloudDemoForm.SunLensFlareCBClick(Sender: TObject);
begin
  GLLensFlare1.Visible := SunLensFlareCB.checked;
end;
/// ////////////////////////////////
/// //////////////////////////////////
/// /////////////////////////////////

end.
