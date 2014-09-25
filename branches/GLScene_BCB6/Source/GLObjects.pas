//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLObjects<p>

  Implementation of basic scene objects plus some management routines.<p>

  All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.<br>

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical
  objects can be found GLGeomObjects.<p>

  <b>History : </b><font size=-1><ul>
  <li>14/03/07 - DaStr - Added explicit pointer dereferencing
                (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>15/02/07 - DaStr - Global $R- removed, added default values to
                 TGLSprite.NoZWrite, MirrorU, MirrorV
  <li>14/01/07 - DaStr - Fixed TGLCube.BuildList. Bugtracker ID=1623743 (Thanks Pete Jones)
  <li>19/10/06 - LC - Fixed IcosahedronBuildList. Bugtracker ID=1490784 (thanks EPA_Couzijn)
  <li>19/10/06 - LC - Fixed TGLLineBase.Assign problem. Bugtracker ID=1549354 (thanks Zapology)
  <li>08/10/05 - Mathx - Fixed TGLLines.nodes.assign problem (thanks to  Yong Yoon Kit);
                 Also fixed a TGLLineBase.assign problem (object being assigned to
                 was refering the base lists, not copying them).
                 Bugtracker ID=830846
  <li>17/01/05 - SG - Added color support for bezier style TGLLines
  <li>08/12/04 - LR - BCB corrections: use record instead array
  <li>03/12/04 - MF - Added TGLSprite.AxisAlignedDimensionsUnscaled override
  <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
                 Replace direct access of some properties by
  a getter and a setter
  <li>06/07/04 - SG - TGLCube.RayCastIntersect fix (Eric Pascual)
  <li>20/01/04 - SG - Added IcosahedronBuildList
  <li>30/11/03 - MF - Added TGLSphere.GenerateSilhouette - it now takes the
  stacks/slices of the sphere into account
  <li>10/09/03 - EG - Introduced TGLNodedLines
  <li>18/08/03 - SG - Added MirrorU and MirrorV to TGLSprite for mirroring textures
  <li>21/07/03 - EG - TGLTeapot moved to new GLTeapot unit,
                      TGLDodecahedron moved to new GLPolyhedron unit,
                      TGLCylinder, TGLCone, TGLTorus, TGLDisk, TGLArrowLine,
                      TGLAnnulus, TGLFrustrum and TGLPolygon moved to new
                       GLGeomObjects unit
  <li>16/07/03 - EG - Style changes and cleanups
  <li>19/06/03 - MF - Added GenerateSilhouette to TGLCube and TGLPlane.
  <li>13/06/03 - EG - Fixed TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
  <li>03/06/03 - EG - Added TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
  <li>01/05/03 - SG - Added NURBS Curve to TGLLines (color not supported yet)
  <li>14/04/03 - SG - Added a Simple Bezier Spline to TGLLines (color not supported yet)
  <li>02/04/03 - EG - TGLPlane.RayCastIntersect fix (Erick Schuitema)
  <li>13/02/03 - DanB - added AxisAlignedDimensionsUnscaled functions
  <li>22/01/03 - EG - TGLCube.RayCastIntersect fixes (Dan Bartlett)
  <li>10/01/03 - EG - TGLCube.RayCastIntersect (Stuart Gooding)
  <li>08/01/03 - RC - Added TGLPlane.XScope and YScope, to use just a part of the texture
  <li>27/09/02 - EG - Added TGLPointParameters
  <li>24/07/02 - EG - Added TGLCylinder.Alignment
  <li>23/07/02 - EG - Added TGLPoints (experimental)
  <li>20/07/02 - EG - TGLCylinder.RayCastIntersect and TGLPlane.RayCastIntersect
  <li>18/07/02 - EG - Added TGLCylinder.Align methods
  <li>07/07/02 - EG - Added TGLPlane.Style
  <li>03/07/02 - EG - TGLPolygon now properly setups normals (filippo)
  <li>17/03/02 - EG - Support for transparent lines
  <li>02/02/02 - EG - Fixed TGLSprite change notification
  <li>26/01/02 - EG - TGLPlane & TGLCube now osDirectDraw
  <li>20/01/02 - EG - TGLSpaceText moved to GLSpaceText
  <li>22/08/01 - EG - TGLTorus.RayCastIntersect fixes
  <li>30/07/01 - EG - Updated AxisAlignedDimensions implems
  <li>16/03/01 - EG - TGLCylinderBase, changed default Stacks from 8 to 4
  <li>27/02/01 - EG - Fix in TGLCube texcoords, added TGLFrustrum (thx Robin Gerrets)
  <li>22/02/01 - EG - Added AxisAlignedDimensions overrides by Uwe Raabe
  <li>05/02/01 - EG - Minor changes to TGLCube.BuildList
  <li>21/01/01 - EG - BaseProjectionMatrix fix for TGLHUDSprite (picking issue),
                      TGLHUDSprite moved to GLHUDObjects
  <li>14/01/01 - EG - Fixed TGLSphere texture coordinates
  <li>13/01/01 - EG - TGLSprite matrix compatibility update
  <li>09/01/01 - EG - TGLSpaceText now handles its TFont.OnFontChange
  <li>08/01/01 - EG - Added TGLLinesNode (color support) and Node size control
  <li>22/12/00 - EG - Sprites are no longer texture enabled by default,
                      updated TGLSprite.BuildList to work with new matrices
  <li>14/11/00 - EG - Added TGLDummyCube.Destroy (thx Airatz)
  <li>08/10/00 - EG - Fixed call to wglUseFontOutlines
  <li>06/08/00 - EG - TRotationSolid renamed to TGLRevolutionSolid & moved to GLExtrusion
  <li>04/08/00 - EG - Fixed sphere main body texture coords + slight speedup
  <li>02/08/00 - EG - Added TGLPolygonBase
  <li>19/07/00 - EG - Added TGLHUDSprite
  <li>18/07/00 - EG - Added TGLRevolutionSolid
  <li>15/07/00 - EG - Code reduction and minor speedup for all quadric objects,
                      Added TGLLineBase (split of TGLLines),
                      TGLDummyCube now uses osDirectDraw instead of special behaviour
  <li>13/07/00 - EG - Added TGLArrowLine (code by Aaron Hochwimmer)
  <li>28/06/00 - EG - Support for "ObjectStyle"
  <li>23/06/00 - EG - Reduced default Loop count for TGLDisk
  <li>18/06/00 - EG - TGLMesh and accompanying stuff moved to GLMesh
  <li>14/06/00 - EG - Added Capacity to TVertexList
  <li>09/06/00 - EG - First row of Geometry-related upgrades
  <li>08/06/00 - EG - Added ReleaseFontManager, fixed TGLSpaceText DestroyList,
  <li>01/06/00 - EG - Added TGLAnnulus (code by Aaron Hochwimmer)
  <li>29/05/00 - EG - TGLLines now uses TGLNode/TGLNodes
  <li>28/05/00 - EG - Added persistence ability to TGLLines,
                      Added defaults for all TGLLines properties
  <li>27/05/00 - EG - Moved in RogerCao's TGLLines object, added a TLineNode
                      class (currently private) and various enhancements + fixes,
                      DodecahedronBuildList now available as a procedure,
                      CubeWireframeBuildList now available as a procedure
  <li>26/05/00 - RoC - Added division property to TGLLines, and Spline supported
  <li>26/05/00 - EG - Moved vectorfile remnants to GLVectorFiles
  <li>14/05/00 - EG - Removed Top/Bottom checks for TGLSphere,
                      Added mmTriangleStrip support in CalcNormals
  <li>08/05/00 - EG - Uncommented DisableAutoTexture in TGLSpaceText.BuildList
  <li>07/05/00 - RoC - TGLLines added, to show a list of vertex
  <li>26/04/00 - EG - Reactivated stuff in SetupQuadricParams (thanks Nelson Chu)
  <li>18/04/00 - EG - Overriden TGLDummyCube.Render
  <li>16/04/00 - EG - FontManager now published and auto-creating
  <li>12/04/00 - EG - Added TGLCylinderBase.Loops (fixes a bug, thanks Uwe)
  <li>24/03/00 - EG - Added Rotation to TGLSprite, fixed sprite size
  <li>20/03/00 - EG - Enhanced FontManager
  <li>17/03/00 - EG - Fixed SpaceText glBaseList bug,
  TGLSprite now uses a transposition of the globalmatrix
  <li>16/03/00 - EG - Enhanced TFontManager to allow lower quality
  <li>14/03/00 - EG - Added subobjects Barycenter support for TGLDummyCube
  <li>09/02/00 - EG - ObjectManager stuff moved to GLSceneRegister,
  FreeForm and vector file stuff moved to new GLVectorFileObjects
  <li>08/02/00 - EG - Added TGLDummyCube
  <li>05/02/00 - EG - Javadocisation, fixes and enhancements :
  TVertexList.AddVertex, "default"s to properties
  </ul></font>
}
unit GLObjects;

interface

{$I GLScene.inc}

uses Classes, VectorGeometry, GLScene, GLTexture, GLMisc, OpenGL1x, SysUtils,
  VectorLists, GLCrossPlatform, GLContext, GLSilhouette;

type

  // TGLVisibilityDeterminationEvent
  //
  TGLVisibilityDeterminationEvent = function(Sender: TObject;
    var Rci: TRenderContextInfo): Boolean of object;

  // TGLDummyCube
  //
  { : A simple cube, invisible at run-time.<p>
    This is a usually non-visible object -except at design-time- used for
    building hierarchies or groups, when some kind of joint or movement
    mechanism needs be described, you can use DummyCubes.<br>
    DummyCube's barycenter is its children's barycenter.<br>
    The DummyCube can optionnally amalgamate all its children into a single
    display list (see Amalgamate property). }
  TGLDummyCube = class(TGLCameraInvariantObject)
  private
    { Private Declarations }
    FCubeSize: TGLFloat;
    FEdgeColor: TGLColor;
    FVisibleAtRunTime, FAmalgamate: Boolean;
    FGroupList: TGLListHandle;
    FOnVisibilityDetermination: TGLVisibilityDeterminationEvent;

  protected
    { Protected Declarations }
    procedure SetCubeSize(const Val: TGLFloat);
    procedure SetEdgeColor(const Val: TGLColor);
    procedure SetVisibleAtRunTime(const Val: Boolean);
    procedure SetAmalgamate(const Val: Boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const RayStart, RayVector: TVector;
      IntersectPoint: PVector = nil; IntersectNormal: PVector = nil)
      : Boolean; override;
    procedure BuildList(var Rci: TRenderContextInfo); override;
    procedure DoRender(var Rci: TRenderContextInfo;
      RenderSelf, RenderChildren: Boolean); override;
    procedure StructureChanged; override;
    function BarycenterAbsolutePosition: TVector; override;

  published
    { Published Declarations }
    property CubeSize: TGLFloat read FCubeSize write SetCubeSize;
    property EdgeColor: TGLColor read FEdgeColor write SetEdgeColor;
    { : If true the dummycube's edges will be visible at runtime.<p>
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. }
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime default False;
    { : Amalgamate the dummy's children in a single OpenGL entity.<p>
      This activates a special rendering mode, which will compile
      the rendering of all of the dummycube's children objects into a
      single display list. This may provide a significant speed up in some
      situations, however, this means that changes to the children will
      be ignored untill you call StructureChanged on the dummy cube.<br>
      Some objects, that have their own display list management, may not
      be compatible with this behaviour. This will also prevents sorting
      and culling to operate as usual.<p>
      In short, this features is best used for static, non-transparent
      geometry, or when the point of view won't change over a large
      number of frames. }
    property Amalgamate: Boolean read FAmalgamate write SetAmalgamate
      default False;
    { : Camera Invariance Options.<p>
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. }
    property CamInvarianceMode default CimNone;
    { : Event for custom visibility determination.<p>
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. }
    property OnVisibilityDetermination: TGLVisibilityDeterminationEvent
      read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;

  // TPlaneStyle
  //
  TPlaneStyle = (PsSingleQuad, PsTileTexture);
  TPlaneStyles = set of TPlaneStyle;

  // Plane
  //
  { : A simple plane object.<p>
    Note that a plane is always made of a single quad (two triangles) and the
    tiling is only applied to texture coordinates. }
  TGLPlane = class(TGLSceneObject)
  private
    { Private Declarations }
    FXOffset, FYOffset: TGLFloat;
    FXScope, FYScope: TGLFloat;
    FWidth, FHeight: TGLFloat;
    FXTiles, FYTiles: Cardinal;
    FStyle: TPlaneStyles;
    FNoZWrite: Boolean;

  protected
    { Protected Declarations }
    procedure SetHeight(const AValue: Single);
    procedure SetWidth(const AValue: Single);
    procedure SetXOffset(const Value: TGLFloat);
    procedure SetXScope(const Value: TGLFloat);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: TGLFloat);
    procedure SetYScope(const Value: TGLFloat);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const Val: TPlaneStyles);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var Rci: TRenderContextInfo); override;
    function GenerateSilhouette(const SilhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const RayStart, RayVector: TVector;
      IntersectPoint: PVector = nil; IntersectNormal: PVector = nil)
      : Boolean; override;
    { : Computes the screen coordinates of the smallest rectangle encompassing the plane.<p>
      Returned extents are NOT limited to any physical screen extents. }
    function ScreenRect: TGLRect;

    { : Computes the signed distance to the point.<p>
      Point coordinates are expected in absolute coordinates. }
    function PointDistance(const APoint: TVector): Single;

  published
    { Public Declarations }
    property Height: TGLFloat read FHeight write SetHeight;
    property Width: TGLFloat read FWidth write SetWidth;
    property XOffset: TGLFloat read FXOffset write SetXOffset;
    property XScope: TGLFloat read FXScope write SetXScope stored StoreXScope;
    property XTiles: Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: TGLFloat read FYOffset write SetYOffset;
    property YScope: TGLFloat read FYScope write SetYScope stored StoreYScope;
    property YTiles: Cardinal read FYTiles write SetYTiles default 1;
    property Style: TPlaneStyles read FStyle write SetStyle
      default [PsSingleQuad, PsTileTexture];
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  // TGLSprite
  //
  { : A rectangular area, perspective projected, but always facing the camera.<p>
    A TGLSprite is perspective projected and as such is scaled with distance,
    if you want a 2D sprite that does not get scaled, see TGLHUDSprite. }
  TGLSprite = class(TGLSceneObject)
  private
    { Private Declarations }
    FWidth: TGLFloat;
    FHeight: TGLFloat;
    FRotation: TGLFloat;
    FAlphaChannel: Single;
    FNoZWrite: Boolean;
    FMirrorU, FMirrorV: Boolean;

  protected
    { Protected Declarations }
    procedure SetWidth(const Val: TGLFloat);
    procedure SetHeight(const Val: TGLFloat);
    procedure SetRotation(const Val: TGLFloat);
    procedure SetAlphaChannel(const Val: Single);
    function StoreAlphaChannel: Boolean;
    procedure SetNoZWrite(const Val: Boolean);
    procedure SetMirrorU(const Val: Boolean);
    procedure SetMirrorV(const Val: Boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var Rci: TRenderContextInfo); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;

    procedure SetSize(const Width, Height: TGLFloat);
    // : Set width and height to "size"
    procedure SetSquareSize(const Size: TGLFloat);

  published
    { Published Declarations }
    { : Sprite Width in 3D world units. }
    property Width: TGLFloat read FWidth write SetWidth;
    { : Sprite Height in 3D world units. }
    property Height: TGLFloat read FHeight write SetHeight;
    { : This the ON-SCREEN rotation of the sprite.<p>
      Rotatation=0 is handled faster. }
    property Rotation: TGLFloat read FRotation write SetRotation;
    { : If different from 1, this value will replace that of Diffuse.Alpha }
    property AlphaChannel: Single read FAlphaChannel write SetAlphaChannel
      stored StoreAlphaChannel;
    { : If True, sprite will not write to Z-Buffer.<p>
      Sprite will STILL be maskable by ZBuffer test. }
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite default False;
    { : Reverses the texture coordinates in the U and V direction to mirror
      the texture. }
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
  end;

  // TGLPointStyle
  //
  TGLPointStyle = (PsSquare, PsRound, PsSmooth, PsSmoothAdditive,
    PsSquareAdditive);

  // TGLPointParameters
  //
  { : Point parameters as in ARB_point_parameters.<p>
    Make sure to read the ARB_point_parameters spec if you want to understand
    what each parameter does. }
  TGLPointParameters = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FMinSize, FMaxSize: Single;
    FFadeTresholdSize: Single;
    FDistanceAttenuation: TGLCoordinates;

  protected
    { Protected Declarations }
    procedure SetEnabled(const Val: Boolean);
    procedure SetMinSize(const Val: Single);
    procedure SetMaxSize(const Val: Single);
    procedure SetFadeTresholdSize(const Val: Single);
    procedure SetDistanceAttenuation(const Val: TGLCoordinates);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Apply;
    procedure UnApply;

  published
    { Published Declarations }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property MinSize: Single read FMinSize write SetMinSize stored False;
    property MaxSize: Single read FMaxSize write SetMaxSize stored False;
    property FadeTresholdSize: Single read FFadeTresholdSize
      write SetFadeTresholdSize stored False;
    { : Components XYZ are for constant, linear and quadratic attenuation. }
    property DistanceAttenuation: TGLCoordinates read FDistanceAttenuation
      write SetDistanceAttenuation;
  end;

  // TGLPoints
  //
  { : Renders a set of non-transparent colored points.<p>
    The points positions and their color are defined through the Positions
    and Colors properties. }
  TGLPoints = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FPositions: TAffineVectorList;
    FColors: TVectorList;
    FSize: Single;
    FStyle: TGLPointStyle;
    FPointParameters: TGLPointParameters;
    FNoZWrite, FStatic: Boolean;

  protected
    { Protected Declarations }
    function StoreSize: Boolean;
    procedure SetNoZWrite(const Val: Boolean);
    procedure SetStatic(const Val: Boolean);
    procedure SetSize(const Val: Single);
    procedure SetPositions(const Val: TAffineVectorList);
    procedure SetColors(const Val: TVectorList);
    procedure SetStyle(const Val: TGLPointStyle);
    procedure SetPointParameters(const Val: TGLPointParameters);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var Rci: TRenderContextInfo); override;

    { : Points positions.<p>
      If empty, a single point is assumed at (0, 0, 0) }
    property Positions: TAffineVectorList read FPositions write SetPositions;
    { : Defines the points colors.<p>
      <ul>
      <li>if empty, point color will be opaque white
      <li>if contains a single color, all points will use that color
      <li>if contains N colors, the first N points (at max) will be rendered
      using the corresponding colors.
      </ul> }
    property Colors: TVectorList read FColors write SetColors;

  published
    { Published Declarations }
    { : If true points do not write their Z to the depth buffer. }
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite;
    { : Tells the component if point coordinates are static.<p>
      If static, changes to the positions should be notified via an
      explicit StructureChanged call, or may not refresh.<br>
      Static sets of points may render faster than dynamic ones. }
    property Static: Boolean read FStatic write SetStatic;
    { : Point size, all points have a fixed size. }
    property Size: Single read FSize write SetSize stored StoreSize;
    { : Points style.<p> }
    property Style: TGLPointStyle read FStyle write SetStyle default PsSquare;
    { : Point parameters as of ARB_point_parameters.<p>
      Allows to vary the size and transparency of points depending
      on their distance to the observer. }
    property PointParameters: TGLPointParameters read FPointParameters
      write SetPointParameters;

  end;

  // TLineNodesAspect
  //
  { : Possible aspects for the nodes of a TLine. }
  TLineNodesAspect = (LnaInvisible, LnaAxes, LnaCube, LnaDodecahedron);

  // TLineSplineMode
  //
  { : Available spline modes for a TLine. }
  TLineSplineMode = (LsmLines, LsmCubicSpline, LsmBezierSpline, LsmNURBSCurve,
    LsmSegments);

  // TGLLinesNode
  //
  { : Specialized Node for use in a TGLLines objects.<p>
    Adds a Color property (TGLColor). }
  TGLLinesNode = class(TGLNode)
  private
    { Private Declarations }
    FColor: TGLColor;

  protected
    { Protected Declarations }
    procedure SetColor(const Val: TGLColor);
    procedure OnColorChange(Sender: TObject);
    function StoreColor: Boolean;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }

    { : The node color.<p>
      Can also defined the line color (interpolated between nodes) if
      loUseNodeColorForLines is set (in TGLLines). }
    property Color: TGLColor read FColor write SetColor stored StoreColor;
  end;

  // TGLLinesNodes
  //
  { : Specialized collection for Nodes in a TGLLines objects.<p>
    Stores TGLLinesNode items. }
  TGLLinesNodes = class(TGLNodes)
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); overload;

    procedure NotifyChange; override;
  end;

  // TGLLineBase
  //
  { : Base class for line objects.<p>
    Introduces line style properties (width, color...). }
  TGLLineBase = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FLineColor: TGLColor;
    FLinePattern: TGLushort;
    FLineWidth: Single;
    FAntiAliased: Boolean;

  protected
    { Protected Declarations }
    procedure SetLineColor(const Value: TGLColor);
    procedure SetLinePattern(const Value: TGLushort);
    procedure SetLineWidth(const Val: Single);
    function StoreLineWidth: Boolean;
    procedure SetAntiAliased(const Val: Boolean);

    { : Setup OpenGL states according to line style.<p>
      You must call RestoreLineStyle after drawing your lines.<p>
      You may use nested calls with SetupLineStyle/RestoreLineStyle. }
    procedure SetupLineStyle;
    { : Restore OpenGL states, must follow a SetupLineStyle }
    procedure RestoreLineStyle;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    { : Indicates if OpenGL should smooth line edges.<p>
      Smoothed lines looks better but are poorly implemented in most OpenGL
      drivers and take *lots* of rendering time. }
    property AntiAliased: Boolean read FAntiAliased write SetAntiAliased
      default False;
    { : Default color of the lines. }
    property LineColor: TGLColor read FLineColor write SetLineColor;
    { : Bitwise line pattern.<p>
      For instance $FFFF (65535) is a white line (stipple disabled), $0000
      is a black line, $CCCC is the stipple used in axes and dummycube, etc. }
    property LinePattern: TGLushort read FLinePattern write SetLinePattern
      default $FFFF;
    { : Default width of the lines. }
    property LineWidth: Single read FLineWidth write SetLineWidth
      stored StoreLineWidth;
    property Visible;
  end;

  // TGLNodedLines
  //
  { : Class that defines lines via a series of nodes.<p>
    Base class, does not render anything. }
  TGLNodedLines = class(TGLLineBase)
  private
    { Private Declarations }
    FNodes: TGLLinesNodes;
    FNodesAspect: TLineNodesAspect;
    FNodeColor: TGLColor;
    FNodeSize: Single;
    FOldNodeColor: TColorVector;

  protected
    { Protected Declarations }
    procedure SetNodesAspect(const Value: TLineNodesAspect);
    procedure SetNodeColor(const Value: TGLColor);
    procedure OnNodeColorChanged(Sender: TObject);
    procedure SetNodes(const ANodes: TGLLinesNodes);
    procedure SetNodeSize(const Val: Single);
    function StoreNodeSize: Boolean;

    procedure DrawNode(var Rci: TRenderContextInfo; X, Y, Z: Single;
      Color: TGLColor);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;

    procedure AddNode(const Coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;

  published
    { Published Declarations }
    { : Default color for nodes.<p>
      lnaInvisible and lnaAxes ignore this setting. }
    property NodeColor: TGLColor read FNodeColor write SetNodeColor;
    { : The nodes list.<p> }
    property Nodes: TGLLinesNodes read FNodes write SetNodes;

    { : Default aspect of line nodes.<p>
      May help you materialize nodes, segments and control points. }
    property NodesAspect: TLineNodesAspect read FNodesAspect
      write SetNodesAspect default LnaAxes;
    { : Size for the various node aspects. }
    property NodeSize: Single read FNodeSize write SetNodeSize
      stored StoreNodeSize;
  end;

  // TLinesOptions
  //
  TLinesOption = (LoUseNodeColorForLines);
  TLinesOptions = set of TLinesOption;

  // TGLLines
  //
  { : Set of 3D line segments.<p>
    You define a 3D Line by adding its nodes in the "Nodes" property. The line
    may be rendered as a set of segment or as a curve (nodes then act as spline
    control points).<p>
    Alternatively, you can also use it to render a set of spacial nodes (points
    in space), just make the lines transparent and the nodes visible by picking
    the node aspect that suits you. }
  TGLLines = class(TGLNodedLines)
  private
    { Private Declarations }
    FDivision: Integer;
    FSplineMode: TLineSplineMode;
    FOptions: TLinesOptions;
    FNURBSOrder: Integer;
    FNURBSTolerance: Single;
    FNURBSKnots: TSingleList;

  protected
    { Protected Declarations }
    procedure SetSplineMode(const Val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetOptions(const Val: TLinesOptions);
    procedure SetNURBSOrder(const Val: Integer);
    procedure SetNURBSTolerance(const Val: Single);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var Rci: TRenderContextInfo); override;

    property NURBSKnots: TSingleList read FNURBSKnots;
    property NURBSOrder: Integer read FNURBSOrder write SetNURBSOrder;
    property NURBSTolerance: Single read FNURBSTolerance
      write SetNURBSTolerance;

  published
    { Published Declarations }
    { : Number of divisions for each segment in spline modes.<p>
      Minimum 1 (disabled), ignored in lsmLines mode. }
    property Division: Integer read FDivision write SetDivision default 10;
    { : Default spline drawing mode.<p> }
    property SplineMode: TLineSplineMode read FSplineMode write SetSplineMode
      default LsmLines;

    { : Rendering options for the line.<p>
      <ul>
      <li>loUseNodeColorForLines: if set lines will be drawn using node
      colors (and color interpolation between nodes), if not, LineColor
      will be used (single color).
      </ul> }
    property Options: TLinesOptions read FOptions write SetOptions;
  end;

  TCubePart = (CpTop, CpBottom, CpFront, CpBack, CpLeft, CpRight);
  TCubeParts = set of TCubePart;

  // TGLCube
  //
  { : A simple cube object.<p>
    This cube use the same material for each of its faces, ie. all faces look
    the same. If you want a multi-material cube, use a mesh in conjunction
    with a TGLFreeForm and a material library. }
  TGLCube = class(TGLSceneObject)
  private
    { Private Declarations }
    FCubeSize: TAffineVector;
    FParts: TCubeParts;
    FNormalDirection: TNormalDirection;
    function GetCubeWHD(const Index: Integer): TGLFloat;
    procedure SetCubeWHD(Index: Integer; AValue: TGLFloat);
    procedure SetParts(AValue: TCubeParts);
    procedure SetNormalDirection(AValue: TNormalDirection);

  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    function GenerateSilhouette(const SilhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
    procedure BuildList(var Rci: TRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const RayStart, RayVector: TVector;
      IntersectPoint: PVector = nil; IntersectNormal: PVector = nil)
      : Boolean; override;

  published
    { Published Declarations }
    property CubeWidth: TGLFloat index 0 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeHeight: TGLFloat index 1 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeDepth: TGLFloat index 2 read GetCubeWHD write SetCubeWHD
      stored False;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default NdOutside;
    property Parts: TCubeParts read FParts write SetParts
      default [CpTop, CpBottom, CpFront, CpBack, CpLeft, CpRight];
  end;

  // TNormalSmoothing
  //
  { : Determines how and if normals are smoothed.<p>
    - nsFlat : facetted look<br>
    - nsSmooth : smooth look<br>
    - nsNone : unlighted rendering, usefull for decla texturing }
  TNormalSmoothing = (NsFlat, NsSmooth, NsNone);

  // TGLQuadricObject
  //
  { : Base class for quadric objects.<p>
    Introduces some basic Quadric interaction functions (the actual quadric
    math is part of the GLU library). }
  TGLQuadricObject = class(TGLSceneObject)
  private
    { Private Declarations }
    FNormals: TNormalSmoothing;
    FNormalDirection: TNormalDirection;

  protected
    { Protected Declarations }
    procedure SetNormals(AValue: TNormalSmoothing);
    procedure SetNormalDirection(AValue: TNormalDirection);
    procedure SetupQuadricParams(Quadric: PGLUquadricObj);
    procedure SetNormalQuadricOrientation(Quadric: PGLUquadricObj);
    procedure SetInvertedQuadricOrientation(Quadric: PGLUquadricObj);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default NsSmooth;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default NdOutside;
  end;

  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TCapType = (CtNone, CtCenter, CtFlat);

  // TGLSphere
  //
  { : A sphere object.<p>
    The sphere can have to and bottom caps, as well as being just a slice
    of sphere. }
  TGLSphere = class(TGLQuadricObject)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;
    procedure SetBottom(AValue: TAngleLimit1);
    procedure SetBottomCap(AValue: TCapType);
    procedure SetRadius(const AValue: TGLFloat);
    procedure SetSlices(AValue: TGLInt);
    procedure SetStart(AValue: TAngleLimit2);
    procedure SetStop(AValue: TAngleLimit2);
    procedure SetStacks(AValue: TGLInt);
    procedure SetTop(AValue: TAngleLimit1);
    procedure SetTopCap(AValue: TCapType);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var Rci: TRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const RayStart, RayVector: TVector;
      IntersectPoint: PVector = nil; IntersectNormal: PVector = nil)
      : Boolean; override;

    function GenerateSilhouette(const SilhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
  published
    { Published Declarations }
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap
      default CtNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default CtNone;
  end;

  // TGLPolygonBase
  //
  { : Base class for objects based on a polygon. }
  TGLPolygonBase = class(TGLSceneObject)
  private
    { Private Declarations }
    FDivision: Integer;
    FSplineMode: TLineSplineMode;

  protected
    { Protected Declarations }
    FNodes: TGLNodes;
    procedure CreateNodes; dynamic;
    procedure SetSplineMode(const Val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const ANodes: TGLNodes);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure AddNode(const Coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;

  published
    { Published Declarations }
    { : The nodes list.<p> }
    property Nodes: TGLNodes read FNodes write SetNodes;
    { : Number of divisions for each segment in spline modes.<p>
      Minimum 1 (disabled), ignored in lsmLines mode. }
    property Division: Integer read FDivision write SetDivision default 10;
    { : Default spline drawing mode.<p>
      This mode is used only for the curve, not for the rotation path. }
    property SplineMode: TLineSplineMode read FSplineMode write SetSplineMode
      default LsmLines;

  end;

  { : Issues OpenGL for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildList(var Rci: TRenderContextInfo; Size: TGLFloat;
  Stipple: Boolean; const Color: TColorVector);
{ : Issues OpenGL for a unit-size dodecahedron. }
procedure DodecahedronBuildList;
{ : Issues OpenGL for a unit-size icosahedron. }
procedure IcosahedronBuildList;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------
implementation

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

uses GLStrings, Spline, XOpenGL, GLState;

const
  CDefaultPointSize: Single = 1.0;

  // CubeWireframeBuildList
  //
procedure CubeWireframeBuildList(var Rci: TRenderContextInfo; Size: TGLFloat;
  Stipple: Boolean; const Color: TColorVector);
var
  Mi, Ma: Single;
begin
  GlPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or
    GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  GlDisable(GL_LIGHTING);
  GlEnable(GL_LINE_SMOOTH);
  if Stipple then
  begin
    GlEnable(GL_LINE_STIPPLE);
    GlEnable(GL_BLEND);
    GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    GlLineStipple(1, $CCCC);
  end;
  GlLineWidth(1);
  Ma := 0.5 * Size;
  Mi := -Ma;
  Rci.GLStates.ResetGLMaterialColors;
  GlColorMaterial(GL_FRONT, GL_EMISSION);
  GlEnable(GL_COLOR_MATERIAL);
  GlColor4fv(@Color);
  GlBegin(GL_LINE_STRIP);
  // front face
  GlVertex3f(Ma, Mi, Mi);
  GlVertex3f(Ma, Ma, Mi);
  GlVertex3f(Ma, Ma, Ma);
  GlVertex3f(Ma, Mi, Ma);
  GlVertex3f(Ma, Mi, Mi);
  // partial up back face
  GlVertex3f(Mi, Mi, Mi);
  GlVertex3f(Mi, Mi, Ma);
  GlVertex3f(Mi, Ma, Ma);
  GlVertex3f(Mi, Ma, Mi);
  // right side low
  GlVertex3f(Ma, Ma, Mi);
  GlEnd;
  GlBegin(GL_LINES);
  // right high
  GlVertex3f(Ma, Ma, Ma);
  GlVertex3f(Mi, Ma, Ma);
  // back low
  GlVertex3f(Mi, Mi, Mi);
  GlVertex3f(Mi, Ma, Mi);
  // left high
  GlVertex3f(Ma, Mi, Ma);
  GlVertex3f(Mi, Mi, Ma);
  GlEnd;
  GlPopAttrib;
end;

// DodecahedronBuildList
//
procedure DodecahedronBuildList;
const
  A = 1.61803398875 * 0.3; // (Sqrt(5)+1)/2
  B = 0.61803398875 * 0.3; // (Sqrt(5)-1)/2
  C = 1 * 0.3;
const
  Vertices: packed array [0 .. 19] of TAffineVector = ((X: - A; Y: 0; Z: B),
    (X: - A; Y: 0; Z: - B), (X: A; Y: 0; Z: - B), (X: A; Y: 0; Z: B), (X: B;
    Y: - A; Z: 0), (X: - B; Y: - A; Z: 0), (X: - B; Y: A; Z: 0), (X: B; Y: A;
    Z: 0), (X: 0; Y: B; Z: - A), (X: 0; Y: - B; Z: - A), (X: 0; Y: - B; Z: A),
    (X: 0; Y: B; Z: A), (X: - C; Y: - C; Z: C), (X: - C; Y: - C; Z: - C), (X: C;
    Y: - C; Z: - C), (X: C; Y: - C; Z: C), (X: - C; Y: C; Z: C), (X: - C; Y: C;
    Z: - C), (X: C; Y: C; Z: - C), (X: C; Y: C; Z: C));

  Polygons: packed array [0 .. 11] of packed array [0 .. 4]
    of Byte = ((0, 12, 10, 11, 16), (1, 17, 8, 9, 13), (2, 14, 9, 8, 18),
    (3, 19, 11, 10, 15), (4, 14, 2, 3, 15), (5, 12, 0, 1, 13),
    (6, 17, 1, 0, 16), (7, 19, 3, 2, 18), (8, 17, 6, 7, 18), (9, 14, 4, 5, 13),
    (10, 12, 5, 4, 15), (11, 19, 7, 6, 16));

var
  I, J: Integer;
  N: TAffineVector;
  FaceIndices: PByteArray;
begin
  for I := 0 to 11 do
  begin
    FaceIndices := @Polygons[I, 0];

    N := CalcPlaneNormal(Vertices[FaceIndices^[0]], Vertices[FaceIndices^[1]],
      Vertices[FaceIndices^[2]]);
    GlNormal3fv(@N);

    GlBegin(GL_TRIANGLE_FAN);
    for J := 0 to 4 do
      GlVertex3fv(@Vertices[FaceIndices^[J]]);
    GlEnd;
  end;
end;

// IcosahedronBuildList
//
procedure IcosahedronBuildList;
const
  A = 0.5;
  B = 0.30901699437; // 1/(1+Sqrt(5))
const
  Vertices: packed array [0 .. 11] of TAffineVector = ((X: 0; Y: - B; Z: - A),
    (X: 0; Y: - B; Z: A), (X: 0; Y: B; Z: - A), (X: 0; Y: B; Z: A), (X: - A;
    Y: 0; Z: - B), (X: - A; Y: 0; Z: B), (X: A; Y: 0; Z: - B), (X: A; Y: 0;
    Z: B), (X: - B; Y: - A; Z: 0), (X: - B; Y: A; Z: 0), (X: B; Y: - A; Z: 0),
    (X: B; Y: A; Z: 0));
  Triangles: packed array [0 .. 19] of packed array [0 .. 2]
    of Byte = ((2, 9, 11), (3, 11, 9), (3, 5, 1), (3, 1, 7), (2, 6, 0),
    (2, 0, 4), (1, 8, 10), (0, 10, 8), (9, 4, 5), (8, 5, 4), (11, 7, 6),
    (10, 6, 7), (3, 9, 5), (3, 7, 11), (2, 4, 9), (2, 11, 6), (0, 8, 4),
    (0, 6, 10), (1, 5, 8), (1, 10, 7));

var
  I, J: Integer;
  N: TAffineVector;
  FaceIndices: PByteArray;
begin
  for I := 0 to 19 do
  begin
    FaceIndices := @Triangles[I, 0];

    N := CalcPlaneNormal(Vertices[FaceIndices^[0]], Vertices[FaceIndices^[1]],
      Vertices[FaceIndices^[2]]);
    GlNormal3fv(@N);

    GlBegin(GL_TRIANGLES);
    for J := 0 to 2 do
      GlVertex3fv(@Vertices[FaceIndices^[J]]);
    GlEnd;
  end;
end;


// ------------------
// ------------------ TGLDummyCube ------------------
// ------------------

// Create
//
constructor TGLDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [OsDirectDraw];
  FCubeSize := 1;
  FEdgeColor := TGLColor.Create(Self);
  FEdgeColor.Initialize(ClrWhite);
  FGroupList := TGLListHandle.Create;
  CamInvarianceMode := CimNone;
end;

// Destroy
//
destructor TGLDummyCube.Destroy;
begin
  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;

// Assign
//
procedure TGLDummyCube.Assign(Source: TPersistent);
begin
  if Source is TGLDummyCube then
  begin
    FCubeSize := TGLDummyCube(Source).FCubeSize;
    FEdgeColor.Color := TGLDummyCube(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TGLDummyCube(Source).FVisibleAtRunTime;
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//
function TGLDummyCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.Coord[0] := 0.5 * Abs(FCubeSize);
  Result.Coord[1] := Result.Coord[0];
  Result.Coord[2] := Result.Coord[0];
  Result.Coord[3] := 0;
end;

// RayCastIntersect
//
function TGLDummyCube.RayCastIntersect(const RayStart, RayVector: TVector;
  IntersectPoint: PVector = nil; IntersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

// BuildList
//
procedure TGLDummyCube.BuildList(var Rci: TRenderContextInfo);
begin
  if (CsDesigning in ComponentState) or (FVisibleAtRunTime) then
    CubeWireframeBuildList(Rci, FCubeSize, True, EdgeColor.Color);
end;

// DoRender
//
procedure TGLDummyCube.DoRender(var Rci: TRenderContextInfo;
  RenderSelf, RenderChildren: Boolean);
begin
  if Assigned(FOnVisibilityDetermination) then
    if not FOnVisibilityDetermination(Self, Rci) then
      Exit;
  if FAmalgamate and (not Rci.Amalgamating) then
  begin
    if FGroupList.Handle = 0 then
    begin
      FGroupList.AllocateHandle;
      Assert(FGroupList.Handle <> 0, 'Handle=0 for ' + ClassName);
      GlNewList(FGroupList.Handle, GL_COMPILE);
      Rci.Amalgamating := True;
      try
        inherited;
      finally
        Rci.Amalgamating := False;
        GlEndList;
      end;
    end;
    GlCallList(FGroupList.Handle);
  end
  else
  begin
    // proceed as usual
    inherited;
  end;
end;

// StructureChanged
//
procedure TGLDummyCube.StructureChanged;
begin
  if FAmalgamate then
    FGroupList.DestroyHandle;
  inherited;
end;

// BarycenterAbsolutePosition
//
function TGLDummyCube.BarycenterAbsolutePosition: TVector;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Result := Children[0].BarycenterAbsolutePosition;
    for I := 1 to Count - 1 do
      Result := VectorAdd(Result, Children[I].BarycenterAbsolutePosition);
    ScaleVector(Result, 1 / Count);
  end
  else
    Result := AbsolutePosition;
end;

// SetCubeSize
//
procedure TGLDummyCube.SetCubeSize(const Val: TGLFloat);
begin
  if Val <> FCubeSize then
  begin
    FCubeSize := Val;
    StructureChanged;
  end;
end;

// SetEdgeColor
//
procedure TGLDummyCube.SetEdgeColor(const Val: TGLColor);
begin
  if Val <> FEdgeColor then
  begin
    FEdgeColor.Assign(Val);
    StructureChanged;
  end;
end;

// SetVisibleAtRunTime
//
procedure TGLDummyCube.SetVisibleAtRunTime(const Val: Boolean);
begin
  if Val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := Val;
    StructureChanged;
  end;
end;

// SetAmalgamate
//
procedure TGLDummyCube.SetAmalgamate(const Val: Boolean);
begin
  if Val <> FAmalgamate then
  begin
    FAmalgamate := Val;
    if Val then
      ObjectStyle := ObjectStyle + [OsDoesTemperWithColorsOrFaceWinding]
    else
    begin
      FGroupList.DestroyHandle;
      ObjectStyle := ObjectStyle - [OsDoesTemperWithColorsOrFaceWinding]
    end;
    inherited StructureChanged;
  end;
end;

// ------------------
// ------------------ TGLPlane ------------------
// ------------------

// Create
//
constructor TGLPlane.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  ObjectStyle := ObjectStyle + [OsDirectDraw];
  FStyle := [PsSingleQuad, PsTileTexture];
end;

// Assign
//
procedure TGLPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLPlane) then
  begin
    FWidth := TGLPlane(Source).FWidth;
    FHeight := TGLPlane(Source).FHeight;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.Coord[0] := 0.5 * Abs(FWidth);
  Result.Coord[1] := 0.5 * Abs(FHeight);
  Result.Coord[2] := 0;
end;

// RayCastIntersect
//
function TGLPlane.RayCastIntersect(const RayStart, RayVector: TVector;
  IntersectPoint: PVector = nil; IntersectNormal: PVector = nil): Boolean;
var
  LocRayStart, LocRayVector, Ip: TVector;
  T: Single;
begin
  LocRayStart := AbsoluteToLocal(RayStart);
  LocRayVector := AbsoluteToLocal(RayVector);
  if LocRayStart.Coord[2] >= 0 then
  begin
    // ray start over plane
    if LocRayVector.Coord[2] < 0 then
    begin
      T := LocRayStart.Coord[2] / LocRayVector.Coord[2];
      Ip.Coord[0] := LocRayStart.Coord[0] - T * LocRayVector.Coord[0];
      Ip.Coord[1] := LocRayStart.Coord[1] - T * LocRayVector.Coord[1];
      if (Abs(Ip.Coord[0]) <= 0.5 * Width) and (Abs(Ip.Coord[1]) <= 0.5 * Height)
      then
      begin
        Result := True;
        if Assigned(IntersectNormal) then
          IntersectNormal^ := AbsoluteDirection;
      end
      else
        Result := False;
    end
    else
      Result := False;
  end
  else
  begin
    // ray start below plane
    if LocRayVector.Coord[2] > 0 then
    begin
      T := LocRayStart.Coord[2] / LocRayVector.Coord[2];
      Ip.Coord[0] := LocRayStart.Coord[0] - T * LocRayVector.Coord[0];
      Ip.Coord[1] := LocRayStart.Coord[1] - T * LocRayVector.Coord[1];
      if (Abs(Ip.Coord[0]) <= 0.5 * Width) and (Abs(Ip.Coord[1]) <= 0.5 * Height)
      then
      begin
        Result := True;
        if Assigned(IntersectNormal) then
          IntersectNormal^ := VectorNegate(AbsoluteDirection);
      end
      else
        Result := False;
    end
    else
      Result := False;
  end;
  if Result and Assigned(IntersectPoint) then
  begin
    Ip.Coord[2] := 0;
    Ip.Coord[3] := 1;
    IntersectPoint^ := LocalToAbsolute(Ip);
  end;
end;

// GenerateSilhouette
//
function TGLPlane.GenerateSilhouette(const SilhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  Hw, Hh: Single;
begin
  Result := TGLSilhouette.Create;

  Hw := FWidth * 0.5;
  Hh := FHeight * 0.5;

  with Result.Vertices do
  begin
    AddPoint(Hw, Hh);
    AddPoint(Hw, -Hh);
    AddPoint(-Hw, -Hh);
    AddPoint(-Hw, Hh);
  end;

  with Result.Indices do
  begin
    Add(0, 1);
    Add(1, 2);
    Add(2, 3);
    Add(3, 0);
  end;

  if SilhouetteParameters.CappingRequired then
    with Result.CapIndices do
    begin
      Add(0, 1, 2);
      Add(2, 3, 0);
    end;
end;

// BuildList
//
procedure TGLPlane.BuildList(var Rci: TRenderContextInfo);
var
  Hw, Hh, PosXFact, PosYFact, PX, PY0, PY1: TGLFloat;
  Tx0, Tx1, Ty0, Ty1, TexSFact, TexTFact: TGLFloat;
  TexS, TexT0, TexT1: TGLFloat;
  X, Y: Integer;
begin
  Hw := FWidth * 0.5;
  Hh := FHeight * 0.5;
  GlNormal3fv(@ZVector);
  // determine tex coords extents
  if PsTileTexture in FStyle then
  begin
    Tx0 := FXOffset;
    Tx1 := FXTiles * FXScope + FXOffset;
    Ty0 := FYOffset;
    Ty1 := FYTiles * FYScope + FYOffset;
  end
  else
  begin
    Tx0 := 0;
    Ty0 := Tx0;
    Tx1 := FXScope;
    Ty1 := FYScope;
  end;

  if NoZWrite then
    GlDepthMask(False);

  if PsSingleQuad in FStyle then
  begin
    // single quad plane
    GlBegin(GL_QUADS);
    XglTexCoord2f(Tx1, Ty1);
    GlVertex2f(Hw, Hh);
    XglTexCoord2f(Tx0, Ty1);
    GlVertex2f(-Hw, Hh);
    XglTexCoord2f(Tx0, Ty0);
    GlVertex2f(-Hw, -Hh);
    XglTexCoord2f(Tx1, Ty0);
    GlVertex2f(Hw, -Hh);
    GlEnd;
  end
  else
  begin
    // multi-quad plane (actually built from tri-strips)
    TexSFact := (Tx1 - Tx0) / FXTiles;
    TexTFact := (Ty1 - Ty0) / FYTiles;
    PosXFact := FWidth / FXTiles;
    PosYFact := FHeight / FYTiles;
    TexT0 := 0;
    PY0 := -Hh;
    for Y := 0 to FYTiles - 1 do
    begin
      TexT1 := (Y + 1) * TexTFact;
      PY1 := (Y + 1) * PosYFact - Hh;
      GlBegin(GL_TRIANGLE_STRIP);
      for X := 0 to FXTiles do
      begin
        TexS := Tx0 + X * TexSFact;
        PX := X * PosXFact - Hw;
        XglTexCoord2f(TexS, TexT1);
        GlVertex2f(PX, PY1);
        XglTexCoord2f(TexS, TexT0);
        GlVertex2f(PX, PY0);
      end;
      GlEnd;
      TexT0 := TexT1;
      PY0 := PY1;
    end;
  end;

  if NoZWrite then
    GlDepthMask(True);

end;

// SetWidth
//
procedure TGLPlane.SetWidth(const AValue: Single);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    StructureChanged;
  end;
end;

// ScreenRect
//
function TGLPlane.ScreenRect: TGLRect;
var
  V: array [0 .. 3] of TVector;
  Buf: TGLSceneBuffer;
  Hw, Hh: TGLFloat;
begin
  Buf := Scene.CurrentBuffer;
  if Assigned(Buf) then
  begin
    Hw := FWidth * 0.5;
    Hh := FHeight * 0.5;
    V[0] := LocalToAbsolute(PointMake(-Hw, -Hh, 0));
    V[1] := LocalToAbsolute(PointMake(Hw, -Hh, 0));
    V[2] := LocalToAbsolute(PointMake(Hw, Hh, 0));
    V[3] := LocalToAbsolute(PointMake(-Hw, Hh, 0));
    Buf.WorldToScreen(@V[0], 4);
    Result.Left := Round(MinFloat([V[0].Coord[0], V[1].Coord[0], V[2].Coord[0],
      V[3].Coord[0]]));
    Result.Right := Round(MaxFloat([V[0].Coord[0], V[1].Coord[0], V[2].Coord[0],
      V[3].Coord[0]]));
    Result.Top := Round(MinFloat([V[0].Coord[1], V[1].Coord[1], V[2].Coord[1],
      V[3].Coord[1]]));
    Result.Bottom := Round(MaxFloat([V[0].Coord[1], V[1].Coord[1],
      V[2].Coord[1], V[3].Coord[1]]));
  end
  else
    FillChar(Result, SizeOf(TGLRect), 0);
end;

// PointDistance
//
function TGLPlane.PointDistance(const APoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(APoint, AbsolutePosition),
    AbsoluteDirection);
end;

// SetHeight
//
procedure TGLPlane.SetHeight(const AValue: Single);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    StructureChanged;
  end;
end;

// SetXOffset
//
procedure TGLPlane.SetXOffset(const Value: TGLFloat);
begin
  if Value <> FXOffset then
  begin
    FXOffset := Value;
    StructureChanged;
  end;
end;

// SetXScope
//
procedure TGLPlane.SetXScope(const Value: TGLFloat);
begin
  if Value <> FXScope then
  begin
    FXScope := Value;
    if FXScope > 1 then
      FXScope := 1;
    StructureChanged;
  end;
end;

// StoreXScope
//
function TGLPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

// SetXTiles
//
procedure TGLPlane.SetXTiles(const Value: Cardinal);
begin
  if Value <> FXTiles then
  begin
    FXTiles := Value;
    StructureChanged;
  end;
end;

// SetYOffset
//
procedure TGLPlane.SetYOffset(const Value: TGLFloat);
begin
  if Value <> FYOffset then
  begin
    FYOffset := Value;
    StructureChanged;
  end;
end;

// SetYScope
//
procedure TGLPlane.SetYScope(const Value: TGLFloat);
begin
  if Value <> FYScope then
  begin
    FYScope := Value;
    if FYScope > 1 then
      FYScope := 1;
    StructureChanged;
  end;
end;

// StoreYScope
//
function TGLPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

// SetYTiles
//
procedure TGLPlane.SetYTiles(const Value: Cardinal);
begin
  if Value <> FYTiles then
  begin
    FYTiles := Value;
    StructureChanged;
  end;
end;

// SetStyle
//
procedure TGLPlane.SetStyle(const Val: TPlaneStyles);
begin
  if Val <> FStyle then
  begin
    FStyle := Val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TGLSprite ------------------
// ------------------

// Create
//
constructor TGLSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [OsDirectDraw, OsNoVisibilityCulling];
  FAlphaChannel := 1;
  FWidth := 1;
  FHeight := 1;
end;

// Assign
//
procedure TGLSprite.Assign(Source: TPersistent);
begin
  if Source is TGLSprite then
  begin
    FWidth := TGLSprite(Source).FWidth;
    FHeight := TGLSprite(Source).FHeight;
    FRotation := TGLSprite(Source).FRotation;
    FAlphaChannel := TGLSprite(Source).FAlphaChannel;
    FNoZWrite := TGLSprite(Source).FNoZWrite;
  end;
  inherited Assign(Source);
end;

function TGLSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.Coord[0] := 0.5 * Abs(FWidth);
  Result.Coord[1] := 0.5 * Abs(FHeight);
  // Sprites turn with the camera and can be considered to have the same depth
  // as width
  Result.Coord[2] := 0.5 * Abs(FWidth);
end;

// BuildList
//
procedure TGLSprite.BuildList(var Rci: TRenderContextInfo);
var
  Vx, Vy: TAffineVector;
  W, H: Single;
  Mat: TMatrix;
  U0, V0, U1, V1: Integer;
begin
  if FAlphaChannel <> 1 then
    Rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, FAlphaChannel);
  if NoZWrite then
    GlDepthMask(False);
  GlGetFloatv(GL_MODELVIEW_MATRIX, @Mat);
  // extraction of the "vecteurs directeurs de la matrice"
  // (dunno how they are named in english)
  W := FWidth * 0.5;
  H := FHeight * 0.5;
  Vx.Coord[0] := Mat.Coord[0].Coord[0];
  Vy.Coord[0] := Mat.Coord[0].Coord[1];
  Vx.Coord[1] := Mat.Coord[1].Coord[0];
  Vy.Coord[1] := Mat.Coord[1].Coord[1];
  Vx.Coord[2] := Mat.Coord[2].Coord[0];
  Vy.Coord[2] := Mat.Coord[2].Coord[1];
  ScaleVector(Vx, W / VectorLength(Vx));
  ScaleVector(Vy, H / VectorLength(Vy));
  if FMirrorU then
  begin
    U0 := 1;
    U1 := 0;
  end
  else
  begin
    U0 := 0;
    U1 := 1;
  end;
  if FMirrorV then
  begin
    V0 := 1;
    V1 := 0;
  end
  else
  begin
    V0 := 0;
    V1 := 1;
  end;

  if FRotation <> 0 then
  begin
    GlPushMatrix;
    GlRotatef(FRotation, Mat.Coord[0].Coord[2], Mat.Coord[1].Coord[2],
      Mat.Coord[2].Coord[2]);
  end;
  GlBegin(GL_QUADS);
  XglTexCoord2f(U1, V1);
  GlVertex3f(Vx.Coord[0] + Vy.Coord[0], Vx.Coord[1] + Vy.Coord[1],
    Vx.Coord[2] + Vy.Coord[2]);
  XglTexCoord2f(U0, V1);
  GlVertex3f(-Vx.Coord[0] + Vy.Coord[0], -Vx.Coord[1] + Vy.Coord[1],
    -Vx.Coord[2] + Vy.Coord[2]);
  XglTexCoord2f(U0, V0);
  GlVertex3f(-Vx.Coord[0] - Vy.Coord[0], -Vx.Coord[1] - Vy.Coord[1],
    -Vx.Coord[2] - Vy.Coord[2]);
  XglTexCoord2f(U1, V0);
  GlVertex3f(Vx.Coord[0] - Vy.Coord[0], Vx.Coord[1] - Vy.Coord[1],
    Vx.Coord[2] - Vy.Coord[2]);
  GlEnd;
  if FRotation <> 0 then
    GlPopMatrix;

  if NoZWrite then
    GlDepthMask(True);
end;

// SetWidth
//
procedure TGLSprite.SetWidth(const Val: TGLFloat);
begin
  if FWidth <> Val then
  begin
    FWidth := Val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//
procedure TGLSprite.SetHeight(const Val: TGLFloat);
begin
  if FHeight <> Val then
  begin
    FHeight := Val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//
procedure TGLSprite.SetRotation(const Val: TGLFloat);
begin
  if FRotation <> Val then
  begin
    FRotation := Val;
    NotifyChange(Self);
  end;
end;

// SetAlphaChannel
//
procedure TGLSprite.SetAlphaChannel(const Val: Single);
begin
  if Val <> FAlphaChannel then
  begin
    if Val < 0 then
      FAlphaChannel := 0
    else if Val > 1 then
      FAlphaChannel := 1
    else
      FAlphaChannel := Val;
    NotifyChange(Self);
  end;
end;

// StoreAlphaChannel
//
function TGLSprite.StoreAlphaChannel: Boolean;
begin
  Result := (FAlphaChannel <> 1);
end;

// SetNoZWrite
//
procedure TGLSprite.SetNoZWrite(const Val: Boolean);
begin
  FNoZWrite := Val;
  NotifyChange(Self);
end;

// SetMirrorU
//
procedure TGLSprite.SetMirrorU(const Val: Boolean);
begin
  FMirrorU := Val;
  NotifyChange(Self);
end;

// SetMirrorV
//
procedure TGLSprite.SetMirrorV(const Val: Boolean);
begin
  FMirrorV := Val;
  NotifyChange(Self);
end;

// SetSize
//
procedure TGLSprite.SetSize(const Width, Height: TGLFloat);
begin
  FWidth := Width;
  FHeight := Height;
  NotifyChange(Self);
end;

// SetSquareSize
//
procedure TGLSprite.SetSquareSize(const Size: TGLFloat);
begin
  FWidth := Size;
  FHeight := Size;
  NotifyChange(Self);
end;

// ------------------
// ------------------ TGLPointParameters ------------------
// ------------------

// Create
//
constructor TGLPointParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMinSize := 0;
  FMaxSize := 128;
  FFadeTresholdSize := 1;
  FDistanceAttenuation := TGLCoordinates.CreateInitialized(Self, XHmgVector,
    CsVector);
end;

// Destroy
//
destructor TGLPointParameters.Destroy;
begin
  FDistanceAttenuation.Free;
  inherited;
end;

// Assign
//
procedure TGLPointParameters.Assign(Source: TPersistent);
begin
  if Source is TGLPointParameters then
  begin
    FMinSize := TGLPointParameters(Source).FMinSize;
    FMaxSize := TGLPointParameters(Source).FMaxSize;
    FFadeTresholdSize := TGLPointParameters(Source).FFadeTresholdSize;
    FDistanceAttenuation.Assign(TGLPointParameters(Source).DistanceAttenuation);
  end;
end;

// DefineProperties
//
procedure TGLPointParameters.DefineProperties(Filer: TFiler);
var
  DefaultParams: Boolean;
begin
  inherited;
  DefaultParams := (FMaxSize = 128) and (FMinSize = 0) and
    (FFadeTresholdSize = 1);
  Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
    not DefaultParams);
end;

// ReadData
//
procedure TGLPointParameters.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FMinSize, SizeOf(Single));
    Read(FMaxSize, SizeOf(Single));
    Read(FFadeTresholdSize, SizeOf(Single));
  end;
end;

// WriteData
//
procedure TGLPointParameters.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FMinSize, SizeOf(Single));
    Write(FMaxSize, SizeOf(Single));
    Write(FFadeTresholdSize, SizeOf(Single));
  end;
end;

// Apply
//
procedure TGLPointParameters.Apply;
begin
  if Enabled and GL_ARB_point_parameters then
  begin
    GlPointParameterfARB(GL_POINT_SIZE_MIN_ARB, FMinSize);
    GlPointParameterfARB(GL_POINT_SIZE_MAX_ARB, FMaxSize);
    GlPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
    GlPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB,
      FDistanceAttenuation.AsAddress);
  end;
end;

// UnApply
//
procedure TGLPointParameters.UnApply;
begin
  if Enabled and GL_ARB_point_parameters then
  begin
    GlPointParameterfARB(GL_POINT_SIZE_MIN_ARB, 0);
    GlPointParameterfARB(GL_POINT_SIZE_MAX_ARB, 128);
    GlPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
    GlPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB, @XVector);
  end;
end;

// SetEnabled
//
procedure TGLPointParameters.SetEnabled(const Val: Boolean);
begin
  if Val <> FEnabled then
  begin
    FEnabled := Val;
    NotifyChange(Self);
  end;
end;

// SetMinSize
//
procedure TGLPointParameters.SetMinSize(const Val: Single);
begin
  if Val <> FMinSize then
  begin
    if Val < 0 then
      FMinSize := 0
    else
      FMinSize := Val;
    NotifyChange(Self);
  end;
end;

// SetMaxSize
//
procedure TGLPointParameters.SetMaxSize(const Val: Single);
begin
  if Val <> FMaxSize then
  begin
    if Val < 0 then
      FMaxSize := 0
    else
      FMaxSize := Val;
    NotifyChange(Self);
  end;
end;

// SetFadeTresholdSize
//
procedure TGLPointParameters.SetFadeTresholdSize(const Val: Single);
begin
  if Val <> FFadeTresholdSize then
  begin
    if Val < 0 then
      FFadeTresholdSize := 0
    else
      FFadeTresholdSize := Val;
    NotifyChange(Self);
  end;
end;

// SetDistanceAttenuation
//
procedure TGLPointParameters.SetDistanceAttenuation(const Val: TGLCoordinates);
begin
  FDistanceAttenuation.Assign(Val);
end;

// ------------------
// ------------------ TGLPoints ------------------
// ------------------

// Create
//
constructor TGLPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [OsDirectDraw, OsNoVisibilityCulling];
  FStyle := PsSquare;
  FSize := CDefaultPointSize;
  FPositions := TAffineVectorList.Create;
  FColors := TVectorList.Create;
  FPointParameters := TGLPointParameters.Create(Self);
end;

// Destroy
//
destructor TGLPoints.Destroy;
begin
  FPointParameters.Free;
  FColors.Free;
  FPositions.Free;
  inherited;
end;

// Assign
//
procedure TGLPoints.Assign(Source: TPersistent);
begin
  if Source is TGLPoints then
  begin
    FSize := TGLPoints(Source).FSize;
    FStyle := TGLPoints(Source).FStyle;
    FPositions.Assign(TGLPoints(Source).FPositions);
    FColors.Assign(TGLPoints(Source).FColors);
    StructureChanged
  end;
  inherited Assign(Source);
end;

// BuildList
//
procedure TGLPoints.BuildList(var Rci: TRenderContextInfo);
var
  N: Integer;
  V: TVector;
begin
  N := FPositions.Count;
  case FColors.Count of
    0:
      GlColor4f(1, 1, 1, 1);
    1:
      GlColor4fv(PGLFloat(FColors.List));
  else
    if FColors.Count < N then
      N := FColors.Count;
    GlColorPointer(4, GL_FLOAT, 0, FColors.List);
    GlEnableClientState(GL_COLOR_ARRAY);
  end;
  GlPushAttrib(GL_ENABLE_BIT);
  GlDisable(GL_LIGHTING);
  if N = 0 then
  begin
    V := NullHmgPoint;
    GlVertexPointer(3, GL_FLOAT, 0, @V);
    N := 1;
  end
  else
    GlVertexPointer(3, GL_FLOAT, 0, FPositions.List);
  GlEnableClientState(GL_VERTEX_ARRAY);
  if NoZWrite then
    GlDepthMask(False);
  GlPointSize(FSize);
  PointParameters.Apply;
  if GL_EXT_compiled_vertex_array and (N > 64) then
    GlLockArraysEXT(0, N);
  case FStyle of
    PsSquare:
      begin
        // square point (simplest method, fastest)
        GlDisable(GL_BLEND);
      end;
    PsRound:
      begin
        GlEnable(GL_POINT_SMOOTH);
        GlEnable(GL_ALPHA_TEST);
        GlAlphaFunc(GL_GREATER, 0.5);
        GlDisable(GL_BLEND);
      end;
    PsSmooth:
      begin
        GlEnable(GL_POINT_SMOOTH);
        GlEnable(GL_ALPHA_TEST);
        GlAlphaFunc(GL_NOTEQUAL, 0.0);
        GlEnable(GL_BLEND);
        GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
    PsSmoothAdditive:
      begin
        GlEnable(GL_POINT_SMOOTH);
        GlEnable(GL_ALPHA_TEST);
        GlAlphaFunc(GL_NOTEQUAL, 0.0);
        GlEnable(GL_BLEND);
        GlBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
    PsSquareAdditive:
      begin
        GlEnable(GL_BLEND);
        GlBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
  else
    Assert(False);
  end;
  GlDrawArrays(GL_POINTS, 0, N);
  if GL_EXT_compiled_vertex_array and (N > 64) then
    GlUnlockArraysEXT;
  PointParameters.UnApply;
  if NoZWrite then
    GlDepthMask(True);
  GlDisableClientState(GL_VERTEX_ARRAY);
  if FColors.Count > 1 then
    GlDisableClientState(GL_COLOR_ARRAY);
  GlPopAttrib;
  // restore default GLScene AlphaFunc
  GlAlphaFunc(GL_GREATER, 0);
end;

// StoreSize
//
function TGLPoints.StoreSize: Boolean;
begin
  Result := (FSize <> CDefaultPointSize);
end;

// SetNoZWrite
//
procedure TGLPoints.SetNoZWrite(const Val: Boolean);
begin
  if FNoZWrite <> Val then
  begin
    FNoZWrite := Val;
    StructureChanged;
  end;
end;

// SetStatic
//
procedure TGLPoints.SetStatic(const Val: Boolean);
begin
  if FStatic <> Val then
  begin
    FStatic := Val;
    if Val then
      ObjectStyle := ObjectStyle - [OsDirectDraw]
    else
      ObjectStyle := ObjectStyle + [OsDirectDraw];
    StructureChanged;
  end;
end;

// SetSize
//
procedure TGLPoints.SetSize(const Val: Single);
begin
  if FSize <> Val then
  begin
    FSize := Val;
    StructureChanged;
  end;
end;

// SetPositions
//
procedure TGLPoints.SetPositions(const Val: TAffineVectorList);
begin
  FPositions.Assign(Val);
  StructureChanged;
end;

// SetColors
//
procedure TGLPoints.SetColors(const Val: TVectorList);
begin
  FColors.Assign(Val);
  StructureChanged;
end;

// SetStyle
//
procedure TGLPoints.SetStyle(const Val: TGLPointStyle);
begin
  if FStyle <> Val then
  begin
    FStyle := Val;
    StructureChanged;
  end;
end;

// SetPointParameters
//
procedure TGLPoints.SetPointParameters(const Val: TGLPointParameters);
begin
  FPointParameters.Assign(Val);
end;

// ------------------
// ------------------ TGLLineBase ------------------
// ------------------

// Create
//
constructor TGLLineBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := TGLColor.Create(Self);
  FLineColor.Initialize(ClrWhite);
  FLinePattern := $FFFF;
  FAntiAliased := False;
  FLineWidth := 1.0;
end;

// Destroy
//
destructor TGLLineBase.Destroy;
begin
  FLineColor.Free;
  inherited Destroy;
end;

// SetLineColor
//
procedure TGLLineBase.SetLineColor(const Value: TGLColor);
begin
  FLineColor.Color := Value.Color;
  StructureChanged;
end;

// SetLinePattern
//
procedure TGLLineBase.SetLinePattern(const Value: TGLushort);
begin
  if FLinePattern <> Value then
  begin
    FLinePattern := Value;
    StructureChanged;
  end;
end;

// SetLineWidth
//
procedure TGLLineBase.SetLineWidth(const Val: Single);
begin
  if FLineWidth <> Val then
  begin
    FLineWidth := Val;
    StructureChanged;
  end;
end;

// StoreLineWidth
//
function TGLLineBase.StoreLineWidth: Boolean;
begin
  Result := (FLineWidth <> 1.0);
end;

// SetAntiAliased
//
procedure TGLLineBase.SetAntiAliased(const Val: Boolean);
begin
  if FAntiAliased <> Val then
  begin
    FAntiAliased := Val;
    StructureChanged;
  end;
end;

// Assign
//
procedure TGLLineBase.Assign(Source: TPersistent);
begin
  if Source is TGLLineBase then
  begin
    LineColor := TGLLineBase(Source).FLineColor;
    LinePattern := TGLLineBase(Source).FLinePattern;
    LineWidth := TGLLineBase(Source).FLineWidth;
    AntiAliased := TGLLineBase(Source).FAntiAliased;
  end;
  inherited Assign(Source);
end;

// SetupLineStyle
//
procedure TGLLineBase.SetupLineStyle;
begin
  GlPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or
    GL_COLOR_BUFFER_BIT);
  GlDisable(GL_LIGHTING);
  if FLinePattern <> $FFFF then
  begin
    GlEnable(GL_LINE_STIPPLE);
    GlEnable(GL_BLEND);
    GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    GlLineStipple(1, FLinePattern);
  end;
  if FAntiAliased then
  begin
    GlEnable(GL_LINE_SMOOTH);
    GlEnable(GL_BLEND);
    GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
    GlDisable(GL_LINE_SMOOTH);
  GlLineWidth(FLineWidth);
  if FLineColor.Alpha <> 1 then
  begin
    if not FAntiAliased then
    begin
      GlEnable(GL_BLEND);
      GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    end;
    GlColor4fv(FLineColor.AsAddress);
  end
  else
    GlColor3fv(FLineColor.AsAddress);
end;

// RestoreLineStyle
//
procedure TGLLineBase.RestoreLineStyle;
begin
  GlPopAttrib;
end;

// ------------------
// ------------------ TGLLinesNode ------------------
// ------------------

// Create
//
constructor TGLLinesNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := TGLColor.Create(Self);
  FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TGLLines)
    .NodeColor.Color);
  FColor.OnNotifyChange := OnColorChange;
end;

// Destroy
//
destructor TGLLinesNode.Destroy;
begin
  FColor.Free;
  inherited Destroy;
end;

// Assign
//
procedure TGLLinesNode.Assign(Source: TPersistent);
begin
  if Source is TGLLinesNode then
    FColor.Assign(TGLLinesNode(Source).FColor);
  inherited;
end;

// SetColor
//
procedure TGLLinesNode.SetColor(const Val: TGLColor);
begin
  FColor.Assign(Val);
end;

// OnColorChange
//
procedure TGLLinesNode.OnColorChange(Sender: TObject);
begin
  (Collection as TGLNodes).NotifyChange;
end;

// StoreColor
//
function TGLLinesNode.StoreColor: Boolean;
begin
  Result := not VectorEquals((TGLLinesNodes(Collection).GetOwner as TGLLines)
    .NodeColor.Color, FColor.Color);
end;

// ------------------
// ------------------ TGLLinesNodes ------------------
// ------------------

// Create
//
constructor TGLLinesNodes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLinesNode);
end;

// NotifyChange
//
procedure TGLLinesNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TGLBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TGLNodedLines ------------------
// ------------------

// Create
//
constructor TGLNodedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TGLLinesNodes.Create(Self);
  FNodeColor := TGLColor.Create(Self);
  FNodeColor.Initialize(ClrBlue);
  FNodeColor.OnNotifyChange := OnNodeColorChanged;
  FOldNodeColor := ClrBlue;
  FNodesAspect := LnaAxes;
  FNodeSize := 1;
end;

// Destroy
//
destructor TGLNodedLines.Destroy;
begin
  FNodes.Free;
  FNodeColor.Free;
  inherited Destroy;
end;

// SetNodesAspect
//
procedure TGLNodedLines.SetNodesAspect(const Value: TLineNodesAspect);
begin
  if Value <> FNodesAspect then
  begin
    FNodesAspect := Value;
    StructureChanged;
  end;
end;

// SetNodeColor
//
procedure TGLNodedLines.SetNodeColor(const Value: TGLColor);
begin
  FNodeColor.Color := Value.Color;
  StructureChanged;
end;

// OnNodeColorChanged
//
procedure TGLNodedLines.OnNodeColorChanged(Sender: TObject);
var
  I: Integer;
begin
  // update color for nodes...
  for I := 0 to Nodes.Count - 1 do
    if VectorEquals(TGLLinesNode(Nodes[I]).Color.Color, FOldNodeColor) then
      TGLLinesNode(Nodes[I]).Color.Assign(FNodeColor);
  SetVector(FOldNodeColor, FNodeColor.Color);
end;

// SetNodes
//
procedure TGLNodedLines.SetNodes(const ANodes: TGLLinesNodes);
begin
  FNodes.Assign(ANodes);
  StructureChanged;
end;

// SetNodeSize
//
procedure TGLNodedLines.SetNodeSize(const Val: Single);
begin
  if Val <= 0 then
    FNodeSize := 1
  else
    FNodeSize := Val;
  StructureChanged;
end;

// StoreNodeSize
//
function TGLNodedLines.StoreNodeSize: Boolean;
begin
  Result := FNodeSize <> 1;
end;

// Assign
//
procedure TGLNodedLines.Assign(Source: TPersistent);
begin
  if Source is TGLNodedLines then
  begin
    SetNodes(TGLNodedLines(Source).FNodes);
    FNodesAspect := TGLNodedLines(Source).FNodesAspect;
    FNodeColor.Color := TGLNodedLines(Source).FNodeColor.Color;
    FNodeSize := TGLNodedLines(Source).FNodeSize;
  end;
  inherited Assign(Source);
end;

// DrawNode
//
procedure TGLNodedLines.DrawNode(var Rci: TRenderContextInfo; X, Y, Z: Single;
  Color: TGLColor);
begin
  GlPushMatrix;
  GlTranslatef(X, Y, Z);
  case NodesAspect of
    LnaAxes:
      AxesBuildList(Rci, $CCCC, FNodeSize * 0.5);
    LnaCube:
      CubeWireframeBuildList(Rci, FNodeSize, False, Color.Color);
    LnaDodecahedron:
      begin
        if FNodeSize <> 1 then
        begin
          GlPushMatrix;
          GlScalef(FNodeSize, FNodeSize, FNodeSize);
          Rci.GLStates.SetGLMaterialColors(GL_FRONT, ClrBlack, ClrGray20,
            Color.Color, ClrBlack, 0);
          DodecahedronBuildList;
          GlPopMatrix;
        end
        else
        begin
          Rci.GLStates.SetGLMaterialColors(GL_FRONT, ClrBlack, ClrGray20,
            Color.Color, ClrBlack, 0);
          DodecahedronBuildList;
        end;
      end;
  else
    Assert(False)
  end;
  GlPopMatrix;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLNodedLines.AxisAlignedDimensionsUnscaled: TVector;
var
  I: Integer;
begin
  RstVector(Result);
  for I := 0 to Nodes.Count - 1 do
    MaxVector(Result, VectorAbs(Nodes[I].AsVector));
  // EG: commented out, line below looks suspicious, since scale isn't taken
  // into account in previous loop, must have been hiding another bug... somewhere...
  // DivideVector(Result, Scale.AsVector);     //DanB ?
end;

// AddNode (coords)
//
procedure TGLNodedLines.AddNode(const Coords: TGLCoordinates);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  if Assigned(Coords) then
    N.AsVector := Coords.AsVector;
  StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLNodedLines.AddNode(const X, Y, Z: TGLfloat);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//
procedure TGLNodedLines.AddNode(const Value: TVector);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := Value;
  StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLNodedLines.AddNode(const Value: TAffineVector);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TGLLines ------------------
// ------------------

// Create
//
constructor TGLLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDivision := 10;
  FSplineMode := LsmLines;
  FNURBSKnots := TSingleList.Create;
  FNURBSOrder := 0;
  FNURBSTolerance := 50;
end;

// Destroy
//
destructor TGLLines.Destroy;
begin
  FNURBSKnots.Free;
  inherited Destroy;
end;

// SetDivision
//
procedure TGLLines.SetDivision(const Value: Integer);
begin
  if Value <> FDivision then
  begin
    if Value < 1 then
      FDivision := 1
    else
      FDivision := Value;
    StructureChanged;
  end;
end;

// SetOptions
//
procedure TGLLines.SetOptions(const Val: TLinesOptions);
begin
  FOptions := Val;
  StructureChanged;
end;

// SetSplineMode
//
procedure TGLLines.SetSplineMode(const Val: TLineSplineMode);
begin
  if FSplineMode <> Val then
  begin
    FSplineMode := Val;
    StructureChanged;
  end;
end;

// SetNURBSOrder
//
procedure TGLLines.SetNURBSOrder(const Val: Integer);
begin
  if Val <> FNURBSOrder then
  begin
    FNURBSOrder := Val;
    StructureChanged;
  end;
end;

// SetNURBSTolerance
//
procedure TGLLines.SetNURBSTolerance(const Val: Single);
begin
  if Val <> FNURBSTolerance then
  begin
    FNURBSTolerance := Val;
    StructureChanged;
  end;
end;

// Assign
//
procedure TGLLines.Assign(Source: TPersistent);
begin
  if Source is TGLLines then
  begin
    FDivision := TGLLines(Source).FDivision;
    FSplineMode := TGLLines(Source).FSplineMode;
    FOptions := TGLLines(Source).FOptions;
  end;
  inherited Assign(Source);
end;

// BuildList
//
procedure TGLLines.BuildList(var Rci: TRenderContextInfo);
var
  I, N: Integer;
  A, B, C: TGLFloat;
  F: Single;
  Spline: TCubicSpline;
  VertexColor: TVector;
  NodeBuffer: array of TAffineVector;
  ColorBuffer: array of TVector;
  NurbsRenderer: PGLUNurbs;
begin
  if Nodes.Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle;

    // Set up the control point buffer for Bezier splines and NURBS curves.
    // If required this could be optimized by storing a cached node buffer.
    if (FSplineMode = LsmBezierSpline) or (FSplineMode = LsmNURBSCurve) then
    begin
      SetLength(NodeBuffer, Nodes.Count);
      SetLength(ColorBuffer, Nodes.Count);
      for I := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[I]) do
        begin
          NodeBuffer[I] := AsAffineVector;
          ColorBuffer[I] := Color.Color;
        end;
    end;

    if FSplineMode = LsmBezierSpline then
    begin
      // map evaluator
      GlPushAttrib(GL_EVAL_BIT);
      GlEnable(GL_MAP1_VERTEX_3);
      GlEnable(GL_MAP1_COLOR_4);

      GlMap1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @NodeBuffer[0]);
      GlMap1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @ColorBuffer[0]);
    end;

    // start drawing the line
    if (FSplineMode = LsmNURBSCurve) and (FDivision >= 2) then
    begin
      if (FNURBSOrder > 0) and (FNURBSKnots.Count > 0) then
      begin
        NurbsRenderer := GluNewNurbsRenderer;
        try
          GluNurbsProperty(NurbsRenderer, GLU_SAMPLING_TOLERANCE,
            FNURBSTolerance);
          GluNurbsProperty(NurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
          GluBeginCurve(NurbsRenderer);
          GluNurbsCurve(NurbsRenderer, FNURBSKnots.Count, @FNURBSKnots.List[0],
            3, @NodeBuffer[0], FNURBSOrder, GL_MAP1_VERTEX_3);
          GluEndCurve(NurbsRenderer);
        finally
          GluDeleteNurbsRenderer(NurbsRenderer);
        end;
      end;
    end
    else
    begin
      // lines, cubic splines or bezier
      if FSplineMode = LsmSegments then
        GlBegin(GL_LINES)
      else
        GlBegin(GL_LINE_STRIP);
      if (FDivision < 2) or (FSplineMode in [LsmLines, LsmSegments]) then
      begin
        // standard line(s), draw directly
        if LoUseNodeColorForLines in Options then
        begin
          // node color interpolation
          for I := 0 to Nodes.Count - 1 do
            with TGLLinesNode(Nodes[I]) do
            begin
              GlColor4fv(Color.AsAddress);
              GlVertex3f(X, Y, Z);
            end;
        end
        else
        begin
          // single color
          for I := 0 to Nodes.Count - 1 do
            with Nodes[I] do
              GlVertex3f(X, Y, Z);
        end;
      end
      else if FSplineMode = LsmCubicSpline then
      begin
        // cubic spline
        Spline := Nodes.CreateNewCubicSpline;
        try
          F := 1 / FDivision;
          for I := 0 to (Nodes.Count - 1) * FDivision do
          begin
            Spline.SplineXYZ(I * F, A, B, C);
            if LoUseNodeColorForLines in Options then
            begin
              N := (I div FDivision);
              if N < Nodes.Count - 1 then
                VectorLerp(TGLLinesNode(Nodes[N]).Color.Color,
                  TGLLinesNode(Nodes[N + 1]).Color.Color, (I mod FDivision) * F,
                  VertexColor)
              else
                SetVector(VertexColor, TGLLinesNode(Nodes[Nodes.Count - 1])
                  .Color.Color);
              GlColor4fv(@VertexColor);
            end;
            GlVertex3f(A, B, C);
          end;
        finally
          Spline.Free;
        end;
      end
      else if FSplineMode = LsmBezierSpline then
      begin
        F := 1 / FDivision;
        for I := 0 to FDivision do
          GlEvalCoord1f(I * F);
      end;
      GlEnd;
    end;

    if FSplineMode = LsmBezierSpline then
      GlPopAttrib;
    if Length(NodeBuffer) > 0 then
    begin
      SetLength(NodeBuffer, 0);
      SetLength(ColorBuffer, 0);
    end;

    RestoreLineStyle;

    if FNodesAspect <> LnaInvisible then
    begin
      GlPushAttrib(GL_ENABLE_BIT);
      if not Rci.IgnoreBlendingRequests then
      begin
        GlEnable(GL_BLEND);
        GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      GlDisable(GL_TEXTURE_2D);
      if GL_ARB_texture_cube_map then
        GlDisable(GL_TEXTURE_CUBE_MAP_ARB);
      for I := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[I]) do
          DrawNode(Rci, X, Y, Z, Color);
      GlPopAttrib;
    end;
  end;
end;

// ------------------
// ------------------ TGLCube ------------------
// ------------------

// Create
//
constructor TGLCube.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FCubeSize := XYZVector;
  FParts := [CpTop, CpBottom, CpFront, CpBack, CpLeft, CpRight];
  FNormalDirection := NdOutside;
  ObjectStyle := ObjectStyle + [OsDirectDraw];
end;

// BuildList
//
procedure TGLCube.BuildList(var Rci: TRenderContextInfo);
var
  Hw, Hh, Hd, Nd: TGLFloat;
begin
  if FNormalDirection = NdInside then
    Nd := -1
  else
    Nd := 1;
  Hw := FCubeSize.Coord[0] * 0.5;
  Hh := FCubeSize.Coord[1] * 0.5;
  Hd := FCubeSize.Coord[2] * 0.5;

  GlBegin(GL_QUADS);
  if CpFront in FParts then
  begin
    GlNormal3f(0, 0, Nd);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(Hw, Hh, Hd);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(-Hw * Nd, Hh * Nd, Hd);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(-Hw, -Hh, Hd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(Hw * Nd, -Hh * Nd, Hd);
  end;
  if CpBack in FParts then
  begin
    GlNormal3f(0, 0, -Nd);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(Hw, Hh, -Hd);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(Hw * Nd, -Hh * Nd, -Hd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(-Hw, -Hh, -Hd);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(-Hw * Nd, Hh * Nd, -Hd);
  end;
  if CpLeft in FParts then
  begin
    GlNormal3f(-Nd, 0, 0);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(-Hw, Hh, Hd);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(-Hw, Hh * Nd, -Hd * Nd);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(-Hw, -Hh, -Hd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(-Hw, -Hh * Nd, Hd * Nd);
  end;
  if CpRight in FParts then
  begin
    GlNormal3f(Nd, 0, 0);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(Hw, Hh, Hd);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(Hw, -Hh * Nd, Hd * Nd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(Hw, -Hh, -Hd);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(Hw, Hh * Nd, -Hd * Nd);
  end;
  if CpTop in FParts then
  begin
    GlNormal3f(0, Nd, 0);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(-Hw, Hh, -Hd);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(-Hw * Nd, Hh, Hd * Nd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(Hw, Hh, Hd);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(Hw * Nd, Hh, -Hd * Nd);
  end;
  if CpBottom in FParts then
  begin
    GlNormal3f(0, -Nd, 0);
    XglTexCoord2fv(@NullTexPoint);
    GlVertex3f(-Hw, -Hh, -Hd);
    XglTexCoord2fv(@XTexPoint);
    GlVertex3f(Hw * Nd, -Hh, -Hd * Nd);
    XglTexCoord2fv(@XYTexPoint);
    GlVertex3f(Hw, -Hh, Hd);
    XglTexCoord2fv(@YTexPoint);
    GlVertex3f(-Hw * Nd, -Hh, Hd * Nd);
  end;
  GlEnd;
end;

// GenerateSilhouette
//
function TGLCube.GenerateSilhouette(const SilhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  Hw, Hh, Hd: TGLFloat;
  Connectivity: TConnectivity;
  Sil: TGLSilhouette;
begin
  Connectivity := TConnectivity.Create(True);

  Hw := FCubeSize.Coord[0] * 0.5;
  Hh := FCubeSize.Coord[1] * 0.5;
  Hd := FCubeSize.Coord[2] * 0.5;

  if CpFront in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(Hw, Hh, Hd),
      AffineVectorMake(-Hw, Hh, Hd), AffineVectorMake(-Hw, -Hh, Hd),
      AffineVectorMake(Hw, -Hh, Hd));
  end;
  if CpBack in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(Hw, Hh, -Hd),
      AffineVectorMake(Hw, -Hh, -Hd), AffineVectorMake(-Hw, -Hh, -Hd),
      AffineVectorMake(-Hw, Hh, -Hd));
  end;
  if CpLeft in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-Hw, Hh, Hd),
      AffineVectorMake(-Hw, Hh, -Hd), AffineVectorMake(-Hw, -Hh, -Hd),
      AffineVectorMake(-Hw, -Hh, Hd));
  end;
  if CpRight in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(Hw, Hh, Hd),
      AffineVectorMake(Hw, -Hh, Hd), AffineVectorMake(Hw, -Hh, -Hd),
      AffineVectorMake(Hw, Hh, -Hd));
  end;
  if CpTop in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-Hw, Hh, -Hd),
      AffineVectorMake(-Hw, Hh, Hd), AffineVectorMake(Hw, Hh, Hd),
      AffineVectorMake(Hw, Hh, -Hd));
  end;
  if CpBottom in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-Hw, -Hh, -Hd),
      AffineVectorMake(Hw, -Hh, -Hd), AffineVectorMake(Hw, -Hh, Hd),
      AffineVectorMake(-Hw, -Hh, Hd));
  end;

  Sil := nil;
  Connectivity.CreateSilhouette(SilhouetteParameters, Sil, False);

  Result := Sil;

  Connectivity.Free;
end;

// SetCubeWHD
//
function TGLCube.GetCubeWHD(const Index: Integer): TGLFloat;
begin
  Result := FCubeSize.Coord[index];
end;

// SetCubeWHD
//
procedure TGLCube.SetCubeWHD(Index: Integer; AValue: TGLFloat);
begin
  if AValue <> FCubeSize.Coord[index] then
  begin
    FCubeSize.Coord[index] := AValue;
    StructureChanged;
  end;
end;

// SetParts
//
procedure TGLCube.SetParts(AValue: TCubeParts);
begin
  if AValue <> FParts then
  begin
    FParts := AValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//
procedure TGLCube.SetNormalDirection(AValue: TNormalDirection);
begin
  if AValue <> FNormalDirection then
  begin
    FNormalDirection := AValue;
    StructureChanged;
  end;
end;

// Assign
//
procedure TGLCube.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLCube) then
  begin
    FCubeSize := TGLCube(Source).FCubeSize;
    FParts := TGLCube(Source).FParts;
    FNormalDirection := TGLCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.Coord[0] := FCubeSize.Coord[0] * 0.5;
  Result.Coord[1] := FCubeSize.Coord[1] * 0.5;
  Result.Coord[2] := FCubeSize.Coord[2] * 0.5;
  Result.Coord[3] := 0;
end;

// RayCastIntersect
//
function TGLCube.RayCastIntersect(const RayStart, RayVector: TVector;
  IntersectPoint: PVector = nil; IntersectNormal: PVector = nil): Boolean;
var
  P: array [0 .. 5] of TVector;
  Rv: TVector;
  Rs, R: TVector;
  I: Integer;
  T, E: Single;
  ESize: TAffineVector;
begin
  Rs := AbsoluteToLocal(RayStart);
  SetVector(Rv, VectorNormalize(AbsoluteToLocal(RayVector)));
  E := 0.5 + 0.0001; // Small value for floating point imprecisions
  ESize.Coord[0] := FCubeSize.Coord[0] * E;
  ESize.Coord[1] := FCubeSize.Coord[1] * E;
  ESize.Coord[2] := FCubeSize.Coord[2] * E;
  P[0] := XHmgVector;
  P[1] := YHmgVector;
  P[2] := ZHmgVector;
  SetVector(P[3], -1, 0, 0);
  SetVector(P[4], 0, -1, 0);
  SetVector(P[5], 0, 0, -1);
  for I := 0 to 5 do
  begin
    if VectorDotProduct(P[I], Rv) > 0 then
    begin
      T := -(P[I].Coord[0] * Rs.Coord[0] + P[I].Coord[1] * Rs.Coord[1] +
        P[I].Coord[2] * Rs.Coord[2] + 0.5 * FCubeSize.Coord[I mod 3]) /
        (P[I].Coord[0] * Rv.Coord[0] + P[I].Coord[1] * Rv.Coord[1] +
        P[I].Coord[2] * Rv.Coord[2]);
      MakePoint(R, Rs.Coord[0] + T * Rv.Coord[0], Rs.Coord[1] + T * Rv.Coord[1],
        Rs.Coord[2] + T * Rv.Coord[2]);
      if (Abs(R.Coord[0]) <= ESize.Coord[0]) and
        (Abs(R.Coord[1]) <= ESize.Coord[1]) and
        (Abs(R.Coord[2]) <= ESize.Coord[2]) and
        (VectorDotProduct(VectorSubtract(R, Rs), Rv) > 0) then
      begin
        if Assigned(IntersectPoint) then
          MakePoint(IntersectPoint^, LocalToAbsolute(R));
        if Assigned(IntersectNormal) then
          MakeVector(IntersectNormal^, LocalToAbsolute(VectorNegate(P[I])));
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

// DefineProperties
//
procedure TGLCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize.Coord[0] <> 1) or (FCubeSize.Coord[1] <> 1) or
    (FCubeSize.Coord[2] <> 1));
end;

// ReadData
//
procedure TGLCube.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// WriteData
//
procedure TGLCube.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// ------------------
// ------------------ TGLQuadricObject ------------------
// ------------------

// Create
//
constructor TGLQuadricObject.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := NsSmooth;
  FNormalDirection := NdOutside;
end;

// SetNormals
//
procedure TGLQuadricObject.SetNormals(AValue: TNormalSmoothing);
begin
  if AValue <> FNormals then
  begin
    FNormals := AValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//
procedure TGLQuadricObject.SetNormalDirection(AValue: TNormalDirection);
begin
  if AValue <> FNormalDirection then
  begin
    FNormalDirection := AValue;
    StructureChanged;
  end;
end;

// SetupQuadricParams
//
procedure TGLQuadricObject.SetupQuadricParams(Quadric: PGLUquadricObj);
const
  CNormalSmoothinToEnum: array [NsFlat .. NsNone] of TGLEnum = (GLU_FLAT,
    GLU_SMOOTH, GLU_NONE);
begin
  GluQuadricDrawStyle(Quadric, GLU_FILL);
  GluQuadricNormals(Quadric, CNormalSmoothinToEnum[FNormals]);
  SetNormalQuadricOrientation(Quadric);
  GluQuadricTexture(Quadric, True);
end;

// SetNormalQuadricOrientation
//
procedure TGLQuadricObject.SetNormalQuadricOrientation(Quadric: PGLUquadricObj);
const
  CNormalDirectionToEnum: array [NdInside .. NdOutside] of TGLEnum =
    (GLU_INSIDE, GLU_OUTSIDE);
begin
  GluQuadricOrientation(Quadric, CNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//
procedure TGLQuadricObject.SetInvertedQuadricOrientation
  (Quadric: PGLUquadricObj);
const
  CNormalDirectionToEnum: array [NdInside .. NdOutside] of TGLEnum =
    (GLU_OUTSIDE, GLU_INSIDE);
begin
  GluQuadricOrientation(Quadric, CNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//
procedure TGLQuadricObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLQuadricObject) then
  begin
    FNormals := TGLQuadricObject(Source).FNormals;
    FNormalDirection := TGLQuadricObject(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TGLSphere ------------------
// ------------------

// Create
//
constructor TGLSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

// BuildList
//
procedure TGLSphere.BuildList(var Rci: TRenderContextInfo);
var
  V1, V2, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  UTexCoord, UTexFactor, VTexFactor, VTexCoord0, VTexCoord1: Single;
  I, J: Integer;
  DoReverse: Boolean;
begin
  DoReverse := (FNormalDirection = NdInside);
  GlPushAttrib(GL_POLYGON_BIT);
  if DoReverse then
    Rci.GLStates.InvertGLFrontFace;

  // common settings
  AngTop := DegToRad(FTop);
  AngBottom := DegToRad(FBottom);
  AngStart := DegToRad(FStart);
  AngStop := DegToRad(FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;
  GlPushMatrix;
  GlScalef(Radius, Radius, Radius);

  // top cap
  if (FTop < 90) and (FTopCap in [CtCenter, CtFlat]) then
  begin
    GlBegin(GL_TRIANGLE_FAN);
    SinCos(AngTop, SinP, CosP);
    XglTexCoord2f(0.5, 0.5);
    if DoReverse then
      GlNormal3f(0, -1, 0)
    else
      GlNormal3f(0, 1, 0);
    if FTopCap = CtCenter then
      GlVertex3f(0, 0, 0)
    else
    begin
      GlVertex3f(0, SinP, 0);
      N1 := YVector;
      if DoReverse then
        N1.Coord[1] := -N1.Coord[1];
    end;
    V1.Coord[1] := SinP;
    Theta := AngStart;
    for I := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      V1.Coord[0] := CosP * SinT;
      V1.Coord[2] := CosP * CosT;
      if FTopCap = CtCenter then
      begin
        N1 := VectorPerpendicular(YVector, V1);
        if DoReverse then
          NegateVector(N1);
      end;
      XglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      GlNormal3fv(@N1);
      GlVertex3fv(@V1);
      Theta := Theta + StepH;
    end;
    GlEnd;
  end;

  // main body
  Phi := AngTop;
  Phi2 := Phi - StepV;
  UTexFactor := 1 / FSlices;
  VTexFactor := 1 / FStacks;

  for J := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);
    V1.Coord[1] := SinP;
    V2.Coord[1] := SinP2;
    VTexCoord0 := 1 - J * VTexFactor;
    VTexCoord1 := 1 - (J + 1) * VTexFactor;

    GlBegin(GL_TRIANGLE_STRIP);
    for I := 0 to FSlices do
    begin

      SinCos(Theta, SinT, CosT);
      V1.Coord[0] := CosP * SinT;
      V2.Coord[0] := CosP2 * SinT;
      V1.Coord[2] := CosP * CosT;
      V2.Coord[2] := CosP2 * CosT;

      UTexCoord := I * UTexFactor;
      XglTexCoord2f(UTexCoord, VTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(V1);
        GlNormal3fv(@N1);
      end
      else
        GlNormal3fv(@V1);
      GlVertex3fv(@V1);

      XglTexCoord2f(UTexCoord, VTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        GlNormal3fv(@N1);
      end
      else
        GlNormal3fv(@V2);
      GlVertex3fv(@V2);

      Theta := Theta + StepH;
    end;
    GlEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [CtCenter, CtFlat]) then
  begin
    GlBegin(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);
    XglTexCoord2f(0.5, 0.5);
    if DoReverse then
      GlNormal3f(0, 1, 0)
    else
      GlNormal3f(0, -1, 0);
    if FBottomCap = CtCenter then
      GlVertex3f(0, 0, 0)
    else
    begin
      GlVertex3f(0, SinP, 0);
      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
        N1 := YVector;
    end;
    V1.Coord[1] := SinP;
    Theta := AngStop;
    for I := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      V1.Coord[0] := CosP * SinT;
      V1.Coord[2] := CosP * CosT;
      if FTopCap = CtCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
        if DoReverse then
          NegateVector(N1);
      end;
      XglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      GlNormal3fv(@N1);
      GlVertex3fv(@V1);
      Theta := Theta - StepH;
    end;
    GlEnd;
  end;
  if DoReverse then
    Rci.GLStates.InvertGLFrontFace;
  GlPopMatrix;
  GlPopAttrib;
end;

// RayCastIntersect
//
function TGLSphere.RayCastIntersect(const RayStart, RayVector: TVector;
  IntersectPoint: PVector = nil; IntersectNormal: PVector = nil): Boolean;
var
  I1, I2: TVector;
  LocalStart, LocalVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(LocalStart, AbsoluteToLocal(RayStart));
  SetVector(LocalVector, AbsoluteToLocal(RayVector));
  NormalizeVector(LocalVector);
  if RayCastSphereIntersect(LocalStart, LocalVector, NullHmgVector, Radius, I1,
    I2) > 0 then
  begin
    Result := True;
    if Assigned(IntersectPoint) then
      SetVector(IntersectPoint^, LocalToAbsolute(I1));
    if Assigned(IntersectNormal) then
    begin
      I1.Coord[3] := 0; // vector transform
      SetVector(IntersectNormal^, LocalToAbsolute(I1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//
function TGLSphere.GenerateSilhouette(const SilhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  I, J: Integer;
  S, C, AngleFactor: Single;
  SVec, TVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  SVec := VectorCrossProduct(SilhouetteParameters.SeenFrom, XVector);
  if VectorLength(SVec) < 1E-3 then
    SVec := VectorCrossProduct(SilhouetteParameters.SeenFrom, YVector);
  TVec := VectorCrossProduct(SilhouetteParameters.SeenFrom, SVec);
  NormalizeVector(SVec);
  NormalizeVector(TVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  AngleFactor := (2 * PI) / Segments;
  for I := 0 to Segments - 1 do
  begin
    SinCos(I * AngleFactor, FRadius, S, C);
    Result.Vertices.AddPoint(VectorCombine(SVec, TVec, S, C));
    J := (I + 1) mod Segments;
    Result.Indices.Add(I, J);
    if SilhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, I, J)
  end;
  if SilhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

// SetBottom
//
procedure TGLSphere.SetBottom(AValue: TAngleLimit1);
begin
  if FBottom <> AValue then
  begin
    FBottom := AValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//
procedure TGLSphere.SetBottomCap(AValue: TCapType);
begin
  if FBottomCap <> AValue then
  begin
    FBottomCap := AValue;
    StructureChanged;
  end;
end;

// SetRadius
//
procedure TGLSphere.SetRadius(const AValue: TGLFloat);
begin
  if AValue <> FRadius then
  begin
    FRadius := AValue;
    StructureChanged;
  end;
end;

// SetSlices
//
procedure TGLSphere.SetSlices(AValue: Integer);
begin
  if AValue <> FSlices then
  begin
    if AValue <= 0 then
      FSlices := 1
    else
      FSlices := AValue;
    StructureChanged;
  end;
end;

// SetStacks
//
procedure TGLSphere.SetStacks(AValue: TGLInt);
begin
  if AValue <> FStacks then
  begin
    if AValue <= 0 then
      FStacks := 1
    else
      FStacks := AValue;
    StructureChanged;
  end;
end;

// SetStart
//
procedure TGLSphere.SetStart(AValue: TAngleLimit2);
begin
  if FStart <> AValue then
  begin
    Assert(AValue <= FStop);
    FStart := AValue;
    StructureChanged;
  end;
end;

// SetStop
//
procedure TGLSphere.SetStop(AValue: TAngleLimit2);
begin
  if FStop <> AValue then
  begin
    Assert(AValue >= FStart);
    FStop := AValue;
    StructureChanged;
  end;
end;

// SetTop
//
procedure TGLSphere.SetTop(AValue: TAngleLimit1);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    StructureChanged;
  end;
end;

// SetTopCap
//
procedure TGLSphere.SetTopCap(AValue: TCapType);
begin
  if FTopCap <> AValue then
  begin
    FTopCap := AValue;
    StructureChanged;
  end;
end;

// Assign
//
procedure TGLSphere.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLSphere) then
  begin
    FRadius := TGLSphere(Source).FRadius;
    FSlices := TGLSphere(Source).FSlices;
    FStacks := TGLSphere(Source).FStacks;
    FBottom := TGLSphere(Source).FBottom;
    FTop := TGLSphere(Source).FTop;
    FStart := TGLSphere(Source).FStart;
    FStop := TGLSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.Coord[0] := Abs(FRadius);
  Result.Coord[1] := Result.Coord[0];
  Result.Coord[2] := Result.Coord[0];
  Result.Coord[3] := 0;
end;

// ------------------
// ------------------ TGLPolygonBase ------------------
// ------------------

// Create
//
constructor TGLPolygonBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodes;
  FDivision := 10;
  FSplineMode := LsmLines;
end;

// CreateNodes
//
procedure TGLPolygonBase.CreateNodes;
begin
  FNodes := TGLNodes.Create(Self);
end;

// Destroy
//
destructor TGLPolygonBase.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

// Assign
//
procedure TGLPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TGLPolygonBase then
  begin
    SetNodes(TGLPolygonBase(Source).FNodes);
    FDivision := TGLPolygonBase(Source).FDivision;
    FSplineMode := TGLPolygonBase(Source).FSplineMode;
  end;
  inherited Assign(Source);
end;

// NotifyChange
//
procedure TGLPolygonBase.NotifyChange(Sender: TObject);
begin
  if Sender = Nodes then
    StructureChanged;
  inherited;
end;

// SetDivision
//
procedure TGLPolygonBase.SetDivision(const Value: Integer);
begin
  if Value <> FDivision then
  begin
    if Value < 1 then
      FDivision := 1
    else
      FDivision := Value;
    StructureChanged;
  end;
end;

// SetNodes
//
procedure TGLPolygonBase.SetNodes(const ANodes: TGLNodes);
begin
  FNodes.Assign(ANodes);
  StructureChanged;
end;

// SetSplineMode
//
procedure TGLPolygonBase.SetSplineMode(const Val: TLineSplineMode);
begin
  if FSplineMode <> Val then
  begin
    FSplineMode := Val;
    StructureChanged;
  end;
end;

// AddNode (coords)
//
procedure TGLPolygonBase.AddNode(const Coords: TGLCoordinates);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  if Assigned(Coords) then
    N.AsVector := Coords.AsVector;
  StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLPolygonBase.AddNode(const X, Y, Z: TGLfloat);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//
procedure TGLPolygonBase.AddNode(const Value: TVector);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := Value;
  StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLPolygonBase.AddNode(const Value: TAffineVector);
var
  N: TGLNode;
begin
  N := Nodes.Add;
  N.AsVector := VectorMake(Value);
  StructureChanged;
end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

RegisterClasses([TGLSphere, TGLCube, TGLPlane, TGLSprite, TGLPoints,
  TGLDummyCube, TGLLines]);

end.
