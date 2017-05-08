//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Implementation of basic scene objects plus some management routines.

  All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical
  objects can be found VXS.GeomObjects.


}
unit VXS.Objects;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Math,

  VXS.OpenGLAdapter,
  VXS.XOpenGL,
  VXS.VectorGeometry,
  VXS.VectorTypes,
  VXS.Scene,
  VXS.VectorLists,
  VXS.CrossPlatform,
  VXS.Context,
  VXS.Silhouette,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.Nodes,
  VXS.Coordinates;

type

  TVXVisibilityDeterminationEvent = function(Sender: TObject;
    var rci: TVXRenderContextInfo): Boolean of object;

  PVertexRec = ^TVertexRec;
  TVertexRec = record
    Position: TVector3f;
    Normal: TVector3f;
    Binormal: TVector3f;
    Tangent: TVector3f;
    TexCoord: TVector2f;
  end;

  { A simple cube, invisible at run-time.
    This is a usually non-visible object -except at design-time- used for
    building hierarchies or groups, when some kind of joint or movement
    mechanism needs be described, you can use DummyCubes.
    DummyCube's barycenter is its children's barycenter.
    The DummyCube can optionnally amalgamate all its children into a single
    display list (see Amalgamate property). }
  TVXDummyCube = class(TVXCameraInvariantObject)
  private
    FCubeSize: GLfloat;
    FEdgeColor: TVXColor;
    FVisibleAtRunTime, FAmalgamate: Boolean;
    FGroupList: TVXListHandle;
    FOnVisibilityDetermination: TVXVisibilityDeterminationEvent;
  protected
    procedure SetCubeSize(const val: GLfloat);
    procedure SetEdgeColor(const val: TVXColor);
    procedure SetVisibleAtRunTime(const val: Boolean);
    procedure SetAmalgamate(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure DoRender(var rci: TVXRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    function BarycenterAbsolutePosition: TVector; override;
  published
    property CubeSize: GLfloat read FCubeSize write SetCubeSize;
    property EdgeColor: TVXColor read FEdgeColor write SetEdgeColor;
    { If true the dummycube's edges will be visible at runtime.
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. }
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime default False;
    { Amalgamate the dummy's children in a single OpenVX entity.
      This activates a special rendering mode, which will compile
      the rendering of all of the dummycube's children objects into a
      single display list. This may provide a significant speed up in some
      situations, however, this means that changes to the children will
      be ignored untill you call StructureChanged on the dummy cube.
      Some objects, that have their own display list management, may not
      be compatible with this behaviour. This will also prevents sorting
      and culling to operate as usual.
      In short, this features is best used for static, non-transparent
      geometry, or when the point of view won't change over a large
      number of frames. }
    property Amalgamate: Boolean read FAmalgamate write SetAmalgamate
      default False;
    { Camera Invariance Options.
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. }
    property CamInvarianceMode default cimNone;
    { Event for custom visibility determination.
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. }
    property OnVisibilityDetermination: TVXVisibilityDeterminationEvent
      read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;

  TVXPlaneStyle = (psSingleQuad, psTileTexture);
  TVXPlaneStyles = set of TVXPlaneStyle;

  { A simple plane object.
    Note that a plane is always made of a single quad (two triangles) and the
    tiling is only applied to texture coordinates. }
  TVXPlane = class(TVXSceneObject)
  private
    FXOffset, FYOffset: GLfloat;
    FXScope, FYScope: GLfloat;
    FWidth, FHeight: GLfloat;
    FXTiles, FYTiles: Cardinal;
    FStyle: TVXPlaneStyles;
    FMesh: array of array of TVertexRec;
  protected
    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);
    procedure SetXOffset(const Value: GLfloat);
    procedure SetXScope(const Value: GLfloat);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: GLfloat);
    procedure SetYScope(const Value: GLfloat);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const val: TVXPlaneStyles);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    function GenerateSilhouette(const silhouetteParameters
      : TVXSilhouetteParameters): TVXSilhouette; override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    { Computes the screen coordinates of the smallest rectangle encompassing the plane.
      Returned extents are NOT limited to any physical screen extents. }
    function ScreenRect(aBuffer: TVXSceneBuffer): TVXRect;
    { Computes the signed distance to the point.
      Point coordinates are expected in absolute coordinates. }
    function PointDistance(const aPoint: TVector): Single;
  published
    property Height: GLfloat read FHeight write SetHeight;
    property Width: GLfloat read FWidth write SetWidth;
    property XOffset: GLfloat read FXOffset write SetXOffset;
    property XScope: GLfloat read FXScope write SetXScope stored StoreXScope;
    property XTiles: Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: GLfloat read FYOffset write SetYOffset;
    property YScope: GLfloat read FYScope write SetYScope stored StoreYScope;
    property YTiles: Cardinal read FYTiles write SetYTiles default 1;
    property Style: TVXPlaneStyles read FStyle write SetStyle
      default [psSingleQuad, psTileTexture];
  end;

  { A rectangular area, perspective projected, but always facing the camera.
    A TVXSprite is perspective projected and as such is scaled with distance,
    if you want a 2D sprite that does not get scaled, see TVXHUDSprite. }
  TVXSprite = class(TVXSceneObject)
  private
    FWidth: GLfloat;
    FHeight: GLfloat;
    FRotation: GLfloat;
    FAlphaChannel: Single;
    FMirrorU, FMirrorV: Boolean;
  protected
    procedure SetWidth(const val: GLfloat);
    procedure SetHeight(const val: GLfloat);
    procedure SetRotation(const val: GLfloat);
    procedure SetAlphaChannel(const val: Single);
    function StoreAlphaChannel: Boolean;
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure SetSize(const Width, Height: GLfloat);
    // Set width and height to "size"
    procedure SetSquareSize(const Size: GLfloat);
  published
    { Sprite Width in 3D world units. }
    property Width: GLfloat read FWidth write SetWidth;
    { Sprite Height in 3D world units. }
    property Height: GLfloat read FHeight write SetHeight;
    { This the ON-SCREEN rotation of the sprite. 
      Rotatation=0 is handled faster. }
    property Rotation: GLfloat read FRotation write SetRotation;
    { If different from 1, this value will replace that of Diffuse.Alpha }
    property AlphaChannel: Single read FAlphaChannel write SetAlphaChannel
      stored StoreAlphaChannel;
    { Reverses the texture coordinates in the U and V direction to mirror
      the texture. }
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
  end;

  TVXPointStyle = (psSquare, psRound, psSmooth, psSmoothAdditive,
    psSquareAdditive);

  { Point parameters as in ARB_point_parameters.
    Make sure to read the ARB_point_parameters spec if you want to understand
    what each parameter does. }
  TVXPointParameters = class(TVXUpdateAbleObject)
  private
    FEnabled: Boolean;
    FMinSize, FMaxSize: Single;
    FFadeTresholdSize: Single;
    FDistanceAttenuation: TVXCoordinates;
  protected
    procedure SetEnabled(const val: Boolean);
    procedure SetMinSize(const val: Single);
    procedure SetMaxSize(const val: Single);
    procedure SetFadeTresholdSize(const val: Single);
    procedure SetDistanceAttenuation(const val: TVXCoordinates);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply;
    procedure UnApply;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property MinSize: Single read FMinSize write SetMinSize stored False;
    property MaxSize: Single read FMaxSize write SetMaxSize stored False;
    property FadeTresholdSize: Single read FFadeTresholdSize
      write SetFadeTresholdSize stored False;
    { Components XYZ are for constant, linear and quadratic attenuation. }
    property DistanceAttenuation: TVXCoordinates read FDistanceAttenuation
      write SetDistanceAttenuation;
  end;

  { Renders a set of non-transparent colored points.
    The points positions and their color are defined through the Positions
    and Colors properties. }
  TVXPoints = class(TVXImmaterialSceneObject)
  private
    FPositions: TAffineVectorList;
    FColors: TVectorList;
    FSize: Single;
    FStyle: TVXPointStyle;
    FPointParameters: TVXPointParameters;
    FStatic, FNoZWrite: Boolean;
  protected
    function StoreSize: Boolean;
    procedure SetNoZWrite(const val: Boolean);
    procedure SetStatic(const val: Boolean);
    procedure SetSize(const val: Single);
    procedure SetPositions(const val: TAffineVectorList);
    procedure SetColors(const val: TVectorList);
    procedure SetStyle(const val: TVXPointStyle);
    procedure SetPointParameters(const val: TVXPointParameters);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    { Points positions. If empty, a single point is assumed at (0, 0, 0) }
    property Positions: TAffineVectorList read FPositions write SetPositions;
    { Defines the points colors.
       if empty, point color will be opaque white
       if contains a single color, all points will use that color
       if contains N colors, the first N points (at max) will be rendered
      using the corresponding colors. }
    property Colors: TVectorList read FColors write SetColors;
  published
    { If true points do not write their Z to the depth buffer. }
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite;
    { Tells the component if point coordinates are static. 
      If static, changes to the positions should be notified via an
      explicit StructureChanged call, or may not refresh. 
      Static sets of points may render faster than dynamic ones. }
    property Static: Boolean read FStatic write SetStatic;
    { Point size, all points have a fixed size. }
    property Size: Single read FSize write SetSize stored StoreSize;
    { Points style.  }
    property Style: TVXPointStyle read FStyle write SetStyle default psSquare;
    { Point parameters as of ARB_point_parameters. 
      Allows to vary the size and transparency of points depending
      on their distance to the observer. }
    property PointParameters: TVXPointParameters read FPointParameters
      write SetPointParameters;
  end;

  { Possible aspects for the nodes of a TLine. }
  TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);

  { Available spline modes for a TLine. }
  TLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve,
    lsmSegments, lsmLoop);

  { Specialized Node for use in a TVXLines objects.
    Adds a Color property (TVXColor). }
  TVXLinesNode = class(TVXNode)
  private
    FColor: TVXColor;
  protected
    procedure SetColor(const val: TVXColor);
    procedure OnColorChange(Sender: TObject);
    function StoreColor: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { The node color.
      Can also defined the line color (interpolated between nodes) if
      loUseNodeColorForLines is set (in TVXLines). }
    property Color: TVXColor read FColor write SetColor stored StoreColor;
  end;

  { Specialized collection for Nodes in a TVXLines objects. Stores TVXLinesNode items. }
  TVXLinesNodes = class(TVXNodes)
  public
    constructor Create(AOwner: TComponent); overload;
    procedure NotifyChange; override;
  end;

  { Base class for line objects.
    Introduces line style properties (width, color...). }
  TVXLineBase = class(TVXImmaterialSceneObject)
  private
    FLineColor: TVXColor;
    FLinePattern: GLushort;
    FLineWidth: Single;
    FAntiAliased: Boolean;
  protected
    procedure SetLineColor(const Value: TVXColor);
    procedure SetLinePattern(const Value: GLushort);
    procedure SetLineWidth(const val: Single);
    function StoreLineWidth: Boolean;
    procedure SetAntiAliased(const val: Boolean);
    { Setup OpenVX states according to line style.
      You must call RestoreLineStyle after drawing your lines.
      You may use nested calls with SetupLineStyle/RestoreLineStyle. }
    procedure SetupLineStyle(var rci: TVXRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    { Indicates if OpenVX should smooth line edges.
      Smoothed lines looks better but are poorly implemented in most OpenGL
      drivers and take *lots* of rendering time. }
    property AntiAliased: Boolean read FAntiAliased write SetAntiAliased
      default False;
    { Default color of the lines. }
    property LineColor: TVXColor read FLineColor write SetLineColor;
    { Bitwise line pattern.
      For instance $FFFF (65535) is a white line (stipple disabled), $0000
      is a black line, $CCCC is the stipple used in axes and dummycube, etc. }
    property LinePattern: GLushort read FLinePattern write SetLinePattern
      default $FFFF;
    { Default width of the lines. }
    property LineWidth: Single read FLineWidth write SetLineWidth
      stored StoreLineWidth;
    property Visible;
  end;

  { Class that defines lines via a series of nodes. Base class, does not render anything. }
  TVXNodedLines = class(TVXLineBase)
  private
    FNodes: TVXLinesNodes;
    FNodesAspect: TLineNodesAspect;
    FNodeColor: TVXColor;
    FNodeSize: Single;
    FOldNodeColor: TColorVector;
  protected
    procedure SetNodesAspect(const Value: TLineNodesAspect);
    procedure SetNodeColor(const Value: TVXColor);
    procedure OnNodeColorChanged(Sender: TObject);
    procedure SetNodes(const aNodes: TVXLinesNodes);
    procedure SetNodeSize(const val: Single);
    function StoreNodeSize: Boolean;
    procedure DrawNode(var rci: TVXRenderContextInfo; X, Y, Z: Single;
      Color: TVXColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure AddNode(const coords: TVXCoordinates); overload;
    procedure AddNode(const X, Y, Z: GLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    { Default color for nodes. lnaInvisible and lnaAxes ignore this setting. }
    property NodeColor: TVXColor read FNodeColor write SetNodeColor;
    { The nodes list.  }
    property Nodes: TVXLinesNodes read FNodes write SetNodes;
    { Default aspect of line nodes.
      May help you materialize nodes, segments and control points. }
    property NodesAspect: TLineNodesAspect read FNodesAspect
      write SetNodesAspect default lnaAxes;
    { Size for the various node aspects. }
    property NodeSize: Single read FNodeSize write SetNodeSize
      stored StoreNodeSize;
  end;

  TLinesOption = (loUseNodeColorForLines, loColorLogicXor);
  TLinesOptions = set of TLinesOption;

  { Set of 3D line segments.
    You define a 3D Line by adding its nodes in the "Nodes" property. The line
    may be rendered as a set of segment or as a curve (nodes then act as spline
    control points).
    Alternatively, you can also use it to render a set of spacial nodes (points
    in space), just make the lines transparent and the nodes visible by picking
    the node aspect that suits you. }
  TVXLines = class(TVXNodedLines)
  private
    FDivision: Integer;
    FSplineMode: TLineSplineMode;
    FOptions: TLinesOptions;
    FNURBSOrder: Integer;
    FNURBSTolerance: Single;
    FNURBSKnots: TSingleList;
  protected
    procedure SetSplineMode(const val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetOptions(const val: TLinesOptions);
    procedure SetNURBSOrder(const val: Integer);
    procedure SetNURBSTolerance(const val: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NURBSKnots: TSingleList read FNURBSKnots;
    property NURBSOrder: Integer read FNURBSOrder write SetNURBSOrder;
    property NURBSTolerance: Single read FNURBSTolerance write SetNURBSTolerance;
  published
    { Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. }
    property Division: Integer read FDivision write SetDivision default 10;
    { Default spline drawing mode.  }
    property SplineMode: TLineSplineMode read FSplineMode write SetSplineMode
      default lsmLines;
    { Rendering options for the line.
       loUseNodeColorForLines: if set lines will be drawn using node
      colors (and color interpolation between nodes), if not, LineColor
      will be used (single color).
      loColorLogicXor: enable logic operation for color of XOR type.  }
    property Options: TLinesOptions read FOptions write SetOptions;
  end;

  TCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TCubeParts = set of TCubePart;

  { A simple cube object.
    This cube use the same material for each of its faces, ie. all faces look
    the same. If you want a multi-material cube, use a mesh in conjunction
    with a TVXFreeForm and a material library. }
  TVXCube = class(TVXSceneObject)
  private
    FCubeSize: TAffineVector;
    FParts: TCubeParts;
    FNormalDirection: TNormalDirection;
    function GetCubeWHD(const Index: Integer): GLfloat;
    procedure SetCubeWHD(Index: Integer; AValue: GLfloat);
    procedure SetParts(aValue: TCubeParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    function GenerateSilhouette(const silhouetteParameters
      : TVXSilhouetteParameters): TVXSilhouette; override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
  published
    property CubeWidth: GLfloat index 0 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeHeight: GLfloat index 1 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeDepth: GLfloat index 2 read GetCubeWHD write SetCubeWHD
      stored False;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Parts: TCubeParts read FParts write SetParts
      default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  { Determines how and if normals are smoothed.
    - nsFlat : facetted look
    - nsSmooth : smooth look
    - nsNone : unlighted rendering, usefull for decla texturing }
  TNormalSmoothing = (nsFlat, nsSmooth, nsNone);

  { Base class for quadric objects.
    Introduces some basic Quadric interaction functions (the actual quadric
    math is part of the GLU library). }
  TVXQuadricObject = class(TVXSceneObject)
  private
    FNormals: TNormalSmoothing;
    FNormalDirection: TNormalDirection;
  protected
    procedure SetNormals(aValue: TNormalSmoothing);
    procedure SetNormalDirection(aValue: TNormalDirection);
    procedure SetupQuadricParams(quadric: GLUquadricObj);
    procedure SetNormalQuadricOrientation(quadric: GLUquadricObj);
    procedure SetInvertedQuadricOrientation(quadric: GLUquadricObj);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
  end;

  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TCapType = (ctNone, ctCenter, ctFlat);

  { A sphere object.
    The sphere can have to and bottom caps, as well as being just a slice
    of sphere. }
  TVXSphere = class(TVXQuadricObject)
  private
    FRadius: GLfloat;
    FSlices, FStacks: GLint;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TCapType);
    procedure SetRadius(const aValue: GLfloat);
    procedure SetSlices(aValue: GLint);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: GLint);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TVXSilhouetteParameters): TVXSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: GLfloat read FRadius write SetRadius;
    property Slices: GLint read FSlices write SetSlices default 16;
    property Stacks: GLint read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
  end;

  { Base class for objects based on a polygon. }
  TVXPolygonBase = class(TVXSceneObject)
  private
    FDivision: Integer;
    FSplineMode: TLineSplineMode;
  protected
    FNodes: TVXNodes;
    procedure CreateNodes; dynamic;
    procedure SetSplineMode(const val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const aNodes: TVXNodes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure AddNode(const coords: TVXCoordinates); overload;
    procedure AddNode(const X, Y, Z: GLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    { The nodes list.  }
    property Nodes: TVXNodes read FNodes write SetNodes;
    { Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. }
    property Division: Integer read FDivision write SetDivision default 10;
    { Default spline drawing mode.
      This mode is used only for the curve, not for the rotation path. }
    property SplineMode: TLineSplineMode read FSplineMode write SetSplineMode
      default lsmLines;
  end;

  { A Superellipsoid object.
    The Superellipsoid can have top and bottom caps,
    as well as being just a slice of Superellipsoid. }
  TVXSuperellipsoid = class(TVXQuadricObject)
  private
    FRadius, FxyCurve, FzCurve: GLfloat;
    FSlices, FStacks: GLint;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TCapType);
    procedure SetRadius(const aValue: GLfloat);
    procedure SetxyCurve(const aValue: GLfloat);
    procedure SetzCurve(const aValue: GLfloat);
    procedure SetSlices(aValue: GLint);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: GLint);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TVXSilhouetteParameters): TVXSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: GLfloat read FRadius write SetRadius;
    property xyCurve: GLfloat read FxyCurve write SetxyCurve;
    property zCurve: GLfloat read FzCurve write SetzCurve;
    property Slices: GLint read FSlices write SetSlices default 16;
    property Stacks: GLint read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
  end;

{ Issues OpenVX for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildList(var rci: TVXRenderContextInfo; Size: GLfloat;
  Stipple: Boolean; const Color: TColorVector);
{ Issues OpenVX for a unit-size dodecahedron. }
procedure DodecahedronBuildList;
{ Issues OpenVX for a unit-size icosahedron. }
procedure IcosahedronBuildList;
{ Issues OpenVX for a unit-size octahedron. }
procedure OctahedronBuildList;
{ Issues OpenVX for a unit-size tetrahedron. }
procedure TetrahedronBuildList;

var
  TangentAttributeName: AnsiString = 'Tangent';
  BinormalAttributeName: AnsiString = 'Binormal';

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

uses
  VXS.Spline,
  VXS.State;

const
  cDefaultPointSize: Single = 1.0;

procedure CubeWireframeBuildList(var rci: TVXRenderContextInfo; Size: GLfloat;
  Stipple: Boolean; const Color: TColorVector);
var
  mi, ma: Single;
begin
{$IFDEF VKS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(22, 'CubeWireframeBuildList');
{$ENDIF}
  rci.VKStates.Disable(stLighting);
  rci.VKStates.Enable(stLineSmooth);
  if stipple then
  begin
    rci.VKStates.Enable(stLineStipple);
    rci.VKStates.Enable(stBlend);
    rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    rci.VKStates.LineStippleFactor := 1;
    rci.VKStates.LineStipplePattern := $CCCC;
  end;
  rci.VKStates.LineWidth := 1;
  ma := 0.5 * Size;
  mi := -ma;

  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  // front face
  glVertex3f(ma, mi, mi);
  glVertex3f(ma, ma, mi);
  glVertex3f(ma, ma, ma);
  glVertex3f(ma, mi, ma);
  glVertex3f(ma, mi, mi);
  // partial up back face
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, mi, ma);
  glVertex3f(mi, ma, ma);
  glVertex3f(mi, ma, mi);
  // right side low
  glVertex3f(ma, ma, mi);
  glEnd;
  glBegin(GL_LINES);
  // right high
  glVertex3f(ma, ma, ma);
  glVertex3f(mi, ma, ma);
  // back low
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, ma, mi);
  // left high
  glVertex3f(ma, mi, ma);
  glVertex3f(mi, mi, ma);
  glEnd;
end;

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
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 11 do
  begin
    faceIndices := @polygons[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

//    glBegin(GL_TRIANGLE_FAN);
//    for j := 0 to 4 do
//      glVertex3fv(@vertices[faceIndices^[j]]);
//    glEnd;

    glBegin(GL_TRIANGLES);

    for j := 1 to 3 do
    begin
      glVertex3fv(@vertices[faceIndices^[0]]);
      glVertex3fv(@vertices[faceIndices^[j]]);
      glVertex3fv(@vertices[faceIndices^[j+1]]);
    end;
    glEnd;
  end;
end;

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
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 19 do
  begin
    faceIndices := @triangles[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

    glBegin(GL_TRIANGLES);
    for j := 0 to 2 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

procedure OctahedronBuildList;
const
  Vertices: packed array [0 .. 5] of TAffineVector =
      ((X: 1.0; Y: 0.0; Z: 0.0),
       (X: -1.0; Y: 0.0; Z: 0.0),
       (X: 0.0; Y: 1.0; Z: 0.0),
       (X: 0.0; Y: -1.0; Z: 0.0),
       (X: 0.0; Y: 0.0; Z: 1.0),
       (X: 0.0; Y: 0.0; Z: -1.0));

  Triangles: packed array [0 .. 7] of packed array [0 .. 2]
    of Byte = ((0, 4, 2), (1, 2, 4), (0, 3, 4), (1, 4, 3),
               (0, 2, 5), (1, 5, 2), (0, 5, 3), (1, 3, 5));

var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 7 do
  begin
    faceIndices := @triangles[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

    glBegin(GL_TRIANGLES);
    for j := 0 to 2 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

procedure TetrahedronBuildList;
const
  TetT = 1.73205080756887729;
const
  Vertices: packed array [0 .. 3] of TAffineVector =
{
       ((X: TetT;  Y: TetT;  Z: TetT),
        (X: TetT;  Y: -TetT; Z: -TetT),
        (X: -TetT; Y: TetT;  Z: -TetT),
        (X: -TetT; Y: -TetT; Z: TetT));
}
       ((X: 1.0;  Y: 1.0;  Z: 1.0),
        (X: 1.0;  Y: -1.0; Z: -1.0),
        (X: -1.0; Y: 1.0;  Z: -1.0),
        (X: -1.0; Y: -1.0; Z: 1.0));

  Triangles: packed array [0 .. 3] of packed array [0 .. 2]
    of Byte = ((0, 1, 3), (2, 1, 0), (3, 2, 0), (1, 2, 3));

var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 3 do
  begin
    faceIndices := @triangles[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

    glBegin(GL_TRIANGLES);
    for j := 0 to 2 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

// ------------------
// ------------------ TVXDummyCube ------------------
// ------------------

constructor TVXDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FCubeSize := 1;
  FEdgeColor := TVXColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
  FGroupList := TVXListHandle.Create;
  CamInvarianceMode := cimNone;
end;

destructor TVXDummyCube.Destroy;
begin
  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;

procedure TVXDummyCube.Assign(Source: TPersistent);
begin
  if Source is TVXDummyCube then
  begin
    FCubeSize := TVXDummyCube(Source).FCubeSize;
    FEdgeColor.Color := TVXDummyCube(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TVXDummyCube(Source).FVisibleAtRunTime;
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

function TVXDummyCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FCubeSize);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

function TVXDummyCube.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

procedure TVXDummyCube.BuildList(var rci: TVXRenderContextInfo);
begin
  if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
    CubeWireframeBuildList(rci, FCubeSize, True, EdgeColor.Color);
end;

procedure TVXDummyCube.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  if Assigned(FOnVisibilityDetermination) then
    if not FOnVisibilityDetermination(Self, rci) then
      Exit;
  if FAmalgamate and (not rci.amalgamating) then
  begin
    if FGroupList.Handle = 0 then
    begin
      FGroupList.AllocateHandle;
      Assert(FGroupList.Handle <> 0, 'Handle=0 for ' + ClassName);
      rci.VKStates.NewList(FGroupList.Handle, GL_COMPILE);
      rci.amalgamating := True;
      try
        inherited;
      finally
        rci.amalgamating := False;
        rci.VKStates.EndList;
      end;
    end;
    rci.VKStates.CallList(FGroupList.Handle);
  end
  else
  begin
    // proceed as usual
    inherited;
  end;
end;

procedure TVXDummyCube.StructureChanged;
begin
  if FAmalgamate then
    FGroupList.DestroyHandle;
  inherited;
end;

function TVXDummyCube.BarycenterAbsolutePosition: TVector;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Children[0].BarycenterAbsolutePosition;
    for i := 1 to Count - 1 do
      Result := VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
    ScaleVector(Result, 1 / Count);
  end
  else
    Result := AbsolutePosition;
end;

procedure TVXDummyCube.SetCubeSize(const val: GLfloat);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

procedure TVXDummyCube.SetEdgeColor(const val: TVXColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

procedure TVXDummyCube.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

procedure TVXDummyCube.SetAmalgamate(const val: Boolean);
begin
  if val <> FAmalgamate then
  begin
    FAmalgamate := val;
    if not val then
      FGroupList.DestroyHandle;
    inherited StructureChanged;
  end;
end;

// ------------------
// ------------------ TVXPlane ------------------
// ------------------

constructor TVXPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FStyle := [psSingleQuad, psTileTexture];
end;

procedure TVXPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXPlane) then
  begin
    FWidth := TVXPlane(Source).FWidth;
    FHeight := TVXPlane(Source).FHeight;
    FXOffset := TVXPlane(Source).FXOffset;
    FXScope := TVXPlane(Source).FXScope;
    FXTiles := TVXPlane(Source).FXTiles;
    FYOffset := TVXPlane(Source).FYOffset;
    FYScope := TVXPlane(Source).FYScope;
    FYTiles := TVXPlane(Source).FYTiles;
    FStyle := TVXPlane(Source).FStyle;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

function TVXPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  Result.Z := 0;
end;

function TVXPlane.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  locRayStart, locRayVector, ip: TVector;
  t: Single;
begin
  locRayStart := AbsoluteToLocal(rayStart);
  locRayVector := AbsoluteToLocal(rayVector);
  if locRayStart.Z >= 0 then
  begin
    // ray start over plane
    if locRayVector.Z < 0 then
    begin
      t := locRayStart.Z / locRayVector.Z;
      ip.X := locRayStart.X - t * locRayVector.X;
      ip.Y := locRayStart.Y - t * locRayVector.Y;
      if (Abs(ip.X) <= 0.5 * Width) and (Abs(ip.Y) <= 0.5 * Height) then
      begin
        Result := True;
        if Assigned(intersectNormal) then
          intersectNormal^ := AbsoluteDirection;
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
    if locRayVector.Z > 0 then
    begin
      t := locRayStart.Z / locRayVector.Z;
      ip.X := locRayStart.X - t * locRayVector.X;
      ip.Y := locRayStart.Y - t * locRayVector.Y;
      if (Abs(ip.X) <= 0.5 * Width) and (Abs(ip.Y) <= 0.5 * Height) then
      begin
        Result := True;
        if Assigned(intersectNormal) then
          intersectNormal^ := VectorNegate(AbsoluteDirection);
      end
      else
        Result := False;
    end
    else
      Result := False;
  end;
  if Result and Assigned(intersectPoint) then
  begin
    ip.Z := 0;
    ip.W := 1;
    intersectPoint^ := LocalToAbsolute(ip);
  end;
end;

function TVXPlane.GenerateSilhouette(const silhouetteParameters
  : TVXSilhouetteParameters): TVXSilhouette;
var
  hw, hh: Single;
begin
  Result := TVXSilhouette.Create;

  hw := FWidth * 0.5;
  hh := FHeight * 0.5;

  with Result.vertices do
  begin
    AddPoint(hw, hh);
    AddPoint(hw, -hh);
    AddPoint(-hw, -hh);
    AddPoint(-hw, hh);
  end;

  with Result.Indices do
  begin
    Add(0, 1);
    Add(1, 2);
    Add(2, 3);
    Add(3, 0);
  end;

  if silhouetteParameters.CappingRequired then
    with Result.CapIndices do
    begin
      Add(0, 1, 2);
      Add(2, 3, 0);
    end;
end;

procedure TVXPlane.BuildList(var rci: TVXRenderContextInfo);

  procedure EmitVertex(ptr: PVertexRec); {$IFDEF VKS_INLINE}inline;{$ENDIF}
  begin
    glTexCoord2fv(@ptr^.TexCoord);
    glVertex3fv(@ptr^.Position);
  end;

var
  hw, hh, posXFact, posYFact, pX, pY1: GLfloat;
  tx0, tx1, ty0, ty1, texSFact, texTFact: GLfloat;
  texS, texT1: GLfloat;
  X, Y: Integer;
  TanLoc, BinLoc: Integer;
  pVertex: PVertexRec;
begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;

  begin
    glNormal3fv(@ZVector);
    if GL_ARB_shader_objects and (rci.VKStates.CurrentProgram > 0) then
    begin
      TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram, PGLChar(TangentAttributeName));
      BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram, PGLChar(BinormalAttributeName));
      if TanLoc > -1 then
        glVertexAttrib3fv(TanLoc, @XVector);
      if BinLoc > -1 then
        glVertexAttrib3fv(BinLoc, @YVector);
    end;
  end;
  // determine tex coords extents
  if psTileTexture in FStyle then
  begin
    tx0 := FXOffset;
    tx1 := FXTiles * FXScope + FXOffset;
    ty0 := FYOffset;
    ty1 := FYTiles * FYScope + FYOffset;
  end
  else
  begin
    tx0 := 0;
    ty0 := tx0;
    tx1 := FXScope;
    ty1 := FYScope;
  end;

  if psSingleQuad in FStyle then
  begin
    // single quad plane
    glBegin(GL_TRIANGLES);
    glTexCoord2f(tx1, ty1);
    glVertex2f(hw, hh);
    glTexCoord2f(tx0, ty1);
    glVertex2f(-hw, hh);
    glTexCoord2f(tx0, ty0);
    glVertex2f(-hw, -hh);

    glVertex2f(-hw, -hh);
    glTexCoord2f(tx1, ty0);
    glVertex2f(hw, -hh);
    glTexCoord2f(tx1, ty1);
    glVertex2f(hw, hh);
    glEnd;
    exit;
  end
  else
  begin
    // multi-quad plane (actually built from tri-strips)
    texSFact := (tx1 - tx0) / FXTiles;
    texTFact := (ty1 - ty0) / FYTiles;
    posXFact := FWidth / FXTiles;
    posYFact := FHeight / FYTiles;
    if FMesh = nil then
    begin
      SetLength(FMesh, FYTiles+1, FXTiles+1);
      for Y := 0 to FYTiles do
      begin
        texT1 := Y * texTFact;
        pY1 := Y * posYFact - hh;
        for X := 0 to FXTiles do
        begin
          texS := X * texSFact;
          pX := X * posXFact - hw;
          FMesh[Y][X].Position := Vector3fMake(pX, pY1, 0.0);
          FMesh[Y][X].TexCoord := Vector2fMake(texS, texT1);
        end;
      end;
    end;
  end;

  begin
    glBegin(GL_TRIANGLES);
    for Y := 0 to FYTiles-1 do
    begin
      for X := 0 to FXTiles-1 do
      begin
        pVertex := @FMesh[Y][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y][X+1];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X+1];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y][X+1];
        EmitVertex(pVertex);
      end;
    end;
    glEnd;
  end;
end;

procedure TVXPlane.SetWidth(const aValue: Single);
begin
  if aValue <> FWidth then
  begin
    FWidth := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TVXPlane.ScreenRect(aBuffer: TVXSceneBuffer): TVXRect;
var
  v: array [0 .. 3] of TVector;
  buf: TVXSceneBuffer;
  hw, hh: GLfloat;
begin
  buf := aBuffer;
  if Assigned(buf) then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    v[0] := LocalToAbsolute(PointMake(-hw, -hh, 0));
    v[1] := LocalToAbsolute(PointMake(hw, -hh, 0));
    v[2] := LocalToAbsolute(PointMake(hw, hh, 0));
    v[3] := LocalToAbsolute(PointMake(-hw, hh, 0));
    buf.WorldToScreen(@v[0], 4);
    Result.Left := Round(MinFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Right := Round(MaxFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Top := Round(MinFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
    Result.Bottom := Round(MaxFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
  end
  else
    FillChar(Result, SizeOf(TVXRect), 0);
end;

function TVXPlane.PointDistance(const aPoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
    AbsoluteDirection);
end;

procedure TVXPlane.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVXPlane.SetXOffset(const Value: GLfloat);
begin
  if Value <> FXOffset then
  begin
    FXOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVXPlane.SetXScope(const Value: GLfloat);
begin
  if Value <> FXScope then
  begin
    FXScope := Value;
    if FXScope > 1 then
      FXScope := 1;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TVXPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

procedure TVXPlane.SetXTiles(const Value: Cardinal);
begin
  if Value <> FXTiles then
  begin
    FXTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVXPlane.SetYOffset(const Value: GLfloat);
begin
  if Value <> FYOffset then
  begin
    FYOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVXPlane.SetYScope(const Value: GLfloat);
begin
  if Value <> FYScope then
  begin
    FYScope := Value;
    if FYScope > 1 then
      FYScope := 1;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TVXPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

procedure TVXPlane.SetYTiles(const Value: Cardinal);
begin
  if Value <> FYTiles then
  begin
    FYTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVXPlane.SetStyle(const val: TVXPlaneStyles);
begin
  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TVXSprite ------------------
// ------------------

constructor TVXSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FAlphaChannel := 1;
  FWidth := 1;
  FHeight := 1;
end;

procedure TVXSprite.Assign(Source: TPersistent);
begin
  if Source is TVXSprite then
  begin
    FWidth := TVXSprite(Source).FWidth;
    FHeight := TVXSprite(Source).FHeight;
    FRotation := TVXSprite(Source).FRotation;
    FAlphaChannel := TVXSprite(Source).FAlphaChannel;
  end;
  inherited Assign(Source);
end;

function TVXSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  // Sprites turn with the camera and can be considered to have the same depth
  // as width
  Result.Z := 0.5 * Abs(FWidth);
end;

procedure TVXSprite.BuildList(var rci: TVXRenderContextInfo);
var
  vx, vy: TAffineVector;
  w, h: Single;
  mat: TMatrix;
  u0, v0, u1, v1: Integer;
begin
  if FAlphaChannel <> 1 then
    rci.VKStates.SetMaterialAlphaChannel(GL_FRONT, FAlphaChannel);

  mat := rci.PipelineTransformation.ModelViewMatrix;
  // extraction of the "vecteurs directeurs de la matrice"
  // (dunno how they are named in english)
  w := FWidth * 0.5;
  h := FHeight * 0.5;
  vx.X := mat.X.X;
  vy.X := mat.X.Y;
  vx.Y := mat.Y.X;
  vy.Y := mat.Y.Y;
  vx.Z := mat.Z.X;
  vy.Z := mat.Z.Y;
  ScaleVector(vx, w / VectorLength(vx));
  ScaleVector(vy, h / VectorLength(vy));
  if FMirrorU then
  begin
    u0 := 1;
    u1 := 0;
  end
  else
  begin
    u0 := 0;
    u1 := 1;
  end;
  if FMirrorV then
  begin
    v0 := 1;
    v1 := 0;
  end
  else
  begin
    v0 := 0;
    v1 := 1;
  end;

  if FRotation <> 0 then
  begin
    glPushMatrix;
    glRotatef(FRotation, mat.X.Z, mat.Y.Z, mat.Z.Z);
  end;
  glBegin(GL_QUADS);
  glTexCoord2f(u1, v1);
  glVertex3f(vx.X + vy.X, vx.Y + vy.Y, vx.Z + vy.Z);
  glTexCoord2f(u0, v1);
  glVertex3f(-vx.X + vy.X, -vx.Y + vy.Y, -vx.Z + vy.Z);
  glTexCoord2f(u0, v0);
  glVertex3f(-vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
  glTexCoord2f(u1, v0);
  glVertex3f(vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
  glEnd;
  if FRotation <> 0 then
    glPopMatrix;
end;

// SetWidth
//

procedure TVXSprite.SetWidth(const val: GLfloat);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TVXSprite.SetHeight(const val: GLfloat);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//

procedure TVXSprite.SetRotation(const val: GLfloat);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

// SetAlphaChannel
//

procedure TVXSprite.SetAlphaChannel(const val: Single);
begin
  if val <> FAlphaChannel then
  begin
    if val < 0 then
      FAlphaChannel := 0
    else if val > 1 then
      FAlphaChannel := 1
    else
      FAlphaChannel := val;
    NotifyChange(Self);
  end;
end;

// StoreAlphaChannel
//

function TVXSprite.StoreAlphaChannel: Boolean;
begin
  Result := (FAlphaChannel <> 1);
end;

// SetMirrorU
//

procedure TVXSprite.SetMirrorU(const val: Boolean);
begin
  FMirrorU := val;
  NotifyChange(Self);
end;

// SetMirrorV
//

procedure TVXSprite.SetMirrorV(const val: Boolean);
begin
  FMirrorV := val;
  NotifyChange(Self);
end;

// SetSize
//

procedure TVXSprite.SetSize(const Width, Height: GLfloat);
begin
  FWidth := Width;
  FHeight := Height;
  NotifyChange(Self);
end;

// SetSquareSize
//

procedure TVXSprite.SetSquareSize(const Size: GLfloat);
begin
  FWidth := Size;
  FHeight := Size;
  NotifyChange(Self);
end;

// ------------------
// ------------------ TVXPointParameters ------------------
// ------------------

// Create
//

constructor TVXPointParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMinSize := 0;
  FMaxSize := 128;
  FFadeTresholdSize := 1;
  FDistanceAttenuation := TVXCoordinates.CreateInitialized(Self, XHmgVector,
    csVector);
end;

// Destroy
//

destructor TVXPointParameters.Destroy;
begin
  FDistanceAttenuation.Free;
  inherited;
end;

// Assign
//

procedure TVXPointParameters.Assign(Source: TPersistent);
begin
  if Source is TVXPointParameters then
  begin
    FMinSize := TVXPointParameters(Source).FMinSize;
    FMaxSize := TVXPointParameters(Source).FMaxSize;
    FFadeTresholdSize := TVXPointParameters(Source).FFadeTresholdSize;
    FDistanceAttenuation.Assign(TVXPointParameters(Source).DistanceAttenuation);
  end;
end;

// DefineProperties
//

procedure TVXPointParameters.DefineProperties(Filer: TFiler);
var
  defaultParams: Boolean;
begin
  inherited;
  defaultParams := (FMaxSize = 128) and (FMinSize = 0) and
    (FFadeTresholdSize = 1);
  Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
    not defaultParams);
end;

// ReadData
//

procedure TVXPointParameters.ReadData(Stream: TStream);
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

procedure TVXPointParameters.WriteData(Stream: TStream);
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

procedure TVXPointParameters.Apply;
begin
  if Enabled and GL_ARB_point_parameters then
  begin
    glPointParameterf(GL_POINT_SIZE_MIN_ARB, FMinSize);
    glPointParameterf(GL_POINT_SIZE_MAX_ARB, FMaxSize);
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
    glPointParameterfv(GL_DISTANCE_ATTENUATION_EXT,
      FDistanceAttenuation.AsAddress);
  end;
end;

// UnApply
//

procedure TVXPointParameters.UnApply;
begin
  if Enabled and GL_ARB_point_parameters then
  begin
    glPointParameterf(GL_POINT_SIZE_MIN_ARB, 0);
    glPointParameterf(GL_POINT_SIZE_MAX_ARB, 128);
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
    glPointParameterfv(GL_DISTANCE_ATTENUATION_EXT, @XVector);
  end;
end;

// SetEnabled
//

procedure TVXPointParameters.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

// SetMinSize
//

procedure TVXPointParameters.SetMinSize(const val: Single);
begin
  if val <> FMinSize then
  begin
    if val < 0 then
      FMinSize := 0
    else
      FMinSize := val;
    NotifyChange(Self);
  end;
end;

// SetMaxSize
//

procedure TVXPointParameters.SetMaxSize(const val: Single);
begin
  if val <> FMaxSize then
  begin
    if val < 0 then
      FMaxSize := 0
    else
      FMaxSize := val;
    NotifyChange(Self);
  end;
end;

// SetFadeTresholdSize
//

procedure TVXPointParameters.SetFadeTresholdSize(const val: Single);
begin
  if val <> FFadeTresholdSize then
  begin
    if val < 0 then
      FFadeTresholdSize := 0
    else
      FFadeTresholdSize := val;
    NotifyChange(Self);
  end;
end;

// SetDistanceAttenuation
//

procedure TVXPointParameters.SetDistanceAttenuation(const val: TVXCoordinates);
begin
  FDistanceAttenuation.Assign(val);
end;

// ------------------
// ------------------ TVXPoints ------------------
// ------------------

// Create
//

constructor TVXPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FStyle := psSquare;
  FSize := cDefaultPointSize;
  FPositions := TAffineVectorList.Create;
  FPositions.Add(NullVector);
  FColors := TVectorList.Create;
  FPointParameters := TVXPointParameters.Create(Self);
end;

// Destroy
//

destructor TVXPoints.Destroy;
begin
  FPointParameters.Free;
  FColors.Free;
  FPositions.Free;
  inherited;
end;

// Assign
//

procedure TVXPoints.Assign(Source: TPersistent);
begin
  if Source is TVXPoints then
  begin
    FSize := TVXPoints(Source).FSize;
    FStyle := TVXPoints(Source).FStyle;
    FPositions.Assign(TVXPoints(Source).FPositions);
    FColors.Assign(TVXPoints(Source).FColors);
    StructureChanged
  end;
  inherited Assign(Source);
end;

// BuildList
//

procedure TVXPoints.BuildList(var rci: TVXRenderContextInfo);
var
  n: Integer;
  v: TVector;
begin
  n := FPositions.Count;
  if n = 0 then
    Exit;

  case FColors.Count of
    0:
      glColor4f(1, 1, 1, 1);
    1:
      glColor4fv(PGLFloat(FColors.List));
  else
    if FColors.Count < n then
      n := FColors.Count;
    glColorPointer(4, GL_FLOAT, 0, FColors.List);
    glEnableClientState(GL_COLOR_ARRAY);
  end;
  if FColors.Count < 2 then
    glDisableClientState(GL_COLOR_ARRAY);

  rci.VKStates.Disable(stLighting);
  if n = 0 then
  begin
    v := NullHmgPoint;
    glVertexPointer(3, GL_FLOAT, 0, @v);
    n := 1;
  end
  else
    glVertexPointer(3, GL_FLOAT, 0, FPositions.List);
  glEnableClientState(GL_VERTEX_ARRAY);

  if NoZWrite then
    rci.VKStates.DepthWriteMask := GLboolean(False);
  rci.VKStates.PointSize := FSize;
  PointParameters.Apply;
  if GL_EXT_compiled_vertex_array and (n > 64) then
    glLockArraysEXT(0, n);
  case FStyle of
    psSquare:
      begin
        // square point (simplest method, fastest)
        rci.VKStates.Disable(stBlend);
      end;
    psRound:
      begin
        rci.VKStates.Enable(stPointSmooth);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetAlphaFunction(cfGreater, 0.5);
        rci.VKStates.Disable(stBlend);
      end;
    psSmooth:
      begin
        rci.VKStates.Enable(stPointSmooth);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetAlphaFunction(cfNotEqual, 0.0);
        rci.VKStates.Enable(stBlend);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
    psSmoothAdditive:
      begin
        rci.VKStates.Enable(stPointSmooth);
        rci.VKStates.Enable(stAlphaTest);
        rci.VKStates.SetAlphaFunction(cfNotEqual, 0.0);
        rci.VKStates.Enable(stBlend);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
    psSquareAdditive:
      begin
        rci.VKStates.Enable(stBlend);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
  else
    Assert(False);
  end;
  glDrawArrays(GL_POINTS, 0, n);
  if GL_EXT_compiled_vertex_array and (n > 64) then
    glUnlockArraysEXT;
  PointParameters.UnApply;
  glDisableClientState(GL_VERTEX_ARRAY);
  if FColors.Count > 1 then
    glDisableClientState(GL_COLOR_ARRAY);
end;

// StoreSize
//

function TVXPoints.StoreSize: Boolean;
begin
  Result := (FSize <> cDefaultPointSize);
end;

// SetNoZWrite
//

procedure TVXPoints.SetNoZWrite(const val: Boolean);
begin
  if FNoZWrite <> val then
  begin
    FNoZWrite := val;
    StructureChanged;
  end;
end;

// SetStatic
//

procedure TVXPoints.SetStatic(const val: Boolean);
begin
  if FStatic <> val then
  begin
    FStatic := val;
    if val then
      ObjectStyle := ObjectStyle - [osDirectDraw]
    else
      ObjectStyle := ObjectStyle + [osDirectDraw];
    StructureChanged;
  end;
end;

// SetSize
//

procedure TVXPoints.SetSize(const val: Single);
begin
  if FSize <> val then
  begin
    FSize := val;
    StructureChanged;
  end;
end;

// SetPositions
//

procedure TVXPoints.SetPositions(const val: TAffineVectorList);
begin
  FPositions.Assign(val);
  StructureChanged;
end;

// SetColors
//

procedure TVXPoints.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
  StructureChanged;
end;

// SetStyle
//

procedure TVXPoints.SetStyle(const val: TVXPointStyle);
begin
  if FStyle <> val then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

// SetPointParameters
//

procedure TVXPoints.SetPointParameters(const val: TVXPointParameters);
begin
  FPointParameters.Assign(val);
end;

// ------------------
// ------------------ TVXLineBase ------------------
// ------------------

// Create
//

constructor TVXLineBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := TVXColor.Create(Self);
  FLineColor.Initialize(clrWhite);
  FLinePattern := $FFFF;
  FAntiAliased := False;
  FLineWidth := 1.0;
end;

// Destroy
//

destructor TVXLineBase.Destroy;
begin
  FLineColor.Free;
  inherited Destroy;
end;

procedure TVXLineBase.NotifyChange(Sender: TObject);
begin
  if Sender = FLineColor then
    StructureChanged;
  inherited;
end;

// SetLineColor
//

procedure TVXLineBase.SetLineColor(const Value: TVXColor);
begin
  FLineColor.Color := Value.Color;
  StructureChanged;
end;

// SetLinePattern
//

procedure TVXLineBase.SetLinePattern(const Value: GLushort);
begin
  if FLinePattern <> Value then
  begin
    FLinePattern := Value;
    StructureChanged;
  end;
end;

// SetLineWidth
//

procedure TVXLineBase.SetLineWidth(const val: Single);
begin
  if FLineWidth <> val then
  begin
    FLineWidth := val;
    StructureChanged;
  end;
end;

// StoreLineWidth
//

function TVXLineBase.StoreLineWidth: Boolean;
begin
  Result := (FLineWidth <> 1.0);
end;

// SetAntiAliased
//

procedure TVXLineBase.SetAntiAliased(const val: Boolean);
begin
  if FAntiAliased <> val then
  begin
    FAntiAliased := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVXLineBase.Assign(Source: TPersistent);
begin
  if Source is TVXLineBase then
  begin
    LineColor := TVXLineBase(Source).FLineColor;
    LinePattern := TVXLineBase(Source).FLinePattern;
    LineWidth := TVXLineBase(Source).FLineWidth;
    AntiAliased := TVXLineBase(Source).FAntiAliased;
  end;
  inherited Assign(Source);
end;

// SetupLineStyle
//

procedure TVXLineBase.SetupLineStyle(var rci: TVXRenderContextInfo);
begin
  with rci.VKStates do
  begin
    Disable(stLighting);
    if FLinePattern <> $FFFF then
    begin
      Enable(stLineStipple);
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      LineStippleFactor := 1;
      LineStipplePattern := FLinePattern;
    end
    else
      Disable(stLineStipple);
    if FAntiAliased then
    begin
      Enable(stLineSmooth);
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end
    else
      Disable(stLineSmooth);
    LineWidth := FLineWidth;

    if FLineColor.Alpha <> 1 then
    begin
      if not FAntiAliased then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      glColor4fv(FLineColor.AsAddress);
    end
    else
      glColor3fv(FLineColor.AsAddress);

  end;
end;

// ------------------
// ------------------ TVXLinesNode ------------------
// ------------------

constructor TVXLinesNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := TVXColor.Create(Self);
  FColor.Initialize((TVXLinesNodes(Collection).GetOwner as TVXLines)
    .NodeColor.Color);
  FColor.OnNotifyChange := OnColorChange;
end;

destructor TVXLinesNode.Destroy;
begin
  FColor.Free;
  inherited Destroy;
end;

procedure TVXLinesNode.Assign(Source: TPersistent);
begin
  if Source is TVXLinesNode then
    FColor.Assign(TVXLinesNode(Source).FColor);
  inherited;
end;

procedure TVXLinesNode.SetColor(const val: TVXColor);
begin
  FColor.Assign(val);
end;

procedure TVXLinesNode.OnColorChange(Sender: TObject);
begin
  (Collection as TVXNodes).NotifyChange;
end;

function TVXLinesNode.StoreColor: Boolean;
begin
  Result := not VectorEquals((TVXLinesNodes(Collection).GetOwner as TVXLines)
    .NodeColor.Color, FColor.Color);
end;

// ------------------
// ------------------ TVXLinesNodes ------------------
// ------------------

constructor TVXLinesNodes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVXLinesNode);
end;

procedure TVXLinesNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TVXBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TVXNodedLines ------------------
// ------------------

constructor TVXNodedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TVXLinesNodes.Create(Self);
  FNodeColor := TVXColor.Create(Self);
  FNodeColor.Initialize(clrBlue);
  FNodeColor.OnNotifyChange := OnNodeColorChanged;
  FOldNodeColor := clrBlue;
  FNodesAspect := lnaAxes;
  FNodeSize := 1;
end;

destructor TVXNodedLines.Destroy;
begin
  FNodes.Free;
  FNodeColor.Free;
  inherited Destroy;
end;

procedure TVXNodedLines.SetNodesAspect(const Value: TLineNodesAspect);
begin
  if Value <> FNodesAspect then
  begin
    FNodesAspect := Value;
    StructureChanged;
  end;
end;

procedure TVXNodedLines.SetNodeColor(const Value: TVXColor);
begin
  FNodeColor.Color := Value.Color;
  StructureChanged;
end;

// OnNodeColorChanged
//

procedure TVXNodedLines.OnNodeColorChanged(Sender: TObject);
var
  i: Integer;
begin
  // update color for nodes...
  for i := 0 to Nodes.Count - 1 do
    if VectorEquals(TVXLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
      TVXLinesNode(Nodes[i]).Color.Assign(FNodeColor);
  SetVector(FOldNodeColor, FNodeColor.Color);
end;

procedure TVXNodedLines.SetNodes(const aNodes: TVXLinesNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

// SetNodeSize
//

procedure TVXNodedLines.SetNodeSize(const val: Single);
begin
  if val <= 0 then
    FNodeSize := 1
  else
    FNodeSize := val;
  StructureChanged;
end;

// StoreNodeSize
//

function TVXNodedLines.StoreNodeSize: Boolean;
begin
  Result := FNodeSize <> 1;
end;

// Assign
//

procedure TVXNodedLines.Assign(Source: TPersistent);
begin
  if Source is TVXNodedLines then
  begin
    SetNodes(TVXNodedLines(Source).FNodes);
    FNodesAspect := TVXNodedLines(Source).FNodesAspect;
    FNodeColor.Color := TVXNodedLines(Source).FNodeColor.Color;
    FNodeSize := TVXNodedLines(Source).FNodeSize;
  end;
  inherited Assign(Source);
end;

// DrawNode
//

procedure TVXNodedLines.DrawNode(var rci: TVXRenderContextInfo; X, Y, Z: Single;
  Color: TVXColor);
begin
  glPushMatrix;
  glTranslatef(X, Y, Z);
  case NodesAspect of
    lnaAxes:
      AxesBuildList(rci, $CCCC, FNodeSize * 0.5);
    lnaCube:
      CubeWireframeBuildList(rci, FNodeSize, False, Color.Color);
    lnaDodecahedron:
      begin
        if FNodeSize <> 1 then
        begin
          glPushMatrix;
          glScalef(FNodeSize, FNodeSize, FNodeSize);
          rci.VKStates.SetMaterialColors(cmFront, clrBlack, clrGray20,
            Color.Color, clrBlack, 0);
          DodecahedronBuildList;
          glPopMatrix;
        end
        else
        begin
          rci.VKStates.SetMaterialColors(cmFront, clrBlack, clrGray20,
            Color.Color, clrBlack, 0);
          DodecahedronBuildList;
        end;
      end;
  else
    Assert(False)
  end;
  glPopMatrix;
end;

function TVXNodedLines.AxisAlignedDimensionsUnscaled: TVector;
var
  i: Integer;
begin
  RstVector(Result);
  for i := 0 to Nodes.Count - 1 do
    MaxVector(Result, VectorAbs(Nodes[i].AsVector));
  // EG: commented out, line below looks suspicious, since scale isn't taken
  // into account in previous loop, must have been hiding another bug... somewhere...
  // DivideVector(Result, Scale.AsVector);     //DanB ?
end;

// AddNode (coords)
//

procedure TVXNodedLines.AddNode(const coords: TVXCoordinates);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

procedure TVXNodedLines.AddNode(const X, Y, Z: GLfloat);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//

procedure TVXNodedLines.AddNode(const Value: TVector);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

procedure TVXNodedLines.AddNode(const Value: TAffineVector);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TVXLines ------------------
// ------------------

constructor TVXLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDivision := 10;
  FSplineMode := lsmLines;
  FNURBSKnots := TSingleList.Create;
  FNURBSOrder := 0;
  FNURBSTolerance := 50;
end;

// Destroy
//

destructor TVXLines.Destroy;
begin
  FNURBSKnots.Free;
  inherited Destroy;
end;

procedure TVXLines.SetDivision(const Value: Integer);
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

procedure TVXLines.SetOptions(const val: TLinesOptions);
begin
  FOptions := val;
  StructureChanged;
end;

// SetSplineMode
//

procedure TVXLines.SetSplineMode(const val: TLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

procedure TVXLines.SetNURBSOrder(const val: Integer);
begin
  if val <> FNURBSOrder then
  begin
    FNURBSOrder := val;
    StructureChanged;
  end;
end;

// SetNURBSTolerance
//

procedure TVXLines.SetNURBSTolerance(const val: Single);
begin
  if val <> FNURBSTolerance then
  begin
    FNURBSTolerance := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVXLines.Assign(Source: TPersistent);
begin
  if Source is TVXLines then
  begin
    FDivision := TVXLines(Source).FDivision;
    FSplineMode := TVXLines(Source).FSplineMode;
    FOptions := TVXLines(Source).FOptions;
  end;
  inherited Assign(Source);
end;

// BuildList
//

procedure TVXLines.BuildList(var rci: TVXRenderContextInfo);
var
  i, n: Integer;
  A, B, C: GLfloat;
  f: Single;
  Spline: TCubicSpline;
  vertexColor: TVector;
  nodeBuffer: array of TAffineVector;
  colorBuffer: array of TVector;
  nurbsRenderer: GLUNurbsObj;
begin
  if Nodes.Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle(rci);
    if rci.bufferDepthTest then
      rci.VKStates.Enable(stDepthTest);
    if loColorLogicXor in Options then
    begin
      rci.VKStates.Enable(stColorLogicOp);
      rci.VKStates.LogicOpMode := loXOr;
    end;
    // Set up the control point buffer for Bezier splines and NURBS curves.
    // If required this could be optimized by storing a cached node buffer.
    if (FSplineMode = lsmBezierSpline) or (FSplineMode = lsmNURBSCurve) then
    begin
      SetLength(nodeBuffer, Nodes.Count);
      SetLength(colorBuffer, Nodes.Count);
      for i := 0 to Nodes.Count - 1 do
        with TVXLinesNode(Nodes[i]) do
        begin
          nodeBuffer[i] := AsAffineVector;
          colorBuffer[i] := Color.Color;
        end;
    end;

    if FSplineMode = lsmBezierSpline then
    begin
      // map evaluator
      rci.VKStates.PushAttrib([sttEval]);
      glEnable(GL_MAP1_VERTEX_3);
      glEnable(GL_MAP1_COLOR_4);

      glMap1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
      glMap1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @colorBuffer[0]);
    end;

    // start drawing the line
    if (FSplineMode = lsmNURBSCurve) and (FDivision >= 2) then
    begin
      if (FNURBSOrder > 0) and (FNURBSKnots.Count > 0) then
      begin

        nurbsRenderer := gluNewNurbsRenderer;
        try
          gluNurbsProperty(nurbsRenderer, GLU_SAMPLING_TOLERANCE,
            FNURBSTolerance);
          gluNurbsProperty(nurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
          gluBeginCurve(nurbsRenderer);
          gluNurbsCurve(nurbsRenderer, FNURBSKnots.Count, @FNURBSKnots.List[0],
            3, @nodeBuffer[0], FNURBSOrder, GL_MAP1_VERTEX_3);
          gluEndCurve(nurbsRenderer);
        finally
          gluDeleteNurbsRenderer(nurbsRenderer);
        end;
      end;
    end
    else
    begin
      // lines, cubic splines or bezier
      if FSplineMode = lsmSegments then
        glBegin(GL_LINES)
      else if FSplineMode = lsmLoop then
        glBegin(GL_LINE_LOOP)
      else
        glBegin(GL_LINE_STRIP);
      if (FDivision < 2) or (FSplineMode in [lsmLines, lsmSegments,
        lsmLoop]) then
      begin
        // standard line(s), draw directly
        if loUseNodeColorForLines in Options then
        begin
          // node color interpolation
          for i := 0 to Nodes.Count - 1 do
            with TVXLinesNode(Nodes[i]) do
            begin
              glColor4fv(Color.AsAddress);
              glVertex3f(X, Y, Z);
            end;
        end
        else
        begin
          // single color
          for i := 0 to Nodes.Count - 1 do
            with Nodes[i] do
              glVertex3f(X, Y, Z);
        end;
      end
      else if FSplineMode = lsmCubicSpline then
      begin
        // cubic spline
        Spline := Nodes.CreateNewCubicSpline;
        try
          f := 1 / FDivision;
          for i := 0 to (Nodes.Count - 1) * FDivision do
          begin
            Spline.SplineXYZ(i * f, A, B, C);
            if loUseNodeColorForLines in Options then
            begin
              n := (i div FDivision);
              if n < Nodes.Count - 1 then
                VectorLerp(TVXLinesNode(Nodes[n]).Color.Color,
                  TVXLinesNode(Nodes[n + 1]).Color.Color, (i mod FDivision) * f,
                  vertexColor)
              else
                SetVector(vertexColor, TVXLinesNode(Nodes[Nodes.Count - 1])
                  .Color.Color);
              glColor4fv(@vertexColor);
            end;
            glVertex3f(A, B, C);
          end;
        finally
          Spline.Free;
        end;
      end
      else if FSplineMode = lsmBezierSpline then
      begin
        f := 1 / FDivision;
        for i := 0 to FDivision do
          glEvalCoord1f(i * f);
      end;
      glEnd;
    end;
    rci.VKStates.Disable(stColorLogicOp);

    if FSplineMode = lsmBezierSpline then
      rci.VKStates.PopAttrib;
    if Length(nodeBuffer) > 0 then
    begin
      SetLength(nodeBuffer, 0);
      SetLength(colorBuffer, 0);
    end;

    if FNodesAspect <> lnaInvisible then
    begin
      if not rci.ignoreBlendingRequests then
      begin
        rci.VKStates.Enable(stBlend);
        rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;

      for i := 0 to Nodes.Count - 1 do
        with TVXLinesNode(Nodes[i]) do
          DrawNode(rci, X, Y, Z, Color);
    end;
  end;
end;

// ------------------
// ------------------ TVXCube ------------------
// ------------------

// Create
//

constructor TVXCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCubeSize := XYZVector;
  FParts := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

// BuildList
//

procedure TVXCube.BuildList(var rci: TVXRenderContextInfo);
var
  hw, hh, hd, nd: GLfloat;
  TanLoc, BinLoc: Integer;
begin
  if FNormalDirection = ndInside then
    nd := -1
  else
    nd := 1;
  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  begin
    if GL_ARB_shader_objects and (rci.VKStates.CurrentProgram > 0) then
    begin
      TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram, PGLChar(TangentAttributeName));
      BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram, PGLChar(BinormalAttributeName));
    end
    else
    begin
      TanLoc := -1;
      BinLoc := -1;
    end;

    glBegin(GL_TRIANGLES);
    if cpFront in FParts then
    begin
      glNormal3f(0, 0, nd);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(hw, hh, hd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-hw * nd, hh * nd, hd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-hw, -hh, hd);
      glVertex3f(-hw, -hh, hd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(hw * nd, -hh * nd, hd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(hw, hh, hd);
    end;
    if cpBack in FParts then
    begin
      glNormal3f(0, 0, -nd);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, -nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(hw, hh, -hd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(hw * nd, -hh * nd, -hd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(-hw, -hh, -hd);
      glVertex3f(-hw, -hh, -hd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(-hw * nd, hh * nd, -hd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(hw, hh, -hd);
    end;
    if cpLeft in FParts then
    begin
      glNormal3f(-nd, 0, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, 0, 0, nd);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(-hw, hh, hd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-hw, hh * nd, -hd * nd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-hw, -hh, -hd);
      glVertex3f(-hw, -hh, -hd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(-hw, -hh * nd, hd * nd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(-hw, hh, hd);
    end;
    if cpRight in FParts then
    begin
      glNormal3f(nd, 0, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, 0, 0, -nd);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(hw, hh, hd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(hw, -hh * nd, hd * nd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(hw, -hh, -hd);
      glVertex3f(hw, -hh, -hd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(hw, hh * nd, -hd * nd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(hw, hh, hd);
    end;
    if cpTop in FParts then
    begin
      glNormal3f(0, nd, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, 0, -nd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-hw, hh, -hd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-hw * nd, hh, hd * nd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(hw, hh, hd);
      glVertex3f(hw, hh, hd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(hw * nd, hh, -hd * nd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-hw, hh, -hd);
    end;
    if cpBottom in FParts then
    begin
      glNormal3f(0, -nd, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, -nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, 0, nd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-hw, -hh, -hd);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(hw * nd, -hh, -hd * nd);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(hw, -hh, hd);
      glVertex3f(hw, -hh, hd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-hw * nd, -hh, hd * nd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-hw, -hh, -hd);
    end;
    glEnd;
  end;
end;

// GenerateSilhouette
//

function TVXCube.GenerateSilhouette(const silhouetteParameters
  : TVXSilhouetteParameters): TVXSilhouette;
var
  hw, hh, hd: GLfloat;
  connectivity: TConnectivity;
  sil: TVXSilhouette;
begin
  connectivity := TConnectivity.Create(True);

  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  if cpFront in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, -hh, hd),
      AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, -hh, hd), AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, -hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, -hh, hd),
      AffineVectorMake(-hw, -hh, hd));
  end;

  sil := nil;
  connectivity.CreateSilhouette(silhouetteParameters, sil, False);

  Result := sil;

  connectivity.Free;
end;

// GetCubeWHD
//
function TVXCube.GetCubeWHD(const Index: Integer): GLfloat;
begin
  Result := FCubeSize.V[index];
end;


// SetCubeWHD
//
procedure TVXCube.SetCubeWHD(Index: Integer; AValue: GLfloat);
begin
  if AValue <> FCubeSize.V[index] then
  begin
    FCubeSize.V[index] := AValue;
    StructureChanged;
  end;
end;


// SetParts
//
procedure TVXCube.SetParts(aValue: TCubeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TVXCube.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVXCube.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXCube) then
  begin
    FCubeSize := TVXCube(Source).FCubeSize;
    FParts := TVXCube(Source).FParts;
    FNormalDirection := TVXCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVXCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := FCubeSize.X * 0.5;
  Result.Y := FCubeSize.Y * 0.5;
  Result.Z := FCubeSize.Z * 0.5;
  Result.W := 0;
end;

// RayCastIntersect
//

function TVXCube.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  p: array [0 .. 5] of TVector;
  rv: TVector;
  rs, r: TVector;
  i: Integer;
  t, e: Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  e := 0.5 + 0.0001; // Small value for floating point imprecisions
  eSize.X := FCubeSize.X * e;
  eSize.Y := FCubeSize.Y * e;
  eSize.Z := FCubeSize.Z * e;
  p[0] := XHmgVector;
  p[1] := YHmgVector;
  p[2] := ZHmgVector;
  SetVector(p[3], -1, 0, 0);
  SetVector(p[4], 0, -1, 0);
  SetVector(p[5], 0, 0, -1);
  for i := 0 to 5 do
  begin
    if VectorDotProduct(p[i], rv) > 0 then
    begin
      t := -(p[i].X * rs.X + p[i].Y * rs.Y +
             p[i].Z * rs.Z + 0.5 *
        FCubeSize.V[i mod 3]) / (p[i].X * rv.X +
                                 p[i].Y * rv.Y +
                                 p[i].Z * rv.Z);
      MakePoint(r, rs.X + t * rv.X, rs.Y +
                             t * rv.Y, rs.Z +
                             t * rv.Z);
      if (Abs(r.X) <= eSize.X) and
         (Abs(r.Y) <= eSize.Y) and
         (Abs(r.Z) <= eSize.Z) and
        (VectorDotProduct(VectorSubtract(r, rs), rv) > 0) then
      begin
        if Assigned(intersectPoint) then
          MakePoint(intersectPoint^, LocalToAbsolute(r));
        if Assigned(intersectNormal) then
          MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

// DefineProperties
//

procedure TVXCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize.X <> 1) or (FCubeSize.Y <> 1) or (FCubeSize.Z <> 1));
end;

// ReadData
//

procedure TVXCube.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// WriteData
//

procedure TVXCube.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// ------------------
// ------------------ TVXQuadricObject ------------------
// ------------------

// Create
//

constructor TVXQuadricObject.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

// SetNormals
//

procedure TVXQuadricObject.SetNormals(aValue: TNormalSmoothing);
begin
  if aValue <> FNormals then
  begin
    FNormals := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TVXQuadricObject.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// SetupQuadricParams
//

procedure TVXQuadricObject.SetupQuadricParams(quadric: GLUquadricObj);
const
  cNormalSmoothinToEnum: array [nsFlat .. nsNone] of GLEnum = (GLU_FLAT,
    GLU_SMOOTH, GLU_NONE);
begin
  gluQuadricDrawStyle(quadric, GLU_FILL);
  gluQuadricNormals(quadric, cNormalSmoothinToEnum[FNormals]);
  SetNormalQuadricOrientation(quadric);
  gluQuadricTexture(quadric, GLboolean(True));
end;

// SetNormalQuadricOrientation
//

procedure TVXQuadricObject.SetNormalQuadricOrientation(quadric: GLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of GLEnum =
    (GLU_INSIDE, GLU_OUTSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//

procedure TVXQuadricObject.SetInvertedQuadricOrientation
  (quadric: GLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of GLEnum =
    (GLU_OUTSIDE, GLU_INSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//

procedure TVXQuadricObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXQuadricObject) then
  begin
    FNormals := TVXQuadricObject(Source).FNormals;
    FNormalDirection := TVXQuadricObject(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TVXSphere ------------------
// ------------------

// Create
//

constructor TVXSphere.Create(AOwner: TComponent);
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

procedure TVXSphere.BuildList(var rci: TVXRenderContextInfo);
var
  v1, V2, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  i, j: Integer;
  DoReverse: Boolean;
begin
  DoReverse := (FNormalDirection = ndInside);
  rci.VKStates.PushAttrib([sttPolygon]);
  if DoReverse then
    rci.VKStates.InvertFrontFace;

  // common settings
  AngTop := DegToRadian(1.0 * FTop);
  AngBottom := DegToRadian(1.0 * FBottom);
  AngStart := DegToRadian(1.0 * FStart);
  AngStop := DegToRadian(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;
  glPushMatrix;
  glScalef(Radius, Radius, Radius);

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngTop, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, -1, 0)
    else
      glNormal3f(0, 1, 0);
    if FTopCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin
      glVertex3f(0, SinP, 0);
      N1 := YVector;
      if DoReverse then
        N1.Y := -N1.Y;
    end;
    v1.Y := SinP;
    Theta := AngStart;
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      v1.Z := CosP * CosT;
      if FTopCap = ctCenter then
      begin
        N1 := VectorPerpendicular(YVector, v1);
        if DoReverse then
          NegateVector(N1);
      end;
      glTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glNormal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta + StepH;
    end;
    glEnd;
  end;

  // main body
  Phi := AngTop;
  Phi2 := Phi - StepV;
  uTexFactor := 1 / FSlices;
  vTexFactor := 1 / FStacks;

  for j := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCosine(Phi, SinP, CosP);
    SinCosine(Phi2, SinP2, CosP2);
    v1.Y := SinP;
    V2.Y := SinP2;
    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;

    glBegin(GL_TRIANGLE_STRIP);
    for i := 0 to FSlices do
    begin

      SinCosine(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      V2.X := CosP2 * SinT;
      v1.Z := CosP * CosT;
      V2.Z := CosP2 * CosT;

      uTexCoord := i * uTexFactor;
      glTexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v1);
      glVertex3fv(@v1);

      glTexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@V2);
      glVertex3fv(@V2);

      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngBottom, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, 1, 0)
    else
      glNormal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin
      glVertex3f(0, SinP, 0);
      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
      begin
        N1 := YVector;
        NegateVector(N1); 
      end;
    end;
    v1.Y := SinP;
    Theta := AngStop;
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      v1.Z := CosP * CosT;
      if FBottomCap = ctCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        if DoReverse then
          NegateVector(N1);
      end;
      glTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glNormal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta - StepH;
    end;
    glEnd;
  end;
  if DoReverse then
    rci.VKStates.InvertFrontFace;
  glPopMatrix;
  rci.VKStates.PopAttrib;
end;

// RayCastIntersect
//

function TVXSphere.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2: TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//

function TVXSphere.GenerateSilhouette(const silhouetteParameters
  : TVXSilhouetteParameters): TVXSilhouette;
var
  i, j: Integer;
  s, C, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TVXSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCosine(i * angleFactor, FRadius, s, C);
    Result.vertices.AddPoint(VectorCombine(sVec, tVec, s, C));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.vertices.Add(NullHmgPoint);
end;

// SetBottom
//

procedure TVXSphere.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TVXSphere.SetBottomCap(aValue: TCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TVXSphere.SetRadius(const aValue: GLfloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TVXSphere.SetSlices(aValue: Integer);
begin
  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TVXSphere.SetStacks(aValue: GLint);
begin
  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

// SetStart
//

procedure TVXSphere.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

// SetStop
//

procedure TVXSphere.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

// SetTop
//

procedure TVXSphere.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TVXSphere.SetTopCap(aValue: TCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVXSphere.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXSphere) then
  begin
    FRadius := TVXSphere(Source).FRadius;
    FSlices := TVXSphere(Source).FSlices;
    FStacks := TVXSphere(Source).FStacks;
    FBottom := TVXSphere(Source).FBottom;
    FTop := TVXSphere(Source).FTop;
    FStart := TVXSphere(Source).FStart;
    FStop := TVXSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVXSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// ------------------
// ------------------ TVXPolygonBase ------------------
// ------------------

// Create
//

constructor TVXPolygonBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

// CreateNodes
//

procedure TVXPolygonBase.CreateNodes;
begin
  FNodes := TVXNodes.Create(Self);
end;

// Destroy
//

destructor TVXPolygonBase.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TVXPolygonBase then
  begin
    SetNodes(TVXPolygonBase(Source).FNodes);
    FDivision := TVXPolygonBase(Source).FDivision;
    FSplineMode := TVXPolygonBase(Source).FSplineMode;
  end;
  inherited Assign(Source);
end;

// NotifyChange
//

procedure TVXPolygonBase.NotifyChange(Sender: TObject);
begin
  if Sender = Nodes then
    StructureChanged;
  inherited;
end;

// SetDivision
//

procedure TVXPolygonBase.SetDivision(const Value: Integer);
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

procedure TVXPolygonBase.SetNodes(const aNodes: TVXNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

// SetSplineMode
//

procedure TVXPolygonBase.SetSplineMode(const val: TLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

// AddNode (coords)
//

procedure TVXPolygonBase.AddNode(const coords: TVXCoordinates);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

// AddNode (xyz)
//

procedure TVXPolygonBase.AddNode(const X, Y, Z: GLfloat);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//

procedure TVXPolygonBase.AddNode(const Value: TVector);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

// AddNode (affine vector)
//

procedure TVXPolygonBase.AddNode(const Value: TAffineVector);
var
  n: TVXNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TVXSuperellipsoid ------------------
// ------------------

// Create
//

constructor TVXSuperellipsoid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FxyCurve := 1.0;
  FzCurve := 1.0;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

// BuildList
//

procedure TVXSuperellipsoid.BuildList(var rci: TVXRenderContextInfo);
var
  CosPc1, SinPc1, CosTc2, SinTc2: Double;

  tc1, tc2: integer;
  v1, v2, vs, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Double;
  i, j: Integer;
  DoReverse: Boolean;

begin
  DoReverse := (FNormalDirection = ndInside);
  if DoReverse then
    rci.VKStates.InvertFrontFace;

  // common settings
  AngTop := DegToRadian(1.0 * FTop);
  AngBottom := DegToRadian(1.0 * FBottom);
  AngStart := DegToRadian(1.0 * FStart);
  AngStop := DegToRadian(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  { Even integer used with the Power function, only produce positive points }
  tc1 := trunc(xyCurve);
  tc2 := trunc(zCurve);
  if tc1 mod 2 = 0 then
    xyCurve := xyCurve + 1e-6;
  if tc2 mod 2 = 0 then
    zCurve := zCurve - 1e-6;

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngTop, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, -1, 0)
    else
      glNormal3f(0, 1, 0);

    if FTopCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = xyCurve) then
        SinPc1 := Power(SinP, xyCurve)
      else
        SinPc1 := -Power(-SinP, xyCurve);
      glVertex3f(0, SinPc1*Radius, 0);

      N1 := YVector;
      if DoReverse then
        N1.Y := -N1.Y;
    end; { FTopCap = ctFlat }

    //  v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);
    v1.Y := SinPc1;

    Theta := AngStart;

    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      //    v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP, xyCurve)
      else
        CosPc1 := -Power(-CosP, xyCurve);

      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);
      v1.X := CosPc1 * SinTc2;

      //    v1.Z := CosP * CosT;
      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);
      v1.Z := CosPc1 * CosTc2;

      if FTopCap = ctCenter then
      begin
        N1 := VectorPerpendicular(YVector, v1);
        if DoReverse then
          NegateVector(N1);
      end;
      //    xglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glTexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      glNormal3fv(@N1);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);
      Theta := Theta + StepH;
    end;
    glEnd;
  end;

  // main body
  Phi := AngTop;
  Phi2 := Phi - StepV;
  uTexFactor := 1 / FSlices;
  vTexFactor := 1 / FStacks;

  for j := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCosine(Phi, SinP, CosP);
    SinCosine(Phi2, SinP2, CosP2);

    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);
    v1.Y := SinPc1;

    if (Sign(SinP2) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP2, xyCurve)
    else
      SinPc1 := -Power(-SinP2, xyCurve);
    v2.Y := SinPc1;

    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;

    glBegin(GL_TRIANGLE_STRIP);
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);

      if (Sign(CosP) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP, xyCurve)
      else
        CosPc1 := -Power(-CosP, xyCurve);

      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);
      v1.X := CosPc1 * SinTc2;

      if (Sign(CosP2) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP2, xyCurve)
      else
        CosPc1 := -Power(-CosP2, xyCurve);
      V2.X := CosPc1 * SinTc2;

      if (Sign(CosP) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP, xyCurve)
      else
        CosPc1 := -Power(-CosP, xyCurve);

      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);
      v1.Z := CosPc1 * CosTc2;

      if (Sign(CosP2) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP2, xyCurve)
      else
        CosPc1 := -Power(-CosP2, xyCurve);
      V2.Z := CosPc1 * CosTc2;

      uTexCoord := i * uTexFactor;
      glTexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v1);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);

      glTexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v2);
      vs := v2;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);

      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngBottom, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, 1, 0)
    else
      glNormal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = xyCurve) then
        SinPc1 := Power(SinP, xyCurve)
      else
        SinPc1 := -Power(-SinP, xyCurve);
      glVertex3f(0, SinPc1*Radius, 0);

      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
        N1 := YVector;
    end;
    //  v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = xyCurve) then
      SinPc1 := Power(SinP, xyCurve)
    else
      SinPc1 := -Power(-SinP, xyCurve);
    v1.Y := SinPc1;

    Theta := AngStop;
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      //    v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = xyCurve) then
        CosPc1 := Power(CosP, xyCurve)
      else
        CosPc1 := -Power(-CosP, xyCurve);

      if (Sign(SinT) = 1) or (tc2 = zCurve) then
        SinTc2 := Power(SinT, zCurve)
      else
        SinTc2 := -Power(-SinT, zCurve);
      v1.X := CosPc1 * SinTc2;

      //    v1.Z := CosP * CosT;
      if (Sign(CosT) = 1) or (tc2 = zCurve) then
        CosTc2 := Power(CosT, zCurve)
      else
        CosTc2 := -Power(-CosT, zCurve);
      v1.Z := CosPc1 * CosTc2;

      if FBottomCap = ctCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        if DoReverse then
          NegateVector(N1);
        glNormal3fv(@N1);
      end;
      //    xglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glTexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);
      Theta := Theta - StepH;
    end;
    glEnd;
  end;
  if DoReverse then
    rci.VKStates.InvertFrontFace;
end;

// RayCastIntersect
// This will probably not work, karamba
// RayCastSphereIntersect -> RayCastSuperellipsoidIntersect ??????

function TVXSuperellipsoid.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2: TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
// This will probably not work;

function TVXSuperellipsoid.GenerateSilhouette(const silhouetteParameters
  : TVXSilhouetteParameters): TVXSilhouette;
var
  i, j: Integer;
  s, C, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TVXSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCosine(i * angleFactor, FRadius, s, C);
    Result.vertices.AddPoint(VectorCombine(sVec, tVec, s, C));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.vertices.Add(NullHmgPoint);
end;

// SetBottom
//

procedure TVXSuperellipsoid.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TVXSuperellipsoid.SetBottomCap(aValue: TCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TVXSuperellipsoid.SetRadius(const aValue: GLfloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetxyCurve
//

procedure TVXSuperellipsoid.SetxyCurve(const aValue: GLfloat);
begin
  if aValue <> FxyCurve then
  begin
    FxyCurve := aValue;
    StructureChanged;
  end;
end;

// SetzCurve
//

procedure TVXSuperellipsoid.SetzCurve(const aValue: GLfloat);
begin
  if aValue <> FzCurve then
  begin
    FzCurve := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TVXSuperellipsoid.SetSlices(aValue: Integer);
begin
  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TVXSuperellipsoid.SetStacks(aValue: GLint);
begin
  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

// SetStart
//

procedure TVXSuperellipsoid.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

// SetStop
//

procedure TVXSuperellipsoid.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

// SetTop
//

procedure TVXSuperellipsoid.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TVXSuperellipsoid.SetTopCap(aValue: TCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVXSuperellipsoid.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXSuperellipsoid) then
  begin
    FRadius := TVXSuperellipsoid(Source).FRadius;
    FSlices := TVXSuperellipsoid(Source).FSlices;
    FStacks := TVXSuperellipsoid(Source).FStacks;
    FBottom := TVXSuperellipsoid(Source).FBottom;
    FTop := TVXSuperellipsoid(Source).FTop;
    FStart := TVXSuperellipsoid(Source).FStart;
    FStop := TVXSuperellipsoid(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVXSuperellipsoid.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

RegisterClasses([TVXSphere, TVXCube, TVXPlane, TVXSprite, TVXPoints,
  TVXDummyCube, TVXLines, TVXSuperellipsoid]);

end.