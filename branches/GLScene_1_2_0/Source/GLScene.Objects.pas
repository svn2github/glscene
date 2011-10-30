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
  <li>16/05/11 - Yar - Transition to indirect rendering objects
  <li>23/03/11 - Yar - Bugfixed TGLPlane.Assign (thanks ltyrosine)
                       Replaced plane primitives to triangles, added tangent and binormal attributes
  <li>29/11/10 - Yar - Bugfixed client color array enabling in TGLPoints.BuildList when it not used (thanks rbenetis)
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>29/06/10 - Yar - Added loColorLogicXor to TGLLines.Options
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>11/04/10 - Yar - Replaced glNewList to GLState.NewList in TGLDummyCube.DoRender
  <li>05/03/10 - DanB - More state added to TGLStateCache
  <li>22/02/10 - Yar - Removed NoZWrite in TGLPlane, TGLSprite
  Now use Material.DepthProperties
  <li>28/12/09 - DanB - Modifying TGLLineBase.LineColor now calls StructureChanged
  <li>13/03/09 - DanB - ScreenRect now accepts a buffer parameter, rather than using CurrentBuffer
  <li>05/10/08 - DaStr - Added lsmLoop support to TGLLines
  (thanks Alejandro Leon Escalera) (BugtrackerID = 2084250)
  <li>22/01/08 - DaStr - Fixed rendering of TGLPoints
  (thanks Kapitan) (BugtrackerID = 1876920)
  <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
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
  <li>03/12/04 - MF - Added TGLSprite.AxisAlignedDimensionsUnscaled override
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
unit GLScene.Objects;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.GeometryBB,
  GLScene.Core,
  GLScene.Base.OpenGL.Adapter,
  GLScene.Base.OpenGL.Tokens,
  SysUtils,
  GLScene.Base.Vector.Lists,
  GLScene.Base.Classes,
  GLScene.Platform,
  GLScene.Base.Context,
  GLScene.Silhouette,
  GLScene.Base.Color,
  GLScene.Base.Transformation,
  GLScene.Base.Context.Info,
  GLScene.Nodes,
  GLScene.Base.Coordinates,
  GLScene.Material,
  GLScene.MaterialEx,
  GLScene.Mesh,
  GLScene.DrawTechnique;

type

  // TGLCustomSceneObjectEx
  //
  TGLCustomSceneObjectEx = class(TGLCustomSceneObject)
  private
    FMeshExtras: TMeshExtras;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
    procedure SetShowAABB(const Value: Boolean);
    function GetShowAABB: Boolean;
    procedure SetMeshExtras(const Value: TMeshExtras);
    function GetPickingMaterial: string;
    procedure SetPickingMaterial(const Value: string);
  protected
    { Protected Declarations }
    FBatch: TDrawBatch;
    FTransformation: TTransformationRec;
    FFinishEvent: TFinishTaskEvent;
    procedure BuildMesh; virtual; stdcall;
    function GetLibMaterialName: string; virtual;
    procedure SetLibMaterialName(const Value: string); virtual;
    procedure SelectMaterial;
    procedure Loaded; override;
    procedure DoShowAxes; override;
    procedure ApplyExtras;

    property MaterialLibrary: TGLAbstractMaterialLibrary read GetMaterialLibrary
      write SetMaterialLibrary;
    property LibMaterialName: string read GetLibMaterialName
      write SetLibMaterialName;
    property CustomPickingMaterial: string read GetPickingMaterial
      write SetPickingMaterial;
    property ShowAABB: Boolean read GetShowAABB write SetShowAABB default False;
    property MeshExtras: TMeshExtras read FMeshExtras write SetMeshExtras default [];
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure TransformationChanged; override;
    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function AxisAlignedBoundingBoxUnscaled(
      const AIncludeChilden: Boolean = True): TAABB; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;
  end;

  TGLSceneObjectEx = class(TGLCustomSceneObjectEx)
  published
    { Published Declarations }
    property MeshExtras;
    property Material;
    property MaterialLibrary;
    property LibMaterialName;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property Static;
    property ShowAxes;
    property ShowAABB;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property CustomPickingMaterial;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // TGLVisibilityDeterminationEvent
  //
  TGLVisibilityDeterminationEvent = function(Sender: TObject;
    var rci: TRenderContextInfo): Boolean of object;

  // TGLDummyCube
  //
  { : A simple cube, invisible at run-time.<p>
    This is a usually non-visible object -except at design-time- used for
    building hierarchies or groups, when some kind of joint or movement
    mechanism needs be described, you can use DummyCubes.<br>
    DummyCube's barycenter is its children's barycenter.<br> }

  TGLDummyCube = class(TGLCameraInvariantObject)
  private
    { Private Declarations }
    FBatch: TDrawBatch;
    FTransformation: TTransformationRec;
    FCubeSize: TGLFloat;
    FEdgeColor: TGLColor;
    FVisibleAtRunTime: Boolean;
    FAmalgamate: Boolean;
    FOnVisibilityDetermination: TGLVisibilityDeterminationEvent;
    procedure BuildMesh;
  protected
    { Protected Declarations }
    procedure SetCubeSize(const val: TGLFloat);
    procedure SetEdgeColor(const val: TGLColor);
    procedure SetVisibleAtRunTime(const val: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterAbsolutePosition: TVector; override;
    procedure TransformationChanged; override;

    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;
  published
    { Published Declarations }
    property CubeSize: TGLFloat read FCubeSize write SetCubeSize;
    property EdgeColor: TGLColor read FEdgeColor write SetEdgeColor;
    { : If true the dummycube's edges will be visible at runtime.<p>
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. }
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime default False;
    { : Fake property for backward compatibility. }
    property Amalgamate: Boolean read FAmalgamate write FAmalgamate
      default False;
    { : Camera Invariance Options.<p>
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. }
    property CamInvarianceMode default cimNone;
    { : Event for custom visibility determination.<p>
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. }
    property OnVisibilityDetermination: TGLVisibilityDeterminationEvent
      read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;

  // TPlaneStyle
  //
  TPlaneStyle = (psSingleQuad, psTileTexture);
  TPlaneStyles = set of TPlaneStyle;

  // Plane
  //
  { : A simple plane object.<p>
    Note that a plane is always made of a single quad (two triangles) and the
    tiling is only applied to texture coordinates. }
  TGLPlane = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FXOffset, FYOffset: TGLFloat;
    FXScope, FYScope: TGLFloat;
    FWidth, FHeight: TGLFloat;
    FXTiles, FYTiles: Cardinal;
    FStyle: TPlaneStyles;
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);
    procedure SetXOffset(const Value: TGLFloat);
    procedure SetXScope(const Value: TGLFloat);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: TGLFloat);
    procedure SetYScope(const Value: TGLFloat);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const val: TPlaneStyles);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function ScreenRect(ABuffer: TGLSceneBuffer): TGLRect;
    function PointDistance(const aPoint: TVector): Single;
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
      default [psSingleQuad, psTileTexture];
  end;

  TGLSpriteAlign = (alSpherical, alCylindrical);

  // TGLSprite
  //
  { : A rectangular area, perspective projected, but always facing the camera.<p>
    A TGLSprite is perspective projected and as such is scaled with distance,
    if you want a 2D sprite that does not get scaled, see TGLHUDSprite. }
  TGLSprite = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FWidth: TGLFloat;
    FHeight: TGLFloat;
    FRotation: TGLFloat;
    FMirrorU: Boolean;
    FMirrorV: Boolean;
    FAlign: TGLSpriteAlign;
    FModulateColor: TGLColor;
    function GetAlphaChannel: Single;
    procedure SetAlphaChannel(const Value: Single);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure SetWidth(const val: TGLFloat);
    procedure SetHeight(const val: TGLFloat);
    procedure SetRotation(const val: TGLFloat);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
    procedure OnColorChange(Sender: TObject);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

    procedure SetSize(const Width, Height: TGLFloat);
    // : Set width and height to "size"
    procedure SetSquareSize(const size: TGLFloat);
  published
    { Published Declarations }
    { : Sprite Width in 3D world units. }
    property Width: TGLFloat read FWidth write SetWidth;
    { : Sprite Height in 3D world units. }
    property Height: TGLFloat read FHeight write SetHeight;
    { : This the ON-SCREEN rotation of the sprite.<p>
      Rotatation=0 is handled faster. }
    property Rotation: TGLFloat read FRotation write SetRotation;
    { : Fake property for backward compatibility. }
    property AlphaChannel: Single read GetAlphaChannel write SetAlphaChannel;
    { : Reverses the texture coordinates in the U and V direction to mirror
      the texture. }
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
    { : Align to camera view. }
    property Align: TGLSpriteAlign read FAlign write FAlign default alSpherical;
  end;

  // TGLPointStyle
  //
  TGLPointStyle =
    (
    psCustom,
    psSquare,
    psRound,
    psSmooth,
    psSmoothAdditive,
    psSquareAdditive
    );

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
    procedure SetEnabled(const val: Boolean);
    procedure SetMinSize(const val: Single);
    procedure SetMaxSize(const val: Single);
    procedure SetFadeTresholdSize(const val: Single);
    procedure SetDistanceAttenuation(const val: TGLCoordinates);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
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
  TGLPoints = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FPositions: TAffineVectorList;
    FColors: TVectorList;
    FSize: Single;
    FStyle: TGLPointStyle;
    FStatic, FNoZWrite: Boolean;
    FPointParameters: TGLPointParameters;
    procedure SetNoZWrite(const Value: Boolean);
    procedure SetSize(const Value: Single);
    procedure SetStatic(const Value: Boolean);
    procedure SetStyle(const Value: TGLPointStyle);
    function StoreSize: Boolean;
    procedure SetPointParameters(const Value: TGLPointParameters);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure UpdateMaterial;
    procedure SetPositions(const val: TAffineVectorList);
    procedure SetColors(const val: TVectorList);
    procedure Loaded; override;
    procedure SetLibMaterialName(const Value: string); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    { : Points positions.<p>
      If empty, a single point is assumed at (0, 0, 0) }
    property Positions: TAffineVectorList read FPositions write SetPositions;
    { : Defines the points colors.<p>
      <ul>
      <li>if empty, point color will be opaque white
      <li>if contains N colors, the first N points (at max) will be rendered
      using the corresponding colors.
      </ul> }
    property Colors: TVectorList read FColors write SetColors;
  published
    { Published Declarations }

    {: Use this properties for quick setup library material.
       Whithout it they has no afect! }

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
    property Style: TGLPointStyle read FStyle write SetStyle default psSquare;
    { : Point parameters as of ARB_point_parameters.<p>
       Allows to vary the size and transparency of points depending
       on their distance to the observer. }
    property PointParameters: TGLPointParameters read FPointParameters
      write SetPointParameters;
  end;

  // TGLLineBase
  //
  { : Base class for line objects.<p>
    Introduces line style properties (width, color...). }
  TGLLineBase = class(TGLCustomSceneObjectEx)
  private
    { Private Declarations }
    FLineColor: TGLColor;
    FLinePattern: TGLushort;
    FLineWidth: Single;
    FAntiAliased: Boolean;
    FMaterialChanged: Boolean;
  protected
    { Protected Declarations }
    FMaterialEx: TGLLibMaterialEx;
    procedure UpdateMaterial;
    procedure SetLineColor(const Value: TGLColor);
    procedure SetLinePattern(const Value: TGLushort);
    procedure SetLineWidth(const val: Single);
    function StoreLineWidth: Boolean;
    procedure SetAntiAliased(const val: Boolean);
    procedure Loaded; override;
    procedure SetLibMaterialName(const Value: string); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
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

    property MaterialLibrary;
    property LibMaterialName;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property ShowAABB;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property CustomPickingMaterial;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // TLineNodesAspect
  //
  { : Possible aspects for the nodes of a TLine. }
  TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);

  // TLineSplineMode
  //
  { : Available spline modes for a TLine. }
  TLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve,
    lsmSegments, lsmLoop);

  // TGLLinesNode
  //
  { : Specialized Node for use in a TGLLines objects.<p>
    Adds a Color property (TGLColor). }
  TGLLinesNode = class(TGLNode)
  private
    { Private Declarations }
    FColor: TGLColor;
    FNodeTransformation: TTransformationRec;
  protected
    { Protected Declarations }
    procedure SetColor(const val: TGLColor);
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

  // TGLNodedLines
  //
  { : Class that defines lines via a series of nodes.<p>
    Base class, does not render anything. }
  TGLNodedLines = class(TGLLineBase)
  private
    { Private Declarations }
    FNodes: TGLLinesNodes;
    FNodesAspect: TLineNodesAspect;
    FDefaultNodeColor: TGLColor;
    FNodeSize: Single;
    FOldNodeColor: TColorVector;
    FNodeBatch: TDrawBatch;
  protected
    { Protected Declarations }
    procedure SetNodesAspect(const Value: TLineNodesAspect);
    procedure SetNodeColor(const Value: TGLColor);
    procedure OnNodeColorChanged(Sender: TObject);
    procedure SetNodes(const aNodes: TGLLinesNodes);
    procedure SetNodeSize(const val: Single);
    function StoreNodeSize: Boolean;

    procedure BuildNodeMesh;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;

    procedure AddNode(const coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLFloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;

  published
    { Published Declarations }
    { : Default color for nodes.<p>
      lnaInvisible and lnaAxes ignore this setting. }
    property DefaultNodeColor: TGLColor read FDefaultNodeColor write SetNodeColor;
    { : The nodes list.<p> }
    property Nodes: TGLLinesNodes read FNodes write SetNodes;

    { : Default aspect of line nodes.<p>
      May help you materialize nodes, segments and control points. }
    property NodesAspect: TLineNodesAspect read FNodesAspect
      write SetNodesAspect default lnaAxes;
    { : Size for the various node aspects. }
    property NodeSize: Single read FNodeSize write SetNodeSize
      stored StoreNodeSize;
  end;

  // TLinesOptions
  //
  TLinesOption = (loUseNodeColorForLines, loTextureCoord, loColorLogicXor);
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
    procedure SetSplineMode(const val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetOptions(const val: TLinesOptions);
    procedure SetNURBSOrder(const val: Integer);
    procedure SetNURBSTolerance(const val: Single);
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

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
      default lsmLines;

    { : Rendering options for the line.<p>
      <ul>
      <li>loUseNodeColorForLines: if set lines will be drawn using node
      colors (and color interpolation between nodes), if not, LineColor
      will be used (single color).
      loColorLogicXor: enable logic operation for color of XOR type.
      </ul> }
    property Options: TLinesOptions read FOptions write SetOptions;
  end;

  TCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TCubeParts = set of TCubePart;

  // TGLCube
  //
  TGLCube = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FCubeSize: TAffineVector;
    FParts: TCubeParts;
    FNormalDirection: TNormalDirection;
    procedure SetCubeWidth(const aValue: Single);
    procedure SetCubeHeight(const aValue: Single);
    procedure SetCubeDepth(const aValue: Single);
    procedure SetParts(aValue: TCubeParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published Declarations }
    property CubeWidth: Single read FCubeSize[0] write SetCubeWidth stored
      False;
    property CubeHeight: Single read FCubeSize[1] write SetCubeHeight stored
      False;
    property CubeDepth: Single read FCubeSize[2] write SetCubeDepth stored
      False;
    property NormalDirection: TNormalDirection read FNormalDirection write
      SetNormalDirection default ndOutside;
    property Parts: TCubeParts read FParts write SetParts
      default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  // TNormalSmoothing
  //
  { : Determines how and if normals are smoothed.<p>
    - nsFlat : facetted look<br>
    - nsSmooth : smooth look<br>
    - nsNone : unlighted rendering, usefull for decal texturing }
  TNormalSmoothing =
    (
    nsFlat,
    nsSmooth,
    nsNone
    );

  TAngleLimit1 = -90..90;
  TAngleLimit2 = 0..360;
  TCapType = (ctNone, ctCenter, ctFlat);

  // TGLSphere
  //
  { : A sphere object.<p>
    The sphere can have to and bottom caps, as well as being just a slice
    of sphere. }
  TGLSphere = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;
    FNormalDirection: TNormalDirection;
    FNormals: TNormalSmoothing;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TCapType);
    procedure SetRadius(const aValue: TGLFloat);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: TGLInt);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TCapType);
    procedure SetNormalDirection(const Value: TNormalDirection);
    procedure SetNormals(const Value: TNormalSmoothing);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
  published
    { Published Declarations }
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
  end;

  // TGLPolygonBase
  //
  { : Base class for objects based on a polygon. }
  TGLPolygonBase = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FDivision: Integer;
    FSplineMode: TLineSplineMode;

  protected
    { Protected Declarations }
    FNodes: TGLNodes;
    procedure CreateNodes; dynamic;
    procedure SetSplineMode(const val: TLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const aNodes: TGLNodes);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure AddNode(const coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLFloat); overload;
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
      default lsmLines;

  end;

  { : Issues OpenGL for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);
{ : Issues OpenGL for a unit-size dodecahedron. }
procedure DodecahedronBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);
{ : Issues OpenGL for a unit-size icosahedron. }
procedure IcosahedronBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);

var
  TangentAttributeName: AnsiString = 'Tangent';
  BinormalAttributeName: AnsiString = 'Binormal';

implementation

uses
{$IFDEF GLS_SERVICE_CONTEXT}
  SyncObjs,
{$ENDIF}
  GLScene.Base.Spline,
  GLScene.Base.GLStateMachine,
  GLScene.Shader.Parameter;

{$IFDEF GLS_REGION}{$REGION 'Helper functions'}{$ENDIF}

// CubeWireframeBuildList
//

procedure CubeWireframeBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);
var
  pn, pp: Single;
begin
  pp := ASize * 0.5;
  pn := -pp;
  with AMesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);

      BeginAssembly(mpLINES);
      Attribute3f(attrPosition, pp, pp, pp);
      EmitVertex;
      Attribute3f(attrPosition, pn, pp, pp);
      EmitVertex;

      Attribute3f(attrPosition, pp, pn, pp);
      EmitVertex;
      Attribute3f(attrPosition, pn, pn, pp);
      EmitVertex;

      Attribute3f(attrPosition, pp, pp, pp);
      EmitVertex;
      Attribute3f(attrPosition, pp, pn, pp);
      EmitVertex;

      Attribute3f(attrPosition, pn, pp, pp);
      EmitVertex;
      Attribute3f(attrPosition, pn, pn, pp);
      EmitVertex;

      Attribute3f(attrPosition, pp, pp, pn);
      EmitVertex;
      Attribute3f(attrPosition, pn, pp, pn);
      EmitVertex;

      Attribute3f(attrPosition, pp, pn, pn);
      EmitVertex;
      Attribute3f(attrPosition, pn, pn, pn);
      EmitVertex;

      Attribute3f(attrPosition, pp, pp, pn);
      EmitVertex;
      Attribute3f(attrPosition, pp, pn, pn);
      EmitVertex;

      Attribute3f(attrPosition, pn, pp, pn);
      EmitVertex;
      Attribute3f(attrPosition, pn, pn, pn);
      EmitVertex;

      Attribute3f(attrPosition, pp, pp, pp);
      EmitVertex;
      Attribute3f(attrPosition, pp, pp, pn);
      EmitVertex;

      Attribute3f(attrPosition, pn, pp, pp);
      EmitVertex;
      Attribute3f(attrPosition, pn, pp, pn);
      EmitVertex;

      Attribute3f(attrPosition, pp, pn, pp);
      EmitVertex;
      Attribute3f(attrPosition, pp, pn, pn);
      EmitVertex;

      Attribute3f(attrPosition, pn, pn, pp);
      EmitVertex;
      Attribute3f(attrPosition, pn, pn, pn);
      EmitVertex;

      EndAssembly;
    finally
      UnLock;
    end;
  end;
end;

// DodecahedronBuildList
//

procedure DodecahedronBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);
const
  A = 1.61803398875 * 0.3; // (Sqrt(5)+1)/2
  B = 0.61803398875 * 0.3; // (Sqrt(5)-1)/2
  C = 1 * 0.3;
const
  vertices: packed array[0..19] of TAffineVector = ((-A, 0, B), (-A, 0, -B),
    (A, 0, -B), (A, 0, B), (B, -A, 0), (-B, -A, 0), (-B, A, 0), (B, A, 0),
    (0, B, -A), (0, -B, -A), (0, -B, A), (0, B, A), (-C, -C, C), (-C, -C, -C),
    (C, -C, -C), (C, -C, C), (-C, C, C), (-C, C, -C), (C, C, -C), (C, C, C));

  polygons: packed array[0..11] of packed array[0..4]
    of Byte = ((0, 12, 10, 11, 16), (1, 17, 8, 9, 13), (2, 14, 9, 8, 18),
    (3, 19, 11, 10, 15), (4, 14, 2, 3, 15), (5, 12, 0, 1, 13),
    (6, 17, 1, 0, 16), (7, 19, 3, 2, 18), (8, 17, 6, 7, 18), (9, 14, 4, 5, 13),
    (10, 12, 5, 4, 15), (11, 19, 7, 6, 16));

var
  i, j: Integer;
  n, t, bn: TAffineVector;
  faceIndices: PByteArray;
begin
  with AMesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);

      BeginAssembly(mpTRIANGLE_FAN);
      for I := 0 to 11 do
      begin
        faceIndices := @polygons[i, 0];

        n := CalcPlaneNormal(
          vertices[faceIndices^[0]],
          vertices[faceIndices^[1]],
          vertices[faceIndices^[2]]);

        t := VectorSubtract(
          vertices[faceIndices^[0]],
          vertices[faceIndices^[1]]);
        NormalizeVector(t);

        bn := VectorCrossProduct(t, n);

        Attribute3f(attrNormal, n);
        Attribute3f(attrTangent, t);
        Attribute3f(attrBinormal, bn);
        for j := 0 to 4 do
        begin
          Attribute3f(attrPosition, VectorScale(vertices[faceIndices^[j]], ASize));
          EmitVertex;
        end;
        RestartStrip;
      end;
      EndAssembly;
      ComputeTexCoords;
    finally
      UnLock;
    end;
  end;
end;

// IcosahedronBuildList
//

procedure IcosahedronBuildMesh(AMesh: TMeshAtom; ASize: TGLFloat);
const
  A = 0.5;
  B = 0.30901699437; // 1/(1+Sqrt(5))
const
  vertices: packed array[0..11] of TAffineVector = ((0, -B, -A), (0, -B, A),
    (0, B, -A), (0, B, A), (-A, 0, -B), (-A, 0, B), (A, 0, -B), (A, 0, B),
    (-B, -A, 0), (-B, A, 0), (B, -A, 0), (B, A, 0));

  triangles: packed array[0..19] of packed array[0..2]
    of Byte = ((2, 9, 11), (3, 11, 9), (3, 5, 1), (3, 1, 7), (2, 6, 0),
    (2, 0, 4), (1, 8, 10), (0, 10, 8), (9, 4, 5), (8, 5, 4), (11, 7, 6),
    (10, 6, 7), (3, 9, 5), (3, 7, 11), (2, 4, 9), (2, 11, 6), (0, 8, 4),
    (0, 6, 10), (1, 5, 8), (1, 10, 7));

var
  i, j: Integer;
  n, t, bn: TAffineVector;
  faceIndices: PByteArray;
begin
  with AMesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);

      BeginAssembly(mpTRIANGLES);
      for i := 0 to 19 do
      begin
        faceIndices := @triangles[i, 0];

        n := CalcPlaneNormal(
          vertices[faceIndices^[0]],
          vertices[faceIndices^[1]],
          vertices[faceIndices^[2]]);

        t := VectorSubtract(
          vertices[faceIndices^[0]],
          vertices[faceIndices^[1]]);
        NormalizeVector(t);

        bn := VectorCrossProduct(t, n);

        Attribute3f(attrNormal, n);
        Attribute3f(attrTangent, t);
        Attribute3f(attrBinormal, bn);

        for j := 0 to 2 do
        begin
          Attribute3f(attrPosition, VectorScale(vertices[faceIndices^[j]], ASize));
          EmitVertex;
        end;
      end;
      EndAssembly;
      ComputeTexCoords;
    finally
      UnLock;
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLCustomSceneObjectEx'}{$ENDIF}

procedure TGLCustomSceneObjectEx.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  SelectMaterial;
end;

function TGLCustomSceneObjectEx.AxisAlignedBoundingBoxUnscaled(
  const AIncludeChilden: Boolean): TAABB;
var
  I: Integer;
  LAABB: TAABB;
begin
  Result := FBatch.Mesh.AABB;
  //not tested for child objects
  if AIncludeChilden then
  begin
    for I := 0 to Count - 1 do
    begin
      LAABB := Children[I].AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(LAABB, Children[I].Matrix);
      AddAABB(Result, LAABB);
    end;
  end;
end;

function TGLCustomSceneObjectEx.AxisAlignedDimensionsUnscaled: TVector;
var
  V3: TVector3f;
  LAABB: TAABB;
begin
  LAABB := FBatch.Mesh.AABB;
  V3[0] := MaxFloat(Abs(LAABB.min[0]), Abs(LAABB.max[0]));
  V3[1] := MaxFloat(Abs(LAABB.min[1]), Abs(LAABB.max[1]));
  V3[2] := MaxFloat(Abs(LAABB.min[2]), Abs(LAABB.max[2]));
  Result := VectorMake(V3);
end;

procedure TGLCustomSceneObjectEx.BuildMesh;
begin
  FBatch.Changed := True;
  ClearStructureChanged;
  if not IsMainThread and Assigned(Scene) then
    Scene.NotifyChange(Self);
end;

constructor TGLCustomSceneObjectEx.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDeferredDraw];
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
  FBatch.Mesh.Owner := Self;
  FBatch.Mesh.TagName := ClassName;
  FBatch.Material := FMaterial;
  FBatch.PickCallback := DoOnPicked;
end;

procedure TGLCustomSceneObjectEx.SelectMaterial;
var
  LMaterial: TGLAbstractLibMaterial;
begin
  if Assigned(FBatch.Material) and (FBatch.Material <> FMaterial) then
    FBatch.Material.UnRegisterUser(Self);

  if Assigned(MaterialLibrary) then
  begin
    if MaterialLibrary is TGLMaterialLibraryEx then
      LMaterial :=
        TGLMaterialLibraryEx(MaterialLibrary).Materials.GetLibMaterialByName(FMaterial.Material.LibMaterialName)
    else
      LMaterial :=
        TGLMaterialLibrary(MaterialLibrary).Materials.GetLibMaterialByName(FMaterial.Material.LibMaterialName);

    if Assigned(LMaterial) then
    begin
      FBatch.Material := LMaterial;
      LMaterial.RegisterUser(Self);
    end
    else
      FBatch.Material := FMaterial;
  end
  else
  begin
    FBatch.Material := FMaterial;
  end;
end;

procedure TGLCustomSceneObjectEx.Loaded;
begin
  inherited;
  SelectMaterial;
end;

procedure TGLCustomSceneObjectEx.TransformationChanged;
begin
  inherited;
  FBatch.Changed := True;
end;

function TGLCustomSceneObjectEx.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
var
  locRayStart, locRayVector, locPoint, locNormal: TVector;
begin
  if ocStructure in Changes then
    BuildMesh;

  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));

  with FBatch.Mesh do
  begin
    Lock;
    try
      Result := RayCastIntersect(locRayStart, locRayVector, locPoint, locNormal);
      if Result then
      begin
        if Assigned(intersectPoint) then
          SetVector(intersectPoint^, LocalToAbsolute(locPoint));
        if Assigned(intersectNormal) then
          SetVector(intersectNormal^, LocalToAbsolute(locNormal));
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TGLCustomSceneObjectEx.SetShowAABB(const Value: Boolean);
begin
  FBatch.ShowAABB := Value;
end;

procedure TGLCustomSceneObjectEx.DoShowAxes;
begin
  FBatch.ShowAxes := ShowAxes;
end;

procedure TGLCustomSceneObjectEx.ApplyExtras;
begin
  if mesTangents in FMeshExtras then
  begin
    if not FBatch.Mesh.Attributes[attrTangent] then
      FBatch.Mesh.ComputeTangents;
  end
  else
  begin
    FBatch.Mesh.Attributes[attrTangent] := False;
    FBatch.Mesh.Attributes[attrBinormal] := False;
    FBatch.Mesh.Validate;
  end;

  if not (osStreamDraw in ObjectStyle) then
    FBatch.Mesh.WeldVertices;

  if mesAdjacency in FMeshExtras then
  begin
    FBatch.Mesh.MakeAdjacencyElements;
  end;

  if mesFastWireframe in FMeshExtras then
  begin

  end;

  if mesOctreeRayCast in FMeshExtras then
  begin

  end;
end;

destructor TGLCustomSceneObjectEx.Destroy;
begin
  SetScene(nil);
  if Assigned(FBatch.Material) and (FBatch.Material <> FMaterial) then
    FBatch.Material.UnRegisterUser(Self);
  FBatch.Mesh.Free;
  FBatch.InstancesChain.Free;
  FFinishEvent.Free;
  inherited;
end;

procedure TGLCustomSceneObjectEx.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);

  procedure PrepareSelf;
  begin
    if ocStructure in Changes then
    begin
{$IFDEF GLS_SERVICE_CONTEXT}
      if not (osStreamDraw in ObjectStyle) and IsServiceContextAvaible then
      begin
        if not Assigned(FFinishEvent) then
        begin
          FFinishEvent := TFinishTaskEvent.Create;
          AddTaskForServiceContext(BuildMesh, FFinishEvent);
        end
        else if FFinishEvent.WaitFor(0) = wrSignaled then
        begin
          FFinishEvent.ResetEvent;
          AddTaskForServiceContext(BuildMesh, FFinishEvent);
        end;
        exit;
      end
      else
{$ENDIF GLS_SERVICE_CONTEXT}
        BuildMesh;
    end;

    if ARenderSelf then
    begin
      FTransformation := ARci.PipelineTransformation.StackTop;
      ARci.drawList.Add(@FBatch);
    end;
  end;

begin
  PrepareSelf;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

function TGLCustomSceneObjectEx.GetLibMaterialName: string;
begin
  if Assigned(FBatch.Material) then
    Result := FBatch.Material.Name
  else
    Result := '';
end;

function TGLCustomSceneObjectEx.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterial.Material.MaterialLibrary;
end;

function TGLCustomSceneObjectEx.GetPickingMaterial: string;
begin
  if Assigned(FBatch.PickingMaterial) then
    Result := FBatch.PickingMaterial.Name
  else
    Result := '';
end;

function TGLCustomSceneObjectEx.GetShowAABB: Boolean;
begin
  Result := FBatch.ShowAABB;
end;

procedure TGLCustomSceneObjectEx.SetLibMaterialName(const Value: string);
var
  LMaterial: TGLAbstractLibMaterial;
begin
  if Value <> GetLibMaterialName then
  begin
    if Assigned(FBatch.Material) then
      FBatch.Material.UnregisterUser(Self);

    if Assigned(MaterialLibrary) then
    begin
      if MaterialLibrary is TGLMaterialLibraryEx then
        LMaterial :=
          TGLMaterialLibraryEx(MaterialLibrary).Materials.GetLibMaterialByName(Value)
      else
        LMaterial :=
          TGLMaterialLibrary(MaterialLibrary).Materials.GetLibMaterialByName(Value);
      FBatch.Material := LMaterial;
      if Assigned(LMaterial) then
        LMaterial.RegisterUser(Self);
    end
    else
    begin
      FBatch.Material := FMaterial;
    end;
  end;
end;

procedure TGLCustomSceneObjectEx.SetMaterialLibrary(
  const Value: TGLAbstractMaterialLibrary);
begin
  FMaterial.Material.MaterialLibrary := Value;
  SelectMaterial;
end;

procedure TGLCustomSceneObjectEx.SetMeshExtras(const Value: TMeshExtras);
begin
  if FMeshExtras <> Value then
  begin
    FMeshExtras := Value;
    StructureChanged;
  end;
end;

procedure TGLCustomSceneObjectEx.SetPickingMaterial(const Value: string);
var
  LMaterial: TGLAbstractLibMaterial;
begin
  if Value <> GetPickingMaterial then
  begin
    if Assigned(FBatch.PickingMaterial) then
      FBatch.PickingMaterial.UnregisterUser(Self);
    LMaterial := nil;
    if Assigned(MaterialLibrary) then
    begin
      if MaterialLibrary is TGLMaterialLibraryEx then
        LMaterial :=
          TGLMaterialLibraryEx(MaterialLibrary).Materials.GetLibMaterialByName(Value);
      if Assigned(LMaterial) then
        LMaterial.RegisterUser(Self);
    end;
    FBatch.PickingMaterial := LMaterial;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLCustomSceneObjectEx'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLDummyCube'}{$ENDIF}

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

procedure TGLDummyCube.BuildMesh;
begin
  CubeWireframeBuildMesh(FBatch.Mesh, FCubeSize);
  with FBatch.InstancesChain do
  begin
    Lock;
    try
      Clear;
      Attributes[attrColor] := True;
      AttributesType[attrColor] := GLSLType4f;
      with FEdgeColor do
        AttributeLists[attrColor].Add(Red, Green, Blue, Alpha);
    finally
      UnLock;
    end;
  end;

  inherited;
end;

constructor TGLDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDeferredDraw];
  FCubeSize := 1;
  FEdgeColor := TGLColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.InstancesChain := TInstancesChain.Create;
  FBatch.Material := GetOrCreateDummyCubeMaterial;
  FBatch.Transformation := @FTransformation;
  FBatch.Mesh.TagName := ClassName;
  FBatch.PickCallback := DoOnPicked;
end;

destructor TGLDummyCube.Destroy;
begin
  FEdgeColor.Free;
  FBatch.Mesh.Destroy;
  FBatch.InstancesChain.Destroy;
  inherited;
end;

procedure TGLDummyCube.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);

  procedure PrepareSelf;
  begin
    if ARenderSelf then
    begin
      FTransformation := ARci.PipelineTransformation.StackTop;
      if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
        ARci.drawList.Add(@FBatch);
    end;

    if ARenderChildren then
      Self.RenderChildren(0, Count - 1, ARci);
  end;

begin
  if ocStructure in Changes then
  begin
    BuildMesh;
  end;

  if CamInvarianceMode <> cimNone then
    with ARci.PipelineTransformation do
    begin
      Push;
      try
        // prepare
        case CamInvarianceMode of
          cimPosition:
            begin
              ViewMatrix := MatrixMultiply(
                CreateTranslationMatrix(ARci.cameraPosition),
                ARci.PipelineTransformation.ViewMatrix);
            end;
          cimOrientation:
            begin
              // makes the coordinates system more 'intuitive' (Z+ forward)
              ViewMatrix := CreateScaleMatrix(Vector3fMake(1, -1, -1))
            end;
        else
          Assert(False);
        end;
        // Apply local transform
        ModelMatrix := LocalMatrix^;

        PrepareSelf;
      finally
        Pop;
      end;
    end
  else
    PrepareSelf;
end;

function TGLDummyCube.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
begin
  Result := False;
end;

procedure TGLDummyCube.SetCubeSize(const val: TGLFloat);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

procedure TGLDummyCube.SetEdgeColor(const val: TGLColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

procedure TGLDummyCube.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

procedure TGLDummyCube.TransformationChanged;
begin
  inherited;
  FBatch.Changed := True;
end;

function TGLDummyCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := 0.5 * Abs(FCubeSize);
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := 0;
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
    for i := 1 to Count - 1 do
      Result := VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
    ScaleVector(Result, 1 / Count);
  end
  else
    Result := AbsolutePosition;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDummyCube'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLPlane'}{$ENDIF}

function TGLPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := 0.5 * Abs(FWidth);
  Result[1] := 0.5 * Abs(FHeight);
  Result[2] := 0;
end;

procedure TGLPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLPlane) then
  begin
    FWidth := TGLPlane(Source).FWidth;
    FHeight := TGLPlane(Source).FHeight;
    FXOffset := TGLPlane(Source).FXOffset;
    FXScope := TGLPlane(Source).FXScope;
    FXTiles := TGLPlane(Source).FXTiles;
    FYOffset := TGLPlane(Source).FYOffset;
    FYScope := TGLPlane(Source).FYScope;
    FYTiles := TGLPlane(Source).FYTiles;
    FStyle := TGLPlane(Source).FStyle;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

procedure TGLPlane.BuildMesh;
var
  hw, hh, posXFact, posYFact, pX, pY: TGLFloat;
  tx0, tx1, ty0, ty1, texSFact, texTFact: TGLFloat;
  texS, texT: TGLFloat;
  X, Y: Integer;

  procedure CalcPosTexCoord(aX, aY: Integer);
  begin
    texT := aY * texTFact;
    pY := aY * posYFact - hh;
    texS := aX * texSFact;
    pX := aX * posXFact - hw;
  end;

begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;
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

  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType2f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);

      BeginAssembly(mpTRIANGLES);
      Attribute3f(attrNormal, ZVector);
      Attribute3f(attrTangent, XVector);
      Attribute3f(attrBinormal, YVector);

      if psSingleQuad in FStyle then
      begin
        // single quad plane
        Attribute2f(attrTexCoord0, tx1, ty1);
        Attribute2f(attrPosition, hw, hh);
        EmitVertex;

        Attribute2f(attrTexCoord0, tx0, ty1);
        Attribute2f(attrPosition, -hw, hh);
        EmitVertex;

        Attribute2f(attrTexCoord0, tx0, ty0);
        Attribute2f(attrPosition, -hw, -hh);
        EmitVertex;

        EmitVertex;

        Attribute2f(attrTexCoord0, tx1, ty0);
        Attribute2f(attrPosition, hw, -hh);
        EmitVertex;

        Attribute2f(attrTexCoord0, tx1, ty1);
        Attribute2f(attrPosition, hw, hh);
        EmitVertex;
      end
      else
      begin
        // multi-quad plane (actually built from tri-strips)
        texSFact := (tx1 - tx0) / FXTiles;
        texTFact := (ty1 - ty0) / FYTiles;
        posXFact := FWidth / FXTiles;
        posYFact := FHeight / FYTiles;
        for Y := 0 to FYTiles - 1 do
        begin
          for X := 0 to FXTiles - 1 do
          begin
            CalcPosTexCoord(X, Y);
            Attribute2f(attrTexCoord0, texS, texT);
            Attribute2f(attrPosition, pX, pY);
            EmitVertex;
            CalcPosTexCoord(X + 1, Y);
            Attribute2f(attrTexCoord0, texS, texT);
            Attribute2f(attrPosition, pX, pY);
            EmitVertex;
            CalcPosTexCoord(X, Y + 1);
            Attribute2f(attrTexCoord0, texS, texT);
            Attribute2f(attrPosition, pX, pY);
            EmitVertex;
            EmitVertex;
            CalcPosTexCoord(X + 1, Y);
            Attribute2f(attrTexCoord0, texS, texT);
            Attribute2f(attrPosition, pX, pY);
            EmitVertex;
            CalcPosTexCoord(X + 1, Y + 1);
            Attribute2f(attrTexCoord0, texS, texT);
            Attribute2f(attrPosition, pX, pY);
            EmitVertex;
          end;
        end;
      end;
      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  inherited;
end;

constructor TGLPlane.Create(AOwner: TComponent);
begin
  inherited;
  FWidth := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  FStyle := [psSingleQuad, psTileTexture];
end;

// SetHeight
//

procedure TGLPlane.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
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

procedure TGLPlane.SetStyle(const val: TPlaneStyles);
begin
  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetWidth(const aValue: Single);
begin
  if aValue <> FWidth then
  begin
    FWidth := aValue;
    StructureChanged;
  end;
end;

// ScreenRect
//

function TGLPlane.ScreenRect(aBuffer: TGLSceneBuffer): TGLRect;
var
  v: array[0..3] of TVector;
  buf: TGLSceneBuffer;
  hw, hh: TGLFloat;
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
    Result.Left := Round(MinFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
    Result.Right := Round(MaxFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
    Result.Top := Round(MinFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
    Result.Bottom := Round(MaxFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
  end
  else
    FillChar(Result, SizeOf(TGLRect), 0);
end;

// PointDistance
//

function TGLPlane.PointDistance(const aPoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
    AbsoluteDirection);
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLPlane'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLSprite'}{$ENDIF}

function TGLSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := 0.5 * Abs(FWidth);
  Result[1] := 0.5 * Abs(FHeight);
  // Sprites turn with the camera and can be considered to have the same depth
  // as width
  Result[2] := 0.5 * Abs(FWidth);
end;

procedure TGLSprite.Assign(Source: TPersistent);
var
  LSprite: TGLSprite;
begin
  if Source is TGLSprite then
  begin
    LSprite := TGLSprite(Source);
    FWidth := LSprite.FWidth;
    FHeight := LSprite.FHeight;
    FRotation := LSprite.FRotation;
    FModulateColor.Assign(LSprite.FModulateColor);
    FMirrorU := LSprite.FMirrorU;
    FMirrorV := LSprite.FMirrorV;
    FAlign := TGLSprite(Source).FAlign;
  end;
  inherited Assign(Source);
end;

procedure TGLSprite.BuildMesh;
var
  x, y: TAffineVector;
  u0, v0, u1, v1: Integer;
  RM: TMatrix;
begin
  x := AffineVectorMake(FWidth * 0.5, 0, 0);
  y := AffineVectorMake(0, FHeight * 0.5, 0);
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
    RM := CreateRotationMatrix(ZVector, FRotation);
    x := VectorTransform(x, RM);
    y := VectorTransform(y, RM);
  end;

  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);
      DeclareAttribute(attrColor, GLSLType4f);

      BeginAssembly(mpTRIANGLES);

      Attribute4f(attrColor, FModulateColor.Color);
      Attribute3f(attrNormal, 0, 0, 1);
      Attribute3f(attrTangent, VectorNormalize(x));
      Attribute3f(attrBinormal, VectorNormalize(y));
      Attribute2f(attrTexCoord0, u1, v1);
      Attribute3f(attrPosition, x[0] + y[0], x[1] + y[1], x[2] + y[2]);
      EmitVertex;
      Attribute2f(attrTexCoord0, u0, v1);
      Attribute3f(attrPosition, -x[0] + y[0], -x[1] + y[1], -x[2] + y[2]);
      EmitVertex;
      Attribute2f(attrTexCoord0, u1, v0);
      Attribute3f(attrPosition, x[0] - y[0], x[1] - y[1], x[2] - y[2]);
      EmitVertex;
      EmitVertex;
      Attribute2f(attrTexCoord0, u0, v1);
      Attribute3f(attrPosition, -x[0] + y[0], -x[1] + y[1], -x[2] + y[2]);
      EmitVertex;
      Attribute2f(attrTexCoord0, u0, v0);
      Attribute3f(attrPosition, -x[0] - y[0], -x[1] - y[1], -x[2] - y[2]);
      EmitVertex;

      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  inherited;
end;

constructor TGLSprite.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osNoVisibilityCulling];
  FWidth := 1;
  FHeight := 1;
  FRotation := 0;
  FMirrorU := False;
  FMirrorV := False;
  FAlign := alSpherical;
  FModulateColor := TGLColor.CreateInitialized(Self, clrGray80);
  FModulateColor.OnNotifyChange := OnColorChange;
end;

destructor TGLSprite.Destroy;
begin
  FModulateColor.Destroy;
  inherited;
end;

procedure TGLSprite.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  M: TMatrix;
begin
  if ARenderSelf then
  begin
    if ocStructure in Changes then
    begin
      BuildMesh;
    end;

    ARci.PipelineTransformation.Push;
    M := ARci.PipelineTransformation.ModelViewMatrix;
    case FAlign of
      alSpherical:
        begin
          M[0, 0] := Scale.X;
          M[0, 1] := 0;
          M[0, 2] := 0;
          M[1, 0] := 0;
          M[1, 1] := Scale.Y;
          M[1, 2] := 0;
          M[2, 0] := 0;
          M[2, 1] := 0;
          M[2, 2] := 1;
        end;
      alCylindrical:
        begin
          M[0, 0] := 1;
          M[0, 1] := 0;
          M[0, 2] := 0;
          M[2, 0] := 0;
          M[2, 1] := 0;
          M[2, 2] := 1;
        end;
    end;
    ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;
    ARci.PipelineTransformation.ViewMatrix := M;
    FTransformation := ARci.PipelineTransformation.StackTop;
    ARci.PipelineTransformation.Pop;

    ARci.drawList.Add(@FBatch);
  end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TGLSprite.GetAlphaChannel: Single;
begin
  Result := FModulateColor.Alpha;
end;

procedure TGLSprite.OnColorChange(Sender: TObject);
begin
  StructureChanged;
end;

function TGLSprite.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
var
  i1, i2, absPos: TVector;
begin
  SetVector(absPos, AbsolutePosition);
  if RayCastSphereIntersect(rayStart, rayVector, absPos, BoundingSphereRadius,
    i1, i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, i1);
    if Assigned(intersectNormal) then
    begin
      SubtractVector(i1, absPos);
      NormalizeVector(i1);
      SetVector(intersectNormal^, i1);
    end;
  end
  else
    Result := False;
end;

// SetWidth
//

procedure TGLSprite.SetWidth(const val: TGLFloat);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TGLSprite.SetAlphaChannel(const Value: Single);
begin
  FModulateColor.Alpha := Value;
end;

procedure TGLSprite.SetHeight(const val: TGLFloat);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//

procedure TGLSprite.SetRotation(const val: TGLFloat);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

// SetMirrorU
//

procedure TGLSprite.SetMirrorU(const val: Boolean);
begin
  FMirrorU := val;
  NotifyChange(Self);
end;

// SetMirrorV
//

procedure TGLSprite.SetMirrorV(const val: Boolean);
begin
  FMirrorV := val;
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

procedure TGLSprite.SetSquareSize(const size: TGLFloat);
begin
  FWidth := size;
  FHeight := size;
  NotifyChange(Self);
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLSprite'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLPoints'}{$ENDIF}

// Create
//

constructor TGLPointParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMinSize := 0;
  FMaxSize := 128;
  FFadeTresholdSize := 1;
  FDistanceAttenuation := TGLCoordinates.CreateInitialized(Self, XHmgVector,
    csVector);
end;

// Destroy
//

destructor TGLPointParameters.Destroy;
begin
  FDistanceAttenuation.Free;
  inherited;
end;

// DefineProperties
//

procedure TGLPointParameters.DefineProperties(Filer: TFiler);
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

// SetEnabled
//

procedure TGLPointParameters.SetDistanceAttenuation(const val: TGLCoordinates);
begin
  FDistanceAttenuation.Assign(val);
end;

procedure TGLPointParameters.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

// SetMinSize
//

procedure TGLPointParameters.SetMinSize(const val: Single);
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

procedure TGLPointParameters.SetMaxSize(const val: Single);
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

procedure TGLPointParameters.SetFadeTresholdSize(const val: Single);
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

procedure TGLPoints.BuildMesh;
var
  bColor: Boolean;
  LColor: TVector4f;
begin
  bColor := (FColors.Count > 1) and (FColors.Count = FPositions.Count);
  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      if bColor then
        DeclareAttribute(attrColor, GLSLType4f);

      BeginAssembly(mpPOINTS);

      AttributeList(attrPosition, FPositions);
      if bColor then
        AttributeList(attrColor, FColors);

      EndAssembly;
    finally
      UnLock;
    end;
  end;

  with FBatch.InstancesChain do
  begin
    Lock;
    try
      Clear;
      if not bColor then
      begin
        Attributes[attrColor] := True;
        AttributesType[attrColor] := GLSLType4f;
        if FColors.Count = 1 then
          LColor := FColors[0]
        else
          LColor := clrWhite;
        AttributeLists[attrColor].Add(LColor[0], LColor[1], LColor[2], LColor[3]);
      end;
    finally
      UnLock;
    end;
  end;

  inherited;
end;

constructor TGLPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBatch.InstancesChain := TInstancesChain.Create;

  FPositions := TAffineVectorList.Create;
  FPositions.Add(NullVector);
  FColors := TVectorList.Create;
  FPointParameters := TGLPointParameters.Create(Self);
end;

destructor TGLPoints.Destroy;
begin
  FColors.Free;
  FPositions.Free;
  FPointParameters.Destroy;
  inherited;
end;

procedure TGLPoints.Loaded;
begin
  inherited;
  UpdateMaterial;
end;

procedure TGLPoints.NotifyChange(Sender: TObject);
begin
  inherited;
  if Sender = FPointParameters then
    UpdateMaterial;
end;

procedure TGLPoints.Assign(Source: TPersistent);
begin
  if Source is TGLPoints then
  begin
    FPositions.Assign(TGLPoints(Source).FPositions);
    FColors.Assign(TGLPoints(Source).FColors);
    StructureChanged;
  end;
  inherited Assign(Source);
end;

procedure TGLPoints.SetPointParameters(const Value: TGLPointParameters);
begin
  FPointParameters := Value;
end;

procedure TGLPoints.SetPositions(const val: TAffineVectorList);
begin
  FPositions.Assign(val);
  StructureChanged;
end;

procedure TGLPoints.SetSize(const Value: Single);
begin
  FSize := Value;
  UpdateMaterial;
end;

procedure TGLPoints.SetStatic(const Value: Boolean);
begin
  FStatic := Value;
  if FStatic then
    ObjectStyle := ObjectStyle - [osStreamDraw]
  else
    ObjectStyle := ObjectStyle + [osStreamDraw];
end;

procedure TGLPoints.SetStyle(const Value: TGLPointStyle);
begin
  FStyle := Value;
  UpdateMaterial;
end;

function TGLPoints.StoreSize: Boolean;
begin
  Result := FSize <> 1.0;
end;

procedure TGLPoints.UpdateMaterial;
begin
  if FBatch.Material is TGLLibMaterialEx then
    with TGLLibMaterialEx(FBatch.Material).FixedFunction do
    begin
      PointProperties.Enabled := True;
      PointProperties.Size := FSize;
      DepthProperties.DepthWrite := not FNoZWrite;

      case FStyle of
        psSquare:
          begin
            MaterialOptions := [moNoLighting];
            PointProperties.Smooth := False;
            BlendingMode := bmOpaque;
          end;

        psRound:
          begin
            MaterialOptions := [moNoLighting];
            PointProperties.Smooth := True;
            BlendingMode := bmAlphaTest50;
          end;

        psSmooth:
          begin
            MaterialOptions := [moNoLighting];
            PointProperties.Smooth := True;
            BlendingMode := bmCustom;
            BlendingParams.UseAlphaFunc := True;
            BlendingParams.AlphaFunctType := cfNotEqual;
            BlendingParams.AlphaFuncRef := 0.0;
            BlendingParams.UseBlendFunc := True;
            BlendingParams.BlendFuncSFactor := bfSrcAlpha;
            BlendingParams.BlendFuncDFactor := bfOneMinusSrcAlpha;
          end;

        psSmoothAdditive:
          begin
            MaterialOptions := [moNoLighting];
            PointProperties.Smooth := True;
            BlendingMode := bmCustom;
            BlendingParams.UseAlphaFunc := True;
            BlendingParams.AlphaFunctType := cfNotEqual;
            BlendingParams.AlphaFuncRef := 0.0;
            BlendingParams.UseBlendFunc := True;
            BlendingParams.BlendFuncSFactor := bfSrcAlpha;
            BlendingParams.BlendFuncDFactor := bfOne;
          end;

        psSquareAdditive:
          begin
            MaterialOptions := [moNoLighting];
            PointProperties.Smooth := False;
            BlendingMode := bmAdditive;
          end;
      end;

      if FPointParameters.Enabled then
      begin
        PointProperties.MinSize := FPointParameters.MinSize;
        PointProperties.MaxSize := FPointParameters.MaxSize;
        PointProperties.FadeTresholdSize := FPointParameters.FadeTresholdSize;
        PointProperties.DistanceAttenuation := FPointParameters.DistanceAttenuation;
      end;
    end;
end;

procedure TGLPoints.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
  StructureChanged;
end;

procedure TGLPoints.SetLibMaterialName(const Value: string);
begin
  inherited SetLibMaterialName(Value);
  UpdateMaterial;
end;

procedure TGLPoints.SetNoZWrite(const Value: Boolean);
begin
  FNoZWrite := Value;
  UpdateMaterial;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLLineBase'}{$ENDIF}

// Create
//

constructor TGLLineBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := TGLColor.Create(Self);
  FLineColor.Initialize(clrWhite);
  FLinePattern := $FFFF;
  FAntiAliased := False;
  FLineWidth := 1.0;
  FMaterialEx := GetInternalMaterialLibrary.Materials.Add;
  FBatch.Material := FMaterialEx;
  FMaterialEx.FixedFunction.MaterialOptions := [moIgnoreFog, moNoLighting];
  FMaterialChanged := True;
end;

// Destroy
//

destructor TGLLineBase.Destroy;
begin
  FLineColor.Free;
  inherited Destroy;
end;

procedure TGLLineBase.Loaded;
begin
  inherited;
  UpdateMaterial;
end;

procedure TGLLineBase.NotifyChange(Sender: TObject);
begin
  if Sender = FLineColor then
    StructureChanged;
  inherited;
end;

// SetLineColor
//

procedure TGLLineBase.SetLibMaterialName(const Value: string);
begin
  inherited SetLibMaterialName(Value);
  UpdateMaterial;
end;

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
    NotifyChange(Self);
  end;
end;

// SetLineWidth
//

procedure TGLLineBase.SetLineWidth(const val: Single);
begin
  if FLineWidth <> val then
  begin
    FLineWidth := val;
    NotifyChange(Self);
  end;
end;

// StoreLineWidth
//

function TGLLineBase.StoreLineWidth: Boolean;
begin
  Result := (FLineWidth <> 1.0);
end;

procedure TGLLineBase.UpdateMaterial;
begin
  if not (FBatch.Material is TGLLibMaterialEx) then
    FBatch.Material := FMaterialEx;

  with TGLLibMaterialEx(FBatch.Material).FixedFunction do
  begin
    LineProperties.Enabled := True;
    LineProperties.Smooth := FAntiAliased;
    LineProperties.Width := FLineWidth;
    LineProperties.StipplePattern := FLinePattern;
    if FLinePattern <> $FFFF then
      LineProperties.StippleFactor := 1
    else
      LineProperties.StippleFactor := 0;
  end;
  FMaterialChanged := False;
end;

// SetAntiAliased
//

procedure TGLLineBase.SetAntiAliased(const val: Boolean);
begin
  if FAntiAliased <> val then
  begin
    FAntiAliased := val;
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLLinesNode'}{$ENDIF}

// Create
//

constructor TGLLinesNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := TGLColor.Create(Self);
  FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TGLLines).DefaultNodeColor.Color);
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

procedure TGLLinesNode.SetColor(const val: TGLColor);
begin
  FColor.Assign(val);
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
    .DefaultNodeColor.Color, FColor.Color);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLLinesNodes'}{$ENDIF}

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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLNodedLines'}{$ENDIF}

// Create
//

constructor TGLNodedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TGLLinesNodes.Create(Self);
  FDefaultNodeColor := TGLColor.Create(Self);
  FDefaultNodeColor.Initialize(clrBlue);
  FDefaultNodeColor.OnNotifyChange := OnNodeColorChanged;
  FOldNodeColor := clrBlue;
  FNodesAspect := lnaAxes;
  FNodeSize := 1;

  FBatch.InstancesChain := TInstancesChain.Create;
  FNodeBatch.Mesh := TMeshAtom.Create;
  FNodeBatch.Mesh.TagName := ClassName + '_' + FNodes.ClassName;
  FNodeBatch.InstancesChain := TInstancesChain.Create;
end;

// Destroy
//

destructor TGLNodedLines.Destroy;
begin
  FNodes.Free;
  FDefaultNodeColor.Free;
  FNodeBatch.Mesh.Destroy;
  FNodeBatch.InstancesChain.Destroy;
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
  FDefaultNodeColor.Color := Value.Color;
  StructureChanged;
end;

// OnNodeColorChanged
//

procedure TGLNodedLines.OnNodeColorChanged(Sender: TObject);
var
  i: Integer;
begin
  // update color for nodes...
  for i := 0 to Nodes.Count - 1 do
    if VectorEquals(TGLLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
      TGLLinesNode(Nodes[i]).Color.Assign(FDefaultNodeColor);
  SetVector(FOldNodeColor, FDefaultNodeColor.Color);
end;

// SetNodes
//

procedure TGLNodedLines.SetNodes(const aNodes: TGLLinesNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

// SetNodeSize
//

procedure TGLNodedLines.SetNodeSize(const val: Single);
begin
  if val <= 0 then
    FNodeSize := 1
  else
    FNodeSize := val;
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
    FDefaultNodeColor.Color := TGLNodedLines(Source).FDefaultNodeColor.Color;
    FNodeSize := TGLNodedLines(Source).FNodeSize;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//

function TGLNodedLines.AxisAlignedDimensionsUnscaled: TVector;
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

procedure TGLNodedLines.BuildNodeMesh;
var
  I: Integer;
begin
  case NodesAspect of
    lnaAxes:
      AxesBuildMesh(FNodeBatch.Mesh, FNodeSize * 0.5);
    lnaCube:
      CubeWireframeBuildMesh(FNodeBatch.Mesh, FNodeSize);
    lnaDodecahedron:
      DodecahedronBuildMesh(FNodeBatch.Mesh, FNodeSize);
  end;

  with FNodeBatch.InstancesChain do
  begin
    Lock;
    try
      Clear;
      if FNodesAspect <> lnaInvisible then
      begin
        if FNodesAspect <> lnaAxes then
        begin
          Attributes[attrColor] := True;
          AttributesType[attrColor] := GLSLType4f;
          for I := 0 to Nodes.Count - 1 do
            with TGLLinesNode(Nodes[I]).Color do
              AttributeLists[attrColor].Add(Red, Green, Blue, Alpha);
        end;

        TransformationEnabled := True;
        for I := 0 to Nodes.Count - 1 do
          with TGLLinesNode(Nodes[I]) do
            Transformations.Add(@FNodeTransformation);
      end;
    finally
      UnLock;
    end;
  end;

  FNodeBatch.Material := GetOrCreateDummyCubeMaterial;
end;

// AddNode (coords)
//

procedure TGLNodedLines.AddNode(const coords: TGLCoordinates);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

// AddNode (xyz)
//

procedure TGLNodedLines.AddNode(const X, Y, Z: TGLFloat);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//

procedure TGLNodedLines.AddNode(const Value: TVector);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

// AddNode (affine vector)
//

procedure TGLNodedLines.AddNode(const Value: TAffineVector);
var
  LNode: TGLNode;
begin
  LNode := Nodes.Add;
  LNode.AsVector := VectorMake(Value);
  StructureChanged;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLLines'}{$ENDIF}

// Create
//

constructor TGLLines.Create(AOwner: TComponent);
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

destructor TGLLines.Destroy;
begin
  FNURBSKnots.Free;
  inherited Destroy;
end;

procedure TGLLines.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  I: Integer;
  M: TMatrix;
begin
  inherited DoRender(ARci, ARenderSelf, ARenderChildren);

  if FMaterialChanged then
    UpdateMaterial;

  if (FNodesAspect <> lnaInvisible) and ARenderSelf then
  begin
    if Nodes.Count > 0 then
    begin
      for I := Nodes.Count - 1 downto 0 do
        with TGLLinesNode(Nodes[I]) do
        begin
          FNodeTransformation := FBatch.Transformation^;
          M := MatrixMultiply(
            PMatrix(@FNodeTransformation.FModelMatrix[0])^,
            CreateTranslationMatrix(AsAffineVector));
          SetMatrix(FNodeTransformation.FModelMatrix, M);
          FNodeTransformation.FStates := cAllStatesChanged;
        end;
      ARci.drawList.Add(@FNodeBatch);
    end;
  end;
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

procedure TGLLines.SetOptions(const val: TLinesOptions);
begin
  FOptions := val;
  StructureChanged;
end;

// SetSplineMode
//

procedure TGLLines.SetSplineMode(const val: TLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

// SetNURBSOrder
//

procedure TGLLines.SetNURBSOrder(const val: Integer);
begin
  if val <> FNURBSOrder then
  begin
    FNURBSOrder := val;
    StructureChanged;
  end;
end;

// SetNURBSTolerance
//

procedure TGLLines.SetNURBSTolerance(const val: Single);
begin
  if val <> FNURBSTolerance then
  begin
    FNURBSTolerance := val;
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

procedure TGLLines.BuildMesh;
var
  LMode: TLineSplineMode;
  I, NC, n: Integer;
  vertexColor: TVector;
  A, B, C: TGLFloat;
  f, invC, u, u0, u1: Single;
  Spline: TCubicSpline;
  BzSpline: TBezierSpline;
begin
  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      if Nodes.Count > 1 then
      begin
        DeclareAttribute(attrPosition, GLSLType3f);
        if loUseNodeColorForLines in Options then
          DeclareAttribute(attrColor, GLSLType4f);
        if loTextureCoord in Options then
          DeclareAttribute(attrTexCoord0, GLSLType1f);

        LMode := FSplineMode;
        if (LMode = lsmBezierSpline) and (Nodes.Count < 3) then
          LMode := lsmLines;
        invC := 1 / Nodes.Count;

        case LMode of
          lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve:
            BeginAssembly(mpLINE_STRIP);
          lsmSegments:
            BeginAssembly(mpLINES);
          lsmLoop:
            BeginAssembly(mpLINE_LOOP);
        end;

        if (FDivision < 2)
          or (LMode in [lsmLines, lsmSegments, lsmLoop]) then
        begin
          // standard line(s)
          NC := Nodes.Count;
          if LMode = lsmSegments then
            NC := 2 * (NC div 2);
          if loUseNodeColorForLines in Options then
          begin
            // node color interpolation
            for I := 0 to NC - 1 do
              with TGLLinesNode(Nodes[i]) do
              begin
                if loTextureCoord in Options then
                  Attribute1f(attrTexCoord0, i * InvC);
                Attribute4f(attrColor, Color.Color);
                Attribute3f(attrPosition, X, Y, Z);
                EmitVertex;
              end;
          end
          else
          begin
            // single color
            for i := 0 to NC - 1 do
              with Nodes[i] do
              begin
                if loTextureCoord in Options then
                  Attribute1f(attrTexCoord0, i * InvC);
                Attribute3f(attrPosition, X, Y, Z);
                EmitVertex;
              end;
          end;
        end
        else if LMode = lsmCubicSpline then
        begin
          // cubic spline
          Spline := Nodes.CreateNewCubicSpline;
          try
            f := 1 / FDivision;
            for I := 0 to (Nodes.Count - 1) * FDivision do
            begin
              u := I * f;
              Spline.SplineXYZ(u, A, B, C);
              if loUseNodeColorForLines in Options then
              begin
                n := (i div FDivision);
                if n < Nodes.Count - 1 then
                  VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                    TGLLinesNode(Nodes[n + 1]).Color.Color, (i mod FDivision) * f,
                    vertexColor)
                else
                  SetVector(vertexColor,
                    TGLLinesNode(Nodes[Nodes.Count - 1]).Color.Color);
                Attribute4f(attrColor, vertexColor);
              end;
              if loTextureCoord in Options then
                Attribute1f(attrTexCoord0, u * InvC);
              Attribute3f(attrPosition, A, B, C);
              EmitVertex;
            end;
          finally
            Spline.Free;
          end;
        end
        else if LMode = lsmBezierSpline then
        begin
          // bezier spline
          BzSpline := Nodes.CreateNewBezierSpline;
          try
            f := 1 / FDivision;
            for I := 0 to FDivision do
            begin
              u := i * f;
              BzSpline.SplineXYZ(u, A, B, C);
              if loUseNodeColorForLines in Options then
              begin
                u := 1 - u;
                n := Floor(u * Nodes.Count);
                if n < Nodes.Count - 1 then
                begin
                  u0 := n * invC;
                  u1 := (1 + n) * invC;
                  VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                    TGLLinesNode(Nodes[n + 1]).Color.Color, (u - u0) / (u1 - u0),
                    vertexColor);
                end
                else
                  SetVector(vertexColor,
                    TGLLinesNode(Nodes[Nodes.Count - 1]).Color.Color);
                Attribute4f(attrColor, vertexColor);
              end;
              if loTextureCoord in Options then
                Attribute1f(attrTexCoord0, u);
              Attribute3f(attrPosition, A, B, C);
              EmitVertex;
            end;
          finally
            BzSpline.Free;
          end;
        end
        else if LMode = lsmNURBSCurve then
        begin
{$MESSAGE Hint 'lsmNURBSCurve mode not yet implemented for TGLLines' }
        end;

        EndAssembly;
      end;
    finally
      UnLock;
    end;
  end;

  with FBatch.InstancesChain do
  begin
    Lock;
    try
      Clear;
      if not (loUseNodeColorForLines in Options) then
      begin
        Attributes[attrColor] := True;
        AttributesType[attrColor] := GLSLType4f;
        with FLineColor do
          AttributeLists[attrColor].Add(Red, Green, Blue, Alpha);
      end;
    finally
      UnLock;
    end;
  end;

  BuildNodeMesh;

  inherited;
end;
{
// BuildList
//

procedure BuildList(var rci: TRenderContextInfo);
var
  i, n: Integer;
  A, B, C: TGLFloat;
  f: Single;
  Spline: TCubicSpline;
  vertexColor: TVector;
  nodeBuffer: array of TAffineVector;
  colorBuffer: array of TVector;
  nurbsRenderer: PGLUNurbs;
begin
  if Nodes.Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle(rci);
    if rci.bufferDepthTest then
      rci.GLStates.Enable(stDepthTest);
    if loColorLogicXor in Options then
    begin
      rci.GLStates.Enable(stColorLogicOp);
      rci.GLStates.LogicOpMode := loXOr;
    end;
    // Set up the control point buffer for Bezier splines and NURBS curves.
    // If required this could be optimized by storing a cached node buffer.
    if (FSplineMode = lsmBezierSpline) or (FSplineMode = lsmNURBSCurve) then
    begin
      SetLength(nodeBuffer, Nodes.Count);
      SetLength(colorBuffer, Nodes.Count);
      for i := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[i]) do
        begin
          nodeBuffer[i] := AsAffineVector;
          colorBuffer[i] := Color.Color;
        end;
    end;

    if FSplineMode = lsmBezierSpline then
    begin
      // map evaluator
      rci.GLStates.PushAttrib([sttEval]);
      GL.Enable(GL_MAP1_VERTEX_3);
      GL.Enable(GL_MAP1_COLOR_4);

      GL.Map1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
      GL.Map1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @colorBuffer[0]);
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
        GL.Begin_(GL_LINES)
      else if FSplineMode = lsmLoop then
        GL.Begin_(GL_LINE_LOOP)
      else
        GL.Begin_(GL_LINE_STRIP);
      if (FDivision < 2) or (FSplineMode in [lsmLines, lsmSegments,
        lsmLoop]) then
      begin
        // standard line(s), draw directly
        if loUseNodeColorForLines in Options then
        begin
          // node color interpolation
          for i := 0 to Nodes.Count - 1 do
            with TGLLinesNode(Nodes[i]) do
            begin
              GL.Color4fv(Color.AsAddress);
              GL.Vertex3f(X, Y, Z);
            end;
        end
        else
        begin
          // single color
          for i := 0 to Nodes.Count - 1 do
            with Nodes[i] do
              GL.Vertex3f(X, Y, Z);
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
                VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                  TGLLinesNode(Nodes[n + 1]).Color.Color, (i mod FDivision) * f,
                  vertexColor)
              else
                SetVector(vertexColor, TGLLinesNode(Nodes[Nodes.Count - 1])
                  .Color.Color);
              GL.Color4fv(@vertexColor);
            end;
            GL.Vertex3f(A, B, C);
          end;
        finally
          Spline.Free;
        end;
      end
      else if FSplineMode = lsmBezierSpline then
      begin
        f := 1 / FDivision;
        for i := 0 to FDivision do
          GL.EvalCoord1f(i * f);
      end;
      GL.End_;
    end;
    rci.GLStates.Disable(stColorLogicOp);

    if FSplineMode = lsmBezierSpline then
      rci.GLStates.PopAttrib;
    if Length(nodeBuffer) > 0 then
    begin
      SetLength(nodeBuffer, 0);
      SetLength(colorBuffer, 0);
    end;

    if FNodesAspect <> lnaInvisible then
    begin
      if not rci.ignoreBlendingRequests then
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;

      for i := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[i]) do
          DrawNode(rci, X, Y, Z, Color);
    end;
  end;
end;
}

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLCube'}{$ENDIF}

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

procedure TGLCube.BuildMesh;
var
  hw, hh, hd: Single;
begin
  hw := FCubeSize[0] * 0.5;
  hh := FCubeSize[1] * 0.5;
  hd := FCubeSize[2] * 0.5;

  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);
      DeclareAttribute(attrColor, GLSLType4f);

      BeginAssembly(mpTRIANGLES);

      if cpFront in FParts then
      begin
        Attribute3f(attrNormal, 0, 0, 1);
        Attribute3f(attrTangent, 1, 0, 0);
        Attribute3f(attrBinormal, 0, 1, 0);
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, hw, hh, hd);
        Attribute4f(attrColor, 1, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, -hw * 1, hh * 1, hd);
        Attribute4f(attrColor, 0, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, -hw, -hh, hd);
        Attribute4f(attrColor, 0, 0, 1, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, hw * 1, -hh * 1, hd);
        Attribute4f(attrColor, 1, 0, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, hw, hh, hd);
        Attribute4f(attrColor, 1, 1, 1, 1);
        EmitVertex;
      end;

      if cpBack in FParts then
      begin
        Attribute3f(attrNormal, 0, 0, -1);
        Attribute3f(attrTangent, -1, 0, 0);
        Attribute3f(attrBinormal, 0, -1, 0);
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, hw, hh, -hd);
        Attribute4f(attrColor, 1, 1, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, hw * 1, -hh * 1, -hd);
        Attribute4f(attrColor, 1, 0, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, -hw, -hh, -hd);
        Attribute4f(attrColor, 0, 0, 0, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, -hw * 1, hh * 1, -hd);
        Attribute4f(attrColor, 0, 1, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, hw, hh, -hd);
        Attribute4f(attrColor, 1, 1, 0, 1);
        EmitVertex;
      end;

      if cpLeft in FParts then
      begin
        Attribute3f(attrNormal, -1, 0, 0);
        Attribute3f(attrTangent, 0, 0, 1);
        Attribute3f(attrBinormal, 0, -1, 0);
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, -hw, hh, hd);
        Attribute4f(attrColor, 0, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, -hw, hh * 1, -hd * 1);
        Attribute4f(attrColor, 0, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, -hw, -hh, -hd);
        Attribute4f(attrColor, 0, 0, 0, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, -hw, -hh * 1, hd * 1);
        Attribute4f(attrColor, 0, 0, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, -hw, hh, hd);
        Attribute4f(attrColor, 0, 1, 1, 1);
        EmitVertex;
      end;

      if cpRight in FParts then
      begin
        Attribute3f(attrNormal, 1, 0, 0);
        Attribute3f(attrTangent, 0, 0, -1);
        Attribute3f(attrBinormal, 0, 1, 0);
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, hw, hh, hd);
        Attribute4f(attrColor, 1, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, hw, -hh * 1, hd * 1);
        Attribute4f(attrColor, 1, 0, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, hw, -hh, -hd);
        Attribute4f(attrColor, 1, 0, 0, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, hw, hh * 1, -hd * 1);
        Attribute4f(attrColor, 1, 1, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, hw, hh, hd);
        Attribute4f(attrColor, 1, 1, 1, 1);
        EmitVertex;
      end;

      if cpTop in FParts then
      begin
        Attribute3f(attrNormal, 0, 1, 0);
        Attribute3f(attrTangent, 1, 0, 0);
        Attribute3f(attrBinormal, 0, 0, 1);
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, -hw, hh, -hd);
        Attribute4f(attrColor, 0, 1, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, -hw * 1, hh, hd * 1);
        Attribute4f(attrColor, 0, 1, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, hw, hh, hd);
        Attribute4f(attrColor, 1, 1, 1, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, hw * 1, hh, -hd * 1);
        Attribute4f(attrColor, 1, 1, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, -hw, hh, -hd);
        Attribute4f(attrColor, 0, 1, 0, 1);
        EmitVertex;
      end;

      if cpBottom in FParts then
      begin
        Attribute3f(attrNormal, 0, -1, 0);
        Attribute3f(attrTangent, -1, 0, 0);
        Attribute3f(attrBinormal, 0, 0, -1);
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, -hw, -hh, -hd);
        Attribute4f(attrColor, 0, 0, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 0);
        Attribute3f(attrPosition, hw * 1, -hh, -hd * 1);
        Attribute4f(attrColor, 1, 0, 0, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 1, 1);
        Attribute3f(attrPosition, hw, -hh, hd);
        Attribute4f(attrColor, 1, 0, 1, 1);
        EmitVertex;
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 1);
        Attribute3f(attrPosition, -hw * 1, -hh, hd * 1);
        Attribute4f(attrColor, 0, 0, 1, 1);
        EmitVertex;
        Attribute2f(attrTexCoord0, 0, 0);
        Attribute3f(attrPosition, -hw, -hh, -hd);
        Attribute4f(attrColor, 0, 0, 0, 1);
        EmitVertex;
      end;

      EndAssembly;
      if FNormalDirection = ndInside then
        FlipFaces;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  inherited;
end;

constructor TGLCube.Create(AOwner: TComponent);
begin
  inherited;
  FCubeSize := XYZVector;
  FParts := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
end;

// SetCubeWidth
//

procedure TGLCube.SetCubeWidth(const aValue: Single);
begin
  if aValue <> FCubeSize[0] then
  begin
    FCubeSize[0] := aValue;
    StructureChanged;
  end;
end;

// SetCubeHeight
//

procedure TGLCube.SetCubeHeight(const aValue: Single);
begin
  if aValue <> FCubeSize[1] then
  begin
    FCubeSize[1] := aValue;
    StructureChanged;
  end;
end;

// SetCubeDepth
//

procedure TGLCube.SetCubeDepth(const aValue: Single);
begin
  if aValue <> FCubeSize[2] then
  begin
    FCubeSize[2] := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TGLCube.SetParts(aValue: TCubeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TGLCube.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// DefineProperties
//

procedure TGLCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize[0] <> 1) or (FCubeSize[1] <> 1) or (FCubeSize[2] <> 1));
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

{$IFDEF GLS_REGION}{$ENDREGION 'TGLCube'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLSphere'}{$ENDIF}

procedure TGLSphere.BuildMesh;
var
  LCapMesh: TMeshAtom;
  V1, V2, N1, T1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  I, J: Integer;
begin
  // common settings
  AngTop := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart := DegToRad(1.0 * FStart);
  AngStop := DegToRad(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;
  LCapMesh := nil;

  // main body
  FBatch.Mesh.Lock;
  try
    with FBatch.Mesh do
    begin
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);

      Phi := AngTop;
      Phi2 := Phi - StepV;

      uTexFactor := 1 / FSlices;
      vTexFactor := 1 / FStacks;

      BeginAssembly(mpTRIANGLE_STRIP);
      for j := 0 to FStacks - 1 do
      begin
        Theta := AngStart;
        SinCos(Phi, SinP, CosP);
        SinCos(Phi2, SinP2, CosP2);

        vTexCoord0 := 1 - j * vTexFactor;
        vTexCoord1 := 1 - (j + 1) * vTexFactor;

        for i := 0 to FSlices do
        begin
          SinCos(Theta, SinT, CosT);
          V1[0] := CosP * SinT;
          V1[1] := SinP;
          V1[2] := CosP * CosT;

          V2[0] := CosP2 * SinT;
          V2[1] := SinP2;
          V2[2] := CosP2 * CosT;

          uTexCoord := i * uTexFactor;
          Attribute2f(attrTexCoord0, uTexCoord, vTexCoord0);
          Attribute3f(attrNormal, V1);
          T1 := VectorCrossProduct(V1, YVector);
          Attribute3f(attrTangent, T1);
          T1 := VectorCrossProduct(T1, V1);
          Attribute3f(attrBinormal, T1);
          ScaleVector(V1, Radius);
          Attribute3f(attrPosition, V1);
          EmitVertex;

          Attribute2f(attrTexCoord0, uTexCoord, vTexCoord1);
          Attribute3f(attrNormal, V2);
          T1 := VectorCrossProduct(V2, YVector);
          Attribute3f(attrTangent, T1);
          T1 := VectorCrossProduct(T1, V2);
          Attribute3f(attrBinormal, T1);
          ScaleVector(V2, Radius);
          Attribute3f(attrPosition, V2);
          EmitVertex;

          Theta := Theta + StepH;
        end;
        RestartStrip;
        Phi := Phi2;
        Phi2 := Phi2 - StepV;
      end;
      EndAssembly;
      Triangulate;
      SplitVertices;
    end;

    // top cap
    if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
    begin
      LCapMesh := TMeshAtom.Create;
      SinCos(AngTop, SinP, CosP);
      with LCapMesh do
      begin
        Lock;
        try
          Clear;
          DeclareAttribute(attrPosition, GLSLType3f);
          DeclareAttribute(attrNormal, GLSLType3f);
          DeclareAttribute(attrTangent, GLSLType3f);
          DeclareAttribute(attrBinormal, GLSLType3f);
          DeclareAttribute(attrTexCoord0, GLSLType2f);

          BeginAssembly(mpTRIANGLE_FAN);

          Attribute2f(attrTexCoord0, 0.5, 0.5);
          Attribute3f(attrNormal, 0, 1, 0);
          Attribute3f(attrTangent, 1, 0, 0);
          Attribute3f(attrBinormal, 0, 0, 1);
          if FTopCap = ctCenter then
            Attribute3f(attrPosition, 0, 0, 0)
          else
          begin
            Attribute3f(attrPosition, 0, SinP * Radius, 0);
            N1 := YVector;
            T1 := XVector;
          end;
          EmitVertex;
          Theta := AngStart;
          for I := 0 to FSlices do
          begin
            SinCos(Theta, SinT, CosT);
            V1[0] := CosP * SinT;
            V1[1] := SinP;
            V1[2] := CosP * CosT;
            if FTopCap = ctCenter then
            begin
              N1 := VectorPerpendicular(YVector, V1);
              T1 := VectorCrossProduct(N1, YVector);
              Attribute3f(attrNormal, N1);
              Attribute3f(attrTangent, T1);
              T1 := VectorCrossProduct(T1, N1);
              Attribute3f(attrBinormal, T1);
            end;
            Attribute2f(attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
            ScaleVector(V1, Radius);
            Attribute3f(attrPosition, V1);
            EmitVertex;
            Theta := Theta + StepH;
          end;
          EndAssembly;
          Triangulate;
          SplitVertices;
          FBatch.Mesh.Merge(LCapMesh);
        finally
          UnLock;
        end;
      end;
    end;

    // bottom cap
    if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
    begin
      if not Assigned(LCapMesh) then
        LCapMesh := TMeshAtom.Create;
      SinCos(AngBottom, SinP, CosP);

      with LCapMesh do
      begin
        Lock;
        try
          Clear;
          DeclareAttribute(attrPosition, GLSLType3f);
          DeclareAttribute(attrNormal, GLSLType3f);
          DeclareAttribute(attrTangent, GLSLType3f);
          DeclareAttribute(attrBinormal, GLSLType3f);
          DeclareAttribute(attrTexCoord0, GLSLType2f);

          BeginAssembly(mpTRIANGLE_FAN);

          Attribute2f(attrTexCoord0, 0.5, 0.5);
          Attribute3f(attrNormal, 0, -1, 0);
          Attribute3f(attrTangent, -1, 0, 0);
          Attribute3f(attrBinormal, 0, 0, -1);

          if FBottomCap = ctCenter then
            Attribute3f(attrPosition, 0, 0, 0)
          else
          begin
            Attribute3f(attrPosition, 0, SinP * Radius, 0);
            N1 := YVector;
            T1 := XVector;
          end;
          EmitVertex;
          Theta := AngStop;
          for I := 0 to FSlices do
          begin
            SinCos(Theta, SinT, CosT);
            V1[0] := CosP * SinT;
            V1[1] := SinP;
            V1[2] := CosP * CosT;
            if FTopCap = ctCenter then
            begin
              N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
              T1 := VectorCrossProduct(N1, YVector);
              Attribute3f(attrNormal, N1);
              Attribute3f(attrTangent, T1);
              T1 := VectorCrossProduct(T1, N1);
              Attribute3f(attrBinormal, T1);
            end;
            Attribute2f(attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
            ScaleVector(V1, Radius);
            Attribute3f(attrPosition, V1);
            EmitVertex;
            Theta := Theta - StepH;
          end;
          EndAssembly;
          Triangulate;
          SplitVertices;
          FBatch.Mesh.Merge(LCapMesh);
          WeldVertices;
        finally
          UnLock;
        end;
      end;
    end;

    case FNormals of
      nsSmooth:
        begin
          if FNormalDirection = ndInside then
          begin
            FBatch.Mesh.Triangulate;
            FBatch.Mesh.FlipFaces;
          end;
          ApplyExtras;
        end;
      nsFlat:
        begin
          if FNormalDirection = ndInside then
          begin
            FBatch.Mesh.Triangulate;
            FBatch.Mesh.FlipFaces;
          end;
          FBatch.Mesh.ComputeNormals(False);
          ApplyExtras;
        end;
      nsNone:
        begin
          FBatch.Mesh.Attributes[attrNormal] := False;
          FBatch.Mesh.Validate;
          ApplyExtras;
        end;
    end;

  finally
    FBatch.Mesh.UnLock;
    LCapMesh.Free;
  end;

  inherited;
end;

constructor TGLSphere.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := 0.5;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

// SetBottom
//

procedure TGLSphere.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TGLSphere.SetBottomCap(aValue: TCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetNormalDirection(const Value: TNormalDirection);
begin
  if Value <> FNormalDirection then
  begin
    FNormalDirection := Value;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetNormals(const Value: TNormalSmoothing);
begin
  if Value <> FNormals then
  begin
    FNormals := Value;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TGLSphere.SetRadius(const aValue: TGLFloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGLSphere.SetSlices(aValue: Integer);
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

procedure TGLSphere.SetStacks(aValue: TGLInt);
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

procedure TGLSphere.SetStart(aValue: TAngleLimit2);
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

procedure TGLSphere.SetStop(aValue: TAngleLimit2);
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

procedure TGLSphere.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TGLSphere.SetTopCap(aValue: TCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

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
  Result[0] := Abs(FRadius);
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := 0;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLSphere'}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLPolygonBase'}{$ENDIF}

// Create
//

constructor TGLPolygonBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
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

procedure TGLPolygonBase.SetNodes(const aNodes: TGLNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

// SetSplineMode
//

procedure TGLPolygonBase.SetSplineMode(const val: TLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

// AddNode (coords)
//

procedure TGLPolygonBase.AddNode(const coords: TGLCoordinates);
var
  LNode: TGLNode;
begin
  LNode := Nodes.Add;
  if Assigned(coords) then
    LNode.AsVector := coords.AsVector;
  StructureChanged;
end;

// AddNode (xyz)
//

procedure TGLPolygonBase.AddNode(const X, Y, Z: TGLFloat);
var
  LNode: TGLNode;
begin
  LNode := Nodes.Add;
  LNode.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

// AddNode (vector)
//

procedure TGLPolygonBase.AddNode(const Value: TVector);
var
  LNode: TGLNode;
begin
  LNode := Nodes.Add;
  LNode.AsVector := Value;
  StructureChanged;
end;

// AddNode (affine vector)
//

procedure TGLPolygonBase.AddNode(const Value: TAffineVector);
var
  LNode: TGLNode;
begin
  LNode := Nodes.Add;
  LNode.AsVector := VectorMake(Value);
  StructureChanged;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses([TGLSphere, TGLCube, TGLPlane, TGLSprite, TGLPoints,
    TGLDummyCube, TGLLines]);

end.

