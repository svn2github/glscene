//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLGeomObjects<p>

  Geometric objects.<p>

  <b>History : </b><font size=-1><ul>
  <li>16/05/11 - Yar - Transition to indirect rendering objects
  <li>13/05/11 - Vince - Add ArrowArc object
  <li>13/05/11 - Vince - Add StartAngle, StopAngle and Parts attributes
  to display a slice of TGLTorus between start and stop angles
  <li>24/03/11 - Yar - Replaced TGLTorus primitives to triangles, added tangent and binormal attributes
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>15/03/08 - DaStr - Deleted TGLFrustrum.AxisAlignedBoundingBox(),
  now this function references the inherited function
  <li>20/01/08 - DaStr - Corrected object centering in TGLFrustrum.BuildList()
  (thanks Sandor Domokos) (BugTrackerID = 1864314)
  Added a TGLCapsule object (thanks Dave Gravel)
  <li>18/11/07 - DaStr - Got rid of compiler warning in TGLCone.RayCastIntersect
  <li>07/05/07 - DanB - Added TGLCone.RayCastIntersect
  Improved TGLDisk.RayCastIntersect
  <li>30/03/07 - DaStr - Added $I GLScene.inc
  <li>25/09/04 - Eric Pascual - Added AxisAlignedBoundingBox,
  AxisAlignedBoundingBoxUnscaled,
  AxisAlignedDimensionsUnscaled
  <li>29/11/03 - MF - Added shadow silhouette code for TGLCylinderBase et al.
  Added GetTopRadius to facilitate silhouette.
  <li>24/10/03 - NelC - Fixed TGLTorus texture coord. bug
  <li>21/07/03 - EG - Creation from GLObjects split
  </ul></font>
}
unit GLGeomObjects;

{$I GLScene.inc}

interface

uses
  Classes,
  GLScene,
  VectorGeometry,
  OpenGLTokens,
  OpenGLAdapter,
  GLContext,
  GLObjects,
  GLSilhouette,
  VectorTypes,
  GeometryBB,
  GLPipelineTransformation,
  GLRenderContextInfo,
  GLNodes,
  GLCoordinates,
  GLMaterial,
  GLMaterialEx,
  GLSMesh,
  GLSDrawTechnique;

type

  // TGLDisk
  //
  { : A Disk object.<p>
    The disk may not be complete, it can have a hole (controled by the
    InnerRadius property) and can only be a slice (controled by the StartAngle
    and SweepAngle properties). }
  TGLDisk = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius: TGLFloat;
    FSlices, FLoops: TGLInt;
    procedure SetOuterRadius(const aValue: Single);
    procedure SetInnerRadius(const aValue: Single);
    procedure SetSlices(aValue: TGLInt);
    procedure SetLoops(aValue: TGLInt);
    procedure SetStartAngle(const aValue: Single);
    procedure SetSweepAngle(const aValue: Single);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published Declarations }
    {: Allows defining a "hole" in the disk. }
    property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
    {: Number of radial mesh subdivisions. }
    property Loops: TGLInt read FLoops write SetLoops default 2;
    {: Outer radius for the disk.<p>
       If you leave InnerRadius at 0, this is the disk radius. }
    property OuterRadius: TGLFloat read FOuterRadius write SetOuterRadius;
    {: Number of mesh slices.<p>
       For instance, if Slices=6, your disk will look like an hexagon. }
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property StartAngle: TGLFloat read FStartAngle write SetStartAngle;
    property SweepAngle: TGLFloat read FSweepAngle write SetSweepAngle;
  end;

  // TGLGeoSphere
  //
  {: A geodesical shpere object. }
  TGLGeoSphere = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FLevel: Integer;
    FNormalDirection: TNormalDirection;
    FNormals: TNormalSmoothing;
    procedure SetRadius(const Value: TGLFloat);
    procedure SetSubdivisionLevel(Value: Integer);
    procedure SetNormalDirection(const Value: TNormalDirection);
    procedure SetNormals(const Value: TNormalSmoothing);
  protected
    { Protected declaration }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published Declarations }
    property Radius: TGLFloat read FRadius write SetRadius;
    property SubdivisionLevel: Integer read FLevel write SetSubdivisionLevel
      default 2;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
  end;

  // TGLCylinderBase
  //
    {: Base class to cylinder-like objects.<p>
       Introduces the basic cylinder description properties.<p>
       Be aware teh default slices and stacks make up for a high-poly cylinder,
       unless you're after high-quality lighting it is recommended to reduce the
       Stacks property to 1. }
  TGLCylinderBase = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FBottomRadius: TGLFloat;
    FSlices, FStacks, FLoops: TGLInt;
    FHeight: TGLFloat;
  protected
    { Protected Declarations }

    procedure SetBottomRadius(const aValue: Single);
    procedure SetHeight(const aValue: Single);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStacks(aValue: TGLInt);
    procedure SetLoops(aValue: TGLInt);
    function GetTopRadius: single; virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;
  published
    { Published Declarations }
    property BottomRadius: TGLFloat read FBottomRadius write SetBottomRadius;
    property Height: TGLFloat read FHeight write SetHeight;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 4;
    {: Number of concentric rings for top/bottom disk(s). }
    property Loops: TGLInt read FLoops write SetLoops default 1;
  end;

  // TConePart
  //
  TConePart = (coSides, coBottom);
  TConeParts = set of TConePart;

  // TGLCone
  //
  {: A cone object. }
  TGLCone = class(TGLCylinderBase)
  private
    { Private Declarations }
    FParts: TConeParts;
    procedure SetParts(aValue: TConeParts);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    function GetTopRadius: single; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Parts: TConeParts read FParts write SetParts default [coSides,
      coBottom];
  end;

  // TCylinderPart
  //
  TCylinderPart = (cySides, cyBottom, cyTop);
  TCylinderParts = set of TCylinderPart;

  // TCylinderAlignment
  //
  TCylinderAlignment = (caCenter, caTop, caBottom);

  // TGLCylinder
  //
    {: Cylinder object, can also be used to make truncated cones }
  TGLCylinder = class(TGLCylinderBase)
  private
    { Private Declarations }
    FParts: TCylinderparts;
    FTopRadius: TGLFloat;
    FAlignment: TCylinderAlignment;
  protected
    { Protected Declarations }
    procedure SetTopRadius(const aValue: Single);
    procedure SetParts(aValue: TCylinderParts);
    procedure SetAlignment(val: TCylinderAlignment);
    function GetTopRadius: single; override;
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure Align(const startPoint, endPoint: TVector); overload;
    procedure Align(const startObj, endObj: TGLBaseSceneObject); overload;
    procedure Align(const startPoint, endPoint: TAffineVector); overload;
  published
    { Published Declarations }
    property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
    property Parts: TCylinderParts read FParts write SetParts default [cySides,
      cyBottom, cyTop];
    property Alignment: TCylinderAlignment read FAlignment write SetAlignment
      default caCenter;
  end;

  // TGLCapsule
  //
  {: Capsule object, can also be used to make truncated cones }
  TGLCapsule = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FParts: TCylinderparts;
    FRadius: TGLFloat;
    FSlices: TGLInt;
    FStacks: TGLInt;
    FHeight: TGLFloat;
    FAlignment: TCylinderAlignment;
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure SetHeight(const aValue: Single);
    procedure SetRadius(const aValue: Single);
    procedure SetSlices(const aValue: integer);
    procedure SetStacks(const aValue: integer);
    procedure SetParts(aValue: TCylinderParts);
    procedure SetAlignment(val: TCylinderAlignment);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure Align(const startPoint, endPoint: TVector); overload;
    procedure Align(const startObj, endObj: TGLBaseSceneObject); overload;
    procedure Align(const startPoint, endPoint: TAffineVector); overload;
  published
    { Published Declarations }
    property Height: TGLFloat read FHeight write SetHeight;
    property Slices: TGLInt read FSlices write SetSlices;
    property Stacks: TGLInt read FStacks write SetStacks;
    property Radius: TGLFloat read FRadius write SetRadius;
    property Parts: TCylinderParts read FParts write SetParts default [cySides,
      cyBottom, cyTop];
    property Alignment: TCylinderAlignment read FAlignment write SetAlignment
      default caCenter;
  end;

  // TAnnulusPart
  //
  TAnnulusPart = (anInnerSides, anOuterSides, anBottom, anTop);
  TAnnulusParts = set of TAnnulusPart;

  // TGLAnnulus
  //
  {: An annulus is a cylinder that can be made hollow (pipe-like). }
  TGLAnnulus = class(TGLCylinderBase)
  private
    { Private Declarations }
    FParts: TAnnulusParts;
    FBottomInnerRadius: TGLFloat;
    FTopInnerRadius: TGLFloat;
    FTopRadius: TGLFloat;
  protected
    { Protected Declarations }
    procedure SetTopRadius(const aValue: Single);
    procedure SetTopInnerRadius(const aValue: Single);
    procedure SetBottomInnerRadius(const aValue: Single);
    procedure SetParts(aValue: TAnnulusParts);
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property BottomInnerRadius: TGLFLoat read FBottomInnerRadius write SetBottomInnerRadius;
    property TopInnerRadius: TGLFloat read FTopInnerRadius write SetTopInnerRadius;
    property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
    property Parts: TAnnulusParts read FParts write SetParts default [anInnerSides, anOuterSides, anBottom, anTop];
  end;

  // TTorusPart
  //
  TTorusPart = (toSides, toStartDisk, toStopDisk);
  TTorusParts = set of TTorusPart;

  // TGLTorus
  //
  { : A Torus object. }
  // TGLTorus
  //
  {: A Torus object. }
  TGLTorus = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FParts: TTorusParts;
    FRings, FSides: Cardinal;
    FMinorRadius, FMajorRadius: Single;
    FStartAngle, FStopAngle: Single;
    procedure SetMajorRadius(const aValue: Single);
    procedure SetMinorRadius(const aValue: Single);
    procedure SetRings(aValue: Cardinal);
    procedure SetSides(aValue: Cardinal);
    procedure SetStartAngle(const aValue: Single);
    procedure SetStopAngle(const aValue: Single);
    procedure SetParts(aValue: TTorusParts);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property MajorRadius: Single read FMajorRadius write SetMajorRadius;
    property MinorRadius: Single read FMinorRadius write SetMinorRadius;
    property Rings: Cardinal read FRings write SetRings default 25;
    property Sides: Cardinal read FSides write SetSides default 15;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property Parts: TTorusParts read FParts write SetParts default [toSides];
  end;

  // TArrowLinePart
  //
  TArrowLinePart = (alLine, alTopArrow, alBottomArrow);
  TArrowLineParts = set of TArrowLinePart;

  // TArrowHeadStackingStyle
  //
  TArrowHeadStackingStyle = (ahssStacked, ahssCentered, ahssIncluded);

  // TGLArrowLine
  //
  { : Draws an arrowhead (cylinder + cone).<p>
    The arrow head is a cone that shares the attributes of the cylinder
    (ie stacks/slices, materials etc). Seems to work ok.<br>
    This is useful for displaying a vector based field (eg velocity) or
    other arrows that might be required.<br>
    By default the bottom arrow is off }
  TGLArrowLine = class(TGLCylinderBase)
  private
    { Private Declarations}
    fParts: TArrowLineParts;
    fTopRadius: Single;
    fTopArrowHeadHeight: Single;
    fTopArrowHeadRadius: Single;
    fBottomArrowHeadHeight: Single;
    fBottomArrowHeadRadius: Single;
    FHeadStackingStyle: TArrowHeadStackingStyle;
  protected
    { Protected Declarations}
    procedure SetTopRadius(const aValue: Single);
    procedure SetTopArrowHeadHeight(const aValue: Single);
    procedure SetTopArrowHeadRadius(const aValue: Single);
    procedure SetBottomArrowHeadHeight(const aValue: Single);
    procedure SetBottomArrowHeadRadius(const aValue: Single);
    procedure SetParts(aValue: TArrowLineParts);
    procedure SetHeadStackingStyle(const val: TArrowHeadStackingStyle);
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations}
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations}
    property TopRadius: TGLFloat read fTopRadius write SetTopRadius;
    property HeadStackingStyle: TArrowHeadStackingStyle read FHeadStackingStyle write SetHeadStackingStyle default ahssStacked;
    property Parts: TArrowLineParts read fParts write SetParts default [alLine, alTopArrow];
    property TopArrowHeadHeight: TGLFloat read fTopArrowHeadHeight write SetTopArrowHeadHeight;
    property TopArrowHeadRadius: TGLFloat read fTopArrowHeadRadius write SetTopArrowHeadRadius;
    property BottomArrowHeadHeight: TGLFloat read fBottomArrowHeadHeight write SetBottomArrowHeadHeight;
    property BottomArrowHeadRadius: TGLFloat read fBottomArrowHeadRadius write SetBottomArrowHeadRadius;
  end;

  // TArrowArcPart
  //
  TArrowArcPart = (aaArc, aaTopArrow, aaBottomArrow);
  TArrowArcParts = set of TArrowArcPart;

  // TGLArrowArc
  //
  { : Draws an arrowhead (Sliced Torus + cone).<p>
    The arrow head is a cone that shares the attributes of the Torus
    (ie stacks/slices, materials etc).<br>
    This is useful for displaying a movement (eg twist) or
    other arc arrows that might be required.<br>
    By default the bottom arrow is off }
  TGLArrowArc = class(TGLCylinderBase)
  private
    { Private Declarations }
    fArcRadius: Single;
    FStartAngle: Single;
    FStopAngle: Single;
    FParts: TArrowArcParts;
    FTopRadius: Single;
    fTopArrowHeadHeight: Single;
    fTopArrowHeadRadius: Single;
    fBottomArrowHeadHeight: Single;
    fBottomArrowHeadRadius: Single;
    FHeadStackingStyle: TArrowHeadStackingStyle;
  protected
    { Protected Declarations }
    procedure SetArcRadius(const aValue: Single);
    procedure SetStartAngle(const aValue: Single);
    procedure SetStopAngle(const aValue: Single);
    procedure SetTopRadius(const aValue: Single);
    procedure SetTopArrowHeadHeight(const aValue: Single);
    procedure SetTopArrowHeadRadius(const aValue: Single);
    procedure SetBottomArrowHeadHeight(const aValue: Single);
    procedure SetBottomArrowHeadRadius(const aValue: Single);
    procedure SetParts(aValue: TArrowArcParts);
    procedure SetHeadStackingStyle(const val: TArrowHeadStackingStyle);
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property ArcRadius: TGLFloat read fArcRadius write SetArcRadius;
    property StartAngle: TGLFloat read FStartAngle write SetStartAngle;
    property StopAngle: TGLFloat read FStopAngle write SetStopAngle;
    property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
    property HeadStackingStyle: TArrowHeadStackingStyle read FHeadStackingStyle
      write SetHeadStackingStyle default ahssStacked;
    property Parts: TArrowArcParts read FParts write SetParts
      default [aaArc, aaTopArrow];
    property TopArrowHeadHeight: TGLFloat read fTopArrowHeadHeight
      write SetTopArrowHeadHeight;
    property TopArrowHeadRadius: TGLFloat read fTopArrowHeadRadius
      write SetTopArrowHeadRadius;
    property BottomArrowHeadHeight: TGLFloat read fBottomArrowHeadHeight
      write SetBottomArrowHeadHeight;
    property BottomArrowHeadRadius: TGLFloat read fBottomArrowHeadRadius
      write SetBottomArrowHeadRadius;
  end;

  // TPolygonParts
  //
  TPolygonPart = (ppTop, ppBottom);
  TPolygonParts = set of TPolygonPart;

  // TGLPolygon
  //
  { : A basic polygon object.<p>
    The curve is described by the Nodes and SplineMode properties, should be
    planar and is automatically tessellated.<p>
    Texture coordinates are deduced from X and Y coordinates only.<p>
    This object allows only for polygons described by a single curve, if you
    need "complex polygons" with holes, patches and cutouts, see GLMultiPolygon. }
  TGLPolygon = class(TGLPolygonBase)
  private
    { Private Declarations }
    FParts: TPolygonParts;

  protected
    { Protected Declarations }
    procedure SetParts(const val: TPolygonParts);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TRenderContextInfo); override;

  published
    { Published Declarations }
    { : Parts of polygon.<p>
      The 'top' of the polygon is the position were the curve describing
      the polygon spin counter-clockwise (i.e. right handed convention). }
    property Parts: TPolygonParts read FParts write SetParts
      default [ppTop, ppBottom];
  end;

  // TFrustrumParts
  //
  TFrustrumPart = (fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight);
  TFrustrumParts = set of TFrustrumPart;

const
  cAllFrustrumParts = [fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight];

type
  // TGLFrustrum
  //
  { A frustrum is a pyramid with the top chopped off.<p>
     The height of the imaginary pyramid is ApexHeight, the height of the
     frustrum is Height. If ApexHeight and Height are the same, the frustrum
     degenerates into a pyramid.<br>
     Height cannot be greater than ApexHeight. }
  TGLFrustrum = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FApexHeight, FBaseDepth, FBaseWidth, FHeight: TGLFloat;
    FParts: TFrustrumParts;
    FNormalDirection: TNormalDirection;
    procedure SetApexHeight(const aValue: Single);
    procedure SetBaseDepth(const aValue: Single);
    procedure SetBaseWidth(const aValue: Single);
    procedure SetHeight(const aValue: Single);
    procedure SetParts(aValue: TFrustrumParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function TopDepth: TGLFloat;
    function TopWidth: TGLFloat;
  published
    { Published Declarations }
    property ApexHeight: TGLFloat read FApexHeight write SetApexHeight stored
      False;
    property BaseDepth: TGLFloat read FBaseDepth write SetBaseDepth stored
      False;
    property BaseWidth: TGLFloat read FBaseWidth write SetBaseWidth stored
      False;
    property Height: TGLFloat read FHeight write SetHeight stored False;
    property NormalDirection: TNormalDirection read FNormalDirection write
      SetNormalDirection default ndOutside;
    property Parts: TFrustrumParts read FParts write SetParts default
      cAllFrustrumParts;
  end;

  // TGLDodecahedron
  //
  {: A Dodecahedron.<p>
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLDodecahedron = class(TGLSceneObjectEx)
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  end;

  // TGLIcosahedron
  //
  {: A Icosahedron.<p>
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLIcosahedron = class(TGLSceneObjectEx)
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Polynomials,
  GLColor,
  GLSLParameter,
  GLState;

{$IFDEF GLS_REGION}{$REGION 'TGLDisk'}{$ENDIF}

procedure TGLDisk.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLDisk) then
  begin
    FOuterRadius := TGLDisk(Source).FOuterRadius;
    FInnerRadius := TGLDisk(Source).FInnerRadius;
    FSlices := TGLDisk(Source).FSlices;
    FLoops := TGLDisk(Source).FLoops;
    FStartAngle := TGLDisk(Source).FStartAngle;
    FSweepAngle := TGLDisk(Source).FSweepAngle;
  end;
  inherited Assign(Source);
end;

procedure TGLDisk.BuildMesh;
var
  cosCache, sinCache: array of Single;
  i, j: Integer;
  Rstep, R: Single;
begin
  SetLength(cosCache, FSlices + 1);
  SetLength(sinCache, FSlices + 1);
  PrepareSinCosCache(sinCache, cosCache,
    FStartAngle, FStartAngle + FSweepAngle);

  Rstep := (FOuterRadius - FInnerRadius) / FLoops;
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

      BeginAssembly(mpTRIANGLE_STRIP);

      Attribute3f(attrNormal, 0, 0, 1);
      Attribute3f(attrTangent, 1, 0, 0);
      Attribute3f(attrBinormal, 0, 1, 0);
      for j := 0 to FLoops - 1 do
      begin
        for i := 0 to FSlices do
        begin
          Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
          R := FInnerRadius + j * Rstep;
          Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, 0);
          EmitVertex;
          Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
          R := FInnerRadius + (j + 1) * Rstep;
          Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, 0);
          EmitVertex;
        end;
      end;
      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;
  FBatch.Changed := True;
  ClearStructureChanged;
end;

constructor TGLDisk.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FOuterRadius := 0.5;
  FInnerRadius := 0;
  FSlices := 16;
  FLoops := 2;
  FStartAngle := 0;
  FSweepAngle := 360;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;

  FBatch.Mesh.TagName := ClassName;
end;

procedure TGLDisk.SetInnerRadius(const aValue: Single);
begin
  if aValue <> FInnerRadius then
  begin
    FInnerRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLDisk.SetLoops(aValue: TGLInt);
begin
  if aValue <> FLoops then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

procedure TGLDisk.SetOuterRadius(const aValue: Single);
begin
  if aValue <> FOuterRadius then
  begin
    FOuterRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLDisk.SetSlices(aValue: TGLInt);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

procedure TGLDisk.SetStartAngle(const aValue: Single);
begin
  if aValue <> FStartAngle then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

procedure TGLDisk.SetSweepAngle(const aValue: Single);
begin
  if aValue <> FSweepAngle then
  begin
    FSweepAngle := aValue;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDisk'}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLGeoSphere'}{$ENDIF}

procedure TGLGeoSphere.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLGeoSphere) then
  begin
    Radius := TGLGeoSphere(Source).FRadius;
    SubdivisionLevel := TGLGeoSphere(Source).FLevel;
  end;
  inherited;
end;

constructor TGLGeoSphere.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FRadius := 0.5;
  FLevel := 1;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;

  FBatch.Mesh.TagName := ClassName;
end;

procedure TGLGeoSphere.BuildMesh;
var
  pivot, dir1, dir2, V, B: TAffineVector;
  level, numLayers: Integer;
  dt, t_top, t_bot, ds_top, ds_bot, s_top, s_bot,
    smax_top, smax_bot: Single;
  offset: Boolean;

  procedure ProduceVertex(s, t, sb, tb: Single; uprow: Boolean);
  var
    XZR: Single;
    LTangent: TVector3f;
  begin
    V[0] := pivot[0] + s * dir1[0] + t * dir2[0];
    V[1] := pivot[1] + s * dir1[1] + t * dir2[1];
    V[2] := pivot[2] + s * dir1[2] + t * dir2[2];
    B[0] := pivot[0] + sb * dir1[0] + tb * dir2[0];
    B[1] := pivot[1] + sb * dir1[1] + tb * dir2[1];
    B[2] := pivot[2] + sb * dir1[2] + tb * dir2[2];
    NormalizeVector(V);
    NormalizeVector(B);

    if uprow then
    begin
      // Poles
      XZR := sqrt(B[0] * B[0] + B[2] * B[2]);
      s := V[0];
      t := V[2];
      if XZR > 0 then
      begin
        s := s / XZR;
        t := t / XZR;
      end;
      s := 0.5 + (1 / 6) * s;
      t := (1 / 6) * (t + 1);
      if V[1] > 0 then
        t := 1 - t;
    end
    else
    begin
      // Equator
      s := 0.5 + 0.5 * arctan2(V[0], V[2]) / Pi;
      if offset and (s > 0.99) then
        s := 0;

      t := 0.5;
      if B[1] <> 0 then
        t := t + (1 / 6) * V[1] / Abs(B[1]);
    end;

    with FBatch.Mesh do
    begin
      Attribute2f(attrTexCoord0, s, t);
      Attribute3f(attrNormal, V);
      LTangent := VectorCrossProduct(YVector, V);
      Attribute3f(attrTangent, LTangent);
      Attribute3f(attrBinormal, VectorCrossProduct(V, LTangent));
      ScaleVector(V, FRadius);
      Attribute3f(attrPosition, V);
      EmitVertex;
    end;
  end;

  procedure ProduceOctant;
  var
    i, j, caps: Integer;
    t_border, smax_border: Single;
  begin
    t_top := 1;
    ds_top := 0;
    caps := numLayers div 2;

    t_border := 1 - caps * dt;
    smax_border := 1 - t_border;

    for i := 0 to numLayers - 1 do
    begin
      t_bot := t_top - dt;
      smax_top := 1 - t_top;
      smax_bot := 1 - t_bot;
      if i > 0 then
        ds_top := smax_top / i;
      ds_bot := smax_bot / (i + 1);
      s_top := 0;
      s_bot := 0;
      for j := 0 to i do
      begin
        ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i
          < caps);
        ProduceVertex(s_top, t_top, s_top * smax_border / smax_top, t_border, i
          < caps);
        s_top := s_top + ds_top;
        s_bot := s_bot + ds_bot;
      end;
      ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i <
        caps);
      FBatch.Mesh.RestartStrip;
      t_top := t_bot;
    end;
  end;

begin
  level := FLevel;
  numLayers := 1 shl level;
  dt := 1 / numLayers;

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
      BeginAssembly(mpTRIANGLE_STRIP);

      offset := false;
      SetVector(pivot, 1, 0, 0);
      SetVector(dir1, -1, 0, 1);
      SetVector(dir2, -1, 1, 0);
      ProduceOctant;

      SetVector(pivot, 0, 0, 1);
      SetVector(dir1, -1, 0, -1);
      SetVector(dir2, 0, 1, -1);
      ProduceOctant;

      SetVector(pivot, 0, 0, -1);
      SetVector(dir1, 1, 0, 1);
      SetVector(dir2, 0, 1, 1);
      ProduceOctant;

      SetVector(pivot, -1, 0, 0);
      SetVector(dir1, 1, 0, 1);
      SetVector(dir2, 1, -1, 0);
      ProduceOctant;

      SetVector(pivot, 0, 0, 1);
      SetVector(dir1, 1, 0, -1);
      SetVector(dir2, 0, -1, -1);
      ProduceOctant;

      SetVector(pivot, 1, 0, 0);
      SetVector(dir1, -1, 0, -1);
      SetVector(dir2, -1, -1, 0);
      ProduceOctant;

      offset := true;
      SetVector(pivot, -1, 0, 0);
      SetVector(dir1, 1, 0, -1);
      SetVector(dir2, 1, 1, 0);
      ProduceOctant;

      SetVector(pivot, 0, 0, -1);
      SetVector(dir1, -1, 0, 1);
      SetVector(dir2, 0, -1, 1);
      ProduceOctant;

      EndAssembly;

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
      UnLock;
    end;
  end;
  ClearStructureChanged;
end;

// SetRadius
//

procedure TGLGeoSphere.SetRadius(const Value: TGLFloat);
begin
  if Value <> FRadius then
  begin
    FRadius := Value;
    StructureChanged;
  end;
end;

procedure TGLGeoSphere.SetSubdivisionLevel(Value: Integer);
begin
  if Value <> FLevel then
  begin
    if Value < 0 then
      Value := 0;
    if Value > 4 then
      Value := 4;
    FLevel := Value;
    StructureChanged;
  end;
end;


procedure TGLGeoSphere.SetNormalDirection(const Value: TNormalDirection);
begin
  if Value <> FNormalDirection then
  begin
    FNormalDirection := Value;
    StructureChanged;
  end;
end;

procedure TGLGeoSphere.SetNormals(const Value: TNormalSmoothing);
begin
  if Value <> FNormals then
  begin
    FNormals := Value;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLCylinderBase'}{$ENDIF}

procedure TGLCylinderBase.Assign(Source: TPersistent);
begin
  if Source is TGLCylinderBase then
  begin
    FBottomRadius := TGLCone(Source).FBottomRadius;
    FSlices := TGLCone(Source).FSlices;
    FStacks := TGLCone(Source).FStacks;
    FLoops := TGLCone(Source).FLoops;
    FHeight := TGLCone(Source).FHeight;
  end;
  inherited Assign(Source);
end;

constructor TGLCylinderBase.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FBottomRadius := 0.5;
  FHeight := 1;
  FSlices := 16;
  FStacks := 4;
  FLoops := 1;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
end;

function TGLCylinderBase.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  connectivity: TConnectivity;
  sil: TGLSilhouette;
  ShadowSlices: integer;

  i: integer;
  p: array[0..3] of TVector3f;
  PiDivSlices: single;
  a1, a2: single;
  c1, c2: TVector3f;
  cosa1, cosa2, sina1, sina2: single;
  HalfHeight: single;
  ShadowTopRadius: single;
begin
  Connectivity := TConnectivity.Create(true);

  ShadowSlices := FSlices div 1;

  if FSlices < 5 then
    FSlices := 5;

  PiDivSlices := 2 * Pi / ShadowSlices;

  a1 := 0;

  // Is this a speed improvement or just a waste of code?
  HalfHeight := FHeight / 2;

  MakeVector(c1, 0, -HalfHeight, 0);
  MakeVector(c2, 0, HalfHeight, 0);

  ShadowTopRadius := GetTopRadius;

  for i := 0 to ShadowSlices - 1 do
  begin
    a2 := a1 + PiDivSlices;

    // Is this a speed improvement or just a waste of code?
    cosa1 := cos(a1);
    cosa2 := cos(a2);
    sina1 := sin(a1);
    sina2 := sin(a2);

    // Generate the four "corners";
    // Bottom corners
    MakeVector(p[0], FBottomRadius * sina2, -HalfHeight, FBottomRadius * cosa2);
    MakeVector(p[1], FBottomRadius * sina1, -HalfHeight, FBottomRadius * cosa1);

    // Top corners
    MakeVector(p[2], ShadowTopRadius * sina1, HalfHeight, ShadowTopRadius *
      cosa1);
    MakeVector(p[3], ShadowTopRadius * sina2, HalfHeight, ShadowTopRadius *
      cosa2);

    // This should be optimized to use AddIndexedFace, because this method
    // searches for each of the vertices and adds them or re-uses them.

    // Skin
    connectivity.AddFace(p[2], p[1], p[0]);
    connectivity.AddFace(p[3], p[2], p[0]);

    // Sides / caps
    connectivity.AddFace(c1, p[0], p[1]);
    connectivity.AddFace(p[2], p[3], c2);

    a1 := a1 + PiDivSlices;
  end;

  sil := nil;
  Connectivity.CreateSilhouette(
    silhouetteParameters, sil, false);

  result := sil;

  Connectivity.Free;
end;

// GetTopRadius
//

function TGLCylinderBase.GetTopRadius: single;
begin
  Result := FBottomRadius;
end;

procedure TGLCylinderBase.SetBottomRadius(const aValue: Single);
begin
  if aValue <> FBottomRadius then
  begin
    FBottomRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLCylinderBase.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLCylinderBase.SetLoops(aValue: TGLInt);
begin
  if (aValue >= 1) and (aValue <> FLoops) then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

procedure TGLCylinderBase.SetSlices(aValue: TGLInt);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

procedure TGLCylinderBase.SetStacks(aValue: TGLInt);
begin
  if aValue <> FStacks then
  begin
    FStacks := aValue;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLCone'}{$ENDIF}

procedure TGLCone.Assign(Source: TPersistent);
begin
  if Source is TGLCone then
  begin
    FParts := TGLCone(Source).FParts;
  end;
  inherited Assign(Source);
end;

procedure TGLCone.BuildMesh;
var
  cosCache, sinCache: array of Single;
  i, j: Integer;
  yLow, yHigh: Single;
  length: Single;
  deltaRadius: Single;
  xzNormalRatio, yNormal: Single;
  radiusLow, radiusHigh, Rstep: Single;
  LNormal, LTangent: TVector3f;
begin
  SetLength(cosCache, FSlices + 1);
  SetLength(sinCache, FSlices + 1);
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

  deltaRadius := Abs(FBottomRadius - GetTopRadius);
  length := SQRT(deltaRadius * deltaRadius + FHeight * FHeight);
  yNormal := deltaRadius / length;
  xzNormalRatio := FHeight / length;
  ScaleFloatArray(@sinCache[0], FSlices + 1, xzNormalRatio);
  ScaleFloatArray(@cosCache[0], FSlices + 1, xzNormalRatio);

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

      BeginAssembly(mpTRIANGLE_STRIP);

      if coSides in FParts then
      begin
        for J := 0 to FStacks - 1 do
        begin
          yLow := j * FHeight / FStacks - 0.5 * FHeight;
          yHigh := (j + 1) * FHeight / FStacks - 0.5 * FHeight;
          radiusLow := FBottomRadius - deltaRadius * (j / FStacks);
          radiusHigh := FBottomRadius - deltaRadius * ((j + 1) / FStacks);

          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition, radiusHigh * sinCache[I], yHigh, radiusHigh
              * cosCache[I]);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);
            LNormal := Vector3fMake(sinCache[I], yNormal, cosCache[I]);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(YVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;
            Attribute3f(attrPosition, radiusLow * sinCache[I], yLow, radiusLow *
              cosCache[I]);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      if coBottom in FParts then
      begin
        yLow := -0.5 * FHeight;
        Rstep := FBottomRadius / FLoops;
        Attribute3f(attrNormal, 0, 0, -1);
        Attribute3f(attrTangent, -1, 0, 0);
        Attribute3f(attrBinormal, 0, -1, 0);
        for j := 0 to FLoops - 1 do
        begin
          for i := 0 to FSlices do
          begin
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            radiusLow := j * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusLow, yLow, sinCache[i]
              * radiusLow);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            radiusLow := (j + 1) * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusLow, yLow, sinCache[i]
              * radiusLow);
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

  FBatch.Changed := True;
  ClearStructureChanged;
end;

constructor TGLCone.Create(AOwner: TComponent);
begin
  inherited;
  FParts := [coSides, coBottom];
  FBatch.Mesh.TagName := ClassName;
end;

function TGLCone.GetTopRadius: single;
begin
  Result := 0;
end;

procedure TGLCone.SetParts(aValue: TConeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLCylinder'}{$ENDIF}

constructor TGLCylinder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTopRadius := 0.5;
  FParts := [cySides, cyBottom, cyTop];
  FAlignment := caCenter;
  FBatch.Mesh.TagName := ClassName;
end;

// SetTopRadius
//

procedure TGLCylinder.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// GetTopRadius
//

function TGLCylinder.GetTopRadius: single;
begin
  result := FTopRadius;
end;

// SetParts
//

procedure TGLCylinder.SetParts(aValue: TCylinderParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetAlignment
//

procedure TGLCylinder.SetAlignment(val: TCylinderAlignment);
begin
  if val <> FAlignment then
  begin
    FAlignment := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGLCylinder.Assign(Source: TPersistent);
begin
  if Assigned(SOurce) and (Source is TGLCylinder) then
  begin
    FParts := TGLCylinder(Source).FParts;
    FTopRadius := TGLCylinder(Source).FTopRadius;
  end;
  inherited Assign(Source);
end;

procedure TGLCylinder.BuildMesh;
var
  cosCache, sinCache: array of Single;
  i, j: Integer;
  yLow, yHigh: Single;
  length: Single;
  deltaRadius: Single;
  xzNormalRatio, yNormal: Single;
  radiusLow, radiusHigh, Rstep: Single;
  LNormal, LTangent: TVector3f;
begin
  SetLength(cosCache, FSlices + 1);
  SetLength(sinCache, FSlices + 1);
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

  deltaRadius := FBottomRadius - GetTopRadius;
  length := SQRT(deltaRadius * deltaRadius + FHeight * FHeight);
  yNormal := deltaRadius / length;
  xzNormalRatio := FHeight / length;
  ScaleFloatArray(@sinCache[0], FSlices + 1, xzNormalRatio);
  ScaleFloatArray(@cosCache[0], FSlices + 1, xzNormalRatio);

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

      BeginAssembly(mpTRIANGLE_STRIP);

      if cySides in FParts then
      begin
        for J := 0 to FStacks - 1 do
        begin
          yLow := j * FHeight / FStacks - 0.5 * FHeight;
          yHigh := (j + 1) * FHeight / FStacks - 0.5 * FHeight;
          radiusLow := FBottomRadius - deltaRadius * (j / FStacks);
          radiusHigh := FBottomRadius - deltaRadius * ((j + 1) / FStacks);
          Attribute3f(attrBinormal, YVector);
          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition, radiusHigh * sinCache[I], yHigh, radiusHigh
              * cosCache[I]);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);
            LNormal := Vector3fMake(sinCache[I], yNormal, cosCache[I]);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(YVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            EmitVertex;
            Attribute3f(attrPosition, radiusLow * sinCache[I], yLow, radiusLow *
              cosCache[I]);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      if cyTop in FParts then
      begin
        yHigh := 0.5 * FHeight;
        Rstep := GetTopRadius / FLoops;
        Attribute3f(attrNormal, 0, 1, 0);
        Attribute3f(attrTangent, 1, 0, 0);
        Attribute3f(attrBinormal, 0, 0, 1);
        for j := 0 to FLoops - 1 do
        begin
          for i := 0 to FSlices do
          begin
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            radiusHigh := (j + 1) * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusHigh, yHigh,
              sinCache[i] * radiusHigh);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            radiusHigh := j * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusHigh, yHigh,
              sinCache[i] * radiusHigh);
            EmitVertex;
          end;
        end;
        RestartStrip;
      end;

      if cyBottom in FParts then
      begin
        yLow := -0.5 * FHeight;
        Rstep := FBottomRadius / FLoops;
        Attribute3f(attrNormal, 0, -1, 0);
        Attribute3f(attrTangent, -1, 0, 0);
        Attribute3f(attrBinormal, 0, 0, -1);
        for j := 0 to FLoops - 1 do
        begin
          for i := 0 to FSlices do
          begin
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            radiusLow := j * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusLow, yLow, sinCache[i]
              * radiusLow);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            radiusLow := (j + 1) * Rstep;
            Attribute3f(attrPosition, cosCache[i] * radiusLow, yLow, sinCache[i]
              * radiusLow);
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

  FBatch.Changed := True;
  ClearStructureChanged;
end;

procedure TGLCylinder.Align(const startPoint, endPoint: TVector);
var
  dir: TAffineVector;
begin
  AbsolutePosition := startPoint;
  VectorSubtract(endPoint, startPoint, dir);
  if Parent <> nil then
    dir := Parent.AbsoluteToLocal(dir);
  Up.AsAffineVector := dir;
  Height := VectorLength(dir);
  Lift(Height * 0.5);
  Alignment := caCenter;
end;

// Align
//

procedure TGLCylinder.Align(const startObj, endObj: TGLBaseSceneObject);
begin
  Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//

procedure TGLCylinder.Align(const startPoint, endPoint: TAffineVector);
begin
  Align(PointMake(startPoint), PointMake(endPoint));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLCapsule'}{$ENDIF}

constructor TGLCapsule.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FHeight := 1;
  FRadius := 0.5;
  FSlices := 4;
  FStacks := 4;
  FParts := [cySides, cyBottom, cyTop];
  FAlignment := caCenter;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;

  FBatch.Mesh.TagName := ClassName;
end;

procedure TGLCapsule.BuildMesh;
var
  i, j, n: Integer;
  start_nx2: Single;
  start_ny2: Single;
  tmp, nx, ny, nz,
    start_nx, start_ny,
    a, ca, sa, l: Single;
  nx2, ny2, nz2: Single;
  zCentre: Single;
  LNormal, LTangent: TVector3f;
begin
  case Alignment of
    caTop: zCentre := FHeight + 1;
    caBottom: zCentre := -FHeight;
  else // caCenter
    zCentre := 0.5;
  end;
  n := FSlices * FStacks;
  l := FHeight;
  l := l * 0.5;
  a := (PI * 2.0) / n;
  sa := sin(a);
  ca := cos(a);
  ny := 0;
  nz := 1;

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

      BeginAssembly(mpTRIANGLE_STRIP);

      if cySides in FParts then
      begin
        Attribute3f(attrBinormal, ZVector);
        for i := 0 to n do
        begin
          LNormal := Vector3fMake(ny, nz, 0);
          Attribute3f(attrNormal, LNormal);
          LTangent := VectorCrossProduct(ZVector, LNormal);
          Attribute3f(attrTangent, LTangent);
          Attribute3f(attrPosition, ny * FRadius, nz * FRadius, zCentre + l - 0.5);
          Attribute2f(attrTexCoord0, i / n, 1);
          EmitVertex;

          Attribute3f(attrPosition, ny * FRadius, nz * FRadius, zCentre - l - 0.5);
          Attribute2f(attrTexCoord0, i / n, 0);
          EmitVertex;

          tmp := ca * ny - sa * nz;
          nz := sa * ny + ca * nz;
          ny := tmp;
        end;
        RestartStrip;
      end;

      zCentre := zCentre - 0.5;

      if cyTop in FParts then
      begin
        start_nx := 0;
        start_ny := 1;
        for j := 0 to (n div FStacks) do
        begin
          start_nx2 := ca * start_nx + sa * start_ny;
          start_ny2 := -sa * start_nx + ca * start_ny;
          nx := start_nx;
          ny := start_ny;
          nz := 0;
          nx2 := start_nx2;
          ny2 := start_ny2;
          nz2 := 0;
          for i := 0 to n do
          begin
            LNormal := Vector3fMake(ny2, nz2, nx2);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(ZVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            Attribute3f(attrPosition, ny2 * FRadius, nz2 * FRadius, zCentre + l + nx2 * FRadius);
            Attribute2f(attrTexCoord0, i / n, j / n);
            EmitVertex;

            LNormal := Vector3fMake(ny, nz, nx);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(ZVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            Attribute3f(attrPosition, ny * FRadius, nz * FRadius, zCentre + l + nx * FRadius);
            Attribute2f(attrTexCoord0, i / n, (j - 1) / n);
            EmitVertex;

            tmp := ca * ny - sa * nz;
            nz := sa * ny + ca * nz;
            ny := tmp;
            tmp := ca * ny2 - sa * nz2;
            nz2 := sa * ny2 + ca * nz2;
            ny2 := tmp;
          end;
          RestartStrip;
          start_nx := start_nx2;
          start_ny := start_ny2;
        end;
      end;

      if cyBottom in FParts then
      begin
        start_nx := 0;
        start_ny := 1;
        for j := 0 to (n div FStacks) do
        begin
          start_nx2 := ca * start_nx - sa * start_ny;
          start_ny2 := sa * start_nx + ca * start_ny;
          nx := start_nx;
          ny := start_ny;
          nz := 0;
          nx2 := start_nx2;
          ny2 := start_ny2;
          nz2 := 0;
          for i := 0 to n do
          begin
            LNormal := Vector3fMake(ny, nz, nx);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(ZVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            Attribute3f(attrPosition, ny * FRadius, nz * FRadius, zCentre - l + nx * FRadius);
            Attribute2f(attrTexCoord0, i / n, (j - 1) / n);
            EmitVertex;

            LNormal := Vector3fMake(ny2, nz2, nx2);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(ZVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            Attribute3f(attrPosition, ny2 * FRadius, nz2 * FRadius, zCentre - l + nx2 * FRadius);
            Attribute2f(attrTexCoord0, i / n, j / n);
            EmitVertex;

            tmp := ca * ny - sa * nz;
            nz := sa * ny + ca * nz;
            ny := tmp;
            tmp := ca * ny2 - sa * nz2;
            nz2 := sa * ny2 + ca * nz2;
            ny2 := tmp;
          end;
          RestartStrip;
          start_nx := start_nx2;
          start_ny := start_ny2;
        end;
      end;
      EndAssembly;
      ApplyExtras;
    finally
      Unlock;
    end;
  end;

  FBatch.Changed := True;
  ClearStructureChanged;
end;

// SetLength
//

procedure TGLCapsule.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TGLCapsule.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGLCapsule.SetSlices(const aValue: integer);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TGLCapsule.SetStacks(const aValue: integer);
begin
  if aValue <> FStacks then
  begin
    FStacks := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TGLCapsule.SetParts(aValue: TCylinderParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetAlignment
//

procedure TGLCapsule.SetAlignment(val: TCylinderAlignment);
begin
  if val <> FAlignment then
  begin
    FAlignment := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGLCapsule.Assign(Source: TPersistent);
begin
  if Assigned(SOurce) and (Source is TGLCapsule) then
  begin
    FParts := TGLCapsule(Source).FParts;
    FRadius := TGLCapsule(Source).FRadius;
  end;
  inherited Assign(Source);
end;

// Align
//

procedure TGLCapsule.Align(const startPoint, endPoint: TVector);
var
  dir: TAffineVector;
begin
  AbsolutePosition := startPoint;
  VectorSubtract(endPoint, startPoint, dir);
  if Parent <> nil then
    dir := Parent.AbsoluteToLocal(dir);
  Up.AsAffineVector := dir;
  FHeight := VectorLength(dir);
  Lift(FHeight * 0.5);
  Alignment := caCenter;
end;

// Align
//

procedure TGLCapsule.Align(const startObj, endObj: TGLBaseSceneObject);
begin
  Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//

procedure TGLCapsule.Align(const startPoint, endPoint: TAffineVector);
begin
  Align(PointMake(startPoint), PointMake(endPoint));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLAnnulus'}{$ENDIF}

constructor TGLAnnulus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  fBottomInnerRadius := 0.3;
  fTopInnerRadius := 0.3;
  fTopRadius := 0.5;
  fParts := [anInnerSides, anOuterSides, anBottom, anTop];
  FBatch.Mesh.TagName := ClassName;
end;

// SetBottomInnerRadius
//

procedure TGLAnnulus.SetBottomInnerRadius(const aValue: Single);
begin
  if aValue <> FBottomInnerRadius then
  begin
    FBottomInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopRadius
//

procedure TGLAnnulus.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopInnerRadius
//

procedure TGLAnnulus.SetTopInnerRadius(const aValue: Single);
begin
  if aValue <> FTopInnerRadius then
  begin
    FTopInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TGLAnnulus.SetParts(aValue: TAnnulusParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TGLAnnulus.BuildMesh;
var
  cosCache, sinCache: array of Single;
  i, j: Integer;
  yLow, yHigh: Single;
  length: Single;
  deltaRadius: Single;
  xzNormalRatio, yNormal: Single;
  radiusLow, radiusHigh, Rstep, R: Single;
  LNormal, LTangent: TVector3f;
begin
  SetLength(cosCache, FSlices + 1);
  SetLength(sinCache, FSlices + 1);
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

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

      BeginAssembly(mpTRIANGLE_STRIP);

      if anOuterSides in FParts then
      begin
        deltaRadius := Abs(FBottomRadius - FTopRadius);
        length := SQRT(deltaRadius * deltaRadius + FHeight * FHeight);
        yNormal := deltaRadius / length;
        xzNormalRatio := FHeight / length;

        for J := 0 to FStacks - 1 do
        begin
          yLow := j * FHeight / FStacks - 0.5 * FHeight;
          yHigh := (j + 1) * FHeight / FStacks - 0.5 * FHeight;
          radiusLow := FBottomRadius - deltaRadius * (j / FStacks);
          radiusHigh := FBottomRadius - deltaRadius * ((j + 1) / FStacks);

          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition,
              radiusHigh * sinCache[I] * xzNormalRatio,
              yHigh,
              radiusHigh * cosCache[I] * xzNormalRatio);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);
            LNormal := Vector3fMake(
              sinCache[I] * xzNormalRatio,
              yNormal,
              cosCache[I] * xzNormalRatio);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(YVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;

            Attribute3f(attrPosition,
              radiusLow * sinCache[I] * xzNormalRatio,
              yLow,
              radiusLow * cosCache[I] * xzNormalRatio);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      if anInnerSides in FParts then
      begin
        deltaRadius := Abs(FTopInnerRadius - FBottominnerRadius);
        length := SQRT(deltaRadius * deltaRadius + FHeight * FHeight);
        yNormal := deltaRadius / length;
        xzNormalRatio := FHeight / length;

        for J := 0 to FStacks - 1 do
        begin
          yLow := j * FHeight / FStacks - 0.5 * FHeight;
          yHigh := (j + 1) * FHeight / FStacks - 0.5 * FHeight;
          radiusLow := FBottominnerRadius - deltaRadius * (j / FStacks);
          radiusHigh := FBottominnerRadius - deltaRadius * ((j + 1) / FStacks);

          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition,
              radiusLow * sinCache[I] * xzNormalRatio,
              yLow,
              radiusLow * cosCache[I] * xzNormalRatio);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            LNormal := Vector3fMake(
              -sinCache[I] * xzNormalRatio,
              -yNormal,
              -cosCache[I] * xzNormalRatio);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(MinusYVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;

            Attribute3f(attrPosition,
              radiusHigh * sinCache[I] * xzNormalRatio,
              yHigh,
              radiusHigh * cosCache[I] * xzNormalRatio);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);

            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      if anTop in FParts then
      begin
        Rstep := (FTopRadius - FTopInnerRadius) / FLoops;
        yHigh := 0.5 * FHeight;

        Attribute3f(attrNormal, 0, 0, 1);
        Attribute3f(attrTangent, 1, 0, 0);
        Attribute3f(attrBinormal, 0, 1, 0);
        for j := 0 to FLoops - 1 do
        begin
          for i := 0 to FSlices do
          begin
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            R := FTopInnerRadius + (j + 1) * Rstep;
            Attribute3f(attrPosition, cosCache[i] * R, yHigh, sinCache[i] * R);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            R := FTopInnerRadius + j * Rstep;
            Attribute3f(attrPosition, cosCache[i] * R, yHigh, sinCache[i] * R);
            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      if anBottom in FParts then
      begin
        Rstep := (FBottomRadius - FBottominnerRadius) / FLoops;
        yLow := - 0.5 * FHeight;

        Attribute3f(attrNormal, 0, 0, -1);
        Attribute3f(attrTangent, -1, 0, 0);
        Attribute3f(attrBinormal, 0, -1, 0);
        for j := 0 to FLoops - 1 do
        begin
          for i := 0 to FSlices do
          begin
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            R := FBottominnerRadius + j * Rstep;
            Attribute3f(attrPosition, cosCache[i] * R, yLow, sinCache[i] * R);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            R := FBottominnerRadius + (j + 1) * Rstep;
            Attribute3f(attrPosition, cosCache[i] * R, yLow, sinCache[i] * R);
            EmitVertex;
          end;
          RestartStrip;
        end;
      end;

      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  FBatch.Changed := True;
  ClearStructureChanged;
end;

// Assign
//

procedure TGLAnnulus.Assign(Source: TPersistent);
begin
  if assigned(SOurce) and (Source is TGLAnnulus) then
  begin
    FParts := TGLAnnulus(Source).FParts;
    FTopRadius := TGLAnnulus(Source).FTopRadius;
    FTopInnerRadius := TGLAnnulus(Source).fTopInnerRadius;
    FBottomRadius := TGLAnnulus(Source).fBottomRadius;
    FBottomInnerRadius := TGLAnnulus(Source).fbottomInnerRadius;
  end;
  inherited Assign(Source);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLTorus'}{$ENDIF}

procedure TGLTorus.Assign(Source: TPersistent);
var
  LTorus: TGLTorus;
begin
  if Source is TGLTorus then
  begin
    LTorus := TGLTorus(Source);
    FParts := LTorus.FParts;
    FRings := LTorus.FRings;
    FSides := LTorus.FSides;
    FMinorRadius := LTorus.FMinorRadius;
    FMajorRadius := LTorus.FMajorRadius;
    FStartAngle := LTorus.FStartAngle;
    FStopAngle := LTorus.FStopAngle;
    StructureChanged;
  end;
  inherited;
end;

procedure TGLTorus.BuildMesh;
var
  I, J: Integer;
  Theta, Phi, Theta1, cosPhi, sinPhi, dist: TGLFloat;
  cosTheta1, sinTheta1: TGLFloat;
  ringDelta, sideDelta: TGLFloat;
  LPosition, LNormal, LTangent: TAffineVector;
  iFact, jFact, cosTheta, sinTheta: Single;
begin
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

      BeginAssembly(mpTRIANGLE_STRIP);

      ringDelta := ((FStopAngle - FStartAngle) / 360) * c2PI / FRings;
      sideDelta := c2PI / FSides;
      iFact := 1 / FRings;
      jFact := 1 / FSides;

      if toSides in FParts then
      begin
        Theta := DegToRad(FStartAngle);
        SinCos(Theta, sinTheta, cosTheta);
        for I := FRings - 1 downto 0 do
        begin
          Theta1 := Theta + ringDelta;
          SinCos(Theta1, sinTheta1, cosTheta1);
          Phi := 0;
          for J := FSides downto 0 do
          begin
            Phi := Phi + sideDelta;
            SinCos(Phi, sinPhi, cosPhi);
            dist := FMajorRadius + FMinorRadius * cosPhi;

            Attribute2f(attrTexCoord0, i * iFact, j * jFact);
            LNormal := Vector3fMake(cosTheta1 * cosPhi, -sinTheta1 * cosPhi,
              sinPhi);
            Attribute3f(attrNormal, LNormal);
            LPosition := Vector3fMake(cosTheta1 * dist, -sinTheta1 * dist,
              FMinorRadius * sinPhi);
            Attribute3f(attrPosition, LPosition);
            LPosition[2] := 0.0;
            NormalizeVector(LPosition);
            LTangent := VectorCrossProduct(ZVector, LPosition);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;

            Attribute2f(attrTexCoord0, (i + 1) * iFact, j * jFact);
            LNormal := Vector3fMake(cosTheta * cosPhi, -sinTheta * cosPhi,
              sinPhi);
            Attribute3f(attrNormal, LNormal);
            LPosition := Vector3fMake(cosTheta * dist, -sinTheta * dist,
              FMinorRadius
              * sinPhi);
            Attribute3f(attrPosition, LPosition);
            LPosition[2] := 0.0;
            NormalizeVector(LPosition);
            LTangent := VectorCrossProduct(ZVector, LPosition);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;
          end;
          RestartStrip;
          Theta := Theta1;
          cosTheta := cosTheta1;
          sinTheta := sinTheta1;
        end;
      end;

      jFact := 1 / (FSides + 1);

      if toStartDisk in FParts then
      begin
        Theta1 := DegToRad(FStartAngle);
        SinCos(Theta1, sinTheta1, cosTheta1);
        Phi := 0;
        LTangent := Vector3fMake(cosTheta1, -sinTheta1, 0);
        LNormal := VectorCrossProduct(ZVector, LTangent);
        Attribute3f(attrNormal, LNormal);
        Attribute3f(attrTangent, LTangent);
        Attribute3f(attrBinormal, ZVector);
        LPosition := Vector3fMake(FMajorRadius * cosTheta1, -FMajorRadius * sinTheta1, 0);
        for J := FSides + 1 downto 0 do
        begin
          SinCos(Phi, sinPhi, cosPhi);
          dist := FMajorRadius + FMinorRadius * cosPhi;
          LNormal := Vector3fMake(cosTheta1 * dist, -sinTheta1 * dist, FMinorRadius * sinPhi);
          Attribute2f(attrTexCoord0, J * jFact, 0.0);
          Attribute3f(attrPosition, LNormal);
          EmitVertex;
          Attribute2f(attrTexCoord0, J * jFact, 1.0);
          Attribute3f(attrPosition, LPosition);
          EmitVertex;
          Phi := Phi + sideDelta;
        end;
        RestartStrip;
      end;

      if toStopDisk in FParts then
      begin
        Theta1 := DegToRad(FStopAngle);
        SinCos(Theta1, sinTheta1, cosTheta1);
        Phi := 0;
        LTangent := Vector3fMake(-cosTheta1, sinTheta1, 0);
        LNormal := VectorCrossProduct(ZVector, LTangent);
        Attribute3f(attrNormal, LNormal);
        Attribute3f(attrTangent, LTangent);
        Attribute3f(attrBinormal, ZVector);
        LPosition := Vector3fMake(FMajorRadius * cosTheta1, -FMajorRadius * sinTheta1, 0);
        for J := FSides + 1 downto 0 do
        begin
          SinCos(Phi, sinPhi, cosPhi);
          Attribute2f(attrTexCoord0, J * jFact, 1.0);
          Attribute3f(attrPosition, LPosition);
          EmitVertex;
          dist := FMajorRadius + FMinorRadius * cosPhi;
          LNormal := Vector3fMake(cosTheta1 * dist, -sinTheta1 * dist, FMinorRadius * sinPhi);
          Attribute2f(attrTexCoord0, J * jFact, 0.0);
          Attribute3f(attrPosition, LNormal);
          EmitVertex;
          Phi := Phi + sideDelta;
        end;
        RestartStrip;
      end;
      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;
  FBatch.Changed := True;
  ClearStructureChanged;
end;

constructor TGLTorus.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FRings := 25;
  FSides := 15;
  FMinorRadius := 0.1;
  FMajorRadius := 0.4;
  FStartAngle := 0.0;
  FStopAngle := 360.0;
  FParts := [toSides, toStartDisk, toStopDisk];
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;

  FBatch.Mesh.TagName := ClassName;
end;

// SetMajorRadius
//

procedure TGLTorus.SetMajorRadius(const aValue: Single);
begin
  if FMajorRadius <> aValue then
  begin
    FMajorRadius := aValue;
    StructureChanged;
  end;
end;

// SetMinorRadius
//

procedure TGLTorus.SetMinorRadius(const aValue: Single);
begin
  if FMinorRadius <> aValue then
  begin
    FMinorRadius := aValue;
    StructureChanged;
  end;
end;

// SetRings
//

procedure TGLTorus.SetRings(aValue: Cardinal);
begin
  if FRings <> aValue then
  begin
    FRings := aValue;
    if FRings < 2 then
      FRings := 2;
    StructureChanged;
  end;
end;

// SetSides
//

procedure TGLTorus.SetSides(aValue: Cardinal);
begin
  if FSides <> aValue then
  begin
    FSides := aValue;
    if FSides < 3 then
      FSides := 3;
    StructureChanged;
  end;
end;

procedure TGLTorus.SetStartAngle(const aValue: Single);
begin
  if FStartAngle <> aValue then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

procedure TGLTorus.SetStopAngle(const aValue: Single);
begin
  if FStopAngle <> aValue then
  begin
    FStopAngle := aValue;
    StructureChanged;
  end;
end;

procedure TGLTorus.SetParts(aValue: TTorusParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLArrowLine'}{$ENDIF}

procedure TGLArrowLine.Assign(Source: TPersistent);
begin
  if assigned(Source) and (Source is TGLArrowLine) then
  begin
    FParts := TGLArrowLine(Source).FParts;
    FTopRadius := TGLArrowLine(Source).FTopRadius;
    FTopArrowHeadHeight := TGLArrowLine(Source).FTopArrowHeadHeight;
    FTopArrowHeadRadius := TGLArrowLine(Source).FTopArrowHeadRadius;
    FBottomArrowHeadHeight := TGLArrowLine(Source).FBottomArrowHeadHeight;
    FBottomArrowHeadRadius := TGLArrowLine(Source).FBottomArrowHeadRadius;
    FHeadStackingStyle := TGLArrowLine(Source).FHeadStackingStyle;
  end;
  inherited Assign(Source);
end;

constructor TGLArrowLine.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  fTopRadius := 0.1;
  BottomRadius := 0.1;
  fTopArrowHeadRadius := 0.2;
  fTopArrowHeadHeight := 0.5;
  fBottomArrowHeadRadius := 0.2;
  fBottomArrowHeadHeight := 0.5;
  FHeadStackingStyle := ahssStacked;
  { by default there is not much point having the top of the line (cylinder)
    showing as it is coincidental with the Toparrowhead bottom.
    Note I've defaulted to "vector" type arrows (arrow head on top only}
  fParts := [alLine, alTopArrow];
  FBatch.Mesh.TagName := ClassName;
end;

procedure TGLArrowLine.SetTopRadius(const aValue: Single);
begin
  if aValue <> fTopRadius then
  begin
    fTopRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetTopArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fTopArrowHeadHeight then
  begin
    fTopArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetTopArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fTopArrowHeadRadius then
  begin
    fTopArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetBottomArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadHeight then
  begin
    fBottomArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetBottomArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadRadius then
  begin
    fBottomArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetParts(aValue: TArrowLineParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.SetHeadStackingStyle(const val: TArrowHeadStackingStyle);
begin
  if val <> FHeadStackingStyle then
  begin
    FHeadStackingStyle := val;
    StructureChanged;
  end;
end;

procedure TGLArrowLine.BuildMesh;
var
  cosCache, sinCache: array of Single;
  cylHeight, cylOffset, headInfluence, headCentre: Single;
  zLow, zHigh: Single;
  length: Single;
  deltaRadius: Single;
  xzNormalRatio, zNormal: Single;
  radiusLow, radiusHigh, R, Rstep, Rstart: Single;
  LNormal, LTangent: TVector3f;
  I, J: Integer;
begin
  case HeadStackingStyle of
    ahssCentered: headInfluence := 0.5;
    ahssIncluded: headInfluence := 1;
  else // ahssStacked
    headInfluence := 0;
  end;
  cylHeight := Height;
  cylOffset := -FHeight * 0.5;

  SetLength(cosCache, FSlices + 1);
  SetLength(sinCache, FSlices + 1);
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

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

      BeginAssembly(mpTRIANGLE_STRIP);

      // does the top arrow part - the cone
      if alTopArrow in Parts then
      begin
        cylHeight := cylHeight - FTopArrowHeadHeight * headInfluence;
        headCentre := Height * 0.5 - FTopArrowHeadHeight * headInfluence;
        length := SQRT(FTopArrowHeadRadius * FTopArrowHeadRadius + FTopArrowHeadHeight * FTopArrowHeadHeight);
        zNormal := FTopArrowHeadRadius / length;
        xzNormalRatio := FTopArrowHeadHeight / length;

        for J := 0 to FStacks - 1 do
        begin
          zLow := j * FTopArrowHeadHeight / FStacks + headCentre;
          zHigh := (j + 1) * FTopArrowHeadHeight / FStacks + headCentre;
          radiusLow := FTopArrowHeadRadius * (1.0 - j / FStacks);
          radiusHigh := FTopArrowHeadRadius * (1.0 - (j + 1) / FStacks);
          radiusLow := radiusLow * xzNormalRatio;
          radiusHigh := radiusHigh * xzNormalRatio;

          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition,
              radiusLow * sinCache[I],
              radiusLow * cosCache[I],
              zLow);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            LNormal := Vector3fMake(
              sinCache[I] * xzNormalRatio,
              cosCache[I] * xzNormalRatio,
              zNormal);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(YVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;

            Attribute3f(attrPosition,
              radiusHigh * sinCache[I],
              radiusHigh * cosCache[I],
              zHigh);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);
            EmitVertex;
          end;
          RestartStrip;
        end;

        if alLine in Parts then
        begin
          Rstep := (FTopArrowHeadRadius - FTopRadius) / FLoops;
          Rstart := FTopRadius;
        end
        else
        begin
          Rstep := FTopArrowHeadRadius / FLoops;
          Rstart := 0;
        end;
          Attribute3f(attrNormal, 0, 0, -1);
          Attribute3f(attrTangent, -1, 0, 0);
          Attribute3f(attrBinormal, 0, -1, 0);
          for j := 0 to FLoops - 1 do
          begin
            for i := 0 to FSlices do
            begin
              Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
              R := Rstart + (j + 1) * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, headCentre);
              EmitVertex;
              Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
              R := Rstart + j * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, headCentre);
              EmitVertex;
            end;
            RestartStrip;
          end;
      end;

      // does the bottom arrow part - another cone
      if alBottomArrow in Parts then
      begin
        cylHeight := cylHeight - FBottomArrowHeadHeight * headInfluence;
        cylOffset := cylOffset + FBottomArrowHeadHeight * headInfluence;
        headCentre := - Height * 0.5 + FBottomArrowHeadHeight * headInfluence;
        length := SQRT(FBottomArrowHeadRadius * FBottomArrowHeadRadius +
          FBottomArrowHeadHeight * FBottomArrowHeadHeight);
        zNormal := FBottomArrowHeadRadius / length;
        xzNormalRatio := FBottomArrowHeadHeight / length;

        for J := 0 to FStacks - 1 do
        begin
          zLow := - j * FBottomArrowHeadHeight / FStacks + headCentre;
          zHigh := - (j + 1) * FBottomArrowHeadHeight / FStacks + headCentre;
          radiusLow := FBottomArrowHeadRadius * (1.0 - j / FStacks);
          radiusHigh := FBottomArrowHeadRadius * (1.0 - (j + 1) / FStacks);

          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition,
              radiusHigh * sinCache[I],
              radiusHigh * cosCache[I],
              zHigh);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(YVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            Attribute3f(attrBinormal, VectorCrossProduct(LNormal, LTangent));
            EmitVertex;

            Attribute3f(attrPosition,
              radiusLow * sinCache[I],
              radiusLow * cosCache[I],
              zLow);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            LNormal := Vector3fMake(
              sinCache[I] * xzNormalRatio,
              cosCache[I] * xzNormalRatio,
              zNormal);
            EmitVertex;
          end;
          RestartStrip;
        end;

        if alLine in Parts then
        begin
          Rstep := (FBottomArrowHeadRadius - FBottomRadius) / FLoops;
          Rstart := FBottomRadius;
        end
        else
        begin
          Rstep := FBottomArrowHeadRadius / FLoops;
          Rstart := 0;
        end;
          Attribute3f(attrNormal, 0, 0, 1);
          Attribute3f(attrTangent, 1, 0, 0);
          Attribute3f(attrBinormal, 0, 1, 0);
          for j := 0 to FLoops - 1 do
          begin
            for i := 0 to FSlices do
            begin
              Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
              R := Rstart + j * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, headCentre);
              EmitVertex;

              Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
              R := Rstart + (j + 1) * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, headCentre);
              EmitVertex;
            end;
            RestartStrip;
          end;
      end;

      // does the cylinder that makes the line
      if (cylHeight > 0) and (alLine in Parts) then
      begin
        deltaRadius := FBottomRadius - FTopRadius;
        length := SQRT(deltaRadius * deltaRadius + cylHeight * cylHeight);
        zNormal := deltaRadius / length;
        xzNormalRatio := cylHeight / length;

        for J := 0 to FStacks - 1 do
        begin
          zLow := j * FHeight / FStacks - 0.5 * cylHeight;
          zHigh := (j + 1) * FHeight / FStacks - 0.5 * cylHeight;
          radiusLow := FBottomRadius - deltaRadius * (j / FStacks);
          radiusHigh := FBottomRadius - deltaRadius * ((j + 1) / FStacks);
          radiusLow := radiusLow * xzNormalRatio;
          radiusHigh := radiusHigh * xzNormalRatio;
          Attribute3f(attrBinormal, ZVector);
          for I := 0 to FSlices do
          begin
            Attribute3f(attrPosition,
              radiusLow * sinCache[I],
              radiusLow * cosCache[I],
              zLow
              );
            Attribute2f(attrTexCoord0, 1 - I / FSlices, J / FStacks);
            LNormal := Vector3fMake(
              sinCache[I] * xzNormalRatio,
              cosCache[I] * xzNormalRatio,
              zNormal);
            Attribute3f(attrNormal, LNormal);
            LTangent := VectorCrossProduct(ZVector, LNormal);
            Attribute3f(attrTangent, LTangent);
            EmitVertex;

            Attribute3f(attrPosition,
              radiusHigh * sinCache[I],
              radiusHigh * cosCache[I],
              zHigh);
            Attribute2f(attrTexCoord0, 1 - I / FSlices, (J + 1) / FStacks);

            EmitVertex;
          end;
          RestartStrip;
        end;

        if not (alBottomArrow in Parts) then
        begin
          Rstep := FBottomRadius / FLoops;
          Attribute3f(attrNormal, 0, 0, -1);
          Attribute3f(attrTangent, -1, 0, 0);
          Attribute3f(attrBinormal, 0, -1, 0);
          for j := 0 to FLoops - 1 do
          begin
            for i := 0 to FSlices do
            begin
              Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
              R := (j + 1) * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, cylOffset);
              EmitVertex;

              Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
              R := j * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, cylOffset);
              EmitVertex;
            end;
            RestartStrip;
          end;
        end;

        if not (alTopArrow in Parts) then
        begin
          cylOffset := cylHeight + cylOffset;
          Rstep := FTopRadius / FLoops;
          Attribute3f(attrNormal, 0, 0, 1);
          Attribute3f(attrTangent, 1, 0, 0);
          Attribute3f(attrBinormal, 0, 1, 0);
          for j := 0 to FLoops - 1 do
          begin
            for i := 0 to FSlices do
            begin
              Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
              R := j * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, cylOffset);
              EmitVertex;

              Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
              R := (j + 1) * Rstep;
              Attribute3f(attrPosition, cosCache[i] * R, sinCache[i] * R, cylOffset);
              EmitVertex;
            end;
            RestartStrip;
          end;
        end;
      end;

      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  FBatch.Changed := True;
  ClearStructureChanged;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLArrowArc'}{$ENDIF}

procedure TGLArrowArc.BuildMesh;

type
  PVertexRec = ^TVertexRec;
  TVertexRec = record
    Position: TVector3f;
    Normal: TVector3f;
    Binormal: TVector3f;
    Tangent: TVector3f;
    TexCoord: TVector2f;
  end;

var
  i, j: integer;
  Theta, Phi, Theta1, cosPhi, sinPhi, dist: TGLFloat;
  cosTheta1, sinTheta1: TGLFloat;
  ringDelta, sideDelta: TGLFloat;
  ringDir: TAffineVector;
  iFact, jFact: Single;
  pVertex: PVertexRec;
  TanLoc, BinLoc: TGLInt;
  MeshSize: integer;
  MeshIndex: integer;
  ConeCenter: TVertexRec;
  StartOffset, StopOffset: Single;
  LVertices: array of array of TVertexRec;

begin
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

      BeginAssembly(mpTRIANGLE_STRIP);

      MeshIndex := 0;
      MeshSize := 0;
      // Check Parts
      if aaArc in FParts then
        MeshSize := MeshSize + FStacks + 1;
      if aaTopArrow in FParts then
        MeshSize := MeshSize + 3
      else
        MeshSize := MeshSize + 1;
      if aaBottomArrow in FParts then
        MeshSize := MeshSize + 3
      else
        MeshSize := MeshSize + 1;
      // Allocate Mesh
      SetLength(LVertices, MeshSize);

      case FHeadStackingStyle of
        ahssStacked:
          begin
            StartOffset := 0;
            StopOffset := 0;
          end;
        ahssCentered:
          begin
            if aaBottomArrow in Parts then
              StartOffset :=
                RadToDeg(ArcTan(0.5 * fBottomArrowHeadHeight / fArcRadius))
            else
              StartOffset :=0;
            if aaTopArrow in Parts then
              StopOffset :=
                RadToDeg(ArcTan(0.5 * fTopArrowHeadHeight / fArcRadius))
            else
              StopOffset :=0;
          end ;
        ahssIncluded:
          begin
            if aaBottomArrow in Parts then
              StartOffset := RadToDeg(ArcTan(fBottomArrowHeadHeight / fArcRadius))
            else
              StartOffset :=0;
            if aaTopArrow in Parts then
              StopOffset := RadToDeg(ArcTan(fTopArrowHeadHeight / fArcRadius))
            else
              StopOffset :=0;
          end ;
      end;

      // handle texture generation
      ringDelta := (((FStopAngle - StopOffset) - (FStartAngle + StartOffset)) /
        360) * c2PI / FStacks;
      sideDelta := c2PI / FSlices;

      iFact := 1 / FStacks;
      jFact := 1 / FSlices;
      if aaArc in FParts then
      begin
        Theta := DegToRad(FStartAngle + StartOffset) - ringDelta;
        for i := FStacks downto 0 do
        begin
          SetLength(LVertices[i], FSlices + 1);
          Theta1 := Theta + ringDelta;
          SinCos(Theta1, sinTheta1, cosTheta1);
          Phi := 0;
          for j := FSlices downto 0 do
          begin
            Phi := Phi + sideDelta;
            SinCos(Phi, sinPhi, cosPhi);
            dist := fArcRadius + Lerp(FTopRadius, FBottomRadius, i * iFact) * cosPhi;

            LVertices[i][j].Position := Vector3fMake(cosTheta1 * dist,
              -sinTheta1 * dist, Lerp(FTopRadius, FBottomRadius, i * iFact) * sinPhi);
            ringDir := LVertices[i][j].Position;
            ringDir[2] := 0.0;
            NormalizeVector(ringDir);
            LVertices[i][j].Normal := Vector3fMake(cosTheta1 * cosPhi,
              -sinTheta1 * cosPhi, sinPhi);
            LVertices[i][j].Tangent := VectorCrossProduct(ZVector, ringDir);
            LVertices[i][j].Binormal := VectorCrossProduct(LVertices[i][j].Normal,
              LVertices[i][j].Tangent);
            LVertices[i][j].TexCoord := Vector2fMake(i * iFact, j * jFact);
          end;
          Theta := Theta1;
        end;
        MeshIndex := FStacks + 1;

        for i := FStacks - 1 downto 0 do
        begin
          for j := FSlices downto 0 do
          begin
            Attribute3f(attrPosition, LVertices[i][j].Position);
            Attribute3f(attrNormal,   LVertices[i][j].Normal);
            Attribute3f(attrTangent,  LVertices[i][j].Tangent);
            Attribute3f(attrBinormal, LVertices[i][j].Binormal);
            Attribute2f(attrTexCoord0,LVertices[i][j].TexCoord);
            EmitVertex;

            Attribute3f(attrPosition, LVertices[i + 1][j].Position);
            Attribute3f(attrNormal,   LVertices[i + 1][j].Normal);
            Attribute3f(attrTangent,  LVertices[i + 1][j].Tangent);
            Attribute3f(attrBinormal, LVertices[i + 1][j].Binormal);
            Attribute2f(attrTexCoord0,LVertices[i + 1][j].TexCoord);
            EmitVertex;
          end;
          RestartStrip;
        end;

      end;

      // Build Arrow or start cap
      if aaBottomArrow in FParts then
      begin
        SetLength(LVertices[MeshIndex], FSlices + 1);
        SetLength(LVertices[MeshIndex + 1], FSlices + 1);
        SetLength(LVertices[MeshIndex + 2], FSlices + 1);
        Theta1 := DegToRad(FStartAngle + StartOffset);
        SinCos(Theta1, sinTheta1, cosTheta1);

        ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
          -sinTheta1 * fArcRadius, 0);

        Phi := 0;
        for j := FSlices downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCos(Phi, sinPhi, cosPhi);
          dist := fArcRadius + fBottomArrowHeadRadius * cosPhi;

          // Cap
          LVertices[MeshIndex][J].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, fBottomArrowHeadRadius * sinPhi);
          ringDir := LVertices[MeshIndex][j].Position;
          ringDir[2] := 0.0;
          NormalizeVector(ringDir);
          LVertices[MeshIndex][j].Normal := VectorCrossProduct(ringDir, ZVector);
          LVertices[MeshIndex][j].Tangent := ringDir;
          LVertices[MeshIndex][j].Binormal := ZVector;
          LVertices[MeshIndex][j].TexCoord := Vector2fMake(1, j * jFact);

          // Cone
          LVertices[MeshIndex+1][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, fBottomArrowHeadRadius * sinPhi);
          LVertices[MeshIndex+2][j].Position := VectorAdd(ConeCenter.Position,
            Vector3fMake(sinTheta1 * fBottomArrowHeadHeight,
            cosTheta1 * fBottomArrowHeadHeight, 0));

          LVertices[MeshIndex + 1][j].Tangent :=
            VectorNormalize(VectorSubtract(LVertices[MeshIndex + 1][j].Position,
            LVertices[MeshIndex + 2][j].Position));
          LVertices[MeshIndex + 2][j].Tangent := LVertices[MeshIndex + 1][j].Tangent;

          LVertices[MeshIndex + 1][j].Binormal := Vector3fMake(cosTheta1 * -sinPhi,
            sinTheta1 * sinPhi, cosPhi);
          LVertices[MeshIndex + 2][j].Binormal := LVertices[MeshIndex + 1][j].Binormal;

          LVertices[MeshIndex + 1][j].Normal :=
            VectorCrossProduct(LVertices[MeshIndex + 1][j].Binormal,
            LVertices[MeshIndex + 1][j].Tangent);
          LVertices[MeshIndex + 2][j].Normal := LVertices[MeshIndex + 1][j].Normal;

          LVertices[MeshIndex + 1][j].TexCoord := Vector2fMake(0, j * jFact);
          LVertices[MeshIndex + 2][j].TexCoord := Vector2fMake(1, j * jFact);
        end;

        ConeCenter.Normal := LVertices[MeshIndex][0].Normal;
        ConeCenter.Tangent := LVertices[MeshIndex][0].Tangent;
        ConeCenter.Binormal := LVertices[MeshIndex][0].Binormal;
        ConeCenter.TexCoord := Vector2fMake(0, 0);

        for j := FSlices downto 0 do
        begin
          Attribute3f(attrPosition, LVertices[MeshIndex][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex][j].TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, ConeCenter.Position);
          Attribute3f(attrNormal,   ConeCenter.Normal);
          Attribute3f(attrTangent,  ConeCenter.Tangent);
          Attribute3f(attrBinormal, ConeCenter.Binormal);
          Attribute2f(attrTexCoord0,ConeCenter.TexCoord);
          EmitVertex;
        end;
        RestartStrip;

        for j := FSlices downto 0 do
        begin
          Attribute3f(attrPosition, LVertices[MeshIndex + 1][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex + 1][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex + 1][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex + 1][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex + 1][j].TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, LVertices[MeshIndex + 2][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex + 2][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex + 2][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex + 2][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex + 2][j].TexCoord);
          EmitVertex;
        end;
        RestartStrip;
        MeshIndex := MeshIndex + 3;
      end
      else
      begin
        SetLength(LVertices[MeshIndex], FSlices + 1);
        Theta1 := DegToRad(FStartAngle);
        SinCos(Theta1, sinTheta1, cosTheta1);

        Phi := 0;
        for j := FSlices downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCos(Phi, sinPhi, cosPhi);
          dist := fArcRadius + fBottomRadius * cosPhi;
          LVertices[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, FBottomRadius * sinPhi);
          ringDir := LVertices[MeshIndex][j].Position;
          ringDir[2] := 0.0;
          NormalizeVector(ringDir);
          LVertices[MeshIndex][j].Normal := VectorCrossProduct(ZVector, ringDir);
          LVertices[MeshIndex][j].Tangent := ringDir;
          LVertices[MeshIndex][j].Binormal := ZVector;
          LVertices[MeshIndex][j].TexCoord := Vector2fMake(0, j * jFact);
        end;

        ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
          -sinTheta1 * fArcRadius, 0);
        ConeCenter.Normal := LVertices[MeshIndex][0].Normal;
        ConeCenter.Tangent := LVertices[MeshIndex][0].Tangent;
        ConeCenter.Binormal := LVertices[MeshIndex][0].Binormal;
        ConeCenter.TexCoord := Vector2fMake(1, 1);


        for j := 0 to FSlices do
        begin
          Attribute3f(attrPosition, ConeCenter.Position);
          Attribute3f(attrNormal,   ConeCenter.Normal);
          Attribute3f(attrTangent,  ConeCenter.Tangent);
          Attribute3f(attrBinormal, ConeCenter.Binormal);
          Attribute2f(attrTexCoord0,ConeCenter.TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, LVertices[MeshIndex][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex][j].TexCoord);
          EmitVertex;
        end;
        RestartStrip;
        MeshIndex := MeshIndex + 1;
      end;

      if aaTopArrow in FParts then
      begin
        SetLength(LVertices[MeshIndex], FSlices + 1);
        SetLength(LVertices[MeshIndex + 1], FSlices + 1);
        SetLength(LVertices[MeshIndex + 2], FSlices + 1);
        Theta1 := DegToRad(FStopAngle - StopOffset);
        SinCos(Theta1, sinTheta1, cosTheta1);

        ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
          -sinTheta1 * fArcRadius, 0);

        Phi := 0;
        for j := FSlices downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCos(Phi, sinPhi, cosPhi);
          dist := fArcRadius + fTopArrowHeadRadius * cosPhi;

          // Cap
          LVertices[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, fTopArrowHeadRadius * sinPhi);
          ringDir := LVertices[MeshIndex][j].Position;
          ringDir[2] := 0.0;
          NormalizeVector(ringDir);
          LVertices[MeshIndex][j].Normal := VectorCrossProduct(ZVector, ringDir);
          LVertices[MeshIndex][j].Tangent := ringDir;
          LVertices[MeshIndex][j].Binormal := ZVector;
          LVertices[MeshIndex][j].TexCoord := Vector2fMake(0, j * jFact);

          // Cone
          LVertices[MeshIndex + 1][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, fTopArrowHeadRadius * sinPhi);
          LVertices[MeshIndex + 2][j].Position := VectorSubtract(ConeCenter.Position,
            Vector3fMake(sinTheta1 * fTopArrowHeadHeight,
            cosTheta1 * fTopArrowHeadHeight, 0));

          LVertices[MeshIndex + 1][j].Tangent :=
            VectorNormalize(VectorSubtract(LVertices[MeshIndex + 2][j].Position,
            LVertices[MeshIndex + 1][j].Position));
          LVertices[MeshIndex + 2][j].Tangent := LVertices[MeshIndex + 1][j].Tangent;

          LVertices[MeshIndex + 1][j].Binormal := Vector3fMake(cosTheta1 * -sinPhi,
            sinTheta1 * sinPhi, cosPhi);
          LVertices[MeshIndex + 2][j].Binormal := LVertices[MeshIndex + 1][j].Binormal;

          LVertices[MeshIndex + 1][j].Normal :=
            VectorCrossProduct(LVertices[MeshIndex + 1][j].Binormal,
            LVertices[MeshIndex + 1][j].Tangent);
          LVertices[MeshIndex + 2][j].Normal := LVertices[MeshIndex + 1][j].Normal;

          LVertices[MeshIndex + 1][j].TexCoord := Vector2fMake(1, j * jFact);
          LVertices[MeshIndex + 2][j].TexCoord := Vector2fMake(0, j * jFact);
        end;

        ConeCenter.Normal := LVertices[MeshIndex][0].Normal;
        ConeCenter.Tangent := LVertices[MeshIndex][0].Tangent;
        ConeCenter.Binormal := LVertices[MeshIndex][0].Binormal;
        ConeCenter.TexCoord := Vector2fMake(1, 1);

        for j := FSlices downto 0 do
        begin
          Attribute3f(attrPosition, LVertices[MeshIndex][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex][j].TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, ConeCenter.Position);
          Attribute3f(attrNormal,   ConeCenter.Normal);
          Attribute3f(attrTangent,  ConeCenter.Tangent);
          Attribute3f(attrBinormal, ConeCenter.Binormal);
          Attribute2f(attrTexCoord0,ConeCenter.TexCoord);
          EmitVertex;
        end;
        RestartStrip;

        for j := FSlices downto 0 do
        begin
          Attribute3f(attrPosition, LVertices[MeshIndex + 2][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex + 2][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex + 2][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex + 2][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex + 2][j].TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, LVertices[MeshIndex + 1][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex + 1][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex + 1][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex + 1][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex + 1][j].TexCoord);
          EmitVertex;
        end;
        RestartStrip;
      end
      else
      begin
        SetLength(LVertices[MeshIndex], FSlices + 1);
        Theta1 := DegToRad(FStopAngle);
        SinCos(Theta1, sinTheta1, cosTheta1);

        Phi := 0;
        for j := FSlices downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCos(Phi, sinPhi, cosPhi);
          dist := fArcRadius + fTopRadius * cosPhi;
          LVertices[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, fTopRadius * sinPhi);
          ringDir := LVertices[MeshIndex][j].Position;
          ringDir[2] := 0.0;
          NormalizeVector(ringDir);
          LVertices[MeshIndex][j].Normal := VectorCrossProduct(ringDir, ZVector);
          LVertices[MeshIndex][j].Tangent := ringDir;
          LVertices[MeshIndex][j].Binormal := VectorNegate(ZVector);
          LVertices[MeshIndex][j].TexCoord := Vector2fMake(1, j * jFact);
        end;
        ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
          -sinTheta1 * fArcRadius, 0);
        ConeCenter.Normal := LVertices[MeshIndex][0].Normal;
        ConeCenter.Tangent := LVertices[MeshIndex][0].Tangent;
        ConeCenter.Binormal := LVertices[MeshIndex][0].Binormal;
        ConeCenter.TexCoord := Vector2fMake(0, 0);

        for j := 0 to FSlices do
        begin
          Attribute3f(attrPosition, ConeCenter.Position);
          Attribute3f(attrNormal,   ConeCenter.Normal);
          Attribute3f(attrTangent,  ConeCenter.Tangent);
          Attribute3f(attrBinormal, ConeCenter.Binormal);
          Attribute2f(attrTexCoord0,ConeCenter.TexCoord);
          EmitVertex;

          Attribute3f(attrPosition, LVertices[MeshIndex][j].Position);
          Attribute3f(attrNormal,   LVertices[MeshIndex][j].Normal);
          Attribute3f(attrTangent,  LVertices[MeshIndex][j].Tangent);
          Attribute3f(attrBinormal, LVertices[MeshIndex][j].Binormal);
          Attribute2f(attrTexCoord0,LVertices[MeshIndex][j].TexCoord);
          EmitVertex;
        end;
        RestartStrip;
      end;

      EndAssembly;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;
  FBatch.Changed := True;
  ClearStructureChanged;
end;

// Create
//

constructor TGLArrowArc.Create(AOwner: TComponent);
begin
  inherited;
  FStacks := 16;
  fArcRadius := 0.5;
  FStartAngle := 0;
  FStopAngle := 360;
  FTopRadius := 0.1;
  BottomRadius := 0.1;
  fTopArrowHeadRadius := 0.2;
  fTopArrowHeadHeight := 0.5;
  fBottomArrowHeadRadius := 0.2;
  fBottomArrowHeadHeight := 0.5;
  FHeadStackingStyle := ahssStacked;
  FParts := [aaArc, aaTopArrow];
  FBatch.Mesh.TagName := ClassName;
end;

// SetArcRadius
//

procedure TGLArrowArc.SetArcRadius(const aValue: Single);
begin
  if fArcRadius <> aValue then
  begin
    fArcRadius := aValue;
    StructureChanged;
  end;
end;

// SetStartAngle
//

procedure TGLArrowArc.SetStartAngle(const aValue: Single);
begin
  if FStartAngle <> aValue then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

// SetStopAngle
//

procedure TGLArrowArc.SetStopAngle(const aValue: Single);
begin
  if FStopAngle <> aValue then
  begin
    FStopAngle := aValue;
    StructureChanged;
  end;
end;

// SetTopRadius
//

procedure TGLArrowArc.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopArrowHeadHeight
//

procedure TGLArrowArc.SetTopArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fTopArrowHeadHeight then
  begin
    fTopArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

// SetTopArrowHeadRadius
//

procedure TGLArrowArc.SetTopArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fTopArrowHeadRadius then
  begin
    fTopArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadHeight
//

procedure TGLArrowArc.SetBottomArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadHeight then
  begin
    fBottomArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadRadius
//

procedure TGLArrowArc.SetBottomArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadRadius then
  begin
    fBottomArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TGLArrowArc.SetParts(aValue: TArrowArcParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetHeadStackingStyle
//

procedure TGLArrowArc.SetHeadStackingStyle(const val: TArrowHeadStackingStyle);
begin
  if val <> FHeadStackingStyle then
  begin
    FHeadStackingStyle := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGLArrowArc.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLArrowLine) then
  begin
    FStartAngle := TGLArrowArc(Source).FStartAngle;
    FStopAngle := TGLArrowArc(Source).FStopAngle;
    fArcRadius := TGLArrowArc(Source).fArcRadius;
    FParts := TGLArrowArc(Source).FParts;
    FTopRadius := TGLArrowArc(Source).FTopRadius;
    fTopArrowHeadHeight := TGLArrowArc(Source).fTopArrowHeadHeight;
    fTopArrowHeadRadius := TGLArrowArc(Source).fTopArrowHeadRadius;
    fBottomArrowHeadHeight := TGLArrowArc(Source).fBottomArrowHeadHeight;
    fBottomArrowHeadRadius := TGLArrowArc(Source).fBottomArrowHeadRadius;
    FHeadStackingStyle := TGLArrowArc(Source).FHeadStackingStyle;
  end;
  inherited Assign(Source);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLFrustrum'}{$ENDIF}

constructor TGLFrustrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FApexHeight := 1;
  FBaseWidth := 1;
  FBaseDepth := 1;
  FHeight := 0.5;
  FParts := cAllFrustrumParts;
  FNormalDirection := ndOutside;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;

  FBatch.Mesh.TagName := ClassName;
end;

procedure TGLFrustrum.BuildMesh;
var
  HBW, HBD: TGLFloat; // half of width, half of depth at base
  HTW, HTD: TGLFloat; // half of width, half of depth at top of frustrum
  HFH: TGLFloat; // half of height, for align to center
  Angle: TGLFloat; // in radians
  ASin, ACos: TGLFloat;
  LNormal: TVector3f;
begin
  HBW := FBaseWidth * 0.5;
  HBD := FBaseDepth * 0.5;
  HTW := HBW * (FApexHeight - FHeight) / FApexHeight;
  HTD := HBD * (FApexHeight - FHeight) / FApexHeight;
  HFH := FHeight * 0.5;

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

      BeginAssembly(mpTRIANGLE_STRIP);

      if [fpFront, fpBack] * FParts <> [] then
      begin
        Angle := Arctan(FApexHeight / HBD);
          // angle of front plane with bottom plane
        SinCos(Angle, ASin, ACos);
        if fpFront in FParts then
        begin
          LNormal := Vector3fMake(0, ACos, ASin);
          Attribute3f(attrNormal, LNormal);
          Attribute3f(attrTangent, XVector);
          Attribute3f(attrBinormal, VectorCrossProduct(LNormal, XVector));

          Attribute3f(attrPosition, HTW, HFH, HTD);
          Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HTW, HFH, HTD);
          Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, HBW, -HFH, HBD);
          Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HBW, -HFH, HBD);
          Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
          EmitVertex;
          RestartStrip;
        end;

        if fpBack in FParts then
        begin
          LNormal := Vector3fMake(0, ACos, -ASin);
          Attribute3f(attrNormal, LNormal);
          Attribute3f(attrTangent, MinusXVector);
          Attribute3f(attrBinormal, VectorCrossProduct(LNormal, MinusXVector));

          Attribute3f(attrPosition, HTW, HFH, -HTD);
          Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, HBW, -HFH, -HBD);
          Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HTW, HFH, -HTD);
          Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HBW, -HFH, -HBD);
          Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
          EmitVertex;
          RestartStrip;
        end;
      end;

      if [fpLeft, fpRight] * FParts <> [] then
      begin
        Angle := Arctan(FApexHeight / HBW);
          // angle of side plane with bottom plane
        SinCos(Angle, ASin, ACos);
        if fpLeft in FParts then
        begin
          LNormal := Vector3fMake(-ASin, ACos, 0);
          Attribute3f(attrNormal, LNormal);
          Attribute3f(attrTangent, MinusZVector);
          Attribute3f(attrBinormal, VectorCrossProduct(LNormal, MinusZVector));

          Attribute3f(attrPosition, -HTW, HFH, HTD);
          Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HTW, HFH, -HTD);
          Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HBW, -HFH, HBD);
          Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, -HBW, -HFH, -HBD);
          Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
          EmitVertex;
          RestartStrip;
        end;
        if fpRight in FParts then
        begin
          LNormal := Vector3fMake(ASin, ACos, 0);
          Attribute3f(attrNormal, LNormal);
          Attribute3f(attrTangent, ZVector);
          Attribute3f(attrBinormal, VectorCrossProduct(LNormal, ZVector));

          Attribute3f(attrPosition, HTW, HFH, HTD);
          Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, HBW, -HFH, HBD);
          Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, HTW, HFH, -HTD);
          Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
          EmitVertex;

          Attribute3f(attrPosition, HBW, -HFH, -HBD);
          Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
          EmitVertex;
          RestartStrip;
        end;
      end;

      if (fpTop in FParts) and (FHeight < FApexHeight) then
      begin
        Attribute3f(attrNormal, YVector);
        Attribute3f(attrTangent, XVector);
        Attribute3f(attrBinormal, ZVector);

        Attribute3f(attrPosition, -HTW, HFH, -HTD);
        Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, -HTW, HFH, HTD);
        Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, HTW, HFH, -HTD);
        Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, HTW, HFH, HTD);
        Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
        EmitVertex;
        RestartStrip;
      end;
      if fpBottom in FParts then
      begin
        Attribute3f(attrNormal, YVector);
        Attribute3f(attrTangent, XVector);
        Attribute3f(attrBinormal, ZVector);

        Attribute3f(attrPosition, -HBW, -HFH, -HBD);
        Attribute2f(attrTexCoord0, NullTexPoint.S, NullTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, HBW, -HFH, -HBD);
        Attribute2f(attrTexCoord0, XTexPoint.S, XTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, -HBW, -HFH, HBD);
        Attribute2f(attrTexCoord0, YTexPoint.S, YTexPoint.T);
        EmitVertex;

        Attribute3f(attrPosition, HBW, -HFH, HBD);
        Attribute2f(attrTexCoord0, XYTexPoint.S, XYTexPoint.T);
        EmitVertex;
        RestartStrip;
      end;

      EndAssembly;
      if FNormalDirection = ndInside then
      begin
        Triangulate;
        FlipFaces;
      end;
      ApplyExtras;
    finally
      UnLock;
    end;
  end;

  FBatch.Changed := True;
  ClearStructureChanged;
end;

procedure TGLFrustrum.SetApexHeight(const aValue: Single);
begin
  if (aValue <> FApexHeight) and (aValue >= 0) then
  begin
    FApexHeight := aValue;
    if FHeight > aValue then
      FHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseDepth(const aValue: Single);
begin
  if (aValue <> FBaseDepth) and (aValue >= 0) then
  begin
    FBaseDepth := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseWidth(const aValue: Single);
begin
  if (aValue <> FBaseWidth) and (aValue >= 0) then
  begin
    FBaseWidth := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetHeight(const aValue: Single);
begin
  if (aValue <> FHeight) and (aValue >= 0) then
  begin
    FHeight := aValue;
    if FApexHeight < aValue then
      FApexHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetParts(aValue: TFrustrumParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFrustrum) then
  begin
    FApexHeight := TGLFrustrum(Source).FApexHeight;
    FBaseDepth := TGLFrustrum(Source).FBaseDepth;
    FBaseWidth := TGLFrustrum(Source).FBaseWidth;
    FHeight := TGLFrustrum(Source).FHeight;
    FParts := TGLFrustrum(Source).FParts;
    FNormalDirection := TGLFrustrum(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TGLFrustrum.TopDepth: TGLFloat;
begin
  Result := FBaseDepth * (FApexHeight - FHeight) / FApexHeight;
end;

function TGLFrustrum.TopWidth: TGLFloat;
begin
  Result := FBaseWidth * (FApexHeight - FHeight) / FApexHeight;
end;

procedure TGLFrustrum.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('FrustrumSize', ReadData, WriteData,
    (FApexHeight <> 1) or (FBaseDepth <> 1) or (FBaseWidth <> 1) or
    (FHeight <> 0.5));
end;

procedure TGLFrustrum.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FApexHeight, SizeOf(FApexHeight));
    Read(FBaseDepth, SizeOf(FBaseDepth));
    Read(FBaseWidth, SizeOf(FBaseWidth));
    Read(FHeight, SizeOf(FHeight));
  end;
end;

procedure TGLFrustrum.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FApexHeight, SizeOf(FApexHeight));
    Write(FBaseDepth, SizeOf(FBaseDepth));
    Write(FBaseWidth, SizeOf(FBaseWidth));
    Write(FHeight, SizeOf(FHeight));
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLPolygon'}{$ENDIF}

// Create
//

constructor TGLPolygon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParts := [ppTop, ppBottom];
end;

// Destroy
//

destructor TGLPolygon.Destroy;
begin
  inherited Destroy;
end;

// SetParts
//

procedure TGLPolygon.SetParts(const val: TPolygonParts);
begin
  if FParts <> val then
  begin
    FParts := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGLPolygon.Assign(Source: TPersistent);
begin
  if Source is TGLPolygon then
  begin
    FParts := TGLPolygon(Source).FParts;
  end;
  inherited Assign(Source);
end;

// BuildList
//

procedure TGLPolygon.BuildList(var rci: TRenderContextInfo);
var
  Normal: TAffineVector;
  pNorm: PAffineVector;
begin
  if (Nodes.Count > 1) then
  begin
    Normal := Nodes.Normal;
    if VectorIsNull(Normal) then
      pNorm := nil
    else
      pNorm := @Normal;
    if ppTop in FParts then
    begin
      if SplineMode = lsmLines then
        Nodes.RenderTesselatedPolygon(true, pNorm, 1)
      else
        Nodes.RenderTesselatedPolygon(true, pNorm, Division);
    end;
    // tessellate bottom polygon
    if ppBottom in FParts then
    begin
      if Assigned(pNorm) then
        NegateVector(Normal);
      if SplineMode = lsmLines then
        Nodes.RenderTesselatedPolygon(true, pNorm, 1, true)
      else
        Nodes.RenderTesselatedPolygon(true, pNorm, Division, true);
    end;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLDodecahedron'}{$ENDIF}

procedure TGLDodecahedron.BuildMesh;
begin
  DodecahedronBuildMesh(FBatch.Mesh, 1.0);
  with FBatch.Mesh do
  begin
    Lock;
    try
      ApplyExtras;
    finally
      UnLock;
    end;
  end;
  FBatch.Changed := True;
  ClearStructureChanged;
end;

constructor TGLDodecahedron.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
  FBatch.Mesh.TagName := ClassName;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGLIcosahedron'}{$ENDIF}

procedure TGLIcosahedron.BuildMesh;
begin
  IcosahedronBuildMesh(FBatch.Mesh, 1.0);
  with FBatch.Mesh do
  begin
    Lock;
    try
      ApplyExtras;
    finally
      UnLock;
    end;
  end;
  FBatch.Changed := True;
  ClearStructureChanged;
end;

constructor TGLIcosahedron.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
  FBatch.Mesh.TagName := ClassName;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization

RegisterClasses([TGLCylinder, TGLCone, TGLTorus, TGLDisk, TGLGeoSphere,
  TGLArrowLine, TGLAnnulus, TGLFrustrum, TGLPolygon, TGLCapsule, TGLArrowArc,
  TGLDodecahedron, TGLIcosahedron]);

end.
