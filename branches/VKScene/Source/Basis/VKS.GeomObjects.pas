//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{  Geometric objects }

unit VKS.GeomObjects;

{$I VKScene.inc}

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  //VKS
//  VKS.OpenGLAdapter,
  VKS.Scene,
  VKS.VectorGeometry,
  VKS.Context,
  VKS.Objects,
  VKS.Silhouette,
  VKS.VectorTypes,
  VKS.GeometryBB,
  VKS.RenderContextInfo,
  VKS.Polynomials,
  VKS.XOpenGL;

type
  // TVKDisk
  //
  { A Disk object. 
    The disk may not be complete, it can have a hole (controled by the
    InnerRadius property) and can only be a slice (controled by the StartAngle
    and SweepAngle properties). }
  TVKDisk = class(TVKQuadricObject)
  private
    { Private Declarations }
    FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius: GLfloat;
    FSlices, FLoops: GLint;
    procedure SetOuterRadius(const aValue: Single);
    procedure SetInnerRadius(const aValue: Single);
    procedure SetSlices(aValue: GLint);
    procedure SetLoops(aValue: GLint);
    procedure SetStartAngle(const aValue: Single);
    procedure SetSweepAngle(const aValue: Single);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;

  published
    { Published Declarations }
    { Allows defining a "hole" in the disk. }
    property InnerRadius: GLfloat read FInnerRadius write SetInnerRadius;
    { Number of radial mesh subdivisions. }
    property Loops: GLint read FLoops write SetLoops default 2;
    { Outer radius for the disk. 
      If you leave InnerRadius at 0, this is the disk radius. }
    property OuterRadius: GLfloat read FOuterRadius write SetOuterRadius;
    { Number of mesh slices. 
      For instance, if Slices=6, your disk will look like an hexagon. }
    property Slices: GLint read FSlices write SetSlices default 16;
    property StartAngle: GLfloat read FStartAngle write SetStartAngle;
    property SweepAngle: GLfloat read FSweepAngle write SetSweepAngle;
  end;

  // TVKCylinderBase
  //
  { Base class to cylinder-like objects. 
    Introduces the basic cylinder description properties. 
    Be aware teh default slices and stacks make up for a high-poly cylinder,
    unless you're after high-quality lighting it is recommended to reduce the
    Stacks property to 1. }
  TVKCylinderBase = class(TVKQuadricObject)
  private
    { Private Declarations }
    FBottomRadius: GLfloat;
    FSlices, FStacks, FLoops: GLint;
    FHeight: GLfloat;

  protected
    { Protected Declarations }
    procedure SetBottomRadius(const aValue: Single);
    procedure SetHeight(const aValue: Single);
    procedure SetSlices(aValue: GLint);
    procedure SetStacks(aValue: GLint);
    procedure SetLoops(aValue: GLint);
    function GetTopRadius: Single; virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;

    function GenerateSilhouette(const silhouetteParameters
      : TVKSilhouetteParameters): TVKSilhouette; override;
  published
    { Published Declarations }
    property BottomRadius: GLfloat read FBottomRadius write SetBottomRadius;
    property Height: GLfloat read FHeight write SetHeight;
    property Slices: GLint read FSlices write SetSlices default 16;
    property Stacks: GLint read FStacks write SetStacks default 4;
    { Number of concentric rings for top/bottom disk(s). }
    property Loops: GLint read FLoops write SetLoops default 1;
  end;

  // TVKConePart
  //
  TVKConePart = (coSides, coBottom);
  TVKConeParts = set of TVKConePart;

  // TVKCone
  //
  { A cone object. }
  TVKCone = class(TVKCylinderBase)
  private
    { Private Declarations }
    FParts: TVKConeParts;

  protected
    { Protected Declarations }
    procedure SetParts(aValue: TVKConeParts);
    function GetTopRadius: Single; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;

  published
    { Published Declarations }
    property Parts: TVKConeParts read FParts write SetParts
      default [coSides, coBottom];
  end;

  // TVKCylinderPart
  //
  TVKCylinderPart = (cySides, cyBottom, cyTop);
  TVKCylinderParts = set of TVKCylinderPart;

  // TVKCylinderAlignment
  //
  TVKCylinderAlignment = (caCenter, caTop, caBottom);

  // TVKCylinder
  //
  { Cylinder object, can also be used to make truncated cones }
  TVKCylinder = class(TVKCylinderBase)
  private
    { Private Declarations }
    FParts: TVKCylinderParts;
    FTopRadius: GLfloat;
    FAlignment: TVKCylinderAlignment;

  protected
    { Protected Declarations }
    procedure SetTopRadius(const aValue: Single);
    procedure SetParts(aValue: TVKCylinderParts);
    procedure SetAlignment(val: TVKCylinderAlignment);
    function GetTopRadius: Single; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;

    procedure Align(const startPoint, endPoint: TVector); overload;
    procedure Align(const startObj, endObj: TVKBaseSceneObject); overload;
    procedure Align(const startPoint, endPoint: TAffineVector); overload;

  published
    { Published Declarations }
    property TopRadius: GLfloat read FTopRadius write SetTopRadius;
    property Parts: TVKCylinderParts read FParts write SetParts
      default [cySides, cyBottom, cyTop];
    property Alignment: TVKCylinderAlignment read FAlignment write SetAlignment
      default caCenter;
  end;

  { Capsule object, can also be used to make truncated cones }
  TVKCapsule = class(TVKSceneObject)
  private
    { Private Declarations }
    FParts: TVKCylinderParts;
    FRadius: GLfloat;
    FSlices: GLint;
    FStacks: GLint;
    FHeight: GLfloat;
    FAlignment: TVKCylinderAlignment;
  protected
    { Protected Declarations }
    procedure SetHeight(const aValue: Single);
    procedure SetRadius(const aValue: Single);
    procedure SetSlices(const aValue: integer);
    procedure SetStacks(const aValue: integer);
    procedure SetParts(aValue: TVKCylinderParts);
    procedure SetAlignment(val: TVKCylinderAlignment);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    procedure Align(const startPoint, endPoint: TVector); overload;
    procedure Align(const startObj, endObj: TVKBaseSceneObject); overload;
    procedure Align(const startPoint, endPoint: TAffineVector); overload;
  published
    { Published Declarations }
    property Height: GLfloat read FHeight write SetHeight;
    property Slices: GLint read FSlices write SetSlices;
    property Stacks: GLint read FStacks write SetStacks;
    property Radius: GLfloat read FRadius write SetRadius;
    property Parts: TVKCylinderParts read FParts write SetParts
      default [cySides, cyBottom, cyTop];
    property Alignment: TVKCylinderAlignment read FAlignment write SetAlignment
      default caCenter;
  end;

  // TVKAnnulusPart
  //
  TVKAnnulusPart = (anInnerSides, anOuterSides, anBottom, anTop);
  TVKAnnulusParts = set of TVKAnnulusPart;

  // TVKAnnulus
  //
  { An annulus is a cylinder that can be made hollow (pipe-like). }
  TVKAnnulus = class(TVKCylinderBase)
  private
    { Private Declarations }
    FParts: TVKAnnulusParts;
    FBottomInnerRadius: GLfloat;
    FTopInnerRadius: GLfloat;
    FTopRadius: GLfloat;

  protected
    { Protected Declarations }
    procedure SetTopRadius(const aValue: Single);
    procedure SetTopInnerRadius(const aValue: Single);
    procedure SetBottomInnerRadius(const aValue: Single);
    procedure SetParts(aValue: TVKAnnulusParts);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;

  published
    { Published Declarations }
    property BottomInnerRadius: GLfloat read FBottomInnerRadius
      write SetBottomInnerRadius;
    property TopInnerRadius: GLfloat read FTopInnerRadius
      write SetTopInnerRadius;
    property TopRadius: GLfloat read FTopRadius write SetTopRadius;
    property Parts: TVKAnnulusParts read FParts write SetParts
      default [anInnerSides, anOuterSides, anBottom, anTop];
  end;

  // TVKTorusPart
  //
  TVKTorusPart = (toSides, toStartDisk, toStopDisk);
  TVKTorusParts = set of TVKTorusPart;

  // TVKTorus
  //
  { A Torus object. }
  TVKTorus = class(TVKSceneObject)
  private
    { Private Declarations }
    FParts: TVKTorusParts;
    FRings, FSides: Cardinal;
    FStartAngle, FStopAngle: Single;
    FMinorRadius, FMajorRadius: Single;
    FMesh: array of array of TVertexRec;
  protected
    { Protected Declarations }
    procedure SetMajorRadius(const aValue: Single);
    procedure SetMinorRadius(const aValue: Single);
    procedure SetRings(aValue: Cardinal);
    procedure SetSides(aValue: Cardinal);
    procedure SetStartAngle(const aValue: Single);
    procedure SetStopAngle(const aValue: Single);
    procedure SetParts(aValue: TVKTorusParts);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;

  published
    { Published Declarations }
    property MajorRadius: Single read FMajorRadius write SetMajorRadius;
    property MinorRadius: Single read FMinorRadius write SetMinorRadius;
    property Rings: Cardinal read FRings write SetRings default 25;
    property Sides: Cardinal read FSides write SetSides default 15;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property Parts: TVKTorusParts read FParts write SetParts default [toSides];
  end;

  // TVKArrowLinePart
  //
  TVKArrowLinePart = (alLine, alTopArrow, alBottomArrow);
  TVKArrowLineParts = set of TVKArrowLinePart;

  // TVKArrowHeadStackingStyle
  //
  TVKArrowHeadStackingStyle = (ahssStacked, ahssCentered, ahssIncluded);

  // TVKArrowLine
  //
  { Draws an arrowhead (cylinder + cone).
    The arrow head is a cone that shares the attributes of the cylinder
    (ie stacks/slices, materials etc). Seems to work ok.
    This is useful for displaying a vector based field (eg velocity) or
    other arrows that might be required.
    By default the bottom arrow is off }
  TVKArrowLine = class(TVKCylinderBase)
  private
    { Private Declarations }
    FParts: TVKArrowLineParts;
    FTopRadius: Single;
    fTopArrowHeadHeight: Single;
    fTopArrowHeadRadius: Single;
    fBottomArrowHeadHeight: Single;
    fBottomArrowHeadRadius: Single;
    FHeadStackingStyle: TVKArrowHeadStackingStyle;

  protected
    { Protected Declarations }
    procedure SetTopRadius(const aValue: Single);
    procedure SetTopArrowHeadHeight(const aValue: Single);
    procedure SetTopArrowHeadRadius(const aValue: Single);
    procedure SetBottomArrowHeadHeight(const aValue: Single);
    procedure SetBottomArrowHeadRadius(const aValue: Single);
    procedure SetParts(aValue: TVKArrowLineParts);
    procedure SetHeadStackingStyle(const val: TVKArrowHeadStackingStyle);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property TopRadius: GLfloat read FTopRadius write SetTopRadius;
    property HeadStackingStyle: TVKArrowHeadStackingStyle read FHeadStackingStyle
      write SetHeadStackingStyle default ahssStacked;
    property Parts: TVKArrowLineParts read FParts write SetParts
      default [alLine, alTopArrow];
    property TopArrowHeadHeight: GLfloat read fTopArrowHeadHeight
      write SetTopArrowHeadHeight;
    property TopArrowHeadRadius: GLfloat read fTopArrowHeadRadius
      write SetTopArrowHeadRadius;
    property BottomArrowHeadHeight: GLfloat read fBottomArrowHeadHeight
      write SetBottomArrowHeadHeight;
    property BottomArrowHeadRadius: GLfloat read fBottomArrowHeadRadius
      write SetBottomArrowHeadRadius;
  end;

  // TArrowArcPart
  //
  TArrowArcPart = (aaArc, aaTopArrow, aaBottomArrow);
  TArrowArcParts = set of TArrowArcPart;

  // TVKArrowArc
  //
  { Draws an arrowhead (Sliced Torus + cone). 
    The arrow head is a cone that shares the attributes of the Torus
    (ie stacks/slices, materials etc). 
    This is useful for displaying a movement (eg twist) or
    other arc arrows that might be required. 
    By default the bottom arrow is off }
  TVKArrowArc = class(TVKCylinderBase)
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
    FHeadStackingStyle: TVKArrowHeadStackingStyle;
    FMesh: array of array of TVertexRec;

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
    procedure SetHeadStackingStyle(const val: TVKArrowHeadStackingStyle);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property ArcRadius: GLfloat read fArcRadius write SetArcRadius;
    property StartAngle: GLfloat read FStartAngle write SetStartAngle;
    property StopAngle: GLfloat read FStopAngle write SetStopAngle;
    property TopRadius: GLfloat read FTopRadius write SetTopRadius;
    property HeadStackingStyle: TVKArrowHeadStackingStyle read FHeadStackingStyle
      write SetHeadStackingStyle default ahssStacked;
    property Parts: TArrowArcParts read FParts write SetParts
      default [aaArc, aaTopArrow];
    property TopArrowHeadHeight: GLfloat read fTopArrowHeadHeight
      write SetTopArrowHeadHeight;
    property TopArrowHeadRadius: GLfloat read fTopArrowHeadRadius
      write SetTopArrowHeadRadius;
    property BottomArrowHeadHeight: GLfloat read fBottomArrowHeadHeight
      write SetBottomArrowHeadHeight;
    property BottomArrowHeadRadius: GLfloat read fBottomArrowHeadRadius
      write SetBottomArrowHeadRadius;
  end;

  // TPolygonParts
  //
  TPolygonPart = (ppTop, ppBottom);
  TPolygonParts = set of TPolygonPart;

  // TVKPolygon
  //
  { A basic polygon object. 
    The curve is described by the Nodes and SplineMode properties, should be
    planar and is automatically tessellated. 
    Texture coordinates are deduced from X and Y coordinates only. 
    This object allows only for polygons described by a single curve, if you
    need "complex polygons" with holes, patches and cutouts, see GLMultiPolygon. }
  TVKPolygon = class(TVKPolygonBase)
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
    procedure BuildList(var rci: TVKRenderContextInfo); override;

  published
    { Published Declarations }
    { Parts of polygon. 
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
  // TVKFrustrum
  //
  { A frustrum is a pyramid with the top chopped off. 
    The height of the imaginary pyramid is ApexHeight, the height of the
    frustrum is Height. If ApexHeight and Height are the same, the frustrum
    degenerates into a pyramid. 
    Height cannot be greater than ApexHeight. }
  TVKFrustrum = class(TVKSceneObject)
  private
    { Private Declarations }
    FApexHeight, FBaseDepth, FBaseWidth, FHeight: GLfloat;
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

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function TopDepth: GLfloat;
    function TopWidth: GLfloat;
    function AxisAlignedBoundingBoxUnscaled: TAABB;
    function AxisAlignedDimensionsUnscaled: TVector; override;
  published
    { Published Declarations }
    property ApexHeight: GLfloat read FApexHeight write SetApexHeight
      stored False;
    property BaseDepth: GLfloat read FBaseDepth write SetBaseDepth
      stored False;
    property BaseWidth: GLfloat read FBaseWidth write SetBaseWidth
      stored False;
    property Height: GLfloat read FHeight write SetHeight stored False;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Parts: TFrustrumParts read FParts write SetParts
      default cAllFrustrumParts;
  end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKDisk ------------------
// ------------------

// Create
//

constructor TVKDisk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius := 0.5;
  FInnerRadius := 0;
  FSlices := 16;
  FLoops := 2;
  FStartAngle := 0;
  FSweepAngle := 360;
end;

// BuildList
//

procedure TVKDisk.BuildList(var rci: TVKRenderContextInfo);
var
  quadric: GLUquadricObj;
begin
  quadric := gluNewQuadric();
  SetupQuadricParams(quadric);
  gluPartialDisk(quadric, FInnerRadius, FOuterRadius, FSlices, FLoops,
    FStartAngle, FSweepAngle);
  gluDeleteQuadric(quadric);
end;

// SetOuterRadius
//

procedure TVKDisk.SetOuterRadius(const aValue: Single);
begin
  if aValue <> FOuterRadius then
  begin
    FOuterRadius := aValue;
    StructureChanged;
  end;
end;

// SetInnerRadius
//

procedure TVKDisk.SetInnerRadius(const aValue: Single);
begin
  if aValue <> FInnerRadius then
  begin
    FInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TVKDisk.SetSlices(aValue: integer);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetLoops
//

procedure TVKDisk.SetLoops(aValue: integer);
begin
  if aValue <> FLoops then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

// SetStartAngle
//

procedure TVKDisk.SetStartAngle(const aValue: Single);
begin
  if aValue <> FStartAngle then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

// SetSweepAngle
//

procedure TVKDisk.SetSweepAngle(const aValue: Single);
begin
  if aValue <> FSweepAngle then
  begin
    FSweepAngle := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKDisk.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKDisk) then
  begin
    FOuterRadius := TVKDisk(Source).FOuterRadius;
    FInnerRadius := TVKDisk(Source).FInnerRadius;
    FSlices := TVKDisk(Source).FSlices;
    FLoops := TVKDisk(Source).FLoops;
    FStartAngle := TVKDisk(Source).FStartAngle;
    FSweepAngle := TVKDisk(Source).FSweepAngle;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKDisk.AxisAlignedDimensionsUnscaled: TVector;
var
  r: GLfloat;
begin
  r := Abs(FOuterRadius);
  Result := VectorMake(r, r, 0);
end;

// RayCastIntersect
//

function TVKDisk.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  ip: TVector;
  d: Single;
  angle, beginAngle, endAngle: Single;
  localIntPoint: TVector;
begin
  Result := False;
  if SweepAngle > 0 then
    if RayCastPlaneIntersect(rayStart, rayVector, AbsolutePosition,
      AbsoluteDirection, @ip) then
    begin
      if Assigned(intersectPoint) then
        SetVector(intersectPoint^, ip);
      localIntPoint := AbsoluteToLocal(ip);
      d := VectorNorm(localIntPoint);
      if (d >= Sqr(InnerRadius)) and (d <= Sqr(OuterRadius)) then
      begin
        if SweepAngle >= 360 then
          Result := true
        else
        begin
          // arctan2 returns results between -pi and +pi, we want between 0 and 360
          angle := 180 / pi * ArcTangent2(localIntPoint.X, localIntPoint.Y);
          if angle < 0 then
            angle := angle + 360;
          // we also want StartAngle and StartAngle+SweepAngle to be in this range
          beginAngle := Trunc(StartAngle) mod 360;
          endAngle := Trunc(StartAngle + SweepAngle) mod 360;
          // If beginAngle>endAngle then area crosses the boundary from 360=>0 degrees
          // therefore have 2 valid regions  (beginAngle to 360) & (0 to endAngle)
          // otherwise just 1 valid region (beginAngle to endAngle)
          if beginAngle > endAngle then
          begin
            if (angle > beginAngle) or (angle < endAngle) then
              Result := true;
          end
          else if (angle > beginAngle) and (angle < endAngle) then
            Result := true;
        end;
      end;
    end;
  if Result = true then
    if Assigned(intersectNormal) then
      SetVector(intersectNormal^, AbsoluteUp);

end;

// ------------------
// ------------------ TVKCylinderBase ------------------
// ------------------

// Create
//

constructor TVKCylinderBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBottomRadius := 0.5;
  FHeight := 1;
  FSlices := 16;
  FStacks := 4;
  FLoops := 1;
end;

// SetBottomRadius
//

procedure TVKCylinderBase.SetBottomRadius(const aValue: Single);
begin
  if aValue <> FBottomRadius then
  begin
    FBottomRadius := aValue;
    StructureChanged;
  end;
end;

// GetTopRadius
//

function TVKCylinderBase.GetTopRadius: Single;
begin
  Result := FBottomRadius;
end;

// SetHeight
//

procedure TVKCylinderBase.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TVKCylinderBase.SetSlices(aValue: GLint);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStack
//

procedure TVKCylinderBase.SetStacks(aValue: GLint);
begin
  if aValue <> FStacks then
  begin
    FStacks := aValue;
    StructureChanged;
  end;
end;

// SetLoops
//

procedure TVKCylinderBase.SetLoops(aValue: GLint);
begin
  if (aValue >= 1) and (aValue <> FLoops) then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKCylinderBase.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKCylinderBase) then
  begin
    FBottomRadius := TVKCylinderBase(Source).FBottomRadius;
    FSlices := TVKCylinderBase(Source).FSlices;
    FStacks := TVKCylinderBase(Source).FStacks;
    FLoops := TVKCylinderBase(Source).FLoops;
    FHeight := TVKCylinderBase(Source).FHeight;
  end;
  inherited Assign(Source);
end;

// GenerateSilhouette
//

function TVKCylinderBase.GenerateSilhouette(const silhouetteParameters
  : TVKSilhouetteParameters): TVKSilhouette;
var
  connectivity: TConnectivity;
  sil: TVKSilhouette;
  ShadowSlices: integer;

  i: integer;
  p: array [0 .. 3] of TVector3f;
  PiDivSlices: Single;
  a1, a2: Single;
  c1, c2: TVector3f;
  cosa1, cosa2, sina1, sina2: Single;
  HalfHeight: Single;
  ShadowTopRadius: Single;
begin
  connectivity := TConnectivity.Create(true);

  ShadowSlices := FSlices div 1;

  if FSlices < 5 then
    FSlices := 5;

  PiDivSlices := 2 * pi / ShadowSlices;

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
    MakeVector(p[2], ShadowTopRadius * sina1, HalfHeight,
      ShadowTopRadius * cosa1);
    MakeVector(p[3], ShadowTopRadius * sina2, HalfHeight,
      ShadowTopRadius * cosa2); // }

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
  connectivity.CreateSilhouette(silhouetteParameters, sil, False);

  Result := sil;

  connectivity.Free;
end;

// ------------------
// ------------------ TVKCone ------------------
// ------------------

// Create
//

constructor TVKCone.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParts := [coSides, coBottom];
end;

// BuildList
//

procedure TVKCone.BuildList(var rci: TVKRenderContextInfo);
var
  quadric: GLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric();
  SetupQuadricParams(quadric);
  glRotated(-90, 1, 0, 0);
  glTranslatef(0, 0, -FHeight * 0.5);
  if coSides in FParts then
    gluCylinder(quadric, BottomRadius, 0, Height, Slices, Stacks);
  if coBottom in FParts then
  begin
    // top of a disk is defined as outside
    SetInvertedQuadricOrientation(quadric);
    gluDisk(quadric, 0, BottomRadius, Slices, FLoops);
  end;
  gluDeleteQuadric(quadric);
  glPopMatrix;
end;

// SetParts
//

procedure TVKCone.SetParts(aValue: TVKConeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKCone.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKCone) then
  begin
    FParts := TVKCone(Source).FParts;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKCone.AxisAlignedDimensionsUnscaled: TVector;
var
  r: GLfloat;
begin
  r := Abs(FBottomRadius);
  Result := VectorMake(r { *Scale.DirectX } , 0.5 * FHeight { *Scale.DirectY } ,
    r { *Scale.DirectZ } );
end;

// GetTopRadius
//

function TVKCone.GetTopRadius: Single;
begin
  Result := 0;
end;

// RayCastIntersect
//

function TVKCone.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  ip, localRayStart, localRayVector: TVector;
  poly: array [0 .. 2] of Double;
  roots: TDoubleArray;
  minRoot: Double;
  d, t, hconst: Single;
begin
  Result := False;
  localRayStart := AbsoluteToLocal(rayStart);
  localRayVector := VectorNormalize(AbsoluteToLocal(rayVector));

  if coBottom in Parts then
  begin
    // bottom can only be raycast from beneath
    if localRayStart.Y < -FHeight * 0.5 then
    begin
      if RayCastPlaneIntersect(localRayStart, localRayVector,
        PointMake(0, -FHeight * 0.5, 0), YHmgVector, @ip) then
      begin
        d := VectorNorm(ip.X, ip.Z);
        if (d <= Sqr(BottomRadius)) then
        begin
          Result := true;
          if Assigned(intersectPoint) then
            SetVector(intersectPoint^, LocalToAbsolute(ip));
          if Assigned(intersectNormal) then
            SetVector(intersectNormal^, VectorNegate(AbsoluteUp));
          Exit;
        end;
      end;
    end;
  end;
  if coSides in Parts then
  begin
    hconst := -Sqr(BottomRadius) / Sqr(Height);
    // intersect against infinite cones (in positive and negative direction)
    poly[0] := Sqr(localRayStart.X) + hconst *
               Sqr(localRayStart.Y - 0.5 * FHeight) +
               Sqr(localRayStart.Z);
    poly[1] := 2 * (localRayStart.X * localRayVector.X + hconst *
                   (localRayStart.Y - 0.5 * FHeight) * localRayVector.Y +
                    localRayStart.Z* localRayVector.Z);
    poly[2] := Sqr(localRayVector.X) + hconst * Sqr(localRayVector.Y) +
               Sqr(localRayVector.Z);
    SetLength(roots, 0);
    roots := SolveQuadric(@poly);
    if MinPositiveCoef(roots, minRoot) then
    begin
      t := minRoot;
      ip := VectorCombine(localRayStart, localRayVector, 1, t);
      // check that intersection with infinite cone is within the range we want
      if (ip.Y > -FHeight * 0.5) and (ip.Y < FHeight * 0.5) then
      begin
        Result := true;
        if Assigned(intersectPoint) then
          intersectPoint^ := LocalToAbsolute(ip);
        if Assigned(intersectNormal) then
        begin
          ip.Y := hconst * (ip.Y - 0.5 * Height);
          ip.W := 0;
          NormalizeVector(ip);
          intersectNormal^ := LocalToAbsolute(ip);
        end;
      end;
    end;
  end;
end;

// ------------------
// ------------------ TVKCylinder ------------------
// ------------------

// Create
//

constructor TVKCylinder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTopRadius := 0.5;
  FParts := [cySides, cyBottom, cyTop];
  FAlignment := caCenter;
end;

// BuildList
//

procedure TVKCylinder.BuildList(var rci: TVKRenderContextInfo);
var
  quadric: GLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams(quadric);
  glRotatef(-90, 1, 0, 0);
  case Alignment of
    caTop:
      glTranslatef(0, 0, -FHeight);
    caBottom:
      ;
  else // caCenter
    glTranslatef(0, 0, -FHeight * 0.5);
  end;
  if cySides in FParts then
    gluCylinder(quadric, FBottomRadius, FTopRadius, FHeight, FSlices, FStacks);
  if cyTop in FParts then
  begin
    glPushMatrix;
    glTranslatef(0, 0, FHeight);
    gluDisk(quadric, 0, FTopRadius, FSlices, FLoops);
    glPopMatrix;
  end;
  if cyBottom in FParts then
  begin
    // swap quadric orientation because top of a disk is defined as outside
    SetInvertedQuadricOrientation(quadric);
    gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
  end;
  gluDeleteQuadric(quadric);
  glPopMatrix;
end;

// SetTopRadius
//

procedure TVKCylinder.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// GetTopRadius
//

function TVKCylinder.GetTopRadius: Single;
begin
  Result := FTopRadius;
end;

// SetParts
//

procedure TVKCylinder.SetParts(aValue: TVKCylinderParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetAlignment
//

procedure TVKCylinder.SetAlignment(val: TVKCylinderAlignment);
begin
  if val <> FAlignment then
  begin
    FAlignment := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKCylinder.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKCylinder) then
  begin
    FParts := TVKCylinder(Source).FParts;
    FTopRadius := TVKCylinder(Source).FTopRadius;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKCylinder.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1: GLfloat;
begin
  r := Abs(FBottomRadius);
  r1 := Abs(FTopRadius);
  if r1 > r then
    r := r1;
  Result := VectorMake(r, 0.5 * FHeight, r);
  // ScaleVector(Result, Scale.AsVector);
end;

// RayCastIntersect
//

function TVKCylinder.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
const
  cOne: Single = 1;
var
  locRayStart, locRayVector, ip: TVector;
  poly: array [0 .. 2] of Double;
  roots: TDoubleArray;
  minRoot: Double;
  t, tr2, invRayVector1, hTop, hBottom: Single;
  tPlaneMin, tPlaneMax: Single;
begin
  Result := False;
  locRayStart := AbsoluteToLocal(rayStart);
  locRayVector := AbsoluteToLocal(rayVector);

  case Alignment of
    caTop:
      begin
        hTop := 0;
        hBottom := -Height;
      end;
    caBottom:
      begin
        hTop := Height;
        hBottom := 0;
      end;
  else
    // caCenter
    hTop := Height * 0.5;
    hBottom := -hTop;
  end;

  if locRayVector.Y = 0 then
  begin
    // intersect if ray shot through the top/bottom planes
    if (locRayStart.X > hTop) or (locRayStart.X < hBottom) then
      Exit;
    tPlaneMin := -1E99;
    tPlaneMax := 1E99;
  end
  else
  begin
    invRayVector1 := cOne / locRayVector.Y;
    tr2 := Sqr(TopRadius);

    // compute intersection with topPlane
    t := (hTop - locRayStart.Y) * invRayVector1;
    if (t > 0) and (cyTop in Parts) then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      if Sqr(ip.X) + Sqr(ip.Z) <= tr2 then
      begin
        // intersect with top plane
        if Assigned(intersectPoint) then
          intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, hTop, ip.Z, 1));
        if Assigned(intersectNormal) then
          intersectNormal^ := LocalToAbsolute(YHmgVector);
        Result := true;
      end;
    end;
    tPlaneMin := t;
    tPlaneMax := t;
    // compute intersection with bottomPlane
    t := (hBottom - locRayStart.Y) * invRayVector1;
    if (t > 0) and (cyBottom in Parts) then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      if (t < tPlaneMin) or (not(cyTop in Parts)) then
      begin
        if Sqr(ip.X) + Sqr(ip.Z) <= tr2 then
        begin
          // intersect with top plane
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, hBottom,
              ip.Z, 1));
          if Assigned(intersectNormal) then
            intersectNormal^ := LocalToAbsolute(VectorNegate(YHmgVector));
          Result := true;
        end;
      end;
    end;
    if t < tPlaneMin then
      tPlaneMin := t;
    if t > tPlaneMax then
      tPlaneMax := t;
  end;
  if cySides in Parts then
  begin
    // intersect against cylinder infinite cylinder
    poly[0] := Sqr(locRayStart.X) + Sqr(locRayStart.Z) - Sqr(TopRadius);
    poly[1] := 2 * (locRayStart.X * locRayVector.X + locRayStart.Z *
      locRayVector.Z);
    poly[2] := Sqr(locRayVector.X) + Sqr(locRayVector.Z);
    roots := SolveQuadric(@poly);
    if MinPositiveCoef(roots, minRoot) then
    begin
      t := minRoot;
      if (t >= tPlaneMin) and (t < tPlaneMax) then
      begin
        if Assigned(intersectPoint) or Assigned(intersectNormal) then
        begin
          ip := VectorCombine(locRayStart, locRayVector, 1, t);
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(ip);
          if Assigned(intersectNormal) then
          begin
            ip.Y := 0;
            ip.W := 0;
            intersectNormal^ := LocalToAbsolute(ip);
          end;
        end;
        Result := true;
      end;
    end;
  end
  else
    SetLength(roots, 0);
end;

// Align
//

procedure TVKCylinder.Align(const startPoint, endPoint: TVector);
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

procedure TVKCylinder.Align(const startObj, endObj: TVKBaseSceneObject);
begin
  Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//

procedure TVKCylinder.Align(const startPoint, endPoint: TAffineVector);
begin
  Align(PointMake(startPoint), PointMake(endPoint));
end;

// ------------------
// ------------------ TVKCapsule ------------------
// ------------------

// Create
//

constructor TVKCapsule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeight := 1;
  FRadius := 0.5;
  FSlices := 4;
  FStacks := 4;
  FParts := [cySides, cyBottom, cyTop];
  FAlignment := caCenter;
end;

// BuildList
//

procedure TVKCapsule.BuildList(var rci: TVKRenderContextInfo);
var
  i, j, n: integer;
  start_nx2: Single;
  start_ny2: Single;
  tmp, nx, ny, nz, start_nx, start_ny, a, ca, sa, l: Single;
  nx2, ny2, nz2: Single;
begin
  glPushMatrix;
  glRotatef(-90, 0, 0, 1);
  case Alignment of
    caTop:
      glTranslatef(0, 0, FHeight + 1);
    caBottom:
      glTranslatef(0, 0, -FHeight);
  else // caCenter
    glTranslatef(0, 0, 0.5);
  end;
  n := FSlices * FStacks;
  l := FHeight;
  l := l * 0.5;
  a := (pi * 2.0) / n;
  sa := sin(a);
  ca := cos(a);
  ny := 0;
  nz := 1;
  if cySides in FParts then
  begin
    glBegin(GL_TRIANGLE_STRIP);
    for i := 0 to n do
    begin
      glNormal3d(ny, nz, 0);
      glTexCoord2f(i / n, 1);
      glVertex3d(ny * FRadius, nz * FRadius, l - 0.5);
      glNormal3d(ny, nz, 0);
      glTexCoord2f(i / n, 0);
      glVertex3d(ny * FRadius, nz * FRadius, -l - 0.5);
      tmp := ca * ny - sa * nz;
      nz := sa * ny + ca * nz;
      ny := tmp;
    end;
    glEnd();
  end;
  //
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
      glPushMatrix;
      glTranslatef(0, 0, -0.5);
      glBegin(GL_TRIANGLE_STRIP);
      for i := 0 to n do
      begin
        glNormal3d(ny2, nz2, nx2);
        glTexCoord2f(i / n, j / n);
        glVertex3d(ny2 * FRadius, nz2 * FRadius, l + nx2 * FRadius);
        glNormal3d(ny, nz, nx);
        glTexCoord2f(i / n, (j - 1) / n);
        glVertex3d(ny * FRadius, nz * FRadius, l + nx * FRadius);
        tmp := ca * ny - sa * nz;
        nz := sa * ny + ca * nz;
        ny := tmp;
        tmp := ca * ny2 - sa * nz2;
        nz2 := sa * ny2 + ca * nz2;
        ny2 := tmp;
      end;
      glEnd();
      glPopMatrix;
      start_nx := start_nx2;
      start_ny := start_ny2;
    end;
  end;
  //
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
      glPushMatrix;
      glTranslatef(0, 0, -0.5);
      glBegin(GL_TRIANGLE_STRIP);
      for i := 0 to n do
      begin
        glNormal3d(ny, nz, nx);
        glTexCoord2f(i / n, (j - 1) / n);
        glVertex3d(ny * FRadius, nz * FRadius, -l + nx * FRadius);
        glNormal3d(ny2, nz2, nx2);
        glTexCoord2f(i / n, j / n);
        glVertex3d(ny2 * FRadius, nz2 * FRadius, -l + nx2 * FRadius);
        tmp := ca * ny - sa * nz;
        nz := sa * ny + ca * nz;
        ny := tmp;
        tmp := ca * ny2 - sa * nz2;
        nz2 := sa * ny2 + ca * nz2;
        ny2 := tmp;
      end;
      glEnd();
      glPopMatrix;
      start_nx := start_nx2;
      start_ny := start_ny2;
    end;
  end;
  glPopMatrix;
end;

// SetLength
//

procedure TVKCapsule.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TVKCapsule.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TVKCapsule.SetSlices(const aValue: integer);
begin
  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TVKCapsule.SetStacks(const aValue: integer);
begin
  if aValue <> FStacks then
  begin
    FStacks := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TVKCapsule.SetParts(aValue: TVKCylinderParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetAlignment
//

procedure TVKCapsule.SetAlignment(val: TVKCylinderAlignment);
begin
  if val <> FAlignment then
  begin
    FAlignment := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKCapsule.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKCapsule) then
  begin
    FParts := TVKCapsule(Source).FParts;
    FRadius := TVKCapsule(Source).FRadius;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKCapsule.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1: GLfloat;
begin
  r := Abs(FRadius);
  r1 := Abs(FRadius);
  if r1 > r then
    r := r1;
  Result := VectorMake(r, 0.5 * FHeight, r);
  // ScaleVector(Result, Scale.AsVector);
end;

// RayCastIntersect
//

function TVKCapsule.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
const
  cOne: Single = 1;
var
  locRayStart, locRayVector, ip: TVector;
  poly: array [0 .. 2] of Double;
  roots: TDoubleArray;
  minRoot: Double;
  t, tr2, invRayVector1, hTop, hBottom: Single;
  tPlaneMin, tPlaneMax: Single;
begin
  Result := False;
  locRayStart := AbsoluteToLocal(rayStart);
  locRayVector := AbsoluteToLocal(rayVector);

  case Alignment of
    caTop:
      begin
        hTop := 0;
        hBottom := -FHeight;
      end;
    caBottom:
      begin
        hTop := FHeight;
        hBottom := 0;
      end;
  else
    // caCenter
    hTop := FHeight * 0.5;
    hBottom := -hTop;
  end;

  if locRayVector.Y = 0 then
  begin
    // intersect if ray shot through the top/bottom planes
    if (locRayStart.X > hTop) or (locRayStart.X < hBottom) then
      Exit;
    tPlaneMin := -1E99;
    tPlaneMax := 1E99;
  end
  else
  begin
    invRayVector1 := cOne / locRayVector.Y;
    tr2 := Sqr(Radius);

    // compute intersection with topPlane
    t := (hTop - locRayStart.Y) * invRayVector1;
    if (t > 0) and (cyTop in Parts) then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      if Sqr(ip.X) + Sqr(ip.Z) <= tr2 then
      begin
        // intersect with top plane
        if Assigned(intersectPoint) then
          intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, hTop, ip.Z, 1));
        if Assigned(intersectNormal) then
          intersectNormal^ := LocalToAbsolute(YHmgVector);
        Result := true;
      end;
    end;
    tPlaneMin := t;
    tPlaneMax := t;
    // compute intersection with bottomPlane
    t := (hBottom - locRayStart.Y) * invRayVector1;
    if (t > 0) and (cyBottom in Parts) then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      if (t < tPlaneMin) or (not(cyTop in Parts)) then
      begin
        if Sqr(ip.X) + Sqr(ip.Z) <= tr2 then
        begin
          // intersect with top plane
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, hBottom,
              ip.Z, 1));
          if Assigned(intersectNormal) then
            intersectNormal^ := LocalToAbsolute(VectorNegate(YHmgVector));
          Result := true;
        end;
      end;
    end;
    if t < tPlaneMin then
      tPlaneMin := t;
    if t > tPlaneMax then
      tPlaneMax := t;
  end;
  if cySides in Parts then
  begin
    // intersect against cylinder infinite cylinder
    poly[0] := Sqr(locRayStart.X) + Sqr(locRayStart.Z) - Sqr(Radius);
    poly[1] := 2 * (locRayStart.X * locRayVector.X +
                    locRayStart.Z * locRayVector.Z);
    poly[2] := Sqr(locRayVector.X) + Sqr(locRayVector.Z);
    roots := SolveQuadric(@poly);
    if MinPositiveCoef(roots, minRoot) then
    begin
      t := minRoot;
      if (t >= tPlaneMin) and (t < tPlaneMax) then
      begin
        if Assigned(intersectPoint) or Assigned(intersectNormal) then
        begin
          ip := VectorCombine(locRayStart, locRayVector, 1, t);
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(ip);
          if Assigned(intersectNormal) then
          begin
            ip.Y := 0;
            ip.W := 0;
            intersectNormal^ := LocalToAbsolute(ip);
          end;
        end;
        Result := true;
      end;
    end;
  end
  else
    SetLength(roots, 0);
end;

// Align
//

procedure TVKCapsule.Align(const startPoint, endPoint: TVector);
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

procedure TVKCapsule.Align(const startObj, endObj: TVKBaseSceneObject);
begin
  Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//

procedure TVKCapsule.Align(const startPoint, endPoint: TAffineVector);
begin
  Align(PointMake(startPoint), PointMake(endPoint));
end;

// ------------------
// ------------------ TVKAnnulus ------------------
// ------------------

// Create
//

constructor TVKAnnulus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBottomInnerRadius := 0.3;
  FTopInnerRadius := 0.3;
  FTopRadius := 0.5;
  FParts := [anInnerSides, anOuterSides, anBottom, anTop];
end;

// SetBottomInnerRadius
//

procedure TVKAnnulus.SetBottomInnerRadius(const aValue: Single);
begin
  if aValue <> FBottomInnerRadius then
  begin
    FBottomInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopRadius
//

procedure TVKAnnulus.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopInnerRadius
//

procedure TVKAnnulus.SetTopInnerRadius(const aValue: Single);
begin
  if aValue <> FTopInnerRadius then
  begin
    FTopInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TVKAnnulus.SetParts(aValue: TVKAnnulusParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TVKAnnulus.BuildList(var rci: TVKRenderContextInfo);
var
  quadric: GLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams(quadric);
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -FHeight * 0.5);
  if anOuterSides in FParts then
    gluCylinder(quadric, FBottomRadius, FTopRadius, FHeight, FSlices, FStacks);
  if anTop in FParts then
  begin
    glPushMatrix;
    glTranslatef(0, 0, FHeight);
    gluDisk(quadric, FTopInnerRadius, FTopRadius, FSlices, FLoops);
    glPopMatrix;
  end;
  if [anBottom, anInnerSides] * FParts <> [] then
  begin
    // swap quadric orientation because top of a disk is defined as outside
    SetInvertedQuadricOrientation(quadric);
    if anBottom in FParts then
      gluDisk(quadric, FBottomInnerRadius, FBottomRadius, FSlices, FLoops);
    if anInnerSides in FParts then
      gluCylinder(quadric, FBottomInnerRadius, FTopInnerRadius, FHeight,
        FSlices, FStacks);
  end;
  gluDeleteQuadric(quadric);
  glPopMatrix;
end;

// Assign
//

procedure TVKAnnulus.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKAnnulus) then
  begin
    FParts := TVKAnnulus(Source).FParts;
    FTopRadius := TVKAnnulus(Source).FTopRadius;
    FTopInnerRadius := TVKAnnulus(Source).FTopInnerRadius;
    FBottomRadius := TVKAnnulus(Source).FBottomRadius;
    FBottomInnerRadius := TVKAnnulus(Source).FBottomInnerRadius;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKAnnulus.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1: GLfloat;
begin
  r := Abs(FBottomRadius);
  r1 := Abs(FTopRadius);
  if r1 > r then
    r := r1;
  Result := VectorMake(r, 0.5 * FHeight, r);
end;

// RayCastIntersect
//

function TVKAnnulus.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
const
  cOne: Single = 1;
var
  locRayStart, locRayVector, ip: TVector;
  poly: array [0 .. 2] of Double;
  t, tr2, invRayVector1: Single;
  tPlaneMin, tPlaneMax: Single;
  tir2, d2: Single;
  Root: Double;
  roots, tmpRoots: TDoubleArray;
  FirstIntersected: Boolean;
  h1, h2, hTop, hBot: Single;
  Draw1, Draw2: Boolean;
begin
  Result := False;
  FirstIntersected := False;
  SetLength(tmpRoots, 0);
  locRayStart := AbsoluteToLocal(rayStart);
  locRayVector := AbsoluteToLocal(rayVector);

  hTop := Height * 0.5;
  hBot := -hTop;
  if locRayVector.Y < 0 then
  begin // Sort the planes according to the direction of view
    h1 := hTop; // Height of the 1st plane
    h2 := hBot; // Height of the 2nd plane
    Draw1 := (anTop in Parts); // 1st "cap" Must be drawn?
    Draw2 := (anBottom in Parts);
  end
  else
  begin
    h1 := hBot;
    h2 := hTop;
    Draw1 := (anBottom in Parts);
    Draw2 := (anTop in Parts);
  end; // if

  if locRayVector.Y = 0 then
  begin
    // intersect if ray shot through the top/bottom planes
    if (locRayStart.X > hTop) or (locRayStart.X < hBot) then
      Exit;
    tPlaneMin := -1E99;
    tPlaneMax := 1E99;
  end
  else
  begin
    invRayVector1 := cOne / locRayVector.Y;
    tr2 := Sqr(TopRadius);
    tir2 := Sqr(TopInnerRadius);
    FirstIntersected := False;

    // compute intersection with first plane
    t := (h1 - locRayStart.Y) * invRayVector1;
    if (t > 0) and Draw1 then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      d2 := Sqr(ip.X) + Sqr(ip.Z);
      if (d2 <= tr2) and (d2 >= tir2) then
      begin
        // intersect with top plane
        FirstIntersected := true;
        if Assigned(intersectPoint) then
          intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, h1, ip.Z, 1));
        if Assigned(intersectNormal) then
          intersectNormal^ := LocalToAbsolute(YHmgVector);
        Result := true;
      end;
    end;
    tPlaneMin := t;
    tPlaneMax := t;

    // compute intersection with second plane
    t := (h2 - locRayStart.Y) * invRayVector1;
    if (t > 0) and Draw2 then
    begin
      ip.X := locRayStart.X + t * locRayVector.X;
      ip.Z := locRayStart.Z + t * locRayVector.Z;
      d2 := Sqr(ip.X) + Sqr(ip.Z);
      if (t < tPlaneMin) or (not FirstIntersected) then
      begin
        if (d2 <= tr2) and (d2 >= tir2) then
        begin
          // intersect with top plane
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(VectorMake(ip.X, h2, ip.Z, 1));
          if Assigned(intersectNormal) then
            intersectNormal^ := LocalToAbsolute(VectorNegate(YHmgVector));
          Result := true;
        end;
      end;
    end;
    if t < tPlaneMin then
    begin
      tPlaneMin := t;
    end; // if
    if t > tPlaneMax then
      tPlaneMax := t;
  end;

  try
    SetLength(roots, 4);
    roots[0] := -1;
    roots[1] := -1;
    roots[2] := -1;
    roots[3] := -1; // By default, side is behind rayStart

    { Compute roots for outer cylinder }
    if anOuterSides in Parts then
    begin
      // intersect against infinite cylinder, will be cut by tPlaneMine and tPlaneMax
      poly[0] := Sqr(locRayStart.X) + Sqr(locRayStart.Z) - Sqr(TopRadius);
      poly[1] := 2 * (locRayStart.X * locRayVector.X + locRayStart.Z *
        locRayVector.Z);
      poly[2] := Sqr(locRayVector.X) + Sqr(locRayVector.Z);
      tmpRoots := SolveQuadric(@poly);
      // Intersect coordinates on rayVector (rayStart=0)
      if ( High(tmpRoots) >= 0) and // Does root exist?
        ((tmpRoots[0] > tPlaneMin) and not FirstIntersected) and
      // In the annulus and not masked by first cap
        ((tmpRoots[0] < tPlaneMax)) { // In the annulus } then
        roots[0] := tmpRoots[0];
      if ( High(tmpRoots) >= 1) and
        ((tmpRoots[1] > tPlaneMin) and not FirstIntersected) and
        ((tmpRoots[1] < tPlaneMax)) then
        roots[1] := tmpRoots[1];
    end; // if

    { Compute roots for inner cylinder }
    if anInnerSides in Parts then
    begin
      // intersect against infinite cylinder
      poly[0] := Sqr(locRayStart.X) +
                 Sqr(locRayStart.Z) - Sqr(TopInnerRadius);
      poly[1] := 2 * (locRayStart.X * locRayVector.X +
                 locRayStart.Z * locRayVector.Z);
      poly[2] := Sqr(locRayVector.X) + Sqr(locRayVector.Z);
                 tmpRoots := SolveQuadric(@poly);
      if ( High(tmpRoots) >= 0) and
        ((tmpRoots[0] > tPlaneMin) and not FirstIntersected) and
        ((tmpRoots[0] < tPlaneMax)) then
        roots[2] := tmpRoots[0];
      if ( High(tmpRoots) >= 1) and
        ((tmpRoots[1] > tPlaneMin) and not FirstIntersected) and
        ((tmpRoots[1] < tPlaneMax)) then
        roots[3] := tmpRoots[1];
    end; // if

    { Find the first intersection point and compute its coordinates and normal }
    if MinPositiveCoef(roots, Root) then
    begin
      t := Root;
      if (t >= tPlaneMin) and (t < tPlaneMax) then
      begin
        if Assigned(intersectPoint) or Assigned(intersectNormal) then
        begin
          ip := VectorCombine(locRayStart, locRayVector, 1, t);
          if Assigned(intersectPoint) then
            intersectPoint^ := LocalToAbsolute(ip);
          if Assigned(intersectNormal) then
          begin
            ip.Y := 0;
            ip.W := 0;
            intersectNormal^ := LocalToAbsolute(ip);
          end;
        end;
        Result := true;
      end;
    end;

  finally
    roots := nil;
    tmpRoots := nil;
  end; // finally
end;

// ------------------
// ------------------ TVKTorus ------------------
// ------------------

// Create
//

constructor TVKTorus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRings := 25;
  FSides := 15;
  FMinorRadius := 0.1;
  FMajorRadius := 0.4;
  FStartAngle := 0.0;
  FStopAngle := 360.0;
  FParts := [toSides, toStartDisk, toStopDisk];
end;

// BuildList
//

procedure TVKTorus.BuildList(var rci: TVKRenderContextInfo);

  procedure EmitVertex(ptr: PVertexRec; L1, L2: integer);
  begin
    glTexCoord2fv(@ptr^.TexCoord);
    begin
      glNormal3fv(@ptr^.Normal);
      if L1 > -1 then
        glVertexAttrib3fv(L1, @ptr.Tangent);
      if L2 > -1 then
        glVertexAttrib3fv(L2, @ptr.Binormal);
      glVertex3fv(@ptr^.Position);
    end;
  end;

var
  i, j: integer;
  Theta, Phi, Theta1, cosPhi, sinPhi, dist: GLfloat;
  cosTheta1, sinTheta1: GLfloat;
  ringDelta, sideDelta: GLfloat;
  ringDir: TAffineVector;
  iFact, jFact: Single;
  pVertex: PVertexRec;
  TanLoc, BinLoc: GLint;
  MeshSize: integer;
  MeshIndex: integer;
  Vertex: TVertexRec;
begin
  if FMesh = nil then
  begin
    MeshSize := 0;
    MeshIndex := 0;
    if toStartDisk in FParts then
      MeshSize := MeshSize + 1;
    if toStopDisk in FParts then
      MeshSize := MeshSize + 1;
    if toSides in FParts then
      MeshSize := MeshSize + Integer(FRings) + 1;
    SetLength(FMesh, MeshSize);
    // handle texture generation
    ringDelta := ((FStopAngle - FStartAngle) / 360) * c2PI / FRings;
    sideDelta := c2PI / FSides;

    iFact := 1 / FRings;
    jFact := 1 / FSides;
    if toSides in FParts then
    begin
      Theta := DegToRadian(FStartAngle) - ringDelta;
      for i := FRings downto 0 do
      begin
        SetLength(FMesh[i], FSides + 1);
        Theta1 := Theta + ringDelta;
        SinCosine(Theta1, sinTheta1, cosTheta1);
        Phi := 0;
        for j := FSides downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCosine(Phi, sinPhi, cosPhi);
          dist := FMajorRadius + FMinorRadius * cosPhi;

          FMesh[i][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, FMinorRadius * sinPhi);
          ringDir := FMesh[i][j].Position;
          ringDir.Z := 0.0;
          NormalizeVector(ringDir);
          FMesh[i][j].Normal := Vector3fMake(cosTheta1 * cosPhi,
            -sinTheta1 * cosPhi, sinPhi);
          FMesh[i][j].Tangent := VectorCrossProduct(ZVector, ringDir);
          FMesh[i][j].Binormal := VectorCrossProduct(FMesh[i][j].Normal,
            FMesh[i][j].Tangent);
          FMesh[i][j].TexCoord := Vector2fMake(i * iFact, j * jFact);
        end;
        Theta := Theta1;
      end;
      MeshIndex := FRings + 1;
    end;

    if toStartDisk in FParts then
    begin
      SetLength(FMesh[MeshIndex], FSides + 1);
      Theta1 := DegToRadian(FStartAngle);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      if toSides in FParts then
      begin
        for j := FSides downto 0 do
        begin
          FMesh[MeshIndex][j].Position := FMesh[MeshIndex - 1][j].Position;
          FMesh[MeshIndex][j].Normal := FMesh[MeshIndex - 1][j].Tangent;
          FMesh[MeshIndex][j].Tangent := FMesh[MeshIndex - 1][j].Position;
          FMesh[MeshIndex][j].Tangent.Z := 0;
          FMesh[MeshIndex][j].Binormal := ZVector;
          FMesh[MeshIndex][j].TexCoord := FMesh[MeshIndex - 1][j].TexCoord;
          FMesh[MeshIndex][j].TexCoord.X := 0;
        end;
      end
      else
      begin
        Phi := 0;
        for j := FSides downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCosine(Phi, sinPhi, cosPhi);
          dist := FMajorRadius + FMinorRadius * cosPhi;
          FMesh[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, FMinorRadius * sinPhi);
          ringDir := FMesh[MeshIndex][j].Position;
          ringDir.Z := 0.0;
          NormalizeVector(ringDir);
          FMesh[MeshIndex][j].Normal := VectorCrossProduct(ZVector, ringDir);
          FMesh[MeshIndex][j].Tangent := ringDir;
          FMesh[MeshIndex][j].Binormal := ZVector;
          FMesh[MeshIndex][j].TexCoord := Vector2fMake(0, j * jFact);
        end;
      end;
      Vertex.Position := Vector3fMake(cosTheta1 * FMajorRadius,
        -sinTheta1 * FMajorRadius, 0);
      Vertex.Normal := FMesh[MeshIndex][0].Normal;
      Vertex.Tangent := FMesh[MeshIndex][0].Tangent;
      Vertex.Binormal := FMesh[MeshIndex][0].Binormal;
      Vertex.TexCoord := Vector2fMake(1, 1);
      MeshIndex := MeshIndex + 1;
    end;

    if toStopDisk in FParts then
    begin
      SetLength(FMesh[MeshIndex], FSides + 1);
      Theta1 := DegToRadian(FStopAngle);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      if toSides in FParts then
      begin
        for j := FSides downto 0 do
        begin
          FMesh[MeshIndex][j].Position := FMesh[0][j].Position;
          FMesh[MeshIndex][j].Normal := VectorNegate(FMesh[0][j].Tangent);
          FMesh[MeshIndex][j].Tangent := FMesh[0][j].Position;
          FMesh[MeshIndex][j].Tangent.Z := 0;
          FMesh[MeshIndex][j].Binormal := VectorNegate(ZVector);
          FMesh[MeshIndex][j].TexCoord := FMesh[0][j].TexCoord;
          FMesh[MeshIndex][j].TexCoord.X := 1;
        end;
      end
      else
      begin
        Phi := 0;
        for j := FSides downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCosine(Phi, sinPhi, cosPhi);
          dist := FMajorRadius + FMinorRadius * cosPhi;
          FMesh[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, FMinorRadius * sinPhi);
          ringDir := FMesh[MeshIndex][j].Position;
          ringDir.Z := 0.0;
          NormalizeVector(ringDir);
          FMesh[MeshIndex][j].Normal := VectorCrossProduct(ringDir, ZVector);
          FMesh[MeshIndex][j].Tangent := ringDir;
          FMesh[MeshIndex][j].Binormal := VectorNegate(ZVector);
          FMesh[MeshIndex][j].TexCoord := Vector2fMake(1, j * jFact);
        end;
      end;
      Vertex.Position := Vector3fMake(cosTheta1 * FMajorRadius,
        -sinTheta1 * FMajorRadius, 0);
      Vertex.Normal := FMesh[MeshIndex][0].Normal;
      Vertex.Tangent := FMesh[MeshIndex][0].Tangent;
      Vertex.Binormal := FMesh[MeshIndex][0].Binormal;
      Vertex.TexCoord := Vector2fMake(0, 0);
    end;
  end;

  begin
    if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
    begin
      TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
        PGLChar(TangentAttributeName));
      BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
        PGLChar(BinormalAttributeName));
    end
    else
    begin
      TanLoc := -1;
      BinLoc := TanLoc;
    end;

    MeshIndex := 0;

    if toSides in FParts then
    begin
      glBegin(GL_TRIANGLES);
      for i := FRings - 1 downto 0 do
        for j := FSides - 1 downto 0 do
        begin
          pVertex := @FMesh[i][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[i][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[i + 1][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[i + 1][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[i + 1][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[i][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
      glEnd;
      MeshIndex := FRings + 1;
    end;

    if toStartDisk in FParts then
    begin
      glBegin(GL_TRIANGLE_FAN);
      pVertex := @Vertex;
      EmitVertex(pVertex, TanLoc, BinLoc);
      for j := 0 to FSides do
      begin
        pVertex := @FMesh[MeshIndex][j];
        EmitVertex(pVertex, TanLoc, BinLoc);
      end;
      glEnd;
      MeshIndex := MeshIndex + 1;
    end;

    if toStopDisk in FParts then
    begin
      glBegin(GL_TRIANGLE_FAN);
      pVertex := @Vertex;
      EmitVertex(pVertex, TanLoc, BinLoc);
      for j := FSides downto 0 do
      begin
        pVertex := @FMesh[MeshIndex][j];
        EmitVertex(pVertex, TanLoc, BinLoc);
      end;
      glEnd;
    end;

  end;
end;

// SetMajorRadius
//

procedure TVKTorus.SetMajorRadius(const aValue: Single);
begin
  if FMajorRadius <> aValue then
  begin
    FMajorRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetMinorRadius
//

procedure TVKTorus.SetMinorRadius(const aValue: Single);
begin
  if FMinorRadius <> aValue then
  begin
    FMinorRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetRings
//

procedure TVKTorus.SetRings(aValue: Cardinal);
begin
  if FRings <> aValue then
  begin
    FRings := aValue;
    if FRings < 2 then
      FRings := 2;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetSides
//

procedure TVKTorus.SetSides(aValue: Cardinal);
begin
  if FSides <> aValue then
  begin
    FSides := aValue;
    if FSides < 3 then
      FSides := 3;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVKTorus.SetStartAngle(const aValue: Single);
begin
  if FStartAngle <> aValue then
  begin
    FStartAngle := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVKTorus.SetStopAngle(const aValue: Single);
begin
  if FStopAngle <> aValue then
  begin
    FStopAngle := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TVKTorus.SetParts(aValue: TVKTorusParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// AxisAlignedDimensionsUnscaled
//

function TVKTorus.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1: GLfloat;
begin
  r := Abs(FMajorRadius);
  r1 := Abs(FMinorRadius);
  Result := VectorMake(r + r1, r + r1, r1); // Danb
end;

// RayCastIntersect
//

function TVKTorus.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i: integer;
  fRo2, fRi2, fDE, fVal, r, nearest: Double;
  polynom: array [0 .. 4] of Double;
  polyRoots: TDoubleArray;
  localStart, localVector: TVector;
  vi, vc: TVector;
begin
  // compute coefficients of quartic polynomial
  fRo2 := Sqr(MajorRadius);
  fRi2 := Sqr(MinorRadius);
  localStart := AbsoluteToLocal(rayStart);
  localVector := AbsoluteToLocal(rayVector);
  NormalizeVector(localVector);
  fDE := VectorDotProduct(localStart, localVector);
  fVal := VectorNorm(localStart) - (fRo2 + fRi2);

  polynom[0] := Sqr(fVal) - 4.0 * fRo2 * (fRi2 - Sqr(localStart.Z));
  polynom[1] := 4.0 * fDE * fVal + 8.0 * fRo2 * localVector.Z * localStart.Z;
  polynom[2] := 2.0 * fVal + 4.0 * Sqr(fDE) + 4.0 * fRo2 * Sqr(localVector.Z);
  polynom[3] := 4.0 * fDE;
  polynom[4] := 1;

  // solve the quartic
  polyRoots := SolveQuartic(@polynom[0]);

  // search for closest point
  Result := (Length(polyRoots) > 0);
  if Result then
  begin
    nearest := 1E20;
    for i := 0 to High(polyRoots) do
    begin
      r := polyRoots[i];
      if (r > 0) and (r < nearest) then
      begin
        nearest := r;
        Result := true;
      end;
    end;
    vi := VectorCombine(localStart, localVector, 1, nearest);
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(vi));
    if Assigned(intersectNormal) then
    begin
      // project vi on local torus plane
      vc.X := vi.X;
      vc.Y := vi.Y;
      vc.Z := 0;
      // project vc on MajorRadius circle
      ScaleVector(vc, MajorRadius / (VectorLength(vc) + 0.000001));
      // calculate circle to intersect vector (gives normal);
      SubtractVector(vi, vc);
      // return to absolute coordinates and normalize
      vi.W := 0;
      SetVector(intersectNormal^, LocalToAbsolute(vi));
    end;
  end;
end;

// ------------------
// ------------------ TVKArrowLine ------------------
// ------------------

// Create
//

constructor TVKArrowLine.Create(AOwner: TComponent);
begin
  inherited;
  FTopRadius := 0.1;
  BottomRadius := 0.1;
  fTopArrowHeadRadius := 0.2;
  fTopArrowHeadHeight := 0.5;
  fBottomArrowHeadRadius := 0.2;
  fBottomArrowHeadHeight := 0.5;
  FHeadStackingStyle := ahssStacked;
  { by default there is not much point having the top of the line (cylinder)
    showing as it is coincidental with the Toparrowhead bottom.
    Note I've defaulted to "vector" type arrows (arrow head on top only }
  FParts := [alLine, alTopArrow];
end;

// SetTopRadius
//

procedure TVKArrowLine.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    StructureChanged;
  end;
end;

// SetTopArrowHeadHeight
//

procedure TVKArrowLine.SetTopArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fTopArrowHeadHeight then
  begin
    fTopArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

// SetTopArrowHeadRadius
//

procedure TVKArrowLine.SetTopArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fTopArrowHeadRadius then
  begin
    fTopArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadHeight
//

procedure TVKArrowLine.SetBottomArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadHeight then
  begin
    fBottomArrowHeadHeight := aValue;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadRadius
//

procedure TVKArrowLine.SetBottomArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadRadius then
  begin
    fBottomArrowHeadRadius := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TVKArrowLine.SetParts(aValue: TVKArrowLineParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetHeadStackingStyle
//

procedure TVKArrowLine.SetHeadStackingStyle(const val: TVKArrowHeadStackingStyle);
begin
  if val <> FHeadStackingStyle then
  begin
    FHeadStackingStyle := val;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TVKArrowLine.BuildList(var rci: TVKRenderContextInfo);
var
  quadric: GLUquadricObj;
  cylHeight, cylOffset, headInfluence: Single;
begin
  case HeadStackingStyle of
    ahssCentered:
      headInfluence := 0.5;
    ahssIncluded:
      headInfluence := 1;
  else // ahssStacked
    headInfluence := 0;
  end;
  cylHeight := Height;
  cylOffset := -FHeight * 0.5;
  // create a new quadric
  quadric := gluNewQuadric();
  SetupQuadricParams(quadric);
  // does the top arrow part - the cone
  if alTopArrow in Parts then
  begin
    cylHeight := cylHeight - TopArrowHeadHeight * headInfluence;
    glPushMatrix;
    glTranslatef(0, 0, Height * 0.5 - TopArrowHeadHeight * headInfluence);
    gluCylinder(quadric, fTopArrowHeadRadius, 0, fTopArrowHeadHeight,
      Slices, Stacks);
    // top of a disk is defined as outside
    SetInvertedQuadricOrientation(quadric);
    if alLine in Parts then
      gluDisk(quadric, FTopRadius, fTopArrowHeadRadius, Slices, FLoops)
    else
      gluDisk(quadric, 0, fTopArrowHeadRadius, Slices, FLoops);
    glPopMatrix;
  end;
  // does the bottom arrow part - another cone
  if alBottomArrow in Parts then
  begin
    cylHeight := cylHeight - BottomArrowHeadHeight * headInfluence;
    cylOffset := cylOffset + BottomArrowHeadHeight * headInfluence;
    glPushMatrix;
    // make the bottom arrow point in the other direction
    glRotatef(180, 1, 0, 0);
    glTranslatef(0, 0, Height * 0.5 - BottomArrowHeadHeight * headInfluence);
    SetNormalQuadricOrientation(quadric);
    gluCylinder(quadric, fBottomArrowHeadRadius, 0, fBottomArrowHeadHeight,
      Slices, Stacks);
    // top of a disk is defined as outside
    SetInvertedQuadricOrientation(quadric);
    if alLine in Parts then
      gluDisk(quadric, FBottomRadius, fBottomArrowHeadRadius, Slices, FLoops)
    else
      gluDisk(quadric, 0, fBottomArrowHeadRadius, Slices, FLoops);
    glPopMatrix;
  end;
  // does the cylinder that makes the line
  if (cylHeight > 0) and (alLine in Parts) then
  begin
    glPushMatrix;
    glTranslatef(0, 0, cylOffset);
    SetNormalQuadricOrientation(quadric);
    gluCylinder(quadric, FBottomRadius, FTopRadius, cylHeight, FSlices,
      FStacks);
    if not(alTopArrow in Parts) then
    begin
      glPushMatrix;
      glTranslatef(0, 0, cylHeight);
      gluDisk(quadric, 0, FTopRadius, FSlices, FLoops);
      glPopMatrix;
    end;
    if not(alBottomArrow in Parts) then
    begin
      // swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
    end;
    glPopMatrix;
  end;
  gluDeleteQuadric(quadric);
end;

// Assign
//

procedure TVKArrowLine.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKArrowLine) then
  begin
    FParts := TVKArrowLine(Source).FParts;
    FTopRadius := TVKArrowLine(Source).FTopRadius;
    fTopArrowHeadHeight := TVKArrowLine(Source).fTopArrowHeadHeight;
    fTopArrowHeadRadius := TVKArrowLine(Source).fTopArrowHeadRadius;
    fBottomArrowHeadHeight := TVKArrowLine(Source).fBottomArrowHeadHeight;
    fBottomArrowHeadRadius := TVKArrowLine(Source).fBottomArrowHeadRadius;
    FHeadStackingStyle := TVKArrowLine(Source).FHeadStackingStyle;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TVKArrowArc ------------------
// ------------------

// Create
//

constructor TVKArrowArc.Create(AOwner: TComponent);
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
end;

// SetArcRadius
//

procedure TVKArrowArc.SetArcRadius(const aValue: Single);
begin
  if fArcRadius <> aValue then
  begin
    fArcRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetStartAngle
//

procedure TVKArrowArc.SetStartAngle(const aValue: Single);
begin
  if FStartAngle <> aValue then
  begin
    FStartAngle := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetStopAngle
//

procedure TVKArrowArc.SetStopAngle(const aValue: Single);
begin
  if FStopAngle <> aValue then
  begin
    FStopAngle := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetTopRadius
//

procedure TVKArrowArc.SetTopRadius(const aValue: Single);
begin
  if aValue <> FTopRadius then
  begin
    FTopRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetTopArrowHeadHeight
//

procedure TVKArrowArc.SetTopArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fTopArrowHeadHeight then
  begin
    fTopArrowHeadHeight := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetTopArrowHeadRadius
//

procedure TVKArrowArc.SetTopArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fTopArrowHeadRadius then
  begin
    fTopArrowHeadRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadHeight
//

procedure TVKArrowArc.SetBottomArrowHeadHeight(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadHeight then
  begin
    fBottomArrowHeadHeight := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetBottomArrowHeadRadius
//

procedure TVKArrowArc.SetBottomArrowHeadRadius(const aValue: Single);
begin
  if aValue <> fBottomArrowHeadRadius then
  begin
    fBottomArrowHeadRadius := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TVKArrowArc.SetParts(aValue: TArrowArcParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

// SetHeadStackingStyle
//

procedure TVKArrowArc.SetHeadStackingStyle(const val: TVKArrowHeadStackingStyle);
begin
  if val <> FHeadStackingStyle then
  begin
    FHeadStackingStyle := val;
    FMesh := nil;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TVKArrowArc.BuildList(var rci: TVKRenderContextInfo);
  procedure EmitVertex(ptr: PVertexRec; L1, L2: integer);
  // {$IFDEF VKS_INLINE}inline;{$ENDIF}
  begin
    glTexCoord2fv(@ptr^.TexCoord);
    begin
      glNormal3fv(@ptr^.Normal);
      if L1 > -1 then
        glVertexAttrib3fv(L1, @ptr.Tangent);
      if L2 > -1 then
        glVertexAttrib3fv(L2, @ptr.Binormal);
      glVertex3fv(@ptr^.Position);
    end;
  end;

var
  i, j: integer;
  Theta, Phi, Theta1, cosPhi, sinPhi, dist: GLfloat;
  cosTheta1, sinTheta1: GLfloat;
  ringDelta, sideDelta: GLfloat;
  ringDir: TAffineVector;
  iFact, jFact: Single;
  pVertex: PVertexRec;
  TanLoc, BinLoc: GLint;
  MeshSize: integer;
  MeshIndex: integer;
  ConeCenter: TVertexRec;
  StartOffset, StopOffset: Single;
begin
  if FMesh = nil then
  begin
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
    SetLength(FMesh, MeshSize);

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
              RadianToDeg(ArcTan(0.5 * fBottomArrowHeadHeight / fArcRadius))
          else
            StartOffset :=0;
          if aaTopArrow in Parts then
            StopOffset :=
              RadianToDeg(ArcTan(0.5 * fTopArrowHeadHeight / fArcRadius))
          else
            StopOffset :=0;
        end ;
      ahssIncluded:
        begin
          if aaBottomArrow in Parts then
            StartOffset := RadianToDeg(ArcTan(fBottomArrowHeadHeight / fArcRadius))
          else
            StartOffset :=0;
          if aaTopArrow in Parts then
            StopOffset := RadianToDeg(ArcTan(fTopArrowHeadHeight / fArcRadius))
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
      Theta := DegToRadian(FStartAngle + StartOffset) - ringDelta;
      for i := FStacks downto 0 do
      begin
        SetLength(FMesh[i], FSlices + 1);
        Theta1 := Theta + ringDelta;
        SinCosine(Theta1, sinTheta1, cosTheta1);
        Phi := 0;
        for j := FSlices downto 0 do
        begin
          Phi := Phi + sideDelta;
          SinCosine(Phi, sinPhi, cosPhi);
          dist := fArcRadius + Lerp(FTopRadius, FBottomRadius, i * iFact) * cosPhi;

          FMesh[i][j].Position := Vector3fMake(cosTheta1 * dist,
            -sinTheta1 * dist, Lerp(FTopRadius, FBottomRadius, i * iFact) * sinPhi);
          ringDir := FMesh[i][j].Position;
          ringDir.Z := 0.0;
          NormalizeVector(ringDir);
          FMesh[i][j].Normal := Vector3fMake(cosTheta1 * cosPhi,
            -sinTheta1 * cosPhi, sinPhi);
          FMesh[i][j].Tangent := VectorCrossProduct(ZVector, ringDir);
          FMesh[i][j].Binormal := VectorCrossProduct(FMesh[i][j].Normal,
            FMesh[i][j].Tangent);
          FMesh[i][j].TexCoord := Vector2fMake(i * iFact, j * jFact);
        end;
        Theta := Theta1;
      end;
      MeshIndex := FStacks + 1;
      begin
        if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
        begin
          TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(TangentAttributeName));
          BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(BinormalAttributeName));
        end
        else
        begin
          TanLoc := -1;
          BinLoc := TanLoc;
        end;

        glBegin(GL_TRIANGLES);
        for i := FStacks - 1 downto 0 do
          for j := FSlices - 1 downto 0 do
          begin
            pVertex := @FMesh[i][j];
            EmitVertex(pVertex, TanLoc, BinLoc);

            pVertex := @FMesh[i][j + 1];
            EmitVertex(pVertex, TanLoc, BinLoc);

            pVertex := @FMesh[i + 1][j];
            EmitVertex(pVertex, TanLoc, BinLoc);

            pVertex := @FMesh[i + 1][j + 1];
            EmitVertex(pVertex, TanLoc, BinLoc);

            pVertex := @FMesh[i + 1][j];
            EmitVertex(pVertex, TanLoc, BinLoc);

            pVertex := @FMesh[i][j + 1];
            EmitVertex(pVertex, TanLoc, BinLoc);
          end;
        glEnd;
      end;
    end;

    // Build Arrow or start cap
    if aaBottomArrow in FParts then
    begin
      SetLength(FMesh[MeshIndex], FSlices + 1);
      SetLength(FMesh[MeshIndex + 1], FSlices + 1);
      SetLength(FMesh[MeshIndex + 2], FSlices + 1);
      Theta1 := DegToRadian(FStartAngle + StartOffset);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
        -sinTheta1 * fArcRadius, 0);

      Phi := 0;
      for j := FSlices downto 0 do
      begin
        Phi := Phi + sideDelta;
        SinCosine(Phi, sinPhi, cosPhi);
        dist := fArcRadius + fBottomArrowHeadRadius * cosPhi;

        // Cap
        FMesh[MeshIndex][J].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, fBottomArrowHeadRadius * sinPhi);
        ringDir := FMesh[MeshIndex][j].Position;
        ringDir.Z := 0.0;
        NormalizeVector(ringDir);
        FMesh[MeshIndex][j].Normal := VectorCrossProduct(ringDir, ZVector);
        FMesh[MeshIndex][j].Tangent := ringDir;
        FMesh[MeshIndex][j].Binormal := ZVector;
        FMesh[MeshIndex][j].TexCoord := Vector2fMake(1, j * jFact);

        // Cone
        FMesh[MeshIndex+1][j].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, fBottomArrowHeadRadius * sinPhi);
        FMesh[MeshIndex+2][j].Position := VectorAdd(ConeCenter.Position,
          Vector3fMake(sinTheta1 * fBottomArrowHeadHeight,
          cosTheta1 * fBottomArrowHeadHeight, 0));

        FMesh[MeshIndex + 1][j].Tangent :=
          VectorNormalize(VectorSubtract(FMesh[MeshIndex + 1][j].Position,
          FMesh[MeshIndex + 2][j].Position));
        FMesh[MeshIndex + 2][j].Tangent := FMesh[MeshIndex + 1][j].Tangent;

        FMesh[MeshIndex + 1][j].Binormal := Vector3fMake(cosTheta1 * -sinPhi,
          sinTheta1 * sinPhi, cosPhi);
        FMesh[MeshIndex + 2][j].Binormal := FMesh[MeshIndex + 1][j].Binormal;

        FMesh[MeshIndex + 1][j].Normal :=
          VectorCrossProduct(FMesh[MeshIndex + 1][j].Binormal,
          FMesh[MeshIndex + 1][j].Tangent);
        FMesh[MeshIndex + 2][j].Normal := FMesh[MeshIndex + 1][j].Normal;

        FMesh[MeshIndex + 1][j].TexCoord := Vector2fMake(0, j * jFact);
        FMesh[MeshIndex + 2][j].TexCoord := Vector2fMake(1, j * jFact);
      end;

      ConeCenter.Normal := FMesh[MeshIndex][0].Normal;
      ConeCenter.Tangent := FMesh[MeshIndex][0].Tangent;
      ConeCenter.Binormal := FMesh[MeshIndex][0].Binormal;
      ConeCenter.TexCoord := Vector2fMake(0, 0);

      begin
        if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
        begin
          TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(TangentAttributeName));
          BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(BinormalAttributeName));
        end
        else
        begin
          TanLoc := -1;
          BinLoc := TanLoc;
        end;

        glBegin(GL_TRIANGLE_FAN);
        pVertex := @ConeCenter;
        EmitVertex(pVertex, TanLoc, BinLoc);
        for j := FSlices downto 0 do
        begin
          pVertex := @FMesh[MeshIndex][j];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;

        glBegin(GL_TRIANGLES);

        for j := FSlices - 1 downto 0 do
        begin
          pVertex := @FMesh[MeshIndex + 1][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 1][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 2][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 2][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 2][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 1][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;

      end;
      MeshIndex := MeshIndex + 3;
    end
    else
    begin
      SetLength(FMesh[MeshIndex], FSlices + 1);
      Theta1 := DegToRadian(FStartAngle);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      Phi := 0;
      for j := FSlices downto 0 do
      begin
        Phi := Phi + sideDelta;
        SinCosine(Phi, sinPhi, cosPhi);
        dist := fArcRadius + fBottomRadius * cosPhi;
        FMesh[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, FBottomRadius * sinPhi);
        ringDir := FMesh[MeshIndex][j].Position;
        ringDir.Z := 0.0;
        NormalizeVector(ringDir);
        FMesh[MeshIndex][j].Normal := VectorCrossProduct(ZVector, ringDir);
        FMesh[MeshIndex][j].Tangent := ringDir;
        FMesh[MeshIndex][j].Binormal := ZVector;
        FMesh[MeshIndex][j].TexCoord := Vector2fMake(0, j * jFact);
      end;

      ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
        -sinTheta1 * fArcRadius, 0);
      ConeCenter.Normal := FMesh[MeshIndex][0].Normal;
      ConeCenter.Tangent := FMesh[MeshIndex][0].Tangent;
      ConeCenter.Binormal := FMesh[MeshIndex][0].Binormal;
      ConeCenter.TexCoord := Vector2fMake(1, 1);
      begin
        if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
        begin
          TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(TangentAttributeName));
          BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(BinormalAttributeName));
        end
        else
        begin
          TanLoc := -1;
          BinLoc := TanLoc;
        end;
        glBegin(GL_TRIANGLE_FAN);
        pVertex := @ConeCenter;
        EmitVertex(pVertex, TanLoc, BinLoc);
        for j := 0 to FSlices do
        begin
          pVertex := @FMesh[MeshIndex][j];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;
      end;
      MeshIndex := MeshIndex + 1;
    end;

    if aaTopArrow in FParts then
    begin
      SetLength(FMesh[MeshIndex], FSlices + 1);
      SetLength(FMesh[MeshIndex + 1], FSlices + 1);
      SetLength(FMesh[MeshIndex + 2], FSlices + 1);
      Theta1 := DegToRadian(FStopAngle - StopOffset);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
        -sinTheta1 * fArcRadius, 0);

      Phi := 0;
      for j := FSlices downto 0 do
      begin
        Phi := Phi + sideDelta;
        SinCosine(Phi, sinPhi, cosPhi);
        dist := fArcRadius + fTopArrowHeadRadius * cosPhi;

        // Cap
        FMesh[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, fTopArrowHeadRadius * sinPhi);
        ringDir := FMesh[MeshIndex][j].Position;
        ringDir.Z := 0.0;
        NormalizeVector(ringDir);
        FMesh[MeshIndex][j].Normal := VectorCrossProduct(ZVector, ringDir);
        FMesh[MeshIndex][j].Tangent := ringDir;
        FMesh[MeshIndex][j].Binormal := ZVector;
        FMesh[MeshIndex][j].TexCoord := Vector2fMake(0, j * jFact);

        // Cone
        FMesh[MeshIndex + 1][j].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, fTopArrowHeadRadius * sinPhi);
        FMesh[MeshIndex + 2][j].Position := VectorSubtract(ConeCenter.Position,
          Vector3fMake(sinTheta1 * fTopArrowHeadHeight,
          cosTheta1 * fTopArrowHeadHeight, 0));

        FMesh[MeshIndex + 1][j].Tangent :=
          VectorNormalize(VectorSubtract(FMesh[MeshIndex + 2][j].Position,
          FMesh[MeshIndex + 1][j].Position));
        FMesh[MeshIndex + 2][j].Tangent := FMesh[MeshIndex + 1][j].Tangent;

        FMesh[MeshIndex + 1][j].Binormal := Vector3fMake(cosTheta1 * -sinPhi,
          sinTheta1 * sinPhi, cosPhi);
        FMesh[MeshIndex + 2][j].Binormal := FMesh[MeshIndex + 1][j].Binormal;

        FMesh[MeshIndex + 1][j].Normal :=
          VectorCrossProduct(FMesh[MeshIndex + 1][j].Binormal,
          FMesh[MeshIndex + 1][j].Tangent);
        FMesh[MeshIndex + 2][j].Normal := FMesh[MeshIndex + 1][j].Normal;

        FMesh[MeshIndex + 1][j].TexCoord := Vector2fMake(1, j * jFact);
        FMesh[MeshIndex + 2][j].TexCoord := Vector2fMake(0, j * jFact);
      end;

      ConeCenter.Normal := FMesh[MeshIndex][0].Normal;
      ConeCenter.Tangent := FMesh[MeshIndex][0].Tangent;
      ConeCenter.Binormal := FMesh[MeshIndex][0].Binormal;
      ConeCenter.TexCoord := Vector2fMake(1, 1);

      begin
        if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
        begin
          TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(TangentAttributeName));
          BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(BinormalAttributeName));
        end
        else
        begin
          TanLoc := -1;
          BinLoc := TanLoc;
        end;

        glBegin(GL_TRIANGLE_FAN);
        pVertex := @ConeCenter;
        EmitVertex(pVertex, TanLoc, BinLoc);
        for j := 0 to FSlices do
        begin
          pVertex := @FMesh[MeshIndex][j];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;

        glBegin(GL_TRIANGLES);

        for j := FSlices - 1 downto 0 do
        begin
          pVertex := @FMesh[MeshIndex + 2][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 2][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 1][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 1][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 1][j];
          EmitVertex(pVertex, TanLoc, BinLoc);

          pVertex := @FMesh[MeshIndex + 2][j + 1];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;

      end;
    end
    else
    begin
      SetLength(FMesh[MeshIndex], FSlices + 1);
      Theta1 := DegToRadian(FStopAngle);
      SinCosine(Theta1, sinTheta1, cosTheta1);

      Phi := 0;
      for j := FSlices downto 0 do
      begin
        Phi := Phi + sideDelta;
        SinCosine(Phi, sinPhi, cosPhi);
        dist := fArcRadius + fTopRadius * cosPhi;
        FMesh[MeshIndex][j].Position := Vector3fMake(cosTheta1 * dist,
          -sinTheta1 * dist, fTopRadius * sinPhi);
        ringDir := FMesh[MeshIndex][j].Position;
        ringDir.Z := 0.0;
        NormalizeVector(ringDir);
        FMesh[MeshIndex][j].Normal := VectorCrossProduct(ringDir, ZVector);
        FMesh[MeshIndex][j].Tangent := ringDir;
        FMesh[MeshIndex][j].Binormal := VectorNegate(ZVector);
        FMesh[MeshIndex][j].TexCoord := Vector2fMake(1, j * jFact);
      end;
      ConeCenter.Position := Vector3fMake(cosTheta1 * fArcRadius,
        -sinTheta1 * fArcRadius, 0);
      ConeCenter.Normal := FMesh[MeshIndex][0].Normal;
      ConeCenter.Tangent := FMesh[MeshIndex][0].Tangent;
      ConeCenter.Binormal := FMesh[MeshIndex][0].Binormal;
      ConeCenter.TexCoord := Vector2fMake(0, 0);
      begin
        if {GL_ARB_shader_objects and} (rci.VKStates.CurrentProgram > 0) then
        begin
          TanLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(TangentAttributeName));
          BinLoc := glGetAttribLocation(rci.VKStates.CurrentProgram,
            PGLChar(BinormalAttributeName));
        end
        else
        begin
          TanLoc := -1;
          BinLoc := TanLoc;
        end;
        glBegin(GL_TRIANGLE_FAN);
        pVertex := @ConeCenter;
        EmitVertex(pVertex, TanLoc, BinLoc);
        for j := FSlices downto 0 do
        begin
          pVertex := @FMesh[MeshIndex][j];
          EmitVertex(pVertex, TanLoc, BinLoc);
        end;
        glEnd;
      end;
    end;
  end;
end;

// Assign
//

procedure TVKArrowArc.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKArrowLine) then
  begin
    FStartAngle := TVKArrowArc(Source).FStartAngle;
    FStopAngle := TVKArrowArc(Source).FStopAngle;
    fArcRadius := TVKArrowArc(Source).fArcRadius;
    FParts := TVKArrowArc(Source).FParts;
    FTopRadius := TVKArrowArc(Source).FTopRadius;
    fTopArrowHeadHeight := TVKArrowArc(Source).fTopArrowHeadHeight;
    fTopArrowHeadRadius := TVKArrowArc(Source).fTopArrowHeadRadius;
    fBottomArrowHeadHeight := TVKArrowArc(Source).fBottomArrowHeadHeight;
    fBottomArrowHeadRadius := TVKArrowArc(Source).fBottomArrowHeadRadius;
    FHeadStackingStyle := TVKArrowArc(Source).FHeadStackingStyle;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TVKFrustrum ------------------
// ------------------

constructor TVKFrustrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApexHeight := 1;
  FBaseWidth := 1;
  FBaseDepth := 1;
  FHeight := 0.5;
  FParts := cAllFrustrumParts;
  FNormalDirection := ndOutside;
end;

procedure TVKFrustrum.BuildList(var rci: TVKRenderContextInfo);
var
  HBW, HBD: GLfloat; // half of width, half of depth at base
  HTW, HTD: GLfloat; // half of width, half of depth at top of frustrum
  HFH: GLfloat; // half of height, for align to center
  Sign: GLfloat; // +1 or -1
  angle: GLfloat; // in radians
  ASin, ACos: GLfloat;
begin
  if FNormalDirection = ndInside then
    Sign := -1
  else
    Sign := 1;
  HBW := FBaseWidth * 0.5;
  HBD := FBaseDepth * 0.5;
  HTW := HBW * (FApexHeight - FHeight) / FApexHeight;
  HTD := HBD * (FApexHeight - FHeight) / FApexHeight;
  HFH := FHeight * 0.5;

  glBegin(GL_QUADS);

  if [fpFront, fpBack] * FParts <> [] then
  begin
    angle := ArcTan(FApexHeight / HBD);
    // angle of front plane with bottom plane
    SinCosine(angle, ASin, ACos);
    if fpFront in FParts then
    begin
      glNormal3f(0, Sign * ACos, Sign * ASin);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(HTW, HFH, HTD);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-HTW, HFH, HTD);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-HBW, -HFH, HBD);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(HBW, -HFH, HBD);
    end;
    if fpBack in FParts then
    begin
      glNormal3f(0, Sign * ACos, -Sign * ASin);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(HTW, HFH, -HTD);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(HBW, -HFH, -HBD);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(-HBW, -HFH, -HBD);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(-HTW, HFH, -HTD);
    end;
  end;

  if [fpLeft, fpRight] * FParts <> [] then
  begin
    angle := ArcTan(FApexHeight / HBW); // angle of side plane with bottom plane
    SinCosine(angle, ASin, ACos);
    if fpLeft in FParts then
    begin
      glNormal3f(-Sign * ASin, Sign * ACos, 0);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(-HTW, HFH, HTD);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(-HTW, HFH, -HTD);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(-HBW, -HFH, -HBD);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(-HBW, -HFH, HBD);
    end;
    if fpRight in FParts then
    begin
      glNormal3f(Sign * ASin, Sign * ACos, 0);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(HTW, HFH, HTD);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(HBW, -HFH, HBD);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(HBW, -HFH, -HBD);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(HTW, HFH, -HTD);
    end;
  end;

  if (fpTop in FParts) and (FHeight < FApexHeight) then
  begin
    glNormal3f(0, Sign, 0);
    glTexCoord2fv(@YTexPoint);
    glVertex3f(-HTW, HFH, -HTD);
    glTexCoord2fv(@NullTexPoint);
    glVertex3f(-HTW, HFH, HTD);
    glTexCoord2fv(@XTexPoint);
    glVertex3f(HTW, HFH, HTD);
    glTexCoord2fv(@XYTexPoint);
    glVertex3f(HTW, HFH, -HTD);
  end;
  if fpBottom in FParts then
  begin
    glNormal3f(0, -Sign, 0);
    glTexCoord2fv(@NullTexPoint);
    glVertex3f(-HBW, -HFH, -HBD);
    glTexCoord2fv(@XTexPoint);
    glVertex3f(HBW, -HFH, -HBD);
    glTexCoord2fv(@XYTexPoint);
    glVertex3f(HBW, -HFH, HBD);
    glTexCoord2fv(@YTexPoint);
    glVertex3f(-HBW, -HFH, HBD);
  end;

  glEnd;
end;

procedure TVKFrustrum.SetApexHeight(const aValue: Single);
begin
  if (aValue <> FApexHeight) and (aValue >= 0) then
  begin
    FApexHeight := aValue;
    if FHeight > aValue then
      FHeight := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.SetBaseDepth(const aValue: Single);
begin
  if (aValue <> FBaseDepth) and (aValue >= 0) then
  begin
    FBaseDepth := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.SetBaseWidth(const aValue: Single);
begin
  if (aValue <> FBaseWidth) and (aValue >= 0) then
  begin
    FBaseWidth := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.SetHeight(const aValue: Single);
begin
  if (aValue <> FHeight) and (aValue >= 0) then
  begin
    FHeight := aValue;
    if FApexHeight < aValue then
      FApexHeight := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.SetParts(aValue: TFrustrumParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TVKFrustrum.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKFrustrum) then
  begin
    FApexHeight := TVKFrustrum(Source).FApexHeight;
    FBaseDepth := TVKFrustrum(Source).FBaseDepth;
    FBaseWidth := TVKFrustrum(Source).FBaseWidth;
    FHeight := TVKFrustrum(Source).FHeight;
    FParts := TVKFrustrum(Source).FParts;
    FNormalDirection := TVKFrustrum(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TVKFrustrum.TopDepth: GLfloat;
begin
  Result := FBaseDepth * (FApexHeight - FHeight) / FApexHeight;
end;

function TVKFrustrum.TopWidth: GLfloat;
begin
  Result := FBaseWidth * (FApexHeight - FHeight) / FApexHeight;
end;

procedure TVKFrustrum.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('FrustrumSize', ReadData, WriteData,
    (FApexHeight <> 1) or (FBaseDepth <> 1) or (FBaseWidth <> 1) or
    (FHeight <> 0.5));
end;

procedure TVKFrustrum.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FApexHeight, SizeOf(FApexHeight));
    Read(FBaseDepth, SizeOf(FBaseDepth));
    Read(FBaseWidth, SizeOf(FBaseWidth));
    Read(FHeight, SizeOf(FHeight));
  end;
end;

procedure TVKFrustrum.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FApexHeight, SizeOf(FApexHeight));
    Write(FBaseDepth, SizeOf(FBaseDepth));
    Write(FBaseWidth, SizeOf(FBaseWidth));
    Write(FHeight, SizeOf(FHeight));
  end;
end;

function TVKFrustrum.AxisAlignedBoundingBoxUnscaled: TAABB;
var
  aabb: TAABB;
  child: TVKBaseSceneObject;
  i: integer;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  OffsetAABB(Result, VectorMake(0, FHeight * 0.5, 0));

  // not tested for child objects
  for i := 0 to Count - 1 do
  begin
    child := TVKBaseSceneObject(Children[i]);
    aabb := child.AxisAlignedBoundingBoxUnscaled;
    AABBTransform(aabb, child.Matrix);
    AddAABB(Result, aabb);
  end;
end;

function TVKFrustrum.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := FBaseWidth * 0.5;
  Result.Y := FHeight * 0.5;
  Result.Z := FBaseDepth * 0.5;
  Result.W := 0;
end;

// ------------------
// ------------------ TVKPolygon ------------------
// ------------------

// Create
//

constructor TVKPolygon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParts := [ppTop, ppBottom];
end;

// Destroy
//

destructor TVKPolygon.Destroy;
begin
  inherited Destroy;
end;

// SetParts
//

procedure TVKPolygon.SetParts(const val: TPolygonParts);
begin
  if FParts <> val then
  begin
    FParts := val;
    StructureChanged;
  end;
end;

// Assign
//

procedure TVKPolygon.Assign(Source: TPersistent);
begin
  if Source is TVKPolygon then
  begin
    FParts := TVKPolygon(Source).FParts;
  end;
  inherited Assign(Source);
end;

// BuildList
//

procedure TVKPolygon.BuildList(var rci: TVKRenderContextInfo);
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

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

RegisterClasses([TVKCylinder, TVKCone, TVKTorus, TVKDisk, TVKArrowLine,
  TVKAnnulus, TVKFrustrum, TVKPolygon, TVKCapsule, TVKArrowArc]);

end.
