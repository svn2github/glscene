//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Graph plotting objects for GLScene 
}
unit VXS.Graph;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  VXS.XOpenGL,
  VXS.Scene,
  VXS.Context,
  VXS.VectorGeometry,
  VXS.Material,
  VXS.Objects,
  VXS.VectorLists,
  VXS.Color,
  VXS.BaseClasses,
  VXS.RenderContextInfo,
  VXS.State,
  VXS.VectorTypes;

type

  TVXSamplingScale = class(TVXUpdateAbleObject)
  private
    FMin: Single;
    FMax: Single;
    FOrigin: Single;
    FStep: Single;
  protected
    procedure SetMin(const val: Single);
    procedure SetMax(const val: Single);
    procedure SetOrigin(const val: Single);
    procedure SetStep(const val: Single);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Returns the Base value for Step browsing.
      ie. the lowest value (superior to Min) that verifies
      Frac((Origin-StepBase)/Step)=0.0, this value may be superior to Max. }
    function StepBase: Single;
    { Maximum number of steps that can occur between Min and Max. }
    function MaxStepCount: Integer;
    function IsValid: Boolean;
    procedure SetBaseStepMaxToVars(var Base, Step, Max: Single;
      SamplingEnabled: Boolean = True);
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Origin: Single read FOrigin write SetOrigin;
    property Step: Single read FStep write SetStep;
  end;

  TVXHeightFieldGetHeightEvent = procedure(const x, y: Single; var z: Single;
    var Color: TColorVector; var TexPoint: TTexPoint) of object;
  TVXHeightFieldGetHeight2Event = procedure(Sender: TObject; const x, y: Single;
    var z: Single; var Color: TColorVector; var TexPoint: TTexPoint) of object;

  TVXHeightFieldOption = (hfoTextureCoordinates, hfoTwoSided);
  TVXHeightFieldOptions = set of TVXHeightFieldOption;

  TVXHeightFieldColorMode = (hfcmNone, hfcmEmission, hfcmAmbient, hfcmDiffuse,
    hfcmAmbientAndDiffuse);

  { Renders a sampled height-field.
    HeightFields are used to materialize z=f(x, y) surfaces, you can use it to
    render anything from math formulas to statistics. Most important properties
    of an height field are its sampling scales (X & Y) that determine the extents
    and the resolution of the base grid.
    The component will then invoke it OnGetHeight event to retrieve Z values for
    all of the grid points (values are retrieved only once for each point). Each
    point may have an additionnal color and texture coordinate. }
  TVXHeightField = class(TVXSceneObject)
  private
    FOnGetHeight: TVXHeightFieldGetHeightEvent;
    FOnGetHeight2: TVXHeightFieldGetHeight2Event;
    FXSamplingScale: TVXSamplingScale;
    FYSamplingScale: TVXSamplingScale;
    FOptions: TVXHeightFieldOptions;
    FTriangleCount: Integer;
    FColorMode: TVXHeightFieldColorMode;
  protected
    procedure SetXSamplingScale(const val: TVXSamplingScale);
    procedure SetYSamplingScale(const val: TVXSamplingScale);
    procedure SetOptions(const val: TVXHeightFieldOptions);
    procedure SetOnGetHeight(const val: TVXHeightFieldGetHeightEvent);
    procedure SetOnGetHeight2(const val: TVXHeightFieldGetHeight2Event);
    procedure SetColorMode(const val: TVXHeightFieldColorMode);
    procedure DefaultHeightField(const x, y: Single; var z: Single;
      var Color: TColorVector; var TexPoint: TTexPoint);
    procedure Height2Field(const x, y: Single; var z: Single;
      var Color: TColorVector; var texPoint: TTexPoint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure NotifyChange(Sender: TObject); override;
    property TriangleCount: Integer read FTriangleCount;
  published
    property XSamplingScale: TVXSamplingScale read FXSamplingScale
      write SetXSamplingScale;
    property YSamplingScale: TVXSamplingScale read FYSamplingScale
      write SetYSamplingScale;
    { Define if and how per vertex color is used. }
    property ColorMode: TVXHeightFieldColorMode read FColorMode write SetColorMode
      default hfcmNone;
    property Options: TVXHeightFieldOptions read FOptions write SetOptions
      default [hfoTwoSided];
    { Primary event to return heights. }
    property OnGetHeight: TVXHeightFieldGetHeightEvent read FOnGetHeight
      write SetOnGetHeight;
    { Alternate this event to return heights.
      This events passes an extra "Sender" parameter, it will be invoked
      only if OnGetHeight isn't defined. }
    property OnGetHeight2: TVXHeightFieldGetHeight2Event read FOnGetHeight2
      write SetOnGetHeight2;
  end;

  TXYZGridPart = (gpX, gpY, gpZ);
  TXYZGridParts = set of TXYZGridPart;

  { Rendering Style for grid lines.
    - glsLine : a single line is used for each grid line (from Min to Max),
    this provides the fastest rendering
    - glsSegments : line segments are used between each node of the grid,
    this enhances perspective and quality, at the expense of computing
    power. }
  TXYZGridLinesStyle = (strLine, glsSegments);

  { An XYZ Grid object.
    Renders an XYZ grid using lines. }
  TVXXYZGrid = class(TVXLineBase)
  private
    FXSamplingScale: TVXSamplingScale;
    FYSamplingScale: TVXSamplingScale;
    FZSamplingScale: TVXSamplingScale;
    FParts: TXYZGridParts;
    FLinesStyle: TXYZGridLinesStyle;
  protected
    procedure SetXSamplingScale(const val: TVXSamplingScale);
    procedure SetYSamplingScale(const val: TVXSamplingScale);
    procedure SetZSamplingScale(const val: TVXSamplingScale);
    procedure SetParts(const val: TXYZGridParts);
    procedure SetLinesStyle(const val: TXYZGridLinesStyle);
    procedure SetLinesSmoothing(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    property XSamplingScale: TVXSamplingScale read FXSamplingScale
      write SetXSamplingScale;
    property YSamplingScale: TVXSamplingScale read FYSamplingScale
      write SetYSamplingScale;
    property ZSamplingScale: TVXSamplingScale read FZSamplingScale
      write SetZSamplingScale;
    property Parts: TXYZGridParts read FParts write SetParts default [gpX, gpY];
    property LinesStyle: TXYZGridLinesStyle read FLinesStyle write SetLinesStyle
      default glsSegments;
    { Adjusts lines smoothing (or antialiasing).
      Obsolete, now maps to Antialiased property. }
    property LinesSmoothing: Boolean write SetLinesSmoothing stored False;
  end;

//=====================================================================
implementation
//=====================================================================

// ------------------
// ------------------ TVXSamplingScale ------------------
// ------------------

constructor TVXSamplingScale.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FStep := 0.1;
end;

destructor TVXSamplingScale.Destroy;
begin
  inherited Destroy;
end;

procedure TVXSamplingScale.Assign(Source: TPersistent);
begin
  if Source is TVXSamplingScale then
  begin
    FMin := TVXSamplingScale(Source).FMin;
    FMax := TVXSamplingScale(Source).FMax;
    FOrigin := TVXSamplingScale(Source).FOrigin;
    FStep := TVXSamplingScale(Source).FStep;
    NotifyChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TVXSamplingScale.SetMin(const val: Single);
begin
  FMin := val;
  if FMax < FMin then
    FMax := FMin;
  NotifyChange(Self);
end;

procedure TVXSamplingScale.SetMax(const val: Single);
begin
  FMax := val;
  if FMin > FMax then
    FMin := FMax;
  NotifyChange(Self);
end;

procedure TVXSamplingScale.SetOrigin(const val: Single);
begin
  FOrigin := val;
  NotifyChange(Self);
end;

procedure TVXSamplingScale.SetStep(const val: Single);
begin
  if val > 0 then
    FStep := val
  else
    FStep := 1;
  NotifyChange(Self);
end;

function TVXSamplingScale.StepBase: Single;
begin
  if FOrigin <> FMin then
  begin
    Result := (FOrigin - FMin) / FStep;
    if Result >= 0 then
      Result := Trunc(Result)
    else
      Result := Trunc(Result) - 1;
    Result := FOrigin - FStep * Result;
  end
  else
    Result := FMin;
end;

function TVXSamplingScale.MaxStepCount: Integer;
begin
  Result := Round(0.5 + (Max - Min) / Step);
end;

function TVXSamplingScale.IsValid: Boolean;
begin
  Result := (Max <> Min);
end;

// SetBaseStepMaxToVars
//

procedure TVXSamplingScale.SetBaseStepMaxToVars(var Base, Step, Max: Single;
  samplingEnabled: Boolean = True);
begin
  Step := FStep;
  if samplingEnabled then
  begin
    Base := StepBase;
    Max := FMax + ((FMax - Base) / Step) * 1E-6; // add precision loss epsilon
  end
  else
  begin
    Base := FOrigin;
    Max := Base;
  end;
end;

// ------------------
// ------------------ TVXHeightField ------------------
// ------------------

constructor TVXHeightField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXSamplingScale := TVXSamplingScale.Create(Self);
  FYSamplingScale := TVXSamplingScale.Create(Self);
  FOptions := [hfoTwoSided];
end;

// Destroy
//

destructor TVXHeightField.Destroy;
begin
  FXSamplingScale.Free;
  FYSamplingScale.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXHeightField.Assign(Source: TPersistent);
begin
  if Source is TVXHeightField then
  begin
    XSamplingScale := TVXHeightField(Source).XSamplingScale;
    YSamplingScale := TVXHeightField(Source).YSamplingScale;
    FOnGetHeight := TVXHeightField(Source).FOnGetHeight;
    FOptions := TVXHeightField(Source).FOptions;
    FColorMode := TVXHeightField(Source).FColorMode;
  end;
  inherited Assign(Source);
end;

// NotifyChange
//

procedure TVXHeightField.NotifyChange(Sender: TObject);
begin
  if Sender is TVXSamplingScale then
    StructureChanged;
  inherited NotifyChange(Sender);
end;

// BuildList
//

procedure TVXHeightField.BuildList(var rci: TVXRenderContextInfo);
type
  TRowData = packed record
    Color: TColorVector;
    Z: Single;
    TexPoint: TTexPoint;
    Normal: TAffineVector;
  end;

  TRowDataArray = array [0 .. Maxint shr 6] of TRowData;
  PRowData = ^TRowDataArray;
const
  cHFCMtoEnum: array [hfcmEmission .. hfcmAmbientAndDiffuse] of GLEnum =
    (GL_EMISSION, GL_AMBIENT, GL_DIFFUSE, GL_AMBIENT_AND_DIFFUSE);

var
  nx, m, k: Integer;
  x, y, x1, y1, y2, xStep, yStep, xBase, dx, dy: Single;
  invXStep, invYStep: Single;
  row: packed array [0 .. 2] of PRowData;
  rowTop, rowMid, rowBottom: PRowData;
  func: TVXHeightFieldGetHeightEvent;

  procedure IssuePoint(var x, y: Single; const pt: TRowData);
  begin
    with pt do
    begin
      glNormal3fv(@normal);
      if ColorMode <> hfcmNone then
        glColor4fv(@color);
      if hfoTextureCoordinates in Options then
        glTexCoord2fv(@texPoint);
      glVertex4f(x, y, z, 1);
    end;
  end;

  procedure RenderRow(pHighRow, pLowRow: PRowData);
  var
    k: Integer;
  begin
    glBegin(GL_TRIANGLE_STRIP);
    x := xBase;
    IssuePoint(x, y1, pLowRow^[0]);
    for k := 0 to m - 2 do
    begin
      x1 := x + xStep;
      IssuePoint(x, y2, pHighRow^[k]);
      IssuePoint(x1, y1, pLowRow^[k + 1]);
      x := x1;
    end;
    IssuePoint(x, y2, pHighRow^[m - 1]);
    glEnd;
  end;

begin
  if not(XSamplingScale.IsValid and YSamplingScale.IsValid) then
    Exit;
  if Assigned(FOnGetHeight) and (not(csDesigning in ComponentState)) then
    func := FOnGetHeight
  else if Assigned(FOnGetHeight2) and (not(csDesigning in ComponentState)) then
    func := Height2Field
  else
    func := DefaultHeightField;
  // allocate row cache
  nx := (XSamplingScale.MaxStepCount + 1) * SizeOf(TRowData);
  for k := 0 to 2 do
  begin
    GetMem(row[k], nx);
    FillChar(row[k][0], nx, 0);
  end;
  try
    // precompute grid values
    xBase := XSamplingScale.StepBase;
    xStep := XSamplingScale.Step;
    invXStep := 1 / xStep;
    yStep := YSamplingScale.Step;
    invYStep := 1 / yStep;
    // get through the grid
    if (hfoTwoSided in Options) or (ColorMode <> hfcmNone) then
    begin
      // if we're not two-sided, we doesn't have to enable face-culling, it's
      // controled at the sceneviewer level
      if hfoTwoSided in Options then
      begin
        rci.VXStates.Disable(stCullFace);
        rci.VXStates.PolygonMode := Material.PolygonMode;
      end;
      if ColorMode <> hfcmNone then
      begin
        rci.VXStates.Enable(stColorMaterial);
        glColorMaterial(GL_FRONT_AND_BACK, cHFCMtoEnum[ColorMode]);
        rci.VXStates.SetMaterialColors(cmFront, clrBlack, clrGray20,
          clrGray80, clrBlack, 0);
        rci.VXStates.SetMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
          clrBlack, 0);
      end;
    end;
    rowBottom := nil;
    rowMid := nil;
    nx := 0;
    y := YSamplingScale.StepBase;
    y1 := y;
    y2 := y;
    while y <= YSamplingScale.Max do
    begin
      rowTop := rowMid;
      rowMid := rowBottom;
      rowBottom := row[nx mod 3];
      x := xBase;
      m := 0;
      while x <= XSamplingScale.Max do
      begin
        with rowBottom^[m] do
        begin
          with texPoint do
          begin
            S := x;
            T := y;
          end;
          func(x, y, z, color, texPoint);
        end;
        Inc(m);
        x := x + xStep;
      end;
      if Assigned(rowMid) then
      begin
        for k := 0 to m - 1 do
        begin
          if k > 0 then
            dx := (rowMid^[k - 1].z - rowMid^[k].z) * invXStep
          else
            dx := 0;
          if k < m - 1 then
            dx := dx + (rowMid^[k].z - rowMid^[k + 1].z) * invXStep;
          if Assigned(rowTop) then
            dy := (rowTop^[k].z - rowMid^[k].z) * invYStep
          else
            dy := 0;
          if Assigned(rowBottom) then
            dy := dy + (rowMid^[k].z - rowBottom^[k].z) * invYStep;
          rowMid^[k].normal := VectorNormalize(AffineVectorMake(dx, dy, 1));
        end;
      end;
      if nx > 1 then
      begin
        RenderRow(rowTop, rowMid);
      end;
      Inc(nx);
      y2 := y1;
      y1 := y;
      y := y + yStep;
    end;
    for k := 0 to m - 1 do
    begin
      if k > 0 then
        dx := (rowBottom^[k - 1].z - rowBottom^[k].z) * invXStep
      else
        dx := 0;
      if k < m - 1 then
        dx := dx + (rowBottom^[k].z - rowBottom^[k + 1].z) * invXStep;
      if Assigned(rowMid) then
        dy := (rowMid^[k].z - rowBottom^[k].z) * invYStep
      else
        dy := 0;
      rowBottom^[k].normal := VectorNormalize(AffineVectorMake(dx, dy, 1));
    end;
    if Assigned(rowMid) and Assigned(rowBottom) then
      RenderRow(rowMid, rowBottom);
    FTriangleCount := 2 * (nx - 1) * (m - 1);
  finally
    FreeMem(row[0]);
    FreeMem(row[1]);
    FreeMem(row[2]);
  end;
end;

// SetXSamplingScale
//

procedure TVXHeightField.SetXSamplingScale(const val: TVXSamplingScale);
begin
  FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//

procedure TVXHeightField.SetYSamplingScale(const val: TVXSamplingScale);
begin
  FYSamplingScale.Assign(val);
end;

// SetOptions
//

procedure TVXHeightField.SetOptions(const val: TVXHeightFieldOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// SetOnGetHeight
//

procedure TVXHeightField.SetOnGetHeight(const val: TVXHeightFieldGetHeightEvent);
begin
  FOnGetHeight := val;
  StructureChanged;
end;

// SetOnGetHeight2
//

procedure TVXHeightField.SetOnGetHeight2(const val
  : TVXHeightFieldGetHeight2Event);
begin
  FOnGetHeight2 := val;
  StructureChanged;
end;

// SetColorMode
//

procedure TVXHeightField.SetColorMode(const val: TVXHeightFieldColorMode);
begin
  if val <> FColorMode then
  begin
    FColorMode := val;
    StructureChanged;
  end;
end;

// DefaultHeightField
//

procedure TVXHeightField.DefaultHeightField(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
begin
  z := VectorNorm(x, y);
  z := cos(z * 12) / (2 * (z * 6.28 + 1));
  color := clrGray80;
end;

// Height2Field
//

procedure TVXHeightField.Height2Field(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
begin
  FOnGetHeight2(Self, x, y, z, color, texPoint);
end;

// ------------------
// ------------------ TVXXYZGrid ------------------
// ------------------

// Create
//

constructor TVXXYZGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXSamplingScale := TVXSamplingScale.Create(Self);
  FYSamplingScale := TVXSamplingScale.Create(Self);
  FZSamplingScale := TVXSamplingScale.Create(Self);
  FParts := [gpX, gpY];
  FLinesStyle := glsSegments;
end;

// Destroy
//

destructor TVXXYZGrid.Destroy;
begin
  FXSamplingScale.Free;
  FYSamplingScale.Free;
  FZSamplingScale.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXXYZGrid.Assign(Source: TPersistent);
begin
  if Source is TVXXYZGrid then
  begin
    XSamplingScale := TVXXYZGrid(Source).XSamplingScale;
    YSamplingScale := TVXXYZGrid(Source).YSamplingScale;
    ZSamplingScale := TVXXYZGrid(Source).ZSamplingScale;
    FParts := TVXXYZGrid(Source).FParts;
    FLinesStyle := TVXXYZGrid(Source).FLinesStyle;
  end;
  inherited Assign(Source);
end;

// SetXSamplingScale
//

procedure TVXXYZGrid.SetXSamplingScale(const val: TVXSamplingScale);
begin
  FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//

procedure TVXXYZGrid.SetYSamplingScale(const val: TVXSamplingScale);
begin
  FYSamplingScale.Assign(val);
end;

// SetZSamplingScale
//

procedure TVXXYZGrid.SetZSamplingScale(const val: TVXSamplingScale);
begin
  FZSamplingScale.Assign(val);
end;

// SetParts
//

procedure TVXXYZGrid.SetParts(const val: TXYZGridParts);
begin
  if FParts <> val then
  begin
    FParts := val;
    StructureChanged;
  end;
end;

// SetLinesStyle
//

procedure TVXXYZGrid.SetLinesStyle(const val: TXYZGridLinesStyle);
begin
  if FLinesStyle <> val then
  begin
    FLinesStyle := val;
    StructureChanged;
  end;
end;

// SetLinesSmoothing
//

procedure TVXXYZGrid.SetLinesSmoothing(const val: Boolean);
begin
  AntiAliased := val;
end;

// NotifyChange
//

procedure TVXXYZGrid.NotifyChange(Sender: TObject);
begin
  if Sender is TVXSamplingScale then
    StructureChanged;
  inherited NotifyChange(Sender);
end;

// BuildList
//

procedure TVXXYZGrid.BuildList(var rci: TVXRenderContextInfo);
var
  xBase, x, xStep, xMax, yBase, y, yStep, yMax, zBase, z, zStep, zMax: Single;
begin
  SetupLineStyle(rci);
  // precache values
  XSamplingScale.SetBaseStepMaxToVars(xBase, xStep, xMax, (gpX in Parts));
  YSamplingScale.SetBaseStepMaxToVars(yBase, yStep, yMax, (gpY in Parts));
  ZSamplingScale.SetBaseStepMaxToVars(zBase, zStep, zMax, (gpZ in Parts));
  // render X parallel lines
  if gpX in Parts then
  begin
    y := yBase;
    while y <= yMax do
    begin
      z := zBase;
      while z <= zMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          x := xBase;
          while x <= xMax do
          begin
            glVertex3f(x, y, z);
            x := x + xStep;
          end;
        end
        else
        begin
          glVertex3f(XSamplingScale.Min, y, z);
          glVertex3f(XSamplingScale.Max, y, z);
        end;
        glEnd;
        z := z + zStep;
      end;
      y := y + yStep;
    end;
  end;
  // render Y parallel lines
  if gpY in Parts then
  begin
    x := xBase;
    while x <= xMax do
    begin
      z := zBase;
      while z <= zMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          y := yBase;
          while y <= yMax do
          begin
            glVertex3f(x, y, z);
            y := y + yStep;
          end;
        end
        else
        begin
          glVertex3f(x, YSamplingScale.Min, z);
          glVertex3f(x, YSamplingScale.Max, z);
        end;
        glEnd;
        z := z + zStep;
      end;
      x := x + xStep;
    end;
  end;
  // render Z parallel lines
  if gpZ in Parts then
  begin
    x := xBase;
    while x <= xMax do
    begin
      y := yBase;
      while y <= yMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          z := zBase;
          while z <= zMax do
          begin
            glVertex3f(x, y, z);
            z := z + zStep;
          end;
        end
        else
        begin
          glVertex3f(x, y, ZSamplingScale.Min);
          glVertex3f(x, y, ZSamplingScale.Max);
        end;
        glEnd;
        y := y + yStep;
      end;
      x := x + xStep;
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

RegisterClasses([TVXHeightField, TVXXYZGrid]);

end.
