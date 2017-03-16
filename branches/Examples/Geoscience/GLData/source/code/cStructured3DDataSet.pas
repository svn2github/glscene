{ -------------------------------------------------------------------------------
  Unit Name: cStructured3DDataset
  Author:    HochwimmerA
  Purpose:   Code for managing a 3D structured dataset - has provision
  $Id: cStructured3DDataSet.pas,v 1.2 2003/10/28 04:18:51 hochwimmera Exp $
  ------------------------------------------------------------------------------- }
unit cStructured3DDataSet;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.Dialogs,
   
  GLVectorgeometry, GLGraph, GLMesh, GLTexture, GLScene, GLObjects,
  GLVectorTypes, cUtilities, GLColor, GLMaterial;

type
  { : T3DSPoint is a class containing information for one point in the
    structured grid. It only holds scalar data }
  T3DSPoint = class(TObject)
  private
    fScalarVal: single; // scalar value
  public
    property ScalarVal: single read fScalarVal write fScalarVal;
  end;

  { : T3DVPoint is a class containing information for one point in the
    structured grid. It only holds vector data }
  T3DVPoint = class(TObject)
  private
    fDirection: TVector; // direction
    fMagnitude: single; // magnitude (positive)
  public
    property Direction: TVector read fDirection write fDirection;
    property Magnitude: single read fMagnitude write fMagnitude;
  end;

  T3DStructuredDataSet = class;

  T3DStructuredScalar = class(TObject)
  private
    fCutXY: TGLHeightField; // cut plane for constant Z
    fCutXZ: TGLHeightField; // cut plane for constant Y
    fCutYZ: TGLHeightField; // cut plane for constant X
    fDescription: string;
    fDummyCube: TGLDummyCube; // used to group objects
    fParentGrid: T3DStructuredDataSet; // external pointer
    fPointList: TStringList;
    fMaxValue: single;
    fMinValue: single;
    fXLevel: integer;
    fYLevel: integer;
    fZLevel: integer;
  protected
    procedure ContourXY(const x, y: single; var z: single;
      var color: TColorVector; var texPoint: TTexPoint);
    procedure ContourXZ(const x, y: single; var z: single;
      var color: TColorVector; var texPoint: TTexPoint);
    procedure ContourYZ(const x, y: single; var z: single;
      var color: TColorVector; var texPoint: TTexPoint);

    procedure Index_IJK(const iIndex: integer; var I, J, K: integer);
    procedure InitCutPlanes;
  public
    constructor Create(aParent: T3DStructuredDataSet);
    destructor Destroy; override;
    procedure GetMinMax;
    property Description: string read fDescription write fDescription;
    property PointList: TStringList read fPointList write fPointList;
    property XLevel: integer read fXLevel write fXLevel;
    property YLevel: integer read fYLevel write fYLevel;
    property ZLevel: integer read fZLevel write fZLevel;

    // use to group gl data below the "parent cube"
    property DummyCube: TGLDummyCube read fDummyCube write fDummyCube;
    property CutXY: TGLHeightField read fCutXY write fCutXY;
    // cut plane for constant Z
    property CutXZ: TGLHeightField read fCutXZ write fCutXZ;
    // cut plane for constant Y
    property CutYZ: TGLHeightField read fCutYZ write fCutYZ;
    // cut plane for constant X    property MaxValue : single read fMaxValue write fMaxValue;
    property MaxValue: single read fMaxValue write fMaxValue;
    property MinValue: single read fMinValue write fMinValue;
    property ParentGrid: T3DStructuredDataSet read fParentGrid
      write fParentGrid;
  end;

  { : T3DStructuredDataSet is a class for managing a structured 3D dataset.
    At its core it manages a list of T3GeneralPoint. GLScene objects that are
    related to the 3D structured dataset are handed in the fDummyCube obtained
    via the constructor. Note that the number of data items n of each type of data
    must match the number of points or cells in the dataset }

  T3DStructuredDataSet = class(TObject)
  private
    FBox: TGLXYZGrid;
    fDescription: string;
    fDummyCube: TGLDummyCube; // external pointer
    FN: integer;
    FNX: integer;
    FNY: integer;
    FNZ: integer;
    FNYNZ: integer; // used for converting index <-> i,j,k
    FScalarData: TStringList; // maintains a list T3DStructuredScalar
    FVectorData: TStringList; // maintains a list T3DStructuredVector
    Fxdelta: double;
    Fxlo: double;
    Fxhi: double;
    Fydelta: double;
    Fylo: double;
    Fyhi: double;
    Fzdelta: double;
    Fzlo: double;
    Fzhi: double;
    procedure GenerateBoundingBox;
  protected

  public
    constructor Create(aDummyCube: TGLDummyCube);
    destructor Destroy; override;

    function ImportVTKFile(sFileName: string): integer;

    property Box: TGLXYZGrid read FBox write FBox;
    property Description: string read fDescription write fDescription;
    property DummyCube: TGLDummyCube read fDummyCube write fDummyCube; // ext
    property N: integer read FN write FN;
    property NX: integer read FNX write FNX;
    property NY: integer read FNY write FNY;
    property NYNZ: integer read FNYNZ write FNYNZ;
    property NZ: integer read FNZ write FNZ;
    property ScalarData: TStringList read FScalarData write FScalarData;
    property VectorData: TStringList read FVectorData write FVectorData;
    property xDelta: double read Fxdelta write Fxdelta;
    property xlo: double read Fxlo write Fxlo;
    property xhi: double read Fxhi write Fxhi;
    property yDelta: double read Fydelta write Fydelta;
    property ylo: double read Fylo write Fylo;
    property yhi: double read Fyhi write Fyhi;
    property zDelta: double read Fzdelta write Fzdelta;
    property zlo: double read Fzlo write Fzlo;
    property zhi: double read Fzhi write Fzhi;
  end;

implementation

// =============================================================================
// T3DStructuredScalar Implementation
// =============================================================================
{ : Converts 1d iIndex into i,j,k notation }
procedure T3DStructuredScalar.Index_IJK(const iIndex: integer;
  var I, J, K: integer);

begin
  I := iIndex div ParentGrid.NYNZ; // int division;
  J := (iIndex - ParentGrid.NYNZ * I) div ParentGrid.NZ; // int division
  K := (iIndex - ParentGrid.NYNZ * I - ParentGrid.NZ * J);
end;

// ----- T3DStructuredScalar.InitCutPlanes -------------------------------------
procedure T3DStructuredScalar.InitCutPlanes;

begin
  // with CutXY do
  CutXY.Position.z := ParentGrid.zlo;
  CutXY.XSamplingScale.Min := ParentGrid.xlo;
  CutXY.XSamplingScale.Max := ParentGrid.xhi + 0.01 * ParentGrid.xDelta;
  CutXY.XSamplingScale.Origin := ParentGrid.xlo;
  CutXY.XSamplingScale.Step := ParentGrid.xDelta;
  CutXY.YSamplingScale.Min := ParentGrid.ylo;
  CutXY.YSamplingScale.Max := ParentGrid.yhi + 0.01 * ParentGrid.yDelta;
  CutXY.YSamplingScale.Origin := ParentGrid.ylo;
  CutXY.YSamplingScale.Step := ParentGrid.yDelta;
  CutXY.ColorMode := hfcmDiffuse;
  CutXY.OngetHeight := ContourXY;
  CutXY.StructureChanged;

  // with CutXZ do
  CutXZ.Up.AsVector := ZHmgVector;
  CutXZ.Direction.AsVector := yHmgVector;
  CutXZ.Direction.y := -1;
  CutXZ.Position.z := ParentGrid.ylo;
  CutXZ.XSamplingScale.Min := ParentGrid.xlo;
  CutXZ.XSamplingScale.Max := ParentGrid.xhi + 0.01 * ParentGrid.xDelta;
  CutXZ.XSamplingScale.Origin := ParentGrid.xlo;
  CutXZ.XSamplingScale.Step := ParentGrid.xDelta;
  CutXZ.YSamplingScale.Min := ParentGrid.zlo;
  CutXZ.YSamplingScale.Max := ParentGrid.zhi + 0.01 * ParentGrid.zDelta;
  CutXZ.YSamplingScale.Origin := ParentGrid.zlo;
  CutXZ.YSamplingScale.Step := ParentGrid.zDelta;
  CutXZ.ColorMode := hfcmEmission;
  CutXZ.OngetHeight := ContourXZ;
  CutXZ.StructureChanged;

  // with CutYZ do
  CutYZ.Up.AsVector := ZHmgVector;
  CutYZ.Direction.AsVector := xHmgVector;
  CutYZ.Position.z := ParentGrid.xlo;
  CutYZ.XSamplingScale.Min := ParentGrid.ylo;
  CutYZ.XSamplingScale.Max := ParentGrid.yhi + 0.01 * ParentGrid.yDelta;
  CutYZ.XSamplingScale.Origin := ParentGrid.ylo;
  CutYZ.XSamplingScale.Step := ParentGrid.yDelta;
  CutYZ.YSamplingScale.Min := ParentGrid.zlo;
  CutYZ.YSamplingScale.Max := ParentGrid.zhi + 0.01 * ParentGrid.zDelta;
  CutYZ.YSamplingScale.Origin := ParentGrid.zlo;
  CutYZ.YSamplingScale.Step := ParentGrid.zDelta;
  CutYZ.ColorMode := hfcmEmission;
  CutYZ.OngetHeight := ContourYZ;
  CutYZ.StructureChanged;
end;

// ----- T3DStructuredScalar.ContourXY -----------------------------------------
procedure T3DStructuredScalar.ContourXY(const x, y: single; var z: single;
  var color: TColorVector; var texPoint: TTexPoint);

const
  SMALL = 0.01;

var
  I, J, K, iIndex: integer;
  dp: single;
  dratio, rstar: double;
  col1, col2: TColor;
  bforward: boolean;
  ColorVect: TColorVector;
  glC1, glC2: TGLColor;

begin
  // flat - its a cut plane
  z := 0;

  I := Trunc((x - ParentGrid.xlo - 0.01 * ParentGrid.xDelta) /
    ParentGrid.xDelta);
  J := Trunc((y - ParentGrid.ylo - 0.01 * ParentGrid.yDelta) /
    ParentGrid.yDelta);
  if ZLevel = ParentGrid.NZ then
    K := ZLevel - 1
  else
    K := ZLevel;

  iIndex := K * ParentGrid.NX * ParentGrid.NY;
  iIndex := iIndex + J * ParentGrid.NY;
  iIndex := iIndex + I;

  if iIndex < PointList.Count - 1 then
  begin

    dp := T3DSPoint(PointList.Objects[iIndex]).ScalarVal;
    dratio := (MaxValue - dp) / (MaxValue - MinValue);
    bforward := false;
    ColourRamp(dratio, bforward, col1, col2, rstar);

    glC1 := TGLColor.Create(nil);
    glC2 := TGLColor.Create(nil);
    glC1.AsWinColor := col1;
    glC2.AsWinColor := col2;

    VectorLerp(glC1.color, glC2.color, rstar, colorvect);

    color := colorvect;

    glC1.Free;
    glC2.Free;
    color.V[3] := 0.75;

  end
  else
    color := clrwhite;
end;

// ----- T3DStructuredScalar.ContourXZ-----------------------------------------
procedure T3DStructuredScalar.ContourXZ(const x, y: single; var z: single;
  var color: TColorVector; var texPoint: TTexPoint);

const
  SMALL = 0.01;

var
  I, J, K, iIndex: integer;
  dp: single;
  dratio, rstar: double;
  col1, col2: TColor;
  bforward: boolean;
  colorvect: TColorVector;
  glC1, glC2: TGLColor;

begin
  // flat - its a cut plane
  z := 0.0;

  I := Trunc((x - ParentGrid.xlo - 0.01 * ParentGrid.xDelta) /
    ParentGrid.xDelta);
  J := Trunc((y - ParentGrid.zlo - 0.01 * ParentGrid.zDelta) /
    ParentGrid.zDelta);
  if YLevel = ParentGrid.NY then
    K := YLevel - 1
  else
    K := YLevel;

  iIndex := J * ParentGrid.NX * ParentGrid.NY;
  iIndex := iIndex + K * ParentGrid.NX;
  iIndex := iIndex + I;

  if iIndex < PointList.Count - 1 then
  begin

    dp := T3DSPoint(PointList.Objects[iIndex]).ScalarVal;
    dratio := (MaxValue - dp) / (MaxValue - MinValue);
    bforward := false;
    ColourRamp(dratio, bforward, col1, col2, rstar);

    glC1 := TGLColor.Create(nil);
    glC2 := TGLColor.Create(nil);
    glC1.AsWinColor := col1;
    glC2.AsWinColor := col2;

    VectorLerp(glC1.color, glC2.color, rstar, colorvect);

    color := colorvect;
    color.V[3] := 0.75;

    // color := clrRed;
    glC1.Free;
    glC2.Free;
  end
  else
    color := clrwhite;
end;

// ----- T3DStructuredScalar.ContourYZ-----------------------------------------
procedure T3DStructuredScalar.ContourYZ(const x, y: single; var z: single;
  var color: TColorVector; var texPoint: TTexPoint);

const
  SMALL = 0.01;

var
  I, J, K, iIndex: integer;
  dp: single;
  dratio, rstar: double;
  col1, col2: TColor;
  bforward: boolean;
  colorvect: TColorVector;
  glC1, glC2: TGLColor;

begin
  // flat - its a cut plane
  z := 0.0;

  I := Trunc((x - ParentGrid.ylo - 0.01 * ParentGrid.yDelta) /
    ParentGrid.yDelta);
  J := Trunc((y - ParentGrid.zlo - 0.01 * ParentGrid.zDelta) /
    ParentGrid.zDelta);
  if XLevel = ParentGrid.NX then
    K := XLevel - 1
  else
    K := XLevel;

  iIndex := J * ParentGrid.NX * ParentGrid.NY;
  iIndex := iIndex + I * ParentGrid.NY;

  iIndex := iIndex + K;

  if iIndex < PointList.Count - 1 then
  begin

    dp := T3DSPoint(PointList.Objects[iIndex]).ScalarVal;
    dratio := (MaxValue - dp) / (MaxValue - MinValue);
    bforward := false;
    ColourRamp(dratio, bforward, col1, col2, rstar);

    glC1 := TGLColor.Create(nil);
    glC2 := TGLColor.Create(nil);
    glC1.AsWinColor := col1;
    glC2.AsWinColor := col2;

    VectorLerp(glC1.color, glC2.color, rstar, colorvect);

    color := colorvect;
    // color := clrRed;
    color.V[3] := 0.75;

    glC1.Free;
    glC2.Free;
  end
  else
    color := clrwhite;
end;

// ----- T3DStructuredScalar.Create --------------------------------------------
constructor T3DStructuredScalar.Create(aParent: T3DStructuredDataSet);

begin
  ParentGrid := aParent;
  PointList := TStringList.Create;
  DummyCube := TGLDummyCube(ParentGrid.DummyCube.AddNewChild(TGLDummyCube));
  CutXY := TGLHeightField(DummyCube.AddNewChild(TGLHeightField));
  CutXY.Material.BlendingMode := bmTransparency;
  CutXZ := TGLHeightField(DummyCube.AddNewChild(TGLHeightField));
  CutXZ.Material.BlendingMode := bmTransparency;
  CutYZ := TGLHeightField(DummyCube.AddNewChild(TGLHeightField));
  CutYZ.Material.BlendingMode := bmTransparency;

  ZLevel := 0;

  InitCutPlanes;
end;

// ----- T3DStructuredScalar.Destroy -------------------------------------------
destructor T3DStructuredScalar.Destroy;

begin

  CutXY.Free;
  CutXZ.Free;
  CutYZ.Free;
  DummyCube.Free;
  PointList.Free;
end;

// ----- T3DStructuredScalar.GetMinMax -----------------------------------------
procedure T3DStructuredScalar.GetMinMax;

const
  BIG = 1E30;

var
  I: integer;
  d: double;

begin
  MinValue := BIG;
  MaxValue := -BIG;
  for I := 0 to PointList.Count - 1 do
  begin
    d := T3DSPoint(PointList.Objects[I]).ScalarVal;
    if d > MaxValue then
      MaxValue := d;
    if d < MinValue then
      MinValue := d;
  end;
end;

// =============================================================================
// T3DStructuredDataSet Implementation
// =============================================================================
procedure T3DStructuredDataSet.GenerateBoundingBox;

begin
  // probably should be done after reading in?
  Box.Parts := Box.Parts + [gpZ];
  Box.LineColor.AsWinColor := clBlue;
  with Box.XSamplingScale do
  begin
    Origin := xlo;
    Min := xlo;
    Step := (xhi - xlo);
    Max := xhi;
  end;
  with Box.YSamplingScale do
  begin
    Origin := ylo;
    Min := ylo;
    Step := (yhi - ylo);
    Max := yhi;
  end;
  with Box.ZSamplingScale do
  begin
    Origin := zlo;
    Min := zlo;
    Step := (zhi - zlo);
    Max := zhi;
  end;
end;

// ----- T3DStructuredDataSet.Create -------------------------------------------
constructor T3DStructuredDataSet.Create(aDummyCube: TGLDummyCube);

begin
  inherited Create;
  fDummyCube := aDummyCube; { ** assign externally }
  FBox := TGLXYZGrid(DummyCube.AddNewChild(TGLXYZGrid));
  FScalarData := TStringList.Create;
  FVectorData := TStringList.Create;
end;

// ------ T3DStructuredDataSet.Destroy -----------------------------------------
destructor T3DStructuredDataSet.Destroy;

begin
  FBox.Free;

  // clear objects first
  while ScalarData.Count > 0 do
  begin
    T3DStructuredScalar(FScalarData.Objects[0]).Free;
    FScalarData.Delete(0);
  end;
  FScalarData.Free;
  FVectorData.Free;
  inherited Destroy;
end;

// ----- T3dStructuredDataSet.ImportVTKFile ------------------------------------
function T3DStructuredDataSet.ImportVTKFile(sFileName: string): integer;

var
  sLine, s1, sTemp: string;
  lines: TStringList;
  ic, iPos, iIndex: integer;
  bSeek: boolean;
  sds: T3DStructuredScalar;
  spt: T3DSPoint;
  I, J, K: integer;

  // ----- SeekNextLine ----------------------------------------------------------
  procedure SeekNextLine;
  var
    bSeek: boolean;
  begin
    bSeek := true;
    while bSeek do
    begin
      sLine := lines.Strings[ic];
      if (Trim(sLine) = '') then
        Inc(ic)
      else
        bSeek := false;
    end;
  end;

begin
  lines := TStringList.Create;
  lines.LoadFromFile(sFileName);
  ic := 0;
  // read header
  sLine := lines.Strings[0];
  if Pos('# vtk DataFile Version', sLine) > 0 then
  begin
    Inc(ic);
    Description := lines.Strings[1];
    Inc(ic);
    if (lines.Strings[ic] = 'ASCII') then
    begin
      Inc(ic);
      bSeek := true;
      while bSeek do
      begin
        sLine := lines.Strings[ic];
        if (Trim(sLine) = '') then
          Inc(ic)
        else
          bSeek := false;
      end;
      if (sLine <> 'DATASET STRUCTURED_POINTS') then
      begin
        MessageDlg('Only STRUCTURED_POINTS supported!', mtError, [mbOK], 0);
        lines.Free;
        exit;
      end;
      Inc(ic);

      // read the Dimensions. Note NX, NY, NZ must be greater than 1
      sLine := lines[ic];
      StripString(sLine, s1); // "Dimensions"
      StripString(sLine, s1);
      NX := StrToInt(s1);
      StripString(sLine, s1);
      NY := StrToInt(s1);
      StripString(sLine, s1);
      NZ := StrToInt(s1);
      Inc(ic);
      NYNZ := NY * NZ;

      { ** read in the origin position }
      sLine := lines[ic];
      StripString(sLine, s1); // "Type"
      StripString(sLine, s1);
      xlo := StrToFloat(s1);
      StripString(sLine, s1);
      ylo := StrToFloat(s1);
      StripString(sLine, s1);
      zlo := StrToFloat(s1);
      Inc(ic);

      // read in the deltas/steps
      sLine := lines[ic];
      StripString(sLine, s1);
      StripString(sLine, s1);
      xDelta := StrToFloat(s1);
      StripString(sLine, s1);
      yDelta := StrToFloat(s1);
      StripString(sLine, s1);
      zDelta := StrToFloat(s1);

      xhi := xlo + NX * xDelta;
      yhi := ylo + NY * yDelta;
      zhi := zlo + NZ * zDelta;
      Inc(ic);

      GenerateBoundingBox;
      SeekNextLine; // this sets the current values of sLine

      StripString(sLine, s1);
      StripString(sLine, s1);
      N := StrToInt(s1);
      Inc(ic);
      Inc(ic); // skip scalar/vector
      Inc(ic); // skip lookup table

      SeekNextLine;

      sds := T3DStructuredScalar.Create(self); // will initialise cut planes
      ScalarData.AddObject('1', sds);
      while (T3DStructuredScalar(ScalarData.Objects[0]).PointList.Count < N) do
      begin
        while (sLine <> '') do
        begin
          StripString(sLine, s1);
          spt := T3DSPoint.Create;
          spt.ScalarVal := StrToFloat(s1);
          iIndex := T3DStructuredScalar(ScalarData.Objects[0]).PointList.Count;
          T3DStructuredScalar(ScalarData.Objects[0])
            .PointList.AddObject(IntToStr(iIndex + 1), spt);
        end;
        if (ic < lines.Count - 1) then
        begin
          Inc(ic);
          sLine := lines[ic];
        end;
      end;
      T3DStructuredScalar(ScalarData.Objects[0]).GetMinMax;
    end
    else
      MessageDlg('Only ASCII files supported..', mtWarning, [mbOK], 0);
  end
  else
    MessageDlg('Not a VTK File!', mtError, [mbOK], 0);

  lines.Free;
end;

end.
