{ -------------------------------------------------------------------------------
  Unit Name: contourtypes
  Author:    HochwimmerA (aaron@graphic-edge.co.nz)
  Purpose:   classes to display x,y,z data points in GLSCene via Proxy Objects
  History:
  ------------------------------------------------------------------------------- }
unit cContourTypes;

interface

uses
  System.Classes, System.SysUtils, Data.DB,
   
  GLScene, Graphics, GLGeomObjects, GLObjects, GLVectorgeometry,
  GLVectorTypes, GLTexture, cColourSpectrum, GLMaterial, GLColor, GLState;

type
  TContourPointCollection = class;
  TVectorField = class;

  TVectorPoint = class(TObject)
  private
    fArrow: TGLArrowLine;
    fBookMark: TBookMark;
    fX: double;
    fY: double;
    fZ: double;
    fMagnitude: double;
    fParent: TVectorField;
  public
    constructor Create(aParent: TVectorField);
    destructor Destroy; override;
    property Arrow: TGLArrowLine read fArrow write fArrow;
    property Bookmark: TBookMark read fBookMark write fBookMark;
    property Magnitude: double read fMagnitude write fMagnitude;
    property X: double read fX write fX;
    property Y: double read fY write fY;
    property Z: double read fZ write fZ;
  end;

  TContourPoint = class(TObject)
  private
    fBookMark: TBookMark;
    fParent: TContourPointCollection;
    fPoint: TGLProxyObject;
    fX: double;
    fY: double;
    fZ: double;
    fValue: double;
    fIsNull: boolean;
    fPointLabel: string;
  public
    constructor Create(aParent: TContourPointCollection);
    destructor Destroy; override;
    procedure DisplayPoint(position: TVector);
    property Bookmark: TBookMark read fBookMark write fBookMark;
    property Parent: TContourPointCollection read fParent write fParent;

    property IsNull: boolean read fIsNull write fIsNull;
    property Point: TGLProxyObject read fPoint write fPoint;
    property PointLabel: string read fPointLabel write fPointLabel;
    property Value: double read fValue write fValue;
    property X: double read fX write fX;
    property Y: double read fY write fY;
    property Z: double read fZ write fZ;
  end;

  TVectorField = class(TObject)
  private
    fDummyCube: TGLDummyCube;
    fColPalette: TColourSpectrum;
    fMaxArrowHeadHeight: single;
    fMaxArrowHeadRadius: single;
    fMaxArrowLength: single;
    fMaxArrowRadius: single;
    fMinArrowHeadHeight: single;
    fMinArrowHeadRadius: single;
    fMinArrowLength: single;
    fMinArrowRadius: single;
    fScaleX: single;
    fScaleY: single;
    fScaleZ: single;
    fSlices: integer;
    fStacks: integer;
    fPointList: TStringList;
    fZeroMin: boolean;
  protected
    function GetMaxMag: double;
    function GetMinMag: double;
    procedure SetScaleX(iScale: single);
    procedure SetScaleY(iScale: single);
    procedure SetScaleZ(iScale: single);
    procedure SetSlices(iSlices: integer);
    procedure SetStacks(iStacks: integer);
  public
    constructor Create(aDummyCube: TGLDummyCube);
    destructor Destroy; override;
    procedure ClearAll;
    procedure CalcBounds(var dleft, dright, dBack, dForward, dBottom,
      dtop: double);
    procedure Insert(xp, yp, zp, mag, ux, uy, uz: double; sLabel: string;
      aBookMark: TBookMark);
    procedure RenderField;
    property ColPalette: TColourSpectrum read fColPalette write fColPalette;
    property DummyCube: TGLDummyCube read fDummyCube write fDummyCube;
    property MaxArrowHeadHeight: single read fMaxArrowHeadHeight
      write fMaxArrowHeadHeight;
    property MaxArrowHeadRadius: single read fMaxArrowHeadRadius
      write fMaxArrowHeadRadius;
    property MaxArrowLength: single read fMaxArrowLength write fMaxArrowLength;
    property MaxArrowRadius: single read fMaxArrowRadius write fMaxArrowRadius;
    property MaxMag: double read GetMaxMag;
    property MinArrowHeadHeight: single read fMinArrowHeadHeight
      write fMinArrowHeadHeight;
    property MinArrowHeadRadius: single read fMinArrowHeadRadius
      write fMinArrowHeadRadius;
    property MinArrowLength: single read fMinArrowLength write fMinArrowLength;
    property MinArrowRadius: single read fMinArrowRadius write fMinArrowRadius;
    property MinMag: double read GetMinMag;
    property PointList: TStringList read fPointList;
    property ScaleX: single read fScaleX write SetScaleX;
    property ScaleY: single read fScaleY write SetScaleY;
    property ScaleZ: single read fScaleZ Write SetScaleZ;
    property Slices: integer read fSlices write SetSlices;
    property Stacks: integer read fStacks write SetStacks;
    property ZeroMin: boolean read fZeroMin write fZeroMin;
  end;

  TContourPointCollection = class(TObject)
  private
    fPointList: TStringList;
    fColour: TColor;

    fShown: boolean;
    fPointType: string;
    fRadius: double;
    fSlices: integer;
    fStacks: integer;
    fWireFrame: boolean;

    fScaleX: single;
    fScaleY: single;
    fScaleZ: single;

    fMaster: TGLSphere;
    fDummyCube: TGLDummyCube;
  protected
    function GetCount: integer;
    function GetMinValue: double;
    function GetMaxValue: double;

    procedure SetRadius(aRadius: double);
    procedure SetColour(aColour: TColor);
    procedure SetShown(bShown: boolean);
    procedure SetSlices(iSlices: integer);
    procedure SetStacks(iStacks: integer);
    procedure SetWireFrame(bWireFrame: boolean);

    procedure SetScaleX(iScale: single);
    procedure SetScaleY(iScale: single);
    procedure SetScaleZ(iScale: single);
  public
    constructor Create(aDummyCube: TGLDummyCube; aMaster: TGLSphere);
    destructor Destroy; override;
    procedure ClearAll;
    procedure CalcBounds(var dleft, dright, dBack, dForward, dBottom,
      dtop: double);
    procedure Insert(dx, dy, dz, val: double; bIsNull: boolean; sLabel: string;
      aBookMark: TBookMark);
    property PointType: string read fPointType write fPointType;

    property Count: integer read GetCount;

    property MinValue: double read GetMinValue;
    property MaxValue: double read GetMaxValue;

    property Shown: boolean read fShown write SetShown;
    property Radius: double read fRadius write SetRadius;
    property Slices: integer read fSlices write SetSlices;
    property Stacks: integer read fStacks write SetStacks;
    property WireFrame: boolean read fWireFrame write SetWireFrame;
    property Colour: TColor read fColour write SetColour; // windows color
    property PointList: TStringList read fPointList write fPointList;

    property ScaleX: single read fScaleX write SetScaleX;
    property ScaleY: single read fScaleY write SetScaleY;
    property ScaleZ: single read fScaleZ Write SetScaleZ;

    { ** these properties are assigned in the Create Method }
    property Master: TGLSphere read fMaster write fMaster;
    property DummyCube: TGLDummyCube read fDummyCube write fDummyCube;
  end;

implementation

uses
  cUtilities;

// =============================================================================
// TVECTORPOINT Implementation
// =============================================================================
// ----- TVectorPoint.Create ---------------------------------------------------
constructor TVectorPoint.Create(aParent: TVectorField);

begin
  inherited Create;
  fParent := aParent;
  fArrow := TGLArrowLine(fParent.DummyCube.AddNewChild(TGLArrowLine));
end;

// ----- TVectorPoint.Destroy --------------------------------------------------
destructor TVectorPoint.Destroy;

begin
  fArrow.Free;
  inherited Destroy;
end;

// =============================================================================
// TCONTOURPOINT Implementation
// =============================================================================
// ----- TContourPoint.Create --------------------------------------------------
constructor TContourPoint.Create(aParent: TContourPointCollection);

begin
  inherited Create;
  fParent := aParent;
  fPoint := TGLProxyObject(Parent.DummyCube.AddNewChild(TGLProxyObject));
end;

// ------ TContourPoint.Destroy ------------------------------------------------
destructor TContourPoint.Destroy;

begin
  fPoint.Free;
  inherited Destroy;
end;

// ------ TContourPoint.DisplayPoint -------------------------------------------
procedure TContourPoint.DisplayPoint(position: TVector);

begin
  Point.Visible := Parent.Shown;
  Point.position.AsVector := position;
end;

{ ** TContourPointCollection Implementation ==================================== }
// ------ TContourPointCollection.Create ---------------------------------------
constructor TContourPointCollection.Create(aDummyCube: TGLDummyCube;
  aMaster: TGLSphere);

begin
  inherited Create;

  fDummyCube := aDummyCube;
  fMaster := aMaster;

  fRadius := 0.05; // metres
  fColour := clRed;
  fMaster.Visible := false;
  fPointList := TStringList.Create;
  fWireFrame := false;
  fSlices := 6;
  fStacks := 6;
  fShown := true;

  fScaleX := 1.0;
  fScaleY := 1.0;
  fScaleZ := 1.0;

  fPointType := 'Point';
end;

// ------ TContourPointCollection.Destroy --------------------------------------
destructor TContourPointCollection.Destroy;

begin
  ClearAll;
  PointList.Free;
  inherited Destroy;
end;

// ----- TContourPointCollection.GetCount --------------------------------------
function TContourPointCollection.GetCount: integer;

begin
  result := PointList.Count;
end;

// ----- TContourPointCollection.GetMinValue -----------------------------------
function TContourPointCollection.GetMinValue: double;

var
  i: integer;
  dMin: double;

begin
  dMin := 1E30;
  for i := 0 to PointList.Count - 1 do
  begin
    if TContourPoint(PointList.Objects[i]).IsNull then
      continue;

    if TContourPoint(PointList.Objects[i]).Value < dMin then
      dMin := TContourPoint(PointList.Objects[i]).Value;
  end;
  result := dMin;
end;

// ----- TContourPointCollection.GetMaxValue -----------------------------------
function TContourPointCollection.GetMaxValue: double;

var
  i: integer;
  dMax: double;

begin
  dMax := -1E30;
  for i := 0 to PointList.Count - 1 do
  begin
    if TContourPoint(PointList.Objects[i]).IsNull then
      continue;

    if TContourPoint(PointList.Objects[i]).Value > dMax then
      dMax := TContourPoint(PointList.Objects[i]).Value;
  end;
  result := dMax;
end;

// ----- TContourPointCollection.SetRadius -------------------------------------
procedure TContourPointCollection.SetRadius(aRadius: double);

begin
  fRadius := aRadius;
  fMaster.Radius := fRadius;
end;

// ------ TContourPointsCollection.SetWireFrame --------------------------------
procedure TContourPointCollection.SetWireFrame(bWireFrame: boolean);

begin
  fWireFrame := bWireFrame;
  if bWireFrame then
    fMaster.Material.PolygonMode := pmLines
  else
    fMaster.Material.PolygonMode := pmFill;
  fMaster.StructureChanged;
end;

// ----- TContourPointCollection.SetScaleX -------------------------------------
procedure TContourPointCollection.SetScaleX(iScale: single);

var
  i: integer;

begin
  fScaleX := iScale;
  for i := 0 to PointList.Count - 1 do
  begin
    with TContourPoint(PointList.Objects[i]) do
      Point.position.X := (iScale * X);
  end;
end;

// ----- TContourPointCollection.SetScaleY -------------------------------------
procedure TContourPointCollection.SetScaleY(iScale: single);

var
  i: integer;

begin
  fScaleY := iScale;
  for i := 0 to PointList.Count - 1 do
  begin
    with TContourPoint(PointList.Objects[i]) do
      Point.position.Y := (iScale * Y);
  end;
end;

// ----- TContourPointCollection.SetScaleZ -------------------------------------
procedure TContourPointCollection.SetScaleZ(iScale: single);

var
  i: integer;

begin
  fScaleZ := iScale;
  for i := 0 to PointList.Count - 1 do
  begin
    with TContourPoint(PointList.Objects[i]) do
      Point.position.Z := (iScale * Z);
  end;
end;

// ------ TContourPointCollection.SetSlices ------------------------------------
procedure TContourPointCollection.SetSlices(iSlices: integer);

begin
  fSlices := iSlices;
  fMaster.Slices := iSlices;
end;

// ----- TContourPointCollection.SetStacks -------------------------------------
procedure TContourPointCollection.SetStacks(iStacks: integer);

begin
  fStacks := iStacks;
  fMaster.Stacks := iStacks;
end;

// ----- TContourPointCollection.SetColour -------------------------------------
procedure TContourPointCollection.SetColour(aColour: TColor);

begin
  fColour := aColour;
  fMaster.Material.FrontProperties.Emission.AsWinColor := aColour;
  fMaster.StructureChanged;
end;

// ------ TContourPointCollection.SetShown -------------------------------------
procedure TContourPointCollection.SetShown(bShown: boolean);

var
  i: integer;

begin
  fShown := bShown;
  for i := 0 to PointList.Count - 1 do
  begin
    with TContourPoint(PointList.Objects[i]) do
      Point.Visible := bShown
  end;
end;

// ------ TContourPointCollection.ClearAll -------------------------------------
procedure TContourPointCollection.ClearAll;

begin
  while PointList.Count > 0 do
  begin
    PointList.Objects[0].Free;
    PointList.Delete(0);
  end
end;

// ------ TContourPointCollection.CalcBounds -----------------------------------
procedure TContourPointCollection.CalcBounds(var dleft, dright, dBack, dForward,
  dBottom, dtop: double);

var
  i: integer;

begin
  dleft := 1E30;
  dright := -1E30;
  dBack := 1E30;
  dForward := -1E30;
  dtop := -1E30;
  dBottom := 1E30;

  for i := 0 to PointList.Count - 1 do
  begin
    with TContourPoint(PointList.Objects[i]) do
    begin
      if X < dleft then
        dleft := X;
      if X > dright then
        dright := X;
      if Y < dBack then
        dBack := Y;
      if Y > dForward then
        dForward := Y;
      if Z < dBottom then
        dBottom := Z;
      if Z > dtop then
        dtop := Z;
    end;
  end;
end;

// ------ TContourPointCollection.Insert ---------------------------------------
procedure TContourPointCollection.Insert(dx, dy, dz, val: double;
  bIsNull: boolean; sLabel: string; aBookMark: TBookMark);

var
  iIndex, iPos: integer;
  sGoodLabel, sName: string;
  v: TVector;

begin
  { ** assumes we only add all at once }
  iPos := PointList.Count;

  { ** get a safe name for the data point }
  sGoodLabel := GetSafeName(sLabel);

  sName := PointType + '_' + sGoodLabel + '_' + IntToStr(iPos);
  { ** to ensure no duplicates }
  while (PointList.IndexOf(sName) <> -1) do
    sName := PointType + '_' + sGoodLabel + '_' + IntToStr(iPos + 1);

  PointList.AddObject(sName, TContourPoint.Create(self));
  iIndex := PointList.IndexOf(sName);

  with TContourPoint(PointList.Objects[iIndex]) do
  begin
    Point.Name := sName;
    Point.MasterObject := Master;
    Point.Up := Master.Up;
    Point.Direction := Master.Direction;
    Point.ProxyOptions := [pooObjects];
    Bookmark := aBookMark;
    PointLabel := sLabel;
    X := dx;
    Y := dy;
    Z := dz;
    Value := val;
    IsNull := bIsNull;
    SetVector(v, X * fScaleX, Y * fScaleY, Z * fScaleZ);
    DisplayPoint(v);
  end;
end;

// =============================================================================
// TVECTORFIELD Implementation
// =============================================================================
// ----- TVectorField.GetMaxMag ------------------------------------------------
function TVectorField.GetMaxMag: double;

var
  i: integer;

begin
  // sweep through and determine magnitudes
  for i := 0 to fPointList.Count - 1 do
  begin
    with TVectorPoint(fPointList.Objects[i]) do
    begin
      if i = 0 then
        result := Magnitude
      else
      begin
        if (result < Magnitude) then
          result := Magnitude;
      end;
    end;
  end;
end;

// ----- TVectorField.GetMinMag ------------------------------------------------
function TVectorField.GetMinMag: double;

var
  i: integer;

begin
  if ZeroMin then
    result := 0.0
  else
  begin
    // sweep through and determine magnitudes
    for i := 0 to fPointList.Count - 1 do
    begin
      with TVectorPoint(fPointList.Objects[i]) do
      begin
        if i = 0 then
          result := Magnitude
        else
        begin
          if (result > Magnitude) then
            result := Magnitude;
        end;
      end;
    end;
  end;
end;

// ----- TVectorField.SetScaleX ------------------------------------------------
procedure TVectorField.SetScaleX(iScale: single);

var
  i: integer;

begin
  fScaleX := iScale;
  // don't scale point but rather the position
  for i := 0 to fPointList.Count - 1 do
  begin
    with TVectorPoint(fPointList.Objects[i]) do
      Arrow.position.X := (iScale * fX);
  end;
end;

// ----- TVectorField.SetScaleY ------------------------------------------------
procedure TVectorField.SetScaleY(iScale: single);

var
  i: integer;

begin
  fScaleY := iScale;
  // don't scale point but rather the position
  for i := 0 to fPointList.Count - 1 do
  begin
    with TVectorPoint(fPointList.Objects[i]) do
      Arrow.position.Y := (iScale * fY);
  end;
end;

// ----- TVectorField.SetScaleZ ------------------------------------------------
procedure TVectorField.SetScaleZ(iScale: single);

var
  i: integer;

begin
  fScaleZ := iScale;
  // don't scale point but rather the position
  for i := 0 to fPointList.Count - 1 do
  begin
    with TVectorPoint(fPointList.Objects[i]) do
      Arrow.position.Z := (iScale * fZ);
  end;
end;

// ----- TVectorField.SetSlices ------------------------------------------------
procedure TVectorField.SetSlices(iSlices: integer);

var
  i: integer;

begin
  fSlices := iSlices;
  for i := 0 to fPointList.Count - 1 do
    with TVectorPoint(fPointList.Objects[i]) do
      Arrow.Slices := iSlices;
end;

// ----- TVectorField.SetStacks ------------------------------------------------
procedure TVectorField.SetStacks(iStacks: integer);

var
  i: integer;

begin
  fStacks := iStacks;
  for i := 0 to fPointList.Count - 1 do
    with TVectorPoint(fPointList.Objects[i]) do
      Arrow.Stacks := iStacks;
end;

// ------ TVectorField.Create --------------------------------------------------
constructor TVectorField.Create(aDummyCube: TGLDummyCube);

begin
  inherited Create;
  fDummyCube := aDummyCube;
  fMaxArrowLength := 1.0;
  fMinArrowLength := 0.0;
  fMaxArrowHeadHeight := 0.5;
  fMinArrowHeadHeight := 0.0;
  fMaxArrowHeadRadius := 0.2;
  fMinArrowHeadRadius := 0.0;
  fMaxArrowRadius := 0.1;
  fMinArrowRadius := 0.0;
  fZeroMin := false;
  fScaleX := 1.0;
  fScaleY := 1.0;
  fScaleZ := 1.0;
  fSlices := 4;
  fStacks := 1;
  fPointList := TStringList.Create;
end;

// ------ TVectorField.Destroy -------------------------------------------------
destructor TVectorField.Destroy;

begin
  fPointList.Free;
  inherited Destroy;
end;

// ------ TVectorField.ClearAll ------------------------------------------------
procedure TVectorField.ClearAll;

begin
  while fPointList.Count > 0 do
  begin
    fPointList.Objects[0].Free;
    fPointList.Delete(0);
  end
end;

// ------ TVectorField.CalcBounds -----------------------------------
procedure TVectorField.CalcBounds(var dleft, dright, dBack, dForward, dBottom,
  dtop: double);

var
  i: integer;

begin
  dleft := 1E30;
  dright := -1E30;
  dBack := 1E30;
  dForward := -1E30;
  dtop := -1E30;
  dBottom := 1E30;

  for i := 0 to PointList.Count - 1 do
  begin
    with TVectorPoint(PointList.Objects[i]) do
    begin
      if X < dleft then
        dleft := X;
      if X > dright then
        dright := X;
      if Y < dBack then
        dBack := Y;
      if Y > dForward then
        dForward := Y;
      if Z < dBottom then
        dBottom := Z;
      if Z > dtop then
        dtop := Z;
    end;
  end;
end;

// ----- TVectorField.Insert ---------------------------------------------------
procedure TVectorField.Insert(xp, yp, zp, mag, ux, uy, uz: double;
  sLabel: string; aBookMark: TBookMark);

var
  iIndex, iPos: integer;
  sGoodLabel, sName: string;
  pos, direct: TVector;

begin
  iPos := fPointList.Count;

  sGoodLabel := GetSafeName(sLabel);

  sName := 'vect' + sGoodLabel + '_' + IntToStr(iPos);
  { ** to ensure no duplicates }
  while (fPointList.IndexOf(sName) <> -1) do
    sName := 'vect' + sGoodLabel + '_' + IntToStr(iPos + 1);

  fPointList.AddObject(sName, TVectorPoint.Create(self));
  iIndex := fPointList.IndexOf(sName);

  with TVectorPoint(fPointList.Objects[iIndex]) do
  begin
    X := xp;
    Y := yp;
    Z := zp;
    Magnitude := mag;
    Arrow.Visible := false;
    Arrow.Name := sName;
    SetVector(direct, ux, uy, uz);
    Arrow.Direction.AsVector := direct;
    Bookmark := aBookMark;
    SetVector(pos, fScaleX * xp, fScaleY * yp, fScaleZ * zp);
    Arrow.position.AsVector := pos;
  end;
end;

// ----- TVectorField.RenderField ----------------------------------------------
procedure TVectorField.RenderField;

var
  i: integer;
  dRatio: double;
  col: TColorVector;

begin
  // should have an option to set this manually:
  ColPalette.MinValue := MinMag;
  ColPalette.MaxValue := MaxMag;

  for i := 0 to fPointList.Count - 1 do
  begin
    with TVectorPoint(fPointList.Objects[i]) do
    begin
      dRatio := (Magnitude - ColPalette.MinValue) /
        (ColPalette.MaxValue - ColPalette.MinValue);
      case ColPalette.SpectrumMode of
        // single colour
        0:
          col := ColPalette.GetColourVector(Magnitude, true);
        // simple minimum/maximum
        1:
          col := ColPalette.GetColourVector(Magnitude, true);
        // rainbow and inverse rainbow spectrum
        2, 3:
          col := ColPalette.GetColourVector(Magnitude,
            (ColPalette.SpectrumMode = 2));
        // using a palette
        4, 5:
          col := ColPalette.GetColourVector(Magnitude,
            (ColPalette.SpectrumMode = 4))
      end;
      // range from Minimum properties to maximum properties
      Arrow.Material.FrontProperties.Emission.Color := col;
      Arrow.Height := MinArrowLength + dRatio *
        (MaxArrowLength - MinArrowLength);
      Arrow.TopRadius := MinArrowRadius + dRatio *
        (MaxArrowRadius - MinArrowRadius);
      Arrow.BottomRadius := Arrow.TopRadius;
      Arrow.TopArrowHeadHeight := MinArrowHeadHeight + dRatio *
        (MaxArrowHeadHeight - MinArrowHeadHeight);
      Arrow.TopArrowHeadRadius := MinArrowHeadRadius + dRatio *
        (MaxArrowHeadRadius - MinArrowHeadRadius);
      Arrow.Slices := fSlices;
      Arrow.Stacks := fStacks;
      Arrow.Visible := true;
    end;
  end;
end;

// =============================================================================
end.
