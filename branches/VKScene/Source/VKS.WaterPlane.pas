//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  A plane simulating animated water
  The Original Code is part of Cosmos4D
  http://users.hol.gr/~sternas/
  Sternas Stefanos 2003
}
unit VKS.WaterPlane;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  FMX.Types,
  FMX.Graphics,

  OpenGLAdapter,
  VKS.VectorGeometry,
  VKS.Scene,
  VKS.VectorLists,
  VKS.CrossPlatform,
  VKS.PersistentClasses,
  VKS.BaseClasses,
  VKS.Context,
  VKS.RenderContextInfo,
  VKS.VectorTypes;

type

  TVKWaterPlaneOption = (wpoTextured);
  TVKWaterPlaneOptions = set of TVKWaterPlaneOption;

const
  cDefaultWaterPlaneOptions = [wpoTextured];

type

  TVKWaterPlane = class(TVKSceneObject)
  private

    FLocks: packed array of ByteBool;
    FPositions, FVelocity: packed array of Single;
    FPlaneQuadIndices: TPersistentObjectList;
    FPlaneQuadTexCoords: TTexPointList;
    FPlaneQuadVertices: TAffineVectorList;
    FPlaneQuadNormals: TAffineVectorList;
    FActive: Boolean;
    FRainTimeInterval: Integer;
    FRainForce: Single;
    FViscosity: Single;
    FElastic: Single;
    FResolution: Integer;
    FSimulationFrequency, FTimeToNextUpdate: Single;
    FTimeToNextRainDrop: Single;
    FMaximumCatchupIterations: Integer;
    FLastIterationStepTime: Single;
    FMask: TVKPicture;
    FOptions: TVKWaterPlaneOptions;
  protected
    procedure SetElastic(const value: Single);
    procedure SetResolution(const value: Integer);
    procedure SetRainTimeInterval(const val: Integer);
    procedure SetViscosity(const val: Single);
    procedure SetRainForce(const val: Single);
    procedure SetSimulationFrequency(const val: Single);
    procedure SetMask(val: TVKPicture);
    procedure SetOptions(const val: TVKWaterPlaneOptions);

    procedure DoMaskChanged(Sender: TObject);
    procedure InitResolution;

    procedure IterComputeVelocity;
    procedure IterComputePositions;
    procedure IterComputeNormals;
    procedure Iterate;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;

    procedure CreateRippleAtGridPos(X, Y: Integer);
    procedure CreateRippleAtWorldPos(const X, Y, z: Single); overload;
    procedure CreateRippleAtWorldPos(const pos: TVector); overload;
    procedure CreateRippleRandom;
    procedure Reset;

    { CPU time (in seconds) taken by the last iteration step. }
    property LastIterationStepTime: Single read FLastIterationStepTime;

  published

    property Active: Boolean read FActive write FActive default True;

    { Delay between raindrops in milliseconds (0 = no rain) }
    property RainTimeInterval: Integer read FRainTimeInterval
      write SetRainTimeInterval default 500;
    property RainForce: Single read FRainForce write SetRainForce;

    property Viscosity: Single read FViscosity write SetViscosity;
    property Elastic: Single read FElastic write SetElastic;
    property Resolution: Integer read FResolution write SetResolution
      default 64;
    property Options: TVKWaterPlaneOptions read FOptions write SetOptions
      default cDefaultWaterPlaneOptions;

    { A picture whose pixels determine what part of the waterplane is active.
      Pixels with a green/gray component beyond 128 are active, the others
      are not (in short, white = active, black = inactive).
      The picture will automatically be stretched to match the resolution. }
    property Mask: TVKPicture read FMask write SetMask;

    { Maximum frequency (in Hz) at which simulation iterations happen. }
    property SimulationFrequency: Single read FSimulationFrequency
      write SetSimulationFrequency;
    { Maximum number of simulation iterations during catchups.
      Catchups happen when for a reason or another, the DoProgress doesn't
      happen as fast SimulationFrequency. }
    property MaximumCatchupIterations: Integer read FMaximumCatchupIterations
      write FMaximumCatchupIterations default 1;
  end;

  // -------------------------------------------------------------
  // -------------------------------------------------------------
  // -------------------------------------------------------------
implementation

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

// Create
//
constructor TVKWaterPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];

  FElastic := 10;
  FActive := True;
  FRainTimeInterval := 500;
  FRainForce := 5000;
  FViscosity := 0.99;
  FSimulationFrequency := 100; // 100 Hz
  FMaximumCatchupIterations := 1;
  FOptions := cDefaultWaterPlaneOptions;

  FPlaneQuadIndices := TPersistentObjectList.Create;
  FPlaneQuadTexCoords := TTexPointList.Create;
  FPlaneQuadVertices := TAffineVectorList.Create;
  FPlaneQuadNormals := TAffineVectorList.Create;
  FMask := TVKPicture.Create(AOwner);
  FMask.Bitmap.OnChange := DoMaskChanged;

  SetResolution(64);
end;

// Destroy
//
destructor TVKWaterPlane.Destroy;
begin
  FMask.Free;
  FPlaneQuadNormals.Free;
  FPlaneQuadVertices.Free;
  FPlaneQuadTexCoords.Free;
  FPlaneQuadIndices.CleanFree;
  inherited;
end;

// DoProgress
//
procedure TVKWaterPlane.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  inherited;
  if Active and Visible then
  begin
    // new raindrops
    if FRainTimeInterval > 0 then
    begin
      FTimeToNextRainDrop := FTimeToNextRainDrop - progressTime.deltaTime;
      i := FMaximumCatchupIterations;
      while FTimeToNextRainDrop <= 0 do
      begin
        CreateRippleRandom;
        FTimeToNextRainDrop := FTimeToNextRainDrop + FRainTimeInterval * 0.001;
        Dec(i);
        if i < 0 then
        begin
          if FTimeToNextRainDrop < 0 then
            FTimeToNextRainDrop := FRainTimeInterval * 0.001;
          Break;
        end;
      end;
    end;
    // iterate simulation
    FTimeToNextUpdate := FTimeToNextUpdate - progressTime.deltaTime;
    if FTimeToNextUpdate <= 0 then
    begin
      i := FMaximumCatchupIterations;
      while FTimeToNextUpdate <= 0 do
      begin
        Iterate;
        FTimeToNextUpdate := FTimeToNextUpdate + 1 / FSimulationFrequency;
        Dec(i);
        if i < 0 then
        begin
          if FTimeToNextUpdate < 0 then
            FTimeToNextUpdate := 1 / FSimulationFrequency;
          Break;
        end;
      end;
      StructureChanged;
    end;
  end;
end;

// CreateRippleAtGridPos
//
procedure TVKWaterPlane.CreateRippleAtGridPos(X, Y: Integer);
begin
  if (X > 0) and (Y > 0) and (X < Resolution - 1) and (Y < Resolution - 1) then
    FVelocity[X + Y * Resolution] := FRainForce;
end;

// CreateRippleAtWorldPos
//
procedure TVKWaterPlane.CreateRippleAtWorldPos(const X, Y, z: Single);
var
  vv: TVector;
begin
  vv := AbsoluteToLocal(PointMake(X, Y, z));
  CreateRippleAtGridPos(Round((vv.X + 0.5) * Resolution),
    Round((vv.z + 0.5) * Resolution));
end;

// CreateRippleAtWorldPos
//
procedure TVKWaterPlane.CreateRippleAtWorldPos(const pos: TVector);
var
  vv: TVector;
begin
  vv := AbsoluteToLocal(PointMake(pos));
  CreateRippleAtGridPos(Round((vv.X + 0.5) * Resolution),
    Round((vv.z + 0.5) * Resolution));
end;

// CreateRippleRandom
//
procedure TVKWaterPlane.CreateRippleRandom;
begin
  CreateRippleAtGridPos(Random(Resolution - 3) + 2, Random(Resolution - 3) + 2);
end;

// InitResolution
//
procedure TVKWaterPlane.InitResolution;
var
  i, j: Integer;
  v: TAffineVector;
  resSqr: Integer;
  invResol: Single;
begin
  resSqr := FResolution * FResolution;
  FPlaneQuadIndices.Capacity := resSqr * 2;
  FPlaneQuadTexCoords.Clear;
  FPlaneQuadTexCoords.Capacity := resSqr;
  FPlaneQuadVertices.Clear;
  FPlaneQuadVertices.Capacity := resSqr;

  invResol := 1 / Resolution;
  for j := 0 to Resolution - 1 do
  begin
    for i := 0 to Resolution - 1 do
    begin
      FPlaneQuadTexCoords.Add(i * invResol, j * invResol);
      FPlaneQuadVertices.Add((i - Resolution * 0.5) * invResol, 0,
        (j - Resolution * 0.5) * invResol);
    end;
  end;

  FPlaneQuadNormals.Count := resSqr;
  v.X := 0;
  v.Y := 2048;
  v.z := 0;
  for i := 0 to FPlaneQuadNormals.Count - 1 do
    FPlaneQuadNormals.List[i] := v;

  SetLength(FPositions, resSqr);
  SetLength(FVelocity, resSqr);
  SetLength(FLocks, resSqr);

  Reset;
  Iterate;

  StructureChanged;
end;

// Reset
//
procedure TVKWaterPlane.Reset;
var
  i, j, ij, resSqr: Integer;
  maskBmp: TBitmap;
  scanLine: PIntegerArray;
  il: TIntegerList;
  locked: Boolean;
begin
  resSqr := FResolution * FResolution;
  for i := 0 to resSqr - 1 do
  begin
    FPositions[i] := 0;
    FVelocity[i] := 0;
    FLocks[i] := False;
  end;
  if FMask.Width > 0 then
  begin
    maskBmp := TBitmap.Create;
    try
      { TODO : E2129 Cannot assign to a read-only property }
      (* maskBmp.PixelFormat:= TPixelFormat.RGBA32F; //in VCL glpf32bit; *)
      maskBmp.Width := Resolution;
      maskBmp.Height := Resolution;
      { TODO : E2003 Undeclared identifier: 'StretchDraw' }
      (* maskBmp.Canvas.StretchDraw(Rect(0, 0, Resolution, Resolution), FMask.Graphic); *)
      for j := 0 to Resolution - 1 do
      begin
        scanLine := BitmapScanLine(maskBmp, Resolution - 1 - j);
        // maskBmp.ScanLine[Resolution-1-j];
        for i := 0 to Resolution - 1 do
          FLocks[i + j * Resolution] := (((scanLine[i] shr 8) and $FF) < 128);
      end;
    finally
      maskBmp.Free;
    end;
  end;

  FPlaneQuadIndices.Clean;
  for j := 0 to Resolution - 2 do
  begin
    il := TIntegerList.Create;
    for i := 0 to Resolution - 1 do
    begin
      ij := i + j * Resolution;
      if (il.Count and 2) <> 0 then
        locked := False
      else
      begin
        locked := FLocks[ij] and FLocks[ij + Resolution];
        if locked and (i < Resolution - 1) then
          locked := FLocks[ij + 1] and FLocks[ij + Resolution + 1];
      end;
      if not locked then
        il.Add(ij, ij + Resolution)
      else if il.Count > 0 then
      begin
        FPlaneQuadIndices.Add(il);
        il := TIntegerList.Create;
      end;
    end;
    if il.Count > 0 then
      FPlaneQuadIndices.Add(il)
    else
      il.Free;
  end;
end;

// IterComputeVelocity
//
procedure TVKWaterPlane.IterComputeVelocity;
var
  i, j, ij: Integer;
  f1, f2: Single;
  posList, velList: PSingleArray;
  lockList: PByteArray;
begin
  f1 := 0.05;
  f2 := 0.01 * FElastic;

  posList := @FPositions[0];
  velList := @FVelocity[0];
  lockList := @FLocks[0];
  for i := 1 to Resolution - 2 do
  begin
    ij := i * Resolution;
    for j := 1 to Resolution - 2 do
    begin
      Inc(ij);
      if lockList[ij] <> 0 then
        continue;
      velList[ij] := velList[ij] + f2 *
        (posList[ij] - f1 * (4 * (posList[ij - 1] + posList[ij + 1] +
        posList[ij - Resolution] + posList[ij + Resolution]) +
        posList[ij - 1 - Resolution] + posList[ij + 1 - Resolution] +
        posList[ij - 1 + Resolution] + posList[ij + 1 + Resolution]));
    end;
  end;
end;

// IterComputePositions
//
procedure TVKWaterPlane.IterComputePositions;
const
  cVelocityIntegrationCoeff: Single = 0.02;
  cHeightFactor: Single = 1E-4;
var
  ij: Integer;
  f: Single;
  coeff: Single;
  posList, velList: PSingleArray;
  lockList: PByteArray;
begin
  // Calculate the new ripple positions and update vertex coordinates
  coeff := cVelocityIntegrationCoeff * Resolution;
  f := cHeightFactor / Resolution;
  posList := @FPositions[0];
  velList := @FVelocity[0];
  lockList := @FLocks[0];
  for ij := 0 to Resolution * Resolution - 1 do
  begin
    if lockList[ij] = 0 then
    begin
      posList[ij] := posList[ij] - coeff * velList[ij];
      velList[ij] := velList[ij] * FViscosity;
      FPlaneQuadVertices.List[ij].Y := posList[ij] * f;
    end;
  end;
end;

// IterComputeNormals
//
procedure TVKWaterPlane.IterComputeNormals;
var
  i, j, ij: Integer;
  pv: PAffineVector;
  posList: PSingleArray;
  normList: PAffineVectorArray;
begin
  // Calculate the new vertex normals (not normalized, the hardware will handle that)
  posList := @FPositions[0];
  normList := FPlaneQuadNormals.List;
  for i := 1 to Resolution - 2 do
  begin
    ij := i * Resolution;
    for j := 1 to Resolution - 2 do
    begin
      Inc(ij);
      pv := @normList[ij];
      pv.X := posList[ij + 1] - posList[ij - 1];
      pv.z := posList[ij + Resolution] - posList[ij - Resolution];
    end;
  end;
end;

// Iterate
//
procedure TVKWaterPlane.Iterate;
var
  t: Int64;
begin
  if Visible then
  begin
    t := StartPrecisionTimer;

    IterComputeVelocity;
    IterComputePositions;
    IterComputeNormals;

    FLastIterationStepTime := StopPrecisionTimer(t);
  end;
end;

// BuildList
//
procedure TVKWaterPlane.BuildList(var rci: TVKRenderContextInfo);
var
  i: Integer;
  il: TIntegerList;
begin
  glPushClientAttribDefaultEXT(GL_CLIENT_VERTEX_ARRAY_BIT);

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, FPlaneQuadVertices.List);
  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, 0, FPlaneQuadNormals.List);
  if wpoTextured in Options then
  begin
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, 0, FPlaneQuadTexCoords.List);
  end
  else
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  if GL_EXT_compiled_vertex_array then
    glLockArraysEXT(0, FPlaneQuadVertices.Count);

  for i := 0 to FPlaneQuadIndices.Count - 1 do
  begin
    il := TIntegerList(FPlaneQuadIndices[i]);
    glDrawElements(GL_QUAD_STRIP, il.Count, GL_UNSIGNED_INT, il.List);
  end;

  if GL_EXT_compiled_vertex_array then
    glUnlockArraysEXT;

  glPopClientAttrib;
end;

// Assign
//
procedure TVKWaterPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKWaterPlane) then
  begin
    Active := TVKWaterPlane(Source).Active;
    RainTimeInterval := TVKWaterPlane(Source).RainTimeInterval;
    RainForce := TVKWaterPlane(Source).RainForce;
    Viscosity := TVKWaterPlane(Source).Viscosity;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//
function TVKWaterPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(Resolution);
  Result.Y := 0;
  Result.z := 0.5 * Abs(FResolution);
end;

// SetElastic
//
procedure TVKWaterPlane.SetElastic(const value: Single);
begin
  FElastic := value;
end;

// SetResolution
//
procedure TVKWaterPlane.SetResolution(const value: Integer);
begin
  if value <> FResolution then
  begin
    FResolution := value;
    if FResolution < 16 then
      FResolution := 16;
    InitResolution;
  end;
end;

// SetRainTimeInterval
//
procedure TVKWaterPlane.SetRainTimeInterval(Const val: Integer);
begin
  if (val >= 0) and (val <= 1000000) then
    FRainTimeInterval := val;
end;

// SetViscosity
//
Procedure TVKWaterPlane.SetViscosity(const val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FViscosity := val;
end;

// SetRainForce
//
procedure TVKWaterPlane.SetRainForce(const val: Single);
begin
  if (val >= 0) and (val <= 1000000) then
    FRainForce := val;
end;

// SetSimulationFrequency
//
procedure TVKWaterPlane.SetSimulationFrequency(const val: Single);
begin
  if FSimulationFrequency <> val then
  begin
    FSimulationFrequency := val;
    if FSimulationFrequency < 1 then
      FSimulationFrequency := 1;
    FTimeToNextUpdate := 0;
  end;
end;

procedure TVKWaterPlane.SetMask(val: TVKPicture);
begin
  FMask.Assign(val);
end;

procedure TVKWaterPlane.DoMaskChanged(Sender: TObject);
begin
  Reset;
  StructureChanged;
end;

procedure TVKWaterPlane.SetOptions(const val: TVKWaterPlaneOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

RegisterClasses([TVKWaterPlane]);

end.
