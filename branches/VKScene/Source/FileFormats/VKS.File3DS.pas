//
// VKScene project based on GLScene library, http://glscene.sourceforge.net
//
{
  3DStudio 3DS vector file format implementation.
}
unit VKS.File3DS;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils, System.Math,
  //VKS
  VKS.Scene, VKS.Objects, VKS.VectorFileObjects, VKS.Texture, VKS.ApplicationFileIO,
  VKS.VectorGeometry, File3DS, Types3DS, VKS.OpenGLTokens, VKS.Context, VKS.PersistentClasses,
  VKS.Strings, VKS.File3DSSceneObjects, VKS.CrossPlatform, VKS.VectorTypes, VKS.VectorLists,
  VKS.RenderContextInfo, VKS.Material;

type

  EGLFile3DS = class(Exception);

  { TVKFile3DSAnimationData.
     A record that holds all the information that is used during 3ds animation. }
  TVKFile3DSAnimationData = packed record
    ModelMatrix: TMatrix;
    Color: TVector;            // Omni Light.
    TargetPos: TAffineVector;  // Spot Light.
    SpotLightCutOff: single;
    HotSpot: single;
    Roll: single;
  end;

  { TVKFile3DSAnimationKeys.

     An abstract class that describes how to interpolate animation keys. }
  TVKFile3DSAnimationKeys = class(TPersistentObject)
  private
    FNumKeys: integer;
    FKeys: array of TKeyHeader3DS;
    procedure InterpolateFrame(var I: integer; var w: real; const AFrame: real);
  protected
    function InterpolateValue(const AValues: array of single;
      const AFrame: real): single; overload;
    function InterpolateValue(const AValues: array of TAffineVector;
      const AFrame: real): TAffineVector; overload;
    function InterpolateValue(const AValues: array of TKFRotKey3DS;
      const AFrame: real): TMatrix; overload;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); virtual;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData; const AFrame: real);
      virtual; abstract;

    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSScaleAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FScale: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSRotationAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FRot: array of TKFRotKey3DS;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSPositionAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSColorAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FCol: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TTVKFile3DSPositionAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FTPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSSpotLightCutOffAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FFall: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSLightHotSpotAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FHot: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSRollAnimationKeys = class(TVKFile3DSAnimationKeys)
  private
    FRoll: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList;
      const AData: Pointer); override;
    procedure Apply(var DataTransf: TVKFile3DSAnimationData;
      const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TVKFile3DSAnimationKeyList = class(TPersistentObject)
  private
    FAnimKeysList: array of TVKFile3DSAnimationKeys;
  protected
    procedure ApplyAnimKeys(var DataTransf: TVKFile3DSAnimationData; const AFrame: real);
  public
    procedure AddKeys(const AItem: TVKFile3DSAnimationKeys);
    procedure ClearKeys;

    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  { Used only for serialization. There probably is a more efficient way to do it. }
  TVKFile3DSAnimKeysClassType = (ctScale, ctRot, ctPos, ctCol, ctTPos,
    ctFall, ctHot, ctRoll);

  { TVKFile3DSDummyObject. A 3ds-specific TMorphableMeshObject. }
  TVKFile3DSDummyObject = class(TMorphableMeshObject)
  private
    FAnimList: TVKFile3DSAnimationKeyList;
    FAnimData: Pointer;
    FRefTranf, FAnimTransf: TVKFile3DSAnimationData;
    FParent: TVKFile3DSDummyObject;
    FParentName: String64;
    FStatic : Boolean; // Static tag used in BuildList to not apply animation matrix
  public
    procedure LoadAnimation(const AData: Pointer); virtual;
    procedure SetFrame(const AFrame: real); virtual;

    procedure MorphTo(morphTargetIndex: integer); override;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: integer;
      lerpFactor: single); override;
    procedure GetExtents(out min, max: TAffineVector); override;
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList;
      override;

    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;

    property AnimList: TVKFile3DSAnimationKeyList read FAnimList;
    property Parent: TVKFile3DSDummyObject read FParent write FParent;
    property RefrenceTransf: TVKFile3DSAnimationData read FRefTranf write FRefTranf;
  end;

  { TVKFile3DSDummyObject. A 3ds-specific mesh object. }
  TVKFile3DSMeshObject = class(TVKFile3DSDummyObject)
  public
    procedure LoadAnimation(const AData: Pointer); override;
    procedure BuildList(var ARci: TRenderContextInfo); override;
  end;

  { TVKFile3DSDummyObject. A 3ds-specific omni light. }
  TVKFile3DSOmniLightObject = class(TVKFile3DSDummyObject)
  private
    FLightSrc: TVKFile3DSLight;
    FLightSrcName: String64;
  public
    constructor Create; override;
    procedure LoadData(const AOwner: TVKBaseMesh; const AData: PLight3DS); virtual;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  { TVKFile3DSSpotLightObject. A 3ds-specific spot light. }
  TVKFile3DSSpotLightObject = class(TVKFile3DSOmniLightObject)
  public
    procedure LoadData(const AOwner: TVKBaseMesh; const AData: PLight3DS); override;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
  end;

  { TVKFile3DSCameraObject. A 3ds-specific camera. }
  TVKFile3DSCameraObject = class(TVKFile3DSDummyObject)
  private
    FTargetObj: TVKDummyCube;
    FCameraSrc: TVKFile3DSCamera;
    FCameraSrcName: String64;
  public
    constructor Create; override;
    procedure LoadData(Owner: TVKBaseMesh; AData: PCamera3DS);
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  // TVK3DSVectorFile

  { The 3DStudio vector file. 
     Uses an upgraded version if a 3DS import library by Mike Lischke. 
     (http://www.lishcke-online.de). A 3DS file may contain material
     information and require textures when loading. }
  TVK3DSVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  { If enabled, advanced parameters will be loaded from a 3ds file
     (TextureScale, TextureOffset), but it might break backwards compatibility.
     If disabled, it won't break anything, but some parameters will not be
     loaded correctly from a 3ds file.
     Also there is a significant drop in FPS when this option is on
     (for unknown reasons), so it is off by default. }
  vGLFile3DS_UseTextureEx: boolean = False;

  { If enabled, allows 3ds animation and fixes loading of some 3ds models,
     but has a few bugs:
     - TVKFreeForm.AutoCentering does now work correctly.
     - TMeshObject.vertices return values different from
        TMeshObject.ExtractTriangles()
     }
  vGLFile3DS_EnableAnimation: boolean = False;

  { If enabled, a -90 degrees (-PI/2) rotation will occured on X Axis.
     By design 3dsmax has a Z Up-Axis, after the rotation the Up axis will
     be Y. (Note: you need vGLFile3DS_EnableAnimation = true)
  }
  vGLFile3DS_FixDefaultUpAxisY: boolean = False;


  { If >= 0, then the vertices list will be updated with selected frame
     animation data. (Note: you need vGLFile3DS_EnableAnimation = true).
     Be aware that in that case animation will not be usable, it is made
     to be used with a static mesh like GLFreeForm.
  }
  vGLFile3DS_LoadedStaticFrame: integer = -1;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
const
  cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE = PI/2;
  CGLFILE3DS_DEFAULT_FRAME = 0;

resourcestring
  gls3DSMapNotFound = 'Loading %s map texture failed: %s in %s';

{$IFDEF VKS_REGIONS}{$REGION 'Misc functions'}{$ENDIF}

// AnimKeysClassTypeToClass

function AnimKeysClassTypeToClass(
  const AAnimKeysClassType: TVKFile3DSAnimKeysClassType): TClass;
begin
  case AAnimKeysClassType of
    ctScale: Result := TVKFile3DSScaleAnimationKeys;
    ctRot: Result := TVKFile3DSRotationAnimationKeys;
    ctPos: Result := TVKFile3DSPositionAnimationKeys;
    ctCol: Result := TVKFile3DSColorAnimationKeys;
    ctTPos: Result := TTVKFile3DSPositionAnimationKeys;
    ctFall: Result := TVKFile3DSSpotLightCutOffAnimationKeys;
    ctHot: Result := TVKFile3DSLightHotSpotAnimationKeys;
    ctRoll: Result := TVKFile3DSRollAnimationKeys;
    else
    begin
      Result := nil;
      Assert(False, vksErrorEx + vksUnknownType);
    end;
  end;
end;


// ClassToAnimKeysClassType

function ClassToAnimKeysClassType(
  const AAnimKeysClass: TClass): TVKFile3DSAnimKeysClassType;
begin
  if AAnimKeysClass.InheritsFrom(TVKFile3DSScaleAnimationKeys) then
    Result := ctScale
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSRotationAnimationKeys) then
    Result := ctRot
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSPositionAnimationKeys) then
    Result := ctPos
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSColorAnimationKeys) then
    Result := ctCol
  else if AAnimKeysClass.InheritsFrom(TTVKFile3DSPositionAnimationKeys) then
    Result := ctTPos
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSSpotLightCutOffAnimationKeys) then
    Result := ctFall
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSLightHotSpotAnimationKeys) then
    Result := ctHot
  else if AAnimKeysClass.InheritsFrom(TVKFile3DSRollAnimationKeys) then
    Result := ctRoll
  else
  begin
    Result := ctScale;
    Assert(False, vksErrorEx + vksUnknownType);
  end;
end;

// MakeRotationQuaternion

function MakeRotationQuaternion(const axis: TAffineVector; angle: single): TQuaternion;
var
  v: Tvector;
  halfAngle, invAxisLengthMult: single;
begin
  halfAngle := (angle) / 2;
  invAxisLengthMult := 1 / VectorLength(axis) * sin(halfAngle);

  v.V[0] := axis.V[0] * invAxisLengthMult;
  v.V[1] := axis.V[1] * invAxisLengthMult;
  v.V[2] := axis.V[2] * invAxisLengthMult;
  v.V[3] := cos(halfAngle);

  Result.ImagPart := AffineVectorMake(v);
  Result.RealPart := v.V[3];
end;

// QuaternionToRotateMatrix

function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TMatrix;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: single;
  quat: TVector;
  m: TMatrix;
begin
  quat := VectorMake(Quaternion.ImagPart);
  quat.V[3] := Quaternion.RealPart;

  x2 := quat.V[0] + quat.V[0];
  y2 := quat.V[1] + quat.V[1];
  z2 := quat.V[2] + quat.V[2];
  xx := quat.V[0] * x2;
  xy := quat.V[0] * y2;
  xz := quat.V[0] * z2;
  yy := quat.V[1] * y2;
  yz := quat.V[1] * z2;
  zz := quat.V[2] * z2;
  wx := quat.V[3] * x2;
  wy := quat.V[3] * y2;
  wz := quat.V[3] * z2;

  m.V[0].V[0] := 1.0 - (yy + zz);
  m.V[0].V[1] := xy - wz;
  m.V[0].V[2] := xz + wy;
  m.V[1].V[0] := xy + wz;
  m.V[1].V[1] := 1.0 - (xx + zz);
  m.V[1].V[2] := yz - wx;
  m.V[2].V[0] := xz - wy;
  m.V[2].V[1] := yz + wx;
  m.V[2].V[2] := 1.0 - (xx + yy);

  m.V[0].V[3] := 0;
  m.V[1].V[3] := 0;
  m.V[2].V[3] := 0;
  m.V[3].V[0] := 0;
  m.V[3].V[1] := 0;
  m.V[3].V[2] := 0;
  m.V[3].V[3] := 1;

  Result := m;
end;

// ------------------
// ------------------ Support classes ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSAnimationKeys'}{$ENDIF}

procedure TVKFile3DSAnimationKeys.InterpolateFrame(var I: integer;
  var w: real; const AFrame: real);
begin
  w := 1;
  I := 0;
  if FNumKeys > 1 then
  begin
    while (FNumKeys > I) and ((FKeys[I].Time) <= AFrame) do
      Inc(I);
    if (FNumKeys > I) and ((FKeys[I - 1].Time) <= AFrame) then
      w := (AFrame - FKeys[I - 1].Time) / (FKeys[I].Time - FKeys[I - 1].Time);

    // Don't allow keys to go our of range.
    if I = FNumKeys then
      I := FNumKeys - 1;
  end;
end;

function TVKFile3DSAnimationKeys.InterpolateValue(const AValues: array of single;
  const AFrame: real): single;
var
  I: integer;
  w: real;
  start, stop: single;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := 0;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := 0;

  Result := Lerp(start, stop, w);
end;

function TVKFile3DSAnimationKeys.InterpolateValue(const AValues: array of TAffineVector;
  const AFrame: real): TAffineVector;
var
  I: integer;
  w: real;
  start, stop: TAffineVector;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := NullVector;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := NullVector;

  Result := VectorLerp(start, stop, w);
end;

function TVKFile3DSAnimationKeys.InterpolateValue(const AValues: array of TKFRotKey3DS;
  const AFrame: real): TMatrix;
var
  I: integer;
  w: real;
begin
  Result := IdentityHmgMatrix;

  // First find the final matrix for this frame.
  I := 0;
  while (FNumKeys > I) and ((FKeys[I].Time) <= AFrame) do
  begin
    with AValues[I] do
      Result := MatrixMultiply(Result, CreateRotationMatrix(
        AffineVectorMake(X, Y, Z), Angle));
    Inc(I);
  end;

  InterpolateFrame(I, w, AFrame);

  // Then interpolate this matrix
  if (FNumKeys > I) and ((FKeys[I].Time) > AFrame) and
    ((FKeys[I - 1].Time) < AFrame) then
  begin
    with AValues[I] do
    begin
      Result := MatrixMultiply(Result, CreateRotationMatrix(
        AffineVectorMake(X, Y, Z), AngleLerp(0, Angle, w)));
    end;
  end;
end;

procedure TVKFile3DSAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  FNumKeys := ANumKeys;
  SetLength(FKeys, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FKeys[I] := Keys[I];
end;

procedure TVKFile3DSAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TVKFile3DSAnimationKeys then
  begin
    FNumKeys := TVKFile3DSAnimationKeys(Source).FNumKeys;
    SetLength(FKeys, FNumKeys);
    for I := 0 to FNumKeys - 1 do
      FKeys[I] := TVKFile3DSAnimationKeys(Source).FKeys[I];
  end
  else
    inherited Assign(Source);
end;

procedure TVKFile3DSAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  Writer.WriteInteger(FNumKeys);
  if FNumKeys > 0 then
    Writer.Write(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;

procedure TVKFile3DSAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  FNumKeys := Reader.ReadInteger;
  SetLength(FKeys, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSScaleAnimationKeys'}{$ENDIF}

procedure TVKFile3DSScaleAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
  AffVect : TAffineVector;
  Sign : ShortInt;
begin
  inherited;
  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FScale[I] := TAffineVector(PPointList(AData)[I]);

    if vGLFile3DS_FixDefaultUpAxisY then
    begin
      AffVect := FScale[I];

      if (AffVect.V[0] < 0) or (AffVect.V[1] < 0) or (AffVect.V[2] < 0) then
        Sign := -1
      else
        Sign:= 1;

      AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

      FScale[I].V[0] := Sign * Abs(AffVect.V[0]);
      FScale[I].V[1] := Sign * Abs(AffVect.V[1]);
      FScale[I].V[2] := Sign * Abs(AffVect.V[2]);
    end;

  end;
end;

procedure TVKFile3DSScaleAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix,
      CreateScaleMatrix(InterpolateValue(FScale, AFrame)));


end;

procedure TVKFile3DSScaleAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FScale[I] := (Source as TVKFile3DSScaleAnimationKeys).Fscale[I];
end;

procedure TVKFile3DSScaleAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TVKFile3DSScaleAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FScale, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSRotationAnimationKeys'}{$ENDIF}

procedure TVKFile3DSRotationAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
  Rot: PKFRotKeyList;
  AffVect : TAffineVector;
begin
  inherited;

  SetLength(FRot, FNumKeys);
  Rot := PKFRotKeyList(AData);
  for I := 0 to FNumKeys - 1 do
  begin
    // The initial values do not contain any turns, that's why we have to make one.
    if (Rot[I].X = 0) and (Rot[I].Y = 0) and (Rot[I].Z = 0) then
      Rot[I].X := 1;

    // One quartalion can't describe a big angle (>180), that's why we have to subtract it from 2*pi
    if Rot[I].Angle > pi then
    begin
      Rot[I].Angle := 2 * pi - Rot[I].Angle;
      Rot[I].X := -Rot[I].X;
      Rot[I].Y := -Rot[I].Y;
      Rot[I].Z := -Rot[I].Z;
    end;
    FRot[I] := Rot[I];

    if vGLFile3DS_FixDefaultUpAxisY then
    begin
      AffVect.V[0] := FRot[I].X;
      AffVect.V[1] := FRot[I].Y;
      AffVect.V[2] := FRot[I].Z;

      AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

      FRot[I].X := AffVect.V[0];
      FRot[I].Y := AffVect.V[1];
      FRot[I].Z := AffVect.V[2];
    end;


  end;
end;


procedure TVKFile3DSRotationAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix,
      InterpolateValue(FRot, AFrame));
end;

procedure TVKFile3DSRotationAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FRot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRot[I] := (Source as TVKFile3DSRotationAnimationKeys).FRot[I];
end;

procedure TVKFile3DSRotationAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;

procedure TVKFile3DSRotationAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FRot, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSPositionAnimationKeys'}{$ENDIF}

procedure TVKFile3DSPositionAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FPos[I] := TAffineVector(PPointList(AData)[I]);

    if vGLFile3DS_FixDefaultUpAxisY then
    begin
      FPos[I] := VectorRotateAroundX(FPos[I], cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
    end;
  end;
end;

procedure TVKFile3DSPositionAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix.V[3] :=
      VectorAdd(DataTransf.ModelMatrix.V[3], VectorMake(InterpolateValue(FPos, AFrame)));
end;

procedure TVKFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FPos[I] := (Source as TVKFile3DSPositionAnimationKeys).FPos[I];
end;

procedure TVKFile3DSPositionAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TVKFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSColorAnimationKeys'}{$ENDIF}

procedure TVKFile3DSColorAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;

  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := TaffineVector(PFColorList(AData)[I]);
end;

procedure TVKFile3DSColorAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.Color := VectorAdd(DataTransf.Color,
      VectorMake(InterpolateValue(FCol, AFrame)));
end;

procedure TVKFile3DSColorAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := (Source as TVKFile3DSColorAnimationKeys).FCol[I];
end;

procedure TVKFile3DSColorAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;

procedure TVKFile3DSColorAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FCol, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TTVKFile3DSPositionAnimationKeys'}{$ENDIF}

procedure TTVKFile3DSPositionAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FTPos[I] := TaffineVector(PPointList(AData)[I]);

    if vGLFile3DS_FixDefaultUpAxisY then
    begin
      FTPos[I] := VectorRotateAroundX(FTPos[I], cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
    end;
  end;
end;

procedure TTVKFile3DSPositionAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.TargetPos := VectorAdd(DataTransf.TargetPos,
      InterpolateValue(FTPos, AFrame));
end;

procedure TTVKFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FTPos[I] := (Source as TTVKFile3DSPositionAnimationKeys).FTPos[I];
end;

procedure TTVKFile3DSPositionAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TTVKFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSSpotLightCutOffAnimationKeys'}{$ENDIF}

procedure TVKFile3DSSpotLightCutOffAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;

  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := PSingleList(AData)[I];
end;

procedure TVKFile3DSSpotLightCutOffAnimationKeys.Apply(
  var DataTransf: TVKFile3DSAnimationData; const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.SpotLightCutOff :=
      DataTransf.SpotLightCutOff + InterpolateValue(FFall, AFrame);
end;

procedure TVKFile3DSSpotLightCutOffAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := (Source as TVKFile3DSSpotLightCutOffAnimationKeys).FFall[I];
end;

procedure TVKFile3DSSpotLightCutOffAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FFall[0], FNumKeys * SizeOf(single));
end;

procedure TVKFile3DSSpotLightCutOffAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FFall, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FFall[0], FNumKeys * SizeOf(single));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSLightHotSpotAnimationKeys'}{$ENDIF}

procedure TVKFile3DSLightHotSpotAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;

  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := PSingleList(AData)[I];
end;

procedure TVKFile3DSLightHotSpotAnimationKeys.Apply(
  var DataTransf: TVKFile3DSAnimationData; const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.HotSpot := DataTransf.HotSpot + InterpolateValue(FHot, AFrame);
end;

procedure TVKFile3DSLightHotSpotAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := (Source as TVKFile3DSLightHotSpotAnimationKeys).FHot[I];
end;

procedure TVKFile3DSLightHotSpotAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FHot[0], FNumKeys * SizeOf(single));
end;

procedure TVKFile3DSLightHotSpotAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FHot, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FHot[0], FNumKeys * SizeOf(single));
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSRollAnimationKeys'}{$ENDIF}

procedure TVKFile3DSRollAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := PSingleList(AData)[I];
end;

procedure TVKFile3DSRollAnimationKeys.Apply(var DataTransf: TVKFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.Roll := DataTransf.Roll + InterpolateValue(FRoll, AFrame);
end;

procedure TVKFile3DSRollAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := (Source as TVKFile3DSRollAnimationKeys).FRoll[I];
end;

procedure TVKFile3DSRollAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FRoll[0], FNumKeys * SizeOf(single));
end;

procedure TVKFile3DSRollAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FRoll[0], FNumKeys * SizeOf(single));
end;

procedure TVKFile3DSAnimationKeyList.AddKeys(const AItem: TVKFile3DSAnimationKeys);
var
  ind: integer;
begin
  if AItem = nil then
    Exit;
  ind := Length(FAnimKeysList);
  SetLength(FAnimKeysList, ind + 1);
  FAnimKeysList[ind] := AItem;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSAnimationKeyList'}{$ENDIF}

procedure TVKFile3DSAnimationKeyList.ApplyAnimKeys(
  var DataTransf: TVKFile3DSAnimationData; const AFrame: real);
var
  I: integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Apply(DataTransf, AFrame);
end;

procedure TVKFile3DSAnimationKeyList.ClearKeys;
var
  I: integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Free;
  SetLength(FAnimKeysList, 0);
end;

procedure TVKFile3DSAnimationKeyList.Assign(Source: TPersistent);
var
  I: integer;
  item: TVKFile3DSAnimationKeys;
begin
  if Source is TVKFile3DSAnimationKeyList then
  begin
    ClearKeys;
    for I := 0 to Length(TVKFile3DSAnimationKeyList(Source).FAnimKeysList) - 1 do
    begin
      item := (TVKFile3DSAnimationKeyList(Source).FAnimKeysList[I].ClassType.Create as
        TVKFile3DSAnimationKeys);
      item.Assign(TVKFile3DSAnimationKeyList(Source).FAnimKeysList[I]);
      AddKeys(item);
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TVKFile3DSAnimationKeyList.WriteToFiler(Writer: TVirtualWriter);
var
  I: integer;
  Val: TVKFile3DSAnimKeysClassType;
begin
  Writer.WriteInteger(Length(FAnimKeysList));
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Val := ClassToAnimKeysClassType(FAnimKeysList[I].ClassType);
    Writer.Write(Val, SizeOf(Val));
    FAnimKeysList[I].WriteToFiler(Writer);
  end;
end;

procedure TVKFile3DSAnimationKeyList.ReadFromFiler(Reader: TVirtualReader);
var
  I, cnt: integer;
  Val: TVKFile3DSAnimKeysClassType;
begin
  ClearKeys;
  cnt := Reader.ReadInteger;
  SetLength(FAnimKeysList, cnt);
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Reader.Read(Val, SizeOf(Val));
    FAnimKeysList[I] := AnimKeysClassTypeToClass(Val).Create as TVKFile3DSAnimationKeys;
    FAnimKeysList[I].ReadFromFiler(Reader);
  end;
end;

destructor TVKFile3DSAnimationKeyList.Destroy;
begin
  ClearKeys;
  inherited Destroy;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSDummyObject'}{$ENDIF}

constructor TVKFile3DSDummyObject.Create;
begin
  inherited;
  FAnimList := TVKFile3DSAnimationKeyList.Create;
  FRefTranf.ModelMatrix := IdentityHmgMatrix;
  FAnimTransf.ModelMatrix := IdentityHmgMatrix;
  FStatic := False;
end;

procedure TVKFile3DSDummyObject.LoadAnimation(const AData: Pointer);
begin
  FAnimList.ClearKeys;
  FAnimData := AData;
end;

procedure TVKFile3DSDummyObject.SetFrame(const AFrame: real);
var
  p: TVKFile3DSDummyObject;
  lAnimationData: TVKFile3DSAnimationData;
begin
  if not vGLFile3DS_EnableAnimation then
    Exit;

  if (FParentName <> '') then
  begin
    FParent := Owner.FindMeshByName(string(FParentName)) as TVKFile3DSDummyObject;
    FParentName := '';
  end;
  lAnimationData := FRefTranf;
  p := self;
  while p <> nil do
  begin
    p.FAnimList.ApplyAnimKeys(lAnimationData, AFrame);
    p := p.FParent;
  end;
  FAnimTransf := lAnimationData;
end;

procedure TVKFile3DSDummyObject.MorphTo(morphTargetIndex: integer);
begin
  SetFrame(morphTargetIndex);
end;

procedure TVKFile3DSDummyObject.Lerp(morphTargetIndex1, morphTargetIndex2: integer;
  lerpFactor: single);
begin
  if (Owner.Owner is TVKActor) and ((Owner.Owner as TVKActor).AnimationMode in
    [aamBounceBackward, aamLoopBackward]) then
    SetFrame(morphTargetIndex1 - lerpFactor)
  else
    SetFrame(morphTargetIndex1 + lerpFactor);
end;

procedure TVKFile3DSDummyObject.Assign(Source: TPersistent);
begin
  inherited; // Assign all published properties here.
  if Source is TVKFile3DSDummyObject then
  begin
    FRefTranf := (Source as TVKFile3DSDummyObject).FRefTranf;
    FParent := (Source as TVKFile3DSDummyObject).FParent;
    FAnimList.Assign((Source as TVKFile3DSDummyObject).FAnimList);
    SetFrame(0);
  end;
end;

procedure TVKFile3DSDummyObject.GetExtents(out min, max: TAffineVector);
begin
  inherited GetExtents(min, max);
  if not FStatic then
  begin
    if not IsInfinite(min.V[0]) then
      min := VectorTransform(min, FAnimTransf.ModelMatrix);
    if not IsInfinite(max.V[0]) then
      max := VectorTransform(max, FAnimTransf.ModelMatrix);
  end;
end;

function TVKFile3DSDummyObject.ExtractTriangles(texCoords, normals: TAffineVectorList):
TAffineVectorList;
var
  I: integer;
begin
  Result := inherited ExtractTriangles(texCoords, normals);

  if not FStatic then
  begin
    if (Result.Count <> 0) and not MatrixEquals(FAnimTransf.ModelMatrix,
      IdentityHmgMatrix) then
      for I := 0 to Result.Count - 1 do
        Result[I] := VectorTransform(Result[I], FAnimTransf.ModelMatrix);
  end;
end;

procedure TVKFile3DSDummyObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string;
begin
  inherited;

  Writer.Write(FRefTranf, SizeOf(FRefTranf));
  if FParent <> nil then
    str := Copy(FParent.Name, 1, 32)
  else
    str := 'nil';
  Writer.WriteString(str);
  FAnimList.WriteToFiler(Writer);
end;

procedure TVKFile3DSDummyObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  Reader.Read(FRefTranf, SizeOf(FRefTranf));
  FParentName := String64(Copy(Reader.ReadString, 1, 64));
  if FParentName = 'nil' then
    FParentName := '';
  FAnimList.ReadFromFiler(Reader);
end;

destructor TVKFile3DSDummyObject.Destroy;
begin
  FAnimList.Free;

  inherited;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSMeshObject'}{$ENDIF}

procedure TVKFile3DSMeshObject.LoadAnimation(const AData: Pointer);
var
  aScale: TVKFile3DSScaleAnimationKeys;
  aRot: TVKFile3DSRotationAnimationKeys;
  aPos: TVKFile3DSPositionAnimationKeys;
  Mat : TMatrix;
  RotMat : TMatrix;
  AffVect : TAffineVector;
begin
  inherited;

  with PKFMesh3DS(AData)^, FAnimList do
  begin
    aScale := TVKFile3DSScaleAnimationKeys.Create;
    aScale.LoadData(NSKeys, SKeys, Scale);
    AddKeys(aScale);

    aRot := TVKFile3DSRotationAnimationKeys.Create;
    aRot.LoadData(NRKeys, RKeys, Rot);
    AddKeys(aRot);

    aPos := TVKFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    if ParentStr <> '' then
      FParent := TVKFile3DSDummyObject(Owner.FindMeshByName(string(ParentStr)));

    with FRefTranf do
    begin

      if vGLFile3DS_FixDefaultUpAxisY then
      begin
        RotMat := CreateRotationMatrixX(cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
        InvertMatrix(RotMat);

        Mat := ModelMatrix;
        ModelMatrix := MatrixMultiply(Mat, RotMat);

        AffVect.V[0] := Pivot.X;
        AffVect.V[1] := Pivot.Y;
        AffVect.V[2] := Pivot.Z;

        AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

        Pivot.X := AffVect.V[0];
        Pivot.Y := AffVect.V[1];
        Pivot.Z := AffVect.V[2];
      end;

      ModelMatrix.V[3].V[0] := ModelMatrix.V[3].V[0] - Pivot.X;
      ModelMatrix.V[3].V[1] := ModelMatrix.V[3].V[1] - Pivot.Y;
      ModelMatrix.V[3].V[2] := ModelMatrix.V[3].V[2] - Pivot.Z;

    end;
  end;

  if vGLFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vGLFile3DS_LoadedStaticFrame);
end;

procedure TVKFile3DSMeshObject.BuildList(var ARci: TRenderContextInfo);
begin
  GL.PushMatrix;
  if not FStatic then
    GL.MultMatrixf(@FAnimTransf.ModelMatrix);
  inherited;
  GL.PopMatrix;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSOmniLightObject'}{$ENDIF}

constructor TVKFile3DSOmniLightObject.Create;
begin
  inherited;
  FLightSrc := TVKFile3DSLight.Create(nil);
end;

procedure TVKFile3DSOmniLightObject.LoadData(const AOwner: TVKBaseMesh;
  const AData: PLight3DS);
begin
  FLightSrc.Parent := AOwner;
  FLightSrc.LightStyle := lsOmni;
  FLightSrc.Name := string(AData.NameStr);
  Name := string(AData.NameStr);
  FLightSrc.Position.SetPoint(PAffineVector(@AData.Pos)^);
  FLightSrc.Diffuse.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Specular.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Diffuse.Color := VectorScale(FLightSrc.Diffuse.Color, AData.Multiplier);
  //надо потестить
  FLightSrc.Shining := not AData.DLOff;
  FLightSrc.Multipler := AData.Multiplier;
  FLightSrc.ConstAttenuation := 1;
  FLightSrc.LinearAttenuation := 0;
  FLightSrc.QuadraticAttenuation := 0;
end;

procedure TVKFile3DSOmniLightObject.LoadAnimation(const AData: Pointer);
var
  aPos: TVKFile3DSPositionAnimationKeys;
  aCol: TVKFile3DSColorAnimationKeys;
begin
  inherited;

  with PKFOmni3DS(AData)^, FAnimList do
  begin
    aPos := TVKFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aCol := TVKFile3DSColorAnimationKeys.Create;
    aCol.LoadData(NCKeys, CKeys, Color);
    AddKeys(aCol);

    if Parent <> '' then
      FParent := TVKFile3DSDummyObject(Owner.FindMeshByName(string(Parent)));
  end;

  if vGLFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vGLFile3DS_LoadedStaticFrame);
end;

procedure TVKFile3DSOmniLightObject.SetFrame(const AFrame: real);
var
  obj: TComponent;
begin
  if FLightSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(string(FLightSrcName), True);
    if obj is TVKFile3DSLight then
    begin
      FLightSrc.Free;
      FLightSrc := obj as TVKFile3DSLight;
    end;
    FLightSrcName := '';
  end;

  inherited;
  FLightSrc.Position.SetPoint(FAnimTransf.ModelMatrix.V[3]);
  FLightSrc.Diffuse.Color := FAnimTransf.Color;
end;

procedure TVKFile3DSOmniLightObject.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TVKFile3DSOmniLightObject then
    FlightSrc.Assign((Source as TVKFile3DSOmniLightObject).FLightSrc);
end;

procedure TVKFile3DSOmniLightObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string;
begin
  inherited;

  if FLightSrc.Name = '' then
    str := 'nil'
  else
    str := Copy(FLightSrc.Name, 1, 64);
  Writer.WriteString(str);
end;

procedure TVKFile3DSOmniLightObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  FLightSrcName := String64(Copy(Reader.ReadString, 1, 64));
  if FLightSrcName = 'nil' then
    FLightSrcName := '';
end;

destructor TVKFile3DSOmniLightObject.Destroy;
begin
  FLightSrc.Free;

  inherited;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSSpotLightObject'}{$ENDIF}

procedure TVKFile3DSSpotLightObject.LoadData(const AOwner: TVKBaseMesh;
  const AData: PLight3DS);
begin
  inherited;

  FLightSrc.LightStyle := lsSpot;
  FLightSrc.SpotTargetPos.SetPoint(TAffineVector(AData.Spot.Target));
  FLightSrc.SpotCutOff := AData.Spot.FallOff / 2;
  FLightSrc.HotSpot := AData.Spot.Hotspot / 2;
end;

procedure TVKFile3DSSpotLightObject.LoadAnimation(const AData: Pointer);
var
  aTPos: TTVKFile3DSPositionAnimationKeys;
  aFall: TVKFile3DSSpotLightCutOffAnimationKeys;
  aHot: TVKFile3DSLightHotSpotAnimationKeys;
begin
  inherited;

  with PKFSpot3DS(AData)^, FAnimList do
  begin
    aTPos := TTVKFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);

    aFall := TVKFile3DSSpotLightCutOffAnimationKeys.Create;
    aFall.LoadData(NFKeys, FKeys, Fall);
    AddKeys(aFall);

    aHot := TVKFile3DSLightHotSpotAnimationKeys.Create;
    aHot.LoadData(NHKeys, HKeys, Hot);
    AddKeys(aHot);

    if Parent <> '' then
      FParent := TVKFile3DSDummyObject(Owner.FindMeshByName(string(Parent)));
  end;

  if vGLFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vGLFile3DS_LoadedStaticFrame);
end;

procedure TVKFile3DSSpotLightObject.SetFrame(const AFrame: real);
begin
  inherited;

  FLightSrc.SpotTargetPos.SetPoint(FAnimTransf.TargetPos);
  FLightSrc.SpotCutOff := FAnimTransf.SpotLightCutOff / 2;
  FLightSrc.HotSpot := FAnimTransf.HotSpot / 2;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKFile3DSCameraObject'}{$ENDIF}

constructor TVKFile3DSCameraObject.Create;
begin
  inherited;

  FCameraSrc := TVKFile3DSCamera.Create(nil);
  FTargetObj := TVKDummyCube.Create(nil);
  FCameraSrc.TargetObject := FTargetObj;
end;

procedure TVKFile3DSCameraObject.LoadData(Owner: TVKBaseMesh; AData: PCamera3DS);
begin
  FCameraSrc.Parent := Owner;
  FTargetObj.Parent := Owner;
  FCameraSrc.Name := string(AData.NameStr);
  Name := string(AData.NameStr);
  FCameraSrc.Position.AsAffineVector := TAffineVector(AData.Position);
  FTargetObj.Position.SetPoint(TAffineVector(AData.Target));
  FCameraSrc.RollAngle := AData.Roll;

  FCameraSrc.FocalLength := AData.FOV;
end;

procedure TVKFile3DSCameraObject.LoadAnimation(const AData: Pointer);
var
  aPos: TVKFile3DSPositionAnimationKeys;
  aRoll: TVKFile3DSRollAnimationKeys;
  aTPos: TTVKFile3DSPositionAnimationKeys;
begin
  inherited;

  with PKFCamera3DS(AData)^, FAnimList do
  begin
    aPos := TVKFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aRoll := TVKFile3DSRollAnimationKeys.Create;
    aRoll.LoadData(NRKeys, RKeys, Roll);
    AddKeys(aRoll);

    aTPos := TTVKFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);
  end;

  if vGLFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vGLFile3DS_LoadedStaticFrame);
end;

procedure TVKFile3DSCameraObject.SetFrame(const AFrame: real);
var
  obj: TComponent;
begin
  inherited;

  if FCameraSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(string(FCameraSrcName), True);
    if obj is TVKFile3DSCamera then
    begin
      FCameraSrc.Free;
      FCameraSrc := obj as TVKFile3DSCamera;
    end;
    FCameraSrcName := '';
  end;

  FCameraSrc.Position.SetPoint(FAnimTransf.ModelMatrix.V[3]);
  FCameraSrc.RollAngle := FAnimTransf.Roll;
  FTargetObj.Position.SetPoint(FAnimTransf.TargetPos);
end;

procedure TVKFile3DSCameraObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string;
begin
  inherited;

  if FCameraSrc.Name = '' then
    str := 'nil'
  else
    str := Copy(FCameraSrc.Name, 1, 64);
  Writer.WriteString(str);
end;

procedure TVKFile3DSCameraObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  FCameraSrcName := String64(Copy(Reader.ReadString, 1, 64));
  if FCameraSrcName = 'nil' then
    FCameraSrcName := '';
end;

destructor TVKFile3DSCameraObject.Destroy;
begin
  FCameraSrc.Free;
  FTargetObj.Free;
  inherited;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVK3DSVectorFile'}{$ENDIF}

// Capabilities

class function TVK3DSVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

// LoadFromStream

procedure TVK3DSVectorFile.LoadFromStream(aStream: TStream);
type
  TSmoothIndexEntry = array[0..31] of cardinal;
  PSmoothIndexArray = ^TSmoothIndexArray;
  TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
  Marker: PByteArray;
  CurrentVertexCount: integer;
  SmoothIndices: PSmoothIndexArray;
  mesh: TVKFile3DSMeshObject;
  hasLightmap: boolean;


  {$IFDEF VKS_REGIONS}{$REGION 'TVK3DSVectorFile.LoadFromStream Local functions'}{$ENDIF}
  //--------------- local functions -------------------------------------------

  function GetOrAllocateMaterial(materials: TMaterialList; const Name: string): string;
  var
    material: PMaterial3DS;
    specColor: TVector;
    matLib: TVKMaterialLibrary;
    libMat, SecondMaterial: TVKLibMaterial;
  begin
    material := Materials.MaterialByName[Name];
    Assert(Assigned(material));
    if GetOwner is TVKBaseMesh then
    begin
      matLib := TVKBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := Name;
        libMat := matLib.Materials.GetLibMaterialByName(Name);
        if not Assigned(libMat) then
        begin
          libMat := matLib.Materials.Add;
          libMat.Name := Name;

          with libMat.Material.FrontProperties do
          begin
            Ambient.Color := VectorMake(material.Ambient.R, material.Ambient.G,
              material.Ambient.B, 1);
            // Material transparency can be stored as a positive or negative value.
            Diffuse.Color := VectorMake(material.Diffuse.R, material.Diffuse.G,
              material.Diffuse.B, 1 - Abs(material.Transparency));
            specColor := VectorMake(material.Specular.R, material.Specular.G,
              material.Specular.B, 1);
            Specular.Color := VectorScale(specColor, material.ShinStrength);
            Shininess := MaxInteger(0, integer(round((material.Shininess) * 128)));
            if material.Transparency <> 0 then
              libMat.Material.BlendingMode := bmTransparency;
          end;
          if Trim(string(material.Texture.Map.NameStr)) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(string(material.Texture.Map.NameStr));
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  TextureIndex := 0;
                  with material.Texture.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  Image.LoadFromFile(string(material.Texture.Map.NameStr));
                  Disabled := False;
                  TextureMode := tmModulate;
                end

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(gls3DSMapNotFound,['diffuse', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;

          if Trim(string(material.Opacity.Map.NameStr)) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  libMat.Material.BlendingMode := bmTransparency;
                  Texture.ImageAlpha := tiaAlphaFromIntensity;
                  Texture.TextureMode := tmModulate;
                  Texture.Image.LoadFromFile(string(material.Opacity.Map.NameStr));
                  Texture.Disabled := False;
                  TextureIndex := 1;
                  with material.Opacity.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(string(
                    material.Opacity.Map.NameStr));
                  SecondMaterial.Material.Texture.Disabled := False;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := string(material.Opacity.Map.NameStr);
                  LibMat.Texture2Name := SecondMaterial.Name;
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(gls3DSMapNotFound,['opacity', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
          if Trim(string(material.Bump.Map.NameStr)) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(string(material.Bump.Map.NameStr));
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  // You need a hight map for this parameter (like in 3d Max).
                  // Texture.TextureFormat := tfNormalMap;
                  TextureIndex := 2;
                  with material.Bump.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(string(
                    material.Bump.Map.NameStr));
                  SecondMaterial.Material.Texture.Disabled := False;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := string(material.Opacity.Map.NameStr);
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(gls3DSMapNotFound,['bump', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

  function GetOrAllocateLightMap(materials: TMaterialList;
  const Name: string): integer;
  var
    material: PMaterial3DS;
    matLib: TVKMaterialLibrary;
    libMat: TVKLibMaterial;
  begin
    Result := -1;
    material := Materials.MaterialByName[Name];
    Assert(Assigned(material));
    if GetOwner is TVKBaseMesh then
    begin
      matLib := TVKBaseMesh(GetOwner).LightmapLibrary;
      if Assigned(matLib) then
      begin
        if Trim(string(material.IllumMap.Map.NameStr)) <> '' then
        begin
          libMat := matLib.Materials.GetLibMaterialByName(string(
            material.IllumMap.Map.NameStr));
          if not Assigned(libMat) then
          begin
            libMat := matLib.Materials.Add;
            libMat.Name := string(material.IllumMap.Map.NameStr);
            try
              with libMat.Material.Texture do
              begin
                Image.LoadFromFile(string(material.IllumMap.Map.NameStr));
                Disabled := False;
                TextureMode := tmModulate;
              end;
            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(gls3DSMapNotFound,['light', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
          end;
          Result := libmat.Index;
          hasLightMap := True;
        end;
      end;
    end;
  end;

  //----------------------------------------------------------------------

  function InvertMeshMatrix(Objects: TObjectList; const Name: string): TMatrix;
    // constructs a 4x4 matrix from 3x4 local mesh matrix given by Name and
    // inverts it so it can be used for the keyframer stuff
  var
    I, Index: integer;
    boolY: boolean;
    m: TMatrix;
    v4: TVector;
    factor: single;
  begin
    with Objects do
    begin
      Index := -1;
      for I := 0 to MeshCount - 1 do
        if CompareText(string(Mesh[I].NameStr), Name) = 0 then
        begin
          Index := I;
          Break;
        end;

      if Index > -1 then
      begin
        with Mesh[Index]^ do
        begin
          Result.V[0].V[0] := LocMatrix[0];
          Result.V[0].V[1] := LocMatrix[1];
          Result.V[0].V[2] := LocMatrix[2];
          Result.V[0].V[3] := 0;
          Result.V[1].V[0] := LocMatrix[3];
          Result.V[1].V[1] := LocMatrix[4];
          Result.V[1].V[2] := LocMatrix[5];
          Result.V[1].V[3] := 0;
          Result.V[2].V[0] := LocMatrix[6];
          Result.V[2].V[1] := LocMatrix[7];
          Result.V[2].V[2] := LocMatrix[8];
          Result.V[2].V[3] := 0;
          Result.V[3].V[0] := LocMatrix[9];
          Result.V[3].V[1] := LocMatrix[10];
          Result.V[3].V[2] := LocMatrix[11];
          Result.V[3].V[3] := 1;
        end;
        InvertMatrix(Result);

        // If the matrix is not normalized, ie the third column is not equal to the vector product of the first two columns,
        // it means that it is necessary to turn to-pi around the axis Y.
        m := Result;
        v4 := m.V[3];
        factor := VectorLength(m.V[0]);
        NormalizeMatrix(m);
        ScaleMatrix(m, factor);
        m.V[3] := v4;

        v4 := VectorAbs(VectorSubtract(Result.V[2], m.V[2]));
        boolY := (v4.V[0] > abs(Result.V[2].V[0])) and
                 (v4.V[1] > abs(Result.V[2].V[1])) and
                 (v4.V[2] > abs(Result.V[2].V[2]));


        if boolY then
          Result := MatrixMultiply(Result, CreateRotationMatrix(
            AffineVectorMake(0, 1, 0), -pi));

      end
      else
        Result := IdentityHmgMatrix;
    end;
  end;

  //----------------------------------------------------------------------

{$IFDEF VKS_NO_ASM}
  function IsVertexMarked(P: PByteArray; Index: word): boolean; inline;
    // tests the Index-th bit, returns True if set else False
  var
    mi: word;
  begin
    DivMod(index, 8, mi, index);
    Result := (((p^[mi] shr Index) and 1) = 1);
  end;

{$ELSE}
  function IsVertexMarked(P: Pointer; Index: integer): boolean; assembler;
           // tests the Index-th bit, returns True if set else False
  asm
           BT      [EAX], EDX
           SETC    AL
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

{$IFDEF VKS_NO_ASM}
  function MarkVertex(P: PByteArray; Index: word): boolean; inline;
    // sets the Index-th bit and return True if it was already set else False
  var
    mi: word;
  begin
    DivMod(index, 8, mi, index);
    Result := (((p^[mi] shr Index) and 1) = 1);
    if not (Result) then
      p^[mi] := p^[mi] or (1 shl index);
  end;

{$ELSE}
  function MarkVertex(P: Pointer; Index: integer): boolean; assembler;
           // sets the Index-th bit and return True if it was already set else False
  asm
           BTS     [EAX], EDX
           SETC    AL
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

  // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
  // using field SmoothingGroup, which must not be 0.
  // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
  // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
  // it has no smoothing group or a different one then the index of the newly created vertex is
  // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
  // and duplicated vertices.
  // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
  //       have more than one group assigned to a face. To make the code fail safe the group ID
  //       is scanned for the lowest bit set.

{$IFDEF VKS_NO_ASM}
  procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: cardinal;
    P: PSmoothIndexArray);
  var
    i: word;
  begin
    i := 0;
    while SmoothingGroup and (1 shl i) = 0 do
      Inc(i);
    p^[ThisIndex, i] := NewIndex;
  end;

{$ELSE}
  procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: cardinal; P: Pointer);
  asm
           PUSH    EBX
           BSF     EBX, EDX
           // determine smoothing group index (convert flag into an index)
           MOV     EDX, [P]                  // get address of index array
           SHL     EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
           ADD     EAX, EDX
           LEA     EDX, [4 * EBX + EAX]
           // Address of array + vertex index + smoothing group index
           MOV     [EDX], ECX
           POP     EBX
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

{$IFDEF VKS_NO_ASM}
  function GetSmoothIndex(ThisIndex, SmoothingGroup: cardinal;
    P: PSmoothIndexArray): integer; inline;
    // Retrieves the vertex index for the given index and smoothing group.
    // This redirection is necessary because a vertex might have been duplicated.
  var
    i: word;
  begin
    i := 0;
    while SmoothingGroup and (1 shl i) = 0 do
      Inc(i);
    Result := integer(p^[ThisIndex, i]);
  end;

{$ELSE}
  function GetSmoothIndex(ThisIndex, SmoothingGroup: cardinal; P: Pointer): integer;
           // Retrieves the vertex index for the given index and smoothing group.
           // This redirection is necessary because a vertex might have been duplicated.
  asm
           PUSH    EBX
           BSF     EBX, EDX                  // determine smoothing group index
           SHL     EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
           ADD     EAX, ECX
           LEA     ECX, [4 * EBX + EAX]
           // Address of array + vertex index + smoothing group index
           MOV     EAX, [ECX]
           POP     EBX
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

  procedure DuplicateVertex(Index: integer);
  // extends the vector and normal array by one entry and duplicates the vertex AData given by Index
  // the marker and texture arrays will be extended too, if necessary
  begin
    // enhance vertex array
    with mesh.Vertices do
      Add(Items[index]);
    mesh.Normals.Add(NullVector);
    // enhance smooth index array
    ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
    FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
    // enhance marker array
    if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then
    begin
      ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
      Marker[(CurrentVertexCount div 8) + 1] := 0;
    end;
    with mesh.TexCoords do
      if Count > 0 then
        Add(Items[index]);
    Inc(CurrentVertexCount);
  end;

  //---------------------------------------------------------------------------

  function FindMotionIndex(KeyFramer: TKeyFramer; const ObjectName: AnsiString): integer;
    // Looks through the motion list for the object "ObjectName" and returns its index
    // or -1 if the name is not it the list
  var
    I: integer;
  begin
    Result := -1;
    with KeyFramer do
      for I := 0 to MeshMotionCount - 1 do
        if CompareText(string(MeshMotion[I].NameStr), string(ObjectName)) = 0 then
        begin
          Result := I;
          Break;
        end;
  end;

  {$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

var
  CurrentMotionIndex, iMaterial, i, j, x: integer;
  aFaceGroup: TFGVertexIndexList;
  Face, Vertex, TargetVertex: integer;
  SmoothingGroup: cardinal;
  CurrentIndex: word;
  Vector1, Vector2, Normal: TAffineVector;
  standardNormalsOrientation: boolean;
  lights_mesh: TVKFile3DSOmniLightObject;
  camera_mesh: TVKFile3DSCameraObject;
  basemesh: TVKBaseMesh;
begin

  with TFile3DS.Create do
    try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation := not (NormalsOrientation = mnoDefault);

      for i := 0 to Objects.MeshCount - 1 do
        with PMesh3DS(Objects.Mesh[I])^ do
        begin
          hasLightMap := False;
          mesh := TVKFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
          mesh.Name := string(PMesh3DS(Objects.Mesh[I])^.NameStr);
          //dummy targets
          for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
            TMeshMorphTarget.CreateOwned(mesh.MorphTargets);

          with mesh do
          begin
            Mode := momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity := NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts > 0 then
            begin
              TexCoords.Capacity := NVertices;
              for j := 0 to NVertices - 1 do
              begin
                Vertices.Add(PAffineVector(@VertexArray[j])^);
                TexCoords.Add(PTexPoint(@TextArray[j])^);
              end;
            end
            else
            begin
              for j := 0 to NVertices - 1 do
                Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
          end;

          // allocate memory for the smoothindices and the marker array
          CurrentVertexCount := NVertices;
          Marker := AllocMem((NVertices div 8) + 1); // one bit for each vertex
          GetMem(SmoothIndices, NVertices * SizeOf(TSmoothIndexEntry));

          if SmoothArray = nil then
          begin
            // no smoothing groups to consider
            for face := 0 to NFaces - 1 do
              with FaceArray^[Face] do
              begin
                // normal vector for the face
                with mesh.Vertices do
                begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
                end;
                if standardNormalsOrientation then
                  Normal := VectorCrossProduct(Vector1, Vector2)
                else
                  Normal := VectorCrossProduct(Vector2, Vector1);
                // go for each vertex in the current face
                for Vertex := 0 to 2 do
                begin
                  // copy current index for faster access
                  CurrentIndex := FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) and (CurrentVertexCount < High(FaceRec[Vertex])) then
                  begin
                    // already touched vertex must be duplicated
                    DuplicateVertex(CurrentIndex);
                    FaceRec[Vertex] := CurrentVertexCount - 1;
                    mesh.Normals[CurrentVertexCount - 1] := Normal;
                  end
                  else
                  begin
                    // not yet touched, so just store the normal
                    mesh.Normals[CurrentIndex] := Normal;
                    MarkVertex(Marker, CurrentIndex);
                  end;
                end;
              end;
          end
          else
          begin
            // smoothing groups are to be considered
            for Face := 0 to NFaces - 1 do
              with FaceArray^[Face] do
              begin
                // normal vector for the face
                with mesh.Vertices do
                begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
                end;
                if standardNormalsOrientation then
                  Normal := VectorCrossProduct(Vector1, Vector2)
                else
                  Normal := VectorCrossProduct(Vector2, Vector1);
                SmoothingGroup := SmoothArray^[Face];
                // go for each vertex in the current face
                for Vertex := 0 to 2 do
                begin
                  // copy current index for faster access
                  currentIndex := FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then
                  begin
                    // check smoothing group
                    if (SmoothingGroup = 0) then
                    begin
                      if (CurrentVertexCount < High(FaceRec[Vertex])) then
                      begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex] := CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1] := Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                      end;
                    end
                    else
                    begin
                      // this vertex must be smoothed, check if there's already
                      // a (duplicated) vertex for this smoothing group
                      TargetVertex :=
                        GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                      if (TargetVertex < 0) then
                      begin
                        if (CurrentVertexCount < High(FaceRec[Vertex])) then
                        begin
                          // vertex has not yet been duplicated for this smoothing
                          // group, so do it now
                          DuplicateVertex(CurrentIndex);
                          FaceRec[Vertex] := CurrentVertexCount - 1;
                          mesh.Normals[CurrentVertexCount - 1] := Normal;
                          StoreSmoothIndex(CurrentIndex, SmoothingGroup,
                            CurrentVertexCount - 1, SmoothIndices);
                          StoreSmoothIndex(CurrentVertexCount - 1,
                            SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                          // mark new vertex also as touched
                          MarkVertex(Marker, CurrentVertexCount - 1);
                        end;  
                      end
                      else
                      begin
                        // vertex has already been duplicated,
                        // so just add normal vector to other vertex...
                        mesh.Normals[TargetVertex] :=
                          VectorAdd(mesh.Normals[TargetVertex], Normal);
                        // ...and tell which new vertex has to be used from now on
                        FaceRec[Vertex] := TargetVertex;
                      end;
                    end;
                  end
                  else
                  begin
                    // vertex not yet touched, so just store the normal
                    mesh.Normals[CurrentIndex] := Normal;
                    // initialize smooth indices for this vertex
                    FillChar(SmoothIndices[CurrentIndex],
                      SizeOf(TSmoothIndexEntry), $FF);
                    if SmoothingGroup <> 0 then
                      StoreSmoothIndex(CurrentIndex, SmoothingGroup,
                        CurrentIndex, SmoothIndices);
                    MarkVertex(Marker, CurrentIndex);
                  end;
                end;
              end;
          end;
          FreeMem(Marker);
          FreeMem(SmoothIndices);

          Assert(mesh.Vertices.Count = CurrentVertexCount);

          // and normalize the Normals array
          mesh.Normals.Normalize;

          // now go for each material group
          // if there's no face to material assignment then just copy the
          // face definitions and rely on the default texture of the scene object
          if (NMats = 0) or (not vGLVectorFileObjectsAllocateMaterials) then
          begin
            aFaceGroup := TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
            with aFaceGroup do
            begin
              basemesh := TVKBaseMesh(Self.GetOwner);
              if basemesh.MaterialLibrary <> nil then
                MaterialName := basemesh.MaterialLibrary.Materials.Add.Name;
              // copy the face list
              for j := 0 to NFaces - 1 do
              begin
                Add(FaceArray[J].V1);
                Add(FaceArray[J].V2);
                Add(FaceArray[J].V3);
              end;
            end;
          end
          else
          begin
            for iMaterial := 0 to NMats - 1 do
            begin
              aFaceGroup := TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
              with aFaceGroup do
              begin
                MaterialName :=
                  GetOrAllocateMaterial(Materials, string(MatArray[iMaterial].NameStr));
                LightMapIndex :=
                  GetOrAllocateLightMap(Materials, string(MatArray[iMaterial].NameStr));
                // copy all vertices belonging to the current face into our index array,
                // there won't be redundant vertices since this would mean a face has more than one
                // material
                // NFaces is the one from FaceGroup
                with MatArray[iMaterial] do
                  for j := 0 to NFaces - 1 do
                  begin
                    Add(FaceArray[FaceIndex[J]].V1);
                    Add(FaceArray[FaceIndex[J]].V2);
                    Add(FaceArray[FaceIndex[J]].V3);
                  end;
              end;
            end;
          end;
          if hasLightMap then
            for j := 0 to mesh.TexCoords.Count - 1 do
              mesh.LightMapTexCoords.Add(mesh.TexCoords[j].V[0], mesh.TexCoords[j].V[1]);
        end;

      // Adding non-mesh objects (for example, dummies).
      for I := 0 to KeyFramer.MeshMotionCount - 1 do
        if (Owner.MeshObjects.FindMeshByName(string(
          KeyFramer.MeshMotion[I].NameStr)) = nil) then
        begin
          mesh := TVKFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
          mesh.Name := string(KeyFramer.MeshMotion[I].NameStr);
          //dummy targets
          for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
            TMeshMorphTarget.CreateOwned(mesh.MorphTargets);

          mesh.LoadAnimation(KeyFramer.MeshMotion[I]);
        end;

      for I := 0 to Objects.MeshCount - 1 do
        with PMesh3DS(Objects.Mesh[I])^ do
        begin
          mesh := Owner.MeshObjects.FindMeshByName(string(NameStr)) as TVKFile3DSMeshObject;
          with mesh, KeyFramer do
          begin
            CurrentMotionIndex := FindMotionIndex(KeyFramer, NameStr);
            FRefTranf.ModelMatrix := InvertMeshMatrix(Objects, string(NameStr));

            if MeshMotionCount > 0 then
              LoadAnimation(MeshMotion[CurrentMotionIndex]);
          end;
        end;


      // Lights Omni.
      for I := 0 to Objects.OmniLightCount - 1 do
      begin
        lights_mesh := TVKFile3DSOmniLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.OmniLight[I]);
        lights_mesh.LoadAnimation(KeyFramer.OmniLightMotion[I]);
      end;

      // Lights Spot.
      for I := 0 to Objects.SpotLightCount - 1 do
      begin
        lights_mesh := TVKFile3DSSpotLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.SpotLight[I]);
        lights_mesh.LoadAnimation(KeyFramer.SpotLightMotion[I]);
      end;

      // Camera Objects.
      for I := 0 to Objects.CameraCount - 1 do
      begin
        camera_mesh := TVKFile3DSCameraObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TMeshMorphTarget.CreateOwned(camera_mesh.MorphTargets);
        camera_mesh.LoadData(Owner, Objects.Camera[I]);
        camera_mesh.LoadAnimation(KeyFramer.CameraMotion[I]);
      end;

      // Apply animation matrix to static data
      if vGLFile3DS_LoadedStaticFrame >= 0 then
      begin
        for i := 0 to Owner.MeshObjects.Count - 1 do
        begin
          if Owner.MeshObjects[i] is TVKFile3DSMeshObject then
          begin
            mesh := Owner.MeshObjects[i] as TVKFile3DSMeshObject;
            mesh.FStatic := True;
            for j := 0 to mesh.Vertices.Count - 1 do
              mesh.Vertices[j] := VectorTransform(mesh.Vertices[j], mesh.FAnimTransf.ModelMatrix);
          end;
        end;
      end;


    finally
      Free;
    end;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  RegisterClasses([TVKFile3DSDummyObject, TVKFile3DSMeshObject,
    TVKFile3DSOmniLightObject, TVKFile3DSSpotLightObject,
    TVKFile3DSCameraObject]);

  RegisterVectorFileFormat('3ds', '3D Studio files', TVK3DSVectorFile);
  RegisterVectorFileFormat('prj', '3D Studio project files', TVK3DSVectorFile);
end.

