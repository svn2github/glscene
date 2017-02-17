//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Coordinate related classes. 
}

unit VKS.Coordinates;

interface

uses
  Winapi.OpenGL,
  System.Classes, System.SysUtils,
  
  VKS.VectorGeometry, VKS.VectorTypes, VKS.BaseClasses,
  VKS.CrossPlatform;

{$I VKScene.inc}

type

  // TVKCoordinatesStyle
  //
  { Identifie le type de données stockées au sein d'un TVKCustomCoordinates. 
      csPoint2D : a simple 2D point (Z=0, W=0)
      csPoint : un point (W=1)
     csVector : un vecteur (W=0)
     csUnknown : aucune contrainte
      }
  TVKCoordinatesStyle = (CsPoint2D, CsPoint, CsVector, CsUnknown);

  // TVKCustomCoordinates
  //
  { Stores and homogenous vector. 
    This class is basicly a container for a TVector, allowing proper use of
    delphi property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal. 
    Handles dynamic default values to save resource file space.  }
  TVKCustomCoordinates = class(TVKUpdateAbleObject)
  private
    
    FCoords: TVector;
    FStyle: TVKCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    function GetAsPoint2D: TVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): GLfloat;
    procedure SetCoordinate(const AIndex: Integer; const AValue: GLfloat);
    function GetDirectCoordinate(const Index: Integer): GLfloat;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: GLfloat);

  protected
    
    procedure SetDirectVector(const V: TVector);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TVector;
      const AStyle: TVKCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);

    procedure Initialize(const Value: TVector);
    procedure NotifyChange(Sender: TObject); override;

    { Identifies the coordinates styles. 
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally). 
      It is used by the TVKCustomCoordinates for internal "assertion" checks
      to detect "misuses" or "misunderstandings" of what the homogeneous
      coordinates system implies. }
    property Style: TVKCoordinatesStyle read FStyle write FStyle;

    procedure Translate(const TranslationVector: TVector); overload;
    procedure Translate(const TranslationVector: TAffineVector); overload;
    procedure AddScaledVector(const Factor: Single;
      const TranslationVector: TVector); overload;
    procedure AddScaledVector(const Factor: Single;
      const TranslationVector: TAffineVector); overload;
    procedure Rotate(const AnAxis: TAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TVector; AnAngle: Single); overload;
    procedure Normalize;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: GLfloat;
    function VectorNorm: GLfloat;
    function MaxXYZ: Single;
    function Equals(const AVector: TVector): Boolean; reintroduce;

    procedure SetVector(const X, Y: Single; Z: Single = 0); overload;
    procedure SetVector(const X, Y, Z, W: Single); overload;
    procedure SetVector(const V: TAffineVector); overload;
    procedure SetVector(const V: TVector); overload;

    procedure SetPoint(const X, Y, Z: Single); overload;
    procedure SetPoint(const V: TAffineVector); overload;
    procedure SetPoint(const V: TVector); overload;

    procedure SetPoint2D(const X, Y: Single); overload;
    procedure SetPoint2D(const Vector: TAffineVector); overload;
    procedure SetPoint2D(const Vector: TVector); overload;
    procedure SetPoint2D(const Vector: TVector2f); overload;

    procedure SetToZero;
    function AsAddress: PGLFloat;

    { The coordinates viewed as a vector. 
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsVector: TVector read FCoords write SetAsVector;

    { The coordinates viewed as an affine vector. 
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. 
      The W component is automatically adjustes depending on style. }
    property AsAffineVector: TAffineVector read GetAsAffineVector
      write SetAsAffineVector;

    { The coordinates viewed as a 2D point. 
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;

    property X: GLfloat index 0 read GetCoordinate write SetCoordinate;
    property Y: GLfloat index 1 read GetCoordinate write SetCoordinate;
    property Z: GLfloat index 2 read GetCoordinate write SetCoordinate;
    property W: GLfloat index 3 read GetCoordinate write SetCoordinate;

    property Coordinate[const AIndex: Integer]: GLfloat read GetCoordinate
      write SetCoordinate; default;

    { The coordinates, in-between brackets, separated by semi-colons. }
    property AsString: String read GetAsString;

    // : Similar to AsVector but does not trigger notification events
    property DirectVector: TVector read FCoords write SetDirectVector;
    property DirectX: GLfloat index 0 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectY: GLfloat index 1 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectZ: GLfloat index 2 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectW: GLfloat index 3 read GetDirectCoordinate
      write SetDirectCoordinate;
  end;

  { A TVKCustomCoordinates that publishes X, Y properties. }
  TVKCoordinates2 = class(TVKCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  { A TVKCustomCoordinates that publishes X, Y, Z properties. }
  TVKCoordinates3 = class(TVKCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  // TVKCoordinates4
  //
  { A TVKCustomCoordinates that publishes X, Y, Z, W properties. }
  TVKCoordinates4 = class(TVKCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property W stored False;
  end;

  // TVKCoordinates
  //
  TVKCoordinates = TVKCoordinates3;

  // Actually Sender should be TVKCustomCoordinates, but that would require
  // changes in a some other GLScene units and some other projects that use
  // TVKCoordinatesUpdateAbleComponent
  IGLCoordinatesUpdateAble = interface(IInterface)
    ['{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}']
    procedure CoordinateChanged(Sender: TVKCustomCoordinates);
  end;

  // TVKCoordinatesUpdateAbleComponent
  //
  TVKCoordinatesUpdateAbleComponent = class(TVKUpdateAbleComponent,
    IGLCoordinatesUpdateAble)
  public
    
    procedure CoordinateChanged(Sender: TVKCustomCoordinates); virtual;
      abstract;
  end;

var
  // Specifies if TVKCustomCoordinates should allocate memory for
  // their default values (ie. design-time) or not (run-time)
  VUseDefaultCoordinateSets: Boolean = False;

implementation

const
  CsVectorHelp =
    'If you are getting assertions here, consider using the SetPoint procedure';
  CsPointHelp =
    'If you are getting assertions here, consider using the SetVector procedure';
  CsPoint2DHelp =
    'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';

  // ------------------
  // ------------------ TVKCustomCoordinates ------------------
  // ------------------

  // CreateInitialized
  //
constructor TVKCustomCoordinates.CreateInitialized(AOwner: TPersistent;
  const AValue: TVector; const AStyle: TVKCoordinatesStyle = CsUnknown);
begin
  Create(AOwner);
  Initialize(AValue);
  FStyle := AStyle;
end;

// Destroy
//
destructor TVKCustomCoordinates.Destroy;
begin
  if Assigned(FPDefaultCoords) then
    Dispose(FPDefaultCoords);
  inherited;
end;

// Initialize
//
procedure TVKCustomCoordinates.Initialize(const Value: TVector);
begin
  FCoords := Value;
  if VUseDefaultCoordinateSets then
  begin
    if not Assigned(FPDefaultCoords) then
      New(FPDefaultCoords);
    FPDefaultCoords^ := Value;
  end;
end;

// Assign
//
procedure TVKCustomCoordinates.Assign(Source: TPersistent);
begin
  if Source is TVKCustomCoordinates then
    FCoords := TVKCustomCoordinates(Source).FCoords
  else
    inherited;
end;

// WriteToFiler
//
procedure TVKCustomCoordinates.WriteToFiler(Writer: TWriter);
var
  WriteCoords: Boolean;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    if VUseDefaultCoordinateSets then
      WriteCoords := not VectorEquals(FPDefaultCoords^, FCoords)
    else
      WriteCoords := True;
    WriteBoolean(WriteCoords);
    if WriteCoords then
      Write(FCoords.X, SizeOf(FCoords));
  end;
end;

// ReadFromFiler
//
procedure TVKCustomCoordinates.ReadFromFiler(Reader: TReader);
var
  N: Integer;
begin
  with Reader do
  begin
    ReadInteger; // Ignore ArchiveVersion
    if ReadBoolean then
    begin
      N := SizeOf(FCoords);
      Assert(N = 4 * SizeOf(Single));
      Read(FCoords.X, N);
    end
    else if Assigned(FPDefaultCoords) then
      FCoords := FPDefaultCoords^;
  end;
end;

// DefineProperties
//
procedure TVKCustomCoordinates.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
    not(Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

// ReadData
//
procedure TVKCustomCoordinates.ReadData(Stream: TStream);
begin
  Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TVKCustomCoordinates.WriteData(Stream: TStream);
begin
  Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TVKCustomCoordinates.NotifyChange(Sender: TObject);
var
  Int: IGLCoordinatesUpdateAble;
begin
  if Supports(Owner, IGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TVKCoordinates(Self));
  inherited NotifyChange(Sender);
end;

// Translate
//
procedure TVKCustomCoordinates.Translate(const TranslationVector: TVector);
begin
  FCoords.X := FCoords.X + TranslationVector.X;
  FCoords.Y := FCoords.Y + TranslationVector.Y;
  FCoords.Z := FCoords.Z + TranslationVector.Z;
  NotifyChange(Self);
end;

// Translate
//
procedure TVKCustomCoordinates.Translate(const TranslationVector
  : TAffineVector);
begin
  FCoords.X := FCoords.X + TranslationVector.X;
  FCoords.Y := FCoords.Y + TranslationVector.Y;
  FCoords.Z := FCoords.Z + TranslationVector.Z;
  NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TVKCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TVKCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TAffineVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// Rotate (affine)
//
procedure TVKCustomCoordinates.Rotate(const AnAxis: TAffineVector;
  AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Rotate (hmg)
//
procedure TVKCustomCoordinates.Rotate(const AnAxis: TVector; AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Normalize
//
procedure TVKCustomCoordinates.Normalize;
begin
  NormalizeVector(FCoords);
  NotifyChange(Self);
end;

// Invert
//
procedure TVKCustomCoordinates.Invert;
begin
  NegateVector(FCoords);
  NotifyChange(Self);
end;

// Scale
//
procedure TVKCustomCoordinates.Scale(Factor: Single);
begin
  ScaleVector(PAffineVector(@FCoords)^, Factor);
  NotifyChange(Self);
end;

// VectorLength
//
function TVKCustomCoordinates.VectorLength: GLfloat;
begin
  Result := VKS.VectorGeometry.VectorLength(FCoords);
end;

// VectorNorm
//
function TVKCustomCoordinates.VectorNorm: GLfloat;
begin
  Result := VKS.VectorGeometry.VectorNorm(FCoords);
end;

// MaxXYZ
//
function TVKCustomCoordinates.MaxXYZ: Single;
begin
  Result := VKS.VectorGeometry.MaxXYZComponent(FCoords);
end;

// Equals
//
function TVKCustomCoordinates.Equals(const AVector: TVector): Boolean;
begin
  Result := VectorEquals(FCoords, AVector);
end;

// SetVector (affine)
//
procedure TVKCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VKS.VectorGeometry.SetVector(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetVector (TAffineVector)
//
procedure TVKCustomCoordinates.SetVector(const V: TAffineVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VKS.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (TVector)
//
procedure TVKCustomCoordinates.SetVector(const V: TVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VKS.VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TVKCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VKS.VectorGeometry.SetVector(FCoords, X, Y, Z, W);
  NotifyChange(Self);
end;

// SetDirectVector
//
procedure TVKCustomCoordinates.SetDirectCoordinate(const Index: Integer;
  const AValue: GLfloat);
begin
  FCoords.V[index] := AValue;
end;

procedure TVKCustomCoordinates.SetDirectVector(const V: TVector);
begin
  FCoords.X := V.X;
  FCoords.Y := V.Y;
  FCoords.Z := V.Z;
  FCoords.W := V.W;
end;

// SetToZero
//
procedure TVKCustomCoordinates.SetToZero;
begin
  FCoords.X := 0;
  FCoords.Y := 0;
  FCoords.Z := 0;
  if FStyle = CsPoint then
    FCoords.W := 1
  else
    FCoords.W := 0;
  NotifyChange(Self);
end;

// SetPoint
//
procedure TVKCustomCoordinates.SetPoint(const X, Y, Z: Single);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VKS.VectorGeometry.MakePoint(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetPoint (TAffineVector)
//
procedure TVKCustomCoordinates.SetPoint(const V: TAffineVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VKS.VectorGeometry.MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint (TVector)
//
procedure TVKCustomCoordinates.SetPoint(const V: TVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VKS.VectorGeometry.MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint2D
//
procedure TVKCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VKS.VectorGeometry.MakeVector(FCoords, X, Y, 0);
  NotifyChange(Self);
end;

// SetPoint2D (TAffineVector)
//
procedure TVKCustomCoordinates.SetPoint2D(const Vector: TAffineVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VKS.VectorGeometry.MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

// SetPoint2D (TVector)
//
procedure TVKCustomCoordinates.SetPoint2D(const Vector: TVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VKS.VectorGeometry.MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

// SetPoint2D (TVector2f)
//
procedure TVKCustomCoordinates.SetPoint2D(const Vector: TVector2f);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VKS.VectorGeometry.MakeVector(FCoords, Vector.X, Vector.Y, 0);
  NotifyChange(Self);
end;

// AsAddress
//
function TVKCustomCoordinates.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//
procedure TVKCustomCoordinates.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  case FStyle of
    CsPoint2D:
      begin
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
    CsPoint:
      FCoords.W := 1;
    CsVector:
      FCoords.W := 0;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// SetAsAffineVector
//
procedure TVKCustomCoordinates.SetAsAffineVector(const Value: TAffineVector);
begin
  case FStyle of
    CsPoint2D:
      MakeVector(FCoords, Value);
    CsPoint:
      MakePoint(FCoords, Value);
    CsVector:
      MakeVector(FCoords, Value);
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// SetAsPoint2D
//
procedure TVKCustomCoordinates.SetAsPoint2D(const Value: TVector2f);
begin
  case FStyle of
    CsPoint2D, CsPoint, CsVector:
      begin
        FCoords.X := Value.X;
        FCoords.Y := Value.Y;
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// GetAsAffineVector
//
function TVKCustomCoordinates.GetAsAffineVector: TAffineVector;
begin
  VKS.VectorGeometry.SetVector(Result, FCoords);
end;

// GetAsPoint2D
//
function TVKCustomCoordinates.GetAsPoint2D: TVector2f;
begin
  Result.X := FCoords.X;
  Result.Y := FCoords.Y;
end;

// SetCoordinate
//
procedure TVKCustomCoordinates.SetCoordinate(const AIndex: Integer;
  const AValue: GLfloat);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

// GetCoordinate
//
function TVKCustomCoordinates.GetCoordinate(const AIndex: Integer): GLfloat;
begin
  Result := FCoords.V[AIndex];
end;

function TVKCustomCoordinates.GetDirectCoordinate(
  const Index: Integer): GLfloat;
begin
  Result := FCoords.V[index]
end;

// GetAsString
//
function TVKCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.X, FCoords.Y]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z,
        FCoords.W]);
  else
    Assert(False);
  end;
end;

initialization

RegisterClasses([TVKCoordinates2, TVKCoordinates3, TVKCoordinates4]);

end.
