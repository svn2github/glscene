//
// This unit is part of the GLScene Project   
//
{ : VKS.Coordinates<p>

  Coordinate related classes.<p>

  <b>History : </b><font size=-1><ul>
  <li>20/11/12 - PW - Added CPP compatibility: replaced direct access to some properties by a get.. and a set.. methods
  <li>30/06/11 - DaStr - Added TVKCustomCoordinates.Coordinate default property
  <li>05/09/10 - Yar - Fix notification in TVKCustomCoordinates.NotifyChange (thanks C4)
  <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses
  <li>05/10/08 - DanB - Created, from GLMisc.pas
  </ul></font>
}
unit VKS.Coordinates;

interface

uses
  System.Classes, System.SysUtils,
   
  VKS.VectorGeometry, VKS.VectorTypes, VKS.OpenGLTokens, VKS.BaseClasses,
  VKS.CrossPlatform;

{$I VKScene.inc}

type

  // TVKCoordinatesStyle
  //
  { : Identifie le type de données stockées au sein d'un TVKCustomCoordinates.<p>
    <ul><li>csPoint2D : a simple 2D point (Z=0, W=0)
    <ul><li>csPoint : un point (W=1)
    <li>csVector : un vecteur (W=0)
    <li>csUnknown : aucune contrainte
    </ul> }
  TVKCoordinatesStyle = (CsPoint2D, CsPoint, CsVector, CsUnknown);

  // TVKCustomCoordinates
  //
  { : Stores and homogenous vector.<p>
    This class is basicly a container for a TVector, allowing proper use of
    delphi property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal.<br>
    Handles dynamic default values to save resource file space.<p> }
  TVKCustomCoordinates = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FCoords: TVector;
    FStyle: TVKCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    function GetAsPoint2D: TVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): TVKFloat;
    procedure SetCoordinate(const AIndex: Integer; const AValue: TVKFloat);
    function GetDirectCoordinate(const Index: Integer): TVKFloat;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: TVKFloat);

  protected
    { Protected Declarations }
    procedure SetDirectVector(const V: TVector);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TVector;
      const AStyle: TVKCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);

    procedure Initialize(const Value: TVector);
    procedure NotifyChange(Sender: TObject); override;

    { : Identifies the coordinates styles.<p>
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally).<p>
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
    function VectorLength: TVKFloat;
    function VectorNorm: TVKFloat;
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

    { : The coordinates viewed as a vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsVector: TVector read FCoords write SetAsVector;

    { : The coordinates viewed as an affine vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.<br>
      The W component is automatically adjustes depending on style. }
    property AsAffineVector: TAffineVector read GetAsAffineVector
      write SetAsAffineVector;

    { : The coordinates viewed as a 2D point.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;

    property X: TVKFloat index 0 read GetCoordinate write SetCoordinate;
    property Y: TVKFloat index 1 read GetCoordinate write SetCoordinate;
    property Z: TVKFloat index 2 read GetCoordinate write SetCoordinate;
    property W: TVKFloat index 3 read GetCoordinate write SetCoordinate;

    property Coordinate[const AIndex: Integer]: TVKFloat read GetCoordinate
      write SetCoordinate; default;

    { : The coordinates, in-between brackets, separated by semi-colons. }
    property AsString: String read GetAsString;

    // : Similar to AsVector but does not trigger notification events
    property DirectVector: TVector read FCoords write SetDirectVector;
    property DirectX: TVKFloat index 0 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectY: TVKFloat index 1 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectZ: TVKFloat index 2 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectW: TVKFloat index 3 read GetDirectCoordinate
      write SetDirectCoordinate;
  end;

  { : A TVKCustomCoordinates that publishes X, Y properties. }
  TVKCoordinates2 = class(TVKCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  { : A TVKCustomCoordinates that publishes X, Y, Z properties. }
  TVKCoordinates3 = class(TVKCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  // TVKCoordinates4
  //
  { : A TVKCustomCoordinates that publishes X, Y, Z, W properties. }
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
    { Public Declarations }
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
      Write(FCoords.V[0], SizeOf(FCoords));
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
      Read(FCoords.V[0], N);
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
  FCoords.V[0] := FCoords.V[0] + TranslationVector.V[0];
  FCoords.V[1] := FCoords.V[1] + TranslationVector.V[1];
  FCoords.V[2] := FCoords.V[2] + TranslationVector.V[2];
  NotifyChange(Self);
end;

// Translate
//
procedure TVKCustomCoordinates.Translate(const TranslationVector
  : TAffineVector);
begin
  FCoords.V[0] := FCoords.V[0] + TranslationVector.V[0];
  FCoords.V[1] := FCoords.V[1] + TranslationVector.V[1];
  FCoords.V[2] := FCoords.V[2] + TranslationVector.V[2];
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
function TVKCustomCoordinates.VectorLength: TVKFloat;
begin
  Result := VKS.VectorGeometry.VectorLength(FCoords);
end;

// VectorNorm
//
function TVKCustomCoordinates.VectorNorm: TVKFloat;
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
  const AValue: TVKFloat);
begin
  FCoords.V[index] := AValue;
end;

procedure TVKCustomCoordinates.SetDirectVector(const V: TVector);
begin
  FCoords.V[0] := V.V[0];
  FCoords.V[1] := V.V[1];
  FCoords.V[2] := V.V[2];
  FCoords.V[3] := V.V[3];
end;

// SetToZero
//
procedure TVKCustomCoordinates.SetToZero;
begin
  FCoords.V[0] := 0;
  FCoords.V[1] := 0;
  FCoords.V[2] := 0;
  if FStyle = CsPoint then
    FCoords.V[3] := 1
  else
    FCoords.V[3] := 0;
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
  VKS.VectorGeometry.MakeVector(FCoords, Vector.V[0], Vector.V[1], 0);
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
        FCoords.V[2] := 0;
        FCoords.V[3] := 0;
      end;
    CsPoint:
      FCoords.V[3] := 1;
    CsVector:
      FCoords.V[3] := 0;
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
        FCoords.V[0] := Value.V[0];
        FCoords.V[1] := Value.V[1];
        FCoords.V[2] := 0;
        FCoords.V[3] := 0;
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
  Result.V[0] := FCoords.V[0];
  Result.V[1] := FCoords.V[1];
end;

// SetCoordinate
//
procedure TVKCustomCoordinates.SetCoordinate(const AIndex: Integer;
  const AValue: TVKFloat);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

// GetCoordinate
//
function TVKCustomCoordinates.GetCoordinate(const AIndex: Integer): TVKFloat;
begin
  Result := FCoords.V[AIndex];
end;

function TVKCustomCoordinates.GetDirectCoordinate(
  const Index: Integer): TVKFloat;
begin
  Result := FCoords.V[index]
end;

// GetAsString
//
function TVKCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.V[0], FCoords.V[1]]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.V[0], FCoords.V[1], FCoords.V[2]]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.V[0], FCoords.V[1], FCoords.V[2],
        FCoords.V[3]]);
  else
    Assert(False);
  end;
end;

initialization

RegisterClasses([TVKCoordinates2, TVKCoordinates3, TVKCoordinates4]);

end.
