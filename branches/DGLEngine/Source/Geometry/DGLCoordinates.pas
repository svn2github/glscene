//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLCoordinates<p>

  Coordinate related classes.<p>

  <b>Historique : </b><font size=-1><ul>
  <li>21/12/15 - JD -  Imported From GLScene
  </ul></font>
}
unit DGLCoordinates;

interface

uses
  System.Classes, System.SysUtils,

  // GLS
  DGLTypes, DGLVectorMaths, DGLVectorTypes, dglOpenGL, DGLBaseClasses,
  DGLCrossPlatform;

{$I DGLEngine.inc}

type
  // TDGLCustomCoordinates
  //
  { : Stores and homogenous vector.<p>
    This class is basicly a container for a TVector, allowing proper use of
    delphi property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal.<br>
    Handles dynamic default values to save resource file space.<p> }
  TDGLCustomCoordinates = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    FCoords:         TVector;
    FStyle:          TDGLCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    function GetAsPoint2D: TVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): TGLFloat;
    procedure SetCoordinate(const AIndex: Integer; const AValue: TGLFloat);
    function GetDirectCoordinate(const Index: Integer): TGLFloat;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: TGLFloat);

  protected
    { Protected Declarations }
    procedure SetDirectVector(const V: TVector);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TVector; const AStyle: TDGLCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);

    procedure Initialize(const Value: TVector);
    procedure NotifyChange(Sender: TObject); override;

    { : Identifies the coordinates styles.<p>
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally).<p>
      It is used by the TDGLCustomCoordinates for internal "assertion" checks
      to detect "misuses" or "misunderstandings" of what the homogeneous
      coordinates system implies. }
    property Style: TDGLCoordinatesStyle read FStyle write FStyle;

    procedure Translate(const TranslationVector: TVector); overload;
    procedure Translate(const TranslationVector: TAffineVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TAffineVector); overload;
    procedure Rotate(const AnAxis: TAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TVector; AnAngle: Single); overload;
    procedure Normalize;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: TGLFloat;
    function VectorNorm: TGLFloat;
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
    property AsAffineVector: TAffineVector read GetAsAffineVector write SetAsAffineVector;

    { : The coordinates viewed as a 2D point.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;

    property X: TGLFloat index 0 read GetCoordinate write SetCoordinate;
    property Y: TGLFloat index 1 read GetCoordinate write SetCoordinate;
    property Z: TGLFloat index 2 read GetCoordinate write SetCoordinate;
    property W: TGLFloat index 3 read GetCoordinate write SetCoordinate;

    property Coordinate[const AIndex: Integer]: TGLFloat read GetCoordinate write SetCoordinate; default;

    { : The coordinates, in-between brackets, separated by semi-colons. }
    property AsString: String read GetAsString;

    // : Similar to AsVector but does not trigger notification events
    property DirectVector: TVector read FCoords write SetDirectVector;
    property DirectX: TGLFloat index 0 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectY: TGLFloat index 1 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectZ: TGLFloat index 2 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectW: TGLFloat index 3 read GetDirectCoordinate write SetDirectCoordinate;
  end;

  { : A TDGLCustomCoordinates that publishes X, Y properties. }
  TDGLCoordinates2 = class(TDGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  { : A TDGLCustomCoordinates that publishes X, Y, Z properties. }
  TDGLCoordinates3 = class(TDGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  // TDGLCoordinates4
  //
  { : A TDGLCustomCoordinates that publishes X, Y, Z, W properties. }
  TDGLCoordinates4 = class(TDGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property W stored False;
  end;

  // TDGLCoordinates
  //
  TDGLCoordinates = TDGLCoordinates3;

  // Actually Sender should be TDGLCustomCoordinates, but that would require
  // changes in a some other GLScene units and some other projects that use
  // TDGLCoordinatesUpdateAbleComponent
  IDGLCoordinatesUpdateAble = interface(IInterface)
    ['{B6FC8234-B9B4-46D7-87F6-A93F96B20CE5}']
    procedure CoordinateChanged(Sender: TDGLCustomCoordinates);
  end;

  // TDGLCoordinatesUpdateAbleComponent
  //
  TDGLCoordinatesUpdateAbleComponent = class(TDGLUpdateAbleComponent, IDGLCoordinatesUpdateAble)
  public
    { Public Declarations }
    procedure CoordinateChanged(Sender: TDGLCustomCoordinates); virtual; abstract;
  end;

var
  // Specifies if TDGLCustomCoordinates should allocate memory for
  // their default values (ie. design-time) or not (run-time)
  VUseDefaultCoordinateSets: Boolean = False;


implementation

uses DGLResStrings;

// ------------------
{ TDGLCustomCoordinates }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLCustomCoordinates'}{$ENDIF}

constructor TDGLCustomCoordinates.CreateInitialized(AOwner: TPersistent; const AValue: TVector; const AStyle: TDGLCoordinatesStyle = CsUnknown);
begin
  Create(AOwner);
  Initialize(AValue);
  FStyle := AStyle;
end;

// Destroy
//
destructor TDGLCustomCoordinates.Destroy;
begin
  if Assigned(FPDefaultCoords) then
    Dispose(FPDefaultCoords);
  inherited;
end;

// Initialize
//
procedure TDGLCustomCoordinates.Initialize(const Value: TVector);
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
procedure TDGLCustomCoordinates.Assign(Source: TPersistent);
begin
  if Source is TDGLCustomCoordinates then
    FCoords := TDGLCustomCoordinates(Source).FCoords
  else
    inherited;
end;

// WriteToFiler
//
procedure TDGLCustomCoordinates.WriteToFiler(Writer: TWriter);
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
procedure TDGLCustomCoordinates.ReadFromFiler(Reader: TReader);
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
procedure TDGLCustomCoordinates.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData, not(Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

// ReadData
//
procedure TDGLCustomCoordinates.ReadData(Stream: TStream);
begin
  Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TDGLCustomCoordinates.WriteData(Stream: TStream);
begin
  Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TDGLCustomCoordinates.NotifyChange(Sender: TObject);
var
  Int: IDGLCoordinatesUpdateAble;
begin
  if Supports(Owner, IDGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TDGLCoordinates(Self));
  inherited NotifyChange(Sender);
end;

// Translate
//
procedure TDGLCustomCoordinates.Translate(const TranslationVector: TVector);
begin
  FCoords.V[0] := FCoords.V[0] + TranslationVector.V[0];
  FCoords.V[1] := FCoords.V[1] + TranslationVector.V[1];
  FCoords.V[2] := FCoords.V[2] + TranslationVector.V[2];
  NotifyChange(Self);
end;

// Translate
//
procedure TDGLCustomCoordinates.Translate(const TranslationVector: TAffineVector);
begin
  FCoords.V[0] := FCoords.V[0] + TranslationVector.V[0];
  FCoords.V[1] := FCoords.V[1] + TranslationVector.V[1];
  FCoords.V[2] := FCoords.V[2] + TranslationVector.V[2];
  NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TDGLCustomCoordinates.AddScaledVector(const Factor: Single; const TranslationVector: TVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TDGLCustomCoordinates.AddScaledVector(const Factor: Single; const TranslationVector: TAffineVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// Rotate (affine)
//
procedure TDGLCustomCoordinates.Rotate(const AnAxis: TAffineVector; AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Rotate (hmg)
//
procedure TDGLCustomCoordinates.Rotate(const AnAxis: TVector; AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Normalize
//
procedure TDGLCustomCoordinates.Normalize;
begin
  NormalizeVector(FCoords);
  NotifyChange(Self);
end;

// Invert
//
procedure TDGLCustomCoordinates.Invert;
begin
  NegateVector(FCoords);
  NotifyChange(Self);
end;

// Scale
//
procedure TDGLCustomCoordinates.Scale(Factor: Single);
begin
  ScaleVector(PAffineVector(@FCoords)^, Factor);
  NotifyChange(Self);
end;

// VectorLength
//
function TDGLCustomCoordinates.VectorLength: TGLFloat;
begin
  Result := DGLVectorMaths.VectorLength(FCoords);
end;

// VectorNorm
//
function TDGLCustomCoordinates.VectorNorm: TGLFloat;
begin
  Result := DGLVectorMaths.VectorNorm(FCoords);
end;

// MaxXYZ
//
function TDGLCustomCoordinates.MaxXYZ: Single;
begin
  Result := MaxXYZComponent(FCoords);
end;

// Equals
//
function TDGLCustomCoordinates.Equals(const AVector: TVector): Boolean;
begin
  Result := VectorEquals(FCoords, AVector);
end;

// SetVector (affine)
//
procedure TDGLCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  DGLVectorMaths.SetVector(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetVector (TAffineVector)
//
procedure TDGLCustomCoordinates.SetVector(const V: TAffineVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  DGLVectorMaths.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (TVector)
//
procedure TDGLCustomCoordinates.SetVector(const V: TVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  DGLVectorMaths.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TDGLCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  DGLVectorMaths.SetVector(FCoords, X, Y, Z, W);
  NotifyChange(Self);
end;

// SetDirectVector
//
procedure TDGLCustomCoordinates.SetDirectCoordinate(const Index: Integer; const AValue: TGLFloat);
begin
  FCoords.V[index] := AValue;
end;

procedure TDGLCustomCoordinates.SetDirectVector(const V: TVector);
begin
  FCoords.V[0] := V.V[0];
  FCoords.V[1] := V.V[1];
  FCoords.V[2] := V.V[2];
  FCoords.V[3] := V.V[3];
end;

// SetToZero
//
procedure TDGLCustomCoordinates.SetToZero;
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
procedure TDGLCustomCoordinates.SetPoint(const X, Y, Z: Single);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetPoint (TAffineVector)
//
procedure TDGLCustomCoordinates.SetPoint(const V: TAffineVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint (TVector)
//
procedure TDGLCustomCoordinates.SetPoint(const V: TVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint2D
//
procedure TDGLCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  DGLVectorMaths.MakeVector(FCoords, X, Y, 0);
  NotifyChange(Self);
end;

// SetPoint2D (TAffineVector)
//
procedure TDGLCustomCoordinates.SetPoint2D(const Vector: TAffineVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

// SetPoint2D (TVector)
//
procedure TDGLCustomCoordinates.SetPoint2D(const Vector: TVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector);
  NotifyChange(Self);
end;

// SetPoint2D (TVector2f)
//
procedure TDGLCustomCoordinates.SetPoint2D(const Vector: TVector2f);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  MakeVector(FCoords, Vector.V[0], Vector.V[1], 0);
  NotifyChange(Self);
end;

// AsAddress
//
function TDGLCustomCoordinates.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//
procedure TDGLCustomCoordinates.SetAsVector(const Value: TVector);
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
procedure TDGLCustomCoordinates.SetAsAffineVector(const Value: TAffineVector);
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
procedure TDGLCustomCoordinates.SetAsPoint2D(const Value: TVector2f);
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
function TDGLCustomCoordinates.GetAsAffineVector: TAffineVector;
begin
  DGLVectorMaths.SetVector(Result, FCoords);
end;

// GetAsPoint2D
//
function TDGLCustomCoordinates.GetAsPoint2D: TVector2f;
begin
  Result.V[0] := FCoords.V[0];
  Result.V[1] := FCoords.V[1];
end;

// SetCoordinate
//
procedure TDGLCustomCoordinates.SetCoordinate(const AIndex: Integer; const AValue: TGLFloat);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

// GetCoordinate
//
function TDGLCustomCoordinates.GetCoordinate(const AIndex: Integer): TGLFloat;
begin
  Result := FCoords.V[AIndex];
end;

function TDGLCustomCoordinates.GetDirectCoordinate(const Index: Integer): TGLFloat;
begin
  Result := FCoords.V[index]
end;

// GetAsString
//
function TDGLCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.V[0], FCoords.V[1]]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.V[0], FCoords.V[1], FCoords.V[2]]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.V[0], FCoords.V[1], FCoords.V[2], FCoords.V[3]]);
  else
    Assert(False);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization

RegisterClasses([TDGLCoordinates2, TDGLCoordinates3, TDGLCoordinates4]);

end.
