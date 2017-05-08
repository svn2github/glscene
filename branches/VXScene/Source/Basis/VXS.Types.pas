//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Defines vector types for geometry only aiming to imply
   compatibility of GLScene for Delphi with C+Builder.
   Do not include any other units in uses clause
   The history is logged in a former GLS version of the unit.    
}
unit VXS.Types;

interface

uses
  System.Types,
  VXS.VectorTypes;

//-----------------------
//Point types
//-----------------------
type

  TVXScalarValue = Single;
  TVXScalarField = function(X, Y, Z: Single): TVXScalarValue;

  // If data are made on integer XYZ index
  TVXScalarFieldInt = function(iX, iY, iZ: Integer): TVXScalarValue of object;

  TVXVertex = record
    P, N: TVector3f;  //Point and Normal
    Density: Single;
  end;
  PGLPoint2D = ^TVXPoint2D;
  TVXPoint2D = record
    X: Single;
    Y: Single;
    public
      function Create(X, Y : Single): TVXPoint2D;
      procedure SetPosition(const X, Y : Single);
      function Add(const APoint2D: TVXPoint2D): TVXPoint2D;
      function Length: Single; //distance to origin
      function Distance(const APoint2D : TVXPoint2D) : Single;
      procedure Offset(const ADeltaX, ADeltaY : Single);
  end;

  PGLPoint3D = ^TVXPoint3D;
  TVXPoint3D = record
    X: Single;
    Y: Single;
    Z: Single;
    public
      function Create(X, Y, Z: Single): TVXPoint3D;
      procedure SetPosition(const X, Y, Z : Single);
      function Add(const AGLPoint3D: TVXPoint3D): TVXPoint3D;
      function Length: Single; //distance to origin
      function Distance(const APoint3D : TVXPoint3D) : Single;
      procedure Offset(const ADeltaX, ADeltaY, ADeltaZ : Single);
  end;

  TVXPoint2DArray = array of TVXPoint2D;
  TVXPoint3DArray = array of TVXPoint3D;


//-----------------------
//Polygon types
//-----------------------
  TVXPolygon2D = TVXPoint2DArray;
  TVXPolygon3D = TVXPoint3DArray;

const
   ClosedPolygon2D: TVXPoint2D = (X: $FFFF; Y: $FFFF);
   ClosedPolygon3D: TVXPoint3D = (X: $FFFF; Y: $FFFF; Z: $FFFF);

// Triangle types
type
  TVXTriangle = record
    v1, v2, v3: Integer;
  end;

  TVXTriangleArray = array [0 .. (MaxInt shr 8)] of TVXTriangle;
  PVKTriangleArray = ^TVXTriangleArray;

  TVXVertexArray = array [0 .. (MaxInt shr 8)] of TVXVertex;
  PVKVertexArray = ^TVXVertexArray;


// Voxel types
  TVXVoxelStatus = (bpExternal, bpInternal);
  TVXVoxel = record
    P: TVector3f;
    Density: TVXScalarValue;
    Status: TVXVoxelStatus;
  end;
  PVKVoxel = ^TVXVoxel;

  TVXVoxelData = array [0 .. (MaxInt shr 8)] of TVXVoxel;
  PVKVoxelData = ^TVXVoxelData;


  TVXBox = record
    ALeft, ATop, ANear, ARight, ABottom, AFar: Single;
  end;


//-----------------------
//Polyhedron types
//-----------------------
type
  TVXPolyhedron = array of TVXPolygon3D;

//-----------------------
// Vector types
//-----------------------
  TVXVector2DType = array [0..1] of Single;
  TVXVector3DType = array [0..2] of Single;

  TVXVector2D = record
    private
      function Norm: Single;
    public
      function Create(const AX, AY, AW : Single): TVXVector2D;
      function Add(const AVector2D: TVXVector2D): TVXVector2D;
      function Length: Single;
    case Integer of
      0: (V: TVXVector2DType;);
      1: (X: Single;
          Y: Single;
          W: Single;)
  end;

  TVXVector3D = record
    private
      function Norm: Single;
    public
      function Create(const AX, AY, AZ, AW : Single): TVXVector3D;
      function Add(const AVector3D: TVXVector3D): TVXVector3D;
      function Length: Single;
    case Integer of
      0: (V: TVXVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;)
  end;

//-----------------------
// Matrix types
//-----------------------
  TVXMatrix2DType = array[0..3] of TVXVector2D;
  {$NODEFINE TVXMatrix2DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVXVector2D TVXMatrix2DArray[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TVXMatrix3DType = array[0..3] of TVXVector3D;
  {$NODEFINE TVXMatrix3DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVXVector3D TVXMatrix3DType[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)

  TVXMatrix2D = record
  private
  public
    case Integer of
      0: (M: TVXMatrix2DType;);
      1: (e11, e12, e13: Single;
          e21, e22, e23: Single;
          e31, e32, e33: Single);
  end;

  TVXMatrix3D = record
  private
  public
    case Integer of
      0: (M: TVXMatrix3DType;);
      1: (e11, e12, e13, e14: Single;
          e21, e22, e23, e24: Single;
          e31, e32, e33, e34: Single;
          e41, e42, e43, e44: Single);
  end;

  TVXMatrix2DArray = array of TVXMatrix2D;
  TVXMatrix3DArray = array of TVXMatrix3D;

type
   TVXMesh2DVertex = packed record
    X, Y: Single;
    NX, NY: Single;
    tU, tV: Single;
  end;

   TVXMesh3DVertex = packed record
    X, Y, Z: Single;
    NX, NY, NZ: Single;
    tU, tV: Single;
  end;

  TVXMesh2D = array of TVXMesh2DVertex;
  TVXMesh3D = array of TVXMesh3DVertex;


  TVXQuaternion3D = record
    ImPart: TVXVector3D;
    RePart: Single;
  end;

//---------------------------------------------------------------
//---------------------------------------------------------------
//---------------------------------------------------------------


implementation

{ TVXPoint2D }

function TVXPoint2D.Create(X, Y : Single): TVXPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TVXPoint2D.SetPosition(const X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TVXPoint2D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TVXPoint2D.Add(const APoint2D: TVXPoint2D): TVXPoint2D;
begin
  Result.SetPosition(Self.X + APoint2D.X, Self.Y + APoint2D.Y);
end;

function TVXPoint2D.Distance(const APoint2D: TVXPoint2D): Single;
begin
  Result := Sqrt(Sqr(Self.X - APoint2D.X) +  Sqr(Self.Y - APoint2D.Y));
end;

procedure TVXPoint2D.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

{ TVXPoint3D }

function TVXPoint3D.Create(X, Y, Z: Single): TVXPoint3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVXPoint3D.Add(const AGLPoint3D: TVXPoint3D): TVXPoint3D;
begin
  Result.X := Self.X + AGLPoint3D.X;
  Result.Y := Self.Y + AGLPoint3D.Y;
  Result.Z := Self.Z + AGLPoint3D.Z;
end;

function TVXPoint3D.Distance(const APoint3D: TVXPoint3D): Single;
begin
  Result := Self.Length - APoint3D.Length;
end;

function TVXPoint3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

procedure TVXPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
  Self.Z := Self.Z + ADeltaZ;
end;

procedure TVXPoint3D.SetPosition(const X, Y, Z: Single);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

{ TVXVector2D }

function TVXVector2D.Create(const AX, AY, AW: Single): TVXVector2D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

function TVXVector2D.Add(const AVector2D: TVXVector2D): TVXVector2D;
begin
  Result.X := Self.X + AVector2D.X;
  Result.Y := Self.Y + AVector2D.Y;
  Result.W := 1.0;
end;

function TVXVector2D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;

function TVXVector2D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

{ TVXVector3D }

function TVXVector3D.Create(const AX, AY, AZ, AW: Single): TVXVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

function TVXVector3D.Add(const AVector3D: TVXVector3D): TVXVector3D;
begin
  Result.X := Self.X + AVector3D.X;
  Result.Y := Self.Y + AVector3D.Y;
  Result.Z := Self.Z + AVector3D.Z;
  Result.W := 1.0;
end;

function TVXVector3D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y) + (Self.Z * Self.Z);
end;

function TVXVector3D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;


end.