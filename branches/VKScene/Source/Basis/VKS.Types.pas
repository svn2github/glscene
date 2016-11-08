//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Defines vector types for geometry only aiming to imply
   compatibility of GLScene for Delphi with C+Builder.
   Do not include any other units in uses clause
    
}
unit VKS.Types;

interface

type
  TVKBox = record
    ALeft, ATop, ANear, ARight, ABottom, AFar: Single;
  end;

//-----------------------
//Point types
//-----------------------
type

  PGLPoint2D = ^TVKPoint2D;
  TVKPoint2D = record
    X: Single;
    Y: Single;
    public
      function Create(X, Y : Single): TVKPoint2D;
      procedure SetPosition(const X, Y : Single);
      function Add(const APoint2D: TVKPoint2D): TVKPoint2D;
      function Length: Single; //distance to origin
      function Distance(const APoint2D : TVKPoint2D) : Single;
      procedure Offset(const ADeltaX, ADeltaY : Single);
  end;

  PGLPoint3D = ^TVKPoint3D;
  TVKPoint3D = record
    X: Single;
    Y: Single;
    Z: Single;
    public
      function Create(X, Y, Z: Single): TVKPoint3D;
      procedure SetPosition(const X, Y, Z : Single);
      function Add(const AGLPoint3D: TVKPoint3D): TVKPoint3D;
      function Length: Single; //distance to origin
      function Distance(const APoint3D : TVKPoint3D) : Single;
      procedure Offset(const ADeltaX, ADeltaY, ADeltaZ : Single);
  end;

  TVKPoint2DArray = array of TVKPoint2D;
  TVKPoint3DArray = array of TVKPoint3D;


//-----------------------
//Polygon types
//-----------------------
  TVKPolygon2D = TVKPoint2DArray;
  TVKPolygon3D = TVKPoint3DArray;

const
   ClosedPolygon2D: TVKPoint2D = (X: $FFFF; Y: $FFFF);
   ClosedPolygon3D: TVKPoint3D = (X: $FFFF; Y: $FFFF; Z: $FFFF);


//-----------------------
//Polyhedron types
//-----------------------
type
  TVKPolyhedron = array of TVKPolygon3D;

//-----------------------
// Vector types
//-----------------------
  TVKVector2DType = array [0..1] of Single;
  TVKVector3DType = array [0..2] of Single;

  TVKVector2D = record
    private
      function Norm: Single;
    public
      function Create(const AX, AY, AW : Single): TVKVector2D;
      function Add(const AVector2D: TVKVector2D): TVKVector2D;
      function Length: Single;
    case Integer of
      0: (V: TVKVector2DType;);
      1: (X: Single;
          Y: Single;
          W: Single;)
  end;

  TVKVector3D = record
    private
      function Norm: Single;
    public
      function Create(const AX, AY, AZ, AW : Single): TVKVector3D;
      function Add(const AVector3D: TVKVector3D): TVKVector3D;
      function Length: Single;
    case Integer of
      0: (V: TVKVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;)
  end;

//-----------------------
// Matrix types
//-----------------------
  TVKMatrix2DType = array[0..3] of TVKVector2D;
  {$NODEFINE TVKMatrix2DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVKVector2D TVKMatrix2DArray[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)
  TVKMatrix3DType = array[0..3] of TVKVector3D;
  {$NODEFINE TVKMatrix3DType}
  (*$HPPEMIT END OPENNAMESPACE*)
  (*$HPPEMIT END 'typedef TVKVector3D TVKMatrix3DType[4];'*)
  (*$HPPEMIT END CLOSENAMESPACE*)

  TVKMatrix2D = record
  private
  public
    case Integer of
      0: (M: TVKMatrix2DType;);
      1: (e11, e12, e13: Single;
          e21, e22, e23: Single;
          e31, e32, e33: Single);
  end;

  TVKMatrix3D = record
  private
  public
    case Integer of
      0: (M: TVKMatrix3DType;);
      1: (e11, e12, e13, e14: Single;
          e21, e22, e23, e24: Single;
          e31, e32, e33, e34: Single;
          e41, e42, e43, e44: Single);
  end;

  TVKMatrix2DArray = array of TVKMatrix2D;
  TVKMatrix3DArray = array of TVKMatrix3D;

type
   TVKMesh2DVertex = packed record
    X, Y: Single;
    NX, NY: Single;
    tU, tV: Single;
  end;

   TVKMesh3DVertex = packed record
    X, Y, Z: Single;
    NX, NY, NZ: Single;
    tU, tV: Single;
  end;

  TVKMesh2D = array of TVKMesh2DVertex;
  TVKMesh3D = array of TVKMesh3DVertex;


  TVKQuaternion3D = record
    ImPart: TVKVector3D;
    RePart: Single;
  end;

//---------------------------------------------------------------
//---------------------------------------------------------------
//---------------------------------------------------------------


implementation

{ TVKPoint2D }

function TVKPoint2D.Create(X, Y : Single): TVKPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TVKPoint2D.SetPosition(const X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TVKPoint2D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TVKPoint2D.Add(const APoint2D: TVKPoint2D): TVKPoint2D;
begin
  Result.SetPosition(Self.X + APoint2D.X, Self.Y + APoint2D.Y);
end;

function TVKPoint2D.Distance(const APoint2D: TVKPoint2D): Single;
begin
  Result := Sqrt(Sqr(Self.X - APoint2D.X) +  Sqr(Self.Y - APoint2D.Y));
end;

procedure TVKPoint2D.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

{ TVKPoint3D }

function TVKPoint3D.Create(X, Y, Z: Single): TVKPoint3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVKPoint3D.Add(const AGLPoint3D: TVKPoint3D): TVKPoint3D;
begin
  Result.X := Self.X + AGLPoint3D.X;
  Result.Y := Self.Y + AGLPoint3D.Y;
  Result.Z := Self.Z + AGLPoint3D.Z;
end;

function TVKPoint3D.Distance(const APoint3D: TVKPoint3D): Single;
begin
  Result := Self.Length - APoint3D.Length;
end;

function TVKPoint3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

procedure TVKPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
  Self.Z := Self.Z + ADeltaZ;
end;

procedure TVKPoint3D.SetPosition(const X, Y, Z: Single);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

{ TVKVector2D }

function TVKVector2D.Create(const AX, AY, AW: Single): TVKVector2D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

function TVKVector2D.Add(const AVector2D: TVKVector2D): TVKVector2D;
begin
  Result.X := Self.X + AVector2D.X;
  Result.Y := Self.Y + AVector2D.Y;
  Result.W := 1.0;
end;

function TVKVector2D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;

function TVKVector2D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

{ TVKVector3D }

function TVKVector3D.Create(const AX, AY, AZ, AW: Single): TVKVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

function TVKVector3D.Add(const AVector3D: TVKVector3D): TVKVector3D;
begin
  Result.X := Self.X + AVector3D.X;
  Result.Y := Self.Y + AVector3D.Y;
  Result.Z := Self.Z + AVector3D.Z;
  Result.W := 1.0;
end;

function TVKVector3D.Norm: Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y) + (Self.Z * Self.Z);
end;

function TVKVector3D.Length: Single;
begin
  Result := Sqrt(Self.Norm);
end;


end.