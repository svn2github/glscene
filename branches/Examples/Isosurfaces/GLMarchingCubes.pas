{: GLMarchingCubes<p>

  Marching Cubes implementation based on a Thomas Lewiner algorithm

	<b>History : </b><font size=-1><ul>
	   <li>15/03/16 - RB - Creation
	</ul></font>
}


unit GLMarchingCubes;


// uncomment next line to memorize vertex Density value to further use
// (i.e. mesh color generation)
{.$Define UseDensity}

interface

uses
  GLVectorGeometry,
  GLMesh,
  GLVectorFileObjects;

const
  ALLOC_SIZE = 65536;

type

  TScalarValue = Single;// SmallInt;
  TScalarField = function (X, Y, Z:Single):TScalarValue;

  // more efficient, if data are made on integer XYZ index
  TScalarFieldInt = function (iX, iY, iZ:Integer):TScalarValue of object;

  TMCVertex = record
    P, N: TAffineVector;
    {$IfDef UseDensity}
    Density:TScalarValue;
    {$EndIf}
  end;

  TMCTriangle = record
    v1, v2, v3:Integer;
  end;

  TMCTriangleArray= array[0..(MaxInt shr 8)] of TMCTriangle;
  PMCTriangleArray=^TMCTriangleArray;

  TMCVertexArray = array[0..(MaxInt shr 8)] of TMCVertex;
  PMCVertexArray = ^TMCVertexArray;

  TMCVoxelStatus = (bpExternal, bpInternal);

  TMCVoxel = record
    P:TAffineVector;
    Density:TScalarValue;
    Status:TMCVoxelStatus;
  end;

  PMCVoxel = ^TMCVoxel;

  TMCVoxelData = array[0..(MaxInt shr 8)] of TMCVoxel;
  PMCVoxelData = ^TMCVoxelData;

  TMarchingCube = class(TObject)
  private
    FIsoValue:TScalarValue;
    // sliceSize:Longword;

    _x_verts:PIntegerArray;
    _y_verts:PIntegerArray;
    _z_verts:PIntegerArray;

    _nverts:Integer;
    _ntrigs:Integer;
    _Sverts:Integer;
    _Strigs:Integer;

    _vertices:PMCVertexArray;
    _triangles:PMCTriangleArray;

    _i, _j, _k:Longword;

    _cube:array[0..7] of TMCVoxel;
    _lut_entry:Byte;
    // _case:Byte;
    // _config:Byte;
    // _subconfig:Byte;


    procedure init_temps;
    procedure init_all;
    procedure init_space;

    procedure clean_temps;
    procedure clean_all(keepFacets:Boolean=False);
    procedure clean_space;
    procedure test_vertex_addiction;

  protected
    FOriginalMC:Boolean; // now only original MC is implemented
    FSizeX:Integer;
    FSizeY:Integer;
    FSizeZ:Integer;
    FxMin: Single;
    FxMax: Single;
    FyMin: Single;
    FyMax: Single;
    FzMin: Single;
    FzMax: Single;
    FStepX:Single;
    FStepY:Single;
    FStepZ:Single;

    VoxelData:PMCVoxelData;

    procedure process_cube;
{    function test_face(face:byte):Boolean;
    function test_interior(s:Byte):boolean}

    procedure compute_intersection_points;
    procedure add_triangle(trig: array of Integer; n:Byte; v12:Integer=-1);

    function add_x_vertex:Integer;
    function add_y_vertex:Integer;
    function add_z_vertex:Integer;
    function add_c_vertex:Integer;

    function get_x_grad(i,j,k:Integer):Single;
    function get_y_grad(i,j,k:Integer):Single;
    function get_z_grad(i,j,k:Integer):Single;

    function get_x_vert(i,j,k:Integer):Integer;
    function get_y_vert(i,j,k:Integer):Integer;
    function get_z_vert(i,j,k:Integer):Integer;

    procedure set_x_vert(a_val,i,j,k:Integer);
    procedure set_y_vert(a_val,i,j,k:Integer);
    procedure set_z_vert(a_val,i,j,k:Integer);

    function getVoxelValue(i, j, k:Integer):TScalarValue;
    procedure setVoxelValue(i, j, k:Integer; HfValue:TScalarValue);

    function getVoxelData(i, j, k:Integer):TMCVoxel;
    function voxel(i, j, k:Integer):PMCVoxel;

    function calc_u(v1, v2:Single):Single; virtual;
  public
    ScalarField:TScalarField;

    constructor Create; overload; virtual;
    constructor Create(SizeX, SizeY, SizeZ:Integer;
      AIsoValue:TScalarValue=0.0;
      xMin:Single=-0.5;   xMax:Single= 0.5;
      yMin:Single=-0.5;   yMax:Single= 0.5;
      zMin:Single=-0.5;   zMax:Single= 0.5); overload; virtual;

    procedure ReDim(ASizeX, ASizeY, ASizeZ:Integer;
      xMin, xMax, yMin, yMax, zMin, zMax:Single); virtual;

    destructor Destroy; override;

    procedure Run; overload;
    procedure Run(IsoValue:TScalarValue);  overload;

    function Internal(AValue:TScalarValue):Boolean; virtual;

    procedure FillVoxelData; overload; virtual;
    procedure FillVoxelData(AIsoValue:TScalarValue; AScalarField:TScalarField=nil); overload; virtual;
    procedure FillVoxelData(AIsoValue:TScalarValue; AScalarField:TScalarFieldInt); overload; virtual;

    procedure CalcVertices(Vertices:TGLVertexList; Alpha:Single=1);
    procedure CalcMeshObject(AMeshObject:TMeshObject; Alpha:Single=1);

    property IsoValue:TScalarValue read FIsoValue write FIsoValue; // TODO SetIsoValue che chiama in automatico la Run

  end;


// Some scalar functions; only for algorithm tests

//Sphere surface
function SFSphere(x,y,z:Single):TScalarValue;
// Minkowski space (http://mathworld.wolfram.com)
function SFMinkowski(x,y,z:Single):TScalarValue;
// Klein Bottle (http://mathworld.wolfram.com)
function SFKleinBottle(x,y,z:Single):TScalarValue;
//Chmutov-surface-1 (http://mathworld.wolfram.com)
function SFChmutov1(x,y,z:Single):TScalarValue;
//Chmutov-surface-2 (http://mathworld.wolfram.com)
function SFChmutov2(x,y,z:Single):TScalarValue;
//Toroidal surface (phantasy!)
function SFToroidal(x,y,z:Single):TScalarValue;
//Double torus Surface (phantasy!)
function SFDoubleTorus(x,y,z:Single):TScalarValue;



const
  DemoScalarField :array[0..6] of record
    //xMin, xMax, yMin, yMax, zMin, zMax:Single; // default -0.5..0.5
    ScalarField:TScalarField;
    IsoValue:TScalarValue
  end =
  ((ScalarField:SFSphere;       IsoValue:0.3),
   (ScalarField:SFMinkowski;    IsoValue:0.0),
   (ScalarField:SFKleinBottle;  IsoValue:0.0),
   (ScalarField:SFChmutov1;     IsoValue:3.0),
   (ScalarField:SFChmutov2;     IsoValue:3.0),
   (ScalarField:SFToroidal;     IsoValue:3.0),
   (ScalarField:SFDoubleTorus;  IsoValue:0.015));


implementation

uses SysUtils;


{$I MCLookUpTable.inc}

function SFSphere(x,y,z:Single):TScalarValue;
begin
  Result:= sqr(x)+sqr(y)+sqr(z)
end;

function SFToroidal(x,y,z:Single):TScalarValue;
const  FScale= 7; a= 2.5;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:= (sqr(sqrt(sqr(X)+sqr(Y))-a)+sqr(Z))*
           (sqr(sqrt(sqr(Y)+sqr(Z))-a)+sqr(X))*
           (sqr(sqrt(sqr(Z)+sqr(X))-a)+sqr(Y));
end;

function SFDoubleTorus(x,y,z:Single):TScalarValue;
const FScale = 2.25;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:= IntPower(X,8)+IntPower(X,4)-2*IntPower(X,6)-2*sqr(X)*sqr(Y)+2*IntPower(X,4)*sqr(Y)+IntPower(Y,4)+sqr(Z)
end;

function SFChmutov1(x,y,z:Single):TScalarValue;
const FScale = 2.5;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:=  8*(sqr(X)+sqr(Y)+sqr(Z))-8*(IntPower(X,4)+IntPower(Y,4)+IntPower(Z,4));
end;

function SFChmutov2(x,y,z:Single):TScalarValue;
const FScale = 2.5;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:=   2*(sqr(X)*sqr(3-4*sqr(X))+sqr(Y)*sqr(3-4*sqr(Y))+sqr(Z)*sqr(3-4*sqr(Z)) );
end;

function SFKleinBottle(x,y,z:Single):TScalarValue;
const FScale = 7.5;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:= (sqr(X)+sqr(Y)+sqr(Z)+2*Y-1)*
           (sqr(sqr(X)+sqr(Y)+sqr(Z)-2*Y-1)-8*sqr(Z))+
           16*X*Z*(sqr(X)+sqr(Y)+sqr(Z)-2*Y-1);
end;

function SFMinkowski(x,y,z:Single):TScalarValue;
const FScale = 7;
begin
  X:= FScale*X;
  Y:= FScale*Y;
  Z:= FScale*Z;
  Result:=(sqr(X)-sqr(Y)-sqr(Z)-2)*(sqr(X)-sqr(Y)-sqr(Z)+2)*
          (sqr(X)-sqr(Y)-sqr(Z)-4)*(sqr(X)-sqr(Y)-sqr(Z)+4)*
          (sqr(X)-sqr(Y)-sqr(Z));
end;



{ TMarchingCube }

function TMarchingCube.add_c_vertex: Integer;
var
  u:Single;
  vid:Integer;
  procedure VertexAdd(iv:Integer);
  begin
    with _vertices^[_nverts] do begin
      u:= u+1;
      P:= VectorAdd(P, _vertices[iv].P);
      N:= VectorAdd(N, _vertices[iv].N);
      {$IfDef UseDensity}
      Density:= Density + _vertices[iv].Density;
      {$EndIf}
    end
  end;

begin
  test_vertex_addiction;

  u:=0;
  with _vertices^[_nverts] do begin
    P:= NullVector;
    N:= NullVector;
    {$IfDef UseDensity}
    Density:= 0;
    {$EndIf}
  end;
  Inc(_nverts);

  // Computes the average of the intersection points of the cube
  vid:= get_x_vert( _i , _j , _k ) ;
  if ( vid <> -1 ) then VertexAdd(vid);
  vid:= get_y_vert(_i+1, _j , _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_x_vert( _i ,_j+1, _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_y_vert( _i , _j , _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_x_vert( _i , _j ,_k+1) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_y_vert(_i+1, _j ,_k+1) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_x_vert( _i ,_j+1,_k+1) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_y_vert( _i , _j ,_k+1) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_z_vert( _i , _j , _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_z_vert(_i+1, _j , _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_z_vert(_i+1,_j+1, _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);
  vid:= get_z_vert( _i ,_j+1, _k ) ;
  if( vid <> -1 )  then VertexAdd(vid);

  ScaleVector(_vertices^[_nverts].P, 1/u);
  NormalizeVector(_vertices^[_nverts].N);
  {$IfDef UseDensity}
  _vertices^[_nverts].Density:= _vertices^[_nverts].Density/u;
  {$EndIf}
	Result:= _nverts-1;
end;

procedure TMarchingCube.add_triangle(trig: array of Integer; n: Byte;
  v12: Integer=-1);

var
  tv:array[0..2]of Integer;
  t, tmod3:Integer;

begin

  for t:= 0 to 3*n-1 do begin
    tmod3:=  t mod 3;
    case trig[t] of
       0 : tv[tmod3]:= get_x_vert( _i , _j , _k );
       1 : tv[tmod3]:= get_y_vert(_i+1, _j , _k );
       2 : tv[tmod3]:= get_x_vert( _i ,_j+1, _k );
       3 : tv[tmod3]:= get_y_vert( _i , _j , _k );
       4 : tv[tmod3]:= get_x_vert( _i , _j ,_k+1);
       5 : tv[tmod3]:= get_y_vert(_i+1, _j ,_k+1);
       6 : tv[tmod3]:= get_x_vert( _i ,_j+1,_k+1);
       7 : tv[tmod3]:= get_y_vert( _i , _j ,_k+1);
       8 : tv[tmod3]:= get_z_vert( _i , _j , _k );
       9 : tv[tmod3]:= get_z_vert(_i+1, _j , _k );
      10 : tv[tmod3]:= get_z_vert(_i+1,_j+1, _k );
      11 : tv[tmod3]:= get_z_vert( _i ,_j+1, _k );
      12 : tv[tmod3]:= v12
    end;

    if( tv[tmod3] = -1 ) then Break ;

    if( tmod3 = 2 ) then begin
      if( _ntrigs >= _Strigs ) then begin
        _Strigs:= 2*_Strigs;
        ReallocMem(_triangles, _Strigs*SizeOf(TMCTriangle)) ;
      end;

      with _triangles^[_ntrigs] do begin
        v1:= tv[0] ;
        v2:= tv[1] ;
        v3:= tv[2] ;
      end;
      Inc(_ntrigs);

    end
  end
end;

function TMarchingCube.calc_u(v1, v2: Single): Single;
begin
  if (abs(FIsoValue-v1)>=0.00001) then Result:=1
  else if(abs(FIsoValue-v2)>=0.00001) then Result:=0
  else if(abs(v1-v2)>=0.00001) then Result:=(FIsoValue-v1)/(v2-v1)
  else Result:=0.5
end;

function TMarchingCube.add_x_vertex: Integer;
var u:Single;
begin
  test_vertex_addiction;
  u:= calc_u(_cube[0].Density, _cube[1].Density);

  with _vertices^[_nverts] do begin
    P.X:= _cube[0].P.X+u*FStepX;
    P.Y:= _cube[0].P.Y;
    P.Z:= _cube[0].P.Z;

    N.X:= (1-u)*get_x_grad(_i,_j,_k) + u*get_x_grad(_i+1,_j,_k);
    N.Y:= (1-u)*get_y_grad(_i,_j,_k) + u*get_y_grad(_i+1,_j,_k) ;
    N.Z:= (1-u)*get_z_grad(_i,_j,_k) + u*get_z_grad(_i+1,_j,_k) ;
    NormalizeVector(N);
    {$IfDef UseDensity}
    Density:= _cube[1].Density
    {$EndIf}
  end;
  Inc(_nverts);
	Result:= _nverts-1;
end;

function TMarchingCube.add_y_vertex: Integer;
var u:Single;
begin
  test_vertex_addiction;
  u:= calc_u(_cube[0].Density, _cube[3].Density);

  with _vertices^[_nverts] do begin
    P.X:= _cube[0].P.X;
    P.Y:= _cube[0].P.Y+u*FStepY;
    P.Z:= _cube[0].P.Z;

    N.X:= (1-u)*get_x_grad(_i,_j,_k) + u*get_x_grad(_i,_j+1,_k);
    N.Y:= (1-u)*get_y_grad(_i,_j,_k) + u*get_y_grad(_i,_j+1,_k) ;
    N.Z:= (1-u)*get_z_grad(_i,_j,_k) + u*get_z_grad(_i,_j+1,_k) ;
    NormalizeVector(N);
    {$IfDef UseDensity}
    Density:= _cube[3].Density
    {$EndIf}
  end;
  Inc(_nverts);
	Result:= _nverts-1;
end;

function TMarchingCube.add_z_vertex: Integer;
var u:Single;
begin
  test_vertex_addiction;

  u:= calc_u(_cube[0].Density, _cube[4].Density);

  with _vertices^[_nverts] do begin
    P.X:= _cube[0].P.X;
    P.Y:= _cube[0].P.Y;
    P.Z:= _cube[0].P.Z+u*FStepZ;;

    N.X:= (1-u)*get_x_grad(_i,_j,_k) + u*get_x_grad(_i,_j,_k+1);
    N.Y:= (1-u)*get_y_grad(_i,_j,_k) + u*get_y_grad(_i,_j,_k+1) ;
    N.Z:= (1-u)*get_z_grad(_i,_j,_k) + u*get_z_grad(_i,_j,_k+1) ;
    NormalizeVector(N);
    {$IfDef UseDensity}
    Density:= _cube[4].Density
    {$EndIf}
  end;
  Inc(_nverts);
	Result:= _nverts-1;
end;

procedure TMarchingCube.clean_all(keepFacets:Boolean=False);
begin
	clean_temps;
	clean_space;
	if (not keepFacets) then
		FreeMem(_vertices);
		FreeMem(_triangles);
		_vertices:= nil;
		_triangles:= nil;
		_nverts:= 0;
    _ntrigs:= 0;
		_Sverts:= 0;
    _Strigs:= 0;
end;

procedure TMarchingCube.clean_space;
begin
	if (VoxelData <> nil) then begin
    FreeMem(VoxelData);
    VoxelData:= nil
	end;
	FSizeX:= 0;
  FSizeY:= 0;
  FSizeZ:= 0
end;

procedure TMarchingCube.clean_temps;
begin
	FreeMem(_x_verts); 	_x_verts:= nil;
	FreeMem(_y_verts);  _y_verts:= nil;
	FreeMem(_z_verts);  _z_verts:= nil;
end;

procedure TMarchingCube.compute_intersection_points;
var k, j, i:Integer;
begin
	_cube[0]:= getVoxelData( 0, 0, 0 ) ;
  _cube[1]:= getVoxelData( 1, 0, 0 ) ;
  _cube[3]:= getVoxelData( 0, 1, 0 ) ;
  _cube[4]:= getVoxelData( 0, 0, 1 ) ;
{  _step_x:= _cube[1].P[0] - _cube[0].P[0] ;
  _step_y:= _cube[3].P[1] - _cube[0].P[1] ;
  _step_z:= _cube[4].P[2] - _cube[0].P[2] ;}

	for k:= 0 to FSizeZ-2 do begin
    _k:= k;
    for j:= 0 to FSizeY-2 do begin
      _j:= j;
      for i:= 0 to FSizeX-2 do begin
        _i:= i;
        _cube[0]:= getVoxelData( _i, _j, _k ) ;
        _cube[1]:= getVoxelData(_i+1, _j , _k ) ;
        _cube[3]:= getVoxelData( _i ,_j+1, _k ) ;
        _cube[4]:= getVoxelData( _i , _j ,_k+1) ;

        if(internal(_cube[0].Density)) then begin
          if( not internal(_cube[1].Density) ) then set_x_vert( add_x_vertex( ), _i,_j,_k ) ;
          if( not internal(_cube[3].Density) ) then set_y_vert( add_y_vertex( ), _i,_j,_k ) ;
          if( not internal(_cube[4].Density) ) then set_z_vert( add_z_vertex( ), _i,_j,_k ) ;
        end else begin
          if( internal(_cube[1].Density) ) then set_x_vert( add_x_vertex( ), _i,_j,_k ) ;
          if( internal(_cube[3].Density) ) then set_y_vert( add_y_vertex( ), _i,_j,_k ) ;
          if( internal(_cube[4].Density) ) then set_z_vert( add_z_vertex( ), _i,_j,_k ) ;
        end
      end
    end
  end
end;

procedure TMarchingCube.ReDim(ASizeX, ASizeY, ASizeZ: Integer; xMin, xMax,
  yMin, yMax, zMin, zMax: Single);
begin
  clean_all;
  FSizeX:= ASizeX;
	FSizeY:= ASizeY;
  FSizeZ:= ASizeZ;
  FxMin:= xMin;
  FxMax:= xMax;
  FyMin:= yMin;
  FyMax:= yMax;
  FzMin:= zMin;
  FzMax:= zMax;

  FStepX:=(FxMax-FxMin)/(FSizeX-1);
  FStepY:=(FyMax-FyMin)/(FSizeY-1);
  FStepZ:=(FzMax-FzMin)/(FSizeZ-1);

  VoxelData:= nil;
  _x_verts:= nil;
  _y_verts:= nil;
  _z_verts:= nil;
  _nverts:= 0;
  _ntrigs:= 0;
  _Sverts:= 0;
  _Strigs:= 0;
  _vertices:= nil;
  _triangles:= nil;

  init_all;
//  FillVoxelData;
end;

constructor TMarchingCube.Create;
begin
  FOriginalMC:= True; // now only original MC is implemented
  FIsoValue:= 0;
  ScalarField:= nil;// SFSphere;//SFKleinBottle;//SFMinkowski;// SFChmutov2;// SFChmutov1;//SFDoubleTorus;// SFToroidal;

  VoxelData:= nil;
  _x_verts:= nil;
  _y_verts:= nil;
  _z_verts:= nil;
  _nverts:= 0;
  _ntrigs:= 0;
  _Sverts:= 0;
  _Strigs:= 0;
  _vertices:= nil;
  _triangles:= nil;
end;


constructor TMarchingCube.Create(
  SizeX, SizeY, SizeZ:Integer;
  AIsoValue:TScalarValue=0.0;
  xMin:Single=-0.5;   xMax:Single= 0.5;
  yMin:Single=-0.5;   yMax:Single= 0.5;
  zMin:Single=-0.5;   zMax:Single= 0.5);
begin
  FOriginalMC:= True;  // now only original MC is implemented
  FIsoValue:= AIsoValue;
  ScalarField:= SFSphere;//SFKleinBottle;//SFMinkowski;// SFChmutov2;// SFChmutov1;//SFDoubleTorus;// SFToroidal;

  ReDim(SizeX, SizeY, SizeZ, xMin, xMax, yMin, yMax, zMin, zMax);
  FillVoxelData;
end;

destructor TMarchingCube.Destroy;
begin
  clean_all;
  inherited;
end;

function TMarchingCube.getVoxelValue(i, j, k: Integer): TScalarValue;
begin
  Result:= VoxelData^[i + j*FSizeX + k*FSizeX*FSizeY].Density
end;

function TMarchingCube.getVoxelData(i, j, k: Integer): TMCVoxel;
begin
  Result:= VoxelData^[i + j*FSizeX + k*FSizeX*FSizeY]
end;

function TMarchingCube.get_x_grad(i, j, k: Integer): Single;
begin
  if ( i > 0 ) then
    if ( i < FSizeX - 1 ) then Result:= ( getVoxelValue( i+1, j, k ) - getVoxelValue( i-1, j, k ) ) / 2
    else Result:= getVoxelValue( i, j, k ) - getVoxelValue( i-1, j, k )
  else Result:= getVoxelValue( i+1, j, k ) - getVoxelValue( i, j, k )
end;

function TMarchingCube.get_x_vert(i, j, k: Integer): Integer;
begin
  Result:= _x_verts^[ i + j*FSizeX + k*FSizeX*FSizeY]
end;

function TMarchingCube.get_y_grad(i, j, k: Integer): Single;
begin
  if( j > 0 ) then
    if ( j < FSizeY - 1 ) then Result:= ( getVoxelValue( i, j+1, k ) - getVoxelValue( i, j-1, k ) ) / 2
    else Result:= getVoxelValue( i, j, k ) - getVoxelValue( i, j-1, k )
  else Result:= getVoxelValue( i, j+1, k ) - getVoxelValue( i, j, k )
end;

function TMarchingCube.get_y_vert(i, j, k: Integer): Integer;
begin
  Result:= _y_verts^[ i + j*FSizeX + k*FSizeX*FSizeY]
end;

function TMarchingCube.get_z_grad(i, j, k: Integer): Single;
begin
  if( k > 0 ) then
    if (k < FSizeZ - 1 ) then Result:= ( getVoxelValue( i, j, k+1 ) - getVoxelValue( i, j, k-1 ) ) / 2
    else Result:= getVoxelValue( i, j, k ) - getVoxelValue( i, j, k-1 )
  else Result:= getVoxelValue( i, j, k+1 ) - getVoxelValue( i, j, k )
end;

function TMarchingCube.get_z_vert(i, j, k: Integer): Integer;
begin
  Result:= _z_verts^[ i + j*FSizeX + k*FSizeX*FSizeY]
end;

procedure TMarchingCube.init_all;
begin
	init_temps;
	init_space;

	if (_vertices <> Nil) then FreeMem(_vertices);
	if (_triangles <> nil) then FreeMem(_triangles);
	_nverts:= 0;
  _ntrigs:= 0;
	_Sverts:= ALLOC_SIZE;
  _Strigs:= ALLOC_SIZE;

  GetMem(_vertices, _Sverts*SizeOf(TMCVertex));
	GetMem(_triangles, _Strigs*SizeOf(TMCTriangle));
end;

procedure TMarchingCube.init_space;
begin
  VoxelData:= AllocMem(FSizeX * FSizeY * FSizeZ* SizeOf(TMCVoxel));
end;

procedure TMarchingCube.init_temps;
var spaceSize:Longword;
begin
	spaceSize:= FSizeX * FSizeY * FSizeZ;

  GetMem(_x_verts, spaceSize*SizeOf(Integer));
  GetMem(_y_verts, spaceSize*SizeOf(Integer));
  GetMem(_z_verts, spaceSize*SizeOf(Integer));

  FillChar(_x_verts^, spaceSize*SizeOf(Integer), -1);
  FillChar(_y_verts^, spaceSize*SizeOf(Integer), -1);
  FillChar(_z_verts^, spaceSize*SizeOf(Integer), -1);
end;

function TMarchingCube.Internal(AValue: TScalarValue): Boolean;
begin
  Result:= AValue<=FIsoValue
end;

procedure TMarchingCube.process_cube;
var nt:Byte;
begin
  if(FOriginalMC) then begin
    nt:= 0 ;
    while (casesClassic[_lut_entry][3*nt] <> -1) do Inc(nt);
    add_triangle( casesClassic[_lut_entry], nt) ;
    Exit;
  end;

  {

  TODO complete algorithm with various tiling...

  }

end;

procedure TMarchingCube.Run;
var i, j, k, p:Integer;
begin
	if (_x_verts = Nil) then begin
    init_temps;
		_nverts:= 0;
    _ntrigs:= 0;
  end;
	compute_intersection_points;
	for k:= 0 to FSizeZ-2 do begin
    _k:= k;
    for j:= 0 to FSizeY-2 do begin
      _j:=j;
      for i:= 0 to FSizeX-2 do begin
        _i:= i;
        _lut_entry:= 0 ;
        for p:= 0 to 7 do begin
          _cube[p]:= getVoxelData( i+((p xor (p shr 1)) and 1), j+((p shr 1) and 1), k+((p shr 2)and 1));
          //_cube[p]:= getVoxelData( i+((p^(p>>1))&1), j+((p>>1)&1), k+((p>>2)&1) ) ;
          if (internal(_cube[p].density)) then _lut_entry := _lut_entry or (1 shl p);
        end;
        process_cube;
      end
    end
  end;
	clean_temps;
end;

procedure TMarchingCube.Run(IsoValue:TScalarValue);
begin
  FIsoValue:= IsoValue;
  Run
end;

procedure TMarchingCube.setVoxelValue(i, j, k: Integer; HfValue: TScalarValue);
begin
  VoxelData^[i + j*FSizeX + k*FSizeX*FSizeY].Density:= HfValue
end;

procedure TMarchingCube.set_x_vert(a_val,i,j,k: Integer);
begin
  _x_verts^[i + j*FSizeX + k*FSizeX*FSizeY]:= a_val
end;

procedure TMarchingCube.set_y_vert(a_val,i,j,k: Integer);
begin
  _y_verts^[i + j*FSizeX + k*FSizeX*FSizeY]:= a_val
end;

procedure TMarchingCube.set_z_vert(a_val,i,j,k: Integer);
begin
  _z_verts^[i + j*FSizeX + k*FSizeX*FSizeY]:= a_val
end;

procedure TMarchingCube.test_vertex_addiction;
begin
  if _nverts>= _Sverts then begin
    _Sverts:= 2*_Sverts;
    ReallocMem(_vertices, _Sverts*SizeOf(TMCVertex))
  end;
end;

function TMarchingCube.voxel(i, j, k:Integer): PMCVoxel;
begin
  if (k >= FSizeZ) or (j >= FSizeY) or ( i >= FSizeX) then Result:= nil
  else Result:= @VoxelData^[i + j*FSizeX + k*FSizeX*FSizeY]
end;

procedure TMarchingCube.FillVoxelData;
var ix, iy, iz:Integer;
  X, Y, Z:Single;
begin
  for ix:=0 to FSizeX-1 do begin
    X:= FxMin+ix*FStepX;
    for iy:=0 to FSizeY-1 do begin
      Y:= FyMin+iy*FStepY;
      for iz:=0 to FSizeZ-1 do
      with VoxelData^[ix + iy*FSizeX + iz*FSizeX*FSizeY] do begin
        Z:= FzMin+iz*FStepZ;
        MakeVector(P, X, Y, Z);
        Density:= ScalarField(X, Y, Z);
        if Internal(Density) then Status:= bpInternal
        else Status:= bpExternal
      end;
    end;
  end;
end;

procedure TMarchingCube.FillVoxelData(AIsoValue:TScalarValue;
  AScalarField: TScalarField=nil);
begin
  FIsoValue:= AIsoValue;
  if Assigned(AScalarField) then ScalarField:= AScalarField;
  FillVoxelData;
end;

procedure TMarchingCube.FillVoxelData(AIsoValue:TScalarValue; AScalarField:TScalarFieldInt);
var ix, iy, iz:Integer;
  X, Y, Z:Single;
begin
  FIsoValue:= AIsoValue;
  for ix:=0 to FSizeX-1 do begin
    X:= FxMin+ix*FStepX;
    for iy:=0 to FSizeY-1 do begin
      Y:= FyMin+iy*FStepY;
      for iz:=0 to FSizeZ-1 do
      with VoxelData^[ix + iy*FSizeX + iz*FSizeX*FSizeY] do begin
        Z:= FzMin+iz*FStepZ;
        MakeVector(P, X, Y, Z);
        Density:= AScalarField(ix, iy, iz);
        if Internal(Density) then Status:= bpInternal
        else Status:= bpExternal
      end;
    end;
  end;
end;


procedure TMarchingCube.CalcVertices(Vertices: TGLVertexList; Alpha:Single=1);
var i:Integer;
  vx1, vx2, vx3:TGLVertexData;

  function GetNrmColor(Nrm:TAffineVector):TVector;
  begin
    Result.V[0]:= 0;
    if Nrm.V[0] > 0.0 then Result.V[0]:= Result.V[0] + Nrm.V[0];
    if Nrm.V[1] < 0.0 then Result.V[0]:= Result.V[0]-0.5*Nrm.V[1];
    if Nrm.V[2] < 0.0 then Result.V[0]:= Result.V[0]-0.5*Nrm.V[2];

    Result.V[1]:= 1;
    if Nrm.V[0] < 0.0 then Result.V[1]:= Result.V[1]-0.5*Nrm.V[0];
    if Nrm.V[1] > 0.0 then Result.V[1]:= Result.V[1] + Nrm.V[1];
    if Nrm.V[2] < 0.0 then Result.V[1]:= Result.V[1]-0.5*Nrm.V[2];

    Result.V[2]:= 0;
    if Nrm.V[0] < 0.0 then Result.V[2]:= Result.V[2]-0.5*Nrm.V[0];
    if Nrm.V[1] < 0.0 then Result.V[2]:= Result.V[2]-0.5*Nrm.V[1];
    if Nrm.V[2] > 0.0 then Result.V[2]:= Result.V[2] + Nrm.V[2];
    Result.V[3]:= 0.3
  end;

  function GetColor(H:TScalarValue):TVector;
  begin
    Result:= VectorMake(0.890, 0.855, 0.788, Alpha)
    {    if H <= 10 then Result:= VectorMake(0.922, 0.957, 0.980, 1.000)  //<=10
    else if H <= 50 then Result:= VectorMake(0.541, 0.027, 0.027, 1.000) // 10..50
    else if H <= 300 then Result:= VectorMake(0.941, 0.910, 0.859, 1.000) //50..300
    else if H <= 2000 then Result:= VectorMake(0.965, 0.969, 0.973, 1.000) //350.. 2000
    else if H <= 4000 then Result:= VectorMake(0.890, 0.855, 0.788, 1.000) //2000..4000
    else Result:= VectorMake(0.9, 0.9, 0.6, 1.0) }
  end;


begin
  Vertices.Clear;
  Vertices.Capacity:= 3*_ntrigs;

	for i:= 0 to _ntrigs-1 do
    with _triangles^[i] do
    begin
      vx1.coord:=  _vertices^[v1].P;
      vx1.normal:= _vertices^[v1].N;
      vx2.coord:=  _vertices^[v2].P;
      vx2.normal:= _vertices^[v2].N;
      vx3.coord:=  _vertices^[v3].P;
      vx3.normal:= _vertices^[v3].N;
      {$IfDef UseDensity}
      vx1.Color:= GetColor(_vertices^[v1].Density);//GetNrmColor(vx1.normal);
      vx2.Color:= GetColor(_vertices^[v2].Density);//GetNrmColor(vx2.normal);
      vx3.Color:= GetColor(_vertices^[v3].Density);//GetNrmColor(vx3.normal);
      {$Else}
      vx1.Color:= VectorMake(0.890, 0.855, 0.788, Alpha);
      vx2.Color:= VectorMake(0.890, 0.855, 0.788, Alpha);
      vx3.Color:= VectorMake(0.890, 0.855, 0.788, Alpha);
      {$EndIf}
      //Vertices.AddVertex3(vx1, vx2, vx3); seems to be correct the next line
      Vertices.AddVertex3(vx3, vx2, vx1);
  end;

end;



procedure TMarchingCube.CalcMeshObject(AMeshObject: TMeshObject;
  Alpha: Single);
var i:Integer;
  vx1, vx2, vx3:TGLVertexData;
begin
  AMeshObject.Clear;
  AMeshObject.Vertices.Capacity:= _nverts;
  AMeshObject.Normals.Capacity:= _nverts;
  AMeshObject.Colors.Capacity:= _nverts;
  with TFGVertexIndexList.CreateOwned(AMeshObject.FaceGroups) do begin
    
    Mode:= fgmmTriangles;
    for i:= 0 to _nverts-1 do begin
      AMeshObject.Vertices.Add(_vertices^[i].P);
      AMeshObject.Normals.Add(_vertices^[i].N);
      //AMeshObject.Normals.Add(VectorScale(_vertices^[i].N, -1));
      //AMeshObject.Colors.Add(VectorMake(0.890, 0.855, 0.788, Alpha));
    end;

    for i:= 0 to _ntrigs-1 do
      //with _triangles^[i] do VertexIndices.Add(v1, v2, v3);
      with _triangles^[i] do VertexIndices.Add(v3, v2, v1);
  end;
end;



end.
