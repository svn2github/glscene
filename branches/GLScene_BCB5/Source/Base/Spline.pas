//
// This unit is part of the GLScene Project, http://glscene.org
//
{: Spline<p>

   Cubic spline interpolation functions<p>

	<b>History : </b><font size=-1><ul>
           <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
           <li>08/07/04 - LR - Removed ../ from the GLScene.inc
           <li>16/07/02 - Egg - Added methods to access slope per axis
	   <li>28/05/00 - Egg - Javadocisation, minor changes & optimizations,
                           Renamed TSpline to TCubicSpline, added W component
                           and a bunch of helper methods
      <li>20/05/00 - RoC - Created, based on the C source code from Eric
   </ul></font>
}


//    uncommented this line in GetTau:
//       if i=T then Dec(i); // <-- RB 28/05/03  

unit Spline;

interface

uses VectorGeometry,SysUtils,classes,vectorTypes;

type

   TDistCalculationMode = (dcmDefault,dcmLinear,dcmSqLinear);

   TSplineInterpolateMode =(simDefault,simNormalized);
{$i GLScene.inc}

type
   TSplineTermCode =(stcSecondDerivations,
                     stcFirstDeriveations,
                     stcFreeSpline,
                     stcPeriodicSpline,
                     stcFixedSpline);

   TTermConditionIndex =(tciStart,TciEnd);
   TTermConditionCoord =(tccX,tccY,tccZ,tccW);

type TGFSpline = class
  private
    fX,fY,fA,fB,fC,fD,fU: array of Double;
    FSize: Integer;
    FCode: Integer;
    Locks: Integer;
    change:Boolean;

    procedure TDMP;
    procedure TRIDIG;
    function GetInterpolate(X: double): Double;
    procedure SetCode(const Value: Integer);
    procedure SetSize(const Value: Integer);
    function getA(Index: Integer): double;
    function getB(Index: Integer): double;
    function getC(Index: Integer): double;
    function getD(Index: Integer): double;
    function getU(Index: Integer): double;
    function getX(Index: Integer): double;
    function getY(Index: Integer): double;
    procedure setA(Index: Integer; const Value: double);
    procedure setB(Index: Integer; const Value: double);
    procedure setC(Index: Integer; const Value: double);
    procedure setD(Index: Integer; const Value: double);
    procedure setU(Index: Integer; const Value: double);
    procedure setX(Index: Integer; const Value: double);
    procedure setY(Index: Integer; const Value: double);
    function GetSlope(X: double): Double;
		function GetConvex(X: double): Double;

     public
       constructor Create;
       destructor Destroy; override;

       procedure BeginUpdate;
       procedure EndUpdate;
       procedure Recalc;
       procedure Changed;

       property Size: Integer read FSize write SetSize;
       property X[Index:Integer]:double read getX Write setX;
       property Y[Index:Integer]:double read getY Write setY;
       property A[Index:Integer]:double read getA Write setA;
       property B[Index:Integer]:double read getB Write setB;
       property C[Index:Integer]:double read getC Write setC;
       property D[Index:Integer]:double read getD Write setD;
       property U[Index:Integer]:double read getU Write setU;
       property Code:Integer read FCode write SetCode;

       property Interpolate[X:double]:Double read GetInterpolate; default;
       property Slope[X:double]:Double read GetSlope;
       property Convex[X:double]:Double read GetConvex;
     end;



   TAbstractSpline = class (TObject)
      public
         {: Calculates X component at time t.<p> }
         function SplineX(const t : Single): Single;virtual;abstract;
         {: Calculates Y component at time t.<p> }
         function SplineY(const t : single): Single;virtual;abstract;
         {: Calculates Z component at time t.<p> }
         function SplineZ(const t : single): Single;virtual;abstract;
         {: Calculates W component at time t.<p> }
         function SplineW(const t : single): Single;virtual;abstract;

         {: Calculates X and Y components at time t.<p> }
         procedure SplineXY(const t : single; var X, Y : Single);virtual;abstract;
         {: Calculates X, Y and Z components at time t.<p> }
         procedure SplineXYZ(const t : single; var X, Y, Z : Single);virtual;abstract;
         {: Calculates X, Y, Z and W components at time t.<p> }
         procedure SplineXYZW(const t : single; var X, Y, Z, W : Single);virtual;abstract;

         {: Calculates affine vector at time t.<p> }
         function SplineAffineVector(const t : single) : TAffineVector; overload;virtual;abstract;
         {: Calculates affine vector at time t.<p> }
         procedure SplineAffineVector(const t : single; var vector : TAffineVector); overload;virtual;abstract;
         {: Calculates vector at time t.<p> }
         function SplineVector(const t : single) : TVector; overload;virtual;abstract;
         {: Calculates vector at time t.<p> }
         procedure SplineVector(const t : single; var vector : TVector); overload;virtual;abstract;

         {: Calculates X component slope at time t.<p> }
         function SplineSlopeX(const t : Single): Single;virtual;abstract;
         {: Calculates Y component slope at time t.<p> }
         function SplineSlopeY(const t : single): Single;virtual;abstract;
         {: Calculates Z component slope at time t.<p> }
         function SplineSlopeZ(const t : single): Single;virtual;abstract;
         {: Calculates W component slope at time t.<p> }
         function SplineSlopeW(const t : single): Single;virtual;abstract;
         {: Calculates the spline slope at time t. }
         function SplineSlopeVector(const t : single) : TAffineVector; overload;virtual;abstract;

         {: Calculates X component convex at time t.<p> }
         function SplineConvexX(const t : Single): Single;virtual;abstract;
         {: Calculates Y component convex at time t.<p> }
         function SplineConvexY(const t : single): Single;virtual;abstract;
         {: Calculates Z component convex at time t.<p> }
         function SplineConvexZ(const t : single): Single;virtual;abstract;
         {: Calculates W component convex at time t.<p> }
         function SplineConvexW(const t : single): Single;virtual;abstract;
         {: Calculates the spline convex at time t. }
         function SplineConvexVector(const t : single) : TAffineVector; overload;virtual;abstract;


         {: Calculates the intersection of the spline with the YZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;virtual;abstract;
         {: Calculates the intersection of the spline with the XZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;virtual;abstract;
         {: Calculates the intersection of the spline with the XY plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;virtual;abstract;
   end;

   // TCubicSpline
   //
   {: 3D cubic spline handler class.<p>
      This class allows to describe and calculate values of a time-based,
      three-dimensionnal cubic spline.<p>
      Cubic spline pass through all given points and tangent on point N is
      given by the (N-1) to (N+1) vector.<p>
      Note : X, Y & Z are actually interpolated independently. }

   TCubicSpline = class (TAbstractSpline)
   private

    ffX,ffY,ffZ,ffW:PFloatArray;
    Tau:array of Double;
    fNb:Integer;
    fSplineX,fSplineY,fSplineZ,fSplineW:TGFSpline;

    FDistCalculationMode: TDistCalculationMode;
    FSplineTermCode: TSplineTermCode;
    FInterpolateMode: TSplineInterpolateMode;

    procedure SetDistCalculationMode(const Value: TDistCalculationMode);
    procedure SetSplineTermCode(const Value: TSplineTermCode);
    procedure SetInterpolateMode(const Value: TSplineInterpolateMode);
    function GetTermCondition(Index: TtermConditionIndex): TVector4f;
    procedure SetTermCondition(Index: TtermConditionIndex; const Value:TVector4f);
    function GetU(Index: Integer): TVector4f;
    procedure SetU(Index: Integer; const Value: TVector4f);
    function GetX(Index: Integer): single;
    function GetY(Index: Integer): single;
    function GetZ(Index: Integer): single;
    procedure SetX(Index: Integer; const Value: single);
    procedure SetY(Index: Integer; const Value: single);
    procedure SetZ(Index: Integer; const Value: single);
    function GetW(Index: Integer): single;
    procedure SetW(Index: Integer; const Value: single);

   public
        property InterpolateMode:TSplineInterpolateMode read FInterpolateMode write SetInterpolateMode;
        property DistCalculationMode:TDistCalculationMode read FDistCalculationMode write SetDistCalculationMode;
        property SplineTermCode:TSplineTermCode read FSplineTermCode write SetSplineTermCode;
        property TermCondition[Index:TtermConditionIndex]:TVector4f read GetTermCondition write SetTermCondition;
        property U[Index:Integer]:TVector4f read GetU write SetU;

   protected
        procedure Reinit;
        procedure AssignCoordinates;
        procedure RecalcTau;
        function GetTau(T:single):Single;
		public
        procedure BeginUpdate;
        procedure EndUpdate;
        constructor Create(const X, Y, Z, W: PFloatArray;
                            const nb: Integer;
                            SplineTermCode :TSplineTermCode = stcFreeSpline;
                            InterpolateMode:TSplineInterpolateMode = simDefault;
                            DistCalc : TDistCalculationMode = dcmLinear
                            );

        procedure SetXYZW(const X, Y, Z, W: PFloatArray; const nb: Integer);

        //Create(const X, Y, Z, W : PFloatArray; const nb : Integer);
        destructor Destroy; override;


         {: Calculates X component at time t.<p> }
         function SplineX(const t : Single): Single;override;
         {: Calculates Y component at time t.<p> }
         function SplineY(const t : single): Single;override;
         {: Calculates Z component at time t.<p> }
         function SplineZ(const t : single): Single;override;
         {: Calculates W component at time t.<p> }
         function SplineW(const t : single): Single;override;

         {: Calculates X and Y components at time t.<p> }
         procedure SplineXY(const t : single; var X, Y : Single);override;
         {: Calculates X, Y and Z components at time t.<p> }
         procedure SplineXYZ(const t : single; var X, Y, Z : Single);override;
         {: Calculates X, Y, Z and W components at time t.<p> }
         procedure SplineXYZW(const t : single; var X, Y, Z, W : Single);override;

         {: Calculates affine vector at time t.<p> }
         function SplineAffineVector(const t : single) : TAffineVector; overload;override;
         {: Calculates affine vector at time t.<p> }
         procedure SplineAffineVector(const t : single; var vector : TAffineVector); overload;override;
         {: Calculates vector at time t.<p> }
         function SplineVector(const t : single) : TVector; overload;override;
         {: Calculates vector at time t.<p> }
         procedure SplineVector(const t : single; var vector : TVector); overload;override;

         {: Calculates X component slope at time t.<p> }
         function SplineSlopeX(const t : Single): Single;override;
         {: Calculates Y component slope at time t.<p> }
         function SplineSlopeY(const t : single): Single;override;
         {: Calculates Z component slope at time t.<p> }
         function SplineSlopeZ(const t : single): Single;override;
         {: Calculates W component slope at time t.<p> }
         function SplineSlopeW(const t : single): Single;override;
         {: Calculates the spline slope at time t. }
         function SplineSlopeVector(const t : single) : TAffineVector; overload;override;

         {: Calculates X component convex at time t.<p> }
         function SplineConvexX(const t : Single): Single;overload;override;
         {: Calculates Y component convex at time t.<p> }
         function SplineConvexY(const t : single): Single;overload;override;
         {: Calculates Z component convex at time t.<p> }
         function SplineConvexZ(const t : single): Single;overload;override;
         {: Calculates W component convex at time t.<p> }
         function SplineConvexW(const t : single): Single;overload;override;
         {: Calculates the spline convex at time t. }
         function SplineConvexVector(const t : single) : TAffineVector; overload;override;

         {: Calculates the intersection of the spline with the YZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;override;
         {: Calculates the intersection of the spline with the XZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;override;
         {: Calculates the intersection of the spline with the XY plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;override;

         function Curvature(const t : Single):Extended;

         property pX[Index:Integer]:single read GetX write SetX;
         property pY[Index:Integer]:single read GetY write SetY;
         property pZ[Index:Integer]:single read GetZ write SetZ;
         property pW[Index:Integer]:single  read GetW write SetW;
   end;


   TCubicSplineOld = class (TAbstractSpline)
      private
         { Private Declarations }
         matX, matY, matZ, matW : Pointer;
         FNb : Integer;

      public
         { Public Declarations }
         {: Creates the spline and declares interpolation points.<p>
            Time references go from 0 (first point) to nb-1 (last point), the
            first and last reference matrices respectively are used when T is
            used beyond this range.<p>
            Note : "nil" single arrays are accepted, in this case the axis is
            disabled and calculus will return 0 (zero) for this component. }
         constructor Create(const X, Y, Z, W : PFloatArray; const nb : Integer);
         destructor Destroy; override;

         {: Calculates X component at time t.<p> }
         function SplineX(const t : Single): Single;override;
         {: Calculates Y component at time t.<p> }
         function SplineY(const t : single): Single;override;
         {: Calculates Z component at time t.<p> }
         function SplineZ(const t : single): Single;override;
         {: Calculates W component at time t.<p> }
         function SplineW(const t : single): Single;override;

         {: Calculates X and Y components at time t.<p> }
         procedure SplineXY(const t : single; var X, Y : Single);override;
         {: Calculates X, Y and Z components at time t.<p> }
         procedure SplineXYZ(const t : single; var X, Y, Z : Single);override;
         {: Calculates X, Y, Z and W components at time t.<p> }
         procedure SplineXYZW(const t : single; var X, Y, Z, W : Single);override;

         {: Calculates affine vector at time t.<p> }
         function SplineAffineVector(const t : single) : TAffineVector; overload;override;
         {: Calculates affine vector at time t.<p> }
         procedure SplineAffineVector(const t : single; var vector : TAffineVector); overload;override;
         {: Calculates vector at time t.<p> }
         function SplineVector(const t : single) : TVector; overload;override;
         {: Calculates vector at time t.<p> }
         procedure SplineVector(const t : single; var vector : TVector); overload;override;

         {: Calculates X component slope at time t.<p> }
         function SplineSlopeX(const t : Single): Single;override;
         {: Calculates Y component slope at time t.<p> }
         function SplineSlopeY(const t : single): Single;override;
         {: Calculates Z component slope at time t.<p> }
         function SplineSlopeZ(const t : single): Single;override;
         {: Calculates W component slope at time t.<p> }
         function SplineSlopeW(const t : single): Single;override;
         {: Calculates the spline slope at time t. }
         function SplineSlopeVector(const t : single) : TAffineVector; overload;override;

         {: Calculates the intersection of the spline with the YZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;override;
         {: Calculates the intersection of the spline with the XZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;override;
         {: Calculates the intersection of the spline with the XY plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;override;
   end;


type ESplineError = class( Exception)
     end;






// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   // local type used by the Hermite/Cholesky code
   TSingleMatrix = array of PFloatArray;

// VECCholeskyTriDiagResol
//
function VECCholeskyTriDiagResol(const b : PFloatArray; const nb : Integer) : PFloatArray;
var
   Y, LDiag, LssDiag : PFloatArray;
   i, k, Debut, Fin: Integer;
begin
   Debut:=0;
   Fin:=nb-1;
   Assert(Assigned(B));
   GetMem(LDiag, nb*sizeof(Single));
   GetMem(LssDiag, (nb-1)*sizeof(Single));
   try
      LDiag[Debut]:=1.4142135; // = sqrt(2)
      LssDiag[Debut]:=1.0/1.4142135;
      for K:=Debut+1 to Fin-1 do begin
         LDiag[K]:=Sqrt(4-LssDiag[K-1]*LssDiag[K-1]);
         LssDiag[K]:=1.0/LDiag[K];
      end;
      LDiag[Fin]:=Sqrt(2-LssDiag[Fin-1]*LssDiag[Fin-1]);
      GetMem(Y, nb*sizeof(Single));
      try
         Y[Debut]:=B[Debut]/LDiag[Debut];
         for I:=Debut+1 to Fin do
            Y[I]:=(B[I]-Y[I-1]*LssDiag[I-1])/LDiag[I];
         GetMem(Result, nb*sizeof(Single));
         Result[Fin]:=Y[Fin]/LDiag[Fin];
         for i:=Fin-1 downto Debut do
            Result[I]:=(Y[I]-Result[I+1]*LssDiag[I])/LDiag[I];
      finally
         FreeMem(Y);
      end;
   finally
      FreeMem(LDiag);
      FreeMem(LssDiag);
   end;
end;

// MATInterpolationHermite
//
function MATInterpolationHermite(const ordonnees : PFloatArray; const nb : Integer): Pointer;
var
   a, b, c, d : Single;
   m : Pointer;
   i, n : Integer;
   bb, deriv, fa : PFloatArray;
begin
   Result:=nil;
   if Assigned(Ordonnees) and (nb>0) then begin
      n:=nb-1;
      GetMem(bb, nb*sizeof(Single));
      try
         PFloatArray(bb)[0]:=3*(ordonnees[1]-ordonnees[0]);
         PFloatArray(bb)[n]:=3*(ordonnees[n]-ordonnees[n-1]);
         for i:=1 to n-1 do
            bb[I]:=3*(ordonnees[I+1]-ordonnees[I-1]);
         deriv:=VECCholeskyTriDiagResol(bb, nb);
         try
            GetMem(m, n*SizeOf(PFloatArray));
            for i:=0 to n-1 do begin
               GetMem(TSingleMatrix(m)[I], 4*SizeOf(Single));
               a:=ordonnees[I];
               b:=deriv[I];
               c:=3*(ordonnees[I+1]-ordonnees[I])-2*deriv[I]-deriv[I+1];
               d:=-2*(ordonnees[I+1]-ordonnees[I])+deriv[I]+deriv[I+1];
               fa:=TSingleMatrix(m)[I];
               fa[3]:=a+I*(I*(c-I*d)-b);
               fa[2]:=b+I*(3*I*d-2*c);
               fa[1]:=c-3*I*d;
               fa[0]:=d;
            end;
         finally
            FreeMem(Deriv);
         end;
      finally
         FreeMem(BB);
      end;
      Result:=m;
   end;
end;

// MATValeurSpline
//
function MATValeurSpline(const spline : Pointer; const x : Single;
                         const nb : Integer) : Single;
var
   i : Integer;
   sa : PFloatArray;
begin
   if Assigned(Spline) then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Integer(Trunc(x));
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      sa:=PFloatArray(TSingleMatrix(spline)[i]);
      Result:=((sa[0]*x+sa[1])*x+sa[2])*x+sa[3];
   end else Result:=0;
end;

// MATValeurSplineSlope
//
function MATValeurSplineSlope(const spline : Pointer; const x : Single;
                              const nb : Integer) : Single;
var
   i : Integer;
   sa : PFloatArray;
begin
   if Assigned(Spline) then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Integer(Trunc(x));
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      sa:=PFloatArray(TSingleMatrix(spline)[i]);
      Result:=(3*sa[0]*x+2*sa[1])*x+sa[2];
   end else Result:=0;
end;

// ------------------
// ------------------ TCubicSplineOld ------------------
// ------------------

// Create
//
constructor TCubicSplineOld.Create(const X, Y, Z, W: PFloatArray; const nb : Integer);
begin
   MatX:=MATInterpolationHermite(X, nb);
   MatY:=MATInterpolationHermite(Y, nb);
   MatZ:=MATInterpolationHermite(Z, nb);
   MatW:=MATInterpolationHermite(W, nb);
   FNb:=nb;
end;

// Destroy
//
destructor TCubicSplineOld.Destroy;

   procedure FreeMatrix(matrix : Pointer);
   var
      i : Integer;
   begin
      if Assigned(matrix) then begin
         for i:=0 to FNb-2 do
            FreeMem(PFloatArray(TSingleMatrix(matrix)[I]));
         FreeMem(matrix);
      end;
   end;

begin
   FreeMatrix(MatX);
   FreeMatrix(MatY);
   FreeMatrix(MatZ);
   FreeMatrix(MatW);
   inherited Destroy;
end;

// SplineX
//
function TCubicSplineOld.SplineX(const t : single): Single;
begin
   Result:=MATValeurSpline(MatX, t, FNb);
end;

// SplineY
//
function TCubicSplineOld.SplineY(const t : single): Single;
begin
   Result:=MATValeurSpline(MatY, t, FNb);
end;

// SplineZ
//
function TCubicSplineOld.SplineZ(const t : single): Single;
begin
   Result:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineW
//
function TCubicSplineOld.SplineW(const t : single): Single;
begin
   Result:=MATValeurSpline(MatW, t, FNb);
end;

// SplineXY
//
procedure TCubicSplineOld.SplineXY(const t : single; var X, Y : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
end;

// SplineXYZ
//
procedure TCubicSplineOld.SplineXYZ(const t : single; var X, Y, Z : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
   Z:=MATValeurSpline(MatZ, T, FNb);
end;

// SplineXYZW
//
procedure TCubicSplineOld.SplineXYZW(const t : single; var X, Y, Z, W : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
   Z:=MATValeurSpline(MatZ, T, FNb);
   W:=MATValeurSpline(MatW, T, FNb);
end;

// SplineAffineVector
//
function TCubicSplineOld.SplineAffineVector(const t : single) : TAffineVector;
begin
   Result.Coord[0]:=MATValeurSpline(MatX, t, FNb);
   Result.Coord[1]:=MATValeurSpline(MatY, t, FNb);
   Result.Coord[2]:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineAffineVector
//
procedure TCubicSplineOld.SplineAffineVector(const t : single; var vector : TAffineVector);
begin
   vector.Coord[0]:=MATValeurSpline(MatX, t, FNb);
   vector.Coord[1]:=MATValeurSpline(MatY, t, FNb);
   vector.Coord[2]:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineVector
//
function TCubicSplineOld.SplineVector(const t : single) : TVector;
begin
   Result.Coord[0]:=MATValeurSpline(MatX, t, FNb);
   Result.Coord[1]:=MATValeurSpline(MatY, t, FNb);
   Result.Coord[2]:=MATValeurSpline(MatZ, t, FNb);
   Result.Coord[3]:=MATValeurSpline(MatW, t, FNb);
end;

// SplineVector
//
procedure TCubicSplineOld.SplineVector(const t : single; var vector : TVector);
begin
   vector.Coord[0]:=MATValeurSpline(MatX, t, FNb);
   vector.Coord[1]:=MATValeurSpline(MatY, t, FNb);
   vector.Coord[2]:=MATValeurSpline(MatZ, t, FNb);
   vector.Coord[3]:=MATValeurSpline(MatW, t, FNb);
end;

// SplineSlopeX
//
function TCubicSplineOld.SplineSlopeX(const t : Single): Single;
begin
   Result:=MATValeurSplineSlope(MatX, t, FNb);
end;

// SplineSlopeY
//
function TCubicSplineOld.SplineSlopeY(const t : single): Single;
begin
   Result:=MATValeurSplineSlope(MatY, t, FNb);
end;

// SplineSlopeZ
//
function TCubicSplineOld.SplineSlopeZ(const t : single): Single;
begin
   Result:=MATValeurSplineSlope(MatZ, t, FNb);
end;

// SplineSlopeW
//
function TCubicSplineOld.SplineSlopeW(const t : single): Single;
begin
   Result:=MATValeurSplineSlope(MatW, t, FNb);
end;

// SplineSlopeVector
//
function TCubicSplineOld.SplineSlopeVector(const t : single) : TAffineVector;
begin
   Result.Coord[0]:=MATValeurSplineSlope(MatX, t, FNb);
   Result.Coord[1]:=MATValeurSplineSlope(MatY, t, FNb);
   Result.Coord[2]:=MATValeurSplineSlope(MatZ, t, FNb);
end;

// SplineIntersecYZ
//
function TCubicSplineOld.SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineX(Sup);
   Sinf:=SplineX(Inf);
   if SSup>Sinf then begin
      if (SSup<X) or (Sinf>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<X) or (SSup>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

// SplineIntersecXZ
//
function TCubicSplineOld.SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineY(Sup);
   Sinf:=SplineY(Inf);
   if SSup>Sinf then begin
      if (SSup<Y) or (Sinf>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Y) or (SSup>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

// SplineIntersecXY
//
function TCubicSplineOld.SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineZ(Sup);
   Sinf:=SplineZ(Inf);
   if SSup>Sinf then begin
      if (SSup<Z) or (Sinf>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Z) or (SSup>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

{TGFSpline}

procedure TGFSpline.BeginUpdate;
begin
  Inc(Locks);
end;

procedure TGFSpline.Changed;
begin
  change:=true;
end;

constructor TGFSpline.Create;
begin
  FSize:=0;
  FCode:=0;
  change:=False;
  Locks:=0;
end;

destructor TGFSpline.Destroy;
begin
  inherited;
end;

procedure TGFSpline.EndUpdate;
begin
  Dec(Locks);if Locks<0 then Locks :=0;
  if (Locks=0) and(change) then
    recalc;
end;

function TGFSpline.getA(Index: Integer): double;
begin
  Result:=fa[Index-1];
end;

function TGFSpline.getB(Index: Integer): double;
begin
  Result:=fB[Index-1];
end;

function TGFSpline.getC(Index: Integer): double;
begin
  Result:=fC[Index-1];
end;

function TGFSpline.GetConvex(X: double): Double;
var i: Integer;
begin
 if X =Self.X[1] then
  begin
    Result:= 6*A[1]*X+2*B[1];
    Exit;
  end;
 i:=1;
 while Self.X[i] <X do begin
 Inc(i);
 if i> Size then
   begin
     I:=Size;break;
     raise ESplineError.Create('TGFSpline.GetConvex');
   end;
 end;
 Dec(i);
 Result:= 6*A[i]*X+2*B[i];
end;

function TGFSpline.getD(Index: Integer): double;
begin
  Result:=fD[Index-1];
end;

function TGFSpline.GetInterpolate(X: double): Double;
var i: Integer;
begin
 if X =Self.X[1] then
  begin
    Result:=Y[1];
    Exit;
  end;

i:=1;
while Self.X[i] <X do begin
 Inc(i);
 if i> Size then
   begin
     I:=Size;break;
     raise ESplineError.Create('TGFSpline.GetInterpolate');
   end;
 end;
 Dec(i);
 Result:=((((A[i]*X)+B[I])*X)+C[i])*X+D[i];
end;

function TGFSpline.GetSlope(X: double): Double;
var i: Integer;
begin
 if X =Self.X[1] then
  begin
    Result:= ((3*A[1]*X)+2*B[1])*X +C[1];
    Exit;
  end;
 i:=1;
 while Self.X[i] <X do begin
 Inc(i);
 if i> Size then
   begin
     I:=Size;break;
     raise ESplineError.Create('TGFSpline.GetSlope');
   end;
 end;
 Dec(i);
 Result:= ((3*A[i]*X)+2*B[i])*X +C[i];
end;

function TGFSpline.getU(Index: Integer): double;
begin
  Result:=fU[Index-1];
end;

function TGFSpline.getX(Index: Integer): double;
begin
  Result:=fX[Index-1];
end;

function TGFSpline.getY(Index: Integer): double;
begin
  Result:=fY[Index-1];
end;


procedure TGFSpline.Recalc;
var I :Integer;
var E,F,G,H1,H2,R,S:Double;
begin
  case Size of
  0:exit;
  1:begin
    D[1]:=Y[1];
    Exit;
    end;
  2:begin
     if X[2]-X[1]=0 then
       C[1]:=1.e20
     else
       C[1]:= (Y[2]-Y[1])/(X[2]-X[1]);
     D[1]:=  Y[1]-C[1]*X[1];
     Exit;
    end;
  end;

      A[2]:=X[2]-X[1];
      if (CODE <>2) then
      try
        TDMP;
        TRIDIG;
      except
       Exit;
      //if (IER<>0) then
      //  raise ESplineError.Create('TFGSpline.recalc');
      end;
      for i :=2 to Size  do
      begin
        E:=X[I]*Y[I-1];
        F:=X[I-1]*Y[I];
        G:=X[I-1]*X[I];
        R:=U[I-1]*X[I];
        S:=U[I]*X[I-1];
        H1:=2./(X[I]-X[I-1]);
        H2:=H1*H1/4.;
        D[I-1]:=H2*(H1*G*(F-E)+E*X[I]+F*X[I-1]-G*(R+S));
        E:=H1*(Y[I]-Y[I-1]);
        F:=X[I-1]+2.*X[I];
        H1:=X[I]+2.*X[I-1];
        C[I-1]:=H2*(-3.*E*G+R*H1+S*F);
        B[I-1]:=1.5*H2*E*(F-X[I])-H2*(U[I-1]*F+U[I]*H1);
        A[I-1]:=H2*(U[I]+U[I-1]-E);
      end;
end;

procedure TGFSpline.setA(Index: Integer; const Value: double);
begin
  fA[Index-1]:=Value;
  changed;
end;

procedure TGFSpline.setB(Index: Integer; const Value: double);
begin
  fB[Index-1]:=Value;
  changed;
end;

procedure TGFSpline.setC(Index: Integer; const Value: double);
begin
  fC[Index-1]:=Value;
  changed;
end;

procedure TGFSpline.SetCode(const Value: Integer);
begin
  if (FCode = Value) then
   Exit;
  FCode := Value;
  changed;
end;

procedure TGFSpline.setD(Index: Integer; const Value: double);
begin
  fD[Index-1]:=Value;
  changed;
end;


procedure TGFSpline.SetSize(const Value: Integer);
begin
  if(FSize=value) then Exit;
  FSize := Value;
  SetLength(fX,Fsize);
  SetLength(fY,Fsize);
  SetLength(fA,Fsize);
  SetLength(fB,Fsize);
  SetLength(fC,Fsize);
  SetLength(fD,Fsize);
  SetLength(fU,Fsize);
  changed;
end;

procedure TGFSpline.setU(Index: Integer; const Value: double);
begin
  fU[Index-1]:=Value;
  changed;
end;

procedure TGFSpline.setX(Index: Integer; const Value: double);
begin
  fX[Index-1]:=Value;
  changed;
end;

procedure TGFSpline.setY(Index: Integer; const Value: double);
begin
  fY[Index-1]:=Value;
  changed;
end;


procedure TGFSpline.TDMP;
label 1,2,3,4,5,6,7;
var
  A2,C2,DY,DY1,DY2:Double;
var i : Integer;
begin
      if (Size <= 2) then Exit;
      DY2:=Y[2]-Y[1];
      C[2]:=X[2]-X[1];
      if (C[2]<=0.0) then Exit;
      DY:=DY2;
      for I:=3 to Size do begin //1
          DY1:=DY2;
          DY2:=Y[I]-Y[I-1];
          C[I]:=X[I]-X[I-1];
          if (C[I]<=0.0) then Exit;
          A[I-1]:=C[I];
          A2:=A[I-1];
          C2:=C[I-1];
          B[I-1]:=2.*(C2+A2);
          D[I-1]:=3.*(A2*DY1/C2+C2*DY2/A2);
      end;
      A[1]:=0.;
      C[Size]:=0.;
      if(CODE+1 <0) then  goto 5
      else if (CODE+1 =0) then goto 3
      else goto 2;
   2:;if(CODE-1<0) then goto 4
       else if(CODE-1=0) then goto 6
       else Exit;
   3:;B[1]:=1.;
      B[Size]:=1.;
      C[1]:=0.;
      A[Size]:=0.;
      Exit;
   4:;D[1]:=0.;
      D[Size]:=0. ;
   5:;B[1]:=2.  ;
      B[Size]:=2.;
      C[1]:=1.;
      A[Size]:=1. ;
      D[1]:=3.*DY/C[2]-(C[2]*D[1])/2.;
      D[Size]:=3.*DY2/A[Size-1]-(A[Size-1]*D[Size])/2.;
      Exit;
   6:;A[1]:=-1.;
      C[1]:=0.;
      D[1]:=0.;
      B[1]:=1.;
      A[Size]:=C[2];
      C[Size]:=A[Size-1];
      B[Size]:=2.*(C[2]+A[Size-1]);
      D[Size]:=3.*((A[Size]/C[Size])*DY2+(C[Size]/A[Size])*DY);
end;

procedure TGFSpline.TRIDIG;
var V,T,AL,BT,GM,S,S1:Double;
var I,N1:Integer;
label 8;
begin
      AL:=0.;
      BT:=0. ;
      GM:=1.;
      N1:=size-1;
      if (B[1]=0.0) then
       begin
         raise ESplineError.Create('TGFSpline.TRIDIG(1)');
       end;
       for i:=1 to N1 do begin
        S:=A[I]*AL+B[I];
        if (S =0.0) then
          raise ESplineError.Create('TGFSpline.TRIDIG(2)');
        S1:=1./S;
        AL:=-C[I]*S1;
        BT:=(D[I]-A[I]*BT)*S1;
        GM:=-A[I]*GM*S1;
        A[I]:=AL;
        B[I]:=BT;
        C[I]:=GM;
      end;

      if (Code =1) then GOTO 8;
      U[size]:=(D[size]-A[size]*BT)/(A[size]*AL+B[Size]);
       for i:=1 to N1 do
        U[size-I]:=A[size-I]*U[size-I+1]+B[size-I];
      Exit;
   8: T:=1.;
      V:=0.;
      for  I:=1 to N1 do begin
        T:=A[size-I]*T+C[size-I];
        V:=A[size-I]*V+B[size-I];
        A[size-I]:=T;
        B[size-I]:=V;
      end;
      U[size]:=(D[size]-C[size]*B[2]-A[size]*B[size-1])/(B[size]+C[size]*A[2]+A[size]*A[size-1]);
      for  I:=1 to N1 do
        U[size-I]:=A[size-I]*U[size]+B[size-I];
      Exit;
end;

{ TCubicSpline }

procedure TCubicSpline.AssignCoordinates;
var i : Integer;
begin
   fSplineX.BeginUpdate;
   fSplineY.BeginUpdate;
   fSplineZ.BeginUpdate;
   fSplineW.BeginUpdate;
   try

     fSplineX.Size:=fNb;
     fSplineY.Size:=fNb;
     fSplineZ.Size:=fNb;
     fSplineW.Size:=fNb;

     fSplineX.Code:=Integer(Self.FSplineTermCode)-2;
     fSplineY.Code:=Integer(Self.FSplineTermCode)-2;
     fSplineZ.Code:=Integer(Self.FSplineTermCode)-2;
     fSplineW.Code:=Integer(Self.FSplineTermCode)-2;



    Self.RecalcTau;
    for i :=0 to fnb-1 do
     begin
        fSplineX.X[i+1]:=Self.tau[i];
        fSplineY.X[i+1]:=Self.tau[i];
        fSplineZ.X[i+1]:=Self.tau[i];
        fSplineW.X[i+1]:=Self.tau[i];

        fSplineX.Y[i+1]:=Self.pX[i];
        fSplineY.Y[i+1]:=Self.pY[i];
        fSplineZ.Y[i+1]:=Self.pZ[i];
        fSplineW.Y[i+1]:=Self.pW[i];

    end;


   finally
     fSplineX.endUpdate;
     fSplineY.endUpdate;
     fSplineZ.endUpdate;
     fSplineW.endUpdate;
   end;
end;

procedure TCubicSpline.BeginUpdate;
begin
   fSplineX.BeginUpdate;
   fSplineY.BeginUpdate;
   fSplineZ.BeginUpdate;
   fSplineW.BeginUpdate;
end;

constructor TCubicSpline.Create(const X, Y, Z, W: PFloatArray;
  const nb: Integer;
  SplineTermCode :TSplineTermCode = stcFreeSpline;
  InterpolateMode:TSplineInterpolateMode = simDefault;
  DistCalc : TDistCalculationMode = dcmLinear
  );
begin
    FSplineTermCode:= SplineTermCode;
    FInterpolateMode:= InterpolateMode;
    DistCalculationMode:= DistCalc;

    fSplineX:=TGFSpline.Create;
    fSplineY:=TGFSpline.Create;
    fSplineZ:=TGFSpline.Create;
    fSplineW:=TGFSpline.Create;
    ffX:=X;
    ffY:=Y;
    ffZ:=Z;
    ffW:=W;
    fNb:=nb;
    Reinit;
end;

destructor TCubicSpline.Destroy;
begin
  FreeAndNil(fSplineX);
  FreeAndNil(fSplineY);
  FreeAndNil(fSplineZ);
  FreeAndNil(fSplineW);
  Tau:= nil;
  inherited;
end;


procedure TCubicSpline.EndUpdate;
begin
   fSplineX.endUpdate;
   fSplineY.endUpdate;
   fSplineZ.endUpdate;
   fSplineW.endUpdate;
end;

function TCubicSpline.GetTau(T: single): Single;
var i :Integer;
begin
 Result:=T;
 case fInterpolateMode of
  simDefault:
   begin
    i:=Trunc(T);
    if i=T then Dec(i); // <-- is it needed?? RB 28-05-03
    Result:=tau[i]+(Tau[i+1]-Tau[i])*(T-i);
   end;
  simNormalized:
    begin
      Result:=T;
    end;
 end;

end;

function TCubicSpline.GetTermCondition(
  Index: TtermConditionIndex): TVector4f;
begin
 case Index of
 tciStart:
   begin
     Result.Coord[0]:=fSplineX.D[1];
     Result.Coord[1]:=fSplineY.D[1];
     Result.Coord[2]:=fSplineZ.D[1];
     Result.Coord[3]:=fSplineW.D[1];
   end;
 TciEnd:
   begin
     Result.Coord[0]:=fSplineX.D[fSplineX.FSize];
     Result.Coord[1]:=fSplineY.D[fSplineY.FSize];
     Result.Coord[2]:=fSplineZ.D[fSplineZ.FSize];
     Result.Coord[3]:=fSplineW.D[fSplineW.FSize];
   end;
 end;

end;

function TCubicSpline.GetU(Index: Integer): TVector4f;
begin
   Result.Coord[0]:=fSplineX.U[Index+1];
   Result.Coord[1]:=fSplineY.U[Index+1];
   Result.Coord[2]:=fSplineZ.U[Index+1];
   Result.Coord[3]:=fSplineW.U[Index+1];
end;

function TCubicSpline.GetW(Index: Integer): single;
begin
 if ffw<>nil then
   Result:=Ffw[index]
 else
   Result:=0;
end;

function TCubicSpline.GetX(Index: Integer): single;
begin
 if ffx<>nil then
   Result:=Ffx[index]
 else
   Result:=0;
end;

function TCubicSpline.GetY(Index: Integer): single;
begin
 if ffY<>nil then
   Result:=FfY[index]
 else
   Result:=0;
end;

function TCubicSpline.GetZ(Index: Integer): single;
begin
 if ffZ<>nil then
   Result:=FfZ[index]
 else
   Result:=0;

end;

procedure TCubicSpline.RecalcTau;
var i :Integer;
var ti: single;
begin
 SetLength(Self.tau, fnb);
 if fnb=0 then Exit;
 ti := 0;
 Tau[0]:=ti;
 for i :=1 to fNb-1 do
  begin
     case fDistCalculationMode  of
     dcmDefault:
       begin
         ti:=i;
         Tau[i]:=ti;
       end;
     dcmLinear:
       begin
        //writeln( fx<>nil,' ',fy<>nil,' ',Fz<>Nil);
         ti:=tau[i-1]+sqrt(sqr(pX[i]-pX[i-1])+
                     sqr(pY[i]-pY[i-1])+
                     sqr(pZ[i]-pZ[i-1])
                     );
// We have to do it  if the points are the same - overwise TRIDIG Fails
//         if Tau[i-1]=tau[i-1] then Tau[i]:=ti+1.e-6;
         if ti=tau[i-1] then Tau[i]:=ti+1.e-6
         else Tau[i]:=ti
       end;
     dcmSqLinear:
       begin
         ti:=ti+sqrt(sqr(pX[i]-pX[i-1])+
                     sqr(pY[i]-pY[i-1])+
                     sqr(pZ[i]-pZ[i-1])
                     );
// We have to do it  if the points are the same - overwise TRIDIG Fails
//         if Tau[i-1]=tau[i-1] then Tau[i]:=ti+1.e-6;
         if ti=tau[i-1] then Tau[i]:=ti+1.e-6
         else Tau[i]:=ti
       end;
     end;
  end;
end;

procedure TCubicSpline.Reinit;
begin
 Assigncoordinates;
 Self.RecalcTau;
end;

procedure TCubicSpline.SetDistCalculationMode(
  const Value: TDistCalculationMode);
begin
  FDistCalculationMode := Value;
end;

procedure TCubicSpline.SetInterpolateMode(
  const Value: TSplineInterpolateMode);
begin
  FInterpolateMode := Value;
end;

procedure TCubicSpline.SetSplineTermCode(const Value: TSplineTermCode);
begin
 if FSplineTermCode = Value then
  Exit;
 BeginUpdate;
 try
  FSplineTermCode := Value;
 finally
  EndUpdate;
 end;
end;


procedure TCubicSpline.SetTermCondition(Index: TtermConditionIndex;
  const Value: TVector4f);
begin
 BeginUpdate;
 try
 case Index of
 tciStart:
   begin
     fSplineX.D[1]:=Value.Coord[0];
     fSplineY.D[1]:=Value.Coord[1];
     fSplineZ.D[1]:=Value.Coord[2];
     fSplineW.D[1]:=Value.Coord[3];
   end;
 TciEnd:
   begin
     fSplineX.D[fSplineX.FSize]:=Value.Coord[0];
     fSplineY.D[fSplineY.FSize]:=Value.Coord[1];
     fSplineZ.D[fSplineZ.FSize]:=Value.Coord[2];
     fSplineW.D[fSplineW.FSize]:=Value.Coord[3];
   end;
 end;
 finally
  EndUpdate;
 end;
end;

procedure TCubicSpline.SetU(Index: Integer; const Value: TVector4f);
begin
 BeginUpdate;
 try
     fSplineX.U[index+1]:=Value.Coord[0];
     fSplineY.U[index+1]:=Value.Coord[1];
     fSplineZ.U[index+1]:=Value.Coord[2];
     fSplineW.U[index+1]:=Value.Coord[3];
 except
   Endupdate;
 end

end;

procedure TCubicSpline.SetW(Index: Integer; const Value: single);
begin

end;

procedure TCubicSpline.SetX(Index: Integer; const Value: single);
begin

end;

procedure TCubicSpline.SetXYZW(const X, Y, Z, W: PFloatArray;
  const nb: Integer);
begin
  ffX:=X;
  ffY:=Y;
  ffZ:=Z;
  ffW:=W;
  fNb:=nb;
  Reinit;
end;

procedure TCubicSpline.SetY(Index: Integer; const Value: single);
begin

end;

procedure TCubicSpline.SetZ(Index: Integer; const Value: single);
begin

end;

procedure TCubicSpline.SplineAffineVector(const t: single;
  var vector: TAffineVector);
var tau:Single;
begin
   tau:=gettau(t);
   vector.Coord[0]:=FsplineX[tau];
   vector.Coord[1]:=FsplineY[tau];
   vector.Coord[2]:=FsplineZ[tau];
end;

function TCubicSpline.SplineAffineVector(const t: single): TAffineVector;
var tau:Single;
begin
   tau:=gettau(t);
   result.Coord[0]:=fSplineX[tau];
   result.Coord[1]:=FsplineY[tau];
   result.Coord[2]:=FsplineZ[tau];
end;

function TCubicSpline.SplineConvexVector(const t: single): TAffineVector;
var tau : Double;
begin
   tau:=getTau(t);
   Result.Coord[0]:=self.fSplineX.Convex[Tau];
   Result.Coord[1]:=self.fSplineY.Convex[Tau];
   Result.Coord[2]:=self.fSplineZ.Convex[Tau];
end;

function TCubicSpline.SplineConvexW(const t: single): Single;
begin
  Result:=self.fSplineW.Convex[Self.GetTau(t)];
end;

function TCubicSpline.SplineConvexX(const t: Single): Single;
begin
  Result:=self.fSplineX.Convex[Self.GetTau(t)];
end;

function TCubicSpline.SplineConvexY(const t: single): Single;
begin
  Result:=self.fSplineY.Convex[Self.GetTau(t)];
end;

function TCubicSpline.SplineConvexZ(const t: single): Single;
begin
  Result:=self.fSplineZ.Convex[Self.GetTau(t)];
end;

function TCubicSpline.SplineIntersecXY(Z: Single; var X,
  Y: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineZ(Sup);
   Sinf:=SplineZ(Inf);
   if SSup>Sinf then begin
      if (SSup<Z) or (Sinf>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Z) or (SSup>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

function TCubicSpline.SplineIntersecXZ(Y: Single; var X,
  Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineY(Sup);
   Sinf:=SplineY(Inf);
   if SSup>Sinf then begin
      if (SSup<Y) or (Sinf>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Y) or (SSup>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

function TCubicSpline.SplineIntersecYZ(X: Single; var Y,
  Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineX(Sup);
   Sinf:=SplineX(Inf);
   if SSup>Sinf then begin
      if (SSup<X) or (Sinf>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<X) or (SSup>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

function TCubicSpline.SplineSlopeVector(const t: single): TAffineVector;
var tau : Double;
begin
   tau:=getTau(t);
   Result.Coord[0]:=self.fSplineX.Slope[Tau];
   Result.Coord[1]:=self.fSplineY.Slope[Tau];
   Result.Coord[2]:=self.fSplineZ.Slope[Tau];
end;

function TCubicSpline.SplineSlopeW(const t: single): Single;
begin
   Result:=self.fSplineW.Slope[Self.GetTau(t)];
end;

function TCubicSpline.SplineSlopeX(const t: Single): Single;
begin
   Result:=self.fSplineX.Slope[Self.GetTau(t)];
end;

function TCubicSpline.SplineSlopeY(const t: single): Single;
begin
   Result:=self.fSplineY.Slope[Self.GetTau(t)];
end;

function TCubicSpline.SplineSlopeZ(const t: single): Single;
begin
   Result:=self.fSplineZ.Slope[Self.GetTau(t)];
end;

procedure TCubicSpline.SplineVector(const t: single; var vector: TVector);
var  tau: double;
begin
  tau:=getTau(t);
  vector.Coord[0]:=fSplineX[tau];
  vector.Coord[1]:=fSplineY[tau];
  vector.Coord[2]:=fSplineZ[tau];
  vector.Coord[3]:=fSplineW[tau];
end;

function TCubicSpline.SplineVector(const t: single): TVector;
var tau: Double;
begin
  tau:=getTau(t);
  result.Coord[0]:=fSplineX[tau];
  result.Coord[1]:=fSplineY[tau];
  result.Coord[2]:=fSplineZ[tau];
  result.Coord[3]:=fSplineW[tau];
end;

function TCubicSpline.SplineW(const t: single): Single;
begin
  result:=fSplineW[gettau(t)];
end;

function TCubicSpline.SplineX(const t: Single): Single;
begin
  result:=fSplineX[GetTau(t)];
end;

procedure TCubicSpline.SplineXY(const t: single; var X, Y: Single);
var tau : Double;
begin
  tau :=GetTau(t);
  X:=fSplineX[Tau];
  Y:=fSplineY[Tau];
end;

procedure TCubicSpline.SplineXYZ(const t: single; var X, Y, Z: Single);
var tau : Double;
begin
  tau :=GetTau(t);
  X:=fSplineX[Tau];
  Y:=fSplineY[Tau];
  Z:=fSplineZ[Tau];
end;

procedure TCubicSpline.SplineXYZW(const t: single; var X, Y, Z,
  W: Single);
var tau : Double;
begin
  tau :=GetTau(t);
  X:=fSplineX[Tau];
  Y:=fSplineY[Tau];
  Z:=fSplineZ[Tau];
  W:=fSplineW[Tau];
end;

function TCubicSpline.SplineY(const t: single): Single;
begin
  result:=fSplineY[GetTau(t)];
end;

function TCubicSpline.SplineZ(const t: single): Single;
begin
  Result:=fSplineZ[GetTau(t)];
end;

function TCubicSpline.Curvature(const t : Single):Extended;
var dx, ddx:TAffineVector;
	ldx:Extended;
begin
	dx:= SplineSlopeVector(t);
  ldx:= VectorLength(dx); ldx:= ldx*sqr(ldx);
  ddx:= SplineConvexVector(t);
	Result:= ldx/VectorLength(VectorCrossProduct(dx, ddx));
end;


end.
