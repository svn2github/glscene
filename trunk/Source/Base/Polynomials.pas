{: Polynomials.<p>

   ********* IN PROGRESS - NOT FULLY STABLE **********

   Utility functions for manipulationg and solving polynomials.<p>

   Direct solving is upported for polynoms up to the 4th degree.<p>

   Polynom solving code heavily based on the qtrct Fortran code by<ul>
        Alan Miller<br>
        CSIRO Mathematical & Information Sciences<br>
        CLAYTON, VICTORIA, AUSTRALIA 3169<br>
   </ul>Which was originally written by<ul>
        ALFRED H. MORRIS<br>
        NAVAL SURFACE WEAPONS CENTER<br>
        DAHLGREN, VIRGINIA<br>
   </ul><p>

   Adapted to pascal by Eric Grange (egrange@glscene.org), if you find
   errors, they are probably mine. Note that contrary to the original code,
   the functions accept 'zero' values for any of the parameters.<br>
   I also made some changes for certain limit cases that (seemingly) weren't
   properly handled, these are marked by comments in the code.<p>

   Complex code is currently *NOT* the purpose of this unit, and is here only
   to provide Complex numbers support for polynom roots output without having
   a dependency.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>22/08/01 - EG - Some fixes, qtcrt still no up to what I expected
	   <li>21/08/01 - EG - Creation
	</ul></font>

}
unit Polynomials;

interface

uses Geometry;

type
   TComplex = record
      Real, Imag : Extended;
   end;
   TComplexArray = array of TComplex;

{: Creates a complex. }
function Complex(const real, imag : Extended) : TComplex;
{: Calculates the square root of a complex.<p>
   This "emulates" the Fortran sqrt for complex, and returns a positive root
   (for which Real>=0) }
function ComplexSqrt(const complex : TComplex) : TComplex;

{: Sign transfer function from ye olde Fortran. }
function FortranSign(const a, b : Extended) : Extended;

{: Calculates the cube root of its parameter. }
function cbrt(const x : Extended) : Extended;

{: Computes the root of a real polynomial of the 2nd degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + A(2)*Z**2 }
function qdcrt(const a : PExtendedArray) : TComplexArray;

{: Computes the root of a real polynomial of the 3rd degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + A(2)*Z**2 + A(3)*Z**3 }
function cbcrt(const a : PExtendedArray) : TComplexArray;

{: Computes the root of a real polynomial of the 4th degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + ... + A(4)*Z**4 }
function qtcrt(const a : PExtendedArray) : TComplexArray;

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

const
   cEpsilon : Extended = 1e-15;

// Complex
//
function Complex(const real, imag : Extended) : TComplex;
begin
   Result.Real:=real;
   Result.Imag:=imag;
end;

// ComplexSqrt
//
function ComplexSqrt(const complex : TComplex) : TComplex;
var
   rho, theta : Extended;
begin
   if complex.Imag<>0 then begin
      // go for polar representation
      rho  :=Sqrt(Sqr(complex.Real)+Sqr(complex.Imag));
      theta:=ArcTan2(complex.Imag, complex.Real);
      // calc polar sqrt
      rho  :=Sqrt(rho);
      theta:=theta*0.5;
      // back to cartesian
      SinCos(theta, rho, Result.Imag, Result.Real);
      if Result.Real<0 then begin
         // we want it positive, take the symetric root
         Result.Real:=-Result.Real;
         Result.Imag:=-Result.Imag;
      end;
   end else begin
      // trivial case
      Result.Real:=Sqrt(complex.Real);
      Result.Imag:=0;
   end;
end;

// FortranSign
//
function FortranSign(const a, b : Extended) : Extended;
begin
   // now, isn't that some weird 'sign' function? ;)
   if (b>=0) then
      Result:=Abs(a)
   else Result:=-Abs(a);
end;

// cbrt
//
function cbrt(const x : Extended) : Extended;
const
   c1div3 : Extended = 1/3;
begin
   if x<0 then
      Result:=-Exp(Ln(-x)*c1div3)
   else if x>0 then
      Result:=Exp(Ln(x)*c1div3)
   else Result:=0;
end;

// qdcrt
//
function qdcrt(const a : PExtendedArray) : TComplexArray;
var
   d, r, w, x, y : Extended;
begin
   if (a[0] = 0) then begin
      // zero is a trivial solution
      SetLength(Result, 2);
      Result[0] := Complex(0, 0);
      Result[1] := Complex(-a[1]/a[2], 0);
   end else if a[2]=0 then begin
      // actually of a lower order
      if a[1]=0 then
         // not even a polynom!
         SetLength(Result, 0)
      else begin
         // of the 1st degree
         SetLength(Result, 1);
         Result[0] := Complex(-a[0]/a[1], 0);
      end;
   end else begin
      SetLength(Result, 2);
      d := Sqr(a[1]) - 4.0*a[0]*a[2];
      if (Abs(d) <= 2.0*cEpsilon*Sqr(a[1])) then begin
         // EQUAL REAL ROOTS
         Result[0] := Complex(-0.5*a[1]/a[2], 0);
         Result[1] := Result[0];
      end else begin
         r := Sqrt(Abs(d));
         if (d < 0) then begin
            // COMPLEX ROOTS
            x := -0.5*a[1]/a[2];
            y := Abs(0.5*r/a[2]);
            Result[0] := Complex(x, y);
            Result[1] := Complex(x,-y);
         end else begin
            // DISTINCT REAL ROOTS
            if (a[1] <> 0) then begin
               w := -(a[1] + FortranSign(r, a[1]));
               Result[0] := Complex(2.0*a[0]/w, 0);
               Result[1] := Complex(0.5*w/a[2], 0);
            end else begin
               x := Abs(0.5*r/a[2]);
               Result[0] := Complex( x, 0);
               Result[1] := Complex(-x, 0);
            end;
         end;
      end;
   end;
end;

// cbcrt
//
function cbcrt(const a : PExtendedArray) : TComplexArray;
var
   aq : array [0..2] of Extended;
   arg, c, cf, d, p, p1, q, q1, r, ra, rb, rq, rt, r1, tol,
   s, sf, sq, sum, t, t1, w, w1, w2, x, x1, x2, x3, y, y1, y2, y3 : Extended;
const
   rt3 : Extended = 1.7320508075689;

   procedure Goto70Remainder;
   begin
      {     HERE W IS MUCH LARGER IN MAGNITUDE THAN THE OTHER ROOTS.
            AS A RESULT, THE OTHER ROOTS MAY BE EXCEEDINGLY INACCURATE
            BECAUSE OF ROUNDOFF ERROR.  TO DEAL WITH THIS, A QUADRATIC
            IS FORMED WHOSE ROOTS ARE THE SAME AS THE SMALLER ROOTS OF
            THE CUBIC.  THIS QUADRATIC IS THEN SOLVED.
            THIS CODE WAS WRITTEN BY WILLIAM L. DAVIS (NSWC). }
      aq[0] := a[0];
      aq[1] := a[1] + a[0]/w;
      aq[2] := -a[3]*w;
      Result:=qdcrt(@aq[0]);
      SetLength(Result, 3);
      Result[2] := Complex(w, 0);
   end;

begin
   if a[3]=0 then begin
      // actually of a lower order
      Result:=qdcrt(@a[0]);
   end else if a[0]=0 then begin
      // zero is a trivial solution
      Result:=qdcrt(@a[1]);
      SetLength(Result, 3);
      Result[2]:=Complex(0, 0);
   end else begin
      SetLength(Result, 3);

      tol := 4*cEpsilon;

      p := a[2]/(3.0*a[3]);
      q := a[1]/a[3];
      r := a[0]/a[3];

      c := 0;
      t := a[1] - p*a[2];
      if (Abs(t) > tol*Abs(a[1])) then c := t/a[3];

      t := 2.0*Sqr(p) - q;
      if (Abs(t) <= tol*Abs(q)) then t := 0;
      d := r + p*t;
      if (Abs(d) <= tol*ABS(r)) then begin
         Result[0] := Complex(-p, 0);
         w := Sqrt(Abs(c));
         if (c < 0) then begin
            if (p <> 0) then begin
               x := -(p + FortranSign(w, p));
               Result[2] := Complex(x, 0);
               t := 3.0*a[0]/(a[2]*x);
               Result[1]:=Complex(t, 0);
            end else begin
               Result[1] := Complex(w, 0);
               Result[2] := Complex(-w, 0);
            end;
         end else begin
            Result[1] := Complex(-p, w);
            Result[2] := Complex(-p,-w);
         end;
         Exit;
      end;

      s := MaxFloat(Abs(a[0]), Abs(a[1]), Abs(a[2]));
      p1 := a[2]/(3.0*s);
      q1 := a[1]/s;
      r1 := a[0]/s;

      t1 := q - 2.25*Sqr(p);
      if (Abs(t1) <= tol*Abs(q)) then t1 := 0;
      w := 0.25*Sqr(r1);
      w1 := 0.5*p1*r1*t;
      w2 := Sqr(q1)*t1/27.0;
      if (w1 >= 0) then begin
         w := w + w1;
         sq := w + w2;
      end else begin
         if (w2 < 0) then
            sq := w + (w1 + w2)
         else begin
            w := w + w2;
            sq := w + w1;
         end;
      end;
      if (Abs(sq) <= tol*w) then sq := 0;
      rq := Abs(s/a[3])*Sqrt(Abs(sq));
// Note by Eric Grange
// test in original code was against '<0', which is wrong, unless I'm mistaken
      if (sq <= 0) then begin
         // ALL ROOTS ARE REAL

         arg := ArcTan2(rq, -0.5*d);
         cf := Cos(arg/3.0);
         sf := Sin(arg/3.0);
         rt := Sqrt(-c/3.0);
         y1 := 2.0*rt*cf;
         y2 := -rt*(cf + rt3*sf);
         y3 := -(d/y1)/y2;

         x1 := y1 - p;
         x2 := y2 - p;
         x3 := y3 - p;
         if (Abs(x1) > Abs(x2)) then begin
            t := x1;
            x1 := x2;
            x2 := t;
         end;
         if (Abs(x2) > ABS(x3)) then begin
            t := x2;
            x2 := x3;
            x3 := t;
            if (Abs(x1) > Abs(x2)) then begin
               t := x1;
               x1 := x2;
               x2 := t;
            end;
         end;

         w := x3;
         if (Abs(x2) < 0.1*Abs(x3)) then begin
            Goto70Remainder;
            Exit;
         end;
         if (ABS(x1) < 0.1*Abs(x2)) then
            x1 := - (r/x3)/x2;
         Result[0] := Complex(x1, 0);
         Result[1] := Complex(x2, 0);
         Result[2] := Complex(x3, 0);
      end else begin
         // REAL AND COMPLEX ROOTS
         ra := cbrt(-0.5*d - FortranSign(rq,d));
         rb := -c/(3.0*ra);
         t := ra + rb;
         w := -p;
         x := -p;
         if (ABS(t) > tol*ABS(ra)) then begin
            w := t - p;
            x := -0.5*t - p;
            if (Abs(x) <= tol*Abs(p)) then x := 0;
         end;
         t := Abs(ra - rb);
         y := 0.5*rt3*t;

         if (t > tol*Abs(ra)) then begin
            if (Abs(x) < Abs(y)) then begin
               s := Abs(y);
               t := x/y;
            end else begin
               s := Abs(x);
               t := y/x;
            end;
            if (s < 0.1*Abs(w)) then
               Goto70Remainder
            else begin
               w1 := w/s;
               sum := 1.0 + Sqr(t);
               if (Sqr(w1) < 0.01*sum) then
                  w := - ((r/sum)/s)/s;
               Result[0] := Complex(w, 0);
               Result[1] := Complex(x, y);
               Result[2] := Complex(x,-y);
            end;
         end else begin
            // AT LEAST TWO ROOTS ARE EQUAL
            if (Abs(x) < Abs(w)) then begin
               if (Abs(x) < 0.1*Abs(w)) then
                  Goto70Remainder
               else begin
                  Result[0] := Complex(x, 0);
                  Result[1] := Result[0];
                  Result[2] := Complex(w, 0);
               end;
            end else begin
               if (Abs(w) < 0.1*Abs(x)) then
                  w := - (r/x)/x;
               Result[0] := Complex(w, 0);
               Result[1] := Complex(x, 0);
               Result[2] := Result[1];
            end;
         end;
      end;
   end;
end;

// qtcrt
//
function qtcrt(const a : PExtendedArray) : TComplexArray;
var
   w : TComplex;
   b, b2, c, d, e, h, p, q, r, t, u, v, v1, v2,
                x, x1, x2, x3, y : Extended;
   temp : array of Extended;

   procedure Goto50Remainder; // couldn't sort that goto out... :(
   begin
      x := -u - b;
      y := v1 - v2;
      Result[0] := Complex(x, y);
      Result[1] := Complex(x,-y);
      x :=  u - b;
      y := v1 + v2;
      Result[2] := Complex(x, y);
      Result[3] := Complex(x,-y);
   end;

   procedure Goto70Remainder; // ...and this one either... :(
   begin
      w := ComplexSqrt(Result[1]);
      u := 2.0*W.Real;
      v := 2.0*Abs(w.Imag);
      t := x - b;
      x1:= t + u;
      x2:= t - u;
      if (Abs(x1) > Abs(x2)) then begin
         t :=x1;
         x1:=x2;
         x2:=t;
      end;
      u := -x - b;
      h := Sqr(u) + Sqr(v);
      if (x1*x1 < 0.01*MinFloat(Sqr(x2), h)) then
         x1 := e/(x2*h);
      Result[0] := Complex(x1, 0);
      Result[1] := Complex(x2, 0);
      Result[2] := Complex(u, v);
      Result[3] := Complex(u,-v);
   end;

begin
   if a[4]=0 then begin
      // actually of a lower order
      Result:=cbcrt(@a[0]);
   end else if a[0]=0 then begin
      // zero is a trivial solution
      Result:=cbcrt(@a[1]);
      SetLength(Result, 4);
      Result[3]:=Complex(0, 0);
   end else begin
      b := a[3]/(4.0*a[4]);
      c := a[2]/a[4];
      d := a[1]/a[4];
      e := a[0]/a[4];
      b2 := Sqr(b);

      p := 0.5*(c - 6.0*b2);
      q := d - 2.0*b*(c - 4.0*b2);
      r := b2*(c - 3.0*b2) - b*d + e;

      //  SOLVE THE RESOLVENT CUBIC EQUATION. THE CUBIC HAS AT LEAST ONE
      //  NONNEGATIVE REAL ROOT.  IF W1, W2, W3 ARE THE ROOTS OF THE CUBIC
      //  THEN THE ROOTS OF THE ORIGINIAL EQUATION ARE
      //
      //  Z = -B + CSQRT(W1) + CSQRT(W2) + CSQRT(W3)
      //
      //  WHERE THE SIGNS OF THE SQUARE ROOTS ARE CHOSEN SO
      //  THAT CSQRT(W1) * CSQRT(W2) * CSQRT(W3) = -Q/8.

      SetLength(temp, 4);
      temp[0] := -Sqr(q)/64.0;
      temp[1] := 0.25*(Sqr(p) - r);
      temp[2] := p;
      temp[3] := 1.0;

      Result:=cbcrt(@temp[0]);
      SetLength(Result, 4);
      if (Result[1].Imag = 0) then begin
         // THE RESOLVENT CUBIC HAS ONLY REAL ROOTS
         // REORDER THE ROOTS IN INCREASING ORDER
         x1 := Result[0].Real;
         x2 := Result[1].Real;
         x3 := Result[2].Real;
         if (x1 > x2) then begin
            t :=x1;
            x1:=x2;
            x2:=t;
         end;
         if (x2 > x3) then begin
            t :=x2;
            x2:=x3;
            x3:=t;
            if (x1 > x2) then begin
               t :=x1;
               x1:=x2;
               x2:=t;
            end;
         end;
         u:=0;
         if (x3 > 0) then
            u := Sqrt(x3);
         if (x2 <= 0) then begin
            v1 := Sqrt(Abs(x1));
            v2 := Sqrt(Abs(x2));
            if (q < 0) then u := -u;
            Goto50Remainder;
            Exit;
         end;

         if (x1 < 0) then begin
            if (Abs(x1) > x2) then begin
               v1 := Sqrt(Abs(x1));
               v2 := 0;
               Goto50Remainder;
               Exit;
            end;
            x1 := 0;
         end;

         x1 := Sqrt(x1);
         x2 := Sqrt(x2);
         if (q > 0) then x1 := -x1;
         temp[0] := (( x1 + x2) + u) - b;
         temp[1] := ((-x1 - x2) + u) - b;
         temp[2] := (( x1 - x2) - u) - b;
         temp[3] := ((-x1 + x2) - u) - b;
// Note by Eric Grange
// This was in the original code, but I find it highly aggressive of a rounding...
//
{         SortArrayAscending(temp);
         if (Abs(temp[0]) < 0.1*Abs(temp[3])) then begin
            t := temp[1]*temp[2]*temp[3];
            if (t <> 0) then temp[0] := e/t;
         end; }
         Result[0] := Complex(temp[0], 0);
         Result[1] := Complex(temp[1], 0);
         Result[2] := Complex(temp[2], 0);
         Result[3] := Complex(temp[3], 0);
      end else begin
         // THE RESOLVENT CUBIC HAS COMPLEX ROOTS
         t := Result[0].Real;
         x := 0;
         if (t >= 0) then begin
            if (t <> 0) then begin
               x := Sqrt(t);
               if (q > 0) then x := -x;
            end;
            Goto70Remainder;
         end else begin
            h := Abs(Result[1].Real) + Abs(Result[1].Imag);
            if (Abs(t) <= h) then
               Goto70Remainder
            else begin
               v := Sqrt(Abs(t));
               Result[0] := Complex(-b, v);
               Result[1] := Complex(-b,-v);
               Result[2] := Result[0];
               Result[3] := Result[1];
            end;
         end;
      end;
   end;
end;

{

MODULE constants_NSWC
! Contains the NSWC functions IPMPAR, SPMPAR, DPMPAR, EPSLN, DEPSLN,
! EXPARG & DXPARG
!-----------------------------------------------------------------------
!     WRITTEN using F90 intrinsics by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     Latest revision - 1 February 1997
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)

CONTAINS

FUNCTION ipmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
!     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
!     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...

!  INTEGERS.

!     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM

!               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )

!               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.

!     IPMPAR(1) = A, THE BASE (radix).

!     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS (digits).

!     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE (huge).

!  FLOATING-POINT NUMBERS.

!     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
!     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
!     NONZERO NUMBERS ARE REPRESENTED IN THE FORM

!               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)

!               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
!               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.

!     IPMPAR(4) = B, THE BASE.

!  SINGLE-PRECISION

!     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.

!     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.

!     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.

!  DOUBLE-PRECISION

!     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.

!     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.

!     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.

!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
INTEGER             :: fn_val

SELECT CASE(i)
  CASE( 1)
    fn_val = RADIX(i)
  CASE( 2)
    fn_val = DIGITS(i)
  CASE( 3)
    fn_val = HUGE(i)
  CASE( 4)
    fn_val = RADIX(1.0)
  CASE( 5)
    fn_val = DIGITS(1.0)
  CASE( 6)
    fn_val = MINEXPONENT(1.0)
  CASE( 7)
    fn_val = MAXEXPONENT(1.0)
  CASE( 8)
    fn_val = DIGITS(1.0D0)
  CASE( 9)
    fn_val = MINEXPONENT(1.0D0)
  CASE(10)
    fn_val = MAXEXPONENT(1.0D0)
  CASE DEFAULT
    RETURN
END SELECT

RETURN
END FUNCTION ipmpar



FUNCTION spmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION spmpar



FUNCTION dpmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     DPMPAR PROVIDES THE DOUBLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     DOUBLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        DPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        DPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        DPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION dpmpar


FUNCTION epsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.0 + EPS .GT. 1.0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION epsln


FUNCTION exparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     EXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION exparg


FUNCTION depsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.D0 + EPS .GT. 1.D0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION depsln


FUNCTION dxparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  DXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     DEXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  DXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF DEXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR DXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION dxparg

END MODULE constants_NSWC



SUBROUTINE aord (a, n)
!-----------------------------------------------------------------------
! THE AORD SORTING PROCEDURE IS USED TO REORDER THE ELEMENTS OF A SO THAT
! ABS(A(I)) .LE. ABS(A(I+1)) FOR I = 1,...,N-1.  IT IS ASSUMED THAT N >= 1.
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN OUT) :: a(:)
INTEGER, INTENT(IN)       :: n

! Local variables
INTEGER   :: k(10) = (/ 1, 4, 13, 40, 121, 364, 1093, 3280, 9841, 28524 /), &
             imax, i, ii, ki, jmax, j, l, ll
REAL (dp) :: s
!------------------------

!             SELECTION OF THE INCREMENTS K(I) = (3**I-1)/2

IF (n < 2) RETURN
imax = 1
DO i = 3,10
  IF (n <= k(i)) EXIT
  imax = imax + 1
END DO

!            STEPPING THROUGH THE INCREMENTS K(IMAX),...,K(1)

i = imax
DO ii = 1,imax
  ki = k(i)
  
!             SORTING ELEMENTS THAT ARE KI POSITIONS APART
!                 SO THAT ABS(A(J)) .LE. ABS(A(J+KI))
  
  jmax = n - ki
  DO j = 1,jmax
    l = j
    ll = j + ki
    s = a(ll)
    DO
      IF (ABS(s) >= ABS(a(l))) EXIT
      a(ll) = a(l)
      ll = l
      l = l - ki
      IF (l <= 0) EXIT
    END DO
    a(ll) = s
  END DO
  
  i = i - 1
END DO
RETURN
END SUBROUTINE aord


FUNCTION cbrt (x) RESULT(fn_val)
!-----------------------------------------------------------------------
!                   CUBE ROOT OF A REAL NUMBER
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN) :: x
REAL (dp)             :: fn_val

!     Local variable
REAL (dp) :: r, zero = 0.0_dp, three = 3.0_dp

IF (x < zero) THEN
  r = LOG(-x) / three
  fn_val = -EXP(r)
ELSE IF (x > zero) THEN
  r = LOG(x) / three
  fn_val = EXP(r)
ELSE
  fn_val = zero
END IF

RETURN
END FUNCTION cbrt


SUBROUTINE qdcrt (a, z)
!-----------------------------------------------------------------------

!        QDCRT COMPUTES THE ROOTS OF THE REAL POLYNOMIAL
!              A(1) + A(2)*Z + A(3)*Z**2
!     AND STORES THE RESULTS IN Z.  IT IS ASSUMED THAT A(3) IS NONZERO.
!-----------------------------------------------------------------------
!     Converted to be compatible with ELF90 by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     WWW-page: http://www.mel.dms.csiro.au/~alan
!     Latest revision - 27 February 1997
!-----------------------------------------------------------------------

USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)     :: a(:)
COMPLEX (dp), INTENT(OUT) :: z(:)
!-----------------------------------------------------------------------

! Local variables
REAL (dp) :: d, eps, r, w, x, y, zero = 0.0_dp

!     ***** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE
!           SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0.

eps = dpmpar(1)

!-------------------
IF (a(1) == zero) GO TO 40
d = a(2)*a(2) - 4.0D0*a(1)*a(3)
IF (ABS(d) <= 2.0D0*eps*a(2)*a(2)) GO TO 20
r = SQRT(ABS(d))
IF (d < zero) GO TO 30

!                 DISTINCT REAL ROOTS

IF (a(2) /= zero) GO TO 10
x = ABS(0.5D0*r/a(3))
z(1) = CMPLX(x, zero, dp)
z(2) = CMPLX(-x, zero, dp)
RETURN

10 w = -(a(2) + SIGN(r,a(2)))
z(1) = CMPLX(2.0D0*a(1)/w, zero, dp)
z(2) = CMPLX(0.5D0*w/a(3), zero, dp)
RETURN

!                  EQUAL REAL ROOTS

20 z(1) = CMPLX(-0.5D0*a(2)/a(3), zero, dp)
z(2) = z(1)
RETURN

!                   COMPLEX ROOTS

30 x = -0.5D0*a(2)/a(3)
y = ABS(0.5D0*r/a(3))
z(1) = CMPLX(x, y, dp)
z(2) = CMPLX(x,-y, dp)
RETURN

!                 CASE WHEN A(1) = 0

40 z(1) = CMPLX(zero, zero, dp)
z(2) = CMPLX(-a(2)/a(3), zero, dp)
RETURN
END SUBROUTINE qdcrt


SUBROUTINE cbcrt (a, z)
!-----------------------------------------------------------------------

!        CBCRT COMPUTES THE ROOTS OF THE REAL POLYNOMIAL
!              A(1) + A(2)*Z + A(3)*Z**2 + A(4)*Z**3
!     AND STORES THE RESULTS IN Z. IT IS ASSUMED THAT A(4) IS NONZERO.

!-----------------------------------------------------------------------
!     WRITTEN BY ALFRED H. MORRIS
!        NAVAL SURFACE WEAPONS CENTER
!        DAHLGREN, VIRGINIA
!-----------------------------------------------------------------------
!     Converted to be compatible with ELF90 by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     WWW-page: http://www.mel.dms.csiro.au/~alan
!     Latest revision - 27 February 1997
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)     :: a(:)
COMPLEX (dp), INTENT(OUT) :: z(:)

INTERFACE
  SUBROUTINE qdcrt (a, z)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)     :: a(:)
    COMPLEX (dp), INTENT(OUT) :: z(:)
  END SUBROUTINE qdcrt
  FUNCTION cbrt (x) RESULT(fn_val)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fn_val
  END FUNCTION cbrt
END INTERFACE

!-------------------
REAL (dp) :: aq(3), arg, c, cf, d, eps, p, p1, q, q1, r, ra, rb, rq, rt, &
             rt3 = 1.7320508075689D0, r1, s, sf, sq, sum, t, tol, t1, w, &
             w1, w2, x, x1, x2, x3, y, y1, y2, y3, zero = 0.0_dp
!-----------------------------------------------------------------------

!     ***** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE
!           SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0.

eps = dpmpar(1)

!-------------------
IF (a(1) == zero) GO TO 100
p = a(3)/(3.0D0*a(4))
q = a(2)/a(4)
r = a(1)/a(4)
tol = 4.0D0*eps

c = zero
t = a(2) - p*a(3)
IF (ABS(t) > tol*ABS(a(2))) c = t/a(4)

t = 2.0D0*p*p - q
IF (ABS(t) <= tol*ABS(q)) t = zero
d = r + p*t
IF (ABS(d) <= tol*ABS(r)) GO TO 110

!           SET  SQ = (A(4)/S)**2 * (C**3/27 + D**2/4)

s = MAX(ABS(a(1)), ABS(a(2)), ABS(a(3)))
p1 = a(3)/(3.0D0*s)
q1 = a(2)/s
r1 = a(1)/s

t1 = q - 2.25D0*p*p
IF (ABS(t1) <= tol*ABS(q)) t1 = zero
w = 0.25D0*r1*r1
w1 = 0.5D0*p1*r1*t
w2 = q1*q1*t1/27.0D0
IF (w1 < zero) GO TO 10
w = w + w1
sq = w + w2
GO TO 12
10 IF (w2 < zero) GO TO 11
w = w + w2
sq = w + w1
GO TO 12
11 sq = w + (w1 + w2)
12 IF (ABS(sq) <= tol*w) sq = zero
rq = ABS(s/a(4))*SQRT(ABS(sq))
IF (sq >= zero) GO TO 40

!                   ALL ROOTS ARE REAL

arg = ATAN2(rq, -0.5D0*d)
cf = COS(arg/3.0D0)
sf = SIN(arg/3.0D0)
rt = SQRT(-c/3.0D0)
y1 = 2.0D0*rt*cf
y2 = -rt*(cf + rt3*sf)
y3 = -(d/y1)/y2

x1 = y1 - p
x2 = y2 - p
x3 = y3 - p
IF (ABS(x1) <= ABS(x2)) GO TO 20
t = x1
x1 = x2
x2 = t
20 IF (ABS(x2) <= ABS(x3)) GO TO 30
t = x2
x2 = x3
x3 = t
IF (ABS(x1) <= ABS(x2)) GO TO 30
t = x1
x1 = x2
x2 = t

30 w = x3
IF (ABS(x2) < 0.1D0*ABS(x3)) GO TO 70
IF (ABS(x1) < 0.1D0*ABS(x2)) x1 = - (r/x3)/x2
z(1) = CMPLX(x1, zero, dp)
z(2) = CMPLX(x2, zero, dp)
z(3) = CMPLX(x3, zero, dp)
RETURN

!                  REAL AND COMPLEX ROOTS

40 ra = cbrt(-0.5D0*d - SIGN(rq,d))
rb = -c/(3.0D0*ra)
t = ra + rb
w = -p
x = -p
IF (ABS(t) <= tol*ABS(ra)) GO TO 41
w = t - p
x = -0.5D0*t - p
IF (ABS(x) <= tol*ABS(p)) x = zero
41 t = ABS(ra - rb)
y = 0.5D0*rt3*t

IF (t <= tol*ABS(ra)) GO TO 60
IF (ABS(x) < ABS(y)) GO TO 50
s = ABS(x)
t = y/x
GO TO 51
50 s = ABS(y)
t = x/y
51 IF (s < 0.1D0*ABS(w)) GO TO 70
w1 = w/s
sum = 1.0D0 + t*t
IF (w1*w1 < 0.01D0*sum) w = - ((r/sum)/s)/s
z(1) = CMPLX(w,zero, dp)
z(2) = CMPLX(x, y, dp)
z(3) = CMPLX(x,-y, dp)
RETURN

!               AT LEAST TWO ROOTS ARE EQUAL

60 IF (ABS(x) < ABS(w)) GO TO 61
IF (ABS(w) < 0.1D0*ABS(x)) w = - (r/x)/x
z(1) = CMPLX(w, zero, dp)
z(2) = CMPLX(x, zero, dp)
z(3) = z(2)
RETURN
61 IF (ABS(x) < 0.1D0*ABS(w)) GO TO 70
z(1) = CMPLX(x, zero, dp)
z(2) = z(1)
z(3) = CMPLX(w, zero, dp)
RETURN

!     HERE W IS MUCH LARGER IN MAGNITUDE THAN THE OTHER ROOTS.
!     AS A RESULT, THE OTHER ROOTS MAY BE EXCEEDINGLY INACCURATE
!     BECAUSE OF ROUNDOFF ERROR.  TO DEAL WITH THIS, A QUADRATIC
!     IS FORMED WHOSE ROOTS ARE THE SAME AS THE SMALLER ROOTS OF
!     THE CUBIC.  THIS QUADRATIC IS THEN SOLVED.

!     THIS CODE WAS WRITTEN BY WILLIAM L. DAVIS (NSWC).

70 aq(1) = a(1)
aq(2) = a(2) + a(1)/w
aq(3) = -a(4)*w
CALL qdcrt(aq, z)
z(3) = CMPLX(w, zero, dp)

IF (AIMAG(z(1)) == zero) RETURN
z(3) = z(2)
z(2) = z(1)
z(1) = CMPLX(w, zero, dp)
RETURN
!-----------------------------------------------------------------------

!                  CASE WHEN A(1) = 0

100 z(1) = CMPLX(zero, zero, dp)
CALL qdcrt(a(2:), z(2:))
RETURN

!                   CASE WHEN D = 0

110 z(1) = CMPLX(-p, zero, dp)
w = SQRT(ABS(c))
IF (c < zero) GO TO 120
z(2) = CMPLX(-p, w, dp)
z(3) = CMPLX(-p,-w, dp)
RETURN

120 IF (p /= zero) GO TO 130
z(2) = CMPLX(w, zero, dp)
z(3) = CMPLX(-w, zero, dp)
RETURN

130 x = -(p + SIGN(w,p))
z(3) = CMPLX(x, zero, dp)
t = 3.0D0*a(1)/(a(3)*x)
IF (ABS(p) > ABS(t)) GO TO 131
z(2) = CMPLX(t, zero, dp)
RETURN
131 z(2) = z(1)
z(1) = CMPLX(t, zero, dp)
RETURN
END SUBROUTINE cbcrt


SUBROUTINE qtcrt (a, z)
!-----------------------------------------------------------------------

!         QTCRT COMPUTES THE ROOTS OF THE REAL POLYNOMIAL
!               A(1) + A(2)*Z + ... + A(5)*Z**4
!         AND STORES THE RESULTS IN Z. IT IS ASSUMED THAT A(5)
!         IS NONZERO.

!-----------------------------------------------------------------------
!     WRITTEN BY ALFRED H. MORRIS
!        NAVAL SURFACE WEAPONS CENTER
!        DAHLGREN, VIRGINIA
!-----------------------------------------------------------------------
!     Converted to be compatible with ELF90 by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     WWW-page: http://www.mel.dms.csiro.au/~alan
!     Latest revision - 27 February 1997
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)     :: a(:)
COMPLEX (dp), INTENT(OUT) :: z(:)

INTERFACE
  SUBROUTINE cbcrt (a, z)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)     :: a(:)
    COMPLEX (dp), INTENT(OUT) :: z(:)
  END SUBROUTINE cbcrt
  SUBROUTINE aord (a, n)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN OUT) :: a(:)
    INTEGER, INTENT(IN)       :: n
  END SUBROUTINE aord
END INTERFACE

!     Local variables
COMPLEX (dp) :: w
REAL (dp)    :: b, b2, c, d, e, h, p, q, r, t, temp(4), u, v, v1, v2,  &
                x, x1, x2, x3, y, zero = 0.0_dp

IF (a(1) == zero) GO TO 100
b = a(4)/(4.0D0*a(5))
c = a(3)/a(5)
d = a(2)/a(5)
e = a(1)/a(5)
b2 = b*b

p = 0.5D0*(c - 6.0D0*b2)
q = d - 2.0D0*b*(c - 4.0D0*b2)
r = b2*(c - 3.0D0*b2) - b*d + e

!          SOLVE THE RESOLVENT CUBIC EQUATION. THE CUBIC HAS AT LEAST ONE
!          NONNEGATIVE REAL ROOT.  IF W1, W2, W3 ARE THE ROOTS OF THE CUBIC
!          THEN THE ROOTS OF THE ORIGINIAL EQUATION ARE

!             Z = -B + CSQRT(W1) + CSQRT(W2) + CSQRT(W3)

!          WHERE THE SIGNS OF THE SQUARE ROOTS ARE CHOSEN SO
!          THAT CSQRT(W1) * CSQRT(W2) * CSQRT(W3) = -Q/8.

temp(1) = -q*q/64.0D0
temp(2) = 0.25D0*(p*p - r)
temp(3) =  p
temp(4) = 1.0D0
CALL cbcrt(temp,z)
IF (AIMAG(z(2)) /= zero) GO TO 60

!               THE RESOLVENT CUBIC HAS ONLY REAL ROOTS
!                REORDER THE ROOTS IN INCREASING ORDER

x1 = DBLE(z(1))
x2 = DBLE(z(2))
x3 = DBLE(z(3))
IF (x1 <= x2) GO TO 10
t = x1
x1 = x2
x2 = t
10 IF (x2 <= x3) GO TO 20
t = x2
x2 = x3
x3 = t
IF (x1 <= x2) GO TO 20
t = x1
x1 = x2
x2 = t

20 u = zero
IF (x3 > zero) u = SQRT(x3)
IF (x2 <= zero) GO TO 41
IF (x1 >= zero) GO TO 30
IF (ABS(x1) > x2) GO TO 40
x1 = zero

30 x1 = SQRT(x1)
x2 = SQRT(x2)
IF (q > zero) x1 = -x1
temp(1) = (( x1 + x2) + u) - b
temp(2) = ((-x1 - x2) + u) - b
temp(3) = (( x1 - x2) - u) - b
temp(4) = ((-x1 + x2) - u) - b
CALL aord (temp,4)
IF (ABS(temp(1)) >= 0.1D0*ABS(temp(4))) GO TO 31
t = temp(2)*temp(3)*temp(4)
IF (t /= zero) temp(1) = e/t
31 z(1) = CMPLX(temp(1), zero, dp)
z(2) = CMPLX(temp(2), zero, dp)
z(3) = CMPLX(temp(3), zero, dp)
z(4) = CMPLX(temp(4), zero, dp)
RETURN

40 v1 = SQRT(ABS(x1))
v2 = zero
GO TO 50
41 v1 = SQRT(ABS(x1))
v2 = SQRT(ABS(x2))
IF (q < zero) u = -u

50 x = -u - b
y = v1 - v2
z(1) = CMPLX(x, y, dp)
z(2) = CMPLX(x,-y, dp)
x =  u - b
y = v1 + v2
z(3) = CMPLX(x, y, dp)
z(4) = CMPLX(x,-y, dp)
RETURN

!                THE RESOLVENT CUBIC HAS COMPLEX ROOTS

60 t = DBLE(z(1))
x = zero
IF (t < zero) THEN
  GO TO 61
ELSE IF (t == zero) THEN
  GO TO 70
ELSE
  GO TO 62
END IF
61 h = ABS(DBLE(z(2))) + ABS(AIMAG(z(2)))
IF (ABS(t) <= h) GO TO 70
GO TO 80
62 x = SQRT(t)
IF (q > zero) x = -x

70 w = SQRT(z(2))
u = 2.0D0*DBLE(w)
v = 2.0D0*ABS(AIMAG(w))
t =  x - b
x1 = t + u
x2 = t - u
IF (ABS(x1) <= ABS(x2)) GO TO 71
t = x1
x1 = x2
x2 = t
71 u = -x - b
h = u*u + v*v
IF (x1*x1 < 0.01D0*MIN(x2*x2,h)) x1 = e/(x2*h)
z(1) = CMPLX(x1, zero, dp)
z(2) = CMPLX(x2, zero, dp)
z(3) = CMPLX(u, v, dp)
z(4) = CMPLX(u,-v, dp)
RETURN

80 v = SQRT(ABS(t))
z(1) = CMPLX(-b, v, dp)
z(2) = CMPLX(-b,-v, dp)
z(3) = z(1)
z(4) = z(2)
RETURN

!                         CASE WHEN A(1) = 0

100 z(1) = CMPLX(zero, zero, dp)
CALL cbcrt(a(2:), z(2:))
RETURN
END SUBROUTINE qtcrt



PROGRAM test_qtcrt
!-----------------------------------------------------------------------
!     Test program written to be compatible with ELF90 by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     WWW-page: http://www.mel.dms.csiro.au/~alan
!     Latest revision - 27 February 1997
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

INTEGER      :: degree, i
REAL (dp)    :: a(0:4)
COMPLEX (dp) :: z(4)

INTERFACE
  SUBROUTINE qdcrt (a, z)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)     :: a(:)
    COMPLEX (dp), INTENT(OUT) :: z(:)
  END SUBROUTINE qdcrt

  SUBROUTINE cbcrt (a, z)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)     :: a(:)
    COMPLEX (dp), INTENT(OUT) :: z(:)
  END SUBROUTINE cbcrt

  SUBROUTINE qtcrt (a, z)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)     :: a(:)
    COMPLEX (dp), INTENT(OUT) :: z(:)
  END SUBROUTINE qtcrt
END INTERFACE

WRITE(*, *)'  Solve quadratic, cubic or quartic eqns. with real coefficients'
WRITE(*, *)

DO
  WRITE(*, *)'Enter 2, 3, 4 for quadratic, cubic or quartic eqn.: '
  READ(*, *) degree
  SELECT CASE (degree)
    CASE (2)
      WRITE(*, *)'Enter a(0), a(1) then a(2): '
      READ(*, *) a(0), a(1), a(2)
      CALL qdcrt(a, z)
      WRITE(*, '(a, 2(/2g20.12))') ' Roots: REAL PART   IMAGINARY PART',  &
                                   (DBLE(z(i)), AIMAG(z(i)), i=1,2)
    CASE (3)
      WRITE(*, *)'Enter a(0), a(1), a(2) then a(3): '
      READ(*, *) a(0), a(1), a(2), a(3)
      CALL cbcrt(a, z)
      WRITE(*, '(a, 3(/2g20.12))') ' Roots: REAL PART   IMAGINARY PART',  &
                                   (DBLE(z(i)), AIMAG(z(i)), i=1,3)
    CASE (4)
      WRITE(*, *)'Enter a(0), a(1), a(2), a(3) then a(4): '
      READ(*, *) a(0), a(1), a(2), a(3), a(4)
      CALL qtcrt(a, z)
      WRITE(*, '(a, 4(/2g20.12))') ' Roots: REAL PART   IMAGINARY PART',  &
                                   (DBLE(z(i)), AIMAG(z(i)), i=1,4)
    CASE DEFAULT
      WRITE(*, *)'*** Try again! ***'
      WRITE(*, *)'Use Ctrl-C to exit the program'
  END SELECT
END DO

STOP
END PROGRAM test_qtcrt

}
end.
