{: Polynomials.<p>

   ********* IN PROGRESS - NOT FULLY STABLE **********

   Utility functions for manipulationg and solving polynomials.<p>

   Direct solving is supported for polynoms up to the 4th degree.<p>

   Polynom solving code based on Jochen Schwarze (schwarze@isa.de) solver
   published in Graphics Gem (1990).<p>

   Adapted to pascal by Eric Grange (egrange@glscene.org), if you find
   errors, they are probably mine. Note that contrary to the original code,
   the functions accept 'zero' values for any of the parameters.<br>
   I also made some changes for certain limit cases that (seemingly) weren't
   properly handled, these are marked by comments in the code.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>14/01/02 - EG - Switched to Jochen Schwarze's solver, droped complex stuff
      <li>22/08/01 - EG - Some fixes, qtcrt still no up to what I expected
	   <li>21/08/01 - EG - Creation
	</ul></font>

}
unit Polynomials;

interface

uses Geometry;

type
   TDoubleArray = array of Double;

{: Calculates the cube root of its parameter. }
function cbrt(const x : Double) : Double;

{: Computes the real roots of a real polynomial of the 2nd degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + A(2)*Z**2 }
function SolveQuadric(const c : PDoubleArray) : TDoubleArray;

{: Computes the real roots of a real polynomial of the 3rd degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + A(2)*Z**2 + A(3)*Z**3 }
function SolveCubic(const c : PDoubleArray) : TDoubleArray;

{: Computes the real roots of a real polynomial of the 4th degree.<p>
   The polynomial is of the form:<br>
   A(0) + A(1)*Z + ... + A(4)*Z**4 }
function SolveQuartic(const c : PDoubleArray) : TDoubleArray;

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

const
   cEpsilon : Double = 1e-50;

// IsZero
//
function IsZero(var v : Double) : Boolean;
begin
   Result:=(Abs(v)<cEpsilon);
end;

// cbrt
//
function cbrt(const x : Double) : Double;
begin
   if x>0 then
      Result:=Power(x, 1/3)
   else if x<0 then
      Result:=-Power(-x, 1/3)
   else Result:=0;
end;

// SolveQuadric
//
function SolveQuadric(const c : PDoubleArray) : TDoubleArray;
var
   p, q, D, sqrt_D : Double;
begin
    // normal form: x^2 + px + q = 0

    p := c[1]/(2*c[2]);
    q := c[0]/c[2];

    D := Sqr(p)-q;

    if IsZero(D) then begin
      SetLength(Result, 1);
      Result[0]:=-p;
    end else if D>0 then begin
      sqrt_D:=Sqrt(D);
      SetLength(Result, 2);
      Result[0]:=sqrt_D-p;
      Result[1]:=-sqrt_D-p;
    end else // if (D < 0)
      SetLength(Result, 0);
end;

// SolveCubic
//
function SolveCubic(const c : PDoubleArray) : TDoubleArray;
var
   i : Integer;
   sub : Double;
   A, B, Cc : Double;
   sq_A, p, q : Double;
   cb_p, D : Double;
   u, v, phi, t, sqrt_D : Double;
begin
   // normal form: x^3 + Ax^2 + Bx + C = 0

   A := c[2]/c[3];
   B := c[1]/c[3];
   Cc := c[0]/c[3];

   // substitute x = y - A/3 to eliminate quadric term:
	// x^3 +px + q = 0

   sq_A := Sqr(A);
   p := 1.0/3 * (- 1.0/3 * sq_A + B);
   q := 1.0/2 * (2.0/27 * A * sq_A - 1.0/3 * A * B + Cc);

   // use Cardano's formula

   cb_p := Sqr(p) * p;
   D := Sqr(q) + cb_p;

   if IsZero(D) then begin
	   if IsZero(q) then begin // one triple solution
         SetLength(Result, 1);
	      Result[0]:=0;
	   end else begin // one single and one double solution
         u:=cbrt(-q);
         SetLength(Result, 2);
	      Result[0]:=2*u;
	      Result[1]:=-u;
      end;
   end else if D<0 then begin // Casus irreducibilis: three real solutions
	   phi:=1.0/3*ArcCos(-q/Sqrt(-cb_p));
	   t:=2*Sqrt(-p);
      SetLength(Result, 3);
	   Result[0]:= t*Cos(phi);
	   Result[1]:=-t*Cos(phi+PI/3);
	   Result[2]:=-t*Cos(phi-PI/3);
   end else begin // one real solution
   	sqrt_D:=Sqrt(D);
	   u:=cbrt(sqrt_D-q);
   	v:=-cbrt(sqrt_D+q);
      SetLength(Result, 1);
      Result[0]:=u+v;
   end;

   // resubstitute

   sub := 1.0/3 * A;

   for i:=0 to High(Result) do
	   Result[i]:=Result[i]-sub;
end;

// SolveQuartic
//
function SolveQuartic(const c : PDoubleArray) : TDoubleArray;
var
   coeffs : array [0..3] of Double;
   z, u, v, sub : Double;
   A, B, Cc, D : Double;
   sq_A, p, q, r : Double;
   i, n, num : Integer;
   temp : TDoubleArray;
begin
   // normal form: x^4 + Ax^3 + Bx^2 + Cx + D = 0

   A := c[ 3 ] / c[ 4 ];
   B := c[ 2 ] / c[ 4 ];
   Cc:= c[ 1 ] / c[ 4 ];
   D := c[ 0 ] / c[ 4 ];

   // substitute x = y - A/4 to eliminate cubic term:
	// x^4 + px^2 + qx + r = 0

   sq_A := Sqr(A);
   p := - 3.0/8 * sq_A + B;
   q := 1.0/8 * sq_A * A - 1.0/2 * A * B + Cc;
   r := - 3.0/256*Sqr(sq_A) + 1.0/16*sq_A*B - 1.0/4*A*Cc + D;

   if IsZero(r) then begin
	   // no absolute term: y(y^3 + py + q) = 0

	   coeffs[ 0 ] := q;
	   coeffs[ 1 ] := p;
	   coeffs[ 2 ] := 0;
	   coeffs[ 3 ] := 1;

	   Result:=SolveCubic(@coeffs[0]);
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:=0;

      SetLength(temp, 0);
   end else begin
	   // solve the resolvent cubic ...

      coeffs[ 0 ] := 1.0/2 * r * p - 1.0/8 * q * q;
      coeffs[ 1 ] := - r;
      coeffs[ 2 ] := - 1.0/2 * p;
      coeffs[ 3 ] := 1;

	   Result:=SolveCubic(@coeffs[0]);

	   // ... and take the one real solution ...

   	z := Result[0];

	   // ... to build two quadric equations

   	u := Sqr(z) - r;
	   v := 2 * z - p;

	   if IsZero(u) then
	      u := 0
	   else if u > 0 then
	      u := Sqrt(u)
	   else begin
         SetLength(Result, 0);
         Exit;
      end;

	   if IsZero(v) then
	      v := 0
	   else if v > 0 then
	      v := Sqrt(v)
	   else begin
         SetLength(Result, 0);
         Exit;
      end;

	   coeffs[ 0 ] := z - u;
      if q<0 then
      	coeffs[ 1 ] := -v
      else coeffs[ 1 ] := v;
	   coeffs[ 2 ] := 1;

	   Result:=SolveQuadric(@coeffs[0]);

   	coeffs[0]:=z+u;
      if q<0 then
      	coeffs[ 1 ] := v
      else coeffs[ 1 ] := -v;
   	coeffs[2]:=1;

      temp:=SolveQuadric(@coeffs[0]);
      n:=Length(Result);
      SetLength(Result, n+Length(temp));
      for i:=0 to High(temp) do
         Result[n+i]:=temp[i];

      // resubstitute

      sub := 1.0/4 * A;

      for i:=0 to High(Result) do
         Result[i]:=Result[i]-sub;
   end;
end;

end.
