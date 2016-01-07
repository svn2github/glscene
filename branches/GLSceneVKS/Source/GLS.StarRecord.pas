//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Unit to interface with simple star records aimed for background skies. 
}

unit GLS.StarRecord;

interface

uses
  GLS.VectorGeometry;

type
   TVKStarRecord = packed record
      RA : Word;              // x100 builtin factor, degrees
      DEC : SmallInt;         // x100 builtin factor, degrees
      BVColorIndex : Byte;    // x100 builtin factor
      VMagnitude : Byte;      // x10 builtin factor
   end;
   PGLStarRecord = ^TVKStarRecord;

{ Computes position on the unit sphere of a star record (Z=up). }
function StarRecordPositionZUp(const starRecord : TVKStarRecord) : TAffineVector;
{ Computes position on the unit sphere of a star record (Y=up). }
function StarRecordPositionYUp(const starRecord : TVKStarRecord) : TAffineVector;
{ Computes star color from BV index (RGB) and magnitude (alpha). }
function StarRecordColor(const starRecord : TVKStarRecord; bias : Single) : TVector;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

uses
  GLS.VectorTypes;
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StarRecordPositionYUp
//
function StarRecordPositionYUp(const starRecord : TVKStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCosine(starRecord.DEC*(0.01*PI/180), Result.V[1], f);
   SinCosine(starRecord.RA*(0.01*PI/180), f, Result.V[0], Result.V[2]);
end;

// StarRecordPositionZUp
//
function StarRecordPositionZUp(const starRecord : TVKStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCosine(starRecord.DEC*(0.01*PI/180), Result.V[2], f);
   SinCosine(starRecord.RA*(0.01*PI/180), f, Result.V[0], Result.V[1]);
end;

// StarRecordColor
//
function StarRecordColor(const starRecord : TVKStarRecord; bias : Single) : TVector;
const
   // very *rough* approximation
   cBVm035 : TVector = (X:0.7; Y:0.8; Z:1.0; W:1);
   cBV015  : TVector = (X:1.0; Y:1.0; Z:1.0; W:1);
   cBV060  : TVector = (X:1.0; Y:1.0; Z:0.7; W:1);
   cBV135  : TVector = (X:1.0; Y:0.8; Z:0.7; W:1);
var
   bvIndex100 : Integer;
begin
   bvIndex100:=starRecord.BVColorIndex-50;
   // compute RGB color for B&V index
   if bvIndex100<-035 then
      Result:=cBVm035
   else if bvIndex100<015 then
      VectorLerp(cBVm035, cBV015, (bvIndex100+035)*(1/(015+035)), Result)
   else if bvIndex100<060 then
      VectorLerp(cBV015, cBV060, (bvIndex100-015)*(1/(060-015)), Result)
   else if bvIndex100<135 then
      VectorLerp(cBV060, cBV135, (bvIndex100-060)*(1/(135-060)), Result)
   else Result:=cBV135;
   // compute transparency for VMag
   // the actual factor is 2.512, and not used here
   Result.V[3]:=PowerSingle(1.2, -(starRecord.VMagnitude*0.1-bias));
end;

end.
