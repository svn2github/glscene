unit UFourier;

(**************************************************************************
 ************************* UFourier ***************************************
 * Purpose: Implements Fast Fourier Transform and InverseFastFourierTrans *
 *                                                                        *
 *                                                                        *
 * procedures                                                             *
 *   FFT(_NumSamples           : Integer;                                 *
 *       _InValues, _OutValues : TComplex1DArray);                        *
 *                                                                        *
 *   IFFT(_NumSamples            : Integer;                               *
 *        _InValues, _OutValues : TComplex1DArray);                       *
 *                                                                        *
 * Note:                                                                  *
 *  Based on the DMath Package by Jean Debord                             *
 *  (http://www.unilim.fr/pages_perso/jean.debord/index.htm)              *
 *                                                                        *
 *  Adapted to use TComplex and TComplex1DArrays - 06.09.2004             *
 * ---------------------------------------------------------------------- *
 * (c) W.Blecher 2004 - ITMC RWTH Aachen                                  *
 **************************************************************************
 **************************************************************************)

interface

uses  SysUtils,
      UGlobalTools;

procedure FFT(_NumSamples    : Integer;
              _InValues      : TComplex1DArray;
              var _OutValues : TComplex1DArray);

procedure IFFT(_NumSamples            : Integer;
               _InValues      : TComplex1DArray;
               var _OutValues : TComplex1DArray);

implementation

const MaxPowerOfTwo : integer = 25;

(***************************************************************
 ************************** IsPowerOfTwo ***********************
 * Purpose: Checks if an Integer is a Power of Two             *
 *                                                             *
 * Input:   _X : Integer                                       *
 *                                                             *
 *        Number to check                                      *
 * Output: True/False                                          *
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
function IsPowerOfTwo(_X : Integer) : Boolean;
var
  I, Y : Integer;
begin
  Y := 2;
  for I := 1 to Pred(MaxPowerOfTwo) do
    begin
      if _X = Y then
        begin
          IsPowerOfTwo := True;
          Exit;
        end;
      Y := Y shl 1;
    end;
  IsPowerOfTwo := False;
end;

(***************************************************************
 ************************** NumberOfBitsNeeded *****************
 * Purpose: How much Bits are needed to store the max value    *
 *                                                             *
 * Input:   _PowerOfTwo : Integer                              *
 *        Max Array Index                                      *

 * Output: Integer                                             *
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
function NumberOfBitsNeeded(_PowerOfTwo : Integer) : Integer;
var
  I : Integer;
begin
  for I := 0 to MaxPowerOfTwo do
    begin
      if (_PowerOfTwo and (1 shl I)) <> 0 then
        begin
          NumberOfBitsNeeded := I;
          Exit;
        end;
    end;
  NumberOfBitsNeeded := 0;
end;

(***************************************************************
 ************************ ReverseBits **************************
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
function ReverseBits(_Index, _NumBits : Integer) : Integer;
var
  I, Rev : Integer;
begin
  Rev := 0;
  for I := 0 to _NumBits - 1 do
    begin
      Rev := (Rev shl 1) or (_Index and 1);
      _Index := _Index shr 1;
    end;
  ReverseBits := Rev;
end;

(***************************************************************
 ************************* FourierTransform ********************
 * Purpose: Calculate the Fourier Transform of _ValsIn         *
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
procedure FourierTransform(_AngleNumerator : Single;
                           _NumSamples     : Integer;
                           _ValsIn         : TComplex1DArray;
                           var _ValsOut    : TComplex1DArray);
var
  NumBits, I, J, K, N, BlockSize, BlockEnd : Integer;
  Delta_angle, Delta_ar                    : Single;
  Alpha, Beta                              : Single;
  Tr, Ti, Ar, Ai                           : Single;
begin
  if not IsPowerOfTwo(_NumSamples) or (_NumSamples < 2) then
    begin
      raise Exception.Create('Error in procedure Fourier: NumSamples=' + IntToStr(_NumSamples)+ ' is not a positive integer power of 2');
    end;

  NumBits := NumberOfBitsNeeded(_NumSamples);
  for I := 0 to _NumSamples - 1 do
    begin
      J := ReverseBits(I, NumBits);
      _ValsOut[J].Re := _ValsIn[I].Re;
      _ValsOut[J].img := _ValsIn[I].Img;
    end;

  BlockEnd := 1;
  BlockSize := 2;
  while BlockSize <= _NumSamples do
    begin
      Delta_angle := _AngleNumerator / BlockSize;
      Alpha := Sin(0.5 * Delta_angle);
      Alpha := 2.0 * Alpha * Alpha;
      Beta := Sin(Delta_angle);

      I := 0;
      while I < _NumSamples do
        begin
          Ar := 1.0;    (* cos(0) *)
          Ai := 0.0;    (* sin(0) *)

          J := I;
          for N := 0 to BlockEnd - 1 do
            begin
              K := J + BlockEnd;
              Tr := Ar * _ValsOut[K].Re - Ai * _ValsOut[K].Img;
              Ti := Ar * _ValsOut[K].Img + Ai * _ValsOut[K].Re;
              _ValsOut[K].Re := _ValsOut[J].Re - Tr;
              _ValsOut[K].Img := _ValsOut[J].Img - Ti;
              _ValsOut[J].Re := _ValsOut[J].Re + Tr;
              _ValsOut[J].Img := _ValsOut[J].Img + Ti;
              Delta_ar := Alpha * Ar + Beta * Ai;
              Ai := Ai - (Alpha * Ai - Beta * Ar);
              Ar := Ar - Delta_ar;
              Inc(J);
            end;

          I := I + BlockSize;
        end;

      BlockEnd := BlockSize;
      BlockSize := BlockSize shl 1;
    end;
end;

(***************************************************************
 ************************* FFT *********************************
 * Purpose: Caller for calculation of Fast Fourier Transform   *
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
procedure FFT(_NumSamples    : Integer;
              _InValues      : TComplex1DArray;
              var _OutValues : TComplex1DArray);
begin
  FourierTransform(2 * PI, _NumSamples, _InValues, _OutValues);
end;

(***************************************************************
 ************************ IFFT *********************************
 * Purpose: Caller of Iverse Fast Fourier Transform            *
 * ----------------------------------------------------------- *
 * http://www.unilim.fr/pages_perso/jean.debord/index.htm      *
 ***************************************************************
 ***************************************************************)
procedure IFFT(_NumSamples    : Integer;
               _InValues      : TComplex1DArray;
               var _OutValues : TComplex1DArray);
var
  I : Integer;
begin
  FourierTransform(- 2 * PI, _NumSamples, _InValues, _OutValues);

  { Normalize the resulting time samples }
  for I := 0 to _NumSamples - 1 do
    begin
      _OutValues[I].Re := _OutValues[I].Re / _NumSamples;
      _OutValues[I].Img := _OutValues[I].Img / _NumSamples;
    end;
end;

end.
