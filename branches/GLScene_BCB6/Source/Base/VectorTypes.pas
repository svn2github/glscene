//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorTypes<p>

   Defines base vector types for use in Geometry.pas and OpenGL1x.pas.<p>

   The sole aim of this unit is to limit dependency between the Geometry
   and OpenGL1x units by introducing the base compatibility types
   (and only the *base* types).<p>

   Conventions:<ul>
      <li><b>d</b> is used for Double precision floating points values (64 bits)
      <li><b>f</b> is used for Single precision floating points values (32 bits)
      <li><b>i</b> is used for 32 bits signed integers (longint)
      <li><b>s</b> is uses for 16 bits signed integers (smallint)
   </ul>

   Note : D3D types untested.<p>

	<b>History : </b><font size=-1><ul>
    <li>03/03/07 - DaStr - Added TMatrix2[d/d/i/s/b/e/w/p] types
    <li>13/01/07 - DaStr - Added T[Matrix/Vector][2/3/4][e/w/p] types
    <li>19/12/04 - PhP - Added byte vectors
    <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
                             move PAffineVectorArray, PVectorArray and PMatrixArray
                             from VectorGeometry to this Unit    
    <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
    <li>24/08/03 - PhP - Added smallint vectors
    <li>04/07/01 - EG - Creation
  </ul>
}
unit VectorTypes;

interface

{$i GLScene.inc}

type
  TVector2d = record
    case boolean of
      true  : (Coord: array[0..1] of double);
      false : (X,Y: double);
  end;
  TVector2f = record
    case boolean of
      true  : (Coord: array[0..1] of single);
      false : (X,Y: single);
  end;
  TVector2i = record
    case boolean of
      true  : (Coord: array[0..1] of longint);
      false : (X,Y: longint);
  end;
  TVector2s = record
    case boolean of
      true  : (Coord: array[0..1] of smallint);
      false : (X,Y: smallint);
  end;
  TVector2b = record
    case boolean of
      true  : (Coord: array[0..1] of byte);
      false : (X,Y: byte);
  end;
  TVector2e = record
    case boolean of
      true  : (Coord: array[0..1] of Extended);
      false : (X,Y: Extended);
  end;
  TVector2w = record
    case boolean of
      true  : (Coord: array[0..1] of Word);
      false : (X,Y: Word);
  end;
  TVector2p = record
    case boolean of
      true  : (Coord: array[0..1] of Pointer);
      false : (X,Y: Pointer);
  end;  

  TVector3d = record
    case boolean of
      true  : (Coord: array[0..2] of double);
      false : (X,Y,Z: double);
  end;
  TVector3f = record
    case boolean of
      true  : (Coord: array[0..2] of single);
      false : (X,Y,Z: single);
  end;
  TVector3i = record
    case boolean of
      true  : (Coord: array[0..2] of longint);
      false : (X,Y,Z: longint);
  end;
  TVector3s = record
    case boolean of
      true  : (Coord: array[0..2] of smallint);
      false : (X,Y,Z: smallint);
  end;
  TVector3b = record
    case boolean of
      true  : (Coord: array[0..2] of byte);
      false : (X,Y,Z: byte);
  end; 
  TVector3e = record
    case boolean of
      true  : (Coord: array[0..2] of Extended);
      false : (X,Y,Z: Extended);
  end;
  TVector3w = record
    case boolean of
      true  : (Coord: array[0..2] of Word);
      false : (X,Y,Z: Word);
  end;
  TVector3p = record
    case boolean of
      true  : (Coord: array[0..2] of Pointer);
      false : (X,Y,Z: Pointer);
  end;    

  TVector4d = record
    case boolean of
      true  : (Coord: array[0..3] of double);
      false : (X,Y,Z,W: double);
  end;
  TVector4f = record
    case boolean of
      true  : (Coord: array[0..3] of single);
      false : (X,Y,Z,W: single);
  end;
  TVector4i = record
    case boolean of
      true  : (Coord: array[0..3] of longint);
      false : (X,Y,Z,W: longint);
  end;
  TVector4s = record
    case boolean of
      true  : (Coord: array[0..3] of smallint);
      false : (X,Y,Z,W: smallint);
  end;
  TVector4b = record
    case boolean of
      true  : (Coord: array[0..3] of byte);
      false : (X,Y,Z,W: byte);
  end; 
  TVector4e = record
    case boolean of
      true  : (Coord: array[0..3] of Extended);
      false : (X,Y,Z,W: Extended);
  end;
  TVector4w = record
    case boolean of
      true  : (Coord: array[0..3] of Word);
      false : (X,Y,Z,W: Word);
  end;
  TVector4p = record
    case boolean of
      true  : (Coord: array[0..3] of Pointer);
      false : (X,Y,Z,W: Pointer);
  end; 
  
 TMatrix2d = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2d);
      false : (X,Y: TVector2d);
  end;
  TMatrix2f = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2f);
      false : (X,Y: TVector2f);
  end;
  TMatrix2i = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2i);
      false : (X,Y: TVector2i);
  end;
  TMatrix2s = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2s);
      false : (X,Y: TVector2s);
  end;
  TMatrix2b = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2b);
      false : (X,Y: TVector2b);
  end;  
  TMatrix2e = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2e);
      false : (X,Y: TVector2e);
  end;
  TMatrix2w = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2w);
      false : (X,Y: TVector2w);
  end;
  TMatrix2p = record
    case boolean of
      true  : (Coord: array[0..1] of TVector2p);
      false : (X,Y: TVector2p);
  end;  

  TMatrix3d = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3d);
      false : (X,Y,Z: TVector3d);
  end;
  TMatrix3f = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3f);
      false : (X,Y,Z: TVector3f);
  end;
  TMatrix3i = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3i);
      false : (X,Y,Z: TVector3i);
  end;
  TMatrix3s = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3s);
      false : (X,Y,Z: TVector3s);
  end;
  TMatrix3b = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3b);
      false : (X,Y,Z: TVector3b);
  end;  
  TMatrix3e = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3e);
      false : (X,Y,Z: TVector3e);
  end;
  TMatrix3w = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3w);
      false : (X,Y,Z: TVector3w);
  end;
  TMatrix3p = record
    case boolean of
      true  : (Coord: array[0..2] of TVector3p);
      false : (X,Y,Z: TVector3p);
  end;  

  TMatrix4d = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4d);
      false : (X,Y,Z,W: TVector4d);
  end;
  TMatrix4f = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4f);
      false : (X,Y,Z,W: TVector4f);
  end;
  TMatrix4i = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4i);
      false : (X,Y,Z,W: TVector4i);
  end;
  TMatrix4s = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4s);
      false : (X,Y,Z,W: TVector4s);
  end;
  TMatrix4b = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4b);
      false : (X,Y,Z,W: TVector4b);
  end;  
  TMatrix4e = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4e);
      false : (X,Y,Z,W: TVector4e);
  end;
  TMatrix4w = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4w);
      false : (X,Y,Z,W: TVector4w);
  end;
  TMatrix4p = record
    case boolean of
      true  : (Coord: array[0..3] of TVector4p);
      false : (X,Y,Z,W: TVector4p);
  end;   

  TD3DVector = packed record
    case Integer of
      0 : (X: single;
           Y: single;
           Z: single);
      1 : (V: TVector3f);
  end;

  TD3DMatrix = packed record
    case Integer of
      0 : (_11, _12, _13, _14: single;
           _21, _22, _23, _24: single;
           _31, _32, _33, _34: single;
           _41, _42, _43, _44: single);
      1 : (M : TMatrix4f);
  end;

  // arrays of vectors
  PAffineVectorArray = ^TAffineVectorArray;
  TAffineVectorArray = array[0..MAXINT shr 4] of TVector3f;

  PVectorArray = ^TVectorArray;
  TVectorArray = array[0..MAXINT shr 5] of TVector4f;

  TMatrixArray = array [0..MaxInt shr 7] of TMatrix4f;
  PMatrixArray = ^TMatrixArray;

implementation

end.

