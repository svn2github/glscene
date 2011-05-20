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
    <li>21/05/11 - Yar - Added pointer types of vectors
    <li>21/02/11 - Yar - Added half and signed byte vectors
    <li>03/03/07 - DaStr - Added TMatrix2[d/d/i/s/b/e/w/p] types
    <li>13/01/07 - DaStr - Added T[Matrix/Vector][2/3/4][e/w/p] types
    <li>19/12/04 - PhP - Added byte vectors
    <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
    <li>24/08/03 - PhP - Added smallint vectors
    <li>04/07/01 - EG - Creation
  </ul>
}
unit VectorTypes;

interface

{$i GLScene.inc}

uses
  GLCrossPlatform;

type
  TVector2d = array[0..1] of double;
  TVector2f = array[0..1] of single;
  TVector2h = array[0..1] of THalfFloat;
  TVector2i = array[0..1] of longint;
  TVector2ui = array[0..1] of longWord;
  TVector2s = array[0..1] of smallint;
  TVector2b = array[0..1] of byte;
  TVector2sb = array[0..1] of ShortInt;
  TVector2e = array[0..1] of Extended;
  TVector2w = array[0..1] of Word;
  TVector2p = array[0..1] of Pointer;

  TVector3d = array[0..2] of double;
  TVector3f = array[0..2] of single;
  TVector3h = array[0..2] of THalfFloat;
  TVector3i = array[0..2] of longint;
  TVector3ui = array[0..2] of longWord;
  TVector3s = array[0..2] of smallint;
  TVector3b = array[0..2] of byte;
  TVector3sb = array[0..2] of ShortInt;
  TVector3e = array[0..2] of Extended;
  TVector3w = array[0..2] of Word;
  TVector3p = array[0..2] of Pointer;

  TVector4d = array[0..3] of double;
  TVector4f = array[0..3] of single;
  TVector4h = array[0..3] of THalfFloat;
  TVector4i = array[0..3] of longint;
  TVector4ui = array[0..3] of longWord;
  TVector4s = array[0..3] of smallint;
  TVector4b = array[0..3] of byte;
  TVector4sb = array[0..3] of ShortInt;
  TVector4e = array[0..3] of Extended;
  TVector4w = array[0..3] of Word;
  TVector4p = array[0..3] of Pointer;

  TMatrix2d = array[0..1] of TVector2d;
  TMatrix2f = array[0..1] of TVector2f;
  TMatrix2i = array[0..1] of TVector2i;
  TMatrix2s = array[0..1] of TVector2s;
  TMatrix2b = array[0..1] of TVector2b;
  TMatrix2e = array[0..1] of TVector2e;
  TMatrix2w = array[0..1] of TVector2w;
  TMatrix2p = array[0..1] of TVector2p;

  TMatrix3d = array[0..2] of TVector3d;
  TMatrix3f = array[0..2] of TVector3f;
  TMatrix3i = array[0..2] of TVector3i;
  TMatrix3s = array[0..2] of TVector3s;
  TMatrix3b = array[0..2] of TVector3b;
  TMatrix3e = array[0..2] of TVector3e;
  TMatrix3w = array[0..2] of TVector3w;
  TMatrix3p = array[0..2] of TVector3p;

  TMatrix4d = array[0..3] of TVector4d;
  TMatrix4f = array[0..3] of TVector4f;
  TMatrix4i = array[0..3] of TVector4i;
  TMatrix4s = array[0..3] of TVector4s;
  TMatrix4b = array[0..3] of TVector4b;
  TMatrix4e = array[0..3] of TVector4e;
  TMatrix4w = array[0..3] of TVector4w;
  TMatrix4p = array[0..3] of TVector4p;

  PVector2d = ^TVector2d;
  PVector2f = ^TVector2f;
  PVector2h = ^TVector2h;
  PVector2i = ^TVector2i;
  PVector2ui = ^TVector2ui;
  PVector2s = ^TVector2s;
  PVector2b = ^TVector2b;
  PVector2sb = ^TVector2sb;
  PVector2e = ^TVector2e;
  PVector2w = ^TVector2w;
  PVector2p = ^TVector2p;

  PVector3d = ^TVector3d;
  PVector3f = ^TVector3f;
  PVector3h = ^TVector3h;
  PVector3i = ^TVector3i;
  PVector3ui = ^TVector3ui;
  PVector3s = ^TVector3s;
  PVector3b = ^TVector3b;
  PVector3sb = ^TVector3sb;
  PVector3e = ^TVector3e;
  PVector3w = ^TVector3w;
  PVector3p = ^TVector3p;

  PVector4d = ^TVector4d;
  PVector4f = ^TVector4f;
  PVector4h = ^TVector4h;
  PVector4i = ^TVector4i;
  PVector4ui = ^TVector4ui;
  PVector4s = ^TVector4s;
  PVector4b = ^TVector4b;
  PVector4sb = ^TVector4sb;
  PVector4e = ^TVector4e;
  PVector4w = ^TVector4w;
  PVector4p = ^TVector4p;

  PMatrix2d = ^TMatrix2d;
  PMatrix2f = ^TMatrix2f;
  PMatrix2i = ^TMatrix2i;
  PMatrix2s = ^TMatrix2s;
  PMatrix2b = ^TMatrix2b;
  PMatrix2e = ^TMatrix2e;
  PMatrix2w = ^TMatrix2w;
  PMatrix2p = ^TMatrix2p;

  PMatrix3d = ^TMatrix3d;
  PMatrix3f = ^TMatrix3f;
  PMatrix3i = ^TMatrix3i;
  PMatrix3s = ^TMatrix3s;
  PMatrix3b = ^TMatrix3b;
  PMatrix3e = ^TMatrix3e;
  PMatrix3w = ^TMatrix3w;
  PMatrix3p = ^TMatrix3p;

  PMatrix4d = ^TMatrix4d;
  PMatrix4f = ^TMatrix4f;
  PMatrix4i = ^TMatrix4i;
  PMatrix4s = ^TMatrix4s;
  PMatrix4b = ^TMatrix4b;
  PMatrix4e = ^TMatrix4e;
  PMatrix4w = ^TMatrix4w;
  PMatrix4p = ^TMatrix4p;

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

implementation

end.

