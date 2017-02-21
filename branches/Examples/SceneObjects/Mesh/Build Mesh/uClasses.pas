unit uClasses;

interface

uses
  GLVectorGeometry,
  GLColor;

type
  TEffect = (effNone,effReflect,effReflect2,effAlphaChanelPNG);
  TMDTexture = record
    Name     : AnsiString;
    FileName : AnsiString;
    Scale    : TVector;
    Offset   : TVector;
    Enable   : Boolean;
  end;

  TMaterialData = record
    Name    : AnsiString;
    Texture : TMDTexture;
    Effect  : TEffect;
    Color   : TGLColor;
  end;

implementation

end.

