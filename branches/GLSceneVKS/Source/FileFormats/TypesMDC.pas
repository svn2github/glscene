//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
    Types for Return to Castle Wolfenstein's MDC file format.
}
unit TypesMDC;

interface

uses
  GLS.VectorTypes;

const
  MDCFILE_IDENTITY = 'IDPC';
  MDCFILE_VERSION  = 2;

  MDC_BASEVERTEX_FACTOR = 0.015625; // 1/64;
  MDC_COMPVERTEX_FACTOR = 0.046875; // 3/64;

type
  TMDCPoint = array[0..2] of Single;
  TMDCAngle = TMDCPoint;

  TMDCFileHeader = packed record
    Ident              : array[0..3] of AnsiChar;
    Version            : Cardinal;
    Name               : array[0..63] of AnsiChar;
    Flags              : Cardinal;
    NumFrames          : Cardinal;
    NumTags            : Cardinal;
    NumSurfaces        : Cardinal;
    NumSkins           : Cardinal;
    OffsetBorderFrames : Cardinal;
    OffsetTagNames     : Cardinal;
    OffsetTagFrames    : Cardinal;
    OffsetSurfaces     : Cardinal;
    OffsetEnd          : Cardinal;
  end;

  TMDCBorderFrame = packed record
    BBMin, BBMax : TMDCPoint;
    LocalOrigin  : TMDCPoint;
    Radius       : Single;
    Name         : array[0..15] of AnsiChar;
  end;

  PMDCTagName = ^TMDCTagName;
  TMDCTagName = packed record
    Name: array[0..63] of AnsiChar;
  end;

  PMDCTagFrame = ^TMDCTagFrame;
  TMDCTagFrame = packed record
    TagPosition: array[0..2] of Word; //or ShortInt?
    TagAngle: array[0..2] of Word;    //or ShortInt?
  end;

  TMDCTag = packed record
    TagName: PMDCTagName;
    TagFrame: PMDCTagFrame;
  end;

  TMDCSurfaceHeader = packed record
    Ident                 : array[0..3] of AnsiChar;
    Name                  : array[0..63] of AnsiChar;
    Flags                 : Cardinal;
    NumCompFrames         : Cardinal;
    NumBaseFrames         : Cardinal;
    NumSkins              : Cardinal;
    NumVertices           : Cardinal;
    NumTriangles          : Cardinal;
    OffsetTriangles       : Cardinal;
    OffsetSkins           : Cardinal;
    OffsetTexCoords       : Cardinal;
    OffsetBaseVerts       : Cardinal;
    OffsetCompVerts       : Cardinal;
    OffsetFrameBaseFrames : Cardinal;
    OffsetFrameCompFrames : Cardinal;
    OffsetEnd             : Cardinal;
  end;

  TMDCTriangle = array[0..2] of Cardinal;

  TMDCSkin = packed record
    Shader : array[0..63] of AnsiChar;
    Flags  : Cardinal;
  end;

  TMDCTexCoord = array[0..1] of Single;

  TMDCBaseVertex = array[0..3] of SmallInt;

  TMDCCompVertex = array[0..3] of Byte;

  TMDCBaseFrame = packed record
    BaseVertices: array of TMDCBaseVertex;
  end;

  TMDCCompFrame = packed record
    CompVertices: array of TMDCCompVertex;
  end;

implementation

end.
