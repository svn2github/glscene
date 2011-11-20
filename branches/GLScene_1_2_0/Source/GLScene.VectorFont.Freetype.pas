//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLScene.VectorFont.Freetype<p>

  <b>History : </b><font size=-1><ul>
  <li>19/11/11 - Yar - Creation
  </ul></font>
}

unit GLScene.VectorFont.Freetype;

interface

{$I GLScene.inc}
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  Graphics,

  GLScene.Base.Classes,
  GLScene.Platform,
  GLScene.LIBFREETYPE,
  GLScene.Base.RedBlackTree,
  GLScene.Base.Generics,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.Vector.Lists,
  GLScene.Base.GeometryBB,
  GLScene.Base.Context,
  GLScene.Mesh,
  GLScene.DrawTechnique,
  GLScene.Core,
  GLScene.VectorFont;

const
  VF_FRONT_FACING = 1.0;
  VF_BACK_FACING = -1.0;
  VF_BEZIER_STEP_SIZE = 0.2;
  VF_TESS_LIST_CAPACITY = 512;

{$IFDEF FPC}
{$IF (LCL_RELEASE < 31)}
{$DEFINE GLS_GENERIC_PREFIX}
{$IFEND}
{$ENDIF}

type

  PAffineDblVector = ^TAffineDblVector;


  // TVF_BBox
  //
  TVF_BBox = packed {$IFNDEF FPC} record {$ELSE} object{$ENDIF}
  private
    FlowerX, FlowerY, FlowerZ, FupperX, FupperY, FupperZ: Double;
  public
    {$IFDEF GLS_DELPHI_OR_CPPB}
    class operator Add(const a, b: TVF_BBox): TVF_BBox; inline;
    {$ENDIF}
    procedure Null;
    procedure SetValue(lx, ly, lz, ux, uy, uz: Single); overload;
    procedure SetValue(AGlyph: FT_GlyphSlot); overload;

    procedure Move(const AVec: TAffineDblVector);
    procedure SetDebth(depth: Single);
  end;

  // TVF_Size
  //
  TVF_Size = class(TObject)
  private
    FErr: FT_Error;
    fSize: Cardinal;
    ftSize: FT_Size;
    ftFace: FT_Face;
  public
    function CharSize(face: FT_Face; point_size, x_resolution,
      y_resolution: Cardinal): Boolean; overload;
    function CharSize: Cardinal; overload;
    function Ascender: Single;
    function Descender: Single;
    function Width: Single;
    function Height: Single;
    class function Underline: Single;
    function XPixelsPerEm: Cardinal;
    function YPixelsPerEm: Cardinal;
    property Error: FT_Error read FErr;
  end;

  TVF_EncodingList = array of FT_Encoding;

  // TVF_Face
  //
  TVF_Face = class
  private
    ftFace: FT_Face;
    FCharSize: TVF_Size;
    FNumGlyphs: Integer;
    FFontEncodingList: TVF_EncodingList;
    FErr: FT_Error;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;
    destructor Destroy; override;

    function Attach(const AFileName: string): Boolean; overload;
    function Attach(pBufferBytes: FT_Byte_ptr; bufferSizeInBytes: Cardinal)
      : Boolean; overload;
    procedure Close;
    function Size(asize, ares: Cardinal): TVF_Size;
    function UnitsPerEM: Cardinal;
    function CharMapCount: Cardinal;
    function CharMapList: TVF_EncodingList;
    function KernAdvance(index1, index2: Cardinal): TAffineDblVector;
    function Glyph(index: Cardinal; load_flags: FT_Int): FT_GlyphSlot;

    property GlyphCount: Integer read FNumGlyphs;
    property face: FT_Face read ftFace;
    property Error: FT_Error read FErr;
  end;

  TVF_CharToGlyphIndexMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree<Cardinal, Integer>;

  // TVF_Charmap
  //

  TVF_Charmap = class
  private
    FftEncoding: FT_Encoding;
    FftFace: FT_Face;
    FErr: FT_Error;
    FCharacterMap: TVF_CharToGlyphIndexMap;
  public
    constructor Create(AFace: TVF_Face);
    destructor Destroy; override;

    function GlyphListIndex(characterCode: Cardinal): Integer;
    function FontIndex(ACharacterCode: Cardinal): Cardinal;
    procedure InsertIndex(characterCode: Cardinal; containerIndex: Integer);
    function CharMap(encoding: FT_Encoding): Boolean;

    property encoding: FT_Encoding read FftEncoding;
    property Error: FT_Error read FErr;
  end;

  // TVF_Library
  //
  TVF_Library = class
  private
    class

      var FLibrary: FT_Library;
    class var FErr: FT_Error;
    class procedure Initialize;
    class procedure Finalize;
  public
    class function GetLybrary: FT_Library;
    class property Error: FT_Error read FErr;
  end;

  TVF_PointList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList<TAffineDblVector>;

  // TVF_Contour
  //
  TVF_Contour = class(TObject)
  private
    FPointList: TVF_PointList;
    FControlPoints: array [0 .. 3, 0 .. 1] of Single;
    procedure AddPoint(x, y: Single); overload; inline;
    procedure AddPoint(const APoint: TAffineDblVector); overload; inline;
    procedure evaluateQuadraticCurve; inline;
    procedure evaluateCubicCurve; inline;
    function GetPoint(I: Integer): TAffineDblVector;
    function GetPointCount: Cardinal;
  public
    constructor Create(const AContour: FT_Vector_ptr; pointTags: FT_Bytes;
      ANumberOfPoints: Cardinal);
    destructor Destroy; override;

    property Point[Index: Integer]: TAffineDblVector read GetPoint;
    property PointCount: Cardinal read GetPointCount;
  end;

  // TVF_Vectoriser
  //
  TVF_Vectoriser = class(TObject)
  private
    FftContourCount: Word;
    FContourFlag: Integer;
    FOutline: FT_Outline;
    FContourList: array of TVF_Contour;
    procedure ProcessContours;
    function GetContour(I: Integer): TVF_Contour;
    function GetContourSize(I: Integer): Cardinal;
  public
    constructor Create(AGlyph: FT_GlyphSlot);
    destructor Destroy; override;
    procedure AddGlyphToMesh(const AMesh: TMeshAtom;
      zNormal: Double = VF_FRONT_FACING);
    procedure AddContourToMesh(const AMesh: TMeshAtom;
      zNormal: Double = VF_FRONT_FACING);
    function PointCount: Cardinal;
    property ContourCount: Word read FftContourCount;
    property Contour[Index: Integer]: TVF_Contour read GetContour;
    property ContourSize[Index: Integer]: Cardinal read GetContourSize;
    property ContourFlag: Integer read FContourFlag;
  end;

  // TVF_Glyph
  //
  TVF_Glyph = class(TObject)
  protected
    FMesh: TMeshAtom;
    FAdvance: Single;
    FBBox: TVF_BBox;
    FErr: FT_Error;
  public
    constructor Create(AGlyph: FT_GlyphSlot); virtual;
    destructor Destroy; override;
    function AddToMesh(const AMesh: TMeshAtom; const APen: TVector3f): Single;
      virtual; abstract;
    property Advance: Single read FAdvance;
    property BBox: TVF_BBox read FBBox;
    property Error: FT_Error read FErr;
  end;

  // TVF_PolyGlyph
  //
  TVF_PolyGlyph = class(TVF_Glyph)
  protected

  public
    constructor Create(AGlyph: FT_GlyphSlot); override;
    destructor Destroy; override;

    function AddToMesh(const AMesh: TMeshAtom; const APen: TVector3f)
      : Single; override;
  end;

  // TVF_ExtrGlyph
  //
  TVF_ExtrGlyph = class(TVF_PolyGlyph)
  public
    constructor Create(AGlyph: FT_GlyphSlot; ADepth: Single); reintroduce;
    destructor Destroy; override;
  end;

  TVF_GlyphList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList<TVF_Glyph>;

  TVF_GlyphContainer = class(TObject)
  private
    FGlyphList: TVF_GlyphList;
    FFace: TVF_Face;
    FCharMap: TVF_Charmap;
    FErr: FT_Error;
    function GetGlyph(ACharacterCode: Cardinal): TVF_Glyph;
    function GetBBox(ACharacterCode: Cardinal): TVF_BBox;
  public
    constructor Create(AFace: TVF_Face);
    destructor Destroy; override;

    function CharMap(AEncoding: FT_Encoding): Boolean;
    function FontIndex(ACharacterCode: Cardinal): Cardinal;
    procedure Add(AGlyph: TVF_Glyph; ACharacterCode: Cardinal);
    function Advance(ACharacterCode, ANextCharacterCode: Cardinal): Single;
    function AddToMesh(ACharacterCode, ANextCharacterCode: Cardinal;
      APen: TVector3f; AMesh: TMeshAtom): TVector3f;

    property Glyph[ACharCode: Cardinal]: TVF_Glyph read GetGlyph;
    property BBox[ACharCode: Cardinal]: TVF_BBox read GetBBox;
    property Error: FT_Error read FErr;
  end;

  // TVF_Font
  //
  TVF_Font = class(TObject)
  private
    function GetAscender: Single;
    function GetDescender: Single;
    procedure CheckGlyph(G: Cardinal); inline;
    procedure GetGlyphs(const AStr: string; APos: Integer;
      out AGlyph, AnextGlyph: Cardinal); inline;
  protected
    FFace: TVF_Face;
    FCharSize: TVF_Size;
    FErr: FT_Error;
    FGlyphList: TVF_GlyphContainer;
    FPen: TVector3f;
    function MakeGlyph(G: Cardinal): TVF_Glyph; virtual; abstract;
  public
    constructor Create(const AFontName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;
    destructor Destroy; override;

    function CharMap(AEncoding: FT_Encoding): Boolean;
    function BBox(const AStr: string): TAABB;
    function Advance(const AStr: string): Single;
    function FaceSize(asize, ares: Cardinal): Boolean;
    procedure AddToMesh(const AStr: string; AMesh: TMeshAtom);

    property Error: FT_Error read FErr;
    property Ascender: Single read GetAscender;
    property Descender: Single read GetDescender;
  end;

  // TVF_PolygonFont
  //
  TVF_PolygonFont = class(TVF_Font)
  protected
    function MakeGlyph(G: Cardinal): TVF_Glyph; override;
  end;

  // TVF_ExtrudedFont
  //
  TVF_ExtrudedFont = class(TVF_Font)
  private
    FDepth: Single;
  protected
    function MakeGlyph(G: Cardinal): TVF_Glyph; override;
  public
    constructor Create(const AFontName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;

    property depth: Single read FDepth write FDepth;
  end;

  // TGLFreetypeVectorFont
  //
  TGLFreetypeVectorFont = class(TGLCustomVectorFont)
  protected
    { Protected Declarations }
    FSystemFont: TFont;
    FFTFont: TVF_Font;
    FFaceSize: Integer;
    FExtrusion: Single;
    FFinishEvent: TFinishTaskEvent;
    procedure Loaded; override;
    procedure PrepareGeometry(AnUrgently: Boolean);
  private
    { Private Declarations }
    procedure SetFont(Value: TFont);
    procedure SetFaceSize(const Value: Integer);
    procedure OnFontChanged(Sender: TObject);
    procedure BuildFont; stdcall;
    procedure SetExtrusion(Value: Single);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BuildString(ABatch: TDrawBatch;
      const aText: UnicodeString); override;
    function GetAABB(const aText: string): TAABB; override;
  published
    { Published Declarations }
    property Ranges;
    property Font: TFont read FSystemFont write SetFont;
    property FaceSize: Integer read FFaceSize write SetFaceSize default 1;
    { : Adjusts the 3D font extrusion.<p>
      If Extrusion=0, the characters will be flat (2D), values >0 will
      give them a third dimension. }
    property Extrusion: Single read FExtrusion write SetExtrusion;
  end;

{$IFDEF FPC}

operator + (const a, b: TVF_BBox): TVF_BBox; overload; inline;
{$ENDIF}

implementation

uses
{$IFDEF GLS_SERVICE_CONTEXT}
  SyncObjs,
{$ENDIF}
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.OpenGL.Adapter,
  GLScene.Base.GLStateMachine,
  GLScene.Shader.Parameter,
  GLScene.Base.Log;

resourcestring
  StrFTError = 'FREETYPE Error: %s';

procedure MakeVector(out v: TAffineDblVector; const p: FT_Vector);
  overload; inline;
begin
  v[0] := p.x;
  v[1] := p.y;
  v[2] := 0.0;
end;

procedure MakeVector(out v: TVector3f; const p: TAffineDblVector);
  overload; inline;
begin
  v[0] := p[0];
  v[1] := p[1];
  v[2] := p[2];
end;

function CompareCardinal(const Item1, Item2: Cardinal): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
  end
  else if (Item1 = Item2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

var
  vCollector: TList;

procedure DestroyInFinal(AObject: TObject);
begin
  if not Assigned(vCollector) then
    vCollector := TList.Create;
  vCollector.Add(AObject);
end;

procedure DoDestroyInFinal;
var
  I: Integer;
begin
  if Assigned(vCollector) then
    for I := vCollector.Count - 1 downto 0 do
      TObject(vCollector[I]).Destroy;
  vCollector.Free;
end;

{$REGION 'TVF_Size'}
// ------------------
// ------------------ TVF_Size ------------------
// ------------------

function TVF_Size.CharSize(face: FT_Face; point_size, x_resolution,
  y_resolution: Cardinal): Boolean;
begin
  FErr := FT_Set_Char_Size(face, 0, point_size * 64, x_resolution,
    y_resolution);
  Result := FErr = 0;
  if Result then
  begin
    ftFace := face;
    fSize := point_size;
    ftSize := ftFace.Size;
  end
  else
  begin
    ftFace := nil;
    fSize := 0;
    ftSize := nil;
    GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end;
end;

function TVF_Size.CharSize: Cardinal;
begin
  Result := fSize;
end;

function TVF_Size.Ascender: Single;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.Ascender
  else
    Result := 0;
end;

function TVF_Size.Descender: Single;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.Descender
  else
    Result := 0;
end;

function TVF_Size.Width: Single;
begin
  if Assigned(ftSize) then
  begin
    if FT_IS_SCALABLE(@ftFace) then
      Result := (ftFace.BBox.xMax - ftFace.BBox.xMin) *
        (ftSize.metrics.x_ppem / ftFace.units_per_EM)
    else
      Result := ftSize.metrics.max_advance / 64.0;
  end
  else
    Result := 0;
end;

function TVF_Size.Height: Single;
begin
  if Assigned(ftSize) then
  begin
    if FT_IS_SCALABLE(@ftFace) then
      Result := (ftFace.BBox.yMax - ftFace.BBox.yMin) *
        (ftSize.metrics.y_ppem / ftFace.units_per_EM)
    else
      Result := ftSize.metrics.Height / 64.0;
  end
  else
    Result := 0;
end;

class function TVF_Size.Underline: Single;
begin
  Result := 0;
end;

function TVF_Size.XPixelsPerEm: Cardinal;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.x_ppem
  else
    Result := 0;
end;

function TVF_Size.YPixelsPerEm: Cardinal;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.y_ppem
  else
    Result := 0;
end;
{$ENDREGION 'TVF_Size'}
{$REGION 'TVF_Size'}
// ------------------
// ------------------ TVF_BBox ------------------
// ------------------

{$IFDEF FPC}

operator + (const a, b: TVF_BBox): TVF_BBox;
{$ELSE}

class operator TVF_BBox.Add(const a, b: TVF_BBox): TVF_BBox;
{$ENDIF}
begin
  Result.FlowerX := MinFloat(a.FlowerX, b.FlowerX);
  Result.FlowerY := MinFloat(a.FlowerY, b.FlowerY);
  Result.FlowerZ := MinFloat(a.FlowerZ, b.FlowerZ);
  Result.FupperX := MaxFloat(a.FupperX, b.FupperX);
  Result.FupperY := MaxFloat(a.FupperY, b.FupperY);
  Result.FupperZ := MaxFloat(a.FupperZ, b.FupperZ);
end;

procedure TVF_BBox.Null;
begin
  FillChar(Self, SizeOf(TVF_BBox), $00);
end;

procedure TVF_BBox.SetValue(lx, ly, lz, ux, uy, uz: Single);
begin
  FlowerX := lx;
  FlowerY := ly;
  FlowerZ := lz;
  FupperX := ux;
  FupperY := uy;
  FupperZ := uz;
end;

procedure TVF_BBox.SetValue(AGlyph: FT_GlyphSlot);
var
  BBox: FT_BBox;
begin
  FT_Outline_Get_CBox(AGlyph.outline, @BBox);
  FlowerX := BBox.xMin / 64.0;
  FlowerY := BBox.yMin / 64.0;
  FlowerZ := 0.0;
  FupperX := BBox.xMax / 64.0;
  FupperY := BBox.yMax / 64.0;
  FupperZ := 0.0;
end;

procedure TVF_BBox.Move(const AVec: TAffineDblVector);
begin
  FlowerX := FlowerX + AVec[0];
  FlowerY := FlowerY + AVec[1];
  FlowerZ := FlowerZ + AVec[2];
  FupperX := FupperX + AVec[0];
  FupperY := FupperY + AVec[1];
  FupperZ := FupperZ + AVec[2];
end;

procedure TVF_BBox.SetDebth(depth: Single);
begin
  FupperZ := FlowerZ + depth;
end;

{$ENDREGION 'TVF_Size'}
{$REGION 'TVF_Face'}
// ------------------
// ------------------ TVF_Face ------------------
// ------------------

constructor TVF_Face.Create(const AFileName: string);
var
  lvFileName: AnsiString;
begin
  if TVF_Library.Error = 0 then
  begin
    lvFileName := AnsiString(AFileName);
    FErr := FT_New_Face(TVF_Library.GetLybrary, PAnsiChar(lvFileName), 0, ftFace);
  end
  else
    FErr := TVF_Library.Error;

  if FErr <> 0 then
  begin
    ftFace := nil;
    GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end
  else
  begin
    FNumGlyphs := ftFace.num_glyphs;
    FCharSize := TVF_Size.Create
  end;
end;

constructor TVF_Face.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  FErr := FT_New_Memory_Face(TVF_Library.GetLybrary, pBufferBytes,
    bufferSizeInBytes, 0, ftFace);

  if FErr <> 0 then
  begin
    ftFace := nil;
    GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end
  else
  begin
    FNumGlyphs := ftFace.num_glyphs;
    FCharSize := TVF_Size.Create
  end;
end;

destructor TVF_Face.Destroy;
begin
  FCharSize.Free;
  Close;
end;

function TVF_Face.Attach(const AFileName: string): Boolean;
var
  lvFileName: AnsiString;
begin
  lvFileName := AnsiString(AFileName);
  FErr := FT_Attach_File(ftFace, PAnsiChar(AFileName[1]));
  Result := FErr = 0;
end;

function TVF_Face.Attach(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal): Boolean;
var
  open: FT_Open_Args;
begin
  open.flags := FT_OPEN_MEMORY;
  open.memory_base := pBufferBytes;
  open.memory_size := bufferSizeInBytes;

  FErr := FT_Attach_Stream(ftFace, open);
  Result := FErr = 0;
end;

procedure TVF_Face.Close;
begin
  if Assigned(ftFace) then
  begin
    FT_Done_Face(ftFace);
    ftFace := nil;
  end;
end;

function TVF_Face.Size(asize, ares: Cardinal): TVF_Size;
begin
  FCharSize.CharSize(ftFace, asize, ares, ares);
  FErr := FCharSize.Error;

  Result := FCharSize;
end;

function TVF_Face.UnitsPerEM: Cardinal;
begin
  Result := ftFace.units_per_EM;
end;

function TVF_Face.CharMapCount: Cardinal;
begin
  Result := ftFace.num_charmaps;
end;

function TVF_Face.CharMapList: TVF_EncodingList;
var
  I: Integer;
begin
  if not Assigned(FFontEncodingList) then
  begin
    SetLength(FFontEncodingList, CharMapCount);
    for I := 0 to CharMapCount - 1 do
      FFontEncodingList[I] := ftFace.charmaps[I].encoding;
  end;

  Result := FFontEncodingList;
end;

function TVF_Face.KernAdvance(index1, index2: Cardinal): TAffineDblVector;
var
  x, y: Single;
  kernAdv: FT_Vector;
begin
  x := 0.0;
  y := 0.0;

  if FT_HAS_KERNING(ftFace) and (index1 <> 0) and (index2 <> 0) then
  begin
    kernAdv.x := 0;
    kernAdv.y := 0;

    FErr := FT_Get_Kerning(ftFace, index1, index2, ft_kerning_unfitted,
      kernAdv);
    if FErr = 0 then
    begin
      x := kernAdv.x / 64.0;
      y := kernAdv.y / 64.0;
    end
    else
      GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end;

  Result[0] := x;
  Result[1] := y;
  Result[2] := 0.0;
end;

function TVF_Face.Glyph(index: Cardinal; load_flags: FT_Int): FT_GlyphSlot;
begin
  FErr := FT_Load_Glyph(ftFace, index, load_flags);
  if FErr <> 0 then
  begin
    GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
    Exit(nil);
  end;
  Result := ftFace.Glyph;
end;
{$ENDREGION 'TVF_Face'}
{$REGION 'TVF_Charmap'}
// ------------------
// ------------------ TVF_Charmap ------------------
// ------------------

constructor TVF_Charmap.Create(AFace: TVF_Face);
begin
  FftFace := AFace.face;
  if FftFace.CharMap = nil then
    FErr := FT_Set_Charmap(FftFace, FftFace.charmaps[0]);

  FftEncoding := FftFace.CharMap.encoding;
  FCharacterMap := TVF_CharToGlyphIndexMap.Create(CompareCardinal, nil);
end;

destructor TVF_Charmap.Destroy;
begin
  FCharacterMap.Free;
end;

function TVF_Charmap.GlyphListIndex(characterCode: Cardinal): Integer;
begin
  if not FCharacterMap.Find(characterCode, Result) then
    Result := -1;
end;

function TVF_Charmap.FontIndex(ACharacterCode: Cardinal): Cardinal;
begin
  Result := FT_Get_Char_Index(FftFace, ACharacterCode);
end;

procedure TVF_Charmap.InsertIndex(characterCode: Cardinal;
  containerIndex: Integer);
begin
  FCharacterMap.Add(characterCode, containerIndex);
end;

function TVF_Charmap.CharMap(encoding: FT_Encoding): Boolean;
begin
  if FftEncoding = encoding then
    Exit(True);

  FErr := FT_Select_Charmap(FftFace, encoding);

  if FErr = 0 then
    FftEncoding := encoding
  else
    FftEncoding := ft_encoding_none;

  FCharacterMap.Clear;
  Result := FErr = 0;
end;
{$ENDREGION 'TVF_Charmap'}
{$REGION 'TVF_Library'}
// ------------------
// ------------------ TVF_Library ------------------
// ------------------

class procedure TVF_Library.Initialize;
var
  major, minor, patch: Integer;
begin
  if InitFreetype then
  begin
    FErr := FT_Init_FreeType(FLibrary);
    if FErr <> 0 then
    begin
      FreeMem(FLibrary);
      FLibrary := nil;
      GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
    end
    else
    begin
      FT_Library_Version(FLibrary, major, minor, patch);
      GLSLogger.LogInfoFmt('FreeType library %d.%d.%d loaded', [major, minor, patch]);
    end;
  end
  else
    FErr := $04;
end;

class procedure TVF_Library.Finalize;
begin
  if Assigned(FLibrary) then
  begin
    FT_Done_FreeType(FLibrary);
    FLibrary := nil;
  end;
  CloseFreetype;
end;

class function TVF_Library.GetLybrary: FT_Library;
begin
  Result := FLibrary;
end;

{$ENDREGION 'TVF_Library'}
{$REGION 'TVF_Glyph'}
// ------------------
// ------------------ TVF_Glyph ------------------
// ------------------

constructor TVF_Glyph.Create(AGlyph: FT_GlyphSlot);
begin
  if Assigned(AGlyph) then
  begin
    FAdvance := AGlyph.Advance.x / 64.0;
    FBBox.SetValue(AGlyph);
  end;
  FMesh := TMeshAtom.Create;
end;

destructor TVF_Glyph.Destroy;
begin
  FMesh.Destroy;
  inherited;
end;

{$ENDREGION 'TVF_Glyph'}
{$REGION 'TVF_Contour'}
// ------------------
// ------------------ TVF_Contour ------------------
// ------------------

constructor TVF_Contour.Create(const AContour: FT_Vector_ptr;
  pointTags: FT_Bytes; ANumberOfPoints: Cardinal);
var
  I: Cardinal;
  pointTag, nextPointTag: Byte;
  controlPoint: TAffineDblVector;
  previousPoint: TAffineDblVector;
  nextPoint: TAffineDblVector;
  controlPoint2: TAffineDblVector;
begin
  FPointList := TVF_PointList.Create;
  I := 0;
  while I < ANumberOfPoints do
  begin

    pointTag := pointTags[I];

    if (pointTag = FT_Curve_Tag_On) or (ANumberOfPoints < 2) then
    begin
      AddPoint(AContour[I].x, AContour[I].y);
      Inc(I);
      continue;
    end;

    MakeVector(controlPoint, AContour[I]);
    if I = 0 then
      MakeVector(previousPoint, AContour[ANumberOfPoints - 1])
    else
      previousPoint := FPointList.Last;

    if I = ANumberOfPoints - 1 then
      nextPoint := FPointList.First
    else
      MakeVector(nextPoint, AContour[I + 1]);

    if pointTag = FT_Curve_Tag_Conic then
    begin
      if I = ANumberOfPoints - 1 then
        nextPointTag := pointTags[0]
      else
        nextPointTag := pointTags[I + 1];

      while nextPointTag = FT_Curve_Tag_Conic do
      begin
        nextPoint[0] := (controlPoint[0] + nextPoint[0]) * 0.5;
        nextPoint[1] := (controlPoint[1] + nextPoint[1]) * 0.5;
        nextPoint[2] := 0;

        FControlPoints[0][0] := previousPoint[0];
        FControlPoints[0][1] := previousPoint[1];
        FControlPoints[1][0] := controlPoint[0];
        FControlPoints[1][1] := controlPoint[1];
        FControlPoints[2][0] := nextPoint[0];
        FControlPoints[2][1] := nextPoint[1];

        evaluateQuadraticCurve;
        Inc(I);

        previousPoint := nextPoint;
        MakeVector(controlPoint, AContour[I]);
        if I = ANumberOfPoints - 1 then
        begin
          nextPoint := FPointList.First;
          nextPointTag := pointTags[0];
        end
        else
        begin
          MakeVector(nextPoint, AContour[I + 1]);
          nextPointTag := pointTags[I + 1];
        end;
      end;

      FControlPoints[0][0] := previousPoint[0];
      FControlPoints[0][1] := previousPoint[1];
      FControlPoints[1][0] := controlPoint[0];
      FControlPoints[1][1] := controlPoint[1];
      FControlPoints[2][0] := nextPoint[0];
      FControlPoints[2][1] := nextPoint[1];

      evaluateQuadraticCurve;
      Inc(I);
      continue;
    end;

    if pointTag = FT_Curve_Tag_Cubic then
    begin
      controlPoint2 := nextPoint;

      if I = ANumberOfPoints - 2 then
        nextPoint := FPointList.First
      else
        MakeVector(nextPoint, AContour[I + 2]);

      FControlPoints[0][0] := previousPoint[0];
      FControlPoints[0][1] := previousPoint[1];
      FControlPoints[1][0] := controlPoint[0];
      FControlPoints[1][1] := controlPoint[1];
      FControlPoints[2][0] := controlPoint2[0];
      FControlPoints[2][1] := controlPoint2[1];
      FControlPoints[3][0] := nextPoint[0];
      FControlPoints[3][1] := nextPoint[1];

      evaluateCubicCurve;
      Inc(I);
      continue;
    end;
    Inc(I);
  end;
end;

destructor TVF_Contour.Destroy;
begin
  FPointList.Free;
end;

procedure TVF_Contour.AddPoint(x, y: Single);
var
  v: TAffineDblVector;
begin
  v[0] := x;
  v[1] := y;
  v[2] := 0.0;
  AddPoint(v);
end;

procedure TVF_Contour.AddPoint(const APoint: TAffineDblVector);
var
  can: Boolean;
begin
  can := True;
  if FPointList.Count > 0 then
    can := not(VectorEquals(APoint, FPointList.Last) and VectorEquals(APoint,
      FPointList.First));
  if can then
  begin
    FPointList.Add(APoint);
  end;
end;

procedure TVF_Contour.evaluateQuadraticCurve;
var
  I: Cardinal;
  bezierValues: array [0 .. 1, 0 .. 1] of Single;
  t: Single;
begin
  for I := 0 to Round(1.0 / VF_BEZIER_STEP_SIZE) do
  begin
    t := I * VF_BEZIER_STEP_SIZE;

    bezierValues[0][0] := (1.0 - t) * FControlPoints[0][0] + t *
      FControlPoints[1][0];
    bezierValues[0][1] := (1.0 - t) * FControlPoints[0][1] + t *
      FControlPoints[1][1];

    bezierValues[1][0] := (1.0 - t) * FControlPoints[1][0] + t *
      FControlPoints[2][0];
    bezierValues[1][1] := (1.0 - t) * FControlPoints[1][1] + t *
      FControlPoints[2][1];

    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    AddPoint(bezierValues[0][0], bezierValues[0][1]);
  end;
end;

procedure TVF_Contour.evaluateCubicCurve;
var
  I: Cardinal;
  bezierValues: array [0 .. 2, 0 .. 1] of Single;
  t: Single;
begin
  for I := 0 to Round(1.0 / VF_BEZIER_STEP_SIZE) do
  begin
    t := I * VF_BEZIER_STEP_SIZE;

    bezierValues[0][0] := (1.0 - t) * FControlPoints[0][0] + t *
      FControlPoints[1][0];
    bezierValues[0][1] := (1.0 - t) * FControlPoints[0][1] + t *
      FControlPoints[1][1];

    bezierValues[1][0] := (1.0 - t) * FControlPoints[1][0] + t *
      FControlPoints[2][0];
    bezierValues[1][1] := (1.0 - t) * FControlPoints[1][1] + t *
      FControlPoints[2][1];

    bezierValues[2][0] := (1.0 - t) * FControlPoints[2][0] + t *
      FControlPoints[3][0];
    bezierValues[2][1] := (1.0 - t) * FControlPoints[2][1] + t *
      FControlPoints[3][1];
    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    bezierValues[1][0] := (1.0 - t) * bezierValues[1][0] + t *
      bezierValues[2][0];
    bezierValues[1][1] := (1.0 - t) * bezierValues[1][1] + t *
      bezierValues[2][1];

    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    AddPoint(bezierValues[0][0], bezierValues[0][1]);
  end;
end;

function TVF_Contour.GetPoint(I: Integer): TAffineDblVector;
begin
  Result := FPointList[I];
end;

function TVF_Contour.GetPointCount: Cardinal;
begin
  Result := FPointList.Count;
end;

{$ENDREGION 'TVF_Contour'}
{$REGION 'TVF_Vectoriser'}
// ------------------
// ------------------ TVF_Vectoriser ------------------
// ------------------

constructor TVF_Vectoriser.Create(AGlyph: FT_GlyphSlot);
begin
  if Assigned(AGlyph) then
  begin
    FOutline := AGlyph.outline;
    FftContourCount := FOutline.n_contours;
    FContourFlag := FOutline.flags;
    ProcessContours;
  end;
end;

destructor TVF_Vectoriser.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FContourList) do
    FContourList[I].Free;
  FContourList := nil;
end;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ELSE}

var
{$ENDIF}
  vMainMesh: TMeshAtom;
  vTempMesh: TMeshAtom;
  vNormal: PAffineVector;
  tempPointList: TAffineVectorList;
  tessellationList: TVF_PointList;

procedure tessError(errno: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(errno) + ': ' + string(gluErrorString(errno)));
end;

procedure tessVertex(vertexData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
const
  k = 1 / 64;
var
  p: ^TAffineVector;
begin
  p := vertexData;
  with vTempMesh do
  begin
    Attribute3f(attrPosition, VectorScale(p^, k));
    Attribute2f(attrTexCoord0, PVector2f(vertexData)^);
    Attribute3f(attrNormal, vNormal^);
    EmitVertex;
  end;
end;

procedure tessCombine(coords: PDoubleVector; vertex_data: Pointer;
  weight: PGLFloat; var outData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
var
  I: Integer;
begin
  I := tempPointList.Count;
  tempPointList.Add(coords^[0], coords^[1], coords^[2]);
  outData := tempPointList.ItemAddress[I];
end;

procedure tessBegin(AType: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with vTempMesh do
  begin
    Clear;
    DeclareAttribute(attrPosition, GLSLType3f);
    DeclareAttribute(attrNormal, GLSLType3f);
    DeclareAttribute(attrTexCoord0, GLSLType2f);
    case AType of
      GL_TRIANGLE_FAN:
        begin
          BeginAssembly(mpTRIANGLE_FAN);
        end;
      GL_TRIANGLE_STRIP:
        begin
          BeginAssembly(mpTRIANGLE_STRIP);
        end;
      GL_TRIANGLES:
        begin
          BeginAssembly(mpTRIANGLES);
        end;
      GL_LINE_LOOP:
        begin
          BeginAssembly(mpLINE_LOOP);
        end;
    end;
  end;
end;

var
  gvC: Integer = 0;

procedure tessEnd();
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  case vTempMesh.Primitive of
    mpTRIANGLE_FAN, mpTRIANGLE_STRIP:
      begin
        vTempMesh.EndAssembly;
        vTempMesh.Triangulate;
        vMainMesh.Merge(vTempMesh);
      end;
    mpTRIANGLES:
      begin
        vTempMesh.EndAssembly;
        vMainMesh.Merge(vTempMesh);
      end;
    mpLINE_LOOP:
      begin
        vTempMesh.EndAssembly;
        vTempMesh.LineSegmentation;
        vMainMesh.Merge(vTempMesh);
      end;
  end;
end;

procedure TVF_Vectoriser.AddGlyphToMesh(const AMesh: TMeshAtom;
  zNormal: Double = VF_FRONT_FACING);
var
  tess: PGLUTesselator;
  c, p: Cardinal;
  Contour: TVF_Contour;
  d: PAffineDblVector;
  n: TAffineVector;
  I: Integer;
begin
  vMainMesh := AMesh;
  if not Assigned(vTempMesh) then
  begin
    vTempMesh := TMeshAtom.Create;
    DestroyInFinal(vTempMesh);
  end;
  if not Assigned(tempPointList) then
  begin
    tempPointList := TAffineVectorList.Create;
    tempPointList.Capacity := VF_TESS_LIST_CAPACITY;
    DestroyInFinal(tempPointList);
  end
  else
    tempPointList.Flush;
  tess := gluNewTess;

  try

    gluTessCallback(tess, GLU_TESS_BEGIN, @tessBegin);
    gluTessCallback(tess, GLU_TESS_VERTEX, @tessVertex);
    gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
    gluTessCallback(tess, GLU_TESS_END, @tessEnd);
    gluTessCallback(tess, GLU_TESS_ERROR, @tessError);

    if FContourFlag and FT_OUTLINE_EVEN_ODD_FILL <> 0 then
      gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_ODD)
    else
      gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);
    gluTessProperty(tess, GLU_TESS_TOLERANCE, 0.0);
    gluTessNormal(tess, 0.0, 0.0, zNormal);
    n[0] := 0;
    n[1] := 0;
    n[2] := zNormal;
    vNormal := @n;

    with vMainMesh do
    begin
      Lock;
      vTempMesh.Lock;
      try
        if VertexCount = 0 then
        begin
          DeclareAttribute(attrPosition, GLSLType3f);
          DeclareAttribute(attrNormal, GLSLType3f);
          DeclareAttribute(attrTexCoord0, GLSLType2f);
        end;

        gluTessBeginPolygon(tess, nil);
        for c := 0 to ContourCount - 1 do
        begin
          Contour := FContourList[c];
          gluTessBeginContour(tess);
          for p := 0 to Contour.PointCount - 1 do
          begin
            d := Contour.FPointList.ItemAddress[p];
            I := tempPointList.Count;
            tempPointList.Add(d^[0], d^[1], d^[2]);
            gluTessVertex(tess, d^, tempPointList.ItemAddress[I]);
          end;
          gluTessEndContour(tess);
        end;
        gluTessEndPolygon(tess);
      finally
        // release stuff
        UnLock;
        vTempMesh.UnLock;
      end;
      Validate;
    end;

  finally
    gluDeleteTess(tess);
  end;
end;

procedure TVF_Vectoriser.AddContourToMesh(const AMesh: TMeshAtom;
  zNormal: Double = VF_FRONT_FACING);
var
  c, p: Cardinal;
  Contour: TVF_Contour;
  v: TAffineDblVector;
begin
  with AMesh do
  begin
    Lock;
    try
      if VertexCount = 0 then
      begin
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
      end;

      BeginAssembly(mpLINES);
      Attribute3f(attrNormal, 0, 0, zNormal);
      for c := 0 to ContourCount - 1 do
      begin
        Contour := FContourList[c];

        for p := 0 to Contour.PointCount - 2 do
        begin
          v := Contour.FPointList[p];

          Attribute3f(attrPosition, v[0] / 64, v[1] / 64, v[2] / 64);
          Attribute2f(attrTexCoord0, v[0], v[1]);
          EmitVertex;
          v := Contour.FPointList[p + 1];
          Attribute3f(attrPosition, v[0] / 64, v[1] / 64, v[2] / 64);
          Attribute2f(attrTexCoord0, v[0], v[1]);
          EmitVertex;
        end;
        v := Contour.FPointList[0];
        Attribute3f(attrPosition, v[0] / 64, v[1] / 64, v[2] / 64);
        Attribute2f(attrTexCoord0, v[0], v[1]);
        EmitVertex;
        v := Contour.FPointList[Contour.PointCount - 1];
        Attribute3f(attrPosition, v[0] / 64, v[1] / 64, v[2] / 64);
        Attribute2f(attrTexCoord0, v[0], v[1]);
        EmitVertex;
      end;
      EndAssembly;
    finally
      // release stuff
      UnLock;
    end;
    Validate;
  end;
end;

function TVF_Vectoriser.PointCount: Cardinal;
var
  I: Integer;
  S: Cardinal;
begin
  S := 0;
  for I := 0 to High(FContourList) do
    S := S + FContourList[I].GetPointCount;
  Result := S;
end;

procedure TVF_Vectoriser.ProcessContours;
var
  contourLength, startIndex, endIndex, contourIndex: ShortInt;
  pointList: FT_Vector_ptr;
  tagList: FT_Bytes;
begin
  startIndex := 0;
  SetLength(FContourList, FftContourCount);

  for contourIndex := 0 to FftContourCount - 1 do
  begin
    pointList := @FOutline.points[startIndex];
    tagList := @FOutline.tags[startIndex];
    endIndex := FOutline.contours[contourIndex];
    contourLength := (endIndex - startIndex) + 1;
    FContourList[contourIndex] := TVF_Contour.Create(pointList, tagList,
      Cardinal(contourLength));
    startIndex := endIndex + 1;
  end;
end;

function TVF_Vectoriser.GetContour(I: Integer): TVF_Contour;
begin
  Result := FContourList[I];
end;

function TVF_Vectoriser.GetContourSize(I: Integer): Cardinal;
begin
  Result := FContourList[I].PointCount;
end;

{$ENDREGION 'TVF_Vectoriser'}
{$REGION 'TVF_PolyGlyph'}
// ------------------
// ------------------ TVF_PolyGlyph ------------------
// ------------------

constructor TVF_PolyGlyph.Create(AGlyph: FT_GlyphSlot);
var
  LVectoriser: TVF_Vectoriser;
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  LVectoriser := TVF_Vectoriser.Create(AGlyph);
  if (LVectoriser.ContourCount > 0) and (LVectoriser.PointCount() >= 3) then
  begin
    LVectoriser.AddGlyphToMesh(FMesh); // AddContourToMesh  (FMesh);
    with FMesh do
    begin
      Lock;
      try
        WeldVertices;
      finally
        UnLOck;
      end;
    end;
  end;
  LVectoriser.Destroy;
end;

destructor TVF_PolyGlyph.Destroy;
begin
  inherited Destroy;
end;

function TVF_PolyGlyph.AddToMesh(const AMesh: TMeshAtom;
  const APen: TVector3f): Single;
var
  flag: Boolean;
begin
  AMesh.Lock;
  FMesh.Lock;
  flag := not VectorIsNull(APen);
  try
    if flag then
      FMesh.Transform(CreateTranslationMatrix(APen));

    AMesh.Merge(FMesh);
    // may be need to store mesh instead of retransformation
    if flag then
      FMesh.Transform(CreateTranslationMatrix(VectorNegate(APen)));
  finally
    AMesh.UnLock;
    FMesh.UnLock;
  end;
  AMesh.Validate;
  Result := FAdvance;
end;

{$ENDREGION}
{$REGION 'TVF_ExtrGlyph'}

// ------------------
// ------------------ TVF_ExtrGlyph ------------------
// ------------------
constructor TVF_ExtrGlyph.Create(AGlyph: FT_GlyphSlot; ADepth: Single);
const
  k = 1 / 64;
var
  LVectoriser: TVF_Vectoriser;
  LMesh: TMeshAtom;
  zOffset, p1, p2, p3, p4: TVector3f;
  c, I, nextIndex: Integer;
  Contour: TVF_Contour;
  numberOfPoints: Cardinal;
  t0, t1: Single;
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  LVectoriser := TVF_Vectoriser.Create(AGlyph);
  if (LVectoriser.ContourCount > 0) and (LVectoriser.PointCount() >= 3) then
  begin
    LMesh := TMeshAtom.Create;
    LVectoriser.AddGlyphToMesh(LMesh);
    FMesh.Assign(LMesh);
    FMesh.Lock;
    LMesh.Lock;
    try
      zOffset := Vector3fMake(0, 0, ADepth);
      LMesh.Transform(CreateTranslationMatrix(zOffset));
      FMesh.FlipFaces(False);
      FMesh.Merge(LMesh);
      FMesh.BeginAssembly(mpTRIANGLES);
      for c := 0 to LVectoriser.ContourCount - 1 do
      begin
        Contour := LVectoriser.Contour[c];
        numberOfPoints := Contour.PointCount;
        t0 := 0;
        for I := 0 to numberOfPoints - 1 do
        begin
          if I = Integer(numberOfPoints - 1) then
            nextIndex := 0
          else
            nextIndex := I + 1;
          MakeVector(p1, Contour.Point[I]);
          MakeVector(p2, Contour.Point[nextIndex]);
          ScaleVector(p1, k);
          ScaleVector(p2, k);
          p3 := VectorAdd(p1, zOffset);
          p4 := VectorAdd(p2, zOffset);
          // Actully need calculation of texcoords based on contour lenght not points number
          t1 := (I + 1) / numberOfPoints;
          with FMesh do
          begin
            Attribute3f(attrPosition, p1);
            Attribute2f(attrTexCoord0, t0, 0);
            EmitVertex;
            Attribute3f(attrPosition, p3);
            Attribute2f(attrTexCoord0, t0, 1);
            EmitVertex;
            Attribute3f(attrPosition, p2);
            Attribute2f(attrTexCoord0, t1, 0);
            EmitVertex;
            EmitVertex;
            Attribute3f(attrPosition, p3);
            Attribute2f(attrTexCoord0, t0, 1);
            EmitVertex;
            Attribute3f(attrPosition, p4);
            Attribute2f(attrTexCoord0, t1, 1);
            EmitVertex;
          end;
        end;
      end;
      FMesh.EndAssembly;
      FMesh.ComputeNormals(True);
    finally
      FMesh.UnLock;
      LMesh.UnLock;
      LMesh.Destroy;
    end;
  end;
  LVectoriser.Destroy;
end;

destructor TVF_ExtrGlyph.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{$REGION 'TVF_GlyphContainer'}
// ------------------
// ------------------ TVF_GlyphContainer ------------------
// ------------------

constructor TVF_GlyphContainer.Create(AFace: TVF_Face);
begin
  FFace := AFace;
  FCharMap := TVF_Charmap.Create(AFace);
  FGlyphList := TVF_GlyphList.Create;
end;

destructor TVF_GlyphContainer.Destroy;
var
  I: Integer;
begin
  for I := 0 to FGlyphList.Count - 1 do
    FGlyphList[I].Free;
  FGlyphList.Destroy;
  FCharMap.Destroy;
end;

function TVF_GlyphContainer.GetGlyph(ACharacterCode: Cardinal): TVF_Glyph;
var
  I: Integer;
begin
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Result := FGlyphList[I]
  else
    Result := nil;
end;

function TVF_GlyphContainer.GetBBox(ACharacterCode: Cardinal): TVF_BBox;
var
  I: Integer;
begin
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Result := FGlyphList[I].BBox
  else
    Result.Null;
end;

function TVF_GlyphContainer.CharMap(AEncoding: FT_Encoding): Boolean;
begin
  Result := FCharMap.CharMap(AEncoding);
  FErr := FCharMap.Error;
end;

function TVF_GlyphContainer.FontIndex(ACharacterCode: Cardinal): Cardinal;
begin
  Result := FCharMap.FontIndex(ACharacterCode)
end;

procedure TVF_GlyphContainer.Add(AGlyph: TVF_Glyph; ACharacterCode: Cardinal);
begin
  FCharMap.InsertIndex(ACharacterCode, FGlyphList.Add(AGlyph));
end;

function TVF_GlyphContainer.AddToMesh(ACharacterCode, ANextCharacterCode
  : Cardinal; APen: TVector3f; AMesh: TMeshAtom): TVector3f;
var
  KernAdvance: TVector3d;
  adv: Single;
  left, right: Cardinal;
begin
  adv := 0;

  left := FCharMap.FontIndex(ACharacterCode);
  right := FCharMap.FontIndex(ANextCharacterCode);

  if FFace.Error = 0 then
  begin
    KernAdvance := FFace.KernAdvance(left, right);

    if FFace.Error = 0 then
      adv := FGlyphList[FCharMap.GlyphListIndex(ACharacterCode)].AddToMesh(AMesh, APen);

    KernAdvance[0] := KernAdvance[0] + adv;
    MakeVector(Result, KernAdvance);
  end;
end;

function TVF_GlyphContainer.Advance(ACharacterCode, ANextCharacterCode
  : Cardinal): Single;
var
  I: Integer;
  left, right: Cardinal;
  Width: Single;
begin
  left := FCharMap.FontIndex(ACharacterCode);
  right := FCharMap.FontIndex(ANextCharacterCode);

  Width := FFace.KernAdvance(left, right)[0];
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Width := Width + FGlyphList[I].Advance;

  Result := Width;
end;
{$ENDREGION}
{$REGION 'TVF_Font'}
// ------------------
// ------------------ TVF_Font ------------------
// ------------------

procedure TVF_Font.AddToMesh(const AStr: string; AMesh: TMeshAtom);
var
  I: Integer;
  G, ng: Cardinal;
  KernAdvance: TVector3f;
begin
  FPen := NullVector;
  for I := 1 to Length(AStr) do
  begin
    GetGlyphs(AStr, I, G, ng);
    CheckGlyph(G);
    CheckGlyph(ng);
    KernAdvance := FGlyphList.AddToMesh(G, ng, FPen, AMesh);
    AddVector(FPen, KernAdvance);
  end;
end;

function TVF_Font.Advance(const AStr: string): Single;
var
  I: Integer;
  G, ng: Cardinal;
  w: Single;
begin
  w := 0;
  for I := 1 to Length(AStr) do
  begin
    GetGlyphs(AStr, I, G, ng);
    CheckGlyph(G);
    CheckGlyph(ng);
    w := w + FGlyphList.Advance(G, ng);
  end;
  Result := w;
end;

function TVF_Font.BBox(const AStr: string): TAABB;
var
  totalBBox, tempBBox: TVF_BBox;
  I: Integer;
  G, ng: Cardinal;
  adv: Single;
begin
  totalBBox.Null;

  if Length(AStr) > 0 then
  begin
    GetGlyphs(AStr, 1, G, ng);
    totalBBox := FGlyphList.BBox[G];
    adv := FGlyphList.Advance(G, ng);

    for I := 2 to Length(AStr) do
    begin
      GetGlyphs(AStr, I, G, ng);
      tempBBox := FGlyphList.BBox[G];
      tempBBox.Move(Vector3dMake(adv, 0, 0));
      totalBBox := totalBBox + tempBBox;
      adv := adv + FGlyphList.Advance(G, ng);
    end;
  end;

  Result.min[0] := totalBBox.FlowerX;
  Result.min[1] := totalBBox.FlowerY;
  Result.min[2] := totalBBox.FlowerZ;
  Result.max[0] := totalBBox.FupperX;
  Result.max[1] := totalBBox.FupperY;
  Result.max[2] := totalBBox.FupperZ;
end;

function TVF_Font.CharMap(AEncoding: FT_Encoding): Boolean;
begin
  Result := FFace.CharMapCount > 0;
end;

procedure TVF_Font.CheckGlyph(G: Cardinal);
var
  newGlyph: TVF_Glyph;
begin
  if not Assigned(FGlyphList) then
  begin
    GLSLogger.LogWarning('FREETYPE font size is undefined - use default');
    FaceSize(1, 72);
  end;

  if (G > 0) and (FGlyphList.Glyph[G] = nil) then
  begin
    newGlyph := MakeGlyph(FGlyphList.FontIndex(G));
    if Assigned(newGlyph) then
      FGlyphList.Add(newGlyph, G);
  end;
end;

constructor TVF_Font.Create(const AFontName: string);
begin
  FFace := TVF_Face.Create(AFontName);
  FErr := FFace.Error;
end;

constructor TVF_Font.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  FFace := TVF_Face.Create(pBufferBytes, bufferSizeInBytes);
  FErr := FFace.Error;
end;

destructor TVF_Font.Destroy;
begin
  FGlyphList.Free;
  FFace.Free;
  inherited;
end;

function TVF_Font.GetAscender: Single;
begin
  Assert(Assigned(FCharSize));
  Result := FCharSize.Ascender;
end;

function TVF_Font.GetDescender: Single;
begin
  Assert(Assigned(FCharSize));
  Result := FCharSize.Descender;
end;

procedure TVF_Font.GetGlyphs(const AStr: string; APos: Integer;
  out AGlyph, AnextGlyph: Cardinal);
begin
  AGlyph := Cardinal(AStr[APos]);
  if APos < Length(AStr) then
    AnextGlyph := Cardinal(AStr[APos + 1])
  else
    AnextGlyph := 0;
end;

function TVF_Font.FaceSize(asize, ares: Cardinal): Boolean;
begin
  FCharSize := FFace.Size(asize, ares);
  if FFace.Error <> 0 then
  begin
    GLSLogger.LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
    Exit(False);
  end;

  if Assigned(FGlyphList) then
    FGlyphList.Destroy;

  FGlyphList := TVF_GlyphContainer.Create(FFace);
  Result := True;
end;

{$ENDREGION}
{$REGION 'TVF_PolygonFont'}
// ------------------
// ------------------ TVF_PolygonFont ------------------
// ------------------

function TVF_PolygonFont.MakeGlyph(G: Cardinal): TVF_Glyph;
var
  ftGlyph: FT_GlyphSlot;
  tempGlyph: TVF_PolyGlyph;
begin
  ftGlyph := FFace.Glyph(G, FT_LOAD_NO_HINTING);

  if Assigned(ftGlyph) then
  begin
    tempGlyph := TVF_PolyGlyph.Create(ftGlyph);
    Result := tempGlyph;
    Exit;
  end;

  FErr := FFace.Error;
  Result := nil;
end;

{$ENDREGION}
{$REGION 'TVF_ExtrudedFont'}
// ------------------
// ------------------ TVF_ExtrudedFont ------------------
// ------------------

constructor TVF_ExtrudedFont.Create(const AFontName: string);
begin
  inherited Create(AFontName);
  FDepth := 0;
end;

constructor TVF_ExtrudedFont.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  inherited Create(pBufferBytes, bufferSizeInBytes);
  FDepth := 0;
end;

function TVF_ExtrudedFont.MakeGlyph(G: Cardinal): TVF_Glyph;
var
  ftGlyph: FT_GlyphSlot;
  tempGlyph: TVF_ExtrGlyph;
begin
  ftGlyph := FFace.Glyph(G, FT_LOAD_NO_HINTING);

  if Assigned(ftGlyph) then
  begin
    tempGlyph := TVF_ExtrGlyph.Create(ftGlyph, FDepth);
    Result := tempGlyph;
    Exit;
  end;

  FErr := FFace.Error;
  Result := nil;
end;

{$ENDREGION}
{$REGION 'TGLFreetypeVectorFont'}
// ------------------
// ------------------ TGLFreetypeVectorFont ------------------
// ------------------

procedure TGLFreetypeVectorFont.BuildFont;
var
  lPath: array [0 .. 255] of WideChar;
  sPath: string;
  I: Integer;
  ch, ch1, ch2: WideChar;
begin
{$IFDEF MSWINDOWS}
  GetWindowsDirectoryW(lPath, 255);
  sPath := IncludeTrailingPathDelimiter(lPath) + IncludeTrailingPathDelimiter
    ('Fonts') + FSystemFont.Name + '.ttf';
{$ELSE}
  raise Exception.Create('Not yet implemented');
{$ENDIF}

  if Extrusion > 0 then
  begin
    FFTFont := TVF_ExtrudedFont.Create(sPath);
    TVF_ExtrudedFont(FFTFont).depth := Extrusion;
  end
  else
    FFTFont := TVF_PolygonFont.Create(sPath);

  if FFTFont.Error = 0 then
  begin
    FFTFont.FaceSize(FFaceSize, 72);

    for I := 0 to Ranges.Count - 1 do
    begin
      with Ranges[I] do
      begin
        ch1 := StartASCII[1];
        ch2 := StopASCII[1];
      end;

      for ch := ch1 to ch2 do
        FFTFont.CheckGlyph(Cardinal(ch));
    end;
  end;
end;

procedure TGLFreetypeVectorFont.BuildString(ABatch: TDrawBatch;
  const aText: UnicodeString);
begin
  with ABatch.Mesh do
    try
      Lock;
      Clear;
    finally
      UnLock;
    end;

  if Length(aText) = 0 then
    Exit;

  if not Assigned(FFTFont) then
    PrepareGeometry(True);

  if FFTFont.Error <> 0 then
  begin
    FreeAndNil(FFTFont);
    Exit;
  end;

  FFTFont.AddToMesh(aText, ABatch.Mesh);
end;

constructor TGLFreetypeVectorFont.Create(AOwner: TComponent);
begin
  inherited;
  FSystemFont := TFont.Create;
  FSystemFont.OnChange := OnFontChanged;
  FFaceSize := 1;
  with Ranges.Add do
  begin
    StartASCII := ' ';
    StopASCII := '}';
  end;
end;

destructor TGLFreetypeVectorFont.Destroy;
begin
  FSystemFont.Free;
  FFTFont.Free;
  FFinishEvent.Free;
  inherited;
end;

function TGLFreetypeVectorFont.GetAABB(const aText: string): TAABB;
begin
  if Assigned(FFTFont) and (FFTFont.Error = 0) then
    Result := FFTFont.BBox(aText)
  else
    Result := BBToAABB(NullBoundingBox);
end;

procedure TGLFreetypeVectorFont.Loaded;
begin
  inherited Loaded;
  PrepareGeometry(False);
end;

procedure TGLFreetypeVectorFont.OnFontChanged(Sender: TObject);
begin
  FreeAndNil(FFTFont);
  InvalidateUsers;
end;

procedure TGLFreetypeVectorFont.PrepareGeometry(AnUrgently: Boolean);
begin
{$IFDEF GLS_SERVICE_CONTEXT}
  if IsServiceContextAvaible and not AnUrgently then
  begin
    if not Assigned(FFinishEvent) then
    begin
      FFinishEvent := TFinishTaskEvent.Create;
      AddTaskForServiceContext(BuildFont, FFinishEvent);
    end
    else if FFinishEvent.WaitFor(0) = wrSignaled then
    begin
      FFinishEvent.ResetEvent;
      AddTaskForServiceContext(BuildFont, FFinishEvent);
    end;
    exit;
  end
  else
{$ENDIF GLS_SERVICE_CONTEXT}
    BuildFont;
end;

procedure TGLFreetypeVectorFont.SetFaceSize(const Value: Integer);
begin
  if (Value > 0) and (FFaceSize <> Value) then
  begin
    FFaceSize := Value;
    FreeAndNil(FFTFont);
    InvalidateUsers;
  end;
end;

procedure TGLFreetypeVectorFont.SetFont(Value: TFont);
begin
  FSystemFont.Assign(Value);
  FreeAndNil(FFTFont);
  InvalidateUsers;
end;

procedure TGLFreetypeVectorFont.SetExtrusion(Value: Single);
begin
  Value := MaxFloat(Value, 0);
  if FExtrusion <> Value then
  begin
    FExtrusion := Value;
    FreeAndNil(FFTFont);
    InvalidateUsers;
  end;
end;

{$ENDREGION}

initialization

TVF_Library.Initialize;
RegisterClasses([TGLFreetypeVectorFont]);

finalization

TVF_Library.Finalize;
DoDestroyInFinal;

end.
