// 14/11/02 - EG - Fixed warnings

{-----------------------------------------------------------------------------
 Unit Name:  Lightwave
 Author:     Brian Johns brianjohns1@hotmail.com
 Purpose:    Lightwave object support unit for Delphi.

 Notes:      For the Lightwave Object File Format documentation please refer to
             http://www.lightwave3d.com/developer.

             This unit provides functions, constants and now classes for use in
             working with Lightwave3D Object files.

             Chunk ID constants are defined for all of the Chunk IDs listed
             in the Lightwave 7.5 sdk.

             It is important to note that this is a constant work-in-progress
             and as such there are omissions and may be errors. Feedback and
             suggestions would be appreciated.

             There are two ways of using this unit. The first uses user-defines
             callbacks to handle parsing lwo chunk data. The second way uses
             object orientation.

             Loading LWO chunk data via callbacks
             ====================================

             A function is provided for loading a Lightwave object from a file.
             The Loader itself uses a callback mechanism for the loading of
             Lightwave object chunks. The callback is called for every chunk
             (with the exception of the FORM and LWOB or LWO2 chunks).

             The Chunk struct passed in the callback contains members for the
             chunk ID, chunk size and pointer to chunk data. This data is
             untouched internally so any parsing and numeric formatting
             is up to you. This provides maximum flexibility and allows you to
             handle the data that you need without loading the entire object
             into ram first.

             The chunk data memory is freed upon the return of the callback
             so do not keep a reference to the chunk data. Copy it to your own
             storage.

               function LoadLW0(const Filename: string; ReadProc: TLWOReadProc;
                 UserData: Pointer): LongWord; cdecl;

                 Filename:      The fully qualified filename of the file to be
                                loaded.

                 ReadCallback:  The address of a TLWOReadCallback procedure
                                defined as:

                                TLWOReadCallback = procedure(Chunk: TLWChunk;
                                  UserData: Pointer); cdecl;

                                This procedure will be called for every chunk
                                encountered in the Lightwave object file. The
                                Chunk parameter is the chunk struct of the chunk
                                being loaded. UserData is the pointer supplied
                                in the original call to LoadLWO (see below).


                 UserData:      A pointer to user supplied data to be passed
                                in the ReadCallback.


               A non-zero results indicates that the object file was parsed
               successfully.

             Loading LWO chunks via objects
             ==============================

             To load data from a lightwave object file, create an instance of
             TLWObjectFile and call its LoadFromFile method.

             The data can then be accessed with the Chunks array property and
             iterated in combination with the ChunkCount property.

             Chunk data is parsed and interfaced by descendents of the TLWChunk
             class. I have made handlers for the following chunk types:

               TLWLayr  Modeler Layer chunk
               TLWPnts  Points chunk
               TLWPols  Polygons chunk
               TLWPTag  Polygon tag mapping
               TLWSurf  Surface subchunk container
               TLWTags  Tags (Name tag strings for named items)
               TLWVMap  Vertex Mapping

             The data for chunks without handlers can be gotten at with the
             Data and Size properties of the TLWChunk. Data is a pointer to
             the start of the chunk data. This data is unparsed.
             Data is nil for descendents.


             This should provide enough to move geometry into your favourite
             delphi-based 3d engine.


             Making chunk handler objects
             ============================

             All chunk types are derived from TLWChunk in the following manner:

             TLWChunk

               ex:

               TLWPTag        <- PTAG chunk type. polygon tag map.



               TLWParentChunk <- A base class for chunks that can contain other chunks.
                                 This is not necessarily how the data is stored in
                                 the file but more for ease of access to data.

                 ex:

                 TLWPnts <- PNTS chunk type (points)
                 TLWLayr <- LAYR chunk type (modeler layer)

                 TLWSurf <- SURF chunk type (constains surface attributes as sub chunks)

               TLWSubChunk <- A base class for chunks whose max data len is 65536 bytes.
                 TLWDiff   <- DIFF subchunk type (diffuse surface parameter)
                 TLWSpec   <- SPEC subchunk type (specularity surface parameter)...
                 etc.

             Each descendent of TLWChunk or TLWSubChunk is required to override
             the GetID class function, the LoadData method and the Clear method
             to provide custom handling for chunktype data.


             ex:

             ...

             type

             TLWPnts = class (TLWParentChunk)
              private
                FPoints: TVEC12DynArray;
                function GetCount: LongWord;
              protected
                procedure Clear; override;
                procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); override;
              public
                class function GetID: TID4; override;
                function GetVMap(VMapID: TID4; out VMap: TLWVMap): boolean;
                property Count: LongWord read GetCount;
                property Points: TVEC12DynArray read FPoints;
              end;

             ...


              /* Return the the chunk id that is the target of this handler */
              class function TLWPnts.GetID: TID4;
              begin
                result := ID_PNTS;
              end;

              /* Load the point data -
                 the stream is already positioned at the start of the chunk data
              */
              procedure TLWPnts.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
              begin
                SetLength(FPoints,DataSize div 12); // allocate storage for DataSize div 12 points
                ReadMotorolaNumber(AStream,@FPoints[0],4,DataSize div 4); // read the point data
              end;

              /* Cleanup - Free any memory that you've allocated */
              procedure TLWPnts.Clear;
              begin
                SetLength(FPoints,0);
              end;

             Utility Functions
             =================
             A function is provided for converting an array of numbers between
             Motorola and Intel format (big endian <-> little endian). Converting
             only needs to be done for numeric types that are of 2 or 4 byte
             lengths.

               procedure ReverseByteOrder(ValueIn: Pointer; Size: integer;

                 Count: integer = 1);

                 ValueIn: The address of a number or array of numbers to have their
                          bytes swapped.

                 Size:    The size in bytes of each numeric type.

                 Count:   The count of numbers in the numbers array. The default
                          value is 1.


             Two routines are provided for reading and writing big endian
             (Motorola and misc other processor vendors ) numbers to and from a
             stream. These routines handle 2 and 4 byte numeric types and can
             also handle arrays.

               procedure ReadMotorolaNumber(Stream: TStream; Data: Pointer;
                 ElementSize: integer; Count: integer = 1);

               function WriteMotorolaNumber(Stream: TStream; Data: Pointer;
                 ElementSize: integer; Count: integer = 1): Integer;

               Each take a valid TStream descendent, a pointer to the numeric data,
               the element size of the data elements (either 2 or 4) and the array
               element count if sending an array. The default count is 1.



             Notes for improvement of this unit:

               - A version ID tag should be visible to all chunks in order to
                 provide handling for Lightwave pre 6.0 object files.

               - Chunk type handlers should leave memory allocation to
                 the base class (TLWChunk) and act more as an interface
                 to the data pointed to by Data in TLWChunk. This would
                 keep memory allocation very efficient and make implementing
                 chunk handlers even easier.

               - A future Lightwave support unit could possibly benefit from the
                 use of delphi's interface type.

 License:    This unit is distributed under the Mozilla Public License.
             For license details, refer to http://www.mozilla.org
             Lightwave3D is a registered trademark of Newtek Incorporated.

-----------------------------------------------------------------------------}
unit LWObjects;

interface

uses Classes;

const
  ID_NULL = '#0#0#0#0'; // NULL ID

  ID_LWSC = 'LWSC';  // Lightwave scene file
  ID_FORM = 'FORM';  // IFF Form
  ID_LWOB = 'LWOB';  // Lightwave Object version 1.0 - 5.x
  ID_LWLO = 'LWLO';  // Lightwave Layered Object
  ID_LAYR = 'LAYR';  // LAYER
  ID_PNTS = 'PNTS';  // Points chunk
  ID_SRFS = 'SRFS';  // Surface Names chunk
  ID_POLS = 'POLS';  // Polygons chunk
  ID_CRVS = 'CRVS';  // Curves chunk
  ID_PCHS = 'PCHS';  // Patches chunk
  ID_SURF = 'SURF';  // Surfaces chunk
  ID_COLR = 'COLR';  // Color chunk

  ID_FLAG = 'FLAG';  // Surface Flags

  ID_LUMI = 'LUMI';  // Luminosity
  ID_DIFF = 'DIFF';  // Diffuse
  ID_SPEC = 'SPEC';  // Specular
  ID_REFL = 'REFL';  // Reflective
  ID_TRAN = 'TRAN';  // Transparency

  ID_VLUM = 'VLUM';  // Luminosity
  ID_VDIF = 'VDIF';  // Diffuse
  ID_VSPC = 'VSPC';  // Specularity
  ID_VRFL = 'VRFL';  // Reflective
  ID_VTRN = 'VTRN';  // Transparency

  ID_GLOS = 'GLOS';  // Glossiness SmallInt

  ID_SIDE = 'SIDE';  // Sidedness

  ID_RFLT = 'RFLT';  // REFLECTION MODE
  ID_RIMG = 'RIMG';  // REFLECTION IMAGE
  ID_RSAN = 'RSAN';  // REFLECTION MAP SEAM ANGLE
  ID_RIND = 'RIND';  // REFRACTIVE INDEX
  ID_EDGE = 'EDGE';  // EDGE TRANSPARENCY THRESHOLD
  ID_SMAN = 'SMAN';  // SMOOTHING ANGLE RADIANS
  ID_ALPH = 'ALPH';  // ALPHA MODE
  ID_CTEX = 'CTEX';  // COLOR TEXTURE
  ID_DTEX = 'DTEX';  // DIFFUSE TEXTURE
  ID_STEX = 'STEX';  // SPECULAR TEXTURE
  ID_RTEX = 'RTEX';  // REFLECTIION TEXTURE
  ID_TTEX = 'TTEX';  // TRANSPARENCY TEXTURE
  ID_LTEX = 'LTEX';  // LUMINANCE TEXTURE
  ID_BTEX = 'BTEX';  // BUMP TEXTURE
  ID_TFLG = 'TFLG';  // TEXTURE FLAGS
  ID_TSIZ = 'TSIZ';  // TEXTURE SIZE
  ID_TCTR = 'TCTR';  // TEXTURE CENTER
  ID_TFAL = 'TFAL';  // TEXTURE FALLOFF
  ID_TVEL = 'TVAL';  // TEXTURE VALUE
  ID_TREF = 'TREF';  // TEXTURE REFERENCE
  ID_TCLR = 'TCLR';  // TEXTURE COLOR
  ID_TVAL = 'TVAL';  // TEXTURE VALUE
  ID_TAMP = 'TAMP';  // TEXTURE AMPLITUDE
  ID_TFP0 = 'TFP0';  // TEXTURE PARAMETERS
  ID_TFP1 = 'TFP1';  //
  ID_TFP2 = 'TFP2';  //
  ID_TIP0 = 'TIP0';  //
  ID_TIP1 = 'TIP1';  //
  ID_TIP2 = 'TIP2';  //
  ID_TSP0 = 'TSP0';  //
  ID_TSP1 = 'TSP1';  //
  ID_TSP2 = 'TSP2';  //
  ID_TFRQ = 'TFRQ';  //
  ID_TIMG = 'TIMG';  // TEXTURE IMG
  ID_TALP = 'TALP';  //
  ID_TWRP = 'TWRP';  // TEXTURE WRAP
  ID_TAAS = 'TAAS';  //
  ID_TOPC = 'TOPC';  //
  ID_SHDR = 'SHDR';  //
  ID_SDAT = 'SDAT';  //
  ID_IMSQ = 'IMSQ';  // IMAGE SEQUENCE
  ID_FLYR = 'FLYR';  // FLYER SEQUENCE
  ID_IMCC = 'IMCC';  //

  SURF_FLAG_LUMINOUS        =     1;
  SURF_FLAG_OUTLINE         =     2;
  SURF_FLAG_SMOOTHING       =     4;
  SURF_FLAG_COLORHIGHLIGHTS =     8;
  SURF_FLAG_COLORFILTER     =    16;
  SURF_FLAG_OPAQUEEDGE      =    32;
  SURF_FLAG_TRANSPARENTEDGE =    64;
  SURF_FLAG_SHARPTERMINATOR =   128;
  SURF_FLAG_DOUBLESIDED     =   256;
  SURF_FLAG_ADDITIVE        =   512;
  SURF_FLAG_SHADOWALPHA     =  1024;

  CURV_CONTINUITY_FIRST = 1;
  CURV_CONTINUITY_LAST  = 2;

  IMSQ_FLAG_LOOP      = 1;
  IMSQ_FLAG_INTERLACE = 2;

  ID_LWO2  = 'LWO2';   // OBJECT
  ID_VMAP =  'VMAP';   // VERTEX MAP
  ID_TAGS =  'TAGS';   // TAGS?
  ID_PTAG =  'PTAG';   // POLYGON TAG MAP
  ID_VMAD =  'VMAD';   // DISCONTINUOUS VERTEX MAP
  ID_ENVL =  'ENVL';   // ENVELOPE
  ID_CLIP =  'CLIP';   // CLIP
  ID_BBOX =  'BBOX';   // BOUNDING BOX
  ID_DESC =  'DESC';   // DESCRIPTION
  ID_TEXT =  'TEXT';   // TEXT
  ID_ICON =  'ICON';   // ICON

  ENVL_PRE  = 'PRE'#0;   // PRE-BEHAVIOUR
  ENVL_POST = 'POST';    // POST
  ENVL_KEY  = 'KEY'#0;   // KEY
  ENVL_SPAN = 'SPAN';    // SPAN
  ENVL_CHAN = 'CHAN';    // CHAN
  ENVL_NAME = 'NAME';    // NAME

  CLIP_STIL   = 'STIL';   // STILL IMAGE FILENAME
  CLIP_ISEQ   = 'ISEQ';   // IMAGE SEQUENCE
  CLIP_ANIM   = 'ANIM';   // PLUGIN ANIMATION
  CLIP_STCC   = 'STCC';   // COLOR CYCLING STILL
  CLIP_CONT   = 'CONT';   // CONTRAST
  CLIP_BRIT   = 'BRIT';   // BRIGHTNESS
  CLIP_SATR   = 'SATR';   // SATURATION
  CLIP_HUE    = 'HUE'#0;  // HUE
  CLIP_GAMMA  = 'GAMMA';  // GAMMA
  CLIP_NEGA   = 'NEGA';   // NEGATIVE IMAGE
  CLIP_IFLT   = 'IFLT';   // IMAGE PLUG-IN FILTER
  CLIP_PFLT   = 'PFLT';   // PIXEL PLUG-IN FILTER

  POLS_TYPE_FACE = 'FACE';  // FACES
  POLS_TYPE_CURV = 'CURV';  // CURVE
  POLS_TYPE_PTCH = 'PTCH';  // PATCH
  POLS_TYPE_MBAL = 'MBAL';  // METABALL
  POLS_TYPE_BONE = 'BONE';  // SKELEGON?

  VMAP_TYPE_PICK = 'PICK';  // SELECTION SET
  VMAP_TYPE_WGHT = 'WGHT';  // WEIGHT MAP
  VMAP_TYPE_MNVW = 'MNVW';  // SUBPATCH WEIGHT MAP
  VMAP_TYPE_TXUV = 'TXUV';  // UV MAP
  VMAP_TYPE_RGB  = 'RGB'#0; // RGB MAP
  VMAP_TYPE_RGBA = 'RGBA';  // RGBA MAP
  VMAP_TYPE_MORF = 'MORF';  // MORPH MAP: RELATIVE VERTEX DISPLACEMENT
  VMAP_TYPE_SPOT = 'SPOT';  // SPOT MAP: ABSOLUTE VERTEX POSITIONS

  PTAG_TYPE_SURF = 'SURF';  // SURFACE
  PTAG_TYPE_PART = 'PART';  // PARENT PART
  PTAG_TYPE_SMGP = 'SMGP';  // SMOOTH GROUP

  PRE_POST_RESET         = 0; // RESET
  PRE_POST_CONSTANT      = 1; // CONSTANT
  PRE_POST_REPEAT        = 2; // REPEAT
  PRE_POST_OSCILLATE     = 3; // OSCILLATE
  PRE_POST_OFFSET        = 4; // OFFSET REPEAT
  PRE_POST_LINEAR        = 5; // LINEAR

  POLS_VCOUNT_MASK       = $3FF;
  POLS_FLAGS_MASK        = $FC00;

  SIDE_FRONT = 1;
  SIDE_BACK  = 2;
  SIDE_FRONT_AND_BACK = SIDE_FRONT and SIDE_BACK;

type

  TID4 = array[0..3] of char;
  PID4 = ^TID4;
  TID4DynArray = array of TID4;

  TI1 = ShortInt;
  PI1 = ^TI1;

  TI2 = SmallInt;
  PI2 = ^TI2;

  TI4 = LongInt;
  PI4 = ^TI4;

  TU1 = Byte;
  PU1 = ^TU1;
  TU1DynArray = array of TU1;

  TU2 = Word;
  PU2 = ^TU2;
  TU2Array = array [0..65534] of TU2;
  PU2Array = ^TU2Array;
  TU2DynArray = array of TU2;

  TU4 = LongWord;
  PU4 = ^TU4;
  TU4Array = array [0..65534] of TU4;
  PU4Array = ^TU4Array;
  TU4DynArray = array of TU4;

  TF4 = Single;
  PF4 = ^TF4;
  TF4Array = array [0..65534] of TF4;
  PF4Array = ^TF4Array;
  TF4DynArray = array of TF4;

  TANG4 = TF4;
  PANG4 = ^TANG4;

  TS0 = PChar;

  TVec12 = array[0..2] of  TF4;
  PVec12 = ^TVec12;

  TVec12Array = array [0..65534] of TVec12;
  PVec12Array = ^TVec12Array;
  TVec12DynArray = array of TVec12;

  TColr12 = TVec12;
  PColr12 = ^TColr12;

  TColr12DynArray = array of TColr12;

  TColr4 = array[0..3] of TU1;
  PColr4 = ^TColr4;

  {{ Lightwave Chunk Struct - Used in TLWOReadCallback }
  PLWChunkRec = ^TLWChunkRec;
  TLWChunkRec = record
    id: TID4;
    size: TU4;
    data: Pointer;
  end;

  {{ Lightwave SubChunk Struct - Used in TLWOReadCallback }
  PLWSubChunkRec = ^TLWSubChunkRec;
  TLWSubChunkRec = record
    id: TID4;
    size: TU2;
    data: Pointer;
  end;

  TLWPolygon = record
    vcount,
    indices: PU2;
  end;

  TLWPolyDynArray = TU2DynArray;

  TLWPolyTagMapDynArray = TU2DynArray;
  TLWPolyTagMap = record
     poly: TU2;
     tag: TU2;
  end;
  PLWPolyTagMap = ^TLWPolyTagMap;

  {{ Value Map }
  TLWVertexMap = record
    vert: TU2;
    values: TF4DynArray;
  end;

  TLWVertexMapDynArray = array of TLWVertexMap;

  TLWChunk = class (TPersistent)
  private
    FData: Pointer;
    FID: TID4;
    FSize: TU4;
  protected
    procedure Clear; virtual;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            virtual;
  public
    destructor Destroy; override;
    class function GetID: TID4; virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    property Data: Pointer read FData;
    property ID: TID4 read FID;
    property Size: TU4 read FSize;
  end;
  
  TLWChunkClass = class of TLWChunk;

  TLWSubChunk = class (TLWChunk)
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;
  
  TLWChunkList = class (TList)
  private
    FOwnsItems: Boolean;
    function GetItem(Index: integer): TLWChunk;
  public
    constructor Create(AOwnsItems: boolean);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function FindChunk(AId: TID4; StartIndex: integer = 0): Integer;
    property Items[Index: integer]: TLWChunk read GetItem; default;
    property OwnsItems: Boolean read FOwnsItems;
  end;
  
  TLWParentChunk = class (TLWChunk)
  private
    FItems: TLWChunkList;
    function GetItems: TLWChunkList;
  protected
    procedure Clear; override;
  public
    property Items: TLWChunkList read GetItems;
  end;
  

  TLWVMap = class;

  TLWPnts = class (TLWParentChunk)
  private
    FPoints: TVEC12DynArray;
    function GetCount: LongWord;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    class function GetID: TID4; override;
    function GetVMap(VMapID: TID4; out VMap: TLWVMap): Boolean;
    property Count: LongWord read GetCount;
    property Points: TVEC12DynArray read FPoints;
  end;
  
  TLWPols = class (TLWParentChunk)
  private
    FPolsType: TID4;
    FPolys: TLWPolyDynArray;
    FPolsCount: integer;
    function GetPolsByIndex(Index: TU2): Integer;
    function GetIndiceCount: TU4;
    function GetIndice(Index: integer): TU2;
    function GetPolsCount: integer;
    procedure CalcPolsCount;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord);
            override;
  public
    class function GetID: TID4; override;
    function GetPolsByVertex(VertIdx: TU2; var VertPolys: TU2DynArray): integer;
    property PolsByIndex[Index: TU2]: Integer read GetPolsByIndex;
    property IndiceCount: TU4 read GetIndiceCount;
    property Indices[Index: integer]: TU2 read GetIndice;
    property PolsType: TID4 read FPolsType;
    property PolsCount: integer read GetPolsCount;
  end;
  
  TLWVMap = class (TLWChunk)
  private
    FDimensions: TU2;
    FName: string;
    FValues: TLWVertexMapDynArray;
    FVMapType: TID4;
    function GetValue(Index: TU2): TLWVertexMap;
    function GetValueCount: Integer;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    class function GetID: TID4; override;
    property Dimensions: TU2 read FDimensions;
    property Name: string read FName;
    property Value[Index: TU2]: TLWVertexMap read GetValue;
    property ValueCount: Integer read GetValueCount;
    property VMapType: TID4 read FVMapType;
  end;
  
  TLWTags = class (TLWChunk)
  private
    FTags: TStrings;
    function GetTags: TStrings;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    destructor Destroy; override;
    class function GetId: TID4; override;
    function TagToName(Tag: TU2): string;
    property Tags: TStrings read GetTags;
  end;
  
  TLWSurf = class (TLWParentChunk)
  private
    FName: string;
    FSource: string;
    function GetParamAddr(Param: TID4): Pointer;
  protected
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    destructor Destroy; override;
    class function GetId: TID4; override;
    property Name: string read FName;
    property ParamAddr[Param: TID4]: Pointer read GetParamAddr;
    property Source: string read FSource;
  end;
  
  TLWLayr = class (TLWParentChunk)
  private
    FFlags: TU2;
    FName: string;
    FNumber: TU2;
    FParent: TU2;
    FPivot: TVec12;
  protected
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    destructor Destroy; override;
    class function GetId: TID4; override;
    property Flags: TU2 read FFlags;
    property Name: string read FName;
    property Number: TU2 read FNumber;
    property Parent: TU2 read FParent;
    property Pivot: TVec12 read FPivot;
  end;
  
  TLWPTag = class (TLWChunk)
  private
    FMapType: TID4;
    FTagMaps: TLWPolyTagMapDynArray;
    FTags: TU2DynArray;
    function AddTag(Value: TU2): Integer;
    function GetTag(Index: integer): TU2;
    function GetTagCount: Integer;
    function GetTagMapCount: Integer;
    function GetTagMaps(Index: Integer): TLWPolyTagMap;
    procedure ValidateTagInfo;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); 
            override;
  public
    constructor Create;
    function GetPolsByTag(Tag: TU2; var PolyIndices: TU2DynArray): Integer;
    class function GetId: TID4; override;
    property MapType: TID4 read FMapType;
    property TagCount: Integer read GetTagCount;
    property TagMapCount: Integer read GetTagMapCount;
    property TagMaps[Index: Integer]: TLWPolyTagMap read GetTagMaps; default;
    property Tags[Index: integer]: TU2 read GetTag;
  end;
  
  TLWObjectFile = class (TObject)
  private
    FChunks: TLWChunkList;
    FFileName: string;
    function GetChunks: TLWChunkList;
    function GetCount: Integer;
    function GetSurfaceByName(Index: string): TLWSurf;
    function GetSurfaceByTag(Index: TU2): TLWSurf;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename, ContentDir: string);
    procedure LoadFromStream(AStream: TStream);
    property ChunkCount: Integer read GetCount;
    property Chunks: TLWChunkList read GetChunks;
    property FileName: string read FFileName;
    property SurfaceByName[Index: string]: TLWSurf read GetSurfaceByName;
    property SurfaceByTag[Index: TU2]: TLWSurf read GetSurfaceByTag;
  end;
  
  TLWOReadCallback = procedure(Chunk: TLWChunkRec; Data: Pointer); cdecl;

  procedure RegisterChunkClass(ChunkClass: TLWChunkClass);

  function LoadLW0FromStream(Stream: TStream; ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord; cdecl;
  function LoadLWOFromFile(const AFilename: string; ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord;


  procedure ReadMotorolaNumber(Stream: TStream; Data: Pointer; ElementSize:
          integer; Count: integer = 1);
  function WriteMotorolaNumber(Stream: TStream; Data: Pointer; ElementSize:
          integer; Count: integer = 1): Integer;

  function ReadS0(Stream: TStream; out Str: string): Integer;
  procedure WriteS0(Stream: TStream; Data: string);

  procedure WriteU4AsVX(Stream:TStream; Data: Pointer; Count: integer);
  function ReadVXAsU4(Stream: TStream; Data: Pointer; Count: integer = 1): Integer;

  procedure ReverseByteOrder(ValueIn: Pointer; Size: integer; Count: integer = 1);

  function ID4ToInt(const Id: TID4): integer;

implementation

uses SysUtils, Contnrs;

type
  PWord = ^Word;
  PLongWord = ^LongWord;

var
  ChunkClasses: TClassList;

{-----------------------------------------------------------------------------
  Procedure: GetChunkClasses
  Date:      08-Aug-2002
  Arguments: None
  Result:    TClassList

  Singleton access for the chunk class list.
-----------------------------------------------------------------------------}
function GetChunkClasses: TClassList;
begin
  if ChunkClasses = nil then
    ChunkClasses := TClassList.Create;
  result := ChunkClasses;
end;

procedure UnRegisterChunkClasses;
var
  i: integer;
begin
  with GetChunkClasses do
    for i := 0 to Count - 1 do
      UnregisterClass(TPersistentClass(Items[i]));
end;


{-----------------------------------------------------------------------------
  Procedure: RegisterChunkClass
  Date:      08-Aug-2002
  Arguments: ChunkClass: TLWChunkClass
  Result:    None

  Adds a user defined chunk class to the chunk class list.
-----------------------------------------------------------------------------}
procedure RegisterChunkClass(ChunkClass: TLWChunkClass);
begin
  GetChunkClasses.Add(ChunkClass);
//  if FindClass(ChunkClass.ClassName) <> nil then
//    UnRegisterClass(ChunkClass);
//  RegisterClass(ChunkClass);
end;

{-----------------------------------------------------------------------------
  Procedure: GetChunkClass
  Date:      08-Aug-2002
  Arguments: ChunkID: TID4
  Result:    TLWChunkClass

  Returns the chunk class associated with ChunkID.
-----------------------------------------------------------------------------}
function GetChunkClass(ChunkID: TID4; ADefault: TLWChunkClass): TLWChunkClass;
var
  i: integer;
begin

  if ADefault = nil then
    result := TLWChunk
  else
    result := ADefault;

  for i := 0 to ChunkClasses.Count - 1 do
  begin

    if TLWChunkClass(ChunkClasses.Items[i]).GetID = ChunkID then
    begin

      result := TLWChunkClass(ChunkClasses.Items[i]);
      Exit;

    end;

  end;

end;

{-----------------------------------------------------------------------------
  Procedure: Tokenize
  Date:      08-Aug-2002
  Arguments: const Src: string; Delimiter: Char; Dst: TStrings
  Result:    None

  Breaks up a string into TStrings items when the Delimiter character is
  encountered.
-----------------------------------------------------------------------------}
procedure Tokenize(const Src: string; Delimiter: Char; Dst: TStrings);
var
  i,L,SL: integer;
  SubStr: string;
begin
  if Dst = nil then exit;
  L := Length(Src);
  if (L = 0) or (Dst = nil) then exit;
  for i := 1 to L do
  begin
    if (Src[i] <> Delimiter) then SubStr := SubStr + Src[i] else
    begin
      SL := Length(SubStr);
      if SL > 0 then
      begin
        Dst.Add(SubStr);
        SubStr := '';
      end;
    end;
  end;
  if Length(SubStr) > 0 then Dst.Add(SubStr);
end;

{-----------------------------------------------------------------------------
  Procedure: LoadLW0FromStream
  Date:      08-Aug-2002
  Arguments: Stream: TStream; ReadCallback: TLWOReadCallback; UserData: Pointer
  Result:    LongWord


-----------------------------------------------------------------------------}
function LoadLW0FromStream(Stream: TStream; ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord;
var
  Chunk: TLWChunkRec;
  CurId: TID4;
  StartPos, CurSize: TU4;

begin
  try
    Stream.Read(CurId,4);

    ReadMotorolaNumber(Stream,@CurSize,4);

    if UpperCase(CurId) = 'FORM' then
    begin

      Stream.Read(CurId,4);

    end else raise Exception.Create('Invalid magic number. Not a valid Lightwave Object');

    with Stream do while Position < Size do
    begin

      Read(Chunk,8);

      ReverseByteOrder(@Chunk.size,4);

      StartPos := Position;

      GetMem(Chunk.data,Chunk.size);

      Stream.Read(Chunk.data^,Chunk.size);

      if Assigned(ReadCallback) then ReadCallback(Chunk,UserData);

      FreeMem(Chunk.data,Chunk.size);

      Position := StartPos + Chunk.size + (StartPos + Chunk.size) mod 2;

    end;
    Stream.Free;
    result := High(LongWord);
  except
    On E: Exception do
    begin
      Stream.Free;
      result := 0;
    end;
  end;
end;

function LoadLWOFromFile(const AFilename: string; ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename,fmOpenRead);
  try
    result := LoadLW0FromStream(FileStream,ReadCallback,UserData);
  except
    on E: Exception do begin
      FileStream.Free;
      raise;
    end;
  end;
end;

procedure ReverseByteOrder(ValueIn: Pointer; Size: integer; Count: integer = 1);
var
  W: Word;
  L: LongWord;
  i: integer;
begin
  i := 0;
  case Size of
    2: begin

      while i < Count do
      begin

        W := PU2Array(ValueIn)^[i];

        asm

          mov ax,w;   { move w into ax register }
          xchg al,ah; { swap lo and hi byte of word }
          mov w,ax;   { move "swapped" ax back to w }

        end;

        PU2Array(ValueIn)^[i] := w;

        Inc(i);

      end;

    end;

    4: begin

      while i < Count do
      begin

        L := PU4Array(ValueIn)^[i];

        asm

          mov eax,l; { move l into eax register }
          BSWAP eax; { reverse the order of bytes in eax }
          mov l,eax; { move "swapped" eax back to 1 }

        end;

        PU4Array(ValueIn)^[i] := l;

        Inc(i);

      end;

    end;

  else
    raise Exception.Create('Lightwave.ReverseByteOrder: Invalid Size = ' + IntToStr(Size));

  end;

end;

procedure ReadMotorolaNumber(Stream: TStream; Data: Pointer; ElementSize:
        integer; Count: integer = 1);
begin

  Stream.Read(Data^,Count * ElementSize);

  if (ElementSize = 2) or (ElementSize = 4) then
    ReverseByteOrder(Data,ElementSize,Count);

end;

function WriteMotorolaNumber(Stream: TStream; Data: Pointer; ElementSize:
        integer; Count: integer = 1): Integer;
var
  TempData: Pointer;
begin
  result := 0;
  if Data <> nil then
  begin
    TempData := AllocMem(ElementSize * Count);
    try

      if (ElementSize = 2) or (ElementSize = 4) then
        ReverseByteOrder(TempData,ElementSize,Count);

      result := Stream.Write(Data,Count * ElementSize);
    except
      on E: Exception do
      begin
        FreeMem(TempData,Count * ElementSize);
        raise;
      end;
    end;
  end;

end;

function ReadS0(Stream: TStream; out Str: string): Integer;
var
  Buf: array[0..1] of char;
  StrBuf: string;
begin

  Stream.Read(Buf,2);

  while Buf[1] <> #0 do
  begin
    StrBuf := StrBuf + string(Buf);
    Stream.Read(Buf,2);
  end;

  if Buf[0] <> #0 then StrBuf := StrBuf + Buf[0];

  Str := Copy(StrBuf,1,Length(StrBuf));

  result := Length(Str) + 1;

  result := result + (result mod 2);

end;


function ValueOfVX(VX: Pointer): TU4;
var
  TmpU2: TU2;
  TmpU4: TU4;
begin
  if PU1(VX)^ = $FF then
  begin
    TmpU4 := TU4(PU1(VX)^) and $FFFFFFF0;
    ReverseByteOrder(@TmpU4,4);
  end else
  begin
    TmpU2 := TU2(PU1(VX)^);
    ReverseByteOrder(@TmpU2,2);
    TmpU4 := TmpU2;
  end;
  result := TmpU4;
end;

function ReadVXAsU4(Stream: TStream; Data: Pointer; Count: integer = 1): integer;
var
  i, ReadCount: integer;
  BufByte: byte;
  TempU2: TU2;
begin
  ReadCount := 0;
  for i := 0 to Count -1 do
  begin

    Stream.Read(BufByte,1);
    Stream.Position := Stream.Position - 1;

    if  BufByte = 255 then
    begin
      Stream.Read(Data^,SizeOf(TU4));
      PU4Array(Data)^[i] := PU4Array(Data)^[i] and $FFFFFFF0;
      ReverseByteOrder(Data,SizeOf(TU4));
      Inc(ReadCount,4);
    end else
    begin
      Stream.Read(TempU2,SizeOf(TU2));
      ReverseByteOrder(@TempU2,SizeOf(TU2));
      PU4Array(Data)^[i] := TempU2;
      Inc(ReadCount,2);
    end;

  end;
  result := ReadCount;
end;

function ReadVXAsU2(Stream: TStream; Data: Pointer; Count: integer = 1): integer;
var
  i, ReadCount: integer;
  BufByte: byte;
  TempU2: TU2;
begin
  ReadCount := 0;
  for i := 0 to Count -1 do
  begin

    Stream.Read(BufByte,1);
    Stream.Position := Stream.Position - 1;

    if  BufByte = 255 then
    begin
      Stream.Position := Stream.Position + 4;
      PU2Array(Data)^[i] := 0;
      Inc(ReadCount,4);
    end else
    begin
      Stream.Read(TempU2,SizeOf(TU2));
      ReverseByteOrder(@TempU2,SizeOf(TU2));
      PU2Array(Data)^[i] := TempU2;
      Inc(ReadCount,2);
    end;

  end;
  result := ReadCount;
end;



procedure WriteS0(Stream: TStream; Data: string);
begin
  {ToDo: WriteS0}
end;

procedure WriteU4AsVX(Stream:TStream; Data: Pointer; Count: integer);
var
  i: integer;
  TempU2: TU2;
begin
  for i := 0 to Count - 1 do
  begin
    if PU4Array(Data)^[i] < 65280 then
    begin
      TempU2 := PU4Array(Data)^[i];
      WriteMotorolaNumber(Stream,@TempU2,SizeOf(TU2));
    end else
      WriteMotorolaNumber(Stream,Data,SizeOf(TU4));
  end;
end;

type
  PInteger = ^Integer;

function ID4ToInt(const Id: TId4): integer;
var
  TmpId: string;
begin

  TmpId := Id;

  TmpId := UpperCase(Id);

  result := PInteger(@TmpId)^;

end;

{ TLWChunk }

{
*********************************** TLWChunk ***********************************
}
destructor TLWChunk.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLWChunk.Clear;
begin
  FreeMem(FData,FSize);
  FSize := 0;
  FData := nil;
end;

class function TLWChunk.GetID: TID4;
begin
  result := #0#0#0#0;
end;

procedure TLWChunk.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  GetMem(FData,DataSize);
  AStream.Read(PByteArray(FData)^[0],DataSize);
end;

procedure TLWChunk.LoadFromStream(AStream: TStream);
var
  DataStart: Integer;
  DataSize: TU4;
begin
  with AStream do
  begin
  
    ReadMotorolaNumber(AStream,@DataSize,4);
  
    DataStart := Position;
  
    FSize := DataSize;
  
    LoadData(AStream, DataStart,DataSize);
  
    Position := Cardinal(DataStart) + DataSize + (Cardinal(DataStart) + DataSize) mod 2;
  
  end;
end;

{ TLWChunks }

{
********************************* TLWChunkList *********************************
}
constructor TLWChunkList.Create(AOwnsItems: boolean);
begin
  FOwnsItems := AOwnsItems;
end;

destructor TLWChunkList.Destroy;
begin
  
  Clear;
  
  inherited;
  
end;

procedure TLWChunkList.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
  inherited;
end;

procedure TLWChunkList.Delete(Index: Integer);
begin
  if FOwnsItems then
    Items[Index].Free;
  inherited Delete(Index);
end;

function TLWChunkList.FindChunk(AId: TID4; StartIndex: integer = 0): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := StartIndex to Count - 1 do
  begin
    if Items[i].ID = AId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TLWChunkList.GetItem(Index: integer): TLWChunk;
begin
  result := TLWChunk(inherited Items[Index]);
end;

{ TLWObjectFile }

{
******************************** TLWObjectFile *********************************
}
constructor TLWObjectFile.Create;
begin
  
  inherited;
  
end;

destructor TLWObjectFile.Destroy;
begin
  
  FreeAndNil(FChunks);

  inherited;
  
end;

function TLWObjectFile.GetChunks: TLWChunkList;
begin
  
  if FChunks = nil then
    FChunks := TLWChunkList.Create(true);
  
  result := FChunks;
  
end;

function TLWObjectFile.GetCount: Integer;
begin
  result := Chunks.Count;
end;

function TLWObjectFile.GetSurfaceByName(Index: string): TLWSurf;
var
  SurfIdx: Integer;
begin
  result := nil;
  
  SurfIdx := Chunks.FindChunk(ID_SURF,0);
  
  while SurfIdx <> -1 do
  begin
    if TLWSurf(Chunks[SurfIdx]).Name = Index then
    begin
      result := TLWSurf(Chunks[SurfIdx]);
      exit;
    end;
    SurfIdx := Chunks.FindChunk(ID_SURF,SurfIdx + 1);

  end;
end;

function TLWObjectFile.GetSurfaceByTag(Index: TU2): TLWSurf;
var
  TagsInd: Integer;
begin
  TagsInd := Chunks.FindChunk(ID_TAGS,0);

  if TagsInd <> -1 then
    with TLWTags(Chunks[TagsInd]) do
      result := SurfaceByName[Tags[Index]]
  else result:=nil;
end;

procedure TLWObjectFile.LoadFromFile(const AFilename, ContentDir: string);
var
  Stream: TMemoryStream;
begin
  
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(AFilename);
  
    LoadFromStream(Stream);
    Stream.Free;
    FFileName := AFilename;
  except
    on E: Exception do
    begin
      Stream.Free;
      raise;
    end;
  end;
  
end;

procedure TLWObjectFile.LoadFromStream(AStream: TStream);
var
  CurId: TID4;
  CurSize: LongWord;
  CurPnts, CurPols, CurItems: TLWChunkList;
begin
   CurPols:=nil;
   CurPnts:=nil;

  AStream.Read(CurId,4);
  
  ReadMotorolaNumber(AStream,@CurSize,4);
  
  if UpperCase(CurId) = 'FORM' then
  begin
  
    AStream.Read(CurId,4);
  
    if CurId <> 'LWO2' then
      raise Exception.Create('Only Version 6.0+ version objects are supported.');
  
  end else raise Exception.Create('Invalid magic number. Not a valid Lightwave Object');
  
  CurItems := Chunks;
  
  while AStream.Position < AStream.Size do
  begin
    AStream.Read(CurId,4);
  
    if (CurId = ID_PTAG) then
    begin
      CurPols.Add(GetChunkClass(CurId, TLWChunk).Create);
  
      with CurPols[CurPols.Count - 1] do
      begin
        FID := CurId;
        LoadFromStream(AStream);
      end;
  
    end else
    if (CurId = ID_VMAP) or (CurId = ID_VMAD) then
    begin
      CurPnts.Add(GetChunkClass(CurId, TLWChunk).Create);
  
      with CurPnts[CurPnts.Count - 1] do
      begin
  
        FID := CurId;
        LoadFromStream(AStream);
  
      end;
    end else
  
    begin
  
      if (CurId = ID_LAYR) or (CurId = ID_SURF) or (CurId = ID_TAGS) then CurItems := Chunks;
  
      CurItems.Add(GetChunkClass(CurId, TLWChunk).Create);
  
      with CurItems[CurItems.Count - 1] do
      begin
        FID := CurId;
        LoadFromStream(AStream);
      end;
  
    end;
  
    if CurId = ID_LAYR then
      CurItems := TLWParentChunk(CurItems[CurItems.Count - 1]).Items
    else if CurId = ID_POLS then
      CurPols := TLWParentChunk(CurItems[CurItems.Count - 1]).Items
    else if CurId = ID_PNTS then
      CurPnts := TLWParentChunk(CurItems[CurItems.Count - 1]).Items;
  end;
end;

{ TLWPnts }

{
*********************************** TLWPnts ************************************
}
procedure TLWPnts.Clear;
begin
  SetLength(FPoints,0);
end;

function TLWPnts.GetCount: LongWord;
begin
  result := Length(FPoints);
end;

class function TLWPnts.GetID: TID4;
begin
  result := ID_PNTS;
end;

function TLWPnts.GetVMap(VMapID: TID4; out VMap: TLWVMap): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Items.Count - 1 do
  begin
    if (Items[i] is TLWVMap) and (TLWVMap(Items[i]).VMapType = VMapID) then
    begin
  
      result := true;
      VMap := TLWVMap(Items[i]);
      Exit;
    end;
  
  end;
  
end;

procedure TLWPnts.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  SetLength(FPoints,DataSize div 12); // allocate storage for DataSize div 12 points
  ReadMotorolaNumber(AStream,@FPoints[0],4,DataSize div 4); // read the point data
end;

{ TLWPols }

{
*********************************** TLWPols ************************************
}
procedure TLWPols.CalcPolsCount;
var
  i: integer;
begin

  FPolsCount := 0;
  i := 0;

  if IndiceCount = 0 then exit;

  while (Cardinal(i) < IndiceCount - 1) do
  begin
    Inc(i,Indices[i] + 1);
    Inc(FPolsCount);
  end;

end;

procedure TLWPols.Clear;
begin
  SetLength(FPolys,0);
end;

function TLWPols.GetPolsByIndex(Index: TU2): Integer;
var
  i, cnt: Cardinal;
begin
  result := -1;
  i := 0;
  cnt := 0;

  if Index = 0 then
  begin
    result := 0;
    exit;
  end;

  while (i < IndiceCount - 1) and (cnt <> Index) do
  begin
    Inc(i,Indices[i]+1);
    Inc(cnt);
  end;
  if cnt = Index then
    result := i;
end;

class function TLWPols.GetID: TID4;
begin
  result := ID_POLS;
end;

function TLWPols.GetIndiceCount: TU4;
begin
  result := Length(FPolys);
end;

function TLWPols.GetIndice(Index: integer): TU2;
begin
  result := FPolys[Index];
end;

function TLWPols.GetPolsCount: integer;
begin
  result := FPolsCount;
end;

procedure TLWPols.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  EndPos: Integer;
  Idx: TU4;
  TmpU2: TU2;
begin

  Idx := 0;
  EndPos := DataStart + DataSize;

  with AStream do
  begin

    Read(FPolsType,4);

    SetLength(FPolys,(DataSize - 4) div 2);

    while Position < EndPos do
    begin

      ReadMotorolaNumber(AStream,@FPolys[Idx],2);
      TmpU2 := FPolys[Idx] and POLS_VCOUNT_MASK;

      ReadVXAsU2(AStream,@FPolys[Idx + 1],TmpU2);
      Inc(Idx,FPolys[Idx] + 1);

    end;

    // correct length guestimate errors if any
    if (Idx + 1) < Cardinal(Length(FPolys)) then
      SetLength(FPolys,Idx + 1);

  end;

  CalcPolsCount;

end;



{ TLWVMap }

{
*********************************** TLWVMap ************************************
}
procedure TLWVMap.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FValues) - 1 do
    SetLength(FValues[i].values,0);
  
  SetLength(FValues,0);
end;

class function TLWVMap.GetID: TID4;
begin
  
  result := ID_VMAP;
  
end;

function TLWVMap.GetValue(Index: TU2): TLWVertexMap;
begin
  
  result := FValues[Index];
  
end;

function TLWVMap.GetValueCount: Integer;
begin
  result := Length(FValues);
end;

procedure TLWVMap.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  Idx: TU4;
begin
  Idx := 0;
  
  with AStream do
  begin
  
    Read(FVMapType,4);
    ReadMotorolaNumber(AStream,@FDimensions,2);
  
    ReadS0(AStream,FName);
  
    if FDimensions > 0 then
    begin
  
      while Position < (DataStart + DataSize) do
      begin
        SetLength(FValues,Length(FValues) + 1);
  
        ReadVXAsU2(AStream,@FValues[Idx].vert,1);
        SetLength(FValues[Idx].values,Dimensions * 4);
        ReadMotorolaNumber(AStream,@FValues[Idx].values[0],4,Dimensions);
  
        Inc(Idx);
      end;
  
    end;
  
  end;
end;

{ TLWTags }

{
*********************************** TLWTags ************************************
}
destructor TLWTags.Destroy;
begin
  inherited;
end;

procedure TLWTags.Clear;
begin
  FreeAndNil(FTags);
end;

class function TLWTags.GetId: TID4;
begin
  result := ID_TAGS;
end;

function TLWTags.GetTags: TStrings;
begin
  if FTags = nil then
    FTags := TStringList.Create;
  result := FTags;
end;

procedure TLWTags.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  EndPos: TU4;
  TmpStr: string;
begin
  EndPos := DataStart + DataSize;
  while AStream.Position < EndPos do
  begin
    ReadS0(AStream,TmpStr);
    Tags.Add(TmpStr);
    TmpStr := '';
  end;
end;

function TLWTags.TagToName(Tag: TU2): string;
begin
  result := Tags[Tag-1];
end;

{ TLWSubChunk }

{
********************************* TLWSubChunk **********************************
}
procedure TLWSubChunk.LoadFromStream(AStream: TStream);
var
  DataStart: Integer;
  DataSize: TU2;
begin
  
  with AStream do
  begin
  
    ReadMotorolaNumber(AStream,@DataSize,2);
  
    DataStart := Position;
  
    FSize := DataSize;
  
    LoadData(AStream,DataStart,DataSize);
  
    Position := DataStart + DataSize + (DataStart + DataSize) mod 2;
  
  end;
  
end;


{
*********************************** TLWLayr ************************************
}
destructor TLWLayr.Destroy;
begin
  inherited;
end;

class function TLWLayr.GetId: TID4;
begin
  result := ID_LAYR;
end;

procedure TLWLayr.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  
  ReadMotorolaNumber(AStream,@FNumber,2);
  ReadMotorolaNumber(AStream,@FFlags,2);
  ReadMotorolaNumber(AStream,@FPivot,4,3);
  ReadS0(AStream,FName);
  
  if ((DataStart + DataSize) - AStream.Position) > 2 then ReadMotorolaNumber(AStream,@FParent,2);
end;

{ TLWSurf }

{
*********************************** TLWSurf ************************************
}
destructor TLWSurf.Destroy;
begin
  inherited;
end;

class function TLWSurf.GetId: TID4;
begin
  result := ID_SURF;
end;

function TLWSurf.GetParamAddr(Param: TID4): Pointer;
var
  Idx: Integer;
begin
  
  result := nil;
  
  Idx := Items.FindChunk(Param,0);
  if Idx <> -1 then
    result := Items[Idx].Data;
  
end;

procedure TLWSurf.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  CurId: TID4;
begin
  
  ReadS0(AStream,FName);
  
  ReadS0(AStream,FSource);
  
  while AStream.Position < (DataStart + DataSize) do
  begin
  
    AStream.Read(CurId,4);
  
    Items.Add(GetChunkClass(CurId, TLWSubChunk).Create);
  
    with Items[Items.Count - 1] do
    begin
  
      FID := CurId;
      LoadFromStream(AStream);
  
    end;
  
  end;
  
end;

{ TLWPTag }

{
*********************************** TLWPTag ************************************
}
constructor TLWPTag.Create;
begin
  inherited;
end;

function TLWPTag.AddTag(Value: TU2): Integer;
var
  i, L: Integer;
begin
  result := -1;
  L := Length(FTags);

  for i := 0 to L - 1 do
    if Value = FTags[i] then
    begin
      result := i;
      exit;
    end;


  if result = -1 then
  begin

    SetLength(FTags,L + 1);
    FTags[L] := Value;
    result := L;

  end;

end;

procedure TLWPTag.Clear;
begin
  SetLength(FTagMaps,0);
  SetLength(FTags,0);
end;

function TLWPTag.GetPolsByTag(Tag: TU2; var PolyIndices: TU2DynArray): Integer;
var
  i: Integer;

  procedure AddPoly(Value: TU2);
  var
    L: integer;

  begin

    L := Length(PolyIndices);
    SetLength(PolyIndices,L+1);
    PolyIndices[L] := Value;

  end;

begin

  for i := 0 to TagMapCount -1 do

    if TagMaps[i].tag = Tag then

      AddPoly(TagMaps[i].poly);

  result := Length(PolyIndices);

end;

class function TLWPTag.GetId: TID4;
begin
  result := ID_PTAG;
end;

function TLWPTag.GetTag(Index: integer): TU2;
begin
  ValidateTagInfo;
  result := FTags[Index];
end;

function TLWPTag.GetTagCount: Integer;
begin
  ValidateTagInfo;
  result := Length(FTags);
end;

function TLWPTag.GetTagMapCount: Integer;
begin
  result := Length(FTagMaps) div 2;
end;

function TLWPTag.GetTagMaps(Index: Integer): TLWPolyTagMap;
begin
  result := PLWPolyTagMap(@FTagMaps[Index * 2])^;
end;

procedure TLWPTag.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  Idx: Integer;
begin

  Idx := 0;

  with AStream do
  begin
    Read(FMapType,4);

    SetLength(FTagMaps,(DataSize - 4) div 2);

    while Position < (DataStart + DataSize) do
    begin
      ReadVXAsU2(AStream, @FTagMaps[Idx]);
      ReadMotorolaNumber(AStream,@FTagMaps[Idx + 1],2);
      Inc(Idx, 2);
    end;

    // correct length guestimate errors if any
    if (Idx + 1) < Length(FTagMaps) then
      SetLength(FTagMaps,Idx + 1);

  end;

end;

procedure TLWPTag.ValidateTagInfo;
var
  i: Integer;

begin

  if Length(FTags) > 0 then exit;

  for i := 0 to TagMapCount -1 do
    AddTag(TagMaps[i].tag);

end;

{ TLWParentChunk }

{
******************************** TLWParentChunk ********************************
}
procedure TLWParentChunk.Clear;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TLWParentChunk.GetItems: TLWChunkList;
begin
  if FItems = nil then
    FItems := TLWChunkList.Create(true);
  result := FItems;
end;


function TLWPols.GetPolsByVertex(VertIdx: TU2;
  var VertPolys: TU2DynArray): integer;
var
  i,j,L: integer;
begin
   L:=0;

  if Length(VertPolys) >0 then
    SetLength(VertPolys,0);

  for i := 0 to PolsCount -1 do
  begin

    for j := 1 to Indices[PolsByIndex[i]] do
    begin

      if Indices[PolsByIndex[i] + j] = VertIdx then
      begin

        L := Length(VertPolys);

        SetLength(VertPolys, L + 1);

        VertPolys[L] := i;

      end;

    end;

  end;

  result := L;

end;

initialization

  {{ Pnts }
  RegisterChunkClass(TLWPnts);

  {{ Pols }
  RegisterChunkClass(TLWPols);

  {{ VMap }
  RegisterChunkClass(TLWVMap);

  {{ Tags }
  RegisterChunkClass(TLWTags);

  {{ PTAG }
  RegisterChunkClass(TLWPTAG);

  {{ SURF }
  RegisterChunkClass(TLWSurf);

  {{ LAYR }
  RegisterChunkClass(TLWLayr);

finalization
//  UnRegisterChunkClasses;
  FreeAndNil(ChunkClasses);

end.
