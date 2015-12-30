unit DGLTexture;

interface

Uses Windows, Classes, Contnrs, VCL.Graphics, SysUtils,
     dglOpenGL,
     //DGLVectorLists,
     DGLTextureFormat, DGLVectorMaths,
     //uBaseResource,
     DGLSLog,
     DGLState;

Type
  TSettedTexParams = set of (stFormats, stFilters, stData,
                             stSizes, stWraps, stTarget);
  TTexTarget = (ttTexture1D, ttTexture2D, ttTexture3D, ttTextureRectangle,
                ttTextureRectangleNV,
                ttCubemap, ttCubemapPX, ttCubemapPY, ttCubemapNX, ttCubemapNY,
                ttCubemapPZ, ttCubemapNZ, tt1DArray, tt2DArray, ttCubeMapArray);
  TTextureWraps = (twClamp, twRepeat, twClampToEdge, twClampToBorder, twMirrorRepeat);
  TTexGens = (tgDisable,tgObjectLinear,tgEyeLinear,tgSphereMap,tgNormalMap,tgReflectionMap);
  TMagFilter = (mgNearest, mgLinear);
  TMinFilter = (mnNearest, mnLinear, mnNearestMipmapNearest, mnNearestMipmapLinear,
                mnLinearMipmapNearest, mnLinearMipmapLinear);
{  TTextureEnvMode = (tmAdd = GL_ADD, tmModulate = GL_MODULATE,
              tmDecal = GL_DECAL, tmBlend = GL_BLEND,
              tmReplace = GL_REPLACE, tmCombine = GL_COMBINE);}
  TTextureBlendingModes = (tbmOpaque, tbmTransparency, tbmAdditive, tbmAlphaTest50,
                    tbmAlphaTest100, tbmModulate, tbmMesh);
  TMapTarget = (mtAmbient, mtDiffuse, mtSpecular, mtShininess, mtBumpMap,
                mtNormalMap, mtAlpha, mtOpacity, mtReflection);
  TMapTargets = set of TMapTarget;

  TTextureDecription = record
     InternalFormat: GLUInt;
     Precision: GLUInt;
     ColorChanels: GLUint;
     PixelSize: cardinal;
     WrapS, WrapT, WrapR: GLUInt;
     Target: GLEnum;
     minFilter: GLEnum;
     magFilter: GLEnum;
     TextureGenS: GLEnum;
     TextureGenT: GLEnum;
     TextureGenR: GLEnum;
     GenerateMipMaps: boolean;
     Data: pointer;
     Id, pboRBId,pboWBId: GLUint;
     FullSize: integer;
     Width, Height, Depth: integer;
     UsePBO: boolean;
     Created: boolean;
  end;
  PTextureDecription = ^TTextureDecription;

  TTexture = class;

  TTextureLibrary = class (TObjectList)
  private
    FHashList: TIntegerList;
    FLocHashList: TIntegerList;
    FOnAdding: TNotifyEvent;
    function Get(Index: Integer): TTexture;
    procedure Put(Index: Integer; Item: TTexture);
  public
    property Items[Index: Integer]: TTexture read Get write Put; default;
    property OnAdding: TNotifyEvent read FOnAdding write FonAdding;

    function AddNewTexture(Name:string=''): TTexture;
    function Add(Texture: TTexture): integer;
    function TextureByName(Name: string; Location:string = ''): TTexture;
    function TextureByLocation(Location: string): TTexture;
    function Last: TTexture;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Texture: TTexture);
    procedure Exchange(Index1, Index2: Integer);

    constructor Create;
    destructor Destroy; override;
  end;


  TTextureCombines = (tcDecal, tcModulate, tcBlend, tcReplace, tcAdd);

  TTexture = class(TPersistentResource)
     private
       FTexture: TTextureDecription;
       FSettedParams: TSettedTexParams;
       FTextureMode: TTextureCombines;
       FMinFilter: TMinFilter;
       FMagFilter: TMagFilter;
       FWrapS, FWrapT, FWrapR: TTextureWraps;
       FCompareMode: boolean;
       FName, FLocation: string;
       FInternalName: string;
       FTarget: TTexTarget;
       FOwner: TObject;
       FResourceHandler: TObject;
       FTextureMatrix: TMatrix;
       FTexMatrixChanged: boolean;
       FBlendingMode: TTextureBlendingModes;
       FTwoSides: boolean;
       FDisabled: boolean;
       FMapTargets: TMapTargets;
       FAppledUnit: integer;

       procedure UploadTexture;
       function GetReadPBO: GLUint;
       function GetWritePBO: GLUint;
       procedure SetTextureMode;
       procedure SetMagFilter(const Value: TMagFilter);
       procedure SetMinFilter(const Value: TMinFilter);
       procedure SetWrap_T(const Value: TTextureWraps);
       procedure SetWrap_S(const Value: TTextureWraps);
       procedure SetWrap_R(const Value: TTextureWraps);

       procedure GenMipMaps(const Value: boolean);
       procedure SetBlending;
       procedure ResetBlending;
       procedure SetTextureMatrix(const Value: TMatrix);
       procedure setName(const Value: string);
       function getTexDesc: PTextureDecription;
       procedure CreateCompressedTexture(dds: PDDSImageDesc);
       procedure CreateUnCompressedTexture2D(dds: PDDSImageDesc);
       procedure CreateUnCompressedTextureCube(dds: PDDSImageDesc);
       procedure setCompareMode(const Value: boolean);
//       function CreateCompressedTexture(DDSDesc: PDDSImageDesc): boolean;
     public
       constructor Create;
       function  CreateTexture: boolean;
       constructor CreateFromFile(Filename: string; target: TTexTarget = ttTexture2D);
       constructor CreateFromBitmap(bmp:TBitmap; target: TTexTarget = ttTexture2D);
       destructor Destroy;override;

       property Created: boolean read FTexture.Created;
       property Owner: TObject read FOwner write FOwner;
       property TextureMode: TTextureCombines read FTextureMode write FTextureMode;
       property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
       property BlendingMode: TTextureBlendingModes read FBlendingMode write FBlendingMode;
       property MapTargets: TMapTargets read FMapTargets write FMapTargets;

       procedure ImportTextureParams(aTarget: GLEnum; TexId: GLUInt);
       procedure Assign(Texture: TTexture);
       procedure Apply(TextureUnit: GLEnum = 0; ApplyCombiner: boolean=true);
       procedure UnApply;
       procedure SetPixel(x,y: integer; color: TVector);
       function  ReadPixel(x,y: integer): TVector;


       function  CreateLuminance32FTextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;

       function  CreateRGBA8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateBGRA8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateBGR8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth16FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth24FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateDepth32FTexture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil1Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil4Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil8Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateStencil16Texture2D(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;

       function  CreateBGRA8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateBGR8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGB8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA8TextureRECT(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA16FTextureRECT_NV(Width, Height: integer; Data: pointer=nil; UsePBO: Boolean=false): boolean;
       function  CreateRGBA16FTextureRECT(Width, Height: integer; Data: pointer=nil;UsePBO: Boolean=false): boolean;

       function  CreateCompressedRGBA_S3TC_DXT1(Width, Height: integer; Data: PDDSImageDesc; UsePBO: Boolean=false): boolean;

       function  CreateCubeMapTexture(Width,Height: Integer; Faces: array of pointer): boolean;

       function  CreateRGB8Texture3D(Width,Height,Depth: integer; Data: pointer=nil; UsePBO: boolean = false): boolean;
       function  CreateRGBA8Texture3D(Width,Height,Depth: integer; Data: pointer=nil; UsePBO: boolean = false): boolean;

       procedure UploadData(Data: pointer; UsePBO: boolean=false);
       function  DownloadData(var Datas: pointer; UsePBO: boolean=false): boolean;
       function  ReadFromPBO(pboId: GLUInt): boolean;
       function  WriteToPBO(pboId: GLUInt): boolean;
       function  LoadDataFromFile(Filename: String; var PixelFormat: GLEnum; var Width, Height: integer): pointer;
       procedure LoadFromFile(Filename: String);
       procedure SaveAsTGA(FileName: String);
       procedure SaveAsBMP(FileName: String);

       procedure FreePBO;


       function SetInternalFormat(texFormat: TGLInternalFormat): boolean;
       function GetInternalFormat: TGLinternalFormat;
       function SetOGLTextureFormat(InternalFormat, PixelFormat, Precission: GLEnum): TGLinternalFormat;

       function SetDimensions(width: integer; height: integer = 1; depth: integer=1): boolean;
       function SetWraps(aWrapS, aWrapT: TTextureWraps; aWrapR: TTextureWraps=twClampToEdge): boolean;
       function SetTarget(Target: TTexTarget): boolean;
       function SetFilters(aMinFilter: TMinFilter; aMagFilter: TMagFilter): boolean;
       function SetTexGens(GenS,GenT: TTexGens; GenR: TTexGens = tgDisable): boolean;
       function SetAnisotropyLevel(level: single): boolean;

       property Name: string read FName write setName;
       property FileName: string read FLocation write FLocation;
       property Disabled: boolean read FDisabled write FDisabled;
       property TextureTarget: TTexTarget read FTarget;
       property Width: integer read FTexture.Width;
       property Height: integer read FTexture.Height;
       property Depth: integer read FTexture.Depth;
       property MemSize: integer read FTexture.FullSize;
       property PixelSize: cardinal read FTexture.PixelSize;
       property InternalOGLFormat: GLEnum read FTexture.InternalFormat;
       property PrecisionOGLFormat: GLEnum read FTexture.Precision;
       property PixelOGLFormat: GLEnum read FTexture.ColorChanels;
       property Mipmapping: boolean read FTexture.GenerateMipMaps write GenMipMaps;
       property MinFilter: TMinFilter read FMinFilter write SetMinFilter;
       property MagFilter: TMagFilter read FMagFilter write SetMagFilter;
       property WrapS: TTextureWraps read FWrapS write SetWrap_S;
       property WrapT: TTextureWraps read FWrapT write SetWrap_T;
       property WrapR: TTextureWraps read FWrapR write SetWrap_R;
       property CompareMode: boolean read FCompareMode write setCompareMode;
       property GLTarget: cardinal read FTexture.Target;

       property Handle: GLUint read FTexture.Id;
       property TexDesc: PTextureDecription read getTexDesc;
       property ResourceHandler: TObject read FResourceHandler write FResourceHandler;
       property TwoSides: boolean read FTwoSides write FTwoSides;
       property PBOReadBuffer: GLUint read GetReadPBO;
       property PBOWriteBuffer: GLUint read GetWritePBO;
  end;

(*
  TImgClass = class (TGLBaseImage);

  TTextureResource = record
     Owner: TTexture;
     img: TGLBaseImage;
     imgclass: TClass;
     Filename: string;
     Data: pointer;
     Status: (stWaiting, stLoading, stTransfering, stCompleat)
  end;
  PTextureResource = ^TTextureResource;
  TUpdateType = (utAll, utOneStage, utOneResource, utOneStageOneRes);
  TTextureManager = class (TObject)
     private
       FQueue: TList;
       FUpdateType: TUpdateType;
       FUsePBO: boolean;
     public
       Procedure LoadTexture(Filename: string; Texture: TTexture);overload;
       Procedure LoadTexture(Data: pointer; Texture: TTexture);overload;
       Procedure Update;
  end;
*)
Const
  cTexTargets: array[ttTexture1D..ttCubeMapArray] of GLEnum = (
     GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D,
     GL_TEXTURE_RECTANGLE,
     GL_TEXTURE_RECTANGLE_NV,
     GL_TEXTURE_CUBE_MAP,
     GL_TEXTURE_CUBE_MAP_POSITIVE_X,
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
     GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
     GL_TEXTURE_CUBE_MAP_ARRAY);
   cWpars: array [twClamp..twMirrorRepeat] of GLEnum = (
     GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);
   cMinFilters: array [mnNearest..mnLinearMipmapLinear] of GLEnum = (
     GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
     GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR);

function GetTextureParams(aTarget: GLEnum; TexId: GLUInt): TTextureDecription;
function GetTextureTarget(aTarget: GLEnum): TTexTarget;

implementation

function StringHashKey(const name: string): Integer;
var i, n, res: Integer;
begin
  n := Length(name); Res := n;
  for i := 1 to n do
      Res := (Res shl 1) + Byte(name[i]);
  result:=res;
end;


function GetTextureParams(aTarget: GLEnum; TexId: GLUInt): TTextureDecription;
var temp: integer;
    ifmt:TGLInternalFormat;
begin
  with result do begin
    Target:=aTarget;
    glBindTexture(target,texId);
    glGetTexParameteriv(target, GL_TEXTURE_MAG_FILTER, @minFilter);
    glGetTexParameteriv(target, GL_TEXTURE_MIN_FILTER, @magFilter);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_S, @WrapS);
    glGetTexParameteriv(target, GL_TEXTURE_WRAP_T, @WrapT);
    glGetTexParameteriv(target, GL_GENERATE_MIPMAP, @Temp);
                            GenerateMipMaps:=boolean(Temp);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_WIDTH,@width);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_HEIGHT,@height);
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_DEPTH,@depth);

    glGetTexLevelParameteriv(target, 0, GL_TEXTURE_INTERNAL_FORMAT, @InternalFormat);
      ifmt:=OpenGLFormatToInternalFormat(InternalFormat);
      FindCompatibleDataFormat(ifmt,ColorChanels,Precision);
      PixelSize:=GetTextureElementSize(ifmt);
      FullSize:=Width*Height*Depth*PixelSize;
    glGetTexLevelParameteriv(target,0,GL_TEXTURE_COMPONENTS,@ColorChanels);
  end;
end;

function GetTextureTarget(aTarget: GLEnum): TTexTarget;
var i: TTexTarget;
begin
  for i:=ttTexture1D to ttCubeMapArray do
    if cTexTargets[i]=aTarget then begin
      result:=i; exit;
    end;
  result:=ttTexture1D;
end;

{ TTexture }

constructor TTexture.Create;
begin
   inherited;
   FOwner:=nil; FTwoSides:=false;
   FResourceHandler:=self;
   FTextureMatrix:=IdentityHmgMatrix;
   FTexMatrixChanged:=false;
   FBlendingMode:=tbmMesh;
   FDisabled:=false;
   FCompareMode:=false;
   with FTexture do begin
      Created:=False;
      UsePBO:=false;
      Width:=0; Height:=0;
      FullSize:=-1;
      pboRBId:=0; pboWBId:=0;
      FSettedParams:=[];
      GenerateMipMaps:=false;
      glGenTextures(1,@Id);
      TextureGenS:=0;
      TextureGenT:=0;
      TextureGenR:=0;
   end;
   FTextureMode:=tcModulate;
   FMapTargets:=[mtDiffuse];
   FAppledUnit:=-1;
end;

function TTexture.CreateTexture: boolean;
var allOk: boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  allOk:=(stFormats in FSettedParams) and (stSizes in FSettedParams) and (Width>0);
  assert(allOk,'Not all of Texture Parameters has been setted');
  with FTexture do begin
    if Height<=0 then Height:=1;
    if Depth<=0 then Depth:=1;
    if FullSize<0 then
      FullSize:=width*height*depth*PixelSize;
    if not (stTarget in FSettedParams) then Target:=GL_TEXTURE_2D;
    glBindTexture(Target, Id);
glPixelStorei ( GL_UNPACK_ALIGNMENT,   1 );
glPixelStorei ( GL_UNPACK_ROW_LENGTH,  0 );
glPixelStorei ( GL_UNPACK_SKIP_ROWS,   0 );
glPixelStorei ( GL_UNPACK_SKIP_PIXELS, 0 );
glPixelStorei ( GL_PACK_ALIGNMENT,   1 );
glPixelStorei ( GL_PACK_ROW_LENGTH,  0 );
glPixelStorei ( GL_PACK_SKIP_ROWS,   0 );
glPixelStorei ( GL_PACK_SKIP_PIXELS, 0 );

    if FCompareMode then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);

    if stWraps in FSettedParams then begin
      glTexParameteri(Target, GL_TEXTURE_WRAP_S, WrapS);
      glTexParameteri(Target, GL_TEXTURE_WRAP_T, WrapT);
      glTexParameteri(Target, GL_TEXTURE_WRAP_R, WrapR);
    end;
    if stFilters in FSettedParams then begin
      glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, magFilter);
      glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, minFilter);
    end;
    if (Target = GL_TEXTURE_2D) or (Target = GL_TEXTURE_RECTANGLE) then begin
       glTexParameteri(Target, GL_GENERATE_MIPMAP, byte(GenerateMipMaps));
       glTexImage2D(Target, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
    end;
    if Target=GL_TEXTURE_CUBE_MAP then begin
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
    end;
    if target = GL_TEXTURE_3D then
       glTexImage3D(Target, 0, InternalFormat, WIDTH, HEIGHT, Depth, 0, ColorChanels, Precision, nil);


    if UsePBO then begin
      glGenBuffers(1,@pboRBId);
      glGenBuffers(1,@pboWBId);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
      glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
      glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    end;
    if stData in FSettedParams then UploadTexture;
    glBindTexture(Target, 0);
    Created:=true;
  end;
end;

destructor TTexture.Destroy;
begin
  if FResourceHandler=self then
  with FTexture do begin
     FSettedParams:=[];
     glBindTexture(Target,0);
     glDeleteTextures(1,@Id);
     FreePBO;
  end;
  inherited;
end;

procedure TTexture.Apply(TextureUnit: GLEnum = 0; ApplyCombiner: boolean=true);
var MMode: GLUInt;
begin
  if FDisabled then exit;
  glActiveTexture(GL_TEXTURE0+TextureUnit);
  glEnable(FTexture.Target);

  if FTexture.TextureGenS>0 then begin
    glEnable ( GL_TEXTURE_GEN_S );
    glTexGeni( GL_S, GL_TEXTURE_GEN_MODE, FTexture.TextureGenS );
  end else glDisable( GL_TEXTURE_GEN_S );
  if FTexture.TextureGenT>0 then begin
    glEnable ( GL_TEXTURE_GEN_T );
    glTexGeni( GL_T, GL_TEXTURE_GEN_MODE, FTexture.TextureGenT );
  end else glDisable( GL_TEXTURE_GEN_T );
  if FTexture.TextureGenR>0 then begin
    glEnable ( GL_TEXTURE_GEN_R );
    glTexGeni( GL_R, GL_TEXTURE_GEN_MODE, FTexture.TextureGenR );
  end else glDisable( GL_TEXTURE_GEN_R );

  glBindTexture(FTexture.Target,FTexture.Id);
  FAppledUnit:=TextureUnit;
  if ApplyCombiner then SetTextureMode;
  if FBlendingMode<>tbmMesh then SetBlending;
  if FTexMatrixChanged then begin
    glGetIntegerv(GL_MATRIX_MODE,@MMode);
    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(PGLFloat(@FTextureMatrix));
    glMatrixMode(MMode);
  end else begin
    glGetIntegerv(GL_MATRIX_MODE,@MMode);
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity;
    glMatrixMode(MMode);
  end;
  if FTwoSides then glDisable(GL_CULL_FACE) else glEnable(GL_CULL_FACE);
end;

procedure TTexture.UnApply;
var MMode: GLUInt;
begin
  if FDisabled or (FAppledUnit=-1) then exit;
  glDisable ( GL_TEXTURE_GEN_S );
  glDisable ( GL_TEXTURE_GEN_T );
  glDisable ( GL_TEXTURE_GEN_R );
  glActiveTexture(GL_TEXTURE0+FAppledUnit);
  glBindTexture(FTexture.Target,0);
  glDisable(FTexture.Target);
  if FAppledUnit<>0 then glActiveTexture(GL_TEXTURE0);
{  if FTexMatrixChanged then begin
    glGetIntegerv(GL_MATRIX_MODE,@MMode);
    glMatrixMode(GL_TEXTURE); glPopMatrix; glMatrixMode(MMode);
  end;
}
  glEnable(GL_CULL_FACE);
  if FBlendingMode<>tbmMesh then ResetBlending;
  FAppledUnit:=-1;
end;


function TTexture.GetInternalFormat: TGLinternalFormat;
begin
  result:=OpenGLFormatToInternalFormat(FTexture.InternalFormat);
end;


function TTexture.SetWraps(aWrapS, aWrapT: TTextureWraps; aWrapR: TTextureWraps): boolean;
begin
  if FTexture.Created then begin
    WrapS:=aWrapS; WrapT:=aWrapT; WrapR:=aWrapR; result:=true;
  end else begin
     result:=true;
     FWrapS:=aWrapS; FWrapT:=aWrapT; FWrapR:=aWrapR;
     FTexture.WrapS:=cWpars[aWrapS];
     FTexture.WrapT:=cWpars[aWrapT];
     FTexture.WrapR:=cWpars[aWrapR];
     FSettedParams:=FSettedParams+[stWraps];
  end;
end;

function TTexture.SetDimensions(width, height: integer; depth: integer): boolean;
begin
//  if FTexture.Created then begin result:=false;exit; end else
//  result:=true;
  FTexture.Width:=Width;
  FTexture.Height:=Height;
  FTexture.Depth:=Depth;
  FSettedParams:=FSettedParams+[stSizes];
  exclude(FSettedParams,stData);
  FTexture.Data:=nil;
  if FTexture.Created then begin
    FTexture.Created:=false;
    result:=CreateTexture;
  end;
end;

function TTexture.SetInternalFormat(texFormat: TGLInternalFormat): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  with FTexture do begin
    InternalFormat:=InternalFormatToOpenGLFormat(texFormat);
    FindCompatibleDataFormat(texFormat,ColorChanels,Precision);
    PixelSize:=GetTextureElementSize(texFormat);
    FullSize:=Width*Height*Depth*PixelSize;
    FSettedParams:=FSettedParams+[stFormats];
  end;
end;

function TTexture.SetTarget(Target: TTexTarget): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  if IsTargetSupported(cTexTargets[Target]) then
  FTexture.Target:=cTexTargets[Target];
  FSettedParams:=FSettedParams+[stTarget];
  FTarget:=Target;
end;

function TTexture.SetFilters(aMinFilter: TMinFilter;
  aMagFilter: TMagFilter): boolean;
var oldmmp: boolean;
begin
  if FTexture.Created then begin
    oldmmp:=FTexture.GenerateMipMaps;
    MagFilter:=aMagFilter;
    MinFilter:=aMinFilter;
    if aMagFilter = mgNearest then
         FTexture.magFilter:=GL_NEAREST
    else FTexture.magFilter:=GL_LINEAR;
    FTexture.minFilter:=cMinFilters[aMinFilter];
    if aMinFilter<=mnLinear then FTexture.GenerateMipMaps:=false
    else FTexture.GenerateMipMaps:=true;
    if oldmmp<>FTexture.GenerateMipMaps then GenMipMaps(FTexture.GenerateMipMaps);

  end else begin
    FMinFilter:=aMinFilter;
    FMagFilter:=aMagFilter;
    if aMagFilter = mgNearest then
         FTexture.magFilter:=GL_NEAREST
    else FTexture.magFilter:=GL_LINEAR;
    FTexture.minFilter:=cMinFilters[aMinFilter];
    if aMinFilter<=mnLinear then FTexture.GenerateMipMaps:=false
    else FTexture.GenerateMipMaps:=true;
    FSettedParams:=FSettedParams+[stFilters];
    GenMipMaps(FTexture.GenerateMipMaps);
  end;
  result:=true;
end;

procedure TTexture.UpLoadData(Data: pointer; UsePBO: boolean);
begin
  assert(FTexture.Target<>GL_TEXTURE_CUBE_MAP, 'Not working with Cubemap textures');
  FTexture.Data:=Data;
  FTexture.UsePBO:=UsePBO;
  if Data=nil then exit;
  if FTexture.Created then begin
     glBindTexture(FTexture.Target,Ftexture.Id);
     UploadTexture;
     glBindTexture(FTexture.Target,0);
  end;
  FSettedParams:=FSettedParams+[stData];
end;

procedure TTexture.UploadTexture;
var t:pointer;
begin
if not FTexture.Created then exit;
assert(FTexture.Target<>GL_TEXTURE_CUBE_MAP, 'Not working with Cubemap textures');
with FTexture do begin
  if UsePBO then begin
     if pboWBId=0 then begin
       glGenBuffers(1,@pboWBId);
       glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
       glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
       glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
     end;
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
     t := glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
//     CopyMemory(t,data,FullSize);
     Move(data^,t^,FullSize);
     glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
  end;
  case Target of
     GL_TEXTURE_1D:
        if UsePBO then glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,nil)
        else glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,Data);
     GL_TEXTURE_2D,GL_TEXTURE_RECTANGLE,
     GL_TEXTURE_CUBE_MAP..GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
        if UsePBO then
             glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,nil)
        else glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,Data);
     GL_TEXTURE_3D:
        if UsePBO then glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,nil)
        else glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,Data);
     Else assert(false,'Unsupported uploading target or method');
  end;

  if UsePBO then glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  if GenerateMipMaps then begin
    glBindTexture(Target,Id);
    glGenerateMipmapEXT(Target);
    glBindTexture(Target,0);
  end;
{  if GenerateMipMaps then glTexParameteri(Target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE)
  else glTexParameteri(Target, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
}
end;
end;

function TTexture.DownloadData(var Datas: pointer; UsePBO: boolean): boolean;
var t:pointer;
begin
  if not FTexture.Created then begin result:=false;exit;
  end else result:=true;
  assert(FTexture.Target<>GL_TEXTURE_CUBE_MAP, 'Not working with Cubemap textures');
  ReallocMem(Datas,FTexture.FullSize);
  FTexture.Data:=datas;
  FTexture.UsePBO:=UsePBO;
  with FTexture do begin
     glEnable(Target); glBindTexture(Target, Id);
     if not UsePBO then glGetTexImage(Target, 0, ColorChanels,Precision, Datas)
     else begin
        if pboRBId=0 then begin
          glGenBuffers(1,@pboRBId);
          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
//          glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
        end;
        glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
        glGetTexImage(Target,0,ColorChanels,Precision, nil);
//        glGetTexImage(Target,0,GL_BGRA,Precision, nil);
        t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
        CopyMemory(Datas,t,FullSize);
        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
     end;
     glBindTexture(Target, 0); glDisable(Target);
  end;
end;

function TTexture.CreateRGBA8Texture2D(Width, Height: integer; Data: pointer; UsePBO:Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfRGBA8);
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGBA8Texture3D(Width, Height, Depth: integer;
  Data: pointer; UsePBO: boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetOGLTextureFormat(GL_RGBA8,GL_RGBA,GL_UNSIGNED_BYTE);
  FTexture.PixelSize:=4;

  SetDimensions(width, height, depth);
  SetWraps(twClampToEdge,twClampToEdge, twClampToEdge);
  SetTarget(ttTexture3D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGB8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_RGB8,GL_RGB,GL_UNSIGNED_BYTE);
  FTexture.PixelSize:=3;
  SetDimensions(width, height);
//  SetInternalFormat(tfRGB8);
  SetTarget(ttTexture2D);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TTexture.CreateRGB8Texture3D(Width, Height, Depth: integer;
  Data: pointer; UsePBO: boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetOGLTextureFormat(GL_RGB8,GL_RGB,GL_UNSIGNED_BYTE);
  FTexture.PixelSize:=3;

  SetDimensions(width, height, depth);
  SetWraps(twClampToEdge,twClampToEdge, twClampToEdge);
  SetTarget(ttTexture3D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGB16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGB16);
  SetOGLTextureFormat(GL_RGB16F_ARB,GL_RGB,GL_HALF_FLOAT);
  FTexture.PixelSize:=6;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  SetTarget(ttTexture2D);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TTexture.CreateRGB32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGB32);
  SetOGLTextureFormat(GL_RGB32F_ARB,GL_RGB,GL_FLOAT);
  FTexture.PixelSize:=12;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGBA16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGBA16);
  SetOGLTextureFormat(GL_RGBA16F_ARB,GL_RGBA,GL_HALF_FLOAT);
  FTexture.PixelSize:=8;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);

end;

function TTexture.CreateRGBA32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

//  SetInternalFormat(tfFLOAT_RGBA32);
  SetOGLTextureFormat(GL_RGBA32F_ARB,GL_RGBA,GL_FLOAT);
  FTexture.PixelSize:=16;

  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TTexture.CreateDepth16FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT16,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);

  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TTexture.CreateDepth24FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT24,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

function TTexture.CreateDepth32FTexture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_DEPTH_COMPONENT32,GL_DEPTH_COMPONENT,GL_FLOAT);
  FTexture.PixelSize:=4;
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);

end;

procedure TTexture.CreateCompressedTexture(dds: PDDSImageDesc);
var i: integer;
    p: pointer;
begin
  glBindTexture(GL_TEXTURE_2D, FTexture.Id);

  FTexture.Width:=dds.Width;
  FTexture.Height:=dds.Height;
  FTexture.Depth:=1;
  FSettedParams:=FSettedParams+[stSizes];

  p:=dds.Data; FTexture.Data:=dds.Data;
  //glTexParameteri(TEXTURE_2D, TEXTURE_MAX_LEVEL, level-1); //TEXTURE_BASE_LEVEL
  for i:=0 to 0{dds.Levels-1} do begin
    p:=pointer(integer(dds.Data)+dds.LODS[i].Offset);
    glCompressedTexImage2DARB(GL_TEXTURE_2D, i, dds.InternalFormat, dds.LODS[i].Width,
      dds.LODS[i].Height, 0, dds.LODS[i].Size, p);
  end;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  FMinFilter := mnLinearMipmapLinear;
  FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;
  glGenerateMipmap(GL_TEXTURE_2D);
{
  if dds.Levels>1 then begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    FMinFilter := mnLinearMipmapLinear;
    FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;
  end else begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    FMinFilter := mnLinear;
    FTexture.minFilter:=GL_LINEAR;
  end;
}
  FMagFilter := mgLinear;
  FTexture.magFilter:=GL_LINEAR;
  glTexParameteri(FTexture.Target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  SetWraps(twRepeat,twRepeat);
  glBindTexture(GL_TEXTURE_2D, 0);
  FTexture.Created:=true;
end;

procedure TTexture.CreateUnCompressedTexture2D(dds: PDDSImageDesc);
var i: integer;
    p: pointer;
begin
  glBindTexture(GL_TEXTURE_2D, FTexture.Id);

  FTexture.Width:=dds.Width;
  FTexture.Height:=dds.Height;
  FTexture.Depth:=1;
  FSettedParams:=FSettedParams+[stSizes];

  p:=dds.Data; FTexture.Data:=dds.Data;
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, dds.Levels-1); //TEXTURE_BASE_LEVEL
  for i:=0 to dds.Levels-1 do begin
    p:=pointer(integer(dds.Data)+dds.LODS[i].Offset);
    glTexImage2D(GL_TEXTURE_2D, i, dds.InternalFormat, dds.LODS[i].Width,
      dds.LODS[i].Height, 0, dds.ColorFormat, dds.DataType, p);
  end;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  FMinFilter := mnLinearMipmapLinear;
  FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;

  glGenerateMipmap(GL_TEXTURE_2D);

{  if dds.Levels>1 then begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    FMinFilter := mnLinearMipmapLinear;
    FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;
  end else begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    FMinFilter := mnLinear;
    FTexture.minFilter:=GL_LINEAR;
  end;
}
  FMagFilter := mgLinear;
  FTexture.magFilter:=GL_LINEAR;
  glTexParameteri(FTexture.Target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  SetWraps(twRepeat,twRepeat);
  glBindTexture(GL_TEXTURE_2D, 0);
  FTexture.Created:=true;
end;

procedure TTexture.CreateUnCompressedTextureCube(dds: PDDSImageDesc);
var i,f: integer;
    p: pointer;
    offs: integer;
const
CubeMapTarget: array[0..5] of cardinal = (

  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB,
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB,

  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB,
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB,

  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB,
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB

  );

begin
  glBindTexture(GL_TEXTURE_CUBE_MAP, FTexture.Id);

  FTexture.Width:=dds.Width;
  FTexture.Height:=dds.Height;
  FTexture.Depth:=1;
  FSettedParams:=FSettedParams+[stSizes];

  p:=dds.Data; offs:=0; FTexture.Data:=dds.Data;
  for f:=0 to 5 do begin
    //glTexParameteri(GL_TEXTURE_CUBE_MAP_POSITIVE_X+f, GL_TEXTURE_MAX_LEVEL, 1); //TEXTURE_BASE_LEVEL
    for i:=0 to 0{dds.Levels-1} do begin
      p:=pointer(integer(dds.Data)+dds.LODS[i].Offset+offs);
      glTexImage2D(CubeMapTarget[f], i, dds.InternalFormat, dds.LODS[i].Width,
        dds.LODS[i].Height, 0, dds.ColorFormat, dds.DataType, p);
    end; offs:=offs+dds.DataSize;
  end;
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  FMinFilter := mnLinearMipmapLinear;
  FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;

  glGenerateMipmap(GL_TEXTURE_CUBE_MAP);
{  if dds.Levels>1 then begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    FMinFilter := mnLinearMipmapLinear;
    FTexture.minFilter:=GL_LINEAR_MIPMAP_LINEAR;
  end else begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    FMinFilter := mnLinear;
    FTexture.minFilter:=GL_LINEAR;
  end;
}
  FMagFilter := mgLinear;
  FTexture.magFilter:=GL_LINEAR;
  glTexParameteri(FTexture.Target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
//  SetWraps(twRepeat,twRepeat);
    SetWraps(twClampToEdge,twClampToEdge);
  glBindTexture(GL_TEXTURE_CUBE_MAP, 0);
  FTexture.Created:=true;
end;

Constructor  TTexture.CreateFromFile(Filename: string; target: TTexTarget);
var format: GLEnum;
    w,h: integer;
    _data: pointer;
    ext: string;
    dds: PDDSImageDesc;
    fs: TFileStream;
begin
   inherited Create;
   FTextureMatrix:=IdentityHmgMatrix;
   FTexMatrixChanged:=false; FTwoSides:=false;
   FBlendingMode:=tbmMesh;
   FTextureMode:=tcModulate;
   FDisabled:=false;
   FOwner:=nil; FResourceHandler:=self;
   with FTexture do begin
      Created:=False;
      UsePBO:=false;
      FullSize:=-1;
      Width:=0; Height:=0;
      pboRBId:=0; pboWBId:=0;
      FSettedParams:=[];
      GenerateMipMaps:=false;
      glGenTextures(1,@Id);
      TextureGenS:=0;
      TextureGenT:=0;
      TextureGenR:=0;
   end;
   ext:=uppercase(ExtractFileExt(FileName));
   if ext='.DDS' then begin
     fs:=TFileStream.Create(FileName, fmOpenRead);
     new(dds); dds:=DDSLoadFromStream(fs); fs.Free;
     if not dds.CubeMap then begin
       SetTarget(ttTexture2D);
       FTexture.Created:=true;
       if dds.Compressed then CreateCompressedTexture(dds)
       else CreateUnCompressedTexture2D(dds);
     end else begin
       SetTarget(ttCubemap);
       FTexture.Created:=true;
       CreateUnCompressedTextureCube(dds);
     end;
     dispose(dds.Data); Dispose(dds);
     FLocation:=FileName; exit;
   end;
   //Data:=LoadDataFromFile(Filename,format, w,h);
   SetTarget(target);
   with FTexture do begin
     _Data:=LoadTexture(Filename,InternalFormat,ColorChanels,Precision,PixelSize,w,h);
     assert(assigned(_Data), 'Unsupported File Format: '+extractfilename(FileName));
     FLocation:=FileName;
     SetOGLTextureFormat(InternalFormat,ColorChanels,Precision);
     SetDimensions(w, h);
     if not (stFilters in FSettedParams) then begin
       if Precision=GL_UNSIGNED_BYTE then SetFilters(mnLinearMipmapLinear, mgLinear)
       else SetFilters(mnNearest, mgNearest);
     end;
     CreateTexture;
     UploadData(_Data,UsePBO);
     freemem(_Data);
   end;
end;

function TTexture.CreateLuminance32FTextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfLUMINANCE_FLOAT32);
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  if not (stFilters in FSettedParams) then
    SetFilters(mnNearest, mgNearest);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateStencil1Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_STENCIL_INDEX1,GL_STENCIL_INDEX,GL_UNSIGNED_BYTE);
  SetDimensions(width, height);
  SetWraps(twClampToEdge,twClampToEdge);
  FTexture.GenerateMipMaps:=false;
  if not (stFilters in FSettedParams) then
    SetFilters(mnNearest, mgNearest);
  SetTarget(ttTexture2D);

  CreateTexture;
  if data<>nil then UploadData(Data,UsePBO);
end;

function TTexture.CreateStencil4Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  assert(false,'Function under construction!');
end;

function TTexture.CreateStencil8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  assert(false,'Function under construction!');
end;

function TTexture.CreateStencil16Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  assert(false,'Function under construction!');
end;

procedure TTexture.FreePBO;
begin
  with FTexture do begin
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER,0);
     glBindBuffer(GL_PIXEL_PACK_BUFFER,0);
     if pboRBId<>0 then glDeleteBuffers(1,@pboRBId);
     if pboWBId<>0 then glDeleteBuffers(1,@pboWBId);
     pboRBId:=0; pboWBId:=0;
  end;
end;

function TTexture.CreateBGRA8Texture2D(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_BGRA;
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateBGRA8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_BGRA;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateCompressedRGBA_S3TC_DXT1(Width, Height: integer;
  Data: PDDSImageDesc; UsePBO: Boolean): boolean;
begin
  assert(false,'Compressed texture in under construction');
  assert(assigned(Data),'PDDSImageDesc is not assigned');
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetInternalFormat(tfCOMPRESSED_RGBA_S3TC_DXT1);
  SetDimensions(width, height);
  SetTarget(ttTexture2D);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateCompressedTexture(Data);
//  UploadData(Data,UsePBO);

end;
(*
function TTexture.CreateCompressedTexture(DDSDesc: PDDSImageDesc): boolean;
var allOk: boolean;
    i: integer;
begin
  if FTexture.Created then begin result:=false;exit; end else result:=true;
//  allOk:=(stFormats in FSettedParams) and (stSizes in FSettedParams) and (Width>0);
//  assert(allOk,'Not all of Texture Parameters has been setted');

{
    Format: Cardinal;
    InternalFormat: cardinal;
    ColorFormat: cardinal;
    DataType: cardinal;
    ElementSize: integer;
    DataSize: integer;
    Data: pointer;
    Width, Height, Depth, Levels: integer;
    LODS: array[0..15] of TImageLevelDesc;
    Compressed: boolean;
    CubeMap: boolean;
    TextureArray: boolean;
}
  with FTexture do begin
    Width:= DDSDesc.Width;  if Width<0 then Width:=1;
    Height:=DDSDesc.Height; if Height<=0 then Height:=1;
    Depth:=DDSDesc.Depth; if Depth<=0 then Depth:=1;
    FullSize:=DDSDesc.DataSize;
    PixelSize:=DDSDesc.ElementSize;
    if FullSize<0 then FullSize:=width*height*depth*PixelSize;
    if not (stTarget in FSettedParams) then Target:=GL_TEXTURE_2D;
    glBindTexture(Target, Id);
    if stWraps in FSettedParams then begin
      glTexParameteri(Target, GL_TEXTURE_WRAP_S, WrapS);
      glTexParameteri(Target, GL_TEXTURE_WRAP_T, WrapT);
      glTexParameteri(Target, GL_TEXTURE_WRAP_R, WrapR);
    end;
    if stFilters in FSettedParams then begin
      glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, magFilter);
      glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, minFilter);
    end;
    if Target = GL_TEXTURE_2D then begin
       for i:=0 to DDSDesc.Levels-1 do begin
         if DDSDesc.Compressed then
           glCompressedTexImage2DARB(Target, i, DDSDesc.ColorFormat, DDSDesc.LODS[i].Width,
             DDSDesc.LODS[i].Height,0,DDSDesc.LODS[i].Size,
             pointer(DDSDesc.LODS[i].Offset+cardinal(DDSDesc.Data)))
         else
           glTexImage2D(Target, i, DDSDesc.InternalFormat, DDSDesc.WIDTH, DDSDesc.HEIGHT, 0,
             DDSDesc.ColorFormat, DDSDesc.DataType, pointer(DDSDesc.LODS[i].Offset+cardinal(DDSDesc.Data)));
       end;
    end;
    if Target=GL_TEXTURE_CUBE_MAP then begin
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, InternalFormat, WIDTH, HEIGHT, 0, ColorChanels, Precision, nil);
    end;
    if target = GL_TEXTURE_3D then
       glTexImage3D(Target, 0, InternalFormat, WIDTH, HEIGHT, Depth, 0, ColorChanels, Precision, nil);


    if UsePBO then begin
      glGenBuffers(1,@pboRBId);
      glGenBuffers(1,@pboWBId);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
      glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
      glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    end;
//    if stData in FSettedParams then UploadTexture;
    glBindTexture(Target, 0);
    Created:=true;
  end;

end;
*)
function TTexture.CreateCubeMapTexture(Width, Height: Integer;
  Faces: array of pointer): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  assert(Length(Faces)=6,'Define all 6 faces!');
  SetInternalFormat(tfRGB8);
  FTexture.ColorChanels:=GL_RGB;
  SetDimensions(width, height);
  SetTarget(ttCubemap);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=false;
  CreateTexture;

  glBindTexture(GL_TEXTURE_CUBE_MAP, FTexture.Id);
  //Define all 6 faces
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[0]);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[1]);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[2]);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[3]);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[4]);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, GL_RGB8, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Faces[5]);
  glBindTexture(GL_TEXTURE_CUBE_MAP, 0);

end;

function TTexture.CreateRGBA8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGBA;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGB8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGB;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateBGR8Texture2D(Width, Height: integer; Data: pointer;
  UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;

  SetOGLTextureFormat(GL_RGB8,GL_BGR,GL_UNSIGNED_BYTE);
  FTexture.PixelSize:=3;
  SetDimensions(width, height);
//  SetInternalFormat(tfRGB8);
  SetTarget(ttTexture2D);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateBGR8TextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetInternalFormat(tfRGBA8);
  FTexture.ColorChanels:=GL_RGB;
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;


function TTexture.ReadFromPBO(pboId: GLUInt): boolean;
begin
 if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
 end else result:=true;
 with FTexture do begin
  glEnable(Target); glBindTexture(Target, Id);
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboId);
  case Target of
     GL_TEXTURE_1D: glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,nil);
     GL_TEXTURE_2D,GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP..GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
           glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,nil);
     GL_TEXTURE_3D: glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,nil);
     Else assert(false,'Unsupported uploading target or method');
  end;
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  if GenerateMipMaps then glGenerateMipmapEXT(Target);
 end;
end;

function TTexture.WriteToPBO(pboId: GLUInt): boolean;
begin
  if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
  end else result:=true;
  with FTexture do begin
    glEnable(Target); glBindTexture(Target, Id);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, pboId);
    glGetTexImage(Target,0,ColorChanels,Precision, nil);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
    glBindTexture(Target, 0); glDisable(Target);
  end;
end;

function TTexture.ReadPixel(x, y: integer): TVector;
var t:pointer;
    i,offs: integer;
begin
  if not FTexture.Created then exit;
  with FTexture do begin
     glEnable(Target); glBindTexture(Target, Id);
     if pboRBId=0 then begin
          glGenBuffers(1,@pboRBId);
          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
     end else glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
     glGetTexImage(Target,0,ColorChanels,Precision, nil);
     t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
     offs:=(y*Width+x)*PixelSize;
     for i:=0 to PixelSize-1 do begin
       case Precision of
         GL_UNSIGNED_BYTE: begin
                result[i]:=PByteArray(t)[offs+i];
              end;
         GL_FLOAT: begin
                result[i]:=PSingleArray(t)[offs+i];
              end;
       end;
     end;
     glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
     glBindTexture(Target, 0); glDisable(Target);
  end;
end;

procedure TTexture.SetPixel(x, y: integer; color: TVector);
var t:pointer;
    i,offs: integer;
begin
  if (not FTexture.Created) then exit;
  with FTexture do begin
    glEnable(Target); glBindTexture(Target, Id);
    if pboWBId=0 then begin
         glGenBuffers(1,@pboWBId);
         glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
         glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
    end else glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);

    t := glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
    offs:=(y*(Width-1)+x)*PixelSize;
    for i:=0 to PixelSize-1 do begin
      case Precision of
         GL_UNSIGNED_BYTE:     PByteArray(t)[offs+i]:=trunc(color[i]);
         GL_FLOAT:             PSingleArray(t)[offs+i]:=color[i];
      end;
    end;
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glTexSubImage2D(Target,0,x,y,1,1,ColorChanels,Precision,pointer(offs));
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glBindTexture(Target, 0); glDisable(Target);
  end;
end;


procedure TTexture.ImportTextureParams(aTarget: GLEnum; TexId: GLUInt);
var temp: integer;
    ifmt:TGLInternalFormat;
begin

  if FTexture.Created then begin
    if FTexture.Target>0 then begin
      glEnable(FTexture.Target);
      glBindTexture(FTexture.Target,0);
    end;
    if FTexture.id>0 then begin
      glDeleteTextures(1,@FTexture.id);
      FTexture.id:=0;
    end;
  end;

  SetTarget(GetTextureTarget(aTarget));
//  FTexture.Target:=aTarget;

  glEnable(FTexture.Target);
  with FTexture do begin
    glBindTexture(atarget,texId);

    glGetTexParameteriv(atarget, GL_TEXTURE_MAG_FILTER, @minFilter);
    glGetTexParameteriv(atarget, GL_TEXTURE_MIN_FILTER, @magFilter);
    glGetTexParameteriv(atarget, GL_TEXTURE_WRAP_S, @WrapS);
    glGetTexParameteriv(atarget, GL_TEXTURE_WRAP_T, @WrapT);
    glGetTexParameteriv(atarget, GL_GENERATE_MIPMAP, @Temp);
    GenerateMipMaps:=boolean(Temp);
    glGetTexLevelParameteriv(atarget,0,GL_TEXTURE_WIDTH,@width);
    glGetTexLevelParameteriv(atarget,0,GL_TEXTURE_HEIGHT,@height);
    glGetTexLevelParameteriv(atarget,0,GL_TEXTURE_DEPTH,@depth);
    glGetTexLevelParameteriv(atarget,0, GL_TEXTURE_INTERNAL_FORMAT, @InternalFormat);
    glGetTexLevelParameteriv(atarget,0,GL_TEXTURE_COMPONENTS,@ColorChanels);

    ifmt:=OpenGLFormatToInternalFormat(InternalFormat);
    FindCompatibleDataFormat(ifmt,ColorChanels,Precision);
    PixelSize:=GetTextureElementSize(ifmt);
    FullSize:=Width*Height*Depth*PixelSize;

    Data:=nil; Id:=TexId;
    FreePBO;UsePBO:=false;
    glBindTexture(atarget,0);
    Created:=true;
  end;
end;

function TTexture.LoadDataFromFile(Filename: String; var PixelFormat: GLEnum; var Width, Height: integer): pointer;
begin
  result:=LoadTexture(Filename, PixelFormat, Width, Height);
  assert(assigned(result),'Unsupported File Format: '+extractfilename(FileName));
  FLocation:=FileName;
end;

function TTexture.SetOGLTextureFormat(InternalFormat, PixelFormat,
  Precission: GLEnum): TGLinternalFormat;
begin
  FTexture.InternalFormat:=InternalFormat;
  FTexture.ColorChanels:=PixelFormat;
  FTexture.Precision:=Precission;
  FSettedParams:=FSettedParams+[stFormats];
  result:=OpenGLFormatToInternalFormat(InternalFormat);
end;

function TTexture.GetReadPBO: GLUint;
begin
with FTexture do begin
  if pboRBId=0 then begin
     glGenBuffers(1,@pboRBId);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
     glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
  end;
  result:=pboRBId;
end;
end;

function TTexture.getTexDesc: PTextureDecription;
begin
  result:=@FTexture;
end;

function TTexture.GetWritePBO: GLUint;
begin
with FTexture do begin
  if pboWBId=0 then begin
     glGenBuffers(1,@pboWBId);
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
     glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  end;
  result:=pboWBId;
end;
end;


(*
{ TTextureManager }

procedure TTextureManager.LoadTexture(Filename: string; Texture: TTexture);
var ext:string;
    res:PTextureResource;
begin
   new(res); res.Filename:=Filename;
   with res^ do begin
     ext:=lowercase(ExtractFileExt(filename)); delete(ext,1,1);
     if ext='bmp' then begin img:=TGLBitmap32.Create; imgclass:=TGLBitmap32;end;
     if (ext='jpg') or (ext='jpeg') then begin
        img:=TGLJPEGImage.Create; imgclass:=TGLJPEGImage;end;
     if ext='png' then begin img:=TGLPNGImage.Create; imgclass:=TGLPNGImage;end;
     if ext='dds' then begin img:=TGLDDSImage.Create; imgclass:=TGLDDSImage;end;
     owner:=Texture; Data:=img.Data;
     Status:=stLoading;
   end; FQueue.Add(res);
end;

procedure TTextureManager.LoadTexture(Data: pointer; Texture: TTexture);
var res:PTextureResource;
begin
   new(res); res.Data:=Data;
   with res^ do begin
     Filename:=''; owner:=Texture;
     Status:=stTransfering; img:=nil;
   end; FQueue.Add(res);
end;

procedure TTextureManager.Update;
  procedure PackList;
  var i:integer;
  begin
     i:=0;
     while i<FQueue.Count do begin
        if FQueue[i]=nil then FQueue.Delete(i)
        else i:=i+1;
     end;
  end;
  procedure GetImageParam (res: PTextureResource);
    begin
     with res^ do begin
        Owner.SetDimensions(TImgClass(img).fWidth,TImgClass(img).fheight);
        Owner.SetTarget(ttTexture2D);
        Owner.FLocation:=Filename;
        Owner.SetInternalFormat(TImgClass(img).fInternalFormat);
     end;
  end;
var res: PTextureResource;
    i:integer;
begin
   case FUpdateType of
     utAll: begin
       for i:=0 to FQueue.Count-1 do begin
          res:=FQueue[i];
          case res.Status of
             stLoading: begin
                  res.img.LoadFromFile(res.filename);
                  res.Data:=res.img.Data;
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.img.Free;
             end;
             stTransfering: begin
                  res.Owner.UploadData(res.Data,FUsePBO);
             end;
          end; dispose(res);
       end; FQueue.Clear;
     end;
     utOneStage: begin
       for i:=0 to FQueue.Count-1 do begin
          res:=FQueue[i];
          case res.Status of
             stLoading: begin
                  res.img.LoadFromFile(res.filename);
                  res.Data:=res.img.Data;
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.img.Free; res.Status:=stTransfering;
             end;
             stTransfering: begin
                  res.Owner.UploadData(res.Data,FUsePBO);
                  res.Status:=stCompleat;
             end;
          end;
          if res.Status=stCompleat then begin
            dispose(res); FQueue[i]:=nil; end;
       end; PackList;
     end;
     utOneResource: begin
        if FQueue.Count=0 then exit;
        res:=FQueue[0];
        case res.Status of
           stLoading: begin
                res.img.LoadFromFile(res.filename);
                res.Data:=res.img.Data;
                res.Owner.UploadData(res.Data,FUsePBO);
                res.img.Free;
           end;
           stTransfering: begin
                res.Owner.UploadData(res.Data,FUsePBO);
           end;
        end; dispose(res); FQueue.Delete(0);
     end;
     utOneStageOneRes: begin
        if FQueue.Count=0 then exit;
        res:=FQueue[0];
        case res.Status of
           stLoading: begin
                res.img.LoadFromFile(res.filename);
                res.Data:=res.img.Data;
                res.Owner.UploadData(res.Data,FUsePBO);
                res.img.Free; res.Status:=stTransfering;
           end;
           stTransfering: begin
                res.Owner.UploadData(res.Data,FUsePBO);
                res.Status:=stCompleat;
           end;
        end;
        if res.Status=stCompleat then begin
           dispose(res); FQueue.Delete(0);
        end;
     end;
   end;
end;
*)

procedure TTexture.Assign(Texture: TTexture);
begin
   FTexture:=Texture.FTexture;
   FSettedParams:=Texture.FSettedParams;
   FName:=Texture.FName;
   FLocation:=Texture.FLocation;
   FTarget:=Texture.FTarget;
end;

function TTexture.SetTexGens(GenS, GenT, GenR: TTexGens): boolean;
const
  cTG:array[tgDisable..tgReflectionMap] of GLEnum =
  (0, GL_OBJECT_LINEAR, GL_EYE_LINEAR, GL_SPHERE_MAP, GL_NORMAL_MAP, GL_REFLECTION_MAP);
begin
  FTexture.TextureGenS:=cTG[GenS];
  FTexture.TextureGenT:=cTG[GenT];
  FTexture.TextureGenR:=cTG[GenR];
  result:=true;
end;

procedure TTexture.SetTextureMatrix(const Value: TMatrix);
begin
  FTextureMatrix := Value;
  FTexMatrixChanged:=true;
end;

procedure TTexture.SetTextureMode;
const
  cTextureMode: array[tcDecal..tcAdd] of TGLEnum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);
begin
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
end;

constructor TTexture.CreateFromBitmap(bmp: TBitmap; target: TTexTarget);
var i,j,w,h, offs: integer;
    p: PByteArray;
    data: array of byte;
    bpp: byte;
begin
   inherited Create;
   FOwner:=nil; FResourceHandler:=self;
   FTextureMatrix:=IdentityHmgMatrix;
   FTexMatrixChanged:=false; FTwoSides:=false;
   FBlendingMode:=tbmMesh; FDisabled:=false;
   w:=bmp.Width; h:=bmp.Height;
   with FTexture do begin
      Created:=False;
      UsePBO:=false;
      Width:=w; Height:=h;
      pboRBId:=0; pboWBId:=0;
      FSettedParams:=[];
      GenerateMipMaps:=true;
      glGenTextures(1,@Id);
      TextureGenS:=0;
      TextureGenT:=0;
      TextureGenR:=0;
   end;
   case bmp.PixelFormat of
     pf8bit: bpp:=1;
     pf16bit: bpp:=2;
     pf24bit: bpp:=3;
     pf32bit: bpp:=4;
     else bpp:=1;
   end;
   setlength(data,w*h*3);
   for i:=h-1 downto 0 do begin
     p:=bmp.ScanLine[i]; offs:=i*w*3;
     for j:=0 to w-1 do begin
        data[offs+j*3]:=p[j*bpp+2];
        data[offs+j*3+1]:=p[j*bpp+1];
        data[offs+j*3+2]:=p[j*bpp];
     end;
   end;
   SetFilters(mnLinearMipmapLinear, mgLinear);
   if target = ttTexture2D then CreateRGB8Texture2D(w,h,data);
   if target = ttTextureRectangle then CreateRGB8TextureRECT(w,h,data);
   FLocation:='TBitmap'+inttostr(integer(bmp));
end;

procedure TTexture.SetMagFilter(const Value: TMagFilter);
begin
  FMagFilter := Value;
  if FMagFilter = mgNearest then
       FTexture.magFilter:=GL_NEAREST
  else FTexture.magFilter:=GL_LINEAR;
  if FTexture.Target=0 then exit;
  glBindTexture(FTexture.Target, FTexture.ID);
  glTexParameteri(FTexture.Target, GL_TEXTURE_MAG_FILTER, FTexture.magFilter);
  glBindTexture(FTexture.Target, 0);
end;

procedure TTexture.SetMinFilter(const Value: TMinFilter);
begin
  FMinFilter := Value;
  FTexture.minFilter:=cMinFilters[FMinFilter];
  if FTexture.Target=0 then exit;
  glBindTexture(FTexture.Target, FTexture.ID);
  glTexParameteri(FTexture.Target, GL_TEXTURE_MIN_FILTER, FTexture.minFilter);
  if MinFilter<=mnLinear then Mipmapping:=false
  else Mipmapping:=true;

  glBindTexture(FTexture.Target, 0);
end;

procedure TTexture.setName(const Value: string);
begin
  FName := Value; FInternalName:=Value+'_'+FLocation;
end;

procedure TTexture.SetWrap_R(const Value: TTextureWraps);
begin
  FWrapS := Value;
  FTexture.WrapR:=cWpars[Value];
  if FTexture.Target=0 then exit;
  glBindTexture(FTexture.Target, FTexture.ID);
  glTexParameteri(FTexture.Target, GL_TEXTURE_WRAP_R, FTexture.WrapR);
  glBindTexture(FTexture.Target, 0);
end;

procedure TTexture.SetWrap_S(const Value: TTextureWraps);
begin
  FWrapS := Value;
  FTexture.WrapS:=cWpars[Value];
  if FTexture.Target=0 then exit;
  glBindTexture(FTexture.Target, FTexture.ID);
  glTexParameteri(FTexture.Target, GL_TEXTURE_WRAP_S, FTexture.WrapS);
  glBindTexture(FTexture.Target, 0);
end;

procedure TTexture.SetWrap_T(const Value: TTextureWraps);
begin
  FWrapT := Value;
  FTexture.WrapT:=cWpars[Value];
  if FTexture.Target=0 then exit;
  glBindTexture(FTexture.Target, FTexture.ID);
  glTexParameteri(FTexture.Target, GL_TEXTURE_WRAP_T, FTexture.WrapT);
  glBindTexture(FTexture.Target, 0);
end;

procedure TTexture.GenMipMaps(const Value: boolean);
begin
  FTexture.GenerateMipMaps := Value;
  if FTexture.Target=0 then exit;
  with FTexture do begin
    glBindTexture(Target, ID);
    if FTexture.GenerateMipMaps then glGenerateMipmapEXT(FTexture.Target);
    glBindTexture(Target, 0);
  end;
end;

function TTexture.CreateRGBA16FTextureRECT_NV(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetOGLTextureFormat(GL_FLOAT_RGBA16_NV,GL_RGBA,GL_HALF_FLOAT);
  SetDimensions(width, height);
  SetTarget(ttTextureRectangleNV);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

function TTexture.CreateRGBA16FTextureRECT(Width, Height: integer;
  Data: pointer; UsePBO: Boolean): boolean;
begin
  if FTexture.Created then begin result:=false;exit;
  end else result:=true;
  SetOGLTextureFormat(GL_RGBA16F,GL_RGBA,GL_HALF_FLOAT);
  SetDimensions(width, height);
  SetTarget(ttTextureRectangle);
  if not (stFilters in FSettedParams) then
    SetFilters(mnLinear, mgLinear);
  FTexture.UsePBO:=UsePBO;
  CreateTexture;
  UploadData(Data,UsePBO);
end;

procedure TTexture.LoadFromFile(Filename: String);
var format: GLEnum;
    w,h: integer;
    _data: pointer;
    ext: string;
    dds: PDDSImageDesc;
    fs: TFileStream;
begin
   if not (stTarget in FSettedParams) then SetTarget(ttTexture2D);
   ext:=uppercase(ExtractFileExt(FileName));
   if ext='.DDS' then begin
     SetTarget(ttTexture2D);
     fs:=TFileStream.Create(FileName, fmOpenRead);
     dds:=DDSLoadFromStream(fs);
     fs.Free;
     if dds.Compressed then CreateCompressedTexture(dds)
     else CreateUnCompressedTexture2D(dds);
     FreeMem(dds.Data,dds.ReservedMem);
//     dispose(dds.Data);
     Dispose(dds); dds:=nil;
     FLocation:=FileName;
     exit;
   end;
   assert(FTexture.Target<>0,'Texture target is not setted');
//   Data:=LoadDataFromFile(Filename,format, w,h);
   with FTexture do begin
     _Data:=LoadTexture(Filename,InternalFormat,ColorChanels,Precision,PixelSize,w,h);
     assert(assigned(_Data), 'Unsupported File Format: '+extractfilename(FileName));
     FLocation:=FileName;
     FSettedParams:=FSettedParams+[stFormats];
     //SetOGLTextureFormat(InternalFormat,ColorChanels,Precision);
     SetDimensions(w, h); FullSize:=w*h*PixelSize; Depth:=0;
     //SetFilters(mnNearest, mgNearest);
     if not (stFilters in FSettedParams) then begin
       if Precision=GL_UNSIGNED_BYTE then
       SetFilters(mnLinearMipmapLinear, mgLinear)
       else SetFilters(mnNearest, mgNearest);
     end;

     CreateTexture; UploadData(_Data,UsePBO); FreeMem(_Data,FullSize);
   end;
(*

    _Data:=LoadDataFromFile(Filename,format, w,h);
    SetFilters(mnLinearMipmapLinear, mgLinear);
    case format of
       GL_RGB: begin
                 if TextureTarget = ttTexture2D then begin
                    CreateRGB8Texture2D(w,h,_data);
                 end;
                 if TextureTarget = ttTextureRectangle then
                    CreateRGB8TextureRECT(w,h,_data);
               end;
       GL_RGBA:begin
                 if TextureTarget = ttTexture2D then
                    CreateRGBA8Texture2D(w,h,_data);
                 if TextureTarget = ttTextureRectangle then
                    CreateRGBA8TextureRECT(w,h,_data);
               end;
    end;
*)
end;

procedure TTexture.SaveAsBMP(FileName: String);
var p: pointer;
    pf: TPixelFormat;
    bmp: TBitmap;
    pb: PByteArray;
    i,j,k: integer;
begin
  bmp:=TBitmap.Create;
  bmp.Width:=Width;
  bmp.Height:=Height;
  new(p); DownloadData(p);
  case PixelSize of
    1: pf:=pf8bit;
    3: pf:=pf24bit;
    4: pf:=pf32bit;
  end;
  bmp.PixelFormat:=pf;
  for i:=0 to Height-1 do begin
    pb:=bmp.ScanLine[Height-i-1];
    for j:=0 to Width-1 do begin
      for k:=0 to PixelSize-1 do
        pb[j*PixelSize+k]:=pbytearray(p)[i*Width*PixelSize+j*PixelSize+k];
    end;
  end;
  bmp.SaveToFile(FileName); bmp.Free;
  dispose(p);
end;

procedure TTexture.SaveAsTGA(FileName: String);
var p: pointer;
    pf: TPixelFormat;
begin
  new(p); DownloadData(p); pf:=pf24bit;
  case PixelSize of
    1: pf:=pf8bit;
    3: pf:=pf24bit;
    4: pf:=pf32bit;
  end;
  SaveTGAImage(FileName,p,Width,Height,pf);
  dispose(p);
end;

function TTexture.SetAnisotropyLevel(level: single): boolean;
begin
  glTexParameterf(FTexture.Target, GL_TEXTURE_MAX_ANISOTROPY_EXT, level);
  result:=true;
end;

procedure TTexture.SetBlending;
begin
  case FBlendingMode of
    tbmOpaque:
      begin
        glDisable(GL_BLEND);
        glDisable(GL_ALPHA_TEST);
      end;
    tbmTransparency:
      begin
        glEnable(GL_BLEND); glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glAlphaFunc(GL_GREATER,0);
      end;
    tbmAdditive:
      begin
        glEnable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE);
        glAlphaFunc(GL_GREATER,0);
      end;
    tbmAlphaTest50:
      begin
        glDisable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GEQUAL,0.5);
      end;
    tbmAlphaTest100:
      begin
        glDisable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GEQUAL,1);
      end;
    tbmModulate:
      begin
        glEnable(GL_BLEND);
        glEnable(GL_ALPHA_TEST);
        glBlendFunc(GL_DST_COLOR,GL_ZERO);
        glAlphaFunc(GL_GREATER,0);
      end;
  end;
end;

procedure TTexture.setCompareMode(const Value: boolean);
begin
  if FTexture.ColorChanels=GL_DEPTH_COMPONENT then
  FCompareMode := Value else FCompareMode := false;
end;

procedure TTexture.ResetBlending;
begin
  OGLStateEmul.GLStateCache.BlendingCache.Reset;
  OGLStateEmul.GLStateCache.AlphaCache.Reset;
end;

{ TTextureLibrary }

function TTextureLibrary.Add(Texture: TTexture): integer;
var Hash: integer;
begin
 if Texture.Name='' then begin
   Texture.Name:='GLTexture'+inttostr(Count+1);
   Texture.FInternalName:=Texture.Name;
 end;
 Result := inherited Add(Texture);
 Hash:=StringHashKey(texture.FInternalName);
 FHashList.Add(Hash);
 Hash:=StringHashKey(lowercase(texture.FLocation));
 FLocHashList.Add(Hash);
 if assigned(FOnAdding) then FOnAdding(self);
end;

function TTextureLibrary.AddNewTexture(Name: string): TTexture;
var tex: TTexture;
begin
  tex:=TTexture.Create;
  if Name<>'' then begin
    tex.FInternalName:=Name+'_'; tex.Name:=Name;
  end else begin
     tex.Name:='GLTexture'+inttostr(Count+1);
     tex.FInternalName:=tex.Name;
  end;
  result:=tex; Add(tex);
end;

procedure TTextureLibrary.Clear;
begin
   inherited;
end;

constructor TTextureLibrary.Create;
begin
  inherited;
  FHashList:=TIntegerList.Create;
  FLocHashList:=TIntegerList.Create;
end;

procedure TTextureLibrary.Delete(Index: Integer);
begin
  if assigned(Items[Index]) then begin
    Items[Index].Free; Items[Index]:=nil;
  end;
  FHashList.Delete(Index);
  inherited;
end;

destructor TTextureLibrary.Destroy;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    if assigned(Items[i]) then
      if Items[i].FOwner=Self then begin
         TTexture(Items[i]).Free; Items[i]:=nil;
      end;
  end;
  FHashList.Free; FLocHashList.Free;
  inherited;
end;

procedure TTextureLibrary.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  FHashList.Exchange(Index1, Index2);
  FLocHashList.Exchange(Index1, Index2);
end;

function TTextureLibrary.Get(Index: Integer): TTexture;
begin
  result := inherited Get(index);
end;

procedure TTextureLibrary.Insert(Index: Integer; Texture: TTexture);
var Hash: integer;
begin
  inherited Insert(Index, Texture);
  Hash:=StringHashKey(Texture.FInternalName+'_');
  FHashList.Insert(Index,Hash);
  Hash:=StringHashKey(lowercase(Texture.FLocation));
  FLocHashList.Insert(Index,Hash);
end;

function TTextureLibrary.Last: TTexture;
begin
  result:=Items[Count-1];
end;

procedure TTextureLibrary.Put(Index: Integer; Item: TTexture);
var Hash: integer;
begin
  inherited Put(Index, Item);
  if Item.Owner=nil then Item.Owner:=Self;
  Hash:=StringHashKey(Item.FInternalName+'_'+Item.FLocation);
  FHashList[Index]:=Hash;
  Hash:=StringHashKey(lowercase(Item.FLocation));
  FLocHashList[Index]:=Hash;
end;

function TTextureLibrary.TextureByLocation(Location: string): TTexture;
var n, i: integer;
    loc: string;
begin
  if Location='' then begin result:=nil; exit; end;
  loc:=lowercase(location);
  n:=StringHashKey(Loc); i:=0;
  while (i<=FLocHashList.Count-1) do begin
    if n=FLocHashList[i] then
      if lowercase(Items[i].FLocation)=Loc then Break;
      inc(i);
  end;
  if i<FLocHashList.Count then result:=Items[i] else result:=nil;
end;

function TTextureLibrary.TextureByName(Name,Location: string): TTexture;
var n, i: integer;
    hName: string;
begin
  hName:=Name+'_'+Location;
  n:=StringHashKey(hName); i:=0;
  while (i<=FHashList.Count-1) do begin
      if n=FHashList[i] then
         if Items[i].FInternalName=hName then Break;
      inc(i);
  end;
  if i<FHashList.Count then result:=Items[i] else result:=nil;
end;

end.

