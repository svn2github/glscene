//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Imposter building and rendering implementation.
}
unit VXS.Imposter;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  VXS.OpenGL1x,
  VXS.Scene,
  VXS.Context,
  VXS.VectorTypes,
  VXS.VectorGeometry,
  VXS.PersistentClasses,
  VXS.CrossPlatform,
  VXS.Graphics,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.Coordinates,
  VXS.BaseClasses,
  VXS.State,
  VXS.PipelineTransformation,
  VXS.TextureFormat,
  VXS.Utils;

type
  { Imposter rendering options.
     Following options are supported:
      impoBlended : the imposters are transparently blended during renders,
     this will smooth their edges but requires them to be rendered sorted
     from back to front
      impoAlphaTest : alpha test is used to eliminate transparent pixels,
     the alpha treshold is adjusted by the AlphaTreshold property
      impoNearestFiltering : use nearest texture filtering (the alternative
     is linear filtering)
      impoPerspectiveCorrection : activates a special imposter rendering
     projection suitable for distorting the sprites when seen from a level
     angle of view with a wide focal camera (think trees/grass when walking
     in a forest), if not active, the imposter sprites are camera-facing  }
  TImposterOption = (impoBlended, impoAlphaTest, impoNearestFiltering,
    impoPerspectiveCorrection);
  TImposterOptions = set of TImposterOption;

const
  cDefaultImposterOptions = [impoBlended, impoAlphaTest];

type
  TVXImposterBuilder = class;

  { Base class for imposters manipulation and handling.
     Rendering imposters is performed by three methods, BeginRender must
     be invoked first, then Render for each of the impostr
     This class assumes a single impostor per texture.

     Note: Remeber to enable Destination Alpha on your viewer.}
  TImposter = class(TObject)
  private
    
    FRequestCount: Integer;
    FBuilder: TVXImposterBuilder;
    FTexture: TVXTextureHandle;
    FImpostoredObject: TVXBaseSceneObject;
    FAspectRatio: Single;
    FModulated: Boolean;
  protected
    FVx, FVy: TVector;
    FStaticOffset: TVector;
    FQuad: array[0..3] of TVector;
    FStaticScale: Single;
    procedure PrepareTexture(var rci: TVXRenderContextInfo); virtual;
    procedure RenderQuad(const texExtents, objPos: TVector; size: Single);
  public
    constructor Create(aBuilder: TVXImposterBuilder); virtual;
    destructor Destroy; override;
    procedure BeginRender(var rci: TVXRenderContextInfo); virtual;
    procedure Render(var rci: TVXRenderContextInfo;
      const objPos, localCameraPos: TVector;
      size: Single); virtual;
    procedure EndRender(var rci: TVXRenderContextInfo); virtual;
    procedure RenderOnce(var rci: TVXRenderContextInfo;
      const objPos, localCameraPos: TVector;
      size: Single);
    property AspectRatio: Single read FAspectRatio write FAspectRatio;
    property Builder: TVXImposterBuilder read FBuilder;
    property Texture: TVXTextureHandle read FTexture;
    property ImpostoredObject: TVXBaseSceneObject read FImpostoredObject write
      FImpostoredObject;
    property Modulated: Boolean read FModulated write FModulated;
  end;

   { Imposter loading events }
   TLoadingImposterEvent = function (Sender : TObject; impostoredObject :
     TVXBaseSceneObject; destImposter : TImposter) : TVXBitmap32 of object;
   {$NODEFINE TLoadingImposterEvent}
   //Used CPPB procedure instead of Delphi function
   //TLoadingImposterEvent = procedure (Sender : TObject; impostoredObject : TVXBaseSceneObject; destImposter : TImposter; var result : TVXBitmap32) of object;
   {$HPPEMIT 'typedef Glgraphics::TVXBitmap32* __fastcall (__closure *TLoadingImposterEvent)(System::TObject* Sender, Glscene::TVXBaseSceneObject* impostoredObject, TImposter* destImposter);'}

   TImposterLoadedEvent = procedure (Sender : TObject; impostoredObject :
         TVXBaseSceneObject;
         destImposter : TImposter) of object;

  TImposterReference = (irCenter, irTop, irBottom);

  { Abstract ImposterBuilder class. }
  TVXImposterBuilder = class(TVXUpdateAbleComponent)
  private
    FBackColor: TVXColor;
    FBuildOffset: TVXCoordinates;
    FImposterRegister: TPersistentObjectList;
    FRenderPoint: TVXRenderPoint;
    FImposterOptions: TImposterOptions;
    FAlphaTreshold: Single;
    FImposterReference: TImposterReference;
    FOnLoadingImposter: TLoadingImposterEvent;
    FOnImposterLoaded: TImposterLoadedEvent;
  protected
    procedure SetRenderPoint(AValue: TVXRenderPoint);
    procedure RenderPointFreed(Sender: TObject);
    procedure SetBackColor(AValue: TVXColor);
    procedure SetBuildOffset(AValue: TVXCoordinates);
    procedure SetImposterReference(AValue: TImposterReference);
    procedure InitializeImpostorTexture(const textureSize: TVXPoint);
    property ImposterRegister: TPersistentObjectList read FImposterRegister;
    procedure UnregisterImposter(imposter: TImposter);
    function CreateNewImposter: TImposter; virtual;
    procedure PrepareImposters(Sender: TObject; var rci: TVXRenderContextInfo);
      virtual;
    procedure DoPrepareImposter(var rci: TVXRenderContextInfo;
      impostoredObject: TVXBaseSceneObject;
      destImposter: TImposter); virtual; abstract;
    procedure DoUserSpecifiedImposter(
      var rci: TVXRenderContextInfo;
      destImposter: TImposter;
      bmp32: TVXBitmap32); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure NotifyChange(Sender: TObject); override;
    { Returns a valid imposter for the specified object. 
       Imposter must have been requested first, and the builder given
       an opportunity to prepare it before it can be available. }
    function ImposterFor(impostoredObject: TVXBaseSceneObject): TImposter;
    { Request an imposter to be prepared for the specified object. }
    procedure RequestImposterFor(impostoredObject: TVXBaseSceneObject);
    { Tells the imposter for the specified object is no longer needed. }
    procedure UnRequestImposterFor(impostoredObject: TVXBaseSceneObject);
  published
      { Specifies the render point at which the impostor texture(s) can be prepared. 
         For best result, the render point should happen in viewer that has
         a destination alpha (otherwise, impostors will be opaque). }
    property RenderPoint: TVXRenderPoint read FRenderPoint write SetRenderPoint;
    { Background color for impostor rendering. 
       Typically, you'll want to leave the alpha channel to zero, and pick
       as RGB as color that matches the impostor'ed objects edge colors most.}
    property BackColor: TVXColor read FBackColor write SetBackColor;
    { Offset applied to the impostor'ed object during imposter construction. 
       Can be used to manually tune the centering of objects. }
    property BuildOffset: TVXCoordinates read FBuildOffset write SetBuildOffset;
    { Imposter rendering options. }
    property ImposterOptions: TImposterOptions read FImposterOptions write
      FImposterOptions default cDefaultImposterOptions;
    { Determines how the imposter are handled. 
       This is the reference point for imposters, impostor'ed objects that
       are centered should use irCenter, those whose bottom is the origin
       should use irBottom, etc. }
    property ImposterReference: TImposterReference read FImposterReference write
      SetImposterReference default irCenter;
    { Alpha testing teshold.  }
    property AlphaTreshold: Single read FAlphaTreshold write FAlphaTreshold;
    { Event fired before preparing/loading an imposter. 
       If an already prepared version of the importer is available, place
       it in the TVXBitmap32 the event shall return (the bitmap will be
       freed by the imposter builder). If a bitmap is specified, it will
       be used in place of what automatic generation could have generated. }
    property OnLoadingImposter: TLoadingImposterEvent read FOnLoadingImposter
      write FOnLoadingImposter;
    { Event fired after preparing/loading an imposter. 
       This events gives an opportunity to save the imposter after it has
       been loaded or prepared. }
    property OnImposterLoaded: TImposterLoadedEvent read FOnImposterLoaded write
      FOnImposterLoaded;
  end;

    { Describes a set of orientation in a corona fashion. }
  TVXStaticImposterBuilderCorona = class(TCollectionItem)
  private
    FSamples: Integer;
    FElevation: Single;
    FSampleBaseIndex: Integer;
  protected
    function GetDisplayName: string; override;
    procedure SetSamples(AValue: Integer);
    procedure SetElevation(AValue: Single);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Samples: Integer read FSamples write SetSamples default 8;
    property Elevation: Single read FElevation write SetElevation;
  end;

  TCoronaTangentLookup = record
    minTan, maxTan: Single;
    corona: TVXStaticImposterBuilderCorona;
  end;

  TVXStaticImposterBuilderCoronas = class(TOwnedCollection)
  private
    FCoronaTangentLookup: array of TCoronaTangentLookup;
  protected
    procedure SetItems(AIndex: Integer; const AValue:
      TVXStaticImposterBuilderCorona);
    function GetItems(AIndex: Integer): TVXStaticImposterBuilderCorona;
    procedure Update(Item: TCollectionItem); override;
    procedure PrepareSampleBaseIndices;
    procedure PrepareCoronaTangentLookup;
    function CoronaForElevationTangent(aTangent: Single):
      TVXStaticImposterBuilderCorona;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TVXStaticImposterBuilderCorona; overload;
    function Add(const elevation: Single; samples: Integer):
      TVXStaticImposterBuilderCorona; overload;
    property Items[AIndex: Integer]: TVXStaticImposterBuilderCorona read GetItems
    write SetItems; default;
    function SampleCount: Integer;
    procedure NotifyChange; virtual;
    procedure EndUpdate; override;
  end;

  { Imposter class whose texture contains several views from different angles. }
  TStaticImposter = class(TImposter)
  public
    procedure Render(var rci: TVXRenderContextInfo;
      const objPos, localCameraPos: TVector;
      size: Single); override;
  end;

  TSIBLigthing = (siblNoLighting, siblStaticLighting, siblLocalLighting);

  { Builds imposters whose texture is a catalog of prerendered views. }
  TVXStaticImposterBuilder = class(TVXImposterBuilder)
  private
    FCoronas: TVXStaticImposterBuilderCoronas;
    FSampleSize: Integer;
    FTextureSize: TVXPoint;
    FSamplesPerAxis: TVXPoint;
    FInvSamplesPerAxis: TVector2f;
    FSamplingRatioBias, FInvSamplingRatioBias: Single;
    FLighting: TSIBLigthing;
    FSamplesAlphaScale: Single;
  protected
    procedure SetCoronas(AValue: TVXStaticImposterBuilderCoronas);
    procedure SetSampleSize(AValue: Integer);
    procedure SetSamplingRatioBias(AValue: Single);
    function StoreSamplingRatioBias: Boolean;
    procedure SetLighting(AValue: TSIBLigthing);
    procedure SetSamplesAlphaScale(AValue: Single);
    function StoreSamplesAlphaScale: Boolean;
    function GetTextureSizeInfo: string;
    procedure SetTextureSizeInfo(const texSize: string);
    { Computes the optimal texture size that would be able to hold all samples. }
    function ComputeOptimalTextureSize: TVXPoint;
    function CreateNewImposter: TImposter; override;
    procedure DoPrepareImposter(var rci: TVXRenderContextInfo;
      impostoredObject: TVXBaseSceneObject;
      destImposter: TImposter); override;
    procedure DoUserSpecifiedImposter(
      var rci: TVXRenderContextInfo;
      destImposter: TImposter;
      bmp32: TVXBitmap32); override;
    procedure ComputeStaticParams(destImposter: TImposter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Render imposter texture. 
       Buffer and object must be compatible, RC must have been activated. }
    procedure Render(var rci: TVXRenderContextInfo;
      impostoredObject: TVXBaseSceneObject;
      destImposter: TImposter);
    { Ratio (0..1) of the texture that will be used by samples. 
       If this value is below 1, you're wasting texture space and may
       as well increase the number of samples. }
    function TextureFillRatio: Single;
    { Meaningful only after imposter texture has been prepared. }
    property TextureSize: TVXPoint read FTextureSize;
    property SamplesPerAxis: TVXPoint read FSamplesPerAxis;
  published
      { Description of the samples looking orientations. }
    property Coronas: TVXStaticImposterBuilderCoronas read FCoronas write
      SetCoronas;
    { Size of the imposter samples (square). }
    property SampleSize: Integer read FSampleSize write SetSampleSize default  32;
    { Size ratio applied to the impostor'ed objects during sampling. 
       Values greater than one can be used to "fill" the samples more
       by scaling up the object. This is especially useful when the impostor'ed
       object doesn't fill its bounding sphere, and/or if the outer details
       are not relevant for impostoring. }
    property SamplingRatioBias: Single read FSamplingRatioBias write
      SetSamplingRatioBias stored StoreSamplingRatioBias;
    { Scale factor apply to the sample alpha channel. 
       Main use is to saturate the samples alpha channel, and make fully
       opaque what would have been partially transparent, while leaving
       fully transparent what was fully transparent. }
    property SamplesAlphaScale: Single read FSamplesAlphaScale write
      SetSamplesAlphaScale stored StoreSamplesAlphaScale;
    { Lighting mode to apply during samples construction. }
    property Lighting: TSIBLigthing read FLighting write FLighting default
      siblStaticLighting;
    { Dummy property that returns the size of the imposter texture. 
       This property is essentially here as a helper at design time,
       to give you the requirements your coronas and samplesize parameters
       imply. }
    property TextureSizeInfo: string read GetTextureSizeInfo write
      SetTextureSizeInfo stored False;
  end;

  TVXDynamicImposterBuilder = class(TVXImposterBuilder)
  private
    FMinTexSize, FMaxTexSize: Integer;
    FMinDistance, FTolerance: Single;
    FUseMatrixError: Boolean;
  protected
    procedure SetMinDistance(const AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {         procedure DoRender(var rci : TVXRenderContextInfo;
                                renderSelf, renderChildren : Boolean); override; }
  published
    property MinTexSize: Integer read FMinTexSize write FMinTexSize;
    property MaxTexSize: Integer read FMaxTexSize write FMaxTexSize;
    property MinDistance: Single read FMinDistance write SetMinDistance;
    property Tolerance: Single read FTolerance write FTolerance;
    property UseMatrixError: Boolean read FUseMatrixError write FUseMatrixError;
  end;

  TVXImposter = class(TVXImmaterialSceneObject)
  private
    FBuilder: TVXImposterBuilder;
    FImpostoredObject: TVXBaseSceneObject;
  protected
    procedure SetBuilder(const AValue: TVXImposterBuilder);
    procedure SetImpostoredObject(const AValue: TVXBaseSceneObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
    procedure DoRender(var ARci: TVXRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    property Builder: TVXImposterBuilder read FBuilder write SetBuilder;
    property ImpostoredObject: TVXBaseSceneObject read FImpostoredObject write
      SetImpostoredObject;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

const
  cReferenceToPos: array[Low(TImposterReference)..High(TImposterReference)] of Single = (0, -1, 1);

  // ----------
  // ---------- TImposter ----------
  // ----------

constructor TImposter.Create(aBuilder: TVXImposterBuilder);
begin
  inherited Create;
  FBuilder := aBuilder;
  FTexture := TVXTextureHandle.Create;
  aBuilder.FImposterRegister.Add(Self);
  FAspectRatio := 1;
end;

destructor TImposter.Destroy;
begin
  if Assigned(FBuilder) then
    FBuilder.UnregisterImposter(Self);
  FTexture.Free;
  inherited;
end;

procedure TImposter.PrepareTexture(var rci: TVXRenderContextInfo);
var
  i: Integer;
begin
  if FTexture.Handle <> 0 then
    Exit;

  FTexture.AllocateHandle;
  FTexture.Target := ttTexture2D;
  rci.VXStates.TextureBinding[0, ttTexture2D] := FTexture.Handle;
  if GL_EXT_texture_edge_clamp then
    i := GL_CLAMP_TO_EDGE
  else
    i := GL_CLAMP;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure TImposter.BeginRender(var rci: TVXRenderContextInfo);
var
  mat: TMatrix;
  filter: GLEnum;
  fx, fy, yOffset, cosAlpha, dynScale: Single;
begin
  with rci.VxStates do
  begin
    Disable(stLighting);
    Disable(stCullFace);
    ActiveTextureEnabled[ttTexture2D] := True;

    if impoAlphaTest in Builder.ImposterOptions then
    begin
      Enable(stAlphaTest);
      SetAlphaFunction(cfGEqual, Builder.AlphaTreshold);
    end
    else
      Disable(stAlphaTest);

    if impoBlended in Builder.ImposterOptions then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end
    else
      Disable(stBlend);

    TextureBinding[0, ttTexture2D] := Texture.Handle;

    if impoNearestFiltering in Builder.ImposterOptions then
      filter := GL_NEAREST
    else
      filter := GL_LINEAR;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
    if FModulated then
    begin
      glColor4fv(@XYZWHmgVector);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    end
    else
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    mat := rci.PipelineTransformation.ModelViewMatrix^;
    FVx.X := mat.X.X;
    FVx.Y := mat.Y.X;
    FVx.Z := mat.Z.X;
    NormalizeVector(FVx);

    FVy.X := mat.X.Y;
    FVy.Y := mat.Y.Y;
    FVy.Z := mat.Z.Y;
    NormalizeVector(FVy);
    if impoPerspectiveCorrection in Builder.ImposterOptions then
    begin
      cosAlpha := VectorDotProduct(FVy, YHmgVector);
      FVy := VectorLerp(FVy, YHmgVector, Abs(cosAlpha));
      NormalizeVector(FVy);
      dynScale := ClampValue(1 / cosAlpha, 1, 1.414) * FStaticScale;
    end
    else
      dynScale := FStaticScale;

    fx := Sqrt(FAspectRatio);
    fy := 1 / fx;
    yOffset := cReferenceToPos[Builder.ImposterReference] * dynScale * fy;
    fx := fx * dynScale;
    fy := fy * dynScale;

    FQuad[0] := VectorSubtract(VectorCombine(FVx, FVy, fx, fy + yOffset),
      FStaticOffset);
    FQuad[1] := VectorSubtract(VectorCombine(FVx, FVy, -fx, fy + yOffset),
      FStaticOffset);
    FQuad[2] := VectorSubtract(VectorCombine(FVx, FVy, -fx, -fy + yOffset),
      FStaticOffset);
    FQuad[3] := VectorSubtract(VectorCombine(FVx, FVy, fx, -fy + yOffset),
      FStaticOffset);

    glBegin(GL_QUADS);
  end;
end;

procedure TImposter.Render(var rci: TVXRenderContextInfo;
  const objPos, localCameraPos: TVector;
  size: Single);
const
  cQuadTexExtents: TVector = (X:0; Y:0; Z:1; W:1);
begin
  RenderQuad(cQuadTexExtents, objPos, size);
end;

procedure TImposter.RenderQuad(const texExtents, objPos: TVector; size: Single);
var
  pos: TVector;
begin
  VectorCombine(objPos, FQuad[0], size, pos);
  glTexCoord2f(texExtents.Z, texExtents.W);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[1], size, pos);
  glTexCoord2f(texExtents.X, texExtents.W);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[2], size, pos);
  glTexCoord2f(texExtents.X, texExtents.Y);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[3], size, pos);
  glTexCoord2f(texExtents.Z, texExtents.Y);
  glVertex3fv(@pos);
end;

procedure TImposter.EndRender(var rci: TVXRenderContextInfo);
begin
  glEnd;
  rci.VXStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

procedure TImposter.RenderOnce(var rci: TVXRenderContextInfo;
  const objPos, localCameraPos: TVector;
  size: Single);
begin
  BeginRender(rci);
  Render(rci, objPos, localCameraPos, size);
  EndRender(rci);
end;

// ----------
// ---------- TVXImposterBuilder ----------
// ----------

constructor TVXImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FImposterRegister := TPersistentObjectList.Create;
  FBackColor := TVXColor.CreateInitialized(Self, clrTransparent);
  FBuildOffset := TVXCoordinates.CreateInitialized(Self, NullHmgPoint, CsPoint);
  FImposterOptions := cDefaultImposterOptions;
  FAlphaTreshold := 0.5;
end;

destructor TVXImposterBuilder.Destroy;
var
  i: Integer;
begin
  FBuildOffset.Free;
  FBackColor.Free;
  for i := 0 to FImposterRegister.Count - 1 do
    TImposter(FImposterRegister[i]).FBuilder := nil;
  FImposterRegister.CleanFree;
  inherited;
end;

procedure TVXImposterBuilder.Notification(AComponent: TComponent; Operation:
  TOperation);
var
  i: Integer;
  imposter: TImposter;
begin
  if Operation = opRemove then
  begin
    if AComponent = FRenderPoint then
      FRenderPoint := nil;
    for i := FImposterRegister.Count - 1 downto 0 do
    begin
      imposter := TImposter(FImposterRegister[i]);
      if imposter.ImpostoredObject = AComponent then
      begin
        imposter.Free;
        Break;
      end;
    end;
  end;
  inherited;
end;

function TVXImposterBuilder.CreateNewImposter: TImposter;
begin
  Result := TImposter.Create(Self);
end;

procedure TVXImposterBuilder.PrepareImposters(Sender: TObject; var rci:
  TVXRenderContextInfo);
var
  i: Integer;
  imp: TImposter;
  bmp32: TVXBitmap32;
begin
  for i := 0 to ImposterRegister.Count - 1 do
  begin
    imp := TImposter(ImposterRegister[i]);
    if (imp.ImpostoredObject <> nil) and (imp.Texture.Handle = 0) then
    begin
      if Assigned(FOnLoadingImposter) then
        bmp32:=FOnLoadingImposter(Self, imp.ImpostoredObject, imp)
      else
        bmp32 := nil;
		      if not Assigned(bmp32) then
       DoPrepareImposter(rci, imp.ImpostoredObject, imp)
      else
      begin
        DoUserSpecifiedImposter(rci, imp, bmp32);
        bmp32.Free;
      end;
      if Assigned(FOnImposterLoaded) then
        FOnImposterLoaded(Self, imp.ImpostoredObject, imp);
    end;
  end;
end;

procedure TVXImposterBuilder.DoUserSpecifiedImposter(
  var rci: TVXRenderContextInfo;
  destImposter: TImposter;
  bmp32: TVXBitmap32);
var
  size: Integer;
begin
  destImposter.PrepareTexture(rci);
  bmp32.RegisterAsOpenVXTexture(
    destImposter.FTexture, False, GL_RGBA8, size, size, size);
end;

procedure TVXImposterBuilder.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FImposterRegister.Count - 1 do
    TImposter(FImposterRegister[i]).Texture.DestroyHandle;
  inherited;
end;

function TVXImposterBuilder.ImposterFor(impostoredObject: TVXBaseSceneObject):
  TImposter;
var
  i: Integer;
begin
  for i := 0 to FImposterRegister.Count - 1 do
  begin
    Result := TImposter(FImposterRegister[i]);
    if Result.ImpostoredObject = impostoredObject then
      Exit;
  end;
  Result := nil;
end;

procedure TVXImposterBuilder.RequestImposterFor(impostoredObject:
  TVXBaseSceneObject);
var
  imposter: TImposter;
begin
  if impostoredObject = nil then
    Exit;
  imposter := ImposterFor(impostoredObject);
  if imposter = nil then
  begin
    imposter := CreateNewImposter;
    imposter.ImpostoredObject := impostoredObject;
  end;
  Inc(imposter.FRequestCount);
end;

procedure TVXImposterBuilder.UnRequestImposterFor(impostoredObject:
  TVXBaseSceneObject);
var
  imposter: TImposter;
begin
  if impostoredObject = nil then
    Exit;
  imposter := ImposterFor(impostoredObject);
  if imposter <> nil then
  begin
    Dec(imposter.FRequestCount);
    if imposter.FRequestCount = 0 then
      imposter.Free;
  end;
end;

procedure TVXImposterBuilder.SetRenderPoint(AValue: TVXRenderPoint);
begin
  if AValue <> FRenderPoint then
  begin
    if Assigned(FRenderPoint) then
    begin
      FRenderPoint.RemoveFreeNotification(Self);
      FRenderPoint.UnRegisterCallBack(PrepareImposters);
    end;
    FRenderPoint := AValue;
    if Assigned(FRenderPoint) then
    begin
      FRenderPoint.FreeNotification(Self);
      FRenderPoint.RegisterCallBack(PrepareImposters, RenderPointFreed);
    end;
  end;
end;

procedure TVXImposterBuilder.RenderPointFreed(Sender: TObject);
begin
  FRenderPoint := nil;
end;

procedure TVXImposterBuilder.SetBackColor(AValue: TVXColor);
begin
  FBackColor.Assign(AValue);
end;

procedure TVXImposterBuilder.SetBuildOffset(AValue: TVXCoordinates);
begin
  FBuildOffset.Assign(AValue);
end;

procedure TVXImposterBuilder.SetImposterReference(AValue: TImposterReference);
begin
  if FImposterReference <> AValue then
  begin
    FImposterReference := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXImposterBuilder.InitializeImpostorTexture(const textureSize:
  TVXPoint);
begin
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, textureSize.X, textureSize.Y, 0,
      GL_RGBA, GL_UNSIGNED_BYTE, nil);
end;

procedure TVXImposterBuilder.UnregisterImposter(imposter: TImposter);
begin
  if imposter.Builder = Self then
  begin
    FImposterRegister.Remove(imposter);
    imposter.FBuilder := nil;
  end;
end;

// ----------
// ---------- TVXStaticImposterBuilderCorona ----------
// ----------

constructor TVXStaticImposterBuilderCorona.Create(ACollection: TCollection);
begin
  inherited;
  FSamples := 8;
end;

destructor TVXStaticImposterBuilderCorona.Destroy;
begin
  inherited;
end;

procedure TVXStaticImposterBuilderCorona.Assign(Source: TPersistent);
begin
  if Source is TVXStaticImposterBuilderCorona then
  begin
    FSamples := TVXStaticImposterBuilderCorona(Source).FSamples;
    FElevation := TVXStaticImposterBuilderCorona(Source).FElevation;
  end;
  inherited;
end;

function TVXStaticImposterBuilderCorona.GetDisplayName: string;
begin
  Result := Format('%.1f° / %d samples', [Elevation, Samples]);
end;

procedure TVXStaticImposterBuilderCorona.SetSamples(AValue: Integer);
begin
  if AValue <> FSamples then
  begin
    FSamples := AValue;
    if FSamples < 1 then
      FSamples := 1;
    (Collection as TVXStaticImposterBuilderCoronas).NotifyChange;
  end;
end;

procedure TVXStaticImposterBuilderCorona.SetElevation(AValue: Single);
begin
  if AValue <> FElevation then
  begin
    FElevation := ClampValue(AValue, -89, 89);
    (Collection as TVXStaticImposterBuilderCoronas).NotifyChange;
  end;
end;

// ----------
// ---------- TVXStaticImposterBuilderCoronas ----------
// ----------

constructor TVXStaticImposterBuilderCoronas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TVXStaticImposterBuilderCorona);
end;

function TVXStaticImposterBuilderCoronas.Add: TVXStaticImposterBuilderCorona;
begin
  Result := (inherited Add) as TVXStaticImposterBuilderCorona;
end;

function TVXStaticImposterBuilderCoronas.Add(const elevation: Single;
  samples: Integer): TVXStaticImposterBuilderCorona;
begin
  Result := (inherited Add) as TVXStaticImposterBuilderCorona;
  Result.Elevation := elevation;
  Result.Samples := samples;
end;

procedure TVXStaticImposterBuilderCoronas.SetItems(AIndex: Integer; const
  AValue: TVXStaticImposterBuilderCorona);
begin
  inherited Items[AIndex] := AValue;
end;

function TVXStaticImposterBuilderCoronas.GetItems(AIndex: Integer):
  TVXStaticImposterBuilderCorona;
begin
  Result := TVXStaticImposterBuilderCorona(inherited Items[AIndex]);
end;

procedure TVXStaticImposterBuilderCoronas.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

procedure TVXStaticImposterBuilderCoronas.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is
    TVXUpdateAbleComponent) then
    TVXUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TVXStaticImposterBuilderCoronas.EndUpdate;
begin
  inherited;
  NotifyChange;
end;

function TVXStaticImposterBuilderCoronas.SampleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].Samples;
end;

procedure TVXStaticImposterBuilderCoronas.PrepareSampleBaseIndices;
var
  p, i: Integer;
begin
  p := 0;
  for i := 0 to Count - 1 do
  begin
    Items[i].FSampleBaseIndex := p;
    Inc(p, Items[i].Samples);
  end;
end;

procedure TVXStaticImposterBuilderCoronas.PrepareCoronaTangentLookup;
var
  i, j: Integer;
  corona: TVXStaticImposterBuilderCorona;
  boundary: Single;
begin
  SetLength(FCoronaTangentLookup, Count);
  // place them in the array and sort by ascending elevation
  for i := 0 to Count - 1 do
    FCoronaTangentLookup[i].corona := Items[i];
  for i := 0 to Count - 2 do
    for j := i + 1 to Count - 1 do
      if FCoronaTangentLookup[j].corona.Elevation <
        FCoronaTangentLookup[i].corona.Elevation then
      begin
        corona := FCoronaTangentLookup[j].corona;
        FCoronaTangentLookup[j].corona := FCoronaTangentLookup[i].corona;
        FCoronaTangentLookup[i].corona := corona;
      end;
  // adjust min max then intermediate boundaries
  FCoronaTangentLookup[0].minTan := -1e30;
  FCoronaTangentLookup[Count - 1].minTan := 1e30;
  for i := 0 to Count - 2 do
  begin
    boundary := Tangent((0.5 * cPIdiv180) * (FCoronaTangentLookup[i].corona.Elevation
      + FCoronaTangentLookup[i + 1].corona.Elevation));
    FCoronaTangentLookup[i].maxTan := boundary;
    FCoronaTangentLookup[i + 1].minTan := boundary;
  end;
end;

function TVXStaticImposterBuilderCoronas.CoronaForElevationTangent(aTangent:
  Single): TVXStaticImposterBuilderCorona;
var
  i, n: Integer;
begin
  n := High(FCoronaTangentLookup);
  if (n = 0) or (aTangent <= FCoronaTangentLookup[0].maxTan) then
    Result := FCoronaTangentLookup[0].corona
  else if aTangent > FCoronaTangentLookup[n].minTan then
    Result := FCoronaTangentLookup[n].corona
  else
  begin
    Result := FCoronaTangentLookup[1].corona;
    for i := 2 to n - 2 do
    begin
      if aTangent <= FCoronaTangentLookup[i].minTan then
        Break;
      Result := FCoronaTangentLookup[i].corona;
    end;
  end;
end;

// ----------
// ---------- TStaticImposter ----------
// ----------

procedure TStaticImposter.Render(var rci: TVXRenderContextInfo;
  const objPos, localCameraPos: TVector;
  size: Single);
var
  azimuthAngle: Single;
  i: Integer;
  x, y: Word;
  bestCorona: TVXStaticImposterBuilderCorona;
  texExtents: TVector;
  tdx, tdy: Single;
  siBuilder: TVXStaticImposterBuilder;
begin // inherited; exit;
  siBuilder := TVXStaticImposterBuilder(Builder);

  // determine closest corona
  bestCorona := siBuilder.Coronas.CoronaForElevationTangent(
    localCameraPos.Y / VectorLength(localCameraPos.X, localCameraPos.Z));

  // determine closest sample in corona
  azimuthAngle := FastArcTangent2(localCameraPos.Z, localCameraPos.X) + cPI;
  i := Round(azimuthAngle * bestCorona.Samples * cInv2PI);
  if i < 0 then
    i := 0
  else if i >= bestCorona.Samples then
    i := bestCorona.Samples - 1;
  i := bestCorona.FSampleBaseIndex + i;

  tdx := siBuilder.FInvSamplesPerAxis.X;
  tdy := siBuilder.FInvSamplesPerAxis.Y;
  DivMod(i, siBuilder.SamplesPerAxis.X, y, x);
  texExtents.X := tdx * x;
  texExtents.Y := tdy * y;
  texExtents.Z := texExtents.X + tdx;
  texExtents.W := texExtents.Y + tdy;

  // then render it
  RenderQuad(texExtents, objPos, Size);
end;

// ----------
// ---------- TVXStaticImposterBuilder ----------
// ----------

constructor TVXStaticImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FCoronas := TVXStaticImposterBuilderCoronas.Create(Self);
  FCoronas.Add;
  FSampleSize := 16;
  FSamplingRatioBias := 1;
  FInvSamplingRatioBias := 1;
  FLighting := siblStaticLighting;
  FSamplesAlphaScale := 1;
end;

destructor TVXStaticImposterBuilder.Destroy;
begin
  FCoronas.Free;
  inherited;
end;

function TVXStaticImposterBuilder.CreateNewImposter: TImposter;
begin
  Result := TStaticImposter.Create(Self);
end;

procedure TVXStaticImposterBuilder.SetCoronas(AValue:
  TVXStaticImposterBuilderCoronas);
begin
  FCoronas.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXStaticImposterBuilder.SetSampleSize(AValue: Integer);
begin
  AValue := RoundUpToPowerOf2(AValue);
  if AValue < 8 then
    AValue := 8;
  if AValue > 1024 then
    AValue := 1024;
  if AValue <> FSampleSize then
  begin
    FSampleSize := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXStaticImposterBuilder.SetSamplingRatioBias(AValue: Single);
begin
  AValue := ClampValue(AValue, 0.1, 10);
  if AValue <> FSamplingRatioBias then
  begin
    FSamplingRatioBias := AValue;
    FInvSamplingRatioBias := 1 / AValue;
    NotifyChange(Self);
  end;
end;

function TVXStaticImposterBuilder.StoreSamplingRatioBias: Boolean;
begin
  Result := (FSamplingRatioBias <> 1);
end;

procedure TVXStaticImposterBuilder.SetLighting(AValue: TSIBLigthing);
begin
  if AValue <> FLighting then
  begin
    FLighting := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXStaticImposterBuilder.SetSamplesAlphaScale(AValue: Single);
begin
  if FSamplesAlphaScale <> AValue then
  begin
    FSamplesAlphaScale := AValue;
    NotifyChange(Self);
  end;
end;

function TVXStaticImposterBuilder.StoreSamplesAlphaScale: Boolean;
begin
  Result := (FSamplesAlphaScale <> 1);
end;

function TVXStaticImposterBuilder.GetTextureSizeInfo: string;
var
  t: TVXPoint;
  fill: Integer;
begin
  t := ComputeOptimalTextureSize;
  Result := Format('%d x %d', [t.X, t.Y]);
  fill := Coronas.SampleCount * SampleSize * SampleSize;
  if fill < t.X * t.Y then
    Result := Result + Format(' (%.1f%%)', [(100 * fill) / (t.X * t.Y)]);
end;

procedure TVXStaticImposterBuilder.SetTextureSizeInfo(const texSize: string);
begin
  // do nothing, this is a dummy property!
end;

procedure TVXStaticImposterBuilder.DoPrepareImposter(var rci:
  TVXRenderContextInfo;
  impostoredObject: TVXBaseSceneObject; destImposter: TImposter);
begin
  Render(rci, impostoredObject, destImposter);
end;

procedure TVXStaticImposterBuilder.DoUserSpecifiedImposter(
  var rci: TVXRenderContextInfo;
  destImposter:
  TImposter;
  bmp32: TVXBitmap32);
begin
  inherited;
  FTextureSize.X := bmp32.Width;
  FTextureSize.Y := bmp32.Height;
  ComputeStaticParams(destImposter);
end;

procedure TVXStaticImposterBuilder.ComputeStaticParams(destImposter: TImposter);
var
  radius: Single;
begin
  Coronas.PrepareCoronaTangentLookup;
  Coronas.PrepareSampleBaseIndices;

  FSamplesPerAxis.X := FTextureSize.X div SampleSize;
  FSamplesPerAxis.Y := FTextureSize.Y div SampleSize;
  FInvSamplesPerAxis.X := 1 / FSamplesPerAxis.X;
  FInvSamplesPerAxis.Y := 1 / FSamplesPerAxis.Y;
  Assert(FSamplesPerAxis.X * FSamplesPerAxis.Y >= Coronas.SampleCount,
    'User specified bitmap and imposter parameters don''t match');

  radius := destImposter.ImpostoredObject.BoundingSphereRadius /
    SamplingRatioBias;

  if ImposterReference = irCenter then
    destImposter.FStaticScale := radius
  else
    destImposter.FStaticScale := radius * 0.5;
  destImposter.FStaticOffset := FBuildOffset.DirectVector;
end;

procedure TVXStaticImposterBuilder.Render(var rci: TVXRenderContextInfo;
  impostoredObject: TVXBaseSceneObject; destImposter: TImposter);
var
  i, coronaIdx, curSample: Integer;
  radius: Single;
  cameraDirection, cameraOffset: TVector;
  xDest, xSrc, yDest, ySrc: Integer;
  corona: TVXStaticImposterBuilderCorona;
  fx, fy, yOffset: Single;
  LM: TMatrix;
begin
  FTextureSize := ComputeOptimalTextureSize;
  if (FTextureSize.X <= 0) and (FTextureSize.Y <= 0) then
  begin
    SampleSize := SampleSize shr 1;
    Assert(False,
      'Too many samples, can''t fit in a texture! Reduce SampleSize.');
  end;

  ComputeStaticParams(destImposter);

  radius := impostoredObject.BoundingSphereRadius / SamplingRatioBias;
  if ImposterReference <> irCenter then
    radius := radius * 0.5;

  Assert((rci.VXStates.ViewPort.Z >= SampleSize) and (rci.VXStates.ViewPort.W >= SampleSize),
    'ViewPort too small to render imposter samples!');

  // Setup the buffer in a suitable fashion for our needs
  with FBackColor do
    rci.VXStates.ColorClearValue := Color;
  if Lighting = siblNoLighting then
    rci.VXStates.Disable(stLighting);

  rci.PipelineTransformation.Push;
  fx := radius * rci.VXStates.ViewPort.Z / SampleSize;
  fy := radius * rci.VXStates.ViewPort.W / SampleSize;
  yOffset := cReferenceToPos[ImposterReference] * radius;
  rci.PipelineTransformation.SetProjectionMatrix(
    CreateOrthoMatrix(-fx, fx, yOffset - fy, yOffset + fy, radius * 0.5, radius * 5));
  xSrc := (rci.VXStates.ViewPort.Z - SampleSize) div 2;
  ySrc := (rci.VXStates.ViewPort.W - SampleSize) div 2;

  // setup imposter texture
  if destImposter.Texture.Handle = 0 then
  begin
    {$IFDEF USE_OPENGL_DEBUG}
      if GL.GREMEDY_string_marker then
        GL.StringMarkerGREMEDY(22, 'Imposter texture setup');
    {$ENDIF}
    destImposter.PrepareTexture(rci);
    InitializeImpostorTexture(FTextureSize);
  end;

  glPixelTransferf(GL_ALPHA_SCALE, FSamplesAlphaScale);

  // Now render each sample
  curSample := 0;
  for coronaIdx := 0 to Coronas.Count - 1 do
  begin
    corona := Coronas[coronaIdx];
    cameraDirection := XHmgVector;
    RotateVector(cameraDirection, ZHmgPoint, corona.Elevation * cPIdiv180);
    for i := 0 to corona.Samples - 1 do
    begin
      cameraOffset := cameraDirection;
      RotateVector(cameraOffset, YHmgVector, (c2PI * i) / corona.Samples);
      ScaleVector(cameraOffset, -radius * 2);
      rci.VXStates.DepthWriteMask := True;
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

      LM := CreateLookAtMatrix(cameraOffset, NullHmgVector, YHmgVector);
      if Lighting = siblStaticLighting then
        (rci.scene as TVXScene).SetupLights(rci.VXStates.MaxLights);
      rci.PipelineTransformation.SetViewMatrix(MatrixMultiply(
        CreateTranslationMatrix(FBuildOffset.AsVector), LM));
      impostoredObject.Render(rci);
      CheckOpenGLError;

      xDest := (curSample mod FSamplesPerAxis.X) * SampleSize;
      yDest := (curSample div FSamplesPerAxis.X) * SampleSize;

      rci.VXStates.TextureBinding[0, ttTexture2D] :=
        destImposter.Texture.Handle;
      glCopyTexSubImage2D(GL_TEXTURE_2D, 0, xDest, yDest, xSrc, ySrc,
        SampleSize, SampleSize);

      Inc(curSample);
    end;
  end;

  // Restore buffer stuff
  glPixelTransferf(GL_ALPHA_SCALE, 1);
  rci.PipelineTransformation.Pop;

  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
  if Lighting = siblStaticLighting then
    (rci.scene as TVXScene).SetupLights(rci.VXStates.MaxLights);
end;

function TVXStaticImposterBuilder.ComputeOptimalTextureSize: TVXPoint;
var
  nbSamples, maxSamples, maxTexSize, baseSize: Integer;
  texDim, bestTexDim: TVXPoint;
  requiredSurface, currentSurface, bestSurface: Integer;
begin
  nbSamples := Coronas.SampleCount;
  if CurrentVXContext = nil then
    maxTexSize := 16 * 1024
  else
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxTexSize);
  maxSamples := Sqr(maxTexSize div SampleSize);
  if nbSamples < maxSamples then
  begin
    Result.X := -1;
    Result.Y := -1;
  end;
  requiredSurface := nbSamples * SampleSize * SampleSize;
  baseSize := RoundUpToPowerOf2(SampleSize);

  // determine the texture size with the best fill ratio
  bestSurface := MaxInt;
  texDim.X := baseSize;
  while texDim.X <= maxTexSize do
  begin
    texDim.Y := baseSize;
    while texDim.Y <= maxTexSize do
    begin
      currentSurface := texDim.X * texDim.Y;
      if currentSurface >= requiredSurface then
      begin
        if currentSurface < bestSurface then
        begin
          bestTexDim := texDim;
          bestSurface := currentSurface;
        end
        else if (currentSurface = bestSurface)
          and (MaxInteger(texDim.X, texDim.Y) < MaxInteger(bestTexDim.X,
          bestTexDim.Y)) then
        begin
          bestTexDim := texDim;
          bestSurface := currentSurface;
        end
        else
          Break;
      end;
      texDim.Y := texDim.Y * 2;
    end;
    texDim.X := texDim.X * 2;
  end;
  Assert(bestSurface <> MaxInt);

  Result := bestTexDim;
end;

function TVXStaticImposterBuilder.TextureFillRatio: Single;
var
  texDim: TVXPoint;
begin
  texDim := ComputeOptimalTextureSize;
  Result := (Coronas.SampleCount * SampleSize * SampleSize) / (texDim.X *
    texDim.Y);
end;

// ----------
// ---------- TVXDynamicImposterBuilder ----------
// ----------

constructor TVXDynamicImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FTolerance := 0.1;
  FUseMatrixError := True;
  FMinTexSize := 16;
  FMaxTexSize := 64;
end;

destructor TVXDynamicImposterBuilder.Destroy;
begin
  inherited;
end;

{
procedure TVXDynamicImposterBuilder.DoRender(var rci : TVXRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  i, size, Left, Top, Width, Height : Integer;
  imposter : TVXImposter;
  mat, projection, modelview : TMatrix;
  BackColor, pos, temp : TVector;
  rad : Single;
  AABB : TAABB;
begin
  if (csDesigning in ComponentState) or not FEnabled then exit;

  // Store the current clear color
  glGetFloatv(GL_COLOR_CLEAR_VALUE, @BackColor[0]);

  // Get the projection matrix
  if UseMatrixError then
    glGetFloatv(GL_PROJECTION_MATRIX, @projection);

  // Render and save each imposter as required
  for i:=0 to FImposterRegister.Count-1 do begin
    imposter:=TVXImposter(FImposterRegister[i]);
    if (imposter.Count = 0) or not imposter.Visible then Continue;
    imposter.FDrawImposter:=True;

    if VectorDistance(imposter.AbsolutePosition, rci.cameraPosition)<FMinDistance then begin
      imposter.FDrawImposter:=False;
      Continue;
    end;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glMultMatrixf(@imposter.AbsoluteMatrixAsAddress[0]);
    glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);

    // Get imposters dimensions
    AABB:=imposter.AxisAlignedBoundingBox;
    rad:=MaxFloat(AABB.max[0],AABB.max[1],AABB.max[2]);
    pos:=imposter.AbsolutePosition;
    temp:=Scene.CurrentBuffer.Camera.AbsoluteEyeSpaceVector(0,1,0);
    temp:=VectorAdd(pos, VectorScale(temp,rad));
    pos:=Scene.CurrentBuffer.WorldToScreen(pos);
    temp:=Scene.CurrentBuffer.WorldToScreen(temp);
    size:=RoundUpToPowerOf2(Round(2*VectorDistance(pos,temp)));
    if size<FMinTexSize then size:=FMinTexSize;
    if size>FMaxTexSize then begin
      imposter.FDrawImposter:=False;
      glPopMatrix;
      Continue;
    end;
    temp:=pos;
    temp[0]:=temp[0]+size;
    temp:=Scene.CurrentBuffer.ScreenToWorld(temp);
    Imposter.FSize:=VectorDistance(imposter.AbsolutePosition,temp);
    imposter.FTexSize:=size;
    pos[0]:=pos[0]-size/2;
    pos[1]:=pos[1]-size/2;

    // Calculate error
    if UseMatrixError then begin
      mat:=MatrixMultiply(modelview, projection);
      if (imposter.CalcError(mat)>FTolerance) or (imposter.FInvalidated) then
        imposter.FOldMatrix:=mat
      else begin
        glPopMatrix;
        Continue;
      end;
    end;

    // Clear to transparent black
    glClearColor(0,0,0,0);

    // Determine size by color (for debug purposes)
    (*case size of
      16 : glClearColor(0,0,1,0.1);
      32 : glClearColor(0,1,0,0.1);
      64 : glClearColor(1,0,0,0.1);
      128 : glClearColor(1,1,0,0.1);
      256 : glClearColor(1,0,1,0.1);
    end;// *)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

    // Render the imposter's children
    imposter.RenderChildren(0, imposter.Count-1, rci);
    glPopMatrix;

    // Select the imposters texture (will create the handle if null)
    glBindTexture(GL_TEXTURE_2D,imposter.TextureHandle);

    // Check for resize or invalidation
    if (imposter.FTexSize <> imposter.FLastTexSize)
    or (imposter.FInvalidated) then begin
      glTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      imposter.FLastTexSize:=imposter.FTexSize;
      imposter.FInvalidated:=False;
      imposter.NotifyChange(self);
    end;

    // Get the region to be copied from the frame buffer
    Left:=Floor(pos[0]); Top:=Floor(pos[1]);
    Width:=Size; Height:=Size;
    // ... Perhaps some region clamping here?

    // Copy the frame buffer pixels to the imposter texture
    glCopyTexSubImage2d(GL_TEXTURE_2D, 0, 0, 0,
                        Left, Top, Width, Height);
  end;

  // Reset the clear color and clear color, depth and stencil buffers
  glClearColor(BackColor[0],BackColor[1],BackColor[2],BackColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;
}

procedure TVXDynamicImposterBuilder.SetMinDistance(const AValue: Single);
begin
  if AValue <> FMinDistance then
  begin
    FMinDistance := AValue;
    NotifyChange(Self);
  end;
end;

// ----------
// ---------- TVXImposter ----------
// ----------

constructor TVXImposter.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TVXImposter.Destroy;
begin
  Builder := nil;
  ImpostoredObject := nil;
  inherited;
end;

procedure TVXImposter.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = Builder then
      Builder := nil;
    if AComponent = ImpostoredObject then
      ImpostoredObject := nil;
  end;
  inherited;
end;

procedure TVXImposter.DoRender(var ARci: TVXRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  camPos: TVector;
  imposter: TImposter;
begin
  if ARenderSelf and Assigned(Builder) and Assigned(ImpostoredObject) then
  begin
    imposter := Builder.ImposterFor(ImpostoredObject);
    if Assigned(imposter) and (imposter.Texture.Handle <> 0) then
    begin
      camPos := AbsoluteToLocal(ARci.cameraPosition);
      imposter.BeginRender(ARci);
      imposter.Render(ARci, NullHmgPoint, camPos, Scale.MaxXYZ);
      imposter.EndRender(ARci);
    end;
  end;
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TVXImposter.SetBuilder(const AValue: TVXImposterBuilder);
begin
  if AValue <> FBuilder then
  begin
    if Assigned(FBuilder) then
    begin
      FBuilder.RemoveFreeNotification(Self);
      FBuilder.UnRequestImposterFor(ImpostoredObject);
    end;
    FBuilder := AValue;
    if Assigned(FBuilder) then
    begin
      FBuilder.FreeNotification(Self);
      FBuilder.RequestImposterFor(ImpostoredObject);
    end;
  end;
end;

procedure TVXImposter.SetImpostoredObject(const AValue: TVXBaseSceneObject);
begin
  if AValue <> FImpostoredObject then
  begin
    if Assigned(Builder) then
      FBuilder.UnRequestImposterFor(ImpostoredObject);
    FImpostoredObject := AValue;
    if Assigned(Builder) then
      FBuilder.RequestImposterFor(ImpostoredObject);
  end;
end;

{
function TVXImposter.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result:=NullHMGVector;
end;

function TVXImposter.CalcError(NewMatrix : TMatrix) : Single;
var
   i : Integer;
   mat : TMatrix;
   err : Single;
begin
   err:=0;
   mat:=NewMatrix;
   InvertMatrix(mat);
   mat:=MatrixMultiply(FOldMatrix, mat);
   for i:=0 to 3 do mat[i][i]:=mat[i][i]-1;
   for i:=0 to 15 do err:=err+Abs(mat[i div 4][i mod 4]);
   Result:=err;
end;

function TVXImposter.GetTextureHandle: Cardinal;
begin
  if FTextureHandle = 0 then
    glGenTextures(1, @FTextureHandle);
  Result:=FTextureHandle;
end;

procedure TVXImposter.Invalidate;
begin
  FInvalidated:=True;
end;
}
initialization

  //  RegisterClasses([TVXDynamicImposterBuilder, TVXImposter]);
  RegisterClasses([TVXImposter]);

end.


