// GLImposter
{: Imposter building and rendering implementation for GLScene.<p>

   <b>History : </b><font size=-1><ul>
      <li>04/05/04 - EG - Reworked architecture
      <li>14/04/04 - SG - Fixed texture clamping for old cards and 
                          switched to GL_NEAREST texture sampling.
      <li>24/03/04 - SG - Initial.
   </ul></font><p>
}
unit GLImposter;

interface

uses
  Classes, GLScene, GLContext, GLTexture, VectorTypes, VectorGeometry,
  GeometryBB, GLMisc, PersistentClasses, GLCrossPlatform;

type
   // TImposterOptions
   //
   TImposterOption = (impoBlended, impoAlphaTest, impoNearestFiltering);
   TImposterOptions = set of TImposterOption;

const
   cDefaultImposterOptions = [impoBlended, impoAlphaTest];

type
   TGLImposterBuilder = class;

   // TImposter
   //
   {: Base class for imposters manipulation and handling.<br>
      Rendering imposters is performed by three methods, BeginRender must
      be invoked first, then Render for each of the impostr
      This class assumes a single impostor per texture. }
   TImposter = class (TObject)
      private
	      { Private Declarations }
         FRequestCount : Integer;
         FBuilder : TGLImposterBuilder;
         FTexture : TGLTextureHandle;
         FImpostoredObject : TGLBaseSceneObject;
         FAspectRatio : Single;

		protected
			{ Protected Declarations }
         FVx, FVy : TAffineVector;
         FStaticOffset : TVector;
         FQuad : array [0..3] of TAffineVector;

         procedure PrepareTexture; dynamic;

      public
	      { Public Declarations }
         constructor Create(aBuilder : TGLImposterBuilder); virtual;
         destructor Destroy; override;

         procedure BeginRender(var rci : TRenderContextInfo); virtual;
         procedure Render(var rci : TRenderContextInfo;
                          const objPos, localCameraPos : TVector;
                          size : Single); virtual;
         procedure EndRender(var rci : TRenderContextInfo); virtual;

         property AspectRatio : Single read FAspectRatio write FAspectRatio;
         property Builder : TGLImposterBuilder read FBuilder;
         property Texture : TGLTextureHandle read FTexture;
         property ImpostoredObject : TGLBaseSceneObject read FImpostoredObject write FImpostoredObject;
   end;

   // TGLImposterBuilder
   //
   {: Abstract ImposterBuilder class. }
   TGLImposterBuilder = class (TGLUpdateAbleComponent)
      private
	      { Private Declarations }
         FBackColor : TGLColor;
         FBuildOffset : TGLCoordinates;
         FImposterRegister : TPersistentObjectList;
         FRenderPoint : TGLRenderPoint;
         FImposterOptions : TImposterOptions;
         FAlphaTreshold : Single;

      protected
			{ Protected Declarations }
         procedure SetRenderPoint(val : TGLRenderPoint);
         procedure RenderPointFreed(Sender : TObject);
         procedure SetBackColor(val : TGLColor);
         procedure SetBuildOffset(val : TGLCoordinates);

         procedure InitializeImpostorTexture(const textureSize : TGLPoint);

         property ImposterRegister : TPersistentObjectList read FImposterRegister;
         procedure UnregisterImposter(imposter : TImposter);

         function CreateNewImposter : TImposter; virtual;
         procedure PrepareImposters(Sender : TObject; var rci : TRenderContextInfo); virtual; abstract;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure NotifyChange(Sender : TObject); override;

         {: Returns a valid imposter for the specified object.<p>
            Imposter must have been requested first, and the builder given
            an opportunity to prepare it before it can be available. } 
         function ImposterFor(impostoredObject : TGLBaseSceneObject) : TImposter;
         {: Request an imposter to be prepared for the specified object. }
         procedure RequestImposterFor(impostoredObject : TGLBaseSceneObject);
         {: Tells the imposter for the specified object is no longer needed. }
         procedure UnRequestImposterFor(impostoredObject : TGLBaseSceneObject);

      published
	      { Published Declarations }
         property RenderPoint : TGLRenderPoint read FRenderPoint write SetRenderPoint;
         property BackColor : TGLColor read FBackColor write SetBackColor;
         property BuildOffset : TGLCoordinates read FBuildOffset write SetBuildOffset;
         property ImposterOptions : TImposterOptions read FImposterOptions write FImposterOptions default cDefaultImposterOptions;
         property AlphaTreshold : Single read FAlphaTreshold write FAlphaTreshold;
   end;

	// TGLStaticImposterBuilderCorona
	//
	TGLStaticImposterBuilderCorona = class (TCollectionItem)
	   private
	      { Private Declarations }
         FSamples : Integer;
         FElevation : Single;
         FSampleBaseIndex : Integer;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetSamples(val : Integer);
         procedure SetElevation(val : Single);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

	   published
	      { Published Declarations }
         property Samples : Integer read FSamples write SetSamples default 8;
         property Elevation : Single read FElevation write SetElevation;
	end;

   TCoronaTangentLookup = record
      minTan, maxTan : Single;
      corona : TGLStaticImposterBuilderCorona;
   end;

	// TGLStaticImposterBuilderCoronas
	//
	TGLStaticImposterBuilderCoronas = class (TOwnedCollection)
	   private
	      { Private Declarations }
         FCoronaTangentLookup : array of TCoronaTangentLookup;

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLStaticImposterBuilderCorona);
	      function GetItems(index : Integer) : TGLStaticImposterBuilderCorona;
         procedure Update(Item: TCollectionItem); override;

         procedure PrepareSampleBaseIndices;
         procedure PrepareCoronaTangentLookup;
         function CoronaForElevationTangent(aTangent : Single) : TGLStaticImposterBuilderCorona;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TPersistent);

         function Add : TGLStaticImposterBuilderCorona; overload;
         function Add(const elevation : Single; samples : Integer) : TGLStaticImposterBuilderCorona; overload;
	      property Items[index : Integer] : TGLStaticImposterBuilderCorona read GetItems write SetItems; default;
         function SampleCount : Integer;

         procedure NotifyChange; virtual;
         procedure EndUpdate; override;
   end;

   // TStaticImposter
   //
   {: Imposter class whose texture contains several views from different angles. }
   TStaticImposter = class (TImposter)
      private
	      { Private Declarations }

		protected
			{ Protected Declarations }

      public
	      { Public Declarations }
         procedure Render(var rci : TRenderContextInfo;
                          const objPos, localCameraPos : TVector;
                          size : Single); override;
   end;

   // TSIBLigthing
   //
   TSIBLigthing = (siblNoLighting, siblStaticLighting, siblLocalLighting);

   // TGLStaticImposterBuilder
   //
   {: Builds imposters whose texture is a catalog of prerendered views. }
   TGLStaticImposterBuilder = class (TGLImposterBuilder)
      private
	      { Private Declarations }
         FCoronas : TGLStaticImposterBuilderCoronas;
         FSampleSize : Integer;
         FTextureSize : TGLPoint;
         FSamplesPerAxis : TGLPoint;
         FInvSamplesPerAxis : TVector2f;
         FSamplingRatioBias, FInvSamplingRatioBias : Single;
         FLighting : TSIBLigthing;

      protected
			{ Protected Declarations }
         procedure SetCoronas(val : TGLStaticImposterBuilderCoronas);
         procedure SetSampleSize(val : Integer);
         procedure SetSamplingRatioBias(val : Single);
         procedure SetLighting(val : TSIBLigthing);

         {: Computes the optimal texture size that would be able to hold all samples. }
         function ComputeOptimalTextureSize : TGLPoint;

         function CreateNewImposter : TImposter; override;
         procedure PrepareImposters(Sender : TObject; var rci : TRenderContextInfo); override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         {: Render imposter texture.<p>
            Buffer and object must be compatible, RC must have been activated. }
         procedure Render(var rci : TRenderContextInfo;
                          impostoredObject : TGLBaseSceneObject;
                          destImposter : TImposter);
         {: Ratio (0..1) of the texture that will be used by samples.<p>
            If this value is below 1, you're wasting texture space and may
            as well increase the number of samples. }
         function TextureFillRatio : Single;

         {: Meaningful only after imposter texture has been prepared. }
         property TextureSize : TGLPoint read FTextureSize;
         property SamplesPerAxis : TGLPoint read FSamplesPerAxis;

      published
	      { Published Declarations }
         property Coronas : TGLStaticImposterBuilderCoronas read FCoronas write SetCoronas;
         property SampleSize : Integer read FSampleSize write SetSampleSize default 32;
         property SamplingRatioBias : Single read FSamplingRatioBias write SetSamplingRatioBias;
         property Lighting : TSIBLigthing read FLighting write FLighting default siblStaticLighting;
   end;

   // TGLDynamicImposterBuilder
   //
   TGLDynamicImposterBuilder = class (TGLImposterBuilder)
      private
	      { Private Declarations }
         FMinTexSize, FMaxTexSize : Integer;
         FMinDistance, FTolerance : Single;
         FUseMatrixError : Boolean;

      protected
			{ Protected Declarations }
         procedure SetMinDistance(const Value : Single);

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
{         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override; }

      published
	      { Published Declarations }
         property MinTexSize : Integer read FMinTexSize write FMinTexSize;
         property MaxTexSize : Integer read FMaxTexSize write FMaxTexSize;
         property MinDistance : Single read FMinDistance write SetMinDistance;
         property Tolerance : Single read FTolerance write FTolerance;
         property UseMatrixError : Boolean read FUseMatrixError write FUseMatrixError;

   end;

   // TGLImposter
   //
   TGLImposter = class(TGLImmaterialSceneObject)
      private
	      { Private Declarations }
         FBuilder : TGLImposterBuilder;
         FImpostoredObject : TGLBaseSceneObject;

      protected
			{ Protected Declarations }
         procedure SetBuilder(const val : TGLImposterBuilder);
         procedure SetImpostoredObject(const val : TGLBaseSceneObject);

{         function CalcError(NewMatrix : TMatrix) : Single;
         function GetTextureHandle : Cardinal;}

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
{         procedure Invalidate;
         function AxisAlignedDimensionsUnscaled : TVector; override; }

      published
	      { Published Declarations }
         property Builder : TGLImposterBuilder read FBuilder write SetBuilder;
         property ImpostoredObject : TGLBaseSceneObject read FImpostoredObject write SetImpostoredObject;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

// ----------
// ---------- TImposter ----------
// ----------

// Create
//
constructor TImposter.Create(aBuilder : TGLImposterBuilder);
begin
   inherited Create;
   FBuilder:=aBuilder;
   FTexture:=TGLTextureHandle.Create;
   aBuilder.FImposterRegister.Add(Self);
   FAspectRatio:=1;
end;

// Destroy
//
destructor TImposter.Destroy;
begin
   if Assigned(FBuilder) then
      FBuilder.UnregisterImposter(Self);
   FTexture.Free;
   inherited;
end;

// PrepareTexture
//
procedure TImposter.PrepareTexture;
var
   i : Integer;
begin
   if FTexture.Handle<>0 then Exit;

   FTexture.AllocateHandle;
   glBindTexture(GL_TEXTURE_2D, FTexture.Handle);
   if GL_EXT_texture_edge_clamp then
      i:=GL_CLAMP_TO_EDGE
   else i:=GL_CLAMP;
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
 	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

// BeginRender
//
procedure TImposter.BeginRender(var rci : TRenderContextInfo);
var
   mat : TMatrix;
   filter : TGLEnum;
   fx, fy : Single;
begin
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   glDisable(GL_CULL_FACE);
   glEnable(GL_TEXTURE_2D);

   if impoAlphaTest in Builder.ImposterOptions then begin
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GEQUAL, Builder.AlphaTreshold);
   end else glDisable(GL_ALPHA_TEST);

   if impoBlended in Builder.ImposterOptions then begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   end else glDisable(GL_BLEND);

   rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, Texture.Handle);

   if impoNearestFiltering in Builder.ImposterOptions then
      filter:=GL_NEAREST
   else filter:=GL_LINEAR;
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
 	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

   glGetFloatv(GL_MODELVIEW_MATRIX, @mat[0][0]);
   FVx[0]:=mat[0][0];   FVy[0]:=mat[0][1];
   FVx[1]:=mat[1][0];   FVy[1]:=mat[1][1];
   FVx[2]:=mat[2][0];   FVy[2]:=mat[2][1];
   NormalizeVector(FVx);
   NormalizeVector(FVy);

   fx:=Sqrt(FAspectRatio);
   fy:=1/fx;

   FQuad[0]:=VectorCombine(FVx, FVy,  fx,  fy);
   FQuad[1]:=VectorCombine(FVx, FVy, -fx,  fy);
   FQuad[2]:=VectorCombine(FVx, FVy, -fx, -fy);
   FQuad[3]:=VectorCombine(FVx, FVy,  fx, -fy);
end;

// Render
//
procedure TImposter.Render(var rci : TRenderContextInfo;
                           const objPos, localCameraPos : TVector;
                           size : Single);
begin
   glBegin(GL_QUADS);
      glTexCoord2f(1, 1);  glVertex3f(FQuad[0][0]*size+objPos[0], FQuad[0][1]*size+objPos[1], FQuad[0][2]*size+objPos[2]);
      glTexCoord2f(0, 1);  glVertex3f(FQuad[1][0]*size+objPos[0], FQuad[1][1]*size+objPos[1], FQuad[1][2]*size+objPos[2]);
      glTexCoord2f(0, 0);  glVertex3f(FQuad[2][0]*size+objPos[0], FQuad[2][1]*size+objPos[1], FQuad[2][2]*size+objPos[2]);
      glTexCoord2f(1, 0);  glVertex3f(FQuad[3][0]*size+objPos[0], FQuad[3][1]*size+objPos[1], FQuad[3][2]*size+objPos[2]);
   glEnd;
end;

// EndRender
//
procedure TImposter.EndRender(var rci : TRenderContextInfo);
begin
   glPopAttrib;
end;

// ----------
// ---------- TGLImposterBuilder ----------
// ----------

// Create
//
constructor TGLImposterBuilder.Create(AOwner : TComponent);
begin
   inherited;
   FImposterRegister:=TPersistentObjectList.Create;
   FBackColor:=TGLColor.CreateInitialized(Self, clrTransparent);
   FBuildOffset:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint);
   FImposterOptions:=cDefaultImposterOptions;
   FAlphaTreshold:=0.5;
end;

// Destroy
//
destructor TGLImposterBuilder.Destroy;
var
   i : Integer;
begin
   FBuildOffset.Free;
   FBackColor.Free;
   for i:=0 to FImposterRegister.Count-1 do
      TImposter(FImposterRegister[i]).FBuilder:=nil;
   FImposterRegister.CleanFree;
   inherited;
end;

// Notification
//
procedure TGLImposterBuilder.Notification(AComponent: TComponent; Operation: TOperation);
var
   i : Integer;
   imposter : TImposter;
begin
   if Operation=opRemove then begin
      if AComponent=FRenderPoint then
         FRenderPoint:=nil;
      for i:=FImposterRegister.Count-1 downto 0 do begin
         imposter:=TImposter(FImposterRegister[i]);
         if imposter.ImpostoredObject=AComponent then begin
            imposter.Free;
            Break;
         end;
      end;
   end;
   inherited;
end;

// CreateNewImposter
//
function TGLImposterBuilder.CreateNewImposter : TImposter;
begin
   Result:=TImposter.Create(Self);
end;

// NotifyChange
//
procedure TGLImposterBuilder.NotifyChange(Sender : TObject);
var
   i : Integer;
begin
   for i:=0 to FImposterRegister.Count-1 do
      TImposter(FImposterRegister[i]).Texture.DestroyHandle;
   inherited;
end;

// ImposterFor
//
function TGLImposterBuilder.ImposterFor(impostoredObject : TGLBaseSceneObject) : TImposter;
var
   i : Integer;
begin
   for i:=0 to FImposterRegister.Count-1 do begin
      Result:=TImposter(FImposterRegister[i]);
      if Result.ImpostoredObject=impostoredObject then
         Exit;
   end;
   Result:=nil;
end;

// RequestImposterFor
//
procedure TGLImposterBuilder.RequestImposterFor(impostoredObject : TGLBaseSceneObject);
var
   imposter : TImposter;
begin
   if impostoredObject=nil then Exit;
   imposter:=ImposterFor(impostoredObject);
   if imposter=nil then begin
      imposter:=CreateNewImposter;
      imposter.ImpostoredObject:=impostoredObject;
   end;
   Inc(imposter.FRequestCount);
end;

procedure TGLImposterBuilder.UnRequestImposterFor(impostoredObject : TGLBaseSceneObject);
var
   imposter : TImposter;
begin
   if impostoredObject=nil then Exit;
   imposter:=ImposterFor(impostoredObject);
   if imposter<>nil then begin
      Dec(imposter.FRequestCount);
      if imposter.FRequestCount=0 then
         imposter.Free;
   end;
end;

// SetRenderPoint
//
procedure TGLImposterBuilder.SetRenderPoint(val : TGLRenderPoint);
begin
   if val<>FRenderPoint then begin
      if Assigned(FRenderPoint) then begin
         FRenderPoint.RemoveFreeNotification(Self);
         FRenderPoint.UnRegisterCallBack(PrepareImposters);
      end;
      FRenderPoint:=val;
      if Assigned(FRenderPoint) then begin
         FRenderPoint.FreeNotification(Self);
         FRenderPoint.RegisterCallBack(PrepareImposters, RenderPointFreed);
      end;
   end;
end;

// RenderPointFreed
//
procedure TGLImposterBuilder.RenderPointFreed(Sender : TObject);
begin
   FRenderPoint:=nil;
end;

// SetBackColor
//
procedure TGLImposterBuilder.SetBackColor(val : TGLColor);
begin
   FBackColor.Assign(val);
end;

// SetBuildOffset
//
procedure TGLImposterBuilder.SetBuildOffset(val : TGLCoordinates);
begin
   FBuildOffset.Assign(val);
end;

// InitializeImpostorTexture
//
procedure TGLImposterBuilder.InitializeImpostorTexture(const textureSize : TGLPoint);
var
   memBuffer : Pointer;
begin
   memBuffer:=GetMemory(textureSize.X*textureSize.Y*4);
   try
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, textureSize.X, textureSize.Y, 0,
                   GL_RGBA, GL_UNSIGNED_BYTE, memBuffer);
   finally
      FreeMemory(memBuffer);
   end;
end;

// UnregisterImposter
//
procedure TGLImposterBuilder.UnregisterImposter(imposter : TImposter);
begin
   if imposter.Builder=Self then begin
      FImposterRegister.Remove(imposter);
      imposter.FBuilder:=nil;
   end;
end;

// ----------
// ---------- TGLStaticImposterBuilderCorona ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilderCorona.Create(Collection : TCollection);
begin
   inherited;
   FSamples:=8;
end;

// Destroy
//
destructor TGLStaticImposterBuilderCorona.Destroy;
begin
   inherited;
end;

// Assign
//
procedure TGLStaticImposterBuilderCorona.Assign(Source: TPersistent);
begin
   if Source is TGLStaticImposterBuilderCorona then begin
      FSamples:=TGLStaticImposterBuilderCorona(Source).FSamples;
      FElevation:=TGLStaticImposterBuilderCorona(Source).FElevation;
   end;
   inherited;
end;

// GetDisplayName
//
function TGLStaticImposterBuilderCorona.GetDisplayName : String;
begin
   Result:=Format('%.1f° / %d samples', [Elevation, Samples]);
end;

// SetSamples
//
procedure TGLStaticImposterBuilderCorona.SetSamples(val : Integer);
begin
   if val<>FSamples then begin
      FSamples:=val;
      if FSamples<1 then FSamples:=1;
      (Collection as TGLStaticImposterBuilderCoronas).NotifyChange;
   end;
end;

// SetElevation
//
procedure TGLStaticImposterBuilderCorona.SetElevation(val : Single);
begin
   if val<>FElevation then begin
      FElevation:=ClampValue(val, -89, 89);
      (Collection as TGLStaticImposterBuilderCoronas).NotifyChange;
   end;
end;

// ----------
// ---------- TGLStaticImposterBuilderCoronas ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilderCoronas.Create(AOwner : TPersistent);
begin
   inherited Create(AOwner, TGLStaticImposterBuilderCorona);
end;

// Add
//
function TGLStaticImposterBuilderCoronas.Add : TGLStaticImposterBuilderCorona;
begin
	Result:=(inherited Add) as TGLStaticImposterBuilderCorona;
end;

// Add (elevation, samples)
//
function TGLStaticImposterBuilderCoronas.Add(const elevation : Single;
                              samples : Integer) : TGLStaticImposterBuilderCorona;
begin
	Result:=(inherited Add) as TGLStaticImposterBuilderCorona;
   Result.Elevation:=elevation;
   Result.Samples:=samples;
end;

// SetItems
//
procedure TGLStaticImposterBuilderCoronas.SetItems(index : Integer; const val : TGLStaticImposterBuilderCorona);
begin
   inherited Items[index]:=val;
end;

// GetItems
//
function TGLStaticImposterBuilderCoronas.GetItems(index : Integer) : TGLStaticImposterBuilderCorona;
begin
   Result:=TGLStaticImposterBuilderCorona(inherited Items[index]);
end;

// Update
//
procedure TGLStaticImposterBuilderCoronas.Update(Item: TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// NotifyChange
//
procedure TGLStaticImposterBuilderCoronas.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLStaticImposterBuilderCoronas.EndUpdate;
begin
   inherited;
   NotifyChange;
end;

// SampleCount
//
function TGLStaticImposterBuilderCoronas.SampleCount : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to Count-1 do
      Result:=Result+Items[i].Samples;
end;

// PrepareSampleBaseIndices
//
procedure TGLStaticImposterBuilderCoronas.PrepareSampleBaseIndices;
var
   p, i : Integer;
begin
   p:=0;
   for i:=0 to Count-1 do begin
      Items[i].FSampleBaseIndex:=p;
      Inc(p, Items[i].Samples);
   end;
end;

// PrepareCoronaTangentLookup
//
procedure TGLStaticImposterBuilderCoronas.PrepareCoronaTangentLookup;
var
   i, j : Integer;
   corona : TGLStaticImposterBuilderCorona;
   boundary : Single;
begin
   SetLength(FCoronaTangentLookup, Count);
   // place them in the array and sort by ascending elevation
   for i:=0 to Count-1 do
      FCoronaTangentLookup[i].corona:=Items[i];
   for i:=0 to Count-2 do for j:=i+1 to Count-1 do
      if FCoronaTangentLookup[j].corona.Elevation<FCoronaTangentLookup[i].corona.Elevation then begin
         corona:=FCoronaTangentLookup[j].corona;
         FCoronaTangentLookup[j].corona:=FCoronaTangentLookup[i].corona;
         FCoronaTangentLookup[i].corona:=corona;
      end;
   // adjust min max then intermediate boundaries
   FCoronaTangentLookup[0].minTan:=-1e30;
   FCoronaTangentLookup[Count-1].minTan:=1e30;
   for i:=0 to Count-2 do begin
      boundary:=Tan((0.5*cPIdiv180)*( FCoronaTangentLookup[i].corona.Elevation
                                     +FCoronaTangentLookup[i+1].corona.Elevation));
      FCoronaTangentLookup[i].maxTan:=boundary;
      FCoronaTangentLookup[i+1].minTan:=boundary;
   end;
end;

// CoronaForElevationTangent
//
function TGLStaticImposterBuilderCoronas.CoronaForElevationTangent(aTangent : Single) : TGLStaticImposterBuilderCorona;
var
   i, n : Integer;
begin
   n:=High(FCoronaTangentLookup);
   if (n=0) or (aTangent<=FCoronaTangentLookup[0].maxTan) then
      Result:=FCoronaTangentLookup[0].corona
   else if aTangent>FCoronaTangentLookup[n].minTan then
      Result:=FCoronaTangentLookup[n].corona
   else begin
      Result:=FCoronaTangentLookup[1].corona;
      for i:=2 to n-2 do begin
         if aTangent<=FCoronaTangentLookup[i].minTan then Break;
         Result:=FCoronaTangentLookup[i].corona;
      end;
   end;
end;

// ----------
// ---------- TStaticImposter ----------
// ----------

// Render
//
procedure TStaticImposter.Render(var rci : TRenderContextInfo;
                                 const objPos, localCameraPos : TVector;
                                 size : Single);
var
   azimuthAngle : Single;
   i : Integer;
   x, y : Word;
   bestCorona : TGLStaticImposterBuilderCorona;
   tx, ty, tdx, tdy, s : Single;
   siBuilder : TGLStaticImposterBuilder;
   locQuad : array [0..3] of TAffineVector;
begin                  // inherited; exit;
   siBuilder:=TGLStaticImposterBuilder(Builder);

   // determine closest corona
   bestCorona:=siBuilder.Coronas.CoronaForElevationTangent(
                     localCameraPos[1]/VectorLength(localCameraPos[0], localCameraPos[2]));

   // determine closest sample in corona
   azimuthAngle:=FastArcTan2(localCameraPos[2], localCameraPos[0])+cPI;
   i:=Round(azimuthAngle*bestCorona.Samples*cInv2PI);
   if i<0 then
      i:=0
   else if i>=bestCorona.Samples then
      i:=bestCorona.Samples-1;
   i:=bestCorona.FSampleBaseIndex+i;

   tdx:=siBuilder.FInvSamplesPerAxis[0];
   tdy:=siBuilder.FInvSamplesPerAxis[1];
   DivMod(i, siBuilder.SamplesPerAxis.X, y, x);
   tx:=tdx*x;
   ty:=tdy*y;
   s:=Size*siBuilder.FInvSamplingRatioBias;

   for i:=0 to 3 do begin
      locQuad[i][0]:=FQuad[i][0]*s+objPos[0]-FStaticOffset[0];
      locQuad[i][1]:=FQuad[i][1]*s+objPos[1]-FStaticOffset[1];
      locQuad[i][2]:=FQuad[i][2]*s+objPos[2]-FStaticOffset[2];
   end;

   // then render it
   glBegin(GL_QUADS);
      glTexCoord2f(tx+tdx, ty+tdy); glVertex3fv(@locQuad[0]);
      glTexCoord2f(tx,     ty+tdy); glVertex3fv(@locQuad[1]);
      glTexCoord2f(tx,     ty);     glVertex3fv(@locQuad[2]);
      glTexCoord2f(tx+tdx, ty);     glVertex3fv(@locQuad[3]);
   glEnd;
end;

// ----------
// ---------- TGLStaticImposterBuilder ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilder.Create(AOwner : TComponent);
begin
   inherited;
   FCoronas:=TGLStaticImposterBuilderCoronas.Create(Self);
   FCoronas.Add;
   FSampleSize:=16;
   FSamplingRatioBias:=1;
   FInvSamplingRatioBias:=1;
   FLighting:=siblStaticLighting;
end;

// Destroy
//
destructor TGLStaticImposterBuilder.Destroy;
begin
   FCoronas.Free;
   inherited;
end;

// CreateNewImposter
//
function TGLStaticImposterBuilder.CreateNewImposter : TImposter;
begin
   Result:=TStaticImposter.Create(Self);
end;

// SetCoronas
//
procedure TGLStaticImposterBuilder.SetCoronas(val : TGLStaticImposterBuilderCoronas);
begin
   FCoronas.Assign(val);
   NotifyChange(Self);
end;

// SetSampleSize
//
procedure TGLStaticImposterBuilder.SetSampleSize(val : Integer);
begin
   val:=RoundUpToPowerOf2(val);
   if val<8 then val:=8;
   if val>1024 then val:=1024;
   if val<>FSampleSize then begin
      FSampleSize:=val;
      NotifyChange(Self);
   end;
end;

// SetSamplingRatioBias
//
procedure TGLStaticImposterBuilder.SetSamplingRatioBias(val : Single);
begin
   val:=ClampValue(val, 0.1, 10);
   if val<>FSamplingRatioBias then begin
      FSamplingRatioBias:=val;
      FInvSamplingRatioBias:=1/val;
      NotifyChange(Self);
   end;
end;

// SetLighting
//
procedure TGLStaticImposterBuilder.SetLighting(val : TSIBLigthing);
begin
   if val<>FLighting then begin
      FLighting:=val;
      NotifyChange(Self);
   end;
end;

// Render
//
procedure TGLStaticImposterBuilder.Render(var rci : TRenderContextInfo;
            impostoredObject : TGLBaseSceneObject; destImposter : TImposter);
var
   i, coronaIdx, curSample, maxLight : Integer;
   radius : Single;
   cameraDirection, cameraOffset : TVector;
   xDest, xSrc, yDest, ySrc : Integer;
   corona : TGLStaticImposterBuilderCorona;
   fx, fy : Single;
   viewPort : TVector4i;
begin
   Coronas.PrepareCoronaTangentLookup;
   Coronas.PrepareSampleBaseIndices;

   FTextureSize:=ComputeOptimalTextureSize;
   if (FTextureSize.X<=0) and (FTextureSize.Y<=0) then begin
      SampleSize:=SampleSize shr 1;
      Assert(False, 'Too many samples, can''t fit in a texture!');
   end;

   FSamplesPerAxis.X:=FTextureSize.X div SampleSize;
   FSamplesPerAxis.Y:=FTextureSize.Y div SampleSize;
   FInvSamplesPerAxis[0]:=1/FSamplesPerAxis.X;
   FInvSamplesPerAxis[1]:=1/FSamplesPerAxis.Y;

   radius:=impostoredObject.BoundingSphereRadius/SamplingRatioBias;
   glGetIntegerv(GL_MAX_LIGHTS, @maxLight);
   glGetIntegerv(GL_VIEWPORT, @viewPort);

   Assert((viewPort[2]>=SampleSize) and (viewPort[3]>=SampleSize),
          'ViewPort too small to render imposter samples!');

   // Setup the buffer in a suitable fashion for our needs
   glPushAttrib(GL_ENABLE_BIT+GL_COLOR_BUFFER_BIT);
   with FBackColor do
      glClearColor(Red, Green, Blue, Alpha);
   if Lighting=siblNoLighting then
      glDisable(GL_LIGHTING);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   fx:=viewPort[2]/SampleSize;
   fy:=viewPort[3]/SampleSize;
   glOrtho(-radius*fx, radius*fx, -radius*fy, radius*fy, radius*0.5, radius*5);
   xSrc:=(viewPort[2]-SampleSize) div 2;
   ySrc:=(viewPort[3]-SampleSize) div 2;
   destImposter.FStaticOffset:=VectorScale(FBuildOffset.DirectVector, 0.5/(radius*SamplingRatioBias));

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;

   // setup imposter texture
   if destImposter.Texture.Handle=0 then begin
      destImposter.PrepareTexture;
      InitializeImpostorTexture(FTextureSize);
      rci.GLStates.ResetGLCurrentTexture;
   end;

   // Now render each sample
   curSample:=0;
   for coronaIdx:=0 to Coronas.Count-1 do begin
      corona:=Coronas[coronaIdx];
      cameraDirection:=XHmgVector;
      RotateVector(cameraDirection, ZHmgPoint, corona.Elevation*cPIdiv180);
      for i:=0 to corona.Samples-1 do begin
         cameraOffset:=cameraDirection;
         RotateVector(cameraOffset, YHmgVector, (c2PI*i)/corona.Samples);
         ScaleVector(cameraOffset, -radius*2);

         glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);
         glLoadIdentity;
         gluLookAt(cameraOffset[0], cameraOffset[1], cameraOffset[2], 0, 0, 0, 0, 1, 0);
         if Lighting=siblStaticLighting then
            (rci.scene as TGLScene).SetupLights(maxLight);
         glTranslatef(FBuildOffset.X, FBuildOffset.Y, FBuildOffset.Z);
         impostoredObject.Render(rci);

         xDest:=(curSample mod FSamplesPerAxis.X)*SampleSize;
         yDest:=(curSample div FSamplesPerAxis.X)*SampleSize;

         glBindTexture(GL_TEXTURE_2D, destImposter.Texture.Handle);
         glCopyTexSubImage2D(GL_TEXTURE_2D, 0, xDest, yDest, xSrc, ySrc,
                             SampleSize, SampleSize);
         rci.GLStates.ResetGLCurrentTexture;

         Inc(curSample);
      end;
   end;

   // Restore buffer stuff
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_PROJECTION);
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);

   glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);
   if Lighting=siblStaticLighting then
      (rci.scene as TGLScene).SetupLights(maxLight);
end;

// ComputeOptimalTextureSize
//
function TGLStaticImposterBuilder.ComputeOptimalTextureSize : TGLPoint;
var
   nbSamples, maxSamples, maxTexSize, baseSize : Integer;
   texDim, bestTexDim : TGLPoint;
   requiredSurface, currentSurface, bestSurface : Integer;
begin
   nbSamples:=Coronas.SampleCount;
   glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxTexSize);
   maxSamples:=Sqr(maxTexSize div SampleSize);
   if nbSamples<maxSamples then begin
      Result.X:=-1;
      Result.Y:=-1;
   end;
   requiredSurface:=nbSamples*SampleSize*SampleSize;
   baseSize:=RoundUpToPowerOf2(SampleSize);

   // determine the texture size with the best fill ratio
   bestSurface:=MaxInt;
   texDim.X:=baseSize;
   while texDim.X<=maxTexSize do begin
      texDim.Y:=baseSize;
      while texDim.Y<=maxTexSize do begin
         currentSurface:=texDim.X*texDim.Y;
         if currentSurface>=requiredSurface then begin
            if currentSurface<bestSurface then begin
               bestTexDim:=texDim;
               bestSurface:=currentSurface;
            end else if (currentSurface=bestSurface)
                  and (MaxInteger(texDim.X, texDim.Y)<MaxInteger(bestTexDim.X, bestTexDim.Y)) then begin
               bestTexDim:=texDim;
               bestSurface:=currentSurface;
            end else Break;
         end;
         texDim.Y:=texDim.Y*2;
      end;
      texDim.X:=texDim.X*2;
   end;
   Assert(bestSurface<>MaxInt);

   Result:=bestTexDim;
end;

// PrepareImposters
//
procedure TGLStaticImposterBuilder.PrepareImposters(Sender : TObject; var rci : TRenderContextInfo);
var
   i : Integer;
   imp : TStaticImposter;
begin
   for i:=0 to ImposterRegister.Count-1 do begin
      imp:=TStaticImposter(ImposterRegister[i]);
      if (imp.ImpostoredObject<>nil) and (imp.Texture.Handle=0) then begin
         Render(rci, imp.ImpostoredObject, imp);
      end;
   end;
end;

// TextureFillRatio
//
function TGLStaticImposterBuilder.TextureFillRatio : Single;
var
   texDim : TGLPoint;
begin
   texDim:=ComputeOptimalTextureSize;
   Result:=(Coronas.SampleCount*SampleSize*SampleSize)/(texDim.X*texDim.Y);
end;

// ----------
// ---------- TGLDynamicImposterBuilder ----------
// ----------

// Create
//
constructor TGLDynamicImposterBuilder.Create(AOwner : TComponent);
begin
  inherited;
  FTolerance:=0.1;
  FUseMatrixError:=True;
  FMinTexSize:=16;
  FMaxTexSize:=64;
end;

// Destroy
//
destructor TGLDynamicImposterBuilder.Destroy;
begin
  inherited;
end;
{
// DoRender
//
procedure TGLDynamicImposterBuilder.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  i, size, Left, Top, Width, Height : Integer;
  imposter : TGLImposter;
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
    imposter:=TGLImposter(FImposterRegister[i]);
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
// SetMinDistance
//
procedure TGLDynamicImposterBuilder.SetMinDistance(const Value : Single);
begin
  if Value<>FMinDistance then begin
    FMinDistance:=Value;
    NotifyChange(Self);
  end;
end;

// ----------
// ---------- TGLImposter ----------
// ----------

// Create
//
constructor TGLImposter.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// Destroy
//
destructor TGLImposter.Destroy;
begin
   Builder:=nil;
   ImpostoredObject:=nil;
   inherited;
end;

// Notification
//
procedure TGLImposter.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=Builder then Builder:=nil;
      if AComponent=ImpostoredObject then ImpostoredObject:=nil;
   end;
   inherited;
end;

// DoRender
//
procedure TGLImposter.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
   camPos : TVector;
   imposter : TImposter;
begin
   if renderSelf and Assigned(Builder) and Assigned(ImpostoredObject) then begin
      imposter:=Builder.ImposterFor(ImpostoredObject);
      if Assigned(imposter) and (imposter.Texture.Handle<>0) then begin
         camPos:=AbsoluteToLocal(rci.cameraPosition);
         imposter.BeginRender(rci);
         imposter.Render(rci, NullHmgPoint, camPos, Scale.MaxXYZ);
         imposter.EndRender(rci);
      end;
   end;
   if renderChildren then
       Self.RenderChildren(0, Count-1,rci);
end;

// SetBuilder
//
procedure TGLImposter.SetBuilder(const val : TGLImposterBuilder);
begin
   if val<>FBuilder then begin
      if Assigned(FBuilder) then begin
         FBuilder.RemoveFreeNotification(Self);
         FBuilder.UnRequestImposterFor(ImpostoredObject);
      end;
      FBuilder:=val;
      if Assigned(FBuilder) then begin
         FBuilder.FreeNotification(Self);
         FBuilder.RequestImposterFor(ImpostoredObject);
      end;
   end;
end;

// SetImpostoredObject
//
procedure TGLImposter.SetImpostoredObject(const val : TGLBaseSceneObject);
begin
   if val<>FImpostoredObject then begin
      if Assigned(Builder) then
         FBuilder.UnRequestImposterFor(ImpostoredObject);
      FImpostoredObject:=val;
      if Assigned(Builder) then
         FBuilder.RequestImposterFor(ImpostoredObject);
   end;
end;

{
// AxisAlignedDimensionsUnscaled
//
function TGLImposter.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result:=NullHMGVector;
end;

// CalcDifference
//
function TGLImposter.CalcError(NewMatrix : TMatrix) : Single;
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

// GetTextureHandle
//
function TGLImposter.GetTextureHandle: Cardinal;
begin
  if FTextureHandle = 0 then
    glGenTextures(1, @FTextureHandle);
  Result:=FTextureHandle;
end;

// Invalidate
//
procedure TGLImposter.Invalidate;
begin
  FInvalidated:=True;
end;
}
initialization

//  RegisterClasses([TGLDynamicImposterBuilder, TGLImposter]);

end.
