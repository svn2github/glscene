{: GLAnimatedSprite<p>

  A sprite that uses a scrolling texture for animation.<p>

  <b>History : </b><font size=-1><ul>
    <li>13/07/04 - SG - Creation
  </ul></font>
}
unit GLAnimatedSprite;

interface

uses
  Classes, SysUtils, GLScene, VectorGeometry, OpenGL1x, GLTexture, GLUtils,
  PersistentClasses, XCollection, GLMisc;

type
  TSpriteAnimFrame = class;
  TSpriteAnimFrameList = class;
  TSpriteAnimation = class;
  TSpriteAnimationList = class;
  TGLAnimatedSprite = class;

  TSpriteAnimFrame = class(TXCollectionItem)
    private
      FOffsetX,
      FOffsetY,
      FWidth,
      FHeight : Integer;

      procedure DoChanged;

    protected
      procedure SetOffsetX(const Value : Integer);
      procedure SetOffsetY(const Value : Integer);
      procedure SetWidth(const Value : Integer);
      procedure SetHeight(const Value : Integer);
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;

    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;

    published
      property OffsetX : Integer read FOffsetX write SetOffsetX;
      property OffsetY : Integer read FOffsetY write SetOffsetY;
      property Width : Integer read FWidth write SetWidth;
      property Height : Integer read FHeight write SetHeight;

  end;

  TSpriteAnimFrameList = class(TXCollection)
    public
      constructor Create(aOwner : TPersistent); override;
      class function ItemsClass : TXCollectionItemClass; override;

  end;

  TSpriteFrameDimensions = (sfdAuto, sfdManual);

  TSpriteAnimation = class(TXCollectionItem)
    private
      FCurrentFrame,
      FStartFrame,
      FEndFrame,
      FFrameWidth,
      FFrameHeight : Integer;
      FFrames : TSpriteAnimFrameList;
      FLibMaterialName : TGLLibMaterialName;
      FLibMaterialCached : TGLLibMaterial;
      FDimensions: TSpriteFrameDimensions;

      procedure DoChanged;

    protected
      procedure SetCurrentFrame(const Value : Integer);
      procedure SetFrameWidth(const Value : Integer);
      procedure SetFrameHeight(const Value : Integer);
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure SetDimensions(const Value: TSpriteFrameDimensions);
      procedure SetLibMaterialName(const val : TGLLibMaterialName);
      function GetLibMaterialCached : TGLLibMaterial;

    public
      constructor Create(aOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;

      property LibMaterialCached : TGLLibMaterial read GetLibMaterialCached;

    published
      property CurrentFrame : Integer read FCurrentFrame write SetCurrentFrame;
      property StartFrame : Integer read FStartFrame write FStartFrame;
      property EndFrame : Integer read FEndFrame write FEndFrame;
      property FrameWidth : Integer read FFrameWidth write SetFrameWidth;
      property FrameHeight : Integer read FFrameHeight write SetFrameHeight;
      property LibMaterialName : TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
      property Frames : TSpriteAnimFrameList read FFrames;
      property Dimensions : TSpriteFrameDimensions read FDimensions write SetDimensions;

  end;

  TSpriteAnimationList = class(TXCollection)
    public
      constructor Create(aOwner : TPersistent); override;
      class function ItemsClass : TXCollectionItemClass; override;

  end;

  TSpriteAnimationMode = (samNone, samPlayOnce, samLoop, samBounceForward,
                          samBounceBackward, samLoopBackward);

  TGLAnimatedSprite = class(TGLBaseSceneObject)
    private
      FAnimations : TSpriteAnimationList;
      FMaterialLibrary : TGLMaterialLibrary;
      FAnimationIndex,
      FInterval,
      FRotation,
      FPixelRatio : Integer;
      FMirrorU,
      FMirrorV : Boolean;
      FAnimationMode : TSpriteAnimationMode;
      FCurrentFrameDelta : Double;
      FOnFrameChanged : TNotifyEvent;
      FOnEndFrameReached : TNotifyEvent;
      FOnStartFrameReached : TNotifyEvent;

    protected
      procedure DefineProperties(Filer: TFiler); override;
      procedure WriteAnimations(Stream : TStream);
      procedure ReadAnimations(Stream : TStream);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure SetInterval(const val : Integer);
      procedure SetAnimationIndex(const val : Integer);
      procedure SetAnimationMode(const val : TSpriteAnimationMode);
      procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
      procedure SetPixelRatio(const val : Integer);
      procedure SetRotation(const val : Integer);
      procedure SetMirrorU(const val : Boolean);
      procedure SetMirrorV(const val : Boolean);

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure DoProgress(const progressTime : TProgressTimes); override;

      procedure NextFrame;

    published
      property Animations : TSpriteAnimationList read FAnimations;
      property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
      property Interval : Integer read FInterval write SetInterval;
      property AnimationIndex : Integer read FAnimationIndex write SetAnimationIndex;
      property AnimationMode : TSpriteAnimationMode read FAnimationMode write SetAnimationMode;
      property PixelRatio : Integer read FPixelRatio write SetPixelRatio;
      property Rotation : Integer read FRotation write SetRotation;
      property MirrorU : Boolean read FMirrorU write SetMirrorU;
      property MirrorV : Boolean read FMirrorV write SetMirrorV;

      property Position;
      property Scale;

      property OnFrameChanged : TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
      property OnEndFrameReached : TNotifyEvent read FOnEndFrameReached write FOnEndFrameReached;
      property OnStartFrameReached : TNotifyEvent read FOnStartFrameReached write FOnStartFrameReached;

  end;

procedure Register;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterClasses([TGLAnimatedSprite,
                   TSpriteAnimFrame, TSpriteAnimFrameList,
                   TSpriteAnimation, TSpriteAnimationList]);
end;

// ----------
// ---------- TSpriteAnimFrame ----------
// ----------

// DoChanged
//
procedure TSpriteAnimFrame.DoChanged;
begin
  if Assigned(Owner) then begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TSpriteAnimation then
        TSpriteAnimation(Owner.Owner).DoChanged;
  end;
end;

// FriendlyName
//
class function TSpriteAnimFrame.FriendlyName : String;
begin
  Result:='Frame';
end;

// FriendlyDescription
//
class function TSpriteAnimFrame.FriendlyDescription : String;
begin
  Result:='Sprite Animation Frame';
end;

// WriteToFiler
//
procedure TSpriteAnimFrame.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // Archive version number
  with writer do begin
    WriteInteger(OffsetX);
    WriteInteger(OffsetY);
    WriteInteger(Width);
    WriteInteger(Height);
  end;
end;

// ReadFromFiler
//
procedure TSpriteAnimFrame.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);
  with reader do begin
    OffsetX:=ReadInteger;
    OffsetY:=ReadInteger;
    Width:=ReadInteger;
    Height:=ReadInteger;
  end;
end;

// SetOffsetX
//
procedure TSpriteAnimFrame.SetOffsetX(const Value: Integer);
begin
  if Value<>FOffsetX then begin
    FOffsetX := Value;
    DoChanged;
  end;
end;

// SetOffsetY
//
procedure TSpriteAnimFrame.SetOffsetY(const Value: Integer);
begin
  if Value<>FOffsetY then begin
    FOffsetY := Value;
    DoChanged;
  end;
end;

// SetWidth
//
procedure TSpriteAnimFrame.SetWidth(const Value: Integer);
begin
  if Value<>FWidth then begin
    FWidth := Value;
    DoChanged;
  end;
end;

// SetHeight
//
procedure TSpriteAnimFrame.SetHeight(const Value: Integer);
begin
  if Value<>FHeight then begin
    FHeight := Value;
    DoChanged;
  end;
end;


// ----------
// ---------- TSpriteAnimFrameList ----------
// ----------

// Create
//
constructor TSpriteAnimFrameList.Create(aOwner: TPersistent);
begin
  inherited;
end;

// ItemsClass
//
class function TSpriteAnimFrameList.ItemsClass : TXCollectionItemClass;
begin
  Result:=TSpriteAnimFrame;
end;


// ----------
// ---------- TSpriteAnimation ----------
// ----------

// Create
//
constructor TSpriteAnimation.Create(aOwner : TXCollection);
begin
  inherited;
  FFrames:=TSpriteAnimFrameList.Create(Self);
end;

// Destroy
//
destructor TSpriteAnimation.Destroy;
begin
  FFrames.Free;
  inherited;
end;

// FriendlyName
//
class function TSpriteAnimation.FriendlyName : String;
begin
  Result:='Animation';
end;

// FriendlyDescription
//
class function TSpriteAnimation.FriendlyDescription : String;
begin
  Result:='Sprite Animation';
end;

// WriteToFiler
//
procedure TSpriteAnimation.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // Archive version number
  Frames.WriteToFiler(writer);
  with writer do begin
    WriteString(LibMaterialName);
    WriteInteger(CurrentFrame);
    WriteInteger(StartFrame);
    WriteInteger(EndFrame);
    WriteInteger(FrameWidth);
    WriteInteger(FrameHeight);
    WriteInteger(Integer(Dimensions));
  end;
end;

// ReadFromFiler
//
procedure TSpriteAnimation.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);
  Frames.ReadFromFiler(reader);
  with reader do begin
    FLibMaterialName:=ReadString;
    CurrentFrame:=ReadInteger;
    StartFrame:=ReadInteger;
    EndFrame:=ReadInteger;
    FrameWidth:=ReadInteger;
    FrameHeight:=ReadInteger;
    Dimensions:=TSpriteFrameDimensions(ReadInteger);
  end;
end;

// DoChanged
//
procedure TSpriteAnimation.DoChanged;
begin
  if Assigned(Owner) then begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TGLBaseSceneObject then
        TGLBaseSceneObject(Owner.Owner).NotifyChange(Self);
  end;
end;

// SetCurrentFrame
//
procedure TSpriteAnimation.SetCurrentFrame(const Value: Integer);
begin
  if Value<>FCurrentFrame then begin
    FCurrentFrame := Value;
    if FCurrentFrame<0 then FCurrentFrame:=-1;
    DoChanged;
  end;
end;

// SetFrameWidth
//
procedure TSpriteAnimation.SetFrameWidth(const Value: Integer);
begin
  if Value<>FFrameWidth then begin
    FFrameWidth := Value;
    DoChanged;
  end;
end;

// SetFrameHeight
//
procedure TSpriteAnimation.SetFrameHeight(const Value: Integer);
begin
  if Value<>FFrameHeight then begin
    FFrameHeight := Value;
    DoChanged;
  end;
end;

// SetDimensions
//
procedure TSpriteAnimation.SetDimensions(
  const Value: TSpriteFrameDimensions);
begin
  if Value<>FDimensions then begin
    FDimensions := Value;
    DoChanged;
  end;
end;

// SetLibMaterialName
//
procedure TSpriteAnimation.SetLibMaterialName(const val : TGLLibMaterialName);
begin
  if val<>FLibMaterialName then begin
    FLibMaterialName:=val;
    FLibMaterialCached:=nil;
  end;
end;

// GetLibMaterialCached
//
function TSpriteAnimation.GetLibMaterialCached : TGLLibMaterial;
begin
  Result:=nil;
  if FLibMaterialName = '' then exit;

  if not Assigned(FLibMaterialCached) then
    if Assigned(Owner) then
      if Assigned(Owner.Owner) then
        if Owner.Owner is TGLAnimatedSprite then
          if Assigned(TGLAnimatedSprite(Owner.Owner).MaterialLibrary) then
            FLibMaterialCached:=TGLAnimatedSprite(Owner.Owner).MaterialLibrary.Materials.GetLibMaterialByName(FLibMaterialName);

  Result:=FLibMaterialCached;
end;


// ----------
// ---------- TSpriteAnimationList ----------
// ----------

// Create
//
constructor TSpriteAnimationList.Create(aOwner: TPersistent);
begin
  inherited;
end;

// ItemsClass
//
class function TSpriteAnimationList.ItemsClass : TXCollectionItemClass;
begin
  Result:=TSpriteAnimation;
end;


// ----------
// ---------- TGLAnimatedSprite ----------
// ----------

// Create
//
constructor TGLAnimatedSprite.Create(AOwner: TComponent);
begin
  inherited;

  FAnimations:=TSpriteAnimationList.Create(Self);
  FAnimationIndex:=-1;
  FInterval:=100;
  FPixelRatio:=100;
  FRotation:=0;
  FMirrorU:=False;
  FMirrorV:=False;

  ObjectStyle:=[osDirectDraw];
end;

// Destroy
//
destructor TGLAnimatedSprite.Destroy;
begin
  FAnimations.Free;
  inherited;
end;

// DoRender
//
procedure TGLAnimatedSprite.BuildList(var rci : TRenderContextInfo);
var
  vx,vy : TAffineVector;
  w,h,temp : Single;
  mat : TMatrix;
  u0,v0,u1,v1 : Single;
  x0,y0,x1,y1,TexWidth,TexHeight : Integer;
  Anim : TSpriteAnimation;
  Frame : TSpriteAnimFrame;
  libMat : TGLLibMaterial;
  IsAuto : Boolean;
begin
  if (FAnimationIndex<>-1) and (FAnimationIndex<Animations.Count) then begin
    Anim:=TSpriteAnimation(Animations[FAnimationIndex]);

    if (Anim.CurrentFrame>=0) then begin
      if (Anim.Dimensions = sfdManual) and (Anim.CurrentFrame<Anim.Frames.Count) then
        Frame:=TSpriteAnimFrame(Anim.Frames[Anim.CurrentFrame])
      else
        Frame:=nil;
      IsAuto:=(Anim.CurrentFrame<=Anim.EndFrame) and
              (Anim.CurrentFrame>=Anim.StartFrame) and
              (Anim.Dimensions = sfdAuto);
      if Assigned(Frame) or IsAuto then begin
        libMat:=Anim.LibMaterialCached;

        h:=0.5;
        w:=0.5;
        u0:=0;
        v0:=0;
        u1:=0;
        v1:=0;

        if Assigned(libMat) then begin
          TexWidth:=libMat.Material.Texture.Image.Width;
          TexHeight:=libMat.Material.Texture.Image.Height;
          if Anim.Dimensions = sfdManual then begin
            x0:=Frame.OffsetX;
            y0:=Frame.OffsetY;
            x1:=x0+Frame.Width;
            y1:=y0+Frame.Height;
          end else begin
            if TexWidth>0 then begin
              x0:=Anim.FrameWidth*(Anim.CurrentFrame mod (TexWidth div Anim.FrameWidth));
              y0:=Anim.FrameHeight*(Anim.CurrentFrame div (TexWidth div Anim.FrameWidth));
            end else begin
              x0:=0;
              y0:=0;
            end;
            x1:=x0+Anim.FrameWidth;
            y1:=y0+Anim.FrameHeight;
          end;
          if (TexWidth>0) and (TexHeight>0) and (x0<>x1) and (y0<>y1) then begin
            u0:=x0/TexWidth; v0:=1-y1/TexHeight;
            u1:=x1/TexWidth; v1:=1-y0/TexHeight;
            w:=0.5*(x1-x0)/FPixelRatio;
            h:=0.5*(y1-y0)/FPixelRatio;
          end;
        end;

        glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
        vx[0]:=mat[0][0]; vy[0]:=mat[0][1];
        vx[1]:=mat[1][0]; vy[1]:=mat[1][1];
        vx[2]:=mat[2][0]; vy[2]:=mat[2][1];
        ScaleVector(vx, w*VectorLength(vx));
        ScaleVector(vy, h*VectorLength(vy));

        if FMirrorU then begin
          temp:=u0;
          u0:=u1;
          u1:=temp;
        end;
        if FMirrorV then begin
          temp:=v0;
          v0:=v1;
          v1:=temp;
        end;

        if Assigned(libMat) then libMat.Apply(rci);
        glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        if FRotation<>0 then begin
          glMatrixMode(GL_MODELVIEW);
          glPushMatrix;
          glRotatef(FRotation,mat[0][2],mat[1][2],mat[2][2]);
        end;
        glBegin(GL_QUADS);
          glTexCoord2f(u1, v1); glVertex3f( vx[0]+vy[0], vx[1]+vy[1], vx[2]+vy[2]);
          glTexCoord2f(u0, v1); glVertex3f(-vx[0]+vy[0],-vx[1]+vy[1],-vx[2]+vy[2]);
          glTexCoord2f(u0, v0); glVertex3f(-vx[0]-vy[0],-vx[1]-vy[1],-vx[2]-vy[2]);
          glTexCoord2f(u1, v0); glVertex3f( vx[0]-vy[0], vx[1]-vy[1], vx[2]-vy[2]);
        glEnd;
        if FRotation<>0 then begin
          glPopMatrix;
        end;
        glPopAttrib;
        if Assigned(libMat) then libMat.UnApply(rci);
      end;
    end;
  end;
end;

// DoProgress
//
procedure TGLAnimatedSprite.DoProgress(const progressTime : TProgressTimes);
var
  i : Integer;
begin
  inherited;
  if (FAnimationMode<>samNone) and (Interval>0) and (AnimationIndex<>-1) then begin
    FCurrentFrameDelta:=FCurrentFrameDelta+(progressTime.deltaTime*1000)/FInterval;
    if FCurrentFrameDelta>=1 then begin
      for i:=0 to Floor(FCurrentFrameDelta)-1 do begin
        NextFrame;
        FCurrentFrameDelta:=FCurrentFrameDelta-1;
      end;
    end;
  end;
end;

// Notification
//
procedure TGLAnimatedSprite.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FMaterialLibrary) then
    MaterialLibrary:=nil;
  inherited;
end;

// DefineProperties
//
procedure TGLAnimatedSprite.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('SpriteAnimations',
                             ReadAnimations, WriteAnimations,
                             FAnimations.Count>0);
end;

// WriteAnimations
//
procedure TGLAnimatedSprite.WriteAnimations(Stream : TStream);
var
  writer : TWriter;
begin
  writer:=TWriter.Create(stream, 16384);
  try
    Animations.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadAnimations
//
procedure TGLAnimatedSprite.ReadAnimations(Stream : TStream);
var
  reader : TReader;
begin
  reader:=TReader.Create(stream, 16384);
  try
    Animations.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

// NextFrame
//
procedure TGLAnimatedSprite.NextFrame;
var
  currentFrame,
  startFrame,
  endFrame : Integer;
  Anim : TSpriteAnimation;
begin
  if (FAnimationIndex=-1) or (FAnimationIndex>=Animations.Count) then exit;

  Anim:=TSpriteAnimation(Animations[FAnimationIndex]);

  currentFrame:=Anim.CurrentFrame;
  if Anim.Dimensions = sfdManual then begin
    startFrame:=0;
    endFrame:=Anim.Frames.Count-1
  end else begin
    startFrame:=Anim.StartFrame;
    endFrame:=Anim.EndFrame;
  end;

  case AnimationMode of
    samLoop, samBounceForward, samPlayOnce : begin
      Inc(currentFrame);
      if (currentFrame = endFrame) and Assigned(FOnEndFrameReached) then
        FOnEndFrameReached(Self);
    end;
    samBounceBackward, samLoopBackward : begin
      Dec(CurrentFrame);
      if (currentFrame = startFrame) and Assigned(FOnStartFrameReached) then
        FOnStartFrameReached(Self);
    end;
  end;

  if (AnimationMode<>samNone) and Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
  
  case AnimationMode of

    samPlayOnce : begin
      if currentFrame = endFrame then
        AnimationMode:=samNone;
    end;

    samLoop : begin
      if currentFrame > endFrame then
        currentFrame:=startFrame;
    end;

    samBounceForward : begin
      if currentFrame = endFrame then
        AnimationMode:=samBounceBackward;
    end;

    samLoopBackward : begin
      if currentFrame < startFrame then
        CurrentFrame:=endFrame;
    end;

    samBounceBackward : begin
      if currentFrame = startFrame then
        AnimationMode:=samBounceForward;
    end;

  end;

  Anim.CurrentFrame:=currentFrame;
end;

// SetInterval
//
procedure TGLAnimatedSprite.SetInterval(const val : Integer);
begin
  if val<>FInterval then begin
    FInterval:=val;
    NotifyChange(Self);
  end;
end;

// SetAnimationIndex
//
procedure TGLAnimatedSprite.SetAnimationIndex(const val : Integer);
begin
  if val<>FAnimationIndex then begin
    FAnimationIndex:=val;
    if FAnimationIndex<0 then FAnimationIndex:=-1;
    NotifyChange(Self);
  end;
end;

// SetAnimationMode
//
procedure TGLAnimatedSprite.SetAnimationMode(const val : TSpriteAnimationMode);
begin
  if val<>FAnimationMode then begin
    FAnimationMode:=val;
    NotifyChange(Self);
  end;
end;

// SetMaterialLibrary
//
procedure TGLAnimatedSprite.SetMaterialLibrary(const val : TGLMaterialLibrary);
var
  i : Integer;
begin
  if val<>FMaterialLibrary then begin
    FMaterialLibrary:=val;
    for i:=0 to Animations.Count-1 do
      TSpriteAnimation(Animations[i]).FLibMaterialCached:=nil;
    NotifyChange(Self);
  end;
end;

// SetPixelRatio
//
procedure TGLAnimatedSprite.SetPixelRatio(const val : Integer);
begin
  if (FPixelRatio<>val) and (val>0) then begin
    FPixelRatio:=val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//
procedure TGLAnimatedSprite.SetRotation(const val : Integer);
begin
  if val<>FRotation then begin
    FRotation:=val;
    NotifyChange(Self);
  end;
end;

// SetMirrorU
//
procedure TGLAnimatedSprite.SetMirrorU(const val : Boolean);
begin
  if val<>FMirrorU then begin
    FMirrorU:=val;
    NotifyChange(Self);
  end;
end;

// SetMirrorV
//
procedure TGLAnimatedSprite.SetMirrorV(const val : Boolean);
begin
  if val<>FMirrorV then begin
    FMirrorV:=val;
    NotifyChange(Self);
  end;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
initialization
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

  RegisterXCollectionItemClass(TSpriteAnimFrame);
  RegisterXCollectionItemClass(TSpriteAnimation);

end.
