//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements projected textures through a GLScene object.
    
}
unit VKS.ProjectedTextures;

interface

{$I VKScene.inc}

uses
  System.Classes,

  VKS.Scene,
  VKS.Texture,
  Winapi.OpenGL, Winapi.OpenGLext, 
  VKS.VectorGeometry,
  XOpenGL,
  VKS.RenderContextInfo,
  VKS.State,
  VKS.Context;

type
  { Possible styles of texture projection. Possible values: 
    ptsOriginal: Original projection method (first pass,
    is default scene render, second pass is texture  projection).
    ptsInverse: Inverse projection method (first pass
    is texture projection, sencond pass is regular scene render).
    This method is useful if you want to simulate
    lighting only through projected textures (the textures
    of the scene are "masked" into the white areas of
    the projection textures). }
  TVKProjectedTexturesStyle = (ptsOriginal, ptsInverse);

  TVKProjectedTextures = class;

  { A projected texture emmiter.
     It's material property will be used as the projected texture.
     Can be places anywhere in the scene. }
  TVKTextureEmitter = class(TVKSceneObject)
  private
    FFOVy: single;
    FAspect: single;
  protected
    { Sets up the base texture matrix for this emitter
       Should be called whenever a change on its properties is made.}
    procedure SetupTexMatrix(var ARci: TVKRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Indicates the field-of-view of the projection frustum.}
    property FOVy: single read FFOVy write FFOVy;
    { x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.}
    property Aspect: single read FAspect write FAspect;
  end;

  { Specifies an item on the TVKTextureEmitters collection. }
  TVKTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TVKTextureEmitter;
  protected
    procedure SetEmitter(const val: TVKTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TVKTextureEmitter read FEmitter write SetEmitter;
  end;

  { Collection of TVKTextureEmitter. }
  TVKTextureEmitters = class(TCollection)
  private
    FOwner: TVKProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TVKTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TVKTextureEmitter);
    property Items[index: Integer]: TVKTextureEmitterItem read GetItems; default;
  end;

  { Projected Textures Manager.
    Specifies active texture Emitters (whose texture will be projected)
    and receivers (children of this object). }
  TVKProjectedTextures = class(TVKImmaterialSceneObject)
  private
    FEmitters: TVKTextureEmitters;
    FStyle: TVKProjectedTexturesStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    { List of texture emitters. }
    property Emitters: TVKTextureEmitters read FEmitters write FEmitters;
    { Indicates the style of the projected textures. }
    property Style: TVKProjectedTexturesStyle read FStyle write FStyle;
  end;

//==============================================================
implementation
//==============================================================

// ------------------
// ------------------ TVKTextureEmitter ------------------
// ------------------

constructor TVKTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOVy := 90;
  FAspect := 1;
end;

procedure TVKTextureEmitter.SetupTexMatrix(var ARci: TVKRenderContextInfo);
const
  cBaseMat: TMatrix =
  (V:((X:0.5; Y:0;   Z:0; W:0),
          (X:0;   Y:0.5; Z:0; W:0),
          (X:0;   Y:0; Z:1; W:0),
          (X:0.5; Y:0.5; Z:0; W:1)));

var
  PM: TMatrix;
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  PM := MatrixMultiply(CreatePerspectiveMatrix(FFOVy, FAspect, 0.1, 1), cBaseMat);
  PM := MatrixMultiply(invAbsoluteMatrix, PM);
  Arci.VKStates.SetTextureMatrix(PM);
end;

// ------------------
// ------------------ TVKTextureEmitterItem ------------------
// ------------------

constructor TVKTextureEmitterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TVKTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TVKTextureEmitterItem then
  begin
    FEmitter := TVKTextureEmitterItem(Source).FEmitter;
    TVKProjectedTextures(TVKTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

procedure TVKTextureEmitterItem.SetEmitter(const val: TVKTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TVKProjectedTextures(TVKTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

// RemoveNotification
//

procedure TVKTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

// GetDisplayName
//

function TVKTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[TexEmitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TVKTextureEmitters ------------------
// ------------------

// GetOwner
//

function TVKTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// GetItems
//

function TVKTextureEmitters.GetItems(index: Integer): TVKTextureEmitterItem;
begin
  Result := TVKTextureEmitterItem(inherited Items[index]);
end;

// RemoveNotification
//

procedure TVKTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RemoveNotification(aComponent);
end;

// AddEmitter
//

procedure TVKTextureEmitters.AddEmitter(texEmitter: TVKTextureEmitter);
var
  item: TVKTextureEmitterItem;
begin
  item := TVKTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
end;

// ------------------
// ------------------ TVKProjectedTextures ------------------
// ------------------

// Create
//

constructor TVKProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TVKTextureEmitters.Create(TVKTextureEmitterItem);
  FEmitters.FOwner := self;
end;

// Destroy
//

destructor TVKProjectedTextures.Destroy;
begin
  FEmitters.Free;
  inherited destroy;
end;

// DoRender
//

procedure TVKProjectedTextures.DoRender(var ARci: TVKRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
const
  PS: array[0..3] of GLfloat = (1, 0, 0, 0);
  PT: array[0..3] of GLfloat = (0, 1, 0, 0);
  PR: array[0..3] of GLfloat = (0, 0, 1, 0);
  PQ: array[0..3] of GLfloat = (0, 0, 0, 1);
var
  i: integer;
  emitter: TVKTextureEmitter;
begin
  if not (ARenderSelf or ARenderChildren) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  //First pass of original style: render regular scene
  if Style = ptsOriginal then
    self.RenderChildren(0, Count - 1, ARci);

  //generate planes
  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);

  glTexGenfv(GL_S, GL_EYE_PLANE, @PS);
  glTexGenfv(GL_T, GL_EYE_PLANE, @PT);
  glTexGenfv(GL_R, GL_EYE_PLANE, @PR);
  glTexGenfv(GL_Q, GL_EYE_PLANE, @PQ);

  //options
  Arci.VKStates.Disable(stLighting);
  Arci.VKStates.DepthFunc := cfLEqual;
  Arci.VKStates.Enable(stBlend);
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
  glEnable(GL_TEXTURE_GEN_R);
  glEnable(GL_TEXTURE_GEN_Q);

  //second pass (original) first pass (inverse): for each emiter,
  //render projecting the texture summing all emitters
  for i := 0 to Emitters.Count - 1 do
  begin
    emitter := Emitters[i].Emitter;
    if not assigned(emitter) then
      continue;
    if not emitter.Visible then
      continue;

    emitter.Material.Apply(ARci);

    ARci.VKStates.Enable(stBlend);
    if Style = ptsOriginal then
    begin
      //on the original style, render blending the textures
      if emitter.Material.Texture.TextureMode <> tmBlend then
        ARci.VKStates.SetBlendFunc(bfDstColor, bfOne)
      else
        ARci.VKStates.SetBlendFunc(bfDstColor, bfZero);
    end
    else
    begin
      //on inverse style: the first texture projector should
      //be a regular rendering (i.e. no blending). All others
      //are "added" together creating an "illumination mask"
      if i = 0 then
        Arci.VKStates.SetBlendFunc(bfOne, bfZero)
      else
        ARci.VKStates.SetBlendFunc(bfOne, bfOne);
    end;

    //get this emitter's tex matrix
    emitter.SetupTexMatrix(ARci);
    repeat
      ARci.ignoreMaterials := true;
      Self.RenderChildren(0, Count - 1, ARci);
      ARci.ignoreMaterials := false;
    until not emitter.Material.UnApply(ARci);
  end;

  // LoseTexMatrix
  ARci.VKStates.SetBlendFunc(bfOne, bfZero);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glDisable(GL_TEXTURE_GEN_R);
  glDisable(GL_TEXTURE_GEN_Q);

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);

  ARci.VKStates.DepthFunc := cfLEqual;

  //second pass (inverse): render regular scene, blending it
  //with the "mask"
  if Style = ptsInverse then
  begin

    Arci.VKStates.Enable(stBlend);
    ARci.VKStates.SetBlendFunc(bfDstColor, bfSrcColor);

    //second pass: render everything, blending with what is
    //already there
    ARci.ignoreBlendingRequests := true;
    self.RenderChildren(0, Count - 1, ARci);
    ARci.ignoreBlendingRequests := false;

  end;
end;

initialization

  RegisterClasses([TVKTextureEmitter, TVKProjectedTextures]);

end.

