//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements projected textures through a GLScene object.
    
}
unit VXS.ProjectedTextures;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,

  VXS.XOpenGL,
  VXS.VectorTypes,
  VXS.Scene,
  VXS.PersistentClasses,
  VXS.Texture,
  VXS.VectorGeometry,
  VXS.RenderContextInfo,
  VXS.State,
  VXS.Context;

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
  TVXProjectedTexturesStyle = (ptsOriginal, ptsInverse);

  TVXProjectedTextures = class;

  { A projected texture emmiter.
     It's material property will be used as the projected texture.
     Can be places anywhere in the scene. }
  TVXTextureEmitter = class(TVXSceneObject)
  private
    FFOVy: single;
    FAspect: single;
  protected
    { Sets up the base texture matrix for this emitter
       Should be called whenever a change on its properties is made.}
    procedure SetupTexMatrix(var ARci: TVXRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Indicates the field-of-view of the projection frustum.}
    property FOVy: single read FFOVy write FFOVy;
    { x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.}
    property Aspect: single read FAspect write FAspect;
  end;

  { Specifies an item on the TVXTextureEmitters collection. }
  TVXTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TVXTextureEmitter;
  protected
    procedure SetEmitter(const val: TVXTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TVXTextureEmitter read FEmitter write SetEmitter;
  end;

  { Collection of TVXTextureEmitter. }
  TVXTextureEmitters = class(TCollection)
  private
    FOwner: TVXProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TVXTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TVXTextureEmitter);
    property Items[index: Integer]: TVXTextureEmitterItem read GetItems; default;
  end;

  { Projected Textures Manager.
    Specifies active texture Emitters (whose texture will be projected)
    and receivers (children of this object). }
  TVXProjectedTextures = class(TVXImmaterialSceneObject)
  private
    FEmitters: TVXTextureEmitters;
    FStyle: TVXProjectedTexturesStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVXRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    { List of texture emitters. }
    property Emitters: TVXTextureEmitters read FEmitters write FEmitters;
    { Indicates the style of the projected textures. }
    property Style: TVXProjectedTexturesStyle read FStyle write FStyle;
  end;

//==============================================================
implementation
//==============================================================

// ------------------
// ------------------ TVXTextureEmitter ------------------
// ------------------

constructor TVXTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOVy := 90;
  FAspect := 1;
end;

procedure TVXTextureEmitter.SetupTexMatrix(var ARci: TVXRenderContextInfo);
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
  Arci.VXStates.SetTextureMatrix(PM);
end;

// ------------------
// ------------------ TVXTextureEmitterItem ------------------
// ------------------

constructor TVXTextureEmitterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TVXTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TVXTextureEmitterItem then
  begin
    FEmitter := TVXTextureEmitterItem(Source).FEmitter;
    TVXProjectedTextures(TVXTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

procedure TVXTextureEmitterItem.SetEmitter(const val: TVXTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TVXProjectedTextures(TVXTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

// RemoveNotification
//

procedure TVXTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

// GetDisplayName
//

function TVXTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[TexEmitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TVXTextureEmitters ------------------
// ------------------

// GetOwner
//

function TVXTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// GetItems
//

function TVXTextureEmitters.GetItems(index: Integer): TVXTextureEmitterItem;
begin
  Result := TVXTextureEmitterItem(inherited Items[index]);
end;

// RemoveNotification
//

procedure TVXTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RemoveNotification(aComponent);
end;

// AddEmitter
//

procedure TVXTextureEmitters.AddEmitter(texEmitter: TVXTextureEmitter);
var
  item: TVXTextureEmitterItem;
begin
  item := TVXTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
end;

// ------------------
// ------------------ TVXProjectedTextures ------------------
// ------------------

// Create
//

constructor TVXProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TVXTextureEmitters.Create(TVXTextureEmitterItem);
  FEmitters.FOwner := self;
end;

// Destroy
//

destructor TVXProjectedTextures.Destroy;
begin
  FEmitters.Free;
  inherited destroy;
end;

// DoRender
//

procedure TVXProjectedTextures.DoRender(var ARci: TVXRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
const
  PS: array[0..3] of GLfloat = (1, 0, 0, 0);
  PT: array[0..3] of GLfloat = (0, 1, 0, 0);
  PR: array[0..3] of GLfloat = (0, 0, 1, 0);
  PQ: array[0..3] of GLfloat = (0, 0, 0, 1);
var
  i: integer;
  emitter: TVXTextureEmitter;
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
  Arci.VXStates.Disable(stLighting);
  Arci.VXStates.DepthFunc := cfLEqual;
  Arci.VXStates.Enable(stBlend);
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

    ARci.VXStates.Enable(stBlend);
    if Style = ptsOriginal then
    begin
      //on the original style, render blending the textures
      if emitter.Material.Texture.TextureMode <> tmBlend then
        ARci.VXStates.SetBlendFunc(bfDstColor, bfOne)
      else
        ARci.VXStates.SetBlendFunc(bfDstColor, bfZero);
    end
    else
    begin
      //on inverse style: the first texture projector should
      //be a regular rendering (i.e. no blending). All others
      //are "added" together creating an "illumination mask"
      if i = 0 then
        Arci.VXStates.SetBlendFunc(bfOne, bfZero)
      else
        ARci.VXStates.SetBlendFunc(bfOne, bfOne);
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
  ARci.VXStates.SetBlendFunc(bfOne, bfZero);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glDisable(GL_TEXTURE_GEN_R);
  glDisable(GL_TEXTURE_GEN_Q);

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);

  ARci.VXStates.DepthFunc := cfLEqual;

  //second pass (inverse): render regular scene, blending it
  //with the "mask"
  if Style = ptsInverse then
  begin

    Arci.VXStates.Enable(stBlend);
    ARci.VXStates.SetBlendFunc(bfDstColor, bfSrcColor);

    //second pass: render everything, blending with what is
    //already there
    ARci.ignoreBlendingRequests := true;
    self.RenderChildren(0, Count - 1, ARci);
    ARci.ignoreBlendingRequests := false;

  end;
end;

initialization

  RegisterClasses([TVXTextureEmitter, TVXProjectedTextures]);

end.

