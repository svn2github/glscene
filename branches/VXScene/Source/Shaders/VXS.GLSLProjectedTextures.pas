//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements projected textures through a GLScene object via GLSL.
}

{; Known bugs/limitations

1. Only 1 texture can be used for all emitters
2. Only up to 6 Emitters can be used (more on better cards)
   A way round this is to make the emiitters a children of the 6 nearest objects
   to the camera.
3. Changing emitter properties causes a slight delay while recreating the shader.
   To make an emitter invisible, just move it to somewhere it won't project on
   anything, or set the brightness to 0. (?)
4. All children of the ProjectedTextures must have use a texture.
   The shader can't be changed between rendering each seperate object..
}

unit VXS.GLSLProjectedTextures;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  
  VXS.PersistentClasses,
  VXS.Scene,
  VXS.Texture,
  VXS.VectorGeometry,
  VXS.Context,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.TextureFormat,
  VXS.PipelineTransformation,
  VXS.VectorTypes;

type
  TVXSLProjectedTexturesStyle = (ptsLight, ptsShadow);

  TVXSLProjectedTextures = class;

  { A projected texture emitter.
     Can be places anywhere in the scene.
     Used to generate a modelview and texture matrix for the shader}
  TVXSLTextureEmitter = class(TVXBaseSceneObject)
  private
    FFOV: single;
    FAspect, FBrightness, FAttenuation: single;
    FStyle: TVXSLProjectedTexturesStyle;
    FColor: TVXColor;
    FUseAttenuation, FAllowReverseProjection: boolean;
    FUseQuadraticAttenuation: boolean;
  protected
    ProjectedTexturesObject: TVXSLProjectedTextures;
    TexMatrix: TMatrix;
    procedure SetupTexMatrix;
    procedure SetStyle(val: TVXSLProjectedTexturesStyle);
    procedure SetUseAttenuation(val: boolean);
    procedure SetUseQuadraticAttenuation(val: boolean);
    procedure SetAllowReverseProjection(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: boolean); override;
  published
    { Indicates the field-of-view of the projection frustum.}
    property FOV: single read FFOV write FFOV;
    { x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.}
    property Aspect: single read FAspect write FAspect;
    { Indicates the style of the projected textures.}
    property Style: TVXSLProjectedTexturesStyle read FStyle write SetStyle;
    {Fall off/ attenuation of the projected texture}
    property Attenuation: single read FAttenuation write FAttenuation;
    property Brightness: single read FBrightness write FBrightness;
    property Color: TVXColor read FColor write FColor;
    property UseAttenuation: boolean read FUseAttenuation write SetUseAttenuation;
    property UseQuadraticAttenuation: Boolean read FUseQuadraticAttenuation write SetUseQuadraticAttenuation;
    property AllowReverseProjection: boolean read FAllowReverseProjection write SetAllowReverseProjection;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  { Specifies an item on the TVXSLTextureEmitters collection. }
  TVXSLTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TVXSLTextureEmitter;
  protected
    procedure SetEmitter(const val: TVXSLTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TVXSLTextureEmitter read FEmitter write SetEmitter;
  end;

  { Collection of TVXSLTextureEmitter. }
  TVXSLTextureEmitters = class(TCollection)
  private
    FOwner: TVXSLProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TVXSLTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TVXSLTextureEmitter);
    property Items[index: Integer]: TVXSLTextureEmitterItem read GetItems; default;
  end;

  { Projected Texture Manager.
     Specifies active Emitters and receivers (children of this object).
     At the moment, only 1 texture can be used.}
  TVXSLProjectedTextures = class(TVXSceneObject)
  private
    FEmitters: TVXSLTextureEmitters;
    FUseLightmaps: boolean;
    Shader: TVXProgramHandle;
    FAmbient: TVXColor;
    procedure SetupShader;
  protected
    ShaderChanged: boolean;
    procedure SetUseLightmaps(val: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TVXRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
  published
    { List of emitters. }
    property Emitters: TVXSLTextureEmitters read FEmitters write FEmitters;

    //Ambient is use if no lightmap..
    property Ambient: TVXColor read fAmbient write fAmbient;
    property UseLightmaps: boolean read FUseLightmaps write SetUseLightmaps;
  end;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

// ------------------
// ------------------ TVXSLTextureEmitter ------------------
// ------------------

constructor TVXSLTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOV := 90;
  FAspect := 1;
  FStyle := ptsLight;
  FAllowReverseProjection := false;
  FUseAttenuation := false;
  FAttenuation := 100;
  FBrightness := 1;
  FColor := TVXColor.create(self);
  FColor.SetColor(1, 1, 1);
end;

destructor TVXSLTextureEmitter.Destroy;
begin
  FColor.Free;
  inherited;
end;

procedure TVXSLTextureEmitter.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  SetupTexMatrix;
  inherited;
end;

procedure TVXSLTextureEmitter.SetupTexMatrix;
const
  cBaseMat: TMatrix = (V:((X:0.5; Y:0;   Z:0; W:0),
                          (X:0;   Y:0.5; Z:0; W:0),
                          (X:0;   Y:0;   Z:1; W:0),
                          (X:0.5; Y:0.5; Z:0; W:1)));
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  TexMatrix := MatrixMultiply(
    CreatePerspectiveMatrix(FFOV, FAspect, 0.1, 1), cBaseMat);
  TexMatrix := MatrixMultiply(
    CurrentVXContext.PipelineTransformation.InvModelViewMatrix^, TexMatrix);
end;

procedure TVXSLTextureEmitter.SetAllowReverseProjection(val: boolean);
begin
  FAllowReverseProjection := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TVXSLTextureEmitter.SetUseAttenuation(val: boolean);
begin
  FUseAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TVXSLTextureEmitter.SetUseQuadraticAttenuation(val: boolean);
begin
  FUseQuadraticAttenuation := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

procedure TVXSLTextureEmitter.SetStyle(val: TVXSLProjectedTexturesStyle);
begin
  FStyle := val;
  if assigned(ProjectedTexturesObject) then
    ProjectedTexturesObject.ShaderChanged := true;
end;

// ------------------
// ------------------ TVXSLTextureEmitterItem ------------------
// ------------------

constructor TVXSLTextureEmitterItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TVXSLTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TVXSLTextureEmitterItem then
  begin
    FEmitter := TVXSLTextureEmitterItem(Source).FEmitter;
    TVXSLProjectedTextures(TVXSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

procedure TVXSLTextureEmitterItem.SetEmitter(const val: TVXSLTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TVXSLProjectedTextures(TVXSLTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

procedure TVXSLTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

function TVXSLTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[Emitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TVXSLTextureEmitters ------------------
// ------------------

function TVXSLTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TVXSLTextureEmitters.GetItems(index: Integer): TVXSLTextureEmitterItem;
begin
  Result := TVXSLTextureEmitterItem(inherited Items[index]);
end;

procedure TVXSLTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].RemoveNotification(aComponent);
    TVXSLProjectedTextures(GetOwner).shaderChanged := true;
  end;
end;

procedure TVXSLTextureEmitters.AddEmitter(texEmitter: TVXSLTextureEmitter);
var
  item: TVXSLTextureEmitterItem;
begin
  item := TVXSLTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
  item.Emitter.ProjectedTexturesObject := TVXSLProjectedTextures(GetOwner);
  TVXSLProjectedTextures(GetOwner).shaderChanged := true;
end;

// ------------------
// ------------------ TVXSLProjectedTextures ------------------
// ------------------

constructor TVXSLProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TVXSLTextureEmitters.Create(TVXSLTextureEmitterItem);
  FEmitters.FOwner := self;
  FUseLightmaps := false;
  ShaderChanged := true;
  Ambient := TVXColor.Create(self);
  ambient.SetColor(0.5, 0.5, 0.5, 0.5);
end;

destructor TVXSLProjectedTextures.Destroy;
begin
  if assigned(shader) then
    Shader.free;
  FEmitters.Free;
  Ambient.Free;
  inherited destroy;
end;

procedure TVXSLProjectedTextures.SetUseLightmaps(val: boolean);
begin
  FUseLightmaps := val;
  ShaderChanged := true;
end;

procedure TVXSLProjectedTextures.SetupShader;
const
  AbsFunc: array[boolean] of string = ('', 'abs');
var
  vp, fp: TStringlist;
  i: integer;
  emitter: TVXSLTextureEmitter;
  OldSeparator: char;
begin
  if assigned(shader) then
    FreeAndNil(shader);

  Shader := TVXProgramHandle.CreateAndAllocate;

  OldSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  vp := TStringlist.create;
  fp := TStringlist.create;

  try
    //define the vertex program
    if emitters.count > 0 then
    begin
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        if not emitter.Visible then
          continue;
        vp.add(format('uniform mat4 TextureMatrix%d;', [i]));
        vp.add(format('varying vec4 ProjTexCoords%d;', [i]));
      end;
    end;

    vp.add('void main(){');
    vp.add('vec4 P = gl_Vertex;');
    vp.add('gl_Position = gl_ModelViewProjectionMatrix * P;');
    vp.add('vec4 Pe = gl_ModelViewMatrix * P;');

    vp.add('gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;');

    if UseLightmaps then
      vp.add('gl_TexCoord[1] = gl_TextureMatrix[1] * gl_MultiTexCoord1;');
    if emitters.count > 0 then
    begin
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        vp.add(format('ProjTexCoords%d = TextureMatrix%d * Pe;', [i, i]));
      end;
    end;
    vp.add('}');

    //define the fragment program
    fp.add('uniform sampler2D TextureMap;');
    if UseLightmaps then
      fp.add('uniform sampler2D LightMap;');
    if emitters.count > 0 then
    begin
      fp.add('uniform sampler2D ProjMap;');

      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        fp.add(format('varying vec4 ProjTexCoords%d;', [i]));
        if Emitter.UseAttenuation then
          fp.add(format('uniform float Attenuation%d;', [i]));
        fp.add(format('uniform float Brightness%d;', [i]));
        fp.add(format('uniform vec3 Color%d;', [i]));
      end;
    end;

    fp.add('void main(){');
    fp.add('vec4 color = texture2D(TextureMap, gl_TexCoord[0].st).rgba;');
    if UseLightmaps then
      fp.add('vec3 light = texture2D(LightMap, gl_TexCoord[1].st).rgb;')
    else
      fp.add(format('vec3 light = vec3(%.4, %.4, %.4);', [Ambient.Red, ambient.Green, ambient.Blue]));
    if emitters.count > 0 then
    begin
      fp.add('vec3 projlight = vec3(0.0);');
      fp.add('vec3 projshadow = vec3(0.0);');
      fp.add('vec3 temp;');
      fp.add('float dist;');
      for i := 0 to emitters.count - 1 do
      begin
        emitter := Emitters[i].Emitter;
        if not assigned(emitter) then
          continue;
        if not emitter.visible then
          continue;
        if not emitter.AllowReverseProjection then
          fp.add(format('if (ProjTexCoords%d.q<0.0){', [i]));
        case emitter.Style of
          ptslight:
            fp.add(format('projlight+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*Color%d*Brightness%d);', [i, i, i]));
          ptsShadow:
            fp.add(format('projshadow+= (texture2DProj(ProjMap, ProjTexCoords%d).rgb*Color%d*Brightness%d);', [i, i, i]));
        end;

        if emitter.UseAttenuation then
        begin
          // for attenuation we need the distance to the point
          // so use absolute value when AllowReverseProjection is enabled
          fp.add(format('dist = 1.0 - clamp(%s(ProjTexCoords%d.q/Attenuation%d), 0.0, 1.0);',
            [AbsFunc[emitter.AllowReverseProjection], i, i]));
          if emitter.UseQuadraticAttenuation then
            fp.add('dist *= dist;');
          case emitter.Style of
            ptslight:
              fp.add('projlight *= dist;');
            ptsShadow:
              fp.add('projshadow *= dist;');
          end;

        end;
        if not emitter.AllowReverseProjection then
          fp[fp.Count - 1] := fp[fp.Count - 1] + '}';
      end;

      fp.add('projlight = clamp(projlight,0.0,1.2);');
      fp.add('projshadow = clamp(projshadow,0.0,0.8);');

      fp.add('vec3 totlight = 1.0-((( 1.0-projlight)*( 1.0-light)) +(projshadow*light)) ;');
    end
    else
      fp.add('vec3 totlight = light;');

    fp.add('gl_FragColor = vec4(1.5*totlight * color.rgb, color.a);}');

    Shader.AddShader(TVXVertexShaderHandle, vp.Text, True);
    Shader.AddShader(TVXFragmentShaderHandle, fp.Text, True);
  finally
    FormatSettings.DecimalSeparator := OldSeparator;
    vp.free;
    fp.free;
  end;

  if not Shader.LinkProgram then
    raise Exception.Create(Shader.InfoLog);
  if not Shader.ValidateProgram then
    raise Exception.Create(Shader.InfoLog);
end;

procedure TVXSLProjectedTextures.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: boolean);
var
  i: integer;
  emitter: TVXSLTextureEmitter;
begin
  if not (renderSelf or renderChildren) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  if ShaderChanged then
  begin
    SetupShader;
    ShaderChanged := false;
  end;

  with Shader do
  begin
    UseProgramObject;

    for i := 0 to Emitters.Count - 1 do
    begin
      emitter := Emitters[i].Emitter;
      if not assigned(emitter) then
        continue;
      if emitter.UseAttenuation then
        // negate attenuation here, instead of negating q inside the shader
        // otherwise the result of q/attenuation is negative.
        Uniform1f['Attenuation' + inttostr(i)] := -emitter.Attenuation;

      Uniform1f['Brightness' + inttostr(i)] := emitter.Brightness;
      Uniform3f['Color' + inttostr(i)] := PAffinevector(@emitter.Color.Color)^;
      Uniformmatrix4fv['TextureMatrix' + inttostr(i)] := emitter.texMatrix;
    end;

    Uniform1i['TextureMap'] := 0;

    if UseLightmaps then
      Uniform1i['LightMap'] := 1;

    if emitters.count > 0 then
      Shader.Uniform1i['ProjMap'] := 2;

    rci.VXStates.TextureBinding[2, ttTexture2D] := Material.Texture.Handle;

    self.RenderChildren(0, Count - 1, rci);

    EndUseProgramObject;
  end;
end;

procedure TVXSLProjectedTextures.StructureChanged;
begin
  inherited;
  shaderchanged := true;
end;

//===========================================================
initialization
//===========================================================

  RegisterClasses([TVXSLTextureEmitter, TVXSLProjectedTextures]);

end.

