//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   SEM shader : Spherical Environment Mapping
   The main idea of SEM is to get the UV coordinates (which are used to lookup the matCap texture)
   from the normal vector on the fragment instead of the original texture coordinates from the object. 
    
   A material using SEM is very useful to highlight variations in the mesh: creases, bumps, even slow ondulations.
   It doesn't work that well on a cube, for instance. And does absolutely nothing on a sphere:
   SEM on a sphere is exactly the same as a planar projection of the matCap texture. 

   At this time only one light source is supported
               
}


unit VXS.GLSLSemShader;

interface

{$I VXScene.inc}

uses
  System.Classes,
  
  VXS.Scene, VXS.CrossPlatform, VXS.BaseClasses, VXS.State, Winapi.OpenGL, Winapi.OpenGLext,  VXS.OpenGL1x, 
  VXS.Context, VXS.RenderContextInfo, VXS.VectorGeometry, VXS.Coordinates, VXS.TextureFormat, 
  VXS.Color, VXS.Texture, VXS.Material, GLSL.Shader, VXS.CustomShader;

//TVXCustomGLSLSimpleSemShader
//
{ Custom class for GLSLSEMShader. 
 SEM Shader : Spherical Environment Mapping }
Type
TVXCustomGLSLSemShader = class(TVXCustomGLSLShader)
  private
    FAmbientColor: TVXColor;
//    FDiffuseColor: TVXColor;
    FSpecularColor: TVXColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;

    FMaterialLibrary: TVXAbstractMaterialLibrary;
    FMainTexture: TVXTexture;
    FMainTexName   : TVXLibMaterialName;

//    FSpecularPower: Single;
//    FLightPower: Single;

    function GetMaterialLibrary: TVXAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TVXTexture);
    function GetMainTexName: TVXLibMaterialName;
    procedure SetMainTexName(const Value: TVXLibMaterialName);

    //procedure SetDiffuseColor(AValue: TVXColor);
    procedure SetAmbientColor(AValue: TVXColor);
    procedure SetSpecularColor(AValue: TVXColor);

  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

//    property DiffuseColor : TVXColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TVXColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TVXColor Read FAmbientColor Write setAmbientColor;

    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;

    property MaterialLibrary: TVXAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TVXTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TVXLibMaterialName read GetMainTexName write SetMainTexName;

//    property SpecularPower: Single read FSpecularPower write FSpecularPower;
//    property LightPower: Single read FLightPower write FLightPower;

  end;

  TVXSLSemShader = class(TVXCustomGLSLSemShader)
  published

    property AmbientColor;
//    property DiffuseColor;
    property SpecularColor;

    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;

    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;


  end;
implementation

constructor TVXCustomGLSLSemShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec4 p = gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  lightVec = vec3(lightPos - p); ');
    Add('  viewVec = -vec3(p); ');
    Add('  normal = normalize(gl_NormalMatrix * gl_Normal ); ');

    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add('uniform float DiffuseIntensity; ');
    Add('uniform float AmbientIntensity; ');
    Add('uniform float SpecularIntensity; ');

    Add('uniform sampler2D MainTexture; ');

    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 r = reflect( V, normal ); ');
    Add('  float m = 2.0 * sqrt( pow( r.x, 2.0 ) + pow( r.y, 2.0 ) + pow( r.z + 1.0, 2.0 ) ); ');
    Add('  vec2 vN = r.xy / m + 0.5; ');
    Add('  vec4 DiffuseColor;    ');
    Add('  DiffuseColor = texture2D( MainTexture, vN ); //.rgb; ');

    // Simple Lighting
    Add('  vec3 L = normalize(lightVec); ');

    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  float NdotL = dot(L, normal); ');
    Add('  float NdotH = clamp(dot(halfAngle, normal), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = DiffuseColor*(0.5 * NdotL + 0.5); ');
    Add('  float specular = pow(NdotH, 64.0); ');

    Add('  vec4 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                     DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                     SpecularColor*specular*SpecularIntensity; ');

    Add('  gl_FragColor = FinalColour; //vec4( FinalColour, 1.0 ); ');
    Add('} ');
  end;

  FAmbientColor := TVXColor.Create(Self);
  //FDiffuseColor := TVXColor.Create(Self);
  FSpecularColor := TVXColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1.0);
  //FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1.0, 1.0, 1.0, 1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;


end;

destructor TVXCustomGLSLSemShader.Destroy;
begin
  FAmbientColor.Destroy;
 // FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TVXCustomGLSLSemShader.DoApply(var rci: TVXRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  //Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;

//  Param['SpecPower'].AsVector1f := FSpecularPower;
//  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

function TVXCustomGLSLSemShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TVXCustomGLSLSemShader.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXCustomGLSLSemShader.SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TVXAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TVXCustomGLSLSemShader.SetMainTexTexture(const Value: TVXTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TVXCustomGLSLSemShader.GetMainTexName: TVXLibMaterialName;
begin
  Result := TVXMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TVXCustomGLSLSemShader.SetMainTexName(const Value: TVXLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TVXMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


//procedure TVXCustomGLSLSemShader.SetDiffuseColor(AValue: TVXColor);
//begin
//  FDiffuseColor.DirectColor := AValue.Color;
//end;

procedure TVXCustomGLSLSemShader.SetAmbientColor(AValue: TVXColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLSemShader.SetSpecularColor(AValue: TVXColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLSemShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FMainTexture <> nil then
        begin
          Index := TVXMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;


end.
