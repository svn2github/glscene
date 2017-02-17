//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Ivory shader simulate Ivory material. 
   At this time only one light source is supported
}

unit VKS.GLSLIvoryShader;

interface

{$I VKScene.inc}

uses
  System.Classes,
  
  VKS.Scene, VKS.CrossPlatform, VKS.BaseClasses, VKS.State, Winapi.OpenGL, Winapi.OpenGLext,  VKS.OpenGL1x, 
  VKS.Context, VKS.RenderContextInfo, VKS.VectorGeometry, VKS.Coordinates,
  VKS.TextureFormat, VKS.Color, VKS.Texture, VKS.Material,
  GLSL.Shader, VKS.CustomShader;

//TVKCustomGLSLIvoryShader
//
{ Custom class for GLSLIvoryShader. 
 A shader that simulate Ivory Material }
type
  TVKCustomGLSLIvoryShader = class(TVKCustomGLSLShader)
  private

  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  end;

type
  TVKSLIvoryShader = class(TVKCustomGLSLIvoryShader)

  end;

implementation


{ TVKCustomGLSLIvoryShader }

constructor TVKCustomGLSLIvoryShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  normal = gl_NormalMatrix * gl_Normal; ');
    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  viewVec = -vec3(vert); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('vec3 norm = normalize(normal); ');
    Add('vec3 L = normalize(lightVec); ');
    Add('vec3 V = normalize(viewVec); ');
    Add('vec3 halfAngle = normalize(L + V); ');
    Add('float NdotL = dot(L, norm); ');
    Add('float NdotH = clamp(dot(halfAngle, norm), 0.0, 1.0); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('float diffuse = 0.5 * NdotL + 0.5; ');
    Add('float specular = pow(NdotH, 64.0); ');
    Add('float result = diffuse + specular; ');
    Add('gl_FragColor = vec4(result); ');
    Add('} ');
  end;


  // Initial stuff.

end;

destructor TVKCustomGLSLIvoryShader.Destroy;
begin

  inherited;
end;

procedure TVKCustomGLSLIvoryShader.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;

end;

function TVKCustomGLSLIvoryShader.DoUnApply(
  var rci: TVKRenderContextInfo): Boolean;
begin
  Result := False;
  GetGLSLProg.EndUseProgramObject;
end;



end.
