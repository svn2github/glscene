//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLPostPixelateShader <p>

   A shader that Pixelate of the entire scene.<p>

   <b>History : </b><font size=-1><ul>
      <li>01/11/15 - J.Delauney - Initial version (contributed to GLScene)

}
unit GLSLPostPixelateShader;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils, System.StrUtils,vcl.Dialogs,
  // GLS
  GLTexture, GLScene, GLVectorGeometry, GLContext,
  GLSLShader, GLCustomShader, GLRenderContextInfo, GLTextureFormat,
  GLVectorTypes;


type
  TGLCustomGLSLPostPixelateShader = class(TGLCustomGLSLShader, IGLPostShader)
  private

    FPixelWidth  : Single;
    FPixelHeight : Single;

    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;

    function StorePixelWidth: Boolean;
    function StorePixelHeight: Boolean;

  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

     property PixelWidth: Single read FPixelWidth write FPixelWidth stored StorePixelWidth;
     property PixelHeight: Single read FPixelHeight write FPixelHeight stored StorePixelHeight;
  end;

  TGLSLPostPixelateShader = class(TGLCustomGLSLPostPixelateShader)
  published
    property PixelWidth;
    property PixelHeight;
  end;

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

{ TGLCustomGLSLPostPixelateShader }

constructor TGLCustomGLSLPostPixelateShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float pixel_w; // 8.0 ');
   Add('uniform float pixel_h; // 8.0 ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 tc = vec3(1.0, 0.0, 0.0); ');
   Add('   vec2 coord = vec2(pixel_w*floor(uv.x/pixel_w),pixel_h*floor(uv.y/pixel_h)); ');
   Add('   tc = texture2DRect(ScreenTex, coord).rgb; ');
   Add('   gl_FragColor = vec4(tc, 1); ');
   Add('} ');

  end;

  FPixelWidth  := 8;
  FPixelHeight := 12;
end;

procedure TGLCustomGLSLPostPixelateShader.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['pixel_w'] := FPixelWidth;
  GetGLSLProg.Uniform1f['pixel_h'] := FPixelHeight;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostPixelateShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostPixelateShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostPixelateShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostPixelateShader.StorePixelWidth: Boolean;
begin
  Result := (Abs(FPixelWidth) > 0) and (Abs(FPixelWidth) <= 64);
end;

function TGLCustomGLSLPostPixelateShader.StorePixelHeight: Boolean;
begin
  Result := (Abs(FPixelHeight) > 0) and (Abs(FPixelHeight) <= 64);
end;

end.

