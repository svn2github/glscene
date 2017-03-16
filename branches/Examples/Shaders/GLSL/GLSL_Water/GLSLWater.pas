//----------------------------------------------------------------------------//
 LWater v1.1                                                             //
//                                                                            //
// Что нового:                                                                //
//   Производительность увеличилась более чем в 6 раз                         //
//   на AMD Athlon 3200+, GF7600 значение FPS повысилось с ~40 до ~270.       //
//                                                                            //
// Изменились следующие имена:                                                //
//   ReflectionObjList -> Reflection                                          //
//   WaterBumpTexture  -> BumpTexture                                         //
//   WaterPlaneWidth   -> WaterWidth                                          //
//   WaterPlaneHeight  -> WaterHeight                                         //
//                                                                            //
// Как пользоваться:                                                          //
//   1. После создания водной поверхности, требуется заполнить все поля       //
//      (такие как: Scene, SceneViewer, Camera и Cadencer). Так же нужно      //
//      указать путь к bump-текстуре воды.                                    //
//   2. Задать размеры водной поверхности (WaterWidth и WaterHeight).         //
//   3. Включить туман в свойствах SceneViewer'a.                             //
//   4. Проинициализировть воду (Water.Initalize).                            //
//   5. Поместить в объект-контейнер Reflection объекты, которые должны      //
//      отражать вода.                                                        //
// P.S.                                                                       //
//   Вся основная работа была сделана не мной, а автором демнострационного    //
//   проекта Forest*. Я лишь свел все в единое целое.                         //
//                                                                            //
//   *Демонстрационный проект можно найти на оф. сайте GLScene или в архиве с //
//    с релизом GLScene от 10.02.06.                                          //
//----------------------------------------------------------------------------//

unit GLSLWater;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLSkydome, GLWin32Viewer, OpenGL1x, GLContext,
  XOpenGL, GLUtils, GLObjects, GLTexture, GLVectorGeometry, GLCadencer, JPEG, TGA,
  {DDS,} ExtCtrls, GLMaterial, GLRenderContextInfo, OpenGLTokens;

type
  TGLSLWater = class(TGLBaseSceneObject)

  private
    mirrorTexture : TGLTextureHandle;
    mirrorTexType : TGLEnum;
    reflectionProgram : TGLProgramHandle;
    supportsGLSL : Boolean;
    enableGLSL : Boolean;
    enableRectReflection, enableTex2DReflection : Boolean;

    FMLWater: TGLMaterialLibrary;
    FScene: TGLScene;
    FSceneViewer: TGLSceneViewer;
    FCadencer: TGLCadencer;
    FCamera: TGLCamera;
    FInitalizeWater, FRenderWater: TGLDirectOpenGL;
    FReflectionObjList: TGLBaseSceneObject;
    FBumpTex: string;
    FWaterWidth, FWaterHeight: single;

    procedure InitalizeWater(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure RenderWater(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure SetupReflectionMatrix;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initalize;
    property Scene: TGLScene                read FScene             write FScene;
    property SceneViewer: TGLSceneViewer    read FSceneViewer       write FSceneViewer;
    property Cadencer: TGLCadencer          read FCadencer          write FCadencer;
    property Camera: TGLCamera              read FCamera            write FCamera;
    property Reflection: TGLBaseSceneObject read FReflectionObjList write FReflectionObjList;
    property BumpTexture: string            read FBumpTex           write FBumpTex;
    property WaterWidth: single             read FWaterWidth        write FWaterWidth;
    property WaterHeight: single            read FWaterHeight       write FWaterHeight;
  end;

implementation

// shaders
const
  water_vp = '#ifndef __GLSL_CG_DATA_TYPES'                                                                  +#10#13+
             '# define half float'                                                                           +#10#13+
             '# define half3x3 mat3'                                                                         +#10#13+
             '# define half2 vec2'                                                                           +#10#13+
             '# define half3 vec3'                                                                           +#10#13+
             '# define half4 vec4'                                                                           +#10#13+
             '#endif'                                                                                        +#10#13+
             'uniform vec4  EyePos;'                                                                         +#10#13+
             'uniform float Time;'                                                                           +#10#13+
             'varying vec3  EyeVec;'                                                                         +#10#13+
             'varying half4 FogColor;'                                                                       +#10#13+
             'void main()'                                                                                   +#10#13+
             '{'                                                                                             +#10#13+
             'EyeVec = gl_Vertex.xyz-EyePos.xyz;'                                                            +#10#13+
             'FogColor.rgb = gl_Fog.color.rgb;'                                                              +#10#13+
             'FogColor.a = clamp((distance(EyePos.xyz, gl_Vertex.xyz)-gl_Fog.start)*gl_Fog.scale, 0.0, 1.0);'+#10#13+
             'gl_TexCoord[0] = gl_TextureMatrix[0]*gl_Vertex;'                                               +#10#13+
             'gl_TexCoord[1] = gl_Vertex*0.01+Time*0.02;'                                                    +#10#13+
             'gl_TexCoord[2] = gl_Vertex*0.02+Time*0.02;'                                                    +#10#13+
             'gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;'                                         +#10#13+
             '}';

  water_fp = '#ifndef __GLSL_CG_DATA_TYPES'                                                                  +#10#13+
             '# define half float'                                                                           +#10#13+
             '# define half3x3 mat3'                                                                         +#10#13+
             '# define half2 vec2'                                                                           +#10#13+
             '# define half3 vec3'                                                                           +#10#13+
             '# define half4 vec4'                                                                           +#10#13+
             '#endif'                                                                                        +#10#13+
             'uniform vec4  EyePos;'                                                                         +#10#13+
             'uniform sampler2D WaveMap;'                                                                    +#10#13+
             '#ifndef __GLSL_CG_DATA_TYPES'                                                                  +#10#13+
             'uniform sampler2D ReflectionMap;'                                                              +#10#13+
             '#else'                                                                                         +#10#13+
             'uniform sampler2DRect ReflectionMap;'                                                          +#10#13+
             '#endif'                                                                                        +#10#13+
             'varying vec3  EyeVec;'                                                                         +#10#13+
             'varying half4 FogColor;'                                                                       +#10#13+
             'const float cFresnelBias = 0.1;'                                                               +#10#13+
             'const half3 cDeepColor = half3(0, 0.1, 0.2);'                                                  +#10#13+
             'const half3 cShallowColor = half3(0, 0.3, 0.6);'                                               +#10#13+
             'void main()'                                                                                   +#10#13+
             '{'                                                                                             +#10#13+
             'half3 wave0 = texture2D(WaveMap, gl_TexCoord[1].xz).xyz;'                                      +#10#13+
             'half3 wave1 = texture2D(WaveMap, gl_TexCoord[2].xz).xyz;'                                      +#10#13+
             'half3 wave = normalize(wave0 + wave1 - half3(1.0, 1.0, 0.0));'                                 +#10#13+
             'float fDist = 10.0/(length(EyeVec)+1.0);'                                                      +#10#13+
             'float facing = 1.0+EyeVec.y*fDist*(0.1-wave.y*0.01);'                                          +#10#13+
             'float fresnel = cFresnelBias + (1.0-cFresnelBias)*pow(facing, 4.0);'                           +#10#13+
             'half3 waterColor = mix(cDeepColor, cShallowColor, facing);'                                    +#10#13+
             'half2 waveJitter = wave.xy*fDist;'                                                             +#10#13+
             'waveJitter.y = abs(waveJitter.y);'                                                             +#10#13+
             '#ifndef __GLSL_CG_DATA_TYPES'                                                                  +#10#13+
             'vec3 reflecUV = gl_TexCoord[0].xyw;'                                                           +#10#13+
             'reflecUV.xy -= waveJitter*reflecUV.z;'                                                         +#10#13+
             'half4 rColor = texture2DProj(ReflectionMap, reflecUV);'                                        +#10#13+
             '#else'                                                                                         +#10#13+
             'vec3 reflecUV = gl_TexCoord[0].xyw;'                                                           +#10#13+
             'reflecUV.xy -= waveJitter*250*reflecUV.z;'                                                     +#10#13+
             'half4 rColor = texture2DRectProj(ReflectionMap, reflecUV);'                                    +#10#13+
             '#endif'                                                                                        +#10#13+
             'half  hdr = 6.0-5.0*rColor.a;'                                                                 +#10#13+
             'half3 finalColor = mix(waterColor, hdr*rColor.rgb , fresnel);'                                 +#10#13+
             'gl_FragColor = half4(mix(finalColor, FogColor.rgb, FogColor.a), 1.0);'                         +#10#13+
             '}';


//Create
//
constructor TGLSLWater.Create(AOwner: TComponent);
begin
  inherited;

  FMLWater           := TGLMaterialLibrary.Create(self);
  FInitalizeWater    := TGLDirectOpenGL(self.AddNewChild(TGLDirectOpenGL));
  FReflectionObjList := TGLBaseSceneObject(self.AddNewChild(TGLBaseSceneObject));
  FRenderWater       := TGLDirectOpenGL(self.AddNewChild(TGLDirectOpenGL));

  FMLWater.AddTextureMaterial('UnusedMaterial', '');

  with FMLWater.AddTextureMaterial('WaterBump', '') do begin
    Material.Texture.TextureMode   := tmReplace;
    Material.Texture.TextureFormat := tfNormalMap;
  end;

  FInitalizeWater.OnRender := InitalizeWater;
  FRenderWater.OnRender    := RenderWater;
end;

//Destroy
//
destructor TGLSLWater.Destroy;
begin
  inherited;
end;

//InitalizeWater
//
procedure TGLSLWater.InitalizeWater(Sender: TObject; var rci: TGLRenderContextInfo);
var
  w, h : Integer;
  refMat, curMat : TMatrix;
  cameraPosBackup, cameraDirectionBackup : TVector;
  frustumBackup : TFrustum;
  clipPlane : TDoubleHmgPlane;
begin
  supportsGLSL := GL_ARB_shader_objects and GL_ARB_fragment_shader and GL_ARB_vertex_shader;
  enableRectReflection := GL_NV_texture_rectangle and ((not enableGLSL) or GL_EXT_Cg_shader);

  if not enableTex2DReflection then Exit;

  if not Assigned(mirrorTexture) then
    mirrorTexture := TGLTextureHandle.Create;

  glPushAttrib(GL_ENABLE_BIT);
  glPushMatrix;

  // Mirror coordinates
  glLoadMatrixf(@FScene.CurrentBuffer.ModelMatrix);
  refMat:=MakeReflectionMatrix(NullVector, YVector);
  glMultMatrixf(@refMat);
  glGetFloatv(GL_MODELVIEW_MATRIX, @curMat);
  glLoadMatrixf(@FScene.CurrentBuffer.ModelMatrix);
  FScene.CurrentBuffer.PushViewMatrix(curMat);

  glFrontFace(GL_CW);

  glEnable(GL_CLIP_PLANE0);
  SetPlane(clipPlane, PlaneMake(AffineVectorMake(0, 1, 0), VectorNegate(YVector)));
  glClipPlane(GL_CLIP_PLANE0, @clipPlane);

  cameraPosBackup       := rci.cameraPosition;
  cameraDirectionBackup := rci.cameraDirection;
  frustumBackup         := rci.rcci.frustum;
  rci.cameraPosition    := VectorTransform(rci.cameraPosition, refMat);
  rci.cameraDirection   := VectorTransform(rci.cameraDirection, refMat);

  with rci.rcci.frustum do begin
    pLeft   := VectorTransform(pLeft, refMat);
    pRight  := VectorTransform(pRight, refMat);
    pTop    := VectorTransform(pTop, refMat);
    pBottom := VectorTransform(pBottom, refMat);
    pNear   := VectorTransform(pNear, refMat);
    pFar    := VectorTransform(pFar, refMat);
  end;

  glLoadIdentity;
  FCamera.Apply;
  glMultMatrixf(@refMat);

// Объекты, которые должны ортажаться в воде ---------------------------//
  FReflectionObjList.DoRender(rci, true, true);
//----------------------------------------------------------------------//

  rci.cameraPosition  := cameraPosBackup;
  rci.cameraDirection := cameraDirectionBackup;
  rci.rcci.frustum    := frustumBackup;

  // Restore to "normal"
  FScene.CurrentBuffer.PopModelViewMatrix;
  glLoadMatrixf(@FScene.CurrentBuffer.ModelViewMatrix);
  FScene.SetupLights(FScene.CurrentBuffer.LimitOf[limLights]);

  glFrontFace(GL_CCW);
  glPopMatrix;
  glPopAttrib;
  rci.GLStates.ResetGLMaterialColors;
  rci.GLStates.ResetGLCurrentTexture;

  if enableRectReflection then begin
    mirrorTexType := GL_TEXTURE_RECTANGLE_NV;
    w := FSceneViewer.Width;
    h := FSceneViewer.Height;
  end else begin
    mirrorTexType:=GL_TEXTURE_2D;
    w := RoundUpToPowerOf2(FSceneViewer.Width);
    h := RoundUpToPowerOf2(FSceneViewer.Height);
  end;

  if mirrorTexture.Handle=0 then begin
    mirrorTexture.AllocateHandle;
    glBindTexture(mirrorTexType, mirrorTexture.Handle);

    glTexParameteri(mirrorTexType, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(mirrorTexType, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(mirrorTexType, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(mirrorTexType, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

   	glCopyTexImage2d(mirrorTexType, 0, GL_RGBA8, 0, 0, w, h, 0);
  end else begin
    glBindTexture(mirrorTexType, mirrorTexture.Handle);

    glCopyTexSubImage2D(mirrorTexType, 0, 0, 0, 0, 0, w, h);
  end;
  glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT+GL_STENCIL_BUFFER_BIT);
end;

//RenderWater
//
procedure TGLSLWater.RenderWater(Sender: TObject; var rci: TGLRenderContextInfo);
var
   x, y : Integer;
begin
  if not (enableGLSL and enableTex2DReflection) then Exit;

  if not Assigned(reflectionProgram) then begin
    reflectionProgram := TGLProgramHandle.CreateAndAllocate;

    reflectionProgram.AddShader(TGLVertexShaderHandle, water_vp);
    reflectionProgram.AddShader(TGLFragmentShaderHandle, water_fp);
    if not reflectionProgram.LinkProgram then
      raise Exception.Create(reflectionProgram.InfoLog);
    if not reflectionProgram.ValidateProgram then
      raise Exception.Create(reflectionProgram.InfoLog);
  end;
  glBindTexture(mirrorTexType, mirrorTexture.Handle);
  glMatrixMode(GL_TEXTURE);
  SetupReflectionMatrix;
  glMatrixMode(GL_MODELVIEW);
  reflectionProgram.UseProgramObject;
  reflectionProgram.Uniform1f['Time']:=FCadencer.CurrentTime;
  reflectionProgram.Uniform4f['EyePos']:=FCamera.AbsolutePosition;
  reflectionProgram.Uniform1i['ReflectionMap']:=0;
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glBindTexture(GL_TEXTURE_2D, FMLWater.Materials[1].Material.Texture.Handle);
  reflectionProgram.Uniform1i['WaveMap']:=1;
  glActiveTextureARB(GL_TEXTURE0_ARB);

  for y := -10 to 10-1 do begin
    glBegin(GL_QUAD_STRIP);
    for x := -10 to 10 do begin
      glVertex3f(x*FWaterWidth, 0, y*FWaterHeight);
      glVertex3f(x*FWaterWidth, 0, (y+1)*FWaterWidth);
    end;
      glEnd;
  end;
  reflectionProgram.EndUseProgramObject;
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_STENCIL_TEST);
end;

//SetupReflectionMatrix;
//
procedure TGLSLWater.SetupReflectionMatrix;
var
   w, h : Single;
begin
  if mirrorTexType=GL_TEXTURE_2D then begin
    w:=0.5*FSceneViewer.Width/RoundUpToPowerOf2(FSceneViewer.Width);
    h:=0.5*FSceneViewer.Height/RoundUpToPowerOf2(FSceneViewer.Height);
  end else begin
    w:=0.5*FSceneViewer.Width;
    h:=0.5*FSceneViewer.Height;
  end;
  glLoadIdentity;
  glTranslatef(w, h, 0);
  glScalef(w, h, 0);
  FCamera.ApplyPerspective(FSceneViewer.Buffer.ViewPort, FSceneViewer.Width, FSceneViewer.Height, 96);
  FCamera.Apply;
  glScalef(1, -1, 1);
end;

//Initalize;
//
procedure TGLSLWater.Initalize;
begin
  FMLWater.LibMaterialByName('WaterBump').Material.Texture.Image.LoadFromFile(FBumpTex);
  enableGLSL            := true;
  enableTex2DReflection := true;
end;

end.
