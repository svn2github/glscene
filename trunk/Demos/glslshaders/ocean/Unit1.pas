unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, GLScene, GLTexture, GLObjects, GLUtils,
  ComCtrls, OpenGL1x, GLContext, Jpeg, TGA, VectorGeometry, GLGeomObjects,
  GLCadencer, ExtCtrls, GLUserShader, GLGraph, VectorTypes, GLSkydome,
  VectorLists, GLCrossPlatform, GLMaterial, GLCoordinates, BaseClasses,
  GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    MatLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    DOInitialize: TGLDirectOpenGL;
    GLUserShader1: TGLUserShader;
    GLHeightField1: TGLHeightField;
    GLMemoryViewer1: TGLMemoryViewer;
    GLScene2: TGLScene;
    CameraCubeMap: TGLCamera;
    GLEarthSkyDome1: TGLEarthSkyDome;
    GLSphere2: TGLSphere;
    DOOceanPlane: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure DOInitializeRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TRenderContextInfo; var Continue: Boolean);
    procedure GLHeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure DOOceanPlaneRender(Sender: TObject;
      var rci: TRenderContextInfo);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my, dmx, dmy : Integer;
    programObject : TGLProgramHandle;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

   // Load the cube map which is used both for environment and as reflection texture

   with matLib.LibMaterialByName('water').Material.Texture do begin
      Image.LoadFromFile('noise.bmp');
   end;
   
   with matLib.LibMaterialByName('cubeMap').Material.Texture do begin
      ImageClassName:=TGLCubeMapImage.ClassName;
      with Image as TGLCubeMapImage do begin
         // Load all 6 texture map components of the cube map
         // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
         // and follow the RenderMan specs/conventions
         Picture[cmtPX].LoadFromFile('cm_left.jpg');
         Picture[cmtNX].LoadFromFile('cm_right.jpg');
         Picture[cmtPY].LoadFromFile('cm_top.jpg');
         Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
         Picture[cmtPZ].LoadFromFile('cm_back.jpg');
         Picture[cmtNZ].LoadFromFile('cm_front.jpg');
      end;
   end;

   GLMemoryViewer1.RenderCubeMapTextures(matLib.LibMaterialByName('cubeMap').Material.Texture);

   SetCurrentDir(ExtractFilePath(Application.ExeName));

//   GLHeightField1.ObjectStyle:=GLHeightField1.ObjectStyle+[osDirectDraw];
end;

procedure TForm1.DOInitializeRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
   if not (    GL_ARB_shader_objects and GL_ARB_vertex_program and GL_ARB_vertex_shader
           and GL_ARB_fragment_shader) then begin
      ShowMessage('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
      Halt;
   end;

   if DOInitialize.Tag<>0 then Exit;
   DOInitialize.Tag:=1;

   GLMemoryViewer1.Buffer.RenderingContext.ShareLists(GLSceneViewer1.Buffer.RenderingContext);

   programObject:=TGLProgramHandle.CreateAndAllocate;

   programObject.AddShader(TGLVertexShaderHandle, String(LoadAnsiStringFromFile('ocean_vp.glsl')), True);
   programObject.AddShader(TGLFragmentShaderHandle, String(LoadAnsiStringFromFile('ocean_fp.glsl')), True);

   if not programObject.LinkProgram then 
      raise Exception.Create(programObject.InfoLog);

   if not programObject.ValidateProgram then
      raise Exception.Create(programObject.InfoLog);

   // initialize the heightmap
   with MatLib.LibMaterialByName('water') do begin
      PrepareBuildList;
      rci.GLStates.ActiveTexture := 0;
      rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, Material.Texture.Handle);
   end;

   // initialize the heightmap
   with MatLib.LibMaterialByName('cubeMap') do begin
      PrepareBuildList;
      rci.GLStates.ActiveTexture := 1;
      rci.GLStates.SetGLCurrentTexture(1, GL_TEXTURE_CUBE_MAP, Material.Texture.Handle);
      rci.GLStates.ActiveTexture := 0;
   end;

   programObject.UseProgramObject;

   programObject.Uniform1i['NormalMap']:=0;
   programObject.Uniform1i['EnvironmentMap']:=1;

   programObject.EndUseProgramObject;

   CheckOpenGLError;
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TRenderContextInfo);
var
   mat : TMatrix;
   camPos : TVector;
begin
   glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
   InvertMatrix(mat);

   programObject.UseProgramObject;

   programObject.Uniform1f['Time']:=GLCadencer1.CurrentTime*0.05;

   camPos:=GLCamera.AbsolutePosition;
   programObject.Uniform4f['EyePos']:=camPos;
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TRenderContextInfo; var Continue: Boolean);
begin
   programObject.EndUseProgramObject;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      Inc(dmx, mx-x); Inc(dmy, my-y);
   end;
   mx:=x; my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if (dmx<>0) or (dmy<>0) then begin
      GLCamera.MoveAroundTarget(dmy*0.3, dmx*0.3);
      dmx:=0; dmy:=0;
   end;
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLHeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
   z:=0;
end;

const
   cExtent = 200;
var
   vbo : TGLVBOArrayBufferHandle;
   nbVerts : Integer;
procedure TForm1.DOOceanPlaneRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
   x, y : Integer;
   v : TTexPointList;
   cont : Boolean;
begin
   GLUserShader1DoApply(Self, rci);
   glEnableClientState(GL_VERTEX_ARRAY);

   if not Assigned(vbo) then begin
      v:=TTexPointList.Create;

      v.Capacity:=Sqr(cExtent+1);
      y:=-cExtent; while y<cExtent do begin
         x:=-cExtent; while x<=cExtent do begin
            v.Add(y, x);
            v.Add(y+2, x);
            Inc(x, 2);
         end;
         Inc(y, 2);
         v.Add(y, cExtent);
         v.Add(y, -cExtent);
      end;

      vbo:=TGLVBOArrayBufferHandle.CreateAndAllocate();
      vbo.Bind;
      vbo.BufferData(v.List, v.DataSize, GL_STATIC_DRAW_ARB);
      nbVerts:=v.Count;

      glVertexPointer(2, GL_FLOAT, 0, nil);
      glDrawArrays(GL_QUAD_STRIP, 0, nbVerts);

      vbo.UnBind;
      
      v.Free;
   end else begin
      vbo.Bind;

      glVertexPointer(2, GL_FLOAT, 0, nil);
      glDrawArrays(GL_TRIANGLE_STRIP, 0, nbVerts);

      vbo.UnBind;
   end;

   glDisableClientState(GL_VERTEX_ARRAY);
   GLUserShader1DoUnApply(Self, 0, rci, cont);
end;

end.
