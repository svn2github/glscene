unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GLScene, GLMesh, GLWin32Viewer, ExtCtrls, GLCadencer,
  GLVectorGeometry, GLVectorLists, OpenGL1x, StdCtrls, GLTexture, GLContext,
  GLObjects, GLCoordinates, GLCrossPlatform,
  GLBaseClasses, GLRenderContextInfo, OpenGLTokens;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    DOImmediate: TGLDirectOpenGL;
    GLMesh1: TGLMesh;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    RBBuildList: TRadioButton;
    RBImmediate: TRadioButton;
    DOVertexArray: TGLDirectOpenGL;
    RBVertexArray: TRadioButton;
    DOVBO: TGLDirectOpenGL;
    RBVBO: TRadioButton;
    RBStreamVBO: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormDestroy(Sender: TObject);
    procedure DOImmediateRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure RBImmediateClick(Sender: TObject);
    procedure DOVertexArrayRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure DOVBORender(Sender: TObject; var rci: TGLRenderContextInfo);
  private
     
  public
     
    vertices, normals : TAffineVectorList;
    indices : TIntegerList;
//    indices : array of SmallInt;
    vboVerticesBuffer, vboNormalsBuffer : TGLVBOArrayBufferHandle;
    vboIndicesBuffer : TGLVBOElementArrayHandle;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GLUtils, GLState;

type
   TSmallIntArray = packed array [0..MaxInt shr 2] of SmallInt;
   PSmallIntArray = ^TSmallIntArray;

procedure TForm1.FormCreate(Sender: TObject);
var
   i : Integer;
   f : Single;
begin
   with GLMesh1 do begin
      Mode:=mmTriangleStrip;
      VertexMode:=vmVN;
      Vertices.Clear;
      for i:=-40000 to 15000 do begin
         f:=i*0.001;
         Vertices.AddVertex(AffineVectorMake(f*0.1, Sin(f), -0.1), YVector);
         Vertices.AddVertex(AffineVectorMake(f*0.1, Sin(f), 0.1), YVector);
      end;
      CalcNormals(fwClockWise);
   end;
   vertices:=TAffineVectorList.Create;
   normals:=TAffineVectorList.Create;
   for i:=0 to GLMesh1.Vertices.Count-1 do with GLMesh1.Vertices.Vertices[i] do begin
      vertices.Add(coord);
      normals.Add(normal);
   end;
   indices:=TIntegerList.Create;
   indices.AddSerie(0, 1, vertices.Count);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   vboVerticesBuffer.Free;
   vboNormalsBuffer.Free;
   vboIndicesBuffer.Free;

   indices.Free;
   vertices.Free;
   normals.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS - %.1f MTri/s',
                   [GLSceneViewer1.FramesPerSecond,
                    GLSceneViewer1.FramesPerSecond*indices.Count/(1024*1024)]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.RBImmediateClick(Sender: TObject);
begin
   GLMesh1.Visible:=RBBuildList.Checked;
   DOImmediate.Visible:=RBImmediate.Checked;
   DOVertexArray.Visible:=RBVertexArray.Checked;
   DOVBO.Visible:=RBVBO.Checked or RBStreamVBO.Checked;
end;

procedure TForm1.DOImmediateRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
   i, k : Integer;
   normalsList, verticesList : PAffineVectorArray;
begin
   normalsList:=normals.List;
   verticesList:=vertices.List;
   glBegin(GL_TRIANGLE_STRIP);
   for i:=0 to indices.Count-1 do begin
      k:=indices.List[i];
      glNormal3fv(@normalsList[k]);
      glVertex3fv(@verticesList[k]);
   end;
   glEnd;
end;

procedure TForm1.DOVertexArrayRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
   glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_NORMAL_ARRAY);
   glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   glDisableClientState(GL_COLOR_ARRAY);

   glVertexPointer(3, GL_FLOAT, 0, vertices.List);
   glNormalPointer(GL_FLOAT, 0, normals.List);

   GL.LockArrays(0, vertices.Count);
   glDrawElements(GL_TRIANGLE_STRIP, indices.Count, GL_UNSIGNED_INT, indices.List);
   GL.UnlockArrays;

   glPopClientAttrib;
end;

procedure TForm1.DOVBORender(Sender: TObject; var rci: TGLRenderContextInfo);
var
   arrayMode : TGLEnum;
begin
   if Tag=0 then begin
      vboVerticesBuffer:=TGLVBOArrayBufferHandle.Create;
      vboVerticesBuffer.AllocateHandle;
      vboNormalsBuffer:=TGLVBOArrayBufferHandle.Create;
      vboNormalsBuffer.AllocateHandle;
      vboIndicesBuffer:=TGLVBOElementArrayHandle.Create;
      vboIndicesBuffer.AllocateHandle;
   end;

   if RBStreamVBO.Checked or (Tag=0) then begin
      if RBStreamVBO.Checked then
         arrayMode:=GL_STREAM_DRAW_ARB
      else arrayMode:=GL_STATIC_DRAW_ARB;
//      arrayMode:=GL_DYNAMIC_DRAW_ARB;
      vboVerticesBuffer.Bind;
      vboVerticesBuffer.BufferData(vertices.List, vertices.DataSize, arrayMode);
      vboVerticesBuffer.UnBind;
      vboNormalsBuffer.Bind;
      vboNormalsBuffer.BufferData(normals.List, normals.DataSize, arrayMode);
      vboNormalsBuffer.UnBind;
      vboIndicesBuffer.Bind;
      vboIndicesBuffer.BufferData(indices.List, indices.DataSize, arrayMode);
      vboIndicesBuffer.UnBind;
      Tag:=1;
   end;

   glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
   vboVerticesBuffer.Bind;
   glVertexPointer(3, GL_FLOAT, 0, nil);
   vboVerticesBuffer.UnBind;

   vboNormalsBuffer.Bind;
   glNormalPointer(GL_FLOAT, 0, nil);
   vboNormalsBuffer.UnBind;

   vboIndicesBuffer.Bind;

   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_NORMAL_ARRAY);

   glDrawElements(GL_TRIANGLE_STRIP, indices.Count, GL_UNSIGNED_INT, nil);

   vboIndicesBuffer.UnBind;

   glPopClientAttrib;
end;

end.
