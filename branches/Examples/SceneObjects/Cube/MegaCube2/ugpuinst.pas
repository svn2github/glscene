unit ugpuinst;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs,
  //GLS
  GLScene, GLObjects, GLRenderContextInfo, GLVectorFileObjects,
  GLVectorGeometry, OpenGL1x, OpenGLTokens, OpenGLAdapter, GLContext, GLMaterial;


type TBenchGPUInst = class( TGLSceneObject )
  private
    FGLSL: TGLProgramHandle;
    FVArr: array of TAffineVector;
    FNArr: array of TAffineVector;
    FTArr: array of TAffineVector;
    FIArr: array of GLUint;
    FBufs: array[0..3] of Cardinal;
  public
    constructor CreateAsChild( aParent:TGLBaseSceneObject;
      aInstFF:TGLFreeForm ); reintroduce;
    destructor Destroy; override;
    procedure DoRender( var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

implementation


constructor TBenchGPUInst.CreateAsChild( aParent:TGLBaseSceneObject;
  aInstFF:TGLFreeForm );
var
    i,j,k,cnt: integer;
    fg1: TFGVertexIndexList;
    fg2: TFGVertexNormalTexIndexList;
begin

  inherited CreateAsChild( aParent );

  if not(GL.ARB_vertex_buffer_object and GL.ARB_shader_objects and
    GL.ARB_vertex_shader and GL.ARB_fragment_shader)then begin
    ShowMessage('VBO or shader not supported by your hardware');
    Halt;
  end;

  Material := aInstFF.Material;
  FGLSL := TGLProgramHandle.CreateAndAllocate;
  FGLSL.AddShader( TGLVertexShaderHandle,
    '#version 120'#13#10'vec3 v0[20]=vec3[20](vec3(-1.,-1.,-1.),vec3(1.,-1.,-1.),vec3(-1.,1.,-1.),'+
    'vec3(-1.,-1.,1.),vec3(1.,1.,-1.),vec3(-1.,1.,1.),vec3(1.,-1.,1.),vec3(1.,1.,1.),'+
    'vec3(0.,-1.,-1.),vec3(0.,1.,-1.),vec3(0.,-1.,1.),vec3(0.,1.,1.),vec3(-1.,0.,-1.),'+
    'vec3(1.,0.,-1.),vec3(-1.,0.,1.),vec3(1.,0.,1.),vec3(-1.,-1.,0.),vec3(1.,-1.,0.),'+
    'vec3(-1.,1.,0.),vec3(1.,1.,0.));varying vec3 pos;void main(){vec3 v1=5.*v0[int(floor(gl_InstanceID*0.008))];'+
    'vec4 p=gl_Vertex+vec4(v1.x+mod(gl_InstanceID,5)-2.,v1.y+mod(int(gl_InstanceID*.2),5)-2.,v1.z+mod(int(gl_InstanceID*.04),5)-2.,.0);'+
    'pos=vec3(p.x+8,p.y+7,p.z+8)*0.066666;gl_Position=gl_ModelViewProjectionMatrix*p;gl_TexCoord[0]=gl_MultiTexCoord0;}');
  FGLSL.AddShader( TGLFragmentShaderHandle,
    '#version 120'#13#10'uniform sampler2D tex;uniform sampler3D shad;varying vec3 pos;void main(){'+
    'gl_FragColor=texture3D(shad,pos)*texture2D(tex,gl_TexCoord[0].xy);}');
  if not FGLSL.LinkProgram then raise Exception.Create( FGLSL.InfoLog );
  FGLSL.UseProgramObject;
  FGLSL.Uniform1i['tex'] := 0;
  FGLSL.Uniform1i['shad'] := 1;
  FGLSL.EndUseProgramObject;
  if not FGLSL.ValidateProgram then raise Exception.Create( FGLSL.InfoLog );

  with aInstFF.MeshObjects[0] do
    // 3DS
    if FaceGroups[0] is TFGVertexIndexList then begin

      setLength( FVArr, Vertices.Count );
      setLength( FNArr, Vertices.Count );
      setLength( FTArr, Vertices.Count );
      system.Move( Vertices.List[0], FVArr[0], length(FVArr) * 12 );
      system.Move( Normals.List[0], FNArr[0], length(FNArr) * 12 );
      system.Move( TexCoords.List[0], FTArr[0], length(FTArr) * 12 );

      fg1 := TFGVertexIndexList( FaceGroups[0] );
      setLength( FIArr, fg1.VertexIndices.Count );
      system.Move( fg1.VertexIndices.List[0], FIArr[0], length(FIArr) * 4 );

    end
    // OBJ
    else begin

    end;

  gl.GenBuffers(1, @FBufs[0]);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[0]);
  gl.BufferData(GL_ARRAY_BUFFER, 12 * length(FVArr), @FVArr[0], GL_STATIC_DRAW);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);

  gl.GenBuffers(1, @FBufs[1]);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[1]);
  gl.BufferData(GL_ARRAY_BUFFER, 12 * length(FNArr), @FNArr[0].X, GL_STATIC_DRAW);

  gl.GenBuffers(1, @FBufs[2]);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[2]);
  gl.BufferData(GL_ARRAY_BUFFER, 12 * length(FTArr), @FTArr[0].X, GL_STATIC_DRAW);

  gl.GenBuffers(1, @FBufs[3]);
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufs[3]);
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, 4 * length(FIArr), @FIArr[0], GL_STATIC_DRAW);
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

end;


procedure TBenchGPUInst.DoRender( var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin

  Material.Apply(rci);
  FGLSL.UseProgramObject;
  FGLSL.Uniform1i['tex'] := 0;
  FGLSL.Uniform1i['shad'] := 1;

  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[0]);
  gl.VertexPointer(3, GL_FLOAT, 0, nil);

  gl.EnableClientState(GL_NORMAL_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[1]);
  gl.NormalPointer(GL_FLOAT, 0, nil);

  gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, FBufs[2]);
  gl.TexCoordPointer(3, GL_FLOAT, 0, nil);

  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, FBufs[3]);

  gl.DrawElementsInstanced(GL_TRIANGLES, length(FIArr), GL_UNSIGNED_INT, nil, 2500);

  gl.DisableClientState(GL_VERTEX_ARRAY);
  gl.DisableClientState(GL_NORMAL_ARRAY);
  gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);

  FGLSL.EndUseProgramObject;
  Material.UnApply(rci);

end;


destructor TBenchGPUInst.Destroy;
begin

  inherited;

end;


end.
