unit uGLFreeForm;

interface

uses
  System.Classes, System.SysUtils,
   
  GLScene, GLObjects, GLVectorFileObjects, GLRenderContextInfo,
  GLVectorGeometry,
  GLContext;

type
  TBenchGLFreeForm = class(TGLFreeForm)
  private
    FGLSL: TGLProgramHandle;
  public
    constructor CreateAsChild(aParent: TGLBaseSceneObject;
      aInstFF: TGLFreeForm); reintroduce;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

implementation

constructor TBenchGLFreeForm.CreateAsChild(aParent: TGLBaseSceneObject;
  aInstFF: TGLFreeForm);
var
  i, j, k, n, m: integer;
  ms, md: TMeshObject;
  av: TAffineVector;
  fg, fg1: TFGVertexIndexList;
  fg2: TFGVertexNormalTexIndexList;
begin
  inherited CreateAsChild(aParent);
  Material := aInstFF.Material;
  FGLSL := TGLProgramHandle.CreateAndAllocate;
  FGLSL.AddShader(TGLVertexShaderHandle,
    'varying vec3 pos;void main(){pos=vec3(gl_Vertex.x+8,gl_Vertex.y+7,gl_Vertex.z+8)*0.066666;'
    + 'gl_Position=ftransform();gl_TexCoord[0]=gl_MultiTexCoord0;}');
  FGLSL.AddShader(TGLFragmentShaderHandle,
    'uniform sampler2D tex;uniform sampler3D shad;varying vec3 pos;void main(){'
    + 'gl_FragColor=texture3D(shad,pos)*texture2D(tex,gl_TexCoord[0].xy);}');
  if not FGLSL.LinkProgram then
    raise Exception.Create(FGLSL.InfoLog);
  FGLSL.UseProgramObject;
  FGLSL.Uniform1i['tex'] := 0;
  FGLSL.Uniform1i['shad'] := 1;
  FGLSL.EndUseProgramObject;
  if not FGLSL.ValidateProgram then
    raise Exception.Create(FGLSL.InfoLog);

  ms := aInstFF.MeshObjects[0];
  md := TMeshObject.CreateOwned(self.MeshObjects);
  md.Mode := momFaceGroups;

  // 3DS
  if ms.FaceGroups[0] is TFGVertexIndexList then
  begin

    fg1 := TFGVertexIndexList(ms.FaceGroups[0]);
    fg := TFGVertexIndexList.CreateOwned(md.FaceGroups);
    fg.Mode := fgmmTriangles;

    md.Vertices.Capacity := ms.Vertices.Count * 2500;
    md.Normals.Capacity := ms.Normals.Count * 2500;
    md.TexCoords.Capacity := ms.TexCoords.Count * 2500;
    fg.VertexIndices.Capacity := fg1.VertexIndices.Count * 2500;

    m := 0;
    for i := -7 to 7 do
      for j := -7 to 7 do
        for k := -7 to 7 do
          if ((abs(i) > 2) and (abs(j) > 2)) or ((abs(i) > 2) and (abs(k) > 2))
            or ((abs(j) > 2) and (abs(k) > 2)) then
          begin

            setvector(av, i, j, k);
            for n := 0 to ms.Vertices.Count - 1 do
              md.Vertices.add(vectoradd(ms.Vertices[n], av));

            md.Normals.add(ms.Normals);
            md.TexCoords.add(ms.TexCoords);

            for n := 0 to fg1.VertexIndices.Count - 1 do
              fg.VertexIndices.add(fg1.VertexIndices[n] + m);
            inc(m, ms.Vertices.Count);

          end;

    self.StructureChanged;

  end
  // OBJ
  else
  begin
  //
  end;
end;

procedure TBenchGLFreeForm.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  FGLSL.UseProgramObject;
  FGLSL.Uniform1i['tex'] := 0;
  FGLSL.Uniform1i['shad'] := 1;

  inherited;
  FGLSL.EndUseProgramObject;
end;

end.
