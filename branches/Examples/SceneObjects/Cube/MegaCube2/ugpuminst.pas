unit ugpuminst;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs,
  GLScene, GLObjects, GLRenderContextInfo, GLVectorFileObjects,
  GLVectorGeometry, OpenGL1x, OpenGLTokens, OpenGLAdapter, GLContext, GLMaterial;


type TBenchGPUMInst = class( TGLSceneObject )
  private
{    FGLSL: TGLProgramHandle;
    FVArr: array of TAffineVector;
    FNArr: array of TAffineVector;
    FTArr: array of TAffineVector;
    FIArr: array of GLUint;
    FBufs: array[0..3] of Cardinal;   }
  public
    constructor CreateAsChild( aParent:TGLBaseSceneObject );
    procedure DoRender( var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

implementation


constructor TBenchGPUMInst.CreateAsChild( aParent:TGLBaseSceneObject );
{var
    i,j,k,cnt: integer;
    fg1: TFGVertexIndexList;
    fg2: TFGVertexNormalTexIndexList;   }
begin
 // inherited CreateAsChild( aParent );
end;


procedure TBenchGPUMInst.DoRender( var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
//
end;


end.
