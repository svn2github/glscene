unit ucpuinst;

interface

uses
  Classes,
  GLScene, GLObjects, GLRenderContextInfo;

type TBenchCPUInst = class( TGLSceneObject )
  private
    FInstObj: TGLSceneObject;
  public
    constructor CreateAsChild( aParent:TGLBaseSceneObject;
      aInstObj:TGLSceneObject ); reintroduce;
    procedure DoRender( var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

implementation


constructor TBenchCPUInst.CreateAsChild( aParent:TGLBaseSceneObject;
  aInstObj:TGLSceneObject );
begin

  inherited CreateAsChild( aParent );

  FInstObj := aInstObj;

end;


procedure TBenchCPUInst.DoRender( var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
const
    d = 1/15;
var
    i,j,k: integer;
begin

  for i := -7 to 7 do
    for j := -7 to 7 do
      for k := -7 to 7 do
        if((abs(i)>2) and (abs(j)>2)) or
          ((abs(i)>2) and (abs(k)>2)) or
          ((abs(j)>2) and (abs(k)>2)) then begin
          FInstObj.position.SetPoint(i, j, k);
          FInstObj.Material.TextureEx[1].TextureOffset.setPoint( (i+8)*d, (j+7)*d, (k+8)*d );
          FInstObj.Render(rci);
        end;

end;


end.
