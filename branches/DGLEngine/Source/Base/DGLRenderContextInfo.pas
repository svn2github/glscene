//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{: DGLRenderContextInfo<p>

      Stores contextual info useful during rendering methods.<p>

 <b>Historique : </b><font size=-1><ul>
      <li>21/12/15 - JD -  Imported From GLScene
 </ul></font>
}
unit DGLRenderContextInfo;

interface
{$I GLScene.inc}

uses
  DGLPersistentClasses, DGLTypes, DGLVectorMaths, DGLState, DGLColor,
  DGLPipelineTransformation;

type



  // TRenderContextClippingInfo
  //
  TRenderContextClippingInfo = record
    origin: TVector;
    clippingDirection: TVector;
    viewPortRadius: Single; // viewport bounding radius per distance unit
    nearClippingDistance: Single;
    farClippingDistance: Single;
    frustum: TFrustum;
  end;

  // TRenderContextInfo
  //
  {: Stores contextual info useful during rendering methods. }
  TRenderContextInfo = record
    scene: TObject; //usually TGLScene
    buffer: TObject; //usually TGLSceneBuffer
    cameraPosition: TVector;
    cameraDirection, cameraUp: TVector;
    viewPortSize: TDGLSize;
    renderDPI: Integer;
    materialLibrary: TObject; //usually TGLMaterialLibrary;
    lightmapLibrary: TObject; //usually TGLMaterialLibrary;
    fogDisabledCounter: Integer;
    drawState: TDrawState;
    objectsSorting: TDGLObjectsSorting;
    visibilityCulling: TDGLVisibilityCulling;
    GLStates: TDGLStateCache;
    PipelineTransformation: TDGLTransformation;
    rcci: TRenderContextClippingInfo;
    sceneAmbientColor: TColorVector;
    bufferFaceCull: Boolean;
    bufferLighting: Boolean;
    bufferFog: Boolean;
    bufferDepthTest: Boolean;
    proxySubObject: Boolean;
    ignoreMaterials: Boolean;
    ignoreBlendingRequests: Boolean;
    ignoreDepthRequests: Boolean;
    amalgamating: Boolean;
    lights: TDGLPersistentObjectList;
    afterRenderEffects: TDGLPersistentObjectList;
    currentMaterialLevel: TDGLShaderMaterialLevel;
    primitiveMask: TDGLMeshPrimitives;
    orderCounter: Integer;
  end;
  PRenderContextInfo = ^TRenderContextInfo;

implementation

end.

