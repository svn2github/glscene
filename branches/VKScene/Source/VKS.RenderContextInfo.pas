//
// VKScene project, http://glscene.sourceforge.net 
//
{
   Stores contextual info useful during rendering methods. 
    
}
unit VKS.RenderContextInfo;

interface

{$I VKScene.inc}

uses
  VKS.PersistentClasses, VKS.VectorGeometry, VKS.State,
  VKS.PipelineTransformation, VKS.Color;

type

  TDrawState = (dsRendering, dsPicking, dsPrinting);

  TVKSize = record
    cx: Longint;
    cy: Longint;
  end;

  // TVKx.ObjectsSorting
  //
  { Determines if objects are sorted, and how. 
     Sorting is done level by level (and not for all entities), values are : 
      osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
      osNone : do not sort objects.
  osRenderFarthestFirst : render objects whose Position is the farthest from
  the camera first.
      osRenderBlendedLast : opaque objects are not sorted and rendered
        first, blended ones are rendered afterwards and depth sorted.
  osRenderNearestFirst : render objects whose Position is the nearest to
  the camera first.
        }
  TVKObjectsSorting = (osInherited, osNone,
    osRenderFarthestFirst, osRenderBlendedLast,
    osRenderNearestFirst);

  // TVKVisibilityCulling
  //
  { Determines the visibility culling mode.
     Culling is done level by level, allowed values are: 
      vcInherited : use inherited culling value, if selected for the root
        level, defaults to vcNone
      vcNone : no visibility culling is performed
      vcObjectBased : culling is done on a per-object basis, each object may
        or may not be culled base on its own AxisAlignedDimensions,
        culling has no impact on the visibility of its children
      vcHierarchical : culling is performed hierarchically, using hierarchical
        bounding boxes, if a parent is culled, all of its children, whatever their
        culling options are invisible.
       Depending on the structure of your scene the most efficient culling
     method will be either vcObjectBased or vcHierarchical. Also note that if
     you use many objects with "static" geometry and have a T&amp;L graphics
     board, it may be faster not to cull at all (ie. leave this to the hardware). }
  TVKVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

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

  // TVKRenderContextInfo
  //
  { Stores contextual info useful during rendering methods. }
  TVKRenderContextInfo = record
    scene: TObject; //usually TVKScene
    buffer: TObject; //usually TVKSceneBuffer
    cameraPosition: TVector;
    cameraDirection, cameraUp: TVector;
    viewPortSize: TVKSize;
    renderDPI: Integer;
    materialLibrary: TObject; //usually TVKMaterialLibrary;
    lightmapLibrary: TObject; //usually TVKMaterialLibrary;
    fogDisabledCounter: Integer;
    drawState: TDrawState;
    objectsSorting: TVKObjectsSorting;
    visibilityCulling: TVKVisibilityCulling;
    VKStates: TVKStateCache;
    PipelineTransformation: TVKTransformation;
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
    lights: TPersistentObjectList;
    afterRenderEffects: TPersistentObjectList;
    currentMaterialLevel: TVKMaterialLevel;
    primitiveMask: TVKMeshPrimitives;
    orderCounter: Integer;
  end;
  PRenderContextInfo = ^TVKRenderContextInfo;

implementation

end.

