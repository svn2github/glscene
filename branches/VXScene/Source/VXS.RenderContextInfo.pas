//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Stores contextual info useful during rendering methods. 
    
}
unit VXS.RenderContextInfo;

interface

{$I VXScene.inc}

uses
  VXS.PersistentClasses, VXS.VectorGeometry, VXS.State,
  VXS.PipelineTransformation, VXS.Color;

type

  TDrawState = (dsRendering, dsPicking, dsPrinting);

  TVXSize = record
    cx: Longint;
    cy: Longint;
  end;

  // TVXx.ObjectsSorting
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
  TVXObjectsSorting = (osInherited, osNone,
    osRenderFarthestFirst, osRenderBlendedLast,
    osRenderNearestFirst);

  // TVXVisibilityCulling
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
  TVXVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

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

  // TVXRenderContextInfo
  //
  { Stores contextual info useful during rendering methods. }
  TVXRenderContextInfo = record
    scene: TObject; //usually TVXScene
    buffer: TObject; //usually TVXSceneBuffer
    cameraPosition: TVector;
    cameraDirection, cameraUp: TVector;
    viewPortSize: TVXSize;
    renderDPI: Integer;
    materialLibrary: TObject; //usually TVXMaterialLibrary;
    lightmapLibrary: TObject; //usually TVXMaterialLibrary;
    fogDisabledCounter: Integer;
    drawState: TDrawState;
    objectsSorting: TVXObjectsSorting;
    visibilityCulling: TVXVisibilityCulling;
    VKStates: TVXStateCache;
    PipelineTransformation: TVXTransformation;
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
    currentMaterialLevel: TVXMaterialLevel;
    primitiveMask: TVXMeshPrimitives;
    orderCounter: Integer;
  end;
  PRenderContextInfo = ^TVXRenderContextInfo;

implementation

end.

