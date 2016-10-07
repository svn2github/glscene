//
// VKScene project, http://glscene.sourceforge.net
//
{
  Vector File related objects for GLScene
  
}
unit VKS.VectorFileObjects;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  //VKS
  VKS.OpenGLAdapter,
  VKS.Scene,
  VKS.VectorGeometry,
  VKS.Texture,
  VKS.Material,
  VKS.Mesh,
  VKS.VectorLists,
  VKS.PersistentClasses,
  VKS.Octree,
  VKS.GeometryBB,
  VKS.ApplicationFileIO,
  VKS.Silhouette,
  VKS.Context,
  VKS.Color,
  VKS.RenderContextInfo,
  VKS.Coordinates,
  VKS.BaseClasses,
  VKS.TextureFormat;

type

  TVKMeshObjectList = class;
  TVKFaceGroups = class;

  // TVKMeshAutoCentering
  //
  TVKMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter, macRestorePosition);
  TVKMeshAutoCenterings = set of TVKMeshAutoCentering;

  // TVKMeshObjectMode
  //
  TVKMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

  // TBaseMeshObject
  //
  { A base class for mesh objects.
     The class introduces a set of vertices and normals for the object but
     does no rendering of its own. }
  TVKBaseMeshObject = class(TPersistentObject)
  private
    { Private Declarations }
    FName: string;
    FVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FVisible: Boolean;

  protected
    { Protected Declarations }
    procedure SetVertices(const val: TAffineVectorList);
    procedure SetNormals(const val: TAffineVectorList);

    procedure ContributeToBarycenter(var currentSum: TAffineVector;
      var nb: Integer); dynamic;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Clears all mesh object data, submeshes, facegroups, etc. }
    procedure Clear; dynamic;

    { Translates all the vertices by the given delta. }
    procedure Translate(const delta: TAffineVector); dynamic;
    { Builds (smoothed) normals for the vertex list. 
       If normalIndices is nil, the method assumes a bijection between
       vertices and normals sets, and when performed, Normals and Vertices
       list will have the same number of items (whatever previously was in
       the Normals list is ignored/removed). 
       If normalIndices is defined, normals will be added to the list and
       their indices will be added to normalIndices. Already defined
       normals and indices are preserved. 
       The only valid modes are currently momTriangles and momTriangleStrip
       (ie. momFaceGroups not supported). }
    procedure BuildNormals(vertexIndices: TIntegerList; mode: TVKMeshObjectMode;
      normalIndices: TIntegerList = nil);
    { Extracts all mesh triangles as a triangles list. 
       The resulting list size is a multiple of 3, each group of 3 vertices
       making up and independant triangle. 
       The returned list can be used independantly from the mesh object
       (all data is duplicated) and should be freed by caller. 
       If texCoords is specified, per vertex texture coordinates will be
       placed there, when available. }
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; dynamic;

    property Name: string read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Vertices: TAffineVectorList read FVertices write SetVertices;
    property Normals: TAffineVectorList read FNormals write SetNormals;
  end;

  TVKSkeletonFrameList = class;

  TVKSkeletonFrameTransform = (sftRotation, sftQuaternion);

  // TVKSkeletonFrame
  //
    { Stores position and rotation for skeleton joints. 
       If you directly alter some values, make sure to call FlushLocalMatrixList
       so that the local matrices will be recalculated (the call to Flush does
       not recalculate the matrices, but marks the current ones as dirty). }
  TVKSkeletonFrame = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TVKSkeletonFrameList;
    FName: string;
    FPosition: TAffineVectorList;
    FRotation: TAffineVectorList;
    FQuaternion: TQuaternionList;
    FLocalMatrixList: PMatrixArray;
    FTransformMode: TVKSkeletonFrameTransform;

  protected
    { Protected Declarations }
    procedure SetPosition(const val: TAffineVectorList);
    procedure SetRotation(const val: TAffineVectorList);
    procedure SetQuaternion(const val: TQuaternionList);

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TVKSkeletonFrameList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TVKSkeletonFrameList read FOwner;
    property Name: string read FName write FName;
    { Position values for the joints. }
    property Position: TAffineVectorList read FPosition write SetPosition;
    { Rotation values for the joints. }
    property Rotation: TAffineVectorList read FRotation write SetRotation;
    { Quaternions are an alternative to Euler rotations to build the
       global matrices for the skeleton bones. }
    property Quaternion: TQuaternionList read FQuaternion write SetQuaternion;
    { TransformMode indicates whether to use Rotation or Quaternion to build
       the local transform matrices. }
    property TransformMode: TVKSkeletonFrameTransform read FTransformMode write
      FTransformMode;

    { Calculate or retrieves an array of local bone matrices.
       This array is calculated on the first call after creation, and the
       first call following a FlushLocalMatrixList. Subsequent calls return
       the same arrays. }
    function LocalMatrixList: PMatrixArray;
    { Flushes (frees) then LocalMatrixList data. 
       Call this function to allow a recalculation of local matrices. }
    procedure FlushLocalMatrixList;
    //: As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: Boolean = True);
  end;

  // TVKSkeletonFrameList
  //
  { A list of TVKSkeletonFrame objects. }
  TVKSkeletonFrameList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GetSkeletonFrame(Index: Integer): TVKSkeletonFrame;

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    //: As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(
      KeepQuaternions: Boolean = True; SetTransformMode: Boolean = True);
    procedure ConvertRotationsToQuaternions(
      KeepRotations: Boolean = True; SetTransformMode: Boolean = True);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TVKSkeletonFrame read GetSkeletonFrame;
      default;
  end;

  TVKSkeleton = class;
  TVKSkeletonBone = class;

  // TVKSkeletonBoneList
  //
    { A list of skeleton bones.  }
  TVKSkeletonBoneList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FSkeleton: TVKSkeleton; // not persistent

  protected
    { Protected Declarations }
    FGlobalMatrix: TMatrix;

    function GetSkeletonBone(Index: Integer): TVKSkeletonBone;
    procedure AfterObjectCreatedByReader(Sender: TObject); override;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TVKSkeleton);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Skeleton: TVKSkeleton read FSkeleton;
    property Items[Index: Integer]: TVKSkeletonBone read GetSkeletonBone; default;

    { Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: Integer): TVKSkeletonBone; virtual;
    { Returns a bone by its Name, nil if not found. }
    function BoneByName(const aName: string): TVKSkeletonBone; virtual;
    { Number of bones (including all children and self). }

    function BoneCount: Integer;

    //: Render skeleton wireframe
    procedure BuildList(var mrci: TVKRenderContextInfo); virtual; abstract;
    procedure PrepareGlobalMatrices; virtual;
  end;

  // TVKSkeletonRootBoneList
  //
    { This list store skeleton root bones exclusively.  }
  TVKSkeletonRootBoneList = class(TVKSkeletonBoneList)
  private
    { Private Declarations }

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    //: Render skeleton wireframe
    procedure BuildList(var mrci: TVKRenderContextInfo); override;

    property GlobalMatrix: TMatrix read FGlobalMatrix write FGlobalMatrix;
  end;

  // TVKSkeletonBone
  //
    { A skeleton bone or node and its children. 
       This class is the base item of the bones hierarchy in a skeletal model.
       The joint values are stored in a TVKSkeletonFrame, but the calculated bone
       matrices are stored here. }
  TVKSkeletonBone = class(TVKSkeletonBoneList)
  private
    { Private Declarations }
    FOwner: TVKSkeletonBoneList; // indirectly persistent
    FBoneID: Integer;
    FName: string;
    FColor: Cardinal;

  protected
    { Protected Declarations }
    function GetSkeletonBone(Index: Integer): TVKSkeletonBone;
    procedure SetColor(const val: Cardinal);

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TVKSkeletonBoneList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    //: Render skeleton wireframe
    procedure BuildList(var mrci: TVKRenderContextInfo); override;

    property Owner: TVKSkeletonBoneList read FOwner;
    property Name: string read FName write FName;
    property BoneID: Integer read FBoneID write FBoneID;
    property Color: Cardinal read FColor write SetColor;
    property Items[Index: Integer]: TVKSkeletonBone read GetSkeletonBone; default;

    { Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: Integer): TVKSkeletonBone; override;
    function BoneByName(const aName: string): TVKSkeletonBone; override;

    { Set the bone's matrix. Becareful using this. }
    procedure SetGlobalMatrix(Matrix: TMatrix); // Ragdoll
    { Set the bone's GlobalMatrix. Used for Ragdoll. }
    procedure SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix); // Ragdoll

    { Calculates the global matrix for the bone and its sub-bone. 
       Call this function directly only the RootBone. }
    procedure PrepareGlobalMatrices; override;
    { Global Matrix for the bone in the current frame.
       Global matrices must be prepared by invoking PrepareGlobalMatrices
       on the root bone. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;

    { Free all sub bones and reset BoneID and Name. }
    procedure Clean; override;
  end;

  TVKSkeletonColliderList = class;

  // TVKSkeletonCollider
  //
  { A general class storing the base level info required for skeleton
     based collision methods. This class is meant to be inherited from
     to create skeleton driven Verlet Constraints, ODE Geoms, etc.
     Overriden classes should be named as TSCxxxxx. }
  TVKSkeletonCollider = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TVKSkeletonColliderList;
    FBone: TVKSkeletonBone;
    FBoneID: Integer;
    FLocalMatrix, FGlobalMatrix: TMatrix;
    FAutoUpdate: Boolean;

  protected
    { Protected Declarations }
    procedure SetBone(const val: TVKSkeletonBone);
    procedure SetLocalMatrix(const val: TMatrix);

  public
    { Public Declarations }
    constructor Create; override;
    constructor CreateOwned(AOwner: TVKSkeletonColliderList);
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    { This method is used to align the colliders and their
       derived objects to their associated skeleton bone.
       Override to set up descendant class alignment properties. }
    procedure AlignCollider; virtual;

    property Owner: TVKSkeletonColliderList read FOwner;
    //: The bone that this collider associates with.
    property Bone: TVKSkeletonBone read FBone write SetBone;
    { Offset and orientation of the collider in the associated
       bone's space. }
    property LocalMatrix: TMatrix read FLocalMatrix write SetLocalMatrix;
    { Global offset and orientation of the collider. This
       gets set in the AlignCollider method. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
  end;

  // TVKSkeletonColliderList
  //
  { List class for storing TVKSkeletonCollider objects. }
  TVKSkeletonColliderList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GetSkeletonCollider(index: Integer): TVKSkeletonCollider;

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure Clear; override;
    { Calls AlignCollider for each collider in the list. }
    procedure AlignColliders;

    property Owner: TPersistent read FOwner;
    property Items[Index: Integer]: TVKSkeletonCollider read GetSkeletonCollider;
      default;
  end;

  TVKBaseMesh = class;

  // TBlendedLerpInfo
  //
  { Small structure to store a weighted lerp for use in blending. }
  TBlendedLerpInfo = record
    frameIndex1, frameIndex2: Integer;
    lerpFactor: Single;
    weight: Single;
    externalPositions: TAffineVectorList;
    externalRotations: TAffineVectorList;
    externalQuaternions: TQuaternionList;
  end;

  // TVKSkeleton
  //
    { Main skeleton object. 
       This class stores the bones hierarchy and animation frames. 
       It is also responsible for maintaining the "CurrentFrame" and allowing
       various frame blending operations. }
  TVKSkeleton = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TVKBaseMesh;
    FRootBones: TVKSkeletonRootBoneList;
    FFrames: TVKSkeletonFrameList;
    FCurrentFrame: TVKSkeletonFrame; // not persistent
    FBonesByIDCache: TList;
    FColliders: TVKSkeletonColliderList;
    FRagDollEnabled: Boolean; // ragdoll
    FMorphInvisibleParts: Boolean;

  protected
    { Protected Declarations }
    procedure SetRootBones(const val: TVKSkeletonRootBoneList);
    procedure SetFrames(const val: TVKSkeletonFrameList);
    function GetCurrentFrame: TVKSkeletonFrame;
    procedure SetCurrentFrame(val: TVKSkeletonFrame);
    procedure SetColliders(const val: TVKSkeletonColliderList);

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TVKBaseMesh);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TVKBaseMesh read FOwner;
    property RootBones: TVKSkeletonRootBoneList read FRootBones write
      SetRootBones;
    property Frames: TVKSkeletonFrameList read FFrames write SetFrames;
    property CurrentFrame: TVKSkeletonFrame read GetCurrentFrame write
      SetCurrentFrame;
    property Colliders: TVKSkeletonColliderList read FColliders write
      SetColliders;

    procedure FlushBoneByIDCache;
    function BoneByID(anID: Integer): TVKSkeletonBone;
    function BoneByName(const aName: string): TVKSkeletonBone;
    function BoneCount: Integer;

    procedure MorphTo(frameIndex: Integer); overload;
    procedure MorphTo(frame: TVKSkeletonFrame); overload;
    procedure Lerp(frameIndex1, frameIndex2: Integer;
      lerpFactor: Single);
    procedure BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);

    { Linearly removes the translation component between skeletal frames.
       This function will compute the translation of the first bone (index 0)
       and linearly subtract this translation in all frames between startFrame
       and endFrame. Its purpose is essentially to remove the 'slide' that
       exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic(startFrame, endFrame: Integer);
    { Removes the absolute rotation component of the skeletal frames. 
       Some formats will store frames with absolute rotation information,
       if this correct if the animation is the "main" animation. 
       This function removes that absolute information, making the animation
       frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta(startFrame, endFrame: Integer);

    { Applies current frame to morph all mesh objects. }
    procedure MorphMesh(normalize: Boolean);

    { Copy bone rotations from reference skeleton. }
    procedure Synchronize(reference: TVKSkeleton);
    { Release bones and frames info. }
    procedure Clear;
    { Backup and prepare the BoneMatrixInvertedMeshes to use with ragdolls }
    procedure StartRagdoll; // ragdoll
    { Restore the BoneMatrixInvertedMeshes to stop the ragdoll }
    procedure StopRagdoll; // ragdoll

    { Turning this option off (by default) alows to increase FPS,
       but may break backwards-compatibility, because some may choose to
       attach other objects to invisible parts. }
    property MorphInvisibleParts: Boolean read FMorphInvisibleParts write
      FMorphInvisibleParts;
  end;

  // TVKMeshObjectRenderingOption
  //
  { Rendering options per TVKMeshObject. 
   
   moroGroupByMaterial : if set, the facegroups will be rendered by material
     in batchs, this will optimize rendering by reducing material switches, but
     also implies that facegroups will not be rendered in the order they are in
     the list.
    }
  TVKMeshObjectRenderingOption = (moroGroupByMaterial);
  TVKMeshObjectRenderingOptions = set of TVKMeshObjectRenderingOption;

  TVBOBuffer = (vbVertices, vbNormals, vbColors, vbTexCoords,
    vbLightMapTexCoords,
    vbTexCoordsEx);
  TVBOBuffers = set of TVBOBuffer;

  // TVKMeshObject
  //
  { Base mesh class. 
     Introduces base methods and properties for mesh objects. 
     Subclasses are named "TMOxxx". }
  TVKMeshObject = class(TVKBaseMeshObject)
  private
    { Private Declarations }
    FOwner: TVKMeshObjectList;
    FExtentCacheRevision: Cardinal;
    FTexCoords: TAffineVectorList; // provision for 3D textures
    FLightMapTexCoords: TAffineVectorList; // reserved for 2D surface needs
    FColors: TVectorList;
    FFaceGroups: TVKFaceGroups;
    FMode: TVKMeshObjectMode;
    FRenderingOptions: TVKMeshObjectRenderingOptions;
    FArraysDeclared: Boolean; // not persistent
    FLightMapArrayEnabled: Boolean; // not persistent
    FLastLightMapIndex: Integer; // not persistent
    FTexCoordsEx: TList;
    FBinormalsTexCoordIndex: Integer;
    FTangentsTexCoordIndex: Integer;
    FLastXOpenGLTexMapping: Cardinal;
    FUseVBO: boolean;
    FVerticesVBO: TVKVBOHandle;
    FNormalsVBO: TVKVBOHandle;
    FColorsVBO: TVKVBOHandle;
    FTexCoordsVBO: array of TVKVBOHandle;
    FLightmapTexCoordsVBO: TVKVBOHandle;
    FValidBuffers: TVBOBuffers;
    FExtentCache: TAABB;

    procedure SetUseVBO(const Value: boolean);
    procedure SetValidBuffers(Value: TVBOBuffers);
  protected
    { Protected Declarations }
    procedure SetTexCoords(const val: TAffineVectorList);
    procedure SetLightmapTexCoords(const val: TAffineVectorList);
    procedure SetColors(const val: TVectorList);

    procedure BufferArrays;

    procedure DeclareArraysToOpenGL(var mrci: TVKRenderContextInfo;
      evenIfAlreadyDeclared: Boolean = False);
    procedure DisableOpenGLArrays(var mrci: TVKRenderContextInfo);

    procedure EnableLightMapArray(var mrci: TVKRenderContextInfo);
    procedure DisableLightMapArray(var mrci: TVKRenderContextInfo);

    procedure SetTexCoordsEx(index: Integer; const val: TVectorList);
    function GetTexCoordsEx(index: Integer): TVectorList;

    procedure SetBinormals(const val: TVectorList);
    function GetBinormals: TVectorList;
    procedure SetBinormalsTexCoordIndex(const val: Integer);
    procedure SetTangents(const val: TVectorList);
    function GetTangents: TVectorList;
    procedure SetTangentsTexCoordIndex(const val: Integer);

    property ValidBuffers: TVBOBuffers read FValidBuffers write SetValidBuffers;
  public
    { Public Declarations }
    { Creates, assigns Owner and adds to list. }
    constructor CreateOwned(AOwner: TVKMeshObjectList);
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; override;
    { Returns number of triangles in the mesh object. }
    function TriangleCount: Integer; dynamic;

    procedure PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering. 
       Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TVKRenderContextInfo); virtual;
    //: Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TVKRenderContextInfo); virtual;

    //: The extents of the object (min and max coordinates)
    procedure GetExtents(out min, max: TAffineVector); overload; virtual;
    procedure GetExtents(out aabb: TAABB); overload; virtual;

    //: Barycenter from vertices data
    function GetBarycenter: TVector;

    //: Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function PointInObject(const aPoint: TAffineVector): Boolean; virtual;

    //: Returns the triangle data for a given triangle
    procedure GetTriangleData(tri: Integer; list: TAffineVectorList;
      var v0, v1, v2: TAffineVector); overload;
    procedure GetTriangleData(tri: Integer; list: TVectorList;
      var v0, v1, v2: TVector); overload;

    //: Sets the triangle data of a given triangle
    procedure SetTriangleData(tri: Integer; list: TAffineVectorList;
      const v0, v1, v2: TAffineVector); overload;
    procedure SetTriangleData(tri: Integer; list: TVectorList;
      const v0, v1, v2: TVector); overload;

    { Build the tangent space from the mesh object's vertex, normal
       and texcoord data, filling the binormals and tangents where
       specified. }
    procedure BuildTangentSpace(
      buildBinormals: Boolean = True;
      buildTangents: Boolean = True);

    property Owner: TVKMeshObjectList read FOwner;
    property Mode: TVKMeshObjectMode read FMode write FMode;
    property TexCoords: TAffineVectorList read FTexCoords write SetTexCoords;
    property LightMapTexCoords: TAffineVectorList read FLightMapTexCoords write
      SetLightMapTexCoords;
    property Colors: TVectorList read FColors write SetColors;
    property FaceGroups: TVKFaceGroups read FFaceGroups;
    property RenderingOptions: TVKMeshObjectRenderingOptions read FRenderingOptions
      write FRenderingOptions;

    { If set, rendering will use VBO's instead of vertex arrays. }
    property UseVBO: boolean read FUseVBO write SetUseVBO;

    { The TexCoords Extension is a list of vector lists that are used
       to extend the vertex data applied during rendering. 

       The lists are applied to the GL_TEXTURE0_ARB + index texture
       environment. This means that if TexCoordsEx 0 or 1 have data it
       will override the TexCoords or LightMapTexCoords repectively.
       Lists are created on demand, meaning that if you request
       TexCoordsEx[4] it will create the list up to and including 4.
       The extensions are only applied to the texture environment if
       they contain data. }
    property TexCoordsEx[index: Integer]: TVectorList read GetTexCoordsEx write
      SetTexCoordsEx;

    { A TexCoordsEx list wrapper for binormals usage,
       returns TexCoordsEx[BinormalsTexCoordIndex]. }
    property Binormals: TVectorList read GetBinormals write SetBinormals;
    { A TexCoordsEx list wrapper for tangents usage,
       returns TexCoordsEx[BinormalsTexCoordIndex]. }
    property Tangents: TVectorList read GetTangents write SetTangents;
    //: Specify the texcoord extension index for binormals (default = 2)
    property BinormalsTexCoordIndex: Integer read FBinormalsTexCoordIndex write
      SetBinormalsTexCoordIndex;
    //: Specify the texcoord extension index for tangents (default = 3)
    property TangentsTexCoordIndex: Integer read FTangentsTexCoordIndex write
      SetTangentsTexCoordIndex;

  end;

  // TVKMeshObjectList
  //
  { A list of TVKMeshObject objects. }
  TVKMeshObjectList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TVKBaseMesh;

    { Resturns True if all its MeshObjects use VBOs. }
    function GetUseVBO: Boolean;
    procedure SetUseVBO(const Value: Boolean);
  protected
    { Protected Declarations }
    function GetMeshObject(Index: Integer): TVKMeshObject;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TVKBaseMesh);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering. 
       Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TVKRenderContextInfo); virtual;
    //: Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TVKRenderContextInfo); virtual;

    procedure MorphTo(morphTargetIndex: Integer);
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer;
      lerpFactor: Single);
    function MorphTargetCount: Integer;

    procedure GetExtents(out min, max: TAffineVector);
    procedure Translate(const delta: TAffineVector);
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList;
    { Returns number of triangles in the meshes of the list. }
    function TriangleCount: Integer;

    { Build the tangent space from the mesh object's vertex, normal
       and texcoord data, filling the binormals and tangents where
       specified. }
    procedure BuildTangentSpace(
      buildBinormals: Boolean = True;
      buildTangents: Boolean = True);

    { If set, rendering will use VBO's instead of vertex arrays.
       Resturns True if all its MeshObjects use VBOs. }
    property UseVBO: Boolean read GetUseVBO write SetUseVBO;

    //: Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function FindMeshByName(MeshName: string): TVKMeshObject;

    property Owner: TVKBaseMesh read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TVKMeshObject read GetMeshObject; default;
  end;

  TVKMeshObjectListClass = class of TVKMeshObjectList;

  TVKMeshMorphTargetList = class;

  // TVKMeshMorphTarget
  //
  { A morph target, stores alternate lists of vertices and normals. }
  TVKMeshMorphTarget = class(TVKBaseMeshObject)
  private
    { Private Declarations }
    FOwner: TVKMeshMorphTargetList;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TVKMeshMorphTargetList);
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TVKMeshMorphTargetList read FOwner;
  end;

  // TVKMeshMorphTargetList
  //
  { A list of TVKMeshMorphTarget objects. }
  TVKMeshMorphTargetList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GeTVKMeshMorphTarget(Index: Integer): TVKMeshMorphTarget;

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Translate(const delta: TAffineVector);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TVKMeshMorphTarget read GeTVKMeshMorphTarget;
      default;
  end;

  // TVKMorphableMeshObject
  //
  { Mesh object with support for morph targets. 
     The morph targets allow to change vertices and normals according to pre-
     existing "morph targets". }
  TVKMorphableMeshObject = class(TVKMeshObject)
  private
    { Private Declarations }
    FMorphTargets: TVKMeshMorphTargetList;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    procedure Translate(const delta: TAffineVector); override;

    procedure MorphTo(morphTargetIndex: Integer); virtual;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer;
      lerpFactor: Single); virtual;

    property MorphTargets: TVKMeshMorphTargetList read FMorphTargets;
  end;

  // TVertexBoneWeight
  //
  TVertexBoneWeight = packed record
    BoneID: Integer;
    Weight: Single;
  end;

  TVertexBoneWeightArray = array[0..MaxInt div (2*SizeOf(TVertexBoneWeight))] of TVertexBoneWeight;
  PVertexBoneWeightArray = ^TVertexBoneWeightArray;
  TVerticesBoneWeights = array[0..MaxInt div (2*SizeOf(PVertexBoneWeightArray))] of PVertexBoneWeightArray;
  PVerticesBoneWeights = ^TVerticesBoneWeights;
  TVertexBoneWeightDynArray = array of TVertexBoneWeight;

  // TVKSkeletonMeshObject
  //
    { A mesh object with vertice bone attachments. 
       The class adds per vertex bone weights to the standard morphable mesh. 
       The TVertexBoneWeight structures are accessed via VerticesBonesWeights,
       they must be initialized by adjusting the BonesPerVertex and
       VerticeBoneWeightCount properties, you can also add vertex by vertex
       by using the AddWeightedBone method. 
       When BonesPerVertex is 1, the weight is ignored (set to 1.0). }
  TVKSkeletonMeshObject = class(TVKMorphableMeshObject)
  private
    { Private Declarations }
    FVerticesBonesWeights: PVerticesBoneWeights;
    FVerticeBoneWeightCount, FVerticeBoneWeightCapacity: Integer;
    FBonesPerVertex: Integer;
    FLastVerticeBoneWeightCount, FLastBonesPerVertex: Integer; // not persistent
    FBoneMatrixInvertedMeshes: TList; // not persistent
    FBackupInvertedMeshes: TList; // ragdoll
    procedure BackupBoneMatrixInvertedMeshes; // ragdoll
    procedure RestoreBoneMatrixInvertedMeshes; // ragdoll
  protected
    { Protected Declarations }
    procedure SetVerticeBoneWeightCount(const val: Integer);
    procedure SetVerticeBoneWeightCapacity(const val: Integer);
    procedure SetBonesPerVertex(const val: Integer);
    procedure ResizeVerticesBonesWeights;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    property VerticesBonesWeights: PVerticesBoneWeights read
      FVerticesBonesWeights;
    property VerticeBoneWeightCount: Integer read FVerticeBoneWeightCount write
      SetVerticeBoneWeightCount;
    property VerticeBoneWeightCapacity: Integer read FVerticeBoneWeightCapacity
      write SetVerticeBoneWeightCapacity;
    property BonesPerVertex: Integer read FBonesPerVertex write
      SetBonesPerVertex;

    function FindOrAdd(boneID: Integer;
      const vertex, normal: TAffineVector): Integer; overload;
    function FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
      const vertex, normal: TAffineVector): Integer; overload;

    procedure AddWeightedBone(aBoneID: Integer; aWeight: Single);
    procedure AddWeightedBones(const boneIDs: TVertexBoneWeightDynArray);
    procedure PrepareBoneMatrixInvertedMeshes;
    procedure ApplyCurrentSkeletonFrame(normalize: Boolean);

  end;

  // TVKFaceGroup
  //
  { Describes a face group of a TVKMeshObject. 
     Face groups should be understood as "a way to use mesh data to render
     a part or the whole mesh object". 
     Subclasses implement the actual behaviours, and should have at least
     one "Add" method, taking in parameters all that is required to describe
     a single base facegroup element. }
  TVKFaceGroup = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TVKFaceGroups;
    FMaterialName: string;
    FMaterialCache: TVKLibMaterial;
    FLightMapIndex: Integer;
    FRenderGroupID: Integer;
      // NOT Persistent, internal use only (rendering options)

  protected
    { Protected Declarations }
    procedure AttachLightmap(lightMap: TVKTexture; var mrci:
      TVKRenderContextInfo);
    procedure AttachOrDetachLightmap(var mrci: TVKRenderContextInfo);

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TVKFaceGroups); virtual;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
    procedure DropMaterialLibraryCache;

    procedure BuildList(var mrci: TVKRenderContextInfo); virtual; abstract;

    { Add to the list the triangles corresponding to the facegroup. 
       This function is used by TVKMeshObjects ExtractTriangles to retrieve
       all the triangles in a mesh. }
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); dynamic;
    { Returns number of triangles in the facegroup. }
    function TriangleCount: Integer; dynamic; abstract;
    { Reverses the rendering order of faces. 
       Default implementation does nothing }
    procedure Reverse; dynamic;

    //: Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    property Owner: TVKFaceGroups read FOwner write FOwner;
    property MaterialName: string read FMaterialName write FMaterialName;
    property MaterialCache: TVKLibMaterial read FMaterialCache;
    { Index of lightmap in the lightmap library. }
    property LightMapIndex: Integer read FLightMapIndex write FLightMapIndex;
  end;

  // TVKFaceGroupMeshMode
  //
  { Known descriptions for face group mesh modes. 
     - fgmmTriangles : issue all vertices with GL_TRIANGLES. 
     - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP. 
     - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
        the same normal for all vertices of a triangle. 
     - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN. 
     - fgmmQuads : issue all vertices with GL_QUADS. }
  TVKFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
    fgmmTriangleFan, fgmmQuads);

  // TFGVertexIndexList
  //
  { A face group based on an indexlist. 
     The index list refers to items in the mesh object (vertices, normals, etc.),
     that are all considered in sync, the render is obtained issueing the items
     in the order given by the vertices.  }
  TFGVertexIndexList = class(TVKFaceGroup)
  private
    { Private Declarations }
    FVertexIndices: TIntegerList;
    FIndexVBO: TVKVBOElementArrayHandle;
    FMode: TVKFaceGroupMeshMode;

    procedure SetupVBO;
    procedure InvalidateVBO;
  protected
    { Protected Declarations }
    procedure SetVertexIndices(const val: TIntegerList);

    procedure AddToList(source, destination: TAffineVectorList;
      indices: TIntegerList);

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TVKRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;
    function TriangleCount: Integer; override;
    procedure Reverse; override;

    procedure Add(idx: Integer);
    procedure GetExtents(var min, max: TAffineVector);
    { If mode is strip or fan, convert the indices to triangle list indices. }
    procedure ConvertToList;

    //: Return the normal from the 1st three points in the facegroup
    function GetNormal: TAffineVector;

    property Mode: TVKFaceGroupMeshMode read FMode write FMode;
    property VertexIndices: TIntegerList read FVertexIndices write
      SetVertexIndices;
  end;

  // TFGVertexNormalTexIndexList
  //
  { Adds normals and texcoords indices. 
     Allows very compact description of a mesh. The Normals ad TexCoords
     indices are optionnal, if missing (empty), VertexIndices will be used. }
  TFGVertexNormalTexIndexList = class(TFGVertexIndexList)
  private
    { Private Declarations }
    FNormalIndices: TIntegerList;
    FTexCoordIndices: TIntegerList;

  protected
    { Protected Declarations }
    procedure SetNormalIndices(const val: TIntegerList);
    procedure SetTexCoordIndices(const val: TIntegerList);

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TVKRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;

    procedure Add(vertexIdx, normalIdx, texCoordIdx: Integer);

    property NormalIndices: TIntegerList read FNormalIndices write
      SetNormalIndices;
    property TexCoordIndices: TIntegerList read FTexCoordIndices write
      SetTexCoordIndices;
  end;

  // TFGIndexTexCoordList
  //
  { Adds per index texture coordinates to its ancestor. 
     Per index texture coordinates allows having different texture coordinates
     per triangle, depending on the face it is used in. }
  TFGIndexTexCoordList = class(TFGVertexIndexList)
  private
    { Private Declarations }
    FTexCoords: TAffineVectorList;

  protected
    { Protected Declarations }
    procedure SetTexCoords(const val: TAffineVectorList);

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TVKRenderContextInfo); override;
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;

    procedure Add(idx: Integer; const texCoord: TAffineVector); overload;
    procedure Add(idx: Integer; const s, t: Single); overload;

    property TexCoords: TAffineVectorList read FTexCoords write SetTexCoords;
  end;

  // TVKFaceGroups
  //
  { A list of TVKFaceGroup objects. }
  TVKFaceGroups = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TVKMeshObject;

  protected
    { Protected Declarations }
    function GetFaceGroup(Index: Integer): TVKFaceGroup;

  public
    { Public Declarations }
    constructor CreateOwned(AOwner: TVKMeshObject);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
    procedure DropMaterialLibraryCache;

    property Owner: TVKMeshObject read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TVKFaceGroup read GetFaceGroup; default;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil);

    { Material Library of the owner TVKBaseMesh. }
    function MaterialLibrary: TVKMaterialLibrary;
    { Sort faces by material. 
       Those without material first in list, followed by opaque materials,
       then transparent materials. }
    procedure SortByMaterial;
  end;

  // TMeshNormalsOrientation
  //
  { Determines how normals orientation is defined in a mesh. 
     - mnoDefault : uses default orientation 
     - mnoInvert : inverse of default orientation 
     - mnoAutoSolid : autocalculate to make the mesh globally solid 
     - mnoAutoHollow : autocalculate to make the mesh globally hollow  }
  TMeshNormalsOrientation = (mnoDefault, mnoInvert);
    //, mnoAutoSolid, mnoAutoHollow);

  // TVKVectorFile
  //
  { Abstract base class for different vector file formats. 
     The actual implementation for these files (3DS, DXF..) must be done
     seperately. The concept for TVKVectorFile is very similar to TGraphic
     (see Delphi Help). }
  TVKVectorFile = class(TVKDataFile)
  private
    { Private Declarations }
    FNormalsOrientation: TMeshNormalsOrientation;

  protected
    { Protected Declarations }
    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation);
      virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    function Owner: TVKBaseMesh;

    property NormalsOrientation: TMeshNormalsOrientation read FNormalsOrientation
      write SetNormalsOrientation;
  end;

  TVKVectorFileClass = class of TVKVectorFile;

  // TVKGLSMVectorFile
  //
  { GLSM (GLScene Mesh) vector file. 
     This corresponds to the 'native' GLScene format, and object persistence
     stream, which should be the 'fastest' of all formats to load, and supports
     all of GLScene features. }
  TVKGLSMVectorFile = class(TVKVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TVKDataFileCapabilities; override;

    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  // TVKBaseMesh
  //
  { Base class for mesh objects. }
  TVKBaseMesh = class(TVKSceneObject)
  private
    { Private Declarations }
    FNormalsOrientation: TMeshNormalsOrientation;
    FMaterialLibrary: TVKMaterialLibrary;
    FLightmapLibrary: TVKMaterialLibrary;
    FAxisAlignedDimensionsCache: TVector;
    FBaryCenterOffsetChanged: Boolean;
    FBaryCenterOffset: TVector;
    FUseMeshMaterials: Boolean;
    FOverlaySkeleton: Boolean;
    FIgnoreMissingTextures: Boolean;
    FAutoCentering: TVKMeshAutoCenterings;
    FAutoScaling: TVKCoordinates;
    FMaterialLibraryCachesPrepared: Boolean;
    FConnectivity: TObject;
    FLastLoadedFilename: string;

  protected
    { Protected Declarations }
    FMeshObjects: TVKMeshObjectList; // a list of mesh objects
    FSkeleton: TVKSkeleton; // skeleton data & frames
    procedure SetUseMeshMaterials(const val: Boolean);
    procedure SetMaterialLibrary(const val: TVKMaterialLibrary);
    procedure SetLightmapLibrary(const val: TVKMaterialLibrary);
    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation);
    procedure SetOverlaySkeleton(const val: Boolean);
    procedure SetAutoScaling(const Value: TVKCoordinates);
    procedure DestroyHandle; override;

    { Invoked after creating a TVKVectorFile and before loading. 
       Triggered by LoadFromFile/Stream and AddDataFromFile/Stream. 
       Allows to adjust/transfer subclass-specific features. }
    procedure PrepareVectorFile(aFile: TVKVectorFile); dynamic;

    { Invoked after a mesh has been loaded/added. 
       Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.
       Allows to adjust/transfer subclass-specific features. }
    procedure PrepareMesh; dynamic;

    { Recursively propagated to mesh object and facegroups. 
       Notifies that they all can establish their material library caches. }
    procedure PrepareMaterialLibraryCache;
    { Recursively propagated to mesh object and facegroups. 
       Notifies that they all should forget their material library caches. }
    procedure DropMaterialLibraryCache;

    { Prepare the texture and materials before rendering. 
       Invoked once, before building the list and NOT while building the list,
       MaterialLibraryCache can be assumed to having been prepared if materials
       are active. Default behaviour is to prepare build lists for the
       meshobjects. }
    procedure PrepareBuildList(var mrci: TVKRenderContextInfo); dynamic;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterOffset: TVector;
    function BarycenterPosition: TVector;
    function BarycenterAbsolutePosition: TVector; override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure DoRender(var rci: TVKRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    { Notifies that geometry data changed, but no re-preparation is needed. 
       Using this method will usually be faster, but may result in incorrect
       rendering, reduced performance and/or invalid bounding box data
       (ie. invalid collision detection). Use with caution. }
    procedure StructureChangedNoPrepare;

    { BEWARE! Utterly inefficient implementation! }
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters:
      TVKSilhouetteParameters): TVKSilhouette; override;

    { This method allows fast shadow volumes for GLActors. 
       If your actor/mesh doesn't change, you don't need to call this.
       It basically caches the connectivity data.}
    procedure BuildSilhouetteConnectivityData;

    property MeshObjects: TVKMeshObjectList read FMeshObjects;
    property Skeleton: TVKSkeleton read FSkeleton;

    { Computes the extents of the mesh.  }
    procedure GetExtents(out min, max: TAffineVector);
    { Computes the barycenter of the mesh.  }
    function GetBarycenter: TAffineVector;
    { Invoked after a mesh has been loaded. 
       Should auto-center according to the AutoCentering property. }
    procedure PerformAutoCentering; dynamic;
    { Invoked after a mesh has been loaded. 
       Should auto-scale the vertices of the meshobjects to AutoScaling the property. }
    procedure PerformAutoScaling; dynamic;
    { Loads a vector file. 
       A vector files (for instance a ".3DS") stores the definition of
       a mesh as well as materials property. 
       Loading a file replaces the current one (if any). }
    procedure LoadFromFile(const filename: string); dynamic;
    { Loads a vector file from a stream. 
       See LoadFromFile. 
       The filename attribute is required to identify the type data you're
       streaming (3DS, OBJ, etc.) }
    procedure LoadFromStream(const filename: string; aStream: TStream); dynamic;
    { Saves to a vector file. 
       Note that only some of the vector files formats can be written to
       by GLScene. }
    procedure SaveToFile(const fileName: string); dynamic;
    { Saves to a vector file in a stream. 
       Note that only some of the vector files formats can be written to
       by GLScene. }
    procedure SaveToStream(const fileName: string; aStream: TStream); dynamic;

    { Loads additionnal data from a file. 
       Additionnal data could be more animation frames or morph target. 
       The VectorFile importer must be able to handle addition of data
       flawlessly. }
    procedure AddDataFromFile(const filename: string); dynamic;
    { Loads additionnal data from stream. 
       See AddDataFromFile. }
    procedure AddDataFromStream(const filename: string; aStream: TStream);
      dynamic;

    { Returns the filename of the last loaded file, or a blank string if not
       file was loaded (or if the mesh was dinamically built). This does not
       take into account the data added to the mesh (through AddDataFromFile)
       or saved files.}
    function LastLoadedFilename: string;

    { Determines if a mesh should be centered and how. 
       AutoCentering is performed  only  after loading a mesh, it has
       no effect on already loaded mesh data or when adding from a file/stream. 
       If you want to alter mesh data, use direct manipulation methods
       (on the TVKMeshObjects). }
    property AutoCentering: TVKMeshAutoCenterings read FAutoCentering write
      FAutoCentering default [];

    { Scales vertices to a AutoScaling. 
       AutoScaling is performed  only  after loading a mesh, it has
       no effect on already loaded mesh data or when adding from a file/stream. 
       If you want to alter mesh data, use direct manipulation methods
       (on the TVKMeshObjects). }
    property AutoScaling: TVKCoordinates read FAutoScaling write FAutoScaling;

    { Material library where mesh materials will be stored/retrieved. 
       If this property is not defined or if UseMeshMaterials is false,
       only the FreeForm's material will be used (and the mesh's materials
       will be ignored. }
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Defines wether materials declared in the vector file mesh are used. 
       You must also define the MaterialLibrary property. }
    property UseMeshMaterials: Boolean read FUseMeshMaterials write
      SetUseMeshMaterials default True;
    { LightMap library where lightmaps will be stored/retrieved. 
       If this property is not defined, lightmaps won't be used.
       Lightmaps currently *always* use the second texture unit (unit 1),
       and may interfere with multi-texture materials. }
    property LightmapLibrary: TVKMaterialLibrary read FLightmapLibrary write
      SetLightmapLibrary;
    { If True, exceptions about missing textures will be ignored. 
       Implementation is up to the file loader class (ie. this property
       may be ignored by some loaders) }
    property IgnoreMissingTextures: Boolean read FIgnoreMissingTextures write
      FIgnoreMissingTextures default False;

    { Normals orientation for owned mesh.  }
    property NormalsOrientation: TMeshNormalsOrientation read FNormalsOrientation
      write SetNormalsOrientation default mnoDefault;

    { Request rendering of skeleton bones over the mesh. }
    property OverlaySkeleton: Boolean read FOverlaySkeleton write
      SetOverlaySkeleton default False;

  end;

  // TVKFreeForm
  //
  { Container objects for a vector file mesh.
     FreeForms allows loading and rendering vector files (like 3DStudio
     ".3DS" file) in GLScene.  Meshes can be loaded with the LoadFromFile
     method. 
     A FreeForm may contain more than one mesh, but they will all be handled
     as a single object in a scene. }
  TVKFreeForm = class(TVKBaseMesh)
  private
    { Private Declarations }
    FOctree: TOctree;

  protected
    { Protected Declarations }
    function GetOctree: TOctree;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
      const velocity, radius: Single;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
    function OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
    { Returns true if Point is inside the free form - this will only work
    properly on closed meshes. Requires that Octree has been prepared.}
    function OctreePointInMesh(const Point: TVector): boolean;
    function OctreeAABBIntersect(const AABB: TAABB; objMatrix, invObjMatrix:
      TMatrix; triangles: TAffineVectorList = nil): boolean;
    //         TODO:  function OctreeSphereIntersect

             { Octree support *experimental*. 
                Use only if you understand what you're doing! }
    property Octree: TOctree read GetOctree;
    procedure BuildOctree(TreeDepth: integer = 3);

  published
    { Published Declarations }
    property AutoCentering;
    property AutoScaling;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
  end;

  // TVKActorOption
  //
  { Miscellanious actor options. 
      
      aoSkeletonNormalizeNormals : if set the normals of a skeleton-animated
         mesh will be normalized, this is not required if no normals-based texture
         coordinates generation occurs, and thus may be unset to improve performance.
       }
  TVKActorOption = (aoSkeletonNormalizeNormals);
  TVKActorOptions = set of TVKActorOption;

const
  cDefaultGLActorOptions = [aoSkeletonNormalizeNormals];

type

  TVKActor = class;

  // TVKActorAnimationReference
  //
  TVKActorAnimationReference = (aarMorph, aarSkeleton, aarNone);

  // TVKActorAnimation
  //
    { An actor animation sequence. 
       An animation sequence is a named set of contiguous frames that can be used
       for animating an actor. The referred frames can be either morph or skeletal
       frames (choose which via the Reference property). 
       An animation can be directly "played" by the actor by selecting it with
       SwitchAnimation, and can also be "blended" via a TVKAnimationControler. }
  TVKActorAnimation = class(TCollectionItem)
  private
    { Private Declarations }
    FName: string;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FReference: TVKActorAnimationReference;

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    function FrameCount: Integer;
    procedure SetStartFrame(const val: Integer);
    procedure SetEndFrame(const val: Integer);
    procedure SetReference(val: TVKActorAnimationReference);
    procedure SetAsString(const val: string);
    function GetAsString: string;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property AsString: string read GetAsString write SetAsString;

    function OwnerActor: TVKActor;

    { Linearly removes the translation component between skeletal frames. 
       This function will compute the translation of the first bone (index 0)
       and linearly subtract this translation in all frames between startFrame
       and endFrame. Its purpose is essentially to remove the 'slide' that
       exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic;
    { Removes the absolute rotation component of the skeletal frames. 
       Some formats will store frames with absolute rotation information,
       if this correct if the animation is the "main" animation. 
       This function removes that absolute information, making the animation
       frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta;

  published
    { Published Declarations }
    property Name: string read FName write FName;
    { Index of the initial frame of the animation. }
    property StartFrame: Integer read FStartFrame write SetStartFrame;
    { Index of the final frame of the animation. }
    property EndFrame: Integer read FEndFrame write SetEndFrame;
    { Indicates if this is a skeletal or a morph-based animation. }
    property Reference: TVKActorAnimationReference read FReference write
      SetReference default aarMorph;
  end;

  TVKActorAnimationName = string;

  // TVKActorAnimations
  //
    { Collection of actor animations sequences. }
  TVKActorAnimations = class(TCollection)
  private
    { Private Declarations }
    FOwner: TVKActor;

  protected
    { Protected Declarations }
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKActorAnimation);
    function GetItems(index: Integer): TVKActorAnimation;

  public
    { Public Declarations }
    constructor Create(AOwner: TVKActor);
    function Add: TVKActorAnimation;
    function FindItemID(ID: Integer): TVKActorAnimation;
    function FindName(const aName: string): TVKActorAnimation;
    function FindFrame(aFrame: Integer; aReference: TVKActorAnimationReference):
      TVKActorAnimation;

    procedure SetToStrings(aStrings: TStrings);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const fileName: string);
    procedure LoadFromFile(const fileName: string);

    property Items[index: Integer]: TVKActorAnimation read GetItems write
      SetItems; default;
    function Last: TVKActorAnimation;
  end;

  // TVKBaseAnimationControler
  //
    { Base class for skeletal animation control.  }
  TVKBaseAnimationControler = class(TComponent)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FActor: TVKActor;

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetEnabled(const val: Boolean);
    procedure SetActor(const val: TVKActor);

    procedure DoChange; virtual;
    function Apply(var lerpInfo: TBlendedLerpInfo): Boolean; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Actor: TVKActor read FActor write SetActor;
  end;

  // TVKAnimationControler
  //
    { Controls the blending of an additionnal skeletal animation into an actor. 
       The animation controler allows animating an actor with several animations
       at a time, for instance, you could use a "run" animation as base animation
       (in TVKActor), blend an animation that makes the arms move differently
       depending on what the actor is carrying, along with an animation that will
       make the head turn toward a target. }
  TVKAnimationControler = class(TVKBaseAnimationControler)
  private
    { Private Declarations }
    FAnimationName: TVKActorAnimationName;
    FRatio: Single;

  protected
    { Protected Declarations }
    procedure SetAnimationName(const val: TVKActorAnimationName);
    procedure SetRatio(const val: Single);

    procedure DoChange; override;
    function Apply(var lerpInfo: TBlendedLerpInfo): Boolean; override;

  published
    { Published Declarations }
    property AnimationName: string read FAnimationName write SetAnimationName;
    property Ratio: Single read FRatio write SetRatio;
  end;

  // TActorFrameInterpolation
  //
  { Actor frame-interpolation mode. 
     - afpNone : no interpolation, display CurrentFrame only 
     - afpLinear : perform linear interpolation between current and next frame }
  TActorFrameInterpolation = (afpNone, afpLinear);

  // TActorActionMode
  //
  { Defines how an actor plays between its StartFrame and EndFrame. 
      
      aamNone : no animation is performed
      aamPlayOnce : play from current frame to EndFrame, once end frame has
        been reached, switches to aamNone
      aamLoop : play from current frame to EndFrame, once end frame has
        been reached, sets CurrentFrame to StartFrame
      aamBounceForward : play from current frame to EndFrame, once end frame
        has been reached, switches to aamBounceBackward
      aamBounceBackward : play from current frame to StartFrame, once start
        frame has been reached, switches to aamBounceForward
      aamExternal : Allows for external animation control
       }
  TVKActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
    aamBounceBackward, aamLoopBackward, aamExternal);

  // TVKActor
  //
  { Mesh class specialized in animated meshes. 
     The TVKActor provides a quick interface to animated meshes based on morph
     or skeleton frames, it is capable of performing frame interpolation and
     animation blending (via TVKAnimationControler components). }
  TVKActor = class(TVKBaseMesh)
  private
    { Private Declarations }
    FStartFrame, FEndFrame: Integer;
    FReference: TVKActorAnimationReference;
    FCurrentFrame: Integer;
    FCurrentFrameDelta: Single;
    FFrameInterpolation: TActorFrameInterpolation;
    FInterval: Integer;
    FAnimationMode: TVKActorAnimationMode;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached, FOnStartFrameReached: TNotifyEvent;
    FAnimations: TVKActorAnimations;
    FTargetSmoothAnimation: TVKActorAnimation;
    FControlers: TList;
    FOptions: TVKActorOptions;

  protected
    { Protected Declarations }
    procedure SetCurrentFrame(val: Integer);
    procedure SetStartFrame(val: Integer);
    procedure SetEndFrame(val: Integer);
    procedure SetReference(val: TVKActorAnimationReference);
    procedure SetAnimations(const val: TVKActorAnimations);
    function StoreAnimations: Boolean;
    procedure SetOptions(const val: TVKActorOptions);

    procedure PrepareMesh; override;
    procedure PrepareBuildList(var mrci: TVKRenderContextInfo); override;
    procedure DoAnimate; virtual;

    procedure RegisterControler(aControler: TVKBaseAnimationControler);
    procedure UnRegisterControler(aControler: TVKBaseAnimationControler);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    procedure LoadFromStream(const filename: string; aStream: TStream);
      override;

    procedure SwitchToAnimation(anAnimation: TVKActorAnimation; smooth: Boolean =
      False); overload;
    procedure SwitchToAnimation(const animationName: string; smooth: Boolean =
      False); overload;
    procedure SwitchToAnimation(animationIndex: Integer; smooth: Boolean =
      False); overload;
    function CurrentAnimation: string;

    { Synchronize self animation with an other actor. 
       Copies Start/Current/End Frame values, CurrentFrameDelta,
       AnimationMode and FrameInterpolation. }
    procedure Synchronize(referenceActor: TVKActor);

    { Provides a direct access to FCurrentFrame without any checks.
       Used in TVKActorProxy. }
    procedure SetCurrentFrameDirect(const Value: Integer);

    function NextFrameIndex: Integer;

    procedure NextFrame(nbSteps: Integer = 1);
    procedure PrevFrame(nbSteps: Integer = 1);

    function FrameCount: Integer;

    { Indicates whether the actor is currently swithing animations (with
       smooth interpolation).}
    function isSwitchingAnimation: boolean;

  published
    { Published Declarations }
    property StartFrame: Integer read FStartFrame write SetStartFrame default 0;
    property EndFrame: Integer read FEndFrame write SetEndFrame default 0;

    { Reference Frame Animation mode. 
       Allows specifying if the model is primarily morph or skeleton based. }
    property Reference: TVKActorAnimationReference read FReference write FReference
      default aarMorph;

    { Current animation frame. }
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame
      default 0;
    { Value in the [0; 1] range expressing the delta to the next frame.  }
    property CurrentFrameDelta: Single read FCurrentFrameDelta write
      FCurrentFrameDelta;
    { Frame interpolation mode (afpNone/afpLinear). }
    property FrameInterpolation: TActorFrameInterpolation read
      FFrameInterpolation write FFrameInterpolation default afpLinear;

    { See TVKActorAnimationMode.  }
    property AnimationMode: TVKActorAnimationMode read FAnimationMode write
      FAnimationMode default aamNone;
    { Interval between frames, in milliseconds. }
    property Interval: Integer read FInterval write FInterval;
    { Actor and animation miscellanious options. }
    property Options: TVKActorOptions read FOptions write SetOptions default
      cDefaultGLActorOptions;

    { Triggered after each CurrentFrame change. }
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write
      FOnFrameChanged;
    { Triggered after EndFrame has been reached by progression or "nextframe" }
    property OnEndFrameReached: TNotifyEvent read FOnEndFrameReached write
      FOnEndFrameReached;
    { Triggered after StartFrame has been reached by progression or "nextframe" }
    property OnStartFrameReached: TNotifyEvent read FOnStartFrameReached write
      FOnStartFrameReached;

    { Collection of animations sequences. }
    property Animations: TVKActorAnimations read FAnimations write SetAnimations
      stored StoreAnimations;

    property AutoCentering;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
    property OverlaySkeleton;
  end;

  // TVKVectorFileFormat
  //
  TVKVectorFileFormat = class
  public
    VectorFileClass: TVKVectorFileClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // TVKVectorFileFormatsList
  //
  { Stores registered vector file formats. }
  TVKVectorFileFormatsList = class(TPersistentObjectList)
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: Integer; AClass:
      TVKVectorFileClass);
    function FindExt(ext: string): TVKVectorFileClass;
    function FindFromFileName(const fileName: string): TVKVectorFileClass;
    procedure Remove(AClass: TVKVectorFileClass);
    procedure BuildFilterStrings(vectorFileClass: TVKVectorFileClass;
      out descriptions, filters: string;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(index: Integer;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidVectorFile = class(Exception);

  //: Read access to the list of registered vector file formats
function GetVectorFileFormats: TVKVectorFileFormatsList;
//: A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter: string;
//: A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter: string;
{ Returns an extension by its index in the vector files dialogs filter. 
   Use VectorFileFormatsFilter to obtain the filter. }
function VectorFileFormatExtensionByIndex(index: Integer): string;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string;
  aClass: TVKVectorFileClass);
procedure UnregisterVectorFileClass(aClass: TVKVectorFileClass);

var
  vGLVectorFileObjectsAllocateMaterials: Boolean = True;
    // Mrqzzz : Flag to avoid loading materials (useful for IDE Extentions or scene editors)
  vGLVectorFileObjectsEnableVBOByDefault: Boolean = True;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  VKS.Strings, VKS.XOpenGL, VKS.CrossPlatform, VKS.MeshUtils, VKS.State, VKS.Utils,
  VKS.BaseMeshSilhouette, VKS.VectorTypes;

var
  vVectorFileFormats: TVKVectorFileFormatsList;
  vNextRenderGroupID: Integer = 1;

const
  cAAFHeader: AnsiString = 'AAF';

  // GetVectorFileFormats
  //

function GetVectorFileFormats: TVKVectorFileFormatsList;
begin
  if not Assigned(vVectorFileFormats) then
    vVectorFileFormats := TVKVectorFileFormatsList.Create;
  Result := vVectorFileFormats;
end;

// VectorFileFormatsFilter
//

function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TVKVectorFile, Result, f);
end;

// VectorFileFormatsSaveFilter
//

function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TVKVectorFile, Result, f, False, True);
end;

// RegisterVectorFileFormat
//

procedure RegisterVectorFileFormat(const AExtension, ADescription: string;
  AClass: TVKVectorFileClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterVectorFileClass
//

procedure UnregisterVectorFileClass(AClass: TVKVectorFileClass);
begin
  if Assigned(vVectorFileFormats) then
    vVectorFileFormats.Remove(AClass);
end;

// VectorFileFormatExtensionByIndex
//

function VectorFileFormatExtensionByIndex(index: Integer): string;
begin
  Result := GetVectorFileFormats.FindExtByIndex(index);
end;

// TVKVectorFileFormatsList.Destroy
//

destructor TVKVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

// Add
//

procedure TVKVectorFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TVKVectorFileClass);
var
  newRec: TVKVectorFileFormat;
begin
  newRec := TVKVectorFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    VectorFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

// FindExt
//

function TVKVectorFileFormatsList.FindExt(ext: string): TVKVectorFileClass;
var
  i: Integer;
begin
  ext := AnsiLowerCase(ext);
  for i := Count - 1 downto 0 do
    with TVKVectorFileFormat(Items[I]) do
    begin
      if Extension = ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

// FindFromFileName
//

function TVKVectorFileFormatsList.FindFromFileName(const fileName: string):
  TVKVectorFileClass;
var
  ext: string;
begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(strUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

// Remove
//

procedure TVKVectorFileFormatsList.Remove(AClass: TVKVectorFileClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TVKVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

// BuildFilterStrings
//

procedure TVKVectorFileFormatsList.BuildFilterStrings(
  vectorFileClass: TVKVectorFileClass;
  out descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TVKVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TVKVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(vectorFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and (dfcRead in
        p.VectorFileClass.Capabilities))
      or (formatsThatCanBeSaved and (dfcWrite in
        p.VectorFileClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s',
          [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s',
      [glsAllFilter, filters, descriptions]);
end;

// FindExtByIndex
//

function TVKVectorFileFormatsList.FindExtByIndex(index: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TVKVectorFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TVKVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities))
        or (formatsThatCanBeSaved and (dfcWrite in
          p.VectorFileClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

// ------------------
// ------------------ TBaseMeshObject ------------------
// ------------------

// Create
//

constructor TVKBaseMeshObject.Create;
begin
  FVertices := TAffineVectorList.Create;
  FNormals := TAffineVectorList.Create;
  FVisible := True;
  inherited Create;
end;

// Destroy
//

destructor TVKBaseMeshObject.Destroy;
begin
  FNormals.Free;
  FVertices.Free;
  inherited;
end;

// Assign
//

procedure TVKBaseMeshObject.Assign(Source: TPersistent);
begin
  if Source is TVKBaseMeshObject then
  begin
    FName := TVKBaseMeshObject(Source).Name;
    FVertices.Assign(TVKBaseMeshObject(Source).FVertices);
    FNormals.Assign(TVKBaseMeshObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

// WriteToFiler
//

procedure TVKBaseMeshObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1, added FVisible
    WriteString(FName);
    FVertices.WriteToFiler(writer);
    FNormals.WriteToFiler(writer);
    WriteBoolean(FVisible);
  end;
end;

// ReadFromFiler
//

procedure TVKBaseMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..1] then
    with reader do
    begin
      FName := ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion >= 1 then
        FVisible := ReadBoolean
      else
        FVisible := True;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear
//

procedure TVKBaseMeshObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
end;

// ContributeToBarycenter
//

procedure TVKBaseMeshObject.ContributeToBarycenter(var currentSum: TAffineVector;
  var nb: Integer);
begin
  AddVector(currentSum, FVertices.Sum);
  nb := nb + FVertices.Count;
end;

// Translate
//

procedure TVKBaseMeshObject.Translate(const delta: TAffineVector);
begin
  FVertices.Translate(delta);
end;

// BuildNormals
//

procedure TVKBaseMeshObject.BuildNormals(vertexIndices: TIntegerList; mode:
  TVKMeshObjectMode;
  normalIndices: TIntegerList = nil);
var
  i, base: Integer;
  n: TAffineVector;
  newNormals: TIntegerList;

  function TranslateNewNormal(vertexIndex: Integer; const delta:
    TAffineVector): Integer;
  var
    pv: PAffineVector;
  begin
    result := newNormals[vertexIndex];
    if result < base then
    begin
      result := Normals.Add(NullVector);
      newNormals[vertexIndex] := result;
    end;
    pv := @Normals.List[result];
    AddVector(pv^, delta);
  end;

begin
  if not Assigned(normalIndices) then
  begin
    // build bijection
    Normals.Clear;
    Normals.Count := Vertices.Count;
    case mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with Normals do
            begin
              with Vertices do
              begin
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]],
                  Items[vertexIndices[i + 2]], n);
              end;
              with Normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 3);
            end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with Normals do
            begin
              with Vertices do
              begin
                if (i and 1) = 0 then
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 1]],
                    Items[vertexIndices[i + 2]], n)
                else
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 2]],
                    Items[vertexIndices[i + 1]], n);
              end;
              with Normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 1);
            end;
        end;
    else
      Assert(False);
    end;
    Normals.Normalize;
  end
  else
  begin
    // add new normals
    base := Normals.Count;
    newNormals := TIntegerList.Create;
    newNormals.AddSerie(-1, 0, Vertices.Count);
    case mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i
                + 1]],
                Items[vertexIndices[i + 2]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 3);
          end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              if (i and 1) = 0 then
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]],
                  Items[vertexIndices[i + 2]], n)
              else
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 2]],
                  Items[vertexIndices[i + 1]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 1);
          end;
        end;
    else
      Assert(False);
    end;
    for i := base to Normals.Count - 1 do
      NormalizeVector(Normals.List^[i]);
    newNormals.Free;
  end;
end;

// ExtractTriangles
//

function TVKBaseMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  Result := TAffineVectorList.Create;
  if (Vertices.Count mod 3) = 0 then
  begin
    Result.Assign(Vertices);
    if Assigned(normals) then
      normals.Assign(Self.Normals);
  end;
end;

// SetVertices
//

procedure TVKBaseMeshObject.SetVertices(const val: TAffineVectorList);
begin
  FVertices.Assign(val);
end;

// SetNormals
//

procedure TVKBaseMeshObject.SetNormals(const val: TAffineVectorList);
begin
  FNormals.Assign(val);
end;

// ------------------
// ------------------ TVKSkeletonFrame ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeletonFrame.CreateOwned(aOwner: TVKSkeletonFrameList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  Create;
end;

// Create
//

constructor TVKSkeletonFrame.Create;
begin
  inherited Create;
  FPosition := TAffineVectorList.Create;
  FRotation := TAffineVectorList.Create;
  FQuaternion := TQuaternionList.Create;
  FTransformMode := sftRotation;
end;

// Destroy
//

destructor TVKSkeletonFrame.Destroy;
begin
  FlushLocalMatrixList;
  FRotation.Free;
  FPosition.Free;
  FQuaternion.Free;
  inherited Destroy;
end;

// WriteToFiler
//

procedure TVKSkeletonFrame.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1
    WriteString(FName);
    FPosition.WriteToFiler(writer);
    FRotation.WriteToFiler(writer);
    FQuaternion.WriteToFiler(writer);
    WriteInteger(Integer(FTransformMode));
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonFrame.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FName := ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
      if (archiveVersion = 1) then
      begin
        FQuaternion.ReadFromFiler(reader);
        FTransformMode := TVKSkeletonFrameTransform(ReadInteger);
      end;
    end
  else
    RaiseFilerException(archiveVersion);
  FlushLocalMatrixList;
end;

// SetPosition
//

procedure TVKSkeletonFrame.SetPosition(const val: TAffineVectorList);
begin
  FPosition.Assign(val);
end;

// SetRotation
//

procedure TVKSkeletonFrame.SetRotation(const val: TAffineVectorList);
begin
  FRotation.Assign(val);
end;

// SetQuaternion
//

procedure TVKSkeletonFrame.SetQuaternion(const val: TQuaternionList);
begin
  FQuaternion.Assign(val);
end;

// LocalMatrixList
//

function TVKSkeletonFrame.LocalMatrixList: PMatrixArray;
var
  i: Integer;
  s, c: Single;
  mat, rmat: TMatrix;
  quat: TQuaternion;
begin
  if not Assigned(FLocalMatrixList) then
  begin
    case FTransformMode of
      sftRotation:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TMatrix) * Rotation.Count);
          for i := 0 to Rotation.Count - 1 do
          begin
            if Rotation[i].X <> 0 then
            begin
              SinCosine(Rotation[i].X, s, c);
              mat := CreateRotationMatrixX(s, c);
            end
            else
              mat := IdentityHmgMatrix;
            if Rotation[i].Y <> 0 then
            begin
              SinCosine(Rotation[i].Y, s, c);
              rmat := CreateRotationMatrixY(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            if Rotation[i].Z <> 0 then
            begin
              SinCosine(Rotation[i].Z, s, c);
              rmat := CreateRotationMatrixZ(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            FLocalMatrixList^[i] := mat;
          end;
        end;
      sftQuaternion:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TMatrix) * Quaternion.Count);
          for i := 0 to Quaternion.Count - 1 do
          begin
            quat := Quaternion[i];
            mat := QuaternionToMatrix(quat);
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            mat.W.W := 1;
            FLocalMatrixList^[i] := mat;
          end;
        end;
    end;
  end;
  Result := FLocalMatrixList;
end;

// FlushLocalMatrixList
//

procedure TVKSkeletonFrame.FlushLocalMatrixList;
begin
  if Assigned(FLocalMatrixList) then
  begin
    FreeMem(FLocalMatrixList);
    FLocalMatrixList := nil;
  end;
end;

// ConvertQuaternionsToRotations
//

procedure TVKSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions: Boolean
  = True);
var
  i: integer;
  t: TTransformations;
  m: TMatrix;
begin
  Rotation.Clear;
  for i := 0 to Quaternion.Count - 1 do
  begin
    m := QuaternionToMatrix(Quaternion[i]);
    if MatrixDecompose(m, t) then
      Rotation.Add(t[ttRotateX], t[ttRotateY], t[ttRotateZ])
    else
      Rotation.Add(NullVector);
  end;
  if not KeepQuaternions then
    Quaternion.Clear;
end;

// ConvertRotationsToQuaternions
//

procedure TVKSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations: Boolean =
  True);
var
  i: integer;
  mat, rmat: TMatrix;
  s, c: Single;
begin
  Quaternion.Clear;
  for i := 0 to Rotation.Count - 1 do
  begin
    mat := IdentityHmgMatrix;
    SinCosine(Rotation[i].X, s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Y, s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Z, s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat := MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TVKSkeletonFrameList ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeletonFrameList.CreateOwned(AOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

// Destroy
//

destructor TVKSkeletonFrameList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TVKSkeletonFrameList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Clear
//

procedure TVKSkeletonFrameList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

// GetSkeletonFrame
//

function TVKSkeletonFrameList.GetSkeletonFrame(Index: Integer): TVKSkeletonFrame;
begin
  Result := TVKSkeletonFrame(List^[Index]);
end;

// ConvertQuaternionsToRotations
//

procedure TVKSkeletonFrameList.ConvertQuaternionsToRotations(KeepQuaternions:
  Boolean = True; SetTransformMode: Boolean = True);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertQuaternionsToRotations(KeepQuaternions);
    if SetTransformMode then
      Items[i].TransformMode := sftRotation;
  end;
end;

// ConvertRotationsToQuaternions
//

procedure TVKSkeletonFrameList.ConvertRotationsToQuaternions(KeepRotations: Boolean
  = True; SetTransformMode: Boolean = True);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode := sftQuaternion;
  end;
end;

// ------------------
// ------------------ TVKSkeletonBoneList ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeletonBoneList.CreateOwned(aOwner: TVKSkeleton);
begin
  FSkeleton := aOwner;
  Create;
end;

// Create
//

constructor TVKSkeletonBoneList.Create;
begin
  inherited;
  FGlobalMatrix := IdentityHmgMatrix;
end;

// Destroy
//

destructor TVKSkeletonBoneList.Destroy;
begin
  Clean;
  inherited;
end;

// WriteToFiler
//

procedure TVKSkeletonBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// AfterObjectCreatedByReader
//

procedure TVKSkeletonBoneList.AfterObjectCreatedByReader(Sender: TObject);
begin
  with (Sender as TVKSkeletonBone) do
  begin
    FOwner := Self;
    FSkeleton := Self.Skeleton;
  end;
end;

// GetSkeletonBone
//

function TVKSkeletonBoneList.GetSkeletonBone(Index: Integer): TVKSkeletonBone;
begin
  Result := TVKSkeletonBone(List^[Index]);
end;

// BoneByID
//

function TVKSkeletonBoneList.BoneByID(anID: Integer): TVKSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByID(anID);
    if Assigned(Result) then
      Break;
  end;
end;

// BoneByName
//

function TVKSkeletonBoneList.BoneByName(const aName: string): TVKSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByName(aName);
    if Assigned(Result) then
      Break;
  end;
end;

// BoneCount
//

function TVKSkeletonBoneList.BoneCount: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i].BoneCount);
end;

// PrepareGlobalMatrices
//

procedure TVKSkeletonBoneList.PrepareGlobalMatrices;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TVKSkeletonRootBoneList ------------------
// ------------------

// WriteToFiler
//

procedure TVKSkeletonRootBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonRootBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// BuildList
//

procedure TVKSkeletonRootBoneList.BuildList(var mrci: TVKRenderContextInfo);
var
  i: Integer;
begin
  // root node setups and restore OpenGL stuff
  mrci.GLStates.Disable(stColorMaterial);
  mrci.GLStates.Disable(stLighting);
  glColor3f(1, 1, 1);
  // render root-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TVKSkeletonBone ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeletonBone.CreateOwned(aOwner: TVKSkeletonBoneList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  FSkeleton := aOwner.Skeleton;
  Create;
end;

// Create
//

constructor TVKSkeletonBone.Create;
begin
  FColor := $FFFFFFFF; // opaque white
  inherited;
end;

// Destroy
//

destructor TVKSkeletonBone.Destroy;
begin
  if Assigned(Owner) then
    Owner.Remove(Self);
  inherited Destroy;
end;

// WriteToFiler
//

procedure TVKSkeletonBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
    WriteInteger(FBoneID);
    WriteInteger(Integer(FColor));
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonBone.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FName := ReadString;
      FBoneID := ReadInteger;
      FColor := Cardinal(ReadInteger);
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// BuildList
//

procedure TVKSkeletonBone.BuildList(var mrci: TVKRenderContextInfo);

  procedure IssueColor(color: Cardinal);
  begin
    glColor4f(GetRValue(color) / 255, GetGValue(color) / 255, GetBValue(color) /
      255,
      ((color shr 24) and 255) / 255);
  end;

var
  i: Integer;
begin
  // point for self
  mrci.GLStates.PointSize := 5;
  glBegin(GL_POINTS);
  IssueColor(Color);
  glVertex3fv(@GlobalMatrix.W.X);
  glEnd;
  // parent-self bone line
  if Owner is TVKSkeletonBone then
  begin
    glBegin(GL_LINES);
    glVertex3fv(@TVKSkeletonBone(Owner).GlobalMatrix.W.X);
    glVertex3fv(@GlobalMatrix.W.X);
    glEnd;
  end;
  // render sub-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// GetSkeletonBone
//

function TVKSkeletonBone.GetSkeletonBone(Index: Integer): TVKSkeletonBone;
begin
  Result := TVKSkeletonBone(List^[Index]);
end;

// SetColor
//

procedure TVKSkeletonBone.SetColor(const val: Cardinal);
begin
  FColor := val;
end;

// BoneByID
//

function TVKSkeletonBone.BoneByID(anID: Integer): TVKSkeletonBone;
begin
  if BoneID = anID then
    Result := Self
  else
    Result := inherited BoneByID(anID);
end;

// BoneByName
//

function TVKSkeletonBone.BoneByName(const aName: string): TVKSkeletonBone;
begin
  if Name = aName then
    Result := Self
  else
    Result := inherited BoneByName(aName);
end;

// Clean
//

procedure TVKSkeletonBone.Clean;
begin
  BoneID := 0;
  Name := '';
  inherited;
end;

// PrepareGlobalMatrices
//

procedure TVKSkeletonBone.PrepareGlobalMatrices;
begin
  if (Skeleton.FRagDollEnabled) then
    Exit; // ragdoll
  FGlobalMatrix :=
    MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList^[BoneID],
    TVKSkeletonBoneList(Owner).FGlobalMatrix);
  inherited;
end;

procedure TVKSkeletonBone.SetGlobalMatrix(Matrix: TMatrix); // ragdoll
begin
  FGlobalMatrix := Matrix;
end;

procedure TVKSkeletonBone.SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix);
  // ragdoll
begin
  FGlobalMatrix := MatrixMultiply(RagDollMatrix,
    Skeleton.Owner.InvAbsoluteMatrix);
  inherited;
end;

// ------------------
// ------------------ TVKSkeletonCollider ------------------
// ------------------

// Create
//

constructor TVKSkeletonCollider.Create;
begin
  inherited;
  FLocalMatrix := IdentityHMGMatrix;
  FGlobalMatrix := IdentityHMGMatrix;
  FAutoUpdate := True;
end;

// CreateOwned
//

constructor TVKSkeletonCollider.CreateOwned(AOwner: TVKSkeletonColliderList);
begin
  Create;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// WriteToFiler
//

procedure TVKSkeletonCollider.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FBone) then
      WriteInteger(FBone.BoneID)
    else
      WriteInteger(-1);
    Write(FLocalMatrix, SizeOf(TMatrix));
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonCollider.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FBoneID := ReadInteger;
      Read(FLocalMatrix, SizeOf(TMatrix));
    end
  else
    RaiseFilerException(archiveVersion);
end;

// AlignCollider
//

procedure TVKSkeletonCollider.AlignCollider;
var
  mat: TMatrix;
begin
  if Assigned(FBone) then
  begin
    if Owner.Owner is TVKSkeleton then
      if TVKSkeleton(Owner.Owner).Owner is TVKBaseSceneObject then
        mat := MatrixMultiply(FBone.GlobalMatrix,
          TVKBaseSceneObject(TVKSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
      else
        mat := FBone.GlobalMatrix;
    MatrixMultiply(FLocalMatrix, mat, FGlobalMatrix);
  end
  else
    FGlobalMatrix := FLocalMatrix;
end;

// SetBone
//

procedure TVKSkeletonCollider.SetBone(const val: TVKSkeletonBone);
begin
  if val <> FBone then
    FBone := val;
end;

// SetMatrix
//

procedure TVKSkeletonCollider.SetLocalMatrix(const val: TMatrix);
begin
  FLocalMatrix := val;
end;

// ------------------
// ------------------ TVKSkeletonColliderList ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeletonColliderList.CreateOwned(AOwner: TPersistent);
begin
  Create;
  FOwner := AOwner;
end;

// Destroy
//

destructor TVKSkeletonColliderList.Destroy;
begin
  Clear;
  inherited;
end;

// GetSkeletonCollider
//

function TVKSkeletonColliderList.GetSkeletonCollider(index: Integer):
  TVKSkeletonCollider;
begin
  Result := TVKSkeletonCollider(inherited Get(index));
end;

// ReadFromFiler
//

procedure TVKSkeletonColliderList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := Self;
    if (Owner is TVKSkeleton) and (Items[i].FBoneID <> -1) then
      Items[i].Bone := TVKSkeleton(Owner).BoneByID(Items[i].FBoneID);
  end;
end;

// Clear
//

procedure TVKSkeletonColliderList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := nil;
    Items[i].Free;
  end;
  inherited;
end;

// AlignColliders
//

procedure TVKSkeletonColliderList.AlignColliders;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AutoUpdate then
      Items[i].AlignCollider;
end;

// ------------------
// ------------------ TVKSkeleton ------------------
// ------------------

// CreateOwned
//

constructor TVKSkeleton.CreateOwned(AOwner: TVKBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

// Create
//

constructor TVKSkeleton.Create;
begin
  inherited Create;
  FRootBones := TVKSkeletonRootBoneList.CreateOwned(Self);
  FFrames := TVKSkeletonFrameList.CreateOwned(Self);
  FColliders := TVKSkeletonColliderList.CreateOwned(Self);
end;

// Destroy
//

destructor TVKSkeleton.Destroy;
begin
  FlushBoneByIDCache;
  FCurrentFrame.Free;
  FFrames.Free;
  FRootBones.Free;
  FColliders.Free;
  inherited Destroy;
end;

// WriteToFiler
//

procedure TVKSkeleton.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      if FColliders.Count > 0 then
        WriteInteger(1) // Archive Version 1 : with colliders
      else
        WriteInteger(0); // Archive Version 0
      FRootBones.WriteToFiler(writer);
      FFrames.WriteToFiler(writer);
      if FColliders.Count > 0 then
        FColliders.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//

procedure TVKSkeleton.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
      if (archiveVersion = 1) then
        FColliders.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

// SetRootBones
//

procedure TVKSkeleton.SetRootBones(const val: TVKSkeletonRootBoneList);
begin
  FRootBones.Assign(val);
end;

// SetFrames
//

procedure TVKSkeleton.SetFrames(const val: TVKSkeletonFrameList);
begin
  FFrames.Assign(val);
end;

// GetCurrentFrame
//

function TVKSkeleton.GetCurrentFrame: TVKSkeletonFrame;
begin
  if not Assigned(FCurrentFrame) then
    FCurrentFrame := TVKSkeletonFrame(FFrames.Items[0].CreateClone);
  Result := FCurrentFrame;
end;

// SetCurrentFrame
//

procedure TVKSkeleton.SetCurrentFrame(val: TVKSkeletonFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TVKSkeletonFrame(val.CreateClone);
end;

// SetColliders
//

procedure TVKSkeleton.SetColliders(const val: TVKSkeletonColliderList);
begin
  FColliders.Assign(val);
end;

// FlushBoneByIDCache
//

procedure TVKSkeleton.FlushBoneByIDCache;
begin
  FBonesByIDCache.Free;
  FBonesByIDCache := nil;
end;

// BoneByID
//

function TVKSkeleton.BoneByID(anID: Integer): TVKSkeletonBone;

  procedure CollectBones(bone: TVKSkeletonBone);
  var
    i: Integer;
  begin
    if bone.BoneID >= FBonesByIDCache.Count then
      FBonesByIDCache.Count := bone.BoneID + 1;
    FBonesByIDCache[bone.BoneID] := bone;
    for i := 0 to bone.Count - 1 do
      CollectBones(bone[i]);
  end;

var
  i: Integer;
begin
  if not Assigned(FBonesByIDCache) then
  begin
    FBonesByIDCache := TList.Create;
    for i := 0 to RootBones.Count - 1 do
      CollectBones(RootBones[i]);
  end;
  Result := TVKSkeletonBone(FBonesByIDCache[anID])
end;

// BoneByName
//

function TVKSkeleton.BoneByName(const aName: string): TVKSkeletonBone;
begin
  Result := RootBones.BoneByName(aName);
end;

// BoneCount
//

function TVKSkeleton.BoneCount: Integer;
begin
  Result := RootBones.BoneCount;
end;

// MorphTo
//

procedure TVKSkeleton.MorphTo(frameIndex: Integer);
begin
  CurrentFrame := Frames[frameIndex];
end;

// MorphTo
//

procedure TVKSkeleton.MorphTo(frame: TVKSkeletonFrame);
begin
  CurrentFrame := frame;
end;

// Lerp
//

procedure TVKSkeleton.Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TVKSkeletonFrame.Create;
  FCurrentFrame.TransformMode := Frames[frameIndex1].TransformMode;
  with FCurrentFrame do
  begin
    Position.Lerp(Frames[frameIndex1].Position,
      Frames[frameIndex2].Position, lerpFactor);
    case TransformMode of
      sftRotation: Rotation.AngleLerp(Frames[frameIndex1].Rotation,
          Frames[frameIndex2].Rotation, lerpFactor);
      sftQuaternion: Quaternion.Lerp(Frames[frameIndex1].Quaternion,
          Frames[frameIndex2].Quaternion, lerpFactor);
    end;
  end;
end;

// BlendedLerps
//

procedure TVKSkeleton.BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);
var
  i, n: Integer;
  blendPositions: TAffineVectorList;
  blendRotations: TAffineVectorList;
  blendQuaternions: TQuaternionList;
begin
  n := High(lerpInfos) - Low(lerpInfos) + 1;
  Assert(n >= 1);
  i := Low(lerpInfos);
  if n = 1 then
  begin
    // use fast lerp (no blend)
    with lerpInfos[i] do
      Lerp(frameIndex1, frameIndex2, lerpFactor);
  end
  else
  begin
    if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
    FCurrentFrame := TVKSkeletonFrame.Create;
    FCurrentFrame.TransformMode :=
      Frames[lerpInfos[i].frameIndex1].TransformMode;
    with FCurrentFrame do
    begin
      blendPositions := TAffineVectorList.Create;
      // lerp first item separately
      Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
        Frames[lerpInfos[i].frameIndex2].Position,
        lerpInfos[i].lerpFactor);
      if lerpInfos[i].weight <> 1 then
        Position.Scale(lerpInfos[i].weight);

      Inc(i);
      // combine the other items
      while i <= High(lerpInfos) do
      begin
        if not Assigned(lerpInfos[i].externalPositions) then
        begin
          blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
            Frames[lerpInfos[i].frameIndex2].Position,
            lerpInfos[i].lerpFactor);
          Position.AngleCombine(blendPositions, 1);
        end
        else
          Position.Combine(lerpInfos[i].externalPositions, 1);
        Inc(i);
      end;
      blendPositions.Free;

      i := Low(lerpInfos);
      case TransformMode of
        sftRotation:
          begin
            blendRotations := TAffineVectorList.Create;
            // lerp first item separately
            Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
              Frames[lerpInfos[i].frameIndex2].Rotation,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // combine the other items
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalRotations) then
              begin
                blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                  Frames[lerpInfos[i].frameIndex2].Rotation,
                  lerpInfos[i].lerpFactor);
                Rotation.AngleCombine(blendRotations, 1);
              end
              else
                Rotation.AngleCombine(lerpInfos[i].externalRotations, 1);
              Inc(i);
            end;
            blendRotations.Free;
          end;

        sftQuaternion:
          begin
            blendQuaternions := TQuaternionList.Create;
            // Initial frame lerp
            Quaternion.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
              Frames[lerpInfos[i].frameIndex2].Quaternion,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // Combine the lerped frames together
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalQuaternions) then
              begin
                blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
                  Frames[lerpInfos[i].frameIndex2].Quaternion,
                  lerpInfos[i].lerpFactor);
                Quaternion.Combine(blendQuaternions, 1);
              end
              else
                Quaternion.Combine(lerpInfos[i].externalQuaternions, 1);
              Inc(i);
            end;
            blendQuaternions.Free;
          end;
      end;
    end;
  end;
end;

// MakeSkeletalTranslationStatic
//

procedure TVKSkeleton.MakeSkeletalTranslationStatic(startFrame, endFrame:
  Integer);
var
  delta: TAffineVector;
  i: Integer;
  f: Single;
begin
  if endFrame <= startFrame then
    Exit;
  delta := VectorSubtract(Frames[endFrame].Position[0],
    Frames[startFrame].Position[0]);
  f := -1 / (endFrame - startFrame);
  for i := startFrame to endFrame do
    Frames[i].Position[0] := VectorCombine(Frames[i].Position[0], delta,
      1, (i - startFrame) * f);
end;

// MakeSkeletalRotationDelta
//

procedure TVKSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame: Integer);
var
  i, j: Integer;
  v: TAffineVector;
begin
  if endFrame <= startFrame then
    Exit;
  for i := startFrame to endFrame do
  begin
    for j := 0 to Frames[i].Position.Count - 1 do
    begin
      Frames[i].Position[j] := NullVector;
      v := VectorSubtract(Frames[i].Rotation[j],
        Frames[0].Rotation[j]);
      if VectorNorm(v) < 1e-6 then
        Frames[i].Rotation[j] := NullVector
      else
        Frames[i].Rotation[j] := v;
    end;
  end;
end;

// MorphMesh
//

procedure TVKSkeleton.MorphMesh(normalize: Boolean);
var
  i: Integer;
  mesh: TVKBaseMeshObject;
begin
  if Owner.MeshObjects.Count > 0 then
  begin
    RootBones.PrepareGlobalMatrices;
    if Colliders.Count > 0 then
      Colliders.AlignColliders;

    if FMorphInvisibleParts then
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TVKSkeletonMeshObject) then
          TVKSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end
    else
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TVKSkeletonMeshObject) and mesh.Visible then
          TVKSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end
  end;
end;

// Synchronize
//

procedure TVKSkeleton.Synchronize(reference: TVKSkeleton);
begin
  CurrentFrame.Assign(reference.CurrentFrame);
  MorphMesh(True);
end;

// Clear
//

procedure TVKSkeleton.Clear;
begin
  FlushBoneByIDCache;
  RootBones.Clean;
  Frames.Clear;
  FCurrentFrame.Free;
  FCurrentFrame := nil;
  FColliders.Clear;
end;

procedure TVKSkeleton.StartRagDoll; // ragdoll
var
  i: Integer;
  mesh: TVKBaseMeshObject;
begin
  if FRagDollEnabled then
    Exit
  else
    FRagDollEnabled := True;

  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TVKSkeletonMeshObject then
      begin
        TVKSkeletonMeshObject(mesh).BackupBoneMatrixInvertedMeshes;
        TVKSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
      end;
    end;
  end;
end;

procedure TVKSkeleton.StopRagDoll; // ragdoll
var
  i: Integer;
  mesh: TVKBaseMeshObject;
begin
  FRagDollEnabled := False;
  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TVKSkeletonMeshObject then
        TVKSkeletonMeshObject(mesh).RestoreBoneMatrixInvertedMeshes;
    end;
  end;
end;

// ------------------
// ------------------ TVKMeshObject ------------------
// ------------------

// CreateOwned
//

constructor TVKMeshObject.CreateOwned(AOwner: TVKMeshObjectList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// Create
//

constructor TVKMeshObject.Create;
begin
  FMode := momTriangles;
  FTexCoords := TAffineVectorList.Create;
  FLightMapTexCoords := TAffineVectorList.Create;
  FColors := TVectorList.Create;
  FFaceGroups := TVKFaceGroups.CreateOwned(Self);
  FTexCoordsEx := TList.Create;
  FTangentsTexCoordIndex := 1;
  FBinormalsTexCoordIndex := 2;

  FUseVBO := vGLVectorFileObjectsEnableVBOByDefault;
  inherited;
end;

// Destroy
//

destructor TVKMeshObject.Destroy;
var
  i: Integer;
begin
  FVerticesVBO.Free;
  FNormalsVBO.Free;
  FColorsVBO.Free;
  for i := 0 to high(FTexCoordsVBO) do
    FTexCoordsVBO[i].Free;
  FLightmapTexCoordsVBO.Free;

  FFaceGroups.Free;
  FColors.Free;
  FTexCoords.Free;
  FLightMapTexCoords.Free;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TVectorList(FTexCoordsEx[i]).Free;
  FTexCoordsEx.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

// Assign
//

procedure TVKMeshObject.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);

  if Source is TVKMeshObject then
  begin
    FTexCoords.Assign(TVKMeshObject(Source).FTexCoords);
    FLightMapTexCoords.Assign(TVKMeshObject(Source).FLightMapTexCoords);
    FColors.Assign(TVKMeshObject(Source).FColors);
    FFaceGroups.Assign(TVKMeshObject(Source).FFaceGroups);
    FMode := TVKMeshObject(Source).FMode;
    FRenderingOptions := TVKMeshObject(Source).FRenderingOptions;
    FBinormalsTexCoordIndex := TVKMeshObject(Source).FBinormalsTexCoordIndex;
    FTangentsTexCoordIndex := TVKMeshObject(Source).FTangentsTexCoordIndex;

    // Clear FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
      TVectorList(FTexCoordsEx[I]).Free;

    FTexCoordsEx.Count := TVKMeshObject(Source).FTexCoordsEx.Count;

    // Fill FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      FTexCoordsEx[I] := TVectorList.Create;
      TVectorList(FTexCoordsEx[I]).Assign(TVKMeshObject(Source).FTexCoordsEx[I]);
    end;
  end;
end;

// WriteToFiler
//

procedure TVKMeshObject.WriteToFiler(writer: TVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(3); // Archive Version 3
    FTexCoords.WriteToFiler(writer);
    FLightMapTexCoords.WriteToFiler(writer);
    FColors.WriteToFiler(writer);
    FFaceGroups.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
    WriteInteger(SizeOf(FRenderingOptions));
    Write(FRenderingOptions, SizeOf(FRenderingOptions));
    WriteInteger(FTexCoordsEx.Count);
    for i := 0 to FTexCoordsEx.Count - 1 do
      TexCoordsEx[i].WriteToFiler(writer);
    WriteInteger(BinormalsTexCoordIndex);
    WriteInteger(TangentsTexCoordIndex);
  end;
end;

// ReadFromFiler
//

procedure TVKMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  i, Count, archiveVersion: Integer;
  lOldLightMapTexCoords: TTexPointList;
  tc: TTexPoint;
  size, ro: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..3] then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);

      if archiveVersion = 0 then
      begin
        // FLightMapTexCoords did not exist back than.
        FLightMapTexCoords.Clear;
      end
      else if (archiveVersion = 1) or (archiveVersion = 2) then
      begin
        lOldLightMapTexCoords := TTexPointList.CreateFromFiler(reader);
        for i := 0 to lOldLightMapTexCoords.Count - 1 do
        begin
          tc:=lOldLightMapTexCoords[i];
          FLightMapTexCoords.Add(tc.S, tc.T);
        end;
        lOldLightMapTexCoords.Free;
      end
      else
      begin
        // Load FLightMapTexCoords the normal way.
        FLightMapTexCoords.ReadFromFiler(reader);
      end;

      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode := TVKMeshObjectMode(ReadInteger);
      size := ReadInteger;
      ro := 0;
      Read(ro, size);
      FRenderingOptions := TVKMeshObjectRenderingOptions(Byte(ro));
      if archiveVersion >= 2 then
      begin
        Count := ReadInteger;
        for i := 0 to Count - 1 do
          TexCoordsEx[i].ReadFromFiler(reader);
        BinormalsTexCoordIndex := ReadInteger;
        TangentsTexCoordIndex := ReadInteger;
      end;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear;
//

procedure TVKMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FFaceGroups.Clear;
  FColors.Clear;
  FTexCoords.Clear;
  FLightMapTexCoords.Clear;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TexCoordsEx[i].Clear;
end;

// ExtractTriangles
//

function TVKMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  case mode of
    momTriangles:
      begin
        Result := inherited ExtractTriangles;
        if Assigned(texCoords) then
          texCoords.Assign(Self.TexCoords);
        if Assigned(normals) then
          normals.Assign(Self.Normals);
      end;
    momTriangleStrip:
      begin
        Result := TAffineVectorList.Create;
        ConvertStripToList(Vertices, Result);
        if Assigned(texCoords) then
          ConvertStripToList(Self.TexCoords, texCoords);
        if Assigned(normals) then
          ConvertStripToList(Self.Normals, normals);
      end;
    momFaceGroups:
      begin
        Result := TAffineVectorList.Create;
        FaceGroups.AddToTriangles(Result, texCoords, normals);
      end;
  else
    Result := nil;
    Assert(False);
  end;
end;

// TriangleCount
//

function TVKMeshObject.TriangleCount: Integer;
var
  i: Integer;
begin
  case Mode of
    momTriangles:
      Result := (Vertices.Count div 3);
    momTriangleStrip:
      begin
        Result := Vertices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    momFaceGroups:
      begin
        Result := 0;
        for i := 0 to FaceGroups.Count - 1 do
          Result := Result + FaceGroups[i].TriangleCount;
      end;
  else
    Result := 0;
    Assert(False);
  end;
end;

// PrepareMaterialLibraryCache
//

procedure TVKMeshObject.PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
begin
  FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TVKMeshObject.DropMaterialLibraryCache;
begin
  FaceGroups.DropMaterialLibraryCache;
end;

// GetExtents
//

procedure TVKMeshObject.GetExtents(out min, max: TAffineVector);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  min := FExtentCache.min;
  max := FExtentCache.max;
end;

procedure TVKMeshObject.GetExtents(out aabb: TAABB);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  aabb := FExtentCache;
end;

// GetBarycenter
//

function TVKMeshObject.GetBarycenter: TVector;
var
  dMin, dMax: TAffineVector;
begin
  GetExtents(dMin, dMax);

  Result.X := (dMin.X + dMax.X) / 2;
  Result.Y := (dMin.Y + dMax.Y) / 2;
  Result.Z := (dMin.Z + dMax.Z) / 2;
  Result.W := 0;
end;

// Prepare
//

procedure TVKMeshObject.Prepare;
var
  i: Integer;
begin
  ValidBuffers := [];
  for i := 0 to FaceGroups.Count - 1 do
    FaceGroups[i].Prepare;
end;

// PointInObject
//

function TVKMeshObject.PointInObject(const aPoint: TAffineVector): Boolean;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  Result := (aPoint.X >= min.X) and
            (aPoint.Y >= min.Y) and
            (aPoint.Z >= min.Z) and
            (aPoint.X <= max.X) and
            (aPoint.Y <= max.Y) and
            (aPoint.Z <= max.Z);
end;

// SetTexCoords
//

procedure TVKMeshObject.SetTexCoords(const val: TAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

// SetLightmapTexCoords
//

procedure TVKMeshObject.SetLightmapTexCoords(const val: TAffineVectorList);
begin
  FLightMapTexCoords.Assign(val);
end;

// SetColors
//

procedure TVKMeshObject.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
end;

// SetTexCoordsEx
//

procedure TVKMeshObject.SetTexCoordsEx(index: Integer; const val: TVectorList);
begin
  TexCoordsEx[index].Assign(val);
end;

// GetTexCoordsEx
//

function TVKMeshObject.GetTexCoordsEx(index: Integer): TVectorList;
var
  i: Integer;
begin
  if index > FTexCoordsEx.Count - 1 then
    for i := FTexCoordsEx.Count - 1 to index do
      FTexCoordsEx.Add(TVectorList.Create);
  Result := TVectorList(FTexCoordsEx[index]);
end;

// SetBinormals
//

procedure TVKMeshObject.SetBinormals(const val: TVectorList);
begin
  Binormals.Assign(val);
end;

// GetBinormals
//

function TVKMeshObject.GetBinormals: TVectorList;
begin
  Result := TexCoordsEx[BinormalsTexCoordIndex];
end;

// SetBinormalsTexCoordIndex
//

procedure TVKMeshObject.SetBinormalsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FBinormalsTexCoordIndex then
  begin
    FBinormalsTexCoordIndex := val;
  end;
end;

// SetTangents
//

procedure TVKMeshObject.SetTangents(const val: TVectorList);
begin
  Tangents.Assign(val);
end;

// GetTangents
//

function TVKMeshObject.GetTangents: TVectorList;
begin
  Result := TexCoordsEx[TangentsTexCoordIndex];
end;

// SetTangentsTexCoordIndex
//

procedure TVKMeshObject.SetTangentsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FTangentsTexCoordIndex then
  begin
    FTangentsTexCoordIndex := val;
  end;
end;

// GetTriangleData
//

procedure TVKMeshObject.GetTriangleData(tri: Integer;
  list: TAffineVectorList; var v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.VertexIndices[3 * Count]];
                  v1 := list[fg.VertexIndices[3 * Count + 1]];
                  v2 := list[fg.VertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.VertexIndices[Count]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.VertexIndices[0]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// GetTriangleData
//

procedure TVKMeshObject.GetTriangleData(tri: Integer;
  list: TVectorList; var v0, v1, v2: TVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.VertexIndices[3 * Count]];
                  v1 := list[fg.VertexIndices[3 * Count + 1]];
                  v2 := list[fg.VertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.VertexIndices[Count]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.VertexIndices[0]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// SetTriangleData
//

procedure TVKMeshObject.SetTriangleData(tri: Integer;
  list: TAffineVectorList; const v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.VertexIndices[3 * Count]] := v0;
                  list[fg.VertexIndices[3 * Count + 1]] := v1;
                  list[fg.VertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.VertexIndices[Count]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.VertexIndices[0]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// SetTriangleData
//

procedure TVKMeshObject.SetTriangleData(tri: Integer;
  list: TVectorList; const v0, v1, v2: TVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.VertexIndices[3 * Count]] := v0;
                  list[fg.VertexIndices[3 * Count + 1]] := v1;
                  list[fg.VertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.VertexIndices[Count]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.VertexIndices[0]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TVKMeshObject.SetUseVBO(const Value: boolean);
var
  i: integer;
begin
  if Value = FUseVBO then
    exit;

  if FUseVBO then
  begin
    FreeAndNil(FVerticesVBO);
    FreeAndNil(FNormalsVBO);
    FreeAndNil(FColorsVBO);
    for i := 0 to high(FTexCoordsVBO) do
      FreeAndNil(FTexCoordsVBO[i]);
    FreeAndNil(FLightmapTexCoordsVBO);
  end;

  FValidBuffers := [];

  FUseVBO := Value;
end;

procedure TVKMeshObject.SetValidBuffers(Value: TVBOBuffers);
var
  I: Integer;
begin
  if FValidBuffers <> Value then
  begin
    FValidBuffers := Value;
    if Assigned(FVerticesVBO) then
      FVerticesVBO.NotifyChangesOfData;
    if Assigned(FNormalsVBO) then
      FNormalsVBO.NotifyChangesOfData;
    if Assigned(FColorsVBO) then
      FColorsVBO.NotifyChangesOfData;
    for I := 0 to high(FTexCoordsVBO) do
      if Assigned(FTexCoordsVBO[I]) then
        FTexCoordsVBO[I].NotifyChangesOfData;
    if Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO.NotifyChangesOfData;
  end;
end;

// BuildTangentSpace
//

procedure TVKMeshObject.BuildTangentSpace(
  buildBinormals: Boolean = True;
  buildTangents: Boolean = True);
var
  i, j: Integer;
  v, n, t: array[0..2] of TAffineVector;
  tangent,
    binormal: array[0..2] of TVector;
  vt, tt: TAffineVector;
  interp, dot: Single;

  procedure SortVertexData(sortidx: Integer);
  begin
    if t[0].V[sortidx] < t[1].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[1];
      t[0] := t[1];
      v[1] := vt;
      t[1] := tt;
    end;
    if t[0].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[2];
      t[0] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
    if t[1].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[1];
      tt := t[1];
      v[1] := v[2];
      t[1] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
  end;

begin
  Tangents.Clear;
  Binormals.Clear;
  if buildTangents then
    Tangents.Count := Vertices.Count;
  if buildBinormals then
    Binormals.Count := Vertices.Count;
  for i := 0 to TriangleCount - 1 do
  begin
    // Get triangle data
    GetTriangleData(i, Vertices, v[0], v[1], v[2]);
    GetTriangleData(i, Normals, n[0], n[1], n[2]);
    GetTriangleData(i, TexCoords, t[0], t[1], t[2]);

    for j := 0 to 2 do
    begin
      // Compute tangent
      if buildTangents then
      begin
        SortVertexData(1);

        if (t[2].Y - t[0].Y) = 0 then
          interp := 1
        else
          interp := (t[1].Y - t[0].Y) / (t[2].Y - t[0].Y);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].X + (t[2].X - t[0].X) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].X < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        tangent[j] := VectorMake(VectorNormalize(vt), 0);
      end;

      // Compute Bi-Normal
      if buildBinormals then
      begin
        SortVertexData(0);

        if (t[2].X - t[0].X) = 0 then
          interp := 1
        else
          interp := (t[1].X - t[0].X) / (t[2].X - t[0].X);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].Y + (t[2].Y - t[0].Y) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].Y < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        binormal[j] := VectorMake(VectorNormalize(vt), 0);
      end;
    end;

    if buildTangents then
      SetTriangleData(i, Tangents, tangent[0], tangent[1], tangent[2]);
    if buildBinormals then
      SetTriangleData(i, Binormals, binormal[0], binormal[1], binormal[2]);
  end;
end;

// DeclareArraysToOpenGL
//

procedure TVKMeshObject.DeclareArraysToOpenGL(var mrci: TVKRenderContextInfo;
  evenIfAlreadyDeclared: Boolean = False);
var
  i: Integer;
  currentMapping: Cardinal;
  lists: array[0..4] of pointer;
  tlists: array of pointer;
begin
  if evenIfAlreadyDeclared or (not FArraysDeclared) then
  begin
    FillChar(lists, sizeof(lists), 0);
    SetLength(tlists, FTexCoordsEx.Count);

    // workaround for ATI bug, disable element VBO if
    // inside a display list
    FUseVBO := FUseVBO
      and GL_ARB_vertex_buffer_object
      and not mrci.GLStates.InsideList;

    if not FUseVBO then
    begin
      lists[0] := Vertices.List;
      lists[1] := Normals.List;
      lists[2] := Colors.List;
      lists[3] := TexCoords.List;
      lists[4] := LightMapTexCoords.List;

      for i := 0 to FTexCoordsEx.Count - 1 do
        tlists[i] := TexCoordsEx[i].List;
    end
    else
    begin
      BufferArrays;
    end;

    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
      begin
        if FUseVBO then
          FNormalsVBO.Bind;
        glEnableClientState(GL_NORMAL_ARRAY);
        glNormalPointer(GL_FLOAT, 0, lists[1]);
      end
      else
        glDisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
      begin
        if FUseVBO then
          FColorsVBO.Bind;
        glEnableClientState(GL_COLOR_ARRAY);
        glColorPointer(4, GL_FLOAT, 0, lists[2]);
      end
      else
        glDisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
      begin
        if FUseVBO then
          FTexCoordsVBO[0].Bind;
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[3]);
      end
      else
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      if GL_ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          if FUseVBO then
            FLightmapTexCoordsVBO.Bind;
          glClientActiveTexture(GL_TEXTURE1);
          glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[4]);
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            if FUseVBO then
              FTexCoordsVBO[i].Bind;
            glClientActiveTexture(GL_TEXTURE0 + i);
            glTexCoordPointer(4, GL_FLOAT, SizeOf(TVector), tlists[i]);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        glClientActiveTexture(GL_TEXTURE0);
      end;
    end
    else
    begin
      glDisableClientState(GL_NORMAL_ARRAY);
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    end;

    if Vertices.Count > 0 then
    begin
      if FUseVBO then
        FVerticesVBO.Bind;
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, 0, lists[0]);
    end
    else
      glDisableClientState(GL_VERTEX_ARRAY);

    if GL_EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and not
      FUseVBO then
      glLockArraysEXT(0, vertices.Count);

    FLastLightMapIndex := -1;
    FArraysDeclared := True;
    FLightMapArrayEnabled := False;
    if mrci.drawState <> dsPicking then
      FLastXOpenGLTexMapping := XGL.GetBitWiseMapping;
  end
  else
  begin
    if not mrci.ignoreMaterials and not (mrci.drawState = dsPicking) then
      if TexCoords.Count > 0 then
      begin
        currentMapping := XGL.GetBitWiseMapping;
        if FLastXOpenGLTexMapping <> currentMapping then
        begin
          XGL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
          XGL.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector),
            TexCoords.List);
          FLastXOpenGLTexMapping := currentMapping;
        end;
      end;
  end;
end;

// DisableOpenGLArrays
//

procedure TVKMeshObject.DisableOpenGLArrays(var mrci: TVKRenderContextInfo);
var
  i: Integer;
begin
  if FArraysDeclared then
  begin
    DisableLightMapArray(mrci);
    if GL_EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and not
      FUseVBO then
        glUnlockArraysEXT;
    if Vertices.Count > 0 then
      glDisableClientState(GL_VERTEX_ARRAY);
    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
        glDisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
        glDisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      if GL_ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          glClientActiveTexture(GL_TEXTURE1);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            glClientActiveTexture(GL_TEXTURE0 + i);
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        glClientActiveTexture(GL_TEXTURE0);
      end;
    end;

    if FUseVBO then
    begin
      if Vertices.Count > 0 then
        FVerticesVBO.UnBind;
      if Normals.Count > 0 then
        FNormalsVBO.UnBind;
      if Colors.Count > 0 then
        FColorsVBO.UnBind;
      if TexCoords.Count > 0 then
        FTexCoordsVBO[0].UnBind;
      if LightMapTexCoords.Count > 0 then
        FLightmapTexCoordsVBO.UnBind;
      if FTexCoordsEx.Count > 0 then
      begin
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
            FTexCoordsVBO[i].UnBind;
        end;
      end;

    end;
    FArraysDeclared := False;
  end;
end;

// EnableLightMapArray
//

procedure TVKMeshObject.EnableLightMapArray(var mrci: TVKRenderContextInfo);
begin
  if GL_ARB_multitexture and (not mrci.ignoreMaterials) then
  begin
    Assert(FArraysDeclared);
    if not FLightMapArrayEnabled then
    begin
      mrci.GLStates.ActiveTexture := 1;
      mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
      mrci.GLStates.ActiveTexture := 0;
      FLightMapArrayEnabled := True;
    end;
  end;
end;

// DisableLightMapArray
//

procedure TVKMeshObject.DisableLightMapArray(var mrci: TVKRenderContextInfo);
begin
  if GL_ARB_multitexture and FLightMapArrayEnabled then
  begin
    mrci.GLStates.ActiveTexture := 1;
    mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
    mrci.GLStates.ActiveTexture := 0;
    FLightMapArrayEnabled := False;
  end;
end;

// PrepareMaterials
//

procedure TVKMeshObject.PrepareBuildList(var mrci: TVKRenderContextInfo);
var
  i: Integer;
begin
  if (Mode = momFaceGroups) and Assigned(mrci.materialLibrary) then
  begin
    for i := 0 to FaceGroups.Count - 1 do
      with TVKFaceGroup(FaceGroups.List^[i]) do
      begin
        if MaterialCache <> nil then
          MaterialCache.PrepareBuildList;
      end;
  end;
end;

// BuildList
//

procedure TVKMeshObject.BufferArrays;
const
  BufferUsage = GL_DYNAMIC_DRAW;
var
  I: integer;
begin
  if Vertices.Count > 0 then
  begin
    if not Assigned(FVerticesVBO) then
      FVerticesVBO := TVKVBOArrayBufferHandle.Create;
    FVerticesVBO.AllocateHandle;

    if FVerticesVBO.IsDataNeedUpdate then
    begin
      FVerticesVBO.BindBufferData(
      Vertices.List,
      sizeof(TAffineVector) * Vertices.Count,
      BufferUsage);
      FVerticesVBO.NotifyDataUpdated;
      FVerticesVBO.UnBind;
    end;

    Include(FValidBuffers, vbVertices);
  end;

  if Normals.Count > 0 then
  begin
    if not Assigned(FNormalsVBO) then
      FNormalsVBO := TVKVBOArrayBufferHandle.Create;
    FNormalsVBO.AllocateHandle;

    if FNormalsVBO.IsDataNeedUpdate then
    begin
      FNormalsVBO.BindBufferData(
        Normals.List,
        sizeof(TAffineVector) * Normals.Count,
        BufferUsage);
      FNormalsVBO.NotifyDataUpdated;
      FNormalsVBO.UnBind;
    end;

    Include(FValidBuffers, vbNormals);
  end;

  if Colors.Count > 0 then
  begin
    if not Assigned(FColorsVBO) then
      FColorsVBO := TVKVBOArrayBufferHandle.Create;
    FColorsVBO.AllocateHandle;

    if FColorsVBO.IsDataNeedUpdate then
    begin
      FColorsVBO.BindBufferData(
        Colors.List,
        sizeof(TVector) * Colors.Count,
        BufferUsage);
      FColorsVBO.NotifyDataUpdated;
      FColorsVBO.UnBind;
    end;

    Include(FValidBuffers, vbColors);
  end;

  if TexCoords.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < 1 then
      SetLength(FTexCoordsVBO, 1);

    if not Assigned(FTexCoordsVBO[0]) then
      FTexCoordsVBO[0] := TVKVBOArrayBufferHandle.Create;
    FTexCoordsVBO[0].AllocateHandle;

    if FTexCoordsVBO[0].IsDataNeedUpdate then
    begin
      FTexCoordsVBO[0].BindBufferData(
        TexCoords.List,
        sizeof(TAffineVector) * TexCoords.Count,
        BufferUsage);
      FTexCoordsVBO[0].NotifyDataUpdated;
      FTexCoordsVBO[0].UnBind;
    end;

    Include(FValidBuffers, vbTexCoords);
  end;

  if LightMapTexCoords.Count > 0 then
  begin
    if not Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO := TVKVBOArrayBufferHandle.Create;
    FLightmapTexCoordsVBO.AllocateHandle;

    FLightmapTexCoordsVBO.BindBufferData(
      LightMapTexCoords.List,
      sizeof(TAffineVector) * LightMapTexCoords.Count,
      BufferUsage);
    FLightmapTexCoordsVBO.NotifyDataUpdated;
    FLightmapTexCoordsVBO.UnBind;

    Include(FValidBuffers, vbLightMapTexCoords);
  end;

  if FTexCoordsEx.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < FTexCoordsEx.Count then
      SetLength(FTexCoordsVBO, FTexCoordsEx.Count);

    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      if TexCoordsEx[i].Count <= 0 then
        continue;

      if not Assigned(FTexCoordsVBO[i]) then
        FTexCoordsVBO[i] := TVKVBOArrayBufferHandle.Create;
      FTexCoordsVBO[i].AllocateHandle;

      if FTexCoordsVBO[i].IsDataNeedUpdate then
      begin
        FTexCoordsVBO[i].BindBufferData(
          TexCoordsEx[i].List, sizeof(TVector) * TexCoordsEx[i].Count,
          BufferUsage);
        FTexCoordsVBO[i].NotifyDataUpdated;
        FTexCoordsVBO[i].UnBind;
      end;
    end;

    Include(FValidBuffers, vbTexCoordsEx);
  end;

  CheckOpenGLError;
end;

procedure TVKMeshObject.BuildList(var mrci: TVKRenderContextInfo);
var
  i, j, groupID, nbGroups: Integer;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  libMat: TVKLibMaterial;
  fg: TVKFaceGroup;
begin
  // Make sure no VBO is bound and states enabled
  FArraysDeclared := False;
  FLastXOpenGLTexMapping := 0;
  gotColor := (Vertices.Count = Colors.Count);
  if gotColor then
  begin
    mrci.GLStates.Enable(stColorMaterial);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    mrci.GLStates.SetGLMaterialColors(cmFront, clrBlack, clrGray20, clrGray80, clrBlack, 0);
    mrci.GLStates.SetGLMaterialColors(cmBack, clrBlack, clrGray20, clrGray80, clrBlack, 0);
  end;
  case Mode of
    momTriangles, momTriangleStrip: if Vertices.Count > 0 then
      begin
        DeclareArraysToOpenGL(mrci);
        gotNormals := (Vertices.Count = Normals.Count);
        gotTexCoords := (Vertices.Count = TexCoords.Count);
        SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
        for i := 0 to FTexCoordsEx.Count - 1 do
          gotTexCoordsEx[i] := (TexCoordsEx[i].Count > 0) and
            GL_ARB_multitexture;
        if Mode = momTriangles then
          glBegin(GL_TRIANGLES)
        else
          glBegin(GL_TRIANGLE_STRIP);
        for i := 0 to Vertices.Count - 1 do
        begin
          if gotNormals then
            glNormal3fv(@Normals.List[i]);
          if gotColor then
            glColor4fv(@Colors.List[i]);
          if FTexCoordsEx.Count > 0 then
          begin
            if gotTexCoordsEx[0] then
              glMultiTexCoord4fv(GL_TEXTURE0, @TexCoordsEx[0].List[i])
            else if gotTexCoords then
              glTexCoord2fv(@TexCoords.List[i]);
            for j := 1 to FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[j] then
                glMultiTexCoord4fv(GL_TEXTURE0 + j,
                  @TexCoordsEx[j].List[i]);
          end
          else
          begin
            if gotTexCoords then
              glTexCoord2fv(@TexCoords.List[i]);
          end;
          glVertex3fv(@Vertices.List[i]);
        end;
        glEnd;
      end;
    momFaceGroups:
      begin
        if Assigned(mrci.materialLibrary) then
        begin
          if moroGroupByMaterial in RenderingOptions then
          begin
            // group-by-material rendering, reduces material switches,
            // but alters rendering order
            groupID := vNextRenderGroupID;
            Inc(vNextRenderGroupID);
            for i := 0 to FaceGroups.Count - 1 do
            begin
              if FaceGroups[i].FRenderGroupID <> groupID then
              begin
                libMat := FaceGroups[i].FMaterialCache;
                if Assigned(libMat) then
                  libMat.Apply(mrci);
                repeat
                  for j := i to FaceGroups.Count - 1 do
                    with FaceGroups[j] do
                    begin
                      if (FRenderGroupID <> groupID)
                        and (FMaterialCache = libMat) then
                      begin
                        FRenderGroupID := groupID;
                        BuildList(mrci);
                      end;
                    end;
                until (not Assigned(libMat)) or (not libMat.UnApply(mrci));
              end;
            end;
          end
          else
          begin
            // canonical rendering (regroups only contiguous facegroups)
            i := 0;
            nbGroups := FaceGroups.Count;
            while i < nbGroups do
            begin
              libMat := FaceGroups[i].FMaterialCache;
              if Assigned(libMat) then
              begin
                libMat.Apply(mrci);
                repeat
                  j := i;
                  while j < nbGroups do
                  begin
                    fg := FaceGroups[j];
                    if fg.MaterialCache <> libMat then
                      Break;
                    fg.BuildList(mrci);
                    Inc(j);
                  end;
                until not libMat.UnApply(mrci);
                i := j;
              end
              else
              begin
                FaceGroups[i].BuildList(mrci);
                Inc(i);
              end;
            end;
          end;
          // restore faceculling
          if (stCullFace in mrci.GLStates.States) then
          begin
            if not mrci.bufferFaceCull then
              mrci.GLStates.Disable(stCullFace);
          end
          else
          begin
            if mrci.bufferFaceCull then
              mrci.GLStates.Enable(stCullFace);
          end;
        end
        else
          for i := 0 to FaceGroups.Count - 1 do
            FaceGroups[i].BuildList(mrci);
      end;
  else
    Assert(False);
  end;
  DisableOpenGLArrays(mrci);
end;

// ------------------
// ------------------ TVKMeshObjectList ------------------
// ------------------

// CreateOwned
//

constructor TVKMeshObjectList.CreateOwned(aOwner: TVKBaseMesh);
begin
  FOwner := AOwner;
  Create;
end;

// Destroy
//

destructor TVKMeshObjectList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TVKMeshObjectList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
  mesh: TVKMeshObject;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    mesh := Items[i];
    mesh.FOwner := Self;
    if mesh is TVKSkeletonMeshObject then
      TVKSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
  end;
end;

// PrepareMaterialLibraryCache
//

procedure TVKMeshObjectList.PrepareMaterialLibraryCache(matLib:
  TVKMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVKMeshObject(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TVKMeshObjectList.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVKMeshObject(List^[i]).DropMaterialLibraryCache;
end;

// PrepareBuildList
//

procedure TVKMeshObjectList.PrepareBuildList(var mrci: TVKRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        PrepareBuildList(mrci);
end;

// BuildList
//

procedure TVKMeshObjectList.BuildList(var mrci: TVKRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        BuildList(mrci);
end;

// MorphTo
//

procedure TVKMeshObjectList.MorphTo(morphTargetIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TVKMorphableMeshObject then
      TVKMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

// Lerp
//

procedure TVKMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2: Integer;
  lerpFactor: Single);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TVKMorphableMeshObject then
      TVKMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2,
        lerpFactor);
end;

// MorphTargetCount
//

function TVKMeshObjectList.MorphTargetCount: Integer;
var
  i: Integer;
begin
  Result := MaxInt;
  for i := 0 to Count - 1 do
    if Items[i] is TVKMorphableMeshObject then
      with TVKMorphableMeshObject(Items[i]) do
        if Result > MorphTargets.Count then
          Result := MorphTargets.Count;
  if Result = MaxInt then
    Result := 0;
end;

// Clear
//

procedure TVKMeshObjectList.Clear;
var
  i: Integer;
begin
  DropMaterialLibraryCache;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

// GetMeshObject
//

function TVKMeshObjectList.GetMeshObject(Index: Integer): TVKMeshObject;
begin
  Result := TVKMeshObject(List^[Index]);
end;

// GetExtents
//

procedure TVKMeshObjectList.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1e30;
  cSmallValue: Single = -1e30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetMeshObject(i).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

// Translate
//

procedure TVKMeshObjectList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetMeshObject(i).Translate(delta);
end;

// ExtractTriangles
//

function TVKMeshObjectList.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
var
  i: Integer;
  obj: TVKMeshObject;
  objTris: TAffineVectorList;
  objTexCoords: TAffineVectorList;
  objNormals: TAffineVectorList;
begin
  Result := TAffineVectorList.Create;
  if Assigned(texCoords) then
    objTexCoords := TAffineVectorList.Create
  else
    objTexCoords := nil;
  if Assigned(normals) then
    objNormals := TAffineVectorList.Create
  else
    objNormals := nil;
  try
    for i := 0 to Count - 1 do
    begin
      obj := GetMeshObject(i);
      if not obj.Visible then
        continue;
      objTris := obj.ExtractTriangles(objTexCoords, objNormals);
      try
        Result.Add(objTris);
        if Assigned(texCoords) then
        begin
          texCoords.Add(objTexCoords);
          objTexCoords.Count := 0;
        end;
        if Assigned(normals) then
        begin
          normals.Add(objNormals);
          objNormals.Count := 0;
        end;
      finally
        objTris.Free;
      end;
    end;
  finally
    objTexCoords.Free;
    objNormals.Free;
  end;
end;

// TriangleCount
//

function TVKMeshObjectList.TriangleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].TriangleCount;
end;

// Prepare
//

procedure TVKMeshObjectList.Prepare;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Prepare;
end;

// FindMeshByName
//

function TVKMeshObjectList.FindMeshByName(MeshName: string): TVKMeshObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = MeshName then
    begin
      Result := Items[i];
      break;
    end;
end;

// BuildTangentSpace
//

procedure TVKMeshObjectList.BuildTangentSpace(buildBinormals,
  buildTangents: Boolean);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).BuildTangentSpace(buildBinormals, buildTangents);
end;

// GetUseVBO
//

function TVKMeshObjectList.GetUseVBO: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      Result := Result and GetMeshObject(I).FUseVBO;
end;

// SetUseVBO
//

procedure TVKMeshObjectList.SetUseVBO(const Value: Boolean);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).SetUseVBO(Value);
end;

// ------------------
// ------------------ TVKMeshMorphTarget ------------------
// ------------------

// CreateOwned
//

constructor TVKMeshMorphTarget.CreateOwned(AOwner: TVKMeshMorphTargetList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// Destroy
//

destructor TVKMeshMorphTarget.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

// WriteToFiler
//

procedure TVKMeshMorphTarget.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    //nothing
  end;
end;

// ReadFromFiler
//

procedure TVKMeshMorphTarget.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      //nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TVKMeshMorphTargetList ------------------
// ------------------

// CreateOwned
//

constructor TVKMeshMorphTargetList.CreateOwned(AOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

// Destroy
//

destructor TVKMeshMorphTargetList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TVKMeshMorphTargetList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Translate
//

procedure TVKMeshMorphTargetList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Translate(delta);
end;

// Clear
//

procedure TVKMeshMorphTargetList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

// GeTVKMeshMorphTarget
//

function TVKMeshMorphTargetList.GeTVKMeshMorphTarget(Index: Integer):
  TVKMeshMorphTarget;
begin
  Result := TVKMeshMorphTarget(List^[Index]);
end;

// ------------------
// ------------------ TVKMorphableMeshObject ------------------
// ------------------

// Create
//

constructor TVKMorphableMeshObject.Create;
begin
  inherited;
  FMorphTargets := TVKMeshMorphTargetList.CreateOwned(Self);
end;

// Destroy
//

destructor TVKMorphableMeshObject.Destroy;
begin
  FMorphTargets.Free;
  inherited;
end;

// WriteToFiler
//

procedure TVKMorphableMeshObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FMorphTargets.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//

procedure TVKMorphableMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FMorphTargets.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear;
//

procedure TVKMorphableMeshObject.Clear;
begin
  inherited;
  FMorphTargets.Clear;
end;

// Translate
//

procedure TVKMorphableMeshObject.Translate(const delta: TAffineVector);
begin
  inherited;
  MorphTargets.Translate(delta);
  ValidBuffers := ValidBuffers - [vbVertices];
end;

// MorphTo
//

procedure TVKMorphableMeshObject.MorphTo(morphTargetIndex: Integer);
begin
  if (morphTargetIndex = 0) and (MorphTargets.Count = 0) then
    Exit;
  Assert(Cardinal(morphTargetIndex) < Cardinal(MorphTargets.Count));
  with MorphTargets[morphTargetIndex] do
  begin
    if Vertices.Count > 0 then
    begin
      Self.Vertices.Assign(Vertices);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if Normals.Count > 0 then
    begin
      Self.Normals.Assign(Normals);
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

// Lerp
//

procedure TVKMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2:
  Integer;
  lerpFactor: Single);
var
  mt1, mt2: TVKMeshMorphTarget;
begin
  Assert((Cardinal(morphTargetIndex1) < Cardinal(MorphTargets.Count))
    and (Cardinal(morphTargetIndex2) < Cardinal(MorphTargets.Count)));
  if lerpFactor = 0 then
    MorphTo(morphTargetIndex1)
  else if lerpFactor = 1 then
    MorphTo(morphTargetIndex2)
  else
  begin
    mt1 := MorphTargets[morphTargetIndex1];
    mt2 := MorphTargets[morphTargetIndex2];
    if mt1.Vertices.Count > 0 then
    begin
      Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if mt1.Normals.Count > 0 then
    begin
      Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
      Normals.Normalize;
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

// ------------------
// ------------------ TVKSkeletonMeshObject ------------------
// ------------------

// Create
//

constructor TVKSkeletonMeshObject.Create;
begin
  FBoneMatrixInvertedMeshes := TList.Create;
  FBackupInvertedMeshes := TList.Create; // ragdoll
  inherited Create;
end;

// Destroy
//

destructor TVKSkeletonMeshObject.Destroy;
begin
  Clear;
  FBoneMatrixInvertedMeshes.Free;
  FBackupInvertedMeshes.Free;
  inherited Destroy;
end;

// WriteToFiler
//

procedure TVKSkeletonMeshObject.WriteToFiler(writer: TVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FVerticeBoneWeightCount);
    WriteInteger(FBonesPerVertex);
    WriteInteger(FVerticeBoneWeightCapacity);
    for i := 0 to FVerticeBoneWeightCount - 1 do
      Write(FVerticesBonesWeights[i][0], FBonesPerVertex *
        SizeOf(TVertexBoneWeight));
  end;
end;

// ReadFromFiler
//

procedure TVKSkeletonMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVerticeBoneWeightCount := ReadInteger;
      FBonesPerVertex := ReadInteger;
      FVerticeBoneWeightCapacity := ReadInteger;
      ResizeVerticesBonesWeights;
      for i := 0 to FVerticeBoneWeightCount - 1 do
        Read(FVerticesBonesWeights[i][0], FBonesPerVertex *
          SizeOf(TVertexBoneWeight));
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear
//

procedure TVKSkeletonMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FVerticeBoneWeightCount := 0;
  FBonesPerVertex := 0;
  ResizeVerticesBonesWeights;
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TVKBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
end;

// SetVerticeBoneWeightCount
//

procedure TVKSkeletonMeshObject.SetVerticeBoneWeightCount(const val: Integer);
begin
  if val <> FVerticeBoneWeightCount then
  begin
    FVerticeBoneWeightCount := val;
    if FVerticeBoneWeightCount > FVerticeBoneWeightCapacity then
      VerticeBoneWeightCapacity := FVerticeBoneWeightCount + 16;
    FLastVerticeBoneWeightCount := FVerticeBoneWeightCount;
  end;
end;

// SetVerticeBoneWeightCapacity
//

procedure TVKSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val: Integer);
begin
  if val <> FVerticeBoneWeightCapacity then
  begin
    FVerticeBoneWeightCapacity := val;
    ResizeVerticesBonesWeights;
  end;
end;

// SetBonesPerVertex
//

procedure TVKSkeletonMeshObject.SetBonesPerVertex(const val: Integer);
begin
  if val <> FBonesPerVertex then
  begin
    FBonesPerVertex := val;
    ResizeVerticesBonesWeights;
  end;
end;

// ResizeVerticesBonesWeights
//

procedure TVKSkeletonMeshObject.ResizeVerticesBonesWeights;
var
  n, m, i, j: Integer;
  newArea: PVerticesBoneWeights;
begin
  n := BonesPerVertex * VerticeBoneWeightCapacity;
  if n = 0 then
  begin
    // release everything
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
      FVerticesBonesWeights := nil;
    end;
  end
  else
  begin
    // allocate new area
    GetMem(newArea, VerticeBoneWeightCapacity * SizeOf(PVertexBoneWeightArray));
    newArea[0] := AllocMem(n * SizeOf(TVertexBoneWeight));
    for i := 1 to VerticeBoneWeightCapacity - 1 do
      newArea[i] := PVertexBoneWeightArray(
        PtrUInt(newArea[0]) +
        PtrUInt(i * SizeOf(TVertexBoneWeight) * BonesPerVertex));
    // transfer old data
    if FLastVerticeBoneWeightCount < VerticeBoneWeightCount then
      n := FLastVerticeBoneWeightCount
    else
      n := VerticeBoneWeightCount;
    if FLastBonesPerVertex < BonesPerVertex then
      m := FLastBonesPerVertex
    else
      m := BonesPerVertex;
    for i := 0 to n - 1 do
      for j := 0 to m - 1 do
        newArea[i][j] := VerticesBonesWeights[i][j];
    // release old area and switch to new
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
    end;
    FVerticesBonesWeights := newArea;
  end;
  FLastBonesPerVertex := FBonesPerVertex;
end;

// AddWeightedBone
//

procedure TVKSkeletonMeshObject.AddWeightedBone(aBoneID: Integer; aWeight:
  Single);
begin
  if BonesPerVertex < 1 then
    BonesPerVertex := 1;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[0] do
  begin
    BoneID := aBoneID;
    Weight := aWeight;
  end;
end;

// AddWeightedBones
//

procedure TVKSkeletonMeshObject.AddWeightedBones(const boneIDs:
  TVertexBoneWeightDynArray);
var
  i: Integer;
  n: Integer;
begin
  n := Length(boneIDs);
  if BonesPerVertex < n then
    BonesPerVertex := n;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  for i := 0 to n - 1 do
  begin
    with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[i] do
    begin
      BoneID := boneIDs[i].BoneID;
      Weight := boneIDs[i].Weight;
    end;
  end;
end;

// FindOrAdd
//

function TVKSkeletonMeshObject.FindOrAdd(boneID: Integer;
  const vertex, normal: TAffineVector): Integer;
var
  i: Integer;
  dynArray: TVertexBoneWeightDynArray;
begin
  if BonesPerVertex > 1 then
  begin
    SetLength(dynArray, 1);
    dynArray[0].BoneID := boneID;
    dynArray[0].Weight := 1;
    Result := FindOrAdd(dynArray, vertex, normal);
    Exit;
  end;
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
    if (VerticesBonesWeights^[i]^[0].BoneID = boneID)
      and VectorEquals(Vertices.List^[i], vertex)
      and VectorEquals(Normals.List^[i], normal) then
    begin
      Result := i;
      Break;
    end;
  if Result < 0 then
  begin
    AddWeightedBone(boneID, 1);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

// FindOrAdd
//

function TVKSkeletonMeshObject.FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
  const vertex, normal: TAffineVector): Integer;
var
  i, j: Integer;
  bonesMatch: Boolean;
begin
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
  begin
    bonesMatch := True;
    for j := 0 to High(boneIDs) do
    begin
      if (boneIDs[j].BoneID <> VerticesBonesWeights^[i]^[j].BoneID)
        or (boneIDs[j].Weight <> VerticesBonesWeights^[i]^[j].Weight) then
      begin
        bonesMatch := False;
        Break;
      end;
    end;
    if bonesMatch and VectorEquals(Vertices[i], vertex)
      and VectorEquals(Normals[i], normal) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result < 0 then
  begin
    AddWeightedBones(boneIDs);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

// PrepareBoneMatrixInvertedMeshes
//

procedure TVKSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
  i, k, boneIndex: Integer;
  invMesh: TVKBaseMeshObject;
  invMat: TMatrix;
  bone: TVKSkeletonBone;
  p: TVector;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TVKBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // calculate
  for k := 0 to BonesPerVertex - 1 do
  begin
    invMesh := TVKBaseMeshObject.Create;
    FBoneMatrixInvertedMeshes.Add(invMesh);
    invMesh.Vertices := Vertices;
    invMesh.Normals := Normals;
    for i := 0 to Vertices.Count - 1 do
    begin
      boneIndex := VerticesBonesWeights^[i]^[k].BoneID;
      bone := Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
      // transform point
      MakePoint(p, Vertices[i]);
      invMat := bone.GlobalMatrix;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Vertices[i] := PAffineVector(@p)^;
      // transform normal
      SetVector(p, Normals[i]);
      invMat := bone.GlobalMatrix;
      invMat.W := NullHmgPoint;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Normals[i] := PAffineVector(@p)^;
    end;
  end;
end;

procedure TVKSkeletonMeshObject.BackupBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TVKBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
    TVKBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  FBackupInvertedMeshes.Clear;
  // copy current stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
  begin
    bm := TVKBaseMeshObject.Create;
    bm.Assign(TVKBaseMeshObject(FBoneMatrixInvertedMeshes[i]));
    FBackupInvertedMeshes.Add(bm);
    TVKBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  end;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TVKSkeletonMeshObject.RestoreBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TVKBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TVKBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // restore the backup
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
  begin
    bm := TVKBaseMeshObject.Create;
    bm.Assign(TVKBaseMeshObject(FBackupInvertedMeshes[i]));
    FBoneMatrixInvertedMeshes.Add(bm);
    TVKBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  end;
  FBackupInvertedMeshes.Clear;
end;

// ApplyCurrentSkeletonFrame
//

procedure TVKSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize: Boolean);
var
  i, j, boneID: Integer;
  refVertices, refNormals: TAffineVectorList;
  n, nt: TVector;
  bone: TVKSkeletonBone;
  skeleton: TVKSkeleton;
  tempvert,
    tempnorm: TAffineVector;
begin
  with TVKBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do
  begin
    refVertices := Vertices;
    refNormals := Normals;
  end;
  skeleton := Owner.Owner.Skeleton;
  n.W := 0;
  if BonesPerVertex = 1 then
  begin
    // simple case, one bone per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      boneID := VerticesBonesWeights^[i]^[0].BoneID;
      bone := skeleton.BoneByID(boneID);
      Vertices.List^[i] := VectorTransform(refVertices.List^[i],
        bone.GlobalMatrix);
      PAffineVector(@n)^ := refNormals.List^[i];
      nt := VectorTransform(n, bone.GlobalMatrix);
      Normals.List^[i] := PAffineVector(@nt)^;
    end;
  end
  else
  begin
    // multiple bones per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      Vertices.List^[i] := NullVector;
      Normals.List^[i] := NullVector;
      for j := 0 to BonesPerVertex - 1 do
      begin
        with TVKBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do
        begin
          refVertices := Vertices;
          refNormals := Normals;
        end;
        tempvert := NullVector;
        tempnorm := NullVector;
        if VerticesBonesWeights^[i]^[j].Weight <> 0 then
        begin
          boneID := VerticesBonesWeights^[i]^[j].BoneID;
          bone := skeleton.BoneByID(boneID);
          CombineVector(tempvert,
            VectorTransform(refVertices.List^[i], bone.GlobalMatrix),
            VerticesBonesWeights^[i]^[j].Weight);
          PAffineVector(@n)^ := refNormals.List^[i];
          n := VectorTransform(n, bone.GlobalMatrix);
          CombineVector(tempnorm,
            PAffineVector(@n)^,
            VerticesBonesWeights^[i]^[j].Weight);
        end;
        AddVector(Vertices.List^[i], tempvert);
        AddVector(Normals.List^[i], tempnorm);
      end;
    end;
  end;
  if normalize then
    Normals.Normalize;
end;

// ------------------
// ------------------ TVKFaceGroup ------------------
// ------------------

// CreateOwned
//

constructor TVKFaceGroup.CreateOwned(AOwner: TVKFaceGroups);
begin
  FOwner := AOwner;
  FLightMapIndex := -1;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// Destroy
//

destructor TVKFaceGroup.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

// WriteToFiler
//

procedure TVKFaceGroup.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FLightMapIndex < 0 then
    begin
      WriteInteger(0); // Archive Version 0
      WriteString(FMaterialName);
    end
    else
    begin
      WriteInteger(1); // Archive Version 1, added FLightMapIndex
      WriteString(FMaterialName);
      WriteInteger(FLightMapIndex);
    end;
  end;
end;

// ReadFromFiler
//

procedure TVKFaceGroup.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0..1] then
    with reader do
    begin
      FMaterialName := ReadString;
      if archiveVersion >= 1 then
        FLightMapIndex := ReadInteger
      else
        FLightMapIndex := -1;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// AttachLightmap
//

procedure TVKFaceGroup.AttachLightmap(lightMap: TVKTexture; var mrci:
  TVKRenderContextInfo);
begin
  if GL_ARB_multitexture then
    with lightMap do
    begin
      Assert(Image.NativeTextureTarget = ttTexture2D);
      mrci.GLStates.TextureBinding[1, ttTexture2D] := Handle;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      mrci.GLStates.ActiveTexture := 0;
    end;
end;

// AttachOrDetachLightmap
//

procedure TVKFaceGroup.AttachOrDetachLightmap(var mrci: TVKRenderContextInfo);
var
  libMat: TVKLibMaterial;
begin
  if GL_ARB_multitexture then
  begin
    if (not mrci.ignoreMaterials) and Assigned(mrci.lightmapLibrary) then
    begin
      if Owner.Owner.FLastLightMapIndex <> lightMapIndex then
      begin
        Owner.Owner.FLastLightMapIndex := lightMapIndex;
        if lightMapIndex >= 0 then
        begin
          // attach and activate lightmap
          Assert(lightMapIndex <
            TVKMaterialLibrary(mrci.lightmapLibrary).Materials.Count);
          libMat :=
            TVKMaterialLibrary(mrci.lightmapLibrary).Materials[lightMapIndex];
          AttachLightmap(libMat.Material.Texture, mrci);
          Owner.Owner.EnableLightMapArray(mrci);
        end
        else
        begin
          // desactivate lightmap
          Owner.Owner.DisableLightMapArray(mrci);
        end;
      end;
    end;
  end;
end;

// PrepareMaterialLibraryCache
//

procedure TVKFaceGroup.PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
begin
  if (FMaterialName <> '') and (matLib <> nil) then
    FMaterialCache := matLib.Materials.GetLibMaterialByName(FMaterialName)
  else
    FMaterialCache := nil;
end;

// DropMaterialLibraryCache
//

procedure TVKFaceGroup.DropMaterialLibraryCache;
begin
  FMaterialCache := nil;
end;

// AddToTriangles
//

procedure TVKFaceGroup.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil;
  aNormals: TAffineVectorList = nil);
begin
  // nothing
end;

// Reverse
//

procedure TVKFaceGroup.Reverse;
begin
  // nothing
end;

// Prepare
//

procedure TVKFaceGroup.Prepare;
begin
  // nothing
end;

// ------------------
// ------------------ TFGVertexIndexList ------------------
// ------------------

// Create
//

constructor TFGVertexIndexList.Create;
begin
  inherited;
  FVertexIndices := TIntegerList.Create;
  FMode := fgmmTriangles;
end;

// Destroy
//

destructor TFGVertexIndexList.Destroy;
begin
  FVertexIndices.Free;
  FIndexVBO.Free;
  inherited;
end;

// WriteToFiler
//

procedure TFGVertexIndexList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FVertexIndices.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
  end;
end;

// ReadFromFiler
//

procedure TFGVertexIndexList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVertexIndices.ReadFromFiler(reader);
      FMode := TVKFaceGroupMeshMode(ReadInteger);
      InvalidateVBO;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// SetIndices
//

procedure TFGVertexIndexList.SetupVBO;
const
  BufferUsage = GL_STATIC_DRAW;
begin
  if not Assigned(FIndexVBO) then
    FIndexVBO := TVKVBOElementArrayHandle.Create;

  FIndexVBO.AllocateHandle;

  if FIndexVBO.IsDataNeedUpdate then
  begin
    FIndexVBO.BindBufferData(
      VertexIndices.List,
      SizeOf(Integer) * VertexIndices.Count,
      BufferUsage);
    FIndexVBO.NotifyDataUpdated;
  end;
end;

procedure TFGVertexIndexList.SetVertexIndices(const val: TIntegerList);
begin
  FVertexIndices.Assign(val);
  InvalidateVBO;
end;

// BuildList
//

procedure TFGVertexIndexList.BuildList(var mrci: TVKRenderContextInfo);
const
  cFaceGroupMeshModeToOpenGL: array[TVKFaceGroupMeshMode] of Integer =
    (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);
begin
  if VertexIndices.Count = 0 then
    Exit;
  Owner.Owner.DeclareArraysToOpenGL(mrci, False);
  AttachOrDetachLightmap(mrci);

  if Owner.Owner.UseVBO then
  begin
    SetupVBO;

    FIndexVBO.Bind;
    glDrawElements(cFaceGroupMeshModeToOpenGL[Mode], VertexIndices.Count,
      GL_UNSIGNED_INT, nil);
    FIndexVBO.UnBind;
  end
  else
  begin
    glDrawElements(cFaceGroupMeshModeToOpenGL[Mode], VertexIndices.Count,
      GL_UNSIGNED_INT, VertexIndices.List);
  end;
end;

// AddToList
//

procedure TFGVertexIndexList.AddToList(source, destination: TAffineVectorList;
  indices: TIntegerList);
var
  i, n: Integer;
begin
  if not Assigned(destination) then
    Exit;
  if indices.Count < 3 then
    Exit;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        n := (indices.Count div 3) * 3;
        if source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 0 to n - 1 do
            destination.Add(source[indices.List^[i]]);
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmTriangleStrip:
      begin
        if source.Count > 0 then
          ConvertStripToList(source, indices, destination)
        else
          destination.AddNulls(destination.Count + (indices.Count - 2) * 3);
      end;
    fgmmTriangleFan:
      begin
        n := (indices.Count - 2) * 3;
        if source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 2 to VertexIndices.Count - 1 do
          begin
            destination.Add(source[indices.List^[0]],
              source[indices.List^[i - 1]],
              source[indices.List^[i]]);
          end;
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmQuads:
      begin
        n := indices.Count div 4;
        if source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n * 6);
          i := 0;
          while n > 0 do
          begin
            destination.Add(source[indices.List^[i]],
              source[indices.List^[i + 1]],
              source[indices.List^[i + 2]]);
            destination.Add(source[indices.List^[i]],
              source[indices.List^[i + 2]],
              source[indices.List^[i + 3]]);
            Inc(i, 4);
            Dec(n);
          end;
        end
        else
          destination.AddNulls(destination.Count + n * 6);
      end;
  else
    Assert(False);
  end;
end;

// AddToTriangles
//

procedure TFGVertexIndexList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil;
  aNormals: TAffineVectorList = nil);
var
  mo: TVKMeshObject;
begin
  mo := Owner.Owner;
  AddToList(mo.Vertices, aList, VertexIndices);
  AddToList(mo.TexCoords, aTexCoords, VertexIndices);
  AddToList(mo.Normals, aNormals, VertexIndices);
  InvalidateVBO;
end;

// TriangleCount
//

function TFGVertexIndexList.TriangleCount: Integer;
begin
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      Result := VertexIndices.Count div 3;
    fgmmTriangleFan, fgmmTriangleStrip:
      begin
        Result := VertexIndices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    fgmmQuads:
      result := VertexIndices.Count div 2;
  else
    Result := 0;
    Assert(False);
  end;
end;

// Reverse
//

procedure TFGVertexIndexList.Reverse;
begin
  VertexIndices.Reverse;
  InvalidateVBO;
end;

// Add
//

procedure TFGVertexIndexList.Add(idx: Integer);
begin
  FVertexIndices.Add(idx);
  InvalidateVBO;
end;

// GetExtents
//

procedure TFGVertexIndexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
  ref: PFloatArray;
const
  cBigValue: Single = 1e50;
  cSmallValue: Single = -1e50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to VertexIndices.Count - 1 do
  begin
    ref := Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
    for k := 0 to 2 do
    begin
      f := ref^[k];
      if f < min.V[k] then
        min.V[k] := f;
      if f > max.V[k] then
        max.V[k] := f;
    end;
  end;
end;

// ConvertToList
//

procedure TFGVertexIndexList.ConvertToList;
var
  i: Integer;
  bufList: TIntegerList;
begin
  if VertexIndices.Count >= 3 then
  begin
    case Mode of
      fgmmTriangleStrip:
        begin
          bufList := TIntegerList.Create;
          try
            ConvertStripToList(VertexIndices, bufList);
            VertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
      fgmmTriangleFan:
        begin
          bufList := TIntegerList.Create;
          try
            for i := 0 to VertexIndices.Count - 3 do
              bufList.Add(VertexIndices[0], VertexIndices[i], VertexIndices[i +
                1]);
            VertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
    end;
    InvalidateVBO;
  end;
end;

// GetNormal
//

function TFGVertexIndexList.GetNormal: TAffineVector;
begin
  if VertexIndices.Count < 3 then
    Result := NullVector
  else
    with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
        Items[VertexIndices[2]], Result);
end;

procedure TFGVertexIndexList.InvalidateVBO;
begin
  if Assigned(FIndexVBO) then
    FIndexVBO.NotifyChangesOfData;
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

// Create
//

constructor TFGVertexNormalTexIndexList.Create;
begin
  inherited;
  FNormalIndices := TIntegerList.Create;
  FTexCoordIndices := TIntegerList.Create;
end;

// Destroy
//

destructor TFGVertexNormalTexIndexList.Destroy;
begin
  FTexCoordIndices.Free;
  FNormalIndices.Free;
  inherited;
end;

// WriteToFiler
//

procedure TFGVertexNormalTexIndexList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FNormalIndices.WriteToFiler(writer);
    FTexCoordIndices.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//

procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

// SetNormalIndices
//

procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val: TIntegerList);
begin
  FNormalIndices.Assign(val);
end;

// SetTexCoordIndices
//

procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val:
  TIntegerList);
begin
  FTexCoordIndices.Assign(val);
end;

// BuildList
//

procedure TFGVertexNormalTexIndexList.BuildList(var mrci: TVKRenderContextInfo);
var
  i: Integer;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  texCoordPool: PAffineVectorArray;
  colorPool: PVectorArray;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
begin
  Assert(((TexCoordIndices.Count = 0) or (VertexIndices.Count <=
    TexCoordIndices.Count))
    and ((NormalIndices.Count = 0) or (VertexIndices.Count <=
      NormalIndices.Count)));
  vertexPool := Owner.Owner.Vertices.List;
  normalPool := Owner.Owner.Normals.List;
  colorPool := Owner.Owner.Colors.List;
  texCoordPool := Owner.Owner.TexCoords.List;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles: glBegin(GL_TRIANGLES);
    fgmmTriangleStrip: glBegin(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: glBegin(GL_TRIANGLE_FAN);
  else
    Assert(False);
  end;
  vertexIdxList := VertexIndices.List;
  if NormalIndices.Count > 0 then
    normalIdxList := NormalIndices.List
  else
    normalIdxList := vertexIdxList;
  if TexCoordIndices.Count > 0 then
    texCoordIdxList := TexCoordIndices.List
  else
    texCoordIdxList := vertexIdxList;

  for i := 0 to VertexIndices.Count - 1 do
  begin
    glNormal3fv(@normalPool[normalIdxList^[i]]);
    if Assigned(colorPool) then
      glColor4fv(@colorPool[vertexIdxList^[i]]);
    if Assigned(texCoordPool) then
      glTexCoord2fv(@texCoordPool[texCoordIdxList^[i]]);
    glVertex3fv(@vertexPool[vertexIdxList^[i]]);
  end;

  glEnd;
end;

// AddToTriangles
//

procedure TFGVertexNormalTexIndexList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil;
  aNormals: TAffineVectorList = nil);
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.TexCoords, aTexCoords, TexCoordIndices);
  AddToList(Owner.Owner.Normals, aNormals, NormalIndices);
end;

// Add
//

procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx:
  Integer);
begin
  inherited Add(vertexIdx);
  FNormalIndices.Add(normalIdx);
  FTexCoordIndices.Add(texCoordIdx);
end;

// ------------------
// ------------------ TFGIndexTexCoordList ------------------
// ------------------

// Create
//

constructor TFGIndexTexCoordList.Create;
begin
  inherited;
  FTexCoords := TAffineVectorList.Create;
end;

// Destroy
//

destructor TFGIndexTexCoordList.Destroy;
begin
  FTexCoords.Free;
  inherited;
end;

// WriteToFiler
//

procedure TFGIndexTexCoordList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FTexCoords.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//

procedure TFGIndexTexCoordList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

// SetTexCoords
//

procedure TFGIndexTexCoordList.SetTexCoords(const val: TAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

// BuildList
//

procedure TFGIndexTexCoordList.BuildList(var mrci: TVKRenderContextInfo);
var
  i, k: integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: boolean;

begin
  Assert(VertexIndices.Count = TexCoords.Count);
  texCoordPool := TexCoords.List;
  vertexPool := Owner.Owner.Vertices.List;
  indicesPool := @VertexIndices.List[0];
  colorPool := @Owner.Owner.Colors.List[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case Mode of
    fgmmTriangles: glBegin(GL_TRIANGLES);
    fgmmFlatTriangles: glBegin(GL_TRIANGLES);
    fgmmTriangleStrip: glBegin(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: glBegin(GL_TRIANGLE_FAN);
    fgmmQuads: glBegin(GL_QUADS);
  else
    Assert(False);
  end;
  if Owner.Owner.Normals.Count = Owner.Owner.Vertices.Count then
  begin
    normalPool := Owner.Owner.Normals.List;
    for i := 0 to VertexIndices.Count - 1 do
    begin
      glTexCoord2fv(@texCoordPool[i]);
      k := indicesPool[i];
      if gotColor then
        glColor4fv(@colorPool[k]);
      glNormal3fv(@normalPool[k]);
      glVertex3fv(@vertexPool[k]);
    end;
  end
  else
  begin
    for i := 0 to VertexIndices.Count - 1 do
    begin
      glTexCoord2fv(@texCoordPool[i]);
      if gotColor then
        glColor4fv(@colorPool[indicesPool[i]]);
      glVertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  glEnd;
  CheckOpenGLError;
end;

// AddToTriangles
//

procedure TFGIndexTexCoordList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil;
  aNormals: TAffineVectorList = nil);
var
  i, n: Integer;
  texCoordList: TAffineVectorList;
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.Normals, aNormals, VertexIndices);
  texCoordList := Self.TexCoords;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        if Assigned(aTexCoords) then
        begin
          n := (VertexIndices.Count div 3) * 3;
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + n);
          for i := 0 to n - 1 do
            aTexCoords.Add(texCoordList[i]);
        end;
      end;
    fgmmTriangleStrip:
      begin
        if Assigned(aTexCoords) then
          ConvertStripToList(aTexCoords, texCoordList);
      end;
    fgmmTriangleFan:
      begin
        if Assigned(aTexCoords) then
        begin
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count +
            (VertexIndices.Count - 2) * 3);
          for i := 2 to VertexIndices.Count - 1 do
          begin
            aTexCoords.Add(texCoordList[0],
              texCoordList[i - 1],
              texCoordList[i]);
          end;
        end;
      end;
  else
    Assert(False);
  end;
end;

// Add (affine)
//

procedure TFGIndexTexCoordList.Add(idx: Integer; const texCoord: TAffineVector);
begin
  TexCoords.Add(texCoord);
  inherited Add(idx);
end;

// Add (s, t)
//

procedure TFGIndexTexCoordList.Add(idx: Integer; const s, t: Single);
begin
  TexCoords.Add(s, t, 0);
  inherited Add(idx);
end;

// ------------------
// ------------------ TVKFaceGroups ------------------
// ------------------

// CreateOwned
//

constructor TVKFaceGroups.CreateOwned(AOwner: TVKMeshObject);
begin
  FOwner := AOwner;
  Create;
end;

// Destroy
//

destructor TVKFaceGroups.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TVKFaceGroups.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Clear
//

procedure TVKFaceGroups.Clear;
var
  i: Integer;
  fg: TVKFaceGroup;
begin
  for i := 0 to Count - 1 do
  begin
    fg := GetFaceGroup(i);
    if Assigned(fg) then
    begin
      fg.FOwner := nil;
      fg.Free;
    end;
  end;
  inherited;
end;

// GetFaceGroup
//

function TVKFaceGroups.GetFaceGroup(Index: Integer): TVKFaceGroup;
begin
  Result := TVKFaceGroup(List^[Index]);
end;

// PrepareMaterialLibraryCache
//

procedure TVKFaceGroups.PrepareMaterialLibraryCache(matLib: TVKMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVKFaceGroup(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TVKFaceGroups.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVKFaceGroup(List^[i]).DropMaterialLibraryCache;
end;

// AddToTriangles
//

procedure TVKFaceGroups.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil;
  aNormals: TAffineVectorList = nil);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

// MaterialLibrary
//

function TVKFaceGroups.MaterialLibrary: TVKMaterialLibrary;
var
  mol: TVKMeshObjectList;
  bm: TVKBaseMesh;
begin
  if Assigned(Owner) then
  begin
    mol := Owner.Owner;
    if Assigned(mol) then
    begin
      bm := mol.Owner;
      if Assigned(bm) then
      begin
        Result := bm.MaterialLibrary;
        ;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

// CompareMaterials
//

function CompareMaterials(item1, item2: TObject): Integer;

  function MaterialIsOpaque(fg: TVKFaceGroup): Boolean;
  var
    libMat: TVKLibMaterial;
  begin
    libMat := fg.MaterialCache;
    Result := (not Assigned(libMat)) or (not libMat.Material.Blended);
  end;

var
  fg1, fg2: TVKFaceGroup;
  opaque1, opaque2: Boolean;
begin
  fg1 := TVKFaceGroup(item1);
  opaque1 := MaterialIsOpaque(fg1);
  fg2 := TVKFaceGroup(item2);
  opaque2 := MaterialIsOpaque(fg2);
  if opaque1 = opaque2 then
  begin
    Result := CompareStr(fg1.MaterialName, fg2.MaterialName);
    if Result = 0 then
      result := fg1.LightMapIndex - fg2.LightMapIndex;
  end
  else if opaque1 then
    Result := -1
  else
    Result := 1;
end;

// SortByMaterial
//

procedure TVKFaceGroups.SortByMaterial;
begin
  PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
  Sort(@CompareMaterials);
end;

// ------------------
// ------------------ TVKVectorFile ------------------
// ------------------

// Create
//

constructor TVKVectorFile.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TVKBaseMesh);
  inherited;
end;

// Owner
//

function TVKVectorFile.Owner: TVKBaseMesh;
begin
  Result := TVKBaseMesh(GetOwner);
end;

// SetNormalsOrientation
//

procedure TVKVectorFile.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  FNormalsOrientation := val;
end;

// ------------------
// ------------------ TVKGLSMVectorFile ------------------
// ------------------

// Capabilities
//

class function TVKGLSMVectorFile.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

// LoadFromStream
//

procedure TVKGLSMVectorFile.LoadFromStream(aStream: TStream);
begin
  Owner.MeshObjects.LoadFromStream(aStream);
end;

// SaveToStream
//

procedure TVKGLSMVectorFile.SaveToStream(aStream: TStream);
begin
  Owner.MeshObjects.SaveToStream(aStream);
end;

// ------------------
// ------------------ TVKBaseMesh ------------------
// ------------------

// Create
//

constructor TVKBaseMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if FMeshObjects = nil then
    FMeshObjects := TVKMeshObjectList.CreateOwned(Self);
  if FSkeleton = nil then
    FSkeleton := TVKSkeleton.CreateOwned(Self);
  FUseMeshMaterials := True;
  FAutoCentering := [];
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  FAutoScaling := TVKCoordinates.CreateInitialized(Self, XYZWHmgVector,
    csPoint);
end;

// Destroy
//

destructor TVKBaseMesh.Destroy;
begin
  FConnectivity.Free;
  DropMaterialLibraryCache;
  FSkeleton.Free;
  FMeshObjects.Free;
  FAutoScaling.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKBaseMesh.Assign(Source: TPersistent);
begin
  if Source is TVKBaseMesh then
  begin
    FSkeleton.Clear;
    FNormalsOrientation := TVKBaseMesh(Source).FNormalsOrientation;
    FMaterialLibrary := TVKBaseMesh(Source).FMaterialLibrary;
    FLightmapLibrary := TVKBaseMesh(Source).FLightmapLibrary;
    FAxisAlignedDimensionsCache :=  TVKBaseMesh(Source).FAxisAlignedDimensionsCache;
    FBaryCenterOffset := TVKBaseMesh(Source).FBaryCenterOffset;
    FUseMeshMaterials := TVKBaseMesh(Source).FUseMeshMaterials;
    FOverlaySkeleton := TVKBaseMesh(Source).FOverlaySkeleton;
    FIgnoreMissingTextures := TVKBaseMesh(Source).FIgnoreMissingTextures;
    FAutoCentering := TVKBaseMesh(Source).FAutoCentering;
    FAutoScaling.Assign(TVKBaseMesh(Source).FAutoScaling);
    FSkeleton.Assign(TVKBaseMesh(Source).FSkeleton);
    FSkeleton.RootBones.PrepareGlobalMatrices;
    FMeshObjects.Assign(TVKBaseMesh(Source).FMeshObjects);
  end;
  inherited Assign(Source);
end;

// LoadFromFile
//

procedure TVKBaseMesh.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
    try
      LoadFromStream(fileName, fs);
      FLastLoadedFilename := filename;
    finally
      fs.Free;
    end;
  end;
end;

// LoadFromStream
//

procedure TVKBaseMesh.LoadFromStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TVKVectorFile;
  vectorFileClass: TVKVectorFileClass;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    MeshObjects.Clear;
    Skeleton.Clear;
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      if Assigned(Scene) then
        Scene.BeginUpdate;
      try
        newVectorFile.LoadFromStream(aStream);
        FLastLoadedFilename := filename;
      finally
        if Assigned(Scene) then
          Scene.EndUpdate;
      end;
    finally
      newVectorFile.Free;
    end;
    PerformAutoScaling;
    PerformAutoCentering;
    PrepareMesh;
  end;
end;

// SaveToFile
//

procedure TVKBaseMesh.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmCreate);
    try
      SaveToStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

// SaveToStream
//

procedure TVKBaseMesh.SaveToStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TVKVectorFile;
  vectorFileClass: TVKVectorFileClass;
begin
  if fileName <> '' then
  begin
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      newVectorFile.SaveToStream(aStream);
    finally
      newVectorFile.Free;
    end;
  end;
end;

// AddDataFromFile
//

procedure TVKBaseMesh.AddDataFromFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
    try
      AddDataFromStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

// AddDataFromStream
//

procedure TVKBaseMesh.AddDataFromStream(const filename: string; aStream:
  TStream);
var
  newVectorFile: TVKVectorFile;
  vectorFileClass: TVKVectorFileClass;
begin
  if fileName <> '' then
  begin
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    newVectorFile.ResourceName := filename;
    PrepareVectorFile(newVectorFile);
    try
      if Assigned(Scene) then
        Scene.BeginUpdate;
      newVectorFile.LoadFromStream(aStream);
      if Assigned(Scene) then
        Scene.EndUpdate;
    finally
      NewVectorFile.Free;
    end;
    PrepareMesh;
  end;
end;

// GetExtents
//

procedure TVKBaseMesh.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1e50;
  cSmallValue: Single = -1e50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to MeshObjects.Count - 1 do
  begin
    TVKMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

// GetBarycenter
//

function TVKBaseMesh.GetBarycenter: TAffineVector;
var
  i, nb: Integer;
begin
  Result := NullVector;
  nb := 0;
  for i := 0 to MeshObjects.Count - 1 do
    TVKMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
  if nb > 0 then
    ScaleVector(Result, 1 / nb);
end;

// LastLoadedFilename
//

function TVKBaseMesh.LastLoadedFilename: string;
begin
  result := FLastLoadedFilename;
end;

// SetMaterialLibrary
//

procedure TVKBaseMesh.SetMaterialLibrary(const val: TVKMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
    if Assigned(FMaterialLibrary) then
    begin
      DestroyHandle;
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

// SetMaterialLibrary
//

procedure TVKBaseMesh.SetLightmapLibrary(const val: TVKMaterialLibrary);
begin
  if FLightmapLibrary <> val then
  begin
    if Assigned(FLightmapLibrary) then
    begin
      DestroyHandle;
      FLightmapLibrary.RemoveFreeNotification(Self);
    end;
    FLightmapLibrary := val;
    if Assigned(FLightmapLibrary) then
      FLightmapLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

// SetNormalsOrientation
//

procedure TVKBaseMesh.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  if val <> FNormalsOrientation then
  begin
    FNormalsOrientation := val;
    StructureChanged;
  end;
end;

// SetOverlaySkeleton
//

procedure TVKBaseMesh.SetOverlaySkeleton(const val: Boolean);
begin
  if FOverlaySkeleton <> val then
  begin
    FOverlaySkeleton := val;
    NotifyChange(Self);
  end;
end;

// SetAutoScaling
//

procedure TVKBaseMesh.SetAutoScaling(const Value: TVKCoordinates);
begin
  FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

// Notification
//

procedure TVKBaseMesh.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      MaterialLibrary := nil
    else if AComponent = FLightmapLibrary then
      LightmapLibrary := nil;
  end;
  inherited;
end;

// AxisAlignedDimensionsUnscaled
//

function TVKBaseMesh.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    MeshObjects.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := (dMax.X - dMin.X) / 2;
    FAxisAlignedDimensionsCache.Y := (dMax.Y - dMin.Y) / 2;
    FAxisAlignedDimensionsCache.Z := (dMax.Z - dMin.Z) / 2;
    FAxisAlignedDimensionsCache.W := 0;
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

// BarycenterOffset
//

function TVKBaseMesh.BarycenterOffset: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FBaryCenterOffsetChanged then
  begin
    MeshObjects.GetExtents(dMin, dMax);

    FBaryCenterOffset.X := (dMin.X + dMax.X) / 2;
    FBaryCenterOffset.Y := (dMin.Y + dMax.Y) / 2;
    FBaryCenterOffset.Z := (dMin.Z + dMax.Z) / 2;
    FBaryCenterOffset.W := 0;
    FBaryCenterOffsetChanged := False;
  end;
  Result := FBaryCenterOffset;
end;

// BarycenterPosition
//

function TVKBaseMesh.BarycenterPosition: TVector;
begin
  Result := VectorAdd(Position.DirectVector, BarycenterOffset);
end;

// BarycenterAbsolutePosition
//
function TVKBaseMesh.BarycenterAbsolutePosition: TVector;
begin
  Result := LocalToAbsolute(BarycenterPosition);
end;

// DestroyHandle
//
procedure TVKBaseMesh.DestroyHandle;
begin
  if Assigned(FMaterialLibrary) then
    MaterialLibrary.DestroyHandles;
  if Assigned(FLightmapLibrary) then
    LightmapLibrary.DestroyHandles;
  inherited;
end;

// PrepareVectorFile
//

procedure TVKBaseMesh.PrepareVectorFile(aFile: TVKVectorFile);
begin
  aFile.NormalsOrientation := NormalsOrientation;
end;

// PerformAutoCentering
//

procedure TVKBaseMesh.PerformAutoCentering;
var
  delta, min, max: TAffineVector;
begin
  if macUseBarycenter in AutoCentering then
  begin
    delta := VectorNegate(GetBarycenter);
  end
  else
  begin
    GetExtents(min, max);
    if macCenterX in AutoCentering then
      delta.X := -0.5 * (min.X + max.X)
    else
      delta.X := 0;
    if macCenterY in AutoCentering then
      delta.Y := -0.5 * (min.Y + max.Y)
    else
      delta.Y := 0;
    if macCenterZ in AutoCentering then
      delta.Z := -0.5 * (min.Z + max.Z)
    else
      delta.Z := 0;
  end;
  MeshObjects.Translate(delta);

  if macRestorePosition in AutoCentering then
    Position.Translate(VectorNegate(delta));
end;

// PerformAutoScaling
//

procedure TVKBaseMesh.PerformAutoScaling;
var
  i: integer;
  vScal: TAffineFltVector;
begin
  if (FAutoScaling.DirectX <> 1) or (FAutoScaling.DirectY <> 1) or
    (FAutoScaling.DirectZ <> 1) then
  begin
    MakeVector(vScal, FAutoScaling.DirectX, FAutoScaling.DirectY,
      FAutoScaling.DirectZ);
    for i := 0 to MeshObjects.Count - 1 do
    begin
      MeshObjects[i].Vertices.Scale(vScal);
    end;
  end;
end;

// PrepareMesh
//

procedure TVKBaseMesh.PrepareMesh;
begin
  StructureChanged;
end;

// PrepareMaterialLibraryCache
//

procedure TVKBaseMesh.PrepareMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
    DropMaterialLibraryCache;
  MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
  FMaterialLibraryCachesPrepared := True;
end;

// DropMaterialLibraryCache
//

procedure TVKBaseMesh.DropMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
  begin
    MeshObjects.DropMaterialLibraryCache;
    FMaterialLibraryCachesPrepared := False;
  end;
end;

// PrepareBuildList
//

procedure TVKBaseMesh.PrepareBuildList(var mrci: TVKRenderContextInfo);
begin
  MeshObjects.PrepareBuildList(mrci);
  if LightmapLibrary <> nil then
    LightmapLibrary.Materials.PrepareBuildList
end;

// SetUseMeshMaterials
//

procedure TVKBaseMesh.SetUseMeshMaterials(const val: Boolean);
begin
  if val <> FUseMeshMaterials then
  begin
    FUseMeshMaterials := val;
    if FMaterialLibraryCachesPrepared and (not val) then
      DropMaterialLibraryCache;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TVKBaseMesh.BuildList(var rci: TVKRenderContextInfo);
begin
  MeshObjects.BuildList(rci);
end;

// DoRender
//

procedure TVKBaseMesh.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  if Assigned(LightmapLibrary) then
    xgl.ForbidSecondTextureUnit;
  if renderSelf then
  begin
    // set winding
    case FNormalsOrientation of
      mnoDefault: ; // nothing
      mnoInvert: rci.GLStates.InvertGLFrontFace;
    else
      Assert(False);
    end;
    if not rci.ignoreMaterials then
    begin
      if UseMeshMaterials and Assigned(MaterialLibrary) then
      begin
        rci.materialLibrary := MaterialLibrary;
        if not FMaterialLibraryCachesPrepared then
          PrepareMaterialLibraryCache;
      end
      else
        rci.materialLibrary := nil;
      if Assigned(LightmapLibrary) then
        rci.lightmapLibrary := LightmapLibrary
      else
        rci.lightmapLibrary := nil;
      if rci.amalgamating
        or not (ListHandleAllocated or (osDirectDraw in ObjectStyle)) then
        PrepareBuildList(rci);
      Material.Apply(rci);
      repeat
        if (osDirectDraw in ObjectStyle)
          or rci.amalgamating
          or UseMeshMaterials then
          BuildList(rci)
        else
          rci.GLStates.CallList(GetHandle(rci));
      until not Material.UnApply(rci);
      rci.materialLibrary := nil;
    end
    else
    begin
      if (osDirectDraw in ObjectStyle) or rci.amalgamating then
        BuildList(rci)
      else
        rci.GLStates.CallList(GetHandle(rci));
    end;
    if FNormalsOrientation <> mnoDefault then
      rci.GLStates.InvertGLFrontFace;
  end;
  if Assigned(LightmapLibrary) then
    xgl.AllowSecondTextureUnit;
  if renderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, rci);
end;

// StructureChanged
//

procedure TVKBaseMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  DropMaterialLibraryCache;
  MeshObjects.Prepare;
  inherited;
end;

// StructureChangedNoPrepare
//

procedure TVKBaseMesh.StructureChangedNoPrepare;
begin
  inherited StructureChanged;
end;

// RayCastIntersect
//

function TVKBaseMesh.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  i: Integer;
  tris: TAffineVectorList;
  locRayStart, locRayVector, iPoint, iNormal: TVector;
  d, minD: Single;
begin
  // BEWARE! Utterly inefficient implementation!
  tris := MeshObjects.ExtractTriangles;
  try
    SetVector(locRayStart, AbsoluteToLocal(rayStart));
    SetVector(locRayVector, AbsoluteToLocal(rayVector));
    minD := -1;
    i := 0;
    while i < tris.Count do
    begin
      if RayCastTriangleIntersect(locRayStart, locRayVector,
        tris.List^[i], tris.List^[i + 1], tris.List^[i + 2],
        @iPoint, @iNormal) then
      begin
        d := VectorDistance2(locRayStart, iPoint);
        if (d < minD) or (minD < 0) then
        begin
          minD := d;
          if intersectPoint <> nil then
            intersectPoint^ := iPoint;
          if intersectNormal <> nil then
            intersectNormal^ := iNormal;
        end;
      end;
      Inc(i, 3);
    end;
  finally
    tris.Free;
  end;
  Result := (minD >= 0);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

// GenerateSilhouette
//

function TVKBaseMesh.GenerateSilhouette(const silhouetteParameters:
  TVKSilhouetteParameters): TVKSilhouette;
var
  mc: TVKBaseMeshConnectivity;
  sil: TVKSilhouette;
begin
  sil := nil;
  if Assigned(FConnectivity) then
  begin
    mc := TVKBaseMeshConnectivity(FConnectivity);
    mc.CreateSilhouette(silhouetteParameters,
      sil,
      true);
  end
  else
  begin
    mc := TVKBaseMeshConnectivity.CreateFromMesh(Self);
    try
      mc.CreateSilhouette(silhouetteParameters,
        sil,
        true);
    finally
      mc.Free;
    end;
  end;
  Result := sil;
end;

// BuildSilhouetteConnectivityData
//

procedure TVKBaseMesh.BuildSilhouetteConnectivityData;
var
  i, j: Integer;
  mo: TVKMeshObject;
begin
  FreeAndNil(FConnectivity);
  // connectivity data works only on facegroups of TFGVertexIndexList class
  for i := 0 to MeshObjects.Count - 1 do
  begin
    mo := (MeshObjects[i] as TVKMeshObject);
    if mo.Mode <> momFacegroups then
      Exit;
    for j := 0 to mo.FaceGroups.Count - 1 do
      if not mo.FaceGroups[j].InheritsFrom(TFGVertexIndexList) then
        Exit;
  end;
  FConnectivity := TVKBaseMeshConnectivity.CreateFromMesh(Self);
end;

// ------------------
// ------------------ TVKFreeForm ------------------
// ------------------

// Create
//

constructor TVKFreeForm.Create(AOwner: TComponent);
begin
  inherited;
//  ObjectStyle := [osDirectDraw];
  FUseMeshMaterials := True;
end;

// Destroy
//

destructor TVKFreeForm.Destroy;
begin
  FOctree.Free;
  inherited Destroy;
end;

// GetOctree
//

function TVKFreeForm.GetOctree: TOctree;
begin
  //   if not Assigned(FOctree) then     //If auto-created, can never use "if Assigned(GLFreeform1.Octree)"
  //     FOctree:=TOctree.Create;        //moved this code to BuildOctree
  Result := FOctree;
end;

// BuildOctree
//

procedure TVKFreeForm.BuildOctree(TreeDepth: integer = 3);
var
  emin, emax: TAffineVector;
  tl: TAffineVectorList;
begin
  if not Assigned(FOctree) then //moved here from GetOctree
    FOctree := TOctree.Create;

  GetExtents(emin, emax);
  tl := MeshObjects.ExtractTriangles;
  try
    with Octree do
    begin
      DisposeTree;
      InitializeTree(emin, emax, tl, TreeDepth);
    end;
  finally
    tl.Free;
  end;
end;

// OctreeRayCastIntersect
//

function TVKFreeForm.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.RayCastIntersect(locRayStart, locRayVector,
    intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

// OctreePointInMesh
//

function TVKFreeForm.OctreePointInMesh(const Point: TVector): boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal: TVector;
  BRad: double;
  HitCount: integer;
  hitDot: double;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  result := false;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    exit;

  BRad := BoundingSphereRadius;

  // This could be a fixed vector, but a fixed vector could have a systemic
  // bug on an non-closed mesh, making it fail constantly for one or several
  // faces.
  rayVector := VectorMake(2 * random - 1, 2 * random - 1, 2 * random - 1);
  rayStart := VectorAdd(VectorScale(rayVector, -BRad), Point);

  HitCount := 0;

  while OctreeRayCastIntersect(rayStart, rayVector, @hitPoint, @hitNormal) do
  begin
    // Are we past our taget?
    if VectorDotProduct(rayVector, VectorSubtract(Point, hitPoint)) < 0 then
    begin
      result := HitCount > 0;
      exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot < 0 then
      inc(HitCount)
    else if hitDot > 0 then
      dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector, BRad /
      cPointRadiusStep));
  end;
end;

// OctreeSphereIntersect
//

function TVKFreeForm.OctreeSphereSweepIntersect(const rayStart, rayVector:
  TVector;
  const velocity, radius: Single;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.SphereSweepIntersect(locRayStart, locRayVector,
    velocity, radius,
    intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

// OctreeTriangleIntersect
//

function TVKFreeForm.OctreeTriangleIntersect(const v1, v2, v3: TAffineVector):
  boolean;
var
  t1, t2, t3: TAffineVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(t1, AbsoluteToLocal(v1));
  SetVector(t2, AbsoluteToLocal(v2));
  SetVector(t3, AbsoluteToLocal(v3));

  Result := Octree.TriangleIntersect(t1, t2, t3);
end;

// OctreeAABBIntersect
//

function TVKFreeForm.OctreeAABBIntersect(const AABB: TAABB;
  objMatrix, invObjMatrix: TMatrix; triangles: TAffineVectorList = nil):
    boolean;
var
  m1to2, m2to1: TMatrix;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  //get matrixes needed
  //object to self
  MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
  //self to object
  MatrixMultiply(AbsoluteMatrix, invObjMatrix, m2to1);

  result := octree.AABBIntersect(aabb, m1to2, m2to1, triangles);
end;

// ------------------
// ------------------ TVKActorAnimation ------------------
// ------------------

// Create
//

constructor TVKActorAnimation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TVKActorAnimation.Destroy;
begin
  with (Collection as TVKActorAnimations).FOwner do
    if FTargetSmoothAnimation = Self then
      FTargetSmoothAnimation := nil;
  inherited Destroy;
end;

// Assign
//

procedure TVKActorAnimation.Assign(Source: TPersistent);
begin
  if Source is TVKActorAnimation then
  begin
    FName := TVKActorAnimation(Source).FName;
    FStartFrame := TVKActorAnimation(Source).FStartFrame;
    FEndFrame := TVKActorAnimation(Source).FEndFrame;
    FReference := TVKActorAnimation(Source).FReference;
  end
  else
    inherited;
end;

// GetDisplayName
//

function TVKActorAnimation.GetDisplayName: string;
begin
  Result := Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

// FrameCount
//

function TVKActorAnimation.FrameCount: Integer;
begin
  case Reference of
    aarMorph:
      Result :=
        TVKActorAnimations(Collection).FOwner.MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := TVKActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
  else
    Result := 0;
    Assert(False);
  end;
end;

// SetStartFrame
//

procedure TVKActorAnimation.SetStartFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FStartFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FStartFrame := m - 1
    else
      FStartFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FEndFrame := FStartFrame;
end;

// SetEndFrame
//

procedure TVKActorAnimation.SetEndFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FEndFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FEndFrame := m - 1
    else
      FEndFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FStartFrame := FEndFrame;
end;

// SetReference
//

procedure TVKActorAnimation.SetReference(val: TVKActorAnimationReference);
begin
  if val <> FReference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
  end;
end;

// SetAsString
//

procedure TVKActorAnimation.SetAsString(const val: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := val;
    Assert(sl.Count >= 3);
    FName := sl[0];
    FStartFrame := StrToInt(sl[1]);
    FEndFrame := StrToInt(sl[2]);
    if sl.Count = 4 then
    begin
      if LowerCase(sl[3]) = 'morph' then
        Reference := aarMorph
      else if LowerCase(sl[3]) = 'skeleton' then
        Reference := aarSkeleton
      else
        Assert(False);
    end
    else
      Reference := aarMorph;
  finally
    sl.Free;
  end;
end;

// GetAsString
//

function TVKActorAnimation.GetAsString: string;
const
  cAARToString: array[aarMorph..aarSkeleton] of string = ('morph', 'skeleton');
begin
  Result := Format('"%s",%d,%d,%s',
    [FName, FStartFrame, FEndFrame, cAARToString[Reference]]);
end;

// OwnerActor
//

function TVKActorAnimation.OwnerActor: TVKActor;
begin
  Result := ((Collection as TVKActorAnimations).GetOwner as TVKActor);
end;

// MakeSkeletalTranslationStatic
//

procedure TVKActorAnimation.MakeSkeletalTranslationStatic;
begin
  OwnerActor.Skeleton.MakeSkeletalTranslationStatic(StartFrame, EndFrame);
end;

// MakeSkeletalRotationDelta
//

procedure TVKActorAnimation.MakeSkeletalRotationDelta;
begin
  OwnerActor.Skeleton.MakeSkeletalRotationDelta(StartFrame, EndFrame);
end;

// ------------------
// ------------------ TVKActorAnimations ------------------
// ------------------

// Create
//

constructor TVKActorAnimations.Create(AOwner: TVKActor);
begin
  FOwner := AOwner;
  inherited Create(TVKActorAnimation);
end;

// GetOwner
//

function TVKActorAnimations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TVKActorAnimations.SetItems(index: Integer; const val: TVKActorAnimation);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TVKActorAnimations.GetItems(index: Integer): TVKActorAnimation;
begin
  Result := TVKActorAnimation(inherited Items[index]);
end;

// Last
//

function TVKActorAnimations.Last: TVKActorAnimation;
begin
  if Count > 0 then
    Result := TVKActorAnimation(inherited Items[Count - 1])
  else
    Result := nil;
end;

// Add
//

function TVKActorAnimations.Add: TVKActorAnimation;
begin
  Result := (inherited Add) as TVKActorAnimation;
end;

// FindItemID
//

function TVKActorAnimations.FindItemID(ID: Integer): TVKActorAnimation;
begin
  Result := (inherited FindItemID(ID)) as TVKActorAnimation;
end;

// FindName
//

function TVKActorAnimations.FindName(const aName: string): TVKActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

// FindFrame
//

function TVKActorAnimations.FindFrame(aFrame: Integer;
  aReference: TVKActorAnimationReference): TVKActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    with Items[i] do
      if (StartFrame <= aFrame) and (EndFrame >= aFrame)
        and (Reference = aReference) then
      begin
        Result := Items[i];
        Break;
      end;
end;

// SetToStrings
//

procedure TVKActorAnimations.SetToStrings(aStrings: TStrings);

var
  i: Integer;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Self.Count - 1 do
      Add(Self.Items[i].Name);
    EndUpdate;
  end;
end;

// SaveToStream
//

procedure TVKActorAnimations.SaveToStream(aStream: TStream);
var
  i: Integer;
begin
  WriteCRLFString(aStream, cAAFHeader);
  WriteCRLFString(aStream, AnsiString(IntToStr(Count)));
  for i := 0 to Count - 1 do
    WriteCRLFString(aStream, AnsiString(Items[i].AsString));
end;

// LoadFromStream
//

procedure TVKActorAnimations.LoadFromStream(aStream: TStream);
var
  i, n: Integer;
begin
  Clear;
  if ReadCRLFString(aStream) <> cAAFHeader then
    Assert(False);
  n := StrToInt(string(ReadCRLFString(aStream)));
  for i := 0 to n - 1 do
    Add.AsString := string(ReadCRLFString(aStream));
end;

// SaveToFile
//

procedure TVKActorAnimations.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// LoadFromFile
//

procedure TVKActorAnimations.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TVKBaseAnimationControler ------------------
// ------------------

// Create
//

constructor TVKBaseAnimationControler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

// Destroy
//

destructor TVKBaseAnimationControler.Destroy;
begin
  SetActor(nil);
  inherited Destroy;
end;

// Notification
//

procedure TVKBaseAnimationControler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FActor) and (Operation = opRemove) then
    SetActor(nil);
  inherited;
end;

// DoChange
//

procedure TVKBaseAnimationControler.DoChange;
begin
  if Assigned(FActor) then
    FActor.NotifyChange(Self);
end;

// SetEnabled
//

procedure TVKBaseAnimationControler.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    if Assigned(FActor) then
      DoChange;
  end;
end;

// SetActor
//

procedure TVKBaseAnimationControler.SetActor(const val: TVKActor);
begin
  if FActor <> val then
  begin
    if Assigned(FActor) then
      FActor.UnRegisterControler(Self);
    FActor := val;
    if Assigned(FActor) then
    begin
      FActor.RegisterControler(Self);
      DoChange;
    end;
  end;
end;

// Apply
//

function TVKBaseAnimationControler.Apply(var lerpInfo: TBlendedLerpInfo):
  Boolean;
begin
  // virtual
  Result := False;
end;

// ------------------
// ------------------ TVKAnimationControler ------------------
// ------------------

// DoChange
//

procedure TVKAnimationControler.DoChange;
begin
  if AnimationName <> '' then
    inherited;
end;

// SetAnimationName
//

procedure TVKAnimationControler.SetAnimationName(const val:
  TVKActorAnimationName);
begin
  if FAnimationName <> val then
  begin
    FAnimationName := val;
    DoChange;
  end;
end;

// SetRatio
//

procedure TVKAnimationControler.SetRatio(const val: Single);
begin
  if FRatio <> val then
  begin
    FRatio := ClampValue(val, 0, 1);
    DoChange;
  end;
end;

// Apply
//

function TVKAnimationControler.Apply(var lerpInfo: TBlendedLerpInfo): Boolean;
var
  anim: TVKActorAnimation;
  baseDelta: Integer;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;

  anim := Actor.Animations.FindName(AnimationName);
  Result := (anim <> nil);
  if not Result then
    Exit;

  with lerpInfo do
  begin
    if Ratio = 0 then
    begin
      frameIndex1 := anim.StartFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else if Ratio = 1 then
    begin
      frameIndex1 := anim.EndFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else
    begin
      baseDelta := anim.EndFrame - anim.StartFrame;
      lerpFactor := anim.StartFrame + baseDelta * Ratio;
      frameIndex1 := VKS.VectorGeometry.Trunc(lerpFactor);
      frameIndex2 := frameIndex1 + 1;
      lerpFactor := VKS.VectorGeometry.Frac(lerpFactor);
    end;
    weight := 1;
    externalRotations := nil;
    externalQuaternions := nil;
  end;
end;

// ------------------
// ------------------ TVKActor ------------------
// ------------------

// Create
//

constructor TVKActor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FFrameInterpolation := afpLinear;
  FAnimationMode := aamNone;
  FInterval := 100; // 10 animation frames per second
  FAnimations := TVKActorAnimations.Create(Self);
  FControlers := nil; // created on request
  FOptions := cDefaultGLActorOptions;
end;

// Destroy
//

destructor TVKActor.Destroy;
begin
  inherited Destroy;
  FControlers.Free;
  FAnimations.Free;
end;

// Assign
//

procedure TVKActor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVKActor then
  begin
    FAnimations.Assign(TVKActor(Source).FAnimations);
    FAnimationMode := TVKActor(Source).FAnimationMode;
    Synchronize(TVKActor(Source));
  end;
end;

// RegisterControler
//

procedure TVKActor.RegisterControler(aControler: TVKBaseAnimationControler);
begin
  if not Assigned(FControlers) then
    FControlers := TList.Create;
  FControlers.Add(aControler);
  FreeNotification(aControler);
end;

// UnRegisterControler
//

procedure TVKActor.UnRegisterControler(aControler: TVKBaseAnimationControler);
begin
  Assert(Assigned(FControlers));
  FControlers.Remove(aControler);
  RemoveFreeNotification(aControler);
  if FControlers.Count = 0 then
    FreeAndNil(FControlers);
end;

// SetCurrentFrame
//

procedure TVKActor.SetCurrentFrame(val: Integer);
begin
  if val <> CurrentFrame then
  begin
    if val > FrameCount - 1 then
      FCurrentFrame := FrameCount - 1
    else if val < 0 then
      FCurrentFrame := 0
    else
      FCurrentFrame := val;
    FCurrentFrameDelta := 0;
    case AnimationMode of
      aamPlayOnce: if (CurrentFrame = EndFrame) and (FTargetSmoothAnimation =
        nil) then
          FAnimationMode := aamNone;
      aamBounceForward: if CurrentFrame = EndFrame then
          FAnimationMode := aamBounceBackward;
      aamBounceBackward: if CurrentFrame = StartFrame then
          FAnimationMode := aamBounceForward;
    end;
    StructureChanged;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
  end;
end;

// SetCurrentFrame
//

procedure TVKActor.SetCurrentFrameDirect(const Value: Integer);
begin
  FCurrentFrame := Value;
end;

// SetStartFrame
//

procedure TVKActor.SetStartFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> StartFrame) then
    FStartFrame := val;
  if EndFrame < StartFrame then
    FEndFrame := FStartFrame;
  if CurrentFrame < StartFrame then
    CurrentFrame := FStartFrame;
end;

// SetEndFrame
//

procedure TVKActor.SetEndFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> EndFrame) then
    FEndFrame := val;
  if CurrentFrame > EndFrame then
    CurrentFrame := FEndFrame;
end;

// SetReference
//

procedure TVKActor.SetReference(val: TVKActorAnimationReference);
begin
  if val <> Reference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
    CurrentFrame := CurrentFrame;
    StructureChanged;
  end;
end;

// SetAnimations
//

procedure TVKActor.SetAnimations(const val: TVKActorAnimations);
begin
  FAnimations.Assign(val);
end;

// StoreAnimations
//

function TVKActor.StoreAnimations: Boolean;
begin
  Result := (FAnimations.Count > 0);
end;

// SetOptions
//

procedure TVKActor.SetOptions(const val: TVKActorOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// NextFrameIndex
//

function TVKActor.NextFrameIndex: Integer;
begin
  case AnimationMode of
    aamLoop, aamBounceForward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > EndFrame then
          begin
            Result := StartFrame + (Result - EndFrame - 1);
            if Result > EndFrame then
              Result := EndFrame;
          end;
        end;
      end;
    aamNone, aamPlayOnce:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > EndFrame then
            Result := EndFrame;
        end;
      end;
    aamBounceBackward, aamLoopBackward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame - 1;
          if Result < StartFrame then
          begin
            Result := EndFrame - (StartFrame - Result - 1);
            if Result < StartFrame then
              Result := StartFrame;
          end;
        end;
      end;
    aamExternal: Result := CurrentFrame; // Do nothing
  else
    Result := CurrentFrame;
    Assert(False);
  end;
end;

// NextFrame
//

procedure TVKActor.NextFrame(nbSteps: Integer = 1);
var
  n: Integer;
begin
  n := nbSteps;
  while n > 0 do
  begin
    CurrentFrame := NextFrameIndex;
    Dec(n);
    if Assigned(FOnEndFrameReached) and (CurrentFrame = EndFrame) then
      FOnEndFrameReached(Self);
    if Assigned(FOnStartFrameReached) and (CurrentFrame = StartFrame) then
      FOnStartFrameReached(Self);
  end;
end;

// PrevFrame
//

procedure TVKActor.PrevFrame(nbSteps: Integer = 1);
var
  value: Integer;
begin
  value := FCurrentFrame - nbSteps;
  if value < FStartFrame then
  begin
    value := FEndFrame - (FStartFrame - value);
    if value < FStartFrame then
      value := FStartFrame;
  end;
  CurrentFrame := Value;
end;

// DoAnimate
//

procedure TVKActor.DoAnimate();
var
  i, k: Integer;
  nextFrameIdx: Integer;
  lerpInfos: array of TBlendedLerpInfo;
begin
  nextFrameIdx := NextFrameIndex;
  case Reference of
    aarMorph: if nextFrameIdx >= 0 then
      begin
        case FrameInterpolation of
          afpLinear:
            MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
        else
          MeshObjects.MorphTo(CurrentFrame);
        end;
      end;
    aarSkeleton: if Skeleton.Frames.Count > 0 then
      begin
        if Assigned(FControlers) and (AnimationMode <> aamExternal) then
        begin
          // Blended Skeletal Lerping
          SetLength(lerpInfos, FControlers.Count + 1);
          if nextFrameIdx >= 0 then
          begin
            case FrameInterpolation of
              afpLinear: with lerpInfos[0] do
                begin
                  frameIndex1 := CurrentFrame;
                  frameIndex2 := nextFrameIdx;
                  lerpFactor := CurrentFrameDelta;
                  weight := 1;
                end;
            else
              with lerpInfos[0] do
              begin
                frameIndex1 := CurrentFrame;
                frameIndex2 := CurrentFrame;
                lerpFactor := 0;
                weight := 1;
              end;
            end;
          end
          else
          begin
            with lerpInfos[0] do
            begin
              frameIndex1 := CurrentFrame;
              frameIndex2 := CurrentFrame;
              lerpFactor := 0;
              weight := 1;
            end;
          end;
          k := 1;
          for i := 0 to FControlers.Count - 1 do
            if TVKBaseAnimationControler(FControlers[i]).Apply(lerpInfos[k])
              then
              Inc(k);
          SetLength(lerpInfos, k);
          Skeleton.BlendedLerps(lerpInfos);
        end
        else if (nextFrameIdx >= 0) and (AnimationMode <> aamExternal) then
        begin
          // Single Skeletal Lerp
          case FrameInterpolation of
            afpLinear:
              Skeleton.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta);
          else
            Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
          end;
        end;
        Skeleton.MorphMesh(aoSkeletonNormalizeNormals in Options);
      end;
    aarNone: ; // do nothing
  end;
end;

// BuildList
//

procedure TVKActor.BuildList(var rci: TVKRenderContextInfo);
begin
  DoAnimate;
  inherited;
  if OverlaySkeleton then
  begin
    rci.GLStates.Disable(stDepthTest);
    Skeleton.RootBones.BuildList(rci);
  end;
end;

// PrepareMesh
//

procedure TVKActor.PrepareMesh;
begin
  FStartFrame := 0;
  FEndFrame := FrameCount - 1;
  FCurrentFrame := 0;
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
  inherited;
end;

// PrepareBuildList
//

procedure TVKActor.PrepareBuildList(var mrci: TVKRenderContextInfo);
begin
  // no preparation needed for actors, they don't use buildlists
end;

// FrameCount
//

function TVKActor.FrameCount: Integer;
begin
  case Reference of
    aarMorph:
      Result := MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := Skeleton.Frames.Count;
    aarNone:
      Result := 0;
  else
    Result := 0;
    Assert(False);
  end;
end;

// DoProgress
//

procedure TVKActor.DoProgress(const progressTime: TProgressTimes);
var
  fDelta: Single;
begin
  inherited;
  if (AnimationMode <> aamNone) and (Interval > 0) then
  begin
    if (StartFrame <> EndFrame) and (FrameCount > 1) then
    begin
      FCurrentFrameDelta := FCurrentFrameDelta + (progressTime.deltaTime * 1000)
        / FInterval;
      if FCurrentFrameDelta > 1 then
      begin
        if Assigned(FTargetSmoothAnimation) then
        begin
          SwitchToAnimation(FTargetSmoothAnimation);
          FTargetSmoothAnimation := nil;
        end;
        // we need to step on
        fDelta := Frac(FCurrentFrameDelta);
        NextFrame(Trunc(FCurrentFrameDelta));
        FCurrentFrameDelta := fDelta;
        StructureChanged;
      end
      else if FrameInterpolation <> afpNone then
        StructureChanged;
    end;
  end;
end;

// LoadFromStream
//

procedure TVKActor.LoadFromStream(const fileName: string; aStream: TStream);
begin
  if fileName <> '' then
  begin
    Animations.Clear;
    inherited LoadFromStream(fileName, aStream);
  end;
end;

// SwitchToAnimation
//

procedure TVKActor.SwitchToAnimation(const animationName: string; smooth: Boolean
  = False);
begin
  SwitchToAnimation(Animations.FindName(animationName), smooth);
end;

// SwitchToAnimation
//

procedure TVKActor.SwitchToAnimation(animationIndex: Integer; smooth: Boolean =
  False);
begin
  if (animationIndex >= 0) and (animationIndex < Animations.Count) then
    SwitchToAnimation(Animations[animationIndex], smooth);
end;

// SwitchToAnimation
//

procedure TVKActor.SwitchToAnimation(anAnimation: TVKActorAnimation; smooth:
  Boolean = False);
begin
  if Assigned(anAnimation) then
  begin
    if smooth then
    begin
      FTargetSmoothAnimation := anAnimation;
      FCurrentFrameDelta := 0;
    end
    else
    begin
      Reference := anAnimation.Reference;
      StartFrame := anAnimation.StartFrame;
      EndFrame := anAnimation.EndFrame;
      CurrentFrame := StartFrame;
    end;
  end;
end;

// CurrentAnimation
//

function TVKActor.CurrentAnimation: string;
var
  aa: TVKActorAnimation;
begin
  aa := Animations.FindFrame(CurrentFrame, Reference);
  if Assigned(aa) then
    Result := aa.Name
  else
    Result := '';
end;

// Synchronize
//

procedure TVKActor.Synchronize(referenceActor: TVKActor);
begin
  if Assigned(referenceActor) then
  begin
    if referenceActor.StartFrame < FrameCount then
      FStartFrame := referenceActor.StartFrame;
    if referenceActor.EndFrame < FrameCount then
      FEndFrame := referenceActor.EndFrame;
    FReference := referenceActor.Reference;
    if referenceActor.CurrentFrame < FrameCount then
      FCurrentFrame := referenceActor.CurrentFrame;
    FCurrentFrameDelta := referenceActor.CurrentFrameDelta;
    FAnimationMode := referenceActor.AnimationMode;
    FFrameInterpolation := referenceActor.FrameInterpolation;
    if referenceActor.FTargetSmoothAnimation <> nil then
      FTargetSmoothAnimation :=
        Animations.FindName(referenceActor.FTargetSmoothAnimation.Name)
    else
      FTargetSmoothAnimation := nil;
    if (Skeleton.Frames.Count > 0) and (referenceActor.Skeleton.Frames.Count > 0)
      then
      Skeleton.Synchronize(referenceActor.Skeleton);
  end;
end;

function TVKActor.isSwitchingAnimation: boolean;
begin
  result := FTargetSmoothAnimation <> nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterVectorFileFormat('glsm', 'GLScene Mesh', TVKGLSMVectorFile);

  RegisterClasses([TVKFreeForm, TVKActor, TVKSkeleton, TVKSkeletonFrame,
    TVKSkeletonBone,
    TVKSkeletonMeshObject, TVKMeshObject, TVKSkeletonFrameList, TVKMeshMorphTarget,
      TVKMorphableMeshObject, TVKFaceGroup, TFGVertexIndexList,
      TFGVertexNormalTexIndexList, TVKAnimationControler,
      TFGIndexTexCoordList, TVKSkeletonCollider, TVKSkeletonColliderList]);

finalization

  FreeAndNil(vVectorFileFormats);

end.