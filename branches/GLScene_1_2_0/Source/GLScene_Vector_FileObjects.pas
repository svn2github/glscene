//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLVectorFileObjects<p>

  Vector File related objects for GLScene<p>

  <b>History :</b><font size=-1><ul>
  <li>23/02/11 - Yar - Added extent caching to TMeshObject
  <li>03/12/10 - Yar - Added mesh visibility checking in TMeshObjectList.ExtractTriangles (thnaks to Sandor Domokos)
  <li>23/08/10 - Yar - Added OpenGLTokens to uses
  <li>23/07/10 - Yar - Bugfixed TSkeleton.WriteToFiler (thanks E-Cone)
  <li>11/06/10 - Yar - Bugfixed binary reading TGLMeshObject for FPC
  Replace OpenGL1x functions to OpenGLAdapter.
  Fixes for Linux x64
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>11/04/10 - Yar - Replaced function InsideList to GLState.InsideList
  <li>05/03/10 - DanB - More state added to TGLStateCache
  <li>25/12/09 - DaStr - Separated TGLActor.DoAnimate() from TGLActor.BuildList()
  <li>16/01/09 - DanB - re-disable VBOs in display list to prevent AV on ATI cards
  <li>27/11/08 - DanB - fix to TFGVertexIndexList.BuildList
  <li>05/10/08 - DaStr - Added GLSM format backward compatibility after
  MeshObject.LightMapTexCoords update
  (thanks Uwe Raabe) (Bugtracker ID = 2140994)
  <li>03/10/08 - DanB -  Added Delphi 2009 (Unicode) support
  <li>22/06/08 - DaStr - TMeshObject.LightMapTexCoords converted to TAffineVectorList
  (thanks Ast) (Bugtracker ID = 2000089)
  <li>07/06/08 - DaStr - Implemented TBaseMeshObject.Assign(), TMeshObject.Assign()
  <li>20/05/08 - Mrqzzz - Fixed memory leak in TSkeletonMeshObject.Destroy (thanks Dave Gravel)
  <li>17/05/08 - DaStr - Added TSkeleton.MorphInvisibleParts
  (thanks andron13 and Veon (BugtrackerID = 1966020)
  Added vGLVectorFileObjectsEnableVBOByDefault
  <li>01/05/08 - DaStr - Implemented TGLBaseMesh.BarycenterAbsolutePosition()
  Bugfixed TGLBaseMesh.AxisAlignedDimensionsUnscaled()
  <li>06/04/08 - DaStr - TMeshObjectList.MorphTo() and Lerp() are now virtual
  <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
  <li>16/05/07 - PvD - Applied fixes to skeletonmesh to fix problems with
  physics engines. (Bugtracker ID = 1719652)
  <li>15/05/07 - LC - Added workaround for ATI bug in TFGVertexIndexList. (Bugtracker ID = 1719611)
  <li>13/05/07 - LC - Fixed AV bug in TMeshObject.BufferArrays (Bugtracker ID = 1718033)
  <li>03/04/07 - LC - Added VBO support for TextureEx (Bugtracker ID = 1693378)
  <li>30/03/07 - DaStr - Added $I GLScene.inc
  <li>28/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>25/03/07 - LC - Added VBO support to TFGVertexIndexList, depends
  on MeshObject owner's UseVBO status
  <li>25/03/07 - LC - Fixed VBO bug. Bugtracker ID=1687665
  <li>16/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>21/02/07 - DaStr - Added TMeshObjectList.BuildTangentSpace, UseVBO
  Added TGLActor.SetCurrentFrameDirect
  <li>19/02/07 - LC - Added some VBO support
  <li>19/10/06 - LC - Fixed bug in TGLActor.SetCurrentFrame. Bugtracker ID=1580511
  <li>04/10/06 - PhP - fixed TGLActor.SetCurrentFrame (thanks dikoe)
  <li>05/12/05 - PhP - fixed TFGIndexTexCoordList.BuildList (thanks fig)
  <li>10/11/05 - Mathx - Added LastLoadedFilename to TGLBaseMesh (RFE 955083).
  <li>09/11/05 - Mathx - Added isSwitchingAnimation to TGLActor.
  <li>05/09/05 - Mathx - Fixed TSkeletonMeshObject read/write filer (thanks to Zapology)
  <li>04/07/05 - Mathx - Protection against picking mode texture mapping errors
  <li>27/01/05 - Mathx - BuildOctree can now specify an (optional) TreeDepth.
  <li>11/01/05 - SG - Another fix for TGLBaseMesh.Assign (dikoe Kenguru)
  <li>11/01/05 - SG - Fix for TGLBaseMesh.Assign when assigning actors
  <li>26/11/04 - MRQZZZ - by Uwe Raabe : fixed TBaseMeshObject.BuildNormals
  <li>26/11/04 - MRQZZZ - Added "Rendered" property to TGLBaseMesh in order
  to prevent rendering of the GLBaseMesh but allowing
  the rendering of it's children
  <li>25/11/04 - SG - Fixed memory leak in TMeshObject (dikoe Kenguru)
  <li>24/11/04 - MF - Added OctreePointInMesh
  <li>03/10/04 - MRQZZZ - Fixed memory leak (FAutoScaling.Free) in TGLBaseMesh.Destroy; (thanks Jan Zizka)
  <li>24/09/04 - SG - Added GetTriangleData/SetTriangleData functions,
  Added TexCoordsEx, Binormals, Tangents,
  Added BuildTangentSpace function (experimental).
  <li>23/07/04 - SG - Added fgmmQuad case for TFGVertexIndexList.TraingleCount
  (Thanks fig).
  <li>18/07/04 - LR - Suppress Consts in uses
  <li>20/06/04 - MRQZZZ - Added AutoScaling property to GLBaseMesh to scale
  a mesh after loading (like Autocentering)
  <li>30/03/04 - EG - Added TSkeletonBoneList.BoneCount
  <li>23/03/04 - SG - External positions added to skeleton blended lerps.
  AutoUpdate flag added to skeleton collider list.
  <li>09/03/04 - SG - TFGIndexTexCoordList.BuildList can now use per vertex color
  <li>29/01/04 - SG - Fix for ApplyCurrentSkeletonFrame with multiple bones per vertex.
  Mesh reassembles correctly now (tested up to 4 bones per vertex).
  <li>03/12/03 - SG - Added TSkeletonCollider and TSkeletonColliderList
  Added Colliders (TSkeletonColliderList) to TSkeleton
  <li>24/10/03 - SG - Various fixes for multiple bones per vertex
  <li>21/09/03 - MRQZZZ - Added "aamLoopBackward" to AnimationMode property
  <li>19/09/03 - EG - "Lighmap" -&gt; "LightMap"
  <li>01/09/03 - SG - Added skeleton frame conversion methods to convert between
  Rotations and Quaternions.
  <li>27/08/03 - SG - Fixed AddWeightedBone for multiple bones per vertex
  <li>13/08/03 - SG - Added quaternion transforms for skeletal animation
  <li>12/08/03 - SG - Fixed a tiny bug in TSkeleton.MorphMesh
  <li>08/07/03 - EG - Fixed puny bug in skeletal normals transformation
  <li>05/06/03 - SG - Split SMD, MD2, 3DS, PLY, TIN and GTS code into separate units,
  FileFormats\GLFile???.pas
  <li>16/05/03 - SG - Fixed OpenGL error caused by glColorMaterial in TMeshObject.BuildList
  <li>08/05/03 - DanB - added OctreeAABBIntersect (Matheus Degiovani)
  <li>07/05/03 - SG - Added TGLSMDVectorFile.SaveToFile method and [read,write] capabilities
  <li>17/04/03 - SG - Added TMeshObjectList.FindMeshByName method
  <li>01/04/03 - SG - Fixed TGLBaseMesh.Assign
  <li>13/02/03 - DanB - added AxisAlignedDimensionsUnscaled
  <li>03/02/03 - EG - Faster PrepareBuildList logic
  <li>31/01/03 - EG - Added MaterialCache logic
  <li>30/01/03 - EG - Fixed color array enable/disable (Nelson Chu),
  Normals extraction and extraction standardization
  <li>27/01/03 - EG - Assign support, fixed MorphableMeshObjects persistence
  <li>16/01/03 - EG - Updated multiples Bones per vertex transformation code,
  now makes use of CVAs
  <li>14/01/03 - EG - Added DisableOpenGLArrays
  <li>09/01/03 - EG - Added Clear methods for MeshObjects
  <li>25/11/02 - EG - Colors and TexCoords lists now disabled if ignoreMaterials is true
  <li>23/10/02 - EG - Faster .GTS and .PLY imports (parsing)
  <li>22/10/02 - EG - Added actor options, fixed skeleton normals transform (thx Marcus)
  <li>21/10/02 - EG - Read support for .GTS (GNU Triangulated Surface library)
  <li>18/10/02 - EG - FindExtByIndex (Adem)
  <li>17/10/02 - EG - TGLSTLVectorFile moved to new GLFileSTL unit
  <li>04/09/02 - EG - Fixed TGLBaseMesh.AxisAlignedDimensions
  <li>23/08/02 - EG - Added TGLBaseMesh.Visible
  <li>23/07/02 - EG - TGLBaseMesh.LoadFromStream fix (D. Angilella)
  <li>13/07/02 - EG - AutoCenter on barycenter
  <li>22/03/02 - EG - TGLAnimationControler basics now functional
  <li>13/03/02 - EG - Octree support (experimental)
  <li>18/02/02 - EG - Fixed persistence of skeletal meshes
  <li>04/01/02 - EG - Added basic RayCastIntersect implementation
  <li>17/12/01 - EG - Upgraded TGLActor.Synchronize (smooth transitions support)
  <li>30/11/01 - EG - Added smooth transitions (based on Mrqzzz code)
  <li>14/09/01 - EG - Use of vFileStreamClass
  <li>18/08/01 - EG - Added TriangleCount methods, STL export, PLY import
  <li>15/08/01 - EG - FaceGroups can now be rendered by material group
  (activate with RenderingOption "moroGroupByMaterial")
  <li>14/08/01 - EG - Added TSkeletonBoneList and support for skeleton with
  multiple root bones, updated SMD loader
  <li>13/08/01 - EG - Improved/fixed SMD loader
  <li>12/08/01 - EG - Completely rewritten handles management,
  Fixed TActorAnimation.Assign,
  Fixed persistence
  <li>08/08/01 - EG - Added TGLBaseMesh.AxisAlignedDimensions
  <li>19/07/01 - EG - AutoCentering is now a property of TGLBaseMesh,
  3DS loader no longer auto-centers,
  Added ExtractTriangles and related methods
  <li>18/07/01 - EG - VisibilityCulling compatibility changes
  <li>19/06/01 - EG - StrToFloat outlawed and replaced by StrToFloatDef
  <li>25/03/01 - EG - Added TGLAnimationControler
  <li>18/03/01 - EG - Added basic Skeleton structures & SMD importer
  <li>16/03/01 - EG - Introduced new PersistentClasses
  <li>15/03/01 - EG - Fix in TActorAnimation.SetEndFrame (thx David Costa)
  <li>08/03/01 - EG - TGL3DSVectorFile now loads materials for TGLBaseMesh
  <li>26/02/01 - EG - Added TBaseMeshObject & BuildNormals, MD2 normals auto-builded
  <li>21/02/01 - EG - Now XOpenGL based (multitexture)
  <li>15/01/01 - EG - Added Translate methods
  <li>10/01/01 - EG - Fixed in TGLBaseMesh.DoRender for RenderChildren states
  <li>08/01/01 - EG - Fixed TGLBaseMesh.BuildList messup of attrib states
  <li>22/12/00 - EG - Fixed non-interpolated TGLActor animation (was freezing),
  Fixed TGLBaseMesh.DoRender messup of attrib states
  <li>18/12/00 - EG - TFGIndexTexCoordList now supports normals (automatically),
  NormalsOrientation code moved to TGLBaseMesh
  <li>11/12/00 - EG - Fix for NormalOrientation (3DS importer)
  <li>06/12/00 - EG - Added PrepareBuildList mechanism
  <li>08/10/00 - EG - Removed TGLOBJVectorFile, use GLFileOBJ instead
  <li>13/08/00 - EG - Enhancements for Portal Rendering support,
  Added utility methods & triangle fans
  <li>10/08/00 - EG - Added CurrentAnimation, fixed TMeshObject.GetExtents
  <li>21/07/00 - EG - Vastly improved memory use and mechanisms for MD2/TGLActor
  <li>19/07/00 - EG - Introduced enhanced mesh structure
  <li>16/07/00 - EG - Made use of new TDataFile class
  <li>15/07/00 - EG - FreeForm can now handle 3DS files with multiple textures,
  Added TGLBaseMesh.GetExtents
  <li>28/06/00 - EG - Support for "ObjectStyle"
  <li>23/06/00 - EG - Reversed "t" texture coord for MD2,
  TActorAnimations can now load/save
  <li>21/06/00 - EG - Added frame change events to TGLActor,
  Added TActorAnimations collection
  <li>19/06/00 - EG - Completed smooth movement interpolation for TGLActor
  <li>07/06/00 - EG - TVectorFile now longers assumes a TGLFreeForm as Owner,
  Added generic TVectorFile.LoadFromFile
  <li>26/05/00 - EG - Removed dependency to GLObjects,
  TGLFreeForm now may use InterleavedArrays instead of
  IndexedArrays (better BuildList compatibility)
  <li>22/04/00 - EG - Fixed Material handlings in TGLFreeForm, inverted CCW/CW
  convention for 3DS Release3
  <li>11/04/00 - EG - Removed unnecessary code in finalization (thanks Uwe)
  <li>09/02/00 - EG - Creation from split of GLObjects,
  fixed class registrations and formats unregistration
  </ul></font>
}
unit GLScene_Vector_FileObjects;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene_Core,
  GLScene_Base_OpenGL_Tokens,
  GLScene_Base_Vector_Geometry,
  SysUtils,
  GLScene_Texture,
  GLScene_Material,
  GLScene_Objects,
  GLScene_Base_Transformation,
  GLScene_Mesh,
  GLScene_DrawTechnique,
  GLScene_Base_Vector_Lists,
  GLScene_Base_PersistentClasses,
  GLScene_Base_Octree,
  GLScene_Base_GeometryBB,
  GLScene_Base_FileIO,
  GLScene_Silhouette,
  GLScene_Base_Context,
  GLScene_Base_Color,
  GLScene_Base_Context_Info,
  GLScene_Base_Coordinates,
  GLScene_Base_Classes,
  GLScene_Texture_Format;

type

  TMeshObjectList = class;
  TFaceGroups = class;

  // TMeshAutoCentering
  //
  TMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter);
  TMeshAutoCenterings = set of TMeshAutoCentering;

  // TMeshObjectMode
  //
  TMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

  // TBaseMeshObject
  //
  { : A base class for mesh objects.<p>
    The class introduces a set of vertices and normals for the object but
    does no rendering of its own. }
  TBaseMeshObject = class(TPersistentObject)
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
      var nb: Integer); virtual;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { : Clears all mesh object data, submeshes, facegroups, etc. }
    procedure Clear; dynamic;

    { : Translates all the vertices by the given delta. }
    procedure Translate(const delta: TAffineVector); dynamic;
    { : Builds (smoothed) normals for the vertex list.<p>
      If normalIndices is nil, the method assumes a bijection between
      vertices and normals sets, and when performed, Normals and Vertices
      list will have the same number of items (whatever previously was in
      the Normals list is ignored/removed).<p>
      If normalIndices is defined, normals will be added to the list and
      their indices will be added to normalIndices. Already defined
      normals and indices are preserved.<p>
      The only valid modes are currently momTriangles and momTriangleStrip
      (ie. momFaceGroups not supported). }
    procedure BuildNormals(vertexIndices: TIntegerList; mode: TMeshObjectMode;
      normalIndices: TIntegerList = nil);
    { : Extracts all mesh triangles as a triangles list.<p>
      The resulting list size is a multiple of 3, each group of 3 vertices
      making up and independant triangle.<br>
      The returned list can be used independantly from the mesh object
      (all data is duplicated) and should be freed by caller.<p>
      If texCoords is specified, per vertex texture coordinates will be
      placed there, when available. }
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; dynamic;

    property Name: string read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Vertices: TAffineVectorList read FVertices write SetVertices;
    property normals: TAffineVectorList read FNormals write SetNormals;
  end;

  TSkeletonFrameList = class;

  TSkeletonFrameTransform = (sftRotation, sftQuaternion);

  // TSkeletonFrame
  //
  { : Stores position and rotation for skeleton joints.<p>
    If you directly alter some values, make sure to call FlushLocalMatrixList
    so that the local matrices will be recalculated (the call to Flush does
    not recalculate the matrices, but marks the current ones as dirty). }
  TSkeletonFrame = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TSkeletonFrameList;
    FName: string;
    FPosition: TAffineVectorList;
    FRotation: TAffineVectorList;
    FQuaternion: TQuaternionList;
    FLocalMatrixList: PMatrixArray;
    FTransformMode: TSkeletonFrameTransform;

  protected
    { Protected Declarations }
    procedure SetPosition(const val: TAffineVectorList);
    procedure SetRotation(const val: TAffineVectorList);
    procedure SetQuaternion(const val: TQuaternionList);

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TSkeletonFrameList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TSkeletonFrameList read FOwner;
    property Name: string read FName write FName;
    { : Position values for the joints. }
    property Position: TAffineVectorList read FPosition write SetPosition;
    { : Rotation values for the joints. }
    property Rotation: TAffineVectorList read FRotation write SetRotation;
    { : Quaternions are an alternative to Euler rotations to build the
      global matrices for the skeleton bones. }
    property Quaternion: TQuaternionList read FQuaternion write SetQuaternion;
    { : TransformMode indicates whether to use Rotation or Quaternion to build
      the local transform matrices. }
    property TransformMode: TSkeletonFrameTransform read FTransformMode
      write FTransformMode;

    { : Calculate or retrieves an array of local bone matrices.<p>
      This array is calculated on the first call after creation, and the
      first call following a FlushLocalMatrixList. Subsequent calls return
      the same arrays. }
    function LocalMatrixList: PMatrixArray;
    { : Flushes (frees) then LocalMatrixList data.<p>
      Call this function to allow a recalculation of local matrices. }
    procedure FlushLocalMatrixList;
    // : As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: Boolean = True);
  end;

  // TSkeletonFrameList
  //
  { : A list of TSkeletonFrame objects. }
  TSkeletonFrameList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GetSkeletonFrame(Index: Integer): TSkeletonFrame;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    // : As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True;
      SetTransformMode: Boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: Boolean = True;
      SetTransformMode: Boolean = True);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TSkeletonFrame
      read GetSkeletonFrame; default;
  end;

  TSkeleton = class;
  TSkeletonBone = class;

  // TSkeletonBoneList
  //
  { : A list of skeleton bones.<p> }
  TSkeletonBoneList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FSkeleton: TSkeleton; // not persistent

  protected
    { Protected Declarations }
    FGlobalMatrix: TMatrix;

    function GetSkeletonBone(Index: Integer): TSkeletonBone;
    procedure AfterObjectCreatedByReader(Sender: TObject); override;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TSkeleton);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Skeleton: TSkeleton read FSkeleton;
    property Items[Index: Integer]: TSkeletonBone read GetSkeletonBone; default;

    { : Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: Integer): TSkeletonBone; virtual;
    { : Returns a bone by its Name, nil if not found. }
    function BoneByName(const aName: string): TSkeletonBone; virtual;
    { : Number of bones (including all children and self). }

    function BoneCount: Integer;

    // : Render skeleton wireframe
    procedure BuildList(var mrci: TRenderContextInfo); virtual; abstract;
    procedure PrepareGlobalMatrices; virtual;
  end;

  // TSkeletonRootBoneList
  //
  { : This list store skeleton root bones exclusively.<p> }
  TSkeletonRootBoneList = class(TSkeletonBoneList)
  private
    { Private Declarations }

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    // : Render skeleton wireframe
    procedure BuildList(var mrci: TRenderContextInfo); override;

    property GlobalMatrix: TMatrix read FGlobalMatrix write FGlobalMatrix;
  end;

  // TSkeletonBone
  //
  { : A skeleton bone or node and its children.<p>
    This class is the base item of the bones hierarchy in a skeletal model.
    The joint values are stored in a TSkeletonFrame, but the calculated bone
    matrices are stored here. }
  TSkeletonBone = class(TSkeletonBoneList)
  private
    { Private Declarations }
    FOwner: TSkeletonBoneList; // indirectly persistent
    FBoneID: Integer;
    FName: string;
    FColor: Cardinal;

  protected
    { Protected Declarations }
    function GetSkeletonBone(Index: Integer): TSkeletonBone;
    procedure SetColor(const val: Cardinal);

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TSkeletonBoneList);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    // : Render skeleton wireframe
    procedure BuildList(var mrci: TRenderContextInfo); override;

    property Owner: TSkeletonBoneList read FOwner;
    property Name: string read FName write FName;
    property BoneID: Integer read FBoneID write FBoneID;
    property Color: Cardinal read FColor write SetColor;
    property Items[Index: Integer]: TSkeletonBone read GetSkeletonBone; default;

    { : Returns a bone by its BoneID, nil if not found. }
    function BoneByID(anID: Integer): TSkeletonBone; override;
    function BoneByName(const aName: string): TSkeletonBone; override;

    { : Set the bone's matrix. Becareful using this. }
    procedure SetGlobalMatrix(Matrix: TMatrix); // Ragdoll
    { : Set the bone's GlobalMatrix. Used for Ragdoll. }
    procedure SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix); // Ragdoll

    { : Calculates the global matrix for the bone and its sub-bone.<p>
      Call this function directly only the RootBone. }
    procedure PrepareGlobalMatrices; override;
    { : Global Matrix for the bone in the current frame.<p>
      Global matrices must be prepared by invoking PrepareGlobalMatrices
      on the root bone. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;

    { : Free all sub bones and reset BoneID and Name. }
    procedure Clean; override;
  end;

  TSkeletonColliderList = class;

  // TSkeletonCollider
  //
  { : A general class storing the base level info required for skeleton
    based collision methods. This class is meant to be inherited from
    to create skeleton driven Verlet Constraints, ODE Geoms, etc.
    Overriden classes should be named as TSCxxxxx. }
  TSkeletonCollider = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TSkeletonColliderList;
    FBone: TSkeletonBone;
    FBoneID: Integer;
    FLocalMatrix, FGlobalMatrix: TMatrix;
    FAutoUpdate: Boolean;

  protected
    { Protected Declarations }
    procedure SetBone(const val: TSkeletonBone);
    procedure SetLocalMatrix(const val: TMatrix);

  public
    { Public Declarations }
    constructor Create; override;
    constructor CreateOwned(aOwner: TSkeletonColliderList);
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    { : This method is used to align the colliders and their
      derived objects to their associated skeleton bone.
      Override to set up descendant class alignment properties. }
    procedure AlignCollider; virtual;

    property Owner: TSkeletonColliderList read FOwner;
    // : The bone that this collider associates with.
    property Bone: TSkeletonBone read FBone write SetBone;
    { : Offset and orientation of the collider in the associated
      bone's space. }
    property LocalMatrix: TMatrix read FLocalMatrix write SetLocalMatrix;
    { : Global offset and orientation of the collider. This
      gets set in the AlignCollider method. }
    property GlobalMatrix: TMatrix read FGlobalMatrix;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
  end;

  // TSkeletonColliderList
  //
  { : List class for storing TSkeletonCollider objects. }
  TSkeletonColliderList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GetSkeletonCollider(index: Integer): TSkeletonCollider;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure Clear; override;
    { : Calls AlignCollider for each collider in the list. }
    procedure AlignColliders;

    property Owner: TPersistent read FOwner;
    property Items[Index: Integer]: TSkeletonCollider
      read GetSkeletonCollider; default;
  end;

  TGLBaseMesh = class;

  // TBlendedLerpInfo
  //
  { : Small structure to store a weighted lerp for use in blending. }
  TBlendedLerpInfo = record
    frameIndex1, frameIndex2: Integer;
    lerpFactor: Single;
    weight: Single;
    externalPositions: TAffineVectorList;
    externalRotations: TAffineVectorList;
    externalQuaternions: TQuaternionList;
  end;

  // TSkeleton
  //
  { : Main skeleton object.<p>
    This class stores the bones hierarchy and animation frames.<br>
    It is also responsible for maintaining the "CurrentFrame" and allowing
    various frame blending operations. }
  TSkeleton = class(TPersistentObject)
  private
    { Private Declarations }
    FOwner: TGLBaseMesh;
    FRootBones: TSkeletonRootBoneList;
    FFrames: TSkeletonFrameList;
    FCurrentFrame: TSkeletonFrame; // not persistent
    FBonesByIDCache: TList;
    FColliders: TSkeletonColliderList;
    FRagDollEnabled: Boolean; // ragdoll
    FMorphInvisibleParts: Boolean;

  protected
    { Protected Declarations }
    procedure SetRootBones(const val: TSkeletonRootBoneList);
    procedure SetFrames(const val: TSkeletonFrameList);
    function GetCurrentFrame: TSkeletonFrame;
    procedure SetCurrentFrame(val: TSkeletonFrame);
    procedure SetColliders(const val: TSkeletonColliderList);

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TGLBaseMesh);
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TGLBaseMesh read FOwner;
    property RootBones: TSkeletonRootBoneList read FRootBones
      write SetRootBones;
    property Frames: TSkeletonFrameList read FFrames write SetFrames;
    property CurrentFrame: TSkeletonFrame read GetCurrentFrame
      write SetCurrentFrame;
    property Colliders: TSkeletonColliderList read FColliders
      write SetColliders;

    procedure FlushBoneByIDCache;
    function BoneByID(anID: Integer): TSkeletonBone;
    function BoneByName(const aName: string): TSkeletonBone;
    function BoneCount: Integer;

    procedure MorphTo(frameIndex: Integer); overload;
    procedure MorphTo(frame: TSkeletonFrame); overload;
    procedure Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
    procedure BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);

    { : Linearly removes the translation component between skeletal frames.<p>
      This function will compute the translation of the first bone (index 0)
      and linearly subtract this translation in all frames between startFrame
      and endFrame. Its purpose is essentially to remove the 'slide' that
      exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic(startFrame, endFrame: Integer);
    { : Removes the absolute rotation component of the skeletal frames.<p>
      Some formats will store frames with absolute rotation information,
      if this correct if the animation is the "main" animation.<br>
      This function removes that absolute information, making the animation
      frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta(startFrame, endFrame: Integer);

    { : Applies current frame to morph all mesh objects. }
    procedure MorphMesh(normalize: Boolean);

    { : Copy bone rotations from reference skeleton. }
    procedure Synchronize(reference: TSkeleton);
    { : Release bones and frames info. }
    procedure Clear;
    { : Backup and prepare the BoneMatrixInvertedMeshes to use with ragdolls }
    procedure StartRagdoll; // ragdoll
    { : Restore the BoneMatrixInvertedMeshes to stop the ragdoll }
    procedure StopRagdoll; // ragdoll

    { : Turning this option off (by default) alows to increase FPS,
      but may break backwards-compatibility, because some may choose to
      attach other objects to invisible parts. }
    property MorphInvisibleParts: Boolean read FMorphInvisibleParts
      write FMorphInvisibleParts;
  end;

  // TMeshObjectRenderingOption
  //
  { : Rendering options per TMeshObject.<p>
    <ul>
    <li>moroGroupByMaterial : if set, the facegroups will be rendered by material
    in batchs, this will optimize rendering by reducing material switches, but
    also implies that facegroups will not be rendered in the order they are in
    the list.
    </ul> }
  TMeshObjectRenderingOption = (moroGroupByMaterial);
  TMeshObjectRenderingOptions = set of TMeshObjectRenderingOption;

  TVBOBuffer = (vbVertices, vbNormals, vbColors, vbTexCoords,
    vbLightMapTexCoords, vbTexCoordsEx);
  TVBOBuffers = set of TVBOBuffer;

  // TMeshObject
  //
  { : Base mesh class.<p>
    Introduces base methods and properties for mesh objects.<p>
    Subclasses are named "TMOxxx". }
  TMeshObject = class(TBaseMeshObject)
  private
    { Private Declarations }
    FOwner: TMeshObjectList;
    FBatch: TDrawBatch;
    FTexCoords: TAffineVectorList; // provision for 3D textures
    FLightMapTexCoords: TAffineVectorList; // reserved for 2D surface needs
    FColors: TVectorList;
    FFaceGroups: TFaceGroups;
    FMode: TMeshObjectMode;
    FRenderingOptions: TMeshObjectRenderingOptions;
    FLightMapArrayEnabled: Boolean; // not persistent
    FLastLightMapIndex: Integer; // not persistent
    FTexCoordsEx: TList;
  protected
    { Protected Declarations }
    procedure SetTexCoords(const val: TAffineVectorList);
    procedure SetLightmapTexCoords(const val: TAffineVectorList);
    procedure SetColors(const val: TVectorList);

    procedure SetTexCoordsEx(index: Integer; const val: TVectorList);
    function GetTexCoordsEx(index: Integer): TVectorList;

    procedure ContributeToBarycenter(var currentSum: TAffineVector;
      var nb: Integer); override;
  public
    { Public Declarations }
    { : Creates, assigns Owner and adds to list. }
    constructor CreateOwned(aOwner: TMeshObjectList);
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Clear; override;

    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList; override;
    { : Returns number of triangles in the mesh object. }
    function TriangleCount: Integer; dynamic;

    procedure PrepareMaterialLibraryCache(matLib: TGLAbstractMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { : Prepare the texture and materials before rendering.<p>
      Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TRenderContextInfo); virtual;
    // : Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TRenderContextInfo); virtual;
    procedure BuildMeshes; virtual;

    // : The extents of the object (min and max coordinates)
    procedure GetExtents(out min, max: TAffineVector); overload; virtual;
    procedure GetExtents(out aabb: TAABB); overload; virtual;

    // : Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function PointInObject(const aPoint: TAffineVector): Boolean; virtual;

    // : Returns the triangle data for a given triangle
    procedure GetTriangleData(tri: Integer; list: TAffineVectorList;
      var v0, v1, v2: TAffineVector); overload;
    procedure GetTriangleData(tri: Integer; list: TVectorList;
      var v0, v1, v2: TVector); overload;

    // : Sets the triangle data of a given triangle
    procedure SetTriangleData(tri: Integer; list: TAffineVectorList;
      const v0, v1, v2: TAffineVector); overload;
    procedure SetTriangleData(tri: Integer; list: TVectorList;
      const v0, v1, v2: TVector); overload;

    property Owner: TMeshObjectList read FOwner;
    property mode: TMeshObjectMode read FMode write FMode;
    property texCoords: TAffineVectorList read FTexCoords write SetTexCoords;
    property LightMapTexCoords: TAffineVectorList read FLightMapTexCoords
      write SetLightmapTexCoords;
    property Colors: TVectorList read FColors write SetColors;
    property FaceGroups: TFaceGroups read FFaceGroups;
    property RenderingOptions: TMeshObjectRenderingOptions
      read FRenderingOptions write FRenderingOptions;

    { : The TexCoords Extension is a list of vector lists that are used
      to extend the vertex data applied during rendering.<p>

      The lists are applied to the GL_TEXTURE0_ARB + index texture
      environment. This means that if TexCoordsEx 0 or 1 have data it
      will override the TexCoords or LightMapTexCoords repectively.
      Lists are created on demand, meaning that if you request
      TexCoordsEx[4] it will create the list up to and including 4.
      The extensions are only applied to the texture environment if
      they contain data. }
    property TexCoordsEx[index: Integer]: TVectorList read GetTexCoordsEx
      write SetTexCoordsEx;
  end;

  // TMeshObjectList
  //
  { : A list of TMeshObject objects. }
  TMeshObjectList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TGLBaseMesh;
  protected
    { Protected Declarations }
    function GetMeshObject(Index: Integer): TMeshObject;
  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TGLBaseMesh);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLAbstractMaterialLibrary);
    procedure DropMaterialLibraryCache;

    { : Prepare the texture and materials before rendering.<p>
      Invoked once, before building the list and NOT while building the list. }
    procedure PrepareBuildList(var mrci: TRenderContextInfo); virtual;
    // : Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TRenderContextInfo); virtual;
    procedure BuildMeshes;

    procedure MorphTo(morphTargetIndex: Integer);
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer;
      lerpFactor: Single);
    function MorphTargetCount: Integer;

    procedure GetAABB(var anAABB: TAABB);
    procedure GetExtents(out min, max: TAffineVector);
    procedure Translate(const delta: TAffineVector);
    function ExtractTriangles(texCoords: TAffineVectorList = nil;
      normals: TAffineVectorList = nil): TAffineVectorList;
    { : Returns number of triangles in the meshes of the list. }
    function TriangleCount: Integer;

    // : Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    function FindMeshByName(MeshName: string): TMeshObject;

    property Owner: TGLBaseMesh read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TMeshObject read GetMeshObject; default;
  end;

  TMeshObjectListClass = class of TMeshObjectList;

  TMeshMorphTargetList = class;

  // TMeshMorphTarget
  //
  { : A morph target, stores alternate lists of vertices and normals. }
  TMeshMorphTarget = class(TBaseMeshObject)
  private
    { Private Declarations }
    FOwner: TMeshMorphTargetList;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TMeshMorphTargetList);
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    property Owner: TMeshMorphTargetList read FOwner;
  end;

  // TMeshMorphTargetList
  //
  { : A list of TMeshMorphTarget objects. }
  TMeshMorphTargetList = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TPersistent;

  protected
    { Protected Declarations }
    function GetMeshMorphTarget(Index: Integer): TMeshMorphTarget;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure Translate(const delta: TAffineVector);

    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TMeshMorphTarget
      read GetMeshMorphTarget; default;
  end;

  // TMorphableMeshObject
  //
  { : Mesh object with support for morph targets.<p>
    The morph targets allow to change vertices and normals according to pre-
    existing "morph targets". }
  TMorphableMeshObject = class(TMeshObject)
  private
    { Private Declarations }
    FMorphTargets: TMeshMorphTargetList;

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

    property MorphTargets: TMeshMorphTargetList read FMorphTargets;
  end;

  // TVertexBoneWeight
  //
  TVertexBoneWeight = packed record
    BoneID: Integer;
    weight: Single;
  end;

  TVertexBoneWeightArray = array [0 .. MaxInt shr 4] of TVertexBoneWeight;
  PVertexBoneWeightArray = ^TVertexBoneWeightArray;
  TVerticesBoneWeights = array [0 .. MaxInt shr 3] of PVertexBoneWeightArray;
  PVerticesBoneWeights = ^TVerticesBoneWeights;
  TVertexBoneWeightDynArray = array of TVertexBoneWeight;

  // TSkeletonMeshObject
  //
  { : A mesh object with vertice bone attachments.<p>
    The class adds per vertex bone weights to the standard morphable mesh.<br>
    The TVertexBoneWeight structures are accessed via VerticesBonesWeights,
    they must be initialized by adjusting the BonesPerVertex and
    VerticeBoneWeightCount properties, you can also add vertex by vertex
    by using the AddWeightedBone method.<p>
    When BonesPerVertex is 1, the weight is ignored (set to 1.0). }
  TSkeletonMeshObject = class(TMorphableMeshObject)
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

    property VerticesBonesWeights: PVerticesBoneWeights
      read FVerticesBonesWeights;
    property VerticeBoneWeightCount: Integer read FVerticeBoneWeightCount
      write SetVerticeBoneWeightCount;
    property VerticeBoneWeightCapacity: Integer read FVerticeBoneWeightCapacity
      write SetVerticeBoneWeightCapacity;
    property BonesPerVertex: Integer read FBonesPerVertex
      write SetBonesPerVertex;

    function FindOrAdd(BoneID: Integer; const vertex, normal: TAffineVector)
      : Integer; overload;
    function FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
      const vertex, normal: TAffineVector): Integer; overload;

    procedure AddWeightedBone(aBoneID: Integer; aWeight: Single);
    procedure AddWeightedBones(const boneIDs: TVertexBoneWeightDynArray);
    procedure PrepareBoneMatrixInvertedMeshes;
    procedure ApplyCurrentSkeletonFrame(normalize: Boolean);

  end;

  // TFaceGroup
  //
  { : Describes a face group of a TMeshObject.<p>
    Face groups should be understood as "a way to use mesh data to render
    a part or the whole mesh object".<p>
    Subclasses implement the actual behaviours, and should have at least
    one "Add" method, taking in parameters all that is required to describe
    a single base facegroup element. }
  TFaceGroup = class(TPersistentObject)
  protected
    { Protected Declarations }
    FOwner: TFaceGroups;
    FBatch: TDrawBatch;
    FMaterialName: string;
    FMaterialCache: TGLLibMaterial;
    FLightMapIndex: Integer;
    FRenderGroupID: Integer;
    procedure AttachLightmap(lightMap: TGLTexture;
      var mrci: TRenderContextInfo);
    procedure AttachOrDetachLightmap(var mrci: TRenderContextInfo);
  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TFaceGroups); virtual;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLAbstractMaterialLibrary);
    procedure DropMaterialLibraryCache;

    procedure BuildList(var mrci: TRenderContextInfo); virtual; abstract;
    procedure BuildMesh; virtual; abstract;

    { : Add to the list the triangles corresponding to the facegroup.<p>
      This function is used by TMeshObjects ExtractTriangles to retrieve
      all the triangles in a mesh. }
    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); dynamic;
    { : Returns number of triangles in the facegroup. }
    function TriangleCount: Integer; dynamic; abstract;
    { : Reverses the rendering order of faces.<p>
      Default implementation does nothing }
    procedure Reverse; dynamic;

    // : Precalculate whatever is needed for rendering, called once
    procedure Prepare; dynamic;

    property Owner: TFaceGroups read FOwner write FOwner;
    property MaterialName: string read FMaterialName write FMaterialName;
    property MaterialCache: TGLLibMaterial read FMaterialCache;
    { : Index of lightmap in the lightmap library. }
    property LightMapIndex: Integer read FLightMapIndex write FLightMapIndex;
  end;

  // TFaceGroupMeshMode
  //
  { : Known descriptions for face group mesh modes.<p>
    - fgmmTriangles : issue all vertices with GL_TRIANGLES.<br>
    - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP.<br>
    - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
    the same normal for all vertices of a triangle.<br>
    - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN.<br>
    - fgmmQuads : issue all vertices with GL_QUADS. }
  TFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
    fgmmTriangleFan, fgmmQuads);

  // TFGVertexIndexList
  //
  { : A face group based on an indexlist.<p>
    The index list refers to items in the mesh object (vertices, normals, etc.),
    that are all considered in sync, the render is obtained issueing the items
    in the order given by the vertices.<p> }
  TFGVertexIndexList = class(TFaceGroup)
  private
    { Private Declarations }
    FVertexIndices: TIntegerList;
    FMode: TFaceGroupMeshMode;
  protected
    { Protected Declarations }
    procedure SetVertexIndices(const val: TIntegerList);

    procedure AddToList(Source, destination: TAffineVectorList;
      indices: TIntegerList);

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure BuildList(var mrci: TRenderContextInfo); override;
    procedure BuildMesh; override;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;
    function TriangleCount: Integer; override;
    procedure Reverse; override;

    procedure Add(idx: Integer);
    procedure GetExtents(var min, max: TAffineVector);
    { : If mode is strip or fan, convert the indices to triangle list indices. }
    procedure ConvertToList;

    // : Return the normal from the 1st three points in the facegroup
    function GetNormal: TAffineVector;

    property mode: TFaceGroupMeshMode read FMode write FMode;
    property vertexIndices: TIntegerList read FVertexIndices
      write SetVertexIndices;
  end;

  // TFGVertexNormalTexIndexList
  //
  { : Adds normals and texcoords indices.<p>
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

    procedure BuildList(var mrci: TRenderContextInfo); override;
    procedure BuildMesh; override;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;

    procedure Add(vertexIdx, normalIdx, texCoordIdx: Integer);

    property normalIndices: TIntegerList read FNormalIndices
      write SetNormalIndices;
    property TexCoordIndices: TIntegerList read FTexCoordIndices
      write SetTexCoordIndices;
  end;

  // TFGIndexTexCoordList
  //
  { : Adds per index texture coordinates to its ancestor.<p>
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

    procedure BuildList(var mrci: TRenderContextInfo); override;
    procedure BuildMesh; override;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil;
      aNormals: TAffineVectorList = nil); override;

    procedure Add(idx: Integer; const texCoord: TAffineVector); overload;
    procedure Add(idx: Integer; const s, t: Single); overload;

    property texCoords: TAffineVectorList read FTexCoords write SetTexCoords;
  end;

  // TFaceGroups
  //
  { : A list of TFaceGroup objects. }
  TFaceGroups = class(TPersistentObjectList)
  private
    { Private Declarations }
    FOwner: TMeshObject;

  protected
    { Protected Declarations }
    function GetFaceGroup(Index: Integer): TFaceGroup;

  public
    { Public Declarations }
    constructor CreateOwned(aOwner: TMeshObject);
    destructor Destroy; override;

    procedure ReadFromFiler(reader: TVirtualReader); override;

    procedure PrepareMaterialLibraryCache(matLib: TGLAbstractMaterialLibrary);
    procedure DropMaterialLibraryCache;

    property Owner: TMeshObject read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TFaceGroup read GetFaceGroup; default;

    procedure AddToTriangles(aList: TAffineVectorList;
      aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);

    { : Material Library of the owner TGLBaseMesh. }
    function MaterialLibrary: TGLAbstractMaterialLibrary;
    { : Sort faces by material.<p>
      Those without material first in list, followed by opaque materials,
      then transparent materials. }
    procedure SortByMaterial;
  end;

  // TMeshFaceWinding
  //
  { : Determines how normals orientation is defined in a mesh.<p>
    - mnoDefault : uses default orientation<br>
    - mnoInvert : inverse of default orientation<br> }
  TMeshFaceWinding = (mnoDefault, mnoInvert);
  // , mnoAutoSolid, mnoAutoHollow);

  // TVectorFile
  //
  { : Abstract base class for different vector file formats.<p>
    The actual implementation for these files (3DS, DXF..) must be done
    seperately. The concept for TVectorFile is very similar to TGraphic
    (see Delphi Help). }
  TVectorFile = class(TDataFile)
  private
    { Private Declarations }
    FFaceWinding: TMeshFaceWinding;

  protected
    { Protected Declarations }
    procedure SetFaceWinding(const val: TMeshFaceWinding); virtual;

  public
    { Public Declarations }
    constructor Create(aOwner: TPersistent); override;

    function Owner: TGLBaseMesh;

    property FaceWinding: TMeshFaceWinding read FFaceWinding
      write SetFaceWinding;
  end;

  TVectorFileClass = class of TVectorFile;

  // TGLGLSMVectorFile
  //
  { : GLSM (GLScene Mesh) vector file.<p>
    This corresponds to the 'native' GLScene format, and object persistence
    stream, which should be the 'fastest' of all formats to load, and supports
    all of GLScene features. }
  TGLGLSMVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  // TGLBaseMesh
  //
  { : Base class for mesh objects. }
  TGLBaseMesh = class(TGLSceneObject)
  private
    { Private Declarations }
    FFaceWinding: TMeshFaceWinding;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FLightmapLibrary: TGLMaterialLibrary;
    FAABBCache: TAABB;
    FUseMeshMaterials: Boolean;
    FOverlaySkeleton: Boolean;
    FIgnoreMissingTextures: Boolean;
    FAutoCentering: TMeshAutoCenterings;
    FAutoScaling: TGLCoordinates;
    FMaterialLibraryCachesPrepared: Boolean;
    FConnectivity: TObject;
    FLastLoadedFilename: string;
    FShowAABB: Boolean;
    procedure SetMeshExtras(const Value: TMeshExtras);
    function GetPickingMaterial: string;
    procedure SetPickingMaterial(const Value: string);

    procedure SetShowAABB(const Value: Boolean);
  protected
    { Protected Declarations }
    FTransformation: TTransformationRec;
    FMeshExtras: TMeshExtras;
    FMeshObjects: TMeshObjectList; // a list of mesh objects
    FSkeleton: TSkeleton; // skeleton data & frames
    procedure SetUseMeshMaterials(const val: Boolean);
    procedure SetMaterialLibrary(const val: TGLAbstractMaterialLibrary);
    procedure SetLightmapLibrary(const val: TGLMaterialLibrary);
    procedure SetFaceWinding(const val: TMeshFaceWinding);
    procedure SetOverlaySkeleton(const val: Boolean);
    procedure SetAutoScaling(const Value: TGLCoordinates);

    { : Invoked after creating a TVectorFile and before loading.<p>
      Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
      Allows to adjust/transfer subclass-specific features. }
    procedure PrepareVectorFile(aFile: TVectorFile); dynamic;

    { : Invoked after a mesh has been loaded/added.<p>
      Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
      Allows to adjust/transfer subclass-specific features. }
    procedure PrepareMesh; dynamic;

    { : Recursively propagated to mesh object and facegroups.<p>
      Notifies that they all can establish their material library caches. }
    procedure PrepareMaterialLibraryCache;
    { : Recursively propagated to mesh object and facegroups.<p>
      Notifies that they all should forget their material library caches. }
    procedure DropMaterialLibraryCache;

    { : Prepare the texture and materials before rendering.<p>
      Invoked once, before building the list and NOT while building the list,
      MaterialLibraryCache can be assumed to having been prepared if materials
      are active. Default behaviour is to prepare build lists for the
      meshobjects. }
    procedure PrepareBuildList(var mrci: TRenderContextInfo); dynamic;

    procedure BuildMeshes;
    procedure ApplyExtras;
    procedure DoShowAxes; override;

    property ShowAABB: Boolean read FShowAABB write SetShowAABB default False;
    property MeshExtras: TMeshExtras read FMeshExtras write SetMeshExtras
      default [];
  public
    { Public Declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterAbsolutePosition: TVector; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure StructureChanged; override;
    { : Notifies that geometry data changed, but no re-preparation is needed.<p>
      Using this method will usually be faster, but may result in incorrect
      rendering, reduced performance and/or invalid bounding box data
      (ie. invalid collision detection). Use with caution. }
    procedure StructureChangedNoPrepare;

    { : BEWARE! Utterly inefficient implementation! }
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;

    { : This method allows fast shadow volumes for GLActors.<p>
      If your actor/mesh doesn't change, you don't need to call this.
      It basically caches the connectivity data. }
    procedure BuildSilhouetteConnectivityData;

    property MeshObjects: TMeshObjectList read FMeshObjects;
    property Skeleton: TSkeleton read FSkeleton;

    function GetAABB: TAABB;
    { : Computes the extents of the mesh.<p> }
    procedure GetExtents(out min, max: TAffineVector);
    { : Computes the barycenter of the mesh.<p> }
    function GetBarycenter: TAffineVector;
    { : Invoked after a mesh has been loaded.<p>
      Should auto-center according to the AutoCentering property. }
    procedure PerformAutoCentering; dynamic;
    { : Invoked after a mesh has been loaded.<p>
      Should auto-scale the vertices of the meshobjects to AutoScaling the property. }
    procedure PerformAutoScaling; dynamic;
    { : Loads a vector file.<p>
      A vector files (for instance a ".3DS") stores the definition of
      a mesh as well as materials property.<p>
      Loading a file replaces the current one (if any). }
    procedure LoadFromFile(const filename: string); dynamic;
    { : Loads a vector file from a stream.<p>
      See LoadFromFile.<br>
      The filename attribute is required to identify the type data you're
      streaming (3DS, OBJ, etc.) }
    procedure LoadFromStream(const filename: string; aStream: TStream); dynamic;
    { : Saves to a vector file.<p>
      Note that only some of the vector files formats can be written to
      by GLScene. }
    procedure SaveToFile(const filename: string); dynamic;
    { : Saves to a vector file in a stream.<p>
      Note that only some of the vector files formats can be written to
      by GLScene. }
    procedure SaveToStream(const filename: string; aStream: TStream); dynamic;

    { : Loads additionnal data from a file.<p>
      Additionnal data could be more animation frames or morph target.<br>
      The VectorFile importer must be able to handle addition of data
      flawlessly. }
    procedure AddDataFromFile(const filename: string); dynamic;
    { : Loads additionnal data from stream.<p>
      See AddDataFromFile. }
    procedure AddDataFromStream(const filename: string;
      aStream: TStream); dynamic;

    { : Returns the filename of the last loaded file, or a blank string if not
      file was loaded (or if the mesh was dinamically built). This does not
      take into account the data added to the mesh (through AddDataFromFile)
      or saved files. }
    function LastLoadedFilename: string;

    { : Determines if a mesh should be centered and how.<p>
      AutoCentering is performed <b>only</b> after loading a mesh, it has
      no effect on already loaded mesh data or when adding from a file/stream.<br>
      If you want to alter mesh data, use direct manipulation methods
      (on the TMeshObjects). }
    property AutoCentering: TMeshAutoCenterings read FAutoCentering
      write FAutoCentering default [];

    { : Scales vertices to a AutoScaling.<p>
      AutoScaling is performed <b>only</b> after loading a mesh, it has
      no effect on already loaded mesh data or when adding from a file/stream.<br>
      If you want to alter mesh data, use direct manipulation methods
      (on the TMeshObjects). }
    property AutoScaling: TGLCoordinates read FAutoScaling write FAutoScaling;

    { : Material library where mesh materials will be stored/retrieved.<p>
      If this property is not defined or if UseMeshMaterials is false,
      only the FreeForm's material will be used (and the mesh's materials
      will be ignored. }
    property MaterialLibrary: TGLAbstractMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    { : Defines wether materials declared in the vector file mesh are used.<p>
      You must also define the MaterialLibrary property. }
    property UseMeshMaterials: Boolean read FUseMeshMaterials
      write SetUseMeshMaterials default True;
    { : LightMap library where lightmaps will be stored/retrieved.<p>
      If this property is not defined, lightmaps won't be used.
      Lightmaps currently *always* use the second texture unit (unit 1),
      and may interfere with multi-texture materials. }
    property LightmapLibrary: TGLMaterialLibrary read FLightmapLibrary
      write SetLightmapLibrary;
    { : If True, exceptions about missing textures will be ignored.<p>
      Implementation is up to the file loader class (ie. this property
      may be ignored by some loaders) }
    property IgnoreMissingTextures: Boolean read FIgnoreMissingTextures
      write FIgnoreMissingTextures default False;
    { : Special material which used when mesh rendered for picking.<p> }
    property CustomPickingMaterial: string read GetPickingMaterial
      write SetPickingMaterial;
    { : Face orientation for owned mesh.<p> }
    property NormalsOrientation: TMeshFaceWinding read FFaceWinding
      write SetFaceWinding default mnoDefault; // deprecated

    property FaceWinding: TMeshFaceWinding read FFaceWinding
      write SetFaceWinding default mnoDefault;

    { : Request rendering of skeleton bones over the mesh. }
    property OverlaySkeleton: Boolean read FOverlaySkeleton
      write SetOverlaySkeleton default False;

  end;

  // TGLFreeForm
  //
  { : Container objects for a vector file mesh.<p>
    FreeForms allows loading and rendering vector files (like 3DStudio
    ".3DS" file) in GLScene. Meshes can be loaded with the LoadFromFile
    method.<p>
    A FreeForm may contain more than one mesh, but they will all be handled
    as a single object in a scene. }
  TGLFreeForm = class(TGLBaseMesh)
  private
    { Private Declarations }
    FOctree: TOctree;

  protected
    { Protected Declarations }
    function GetOctree: TOctree;

  public
    { Public Declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
      const velocity, radius: Single; intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
    function OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): Boolean;
    { : Returns true if Point is inside the free form - this will only work
      properly on closed meshes. Requires that Octree has been prepared. }
    function OctreePointInMesh(const Point: TVector): Boolean;
    function OctreeAABBIntersect(const aabb: TAABB;
      objMatrix, invObjMatrix: TMatrix;
      triangles: TAffineVectorList = nil): Boolean;
    // TODO:  function OctreeSphereIntersect

    { : Octree support *experimental*.<p>
      Use only if you understand what you're doing! }
    property Octree: TOctree read GetOctree;
    procedure BuildOctree(TreeDepth: Integer = 3);

  published
    { Published Declarations }
    property AutoCentering;
    property AutoScaling;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property CustomPickingMaterial;
    property NormalsOrientation;
    property FaceWinding;
  end;

  // TGLActorOption
  //
  { : Miscellanious actor options.<p>
    <ul>
    <li>aoSkeletonNormalizeNormals : if set the normals of a skeleton-animated
    mesh will be normalized, this is not required if no normals-based texture
    coordinates generation occurs, and thus may be unset to improve performance.
    </ul> }
  TGLActorOption = (aoSkeletonNormalizeNormals);
  TGLActorOptions = set of TGLActorOption;

const
  cDefaultGLActorOptions = [aoSkeletonNormalizeNormals];

type

  TGLActor = class;

  // TActorAnimationReference
  //
  TActorAnimationReference = (aarMorph, aarSkeleton, aarNone);

  // TActorAnimation
  //
  { : An actor animation sequence.<p>
    An animation sequence is a named set of contiguous frames that can be used
    for animating an actor. The referred frames can be either morph or skeletal
    frames (choose which via the Reference property).<p>
    An animation can be directly "played" by the actor by selecting it with
    SwitchAnimation, and can also be "blended" via a TGLAnimationControler. }
  TActorAnimation = class(TCollectionItem)
  private
    { Private Declarations }
    FName: string;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FReference: TActorAnimationReference;

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    function FrameCount: Integer;
    procedure SetStartFrame(const val: Integer);
    procedure SetEndFrame(const val: Integer);
    procedure SetReference(val: TActorAnimationReference);
    procedure SetAsString(const val: string);
    function GetAsString: string;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property AsString: string read GetAsString write SetAsString;

    function OwnerActor: TGLActor;

    { : Linearly removes the translation component between skeletal frames.<p>
      This function will compute the translation of the first bone (index 0)
      and linearly subtract this translation in all frames between startFrame
      and endFrame. Its purpose is essentially to remove the 'slide' that
      exists in some animation formats (f.i. SMD). }
    procedure MakeSkeletalTranslationStatic;
    { : Removes the absolute rotation component of the skeletal frames.<p>
      Some formats will store frames with absolute rotation information,
      if this correct if the animation is the "main" animation.<br>
      This function removes that absolute information, making the animation
      frames suitable for blending purposes. }
    procedure MakeSkeletalRotationDelta;

  published
    { Published Declarations }
    property Name: string read FName write FName;
    { : Index of the initial frame of the animation. }
    property startFrame: Integer read FStartFrame write SetStartFrame;
    { : Index of the final frame of the animation. }
    property endFrame: Integer read FEndFrame write SetEndFrame;
    { : Indicates if this is a skeletal or a morph-based animation. }
    property reference: TActorAnimationReference read FReference
      write SetReference default aarMorph;
  end;

  TActorAnimationName = string;

  // TActorAnimations
  //
  { : Collection of actor animations sequences. }
  TActorAnimations = class(TCollection)
  private
    { Private Declarations }
    FOwner: TGLActor;

  protected
    { Protected Declarations }
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TActorAnimation);
    function GetItems(index: Integer): TActorAnimation;

  public
    { Public Declarations }
    constructor Create(aOwner: TGLActor);
    function Add: TActorAnimation;
    function FindItemID(ID: Integer): TActorAnimation;
    function FindName(const aName: string): TActorAnimation;
    function FindFrame(aFrame: Integer; aReference: TActorAnimationReference)
      : TActorAnimation;

    procedure SetToStrings(aStrings: TStrings);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const filename: string);
    procedure LoadFromFile(const filename: string);

    property Items[index: Integer]: TActorAnimation read GetItems
      write SetItems; default;
    function Last: TActorAnimation;
  end;

  // TGLBaseAnimationControler
  //
  { : Base class for skeletal animation control.<p> }
  TGLBaseAnimationControler = class(TComponent)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FActor: TGLActor;

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetEnabled(const val: Boolean);
    procedure SetActor(const val: TGLActor);

    procedure DoChange; virtual;
    function Apply(var lerpInfo: TBlendedLerpInfo): Boolean; virtual;

  public
    { Public Declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Actor: TGLActor read FActor write SetActor;
  end;

  // TGLAnimationControler
  //
  { : Controls the blending of an additionnal skeletal animation into an actor.<p>
    The animation controler allows animating an actor with several animations
    at a time, for instance, you could use a "run" animation as base animation
    (in TGLActor), blend an animation that makes the arms move differently
    depending on what the actor is carrying, along with an animation that will
    make the head turn toward a target. }
  TGLAnimationControler = class(TGLBaseAnimationControler)
  private
    { Private Declarations }
    FAnimationName: TActorAnimationName;
    FRatio: Single;

  protected
    { Protected Declarations }
    procedure SetAnimationName(const val: TActorAnimationName);
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
  { : Actor frame-interpolation mode.<p>
    - afpNone : no interpolation, display CurrentFrame only<br>
    - afpLinear : perform linear interpolation between current and next frame }
  TActorFrameInterpolation = (afpNone, afpLinear);

  // TActorActionMode
  //
  { : Defines how an actor plays between its StartFrame and EndFrame.<p>
    <ul>
    <li>aamNone : no animation is performed
    <li>aamPlayOnce : play from current frame to EndFrame, once end frame has
    been reached, switches to aamNone
    <li>aamLoop : play from current frame to EndFrame, once end frame has
    been reached, sets CurrentFrame to StartFrame
    <li>aamBounceForward : play from current frame to EndFrame, once end frame
    has been reached, switches to aamBounceBackward
    <li>aamBounceBackward : play from current frame to StartFrame, once start
    frame has been reached, switches to aamBounceForward
    <li>aamExternal : Allows for external animation control
    </ul> }
  TActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
    aamBounceBackward, aamLoopBackward, aamExternal);

  // TGLActor
  //
  { : Mesh class specialized in animated meshes.<p>
    The TGLActor provides a quick interface to animated meshes based on morph
    or skeleton frames, it is capable of performing frame interpolation and
    animation blending (via TGLAnimationControler components). }
  TGLActor = class(TGLBaseMesh)
  private
    { Private Declarations }
    FStartFrame, FEndFrame: Integer;
    FReference: TActorAnimationReference;
    FCurrentFrame: Integer;
    FCurrentFrameDelta: Single;
    FFrameInterpolation: TActorFrameInterpolation;
    FInterval: Integer;
    FAnimationMode: TActorAnimationMode;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached, FOnStartFrameReached: TNotifyEvent;
    FAnimations: TActorAnimations;
    FTargetSmoothAnimation: TActorAnimation;
    FControlers: TList;
    FOptions: TGLActorOptions;

  protected
    { Protected Declarations }
    procedure SetCurrentFrame(val: Integer);
    procedure SetStartFrame(val: Integer);
    procedure SetEndFrame(val: Integer);
    procedure SetReference(val: TActorAnimationReference);
    procedure SetAnimations(const val: TActorAnimations);
    function StoreAnimations: Boolean;
    procedure SetOptions(const val: TGLActorOptions);

    procedure PrepareMesh; override;
    procedure PrepareBuildList(var mrci: TRenderContextInfo); override;
    procedure DoAnimate; virtual;

    procedure RegisterControler(aControler: TGLBaseAnimationControler);
    procedure UnRegisterControler(aControler: TGLBaseAnimationControler);

  public
    { Public Declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    procedure LoadFromStream(const filename: string; aStream: TStream);
      override;

    procedure SwitchToAnimation(anAnimation: TActorAnimation;
      smooth: Boolean = False); overload;
    procedure SwitchToAnimation(const AnimationName: string;
      smooth: Boolean = False); overload;
    procedure SwitchToAnimation(animationIndex: Integer;
      smooth: Boolean = False); overload;
    function CurrentAnimation: string;

    { : Synchronize self animation with an other actor.<p>
      Copies Start/Current/End Frame values, CurrentFrameDelta,
      AnimationMode and FrameInterpolation. }
    procedure Synchronize(referenceActor: TGLActor);

    { : Provides a direct access to FCurrentFrame without any checks.
      Used in TGLActorProxy. }
    procedure SetCurrentFrameDirect(const Value: Integer);

    function NextFrameIndex: Integer;

    procedure NextFrame(nbSteps: Integer = 1);
    procedure PrevFrame(nbSteps: Integer = 1);

    function FrameCount: Integer;

    { : Indicates whether the actor is currently swithing animations (with
      smooth interpolation). }
    function isSwitchingAnimation: Boolean;

  published
    { Published Declarations }
    property startFrame: Integer read FStartFrame write SetStartFrame default 0;
    property endFrame: Integer read FEndFrame write SetEndFrame default 0;

    { : Reference Frame Animation mode.<p>
      Allows specifying if the model is primarily morph or skeleton based. }
    property reference: TActorAnimationReference read FReference
      write FReference default aarMorph;

    { : Current animation frame. }
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame
      default 0;
    { : Value in the [0; 1] range expressing the delta to the next frame.<p> }
    property CurrentFrameDelta: Single read FCurrentFrameDelta
      write FCurrentFrameDelta;
    { : Frame interpolation mode (afpNone/afpLinear). }
    property FrameInterpolation: TActorFrameInterpolation
      read FFrameInterpolation write FFrameInterpolation default afpLinear;

    { : See TActorAnimationMode.<p> }
    property AnimationMode: TActorAnimationMode read FAnimationMode
      write FAnimationMode default aamNone;
    { : Interval between frames, in milliseconds. }
    property Interval: Integer read FInterval write FInterval;
    { : Actor and animation miscellanious options. }
    property Options: TGLActorOptions read FOptions write SetOptions
      default cDefaultGLActorOptions;

    { : Triggered after each CurrentFrame change. }
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged
      write FOnFrameChanged;
    { : Triggered after EndFrame has been reached by progression or "nextframe" }
    property OnEndFrameReached: TNotifyEvent read FOnEndFrameReached
      write FOnEndFrameReached;
    { : Triggered after StartFrame has been reached by progression or "nextframe" }
    property OnStartFrameReached: TNotifyEvent read FOnStartFrameReached
      write FOnStartFrameReached;

    { : Collection of animations sequences. }
    property Animations: TActorAnimations read FAnimations write SetAnimations
      stored StoreAnimations;

    property AutoCentering;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
    property OverlaySkeleton;
  end;

  // TVectorFileFormat
  //
  TVectorFileFormat = class
  public
    VectorFileClass: TVectorFileClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // TVectorFileFormatsList
  //
  { : Stores registered vector file formats. }
  TVectorFileFormatsList = class(TPersistentObjectList)
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: Integer;
      AClass: TVectorFileClass);
    function FindExt(Ext: string): TVectorFileClass;
    function FindFromFileName(const filename: string): TVectorFileClass;
    procedure Remove(AClass: TVectorFileClass);
    procedure BuildFilterStrings(VectorFileClass: TVectorFileClass;
      out descriptions, filters: string; formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(index: Integer;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidVectorFile = class(Exception);

  // : Read access to the list of registered vector file formats
function GetVectorFileFormats: TVectorFileFormatsList;
// : A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter: string;
// : A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter: string;
{ : Returns an extension by its index in the vector files dialogs filter.<p>
  Use VectorFileFormatsFilter to obtain the filter. }
function VectorFileFormatExtensionByIndex(index: Integer): string;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string;
  AClass: TVectorFileClass);
procedure UnregisterVectorFileClass(AClass: TVectorFileClass);

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
  GLScene_Base_Strings,
  GLScene_Platform,
  GLScene_Mesh_Utils,
  GLScene_Base_GLStateMachine,
  GLScene_Shader_Parameter,
  GLScene_Utils,
  GLScene_Mesh_Silhouette
{$IFDEF GLS_DELPHI},
  GLScene_Base_Vector_Types
{$ENDIF};

var
  vVectorFileFormats: TVectorFileFormatsList;
  vNextRenderGroupID: Integer = 1;

const
  cAAFHeader: AnsiString = 'AAF';

  // GetVectorFileFormats
  //

function GetVectorFileFormats: TVectorFileFormatsList;
begin
  if not Assigned(vVectorFileFormats) then
    vVectorFileFormats := TVectorFileFormatsList.Create;
  Result := vVectorFileFormats;
end;

// VectorFileFormatsFilter
//

function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TVectorFile, Result, f);
end;

// VectorFileFormatsSaveFilter
//

function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TVectorFile, Result, f, False, True);
end;

// RegisterVectorFileFormat
//

procedure RegisterVectorFileFormat(const aExtension, aDescription: string;
  AClass: TVectorFileClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(aExtension, aDescription, 0, AClass);
end;

// UnregisterVectorFileClass
//

procedure UnregisterVectorFileClass(AClass: TVectorFileClass);
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

// TVectorFileFormatsList.Destroy
//

destructor TVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

// Add
//

procedure TVectorFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TVectorFileClass);
var
  newRec: TVectorFileFormat;
begin
  newRec := TVectorFileFormat.Create;
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

function TVectorFileFormatsList.FindExt(Ext: string): TVectorFileClass;
var
  i: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with TVectorFileFormat(Items[i]) do
    begin
      if Extension = Ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

// FindFromFileName
//

function TVectorFileFormatsList.FindFromFileName(const filename: string)
  : TVectorFileClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(filename);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(glsUnknownExtension,
      [Ext, 'GLFile' + UpperCase(Ext)]);
end;

// Remove
//

procedure TVectorFileFormatsList.Remove(AClass: TVectorFileClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

// BuildFilterStrings
//

procedure TVectorFileFormatsList.BuildFilterStrings(VectorFileClass
  : TVectorFileClass; out descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(VectorFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and
      (dfcRead in p.VectorFileClass.Capabilities)) or
      (formatsThatCanBeSaved and
      (dfcWrite in p.VectorFileClass.Capabilities))) then
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
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description,
          Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s', [glsAllFilter, filters,
      descriptions]);
end;

// FindExtByIndex
//

function TVectorFileFormatsList.FindExtByIndex(index: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TVectorFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities)
        ) or (formatsThatCanBeSaved and
        (dfcWrite in p.VectorFileClass.Capabilities)) then
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

constructor TBaseMeshObject.Create;
begin
  FVertices := TAffineVectorList.Create;
  FNormals := TAffineVectorList.Create;
  FVisible := True;
  inherited Create;
end;

// Destroy
//

destructor TBaseMeshObject.Destroy;
begin
  FNormals.Free;
  FVertices.Free;
  inherited;
end;

// Assign
//

procedure TBaseMeshObject.Assign(Source: TPersistent);
begin
  if Source is TBaseMeshObject then
  begin
    FName := TBaseMeshObject(Source).Name;
    FVertices.Assign(TBaseMeshObject(Source).FVertices);
    FNormals.Assign(TBaseMeshObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

// WriteToFiler
//

procedure TBaseMeshObject.WriteToFiler(writer: TVirtualWriter);
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

procedure TBaseMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
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

procedure TBaseMeshObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
end;

// ContributeToBarycenter
//

procedure TBaseMeshObject.ContributeToBarycenter(var currentSum: TAffineVector;
  var nb: Integer);
begin
  AddVector(currentSum, FVertices.Sum);
  nb := nb + FVertices.Count;
end;

// Translate
//

procedure TBaseMeshObject.Translate(const delta: TAffineVector);
begin
  FVertices.Translate(delta);
end;

// BuildNormals
//

procedure TBaseMeshObject.BuildNormals(vertexIndices: TIntegerList;
  mode: TMeshObjectMode; normalIndices: TIntegerList = nil);
var
  i, Base: Integer;
  n: TAffineVector;
  newNormals: TIntegerList;

  function TranslateNewNormal(vertexIndex: Integer;
    const delta: TAffineVector): Integer;
  var
    pv: PAffineVector;
  begin
    Result := newNormals[vertexIndex];
    if Result < Base then
    begin
      Result := normals.Add(NullVector);
      newNormals[vertexIndex] := Result;
    end;
    pv := @normals.list[Result];
    AddVector(pv^, delta);
  end;

begin
  if not Assigned(normalIndices) then
  begin
    // build bijection
    normals.Clear;
    normals.Count := Vertices.Count;
    case mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with normals do
            begin
              with Vertices do
              begin
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n);
              end;
              with normals do
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
            with normals do
            begin
              with Vertices do
              begin
                if (i and 1) = 0 then
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n)
                else
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 2]],
                    Items[vertexIndices[i + 1]], n);
              end;
              with normals do
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
    normals.normalize;
  end
  else
  begin
    // add new normals
    Base := normals.Count;
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
              CalcPlaneNormal(Items[vertexIndices[i + 0]],
                Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n);
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
                  Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n)
              else
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 2]], Items[vertexIndices[i + 1]], n);
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
    for i := Base to normals.Count - 1 do
      NormalizeVector(normals.list^[i]);
    newNormals.Free;
  end;
end;

// ExtractTriangles
//

function TBaseMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  Result := TAffineVectorList.Create;
  if (Vertices.Count mod 3) = 0 then
  begin
    Result.Assign(Vertices);
    if Assigned(normals) then
      normals.Assign(Self.normals);
  end;
end;

// SetVertices
//

procedure TBaseMeshObject.SetVertices(const val: TAffineVectorList);
begin
  FVertices.Assign(val);
end;

// SetNormals
//

procedure TBaseMeshObject.SetNormals(const val: TAffineVectorList);
begin
  FNormals.Assign(val);
end;

// ------------------
// ------------------ TSkeletonFrame ------------------
// ------------------

// CreateOwned
//

constructor TSkeletonFrame.CreateOwned(aOwner: TSkeletonFrameList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  Create;
end;

// Create
//

constructor TSkeletonFrame.Create;
begin
  inherited Create;
  FPosition := TAffineVectorList.Create;
  FRotation := TAffineVectorList.Create;
  FQuaternion := TQuaternionList.Create;
  FTransformMode := sftRotation;
end;

// Destroy
//

destructor TSkeletonFrame.Destroy;
begin
  FlushLocalMatrixList;
  FRotation.Free;
  FPosition.Free;
  FQuaternion.Free;
  inherited Destroy;
end;

// WriteToFiler
//

procedure TSkeletonFrame.WriteToFiler(writer: TVirtualWriter);
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

procedure TSkeletonFrame.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
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
        FTransformMode := TSkeletonFrameTransform(ReadInteger);
      end;
    end
  else
    RaiseFilerException(archiveVersion);
  FlushLocalMatrixList;
end;

// SetPosition
//

procedure TSkeletonFrame.SetPosition(const val: TAffineVectorList);
begin
  FPosition.Assign(val);
end;

// SetRotation
//

procedure TSkeletonFrame.SetRotation(const val: TAffineVectorList);
begin
  FRotation.Assign(val);
end;

// SetQuaternion
//

procedure TSkeletonFrame.SetQuaternion(const val: TQuaternionList);
begin
  FQuaternion.Assign(val);
end;

// LocalMatrixList
//

function TSkeletonFrame.LocalMatrixList: PMatrixArray;
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
            if Rotation[i][0] <> 0 then
            begin
              SinCos(Rotation[i][0], s, c);
              mat := CreateRotationMatrixX(s, c);
            end
            else
              mat := IdentityHmgMatrix;
            if Rotation[i][1] <> 0 then
            begin
              SinCos(Rotation[i][1], s, c);
              rmat := CreateRotationMatrixY(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            if Rotation[i][2] <> 0 then
            begin
              SinCos(Rotation[i][2], s, c);
              rmat := CreateRotationMatrixZ(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            mat[3][0] := Position[i][0];
            mat[3][1] := Position[i][1];
            mat[3][2] := Position[i][2];
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
            mat[3][0] := Position[i][0];
            mat[3][1] := Position[i][1];
            mat[3][2] := Position[i][2];
            mat[3][3] := 1;
            FLocalMatrixList^[i] := mat;
          end;
        end;
    end;
  end;
  Result := FLocalMatrixList;
end;

// FlushLocalMatrixList
//

procedure TSkeletonFrame.FlushLocalMatrixList;
begin
  if Assigned(FLocalMatrixList) then
  begin
    FreeMem(FLocalMatrixList);
    FLocalMatrixList := nil;
  end;
end;

// ConvertQuaternionsToRotations
//

procedure TSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions
  : Boolean = True);
var
  i: Integer;
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

procedure TSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations
  : Boolean = True);
var
  i: Integer;
  mat, rmat: TMatrix;
  s, c: Single;
begin
  Quaternion.Clear;
  for i := 0 to Rotation.Count - 1 do
  begin
    mat := IdentityHmgMatrix;
    SinCos(Rotation[i][0], s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCos(Rotation[i][1], s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCos(Rotation[i][2], s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat := MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TSkeletonFrameList ------------------
// ------------------

// CreateOwned
//

constructor TSkeletonFrameList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := aOwner;
  Create;
end;

// Destroy
//

destructor TSkeletonFrameList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TSkeletonFrameList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Clear
//

procedure TSkeletonFrameList.Clear;
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

function TSkeletonFrameList.GetSkeletonFrame(Index: Integer): TSkeletonFrame;
begin
  Result := TSkeletonFrame(list^[Index]);
end;

// ConvertQuaternionsToRotations
//

procedure TSkeletonFrameList.ConvertQuaternionsToRotations(KeepQuaternions
  : Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
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

procedure TSkeletonFrameList.ConvertRotationsToQuaternions
  (KeepRotations: Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode := sftQuaternion;
  end;
end;

// ------------------
// ------------------ TSkeletonBoneList ------------------
// ------------------

// CreateOwned
//

constructor TSkeletonBoneList.CreateOwned(aOwner: TSkeleton);
begin
  FSkeleton := aOwner;
  Create;
end;

// Create
//

constructor TSkeletonBoneList.Create;
begin
  inherited;
  FGlobalMatrix := IdentityHmgMatrix;
end;

// Destroy
//

destructor TSkeletonBoneList.Destroy;
begin
  Clean;
  inherited;
end;

// WriteToFiler
//

procedure TSkeletonBoneList.WriteToFiler(writer: TVirtualWriter);
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

procedure TSkeletonBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: Integer;
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

procedure TSkeletonBoneList.AfterObjectCreatedByReader(Sender: TObject);
begin
  with (Sender as TSkeletonBone) do
  begin
    FOwner := Self;
    FSkeleton := Self.Skeleton;
  end;
end;

// GetSkeletonBone
//

function TSkeletonBoneList.GetSkeletonBone(Index: Integer): TSkeletonBone;
begin
  Result := TSkeletonBone(list^[Index]);
end;

// BoneByID
//

function TSkeletonBoneList.BoneByID(anID: Integer): TSkeletonBone;
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

function TSkeletonBoneList.BoneByName(const aName: string): TSkeletonBone;
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

function TSkeletonBoneList.BoneCount: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i].BoneCount);
end;

// PrepareGlobalMatrices
//

procedure TSkeletonBoneList.PrepareGlobalMatrices;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TSkeletonRootBoneList ------------------
// ------------------

// WriteToFiler
//

procedure TSkeletonRootBoneList.WriteToFiler(writer: TVirtualWriter);
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

procedure TSkeletonRootBoneList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: Integer;
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

procedure TSkeletonRootBoneList.BuildList(var mrci: TRenderContextInfo);
var
  i: Integer;
begin
  // root node setups and restore OpenGL stuff
  mrci.GLStates.Disable(stColorMaterial);
  mrci.GLStates.Disable(stLighting);
  GL.Color3f(1, 1, 1);
  // render root-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TSkeletonBone ------------------
// ------------------

// CreateOwned
//

constructor TSkeletonBone.CreateOwned(aOwner: TSkeletonBoneList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  FSkeleton := aOwner.Skeleton;
  Create;
end;

// Create
//

constructor TSkeletonBone.Create;
begin
  FColor := $FFFFFFFF; // opaque white
  inherited;
end;

// Destroy
//

destructor TSkeletonBone.Destroy;
begin
  if Assigned(Owner) then
    Owner.Remove(Self);
  inherited Destroy;
end;

// WriteToFiler
//

procedure TSkeletonBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
    WriteInteger(FBoneID);
    WriteInteger(FColor);
  end;
end;

// ReadFromFiler
//

procedure TSkeletonBone.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FName := ReadString;
      FBoneID := ReadInteger;
      FColor := ReadInteger;
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// BuildList
//

procedure TSkeletonBone.BuildList(var mrci: TRenderContextInfo);

  procedure IssueColor(Color: Cardinal);
  begin
    GL.Color4f(GetRValue(Color) / 255, GetGValue(Color) / 255,
      GetBValue(Color) / 255, ((Color shr 24) and 255) / 255);
  end;

var
  i: Integer;
begin
  // point for self
  mrci.GLStates.PointSize := 5;
  GL.Begin_(GL_POINTS);
  IssueColor(Color);
  GL.Vertex3fv(@GlobalMatrix[3][0]);
  GL.End_;
  // parent-self bone line
  if Owner is TSkeletonBone then
  begin
    GL.Begin_(GL_LINES);
    GL.Vertex3fv(@TSkeletonBone(Owner).GlobalMatrix[3][0]);
    GL.Vertex3fv(@GlobalMatrix[3][0]);
    GL.End_;
  end;
  // render sub-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// GetSkeletonBone
//

function TSkeletonBone.GetSkeletonBone(Index: Integer): TSkeletonBone;
begin
  Result := TSkeletonBone(list^[Index]);
end;

// SetColor
//

procedure TSkeletonBone.SetColor(const val: Cardinal);
begin
  FColor := val;
end;

// BoneByID
//

function TSkeletonBone.BoneByID(anID: Integer): TSkeletonBone;
begin
  if BoneID = anID then
    Result := Self
  else
    Result := inherited BoneByID(anID);
end;

// BoneByName
//

function TSkeletonBone.BoneByName(const aName: string): TSkeletonBone;
begin
  if Name = aName then
    Result := Self
  else
    Result := inherited BoneByName(aName);
end;

// Clean
//

procedure TSkeletonBone.Clean;
begin
  BoneID := 0;
  Name := '';
  inherited;
end;

// PrepareGlobalMatrices
//

procedure TSkeletonBone.PrepareGlobalMatrices;
begin
  if (Skeleton.FRagDollEnabled) then
    Exit; // ragdoll
  FGlobalMatrix := MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList^
    [BoneID], TSkeletonBoneList(Owner).FGlobalMatrix);
  inherited;
end;

procedure TSkeletonBone.SetGlobalMatrix(Matrix: TMatrix); // ragdoll
begin
  FGlobalMatrix := Matrix;
end;

procedure TSkeletonBone.SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix);
// ragdoll
begin
  FGlobalMatrix := MatrixMultiply(RagDollMatrix,
    Skeleton.Owner.InvAbsoluteMatrix);
  inherited;
end;

// ------------------
// ------------------ TSkeletonCollider ------------------
// ------------------

// Create
//

constructor TSkeletonCollider.Create;
begin
  inherited;
  FLocalMatrix := IdentityHmgMatrix;
  FGlobalMatrix := IdentityHmgMatrix;
  FAutoUpdate := True;
end;

// CreateOwned
//

constructor TSkeletonCollider.CreateOwned(aOwner: TSkeletonColliderList);
begin
  Create;
  FOwner := aOwner;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// WriteToFiler
//

procedure TSkeletonCollider.WriteToFiler(writer: TVirtualWriter);
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

procedure TSkeletonCollider.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
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

procedure TSkeletonCollider.AlignCollider;
var
  mat: TMatrix;
begin
  if Assigned(FBone) then
  begin
    if Owner.Owner is TSkeleton then
      if TSkeleton(Owner.Owner).Owner is TGLBaseSceneObject then
        mat := MatrixMultiply(FBone.GlobalMatrix,
          TGLBaseSceneObject(TSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
      else
        mat := FBone.GlobalMatrix;
    MatrixMultiply(FLocalMatrix, mat, FGlobalMatrix);
  end
  else
    FGlobalMatrix := FLocalMatrix;
end;

// SetBone
//

procedure TSkeletonCollider.SetBone(const val: TSkeletonBone);
begin
  if val <> FBone then
    FBone := val;
end;

// SetMatrix
//

procedure TSkeletonCollider.SetLocalMatrix(const val: TMatrix);
begin
  FLocalMatrix := val;
end;

// ------------------
// ------------------ TSkeletonColliderList ------------------
// ------------------

// CreateOwned
//

constructor TSkeletonColliderList.CreateOwned(aOwner: TPersistent);
begin
  Create;
  FOwner := aOwner;
end;

// Destroy
//

destructor TSkeletonColliderList.Destroy;
begin
  Clear;
  inherited;
end;

// GetSkeletonCollider
//

function TSkeletonColliderList.GetSkeletonCollider(index: Integer)
  : TSkeletonCollider;
begin
  Result := TSkeletonCollider( inherited Get(index));
end;

// ReadFromFiler
//

procedure TSkeletonColliderList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := Self;
    if (Owner is TSkeleton) and (Items[i].FBoneID <> -1) then
      Items[i].Bone := TSkeleton(Owner).BoneByID(Items[i].FBoneID);
  end;
end;

// Clear
//

procedure TSkeletonColliderList.Clear;
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

procedure TSkeletonColliderList.AlignColliders;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AutoUpdate then
      Items[i].AlignCollider;
end;

// ------------------
// ------------------ TSkeleton ------------------
// ------------------

// CreateOwned
//

constructor TSkeleton.CreateOwned(aOwner: TGLBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

// Create
//

constructor TSkeleton.Create;
begin
  inherited Create;
  FRootBones := TSkeletonRootBoneList.CreateOwned(Self);
  FFrames := TSkeletonFrameList.CreateOwned(Self);
  FColliders := TSkeletonColliderList.CreateOwned(Self);
end;

// Destroy
//

destructor TSkeleton.Destroy;
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

procedure TSkeleton.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
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

procedure TSkeleton.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
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

procedure TSkeleton.SetRootBones(const val: TSkeletonRootBoneList);
begin
  FRootBones.Assign(val);
end;

// SetFrames
//

procedure TSkeleton.SetFrames(const val: TSkeletonFrameList);
begin
  FFrames.Assign(val);
end;

// GetCurrentFrame
//

function TSkeleton.GetCurrentFrame: TSkeletonFrame;
begin
  if not Assigned(FCurrentFrame) then
    FCurrentFrame := TSkeletonFrame(FFrames.Items[0].CreateClone);
  Result := FCurrentFrame;
end;

// SetCurrentFrame
//

procedure TSkeleton.SetCurrentFrame(val: TSkeletonFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TSkeletonFrame(val.CreateClone);
end;

// SetColliders
//

procedure TSkeleton.SetColliders(const val: TSkeletonColliderList);
begin
  FColliders.Assign(val);
end;

// FlushBoneByIDCache
//

procedure TSkeleton.FlushBoneByIDCache;
begin
  FBonesByIDCache.Free;
  FBonesByIDCache := nil;
end;

// BoneByID
//

function TSkeleton.BoneByID(anID: Integer): TSkeletonBone;

  procedure CollectBones(Bone: TSkeletonBone);
  var
    i: Integer;
  begin
    if Bone.BoneID >= FBonesByIDCache.Count then
      FBonesByIDCache.Count := Bone.BoneID + 1;
    FBonesByIDCache[Bone.BoneID] := Bone;
    for i := 0 to Bone.Count - 1 do
      CollectBones(Bone[i]);
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
  Result := TSkeletonBone(FBonesByIDCache[anID])
end;

// BoneByName
//

function TSkeleton.BoneByName(const aName: string): TSkeletonBone;
begin
  Result := RootBones.BoneByName(aName);
end;

// BoneCount
//

function TSkeleton.BoneCount: Integer;
begin
  Result := RootBones.BoneCount;
end;

// MorphTo
//

procedure TSkeleton.MorphTo(frameIndex: Integer);
begin
  CurrentFrame := Frames[frameIndex];
end;

// MorphTo
//

procedure TSkeleton.MorphTo(frame: TSkeletonFrame);
begin
  CurrentFrame := frame;
end;

// Lerp
//

procedure TSkeleton.Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TSkeletonFrame.Create;
  FCurrentFrame.TransformMode := Frames[frameIndex1].TransformMode;
  with FCurrentFrame do
  begin
    Position.Lerp(Frames[frameIndex1].Position, Frames[frameIndex2].Position,
      lerpFactor);
    case TransformMode of
      sftRotation:
        Rotation.AngleLerp(Frames[frameIndex1].Rotation,
          Frames[frameIndex2].Rotation, lerpFactor);
      sftQuaternion:
        Quaternion.Lerp(Frames[frameIndex1].Quaternion,
          Frames[frameIndex2].Quaternion, lerpFactor);
    end;
  end;
end;

// BlendedLerps
//

procedure TSkeleton.BlendedLerps(const lerpInfos: array of TBlendedLerpInfo);
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
    FCurrentFrame := TSkeletonFrame.Create;
    FCurrentFrame.TransformMode := Frames[lerpInfos[i].frameIndex1]
      .TransformMode;
    with FCurrentFrame do
    begin
      blendPositions := TAffineVectorList.Create;
      // lerp first item separately
      Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
        Frames[lerpInfos[i].frameIndex2].Position, lerpInfos[i].lerpFactor);
      if lerpInfos[i].weight <> 1 then
        Position.Scale(lerpInfos[i].weight);

      Inc(i);
      // combine the other items
      while i <= High(lerpInfos) do
      begin
        if not Assigned(lerpInfos[i].externalPositions) then
        begin
          blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
            Frames[lerpInfos[i].frameIndex2].Position, lerpInfos[i].lerpFactor);
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
                blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1]
                  .Rotation, Frames[lerpInfos[i].frameIndex2].Rotation,
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
                blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1]
                  .Quaternion, Frames[lerpInfos[i].frameIndex2].Quaternion,
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

procedure TSkeleton.MakeSkeletalTranslationStatic(startFrame,
  endFrame: Integer);
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
    Frames[i].Position[0] := VectorCombine(Frames[i].Position[0], delta, 1,
      (i - startFrame) * f);
end;

// MakeSkeletalRotationDelta
//

procedure TSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame: Integer);
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
      v := VectorSubtract(Frames[i].Rotation[j], Frames[0].Rotation[j]);
      if VectorNorm(v) < 1E-6 then
        Frames[i].Rotation[j] := NullVector
      else
        Frames[i].Rotation[j] := v;
    end;
  end;
end;

// MorphMesh
//

procedure TSkeleton.MorphMesh(normalize: Boolean);
var
  i: Integer;
  Mesh: TBaseMeshObject;
begin
  if Owner.MeshObjects.Count > 0 then
  begin
    RootBones.PrepareGlobalMatrices;
    if Colliders.Count > 0 then
      Colliders.AlignColliders;

    if FMorphInvisibleParts then
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        Mesh := Owner.MeshObjects.Items[i];
        if (Mesh is TSkeletonMeshObject) then
          TSkeletonMeshObject(Mesh).ApplyCurrentSkeletonFrame(normalize);
      end
    else
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        Mesh := Owner.MeshObjects.Items[i];
        if (Mesh is TSkeletonMeshObject) and Mesh.Visible then
          TSkeletonMeshObject(Mesh).ApplyCurrentSkeletonFrame(normalize);
      end
  end;
end;

// Synchronize
//

procedure TSkeleton.Synchronize(reference: TSkeleton);
begin
  CurrentFrame.Assign(reference.CurrentFrame);
  MorphMesh(True);
end;

// Clear
//

procedure TSkeleton.Clear;
begin
  FlushBoneByIDCache;
  RootBones.Clean;
  Frames.Clear;
  FCurrentFrame.Free;
  FCurrentFrame := nil;
  FColliders.Clear;
end;

procedure TSkeleton.StartRagdoll; // ragdoll
var
  i: Integer;
  Mesh: TBaseMeshObject;
begin
  if FRagDollEnabled then
    Exit
  else
    FRagDollEnabled := True;

  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      Mesh := Owner.MeshObjects.Items[i];
      if Mesh is TSkeletonMeshObject then
      begin
        TSkeletonMeshObject(Mesh).BackupBoneMatrixInvertedMeshes;
        TSkeletonMeshObject(Mesh).PrepareBoneMatrixInvertedMeshes;
      end;
    end;
  end;
end;

procedure TSkeleton.StopRagdoll; // ragdoll
var
  i: Integer;
  Mesh: TBaseMeshObject;
begin
  FRagDollEnabled := False;
  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      Mesh := Owner.MeshObjects.Items[i];
      if Mesh is TSkeletonMeshObject then
        TSkeletonMeshObject(Mesh).RestoreBoneMatrixInvertedMeshes;
    end;
  end;
end;

// ------------------
// ------------------ TMeshObject ------------------
// ------------------

// CreateOwned
//

constructor TMeshObject.CreateOwned(aOwner: TMeshObjectList);
begin
  FOwner := aOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// Create
//

constructor TMeshObject.Create;
begin
  FMode := momTriangles;
  FTexCoords := TAffineVectorList.Create;
  FLightMapTexCoords := TAffineVectorList.Create;
  FColors := TVectorList.Create;
  FFaceGroups := TFaceGroups.CreateOwned(Self);
  FTexCoordsEx := TList.Create;
  FBatch.Mesh := TMeshAtom.Create;
  if Assigned(FOwner) then
    FBatch.Mesh.Owner := FOwner.Owner;
  FBatch.Mesh.TagName := ClassName;
  inherited;
end;

// Destroy
//

destructor TMeshObject.Destroy;
var
  i: Integer;
begin
  FFaceGroups.Free;
  FColors.Free;
  FTexCoords.Free;
  FLightMapTexCoords.Free;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TVectorList(FTexCoordsEx[i]).Free;
  FTexCoordsEx.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  if Assigned(FBatch.Material) then
    FBatch.Material.UnRegisterUser(FOwner.Owner);
  FBatch.Mesh.Free;
  FBatch.InstancesChain.Free;
  inherited;
end;

// Assign
//

procedure TMeshObject.Assign(Source: TPersistent);
var
  i: Integer;
begin
  inherited Assign(Source);

  if Source is TMeshObject then
  begin
    FTexCoords.Assign(TMeshObject(Source).FTexCoords);
    FLightMapTexCoords.Assign(TMeshObject(Source).FLightMapTexCoords);
    FColors.Assign(TMeshObject(Source).FColors);
    FFaceGroups.Assign(TMeshObject(Source).FFaceGroups);
    FMode := TMeshObject(Source).FMode;
    FRenderingOptions := TMeshObject(Source).FRenderingOptions;

    // Clear FTexCoordsEx.
    for i := 0 to FTexCoordsEx.Count - 1 do
      TVectorList(FTexCoordsEx[i]).Free;

    FTexCoordsEx.Count := TMeshObject(Source).FTexCoordsEx.Count;

    // Fill FTexCoordsEx.
    for i := 0 to FTexCoordsEx.Count - 1 do
    begin
      FTexCoordsEx[i] := TVectorList.Create;
      TVectorList(FTexCoordsEx[i]).Assign(TMeshObject(Source).FTexCoordsEx[i]);
    end;
  end;
end;

// WriteToFiler
//

procedure TMeshObject.WriteToFiler(writer: TVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(4); // Archive Version 3
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
  end;
end;

// ReadFromFiler
//

procedure TMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  i, Count, archiveVersion: Integer;
  lOldLightMapTexCoords: TTexPointList;
  tc: TTexPoint;
  size, ro: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 3] then
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
          tc := lOldLightMapTexCoords[i];
          FLightMapTexCoords.Add(tc.s, tc.t);
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
      FMode := TMeshObjectMode(ReadInteger);
      size := ReadInteger;
      ro := 0;
      Read(ro, size);
{$IFNDEF FPC}
      FRenderingOptions := TMeshObjectRenderingOptions(Byte(ro));
{$ELSE}
      FRenderingOptions := TMeshObjectRenderingOptions(ro);
{$ENDIF}
      if archiveVersion >= 2 then
      begin
        Count := ReadInteger;
        for i := 0 to Count - 1 do
          TexCoordsEx[i].ReadFromFiler(reader);
        if archiveVersion < 4 then
        begin
          ReadInteger; // BinormalsTexCoordIndex - obsolite
          ReadInteger; // TangentsTexCoordIndex - obsolite
        end;
      end;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// Clear;
//

procedure TMeshObject.Clear;
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

function TMeshObject.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
begin
  case mode of
    momTriangles:
      begin
        Result := inherited ExtractTriangles;
        if Assigned(texCoords) then
          texCoords.Assign(Self.texCoords);
        if Assigned(normals) then
          normals.Assign(Self.normals);
      end;
    momTriangleStrip:
      begin
        Result := TAffineVectorList.Create;
        ConvertStripToList(Vertices, Result);
        if Assigned(texCoords) then
          ConvertStripToList(Self.texCoords, texCoords);
        if Assigned(normals) then
          ConvertStripToList(Self.normals, normals);
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

function TMeshObject.TriangleCount: Integer;
var
  i: Integer;
begin
  case mode of
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

procedure TMeshObject.PrepareMaterialLibraryCache
  (matLib: TGLAbstractMaterialLibrary);
begin
  FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TMeshObject.DropMaterialLibraryCache;
begin
  FaceGroups.DropMaterialLibraryCache;
end;

// GetExtents
//

procedure TMeshObject.GetExtents(out min, max: TAffineVector);
begin
  FVertices.GetExtents(min, max);
end;

procedure TMeshObject.GetExtents(out aabb: TAABB);
begin
  FVertices.GetExtents(aabb.min, aabb.max);
end;

// Prepare
//

procedure TMeshObject.Prepare;
var
  i: Integer;
begin
  for i := 0 to FaceGroups.Count - 1 do
    FaceGroups[i].Prepare;
end;

// PointInObject
//

function TMeshObject.PointInObject(const aPoint: TAffineVector): Boolean;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  Result := (aPoint[0] >= min[0]) and (aPoint[1] >= min[1]) and
    (aPoint[2] >= min[2]) and (aPoint[0] <= max[0]) and (aPoint[1] <= max[1])
    and (aPoint[2] <= max[2]);
end;

// SetTexCoords
//

procedure TMeshObject.SetTexCoords(const val: TAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

// SetLightmapTexCoords
//

procedure TMeshObject.SetLightmapTexCoords(const val: TAffineVectorList);
begin
  FLightMapTexCoords.Assign(val);
end;

// SetColors
//

procedure TMeshObject.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
end;

// SetTexCoordsEx
//

procedure TMeshObject.SetTexCoordsEx(index: Integer; const val: TVectorList);
begin
  TexCoordsEx[index].Assign(val);
end;

// GetTexCoordsEx
//

function TMeshObject.GetTexCoordsEx(index: Integer): TVectorList;
var
  i: Integer;
begin
  if index > FTexCoordsEx.Count - 1 then
    for i := FTexCoordsEx.Count - 1 to index do
      FTexCoordsEx.Add(TVectorList.Create);
  Result := TVectorList(FTexCoordsEx[index]);
end;

procedure TMeshObject.ContributeToBarycenter(var currentSum: TAffineVector;
  var nb: Integer);
var
  i: Integer;
  tempSum: TAffineVector;
  tempN: Integer;
begin
  case mode of
    momTriangles, momTriangleStrip:
      FBatch.Mesh.GetPositionSum(currentSum, nb);
    momFaceGroups:
      begin
        for i := FaceGroups.Count - 1 downto 0 do
        begin
          FaceGroups[i].FBatch.Mesh.GetPositionSum(tempSum, tempN);
          AddVector(currentSum, tempSum);
          nb := nb + tempN;
        end;
      end;
  end;
end;

// GetTriangleData
//

procedure TMeshObject.GetTriangleData(tri: Integer; list: TAffineVectorList;
  var v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case mode of
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
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.vertexIndices[3 * Count]];
                  v1 := list[fg.vertexIndices[3 * Count + 1]];
                  v2 := list[fg.vertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.vertexIndices[Count]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.vertexIndices[0]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// GetTriangleData
//

procedure TMeshObject.GetTriangleData(tri: Integer; list: TVectorList;
  var v0, v1, v2: TVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case mode of
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
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.vertexIndices[3 * Count]];
                  v1 := list[fg.vertexIndices[3 * Count + 1]];
                  v2 := list[fg.vertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.vertexIndices[Count]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.vertexIndices[0]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// SetTriangleData
//

procedure TMeshObject.SetTriangleData(tri: Integer; list: TAffineVectorList;
  const v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case mode of
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
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.vertexIndices[3 * Count]] := v0;
                  list[fg.vertexIndices[3 * Count + 1]] := v1;
                  list[fg.vertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.vertexIndices[Count]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.vertexIndices[0]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// SetTriangleData
//

procedure TMeshObject.SetTriangleData(tri: Integer; list: TVectorList;
  const v0, v1, v2: TVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case mode of
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
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.vertexIndices[3 * Count]] := v0;
                  list[fg.vertexIndices[3 * Count + 1]] := v1;
                  list[fg.vertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.vertexIndices[Count]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.vertexIndices[0]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

// PrepareMaterials
//

procedure TMeshObject.PrepareBuildList(var mrci: TRenderContextInfo);
var
  i: Integer;
begin
  if (mode = momFaceGroups) and Assigned(mrci.MaterialLibrary) then
  begin
    for i := 0 to FaceGroups.Count - 1 do
      with TFaceGroup(FaceGroups.list^[i]) do
      begin
        if MaterialCache <> nil then
          MaterialCache.PrepareBuildList;
      end;
  end;
end;

procedure TMeshObject.BuildList(var mrci: TRenderContextInfo);
var
  i, j, groupID, nbGroups: Integer;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  libMat: TGLLibMaterial;
  fg: TFaceGroup;
begin
  // Make sure no VBO is bound and states enabled
  gotColor := (Vertices.Count = Colors.Count);
  if gotColor then
  begin
    mrci.GLStates.Enable(stColorMaterial);
    GL.ColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    mrci.GLStates.SetGLMaterialColors(cmFront, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
    mrci.GLStates.SetGLMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
  end;
  case mode of
    momTriangles, momTriangleStrip:
      if Vertices.Count > 0 then
      begin
        gotNormals := (Vertices.Count = normals.Count);
        gotTexCoords := (Vertices.Count = texCoords.Count);
        SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
        for i := 0 to FTexCoordsEx.Count - 1 do
          gotTexCoordsEx[i] := (TexCoordsEx[i].Count > 0) and
            GL.ARB_multitexture;
        if mode = momTriangles then
          GL.Begin_(GL_TRIANGLES)
        else
          GL.Begin_(GL_TRIANGLE_STRIP);
        for i := 0 to Vertices.Count - 1 do
        begin
          if gotNormals then
            GL.Normal3fv(@normals.list[i]);
          if gotColor then
            GL.Color4fv(@Colors.list[i]);
          if FTexCoordsEx.Count > 0 then
          begin
            if gotTexCoordsEx[0] then
              GL.MultiTexCoord4fv(GL_TEXTURE0, @TexCoordsEx[0].list[i])
            else if gotTexCoords then
              GL.TexCoord2fv(@texCoords.list[i]);
            for j := 1 to FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[j] then
                GL.MultiTexCoord4fv(GL_TEXTURE0 + j, @TexCoordsEx[j].list[i]);
          end
          else
          begin
            if gotTexCoords then
              GL.TexCoord2fv(@texCoords.list[i]);
          end;
          GL.Vertex3fv(@Vertices.list[i]);
        end;
        GL.End_;
      end;
    momFaceGroups:
      begin
        if Assigned(mrci.MaterialLibrary) then
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
                      if (FRenderGroupID <> groupID) and
                        (FMaterialCache = libMat) then
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
end;

procedure TMeshObject.BuildMeshes;
var
  i: Integer;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
begin
  gotColor := (Vertices.Count = Colors.Count);
  case mode of
    momTriangles, momTriangleStrip:
      if Vertices.Count > 0 then
      begin
        FBatch.Material := Owner.Owner.FMaterial;
        FBatch.Changed := True;

        gotNormals := (Vertices.Count = normals.Count);
        gotTexCoords := (Vertices.Count = texCoords.Count);
        if FTexCoordsEx.Count > 0 then
        begin
          SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
          for i := 0 to FTexCoordsEx.Count - 1 do
            gotTexCoordsEx[i] := (TexCoordsEx[i].Count > 0);
          gotTexCoords := False;
        end;

        with FBatch.Mesh do
        begin
          Lock;
          try
            Clear;
            DeclareAttribute(attrPosition, GLSLType3f);
            if gotColor then
              DeclareAttribute(attrColor, GLSLType4f);
            if gotNormals then
              DeclareAttribute(attrNormal, GLSLType3f);
            if gotTexCoords then
              DeclareAttribute(attrTexCoord0, GLSLType2f)
            else
              for i := 0 to FTexCoordsEx.Count - 1 do
                if gotTexCoordsEx[i] then
                  DeclareAttribute(TAttribLocation(i + ord(attrTexCoord0)),
                    GLSLType4f);

            if mode = momTriangles then
              BeginAssembly(mpTRIANGLES)
            else
              BeginAssembly(mpTRIANGLE_STRIP);

            AttributeList(attrPosition, Vertices);
            if gotColor then
              AttributeList(attrColor, Colors);
            if gotNormals then
              AttributeList(attrNormal, normals);
            if gotTexCoords then
              AttributeList(attrTexCoord0, texCoords)
            else
              for i := 0 to FTexCoordsEx.Count - 1 do
                if gotTexCoordsEx[i] then
                  AttributeList(TAttribLocation(i + ord(attrTexCoord0)),
                    TexCoordsEx[i]);

            EndAssembly;
          finally
            UnLock;
          end;
        end;
      end;
    momFaceGroups:
      begin
        for i := FaceGroups.Count - 1 downto 0 do
          FaceGroups[i].BuildMesh;
      end;
  end;
end;

// ------------------
// ------------------ TMeshObjectList ------------------
// ------------------

// CreateOwned
//

constructor TMeshObjectList.CreateOwned(aOwner: TGLBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

// Destroy
//

destructor TMeshObjectList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TMeshObjectList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
  Mesh: TMeshObject;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Mesh := Items[i];
    Mesh.FOwner := Self;
    if Mesh is TSkeletonMeshObject then
      TSkeletonMeshObject(Mesh).PrepareBoneMatrixInvertedMeshes;
  end;
end;

// PrepareMaterialLibraryCache
//

procedure TMeshObjectList.PrepareMaterialLibraryCache
  (matLib: TGLAbstractMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TMeshObject(list^[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TMeshObjectList.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TMeshObject(list^[i]).DropMaterialLibraryCache;
end;

// PrepareBuildList
//

procedure TMeshObjectList.PrepareBuildList(var mrci: TRenderContextInfo);
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

procedure TMeshObjectList.BuildList(var mrci: TRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        BuildList(mrci);
end;

procedure TMeshObjectList.BuildMeshes;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].BuildMeshes;
end;

// MorphTo
//

procedure TMeshObjectList.MorphTo(morphTargetIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

// Lerp
//

procedure TMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2: Integer;
  lerpFactor: Single);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2,
        lerpFactor);
end;

// MorphTargetCount
//

function TMeshObjectList.MorphTargetCount: Integer;
var
  i: Integer;
begin
  Result := MaxInt;
  for i := 0 to Count - 1 do
    if Items[i] is TMorphableMeshObject then
      with TMorphableMeshObject(Items[i]) do
        if Result > MorphTargets.Count then
          Result := MorphTargets.Count;
  if Result = MaxInt then
    Result := 0;
end;

// Clear
//

procedure TMeshObjectList.Clear;
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

function TMeshObjectList.GetMeshObject(Index: Integer): TMeshObject;
begin
  Result := TMeshObject(list^[Index]);
end;

// GetAABB
//

procedure TMeshObjectList.GetAABB(var anAABB: TAABB);
var
  i, j: Integer;
  lAABB, sAABB: TAABB;
begin
  sAABB.min := NullVector;
  sAABB.max := NullVector;

  for i := Count - 1 downto 0 do
  begin
    with Items[i] do
    begin
      case mode of
        momTriangles, momTriangleStrip:
          begin
            lAABB := FBatch.Mesh.aabb;
            AddAABB(sAABB, lAABB);
          end;
        momFaceGroups:
          for j := FaceGroups.Count - 1 downto 0 do
          begin
            lAABB := FaceGroups[j].FBatch.Mesh.aabb;
            AddAABB(sAABB, lAABB);
          end;
      end; // of case
    end;
  end;
  anAABB.min := sAABB.min;
  sAABB.max := sAABB.max;
  Inc(anAABB.revision);
end;

// GetExtents
//

procedure TMeshObjectList.GetExtents(out min, max: TAffineVector);
var
  lAABB: TAABB;
begin
  GetAABB(lAABB);
  min := lAABB.min;
  max := lAABB.max;
end;

// Translate
//

procedure TMeshObjectList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetMeshObject(i).Translate(delta);
end;

// ExtractTriangles
//

function TMeshObjectList.ExtractTriangles(texCoords: TAffineVectorList = nil;
  normals: TAffineVectorList = nil): TAffineVectorList;
var
  i: Integer;
  obj: TMeshObject;
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

function TMeshObjectList.TriangleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].TriangleCount;
end;

// Prepare
//

procedure TMeshObjectList.Prepare;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Prepare;
end;

// FindMeshByName
//

function TMeshObjectList.FindMeshByName(MeshName: string): TMeshObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = MeshName then
    begin
      Result := Items[i];
      Break;
    end;
end;

// ------------------
// ------------------ TMeshMorphTarget ------------------
// ------------------

// CreateOwned
//

constructor TMeshMorphTarget.CreateOwned(aOwner: TMeshMorphTargetList);
begin
  FOwner := aOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

// Destroy
//

destructor TMeshMorphTarget.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

// WriteToFiler
//

procedure TMeshMorphTarget.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing
  end;
end;

// ReadFromFiler
//

procedure TMeshMorphTarget.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TMeshMorphTargetList ------------------
// ------------------

// CreateOwned
//

constructor TMeshMorphTargetList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := aOwner;
  Create;
end;

// Destroy
//

destructor TMeshMorphTargetList.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TMeshMorphTargetList.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Translate
//

procedure TMeshMorphTargetList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Translate(delta);
end;

// Clear
//

procedure TMeshMorphTargetList.Clear;
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

// GetMeshMorphTarget
//

function TMeshMorphTargetList.GetMeshMorphTarget(Index: Integer)
  : TMeshMorphTarget;
begin
  Result := TMeshMorphTarget(list^[Index]);
end;

// ------------------
// ------------------ TMorphableMeshObject ------------------
// ------------------

// Create
//

constructor TMorphableMeshObject.Create;
begin
  inherited;
  FMorphTargets := TMeshMorphTargetList.CreateOwned(Self);
end;

// Destroy
//

destructor TMorphableMeshObject.Destroy;
begin
  FMorphTargets.Free;
  inherited;
end;

// WriteToFiler
//

procedure TMorphableMeshObject.WriteToFiler(writer: TVirtualWriter);
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

procedure TMorphableMeshObject.ReadFromFiler(reader: TVirtualReader);
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

procedure TMorphableMeshObject.Clear;
begin
  inherited;
  FMorphTargets.Clear;
end;

// Translate
//

procedure TMorphableMeshObject.Translate(const delta: TAffineVector);
begin
  inherited;
  MorphTargets.Translate(delta);
end;

// MorphTo
//

procedure TMorphableMeshObject.MorphTo(morphTargetIndex: Integer);
begin
  if (morphTargetIndex = 0) and (MorphTargets.Count = 0) then
    Exit;
  Assert(Cardinal(morphTargetIndex) < Cardinal(MorphTargets.Count));
  with MorphTargets[morphTargetIndex] do
  begin
    if Vertices.Count > 0 then
    begin
      Self.Vertices.Assign(Vertices);
    end;
    if normals.Count > 0 then
    begin
      Self.normals.Assign(normals);
    end;
  end;
end;

// Lerp
//

procedure TMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2
  : Integer; lerpFactor: Single);
var
  mt1, mt2: TMeshMorphTarget;
begin
  Assert((Cardinal(morphTargetIndex1) < Cardinal(MorphTargets.Count)) and
    (Cardinal(morphTargetIndex2) < Cardinal(MorphTargets.Count)));
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
    end;
    if mt1.normals.Count > 0 then
    begin
      normals.Lerp(mt1.normals, mt2.normals, lerpFactor);
      normals.normalize;
    end;
  end;
end;

// ------------------
// ------------------ TSkeletonMeshObject ------------------
// ------------------

// Create
//

constructor TSkeletonMeshObject.Create;
begin
  FBoneMatrixInvertedMeshes := TList.Create;
  FBackupInvertedMeshes := TList.Create; // ragdoll
  inherited Create;
end;

// Destroy
//

destructor TSkeletonMeshObject.Destroy;
begin
  Clear;
  FBoneMatrixInvertedMeshes.Free;
  FBackupInvertedMeshes.Free;
  inherited Destroy;
end;

// WriteToFiler
//

procedure TSkeletonMeshObject.WriteToFiler(writer: TVirtualWriter);
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

procedure TSkeletonMeshObject.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion, i: Integer;
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

procedure TSkeletonMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FVerticeBoneWeightCount := 0;
  FBonesPerVertex := 0;
  ResizeVerticesBonesWeights;
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
end;

// SetVerticeBoneWeightCount
//

procedure TSkeletonMeshObject.SetVerticeBoneWeightCount(const val: Integer);
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

procedure TSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val: Integer);
begin
  if val <> FVerticeBoneWeightCapacity then
  begin
    FVerticeBoneWeightCapacity := val;
    ResizeVerticesBonesWeights;
  end;
end;

// SetBonesPerVertex
//

procedure TSkeletonMeshObject.SetBonesPerVertex(const val: Integer);
begin
  if val <> FBonesPerVertex then
  begin
    FBonesPerVertex := val;
    ResizeVerticesBonesWeights;
  end;
end;

// ResizeVerticesBonesWeights
//

procedure TSkeletonMeshObject.ResizeVerticesBonesWeights;
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
      newArea[i] := PVertexBoneWeightArray(PtrUInt(newArea[0]) +
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

procedure TSkeletonMeshObject.AddWeightedBone(aBoneID: Integer;
  aWeight: Single);
begin
  if BonesPerVertex < 1 then
    BonesPerVertex := 1;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[0] do
  begin
    BoneID := aBoneID;
    weight := aWeight;
  end;
end;

// AddWeightedBones
//

procedure TSkeletonMeshObject.AddWeightedBones(const boneIDs
  : TVertexBoneWeightDynArray);
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
      weight := boneIDs[i].weight;
    end;
  end;
end;

// FindOrAdd
//

function TSkeletonMeshObject.FindOrAdd(BoneID: Integer;
  const vertex, normal: TAffineVector): Integer;
var
  i: Integer;
  dynArray: TVertexBoneWeightDynArray;
begin
  if BonesPerVertex > 1 then
  begin
    SetLength(dynArray, 1);
    dynArray[0].BoneID := BoneID;
    dynArray[0].weight := 1;
    Result := FindOrAdd(dynArray, vertex, normal);
    Exit;
  end;
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
    if (VerticesBonesWeights^[i]^[0].BoneID = BoneID) and
      VectorEquals(Vertices.list^[i], vertex) and VectorEquals(normals.list^[i],
      normal) then
    begin
      Result := i;
      Break;
    end;
  if Result < 0 then
  begin
    AddWeightedBone(BoneID, 1);
    Vertices.Add(vertex);
    Result := normals.Add(normal);
  end;
end;

// FindOrAdd
//

function TSkeletonMeshObject.FindOrAdd(const boneIDs: TVertexBoneWeightDynArray;
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
      if (boneIDs[j].BoneID <> VerticesBonesWeights^[i]^[j].BoneID) or
        (boneIDs[j].weight <> VerticesBonesWeights^[i]^[j].weight) then
      begin
        bonesMatch := False;
        Break;
      end;
    end;
    if bonesMatch and VectorEquals(Vertices[i], vertex) and
      VectorEquals(normals[i], normal) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result < 0 then
  begin
    AddWeightedBones(boneIDs);
    Vertices.Add(vertex);
    Result := normals.Add(normal);
  end;
end;

// PrepareBoneMatrixInvertedMeshes
//

procedure TSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
  i, k, boneIndex: Integer;
  invMesh: TBaseMeshObject;
  invMat: TMatrix;
  Bone: TSkeletonBone;
  p: TVector;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // calculate
  for k := 0 to BonesPerVertex - 1 do
  begin
    invMesh := TBaseMeshObject.Create;
    FBoneMatrixInvertedMeshes.Add(invMesh);
    invMesh.Vertices := Vertices;
    invMesh.normals := normals;
    for i := 0 to Vertices.Count - 1 do
    begin
      boneIndex := VerticesBonesWeights^[i]^[k].BoneID;
      Bone := Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
      // transform point
      MakePoint(p, Vertices[i]);
      invMat := Bone.GlobalMatrix;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Vertices[i] := PAffineVector(@p)^;
      // transform normal
      SetVector(p, normals[i]);
      invMat := Bone.GlobalMatrix;
      invMat[3] := NullHmgPoint;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.normals[i] := PAffineVector(@p)^;
    end;
  end;
end;

procedure TSkeletonMeshObject.BackupBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  FBackupInvertedMeshes.Clear;
  // copy current stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
  begin
    bm := TBaseMeshObject.Create;
    bm.Assign(TBaseMeshObject(FBoneMatrixInvertedMeshes[i]));
    FBackupInvertedMeshes.Add(bm);
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  end;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TSkeletonMeshObject.RestoreBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // restore the backup
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
  begin
    bm := TBaseMeshObject.Create;
    bm.Assign(TBaseMeshObject(FBackupInvertedMeshes[i]));
    FBoneMatrixInvertedMeshes.Add(bm);
    TBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  end;
  FBackupInvertedMeshes.Clear;
end;

// ApplyCurrentSkeletonFrame
//

procedure TSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize: Boolean);
var
  i, j, BoneID: Integer;
  refVertices, refNormals: TAffineVectorList;
  n, nt: TVector;
  Bone: TSkeletonBone;
  Skeleton: TSkeleton;
  tempvert, tempnorm: TAffineVector;
begin
  with TBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do
  begin
    refVertices := Vertices;
    refNormals := normals;
  end;
  Skeleton := Owner.Owner.Skeleton;
  n[3] := 0;
  if BonesPerVertex = 1 then
  begin
    // simple case, one bone per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      BoneID := VerticesBonesWeights^[i]^[0].BoneID;
      Bone := Skeleton.BoneByID(BoneID);
      Vertices.list^[i] := VectorTransform(refVertices.list^[i],
        Bone.GlobalMatrix);
      PAffineVector(@n)^ := refNormals.list^[i];
      nt := VectorTransform(n, Bone.GlobalMatrix);
      normals.list^[i] := PAffineVector(@nt)^;
    end;
  end
  else
  begin
    // multiple bones per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      Vertices.list^[i] := NullVector;
      normals.list^[i] := NullVector;
      for j := 0 to BonesPerVertex - 1 do
      begin
        with TBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do
        begin
          refVertices := Vertices;
          refNormals := normals;
        end;
        tempvert := NullVector;
        tempnorm := NullVector;
        if VerticesBonesWeights^[i]^[j].weight <> 0 then
        begin
          BoneID := VerticesBonesWeights^[i]^[j].BoneID;
          Bone := Skeleton.BoneByID(BoneID);
          CombineVector(tempvert, VectorTransform(refVertices.list^[i],
            Bone.GlobalMatrix), VerticesBonesWeights^[i]^[j].weight);
          PAffineVector(@n)^ := refNormals.list^[i];
          n := VectorTransform(n, Bone.GlobalMatrix);
          CombineVector(tempnorm, PAffineVector(@n)^,
            VerticesBonesWeights^[i]^[j].weight);
        end;
        AddVector(Vertices.list^[i], tempvert);
        AddVector(normals.list^[i], tempnorm);
      end;
    end;
  end;
  if normalize then
    normals.normalize;
end;

// ------------------
// ------------------ TFaceGroup ------------------
// ------------------

// CreateOwned
//

constructor TFaceGroup.CreateOwned(aOwner: TFaceGroups);
begin
  FOwner := aOwner;
  FLightMapIndex := -1;
  Create;
  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Mesh.TagName := ClassName;
  if Assigned(FOwner) then
  begin
    FOwner.Add(Self);
    FBatch.Mesh.Owner := FOwner.Owner.Owner.Owner;
  end;
end;

// Destroy
//

destructor TFaceGroup.Destroy;
begin
  FBatch.Mesh.Free;
  FBatch.InstancesChain.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

// WriteToFiler
//

procedure TFaceGroup.WriteToFiler(writer: TVirtualWriter);
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

procedure TFaceGroup.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
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

procedure TFaceGroup.AttachLightmap(lightMap: TGLTexture;
  var mrci: TRenderContextInfo);
begin
  if GL.ARB_multitexture then
    with lightMap do
    begin
      Assert(Image.NativeTextureTarget = ttTexture2D);
      mrci.GLStates.TextureBinding[1, ttTexture2D] := Handle;
      GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      mrci.GLStates.ActiveTexture := 0;
    end;
end;

// AttachOrDetachLightmap
//

procedure TFaceGroup.AttachOrDetachLightmap(var mrci: TRenderContextInfo);
var
  libMat: TGLLibMaterial;
begin
  if GL.ARB_multitexture then
  begin
    if (not mrci.ignoreMaterials) and Assigned(mrci.LightmapLibrary) then
    begin
      if Owner.Owner.FLastLightMapIndex <> LightMapIndex then
      begin
        Owner.Owner.FLastLightMapIndex := LightMapIndex;
        if LightMapIndex >= 0 then
        begin
          // attach and activate lightmap
          Assert(LightMapIndex < TGLMaterialLibrary(mrci.LightmapLibrary)
            .Materials.Count);
          libMat := TGLMaterialLibrary(mrci.LightmapLibrary).Materials
            [LightMapIndex];
          AttachLightmap(libMat.Material.Texture, mrci);
          // Owner.Owner.EnableLightMapArray(mrci);
        end
        else
        begin
          // desactivate lightmap
          // Owner.Owner.DisableLightMapArray(mrci);
        end;
      end;
    end;
  end;
end;

// PrepareMaterialLibraryCache
//

procedure TFaceGroup.PrepareMaterialLibraryCache
  (matLib: TGLAbstractMaterialLibrary);
begin
  if (FMaterialName <> '') and (matLib <> nil) then
  begin
    if matLib is TGLMaterialLibrary then
      FMaterialCache := TGLMaterialLibrary(matLib)
        .Materials.GetLibMaterialByName(FMaterialName)
    else
      Assert(False);
  end
  else
    FMaterialCache := nil;
end;

// DropMaterialLibraryCache
//

procedure TFaceGroup.DropMaterialLibraryCache;
begin
  FMaterialCache := nil;
end;

// AddToTriangles
//

procedure TFaceGroup.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
begin
  // nothing
end;

// Reverse
//

procedure TFaceGroup.Reverse;
begin
  // nothing
end;

// Prepare
//

procedure TFaceGroup.Prepare;
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
      FMode := TFaceGroupMeshMode(ReadInteger);
      Owner.Owner.Owner.Owner.StructureChanged;;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexIndexList.SetVertexIndices(const val: TIntegerList);
begin
  FVertexIndices.Assign(val);
  Owner.Owner.Owner.Owner.StructureChanged;
end;

// BuildList
//

procedure TFGVertexIndexList.BuildList(var mrci: TRenderContextInfo);
const
  cFaceGroupMeshModeToOpenGL: array [TFaceGroupMeshMode] of Integer =
    (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);
begin
  if vertexIndices.Count = 0 then
    Exit;
  AttachOrDetachLightmap(mrci);

  GL.DrawElements(cFaceGroupMeshModeToOpenGL[mode], vertexIndices.Count,
    GL_UNSIGNED_INT, vertexIndices.list);
end;

procedure TFGVertexIndexList.BuildMesh;
var
  mo: TMeshObject;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  i, j, e: Integer;
begin
  mo := Owner.Owner;

  if mo.Vertices.Count > 0 then
  begin
    gotColor := (mo.Vertices.Count = mo.Colors.Count);
    gotNormals := (mo.Vertices.Count = mo.normals.Count);
    gotTexCoords := (mo.Vertices.Count = mo.texCoords.Count);
    if mo.FTexCoordsEx.Count > 0 then
    begin
      SetLength(gotTexCoordsEx, mo.FTexCoordsEx.Count);
      for i := 0 to mo.FTexCoordsEx.Count - 1 do
        gotTexCoordsEx[i] := (mo.TexCoordsEx[i].Count > 0);
      gotTexCoords := False;
    end;

    with FBatch.Mesh do
    begin
      Lock;
      try
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        if gotColor then
          DeclareAttribute(attrColor, GLSLType4f);
        if gotNormals then
          DeclareAttribute(attrNormal, GLSLType3f);
        if gotTexCoords then
          DeclareAttribute(attrTexCoord0, GLSLType3f)
        else
          for i := 0 to mo.FTexCoordsEx.Count - 1 do
            if gotTexCoordsEx[i] then
              DeclareAttribute(TAttribLocation(i + ord(attrTexCoord0)),
                GLSLType4f);

        case mode of
          fgmmTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleStrip:
            BeginAssembly(mpTRIANGLE_STRIP);
          fgmmFlatTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleFan:
            BeginAssembly(mpTRIANGLE_FAN);
          fgmmQuads:
            Abort;
        end;

        for j := 0 to vertexIndices.Count - 1 do
        begin
          e := vertexIndices[j];
          Attribute3f(attrPosition, mo.Vertices[e]);
          if gotColor then
            Attribute4f(attrColor, mo.Colors[e]);
          if gotNormals then
            Attribute3f(attrNormal, mo.normals[e]);
          if gotTexCoords then
            Attribute3f(attrTexCoord0, mo.texCoords[e])
          else
            for i := 0 to mo.FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[i] then
                Attribute4f(TAttribLocation(i + ord(attrTexCoord0)),
                  mo.TexCoordsEx[i][e]);
          EmitVertex;
        end;

        EndAssembly;
      finally
        UnLock;
      end;
    end;
  end;
end;

// AddToList
//

procedure TFGVertexIndexList.AddToList(Source, destination: TAffineVectorList;
  indices: TIntegerList);
var
  i, n: Integer;
begin
  if not Assigned(destination) then
    Exit;
  if indices.Count < 3 then
    Exit;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        n := (indices.Count div 3) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 0 to n - 1 do
            destination.Add(Source[indices.list^[i]]);
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmTriangleStrip:
      begin
        if Source.Count > 0 then
          ConvertStripToList(Source, indices, destination)
        else
          destination.AddNulls(destination.Count + (indices.Count - 2) * 3);
      end;
    fgmmTriangleFan:
      begin
        n := (indices.Count - 2) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 2 to vertexIndices.Count - 1 do
          begin
            destination.Add(Source[indices.list^[0]],
              Source[indices.list^[i - 1]], Source[indices.list^[i]]);
          end;
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmQuads:
      begin
        n := indices.Count div 4;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n * 6);
          i := 0;
          while n > 0 do
          begin
            destination.Add(Source[indices.list^[i]],
              Source[indices.list^[i + 1]], Source[indices.list^[i + 2]]);
            destination.Add(Source[indices.list^[i]],
              Source[indices.list^[i + 2]], Source[indices.list^[i + 3]]);
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
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  mo: TMeshObject;
begin
  mo := Owner.Owner;
  AddToList(mo.Vertices, aList, vertexIndices);
  AddToList(mo.texCoords, aTexCoords, vertexIndices);
  AddToList(mo.normals, aNormals, vertexIndices);
  Owner.Owner.Owner.Owner.StructureChanged;
end;

// TriangleCount
//

function TFGVertexIndexList.TriangleCount: Integer;
begin
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      Result := vertexIndices.Count div 3;
    fgmmTriangleFan, fgmmTriangleStrip:
      begin
        Result := vertexIndices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    fgmmQuads:
      Result := vertexIndices.Count div 2;
  else
    Result := 0;
    Assert(False);
  end;
end;

// Reverse
//

procedure TFGVertexIndexList.Reverse;
begin
  vertexIndices.Reverse;
  Owner.Owner.Owner.Owner.StructureChanged;
end;

// Add
//

procedure TFGVertexIndexList.Add(idx: Integer);
begin
  FVertexIndices.Add(idx);
  Owner.Owner.Owner.Owner.StructureChanged;
end;

// GetExtents
//

procedure TFGVertexIndexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
  ref: PFloatArray;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to vertexIndices.Count - 1 do
  begin
    ref := Owner.Owner.Vertices.ItemAddress[vertexIndices[i]];
    for k := 0 to 2 do
    begin
      f := ref^[k];
      if f < min[k] then
        min[k] := f;
      if f > max[k] then
        max[k] := f;
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
  if vertexIndices.Count >= 3 then
  begin
    case mode of
      fgmmTriangleStrip:
        begin
          bufList := TIntegerList.Create;
          try
            ConvertStripToList(vertexIndices, bufList);
            vertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
      fgmmTriangleFan:
        begin
          bufList := TIntegerList.Create;
          try
            for i := 0 to vertexIndices.Count - 3 do
              bufList.Add(vertexIndices[0], vertexIndices[i],
                vertexIndices[i + 1]);
            vertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
    end;
    Owner.Owner.Owner.Owner.StructureChanged;
  end;
end;

// GetNormal
//

function TFGVertexIndexList.GetNormal: TAffineVector;
begin
  if vertexIndices.Count < 3 then
    Result := NullVector
  else
    with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[vertexIndices[0]], Items[vertexIndices[1]],
        Items[vertexIndices[2]], Result);
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

procedure TFGVertexNormalTexIndexList.SetTexCoordIndices
  (const val: TIntegerList);
begin
  FTexCoordIndices.Assign(val);
end;

// BuildList
//

procedure TFGVertexNormalTexIndexList.BuildList(var mrci: TRenderContextInfo);
var
  i: Integer;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  texCoordPool: PAffineVectorArray;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
begin
  Assert(((TexCoordIndices.Count = 0) or (vertexIndices.Count <=
    TexCoordIndices.Count)) and ((normalIndices.Count = 0) or
    (vertexIndices.Count <= normalIndices.Count)));
  vertexPool := Owner.Owner.Vertices.list;
  normalPool := Owner.Owner.normals.list;
  texCoordPool := Owner.Owner.texCoords.list;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      GL.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip:
      GL.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan:
      GL.Begin_(GL_TRIANGLE_FAN);
  else
    Assert(False);
  end;
  vertexIdxList := vertexIndices.list;
  if normalIndices.Count > 0 then
    normalIdxList := normalIndices.list
  else
    normalIdxList := vertexIdxList;
  if TexCoordIndices.Count > 0 then
    texCoordIdxList := TexCoordIndices.list
  else
    texCoordIdxList := vertexIdxList;
  if Assigned(texCoordPool) then
  begin
    for i := 0 to vertexIndices.Count - 1 do
    begin
      GL.Normal3fv(@normalPool[normalIdxList^[i]]);
      GL.TexCoord2fv(@texCoordPool[texCoordIdxList^[i]]);
      GL.Vertex3fv(@vertexPool[vertexIdxList^[i]]);
    end;
  end
  else
  begin
    for i := 0 to vertexIndices.Count - 1 do
    begin
      GL.Normal3fv(@normalPool[normalIdxList^[i]]);
      GL.Vertex3fv(@vertexPool[vertexIdxList^[i]]);
    end;
  end;
  GL.End_;
end;

procedure TFGVertexNormalTexIndexList.BuildMesh;
var
  mo: TMeshObject;
  gotNormals, gotTexCoords, gotColor: Boolean;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
  gotTexCoordsEx: array of Boolean;
  i, j, e: Integer;
begin
  mo := Owner.Owner;

  if mo.Vertices.Count > 0 then
  begin
    gotColor := (mo.Vertices.Count = mo.Colors.Count);

    vertexIdxList := vertexIndices.list;
    if normalIndices.Count > 0 then
    begin
      normalIdxList := normalIndices.list;
      gotNormals := (normalIndices.Count = vertexIndices.Count);
    end
    else
    begin
      normalIdxList := vertexIdxList;
      gotNormals := (mo.Vertices.Count = mo.normals.Count);
    end;

    if TexCoordIndices.Count > 0 then
    begin
      texCoordIdxList := TexCoordIndices.list;
      gotTexCoords := (TexCoordIndices.Count = vertexIndices.Count);
    end
    else
    begin
      texCoordIdxList := vertexIdxList;
      gotTexCoords := (mo.Vertices.Count = mo.texCoords.Count);
    end;

    if mo.FTexCoordsEx.Count > 0 then
    begin
      SetLength(gotTexCoordsEx, mo.FTexCoordsEx.Count);
      for i := 0 to mo.FTexCoordsEx.Count - 1 do
        gotTexCoordsEx[i] := (mo.TexCoordsEx[i].Count > 0);
      gotTexCoords := False;
    end;

    with FBatch.Mesh do
    begin
      Lock;
      try
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        if gotColor then
          DeclareAttribute(attrColor, GLSLType4f);
        if gotNormals then
          DeclareAttribute(attrNormal, GLSLType3f);
        if gotTexCoords then
          DeclareAttribute(attrTexCoord0, GLSLType3f)
        else
          for i := 0 to mo.FTexCoordsEx.Count - 1 do
            if gotTexCoordsEx[i] then
              DeclareAttribute(TAttribLocation(i + ord(attrTexCoord0)),
                GLSLType4f);

        case mode of
          fgmmTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleStrip:
            BeginAssembly(mpTRIANGLE_STRIP);
          fgmmFlatTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleFan:
            BeginAssembly(mpTRIANGLE_FAN);
          fgmmQuads:
            Abort;
        end;

        for j := 0 to vertexIndices.Count - 1 do
        begin
          e := vertexIdxList[j];
          Attribute3f(attrPosition, mo.Vertices[e]);
          if gotColor then
            Attribute4f(attrColor, mo.Colors[e]);
          e := normalIdxList[j];
          if gotNormals then
            Attribute3f(attrNormal, mo.normals[e]);
          e := texCoordIdxList[j];
          if gotTexCoords then
            Attribute3f(attrTexCoord0, mo.texCoords[e])
          else
            for i := 0 to mo.FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[i] then
                Attribute4f(TAttribLocation(i + ord(attrTexCoord0)),
                  mo.TexCoordsEx[i][e]);
          EmitVertex;
        end;

        EndAssembly;
      finally
        UnLock;
      end;
    end;
  end;
end;

// AddToTriangles
//

procedure TFGVertexNormalTexIndexList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
begin
  AddToList(Owner.Owner.Vertices, aList, vertexIndices);
  AddToList(Owner.Owner.texCoords, aTexCoords, TexCoordIndices);
  AddToList(Owner.Owner.normals, aNormals, normalIndices);
end;

// Add
//

procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx,
  texCoordIdx: Integer);
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

procedure TFGIndexTexCoordList.BuildList(var mrci: TRenderContextInfo);
var
  i, k: Integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: Boolean;

begin
  Assert(vertexIndices.Count = texCoords.Count);
  texCoordPool := texCoords.list;
  vertexPool := Owner.Owner.Vertices.list;
  indicesPool := @vertexIndices.list[0];
  colorPool := @Owner.Owner.Colors.list[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case mode of
    fgmmTriangles:
      GL.Begin_(GL_TRIANGLES);
    fgmmFlatTriangles:
      GL.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip:
      GL.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan:
      GL.Begin_(GL_TRIANGLE_FAN);
    fgmmQuads:
      GL.Begin_(GL_QUADS);
  else
    Assert(False);
  end;
  if Owner.Owner.normals.Count = Owner.Owner.Vertices.Count then
  begin
    normalPool := Owner.Owner.normals.list;
    for i := 0 to vertexIndices.Count - 1 do
    begin
      GL.TexCoord2fv(@texCoordPool[i]);
      k := indicesPool[i];
      if gotColor then
        GL.Color4fv(@colorPool[k]);
      GL.Normal3fv(@normalPool[k]);
      GL.Vertex3fv(@vertexPool[k]);
    end;
  end
  else
  begin
    for i := 0 to vertexIndices.Count - 1 do
    begin
      GL.TexCoord2fv(@texCoordPool[i]);
      if gotColor then
        GL.Color4fv(@colorPool[indicesPool[i]]);
      GL.Vertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  GL.End_;
  GL.CheckError;
end;

procedure TFGIndexTexCoordList.BuildMesh;
var
  mo: TMeshObject;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  i, j, e: Integer;
begin
  mo := Owner.Owner;

  if mo.Vertices.Count > 0 then
  begin
    gotColor := (mo.Vertices.Count = mo.Colors.Count);
    gotNormals := (mo.Vertices.Count = mo.normals.Count);
    gotTexCoords := (mo.Vertices.Count = texCoords.Count);
    if mo.FTexCoordsEx.Count > 0 then
    begin
      SetLength(gotTexCoordsEx, mo.FTexCoordsEx.Count);
      for i := 0 to mo.FTexCoordsEx.Count - 1 do
        gotTexCoordsEx[i] := (mo.TexCoordsEx[i].Count > 0);
      gotTexCoords := False;
    end;

    with FBatch.Mesh do
    begin
      Lock;
      try
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        if gotColor then
          DeclareAttribute(attrColor, GLSLType4f);
        if gotNormals then
          DeclareAttribute(attrNormal, GLSLType3f);
        if gotTexCoords then
          DeclareAttribute(attrTexCoord0, GLSLType3f)
        else
          for i := 0 to mo.FTexCoordsEx.Count - 1 do
            if gotTexCoordsEx[i] then
              DeclareAttribute(TAttribLocation(i + ord(attrTexCoord0)),
                GLSLType4f);

        case mode of
          fgmmTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleStrip:
            BeginAssembly(mpTRIANGLE_STRIP);
          fgmmFlatTriangles:
            BeginAssembly(mpTRIANGLES);
          fgmmTriangleFan:
            BeginAssembly(mpTRIANGLE_FAN);
          fgmmQuads:
            Abort;
        end;

        for j := 0 to vertexIndices.Count - 1 do
        begin
          e := vertexIndices[j];
          Attribute3f(attrPosition, mo.Vertices[e]);
          if gotColor then
            Attribute4f(attrColor, mo.Colors[e]);
          if gotNormals then
            Attribute3f(attrNormal, mo.normals[e]);
          if gotTexCoords then
            Attribute3f(attrTexCoord0, texCoords[e])
          else
            for i := 0 to mo.FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[i] then
                Attribute4f(TAttribLocation(i + ord(attrTexCoord0)),
                  mo.TexCoordsEx[i][e]);
          EmitVertex;
        end;

        EndAssembly;
      finally
        UnLock;
      end;
    end;
  end;
end;

// AddToTriangles
//

procedure TFGIndexTexCoordList.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  i, n: Integer;
  texCoordList: TAffineVectorList;
begin
  AddToList(Owner.Owner.Vertices, aList, vertexIndices);
  AddToList(Owner.Owner.normals, aNormals, vertexIndices);
  texCoordList := Self.texCoords;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        if Assigned(aTexCoords) then
        begin
          n := (vertexIndices.Count div 3) * 3;
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
            (vertexIndices.Count - 2) * 3);
          for i := 2 to vertexIndices.Count - 1 do
          begin
            aTexCoords.Add(texCoordList[0], texCoordList[i - 1],
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
  texCoords.Add(texCoord);
  inherited Add(idx);
end;

// Add (s, t)
//

procedure TFGIndexTexCoordList.Add(idx: Integer; const s, t: Single);
begin
  texCoords.Add(s, t, 0);
  inherited Add(idx);
end;

// ------------------
// ------------------ TFaceGroups ------------------
// ------------------

// CreateOwned
//

constructor TFaceGroups.CreateOwned(aOwner: TMeshObject);
begin
  FOwner := aOwner;
  Create;
end;

// Destroy
//

destructor TFaceGroups.Destroy;
begin
  Clear;
  inherited;
end;

// ReadFromFiler
//

procedure TFaceGroups.ReadFromFiler(reader: TVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

// Clear
//

procedure TFaceGroups.Clear;
var
  i: Integer;
  fg: TFaceGroup;
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

function TFaceGroups.GetFaceGroup(Index: Integer): TFaceGroup;
begin
  Result := TFaceGroup(list^[Index]);
end;

// PrepareMaterialLibraryCache
//

procedure TFaceGroups.PrepareMaterialLibraryCache
  (matLib: TGLAbstractMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TFaceGroup(list^[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//

procedure TFaceGroups.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TFaceGroup(list^[i]).DropMaterialLibraryCache;
end;

// AddToTriangles
//

procedure TFaceGroups.AddToTriangles(aList: TAffineVectorList;
  aTexCoords: TAffineVectorList = nil; aNormals: TAffineVectorList = nil);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

// MaterialLibrary
//

function TFaceGroups.MaterialLibrary: TGLAbstractMaterialLibrary;
var
  mol: TMeshObjectList;
  bm: TGLBaseMesh;
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
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

// CompareMaterials
//

function CompareMaterials(item1, item2: TObject): Integer;

  function MaterialIsOpaque(fg: TFaceGroup): Boolean;
  var
    libMat: TGLLibMaterial;
  begin
    libMat := fg.MaterialCache;
    Result := (not Assigned(libMat)) or (not libMat.Material.Blended);
  end;

var
  fg1, fg2: TFaceGroup;
  opaque1, opaque2: Boolean;
begin
  fg1 := TFaceGroup(item1);
  opaque1 := MaterialIsOpaque(fg1);
  fg2 := TFaceGroup(item2);
  opaque2 := MaterialIsOpaque(fg2);
  if opaque1 = opaque2 then
  begin
    Result := CompareStr(fg1.MaterialName, fg2.MaterialName);
    if Result = 0 then
      Result := fg1.LightMapIndex - fg2.LightMapIndex;
  end
  else if opaque1 then
    Result := -1
  else
    Result := 1;
end;

// SortByMaterial
//

procedure TFaceGroups.SortByMaterial;
begin
  PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
  Sort(@CompareMaterials);
end;

// ------------------
// ------------------ TVectorFile ------------------
// ------------------

// Create
//

constructor TVectorFile.Create(aOwner: TPersistent);
begin
  Assert(aOwner is TGLBaseMesh);
  inherited;
end;

// Owner
//

function TVectorFile.Owner: TGLBaseMesh;
begin
  Result := TGLBaseMesh(GetOwner);
end;

// SetFaceWinding
//

procedure TVectorFile.SetFaceWinding(const val: TMeshFaceWinding);
begin
  FFaceWinding := val;
end;

// ------------------
// ------------------ TGLGLSMVectorFile ------------------
// ------------------

// Capabilities
//

class function TGLGLSMVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

// LoadFromStream
//

procedure TGLGLSMVectorFile.LoadFromStream(aStream: TStream);
begin
  Owner.MeshObjects.LoadFromStream(aStream);
end;

// SaveToStream
//

procedure TGLGLSMVectorFile.SaveToStream(aStream: TStream);
begin
  Owner.MeshObjects.SaveToStream(aStream);
end;

// ------------------
// ------------------ TGLBaseMesh ------------------
// ------------------

// Create
//

constructor TGLBaseMesh.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ObjectStyle := ObjectStyle + [osDeferredDraw];
  if FMeshObjects = nil then
    FMeshObjects := TMeshObjectList.CreateOwned(Self);
  if FSkeleton = nil then
    FSkeleton := TSkeleton.CreateOwned(Self);
  FUseMeshMaterials := True;
  FAutoCentering := [];
  FAutoScaling := TGLCoordinates.CreateInitialized(Self, XYZWHmgVector,
    csPoint);
end;

// Destroy
//

destructor TGLBaseMesh.Destroy;
begin
  FConnectivity.Free;
  DropMaterialLibraryCache;
  FSkeleton.Free;
  FMeshObjects.Free;
  FAutoScaling.Free;
  inherited Destroy;
end;

// DoRender
//

procedure TGLBaseMesh.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  i, j: Integer;
  bUseOwnMaterial: Boolean;
begin
  if ocStructure in Changes then
  begin
    BuildMeshes;
  end;

  if ARenderSelf then
  begin
    FTransformation := ARci.PipelineTransformation.StackTop;
    if Tag = 1 then
      Sleep(0);

    bUseOwnMaterial := UseMeshMaterials and Assigned(FMaterialLibrary);
    if bUseOwnMaterial and not FMaterialLibraryCachesPrepared then
      PrepareMaterialLibraryCache;

    for i := MeshObjects.Count - 1 downto 0 do
    begin
      with MeshObjects[i] do
      begin
        if Visible then
        begin
          case mode of
            momTriangles, momTriangleStrip:
              ARci.drawList.Add(@FBatch);
            momFaceGroups:
              for j := FaceGroups.Count - 1 downto 0 do
                with FaceGroups[j] do
                begin
                  if bUseOwnMaterial and Assigned(FMaterialCache) then
                    FBatch.Material := FMaterialCache
                  else
                    FBatch.Material := FMaterial;
                  ARci.drawList.Add(@FBatch);
                end;
          end;
        end;
      end;
    end;
  end;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

procedure TGLBaseMesh.DoShowAxes;
var
  i, j: Integer;
begin
  for i := MeshObjects.Count - 1 downto 0 do
  begin
    with MeshObjects[i] do
    begin
      FBatch.ShowAxes := ShowAxes;
      for j := FaceGroups.Count - 1 downto 0 do
        FaceGroups[j].FBatch.ShowAxes := ShowAxes
    end;
  end;
end;

// Assign
//

procedure TGLBaseMesh.Assign(Source: TPersistent);
begin
  if Source is TGLBaseMesh then
  begin
    FSkeleton.Clear;
    FFaceWinding := TGLBaseMesh(Source).FFaceWinding;
    FMaterialLibrary := TGLBaseMesh(Source).FMaterialLibrary;
    FLightmapLibrary := TGLBaseMesh(Source).FLightmapLibrary;
    FUseMeshMaterials := TGLBaseMesh(Source).FUseMeshMaterials;
    FOverlaySkeleton := TGLBaseMesh(Source).FOverlaySkeleton;
    FIgnoreMissingTextures := TGLBaseMesh(Source).FIgnoreMissingTextures;
    FAutoCentering := TGLBaseMesh(Source).FAutoCentering;
    FAutoScaling.Assign(TGLBaseMesh(Source).FAutoScaling);
    FSkeleton.Assign(TGLBaseMesh(Source).FSkeleton);
    FSkeleton.RootBones.PrepareGlobalMatrices;
    FMeshObjects.Assign(TGLBaseMesh(Source).FMeshObjects);
  end;
  inherited Assign(Source);
end;

// LoadFromFile
//

procedure TGLBaseMesh.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  FLastLoadedFilename := '';
  if filename <> '' then
  begin
    fs := CreateFileStream(filename, fmOpenRead + fmShareDenyWrite);
    try
      LoadFromStream(filename, fs);
      FLastLoadedFilename := filename;
    finally
      fs.Free;
    end;
  end;
end;

// LoadFromStream
//

procedure TGLBaseMesh.LoadFromStream(const filename: string; aStream: TStream);
var
  newVectorFile: TVectorFile;
  VectorFileClass: TVectorFileClass;
begin
  FLastLoadedFilename := '';
  if filename <> '' then
  begin
    MeshObjects.Clear;
    Skeleton.Clear;
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
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

procedure TGLBaseMesh.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  if filename <> '' then
  begin
    fs := CreateFileStream(filename, fmCreate);
    try
      SaveToStream(filename, fs);
    finally
      fs.Free;
    end;
  end;
end;

// SaveToStream
//

procedure TGLBaseMesh.SaveToStream(const filename: string; aStream: TStream);
var
  newVectorFile: TVectorFile;
  VectorFileClass: TVectorFileClass;
begin
  if filename <> '' then
  begin
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
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

procedure TGLBaseMesh.AddDataFromFile(const filename: string);
var
  fs: TStream;
begin
  if filename <> '' then
  begin
    fs := CreateFileStream(filename, fmOpenRead + fmShareDenyWrite);
    try
      AddDataFromStream(filename, fs);
    finally
      fs.Free;
    end;
  end;
end;

// AddDataFromStream
//

procedure TGLBaseMesh.AddDataFromStream(const filename: string;
  aStream: TStream);
var
  newVectorFile: TVectorFile;
  VectorFileClass: TVectorFileClass;
begin
  if filename <> '' then
  begin
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
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
      newVectorFile.Free;
    end;
    PrepareMesh;
  end;
end;

procedure TGLBaseMesh.ApplyExtras;
var
  i, j: Integer;
  LBatch: PDrawBatch;

  procedure DoApplyExtras;
  begin
    with LBatch.Mesh do
    begin
      try
        Lock;
        if FFaceWinding = mnoInvert then
          FlipFaces(False, False);

        if mesTangents in FMeshExtras then
        begin
          if not Attributes[attrTangent] then
            ComputeTangents;
        end
        else
        begin
          Attributes[attrTangent] := False;
          Attributes[attrBinormal] := False;
          Validate;
        end;

        if not(osStreamDraw in ObjectStyle) then
          WeldVertices;
      finally
        UnLock;
      end;
    end;
  end;

begin
  for i := MeshObjects.Count - 1 downto 0 do
  begin
    with MeshObjects[i] do
    begin
      case mode of
        momTriangles, momTriangleStrip:
          begin
            LBatch := @FBatch;
            DoApplyExtras;
          end;
        momFaceGroups:
          for j := FaceGroups.Count - 1 downto 0 do
          begin
            LBatch := @FaceGroups[j].FBatch;
            DoApplyExtras;
          end;
      end; // of case
    end;
  end;
end;

// GetExtents
//

procedure TGLBaseMesh.GetExtents(out min, max: TAffineVector);
var
  lAABB: TAABB;
begin
  lAABB := GetAABB();
  min := lAABB.min;
  max := lAABB.max;
end;

function TGLBaseMesh.GetPickingMaterial: string;
begin

end;

// GetBarycenter
//

function TGLBaseMesh.GetBarycenter: TAffineVector;
var
  i, nb: Integer;
begin
  Result := NullVector;
  nb := 0;
  for i := 0 to MeshObjects.Count - 1 do
    TMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
  if nb > 0 then
    ScaleVector(Result, 1 / nb);
end;

// LastLoadedFilename
//

function TGLBaseMesh.LastLoadedFilename: string;
begin
  Result := FLastLoadedFilename;
end;

// SetMaterialLibrary
//

procedure TGLBaseMesh.SetMaterialLibrary(const val: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
    if Assigned(FMaterialLibrary) then
    begin
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetMeshExtras(const Value: TMeshExtras);
begin
  if FMeshExtras <> Value then
  begin
    FMeshExtras := Value;
    StructureChanged;
  end;
end;

// SetMaterialLibrary
//

procedure TGLBaseMesh.SetLightmapLibrary(const val: TGLMaterialLibrary);
begin
  if FLightmapLibrary <> val then
  begin
    if Assigned(FLightmapLibrary) then
    begin
      FLightmapLibrary.RemoveFreeNotification(Self);
    end;
    FLightmapLibrary := val;
    if Assigned(FLightmapLibrary) then
      FLightmapLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

// SetFaceWinding
//

procedure TGLBaseMesh.SetFaceWinding(const val: TMeshFaceWinding);
begin
  if val <> FFaceWinding then
  begin
    FFaceWinding := val;
    StructureChanged;
  end;
end;

// SetOverlaySkeleton
//

procedure TGLBaseMesh.SetOverlaySkeleton(const val: Boolean);
begin
  if FOverlaySkeleton <> val then
  begin
    FOverlaySkeleton := val;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseMesh.SetPickingMaterial(const Value: string);
begin

end;

procedure TGLBaseMesh.SetShowAABB(const Value: Boolean);
begin
  FShowAABB := Value;
end;

// AutoScaling
//

procedure TGLBaseMesh.SetAutoScaling(const Value: TGLCoordinates);
begin
  FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

// Notification
//

procedure TGLBaseMesh.Notification(AComponent: TComponent;
  Operation: TOperation);
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

function TGLBaseMesh.GetAABB: TAABB;
begin
  if ocStructure in Changes then
    BuildMeshes;
  Result := FAABBCache;
end;

// AxisAlignedDimensionsUnscaled
//

function TGLBaseMesh.AxisAlignedDimensionsUnscaled: TVector;
var
  lAABB: TAABB;
  v3: TAffineVector;
begin
  lAABB := GetAABB;
  v3[0] := MaxFloat(Abs(lAABB.min[0]), Abs(lAABB.max[0]));
  v3[1] := MaxFloat(Abs(lAABB.min[1]), Abs(lAABB.max[1]));
  v3[2] := MaxFloat(Abs(lAABB.min[2]), Abs(lAABB.max[2]));
  Result := VectorMake(v3);
end;

// BarycenterAbsolutePosition
//

function TGLBaseMesh.BarycenterAbsolutePosition: TVector;
var
  lAABB: TAABB;
begin
  lAABB := GetAABB();
  Result[0] := (lAABB.max[0] + lAABB.min[0]) / 2;
  Result[1] := (lAABB.max[1] + lAABB.min[1]) / 2;
  Result[2] := (lAABB.max[2] + lAABB.min[2]) / 2;
  Result[3] := 1;

  Result := LocalToAbsolute(Result);
end;

// PrepareVectorFile
//

procedure TGLBaseMesh.PrepareVectorFile(aFile: TVectorFile);
begin
  aFile.FaceWinding := FaceWinding;
end;

// PerformAutoCentering
//

procedure TGLBaseMesh.PerformAutoCentering;
var
  delta, min, max: TAffineVector;
  i, j: Integer;
  LTranslateMatrix: TMatrix;
begin
  if macUseBarycenter in AutoCentering then
  begin
    delta := VectorNegate(GetBarycenter);
  end
  else
  begin
    GetExtents(min, max);
    if macCenterX in AutoCentering then
      delta[0] := -0.5 * (min[0] + max[0])
    else
      delta[0] := 0;
    if macCenterY in AutoCentering then
      delta[1] := -0.5 * (min[1] + max[1])
    else
      delta[1] := 0;
    if macCenterZ in AutoCentering then
      delta[2] := -0.5 * (min[2] + max[2])
    else
      delta[2] := 0;
  end;
  LTranslateMatrix := CreateTranslationMatrix(delta);

  for i := MeshObjects.Count - 1 downto 0 do
    with MeshObjects[i] do
    begin
      case mode of
        momTriangles, momTriangleStrip:
        with FBatch.Mesh do
        begin
          Lock;
          try
           Transform(LTranslateMatrix);
          finally
           Unlock;
          end;
        end;
        momFaceGroups:
          for j := FaceGroups.Count - 1 downto 0 do
          with FaceGroups[j].FBatch.Mesh do
          begin
             Lock;
             try
               Transform(LTranslateMatrix);
             finally
               Unlock;
             end;
          end;
      end; // of case
    end;
end;

// PerformAutoScaling
//

procedure TGLBaseMesh.PerformAutoScaling;
var
  i, j: Integer;
  LScaleMatrix: TMatrix;
begin
  if (FAutoScaling.DirectX <> 1) or (FAutoScaling.DirectY <> 1) or
    (FAutoScaling.DirectZ <> 1) then
  begin
    LScaleMatrix := CreateScaleMatrix(FAutoScaling.AsAffineVector);
    for i := MeshObjects.Count - 1 downto 0 do
      with MeshObjects[i] do
      begin
        case mode of
          momTriangles, momTriangleStrip:
          with FBatch.Mesh do
          begin
            Lock;
            try
             Transform(LScaleMatrix);
            finally
             Unlock;
            end;
          end;
          momFaceGroups:
            for j := FaceGroups.Count - 1 downto 0 do
            with FaceGroups[j].FBatch.Mesh do
            begin
               Lock;
               try
                 Transform(LScaleMatrix);
               finally
                 Unlock;
               end;
            end;
        end; // of case
      end;
  end;
end;

// PrepareMesh
//

procedure TGLBaseMesh.PrepareMesh;
begin
  StructureChanged;
end;

// PrepareMaterialLibraryCache
//

procedure TGLBaseMesh.PrepareMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
    DropMaterialLibraryCache;
  MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
  FMaterialLibraryCachesPrepared := True;
end;

// DropMaterialLibraryCache
//

procedure TGLBaseMesh.DropMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
  begin
    MeshObjects.DropMaterialLibraryCache;
    FMaterialLibraryCachesPrepared := False;
  end;
end;

// PrepareBuildList
//

procedure TGLBaseMesh.PrepareBuildList(var mrci: TRenderContextInfo);
begin
  MeshObjects.PrepareBuildList(mrci);
  if LightmapLibrary <> nil then
    LightmapLibrary.Materials.PrepareBuildList
end;

// SetUseMeshMaterials
//

procedure TGLBaseMesh.SetUseMeshMaterials(const val: Boolean);
begin
  if val <> FUseMeshMaterials then
  begin
    FUseMeshMaterials := val;
    if FMaterialLibraryCachesPrepared and (not val) then
      DropMaterialLibraryCache;
    StructureChanged;
  end;
end;

// StructureChanged
//

procedure TGLBaseMesh.StructureChanged;
begin
  DropMaterialLibraryCache;
  MeshObjects.Prepare;
  inherited;
end;

// StructureChangedNoPrepare
//

procedure TGLBaseMesh.StructureChangedNoPrepare;
begin
  inherited StructureChanged;
end;

// RayCastIntersect
//

function TGLBaseMesh.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector; intersectNormal: PVector): Boolean;
var
  i, j: Integer;
  locRayStart, locRayVector, locPoint, locNormal: TVector;
  d, minD: Single;
  LBatch: PDrawBatch;

  procedure DoIntersect;
  begin
    with LBatch.Mesh do
    begin
      Lock;
      try
        if RayCastIntersect(locRayStart, locRayVector, locPoint, locNormal) then
        begin
          d := VectorDistance2(locRayStart, locPoint);
          if (d < minD) or (minD < 0) then
          begin
            minD := d;
            if intersectPoint <> nil then
              intersectPoint^ := locPoint;
            if intersectNormal <> nil then
              intersectNormal^ := locNormal;
          end;
        end;
      finally
        UnLock;
      end;
    end;
  end;

begin
  if ocStructure in Changes then
    BuildMeshes;

  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  minD := -1;

  for i := MeshObjects.Count - 1 downto 0 do
  begin
    with MeshObjects[i] do
    begin
      case mode of
        momTriangles, momTriangleStrip:
          begin
            LBatch := @FBatch;
            DoIntersect;
          end;
        momFaceGroups:
          for j := FaceGroups.Count - 1 downto 0 do
          begin
            LBatch := @FaceGroups[j].FBatch;
            DoIntersect;
          end;
      end; // of case
    end;
  end;

  Result := (minD >= 0);
  if Result then
  begin
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(locPoint));
    if Assigned(intersectNormal) then
      SetVector(intersectNormal^, LocalToAbsolute(locNormal));
  end;
end;

// GenerateSilhouette
//

function TGLBaseMesh.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  mc: TGLBaseMeshConnectivity;
  sil: TGLSilhouette;
begin
  sil := nil;
  if Assigned(FConnectivity) then
  begin
    mc := TGLBaseMeshConnectivity(FConnectivity);
    mc.CreateSilhouette(silhouetteParameters, sil, True);
  end
  else
  begin
    mc := TGLBaseMeshConnectivity.CreateFromMesh(Self);
    try
      mc.CreateSilhouette(silhouetteParameters, sil, True);
    finally
      mc.Free;
    end;
  end;
  Result := sil;
end;

procedure TGLBaseMesh.BuildMeshes;
var
  i, j: Integer;
begin
  FMeshObjects.BuildMeshes;

  for i := MeshObjects.Count - 1 downto 0 do
  begin
    with MeshObjects[i] do
    begin
      case mode of
        momTriangles, momTriangleStrip:
          begin
            FBatch.Transformation := @FTransformation;
            FBatch.Material := FMaterial;
          end;
        momFaceGroups:
          for j := FaceGroups.Count - 1 downto 0 do
          begin
            FaceGroups[j].FBatch.Transformation := @FTransformation;;
            if FaceGroups[j].FBatch.Material = nil then
              FaceGroups[j].FBatch.Material := FMaterial;
          end;
      end; // of case
    end;
  end;

  ApplyExtras;

  MeshObjects.GetAABB(FAABBCache);
  ClearStructureChanged;
end;

// BuildSilhouetteConnectivityData
//

procedure TGLBaseMesh.BuildSilhouetteConnectivityData;
var
  i, j: Integer;
  mo: TMeshObject;
begin
  FreeAndNil(FConnectivity);
  // connectivity data works only on facegroups of TFGVertexIndexList class
  for i := 0 to MeshObjects.Count - 1 do
  begin
    mo := (MeshObjects[i] as TMeshObject);
    if mo.mode <> momFaceGroups then
      Exit;
    for j := 0 to mo.FaceGroups.Count - 1 do
      if not mo.FaceGroups[j].InheritsFrom(TFGVertexIndexList) then
        Exit;
  end;
  FConnectivity := TGLBaseMeshConnectivity.CreateFromMesh(Self);
end;

// ------------------
// ------------------ TGLFreeForm ------------------
// ------------------

// Create
//

constructor TGLFreeForm.Create(aOwner: TComponent);
begin
  inherited;
  FUseMeshMaterials := True;
end;

// Destroy
//

destructor TGLFreeForm.Destroy;
begin
  FOctree.Free;
  inherited Destroy;
end;

// GetOctree
//

function TGLFreeForm.GetOctree: TOctree;
begin
  // if not Assigned(FOctree) then     //If auto-created, can never use "if Assigned(GLFreeform1.Octree)"
  // FOctree:=TOctree.Create;        //moved this code to BuildOctree
  Result := FOctree;
end;

// BuildOctree
//

procedure TGLFreeForm.BuildOctree(TreeDepth: Integer = 3);
var
  emin, emax: TAffineVector;
  tl: TAffineVectorList;
begin
  if not Assigned(FOctree) then // moved here from GetOctree
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

function TGLFreeForm.OctreeRayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.RayCastIntersect(locRayStart, locRayVector, intersectPoint,
    intersectNormal);
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

function TGLFreeForm.OctreePointInMesh(const Point: TVector): Boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal: TVector;
  BRad: double;
  HitCount: Integer;
  hitDot: double;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  Result := False;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    Exit;

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
      Result := HitCount > 0;
      Exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot < 0 then
      Inc(HitCount)
    else if hitDot > 0 then
      Dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector,
      BRad / cPointRadiusStep));
  end;
end;

// OctreeSphereIntersect
//

function TGLFreeForm.OctreeSphereSweepIntersect(const rayStart,
  rayVector: TVector; const velocity, radius: Single;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  locRayStart, locRayVector: TVector;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.SphereSweepIntersect(locRayStart, locRayVector, velocity,
    radius, intersectPoint, intersectNormal);
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

function TGLFreeForm.OctreeTriangleIntersect(const v1, v2,
  v3: TAffineVector): Boolean;
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

function TGLFreeForm.OctreeAABBIntersect(const aabb: TAABB;
  objMatrix, invObjMatrix: TMatrix; triangles: TAffineVectorList = nil)
  : Boolean;
var
  m1to2, m2to1: TMatrix;
begin
  Assert(Assigned(FOctree),
    'Octree must have been prepared and setup before use.');

  // get matrixes needed
  // object to self
  MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
  // self to object
  MatrixMultiply(AbsoluteMatrix, invObjMatrix, m2to1);

  Result := Octree.AABBIntersect(aabb, m1to2, m2to1, triangles);
end;

// ------------------
// ------------------ TActorAnimation ------------------
// ------------------

// Create
//

constructor TActorAnimation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TActorAnimation.Destroy;
begin
  with (Collection as TActorAnimations).FOwner do
    if FTargetSmoothAnimation = Self then
      FTargetSmoothAnimation := nil;
  inherited Destroy;
end;

// Assign
//

procedure TActorAnimation.Assign(Source: TPersistent);
begin
  if Source is TActorAnimation then
  begin
    FName := TActorAnimation(Source).FName;
    FStartFrame := TActorAnimation(Source).FStartFrame;
    FEndFrame := TActorAnimation(Source).FEndFrame;
    FReference := TActorAnimation(Source).FReference;
  end
  else
    inherited;
end;

// GetDisplayName
//

function TActorAnimation.GetDisplayName: string;
begin
  Result := Format('%d - %s [%d - %d]', [Index, Name, startFrame, endFrame]);
end;

// FrameCount
//

function TActorAnimation.FrameCount: Integer;
begin
  case reference of
    aarMorph:
      Result := TActorAnimations(Collection)
        .FOwner.MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := TActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
  else
    Result := 0;
    Assert(False);
  end;
end;

// SetStartFrame
//

procedure TActorAnimation.SetStartFrame(const val: Integer);
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

procedure TActorAnimation.SetEndFrame(const val: Integer);
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

procedure TActorAnimation.SetReference(val: TActorAnimationReference);
begin
  if val <> FReference then
  begin
    FReference := val;
    startFrame := startFrame;
    endFrame := endFrame;
  end;
end;

// SetAsString
//

procedure TActorAnimation.SetAsString(const val: string);
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
        reference := aarMorph
      else if LowerCase(sl[3]) = 'skeleton' then
        reference := aarSkeleton
      else
        Assert(False);
    end
    else
      reference := aarMorph;
  finally
    sl.Free;
  end;
end;

// GetAsString
//

function TActorAnimation.GetAsString: string;
const
  cAARToString: array [aarMorph .. aarSkeleton] of string = ('morph',
    'skeleton');
begin
  Result := Format('"%s",%d,%d,%s', [FName, FStartFrame, FEndFrame,
    cAARToString[reference]]);
end;

// OwnerActor
//

function TActorAnimation.OwnerActor: TGLActor;
begin
  Result := ((Collection as TActorAnimations).GetOwner as TGLActor);
end;

// MakeSkeletalTranslationStatic
//

procedure TActorAnimation.MakeSkeletalTranslationStatic;
begin
  OwnerActor.Skeleton.MakeSkeletalTranslationStatic(startFrame, endFrame);
end;

// MakeSkeletalRotationDelta
//

procedure TActorAnimation.MakeSkeletalRotationDelta;
begin
  OwnerActor.Skeleton.MakeSkeletalRotationDelta(startFrame, endFrame);
end;

// ------------------
// ------------------ TActorAnimations ------------------
// ------------------

// Create
//

constructor TActorAnimations.Create(aOwner: TGLActor);
begin
  FOwner := aOwner;
  inherited Create(TActorAnimation);
end;

// GetOwner
//

function TActorAnimations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TActorAnimations.SetItems(index: Integer; const val: TActorAnimation);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TActorAnimations.GetItems(index: Integer): TActorAnimation;
begin
  Result := TActorAnimation( inherited Items[index]);
end;

// Last
//

function TActorAnimations.Last: TActorAnimation;
begin
  if Count > 0 then
    Result := TActorAnimation( inherited Items[Count - 1])
  else
    Result := nil;
end;

// Add
//

function TActorAnimations.Add: TActorAnimation;
begin
  Result := ( inherited Add) as TActorAnimation;
end;

// FindItemID
//

function TActorAnimations.FindItemID(ID: Integer): TActorAnimation;
begin
  Result := ( inherited FindItemID(ID)) as TActorAnimation;
end;

// FindName
//

function TActorAnimations.FindName(const aName: string): TActorAnimation;
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

function TActorAnimations.FindFrame(aFrame: Integer;
  aReference: TActorAnimationReference): TActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    with Items[i] do
      if (startFrame <= aFrame) and (endFrame >= aFrame) and
        (reference = aReference) then
      begin
        Result := Items[i];
        Break;
      end;
end;

// SetToStrings
//

procedure TActorAnimations.SetToStrings(aStrings: TStrings);

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

procedure TActorAnimations.SaveToStream(aStream: TStream);
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

procedure TActorAnimations.LoadFromStream(aStream: TStream);
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

procedure TActorAnimations.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(filename, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// LoadFromFile
//

procedure TActorAnimations.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(filename, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TGLBaseAnimationControler ------------------
// ------------------

// Create
//

constructor TGLBaseAnimationControler.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnabled := True;
end;

// Destroy
//

destructor TGLBaseAnimationControler.Destroy;
begin
  SetActor(nil);
  inherited Destroy;
end;

// Notification
//

procedure TGLBaseAnimationControler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FActor) and (Operation = opRemove) then
    SetActor(nil);
  inherited;
end;

// DoChange
//

procedure TGLBaseAnimationControler.DoChange;
begin
  if Assigned(FActor) then
    FActor.NotifyChange(Self);
end;

// SetEnabled
//

procedure TGLBaseAnimationControler.SetEnabled(const val: Boolean);
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

procedure TGLBaseAnimationControler.SetActor(const val: TGLActor);
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

function TGLBaseAnimationControler.Apply(var lerpInfo
  : TBlendedLerpInfo): Boolean;
begin
  // virtual
  Result := False;
end;

// ------------------
// ------------------ TGLAnimationControler ------------------
// ------------------

// DoChange
//

procedure TGLAnimationControler.DoChange;
begin
  if AnimationName <> '' then
    inherited;
end;

// SetAnimationName
//

procedure TGLAnimationControler.SetAnimationName
  (const val: TActorAnimationName);
begin
  if FAnimationName <> val then
  begin
    FAnimationName := val;
    DoChange;
  end;
end;

// SetRatio
//

procedure TGLAnimationControler.SetRatio(const val: Single);
begin
  if FRatio <> val then
  begin
    FRatio := ClampValue(val, 0, 1);
    DoChange;
  end;
end;

// Apply
//

function TGLAnimationControler.Apply(var lerpInfo: TBlendedLerpInfo): Boolean;
var
  anim: TActorAnimation;
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
      frameIndex1 := anim.startFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else if Ratio = 1 then
    begin
      frameIndex1 := anim.endFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else
    begin
      baseDelta := anim.endFrame - anim.startFrame;
      lerpFactor := anim.startFrame + baseDelta * Ratio;
      frameIndex1 := GLScene_Base_Vector_Geometry.Trunc(lerpFactor);
      frameIndex2 := frameIndex1 + 1;
      lerpFactor := GLScene_Base_Vector_Geometry.Frac(lerpFactor);
    end;
    weight := 1;
    externalRotations := nil;
    externalQuaternions := nil;
  end;
end;

// ------------------
// ------------------ TGLActor ------------------
// ------------------

// Create
//

constructor TGLActor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FFrameInterpolation := afpLinear;
  FAnimationMode := aamNone;
  FInterval := 100; // 10 animation frames per second
  FAnimations := TActorAnimations.Create(Self);
  FControlers := nil; // created on request
  FOptions := cDefaultGLActorOptions;
end;

// Destroy
//

destructor TGLActor.Destroy;
begin
  inherited Destroy;
  FControlers.Free;
  FAnimations.Free;
end;

// Assign
//

procedure TGLActor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGLActor then
  begin
    FAnimations.Assign(TGLActor(Source).FAnimations);
    FAnimationMode := TGLActor(Source).FAnimationMode;
    Synchronize(TGLActor(Source));
  end;
end;

// RegisterControler
//

procedure TGLActor.RegisterControler(aControler: TGLBaseAnimationControler);
begin
  if not Assigned(FControlers) then
    FControlers := TList.Create;
  FControlers.Add(aControler);
  FreeNotification(aControler);
end;

// UnRegisterControler
//

procedure TGLActor.UnRegisterControler(aControler: TGLBaseAnimationControler);
begin
  Assert(Assigned(FControlers));
  FControlers.Remove(aControler);
  RemoveFreeNotification(aControler);
  if FControlers.Count = 0 then
    FreeAndNil(FControlers);
end;

// SetCurrentFrame
//

procedure TGLActor.SetCurrentFrame(val: Integer);
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
      aamPlayOnce:
        if (CurrentFrame = endFrame) and (FTargetSmoothAnimation = nil) then
          FAnimationMode := aamNone;
      aamBounceForward:
        if CurrentFrame = endFrame then
          FAnimationMode := aamBounceBackward;
      aamBounceBackward:
        if CurrentFrame = startFrame then
          FAnimationMode := aamBounceForward;
    end;
    StructureChanged;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
  end;
end;

// SetCurrentFrame
//

procedure TGLActor.SetCurrentFrameDirect(const Value: Integer);
begin
  FCurrentFrame := Value;
end;

// SetStartFrame
//

procedure TGLActor.SetStartFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> startFrame) then
    FStartFrame := val;
  if endFrame < startFrame then
    FEndFrame := FStartFrame;
  if CurrentFrame < startFrame then
    CurrentFrame := FStartFrame;
end;

// SetEndFrame
//

procedure TGLActor.SetEndFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> endFrame) then
    FEndFrame := val;
  if CurrentFrame > endFrame then
    CurrentFrame := FEndFrame;
end;

// SetReference
//

procedure TGLActor.SetReference(val: TActorAnimationReference);
begin
  if val <> reference then
  begin
    FReference := val;
    startFrame := startFrame;
    endFrame := endFrame;
    CurrentFrame := CurrentFrame;
    StructureChanged;
  end;
end;

// SetAnimations
//

procedure TGLActor.SetAnimations(const val: TActorAnimations);
begin
  FAnimations.Assign(val);
end;

// StoreAnimations
//

function TGLActor.StoreAnimations: Boolean;
begin
  Result := (FAnimations.Count > 0);
end;

// SetOptions
//

procedure TGLActor.SetOptions(const val: TGLActorOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// NextFrameIndex
//

function TGLActor.NextFrameIndex: Integer;
begin
  case AnimationMode of
    aamLoop, aamBounceForward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > endFrame then
          begin
            Result := startFrame + (Result - endFrame - 1);
            if Result > endFrame then
              Result := endFrame;
          end;
        end;
      end;
    aamNone, aamPlayOnce:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > endFrame then
            Result := endFrame;
        end;
      end;
    aamBounceBackward, aamLoopBackward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame - 1;
          if Result < startFrame then
          begin
            Result := endFrame - (startFrame - Result - 1);
            if Result < startFrame then
              Result := startFrame;
          end;
        end;
      end;
    aamExternal:
      Result := CurrentFrame; // Do nothing
  else
    Result := CurrentFrame;
    Assert(False);
  end;
end;

// NextFrame
//

procedure TGLActor.NextFrame(nbSteps: Integer = 1);
var
  n: Integer;
begin
  n := nbSteps;
  while n > 0 do
  begin
    CurrentFrame := NextFrameIndex;
    Dec(n);
    if Assigned(FOnEndFrameReached) and (CurrentFrame = endFrame) then
      FOnEndFrameReached(Self);
    if Assigned(FOnStartFrameReached) and (CurrentFrame = startFrame) then
      FOnStartFrameReached(Self);
  end;
end;

// PrevFrame
//

procedure TGLActor.PrevFrame(nbSteps: Integer = 1);
var
  Value: Integer;
begin
  Value := FCurrentFrame - nbSteps;
  if Value < FStartFrame then
  begin
    Value := FEndFrame - (FStartFrame - Value);
    if Value < FStartFrame then
      Value := FStartFrame;
  end;
  CurrentFrame := Value;
end;

// DoAnimate
//

procedure TGLActor.DoAnimate();
var
  i, k: Integer;
  nextFrameIdx: Integer;
  lerpInfos: array of TBlendedLerpInfo;
begin
  nextFrameIdx := NextFrameIndex;
  case reference of
    aarMorph:
      if nextFrameIdx >= 0 then
      begin
        case FrameInterpolation of
          afpLinear:
            MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
        else
          MeshObjects.MorphTo(CurrentFrame);
        end;
      end;
    aarSkeleton:
      if Skeleton.Frames.Count > 0 then
      begin
        if Assigned(FControlers) and (AnimationMode <> aamExternal) then
        begin
          // Blended Skeletal Lerping
          SetLength(lerpInfos, FControlers.Count + 1);
          if nextFrameIdx >= 0 then
          begin
            case FrameInterpolation of
              afpLinear:
                with lerpInfos[0] do
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
            if TGLBaseAnimationControler(FControlers[i])
              .Apply(lerpInfos[k]) then
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
    aarNone:
      ; // do nothing
  end;
end;

// BuildList
//

procedure TGLActor.BuildList(var rci: TRenderContextInfo);
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

procedure TGLActor.PrepareMesh;
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

procedure TGLActor.PrepareBuildList(var mrci: TRenderContextInfo);
begin
  // no preparation needed for actors, they don't use buildlists
end;

// FrameCount
//

function TGLActor.FrameCount: Integer;
begin
  case reference of
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

procedure TGLActor.DoProgress(const progressTime: TProgressTimes);
var
  fDelta: Single;
begin
  inherited;
  if (AnimationMode <> aamNone) and (Interval > 0) then
  begin
    if (startFrame <> endFrame) and (FrameCount > 1) then
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

procedure TGLActor.LoadFromStream(const filename: string; aStream: TStream);
begin
  if filename <> '' then
  begin
    Animations.Clear;
    inherited LoadFromStream(filename, aStream);
  end;
end;

// SwitchToAnimation
//

procedure TGLActor.SwitchToAnimation(const AnimationName: string;
  smooth: Boolean = False);
begin
  SwitchToAnimation(Animations.FindName(AnimationName), smooth);
end;

// SwitchToAnimation
//

procedure TGLActor.SwitchToAnimation(animationIndex: Integer;
  smooth: Boolean = False);
begin
  if (animationIndex >= 0) and (animationIndex < Animations.Count) then
    SwitchToAnimation(Animations[animationIndex], smooth);
end;

// SwitchToAnimation
//

procedure TGLActor.SwitchToAnimation(anAnimation: TActorAnimation;
  smooth: Boolean = False);
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
      reference := anAnimation.reference;
      startFrame := anAnimation.startFrame;
      endFrame := anAnimation.endFrame;
      CurrentFrame := startFrame;
    end;
  end;
end;

// CurrentAnimation
//

function TGLActor.CurrentAnimation: string;
var
  aa: TActorAnimation;
begin
  aa := Animations.FindFrame(CurrentFrame, reference);
  if Assigned(aa) then
    Result := aa.Name
  else
    Result := '';
end;

// Synchronize
//

procedure TGLActor.Synchronize(referenceActor: TGLActor);
begin
  if Assigned(referenceActor) then
  begin
    if referenceActor.startFrame < FrameCount then
      FStartFrame := referenceActor.startFrame;
    if referenceActor.endFrame < FrameCount then
      FEndFrame := referenceActor.endFrame;
    FReference := referenceActor.reference;
    if referenceActor.CurrentFrame < FrameCount then
      FCurrentFrame := referenceActor.CurrentFrame;
    FCurrentFrameDelta := referenceActor.CurrentFrameDelta;
    FAnimationMode := referenceActor.AnimationMode;
    FFrameInterpolation := referenceActor.FrameInterpolation;
    if referenceActor.FTargetSmoothAnimation <> nil then
      FTargetSmoothAnimation := Animations.FindName
        (referenceActor.FTargetSmoothAnimation.Name)
    else
      FTargetSmoothAnimation := nil;
    if (Skeleton.Frames.Count > 0) and
      (referenceActor.Skeleton.Frames.Count > 0) then
      Skeleton.Synchronize(referenceActor.Skeleton);
  end;
end;

function TGLActor.isSwitchingAnimation: Boolean;
begin
  Result := FTargetSmoothAnimation <> nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterVectorFileFormat('glsm', 'GLScene Mesh', TGLGLSMVectorFile);

RegisterClasses([TGLFreeForm, TGLActor, TSkeleton, TSkeletonFrame,
  TSkeletonBone, TSkeletonMeshObject, TMeshObject, TSkeletonFrameList,
  TMeshMorphTarget, TMorphableMeshObject, TFaceGroup, TFGVertexIndexList,
  TFGVertexNormalTexIndexList, TGLAnimationControler, TFGIndexTexCoordList,
  TSkeletonCollider, TSkeletonColliderList]);

finalization

FreeAndNil(vVectorFileFormats);

end.
