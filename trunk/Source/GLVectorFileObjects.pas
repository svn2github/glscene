{: GLVectorFileObjects<p>

	Vector File related objects for GLScene<p>

	<b>Historique : </b><font size=-1><ul>
      <li>14/08/01 - Egg - Added TSkeletonBoneList and support for skeleton with
                           multiple root bones, updated SMD loader 
      <li>13/08/01 - Egg - Improved/fixed SMD loader
      <li>12/08/01 - Egg - Completely rewritten handles management,
                           Fixed TActorAnimation.Assign,
                           Fixed persistence
      <li>08/08/01 - Egg - Added TBaseMesh.AxisAlignedDimensions
      <li>19/07/01 - Egg - AutoCentering is now a property of TBaseMesh,
                           3DS loader no longer auto-centers,
                           Added ExtractTriangles and related methods
      <li>18/07/01 - Egg - VisibilityCulling compatibility changes
      <li>19/06/01 - Egg - StrToFloat outlawed and replaced by StrToFloatDef
      <li>25/03/01 - Egg - Added TAnimationControler
      <li>18/03/01 - Egg - Added basic Skeleton structures & SMD importer
      <li>16/03/01 - Egg - Introduced new PersistentClasses
      <li>15/03/01 - Egg - Fix in TActorAnimation.SetEndFrame (thx David Costa)
      <li>08/03/01 - Egg - TGL3DSVectorFile now loads materials for TBaseMesh
      <li>26/02/01 - Egg - Added TBaseMeshObject & BuildNormals, MD2 normals auto-builded
      <li>21/02/01 - Egg - Now XOpenGL based (multitexture)
      <li>15/01/01 - Egg - Added Translate methods
      <li>10/01/01 - Egg - Fixed in TBaseMesh.DoRender for RenderChildren states
      <li>08/01/01 - Egg - Fixed TBaseMesh.BuildList messup of attrib states
      <li>22/12/00 - Egg - Fixed non-interpolated TActor animation (was freezing),
                           Fixed TBaseMesh.DoRender messup of attrib states
      <li>18/12/00 - Egg - TFGIndexTexCoordList now supports normals (automatically),
                           NormalsOrientation code moved to TBaseMesh
      <li>11/12/00 - Egg - Fix for NormalOrientation (3DS importer)
      <li>06/12/00 - Egg - Added PrepareBuildList mechanism
      <li>08/10/00 - Egg - Removed TGLOBJVectorFile, use GLFileOBJ instead
      <li>13/08/00 - Egg - Enhancements for Portal Rendering support,
                           Added utility methods & triangle fans
      <li>10/08/00 - Egg - Added CurrentAnimation, fixed TMeshObject.GetExtents
      <li>21/07/00 - Egg - Vastly improved memory use and mechanisms for MD2/TActor
      <li>19/07/00 - Egg - Introduced enhanced mesh structure
      <li>16/07/00 - Egg - Made use of new TDataFile class
      <li>15/07/00 - Egg - FreeForm can now handle 3DS files with multiple textures,
                           Added TBaseMesh.GetExtents
      <li>28/06/00 - Egg - Support for "ObjectStyle"
      <li>23/06/00 - Egg - Reversed "t" texture coord for MD2,
                           TActorAnimations can now load/save
      <li>21/06/00 - Egg - Added frame change events to TActor,
                           Added TActorAnimations collection
      <li>19/06/00 - Egg - Completed smooth movement interpolation for TActor
      <li>07/06/00 - Egg - TVectorFile now longers assumes a TFreeForm as Owner,
                           Added generic TVectorFile.LoadFromFile
      <li>26/05/00 - Egg - Removed dependency to GLObjects,
                           TFreeForm now may use InterleavedArrays instead of
                           IndexedArrays (better BuildList compatibility)
      <li>22/04/00 - Egg - Fixed Material handlings in TFreeForm, inverted CCW/CW
                           convention for 3DS Release3
		<li>11/04/00 - Egg - Removed unnecessary code in finalization (thanks Uwe)
	   <li>09/02/00 - Egg - Creation from split of GLObjects,
                           fixed class registrations and formats unregistration
	</ul></font>
}
unit GLVectorFileObjects;

interface

uses Windows, Classes, GLScene, OpenGL12, Geometry, SysUtils, GLMisc, GLTexture,
   GLMesh, VectorLists, PersistentClasses;

type

   TMeshObjectList = class;
   TFaceGroups = class;

   // TMeshAutoCentering
   //
   TMeshAutoCentering = (macCenterX, macCenterY, macCenterZ);
   TMeshAutoCenterings = set of TMeshAutoCentering;

   // TMeshObjectMode
   //
   TMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

   // TBaseMeshObject
   //
   {: A base class for mesh objects. }
   TBaseMeshObject = class (TPersistentObject)
      private
         { Private Declarations }
         FName : String;
         FVertices : TAffineVectorList;
         FNormals : TAffineVectorList;

      protected
         { Protected Declarations }
         procedure SetVertices(const val : TAffineVectorList);
         procedure SetNormals(const val : TAffineVectorList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         {: Translates all the vertices by the given delta. }
         procedure Translate(const delta : TAffineVector); dynamic;
         {: Builds (smoothed) normals for the vertex list.<p>
            If normalIndices is nil, the method assumes a bijection between
            vertices and normals sets, and when exiting Normals and Vertices
            list will have the same number of items (whatever previously was in
            the Normals list is ignored/removed).<p>
            If normalIndices is defined, normals will be added to the list and
            their indices will be added to normalIndices. Already defined
            normals and indices are preserved.<p>
            The only valid modes are currently momTriangles and momTriangleStrip }
         procedure BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                normalIndices : TIntegerList = nil);
         {: Extracts all mesh triangles as a triangles list.<p>
            The resulting list size is a multiple of 3, each group of 3 vertices
            making up and independant triangle.<br>
            The returned list can be used independantly from the mesh object
            (all data is duplicated) and should be freed by caller. }
         function ExtractTriangles : TAffineVectorList; dynamic;

         property Name : String read FName write FName;
         property Vertices : TAffineVectorList read FVertices write SetVertices;
         property Normals : TAffineVectorList read FNormals write SetNormals;
   end;

   TSkeletonFrameList = class;

	// TSkeletonFrame
	//
   {: Stores position and rotation for skeleton joints.<p>
      If you directly alter some values, make sure to call FlushLocalMatrixList
      so that the local matrices will be recalculated (the call to Flush does
      not recalculate the matrices, but marks the current ones as dirty). }
	TSkeletonFrame = class (TPersistentObject)
	   private
	      { Private Declarations }
         FOwner : TSkeletonFrameList;
         FName : String;
         FPosition : TAffineVectorList;
         FRotation : TAffineVectorList;
         FLocalMatrixList : PMatrixArray;

	   protected
	      { Protected Declarations }
         procedure SetPosition(const val : TAffineVectorList);
         procedure SetRotation(const val : TAffineVectorList);

	   public
	      { Public Declarations }
         constructor CreateOwned(aOwner : TSkeletonFrameList);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TSkeletonFrameList read FOwner;
         property Name : String read FName write FName;
         {: Position values for the joints. }
         property Position : TAffineVectorList read FPosition write SetPosition;
         {: Rotation values for the joints. }
         property Rotation : TAffineVectorList read FRotation write SetRotation;

         {: Calculate or retrieves an array of local bone matrices.<p>
            This array is calculated on the first call after creation, and the
            first call following a FlushLocalMatrixList. Subsequent calls return
            the same arrays. }
         function LocalMatrixList : PMatrixArray;
         {: Flushes (frees) then LocalMatrixList data.<p>
            Call this function to allow a recalculation of local matrices. }
         procedure FlushLocalMatrixList;
	end;

   // TSkeletonFrameList
   //
   {: A list of TSkeletonFrame objects. }
   TSkeletonFrameList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetSkeletonFrame(Index: Integer) : TSkeletonFrame;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TPersistent);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TSkeletonFrame read GetSkeletonFrame; default;
   end;

   TSkeleton = class;
   TSkeletonBone = class;

	// TSkeletonBoneList
	//
   {: A list of skeleton bones.<p> }
	TSkeletonBoneList = class (TPersistentObjectList)
	   private
	      { Private Declarations }
         FSkeleton : TSkeleton;     // not persistent

	   protected
	      { Protected Declarations }
         function GetSkeletonBone(Index: Integer) : TSkeletonBone;
         procedure AfterObjectCreatedByReader(Sender : TObject); override;

	   public
	      { Public Declarations }
	      constructor CreateOwned(aOwner : TSkeleton);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Skeleton : TSkeleton read FSkeleton;
         property Items[Index: Integer] : TSkeletonBone read GetSkeletonBone; default;

         {: Returns a bone by its BoneID, nil if not found. }
         function BoneByID(anID : Integer) : TSkeletonBone; virtual;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); virtual; abstract;
         procedure PrepareGlobalMatrices; virtual;
	end;

	// TSkeletonRootBoneList
	//
   {: This list store skeleton root bones exclusively.<p> }
	TSkeletonRootBoneList = class (TSkeletonBoneList)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); override;
   end;


	// TSkeletonBone
	//
   {: A skeleton bone or node and its children.<p>
      This class is the base item of the bones hierarchy in a skeletal model.
      The joint values are stored in a TSkeletonFrame, but the calculated bone
      matrices are stored here. }
	TSkeletonBone = class (TSkeletonBoneList)
	   private
	      { Private Declarations }
         FOwner : TSkeletonBoneList;    // indirectly persistent
         FBoneID : Integer;
         FName : String;
         FColor : Cardinal;
         FGlobalMatrix : TMatrix;

	   protected
	      { Protected Declarations }
         function GetSkeletonBone(Index: Integer) : TSkeletonBone;
         procedure SetColor(const val : Cardinal);

	   public
	      { Public Declarations }
	      constructor CreateOwned(aOwner : TSkeletonBoneList);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); override;

         property Owner : TSkeletonBoneList read FOwner;
         property Skeleton : TSkeleton read FSkeleton;
         property Name : String read FName write FName;
         property BoneID : Integer read FBoneID write FBoneID;
         property Color : Cardinal read FColor write SetColor;
         property Items[Index: Integer] : TSkeletonBone read GetSkeletonBone; default;

         {: Returns a bone by its BoneID, nil if not found. }
         function BoneByID(anID : Integer) : TSkeletonBone; override;

         {: Calculates the global matrix for the bone and its sub-bone.<p>
            Call this function directly only the RootBone. }
         procedure PrepareGlobalMatrices; override;
         {: Global Matrix for the bone in the current frame.<p>
            Global matrices must be prepared by invoking PrepareGlobalMatrices
            on the root bone. }
         property GlobalMatrix : TMatrix read FGlobalMatrix;

         {: Free all sub bones and reset BoneID and Name. }
         procedure Clean; override;
	end;

   TBaseMesh = class;

   // TBlendedLerpInfo
   //
   {: Small structure to store a weighted lerp for use in blending. }
   TBlendedLerpInfo = record
      frameIndex1, frameIndex2 : Integer;
      lerpFactor : Single;
      weight : Single;
   end;

	// TSkeleton
	//
   {: Main skeleton object.<p>
      This class stores the bones hierarchy and animation frames.<br>
      It is also responsible for maintaining the "CurrentFrame" and allowing
      various frame blending operations. }
	TSkeleton = class (TPersistentObject)
	   private
	      { Private Declarations }
         FOwner : TBaseMesh;
         FRootBones : TSkeletonRootBoneList;
         FFrames : TSkeletonFrameList;
         FCurrentFrame : TSkeletonFrame; // not persistent
         FBonesByIDCache : TList;

	   protected
	      { Protected Declarations }
         procedure SetRootBones(const val : TSkeletonRootBoneList);
         procedure SetFrames(const val : TSkeletonFrameList);
         function GetCurrentFrame : TSkeletonFrame;
         procedure SetCurrentFrame(val : TSkeletonFrame);

	   public
	      { Public Declarations }
         constructor CreateOwned(AOwner : TBaseMesh);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TBaseMesh read FOwner;
         property RootBones : TSkeletonRootBoneList read FRootBones write SetRootBones;
         property Frames : TSkeletonFrameList read FFrames write SetFrames;
         property CurrentFrame : TSkeletonFrame read GetCurrentFrame write SetCurrentFrame;

         procedure FlushBoneByIDCache;
         function BoneByID(anID : Integer) : TSkeletonBone;

         procedure MorphTo(frameIndex : Integer);
         procedure Lerp(frameIndex1, frameIndex2 : Integer;
                        lerpFactor : Single);
         procedure BlendedLerps(const lerpInfos : array of TBlendedLerpInfo);

         {: Applies current frame to morph all mesh objects. }
         procedure MorphMesh;

         {: Release bones and frames info. }
         procedure Clear;
	end;

   // TMeshObject
   //
   {: Base mesh class.<p>
      Introduces base methods and properties for mesh objects.<p>
      Subclasses are named "TMOxxx". }
   TMeshObject = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshObjectList;
         FTexCoords : TAffineVectorList; // provision for 3D textures
         FColors : TVectorList;
         FFaceGroups: TFaceGroups;
         FMode : TMeshObjectMode;
         FArraysDeclared : Boolean; // not persistent

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);
         procedure SetColors(const val : TVectorList);

         procedure DeclareArraysToOpenGL(evenIfAlreadyDeclared : Boolean = False);

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObjectList);
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         function ExtractTriangles : TAffineVectorList; override;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         //: The extents of the object (min and max coordinates)
         procedure GetExtents(var min, max : TAffineVector); dynamic;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         function PointInObject(const aPoint : TAffineVector) : Boolean; virtual;

         property Owner : TMeshObjectList read FOwner;
         property Mode : TMeshObjectMode read FMode write FMode;
         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
         property Colors : TVectorList read FColors write SetColors;
         property FaceGroups : TFaceGroups read FFaceGroups;
   end;

   // TMeshObjectList
   //
   {: A list of TMeshObject objects. }
   TMeshObjectList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TBaseMesh;

      protected
         { Protected Declarations }
         function GetMeshObject(Index: Integer) : TMeshObject;

      public
         { Public Declarations }
         constructor CreateOwned(aOwner : TBaseMesh);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);
         function MorphTargetCount : Integer;

         procedure GetExtents(var min, max : TAffineVector);
         procedure Translate(const delta : TAffineVector);
         function ExtractTriangles : TAffineVectorList;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         property Owner : TBaseMesh read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshObject read GetMeshObject; default;
   end;

   TMeshObjectListClass = class of TMeshObjectList;

   TMeshMorphTargetList = class;

   // TMeshMorphTarget
   //
   {: A morph target, stores alternate lists of vertices and normals. }
   TMeshMorphTarget = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshMorphTargetList);
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TMeshMorphTargetList read FOwner;
   end;

   // TMeshMorphTargetList
   //
   {: A list of TMeshMorphTarget objects. }
   TMeshMorphTargetList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetMeshMorphTarget(Index: Integer) : TMeshMorphTarget;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TPersistent);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure Translate(const delta : TAffineVector);

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshMorphTarget read GetMeshMorphTarget; default;
   end;

   // TMorphableMeshObject
   //
   {: Mesh object with support for morph targets.<p>
      The morph targets allow to change vertices and normals according to pre-
      existing "morph targets". }
   TMorphableMeshObject = class (TMeshObject)
      private
         { Private Declarations }
         FMorphTargets : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         procedure Translate(const delta : TAffineVector); override;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);

         property MorphTargets : TMeshMorphTargetList read FMorphTargets;
   end;

   // TVertexBoneWeight
   //
   TVertexBoneWeight = packed record
      BoneID : Integer;
      Weight : Single;
   end;

   TVertexBoneWeightArray = array [0..MaxInt shr 4] of TVertexBoneWeight;
   PVertexBoneWeightArray = ^TVertexBoneWeightArray;
   TVerticesBoneWeights = array [0..MaxInt shr 3] of PVertexBoneWeightArray;
   PVerticesBoneWeights = ^TVerticesBoneWeights;

	// TSkeletonMeshObject
	//
   {: A mesh object with vertice bone attachments.<p>
      The class adds per vertex bone weights to the standard morphable mesh.<br>
      The TVertexBoneWeight structures are accessed via VerticesBonesWeights,
      they must be initialized by adjusting the BonesPerVertex and
      VerticeBoneWeightCount properties, you can also add vertex by vertex
      by using the AddWeightedBone method.<p>
      When BonesPerVertex is 1, the weight is ignored (set to 1.0). }
	TSkeletonMeshObject = class (TMorphableMeshObject)
	   private
	      { Private Declarations }
         FVerticesBonesWeights : PVerticesBoneWeights;
         FVerticeBoneWeightCount : Integer;
         FBonesPerVertex : Integer;
         FLastVerticeBoneWeightCount, FLastBonesPerVertex : Integer; // not persistent
         FBoneMatrixInvertedMeshes : TList;

	   protected
	      { Protected Declarations }
         procedure SetVerticeBoneWeightCount(const val : Integer);
         procedure SetBonesPerVertex(const val : Integer);
	      procedure ResizeVerticesBonesWeights;

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property VerticesBonesWeights : PVerticesBoneWeights read FVerticesBonesWeights;
         property VerticeBoneWeightCount : Integer read FVerticeBoneWeightCount write SetVerticeBoneWeightCount;
         property BonesPerVertex : Integer read FBonesPerVertex write SetBonesPerVertex;

         function FindOrAdd(boneID : Integer; const vertex, normal : TAffineVector) : Integer;

         procedure AddWeightedBone(aBoneID : Integer; aWeight : Single);
         procedure PrepareBoneMatrixInvertedMeshes;
         procedure ApplyCurrentSkeletonFrame;
	      procedure Clear;
	end;

   // TFaceGroup
   //
   {: Describes a face group of a TMeshObject.<p>
      Face groups should be understood as "a way to use mesh data to render
      a part or the whole mesh object".<p>
      Subclasses implement the actual behaviours, and should have at least
      one "Add" method, taking in parameters all that is required to describe
      a single base facegroup element. }
   TFaceGroup = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TFaceGroups;
         FMaterialName : String;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TFaceGroups); virtual;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         {: Add to the list the triangles corresponding to the facegroup.<p>
            This function is used by TMeshObjects ExtractTriangles to retrieve
            all the triangles in a mesh. }
         procedure AddToTriangles(aList : TAffineVectorList); dynamic;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         property Owner : TFaceGroups read FOwner;
         property MaterialName : String read FMaterialName write FMaterialName;
   end;

   // TFaceGroupMeshMode
   //
   {: Known descriptions for face group mesh modes.<p>
      - fgmmTriangles : issue all vertices with GL_TRIANGLES<br>
      - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP<br>
      - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
         the same normal for all vertices of a triangle.<br>
      - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN }
   TFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
                         fgmmTriangleFan);

   // TFGVertexIndexList
   //
   {: A face group based on an indexlist.<p>
      The index list refers to items in the mesh object (vertices, normals, etc.),
      that are all considered in sync, the render is obtained issueing the items
      in the order given by the vertices.<p> }
   TFGVertexIndexList = class (TFaceGroup)
      private
         { Private Declarations }
         FVertexIndices : TIntegerList;
         FMode : TFaceGroupMeshMode;

      protected
         { Protected Declarations }
         procedure SetVertexIndices(const val : TIntegerList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList); override;

         procedure Add(idx : Integer);
         procedure GetExtents(var min, max : TAffineVector);
         //: Return the normal from the 1st three points in the facegroup
         function  GetNormal : TAffineVector;

         property Mode : TFaceGroupMeshMode read FMode write FMode;
         property VertexIndices : TIntegerList read FVertexIndices write SetVertexIndices;
   end;

   // TFGVertexNormalTexIndexList
   //
   {: Adds normals and texcoords indices.<p>
      Allows very compact description of a mesh. }
   TFGVertexNormalTexIndexList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FNormalIndices : TIntegerList;
         FTexCoordIndices : TIntegerList;

      protected
         { Protected Declarations }
         procedure SetNormalIndices(const val : TIntegerList);
         procedure SetTexCoordIndices(const val : TIntegerList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList); override;

         procedure Add(vertexIdx, normalIdx, texCoordIdx : Integer);

         property NormalIndices : TIntegerList read FNormalIndices write SetNormalIndices;
         property TexCoordIndices : TIntegerList read FTexCoordIndices write SetTexCoordIndices;
   end;

   // TFGIndexTexCoordList
   //
   {: Adds per index texture coordinates to its ancestor.<p>
      Per index texture coordinates allows having different texture coordinates
      per triangle, depending on the face it is used in. }
   TFGIndexTexCoordList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FTexCoords : TAffineVectorList;

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList); override;

         procedure Add(idx : Integer; const texCoord : TAffineVector); overload;
         procedure Add(idx : Integer; const s, t : Single); overload;

         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
   end;

   // TFaceGroups
   //
   {: A list of TFaceGroup objects. }
   TFaceGroups = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TMeshObject;

      protected
         { Protected Declarations }
         function GetFaceGroup(Index: Integer) : TFaceGroup;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObject);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TMeshObject read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TFaceGroup read GetFaceGroup; default;

         procedure AddToTriangles(aList : TAffineVectorList);

         //: Sort faces by material, those without material first in list
         procedure SortByMaterial;
   end;

   // TMeshNormalsOrientation
   //
   {: Determines how normals orientation is defined in a mesh.<p>
      - mnoDefault : uses default orientation<br>
      - mnoInvert : inverse of default orientation<br>
      - mnoAutoSolid : autocalculate to make the mesh globally solid<br>
      - mnoAutoHollow : autocalculate to make the mesh globally hollow<br> }
   TMeshNormalsOrientation = (mnoDefault, mnoInvert); //, mnoAutoSolid, mnoAutoHollow);

   // TVectorFile
   //
   {: Abstract base class for different vector file formats.<p>
      The actual implementation for these files (3DS, DXF..) must be done
      seperately. The concept for TVectorFile is very similar to TGraphic
      (see Delphi Help). }
   TVectorFile = class (TDataFile)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;

      protected
         { Protected Declarations }
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation); virtual;

      public
         { Public Declarations }
         constructor Create(AOwner: TPersistent); virtual;

         function Owner : TBaseMesh;

         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation;
   end;

   TVectorFileClass = class of TVectorFile;

   // TGL3DSVectorFile
   //
   {: The 3DStudio vector file.<p>
      Uses 3DS import library by Mike Lischke (http://www.lishcke-online.de).<p>
      A 3DS file may contain material information and require textures when
      loading. Only the primary texture map is used by GLScene, transparency,
      bump mapping, etc. are ignored as of now. }
   TGL3DSVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLMD2VectorFile
   //
   {: The MD2 vector file (Quake2 actor file).<p>
      Stores a set of "frames" describing the different postures of the actor,
      it may be animated by TActor. The "Skin" must be loaded indepentendly
      (the whole mesh uses a single texture bitmap).<p>
      Based on code by Roger Cao. }
   TGLMD2VectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLTINVectorFile
   //
   {: The TIN vector file (triangle irregular network).<p>
      It is a simple text format, with one triangle record per line, no materials,
      no texturing (there may be more, but I never saw anything in this files).<p>
      This format is encountered in the DEM/DTED world and used in place of grids. }
   TGLTINVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLSTLVectorFile
   //
   {: The STL vector file (stereolithography format).<p>
      It is a list of the triangular surfaces that describe a computer generated
      solid model. This is the standard input for most rapid prototyping machines.<p>
      There are two flavors of STL, the "text" and the "binary", this reader
      supports only the "binary" version.<p>
      Original code by Paul M. Bearne. }
   TGLSTLVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLSMDVectorFile
   //
   {: The SMD vector file is Half-life's skeleton format.<p>
      The SMD is a text-based file format. They come in two flavors: one that
      old Skeleton and triangle (mesh) data, and animation files that store
      Skeleton frames.<p>
      This reader curently reads both, but requires that the main file
      (the one with mesh data) be read first. }
   TGLSMDVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TBaseMesh
   //
   {: Base class for mesh objects. }
   TBaseMesh = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;
         FMaterialLibrary : TGLMaterialLibrary;
         FUseMeshMaterials : Boolean;
         FOverlaySkeleton : Boolean;
         FAutoCentering : TMeshAutoCenterings;
         FAxisAlignedDimensionsCache : TVector;

      protected
         { Protected Declarations }
         FMeshObjects : TMeshObjectList;     // a list of mesh objects
         FSkeleton : TSkeleton;              // skeleton data & frames
         procedure SetUseMeshMaterials(const val : Boolean);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation);
         procedure SetOverlaySkeleton(const val : Boolean);

         procedure DestroyHandles; override;

         {: Invoked after creating a TVectorFile and before loading.<p>
            Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
            Allows to adjust/transfer subclass-specific features. }
         procedure PrepareVectorFile(aFile : TVectorFile); dynamic;


         {: Invoked after a mesh has been loaded.<p>
            Should auto-center according to the AutoCentering property. }
         procedure PerformAutoCentering; dynamic;
         {: Invoked after a mesh has been loaded/added.<p>
            Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
            Allows to adjust/transfer subclass-specific features. }
         procedure PrepareMesh; dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         function AxisAlignedDimensions : TVector; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
			procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
         procedure StructureChanged; override;

         property MeshObjects : TMeshObjectList read FMeshObjects;
         property Skeleton : TSkeleton read FSkeleton;

         {: Calculates the extents of a mesh.<p> }
         procedure GetExtents(var min, max : TAffineVector);

         {: Loads a vector file.<p>
            A vector files (for instance a ".3DS") stores the definition of
            a mesh as well as materials property.<p>
            Loading a file replaces the current one (if any). }
         procedure LoadFromFile(const filename : String); dynamic;
         {: Loads a vector file from a stream.<p>
            See LoadFromFile.<br>
            The filename attribute is required to identify the type data you're
            streaming (3DS, OBJ, etc.) }
         procedure LoadFromStream(const filename : String; aStream : TStream); dynamic;

         {: Loads additionnal data from a file.<p>
            Additionnal data could be more animation frames or morph target.<br>
            The VectorFile importer must be able to handle addition of data
            flawlessly. }
         procedure AddDataFromFile(const filename : String); dynamic;
         {: Loads additionnal data from stream.<p>
            See AddDataFromFile. }
         procedure AddDataFromStream(const filename : String; aStream : TStream); dynamic;

         {: Determines if a mesh should be centered and how.<p>
            AutoCentering is performed <b>only</b> after loading a mesh, it has
            no effect on already loaded mesh data or when adding from a file/stream.<br>
            If you want to alter mesh data, use direct manipulation methods
            (on the TMeshObjects). }
         property AutoCentering : TMeshAutoCenterings read FAutoCentering write FAutoCentering default [];

         {: Material library where mesh materials will be stored/retrieved.<p>
            If this property is not defined or if UseMeshMaterials is false,
            only the FreeForm's material will be used (and the mesh's materials
            will be ignored. }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         {: Defines wether materials declared in the vector file mesh are used.<p>
            You must also define the MaterialLibrary property. }
         property UseMeshMaterials : Boolean read FUseMeshMaterials write SetUseMeshMaterials default True;

         {: Normals orientation for owned mesh.<p> }
         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation default mnoDefault;

         {: Request rendering of skeleton bones over the mesh. }
         property OverlaySkeleton : Boolean read FOverlaySkeleton write SetOverlaySkeleton;
   end;

   // TFreeForm
   //
   {: Container objects for a vector file mesh.<p>
      FreeForms allows loading and rendering vector files (like 3DStudio
      ".3DS" file) in GLScene. Meshes can be loaded with the LoadFromFile
      method.<p>
      A FreeForm may contain more than one mesh, but they will all be handled
      as a single object in a scene. }
   TFreeForm = class (TBaseMesh)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;


      published
         { Published Declarations }
         property AutoCentering;
         property MaterialLibrary;
         property UseMeshMaterials;
         property NormalsOrientation;
   end;

   // TActorAnimationReference
   //
   TActorAnimationReference = (aarMorph, aarSkeleton);

	// TActorAnimation
	//
   {: An actor animation sequence.<p>
      An animation sequence is a named set of contiguous frames that can be used
      for animating an actor. The referred frames can be either morph or skeletal
      frames (choose which via the Reference property).<p>
      An animation can be directly "played" by the actor by selecting it with
      SwitchAnimation, and can also be "blended" via a TAnimationControler. }
	TActorAnimation = class (TCollectionItem)
	   private
	      { Private Declarations }
         FName : String;
         FStartFrame : Integer;
         FEndFrame : Integer;
         FReference : TActorAnimationReference;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         function FrameCount : Integer;
         procedure SetStartFrame(const val : Integer);
         procedure SetEndFrame(const val : Integer);
         procedure SetReference(val : TActorAnimationReference);
         procedure SetAsString(const val : String);
         function GetAsString : String;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         property AsString : String read GetAsString write SetAsString;

	   published
	      { Published Declarations }
         property Name : String read FName write FName;
         property StartFrame : Integer read FStartFrame write SetStartFrame;
         property EndFrame : Integer read FEndFrame write SetEndFrame;
         property Reference : TActorAnimationReference read FReference write SetReference default aarMorph;
	end;

   TActorAnimationName = String;
   TActor = class;

	// TActorAnimations
	//
   {: Collection of actor anuimations sequences. }
	TActorAnimations = class (TCollection)
	   private
	      { Private Declarations }
	      owner : TActor;

	   protected
	      { Protected Declarations }
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TActorAnimation);
	      function GetItems(index : Integer) : TActorAnimation;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TActor);
         function Add: TActorAnimation;
	      function FindItemID(ID: Integer): TActorAnimation;
	      function FindName(const aName : String) : TActorAnimation;
         function FindFrame(aFrame : Integer; aReference : TActorAnimationReference) : TActorAnimation;

	      procedure SetToStrings(aStrings : TStrings);
         procedure SaveToStream(aStream : TStream);
         procedure LoadFromStream(aStream : TStream);
         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);

	      property Items[index : Integer] : TActorAnimation read GetItems write SetItems; default;
   end;

	// TAnimationControler
	//
	TAnimationControler = class (TComponent)
	   private
	      { Private Declarations }
         FActor : TActor;
         FAnimationName : TActorAnimationName;

	   protected
	      { Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetActor(const val : TActor);
         procedure SetAnimationName(const val : TActorAnimationName);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         { Published Declarations }
         property Actor : TActor read FActor write SetActor;
         property AnimationName : String read FAnimationName write SetAnimationName;
	end;

   // TActorFrameInterpolation
   //
   {: Actor frame-interpolation mode.<p>
      - afpNone : no interpolation, display CurrentFrame only<br>
      - afpLinear : perform linear interpolation between current and next frame }
   TActorFrameInterpolation = (afpNone, afpLinear);

   // TActorActionMode
   //
   {: Defines how an actor plays between its StartFrame and EndFrame.<p>
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
      </ul> }
   TActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
                          aamBounceBackward);

   // TActor
   //
   {: Mesh class specialized in animated meshes.<p>
      The TActor provides a quick interface to animated meshes based on morph
      or skeleton frames, it is capable of performing frame interpolation and
      animation blending (via TAnimationControler components). }
   TActor = class (TBaseMesh)
      private
         { Private Declarations }
         FStartFrame, FEndFrame : Integer;
         FReference : TActorAnimationReference;
         FCurrentFrame : Integer;
         FCurrentFrameDelta : Single;
         FFrameInterpolation : TActorFrameInterpolation;
         FInterval : Integer;
         FAnimationMode : TActorAnimationMode;
         FOnFrameChanged : TNotifyEvent;
         FOnEndFrameReached, FOnStartFrameReached : TNotifyEvent;
         FAnimations : TActorAnimations;

      protected
         { Protected Declarations }
         procedure SetCurrentFrame(val : Integer);
         procedure SetStartFrame(val : Integer);
         procedure SetEndFrame(val : Integer);
         procedure SetReference(val : TActorAnimationReference);
         procedure SetAnimations(const val : TActorAnimations);

         procedure PrepareMesh; override;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;

			procedure DoProgress(const deltaTime, newTime : Double); override;

	      procedure SwitchToAnimation(anAnimation : TActorAnimation); overload;
	      procedure SwitchToAnimation(const animationName : String); overload;
	      procedure SwitchToAnimation(animationIndex : Integer); overload;
         function CurrentAnimation : String;

         {: Synchronize self animation with an other actor.<p>
            Copies Start/Current/End Frame values, CurrentFrameDelta,
            AnimationMode and FrameInterpolation. }
         procedure Synchronize(referenceActor : TActor);

         function  NextFrameIndex : Integer;

         procedure NextFrame(nbSteps : Integer = 1);
         procedure PrevFrame(nbSteps : Integer = 1);

         function FrameCount : Integer;

      published
         { Published Declarations }
         property StartFrame : Integer read FStartFrame write SetStartFrame;
         property EndFrame : Integer read FEndFrame write SetEndFrame;
         property Reference : TActorAnimationReference read FReference write FReference default aarMorph;

         property CurrentFrame : Integer read FCurrentFrame write SetCurrentFrame;
         {: Value in the [0; 1] range expressing the delta to the next frame.<p> }
         property CurrentFrameDelta : Single read FCurrentFrameDelta write FCurrentFrameDelta;

         property FrameInterpolation : TActorFrameInterpolation read FFrameInterpolation write FFrameInterpolation default afpLinear;

         {: See TActorAnimationMode.<p> }
         property AnimationMode : TActorAnimationMode read FAnimationMode write FAnimationMode default aamNone;
         {: Interval between frames, in milliseconds. }
         property Interval : Integer read FInterval write FInterval;

         {: Triggered after each CurrentFrame change. }
         property OnFrameChanged : TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
         {: Triggered after EndFrame has been reached by progression or "nextframe" }
         property OnEndFrameReached : TNotifyEvent read FOnEndFrameReached write FOnEndFrameReached;
         {: Triggered after StartFrame has been reached by progression or "nextframe" }
         property OnStartFrameReached : TNotifyEvent read FOnStartFrameReached write FOnStartFrameReached;

         {: Collection of animations sequences. }
         property Animations : TActorAnimations read FAnimations write SetAnimations;

         property AutoCentering;
         property MaterialLibrary;
         property UseMeshMaterials;
         property NormalsOrientation;
         property OverlaySkeleton;
   end;

   PVectorFileFormat = ^TVectorFileFormat;
   TVectorFileFormat = record
      VectorFileClass : TVectorFileClass;
      Extension       : String;
      Description     : String;
      DescResID       : Integer;
   end;

   // TVectorFileFormatsList
   //
   TVectorFileFormatsList = class(TList)
      public
         { Public Declarations }
         destructor Destroy; override;

         procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TVectorFileClass);
         function FindExt(ext : string) : TVectorFileClass;
         function FindFromFileName(const fileName : String) : TVectorFileClass;
         procedure Remove(AClass: TVectorFileClass);
         procedure BuildFilterStrings(VectorFileClass: TVectorFileClass; var Descriptions, Filters: string);
   end;

   EInvalidVectorFile = class(Exception);

function GetVectorFileFormats : TVectorFileFormatsList;
procedure RegisterVectorFileFormat(const aExtension, aDescription: String;
                                   aClass : TVectorFileClass);
procedure UnregisterVectorFileClass(aClass : TVectorFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLStrings, consts, XOpenGL,
     // 3DS Support
	  File3DS, Types3DS,
     // STL Support
     TypesSTL,
     // MD2 Support
	  FileMD2, TypesMD2;

var
   vVectorFileFormats : TVectorFileFormatsList;

const
   cAAFHeader = 'AAF';

function GetVectorFileFormats: TVectorFileFormatsList;
begin
   if not Assigned(vVectorFileFormats)then
      vVectorFileFormats:=TVectorFileFormatsList.Create;
   Result:=vVectorFileFormats;
end;

procedure RegisterVectorFileFormat(const AExtension, ADescription: String; AClass: TVectorFileClass);
begin
   RegisterClass(AClass);
	GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterVectorFileClass(AClass: TVectorFileClass);
begin
	if Assigned(vVectorFileFormats) then
		vVectorFileFormats.Remove(AClass);
end;

//----------------- vector format support --------------------------------------

destructor TVectorFileFormatsList.Destroy;

var I: Integer;

begin
  for I:=0 to Count - 1 do Dispose(PVectorFileFormat(Items[I]));
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TVectorFileClass);

var NewRec: PVectorFileFormat;

begin
  New(NewRec);
  with NewRec^ do
  begin
    Extension:=AnsiLowerCase(Ext);
    VectorFileClass:=AClass;
    Description:=Desc;
    DescResID:=DescID;
  end;
  inherited Add(NewRec);
end;

// FindExt
//
function TVectorFileFormatsList.FindExt(ext : String) : TVectorFileClass;
var
   i : Integer;
begin
   ext:=AnsiLowerCase(ext);
   for i:=Count-1 downto 0 do
      with PVectorFileFormat(Items[I])^ do
         if Extension=ext then begin
            Result:=VectorFileClass;
            Exit;
         end;
  Result:=nil;
end;

// FindFromFileName
//
function TVectorFileFormatsList.FindFromFileName(const fileName : String) : TVectorFileClass;
var
   ext : String;
begin
   ext:=ExtractFileExt(Filename);
   System.Delete(ext, 1, 1);
   Result:=FindExt(ext);
   if not Assigned(Result) then
      raise EInvalidVectorFile.CreateFmt(SUnknownExtension, [ext]);
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.Remove(AClass: TVectorFileClass);

var I : Integer;
    P : PVectorFileFormat;

begin
  for I:=Count-1 downto 0 do
  begin
    P:=PVectorFileFormat(Items[I]);
    if P^.VectorFileClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.BuildFilterStrings(VectorFileClass: TVectorFileClass;
                                                    var Descriptions, Filters: string);

var C, I : Integer;
    P    : PVectorFileFormat;

begin
  Descriptions:='';
  Filters:='';
  C:=0;
  for I:=Count-1 downto 0 do
  begin
    P:=PVectorFileFormat(Items[I]);
    if P^.VectorFileClass.InheritsFrom(VectorFileClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions:=Descriptions+'|';
          Filters:=Filters+';';
        end;
        if (Description = '') and (DescResID <> 0) then Description:=LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then FmtStr(Descriptions, '%s (%s)|%1:s|%s', [sAllFilter, Filters, Descriptions]);
end;

// ------------------
// ------------------ TBaseMeshObject ------------------
// ------------------

// Create
//
constructor TBaseMeshObject.Create;
begin
   FVertices:=TAffineVectorList.Create;
   FNormals:=TAffineVectorList.Create;
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

// WriteToFiler
//
procedure TBaseMeshObject.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      WriteString(FName);
      FVertices.WriteToFiler(writer);
      FNormals.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TBaseMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FName:=ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// Translate
//
procedure TBaseMeshObject.Translate(const delta : TAffineVector);
begin
   FVertices.Translate(delta);
end;

// BuildNormals
//
procedure TBaseMeshObject.BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                       normalIndices : TIntegerList = nil);
var
   i, base : Integer;
   n : TAffineVector;
   newNormals : TList;

   procedure TranslateNewNormal(vertexIndex : Integer; const delta : TAffineVector);
   var
      pv : PAffineVector;
   begin
      pv:=PAffineVector(newNormals[vertexIndex]);
      if not Assigned(pv) then begin
         Normals.Add(NullVector);
         pv:=@Normals.List[Normals.Count-1];
         newNormals[vertexIndex]:=pv;
      end;
      AddVector(pv^, delta);
   end;

begin
   if not Assigned(normalIndices) then begin
      // build bijection
      Normals.Clear;
      Normals.Count:=Vertices.Count;
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      Normals.Normalize;
   end else begin
      // add new normals
      base:=Normals.Count;
      newNormals:=TList.Create;
      newNormals.Count:=Vertices.Count;
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               with Normals do begin
                  TranslateNewNormal(vertexIndices[i+0], n);
                  TranslateNewNormal(vertexIndices[i+1], n);
                  TranslateNewNormal(vertexIndices[i+2], n);
               end;
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               with Normals do begin
                  TranslateNewNormal(vertexIndices[i+0], n);
                  TranslateNewNormal(vertexIndices[i+1], n);
                  TranslateNewNormal(vertexIndices[i+2], n);
               end;
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      for i:=base to Normals.Count-1 do
         NormalizeVector(Normals.List[i]);
      newNormals.Free;
   end;
end;

// ExtractTriangles
//
function TBaseMeshObject.ExtractTriangles : TAffineVectorList;
begin
   Result:=TAffineVectorList.Create;
   if (Vertices.Count mod 3)=0 then
      Result.Assign(Vertices);
end;

// SetVertices
//
procedure TBaseMeshObject.SetVertices(const val : TAffineVectorList);
begin
   FVertices.Assign(val);
end;

// SetNormals
//
procedure TBaseMeshObject.SetNormals(const val : TAffineVectorList);
begin
   FNormals.Assign(val);
end;

// ------------------
// ------------------ TSkeletonFrame ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonFrame.CreateOwned(aOwner : TSkeletonFrameList);
begin
   FOwner:=aOwner;
   aOwner.Add(Self);
   Create;
end;

// Create
//
constructor TSkeletonFrame.Create;
begin
	inherited Create;
   FPosition:=TAffineVectorList.Create;
   FRotation:=TAffineVectorList.Create;
end;

// Destroy
//
destructor TSkeletonFrame.Destroy;
begin
   FlushLocalMatrixList;
   FRotation.Free;
   FPosition.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeletonFrame.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteString(FName);
      FPosition.WriteToFiler(writer);
      FRotation.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TSkeletonFrame.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FName:=ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
	end else RaiseFilerException(archiveVersion);
   FlushLocalMatrixList;
end;

// SetPosition
//
procedure TSkeletonFrame.SetPosition(const val : TAffineVectorList);
begin
   FPosition.Assign(val);
end;

// SetRotation
//
procedure TSkeletonFrame.SetRotation(const val : TAffineVectorList);
begin
   FRotation.Assign(val);
end;

// LocalMatrixList
//
function TSkeletonFrame.LocalMatrixList : PMatrixArray;
var
   i : Integer;
   s, c : Single;
   mat, rmat : TMatrix;
begin
   if not Assigned(FLocalMatrixList) then begin
      FLocalMatrixList:=AllocMem(SizeOf(TMatrix)*Rotation.Count);
      for i:=0 to Rotation.Count-1 do begin
         mat:=IdentityHmgMatrix;
         SinCos(Rotation[i][0], s, c);
         rmat:=CreateRotationMatrixX(s, c);
         mat:=MatrixMultiply(mat, rmat);
         SinCos(Rotation[i][1], s, c);
         rmat:=CreateRotationMatrixY(s, c);
         mat:=MatrixMultiply(mat, rmat);
         SinCos(Rotation[i][2], s, c);
         rmat:=CreateRotationMatrixZ(s, c);
         mat:=MatrixMultiply(mat, rmat);
         mat[3][0]:=Position[i][0];
         mat[3][1]:=Position[i][1];
         mat[3][2]:=Position[i][2];
         FLocalMatrixList[i]:=mat;
      end;
   end;
   Result:=FLocalMatrixList;
end;

// FlushLocalMatrixList
//
procedure TSkeletonFrame.FlushLocalMatrixList;
begin
   if Assigned(FLocalMatrixList) then begin
      FreeMem(FLocalMatrixList);
      FLocalMatrixList:=nil;
   end;
end;

// ------------------
// ------------------ TSkeletonFrameList ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonFrameList.CreateOwned(AOwner : TPersistent);
begin
   FOwner:=AOwner;
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
procedure TSkeletonFrameList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Clear
//
procedure TSkeletonFrameList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetSkeletonFrame
//
function TSkeletonFrameList.GetSkeletonFrame(Index: Integer): TSkeletonFrame;
begin
   Result:=TSkeletonFrame(List^[Index]);
end;

// ------------------
// ------------------ TSkeletonBoneList ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonBoneList.CreateOwned(aOwner : TSkeleton);
begin
   FSkeleton:=aOwner;
	Create;
end;

// Create
//
constructor TSkeletonBoneList.Create;
begin
	inherited;
end;

// Destroy
//
destructor TSkeletonBoneList.Destroy;
begin
	inherited;
end;

// WriteToFiler
//
procedure TSkeletonBoneList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing, yet
   end;
end;

// ReadFromFiler
//
procedure TSkeletonBoneList.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      // nothing, yet
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// AfterObjectCreatedByReader
//
procedure TSkeletonBoneList.AfterObjectCreatedByReader(Sender : TObject);
begin
   with (Sender as TSkeletonBone) do begin
      FOwner:=Self;
      FSkeleton:=Self.Skeleton;
   end;
end;

// GetSkeletonBone
//
function TSkeletonBoneList.GetSkeletonBone(Index : Integer) : TSkeletonBone;
begin
   Result:=TSkeletonBone(List^[Index]);
end;

// BoneByID
//
function TSkeletonBoneList.BoneByID(anID : Integer) : TSkeletonBone;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do begin
      Result:=Items[i].BoneByID(anID);
      if Assigned(Result) then Break;
   end;
end;

// PrepareGlobalMatrices
//
procedure TSkeletonBoneList.PrepareGlobalMatrices;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TSkeletonRootBoneList ------------------
// ------------------

// Create
//
constructor TSkeletonRootBoneList.Create;
begin
	inherited;
end;

// Destroy
//
destructor TSkeletonRootBoneList.Destroy;
begin
	inherited;
end;

// WriteToFiler
//
procedure TSkeletonRootBoneList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing, yet
   end;
end;

// ReadFromFiler
//
procedure TSkeletonRootBoneList.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      // nothing, yet
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// BuildList
//
procedure TSkeletonRootBoneList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   // root node setups and restore OpenGL stuff
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_LIGHTING);
   // render root-bones
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
   glPopAttrib;
end;

// ------------------
// ------------------ TSkeletonBone ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonBone.CreateOwned(aOwner : TSkeletonBoneList);
begin
   FOwner:=aOwner;
   aOwner.Add(Self);
   FSkeleton:=aOwner.Skeleton;
	Create;
end;

// Create
//
constructor TSkeletonBone.Create;
begin
   FColor:=$FFFFFFFF; // opaque white
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
procedure TSkeletonBone.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteString(FName);
      WriteInteger(FBoneID);
      WriteInteger(FColor);
   end;
end;

// ReadFromFiler
//
procedure TSkeletonBone.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FName:=ReadString;
      FBoneID:=ReadInteger;
      FColor:=ReadInteger;
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// BuildList
//
procedure TSkeletonBone.BuildList(var mrci : TRenderContextInfo);

   procedure IssueColor(color : Cardinal);
   begin
      glColor4f(GetRValue(color)/255, GetGValue(color)/255, GetBValue(color)/255,
                ((color shr 24) and 255)/255);
   end;

var
   i : Integer;
begin
   // point for self
   glPointSize(5);
   glBegin(GL_POINTS);
      IssueColor(Color);
      glVertex3fv(@GlobalMatrix[3][0]);
   glEnd;
   glPointSize(1);
   // parent-self bone line
   if Owner is TSkeletonBone then begin
      glBegin(GL_LINES);
         glVertex3fv(@TSkeletonBone(Owner).GlobalMatrix[3][0]);
         glVertex3fv(@GlobalMatrix[3][0]);
      glEnd;
   end;
   // render sub-bones
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
end;

// GetSkeletonBone
//
function TSkeletonBone.GetSkeletonBone(Index: Integer): TSkeletonBone;
begin
   Result:=TSkeletonBone(List^[Index]);
end;

// SetColor
//
procedure TSkeletonBone.SetColor(const val : Cardinal);
begin
   FColor:=val;
end;

// BoneByID
//
function TSkeletonBone.BoneByID(anID : Integer) : TSkeletonBone;
begin
   if BoneID=anID then
      Result:=Self
   else Result:=inherited BoneByID(anID);
end;

// Clean
//
procedure TSkeletonBone.Clean;
begin
   BoneID:=0;
   Name:='';
   inherited;
end;

// PrepareGlobalMatrices
//
procedure TSkeletonBone.PrepareGlobalMatrices;
begin
   if Owner is TSkeletonBone then
      FGlobalMatrix:=MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList[BoneID],
                                    TSkeletonBone(Owner).FGlobalMatrix)
   else FGlobalMatrix:=Skeleton.CurrentFrame.LocalMatrixList[BoneID];
   inherited;
end;

// ------------------
// ------------------ TSkeleton ------------------
// ------------------

// CreateOwned
//
constructor TSkeleton.CreateOwned(AOwner : TBaseMesh);
begin
   FOwner:=aOwner;
   Create;
end;

// Create
//
constructor TSkeleton.Create;
begin
	inherited Create;
   FRootBones:=TSkeletonRootBoneList.CreateOwned(Self);
   FFrames:=TSkeletonFrameList.CreateOwned(Self);
end;

// Destroy
//
destructor TSkeleton.Destroy;
begin
   FlushBoneByIDCache;
   FCurrentFrame.Free;
   FFrames.Free;
   FRootBones.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeleton.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      FRootBones.WriteToFiler(writer);
      FFrames.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TSkeleton.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
	end else RaiseFilerException(archiveVersion);
end;

// SetRootBones
//
procedure TSkeleton.SetRootBones(const val : TSkeletonRootBoneList);
begin
   FRootBones.Assign(val);
end;

// SetFrames
//
procedure TSkeleton.SetFrames(const val : TSkeletonFrameList);
begin
   FFrames.Assign(val);
end;

// GetCurrentFrame
//
function TSkeleton.GetCurrentFrame : TSkeletonFrame;
begin
   if not Assigned(FCurrentFrame) then
      FCurrentFrame:=TSkeletonFrame(FFrames.Items[0].CreateClone);
   Result:=FCurrentFrame;
end;

// SetCurrentFrame
//
procedure TSkeleton.SetCurrentFrame(val : TSkeletonFrame);
begin
   if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
   FCurrentFrame:=TSkeletonFrame(val.CreateClone);
end;

// FlushBoneByIDCache
//
procedure TSkeleton.FlushBoneByIDCache;
begin
   FBonesByIDCache.Free;
   FBonesByIDCache:=nil;
end;

// BoneByID
//
function TSkeleton.BoneByID(anID : Integer) : TSkeletonBone;

   procedure CollectBones(bone : TSkeletonBone);
   var
      i : Integer;
   begin
      if bone.BoneID>=FBonesByIDCache.Count then
         FBonesByIDCache.Count:=bone.BoneID+1;
      FBonesByIDCache[bone.BoneID]:=bone;
      for i:=0 to bone.Count-1 do
         CollectBones(bone[i]);
   end;

var
   i : Integer;
begin
   if not Assigned(FBonesByIDCache) then begin
      FBonesByIDCache:=TList.Create;
      for i:=0 to RootBones.Count-1 do
         CollectBones(RootBones[i]);
   end;
   Result:=TSkeletonBone(FBonesByIDCache[anID])
end;

// MorphTo
//
procedure TSkeleton.MorphTo(frameIndex : Integer);
begin
   CurrentFrame:=Frames[frameIndex];
end;

// Lerp
//
procedure TSkeleton.Lerp(frameIndex1, frameIndex2 : Integer; lerpFactor : Single);
begin
   if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
   FCurrentFrame:=TSkeletonFrame.Create;
   with FCurrentFrame do begin
      Position.Lerp(Frames[frameIndex1].Position, Frames[frameIndex2].Position, lerpFactor);
      Rotation.AngleLerp(Frames[frameIndex1].Rotation, Frames[frameIndex2].Rotation, lerpFactor);
   end;
end;

// BlendedLerps
//
procedure TSkeleton.BlendedLerps(const lerpInfos : array of TBlendedLerpInfo);
var
   i, n : Integer;
begin
   n:=High(lerpInfos)-Low(lerpInfos)+1;
   Assert(n>=1);
   i:=Low(lerpInfos);
   if n=1 then begin
      // use fast lerp (no blend)
      with lerpInfos[i] do
         Lerp(frameIndex1, frameIndex2, lerpFactor);
   end else begin
      if Assigned(FCurrentFrame) then
         FCurrentFrame.Free;
      FCurrentFrame:=TSkeletonFrame.Create;
      with FCurrentFrame do begin
         // lerp first item separately
         Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
                       Frames[lerpInfos[i].frameIndex2].Position,
                       lerpInfos[i].lerpFactor);
         Position.Scale(lerpInfos[i].weight);
         Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                            Frames[lerpInfos[i].frameIndex2].Rotation,
                            lerpInfos[i].lerpFactor);
         Rotation.Scale(lerpInfos[i].weight);
         Inc(i);
         // combine the other items
         while i<=High(lerpInfos) do begin
            Position.Combine(Frames[lerpInfos[i].frameIndex1].Position,
                             (1-lerpInfos[i].lerpFactor)*lerpInfos[i].weight);
            Position.Combine(Frames[lerpInfos[i].frameIndex2].Position,
                             lerpInfos[i].lerpFactor*lerpInfos[i].weight);
            Rotation.Combine(Frames[lerpInfos[i].frameIndex1].Rotation,
                             (1-lerpInfos[i].lerpFactor)*lerpInfos[i].weight);
            Rotation.Combine(Frames[lerpInfos[i].frameIndex2].Rotation,
                             lerpInfos[i].lerpFactor*lerpInfos[i].weight);
            Inc(i);
         end;
      end;
   end;
end;

// MorphMesh
//
procedure TSkeleton.MorphMesh;
var
   i : Integer;
   mesh : TBaseMeshObject;
begin
   if Owner.MeshObjects.Count>0 then begin
      RootBones.PrepareGlobalMatrices;
      for i:=0 to Owner.MeshObjects.Count-1 do begin
         mesh:=Owner.MeshObjects.Items[0];
         if mesh is TSkeletonMeshObject then
            TSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame;
      end;
   end;
end;

// Clear
//
procedure TSkeleton.Clear;
begin
   FlushBoneByIDCache;
   RootBones.Clean;
   Frames.Clear;
   FCurrentFrame.Free;
   FCurrentFrame:=nil;
end;

// ------------------
// ------------------ TMeshObject ------------------
// ------------------

// CreateOwned
//
constructor TMeshObject.CreateOwned(AOwner : TMeshObjectList);
begin
   FOwner:=AOwner;
   Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Create
//
constructor TMeshObject.Create;
begin
   FMode:=momTriangles;
   FTexCoords:=TAffineVectorList.Create;
   FColors:=TVectorList.Create;
   FFaceGroups:=TFaceGroups.CreateOwned(Self);
   inherited;
end;

// Destroy
//
destructor TMeshObject.Destroy;
begin
   FFaceGroups.Free;
   FColors.Free;
   FTexCoords.Free;
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// WriteToFiler
//
procedure TMeshObject.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FTexCoords.WriteToFiler(writer);
      FColors.WriteToFiler(writer);
      FFaceGroups.WriteToFiler(writer);
      WriteInteger(Integer(FMode));
   end;
end;

// ReadFromFiler
//
procedure TMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FTexCoords.ReadFromFiler(reader);
      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode:=TMeshObjectMode(ReadInteger);
   end else RaiseFilerException(archiveVersion);
end;

// ExtractTriangles
//
function TMeshObject.ExtractTriangles : TAffineVectorList;
var
   i : Integer;
begin
   case mode of
      momTriangles : begin
         Result:=inherited ExtractTriangles;
      end;
      momTriangleStrip : begin
         Result:=TAffineVectorList.Create;
         Result.Capacity:=3*(Vertices.Count-2);
         for i:=0 to Vertices.Count-3 do begin
            if (i and 1)=0 then
               Result.Add(Vertices.List[i+0], Vertices.List[i+1], Vertices.List[i+2])
            else Result.Add(Vertices.List[i+2], Vertices.List[i+1], Vertices.List[i+0]);
         end;
      end;
      momFaceGroups : begin
         Result:=TAffineVectorList.Create;
         FaceGroups.AddToTriangles(Result);
      end;
   else
      Result:=nil;
      Assert(False);
   end;
end;

// GetExtents
//
procedure TMeshObject.GetExtents(var min, max : TAffineVector);
begin
   FVertices.GetExtents(min, max);
end;

// Prepare
//
procedure TMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      FaceGroups[i].Prepare;
end;

// PointInObject
//
function TMeshObject.PointInObject(const aPoint : TAffineVector) : Boolean;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   Result:=(aPoint[0]>=min[0]) and (aPoint[1]>=min[1]) and (aPoint[2]>=min[2])
           and (aPoint[0]<=max[0]) and (aPoint[1]<=max[1]) and (aPoint[2]<=max[2]);
end;

// SetTexCoords
//
procedure TMeshObject.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// SetColors
//
procedure TMeshObject.SetColors(const val : TVectorList);
begin
   FColors.Assign(val);
end;

// DeclareArraysToOpenGL
//
procedure TMeshObject.DeclareArraysToOpenGL(evenIfAlreadyDeclared : Boolean = False);
begin
   if evenIfAlreadyDeclared or (not FArraysDeclared) then begin
      if Vertices.Count>0 then begin
         glEnableClientState(GL_VERTEX_ARRAY);
         glVertexPointer(3, GL_FLOAT, 0, Vertices.List);
      end else glDisableClientState(GL_VERTEX_ARRAY);
      if Normals.Count>0 then begin
         glEnableClientState(GL_NORMAL_ARRAY);
         glNormalPointer(GL_FLOAT, 0, Normals.List);
      end else glDisableClientState(GL_NORMAL_ARRAY);
      if Colors.Count>0 then begin
         glEnableClientState(GL_COLOR_ARRAY);
         glColorPointer(4, GL_FLOAT, 0, Colors.List);
      end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      if TexCoords.Count>0 then begin
         glEnableClientState(GL_TEXTURE_COORD_ARRAY);
         xglTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), TexCoords.List);
      end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      FArraysDeclared:=True;
   end;
end;

// PrepareMaterials
//
procedure TMeshObject.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   if (Mode=momFaceGroups) and Assigned(mrci.materialLibrary) then begin
      for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
         libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
         if Assigned(libMat) then
            libMat.PrepareBuildList;
      end;
   end;
end;

// BuildList
//
procedure TMeshObject.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   gotNormals, gotTexCoords, gotColor : Boolean;
   libMat : TGLLibMaterial;
begin
   FArraysDeclared:=False;
   case Mode of
      momTriangles, momTriangleStrip : if Vertices.Count>0 then begin
         DeclareArraysToOpenGL;
         if Mode=momTriangles then
            glBegin(GL_TRIANGLES)
         else glBegin(GL_TRIANGLE_STRIP);
         gotNormals:=(Vertices.Count=Normals.Count);
         gotTexCoords:=(Vertices.Count=TexCoords.Count);
         gotColor:=(Vertices.Count=Colors.Count);
         if gotColor then begin
            glPushAttrib(GL_ENABLE_BIT);
            glEnable(GL_COLOR_MATERIAL);
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
            ResetGLMaterialColors;
         end;
         for i:=0 to Vertices.Count-1 do begin
            if gotNormals   then glNormal3fv(@Normals.List[i]);
            if gotTexCoords then xglTexCoord2fv(@TexCoords.List[i]);
            if gotColor     then glColor4fv(@Colors.List[i]);
            glVertex3fv(@Vertices.List[i]);
         end;
         glEnd;
         if gotColor then glPopAttrib;
      end;
      momFaceGroups : begin
         if Assigned(mrci.materialLibrary) then begin
            for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
               libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
               if Assigned(libMat) then begin
                  libMat.Apply(mrci);
                  BuildList(mrci);
                  libMat.UnApply(mrci);
               end else BuildList(mrci);
            end;
         end else for i:=0 to FaceGroups.Count-1 do
            FaceGroups[i].BuildList(mrci);
      end;
   else
      Assert(False);
   end;
end;

// ------------------
// ------------------ TMeshObjectList ------------------
// ------------------

// CreateOwned
//
constructor TMeshObjectList.CreateOwned(aOwner : TBaseMesh);
begin
   FOwner:=AOwner;
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
procedure TMeshObjectList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// PrepareBuildList
//
procedure TMeshObjectList.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].PrepareBuildList(mrci);
end;

// BuildList
//
procedure TMeshObjectList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
end;

// MorphTo
//
procedure TMeshObjectList.MorphTo(morphTargetIndex : Integer);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

// Lerp
//
procedure TMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                               lerpFactor : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2, lerpFactor);
end;

// MorphTargetCount
//
function TMeshObjectList.MorphTargetCount : Integer;
var
   i : Integer;
begin
   Result:=MaxInt;
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      with TMorphableMeshObject(Items[i]) do
         if Result>MorphTargets.Count then
            Result:=MorphTargets.Count;
   if Result=MaxInt then
      Result:=0;
end;

// Clear
//
procedure TMeshObjectList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshObject
//
function TMeshObjectList.GetMeshObject(Index: Integer): TMeshObject;
begin
   Result:=TMeshObject(List^[Index]);
end;

// GetExtents
//
procedure TMeshObjectList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      GetMeshObject(i).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// Translate
//
procedure TMeshObjectList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      GetMeshObject(i).Translate(delta);
end;

// ExtractTriangles
//
function TMeshObjectList.ExtractTriangles : TAffineVectorList;
var
   i : Integer;
   objTris : TAffineVectorList;
begin
   Result:=TAffineVectorList.Create;
   for i:=0 to Count-1 do begin
      objTris:=GetMeshObject(i).ExtractTriangles;
      try
         Result.Add(objTris);
      finally
         objTris.Free;
      end;
   end;
end;

// Prepare
//
procedure TMeshObjectList.Prepare;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Prepare;
end;

// ------------------
// ------------------ TMeshMorphTarget ------------------
// ------------------

// CreateOwned
//
constructor TMeshMorphTarget.CreateOwned(AOwner : TMeshMorphTargetList);
begin
   FOwner:=AOwner;
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
procedure TMeshMorphTarget.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      //nothing
   end;
end;

// ReadFromFiler
//
procedure TMeshMorphTarget.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      //nothing
   end else RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TMeshMorphTargetList ------------------
// ------------------

// CreateOwned
//
constructor TMeshMorphTargetList.CreateOwned(AOwner : TPersistent);
begin
   FOwner:=AOwner;
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
procedure TMeshMorphTargetList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Translate
//
procedure TMeshMorphTargetList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Translate(delta);
end;

// Clear
//
procedure TMeshMorphTargetList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshMorphTarget
//
function TMeshMorphTargetList.GetMeshMorphTarget(Index: Integer): TMeshMorphTarget;
begin
   Result:=TMeshMorphTarget(List^[Index]);
end;

// ------------------
// ------------------ TMorphableMeshObject ------------------
// ------------------

// Create
//
constructor TMorphableMeshObject.Create;
begin
   FMorphTargets:=TMeshMorphTargetList.CreateOwned(Self);
   inherited;
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
procedure TMorphableMeshObject.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FMorphTargets.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TMorphableMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FMorphTargets.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// Translate
//
procedure TMorphableMeshObject.Translate(const delta : TAffineVector);
begin
   inherited;
   MorphTargets.Translate(delta);
end;

// MorphTo
//
procedure TMorphableMeshObject.MorphTo(morphTargetIndex : Integer);
begin
   Assert(Cardinal(morphTargetIndex)<Cardinal(MorphTargets.Count));
   with MorphTargets[morphTargetIndex] do begin
      if Vertices.Count>0 then
         Self.Vertices.Assign(Vertices);
      if Normals.Count>0 then
         Self.Normals.Assign(Normals);
   end;
end;

// Lerp
//
procedure TMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                                    lerpFactor : Single);
var
   mt1, mt2 : TMeshMorphTarget;
begin
   Assert((Cardinal(morphTargetIndex1)<Cardinal(MorphTargets.Count))
          and (Cardinal(morphTargetIndex2)<Cardinal(MorphTargets.Count)));
   if lerpFactor=0 then
      MorphTo(morphTargetIndex1)
   else if lerpFactor=1 then
      MorphTo(morphTargetIndex2)
   else begin
      mt1:=MorphTargets[morphTargetIndex1];
      mt2:=MorphTargets[morphTargetIndex2];
      if mt1.Vertices.Count>0 then
         Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      if mt1.Normals.Count>0 then begin
         Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
         Normals.Normalize;
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
   FBoneMatrixInvertedMeshes:=TList.Create;
	inherited Create;
end;

// Destroy
//
destructor TSkeletonMeshObject.Destroy;
begin
   Clear;
   FBoneMatrixInvertedMeshes.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeletonMeshObject.WriteToFiler(writer : TVirtualWriter);
var
   i : Integer;
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteInteger(FVerticeBoneWeightCount);
      WriteInteger(FBonesPerVertex);
      for i:=0 to FVerticeBoneWeightCount-1 do
         Write(FVerticesBonesWeights[i][0], FBonesPerVertex*SizeOf(TVertexBoneWeight));
   end;
end;

// ReadFromFiler
//
procedure TSkeletonMeshObject.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FVerticeBoneWeightCount:=ReadInteger;
      FBonesPerVertex:=ReadInteger;
      ResizeVerticesBonesWeights;
      for i:=0 to FVerticeBoneWeightCount-1 do
         Read(FVerticesBonesWeights[i][0], FBonesPerVertex*SizeOf(TVertexBoneWeight));
	end else RaiseFilerException(archiveVersion);
end;

// SetVerticeBoneWeightCount
//
procedure TSkeletonMeshObject.SetVerticeBoneWeightCount(const val : Integer);
begin
   if val<>FVerticeBoneWeightCount then begin
      FVerticeBoneWeightCount:=val;
      ResizeVerticesBonesWeights;
   end;
end;

// SetBonesPerVertex
//
procedure TSkeletonMeshObject.SetBonesPerVertex(const val : Integer);
begin
   if val<>FBonesPerVertex then begin
      FBonesPerVertex:=val;
      ResizeVerticesBonesWeights;
   end;
end;

// ResizeVerticesBonesWeights
//
procedure TSkeletonMeshObject.ResizeVerticesBonesWeights;
var
   n, m, i, j : Integer;
   newArea : PVerticesBoneWeights;
begin
   n:=BonesPerVertex*VerticeBoneWeightCount;
   if n=0 then begin
      // release everything
      if Assigned(FVerticesBonesWeights) then begin
         FreeMem(FVerticesBonesWeights[0]);
         FreeMem(FVerticesBonesWeights);
         FVerticesBonesWeights:=nil;
      end;
   end else begin
      // allocate new area
      newArea:=AllocMem(VerticeBoneWeightCount*SizeOf(PVertexBoneWeightArray));
      newArea[0]:=AllocMem(n*SizeOf(TVertexBoneWeight));
      for i:=1 to VerticeBoneWeightCount-1 do
         newArea[i]:=PVertexBoneWeightArray(Integer(newArea[0])+i*SizeOf(TVertexBoneWeight));
      // transfer old data
      if FLastVerticeBoneWeightCount<VerticeBoneWeightCount then
         n:=FLastVerticeBoneWeightCount
      else n:=VerticeBoneWeightCount;
      if FLastBonesPerVertex<BonesPerVertex then
         m:=FLastBonesPerVertex
      else m:=BonesPerVertex;
      for i:=0 to n-1 do
         for j:=0 to m-1 do newArea[i][j]:=VerticesBonesWeights[i][j];
      // release old area and switch to new
      if Assigned(FVerticesBonesWeights) then begin
         FreeMem(FVerticesBonesWeights[0]);
         FreeMem(FVerticesBonesWeights);
      end;
      FVerticesBonesWeights:=newArea;
   end;
   FLastVerticeBoneWeightCount:=FVerticeBoneWeightCount;
   FLastBonesPerVertex:=FBonesPerVertex;
end;

// AddWeightedBone
//
procedure TSkeletonMeshObject.AddWeightedBone(aBoneID : Integer; aWeight : Single);
begin
   if BonesPerVertex<1 then BonesPerVertex:=1;
   VerticeBoneWeightCount:=VerticeBoneWeightCount+1;
   with VerticesBonesWeights[VerticeBoneWeightCount-1][0] do begin
      BoneID:=aBoneID;
      Weight:=aWeight;
   end;
end;

// FindOrAdd
//
function TSkeletonMeshObject.FindOrAdd(boneID : Integer; const vertex, normal : TAffineVector) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Vertices.Count-1 do
      if (VerticesBonesWeights[i][0].BoneID=boneID)
            and VectorEquals(Vertices[i], vertex)
            and VectorEquals(Normals[i], normal) then begin
         Result:=i;
         Break;
      end;
   if Result<0 then begin
      AddWeightedBone(boneID, 1);
      Vertices.Add(vertex);
      Result:=Normals.Add(normal);
   end;
end;

// PrepareBoneMatrixInvertedMeshes
//
procedure TSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
   i, k, boneIndex : Integer;
   invMesh : TBaseMeshObject;
   invMat : TMatrix;
   bone : TSkeletonBone;
   p : TVector;
begin
   // cleanup existing stuff
   for i:=0 to FBoneMatrixInvertedMeshes.Count-1 do
      TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
   FBoneMatrixInvertedMeshes.Clear;
   // calculate
   for k:=0 to BonesPerVertex-1 do begin
      invMesh:=TBaseMeshObject.Create;
      FBoneMatrixInvertedMeshes.Add(invMesh);
      invMesh.Vertices:=Vertices;
      invMesh.Normals:=Normals;
      for i:=0 to Vertices.Count-1 do begin
         boneIndex:=VerticesBonesWeights[i][k].BoneID;
         bone:=Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
         // transform point
         MakePoint(p, Vertices[i]);
         invMat:=bone.GlobalMatrix;
         InvertMatrix(invMat);
         p:=VectorTransform(p, invMat);
         invMesh.Vertices[i]:=PAffineVector(@p)^;
         // transform normal
         SetVector(p, Normals[i]);
         invMat:=bone.GlobalMatrix;
         invMat[3]:=NullHmgPoint;
         InvertMatrix(invMat);
         p:=VectorTransform(p, invMat);
         invMesh.Normals[i]:=PAffineVector(@p)^;
      end;
   end;
end;

// ApplyCurrentSkeletonFrame
//
procedure TSkeletonMeshObject.ApplyCurrentSkeletonFrame;
var
   i, boneID : Integer;
   refVertices, refNormals : TAffineVectorList;
   p, n : TVector;
   bone : TSkeletonBone;
   skeleton : TSkeleton;
begin
   refVertices:=TBaseMeshObject(FBoneMatrixInvertedMeshes[0]).Vertices;
   refNormals:=TBaseMeshObject(FBoneMatrixInvertedMeshes[0]).Normals;
   skeleton:=Owner.Owner.Skeleton;
   for i:=0 to refVertices.Count-1 do begin
      boneID:=VerticesBonesWeights[i][0].BoneID;
      bone:=skeleton.BoneByID(boneID);
      MakePoint(p, refVertices[i]);
      p:=VectorTransform(p, bone.GlobalMatrix);
      Vertices[i]:=PAffineVector(@p)^;
      MakeVector(n, refNormals[i]);
      n:=VectorTransform(n, bone.GlobalMatrix);
      Normals[i]:=PAffineVector(@n)^;
   end;
end;

// Clear
//
procedure TSkeletonMeshObject.Clear;
var
   i : Integer;
begin
   FVerticeBoneWeightCount:=0;
   FBonesPerVertex:=0;
   ResizeVerticesBonesWeights;
   for i:=0 to FBoneMatrixInvertedMeshes.Count-1 do
      TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
   FBoneMatrixInvertedMeshes.Clear;
end;

// ------------------
// ------------------ TFaceGroup ------------------
// ------------------

// CreateOwned
//
constructor TFaceGroup.CreateOwned(AOwner : TFaceGroups);
begin
   FOwner:=AOwner;
   Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TFaceGroup.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// WriteToFiler
//
procedure TFaceGroup.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      WriteString(FMaterialName);
   end;
end;

// ReadFromFiler
//
procedure TFaceGroup.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FMaterialName:=ReadString;
   end else RaiseFilerException(archiveVersion);
end;

// BuildList
//
procedure TFaceGroup.BuildList(var mrci : TRenderContextInfo);
begin
   // nothing
end;

// AddToTriangles
//
procedure TFaceGroup.AddToTriangles(aList : TAffineVectorList);
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
   FVertexIndices:=TIntegerList.Create;
   FMode:=fgmmTriangles;
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
procedure TFGVertexIndexList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FVertexIndices.WriteToFiler(writer);
      WriteInteger(Integer(FMode));
   end;
end;

// ReadFromFiler
//
procedure TFGVertexIndexList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FVertexIndices.ReadFromFiler(reader);
      FMode:=TFaceGroupMeshMode(ReadInteger);
   end else RaiseFilerException(archiveVersion);
end;

// SetIndices
//
procedure TFGVertexIndexList.SetVertexIndices(const val : TIntegerList);
begin
   FVertexIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexIndexList.BuildList(var mrci : TRenderContextInfo);
begin
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLES, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
      fgmmTriangleStrip : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLE_STRIP, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
      fgmmTriangleFan : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLE_FAN, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
   else
      Assert(False);
   end;
end;

// AddToTriangles
//
procedure TFGVertexIndexList.AddToTriangles(aList : TAffineVectorList);
var
   i : Integer;
   vertexList : TAffineVectorList;
begin
   vertexList:=Owner.Owner.Vertices;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : begin
         aList.AdjustCapacityToAtLeast(aList.Count+VertexIndices.Count);
         for i:=0 to VertexIndices.Count-1 do
            aList.Add(vertexList[VertexIndices[i]]);
      end;
      fgmmTriangleStrip : begin
         aList.AdjustCapacityToAtLeast(aList.Count+(VertexIndices.Count-2)*3);
         for i:=0 to VertexIndices.Count-3 do begin
            if (i and 1)=0 then
               aList.Add(vertexList[VertexIndices[i+0]],
                         vertexList[VertexIndices[i+1]],
                         vertexList[VertexIndices[i+2]])
            else
               aList.Add(vertexList[VertexIndices[i+2]],
                         vertexList[VertexIndices[i+1]],
                         vertexList[VertexIndices[i+0]]);
         end;
      end;
      fgmmTriangleFan : begin
         aList.AdjustCapacityToAtLeast(aList.Count+(VertexIndices.Count-2)*3);
         for i:=2 to VertexIndices.Count-1 do begin
            aList.Add(vertexList[VertexIndices[0]],
                      vertexList[VertexIndices[i-1]],
                      vertexList[VertexIndices[i]]);
         end;
      end;
   else
      Assert(False);
   end;
end;

// Add
//
procedure TFGVertexIndexList.Add(idx : Integer);
begin
   FVertexIndices.Add(idx);
end;

// GetExtents
//
procedure TFGVertexIndexList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
   ref : PFloatArray;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to VertexIndices.Count-1 do begin
      ref:=Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
      for k:=0 to 2 do begin
         f:=ref[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// GetNormal
//
function TFGVertexIndexList.GetNormal : TAffineVector;
begin
   if VertexIndices.Count<3 then
      Result:=NullVector
   else with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
                      Items[VertexIndices[2]], Result);
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

// Create
//
constructor TFGVertexNormalTexIndexList.Create;
begin
   inherited;
   FNormalIndices:=TIntegerList.Create;
   FTexCoordIndices:=TIntegerList.Create;
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
procedure TFGVertexNormalTexIndexList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FNormalIndices.WriteToFiler(writer);
      FTexCoordIndices.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// SetNormalIndices
//
procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val : TIntegerList);
begin
   FNormalIndices.Assign(val);
end;

// SetTexCoordIndices
//
procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val : TIntegerList);
begin
   FTexCoordIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexNormalTexIndexList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   vertexPool : PAffineVectorArray;
   normalPool : PAffineVectorArray;
   texCoordPool : PAffineVectorArray;
begin
   Assert((VertexIndices.Count=TexCoordIndices.Count)
          and (VertexIndices.Count=NormalIndices.Count));
   vertexPool:=Owner.Owner.Vertices.List;
   normalPool:=Owner.Owner.Normals.List;
   texCoordPool:=Owner.Owner.TexCoords.List;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : glBegin(GL_TRIANGLES);
      fgmmTriangleStrip :                glBegin(GL_TRIANGLE_STRIP);
      fgmmTriangleFan   :                glBegin(GL_TRIANGLE_FAN);
   else
      Assert(False);
   end;
   for i:=0 to VertexIndices.Count-1 do begin
      glNormal3fv(@normalPool[NormalIndices.List[i]]);
      xglTexCoord2fv(@texCoordPool[TexCoordIndices.List[i]]);
      glVertex3fv(@vertexPool[VertexIndices.List[i]]);
   end;
   glEnd;
end;

// AddToTriangles
//
procedure TFGVertexNormalTexIndexList.AddToTriangles(aList : TAffineVectorList);
begin
   inherited AddToTriangles(aList);
end;

// Add
//
procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx : Integer);
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
   FTexCoords:=TAffineVectorList.Create;
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
procedure TFGIndexTexCoordList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FTexCoords.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TFGIndexTexCoordList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FTexCoords.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// SetTexCoords
//
procedure TFGIndexTexCoordList.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// BuildList
//
procedure TFGIndexTexCoordList.BuildList(var mrci : TRenderContextInfo);
var
   i, k : Integer;
   texCoordPool : PAffineVectorArray;
   vertexPool : PAffineVectorArray;
   normalPool : PAffineVectorArray;
   indicesPool : PIntegerArray;
begin
   Assert(VertexIndices.Count=TexCoords.Count);
   texCoordPool:=TexCoords.List;
   vertexPool:=Owner.Owner.Vertices.List;
   indicesPool:=@VertexIndices.List[0];
   case Mode of
      fgmmTriangles, fgmmFlatTriangles :
         glBegin(GL_TRIANGLES);
      fgmmTriangleStrip :
         glBegin(GL_TRIANGLE_STRIP);
      fgmmTriangleFan :
         glBegin(GL_TRIANGLE_FAN);
   else
      Assert(False);
   end;
   if Owner.Owner.Normals.Count=Owner.Owner.Vertices.Count then begin
      normalPool:=Owner.Owner.Normals.List;
      for i:=0 to VertexIndices.Count-1 do begin
         xglTexCoord2fv(@texCoordPool[i]);
         k:=indicesPool[i];
         glNormal3fv(@normalPool[k]);
         glVertex3fv(@vertexPool[k]);
      end;
   end else begin
      for i:=0 to VertexIndices.Count-1 do begin
         xglTexCoord2fv(@texCoordPool[i]);
         glVertex3fv(@vertexPool[indicesPool[i]]);
      end;
   end;
   glEnd;
end;

// AddToTriangles
//
procedure TFGIndexTexCoordList.AddToTriangles(aList : TAffineVectorList);
begin
   inherited AddToTriangles(aList);
end;

// Add (affine)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const texCoord : TAffineVector);
begin
   TexCoords.Add(texCoord);
   inherited Add(idx);
end;

// Add (s, t)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const s, t : Single);
begin
   TexCoords.Add(s, t, 0);
   inherited Add(idx);
end;

// ------------------
// ------------------ TFaceGroups ------------------
// ------------------

// CreateOwned
//
constructor TFaceGroups.CreateOwned(AOwner : TMeshObject);
begin
   FOwner:=AOwner;
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
procedure TFaceGroups.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Clear
//
procedure TFaceGroups.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with GetFaceGroup(i) do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetFaceGroup
//
function TFaceGroups.GetFaceGroup(Index: Integer): TFaceGroup;
begin
   Result:=TFaceGroup(List^[Index]);
end;

// AddToTriangles
//
procedure TFaceGroups.AddToTriangles(aList : TAffineVectorList);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].AddToTriangles(aList);
end;

// SortByMaterial
//
procedure TFaceGroups.SortByMaterial;
var
   i, j : Integer;
   cur : String;
   curIdx : Integer;
begin
   for i:=0 to Count-2 do begin
      curIdx:=i;
      cur:=Items[i].MaterialName;
      for j:=i+1 to Count-1 do begin
         if Items[j].MaterialName<cur then begin
            cur:=Items[j].MaterialName;
            curIdx:=j;
         end;
      end;
      Exchange(curIdx, i);
   end;
end;

// ------------------
// ------------------ TVectorFile ------------------
// ------------------

// Create
//
constructor TVectorFile.Create(AOwner: TPersistent);
begin
   Assert(AOwner is TBaseMesh);
   inherited;
end;

// Owner
//
function TVectorFile.Owner : TBaseMesh;
begin
   Result:=TFreeForm(GetOwner);
end;

// SetNormalsOrientation
//
procedure TVectorFile.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   FNormalsOrientation:=val;
end;

// ------------------
// ------------------ TGL3DSVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGL3DSVectorFile.LoadFromStream(aStream: TStream);
type
   TSmoothIndexEntry = array[0..31] of Cardinal;
   PSmoothIndexArray = ^TSmoothIndexArray;
   TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
   Marker: PByteArray;
   CurrentVertexCount: Integer;
   SmoothIndices: PSmoothIndexArray;
   mesh : TMeshObject;

   //--------------- local functions -------------------------------------------

   function GetOrAllocateMaterial(materials : TMaterialList; const name : String) : String;
   var
      material : PMaterial3DS;
      specColor : TVector;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      material:=Materials.MaterialByName[Name];
      Assert(Assigned(material));
      if GetOwner is TBaseMesh then begin
         matLib:=TBaseMesh(GetOwner).MaterialLibrary;
         if Assigned(matLib) then begin
            Result:=name;
            libMat:=matLib.Materials.GetLibMaterialByName(name);
            if not Assigned(libMat) then begin
               libMat:=matLib.Materials.Add;
               libMat.Name:=name;
               with libMat.Material.FrontProperties do begin
                  Ambient.Color:=VectorMake(material.Ambient.R, material.Ambient.G, material.Ambient.B, 1);
                  Diffuse.Color:=VectorMake(material.Diffuse.R, material.Diffuse.G, material.Diffuse.B, 1);
                  specColor:=VectorMake(material.Specular.R, material.Specular.G, material.Specular.B, 1);
                  ScaleVector(specColor, 1 - material.Shininess);
                  Specular.Color:=specColor;
                  Shininess:=Round((1 - material.ShinStrength) * 128);
               end;
               if Trim(material.Texture.Map.Name)<>'' then begin
                  libMat.Material.Texture.Image.LoadFromFile(material.Texture.Map.Name);
                  libMat.Material.Texture.Disabled:=False;
               end;
            end;
         end else Result:='';
      end else Result:='';
   end;

   //----------------------------------------------------------------------

   function IsVertexMarked(P: Pointer; Index: Integer): Boolean; assembler;
      // tests the Index-th bit, returns True if set else False
   asm
                     BT [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   function MarkVertex(P: Pointer; Index: Integer): Boolean; assembler;
      // sets the Index-th bit and return True if it was already set else False
   asm
                     BTS [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: Cardinal; P: Pointer);
      // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
      // using field SmoothingGroup, which must not be 0.
      // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
      // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
      // it has no smoothing group or a different one then the index of the newly created vertex is
      // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
      // and duplicated vertices.
      // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
      //       have more than one group assigned to a face. To make the code fail safe the group ID
      //       is scanned for the lowest bit set.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index (convert flag into an index)
                   MOV EDX, [P]                  // get address of index array
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, EDX
                   LEA EDX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV [EDX], ECX
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   function GetSmoothIndex(ThisIndex, SmoothingGroup: Cardinal; P: Pointer): Cardinal;
      // Retrieves the vertex index for the given index and smoothing group.
      // This redirection is necessary because a vertex might have been duplicated.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, ECX
                   LEA ECX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV EAX, [ECX]
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   procedure DuplicateVertex(Index: Integer);
      // extends the vector and normal array by one entry and duplicates the vertex data given by Index
      // the marker and texture arrays will be extended too, if necessary
   begin
      // enhance vertex array
      with mesh.Vertices do Add(Items[index]);
      mesh.Normals.Add(NullVector);
      // enhance smooth index array
      ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
      FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
      // enhance marker array
      if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then begin
         ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
         Marker[(CurrentVertexCount div 8) + 1]:=0;
      end;
      with mesh.TexCoords do if Count>0 then Add(Items[index]);
      Inc(CurrentVertexCount);
   end;

   //--------------- end local functions ---------------------------------------

var
  iMaterial, i, j : Integer;
  aFaceGroup : TFGVertexIndexList;
  Face, Vertex, TargetVertex: Integer;
  SmoothingGroup: Cardinal;
  CurrentIndex: Word;
  Vector1, Vector2, Normal : TAffineVector;
  standardNormalsOrientation : Boolean;
begin
   with TFile3DS.Create do try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation:=not (NormalsOrientation=mnoDefault);
      for i:=0 to Objects.MeshCount-1 do with PMesh3DS(Objects.Mesh[I])^ do begin
         if IsHidden or (NVertices<3) then Continue;
         mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
         mesh.Name:=PMesh3DS(Objects.Mesh[I])^.Name;
         with mesh do begin
            Mode:=momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity:=NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts>0 then begin
               TexCoords.Capacity:=NVertices;
               for j:=0 to NVertices-1 do begin
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
                  TexCoords.Add(PTexPoint(@TextArray[j])^);
               end;
            end else begin
               for j:=0 to NVertices-1 do
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
         end;
         // allocate memory for the smoothindices and the marker array
         CurrentVertexCount:=NVertices;
         Marker:=AllocMem((NVertices div 8) + 1); // one bit for each vertex
         GetMem(SmoothIndices, NVertices * SizeOf(TSmoothIndexEntry));

         if SmoothArray=nil then begin
            // no smoothing groups to consider
            for face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  CurrentIndex:=FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) then begin
                     // already touched vertex must be duplicated
                     DuplicateVertex(CurrentIndex);
                     FaceRec[Vertex]:=CurrentVertexCount-1;
                     mesh.Normals[CurrentVertexCount-1]:=Normal;
                  end else begin
                     // not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end else begin
            // smoothing groups are to be considered
            for Face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               SmoothingGroup:=SmoothArray[Face];
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  currentIndex:=FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then begin
                     // check smoothing group
                     if SmoothingGroup = 0 then begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex]:=CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1]:=Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                     end else begin
                        // this vertex must be smoothed, check if there's already
                        // a (duplicated) vertex for this smoothing group
                        TargetVertex:=GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                        if TargetVertex < 0 then begin
                           // vertex has not yet been duplicated for this smoothing
                           // group, so do it now
                           DuplicateVertex(CurrentIndex);
                           FaceRec[Vertex]:=CurrentVertexCount - 1;
                           mesh.Normals[CurrentVertexCount - 1]:=Normal;
                           StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           StoreSmoothIndex(CurrentVertexCount - 1, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           // mark new vertex also as touched
                           MarkVertex(Marker, CurrentVertexCount - 1);
                        end else begin
                           // vertex has already been duplicated,
                           // so just add normal vector to other vertex...
                           mesh.Normals[TargetVertex]:=VectorAdd(mesh.Normals[TargetVertex], Normal);
                           // ...and tell which new vertex has to be used from now on
                           FaceRec[Vertex]:=TargetVertex;
                        end;
                     end;
                  end else begin
                     // vertex not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     // initialize smooth indices for this vertex
                     FillChar(SmoothIndices[CurrentIndex], SizeOf(TSmoothIndexEntry), $FF);
                     if SmoothingGroup <> 0 then
                        StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentIndex, SmoothIndices);
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end;
         FreeMem(Marker);
         FreeMem(SmoothIndices);

         Assert(mesh.Vertices.Count=CurrentVertexCount);

         // and normalize the Normals array
         mesh.Normals.Normalize;

         // now go for each material group
         // if there's no face to material assignment then just copy the
         // face definitions and rely on the default texture of the scene object
         if NMats = 0 then begin
            aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
            with aFaceGroup do begin
               MaterialName:='';
               // copy the face list
               for j:=0 to NFaces-1 do begin
                  Add(FaceArray[J].V1);
                  Add(FaceArray[J].V2);
                  Add(FaceArray[J].V3);
               end;
            end;
         end else begin
            for iMaterial:=0 to NMats - 1 do begin
               aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
               with aFaceGroup do begin
                  MaterialName:=GetOrAllocateMaterial(Materials, MatArray[iMaterial].Name);
                  // copy all vertices belonging to the current face into our index array,
                  // there won't be redundant vertices since this would mean a face has more than one
                  // material
                  // NFaces is the one from FaceGroup
                  with MatArray[iMaterial] do for j:=0 to NFaces - 1 do begin
                     Add(FaceArray[FaceIndex[J]].V1);
                     Add(FaceArray[FaceIndex[J]].V2);
                     Add(FaceArray[FaceIndex[J]].V3);
                  end;
               end;
            end;
         end;
      end;
   finally
      Free;
   end;
end;

//----------------- TBaseMesh --------------------------------------------------

// Create
//
constructor TBaseMesh.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding];
   if FMeshObjects=nil then
      FMeshObjects:=TMeshObjectList.CreateOwned(Self);
   if FSkeleton=nil then
      FSkeleton:=TSkeleton.CreateOwned(Self);
   FUseMeshMaterials:=True;
   FAutoCentering:=[];
   FAxisAlignedDimensionsCache[0]:=-1;
end;

// Destroy
//
destructor TBaseMesh.Destroy;
begin
   FSkeleton.Free;
   FMeshObjects.Free;
   inherited Destroy;
end;

// LoadFromFile
//
procedure TBaseMesh.LoadFromFile(const filename : String);
var
   fs : TFileStream;
begin
   if fileName <> '' then begin
      fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
      try
         LoadFromStream(fileName, fs);
      finally
         fs.Free;
      end;
   end;
end;

// LoadFromStream
//
procedure TBaseMesh.LoadFromStream(const fileName : String; aStream : TStream);
var
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if fileName<>'' then begin
      MeshObjects.Clear;
      Skeleton.Clear;
      vectorFileClass:=GetVectorFileFormats.FindFromFileName(filename);
      newVectorFile:=VectorFileClass.Create(Self);
      try
         newVectorFile.ResourceName:=filename;
         PrepareVectorFile(newVectorFile);
         if Assigned(Scene) then Scene.BeginUpdate;
         newVectorFile.LoadFromStream(aStream);
         if Assigned(Scene) then Scene.EndUpdate;
      finally
         newVectorFile.Free;
      end;
      PerformAutoCentering;
      PrepareMesh;
   end;
end;

// AddDataFromFile
//
procedure TBaseMesh.AddDataFromFile(const filename : String);
var
   fs : TFileStream;
begin
   if fileName <> '' then begin
      fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
      try
         AddDataFromStream(fileName, fs);
      finally
         fs.Free;
      end;
   end;
end;

// AddDataFromStream
//
procedure TBaseMesh.AddDataFromStream(const filename : String; aStream : TStream);
var
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if fileName <> '' then begin
      vectorFileClass:=GetVectorFileFormats.FindFromFileName(filename);
      newVectorFile:=VectorFileClass.Create(Self);
      newVectorFile.ResourceName:=filename;
      PrepareVectorFile(newVectorFile);
      try
         if Assigned(Scene) then Scene.BeginUpdate;
         newVectorFile.LoadFromStream(aStream);
         if Assigned(Scene) then Scene.EndUpdate;
      finally
         NewVectorFile.Free;
      end;
      PrepareMesh;
   end;
end;

// GetExtents
//
procedure TBaseMesh.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to MeshObjects.Count-1 do begin
      TMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// SetMaterialLibrary
//
procedure TBaseMesh.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   if FMaterialLibrary<>val then begin
      if Assigned(FMaterialLibrary) then begin
         DestroyHandles;
         FMaterialLibrary.RemoveFreeNotification(Self);
      end;
      FMaterialLibrary:=val;
      if Assigned(FMaterialLibrary) then
         FMaterialLibrary.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetNormalsOrientation
//
procedure TBaseMesh.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   if val<>FNormalsOrientation then begin
      FNormalsOrientation:=val;
      StructureChanged;
   end;
end;

// SetOverlaySkeleton
//
procedure TBaseMesh.SetOverlaySkeleton(const val : Boolean);
begin
   if FOverlaySkeleton<>val then begin
      FOverlaySkeleton:=val;
      NotifyChange(Self);
   end;
end;

// Notification
//
procedure TBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FMaterialLibrary) then
      MaterialLibrary:=nil;
   inherited;
end;

// AxisAlignedDimensions
//
function TBaseMesh.AxisAlignedDimensions : TVector;
var
   dMin, dMax : TAffineVector;
begin
   if FAxisAlignedDimensionsCache[0]<0 then begin
      MeshObjects.GetExtents(dMin, dMax);
      FAxisAlignedDimensionsCache[0]:=MaxFloat(Abs(dMin[0]), Abs(dMax[0]));
      FAxisAlignedDimensionsCache[1]:=MaxFloat(Abs(dMin[1]), Abs(dMax[1]));
      FAxisAlignedDimensionsCache[2]:=MaxFloat(Abs(dMin[2]), Abs(dMax[2]));
   end;
   SetVector(Result, FAxisAlignedDimensionsCache);
   ScaleVector(Result, Scale.DirectVector);
end;

// DestroyHandles
//
procedure TBaseMesh.DestroyHandles;
begin
   if Assigned(FMaterialLibrary) then
      MaterialLibrary.DestroyHandles;
   inherited;
end;

// PrepareVectorFile
//
procedure TBaseMesh.PrepareVectorFile(aFile : TVectorFile);
begin
   aFile.NormalsOrientation:=NormalsOrientation;
end;

// PerformAutoCentering
//
procedure TBaseMesh.PerformAutoCentering;
var
   delta, min, max : TAffineVector;
begin
   GetExtents(min, max);
   if macCenterX in AutoCentering then
      delta[0]:=-0.5*(min[0]+max[0])
   else delta[0]:=0;
   if macCenterY in AutoCentering then
      delta[1]:=-0.5*(min[1]+max[1])
   else delta[1]:=0;
   if macCenterZ in AutoCentering then
      delta[2]:=-0.5*(min[2]+max[2])
   else delta[2]:=0;
   MeshObjects.Translate(delta);
end;

// PrepareMesh
//
procedure TBaseMesh.PrepareMesh;
begin
   StructureChanged;
end;

// SetUseMeshMaterials
//
procedure TBaseMesh.SetUseMeshMaterials(const val : Boolean);
begin
   if val<>FUseMeshMaterials then begin
      FUseMeshMaterials:=val;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TBaseMesh.BuildList(var rci : TRenderContextInfo);
var
   states : TGLStates;
begin
   if rci.materialLibrary<>nil then begin
      states:=rci.currentStates;
      glPushAttrib(GL_ENABLE_BIT);
      MeshObjects.BuildList(rci);
      glPopAttrib;
      rci.currentStates:=states;
   end else MeshObjects.BuildList(rci);
end;

// DoRender
//
procedure TBaseMesh.DoRender(var rci : TRenderContextInfo;
                             renderSelf, renderChildren : Boolean);
begin
   if renderSelf then begin
      // set winding
      case FNormalsOrientation of
         mnoDefault : glFrontFace(GL_CCW);
         mnoInvert : glFrontFace(GL_CW);
      else
         Assert(False);
      end;
      if UseMeshMaterials and Assigned(MaterialLibrary) then
         rci.materialLibrary:=MaterialLibrary
      else rci.materialLibrary:=nil;
      MeshObjects.PrepareBuildList(rci);
      Material.Apply(rci);
      if osDirectDraw in ObjectStyle then
         BuildList(rci)
      else glCallList(GetHandle(rci));
      Material.UnApply(rci);
      rci.materialLibrary:=nil;
      if FNormalsOrientation<>mnoDefault then
         glFrontFace(GL_CCW);
   end;
   if renderChildren and (Count>0) then
      Self.RenderChildren(0, Count-1, rci);
end;

// StructureChanged
//
procedure TBaseMesh.StructureChanged;
begin
   FAxisAlignedDimensionsCache[0]:=-1;
   MeshObjects.Prepare;
   inherited;
end;

// ------------------
// ------------------ TFreeForm ------------------
// ------------------

// Create
//
constructor TFreeForm.Create(AOwner:TComponent);
begin
   inherited;
   FUseMeshMaterials:=True;
end;

// Destroy
//
destructor TFreeForm.Destroy;
begin
   inherited Destroy;
end;

// ------------------
// ------------------ TActorAnimation ------------------
// ------------------

// Create
//
constructor TActorAnimation.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

destructor TActorAnimation.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TActorAnimation.Assign(Source: TPersistent);
begin
	if Source is TActorAnimation then begin
      FName:=TActorAnimation(Source).FName;
      FStartFrame:=TActorAnimation(Source).FStartFrame;
      FEndFrame:=TActorAnimation(Source).FEndFrame;
      FReference:=TActorAnimation(Source).FReference;
	end else inherited;
end;

// GetDisplayName
//
function TActorAnimation.GetDisplayName : String;
begin
	Result:=Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

// FrameCount
//
function TActorAnimation.FrameCount : Integer;
begin
   case Reference of
      aarMorph :
         Result:=TActorAnimations(Collection).Owner.MeshObjects.MorphTargetCount;
      aarSkeleton :
         Result:=TActorAnimations(Collection).Owner.Skeleton.Frames.Count;
   else
      Result:=0;
      Assert(False);
   end;
end;

// SetStartFrame
//
procedure TActorAnimation.SetStartFrame(const val : Integer);
var
   m : Integer;
begin
   if val<0 then
      FStartFrame:=0
   else begin
      m:=FrameCount;
      if val>=m then
         FStartFrame:=m-1
      else FStartFrame:=val;
   end;
   if FStartFrame>FEndFrame then
      FEndFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TActorAnimation.SetEndFrame(const val : Integer);
var
   m : Integer;
begin
   if val<0 then
      FEndFrame:=0
   else begin
      m:=FrameCount;
      if val>=m then
        FEndFrame:=m-1
      else FEndFrame:=val;
   end;
   if FStartFrame>FEndFrame then
      FStartFrame:=FEndFrame;
end;

// SetReference
//
procedure TActorAnimation.SetReference(val : TActorAnimationReference);
begin
   if val<>FReference then begin
      FReference:=val;
      StartFrame:=StartFrame;
      EndFrame:=EndFrame;
   end;
end;

// SetAsString
//
procedure TActorAnimation.SetAsString(const val : String);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.CommaText:=val;
      Assert(sl.Count>=3);
      FName:=sl[0];
      FStartFrame:=StrToInt(sl[1]);
      FEndFrame:=StrToInt(sl[2]);
      if sl.Count=4 then begin
         if LowerCase(sl[3])='morph' then
            Reference:=aarMorph
         else if LowerCase(sl[3])='skeleton' then
            Reference:=aarSkeleton
         else Assert(False);
      end else Reference:=aarMorph;
   finally
      sl.Free;
   end;
end;

// GetAsString
//
function TActorAnimation.GetAsString : String;
const
   cAARToString : array [aarMorph..aarSkeleton] of String = ('morph', 'skeleton');
begin
   Result:=Format('"%s",%d,%d,%s',
                  [FName, FStartFrame, FEndFrame, cAARToString[Reference]]);
end;

// ------------------
// ------------------ TActorAnimations ------------------
// ------------------

// Create
//
constructor TActorAnimations.Create(AOwner : TActor);
begin
	Owner:=AOwner;
	inherited Create(TActorAnimation);
end;

// GetOwner
//
function TActorAnimations.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

// SetItems
//
procedure TActorAnimations.SetItems(index : Integer; const val : TActorAnimation);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TActorAnimations.GetItems(index : Integer) : TActorAnimation;
begin
	Result:=TActorAnimation(inherited Items[index]);
end;

// Add
//
function TActorAnimations.Add: TActorAnimation;
begin
	Result:=(inherited Add) as TActorAnimation;
end;

// FindItemID
//
function TActorAnimations.FindItemID(ID: Integer): TActorAnimation;
begin
	Result:=(inherited FindItemID(ID)) as TActorAnimation;
end;

// FindName
//
function TActorAnimations.FindName(const aName : String) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do if CompareText(Items[i].Name, aName)=0 then begin
      Result:=Items[i];
      Break;
   end;
end;

// FindFrame
//
function TActorAnimations.FindFrame(aFrame : Integer;
                        aReference : TActorAnimationReference) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do with Items[i] do
      if (StartFrame<=aFrame) and (EndFrame>=aFrame)
            and (Reference=aReference) then begin
         Result:=Items[i];
         Break;
      end;
end;

// SetToStrings
//
procedure TActorAnimations.SetToStrings(aStrings : TStrings);
var
   i : Integer;
begin
   with aStrings do begin
      BeginUpdate;
      Clear;
      for i:=0 to Self.Count-1 do
         Add(Self.Items[i].Name);
      EndUpdate;
   end;
end;

// SaveToStream
//
procedure TActorAnimations.SaveToStream(aStream : TStream);
var
   i : Integer;
begin
   WriteCRLFString(aStream, cAAFHeader);
   WriteCRLFString(aStream, IntToStr(Count));
   for i:=0 to Count-1 do
      WriteCRLFString(aStream, Items[i].AsString);
end;

// LoadFromStream
//
procedure TActorAnimations.LoadFromStream(aStream : TStream);
var
   i, n : Integer;
begin
   Clear;
   Assert(ReadCRLFString(aStream)=cAAFHeader);
   n:=StrToInt(ReadCRLFString(aStream));
   for i:=0 to n-1 do
      Add.AsString:=ReadCRLFString(aStream);
end;

// SaveToFile
//
procedure TActorAnimations.SaveToFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TActorAnimations.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// ------------------
// ------------------ TAnimationControler ------------------
// ------------------

// Create
//
constructor TAnimationControler.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
end;

// Destroy
//
destructor TAnimationControler.Destroy;
begin
	inherited Destroy;
end;

// Notification
//
procedure TAnimationControler.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (AComponent=FActor) and (Operation=opRemove) then
      SetActor(nil); 
end;

// SetActor
//
procedure TAnimationControler.SetActor(const val : TActor);
begin
   FActor:=val;
end;

// SetAnimationName
//
procedure TAnimationControler.SetAnimationName(const val : TActorAnimationName);
begin
   if FAnimationName<>val then begin
      FAnimationName:=val;
   end;
end;

// ------------------
// ------------------ TActor ------------------
// ------------------

// Create
//
constructor TActor.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FFrameInterpolation:=afpLinear;
   FAnimationMode:=aamNone;
   FInterval:=100; // 10 animation frames per second
   FAnimations:=TActorAnimations.Create(Self);
end;

// Destroy
//
destructor TActor.Destroy;
begin
   FAnimations.Free;
   inherited Destroy;
end;

// SetCurrentFrame
//
procedure TActor.SetCurrentFrame(val : Integer);
begin
   if val<>CurrentFrame then begin
      if val>FrameCount-1 then
         FCurrentFrame:=FrameCount-1
      else if val<0 then
         FCurrentFrame:=0
      else FCurrentFrame:=val;
      FCurrentFrameDelta:=0;
      case AnimationMode of
         aamPlayOnce :
            if CurrentFrame=EndFrame then FAnimationMode:=aamNone;
         aamBounceForward :
            if CurrentFrame=EndFrame then FAnimationMode:=aamBounceBackward;
         aamBounceBackward :
            if CurrentFrame=StartFrame then FAnimationMode:=aamBounceForward;
      end;
      StructureChanged;
      if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
   end;
end;

// SetStartFrame
//
procedure TActor.SetStartFrame(val : Integer);
begin
   if (val>=0) and (val<FrameCount) and (val<>StartFrame) then
      FStartFrame:=val;
   if EndFrame<StartFrame then
      FEndFrame:=FStartFrame;
   if CurrentFrame<StartFrame then
      CurrentFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TActor.SetEndFrame(val : Integer);
begin
   if (val>=0) and (val<FrameCount) and (val<>EndFrame) then
      FEndFrame:=val;
   if CurrentFrame>EndFrame then
      CurrentFrame:=FEndFrame;
end;

// SetReference
//
procedure TActor.SetReference(val : TActorAnimationReference);
begin
   if val<>Reference then begin
      FReference:=val;
      StartFrame:=StartFrame;
      EndFrame:=EndFrame;
      CurrentFrame:=CurrentFrame;
      StructureChanged;
   end;
end;

// SetAnimations
//
procedure TActor.SetAnimations(const val : TActorAnimations);
begin
   FAnimations.Assign(val);
end;

// NextFrameIndex
//
function TActor.NextFrameIndex : Integer;
begin
   case AnimationMode of
      aamNone, aamPlayOnce, aamLoop, aamBounceForward : begin
         Result:=CurrentFrame+1;
         if Result>EndFrame then begin
            Result:=StartFrame+(Result-EndFrame-1);
            if Result>EndFrame then
               Result:=EndFrame;
         end;
      end;
      aamBounceBackward : begin
         Result:=CurrentFrame-1;
         if Result<StartFrame then begin
            Result:=EndFrame-(StartFrame-Result-1);
            if Result<StartFrame then
               Result:=StartFrame;
         end;
      end;
   else
      Result:=CurrentFrame;
      Assert(False);
   end;
end;

// NextFrame
//
procedure TActor.NextFrame(nbSteps : Integer = 1);
var
   n : Integer;
begin
   n:=nbSteps;
   while n>0 do begin
      CurrentFrame:=NextFrameIndex;
      Dec(n);
      if Assigned(FOnEndFrameReached) and (CurrentFrame=EndFrame) then
         FOnEndFrameReached(Self);
      if Assigned(FOnStartFrameReached) and (CurrentFrame=StartFrame) then
         FOnStartFrameReached(Self);
   end;
end;

// PrevFrame
//
procedure TActor.PrevFrame(nbSteps : Integer = 1);
var
   value : Integer;
begin
   value:=FCurrentFrame-nbSteps;
   if value<FStartFrame then begin
      value:=FEndFrame-(FStartFrame-value);
      if value<FStartFrame then
         value:=FStartFrame;
   end;
   CurrentFrame:=Value;
end;

// BuildList
//
procedure TActor.BuildList(var rci : TRenderContextInfo);
begin
   if NextFrameIndex>=0 then begin
      case Reference of
         aarMorph : begin
            case FrameInterpolation of
               afpLinear :
                  MeshObjects.Lerp(CurrentFrame, NextFrameIndex, CurrentFrameDelta)
            else
               MeshObjects.MorphTo(CurrentFrame);
            end;
         end;
         aarSkeleton : begin
            case FrameInterpolation of
               afpLinear :
                  Skeleton.Lerp(CurrentFrame, NextFrameIndex, CurrentFrameDelta);
            else
               Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
            end;
            Skeleton.MorphMesh;
         end;
      end;
   end;
   inherited;
   if OverlaySkeleton then begin
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      Skeleton.RootBones.BuildList(rci);
      glPopAttrib;
   end;
end;

// PrepareMesh
//
procedure TActor.PrepareMesh;
begin
   FStartFrame:=0;
   FEndFrame:=FrameCount-1;
   FCurrentFrame:=0;
   if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
   inherited;
end;

// FrameCount
//
function TActor.FrameCount : Integer;
begin
   case Reference of
      aarMorph :
         Result:=MeshObjects.MorphTargetCount;
      aarSkeleton :
         Result:=Skeleton.Frames.Count;
   else
      Result:=0;
      Assert(False);
   end;
end;

// DoProgress
//
procedure TActor.DoProgress(const deltaTime, newTime : Double);
var
   fDelta : Single;
begin
   inherited;
   if (AnimationMode<>aamNone) and (Interval>0) then begin
      if (StartFrame<>EndFrame) and (FrameCount>1)  then begin
         FCurrentFrameDelta:=FCurrentFrameDelta+(deltaTime*1000)/FInterval;
         if FCurrentFrameDelta>1 then begin
            // we need to step on
            fDelta:=Frac(FCurrentFrameDelta);
            NextFrame(Trunc(FCurrentFrameDelta));
            FCurrentFrameDelta:=fDelta;
            StructureChanged;
         end else if FrameInterpolation<>afpNone then
            StructureChanged;
      end;
   end;
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(const animationName : String);
begin
   SwitchToAnimation(Animations.FindName(animationName));
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(animationIndex : Integer);
begin
   if (animationIndex>=0) and (animationIndex<Animations.Count) then
      SwitchToAnimation(Animations[animationIndex]);
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(anAnimation : TActorAnimation);
begin
   if Assigned(anAnimation) then begin
      Reference:=anAnimation.Reference;
      StartFrame:=anAnimation.StartFrame;
      EndFrame:=anAnimation.EndFrame;
      CurrentFrame:=StartFrame;
   end;
end;

// CurrentAnimation
//
function TActor.CurrentAnimation : String;
var
   aa : TActorAnimation;
begin
   aa:=Animations.FindFrame(CurrentFrame, Reference);
   if Assigned(aa) then
      Result:=aa.Name
   else Result:='';
end;

// Synchronize
//
procedure TActor.Synchronize(referenceActor : TActor);
begin
   if Assigned(referenceActor) then begin
      FStartFrame:=referenceActor.StartFrame;
      FEndFrame:=referenceActor.EndFrame;
      FReference:=referenceActor.Reference;
      CurrentFrame:=referenceActor.CurrentFrame;
      CurrentFrameDelta:=referenceActor.CurrentFrameDelta;
      AnimationMode:=referenceActor.AnimationMode;
      FrameInterpolation:=referenceActor.FrameInterpolation;
   end;
end;

// ------------------
// ------------------ TGLMD2VectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLMD2VectorFile.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   MD2File : TFileMD2;
   mesh : TMorphableMeshObject;
   faceGroup : TFGIndexTexCoordList;
   morphTarget : TMeshMorphTarget;
begin
   MD2File:=TFileMD2.Create;
   MD2File.LoadFromStream(aStream);
   try
      // retrieve mesh data
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      with mesh, MD2File do begin
         Mode:=momFaceGroups;
         faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
         with faceGroup do begin
            MaterialName:='';
            VertexIndices.Capacity:=m_iTriangles*3;
            TexCoords.Capacity:=m_iTriangles*3;
            // copy the face list
            for i:=0 to m_iTriangles-1 do with IndexList(m_index_list)[i] do begin
               Add(a, a_s, -a_t);
               Add(b, b_s, -b_t);
               Add(c, c_s, -c_t);
            end;
         end;
         // retrieve frames data (morph targets)
         for i:=0 to m_iFrames-1 do begin
            morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
            with morphTarget do begin
               Name:='Frame'+IntToStr(i);
               Vertices.Capacity:=m_iVertices;
               for j:=0 to m_iVertices-1 do
                  with VertList(frameList(m_frame_list)[i].vertex)[j] do
                     Vertices.Add(x, y, z);
               BuildNormals(faceGroup.VertexIndices, momTriangles);
            end;
         end;
      end;
      if GetOwner is TActor then with TActor(GetOwner).Animations do begin
         Clear;
         with MD2File do for i:=0 to frameNames.Count-1 do with Add do begin
            Name:=frameNames[i];
            Reference:=aarMorph;
            StartFrame:=Integer(frameNames.Objects[i]);
            if i<frameNames.Count-1 then
               EndFrame:=Integer(frameNames.Objects[i+1])-1
            else EndFrame:=m_iFrames-1;
         end;
      end;
   finally
      MD2File.Free;
   end;
end;

// ------------------
// ------------------ TGLTINVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLTINVectorFile.LoadFromStream(aStream : TStream);
var
   i : Integer;
   sl, tl : TStringList;
   mesh : TMeshObject;
   v1, v2, v3, n : TAffineVector;
begin
   sl:=TStringList.Create;
   tl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Mode:=momTriangles;
      for i:=0 to sl.Count-1 do if Copy(sl[i], 1, 2)='t ' then begin
         tl.CommaText:=Trim(Copy(sl[i], 3, MaxInt));
         if tl.Count=9 then begin
            SetVector(v1, StrToFloatDef(tl[0]), StrToFloatDef(tl[1]), StrToFloatDef(tl[2]));
            SetVector(v2, StrToFloatDef(tl[3]), StrToFloatDef(tl[4]), StrToFloatDef(tl[5]));
            SetVector(v3, StrToFloatDef(tl[6]), StrToFloatDef(tl[7]), StrToFloatDef(tl[8]));
            mesh.Vertices.Add(v1, v2, v3);
            n:=CalcPlaneNormal(v1, v2, v3);
            mesh.Normals.Add(n, n, n);
         end;
      end;
   finally
      tl.Free;
      sl.Free;
   end;
end;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLSTLVectorFile.LoadFromStream(aStream : TStream);
var
   i : Integer;
   mesh : TMeshObject;
   header : TSTLHeader;
   dataFace : TSTLFace;
   calcNormal : TAffineVector;
begin
   mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
   mesh.Mode:=momTriangles;
   aStream.Read(header, SizeOf(TSTLHeader));
   for i:=0 to header.nbFaces-1 do begin
      aStream.Read(dataFace, SizeOf(TSTLFace));
      with dataFace, mesh do begin
         // STL faces have a normal, but do not necessarily follow the winding rule,
         // so we must first determine if the triangle is properly oriented
         // and rewind it properly if not...
         calcNormal:=CalcPlaneNormal(v1, v2, v3);
         if VectorDotProduct(calcNormal, normal)>0 then
            Vertices.Add(v1, v2, v3)
         else Vertices.Add(v3, v2, v1);
         Normals.Add(normal, normal, normal);
      end;
   end;
end;

// ------------------
// ------------------ TGLSMDVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLSMDVectorFile.LoadFromStream(aStream : TStream);

   procedure AllocateMaterial(const name : String);
   var
      matLib : TGLMaterialLibrary;
   begin
      if Owner is TBaseMesh then begin
         matLib:=TBaseMesh(GetOwner).MaterialLibrary;
         if Assigned(matLib) then
            if matLib.Materials.GetLibMaterialByName(name)=nil then
               if CompareText(name, 'null.bmp')<>0 then
                  matLib.AddTextureMaterial(name, name)
               else matLib.AddTextureMaterial(name, '');
      end;
   end;

var
   i, k, nVert, nTex, firstFrame : Integer;
   mesh : TSkeletonMeshObject;
   sl, tl : TStringList;
   bone : TSkeletonBone;
   frame : TSkeletonFrame;
   faceGroup : TFGVertexNormalTexIndexList;
   v : TAffineVector;
begin
   sl:=TStringList.Create;
   tl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      if sl[0]<>'version 1' then
         raise Exception.Create('SMD version 1 required');
      if sl[1]<>'nodes' then
         raise Exception.Create('nodes not found');
      if sl.IndexOf('triangles')>=0 then begin
         mesh:=TSkeletonMeshObject.CreateOwned(Owner.MeshObjects);
         mesh.Mode:=momFaceGroups;
      end else if Owner.MeshObjects.Count>0 then
         mesh:=(Owner.MeshObjects[0] as TSkeletonMeshObject)
      else raise Exception.Create('SMD is an animation, load model SMD first.');
      // read skeleton nodes
      i:=2;
      if Owner.Skeleton.RootBones.Count=0 then begin
         // new bone structure
         while sl[i]<>'end' do begin
            tl.CommaText:=sl[i];
            with Owner.Skeleton do
               if (tl[2]<>'-1') then
                  bone:=TSkeletonBone.CreateOwned(RootBones.BoneByID(StrToInt(tl[2])))
               else bone:=TSkeletonBone.CreateOwned(RootBones);
            if Assigned(bone) then begin
               bone.BoneID:=StrToInt(tl[0]);
               bone.Name:=tl[1];
            end;
            Inc(i);
         end;
      end else begin
         // animation file, skip structure
         while sl[i]<>'end' do Inc(i);
      end;
      Inc(i);
      if sl[i]<>'skeleton' then
         raise Exception.Create('skeleton not found');
      Inc(i);
      // read animation time frames
      firstFrame:=Owner.Skeleton.Frames.Count;
      while sl[i]<>'end' do begin
         if Copy(sl[i], 1, 5)<>'time ' then
            raise Exception.Create('time not found, got: '+sl[i]);
         frame:=TSkeletonFrame.CreateOwned(Owner.Skeleton.Frames);
         frame.Name:=ResourceName+' '+sl[i];
         Inc(i);
         while Pos(Copy(sl[i], 1, 1), ' 1234567890')>0 do begin
            tl.CommaText:=sl[i];
            Assert(StrToInt(tl[0])=frame.Position.Count, 'Invalid vertex count: '+tl[0]+' vs '+IntToStr(frame.Position.Count));
            frame.Position.Add(StrToFloatDef(tl[1]), StrToFloatDef(tl[2]), StrToFloatDef(tl[3]));
            v:=AffineVectorMake(StrToFloatDef(tl[4]), StrToFloatDef(tl[5]), StrToFloatDef(tl[6]));
            frame.Rotation.Add(v);
            Inc(i);
         end;
      end;
      if Owner is TActor then with TActor(Owner).Animations.Add do begin
         k:=Pos('.', ResourceName);
         if k>0 then
            Name:=Copy(ResourceName, 1, k-1)
         else Name:=ResourceName;
         Reference:=aarSkeleton;
         StartFrame:=firstFrame;
         EndFrame:=Self.Owner.Skeleton.Frames.Count-1;
      end;
      Inc(i);
      if (i<sl.Count) and (sl[i]='triangles') then begin
         // read optional mesh data
         Inc(i);
         faceGroup:=nil;
         while sl[i]<>'end' do begin
            if (faceGroup=nil) or (faceGroup.MaterialName<>sl[i]) then begin
               faceGroup:=TFGVertexNormalTexIndexList.CreateOwned(mesh.FaceGroups);
               faceGroup.Mode:=fgmmTriangles;
               faceGroup.MaterialName:=sl[i];
               AllocateMaterial(sl[i]);
            end;
            Inc(i);
            for k:=1 to 3 do with mesh do begin
               tl.CommaText:=sl[i];
               nVert:=FindOrAdd(StrToInt(tl[0]),
                                AffineVectorMake(StrToFloatDef(tl[1]), StrToFloatDef(tl[2]), StrToFloatDef(tl[3])),
                                AffineVectorMake(StrToFloatDef(tl[4]), StrToFloatDef(tl[5]), StrToFloatDef(tl[6])));
               nTex:=TexCoords.FindOrAdd(AffineVectorMake(StrToFloatDef(tl[7]), StrToFloatDef(tl[8]), 0));
               faceGroup.Add(nVert, nVert, nTex);
               Inc(i);
            end;
         end;
         Owner.Skeleton.RootBones.PrepareGlobalMatrices;
         mesh.PrepareBoneMatrixInvertedMeshes;
      end;
   finally
      tl.Free;
      sl.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('md2', 'Quake II model files', TGLMD2VectorFile);
   RegisterVectorFileFormat('3ds', '3D Studio files', TGL3DSVectorFile);
   RegisterVectorFileFormat('prj', '3D Studio project files', TGL3DSVectorFile);
   RegisterVectorFileFormat('tin', 'Triangular Irregular Network', TGLTINVectorFile);
   RegisterVectorFileFormat('stl', 'Stereolithography files', TGLSTLVectorFile);
   RegisterVectorFileFormat('smd', 'Half-Life SMD files', TGLSMDVectorFile);
   RegisterClasses([TFreeForm, TActor, TSkeleton, TSkeletonFrame, TSkeletonBone,
                    TSkeletonMeshObject, TMeshObject, TSkeletonFrame,
                    TMorphableMeshObject, TFaceGroup, TFGVertexIndexList,
                    TFGVertexNormalTexIndexList, TAnimationControler]);

finalization

   vVectorFileFormats.Free;

end.

