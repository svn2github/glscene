{: GLMisc<p>

   Miscellaneous support routines & classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>07/01/02 - EG - TGLNodes.Barycenter fix (thx Bob)
      <li>15/12/01 - EG - Added support for cube maps
      <li>14/09/01 - EG - Addition of vFileStreamClass
      <li>04/09/01 - EG - SetGLCurrentTexture stuff
      <li>18/07/01 - EG - Added TGLVisibilityCulling
      <li>08/07/01 - EG - Changes in TGLNodes based on code from Uwe Raabe
      <li>19/06/01 - EG - Added StrToFloatDef
      <li>16/03/01 - EG - Added Capabilities to TDataFile
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>05/02/01 - EG - Faster SetGLMaterialColors
      <li>15/01/01 - EG - Added SizeOfFile
      <li>04/01/00 - EG - Added AsAffineVector to TGLNode
      <li>22/12/00 - EG - Fixed TGLNodes.Vector when there is only one node
      <li>03/11/00 - EG - Added TGLCoordinates.AsAffineVector
      <li>08/10/00 - EG - Added "Style" to TGLCoordinates to detect some misuses
      <li>06/08/00 - EG - TGLCoordinates moved in, added TextureMatrix stuff,
                          added TGLNodes.AddXYArc
      <li>19/07/00 - EG - Improvements to TGLNodes (tessellation, scaling...)
      <li>16/07/00 - EG - Added "Managers" support classes,
                          Added TDataFile
      <li>11/07/00 - EG - Added 'Sender' to MotifyChange
      <li>05/07/00 - EG - Added Begin/EndUpdate to TGLNodes
      <li>23/06/00 - EG - Added Read/WriteCRLFString
      <li>18/06/00 - EG - Added update control to TGLUpdateAbleObject
      <li>09/06/00 - EG - Added TGLCadenceAbleComponent
      <li>07/06/00 - EG - Added RemoveFreeNotification for Delphi 4
      <li>29/05/00 - EG - Added TGLNode/TGLNodes
      <li>26/05/00 - EG - TMeshMode & TVertexMode moved in
		<li>22/03/00 - EG - Added SetGLState/UnSetGLState
		<li>21/03/00 - EG - Added SaveStringToFile/LoadStringFromFile
		<li>18/03/00 - EG - Added GetSqrt255Array
      <li>06/02/00 - EG - Javadocisation, RoundUpToPowerOf2,
                          RoundDownToPowerOf2 and IsPowerOf2 moved in
   </ul></font>

   TODO : separating misc stuff from base classes and OpenGL support

}
unit GLMisc;

// GLMisc      - miscellaneous support routines
// version     - 0.1.0
// last change - 31. January 1999
// for more information see help file

interface

uses Classes, Geometry, SysUtils, OpenGL12, Spline;

{$i GLScene.inc}

type

	TGLMinFilter   = (miNearest, miLinear, miNearestMipmapNearest,
							miLinearMipmapNearest, miNearestMipmapLinear,
							miLinearMipmapLinear);
	TGLMagFilter   = (maNearest, maLinear);

	// used to describe what kind of winding has a front face
	TFaceWinding = (fwCounterClockWise, fwClockWise);

	// used to reflect all relevant (binary) states of OpenGL subsystem
	TGLState = (stAlphaTest, stAutoNormal,
					stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
					stFog, stLighting, stLineSmooth, stLineStipple,
					stLogicOp, stNormalize, stPointSmooth, stPolygonSmooth,
					stPolygonStipple, stScissorTest, stStencilTest,
					stTexture1D, stTexture2D, stTextureCubeMap);
	TGLStates = set of TGLState;

   TMeshMode = (mmTriangleStrip, mmTriangleFan, mmTriangles,
                mmQuadStrip, mmQuads, mmPolygon);
   TVertexMode = (vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT);

const
   cMeshModeToGLEnum : array [Low(TMeshMode)..High(TMeshMode)] of TGLEnum =
                     (GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
                      GL_QUAD_STRIP, GL_QUADS, GL_POLYGON);
   cVertexModeToGLEnum : array [Low(TVertexMode)..High(TVertexMode)] of TGLEnum =
                     (GL_V3F, GL_N3F_V3F, GL_C4F_N3F_V3F, GL_T2F_C4F_N3F_V3F,
                      GL_T2F_N3F_V3F, GL_T2F_V3F);

type

   // TGLObjectsSorting
   //
   {: Determines if objects are sorted, and how.<p>
      Sorting is done level by level (and not for all entities), values are :<ul>
      <li>osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
      <li>osNone : do not sort objects.
		<li>osRenderFarthestFirst : render objects whose Position is the farthest from
			the camera first.
      <li>osRenderBlendedLast : opaque objects are not sorted and rendered
         first, blended ones are rendered afterwards and depth sorted.
       </ul> }
   TGLObjectsSorting = (osInherited, osNone, osRenderFarthestFirst, osRenderBlendedLast);

   // TGLVisibilityCulling
   //
   {: Determines the visibility culling mode.
      Culling is done level by level, allowed values are:<ul>
      <li>vcInherited : use inherited culling value, if selected for the root
         level, defaults to vcNone
      <li>vcNone : no visibility culling is performed
      <li>vcObjectBased : culling is done on a per-object basis, each object may
         or may not be culled base on its own AxisAlignedDimensions,
         culling has no impact on the visibility of its children
      <li>vcHierarchical : culling is performed hierarchically, using hierarchical
         bounding boxes, if a parent is culled, all of its children, whatever their
         culling options are invisible.
      <li><br>Depending on the structure of your scene the most efficient culling
      method will be either vcObjectBased or vcHierarchical. Also note that if
      you use many objects with "static" geometry and have a T&amp;L graphics
      board, it may be faster not to cull at all (ie. leave this to the hardware). }
   TGLVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical); 

   // TGLUpdateAbleObject
   //
   {: An abstract class describing the "update" interface.<p> }
   TGLUpdateAbleObject = class (TPersistent)
      private
	      { Private Declarations }
         FOwner : TPersistent;
         FUpdating : Integer;
         FOnNotifyChange : TNotifyEvent;

      public
	      { Public Declarations }
         constructor Create(AOwner: TPersistent); virtual;

			procedure NotifyChange(Sender : TObject); virtual;
         function GetOwner : TPersistent; override;

         property Updating : Integer read FUpdating;
         procedure BeginUpdate;
         procedure EndUpdate;

         property Owner : TPersistent read FOwner;
         property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
	end;

   // TProgressTimes
   //
   TProgressTimes = record
      deltaTime, newTime : Double
   end;

	// TGLCadenceAbleComponent
	//
	{: An base class describing the "cadencing" interface.<p> }
	TGLCadenceAbleComponent = class (TComponent)
		public
	      { Public Declarations }
{$ifndef GLS_DELPHI_5_UP}
         procedure RemoveFreeNotification(AComponent: TComponent);
{$endif}
			procedure DoProgress(const progressTime : TProgressTimes); virtual;
	end;

	// TGLUpdateAbleComponent
	//
	{: An base class describing the "update" interface.<p> }
	TGLUpdateAbleComponent = class (TGLCadenceAbleComponent)
		public
	      { Public Declarations }
			procedure NotifyChange(Sender : TObject); virtual;
	end;

   // TGLCoordinatesStyle
   //
   {: Identifie le type de données stockées au sein d'un TGLCoordinates.<p>
      <ul><li>csPoint : un point (W=1)
      <li>csVector : un vecteur (W=0)
      <li>csUnknown : aucune contrainte
      </ul> }
   TGLCoordinatesStyle = (csPoint, csVector, csUnknown);

	// TGLCoordinates
	//
	{: Stores and homogenous vector.<p>
		This class is basicly a container for a TVector, allowing proper use of
		delphi property editors and editing in the IDE. Vector/Coordinates
		manipulation methods are only minimal.<br>
		Handles dynamic default values to save resource file space.<p>
		Note : only affine components are published. }
	TGLCoordinates = class(TGLUpdateAbleObject)
		private
			{ Private Declarations }
         FStyle : TGLCoordinatesStyle; // NOT Persistent
			FCoords, FDefaultCoords : TVector;
			procedure SetAsVector(const value : TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
         function GetAsAffineVector : TAffineVector;
			procedure SetCoordinate(Index: Integer; AValue: TGLFloat);

		protected
			{ Protected Declarations }
			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

		public
			{ Public Declarations }
         constructor CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                       const aStyle : TGLCoordinatesStyle = csUnknown);

			procedure Assign(Source: TPersistent); override;
         procedure WriteToFiler(writer : TWriter);
         procedure ReadFromFiler(reader : TReader);

         procedure Initialize(const value : TVector);
			procedure NotifyChange(Sender : TObject); override;

         {: Identifies the coordinates styles.<p>
            The property is NOT persistent, csUnknown by default, and should be
            managed by owner object only (internally).<p>
            It is used by the TGLCoordinates for internal "assertion" checks
            to detect "misuses" or "misunderstandings" of what the homogeneous
            coordinates system implies. }
         property Style : TGLCoordinatesStyle read FStyle write FStyle;

			procedure Translate(const translationVector : TVector); overload;
			procedure Translate(const translationVector : TAffineVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TAffineVector); overload;
         procedure Rotate(const Axis : TAffineVector; Angle: Single);
         procedure Normalize;
         procedure Invert;
         procedure Scale(factor : Single);
         function  VectorLength : TGLFloat;
         function  Equals(const aVector : TVector) : Boolean;
         procedure SetVector(const x, y, z : Single); overload;
         procedure SetVector(const x, y, z, w : Single); overload;
         procedure SetPoint(const x, y, z : Single);

         function AsAddress : PGLFloat;

         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;
         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate;

         //: Similar to AsVector but does not trigger notification events
         property DirectVector : TVector read FCoords write FCoords;
         property DirectX : TGLFloat read FCoords[0] write FCoords[0];
         property DirectY : TGLFloat read FCoords[1] write FCoords[1];
         property DirectZ : TGLFloat read FCoords[2] write FCoords[2];
         property DirectW : TGLFloat read FCoords[3] write FCoords[3];

		published
			{ Published Declarations }
			property X: TGLFloat index 0 read FCoords[0] write SetCoordinate stored False;
			property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate stored False;
			property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate stored False;

  	end;

	// TGLNode
	//
	TGLNode = class (TCollectionItem)
	   private
	      { Private Declarations }
			FCoords : TVector;
			procedure SetAsVector(const value: TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
         function GetAsAffineVector : TAffineVector;
			procedure SetCoordinate(Index: Integer; AValue: TGLFloat);

	   protected
	      { Protected Declarations }
         function StoreCoordinate(Index: Integer) : Boolean;

         function GetDisplayName : String; override;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function AsAddress : PGLFloat;
         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;
         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate stored StoreCoordinate;

	   published
	      { Published Declarations }
			property X: TGLFloat index 0 read FCoords[0] write SetCoordinate stored StoreCoordinate;
			property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate stored StoreCoordinate;
			property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate stored StoreCoordinate;
	end;

	// TGLNodes
	//
	TGLNodes = class (TOwnedCollection)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLNode);
	      function GetItems(index : Integer) : TGLNode;
         procedure Update(Item: TCollectionItem); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TPersistent; ItemClass: TCollectionItemClass = nil);
         function CreateCopy(AOwner : TPersistent) : TGLNodes;

         function Add : TGLNode;
	      function FindItemID(ID : Integer) : TGLNode;
	      property Items[index : Integer] : TGLNode read GetItems write SetItems; default;

         procedure NotifyChange; virtual;
         procedure EndUpdate; override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;
         procedure AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);

         //: Calculates and returns the barycenter of the nodes
         function Barycenter : TAffineVector;
         //: Returns normalized vector Nodes[i+1]-Nodes[i]
         function Vector(i : Integer) : TAffineVector;

         {: Calculates the extents of the nodes (min-max for all coordinates).<p>
            The returned values are also the two corners of the axis-aligned
            bounding box. }
         procedure GetExtents(var min, max : TAffineVector);
         //: Translate all nodes
         procedure Translate(const tv : TAffineVector);
         //: Scale all node coordinates
         procedure Scale(const fv : TAffineVector); overload;
         //: Scale all node coordinates
         procedure Scale(f : Single); overload;
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundX(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundY(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundZ(angle : Single);

         procedure RenderTesselatedPolygon(textured : Boolean;
                                           normal : PAffineVector = nil;
                                           splineDivisions : Integer = 1;
                                           invertNormals : Boolean = False);

         function CreateNewCubicSpline : TCubicSpline;

   end;

   TGLNodesClass = class of TGLNodes;

   // TDataFileCapabilities
   //
   TDataFileCapability = (dfcRead, dfcWrite);
   TDataFileCapabilities = set of TDataFileCapability;

   // TDataFile
   //
   {: Abstract base class for data file formats interfaces.<p>
      This class declares base file-related behaviours, ie. ability to load/save
      from a file or a stream.<p>
      It is highly recommended to overload ONLY the stream based methods, as the
      file-based one just call these, and stream-based behaviours allow for more
      enhancement (such as other I/O abilities, compression, cacheing, etc.)
      to this class, without the need to rewrite subclasses. }
   TDataFile = class (TPersistent)
      private
         { Private Declarations }
         FOwner : TPersistent;
         FResourceName : String;

      protected
         { Protected Declarations }
         function GetOwner : TPersistent; override;

      public
         { Public Declarations }
	      constructor Create(AOwner: TPersistent);
         destructor Destroy; override;

         {: Describes what the TDataFile is capable of.<p>
            Default value is [dfcRead]. }
         class function Capabilities : TDataFileCapabilities; virtual;

         {: Duplicates Self and returns a copy.<p>
            Subclasses should override this method to duplicate their data. }
         function CreateCopy(AOwner: TPersistent) : TDataFile; dynamic;

         procedure LoadFromFile(const fileName : String); dynamic;
         procedure SaveToFile(const fileName : String); dynamic;
         procedure LoadFromStream(stream : TStream); dynamic; abstract;
         procedure SaveToStream(stream : TStream); dynamic;

         {: Optionnal resource name.<p>
            When using LoadFromFile/SaveToFile, the filename is placed in it,
            when using the Stream variants, the caller may place the resource
            name in it for parser use. }
         property ResourceName : String read FResourceName write FResourceName;
   end;

   TDataFileClass = class of TDataFile;

	TSqrt255Array = array [0..255] of Byte;
	PSqrt255Array = ^TSqrt255Array;

   TFileStreamClass = class of TFileStream;

//: Copies the values of Source to Dest (converting word values to integer values)
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal);
//: Round ups to the nearest power of two, value must be positive
function RoundUpToPowerOf2(value : Integer): Integer;
//: Round down to the nearest power of two, value must be strictly positive
function RoundDownToPowerOf2(value : Integer): Integer;
//: Returns True if value is a true power of two
function IsPowerOf2(value : Integer) : Boolean;
//: Normalize and angle in degrees in the -180 +180 range
function NormalizeAngle(angle : Single) : Single;
{: Read a CRLF terminated string from a stream.<p>
   The CRLF is NOT in the returned string. }
function ReadCRLFString(aStream : TStream) : String;
//: Write the string and a CRLF in the stream
procedure WriteCRLFString(aStream : TStream; const aString : String);
//: StrToFloatDef
function StrToFloatDef(strValue : String; defValue : Extended = 0) : Extended;

{: Returns a pointer to an array containing the results of "255*sqrt(i/255)". }
function GetSqrt255Array : PSqrt255Array;

{: Saves "data" to "filename". }
procedure SaveStringToFile(const fileName, data : String);
{: Returns the content of "filename". }
function LoadStringFromFile(const fileName : String) : String;
{: Returns the size of "filename".<p>
   Returns 0 (zero) is file does not exists. }
function SizeOfFile(const fileName : String) : Integer;

//: Update the GLState machine if necessary
procedure SetGLState(var states : TGLStates; const aState : TGLState);
//: Update the GLState machine if necessary
procedure UnSetGLState(var states : TGLStates; const aState : TGLState);

//: Defines the GLPolygonMode if necessary
procedure SetGLPolygonMode(const aFace, mode : TGLEnum);
//: Reset GLPolygonMode, next calls to SetGLPolygonMode WILL do something
procedure ResetGLPolygonMode;

procedure SetGLMaterialColors(const aFace : TGLEnum;
                        const emission, ambient, diffuse, specular : PGLFloat;
                        const shininess : Integer);
procedure SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
procedure ResetGLMaterialColors;

procedure SetGLCurrentTexture(const textureUnit, target, handle : Integer);
procedure ResetGLCurrentTexture;

{: Defines the OpenGL texture matrix.<p>
   Assumed texture mode is GL_MODELVIEW. }
procedure SetGLTextureMatrix(const matrix : TMatrix);
{: Resets the OpenGL texture matrix to Identity.<p>
   Assumed texture mode is GL_MODELVIEW. }
procedure ResetGLTextureMatrix;

{: Inverts front face winding (CCW/CW). }
procedure InvertGLFrontFace;
{: Reset to default front face winding (CCW). }
procedure ResetGLFrontFace;

procedure RegisterManager(aManager : TComponent);
procedure DeRegisterManager(aManager : TComponent);
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;

var
   // Class of file streams to use throughout all of GLScene
   vFileStreamClass : TFileStreamClass = TFileStream;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------

uses GLScene, XOpenGL;

const
	cGLStateToGLEnum : array [stAlphaTest..stTextureCubeMap] of TGLEnum =
		(GL_ALPHA_TEST, GL_AUTO_NORMAL, GL_BLEND, GL_COLOR_MATERIAL, GL_CULL_FACE,
		 GL_DEPTH_TEST, GL_DITHER, GL_FOG, GL_LIGHTING, GL_LINE_SMOOTH,
		 GL_LINE_STIPPLE, GL_LOGIC_OP, GL_NORMALIZE, GL_POINT_SMOOTH,
		 GL_POLYGON_SMOOTH, GL_POLYGON_STIPPLE, GL_SCISSOR_TEST, GL_STENCIL_TEST,
		 GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_ARB);

var
	vSqrt255 : TSqrt255Array;
   vManagers : TList;

// RegisterManager
//
procedure RegisterManager(aManager : TComponent);
begin
   if not Assigned(vManagers) then
      vManagers:=TList.Create;
   if vManagers.IndexOf(aManager)<0 then
      vManagers.Add(aManager);
end;

// DeRegisterManager
//
procedure DeRegisterManager(aManager : TComponent);
begin
   if Assigned(vManagers) then
      vManagers.Remove(aManager);
end;

// FindManager
//
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;
var
   i : Integer;
begin
   Result:=nil;
   if Assigned(vManagers) then
      for i:=0 to vManagers.Count-1 do with TComponent(vManagers[i]) do
         if InheritsFrom(classType) and (Name=managerName) then begin
            Result:=TComponent(vManagers[i]);
            Break;
         end;
end;

// GetSqrt255Array
//
function GetSqrt255Array : PSqrt255Array;
var
	i : Integer;
begin
	if vSqrt255[255]<>255 then begin
		for i:=0 to 255 do
			vSqrt255[i]:=Trunc(255*Sqrt(i/255));
	end;
	Result:=@vSqrt255;
end;

// SetGLPolygonMode
//
var
   vLastFrontMode, vLastBackMode : TGLEnum;
procedure SetGLPolygonMode(const aFace, mode : TGLEnum);
begin
   case aFace of
      GL_FRONT :
         if mode<>vLastFrontMode then begin
            glPolygonMode(aFace, mode);
            vLastFrontMode:=mode;
         end;
      GL_BACK :
         if mode<>vLastBackMode then begin
            glPolygonMode(aFace, mode);
            vLastBackMode:=mode;
         end;
      GL_FRONT_AND_BACK :
         if (mode<>vLastFrontMode) or (mode<>vLastBackMode) then begin
            glPolygonMode(aFace, mode);
            vLastFrontMode:=mode;
            vLastBackMode:=mode;
         end;
   end;
end;

// ResetGLPolygonMode
//
procedure ResetGLPolygonMode;
begin
   vLastFrontMode:=0;
   vLastBackMode:=0;
end;

// SetGLMaterialColors
//
type
   THomogeneousFltVectorArray = array [0..3] of THomogeneousFltVector;
   PHomogeneousFltVectorArray = ^THomogeneousFltVectorArray;
var
   vFrontColors, vBackColors : THomogeneousFltVectorArray;
   vFrontShininess, vBackShininess : Integer;
procedure SetGLMaterialColors(const aFace : TGLEnum;
                              const emission, ambient, diffuse, specular : PGLFloat;
                              const shininess : Integer);
var
   ar : PHomogeneousFltVectorArray;
begin
   if aFace=GL_FRONT then begin
      ar:=@vFrontColors;
      if vFrontShininess<>shininess then begin
       	glMateriali(AFace, GL_SHININESS, shininess);
         vFrontShininess:=shininess;
      end;
   end else begin
      ar:=@vBackColors;
      if vBackShininess<>shininess then begin
       	glMateriali(AFace, GL_SHININESS, shininess);
         vBackShininess:=shininess;
      end;
   end;
   if not VectorEquals(PAffineVector(@ar[0])^, PAffineVector(emission)^) then begin
     	glMaterialfv(aFace, GL_EMISSION, emission);
      SetVector(ar[0], PHomogeneousFltVector(emission)^);
   end;
   if not VectorEquals(PAffineVector(@ar[1])^, PAffineVector(ambient)^) then begin
     	glMaterialfv(aFace, GL_AMBIENT, ambient);
      SetVector(ar[1], PHomogeneousFltVector(ambient)^);
   end;
   if not VectorEquals(PVector(@ar[2])^, PVector(diffuse)^) then begin
     	glMaterialfv(aFace, GL_DIFFUSE, diffuse);
      SetVector(ar[2], PHomogeneousFltVector(diffuse)^);
   end;
   if not VectorEquals(PAffineVector(@ar[3])^, PAffineVector(specular)^) then begin
     	glMaterialfv(aFace, GL_SPECULAR, specular);
      SetVector(ar[3], PHomogeneousFltVector(specular)^);
   end;
end;

// SetGLMaterialAlphaChannel
//
procedure SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
var
   ar : PHomogeneousFltVectorArray;
begin
   if aFace=GL_FRONT then
      ar:=@vFrontColors
   else ar:=@vBackColors;
   if ar[2][3]<>alpha then begin
      ar[2][3]:=alpha;
     	glMaterialfv(aFace, GL_DIFFUSE, @ar[2]);
   end;
end;

// ResetGLMaterialColors
//
procedure ResetGLMaterialColors;
const
   clrBlack  : TVector = (0,    0,    0,    1);
   clrGray20 : TVector = (0.20, 0.20, 0.20, 1);
   clrGray80 : TVector = (0.80, 0.80, 0.80, 1);
begin
  	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
 	glMateriali(GL_FRONT_AND_BACK,  GL_SHININESS, 0);
   FillChar(vFrontColors, SizeOf(THomogeneousFltVectorArray), 127);
   FillChar(vBackColors, SizeOf(THomogeneousFltVectorArray), 127);
   vFrontShininess:=0;
   vBackShininess:=0;
end;

// SetGLCurrentTexture
//
var
   lastTextureHandle : array [0..7] of Integer;
procedure SetGLCurrentTexture(const textureUnit, target, handle : Integer);
begin
   if handle<>lastTextureHandle[textureUnit] then begin
      glBindTexture(target, Handle);
      lastTextureHandle[textureUnit]:=handle;
   end;
end;

// ResetGLCurrentTexture
//
procedure ResetGLCurrentTexture;
var
   i : Integer;
begin
   for i:=0 to 7 do
      lastTextureHandle[i]:=-1;
end;

// SetGLTextureMatrix
//
var
   vTextureMatrixIsIdenty : Boolean = True;
procedure SetGLTextureMatrix(const matrix : TMatrix);
begin
   vTextureMatrixIsIdenty:=False;
   glMatrixMode(GL_TEXTURE);
   glLoadMatrixf(PGLFloat(@matrix[0][0]));
   glMatrixMode(GL_MODELVIEW);
end;

// ResetGLTextureMatrix
//
procedure ResetGLTextureMatrix;
begin
   if not vTextureMatrixIsIdenty then begin
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      vTextureMatrixIsIdenty:=True;
   end;
end;

// InvertGLFrontFace
//
var
   vFrontFaceCCW : Boolean = True;
procedure InvertGLFrontFace;
begin
   vFrontFaceCCW:=not vFrontFaceCCW;
   if vFrontFaceCCW then
      glFrontFace(GL_CCW)
   else glFrontFace(GL_CW);
end;

// ResetGLFrontFace
//
procedure ResetGLFrontFace;
begin
   glFrontFace(GL_CCW);
   vFrontFaceCCW:=True;
end;

// SetGLState
//
procedure SetGLState(var states : TGLStates; const aState : TGLState);
begin
	if not (aState in states) then begin
		glEnable(cGLStateToGLEnum[aState]);
		Include(states, aState);
	end;
end;

// UnSetGLState
//
procedure UnSetGLState(var states : TGLStates; const aState : TGLState);
begin
	if (aState in states) then begin
		glDisable(cGLStateToGLEnum[aState]);
		Exclude(states, aState);
	end;
end;

// SaveStringToFile
//
procedure SaveStringToFile(const fileName, data : String);
var
	fs : TFileStream;
begin
	fs:=vFileStreamClass.Create(fileName, fmCreate);
	fs.Write(data[1], Length(data));
	fs.Free;
end;

// LoadStringFromFile
//
function LoadStringFromFile(const fileName : String) : String;
var
	fs : TFileStream;
begin
   if FileExists(fileName) then begin
   	fs:=vFileStreamClass.Create(fileName, fmOpenRead+fmShareDenyNone);
	   SetLength(Result, fs.Size);
   	fs.Read(Result[1], fs.Size);
	   fs.Free;
   end else Result:='';
end;

// SizeOfFile
//
function SizeOfFile(const fileName : String) : Integer;
var
	fs : TFileStream;
begin
   if FileExists(fileName) then begin
   	fs:=vFileStreamClass.Create(fileName, fmOpenRead+fmShareDenyNone);
      Result:=fs.Size;
	   fs.Free;
   end else Result:=0;
end;

//---------------------- TGLUpdateAbleObject -----------------------------------------

// Create
//
constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
	inherited Create;
	FOwner:=AOwner;
end;

// NotifyChange
//
procedure TGLUpdateAbleObject.NotifyChange(Sender : TObject);
begin
   if (FUpdating=0) and Assigned(Owner) then begin
      if Owner is TGLUpdateAbleObject then
         TGLUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TGLUpdateAbleComponent then
         TGLUpdateAbleComponent(Owner).NotifyChange(Self);
      if Assigned(FOnNotifyChange) then
         FOnNotifyChange(Self);
   end;
end;

// GetOwner
//
function TGLUpdateAbleObject.GetOwner : TPersistent;
begin
   Result:=Owner;
end;

// BeginUpdate
//
procedure TGLUpdateAbleObject.BeginUpdate;
begin
   Inc(FUpdating);
end;

// EndUpdate
//
procedure TGLUpdateAbleObject.EndUpdate;
begin
   Dec(FUpdating);
   if FUpdating<=0 then begin
      Assert(FUpdating=0);
      NotifyChange(Self);
   end;
end;

//---------------------- TGLCadenceAbleComponent -------------------------------

{$ifndef GLS_DELPHI_5_UP}
// RemoveFreeNotification
//
procedure TGLCadenceAbleComponent.RemoveFreeNotification(AComponent: TComponent);
begin
   Notification(AComponent, opRemove);
end;
{$endif}

// DoProgress
//
procedure TGLCadenceAbleComponent.DoProgress(const progressTime : TProgressTimes);
begin
   // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

procedure TGLUpdateAbleComponent.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;

// WordToIntegerArray
//
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal); assembler;
// EAX contains Source
// EDX contains Dest
// ECX contains Count
asm
              JECXZ @@Finish
              PUSH ESI
              PUSH EDI
              MOV ESI,EAX
              MOV EDI,EDX
              XOR EAX,EAX
@@1:          LODSW
              STOSD
              DEC ECX
              JNZ @@1
              POP EDI
              POP ESI
@@Finish:
end;

// RoundUpToPowerOf2
//
function RoundUpToPowerOf2(value : Integer) : Integer;
begin
   Result:=1;
   while (Result<value) do Result:=Result*2;
end;

// RoundDownToPowerOf2
//
function RoundDownToPowerOf2(value : Integer) : Integer;
var
   LogTwo : Extended;
begin
   LogTwo:=log2(Value);
   if Trunc(LogTwo) < LogTwo then
      Result:=Trunc(Power(2,Trunc(LogTwo)))
   else Result:=Value;
end;

// IsPowerOf2
//
function IsPowerOf2(value : Integer) : Boolean;
begin
   Result:=(Trunc(log2(Value))=log2(Value));
end;

// NormalizeAngle
//
function NormalizeAngle(angle : Single) : Single;
begin
   if angle>180 then
      if angle>180+360 then
         Result:=angle-Round((angle+180)*(1/360))*360
      else Result:=angle-360
   else if angle<-180 then
      if angle<-180-360 then
         Result:=angle+Round((180-angle)*(1/360))*360
      else Result:=angle+360
   else Result:=angle;
end;

// ReadCRLFString
//
function ReadCRLFString(aStream : TStream) : String;
var
   c : Char;
begin
   Result:='';
   while Copy(Result, Length(Result)-1, 2)<>#13#10 do begin
      aStream.Read(c, 1);
      Result:=Result+c;
   end;
   Result:=Copy(Result, 1, Length(Result)-2);
end;

// WriteCRLFString
//
procedure WriteCRLFString(aStream : TStream; const aString : String);
const
   cCRLF : Integer = $0A0D;
begin
   with aStream do begin
      Write(aString[1], Length(aString));
      Write(cCRLF, 2);
   end;
end;

// StrToFloatDef
//
function StrToFloatDef(strValue : String; defValue : Extended = 0) : Extended;
var
   i, divider, lLen : Integer;
   c : Char;
begin
   Result:=0;
   if strValue='' then Exit;
   divider:=MaxInt;
   strValue:=Trim(StrValue);
   lLen:=length(strValue);
	for i:=1 to lLen do begin
      c:=strValue[i];
      case c of
         '0'..'9' : Result:=(Result*10)+Integer(c)-Integer('0');
         ',', '.' : begin
            if (divider=MaxInt) then
               divider:=i
            else begin
               Result:=defValue;
               Exit;
            end;
         end;
         '-', '+' : if i>1 then begin
            Result:=defValue;
            Exit;
         end;
		else
         if (c<>' ') or (divider<>MaxInt) then begin
            Result:=defValue;
            Exit;
         end;
      end;
   end;
   divider:=lLen-divider;
   if divider>0 then Result:=Result*Exp(-divider*Ln(10));
   if (strValue[1]='-') then Result:=-Result;
end;

// ------------------
// ------------------ TGLCoordinates ------------------
// ------------------

// CreateInitialized
//
constructor TGLCoordinates.CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                             const aStyle : TGLCoordinatesStyle = csUnknown);
begin
   Create(aOwner);
   FCoords:=aValue;
   FDefaultCoords:=aValue;
   FStyle:=aStyle;
end;

// Initialize
//
procedure TGLCoordinates.Initialize(const value : TVector);
begin
   FCoords:=value;
   FDefaultCoords:=value;
end;

// Assign
//
procedure TGLCoordinates.Assign(Source: TPersistent);
begin
   if Source is TGLCoordinates then
      FCoords:=TGLCoordinates(Source).FCoords
   else inherited;
end;

// WriteToFiler
//
procedure TGLCoordinates.WriteToFiler(writer : TWriter);
var
   writeCoords : Boolean;
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      writeCoords:=not VectorEquals(FDefaultCoords, FCoords);
      WriteBoolean(writeCoords);
      if writeCoords then
         Write(FCoords, SizeOf(FCoords));
   end;
end;

// ReadFromFiler
//
procedure TGLCoordinates.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      ReadInteger; // Ignore ArchiveVersion
      if ReadBoolean then
         Read(FCoords, SizeOf(FCoords))
      else FCoords:=FDefaultCoords;
   end;
end;

// DefineProperties
//
procedure TGLCoordinates.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
                              not VectorEquals(FDefaultCoords, FCoords));
end;

// ReadData
//
procedure TGLCoordinates.ReadData(Stream: TStream);
begin
	Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TGLCoordinates.WriteData(Stream: TStream);
begin
	Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TGLCoordinates.NotifyChange(Sender : TObject);
begin
	if (Owner is TGLBaseSceneObject) then begin
 		TGLBaseSceneObject(Owner).CoordinateChanged(Self);
	end else inherited NotifyChange(Sender);
end;

// Translate
//
procedure TGLCoordinates.Translate(const translationVector : TVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// Translate
//
procedure TGLCoordinates.Translate(const translationVector : TAffineVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TGLCoordinates.AddScaledVector(const factor : Single; const translationVector : TVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TGLCoordinates.AddScaledVector(const factor : Single; const translationVector : TAffineVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// Rotate
//
procedure TGLCoordinates.Rotate(const Axis : TAffineVector; Angle : Single);
begin
   RotateVector(FCoords, Axis, Angle);
end;

// Normalize
//
procedure TGLCoordinates.Normalize;
begin
   NormalizeVector(FCoords);
end;

// Invert
//
procedure TGLCoordinates.Invert;
begin
   NegateVector(FCoords);
end;

// Scale
//
procedure TGLCoordinates.Scale(factor : Single);
begin
   ScaleVector(PAffineVector(@FCoords)^, factor);
end;

// VectorLength
//
function TGLCoordinates.VectorLength : TGLFloat;
begin
   Result:=Geometry.VectorLength(FCoords);
end;

// Equals
//
function TGLCoordinates.Equals(const aVector : TVector) : Boolean;
begin
   Result:=VectorEquals(FCoords, aVector);
end;

// SetVector (affine)
//
procedure TGLCoordinates.SetVector(const x, y, z : Single);
begin
   Assert(FStyle<>csPoint);
   Geometry.SetVector(FCoords, x, y, z);
	NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TGLCoordinates.SetVector(const x, y, z, w : Single);
begin
   Assert(FStyle<>csPoint);
   Geometry.SetVector(FCoords, x, y, z, w);
	NotifyChange(Self);
end;

// SetPoint
//
procedure TGLCoordinates.SetPoint(const x, y, z : Single);
begin
   Assert(FStyle<>csVector);
   Geometry.MakePoint(FCoords, x, y, z);
	NotifyChange(Self);
end;

// AsAddress
//
function TGLCoordinates.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLCoordinates.SetAsVector(const value: TVector);
begin
   FCoords:=value;
   case FStyle of
      csPoint :  FCoords[3]:=1;
      csVector : FCoords[3]:=0;
   end;
	NotifyChange(Self);
end;

// SetAsAffineVector
//
procedure TGLCoordinates.SetAsAffineVector(const value : TAffineVector);
begin
   case FStyle of
      csPoint : MakePoint(FCoords, value);
   else
      MakeVector(FCoords, value);
   end;
	NotifyChange(Self);
end;

// GetAsAffineVector
//
function TGLCoordinates.GetAsAffineVector : TAffineVector;
begin
   Geometry.SetVector(Result, FCoords);
end;

// SetCoordinate
//
procedure TGLCoordinates.SetCoordinate(Index: Integer; AValue: TGLFloat);
begin
	FCoords[Index]:=AValue;
	NotifyChange(Self);
end;

// ------------------
// ------------------ TGLNode ------------------
// ------------------

// Create
//
constructor TGLNode.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   // nothing, yet
end;

// Destroy
//
destructor TGLNode.Destroy;
begin
   // nothing, yet
	inherited Destroy;
end;

// Assign
//
procedure TGLNode.Assign(Source: TPersistent);
begin
	if Source is TGLNode then begin
      FCoords:=TGLNode(Source).FCoords;
	end else inherited;
end;

// GetDisplayName
//
function TGLNode.GetDisplayName : String;
begin
	Result:=Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//
function TGLNode.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLNode.SetAsVector(const value: TVector);
begin
	FCoords:=Value;
   (Collection as TGLNodes).NotifyChange;
end;

// SetAsAffineVector
//
procedure TGLNode.SetAsAffineVector(const value : TAffineVector);
begin
   Geometry.SetVector(FCoords, value);
   (Collection as TGLNodes).NotifyChange;
end;

// GetAsAffineVector
//
function TGLNode.GetAsAffineVector : TAffineVector;
begin
   Geometry.SetVector(Result, FCoords);
end;

// SetCoordinate
//
procedure TGLNode.SetCoordinate(Index: Integer; AValue: TGLFloat);
begin
	FCoords[Index]:=AValue;
   (Collection as TGLNodes).NotifyChange;
end;

// StoreCoordinate
//
function TGLNode.StoreCoordinate(Index: Integer) : Boolean;
begin
   Result:=(FCoords[Index]<>0);
end;

// ------------------
// ------------------ TGLNodes ------------------
// ------------------

// Create
//
constructor TGLNodes.Create(AOwner : TPersistent; ItemClass: TCollectionItemClass = nil);
begin
   if not Assigned(ItemClass) then
      inherited Create(AOwner, TGLNode)
   else inherited Create(AOwner, ItemClass);
end;

// CreateCopy
//
function TGLNodes.CreateCopy(AOwner : TPersistent) : TGLNodes;
begin
   if Self<>nil then begin
      Result:=TGLNodesClass(Self.ClassType).Create(AOwner);
      Result.Assign(Self);
   end else Result:=nil;
end;

// SetItems
//
procedure TGLNodes.SetItems(index : Integer; const val : TGLNode);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLNodes.GetItems(index : Integer) : TGLNode;
begin
	Result:=TGLNode(inherited Items[index]);
end;

// Update
//
procedure TGLNodes.Update(Item: TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// Add
//
function TGLNodes.Add: TGLNode;
begin
	Result:=(inherited Add) as TGLNode;
end;

// FindItemID
//
function TGLNodes.FindItemID(ID: Integer): TGLNode;
begin
	Result:=(inherited FindItemID(ID)) as TGLNode;
end;

// NotifyChange
//
procedure TGLNodes.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLNodes.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// AddNode (TGLCoordinates)
//
procedure TGLNodes.AddNode(const coords : TGLCoordinates);
begin
   Add.AsVector:=coords.AsVector;
end;

// AddNode (floats)
//
procedure TGLNodes.AddNode(const x, y, z : Single);
begin
   Add.AsVector:=PointMake(x, y, z);
end;

// AddNode (TVector)
//
procedure TGLNodes.AddNode(const value : TVector);
begin
   Add.AsVector:=value;
end;

// AddNode (TAffineVector)
//
procedure TGLNodes.AddNode(const value : TAffineVector);
begin
   Add.AsAffineVector:=value;
end;

// AddXYArc
//
procedure TGLNodes.AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);
var
   i : Integer;
   f : Single;
   s, c : Single;
begin
   BeginUpdate;
   try
      startAngle:=DegToRad(startAngle);
      stopAngle :=DegToRad(stopAngle);
      f:=(stopAngle-startAngle)/nbSegments;
      for i:=0 to nbSegments do begin
         SinCos(i*f+startAngle, s, c);
         SetVector(Add.FCoords, center[0]+xRadius*c, center[1]+yRadius*s, center[2], 1);
      end;
   finally
      EndUpdate;
   end;
end;

// Barycenter
//
function TGLNodes.Barycenter : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   if Count>0 then begin
      for i:=0 to Count-1 do
         AddVector(Result, PAffineVector(Items[i].AsAddress)^);
      ScaleVector(Result, 1/Count);
   end;
end;

// Vector
//
function TGLNodes.Vector(i : Integer) : TAffineVector;

   procedure CalcUsingPrev; forward;

   procedure CalcUsingNext;
   begin
      if i<Count-1 then
         VectorSubtract(Items[i].AsVector, Items[i+1].AsVector, Result)
      else CalcUsingPrev;
   end;

   procedure CalcUsingPrev;
   begin
      if i>0 then
         VectorSubtract(Items[i-1].AsVector, Items[i].AsVector, Result)
      else CalcUsingNext;
   end;

begin
   Assert((i>=0) and (i<Count));
   if i=0 then
      if i=Count-1 then
         SetVector(Result, NullVector)
      else VectorSubtract(Items[i+1].AsVector, Items[i].AsVector, Result)
   else if i=Count-1 then
      VectorSubtract(Items[i].AsVector, Items[i-1].AsVector, Result)
   else VectorSubtract(Items[i+1].AsVector, Items[i-1].AsVector, Result);
   if VectorNorm(Result)<1e-5 then
      SetVector(Result, NullVector)
   else NormalizeVector(Result);
end;

// GetExtents
//
procedure TGLNodes.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      for k:=0 to 2 do begin
         f:=PAffineVector(Items[i].AsAddress)[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// Translate
//
procedure TGLNodes.Translate(const tv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(PAffineVector(Items[i].AsAddress)^, tv);
   NotifyChange;
end;

// Scale (vector)
//
procedure TGLNodes.Scale(const fv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, fv);
   NotifyChange;
end;

// Scale (single)
//
procedure TGLNodes.Scale(f : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, f);
   NotifyChange;
end;

// RotateAroundX
//
procedure TGLNodes.RotateAroundX(angle : Single);
var
   i : Integer;
   c, s, v2 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v2:=v[2];
      v[2]:=c*v[1]+s*v2;
      v[1]:=c*v2-s*v[1];
   end;
   NotifyChange;
end;

// RotateAroundY
//
procedure TGLNodes.RotateAroundY(angle : Single);
var
   i : Integer;
   c, s, v0 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v0:=v[0];
      v[0]:=c*v0+s*v[2];
      v[2]:=c*v[2]-s*v0;
   end;
   NotifyChange;
end;

// RotateAroundZ
//
procedure TGLNodes.RotateAroundZ(angle : Single);
var
   i : Integer;
   c, s, v1 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v1:=v[1];
      v[1]:=c*v1+s*v[0];
      v[0]:=c*v[0]-s*v1;
   end;
   NotifyChange;
end;

// CreateNewCubicSpline
//
function TGLNodes.CreateNewCubicSpline : TCubicSpline;
var
   i : Integer;
   xa, ya, za : PFloatArray;
begin
   GetMem(xa, SizeOf(TGLFloat)*Count);
   GetMem(ya, SizeOf(TGLFloat)*Count);
   GetMem(za, SizeOf(TGLFloat)*Count);
   for i:=0 to Count-1 do with Items[i] do begin
      xa[i]:=X;
      ya[i]:=Y;
      za[i]:=Z;
   end;
   Result:=TCubicSpline.Create(xa, ya, za, nil, Count);
   FreeMem(xa);
   FreeMem(ya);
   FreeMem(za);
end;

// RenderTesselatedPolygon
//
var
   nbExtraVertices : Integer;
   newVertices : PAffineVectorArray;
procedure TGLNodes.RenderTesselatedPolygon(textured : Boolean;
                                           normal : PAffineVector = nil;
                                           splineDivisions : Integer = 1;
                                           invertNormals : Boolean = False);
var
   i : Integer;
   tess : PGLUTesselator;
   dblVector : TAffineDblVector;
   spline : TCubicSpline;
   splinePos : PAffineVector;
   f : Single;

   function AllocNewVertex : PAffineVector;
   begin
      Inc(nbExtraVertices);
      Result:=@newVertices[nbExtraVertices-1];
   end;

   procedure tessError(errno : TGLEnum); stdcall;
   begin
      Assert(False, IntToStr(errno)+': '+gluErrorString(errno));
   end;

   procedure tessIssueVertex(vertexData : Pointer); stdcall;
   begin
      xglTexCoord2fv(vertexData);
      glVertex3fv(vertexData);
   end;

   procedure tessCombine(coords : PDoubleVector; vertex_data : Pointer;
                         weight : PGLFloat; var outData : Pointer); stdcall;
   begin
      outData:=AllocNewVertex;
      SetVector(PAffineVector(outData)^, coords[0], coords[1], coords[2]);
   end;

begin
   if Count>2 then begin
      // Create and initialize the GLU tesselator
      tess:=gluNewTess;
      gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
      if textured then
         gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
      else gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
      gluTessCallback(tess, GLU_TESS_END, @glEnd);
      gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
      gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
      nbExtraVertices:=0;
      // Issue normal
      if Assigned(normal) then begin
         glNormal3fv(PGLFloat(normal));
         gluTessNormal(tess, normal[0], normal[1], normal[2]);
      end;
      // Issue polygon
      gluTessBeginPolygon(tess, nil);
      gluTessBeginContour(tess);
      if splineDivisions<=1 then begin
         // no spline, use direct coordinates
         GetMem(newVertices, Count*SizeOf(TAffineVector));
         if invertNormals then begin
            for i:=Count-1 downto 0 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end else begin
            for i:=0 to Count-1 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end;
      end else begin
         // cubic spline
         GetMem(newVertices, 2*splineDivisions*Count*SizeOf(TAffineVector));
         spline:=CreateNewCubicSpline;
         f:=1/splineDivisions;
         if invertNormals then begin
            for i:=splineDivisions*(Count-1) downto 0 do begin
               splinePos:=AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end else begin
            for i:=0 to splineDivisions*(Count-1) do begin
               splinePos:=AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end;
         spline.Free;
      end;
      gluTessEndContour(tess);
      gluTessEndPolygon(tess);
      // release stuff
      if Assigned(newVertices) then
         FreeMem(newVertices);
      gluDeleteTess(tess);
   end;
end;

// ------------------
// ------------------ TDataFile ------------------
// ------------------

// Create
//
constructor TDataFile.Create(AOwner: TPersistent);
begin
   inherited Create;
   FOwner:=AOwner;
end;

// Destroy
//
destructor TDataFile.Destroy;
begin
   inherited;
end;

// Capabilities
//
class function TDataFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// GetOwner
//
function TDataFile.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// CreateCopy
//
function TDataFile.CreateCopy(AOwner: TPersistent) : TDataFile;
begin
   if Self<>nil then
      Result:=TDataFileClass(Self.ClassType).Create(AOwner)
   else Result:=nil;
end;

// LoadFromFile
//
procedure TDataFile.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   ResourceName:=ExtractFileName(fileName);
   fs:=vFileStreamClass.Create(fileName, fmOpenRead+fmShareDenyNone);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveToFile
//
procedure TDataFile.SaveToFile(const fileName : String);
var
   fs : TFileStream;
begin
   ResourceName:=ExtractFileName(fileName);
   fs:=vFileStreamClass.Create(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveToStream
//
procedure TDataFile.SaveToStream(stream : TStream);
begin
   Assert(False, 'Export for '+ClassName+' not available.');
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

finalization

   vManagers.Free;
   vManagers:=nil;

end.

