// GLObjects
{: Implementation of standard scene objects plus some management routines.<p>

   All objects declared in this unit are part of the basic GLScene package,
   these are only simple objects and should be kept simple and lightweight.<br>
   More complex or more specialized versions should be placed in dedicated
   units where they can grow and prosper untammed.<p>

   TODO : Classe(s) on the move :<ul>
      <li>TGLTeapot will move to a 'doodad objects' unit
   </ul>

	<b>History : </b><font size=-1><ul>
      <li>19/06/03 - MF - Added GenerateSilhouette to TGLCube and TGLPlane.
      <li>13/06/03 - EG - Fixed TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
      <li>03/06/03 - EG - Added TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
      <li>01/05/03 - SG - Added NURBS Curve to TGLLines (color not supported yet)
      <li>14/04/03 - SG - Added a Simple Bezier Spline to TGLLines (color not supported yet)
      <li>02/04/03 - EG - TGLPlane.RayCastIntersect fix (Erick Schuitema)
      <li>13/02/03 - DanB - added AxisAlignedDimensionsUnscaled functions
      <li>22/01/03 - EG - TGLCube.RayCastIntersect fixes (Dan Bartlett)
      <li>10/01/03 - EG - TGLCube.RayCastIntersect (Stuart Gooding)
      <li>08/01/03 - RC - Added TGLPlane.XScope and YScope, to use just a part of the texture
      <li>27/09/02 - EG - Added TGLPointParameters
      <li>24/07/02 - EG - Added TGLCylinder.Alignment
      <li>23/07/02 - EG - Added TGLPoints (experimental)
      <li>20/07/02 - EG - TGLCylinder.RayCastIntersect and TGLPlane.RayCastIntersect
      <li>18/07/02 - EG - Added TGLCylinder.Align methods
      <li>07/07/02 - EG - Added TGLPlane.Style
      <li>03/07/02 - EG - TGLPolygon now properly setups normals (filippo)
      <li>17/03/02 - EG - Support for transparent lines
      <li>02/02/02 - EG - Fixed TGLSprite change notification
      <li>26/01/02 - EG - TGLPlane & TGLCube now osDirectDraw
      <li>20/01/02 - EG - TGLSpaceText moved to GLSpaceText
      <li>22/08/01 - EG - TGLTorus.RayCastIntersect fixes
      <li>30/07/01 - EG - Updated AxisAlignedDimensions implems
      <li>16/03/01 - EG - TGLCylinderBase, changed default Stacks from 8 to 4
      <li>27/02/01 - EG - Fix in TGLCube texcoords, added TGLFrustrum (thx Robin Gerrets)
      <li>22/02/01 - EG - Added AxisAlignedDimensions overrides by Uwe Raabe
      <li>05/02/01 - EG - Minor changes to TGLCube.BuildList
      <li>21/01/01 - EG - BaseProjectionMatrix fix for TGLHUDSprite (picking issue),
                          TGLHUDSprite moved to GLHUDObjects
      <li>14/01/01 - EG - Fixed TGLSphere texture coordinates
      <li>13/01/01 - EG - TGLSprite matrix compatibility update
      <li>09/01/01 - EG - TGLSpaceText now handles its TFont.OnFontChange
      <li>08/01/01 - EG - Added TGLLinesNode (color support) and Node size control
      <li>22/12/00 - EG - Sprites are no longer texture enabled by default,
                          updated TGLSprite.BuildList to work with new matrices
      <li>14/11/00 - EG - Added TGLDummyCube.Destroy (thx Airatz)
      <li>08/10/00 - EG - Fixed call to wglUseFontOutlines
      <li>06/08/00 - EG - TRotationSolid renamed to TGLRevolutionSolid & moved to GLExtrusion
      <li>04/08/00 - EG - Fixed sphere main body texture coords + slight speedup
      <li>02/08/00 - EG - Added TGLPolygonBase
      <li>19/07/00 - EG - Added TGLHUDSprite
      <li>18/07/00 - EG - Added TGLRevolutionSolid
      <li>15/07/00 - EG - Code reduction and minor speedup for all quadric objects,
                          Added TGLLineBase (split of TGLLines),
                          TGLDummyCube now uses osDirectDraw instead of special behaviour
      <li>13/07/00 - EG - Added TGLArrowLine (code by Aaron Hochwimmer)
      <li>28/06/00 - EG - Support for "ObjectStyle"
      <li>23/06/00 - EG - Reduced default Loop count for TGLDisk
      <li>18/06/00 - EG - TGLMesh and accompanying stuff moved to GLMesh
      <li>14/06/00 - EG - Added Capacity to TVertexList
      <li>09/06/00 - EG - First row of Geometry-related upgrades
      <li>08/06/00 - EG - Added ReleaseFontManager, fixed TGLSpaceText DestroyList,
      <li>01/06/00 - EG - Added TGLAnnulus (code by Aaron Hochwimmer)
      <li>29/05/00 - EG - TGLLines now uses TGLNode/TGLNodes
      <li>28/05/00 - EG - Added persistence ability to TGLLines,
                          Added defaults for all TGLLines properties
      <li>27/05/00 - EG - Moved in RogerCao's TGLLines object, added a TLineNode
                          class (currently private) and various enhancements + fixes,
                          DodecahedronBuildList now available as a procedure,
                          CubeWireframeBuildList now available as a procedure
      <li>26/05/00 - RoC - Added division property to TGLLines, and Spline supported
      <li>26/05/00 - EG - Moved vectorfile remnants to GLVectorFiles
      <li>14/05/00 - EG - Removed Top/Bottom checks for TGLSphere,
                          Added mmTriangleStrip support in CalcNormals
      <li>08/05/00 - EG - Uncommented DisableAutoTexture in TGLSpaceText.BuildList
      <li>07/05/00 - RoC - TGLLines added, to show a list of vertex
		<li>26/04/00 - EG - Reactivated stuff in SetupQuadricParams (thanks Nelson Chu)
		<li>18/04/00 - EG - Overriden TGLDummyCube.Render
		<li>16/04/00 - EG - FontManager now published and auto-creating
		<li>12/04/00 - EG - Added TGLCylinderBase.Loops (fixes a bug, thanks Uwe)
      <li>24/03/00 - EG - Added Rotation to TGLSprite, fixed sprite size
		<li>20/03/00 - EG - Enhanced FontManager
		<li>17/03/00 - EG - Fixed SpaceText glBaseList bug,
								  TGLSprite now uses a transposition of the globalmatrix
		<li>16/03/00 - EG - Enhanced TFontManager to allow lower quality
		<li>14/03/00 - EG - Added subobjects Barycenter support for TGLDummyCube
      <li>09/02/00 - EG - ObjectManager stuff moved to GLSceneRegister,
                          FreeForm and vector file stuff moved to new GLVectorFileObjects
      <li>08/02/00 - EG - Added TGLDummyCube
      <li>05/02/00 - EG - Javadocisation, fixes and enhancements :
                          TVertexList.AddVertex, "default"s to properties
   </ul></font>
}
unit GLObjects;

// GLObjects   - implementation of scene objects plus some management routines
// version     - 0.5.8
// 05-JAN-2000 ml: adjustment of loader routine for 3DS files
// 04-JAN-2000 ml: included new 3DS classes

{$R-}

interface

{$i GLScene.inc}

uses Classes, Geometry, GLScene, GLTexture, GLMisc, OpenGL12, SysUtils,
   VectorLists, GLCrossPlatform, GLContext, GLSilhouette;

type

	// TGLDummyCube
	//
	{: A simple cube, invisible at run-time.<p>
      This is a usually non-visible object -except at design-time- used for
      building hierarchies or groups, when some kind of joint or movement
      mechanism needs be described, you can use DummyCubes.<br>
		DummyCube's barycenter is its children's barycenter.<br>
      The DummyCube can optionnally amalgamate all its children into a single
      display list (see Amalgamate property). }
	TGLDummyCube = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
			FCubeSize : TGLFloat;
			FEdgeColor : TGLColor;
			FVisibleAtRunTime, FAmalgamate : Boolean;
         FGroupList : TGLListHandle;

		protected
			{ Protected Declarations }
			procedure SetCubeSize(const val : TGLFloat);
			procedure SetEdgeColor(const val : TGLColor);
			procedure SetVisibleAtRunTime(const val : Boolean);
         procedure SetAmalgamate(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;
			procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
         procedure StructureChanged; override;
			function BarycenterAbsolutePosition : TVector; override;


		published
			{ Published Declarations }
			property CubeSize : TGLFloat read FCubeSize write SetCubeSize;
			property EdgeColor : TGLColor read FEdgeColor write SetEdgeColor;
         {: If true the dummycube's edges will be visible at runtime.<p>
            The default behaviour of the dummycube is to be visible at design-time
            only, and invisible at runtime. }
			property VisibleAtRunTime : Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;
         {: Amalgamate the dummy's children in a single OpenGL entity.<p>
            This activates a special rendering mode, which will compile
            the rendering of all of the dummycube's children objects into a
            single display list. This may provide a significant speed up in some
            situations, however, this means that changes to the children will
            be ignored untill you call StructureChanged on the dummy cube.<br>
            Some objects, that have their own display list management, may not
            be compatible with this behaviour. This will also prevents sorting
            and culling to operate as usual.<p>
            In short, this features is best used for static, non-transparent
            geometry, or when the point of view won't change over a large
            number of frames. }
         property Amalgamate : Boolean read FAmalgamate write SetAmalgamate default False;
	end;

   // TPlaneStyle
   //
   TPlaneStyle = (psSingleQuad, psTileTexture);
   TPlaneStyles = set of TPlaneStyle;

   // Plane
   //
   {: A simple plane object.<p>
      Note that a plane is always made of a single quad (two triangles) and the
      tiling is only applied to texture coordinates. }
	TGLPlane = class (TGLSceneObject)
	   private
			{ Private Declarations }
	      FXOffset, FYOffset : TGLFloat;
	      FXScope, FYScope : TGLFloat;
			FWidth, FHeight : TGLFloat;
		   FXTiles, FYTiles : Cardinal;
         FStyle : TPlaneStyles;

		protected
			{ Protected Declarations }
		   procedure SetHeight(AValue: TGLFloat);
		   procedure SetWidth(AValue: TGLFloat);
		   procedure SetXOffset(const Value: TGLFloat);
		   procedure SetXScope(const Value: TGLFloat);
         function  StoreXScope : Boolean;
		   procedure SetXTiles(const Value: Cardinal);
		   procedure SetYOffset(const Value: TGLFloat);
		   procedure SetYScope(const Value: TGLFloat);
         function  StoreYScope : Boolean;
		   procedure SetYTiles(const Value: Cardinal);
         procedure SetStyle(const val : TPlaneStyles);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

		   procedure Assign(Source: TPersistent); override;

		   procedure BuildList(var rci : TRenderContextInfo); override;
         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;

         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;
         {: Computes the screen coordinates of the smallest rectangle encompassing the plane.<p>
            Returned extents are NOT limited to any physical screen extents. }
         function ScreenRect : TGLRect;

         {: Computes the signed distance to the point.<p>
            Point coordinates are expected in absolute coordinates. }
         function PointDistance(const aPoint : TVector) : Single;

		published
			{ Public Declarations }
			property Height : TGLFloat read FHeight write SetHeight;
         property Width : TGLFloat read FWidth write SetWidth;
         property XOffset : TGLFloat read FXOffset write SetXOffset;
         property XScope : TGLFloat read FXScope write SetXScope stored StoreXScope;
         property XTiles : Cardinal read FXTiles write SetXTiles default 1;
         property YOffset : TGLFloat read FYOffset write SetYOffset;
         property YScope : TGLFloat read FYScope write SetYScope stored StoreYScope;
         property YTiles : Cardinal read FYTiles write SetYTiles default 1;
         property Style : TPlaneStyles read FStyle write SetStyle default [psSingleQuad, psTileTexture];
   end;

	// TGLSprite
	//
	{: A rectangular area, perspective projected, but always facing the camera.<p>
      A TGLSprite is perspective projected and as such is scaled with distance,
      if you want a 2D sprite that does not get scaled, see TGLHUDSprite. }
	TGLSprite = class (TGLSceneObject)
		private
			{ Private Declarations }
			FWidth : TGLFloat;
			FHeight : TGLFloat;
			FRotation : TGLFloat;
         FAlphaChannel : Single;
         FNoZWrite : Boolean;

		protected
			{ Protected Declarations }
			procedure SetWidth(const val : TGLFloat);
			procedure SetHeight(const val : TGLFloat);
         procedure SetRotation(const val : TGLFloat);
         procedure SetAlphaChannel(const val : Single);
         function StoreAlphaChannel : Boolean;
         procedure SetNoZWrite(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

			procedure SetSize(const width, height : TGLFloat);
			//: Set width and height to "size"
			procedure SetSquareSize(const size : TGLFloat);

		published
			{ Published Declarations }
         {: Sprite Width in 3D world units. }
			property Width : TGLFloat read FWidth write SetWidth;
         {: Sprite Height in 3D world units. }
			property Height : TGLFloat read FHeight write SetHeight;
			{: This the ON-SCREEN rotation of the sprite.<p>
            Rotatation=0 is handled faster. }
         property Rotation : TGLFloat read FRotation write SetRotation;
         {: If different from 1, this value will replace that of Diffuse.Alpha }
         property AlphaChannel : Single read FAlphaChannel write SetAlphaChannel stored StoreAlphaChannel;
         {: If True, sprite will not write to Z-Buffer.<p>
            Sprite will STILL be maskable by ZBuffer test. }
         property NoZWrite : Boolean read FNoZWrite write SetNoZWrite;
	end;

   // TGLPointStyle
   //
   TGLPointStyle = (psSquare, psRound, psSmooth, psSmoothAdditive, psSquareAdditive);

	// TGLPointParameters
	//
	{: Point parameters as in ARB_point_parameters.<p>
      Make sure to read the ARB_point_parameters spec if you want to understand
      what each parameter does. }
	TGLPointParameters = class (TGLUpdateAbleObject)
		private
			{ Private Declarations }
         FEnabled : Boolean;
         FMinSize, FMaxSize : Single;
         FFadeTresholdSize : Single;
         FDistanceAttenuation : TGLCoordinates;

		protected
			{ Protected Declarations }
         procedure SetEnabled(const val : Boolean);
         procedure SetMinSize(const val : Single);
         procedure SetMaxSize(const val : Single);
         procedure SetFadeTresholdSize(const val : Single);
         procedure SetDistanceAttenuation(const val : TGLCoordinates);

			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

		public
			{ Public Declarations }
         constructor Create(AOwner : TPersistent); override;
         destructor Destroy; override;

         procedure Assign(Source : TPersistent); override;

         procedure Apply;
         procedure UnApply;

		published
			{ Published Declarations }
         property Enabled : Boolean read FEnabled write SetEnabled default False;
         property MinSize : Single read FMinSize write SetMinSize stored False;
         property MaxSize : Single read FMaxSize write SetMaxSize stored False;
         property FadeTresholdSize : Single read FFadeTresholdSize write SetFadeTresholdSize stored False;
         {: Components XYZ are for constant, linear and quadratic attenuation. }
         property DistanceAttenuation : TGLCoordinates read FDistanceAttenuation write SetDistanceAttenuation;
   end;

	// TGLPoints
	//
	{: Renders a set of non-transparent colored points.<p>
      The points positions and their color are defined through the Positions
      and Colors properties. }
	TGLPoints = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
         FPositions : TAffineVectorList;
         FColors : TVectorList;
         FSize : Single;
         FStyle : TGLPointStyle;
         FPointParameters : TGLPointParameters;
         FNoZWrite, FStatic : Boolean;

		protected
			{ Protected Declarations }
         function StoreSize : Boolean;
         procedure SetNoZWrite(const val : Boolean);
         procedure SetStatic(const val : Boolean);
         procedure SetSize(const val : Single);
         procedure SetPositions(const val : TAffineVectorList);
         procedure SetColors(const val : TVectorList);
         procedure SetStyle(const val : TGLPointStyle);
         procedure SetPointParameters(const val : TGLPointParameters);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

         {: Points positions.<p>
            If empty, a single point is assumed at (0, 0, 0) }
         property Positions : TAffineVectorList read FPositions write SetPositions;
         {: Defines the points colors.<p>
            <ul>
            <li>if empty, point color will be opaque white
            <li>if contains a single color, all points will use that color
            <li>if contains N colors, the first N points (at max) will be rendered
                using the corresponding colors.
            </ul> }
         property Colors : TVectorList read FColors write SetColors;

		published
			{ Published Declarations }
         {: If true points do not write their Z to the depth buffer. }
         property NoZWrite : Boolean read FNoZWrite write SetNoZWrite;
         {: Tells the component if point coordinates are static.<p>
            If static, changes to the positions should be notified via an
            explicit StructureChanged call, or may not refresh.<br>
            Static sets of points may render faster than dynamic ones. }
         property Static : Boolean read FStatic write SetStatic;
         {: Point size, all points have a fixed size. }
         property Size : Single read FSize write SetSize stored StoreSize;
         {: Points style.<p> }
         property Style : TGLPointStyle read FStyle write SetStyle default psSquare;
         {: Point parameters as of ARB_point_parameters.<p>
            Allows to vary the size and transparency of points depending
            on their distance to the observer. }
         property PointParameters : TGLPointParameters read FPointParameters write SetPointParameters;

	end;

   // TLineNodesAspect
   //
   {: Possible aspects for the nodes of a TLine. }
   TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);

   // TLineSplineMode
   //
   {: Available spline modes for a TLine. }
   TLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve,
                      lsmSegments);

   // TGLLinesNode
   //
   {: Specialized Node for use in a TGLLines objects.<p>
      Adds a Color property (TGLColor). }
   TGLLinesNode = class(TGLNode)
      private
			{ Private Declarations }
         FColor : TGLColor;

		protected
			{ Protected Declarations }
         procedure SetColor(const val : TGLColor);
         procedure OnColorChange(sender : TObject);
         function StoreColor : Boolean;

      public
         { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }

         {: The node color.<p>
            Can also defined the line color (interpolated between nodes) if
            loUseNodeColorForLines is set (in TGLLines). }
         property Color : TGLColor read FColor write SetColor stored StoreColor;
   end;

   // TGLLinesNodes
   //
   {: Specialized collection for Nodes in a TGLLines objects.<p>
      Stores TGLLinesNode items. }
   TGLLinesNodes = class(TGLNodes)
      public
        { Public Declarations }
	      constructor Create(AOwner : TComponent); overload;

         procedure NotifyChange; override;
   end;

   // TGLLineBase
   //
   {: Base class for line objects.<p>
      Introduces line style properties (width, color...). }
   TGLLineBase = class(TGLImmaterialSceneObject)
      private
			{ Private Declarations }
         FLineColor : TGLColor;
         FLinePattern : TGLushort;
         FLineWidth : Single;
         FAntiAliased : Boolean;

		protected
			{ Protected Declarations }
         procedure SetLineColor(const value: TGLColor);
         procedure SetLinePattern(const value: TGLushort);
         procedure SetLineWidth(const val : Single);
         function StoreLineWidth : Boolean;
         procedure SetAntiAliased(const val : Boolean);

         {: Setup OpenGL states according to line style.<p>
            You must call RestoreLineStyle after drawing your lines.<p>
            You may use nested calls with SetupLineStyle/RestoreLineStyle. }
         procedure SetupLineStyle;
         {: Restore OpenGL states, must follow a SetupLineStyle }
         procedure RestoreLineStyle;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }
         {: Indicates if OpenGL should smooth line edges.<p>
            Smoothed lines looks better but are poorly implemented in most OpenGL
            drivers and take *lots* of rendering time. }
         property AntiAliased : Boolean read FAntiAliased write SetAntiAliased default False;
         {: Default color of the lines. }
         property LineColor: TGLColor read FLineColor write SetLineColor;
         {: Bitwise line pattern.<p>
            For instance $FFFF (65535) is a white line (stipple disabled), $0000
            is a black line, $CCCC is the stipple used in axes and dummycube, etc. }
         property LinePattern: TGLushort read FLinePattern write SetLinePattern default $FFFF;
         {: Default width of the lines. }
         property LineWidth : Single read FLineWidth write SetLineWidth stored StoreLineWidth;
         property Visible;
   end;

   // TLinesOptions
   //
   TLinesOption = (loUseNodeColorForLines);
   TLinesOptions = set of TLinesOption;

   // TGLLines
   //
   {: Set of 3D line segments.<p>
      You define a 3D Line by adding its nodes in the "Nodes" property. The line
      may be rendered as a set of segment or as a curve (nodes then act as spline
      control points).<p>
      Alternatively, you can also use it to render a set of spacial nodes (points
      in space), just make the lines transparent and the nodes visible by picking
      the node aspect that suits you. }
   TGLLines = class(TGLLineBase)
      private
			{ Private Declarations }
         FNodes : TGLLinesNodes;
         FNodesAspect : TLineNodesAspect;
         FNodeColor : TGLColor;
         FDivision : Integer;
         FSplineMode : TLineSplineMode;
         FOptions : TLinesOptions;
         FNodeSize : Single;
         FOldNodeColor : TColorVector;
         FNURBSOrder : Integer;
         FNURBSTolerance : Single;
         FNURBSKnots : TSingleList;

		protected
			{ Protected Declarations }
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetNodesAspect(const value : TLineNodesAspect);
         procedure SetNodeColor(const value: TGLColor);
         procedure OnNodeColorChanged(sender : TObject);
         procedure SetDivision(const value: Integer);
         procedure SetNodes(const aNodes : TGLLinesNodes);
         procedure SetOptions(const val : TLinesOptions);
         procedure SetNodeSize(const val : Single);
         procedure SetNURBSOrder(const val : Integer);
         procedure SetNURBSTolerance(const val : Single);
         function StoreNodeSize : Boolean;

         procedure DrawNode(var rci : TRenderContextInfo; X, Y, Z: Single; Color: TGLColor);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

         property NURBSKnots : TSingleList read FNURBSKnots;
         property NURBSOrder : Integer read FNURBSOrder write SetNURBSOrder;
         property NURBSTolerance : Single read FNURBSTolerance write SetNURBSTolerance;

      published
			{ Published Declarations }
         {: The nodes list.<p> }
         property Nodes : TGLLinesNodes read FNodes write SetNodes;

         {: Default color for nodes.<p>
            lnaInvisible and lnaAxes ignore this setting. }
         property NodeColor: TGLColor read FNodeColor write SetNodeColor;
         {: Default aspect of line nodes.<p>
            May help you materialize nodes, segments and control points. }
         property NodesAspect: TLineNodesAspect read FNodesAspect write SetNodesAspect default lnaAxes;
         {: Size for the various node aspects. }
         property NodeSize : Single read FNodeSize write SetNodeSize stored StoreNodeSize;

         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p> }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

         {: Rendering options for the line.<p>
            <ul>
            <li>loUseNodeColorForLines: if set lines will be drawn using node
               colors (and color interpolation between nodes), if not, LineColor
               will be used (single color).
            </ul> }
         property Options : TLinesOptions read FOptions write SetOptions;
   end;

	TCubePart  = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
	TCubeParts = set of TCubePart;

   // TGLCube
   //
   {: A simple cube object.<p>
      This cube use the same material for each of its faces, ie. all faces look
      the same. If you want a multi-material cube, use a mesh in conjunction
      with a TGLFreeForm and a material library. }
   TGLCube = class (TGLSceneObject)
		private
			{ Private Declarations }
         FCubeSize : TAffineVector;
         FParts : TCubeParts;
         FNormalDirection : TNormalDirection;
         procedure SetCubeWidth(AValue: TGLFloat);
         procedure SetCubeHeight(AValue: TGLFloat);
         procedure SetCubeDepth(AValue: TGLFloat);
         procedure SetParts(AValue: TCubeParts);
         procedure SetNormalDirection(AValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property CubeWidth: TGLFloat read FCubeSize[0] write SetCubeWidth stored False;
         property CubeHeight: TGLFloat read FCubeSize[1] write SetCubeHeight stored False;
         property CubeDepth: TGLFloat read FCubeSize[2] write SetCubeDepth stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
   end;

   // TFrustrumParts
   //
	TFrustrumPart = (fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight);
   TFrustrumParts = set of TFrustrumPart;

const
	cAllFrustrumParts = [fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight];

type
   // TGLFrustrum
   //
   { A frustrum is a pyramid with the top chopped off.<p>
      The height of the imaginary pyramid is ApexHeight, the height of the
      frustrum is Height. If ApexHeight and Height are the same, the frustrum
      degenerates into a pyramid.<br>
      Height cannot be greater than ApexHeight. }
   TGLFrustrum = class(TGLSceneObject)
      private
			{ Private Declarations }
         FApexHeight, FBaseDepth, FBaseWidth, FHeight: TGLFloat;
         FParts: TFrustrumParts;
         FNormalDirection: TNormalDirection;
         procedure SetApexHeight(AValue: TGLFloat);
         procedure SetBaseDepth(AValue: TGLFloat);
         procedure SetBaseWidth(AValue: TGLFloat);
         procedure SetHeight(AValue: TGLFloat);
         procedure SetParts(AValue: TFrustrumParts);
         procedure SetNormalDirection(AValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci: TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;

         function TopDepth: TGLFloat;
         function TopWidth: TGLFloat;

      published
			{ Published Declarations }
         property ApexHeight: TGLFloat read FApexHeight write SetApexHeight stored False;
         property BaseDepth: TGLFloat read FBaseDepth write SetBaseDepth stored False;
         property BaseWidth: TGLFloat read FBaseWidth write SetBaseWidth stored False;
         property Height: TGLFloat read FHeight write SetHeight stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TFrustrumParts read FParts write SetParts default cAllFrustrumParts;
   end;

   // TNormalSmoothing
   //
   {: Determines how and if normals are smoothed.<p>
      - nsFlat : facetted look<br>
      - nsSmooth : smooth look<br>
      - nsNone : unlighted rendering, usefull for decla texturing }
   TNormalSmoothing = (nsFlat, nsSmooth, nsNone);

   // TGLQuadricObject
   //
   {: Base class for quadric objects.<p>
      Introduces some basic Quadric interaction functions (the actual quadric
      math is part of the GLU library). }
   TGLQuadricObject = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormals : TNormalSmoothing;
         FNormalDirection : TNormalDirection;

      protected
         { Protected Declarations }
         procedure SetNormals(aValue : TNormalSmoothing);
         procedure SetNormalDirection(aValue : TNormalDirection);
         procedure SetupQuadricParams(quadric : PGLUquadricObj);
         procedure SetNormalQuadricOrientation(quadric : PGLUquadricObj);
         procedure SetInvertedQuadricOrientation(quadric : PGLUquadricObj);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations }
         property Normals : TNormalSmoothing read FNormals write SetNormals default nsSmooth;
         property NormalDirection : TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
   end;

   TAngleLimit1 = -90..90;
   TAngleLimit2 = 0..360;
   TCapType = (ctNone, ctCenter, ctFlat);

   // TGLSphere
   //
   {: A sphere object.<p>
      The sphere can have to and bottom caps, as well as being just a slice
      of sphere. }
   TGLSphere = class (TGLQuadricObject)
      private
         { Private Declarations }
         FRadius  : TGLFloat;
         FSlices, FStacks  : TGLInt;
         FTop     : TAngleLimit1;
         FBottom  : TAngleLimit1;
         FStart   : TAngleLimit2;
         FStop    : TAngleLimit2;
         FTopCap, FBottomCap : TCapType;
         procedure SetBottom(AValue: TAngleLimit1);
         procedure SetBottomCap(AValue: TCapType);
         procedure SetRadius(const aValue : TGLFloat);
         procedure SetSlices(AValue: TGLInt);
         procedure SetStart(AValue: TAngleLimit2);
         procedure SetStop(AValue: TAngleLimit2);
         procedure SetStacks(AValue: TGLInt);
         procedure SetTop(AValue: TAngleLimit1);
         procedure SetTopCap(AValue: TCapType);

      public
         { Public Declarations }
         constructor Create(AOwner:TComponent); override;
         procedure Assign(Source:TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
         { Published Declarations }
         property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
         property BottomCap: TCapType read FBottomCap write SetBottomCap default ctNone;
         property Radius: TGLFloat read FRadius write SetRadius;
         property Slices: TGLInt read FSlices write SetSlices default 16;
         property Stacks: TGLInt read FStacks write SetStacks default 16;
         property Start: TAngleLimit2 read FStart write SetStart default 0;
         property Stop: TAngleLimit2 read FStop write SetStop default 360;
         property Top: TAngleLimit1 read FTop write SetTop default 90;
         property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
   end;

   // TGLDisk
   //
   {: A Disk object.<p>
      The disk may not be complete, it can have a hole (controled by the
      InnerRadius property) and can only be a slice (controled by the StartAngle
      and SweepAngle properties). }
   TGLDisk = class(TGLQuadricObject)
      private
         { Private Declarations }
         FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius : TGLFloat;
         FSlices, FLoops : TGLInt;
         procedure SetOuterRadius(AValue: TGLFloat);
         procedure SetInnerRadius(AValue: TGLFloat);
         procedure SetSlices(AValue: TGLInt);
         procedure SetLoops(AValue: TGLInt);
         procedure SetStartAngle(AValue: TGLFloat);
         procedure SetSweepAngle(AValue: TGLFloat);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
         { Published Declarations }
         {: Allows defining a "hole" in the disk. }
         property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
         {: Number of radial mesh subdivisions. }
         property Loops : TGLInt read FLoops write SetLoops default 2;
         {: Outer radius for the disk.<p>
            If you leave InnerRadius at 0, this is the disk radius. }
         property OuterRadius : TGLFloat read FOuterRadius write SetOuterRadius;
         {: Number of mesh slices.<p>
            For instance, if Slices=6, your disk will look like an hexagon. }
         property Slices : TGLInt read FSlices write SetSlices default 16;
         property StartAngle : TGLFloat read FStartAngle write SetStartAngle;
         property SweepAngle : TGLFloat read FSweepAngle write SetSweepAngle;
   end;

	// TGLCylinderBase
   //
   {: Base class to cylinder-like objects.<p>
      Introduces the basic cylinder description properties.<p>
      Be aware teh default slices and stacks make up for a high-poly cylinder,
      unless you're after high-quality lighting it is recommended to reduce the
      Stacks property to 1. }
	TGLCylinderBase = class (TGLQuadricObject)
		private
			{ Private Declarations }
			FBottomRadius : TGLFloat;
			FSlices,	FStacks, FLoops  : TGLInt;
			FHeight  : TGLFloat;

		protected
			{ Protected Declarations }
			procedure SetBottomRadius(AValue: TGLFloat);
			procedure SetHeight(AValue: TGLFloat);
			procedure SetSlices(AValue: TGLInt);
			procedure SetStacks(AValue: TGLInt);
			procedure SetLoops(AValue: TGLInt);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

			procedure Assign(Source: TPersistent); override;

		published
			{ Published Declarations }
			property BottomRadius: TGLFloat read FBottomRadius write SetBottomRadius;
			property Height: TGLFloat read FHeight write SetHeight;
			property Slices: TGLInt read FSlices write SetSlices default 16;
			property Stacks: TGLInt read FStacks write SetStacks default 4;
			{: Number of concentric rings for top/bottom disk(s). }
			property Loops: TGLInt read FLoops write SetLoops default 1;
	end;

   // TConePart
   //
	TConePart  = (coSides, coBottom);
	TConeParts = set of TConePart;

	// TGLCone
	//
   {: A cone object. }
	TGLCone = class (TGLCylinderBase)
		private
			{ Private Declarations }
			FParts : TConeParts;

		protected
			{ Protected Declarations }
			procedure SetParts(AValue: TConeParts);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
//         function AxisAlignedDimensions : TVector; override;
         function AxisAlignedDimensionsUnscaled : TVector; override;

		published
			{ Published Declarations }
			property Parts : TConeParts read FParts Write SetParts default [coSides, coBottom];
	end;

	// TCylinderPart
	//
	TCylinderPart = (cySides, cyBottom, cyTop);
	TCylinderParts = set of TCylinderPart;

   // TCylinderAlignment
   //
   TCylinderAlignment = (caCenter, caTop, caBottom);

	// TGLCylinder
	//
   {: Cylinder object, can also be used to make truncated cones }
	TGLCylinder = class(TGLCylinderBase)
		private
			{ Private Declarations }
			FParts     : TCylinderparts;
			FTopRadius : TGLFloat;
         FAlignment : TCylinderAlignment;

		protected
			{ Protected Declarations }
			procedure SetTopRadius(aValue : TGLFloat);
			procedure SetParts(aValue : TCylinderParts);
         procedure SetAlignment(val : TCylinderAlignment);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

         procedure Align(const startPoint, endPoint : TVector); overload;
         procedure Align(const startObj, endObj : TGLBaseSceneObject); overload;
         procedure Align(const startPoint, endPoint : TAffineVector); overload;

		published
			{ Published Declarations }
			property TopRadius : TGLFloat read FTopRadius write SetTopRadius;
			property Parts : TCylinderParts read FParts write SetParts default [cySides, cyBottom, cyTop];
         property Alignment : TCylinderAlignment read FAlignment write SetAlignment default caCenter;
	end;

   // TAnnulusPart
   //
   TAnnulusPart = (anInnerSides, anOuterSides, anBottom, anTop);
	TAnnulusParts = set of TAnnulusPart;

   // TGLAnnulus
   //
   {: An annulus is a cylinder that can be made hollow (pipe-like). }
   TGLAnnulus = class(TGLCylinderBase)
      private
			{ Private Declarations }
         FParts : TAnnulusParts;
         FBottomInnerRadius : TGLFloat;
         FTopInnerRadius : TGLFloat;
         FTopRadius : TGLFloat;

      protected
			{ Protected Declarations }
         procedure SetTopRadius(AValue: TGLFloat);
         procedure SetTopInnerRadius(AValue: TGLFloat);
         procedure SetBottomInnerRadius(AValue: TGLFloat);
         procedure SetParts(AValue:TAnnulusParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property BottomInnerRadius: TGLFLoat read FBottomInnerRadius write SetBottomInnerRadius;
         property TopInnerRadius: TGLFloat read FTopInnerRadius write SetTopInnerRadius;
         property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
         property Parts: TAnnulusParts read FParts Write SetParts default [anInnerSides, anOuterSides, anBottom, anTop];
   end;

   // TGLTorus
   //
   {: A Torus object. }
   TGLTorus = class(TGLSceneObject)
      private
			{ Private Declarations }
         FRings, FSides : Cardinal;
         FMinorRadius, FMajorRadius  : Single;

      protected
			{ Protected Declarations }
         procedure SetMajorRadius(const AValue: Single);
         procedure SetMinorRadius(const AValue: Single);
         procedure SetRings(AValue: Cardinal);
         procedure SetSides(aValue : Cardinal);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property MajorRadius: Single read FMajorRadius write SetMajorRadius;
         property MinorRadius: Single read FMinorRadius write SetMinorRadius;
         property Rings: Cardinal read FRings write SetRings default 25;
         property Sides: Cardinal read FSides write SetSides default 15;
   end;

   // TGLTeapot
   //
   {: The age old teapot.<p>
      The only use of this object is testing... }
   TGLTeapot = class(TGLSceneObject)
      private
			{ Private Declarations }
         FGrid : Cardinal;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TArrowLinePart
   //
   TArrowLinePart = (alLine, alTopArrow, alBottomArrow);
   TArrowLineParts = set of TArrowLinePart;

   // TArrowHeadStackingStyle
   //
   TArrowHeadStackingStyle = (ahssStacked, ahssCentered, ahssIncluded);

   // TGLArrowLine
   //
   {: Draws an arrowhead (cylinder + cone).<p>
      The arrow head is a cone that shares the attributes of the cylinder
      (ie stacks/slices, materials etc). Seems to work ok.<br>
      This is useful for displaying a vector based field (eg velocity) or
      other arrows that might be required.<br>
      By default the bottom arrow is off }
   TGLArrowLine = class(TGLCylinderBase)
      private
         { Private Declarations}
         fParts: TArrowLineParts;
         fTopRadius:TGLFloat;
         fTopArrowHeadHeight:TGLFloat;
         fTopArrowHeadRadius:TGLFloat;
         fBottomArrowHeadHeight:TGLFloat;
         fBottomArrowHeadRadius:TGLFloat;
         FHeadStackingStyle : TArrowHeadStackingStyle;

      protected
         { Protected Declarations}
         procedure SetTopRadius(AValue:TGLFloat);
         procedure SetTopArrowHeadHeight(AValue:TGLFloat);
         procedure SetTopArrowHeadRadius(AValue:TGLFloat);
         procedure SetBottomArrowHeadHeight(AValue:TGLFloat);
         procedure SetBottomArrowHeadRadius(AValue:TGLFloat);
         procedure SetParts(AValue:TArrowLineParts);
         procedure SetHeadStackingStyle(const val : TArrowHeadStackingStyle);

      public
         { Public Declarations}
         constructor Create(AOwner:TComponent);override;
         procedure BuildList(var rci : TRenderContextInfo);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations}
         property TopRadius : TGLFloat read fTopRadius write SetTopRadius;
         property HeadStackingStyle : TArrowHeadStackingStyle read FHeadStackingStyle write SetHeadStackingStyle default ahssStacked;
         property Parts : TArrowLineParts read fParts write SetParts default [alLine, alTopArrow];
         property TopArrowHeadHeight : TGLFloat read fTopArrowHeadHeight write SetTopArrowHeadHeight;
         property TopArrowHeadRadius : TGLFloat read fTopArrowHeadRadius write SetTopArrowHeadRadius;
         property BottomArrowHeadHeight : TGLFloat read fBottomArrowHeadHeight write SetBottomArrowHeadHeight;
         property BottomArrowHeadRadius : TGLFloat read fBottomArrowHeadRadius write SetBottomArrowHeadRadius;
   end;

   // TGLDodecahedron
   //
   {: A Dodecahedron.<p>
      The dodecahedron has no texture coordinates defined, ie. without using
      a texture generation mode, no texture will be mapped. }
   TGLDodecahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TGLPolygonBase
   //
   {: Base class for objects based on a polygon. }
   TGLPolygonBase = class(TGLSceneObject)
      private
			{ Private Declarations }
         FDivision : Integer;
         FSplineMode : TLineSplineMode;

		protected
			{ Protected Declarations }
         FNodes : TGLNodes;
         procedure CreateNodes; dynamic;
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetDivision(const value: Integer);
         procedure SetNodes(const aNodes : TGLNodes);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure NotifyChange(Sender : TObject); override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

      published
			{ Published Declarations }
         {: The nodes list.<p> }
         property Nodes : TGLNodes read FNodes write SetNodes;
         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p>
            This mode is used only for the curve, not for the rotation path. }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

   end;

   // TPolygonParts
   //
   TPolygonPart = (ppTop, ppBottom);
   TPolygonParts = set of TPolygonPart;

   // TGLPolygon
   //
   {: A basic polygon object.<p>
      The curve is described by the Nodes and SplineMode properties, should be
      planar and is automatically tessellated.<p>
      Texture coordinates are deduced from X and Y coordinates only.<p>
      This object allows only for polygons described by a single curve, if you
      need "complex polygons" with holes, patches and cutouts, see GLMultiPolygon. }
   TGLPolygon = class(TGLPolygonBase)
      private
			{ Private Declarations }
         FParts : TPolygonParts;

		protected
			{ Protected Declarations }
         procedure SetParts(const val : TPolygonParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

      published
			{ Published Declarations }
         {: Parts of polygon.<p>
            The 'top' of the polygon is the position were the curve describing
            the polygon spin counter-clockwise (i.e. right handed convention). }
         property Parts : TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
   end;

{: Issues OpenGL for a unit-size dodecahedron. }
procedure DodecahedronBuildList;
{: Issues OpenGL for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildList(size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses Consts, GLStrings, Spline, XOpenGL, Polynomials;

const
   cDefaultPointSize : Single = 1.0;

// CubeWireframeBuildList
//
procedure CubeWireframeBuildList(size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);
var
	mi, ma : Single;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   glEnable(GL_LINE_SMOOTH);
   if stipple then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, $CCCC);
   end;
   glLineWidth(1);
   ma:=0.5*size;
   mi:=-ma;
   ResetGLMaterialColors;
   glColorMaterial(GL_FRONT, GL_EMISSION);
   glEnable(GL_COLOR_MATERIAL);
   glColor4fv(@color);
   glBegin(GL_LINE_STRIP);
      // front face
      glVertex3f(ma, mi, mi); glVertex3f(ma, ma, mi);
      glVertex3f(ma, ma, ma); glVertex3f(ma, mi, ma);
      glVertex3f(ma, mi, mi);
      // partial up back face
      glVertex3f(mi, mi, mi); glVertex3f(mi, mi, ma);
      glVertex3f(mi, ma, ma); glVertex3f(mi, ma, mi);
      // right side low
      glVertex3f(ma, ma, mi);
   glEnd;
   glBegin(GL_LINES);
      // right high
      glVertex3f(ma, ma, ma);	glVertex3f(mi, ma, ma);
      // back low
      glVertex3f(mi, mi, mi); glVertex3f(mi, ma, mi);
      // left high
      glVertex3f(ma, mi, ma); glVertex3f(mi, mi, ma);
   glEnd;
   glPopAttrib;
end;

// ------------------
// ------------------ TGLDummyCube ------------------
// ------------------

// Create
//
constructor TGLDummyCube.Create(AOwner : TComponent);
begin
	inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
	FCubeSize:=1;
	FEdgeColor:=TGLColor.Create(Self);
	FEdgeColor.Initialize(clrWhite);
   FGroupList:=TGLListHandle.Create;
end;

// Destroy
//
destructor TGLDummyCube.Destroy;
begin
   FGroupList.Free;
   FEdgeColor.Free;
   inherited;
end;

// Assign
//
procedure TGLDummyCube.Assign(Source: TPersistent);
begin
	if Source is TGLDummyCube then begin
		FCubeSize:=TGLDummyCube(Source).FCubeSize;
		FEdgeColor.Color:=TGLDummyCube(Source).FEdgeColor.Color;
		FVisibleAtRunTime:=TGLDummyCube(Source).FVisibleAtRunTime;
		NotifyChange(Self);
	end;
	inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLDummyCube.AxisAlignedDimensions : TVector;
begin
   VectorScale(Scale.AsVector, 0.5*Abs(FCubeSize), Result);
end;
}
function TGLDummyCube.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=0.5*Abs(FCubeSize);
   Result[1]:=Result[0];
   Result[2]:=Result[0];
   Result[3]:=0;
//   VectorScale(Scale.AsVector, 0.5*Abs(FCubeSize), Result);
end;

// RayCastIntersect
//
function TGLDummyCube.RayCastIntersect(const rayStart, rayVector : TVector;
                                     intersectPoint : PVector = nil;
                                     intersectNormal : PVector = nil) : Boolean;
begin
   Result:=False;
end;

// BuildList
//
procedure TGLDummyCube.BuildList(var rci : TRenderContextInfo);
begin
 	if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
      CubeWireframeBuildList(FCubeSize, True, EdgeColor.Color);
end;

// DoRender
//
procedure TGLDummyCube.DoRender(var rci : TRenderContextInfo;
                                renderSelf, renderChildren : Boolean);
begin
   if FAmalgamate and (not rci.amalgamating) then begin
      if FGroupList.Handle=0 then begin
         FGroupList.AllocateHandle;
         Assert(FGroupList.Handle<>0, 'Handle=0 for '+ClassName);
         glNewList(FGroupList.Handle, GL_COMPILE);
         rci.amalgamating:=True;
         try
            inherited;
         finally
            rci.amalgamating:=False;
            glEndList;
         end;
      end else begin
         glCallList(FGroupList.Handle);
      end;
   end else begin
      // proceed as usual
      inherited;
   end;
end;

// StructureChanged
//
procedure TGLDummyCube.StructureChanged;
begin
   if FAmalgamate then
      FGroupList.DestroyHandle;
   inherited;
end;

// BarycenterAbsolutePosition
//
function TGLDummyCube.BarycenterAbsolutePosition : TVector;
var
	i : Integer;
begin
	if Count>0 then begin
		Result:=Children[0].BarycenterAbsolutePosition;
		for i:=1 to Count-1 do
			Result:=VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
		ScaleVector(Result, 1/Count);
	end else Result:=AbsolutePosition;
end;

// SetCubeSize
//
procedure TGLDummyCube.SetCubeSize(const val : TGLFloat);
begin
	if val<>FCubeSize then begin
		FCubeSize:=val;
		StructureChanged;
	end;
end;

// SetEdgeColor
//
procedure TGLDummyCube.SetEdgeColor(const val : TGLColor);
begin
	if val<>FEdgeColor then begin
		FEdgeColor.Assign(val);
		StructureChanged;
	end;
end;

// SetVisibleAtRunTime
//
procedure TGLDummyCube.SetVisibleAtRunTime(const val : Boolean);
begin
	if val<>FVisibleAtRunTime then begin
		FVisibleAtRunTime:=val;
		StructureChanged;
	end;
end;

// SetAmalgamate
//
procedure TGLDummyCube.SetAmalgamate(const val : Boolean);
begin
   if val<>FAmalgamate then begin
      FAmalgamate:=val;
      if val then
         ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding]
      else begin
         FGroupList.DestroyHandle;
         ObjectStyle:=ObjectStyle-[osDoesTemperWithColorsOrFaceWinding]
      end;
      inherited StructureChanged;
   end;
end;

// ------------------
// ------------------ TGLPlane ------------------
// ------------------

// Create
//
constructor TGLPlane.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FXTiles:=1;
   FYTiles:=1;
   FXScope:=1;
   FYScope:=1;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FStyle:=[psSingleQuad, psTileTexture];
end;

// Assign
//
procedure TGLPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLPlane) then begin
      FWidth:=TGLPlane(Source).FWidth;
      FHeight:=TGLPlane(Source).FHeight;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLPlane.AxisAlignedDimensions: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth)*Scale.DirectX,
                      0.5*Abs(FHeight)*Scale.DirectY, 0);
end;
}
// AxisAlignedDimensions
//
function TGLPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth){*Scale.DirectX},
                      0.5*Abs(FHeight){*Scale.DirectY}, 0);
end;


// RayCastIntersect
//
function TGLPlane.RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean;
var
   locRayStart, locRayVector, ip : TVector;
   t : Single;
begin
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);
   if locRayStart[2]>=0 then begin
      // ray start over plane
      if locRayVector[2]<0 then begin
         t:=locRayStart[2]/locRayVector[2];
         ip[0]:=locRayStart[0]-t*locRayVector[0];
         ip[1]:=locRayStart[1]-t*locRayVector[1];
         if (Abs(ip[0])<=0.5*Width) and (Abs(ip[1])<=0.5*Height) then begin
            Result:=True;
            if Assigned(intersectNormal) then
               intersectNormal^:=AbsoluteDirection;
         end else Result:=False;
      end else Result:=False;
   end else begin
      // ray start below plane
      if locRayVector[2]>0 then begin
         t:=locRayStart[2]/locRayVector[2];
         ip[0]:=locRayStart[0]-t*locRayVector[0];
         ip[1]:=locRayStart[1]-t*locRayVector[1];
         if (Abs(ip[0])<=0.5*Width) and (Abs(ip[1])<=0.5*Height) then begin
            Result:=True;
            if Assigned(intersectNormal) then
               intersectNormal^:=VectorNegate(AbsoluteDirection);
         end else Result:=False;
      end else Result:=False;
   end;
   if Result and Assigned(intersectPoint) then begin
      ip[2]:=0;
      ip[3]:=1;
      intersectPoint^:=LocalToAbsolute(ip);
   end;
end;

// GenerateSilhouette
//
function TGLPlane.GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette;
var
   hw, hh : single;
begin
   Result := TGLSilhouette.Create;

   hw:=FWidth*0.5;
   hh:=FHeight*0.5;

   with Result.Vertices do begin
      AddPoint( hw,  hh);
      AddPoint( hw, -hh);
      AddPoint(-hw, -hh);
      AddPoint(-hw,  hh);
   end;

   with Result.Indices do begin
      Add(0, 1);
      Add(1, 2);
      Add(2, 3);
      Add(3, 0);
   end;

   if silhouetteParameters.CappingRequired then with Result.CapIndices do begin
      Add(0, 1, 2);
      Add(2, 3, 0);
   end;
end;

// BuildList
//
procedure TGLPlane.BuildList(var rci : TRenderContextInfo);
var
   hw, hh, posXFact, posYFact, pX, pY0, pY1 : TGLFloat;
   tx0, tx1, ty0, ty1, texSFact, texTFact : TGLFloat;
   texS, texT0, texT1 : TGLFloat;
   x, y : Integer;
begin
   hw:=FWidth*0.5;
   hh:=FHeight*0.5;
   glNormal3fv(@ZVector);
   // determine tex coords extents
   if psTileTexture in FStyle then begin
      tx0:=FXOffset;
      tx1:=FXTiles*FXScope+FXOffset;
      ty0:=FYOffset;
      ty1:=FYTiles*FYScope+FYOffset;
   end else begin
      tx0:=0;
      ty0:=tx0;
      tx1:=FXScope;
      ty1:=FYScope;
   end;
   if psSingleQuad in FStyle then begin
      // single quad plane
      glBegin(GL_QUADS);
         xglTexCoord2f(tx1, ty1);
         glVertex2f( hw, hh);
         xglTexCoord2f(tx0, ty1);
         glVertex2f(-hw, hh);
         xglTexCoord2f(tx0, ty0);
         glVertex2f(-hw, -hh);
         xglTexCoord2f(tx1, ty0);
         glVertex2f( hw, -hh);
      glEnd;
   end else begin
      // multi-quad plane (actually built from tri-strips)
      texSFact:=(tx1-tx0)/FXTiles;
      texTFact:=(ty1-ty0)/FYTiles;
      posXFact:=FWidth/FXTiles;
      posYFact:=FHeight/FYTiles;
      texT0:=0;
      pY0:=-hh;
      for y:=0 to FYTiles-1 do begin
         texT1:=(y+1)*texTFact;
         pY1:=(y+1)*posYFact-hh;
         glBegin(GL_TRIANGLE_STRIP);
         for x:=0 to FXTiles do begin
            texS:=tx0+x*texSFact;
            pX:=x*posXFact-hw;
            xglTexCoord2f(texS, texT1);
            glVertex2f(pX, pY1);
            xglTexCoord2f(texS, texT0);
            glVertex2f(pX, pY0);
         end;
         glEnd;
         texT0:=texT1;
         pY0:=pY1;
      end;
   end;
end;

// SetWidth
//
procedure TGLPlane.SetWidth(AValue : TGLFloat);
begin
   if AValue<>FWidth then begin
      FWidth:=AValue;
	   StructureChanged;
   end;
end;

// ScreenRect
//
function TGLPlane.ScreenRect : TGLRect;
var
   v : array [0..3] of TVector;
   buf : TGLSceneBuffer;
   hw, hh : TGLFloat;
begin
   buf:=Scene.CurrentBuffer;
   if Assigned(buf) then begin
      hw:=FWidth*0.5;
      hh:=FHeight*0.5;
      v[0]:=LocalToAbsolute(PointMake(-hw, -hh, 0));
      v[1]:=LocalToAbsolute(PointMake( hw, -hh, 0));
      v[2]:=LocalToAbsolute(PointMake( hw,  hh, 0));
      v[3]:=LocalToAbsolute(PointMake(-hw,  hh, 0));
      buf.WorldToScreen(@v[0], 4);
      Result.Left  :=Round(MinFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
      Result.Right :=Round(MaxFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
      Result.Top   :=Round(MinFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
      Result.Bottom:=Round(MaxFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
   end else FillChar(Result, SizeOf(TGLRect), 0);
end;

// PointDistance
//
function TGLPlane.PointDistance(const aPoint : TVector) : Single;
begin
   Result:=VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
                            AbsoluteDirection);
end;

// SetHeight
//
procedure TGLPlane.SetHeight(AValue:TGLFloat);
begin
   if AValue<>FHeight then begin
      FHeight:=AValue;
      StructureChanged;
   end;
end;

// SetXOffset
//
procedure TGLPlane.SetXOffset(const Value: TGLFloat);
begin
   if Value<>FXOffset then begin
      FXOffset:=Value;
      StructureChanged;
   end;
end;

// SetXScope
//
procedure TGLPlane.SetXScope(const Value: TGLFloat);
begin
   if Value<>FXScope then begin
      FXScope:=Value;
      if FXScope>1 then
         FXScope:=1;
      StructureChanged;
   end;
end;

// StoreXScope
//
function TGLPlane.StoreXScope : Boolean;
begin
   Result:=(FXScope<>1);
end;

// SetXTiles
//
procedure TGLPlane.SetXTiles(const Value: Cardinal);
begin
   if Value<>FXTiles then begin
      FXTiles:=Value;
      StructureChanged;
   end;
end;

// SetYOffset
//
procedure TGLPlane.SetYOffset(const Value: TGLFloat);
begin
   if Value<>FYOffset then begin
      FYOffset:=Value;
      StructureChanged;
   end;
end;

// SetYScope
//
procedure TGLPlane.SetYScope(const Value: TGLFloat);
begin
   if Value<>FYScope then begin
      FYScope:=Value;
      if FYScope>1 then
         FYScope:=1;
      StructureChanged;
   end;
end;

// StoreYScope
//
function TGLPlane.StoreYScope : Boolean;
begin
   Result:=(FYScope<>1);
end;

// SetYTiles
//
procedure TGLPlane.SetYTiles(const Value: Cardinal);
begin
   if Value<>FYTiles then begin
      FYTiles:=Value;
      StructureChanged;
   end;
end;

// SetStyle
//
procedure TGLPlane.SetStyle(const val : TPlaneStyles);
begin
   if val<>FStyle then begin
      FStyle:=val;
      StructureChanged;
   end;
end;

// ------------------
// ------------------ TGLSprite ------------------
// ------------------

// Create
//
constructor TGLSprite.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FAlphaChannel:=1;
	FWidth:=1;
	FHeight:=1;
end;

// Assign
//
procedure TGLSprite.Assign(Source : TPersistent);
begin
	if Source is TGLSprite then begin
		FWidth:=TGLSprite(Source).FWidth;
		FHeight:=TGLSprite(Source).FHeight;
		FRotation:=TGLSprite(Source).FRotation;
      FAlphaChannel:=TGLSprite(Source).FAlphaChannel;
      FNoZWrite:=TGLSprite(Source).FNoZWrite;
	end;
	inherited Assign(Source);
end;

// BuildList
//
procedure TGLSprite.BuildList(var rci : TRenderContextInfo);
var
	vx, vy, vx1, vy1 : TAffineVector;
	i : Integer;
   w, h, c, s : Single;
   mat : TMatrix;
begin
   if FAlphaChannel<>1 then
      SetGLMaterialAlphaChannel(GL_FRONT, FAlphaChannel);
   if NoZWrite then
      glDepthMask(False);
   glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
	glBegin(GL_QUADS);
		// extraction of the "vecteurs directeurs de la matrice"
		// (dunno how they are named in english)
      w:=FWidth*0.5;
      h:=FHeight*0.5;
   	vx[0]:=mat[0][0];  vy[0]:=mat[0][1];
   	vx[1]:=mat[1][0];  vy[1]:=mat[1][1];
   	vx[2]:=mat[2][0];  vy[2]:=mat[2][1];
      ScaleVector(vx, w/VectorLength(vx));
      ScaleVector(vy, h/VectorLength(vy));
      if FRotation=0 then begin
         // no rotation, use fast, direct projection
   		xglTexCoord2f(1, 1);  glVertex3f( vx[0]+vy[0], vx[1]+vy[1], vx[2]+vy[2]);
	   	xglTexCoord2f(0, 1);  glVertex3f(-vx[0]+vy[0],-vx[1]+vy[1],-vx[2]+vy[2]);
		   xglTexCoord2f(0, 0);  glVertex3f(-vx[0]-vy[0],-vx[1]-vy[1],-vx[2]-vy[2]);
   		xglTexCoord2f(1, 0);  glVertex3f( vx[0]-vy[0], vx[1]-vy[1], vx[2]-vy[2]);
      end else begin
         // we need to compose main vectors...
         SinCos(FRotation*cPIdiv180, s, c);
   		for i:=0 to 2 do begin
            vx1[i]:=vx[i]+vy[i];
            vy1[i]:=vy[i]-vx[i];
         end;
         // ...and apply rotation... way slower
   		xglTexCoord2f(1, 1);  glVertex3f( c*vx1[0]+s*vy1[0], c*vx1[1]+s*vy1[1], c*vx1[2]+s*vy1[2]);
	   	xglTexCoord2f(0, 1);  glVertex3f(-s*vx1[0]+c*vy1[0],-s*vx1[1]+c*vy1[1],-s*vx1[2]+c*vy1[2]);
		   xglTexCoord2f(0, 0);  glVertex3f(-c*vx1[0]-s*vy1[0],-c*vx1[1]-s*vy1[1],-c*vx1[2]-s*vy1[2]);
   		xglTexCoord2f(1, 0);  glVertex3f( s*vx1[0]-c*vy1[0], s*vx1[1]-c*vy1[1], s*vx1[2]-c*vy1[2]);
      end;
	glEnd;
   if NoZWrite then
      glDepthMask(True);
end;

// SetWidth
//
procedure TGLSprite.SetWidth(const val : TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
		NotifyChange(Self);
	end;
end;

// SetHeight
//
procedure TGLSprite.SetHeight(const val : TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
		NotifyChange(Self);
	end;
end;

// SetRotation
//
procedure TGLSprite.SetRotation(const val : TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		NotifyChange(Self);
	end;
end;

// SetAlphaChannel
//
procedure TGLSprite.SetAlphaChannel(const val : Single);
begin
   if val<>FAlphaChannel then begin
      if val<0 then
         FAlphaChannel:=0
      else if val>1 then
         FAlphaChannel:=1
      else FAlphaChannel:=val;
		NotifyChange(Self);
   end;
end;

// StoreAlphaChannel
//
function TGLSprite.StoreAlphaChannel : Boolean;
begin
	Result:=(FAlphaChannel<>1);
end;

// SetNoZWrite
//
procedure TGLSprite.SetNoZWrite(const val : Boolean);
begin
   FNoZWrite:=val;
   NotifyChange(Self);
end;

// SetSize
//
procedure TGLSprite.SetSize(const width, height : TGLFloat);
begin
	FWidth:=width;
	FHeight:=height;
   NotifyChange(Self);
end;

// SetSquareSize
//
procedure TGLSprite.SetSquareSize(const size : TGLFloat);
begin
	FWidth:=size;
	FHeight:=size;
   NotifyChange(Self);
end;

// ------------------
// ------------------ TGLPointParameters ------------------
// ------------------

// Create
//
constructor TGLPointParameters.Create(AOwner : TPersistent);
begin
	inherited Create(AOwner);
   FMinSize:=0;
   FMaxSize:=128;
   FFadeTresholdSize:=1;
   FDistanceAttenuation:=TGLCoordinates.CreateInitialized(Self, XHmgVector, csVector);
end;

// Destroy
//
destructor TGLPointParameters.Destroy;
begin
   FDistanceAttenuation.Free;
   inherited;
end;

// Assign
//
procedure TGLPointParameters.Assign(Source : TPersistent);
begin
	if Source is TGLPointParameters then begin
      FMinSize:=TGLPointParameters(Source).FMinSize;
      FMaxSize:=TGLPointParameters(Source).FMaxSize;
      FFadeTresholdSize:=TGLPointParameters(Source).FFadeTresholdSize;
      FDistanceAttenuation.Assign(TGLPointParameters(Source).DistanceAttenuation);
	end;
end;

// DefineProperties
//
procedure TGLPointParameters.DefineProperties(Filer: TFiler);
var
   defaultParams : Boolean;
begin
   inherited;
   defaultParams:=(FMaxSize=128) and (FMinSize=0) and (FFadeTresholdSize=1);
   Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
                              not defaultParams);
end;

// ReadData
//
procedure TGLPointParameters.ReadData(Stream: TStream);
begin
   with Stream do begin
      Read(FMinSize, SizeOf(Single));
      Read(FMaxSize, SizeOf(Single));
      Read(FFadeTresholdSize, SizeOf(Single));
   end;
end;

// WriteData
//
procedure TGLPointParameters.WriteData(Stream: TStream);
begin
   with Stream do begin
      Write(FMinSize, SizeOf(Single));
      Write(FMaxSize, SizeOf(Single));
      Write(FFadeTresholdSize, SizeOf(Single));
   end;
end;

// Apply
//
procedure TGLPointParameters.Apply;
begin
   if Enabled and GL_ARB_point_parameters then begin
      glPointParameterfARB(GL_POINT_SIZE_MIN_ARB, FMinSize);
      glPointParameterfARB(GL_POINT_SIZE_MAX_ARB, FMaxSize);
      glPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
      glPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB, FDistanceAttenuation.AsAddress);
   end;
end;

// UnApply
//
procedure TGLPointParameters.UnApply;
begin
   if Enabled and GL_ARB_point_parameters then begin
      glPointParameterfARB(GL_POINT_SIZE_MIN_ARB, 0);
      glPointParameterfARB(GL_POINT_SIZE_MAX_ARB, 128);
      glPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
      glPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB, @XVector);
   end;
end;

// SetEnabled
//
procedure TGLPointParameters.SetEnabled(const val : Boolean);
begin
   if val<>FEnabled then begin
      FEnabled:=val;
      NotifyChange(Self);
   end;
end;

// SetMinSize
//
procedure TGLPointParameters.SetMinSize(const val : Single);
begin
   if val<>FMinSize then begin
      if val<0 then
         FMinSize:=0
      else FMinSize:=val;
      NotifyChange(Self);
   end;
end;

// SetMaxSize
//
procedure TGLPointParameters.SetMaxSize(const val : Single);
begin
   if val<>FMaxSize then begin
      if val<0 then
         FMaxSize:=0
      else FMaxSize:=val;
      NotifyChange(Self);
   end;
end;

// SetFadeTresholdSize
//
procedure TGLPointParameters.SetFadeTresholdSize(const val : Single);
begin
   if val<>FFadeTresholdSize then begin
      if val<0 then
         FFadeTresholdSize:=0
      else FFadeTresholdSize:=val;
      NotifyChange(Self);
   end;
end;

// SetDistanceAttenuation
//
procedure TGLPointParameters.SetDistanceAttenuation(const val : TGLCoordinates);
begin
   FDistanceAttenuation.Assign(val);
end;

// ------------------
// ------------------ TGLPoints ------------------
// ------------------

// Create
//
constructor TGLPoints.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FStyle:=psSquare;
   FSize:=cDefaultPointSize;
   FPositions:=TAffineVectorList.Create;
   FColors:=TVectorList.Create;
   FPointParameters:=TGLPointParameters.Create(Self);
end;

// Destroy
//
destructor TGLPoints.Destroy;
begin
   FPointParameters.Free;
   FColors.Free;
   FPositions.Free;
   inherited;
end;

// Assign
//
procedure TGLPoints.Assign(Source : TPersistent);
begin
	if Source is TGLPoints then begin
      FSize:=TGLPoints(Source).FSize;
      FStyle:=TGLPoints(Source).FStyle;
      FPositions.Assign(TGLPoints(Source).FPositions);
      FColors.Assign(TGLPoints(Source).FColors);
      StructureChanged
	end;
	inherited Assign(Source);
end;

// BuildList
//
procedure TGLPoints.BuildList(var rci : TRenderContextInfo);
var
   n : Integer;
   v : TVector;
begin
   n:=FPositions.Count;
   case FColors.Count of
      0 : glColor4f(1, 1, 1, 1);
      1 : glColor4fv(PGLFloat(FColors.List));
   else
      if FColors.Count<n then
         n:=FColors.Count;
      glColorPointer(4, GL_FLOAT, 0, FColors.List);
      glEnableClientState(GL_COLOR_ARRAY);
   end;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   if n=0 then begin
      v:=NullHmgPoint;
      glVertexPointer(3, GL_FLOAT, 0, @v);
      n:=1;
   end else glVertexPointer(3, GL_FLOAT, 0, FPositions.List);
   glEnableClientState(GL_VERTEX_ARRAY);
   if NoZWrite then
      glDepthMask(False);
   glPointSize(FSize);
   PointParameters.Apply;
   if GL_EXT_compiled_vertex_array and (n>64) then
      glLockArraysEXT(0, n);
   case FStyle of
      psSquare : begin
         // square point (simplest method, fastest)
         glDisable(GL_BLEND);
      end;
      psRound : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_GREATER, 0.5);
         glDisable(GL_BLEND);
      end;
      psSmooth : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_NOTEQUAL, 0.0);
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      psSmoothAdditive : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_NOTEQUAL, 0.0);
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
      psSquareAdditive : begin
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
   else
      Assert(False);
   end;
   glDrawArrays(GL_POINTS, 0, n);
   if GL_EXT_compiled_vertex_array and (n>64) then
      glUnlockArraysEXT;
   PointParameters.UnApply;
   if NoZWrite then
      glDepthMask(True);
   glDisableClientState(GL_VERTEX_ARRAY);
   if FColors.Count>1 then
      glDisableClientState(GL_COLOR_ARRAY);
   glPopAttrib;
end;

// StoreSize
//
function TGLPoints.StoreSize : Boolean;
begin
   Result:=(FSize<>cDefaultPointSize);
end;

// SetNoZWrite
//
procedure TGLPoints.SetNoZWrite(const val : Boolean);
begin
   if FNoZWrite<>val then begin
      FNoZWrite:=val;
      StructureChanged;
   end;
end;

// SetStatic
//
procedure TGLPoints.SetStatic(const val : Boolean);
begin
   if FStatic<>val then begin
      FStatic:=val;
      if val then
         ObjectStyle:=ObjectStyle-[osDirectDraw]
      else ObjectStyle:=ObjectStyle+[osDirectDraw];
      StructureChanged;
   end;
end;

// SetSize
//
procedure TGLPoints.SetSize(const val : Single);
begin
   if FSize<>val then begin
      FSize:=val;
      StructureChanged;
   end;
end;

// SetPositions
//
procedure TGLPoints.SetPositions(const val : TAffineVectorList);
begin
   FPositions.Assign(val);
   StructureChanged;
end;

// SetColors
//
procedure TGLPoints.SetColors(const val : TVectorList);
begin
   FColors.Assign(val);
   StructureChanged;
end;

// SetStyle
//
procedure TGLPoints.SetStyle(const val : TGLPointStyle);
begin
   if FStyle<>val then begin
      FStyle:=val;
      StructureChanged;
   end;
end;

// SetPointParameters
//
procedure TGLPoints.SetPointParameters(const val : TGLPointParameters);
begin
   FPointParameters.Assign(val);
end;

// ------------------
// ------------------ TGLLineBase ------------------
// ------------------

// Create
//
constructor TGLLineBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FLineColor:=TGLColor.Create(Self);
   FLineColor.Initialize(clrWhite);
   FLinePattern:=$FFFF;
   FAntiAliased:=False;
   FLineWidth:=1.0;
end;

// Destroy
//
destructor TGLLineBase.Destroy;
begin
   FLineColor.Free;
   inherited Destroy;
end;

// SetLineColor
//
procedure TGLLineBase.SetLineColor(const value: TGLColor);
begin
   FLineColor.Color:=Value.Color;
   StructureChanged;
end;

// SetLinePattern
//
procedure TGLLineBase.SetLinePattern(const value: TGLushort);
begin
   if FLinePattern<>value then begin
      FLinePattern:=Value;
      StructureChanged;
   end;
end;

// SetLineWidth
//
procedure TGLLineBase.SetLineWidth(const val : Single);
begin
   if FLineWidth<>val then begin
      FLineWidth:=val;
      StructureChanged;
   end;
end;

// StoreLineWidth
//
function TGLLineBase.StoreLineWidth : Boolean;
begin
   Result:=(FLineWidth<>1.0);
end;

// SetAntiAliased
//
procedure TGLLineBase.SetAntiAliased(const val : Boolean);
begin
   if FAntiAliased<>val then begin
      FAntiAliased:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLLineBase.Assign(Source: TPersistent);
begin
   if Source is TGLLineBase then begin
      FLineColor:=TGLLineBase(Source).FLineColor;
      FLinePattern:=TGLLineBase(Source).FLinePattern;
      FLineWidth:=TGLLineBase(Source).FLineWidth;
      FAntiAliased:=TGLLineBase(Source).FAntiAliased;
   end else inherited Assign(Source);
end;

// SetupLineStyle
//
procedure TGLLineBase.SetupLineStyle;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   if FLinePattern<>$FFFF then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, FLinePattern);
   end;
   if FAntiAliased then
      glEnable(GL_LINE_SMOOTH)
   else glDisable(GL_LINE_SMOOTH);
   glLineWidth(FLineWidth);
   if FLineColor.Alpha<>1 then begin
      glEnable(GL_BLEND);
   	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glColor4fv(FLineColor.AsAddress);
   end else glColor3fv(FLineColor.AsAddress);
end;

// RestoreLineStyle
//
procedure TGLLineBase.RestoreLineStyle;
begin
   glPopAttrib;
end;

// ------------------
// ------------------ TGLLinesNode ------------------
// ------------------

// Create
//
constructor TGLLinesNode.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FColor:=TGLColor.Create(Self);
   FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TGLLines).NodeColor.Color);
   FColor.OnNotifyChange:=OnColorChange;
end;

// Destroy
//
destructor TGLLinesNode.Destroy;
begin
   FColor.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLLinesNode.Assign(Source: TPersistent);
begin
	if Source is TGLNode then begin
      FColor.Assign(TGLLinesNode(Source).FColor);
	end else inherited;
end;

// SetColor
//
procedure TGLLinesNode.SetColor(const val : TGLColor);
begin
   FColor.Assign(val);
end;

// OnColorChange
//
procedure TGLLinesNode.OnColorChange(sender : TObject);
begin
   (Collection as TGLNodes).NotifyChange;
end;

// StoreColor
//
function TGLLinesNode.StoreColor : Boolean;
begin
   Result:=not VectorEquals((TGLLinesNodes(Collection).GetOwner as TGLLines).NodeColor.Color,
                            FColor.Color);
end;

// ------------------
// ------------------ TGLLinesNodes ------------------
// ------------------

// Create
//
constructor TGLLinesNodes.Create(AOwner : TComponent);
begin
   inherited Create(AOwner, TGLLinesNode);
end;

// NotifyChange
//
procedure TGLLinesNodes.NotifyChange;
begin
   if (GetOwner<>nil) then
      (GetOwner as TGLBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TGLLines ------------------
// ------------------

// Create
//
constructor TGLLines.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FNodes:=TGLLinesNodes.Create(Self);
   FNodeColor:=TGLColor.Create(Self);
   FNodeColor.Initialize(clrBlue);
   FNodeColor.OnNotifyChange:=OnNodeColorChanged;
   FOldNodeColor:=clrBlue;
   FDivision:=10;
   FNodesAspect:=lnaAxes;
   FSplineMode:=lsmLines;
   FNodeSize:=1;
   FNURBSKnots:=TSingleList.Create;
   FNURBSOrder:=0;
   FNURBSTolerance:=50;
end;

// Destroy
//
destructor TGLLines.Destroy;
begin
   FNodes.Free;
   FNodeColor.Free;
   FNURBSKnots.Free;
   inherited Destroy;
end;

// SetNodesAspect
//
procedure TGLLines.SetNodesAspect(const value : TLineNodesAspect);
begin
   if Value<>FNodesAspect then begin
      FNodesAspect:=value;
      StructureChanged;
   end;
end;

// SetNodeColor
//
procedure TGLLines.SetNodeColor(const value: TGLColor);
begin
   FNodeColor.Color:=Value.Color;
   StructureChanged;
end;

// OnNodeColorChanged
//
procedure TGLLines.OnNodeColorChanged(sender : TObject);
var
   i : Integer;
begin
   // update color for nodes...
   for i:=0 to Nodes.Count-1 do
      if VectorEquals(TGLLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
         TGLLinesNode(Nodes[i]).Color.Assign(FNodeColor);
   SetVector(FOldNodeColor, FNodeColor.Color);
end;

// SetDivision
//
procedure TGLLines.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetNodes
//
procedure TGLLines.SetNodes(const aNodes : TGLLinesNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetOptions
//
procedure TGLLines.SetOptions(const val : TLinesOptions);
begin
   FOptions:=val;
   StructureChanged;
end;

// SetNodeSize
//
procedure TGLLines.SetNodeSize(const val : Single);
begin
   if val<=0 then
      FNodeSize:=1
   else FNodeSize:=val;
   StructureChanged;
end;

// StoreNodeSize
//
function TGLLines.StoreNodeSize : Boolean;
begin
   Result:=FNodeSize<>1;
end;

// SetSplineMode
//
procedure TGLLines.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// SetNURBSOrder
procedure TGLLines.SetNURBSOrder(const val : Integer);
begin
   if val<>FNURBSOrder then begin
      FNURBSOrder:=val;
      StructureChanged;
   end;
end;

// SetNURBSTolerance
procedure TGLLines.SetNURBSTolerance(const val : Single);
begin
   if val<>FNURBSTolerance then begin
      FNURBSTolerance:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLLines.Assign(Source: TPersistent);
begin
   if Source is TGLLines then begin
      SetNodes(TGLLines(Source).FNodes);
      FNodesAspect:=TGLLines(Source).FNodesAspect;
      FNodeColor.Color:=TGLLines(Source).FNodeColor.Color;
      FDivision:=TGLLines(Source).FDivision;
      FSplineMode:=TGLLines(Source).FSplineMode;
   end;
   inherited Assign(Source);
end;

// DrawNode
//
procedure TGLLines.DrawNode(var rci : TRenderContextInfo; X, Y, Z: Single; Color: TGLColor);
begin
   glPushMatrix;
   glTranslatef(x, y, z);
   case NodesAspect of
      lnaAxes :
         AxesBuildList(rci, $CCCC, FNodeSize*0.5);
      lnaCube :
         CubeWireframeBuildList(FNodeSize, False, Color.Color);
      lnaDodecahedron : begin
         if FNodeSize<>1 then begin
            glPushMatrix;
            glScalef(FNodeSize, FNodeSize, FNodeSize);
            SetGLMaterialColors(GL_FRONT, @clrBlack, @clrGray20, Color.AsAddress, @clrBlack, 0);
            DodecahedronBuildList;
            glPopMatrix;
         end else begin
            SetGLMaterialColors(GL_FRONT, @clrBlack, @clrGray20, Color.AsAddress, @clrBlack, 0);
            DodecahedronBuildList;
         end;
      end;
   else
      Assert(False)
   end;
   glPopMatrix;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLLines.AxisAlignedDimensionsUnscaled : TVector;
var
   i : Integer;
begin
   RstVector(Result);
   for i:=0 to Nodes.Count-1 do
      MaxVector(Result, VectorAbs(Nodes[i].AsVector));
   // EG: commented out, line below looks suspicious, since scale isn't taken
   //     into account in previous loop, must have been hiding another bug... somewhere...
   //DivideVector(Result, Scale.AsVector);     //DanB ?
end;

// BuildList
//
procedure TGLLines.BuildList(var rci : TRenderContextInfo);
var
   i, n : Integer;
   a, b, c : TGLFloat;
   f : Single;
   spline : TCubicSpline;
   vertexColor : TVector;
   nodeBuffer : array of TAffineVector;
   nurbsRenderer : PGLUNurbs;
begin
   if Nodes.Count>1 then begin
      // first, we setup the line color & stippling styles
      SetupLineStyle;

      // Set up the control point buffer for Bezier splines and NURBS curves.
      // If required this could be optimized by storing a cached node buffer.
      if (FSplineMode=lsmBezierSpline) or (FSplineMode=lsmNURBSCurve) then begin
         SetLength(nodeBuffer, Nodes.Count);
         for i:=0 to Nodes.Count-1 do
            nodeBuffer[i]:=Nodes[i].AsAffineVector;
      end;

      if FSplineMode=lsmBezierSpline then begin
         // map evaluator
         glPushAttrib(GL_EVAL_BIT);
         glMap1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
         glEnable(GL_MAP1_VERTEX_3);
      end;

      // start drawing the line
      if (FSplineMode=lsmNURBSCurve) and (FDivision>=2) then begin
         if (FNURBSOrder>0) and (FNURBSKnots.Count>0) then begin
            nurbsRenderer:=gluNewNurbsRenderer;
            try
               gluNurbsProperty(nurbsRenderer, GLU_SAMPLING_TOLERANCE, FNURBSTolerance);
               gluNurbsProperty(nurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
               gluBeginCurve(nurbsRenderer);
                  gluNurbsCurve(nurbsRenderer,
                                FNURBSKnots.Count, @FNURBSKnots.List[0],
                                3, @nodeBuffer[0],
                                FNURBSOrder,
                                GL_MAP1_VERTEX_3);
               gluEndCurve(nurbsRenderer);
            finally
               gluDeleteNurbsRenderer(nurbsRenderer);
            end;
         end;
      end else begin
         // lines, cubic splines or bezier
         if FSplineMode=lsmSegments then
            glBegin(GL_LINES)
         else glBegin(GL_LINE_STRIP);
         if (FDivision<2) or (FSplineMode in [lsmLines, lsmSegments]) then begin
            // standard line(s), draw directly
            if loUseNodeColorForLines in Options then begin
               // node color interpolation
               for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do begin
                  glColor4fv(Color.AsAddress);
                  glVertex3f(X, Y, Z);
               end;
            end else begin
               // single color
               for i:=0 to Nodes.Count-1 do with Nodes[i] do
                  glVertex3f(X, Y, Z);
            end;
         end else if FSplineMode=lsmCubicSpline then begin
            // cubic spline
            spline:=Nodes.CreateNewCubicSpline;
            try
               f:=1/FDivision;
               for i:=0 to (Nodes.Count-1)*FDivision  do begin
                  Spline.SplineXYZ(i*f, a, b, c);
                  if loUseNodeColorForLines in Options then begin
                     n:=(i div FDivision);
                     if n<Nodes.Count-1 then
                        VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                                   TGLLinesNode(Nodes[n+1]).Color.Color,
                                   (i mod FDivision)*f, vertexColor)
                     else SetVector(vertexColor, TGLLinesNode(Nodes[Nodes.Count-1]).Color.Color);
                     glColor4fv(@vertexColor);
                  end;
                  glVertex3f(a, b, c);
               end;
            finally
               spline.Free;
            end;
         end else if FSplineMode=lsmBezierSpline then begin
            f:=1/FDivision;
            for i:=0 to FDivision do
               glEvalCoord1f(i*f);
         end;
         glEnd;
      end;

      if FSplineMode=lsmBezierSpline then
         glPopAttrib;

      RestoreLineStyle;

      if FNodesAspect<>lnaInvisible then begin
         glPushAttrib(GL_ENABLE_BIT);
         if not rci.ignoreBlendingRequests then begin
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         end;
         for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do
            DrawNode(rci, X, Y, Z, Color);
         glPopAttrib;
      end;
   end;
end;

// AddNode (coords)
//
procedure TGLLines.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLLines.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TGLLines.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLLines.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

// ------------------
// ------------------ TGLCube ------------------
// ------------------

// Create
//
constructor TGLCube.Create(AOwner:Tcomponent);
begin
  inherited Create(AOwner);
  FCubeSize:=XYZVector;
  FParts:=[cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection:=ndOutside;
  ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// BuildList
//
procedure TGLCube.BuildList(var rci : TRenderContextInfo);
var
	hw, hh, hd, nd  : TGLFloat;
begin
   if FNormalDirection=ndInside then
      nd:=-1
   else nd:=1;
   hw:=FCubeSize[0]*0.5;
   hh:=FCubeSize[1]*0.5;
   hd:=FCubeSize[2]*0.5;

   glBegin(GL_QUADS);
   if cpFront in FParts then begin
      glNormal3f(  0,  0, nd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw,  hh, hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw,  hh, hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, hd);
   end;
   if cpBack in FParts then begin
      glNormal3f(  0,  0, -nd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f( hw,  hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw,  hh, -hd);
   end;
   if cpLeft in FParts then begin
      glNormal3f(-nd,  0,  0);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw,  hh,  hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw,  hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh,  hd);
   end;
   if cpRight in FParts then begin
      glNormal3f(nd,  0,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(hw,  hh,  hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(hw, -hh,  hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(hw,  hh, -hd);
   end;
   if cpTop in FParts then begin
      glNormal3f(  0, nd,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, hh,  hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, hh,  hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, hh, -hd);
   end;
   if cpBottom in FParts then begin
      glNormal3f(  0, -nd,  0);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, -hh,  hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, -hh,  hd);
   end;
   glEnd;
end;

// GenerateSilhouette
//
function TGLCube.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
	hw, hh, hd : TGLFloat;
   connectivity : TConnectivity;
   sil : TGLSilhouette;
begin
   Connectivity := TConnectivity.Create(true);

   hw:=FCubeSize[0]*0.5;
   hh:=FCubeSize[1]*0.5;
   hd:=FCubeSize[2]*0.5;

   if cpFront in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake( hw,  hh, hd),
        AffineVectorMake(-hw,  hh, hd),
        AffineVectorMake(-hw, -hh, hd),
        AffineVectorMake( hw, -hh, hd));
   end;
   if cpBack in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(hw,  hh, -hd),
        AffineVectorMake( hw, -hh, -hd),
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake(-hw,  hh, -hd));
   end;
   if cpLeft in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw,  hh,  hd),
        AffineVectorMake(-hw,  hh, -hd),
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake(-hw, -hh,  hd));
   end;
   if cpRight in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(hw,  hh,  hd),
        AffineVectorMake(hw, -hh,  hd),
        AffineVectorMake(hw, -hh, -hd),
        AffineVectorMake(hw,  hh, -hd));
   end;
   if cpTop in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw, hh, -hd),
        AffineVectorMake(-hw, hh,  hd),
        AffineVectorMake( hw, hh,  hd),
        AffineVectorMake( hw, hh, -hd));
   end;
   if cpBottom in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake( hw, -hh, -hd),
        AffineVectorMake( hw, -hh,  hd),
        AffineVectorMake(-hw, -hh,  hd));
   end;

   sil := nil;
   Connectivity.CreateSilhouette(
      silhouetteParameters, sil, false);

   result := sil;

   Connectivity.Free;
end;

// SetCubeWidth
//
procedure TGLCube.SetCubeWidth(AValue : TGLFloat);
begin
   if AValue<>FCubeSize[0] then begin
      FCubeSize[0]:=AValue;
      StructureChanged;
   end;
end;

// SetCubeHeight
//
procedure TGLCube.SetCubeHeight(AValue:TGLFloat);
begin
   if AValue<>FCubeSize[1] then begin
      FCubeSize[1]:=AValue;
      StructureChanged;
   end;
end;

// SetCubeDepth
//
procedure TGLCube.SetCubeDepth(AValue: TGLFloat);
begin
   if AValue<>FCubeSize[2] then begin
      FCubeSize[2]:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TGLCube.SetParts(AValue:TCubeParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// SetNormalDirection
//
procedure TGLCube.SetNormalDirection(AValue: TNormalDirection);
begin
   if AValue<>FNormalDirection then begin
      FNormalDirection:=AValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLCube.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLCube) then begin
      FCubeSize:=TGLCube(Source).FCubeSize;
      FParts:=TGLCube(Source).FParts;
      FNormalDirection:=TGLCube(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLCube.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=FCubeSize[0]*0.5;
   Result[1]:=FCubeSize[1]*0.5;
   Result[2]:=FCubeSize[2]*0.5;
   Result[3]:=0;
end;

// RayCastIntersect
//
function TGLCube.RayCastIntersect(const rayStart, rayVector : TVector;
                                  intersectPoint : PVector = nil;
                                  intersectNormal : PVector = nil) : Boolean;
var
   p     : array [0..5] of TVector;
   rv    : TVector;
   rs, r : TVector;
   i     : Integer;
   t, e  : Single;
   eSize : TAffineVector;
begin
   rs:=AbsoluteToLocal(rayStart);
   SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
   e:=0.5+0.0001; //Small value for floating point imprecisions
   eSize[0]:=FCubeSize[0]*e;
   eSize[1]:=FCubeSize[1]*e;
   eSize[2]:=FCubeSize[2]*e;
   p[0]:=XHmgVector;
   p[1]:=YHmgVector;
   p[2]:=ZHmgVector;
   SetVector(p[3], -1,  0,  0);
   SetVector(p[4],  0, -1,  0);
   SetVector(p[5],  0,  0, -1);
   for i:=0 to 5 do begin
      if VectorDotProduct(p[i], rv)>0 then begin
         t:=- (p[i][0]*rs[0]+p[i][1]*rs[1]+p[i][2]*rs[2]+0.5*FCubeSize[i mod 3])
             /(p[i][0]*rv[0]+p[i][1]*rv[1]+p[i][2]*rv[2]);
         MakePoint(r, rs[0]+t*rv[0], rs[1]+t*rv[1], rs[2]+t*rv[2]);
         if (Abs(r[0])<=eSize[0]) and (Abs(r[1])<=eSize[1]) and (Abs(r[2])<=eSize[2]) then begin
            if Assigned(intersectPoint) then
               MakePoint(intersectPoint^, LocalToAbsolute(r));
            if Assigned(intersectNormal) then
               MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
            Result:=True;
            Exit;
         end;
      end;
   end;
   Result:=False;
end;

// DefineProperties
//
procedure TGLCube.DefineProperties(Filer: TFiler);
begin
   inherited;
   Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
                              (FCubeSize[0]<>1) or (FCubeSize[1]<>1) or (FCubeSize[2]<>1));
end;

// ReadData
//
procedure TGLCube.ReadData(Stream: TStream);
begin
   with Stream do begin
      Read(FCubeSize, SizeOf(TAffineVector));
   end;
end;

// WriteData
//
procedure TGLCube.WriteData(Stream: TStream);
begin
   with Stream do begin
      Write(FCubeSize, SizeOf(TAffineVector));
   end;
end;

// ------------------
// ------------------ TGLFrustrum ------------------
// ------------------

constructor TGLFrustrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApexHeight := 1;
  FBaseWidth := 1;
  FBaseDepth := 1;
  FHeight := 0.5;
  FParts := cAllFrustrumParts;
  FNormalDirection := ndOutside;
end;

procedure TGLFrustrum.BuildList(var rci: TRenderContextInfo);
var
  HBW, HBD: TGLFloat; // half of width, half of depth at base
  HTW, HTD: TGLFloat; // half of width, half of depth at top of frustrum
  Sign: TGLFloat;     // +1 or -1
  Angle: TGLFloat;    // in radians
  ASin, ACos: TGLFloat;
begin
  if FNormalDirection = ndInside then
    Sign := -1
  else
    Sign := 1;
  HBW := FBaseWidth * 0.5;
  HBD := FBaseDepth * 0.5;
  HTW := HBW * (FApexHeight - FHeight) / FApexHeight;
  HTD := HBD * (FApexHeight - FHeight) / FApexHeight;

  glBegin(GL_QUADS);

  if [fpFront, fpBack] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBD); // angle of front plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpFront in FParts then
    begin
      glNormal3f(0, Sign * ACos, Sign * ASin);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, HBD);
    end;
    if fpBack in FParts then
    begin
      glNormal3f(0, Sign * ACos, -Sign * ASin);
      xglTexCoord2fv(@YTexPoint);     glVertex3f( HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f( HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight, -HTD);
    end;
  end;

  if [fpLeft, fpRight] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBW); // angle of side plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpLeft in FParts then
    begin
      glNormal3f(-Sign * ASin, Sign * ACos, 0);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight,  HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0,  HBD);
    end;
    if fpRight in FParts then
    begin
      glNormal3f(Sign * ASin, Sign * ACos, 0);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(HBW, 0,  HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(HTW, FHeight, -HTD);
    end;
  end;

  if (fpTop in FParts) and (FHeight < FApexHeight) then
  begin
    glNormal3f(0, Sign, 0);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HTW, FHeight,  HTD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HTW, FHeight,  HTD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, -HTD);
  end;
  if fpBottom in FParts then
  begin
    glNormal3f(0, -Sign, 0);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, -HBD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HBW, 0,  HBD);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HBW, 0,  HBD);
  end;

  glEnd;
end;

procedure TGLFrustrum.SetApexHeight(AValue: TGLFloat);
begin
  if (AValue <> FApexHeight) and (AValue >= 0) then
  begin
    FApexHeight := AValue;
    if FHeight > AValue then
      FHeight := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseDepth(AValue: TGLFloat);
begin
  if (AValue <> FBaseDepth) and (AValue >= 0) then
  begin
    FBaseDepth := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseWidth(AValue: TGLFloat);
begin
  if (AValue <> FBaseWidth) and (AValue >= 0) then
  begin
    FBaseWidth := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetHeight(AValue: TGLFloat);
begin
  if (AValue <> FHeight) and (AValue >= 0) then
  begin
    FHeight := AValue;
    if FApexHeight < AValue then
      FApexHeight := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetParts(AValue: TFrustrumParts);
begin
  if AValue <> FParts then
  begin
    FParts := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetNormalDirection(AValue: TNormalDirection);
begin
  if AValue <> FNormalDirection then
  begin
    FNormalDirection := AValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFrustrum) then
  begin
    FApexHeight := TGLFrustrum(Source).FApexHeight;
    FBaseDepth := TGLFrustrum(Source).FBaseDepth;
    FBaseWidth := TGLFrustrum(Source).FBaseWidth;
    FHeight := TGLFrustrum(Source).FHeight;
    FParts := TGLFrustrum(Source).FParts;
    FNormalDirection := TGLFrustrum(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TGLFrustrum.TopDepth: TGLFloat;
begin
  Result := FBaseDepth * (FApexHeight - FHeight) / FApexHeight;
end;

function TGLFrustrum.TopWidth: TGLFloat;
begin
  Result := FBaseWidth * (FApexHeight - FHeight) / FApexHeight;
end;

procedure TGLFrustrum.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('FrustrumSize', ReadData, WriteData,
    (FApexHeight <> 1) or (FBaseDepth <> 1) or (FBaseWidth <> 1) or
    (FHeight <> 0.5));
end;

procedure TGLFrustrum.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FApexHeight, SizeOf(FApexHeight));
    Read(FBaseDepth, SizeOf(FBaseDepth));
    Read(FBaseWidth, SizeOf(FBaseWidth));
    Read(FHeight, SizeOf(FHeight));
  end;
end;

procedure TGLFrustrum.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FApexHeight, SizeOf(FApexHeight));
    Write(FBaseDepth, SizeOf(FBaseDepth));
    Write(FBaseWidth, SizeOf(FBaseWidth));
    Write(FHeight, SizeOf(FHeight));
  end;
end;

//----------------- TGLQuadricObject ---------------------------------------------

// Create
//
constructor TGLQuadricObject.Create(AOwner : TComponent);
begin
  inherited;
  FNormals:=nsSmooth;
  FNormalDirection:=ndOutside;
end;

// SetNormals
//
procedure TGLQuadricObject.SetNormals(aValue : TNormalSmoothing);
begin
	if AValue<>FNormals then begin
		FNormals:=AValue;
		StructureChanged;
	end;
end;

// SetNormalDirection
//
procedure TGLQuadricObject.SetNormalDirection(aValue : TNormalDirection);
begin
	if AValue<>FNormalDirection then begin
		FNormalDirection:=AValue;
		StructureChanged;
  	end;
end;

// SetupQuadricParams
//
procedure TGLQuadricObject.SetupQuadricParams(quadric : PGLUquadricObj);
const
   cNormalSmoothinToEnum : array [nsFlat..nsNone] of TGLEnum = (
         GLU_FLAT, GLU_SMOOTH, GLU_NONE );
var
	withTexture : Boolean;
begin
	gluQuadricDrawStyle(Quadric, GLU_FILL);
	gluQuadricNormals(Quadric, cNormalSmoothinToEnum[FNormals]);
   SetNormalQuadricOrientation(Quadric);
{ TODO : things don't look that good here... we'll have to check it. }
	{WithTexture:=(stTexture1D in Scene.CurrentStates) or
					(stTexture2D in Scene.CurrentStates);}
	WithTexture:=True;
	gluQuadricTexture(Quadric, WithTexture);
end;

// SetNormalQuadricOrientation
//
procedure TGLQuadricObject.SetNormalQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_INSIDE, GLU_OUTSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//
procedure TGLQuadricObject.SetInvertedQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_OUTSIDE, GLU_INSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//
procedure TGLQuadricObject.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TGLQuadricObject) then begin
      FNormals:=TGLQuadricObject(Source).FNormals;
      FNormalDirection:=TGLQuadricObject(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

//----------------- TGLSphere ----------------------------------------------------

// Create
//
constructor TGLSphere.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FRadius:=0.5;
   FSlices:=16;
   FStacks:=16;
   FTop:=90;
   FBottom:=-90;
   FStart:=0;
   FStop:=360;
end;

// BuildList
//
procedure TGLSphere.BuildList(var rci : TRenderContextInfo);
var
   V1, V2, N1 : TAffineVector;
   AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
   SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
   uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
   I, J: Integer;
   DoReverse: Boolean;
begin         
   DoReverse:=(FNormalDirection=ndInside);
   glPushAttrib(GL_POLYGON_BIT);
   if DoReverse then
      InvertGLFrontFace;

   // common settings
   AngTop:=DegToRad(FTop);
   AngBottom:=DegToRad(FBottom);
   AngStart:=DegToRad(FStart);
   AngStop:=DegToRad(FStop);
   StepH:=(AngStop - AngStart) / FSlices;
   StepV:=(AngTop - AngBottom) / FStacks;
   glPushMatrix;
   glScalef(Radius, Radius, Radius);

   // top cap
   if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngTop, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, -1, 0)
      else glNormal3f(0, 1, 0);
      if FTopCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1[1]:=SinP;
      Theta:=AngStart;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP*SinT;
         V1[2]:=CosP*CosT;
         if FTopCap=ctCenter then begin
            N1:=VectorPerpendicular(YVector, V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT / 2 + 0.5, CosT / 2 + 0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta + StepH;
      end;
      glEnd;
   end;

   // main body
   Phi:=AngTop;
   Phi2:=Phi-StepV;
   uTexFactor:=1/FSlices;
   vTexFactor:=1/FStacks;

   for J:=0 to FStacks-1 do begin
      Theta:=AngStart;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      V1[1]:=SinP;
      V2[1]:=SinP2;
      vTexCoord0:=1-j*vTexFactor;
      vTexCoord1:=1-(j+1)*vTexFactor;

      glBegin(GL_TRIANGLE_STRIP);
      for i:=0 to FSlices do begin

         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V2[0]:=CosP2 * SinT;
         V1[2]:=CosP * CosT;
         V2[2]:=CosP2 * CosT;

         uTexCoord:=i*uTexFactor;
         xglTexCoord2f(uTexCoord, vTexCoord0);
         if DoReverse then begin
            N1:=V1;
            NegateVector(N1);
            glNormal3fv(@N1);
         end else glNormal3fv(@V1);
         glVertex3fv(@V1);

         xglTexCoord2f(uTexCoord, vTexCoord1);
         if DoReverse then begin
            N1:=V2;
            NegateVector(N1);
            glNormal3fv(@N1);
         end else glNormal3fv(@V2);
         glVertex3fv(@V2);

         Theta:=Theta+StepH;
      end;
      glEnd;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;

   // bottom cap
   if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngBottom, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, 1, 0)
      else glNormal3f(0, -1, 0);
      if FBottomCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1[1]:=SinP;
      Theta:=AngStop;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V1[2]:=CosP * CosT;
         if FTopCap = ctCenter then begin
            N1:=VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT*0.5+0.5, CosT*0.5+0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta - StepH;
      end;
      glEnd;
   end;
   if DoReverse then
      InvertGLFrontFace;
   glPopMatrix;
   glPopAttrib;
end;

// RayCastIntersect
//
function TGLSphere.RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   i1, i2 : TVector;
   localStart, localVector : TVector;
begin
   // compute coefficients of quartic polynomial
   SetVector(localStart,  AbsoluteToLocal(rayStart));
   SetVector(localVector, AbsoluteToLocal(rayVector));
   NormalizeVector(localVector);
   if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1, i2)>0 then begin
      Result:=True;
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^,  LocalToAbsolute(i1));
      if Assigned(intersectNormal) then begin
         i1[3]:=0; // vector transform
         SetVector(intersectNormal^, LocalToAbsolute(i1));
      end;
   end else Result:=False;
end;

// SetBottom
//
procedure TGLSphere.SetBottom(AValue: TAngleLimit1);
begin
   if FBottom<>AValue then begin
      FBottom:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetBottomCap(AValue: TCapType);

begin
  if FBottomCap<>AValue then
  begin
    FBottomCap:=AValue;
    StructureChanged;
  end;
end;

// SetRadius
//
procedure TGLSphere.SetRadius(const aValue : TGLFloat);
begin
   if aValue<>FRadius then begin
      FRadius:=aValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetSlices(AValue:TGLInt);

begin
  if AValue<>FSlices then
  begin
    FSlices:=AValue;
    if FSlices = 0 then FSlices:=1;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetStacks(AValue:TGLInt);

begin
  if AValue<>FStacks then
  begin
    FStacks:=AValue;
    if FStacks = 0 then FStacks:=1;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetStart(AValue: TAngleLimit2);
begin
   if FStart<>AValue then begin
      Assert(AValue <= FStop);
      FStart:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetStop(AValue: TAngleLimit2);
begin
   if FStop<>AValue then begin
      Assert(AValue >= FStart);
      FStop:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetTop(AValue: TAngleLimit1);
begin
   if FTop<>AValue then begin
      FTop:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.SetTopCap(AValue: TCapType);

begin
  if FTopCap<>AValue then
  begin
    FTopCap:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLSphere.Assign(Source:TPersistent);

begin
  if assigned(Source) and (Source is TGLSphere) then
  begin
    FRadius:=TGLSphere(Source).FRadius;
    FSlices:=TGLSphere(Source).FSlices;
    FStacks:=TGLSphere(Source).FStacks;
    FBottom:=TGLSphere(Source).FBottom;
    FTop:=TGLSphere(Source).FTop;
    FStart:=TGLSphere(Source).FStart;
    FStop:=TGLSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLSphere.AxisAlignedDimensions : TVector;
// ToDo: take bottom and top into account
begin
   VectorScale(Scale.AsVector, Abs(FRadius), Result);
end;
}
// AxisAlignedDimensions
//
function TGLSphere.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=Abs(FRadius);
   Result[1]:=Abs(FRadius);
   Result[2]:=Abs(FRadius);
   Result[3]:=0;
end;


//----------------- TGLDisk ------------------------------------------------------

// Create
//
constructor TGLDisk.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius:=0.5;
  FInnerRadius:=0;
  FSlices:=16;
  FLoops:=2;
  FStartAngle:=0;
  FSweepAngle:=360;
end;

// BuildList
//
procedure TGLDisk.BuildList(var rci : TRenderContextInfo);
var
   quadric : PGLUquadricObj;
begin
   quadric:=gluNewQuadric();
   SetupQuadricParams(Quadric);
   gluPartialDisk(Quadric, FInnerRadius, FOuterRadius, FSlices, FLoops, FStartAngle, FSweepAngle);
   gluDeleteQuadric(Quadric);
end;

// SetOuterRadius
//
procedure TGLDisk.SetOuterRadius(AValue:TGLFloat);
begin
   if AValue<>FOuterRadius then begin
      FOuterRadius:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TGLDisk.SetInnerRadius(AValue:TGLFloat);

begin
  if AValue<>FInnerRadius then
  begin
    FInnerRadius:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLDisk.SetSlices(AValue:TGLInt);

begin
  if AValue<>FSlices then
  begin
    FSlices:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLDisk.SetLoops(AValue:TGLInt);

begin
  if AValue<>FLoops then
  begin
    FLoops:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLDisk.SetStartAngle(AValue:TGLFloat);

begin
  if AValue<>FStartAngle then
  begin
    FStartAngle:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLDisk.SetSweepAngle(AValue:TGLFloat);

begin
  if AValue<>FSweepAngle then
  begin
    FSweepAngle:=AValue;
    StructureChanged;
  end;
end;

// Assign
//
procedure TGLDisk.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TGLDisk) then begin
      FOuterRadius:=TGLDisk(Source).FOuterRadius;
      FInnerRadius:=TGLDisk(Source).FInnerRadius;
      FSlices:=TGLDisk(Source).FSlices;
      FLoops:=TGLDisk(Source).FLoops;
      FStartAngle:=TGLDisk(Source).FStartAngle;
      FSweepAngle:=TGLDisk(Source).FSweepAngle;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLDisk.AxisAlignedDimensions : TVector;
var
  r : TGLFloat;
begin
   r:=Abs(FOuterRadius);
   Result:=VectorMake(r*Scale.DirectX, r*Scale.DirectY, 0);
end;
}

// AxisAlignedDimensions
//
function TGLDisk.AxisAlignedDimensionsUnscaled : TVector;
var
  r : TGLFloat;
begin
   r:=Abs(FOuterRadius);
   Result:=VectorMake(r{*Scale.DirectX}, r{*Scale.DirectY}, 0);
end;

// RayCastIntersect
//
function TGLDisk.RayCastIntersect(const rayStart, rayVector : TVector;
                                intersectPoint : PVector = nil;
                                intersectNormal : PVector = nil) : Boolean;
var
   ip : TVector;
   d : Single;
begin
   // start and sweep angle aren't honoured yet!
   if RayCastPlaneIntersect(rayStart, rayVector, AbsolutePosition, AbsoluteDirection, @ip) then begin
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^, ip);
      d:=VectorNorm(AbsoluteToLocal(ip));
      if (d>=Sqr(InnerRadius)) and (d<=Sqr(OuterRadius)) then begin
         if Assigned(intersectNormal) then
            SetVector(intersectNormal^, AbsoluteUp);
         Result:=True;
      end else Result:=False;
   end else Result:=False;
end;

//----------------- TGLCylinderBase ----------------------------------------------

// Create
//
constructor TGLCylinderBase.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBottomRadius:=0.5;
  FHeight:=1;
  FSlices:=16;
  FStacks:=4;
  FLoops:=1;
end;

// SetBottomRadius
//
procedure TGLCylinderBase.SetBottomRadius(AValue : TGLFloat);
begin
	if AValue<>FBottomRadius then begin
		FBottomRadius:=AValue;
		StructureChanged;
	end;
end;

// SetHeight
//
procedure TGLCylinderBase.SetHeight(AValue:TGLFloat);
begin
	if AValue<>FHeight then begin
		FHeight:=AValue;
		StructureChanged;
	end;
end;

// SetSlices
//
procedure TGLCylinderBase.SetSlices(AValue : TGLInt);
begin
	if AValue<>FSlices then begin
		FSlices:=AValue;
		StructureChanged;
  	end;
end;

// SetStack
//
procedure TGLCylinderBase.SetStacks(AValue : TGLInt);
begin
	if AValue<>FStacks then begin
		FStacks:=AValue;
		StructureChanged;
	end;
end;

// SetLoops
//
procedure TGLCylinderBase.SetLoops(AValue : TGLInt);
begin
	if (AValue>=1) and (AValue<>FLoops) then begin
		FLoops:=AValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TGLCylinderBase.Assign(Source : TPersistent);
begin
	if assigned(Source) and (Source is TGLCylinderBase) then begin
		FBottomRadius:=TGLCylinderBase(Source).FBottomRadius;
		FSlices:=TGLCylinderBase(Source).FSlices;
		FStacks:=TGLCylinderBase(Source).FStacks;
		FLoops :=TGLCylinderBase(Source).FLoops;
		FHeight:=TGLCylinderBase(Source).FHeight;
	end;
	inherited Assign(Source);
end;

//----------------- TGLCone ------------------------------------------------------

// Create
//
constructor TGLCone.Create(AOwner:TComponent);
begin
	inherited Create(AOwner);
	FParts:=[coSides, coBottom];
end;

// BuildList
//
procedure TGLCone.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric();
	SetupQuadricParams(Quadric);
	glRotated(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if coSides in FParts then
		gluCylinder(quadric, BottomRadius, 0, Height, Slices, Stacks);
	if coBottom in FParts then begin
		// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, BottomRadius, Slices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetParts
//
procedure TGLCone.SetParts(AValue : TConeParts);
begin
	if AValue<>FParts then begin
		FParts:=AValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TGLCone.Assign(Source: TPersistent);
begin
	if Assigned(Source) and (Source is TGLCone) then begin
		FParts:=TGLCone(Source).FParts;
	end;
  	inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLCone.AxisAlignedDimensions : TVector;
var
   r : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   Result:=VectorMake(r*Scale.DirectX, 0.5*FHeight*Scale.DirectY, r*Scale.DirectZ);
end;
}
// AxisAlignedDimensions
//
function TGLCone.AxisAlignedDimensionsUnscaled : TVector;
var
   r : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   Result:=VectorMake(r{*Scale.DirectX}, 0.5*FHeight{*Scale.DirectY}, r{*Scale.DirectZ});
end;


//----------------- TGLCylinder --------------------------------------------------

// Create
//
constructor TGLCylinder.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTopRadius:=0.5;
   FParts:=[cySides, cyBottom, cyTop];
   FAlignment:=caCenter;
end;

// BuildList
//
procedure TGLCylinder.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
   case Alignment of
      caTop : glTranslatef(0, 0, -FHeight);
      caBottom : ;
   else // caCenter
   	glTranslatef(0, 0, -FHeight*0.5);
   end;
	if cySides in FParts then
		gluCylinder(Quadric, FBottomRadius, FTopRadius, FHeight, FSlices, FStacks);
	if cyTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
	if cyBottom in FParts then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetTopRadius
//
procedure TGLCylinder.SetTopRadius(AValue: TGLFloat);
begin
   if AValue<>FTopRadius then begin
      FTopRadius:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TGLCylinder.SetParts(AValue: TCylinderParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// SetAlignment
//
procedure TGLCylinder.SetAlignment(val : TCylinderAlignment);
begin
   if val<>FAlignment then begin
      FAlignment:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLCylinder.Assign(Source: TPersistent);
begin
   if Assigned(SOurce) and (Source is TGLCylinder) then begin
      FParts:=TGLCylinder(Source).FParts;
      FTopRadius:=TGLCylinder(Source).FTopRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLCylinder.AxisAlignedDimensions: TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FBottomRadius);
  r1:=Abs(FTopRadius);
  if r1>r then r:=r1;
  Result:=VectorMake(r, 0.5*FHeight, r);
  ScaleVector(Result, Scale.AsVector);
end;
}
// AxisAlignedDimensions
//
function TGLCylinder.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FBottomRadius);
  r1:=Abs(FTopRadius);
  if r1>r then r:=r1;
  Result:=VectorMake(r, 0.5*FHeight, r);
//  ScaleVector(Result, Scale.AsVector);
end;

// RayCastIntersect
//
function TGLCylinder.RayCastIntersect(const rayStart, rayVector : TVector;
                                    intersectPoint : PVector = nil;
                                    intersectNormal : PVector = nil) : Boolean;
const
   cOne : Single = 1;
var
   locRayStart, locRayVector, ip : TVector;
   poly : array [0..2] of Double;
   roots : TDoubleArray;
   minRoot : Double;
   t, tr2, invRayVector1, hTop, hBottom : Single;
   tPlaneMin, tPlaneMax : Single;
begin
   Result:=False;
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);

   case Alignment of
      caTop : begin
         hTop:=0;
         hBottom:=-Height;
      end;
      caBottom : begin
         hTop:=Height;
         hBottom:=0;
      end;
   else
      // caCenter
      hTop:=Height*0.5;
      hBottom:=-hTop;
   end;

   if locRayVector[1]=0 then begin
      // intersect if ray shot through the top/bottom planes
      if (locRayStart[0]>hTop) or (locRayStart[0]<hBottom) then
         Exit;
      tPlaneMin:=-1e99;
      tPlaneMax:=1e99;
   end else begin
      invRayVector1:=cOne/locRayVector[1];
      tr2:=Sqr(TopRadius);

      // compute intersection with topPlane
      t:=(hTop-locRayStart[1])*invRayVector1;
      if (t>0) and (cyTop in Parts) then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         if Sqr(ip[0])+Sqr(ip[2])<=tr2 then begin
            // intersect with top plane
            if Assigned(intersectPoint) then
               intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], hTop, ip[2], 1));
            if Assigned(intersectNormal) then
               intersectNormal^:=LocalToAbsolute(YHmgVector);
            Result:=True;
         end;
      end;
      tPlaneMin:=t;
      tPlaneMax:=t;
      // compute intersection with bottomPlane
      t:=(hBottom-locRayStart[1])*invRayVector1;
      if (t>0) and (cyBottom in Parts) then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         if (t<tPlaneMin) or (not (cyTop in Parts)) then begin
            if Sqr(ip[0])+Sqr(ip[2])<=tr2 then begin
               // intersect with top plane
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], hBottom, ip[2], 1));
               if Assigned(intersectNormal) then
                  intersectNormal^:=LocalToAbsolute(VectorNegate(YHmgVector));
               Result:=True;
            end;
         end;
      end;
      if t<tPlaneMin then
         tPlaneMin:=t;
      if t>tPlaneMax then
         tPlaneMax:=t;
   end;
   if cySides in Parts then begin
      // intersect against cylinder infinite cylinder
      poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopRadius);
      poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
      poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
      roots:=SolveQuadric(@poly);
      if MinPositiveCoef(roots, minRoot) then begin
         t:=minRoot;
         if (t>=tPlaneMin) and (t<tPlaneMax) then begin
            if Assigned(intersectPoint) or Assigned(intersectNormal) then begin
               ip:=VectorCombine(locRayStart, locRayVector, 1, t);
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(ip);
               if Assigned(intersectNormal) then begin
                  ip[1]:=0;
                  ip[3]:=0;
                  intersectNormal^:=LocalToAbsolute(ip);
               end;
            end;
            Result:=True;
         end;
      end;
   end else SetLength(roots, 0);
end;

// Align
//
procedure TGLCylinder.Align(const startPoint, endPoint : TVector);
var
   dir : TAffineVector;
begin
   AbsolutePosition:=startPoint;
   VectorSubtract(endPoint, startPoint, dir);
   if Parent<>nil then
      dir:=Parent.AbsoluteToLocal(dir);
   Up.AsAffineVector:=dir;
   Height:=VectorLength(dir);
   Lift(Height*0.5);
   Alignment:=caCenter;
end;

// Align
//
procedure TGLCylinder.Align(const startObj, endObj : TGLBaseSceneObject);
begin
   Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//
procedure TGLCylinder.Align(const startPoint, endPoint : TAffineVector);
begin
   Align(PointMake(startPoint), PointMake(endPoint));
end;

//----------------- TGLAnnulus ---------------------------------------------------

// Create
//
constructor TGLAnnulus.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fBottomInnerRadius:=0.3;
   fTopInnerRadius:=0.3;
   fTopRadius:=0.5;
   fParts:=[anInnerSides, anOuterSides, anBottom, anTop];
end;

// SetBottomInnerRadius
//
procedure TGLAnnulus.SetBottomInnerRadius(AValue : TGLFloat);
begin
	if AValue<>FBottomInnerRadius then begin
		FBottomInnerRadius:=AValue;
		StructureChanged;
	end;
end;

// SetTopRadius
//
procedure TGLAnnulus.SetTopRadius(AValue : TGLFloat);
begin
	if AValue<>FTopRadius then begin
		FTopRadius:=AValue;
		StructureChanged;
	end;
end;

// SetTopInnerRadius
//
procedure TGLAnnulus.SetTopInnerRadius(AValue : TGLFloat);
begin
	if AValue<>FTopInnerRadius then begin
		FTopInnerRadius:=AValue;
		StructureChanged;
	end;
end;

// SetParts
//
procedure TGLAnnulus.SetParts(AValue: TAnnulusParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLAnnulus.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if anOuterSides in FParts then
		gluCylinder(Quadric, fBottomRadius, fTopRadius, fHeight, fSlices, fStacks);
	if anTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric,fTopInnerRadius, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
   if [anBottom, anInnerSides]*FParts<>[] then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
   	if anBottom in FParts then
	   	gluDisk(quadric,fBottominnerRadius,FBottomRadius, FSlices, FLoops);
      if anInnerSides in fParts then
         gluCylinder(Quadric, fBottomInnerRadius, fTopInnerRadius, fHeight, fSlices, fStacks);
   end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// Assign
//
procedure TGLAnnulus.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TGLAnnulus) then begin
      FParts:=TGLAnnulus(Source).FParts;
      FTopRadius:=TGLAnnulus(Source).FTopRadius;
      FTopInnerRadius:=TGLAnnulus(Source).fTopInnerRadius;
      FBottomRadius:=TGLAnnulus(Source).fBottomRadius;
      FBottomInnerRadius:=TGLAnnulus(Source).fbottomInnerRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
{function TGLAnnulus.AxisAlignedDimensions : TVector;
var
   r, r1 : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   r1:=Abs(FTopRadius);
   if r1>r then r:=r1;
   Result:=VectorMake(r, 0.5*FHeight, r);
   ScaleVector(Result, Scale.AsVector);
end;
}
// AxisAlignedDimensions
//
function TGLAnnulus.AxisAlignedDimensionsUnscaled : TVector;
var
   r, r1 : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   r1:=Abs(FTopRadius);
   if r1>r then r:=r1;
   Result:=VectorMake(r, 0.5*FHeight, r);
end;

// RayCastIntersect
//
function TGLAnnulus.RayCastIntersect(const rayStart, rayVector : TVector;
                  intersectPoint, intersectNormal : PVector) : Boolean;
const
   cOne : Single = 1;
var
   locRayStart, locRayVector, ip : TVector;
   poly : array [0..2] of Double;
   t, tr2, invRayVector1 : Single;
   tPlaneMin, tPlaneMax : Single;
   tir2,d2		:single;
   Root 			:Double;
   Roots,tmpRoots	:TDoubleArray;
   FirstIntersected	:boolean;
   h1,h2,hTop,hBot	:single;
   Draw1,Draw2	:boolean;
begin
   Result:=False;
   FirstIntersected:=False;
   SetLength(tmpRoots, 0);
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);

   hTop:=Height*0.5;
   hBot:=-hTop;
   if locRayVector[1]<0 then begin // Sort the planes according to the direction of view
     h1:=hTop;   // Height of the 1st plane
     h2:=hBot;   // Height of the 2nd plane
     Draw1:=(anTop in Parts);    // 1st "cap" Must be drawn?
     Draw2:=(anBottom in Parts);
     end
   else begin
     h1:=hBot;
     h2:=hTop;
     Draw1:=(anBottom in Parts);
     Draw2:=(anTop in Parts);
   end;//if

   if locRayVector[1]=0 then begin
      // intersect if ray shot through the top/bottom planes
      if (locRayStart[0]>hTop) or (locRayStart[0]<hBot) then
         Exit;
      tPlaneMin:=-1e99;
      tPlaneMax:=1e99;
   end else begin
      invRayVector1:=cOne/locRayVector[1];
      tr2:=Sqr(TopRadius);
      tir2:=Sqr(TopInnerRadius);
      FirstIntersected:=False;

      // compute intersection with first plane
      t:=(h1-locRayStart[1])*invRayVector1;
      if (t>0) and Draw1 then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         d2:=Sqr(ip[0])+Sqr(ip[2]);
         if (d2<=tr2)and(d2>=tir2) then begin
            // intersect with top plane
            FirstIntersected:=true;
            if Assigned(intersectPoint) then
               intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], h1, ip[2], 1));
            if Assigned(intersectNormal) then
               intersectNormal^:=LocalToAbsolute(YHmgVector);
            Result:=True;
         end;
      end;
      tPlaneMin:=t;
      tPlaneMax:=t;

      // compute intersection with second plane
      t:=(h2-locRayStart[1])*invRayVector1;
      if (t>0) and Draw2 then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         d2:=Sqr(ip[0])+Sqr(ip[2]);
         if (t<tPlaneMin) or (not FirstIntersected) then begin
            if (d2<=tr2)and(d2>=tir2) then begin
               // intersect with top plane
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], h2, ip[2], 1));
               if Assigned(intersectNormal) then
                  intersectNormal^:=LocalToAbsolute(VectorNegate(YHmgVector));
               Result:=True;
            end;
         end;
      end;
      if t<tPlaneMin then begin
         tPlaneMin:=t;
      end;//if
      if t>tPlaneMax then
         tPlaneMax:=t;
   end;

   try
     SetLength(Roots,4);
     Roots[0]:=-1; Roots[1]:=-1; Roots[2]:=-1; Roots[3]:=-1; // By default, side is behind rayStart

     {Compute roots for outer cylinder}
     if anOuterSides in Parts then begin
        // intersect against infinite cylinder, will be cut by tPlaneMine and tPlaneMax
        poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopRadius);
        poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
        poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
        tmpRoots:=SolveQuadric(@poly);  // Intersect coordinates on rayVector (rayStart=0)
        if (High(tmproots)>=0)and  // Does root exist?
           ((tmpRoots[0]>tPlaneMin) and not FirstIntersected) and // In the annulus and not masked by first cap
           ((tmpRoots[0]<tPlaneMax) )  // In the annulus
        then Roots[0]:=tmpRoots[0];
        if (High(tmproots)>=1)and
           ((tmpRoots[1]>tPlaneMin) and not FirstIntersected) and
           ((tmpRoots[1]<tPlaneMax) )
        then Roots[1]:=tmpRoots[1];
      end;//if

     {Compute roots for inner cylinder}
     if anInnerSides in Parts then begin
        // intersect against infinite cylinder
        poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopInnerRadius);
        poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
        poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
        tmproots:=SolveQuadric(@poly);
        if (High(tmproots)>=0)and
           ((tmpRoots[0]>tPlaneMin) and not FirstIntersected) and
           ((tmpRoots[0]<tPlaneMax) )
        then Roots[2]:=tmpRoots[0];
        if (High(tmproots)>=1)and
           ((tmpRoots[1]>tPlaneMin) and not FirstIntersected) and
           ((tmpRoots[1]<tPlaneMax) )
        then Roots[3]:=tmpRoots[1];
     end;//if

    {Find the first intersection point and compute its coordinates and normal}
    if MinPositiveCoef(Roots, Root) then begin
       t:=Root;
       if (t>=tPlaneMin) and (t<tPlaneMax) then begin
          if Assigned(intersectPoint) or Assigned(intersectNormal) then begin
             ip:=VectorCombine(locRayStart, locRayVector, 1, t);
             if Assigned(intersectPoint) then
                intersectPoint^:=LocalToAbsolute(ip);
             if Assigned(intersectNormal) then begin
                ip[1]:=0;
                ip[3]:=0;
                intersectNormal^:=LocalToAbsolute(ip);
             end;
          end;
          Result:=True;
       end;
    end;

  finally
    Roots:=nil;
    tmpRoots:=nil;
  end;//finally
end;

// ------------------
// ------------------ TGLTorus ------------------
// ------------------

// Create
//
constructor TGLTorus.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FRings:=25;
   FSides:=15;
   FMinorRadius:=0.1;
   FMajorRadius:=0.4;
end;

// BuildList
//
procedure TGLTorus.BuildList(var rci : TRenderContextInfo);
var
   I, J         : Integer;
   Theta, Phi, Theta1, cosPhi, sinPhi, dist : TGLFloat;
   cosTheta, sinTheta: TGLFloat;
   cosTheta1, sinTheta1: TGLFloat;
   ringDelta, sideDelta: TGLFloat;
   iFact, jFact : Single;
begin
   // handle texture generation
   ringDelta:=c2PI/FRings;
   sideDelta:=c2PI/FSides;
   theta:=0;
   cosTheta:=1;
   sinTheta:=0;
   iFact:=1/FRings;
   jFact:=1/FSides;
   for I:=FRings-1 downto 0 do  begin
      theta1:=theta+ringDelta;
      SinCos(theta1, sinTheta1, cosTheta1);
      glBegin(GL_QUAD_STRIP);
      phi:=0;
      for J:=FSides downto 0 do begin
         phi:=phi+sideDelta;
         SinCos(phi, sinPhi, cosPhi);
         dist:=FMajorRadius+FMinorRadius*cosPhi;
         xglTexCoord2f((i+1)*iFact, j*jFact);
         glNormal3f(cosTheta1*cosPhi, -sinTheta1*cosPhi, sinPhi);
         glVertex3f(cosTheta1*dist, -sinTheta1*dist, FMinorRadius*sinPhi);
         xglTexCoord2f(i*iFact, j*jFact);
         glNormal3f(cosTheta*cosPhi, -sinTheta*cosPhi, sinPhi);
         glVertex3f(cosTheta*dist, -sinTheta*dist, FMinorRadius*sinPhi);
      end;
      glEnd;
      theta:=theta1;
      cosTheta:=cosTheta1;
      sinTheta:=sinTheta1;
   end;
end;

// SetMajorRadius
//
procedure TGLTorus.SetMajorRadius(const AValue: Single);
begin
   if FMajorRadius<>AValue then begin
      FMajorRadius:=AValue;
      StructureChanged;
   end;
end;

// SetMinorRadius
//
procedure TGLTorus.SetMinorRadius(const AValue : Single);
begin
   if FMinorRadius<>AValue then begin
      FMinorRadius:=AValue;
      StructureChanged;
   end;
end;

// SetRings
//
procedure TGLTorus.SetRings(AValue: Cardinal);
begin
   if FRings<>AValue then begin
      FRings:=AValue;
      if FRings<2 then FRings:=2;
      StructureChanged;
   end;
end;

// SetSides
//
procedure TGLTorus.SetSides(aValue : Cardinal);
begin
   if FSides<>aValue then begin
      FSides:=aValue;
      if FSides<3 then FSides:=3;
      StructureChanged;
   end;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLTorus.AxisAlignedDimensionsUnscaled : TVector;
var
   r, r1 : TGLFloat;
begin
   r:=Abs(FMajorRadius);
   r1:=Abs(FMinorRadius);
   Result:=VectorMake(r+r1, r+r1, r1); //Danb
end;

// RayCastIntersect
//
function TGLTorus.RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean;
var
   i : Integer;
   fRo2, fRi2, fDE, fVal, r, nearest : Double;
   polynom : array [0..4] of Double;
   polyRoots : TDoubleArray;
   localStart, localVector : TVector;
   vi, vc : TVector;
begin
   // compute coefficients of quartic polynomial
   fRo2:=Sqr(MajorRadius);
   fRi2:=Sqr(MinorRadius);
   localStart :=AbsoluteToLocal(rayStart);
   localVector:=AbsoluteToLocal(rayVector);
   NormalizeVector(localVector);
   fDE :=VectorDotProduct(localStart, localVector);
   fVal:=VectorNorm(localStart)-(fRo2+fRi2);

   polynom[0] := Sqr(fVal) - 4.0*fRo2*(fRi2 - Sqr(localStart[2]));
   polynom[1] := 4.0*fDE*fVal + 8.0*fRo2*localVector[2]*localStart[2];
   polynom[2] := 2.0*fVal + 4.0*Sqr(fDE) + 4.0*fRo2*Sqr(localVector[2]);
   polynom[3] := 4.0*fDE;
   polynom[4] := 1;

   // solve the quartic
   polyRoots:=SolveQuartic(@polynom[0]);

   // search for closest point
   Result:=(Length(polyRoots)>0);
   if Result then begin
      nearest:=1e20;
      for i:=0 to High(polyRoots) do begin
         r:=polyRoots[i];
         if (r>0) and (r<nearest) then begin
            nearest:=r;
            Result:=True;
         end;
      end;
      vi:=VectorCombine(localStart, localVector, 1, nearest);
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^, LocalToAbsolute(vi));
      if Assigned(intersectNormal) then begin
         // project vi on local torus plane
         vc[0]:=vi[0];
         vc[1]:=vi[1];
         vc[2]:=0;
         // project vc on MajorRadius circle
         ScaleVector(vc, MajorRadius/(VectorLength(vc)+0.000001));
         // calculate circle to intersect vector (gives normal);
         SubtractVector(vi, vc);
         // return to absolute coordinates and normalize
         vi[3]:=0;
         SetVector(intersectNormal^, LocalToAbsolute(vi));
      end;
   end;
end;

// ------------------
// ------------------ TGLTeapot ------------------
// ------------------

// Create
//
constructor TGLTeapot.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FGrid:=5;
end;

// BuildList
//
procedure TGLTeapot.BuildList(var rci : TRenderContextInfo);

const PatchData : array[0..9, 0..15] of Integer =
      ((102, 103, 104, 105,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15), // rim
       ( 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27), // body
       ( 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40), // body
       ( 96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101,  0,  1,  2,  3), // lid
       (  0,  1,  2,  3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117), // lid
       (118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37), // bottom
       ( 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56), // handle
       ( 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67), // handle
       ( 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83), // spout
       ( 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95));// spout

      CPData : array[0..126, 0..2] of TGLFloat =
      ((0.2, 0, 2.7), (0.2, -0.112, 2.7), (0.112, -0.2, 2.7), (0, -0.2, 2.7), (1.3375, 0, 2.53125),
       (1.3375, -0.749, 2.53125), (0.749, -1.3375, 2.53125), (0, -1.3375, 2.53125),
       (1.4375, 0, 2.53125), (1.4375, -0.805, 2.53125), (0.805, -1.4375, 2.53125),
       (0, -1.4375, 2.53125), (1.5, 0, 2.4), (1.5, -0.84, 2.4), (0.84, -1.5, 2.4), (0, -1.5, 2.4),
       (1.75, 0, 1.875), (1.75, -0.98, 1.875), (0.98, -1.75, 1.875), (0, -1.75, 1.875), (2, 0, 1.35),
       (2, -1.12, 1.35), (1.12, -2, 1.35), (0, -2, 1.35), (2, 0, 0.9), (2, -1.12, 0.9), (1.12, -2, 0.9),
       (0, -2, 0.9), (-2, 0, 0.9), (2, 0, 0.45), (2, -1.12, 0.45), (1.12, -2, 0.45), (0, -2, 0.45),
       (1.5, 0, 0.225), (1.5, -0.84, 0.225), (0.84, -1.5, 0.225), (0, -1.5, 0.225), (1.5, 0, 0.15),
       (1.5, -0.84, 0.15), (0.84, -1.5, 0.15), (0, -1.5, 0.15), (-1.6, 0, 2.025), (-1.6, -0.3, 2.025),
       (-1.5, -0.3, 2.25), (-1.5, 0, 2.25), (-2.3, 0, 2.025), (-2.3, -0.3, 2.025), (-2.5, -0.3, 2.25),
       (-2.5, 0, 2.25), (-2.7, 0, 2.025), (-2.7, -0.3, 2.025), (-3, -0.3, 2.25), (-3, 0, 2.25),
       (-2.7, 0, 1.8), (-2.7, -0.3, 1.8), (-3, -0.3, 1.8), (-3, 0, 1.8), (-2.7, 0, 1.575),
       (-2.7, -0.3, 1.575), (-3, -0.3, 1.35), (-3, 0, 1.35), (-2.5, 0, 1.125), (-2.5, -0.3, 1.125),
       (-2.65, -0.3, 0.9375), (-2.65, 0, 0.9375), (-2, -0.3, 0.9), (-1.9, -0.3, 0.6), (-1.9, 0, 0.6),
       (1.7, 0, 1.425), (1.7, -0.66, 1.425), (1.7, -0.66, 0.6), (1.7, 0, 0.6), (2.6, 0, 1.425),
       (2.6, -0.66, 1.425), (3.1, -0.66, 0.825), (3.1, 0, 0.825), (2.3, 0, 2.1), (2.3, -0.25, 2.1),
       (2.4, -0.25, 2.025), (2.4, 0, 2.025), (2.7, 0, 2.4), (2.7, -0.25, 2.4), (3.3, -0.25, 2.4),
       (3.3, 0, 2.4), (2.8, 0, 2.475), (2.8, -0.25, 2.475), (3.525, -0.25, 2.49375),
       (3.525, 0, 2.49375), (2.9, 0, 2.475), (2.9, -0.15, 2.475), (3.45, -0.15, 2.5125),
       (3.45, 0, 2.5125), (2.8, 0, 2.4), (2.8, -0.15, 2.4), (3.2, 0.15, 2.4), (3.2, 0, 2.4),
       (0, 0, 3.15), (0.8, 0, 3.15), (0.8, -0.45, 3.15), (0.45, -0.8, 3.15), (0, -0.8, 3.15),
       (0, 0, 2.85), (1.4, 0, 2.4), (1.4, -0.784, 2.4), (0.784, -1.4, 2.4), (0, -1.4, 2.4),
       (0.4, 0, 2.55), (0.4, -0.224, 2.55), (0.224, -0.4, 2.55), (0, -0.4, 2.55), (1.3, 0, 2.55),
       (1.3, -0.728, 2.55), (0.728, -1.3, 2.55), (0, -1.3, 2.55), (1.3, 0, 2.4), (1.3, -0.728, 2.4),
       (0.728, -1.3, 2.4), (0, -1.3, 2.4), (0, 0, 0), (1.425, -0.798, 0), (1.5, 0, 0.075), (1.425, 0, 0),
       (0.798, -1.425, 0), (0, -1.5, 0.075), (0, -1.425, 0), (1.5, -0.84, 0.075), (0.84, -1.5, 0.075));

      Tex : array[0..1, 0..1, 0..1] of TGLFloat = (((0, 0), (1, 0)), ((0, 1), (1, 1)));

var P, Q, R, S  : array[0..3, 0..3, 0..2] of TGLFloat;
    I, J, K, L,
    GRD      : Integer;

begin
  if FGrid < 2 then FGrid:=2;
  GRD:=FGrid;
  glPushMatrix;
  glTranslatef(0, -0.25, 0);
  glRotatef(-90, 1, 0, 0);
  glScalef(0.15, 0.15, 0.15);
  glPushAttrib(GL_POLYGON_BIT or GL_ENABLE_BIT or GL_EVAL_BIT);
  InvertGLFrontFace;
  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_MAP2_VERTEX_3);
  glEnable(GL_MAP2_TEXTURE_COORD_2);
  for I:=0 to 9 do begin
    for J:=0 to 3 do begin
      for K:=0 to 3 do begin
        for L:=0 to 2 do begin
          P[J, K, L]:=CPData[PatchData[I, J*4+K], L];
          Q[J, K, L]:=CPData[PatchData[I, J*4+(3-K)], L];
          if L = 1 then Q[J, K, L]:=-Q[J, K, L];
          if I < 6 then begin
            R[J, K, L]:=CPData[PatchData[I, J*4+(3-K)], L];
            if L = 0 then R[J, K, L]:=-R[J, K, L];
            S[J, K, L]:=CPData[PatchData[I, J*4+K], L];
            if L < 2 then S[J, K, L]:=-S[J, K, L];
          end;
        end;
      end;
    end;
    glMapGrid2f(GRD, 0, 1, GRD, 0, 1);
    glMap2f(GL_MAP2_TEXTURE_COORD_2, 0, 1, 2, 2, 0, 1, 4, 2, @Tex);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @P);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @Q);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    if I < 6 then begin
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @R);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @S);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    end;
  end;
  InvertGLFrontFace;
  glPopAttrib;
  glPopMatrix;
end;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

// DodecahedronBuildList
//
procedure DodecahedronBuildList;
const
   A = 1.61803398875; // (Sqrt(5)+1)/2
   B = 0.61803398875; // (Sqrt(5)-1)/2
   C = 1;
const
   Vertices : array [0..19, 0..2] of TGLFloat =
      ((-A, 0, B), (-A, 0, -B), (A, 0, -B), (A, 0, B),
       (B, -A, 0), (-B, -A, 0), (-B, A, 0), (B, A, 0),
       (0, B, -A), (0, -B, -A), (0, -B, A), (0, B, A),
       (-C, -C, C), (-C, -C, -C), (C, -C, -C), (C, -C, C),
       (-C, C, C), (-C, C, -C), (C, C, -C), (C, C, C));

   Polygons : array [0..11, 0..4] of TGLInt =
      (( 0, 12, 10, 11, 16),
       ( 1, 17, 8, 9, 13),
       ( 2, 14, 9, 8, 18),
       ( 3, 19, 11, 10, 15),
       ( 4, 14, 2, 3, 15),
       ( 5, 12, 0, 1, 13),
       ( 6, 17, 1, 0, 16),
       ( 7, 19, 3, 2, 18),
       ( 8, 17, 6, 7, 18),
       ( 9, 14, 4, 5, 13),
       (10, 12, 5, 4, 15),
       (11, 19, 7, 6, 16));

var
   I     : Integer;
   U, V, N : TAffineVector;
begin
   glPushMatrix;
   glScalef(0.3, 0.3, 0.3);
   for I:=0 to 11 do begin
      U[0]:=Vertices[Polygons[I, 2], 0]-Vertices[Polygons[I, 1], 0];
      U[1]:=Vertices[Polygons[I, 2], 1]-Vertices[Polygons[I, 1], 1];
      U[2]:=Vertices[Polygons[I, 2], 2]-Vertices[Polygons[I, 1], 2];

      V[0]:=Vertices[Polygons[I, 0], 0]-Vertices[Polygons[I, 1], 0];
      V[1]:=Vertices[Polygons[I, 0], 1]-Vertices[Polygons[I, 1], 1];
      V[2]:=Vertices[Polygons[I, 0], 2]-Vertices[Polygons[I, 1], 2];

      VectorCrossProduct(U, V, N);
      NormalizeVector(N);

      glBegin(GL_TRIANGLE_FAN);
         glNormal3fv(@N);
         glVertex3fv(@Vertices[Polygons[I, 0], 0]);
         glVertex3fv(@Vertices[Polygons[I, 1], 0]);
         glVertex3fv(@Vertices[Polygons[I, 2], 0]);
         glVertex3fv(@Vertices[Polygons[I, 3], 0]);
         glVertex3fv(@Vertices[Polygons[I, 4], 0]);
      glEnd;
   end;
   glPopMatrix;
end;

// BuildList
//
procedure TGLDodecahedron.BuildList(var rci : TRenderContextInfo);
begin
   DodecahedronBuildList;
end;

// ------------------
// ------------------ TGLArrowLine ------------------
// ------------------

// Create
//
constructor TGLArrowLine.Create(AOwner:TComponent);
begin
   inherited;
   fTopRadius:=0.1;
   BottomRadius:=0.1;
   fTopArrowHeadRadius:=0.2;
   fTopArrowHeadHeight:=0.5;
   fBottomArrowHeadRadius:=0.2;
   fBottomArrowHeadHeight:=0.5;
   FHeadStackingStyle:=ahssStacked;
   { by default there is not much point having the top of the line (cylinder)
     showing as it is coincidental with the Toparrowhead bottom.
     Note I've defaulted to "vector" type arrows (arrow head on top only}
   fParts:=[alLine, alTopArrow];
end;

// SetTopRadius
//
procedure TGLArrowLine.SetTopRadius(AValue:TGLFloat);
begin
   if AValue<>fTopRadius then begin
      fTopRadius:=AValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadHeight
//
procedure TGLArrowLine.SetTopArrowHeadHeight(AValue:TGLFloat);
begin
   if AValue<>fTopArrowHeadHeight then begin
      fTopArrowHeadHeight:=AValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadRadius
//
procedure TGLArrowLine.SetTopArrowHeadRadius(AValue:TGLFloat);
begin
   if AValue<>fTopArrowHeadRadius then begin
      fTopArrowHeadRadius:=AValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadHeight
//
procedure TGLArrowLine.SetBottomArrowHeadHeight(AValue:TGLFloat);
begin
   if AValue<>fBottomArrowHeadHeight then begin
      fBottomArrowHeadHeight:=AValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadRadius
//
procedure TGLArrowLine.SetBottomArrowHeadRadius(AValue:TGLFloat);
begin
   if AValue<>fBottomArrowHeadRadius then begin
      fBottomArrowHeadRadius:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TGLArrowLine.SetParts(AValue: TArrowLineParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// SetHeadStackingStyle
//
procedure TGLArrowLine.SetHeadStackingStyle(const val : TArrowHeadStackingStyle);
begin
   if val<>FHeadStackingStyle then begin
      FHeadStackingStyle:=val;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLArrowLine.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
   cylHeight, cylOffset, headInfluence : Single;
begin
   case HeadStackingStyle of
      ahssCentered : headInfluence:=0.5;
      ahssIncluded : headInfluence:=1;
   else // ahssStacked
      headInfluence:=0;
   end;
   cylHeight:=Height;
   cylOffset:=-FHeight*0.5;
   // create a new quadric
 	quadric:=gluNewQuadric;
   SetupQuadricParams(Quadric);
   // does the top arrow part - the cone
   if alTopArrow in Parts then begin
      cylHeight:=cylHeight-TopArrowHeadHeight*headInfluence;
      glPushMatrix;
      glTranslatef(0, 0, Height*0.5-TopArrowHeadHeight*headInfluence);
      gluCylinder(quadric, fTopArrowHeadRadius, 0, fTopArrowHeadHeight, Slices, Stacks);
     	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fTopRadius, fTopArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fTopArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the bottom arrow part - another cone
   if alBottomArrow in Parts then begin
      cylHeight:=cylHeight-BottomArrowHeadHeight*headInfluence;
      cylOffset:=cylOffset+BottomArrowHeadHeight*headInfluence;
      glPushMatrix;
      // make the bottom arrow point in the other direction
	   glRotatef(180, 1, 0, 0);
      glTranslatef(0, 0, Height*0.5-BottomArrowHeadHeight*headInfluence);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(quadric, fBottomArrowHeadRadius, 0, fBottomArrowHeadHeight, Slices, Stacks);
   	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fBottomRadius, fBottomArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fBottomArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the cylinder that makes the line
   if (cylHeight>0) and (alLine in Parts) then begin
      glPushMatrix;
      glTranslatef(0, 0, cylOffset);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(Quadric, FBottomRadius, FTopRadius, cylHeight, FSlices, FStacks);
      if not (alTopArrow in Parts) then begin
         glPushMatrix;
         glTranslatef(0, 0, cylHeight);
         gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
         glPopMatrix;
      end;
      if not (alBottomArrow in Parts) then begin
         // swap quadric orientation because top of a disk is defined as outside
         SetInvertedQuadricOrientation(quadric);
         gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
      end;
      glPopMatrix;
   end;
   gluDeleteQuadric(Quadric);
end;

// Assign
//
procedure TGLArrowLine.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TGLArrowLine) then begin
      FParts:=TGLArrowLine(Source).FParts;
      FTopRadius:=TGLArrowLine(Source).FTopRadius;
      fTopArrowHeadHeight:=TGLArrowLine(Source).fTopArrowHeadHeight;
      fTopArrowHeadRadius:=TGLArrowLine(Source).fTopArrowHeadRadius;
      fBottomArrowHeadHeight:=TGLArrowLine(Source).fBottomArrowHeadHeight;
      fBottomArrowHeadRadius:=TGLArrowLine(Source).fBottomArrowHeadRadius;
      FHeadStackingStyle:=TGLArrowLine(Source).FHeadStackingStyle;
   end;
   inherited Assign(Source);
end;

// ------------------
// ------------------ TGLPolygonBase ------------------
// ------------------

// Create
//
constructor TGLPolygonBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   CreateNodes;
   FDivision:=10;
   FSplineMode:=lsmLines;
end;

// CreateNodes
//
procedure TGLPolygonBase.CreateNodes;
begin
   FNodes:=TGLNodes.Create(Self);
end;

// Destroy
//
destructor TGLPolygonBase.Destroy;
begin
   FNodes.Free;
   inherited Destroy;
end;

// Assign
//
procedure TGLPolygonBase.Assign(Source: TPersistent);
begin
   if Source is TGLPolygonBase then begin
      SetNodes(TGLPolygonBase(Source).FNodes);
      FDivision:=TGLPolygonBase(Source).FDivision;
      FSplineMode:=TGLPolygonBase(Source).FSplineMode;
   end;
   inherited Assign(Source);
end;

// NotifyChange
//
procedure TGLPolygonBase.NotifyChange(Sender : TObject);
begin
   if Sender=Nodes then StructureChanged;
   inherited;
end;

// SetDivision
//
procedure TGLPolygonBase.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetNodes
//
procedure TGLPolygonBase.SetNodes(const aNodes : TGLNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetSplineMode
//
procedure TGLPolygonBase.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// AddNode (coords)
//
procedure TGLPolygonBase.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLPolygonBase.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TGLPolygonBase.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLPolygonBase.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

// ------------------
// ------------------ TGLPolygon ------------------
// ------------------

// Create
//
constructor TGLPolygon.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FParts:=[ppTop, ppBottom];
end;

// Destroy
//
destructor TGLPolygon.Destroy;
begin
   inherited Destroy;
end;

// SetParts
//
procedure TGLPolygon.SetParts(const val : TPolygonParts);
begin
   if FParts<>val then begin
      FParts:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLPolygon.Assign(Source: TPersistent);
begin
   if Source is TGLPolygon then begin
      FParts:=TGLPolygon(Source).FParts;
   end;
   inherited Assign(Source);
end;

// BuildList
//
procedure TGLPolygon.BuildList(var rci : TRenderContextInfo);
var
   normal : TAffineVector;
   pNorm : PAffineVector;
begin
   if (Nodes.Count>1) then begin
      normal:=Nodes.Normal;
      if VectorIsNull(normal) then
         pNorm:=nil
      else pNorm:=@normal;
      if ppTop in FParts then begin
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, pNorm, 1)
         else Nodes.RenderTesselatedPolygon(True, pNorm, FDivision);
      end;
      // tessellate bottom polygon
      if ppBottom in FParts then begin
         if Assigned(pNorm) then
            NegateVector(normal);
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, pNorm, 1, True)
         else Nodes.RenderTesselatedPolygon(True, pNorm, FDivision, True);
      end;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------


initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLSphere, TGLCube, TGLFrustrum, TGLCylinder, TGLCone, TGLTorus,
                    TGLTeapot, TGLDodecahedron, TGLDisk, TGLPlane, TGLSprite, TGLPoints,
                    TGLDummyCube, TGLLines, TGLAnnulus, TGLArrowLine, TGLPolygon]);

end.
