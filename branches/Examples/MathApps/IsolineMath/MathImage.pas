unit MathImage;
(***********************************************************************
 *  TMathImage 7.0
 *  Based on Renate Schaaf's TMathImage 6.0
 ***********************************************************************)
interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.UITypes,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  //
  WorldDrawing,
  OverlayImage;



type
  {: This type is currently set to double. Change it according to
  your needs in the WorldDrawing unit.
  }
  MathFloat = WorldDrawing.MathFloat;

  PFloatPoint = WorldDrawing.PFloatPoint;
  TFloatpoint = WorldDrawing.TFloatpoint;

  TFloatPointArray = WorldDrawing.TFloatPointArray;

  PD3FloatPoint = WorldDrawing.PD3FloatPoint;
  TD3FloatPoint = WorldDrawing.TD3FloatPoint;

  Td3FloatPointArray = WorldDrawing.Td3FloatPointArray;

  TColorArray = WorldDrawing.TColorArray;

  TFloatarray = WorldDrawing.TFloatarray;

  TNormalKind = WorldDrawing.TNormalKind;

  {: Cracker class to hook into the low level routines of
   TWorldDrawing for speed.
   }
  TCrackerDrawing = class(TWorldDrawing)
  end;

 {: This exception is raised whenever the world settings of TMathImage
   create an EMathError exception, for example if a division by zero occurs, because
   the world-width is zero. In this case the old settings
   are restored and an EMathImageError is raised.
  }

{: <New topic=MAIN@Mathimage Main>
   <B=Mathimage Component, version 6.0(beta)><par><par>
   <B=Author: Renate Schaaf><par>
   renates@xmission.com<par>
   schaaf@math.usu.edu<par> <par>
   For info and upgrades see<par>
   http://www.xmission.com/~renates/delphi.html<par><par>
   <B=Component packages:> Math_MathImage, Math_Overlay<par>
   <B=Component source files:> MathImge.pas, Overlay.pas, WorldDrawing.pas<par>
   <B=Delphi versions:> 5. 4 should work, for 1-3 see version 5.0.
   <par>
   <B=License:> The use of the component is free for educational, academic
   or noncommercial applications. For more details read the file MathImge.txt which
   comes with the component.<par> <par>
   <See=Overview><par>
   <See=Thanks>
}

{: <New topic=Overview@Overview>
   <B=Overview>

   <See class=TMathImage> is a component which helps displaying mathematical graphics
   objects in 2 or 3 dimensions. It used to be derived from TImage (hence the name),
   but is now a descendant of TGraphicControl, for better flexibility and better control
   of drawing speed. The component implements many TCanvas-drawing routines under similar
   names, but you can pass the coordinates as float types (world coordinates). Below is a
   selection of drawing routines:
   <Par>
   <LI=<See method=TMathImage@DrawLineTo> >     <LI=<See method=TMathImage@MoveToPoint> >
   <LI=<See method=TMathImage@DrawLine> >       <LI=<See method=TMathImage@DrawPoint> >
   <LI=<See method=TMathImage@DrawPolyline> >   <LI=<See method=TMathImage@d3DrawLineTo> >
   <LI=<See method=TMathImage@d3MoveToPoint>>   <LI=<See method=TMathImage@d3DrawLine> >
   <LI=<See method=TMathImage@d3PolyLine> >     <LI=<See method=TMathImage@d3DrawSurface> >
   <LI=<See method=TMathImage@d3DrawLitSurface>><LI=<See method=TMathImage@DrawLevelCurves> >
   <LI=<See method=TMathImage@DrawFilledLevelCurves> >
   <Par>
   To set the world-to-pixel scalings, use the following methodsat run time:
   <Par>
   <LI=<See method=TMathImage@SetWorld> >   <LI=<See method=TMathImage@d3SetWorld> >
   <Par>
   The world bounds and more world settings can also be set using the following properties:
   <Par>
   <LI=<See property=TMathImage@d2WorldX1> >     <LI=<See property=TMathImage@d2WorldXW> >
   <LI=<See property=TMathImage@d2WorldY1> >     <LI=<See property=TMathImage@d2WorldYW> >
   <LI=<See property=TMathImage@d3WorldX1> >     <LI=<See property=TMathImage@d3WorldXW> >
   <LI=<See property=TMathImage@d3WorldY1> >     <LI=<See property=TMathImage@d3WorldYW> >
   <LI=<See property=TMathImage@d3WorldZ1> >     <LI=<See property=TMathImage@d3WorldZW> >
   <LI=<See property=TMathImage@d3ViewDist>>     <LI=<See property=TMathImage@d3ViewAngle> >
   <LI=<See property=TMathImage@d3ZRotation> >   <LI=<See property=TMathImage@d3YRotation> >
   <LI=<See property=TMathImage@d3AspectRatio> >
   <LI=<See property=TMathImage@d3XScale> >      <LI=<See property=TMathImage@d3YScale> >
   <LI=<See property=TMathImage@d3ZScale> >
   <Par>
   For an explanation of how D-3-graphics is implemented, see <see=D3_Graphics_Explanation>.
   <Par>
   TMathimage also has a <see property=TMathimage@Canvas> property, which you can use to
   add to the drawing using all (pixel) routines of TCanvas. To translate between world and
   pixel coordinates, use the routines
   <Par>
   <LI=<See method=TMathImage@WindowX> >    <LI=<See method=TMathImage@WindowY> >
   <LI=<See method=TMathImage@WorldX> >     <LI=<See method=TMathImage@WorldY> >
   <LI=<See method=TMathImage@d3Window> >
   <Par>
   For convenient loading, saving, etc., TMathImage has a <see property=TMAthImage@Bitmap>
   property. The bitmap holds the current drawing.
   (TMathimage.Canvas is actually TMathImage.Bitmap.Canvas).
   <Par>
   Use the <see property=TMAthImage@Brush> and <see property=TMathImage@Pen>
   properties of TMathImage to set drawing- and fill- colors and -styles.
   Use the <see property=TMathImage@Font> property to set the font used for
   labelling axes and for TMathImage.Canvas.TextOut.
   <Par>
   Use
   <Par>
   <LI=<see method=TMathImage@Clear> >          <LI=<see method=TMathImage@ClearClipped> >
   to erase drawings.
   <Par> <Par>
   <B=Helper Objects> <Par>
   For drawing polylines in 2-D or 3-D use the following helper classes to store points:
   <Par>
   <LI=<see class=TFloatPointList> >           <LI=<see class=Td3FloatPointList> >
   <LI=<see class=TFloatPointListList> >       <LI=<see class=Td3FloatPointListList> >
   <Par>
   For storing points that define a 3-D surface use the <see class=TSurface> class.
   <Par>
   TMathImage can raise exceptions <see class=EMathImageError> and <see class=ESurfaceError>.
}
  EMathImageError = class(Exception);

{: ESurfaceError is raised whenever a value is being assigned to a nonexisting  <see class=TSurface> grid point, a color is being
   assigned to a nonexisting <see class=TColorSurface> gridpoint, or the corresponding are tried to be accessed.
}
  ESurfaceError = class(Exception);

{: Surface Object to be passed to <See Method=TMathImage@D3DrawSurface>. It's a matrix scheme of
  3D-Points (<see type=TSurfPoint>) Think of the surface being made up of cells whose
  corner (grid) points sit at location (i,j),)(i+1,j),(i,j+1),(i+1,j+1) in the scheme. Use
  <See method=TSurface@Make> to fill the scheme with 3D-Points of your surface.

  <B=Note:> TSurface Objects need to be created and freed as needed.
}
  TSurface = class(TObject)
  private
    fError: Boolean;
    fxm, fym: Integer;
    fDefaultFillColor, fDefaultWireColor: TColor;
    fFloatsurface: array of Td3FloatPointArray;
    fTriangles: TD3TriangleArray;
    fSurfaceCells: array of TD3SurfaceCell;
    fPrepared: Boolean;
    procedure GetTriangles;
  protected
    function GetWireColor(i, j: Integer): Pointer; virtual;
    function GetFillColor(i, j: Integer): Pointer; virtual;
  public
{: The Error property has been kept for backwards compatability. Its value is
   always false now. Unless you create a surface with xGrid<=0 or yGrid<=0, which would be very unlogical, it will be created OK. Another cause of
   error could be an out of memory because of too many grid points. I leave itto Delphi to trap those errors.
}  property Error: Boolean read fError;

{: Xmesh is the number of surface cells in x- (or i-) direction. There are Xmesh+1 grid points in this direction,
     numbered from i=0 to i=Xmesh.}
    property xMesh: Integer read fxm;

{: Ymesh is the number of surface cells in y- (or j-) direction. There are Ymesh+1 grid points in this direction,
      numbered from j=0 to j=Ymesh.
}   property yMesh: Integer read fym;

{: The Surface has (xgrid+1) by (ygrid+1) grid (matrix) points. grids number from 0 to xgrid etc.. A created surface always has
       to be freed, too.
}   constructor Create(xGrid, yGrid: Integer);

{:Assigns the point (x,y,z) to grid (i,j).}
  procedure Make(i, j: Integer; x, y, z: MathFloat); virtual;

{:Returns the 3-D-point (<See Type=TD3FloatPoint>) at grid (i,j.) }
   function d3Point(i, j: Integer): TD3FloatPoint;

   procedure PrepareIllumination;

{: Frees the memory allocated by the surface object.}
    destructor Destroy; override;
  end;

{: TSurface descendent which can also store different colors. }
  TColorSurface = class(TSurface)
  private
    fColors: array of TColorArray;
  protected
    function GetFillColor(i, j: Integer): Pointer; override;
    function GetWireColor(i, j: Integer): Pointer; override;
  public
    constructor Create(xGrid, yGrid: Integer);
    destructor Destroy; override;
{: Assign the point (x,y,z) to grid location (i,j), and specify the color for this surface part.}
    procedure Make(i, j: Integer; x, y, z: MathFloat; Color: TColor); reintroduce; overload;
    function GetColor(i, j: Integer): TColor;
  end;

{: TSurface descendent which can be used for level color coded surface drawing.}
  TLevelSurface = class(TSurface)
  private
    fLevels: array of MathFloat;
    fColors: array of TColor;
    fNewPoints: array of PD3FloatPoint;
  public
    destructor Destroy; override;
{: Use this to set an array of z-levels at which the color of the surface
should change, together with an array of associated colors. Levels must be
in ascending order. Between Levels[i] and Levels[i+1] the color will be
Colors[i]. Levels higher than the max given level get the max given color, etc.
Levels and Colors must have the same length, or else both will be chopped to the
shortest. TLevelSurfaces can be passed to TMathImage.d3DrawSurface,
TMathImage.d3DrawLitSurface and TMathImage.DrawFilledLevelCurves.
}
    procedure SetLevels(const Levels: array of MathFloat; const Colors: array of TColor);
  end;

  TSurfaceCollection = class
  private
    fSurfaces: array of TSurface;
    fCount, fLength: Integer;
    fCells: array of TD3SurfaceCell;
    fTriangs: array of TD3Triangle;
    fprepared: boolean;
  public
    constructor Create;
    procedure add(const Surface: TSurface; FillColor, WireColor: TColor);
    procedure PrepareIllumination;
    property Count: Integer read fCount;
  end;

  THeightMap = class
  private
    fHeightArray: array of TFloatarray;
    fColors: array of TColorArray;
    fxm, fym: Integer;
  public
    constructor Create(xGrid, yGrid: Integer);
    property xMesh: Integer read fxm;
    property yMesh: Integer read fym;
    procedure Make(i, j: Integer; z: MathFloat; Color: TColor);
  end;

{:FloatPointList object to be passed to <See Method=TMathImage@DrawPolyline>.
   The intended use is to fill a FloatPointList object sequentially with
   pairs of number (float points) (see <See Method=TFloatPointList@Add>), then pass it to DrawPolyline
   to connect all points in the list sequentially by lines.
  <B=Note:> You have to create and free these lists as needed. }
  TFloatPointlist = class(TObject)
  private
    fCount, fLength: Integer;
    fFirstpoint, fCurrentpoint: PFloatPoint;
    fFloatArray: TFloatPointArray;
  public
{:Use to read the first point of the list. The type of FirstPoint is a pointer to <See Type=TFloatPoint>.}
    property FirstPoint: PFloatPoint read fFirstpoint;

{:Use to read the current(i.e. last) point. The type of CurrentPoint is a pointer to <See Type=TFloatPoint>.}
    property CurrentPoint: PFloatPoint read fCurrentpoint;

{:Use to read the number of points currently in the list. Note: not necessarily the same as length(Points)!}
    property Count: longint read fCount;

{:Use to access the points in the list as a dynamic array. Note that length(points)
might be larger than the number of meaningful points stored in the list. So always
use the count property as iteration delimiters.}
    property Points: TFloatPointArray read fFloatArray;


{:Increments the pointer p to point to the next item in the list.
Needed for somewhat of a backwards compatability. Instead of previously saying
p:=p.next you can now call p:=MyFloatPointList.NextPoint(p). The result is nil for the last point in
the list. Only use in connection with Firstpoint. }
    function NextPoint(p: PFloatPoint): PFloatPoint;
{: Create a list before you use it. Call MyList.free to deallocate its memory after use.}
    constructor Create;

{: Destroy a list after use. Each created list needs to be freed, too.}
    destructor Destroy; override;

{:Add a point (x,y) to the end of the list. }
    procedure add(x, y: MathFloat);

{:Copy AFloatPointList to this instance. AFloatPointList must have been
    created and is still around as another instance after assign. }
    procedure assign(AFloatPointList: TFloatPointlist); virtual;

  end;

  TGraphlist = array of TFloatPointlist;

{:FloatPointListList object: list of FloatPointLists. Intended to be passed to
  <See Method=TMathImage@DrawPolyPolyLine>. PolyPolyLines can have breaks in them.
  The points in each list form a Polyline. Use <See method=TFloatPointListList@Add>
  to add a new list (break). Use <See method=TFloatPointListList@AddToCurrent> to
  add a point at the end of the current (last) list.
  <B=Note:> You have to create and free this list class as needed;  }

  TFloatPointListList = class(TObject)
  private
    fCount, FTotalCount: longint;
    fgraphlist: TGraphlist;
    fFirstlist, fCurrentlist: TFloatPointlist;
  public
{:Returns the first point list}
    property FirstList: TFloatPointlist read fFirstlist;
{:Returns the current (i.e. last) list. }
    property CurrentList: TFloatPointlist read fCurrentlist;

{: You can use the Lists property to access the points in the listlist as
a dynamic array of TFloatPointArray ("double" dynamic array). }
    property Lists: TGraphlist read fgraphlist;
{:Returns the number of <B=lists>. }
    property Count: longint read fCount;
{:Returns the total number of points in all lists. }
    property TotalCount: longint read FTotalCount;
{: Create the list before you use it. Sublists are created
    automatically when you call <see property=TFloatPointListList@Add>.
    You need to call ..listlist.free when done. }
    constructor Create;
{: Deallocates memory for the listlist object. Called by free.
    Memory for all sub lists is automatically freed.}
    destructor Destroy; override;
{:Start a new point list. }
    procedure add;
{:Add the point (x,y) to the current (last) list.}
    procedure AddToCurrent(x, y: MathFloat);

  end;

{:D3FloatPointList object to be passed to <See Method=TMathImage@D3Polyline>.
   The intended use is to fill a D3FloatPointList object sequentially with
   triplets of numbers (D3-float points) (see <See Method=TD3FloatPointList@Add>), then pass it to D3DrawPolyline
   to   connect all points in the list sequentially by lines.

  <B=Note:> You have to create and free these lists as needed.}
  TD3FloatPointList = class(TObject)
  private
    fCount, fLength: Integer;
    fFirstpoint, fCurrentpoint: PD3FloatPoint;
    fFloatArray: Td3FloatPointArray;
    fLineSegmentArray: Td3LineSegmentArray;
    fPrepared: Boolean;
    fNormalKind: TNormalKind;
    procedure SetNormalKind(Value: TNormalKind);
  public

{:Use to read the first point of the list. The type of FirstPoint is a
    pointer to <See Type=TD3FloatPoint>. }
    property FirstPoint: PD3FloatPoint read fFirstpoint;

    {:Use to read the current(i.e. last) point. The type of CurrentPoint is a
    pointer to <See Type=TD3FloatPoint>.}
    property CurrentPoint: PD3FloatPoint read fCurrentpoint;
{:The count of points currently in the list. Note: not necessarily the same as
length(Points)! }
    property Count: longint read fCount;

{:  Use the Points property to access the list of 3d points as a dynamic array. Its length might
be longer than the number of meaningful points, so always use the count property of
the list to delimit iterations.}
    property Points: Td3FloatPointArray read fFloatArray;

    property NormalKind: TNormalKind read fNormalKind write SetNormalKind;
    constructor Create;
    destructor Destroy; override;
{:Add a point (x,y) to the end of the list. }
    procedure add(x, y, z: MathFloat);
{:Copy AFloatPointList to this instance. If AFloatPointList isn't nil,
 it is still around as another instance after assign. }

{:Increments the pointer p to point to the next item in the list.
Needed for somewhat of a backwards compatability. Instead of previously saying
p:=p.next you can now call p:=Myd3FloatPointList.NextPoint(p). The result is nil for the last point in
the list. Only use in connection with Firstpoint. You've got to know what
you are doing here. }
    function NextPoint(p: PD3FloatPoint): PD3FloatPoint;
    procedure assign(AFloatPointList: TD3FloatPointList); virtual;
    procedure PrepareIllumination;
  end;

  TD3GraphList = array of TD3FloatPointList;


{:D3FloatPointListList object: list of D3FloatPointLists. Intended to be passed to
  <See Method=TMathImage@D3PolyPolyLine>. PolyPolyLines can have breaks in them.
  The points in each list form a Polyline. Use <See method=TD3FloatPointListList@Add>
  to add a new list (break). Use <See method=TD3FloatPointListList@AddToCurrent> to
  add a point at the end of the current (last) list.

  <B=Note:> You have to create and free this list class as needed;

  <B=Note:> The intended usage of the object is to sequentially fill it with the 1st, 2nd etc. list of points to be drawn.
    It you want more functionality (addressing a particular itemin the list, moving back & forth) (and slower performance) use a
    TList descendent instead, and fill a FloatpointListList with the points
    before drawing. Or use <See method=TMathImage@D3Window> on the points and do <See property=TMathImage@Canvas>.polyline's.}

  TD3FloatPointListList = class(TObject)
  private
    fCount, FTotalCount: longint;
    fgraphlist: TD3GraphList;
    fFirstlist, fCurrentlist: TD3FloatPointList;
    fNormalKind: TNormalKind;
    procedure SetNormalKind(Value: TNormalKind);
  public
    {:Returns the first point list   }
    property FirstList: TD3FloatPointList read fFirstlist;

    {:Returns the current (i.e. last) list.    }
    property CurrentList: TD3FloatPointList read fCurrentlist;

    {:Returns the number of <B=lists>.     }
    property Count: longint read fCount;

    {:Returns the total number of points in all lists.    }
    property TotalCount: longint read FTotalCount;

    property GraphList: TD3GraphList read fgraphlist;

    property NormalKind: TNormalKind read fNormalKind write SetNormalKind;

    constructor Create;
    destructor Destroy; override;
    {:Start a new point list.     }
    procedure add;
    {:Add the point (x,y,z) to the current (last) list.     }
    procedure AddToCurrent(x, y, z: MathFloat);

  end;

  {: TMathImage is the main object in the MathImge unit. It is a TGraphicControl
   descendant and can as such be installed in the Delphi component palette. For
   general info see the <see=main> help topic. The component contains
   properties, methods and events for doing graphics in world coordinates. 2-D and
   3-D graphics are supported. Browse through the properties, methods and events to
   get an idea, or see the <see=overview>. Best way to learn is to look at the included demos.   }

  TMathImage = class(TOverlayImage)
  private
    maxth, maxxtw, maxytw: Integer;
    fClipRect: TRect;
    fVersion: string;
    Rotating, Zooming, Moving, FRecordMetafile: Boolean;
    FOnRotating, FOnEndRotate, FOnMoving, FOnEndMove,
      FOnZooming, FOnEndZoom: TNotifyEvent;
    fWorldDrawing: TCrackerDrawing;

    procedure SetVersion(x: string);
    procedure d3ResetWorld;
    procedure SetAxis(A: Boolean);
    procedure Setx1d2(x: MathFloat);             procedure Setxwd2(x: MathFloat);
    procedure Sety1d2(x: MathFloat);             procedure Setywd2(x: MathFloat);
    procedure Setx1d3(x: MathFloat);             procedure Sety1d3(x: MathFloat);
    procedure Setxwd3(x: MathFloat);             procedure Setywd3(x: MathFloat);
    procedure Setz1d3(x: MathFloat);             procedure Setzwd3(x: MathFloat);
    procedure Setvd(x: MathFloat);               procedure Setzrd3(x: MathFloat);
    procedure Setyrd3(x: MathFloat);             procedure Setalpha(x: MathFloat);
    procedure Setard3(x: Boolean);               procedure SetXscale(x: MathFloat);
    procedure SetYscale(x: MathFloat);           procedure SetZscale(x: MathFloat);
    procedure SetClipRect(Value: TRect);         procedure SetRecordMetafile(x: Boolean);
    function Getd2Worldx2: MathFloat;            function Getd2Worldy2: MathFloat;
    function Getd3Worldx2: MathFloat;            function Getd3Worldy2: MathFloat;
    function Getd3Worldz2: MathFloat;            function GetAxis: Boolean;
    function Getx1d2: MathFloat;                 function Getxwd2: MathFloat;
    function Gety1d2: MathFloat;                 function Getywd2: MathFloat;
    function Getx1d3: MathFloat;                 function Gety1d3: MathFloat;
    function Getxwd3: MathFloat;                 function Getywd3: MathFloat;
    function Getz1d3: MathFloat;                 function Getzwd3: MathFloat;
    function Getvd: MathFloat;                   function Getzrd3: MathFloat;
    function Getyrd3: MathFloat;                 function Getalpha: MathFloat;
    function Getard3: Boolean;                   function GetXscale: MathFloat;
    function GetYscale: MathFloat;               function GetZscale: MathFloat;
    { Private declarations, never mind }
  protected

    {: Extra stuff to do when bounds of a TMathimage change.    }
    procedure SizeChanged; override;

    { Protected declarations }
{---------------------*********************************--------------------------}
{                               THE IMPORTANT STUFF                                                 }
{---------------------*********************************--------------------------}
  public

    {: If true, space is reserved in a 2-D drawing to include axes. You need to call
    <see method=TMathImage@DrawAxes> in order to actually draw any. A call to DrawAxes
    automatically makes D2Axes true. So use this property if for some reason you want
    to draw the axes after the curves.     }
    property d2Axes: Boolean read GetAxis write SetAxis;

    {: Upper bounds for D2world rectangle and D3world box.
    Those used to be published, but were causing unnecessary exceptions.
    They are kept as public and read only for backwards compatability.
    For the new published properties see <see property=TMathImage@D2WorldX1>,
    <See Property=TMathImage@D2WorldXW>, etc.     }
    property d2WorldX2: MathFloat read Getd2Worldx2;
    property d2WorldY2: MathFloat read Getd2Worldy2;
    property d3Worldx2: MathFloat read Getd3Worldx2;
    property d3Worldy2: MathFloat read Getd3Worldy2;
    property d3Worldz2: MathFloat read Getd3Worldz2;

    {: Intended to be able to set the current clip rectangle. Not really implemented so far,
                    except for clipping the region within axes. }
    property ClipRect: TRect read fClipRect write SetClipRect;
  
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: Erases the current drawing and sets the background to the current
    <see property=TOverlayImage@brush> color.     }
    procedure Clear; //reintroduce; overload;

    {: Erases the area inside of the axes in a 2-D drawing.     }
    procedure ClearClipped;

    {The following are the methods for 2-d graphing}

    {: Set the world range of a 2-D drawing in one step
    (to be preferred at runtime). x1,y1 are the lower bounds
    of the world rectangle, x2,y2 are the <B=upper bounds>.
    If x2<<=x1 or y2<<=y1, an exception is raised.
     <par>
     Compare to published properties <see property=TMathImage@D2Worldx1>,
     <see property=TMathImage@D2Worldxw>, etc., where D2WorldxW is
     the <B=x-width> of the world rectangle.
     }
    procedure SetWorld(x1, y1, x2, y2: MathFloat);


    {: Short(?) for <See property=TOverlayImage@pen>.color:=color.
    Has been kept for compatability.
    }
    procedure SetColor(Color: TColor);

    {: Short(?) for result:=<See property=TOverlayImage@pen>.color.
    Has been kept for compatability.
    }
    function GetColor: TColor;


    {: In 2-D, translates world coordinate x to pixel-x.
    Main use is internally.
    }
    function Windowx(x: MathFloat): longint;

    {: In 2-D, translates world coordinate y to pixel-y. Main use
    is internally.
    }
    function Windowy(y: MathFloat): longint;

    procedure WorldToScreen(const x, y: MathFloat; var xs, Ys: Integer);

    {: In 2-D, translates pixel coordinate xs to world-x. Useful for reading the world
    coordinates of a clicked point, or a point the mouse is over.
    }
    function WorldX(xs: longint): MathFloat;

    {: In 2-D, translates pixel coordinate ys to world-y. Useful for reading the world
    coordinates of a clicked point, or a point the mouse is over.
    }
    function WorldY(Ys: longint): MathFloat;

    {: Length of vector (x,y).
    }
    function Norm(x, y: MathFloat): MathFloat;

    {: Puts a pixel with world coordinates (x,y) on the screen. Color
    is the currently selected <see property=TOverlayImage@Pen> color.
    }
    procedure DrawPoint(x, y: MathFloat);

    {: Moves the graphics cursor to the point with D2-world coordinates (x,y).
    }
    procedure MoveToPoint(x, y: MathFloat);

    {: Draws a line from (x1,y1) to (x2,y2) in D2-world coordinates.
    Both end pixels are drawn, in contrast to a <see method=TMathImage@MovetoPoint>-
    <see method=TMAthImage@DrawLineto> combination.
    }
    procedure DrawLine(x1, y1, x2, y2: MathFloat);

    {: Draws a line from the current graphics cursor position
    (see <see method=TMathImage@MovetoPoint>) to
   point (x,y) in D2-world coordinates. DrawLineto never draws
   the endpixel (Win-default).
   }
    procedure DrawLineTo(x, y: MathFloat);

    {: Draws an ellipse in the D2-world rectangle between (x1,y1) (lower left)
     and (x2,y2) (upper right) and fills it with the current brush.
     }
    procedure DrawEllipse(x1, y1, x2, y2: MathFloat);

    procedure DrawCircle(xCenter, yCenter: MathFloat; PixRadius: Integer);

    {: Draws a D2-world rectangle between (x1,y1) (lower left)
     and (x2,y2) (upper right) and fills it with the current brush.
     }
    procedure DrawRectangle(x1, y1, x2, y2: MathFloat);


    {: Puts axes at the left and bottom boundary of the drawing. Ticks and
     labelling of numeric values are done automatically. xlabel, ylabel is
     text that goes to the end of the axes. Zerolines=true draws lines x=0,
     y=0. Axescolor,ZerolinesColor are selfexplaining.
     }
    procedure DrawAxes(xLabel, yLabel: string;
      ZeroLines: Boolean;
      AxesColor, ZeroLinesColor: TColor; Arrows: Boolean = True);

    {: Draws a vector (a,b) at base point(x,y) (D2-world).
    }
    procedure DrawVector(x, y, A, b: MathFloat);

    {: Draws a curve by sequentially connecting the points in FloatPointList.
    Faster than individual lines. See <See type=TFloatPointList>.
    <B=Note:> Win95/98 GDI only accepts up to 16320 points for a polyline.
    }
    procedure DrawPolyline(FloatPointList: TFloatPointlist);

    {: Draws a curve connecting the points in FloatPointList, closes the
    shape and fills it with the current brush. See <See type=TFloatPointList>.
    }
    procedure DrawPolygon(FloatPointList: TFloatPointlist);


    {: Draws all point lists in the ListList as Polylines. Use if you want to draw curves
    with "breaks". See <See type=TFloatPointListList>.
    <B=Note:> Win95/98 GDI only accepts up to 16320 points for a polygon.
    }
    procedure DrawPolyPolyline(FloatPointListList: TFloatPointListList);



   {D3Graphics procedures:}

    {: Sets all D3-world bounds in one step. *1 are the lower bounds, *2 the <B=upper bounds>.
    Lower bounds must be strictly less than upper bounds.
    This method is to be preferred at run time over using the published properties <see Property=TMathImage@D3Worldx1>, etc..
    Notice the difference: Using the published properties, you need to set the <B=width> of the
    world instead of the upper bound.     }
    procedure d3SetWorld(x1, y1, z1, x2, y2, z2: MathFloat);

    {: This procedure translates D3-world-(x,y,z) to pixel-(xs,ys), using the current
    world bounds, view distance, view angle and view point location. Mostly for internal use.    }
    procedure d3Window(x, y, z: MathFloat; var xs, Ys: longint);

{: Returns 3D-world coordinates x,y,z for screen coordinates xs,ys, assuming that (x,y,z)
       lies in the plane through the center of the world pependicular to the viewer's direction.
     Can be used in a limited way to "Click on a point" in the 3D-world. See DataPlot demo part.     }
    procedure PseudoD3World(xs, Ys: longint; var x, y, z: MathFloat);

    {: Moves the graphics cursor to the point with D3-world coordinates (x,y,z).    }
    procedure d3Moveto(x, y, z: MathFloat);

    {: Puts a pixel with D3-world coordinates (x,y,z) on the screen. Color
    is the currently selected <see property=TMathImage@Pen> color.   }
    procedure d3DrawPoint(x, y, z: MathFloat);

    {: Draws a line from (x1,y1,z1) to (x2,y2,z2) in D3-world coordinates.
    Both end pixels are drawn, in contrast to a <see method=TMathImage@D3Moveto>-
    <see method=TMAthImage@D3DrawLineto> combination.
    }
    procedure d3DrawLine(x1, y1, z1, x2, y2, z2: MathFloat);

    {: Draws a line from the current graphics cursor position
    (see <see method=TMathImage@d3Moveto>) to
   point (x,y,z) in D3-world coordinates. DrawLineto never draws
   the endpixel (Win-default).
   }
    procedure d3DrawLineto(x, y, z: MathFloat);


    {: Draws axes at the bondary of the world box
    and puts xlabel,ylabel,zlabel on their ends.
    xticks,yticks,zticks specify the number of ticks on the axes. Each
    can be set to 0.
    xpos,ypos,zpos specifies the position of the axis. These parameters
    can have the values MinMin(=0), MinMax(=1) or MaxMax(=2).
    A position MinMin places the axis at the minimum of both of the
    remaining variables. MinMax places it at the minimum/maximum of
    the other variables (alphabetical order), etc.
    Example: If your D3-World is (-1,-1,-1,1,1,1) then
    D3DrawAxes('x','y','z',4,4,4,MinMin,MaxMin,MinMin) draws axes with
    (about) 4 ticks. The x-axis is displayed along the line y=z=-1,
    the y-axis along x=1,z=-1, and the z-axis along x=y=-1.
    }
    procedure d3DrawAxes(xLabel, yLabel, zLabel: string;
      xTicks, yTicks, zTicks, xPos, yPos, zPos: byte; Arrows: Boolean = True);

    procedure d3DrawBestAxes(xLabel, yLabel, zLabel: string;
      xTicks, yTicks, zTicks: byte; Arrows: Boolean = True);

   {Draws axes centered at (xmin,ymin,zmin)extending to (xmax,ymax,zmax),  without ticks.}
    procedure d3DrawCustomAxes(xmin, ymin, zmin, xmax, ymax, zmax: MathFloat;
                                         xLabel, yLabel, zLabel: string);

  {: Draws the box the current D3-world resides in as a wire frame, with the 3 sides facing
    the viewer left open. Also see <see property=TMAthImage@d3DrawFullWorldBox>.}
    procedure d3DrawWorldbox;

 {: Draws a wire frame box between D3-points(x1,y1,z1) (lower) and(x2,y2,z2) (upper).    }
    procedure d3DrawBox(x1, y1, z1, x2, y2, z2: MathFloat);

 {: Draws the full box the 3D-world resides in as a wire frame. see <see property=TMAthImage@d3DrawWorldBox>.    }
    procedure d3DrawFullWorldBox;

 {: In 3-D, draws lines x=y=0, x=z=0, y=z=0.    }
    procedure d3drawZeroCross;

{: Draws a 3D-curve by sequentially connecting the points in FloatPointList.
           Faster than individual lines. See <See type=TD3FloatPointList>.
    <B=Note:> Win95/98 GDI only accepts up to 16320 points for a polyline.   }
    procedure d3Polyline(FloatPointList: TD3FloatPointList);

    procedure d3LitPolyLine(FloatPointList: TD3FloatPointList; diffuse, focussed, RightIntensity: MathFloat;
      zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);

{Draws all point lists in the ListList as Polylines. Use if you want to draw curves
    with "breaks". See <See type=TD3FloatPointListList>.    }
    procedure d3PolyPolyline(FloatPointListList: TD3FloatPointListList);

    procedure d3LitPolyPolyline(FloatPointListList: TD3FloatPointListList; diffuse, focussed, RightIntensity: MathFloat;
      zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);

 {: Rotates the viewpoint (not the object) to the left in the specified angle increment at a time. Note: The rotation goes
     on until you call <see method=TMathImage@d3StopRotating> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnRotating> fires at each increment. Use it to
     make rotating visible. See the demo project for usage.      }
    procedure d3StartRotatingLeft(Increment: MathFloat);

    {: Rotates the viewpoint (not the object) to the right in the specified angle increment at a time. Note: The rotation goes
     on until you call <see method=TMathImage@d3StopRotating> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnRotating> fires at each increment. Use it to
     make rotating visible. See the demo project for usage.

     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.     }
    procedure d3StartRotatingRight(Increment: MathFloat);

    {: Rotates the viewpoint (not the object) up in the specified angle increment at a time. Note: The rotation goes
     on until you call <see method=TMathImage@d3StopRotating> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnRotating> fires at each increment. Use it to
     make rotating visible. See the demo project for usage.
     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.    }
    procedure d3StartRotatingUp(Increment: MathFloat);

 {: Rotates the viewpoint (not the object) down in the specified angle increment at a time. Note: The rotation goes
     on until you call <see method=TMathImage@d3StopRotating> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnRotating> fires at each increment. Use it to
     make rotating visible. See the demo project for usage.

     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.      }
    procedure d3StartRotatingDown(Increment: MathFloat);

{: This method must be called to stop any rotation started by the methods
    <see method=TMathImage@d3StartRotatingLeft>, <see methode=TMAthImage@d3StartRotatingRight>,
    <see method=TMathImage@d3StartRotatingUp> and <see method=TMathimage@d3StartRotatingDown>.
    The Event <see property=TMathImage@OnRotateStop> fires, so you can redraw your picture as necessary.
    See demo project for usage.      }
    procedure d3StopRotating;

{: Decreases the viewdistance by by increment*<see property=TMathImage@d3ViewDist> at a time.
    (Relative decrease makes more sense).
     Note: The moving goes  on until you call <see method=TMathImage@d3StopMoving> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnMoving> fires at each increment. Use it to
     make moving visible. See the demo project for usage.

     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.      }
    procedure d3StartMovingIn(Increment: MathFloat);

{: Increases the viewdistance by by increment*<see property=TMathImage@d3ViewDist> at a time.
    (Relative increase makes more sense).
     Note: The moving goeson until you call <see method=TMathImage@d3StopMoving> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnMoving> fires at each increment. Use it to
     make moving visible. See the demo project for usage.

     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.    }
    procedure d3StartMovingOut(Increment: MathFloat);

{: This method must be called to stop any moving started by the methods
    <see method=TMathImage@d3StartMovingIn> or <see methode=TMAthImage@d3StartMovingOut>.
    The event <see property=TMathImage@OnMoveStop> fires, so you can redraw your picture as necessary.
    See demo project for usage.      }
    procedure d3StopMoving;

{: Decreases the view angle by by increment*<see property=TMathImage@d3ViewAngle> at a time.
    (Relative decrease makes more sense).
     Note: The zooming goes on until you call <see method=TMathImage@d3StopZooming> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnZooming> fires at each increment. Use it to
     make zooming visible. See the demo project for usage.       }
    procedure d3StartZoomingIn(Increment: MathFloat);

{: Increases the view angle by by increment*<see property=TMathImage@d3ViewAngle> at a time.
    (Relative increase makes more sense).
     Note: The zooming goes  on until you call <see method=TMathImage@d3StopZooming> in some event handler (like OnMouseUp).
     The event <see property=TMathImage@OnZooming> fires at each increment. Use it to
     make zooming visible. See the demo project for usage.

     <B=Caution:> This method calls Application.ProcessMessages. You need to make sure that this
     does not lead to unwanted user input while the method executes.      }
    procedure d3StartZoomingOut(Increment: MathFloat);

{: This method must be called to stop any zooming started by the methods
    <see method=TMathImage@d3StartZoomingIn> or <see methode=TMAthImage@d3StartZoomingOut>.
    The event <see property=TMathImage@OnZoomStop> fires, so you can redraw your picture as necessary.
    See demo project for usage.   }
    procedure d3StopZooming;

{SURFACE ROUTINES}

{: Draw a surface (a 2-dimensional curved object, like a graph or a sphere)
    in the 3-D-world. Surface (see <see type=TSurface>)  must have been created and
    filled with the world coordinates of the gridpoints.
    Fill=false gives a wire frame in the current pen color,
    Fill=true displays it filled , invisible parts hidden.
    The fill coloring depends on the type of surface you
    pass. Just a plain TSurface gets filled with the current
    brush color. A TColorSurface displays its cells with the
    colors you have spedified. A TLevelSurface does not work any different
    from a TSurface here. To see those nicely, use d3DrawLitSurface.
    NoUpdate=true/false: Has no effect presently, as the implementation
    of this feature was too unsafe.
    See demo project for usage. It's easiest to first understand how the
    graph is drawn. The knot surface is only there to show off the      possibilities.    }
    procedure d3DrawSurface(Surface: TSurface; fill, NoUpdate: Boolean);

    {: Analogous to the <see method=TMathimage@d3DrawSurface> procedure,
    but lighting is used to display the
    filled surface, and no wireframe is drawn. There are 2 light sources:
    Diffuse light, which lights up the whole surface evenly, and focussed
    light which is a beam having its source at the viewpoint (thats easiest
    and enough to see the surface). Coloring depends on the Surface type
    you pass. A plain TSurface gets the basecolor of the current brush color.
    A TColorSurface or a TLevelSurface get drawn according to the colors
    you have specified for them.
    Diffuse, focussed set the strength of the light
    sources. A total strength 1 displays the exact brush color on a
    maximally lit surface part (one that's perpendicular to the view direction).
    }
    procedure d3DrawLitSurface
      (Surface: TSurface; diffuse, focussed: MathFloat; NoUpdate: Boolean = True);

    procedure d3DrawSurfaceCollection(Surfaces: TSurfaceCollection;
      fill: Boolean);

    procedure d3DrawLitSurfaceCollection(Surfaces: TSurfaceCollection; ambient, focussed: MathFloat);  

    {: Draw blocks within the grid which have height given by the heightmap
    data. Doesn't really work yet.
    }
    procedure d3DrawHeightCubes(HeightMap: THeightMap);

    {: Draw blocks within the grid which have height given by the heightmap
    data. Doesn't really work yet.
    }
    procedure d3DrawLitHeightCubes(HeightMap: THeightMap; diffuse, focussed: MathFloat);

    {: Draws a cube with lower  edge (x1,y1,z1) und upper edge (x2,y2,z2). It is
    necessary that x1<<x2, y1<<y2, z1<<z2. Fill=true fills the sides with the current
     brush color. Fill=false is the same as <see method=TMathImage@d3DrawBox>.      }
    procedure d3DrawCube(x1, y1, z1, x2, y2, z2: MathFloat; fill: Boolean);



    {: Draws all level curves (contours) of the given surface <see class=TSurface>
    for the z-level passed in level. Note that you can pass a TSurface or any descendent.    }
    procedure DrawLevelCurves(Surface: TSurface; Level: MathFloat);

    {: Fills points (x,y) whose z-level is between levels[k] and levels[k+1] with
    color colors[k].       }
    procedure DrawFilledLevelCurves(LevelSurface: TLevelSurface);

     

  published
    property Align;
    property Hint;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Visible;

{: Fake property to display the version of the component in the object inspector. }
    property Version: string read fVersion write SetVersion;

 {: When set true, this property causes a metafile to be recorded in the background,
    on which the same drawing operations are performed as in the visible component. Recording a metafile
    slows down drawing a little. Metafiles have advantages over bitmaps in that they scale better, and
    give better printouts. To further enhance the quality, metafiles are being written at twice the resolution
    of the visible drawing. This option is not available under Delphi 1.     }
    property RecordMetafile: Boolean read FRecordMetafile write SetRecordMetafile;

{: The properties D2WorldX1, D2WorldXW, D2WorldY1, D2WorldYW set the
    boundary for the 2-d-drawing world. Analogous to the top, left, width, height properties of a control, you set the left
    boundary of the world with D2WorldX1 and the width of the x-range with
    D2WorldXW etc...     }
    property d2WorldX1: MathFloat read Getx1d2 write Setx1d2;
    property d2WorldXW: MathFloat read Getxwd2 write Setxwd2;
    property d2WorldY1: MathFloat read Gety1d2 write Sety1d2;
    property d2WorldYW: MathFloat read Getywd2 write Setywd2;

    {: <New topic=D3_Graphics_Explanation@D3 Graphics Explanation>
    <B=Explanation of the 3D-graphics process:>

     When graphed,the world box is scaled so its longest edge has length 2, and
     the other edges have lengthes according to the true aspect ratio of
     the bounds you specify. If you set the property <see property=TMathImage@D3AspectRatio>
     to false, the edges have all the same length 2. The box is then projected onto the
     picture according to the settings of <see property=TMathimage@D3ViewDist>,
     <see property=TMathImage@D3ViewAngle>, <see property=TMathImage@D3Zrotation>,
     <see property=TMathImage@D3Yrotation> as follows:

     Everything is projected from the viewer location onto
     the plane through the center of the box which is perpendicular to the
     viewer direction. The part of the plane which you see, is what the
     view angle can sweep out from the view distance. The viewpoint moves on
     a spherical grid around the center of the world box, with the north and
     south poles of the sphere along the z-axis. The viewer always looks
     at the center of the box, and can't tilt her head.., enough for my math applications.     }

{: D3WorldX1, D3WorldY1, D3WorldZ1 and D3WorldXW, D3WorldYW, D3WorldZW set
     the boundaries for the 3-d-drawing world. ...X1 etc. is the
    lower bound, ...XW etc. is the range <B=width>. It's analogous to setting
    the left, top, width, height properties of a control.
    See <See=D3_Graphics_Explanation>     }
    property d3WorldX1: MathFloat read Getx1d3 write Setx1d3;
    property d3WorldXW: MathFloat read Getxwd3 write Setxwd3;
    property d3WorldY1: MathFloat read Gety1d3 write Sety1d3;
    property d3WorldYW: MathFloat read Getywd3 write Setywd3;
    property d3WorldZ1: MathFloat read Getz1d3 write Setz1d3;
    property d3WorldZW: MathFloat read Getzwd3 write Setzwd3;

    {: If D3AspectRatio is true, these are scale factors for the D3-world display.  }
    property d3Xscale: MathFloat read GetXscale write SetXscale;
    property d3Yscale: MathFloat read GetYscale write SetYscale;
    property d3Zscale: MathFloat read GetZscale write SetZscale;

{: Angle of viewpoint with the x-axis. ("How much it's rotated
     about the z-axis", I know it's a bad name, but can't change it now.))    }
    property d3Zrotation: MathFloat read Getzrd3 write Setzrd3;
{: Angle of viewpoint with the z-axis. ("How much the viewpoint is
     rotated about the y-axis". Bad name, sorry.)       }
    property d3Yrotation: MathFloat read Getyrd3 write Setyrd3;
{: Uniformly scaled distance of the viewpoint to the center of the d3-world.
     See <see=D3_Graphics_Explanation>      }
    property d3ViewDist: MathFloat read Getvd write Setvd;

{: Opening angle of the lens of the viewpoint. Large D3ViewAngle combined with
     small <see property=TMathImage@D3ViewDist> give a fish eye effect. The opposite gives almost no perspective
     effect at all.      }
    property d3ViewAngle: MathFloat read Getalpha write Setalpha;

{: When true (default) the true aspect ratio of the data axes
    is used for the worldbox (modulo scaling factors). Otherwise,
    the box is a perfect cube.     }
    property d3AspectRatio: Boolean read Getard3 write Setard3;

    {: Events}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF Ver120}
    property OnEndDock;
{$ENDIF}
{$IFDEF Ver130}
    property OnEndDock;
{$ENDIF}
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    {: Event which fires at each increment of the angle in
     <see method=TMathImage@D3StartRotatingLeft>, etc.. Use
     it to update you drawing or part of it, to make rotation
     visible.

     <B=Note:> The event is not called when you just
     alter values of <see property=TMathImage@D3ZRotation>, etc.        }
    property OnRotating: TNotifyEvent read FOnRotating write FOnRotating;

    {: Event which fires in <see method=TMAthImage@D3StopRotating>. Use it
    to redraw everything after the rotation is complete.     }
    property OnRotateStop: TNotifyEvent read FOnEndRotate write FOnEndRotate;

    {: Event which fires at each increment in <see method=TMathImage@D3StartMovingIn>
    and  -Out. Use it to update your drawing, or part of it, to make moving
    visible.

    <B=Note:> The event does not fire
    when you justchange <see property=TMathImage@D3Viewdist>.     }
    property OnMoving: TNotifyEvent read FOnMoving write FOnMoving;

    {: Event which fires in <see method=TMAthImage@D3StopMoving>. Use it
    to redraw everything after the move in/out is complete.      }
    property OnMoveStop: TNotifyEvent read FOnEndMove write FOnEndMove;

    {: Event which fires at each increment in <see method=TMathImage@D3StartZoomingIn>
    and  -Out. Use it to update your drawing, or part of it, to make zooming
    visible.

    <B=Note:> The event does not fire
    when you justchange <see property=TMathImage@D3ViewAngle>.        }
    property OnZooming: TNotifyEvent read FOnZooming write FOnZooming;

    {: Event which fires in <see method=TMAthImage@D3StopZooming>. Use it
    to redraw everything after the zoom in/out is complete.      }


     {: <New topic=Thanks@Thanks>
     <B=Thanks:><par><par>
     Team-B at the Compuserve Delphi Forum, and later at the Borland News Groups,
     for donating part of their free time to giving incredibly accurate and knowledgable help
     to all of us Delphi users. I am particularly indepted for critical pointers to (in no particular order)
     Steve Schafer, Kurt Bartholomess, Ralph Friedman, Peter Below, Rick Rogers.
     <par>
     Also thanks for innumerable tips from other fellow users. Very special thanks go to Earl F. Glynn,
     Sergey Prilutski, Robert Rossmair, KH. Brenner and Rene Tschaggeler for graphics specific pointers.
     <par>
     To Atanas Stoyanov for making his MemProof program available for free. It helped to find memory leaks in the component.
     <par>
     For GpProfile (Primoz Gabrijelcic/Open Source) This profiler helped
     to speed up things.
     <par>
     To Robert Lee for floating point specific speed improvement.
     <par>
     To Piero Valagussa for his free help creator, which translated the commented component interface into
     a component help file.
     <par>
     To Egbert van Nes for his great free source formatter DelForExp. Having been very source code sloppy,
     it improved things a lot, I think.
     <par>
     last but most important
     <par>
     To all <B=Component Users> who pointed out flaws and asked for new features.
     }
    property OnZoomStop: TNotifyEvent read FOnEndZoom write FOnEndZoom;
    {analogous}

      

  end;

const
  {:constants for D3-axes-positions     }
  MinMin = 0; MinMax = 1; MaxMin = 2; MaxMax = 3;


procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('MathStuff', [TMathImage]);
end;


{TSurface}


procedure TSurface.GetTriangles;
var i, j, Current: Integer;
begin
  SetLength(fTriangles, 2 * fxm * fym);
  Current := 0;
  for i := 0 to fxm - 1 do
    for j := 0 to fym - 1 do
    begin
      if not (odd(i) or odd(j)) or (odd(i) and odd(j)) then
      begin
        with fTriangles[Current] do
        begin
          p := @fFloatsurface[i][j];
          q := @fFloatsurface[i + 1][j];
          r := @fFloatsurface[i][j + 1];
          FillColor := GetFillColor(i, j);
          WireColor := GetWireColor(i, j);
        end;
        inc(Current);
        with fTriangles[Current] do
        begin
          p := @fFloatsurface[i + 1][j + 1];
          q := @fFloatsurface[i + 1][j];
          r := @fFloatsurface[i][j + 1];
          FillColor := GetFillColor(i, j);
          WireColor := GetWireColor(i, j);
        end;
        inc(Current);
      end
      else
      begin
        with fTriangles[Current] do
        begin
          p := @fFloatsurface[i][j];
          q := @fFloatsurface[i][j + 1];
          r := @fFloatsurface[i + 1][j + 1];
          FillColor := GetFillColor(i, j);
          WireColor := GetWireColor(i, j);
        end;
        inc(Current);
        with fTriangles[Current] do
        begin
          p := @fFloatsurface[i + 1][j];
          q := @fFloatsurface[i + 1][j + 1];
          r := @fFloatsurface[i][j];
          FillColor := GetFillColor(i, j);
          WireColor := GetWireColor(i, j);
        end;
        inc(Current);
      end;
    end;
end;


constructor TSurface.Create(xGrid, yGrid: Integer);
var
  i, j, Current: Integer;
begin
  inherited Create;
  fxm := xGrid; fym := yGrid;
  SetLength(fFloatsurface, xGrid + 1);
  for i := 0 to xGrid do
    SetLength(fFloatsurface[i], yGrid + 1);
  GetTriangles;
  fPrepared := False;
  SetLength(fSurfaceCells, xGrid * yGrid);
  Current := 0;
  for i := 0 to xGrid - 1 do
    for j := 0 to yGrid - 1 do
    begin
      fSurfaceCells[Current].p := @fFloatsurface[i][j];
      fSurfaceCells[Current].q := @fFloatsurface[i + 1][j];
      fSurfaceCells[Current].r := @fFloatsurface[i + 1][j + 1];
      fSurfaceCells[Current].s := @fFloatsurface[i][j + 1];
      fSurfaceCells[Current].FillColor := @fDefaultFillColor;
      fSurfaceCells[Current].WireColor := @fDefaultWireColor;
      inc(Current);
    end;
end;

procedure TSurface.Make(i, j: Integer; x, y, z: MathFloat);
begin
  if (i >= 0) and (i <= fxm) and (j >= 0) and (j <= fym)
    then
  begin
    D3FloatPoint(x, y, z, fFloatsurface[i][j]);
  end else
    raise ESurfaceError.Create('Surface gridpoint does not exist');
  fPrepared := False;
end;

function TSurface.d3Point(i, j: Integer): TD3FloatPoint;
begin
  if (i >= 0) and (i <= fxm) and (j >= 0) and (j <= fym) then
    Result := fFloatsurface[i][j]
  else
  begin
    D3FloatPoint(0, 0, 0, Result);
    raise ESurfaceError.Create('Surface Gridpoint does not exist');
  end;
end;

destructor TSurface.Destroy;
begin
  //if Win32Platform = VER_PLATFORM_WIN32_NT then
    //SetProcessWorkingSetSize(GetCurrentProcess, DWORD(-1), DWORD(-1));
  inherited Destroy;
end;

procedure TSurface.PrepareIllumination;
var i: Integer;
begin
  for i := 0 to High(fTriangles) do
    with fTriangles[i] do
      CrossProduct(p.x - r.x, p.y - r.y, p.z - r.z, q.x - r.x, q.y - r.y, q.z - r.z, n.x, n.y, n.z);
  fPrepared := True;
end;

function TSurface.GetFillColor(i, j: Integer): Pointer;
begin
  Result := @fDefaultFillColor;
end;

function TSurface.GetWireColor(i, j: Integer): Pointer;
begin
  Result := @fDefaultWireColor;
end;

{TFloatPointList}

constructor TFloatPointlist.Create;
begin
  inherited Create;
  SetLength(fFloatArray, 500);
  fLength := 500;
  fFirstpoint := nil;
  fCount := 0;
  fCurrentpoint := nil;
end;

procedure TFloatPointlist.add(x, y: MathFloat);
var p: TFloatpoint;
begin
  inc(fCount);
  if fCount > fLength then
  begin
    inc(fLength, 500);
    SetLength(fFloatArray, fLength);
  end;
  p.x := x; p.y := y;
  fFloatArray[fCount - 1] := p;
  if fFirstpoint = nil then
    fFirstpoint := @fFloatArray[fCount - 1];
  fCurrentpoint := @fFloatArray[fCount - 1];
end;

procedure TFloatPointlist.assign;
var
  i: Integer;
begin
  if AFloatPointList.Count > 0 then
  begin
    fFirstpoint := nil;
    fCount := 0;
    SetLength(fFloatArray, 500);
    for i := 0 to AFloatPointList.fCount - 1 do
      with AFloatPointList.fFloatArray[i] do
        add(x, y);
  end;
end;


destructor TFloatPointlist.Destroy;
begin
  SetLength(fFloatArray, 0);
  fFirstpoint := nil;
  fCurrentpoint := nil;
  fCount := 0; fLength := 0;
  //not really necessary. But the following helps a bit  under Win2K:
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetProcessWorkingSetSize(GetCurrentProcess, DWORD(-1), DWORD(-1));
  inherited Destroy;
end;


function TFloatPointlist.NextPoint(p: PFloatPoint): PFloatPoint;
begin
  if p = CurrentPoint then
    Result := nil
  else
  begin
    Result := p;
    inc(Result);
  end;
end;

{TFloatPointListList}

constructor TFloatPointListList.Create;
begin
  inherited Create;
  fFirstlist := nil;
  fCount := 0;
  FTotalCount := 0;
  fCurrentlist := nil;
end;

procedure TFloatPointListList.add;
var
  p: TFloatPointlist;
begin
  p := TFloatPointlist.Create;
  inc(fCount);
  SetLength(fgraphlist, fCount);
  fgraphlist[fCount - 1] := p;
  if fFirstlist = nil then
  begin
    fFirstlist := p;
    fCurrentlist := p;
  end
  else
  begin
    fCurrentlist := p;
  end;
end;

procedure TFloatPointListList.AddToCurrent(x, y: MathFloat);
begin
  fCurrentlist.add(x, y);
  inc(FTotalCount);
end;

destructor TFloatPointListList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    fgraphlist[i].Free;
  fCount := 0;
  FTotalCount := 0;
  fFirstlist := nil;
  fCurrentlist := nil;
  inherited Destroy;
end;

{TD3FloatPointList}

constructor TD3FloatPointList.Create;
begin
  inherited Create;
  SetLength(fFloatArray, 500);
  fLength := 500;
  fFirstpoint := nil;
  fCount := 0;
  fCurrentpoint := nil;
  fNormalKind := nkPrincipal;
end;

procedure TD3FloatPointList.add(x, y, z: MathFloat);
var p: TD3FloatPoint;
begin
  inc(fCount);
  if fCount > fLength then
  begin
    inc(fLength, 500);
    SetLength(fFloatArray, fLength);
  end;
  p.x := x; p.y := y; p.z := z;
  fFloatArray[fCount - 1] := p;
  if fFirstpoint = nil then
    fFirstpoint := @fFloatArray[fCount - 1];
  fCurrentpoint := @fFloatArray[fCount - 1];
  fPrepared := False;
end;

procedure TD3FloatPointList.assign;
var
  i: Integer;
begin
  if AFloatPointList.Count > 0 then
  begin
    fFirstpoint := nil;
    fCount := 0;
    SetLength(fFloatArray, 500);
    for i := 0 to AFloatPointList.fCount - 1 do
      with AFloatPointList.fFloatArray[i] do
        add(x, y, z);
  end;
end;

destructor TD3FloatPointList.Destroy;
begin
  SetLength(fFloatArray, 0);
  fFirstpoint := nil;
  fCurrentpoint := nil;
  fCount := 0;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetProcessWorkingSetSize(GetCurrentProcess, DWORD(-1), DWORD(-1));
  inherited Destroy;
end;

function TD3FloatPointList.NextPoint(p: PD3FloatPoint): PD3FloatPoint;
begin
  if p = CurrentPoint then   Result := nil
  else
  begin
    Result := p;
    inc(Result);
  end;
end;

procedure TD3FloatPointList.PrepareIllumination;
begin
  GetLineSegments(fFloatArray, fCount, fNormalKind, fLineSegmentArray);
  fPrepared := True;
end;

procedure TD3FloatPointList.SetNormalKind(Value: TNormalKind);
begin
  if fNormalKind <> Value then
  begin
    fNormalKind := Value;
    fPrepared := False;
  end;
end;

{TD3FloatPointListList}

constructor TD3FloatPointListList.Create;
begin
  inherited Create;
  fFirstlist := nil;
  fCount := 0;
  FTotalCount := 0;
  fCurrentlist := nil;
end;

procedure TD3FloatPointListList.add;
var
  p: TD3FloatPointList;
begin
  p := TD3FloatPointList.Create;
  inc(fCount);
  SetLength(fgraphlist, fCount);
  fgraphlist[fCount - 1] := p;
  if fFirstlist = nil then
  begin
    fFirstlist := p;
    fCurrentlist := p;
  end
  else
  begin
    fCurrentlist := p;
  end;
end;

procedure TD3FloatPointListList.AddToCurrent(x, y, z: MathFloat);
begin
  fCurrentlist.add(x, y, z);
  inc(FTotalCount);
end;

destructor TD3FloatPointListList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    fgraphlist[i].Free;
  fCount := 0;
  FTotalCount := 0;
  fFirstlist := nil;
  fCurrentlist := nil;
  inherited Destroy;
end;




procedure TD3FloatPointListList.SetNormalKind(Value: TNormalKind);
var i: Integer;
begin
  fNormalKind := Value;
  for i := 0 to High(fgraphlist) do
    fgraphlist[i].NormalKind := fNormalKind;
end;

{TMathImage}


procedure TMathImage.SetVersion;
begin
end;

procedure TMathImage.SetRecordMetafile;
begin
  FRecordMetafile := x;
  if not x then
    EraseMetafile;
end;



procedure TMathImage.Setx1d2;
begin
  SetWorld(x, Gety1d2, x + Getxwd2, Gety1d2 + Getywd2);
end;

procedure TMathImage.Setxwd2;
begin
  if x > 0 then
    SetWorld(Getx1d2, Gety1d2, Getx1d2 + x, Gety1d2 + Getywd2)
  else
    raise EMathImageError.Create('x-worldwidth must be positive');
end;

procedure TMathImage.Sety1d2;
begin
  SetWorld(Getx1d2, x, Getx1d2 + Getxwd2, x + Getywd2);
end;

procedure TMathImage.Setywd2;
begin
  if x > 0 then
    SetWorld(Getx1d2, Gety1d2, Getx1d2 + Getxwd2, Gety1d2 + x)
  else raise EMathImageError.Create('y-worldwidth must be positive');
end;

procedure TMathImage.Setx1d3;
begin
  d3SetWorld(x, Gety1d3, Getz1d3, x + Getxwd3, Gety1d3 + Getywd3, Getz1d3 + Getzwd3);
end;

procedure TMathImage.Setxwd3;
begin
  if x > 0 then
    d3SetWorld(Getx1d3, Gety1d3, Getz1d3, Getx1d3 + x, Gety1d3 + Getywd3, Getz1d3 + Getzwd3)
  else raise EMathImageError.Create('x-worldwidth must be positive');
end;

procedure TMathImage.Sety1d3;
begin
  d3SetWorld(Getx1d3, x, Getz1d3, Getx1d3 + Getxwd3, x + Getywd3, Getz1d3 + Getzwd3);
end;

procedure TMathImage.Setywd3;
begin
  if x > 0 then
    d3SetWorld(Getx1d3, Gety1d3, Getz1d3, Getx1d3 + Getxwd3, Gety1d3 + x, Getz1d3 + Getzwd3)
  else raise EMathImageError.Create('y-worldwidth must be positive');
end;

procedure TMathImage.Setz1d3;
begin
  d3SetWorld(Getx1d3, Gety1d3, x, Getx1d3 + Getxwd3, Gety1d3 + Getywd3, x + Getzwd3);
end;

procedure TMathImage.Setzwd3;
begin
  if x > 0 then
    d3SetWorld(Getx1d3, Gety1d3, Getz1d3, Getx1d3 + Getxwd3, Gety1d3 + Getywd3, Getz1d3 + x)
  else raise EMathImageError.Create('z-worldwidth must be positive');
end;

procedure TMathImage.Setvd;
begin
  fWorldDrawing.d3SetViewPoint(x, Getalpha, Getyrd3, Getzrd3);
end;

procedure TMathImage.Setalpha;
begin
  fWorldDrawing.d3SetViewPoint(Getvd, x, Getyrd3, Getzrd3);
end;

procedure TMathImage.Setzrd3;
begin
  fWorldDrawing.d3SetViewPoint(Getvd, Getalpha, Getyrd3, x);
end;

procedure TMathImage.Setyrd3;
begin
  fWorldDrawing.d3SetViewPoint(Getvd, Getalpha, x, Getzrd3);
end;

procedure TMathImage.Setard3;
begin
  fWorldDrawing.d3SetWorld(Getx1d3, Gety1d3, Getz1d3, Getx1d3 + Getxwd3, Gety1d3 + Getywd3, Getz1d3 + Getzwd3, x);
end;

procedure TMathImage.SetXscale;
begin
  fWorldDrawing.d3SetScales(x, GetYscale, GetZscale);
end;

procedure TMathImage.SetYscale;
begin
  fWorldDrawing.d3SetScales(GetXscale, x, GetZscale);
end;

procedure TMathImage.SetZscale;
begin
  fWorldDrawing.d3SetScales(GetXscale, GetYscale, x);
end;


procedure TMathImage.SetClipRect(Value: TRect);
begin
  fClipRect := Value;
  NewClipRegion(Value);
end;

function TMathImage.Getd2Worldx2;
begin
  Result := Getx1d2 + Getxwd2;
end;

function TMathImage.Getd2Worldy2;
begin
  Result := Gety1d2 + Getywd2;
end;

function TMathImage.Getd3Worldx2;
begin
  Result := Getx1d3 + Getxwd3;
end;

function TMathImage.Getd3Worldy2;
begin
  Result := Gety1d3 + Getywd3;
end;

function TMathImage.Getd3Worldz2;
begin
  Result := Getz1d3 + Getzwd3;
end;




constructor TMathImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  fWorldDrawing := TCrackerDrawing.Create;
  maxxtw := 20; maxytw := 20; maxth := 20;
  if AOwner <> nil then
    if (csDesigning in ComponentState) and not (csReading in AOwner.ComponentState) then
    begin
      fWorldDrawing.SetWorld(Canvas, -1, -1, 1, 1);
      fWorldDrawing.Setd2Axes(Canvas, False);
      fWorldDrawing.d3SetWorld(-1, -1, -1, 1, 1, 1, True);
      fWorldDrawing.d3SetViewPoint(6.4, 6, 45, 45);
      fWorldDrawing.d3SetScales(1, 1, 1);
    end;
  //Ray Lischner's trick to circumvent the default=0 gotcha for float properties.
  fVersion := '6.0(beta 5) May 2000';
  FRecordMetafile := False;
  if AOwner <> nil then
    if (csDesigning in ComponentState) and not (csReading in AOwner.ComponentState) then
    begin
      Width := 30;
      Height := 30;
    end;
end;

destructor TMathImage.Destroy;
begin
  fWorldDrawing.Free;
  inherited Destroy;
end;

procedure TMathImage.SizeChanged;
begin
  inherited;
  if Width <> 0 then if Height <> 0 then
    begin
      fWorldDrawing.SetScreen(Width, Height);
      SetAxis(GetAxis);
      d3ResetWorld;
      invalidate;
    end;
end;



procedure TMathImage.SetAxis;
begin
  fWorldDrawing.Setd2Axes(Canvas, A);
  ClipRect := fWorldDrawing.AxesClipRect;
end;



procedure TMathImage.SetWorld;
var
  sx1, Sx2, sy1, Sy2: MathFloat;

begin
  sx1 := Getx1d2; Sx2 := Getd2Worldx2; sy1 := Gety1d2; Sy2 := Getd2Worldy2;
  try
    fWorldDrawing.SetWorld(Canvas, x1, y1, x2, y2);
  except
    on e: EMathError do
    begin
      fWorldDrawing.SetWorld(Canvas, sx1, sy1, Sx2, Sy2);
      raise EMathImageError.Create('Invalid D2-world bounds');
    end;
  end;
end;


procedure TMathImage.SetColor;
begin
  Pen.Color := Color;
end;

function TMathImage.GetColor;
begin
  Result := Pen.Color;
end;

function TMathImage.Windowx;
begin
  Result := fWorldDrawing.Windowx(x);
end;

function TMathImage.Windowy;
begin
  Result := fWorldDrawing.Windowy(y);
end;

function TMathImage.Norm;
begin
  Result := sqrt(sqr(x) + sqr(y));
end;

function TMathImage.WorldX;
begin
  Result := fWorldDrawing.WorldX(xs);
end;

function TMathImage.WorldY;
begin
  Result := fWorldDrawing.WorldY(Ys);
end;

procedure TMathImage.Clear;

  function NotClipped: Boolean;
  begin
    with ClipRect do
    begin
      Result := (Left = 0) and (Top = 0) and (Right = Width) and (Bottom =
        Height);
    end;
  end;
var
  save: TRect;
begin
  if NotClipped then ClearClipped
  else
  begin
    save := ClipRect;
    ClipRect := ClientRect;
    inherited Clear(Canvas, Brush.Color);
    if FRecordMetafile then
      inherited Clear(MetafileCanvas, Brush.Color);
    //repaint;
    ClipRect := save;
  end;
end;

procedure TMathImage.ClearClipped;
begin
  inherited Clear(Canvas, Brush.Color);
  if FRecordMetafile then
    inherited Clear(MetafileCanvas, Brush.Color);
end;

procedure TMathImage.DrawPoint;
begin
  Canvas.Pixels[Windowx(x), Windowy(y)] := Canvas.Pen.Color;
  if FRecordMetafile then
  begin
    MetafileCanvas.Pixels[Windowx(x), Windowy(y)] := Canvas.Pen.Color;
  end;
end;

procedure TMathImage.MoveToPoint;
begin
  fWorldDrawing.MoveToPoint(Canvas, x, y);
  if FRecordMetafile then
    fWorldDrawing.MoveToPoint(MetafileCanvas, x, y);
end;

procedure TMathImage.DrawLine;
begin
  LockUpdate;
  fWorldDrawing.DrawLine(Canvas, x1, y1, x2, y2);
  if FRecordMetafile then
    fWorldDrawing.DrawLine(MetafileCanvas, x1, y1, x2, y2);
  UnlockUpdate;
end;


procedure TMathImage.DrawLineTo(x, y: MathFloat);
begin
  fWorldDrawing.DrawLineTo(Canvas, x, y);
  if FRecordMetafile then
    fWorldDrawing.DrawLineTo(MetafileCanvas, x, y);
end;

procedure TMathImage.DrawEllipse(x1, y1, x2, y2: MathFloat);
begin
  fWorldDrawing.DrawEllipse(Canvas, x1, y1, x2, y2);
  if FRecordMetafile then
    fWorldDrawing.DrawEllipse(MetafileCanvas, x1, y1, x2, y2);
end;

procedure TMathImage.DrawRectangle;
begin
  fWorldDrawing.DrawRectangle(Canvas, x1, y1, x2, y2);
  if FRecordMetafile then
    fWorldDrawing.DrawRectangle(MetafileCanvas, x1, y1, x2, y2);
end;

procedure TMathImage.DrawAxes;
var
  SaveRect: TRect;
begin
  LockUpdate;
  try
    SetAxis(True);
    SaveRect := ClipRect;
    ClipRect := ClientRect;
    fWorldDrawing.DrawAxes(Canvas, xLabel, yLabel, AxesColor, Arrows);
    if FRecordMetafile then
      fWorldDrawing.DrawAxes(MetafileCanvas, xLabel, yLabel, AxesColor, Arrows);
    ClipRect := SaveRect;
    if ZeroLines then
    begin
      fWorldDrawing.DrawZeroLines(Canvas, ZeroLinesColor);
      if FRecordMetafile then
        fWorldDrawing.DrawZeroLines(MetafileCanvas, ZeroLinesColor);
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.DrawVector;
begin
  LockUpdate;
  try
    fWorldDrawing.DrawVector(Canvas, x, y, A, b);
    if FRecordMetafile then
      fWorldDrawing.DrawVector(MetafileCanvas, x, y, A, b);
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.DrawPolyline(FloatPointList: TFloatPointlist);

begin
  fWorldDrawing.DrawPolyline(Canvas, FloatPointList.fFloatArray, FloatPointList.fCount);
  if FRecordMetafile then
end;

procedure TMathImage.DrawPolygon(FloatPointList: TFloatPointlist);
begin
  fWorldDrawing.DrawPolygon(Canvas, FloatPointList.fFloatArray, FloatPointList.Count);
  if FRecordMetafile then
    fWorldDrawing.DrawPolygon(MetafileCanvas, FloatPointList.fFloatArray, FloatPointList.Count);
end;

procedure TMathImage.DrawPolyPolyline(FloatPointListList: TFloatPointListList);
var
  i: longint;
begin
  LockUpdate;
  try
    if assigned(FloatPointListList) then
      if FloatPointListList.Count > 0 then
        with FloatPointListList do
        begin
          for i := 0 to Count - 1 do
            DrawPolyline(fgraphlist[i]);
        end;
  finally
    UnlockUpdate;
  end;
end;



procedure TMathImage.d3SetWorld;
var
  sx1, sxw, sy1, syw, sz1, szw: MathFloat;
begin
  sx1 := Getx1d3; sxw := Getxwd3; sy1 := Gety1d3;
  syw := Getywd3; sz1 := Getz1d3; szw := Getzwd3;
  try
    fWorldDrawing.d3SetWorld(x1, y1, z1, x2, y2, z2, Getard3);
  except
    on e: EMathError do
    begin
      d3SetWorld(sx1, sy1, sz1, sx1 + sxw, sy1 + syw, sz1 + szw);
      raise EMathImageError.Create('Invalid D3-world bounds');
    end;
  end;
end;

procedure TMathImage.d3ResetWorld;
begin
  fWorldDrawing.d3ResetWorld;
end;


procedure TMathImage.d3Window(x, y, z: MathFloat; var xs, Ys: longint);

begin
  fWorldDrawing.d3Window(x, y, z, xs, Ys);
end;


procedure TMathImage.PseudoD3World;

begin
  fWorldDrawing.PseudoD3World(xs, Ys, x, y, z);
end;


procedure TMathImage.d3Moveto(x, y, z: MathFloat);
begin
  fWorldDrawing.d3Moveto(Canvas, x, y, z);
  if FRecordMetafile then
    fWorldDrawing.d3Moveto(MetafileCanvas, x, y, z);
end;

procedure TMathImage.d3DrawPoint(x, y, z: MathFloat);
begin
  fWorldDrawing.d3DrawPoint(Canvas, x, y, z);
  if FRecordMetafile then
    fWorldDrawing.d3DrawPoint(MetafileCanvas, x, y, z);
end;

procedure TMathImage.d3DrawLine(x1, y1, z1, x2, y2, z2: MathFloat);
begin
  LockUpdate;
  try
    fWorldDrawing.d3DrawLine(Canvas, x1, y1, z1, x2, y2, z2);
    if FRecordMetafile then
      fWorldDrawing.d3DrawLine(MetafileCanvas, x1, y1, z1, x2, y2, z2);
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.d3DrawLineto(x, y, z: MathFloat);
begin
  fWorldDrawing.d3DrawLineto(Canvas, x, y, z);
  if FRecordMetafile then
    fWorldDrawing.d3DrawLineto(MetafileCanvas, x, y, z);
end;


procedure TMathImage.d3DrawAxes;

begin {******* drawd3axes ******}
  LockUpdate;
  fWorldDrawing.d3DrawAxes(Canvas, xLabel, yLabel,
    zLabel, xTicks, yTicks, zTicks, xPos, yPos, zPos, Arrows);
  if FRecordMetafile then
    fWorldDrawing.d3DrawAxes(MetafileCanvas, xLabel, yLabel,
      zLabel, xTicks, yTicks, zTicks, xPos, yPos, zPos, Arrows);
  UnlockUpdate;
end;

procedure TMathImage.d3DrawBestAxes;

begin {******* drawd3axes ******}
  LockUpdate;
  fWorldDrawing.d3DrawBestAxes(Canvas, xLabel, yLabel,
    zLabel, xTicks, yTicks, zTicks, Arrows);
  if FRecordMetafile then
    fWorldDrawing.d3DrawBestAxes(MetafileCanvas, xLabel, yLabel,
      zLabel, xTicks, yTicks, zTicks, Arrows);
  UnlockUpdate;
end;

procedure TMathImage.d3drawZeroCross;
begin
  LockUpdate;
  fWorldDrawing.d3drawZeroCross(Canvas);
  if FRecordMetafile then
    fWorldDrawing.d3drawZeroCross(MetafileCanvas);
  UnlockUpdate;
end;

procedure TMathImage.d3DrawWorldbox;
begin
  LockUpdate;
  fWorldDrawing.d3DrawWorldbox(Canvas);
  if FRecordMetafile then
    fWorldDrawing.d3DrawWorldbox(MetafileCanvas);
  UnlockUpdate;
end;


procedure TMathImage.d3DrawBox;

begin
  LockUpdate;
  fWorldDrawing.d3DrawBox(Canvas, x1, y1, z1, x2, y2, z2);
  if FRecordMetafile then
    fWorldDrawing.d3DrawBox(MetafileCanvas, x1, y1, z1, x2, y2, z2);
  UnlockUpdate;
end;

procedure TMathImage.d3DrawFullWorldBox;
begin
  d3DrawBox(Getx1d3, Gety1d3, Getz1d3, Getx1d3 + Getxwd3, Gety1d3 + Getywd3, Getz1d3 + Getzwd3);
end;

procedure TMathImage.d3Polyline(FloatPointList: TD3FloatPointList);
begin
  LockUpdate;
  try
    fWorldDrawing.d3Polyline(Canvas, FloatPointList.fFloatArray, FloatPointList.Count);
    if FRecordMetafile then
      fWorldDrawing.d3Polyline(MetafileCanvas, FloatPointList.fFloatArray, FloatPointList.Count);
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.d3LitPolyLine(FloatPointList: TD3FloatPointList; diffuse, focussed, RightIntensity: MathFloat;
  zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);
begin
  LockUpdate;
  try
    if not FloatPointList.fPrepared then
      FloatPointList.PrepareIllumination;
    fWorldDrawing.GetIlluminatedLinesegments(Pen.Color, diffuse, focussed, RightIntensity, zrot1, zrot2, yrot1, yrot2, dist1, dist2, fixed, FloatPointList.fLineSegmentArray);
    fWorldDrawing.DrawLineSegments(Canvas, FloatPointList.fLineSegmentArray);
    if FRecordMetafile then
      fWorldDrawing.DrawLineSegments(MetafileCanvas, FloatPointList.fLineSegmentArray);
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.d3PolyPolyline(FloatPointListList: TD3FloatPointListList);
var
  i: longint;
begin
  LockUpdate;
  if assigned(FloatPointListList) then
    if FloatPointListList.Count > 0 then
      with FloatPointListList do
      begin
        for i := 0 to Count - 1 do
          d3Polyline(fgraphlist[i]);
      end;
  UnlockUpdate;
end;

procedure TMathImage.d3LitPolyPolyline(FloatPointListList: TD3FloatPointListList; diffuse, focussed, RightIntensity: MathFloat;
  zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);
var
  i: longint;
begin
  LockUpdate;
  if assigned(FloatPointListList) then
    if FloatPointListList.Count > 0 then
      with FloatPointListList do
      begin
        NormalKind := NormalKind;
        for i := 0 to Count - 1 do
          d3LitPolyLine(fgraphlist[i], diffuse, focussed, RightIntensity, zrot1, zrot2, yrot1, yrot2, dist1, dist2, fixed);
      end;
  UnlockUpdate;
end;





procedure TMathImage.d3StartRotatingLeft(Increment: MathFloat);
var
  inc: MathFloat;
begin
  Rotating := True;
  inc := Increment;
  if ((d3Yrotation > 0) and (trunc(d3Yrotation / 180) mod 2 = 1))
    or ((d3Yrotation <= 0) and (trunc(d3Yrotation / 180) mod 2 = 0))
    then inc := -inc;
  while Rotating do
  begin
    d3Zrotation := d3Zrotation - inc;
    if assigned(FOnRotating) then FOnRotating(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StartRotatingRight(Increment: MathFloat);
var
  inc: MathFloat;
begin
  Rotating := True;
  inc := Increment;
  if ((d3Yrotation > 0) and (trunc(d3Yrotation / 180) mod 2 = 1))
    or ((d3Yrotation <= 0) and (trunc(d3Yrotation / 180) mod 2 = 0))
    then inc := -inc;
  while Rotating do
  begin
    d3Zrotation := d3Zrotation + inc;
    if assigned(FOnRotating) then FOnRotating(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StartRotatingUp(Increment: MathFloat);
begin
  Rotating := True;
  while Rotating do
  begin
    d3Yrotation := d3Yrotation - Increment;
    if assigned(FOnRotating) then FOnRotating(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StartRotatingDown(Increment: MathFloat);
begin
  Rotating := True;
  while Rotating do
  begin
    d3Yrotation := d3Yrotation + Increment;
    if assigned(FOnRotating) then FOnRotating(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StopRotating;
begin
  Rotating := False;
  if assigned(FOnEndRotate) then FOnEndRotate(self);
end;

procedure TMathImage.d3StartMovingIn(Increment: MathFloat);
begin
  Moving := True;
  while Moving do
  begin
    d3ViewDist := d3ViewDist * (1 - Increment);
    if assigned(FOnMoving) then FOnMoving(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StartMovingOut(Increment: MathFloat);
begin
  Moving := True;
  while Moving do
  begin
    d3ViewDist := d3ViewDist * (1 + Increment);
    if assigned(FOnMoving) then FOnMoving(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StopMoving;
begin
  Moving := False;
  if assigned(FOnEndMove) then FOnEndMove(self);
end;

procedure TMathImage.d3StartZoomingIn(Increment: MathFloat);
begin
  Zooming := True;
  while Zooming do
  begin
    d3ViewAngle := d3ViewAngle * (1 - Increment);
    if assigned(FOnZooming) then FOnZooming(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StartZoomingOut(Increment: MathFloat);
begin
  Zooming := True;
  while Zooming do
  begin
    d3ViewAngle := d3ViewAngle * (1 + Increment);
    if assigned(FOnZooming) then FOnZooming(self);
    Application.ProcessMessages;
  end;
end;

procedure TMathImage.d3StopZooming;
begin
  Zooming := False;
  if assigned(FOnEndZoom) then FOnEndZoom(self);
end;

{Surface}


procedure TMathImage.d3DrawSurface(Surface: TSurface; fill, NoUpdate: Boolean);
begin
  LockUpdate;
  with Surface do
  begin
    fDefaultFillColor := Canvas.Brush.Color;
    fDefaultWireColor := Canvas.Pen.Color;
    if not fill then
      fWorldDrawing.d3DrawSurface(Canvas, fFloatsurface, False)
    else
    begin
      fWorldDrawing.d3DrawSurfaceCells(Canvas, fSurfaceCells);
      if FRecordMetafile then
        fWorldDrawing.d3DrawSurfaceCells(MetafileCanvas, fSurfaceCells);
    end;
  end;
  UnlockUpdate;
end;

procedure TMathImage.d3DrawSurfaceCollection(Surfaces: TSurfaceCollection; fill: Boolean);
var i: Integer;
  savecolor: TColor;
begin
  LockUpdate;
  if not fill then
  begin
    savecolor := Pen.Color;
    for i := 0 to Surfaces.Count - 1 do
    begin
      Pen.Color := Surfaces.fSurfaces[i].fDefaultWireColor;
      fWorldDrawing.d3DrawSurface(Canvas, Surfaces.fSurfaces[i].fFloatsurface, False);
    end;
    Pen.Color := savecolor;
  end
  else
  begin
    fWorldDrawing.d3DrawSurfaceCells(Canvas, Surfaces.fCells);
    if FRecordMetafile then
      fWorldDrawing.d3DrawSurfaceCells(MetafileCanvas, Surfaces.fCells);
  end;
  UnlockUpdate;
end;

procedure TMathImage.d3DrawLitSurfaceCollection(Surfaces: TSurfaceCollection; ambient, focussed: MathFloat);
begin
  LockUpdate;
  if not Surfaces.fprepared then
    Surfaces.prepareIllumination;
    fWorldDrawing.d3DrawLitTriangles(Canvas, Surfaces.fTriangs, ambient, focussed);
    if FRecordMetafile then
      fWorldDrawing.d3DrawLitTriangles(MetafileCanvas, Surfaces.fTriangs, ambient, focussed);
  UnlockUpdate;
end;




procedure TMathImage.d3DrawLitSurface(Surface: TSurface; diffuse, focussed:
  MathFloat; NoUpdate: Boolean);

begin
  LockUpdate;
  with Surface do
  begin
    fDefaultFillColor := Brush.Color;
    fDefaultWireColor := Pen.Color;
    if not fPrepared then
      PrepareIllumination;
    fWorldDrawing.d3DrawLitTriangles(Canvas, fTriangles, diffuse, focussed);
    if FRecordMetafile then
      fWorldDrawing.d3DrawLitTriangles(MetafileCanvas, fTriangles, diffuse, focussed);
  end;
  UnlockUpdate;
end;


procedure TMathImage.d3DrawCube;
var
  Cubes: array of TCube;
begin
  if x1 < x2 then
    if y1 < y2 then
      if z1 < z2 then
      begin
        SetLength(Cubes, 1);
        Cubes[0].x1 := x1;
        Cubes[0].y1 := y1;
        Cubes[0].z1 := z1;
        Cubes[0].x2 := x2;
        Cubes[0].y2 := y2;
        Cubes[0].z2 := z2;
        Cubes[0].FillColor := Brush.Color;
        Cubes[0].WireColor := Pen.Color;
        LockUpdate;
        try
          fWorldDrawing.d3DrawCubes(Canvas, Cubes, fill);
          if FRecordMetafile then
            fWorldDrawing.d3DrawCubes(MetafileCanvas, Cubes, fill);
        finally
          UnlockUpdate;
        end;
      end
      else
        raise(EMathImageError.Create('Cube coordinates must be (xlow,ylow,zlow, xup,yup,zup)'));

end;

{procedure TMathImage.d3DrawLitCube;
begin
end;}

procedure TMathImage.DrawFilledLevelCurves(LevelSurface: TLevelSurface);

begin
  LockUpdate;
  with LevelSurface do
    if Length(fLevels) > 0 then
    begin
      fWorldDrawing.DrawProjections(Canvas, fTriangles);
      if FRecordMetafile then
        fWorldDrawing.DrawProjections(MetafileCanvas, fTriangles);
    end;
  UnlockUpdate;
end;


procedure TMathImage.DrawLevelCurves(Surface: TSurface; Level: MathFloat);
begin
  LockUpdate;
  with Surface do
  begin
    fWorldDrawing.DrawLevelLines(Canvas, fTriangles, Level);
    if FRecordMetafile then
      fWorldDrawing.DrawLevelLines(MetafileCanvas, fTriangles, Level);
  end;
  UnlockUpdate;
end;

procedure TMathImage.d3DrawCustomAxes(
  xmin, ymin, zmin, xmax, ymax, zmax: MathFloat;
  xLabel, yLabel, zLabel: string);

begin
  LockUpdate;
  fWorldDrawing.d3DrawCustomAxes(Canvas, xmin, ymin, zmin, xmax, ymax, zmax, xLabel, yLabel, zLabel);
  if FRecordMetafile then
    fWorldDrawing.d3DrawCustomAxes(MetafileCanvas, xmin, ymin, zmin, xmax, ymax, zmax, xLabel, yLabel, zLabel);
  UnlockUpdate;
end;

procedure TMathImage.d3DrawHeightCubes(HeightMap: THeightMap);
begin
  LockUpdate;
  try
    if assigned(HeightMap) then
      with HeightMap do
      begin
        fWorldDrawing.d3DrawHeightCubes(Canvas, fHeightArray, fColors);
        if FRecordMetafile then
          fWorldDrawing.d3DrawHeightCubes(MetafileCanvas, fHeightArray, fColors);
      end;
  finally
    UnlockUpdate;
  end;
end;

procedure TMathImage.d3DrawLitHeightCubes(HeightMap: THeightMap; diffuse, focussed: MathFloat);
begin
  LockUpdate;
  try
    if assigned(HeightMap) then
      with HeightMap do
      begin
        fWorldDrawing.d3DrawLitHeightCubes(Canvas, fHeightArray, fColors, diffuse, focussed);
        if FRecordMetafile then
          fWorldDrawing.d3DrawLitHeightCubes(MetafileCanvas, fHeightArray, fColors, diffuse, focussed);
      end;
  finally
    UnlockUpdate;
  end;
end;

function TMathImage.Getalpha: MathFloat;
begin
  Result := fWorldDrawing.d3alpha;
end;

function TMathImage.Getard3: Boolean;
begin
  Result := fWorldDrawing.d3ar;
end;

function TMathImage.GetAxis: Boolean;
begin
  Result := fWorldDrawing.d2Axes;
end;

function TMathImage.Getvd: MathFloat;
begin
  Result := fWorldDrawing.d3vd;
end;

function TMathImage.Getx1d2: MathFloat;
begin
  Result := fWorldDrawing.d2x1;
end;

function TMathImage.Getx1d3: MathFloat;
begin
  Result := fWorldDrawing.d3x1;
end;

function TMathImage.GetXscale: MathFloat;
begin
  Result := fWorldDrawing.d3Xscale;
end;

function TMathImage.Getxwd2: MathFloat;
begin
  Result := fWorldDrawing.d2xw;
end;

function TMathImage.Getxwd3: MathFloat;
begin
  Result := fWorldDrawing.d3xw;
end;

function TMathImage.Gety1d2: MathFloat;
begin
  Result := fWorldDrawing.d2y1;
end;

function TMathImage.Gety1d3: MathFloat;
begin
  Result := fWorldDrawing.d3y1;
end;

function TMathImage.Getyrd3: MathFloat;
begin
  Result := fWorldDrawing.d3yr;
end;

function TMathImage.GetYscale: MathFloat;
begin
  Result := fWorldDrawing.d3Yscale;
end;

function TMathImage.Getywd2: MathFloat;
begin
  Result := fWorldDrawing.d2yw;
end;

function TMathImage.Getywd3: MathFloat;
begin
  Result := fWorldDrawing.d3yw;
end;

function TMathImage.Getz1d3: MathFloat;
begin
  Result := fWorldDrawing.d3z1;
end;

function TMathImage.Getzrd3: MathFloat;
begin
  Result := fWorldDrawing.d3zr;
end;

function TMathImage.GetZscale: MathFloat;
begin
  Result := fWorldDrawing.d3Zscale;
end;

function TMathImage.Getzwd3: MathFloat;
begin
  Result := fWorldDrawing.d3zw;
end;

procedure TMathImage.DrawCircle(xCenter, yCenter: MathFloat;
  PixRadius: Integer);
begin
  fWorldDrawing.DrawCircle(Canvas, xCenter, yCenter, PixRadius);
  if FRecordMetafile then
    fWorldDrawing.DrawCircle(MetafileCanvas, xCenter, yCenter, PixRadius);
end;



procedure TMathImage.WorldToScreen(const x, y: MathFloat; var xs,
  Ys: Integer);
begin
  fWorldDrawing.WorldToScreen(x, y, xs, Ys);
end;

{ TColorSurface }

constructor TColorSurface.Create(xGrid, yGrid: Integer);
var
  i, j, Current: Integer;
begin
  SetLength(fColors, xGrid + 1);
  for i := 0 to xGrid do
    SetLength(fColors[i], yGrid + 1);
  inherited Create(xGrid, yGrid);
  Current := 0;
  for i := 0 to xGrid - 1 do
    for j := 0 to yGrid - 1 do
    begin
      fSurfaceCells[Current].FillColor := @fColors[i][j];
      inc(Current);
    end;
end;

destructor TColorSurface.Destroy;
var i: Integer;
begin
  for i := 0 to xMesh do
    SetLength(fColors[i], 0);
  SetLength(fColors, 0);
  inherited;
end;

function TColorSurface.GetColor(i, j: Integer): TColor;
begin
  Result := fColors[i][j];
end;

function TColorSurface.GetFillColor(i, j: Integer): Pointer;
begin
  Result := @fColors[i][j];
end;

function TColorSurface.GetWireColor(i, j: Integer): Pointer;
begin
  Result := @fDefaultWireColor;
end;

procedure TColorSurface.Make(i, j: Integer; x, y, z: MathFloat;
  Color: TColor);
begin
  inherited Make(i, j, x, y, z);
  fColors[i][j] := Color;
end;

{ THeightMap }

constructor THeightMap.Create(xGrid, yGrid: Integer);
var i: Integer;
begin
  inherited Create;
  SetLength(fHeightArray, xGrid + 1);
  for i := 0 to xGrid do
    SetLength(fHeightArray[i], yGrid + 1);
  SetLength(fColors, xGrid + 1);
  for i := 0 to xGrid do
    SetLength(fColors[i], yGrid + 1);
  fxm := xGrid;
  fym := yGrid;
end;

procedure THeightMap.Make(i, j: Integer; z: MathFloat; Color: TColor);
begin
  if (i >= 0) and (i <= fxm) and (j >= 0) and (j <= fym) then
  begin
    fHeightArray[i][j] := z;
    fColors[i][j] := Color;
  end
  else
    raise ESurfaceError.Create('Heightmap grid point does not exist');
end;

{ TLevelSurface }

function SplitTriangle(c: MathFloat; tr: TD3Triangle; var tr1, tr2, tr3:
  TD3Triangle; var NewPoint1, NewPoint2: PD3FloatPoint): Boolean;
var
  t1, t2, xp, yp, p, xq, yq, q, xr, yr, r, x1, y1, x2, y2, epsilon: MathFloat;
begin
  Result := False;
  epsilon := 1.0E-15;
  if not (((c - tr.p.z) * (tr.q.z - c) > epsilon) or ((c - tr.p.z) * (tr.r.z - c) > epsilon)) then
    exit; //testing 2 is enough
  xp := tr.p.x; yp := tr.p.y; p := tr.p.z;
  xq := tr.q.x; yq := tr.q.y; q := tr.q.z;
  xr := tr.r.x; yr := tr.r.y; r := tr.r.z;
  if (c - p) * (q - c) > 0 then //sign change p-q
  begin
    t1 := (c - q) / (p - q);
    x1 := t1 * xp + (1 - t1) * xq;
    y1 := t1 * yp + (1 - t1) * yq;
    if (c - p) * (r - c) >= 0 then //sign change p-r
    begin
      if p = r then
        exit;
      t2 := (c - r) / (p - r);
      x2 := t2 * xp + (1 - t2) * xr;
      y2 := t2 * yp + (1 - t2) * yr;
      Result := True;
      tr1.p := tr.p;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.q;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.q;
      tr3.q := tr.r;
      tr3.r := NewPoint2;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end
    else //sign change must be q-r
    begin
      if r = q then
        exit;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      tr1.p := tr.q;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.p;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.r;
      tr3.r := NewPoint2;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end;
  end
  else
  begin
    if (c - p) * (r - c) > 0 then
      //sign change p-r which implies sign change q-r
    begin
      if p = r then
        exit;
      t1 := (c - r) / (p - r);
      x1 := t1 * xp + (1 - t1) * xr;
      y1 := t1 * yp + (1 - t1) * yr;
      if q = r then
        exit;
      if p = q then
        exit;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.p := tr.q;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.r;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.q;
      tr3.r := NewPoint1;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end
    else
    begin
      //now sign change must be q-r, and c=p, so:
      x1 := xp; y1 := yp;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.p := tr.q;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.r;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.q;
      tr3.r := NewPoint2;
      //still need to come up with 3 triangles, though is splits in 2
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end;
  end;
  CrossProduct(tr1.p.x - tr1.r.x, tr1.p.y - tr1.r.y, tr1.p.z - tr1.r.z, tr1.q.x - tr1.r.x, tr1.q.y - tr1.r.y, tr1.q.z - tr1.r.z, tr1.n.x, tr1.n.y, tr1.n.z);
  CrossProduct(tr2.p.x - tr2.r.x, tr2.p.y - tr2.r.y, tr2.p.z - tr2.r.z, tr2.q.x - tr2.r.x, tr2.q.y - tr2.r.y, tr2.q.z - tr2.r.z, tr2.n.x, tr2.n.y, tr2.n.z);
  CrossProduct(tr3.p.x - tr3.r.x, tr3.p.y - tr3.r.y, tr3.p.z - tr3.r.z, tr3.q.x - tr3.r.x, tr3.q.y - tr3.r.y, tr3.q.z - tr3.r.z, tr3.n.x, tr3.n.y, tr3.n.z);
end;


procedure TLevelSurface.SetLevels(const Levels: array of MathFloat;
  const Colors: array of TColor);
var
  i, j,
    ColCount, SplitCount,
    TriangleCount, NewPointCount,
    TriangleLength, NewPointLength: Integer;
  Level, epsilon: MathFloat;
  Done: Boolean;
  NewPoint1, NewPoint2: PD3FloatPoint;
  tr1, tr2, tr3: TD3Triangle;
begin
  if not fPrepared then
    PrepareIllumination;
  epsilon := 1.0E-12;
  ColCount := High(Colors);
  if ColCount > High(Levels) then
    ColCount := High(Levels);
  SetLength(fLevels, ColCount + 1);
  SetLength(fColors, ColCount + 1);
  for i := 0 to ColCount do
  begin
    fLevels[i] := Levels[i];
    fColors[i] := Colors[i];
  end;
  TriangleCount := Length(fTriangles);
  SetLength(fTriangles, TriangleCount + 200);
  TriangleLength := Length(fTriangles);
  if Length(fNewPoints) > 0 then
  begin
    for i := 0 to High(fNewPoints) do
      dispose(fNewPoints[i]);
  end;
  SetLength(fNewPoints, 200);
  NewPointLength := 200;
  NewPointCount := 0;
  i := 0;
  //This loop should always stop, even though TriangleCount is
  //being incremented. Please tell me if it bombs on you.
  while i < TriangleCount do
  begin
    SplitCount := 0;
    for j := 0 to ColCount do
    begin
      //The new pointer allocations in this routine are not so great
      // of a memory use. but I've got no better idea.
      if SplitTriangle(fLevels[j], fTriangles[i], tr1, tr2, tr3, NewPoint1, NewPoint2) then
      begin
        inc(SplitCount);
        if NewPointCount > NewPointLength - 2 then
        begin
          NewPointLength := NewPointLength + 100;
          SetLength(fNewPoints, NewPointLength);
        end;
        fNewPoints[NewPointCount] := NewPoint1;
        inc(NewPointCount);
        fNewPoints[NewPointCount] := NewPoint2;
        inc(NewPointCount);
        if TriangleCount > TriangleLength - 2 then
        begin
          TriangleLength := TriangleLength + 100;
          SetLength(fTriangles, TriangleLength);
        end;
        fTriangles[i] := tr1;
        fTriangles[TriangleCount] := tr2;
        inc(TriangleCount);
        fTriangles[TriangleCount] := tr3;
        inc(TriangleCount);
      end
      else
        if SplitCount > 0 then break;
    end;
    inc(i);
  end;
  for i := 0 to TriangleCount - 1 do
    with fTriangles[i] do
    begin
      Done := False;
      Level := 0.3333333333333333 * (p.z + q.z + r.z);
      for j := 0 to ColCount - 1 do
      begin
        if fLevels[j] < Level + epsilon then
          if Level < fLevels[j + 1] + epsilon then
          begin
            FillColor := @fColors[j];
            Done := True;
            break;
          end;
      end;
      if not Done then
      begin
        if Level >= fLevels[ColCount] then
          FillColor := @fColors[ColCount]
        else
          if Level <= fLevels[0] then
            FillColor := @fColors[0];
      end;
    end;
  SetLength(fTriangles, TriangleCount);
  SetLength(fNewPoints, NewPointCount);
end;

destructor TLevelSurface.Destroy;
var
  i: Integer;
begin
  if Length(fNewPoints) > 0 then
  begin
    for i := 0 to High(fNewPoints) do
      dispose(fNewPoints[i]);
  end;
  inherited Destroy;
end;

{ TSurfaceCollection }

procedure TSurfaceCollection.add(const Surface: TSurface; FillColor, WireColor: TColor);
var j, l, Current: Integer;
begin
  if fCount = fLength then
  begin
    inc(fLength, 10);
    SetLength(fSurfaces, fLength);
  end;
  fSurfaces[fCount] := Surface;
  Surface.fDefaultFillColor := FillColor;
  Surface.fDefaultWireColor := WireColor;
  inc(fCount);
  Current := Length(fCells);
  with fSurfaces[fCount-1] do
  begin
    l := Current + Length(fSurfaceCells);
    SetLength(fCells, l);
    for j := Current to l - 1 do
      fCells[j] := fSurfaceCells[j - Current];
  end;
  Current := Length(fTriangs);
  with fSurfaces[fCount-1] do
  begin
    l:=Current+Length(fTriangles);
    SetLength(fTriangs,l);
    for j:=Current to l-1 do
      fTriangs[j]:=fTriangles[j-Current];
  end;
  fprepared:=false;
end;

procedure TSurfaceCollection.PrepareIllumination;
var i: Integer;
begin
  for i := 0 to High(fTriangs) do
    with fTriangs[i] do
      CrossProduct(p.x - r.x, p.y - r.y, p.z - r.z, q.x - r.x, q.y - r.y, q.z - r.z, n.x, n.y, n.z);
  fPrepared := True;
end;

constructor TSurfaceCollection.Create;
begin
  fLength := 10;
  fCount := 0;
  SetLength(fSurfaces, fLength);
  SetLength(fCells, 0);
  SetLength(fTriangs,0);
  fprepared:=false;
end;

end.

