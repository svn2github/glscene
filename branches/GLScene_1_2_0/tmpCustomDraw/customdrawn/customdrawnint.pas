{
 /***************************************************************************
                CustomDrawnInt.pas -  CustomDrawn Interface Object
                             -------------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit CustomDrawnInt;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // RTL
  Types, Classes, SysUtils, Math,
  fpimage, fpcanvas, fpimgcanv, ctypes, dateutils,
  {$ifdef CD_Windows}Windows, customdrawn_WinProc,{$endif}
  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, CocoaPrivate, CocoaGDIObjects,{$endif}
  {$ifdef CD_X11}X, XLib, XUtil, BaseUnix, customdrawn_x11proc,{$ifdef CD_UseNativeText}xft, fontconfig,{$endif}{$endif}
  {$ifdef CD_Android}
  customdrawn_androidproc, jni, bitmap, log, keycodes,
  {$endif}
  {$ifdef WinCE}aygshell,{$endif}
  // Widgetset
  customdrawnproc,
  // LCL
 // customdrawn_common, customdrawncontrols, customdrawndrawers,
  lazcanvas, lazregions, lazdeviceapis,
  InterfaceBase,
  Controls,  Forms, lclproc, IntfGraphics, GraphType,
  LCLType, LMessages, Graphics, LCLStrConsts, LazLogger;

type
  {$ifdef CD_Windows}
  TWinCETitlePolicy = (tpAlwaysUseOKButton, tpOKButtonOnlyOnDialogs, tpControlWithBorderIcons);

  PPPipeEventInfo = ^PPipeEventInfo;
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handle: THandle;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
    Prev: PPipeEventInfo;
    Next: PPipeEventInfo;
  end;

  TWaitHandler = record
    ListIndex: pdword;
    UserData: PtrInt;
    OnEvent: TWaitHandleEvent;
  end;

  TSocketEvent = function(ASocket: THandle; Flags: dword): Integer of object;
  {$endif}
  {$ifdef CD_Cocoa}

  TCDTimerObject=objcclass(NSObject)
    func : TWSTimerProc;
    procedure timerEvent; message 'timerEvent';
    class function initWithFunc(afunc: TWSTimerProc): TCDTimerObject; message 'initWithFunc:';
  end;

  TCDAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
    function applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply; message 'applicationShouldTerminate:';
  end;
  {$endif}

  { TCDWidgetSet }

  TCDWidgetSet = class(TWidgetSet)
  private
    FTerminating: Boolean;

    {$ifdef CD_WINDOWS}
    // In win32 it is: The parent of all windows, represents the button of the taskbar
    // In wince it is just an invisible window, but retains the following functions:
    // * This window is also the owner of the clipboard.
    // * Assoc. windowproc also acts as handler for popup menus
    // * It is indispensable for popupmenus and thread synchronization
    FAppHandle: THandle;

    FMetrics: TNonClientMetrics;
    FMetricsFailed: Boolean;

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    FStatusFont: HFONT;
    FMessageFont: HFONT;

    FWaitHandleCount: dword;
    FWaitHandles: array of HANDLE;
    FWaitHandlers: array of TWaitHandler;
    FWaitPipeHandlers: PPipeEventInfo;

    FOnAsyncSocketMsg: TSocketEvent;

    function WinRegister: Boolean;
    procedure CreateAppHandle;
    {$endif}
  public
    {$ifdef CD_X11}
    FDisplayName: string;
    FDisplay: PDisplay;

    LeaderWindow: X.TWindow;
    ClientLeaderAtom: TAtom;

    FWMProtocols: TAtom;	  // Atom for "WM_PROTOCOLS"
    FWMDeleteWindow: TAtom;	  // Atom for "WM_DELETE_WINDOW"
    FWMHints: TAtom;		  // Atom for "_MOTIF_WM_HINTS"

    // For composing character events
    ComposeBuffer: string;
    ComposeStatus: TStatus;
    InputMethod: xlib.PXIM;
    InputContext: PXIC;
    LastKeySym: TKeySym; // Used for KeyRelease event
    LastKey: Word;       // Used for KeyRelease event

    // XConnections list
    XConnections: TFPList;

    function FindWindowByXID(XWindowID: X.TWindow; out AWindowInfo: TX11WindowInfo): TWinControl;
    procedure AppProcessMessage;
    {$endif}
    {$ifdef CD_Android}
    CombiningAccent: Cardinal;
    MajorVersion, MinorVersion: Integer;
    {$IFnDEF WithOldDebugln}
    procedure AndroidDebugLn(ASender: TObject; AStr: string; var AHandled: Boolean);
    {$ELSE}
    procedure AndroidDebugLn(AStr: string);
    {$ENDIF}
    procedure StartEGL();
    procedure FinishEGL() ;
    function CreateContext():Pointer;
    procedure DestroyContext(aContext:Pointer);
    procedure CreateSurface();
    procedure DestroySurface();
    procedure PurgeBuffers();
    procedure ClearBuffers();
    procedure SwapBuffers();

    function AndroidKeyCodeToLCLKeyCode(AAndroidKeyCode: Integer): Word;
    {$endif}
    {$ifdef CD_Cocoa}
    pool      : NSAutoreleasePool;
    NSApp     : NSApplication;
    delegate  : TCDAppDelegate;
    ScreenBitmapContext: CGContextRef;
    {$endif}
  // For generic methods added in customdrawn
  // They are used internally in LCL-CustomDrawn, LCL app should not use them
  public
    AccumulatedStr: string;
    // The currently focused control
    FocusedControl: TWinControl;
    FocusedIntfControl: TWinControl;
    // Default Fonts
    DefaultFont: TFPCustomFont;
    DefaultFontAndroidSize: Integer;
    // Mobile emulator and mobile mode
    MobileMainForm: TLCLIntfHandle;
    // For unusual implementations of DebugLn/DebugOut
    {$IFnDEF WithOldDebugln}
    procedure AccumulatingDebugOut(ASender: TObject; AStr: string; var AHandled: Boolean);
    {$ELSE}
    procedure AccumulatingDebugOut(AStr: string);
    {$ENDIF}
    //
    procedure CDSetFocusToControl(ALCLControl, AIntfControl: TWinControl);
  //
  protected
    {function CreateThemeServices: TThemeServices; override;}
    {function GetAppHandle: THandle; override;
    procedure SetAppHandle(const AValue: THandle); override;}
    //
    procedure BackendCreate;
    procedure BackendDestroy;
  public
    // ScreenDC and Image for doing Canvas operations outside the Paint event
    // and also for text drawing operations
   { ScreenDC: TLazCanvas;
    ScreenBitmapRawImage: TRawImage;
    ScreenBitmapHeight: Integer;
    ScreenBitmapWidth: Integer;
    ScreenImage: TLazIntfImage;   }

    // Android Activity callbacks
    ActivityOnCreate: TProcedure;

    constructor Create; override;
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;
    function GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetIcon(const Small, Big: HICON); override;
    procedure AppSetTitle(const ATitle: string); override;
    procedure AppSetVisible(const AVisible: Boolean); override;
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
    procedure AppSetMainFormOnTaskBar(const DoSet: Boolean); override;

    //function  InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean; override;

    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle; override;
    function DestroyTimer(TimerHandle: THandle): boolean; override;

    {$I customdrawnwinapih.inc}
    {$I customdrawnlclintfh.inc}
  public
    { Variables to be set by the user }
    {$ifdef WinCE}
    WinCETitlePolicy: TWinCETitlePolicy;
    {$endif}
  end;

var
  CDWidgetSet: TCDWidgetSet absolute WidgetSet;

function CDMessageBoxFunction(Text, Caption : PChar; Flags : Longint) : Integer;

{$ifdef CD_WINDOWS}
function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult; {$ifdef WinCE}cdecl;{$else}stdcall;{$endif}
{$endif}
{$ifdef CD_X11}
procedure MyXConnectionWatchProc(display: PDisplay; client_data: TXPointer;
  fd: cint; opening: XLib.TBool; watch_data: PXPointer); cdecl;
{$endif}
{$ifdef CD_Android}
function Java_com_pascal_lclproject_LCLActivity_LCLOnTouch(env:PJNIEnv;this:jobject; x, y: single; action: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnDraw(
    env:PJNIEnv;this:jobject; width, height: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnCreate(
    env:PJNIEnv; this:jobject; alclactivity: jobject): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished(
    env:PJNIEnv; this:jobject; AResult: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnKey(
    env:PJNIEnv; this:jobject; AKind: jint; AKeyCode: jint;
    AEvent: jobject; AChar: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnTimer(
    env:PJNIEnv; this:jobject; ATimer: jobject): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged(
    env:PJNIEnv; this:jobject; ANewDPI, ANewWidth: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged(
    env:PJNIEnv; this:jobject; ASensorKind: jint; AValues: JDoubleArray): jint; cdecl;

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;

var
  javaVMRef: PJavaVM=nil;
  javaEnvRef: PJNIEnv=nil;
  javaActivityClass: JClass = nil;
  javaActivityObject: jobject = nil;

  // Other classes and objects
  javaAndroidAppActivityClass: JClass = nil;
  javaJavaLandSystemClass: JClass = nil;
  javaAndroidOSBuildClass: JClass = nil;
  javaAndroidOSVibratorClass: JClass = nil;
  javaAndroidContentContextClass: JClass = nil;

  // Fields of our Activity
  // Strings
  javaField_lcltext: JfieldID=nil;
  javaField_lcltitle: JfieldID=nil;
  javaField_lclbutton1str: JfieldID=nil;
  javaField_lclbutton2str: JfieldID=nil;
  javaField_lclbutton3str: JfieldID=nil;
  // Integers
  javaField_lclwidth: JfieldID=nil;
  javaField_lclheight: JfieldID=nil;
  javaField_lclbutton1: JfieldID=nil;
  javaField_lclbutton2: JfieldID=nil;
  javaField_lclbutton3: JfieldID=nil;
  javaField_lclbitmap: JfieldID=nil;
  javaField_lcltextsize: JfieldID=nil;
  // Text metrics
  javaField_lcltextascent: JfieldID=nil;
  javaField_lcltextbottom: JfieldID=nil;
  javaField_lcltextdescent: JfieldID=nil;
  javaField_lcltextleading: JfieldID=nil;
  javaField_lcltexttop: JfieldID=nil;
  javaField_lclmaxwidth: JfieldID=nil;
  javaField_lclmaxcount: JfieldID=nil;
  javaField_lclpartialwidths: JfieldID=nil;
  // Timer
  javaField_lcltimerinterval: JfieldID=nil;
  javaField_lcltimerid: JfieldID=nil;
  // Screen Metrics
  javaField_lclxdpi: JfieldID=nil;
  javaField_lclydpi: JfieldID=nil;
  javaField_lclformwidth: JfieldID=nil;
  javaField_lclformheight: JfieldID=nil;
  javaField_lclscreenwidth: JfieldID=nil;
  javaField_lclscreenheight: JfieldID=nil;
  // For LazDeviceAPIs
  javaField_lcldestination: JfieldID=nil;
  javaField_lclkind: JfieldID=nil;

  // Methods of our Activity
  javaMethod_LCLDoGetTextBounds: jmethodid = nil;
  javaMethod_LCLDoGetTextPartialWidths: jmethodid = nil;
  javaMethod_LCLDoDrawText: jmethodid = nil;
  javaMethod_LCLDoShowMessageBox: jmethodid = nil;
  javaMethod_LCLDoCreateTimer: jmethodid = nil;
  javaMethod_LCLDoDestroyTimer: jmethodid = nil;
  javaMethod_LCLDoHideVirtualKeyboard: jmethodid = nil;
  javaMethod_LCLDoShowVirtualKeyboard: jmethodid = nil;
  javaMethod_LCLDoStartReadingAccelerometer: jmethodid = nil;
  javaMethod_LCLDoStopReadingAccelerometer: jmethodid = nil;
  javaMethod_LCLDoSendMessage: jmethodid = nil;
  javaMethod_LCLDoRequestPositionInfo: jmethodid = nil;
  // Methods from android.app.Activity
  javaMethod_Activity_finish: jmethodid = nil;
  // Methods from java.lang.System
  javaMethod_System_exit: jmethodid = nil;
  // Generic methods from Context
  javaMethod_getSystemService: jmethodid = nil;

  javaField_lclsurfaceholder: JfieldID=nil;

  // This is utilized to store the information such as invalidate requests in events
  eventResult: jint;
  // Methods of OpenGL
  javaMethod_LCLDoStartEGL: jmethodid = nil;
  javaMethod_LCLDoFinishEGL: jmethodid = nil;
  javaMethod_LCLDoCreateContext: jmethodid = nil;
  javaMethod_LCLDoDestroyContext: jmethodid = nil;
  javaMethod_LCLDoCreateSurface: jmethodid = nil;
  javaMethod_LCLDoDestroySurface: jmethodid = nil;
  javaMethod_LCLDoPurgeBuffers: jmethodid = nil;
  javaMethod_LCLDoClearBuffers: jmethodid = nil;
  javaMethod_LCLDoSwapBuffers: jmethodid = nil;

  javaField_lclmajorversion: JfieldID=nil;
  javaField_lclminorversion: JfieldID=nil;

{$endif}

implementation

uses
  WsControls, lclintf,
  CustomDrawnWSFactory,  //registracia komponentov
  CustomDrawnWSForms,    //formi
{  Win32WSButtons,
  Win32WSMenus,
  Win32WSStdCtrls,
  Win32WSDialogs,
  Win32Themes,
////////////////////////////////////////////////////
  Win32Extra,}
  customdrawnprivate,  //klaviatura i mish
  LCLMessageGlue;

  {$ifdef CD_Windows}
const
  CDBackendNativeHandle = nhtWindowsHWND;
  {$define CD_HasNativeFormHandle}
  {$endif}
  {$ifdef CD_X11}
const
  CDBackendNativeHandle = nhtX11TWindow;
  {$define CD_HasNativeFormHandle}
  {$endif}
  {$ifdef CD_Cocoa}
const
  CDBackendNativeHandle = nhtCocoaNSWindow;
  {$define CD_HasNativeFormHandle}
  {$endif}

{$I customdrawnobject.inc}

{$I customdrawnwinapi.inc}
{$I customdrawnlclintf.inc}

{$ifdef CD_Windows}
  {$include wincallback.inc}
  {$I customdrawnobject_win.inc}
  {$I customdrawnwinapi_win.inc}
  {$I customdrawnlclintf_win.inc}
{$endif}
{$ifdef CD_Cocoa}
  {$I customdrawnobject_cocoa.inc}
  {$I customdrawnwinapi_cocoa.inc}
  {$I customdrawnlclintf_cocoa.inc}
{$endif}
{$ifdef CD_X11}
  {$I customdrawnobject_x11.inc}
  {$I customdrawnwinapi_x11.inc}
  {$I customdrawnlclintf_x11.inc}
{$endif}
{$ifdef CD_Android}
  {$I customdrawnobject_android.inc}
  {$I customdrawnwinapi_android.inc}
  {$I customdrawnlclintf_android.inc}
{$endif}

end.