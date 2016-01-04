//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{@HTML (
   DGLContext
   <p>Prototypes and base implementation of TDGLContext.</p>
   <p>
   <b>History: </b><font size=-1><ul>
      <li>21/12/15 - JD -  Simplyfied and Imported From GLScene
   </ul></font></p> )
}
unit DGLContext;

interface

{$I DGLEngine.inc}

uses
  //System
  Winapi.Windows,
  System.Classes, System.SysUtils, System.Types, System.SyncObjs,
  VCL.Forms, VCL.Controls, VCL.Consts,
  //DGLE
  DGLSLog,
  dglOpenGL,
  DGLCrossPlatform,
  DGLTypes,
  DGLResStrings,
  DGLState,
  DGLTextureFormat,
  DGLPipelineTransformation;

  //array[0..3] of GLenum = (GL_FRONT_LEFT), GL_AUX0, GL_AUX1, GL_AUX2);  Deprecated

Type
  EDGLContext = class(Exception);
  EPBuffer = class(Exception);

  TDGLContext = class;
  TDGLContextManager = class;

  TDGLRCOption = (rcoDoubleBuffered, rcoStereo, rcoDebug, rcoOGL_ES);
  TDGLRCOptions = set of TDGLRCOption;

  TDGLContextLayer = (clUnderlay2, clUnderlay1, clMainPlane, clOverlay1, clOverlay2);


  TAbstractMultitextureCoordinator = class(TObject)
  protected
    FOwner: TDGLContext;
  public
    constructor Create(AOwner: TDGLContext); virtual;
  end;

  TAbstractMultitextureCoordinatorClass = class of TAbstractMultitextureCoordinator;


  // ****************************************************************************************
  // TContextOption
  //
  { @HTML ( Options for the rendering context.<p>
     <ul>
     <li>roDoubleBuffer: enables double-buffering.</li>
     <li>roRenderToWindows: ignored (legacy).</li>
     <li>roTwoSideLighting: enables two-side lighting model.</li>
     <li>roStereo: enables stereo support in the driver (dunno if it works, I don't have a stereo device to test...)</li>
     <li>roDestinationAlpha: request an Alpha channel for the rendered output</li>
     <li>roNoColorBuffer: don't request a color buffer (color depth setting ignored)</li>
     <li>roNoColorBufferClear: do not clear the color buffer automatically, if the whole viewer is fully repainted each frame, this can improve framerate</li>
     <li>roNoSwapBuffers: don't perform RenderingContext.SwapBuffers after rendering</li>
     <li>roNoDepthBufferClear: do not clear the depth buffer automatically. Useful for early-z culling.</li>
     </ul></p> ) }

  TContextOption = ( roDoubleBuffer, roStencilBuffer,
                    roRenderToWindow, roTwoSideLighting, roStereo,
                    roDestinationAlpha, roNoColorBuffer, roNoColorBufferClear,
                    roNoSwapBuffers, roNoDepthBufferClear, roDebugContext,
                    roOpenGL_ES2_Context);

  TContextOptions = set of TContextOption;

  PGLRCHandle = ^TGLRCHandle;
  TGLRCHandle = record
    FRenderingContext: TDGLContext;
    FHandle: TGLuint;
    FChanged: Boolean;
  end;

  TOnPrepareHandleData = procedure(AContext: TDGLContext) of object;

  // ****************************************************************************************
  // TGLContext
  //
  { @HTML ( Wrapper around an OpenGL rendering context.<p>
     The aim of this class is to offer platform-independant
     initialization, activation and management of OpenGL
     rendering context. The class also offers notifications
     event and error/problems detection.<br>
     This is a virtual abstract a class, and platform-specific
     subclasses must be used.<br>
     All rendering context share the same lists.</p> ) }
  TDGLContext = class
  private
    FColorBits    : Integer;
    FAlphaBits    : Integer;
    FDepthBits    : Integer;
    FStencilBits  : Integer;
    FAccumBits    : Integer;
    FAuxBuffers   : Integer;
    FAntiAliasing : TDGLAntiAliasing;
    FOptions: TDGLRCOptions;

    FActivationCount: Integer;
    FOwnedHandlesCount: Integer;
    FManager: TDGLContextManager;

    FOnDestroyContext: TNotifyEvent;

    FIsPreparationNeed : Boolean;

    procedure SetColorBits(const aColorBits: Integer);
    procedure SetAlphaBits(const aAlphaBits: Integer);
    procedure SetDepthBits(const val: Integer);
    procedure SetStencilBits(const aStencilBits: Integer);
    procedure SetAccumBits(const aAccumBits: Integer);
    procedure SetAuxBuffers(const aAuxBuffers: Integer);
    procedure SetOptions(const aOptions: TDGLRCOptions);
    procedure SetAntiAliasing(const val: TDGLAntiAliasing);
//    procedure SetAcceleration(const val: TDGLContextAcceleration);
    function GetActive: Boolean;
    procedure SetActive(const aActive: Boolean);
    procedure SetLayer(const Value: TDGLContextLayer);

  protected
    FXGL: TAbstractMultitextureCoordinator;
    FGLStates: TDGLStateCache;
    FTransformation: TDGLTransformation;
    FLayer: TDGLContextLayer;
    FInitialized : Boolean;
    //MULTI-THREADED
    FSharedContexts: TThreadList;
    FLock: TCriticalSection;

    procedure PropagateSharedContext;

    procedure DoCreateContext(ADeviceHandle: HDC); virtual; abstract;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer = 1); virtual; abstract;

    procedure DoDestroyContext; virtual; abstract;

    function DoShareLists(aContext: TDGLContext): Boolean; virtual; abstract;

    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;

    //class function ServiceContext: TDGLContext;
    function GetXGL: TAbstractMultitextureCoordinator;

  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    { @HTML ( Creates the context.<p>
       This method must be invoked before the context can be used. }
    procedure CreateContext(ADeviceHandle: HDC); overload;

    { @HTML ( Creates an in-memory context.<p>
       The function should fail if no hardware-accelerated memory context
       can be created (the CreateContext method can handle software OpenGL
       contexts). </p> ) }
    procedure CreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer = 1);
    { @HTML ( Destroy the context.<p>
       Will fail if no context has been created.<br>
       The method will first invoke the OnDestroyContext
       event, then attempts to deactivate the context
       (if it is active) before destroying it. </p> ) }
    procedure DestroyContext;

    { @HTML ( Activates the context.<p>
       A context can be activated multiple times (and must be
       deactivated the same number of times), but this function
       will fail if another context is already active. </p> ) }
    procedure Activate;

    { @HTML ( Deactivates the context.<p>
       Will fail if the context is not active or another
       context has been activated. </p> ) }
    procedure Deactivate;

    { @HTML ( <p>Setup display list sharing between two rendering contexts.
       Both contexts must have the same pixel format. </p> ) }
    procedure ShareLists(aContext: TDGLContext);

    { @HTML ( <p>Call OnPrepare for all handles. </p> ) }
    procedure PrepareHandlesData;

    { @HTML ( Returns true if the context is valid.<p>
       A context is valid from the time it has been successfully
       created to the time of its destruction. </p> ) }
    function IsValid: Boolean; virtual; abstract;

    { @HTML ( <p>Request to swap front and back buffers if they were defined. </p> ) }
    procedure SwapBuffers; virtual; abstract;

    { @HTML ( <p>Returns the first compatible context that isn't self in the shares.</p> ) }
    function FindCompatibleContext: TDGLContext;

    procedure DestroyAllHandles;
    function RenderOutputDevice: Pointer; virtual;abstract;


    //---------------------------------------------------------------------------------------

    { @HTML ( <p>Color bits for the rendering context </p> ) }
    property ColorBits: Integer read FColorBits write SetColorBits;

    { @HTML (<p> Alpha bits for the rendering context </p> ) }
    property AlphaBits: Integer read FAlphaBits write SetAlphaBits;

    { @HTML (<p> Depth bits for the rendering context </p> ) }
    property DepthBits: Integer read FDepthBits write SetDepthBits;

    { @HTML (<p> Stencil bits for the rendering context  </p> )}
    property StencilBits: Integer read FStencilBits write SetStencilBits;

    { @HTML (<p> Accumulation buffer bits for the rendering context </p> ) }
    property AccumBits: Integer read FAccumBits write SetAccumBits;

    { @HTML (<p> Auxiliary buffers bits for the rendering context  </p> )}
    property AuxBuffers: Integer read FAuxBuffers write SetAuxBuffers;

    { @HTML ( AntiAliasing option.<p>
       Ignored if not hardware supported, currently based on ARB_multisample. </p> ) }
    property AntiAliasing: TDGLAntiAliasing read FAntiAliasing write SetAntiAliasing;

    { @HTML ( <p>Specifies the layer plane that the rendering context is bound to. </p> ) }
    property Layer: TDGLContextLayer read FLayer write SetLayer;

    { @HTML ( <p>Rendering context options. </p> ) }
    property Options: TDGLRCOptions read FOptions write SetOptions;

    { @HTML ( Allows reading and defining the activity for the context.<p>
       The methods of this property are just wrappers around calls
       to Activate and Deactivate. </p> ) }
    property Active: Boolean read GetActive write SetActive;

    { @HTML ( Triggered whenever the context is destroyed.<p>
       This events happens *before* the context has been
       actually destroyed, OpenGL resource cleanup can
       still occur here. </p> ) }
    property OnDestroyContext: TNotifyEvent read FOnDestroyContext write FOnDestroyContext;

    property IsPreparationNeed: Boolean read FIsPreparationNeed;
     //: Context manager reference
    property Manager: TDGLContextManager read FManager;
    property Initialized : Boolean read FInitialized;

    property GLStates: TDGLStateCache read FGLStates;
    property PipelineTransformation: TDGLTransformation read FTransformation;
    property MultitextureCoordinator: TAbstractMultitextureCoordinator read GetXGL;
  end;

  TDGLContextClass = class of TDGLContext;

  // ****************************************************************************************
  // TGLContextNotification
  //
  TDGLContextNotification = record
    obj: TObject;
    event: TNotifyEvent;
  end;

  // ****************************************************************************************
  // TGLContextManager
  //
  { @HTML ( <p>Stores and manages all the TGLContext objects. </p> ) }
  TDGLContextManager = class
  private
    { Private Declarations }
    FList: TThreadList;
    FTerminated: Boolean;
    FNotifications: array of TDGLContextNotification;
    FCreatedRCCount: Integer;

    FHandles: TThreadList;

   // FServiceContext: TDGLContext;
  protected
    { Protected Declarations }
    procedure Lock;
    procedure UnLock;

    procedure RegisterContext(aContext: TDGLContext);
    procedure UnRegisterContext(aContext: TDGLContext);

    procedure ContextCreatedBy(aContext: TDGLContext);
    procedure DestroyingContextBy(aContext: TDGLContext);

  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    { @HTML ( Returns an appropriate, ready-to use context.<p>
       The returned context should be freed by caller. </p> ) }
    function CreateContext(AClass: TDGLContextClass = nil): TDGLContext;

    { @HTML ( Returns the number of TGLContext object.<p>
       This is *not* the number of OpenGL rendering contexts! </p> ) }
    function ContextCount: Integer;
    { @HTML ( Registers a new object to notify when the last context is destroyed.<p>
       When the last rendering context is destroyed, the 'anEvent' will
       be invoked with 'anObject' as parameter.<br>
       Note that the registration is kept until the notification is triggered
       or a RemoveNotification on 'anObject' is issued. </p> ) }
    procedure LastContextDestroyNotification(anObject: TObject; anEvent: TNotifyEvent);
    { @HTML ( <p>Unregisters an object from the notification lists. </p> ) }
    procedure RemoveNotification(anObject: TObject);

    { @HTML ( <p> Marks the context manager for termination </p> ) }
    procedure Terminate;

    { @HTML ( <p>Request all contexts to destroy all their handles.</p> ) }
    procedure DestroyAllHandles;

    { @HTML ( <p>Notify all contexts about necessity of handles preparation.</p> ) }
    procedure NotifyPreparationNeed;
   // function RenderOutputDevice: Pointer; override;
    //property ServiceContext: TDGLContext read FServiceContext;
  end;

  // ****************************************************************************************
  // TGLContextHandle
  //
  { @HTML ( Wrapper around an OpenGL context handle.<p>
     This wrapper also takes care of context registrations and data releases
     related to context releases an cleanups. This is an abstract class,
     use the TGLListHandle and TGLTextureHandle subclasses.</p> ) }
  TDGLContextHandle = class
  private
    { Private Declarations }
    FHandles: TList;
    FLastHandle : PGLRCHandle;
    FOnPrepare: TOnPrepareHandleData;


    function SearchRC(AContext: TDGLContext): PGLRCHandle;
    function RCItem(AIndex: integer): PGLRCHandle; {$IFDEF GLS_INLINE}inline;{$ENDIF}
    procedure CheckCurrentRC;
  protected
    { Protected Declarations }
    //: Invoked by when there is no compatible context left for relocation
    procedure ContextDestroying;
    function GetContext: TDGLContext;
    function GetHandle: TGLuint;
    //: Specifies if the handle can be transfered across shared contexts
    class function Transferable: Boolean; virtual;
    class function IsValid(const ID: GLuint): Boolean; virtual;

    function DoAllocateHandle: Cardinal; virtual; abstract;
    procedure DoDestroyHandle(var AHandle: TGLuint); virtual; abstract;


  public
    { Public Declarations }
    constructor Create; virtual;
    constructor CreateAndAllocate(failIfAllocationFailed: Boolean = True);
    destructor Destroy; override;

    { @HTML ( <p>Return OpenGL identifier in current context. </p> )}
    property Handle: TGLuint read GetHandle;
    { @HTML ( <p>Return current rendering context if handle is allocated in it
       or first context where handle is allocated. </p> )}
    property RenderingContext: TDGLContext read GetContext;
    { @HTML ( <p>Return True is data need update in current context. </p> )}
    function IsDataNeedUpdate: Boolean;
    { @HTML ( <p>Return True if data updated in all contexts. </p> )}
    function IsDataComplitelyUpdated: Boolean;
    { @HTML ( <p>Notify the data was updated in current context. </p> )}
    procedure NotifyDataUpdated;
    { @HTML ( <p>Notify the data was changed through all context. </p> )}
    procedure NotifyChangesOfData;

    //: Checks if required extensions / OpenGL version are met
    class function IsSupported: Boolean; virtual;
    function IsAllocatedForContext(AContext: TDGLContext = nil): Boolean;
    function IsShared: Boolean;

    function  AllocateHandle: TGLuint;
    procedure DestroyHandle;

    property OnPrepare: TOnPrepareHandleData read FOnPrepare write FOnPrepare;
  end;

// ****************************************************************************************

{ @HTML ( <p>Drivers should register themselves via this function.</p> ) }
procedure RegisterDGLContextClass(aDGLContextClass: TDGLContextClass);

{ @HTML ( The TGLContext that is the currently active context, if any.<p>
   Returns nil if no context is active.</p> ) }
function CurrentDGLContext: TDGLContext;
function SafeCurrentDGLContext: TDGLContext;
function IsMainThread: Boolean;
//function IsServiceContextAvaible: Boolean;

procedure ClearOpenGLError;
procedure CheckOpenGLError;

var
  DGLContextManager: TDGLContextManager;
  vIgnoreOpenGLErrors: Boolean = False;
  vContextActivationFailureOccurred: Boolean = false;
  vMultitextureCoordinatorClass: TAbstractMultitextureCoordinatorClass;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vContextClasses: TList;
//  vServiceWindow: TForm;

threadvar
  vCurrentDGLContext: TDGLContext;
  vMainThread: Boolean;

// ------------------
// { Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

procedure RegisterDGLContextClass(aDGLContextClass: TDGLContextClass);
begin
  if not Assigned(vContextClasses) then
    vContextClasses := TList.Create;
  vContextClasses.Add(aDGLContextClass);
end;

function CurrentDGLContext: TDGLContext;
begin
  Result := vCurrentDGLContext;
end;

function SafeCurrentDGLContext: TDGLContext;
begin
  Result := CurrentDGLContext;
  if not Assigned(Result) then
  begin
   {$IFDEF DGLS_LOGGING}
    DGLSLogger.LogError(cNoActiveRC);
   {$ENDIF}
    Abort;
  end;
end;

function IsMainThread: Boolean;
begin
  Result := vMainThread;
end;

//function IsServiceContextAvaible: Boolean;
//begin
//  Result := DGLContextManager.ServiceContext <> nil;
//end;

// ------------------
// ------------------ Misc functions ------------------
// ------------------
procedure ClearOpenGLError;
var
  n: integer;
begin
  n := 0;
  while (glGetError <> GL_NO_ERROR) and (n < 6) do
    Inc(n);
end;

procedure CheckOpenGLError;
var
  glError: TGLuint;
  Count: word;
begin
    try
      glError := glGetError();
      if glError <> GL_NO_ERROR then
      begin
        Count := 0;
        try
          while (glGetError <> GL_NO_ERROR) and (Count < 6) do
            Inc(Count);
        except
        end;
        {$IFDEF GLS_LOGGING }
          case glError of
            GL_INVALID_ENUM:
              DGLSLogger.LogError(format(rstrOpenGLError, ['Invalid enum']));
            GL_INVALID_VALUE:
              DGLSLogger.LogError(format(rstrOpenGLError, ['Invalid value']));
            GL_INVALID_OPERATION:
              DGLSLogger.LogError(format(rstrOpenGLError, ['Invalid Operation']));
            GL_OUT_OF_MEMORY:
              DGLSLogger.LogError(format(rstrOpenGLError, ['Out of memory']));
          end;
        {$ENDIF}
      end;
    except
      DGLSLogger.LogError(format(rstrOpenGLError, ['Exception in glGetError']));
    end;
end;


{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TAbstractMultitextureCoordinator }
{$IFDEF GLS_REGION}{$REGION 'TAbstractMultitextureCoordinator'}{$ENDIF}

constructor TAbstractMultitextureCoordinator.Create(AOwner: TDGLContext);
begin
  FOwner := AOwner;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TDGLContext }
{$IFDEF GLS_REGION}{$REGION 'TDGLContext'}{$ENDIF}

constructor TDGLContext.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;

  FColorBits := 32;
  FStencilBits := 0;
  FAccumBits := 0;
  FAuxBuffers := 0;
  FLayer := clMainPlane;
  FOptions := [];

  FSharedContexts := TThreadList.Create;

  FSharedContexts.Add(Self);

  //FAcceleration := chaUnknown;
  FGLStates := TDGLStateCache.Create;
  //FGL := TGLExtensionsAndEntryPoints.Create;
  FTransformation := TDGLTransformation.Create;
  //FTransformation.LoadMatricesEnabled := True;

  DGLContextManager.RegisterContext(Self);

  FIsPreparationNeed := True;
end;

destructor TDGLContext.Destroy;
begin
  if IsValid then
    DestroyContext;

  DGLContextManager.UnRegisterContext(Self);

  //FGLStates.Free;
  //FGL.Free;
  //FXGL.Free;
  //FTransformation.Free;

  FSharedContexts.Free;

  FLock.Free;

  inherited Destroy;
end;

//class function TDGLContext.ServiceContext: TDGLContext;
//begin
//  Result := DGLContextManager.FServiceContext;
//end;

procedure TDGLContext.SetColorBits(const aColorBits: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FColorBits := aColorBits;
end;

procedure TDGLContext.SetAlphaBits(const aAlphaBits: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAlphaBits := aAlphaBits;
end;

procedure TDGLContext.SetDepthBits(const val: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FDepthBits := val;
end;

procedure TDGLContext.SetLayer(const Value: TDGLContextLayer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FLayer := Value;
end;

procedure TDGLContext.SetStencilBits(const aStencilBits: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FStencilBits := aStencilBits;
end;

procedure TDGLContext.SetAccumBits(const aAccumBits: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAccumBits := aAccumBits;
end;

procedure TDGLContext.SetAuxBuffers(const aAuxBuffers: Integer);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAuxBuffers := aAuxBuffers;
end;

procedure TDGLContext.SetOptions(const aOptions: TDGLRCOptions);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FOptions := aOptions;
end;

procedure TDGLContext.SetAntiAliasing(const val: TDGLAntiAliasing);
begin
  if Active then
    raise EDGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAntiAliasing := val;
end;

//procedure TDGLContext.SetAcceleration(const val: TDGLContextAcceleration);
//begin
//  if Active then
//    raise EDGLContext.Create(cCannotAlterAnActiveContext)
//  else
//    FAcceleration := val;
//end;

function TDGLContext.GetActive: Boolean;
begin
  Result := (FActivationCount > 0);
end;

procedure TDGLContext.SetActive(const aActive: Boolean);
begin
  // activation/deactivation can be nested...
  while aActive <> Active do
  begin
    if aActive then
      Activate
    else
      Deactivate;
  end;
end;

procedure TDGLContext.CreateContext(ADeviceHandle: HDC);
begin
  if IsValid then
    raise EDGLContext.Create(cContextAlreadyCreated);
  DoCreateContext(ADeviceHandle);
  Manager.ContextCreatedBy(Self);
end;

procedure TDGLContext.CreateMemoryContext(outputDevice: HWND;width, height: Integer; BufferCount: integer);
begin
  if IsValid then
    raise EDGLContext.Create(cContextAlreadyCreated);
  DoCreateMemoryContext(outputDevice, width, height, BufferCount);
  Manager.ContextCreatedBy(Self);
end;

procedure TDGLContext.PrepareHandlesData;
var
  I: Integer;
  LHandle: TDGLContextHandle;
begin
  if vCurrentDGLContext = Self then
  begin
    with Manager.FHandles.LockList do
      try
        for i := Count - 1 downto 0 do
        begin
          LHandle := TDGLContextHandle(Items[i]);
          if Assigned(LHandle.FOnPrepare) then
            LHandle.FOnPrepare(Self);
        end;
      finally
        Manager.FHandles.UnlockList;
      end;
    FIsPreparationNeed := False;
  end;
end;

procedure TDGLContext.PropagateSharedContext;
var
  i, j: Integer;
  otherContext: TDGLContext;
  otherList: TList;
begin
  with FSharedContexts.LockList do
    try
      for i := 1 to Count - 1 do
      begin
        otherContext := TDGLContext(Items[i]);
        otherList := otherContext.FSharedContexts.LockList;
        for J := 0 to otherList.Count - 1 do
          if IndexOf(otherList[J]) < 0 then
            Add(otherList[J]);
        otherContext.FSharedContexts.UnlockList;
      end;
      for i := 1 to Count - 1 do
      begin
        otherContext := TDGLContext(Items[i]);
        otherList := otherContext.FSharedContexts.LockList;
        if otherList.IndexOf(Self) < 0 then
          otherList.Add(Self);
        otherContext.FSharedContexts.UnlockList;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
end;

procedure TDGLContext.ShareLists(aContext: TDGLContext);
begin

  with FSharedContexts.LockList do
    try
      if IndexOf(aContext) < 0 then
      begin
        if DoShareLists(aContext) then
        begin
          Add(aContext);
          PropagateSharedContext;
        end;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
end;

procedure TDGLContext.DestroyAllHandles;
var
  i: Integer;
begin
  Activate;
  try

    with Manager.FHandles.LockList do
      try
        for i := Count - 1 downto 0 do
          TDGLContextHandle(Items[i]).ContextDestroying;
      finally
        Manager.FHandles.UnlockList;
      end;

  finally
    Deactivate;
  end;
end;

procedure TDGLContext.DestroyContext;
var
  I: Integer;
  oldContext, otherContext: TDGLContext;
  contextHandle: TDGLContextHandle;
  aList: TList;
begin

  if vCurrentDGLContext <> Self then
  begin
    oldContext := vCurrentDGLContext;
    if Assigned(oldContext) then
      oldContext.Deactivate;
  end
  else
    oldContext := nil;

  Activate;
  try

    aList := Manager.FHandles.LockList;
    try
      for i := aList.Count - 1 downto 0 do
      begin
        contextHandle := TDGLContextHandle(aList[i]);
        contextHandle.ContextDestroying;
      end;
    finally
      Manager.FHandles.UnlockList;
    end;

    Manager.DestroyingContextBy(Self);

    aList := FSharedContexts.LockList;

    for I := 1 to aList.Count - 1 do
    begin
      otherContext := TDGLContext(aList[I]);
      otherContext.FSharedContexts.Remove(Self);
    end;
    FSharedContexts.Clear;
    FSharedContexts.Add(Self);

    FSharedContexts.UnlockList;

    Active := False;
    DoDestroyContext;
  finally
    if Assigned(oldContext) then
      oldContext.Activate;
  end;

end;

procedure TDGLContext.Activate;
begin

  FLock.Enter;

  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EDGLContext.Create(cContextNotCreated);

    vContextActivationFailureOccurred := False;
    try
      DoActivate;
    except
      vContextActivationFailureOccurred := True;
    end;

    vCurrentDGLContext := Self;
  end
  else
    Assert(vCurrentDGLContext = Self, 'vCurrentDGLContext <> Self');
  Inc(FActivationCount);
end;

procedure TDGLContext.Deactivate;
begin
  Assert(vCurrentDGLContext = Self);
  Dec(FActivationCount);
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EDGLContext.Create(cContextNotCreated);
    if not vContextActivationFailureOccurred then
      DoDeactivate;
    vCurrentDGLContext := nil;

  end
  else if FActivationCount < 0 then
    raise EDGLContext.Create(cUnbalancedContexActivations);

  FLock.Leave;

end;

function TDGLContext.FindCompatibleContext: TDGLContext;
var
  i: Integer;
begin
  Result := nil;
  with FSharedContexts.LockList do
    try
      for i := 0 to Count - 1 do
        if TDGLContext(Items[i]) <> Self then
        begin
          Result := TDGLContext(Items[i]);
          Break;
        end;
    finally
      FSharedContexts.UnlockList;
    end;

end;

function TDGLContext.GetXGL: TAbstractMultitextureCoordinator;
begin
  if FXGL = nil then
    FXGL := vMultitextureCoordinatorClass.Create(Self);
  Result := FXGL;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TDGLContextManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLContextManager'}{$ENDIF}

constructor TDGLContextManager.Create;
begin
  inherited Create;
  FHandles := TThreadList.Create;
  FList := TThreadList.Create;
end;

destructor TDGLContextManager.Destroy;
begin
  FHandles.Free;
  FList.Free;
  inherited Destroy;
end;

function TDGLContextManager.CreateContext(AClass: TDGLContextClass): TDGLContext;
begin
  if Assigned(AClass) then
  begin
    Result := AClass.Create;
    Result.FManager := Self;
  end
  else if Assigned(vContextClasses) and (vContextClasses.Count > 0) then
  begin
    Result := TDGLContextClass(vContextClasses.Last).Create;
    Result.FManager := Self;
  end
  else
    Result := nil;
end;

procedure TDGLContextManager.Lock;
begin
  FList.LockList;
end;

procedure TDGLContextManager.NotifyPreparationNeed;
var
  I: Integer;
  LList: TList;
begin
  LList := FList.LockList;
  try
    for I := LList.Count - 1 downto 0 do
      TDGLContext(LList[I]).FIsPreparationNeed := True;
  finally
    FList.UnlockList;
  end;
end;

procedure TDGLContextManager.UnLock;
begin
  FList.UnlockList;
end;

function TDGLContextManager.ContextCount: Integer;
begin
  // try..finally just a waste of CPU here, if Count fails, the list is amok,
  // and so is the lock...
  Result := FList.LockList.Count;
  FList.UnLockList;
end;

procedure TDGLContextManager.RegisterContext(aContext: TDGLContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) >= 0 then
        raise EDGLContext.Create(cInvalidContextRegistration)
      else
        Add(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TDGLContextManager.UnRegisterContext(aContext: TDGLContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) < 0 then
        raise EDGLContext.Create(cInvalidContextRegistration)
      else
        Remove(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TDGLContextManager.ContextCreatedBy(aContext: TDGLContext);
begin
  Lock;
  try
    Inc(FCreatedRCCount);
  finally
    UnLock;
  end;
end;

procedure TDGLContextManager.DestroyingContextBy(aContext: TDGLContext);
var
  cn: TDGLContextNotification;
begin
  Lock;
  try
    Dec(FCreatedRCCount);
    if FCreatedRCCount = 0 then
    begin
      // yes, slow and bulky, but allows for the triggered event to
      // cascade-remove notifications safely
      while Length(FNotifications) > 0 do
      begin
        cn := FNotifications[High(FNotifications)];
        SetLength(FNotifications, Length(FNotifications) - 1);
        cn.event(cn.obj);
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TDGLContextManager.LastContextDestroyNotification(
  anObject: TObject; anEvent: TNotifyEvent);
begin
  Lock;
  try
    SetLength(FNotifications, Length(FNotifications) + 1);
    with FNotifications[High(FNotifications)] do
    begin
      obj := anObject;
      event := anEvent;
    end;
  finally
    UnLock;
  end;
end;

procedure TDGLContextManager.RemoveNotification(anObject: TObject);
var
  i: Integer;
  found: Boolean;
begin
  Lock;
  try
    found := False;
    i := Low(FNotifications);
    while i <= High(FNotifications) do
    begin
      if FNotifications[i].obj = anObject then
      begin
        found := True;
        while i <= High(FNotifications) do
        begin
          FNotifications[i] := FNotifications[i + 1];
          Inc(i);
        end;
        SetLength(FNotifications, Length(FNotifications) - 1);
        Break;
      end;
      Inc(i);
    end;
    if not found then
      raise EDGLContext.Create(cInvalidNotificationRemoval);
  finally
    UnLock;
  end;
end;

procedure TDGLContextManager.Terminate;
begin
  FTerminated := True;

  if ContextCount = 0 then
  begin
    DGLContextManager := nil;
    Free;
  end;
end;

procedure TDGLContextManager.DestroyAllHandles;
var
  i: Integer;
begin
  with FList.LockList do
    try
      for i := Count - 1 downto 0 do
        TDGLContext(Items[i]).DestroyAllHandles;
    finally
      FList.UnLockList;
    end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TDGLContextHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLContextHandle'}{$ENDIF}

constructor TDGLContextHandle.Create;
begin
  inherited Create;
  FHandles := TList.Create;
  //first is a dummy record
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  DGLContextManager.FHandles.Add(Self);
end;

constructor TDGLContextHandle.CreateAndAllocate(failIfAllocationFailed: Boolean =
  True);
begin
  Create;
  AllocateHandle;
  if failIfAllocationFailed and (Handle = 0) then
    raise EDGLContext.Create('Auto-allocation failed');
end;

destructor TDGLContextHandle.Destroy;
var
  i : integer;
begin
  DestroyHandle;
  for i := 0 to FHandles.Count-1 do
    Dispose(RCItem(i));
  FHandles.Free;
  if Assigned(DGLContextManager) then
    DGLContextManager.FHandles.Remove(Self);
  inherited Destroy;
end;

function TDGLContextHandle.AllocateHandle: TGLuint;
var
  I: Integer;
  bSucces: Boolean;
  aList: TList;
  p : PGLRCHandle;

begin
  // if handle aready allocated in current context
  Result := GetHandle;
  if Result <> 0 then
    exit;

  if vCurrentDGLContext = nil then
  begin
    DGLSLogger.LogError('Failed to allocate OpenGL identifier - no active rendering context!');
    exit;
  end;

  //add entry
  New(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  FLastHandle.FRenderingContext := vCurrentDGLContext;

  bSucces := False;
  if Transferable then
  begin

    aList := vCurrentDGLContext.FSharedContexts.LockList;
    try

      for I := aList.Count - 1 downto 0 do
      begin
        P := SearchRC(aList[I]);
        if (P.FHandle > 0) then
        begin
          // Copy shared handle
          FLastHandle.FRenderingContext := vCurrentDGLContext;
          FLastHandle.FHandle           := P.FHandle;
          FLastHandle.FChanged          := P.FChanged;
          Inc(vCurrentDGLContext.FOwnedHandlesCount);
          bSucces := True;
          break;
        end;
      end;
    finally
      vCurrentDGLContext.FSharedContexts.UnlockList;
    end;
  end;

  if not bSucces then
  begin
    // Allocate handle in current context
    FLastHandle.FHandle := DoAllocateHandle;
    bSucces := FLastHandle.FHandle <> 0;
    FLastHandle.FChanged := bSucces;
    if bSucces then
      Inc(vCurrentDGLContext.FOwnedHandlesCount);
  end;

  Result := FLastHandle.FHandle;
  if not bSucces then
    DGLSLogger.LogError(cNoActiveRC)
  else if Assigned(FOnPrepare) then
    DGLContextManager.NotifyPreparationNeed;
end;

function TDGLContextHandle.IsAllocatedForContext(AContext: TDGLContext = nil): Boolean;
begin
  Result := SearchRC(AContext).FHandle > 0;
end;

function TDGLContextHandle.SearchRC(AContext: TDGLContext): PGLRCHandle;
var
  i : integer;
begin
  if AContext = nil then
    AContext := vCurrentDGLContext;

  if AContext = FLastHandle.FRenderingContext then
  begin
    Result := FLastHandle;
    exit;
  end;

  for i := 1 to FHandles.Count-1 do
    if RCItem(i).FRenderingContext = AContext then
    begin
      Result := RCItem(i);
      exit;
    end;

  //first handle is always a dummy
  Result := FHandles[0];
end;

procedure TDGLContextHandle.CheckCurrentRC;
begin
  if vCurrentDGLContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentDGLContext);
end;

function TDGLContextHandle.GetHandle: TGLuint;
begin
 //CheckCurrentRC;
//inline doesn't always work... so optimize it here
  if vCurrentDGLContext <> FLastHandle.FRenderingContext then FLastHandle := SearchRC(vCurrentDGLContext);
  Result := FLastHandle.FHandle;
end;

procedure TDGLContextHandle.DestroyHandle;
var
  oldContext: TDGLContext;
  P : PGLRCHandle;
  I: Integer;
begin
  oldContext := vCurrentDGLContext;
  if Assigned(oldContext) then
    oldContext.Deactivate;
  try
    for I := FHandles.Count-1 downto 1 do
    begin
      P := FHandles[I];
      if P.FHandle > 0 then
      begin
        P.FRenderingContext.Activate;
        if IsValid(P.FHandle) then
          DoDestroyHandle(P.FHandle);
        Dec(P.FRenderingContext.FOwnedHandlesCount);
        P.FRenderingContext.Deactivate;
        P.FRenderingContext := nil;
        P.FHandle := 0;
        P.FChanged := True;
      end;
      Dispose(P);
    end;
    FHandles.Count := 1; //delete all in 1 step
    FLastHandle := FHandles[0];
  finally
    if Assigned(vCurrentDGLContext) then
      vCurrentDGLContext.Deactivate;
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
end;

procedure TDGLContextHandle.ContextDestroying;
var
  I: Integer;
  P: PGLRCHandle;
  aList: TList;
  bShared: Boolean;
begin
  if Assigned(vCurrentDGLContext) then
  begin
    bShared := False;
    if Transferable then
    begin

      aList := vCurrentDGLContext.FSharedContexts.LockList;
      try
        for I := FHandles.Count-1 downto 1 do
        begin
          P := RCItem(I);
          if (P.FRenderingContext <> vCurrentDGLContext)
            and (P.FHandle <> 0)
            and (aList.IndexOf(P.FRenderingContext) > -1) then
            begin
              bShared := True;
              break;
            end;
        end;

      finally
        vCurrentDGLContext.FSharedContexts.UnLockList;
      end;

    end;

    for I := FHandles.Count-1 downto 1 do
    begin
      P := RCItem(I);
      if (P.FRenderingContext = vCurrentDGLContext) and (P.FHandle <> 0) then
      begin
        if not bShared then
          if IsValid(P.FHandle) then
            DoDestroyHandle(P.FHandle);
        Dec(P.FRenderingContext.FOwnedHandlesCount);
        P.FHandle := 0;
        P.FRenderingContext := nil;
        P.FChanged := True;
        Dispose(P);
        FHandles.Delete(I);
        if FLastHandle = P then
          FLastHandle := FHandles[0];
        exit;
      end;
    end;
  end;
end;

function TDGLContextHandle.GetContext: TDGLContext;
var
  I: Integer;
  P: PGLRCHandle;
begin
  Result := nil;
  // Return first context where handle is allocated
  for I := FHandles.Count-1 downto 1 do
  begin
    P := RCItem(I);
    if (P.FRenderingContext <> nil) and (P.FHandle <> 0) then
    begin
      Result := P.FRenderingContext;
      // If handle allocated in active context - return it
      if (Result = vCurrentDGLContext) then exit;
    end;
  end;
end;

function TDGLContextHandle.IsDataNeedUpdate: Boolean;
begin
  if GetHandle = 0 then
    CheckCurrentRC;
  Result := (FLastHandle.FHandle = 0) or FLastHandle.FChanged;
end;

function TDGLContextHandle.IsDataComplitelyUpdated: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := FHandles.Count-1 downto 1 do
  begin
    with RCItem(i)^ do
      if (FRenderingContext <> nil) and (FHandle <> 0) and FChanged then exit;
  end;
  Result := true;
end;

procedure TDGLContextHandle.NotifyDataUpdated;
var
  I: Integer;
  aList: TList;
begin
  if Assigned(vCurrentDGLContext) then
  begin
    if not Transferable then
    begin
      CheckCurrentRC();
      if FLastHandle.FHandle <> 0 then
      begin
        FLastHandle.FChanged := False;
        exit;
      end;
    end
    else
    begin

      aList := vCurrentDGLContext.FSharedContexts.LockList;
      try

        for I := 0 to aList.Count - 1 do
        begin
          with SearchRC(aList[I])^ do
            if (FHandle <> 0) then
              FChanged := False;
        end;

      finally
        vCurrentDGLContext.FSharedContexts.UnlockList;
      end;

    end;
  end
  else
    DGLSLogger.LogError(cNoActiveRC);
end;

function TDGLContextHandle.RCItem(AIndex: integer): PGLRCHandle;
begin
  Result := FHandles[AIndex];
end;

procedure TDGLContextHandle.NotifyChangesOfData;
var
  I: Integer;
begin
  for I := FHandles.Count-1 downto 1 do
    RCItem(I).FChanged := True;
  if Assigned(FOnPrepare) then
    DGLContextManager.NotifyPreparationNeed;
end;

function TDGLContextHandle.IsShared: Boolean;
var
  I: Integer;
  vContext: TDGLContext;
  aList: TList;
begin
  Result := False;
  // untransferable handles can't be shared
  if not Transferable then
    exit;
  Result := True;

  aList := vCurrentDGLContext.FSharedContexts.LockList;
  try
    for I := 0 to aList.Count - 1 do
    begin
      vContext := aList[I];
      if (vContext <> vCurrentDGLContext) and
        // at least one context is friendly
        (SearchRC(vContext).FHandle <> 0) then
        exit;
    end;

  finally
    vCurrentDGLContext.FSharedContexts.UnlockList;
  end;

  Result := false;
end;

class function TDGLContextHandle.Transferable: Boolean;
begin
  Result := True;
end;

class function TDGLContextHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := True;
end;

class function TDGLContextHandle.IsSupported: Boolean;
begin
  Result := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

  vMainThread := True;
  DGLContextManager := TDGLContextManager.Create;


finalization

  DGLContextManager.Terminate;
  vContextClasses.Free;
  vContextClasses := nil;

end.
