{: GLContext<p>

   Prototypes and base implementation of TGLContext.<p>
   Currently NOT thread-safe.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>30/01/02 - EG - Added TGLVirtualHandle
      <li>29/01/02 - EG - Improved recovery for context creation failures
      <li>28/01/02 - EG - Activation failures always ignored
      <li>21/01/02 - EG - Activation failures now ignored if application is
                          terminating (workaround for some weird ICDs)
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Added TGLContextAcceleration
      <li>06/09/01 - EG - Win32Context moved to new GLWin32Context unit
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLContext;

interface

uses Classes, SysUtils;

type

   // TGLRCOptions
   //
   TGLRCOption = ( rcoDoubleBuffered, rcoStereo );
   TGLRCOptions = set of TGLRCOption;

   TGLContextManager = class;

   // TGLContextAcceleration
   //
   TGLContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

   // TGLAntiAliasing
   //
   TGLAntiAliasing = (aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ);

   // TGLContext
   //
   {: Wrapper around an OpenGL rendering context.<p>
      The aim of this class is to offer platform-independant
      initialization, activation and management of OpenGL
      rendering context. The class also offers notifications
      event and error/problems detection.<br>
      This is a virtual abstract a class, and platform-specific
      subclasses must be used.<br>
      All rendering context share the same lists. }
   TGLContext = class
      private
         { Private Declarations }
         FColorBits, FAlphaBits : Integer;
         FStencilBits : Integer;
         FAccumBits : Integer;
         FAuxBuffers : Integer;
         FAntiAliasing : TGLAntiAliasing;
         FOptions : TGLRCOptions;
         FOnDestroyContext : TNotifyEvent;
         FManager : TGLContextManager;
         FActivationCount : Integer;
         FSharedContexts : TList;
         FOwnedHandles : TList;

      protected
         { Protected Declarations }
         FAcceleration : TGLContextAcceleration;

         procedure SetColorBits(const aColorBits : Integer);
         procedure SetAlphaBits(const aAlphaBits : Integer);
         procedure SetStencilBits(const aStencilBits : Integer);
         procedure SetAccumBits(const aAccumBits : Integer);
         procedure SetAuxBuffers(const aAuxBuffers : Integer);
         procedure SetOptions(const aOptions : TGLRCOptions);
         procedure SetAntiAliasing(const val : TGLAntiAliasing);
         function  GetActive : Boolean;
         procedure SetActive(const aActive : Boolean);
         procedure PropagateSharedContext;

         procedure DoCreateContext(outputDevice : Integer); dynamic; abstract;
         procedure DoCreateMemoryContext(outputDevice, width, height : Integer); dynamic; abstract;
         procedure DoShareLists(aContext : TGLContext); dynamic; abstract;
         procedure DoDestroyContext; dynamic; abstract;
         procedure DoActivate; virtual; abstract;
         procedure DoDeactivate; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         //: Context manager reference
         property Manager : TGLContextManager read FManager;

         {: Color bits for the rendering context }
         property ColorBits : Integer read FColorBits write SetColorBits;
         {: Alpha bits for the rendering context }
         property AlphaBits : Integer read FAlphaBits write SetAlphaBits;
         {: Stencil bits for the rendering context }
         property StencilBits : Integer read FStencilBits write SetStencilBits;
         {: Accumulation buffer bits for the rendering context }
         property AccumBits : Integer read FAccumBits write SetAccumBits;
         {: Auxiliary buffers bits for the rendering context }
         property AuxBuffers : Integer read FAuxBuffers write SetAuxBuffers;
         {: AntiAliasing option.<p>
            Ignored if not hardware supported, currently based on ARB_multisample. }
         property AntiAliasing : TGLAntiAliasing read FAntiAliasing write SetAntiAliasing;
         {: Rendering context options. }
         property Options : TGLRCOptions read FOptions write SetOptions;
         {: Allows reading and defining the activity for the context.<p>
            The methods of this property are just wrappers around calls
            to Activate and Deactivate. }
         property Active : Boolean read GetActive write SetActive;
         {: Indicates if the context is hardware-accelerated. }
         property Acceleration : TGLContextAcceleration read FAcceleration;
         {: Triggered whenever the context is destroyed.<p>
            This events happens *before* the context has been
            actually destroyed, OpenGL resource cleanup can
            still occur here. }
         property OnDestroyContext : TNotifyEvent read FOnDestroyContext write FOnDestroyContext;

         {: Creates the context.<p>
            This method must be invoked before the context can be used. }
         procedure CreateContext(outputDevice : Integer);
         {: Creates an in-memory context.<p>
            The function should fail if no hardware-accelerated memory context
            can be created (the CreateContext method can handle software OpenGL
            contexts). }
         procedure CreateMemoryContext(outputDevice, width, height : Integer);
         {: Setup display list sharing between two rendering contexts.<p>
            Both contexts must have the same pixel format. }
         procedure ShareLists(aContext : TGLContext);
         {: Destroy the context.<p>
            Will fail if no context has been created.<br>
            The method will first invoke the OnDestroyContext
            event, then attempts to deactivate the context
            (if it is active) before destroying it. }
         procedure DestroyContext;
         {: Activates the context.<p>
            A context can be activated multiple times (and must be
            deactivated the same number of times), but this function
            will fail if another context is already active. }
         procedure Activate;
         {: Deactivates the context.<p>
            Will fail if the context is not active or another
            context has been activated. }
         procedure Deactivate;
         {: Returns true if the context is valid.<p>
            A context is valid from the time it has been successfully
            created to the time of its destruction. }
         function IsValid : Boolean; virtual; abstract;
         {: Request to swap front and back buffers if they were defined. }
         procedure SwapBuffers; virtual; abstract;

         {: Returns the first compatible context that isn't self in the shares. }
         function FindCompatibleContext : TGLContext;
         procedure DestroyAllHandles;
   end;

   TGLContextClass = class of TGLContext;

   // TGLScreenControlingContext
   //
   {: A TGLContext with screen control property and methods.<p>
      This variety of contexts is for drivers that access windows and OpenGL
      through an intermediate opaque cross-platform API.<p>
      TGLSceneViewer won't use them, TGLMemoryViewer may be able to use them,
      but most of the time they will be accessed through a specific viewer
      class/subclass. } 
   TGLScreenControlingContext = class (TGLContext)
      private
         { Private Declarations }
         FWidth, FHeight : Integer;
         FFullScreen : Boolean;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         property Width : Integer read FWidth write FWidth;
         property Height : Integer read FHeight write FHeight;
         property FullScreen : Boolean read FFullScreen write FFullScreen;
   end;

   // TGLContextHandle
   //
   {: Wrapper around an OpenGL context handle.<p>
      This wrapper also takes care of context registrations and data releases
      related to context releases an cleanups. This is an abstract class,
      use the TGLListHandle and TGLTextureHandle subclasses. }
   TGLContextHandle = class
      private
         { Private Declarations }
         FRenderingContext : TGLContext;
         FHandle : Integer;

      protected
         { Protected Declarations }
         //: Invoked by when there is no compatible context left for relocation
         procedure ContextDestroying;

         function DoAllocateHandle : Integer; virtual; abstract;
         procedure DoDestroyHandle; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         property Handle : Integer read FHandle;
         property RenderingContext : TGLContext read FRenderingContext;

         procedure AllocateHandle;
         procedure DestroyHandle;
   end;

   TGLVirtualHandle = class;
   TGLVirtualHandleEvent = procedure (sender : TGLVirtualHandle; var handle : Integer) of object;

   // TGLVirtualHandle
   //
   TGLVirtualHandle = class (TGLContextHandle)
      private
         { Private Declarations }
         FOnAllocate, FOnDestroy : TGLVirtualHandleEvent;
         FTag : Integer;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         property OnAllocate : TGLVirtualHandleEvent read FOnAllocate write FOnAllocate;
         property OnDestroy : TGLVirtualHandleEvent read FOnDestroy write FOnDestroy;

         property Tag : Integer read FTag write FTag;
   end;

   // TGLListHandle
   //
   TGLListHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
   end;

   // TGLTextureHandle
   //
   TGLTextureHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
   end;

   // TGLContextNotification
   //
   TGLContextNotification = record
      obj : TObject;
      event : TNotifyEvent;
   end;

   // TGLContextManager
   //
   {: Stores and manages all the TGLContext objects.<p> }
   TGLContextManager = class
      private
         { Private Declarations }
         FList : TThreadList;
         FTerminated : Boolean;
         FNotifications : array of TGLContextNotification;
         FCreatedRCCount : Integer;

      protected
         { Protected Declarations }
         procedure Lock;
         procedure UnLock;

         procedure RegisterContext(aContext : TGLContext);
         procedure UnRegisterContext(aContext : TGLContext);

         procedure ContextCreatedBy(aContext : TGLContext);
         procedure DestroyingContextBy(aContext : TGLContext);

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         {: Returns an appropriate, ready-to use context.<p>
            The returned context should be freed by caller. }
         function CreateContext : TGLContext;

         {: Returns the number of TGLContext object.<p>
            This is *not* the number of OpenGL rendering contexts! }
         function ContextCount : Integer;
         {: Registers a new object to notify when the last context is destroyed.<p>
            When the last rendering context is destroyed, the 'anEvent' will
            be invoked with 'anObject' as parameter.<br>
            Note that the registration is kept until the notification is triggered
            or a RemoveNotification on 'anObject' is issued. }
         procedure LastContextDestroyNotification(anObject : TObject; anEvent : TNotifyEvent);
         {: Unregisters an object from the notification lists.<p> }
         procedure RemoveNotification(anObject : TObject);

         //: Marks the context manager for termination
         procedure Terminate;

         {: Request all contexts to destroy all their handles. }
         procedure DestroyAllHandles;
   end;

   EGLContext = class (Exception)
   end;

   EOpenGLError = class(Exception);

{: Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);

function CurrentGLContext : TGLContext;

procedure CheckOpenGLError;
procedure ClearGLError;

var
   GLContextManager : TGLContextManager;
   vIgnoreOpenGLErrors : Boolean = False;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL12, GLCrossPlatform;

resourcestring
   cCannotAlterAnActiveContext = 'Cannot alter an active context';
   cInvalidContextRegistration = 'Invalid context registration';
   cInvalidNotificationRemoval = 'Invalid notification removal';
   cContextAlreadyCreated =      'Context already created';
   cContextNotCreated =          'Context not created';
   cUnbalancedContexActivations= 'Unbalanced context activations';

var
   vContextClasses : TList = nil;
   vIgnoreContextActivationFailures : Boolean = False;

threadvar
   vCurrentGLContext : TGLContext;

// CurrentGLContext
//
function CurrentGLContext : TGLContext;
begin
   Result:=vCurrentGLContext;
end;

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError : LongWord;
	Count : Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
      if not vIgnoreOpenGLErrors then
   		raise EOpenGLError.Create(gluErrorString(GLError));
	end;
end;

// ClearGLError
//
procedure ClearGLError;
var
   n : Integer;
begin
   n:=0;
   while (glGetError<>GL_NO_ERROR) and (n<6) do Inc(n);
end;

// RegisterGLContextClass
//
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);
begin
   if not Assigned(vContextClasses) then
      vContextClasses:=TList.Create;
   vContextClasses.Add(aGLContextClass);
end;

// ------------------
// ------------------ TGLContext ------------------
// ------------------

// Create
//
constructor TGLContext.Create;
begin
   inherited Create;
   FColorBits:=32;
   FStencilBits:=0;
   FAccumBits:=0;
   FAuxBuffers:=0;
   FOptions:=[];
   FSharedContexts:=TList.Create;
   FOwnedHandles:=TList.Create;
   FAcceleration:=chaUnknown;
   GLContextManager.RegisterContext(Self);
end;

// Destroy
//
destructor TGLContext.Destroy;
begin
   if IsValid then
      DestroyContext;
   GLContextManager.UnRegisterContext(Self);
   FOwnedHandles.Free;
   FSharedContexts.Free;
   inherited Destroy;
end;

// SetColorBits
//
procedure TGLContext.SetColorBits(const aColorBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FColorBits:=aColorBits;
end;

// SetAlphaBits
//
procedure TGLContext.SetAlphaBits(const aAlphaBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAlphaBits:=aAlphaBits;
end;

// SetStencilBits
//
procedure TGLContext.SetStencilBits(const aStencilBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FStencilBits:=aStencilBits;
end;

// SetAccumBits
//
procedure TGLContext.SetAccumBits(const aAccumBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAccumBits:=aAccumBits;
end;

// SetAuxBuffers
//
procedure TGLContext.SetAuxBuffers(const aAuxBuffers : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAuxBuffers:=aAuxBuffers;
end;

// SetOptions
//
procedure TGLContext.SetOptions(const aOptions : TGLRCOptions);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FOptions:=aOptions;
end;

// SetAntiAliasing
//
procedure TGLContext.SetAntiAliasing(const val : TGLAntiAliasing);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAntiAliasing:=val;
end;

// GetActive
//
function TGLContext.GetActive : Boolean;
begin
   Result:=(FActivationCount>0);
end;

// SetActive
//
procedure TGLContext.SetActive(const aActive : Boolean);
begin
   // activation/deactivation can be nested...
   while aActive<>Active do begin
      if aActive then
         Activate
      else Deactivate;
   end;
end;

// CreateContext
//
procedure TGLContext.CreateContext(outputDevice : Integer);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateContext(outputDevice);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// CreateMemoryContext
//
procedure TGLContext.CreateMemoryContext(outputDevice, width, height : Integer);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateMemoryContext(outputDevice, width, height);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// PropagateSharedContext
//
procedure TGLContext.PropagateSharedContext;
var
   i, j : Integer;
   otherContext : TGLContext;
begin
   for i:=0 to FSharedContexts.Count-1 do begin
      if TGLContext(FSharedContexts[i])<>Self then begin
         otherContext:=TGLContext(FSharedContexts[i]);
         otherContext.FSharedContexts.Clear;
         for j:=0 to FSharedContexts.Count-1 do
            otherContext.FSharedContexts.Add(FSharedContexts[j]);
      end;
   end;
end;

// ShareLists
//
procedure TGLContext.ShareLists(aContext : TGLContext);
begin
   if IsValid then begin
      DoShareLists(aContext);
      if FSharedContexts.IndexOf(aContext)<0 then begin
         FSharedContexts.Add(aContext);
         PropagateSharedContext;
      end;
   end else raise EGLContext.Create(cContextNotCreated);
end;

// DestroyAllHandles
//
procedure TGLContext.DestroyAllHandles;
var
   i : Integer;
begin
   Activate;
   try
      for i:=FOwnedHandles.Count-1 downto 0 do
         TGLContextHandle(FOwnedHandles[i]).DestroyHandle;
   finally
      Deactivate;
   end;
end;

// DestroyContext
//
procedure TGLContext.DestroyContext;
var
   i : Integer;
   oldContext, compatContext : TGLContext;
begin
   if vCurrentGLContext<>Self then begin
      oldContext:=vCurrentGLContext;
      if Assigned(oldContext) then
         oldContext.Deactivate;
   end else oldContext:=nil;
   Activate;
   try
      compatContext:=FindCompatibleContext;
      if Assigned(compatContext) then begin
         // transfer handle ownerships to the compat context
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            compatContext.FOwnedHandles.Add(FOwnedHandles[i]);
            TGLContextHandle(FOwnedHandles[i]).FRenderingContext:=compatContext;
         end;
      end else begin
         // no compat context, release handles
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            with TGLContextHandle(FOwnedHandles[i]) do begin
               DoDestroyHandle;
               FHandle:=0;
               FRenderingContext:=nil;
            end;
         end;
      end;
      FOwnedHandles.Clear;
      Manager.DestroyingContextBy(Self);
      FSharedContexts.Remove(Self);
      PropagateSharedContext;
      FSharedContexts.Clear;
      Active:=False;
      DoDestroyContext;
   finally
      if Assigned(oldContext) then
         oldContext.Activate;
   end;
   vIgnoreContextActivationFailures:=False;
   FAcceleration:=chaUnknown;
end;

// Activate
//
procedure TGLContext.Activate;
begin
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      try
         DoActivate;
      except
//         if True then // ApplicationTerminated?
//            vIgnoreContextActivationFailures:=True
//         else
         raise;
      end;
      vCurrentGLContext:=Self;
   end else Assert(vCurrentGLContext=Self);
   Inc(FActivationCount);
end;

// Deactivate
//
procedure TGLContext.Deactivate;
begin
   Assert(vCurrentGLContext=Self);
   Dec(FActivationCount);
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      if not vIgnoreContextActivationFailures then
         DoDeactivate;
      vCurrentGLContext:=nil;
   end else if FActivationCount<0 then
      raise EGLContext.Create(cUnbalancedContexActivations);
end;

// FindCompatibleContext
//
function TGLContext.FindCompatibleContext : TGLContext;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to FSharedContexts.Count-1 do
      if FSharedContexts[i]<>Self then begin
         Result:=TGLContext(FSharedContexts[i]);
         Break;
      end;
end;

// ------------------
// ------------------ TGLContextHandle ------------------
// ------------------

// Create
//
constructor TGLContextHandle.Create;
begin
   inherited Create;
end;

// Destroy
//
destructor TGLContextHandle.Destroy;
begin
   DestroyHandle;
   inherited Destroy;
end;

// AllocateHandle
//
procedure TGLContextHandle.AllocateHandle;
begin
   Assert(FHandle=0);
   Assert(vCurrentGLContext<>nil);
   FHandle:=DoAllocateHandle;
   if FHandle<>0 then begin
      FRenderingContext:=vCurrentGLContext;
      vCurrentGLContext.FOwnedHandles.Add(Self);
   end;
end;

// DestroyHandle
//
procedure TGLContextHandle.DestroyHandle;
var
   oldContext, handleContext : TGLContext;
begin
   if FHandle<>0 then begin
      FRenderingContext.FOwnedHandles.Remove(Self);
      if (vCurrentGLContext=FRenderingContext)
            or ((vCurrentGLContext<>nil)
                and (vCurrentGLContext.FSharedContexts.IndexOf(FRenderingContext)>=0)) then begin
         // current context is ours or compatible one
         DoDestroyHandle;
         FHandle:=0;
         FRenderingContext:=nil;
      end else begin
         // some other context (or none)
         oldContext:=vCurrentGLContext;
         if Assigned(oldContext) then
            oldContext.Deactivate;
         FRenderingContext.Activate;
         handleContext:=FRenderingContext;
         try
            DoDestroyHandle;
            FHandle:=0;
            FRenderingContext:=nil;
         finally
            handleContext.Deactivate;
            if Assigned(oldContext) then
               oldContext.Activate;
         end;
      end;
   end;
end;

// ContextDestroying
//
procedure TGLContextHandle.ContextDestroying;
begin
   if FHandle<>0 then begin
      // we are always in the original context or a compatible context
      DoDestroyHandle;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;

// ------------------
// ------------------ TGLVirtualHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLVirtualHandle.DoAllocateHandle : Integer;
begin
   Result:=0;
   if Assigned(FOnAllocate) then
      FOnAllocate(Self, Result);
end;

// DoDestroyHandle
//
procedure TGLVirtualHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      ClearGLError;
      // delete
      if Assigned(FOnDestroy) then
         FOnDestroy(Self, FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLListHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLListHandle.DoAllocateHandle : Integer;
begin
   Result:=glGenLists(1);
end;

// DoDestroyHandle
//
procedure TGLListHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      ClearGLError;
      // delete
      glDeleteLists(FHandle, 1);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLTextureHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLTextureHandle.DoAllocateHandle : Integer;
begin
   glGenTextures(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLTextureHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      glGetError;
      // delete
 	   glDeleteTextures(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLContextManager ------------------
// ------------------

// Create
//
constructor TGLContextManager.Create;
begin
   inherited Create;
   FList:=TThreadList.Create;
end;

// Destroy
//
destructor TGLContextManager.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

// CreateContext
//
function TGLContextManager.CreateContext : TGLContext;
begin
   if Assigned(vContextClasses) and (vContextClasses.Count>0) then begin
      Result:=TGLContextClass(vContextClasses[0]).Create;
      Result.FManager:=Self;
   end else Result:=nil;
end;

// Lock
//
procedure TGLContextManager.Lock;
begin
   FList.LockList;
end;

// UnLock
//
procedure TGLContextManager.UnLock;
begin
   FList.UnlockList;
end;

// ContextCount
//
function TGLContextManager.ContextCount : Integer;
begin
   // try..finally just a waste of CPU here, if Count fails, the list is amok,
   // and so is the lock...
   Result:=FList.LockList.Count;
   FList.UnLockList;
end;

// RegisterContext
//
procedure TGLContextManager.RegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)>=0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Add(aContext);
   finally
      FList.UnlockList;
   end;
end;

// UnRegisterContext
//
procedure TGLContextManager.UnRegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)<0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Remove(aContext);
   finally
      FList.UnlockList;
   end;
end;

// ContextCreatedBy
//
procedure TGLContextManager.ContextCreatedBy(aContext : TGLContext);
begin
   Lock;
   try
      Inc(FCreatedRCCount);
   finally
      UnLock;
   end;
end;

// DestroyingContextBy
//
procedure TGLContextManager.DestroyingContextBy(aContext : TGLContext);
var
   cn : TGLContextNotification;
begin
   Lock;
   try
      Dec(FCreatedRCCount);
      if FCreatedRCCount=0 then begin
         // yes, slow and bulky, but allows for the triggered event to
         // cascade-remove notifications safely
         while Length(FNotifications)>0 do begin
            cn:=FNotifications[High(FNotifications)];
            SetLength(FNotifications, Length(FNotifications)-1);
            cn.event(cn.obj);
         end;
      end;
   finally
      UnLock;
   end;
end;

// LastContextDestroyNotification
//
procedure TGLContextManager.LastContextDestroyNotification(
                                    anObject : TObject; anEvent : TNotifyEvent);
begin
   Lock;
   try
      SetLength(FNotifications, Length(FNotifications)+1);
      with FNotifications[High(FNotifications)] do begin
         obj:=anObject;
         event:=anEvent;
      end;
   finally
      UnLock;
   end;
end;

// RemoveNotification
//
procedure TGLContextManager.RemoveNotification(anObject : TObject);
var
   i : Integer;
   found : Boolean;
begin
   Lock;
   try
      found:=False;
      i:=Low(FNotifications);
      while i<=High(FNotifications) do begin
         if FNotifications[i].obj=anObject then begin
            found:=True;
            while i<=High(FNotifications) do begin
               FNotifications[i]:=FNotifications[i+1];
               Inc(i);
            end;
            SetLength(FNotifications, Length(FNotifications)-1);
            Break;
         end;
         Inc(i);
      end;
      if not found then
         raise EGLContext.Create(cInvalidNotificationRemoval);
   finally
      UnLock;
   end;
end;

// Terminate
//
procedure TGLContextManager.Terminate;
begin
   FTerminated:=True;
   if ContextCount=0 then begin
      GLContextManager:=nil;
      Free;
   end;
end;

// DestroyAllHandles
//
procedure TGLContextManager.DestroyAllHandles;
var
   i : Integer;
begin
   with FList.LockList do try
      for i:=Count-1 downto 0 do
         TGLContext(Items[i]).DestroyAllHandles;
   finally
      FList.UnLockList;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   GLContextManager:=TGLContextManager.Create;

finalization

   GLContextManager.Terminate;
   vContextClasses.Free;
   vContextClasses:=nil;

end.
