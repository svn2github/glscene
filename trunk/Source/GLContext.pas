// GLContext
{: Shaka<p>

   Prototypes and base implementation of TGLContext.<p>
   Currently NOT thread-safe.<p>

   <b>Historique : </b><font size=-1><ul>
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
         FColorBits : Integer;
         FStencilBits : Integer;
         FAccumBits : Integer;
         FAuxBuffers : Integer;
         FOptions : TGLRCOptions;
         FOnDestroyContext : TNotifyEvent;
         FManager : TGLContextManager;
         FActivationCount : Integer;
         FSharedContexts : TList;
         FOwnedHandles : TList;

      protected
         { Protected Declarations }
         procedure SetColorBits(const aColorBits : Integer);
         procedure SetStencilBits(const aStencilBits : Integer);
         procedure SetAccumBits(const aAccumBits : Integer);
         procedure SetAuxBuffers(const aAuxBuffers : Integer);
         procedure SetOptions(const aOptions : TGLRCOptions);
         function GetActive : Boolean;
         procedure SetActive(const aActive : Boolean);
         procedure PropagateSharedContext;

         procedure DoCreateContext(outputDevice : Integer); virtual; abstract;
         procedure DoShareLists(aContext : TGLContext); virtual; abstract;
         procedure DoDestroyContext; virtual; abstract;
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
         {: Stencil bits for the rendering context }
         property StencilBits : Integer read FStencilBits write SetStencilBits;
         {: Accumulation buffer bits for the rendering context }
         property AccumBits : Integer read FAccumBits write SetAccumBits;
         {: Auxiliary buffers bits for the rendering context }
         property AuxBuffers : Integer read FAuxBuffers write SetAuxBuffers;
         {: Rendering context options. }
         property Options : TGLRCOptions read FOptions write SetOptions;
         {: Allows reading and defining the activity for the context.<p>
            The methods of this property are just wrappers around calls
            to Activate and Deactivate. }
         property Active : Boolean read GetActive write SetActive;
         {: Triggered whenever the context is destroyed.<p>
            This events happens *before* the context has been
            actually destroyed, OpenGL resource cleanup can
            still occur here. }
         property OnDestroyContext : TNotifyEvent read FOnDestroyContext write FOnDestroyContext;

         {: Creates the context.<p>
            This method must be invoked before the context can be used.<br>
            The class will monitor the outputDevice, and if itbecomes
            invalid, attempt to destroy the context. }
         procedure CreateContext(outputDevice : Integer);
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

         {: Returns the first compatible context that isn't self in the shares. }
         function FindCompatibleContext : TGLContext;
         procedure DestroyAllHandles;
   end;

   TGLContextClass = class of TGLContext;

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

   // TGLWin32Context
   //
   {: A context driver for standard Windows OpenGL (via MS OpenGL). }
   TGLWin32Context = class (TGLContext)
      private
         { Private Declarations }
         FRC, FDC : Integer;

      protected
         { Protected Declarations }
         procedure DoCreateContext(outputDevice : Integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
   end;

  EOpenGLError = class(Exception);

// RegisterGLContextClass
//
{: Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);

function CurrentGLContext : TGLContext;

var
   GLContextManager : TGLContextManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Forms, OpenGL12, Windows;

resourcestring
   cCannotAlterAnActiveContext = 'Cannot alter an active context';
   cInvalidContextRegistration = 'Invalid context registration';
   cInvalidNotificationRemoval = 'Invalid notification removal';
   cContextAlreadyCreated =      'Context already created';
   cContextNotCreated =          'Context not created';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnbalancedContexActivations= 'Unbalanced context activations';
   cIncompatibleContexts =       'Incompatible contexts';

var
   vContextClasses : TList = nil;

threadvar
   vCurrentGLContext : TGLContext;

// CurrentGLContext
//
function CurrentGLContext : TGLContext;
begin
   Result:=vCurrentGLContext;
end;

{$IFNDEF VER140}
procedure RaiseLastOSError;
begin
   RaiseLastWin32Error;
end;
{$ENDIF}

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError: UINT;
	Count: Word;
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
		raise EOpenGLError.Create(gluErrorString(GLError));
	end;
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
   GLContextManager.RegisterContext(Self);
end;

// Destroy
//
destructor TGLContext.Destroy;
begin
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
   DoCreateContext(outputDevice);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// PropagateSharedContext
//
procedure TGLContext.PropagateSharedContext;
var
   i, j : Integer;
begin
   for i:=0 to FSharedContexts.Count-1 do begin
      if TGLContext(FSharedContexts[i])<>Self then begin
         with TGLContext(FSharedContexts[i]).FSharedContexts do begin
            Clear;
            for j:=0 to FSharedContexts.Count-1 do
               Add(FSharedContexts[j]);
         end;
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
      for i:=FSharedContexts.Count-1 downto 0 do
         TGLContextHandle(FSharedContexts[i]).DestroyHandle;
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
   if IsValid then begin
      if vCurrentGLContext<>Self then begin
         oldContext:=vCurrentGLContext;
         if Assigned(oldContext) then
            oldContext.Deactivate;
      end else oldContext:=nil;
      Activate;
      try
         compatContext:=FindCompatibleContext;
         if Assigned(compatContext) then begin
            // transfer handle owner ship to a compat context
            for i:=FOwnedHandles.Count-1 downto 0 do begin
               compatContext.FOwnedHandles.Add(FOwnedHandles[i]);
               TGLContextHandle(FOwnedHandles).FRenderingContext:=compatContext;
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
   end else raise EGLContext.Create(cContextNotCreated);
end;

// Activate
//
procedure TGLContext.Activate;
begin
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      DoActivate;
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
   // reset error status
   glGetError;
   // delete
   glDeleteLists(FHandle, 1);
   // check for error
   CheckOpenGLError;
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
   // reset error status
   glGetError;
   // delete
 	glDeleteTextures(1, @FHandle);
   // check for error
   CheckOpenGLError;
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
   if Assigned(vContextClasses) and (vContextClasses.Count>0) then
      Result:=TGLContextClass(vContextClasses[0]).Create
   else Result:=nil;
   Result.FManager:=Self;
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

// ------------------
// ------------------ TGLWin32Context ------------------
// ------------------

var
   vLastPixelFormat : Integer; 

// Create
//
constructor TGLWin32Context.Create;
begin
   inherited Create;
end;

// Destroy
//
destructor TGLWin32Context.Destroy;
begin
   inherited Destroy;
end;

// SetupPalette
//
function SetupPalette(DC: HDC; PFD: TPixelFormatDescriptor): HPalette;
var
   nColors, I : Integer;
   LogPalette : TMaxLogPalette;
   RedMask, GreenMask, BlueMask : Byte;
begin
   nColors := 1 shl Pfd.cColorBits;
   LogPalette.palVersion := $300;
   LogPalette.palNumEntries := nColors;
   RedMask := (1 shl Pfd.cRedBits  ) - 1;
   GreenMask := (1 shl Pfd.cGreenBits) - 1;
   BlueMask := (1 shl Pfd.cBlueBits ) - 1;
   with LogPalette, PFD do for I := 0 to nColors - 1 do begin
      palPalEntry[I].peRed := (((I shr cRedShift  ) and RedMask  ) * 255) div RedMask;
      palPalEntry[I].peGreen := (((I shr cGreenShift) and GreenMask) * 255) div GreenMask;
      palPalEntry[I].peBlue := (((I shr cBlueShift ) and BlueMask ) * 255) div BlueMask;
      palPalEntry[I].peFlags := 0;
   end;

   Result := CreatePalette(PLogPalette(@LogPalette)^);
   if Result <> 0 then begin
      SelectPalette(DC, Result, False);
      RealizePalette(DC);
   end else RaiseLastOSError;
end;

// DoCreateContext
//
procedure TGLWin32Context.DoCreateContext(outputDevice : Integer);
const
   cMemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
   cBoolToInt : array [False..True] of Integer = (GL_FALSE, GL_TRUE);
var
   pfDescriptor : TPixelFormatDescriptor;
   pixelFormat : Integer;
   aType : DWORD;
{   iAttribList, iFormats, iValues : array of Integer;
   fAttribList : array of Single;
   nbFormats, rc, i : Integer;
   chooseResult : LongBool;

   procedure AddAttrib(attrib, value : Integer);
   var
      n : Integer;
   begin
      n:=Length(iAttribList);
      SetLength(iAttribList, n+2);
      iAttribList[n]:=attrib;
      iAttribList[n+1]:=value;
   end; }

begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;
   // *UNDER CONSTRUCTION... ENABLE BACK ONLY IF YOU UNDERSTAND THAT STUFF*
   //
{   if WGL_ARB_pixel_format then begin
      // New pixel format selection via wglChoosePixelFormatARB
      SetLength(iAttribList, 0);
      AddAttrib(WGL_DRAW_TO_WINDOW_ARB, GL_TRUE);
//      AddAttrib(WGL_DRAW_TO_BITMAP_ARB, GL_TRUE);
      AddAttrib(WGL_DOUBLE_BUFFER_ARB, cBoolToInt[rcoDoubleBuffered in Options]);
//      AddAttrib(WGL_STEREO_ARB, cBoolToInt[rcoStereo in Options]);
//      AddAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
      AddAttrib(WGL_SUPPORT_OPENGL_ARB, GL_TRUE);
      AddAttrib(WGL_COLOR_BITS_ARB, ColorBits);
//      AddAttrib(WGL_DEPTH_BITS_ARB, 24);
//      AddAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
//      AddAttrib(WGL_ACCUM_BITS_ARB, AccumBits);
//      AddAttrib(WGL_AUX_BUFFERS_ARB, AuxBuffers);
//      AddAttrib(WGL_SAMPLE_BUFFERS_ARB, 4);
      AddAttrib(0, 0);
      SetLength(fAttribList, 2); fAttribList[0]:=0; fAttribList[1]:=0;
      SetLength(iFormats, 512);
      nbFormats:=1;
      outputDevice:=GetDC(0);
      rc:=CreateRenderingContext(outputDevice, [], 24, 0, 0, 0, 0);
      wglMakeCurrent(outputDevice, rc);
      chooseResult:=wglChoosePixelFormatARB(outputDevice, @iAttribList[0], @fAttribList[0], 512, @iFormats[0], @nbFormats);
      Assert(chooseResult);
      Assert(nbFormats<>0);
      for i:=0 to nbFormats-1 do begin
         SetLength(iAttribList, 6);
         iAttribList[0]:=WGL_ACCELERATION_ARB;
         iAttribList[1]:=WGL_SWAP_METHOD_ARB;
         iAttribList[2]:=WGL_COLOR_BITS_ARB;
         iAttribList[3]:=WGL_DEPTH_BITS_ARB;
         iAttribList[4]:=WGL_STENCIL_BITS_ARB;
         iAttribList[5]:=WGL_SAMPLES_ARB;
         SetLength(iValues, 6);
         wglGetPixelFormatAttribivARB(outputDevice, iFormats[i], 0, 6, @iAttribList[0], @iValues[0]);
      end;

   end else begin }
      // Legacy pixel format selection
      FillChar(pfDescriptor, SizeOf(pfDescriptor), 0);
      with PFDescriptor do begin
         nSize:=SizeOf(PFDescriptor);
         nVersion:=1;
         dwFlags:=PFD_SUPPORT_OPENGL;
         aType:=GetObjectType(outputDevice);
         if aType=0 then
            RaiseLastOSError;
         if aType in cMemoryDCs then
            dwFlags:=dwFlags or PFD_DRAW_TO_BITMAP
         else dwFlags:=dwFlags or PFD_DRAW_TO_WINDOW;
         if rcoDoubleBuffered in Options then
            dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
         if rcoStereo in Options then
            dwFlags:=dwFlags or PFD_STEREO;
         iPixelType:=PFD_TYPE_RGBA;
         cColorBits:=ColorBits;
         cDepthBits:=32;
         cStencilBits:=StencilBits;
         cAccumBits:=AccumBits;
         cAuxBuffers:=AuxBuffers;
         iLayerType:=PFD_MAIN_PLANE;
      end;

      pixelFormat:=ChoosePixelFormat(outputDevice, @PFDescriptor);
//   end;

   if pixelFormat=0 then RaiseLastOSError;

   if GetPixelFormat(outputDevice)<>pixelFormat then begin
      if not SetPixelFormat(outputDevice, pixelFormat, @PFDescriptor) then
         RaiseLastOSError;
   end;

   // Check the properties we just set.
   DescribePixelFormat(outputDevice, pixelFormat, SizeOf(PFDescriptor), PFDescriptor);
   with pfDescriptor do
      if (dwFlags and PFD_NEED_PALETTE) <> 0 then
         SetupPalette(outputDevice, PFDescriptor);

   FRC:=wglCreateContext(outputDevice);
   if FRC=0 then
      RaiseLastOSError
   else vLastPixelFormat:=0;
   FDC:=outputDevice;

end;

// DoShareLists
//
procedure TGLWin32Context.DoShareLists(aContext : TGLContext);
begin
   if aContext is TGLWin32Context then
      wglShareLists(FRC, TGLWin32Context(aContext).FRC)
   else raise Exception.Create(cIncompatibleContexts);
end;

// DoDestroyContext
//
procedure TGLWin32Context.DoDestroyContext;
begin
   if not wglDeleteContext(FRC) then
      raise EGLContext.Create(cDeleteContextFailed);
   FRC:=0;
   FDC:=0;
end;

// DoActivate
//
procedure TGLWin32Context.DoActivate;
var
   pixelFormat : Integer;
begin
   if not wglMakeCurrent(FDC, FRC) then
      raise EGLContext.Create(Format(cContextActivationFailed, [GetLastError]));

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
   pixelFormat:=GetPixelFormat(FDC);
   if PixelFormat<>vLastPixelFormat then begin
      ReadImplementationProperties;
      ReadExtensions;
      vLastPixelFormat:=pixelFormat;
   end;
end;

// Deactivate
//
procedure TGLWin32Context.DoDeactivate;
begin
   if not wglMakeCurrent(0, 0) then
      raise Exception.Create(cContextDeactivationFailed);
end;

// IsValid
//
function TGLWin32Context.IsValid : Boolean;
begin
   Result:=(FRC<>0);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   GLContextManager:=TGLContextManager.Create;
   RegisterGLContextClass(TGLWin32Context);

finalization

   GLContextManager.Terminate;
   vContextClasses.Free;
   vContextClasses:=nil;

end.
