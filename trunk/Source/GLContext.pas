// GLContext
{: Shaka<p>

   Prototypes and base implementation of TGLContext.<p>
   Currently NOT thread-safe.<p>

   <b>Historique : </b><font size=-1><ul>
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

      protected
         { Protected Declarations }
         procedure SetColorBits(const aColorBits : Integer);
         procedure SetStencilBits(const aStencilBits : Integer);
         procedure SetAccumBits(const aAccumBits : Integer);
         procedure SetAuxBuffers(const aAuxBuffers : Integer);
         procedure SetOptions(const aOptions : TGLRCOptions);
         function GetActive : Boolean;
         procedure SetActive(const aActive : Boolean);

         procedure DoCreateContext(outputDevice : Integer); virtual; abstract;
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

   end;

   TGLContextClass = class of TGLContext;

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
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
   end;

// RegisterGLContextClass
//
{: Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);

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
   cContextActivationFailed =    'Context activation failed';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnbalancedContexActivations= 'Unbalanced context activations';

var
   vContextClasses : TList = nil;


{$IFNDEF VER140}
procedure RaiseLastOSError;
begin
   RaiseLastWin32Error;
end;
{$ENDIF}

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
   GLContextManager.RegisterContext(Self);
end;

// Destroy
//
destructor TGLContext.Destroy;
begin
   GLContextManager.UnRegisterContext(Self);
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
   Manager.ContextCreatedBy(Self);
end;

// DestroyContext
//
procedure TGLContext.DestroyContext;
begin
   if IsValid then begin
      Manager.DestroyingContextBy(Self);
      Active:=False;
      DoDestroyContext;
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
      Inc(FActivationCount);
   end;
end;

// Deactivate
//
procedure TGLContext.Deactivate;
begin
   Dec(FActivationCount);
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      DoDeactivate;
   end else if FActivationCount<0 then
      raise EGLContext.Create(cUnbalancedContexActivations);
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
var
   pfDescriptor : TPixelFormatDescriptor;
   pixelFormat : Integer;
   aType : DWORD;
begin
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

   // Just in case it didn't happen already.
   if not InitOpenGL then
      RaiseLastOSError;
   pixelFormat:=ChoosePixelFormat(outputDevice, @PFDescriptor);
   if pixelFormat=0 then
      RaiseLastOSError;

   if GetPixelFormat(outputDevice)<>pixelFormat then begin
      if not SetPixelFormat(outputDevice, pixelFormat, @PFDescriptor) then
         RaiseLastOSError;
   end;

   // Check the properties we just set.
   DescribePixelFormat(outputDevice, PixelFormat, SizeOf(PFDescriptor), PFDescriptor);
   with pfDescriptor do
      if (dwFlags and PFD_NEED_PALETTE) <> 0 then
         SetupPalette(outputDevice, PFDescriptor);

   FRC:=wglCreateContext(outputDevice);
   if FRC=0 then
      RaiseLastOSError
   else vLastPixelFormat:=0;
   FDC:=outputDevice;

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
      raise EGLContext.Create(cContextActivationFailed);

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

finalization

   GLContextManager.Terminate;
   vContextClasses.Free;
   vContextClasses:=nil;

end.
