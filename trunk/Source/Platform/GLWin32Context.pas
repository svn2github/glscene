{: GLWin32Context<p>

   Win32 specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/02/02 - EG - Fixed UnTrackWindow
      <li>03/02/02 - EG - Added experimental Hook-based window tracking
      <li>29/01/02 - EG - Improved recovery for ICDs without pbuffer  support 
      <li>21/01/02 - EG - More graceful recovery for ICDs without pbuffer support
      <li>07/01/02 - EG - DoCreateMemoryContext now retrieved topDC when needed
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Hardware acceleration support now detected
      <li>20/11/01 - EG - New temp HWnd code for memory contexts (improved compat.)
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLWin32Context;

interface

{$i ../GLScene.inc}

uses Classes, SysUtils, GLContext;

type

   // TGLWin32Context
   //
   {: A context driver for standard Windows OpenGL (via MS OpenGL). }
   TGLWin32Context = class (TGLContext)
      private
         { Private Declarations }
         FRC, FDC, FHPBUFFER : Integer;
         FiAttribs : packed array of Integer;
         FfAttribs : packed array of Single;

      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure ClearFAttribs;
         procedure AddFAttrib(attrib, value : Single);

         procedure DestructionEarlyWarning(sender : TObject);

         procedure DoCreateContext(outputDevice : Integer); override;
         procedure DoCreateMemoryContext(outputDevice, width, height : Integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;
   end;

var
   { This boolean controls a hook-based tracking of top-level forms destruction,
     with the purpose of being able to properly release OpenGL contexts before
     they are (improperly) released by some drivers upon top-level form
     destruction. }
   vUseWindowTrackingHook : Boolean = True;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Forms, OpenGL12, Windows, GLCrossPlatform, Messages;

resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateMemoryContext= 'Unable to create memory context, pbuffer probably not supported';

var
   vTrackingCount : Integer;
   vTrackedHwnd : array of HWND;
   vTrackedEvents : array of TNotifyEvent;
   vTrackingHook : HHOOK;

// TrackHookProc
//
function TrackHookProc(nCode : Integer; wParam : wParam; lParam : LPARAM) : Integer; stdcall;
var
   i : Integer;
   p : PCWPStruct;
begin
   if nCode=HC_ACTION then begin
      p:=PCWPStruct(lParam);
 //   if (p.message=WM_DESTROY) or (p.message=WM_CLOSE) then begin // destroy & close variant
      if p.message=WM_DESTROY then begin
         // special care must be taken by this loop, items may go away unexpectedly
         i:=vTrackingCount-1;
         while i>=0 do begin
            if IsChild(p.hwnd, vTrackedHwnd[i]) then begin
               // got one, send notification
               vTrackedEvents[i](nil);
            end;
            Dec(i);
            while i>=vTrackingCount do Dec(i);
         end;
      end;
      CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
      Result:=0;
   end else Result:=CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
end;

// TrackWindow
//
procedure TrackWindow(h : HWND; notifyEvent : TNotifyEvent);
begin
   if not IsWindow(h) then Exit;
   if vTrackingCount=0 then
      vTrackingHook:=SetWindowsHookEx(WH_CALLWNDPROC, @TrackHookProc, 0, GetCurrentThreadID);
   Inc(vTrackingCount);
   SetLength(vTrackedHwnd, vTrackingCount);
   vTrackedHwnd[vTrackingCount-1]:=h;
   SetLength(vTrackedEvents, vTrackingCount);
   vTrackedEvents[vTrackingCount-1]:=notifyEvent;
end;

// UnTrackWindows
//
procedure UnTrackWindow(h : HWND);
var
   i, k : Integer;
begin
   if not IsWindow(h) then Exit;
   if vTrackingCount=0 then Exit;
   k:=0;
   for i:=0 to vTrackingCount-1 do if vTrackedHwnd[i]<>h then begin
      vTrackedHwnd[k]:=vTrackedHwnd[i];
      vTrackedEvents[k]:=vTrackedEvents[i];
      Inc(k);
   end;
   Dec(vTrackingCount);
   SetLength(vTrackedHwnd, vTrackingCount);
   SetLength(vTrackedEvents, vTrackingCount);
   if vTrackingCount=0 then
      UnhookWindowsHookEx(vTrackingHook);
end;


// ------------------
// ------------------ TGLWin32Context ------------------
// ------------------

var
   vLastPixelFormat : Integer;
   vLastVendor : String;

// Create
//
constructor TGLWin32Context.Create;
begin
   inherited Create;
   ClearIAttribs;
   ClearFAttribs;
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

// ClearIAttribs
//
procedure TGLWin32Context.ClearIAttribs;
begin
   SetLength(FiAttribs, 1);
   FiAttribs[0]:=0;
end;

// AddIAttrib
//
procedure TGLWin32Context.AddIAttrib(attrib, value : Integer);
var
   n : Integer;
begin
   n:=Length(FiAttribs);
   SetLength(FiAttribs, n+2);
   FiAttribs[n-1]:=attrib;       FiAttribs[n]:=value;
   FiAttribs[n+1]:=0;
end;

// ChangeIAttrib
//
procedure TGLWin32Context.ChangeIAttrib(attrib, newValue : Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         FiAttribs[i+1]:=newValue;
         Exit;
      end;
      Inc(i, 2);
   end;
   AddIAttrib(attrib, newValue);
end;

// ClearFAttribs
//
procedure TGLWin32Context.ClearFAttribs;
begin
   SetLength(FfAttribs, 1);
   FfAttribs[0]:=0;
end;

// AddFAttrib
//
procedure TGLWin32Context.AddFAttrib(attrib, value : Single);
var
   n : Integer;
begin
   n:=Length(FfAttribs);
   SetLength(FfAttribs, n+2);
   FfAttribs[n-1]:=attrib;       FfAttribs[n]:=value;
   FfAttribs[n+1]:=0;
end;

// DestructionEarlyWarning
//
procedure TGLWin32Context.DestructionEarlyWarning(sender : TObject);
begin
   DestroyContext;
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
{  iFormats, iValues : array of Integer;
   nbFormats, rc, i : Integer;
   chooseResult : LongBool; }
begin
   if vUseWindowTrackingHook then
      TrackWindow(WindowFromDC(outputDevice), DestructionEarlyWarning);

   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;
   // *UNDER CONSTRUCTION... ENABLE BACK ONLY IF YOU UNDERSTAND THAT STUFF*
   //
{   if WGL_ARB_pixel_format then begin
      // New pixel format selection via wglChoosePixelFormatARB
      ClearIAttribs;
      AddIAttrib(WGL_DRAW_TO_WINDOW_ARB, GL_TRUE);
//      AddIAttrib(WGL_DRAW_TO_BITMAP_ARB, GL_TRUE);
      AddIAttrib(WGL_DOUBLE_BUFFER_ARB, cBoolToInt[rcoDoubleBuffered in Options]);
//      AddIAttrib(WGL_STEREO_ARB, cBoolToInt[rcoStereo in Options]);
//      AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
      AddAttrib(WGL_SUPPORT_OPENGL_ARB, GL_TRUE);
      AddIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
//      AddIAttrib(WGL_DEPTH_BITS_ARB, 24);
//      AddIAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
//      AddIAttrib(WGL_ACCUM_BITS_ARB, AccumBits);
//      AddIAttrib(WGL_AUX_BUFFERS_ARB, AuxBuffers);
//      AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, 4);
      ClearFAttribs;
      SetLength(iFormats, 512);
      nbFormats:=1;
      outputDevice:=GetDC(0);
      rc:=CreateRenderingContext(outputDevice, [], 24, 0, 0, 0, 0);
      wglMakeCurrent(outputDevice, rc);
      chooseResult:=wglChoosePixelFormatARB(outputDevice, @FiAttribList[0], @FfAttribList[0],
                                            512, @iFormats[0], @nbFormats);
      Assert(chooseResult);
      Assert(nbFormats<>0);
      for i:=0 to nbFormats-1 do begin
         SetLength(FiAttribList, 6);
         FiAttribList[0]:=WGL_ACCELERATION_ARB;
         FiAttribList[1]:=WGL_SWAP_METHOD_ARB;
         FiAttribList[2]:=WGL_COLOR_BITS_ARB;
         FiAttribList[3]:=WGL_DEPTH_BITS_ARB;
         FiAttribList[4]:=WGL_STENCIL_BITS_ARB;
         FiAttribList[5]:=WGL_SAMPLES_ARB;
         SetLength(iValues, 6);
         wglGetPixelFormatAttribivARB(outputDevice, iFormats[i], 0, 6, @FiAttribList[0], @iValues[0]);
      end;

   end else begin }
      // Legacy pixel format selection
      FillChar(pfDescriptor, SizeOf(pfDescriptor), 0);
      with PFDescriptor do begin
         nSize:=SizeOf(PFDescriptor);
         nVersion:=1;
         dwFlags:=PFD_SUPPORT_OPENGL;
         aType:=GetObjectType(Cardinal(outputDevice));
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
         cDepthBits:=24;
         cStencilBits:=StencilBits;
         cAccumBits:=AccumBits;
         cAlphaBits:=AlphaBits;
         cAuxBuffers:=AuxBuffers;
         iLayerType:=PFD_MAIN_PLANE;
      end;

      pixelFormat:=ChoosePixelFormat(Cardinal(outputDevice), @PFDescriptor);
//   end;

   if pixelFormat=0 then RaiseLastOSError;

   if GetPixelFormat(Cardinal(outputDevice))<>pixelFormat then begin
      if not SetPixelFormat(Cardinal(outputDevice), pixelFormat, @PFDescriptor) then
         RaiseLastOSError;
   end;

   // Check the properties we just set.
   DescribePixelFormat(Cardinal(outputDevice), pixelFormat, SizeOf(PFDescriptor), PFDescriptor);
   with pfDescriptor do
      if (dwFlags and PFD_NEED_PALETTE) <> 0 then
         SetupPalette(outputDevice, PFDescriptor);

   if (pfDescriptor.dwFlags and PFD_GENERIC_FORMAT)>0 then
      FAcceleration:=chaSoftware
   else FAcceleration:=chaHardware;

   FRC:=wglCreateContext(Cardinal(outputDevice));
   if FRC=0 then
      RaiseLastOSError
   else vLastPixelFormat:=0;
   FDC:=outputDevice;

end;

var
  vUtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'GLSUtilWindow');

// DoCreateMemoryContext
//
procedure TGLWin32Context.DoCreateMemoryContext(outputDevice, width, height : Integer);
var
   nbFormats : Integer;
   iFormats : array [0..31] of Integer;
   iPBufferAttribs : array [0..0] of Integer;
   localHPBuffer, localDC, localRC, tempDC : Integer;
   tempWnd : HWND;
   tempClass: TWndClass;
   classRegistered: Boolean;
   topDC : Integer;
begin
   localHPBuffer:=0;
   localDC:=0;
   localRC:=0;
   // if outputDevice is null, assume TopDC needs be used
   if outputDevice=0 then begin
      topDC:=GetDC(0);
      outputDevice:=topDC;
   end else topDC:=0;
   try

      // the WGL mechanism is a little awkward: we first create a dummy context
      // on the TOP-level DC (ie. screen), to retrieve our pixelformat, create
      // our stuff, etc.
      vUtilWindowClass.hInstance:=HInstance;
      classRegistered:=GetClassInfo(HInstance, vUtilWindowClass.lpszClassName,
                                    tempClass);
      if not classRegistered  then begin
         Windows.RegisterClass(vUtilWindowClass);
      end;
      tempWnd:=CreateWindowEx(WS_EX_TOOLWINDOW, vUtilWindowClass.lpszClassName,
                              '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
      tempDC:=GetDC(tempWnd);
      try
         try
            DoCreateContext(tempDC);
         except
            on E: Exception do begin
               raise Exception.Create(cUnableToCreateMemoryContext+#13#10
                                      +E.ClassName+E.Message); 
            end;
         end;
         try
            DoActivate;
            try
               ClearGLError;
               if WGL_ARB_pixel_format and WGL_ARB_pbuffer then begin
                  ClearIAttribs;
                  AddIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
                  AddIAttrib(WGL_ALPHA_BITS_ARB, AlphaBits);
                  AddIAttrib(WGL_DEPTH_BITS_ARB, 24);
                  if StencilBits>0 then
                     AddIAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
                  AddIAttrib(WGL_DRAW_TO_PBUFFER_ARB, 1);
                  ClearFAttribs;
                  wglChoosePixelFormatARB(outputDevice, @FiAttribs[0], @FfAttribs[0],
                                          32, @iFormats, @nbFormats);
                  if nbFormats=0 then begin
                     // couldn't find 24 bits depth buffer, 16 bits one available?
                     ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
                     wglChoosePixelFormatARB(outputDevice, @FiAttribs[0], @FfAttribs[0],
                                             32, @iFormats, @nbFormats);
                  end;
                  if nbFormats=0 then
                     raise Exception.Create('Format not supported for pbuffer operation.');
                  iPBufferAttribs[0]:=0;

                  localHPBuffer:=wglCreatePbufferARB(outputDevice, iFormats[0], width, height,
                                                     @iPBufferAttribs[0]);
                  if localHPBuffer=0 then
                     raise Exception.Create('Unabled to create pbuffer.');
                  try
                     localDC:=wglGetPbufferDCARB(localHPBuffer);
                     if localDC=0 then
                        raise Exception.Create('Unabled to create pbuffer''s DC.');
                     try
                        localRC:=wglCreateContext(localDC);
                        if localRC=0 then
                           raise Exception.Create('Unabled to create pbuffer''s RC.');
                     except
                        wglReleasePbufferDCARB(localHPBuffer, localDC);
                        raise;
                     end;
                  except
                     wglDestroyPbufferARB(localHPBuffer);
                     raise;
                  end;
               end else raise Exception.Create('WGL_ARB_pbuffer support required.');
               CheckOpenGLError;
            finally
               DoDeactivate;
            end;
         finally
            DoDestroyContext;
         end;
      finally
         ReleaseDC(0, tempDC);
         DestroyWindow(tempWnd);
         FHPBUFFER:=localHPBuffer;
         FDC:=localDC;
         FRC:=localRC;
      end;
      FAcceleration:=chaHardware;

   finally
      if topDC<>0 then
         ReleaseDC(0, topDC);
   end;
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
   if vUseWindowTrackingHook then
      UnTrackWindow(WindowFromDC(FDC));

   if FRC<>0 then
      if not wglDeleteContext(FRC) then
         raise EGLContext.Create(cDeleteContextFailed);
   if FHPBUFFER<>0 then begin
      wglReleasePbufferDCARB(FHPBuffer, FDC);
      wglDestroyPbufferARB(FHPBUFFER);
      FHPBUFFER:=0;
   end;
   FRC:=0;
   FDC:=0;
end;

// DoActivate
//
procedure TGLWin32Context.DoActivate;
var
   pixelFormat : Integer;
begin
   if not wglMakeCurrent(Cardinal(FDC), Cardinal(FRC)) then
      raise EGLContext.Create(Format(cContextActivationFailed, [GetLastError]));

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
   pixelFormat:=GetPixelFormat(Cardinal(FDC));
   if PixelFormat<>vLastPixelFormat then begin
      if glGetString(GL_VENDOR)<>vLastVendor then begin
         ReadExtensions;
         ReadImplementationProperties;
         vLastVendor:=glGetString(GL_VENDOR);
      end else begin
         ReadWGLExtensions;
         ReadWGLImplementationProperties;
      end;
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

// SwapBuffers
//
procedure TGLWin32Context.SwapBuffers;
begin
   if (FHPBUFFER=0) and (FDC<>0) and (rcoDoubleBuffered in Options) then
      Windows.SwapBuffers(Cardinal(FDC));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLWin32Context);
   
end.
