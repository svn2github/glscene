{: GLGLXContext<p>

   GLX specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>10/06/09 - DanB - Added to main GLScene CVS repository (from GLScene-Lazarus).
      <li>14/01/05 - CU - Creation
   </ul></font>
}
unit GLGLXContext;

interface

{$i GLScene.inc}

uses
  xlib, Classes, sysutils, GLCrossPlatform, GLContext, OpenGL1x,
  x, xutil;

type
   // TGLGLXContext
   //
   {: A context driver for GLX. }
   TGLGLXContext = class (TGLContext)
      private
         { Private Declarations }
         FRenderingContext: GLXContext;
         FCurXDisplay: Pointer;
         FCurXWindow: LongInt;
         FiAttribs : packed array of Integer;
      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure DropIAttrib(attrib : Integer);

         procedure DestructionEarlyWarning(sender : TObject);

         // DoGetHandles must be implemented in child classes, and return the display + window
         procedure DoGetHandles(outputDevice: Cardinal; out XDisp: Pointer; out XWin: LongInt); virtual; abstract;
         procedure GetHandles(outputDevice: Cardinal);
         procedure DoCreateContext(outputDevice : Cardinal); override;
         procedure DoCreateMemoryContext(outputDevice : Cardinal;width, height : Integer; BufferCount : integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

         property RenderingContext: GLXContext read FRenderingContext;
         property CurXDisplay: Pointer read FCurXDisplay;// write FCurXDisplay;
         property CurXWindow: LongInt read FCurXWindow;// write FCurXWindow;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;

         function RenderOutputDevice : Integer; override;
   end;

implementation

// ------------------
// ------------------ TGLGLXContext ------------------
// ------------------

var
//   vLastPixelFormat : Integer;
   vLastVendor : String;


procedure TGLGLXContext.ClearIAttribs;
begin
   SetLength(FiAttribs, 1);
   FiAttribs[0]:=0;
end;

procedure TGLGLXContext.AddIAttrib(attrib, value: Integer);
var
   n : Integer;
begin
   n:=Length(FiAttribs);
   SetLength(FiAttribs, n+2);
   FiAttribs[n-1]:=attrib;
   FiAttribs[n]:=value;
   FiAttribs[n+1]:=0;
end;

procedure TGLGLXContext.ChangeIAttrib(attrib, newValue: Integer);
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

procedure TGLGLXContext.DropIAttrib(attrib: Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         Inc(i, 2);
         while i<Length(FiAttribs) do begin
            FiAttribs[i-2]:=FiAttribs[i];
            Inc(i);
         end;
         SetLength(FiAttribs, Length(FiAttribs)-2);
         Exit;
      end;
      Inc(i, 2);
   end;
end;

procedure TGLGLXContext.DestructionEarlyWarning(sender: TObject);
begin
   DestroyContext;
end;

procedure GetHandles(outputDevice: Cardinal);
begin
  DoGetHandles(outputDevice, FCurXDisplay, FCurXWindow);
end;

// DoCreateContext
//
procedure TGLGLXContext.DoCreateContext(outputDevice : Cardinal);
var
  winattr: TXWindowAttributes;
  vitemp: TXVisualInfo;
  nret: Integer;
  vi: PXvisualInfo;
  CurScreen : Integer;
begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;

    // Get CurXDisplay & CurXWindow
    GetHandles(outputDevice);

    XGetWindowAttributes(CurXDisplay, CurXWindow, @winattr);
    vitemp.visual := winattr.visual;
    vitemp.visualid := XVisualIDFromVisual(vitemp.visual);
    vi := XGetVisualInfo(CurXDisplay, VisualIDMask, @vitemp, @nret);
    CurScreen := vi^.screen;

    {crossbuilder: better free vi instead of just nil it to avoid leak
    vi := nil;}
    XFree(vi);

    AddIAttrib(GLX_RGBA,GL_TRUE);

    AddIAttrib(GLX_RED_SIZE, round(ColorBits/4));
    AddIAttrib(GLX_GREEN_SIZE, round(ColorBits/4));
    AddIAttrib(GLX_BLUE_SIZE, round(ColorBits/4));
    if AlphaBits>0 then
     AddIAttrib(GLX_ALPHA_SIZE, AlphaBits);
    AddIAttrib(GLX_DEPTH_SIZE, DepthBits);
    if StencilBits>0 then
       AddIAttrib(GLX_STENCIL_SIZE, StencilBits);
    if AccumBits>0 then
      begin
       AddIAttrib(GLX_ACCUM_RED_SIZE, round(AccumBits/4));
       AddIAttrib(GLX_ACCUM_GREEN_SIZE, round(AccumBits/4));
       AddIAttrib(GLX_ACCUM_BLUE_SIZE, round(AccumBits/4));
      end;
    if AuxBuffers>0 then
       AddIAttrib(GLX_AUX_BUFFERS, AuxBuffers);
    if (rcoDoubleBuffered in Options) then
       AddIAttrib(GLX_DOUBLEBUFFER,GL_TRUE);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (DepthBits>=32) then
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 24);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (DepthBits>=24) then
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (ColorBits>=24) then
      begin
        AddIAttrib(GLX_RED_SIZE, 4);
        AddIAttrib(GLX_GREEN_SIZE, 4);
        AddIAttrib(GLX_BLUE_SIZE, 4);
      end;
    if not Assigned(vi) then
      vi := XGetVisualInfo(CurXDisplay, VisualIDMask, @vitemp, @nret);
    { Create OpenGL context }
    //FRenderingContext := glXCreateContext(CurXDisplay, vi, nil, false); //Last Param (Direct Draw) schoud be true but guves some strange errors
    FRenderingContext := glXCreateContext(CurXDisplay, vi, nil, true); //Last Param (Direct Draw) schoud MUST true, otherwise I get
                                                                       //GdK-Error on machines with HW-Accel. Not at this line, but somwhere latere (didn't find where)
    XFree(vi);
    if RenderingContext = nil then
      raise EGLContext.Create('Failed to create rendering context');
    if PtrUInt(RenderingContext) = GLX_BAD_CONTEXT then
      raise EGLContext.Create('bad context');
end;

// DoCreateMemoryContext
//
procedure TGLGLXContext.DoCreateMemoryContext(outputDevice : Cardinal; width, height : Integer; BufferCount : integer);
begin
  {$MESSAGE Warn 'DoCreateMemoryContext: Needs to be implemented'}
end;

// DoShareLists
//
procedure TGLGLXContext.DoShareLists(aContext : TGLContext);
var
   otherRC : GLXContext;
begin
  {$MESSAGE Warn 'DoShareLists: Needs to be implemented'}
   if aContext is TGLGLXContext then begin
      otherRC:=TGLGLXContext(aContext).RenderingContext;
      // some drivers fail (access violation) when requesting to share
      // a context with itself
      if FRenderingContext<>otherRC then
         //Can't find such a function.
         //glXShareLists(FRC, otherRC);
         //Seems, a sharedList context must be given when creating the context (3. parameter of glXCeateContext)
   end else raise Exception.Create(cIncompatibleContexts);
end;

procedure TGLGLXContext.DoDestroyContext;
begin
  if (glXGetCurrentContext() = RenderingContext) and
     (not glXMakeCurrent(CurXDisplay, 0, nil)) then
    raise Exception.Create('Failed to deselect rendering context');
  glXDestroyContext(CurXDisplay, RenderingContext);
  FRenderingContext := nil;
//  FOutputDevice := nil;
end;

// DoActivate
//
procedure TGLGLXContext.DoActivate;
begin
  if not glXMakeCurrent(CurXDisplay,CurXWindow,FRenderingContext) then
    raise EGLContext.Create(cContextActivationFailed);

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
  if glGetString(GL_VENDOR) <> vLastVendor then
    begin
      ReadExtensions;
      ReadImplementationProperties;
      vLastVendor:=glGetString(GL_VENDOR);
    end
end;

// Deactivate
//
procedure TGLGLXContext.DoDeactivate;
begin
  if not glXMakeCurrent(CurXDisplay,0,nil) then
    raise Exception.Create(cContextDeactivationFailed);
end;

constructor TGLGLXContext.Create;
begin
   inherited Create;
   ClearIAttribs;
end;

destructor TGLGLXContext.Destroy;
begin
  inherited Destroy;
end;

// IsValid
//
function TGLGLXContext.IsValid : Boolean;
begin
   Result:=(FRenderingContext<>nil);
end;

// SwapBuffers
//
procedure TGLGLXContext.SwapBuffers;
begin
   if (FRenderingContext<>nil) and (rcoDoubleBuffered in Options) then
    glXSwapBuffers(CurXDisplay,CurXWindow);
end;

function TGLGLXContext.RenderOutputDevice: Integer;
begin
  Result:=0;
end;

end.

