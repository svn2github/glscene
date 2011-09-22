//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLScene.Context.OES<p>

   <b>History : </b><font size=-1><ul>
      <li>03/09/11 - Yar - Creation
   </ul></font>
}
unit GLScene.Context.OES;

interface

{$I GLScene.inc}

{$IFDEF GLS_OPENGL_ES}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}
  Classes,
  SysUtils,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.Context;

type

  // TGLOESContext
  //
  {: OpenGL ES context. }

  TGLOESContext = class(TGLContext)
  private
    { Private Declarations }
    FDC: HDC;
    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FConfig: EGLConfig;
    FContext: EGLContext;
    FShareContext: TGLOESContext;
    procedure ChoosePixelFormat;
  protected
    { Protected Declarations }
    procedure DoCreateContext(outputDevice: HWND); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer); override;
    function DoShareLists(aContext: TGLContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;

    function RenderOutputDevice: HDC; override;
  end;

{$ENDIF GLS_OPENGL_ES}

implementation

{$IFDEF GLS_OPENGL_ES}

uses
  GLScene.Base.OpenGL.Adapter,
  GLScene.Platform,
  GLScene.Base.Log;

// ------------------
// ------------------ TGLOESContext ------------------
// ------------------

// Create
//

constructor TGLOESContext.Create;
begin
  inherited Create;
end;

// Destroy
//

destructor TGLOESContext.Destroy;
begin
  inherited Destroy;
end;

procedure TGLOESContext.ChoosePixelFormat;
var
  LConfigs: array of EGLConfig;
  LNumElements: Integer;

  function GetFixedAttribute(Attrib: TGLInt; Param: TGLInt): TGLInt;
  var
    I, J, Res, OverRes: integer;
  begin
    {: Appointment of a function to look for equal or approximate values
       of attributes from the list glx.
      If you just ask all the attributes
      that the user can put it out of ignorance
      Access Violation could appear as the list will be empty. }
    OverRes := -1;
    Result := -1;
    J := 0;
    for i := 0 to High(LConfigs) do
    begin
      if eglGetConfigAttrib(FDisplay, LConfigs[i], Attrib, @Res) <> EGL_TRUE then
        continue;
      if (Res > 0) and (Res <= Param) then
        Result := res;
      if (Res > Param) and (OverRes < Res) then
        OverRes := Res;
      J := I;
    end;
    if (Result = -1) and (J = High(LConfigs)) then
      Result := OverRes;
  end;

  function ChooseConfig: Boolean;
  begin
    if eglChooseConfig(FDisplay, nil, nil, 0, @LNumElements) = EGL_TRUE then
    begin
      SetLength(LConfigs, LNumElements);
      Result := eglGetConfigs(FDisplay, @LConfigs[0], Length(LConfigs), @LNumElements) = EGL_TRUE;
    end
    else
      Result := False;
  end;

const
  cAAToSamples: array[aaDefault..csa16xHQ] of Integer =
    (0, 0, 2, 2, 4, 4, 6, 8, 16, 8, 8, 16, 16);
  cCSAAToSamples: array[csa8x..csa16xHQ] of Integer = (4, 8, 4, 8);

begin
  // Temporarily create a list of available attributes
  LConfigs := nil;
  if not ChooseConfig then
    raise EGLContext.Create('Failed to accept attributes');

    ColorBits := GetFixedAttribute(EGL_BUFFER_SIZE, ColorBits);
    AddIAttrib(EGL_BUFFER_SIZE, ColorBits);
    if ColorBits = 16 then
    begin
      AddIAttrib(EGL_RED_SIZE, 5);
      AddIAttrib(EGL_GREEN_SIZE, 6);
      AddIAttrib(EGL_BLUE_SIZE, 5);
    end;

    if AlphaBits > 0 then
    begin
      AlphaBits := GetFixedAttribute(EGL_ALPHA_SIZE, AlphaBits);
      AddIAttrib(EGL_ALPHA_SIZE, AlphaBits);
    end
    else
      AddIAttrib(EGL_ALPHA_SIZE, 0);

    DepthBits := GetFixedAttribute(EGL_DEPTH_SIZE, DepthBits);
    AddIAttrib(EGL_DEPTH_SIZE, DepthBits);

    if StencilBits > 0 then
    begin
      StencilBits := GetFixedAttribute(EGL_STENCIL_SIZE, StencilBits);
      AddIAttrib(EGL_STENCIL_SIZE, StencilBits);
    end;

    if AntiAliasing <> aaDefault then
    begin
      if AntiAliasing <> aaNone then
      begin
        AddIAttrib(EGL_SAMPLE_BUFFERS, 1);
        AddIAttrib(EGL_SAMPLES, GetFixedAttribute(EGL_SAMPLES,
          cAAToSamples[AntiAliasing]));
      end
      else
        AddIAttrib(EGL_SAMPLE_BUFFERS, 0);
    end;

  AddIAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT);
  AddIAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT);

  if eglChooseConfig(FDisplay, @FiAttribs[0], @FConfig, 1, @LNumElements) <> EGL_TRUE then
    raise EGLContext.Create('Failed to accept attributes');
end;

// DoCreateContext
//

procedure TGLOESContext.DoCreateContext(outputDevice: HWND);
var
  MajorVersion, MinorVersion, Err: TGLInt;
  shareRC: EGLContext;
  LWND: EGLNativeWindowType;
begin
  {: Load OpenGL ES library. }

  if not InitOpenGL then
    RaiseLastOSError;

 {: Get the default display.
		EGL uses the concept of a "display" which in most environments
		corresponds to a single physical screen. Since we usually want
		to draw to the main screen or only have a single screen to begin
		with, we let EGL pick the default display.
		Querying other displays is platform specific. }

{$IFDEF FPC}
  DoGetHandles(outputDevice, HDC(FDC));
{$ELSE}
  FDC := HDC(outputDevice);
  LWND := WindowFromDC(FDC);
{$ENDIF}

  FDisplay := eglGetDisplay(FDC);

  if FDisplay = EGL_NO_DISPLAY then
    raise EGLContext.Create('Failed to get display');

  {: Initialize EGL.
		EGL has to be initialized with the display obtained in the
		previous step. We cannot use other EGL functions except
		eglGetDisplay and eglGetError before eglInitialize has been
		called.
		If we're not interested in the EGL version number we can just
		pass NULL for the second and third parameters. }

  MajorVersion := 0;
  MinorVersion := 0;

  if eglInitialize(FDisplay, @MajorVersion, @MinorVersion) = 0 then
    raise EGLContext.Create('Failed to initialize OpenGL ES');

  FGL.EGL_VERSION_1_0 := IsVersionMet(1, 0, MajorVersion, MinorVersion);
  FGL.EGL_VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  FGL.EGL_VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  FGL.EGL_VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  FGL.EGL_VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);
  FGL.EGL_VERSION_2_0 := IsVersionMet(2, 0, MajorVersion, MinorVersion);

  FAcceleration := chaHardware;

 {: Specify the required configuration attributes.
		An EGL "configuration" describes the pixel format and type of
		surfaces that can be used for drawing.
		For now we just want to use a 16 bit RGB surface that is a
		Window surface, i.e. it will be visible on screen. The list
		has to contain key/value pairs, terminated with EGL_NONE. }

  ClearIAttribs;
  ChoosePixelFormat;

  {: Create a surface to draw to.
		Use the config picked in the previous step and the native window
		handle when available to create a window surface. A window surface
		is one that will be visible on screen inside the native display (or
		fullscreen if there is no windowing system).
		Pixmaps and pbuffers are surfaces which only exist in off-screen
		memory. }

	FSurface := eglCreateWindowSurface(FDisplay, FConfig, LWND, nil);
  Err := eglGetError;
	if Err <> EGL_SUCCESS then
    raise EGLContext.Create('Failed to create surface to draw');

  {: Create a context.
		EGL has to create a context for OpenGL ES. Our OpenGL ES resources
		like textures will only be valid inside this context
		(or shared contexts) }

  if (ServiceContext <> nil) and (Self <> ServiceContext) then
    shareRC := TGLOESContext(ServiceContext).FContext
  else if Assigned(FShareContext) then
    shareRC := FShareContext.FContext
  else
    shareRC := EGL_NO_CONTEXT;

  ClearIAttribs;
  if FGL.EGL_VERSION_1_3 then
    AddIAttrib(EGL_CONTEXT_CLIENT_VERSION, 2);

	FContext := eglCreateContext(FDisplay, FConfig, shareRC, @FiAttribs[0]);
  if Assigned(shareRC) then
  begin
    if eglGetError = EGL_SUCCESS then
    begin
      FSharedContexts.Add(FShareContext);
      PropagateSharedContext;
    end
    else
    begin
      GLSLogger.LogWarning(glsFailedToShare);
      FContext := eglCreateContext(FDisplay, FConfig, EGL_NO_CONTEXT, @FiAttribs[0]);
    end;
  end;

  Err := eglGetError;
	if Err <> EGL_SUCCESS then
    raise EGLContext.Create('Failed to create OES rendering context');

  if (ServiceContext <> nil) and (Self <> ServiceContext) then
  begin
    FSharedContexts.Add(ServiceContext);
    PropagateSharedContext;
  end;

  try
    Activate;
    FGL.Initialize;
    GLSLogger.LogInfoFmt('OpenGL ES %d.%d context successfully created', [MajorVersion, MinorVersion]);
  finally
    if Active then
      Deactivate;
  end;
end;

// DoCreateMemoryContext
//

procedure TGLOESContext.DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer);
begin
  raise Exception.Create(ClassName + ': Memory contexts not yet implemented');
end;

// DoShareLists
//

function TGLOESContext.DoShareLists(aContext: TGLContext): Boolean;
var
  otherRC: EGLContext;
begin
  Result := False;
  if aContext is TGLOESContext then
  begin
    otherRC := TGLOESContext(aContext).FContext;
    if FContext <> nil then
    begin
      if (FContext <> otherRC) then
      begin
        DestroyContext;
        FShareContext:= TGLOESContext(aContext);
        Result := True;
      end;
    end
    else
    begin
      FShareContext := TGLOESContext(aContext);
      Result := False;
    end;
  end
  else
    raise Exception.Create(cIncompatibleContexts);
end;

// DoDestroyContext
//

procedure TGLOESContext.DoDestroyContext;
begin
  if FContext <> EGL_NO_CONTEXT then
    if eglDestroyContext(FDisplay, FContext) <> EGL_TRUE then
      raise EGLContext.Create(cDeleteContextFailed);

  FDisplay := EGL_NO_DISPLAY;
  FContext := EGL_NO_CONTEXT;
  FShareContext := nil;
end;

// DoActivate
//

procedure TGLOESContext.DoActivate;
begin
  eglMakeCurrent(FDisplay, FSurface, FSurface, FContext);
  if eglGetError <> EGL_SUCCESS then
    raise EGLContext.Create(Format(cContextActivationFailed,
      [GetLastError, SysErrorMessage(GetLastError)]));

  if not FGL.IsInitialized then
    FGL.Initialize(CurrentGLContext = nil);
end;

// Deactivate
//

procedure TGLOESContext.DoDeactivate;
begin
  eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
end;

// IsValid
//

function TGLOESContext.IsValid: Boolean;
begin
  Result := FContext <> EGL_NO_CONTEXT;
end;

// SwapBuffers
//

procedure TGLOESContext.SwapBuffers;
begin
  eglSwapBuffers(FDisplay, FSurface);
  if eglGetError <> EGL_SUCCESS then
    raise EGLContext.Create('Swap buffer failed');
end;

// RenderOutputDevice
//

function TGLOESContext.RenderOutputDevice: HDC;
begin
  Result := FDC;
end;

initialization

  RegisterGLContextClass(TGLOESContext);

{$ENDIF GLS_OPENGL_ES}

end.
