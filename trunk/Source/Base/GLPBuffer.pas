//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPBuffer<p>

  Simple handling of pixelbuffers.<p>

  TGLPixelBuffer can be used for offscreen rendering.<p>
  It does not require a fully-functional rendering context.<p>

  <b>Historique : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>08/03/10 - Yar - Added more conditional brackets for Linux systems
      <li>27/01/10 - Yar - Updated header and moved to the /Source/Base/ folder
      <li>26/01/10 - DaStr - Bugfixed range check error, for real ;)
                             Enhanced TGLPixelBuffer.IsLost()
      <li>24/01/10 - Yar - Removed initialization from constructor,
                           changes of use of desktop windows on foreground
                           (improved work on Delphi7 and Lazarus)
      <li>21/01/10 - DaStr - Bugfixed range check error
      <li>22/01/10 - Yar - Added to GLScene (contributed by Sascha Willems)
  </ul></font>

   Copyright © 2003-2009 by Sascha Willems - http://www.saschawillems.de

   The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"; you may not use this file except in
   compliance with the License. You may obtain a copy of the License at
   http://www.mozilla.org/MPL/

   Software distributed under the License is distributed on an "AS IS"
   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
   License for the specific language governing rights and limitations
   under the License.
}

unit GLPBuffer;

{$I GLScene.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Classes,
{$ENDIF}
{$IFDEF Linux}
  LCLType,
  xlib,
{$ENDIF}
  SysUtils,
  OpenGLTokens,
  OpenGLAdapter;

type

  TGLPixelBuffer = class
  private
{$IFDEF MSWINDOWS}
    DC: HDC;
    RC: HGLRC;
    ParentDC: HDC;
    ParentRC: HGLRC;
    FHandle: HPBUFFERARB;
{$ENDIF}
{$IFDEF Linux}
    DC: PDisplay;
    RC: GLXContext;
    ParentDC: PDisplay;
    ParentRC: GLXContext;
    FHandle: LongInt;
{$ENDIF}
    FGL: TGLExtensionsAndEntryPoints;
    FWidth: GLint;
    FHeight: GLint;
    FTextureID: GLuint;
    FEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(pWidth, pHeight: integer);
    function IsLost: boolean;
    procedure Enable;
    procedure Disable;
    procedure Bind;
    procedure Release;

    property GL: TGLExtensionsAndEntryPoints read FGL;
    property Handle: GLint read fHandle;
    property Width: GLint read fWidth;
    property Height: GLint read fHeight;
    property TextureID: GLuint read fTextureID;
    property IsEnabled: Boolean read FEnabled;
  end;

  EGLPixelBuffer = class(Exception);
{$IFDEF Linux}
  TGLXFBConfigArray = array[0..MaxInt div (SizeOf(GLXFBConfig) * 2)] of GLXFBConfig;
  PGLXFBConfigArray = ^TGLXFBConfigArray;
{$ENDIF}

function PBufferService: TGLPixelBuffer;

implementation

{$IFDEF MULTITHREADOPENGL}
threadvar
{$ELSE}
var
{$ENDIF}
  vPBuffer: TGLPixelBuffer;

function PBufferService: TGLPixelBuffer;
begin
  if not Assigned(vPBuffer) then
    vPBuffer := TGLPixelBuffer.Create;
  Result := vPBuffer;
end;

constructor TGLPixelBuffer.Create;
begin
  inherited Create;
{$IFDEF MSWINDOWS}
  ParentDC := 0;
  ParentRC := 0;
{$ENDIF}
{$IFDEF Linux}
  DC := nil;
  fHandle := 0;
  RC := nil;
{$ENDIF}
  FGL := TGLExtensionsAndEntryPoints.Create;
end;

procedure TGLPixelBuffer.Initialize(pWidth, pHeight: integer);
{$IFDEF MSWINDOWS}
const
  PixelFormatAttribs: array[0..12] of TGLInt =
    (WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
    WGL_DRAW_TO_PBUFFER_ARB, GL_TRUE,
    WGL_COLOR_BITS_ARB, 24,
    WGL_ALPHA_BITS_ARB, 8,
    WGL_DEPTH_BITS_ARB, 24,
    WGL_DOUBLE_BUFFER_ARB, GL_FALSE, 0);

  PixelBufferAttribs: array[0..4] of TGLInt =
    (WGL_TEXTURE_FORMAT_ARB, WGL_TEXTURE_RGBA_ARB,
    WGL_TEXTURE_TARGET_ARB, WGL_TEXTURE_2D_ARB, 0);

  EmptyF: TGLFLoat = 0;
var
  PFormat: array[0..64] of TGLInt;
  NumPFormat: TGLenum;
  TempW, TempH: TGLInt;
{$ENDIF}
{$IFDEF Linux}
const
  PixelFormatAttribs: array[0..12] of TGLInt =
    (GLX_RENDER_TYPE, GLX_RGBA_BIT,
    GLX_DRAWABLE_TYPE, GLX_PBUFFER_BIT,
    GLX_BUFFER_SIZE, 24,
    GLX_ALPHA_SIZE, 8,
    GLX_DEPTH_SIZE, 24,
    GLX_DOUBLEBUFFER, GL_FALSE,
    0);

  PixelBufferAttribs: array[0..10] of Integer =
    (GLX_TEXTURE_FORMAT_EXT, GLX_TEXTURE_FORMAT_RGBA_EXT,
    GLX_TEXTURE_TARGET_EXT, GLX_TEXTURE_2D_EXT,
    GLX_PBUFFER_WIDTH, 1,
    GLX_PBUFFER_HEIGHT, 1,
    GLX_PRESERVED_CONTENTS, GL_TRUE,
    0);
var
  fbConfigs: PGLXFBConfigArray;
  nitems: integer;
{$ENDIF}
begin
  fWidth := pWidth;
  fHeight := pHeight;
{$IFDEF MSWINDOWS}
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  if ParentDC = 0 then
  begin
    ParentDC := GetDC(GetForegroundWindow);
    if ParentDC = 0 then
      raise EGLPixelBuffer.Create(
        'PixelBuffer->wglGetCurrentDC->Couldn''t obtain valid device context');
  end;

  if not wglChoosePixelFormatARB(ParentDC, @PixelFormatAttribs, @EmptyF,
    Length(PFormat), @PFormat, @NumPFormat) then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglChoosePixelFormatARB->No suitable pixelformat found');

  fHandle := wglCreatePBufferARB(ParentDC, PFormat[0], fWidth, fHeight,
    @PixelBufferAttribs);
  if fHandle <> 0 then
  begin
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_WIDTH_ARB, @TempW);
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_HEIGHT_ARB, @TempH);
  end
  else
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglCreatePBufferARB->Couldn''t obtain valid handle');

  DC := wglGetPBufferDCARB(fHandle);
  if DC = 0 then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t obtain valid DC for PBuffer');

  RC := wglCreateContext(DC);
  if RC = 0 then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t create rendercontext for PBuffer');

  wglMakeCurrent(DC, RC);
{$ELSE}
  ParentDC := glXGetCurrentReadDrawable;
  ParentRC := glxGetCurrentContext;

  DC := glXGetCurrentDisplay;
  if not Assigned(DC) then
  begin
    DC := XOpenDisplay(nil);
    if not Assigned(DC) then
      raise EGLPixelBuffer.Create(
        'PixelBuffer->glXGetCurrentDisplay->Couldn''t obtain valid Display');
  end;
  fbConfigs := glXChooseFBConfig(DC, XDefaultScreen(dpy), PixelFormatAttribs, @nitems);

  if fbConfigs = nil then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->glXChooseFBConfig->No suitable pixelformat found');

  fHandle := glXCreatePbuffer(DC, fbConfigs[0], PixelBufferAttribs);
  if fHandle <> 0 then
  begin
    glXQueryDrawable(DC, fHandle, GLX_PBUFFER_WIDTH, @fwidth);
    glXQueryDrawable(DC, fHandle, GLX_PBUFFER_HEIGHT, @fHeight);
  end
  else
    raise EGLPixelBuffer.Create(
      'PixelBuffer->glXCreatePbuffer->Couldn''t obtain valid handle');

  RC := glXCreateNewContext(DC, fbConfigs[0], GLX_RGBA_TYPE, nil, true);

  if RC = nil then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->glXCreateNewContext->Couldn''t create rendercontext for PBuffer');

  glXMakeContextCurrent(DC, fHandle, fHandle, RC);
{$ENDIF}
  FEnabled := True;
  FGL.Initialize;
  FGL.GenTextures(1, @fTextureID);
end;

destructor TGLPixelBuffer.Destroy;
begin
{$IFDEF MSWINDOWS}
  Disable;
  if (fHandle <> 0) then
  begin
    wglDeleteContext(RC);
    wglReleasePbufferDCARB(fHandle, DC);
    wglDestroyPbufferARB(fHandle);
  end;
{$ENDIF}
{$IFDEF Linux}
  Disable;
  if DC = nil then
    Exit;
  if (fHandle <> 0) then
  begin
    glXDestroyContext(DC, RC);
    glXDestroyPbuffer(DC, fHandle);
    fHandle := 0;
  end;
  XCloseDisplay(DC);
{$ENDIF}
  FGL.Free;
  inherited;
end;

function TGLPixelBuffer.IsLost: boolean;
{$IFDEF MSWINDOWS}
var
  Flag: TGLUInt;
begin
  Assert(fHandle <> 0);
  if wglQueryPbufferARB(fHandle, WGL_PBUFFER_LOST_ARB, @Flag) then
  begin
    Result := (Flag <> 0);
  end
  else
    Result := False;
{$ENDIF}
{$IFDEF Linux}
  begin
    Result := False;
{$ENDIF}
  end;

procedure TGLPixelBuffer.Enable;
begin
  if not FEnabled then
  begin
  {$IFDEF MSWINDOWS}
    ParentDC := wglGetCurrentDC;
    ParentRC := wglGetCurrentContext;
    if (DC <> 0)  and (RC <> 0) and (fHandle <> 0) then
      FEnabled := wglMakeCurrent(DC, RC);
  {$ENDIF}
  {$IFDEF Linux}
    ParentDC := glXGetCurrentReadDrawable;
    ParentRC := glxGetCurrentContext;
    if Assigned(DC) and Assigned(RC) and (fHandle <> 0) then
      FEnabled := glXMakeContextCurrent(DC, fHandle, fHandle, RC);
  {$ENDIF}
  end;
end;

procedure TGLPixelBuffer.Disable;
begin
  if FEnabled then
  begin
  {$IFDEF MSWINDOWS}
    if (ParentDC = 0) or (ParentRC = 0) then
      FEnabled := not wglMakeCurrent(0, 0)
    else
      FEnabled := not wglMakeCurrent(ParentDC, ParentRC);
  {$ENDIF}
  {$IFDEF Linux}
    if Assigned(DC) then
      if Assigned(ParentDC) and Assigned(ParentRC) then
        FEnabled := not glXMakeContextCurrent(ParentDC, 0, 0, ParentRC);
      else
        FEnabled := not glXMakeContextCurrent(DC, 0, 0, nil);
  {$ENDIF}
  end;
end;

procedure TGLPixelBuffer.Bind;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle <> 0);
  wglBindTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ENDIF}
{$IFDEF Linux}
{$WARNING GLPixelBuffer.Bind not yet implemented for your platform!}
{$ENDIF}
end;

procedure TGLPixelBuffer.Release;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle <> 0);
  wglReleaseTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ENDIF}
{$IFDEF Linux}
  //not needed
{$ENDIF}
end;

initialization

finalization

  vPBuffer.Free;
  vPBuffer := nil;

end.

