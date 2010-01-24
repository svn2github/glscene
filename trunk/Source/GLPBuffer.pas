//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPBuffer<p>

  Simple handling of pixelbuffers.<p>
  TGLPixelBuffer can be used for offscreen rendering.<p>
  Goal is that it does not require the creation of a window.<p>

  <b>Historique : </b><font size=-1><ul>
      <li>24/01/10 - Yar - Removed initialization form constructor,
                           changes of use of desktop windows on foreground
                           (improved work on Delphi7 and Lazarus)
      <li>21/01/10 - DaStr - Bugfixed range check error
      <li>22/01/10 - Yar - Added to GLScene
  </ul></font>

}
// Previous header:
// 
// =============================================================================
//   Copyright © 2003-2004 by Sascha Willems - http://www.delphigl.de
// =============================================================================
//   --> visit the Delphi OpenGL Community - http://www.delphigl.com <--
// =============================================================================
//   Contents of this file are subject to the GNU Public License (GPL) which can
//   be obtained here : http://opensource.org/licenses/gpl-license.php
//   So only use this file if you fully unterstand that license!!!
// =============================================================================
//   Simple handling of pixelbuffers
// =============================================================================
unit GLPBuffer;

{$i GLScene.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Classes,
  SysUtils,
{$ENDIF}
  OpenGL1x;

type

  TGLPixelBuffer = class
  private
    DC: HDC;
    RC: HGLRC;
    ParentDC: HDC;
    ParentRC: HGLRC;
    fHandle: GLuint;
    fWidth: GLuint;
    fHeight: GLuint;
    fTextureID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(pWidth, pHeight: integer);
    function  IsLost: boolean;
    procedure Enable;
    procedure Disable;
    procedure Bind;
    procedure Release;

    property Handle: GLuint read fHandle;
    property Width: GLuint read fWidth;
    property Height: GLuint read fHeight;
    property TextureID: GLuint read fTextureID;
  end;

  EGLPixelBuffer = class(Exception);

implementation

constructor TGLPixelBuffer.Create;
begin
  inherited Create;
  ParentDC := 0;
  ParentRC := 0;
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
    WGL_TEXTURE_TARGET_ARB,
    WGL_TEXTURE_2D_ARB, 0);

  EmptyF: TGLFLoat = 0;
var
  PFormat: array[0..64] of TGLInt;
  NumPFormat: TGLUInt;
  TempW, TempH: TGLUInt;
{$ELSE}

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
  if fHandle > 0 then
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
  glGenTextures(1, @fTextureID);
{$ELSE}

{$ENDIF}
end;

destructor TGLPixelBuffer.Destroy;
begin
{$IFDEF MSWINDOWS}
  Disable;
  if fHandle<>0 then
  begin
    wglDeleteContext(RC);
    wglReleasePbufferDCARB(fHandle, DC);
    wglDestroyPbufferARB(fHandle);
  end;
{$ELSE}

{$ENDIF}
  inherited;
end;

function TGLPixelBuffer.IsLost: boolean;
var
  Flag: TGLUInt;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle<>0);
  Result := False;
  wglQueryPbufferARB(fHandle, WGL_PBUFFER_LOST_ARB, @Flag);
  if Flag <> 0 then
    Result := True;
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Enable;
begin
{$IFDEF MSWINDOWS}
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  wglMakeCurrent(DC, RC);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Disable;
begin
{$IFDEF MSWINDOWS}
  if (ParentDC = 0) or (ParentRC = 0) then
    wglMakeCurrent(0, 0)
  else
    wglMakeCurrent(ParentDC, ParentRC);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Bind;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle<>0);
  wglBindTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Release;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle<>0);
  wglReleaseTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ELSE}

{$ENDIF}
end;

end.

