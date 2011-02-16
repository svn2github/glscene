{: GLCarbonContext<p>

   Carbon specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>10/06/09 - DanB - Added to main GLScene CVS repository (from GLScene-Lazarus).
      <li>14/11/08 - Creation
   </ul></font>
}
unit GLCarbonContext;

{$i ../GLScene.inc}

interface

uses
  MacOSAll,
  Classes, SysUtils,LCLType,  GLCrossPlatform, GLContext, LCLProc, Forms, Controls,
  OpenGLAdapter, OpenGLTokens, CarbonDef, CarbonCanvas, CarbonProc, CarbonPrivate;

type
   // TGLCarbonContext
   //
   {: A context driver for standard XOpenGL. }
   TGLCarbonContext = class (TGLContext)
      private
         { Private Declarations }
         FRC: TAGLContext;
         FBounds: TRect;
         FViewer, FForm: TControl;
         FIAttribs : packed array of Integer;

         function GetFormBounds: TRect;
         procedure BoundsChanged;
         function CreateWindow: WindowRef;
         procedure DestroyWindow(AWin: WindowRef);
      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure DropIAttrib(attrib : Integer);

         procedure DoCreateContext(outputDevice : HWND); override;
         procedure DoCreateMemoryContext(outputDevice : HWND;width, height : Integer; BufferCount : integer); override;
         function  DoShareLists(aContext : TGLContext): Boolean;  override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;
         //property DC: HWND read FDC;
         //property RenderingContext: GLXContext read FRC;
      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;

         function RenderOutputDevice : Integer; override;
   end;
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// -----------------------------------------------------------------
uses
  GLSLog;



resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X, %s';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateLegacyContext= 'Unable to create legacy context';

{ TGLCarbonContext }

function TGLCarbonContext.GetFormBounds: TRect;
begin
  Result.TopLeft := FForm.ScreenToClient(FViewer.ControlToScreen(Point(0, 0)));
  Result.Right := Result.Left + FViewer.Width;
  Result.Bottom := Result.Top + FViewer.Height;
end;

procedure TGLCarbonContext.BoundsChanged;
var
  Bounds: Array [0..3] of GLint;
begin
  Bounds[0] := FBounds.Left;
  Bounds[1] := FForm.Height - FBounds.Bottom;
  Bounds[2] := FBounds.Right - FBounds.Left;
  Bounds[3] := FBounds.Bottom - FBounds.Top;

  FGL.aSetInteger(FRC, AGL_BUFFER_RECT, @Bounds[0]);
  FGL.aEnable(FRC, AGL_BUFFER_RECT);

  {$MESSAGE Warn 'Removing child controls from clip region needs to be implemented'}
(*BoundsRGN := NewRgn;
  RectRgn(BoundsRGN, GetCarbonRect(TCarbonControlContext(DC).Owner.LCLObject.BoundsRect));

  aglSetInteger(FContext, AGL_CLIP_REGION, PGLInt(BoundsRGN));
  aglEnable(FContext, AGL_CLIP_REGION);*)

  FGL.aUpdateContext(FRC);
end;

procedure TGLCarbonContext.ClearIAttribs;
begin
  SetLength(FIAttribs, 1);
  FiAttribs[0]:=0;
end;

procedure TGLCarbonContext.AddIAttrib(attrib, value: Integer);
var
  N: Integer;
begin
  N := Length(FIAttribs);
  SetLength(FIAttribs, N+2);
  FiAttribs[N-1]:=attrib;
  FiAttribs[N]:=value;
  FiAttribs[N+1]:=0;
end;

procedure TGLCarbonContext.ChangeIAttrib(attrib, newValue: Integer);
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

procedure TGLCarbonContext.DropIAttrib(attrib: Integer);
var
  i: Integer;
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

function TGLCarbonContext.CreateWindow: WindowRef;
var
     windowAttrs : WindowAttributes;
     bounds      : MacOSAll.Rect;
     format            :   TAGLPixelFormat ;         //* OpenGL pixel format */
const
     Attributes: array[0..8] of GLint = (  //* OpenGL attributes */
                          AGL_RGBA, GL_TRUE,
                          AGL_GREEN_SIZE, 1,
                          AGL_DOUBLEBUFFER, GL_TRUE,
                          AGL_DEPTH_SIZE, 16,
                          AGL_NONE      );
begin
  // Lets create temporary window with glcontext
  windowAttrs := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute or kWindowStandardHandlerAttribute;

   MacOSAll.SetRect(bounds, 0, 0, 1, 1);
   MacOSAll.OffsetRect(bounds, 1, 1);

   CreateNewWindow(kDocumentWindowClass, windowAttrs, bounds, Result);
   ShowWindow(Result);
   //ShowHide(window,false);  //показать спрятать окошко
   format      := aglChoosePixelFormat(nil, 0, attributes);
   if  format<>nil then
     FRC := aglCreateContext(format, nil);
   if  format<>nil then
     aglDestroyPixelFormat(format);
   if (FRC<>nil) then
     begin
      aglSetDrawable(FRC, GetWindowPort(Result));
      aglSetCurrentContext(FRC);
     end;
end;

//Free Window and GLContext
//

procedure TGLCarbonContext.DestroyWindow(AWin: WindowRef);
begin

  if FRC <> nil then
  begin
    aglSetCurrentContext(nil);
    aglSetDrawable(nil, GetWindowPort(AWin));
    aglDestroyContext(FRC);
    FRC := nil;
  end;

  if @AWin <> nil then
  begin
    HideWindow(AWin);
    ReleaseWindow(AWin);
  end;
end;

procedure TGLCarbonContext.DoCreateContext(outputDevice: HWND);
var
  DC: TCarbonDeviceContext absolute outputDevice;
  Window: WindowRef;
  Disp: GDHandle;
  PixelFmt: TAGLPixelFormat;
  tempWnd: WindowRef;
begin
  tempWnd := CreateWindow;
  GLSLogger.Log('GLCarbonContext: Is created a temporary context');

  FGL.Initialize(True);

  DestroyWindow(tempWnd);
  GLSLogger.LogInfo('GLCarbonContext: Temporary rendering context destroyed');

  if not (CheckDC(outputDevice, 'DoCreateContext') or (DC is TCarbonControlContext)) then
  begin
    raise EGLContext.Create('Creating context failed: invalid device context!');
    GLSLogger.LogInfo('GLCarbonContext:Creating context failed: invalid device context!');
  end;

  FViewer := TCarbonControlContext(DC).Owner.LCLObject;
  FForm := FViewer.GetTopParent;
  if not (FForm is TCustomForm) then
  begin
    raise EGLContext.Create('Creating context failed: control not on the form!');
    GLSLogger.LogInfo('GLCarbonContext: Creating context failed: control not on the form!');
  end;

  Window := TCarbonWindow((FForm as TWinControl).Handle).Window;

  // create the AGL context
  Disp := GetMainDevice();
  GLSLogger.LogInfo('GLCarbonContext: Control Handle Accepted');

  AddIAttrib(AGL_WINDOW, GL_TRUE);
  AddIAttrib(AGL_RGBA, GL_TRUE);

  AddIAttrib(AGL_RED_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_GREEN_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_BLUE_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_DEPTH_SIZE, DepthBits);

  if AlphaBits > 0 then AddIAttrib(AGL_ALPHA_SIZE, AlphaBits);

  AddIAttrib(AGL_DEPTH_SIZE, DepthBits);

  if StencilBits > 0 then AddIAttrib(AGL_STENCIL_SIZE, StencilBits);
  if AccumBits > 0 then
  begin
    AddIAttrib(AGL_ACCUM_RED_SIZE, round(AccumBits/4));
    AddIAttrib(AGL_ACCUM_GREEN_SIZE, round(AccumBits/4));
    AddIAttrib(AGL_ACCUM_BLUE_SIZE, round(AccumBits/4));
  end;
  if AuxBuffers > 0 then AddIAttrib(AGL_AUX_BUFFERS, AuxBuffers);
  if (rcoDoubleBuffered in Options) then AddIAttrib(AGL_DOUBLEBUFFER, GL_TRUE);

  // choose the best compatible pixel format
  PixelFmt := FGL.aChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
  if PixelFmt = nil then
  begin
    if DepthBits >= 32 then ChangeIAttrib(AGL_DEPTH_SIZE, 24);
    PixelFmt := FGL.aChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
    if PixelFmt = nil then
    begin
      if DepthBits >= 24 then ChangeIAttrib(AGL_DEPTH_SIZE, 16);
      PixelFmt := FGL.aChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
      if PixelFmt = nil then
      begin
        AddIAttrib(AGL_RED_SIZE, 4);
        AddIAttrib(AGL_GREEN_SIZE, 4);
        AddIAttrib(AGL_BLUE_SIZE, 4);
        PixelFmt := FGL.aChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
      end;
    end;
  end;

  try
    GLSLogger.LogInfo('GLCarbonContext: AGLFormat it is choosed');

    FRC := FGL.aCreateContext(PixelFmt, nil);
    GLSLogger.LogInfo('GLCarbonContext: Context Created');

    FGL.aDestroyPixelFormat(PixelFmt);

    FGL.aSetDrawable(FRC, GetWindowPort(Window));
    GLSLogger.LogInfo('GLCarbonContext: SetDrawable');

    FBounds := GetFormBounds;
    BoundsChanged;
    GLSLogger.LogInfo('GLCarbonContext: BoundsChanged');

    Activate;
    FGL.Initialize;

    if FRC = nil then
      raise EGLContext.Create('Failed to create rendering context!');
    if PtrUInt(FRC) = AGL_BAD_CONTEXT then
      raise EGLContext.Create('Created bad context!');

    GLSLogger.LogInfo('Backward compatible core context successfully created');
  finally
    if Active then
      Deactivate;
  end;

end;

procedure TGLCarbonContext.DoCreateMemoryContext(outputDevice: HWND; width,
  height: Integer; BufferCount: integer);
begin
  {$MESSAGE Warn 'DoCreateMemoryContext: Needs to be implemented'}
end;

function  TGLCarbonContext.DoShareLists(aContext: TGLContext): Boolean;
begin
  {$MESSAGE Warn 'DoShareLists: Needs to be implemented'}
end;

procedure TGLCarbonContext.DoDestroyContext;
begin
  if (FGL.aGetCurrentContext = FRC) and
     (not FGL.aSetCurrentContext(nil)) then
    raise EGLContext.Create('Failed to deselect rendering context');

  FGL.aDestroyContext(FRC);
  FRC := nil;
end;

procedure TGLCarbonContext.DoActivate;
var
  B: TRect;
begin
  B := GetFormBounds;
  if (B.Left <> FBounds.Left) or (B.Top <> FBounds.Top) or
    (B.Right <> FBounds.Right) or (B.Bottom <> FBounds.Bottom) then
  begin
    FBounds := B;
    BoundsChanged;
      GLSLogger.LogInfo('DoActivate-BoundsChanged');
  end;

  if (not FGL.aSetCurrentContext(FRC)) then
    raise EGLContext.Create(cContextActivationFailed);

  if not FGL.IsInitialized then
     FGL.Initialize;
end;

procedure TGLCarbonContext.DoDeactivate;
begin
  if (not FGL.aSetCurrentContext(nil)) then
    raise EGLContext.Create(cContextDeactivationFailed);
end;

constructor TGLCarbonContext.Create;
begin
  inherited Create;
  ClearIAttribs;

end;

destructor TGLCarbonContext.Destroy;
begin
  inherited Destroy;
end;

function TGLCarbonContext.IsValid: Boolean;
begin
  Result := (FRC <> nil);
end;

procedure TGLCarbonContext.SwapBuffers;
begin
  if (FRC <> nil) and (rcoDoubleBuffered in Options) then
    FGL.aSwapBuffers(FRC);
end;

function TGLCarbonContext.RenderOutputDevice: Integer;
begin
  Result := 0;
end;

end.
