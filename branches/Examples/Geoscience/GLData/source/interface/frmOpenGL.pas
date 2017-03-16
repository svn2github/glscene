{-----------------------------------------------------------------------------
 Unit Name: frmOpenGL
 Author:    HochwimmerA
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit frmOpenGL;

interface

uses
  Windows, Forms, GLScene, Classes, Controls, Buttons, StdCtrls, ComCtrls,
     CommCtrl, ExtCtrls, Graphics, OpenGLTokens;

type
  TformOpenGL = class(TForm)
    pcInfo: TPageControl;
    pnlBottom: TPanel;
    bOK: TBitBtn;
    tsCommon: TTabSheet;
    tsBufferPixelDepths: TTabSheet;
    tsMaxValues: TTabSheet;
    tsExtensions: TTabSheet;
    lbExtensions: TListBox;
    memCommon: TMemo;
    memBuffer: TMemo;
    memValues: TMemo;
    procedure lbExtensionsDblClick(Sender: TObject);
  private
     
  public
    procedure GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
  end;

implementation

uses
  OpenGL1x,shellapi,SysUtils;

{$R *.dfm}
// ----- TformOpenGL.lbExtensionsDblClick --------------------------------------
// double click to go to an extensions page...
procedure TformOpenGL.lbExtensionsDblClick(Sender: TObject);

var
   p : Integer;
   url, buf : String;

begin
   with lbExtensions do begin
      if ItemIndex<0 then Exit;
      url:=Items[ItemIndex];
   end;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:=Copy(url, p+1, 255);
   if (buf<>'GL') and (buf<>'WGL') and (buf<>'GLX') then Exit;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:= 'http://oss.sgi.com/projects/ogl-sample/registry/'
        +buf+'/'+Copy(url, p+1, 255)+'.txt';
   ShellExecute(0, 'open', PChar(url), nil, nil, SW_SHOW);
end;
// ----- TformOpenGL.GetInfoFrom -----------------------------------------------
procedure TformOpenGL.GetInfoFrom(aSceneBuffer : TGLSceneBuffer);

const
  DRIVER_MASK = PFD_GENERIC_FORMAT or PFD_GENERIC_ACCELERATED;

var
  pfd : TPixelFormatDescriptor;
  i,pixelformat : integer;
  extstr,sTemp : string;

  function IntLimitToString(const aLimit:TGLLimitType):string;

  begin
    result := IntToStr(aSceneBuffer.LimitOf[aLimit]);
  end;

begin
  with aSceneBuffer do
  begin
    RenderingContext.Activate;
    memCommon.Lines.Clear;
    sTemp := StrPas(PChar(glGetString(GL_VENDOR)));
    memCommon.Lines.Add('OpenGL Vendor:'+#9+sTemp);
    sTemp := StrPas(PChar(glGetString(GL_RENDERER)));
    memCommon.Lines.Add('Renderer:'+#9+sTemp);
// figure out the driver type
    if (DRIVER_MASK and pfd.dwFlags) = 0 then
      memCommon.Lines.Add('Acceleration:'+#9+'Installable Client Driver')
    else if (DRIVER_MASK and pfd.dwFlags ) = DRIVER_MASK then
      memCommon.Lines.Add('Acceleration:'+#9+'Mini-Client Driver')
    else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then
      memCommon.Lines.Add('Acceleration:'+#9+'Generic Software Driver');
    memCommon.Lines.Add('Version Info:'+#9+
      StrPas(PChar(glGetString(GL_VERSION))));
    memCommon.Lines.Add('');
// stereo
    if (pfd.dwFlags and PFD_STEREO) > 0 then
      memCommon.Lines.Add('Stereo Enabled:'+#9+'Yes')
    else
      memCommon.Lines.Add('Stereo Enabled:'+#9+'No');
    memCommon.Lines.Add('');
// double buffering
    if DoubleBuffered then
    begin
      memCommon.Lines.Add('Double Buffered:'+#9+'Yes');
      sTemp := '';
      if (pfd.dwFlags and PFD_SWAP_EXCHANGE) > 0 then sTemp:='exchange';
      if Length(sTemp) > 0 then sTemp:=sTemp+', ';
      if (pfd.dwFlags and PFD_SWAP_COPY) > 0 then sTemp:=sTemp+'copy';
      if Length(sTemp) = 0 then sTemp:='no info available';
      memCommon.Lines.Add('Buffer Switch:'+#9+sTemp);
    end else
    begin
        memCommon.Lines.Add('Double Buffered:'+#9+'No');
        memCommon.Lines.Add('Buffer Switch:'+#9+'n/a');
    end;

// Buffer and Pixel Depths
    memBuffer.Clear;
    PixelFormat:=GetPixelFormat(Canvas.Handle);
    DescribePixelFormat(Canvas.Handle,PixelFormat,SizeOf(pfd), PFD);
    sTemp := Format('red: %d, green: %d, blue: %d, alpha: %d bits',
      [LimitOf[limRedBits],LimitOf[limGreenBits],LimitOf[limBlueBits],
        LimitOf[limAlphaBits]]);
    memBuffer.Lines.Add('Colour Buffer:'+#9+sTemp);
    sTemp := Format('%d bits', [LimitOf[limDepthBits]]);
    memBuffer.Lines.Add('Depth Buffer:'+#9+sTemp);
    sTemp := Format('%d bits', [LimitOf[limStencilBits]]);
    memBuffer.Lines.Add('Stencil Buffer:'+#9+sTemp);
    sTemp := Format('red: %d, green: %d, blue: %d, alpha: %d bits',
      [LimitOf[limAccumRedBits],LimitOf[limAccumGreenBits],
      LimitOf[limAccumBlueBits],LimitOf[limAccumAlphaBits]]);
    memBuffer.Lines.Add('Accum. Buffer:'+#9+sTemp);
    memBuffer.Lines.Add('Auxilliary Buffers:'+#9+IntLimitToString(limAuxBuffers));
    memBuffer.Lines.Add('Subpixels:'+#9+IntLimitToString(limSubPixelBits));
    memBuffer.Lines.Add('');
    memBuffer.Lines.Add('Supported Overlays:'+#9+IntToStr(pfd.bReserved and 7));
    memBuffer.Lines.Add('Supported Underlays:'+#9+IntToStr(pfd.bReserved shr 3));

// Maximum Values
    memValues.Lines.Clear;
    memValues.Lines.Add('Clip Planes:'+#9+IntLimitToString(limClipPlanes));
    memValues.Lines.Add('Evaluation Order:'+#9+IntLimitToString(limEvalOrder));
    memValues.Lines.Add('Light Sources:'+#9+IntLimitToString(limLights));
    memValues.Lines.Add('List Nesting:'+#9+IntLimitToString(limListNesting));
    memValues.Lines.Add('Modelview Stack:'+#9+IntLimitToString(limModelViewStack));
    memValues.Lines.Add('');
    memValues.Lines.Add('Viewport Dimensions:'+#9+IntLimitToString(limViewPortDims));
    memValues.Lines.Add('');
    memValues.Lines.Add('Name Stack:'+#9+IntLimitToString(limNameStack));
    memValues.Lines.Add('Pixel Map Table:'+#9+IntLimitToString(limPixelMapTable));
    memValues.Lines.Add('Projection Stack:'+#9+IntLimitToString(limProjectionStack));
    memValues.Lines.Add('Texture Size:'+#9+IntLimitToString(limTextureSize));
    memValues.Lines.Add('Texture Stack:'+#9+IntLimitToString(limTextureStack));
    memValues.Lines.Add('Texture Units:'+#9+IntLimitToString(limNbTextureUnits));

// Extensions
    ExtStr:=PChar(glGetString(GL_EXTENSIONS));
    lbExtensions.Clear;
    while Length(ExtStr) > 0 do begin
      I:=Pos(' ',ExtStr);
      if I = 0 then I:=255;
      lbExtensions.Items.Add(Copy(ExtStr,1,I-1));
      Delete(ExtStr,1,I);
    end;
  end;
end;
// =============================================================================
end.

