{: GLGraphics<p>

	Utility class and functions to manipulate a bitmap in OpenGL's default
   byte order (GL_RGBA vs TBitmap's GL_BGRA)<p>

   Nota: TGLBitmap32 has support for Alex Denissov's Graphics32 library
   (http://www.g32.org), just make sure the GLS_Graphics32_SUPPORT conditionnal
   is active in GLScene.inc and recompile.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>20/01/02 - EG - Fixed BGR24/RGB24 last pixel transfer
      <li>17/01/02 - EG - Faster assignments from bitmaps (approx. x2),
                          Added AssignFromBitmap24WithoutRGBSwap
      <li>28/12/01 - EG - Graphics32 support added
      <li>15/12/01 - EG - Texture target support
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>31/08/01 - EG - 24bits Bitmaps are now made opaque by default
      <li>12/08/01 - EG - Now detects and uses GL_SGIS_generate_mipmap
      <li>20/02/01 - EG - Fixed SetHeight & SetWidth (thx Nelson Chu)
      <li>14/02/01 - EG - Simplified RegisterAsOpenGLTexture
      <li>15/01/01 - EG - Fixed RegisterAsOpenGLTexture (clamping)
      <li>14/01/01 - EG - Fixed isEmpty (was invalid for rectangles)
      <li>08/10/00 - EG - Fixed RegisterAsOpenGLTexture and Assign(nil)
      <li>25/09/00 - EG - First operational code
	   <li>19/08/00 - EG - Creation
	</ul></font>
}
unit GLGraphics;

interface

{$i GLScene.inc}

uses Classes,
{$ifdef GLS_Graphics32_SUPPORT}
   G32,
{$endif}
   GLMisc, OpenGL12, GLCrossPlatform;

type

   TColor = TDelphiColor;

   // TGLPixel24
   //
   TGLPixel24 = packed record
      r, g, b : Byte;
   end;
   PGLPixel24 = ^TGLPixel24;

   // TGLPixel32
   //
   TGLPixel32 = packed record
      r, g, b, a : Byte;
   end;
   PGLPixel32 = ^TGLPixel32;

   TGLPixel32Array = array [0..MaxInt shr 3] of TGLPixel32;
   PGLPixel32Array = ^TGLPixel32Array;

	// TGLBitmap32
	//
   {: Contains and manipulates a 32 bits (24+8) bitmap.<p>
      This is the base class for preparing and manipulating textures in GLScene,
      this function does not rely on a windows handle and should be used for
      in-memory manipulations only.<br>
      16 bits textures are automatically converted to 24 bits and an opaque (255)
      alpha channel is assumed for all planes, the byte order is as specified
      in GL_RGBA. If 32 bits is used in this class, it can however output 16 bits texture
      data for use in OpenGL.<p>
      The class has support for registering its content as a texture, as well
      as for directly drawing/reading from the current OpenGL buffer. }
	TGLBitmap32 = class (TPersistent)
	   private
	      { Private Declarations }
         FData : PGLPixel32Array;
         FWidth, FHeight : Integer;
         FDataSize : Integer;
         FVerticalReverseOnAssignFromBitmap : Boolean;

	   protected
	      { Protected Declarations }
         procedure SetWidth(val : Integer);
         procedure SetHeight(const val : Integer);
         function GetScanLine(index : Integer) : PGLPixel32Array;
         procedure AssignFrom24BitsBitmap(aBitmap : TGLBitmap);
         procedure AssignFrom32BitsBitmap(aBitmap : TGLBitmap);
{$ifdef GLS_Graphics32_SUPPORT}
         procedure AssignFromBitmap32(aBitmap32 : TBitmap32);
{$endif}

	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         {: Accepts TGLBitmap32 and TGraphic subclasses. }
         procedure Assign(Source: TPersistent); override;
         {: Assigns from a 24 bits bitmap without swapping RGB.<p>
            This is faster than a regular assignment, but R and B channels
            will be reversed (from what you would view in a TImage). Suitable
            if you do you own drawing and reverse RGB on the drawing side.<br>
            If you're after speed, don't forget to set the bitmap's dimensions
            to a power of two! }
         procedure AssignFromBitmap24WithoutRGBSwap(aBitmap : TGLBitmap);

         {: Create a 32 bits TBitmap from self content. }
         function Create32BitsBitmap : TGLBitmap;

         {: True if the bitmap is empty (ie. width or height is zero). }
	      function IsEmpty : Boolean;

         {: Width of the bitmap.<p>
            Will be forced to the nearest superior multiple of 4, f.i. writing
            Width:=6 is equivalent to writing Width:=8. }
         property Width : Integer read FWidth write SetWidth;
         {: Height of the bitmap. }
         property Height : Integer read FHeight write SetHeight;
         {: Size of the bitmap data in bytes. }
         property DataSize : Integer read FDataSize;

         {: Access to a specific Bitmap ScanLine.<p>
            index should be in the [0; Height[ range.<p>
            Warning : this function is NOT protected against invalid indexes,
            and invoking it is invalid if the bitmap is Empty. }
         property ScanLine[index : Integer] : PGLPixel32Array read GetScanLine;

         property VerticalReverseOnAssignFromBitmap : Boolean read FVerticalReverseOnAssignFromBitmap write FVerticalReverseOnAssignFromBitmap;

         {: Grants direct access to the bitmap's data.<p>
            This property is equivalent to ScanLine[0], and may be nil if the
            bitmap is empty. }
         property Data : PGLPixel32Array read FData;

         {: Set Alpha channel values to the pixel intensity.<p>
            The intensity is calculated as the mean of RGB components. }
         procedure SetAlphaFromIntensity;
         {: Set Alpha channel to 0 for pixels of given color, 255 for others).<p>
            This makes pixels of given color totally transparent while the others
            are completely opaque. }
         procedure SetAlphaTransparentForColor(const aColor : TColor); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel32); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel24); overload;
         {: Set Alpha channel values to given byte value. }
         procedure SetAlphaToValue(const aValue : Byte);
         {: Set Alpha channel values to given float [0..1] value. }
         procedure SetAlphaToFloatValue(const aValue : Single);
         {: Inverts the AlphaChannel component.<p>
            What was transparent becomes opaque and vice-versa. }
         procedure InvertAlpha;
         {: AlphaChannel components are replaced by their sqrt.<p> }
         procedure SqrtAlpha;

         {: Registers the bitmap's content as an OpenGL texture map.<p>
            Legal values for bytesPerPixel are :<ul>
            <li>4 : RGB+A (32 bits)
            <li>3 : RGB (24 bits)
            <li>1 : Alpha channel only (8 bits)
            </ul>The texWidth and texHeight parameters are used to return
            the actual width and height of the texture (that can be different
            from the size of the bitmap32). }
         procedure RegisterAsOpenGLTexture(target : TGLUInt;
                                           minFilter : TGLMinFilter;
                                           texFormat : Integer;
                                           var texWidth, texHeight : Integer); overload;
         {: Helper version of RegisterAsOpenGLTexture. }
         procedure RegisterAsOpenGLTexture(target : TGLUInt;
                                           minFilter : TGLMinFilter;
                                           texFormat : Integer); overload;

         {: Reads the given area from the current active OpenGL rendering context.<p>
            The best spot for reading pixels is within a SceneViewer's PostRender
            event : the scene has been fully rendered and the OpenGL context
            is still active. }
         procedure ReadPixels(const area : TGLRect);
         {: Draws the whole bitmap at given position in the current OpenGL context.<p>
            This function must be called with a rendering context active.<p>
            Blending and Alpha channel functions are not altered by this function
            and must be adjusted separately. }
         procedure DrawPixels(const x, y : Single);
	end;

procedure BGR24ToRGBA32(src, dest : Pointer; pixelCount : Integer);
procedure RGB24ToRGBA32(src, dest : Pointer; pixelCount : Integer);
procedure BGRA32ToRGBA32(src, dest : Pointer; pixelCount : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Geometry;

// BGR24ToRGBA32
//
procedure BGR24ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
{begin
   while pixelCount>0 do begin
      PChar(dest)[0]:=PChar(src)[2];
      PChar(dest)[1]:=PChar(src)[1];
      PChar(dest)[2]:=PChar(src)[0];
      PChar(dest)[3]:=#255;
      dest:=Pointer(Integer(dest)+4);
      src:=Pointer(Integer(src)+3);
      Dec(pixelCount);
   end; }
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         or    eax, $FF
         bswap eax
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   cx, [edi+1]
         shl   ecx, 16
         mov   ah, [edi]
         mov   al, $FF
         and   eax, $FFFF
         or    eax, ecx
         bswap eax
         mov   [edx], eax
@@Done:
         pop   edi
end;

// RGB24ToRGBA32
//
procedure RGB24ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         or    eax, $FF000000
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   ax, [edi+1]
         shl   eax, 8
         mov   al, [edi];
         or    eax, $FF000000
         mov   [edx], eax
@@Done:
         pop   edi
end;

// BGRA32ToRGBA32
//
procedure BGRA32ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
{begin
   while pixelCount>0 do begin
      PChar(dest)[0]:=PChar(src)[2];
      PChar(dest)[1]:=PChar(src)[1];
      PChar(dest)[2]:=PChar(src)[0];
      PChar(dest)[3]:=PChar(src)[3];
      dest:=Pointer(Integer(dest)+4);
      src:=Pointer(Integer(src)+4);
      Dec(pixelCount);
   end; }
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         mov   al, [edi+3]
         bswap eax
         mov   [edx], eax
         add   edi, 4
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Done:
         pop   edi 
end;

// ------------------
// ------------------ TGLBitmap32 ------------------
// ------------------

// Create
//
constructor TGLBitmap32.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TGLBitmap32.Destroy;
begin
   FreeMem(FData);
	inherited Destroy;
end;

// Assign
//
procedure TGLBitmap32.Assign(Source: TPersistent);
var
   bmp : TGLBitmap;
   graphic : TGLGraphic;
begin
   if Source=nil then begin
      FDataSize:=0;
      FWidth:=0;
      FHeight:=0;
      FreeMem(FData);
   end else if Source is TGLBitmap32 then begin
      // duplicate the data
      FDataSize:=TGLBitmap32(Source).DataSize;
      FWidth:=TGLBitmap32(Source).Width;
      FHeight:=TGLBitmap32(Source).Height;
      ReallocMem(FData, FDataSize);
      Move(TGLBitmap32(Source).Data^, Data^, DataSize);
   end else if Source is TGLGraphic then begin
      if (Source is TGLBitmap) and (TGLBitmap(Source).PixelFormat in [glpf24bit, glpf32bit])
            and ((TGLBitmap(Source).Width and 3)=0) then begin
         if TGLBitmap(Source).PixelFormat=glpf24bit then
            AssignFrom24BitsBitmap(TGLBitmap(Source))
         else AssignFrom32BitsBitmap(TGLBitmap(Source))
      end else begin
         graphic:=TGLGraphic(Source);
         bmp:=TGLBitmap.Create;
         try
            bmp.PixelFormat:=glpf24bit;
            bmp.Height:=graphic.Height;
            if (graphic.Width and 3)=0 then begin
               bmp.Width:=graphic.Width;
               bmp.Canvas.Draw(0, 0, graphic);
            end else begin
               bmp.Width:=(graphic.Width and $FFFC)+4;
               bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), graphic);
            end;
            AssignFrom24BitsBitmap(bmp);
         finally
            bmp.Free;
         end;
      end;
{$ifdef GLS_Graphics32_SUPPORT}
   end else if Source is TBitmap32 then begin
      AssignFromBitmap32(TBitmap32(Source));
{$endif}
   end else inherited;
end;

// AssignFrom24BitsBitmap
//
procedure TGLBitmap32.AssignFrom24BitsBitmap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf24bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   if VerticalReverseOnAssignFromBitmap then begin
      pSrc:=aBitmap.ScanLine[Height-1];
      rowOffset:=Integer(aBitmap.ScanLine[Height-2])-Integer(pSrc);
   end else begin
      pSrc:=aBitmap.ScanLine[0];
      rowOffset:=Integer(aBitmap.ScanLine[1])-Integer(pSrc);
   end;
   for y:=0 to Height-1 do begin
      BGR24ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width*4);
      Inc(pSrc, rowOffset);
   end;
end;

// AssignFromBitmap24WithoutRGBSwap
//
procedure TGLBitmap32.AssignFromBitmap24WithoutRGBSwap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf24bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   if VerticalReverseOnAssignFromBitmap then begin
      pSrc:=aBitmap.ScanLine[Height-1];
      rowOffset:=Integer(aBitmap.ScanLine[Height-2])-Integer(pSrc);
   end else begin
      pSrc:=aBitmap.ScanLine[0];
      rowOffset:=Integer(aBitmap.ScanLine[1])-Integer(pSrc);
   end;
   for y:=0 to Height-1 do begin
      RGB24ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width*4);
      Inc(pSrc, rowOffset);
   end;
end;

// AssignFrom32BitsBitmap
//
procedure TGLBitmap32.AssignFrom32BitsBitmap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf32bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   if VerticalReverseOnAssignFromBitmap then begin
      pSrc:=aBitmap.ScanLine[Height-1];
      rowOffset:=Integer(aBitmap.ScanLine[Height-2])-Integer(pSrc);
   end else begin
      pSrc:=aBitmap.ScanLine[0];
      rowOffset:=Integer(aBitmap.ScanLine[1])-Integer(pSrc);
   end;
   for y:=0 to Height-1 do begin
      BGRA32ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width*4);
      Inc(pSrc, rowOffset);
   end;
end;

{$ifdef GLS_Graphics32_SUPPORT}
// AssignFromBitmap32
//
procedure TGLBitmap32.AssignFromBitmap32(aBitmap32 : TBitmap32);
var
   y : Integer;
   pSrc, pDest : PChar;
begin
   Assert((aBitmap32.Width and 3)=0);
   FWidth:=aBitmap32.Width;
   FHeight:=aBitmap32.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   for y:=0 to Height-1 do begin
      if VerticalReverseOnAssignFromBitmap then
         pSrc:=PChar(aBitmap32.ScanLine[Height-1-y])
      else pSrc:=PChar(aBitmap32.ScanLine[y]);
      BGRA32ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width*4);
   end;
end;
{$endif}

// Create32BitsBitmap
//
function TGLBitmap32.Create32BitsBitmap : TGLBitmap;
var
   y, x, x4 : Integer;
   pSrc, pDest : PChar;
begin
   Result:=TGLBitmap.Create;
   Result.PixelFormat:=glpf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
   pSrc:=@PChar(FData)[Width*4*(Height-1)];
   for y:=0 to Height-1 do begin
      pDest:=Result.ScanLine[y];
      for x:=0 to Width-1 do begin
         x4:=x*4;
         pDest[x4+0]:=pSrc[x4+2];
         pDest[x4+1]:=pSrc[x4+1];
         pDest[x4+2]:=pSrc[x4+0];
         pDest[x4+3]:=pSrc[x4+3];
      end;
      Dec(pSrc, Width*4);
   end;
end;

// IsEmpty
//
function TGLBitmap32.IsEmpty : Boolean;
begin
	Result:=(Width=0) or (Height=0);
end;

// SetWidth
//
procedure TGLBitmap32.SetWidth(val : Integer);
begin
   if (val and 3)>0 then
      val:=(val and $FFFC)+4;
   if val<>FWidth then begin
      Assert(val>=0);
      FWidth:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// SetHeight
//
procedure TGLBitmap32.SetHeight(const val : Integer);
begin
   if val<>FHeight then begin
      Assert(val>=0);
      FHeight:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// GetScanLine
//
function TGLBitmap32.GetScanLine(index : Integer) : PGLPixel32Array;
begin
   Result:=PGLPixel32Array(@FData[index*Width]);
end;

// SetAlphaFromIntensity
//
procedure TGLBitmap32.SetAlphaFromIntensity;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=(Integer(r)+Integer(g)+Integer(b)) div 3;
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TColor);
var
   color : TGLPixel24;
begin
   color.r:=GetRValue(aColor);
   color.g:=GetGValue(aColor);
   color.b:=GetBValue(aColor);
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel32);
var
   color : TGLPixel24;
begin
   color.r:=aColor.r;
   color.g:=aColor.g;
   color.b:=aColor.b;
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel24);
var
   i : Integer;
   intCol : Integer;
begin
   intCol:=(PInteger(@aColor)^) and $FFFFFF;
   for i:=0 to (FDataSize div 4)-1 do
      if PInteger(@FData[i])^ and $FFFFFF=intCol then
         FData[i].a:=0
      else FData[i].a:=255;
end;

// SetAlphaToValue
//
procedure TGLBitmap32.SetAlphaToValue(const aValue : Byte);
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=aValue
end;

// SetAlphaToFloatValue
//
procedure TGLBitmap32.SetAlphaToFloatValue(const aValue : Single);
begin
   SetAlphaToValue(Byte(Trunc(aValue*255) and 255));
end;

// InvertAlpha
//
procedure TGLBitmap32.InvertAlpha;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=255-FData[i].a;
end;

// SqrtAlpha
//
procedure TGLBitmap32.SqrtAlpha;
var
   i : Integer;
	sqrt255Array : PSqrt255Array;
begin
   sqrt255Array:=GetSqrt255Array;
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=sqrt255Array[(Integer(r)+Integer(g)+Integer(b)) div 3];
end;

// RegisterAsOpenGLTexture
//
procedure TGLBitmap32.RegisterAsOpenGLTexture(target : TGLUInt;
                                              minFilter : TGLMinFilter;
                                              texFormat : Integer);
var
   tw, th : Integer;
begin
   RegisterAsOpenGLTexture(target, minFilter, texFormat, tw, th);
end;

// RegisterAsOpenGLTexture
//
procedure TGLBitmap32.RegisterAsOpenGLTexture(target : TGLUInt;
                                              minFilter : TGLMinFilter;
                                              texFormat : Integer;
                                              var texWidth, texHeight : Integer);
var
   w2, h2, maxSize : Integer;
   buffer : Pointer;
begin
   if DataSize>0 then begin
      w2:=RoundUpToPowerOf2(Width);
      h2:=RoundUpToPowerOf2(Height);
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
      if w2>maxSize then w2:=maxSize;
      if h2>maxSize then h2:=maxSize;
      texWidth:=w2;
      texHeight:=h2;
      if (w2<>Width) or (h2<>Height) then begin
         GetMem(buffer, w2*h2*4);
         gluScaleImage(GL_RGBA, Width, Height, GL_UNSIGNED_BYTE, Data, w2, h2,
                       GL_UNSIGNED_BYTE, buffer);
      end else buffer:=Pointer(FData);
      try
   		case minFilter of
			   miNearest, miLinear :
		   		glTexImage2d(target, 0, texFormat, w2, h2, 0,
	   							 GL_RGBA, GL_UNSIGNED_BYTE, buffer)
   		else
            if GL_SGIS_generate_mipmap and (target=GL_TEXTURE_2D) then begin
               // hardware-accelerated when supported
               glTexParameteri(target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
		   		glTexImage2d(target, 0, texFormat, w2, h2, 0,
	   							 GL_RGBA, GL_UNSIGNED_BYTE, buffer);
            end else begin
               // slower (software mode)
   		   	gluBuild2DMipmaps(target, texFormat, w2, h2,
	      								GL_RGBA, GL_UNSIGNED_BYTE, buffer);
            end;
   		end;
		finally
         if buffer<>Pointer(FData) then
   			FreeMem(buffer);
		end;
   end;
end;

// ReadPixels
//
procedure TGLBitmap32.ReadPixels(const area : TGLRect);
begin
   FWidth:=(area.Right-area.Left) and $FFFC;
   FHeight:=(area.Bottom-area.Top);
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   glReadPixels(0, 0, FWidth, FHeight, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// DrawPixels
//
procedure TGLBitmap32.DrawPixels(const x, y : Single);
begin
   glRasterPos2f(x, y);
   glDrawPixels(Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations

end.

