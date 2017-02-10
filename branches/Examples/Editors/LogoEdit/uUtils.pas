unit uUtils;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  math;

procedure Stretch(imgSrc,imgDst:TBitmap;q:integer=0);

implementation

procedure Stretch(imgSrc,imgDst:TBitmap;q:integer=0);

  function flt(v:single):single;
  const c=1/3;
  var tt:single;
    function s(v:single):single;begin if v<>0 then begin v:=v*pi;Result:=sin(v)/v end else Result:=1;end;
  begin
    if v<0 then v:=-v;result:=0;
    case q of
      2:if v<1 then result:=1-v;
      3:begin tt:=Sqr(v);if v<1 then begin v:=7*v*tt-12*tt+5.333333;Result:=v*0.1666667;end
        else if v<2 then begin v:=-2.333333*v*tt+12*tt-20*v+10.666667;Result:=v*0.16666667;end;end;
      4:if v<3 then Result:=s(v)*s(v*c);
      end;
    end;

type TContr=record pixel:integer;weight:single;end;
  TContrList=array[0..0]of TContr;PContrList=^TContrList;
  TCList=record n:integer;p:PContrList;end;
  TCListList=array[0..0]of TCList;PCListList=^TCListList;

  TRGB=packed record r,g,b:single;end;
  TColorRGB=packed record r,g,b:BYTE;end;
  PColorRGB=^TColorRGB;
  TRGBList=packed array[0..0]of TColorRGB;
  PRGBList=^TRGBList;

var
  dx,dy,dw,c,weight:single;
  a1,a2,a3,d1,d2,l,r,n,_dx,_dy:integer;

  bmp:TBitmap;
  contrib:PCListList;
  rgb:TRGB;
  color:TColorRGB;

  sLn,dLn:PRGBList;
  dPix:PColorRGB;

begin

  //_dx:=rDst.Right-rDst.Left;_dy:=rDst.Bottom-rDst.Top;
  _dx:=imgDst.Width;_dy:=imgDst.Height;

  if q=0 then if(imgSrc.Width>_dx)or(imgSrc.Height>_dy)then q:=4 else q:=2;

  {if q=1 then begin
    stretchBlt(imgDst.Canvas.Handle,rDst.Left,rDst.Top,rDst.Right-rDst.Left,rDst.Bottom-rDst.Top,imgSrc.Canvas.Handle,0,0,imgSrc.Width,imgSrc.Height,srccopy);
    exit;
    end;}

  bmp:=TBitmap.Create;
  bmp.Height:=imgSrc.Height;bmp.Width:=_dx;

  dx:=(_dx-1)/(imgSrc.Width-1);dy:=(_dy-1)/(imgSrc.Height-1);

  imgSrc.PixelFormat:=pf24bit;imgDst.PixelFormat:=pf24bit;bmp.PixelFormat:=pf24bit;

  GetMem(contrib,_dx*sizeof(TCList));

  if dx<1 then begin dw:=3/dx;
    for a1:=0 to _dx-1 do begin
      contrib^[a1].n:=0;c:=a1/dx;l:=floor(c-dw);r:=ceil(c+dw);
      GetMem(contrib^[a1].p,trunc(dw*2+1)*8);
      for a2:=l to r do begin weight:=flt((c-a2)*dx)*dx;if weight=0 then continue;
        if a2<0 then n:=-a2 else if a2>=imgSrc.Width then n:=2*imgSrc.Width-a2-1 else n:=a2;
        a3:=contrib^[a1].n;contrib^[a1].n:=contrib^[a1].n+1;
        contrib^[a1].p^[a3].pixel:=n;contrib^[a1].p^[a3].weight:=weight;
        end;
      end;
    end
  else begin
    for a1:=0 to _dx-1 do begin
      contrib^[a1].n:=0;c:=a1/dx;l:=floor(c-2);r:=ceil(c+2);
      GetMem(contrib^[a1].p,56);
      for a2:=l to r do begin weight:=flt(c-a2);if weight=0 then continue;
        if a2<0 then n:=-a2 else if a2>=imgSrc.Width then n:=2*imgSrc.Width-a2-1 else n:=a2;
        a3:=contrib^[a1].n;contrib^[a1].n:=contrib^[a1].n+1;
        contrib^[a1].p^[a3].pixel:=n;contrib^[a1].p^[a3].weight:=weight;
        end;
      end;
    end;

  for a3:=0 to imgSrc.Height-1 do begin
    sLn:=imgSrc.ScanLine[a3];dPix:=bmp.ScanLine[a3];
    for a1:=0 to _dx-1 do begin rgb.r:=0;rgb.g:=0;rgb.b:=0;
      for a2:=0 to contrib^[a1].n-1 do begin
        color:=sLn^[contrib^[a1].p^[a2].pixel];weight:=contrib^[a1].p^[a2].weight;
        if weight=0 then continue;
        rgb.r:=rgb.r+color.r*weight;rgb.g:=rgb.g+color.g*weight;rgb.b:=rgb.b+color.b*weight;
        end;
        if rgb.r>255 then color.r:=255 else if rgb.r<0 then color.r:=0 else color.r:=round(rgb.r);
        if rgb.g>255 then color.g:=255 else if rgb.g<0 then color.g:=0 else color.g:=round(rgb.g);
        if rgb.b>255 then color.b:=255 else if rgb.b<0 then color.b:=0 else color.b:=round(rgb.b);
        dPix^:=color;inc(dPix);
      end;
    end;
  for a1:=0 to _dx-1 do FreeMem(contrib^[a1].p);
  FreeMem(contrib);

  GetMem(contrib,_dy*sizeof(TCList));
  if dy<1 then begin dw:=3/dy;
    for a1:=0 to _dy-1 do begin
      contrib^[a1].n:=0;c:=a1/dy;l:=floor(c-dw);r:=ceil(c+dw);
      GetMem(contrib^[a1].p,trunc(dw*2+1)*sizeof(TContr));
      for a2:=l to r do begin
        weight:=flt((c-a2)*dy)*dy;
        if weight=0 then continue;
        if a2<0 then n:=-a2 else if a2>=imgSrc.Height then n:=2*imgSrc.Height-a2-1 else n:=a2;
        a3:=contrib^[a1].n;contrib^[a1].n:=contrib^[a1].n+1;
        contrib^[a1].p^[a3].pixel:=n;contrib^[a1].p^[a3].weight:=weight;
        end;
      end
    end
  else begin
    for a1:=0 to _dy-1 do begin
      contrib^[a1].n:=0;c:=a1/dy;l:=floor(c-2);r:=ceil(c+2);
      GetMem(contrib^[a1].p,7*sizeof(TContr));
      for a2:=l to r do begin
        weight:=flt(c-a2);
        if weight=0 then continue;
        if a2<0 then n:=-a2 else if a2>=imgSrc.Height then n:=2*imgSrc.Height-a2-1 else n:=a2;
        a3:=contrib^[a1].n;contrib^[a1].n:=contrib^[a1].n+1;
        contrib^[a1].p^[a3].pixel:=n;contrib^[a1].p^[a3].weight:=weight;
        end;
      end;
    end;

  sLn:=bmp.ScanLine[0];d1:=integer(bmp.ScanLine[1])-integer(sLn);
  dLn:=imgDst.ScanLine[0];d2:=integer(imgDst.ScanLine[0+1])-integer(dLn);
  for a3:=0 to _dx-1 do begin dPix:=pointer(dLn);inc(integer(dPix),0);
    for a1:=0 to _dy-1 do begin rgb.r:=0;rgb.g:=0;rgb.b:=0;
      for a2:=0 to contrib^[a1].n-1 do begin
        color:=PColorRGB(integer(sLn)+contrib^[a1].p^[a2].pixel*d1)^;
        weight:=contrib^[a1].p^[a2].weight;
        if weight=0 then continue;
        rgb.r:=rgb.r+color.r*weight;rgb.g:=rgb.g+color.g*weight;rgb.b:=rgb.b+color.b*weight;
        end;
      if rgb.r>255 then color.r:=255 else if rgb.r<0 then color.r:=0 else color.r:=round(rgb.r);
      if rgb.g>255 then color.g:=255 else if rgb.g<0 then color.g:=0 else color.g:=round(rgb.g);
      if rgb.b>255 then color.b:=255 else if rgb.b<0 then color.b:=0 else color.b:=round(rgb.b);
      dPix^:=color;inc(integer(dPix),d2);
      end;
    Inc(sLn);Inc(dLn);
    end;

  for a1:=0 to _dy-1 do FreeMem(contrib^[a1].p);
  FreeMem(contrib);
  bmp.Free;

end;

end.
