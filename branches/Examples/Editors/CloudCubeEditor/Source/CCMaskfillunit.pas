//If you read this: Go to the Web site and get the original.

{Copyright Francesco Savastano 2000 : This source is free , but you
 cannot distribute it , if you want others have it , let them know
 to come to my web site : http://digilander.iol.it/gensavas/francosava/ }

 {Description :unit to fill parts of a bitmap selected by color :
  it is much more powerful than the simple floodfill windows command
  (you can fill an area with whatever you want and can regulate
  the tolerance of the color similarity research algorithm}
 unit CCMaskfillunit;



interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  System.Math;

procedure fill(var bitmap1:tbitmap;
               a,b:integer;newcolor,oldcolor:tcolor;
               tolerance:real;fill_type:byte;intens,tras:real);
{procedure exec(var bitmap1:tbitmap;
xf,yf:integer;var nf:integer;newcolor,oldcolor:tcolor;
tolerance:real;fill_type:byte;intens,tras:real);}


implementation
var
mask:array of array of boolean;



function valore(n1,n2,hue :real) : real;
begin
result := n1;
if hue >= 360 then
hue := hue-360
else
if hue < 0 then
hue := hue+360;
if hue>=240 then
result := n1;
if hue<240 then
result := n1+(n2-n1)*(240-hue)/60;
if hue<180 then
result := n2;
if hue < 60 then
result := n1+(n2-n1)*hue/60;
end;


procedure rgb2hls (c1,c2,c3 :real;var h,l,s :real);
var
delta,kmin,kmax :real;
minmax:array[1..3] of double;

begin
minmax[1]:=c1;
minmax[2]:=c2;
minmax[3]:=c3;
kmin:= minvalue(minmax);
kmax:=maxvalue(minmax);
{Valuta la luminosità }
l := (kmax+kmin)/2;
{Valuta la saturazione}
if kmax = kmin then
begin
s := 0;
h:= 0;
end
else
begin
if l <= 0.5 then
s := (kmax-kmin)/(kmax+kmin)
else
s := (kmax-kmin)/(2-kmax-kmin) ;
{Valuta il valore di hue}
delta := kmax-kmin ;
if kmax = c1 then
h := (c2-c3)/delta;
if kmax = c2 then
h := 2+(c3-c1)/delta;
if kmax = c3 then
h := 4+(c1-c2)/delta ;
h := h*60;
if h<0 then
h := h+360;
end;
end;

procedure hls2rgb (h,l,s : real ;var c1,c2,c3 : real);
var
m1,m2 : real;
begin
if l<=0.5 then
m2 := l*(1+s)
else
m2 := l+s*(1-l);
m1 := 2*l-m2;
c1 := valore(m1,m2,h+120);
c2 := valore(m1,m2,h);
c3 := valore(m1,m2,h-120);
if (s = 0) and (h = 0) then
begin
c1 := l;
c2 := l;
c3 := l;
end;
end;


  {riempimento}
procedure fill(var bitmap1:tbitmap;a,b:integer;newcolor,oldcolor:tcolor;tolerance:real;fill_type:byte;intens,tras:real);
{procedure fill;}
var
  nf:integer;
  i,ir:integer;
  xf,yf:integer;
  fillx:array[0..80000]of integer;
  filly:array[0..80000]of integer;
procedure exec(var bitmap1:tbitmap;xf,yf:integer;
               var nf:integer;newcolor,oldcolor:tcolor;
               tolerance:real;fill_type:byte;intens,tras:real);
{procedure exec;}
type
  trgbtriplerow=array[0..2000]of trgbtriple;
  prgbtriplerow=^trgbtriplerow;
var
hh,ll,ss,h1,l1,s1,aa,bb,cc:real;
c1,c2,c3,pixr,pixg,pixb,oldr,oldg,oldb,newr,newg,newb:byte;
pix:tcolorref;
jj:byte;
xr,yr:longint;
gray:real;
pp:prgbtriplerow;
begin
oldr:=getrvalue(oldcolor);
oldg:=getgvalue(oldcolor);
oldb:=getbvalue(oldcolor);
newr:=getrvalue(newcolor);
newg:=getgvalue(newcolor);
newb:=getbvalue(newcolor);
xr:=xf;
yr:=yf;
for jj:=1 to 4 do
begin
if jj=1 then
begin
xr:=xf+1;
yr:=yf;
end;
if jj=2 then
begin
xr:=xf-1;
yr:=yf;
end;
if jj=3 then
begin
xr:=xf;
yr:=yf+1;
end;
if jj=4 then
begin
xr:=xf;
yr:=yf-1;
end;
if ((xr< bitmap1.width)and(jj=1))or((xr>=0)and(jj=2))
or ((yr< bitmap1.height)and(jj=3))or((yr>=0)and(jj=4)) then
begin
pp:=bitmap1.scanline[yr];
pix:=rgb(pp[xr].rgbtred,pp[xr].rgbtgreen,pp[xr].rgbtblue);
pixr:=getrvalue(pix);
pixg:=getgvalue(pix);
pixb:=getbvalue(pix);
if (not mask[xr,yr])and(abs(pixr-oldr)<=tolerance*150)and(abs(pixg-oldg)<=tolerance*150)and(abs(pixb-oldb)<=tolerance*150)then
begin
if fill_type=0 then
begin
pp[xr].rgbtred:=round(tras*pixr+(1-tras)*newr);
pp[xr].rgbtgreen:=round(tras*pixg+(1-tras)*newg);
pp[xr].rgbtblue:=round(tras*pixb+(1-tras)*newb);
end;
if fill_type=1 then
begin
rgb2hls(newr/255,newg/255,newb/255,h1,l1,s1);
rgb2hls(pixr/255,pixg/255,pixb/255,hh,ll,ss);
hls2rgb(h1,ll,ss,aa,bb,cc);
pp[xr].rgbtred:=round(aa*255);
pp[xr].rgbtgreen:=round(bb*255);
pp[xr].rgbtblue:=round(cc*255);
end;
if (fill_type>=2)and(fill_type<8) then
begin
aa:=pixr/255;
bb:=pixg/255;
cc:=pixb/255;
if fill_type=2 then
begin
aa:=aa+aa*intens;
bb:=bb+bb*intens;
cc:=cc+cc*intens;
end;
if fill_type=3 then
begin
aa:=aa-(1-aa)*intens;
bb:=bb-(1-bb)*intens;
cc:=cc-(1-cc)*intens;
end;
if fill_type=4 then
begin
gray:=(aa+bb+cc)/3;
aa:=aa+(aa-gray)*intens;
bb:=bb+(bb-gray)*intens;
cc:=cc+(cc-gray)*intens;
end;
if fill_type=5 then
begin
gray:=(aa+bb+cc)/3;
aa:=aa-(aa-gray)*intens;
bb:=bb-(bb-gray)*intens;
cc:=cc-(cc-gray)*intens;
end;
if fill_type=6 then
begin
c1:=round(255*intens+pixr*(1-intens));
c2:=round(255*intens+pixg*(1-intens));
c3:=round(255*intens+pixb*(1-intens));
aa:=c1/255;
bb:=c2/255;
cc:=c3/255;
end;
if fill_type=7 then
begin
c1:=round(1*intens+pixr*(1-intens));
c2:=round(1*intens+pixg*(1-intens));
c3:=round(1*intens+pixb*(1-intens));
aa:=c1/255;
bb:=c2/255;
cc:=c3/255;
end;
if aa>1 then aa:=1;
if bb>1 then bb:=1;
if cc>1 then cc:=1;
if aa<0 then aa:=0;
if bb<0 then bb:=0;
if cc<0 then cc:=0;
pp[xr].rgbtred:=round(aa*255);
pp[xr].rgbtgreen:=round(bb*255);
pp[xr].rgbtblue:=round(cc*255);
end;
mask[xr,yr]:=true;
nf := nf+1;
fillx[nf]:= xr;
filly[nf]:= yr;
end;
end;
end;
end;

begin{Fill}
setlength(mask,bitmap1.width+5,bitmap1.height+5);
nf := 1;
ir := 1;
fillx[nf]:= a;
filly[nf]:= b;
exec(bitmap1,a,b,nf,newcolor,oldcolor,tolerance,fill_type,intens,tras);
while nf>ir do
begin
ir := ir+1;
xf := fillx[ir];
yf := filly[ir];
exec(bitmap1,xf,yf,nf,newcolor,oldcolor,tolerance,fill_type,intens,tras);
if (nf>75000)  then
begin
for i := 1 to nf-ir do
begin
fillx[i] := fillx[ir+i];
filly[i] := filly[ir+i];
end;
nf := nf-ir ;
ir := 0;
end;
end;
mask:=nil;
end;




end.
