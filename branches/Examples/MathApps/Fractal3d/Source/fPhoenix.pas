{Phoenix copied..modified by Ivan Lee Herring from
Mandel / Julia Explorer
Copyright 2000 Hugh Allen     Hugh.Allen@oz.quest.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
}
unit fPhoenix;

interface

uses
  Controls,
  Classes,
  Messages,
  FastDIB  ;

type

  TPhoenix = class(TCustomControl)
  private
    dib: TFastDIB;
    FIsEgg: Boolean;
    FCx, FCy: Real;
    FChanged: Boolean;
    FOnPaint: TNotifyEvent;
    FMaxIters:Integer;
    procedure SetCx(const Value: Real);
    procedure SetCy(const Value: Real);
    procedure SetIsEgg(const Value: Boolean);
    procedure SetMaxIters(const Value: Integer);
    function  GetDIBPalette: PFColorTable;
    procedure Render();
    procedure WMEraseBackGround(var message: TMessage);
     message WM_ERASEBKGND;
  protected
    procedure Paint(); override;
    procedure Resize(); override;
  public
    ViewLeft: real;
    ViewTop: real;
    ViewWidth: real;
    ViewHeight: real;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure SetChanged();
    function PixelToViewX(const x: integer): Real;
    function PixelToViewY(const y: integer): Real;
    function ViewToPixelX(const x: Real): integer;
    function ViewToPixelY(const y: Real): integer;
    property Canvas;
    property Palette: PFColorTable   read GetDIBPalette;
    procedure MakePalette(Seed: Longint);
  published
    property IsEgg:      Boolean
    read FIsEgg  write SetIsEgg;
    property Cx:           Real         read FCx       write SetCx;
    property Cy:           Real         read FCy       write SetCy;
    property OnPaint:      TNotifyEvent read FOnPaint  write FOnPaint;
    property MaxIterations:Integer
     read FMaxIters write SetMaxIters default 200;

    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

procedure Register();

// Complex Square Root
procedure CplxSqrt(a, b: Real; out x, y: Real);

implementation

procedure Register();
begin
  RegisterComponents('Phoenix', [TPhoenix]);
end;

procedure CplxSqrt(a, b: Real; out x, y: Real);
var
  r: Real;
begin
  r := sqrt(a*a + b*b);
  x := sqrt(0.5 * (r + a));
  if b < 0.0 then
    y := -sqrt(0.5 * (r - a))
  else
    y := sqrt(0.5 * (r - a));
end;

{ TPhoenix }

constructor TPhoenix.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  dib := TFastDIB.Create;
{Egg
  ViewLeft   := -2.30;
  ViewWidth  :=  2.5;
  ViewTop    := -2.30;
  ViewHeight :=  2.5;}
{  if IsEgg then
  begin}
{  ViewLeft   := -2.30;
  ViewWidth  :=  2.5;
  ViewTop    := -2.30;
  ViewHeight :=  2.5;}
 { end else
  begin}
  ViewLeft   := -2.0;
  ViewWidth  :=  4.0;
  ViewTop    := -2.0;
  ViewHeight :=  4.0;
{  end;}
  dib.SetSize(2, 2, 8{, 0});
  MakePalette(32);
  MaxIterations := 200;
  FChanged := True;
  FOnPaint := nil;
end;

destructor TPhoenix.Destroy;
begin
  dib.Free;
  inherited;
end;

Procedure TPhoenix.MakePalette(Seed: Longint);
var
  i : integer;
  r0, g0, b0: integer;
  r1, g1, b1: integer;
  s: real;
begin
  RandSeed := Seed;
  r0 := 64;
  g0 := 64;
  b0 := 64;
  //r1 := 255;
  //g1 := 200;
  //b1 := 64;
        r1 := random(256);
        g1 := random(256);
        b1 := random(256);
  s := 0;
  for i := 0 to 255 do
  begin
    s := s + 0.02 + 0.03 * (255-i)/255;
    if s >= 1.0 then
    begin
      r0 := r1;
      g0 := g1;
      b0 := b1;
      repeat
        r1 := random(256);
        g1 := random(256);
        b1 := random(256);
      until abs(r1-r0) + abs(g1-g0) + abs(b1-b0) > 200; // fixme
      s := 0;
    end;
    dib.colors[i].r := trunc((1-s) * r0 + s * r1);
    dib.colors[i].g := trunc((1-s) * g0 + s * g1);
    dib.colors[i].b := trunc((1-s) * b0 + s * b1);
  end;
  dib.colors[255].r := 0;//255;
  dib.colors[255].g := 0;//255;
  dib.colors[255].b := 0;//255;
end;

procedure TPhoenix.Paint;
begin
  if FChanged then
    Render;
  FChanged := False;
  dib.Draw(integer(Canvas.Handle), 0, 0);
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TPhoenix.Render();
var
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
    Xi, Yi, Xisquare, Xitemp,{ deltaXi,deltaX,}{deltaP, deltaQ,}
    Xtemp,X, Y,Xsquare, Q, P: Extended;
{  Qarray: array[0..302] of Extended; }
begin
  dib.SetSize(Width, Height, 8{, 0});
      maxcolx := (Width - 1);
      maxrowy := (Height - 1);
{  if IsEgg then
  begin}
{Begin here   FVP := P;     FHQ := Q;}
{        vx := 0;
        vy := 0; }
{        FCx := PixelToViewX(x);
        FCy := PixelToViewY(y);}
      P := FCy;{FVP;}
      Q := FCx;{FHQ;}
{     deltaXi:=(ViewWidth- ViewLeft)/ maxcolx ;
    deltaX := (ViewHeight-ViewTop)/ maxrowy;}
      for colx := 0 to maxcolx do begin
        for rowy := 0 to maxrowy do begin
{          PixelLine := Bitmap.ScanLine[rowy];}
          Y := 0;
          Yi := 0;
{          X := ViewWidth - rowy * deltaX;
          Xi := ViewLeft + colx * deltaXi;}
        X := PixelToViewX(colx);
        Xi := PixelToViewY(rowy);
          Xsquare := 0;
          Xisquare := 0;
          FMcolor := 0;
          while (FMcolor < FMaxIters) and
            ((Xsquare + Xisquare)< 4.0)
              do
          begin
            Xsquare := X * X;
            Xisquare := Xi * Xi;
            Xtemp := Xsquare - Xisquare + P + Q * Y;
            Xitemp := 2 * X * Xi + Q * Yi;
            Y := X;
            Yi := Xi;
            X := Xtemp;
            Xi := Xitemp;
            inc(FMcolor);
          end;
          dib.Pixels8[rowy,colx{colx, rowy}] := FMcolor;
        end;end;

 (* end else // Mandelbrot
  begin
{  Phoenix1.ViewLeft +  Phoenix1.ViewWidth  / Phoenix1.Width;}
{vx := 0; vy := 0;
FCx := PixelToViewX(x);    FCy := PixelToViewY(y);}
    deltaP := (ViewWidth {- ViewLeft})/ maxcolx;
{    (FXMax - FXMin) / maxcolx;}
    deltaQ := (ViewHeight {-ViewTop})/ maxrowy;
    {(FYMax - FYMin) / maxrowy;}
    Qarray[0] :=(deltaQ*Height) +ViewTop;
      {Height}{ abs(ViewHeight)-abs(ViewTop);}{FYMax;}
    for rowy := 1 to maxrowy do
               Qarray[rowy] := Qarray[rowy - 1] - deltaQ;
    P :=(ViewLeft{+ (deltaP)});
{    (FXMin + (start_col * deltaP));}
    for colx := 0 to maxcolx do begin
      for rowy := 0 to maxrowy do begin
{        PixelLine := Bitmap.ScanLine[rowy];}
        Y := 0;
        Yi := 0;
        X := 0;
        Xi := 0;
        FMcolor := 0;
        Xsquare := 0;
        Xisquare := 0;
        while (FMcolor < FMaxIters) and
          (Xsquare + Xisquare < 4.0) do
        begin
          Xsquare := X * X;
          Xisquare := Xi * Xi;
          Xtemp := Xsquare - Xisquare
                  + P + Qarray[rowy] * Y;
          Xitemp := 2 * X * Xi + Qarray[rowy] * Yi;
          Y := X;
          Yi := Xi;
          X := Xtemp;
          Xi := Xitemp;
          inc(FMcolor);
        end;
        dib.Pixels8[rowy,colx] := FMcolor;
      end;
            P := P + deltaP;
      end;
  end;
*)
end;

procedure TPhoenix.SetCx(const Value: real);
begin
  FCx := Value;
  SetChanged();
end;

procedure TPhoenix.SetCy(const Value: real);
begin
  FCy := Value;
  SetChanged();
end;

procedure TPhoenix.SetIsEgg(const Value: Boolean);
begin
  FIsEgg := Value;
  FCx := 0.1953;{-1.209169;}{0.28;   FHQ := -1.209169;}
  FCy := 0.2001;{ 0.356338;}{0.00;    FVP := 0.356338;}
  SetChanged();
end;

function TPhoenix.GetDIBPalette: PFColorTable;
begin
  Result := dib.Colors;
end;

procedure TPhoenix.SetChanged();
begin
  FChanged := True;
  Invalidate;
end;

procedure TPhoenix.WMEraseBackGround(var message: TMessage);
begin
  // don't do it. It makes them flicker!
end;

procedure TPhoenix.Resize;
begin
  inherited;
  FChanged := True;
end;

function TPhoenix.PixelToViewX(const x: integer): Real;
begin
  Result := ViewLeft + x * ViewWidth / Width;
end;
function TPhoenix.ViewToPixelX(const x: Real): integer;
begin
  Result := round((x - ViewLeft) * Width / ViewWidth);
end;

function TPhoenix.PixelToViewY(const y: integer): Real;
begin
  Result := ViewTop + y * ViewHeight / Height;
end;
function TPhoenix.ViewToPixelY(const y: Real): integer;
begin
  Result := round((y - ViewTop) * Height / ViewHeight);
end;

procedure TPhoenix.SetMaxIters(const Value: Integer);
begin
  FMaxIters := Value;
  SetChanged;
end;

end.
