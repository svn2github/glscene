{Newton copied from
Mandel / Julia Explorer
Copyright 2000 Hugh Allen     Hugh.Allen@oz.quest.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
}
unit fNewton;

interface

uses
  Controls,
  Classes,
  Messages,
  FastDIB
  ;

type

  TNewton = class(TCustomControl)
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

implementation

procedure Register();
begin
  RegisterComponents('Newton', [TNewton]);
end;


{ TNewton }

constructor TNewton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  dib := TFastDIB.Create;
  ViewLeft   := -3.0;
  ViewTop    := -3.0;
  ViewWidth  :=  6.0;
  ViewHeight :=  6.0;
  dib.SetSize(2, 2, 8, 0);
  MakePalette(2);
  MaxIterations := 200;
  FChanged := True;
  FOnPaint := nil;
end;

destructor TNewton.Destroy;
begin
  dib.Free;
  inherited;
end;

Procedure TNewton.MakePalette(Seed: Longint);
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

procedure TNewton.Paint;
begin
  if FChanged then
    Render;
  FChanged := False;
  dib.Draw(integer(Canvas.Handle), 0, 0);
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TNewton.Render();
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xi, Yi, Xisquare, Xitemp, deltaXi,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare, deltaX, deltaY,
    max_size, deltaP, deltaQ, Q, P: Extended;
{  S: string;}
{  Bitmap: TBitmap;}
{  PixelLine: PByteArray;}
var
  x, y, i: integer;
  vx, vy: real;
  vx2: real;
  tx1, ty1: real;
  //tx2, ty2: real;
  lake: boolean;
  dx, dy: real;
  d, ld: real;
{  label done;}
begin
  dib.SetSize(Width, Height, 8, 0);
      maxcolx := (Width - 1);
      maxrowy := (Height - 1);
  if IsEgg then {LobsterSets}
  begin
    deltaX:=(ViewWidth- ViewLeft)/ maxcolx ;
    deltaY := (ViewHeight-ViewTop)/ maxrowy;
{    deltaX := (FXMax - FXMin) / maxcolx;
    deltaY := (FYMax - FYMin) / maxrowy;}
      for colx := 0 to maxcolx do begin
        for rowy := 0 to maxrowy do begin
          Y := ViewWidth - rowy * deltaY;
          X := ViewLeft + colx * deltaX;
{        X := FXMin + colx * deltaX;
        Y := FYMax - rowy * deltaY;}
        Xold := 42;
        Yold := 42;
        FMcolor := 0;
        flag := 0;
          while (FMcolor < FMaxIters) and
            (flag = 0)
              do
          begin
          Xsquare := X * X;
          Ysquare := Y * Y;
          denom := 3 * ((Xsquare - Ysquare) * (Xsquare -
            Ysquare) + 4 * Xsquare * Ysquare);
          if denom = 0 then
            denom := 0.00000001;
          X := 0.6666667 * X + (Xsquare - Ysquare) / denom;

          Y := 0.6666667 * Y - 2 * X * Y / denom;
          if (Xold = X) and (Yold = Y) then
            flag := 1;
          Xold := X;
          Yold := Y;
          inc(FMcolor);
          end;
          dib.Pixels8[y, x] := FMcolor;
        end;end;
  end else // Mandelbrot
  begin
    deltaX:=(ViewWidth- ViewLeft)/ maxcolx ;
    deltaY := (ViewHeight-ViewTop)/ maxrowy;
{    deltaX := (FXMax - FXMin) / maxcolx;
    deltaY := (FYMax - FYMin) / maxrowy;}
      for colx := 0 to maxcolx do begin
        for rowy := 0 to maxrowy do begin
          Y := ViewWidth - rowy * deltaY;
          X := ViewLeft + colx * deltaX;
{        X := FXMin + colx * deltaX;
        Y := FYMax - rowy * deltaY;}
        Xold := 42;
        Yold := 42;
        FMcolor := 0;
        flag := 0;
          while (FMcolor < FMaxIters) and
            (flag = 0)
              do
          begin
          Xsquare := X * X;
          Ysquare := Y * Y;
          denom := (3 * Xsquare - 3 * Ysquare - 2);
          denom := denom * denom + 36 * Xsquare * Ysquare;
          if denom = 0 then
            denom := 0.00000001;
          temp1 := X * Xsquare - 3 * X * Ysquare - 2 * X - 5;
          temp2 := 3 * Xsquare - 3 * Ysquare - 2;
          temp3 := 3 * Xsquare * Y - Ysquare * Y - 2 * Y;
          X := X - (temp1 * temp2 - 6 * X * Y * temp3) / denom;
          Y := Y - (temp1 * (-6 * X * Y) + temp3 * temp2)
            / denom;
          Xnew := X;
          Ynew := Y;
          if (Xold = Xnew) and (Yold = Ynew) then
            flag := 1;
          Xold := X;
          Yold := Y;
          inc(FMcolor);
          end;

        begin
 {Actual 16 color selection}
          if x > 0 then begin
          dib.Pixels8[y, x] := FMcolor mod 8;
{            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[0, FMcolor mod 8];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 8];
            PixelLine[(Pixelx + 2)] := Colors[2, FMcolor mod 8]; }
          end
          else begin
            if ((x < -0.3) and (y > 0)) then begin
              Pixelx := (colx * PixelScanSize);
           PixelLine[Pixelx] := Colors[0, ((FMcolor mod 8) + 15)];
           PixelLine[(Pixelx + 1)] := Colors[1, ((FMcolor mod 8) +15)];
           PixelLine[(Pixelx + 2)] := Colors[2, ((FMcolor mod 8) +15)];
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[0, ((FMcolor mod 8) + 32)];
              PixelLine[(Pixelx + 1)] := Colors[1, ((FMcolor mod 8) +
                32)];
              PixelLine[(Pixelx + 2)] := Colors[2, ((FMcolor mod 8) +
                32)];
            end
          end; end

        end;end;
  end;
end;

procedure TNewton.SetCx(const Value: real);
begin
  FCx := Value;
  SetChanged();
end;

procedure TNewton.SetCy(const Value: real);
begin
  FCy := Value;
  SetChanged();
end;

procedure TNewton.SetIsEgg(const Value: Boolean);
begin
  FIsEgg := Value;
  FCx := -1.209169;{0.28;   FHQ := -1.209169;}
  FCy := 0.356338;{0.00;    FVP := 0.356338;}
  SetChanged();
end;

function TNewton.GetDIBPalette: PFColorTable;
begin
  Result := dib.Colors;
end;

procedure TNewton.SetChanged();
begin
  FChanged := True;
  Invalidate;
end;

procedure TNewton.WMEraseBackGround(var message: TMessage);
begin
  // don't do it. It makes them flicker!
end;

procedure TNewton.Resize;
begin
  inherited;
  FChanged := True;
end;

function TNewton.PixelToViewX(const x: integer): Real;
begin
  Result := ViewLeft + x * ViewWidth / Width;
end;

function TNewton.PixelToViewY(const y: integer): Real;
begin
  Result := ViewTop + y * ViewHeight / Height;
end;

function TNewton.ViewToPixelX(const x: Real): integer;
begin
  Result := round((x - ViewLeft) * Width / ViewWidth);
end;

function TNewton.ViewToPixelY(const y: Real): integer;
begin
  Result := round((y - ViewTop) * Height / ViewHeight);
end;

procedure TNewton.SetMaxIters(const Value: Integer);
begin
  FMaxIters := Value;
  SetChanged;
end;

end.
