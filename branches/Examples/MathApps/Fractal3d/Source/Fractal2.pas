{
Mandel / Julia Explorer
Copyright 2000 Hugh Allen     Hugh.Allen@oz.quest.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
}
unit Fractal;

interface

uses
  Controls,
  Classes,
  Messages
 // ,  FastDIB
  ;

type

  TFractal = class(TCustomControl)
  private
    //dib: TFastDIB;
    FIsJulia: Boolean;
    FCx, FCy: Real;
    FChanged: Boolean;
    FOnPaint: TNotifyEvent;
    FMaxIters:Integer;
    procedure SetCx(const Value: Real);
    procedure SetCy(const Value: Real);
    procedure SetIsJulia(const Value: Boolean);
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
    property IsJulia:      Boolean
    read FIsJulia  write SetIsJulia;
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
  RegisterComponents('Fractal', [TFractal]);
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

{ TFractal }

constructor TFractal.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  dib := TFastDIB.Create;
  ViewLeft   := -2.0;
  ViewTop    := -2.0;    {-1.2}
  ViewWidth  :=  4.0;    {3.2}
  ViewHeight :=  4.0;    {2.4}
  dib.SetSize(2, 2, 8, 0);
  MakePalette(43);
  MaxIterations := 200;
  FChanged := True;
  FOnPaint := nil;
end;

destructor TFractal.Destroy;
begin
  dib.Free;
  inherited;
end;

Procedure TFractal.MakePalette(Seed: Longint);
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

procedure TFractal.Paint;
begin
  if FChanged then
    Render;
  FChanged := False;
  dib.Draw(integer(Canvas.Handle), 0, 0);
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TFractal.Render();
var
  x, y, i, j: integer;
  vx, vy: real;
  vx2: real;
  tx1, ty1: real;
  //tx2, ty2: real;
  lake: boolean;
  dx, dy: real;
  d, ld: real;
  label done;
begin
  dib.SetSize(Width, Height, 8, 0);

  if IsJulia then
  begin
    // see if it has a lake
    vx := 0;
    vy := 0;
    for i := 1 to 500 do
    begin
      if (vx * vx + vy * vy) > 4.0 then
        break;
      vx2 := vx * vx - vy * vy + FCx;
      vy :=  vx * vy * 2.0 + FCy;
      vx := vx2;
    end;
    lake := i > 500;
    tx1 := vx; ty1 := vy;   // trap point
  end
  else
    lake := false;

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      j := 255;
      if IsJulia then
      begin
        vx := PixelToViewX(x);
        vy := PixelToViewY(y);
        ld := 0.0;
        for i := 0 to FMaxIters do
        begin
          if (vx * vx + vy * vy) > 4.0 then
          begin
            j := i;
            break;
          end;
          vx2 := vx * vx - vy * vy + FCx;
          vy :=  vx * vy * 2.0 + FCy;
          vx := vx2;
          if lake and (i > 9) then
          begin
            dx := vx - tx1;
            dy := vy - ty1;
            d := (dx * dx + dy * dy);
            if d < 0.001 then
            begin
              if d < ld then
              begin
                j := 255;//4 - 4*i;
                break;
              end;
              ld := d * 0.999;
            end
            else
              ld := ld * 0.99;
          end;
        end;
      end
      else // Mandelbrot
      begin
        vx := 0;
        vy := 0;
        FCx := PixelToViewX(x);
        FCy := PixelToViewY(y);

        // is point in main cardioid?
        // calculate stable fixed point
        tx1 := 1 - 4 * FCx;
        ty1 := 0 - 4 * FCy;
        CplxSqrt(tx1, ty1, tx1, ty1);
        //tx2 := 0.5 + 0.5 * tx1;  this one not used because
        //ty2 := 0.0 + 0.5 * ty1;  it's not a stable fixed-point
        //tx1 := 0.5 - 0.5 * tx1;
        //ty1 := 0.0 - 0.5 * ty1;
        tx1 := 1.0 - tx1;  // doubling it
        //ty1 :=     - ty1; // not needed; it'll be squared
        d := (tx1 * tx1 + ty1 * ty1);
        // square of magnitude of derivative at fixed-point
        if d < 1.0 then
        begin
          j := 255;
          goto done;
        end;

        // is point in circle to left of main cardioid?
        dx := FCx + 1.0;
        d := dx * dx + FCy * FCy;
        if d < 0.0625 then
        begin
          j := 255;
          goto done;
        end;

        for i := 0 to FMaxIters do
        begin
          if (vx * vx + vy * vy) > 4.0 then
          begin
            j := i;
            break;
          end;
          vx2 := vx * vx - vy * vy + FCx;
          vy :=  vx * vy * 2.0 + FCy;
          vx := vx2;
        end;
      end;
      done:
      dib.Pixels8[y, x] := j;
    end;
end;

procedure TFractal.SetCx(const Value: real);
begin
  FCx := Value;
  SetChanged();
end;

procedure TFractal.SetCy(const Value: real);
begin
  FCy := Value;
  SetChanged();
end;

procedure TFractal.SetIsJulia(const Value: Boolean);
begin
  FIsJulia := Value;
  FCx := 0.28;
  FCy := 0.00;
  SetChanged();
end;

function TFractal.GetDIBPalette: PFColorTable;
begin
  Result := dib.Colors;
end;

procedure TFractal.SetChanged();
begin
  FChanged := True;
  Invalidate;
end;

procedure TFractal.WMEraseBackGround(var message: TMessage);
begin
  // don't do it. It makes them flicker!
end;

procedure TFractal.Resize;
begin
  inherited;
  FChanged := True;
end;

function TFractal.PixelToViewX(const x: integer): Real;
begin
  Result := ViewLeft + x * ViewWidth / Width;
end;

function TFractal.PixelToViewY(const y: integer): Real;
begin
  Result := ViewTop + y * ViewHeight / Height;
end;

function TFractal.ViewToPixelX(const x: Real): integer;
begin
  Result := round((x - ViewLeft) * Width / ViewWidth);
end;

function TFractal.ViewToPixelY(const y: Real): integer;
begin
  Result := round((y - ViewTop) * Height / ViewHeight);
end;

procedure TFractal.SetMaxIters(const Value: Integer);
begin
  FMaxIters := Value;
  SetChanged;
end;

end.
