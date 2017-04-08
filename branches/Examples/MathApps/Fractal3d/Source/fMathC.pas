unit fMathC;

interface

uses
  Windows, Messages, SysUtils, Dialogs, Forms, Graphics, Classes;

procedure qkoch3(level: integer);

procedure Peanoor(level: integer);
procedure Peanomod(level: integer);

procedure Cesaro(level: integer);
procedure CesaroMD(level: integer);
procedure Cesaro2(level: integer);
procedure polya(level: integer);
procedure PGosper(Level: Integer);
procedure snow7(level: integer);
procedure snowhall(level: integer);
procedure snow13(level: integer);

procedure Snoflake(level: integer);
procedure Gosper(level: integer);
procedure hkoch8(level: integer);


procedure qkoch8(level: integer);
procedure qkoch18(level: integer);
procedure qkoch32(level: integer);
procedure qkoch50(level: integer);

procedure hilbert(level: integer);
procedure hilbert2(level: integer);
procedure hil3d(level: integer);


procedure sierp(level: integer); { of 8 }
procedure sierbox(level: integer); { of 3 }
procedure siergask(level: integer); { fast enough at all 5 levels }

procedure Dragon_Curve(level: integer);
procedure TwinDrag(level: integer);

implementation
uses fMain, fGMath;

var
  turtle_r, turtle_x, turtle_y, turtle_theta: Extended;

function point(x1: real; y_one: real; x2: real; y2: Extended):
  Extended;
var theta: Extended;
begin
  if (x2 - x1) = 0 then
    if y2 > y_one then
      theta := 90
    else
      theta := 270
  else
    theta := arctan((y2 - y_one) / (x2 - x1)) * 57.295779;
  if x1 > x2 then
    theta := theta + 180;
  point := theta;
end;

procedure step;
begin
  turtle_x := turtle_x + turtle_r * cos(turtle_theta * 0.017453292);
  turtle_y := turtle_y + turtle_r * sin(turtle_theta * 0.017453292);
end;

procedure turn(angle: Extended);
begin
  turtle_theta := turtle_theta + angle;
end;

function IStringer(I: Longint): string;
{ Convert any integer type to a string }
var
  S: string[11];
begin
  Str(I, S);
  Result := S;
end;


procedure qkoch3(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor, }
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2,
    initiator_y1, initiator_y2
    : array[0..9] of Extended;

  procedure generate
      (X1: Extended; Y1: Extended;
      X2: Extended; Y2: Extended;
      level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..24] of Extended;
  begin
    dec(level);
    turtle_r := sqrt(((X2 - X1) * (X2 - X1)
      + (Y2 - Y1) * (Y2 - Y1))
      / 5.0);
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[3] := X2;
    Ypoints[3] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(26.56);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(-90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;
begin
{gfcolor:=4;  }
  generator_size := 3;
  init_size := 4;
  initiator_x1[0] := -130;
  initiator_x1[1] := 130;
  initiator_x1[2] := 130;
  initiator_x1[3] := -130;
  initiator_x2[0] := 130;
  initiator_x2[1] := 130;
  initiator_x2[2] := -130;
  initiator_x2[3] := -130;
  initiator_y1[0] := 130;
  initiator_y1[1] := 130;
  initiator_y1[2] := -130;
  initiator_y1[3] := -130;
  initiator_y2[0] := 130;
  initiator_y2[1] := -130;
  initiator_y2[2] := -130;
  initiator_y2[3] := 130;
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
{MainForm.Show;}
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      Textout(10, 10, 'Von Koch 3' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
{MainForm.Show;}
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Von Koch 3' + ' Level: ' +
          IStringer(Level));
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);
      end;
    end; end;
end; (* of qkoch3 *)



procedure peanoor(level: integer);
var TempColor: TColor;
  Gfcolor, maxcolx, maxrowy,
    lc, {lcc,counter,} i, generator_size, init_size: integer;
      {	Xpoints, Ypoints: array[0..24] of Extended;  }
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
      {	a, b: Extended; }
    Xpoints, Ypoints: array[0..24] of Extended;
  begin

    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1)
      + (Y2 - Y1) * (Y2 - Y1))) / 3.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[9] := X2;
    Ypoints[9] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-90);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-90);
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    turn(90);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(90);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    turn(90);
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin {*0.729}
        TempColor := RGB(RGBArray[0, GfColor],
          RGBArray[1, GfColor], RGBArray[2, GfColor]);
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.LineTo(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
  GfColor := 4;
  generator_size := 9;
  init_size := 1;
  initiator_x1[0] := 0;
  initiator_x2[0] := 0;
  initiator_y1[0] := -200; {-100}
  initiator_y2[0] := 200; {100}

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level {GfColor}],
        RGBArray[1, Level {GfColor}], RGBArray[2, Level {GfColor}]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Peano' + ' Level: ' + IStringer(Level));
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
    end; end else
  begin
    MainForm.Show;
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      for lc := 1 to 4 do
      begin
        level := lc;
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, GfColor],
          RGBArray[1, GfColor], RGBArray[2, GfColor]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 10, 'Peano' + ' Level: ' + IStringer(Level));

        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        inc(GFColor);
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);
      end; end;
  end;
end;


procedure peanomod(level: integer);
var TempColor: TColor;
  Gfcolor, maxcolx, maxrowy,
    lc, {lcc,counter,} i, generator_size, init_size: integer;
  Xptemp, Yptemp: Extended;
{	Xpoints, Ypoints: array[0..24] of Extended;   }
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
{		a, b: Extended;}
    Xpoints, Ypoints: array[0..24] of Extended;
  begin
    dec(level);
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    if level <> 0 then
    begin
      turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) *
        (Y2 - Y1))) / 3.0;
      Xpoints[9] := X2;
      Ypoints[9] := Y2;
      step;
      Xpoints[1] := turtle_x;
      Ypoints[1] := turtle_y;
      turn(90);
      step;
      Xpoints[2] := turtle_x;
      Ypoints[2] := turtle_y;
      turn(-90);
      step;
      Xpoints[3] := turtle_x;
      Ypoints[3] := turtle_y;
      turn(-90);
      step;
      Xpoints[4] := turtle_x;
      Ypoints[4] := turtle_y;
      turn(-90);
      step;
      Xpoints[5] := turtle_x;
      Ypoints[5] := turtle_y;
      turn(90);
      step;
      Xpoints[6] := turtle_x;
      Ypoints[6] := turtle_y;
      turn(90);
      step;
      Xpoints[7] := turtle_x;
      Ypoints[7] := turtle_y;
      turn(90);
      step;
      Xpoints[8] := turtle_x;
      Ypoints[8] := turtle_y;
      for j := 0 to 8 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) *
        (Y2 - Y1))) / 18.0;
      Xpoints[0] := Xptemp;
      Ypoints[0] := Yptemp;
      Xpoints[19] := X2;
      Ypoints[19] := Y2;
      step;
      Xpoints[1] := turtle_x;
      Ypoints[1] := turtle_y;
      step;
      step;
      step;
      Xpoints[2] := turtle_x;
      Ypoints[2] := turtle_y;
      step;
      turn(90);
      step;
      Xpoints[3] := turtle_x;
      Ypoints[3] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[4] := turtle_x;
      Ypoints[4] := turtle_y;
      step;
      turn(-90);
      step;
      Xpoints[5] := turtle_x;
      Ypoints[5] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[6] := turtle_x;
      Ypoints[6] := turtle_y;
      step;
      turn(-90);
      step;
      Xpoints[7] := turtle_x;
      Ypoints[7] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[8] := turtle_x;
      Ypoints[8] := turtle_y;
      step;
      turn(-90);
      step;
      Xpoints[9] := turtle_x;
      Ypoints[9] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[10] := turtle_x;
      Ypoints[10] := turtle_y;
      step;
      turn(90);
      step;
      Xpoints[11] := turtle_x;
      Ypoints[11] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[12] := turtle_x;
      Ypoints[12] := turtle_y;
      step;
      turn(90);
      step;
      Xpoints[13] := turtle_x;
      Ypoints[13] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[14] := turtle_x;
      Ypoints[14] := turtle_y;
      step;
      turn(90);
      step;
      Xpoints[15] := turtle_x;
      Ypoints[15] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[16] := turtle_x;
      Ypoints[16] := turtle_y;
      step;
      turn(-90);
      step;
      Xpoints[17] := turtle_x;
      Ypoints[17] := turtle_y;
      step;
      step;
      step;
      step;
      Xpoints[18] := turtle_x;
      Ypoints[18] := turtle_y;
      Xptemp := Xpoints[18];
      Yptemp := Ypoints[18];
      for k := 0 to generator_size - 2 do
      begin
        TempColor := RGB(RGBArray[0, GfColor],
          RGBArray[1, GfColor], RGBArray[2, GfColor]);
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.LineTo(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
  GfColor := 1;
  generator_size := 19;
  init_size := 1;
  initiator_x1[0] := 0;
  initiator_x2[0] := 0;
  initiator_y1[0] := -200;
  initiator_y2[0] := 200;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      if Level = 1 then Level := 2;
      TextOut(10, 10, 'Peano Modified' + ' Level: ' +
        IStringer(Level));
      Xptemp := initiator_x1[0];
      Yptemp := initiator_y1[0];
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
    end; end else
  begin
    for lc := 2 to 5 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 10, 'Peano Modified' + ' Level: ' +
          IStringer(Level));
        MainForm.Show;
        Xptemp := initiator_x1[0];
        Yptemp := initiator_y1[0];
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        inc(GFColor);
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);
      end; end;
  end;
end;




procedure cesaro(level: integer);
var TempColor: TColor;
  Gfcolor, maxcolx, maxrowy,
    lc, {lcc,counter,} sign1, i, generator_size, init_size: integer;
{	Xpoints, Ypoints: array[0..24] of extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of extended;
  sign: array[0..15] of integer;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j {,k}: integer;
        {	a, b: extended; }
    Xpoints, Ypoints: array[0..24] of extended;

  begin

    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      2.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[2] := X2;
    Ypoints[2] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(90 * sign[level]);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[0] + 320), Round(240
        - Ypoints[0]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[2] + 320), Round(240
        - Ypoints[2]));
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[1] + 320), Round(240
        - Ypoints[1]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[3] + 320), Round(240
        - Ypoints[3]));
    end;
  end;

begin
  GfColor := 4;
  generator_size := 3;
  init_size := 1;
  initiator_x1[0] := -200; {-150}
  initiator_x2[0] := 200; {150}
  initiator_y1[0] := -50; {0}
  initiator_y2[0] := -50; {0}

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if LEvel > 0 then
  begin
    level := 2 * level; { this was a 16 level dodad }
    MainForm.Show;
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, GfColor],
        RGBArray[1, GfColor], RGBArray[2, GfColor]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Cesaro' + ' Level: ' + IStringer(Level));
      sign1 := -1;
      for i := level downto 0 do
      begin
        sign[i] := sign1;
        sign1 := -sign1;
      end;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc * 2; { this was a 16 level dodad }
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, GfColor],
          RGBArray[1, GfColor], RGBArray[2, GfColor]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 10, 'Cesaro' + ' Level: ' + IStringer(Level));
        MainForm.Show;
        sign1 := -1;
        for i := level downto 0 do
        begin
          sign[i] := sign1;
          sign1 := -sign1;
        end;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        inc(GFColor);
      end; end;
  end;
end; { of Cesaro }



procedure CesaroMD(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, GfColor,
  lc, {lcc,counter, } sign1, i, generator_size, init_size: integer;
{   Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  sign: array[0..15] of integer;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j {,k}: integer;
    a, b: Extended;
    Xpoints, Ypoints: array[0..24] of Extended;
  begin
    dec(level);
    a := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) / 2.0;
    b := a * 0.9128442;
    turtle_r := b;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[2] := X2;
    Ypoints[2] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(85 * sign[level]);
    turtle_r := a;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(-170 * sign[level]);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[0] + 320), Round(240
        - Ypoints[0]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[3] + 320), Round(240
        - Ypoints[3]));
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[2] + 320), Round(240
        - Ypoints[2]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[4] + 320), Round(240
        - Ypoints[4]));
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[3] + 320), Round(240
        - Ypoints[3]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[1] + 320), Round(240
        - Ypoints[1]));
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[4] + 320), Round(240
        - Ypoints[4]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[1] + 320), Round(240
        - Ypoints[1]));
    end;
  end;

begin
  GfColor := 4;
  generator_size := 3;
  init_size := 1;
  initiator_x1[0] := -200; {-150}
  initiator_x2[0] := 200; {150}
  initiator_y1[0] := -50; {0}
  initiator_y2[0] := -50; {0}

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if LEvel > 0 then
  begin
    level := 2 * level; { this was 16 levels }

    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, GfColor],
        RGBArray[1, GfColor], RGBArray[2, GfColor]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Cesaro MD' + ' Level: ' + IStringer(Level));
      MainForm.Show;
      sign1 := -1;
      for i := level downto 0 do
      begin
        sign[i] := sign1;
        sign1 := -sign1;
      end;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc * 2; { this was 16 levels }
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, GfColor],
          RGBArray[1, GfColor], RGBArray[2, GfColor]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 10, 'Cesaro MD' + ' Level: ' + IStringer(Level));
        MainForm.Show;
        sign1 := -1;
        for i := level downto 0 do
        begin
          sign[i] := sign1;
          sign1 := -sign1;
        end;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        inc(GFColor);
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end; { of cesaromod }



procedure Cesaro2(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  lc, {lcc,counter, } sign, i, generator_size, init_size: integer;
       {	Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j: integer;
{		b: Extended;  }
    Xpoints, Ypoints: array[0..24] of Extended;

  begin

    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      2.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[2] := X2;
    Ypoints[2] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(90 * sign);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    sign := -1;
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin { was TOO low so 240 is now 140}
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[0] + 320), Round(140
        - Ypoints[0]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[2] + 320), Round(140
        - Ypoints[2]));
      MainForm.Image2.Canvas.Moveto(Round(Xpoints[1] + 320), Round(140
        - Ypoints[1]));
      MainForm.Image2.Canvas.Lineto(Round(Xpoints[3] + 320), Round(140
        - Ypoints[3]));
    end;
  end;

begin
{GfColor :=4; }
{pcxVGASetup;}
  generator_size := 3;
  init_size := 1;
  initiator_x1[0] := -150;
  initiator_x2[0] := 150;
  initiator_y1[0] := 0; {0}
  initiator_y2[0] := 0; {0}

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    level := 2 * level; { this was 16 levels }
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, 'Cesaro 2' + ' Level: ' + IStringer(Level));
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc * 2; { this was 16 levels }
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Cesaro 2' + ' Level: ' + IStringer(Level));
{inc(GFColor);  }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end;


procedure polya(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  ill, iin, lc, {lcc,counter, } sign1, generator_size, init_size:
    integer;
{	Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  sign: array[0..16] of integer;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
        {	a, b: Extended;   }
    Xpoints, Ypoints: array[0..24] of Extended;

  begin {/1.41421}
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 1.41421;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[2] := X2;
    Ypoints[2] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(sign[level] * 45);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    dec(level);
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
        sign[level] := -sign[level];
      end;
    end
    else
    begin
      for k := 0 to generator_size - 2 do begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
{GfColor:=4; }
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      sign1 := 1;
      generator_size := 3;
      init_size := 2;
      initiator_x1[0] := -150;
      initiator_x2[0] := 150;
      initiator_y1[0] := -75; {-75}
      initiator_y2[0] := -75; {-75}
      for ill := level downto 0 do
      begin
        sign[ill] := sign1;
        sign1 := -sign1;
      end;
      for iin := 0 to init_size - 2 do
      begin
        generate(initiator_x1[iin], initiator_y1[iin],
          initiator_x2[iin],
          initiator_y2[iin], level);
      end;
      TextOut(10, 10, 'Polya' + ' Level: ' + IStringer(Level));
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        sign1 := 1;
        generator_size := 3;
        init_size := 2;
        initiator_x1[0] := -150;
        initiator_x2[0] := 150;
        initiator_y1[0] := -75; {-75}
        initiator_y2[0] := -75; {-75}
        for ill := level downto 0 do
        begin
          sign[ill] := sign1;
          sign1 := -sign1;
        end;
        for iin := 0 to init_size - 2 do
        begin
          generate(initiator_x1[iin], initiator_y1[iin],
            initiator_x2[iin],
            initiator_y2[iin], level);
        end;
        TextOut(10, 10, 'Polya' + ' Level: ' + IStringer(Level));
   {inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Polya }


procedure pgosper(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, lc,
  i, generator_size, init_size: integer;
  gen_type: integer;
  sign: Extended;
  Xpoints, Ypoints: array[0..24] of Extended;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; gen_type: integer);
  var
    j, k, set_type: integer;
{		a,b,} temp: Extended;
{		Xpoints, Ypoints: array[0..24] of Extended;}

  begin
    case gen_type of
      1: sign := -sign;
      2: begin
          sign := -sign;
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
      3: begin
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
    end;
    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
                {/ 1.81421}/ 2.6457513;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[7] := X2;
    Ypoints[7] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turn(-19 * sign);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(120 * sign);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-120 * sign);
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    if level = 0 then
    begin
      for k := 0 to generator_size - 2 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(175 - Ypoints[k] * 0.729));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(175 - Ypoints[k + 1] * 0.729));
      end;
    end
    else
    begin
      for j := 0 to generator_size - 2 do
      begin
        case j of
          0, 3, 4, 5: set_type := 0;
          2, 1, 6: set_type := 3;
        else set_type := 0;
        end;
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, set_type);
      end;
    end;
  end;

begin
  if level < 0 then level := 0
  else if Level > 8 then Level := 8;
  if Level > 0 then
  begin
    if level < 3 then level := 3
    else if Level > 5 then Level := 5;
    sign := 1;
    gen_type := 3;
    generator_size := 8;
    init_size := 1;
    initiator_x1[0] := -150;
    initiator_x2[0] := 150;
    initiator_y1[0] := 75;
    initiator_y2[0] := 75;
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level, gen_type);
      end;
      TextOut(10, 10, 'Polya Gosper' + ' Level: ' +
        IStringer(Level));
    end; end else
  begin
    for lc := 2 to 4 do
    begin
      level := lc;
      sign := 1;
      gen_type := 3;
      generator_size := 8;
      init_size := 1;
      initiator_x1[0] := -150;
      initiator_x2[0] := 150;
      initiator_y1[0] := 75;
      initiator_y2[0] := 75;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level, gen_type);
        end;
        TextOut(10, 10, 'Polya Gosper' + ' Level: ' +
          IStringer(Level));
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end;

procedure snow7(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  lc, {lcc,counter,} i, generator_size, init_size {, gen_type}, sign:
    integer;
      {	Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; gen_type: integer; sign: integer);
  var
    j, k, set_type: integer;
    temp: Extended;
    Xpoints, Ypoints: array[0..24] of Extended;
  begin
    case gen_type of
      1: begin
          sign := -sign;
        end;
      2: begin
          sign := -sign;
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
      3: begin
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
    end;
    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 3.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[7] := X2;
    Ypoints[7] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(60 * sign);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    if level = 0 then
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end
    else
    begin
      for j := 0 to generator_size - 1 do
      begin
        case j of
          0, 5: set_type := 1;
          1, 2, 3, 6: set_type := 2;
          4: set_type := 3;
        else set_type := 3;
        end;
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, set_type, sign);
      end;
    end;
  end;

begin
{GfColor:=4;}
{pcxVGASetup;}
  sign := 1;
  generator_size := 7;
  init_size := 1;
  initiator_x1[0] := -150;
  initiator_x2[0] := 150;
  initiator_y1[0] := -50; {    }
  initiator_y2[0] := -50;

{ original ... backup
 initiator_x1[0] := -125;
 initiator_x2[0] := 125;
 initiator_y1[0] := 0;
 initiator_y2[0] := 0;
}
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level, 7 {gen type?}, sign);
      end;
      TextOut(10, 10, 'Snow 7 ' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level, 7, sign);
        end;
        TextOut(10, 10, 'Snow 7 ' + ' Level: ' + IStringer(Level));
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end;



procedure snowhall(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  lc, {lcc,counter,} i, generator_size, init_size, set_type: integer;
  sign: Extended;
{	Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; gen_type: integer; sign: Extended);
  var
    j, k, set_type: integer;
        {	a,b,} temp: Extended;
    Xpoints, Ypoints: array[0..24] of Extended;

  begin
    case gen_type of
      1: sign := -sign;
      2: begin
          sign := -sign;
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
      3: begin
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
    end;
    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 3.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[11] := X2;
    Ypoints[11] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turn(60 * sign);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-120 * sign);
    step;
    turn(60 * sign);
    step;
    Xpoints[9] := turtle_x;
    Ypoints[9] := turtle_y;
    turn(120 * sign);
    step;
    Xpoints[10] := turtle_x;
    Ypoints[10] := turtle_y;
    turtle_r := (sqrt((Xpoints[9] - Xpoints[4]) * (Xpoints[9] -
      Xpoints[4]) + (Ypoints[9] - Ypoints[4]) * (Ypoints[9] -
      Ypoints[4]))) / 3.0;
    turtle_theta := point(Xpoints[4], Ypoints[4], Xpoints[9],
      Ypoints[9]);
    turn(-60 * sign);
    turtle_x := Xpoints[4];
    turtle_y := Ypoints[4];
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    if level = 0 then
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end
    else
    begin
      for j := 0 to generator_size - 1 do
      begin
        if level = 1 then
        begin
          case j of
            2, 8, 10: set_type := 0;
            0, 5: set_type := 1;
            1, 3, 4: set_type := 2;
            6, 7, 9: set_type := 3;
          else set_type := 3;
          end;
        end else
          if level > 1 then
          begin
            case j of
              2, 8, 10: set_type := 0;
              0: set_type := 1;
              1, 3, 4: set_type := 2;
              5, 6, 7, 9: set_type := 3;
            else set_type := 3;
            end;
          end else set_type := 3;
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, set_type, sign);
      end;
    end;
  end;

begin
{GfColor:=4; }
  sign := 1;
  set_type := 0;
  generator_size := 11;
  init_size := 1;
  initiator_x1[0] := -150;
  initiator_x2[0] := 150;
  initiator_y1[0] := -75;
  initiator_y2[0] := -75;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level, set_type, sign);
      end;
      TextOut(10, 10, 'Snow Hall' + ' Level: ' + IStringer(Level));
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level, set_type, sign);
        end;
        Textout(10, 10, 'Snow Hall' + ' Level: ' + IStringer(Level));
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end; { of  Snow Hall }




procedure snow13(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  lc, {lcc,counter,} i, generator_size, init_size, {gen_type,} sign:
    integer;
{	Xpoints, Ypoints: array[0..24] of Extended;   }
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; gen_type: integer; sign: integer);
  var
    j, k, set_type: integer;
    temp: Extended;
    Xpoints, Ypoints: array[0..24] of Extended;

  begin
    case gen_type of
      1: begin
          sign := -sign;
        end;
      2: begin
          sign := -sign;
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
      3: begin
          temp := X1;
          X1 := X2;
          X2 := temp;
          temp := Y1;
          Y1 := Y2;
          Y2 := temp;
        end;
    end;
    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 3.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[13] := X2;
    Ypoints[13] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(60 * sign);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[12] := turtle_x;
    Ypoints[12] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[11] := turtle_x;
    Ypoints[11] := turtle_y;
    turtle_r := (sqrt((Xpoints[11] - Xpoints[4]) * (Xpoints[11]
      - Xpoints[4]) + (Ypoints[11] - Ypoints[4]) * (Ypoints[11] -
      Ypoints[4]))) / 3.0;
    turtle_theta := point(Xpoints[4], Ypoints[4], Xpoints[11],
      Ypoints[11]);
    turtle_x := Xpoints[4];
    turtle_y := Ypoints[4];
    turn(-60 * sign);
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[10] := turtle_x;
    Ypoints[10] := turtle_y;
    turn(60 * sign);
    step;
    Xpoints[9] := turtle_x;
    Ypoints[9] := turtle_y;
    if level = 0 then
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end
    else
    begin
      for j := 0 to generator_size - 1 do
      begin
        case j of
          1, 2, 3, 4, 8, 9, 12: set_type := 0;
          0, 5, 6, 7, 10, 11: set_type := 1;
        else set_type := 1;
        end;
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, set_type, sign);
      end;
    end;
  end;
begin
{GfColor:=4;  }
  sign := 1;
  generator_size := 13;
  init_size := 1;
  initiator_x1[0] := -150;
  initiator_x2[0] := 150;
  initiator_y1[0] := -75; {-50}
  initiator_y2[0] := -75; {50}
{ original
 initiator_x1[0] := -125;
 initiator_x2[0] := 125;
 initiator_y1[0] := 0;
 initiator_y2[0] := 0;
}
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level, 13, sign);
      end;
      TextOut(10, 10, 'Snow 13' + ' Level: ' + IStringer(Level));
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level, 13, sign);
        end;
        Textout(10, 10, 'Snow 13' + ' Level: ' + IStringer(Level));
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end; { of Snow13  }


procedure Snoflake(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, GfColor,
  lc, {lcc,counter,} i, generator_size, init_size: integer;
 {Xpoints, Ypoints: array[0..24] of Extended;}
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
       {	a, b: Extended; }
    Xpoints, Ypoints: array[0..24] of Extended;

  begin

    dec(level);
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 3.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[4] := X2;
    Ypoints[4] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(60);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-120);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 2 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
  GfColor := 4;
  generator_size := 5;
  init_size := 3;
  initiator_x1[0] := -150;
  initiator_x1[1] := 0;
  initiator_x1[2] := 150;
  initiator_x2[0] := 0;
  initiator_x2[1] := 150;
  initiator_x2[2] := -150;
  initiator_y1[0] := -75;
  initiator_y1[1] := 185;
  initiator_y1[2] := -75;
  initiator_y2[0] := 185;
  initiator_y2[1] := -75;
  initiator_y2[2] := -75;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level = 0 then
  begin
    for lC := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, ' Snowflake' + ' Level: ' +
          IStringer(Level));
{inc(GFColor); }
      end;
    end; end else
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, GfColor],
        RGBArray[1, GfColor], RGBArray[2, GfColor]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, ' Snowflake' + ' Level: ' + IStringer(Level));
      bRotateImage := False; {in the Drawing...}
      bRotatingImage := True;
      repeat Application.ProcessMessages until (bRotateImage = True);

    end; end;
end; { of Snow  }


procedure gosper(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfColor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
       {	a, b: Extended; }
    Xpoints, Ypoints: array[0..24] of Extended;

  begin

    dec(level);
    turtle_r := sqrt(((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1))
      / 7.0);
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[3] := X2;
    Ypoints[3] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(19.1);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(-60);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(175 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(175 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
{GfColor := 4; }
  generator_size := 3;
  init_size := 6;
{  original ... backup}
  initiator_x1[0] := 0;
  initiator_x1[1] := 130;
  initiator_x1[2] := 130;
  initiator_x1[3] := 0;
  initiator_x1[4] := -130;
  initiator_x1[5] := -130;

  initiator_x2[0] := 130;
  initiator_x2[1] := 130;
  initiator_x2[2] := 0;
  initiator_x2[3] := -130;
  initiator_x2[4] := -130;
  initiator_x2[5] := 0;

  initiator_y1[0] := 150;
  initiator_y1[1] := 75;
  initiator_y1[2] := -75;
  initiator_y1[3] := -150;
  initiator_y1[4] := -75;
  initiator_y1[5] := 75;

  initiator_y2[0] := 75;
  initiator_y2[1] := -75;
  initiator_y2[2] := -150;
  initiator_y2[3] := -75;
  initiator_y2[4] := 75;
  initiator_y2[5] := 150;
{   always got lost on which way is UP
 initiator_y1[0] := 125;
 initiator_y1[1] := 50;
 initiator_y1[2] := -100;
 initiator_y1[3] := -175;
 initiator_y1[4] := -100;
 initiator_y1[5] := 50;

 initiator_y2[0] := 50;
 initiator_y2[1] := -100;
 initiator_y2[2] := -175;
 initiator_y2[3] := -100;
 initiator_y2[4] := 50;
 initiator_y2[5] := 175;
}
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      Textout(10, 10, 'Gosper' + ' Level: ' + IStringer(Level));
    end; end else

  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Gosper' + ' Level: ' + IStringer(Level));
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Gosper }



procedure HKoch8(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {GfColor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1,
    initiator_y2: array[0..9] of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..24] of Extended; { dupe ? }

  begin
    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      4.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[8] := X2;
    Ypoints[8] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-90);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    turn(90);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(90);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin { Main block of   }
{GfColor := 4;}
  generator_size := 8;
  init_size := 6;
  initiator_x1[0] := -75;
  initiator_x1[1] := 75;
  initiator_x1[2] := 150;
  initiator_x1[3] := 75;
  initiator_x1[4] := -75;
  initiator_x1[5] := -150;
  initiator_x2[0] := 75;
  initiator_x2[1] := 150;
  initiator_x2[2] := 75;
  initiator_x2[3] := -75;
  initiator_x2[4] := -150;
  initiator_x2[5] := -75;
  initiator_y1[0] := 115;
  initiator_y1[1] := 115;
  initiator_y1[2] := 0;
  initiator_y1[3] := -115;
  initiator_y1[4] := -115;
  initiator_y1[5] := 0;
  initiator_y2[0] := 115;
  initiator_y2[1] := 0;
  initiator_y2[2] := -115;
  initiator_y2[3] := -115;
  initiator_y2[4] := 0;
  initiator_y2[5] := 115;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      Textout(10, 10, 'Hexagonal Snowflake' + ' Level: ' +
        IStringer(Level)); { the real Hex }
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        Textout(10, 10, 'Hexagonal Snowflake' + ' Level: ' +
          IStringer(Level)); { the real Hex }
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Procedure }






procedure qkoch8(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..24] of Extended;

  begin

    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      4.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[8] := X2;
    Ypoints[8] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-90);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    turn(90);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(90);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
{gfcolor:=4; }
{pcxVGASetup;}
  generator_size := 8;
  init_size := 4;
  initiator_x1[0] := -130;
  initiator_x1[1] := 130;
  initiator_x1[2] := 130;
  initiator_x1[3] := -130;
  initiator_x2[0] := 130;
  initiator_x2[1] := 130;
  initiator_x2[2] := -130;
  initiator_x2[3] := -130;
  initiator_y1[0] := 130;
  initiator_y1[1] := 130;
  initiator_y1[2] := -130;
  initiator_y1[3] := -130;
  initiator_y2[0] := 130;
  initiator_y2[1] := -130;
  initiator_y2[2] := -130;
  initiator_y2[3] := 130;

  if level < 0 then level := 0
  else if level > 8 then level := 8;

  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, 'Von Koch 8' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Von Koch 8' + ' Level: ' +
          IStringer(Level));
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Von Koch8  }



procedure qkoch18(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..24] of Extended;

  begin

    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      6.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[18] := X2;
    Ypoints[18] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    turn(-90);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(-90);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    turn(90);
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    turn(90);
    step;
    Xpoints[9] := turtle_x;
    Ypoints[9] := turtle_y;
    step;
    Xpoints[10] := turtle_x;
    Ypoints[10] := turtle_y;
    turn(-90);
    step;
    Xpoints[11] := turtle_x;
    Ypoints[11] := turtle_y;
    turn(-90);
    step;
    Xpoints[12] := turtle_x;
    Ypoints[12] := turtle_y;
    turn(90);
    step;
    Xpoints[13] := turtle_x;
    Ypoints[13] := turtle_y;
    turn(90);
    step;
    Xpoints[14] := turtle_x;
    Ypoints[14] := turtle_y;
    step;
    Xpoints[15] := turtle_x;
    Ypoints[15] := turtle_y;
    turn(90);
    step;
    Xpoints[16] := turtle_x;
    Ypoints[16] := turtle_y;
    step;
    Xpoints[17] := turtle_x;
    Ypoints[17] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin {175} {*0.729}
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
{gfcolor:=4;  }
{pcxVGASetup;}
  generator_size := 18;
  init_size := 4;
  initiator_x1[0] := -130;
  initiator_x1[1] := 130;
  initiator_x1[2] := 130;
  initiator_x1[3] := -130;
  initiator_x2[0] := 130;
  initiator_x2[1] := 130;
  initiator_x2[2] := -130;
  initiator_x2[3] := -130;
  initiator_y1[0] := 130;
  initiator_y1[1] := 130;
  initiator_y1[2] := -130;
  initiator_y1[3] := -130;
  initiator_y2[0] := 130;
  initiator_y2[1] := -130;
  initiator_y2[2] := -130;
  initiator_y2[3] := 130;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, 'Von Koch 18' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Von Koch 18' + ' Level: ' +
          IStringer(Level));
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Von Koch 18  }



procedure qkoch32(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;

  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..32] of Extended;
  begin
    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      8.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[32] := X2;
    Ypoints[32] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(90);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(-90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(90);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    turn(-90);
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    turn(-90);
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    turn(90);
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    turn(-90);
    step;
    Xpoints[9] := turtle_x;
    Ypoints[9] := turtle_y;
    turn(-90);
    step;
    Xpoints[10] := turtle_x;
    Ypoints[10] := turtle_y;
    step;
    Xpoints[11] := turtle_x;
    Ypoints[11] := turtle_y;
    turn(-90);
    step;
    Xpoints[12] := turtle_x;
    Ypoints[12] := turtle_y;
    turn(90);
    step;
    Xpoints[13] := turtle_x;
    Ypoints[13] := turtle_y;
    turn(90);
    step;
    Xpoints[14] := turtle_x;
    Ypoints[14] := turtle_y;
    step;
    Xpoints[15] := turtle_x;
    Ypoints[15] := turtle_y;
    turn(-90);
    step;
    Xpoints[16] := turtle_x;
    Ypoints[16] := turtle_y;
    step;
    Xpoints[17] := turtle_x;
    Ypoints[17] := turtle_y;
    turn(90);
    step;
    Xpoints[18] := turtle_x;
    Ypoints[18] := turtle_y;
    step;
    Xpoints[19] := turtle_x;
    Ypoints[19] := turtle_y;
    turn(-90);
    step;
    Xpoints[20] := turtle_x;
    Ypoints[20] := turtle_y;
    turn(-90);
    step;
    Xpoints[21] := turtle_x;
    Ypoints[21] := turtle_y;
    turn(90);
    step;
    Xpoints[22] := turtle_x;
    Ypoints[22] := turtle_y;
    step;
    Xpoints[23] := turtle_x;
    Ypoints[23] := turtle_y;
    turn(90);
    step;
    Xpoints[24] := turtle_x;
    Ypoints[24] := turtle_y;
    turn(90);
    step;
    Xpoints[25] := turtle_x;
    Ypoints[25] := turtle_y;
    turn(-90);
    step;
    Xpoints[26] := turtle_x;
    Ypoints[26] := turtle_y;
    step;
    Xpoints[27] := turtle_x;
    Ypoints[27] := turtle_y;
    turn(90);
    step;
    Xpoints[28] := turtle_x;
    Ypoints[28] := turtle_y;
    turn(90);
    step;
    Xpoints[29] := turtle_x;
    Ypoints[29] := turtle_y;
    turn(-90);
    step;
    Xpoints[30] := turtle_x;
    Ypoints[30] := turtle_y;
    turn(-90);
    step;
    Xpoints[31] := turtle_x;
    Ypoints[31] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin

  generator_size := 32;
  init_size := 4;
  initiator_x1[0] := -100;
  initiator_x1[1] := 100;
  initiator_x1[2] := 100;
  initiator_x1[3] := -100;
  initiator_x2[0] := 100;
  initiator_x2[1] := 100;
  initiator_x2[2] := -100;
  initiator_x2[3] := -100;
  initiator_y1[0] := 100;
  initiator_y1[1] := 100;
  initiator_y1[2] := -100;
  initiator_y1[3] := -100;
  initiator_y2[0] := 100;
  initiator_y2[1] := -100;
  initiator_y2[2] := -100;
  initiator_y2[3] := 100;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
{gfcolor:=4;}
{pcxVGASetup;}

  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, 'Von Koch 32' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 3 do
    begin

      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Von Koch 32' + ' Level: ' +
          IStringer(Level));
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Von Koch 32  }




procedure qkoch50(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor,}
  lc, i, generator_size, init_size: integer;
  initiator_x1, initiator_x2, initiator_y1, initiator_y2: array[0..9]
  of Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer);
  var
    j, k: integer;
    Xpoints, Ypoints: array[0..50] of Extended;

  begin
    dec(level);
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      10.0;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[50] := X2;
    Ypoints[50] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(90);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    turn(-90);
    step;
    Xpoints[3] := turtle_x;
    Ypoints[3] := turtle_y;
    turn(-90);
    step;
    Xpoints[4] := turtle_x;
    Ypoints[4] := turtle_y;
    step;
    Xpoints[5] := turtle_x;
    Ypoints[5] := turtle_y;
    step;
    Xpoints[6] := turtle_x;
    Ypoints[6] := turtle_y;
    turn(90);
    step;
    Xpoints[7] := turtle_x;
    Ypoints[7] := turtle_y;
    step;
    Xpoints[8] := turtle_x;
    Ypoints[8] := turtle_y;
    turn(-90);
    step;
    Xpoints[9] := turtle_x;
    Ypoints[9] := turtle_y;
    step;
    Xpoints[10] := turtle_x;
    Ypoints[10] := turtle_y;
    turn(90);
    step;
    Xpoints[11] := turtle_x;
    Ypoints[11] := turtle_y;
    turn(90);
    step;
    Xpoints[12] := turtle_x;
    Ypoints[12] := turtle_y;
    step;
    Xpoints[13] := turtle_x;
    Ypoints[13] := turtle_y;
    step;
    Xpoints[14] := turtle_x;
    Ypoints[14] := turtle_y;
    turn(90);
    step;
    Xpoints[15] := turtle_x;
    Ypoints[15] := turtle_y;
    step;
    Xpoints[16] := turtle_x;
    Ypoints[16] := turtle_y;
    turn(-90);
    step;
    Xpoints[17] := turtle_x;
    Ypoints[17] := turtle_y;
    step;
    Xpoints[18] := turtle_x;
    Ypoints[18] := turtle_y;
    step;
    Xpoints[19] := turtle_x;
    Ypoints[19] := turtle_y;
    step;
    Xpoints[20] := turtle_x;
    Ypoints[20] := turtle_y;
    turn(-90);
    step;
    Xpoints[21] := turtle_x;
    Ypoints[21] := turtle_y;
    turn(-90);
    step;
    Xpoints[22] := turtle_x;
    Ypoints[22] := turtle_y;
    step;
    Xpoints[23] := turtle_x;
    Ypoints[23] := turtle_y;
    step;
    Xpoints[24] := turtle_x;
    Ypoints[24] := turtle_y;
    turn(90);
    step;
    Xpoints[25] := turtle_x;
    Ypoints[25] := turtle_y;
    step;
    Xpoints[26] := turtle_x;
    Ypoints[26] := turtle_y;
    turn(-90);
    step;
    Xpoints[27] := turtle_x;
    Ypoints[27] := turtle_y;
    step;
    Xpoints[28] := turtle_x;
    Ypoints[28] := turtle_y;
    step;
    Xpoints[29] := turtle_x;
    Ypoints[29] := turtle_y;
    turn(90);
    step;
    Xpoints[30] := turtle_x;
    Ypoints[30] := turtle_y;
    turn(90);
    step;
    Xpoints[31] := turtle_x;
    Ypoints[31] := turtle_y;
    step;
    Xpoints[32] := turtle_x;
    Ypoints[32] := turtle_y;
    step;
    Xpoints[33] := turtle_x;
    Ypoints[33] := turtle_y;
    step;
    Xpoints[34] := turtle_x;
    Ypoints[34] := turtle_y;
    turn(90);
    step;
    Xpoints[35] := turtle_x;
    Ypoints[35] := turtle_y;
    step;
    Xpoints[36] := turtle_x;
    Ypoints[36] := turtle_y;
    turn(-90);
    step;
    Xpoints[37] := turtle_x;
    Ypoints[37] := turtle_y;
    step;
    Xpoints[38] := turtle_x;
    Ypoints[38] := turtle_y;
    step;
    Xpoints[39] := turtle_x;
    Ypoints[39] := turtle_y;
    turn(-90);
    step;
    Xpoints[40] := turtle_x;
    Ypoints[40] := turtle_y;
    turn(-90);
    step;
    Xpoints[41] := turtle_x;
    Ypoints[41] := turtle_y;
    step;
    Xpoints[42] := turtle_x;
    Ypoints[42] := turtle_y;
    turn(90);
    step;
    Xpoints[43] := turtle_x;
    Ypoints[43] := turtle_y;
    step;
    Xpoints[44] := turtle_x;
    Ypoints[44] := turtle_y;
    turn(-90);
    step;
    Xpoints[45] := turtle_x;
    Ypoints[45] := turtle_y;
    step;
    Xpoints[46] := turtle_x;
    Ypoints[46] := turtle_y;
    step;
    Xpoints[47] := turtle_x;
    Ypoints[47] := turtle_y;
    turn(90);
    step;
    Xpoints[48] := turtle_x;
    Ypoints[48] := turtle_y;
    turn(90);
    step;
    Xpoints[49] := turtle_x;
    Ypoints[49] := turtle_y;
    if level > 0 then
    begin
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level);
      end;
    end
    else
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;

begin
{gfcolor:=4;  }
  generator_size := 50;
  init_size := 4;
  initiator_x1[0] := -100;
  initiator_x1[1] := 100;
  initiator_x1[2] := 100;
  initiator_x1[3] := -100;
  initiator_x2[0] := 100;
  initiator_x2[1] := 100;
  initiator_x2[2] := -100;
  initiator_x2[3] := -100;
  initiator_y1[0] := 100;
  initiator_y1[1] := 100;
  initiator_y1[2] := -100;
  initiator_y1[3] := -100;
  initiator_y2[0] := 100;
  initiator_y2[1] := -100;
  initiator_y2[2] := -100;
  initiator_y2[3] := 100;

  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level);
      end;
      TextOut(10, 10, 'Von Koch 50' + ' Level: ' + IStringer(Level));

    end; end else
  begin
    for lc := 1 to 2 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level);
        end;
        TextOut(10, 10, 'Von Koch 50' + ' Level: ' +
          IStringer(Level));
{inc(GFColor);  }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of  Von Koch 50 }



procedure hilbert(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfColor,}
  lc, i: integer;
  x1, x2, y1, y2, r, temp: Extended;

  procedure generate(r1: Extended; r2: Extended);
  begin
    dec(level);
    if level > 0 then
      generate(r2, r1);
    x2 := x2 + r1;
    y2 := y2 + r2;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := x2;
    y1 := y2;
    if level > 0 then
      generate(r1, r2);
    x2 := x2 + r2;
    y2 := y2 + r1;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := x2;
    y1 := y2;
    if level > 0 then
      generate(r1, r2);
    x2 := x2 - r1;
    y2 := y2 - r2;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := x2;
    y1 := y2;
    if level > 0 then
      generate(-r2, -r1);
    inc(level);
  end;
begin
{gfColor:=4;   }
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Hilbert' + ' Level: ' + IStringer(Level));
      Brush.Color := TempColor;
      MainForm.Show;
      temp := 2;
      i := level;
      while i > 1 do
      begin
        temp := temp * 2;
        dec(i);
      end;
      r := 400 / temp;
      x1 := -200;
      y1 := -200;
      x2 := -200;
      y2 := -200;
      generate(r, 0);
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor; Brush.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 10, 'Hilbert' + ' Level: ' + IStringer(Level));
        Brush.Color := TempColor;
        MainForm.Show;
        temp := 2;
        i := level;
        while i > 1 do
        begin
          temp := temp * 2;
          dec(i);
        end;
        r := 400 / temp;
        x1 := -200;
        y1 := -200;
        x2 := -200;
        y2 := -200;
        generate(r, 0);
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Procedure }



procedure hilbert2(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfColor, }
  I, lc, x, y, old_x, old_y, h: integer;

  procedure gen2(i: integer); forward;
  procedure gen3(i: integer); forward;
  procedure gen4(i: integer); forward;
  procedure gen1(i: integer);
  begin
    if i > 0 then
    begin
      gen4(i - 1);
      x := x - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen1(i - 1);
      y := y - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen1(i - 1);
      x := x + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen2(i - 1);
    end;
  end;

  procedure gen2(i: integer);
  begin
    if i > 0 then
    begin
      gen3(i - 1);
      y := y + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen2(i - 1);
      x := x + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen2(i - 1);
      y := y - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen1(i - 1);
    end;
  end;

  procedure gen3(i: integer);
  begin
    if i > 0 then
    begin
      gen2(i - 1);
      x := x + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
      old_x := x;
      old_y := y;
      gen3(i - 1);
      y := y + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
{	Line(old_x+320,240-(old_y*32) div 48,x+320,240-(y*32) div 48);}
      old_x := x;
      old_y := y;
      gen3(i - 1);
      x := x - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
{	Line(old_x+320,240-(old_y*32) div 48,x+320,240-(y*32) div 48);}
      old_x := x;
      old_y := y;
      gen4(i - 1);
    end;
  end;

  procedure gen4(i: integer);
  begin
    if i > 0 then
    begin
      gen1(i - 1);
      y := y - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
{	Line(old_x+320,240-(old_y*32) div 48,x+320,240-(y*32) div 48);}
      old_x := x;
      old_y := y;
      gen4(i - 1);
      x := x - h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
{	Line(old_x+320,240-(old_y*32) div 48,x+320,240-(y*32) div 48);}
      old_x := x;
      old_y := y;
      gen4(i - 1);
      y := y + h;
      MainForm.Image2.Canvas.Moveto(old_x + 320, 240 - old_y);
        {(old_y*32) div 48}
      MainForm.Image2.Canvas.Lineto(x + 320, 240 - y); {(y*32) div 48}
{	Line(old_x+320,240-(old_y*32) div 48,x+320,240-(y*32) div 48);}
      old_x := x;
      old_y := y;
      gen3(i - 1);
    end;
  end;

begin
{GfColor:=4;  }
  if level < 0 then level := 0
  else if level > 7 then level := 7;

  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Font.Color := TempColor;
      TextOut(10, 10, 'Hilbert 2' + ' Level: ' + IStringer(Level));
      Pen.Color := TempColor; Brush.Color := TempColor;
      MainForm.Show;
      h := 480;
      x := 0;
      y := 0;
      for i := 1 to level do
      begin
        h := h div 2;
        x := x + h div 2;
        y := y + h div 2;
        old_x := x;
        old_y := y;
      end;
      gen1(level);
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Font.Color := TempColor;
        TextOut(10, 10, 'Hilbert 2' + ' Level: ' + IStringer(Level));
        Pen.Color := TempColor; Brush.Color := TempColor;
        Font.Color := TempColor;
        MainForm.Show;
        h := 480;
        x := 0;
        y := 0;
        for i := 1 to level do
        begin
          h := h div 2;
          x := x + h div 2;
          y := y + h div 2;
          old_x := x;
          old_y := y;
        end;
        gen1(level);
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end;
  end;
end; { of Procedure }



procedure hil3d(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor, }
  lc, i: integer;
  points: array[0..2] of Extended;
  temp, x1, x2, y1, y2, r, x_angle, y_angle, z_angle, cx, cy, cz, sx,
    sy, sz: Extended;

  procedure generate(a: integer; b: integer; c: integer);

  var
    sign: array[0..2] of integer;

  begin
    sign[0] := 1;
    sign[1] := 1;
    sign[2] := 1;
    dec(level);
    if a < 0 then
      sign[0] := -1;
    a := Abs(a) - 1;
    if b < 0 then
      sign[1] := -1;
    b := Abs(b) - 1;
    if c < 0 then
      sign[2] := -1;
    c := Abs(c) - 1;
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(-2, 1, 3);
    points[a] := points[a] + (r * sign[0]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(3, 1, -2);
    points[b] := points[b] + (r * sign[1]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(3, 1, -2);
    points[a] := points[a] - (r * sign[0]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(2, -3, 1);
    points[c] := points[c] + (r * sign[2]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(-3, 1, 2);
    points[a] := points[a] + (r * sign[0]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(-2, 3, 1);
    points[b] := points[b] - (r * sign[1]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(3, -1, 2);
    points[a] := points[a] - (r * sign[0]);
    x2 := points[0] * cx + points[1] * cy + points[2] * cz;
    y2 := points[0] * sx + points[1] * sy + points[2] * sz;
    MainForm.Image2.Canvas.Moveto(Round(x1 + 320), Round(240 - y1));
    MainForm.Image2.Canvas.Lineto(Round(x2 + 320), Round(240 - y2));
    x1 := points[0] * cx + points[1] * cy + points[2] * cz;
    y1 := points[0] * sx + points[1] * sy + points[2] * sz;
    if level > 0 then
      generate(-2, -1, -3);
    inc(level);
  end;

begin
{gfColor:=4;    }
  x_angle := -55;
  y_angle := 90;
  z_angle := 0;
  if level < 0 then level := 0
  else if level > 3 then level := 3;

  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor; {Brush.Color:=TempColor;  }
      Font.Color := TempColor;
      MainForm.Show;
      sx := Sin(x_angle * 0.017453292);
      sy := Sin(y_angle * 0.017453292);
      sz := Sin(z_angle * 0.017453292);
      cx := Cos(x_angle * 0.017453292);
      cy := Cos(y_angle * 0.017453292);
      cz := Cos(z_angle * 0.017453292);
      temp := 2;
      i := level;
      while i > 1 do
      begin
        temp := temp * 2;
        dec(i);
      end;
      r := 300 / temp;
      points[0] := -200;
      points[1] := 50;
      points[2] := 0;
      generate(3, -2, 1);
      TextOut(10, 10, 'Hilbert 3D' + ' Level: ' + IStringer(Level));
    end; end else
  begin

    for lc := 1 to 2 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor; {Brush.Color:=TempColor;}
        Font.Color := TempColor;
        MainForm.Show;
        sx := Sin(x_angle * 0.017453292);
        sy := Sin(y_angle * 0.017453292);
        sz := Sin(z_angle * 0.017453292);
        cx := Cos(x_angle * 0.017453292);
        cy := Cos(y_angle * 0.017453292);
        cz := Cos(z_angle * 0.017453292);
        temp := 2;
        i := level;
        while i > 1 do
        begin
          temp := temp * 2;
          dec(i);
        end;
        r := 300 / temp;
        points[0] := -200;
        points[1] := 50;
        points[2] := 0;
        generate(3, -2, 1);
        TextOut(10, 10, 'Hilbert 3D' + ' Level: ' +
          IStringer(Level));
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end; end;
end; { of Procedure }


procedure sierp(level: integer); { of 8 }
var
  TempColor: TColor; maxcolx, maxrowy, {[gfColor,  ]}
  lc, generator_size: integer;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; sign: integer);
  var
    j, k, int_sign: integer;
    Xpoints, Ypoints: array[0..24] of Extended;
  begin
    turtle_r := sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)) /
      2.0;
    turtle_x := X1;
    turtle_y := Y1;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[3] := X2;
    Ypoints[3] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turn(60 * sign);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    turn(-60 * sign);
    step;
    Xpoints[2] := turtle_x;
    Ypoints[2] := turtle_y;
    dec(level);
    sign := -sign;
    if level = 0 then
    begin
      for k := 0 to generator_size - 1 do
      begin
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end
    else
    begin
      int_sign := sign;
      for j := 0 to generator_size - 1 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, int_sign);
        int_sign := -int_sign;
      end;
    end;
  end;

begin

  generator_size := 3;
{GfColor:=4;  }
  if level < 0 then level := 0
  else if level > 8 then level := 8;
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor; {Brush.Color:=TempColor;}
      Font.Color := TempColor;
      TextOut(10, 10, 'Sierpinski' + ' Level: ' + IStringer(Level));
      MainForm.Show;
      generate(-150, -50, 150, -50, level, 1);
        { -50 was 0, 150 was 130 }
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      level := lc;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor; {Brush.Color:=TempColor;}
        Font.Color := TempColor;
        TextOut(10, 10, 'Sierpinski' + ' Level: ' +
          IStringer(Level));
        MainForm.Show;
        generate(-150, -50, 150, -50, level, 1); { 50 was 0 }
{inc(GFColor); }
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end; { of   Sierp }




procedure siergask(level: integer); {fast enough at all 5 levels }
var
  TempColor: TColor; maxcolx, maxrowy, {gfcolor, }
  lc, L_length, x1, y1, x2, y2, x3, y3: integer;
  Triangle: array[1..3] of TPoint;

  procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
    x3: integer; y3: integer; x4: integer; y4: integer;
    x5: integer; y5: integer; x6: integer; y6: integer;
    level: integer; l_length: integer); forward;

  procedure generate(x1: integer; y1: integer; x2: integer; y2:
    integer;
    x3: integer; y3: integer; level: integer; l_length: integer);
  var
    line_length, x4, y4, x5, y5, x6, y6: integer;
  begin
    line_length := l_length div 2;
    x4 := x1 + line_length;
    y4 := y1;
    x5 := x1 + line_length div 2;
{???} y5 := y1 - Round((line_length / 2) * 1.26292);
    x6 := x5 + line_length;
    y6 := y5;
    node(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, level,
      line_length);
  end;

  procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
    x3: integer; y3: integer; x4: integer; y4: integer;
    x5: integer; y5: integer; x6: integer; y6: integer;
    level: integer; l_length: integer);

  begin
    Triangle[1].x := x4;
    Triangle[1].y := y4;
    Triangle[2].x := x5;
    Triangle[2].y := y5;
    Triangle[3].x := x6;
    Triangle[3].y := y6;
    MainForm.Image2.Canvas.Polygon(Triangle);
    if level <> 0 then
    begin
      generate(x1, y1, x4, y4, x5, y5, level - 1, l_length);
      generate(x4, y4, x2, y2, x6, y6, level - 1, l_length);
      generate(x5, y5, x6, y6, x3, y3, level - 1, l_length);
    end;
  end;

begin
  if level < 0 then level := 0
  else if level > 5 then level := 5;
{GfColor:=4; }
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Sier Gasket' + ' Level: ' + IStringer(Level));
      MainForm.Show;
{Brush.Color:=TempColor;}
      x1 := 64; {64;}
      y1 := 385; {335}
      x2 := 576; {576;}
      y2 := 385; {335}
      x3 := 320;
      y3 := 65; {15}
      l_length := 512; {512;}

{	SetFillStyle(1,4);}
      Triangle[1].x := x1;
      Triangle[1].y := y1;
      Triangle[2].x := x2;
      Triangle[2].y := y2;
      Triangle[3].x := x3;
      Triangle[3].y := y3;
      MainForm.Image2.Canvas.Polygon(Triangle);
       {	SetFillStyle(1,3);
 SetBkColor(1);
 SetColor(GfColor);}
      generate(x1, y1, x2, y2, x3, y3, level, l_length);
    end; end else
  begin
    for lc := 1 to 4 do
    begin
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        Textout(10, 10, 'Sier Gasket' + ' Level: ' +
          IStringer(Level));
        MainForm.Show;
{inc(GFColor); }
        level := lc;
        x1 := 64; {64;}
        y1 := 385;
        x2 := 576; {576;}
        y2 := 385;
        x3 := 320;
        y3 := 65;
        l_length := 512; {512;}

{	SetFillStyle(1,4);}
        Triangle[1].x := x1;
        Triangle[1].y := y1;
        Triangle[2].x := x2;
        Triangle[2].y := y2;
        Triangle[3].x := x3;
        Triangle[3].y := y3;
        MainForm.Image2.Canvas.Polygon(Triangle);
{	SetFillStyle(1,3);
 SetBkColor(1);
 SetColor(GfColor);}
        generate(x1, y1, x2, y2, x3, y3, level, l_length);

        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);
      end;
    end; end;
end; { of  Siergask  }



procedure sierbox(level: integer); { of 3 }
{const}
var
  Square: array[1..4] of TPoint;
var
  TempColor: TColor;
{maxcolx,maxrowy,gfcolor,} lc, L_length,
    x1, y1, x2, y2: integer;

  procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
    x3: integer; y3: integer; x4: integer; y4: integer;
    level: integer; l_length: integer); forward;

  procedure generate(x1: integer; y1: integer; x2: integer; y2:
    integer;
    level: integer; l_length: integer);
  var
    line_length, x3, y3, x4, y4: integer;

  begin {35/49=.729...}
    line_length := l_length div 3;
    x3 := x1 + line_length;
    y3 := y1 - (35 * line_length) div 48;
    x4 := x2 - line_length;
    y4 := y2 + (35 * line_length) div 48;
    node(x1, y1, x2, y2, x3, y3, x4, y4, level, line_length);
  end;

  procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
    x3: integer; y3: integer; x4: integer; y4: integer;
    level: integer; l_length: integer);

  begin
    Square[1].x := x3;
    Square[1].y := y3;
    Square[2].x := x4;
    Square[2].y := y3;
    Square[3].x := x4;
    Square[3].y := y4;
    Square[4].x := x3;
    Square[4].y := y4;
    MainForm.Image2.Canvas.Polygon(Square);
    if level <> 0 then
    begin
      generate(x1, y1, x3, y3, level - 1, l_length);
      generate(x3, y1, x4, y3, level - 1, l_length);
      generate(x4, y1, x2, y3, level - 1, l_length);
      generate(x1, y3, x3, y4, level - 1, l_length);
      generate(x4, y3, x2, y4, level - 1, l_length);
      generate(x1, y4, x3, y2, level - 1, l_length);
      generate(x3, y4, x4, y2, level - 1, l_length);
      generate(x4, y4, x2, y2, level - 1, l_length);
    end;
  end;

begin
  if level < 0 then level := 0
  else if level > 3 then level := 3;
{GfColor:=4; }
{pcxVGASetup;  }
  if Level > 0 then
  begin
    with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1); }
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, FYImageX, FYImageY));
      TempColor := RGB(RGBArray[0, Level],
        RGBArray[1, Level], RGBArray[2, Level]);
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 470, 'Sierpinski Box' + ' Level: ' +
        IStringer(Level));
      Brush.Color := TempColor;
      MainForm.Show;
      x1 := 100; {100;}
      y1 := 335; {335}
      x2 := 540; {540;}
      y2 := 15; {15}
      l_length := 440; {440;}
{	SetFillStyle(1,15);}
      Square[1].x := x1;
      Square[1].y := y1;
      Square[2].x := x2;
      Square[2].y := y1;
      Square[3].x := x2;
      Square[3].y := y2;
      Square[4].x := x1;
      Square[4].y := y2;
      MainForm.Image2.Canvas.Polygon(Square);
{	SetFillStyle(1,4);  }{1,0}
{	SetColor(GfColor);
 SetBkColor(1);}
      generate(x1, y1, x2, y2, level, l_length);
    end; end else begin
    for lc := 1 to 3 do begin
      with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, FYImageX, FYImageY));
        TempColor := RGB(RGBArray[0, Level],
          RGBArray[1, Level], RGBArray[2, Level]);
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(10, 470, 'Sierpinski Box' + ' Level: ' +
          IStringer(Level));
        Brush.Color := TempColor;
        MainForm.Show;
        level := lc;
        x1 := 100; {100;}
        y1 := 335; {335}
        x2 := 540; {540;}
        y2 := 15; {15}
        l_length := 440; {440;}
{	SetFillStyle(1,15);}
        Square[1].x := x1;
        Square[1].y := y1;
        Square[2].x := x2;
        Square[2].y := y1;
        Square[3].x := x2;
        Square[3].y := y2;
        Square[4].x := x1;
        Square[4].y := y2;
        MainForm.Image2.Canvas.Polygon(Square);
{	SetFillStyle(1,12);
 SetColor(GfColor);}
        generate(x1, y1, x2, y2, level, l_length);
{inc(GFColor);}
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end; end;
  end;
end; { of  Sier Box  }


procedure Dragon_Curve(level: integer);
var
  XAxis, YAxis: array[1..4098] of integer;
  TempColor: TColor; maxcolx, maxrowy, Step, Sign: integer;

  procedure Generate_Dragon(color: integer);
  var
    i, j, dx, dy: integer;
  begin
    j := Step div 2;
{   setcolor( color );]}
    i := 1;
    repeat
      dx := xaxis[Step + i] - xaxis[i];
      dy := yaxis[Step + i] - yaxis[i];
      Sign := Sign * -1; { omit for Arabesque }
      xaxis[i + j] := xaxis[i] + (dx + (dy * sign)) div 2;
      yaxis[i + j] := yaxis[i] + (dy - (dx * sign)) div 2;
{      if color <> 0 then}
      begin
        MainForm.Image2.Canvas.Moveto(xaxis[i], yaxis[i]);
        MainForm.Image2.Canvas.Lineto(xaxis[i + j], yaxis[i + j]);
        MainForm.Image2.Canvas.Moveto(xaxis[i + j], yaxis[i + j]);
        MainForm.Image2.Canvas.Lineto(xaxis[i + Step], yaxis[i +
          Step]);
      end;
      inc(i, Step);
    until i >= 4096;
  end;

var {Change to getting the Image X Y}
{   GetMaxX,GetMaxY,} i: integer;
begin
  Step := 4096;
  sign := -1;
  maxcolx := (FYImageX - 1);
  maxrowy := (FYImageY - 1);
  xaxis[1] := maxcolx div 4;
  xaxis[4097] := 3 * maxcolx div 4; {480} { 3 * 160 = 480 }
  yaxis[1] := 3 * maxrowy div 4; {360} { 2 * 160 = 320 }
  yaxis[4097] := yaxis[1];
  if Level > 0 then begin
    with MainForm.Image2.Canvas do begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(1, 10, 'Fractal Dragon Curve' + ' Level: ' +
        IStringer(Level));
      MainForm.Show;
      Moveto(xaxis[1], yaxis[1]);
      Lineto(xaxis[4097], yaxis[4097]);
      Generate_Dragon(Level);
    end; end else begin
    with MainForm.Image2.Canvas do begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      MainForm.Show;
      Moveto(xaxis[1], yaxis[1]);
      Lineto(xaxis[4097], yaxis[4097]);
      for i := 3 to 14 do
      begin

        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(255 - GetRValue(FBackGroundColor),
          255 - GetGValue(FBackGroundColor),
          255 - GetBValue(FBackGroundColor));
        Pen.Color := TempColor;
        Font.Color := TempColor;
        Level := i;
        TextOut(1, 10, 'Fractal Dragon Curve' + ' Level: ' +
          IStringer(Level));

        Generate_Dragon(i);
        Step := Step div 2;
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);
      end;
    end; end;
end; { of procedure Dragon_Curve }



procedure twindrag(level: integer); {231.18 seconds}
var
  TempColor: TColor; maxcolx, maxrowy, DOIT,
  I, generator_size, init_size: integer;
  initiator_x1, initiator_x2,
    initiator_y1, initiator_y2: array[0..9] of Extended;
  sign: Extended;
  procedure generate(X1: Extended; Y1: Extended; X2: Extended; Y2:
    Extended;
    level: integer; sign: Extended);
  var
    j, k: integer;
    sign2: Extended;
    Xpoints, Ypoints: array[0..24] of Extended;

  begin
    sign2 := -1;
    turtle_r := (sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)))
      / 1.41421;
    Xpoints[0] := X1;
    Ypoints[0] := Y1;
    Xpoints[2] := X2;
    Ypoints[2] := Y2;
    turtle_theta := point(X1, Y1, X2, Y2);
    turtle_x := X1;
    turtle_y := Y1;
    turn(sign * 45);
    step;
    Xpoints[1] := turtle_x;
    Ypoints[1] := turtle_y;
    dec(level);
    if level > 0 then
    begin
      for j := 0 to generator_size - 2 do
      begin
        X1 := Xpoints[j];
        X2 := Xpoints[j + 1];
        Y1 := Ypoints[j];
        Y2 := Ypoints[j + 1];
        generate(X1, Y1, X2, Y2, level, sign2);
        sign2 := -sign2;
      end;
    end
    else
    begin
      for k := 0 to generator_size - 2 do
      begin {y[]*0.729}
        MainForm.Image2.Canvas.Moveto(Round(Xpoints[k] + 320),
          Round(240 - Ypoints[k]));
        MainForm.Image2.Canvas.Lineto(Round(Xpoints[k + 1] + 320),
          Round(240 - Ypoints[k + 1]));
      end;
    end;
  end;
begin
  initiator_x1[0] := -150;
  initiator_x1[1] := 150;
  initiator_x2[0] := 150;
  initiator_x2[1] := -150;
  sign := 1;
  generator_size := 3;
  init_size := 2;
  initiator_y1[0] := -25;
  initiator_y1[1] := -25;
  initiator_y2[0] := -25;
  initiator_y2[1] := -25;
{	level := 6;
REWRITE for LEVELS see others for loop}
  if Level > 0 then begin
    with MainForm.Image2.Canvas do begin
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, maxcolx, maxrowy));
      TempColor := RGB(255 - GetRValue(Level * 4),
        255 - GetGValue(Level * 4),
        255 - GetBValue(Level * 4));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(320, 10, 'Twin Dragon' + ' Level: ' +
        IStringer(Level));
      MainForm.Show;
      for i := 0 to init_size - 1 do
      begin
        generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
          initiator_y2[i], level, sign);
        TempColor := RGB(155 - GetRValue(Level * 4),
          155 - GetGValue(Level * 4),
          155 - GetBValue(Level * 4));
        Pen.Color := TempColor;
      end;
    end; end else
    for DOIT := 1 to 6 do
    begin
      Level := Doit;
      with MainForm.Image2.Canvas do begin
        maxcolx := (FYImageX - 1);
        maxrowy := (FYImageY - 1);
        Brush.Color := FBackGroundColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, maxcolx, maxrowy));
        TempColor := RGB(255 - GetRValue(Level * 4),
          255 - GetGValue(Level * 4),
          255 - GetBValue(Level * 4));
        Pen.Color := TempColor;
        Font.Color := TempColor;
        TextOut(320, 10, 'Twin Dragon' + ' Level: ' +
          IStringer(Level));
        MainForm.Show;
        for i := 0 to init_size - 1 do
        begin
          generate(initiator_x1[i], initiator_y1[i], initiator_x2[i],
            initiator_y2[i], level, sign);
          TempColor := RGB(155 - GetRValue(Level * 4),
            155 - GetGValue(Level * 4),
            155 - GetBValue(Level * 4));
          Pen.Color := TempColor;
        end;
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages until (bRotateImage =
          True);

      end;
    end;
end; { TWINDRAG }


end.
