{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit nGSRShow;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TGSRShowForm = class(TForm)
    SaveDialog1: TSaveDialog;
    GRFPanel: TPanel;
    Image1: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;
    procedure FormCreate(Sender: TObject);
    procedure InitializeWorld;
    procedure GrowGrass;
    procedure FeedRabbits;
    procedure FoxRabbits;
    procedure MakeSallyDance;
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
     
  public
     
  end;

var
  GSRShowForm: TGSRShowForm;

implementation
uses nUGlobal, nGRF;
{$R *.DFM}

procedure TGSRShowForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GSRShowFormY := GSRShowForm.top;
  GSRShowFormX := GSRShowForm.left;
  RabbitsRunning := False;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TGSRShowForm.ReallyClose;
begin
  Close;
end;

procedure TGSRShowForm.FormCreate(Sender: TObject);
begin
  top := GSRShowFormY;
  left := GSRShowFormX;
end;

procedure TGSRShowForm.InitializeWorld;
var Planting, I, J: Integer;
  NewRect: TRect;
{CheckString:String;}
begin
{Set the Array size 0..?-1, 0..?-1}
  SetLength(WorldArray, GRFWide, GRFHeight);
  Randomize;
  for I := 0 to GRFWide - 1 do
    for J := 0 to GRFHeight - 1 do begin
      WorldArray[I, J].What := Empty;
      WorldArray[I, J].Mark := 0;
      WorldArray[I, J].Energy := 0;
      Image1.Canvas.Brush.Color := Desolate;
      NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
        4));
      Image1.Canvas.FillRect(NewRect);
    end;
  Application.ProcessMessages;
  Planting := 0;
  repeat
    begin
      I := Random(GRFWide);
      J := Random(GRFHeight);
      if (WorldArray[I, J].What = Empty) then
      begin
        inc(Planting);
        WorldArray[I, J].What := Grass;
        WorldArray[I, J].Energy := GrassEnergy;
        Image1.Canvas.Brush.Color := Full;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end
  until (Planting = Grasses);
  Planting := 0;
  repeat
    begin
      I := Random(GRFWide);
      J := Random(GRFHeight);
      if (WorldArray[I, J].What = Empty) then
      begin
        inc(Planting);
        WorldArray[I, J].What := Rabbit;
        WorldArray[I, J].Energy := RabbitEnergy;
        Image1.Canvas.Brush.Color := Growing;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end
  until (Planting = Rabbits);
  Planting := 0;
  repeat
    begin
      I := Random(GRFWide);
      J := Random(GRFHeight);
      if (WorldArray[I, J].What = Empty) then
      begin
        inc(Planting);
        WorldArray[I, J].What := Fox;
        WorldArray[I, J].Energy := FoxEnergy;
        Image1.Canvas.Brush.Color := Overpopulated;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end
  until (Planting = Foxes);
{ What:Short;Empty=0;Grass=1;Rabbit=2;Fox=3;NewGrass=4;
  Mark:Short;
  Energy:Integer;  }
end;

procedure TGSRShowForm.GrowGrass;
var Planting, I, J, NI, NJ: Integer;
  NewRect: TRect;
begin
{ What:Short;Empty=0;Grass=1;Rabbit=2;Fox=3;NewGrass=4;
  Mark:Short;
  Energy:Integer;  }
  for I := 1 to GRFWide - 2 do begin
    for J := 1 to GRFHeight - 2 do begin
      WorldArray[I, J].Energy := (WorldArray[I, J].Energy + 1);
      if (WorldArray[I, J].What = Empty) then
      begin
        Planting := 0;
        WorldArray[I, J].Energy := (WorldArray[I, J].Energy + 1);
        for NI := -1 to 1 do begin
          for NJ := -1 to 1 do begin
            if (WorldArray[I + NI, J + NJ].What = Grass) then
              inc(Planting);
          end;
        end;
        if (((Planting < GrassGMax)
          and (Planting > GrassGMin))
          or (WorldArray[I, J].Energy >= GrassGE))
          then WorldArray[I, J].What := NewGrass;
      end;
    end;
  end;
  Grasses := 0;
  for I := 1 to GRFWide - 2 do begin
    for J := 1 to GRFHeight - 2 do begin
      if (WorldArray[I, J].What = NewGrass) then
        WorldArray[I, J].What := Grass;
      if WorldArray[I, J].What = Grass then
      begin
        inc(Grasses);
        Image1.Canvas.Brush.Color := Full;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TGSRShowForm.FeedRabbits;
var I, J, NI, NJ: Integer;
  NewRect: TRect;
begin
{ What:Short;Empty=0;Grass=1;Rabbit=2;Fox=3;NewGrass=4;
  Mark:Short;
  Energy:Integer;  }
  for I := 1 to GRFWide - 2 do begin
    for J := 1 to GRFHeight - 2 do begin
      if ((WorldArray[I, J].What = Rabbit)
        and (WorldArray[I, J].Energy > RabbitEMove)) then
      begin
        NI := Random(3) - 1;
        NJ := Random(3) - 1;
        if ((WorldArray[I + NI, J + NJ].What = Grass)) then
        begin
          WorldArray[I + NI, J + NJ].What := Rabbit;
          WorldArray[I + NI, J + NJ].Energy :=
            (WorldArray[I + NI, J + NJ].Energy + GrassEnergy +
            WorldArray[I, J].Energy - RabbitEMove);
          if WorldArray[I + NI, J + NJ].Energy >= RabbitEnergy then
          begin
            WorldArray[I + NI, J + NJ].Energy :=
              (WorldArray[I + NI, J + NJ].Energy div 2);
            WorldArray[I, J].Energy := WorldArray[I + NI, J +
              NJ].Energy;
          end else
          begin
            WorldArray[I, J].Energy := 0;
            WorldArray[I, J].What := Empty;
            WorldArray[I, J].Mark := 0;
          end;
        end else
        begin
          WorldArray[I + NI, J + NJ].What := Rabbit;
          WorldArray[I + NI, J + NJ].Energy :=
            (WorldArray[I, J].Energy - RabbitEMove);
        end;
      end else begin
        if ((WorldArray[I, J].What = Rabbit)
          and (WorldArray[I, J].Energy < RabbitEMove)) then
        begin
          WorldArray[I, J].What := Empty;
          WorldArray[I, J].Mark := 0;
        end;
      end;
    end;
  end;
  Rabbits := 0;
  for I := 1 to GRFWide - 2 do begin
{     Application.ProcessMessages;}
    for J := 1 to GRFHeight - 2 do begin
      WorldArray[I, J].Mark := 0;
      if WorldArray[I, J].What = Rabbit then
      begin
        inc(Rabbits);
        Image1.Canvas.Brush.Color := Growing;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TGSRShowForm.FoxRabbits;
var {Planting,}  I, J, NI, NJ: Integer;
  NewRect: TRect;
{StillHungry:Boolean;}
begin
  for I := 1 to GRFWide - 2 do begin
    for J := 1 to GRFHeight - 2 do begin
      if ((WorldArray[I, J].What = Fox)
        and (WorldArray[I, J].Energy > FoxEMove)) then
      begin
{        StillHungry:=True;}
        for NI := -1 to 1 do begin
          for NJ := -1 to 1 do begin
            if ((WorldArray[I + NI, J + NJ].What = Rabbit)) then
            begin
{          StillHungry:=False;}
              WorldArray[I + NI, J + NJ].What := Empty;
              WorldArray[I, J].Energy :=
                (WorldArray[I + NI, J + NJ].Energy + RabbitEnergy +
                WorldArray[I, J].Energy);
            end; end; end;
        if WorldArray[I, J].Energy >= FoxEnergy then
        begin {Split}
          NI := Random(3) - 1;
          NJ := Random(3) - 1;
          WorldArray[I + NI, J + NJ].What := Fox;
          WorldArray[I + NI, J + NJ].Energy :=
            (WorldArray[I, J].Energy div 2) - FoxEMove;
          WorldArray[I, J].Energy := (WorldArray[I, J].Energy div 2);
        end else
        begin {move}
          NI := Random(3) - 1;
          NJ := Random(3) - 1;
          WorldArray[I + NI, J + NJ].What := Fox;
          WorldArray[I + NI, J + NJ].Energy := WorldArray[I, J].Energy
            - FoxEMove;
          WorldArray[I, J].Energy := 0;
          WorldArray[I, J].What := Empty;
          WorldArray[I, J].Mark := 0;
        end;
{        If (StillHungry
             and(WorldArray[I,J].Energy>FoxEMove)
             and(WorldArray[I,J].What = Fox)) then
        begin
           NI:=(Random(2)-1);
           NJ:=(Random(2)-1);
           WorldArray[I+NI,J+NJ].What :=Fox;
           WorldArray[I+NI,J+NJ].Energy:=
           (WorldArray[I+NI,J+NJ].Energy +
            WorldArray[I,J].Energy - FoxEMove);
           WorldArray[I,J].Energy:=0;
           WorldArray[I,J].What:=Empty;  }
      end else
        if ({StillHungry
             and}(WorldArray[I, J].Energy <= FoxEMove)
          and (WorldArray[I, J].What = Fox)) then
        begin
         { WorldArray[I,J].Energy:=0;}
          WorldArray[I, J].What := Empty;
{        end;}
        end;
    end;
  end;

  Foxes := 0;
  for I := 1 to GRFWide - 2 do begin
    for J := 1 to GRFHeight - 2 do begin
      WorldArray[I, J].Mark := 0;
      if WorldArray[I, J].What = Fox then
      begin
        inc(Foxes);
        Image1.Canvas.Brush.Color := Overpopulated;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end;
    end;
  end;
  Application.ProcessMessages;
end;


procedure TGSRShowForm.MakeSallyDance;
var
  Bitmap: TBitmap;
  SallyGString, CheckGString, CheckRString, CheckFString: string;
  NewRect: TRect;
  Sally, I, J, NI: Integer;
begin
  RabbitsRunning := True;
  if (GRFHeight > 61) then begin
    GSRShowForm.Left := 0;
    GSRShowForm.Top := 0;
  end else begin
    GSRShowForm.Left := GSRShowFormX;
    GSRShowForm.Top := GSRShowFormY;
  end;
  GSRShowForm.Width := (GRFWide * 4) + 11 + 4;
  GSRShowForm.Height := (GRFHeight * 4) + 28 + 4;
  GRFPanel.Left := 0;
  GRFPanel.Top := 0;
  GRFPanel.Width := (GRFWide * 4) + 4;
  GRFPanel.Height := (GRFHeight * 4) + 4;
  Image1.Left := 1;
  Image1.Top := 1;
  Image1.Width := (GRFWide * 4) + 1; {391 401}
  Image1.Height := (GRFHeight * 4) + 1; {241 268}
  Bitmap := TBitmap.Create;
  Bitmap.Width := (GRFWide * 4) + 1; {393}
  Bitmap.Height := (GRFHeight * 4) + 1; {241}
  Image1.Picture.Graphic := Bitmap;
  Application.ProcessMessages;
  InitializeWorld;
  Sally := 0;
  repeat
    begin
      inc(Sally);
      GrowGrass;
      FeedRabbits;
      FoxRabbits;
      for I := 0 to GRFWide - 1 do begin
        for J := 0 to GRFHeight - 1 do begin
          if WorldArray[I, J].What = Empty then
            Image1.Canvas.Brush.Color := Desolate;
          if WorldArray[I, J].What = Grass then
            Image1.Canvas.Brush.Color := Full;
          if WorldArray[I, J].What = Rabbit then
            Image1.Canvas.Brush.Color := Growing;
          if WorldArray[I, J].What = Fox then
            Image1.Canvas.Brush.Color := Overpopulated;
          NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
            4));
          Image1.Canvas.FillRect(NewRect);
        end;
      end;
      str(Sally, SallyGString);
      str(Grasses, CheckGString);
      str(Rabbits, CheckRString);
      str(Foxes, CheckFString);
      GSRShowForm.Caption := '#: ' + SallyGString +
        ' Grass: ' + CheckGString +
        ' Rabbit: ' + CheckRString +
        ' Fox: ' + CheckFString;
      Application.ProcessMessages;

      if ((RabbitsRunning) and GRFForm.GRFCheckBox.Checked) then begin
        if Grasses = 0 then for NI := 0 to 1000 do
          begin
            I := Random(GRFWide - 1) + 1;
            J := Random(GRFHeight - 1) + 1;
            WorldArray[I, J].What := Grass;
            WorldArray[I, J].Energy := GrassEnergy;
            inc(Grasses);
          end;
        if Rabbits = 0 then for NI := 0 to 100 do
          begin
            I := Random(GRFWide - 1) + 1;
            J := Random(GRFHeight - 1) + 1;
            WorldArray[I, J].What := Rabbit;
            WorldArray[I, J].Energy := RabbitEnergy;
            inc(Rabbits);
          end;
        if Foxes = 0 then for NI := 0 to 10 do
          begin
            I := Random(GRFWide - 1) + 1;
            J := Random(GRFHeight - 1) + 1;
            WorldArray[I, J].What := Fox;
            WorldArray[I, J].Energy := FoxEnergy;
            inc(Foxes);
          end;
      end;
    end;
  until ((not RabbitsRunning) or (Grasses = 0)
    or (Foxes = 0) or (Rabbits = 0));
  SetLength(WorldArray, 0, 0);
end;


procedure TGSRShowForm.Image1MouseUp(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    RabbitsRunning := False;
    GSRShowForm.Close;
    GRFForm.GRFOKBtn.Enabled := True;
  end
  else if (Button = mbRight) then
  begin
    SaveDialog1.DefaultExt := GraphicExtension(TBitmap);
    SaveDialog1.Filter := GraphicFilter(TBitmap);
    if SaveDialog1.Execute then
      Image1.Picture.Bitmap.SaveToFile(SaveDialog1.Filename);
  end;
end;


end.
