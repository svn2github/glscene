unit fzcfrm;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages,
  Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus;

const
   NumPt   = 100;
   NumClus = 10;

type
  Tpoint = record
    x, y : integer;
    Clus : integer;
  end;

  TClus = record
    x, y   : integer;
    tx, ty : longint;
    count  : longint;
    ox, oy : integer;
  end;

  TfzcForm = class(TForm)
    fzcMainMenu1: TMainMenu;
    Go1: TMenuItem;
    pb: TPaintBox;
    Pts1: TMenuItem;
    Panel1: TPanel;
    procedure Go1Click(Sender: TObject);
    procedure Pts1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    pt        : array[1..NumPt] of TPoint;
    Clus      : array[1..NumPt] of TClus;
    b         : TBitmap;
    SelClus   : integer;
    procedure DrawClus;
  end;

var
  fzcForm: TfzcForm;

implementation

{$R *.DFM}

procedure TfzcForm.DrawClus;
const
   Col : array[1..16] of TColor = ( clAqua, clBlack, clBlue,
      clGray, clGreen, clLime, clPurple, clRed, clWhite, clYellow,
      clFuchsia, clMaroon, clNavy, clOlive, clLtGray, clTeal );
var
   i : integer;
   R : TRect;
begin
   b.Canvas.Font.Name := 'Arial';
   b.Canvas.Font.Size := 8;

   b.Canvas.Brush.Color := Color;
   R := Rect(0,0, b.width, b.height);
   b.Canvas.FillRect(R);

   for i := 1 to NumClus do begin
      b.Canvas.Brush.Color := clSilver;
      b.Canvas.Ellipse(Clus[i].x, Clus[i].y, Clus[i].x+5, Clus[i].y+5);
      b.Canvas.Brush.Color := Color;
      b.Canvas.TextOut(Clus[i].x+4, Clus[i].y+4, IntToStr(i));
   end;

   for i := 1 to NumPt do begin
      b.Canvas.Brush.Color := Col[pt[i].Clus];
      b.Canvas.Ellipse(pt[i].x, pt[i].y, pt[i].x+5, pt[i].y+5);
   end;

   pb.Canvas.Draw(0,0, b);
end;

function PtDis(x1, y1, x2, y2 : integer): single;
var
   xd, yd : single;
begin
   xd := x2 - x1;
   yd := y2 - y1;
   Result := xd * xd + yd * yd;
end;

procedure TfzcForm.Go1Click(Sender: TObject);
label
   More;
var
   i, j,Xmax, Ymax : integer;
   MinDis, Dis, TotDis     : single;
   which, NumNew   : integer;
begin
   Go1.Enabled := False;

   { Create Random Initial cluster centers }
   Xmax := pb.Width - 5;
   Ymax := pb.Height - 5;
   pb.Canvas.Brush.Color := clGreen;
   for i := 1 to NumClus do begin
      Clus[i].x := Random(Xmax);
      Clus[i].y := Random(Ymax);
   end;

More:
   { Assign each point to nearest cluster }
      { Init Cluster Stats }
      for i := 1 to NumClus do with Clus[i] do begin
         ox := x;
         oy := y;
         tx := 0;
         ty := 0;
         count := 0;
      end;

      TotDis := 0;
      for i := 1 to NumPt do begin
         MinDis := 1e+30;
         which  := -1;
         for j := 1 to NumClus do begin
            Dis := PtDis(pt[i].x, pt[i].y, Clus[j].x, Clus[j].y);
            if Dis < MinDis then begin
               MinDis := Dis;
               which := j;
            end;
         end;
         TotDis := TotDis + MinDis;
         pt[i].Clus := which;
         Clus[which].tx := Clus[which].tx + pt[i].x;
         Clus[which].ty := Clus[which].ty + pt[i].y;
         Clus[which].count := Clus[which].count + 1;
      end;
   Panel1.Caption := 'TotDis = ' + FloatToStrF(TotDis, ffFixed, 7, 0);

   { Move Clusters to center of gravity }
   NumNew := 0;
   for i := 1 to NumClus do if i<>SelClus then with Clus[i] do begin
      if count > 0 then begin
         x := ROUND(tx / count);
         y := ROUND(ty / count);
      end else begin
         x := Random(Xmax);
         y := Random(Ymax);
      end;
      if (x<>ox) or (y<>oy) then NumNew := NumNew + 1;
   end;

   { Check for clusters with no points }

   { Draw Clusters }
   DrawClus;
   Application.ProcessMessages;

   { Repeat until Clusters haven't moved }
   if (NumNew >= 0) and (not Application.Terminated) then
      goto More;
end;

procedure TfzcForm.Pts1Click(Sender: TObject);
var
   i, Xmax, Ymax : integer;
begin
   { Create Random Points }
   Xmax := pb.Width - 5;
   Ymax := pb.Height - 5;
   pb.Canvas.Brush.Color := clRed;
   for i := 1 to NumPt do begin
      pt[i].x := Random(Xmax);
      pt[i].y := Random(Ymax);
      pt[i].Clus := 1;
   end;

   DrawClus;
end;

procedure TfzcForm.FormCreate(Sender: TObject);
begin
   b := TBitmap.Create;
   Pts1Click(Self);
   Show;
   SelClus := -1;
end;

procedure TfzcForm.pbPaint(Sender: TObject);
begin
   if Application.Terminated then exit;
   DrawClus;
end;

procedure TfzcForm.FormResize(Sender: TObject);
begin
   if Application.Terminated then exit;
   b.Width := pb.Width;
   b.Height := pb.Height;
end;

procedure TfzcForm.FormDestroy(Sender: TObject);
begin
   b.Free;
end;

procedure TfzcForm.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   xd, yd      : single;
   mindis,dis  : single;
   i           : integer;
begin
   { Find the nearest cluster to X, Y }
   SelClus := -1;
   mindis  := 1e38;
   for i := 1 to NumClus do begin
      xd := Clus[i].x - X;
      yd := Clus[i].y - Y;
      dis := xd*xd + yd*yd;
      if dis < mindis then begin
         mindis := dis;
         SelClus := i;
      end;
   end;
   Clus[SelClus].x := X;
   Clus[SelClus].y := Y;
   pb.OnPaint(Self);
end;

procedure TfzcForm.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if SelClus = -1 then exit;

   Clus[SelClus].x := X;
   Clus[SelClus].y := Y;
end;

procedure TfzcForm.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   SelClus := -1;
end;

end.
