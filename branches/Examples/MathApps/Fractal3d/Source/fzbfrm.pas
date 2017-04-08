unit fzbfrm;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, fzBSim,
  StdCtrls, Menus, ImgList;

type
  TfzbForm = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    pb: TPaintBox;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    RunBut: TToolButton;
    StopBut: TToolButton;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Gravpb: TPaintBox;
    SelPanel: TPanel;
    Label1: TLabel;
    PointLab: TLabel;
    Label2: TLabel;
    XPosLab: TLabel;
    YPosLab: TLabel;
    Label3: TLabel;
    XVelLab: TLabel;
    YVelLab: TLabel;
    ToolButton1: TToolButton;
    ShapeBut: TToolButton;
    ShapeMenu: TPopupMenu;
    N3Sides1: TMenuItem;
    Sides1: TMenuItem;
    N5Sides1: TMenuItem;
    N6Sides1: TMenuItem;
    N7Sides1: TMenuItem;
    N8Sides1: TMenuItem;
    N20Sides1: TMenuItem;
    Normal1: TMenuItem;
    Big1: TMenuItem;
    Large1: TMenuItem;
    Huge1: TMenuItem;
    Panel4: TPanel;
    ToolButton2: TToolButton;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    ToolButton3: TToolButton;
    Panel5: TPanel;
    Label5: TLabel;
    TrackBar2: TTrackBar;
    procedure RunButClick(Sender: TObject);
    procedure StopButClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GravpbPaint(Sender: TObject);
    procedure GravpbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GravpbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GravpbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel3Resize(Sender: TObject);
    procedure ShapeMenuClick(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Panel5Resize(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StopNow    : boolean;
    bs         : TBounceSim;
    GravDown   : boolean;
    BeginMove  : boolean;
    Moving     : boolean;
    Connecting : boolean;
    SelPt      : TBPoint;
    Num        : integer;
    p1         : TBPoint;
    DownX      : integer;
    DownY      : integer;
    procedure  UpdateSelStats;
    procedure  SelectPoint(x,y: integer);
  end;

var
  fzbForm: TfzbForm;

implementation

{$R *.DFM}

procedure TfzbForm.RunButClick(Sender: TObject);
var
   bm       : TBitmap;
   nf       : integer;
   t1       : DWord;{integer;}
   td, fps  : double;
begin
   bm := TBitmap.Create;
   bm.Width := pb.Width;
   bm.Height := pb.Height;

   StopNow := False;
   t1 := GetTickCount;
   nf := 0;
   repeat
      Application.ProcessMessages;

      bs.RunIteration(bm.Canvas);

      pb.Canvas.Draw(0, 0, bm);
      UpdateSelStats;
      
      nf := nf + 1;
      td := (GetTickCount - t1) / 1000.0;
      fps := nf / td;
      StatusBar1.Panels[0].Text := Format('FPS = %5.1n', [fps]);
      StatusBar1.Panels[1].Text := Format('Frame %5.0n', [nf+0.0]);
      StatusBar1.Panels[2].Text := Format('Time = %6.2n', [td]);
   until StopNow or Application.Terminated;

   bm.Free;
end;

procedure TfzbForm.UpdateSelStats;
begin
   if SelPt<>nil then begin
      PointLab.Caption := IntToStr(SelPt.Num);
      XPosLab.Caption := Format('%5.1n', [SelPt.PosX]);
      YPosLab.Caption := Format('%5.1n', [SelPt.PosY]);
      XVelLab.Caption := Format('%5.1n', [SelPt.VelX]);
      YVelLab.Caption := Format('%5.1n', [SelPt.VelY]);
   end else begin
      PointLab.Caption := 'None';
      XPosLab.Caption := '';
      YPosLab.Caption := '';
      XVelLab.Caption := '';
      YVelLab.Caption := '';
   end;
   SelPanel.Update;
end;


procedure TfzbForm.StopButClick(Sender: TObject);
begin
   StopNow := True;
end;

procedure TfzbForm.FormCreate(Sender: TObject);
begin
   bs := TBounceSim.Create;
end;

procedure TfzbForm.FormDestroy(Sender: TObject);
begin
   bs.Free;
end;

procedure TfzbForm.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   bp : TBPoint;
begin
   DownX := X;
   DownY := Y;
   Moving := False;
   if ssCtrl in Shift then begin
      BeginMove := True;
      Num := Num + 1;
      bp := TBPoint.Create;
      bp.Num := Num;
      bp.PosX := X;
      bp.PosY := Y;
      bs.PointList.Add(bp);
      SelectPoint(x,y);
   end else if ssShift in Shift then begin
      if SelPt<>nil then Connecting := True;
      p1 := SelPt;
   end else begin
      BeginMove := True;
      SelectPoint(x,y);
   end;
   
   bs.Draw(pb.Canvas);
end;

procedure TfzbForm.GravpbPaint(Sender: TObject);
var
   xc, yc   : integer;
   rad      : integer;
   xd, yd   : integer;
begin
   xc := GravPB.Width div 2;
   yc := GravPB.Height div 2;

   rad := xc;
   if yc < rad then rad := yc;
   rad := rad - 2;

   with GravPB.Canvas do begin
      Brush.Color := clBtnFace;
      Ellipse(xc-rad, yc-rad, xc+rad,yc+rad);

      xd := Round(bs.GravX * rad);
      yd := Round(bs.GravY * rad);

      MoveTo(xc, yc);
      LineTo(xc + xd, yc + yd);
   end;
end;

procedure TfzbForm.GravpbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   GravDown := True;
   GravpbMouseMove(Sender, [], X, Y);
end;

procedure TfzbForm.GravpbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   GravDown := False;
end;

procedure TfzbForm.GravpbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   xc, yc   : integer;
   xd, yd   : integer;
   d        : double;
begin
   if GravDown then begin
      xc := GravPB.Width div 2;
      yc := GravPB.Height div 2;

      xd := X - xc;
      yd := Y - yc;
      d := Sqrt(xd*xd + yd*yd);
      if d=0 then exit;
      bs.GravX := xd / d;
      bs.GravY := yd / d;
      GravPB.Refresh;
   end;
end;

procedure TfzbForm.SelectPoint(x,y: integer);
var
   p : TBPoint;
begin
   p := bs.NearestPoint(x,y);
   if p<>nil then p.Selected := True;
   if (SelPt<>p) then begin
      if (SelPt<>nil) then SelPt.Selected := False;
      bs.Draw(pb.Canvas);
      SelPt := p;
   end;

   UpdateSelStats;
end;



procedure TfzbForm.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   l : TBLink;
begin
   BeginMove := False;

   if Moving then
      SelPt.Locked := False;
   Moving := False;

   if Connecting and (p1<>nil) and (SelPt<>p1) then begin
      l := TBLink.Create;
      l.Pt1 := bs.PointList.IndexOf(p1);
      l.Pt2 := bs.PointList.IndexOf(SelPt);
      l.LinkDis := p1.PointDis(SelPt);
      bs.LinkList.Add(l);
      bs.Draw(pb.Canvas);
   end;
   Connecting := False;
end;

procedure TfzbForm.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if SelPt=nil then exit;

   if BeginMove then
      if (Abs(X-DownX) > 3) or (Abs(Y-DownY) > 3) then Moving := True;

   if Connecting then begin
      SelectPoint(x,y);
      if SelPt<>p1 then begin
         bs.Draw(pb.Canvas);
         pb.Canvas.MoveTo(Round(p1.PosX), Round(p1.PosY));
         pb.Canvas.LineTo(Round(SelPt.PosX), Round(SelPt.PosY));
      end;
   end;

   if Moving then begin
      SelPt.PosX := X;
      SelPt.PosY := Y;
      SelPt.Locked := True;
      bs.Draw(pb.Canvas);
      UpdateSelStats;
   end;
end;

procedure TfzbForm.Panel3Resize(Sender: TObject);
begin
   bs.mx := pb.Width;
   bs.my := pb.Height;
end;

procedure TfzbForm.ShapeMenuClick(Sender: TObject);
var
   Num   : integer;
   i, j  : integer;
   ang   : double;
   x, y  : double;
   p     : TBPoint;
   ba    : array[0..20] of TBPoint;
   l     : TBLink;
   rad   : integer;
begin
   Num := (Sender as TComponent).Tag and $FF;

   rad := 50 + ((Sender as TComponent).Tag SHR 8 * 25);

   for i := 0 to Num-1 do begin
      ang := (2.0 * PI) * i / Num;
      x := sin(ang) * rad + pb.Width div 2;
      y := cos(ang) * rad + pb.Height div 2;

      p := TBPoint.Create;
      ba[i] := p;
      p.Num := i;
      p.PosX := x;
      p.PosY := y;
      bs.PointList.Add(p);
   end;

   for i := 0 to Num-1 do
      for j := i+1 to Num-1 do begin
         l := TBLink.Create;
         l.Pt1 := bs.PointList.IndexOf(ba[i]);
         l.Pt2 := bs.PointList.IndexOf(ba[j]);
         l.LinkDis := ba[i].PointDis(ba[j]);
         bs.LinkList.Add(l);
      end;


   bs.Draw(pb.Canvas);
end;

procedure TfzbForm.Panel4Resize(Sender: TObject);
begin
   TrackBar1.Width := Panel4.ClientWidth;
end;

procedure TfzbForm.TrackBar1Change(Sender: TObject);
begin
   case Trackbar1.Position of
      0 : AirFric := 0.95;
      1 : AirFric := 0.96;
      2 : AirFric := 0.97;
      3 : AirFric := 0.98;
      4 : AirFric := 0.985;
      5 : AirFric := 0.99;
      6 : AirFric := 0.993;
      7 : AirFric := 0.995;
      8 : AirFric := 0.997;
      9 : AirFric := 0.999;
      10 : AirFric := 0.9995;
   end;
end;

procedure TfzbForm.Panel5Resize(Sender: TObject);
begin
   TrackBar2.Width := Panel5.ClientWidth;
end;

procedure TfzbForm.TrackBar2Change(Sender: TObject);
begin
   case Trackbar2.Position of
      0 : LinkStr := 0.0005;
      1 : LinkStr := 0.001;
      2 : LinkStr := 0.005;
      3 : LinkStr := 0.01;
      4 : LinkStr := 0.02;
      5 : LinkStr := 0.05;
      6 : LinkStr := 0.1;
      7 : LinkStr := 0.13;
      8 : LinkStr := 0.15;
      9 : LinkStr := 0.175;
      10 : LinkStr := 0.2;
   end;
end;

end.
