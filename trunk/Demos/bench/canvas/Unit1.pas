{: Benchmark for GLCanvas.<p>

   This project pits TGLCanvas against TCanvas in direct mode (no double
   buffering, and hardware acceleration should be available on both sides).<p>
   You may usually bet on TGLCanvas being 3 to 5 times faster, but on some
   fast 3D hardware, or when PenWidth is not 1, the performance ratio can
   be much higher.<p>

   Figures for PenWidth = 1<p>

   CPU         Graphics Board    Lines          Ellipses         Points

   Duron 800   TNT2 M64          105 / 571      400 / 1148       126 / 676
   ----21/01/02 - Initial
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLMisc, ExtCtrls, StdCtrls, GLWin32Viewer;

type
  TForm1 = class(TForm)
    BULines: TButton;
    BUEllipses: TButton;
    GLSceneViewer: TGLSceneViewer;
    PaintBox: TPaintBox;
    LAGLCanvas: TLabel;
    LAGDI: TLabel;
    Bevel1: TBevel;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    RBPenWidth1: TRadioButton;
    RBPenWidth2: TRadioButton;
    BUPoints: TButton;
    procedure GLSceneViewerPostRender(Sender: TObject);
    procedure BULinesClick(Sender: TObject);
    procedure BUEllipsesClick(Sender: TObject);
    procedure BUPointsClick(Sender: TObject);
  private
    { Private declarations }
    procedure PaintTheBox;
    procedure Bench;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GLCanvas, GLCrossPlatform;

type
   TWhat = (wLines, wEllipses, wPoints);

var
   vWhat : TWhat;
   vPenWidth : Integer;

const
   cNbLines = 20000;
   cNbEllipses = 20000;
   cNbPoints = 200000;

procedure TForm1.BULinesClick(Sender: TObject);
begin
   vWhat:=wLines;
   Bench;
end;

procedure TForm1.BUEllipsesClick(Sender: TObject);
begin
   vWhat:=wEllipses;
   Bench;
end;

procedure TForm1.BUPointsClick(Sender: TObject);
begin
   vWhat:=wPoints;
   Bench;
end;

procedure TForm1.Bench;
var
   t : Int64;
begin
   if RBPenWidth1.Checked then
      vPenWidth:=1
   else vPenWidth:=2;

   Application.ProcessMessages;

   t:=StartPrecisionTimer;
   GLSceneViewer.Refresh;
   LAGLCanvas.Caption:=Format('GLCanvas: %.1f msec', [StopPrecisionTimer(t)*1000]);

   Application.ProcessMessages;

   t:=StartPrecisionTimer;
   PaintTheBox;
   LAGDI.Caption:=Format('GDI: %.1f msec', [StopPrecisionTimer(t)*1000]);
end;

procedure TForm1.GLSceneViewerPostRender(Sender: TObject);
var
   i : Integer;
   glc : TGLCanvas;
begin
   glc:=TGLCanvas.Create(256, 256);
   with glc do begin
      PenWidth:=vPenWidth;
      case vWhat of
         wLines : begin
            for i:=1 to cNbLines do begin
               PenColor:=Random(256*256*256);
               MoveTo(Random(256), Random(256));
               LineTo(Random(256), Random(256));
            end;
         end;
         wEllipses : begin
            for i:=1 to cNbEllipses do begin
               PenColor:=Random(256*256*256);
               Ellipse(Random(256), Random(256),
                       Random(256), Random(256));
            end;
         end;
         wPoints : begin
            for i:=1 to cNbPoints do begin
               PenColor:=Random(256*256*256);
               PlotPixel(Random(256), Random(256));
            end;
         end;
      end;
   end;
   glc.Free;
end;

procedure TForm1.PaintTheBox;
var
   i : Integer;
begin
   with PaintBox.Canvas do begin
      Brush.Style:=bsClear;
      Pen.Width:=vPenWidth;
      case vWhat of
         wLines : begin
            for i:=1 to cNbLines do begin
               Pen.Color:=Random(256*256*256);
               MoveTo(Random(256), Random(256));
               LineTo(Random(256), Random(256));
            end;
         end;
         wEllipses : begin
            for i:=1 to cNbEllipses do begin
               Pen.Color:=Random(256*256*256);
               Ellipse(Random(256), Random(256),
                       Random(256), Random(256));
            end;
         end;
         wPoints : begin
            for i:=1 to cNbPoints do begin
               Pixels[Random(256), Random(256)]:=Random(256*256*256);
            end;
         end;
      end;
   end;
end;

end.
