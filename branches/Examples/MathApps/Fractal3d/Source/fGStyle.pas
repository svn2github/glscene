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
unit fGStyle;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TDIYStyleForm = class(TForm)
    Panel1: TPanel;
    RectangleButton: TSpeedButton;
    EllipseButton: TSpeedButton;
    RoundRectButton: TSpeedButton;
    PenBar: TPanel;
    SolidPen: TSpeedButton;
    DashPen: TSpeedButton;
    DotPen: TSpeedButton;
    DashDotPen: TSpeedButton;
    DashDotDotPen: TSpeedButton;
    ClearPen: TSpeedButton;
    PenColor: TSpeedButton;
    PenWidth: TUpDown;
    PenSize: TEdit;
    BrushBar: TPanel;
    SolidBrush: TSpeedButton;
    ClearBrush: TSpeedButton;
    HorizontalBrush: TSpeedButton;
    VerticalBrush: TSpeedButton;
    FDiagonalBrush: TSpeedButton;
    BDiagonalBrush: TSpeedButton;
    CrossBrush: TSpeedButton;
    DiagCrossBrush: TSpeedButton;
    BrushColor: TSpeedButton;
    ColorDialog1: TColorDialog;
    ExitBtn: TSpeedButton;
    BitmapBrush: TSpeedButton;
    bBBrush: TPanel;
    StyleHelp: TBitBtn;
    PBrush: TSpeedButton;
    BrushSize: TEdit;
    BrushWidth: TUpDown;
    PaintBar: TPanel;
    cmSrcCopyBtn: TSpeedButton;
    cmmergeCopyBtn: TSpeedButton;
    cmMergePaintBtn: TSpeedButton;
    cmNotSrcEraseBtn: TSpeedButton;
    cmPatCopyBtn: TSpeedButton;
    cmPatPaintBtn: TSpeedButton;
    cmBlacknessBtn: TSpeedButton;
    cmNotSrcCopyBtn: TSpeedButton;
    cmSrcInvertBtn: TSpeedButton;
    cmSrcPaintBtn: TSpeedButton;
    cmWhitenessBtn: TSpeedButton;
    cmPatInvertBtn: TSpeedButton;
    cmSrcAndBtn: TSpeedButton;
    cmSrcEraseBtn: TSpeedButton;
    cmDstInvertBtn: TSpeedButton;
    LineButton: TSpeedButton;
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LineButtonClick(Sender: TObject);
    procedure RectangleButtonClick(Sender: TObject);
    procedure EllipseButtonClick(Sender: TObject);
    procedure RoundRectButtonClick(Sender: TObject);
    procedure PenColorClick(Sender: TObject);
    procedure BrushColorClick(Sender: TObject);
    procedure SetPenStyle(Sender: TObject);
    procedure PenSizeChange(Sender: TObject);
    procedure SetBrushStyle(Sender: TObject);
    procedure KillBitmapBrush;
    procedure DoBitmapBrush;
    procedure bBBrushClick(Sender: TObject);
    procedure PBrushClick(Sender: TObject);
    procedure BrushSizeChange(Sender: TObject);
    procedure StyleHelpClick(Sender: TObject);
    procedure cmSrcCopyBtnClick(Sender: TObject);
    procedure cmBlacknessBtnClick(Sender: TObject);
    procedure cmDstInvertBtnClick(Sender: TObject);
    procedure cmmergeCopyBtnClick(Sender: TObject);
    procedure cmMergePaintBtnClick(Sender: TObject);
    procedure cmNotSrcCopyBtnClick(Sender: TObject);
    procedure cmNotSrcEraseBtnClick(Sender: TObject);
    procedure cmPatCopyBtnClick(Sender: TObject);
    procedure cmPatInvertBtnClick(Sender: TObject);
    procedure cmPatPaintBtnClick(Sender: TObject);
    procedure cmSrcAndBtnClick(Sender: TObject);
    procedure cmSrcEraseBtnClick(Sender: TObject);
    procedure cmSrcInvertBtnClick(Sender: TObject);
    procedure cmSrcPaintBtnClick(Sender: TObject);
    procedure cmWhitenessBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DIYStyleForm: TDIYStyleForm;

  DIYSBitmap: TBitmap;

implementation

uses fUGlobal, fMain, fGMath;

{$R *.DFM}


procedure TDIYStyleForm.FormCreate(Sender: TObject);
begin
  top := DIYStyleFormY; {460;}
  left := DIYStyleFormX;  {560;}
  MainForm.DrawingTool := dtLine;
end;
procedure TDIYStyleForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  DIYStyleFormX := DIYStyleForm.left;
  DIYStyleFormY := DIYStyleForm.top;
  DoSaver;
end;
procedure TDIYStyleForm.ExitBtnClick(Sender: TObject);
begin
  KillBitmapBrush;
  Close;
end;

procedure TDIYStyleForm.LineButtonClick(Sender: TObject);
begin
  MainForm.DrawingTool := dtLine;
end;

procedure TDIYStyleForm.RectangleButtonClick(Sender: TObject);
begin
  MainForm.DrawingTool := dtRectangle;
end;

procedure TDIYStyleForm.EllipseButtonClick(Sender: TObject);
begin
  MainForm.DrawingTool := dtEllipse;
end;

procedure TDIYStyleForm.RoundRectButtonClick(Sender: TObject);
begin
  MainForm.DrawingTool := dtRoundRect;
end;

procedure TDIYStyleForm.PenColorClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Pen.Color;
  if ColorDialog1.Execute then
    MainForm.Image2.Canvas.Pen.Color := ColorDialog1.Color;
end;

procedure TDIYStyleForm.BrushColorClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Brush.Color;
  if ColorDialog1.Execute then
    MainForm.Image2.Canvas.Brush.Color := ColorDialog1.Color;
end;

procedure TDIYStyleForm.SetPenStyle(Sender: TObject);
begin
  with MainForm.Image2.Canvas.Pen do
  begin
    if Sender = SolidPen then Style := psSolid
    else if Sender = DashPen then Style := psDash
    else if Sender = DotPen then Style := psDot
    else if Sender = DashDotPen then Style := psDashDot
    else if Sender = DashDotDotPen then Style := psDashDotDot
    else if Sender = ClearPen then Style := psClear;
  end;

end;

procedure TDIYStyleForm.PenSizeChange(Sender: TObject);
begin
  MainForm.Image2.Canvas.Pen.Width := PenWidth.Position;
end;

procedure TDIYStyleForm.SetBrushStyle(Sender: TObject);
begin
  with MainForm.Image2.Canvas.Brush do
  begin
    if Sender = SolidBrush then Style := bsSolid
    else if Sender = ClearBrush then Style := bsClear
    else if Sender = HorizontalBrush then Style := bsHorizontal
    else if Sender = VerticalBrush then Style := bsVertical
    else if Sender = FDiagonalBrush then Style := bsFDiagonal
    else if Sender = BDiagonalBrush then Style := bsBDiagonal
    else if Sender = CrossBrush then Style := bsCross
    else if Sender = DiagCrossBrush then Style := bsDiagCross
    else if Sender = BitmapBrush then DoBitmapBrush;
    if Sender <> BitmapBrush then KillBitmapBrush;
  end;
end;

procedure TDIYStyleForm.KillBitmapBrush;
begin {}
  if (bBrushHasBitmap = True) then begin
    MainForm.Image2.Canvas.Brush.Bitmap := nil;
    DIYSBitmap.Free;
    bBBrush.Color := clRed;
    BrushBar.Color := clBtnFace;
    bBrushHasBitmap := False;
  end;
end;

procedure TDIYStyleForm.DoBitmapBrush;
var
  NameS: string;
begin
  bBrushHasBitmap := False;
  DIYSBitmap := TBitmap.Create;
  try
        {Check to SEE if a bitmap is loaded then
        make it THE Brush Bitmap}
    NameS := ExtractFileName(DIYFilename);
    if (NameS <> 'NONE.BMP') then begin
      DIYSBitmap.LoadFromFile(DIYFilename);
      MainForm.Image2.Canvas.Brush.Bitmap := DIYSBitmap;
      bBrushHasBitmap := True;
      bBBrush.Color := clLime;
      BrushBar.Color := clRed;
    end else begin
      DoMessages(30041);
      bBrushHasBitmap := False;
      bBBrush.Color := clRed;
      BrushBar.Color := clBtnFace;
    end;
{    MainForm.Image1.Canvas.FillRect(Rect(0,0,100,100));}
  finally
    if (bBrushHasBitmap = False) then begin
      MainForm.Image2.Canvas.Brush.Bitmap := nil;
      DIYSBitmap.Free;
      bBBrush.Color := clRed;
      BrushBar.Color := clBtnFace;
    end;
  end;
end;
{Specifies an external bitmap image
that defines a pattern for the brush.
property Bitmap: TBitmap;
Description
Bitmap points to a TBitmap object that holds a BMP image.
If Bitmap is nonempty,
the BMP image (rather than the Style property)
defines the brush’s pattern.
If the image is larger than eight pixels by eight pixels,
only the top left eight-by-eight region is used.
Changing the image does not affect the brush until
the TBitmap is reassigned to the Bitmap property.
Be sure to free the TBitmap after finishing
with the brush, since TBrush will not free it.

Determines whether the TransparentColor property's value
is automatically calculated or stored with the bitmap object.
property Transparent: TTransparentMode;
bitmap.TransparentMode := tmAuto;
Description
When TransparentMode is set to tmAuto,
the TransparentColor property returns the color of the
bottom-leftmost pixel of the bitmap image.
When TransparentMode is set to tmFixed,
the TransparentColor property refers to
the color stored in the bitmap object.}


procedure TDIYStyleForm.bBBrushClick(Sender: TObject);
begin
  if (bBrushHasBitmap = True) then begin
    MainForm.Image2.Canvas.Brush.Bitmap := nil;
    DIYSBitmap.Free;
    bBBrush.Color := clRed;
    BrushBar.Color := clBtnFace;
  end;
end;

procedure TDIYStyleForm.PBrushClick(Sender: TObject);
begin
  MainForm.DrawingTool := dtPBrush;
end;

procedure TDIYStyleForm.BrushSizeChange(Sender: TObject);
begin
  DrawingTool_PBrush_Width := BrushWidth.Position;
end;

procedure TDIYStyleForm.StyleHelpClick(Sender: TObject);
begin
  Application.HelpContext(2525);
end;

procedure TDIYStyleForm.cmSrcCopyBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmSrcCopy;
{cmSrcCopy
Copies the source bitmap to the canvas.}
end;

procedure TDIYStyleForm.cmBlacknessBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmBlackness;
{cmBlackness
Fills the destination rectangle on the canvas with black.}
end;

procedure TDIYStyleForm.cmDstInvertBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmDstInvert;
{cmDstInvert
Inverts the image on the canvas and ignores the source.}
end;

procedure TDIYStyleForm.cmmergeCopyBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmmergeCopy;
{cmMergeCopy
Combines the image on the canvas and the source bitmap
by using the Boolean AND operator.}
end;

procedure TDIYStyleForm.cmMergePaintBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmMergePaint;
{cmMergePaint
Combines the inverted source bitmap with the image on the canvas
by using the Boolean OR operator.}
end;

procedure TDIYStyleForm.cmNotSrcCopyBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmNotSrcCopy;
{cmNotSrcCopy
Copies the inverted source bitmap to the canvas.}
end;

procedure TDIYStyleForm.cmNotSrcEraseBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmNotSrcErase;
{cmNotSrcErase
Combines the image on the canvas and the source bitmap
 by using the Boolean OR operator, and inverts the result.}
end;

procedure TDIYStyleForm.cmPatCopyBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmPatCopy;
{cmPatCopy
Copies the source pattern to the canvas.}
end;

procedure TDIYStyleForm.cmPatInvertBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmPatInvert;
{cmPatInvert
Combines the source pattern with the image on the canvas
using the Boolean XOR operator}
end;

procedure TDIYStyleForm.cmPatPaintBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmPatPaint;
{cmPatPaint
Combines the inverted source bitmap with the source pattern
by using the Boolean OR operator.
Combines the result of this operation with the image
on the canvas by using the Boolean OR operator.}
end;

procedure TDIYStyleForm.cmSrcAndBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmSrcAnd;
{cmSrcAnd
Combines the image on the canvas and source bitmap
by using the Boolean AND operator.}
end;

procedure TDIYStyleForm.cmSrcEraseBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmSrcErase;
{cmSrcErase
Inverts the image on the canvas and combines the
result with the source bitmap by using the Boolean AND operator.}
end;

procedure TDIYStyleForm.cmSrcInvertBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmSrcInvert;
{cmSrcInvert
Combines the image on the canvas and the source bitmap
 by using the Boolean XOR operator.  }
end;

procedure TDIYStyleForm.cmSrcPaintBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmSrcPaint;
{cmSrcPaint
Combines the image on the canvas and the source bitmap
 by using the Boolean OR operator.}
end;

procedure TDIYStyleForm.cmWhitenessBtnClick(Sender: TObject);
begin
  MainForm.Image2.Canvas.CopyMode := cmWhiteness;
{cmWhiteness
Fills the destination rectangle on the canvas with white.}
end;



end.
