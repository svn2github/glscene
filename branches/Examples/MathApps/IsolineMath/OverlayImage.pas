unit OverlayImage;
(*
*****      Base Component for Graphing     ****************************
**              TOverlayImage
**
**             © Renate Schaaf
**            renates@xmission.com
**
**  Specialty: Has methods for
**  temporary flickerless speedy overlayed drawings
**  like zoom rectangles or even sprites.
**  Use the usual canvas routines with the prefix Overlay,
**  like OverlayEllipse, OverlayRectangle, etc.
**  Exceptions:
**  The analog of Moveto/Lineto is as a command OverlayLine.
**  The analog of Canvas.Draw(x,y,MyGraphic) is OvelayDraw(DestRect,MyGraphic).
**  After finished with the overlayed (possibly compound) drawing,
**  call ShowOverlay. The next overlayed drawing
**  will start from scratch.
**
**                                                                       *)


interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Clipbrd;

type
  TOverlayImage = class(TGraphicControl)
  private
    fBitmap: TBitmap;
    //Bitmap for the persistant Drawing
    fDrawWidth, fDrawHeight: Integer;
    fScreenBitmap: TBitmap;
    //Bitmap that gets blasted to screen
    fLockCount: Integer;
    fDrawing: Boolean;
    fTempDrawing: Boolean;
    fOverlayed: Boolean;
    fClipRect: TRect;
    //ClipRect for fBitmap
    fTempRect: TRect;
    //fTempRect is fClipRect translated to screen
    //coordinates
    fClipRgn: HRgn;
    //corresponding region
    fOldRgn, fNewRgn: HRgn;
    //regions to optimize overlaid drawing
    fxoff, fyoff: Integer;
    //scaling data, bmp to screen, rsp. device
    //regions have to be given in device coordinates.
    //if the control is not located at top=left=0 in
    //the parent, those will be offset from the control
    //coordinates
    fOrgFontChanged,
      fOrgPenChanged,
      fOrgBrushChanged,
      fOrgPaint,
      fOnMouseLeave,
      fOnMouseEnter: TNotifyEvent;
    fOnTempPaint: TNotifyEvent;
    //Something that should always be added


    procedure fBitmapChanged(Sender: TObject);
    procedure fFontChanged(Sender: TObject);
    procedure fPenChanged(Sender: TObject);
    procedure fBrushChanged(Sender: TObject);

    function GetCanvas: TCanvas;
    function GetTempCanvas: TCanvas;
    function GetMetafileCanvas: TCanvas;
    function GetFont: TFont;
    function GetPen: TPen;
    function GetBrush: TBrush;
    function GetTempFont: TFont;
    function GetTempBrush: TBrush;
    function GetTempPen: TPen;

    procedure SetFont(Value: TFont);
    procedure SetPen(Value: TPen);
    procedure SetBrush(Value: TBrush);
    procedure SetTempFont(Value: TFont);
    procedure SetTempBrush(Value: TBrush);
    procedure SetTempPen(Value: TPen);

    procedure GetOffSet;
    procedure fTempCanvasChanging;

    { Private declarations }
  protected
    fMetaFileCanvas: TMetaFileCanvas;
    fMetaFile: TMetaFile;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMMouseLeave(var msg: TMessage); message CM_MouseLeave;
    procedure CMMouseEnter(var msg: TMessage); message CM_MouseEnter;
    procedure WMWindowPosChanged(var msg: TMessage); message
      WM_WINDOWPOSCHANGED;
    procedure SizeChanged; virtual;
    procedure Loaded; override;
    {: Canvas for overlaid drawings like
    zoom rectangles, or helper shapes which aren't part of
    the actual drawings. Now protected, because it can't be
    used properly without some specific precautions.
    }
    property OverlayCanvas: TCanvas read GetTempCanvas;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: Define a new rectangular clip region. The previous region
     is discarded.
    }
    procedure NewClipRegion(Value: TRect);

    {: Add a rectangle to the current clip region.
    }
    procedure AddClipRegion(Value: TRect);

    {: Subtract a rectangular region from the current clip region.
    }
    procedure SubtractClipRegion(Value: TRect);

    {: Intersect the current clip region with the region given by Value.
    }
    procedure IntersectClipRegion(Value: TRect);

    {: Call when the component is inside a scrollbox to adjust to scroll. Since
    the usual scrollbox has no OnScroll event, there is a derived one
    (<See class=TScrollEventBox>) in this same unit.
    }
    procedure AdjustToScroll;

    {: Drawing surrounded by LockUpdate and UnlockUpdate is not
     updated to the screen immediately. Nested calls are OK,
     Screen will be updated on the last UnlockUpdate. This speeds up
     compound drawing.
     }
    procedure LockUpdate;

    {: Drawing surrounded by LockUpdate and UnlockUpdate is not
     updated to the screen immediately. Nested calls are OK,
     Screen will be updated on the last UnlockUpdate. This speeds up
     compound drawing.
     }
    procedure UnlockUpdate;

    {: A call to ShowOverlay puts the current drawing on the
    Overlaycanvas to screen. The next Overlaycanvas call, or a call of
    <See method=HideOverlay> clears the canvas.
    }
    procedure ShowOverlay;

    {: Clears the overlayed canvas. Call, if no overlayed drawing is needed anymore,
    as this speeds up normal drawing.
    }
    procedure HideOverlay;

    {: Clears the Canvas, sets background to AColor.
    }
    procedure Clear(Acanvas: TCanvas; AColor: TColor); // overload; virtual;

    {: Saves any drawing on the <See property=MetafileCanvas> to file.
    }
    procedure SaveMetafile(const filename: string);

    {: Releases memory for metafile support.
    }
    procedure EraseMetafile;

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayCopyRect(dest: TRect; Canvas: TCanvas; Source: TRect);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayDraw(dest: TRect; Graphic: TGraphic);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayFillRect(const Rect: TRect);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayFrameRect(const Rect: TRect);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayLine(x1, y1, x2, y2: Integer);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayPolygon(Points: array of TPoint);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayPolyline(Points: array of TPoint);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayEllipse(x1, y1, x2, y2: Integer);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayRectangle(x1, y1, x2, y2: Integer);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayRoundRect(x1, y1, x2, y2, X3, Y3: Integer);

    {: For speed optimized drawings on the overlayed canvas use this
    analogon of the TCanvas method.
    }
    procedure OverlayTextOut(x, y: Integer; const s: string);

    {: Actually the canvas of the offscreen fbitmap.
    }
    property Canvas: TCanvas read GetCanvas;


    {: In case you'd like to draw a metafile, just use this property.
    It will be created, if needed.
    }
    property MetafileCanvas: TCanvas read GetMetafileCanvas;

    {: The metafile generated by drawing on <See property=MetafileCanvas>.
    }
    property Metafile: TMetaFile read fMetaFile;

    {: This Bitmap which holds the current main (not overlayed) drawing.
    }
    property Bitmap: TBitmap read fBitmap;

    property OverlayBrush: TBrush read GetTempBrush write SetTempBrush;

    property OverlayPen: TPen read GetTempPen write SetTempPen;

    property OverlayFont: TFont read GetTempFont write SetTempFont;


    { Public declarations }
  published
    property Align;


    {: Pen, brush and font properties for the main drawing. To set the corresponding
    for the overlayed canvas use OverlayPen. For the Metafile canvas use MetafileCanvas.Pen,
    as usual.
    }
    property Pen: TPen read GetPen write SetPen;

    {: Pen, brush and font properties for the main drawing. To set the corresponding
    for the overlayed canvas use OverlayBrush. For the Metafile canvas use MetafileCanvas.Brush,
    as usual.
    }
    property Brush: TBrush read GetBrush write SetBrush;

    {: Pen, brush and font properties for the main drawing. To set the corresponding
    for the overlayed canvas use OverlayFont. For the Metafile canvas use MetafileCanvas.Font,
    as usual.
    }
    property Font: TFont read GetFont write SetFont;

    {: Events}
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnResize;

    {: If you want to see a persistent drawing on the overlayed canvas, use this
    event for the drawing commands.
    }
    property OnOverlayPaint: TNotifyEvent read fOnTempPaint write fOnTempPaint;

    {: Event which fires if the mouse leaves the control. Note: There must be space
    between the control and the boundary of the parent for this to work.
    }
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;

    {: Event which fires if the mouse enters the control.
    }
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;

    { Published declarations }
  end;

  {: Had to create an extra scrollbox, which fires scrollevents, because
  when the TOverlayImage is scrolled, the offsets for the device regions
  have to be recomputed. See procedure TOverlayImage.AdjustToScroll
  }
  TScrollEventBox = class(TScrollbox)
  private
    fOnScroll: TNotifyEvent;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  published
    {: Event fires on vertical or horizontal scroll.
    }
    property OnScroll: TNotifyEvent read fOnScroll write fOnScroll;
  end;

procedure Register;

implementation

constructor TOverlayImage.Create;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    ControlStyle := ControlStyle + [csOpaque];
  {avoids flicker}
  fDrawWidth := 0;
  fDrawHeight := 0;
  fScreenBitmap := TBitmap.Create;
  fBitmap := TBitmap.Create;
  fOrgPenChanged := fBitmap.Canvas.Pen.OnChange;
  fOrgFontChanged := fBitmap.Canvas.Font.OnChange;
  fOrgBrushChanged := fBitmap.Canvas.Brush.OnChange;
  fBitmap.OnChange := fBitmapChanged;
  fLockCount := 0;
  fOrgPaint := nil;
  fMetaFileCanvas := nil;
  fMetaFile := nil;
  fClipRgn := 0;
  fOldRgn := 0;
  fNewRgn := 0;
  if csDesigning in ComponentState then
  begin
    Width := 50;
    Height := 50;
  end;
  fClipRect := Rect(0, 0, Width, Height);
  fTempRect := Rect(0, 0, Width, Height);
  fTempDrawing := False;
end;

destructor TOverlayImage.Destroy;
begin
  if fBitmap <> nil then fBitmap.Free;
  if fBitmap <> nil then fScreenBitmap.Free;
  if fMetaFileCanvas <> nil then fMetaFileCanvas.Free;
  if fMetaFile <> nil then fMetaFile.Free;
  if fClipRgn <> 0 then DeleteObject(fClipRgn);
  if fOldRgn <> 0 then DeleteObject(fOldRgn);
  if fNewRgn <> 0 then DeleteObject(fNewRgn);
  inherited;
end;

procedure TOverlayImage.Paint;
begin
 // inherited;
  if (csDesigning in ComponentState) then
    inherited Canvas.FrameRect(Rect(0, 0, Width, Height))
  else
  begin
    if not fDrawing then
    begin
      fDrawing := True;
      try
        if assigned(fOnTempPaint) then
        begin
          BitBlt(fScreenBitmap.Canvas.Handle, 0, 0, fDrawWidth, fDrawHeight,
            fBitmap.Canvas.Handle, 0, 0, SRCCopy);
          fTempDrawing := True;
          fOnTempPaint(self);
          fTempDrawing := False;
          BitBlt(inherited Canvas.Handle, 0, 0, fDrawWidth, fDrawHeight,
            fScreenBitmap.Canvas.Handle,
            0, 0, SRCCopy);
        end
        else
          if fOverlayed then
            BitBlt(inherited Canvas.Handle, 0, 0, fDrawWidth, fDrawHeight,
              fScreenBitmap.Canvas.Handle,
              0, 0, SRCCopy)
          else
            BitBlt(inherited Canvas.Handle, 0, 0, fDrawWidth, fDrawHeight,
              fBitmap.Canvas.Handle, 0, 0, SRCCopy);
      finally
        fDrawing := False;
      end;
    end;
  end;
end;

function TOverlayImage.GetCanvas;
begin
  SelectClipRgn(fBitmap.Canvas.Handle, fClipRgn);
  Result := fBitmap.Canvas;
end;


function TOverlayImage.GetTempCanvas;
begin
  SelectClipRgn(fScreenBitmap.Canvas.Handle, fNewRgn);
  Result := fScreenBitmap.Canvas;
end;

procedure TOverlayImage.fTempCanvasChanging;
begin
  if not fTempDrawing then
  begin
    fTempDrawing := True;
    SelectClipRgn(fScreenBitmap.Canvas.Handle, fOldRgn);
    BitBlt(fScreenBitmap.Canvas.Handle, 0, 0, fDrawWidth, fDrawHeight,
      fBitmap.Canvas.Handle,
      0, 0, SRCCopy);
    fOverlayed := False;
  end;
end;

procedure TOverlayImage.fBitmapChanged;
begin
  if not fDrawing then
  begin
    if fOldRgn <> 0 then
    begin
      DeleteObject(fOldRgn);
      fOldRgn := 0;
    end;
    fOverlayed := False;
    invalidate;
  end
  else update;
  {On each bitmap change the picture is invalidated, unless it's
   just being painted already and unless LockUpdate has been called}
end;

procedure TOverlayImage.fFontChanged(Sender: TObject);
begin
  fOrgFontChanged(Sender);
  if fMetaFileCanvas <> nil then
    fMetaFileCanvas.Font.assign(fBitmap.Canvas.Font);
end;

procedure TOverlayImage.fPenChanged(Sender: TObject);
begin
  fOrgPenChanged(Sender);
  if fMetaFileCanvas <> nil then
    fMetaFileCanvas.Pen.assign(fBitmap.Canvas.Pen);
end;

procedure TOverlayImage.fBrushChanged(Sender: TObject);
begin
  fOrgBrushChanged(Sender);
  if fMetaFileCanvas <> nil then
    fMetaFileCanvas.Brush.assign(fBitmap.Canvas.Brush);
end;


function TOverlayImage.GetMetafileCanvas: TCanvas;
begin
  if fMetaFileCanvas = nil then
  begin
    fMetaFile := TMetaFile.Create;
    fMetaFile.enhanced := True;
    fMetaFile.Height := fBitmap.Height;
    fMetaFile.Width := fBitmap.Width;
    fMetaFileCanvas := TMetaFileCanvas.Create(fMetaFile, 0);
    with fBitmap.Canvas do
    begin
      Font.OnChange := fFontChanged;
      Brush.OnChange := fBrushChanged;
      Pen.OnChange := fPenChanged;
    end;
    fFontChanged(nil); fPenChanged(nil); fBrushChanged(nil);
  end;
  SelectClipRgn(fMetaFileCanvas.Handle, fClipRgn);
  Result := fMetaFileCanvas;
end;

procedure TOverlayImage.SaveMetafile(const filename: string);
begin
  if fMetaFileCanvas <> nil then
  begin
    fMetaFileCanvas.Free;
    fMetaFile.SaveToFile(filename);
    fMetaFileCanvas := TMetaFileCanvas.Create(fMetaFile, 0);
    fMetaFileCanvas.draw(0, 0, Metafile);
  end;
end;

function TOverlayImage.GetFont: TFont;
begin
  Result := fBitmap.Canvas.Font;
end;

function TOverlayImage.GetPen: TPen;
begin
  Result := fBitmap.Canvas.Pen;
end;

function TOverlayImage.GetBrush: TBrush;
begin
  Result := fBitmap.Canvas.Brush;
end;

procedure TOverlayImage.SetFont(Value: TFont);
begin
  fBitmap.Canvas.Font.assign(Value);
end;

procedure TOverlayImage.SetPen(Value: TPen);
begin
  fBitmap.Canvas.Pen.assign(Value);
end;

procedure TOverlayImage.SetBrush(Value: TBrush);
begin
  fBitmap.Canvas.Brush.assign(Value);
end;

procedure TOverlayImage.NewClipRegion(Value: TRect);
begin
  if fClipRgn <> 0 then DeleteObject(fClipRgn);
  fClipRgn := 0;
  fClipRgn := CreateRectRgnIndirect(Value);
end;

procedure TOverlayImage.AddClipRegion(Value: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(Value);
  CombineRgn(fClipRgn, fClipRgn, rgn, RGN_OR);
  DeleteObject(rgn);
end;

procedure TOverlayImage.SubtractClipRegion(Value: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(Value);
  CombineRgn(fClipRgn, fClipRgn, rgn, RGN_DIFF);
  DeleteObject(rgn);
end;

procedure TOverlayImage.IntersectClipRegion(Value: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(Value);
  CombineRgn(fClipRgn, fClipRgn, rgn, RGN_And);
  DeleteObject(rgn);
end;



procedure TOverlayImage.WMWindowPosChanged;
begin
  inherited;
  if not (csDesigning in ComponentState) then  if assigned(Parent) then
    begin
      if (Width <> fDrawWidth) or (Height <> fDrawHeight) then
      begin
        fDrawWidth := Width;
        fDrawHeight := Height;
        SizeChanged;
        if fOldRgn <> 0 then
          DeleteObject(fOldRgn);
        fOldRgn := 0;
        fTempDrawing := False;
        fOverlayed := False;
      end;
     GetOffSet;                   //// ????
    end;
end;

procedure TOverlayImage.SizeChanged;
begin
  fScreenBitmap.Width := 0;
  fScreenBitmap.Width := fDrawWidth;
  fScreenBitmap.Height := fDrawHeight;
 // fBitmap.Width:=0;
  fBitmap.Width := fDrawWidth;
  fBitmap.Height := fDrawHeight;
  if fMetaFile <> nil then
  begin
    fMetaFileCanvas.Free;
    fMetaFile.Width := Width;
    fMetaFile.Height := Height;
    fMetaFileCanvas := TMetaFileCanvas.Create(fMetaFile, 0);
    fMetaFileCanvas.draw(0, 0, fMetaFile);
  end;
end;

procedure TOverlayImage.Loaded;
begin
  inherited;
  fDrawWidth := Width;
  fDrawHeight := Height;
  Clear(Canvas, Brush.Color);
end;

procedure TOverlayImage.GetOffSet;
var
  p, q: TPoint;
begin
  if assigned(Parent) then
  begin
    with Parent do
      p := ClienttoScreen(Point(0, 0));
    q := ClienttoScreen(Point(0, 0));
    fxoff := q.x - p.x;
    fyoff := q.y - p.y;
  end;
end;



procedure TOverlayImage.CMMouseLeave;
begin
  inherited;
  if assigned(fOnMouseLeave) then
    fOnMouseLeave(self);
  {use to get rid of stray drawing on the TempCanvas}
end;

procedure TOverlayImage.CMMouseEnter;
begin
  inherited;
  if assigned(fOnMouseEnter) then
    fOnMouseEnter(self);
end;


procedure TOverlayImage.ShowOverlay;
var
  DC: HDC;
begin
  fTempDrawing := True;
  DC := inherited Canvas.Handle;
  if assigned(fOnTempPaint) then fOnTempPaint(self);
  if fOldRgn <> 0 then
  begin
    if fNewRgn <> 0 then
    begin
      CombineRgn(fOldRgn, fOldRgn, fNewRgn, RGN_OR);
      OffsetRgn(fOldRgn, fxoff, fyoff);
      SelectClipRgn(DC, fOldRgn);
    end
    else
      SelectClipRgn(DC, 0);
    DeleteObject(fOldRgn);
  end
  else
    SelectClipRgn(DC, 0);
  BitBlt(DC, 0, 0, fDrawWidth, fDrawHeight,
    fScreenBitmap.Canvas.Handle,
    0, 0, SRCCopy);
  fOldRgn := fNewRgn;
  fNewRgn := 0;
  fTempDrawing := False;
  fOverlayed := True;
end;

procedure TOverlayImage.LockUpdate;
begin
  fBitmap.OnChange := nil;
  {Don't update bitmap to screen}
  inc(fLockCount);
end;

procedure TOverlayImage.UnlockUpdate;
begin
  dec(fLockCount);
  if fLockCount <= 0 then
  begin
    fLockCount := 0;
    {safety}
    fBitmap.OnChange := fBitmapChanged;
    fBitmapChanged(nil);
  end;
end;



procedure TOverlayImage.Clear(Acanvas: TCanvas; AColor: TColor);
begin
  Acanvas.Brush.Color := AColor;
  Acanvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TOverlayImage.EraseMetafile;
begin
  if fMetaFileCanvas <> nil then
    fMetaFileCanvas.Free;
  if fMetaFile <> nil then
    fMetaFile.Free;
  fMetaFileCanvas := nil;
  fMetaFile := nil;
  with fBitmap.Canvas do
  begin
    Font.OnChange := fOrgFontChanged;
    Brush.OnChange := fOrgBrushChanged;
    Pen.OnChange := fOrgPenChanged;
  end;
end;

procedure TScrollEventBox.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if assigned(fOnScroll) then fOnScroll(self);
end;

procedure TScrollEventBox.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if assigned(fOnScroll) then fOnScroll(self);
end;




procedure TOverlayImage.AdjustToScroll;
begin
  GetOffSet;
end;


procedure TOverlayImage.OverlayCopyRect(dest: TRect; Canvas: TCanvas;
  Source: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(dest);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn;
  fTempCanvasChanging;
  OverlayCanvas.CopyRect(dest, Canvas, Source);
end;

procedure TOverlayImage.OverlayDraw(dest: TRect; Graphic: TGraphic);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(dest);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.draw(dest.Left, dest.Top, Graphic);
end;

procedure TOverlayImage.OverlayEllipse(x1, y1, x2, y2: Integer);
var
  rgn: HRgn;
begin
  rgn := CreateEllipticRgnIndirect(Rect(x1 - 1, y1 - 1, x2 + 2, y2 + 2));
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.Ellipse(x1, y1, x2, y2);
end;

procedure TOverlayImage.OverlayFillRect(const Rect: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(Rect);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.FillRect(Rect);
end;

procedure TOverlayImage.OverlayFrameRect(const Rect: TRect);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgnIndirect(Rect);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.FrameRect(Rect);
end;

procedure TOverlayImage.OverlayLine(x1, y1, x2, y2: Integer);
var
  rgn: HRgn;
  xmin, ymin, xmax, ymax: Integer;
  points: array[0..2] of TPoint;
begin
  if x1 < x2 then
  begin
    xmin := x1;
    xmax := x2;
  end
  else
  begin
    xmin := x2;
    xmax := x1;
  end;
  if y1 < y2 then
  begin
    ymin := y1;
    ymax := y2;
  end
  else
  begin
    ymin := y2;
    ymax := y1;
  end;
  rgn := CreateRectRgnIndirect(Rect(xmin - 1, ymin - 1, xmax + 1, ymax + 1));
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  Points[0]:=Point(x1,y1);
  Points[1]:=Point(x2,y2);
  Points[2]:=Points[0];
  OverlayCanvas.Polyline(Points);
end;

procedure TOverlayImage.OverlayPolygon(Points: array of TPoint);
var
  rgn: HRgn;
begin
  rgn := CreatePolygonRgn(Points, High(Points) + 1, WINDING);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.Polygon(Points);
end;

procedure TOverlayImage.OverlayPolyline(Points: array of TPoint);
var
  rgn: HRgn;
begin
  rgn := CreatePolygonRgn(Points, High(Points) + 1, WINDING);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.Polyline(Points);
end;

procedure TOverlayImage.OverlayRectangle(x1, y1, x2, y2: Integer);
var
  rgn: HRgn;
begin
  rgn := CreateRectRgn(x1, y1, x2, y2);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.Rectangle(x1, y1, x2, y2);
end;

procedure TOverlayImage.OverlayRoundRect(x1, y1, x2, y2, X3, Y3: Integer);
var
  rgn: HRgn;
begin
  rgn := CreateRoundRectRgn(x1, y1, x2 + 3, y2 + 3, X3, Y3);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  fTempCanvasChanging;
  OverlayCanvas.RoundRect(x1, y1, x2, y2, X3, Y3);
end;

procedure TOverlayImage.Notification;
begin
  //doesn't do any good tho
  inherited;
  if csDesigning in ComponentState then
    repaint;
end;

procedure TOverlayImage.HideOverlay;
begin
  fOverlayed := False;
  invalidate;
end;

procedure TOverlayImage.OverlayTextOut(x, y: Integer; const s: string);
var
  rgn: HRgn;
  w, h: Integer;
begin
  with OverlayCanvas do
  begin
    w := TextWIdth(s);
    h := TextHeight(s);
  end;
  rgn := CreateRectRgn(x, y, x + w, y + h);
  if fNewRgn <> 0 then
  begin
    CombineRgn(fNewRgn, fNewRgn, rgn, RGN_OR);
    DeleteObject(rgn);
  end
  else
    if not fTempDrawing then
      fNewRgn := rgn
    else
      DeleteObject(rgn);
  OverlayCanvas.TextOut(x, y, s);
end;

function TOverlayImage.GetTempBrush: TBrush;
begin
  Result := fScreenBitmap.Canvas.Brush;
end;

function TOverlayImage.GetTempFont: TFont;
begin
  Result := fScreenBitmap.Canvas.Font;
end;

function TOverlayImage.GetTempPen: TPen;
begin
  Result := fScreenBitmap.Canvas.Pen;
end;

procedure TOverlayImage.SetTempBrush(Value: TBrush);
begin
  fScreenBitmap.Canvas.Brush.assign(Value);
end;

procedure TOverlayImage.SetTempFont(Value: TFont);
begin
  fScreenBitmap.Canvas.Font.assign(Value);
end;

procedure TOverlayImage.SetTempPen(Value: TPen);
begin
  fScreenBitmap.Canvas.Pen.assign(Value);
end;

procedure Register;
begin
  RegisterComponents('MathStuff', [TOverlayImage, TScrollEventBox]);
end;


end.

