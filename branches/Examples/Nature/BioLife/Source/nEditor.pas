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
unit nEditor;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Grids, Menus, ExtCtrls, FileCtrl;

type
  TEditorForm = class(TForm)
    DrawGrid2: TDrawGrid;
    b78: TRadioButton;
    b39: TRadioButton;
    LifEdOpenBtn: TSpeedButton;
    LifEdResetBtn: TSpeedButton;
    LifeEditorBtn: TSpeedButton;
    SandM78: TRadioButton;
    SOB39: TRadioButton;
    SOS156: TRadioButton;
    SOPixels: TRadioButton;
    LifEdSaveBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;

    procedure LifEdOpenBtnClick(Sender: TObject);
    procedure LifEdResetBtnClick(Sender: TObject);
    procedure EditorClearCells;
    procedure LifeEditorBtnClick(Sender: TObject);
    procedure DrawGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid2SelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure LifEdSaveBtnClick(Sender: TObject);
    procedure ChangeArray;
    procedure GoFigure(X0, X1, X2, Y0, Y1, Y2: Integer);

  private
     
  public
     
  end;

var
  EditorForm: TEditorForm;

implementation
uses nUGlobal, nEP, ncLife{, nLifeFiles};
{$R *.DFM}
var Bitmap: TBitmap;
  TVEd, {:Array3924;}
    TV7848Ed, {:Array7848;}
    TV15696Ed: array of array of Byte;
(********************************************************************)
(********************************************************************)

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  top := EditorFormY;
  left := EditorFormX;
  bCell1 := True;
  SetLength(TVEd, 39, 24);
  SetLength(TV7848Ed, 78, 48);
  SetLength(TV15696Ed, 156, 96);
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  EditorFormY := EditorForm.top;
  EditorFormX := EditorForm.left;
  SetLength(TVEd, 0, 0);
  SetLength(TV7848Ed, 0, 0);
  SetLength(TV15696Ed, 0, 0);
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TEditorForm.ReallyClose;
begin
  Close;
end;



(********************************************************************)
(********************************************************************)

procedure TEditorForm.LifEdOpenBtnClick(Sender: TObject);
var
  ImageByte: Byte;
  fil: file of Byte; {Array3924;}
  fil84: file of Byte; {Array7848;}
  i, j: Integer; CheckStr: string;
begin {}
    OpenDialog1.InitialDir := nLifeDir;
  if b39.Checked then begin
 {set OpenDialog1 filter to display Life's only}
    OpenDialog1.filter := 'Life Large files|*.llf';
    if OpenDialog1.execute then begin
    nLifeDir:= ExtractFilePath(OpenDialog1.filename);
  {Check the file extension to be b39}
      CheckStr := Uppercase(ExtractFileExt(OpenDialog1.filename));
      if (CheckStr = '.LLF') then begin
        bEditor39 := True; bEditor78 := False;
        EditorClearCells;
        AssignFile(fil, OpenDialog1.filename);
{$I-}Reset(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to 38 do for J := 0 to 23 do
            begin
              read(fil, ImageByte);
              TVEd[I, J] := ImageByte;
            end;
          CloseFile(fil);
        end; {0 is the Help Context}
      end else
        MessageDlg('This is not a Large file', mtError, [mbRetry], 0);
    end;
  end else if b78.Checked then begin
    OpenDialog1.filter := 'Life Medium files|*.lmf';
    if OpenDialog1.execute then begin
  {Check the file extension to be b78}
    nLifeDir:= ExtractFilePath(OpenDialog1.filename);
      CheckStr := Uppercase(ExtractFileExt(OpenDialog1.filename));
      if (CheckStr = '.LMF') then begin
        bEditor39 := False; bEditor78 := True;
        EditorClearCells;
        AssignFile(fil84, OpenDialog1.filename);
{$I-}Reset(fil84); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              read(fil, ImageByte);
              TV7848Ed[I, J] := ImageByte;
            end;
          CloseFile(fil84);
        end;
      end else
        MessageDlg('This is not a Medium file', mtError, [mbRetry], 0);
    end;
  end;
end;


procedure TEditorForm.LifEdResetBtnClick(Sender: TObject);
begin
  EditorClearCells;
end;

procedure TEditorForm.EditorClearCells;
var I, J: Integer;
begin {}
  if b39.Checked then begin
    bEditor39 := True; bEditor78 := False;
    for I := 0 to 38 do for J := 0 to 23 do TVEd[I, J] := 0;
    DrawGrid2.ColCount := 39;
    DrawGrid2.RowCount := 24;
    DrawGrid2.DefaultRowHeight := 9;
    DrawGrid2.DefaultColWidth := 9;
  end else if b78.Checked then begin
    bEditor39 := False; bEditor78 := True;
    for I := 0 to 77 do for J := 0 to 47 do TV7848Ed[I, J] := 0;
    DrawGrid2.ColCount := 78;
    DrawGrid2.RowCount := 48;
    DrawGrid2.DefaultRowHeight := 4;
    DrawGrid2.DefaultColWidth := 4;
  end;
end;


procedure TEditorForm.LifeEditorBtnClick(Sender: TObject);
var I, J: Integer;
begin {}
  Randomize;
  if b39.Checked then begin
    bEditor39 := True; bEditor78 := False;
    for I := 0 to 38 do for J := 0 to 23 do
      TVEd[I, J] := Trunc(Random(2)); ;
    DrawGrid2.ColCount := 39;
    DrawGrid2.RowCount := 24;
    DrawGrid2.DefaultRowHeight := 9;
    DrawGrid2.DefaultColWidth := 9;
  end else if b78.Checked then begin
    bEditor39 := False; bEditor78 := True;
    for I := 0 to 77 do for J := 0 to 47 do
      TV7848Ed[I, J] := Trunc(Random(2)); ;
    DrawGrid2.ColCount := 78;
    DrawGrid2.RowCount := 48;
    DrawGrid2.DefaultRowHeight := 4;
    DrawGrid2.DefaultColWidth := 4;
  end;
end;


procedure TEditorForm.DrawGrid2DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if bEditor39 {b39.Checked} then begin
    if (TVEd[ACol, ARow] = 1) then
      DrawGrid2.Canvas.Brush.Color := Full {CurrentColor} else
      DrawGrid2.Canvas.Brush.Color := clWhite;
    DrawGrid2.Canvas.FillRect(Rect);
  end else if bEditor78 {b78.Checked} then begin
    if (TV7848Ed[ACol, ARow] = 1) then
      DrawGrid2.Canvas.Brush.Color := Full {CurrentColor} else
      DrawGrid2.Canvas.Brush.Color := clWhite;
    DrawGrid2.Canvas.FillRect(Rect);
  end;
end;

procedure TEditorForm.DrawGrid2SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := True; { foobar:=false;}
  ChangeRow := ARow; ChangeCol := ACol;
  ChangeArray;
end;

{Binary reaction to mouseing a cell}
procedure TEditorForm.ChangeArray;
begin {ChangeRow:=Row;ChangeCol:=Col;}
  begin
    if bEditor39 then
    begin
      if (TVEd[ChangeCol, ChangeRow] = 1) then
        TVEd[ChangeCol, ChangeRow] := 0 else
        TVEd[ChangeCol, ChangeRow] := 1;
    end else if bEditor78 then
    begin
      if (TV7848Ed[ChangeCol, ChangeRow] = 1) then
        TV7848Ed[ChangeCol, ChangeRow] := 0 else
        TV7848Ed[ChangeCol, ChangeRow] := 1;
    end;
  end;
end;







{MUST be able to SAVE as any type from any input...}

procedure TEditorForm.LifEdSaveBtnClick(Sender: TObject);
var
  ImageByte: Byte;
  fil: file of Byte {Array3924  llf};
  fil84: file of Byte {Array7848 lmf};
  fil96: file of Byte {Array15696 lrf};
  Code, i, j: Integer;
  CheckStr, Temptress, Tempted: string;
begin {}
  Tempted := ParamStr(0);
  Temptress := ExtractFilePath(Tempted);
  SaveDialog1.InitialDir := Temptress;
  if (SOB39.Checked and bEditor39) then begin
  {set OpenDialog1 filter to display Life's only}
    SaveDialog1.filter := 'Life Large files|*.llf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'llf';
    SaveDialog1.filename := 'RandomLifeLarge.llf';
    if SaveDialog1.execute then begin
   {Check the file extension to be b39}
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.LLF'))
        then begin
        AssignFile(fil, SaveDialog1.filename);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TVFile );  }
          for I := 0 to 38 do for J := 0 to 23 do
            begin
              ImageByte := TVEd[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end else
        MessageDlg('This is not a Large file', mtError, [mbRetry], 0);
    end;
  end;

  if (SandM78.Checked and bEditor39) then begin
    SaveDialog1.filter := 'Life Medium files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLifeMedium.lmf';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.LMF'))
        then begin
        for I := 0 to 77 do
          for J := 0 to 47 do begin
            TV7848Ed[I, J] := 0;
          end;
        for I := 0 to 38 do
          for J := 0 to 23 do begin
            TV7848Ed[I + 19, J + 12] := TVEd[I, J];
          end;
        AssignFile(fil84, SaveDialog1.filename);
{$I-}rewrite(fil84); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil84, TV7848File );  }
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848Ed[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil84);
        end;
      end else
        MessageDlg('This is not a Medium file', mtError, [mbRetry], 0);
    end;
  end;

  if (SandM78.Checked and bEditor78) then begin
    SaveDialog1.filter := 'Life Medium files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLifeMedium.lmf';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.LMF'))
        then begin
    {bEditor39:=True;bEditor78:=False;}
        AssignFile(fil84, SaveDialog1.filename);
{$I-}rewrite(fil84); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil84, TV7848File );}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848Ed[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil84);
        end;
      end else
        MessageDlg('This is not a Medium file', mtError, [mbRetry], 0);
    end;
  end;

  if (SOS156.Checked and bEditor39) then begin
    SaveDialog1.filter := 'Life Small files|*.lrf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'RandomLifeSmall.lrf';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.LRF'))
        then begin
        if EditorPlacer.ShowModal = mrOk then begin
          val(EditorPlacer.EditorPlacerEditX.Text, XPlacer, Code);
          val(EditorPlacer.EditorPlacerEditY.Text, YPlacer, Code);
          GoFigure(XPlacer, 38, 155, YPlacer, 23, 95);
          XPlacer := 58; YPlacer := 24;
          for I := 0 to 155 do begin for J := 0 to 95 do begin
              TV15696Ed[I, J] := 0; end; end;
          for I := 0 to 38 do
            for J := 0 to 23 do begin
              TV15696Ed[I + XPlacer, J + YPlacer] := TVEd[I, J];
            end;
          AssignFile(fil96, SaveDialog1.filename);
{$I-}rewrite(fil96); {$I+}
          i := IOresult;
          if i <> 0 then EXIT {ReportError( i )} else
          begin
{         write( fil96, TV15696File );}
            for I := 0 to 155 do for J := 0 to 95 do
              begin
                ImageByte := TV15696Ed[I, J];
                write(fil, ImageByte);
              end;
            CloseFile(fil96);
          end;
        end;
      end else
        MessageDlg('This is not a Small file', mtError, [mbRetry], 0);
    end;
  end;

  if (SOS156.Checked and bEditor78) then begin
    SaveDialog1.filter := 'Life Small files|*.lrf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'RandomLifeSmall.lrf';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.LRF'))
        then begin
        if EditorPlacer.ShowModal = mrOk then begin
          val(EditorPlacer.EditorPlacerEditX.Text, XPlacer, Code);
          val(EditorPlacer.EditorPlacerEditY.Text, YPlacer, Code);
          GoFigure(XPlacer, 77, 155, YPlacer, 47, 95);
          XPlacer := 39; YPlacer := 24;

    {bEditor39:=True;bEditor78:=False;}
          for I := 0 to 155 do begin for J := 0 to 95 do begin
              TV15696Ed[I, J] := 0; end; end;
          for I := 0 to 77 do
            for J := 0 to 47 do begin
              TV15696Ed[I + XPlacer, J + YPlacer] := TV7848Ed[I, J];
            end;
          AssignFile(fil96, SaveDialog1.filename);
{$I-}rewrite(fil96); {$I+}
          i := IOresult;
          if i <> 0 then EXIT {ReportError( i )} else
          begin
{         write( fil96, TV15696File );}
            for I := 0 to 155 do for J := 0 to 95 do
              begin
                ImageByte := TV15696Ed[I, J];
                write(fil, ImageByte);
              end;
            CloseFile(fil96);
          end;
        end;
      end else
        MessageDlg('This is not a Small file', mtError, [mbRetry], 0);
    end;
  end;

{(SOPixels.Checked and bEditor78)}
  if (SOPixels.Checked and bEditor39) then begin
    SaveDialog1.filter := 'Life Pixel files|*.bmp';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'bmp';
    SaveDialog1.filename := 'RandomLifePixels.bmp';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.BMP'))
        then begin
        if EditorPlacer.ShowModal = mrOk then begin
          val(EditorPlacer.EditorPlacerEditX.Text, XPlacer, Code);
          val(EditorPlacer.EditorPlacerEditY.Text, YPlacer, Code);
          GoFigure(XPlacer, 38, 781, YPlacer, 23, 481);
          XPlacer := 371;
          YPlacer := 228;

          Bitmap := TBitmap.Create;
          Bitmap.Width := 781;
          Bitmap.Height := 481;
          LifeForm.Image1.Picture.Graphic := Bitmap;
          LifeForm.Image1.Canvas.Pen.Color := RGB(0, 0, 0);
          LifeForm.Image1.Canvas.Brush.Color := RGB(0, 0, 0);
          LifeForm.Image1.Canvas.Brush.Style := bsSolid;
          LifeForm.Image1.Canvas.FillRect(Rect(0, 0, 781, 481));
          for I := 0 to 38 do
            for J := 0 to 23 do begin
              if (TVEd[I, J] = 1) then
                LifeForm.Image1.Canvas.Pixels[I + XPlacer, J +
                  YPlacer]
                  := RGB(255, 255, 255);
            end;
          LifeForm.Image1.Picture.SaveToFile(SaveDialog1.filename)
        end;
      end else
        MessageDlg('This is not a Pixel file', mtError, [mbRetry], 0);
    end;
  end;

  if (SOPixels.Checked and bEditor78) then begin
    SaveDialog1.filter := 'Life Pixel files|*.bmp';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'bmp';
    SaveDialog1.filename := 'RandomLifePixels.bmp';
    if SaveDialog1.execute then begin
      CheckStr := Uppercase(ExtractFileExt(SaveDialog1.filename));
      if ((not (SaveDialog1.filename = '')) and (CheckStr = '.BMP'))
        then begin
        if EditorPlacer.ShowModal = mrOk then begin
          val(EditorPlacer.EditorPlacerEditX.Text, XPlacer, Code);
          val(EditorPlacer.EditorPlacerEditY.Text, YPlacer, Code);
          GoFigure(XPlacer, 77, 781, YPlacer, 47, 481);
          XPlacer := 351; YPlacer := 216;
          Bitmap := TBitmap.Create;
          Bitmap.Width := 781;
          Bitmap.Height := 481;
          LifeForm.Image1.Picture.Graphic := Bitmap;
          LifeForm.Image1.Canvas.Pen.Color := RGB(0, 0, 0);
          LifeForm.Image1.Canvas.Brush.Color := RGB(0, 0, 0);
          LifeForm.Image1.Canvas.Brush.Style := bsSolid;
          LifeForm.Image1.Canvas.FillRect(Rect(0, 0, 781, 481));
          for I := 0 to 77 do
            for J := 0 to 47 do begin
              if (TV7848Ed[I, J] = 1) then
                LifeForm.Image1.Canvas.Pixels[I + XPlacer, J +
                  YPlacer]
                  := RGB(255, 255, 255);
            end;
          LifeForm.Image1.Picture.SaveToFile(SaveDialog1.filename)
        end;
      end else
        MessageDlg('This is not a Pixel file', mtError, [mbRetry], 0);
    end;
  end;

end;
(********************************************************************)
(********************************************************************)

procedure TEditorForm.GoFigure(X0, X1, X2, Y0, Y1, Y2: Integer);
begin
{XPlacer 155 38
 YPlacer  95  23
GoFigure(XPlacer,38,155,YPlacer,23,95);
GoFigure(XPlacer,38,155,YPlacer,23,95);
    XPlacer:=58;              YPlacer :=24;
GoFigure(XPlacer,77,155,YPlacer,47,95);
    XPlacer:=39;              YPlacer :=24;
GoFigure(XPlacer,38,781,YPlacer,23,481);
    XPlacer:=371;              YPlacer :=228;
GoFigure(XPlacer,77,781,YPlacer,47,481);
    XPlacer:=351;              YPlacer :=216;}
end;
(********************************************************************)
(********************************************************************)






end.
