unit Preprint;

interface

uses
  Windows, Messages, SysUtils, Classes, Winprocs,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, GraphicEx,
  Gauges, Printers, jpeg,  Preview,  Paper,
  Spin, ComCtrls, ToolWin, ImgList,  Menus;


Type
  TPrePrintForm = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    PrinterPagePanel: TPanel;
    FontDialog1: TFontDialog;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    LOFontBtn: TToolButton;
    PreviewBtn: TToolButton;
    AbortBtn: TToolButton;
    ExportBtn: TToolButton;
    HelpBtn: TToolButton;
    AboutBtn: TToolButton;
    CloseBtn: TToolButton;
    PrinterSizeShape: TImage;
    PrePrintIList: TImageList;
    TitlePanel: TPanel;
    AuthorPanel: TPanel;
    PrintStyleBtn: TToolButton;
    PSPopup: TPopupMenu;
    Portrait1: TMenuItem;
    Landscape1: TMenuItem;
    ImageSizeBtn: TToolButton;
    ImageSizePopup: TPopupMenu;
    ISOriginal1: TMenuItem;
    IS2Times1: TMenuItem;
    IS3Times1: TMenuItem;
    IS4Times1: TMenuItem;
    IS5Times1: TMenuItem;
    IS6Times1: TMenuItem;
    ISMaxSize1: TMenuItem;
    Label2: TLabel;
    WidthSpinEdit: TSpinEdit;
    Label3: TLabel;
    HeightSpinEdit: TSpinEdit;
    PageWidthLabel: TLabel;
    PageHeightLabel: TLabel;
    ImageXLocation: TSpinEdit;
    ImageYLocation: TSpinEdit;
    TitleEdit: TMemo;
    AuthorEdit: TMemo;
    FontMenu: TPopupMenu;
    TitleFont1: TMenuItem;
    AuthorFont1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
procedure SetupForm(WhatFile:String;Switch:Integer);
    procedure PreviewBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
{}
    procedure LOFontBtnClick(Sender: TObject);
procedure CheckText;
    procedure Portrait1Click(Sender: TObject);
    procedure Landscape1Click(Sender: TObject);
procedure CheckStyle;
    procedure ISOriginal1Click(Sender: TObject);
    procedure IS2Times1Click(Sender: TObject);
    procedure IS3Times1Click(Sender: TObject);
    procedure IS4Times1Click(Sender: TObject);
    procedure IS5Times1Click(Sender: TObject);
    procedure IS6Times1Click(Sender: TObject);
    procedure ISMaxSize1Click(Sender: TObject);
procedure AllCheckedOff;
procedure PrintSizeRG;
procedure CheckImage;
    procedure CloseBtnClick(Sender: TObject);
    procedure TitleEditChange(Sender: TObject);
    procedure AuthorEditChange(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure PrinterSizeShapeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PrinterSizeShapeMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PrinterSizeShapeMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageXLocationChange(Sender: TObject);
    procedure ImageYLocationChange(Sender: TObject);
    procedure TitleFont1Click(Sender: TObject);
    procedure AuthorFont1Click(Sender: TObject);
  private
     
    procedure ChangePosition(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
{    procedure ChangeSize(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);  }

    Procedure Draw;
  public
     
  end;

var
  PrePrintForm: TPrePrintForm;
  Preview1 : TPreview;

implementation

uses Globals, PicAbout, PicView, GCfrm;
{$R *.DFM}
(*const
  sizeBorder = 2;
  sc_SizeLeft              = $F001;  { these are the variations on the }
  sc_SizeRight           = $F002;  { SC_SIZE value }
  sc_SizeTop             = $F003;
  sc_SizeTopLeft       = $F004;
  sc_SizeTopRight     = $F005;
  sc_SizeBottom         = $F006;
  sc_SizeBottomLeft   = $F007;
  sc_SizeBottomRight  = $F008;
  sc_DragMove          = $F012;
 SC_SIZE = $F012;*)

var
  MovingOn, ImageOk: Boolean;
  PrintSizeRGItemIndex,
  {Bitmap}bht ,bwt, {Paper}pwt ,pht,
  {Bitmap at Print scale}
  PrintImageWidth, PrintImageHeight: Integer;
  {TextWidth, TextHeight: Integer;}
  TitleStr, AuthorStr: string;
  AuthorEditFont,TitleEditFont:TFont;
  PrepTarget:TBitmap;
(***********************************************************)
{procedure TPrePrintForm.ChangeSize(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture;
   (Sender as TWinControl).Perform(WM_SYSCOMMAND, SC_MOVE -8, 0);
end;}

procedure TPrePrintForm.ChangePosition(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture;
   (Sender as TWinControl).Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
end;



(***********************************************************)
procedure TPrePrintForm.FormCreate(Sender: TObject);
{var IniFile: TIniFile;
    n: integer;}
begin
// Restore control-values from inifile
{  IniFile:= TIniFile.Create('ctrls.ini');
  for n:=0 to ControlCount - 1 do
  begin
    Controls[n].Top:= IniFile.ReadInteger(Controls[n].Name, 'Top', Controls[n].Top);
    Controls[n].Left:= IniFile.ReadInteger(Controls[n].Name, 'Left', Controls[n].Left);
    Controls[n].Height:= IniFile.ReadInteger(Controls[n].Name, 'Height', Controls[n].Height);
    Controls[n].Width:= IniFile.ReadInteger(Controls[n].Name, 'Width', Controls[n].Width);
  end;
  IniFile.Free;
}
  top := PrePrintFormY;
  left := PrePrintFormX;
  PrepTarget := TBitmap.Create;
{  TextOK := False;}
  ImageOk := False;
  MovingOn:=False;
  PrintSizeRGItemIndex:=0;
    TitlePanel.OnMouseDown:= ChangePosition;
    AuthorPanel.OnMouseDown:= ChangePosition;
    AuthorEditFont:=AuthorEdit.Font;
    TitleEditFont:=TitleEdit.Font;
{  Button1.OnMouseDown:= ChangeSize;}
  Preview1 := TPreview.Create( Self );
{  Preview1.Buttons:=  '[btPrint,btPageSetup,btOpen,btSave,btLanguage]';}
{  Preview1.Buttons.btHelp:=False;
  Preview1.Buttons.btAbout:=False; }
  Preview1.Paper.PrinterSetup.PaperFormat:=pfLETTER;
  Preview1.Paper.PrinterSetup.PaperHeight_MM:=279;
  Preview1.Paper.PrinterSetup.PaperWidth_MM:=216;
  Preview1.Paper.PrinterSetup.PixelsPerMM_byX:=11.1111111111111;
  Preview1.Paper.PrinterSetup.PixelsPerMM_byY:=11.1541218637993;
  Preview1.Paper.Units := unTenthMillimeters;
{  Draw;}
end;

procedure TPrePrintForm.FormDestroy(Sender: TObject);
begin
 PrepTarget.free;
 Preview1.Free;
end;

procedure TPrePrintForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{var IniFile: TIniFile;
    n: integer;}
begin
// Save control-values to inifile
{  IniFile:= TIniFile.Create('ctrls.ini');
  for n:=0 to ControlCount - 1 do
  begin
    IniFile.WriteInteger(Controls[n].Name, 'Top', Controls[n].Top);
    IniFile.WriteInteger(Controls[n].Name, 'Left', Controls[n].Left);
    IniFile.WriteInteger(Controls[n].Name, 'Height', Controls[n].Height);
    IniFile.WriteInteger(Controls[n].Name, 'Width', Controls[n].Width);
  end;
  IniFile.Free;
}
  PrePrintFormY := PrePrintForm.top;
  PrePrintFormX := PrePrintForm.left;
end;
procedure TPrePrintForm.CloseBtnClick(Sender: TObject);
begin
Close
end;
  procedure TPrePrintForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(8800);
end;

procedure TPrePrintForm.AboutBtnClick(Sender: TObject);
begin
  PicAboutBox.Init(5);
  PicAboutBox.ShowModal;
end;

procedure TPrePrintForm.SetupForm(WhatFile:String;Switch:Integer);
begin
{Get the Image filename, X,Y size;Text}
          PrepTarget.PixelFormat := pf24Bit;
          PrinterSizeShape.Stretch:=False;
          PrinterSizeShape.Autosize:=false;
If Switch=1 then
begin
          PrepTarget.Width := PicViewBitmap.Image1.Picture.Graphic.Width;
          PrepTarget.Height := PicViewBitmap.Image1.Picture.Graphic.Height;
          PrepTarget.Canvas.Draw(0, 0, PicViewBitmap.Image1.Picture.Graphic);
end else
begin
          PrepTarget.Width := GCForm.Image1.Picture.Graphic.Width;
          PrepTarget.Height := GCForm.Image1.Picture.Graphic.Height;
          PrepTarget.Canvas.Draw(0, 0, GCForm.Image1.Picture.Graphic);
end;
        PrinterSizeShape.Picture.Bitmap.PixelFormat := pf24bit;
        PrinterSizeShape.Picture.Graphic := PrepTarget;
        PrinterSizeShape.Stretch:=True;
        bwt:=PrepTarget.Width;
        bht:=PrepTarget.Height;
HeightSpinEdit.Value := bht;
WidthSpinEdit.Value := bwt;
  pwt := Printer.PageWidth;
  PageWidthLabel.Caption := inttostr(pwt);
  pht := Printer.PageHeight;
  PageHeightLabel.Caption := inttostr(pht);
   {Get the printer orientation: portrait or landscape}
  if (Printer.Orientation = poPortrait) then
    Portrait1.Checked:=True
  else      { Printer.Orientation := poLandscape;}
    Landscape1.Checked:=True;
ISOriginal1.Checked:=True;
PrintSizeRGItemIndex:=0;
  CheckStyle;{ALL}
End;

(***********************************************************)
Procedure TPrePrintForm.Draw;
Begin
  With Preview1.Paper Do
    Begin
      Clear;
      SetPage( 1 );
      SetBrush( exClear, clWhite, clWhite{clBlack}, Nil, 0 );
      SetFont(
       TitleEditFont.Name,{'Times New Roman',}
       TitleEditFont.Size,{26,}
        TitleEditFont.Style,  {[fsBold],}
        TitleEditFont.Color);{ clBlack );}
TextOut( Round(TitlePanel.Left*13.77){400},
         Round(TitlePanel.Top*13.77) {1700 + (I*100)},
         TitleEdit.Text );
{      SetBrush( exClear, clRed, clAqua, Nil, 0 );}

      StretchDraw( Rect(
      Round(PrinterSizeShape.Left*13.77),{200,}
      Round(PrinterSizeShape.Top*13.77),{1000,}
      Round((PrinterSizeShape.Left*13.77)+
                PrintImageWidth),{PrepTarget.Width}{600,}
      Round((PrinterSizeShape.Top*13.77)+
            PrintImageHeight)){PrepTarget.Height}{1400),}
              ,PrepTarget{ Image1.Picture.Graphic} );
{      For I := 0 To Memo1.Lines.Count-1 Do
        Begin
          TextOut( 400, 1700 + (I*100), Memo1.Lines.Strings[I] );
        End;}
      SetFont(
       AuthorEditFont.Name,
       AuthorEditFont.Size,
        AuthorEditFont.Style,
        AuthorEditFont.Color);
TextOut( Round(AuthorPanel.Left*13.77){400},
         Round(AuthorPanel.Top*13.77) {1700 + (I*100)},
         AuthorEdit.Text );
    End;
End;


procedure TPrePrintForm.PreviewBtnClick(Sender: TObject);
begin
  Draw;
  Preview1.Preview;  {    Preview1.Print;}
end;
(***********************************************************)

procedure TPrePrintForm.ExportBtnClick(Sender: TObject);
begin
  Preview1.Paper.CompressionQuality := 100;
  Preview1.Paper.ExportPageToFile( 'Page1.bmp', 1, ifBitmap );
{  Preview1.Paper.ExportPageToFile( 'Page1.jpg', 1, ifJPEG );
  Preview1.Paper.ExportPageToFile( 'Page1.emf', 1, ifMetaFile );}
end;
(*************************************************************)
(*************************************************************)


(*************************************************************)
(*************************************************************)
procedure TPrePrintForm.LOFontBtnClick(Sender: TObject);
begin
   FontDialog1.Font:=AuthorEditFont;
  if FontDialog1.Execute then
  begin
    TitleEditFont := FontDialog1.Font;
    AuthorEditFont:= FontDialog1.Font;
  end;
  CheckText;
end;
procedure TPrePrintForm.TitleEditChange(Sender: TObject);
begin
  CheckText;
end;
procedure TPrePrintForm.TitleFont1Click(Sender: TObject);
begin
   FontDialog1.Font:=TitleEditFont;
  if FontDialog1.Execute then
  begin
    TitleEditFont := FontDialog1.Font;
  end;
  CheckText;
end;

procedure TPrePrintForm.AuthorFont1Click(Sender: TObject);
begin
   FontDialog1.Font:=AuthorEditFont;
  if FontDialog1.Execute then
  begin
    AuthorEditFont:= FontDialog1.Font;
  end;
  CheckText;
end;

procedure TPrePrintForm.AuthorEditChange(Sender: TObject);
begin
  CheckText;
end;

procedure TPrePrintForm.CheckText;
var
  TitleTextSize, AuthorTextSize: TSize;
begin     {  PrinterSizeShape }
  TitleStr := TitleEdit.Text;
  AuthorStr := AuthorEdit.Text;
PrepTarget.Canvas.Font:=TitleEditFont;
  TitleTextSize := PrepTarget.Canvas.TextExtent(TitleEdit.Lines[0]{TitleStr});
PrepTarget.Canvas.Font:=AuthorEditFont;
  AuthorTextSize := PrepTarget.Canvas.TextExtent(AuthorEdit.Lines[0]{AuthorStr});
TitlePanel.Width:=  Round(TitleTextSize.cx/13.77);
AuthorPanel.Width:=  Round(AuthorTextSize.cx/13.77);
TitlePanel.Height:=  Round((TitleEdit.Lines.Count*TitleTextSize.cy)/13.77);
AuthorPanel.Height:=  Round((AuthorEdit.Lines.Count*AuthorTextSize.cy)/13.77);

if (pwt < TitleTextSize.cx)then TitlePanel.Color:=clRed
else TitlePanel.Color:=clBtnFace;
if (pwt < AuthorTextSize.cx)then AuthorPanel.Color:=clRed
else AuthorPanel.Color:=clBtnFace;
end;


procedure TPrePrintForm.Portrait1Click(Sender: TObject);
begin
Landscape1.Checked:=False;
CheckStyle;
end;
procedure TPrePrintForm.Landscape1Click(Sender: TObject);
begin
Portrait1.Checked:=False;
CheckStyle;
end;
procedure TPrePrintForm.CheckStyle;
begin
  if Portrait1.Checked then
  begin
    Printer.Orientation := poPortrait;
    PrinterPagePanel.Top := 8;
    PrinterPagePanel.Left := 8;
    PrinterPagePanel.Height :=
     round(Printer.PageHeight / 13.77){240};
    PrinterPagePanel.Width := round(Printer.PageWidth / 13.77) {185};
(*          LegendPanel.Top := 8;
          LegendPanel.Left := ((PrinterPagePanel.Width {185} - 166)
        LegendPanel.Top := ((PrinterPagePanel.Height {185} - 40));
        LegendPanel.Left := ((PrinterPagePanel.Width {185} - 166) div
*)
  end else
  begin
    Printer.Orientation := poLandscape;
    PrinterPagePanel.Top := 8;
    PrinterPagePanel.Left := 8;
    PrinterPagePanel.Height := round(Printer.PageHeight / 13.77);
    PrinterPagePanel.Width := round(Printer.PageWidth / 13.77);
(*          LegendPanel.Top := 8;
          LegendPanel.Left := ((PrinterPagePanel.Width {185} - 166)
            div 2);
        LegendPanel.Top := ((PrinterPagePanel.Height {185} - 40));
        LegendPanel.Left := ((PrinterPagePanel.Width {185} - 166) div
          2);*)
  end;
  PageWidthLabel.Caption :=
    inttostr(Printer.PageWidth);
  PageHeightLabel.Caption :=
    inttostr(Printer.PageHeight);
  pwt := (Printer.PageWidth);
  pht := (Printer.PageHeight);
  CheckImage;
end;

procedure TPrePrintForm.ISOriginal1Click(Sender: TObject);
begin
AllCheckedOff;
ISOriginal1.Checked:=True;
PrintSizeRG;
end;
procedure TPrePrintForm.IS2Times1Click(Sender: TObject);
begin
AllCheckedOff;
IS2Times1.Checked:=True;
PrintSizeRG;
end;

procedure TPrePrintForm.IS3Times1Click(Sender: TObject);
begin
AllCheckedOff;
IS3Times1.Checked:=True;
PrintSizeRG;
end;

procedure TPrePrintForm.IS4Times1Click(Sender: TObject);
begin
AllCheckedOff;
IS4Times1.Checked:=True;
PrintSizeRG;
end;

procedure TPrePrintForm.IS5Times1Click(Sender: TObject);
begin
AllCheckedOff;
IS5Times1.Checked:=True;
PrintSizeRG;
end;

procedure TPrePrintForm.IS6Times1Click(Sender: TObject);
begin
AllCheckedOff;
IS6Times1.Checked:=True;
PrintSizeRG;
end;

procedure TPrePrintForm.ISMaxSize1Click(Sender: TObject);
begin
AllCheckedOff;
ISMaxSize1.Checked:=True;
PrintSizeRG;
end;
procedure TPrePrintForm.AllCheckedOff;
Begin
ISOriginal1.Checked:=False;
IS2Times1.Checked:=False;
IS3Times1.Checked:=False;
IS4Times1.Checked:=False;
IS5Times1.Checked:=False;
IS6Times1.Checked:=False;
ISMaxSize1.Checked:=False;
End;
procedure TPrePrintForm.PrintSizeRG;
begin
If ISOriginal1.Checked then PrintSizeRGItemIndex:=0 else
If IS2Times1.Checked then PrintSizeRGItemIndex:=1 else
If IS3Times1.Checked then PrintSizeRGItemIndex:=2 else
If IS4Times1.Checked then PrintSizeRGItemIndex:=3 else
If IS5Times1.Checked then PrintSizeRGItemIndex:=4 else
If IS6Times1.Checked then PrintSizeRGItemIndex:=5 else
PrintSizeRGItemIndex:=6;{ISMaxSize1}
  CheckImage;
end;


procedure TPrePrintForm.CheckImage;
begin
  ImageOk := False;
  case PrintSizeRGItemIndex of
    0: begin
          ImageOk := True;
        if ((pht > bht) and (pwt > bwt)) then
        begin
          HeightSpinEdit.Value := bht;
          WidthSpinEdit.Value := bwt;
{          PrintSizeRG.Color := clBtnFace;}
          PrintImageWidth := bwt;
          PrintImageHeight := bht;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end {else PrintSizeRG.Color := clWindow};
      end;
    1:
      begin
        if ((pht > (bht * 2)) and (pwt > (bwt * 2))) then
        begin
          ImageOk := True;
          HeightSpinEdit.Value := bht * 2;
          WidthSpinEdit.Value := bwt * 2;
{          PrintSizeRG.Color := clBtnFace;}
          PrintImageWidth := bwt * 2;
          PrintImageHeight := bht * 2;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end {else PrintSizeRG.Color := clWindow};
      end;
    2:
      begin
        if ((pht > (bht * 3)) and (pwt > (bwt * 3))) then
        begin
          ImageOk := True;
          HeightSpinEdit.Value := bht * 3;
          WidthSpinEdit.Value := bwt * 3;
{          PrintSizeRG.Color := clBtnFace;  }
          PrintImageWidth := bwt * 3;
          PrintImageHeight := bht * 3;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end{ else PrintSizeRG.Color := clWindow};
      end;
    3:
      begin
        if ((pht > (bht * 4)) and (pwt > (bwt * 4))) then
        begin
          ImageOk := True;
          HeightSpinEdit.Value := bht * 4;
          WidthSpinEdit.Value := bwt * 4;
{          PrintSizeRG.Color := clBtnFace; }
          PrintImageWidth := bwt * 4;
          PrintImageHeight := bht * 4;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end {else PrintSizeRG.Color := clWindow};
      end;
    4:
      begin
        if ((pht > (bht * 5)) and (pwt > (bwt * 5))) then
        begin
          ImageOk := True;
          HeightSpinEdit.Value := bht * 5;
          WidthSpinEdit.Value := bwt * 5;
{          PrintSizeRG.Color := clBtnFace;}
          PrintImageWidth := bwt * 5;
          PrintImageHeight := bht * 5;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end{ else PrintSizeRG.Color := clWindow};
      end;
    5:
      begin
        if ((pht > (bht * 6)) and (pwt > (bwt * 6))) then
        begin
          ImageOk := True;
          HeightSpinEdit.Value := bht * 6;
          WidthSpinEdit.Value := bwt * 6;
{          PrintSizeRG.Color := clBtnFace;}
          PrintImageWidth := bwt * 6;
          PrintImageHeight := bht * 6;
          PrinterSizeShape.Height := round(PrintImageHeight / 13.77);
          PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
          PrinterSizeShape.Top := ((PrinterPagePanel.Height
            - PrinterSizeShape.Height) div 2);
          PrinterSizeShape.Left := ((PrinterPagePanel.Width
            - PrinterSizeShape.Width) div 2);
        end {else PrintSizeRG.Color := clWindow};
      end;
    6:
      begin {Compute Max Size}
        if ((pht > bwt) and (pwt > bht)) then
        begin
          if ((pwt div bwt) > (pht div bht)) then
          begin {height controls}
            HeightSpinEdit.Value := bht * (pht div bht);
            WidthSpinEdit.Value := bwt * (pht div bht);
            ImageOk := True;
{            PrintSizeRG.Color := clBtnFace;   }
            PrintImageWidth := bwt * (pht div bht);
            PrintImageHeight := bht * (pht div bht);
            PrinterSizeShape.Height := round(PrintImageHeight /
              13.77);
            PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
            PrinterSizeShape.Top := ((PrinterPagePanel.Height
              - PrinterSizeShape.Height) div 2);
            PrinterSizeShape.Left := ((PrinterPagePanel.Width
              - PrinterSizeShape.Width) div 2);
          end else
          begin
            HeightSpinEdit.Value := bht * (pwt div bwt);
            WidthSpinEdit.Value := bwt * (pwt div bwt);
            ImageOk := True;
{            PrintSizeRG.Color := clBtnFace; }
            PrintImageWidth := bwt * (pwt div bwt);
            PrintImageHeight := bht * (pwt div bwt);
            PrinterSizeShape.Height := round(PrintImageHeight /
              13.77);
            PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
            PrinterSizeShape.Top := ((PrinterPagePanel.Height
              - PrinterSizeShape.Height) div 2);
            PrinterSizeShape.Left := ((PrinterPagePanel.Width
              - PrinterSizeShape.Width) div 2);
          end;
        end else {Reduce Image to fit Paper}
        begin
          if ((pwt div bwt) > (pht div bht)) then
          begin {height controls}
            HeightSpinEdit.Value := round(bht * ((pht - 100) / bht));
            WidthSpinEdit.Value := round(bwt * ((pht - 100) / bht));
            ImageOk := True;
{            PrintSizeRG.Color := clBtnFace;}
            PrintImageWidth := round(bwt * ((pht - 100) / bht));
            PrintImageHeight := round(bht * ((pht - 100) / bht));
            PrinterSizeShape.Height := round(PrintImageHeight /
              13.77);
            PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
            PrinterSizeShape.Top := ((PrinterPagePanel.Height
              - PrinterSizeShape.Height) div 2);
            PrinterSizeShape.Left := ((PrinterPagePanel.Width
              - PrinterSizeShape.Width) div 2);
          end else
          begin
            HeightSpinEdit.Value := round(bht * ((pwt - 100) / bwt));
            WidthSpinEdit.Value := round(bwt * ((pwt - 100) / bwt));
            ImageOk := True;
{            PrintSizeRG.Color := clBtnFace;}
            PrintImageWidth := round(bwt * ((pwt - 100) / bwt));
            PrintImageHeight := round(bht * ((pwt - 100) / bwt));
            PrinterSizeShape.Height := round(PrintImageHeight /
              13.77);
            PrinterSizeShape.Width := round(PrintImageWidth / 13.77);
            PrinterSizeShape.Top := ((PrinterPagePanel.Height
              - PrinterSizeShape.Height) div 2);
            PrinterSizeShape.Left := ((PrinterPagePanel.Width
              - PrinterSizeShape.Width) div 2);
          end;
        end;
      end; {6}
  end; {case}
  If (not  ImageOk) then Panel1.Color:=clRed
  else
  begin
    Panel1.Color:=clBtnFace;
    Draw;
  end;
end;
(***********************************************************)


(***********************************************************)
procedure TPrePrintForm.PrinterSizeShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
      MovePt.X := X+(X-PrinterSizeShape.Left);
      MovePt.Y :=Y+(Y-PrinterSizeShape.Top);
      MovingOn:=True;
end;

procedure TPrePrintForm.PrinterSizeShapeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  If MovingOn then
  begin
      if (((X-MovePt.X) > 0)
          and ((X-MovePt.X) < (PrinterPagePanel.Width - PrinterSizeShape.Width))) then
        PrinterSizeShape.Left:=MovePt.X;
      if (((Y-MovePt.Y) > 0)
         and ((Y-MovePt.Y) < (PrinterPagePanel.Height - PrinterSizeShape.Height))) then
        PrinterSizeShape.Top:=MovePt.Y;
   end;
end;

procedure TPrePrintForm.PrinterSizeShapeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MovingOn:=False;
  ImageXLocation.Value:=PrinterSizeShape.Left;
  ImageYLocation.Value:=PrinterSizeShape.Top;
end;

procedure TPrePrintForm.ImageXLocationChange(Sender: TObject);
begin
 If ((ImageXLocation.Value>0) and
     (ImageXLocation.Value <
     (PrinterPagePanel.Width - PrinterSizeShape.Width))) then
  PrinterSizeShape.Left:=ImageXLocation.Value;
end;

procedure TPrePrintForm.ImageYLocationChange(Sender: TObject);
begin
 If ((ImageYLocation.Value>0) and
     (ImageYLocation.Value <
     (PrinterPagePanel.Height - PrinterSizeShape.Height))) then
  PrinterSizeShape.Top:=ImageYLocation.Value;
end;
(***********************************************************)


(***********************************************************)


end.

