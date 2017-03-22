unit CCATextureEditorFrm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Samples.Spin,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.PNGImage,
  GLFileTGA,
//  GR32_Image,
  ShellApi,
  GLVectorGeometry,
  GLGraphics, GR32_Image;

type
  TATextureEditorForm = class(TForm)
    ClonePopupMenu: TPopupMenu;
    CloneSize1: TMenuItem;
    CloneBoxorCircleMenu: TMenuItem;
    CloneXorShapeMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    ColorDialog1: TColorDialog;
    PreviewImage: TImage;
    SaveDialog1: TSaveDialog;
    PaintPopupMenu: TPopupMenu;
    PaintResize: TMenuItem;
    PaintCrop: TMenuItem;
    AguiPanel: TPanel;
    Image321: TImage32;
    IOriginalBottomPanel: TPanel;
    openmaskBtn: TSpeedButton;
    UndoBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    MaskFilenameEdit: TEdit;
    PaintCloneRG: TRadioGroup;
    SaveAlphaRG: TRadioGroup;
    GroupBox1: TGroupBox;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    IsWhiteCB: TCheckBox;
    PaintColorPanel: TPanel;
    IsColorCB: TCheckBox;
    DotBoxRG: TRadioGroup;
    GroupBox2: TGroupBox;
    intensTrackBar: TTrackBar;
    trasTrackBar: TTrackBar;
    WandTrackBar: TTrackBar;
    WandCoB: TComboBox;
    ClonePanel: TPanel;
    TLCloneBox: TCheckBox;
    SmoothAllBtn: TSpeedButton;
    SmoothRG: TRadioGroup;
    procedure openmaskBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IsWhiteCBClick(Sender: TObject);
    procedure PreviewImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PreviewImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloneSize1Click(Sender: TObject);
    procedure CloneBoxorCircleMenuClick(Sender: TObject);
    procedure CloneXorShapeMenuClick(Sender: TObject);
    procedure PaintColorPanelClick(Sender: TObject);

    procedure PaintCloneRGClick(Sender: TObject);
    procedure SaveAlphaRGClick(Sender: TObject);
    procedure GenerateAlpha(transparentColor: TColor; fromIntensity: Boolean;
      doSqrt: Boolean);
    procedure ExitBtnClick(Sender: TObject);
    procedure SmoothAllBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintResizeClick(Sender: TObject);
    procedure PaintCropClick(Sender: TObject);

  private
    Cloning, CloningOn, CloneProcessing, CloneMoving, CloneBoxorCircle,
      CloneXorShape, WandProcessing, Mousing, Processing: Boolean;
    CloneMouseX, CloneMouseY, CloneXOffset, CloneYOffset, CloneSize: Integer;
    IsWhiteColor: TColor;
    TipMaskMaker: TBitmap;
  public
    FirstTime: Boolean;
  end;

var
  ATextureEditorForm: TATextureEditorForm;

implementation

uses
  // Options,
  CCMaskfillunit,
  CCEditorFrm;

{$R *.DFM}

procedure TATextureEditorForm.FormCreate(Sender: TObject);
begin
  FirstTime := True;
end;

procedure TATextureEditorForm.FormShow(Sender: TObject);
begin
  If FirstTime then
  begin
    FirstTime := False;
    // top := TipMaskMakerFormY;
    // left := TipMaskMakerFormX;
    TipMaskMaker := TBitmap.create;
    TipMaskMaker.Pixelformat := pf24bit;
    WandProcessing := False;
    Mousing := False;
    Processing := False;
    CloningOn := False;
    Cloning := False;
    CloneProcessing := False;
    CloneMoving := False;
    CloneSize := 10;
    CloneXOffset := 60;
    CloneYOffset := 60;
    CloneMouseX := 10;
    CloneMouseY := 10;
    PreviewImage.Canvas.Brush.Style := bsClear; // bsSolid
    PreviewImage.Canvas.Pen.Color := clBlack;
    CloneBoxorCircle := True;
    CloneXorShape := True;
  end;
end;

procedure TATextureEditorForm.FormDestroy(Sender: TObject);
begin
  TipMaskMaker.free;
end;

procedure TATextureEditorForm.openmaskBtnClick(Sender: TObject);
var
  TempImage: TImage;
  JPGImage: TJPEGImage;
  PNG: TPNGObject;
  Bmp1: TBitmap;
  f: string;
begin
  OpenDialog1.Filter := 'Cloud Texture (*.bmp)|*.bmp';
  OpenDialog1.DefaultExt := 'bmp';
  OpenDialog1.Filename := '*.bmp';
  OpenDialog1.InitialDir := ImagePath; // ExtractFilePath(Application.Exename);
  if OpenDialog1.Execute then
  begin
    MaskFilenameEdit.Text := OpenDialog1.Filename;
    // ProjectPath:=ExtractFilePath(OpenDialog1.FileName);
    Application.ProcessMessages;
    f := OpenDialog1.Filename;
    Bmp1 := TBitmap.create;
    try
      if (lowercase(extractfileext(f)) = '.png') then
      begin
        PNG := TPNGObject.create;
        // In case something goes wrong, free both PNG and Bitmap
        try
          PNG.LoadFromFile(f);
          Bmp1.Assign(PNG); // Convert data into bitmap
        finally
          PNG.free;
        end;
      end
      else
      begin
        TempImage := TImage.create(self);
        try
          TempImage.Picture.LoadFromFile(f);

          Bmp1.Pixelformat := pf24bit; // set format first before
          Bmp1.Height := TempImage.Picture.Height;
          // dimension can save some resources
          Bmp1.Width := TempImage.Picture.Width;

          if TempImage.Picture.Graphic is TJPEGImage then
          begin
            JPGImage := TJPEGImage.create;
            try
              JPGImage.LoadFromFile(f);
              Bmp1.Canvas.Draw(0, 0, JPGImage);
            finally
              JPGImage.free;
            end;
          end
          else // Must have been a bmp file
            Bmp1.Canvas.Draw(0, 0, TempImage.Picture.Graphic);
          // TempImage is a BMP
        finally
          TempImage.free;
        end;
      end;
      PreviewImage.Picture.bitmap.Assign(Bmp1);
    finally
      Bmp1.free;
    end;
    // IsWhiteCB
    PreviewImage.Picture.bitmap.Pixelformat := pf24bit;
    PreviewImage.Picture.bitmap.Canvas.Brush.Color := clwhite;
    PreviewImage.Picture.bitmap.Canvas.Pen.Color := clwhite;
    PreviewImage.Picture.bitmap.Canvas.Brush.Style := bsSolid;
    TipMaskMaker.Assign(PreviewImage.Picture.bitmap);
    Image321.bitmap.Assign(PreviewImage.Picture.bitmap);
  end;
end;

procedure TATextureEditorForm.UndoBtnClick(Sender: TObject);
begin
  If Processing then
    exit;
  Processing := True;
  ATextureEditorForm.cursor := crhourglass;
  PreviewImage.Picture.bitmap.Assign(TipMaskMaker);
  Processing := False;
  ATextureEditorForm.cursor := crdefault;
end;

procedure TATextureEditorForm.HelpBtnClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(ExtractFilePath(ParamStr(0)) +
    'TextureEditor.htm'), '', '', SW_SHOW);
end;

/// //////////////////////////
procedure TATextureEditorForm.PaintResizeClick(Sender: TObject);
var
  bitmap: TBitmap;
begin
  TipMaskMaker.Assign(PreviewImage.Picture.bitmap);
  bitmap := TBitmap.create;
  bitmap.Width := 128;
  bitmap.Height := 128;
  bitmap.Pixelformat := pf32bit;
  bitmap.Canvas.StretchDraw(Rect(0, 0, 128, 128), PreviewImage.Picture.bitmap);
  PreviewImage.Picture.bitmap.Width := 128;
  PreviewImage.Picture.bitmap.Height := 128;
  PreviewImage.Picture.bitmap.Assign(bitmap);
  bitmap.free;
end;

procedure TATextureEditorForm.PaintCropClick(Sender: TObject);
begin
  // Resize to 128,
  // Crop 128 Square from a 256
  If PreviewImage.Picture.bitmap.Width = 256 then
  begin
    // MouseDown: Draw a 128x128 box for Crop size location
    // MouseUp: Remove Box, Crop to location,Resize to 128x128:the crop
  end;
end;
/// //////////////////////////

procedure TATextureEditorForm.IsWhiteCBClick(Sender: TObject);
begin
  If IsWhiteCB.checked then
    Shape1.Brush.Color := clwhite
  else
    Shape1.Brush.Color := clBlack;
end;

procedure TATextureEditorForm.PaintColorPanelClick(Sender: TObject);
begin
  ColorDialog1.Color := PaintColorPanel.Color;
  if ColorDialog1.Execute then
  Begin
    PaintColorPanel.Color := ColorDialog1.Color;
  End;
end;

procedure TATextureEditorForm.PreviewImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// var ChangeColor:TColor;
begin
  // Cloning, CloningOn, CloneProcessing,
  If CloningOn then
  begin
    TipMaskMaker.Assign(PreviewImage.Picture.bitmap); // allows 1 Undo
    if (Button = mbRight) then
      CloneMoving := True
    else if (Button = mbLeft) then
      Cloning := True;
    CloneMouseX := X;
    CloneMouseY := Y;
  end
  else
  begin
    If Mousing or Processing then
      exit;
    If Button = mbLeft then // mbRight is Wand
    begin
      Mousing := True;
      Processing := True;
      TipMaskMaker.Assign(PreviewImage.Picture.bitmap); // allows 1 Undo
      If IsWhiteCB.checked then
        IsWhiteColor := clwhite
      else If IsColorCB.checked then
        IsWhiteColor := PaintColorPanel.Color
      else
        IsWhiteColor := clBlack;
      PreviewImage.Picture.bitmap.Canvas.Brush.Color := IsWhiteColor;
      PreviewImage.Picture.bitmap.Canvas.Pen.Color := IsWhiteColor;
    end
    else If Button = mbRight then
      WandProcessing := True
    else
      WandProcessing := False;
  end;
end;

procedure TATextureEditorForm.PreviewImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  MaxPixelCount = 65536;
type
  pRGBArray = ^TRGBArray; // Use SysUtils.pByteArray for 8-bit color
  TRGBArray = ARRAY [0 .. MaxPixelCount - 1] OF TRGBTriple;
var
  p, P2: pRGBArray;
  BGx, BGy: Integer;
begin
  If CloneProcessing then
    exit;
  If CloningOn then
  begin
    PreviewImage.Canvas.Pen.Mode := pmNotXor; // use XOR mode to draw/erase
    If CloneBoxorCircle then
      PreviewImage.Canvas.Ellipse(CloneMouseX + CloneXOffset - CloneSize,
        CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
        CloneSize, CloneMouseY + CloneYOffset + CloneSize)
    else
      PreviewImage.Canvas.Rectangle(CloneMouseX + CloneXOffset - CloneSize,
        CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
        CloneSize, CloneMouseY + CloneYOffset + CloneSize);
  end;
  If CloningOn and Cloning then
  begin
    CloneProcessing := True;
    // Copy the CloneBox area TO IMAGE
    If ((X + CloneXOffset - CloneSize > 0) and (X > 0) and
      (X < PreviewImage.Width - 1) and (X + CloneXOffset + CloneSize <
      PreviewImage.Width - 1) and (Y + CloneYOffset - CloneSize > 0) and
      (Y > 0) and (Y < PreviewImage.Height - 1) and
      (Y + CloneYOffset + CloneSize < PreviewImage.Height - 1)) then
    begin
      If CloneBoxorCircle then
        for BGy := Y - CloneSize to Y + CloneSize do
        begin
          p := PreviewImage.Picture.bitmap.ScanLine[(BGy + CloneYOffset)];
          P2 := PreviewImage.Picture.bitmap.ScanLine[BGy];
          for BGx := X - CloneSize to X + CloneSize do
          begin
            // How to turn a Box into a Circle
            If ((((BGx - X) * (BGx - X)) + ((BGy - Y) * (BGy - Y))) <
              (CloneSize * 3.1415926535)) then
            begin
              If CloneXorShape then
                p[(BGx + CloneXOffset)] := P2[BGx]
              else
                P2[BGx] := p[BGx + CloneXOffset];
            end;
          end;
        end
      else
        for BGy := Y - CloneSize to Y + CloneSize do
        begin
          p := PreviewImage.Picture.bitmap.ScanLine[(BGy + CloneYOffset)];
          P2 := PreviewImage.Picture.bitmap.ScanLine[BGy];
          for BGx := X - CloneSize to X + CloneSize do
          begin
            If CloneXorShape then
              p[(BGx + CloneXOffset)] := P2[BGx]
            else
              P2[BGx] := p[BGx + CloneXOffset];
          end;
        end;
    end;
    CloneProcessing := False;
  end
  else If CloningOn and CloneMoving then
  begin
    CloneProcessing := True;
    If CloneMouseX > X then
      CloneXOffset := CloneXOffset + (CloneMouseX - X)
    else
      CloneXOffset := CloneXOffset + (CloneMouseX - X);
    If CloneMouseY > Y then
      CloneYOffset := CloneYOffset + (CloneMouseY - Y)
    else
      CloneYOffset := CloneYOffset + (CloneMouseY - Y);
    CloneProcessing := False;
  end;
  If CloningOn then
  begin
    // Draw the CloneBox
    { If ( (X+CloneXOffset-CloneSize < 0) and
      (X+CloneXOffset+CloneSize < BGImage.Width)and
      (Y+CloneYOffset-CloneSize < 0) and
      (Y+CloneYOffset+CloneSize < BGImage.Height)) then }
    begin
      PreviewImage.Canvas.Pen.Mode := pmNotXor; // pmCopy;
      If CloneBoxorCircle then
        PreviewImage.Canvas.Ellipse(X + CloneXOffset - CloneSize,
          Y + CloneYOffset - CloneSize, X + CloneXOffset + CloneSize,
          Y + CloneYOffset + CloneSize)
      else
        PreviewImage.Canvas.Rectangle(X + CloneXOffset - CloneSize,
          Y + CloneYOffset - CloneSize, X + CloneXOffset + CloneSize,
          Y + CloneYOffset + CloneSize);
    end;
    CloneMouseX := X;
    CloneMouseY := Y;
    If TLCloneBox.checked then
      ClonePanel.Caption :=
        (inttostr(X) + ' x:y ' + inttostr(PreviewImage.Height - Y))
    else
      ClonePanel.Caption := (inttostr(X) + ' x:y ' + inttostr(Y));
  end;
  If Mousing then
  begin
    If DotBoxRG.Itemindex = 0 then
    begin
      If SpinEdit1.value = 1 then
        PreviewImage.Picture.bitmap.Canvas.pixels[X, Y] := IsWhiteColor
      else
        PreviewImage.Picture.bitmap.Canvas.Ellipse(Rect(X - SpinEdit1.value,
          Y - SpinEdit1.value, X + SpinEdit1.value, Y + SpinEdit1.value))
    end
    else If DotBoxRG.Itemindex = 1 then
      PreviewImage.Picture.bitmap.Canvas.fillrect(Rect(X - SpinEdit1.value,
        Y - SpinEdit1.value, X + SpinEdit1.value, Y + SpinEdit1.value))
    else If DotBoxRG.Itemindex = 2 then
      PreviewImage.Picture.bitmap.Canvas.Ellipse(Rect(X - SpinEdit1.value,
        Y - SpinEdit1.value, X + SpinEdit1.value, Y + SpinEdit1.value))
    else If DotBoxRG.Itemindex = 3 then
      PreviewImage.Picture.bitmap.Canvas.floodfill(X, Y,
        PreviewImage.Picture.bitmap.Canvas.pixels[X, Y], fsSurface)
    else If DotBoxRG.Itemindex = 4 then
      PreviewImage.Picture.bitmap.Canvas.floodfill(X, Y, IsWhiteColor, fsBorder)
    else
    begin // Painting
      PreviewImage.Picture.bitmap.Canvas.pixels[X, Y] := IsWhiteColor;
      PreviewImage.Picture.bitmap.Canvas.pixels[X + random(SpinEdit1.value),
        Y + random(SpinEdit1.value)] := IsWhiteColor;
      PreviewImage.Picture.bitmap.Canvas.pixels[X - random(SpinEdit1.value),
        Y - random(SpinEdit1.value)] := IsWhiteColor;
    end;
  end;
end;

procedure TATextureEditorForm.PreviewImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempBitmap: TBitmap;
  ind: Integer;
begin
  If CloningOn then
  begin
    If Cloning then
      Cloning := False;
    If CloneMoving then
      CloneMoving := False;
  end
  else If Mousing then
  begin // Painting
    Mousing := False;
    Processing := False;
  end
  else
  begin // Wand Filling  on Right Click
    If Processing then
      exit
    else If WandProcessing then
    begin
      Processing := True;
      ATextureEditorForm.cursor := crhourglass;
      TipMaskMaker.Assign(PreviewImage.Picture.bitmap); // allows 1 Undo
      TempBitmap := TBitmap.create;
      TempBitmap.Pixelformat := pf24bit;
      TempBitmap.Assign(PreviewImage.Picture.bitmap);
      ind := WandCoB.Itemindex;
      if ind < 0 then
        ind := 0;
      fill(TempBitmap, X, Y, clwhite, TempBitmap.Canvas.pixels[X, Y],
        WandTrackBar.Position / 100, ind, intensTrackBar.Position /
        100 { 0.1 } , trasTrackBar.Position / 100 { 0.5 } );
      { procedure fill(var bitmap1:tbitmap;
        a,b:integer;newcolor,oldcolor:tcolor;
        tolerance:Double;fill_type:byte;intens,tras:Double); }
      PreviewImage.Picture.bitmap := TempBitmap;
      TempBitmap.free;
      Processing := False;
      WandProcessing := False;
      ATextureEditorForm.cursor := crdefault;
    end;
  end;
end;

/// ////////////////////////////////////////////////
procedure TATextureEditorForm.PaintCloneRGClick(Sender: TObject);
begin
  Case PaintCloneRG.Itemindex of
    0:
      begin
        CloningOn := False;
      end;
    1:
      begin
        CloningOn := True;
        // use XOR mode to draw/erase the First / Last one
        PreviewImage.Canvas.Pen.Mode := pmNotXor;
        If CloneBoxorCircle then
          PreviewImage.Canvas.Ellipse(CloneMouseX + CloneXOffset - CloneSize,
            CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
            CloneSize, CloneMouseY + CloneYOffset + CloneSize)
        else
          PreviewImage.Canvas.Rectangle(CloneMouseX + CloneXOffset - CloneSize,
            CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
            CloneSize, CloneMouseY + CloneYOffset + CloneSize);
      end;
  end; // case
  CloneSize1.Enabled := CloningOn;
  CloneBoxorCircleMenu.Enabled := CloningOn;
  CloneXorShapeMenu.Enabled := CloningOn;
end;

procedure TATextureEditorForm.CloneSize1Click(Sender: TObject);
begin
  CloneSize := strtoint(inputbox('Input Desired Clone Size', 'Clone Area Size',
    inttostr(CloneSize)));
end;

procedure TATextureEditorForm.CloneBoxorCircleMenuClick(Sender: TObject);
begin
  CloneBoxorCircleMenu.checked := not CloneBoxorCircleMenu.checked;
  CloneBoxorCircle := CloneBoxorCircleMenu.checked;
  // Erase the LAST type..Reverse the other
  If CloneBoxorCircle then
    PreviewImage.Canvas.Rectangle(CloneMouseX + CloneXOffset - CloneSize,
      CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
      CloneSize, CloneMouseY + CloneYOffset + CloneSize)
  else
    PreviewImage.Canvas.Ellipse(CloneMouseX + CloneXOffset - CloneSize,
      CloneMouseY + CloneYOffset - CloneSize, CloneMouseX + CloneXOffset +
      CloneSize, CloneMouseY + CloneYOffset + CloneSize);
end;

procedure TATextureEditorForm.CloneXorShapeMenuClick(Sender: TObject);
begin
  CloneXorShapeMenu.checked := not CloneXorShapeMenu.checked;
  CloneXorShape := CloneXorShapeMenu.checked;
end;

procedure TATextureEditorForm.SaveAlphaRGClick(Sender: TObject);
var
  s: string;
  tga: TTGAImage;
begin
  If Processing then
    exit;
  If SaveAlphaRG.Itemindex = 4 then
    exit;
  Processing := True;
  ATextureEditorForm.cursor := crhourglass;
  SaveDialog1.Filter := 'Clouds (*.bmp;*.tga)|*.tga;*.bmp';
  SaveDialog1.InitialDir := ImagePath; // ExtractFilePath(Application.Exename);
  SaveDialog1.DefaultExt := 'bmp';
  SaveDialog1.Filename := MaskFilenameEdit.Text;
  if SaveDialog1.Execute then
  begin
    s := SaveDialog1.Filename;
    if extractfileext(s) = '' then
      s := s + '.tga'
    else if extractfileext(s) = '.bmp' then
    begin
    end
    else
      changefileext(s, '.tga');
    { begin
      s:=copy(s,0,length(s)-4);
      s:=s+'.bmp';
      end; }

    Case SaveAlphaRG.Itemindex of
      0: // 24 bit..No alpha
        Begin
          changefileext(s, '.bmp');
          PreviewImage.Picture.bitmap.SaveToFile(s);
        End;
      1: // Alpha SuperBlack
        Begin
          GenerateAlpha(clBlack, False, False);
          // Image321.bitmap.SaveToFile(s);
        End;
      2: // Alpha  FromRGBIntensity
        Begin
          GenerateAlpha(-1, True, False);
          // Image321.bitmap.SaveToFile(s);
        End;
      3: // Alpha  FromRGBSqrtIntensity
        Begin
          GenerateAlpha(-1, True, True);
          // Image321.bitmap.SaveToFile(s);
        End;
    End;
    Case SaveAlphaRG.Itemindex of
      1 .. 3:
        begin
          if lowercase(extractfileext(s)) = '.tga' then
          begin
            tga := TTGAImage.create;
            try
              tga.Assign(Image321.bitmap { bmp }{ pic.Bitmap } );
              tga.SaveToFile(s)
            finally
              tga.free;
            end;
          end
          else
            PreviewImage.Picture.bitmap.SaveToFile(s);
        end;
    end; // case

  end; // save
  Processing := False;
  ATextureEditorForm.cursor := crdefault;
end;

// From GLScene TTB demo
procedure TATextureEditorForm.GenerateAlpha(transparentColor: TColor;
  fromIntensity: Boolean; doSqrt: Boolean);
var
  bmp: TBitmap;
  bmp32: TGLBitmap32;
  X, Y: Integer;
  pSrc: PGLPixel32Array;
  pDest: PIntegerArray;
  c: Integer;
begin
  // Opaque alpha channel
  // bmp:=SpawnBitmap;
  bmp := TBitmap.create;
  bmp.Pixelformat := pf32bit;
  bmp.Width := PreviewImage.Width;
  bmp.Height := PreviewImage.Height;
  bmp32 := TGLBitmap32.create;
  try
    bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height),
      PreviewImage.Picture.Graphic);
    bmp32.Assign(bmp);
    if transparentColor <> -1 then
      bmp32.SetAlphaTransparentForColor(transparentColor);
    if fromIntensity then
      bmp32.SetAlphaFromIntensity;
    if doSqrt then
      bmp32.SqrtAlpha;
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := bmp32.ScanLine[Y];
      pDest := bmp.ScanLine[bmp.Height - 1 - Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := pSrc[X].a;
        c := c + (c shl 8) + (c shl 16);
        pDest[X] := c;
      end;
    end;
    // Image321.Bitmap:=bmp;
    Image321.bitmap.Assign(bmp);
  finally
    bmp32.free;
    bmp.free;
  end;
  // TextureChanged;
end;

procedure TATextureEditorForm.ExitBtnClick(Sender: TObject);
begin
  close;
end;

procedure TATextureEditorForm.SmoothAllBtnClick(Sender: TObject);
var
  bmp: TBitmap;
  TotalUp, Reducer, Where, Y, X, sx: Integer;

  R_Filtered0, R_Filtered1, R_Filtered2: Double;
  Filter: Array [0 .. 2, 0 .. 2] of Double;
begin
  If CloningOn then
    exit;
  If Mousing or Processing then
    exit;
  Processing := True;
  SmoothAllBtn.Caption := 'working';
  Application.ProcessMessages;
  Case SmoothRG.Itemindex of
    0:
      Begin // Mean
        Filter[0, 0] := 1;
        Filter[1, 0] := 1;
        Filter[2, 0] := 1;
        Filter[0, 1] := 1;
        Filter[1, 1] := 1;
        Filter[2, 1] := 1;
        Filter[0, 2] := 1;
        Filter[1, 2] := 1;
        Filter[2, 2] := 1;
        Reducer := 9;
      end;
    1:
      Begin // Gaussian
        Filter[0, 0] := 0.25;
        Filter[1, 0] := 0.5;
        Filter[2, 0] := 0.25;
        Filter[0, 1] := 0.5;
        Filter[1, 1] := 1;
        Filter[2, 1] := 0.5;
        Filter[0, 2] := 0.25;
        Filter[1, 2] := 0.5;
        Filter[2, 2] := 0.25;
        Reducer := 4;
      end;
    2:
      Begin // Gaussian
        Filter[0, 0] := 0.05;
        Filter[1, 0] := 0.1;
        Filter[2, 0] := 0.05;
        Filter[0, 1] := 0.1;
        Filter[1, 1] := 0.4;
        Filter[2, 1] := 0.1;
        Filter[0, 2] := 0.05;
        Filter[1, 2] := 0.1;
        Filter[2, 2] := 0.05;
        Reducer := 1;
      end;
    3:
      Begin // Gaussian
        Filter[0, 0] := 1;
        Filter[1, 0] := 2;
        Filter[2, 0] := 1;
        Filter[0, 1] := 2;
        Filter[1, 1] := 4;
        Filter[2, 1] := 2;
        Filter[0, 2] := 1;
        Filter[1, 2] := 2;
        Filter[2, 2] := 1;
        Reducer := 16;
      end;
    4:
      Begin // sharp
        Filter[0, 0] := -1;
        Filter[1, 0] := -1;
        Filter[2, 0] := -1;
        Filter[0, 1] := -1;
        Filter[1, 1] := 9;
        Filter[2, 1] := -1;
        Filter[0, 2] := -1;
        Filter[1, 2] := -1;
        Filter[2, 2] := -1;
        Reducer := 1;
      end;
  else // repeated instead of 1..4 and have this here, huh
    Begin // Mean
      Filter[0, 0] := 1;
      Filter[1, 0] := 1;
      Filter[2, 0] := 1;
      Filter[0, 1] := 1;
      Filter[1, 1] := 1;
      Filter[2, 1] := 1;
      Filter[0, 2] := 1;
      Filter[1, 2] := 1;
      Filter[2, 2] := 1;
      Reducer := 9;
    end;
  End; // case
  TipMaskMaker.Assign(PreviewImage.Picture.bitmap); // allows 1 Undo
  bmp := TBitmap.create;
  bmp.Pixelformat := pf24bit; // pf32bit;
  bmp.Width := PreviewImage.Width;
  bmp.Height := PreviewImage.Height;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Brush.Style := bsSolid;
  bmp.Canvas.fillrect(Rect(0, 0, bmp.Height, bmp.Width));
  // FillRect excludes right and bottom..so NOT -1
  // bmp.Canvas.FillRect(Rect(0,0,bmp.Height-1,bmp.Width-1));

  Where := 1;
  try
    with PreviewImage.Picture.bitmap.Canvas { TipMaskMaker.Canvas } do
    begin
      for Y := 1 to PreviewImage.Picture.bitmap.Height - 2 do
      begin
        for X := 1 to PreviewImage.Picture.bitmap.Width - 2 do
        begin
          R_Filtered0 := 0;
          R_Filtered1 := 0;
          R_Filtered2 := 0;
          for sx := -1 to 1 do
          begin
            R_Filtered0 := (R_Filtered0 + // Where
              ((GetBValue(pixels[sx + X, Y - 1])) * Filter[(sx + Where), (0)]));
            R_Filtered1 :=
              (R_Filtered1 + ((GetBValue(pixels[sx + X, Y])) *
              Filter[(sx + Where), (1)]));
            R_Filtered2 :=
              (R_Filtered2 + ((GetBValue(pixels[sx + X, Y + 1])) *
              Filter[(sx + Where), (2)]));
          end;
          TotalUp := round((R_Filtered0 + R_Filtered1 + R_Filtered2) / Reducer);
          If TotalUp > 255 then
            TotalUp := 255;
          bmp.Canvas.pixels[X, Y] := RGB(TotalUp, TotalUp, TotalUp);
        end;
      end;
    end;
    PreviewImage.Picture.bitmap.Assign(bmp);
  finally
    bmp.free;
  end;
  SmoothAllBtn.Caption := 'Smooth';
  Processing := False;
end;

end.
