{: A simple utility that allows combining RGB and Alpha channel into a single
   32 bits texture, also allows to view RGB & Alpha channel of a 32 bits texture.<br>
   The implementation isn't high performance, just sufficiently fast for
   interactive use.<p>

   Eric Grange / GLScene<br>
   http://glscene.org
}
unit FTTBMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLObjects, GLHUDObjects, GLScene, GLMisc, ExtCtrls,
  GLWin32Viewer, ComCtrls, ActnList, Menus, ImgList, JPeg, TGA, ToolWin,
  ExtDlgs, StdCtrls;

type
  TTTBMain = class(TForm)
    MainMenu: TMainMenu;
    ImageList: TImageList;
    ActionList: TActionList;
    File1: TMenuItem;
    ACExit: TAction;
    Exit1: TMenuItem;
    PAImages: TPanel;
    PAPreview: TPanel;
    Splitter1: TSplitter;
    PageControl: TPageControl;
    TSRGB: TTabSheet;
    TSAlpha: TTabSheet;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLCube: TGLCube;
    GLLightSource: TGLLightSource;
    HSBkgnd: TGLHUDSprite;
    ToolBar: TToolBar;
    ACImport: TAction;
    ACOpenTexture: TAction;
    TBImport: TToolButton;
    ScrollBox1: TScrollBox;
    IMRGB: TImage;
    ScrollBox2: TScrollBox;
    IMAlpha: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    Label1: TLabel;
    CBWidth: TComboBox;
    Label2: TLabel;
    CBHeight: TComboBox;
    N1: TMenuItem;
    Exit2: TMenuItem;
    ACSaveTexture: TAction;
    SaveDialog: TSaveDialog;
    SaveTexture1: TMenuItem;
    Panel2: TPanel;
    CBTextureFiltering: TCheckBox;
    CBBackground: TComboBox;
    procedure PAPreviewResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ACImportExecute(Sender: TObject);
    procedure CBWidthChange(Sender: TObject);
    procedure CBTextureFilteringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACOpenTextureExecute(Sender: TObject);
    procedure ACSaveTextureExecute(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBBackgroundChange(Sender: TObject);

  private
    { Private declarations }
    mx, my : Integer;

    procedure ResetAlpha;
    function SpawnBitmap : TBitmap;
    procedure ResizeImage(im : TImage);
    procedure NormalizeAlpha;
    procedure TextureChanged;
    procedure BreakupTexture(bmp : TBitmap);

  public
    { Public declarations }
  end;

var
  TTBMain: TTTBMain;

implementation

uses GLTexture;

{$R *.dfm}

procedure TTTBMain.FormCreate(Sender: TObject);
begin
   ResetAlpha;
end;

procedure TTTBMain.FormShow(Sender: TObject);
begin
   PAPreviewResize(Self);
end;

procedure TTTBMain.PAPreviewResize(Sender: TObject);
const
   cTileSize = 32;
var
   w, h : Integer;
begin
   // adjust background, we could just have made huge one,
   // but that would have been too simple for a demo ;)
   w:=(GLSceneViewer.Width div cTileSize);
   h:=(GLSceneViewer.Height div cTileSize);
   HSBkgnd.XTiles:=w;
   HSBkgnd.YTiles:=h;
   w:=w*cTileSize+cTileSize;
   h:=h*cTileSize+cTileSize;
   HSBkgnd.Width:=w;
   HSBkgnd.Height:=h;
   HSBkgnd.Position.SetPoint(w div 2, h div 2, 0);
   // zoom scene with viewer's width
   GLCamera.SceneScale:=GLSceneViewer.Width/120;
end;

procedure TTTBMain.ACImportExecute(Sender: TObject);
begin
   if OpenPictureDialog.Execute then begin
      if PageControl.ActivePage=TSRGB then begin
         IMRGB.Picture.LoadFromFile(OpenPictureDialog.FileName);
         ResizeImage(IMRGB);
      end else begin
         IMAlpha.Picture.LoadFromFile(OpenPictureDialog.FileName);
         ResizeImage(IMAlpha);
         NormalizeAlpha;
      end;
      TextureChanged;
   end;
end;

function TTTBMain.SpawnBitmap : TBitmap;
begin
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf32bit;
   Result.Width:=StrToInt(CBWidth.Text);
   Result.Height:=StrToInt(CBHeight.Text);
end;

procedure TTTBMain.ResetAlpha;
var
   bmp : TBitmap;
begin
   // Opaque alpha channel
   bmp:=SpawnBitmap;
   try
      with bmp.Canvas do begin
         Brush.Color:=clWhite;
         FillRect(Rect(0, 0, bmp.Width, bmp.Height));
      end;
      IMAlpha.Picture.Bitmap:=bmp;
   finally
      bmp.Free;
   end;
end;

procedure TTTBMain.NormalizeAlpha;
var
   col : Byte;
   x, y, c : Integer;
   bmp : TBitmap;
   pSrc, pDest : PIntegerArray;
begin
   bmp:=SpawnBitmap;
   try
      for y:=0 to bmp.Height-1 do begin
         pSrc:=IMAlpha.Picture.Bitmap.ScanLine[y];
         pDest:=bmp.ScanLine[y];
         for x:=0 to bmp.Width-1 do begin
            c:=pSrc[x];
            col:=Round(0.3*(c and $FF)+0.59*((c shr 8) and $FF)+0.11*((c shr 16) and $FF));
            pDest[x]:=col+(col shl 8)+(col shl 16);
         end;
      end;
      IMAlpha.Picture.Bitmap:=bmp;
   finally
      bmp.Free;
   end;
end;

procedure TTTBMain.ResizeImage(im : TImage);
var
   bmp : TBitmap;
begin
   if im.Height=0 then Exit;
   bmp:=SpawnBitmap;
   try
      bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), im.Picture.Graphic);
      im.Picture.Bitmap:=bmp;
   finally
      bmp.Free;
   end;
end;

procedure TTTBMain.BreakupTexture(bmp : TBitmap);
var
   bmpAlpha, bmpRGB : TBitmap;
   y, x, c : Integer;
   pRGB, pAlpha, pSrc : PIntegerArray;
begin
   bmpAlpha:=SpawnBitmap;
   bmpRGB:=SpawnBitmap;
   try
      bmpAlpha.Width:=bmp.Width;
      bmpAlpha.Height:=bmp.Height;
      bmpRGB.Width:=bmp.Width;
      bmpRGB.Height:=bmp.Height;
      for y:=0 to bmp.Height-1 do begin
         pRGB:=bmpRGB.ScanLine[y];
         pAlpha:=bmpAlpha.ScanLine[y];
         pSrc:=bmp.ScanLine[y];
         for x:=0 to bmp.Width-1 do begin
            c:=pSrc[x];
            pRGB[x]:=(c and $FFFFFF);
            c:=(c shr 24) and $FF;
            pAlpha[x]:=c+(c shl 8)+(c shl 16);
         end;
      end;
      IMRGB.Picture.Bitmap:=bmpRGB;
      IMAlpha.Picture.Bitmap:=bmpAlpha;
   finally
      bmpAlpha.Free;
      bmpRGB.Free;
   end;

end;

procedure TTTBMain.TextureChanged;
var
   bmp : TBitmap;
   y, x : Integer;
   pRGB, pAlpha, pDest : PIntegerArray;
begin
   if IMRGB.Picture.Graphic.Empty then Exit;
   if IMAlpha.Picture.Height=0 then begin
      GLCube.Material.Texture.Assign(IMRGB.Picture);
   end else begin
      bmp:=SpawnBitmap;
      try
         for y:=0 to bmp.Height-1 do begin
            pRGB:=IMRGB.Picture.Bitmap.ScanLine[y];
            pAlpha:=IMAlpha.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to bmp.Width-1 do
               pDest[x]:=pRGB[x] or ((pAlpha[x] and $FF) shl 24);
         end;
         GLCube.Material.Texture.Assign(bmp);
      finally
         bmp.Free;
      end;
   end;
end;

procedure TTTBMain.CBWidthChange(Sender: TObject);
begin
   ResizeImage(IMRGB);
   ResizeImage(IMAlpha);
end;

procedure TTTBMain.CBTextureFilteringClick(Sender: TObject);
begin
   with GLCube.Material.Texture do begin
      if CBTextureFiltering.Checked then begin
         MagFilter:=maLinear;
         MinFilter:=miLinearMipmapLinear;
      end else begin
         MagFilter:=maNearest;
         MinFilter:=miNearest;
      end;
   end;
end;

procedure TTTBMain.ACOpenTextureExecute(Sender: TObject);
var
   pic : TPicture;
begin
   if OpenPictureDialog.Execute then begin
      pic:=TPicture.Create;
      try
         pic.LoadFromFile(OpenPictureDialog.FileName);
         if (pic.Graphic is TBitmap) and (pic.Bitmap.PixelFormat=pf32bit) then begin
            BreakupTexture(pic.Bitmap);
            ResizeImage(IMAlpha);
         end else begin
            IMRGB.Picture:=pic;
            ResetAlpha;
         end;
         ResizeImage(IMRGB);
         TextureChanged;
      finally
         pic.Free;
      end;
   end;
end;

procedure TTTBMain.ACSaveTextureExecute(Sender: TObject);
var
   pic : TPicture;
   fName : String;
   tga : TTGAImage;
begin
   pic:=(GLCube.Material.Texture.Image as TGLPictureImage).Picture;
   if (pic.Height>0) and SaveDialog.Execute then begin
      fName:=SaveDialog.FileName;
      if ExtractFileExt(fName)='' then
         if SaveDialog.FilterIndex=1 then
            fName:=fName+'.bmp'
         else fName:=fName+'.tga';
      if LowerCase(ExtractFileExt(fName))='.tga' then begin
         tga:=TTGAImage.Create;
         try
            tga.Assign(pic.Bitmap);
            tga.SaveToFile(fName)
         finally
            tga.Free;
         end;
      end else pic.SaveToFile(fName);
   end;
end;

procedure TTTBMain.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TTTBMain.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then begin
      GLCamera.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TTTBMain.CBBackgroundChange(Sender: TObject);
begin
   HSBkgnd.Visible:=(CBBackground.ItemIndex=0);
   case CBBackground.ItemIndex of
      1 : GLSceneViewer.Buffer.BackgroundColor:=clBlack;
      2 : GLSceneViewer.Buffer.BackgroundColor:=clSilver;
      3 : GLSceneViewer.Buffer.BackgroundColor:=clWhite;
   end;
end;

end.
