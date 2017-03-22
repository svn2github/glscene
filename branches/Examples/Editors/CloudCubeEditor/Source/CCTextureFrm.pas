unit CCTextureFrm;

//Written Dec2006 by Ivan Lee Herring to make
//a 512x512 Combined Cloud Texture from 16 128x128 images

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ExtDlgs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  GLVectorGeometry,
  GLFileTGA,
  CCEditorFrm;

type
  TATextureCombinerForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Image16: TImage;
    Image15: TImage;
    Image14: TImage;
    Image13: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image8: TImage;
    Image7: TImage;
    Image6: TImage;
    Image5: TImage;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label104: TLabel;
    Label105: TLabel;
    CombineTex1Btn: TSpeedButton;
    CombineTex1Edit: TEdit;
    CombineTex2Btn: TSpeedButton;
    CombineTex2Edit: TEdit;
    CombineTex3Btn: TSpeedButton;
    CombineTex3Edit: TEdit;
    CombineTex4Btn: TSpeedButton;
    CombineTex4Edit: TEdit;
    CombineTexALLBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    ImageSizeRG: TRadioGroup;
    Label1: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    function SpawnBitmap(Width,Height :Integer) : TBitmap;
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure Image8Click(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure Image10Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure Image13Click(Sender: TObject);
    procedure Image14Click(Sender: TObject);
    procedure Image15Click(Sender: TObject);
    procedure Image16Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure CombineTex1BtnClick(Sender: TObject);
    procedure CombineTex2BtnClick(Sender: TObject);
    procedure CombineTex3BtnClick(Sender: TObject);
    procedure CombineTex4BtnClick(Sender: TObject);
    procedure CombineTexALLBtnClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  ATextureCombinerForm: TATextureCombinerForm;

//=====================================================================
implementation
//=====================================================================

{$R *.DFM}

procedure TATextureCombinerForm.FormCreate(Sender: TObject);
begin
//
end;

procedure TATextureCombinerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
// Should Clear all to save memory.. o well
end;

procedure TATextureCombinerForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TATextureCombinerForm.SpawnBitmap(Width,Height :Integer) : TBitmap;
begin
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
end;

procedure TATextureCombinerForm.Image1Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then
   begin
      try
         Image1.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
         if (Image1.Picture.Graphic is TBitmap) and (Image1.Picture.Bitmap.PixelFormat=pf32bit) then
         begin

         end else
         begin
           Image1.Picture.Bitmap.PixelFormat:=pf32bit;
         end;
      finally

      end;
   end;
end;

procedure TATextureCombinerForm.Image2Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image2.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image2.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image3Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image3.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image3.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image4Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image4.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image4.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image5Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image5.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image5.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image6Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image6.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image6.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image7Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image7.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image7.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image8Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image8.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image8.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image9Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image9.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image9.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image10Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image10.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image10.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image11Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image11.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image11.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image12Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image12.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image12.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image13Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image13.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image13.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image14Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image14.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image14.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image15Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image15.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image15.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;

procedure TATextureCombinerForm.Image16Click(Sender: TObject);
begin
   OpenPictureDialog.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
   if OpenPictureDialog.Execute then begin
   Image16.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
   Image16.Picture.Bitmap.PixelFormat:=pf32bit;
   end;
end;


//Crude but effective..well at least it works for me...
procedure TATextureCombinerForm.Save1Click(Sender: TObject);
var
   bmp : TBitmap;
   y, x : Integer;
   pRGBA,  pDest ://PByteArray;
                  PIntegerArray;
   fName : String;
   tga : TTGAImage;
begin
  //Should check the all...
  if Image1.Picture.Graphic.Empty then Exit;
  SaveDialog1.Filter := 'Cloud image (*.bmp;*.tga)|*.bmp;*.tga';
  SaveDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  SaveDialog1.DefaultExt:='bmp';
  SaveDialog1.Filename:='';//'*.bmp' ;
  If SaveDialog1.Execute then
  begin
    bmp:=SpawnBitmap(512,512);
    try
      //1
      for y:=0 to Image1.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image1.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to Image1.Picture.Bitmap.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //2
      for y:=0 to Image2.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image2.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to Image2.Picture.Bitmap.Width-1 do
               pDest[x+128]:=pRGBA[x] ;
      end;
      //3
      for y:=0 to Image3.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image3.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to Image3.Picture.Bitmap.Width-1 do
               pDest[x+256]:=pRGBA[x] ;
      end;
      //4
      for y:=0 to Image4.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image4.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to Image4.Picture.Bitmap.Width-1 do
               pDest[x+384]:=pRGBA[x] ;
      end;
      //5 .. NEXT ROW DOWN
      for y:=0 to Image5.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image5.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+128];
            for x:=0 to Image5.Picture.Bitmap.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //6
      for y:=0 to Image6.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image6.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+128];
            for x:=0 to Image6.Picture.Bitmap.Width-1 do
               pDest[x+128]:=pRGBA[x] ;
      end;
      //7
      for y:=0 to Image7.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image7.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+128];
            for x:=0 to Image7.Picture.Bitmap.Width-1 do
               pDest[x+256]:=pRGBA[x] ;
      end;
      //8
      for y:=0 to Image8.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image8.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+128];
            for x:=0 to Image8.Picture.Bitmap.Width-1 do
               pDest[x+384]:=pRGBA[x] ;
      end;
      //9 .. NEXT ROW DOWN
      for y:=0 to Image9.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image9.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+256];
            for x:=0 to Image9.Picture.Bitmap.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //10
      for y:=0 to Image10.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image10.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+256];
            for x:=0 to Image10.Picture.Bitmap.Width-1 do
               pDest[x+128]:=pRGBA[x] ;
      end;
      //11
      for y:=0 to Image11.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image11.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+256];
            for x:=0 to Image11.Picture.Bitmap.Width-1 do
               pDest[x+256]:=pRGBA[x] ;
      end;
      //12
      for y:=0 to Image12.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image12.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+256];
            for x:=0 to Image12.Picture.Bitmap.Width-1 do
               pDest[x+384]:=pRGBA[x] ;
      end;
      //13 .. NEXT ROW DOWN
      for y:=0 to Image13.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image13.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+384];
            for x:=0 to Image13.Picture.Bitmap.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //14
      for y:=0 to Image14.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image14.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+384];
            for x:=0 to Image14.Picture.Bitmap.Width-1 do
               pDest[x+128]:=pRGBA[x] ;
      end;
      //15
      for y:=0 to Image15.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image15.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+384];
            for x:=0 to Image15.Picture.Bitmap.Width-1 do
               pDest[x+256]:=pRGBA[x] ;
      end;
      //16
      for y:=0 to Image16.Picture.Bitmap.Height-1 do
      begin
            pRGBA:=Image16.Picture.Bitmap.ScanLine[y];
            pDest:=bmp.ScanLine[y+384];
            for x:=0 to Image16.Picture.Bitmap.Width-1 do
               pDest[x+384]:=pRGBA[x] ;
      end;
    finally
      fName:=SaveDialog1.FileName;
      if ExtractFileExt(fName)='' then fName:=fName+'.bmp' else
      if ExtractFileExt(fName)='.bmp' then begin {do nothing} end
         else changefileext(fName,'.tga');
      if LowerCase(ExtractFileExt(fName))='.tga' then begin
         tga:=TTGAImage.Create;
         try
            tga.Assign(bmp{pic.Bitmap});
            tga.SaveToFile(fName)
         finally
            tga.Free;
         end;
      end else bmp.SaveToFile(fName);
      //bmp.SaveToFile(SaveDialog1.FileName);
      bmp.Free;
    end;
  end;
end;

procedure TATextureCombinerForm.CombineTex1BtnClick(Sender: TObject);
begin
   OpenDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  if OpenDialog1.Execute then    { Display Open dialog box }
    CombineTex1Edit.Text := OpenDialog1.FileName;
end;

procedure TATextureCombinerForm.CombineTex2BtnClick(Sender: TObject);
begin
   OpenDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  if OpenDialog1.Execute then    { Display Open dialog box }
    CombineTex2Edit.Text := OpenDialog1.FileName;
end;

procedure TATextureCombinerForm.CombineTex3BtnClick(Sender: TObject);
begin
   OpenDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  if OpenDialog1.Execute then    { Display Open dialog box }
    CombineTex3Edit.Text := OpenDialog1.FileName;
end;

procedure TATextureCombinerForm.CombineTex4BtnClick(Sender: TObject);
begin
   OpenDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  if OpenDialog1.Execute then    { Display Open dialog box }
    CombineTex4Edit.Text := OpenDialog1.FileName;
end;

procedure TATextureCombinerForm.CombineTexALLBtnClick(Sender: TObject);
var
   bmp,bmp1,bmp2,bmp3,bmp4 : TBitmap;
   InSize,OutSize,
   y, x : Integer;
   pRGBA,  pDest :PIntegerArray;
   //   pic : TPicture;
   fName : String;
   tga : TTGAImage;
function SpawnBitmap(Width,Height :Integer) : TBitmap;
begin
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
end;
begin
  //Should check the all...
  if CombineTex1Edit.Text='' then Exit;
  if CombineTex2Edit.Text='' then Exit;
  if CombineTex3Edit.Text='' then Exit;
  if CombineTex4Edit.Text='' then Exit;
//  SaveDialog1.Filter := 'Cloud image (*.tga)|*.tga';
  SaveDialog1.Filter := 'Cloud image (*.bmp;*.tga)|*.bmp;*.tga';
  SaveDialog1.InitialDir:=ImagePath;//ExtractFilePath(Application.Exename);
  SaveDialog1.DefaultExt:='bmp';
  SaveDialog1.Filename:='';//'*.tga' ;
Case ImageSizeRG.ItemIndex of
0:Begin
InSize:=128;
OutSize:=256;
End;
1:Begin
InSize:=256;
OutSize:=512;
End;
else  //2:
Begin
InSize:=512;
OutSize:=1024;
End;
End;
  If SaveDialog1.Execute then
  begin
    bmp1:=SpawnBitmap(InSize,InSize);
    bmp2:=SpawnBitmap(InSize,InSize);
    bmp3:=SpawnBitmap(InSize,InSize);
    bmp4:=SpawnBitmap(InSize,InSize);
    bmp:=SpawnBitmap(OutSize,OutSize);
    bmp1.LoadFromFile(CombineTex1Edit.Text);
    bmp1.PixelFormat:=pf32bit;  //just in case it Not 32 bit
    bmp2.LoadFromFile(CombineTex2Edit.Text);
    bmp2.PixelFormat:=pf32bit;
    bmp3.LoadFromFile(CombineTex3Edit.Text);
    bmp3.PixelFormat:=pf32bit;
    bmp4.LoadFromFile(CombineTex4Edit.Text);
    bmp4.PixelFormat:=pf32bit;
    try
      //1
      for y:=0 to bmp1.Height-1 do
      begin
            pRGBA:=bmp1.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to bmp1.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //2
      for y:=0 to bmp2.Height-1 do
      begin
            pRGBA:=bmp2.ScanLine[y];
            pDest:=bmp.ScanLine[y];
            for x:=0 to bmp2.Width-1 do
               pDest[x+InSize]:=pRGBA[x] ;
      end;
      //3 .. NEXT ROW DOWN
      for y:=0 to bmp3.Height-1 do
      begin
            pRGBA:=bmp3.ScanLine[y];
            pDest:=bmp.ScanLine[y+InSize];
            for x:=0 to bmp3.Width-1 do
               pDest[x]:=pRGBA[x] ;
      end;
      //4
      for y:=0 to bmp4.Height-1 do
      begin
            pRGBA:=bmp4.ScanLine[y];
            pDest:=bmp.ScanLine[y+InSize];
            for x:=0 to bmp4.Width-1 do
               pDest[x+InSize]:=pRGBA[x] ;
      end;
    finally
    //tga

      fName:=SaveDialog1.FileName;
      if ExtractFileExt(fName)='' then fName:=fName+'.bmp' else
      if ExtractFileExt(fName)='.bmp' then begin {do nothing} end
         else changefileext(fName,'.tga');
      if LowerCase(ExtractFileExt(fName))='.tga' then begin
         tga:=TTGAImage.Create;
         try
            tga.Assign(bmp{pic.Bitmap});
            tga.SaveToFile(fName)
         finally
            tga.Free;
         end;
      end else bmp.SaveToFile(fName);
      //bmp.SaveToFile(SaveDialog1.FileName);
      bmp.Free;
      bmp1.Free;
      bmp2.Free;
      bmp3.Free;
      bmp4.Free;
    end;
  end;
end;


(* var
  bmp32 : TGLBitmap32;
  SprMat:TGLLibMaterial;//TGLLibMaterial


   bmp32:=TGLBitmap32.Create;
   bmp32.Width:=128;
   bmp32.Height:=128;
//GPFNodes.CCubes[PfxCSOnlyOne].Sprites[Tempj].Position[0];
//        spr.Material.MaterialLibrary:=GLCloudsMatLib;
//        spr.Material.LibMaterialName:=;
Token:=Random(GPFNodes.CCubes[PfxCSOnlyOne].SpriteCount-1);
//Only called once ? for ALL the particles!..even after different loads.
//showmessage(inttostr(GPFNodes.CCubes[PfxCSOnlyOne].SpriteCount)+' : '+inttostr(Token));
SprMat:=
GLCloudsMatLib.LibMaterialByName(GPFNodes.CCubes[PfxCSOnlyOne].Sprites[Token].TextureName);
  bmp32.Assign(
   //GLCloudsMatLib.Materials[0].Material
   SprMat.Material.Texture.Image.GetBitmap32(GL_TEXTURE_2D) );
   bmp32.SetAlphaFromIntensity;
   destBmp32.Assign(bmp32);



    //PLMask
    PLPSize,//StrtoFloatDef(PLPSizeEdit.Text,0.3);
    PLPLife, //StrtoFloatDef(PLPLifeEdit.Text,1.0);
    PLPInterval,   //StrtoFloatDef(PLPIntervalEdit.Text,0.003);
    PLDepth,        //StrToIntDef(PLDepthEdit.Text, 3);
    PLScale : Double;     //StrToFloatDef(PLScaleEdit.Text, 1);
    PLMaskString:String;   // PLCharEdit.Text;
    PLMaskStringSize,     //StrToIntDef(PLCharacterSizeEdit.Text, 16);
    PLMaskType ,      //PLMaskTypeRG.Itemindex
//GPFNodes.CCubes[i].PLMaskImageSize:=PLMaskImageSizeTB.Position;
    PLMaskImageSize,//PLMaskImageSizeTB.Position;
    PLMaskImageSizeChange(NewSize:Integer);
    PLMaskXOffset           
  LabelX.Caption:=Inttostr(PLMaskMaskXOffsetTB.Position);
    PLMaskZOffset
  LabelZ.Caption:=Inttostr(PLMaskMaskZOffsetTB.Position);
    PLMaskYOffset
  LabelY.Caption:=Inttostr(PLMaskMaskYOffsetTB.Position);

    PLMaskMenuNumber: Integer;  //PLMaskMenuNumber:=TComponent(Sender).Tag;
    0 is nil             //PLMaskMenuSet(TComponent(Sender).Tag);
                           {PLMaskMenuSet(MaskMenuNumber:Integer);}
procedure TACloudDemoForm.PLMaskMenuSet(MaskMenuNumber,
            XOffset,YOffset,ZOffset,ImageSize:Integer);
PLMaskMenuNumberStored   is used to keep it around until Added
    PLColor:TColor; //PFXPLMaskManager.ColorOuter.AsWinColor:= PLColorPanel.Color;
    PLXImageFileName :String; //PlLoadXBtn.Caption:=OpenDialog1.FileName;
    PLPitch,  //StrToFloatDef(PLPitchEdit.Text, -90);
    PLRoll,   //StrToFloatDef(PLRollEdit.Text, 0);
    PLTurn: Double;  //StrToFloatDef(PLTurnEdit.Text, 0);

*)


end.
