{
  Created by Lucas R. Goraieb
  01/19/2004
}

unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes, System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ExtDlgs,
  Vcl.Imaging.Jpeg,

  //Graphics32
  GR32_Image,GR32,
  GR32_Filters,
  GR32_RangeBars,
  GR32_Blend,GR32_Layers,

  //GLS
  OpenGLTokens,
  GLScene,
  GLGraph,
  GLObjects,
  GLWin32Viewer,
  GLCrossPlatform,
  GLVectorGeometry,
  GLVectorTypes,
  GLTexture,
  GLGraphics,
  GLCadencer,
  GLCoordinates,
  GLBaseClasses;

const
  TextureScaleFrac = 102.4;

type
  TGLTerrainBrush = class(TBitmap32)
  private
    FTextureIndex: Integer;
    FBrushIndex: Integer;
    FAlpha: Cardinal;
    FSize: Integer;
    FTextureList: TBitmap32List;
    FBrushList: TBitmap32List;
  public
    procedure DrawToTexture(X,Y: Integer; Texture: TBitmap32);
  published
    property TextureIndex: Integer read FTextureIndex write FTextureIndex default 0;
    property BrushIndex: Integer read FBrushIndex write FBrushIndex default 0;
    property Alpha: Cardinal read FAlpha write FAlpha default 255;
    property Size: Integer read FSize write FSize default 32;
    property TextureList: TBitmap32List read FTextureList write FTextureList;
    property BrushList: TBitmap32List read FBrushList write FBrushList;
  end;

  TGLTerrainTextureRes = (tr128, tr256, tr512, tr1024, tr2048);

  TGLTerrainPainter = class(TComponent)
  private
    FTextureBuffer: TBitmap32;
    FTextureResolution: TGLTerrainTextureRes;
    FTextureSize: Cardinal;
    FTextureUpdated: Boolean;
    FBrush: TGLTerrainBrush;
    FTextureList: TBitmap32List;
    FBrushList: TBitmap32List;
    procedure LoadFromPath(Path: String; var BmpList: TBitmap32List);
    procedure SetTextureSize(Value: TGLTerrainTextureRes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Load all textures from texture directory
    procedure LoadTextures(Path: String);
    //Load all brushes from its directory
    procedure LoadBrushes(Path: String);
    //Update UV coordinates of the height field
    procedure UpdateHeightField(var HeightField: TGLHeightField);
    //Draw to texture buffer
    function Draw(X,Y: Integer;ViewerBuffer: TGLSceneBuffer;var HeightField: TGLHeightField): TPoint;
    //Apply final texture to heightfield
    procedure ApplyTexture(var HeightField: TGLHeightField);
    property TextureList: TBitmap32List read FTextureList write FTextureList;
    property BrushList: TBitmap32List read FBrushList write FBrushList;
  published
    property TextureBuffer: TBitmap32 read FTextureBuffer;
    property TextureSize: Cardinal read FTextureSize;
    property TextureResolution: TGLTerrainTextureRes read FTextureResolution write SetTextureSize;
    property Brush: TGLTerrainBrush read FBrush write FBrush;
  end;

  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    pbTexture: TPaintBox32;
    gbTexture: TGaugeBar;
    pbBrush: TPaintBox32;
    gbBrush: TGaugeBar;
    gbGrid: TGaugeBar;
    lbGridSize: TLabel;
    gbBrushSize: TGaugeBar;
    lbBrushSize: TLabel;
    gbAlpha: TGaugeBar;
    lbAlpha: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    GLScene1: TGLScene;
    TerrainViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLCamTarget: TGLDummyCube;
    HeightField: TGLHeightField;
    GLLightSource1: TGLLightSource;
    Memo1: TMemo;
    rgTexSize: TRadioGroup;
    GLCadencer1: TGLCadencer;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    dSave: TSavePictureDialog;
    dLoad: TOpenPictureDialog;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbTextureChange(Sender: TObject);
    procedure gbBrushChange(Sender: TObject);
    procedure gbGridChange(Sender: TObject);
    procedure gbBrushSizeChange(Sender: TObject);
    procedure gbAlphaChange(Sender: TObject);
    procedure TerrainViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HeightFieldGetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure TerrainViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure rgTexSizeClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    TerrainPainter: TGLTerrainPainter;
    //Return the application path
    function ExePath: String;
  end;

var
  Form1: TForm1;

const
  path_Textures = '..\Textures\';
  path_Brushes = '..\Brushes\';

implementation

{$R *.dfm}

{TGLTerrainBrush}

procedure TGLTerrainBrush.DrawToTexture(X,Y: Integer; Texture: TBitmap32);
var
  P: TPoint;
  TileX,TileY, TileHeight,TileWidth: Integer;
  Tmp: TBitmap32;
begin
  SetSize(FSize,FSize);
  MasterAlpha := FAlpha;
  DrawMode := dmBlend;
  //FTextureList.Bitmap[FTextureIndex].DrawTo(Self,0,0);

  //Tile
  TileWidth := FTextureList.Bitmap[FTextureIndex].Width;
  TileHeight := FTextureList.Bitmap[FTextureIndex].Height;

  TileY := -Y;
  while (TileY < FSize) do
  begin
    TileX := -X;
    while (TileX < FSize) do
    begin
      FTextureList.Bitmap[FTextureIndex].DrawTo(Self,TileX,TileY);
      Inc(TileX, TileWidth);
    end;
    Inc(TileY, TileHeight);
  end;//}

  Tmp := TBitmap32.Create;
  try
    Tmp.SetSize(Width, Height);
    FBrushList.Bitmap[FBrushIndex].DrawTo(Tmp, Rect(0, 0, Tmp.Width, Tmp.Height));

    // combine Alpha into already loaded RGB colors
    IntensityToAlpha(Self, Tmp);
  finally
     Tmp.Free;
  end;

  P.X := X - (Width div 2);
  P.Y := Y - (Height div 2);
  Texture.Draw(P.x,P.y,Self);
end;

{TGLTerrainPainter}

constructor TGLTerrainPainter.Create(AOwner: TComponent);
begin
  //Create
  FTextureBuffer := TBitmap32.Create;
  FTextureList := TBitmap32List.Create(Self);
  FBrushList := TBitmap32List.Create(Self);
  FBrush := TGLTerrainBrush.Create;
  //Update
  SetTextureSize(tr1024); //1024 x 1024 pixels
  FTextureUpdated := False;
  FBrush.TextureList := FTextureList;
  FBrush.BrushList := FBrushList;

  Inherited Create(AOwner);
end;

destructor TGLTerrainPainter.Destroy;
begin
  FTextureBuffer.Free;
  FTextureList.Free;
  FBrushList.Free;
  FBrush.Free;
  inherited DEstroy;
end;

procedure TGLTerrainPainter.LoadFromPath(Path: String; var BmpList: TBitmap32List);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  Ext: String;
begin
  FileAttrs := faAnyFile - faDirectory;
  if FindFirst(Path+'*.*', FileAttrs, sr) = 0 then
  begin
    repeat
      Ext := ExtractFileExt(sr.Name);
      if (Ext = '.bmp') or (Ext = '.jpg') then
        with BmpList.Bitmaps.Add do
          Bitmap.LoadFromFile(Path+sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TGLTerrainPainter.SetTextureSize(Value: TGLTerrainTextureRes);
begin
  FTextureResolution := Value;
  FTextureSize := 512;
  case FTextureResolution of
    tr128: FTextureSize := 128;
    tr256: FTextureSize := 256;
    tr512: FTextureSize := 512;
    tr1024: FTextureSize := 1024;
    tr2048: FTextureSize := 2048;
  end;
  FTextureBuffer.SetSize(FTextureSize,FTextureSize);
end;

procedure TGLTerrainPainter.LoadTextures(Path: String);
begin
  LoadFromPath(Path,FTextureList);
  //gbTexture.Max := Bitmap.Bitmaps.Count-1;
  //gbTexture.Position := 0;
end;

procedure TGLTerrainPainter.LoadBrushes(Path: String);
begin
  LoadFromPath(Path,FBrushList);
  //gbBrush.Max := bmpBrush.Bitmaps.Count-1;
  //gbBrush.Position := 0;
end;

procedure TGLTerrainPainter.UpdateHeightField(var HeightField: TGLHeightField);
var
  TerrainSizeX, TerrainSizeY: Single;
begin
  with HeightField.Material.Texture do begin
     Disabled := False;
     MappingMode:=tmmObjectLinear;
     TerrainSizeX := 1 / HeightField.XSamplingScale.Max-HeightField.XSamplingScale.Min;
     TerrainSizeY := 1 / HeightField.YSamplingScale.Max-HeightField.YSamplingScale.Min;
     MappingSCoordinates.AsVector:=VectorMake(TerrainSizeX, 0, 0, 0);
     MappingTCoordinates.AsVector:=VectorMake(0, -TerrainSizeY, 0, 0);
     TextureMode := tmModulate;
  end;
end;

function TGLTerrainPainter.Draw(X,Y: Integer; ViewerBuffer: TGLSceneBuffer; var HeightField: TGLHeightField): TPoint;
var
   v : TAffineVector;
   ix, iy : Integer;
   TerrainSizeX, TerrainSizeY: Single;
begin
  // get absolute 3D coordinates of the point below the mouse
  v:=ViewerBuffer.PixelRayToWorld(x, y);
  // convert to heightfield local coordinates
  v:=HeightField.AbsoluteToLocal(v);
  // convert that local coords to texture pos
  TerrainSizeX := TextureScaleFrac / (HeightField.XSamplingScale.Max-HeightField.XSamplingScale.Min);
  TerrainSizeY := TextureScaleFrac / (HeightField.YSamplingScale.Max-HeightField.YSamplingScale.Min);
  ix:=Round(v.X* TerrainSizeX * (FTextureSize / TextureScaleFrac));
  iy:=Round(v.Y* TerrainSizeY * (FTextureSize / TextureScaleFrac));
  result := Point(ix,iy);
  // if we are in the texture...
  if (ix >= 0) and (iy >= 0) then
  begin
   FBrush.DrawToTexture(ix,iy,FTextureBuffer);
   FTextureUpdated := True;
  end;
end;

procedure TGLTerrainPainter.ApplyTexture(var HeightField: TGLHeightField);
begin
  if not FTextureUpdated then Exit;
  FTextureUpdated := False;
  with HeightField.Material.Texture.Image do
  begin
    GetBitmap32.Assign(FTextureBuffer);
    NotifyChange(self);
  end;
end;

{TForm1}

function TForm1.ExePath: String;
begin
  result := ExtractFilePath(Application.ExeName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExePath);

  TerrainPainter := TGLTerrainPainter.Create(Self);
  with TerrainPainter do
  begin
    LoadTextures(path_Textures);
    LoadBrushes(path_Brushes);
    UpdateHeightField(HeightField);
  end;

  gbTexture.Max := TerrainPainter.TextureList.Bitmaps.Count-1;
  gbTexture.Position := 0;
  gbBrush.Max := TerrainPainter.TextureList.Bitmaps.Count-1;
  gbBrush.Position := 0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Update the brush
  gbTextureChange(Self);
  gbBrushChange(Self);
  TerrainPainter.Brush.Alpha := gbAlpha.Position;
  TerrainPainter.Brush.Size := gbBrushSize.Position;
end;

procedure TForm1.gbTextureChange(Sender: TObject);
begin
  //Show current texture
  pbTexture.Buffer.Clear(clBlack);
  if (gbTexture.Position > -1) and (gbTexture.Position < TerrainPainter.TextureList.Bitmaps.Count) then
  begin
    pbTexture.Buffer.Draw(0,0,TerrainPainter.TextureList.Bitmap[gbTexture.Position]);
    pbTexture.Invalidate;
    TerrainPainter.Brush.TextureIndex := gbTexture.Position;
  end;
end;

procedure TForm1.gbBrushChange(Sender: TObject);
var X,Y: Integer;
begin
  //Show current brush
  pbBrush.Buffer.Clear(clBlack);
  if (gbBrush.Position > -1) and (gbBrush.Position < TerrainPainter.BrushList.Bitmaps.Count) then
  begin
    X := (pbBrush.Width-TerrainPainter.BrushList.Bitmap[gbBrush.Position].Width) div 2;
    Y := (pbBrush.Height-TerrainPainter.BrushList.Bitmap[gbBrush.Position].Height) div 2;
    pbBrush.Buffer.Draw(x,y,TerrainPainter.BrushList.Bitmap[gbBrush.Position]);
    pbBrush.Invalidate;
    TerrainPainter.Brush.BrushIndex := gbBrush.Position;
  end;
end;

procedure TForm1.gbGridChange(Sender: TObject);
begin
  //New terrain size
  lbGridSize.Caption := 'Terrain Size = '+IntToStr(gbGrid.Position);
  HeightField.XSamplingScale.Max := gbGrid.Position;
  HeightField.YSamplingScale.Max := gbGrid.Position;
  HeightField.Position.X := -(gbGrid.Position div 2);
  HeightField.Position.Z := (gbGrid.Position div 2);
  //Update
  TerrainPainter.UpdateHeightField(HeightField);
end;

procedure TForm1.gbBrushSizeChange(Sender: TObject);
begin
  lbBrushSize.Caption := 'Brush Size = '+IntToStr(gbBrushSize.Position);
  TerrainPainter.Brush.Size := gbBrushSize.Position;
end;

procedure TForm1.gbAlphaChange(Sender: TObject);
begin
  lbAlpha.Caption := 'Alpha = '+IntToStr(gbAlpha.Position);
  TerrainPainter.Brush.Alpha := gbAlpha.Position;
end;

procedure TForm1.TerrainViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = TMouseButton.mbRight) then
   begin
     mx:=x;
     my:=y;
   end;

   if (Button = TMouseButton.mbLeft) then
   begin
     //Paint
     TerrainPainter.Draw(x,y,TerrainViewer.Buffer,HeightField);
   end;
end;

procedure TForm1.TerrainViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //Drag paint or camera move
  if (ssRight in Shift) then begin
     GLCamera.MoveAroundTarget(my-y, mx-x);
     mx:=x;
     my:=y;
  end;
  if ssLeft in Shift then
     TerrainViewerMouseDown(Sender, TMouseButton.mbLeft, Shift, x, y)
end;

procedure TForm1.HeightFieldGetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  //Set terrain elevation
  z:=Cos(VectorLength(x, y)*1.5)*0.5;
end;

procedure TForm1.rgTexSizeClick(Sender: TObject);
begin
  case rgTexSize.ItemIndex of
    0: TerrainPainter.TextureResolution := tr1024;
    1: TerrainPainter.TextureResolution := tr512;
    2: TerrainPainter.TextureResolution := tr256;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  TerrainPainter.ApplyTexture(HeightField);
  TerrainViewer.Invalidate;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if dSave.Execute then
    TerrainPainter.TextureBuffer.SaveToFile(dSave.FileName);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if dLoad.Execute then
  begin
    TerrainPainter.TextureBuffer.LoadFromFile(dLoad.FileName);
    gbGridChange(Self);
    TerrainPainter.FTextureUpdated := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  caption := Format('3D Terrain Painter - FPS: %f',[TerrainViewer.FramesPerSecond]);
  TerrainViewer.ResetPerformanceMonitor;
end;

end.
