unit glAvi;

interface

uses
  Winapi.Windows,
  System.Classes,
//  System.Math,
  Vcl.Graphics,
  Vcl.Dialogs,

  GLScene,
  GLVectorGeometry,
  XOpenGL,
  OpenGLTokens,
  OpenGLAdapter,
  GLContext,
  GLTexture,
  GLSVFW,
  GLGraphics,
  GLRenderContextInfo,
  GLCrossPlatform,
  GLHUDObjects,
  GLCoordinates,
  GLBaseClasses;

type

  TAVIUpdateEvent = procedure(sender: TObject; FrameIndex: integer) of object;

  TGLAvi = class
  private
    fFilename: string;
    currdelta: single;
    fFrameindex, fFirstFrame, fLastFrame: integer;
    fUpdateRate: integer;
    PAvi: IAVIFile;
    PAvis: IAVIStream;
    AviInfo: TAVISTREAMINFOA;       //TAVISTREAMINFOA;
    pFrame: IGetFrame;
    pBmi: PBitmapInfoHeader;
    pColors: pRGBTriple;
    fAviOpened: boolean;
    CurrTime: Double;
    CurrFrameCount: integer;
    FileOrPosChanged: boolean;

    fwidth, fheight: single;

    fCurrentFrameRate, FUserFrameRate: integer;
    fTargetFrameRate: single;
    fAutoFrameRate: boolean;
    fOnUpdate: TAVIUpdateEvent;
    fTexture: TGLTexture;
    fBmp: TBitmap;
    fDx, fDy: Word;
    function OpenAvi: boolean;
    procedure ReadToTexture;
  protected
    procedure SetFilename(val: string);
    procedure SetTexture(tex: TGLTexture);
    procedure SetFrameIndex(val: integer);
    procedure SetAutoFrameRate(val: boolean);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure update(ct, dt: single);
    procedure CloseAvi;
    function GetFrame(f: integer): boolean;
    property UserFrameRate: integer read FUserFrameRate write FUserFrameRate;
    property FrameIndex: longint read fFrameindex write SetFrameIndex;
    property FirstFrame: longint read fFirstFrame write fFirstFrame;
    property LastFrame: longint read fLastFrame write fLastFrame;
    property Filename: string read fFilename write SetFilename;
    property Texture: TGLTexture read fTexture write SetTexture;
    property AviOpened: boolean read fAviOpened;
    property width: single read fwidth;
    property height: single read fheight;
    property CurrentFrameRate: integer read fCurrentFrameRate;
    // current number of frames shown per second
    property TargetFrameRate: single read fTargetFrameRate;
    // Actual avi file frame rate
    property AutoFrameRate: boolean read fAutoFrameRate write SetAutoFrameRate;
    // Ignores UserFrameRate and uses TargetFrameRate instead
    property OnUpdate: TAVIUpdateEvent read fOnUpdate write fOnUpdate;
  end;

//===================================================================
implementation
//===================================================================

procedure TGLAvi.SetAutoFrameRate(val: boolean);
begin

  fAutoFrameRate := val;
  FileOrPosChanged := true;

end;

procedure TGLAvi.SetFrameIndex(val: integer);
begin
  fFrameindex := val;
  FileOrPosChanged := true;
end;

procedure TGLAvi.SetFilename(val: string);
begin
  fFilename := val;
  OpenAvi;
end;

procedure TGLAvi.SetTexture(tex: TGLTexture);
begin
  fTexture := tex;
end;

constructor TGLAvi.Create;
begin
  inherited;
  fFrameindex := 0;
  fFirstFrame := 0;
  fLastFrame := 0;
  fUpdateRate := 10;
  pBmi := nil;
  pColors := nil;
  fCurrentFrameRate := 0;
  CurrTime := 0;
  CurrFrameCount := 0;
  fTargetFrameRate := 0;
  fAutoFrameRate := true;

end;

destructor TGLAvi.Destroy;
begin
  fBmp.Free;
  inherited;
end;

procedure TGLAvi.update(ct, dt: single);
var
  t: single;
begin

  inherited;

  if fAutoFrameRate then
    t := fTargetFrameRate
  else
    t := FUserFrameRate;

  if (t = 0) or (not fAviOpened) then
    exit;

  currdelta := currdelta + dt;

  if (currdelta >= 1 / t) or FileOrPosChanged then
  begin
    currdelta := currdelta - 1 / t;
    GetFrame(fFrameindex);
    ReadToTexture;
    inc(fFrameindex);
    if fFrameindex > fLastFrame then
      fFrameindex := fFirstFrame;
    CurrFrameCount := CurrFrameCount + 1;
    if (ct >= CurrTime + 1) or FileOrPosChanged then
    begin
      if FileOrPosChanged then
      begin
        CurrTime := ct;
        currdelta := 0;
        FileOrPosChanged := false;
      end
      else
        CurrTime := CurrTime + 1;

      fCurrentFrameRate := CurrFrameCount;
      CurrFrameCount := 0;
      if assigned(fOnUpdate) then
        fOnUpdate(self, fFrameindex);
    end;
  end;
end;

function TGLAvi.GetFrame(f: integer): boolean;
var
  tbmi: PBitmapInfoHeader;
begin

  if not fAviOpened then
  begin
    result := false;
    exit;
  end;

  try
    tbmi := AVIStreamGetFrame(pFrame, f);
    result := assigned(tbmi);
    if result then
    begin
      pBmi := tbmi;
      pColors := pRGBTriple(DWORD(pBmi) + pBmi^.bisize);
    end;
  except
    result := false;
  end;

end;

function TGLAvi.OpenAvi: boolean;
var
  m: integer;
begin
  fFirstFrame := 0;
  fFrameindex := 0;
  fLastFrame := 0;
  result := AVIFileOpen(PAvi, PChar(fFilename), OF_READ, nil) = AVIERR_OK;
  if not result then
    exit;
  result := AVIFILEGetStream(PAvi, PAvis, streamtypeVIDEO, 0) = AVIERR_OK;
  if not result then
    exit;
  result := AVIStreamInfoA(PAvis, AviInfo, sizeof(TAVISTREAMINFOA)) = AVIERR_OK;
  if not result then
    exit;

  if AviInfo.dwRate > 1000 then
  begin
    if AviInfo.dwRate > 100000 then
      m := 10000
    else if AviInfo.dwRate > 10000 then
      m := 1000
    else
      m := 100;
  end
  else
    m := 1;

  fTargetFrameRate := AviInfo.dwRate / m;
  fFirstFrame := AVIStreamStart(PAvis);
  fLastFrame := AVIStreamENd(PAvis);
  pFrame := AVIStreamGetFrameOpen(PAvis, nil);
  result := assigned(pFrame);
  AVIStreamBeginStreaming(PAvis, fFirstFrame, fLastFrame, 1000);
  if not result then
  begin
    CloseAvi;
    showMessage('Codec not found.');
  end
  else
  begin
    fAviOpened := true;
    GetFrame(fFirstFrame);
    currdelta := 0;
    CurrFrameCount := 0;
    FileOrPosChanged := true;
  end;
end;

procedure TGLAvi.CloseAvi;
begin
  AVIStreamEndStreaming(PAvis);
  AVIStreamGetFrameClose(pFrame);
  AVIStreamrelease(PAvis);
  if assigned(pBmi) then
  begin
    pBmi := nil;
    pColors := nil;
  end;
  fFirstFrame := 0;
  fFrameindex := 0;
  fLastFrame := 0;
  fAviOpened := false;
end;

procedure TGLAvi.ReadToTexture;
var
  a1, a2, wi, hi: integer;
  p: PByteArray;
  rgb: pRGBTriple;
begin
  if fFilename = '' then
    exit;
  if fBmp = nil then
  begin
    fBmp := TBitmap.Create;
    fBmp.PixelFormat := pf24bit;
  end;

  wi := Ceil(Logarithm2(pBmi^.biWidth));
  hi := Ceil(Logarithm2(pBmi^.biHeight));
  with fBmp do
    if (width * height = 0) or (width <> 1 shl wi) or (height <> 1 shl hi) then
    begin
      width := 1 shl wi;
      height := 1 shl hi;
      for a2 := height - 1 downto 0 do
      begin
        p := fBmp.ScanLine[a2];
        for a1 := 0 to width * 3 - 1 do
          p[a1] := 0;
      end;
      fDx := (width - pBmi^.biWidth) div 2;
      fDy := (height - pBmi^.biHeight) div 2;
    end;

  rgb := pColors;
  for a2 := pBmi^.biHeight - 1 downto 0 do
  begin
    p := fBmp.ScanLine[fDy + a2];
    System.move(rgb^, p^[fDx * 3], pBmi^.biWidth * 3);
    inc(rgb, pBmi^.biWidth);
  end;
  fTexture.Image.Assign(fBmp);
end;

initialization

   AVIFileInit;

finalization

   AVIFileExit;

end.
