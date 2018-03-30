unit OffSetAnim;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Dialogs,

  GLVectorTypes,
  GLCoordinates,
  GLTexture, GLMaterial;

type
  TAOffSet = record
    x, y: Single;
  end;

  TOffSetAnim = class
  private
    OffSets: Array of TAOffSet;
    ticker, qtick: double;
    ScaleX, ScaleY: Single;
    C, R: Integer;
    Lib: TGLMaterialLibrary;
    AnimReady: Boolean;
  public
    FPS: Integer;
    Bind: Integer;
    Mode: Integer;
    CFrame: Integer;
    procedure MakeAnim(TileWidth: Integer; TileHeight: Integer; sBind: Integer;
      src: TGLMaterialLibrary; FrameRate: Integer; pm: Integer);
    procedure NextFrame;
    procedure SetFrame(Frame: Integer);
    procedure Tick(Time: double);
    Constructor Create;
    Destructor Destroy; Override;
  end;

const
  apmNone = 111;
  apmLoop = 112;
  apmOnce = 113;

implementation

Constructor TOffSetAnim.Create;
begin
  // initialise clock
  qtick := ticker;
  AnimReady := False;
end;

Destructor TOffSetAnim.Destroy;
begin
  //
end;

procedure TOffSetAnim.Tick(Time: double);
begin
  ticker := Time;
  if ticker - qtick > 1 / FPS then
  begin
    qtick := ticker;
    //
    if (AnimReady = True) and (Mode <> apmNone) then
      NextFrame;
  end;
end;

procedure TOffSetAnim.MakeAnim(TileWidth: Integer; TileHeight: Integer;
  sBind: Integer; src: TGLMaterialLibrary; FrameRate: Integer; pm: Integer);
var
  i, i2, o: Integer;
  x, y: Single;
begin
  try
    AnimReady := False;
    FPS := FrameRate;
    Bind := sBind;
    Lib := (src as TGLMaterialLibrary);
    C := Lib.Materials[Bind].Material.Texture.Image.Width div TileWidth;
    R := Lib.Materials[Bind].Material.Texture.Image.Height div TileHeight;
    ScaleX := 1 / C;
    ScaleY := 1 / R;
    x := 0;
    y := ScaleY * R - 1;
    for i := 0 to R - 1 do
    begin
      for i2 := 0 to C - 1 do
      begin
        SetLength(OffSets, Length(OffSets) + 1);
        o := Length(OffSets) - 1;
        OffSets[o].x := x;
        OffSets[o].y := y;
        x := x + ScaleX;
      end;
      x := 0;
      y := y - ScaleY;
    end;
    Lib.Materials[Bind].TextureScale.x := ScaleX;
    Lib.Materials[Bind].TextureScale.y := ScaleY;
    Lib.Materials[Bind].TextureOffset.y := ScaleY * R - 1;
    Mode := pm;
    AnimReady := True;
  except
    SetLength(OffSets, 0);
    ShowMessage('MakeAnim failed... Make sure your texture is Power of 2.');
  end;
end;

procedure TOffSetAnim.NextFrame;
begin
  Lib.Materials[Bind].TextureOffset.x := OffSets[CFrame].x;
  Lib.Materials[Bind].TextureOffset.y := OffSets[CFrame].y;
  if Mode = apmLoop then
  begin
    if CFrame = Length(OffSets) then
      CFrame := 0
    else
      Inc(CFrame);
  end
  else
  begin
    if CFrame <> Length(OffSets) then
      Inc(CFrame);
  end;
end;

procedure TOffSetAnim.SetFrame(Frame: Integer);
begin
  if (Frame >= 0) and (Frame < Length(OffSets)) then
  begin
    CFrame := Frame;
    Lib.Materials[Bind].TextureOffset.x := OffSets[CFrame].x;
    Lib.Materials[Bind].TextureOffset.y := OffSets[CFrame].y;
  end;
end;

end.
