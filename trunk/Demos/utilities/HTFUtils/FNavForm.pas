unit FNavForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, G32_Image, HeightTileFile, G32, G32_Layers;

type
  TNavForm = class(TForm)
    Image: TImage32;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    { Private declarations }
    FPickX, FPickY : Integer;
  public
    { Public declarations }
    function Execute(htf : THeightTileFile) : Boolean;

    property PickX : Integer read FPickX;
    property PickY : Integer read FPickY;
  end;

var
  NavForm: TNavForm;

implementation

{$R *.dfm}

uses FViewerForm;

function TNavForm.Execute(htf : THeightTileFile) : Boolean;
var
   x, y, w, s, wx, wy : Integer;
   tileInfo : PHeightTileInfo;
begin
   // Computes scaling so that preview window isn't too small
   with htf do begin
      wx:=(SizeX+TileSize div 2) div TileSize;
      wy:=(SizeY+TileSize div 2) div TileSize;
   end;
   if wx<wy then
      w:=wy
   else w:=wx;
   s:=1;
   while w<256 do begin
      w:=w*2;
      s:=s*2;
   end;
   Image.Scale:=s;
   // Prepare the world tile map
   with Image.Bitmap do begin
      Width:=wx;
      Height:=wy;
      for y:=0 to wy-1 do begin
         for x:=0 to wx-1 do begin
            tileInfo:=htf.XYTileInfo(x*htf.TileSize, y*htf.TileSize);
            if Assigned(tileInfo) then begin
               Pixel[x, y]:=HeightToColor(tileInfo.average)
            end else Pixel[x, y]:=clGray32;
         end;
      end;
   end;
   // Couldn't get the form's AutoSize to work...
   Image.Width:=wx*s;
   Image.Height:=wy*s;
   Width:=Image.Width;
   Height:=Image.Height;
   // Show the Nav map
   Result:=(ShowModal=mrOk);
   // Convert back to world coordinates
   if Result then begin
      FPickX:=(FPickX*htf.TileSize) div s - htf.TileSize;
      FPickY:=(FPickY*htf.TileSize) div s - htf.TileSize;
   end;
end;

procedure TNavForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
   FPickX:=X;
   FPickY:=Y;
   ModalResult:=mrOk;
end;

end.
