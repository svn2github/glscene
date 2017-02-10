unit fZoomImage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, ExtCtrls, uNeatClasses, StdCtrls, Menus;

const
  cFastDrawSkip = 8;

type
  TfrmZoomImage = class(TForm)
    Button_Redraw: TButton;
    PopupMenu_Zoomed: TPopupMenu;
    SetResolution1: TMenuItem;
    N640x4801: TMenuItem;
    N800x6001: TMenuItem;
    N1024x7681: TMenuItem;
    N1280x10241: TMenuItem;
    SaveImage1: TMenuItem;
    Panel1: TPanel;
    Shape1: TShape;
    ScrollBox1: TScrollBox;
    Image: TImage32;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_RedrawClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetResolution_Click(Sender: TObject);
    procedure SaveImage1Click(Sender: TObject);
  private
    { Private declarations }
    FGenotype : TGenotype;
    procedure RenderImage(const ARenderSkip : integer);
  public
    { Public declarations }
    procedure ShowAndRender(const AGenotype : TGenotype);
  end;

implementation

uses fGeneticArt, uNEATStrFunctions;

{$R *.dfm}

{ TfrmZoomImage }

procedure TfrmZoomImage.ShowAndRender(const AGenotype: TGenotype);
begin
  FGenotype := TGenotype.Create(frmGeneticArt.NEATPopulation);
  AGenotype.CopyTo(FGenotype);
  FGenotype.PreparePhenotype;

  Show;
  Button_RedrawClick(nil);
  {RenderImage(cFastDrawSkip);
  Application.ProcessMessages;
  RenderImage(1);//}
end;

procedure TfrmZoomImage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmZoomImage.RenderImage(const ARenderSkip : integer);
begin
  Screen.Cursor := crHourGlass;
  try
    Image.Bitmap.Width := Image.ClientWidth;
    Image.Bitmap.Height := Image.ClientHeight;
    Image.Bitmap.Clear(clBlack);

    frmGeneticArt.RenderGenotype(FGenotype, Image.Bitmap, ARenderSkip);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmZoomImage.Button_RedrawClick(Sender: TObject);
begin
  RenderImage(cFastDrawSkip);
  Application.ProcessMessages;
  RenderImage(1);
end;

procedure TfrmZoomImage.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGenotype);
end;

procedure TfrmZoomImage.SetResolution_Click(Sender: TObject);
var
  w, h : integer;
begin
  w := StrToInt(ReplaceStr('&', '', GetBefore('x', TMenuItem(Sender).Caption)));
  h := StrToInt(GetAfter('x', TMenuItem(Sender).Caption));

  Image.ClientWidth := w;
  Image.ClientHeight := h;

  RenderImage(cFastDrawSkip);
  Application.ProcessMessages;
  RenderImage(1);
end;

procedure TfrmZoomImage.SaveImage1Click(Sender: TObject);
begin
  frmGeneticArt.SaveImage(Image.Bitmap);
end;

end.
