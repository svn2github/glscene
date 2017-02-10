unit fGodMode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GR32_Image, uNeatClasses, uDrawGenotype, StdCtrls,
  mmsystem;

const
  cFullRenderWaitPeriod = 500;
  cFastRenderWaitPeriod = 50;

type
  TfrmGodMode = class(TForm)
    Image: TImage32;
    Image1: TImage;
    ScrollBox_LinkSliders: TScrollBox;
    Shape1: TShape;
    Memo_GenotypeCode: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Timer_RedrawEvent: TTimer;
    Button_Zoom: TButton;
    procedure ScrollBar_Change(Sender: TObject);
    procedure Timer_RedrawEventTimer(Sender: TObject);
    procedure Button_ZoomClick(Sender: TObject);
  private
    { Private declarations }
    FGenotype: TGenotype;
    FChangedAt : cardinal;
    FFastDrawn : boolean;

    procedure RenderImage(const ARenderSkip : integer=1);
    procedure ShowGenotype;
    procedure CreateLinkSliders;

  public
    procedure ShowAndModify(const AGenotype : TGenotype);

  public // Properties
    property Genotype : TGenotype read FGenotype;
  end;

var
  frmGodMode: TfrmGodMode;

implementation

uses fGeneticArt, fZoomImage, MAth;

{$R *.dfm}

{ TfrmGodMode }

procedure TfrmGodMode.CreateLinkSliders;
var
  i : integer;
  Connect : TConnect;
  NextTop : integer;
begin
  NextTop := 4;
  for i := 0 to FGenotype.ConnectList.Count-1 do
  begin
    Connect := FGenotype.ConnectList[i];
    if not Connect.Enabled then
      continue;

    with TLabel.Create(ScrollBox_LinkSliders) do
    begin
      Parent := ScrollBox_LinkSliders;
      Top := NextTop;
      Left := 8;
      Caption := Format('Link %d',[Connect.InnovationID]);

      NextTop := NextTop + 16;
    end;

    with TScrollBar.Create(ScrollBox_LinkSliders) do
    begin
      Parent := ScrollBox_LinkSliders;
      Left := 8;
      Top := NextTop;
      Width := ScrollBox_LinkSliders.Width-32;
      Height := 17;
      Max := 300;
      Min := -300;
      PageSize := 0;
      Tag := integer(Connect);
      OnChange := ScrollBar_Change;
      Position := Math.Max(-300, Math.Min(300, Trunc(Connect.Weight*100)));

      NextTop := NextTop + 24;
    end;
  end;
end;

procedure TfrmGodMode.RenderImage(const ARenderSkip : integer);
begin
  Screen.Cursor := crHourGlass;
  try
    Image.Bitmap.Width := Image.Width;
    Image.Bitmap.Height := Image.Height;
    Image.Bitmap.Clear(clBlack);

    frmGeneticArt.RenderGenotype(FGenotype, Image.Bitmap, ARenderSkip);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGodMode.ShowAndModify(const AGenotype: TGenotype);
begin
  FGenotype := AGenotype;
  // RenderImage;
  ShowGenotype;

  CreateLinkSliders;

  ShowModal;
end;

procedure TfrmGodMode.ShowGenotype;
begin
  DrawGenotype(FGenotype, Image1.Canvas, false, true, false, false);
  Memo_GenotypeCode.Lines.Text := FGenotype.SaveToString;
end;

procedure TfrmGodMode.ScrollBar_Change(Sender: TObject);
begin
  FChangedAt := timeGetTime;
  TConnect(TScrollBar(Sender).Tag).Weight := TScrollBar(Sender).Position/100;
  RenderImage(8);
  FFastDrawn := false;
  ShowGenotype;
  Timer_RedrawEvent.Enabled := true;
end;

procedure TfrmGodMode.Timer_RedrawEventTimer(Sender: TObject);
begin
  if (timeGetTime-FChangedAt)>cFullRenderWaitPeriod then
  begin
    Timer_RedrawEvent.Enabled:= false;
    RenderImage;
  end else if ((timeGetTime-FChangedAt)>cFastRenderWaitPeriod) and not FFastDrawn then
  begin
    FFastDrawn := true;
    RenderImage(4);
  end;
end;

procedure TfrmGodMode.Button_ZoomClick(Sender: TObject);
begin
  with TfrmZoomImage.Create(Application) do
    ShowAndRender(FGenotype);
end;

end.
