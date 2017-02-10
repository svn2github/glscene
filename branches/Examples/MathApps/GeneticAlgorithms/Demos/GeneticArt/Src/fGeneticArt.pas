unit fGeneticArt;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math, System.Contnrs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg,
  GR32_Image, GR32, GR32_Layers, Vcl.StdCtrls, XPMan, Noise,
  Vcl.ExtCtrls, Vcl.Menus,
  sdXmlDocuments,
  uNeatClasses,
  uTransferFunctionClasses;

const
  cImageWidth = 210;
  cImageHeight = 128;

type
  TfrmGeneticArt = class(TForm)
    NEATPopulation: TNEATPopulation;
    XPManifest1: TXPManifest;
    Button_Restart: TButton;
    Button_NewGeneration: TButton;
    Panel_Images: TPanel;
    Shape1: TShape;
    cbUseColor: TCheckBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    cbCoords: TCheckBox;
    cbPerlinNoise: TCheckBox;
    PopupMenu_Image: TPopupMenu;
    RaiseFitness1: TMenuItem;
    LowerFitness1: TMenuItem;
    N1: TMenuItem;
    ZoomImage1: TMenuItem;
    GodMode1: TMenuItem;
    Button_Zoom: TButton;
    Button_GodMode: TButton;
    SaveGenotype1: TMenuItem;
    N2: TMenuItem;
    Button_Favorites: TButton;
    SaveDialog_SaveImage: TSaveDialog;
    SaveImage1: TMenuItem;
    cbModifyPerlin: TCheckBox;
    procedure Image32_Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_RestartClick(Sender: TObject);
    procedure Button_NewGenerationClick(Sender: TObject);
    function NEATPopulationCalculateFitness(
      NEATPopulation: TNEATPopulation; Genotype: TGenotype): Double;
    procedure cbUseColorClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbCoordsClick(Sender: TObject);
    procedure RaiseFitness1Click(Sender: TObject);
    procedure LowerFitness1Click(Sender: TObject);
    procedure ZoomImage1Click(Sender: TObject);
    procedure Image32_MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure GodMode1Click(Sender: TObject);
    procedure Button_ZoomClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button_GodModeClick(Sender: TObject);
    procedure SaveGenotype1Click(Sender: TObject);
    procedure Button_FavoritesClick(Sender: TObject);
    procedure SaveImage1Click(Sender: TObject);
  private
    { Private declarations }
    FImages : TObjectList;
    FNoise : TNoise;
    FCurrentImage : TImage32;
    FWantsZoom : boolean;
    FWantsGodMode : boolean;
    FWorkingWithFavorites : boolean;
    procedure SetCursor(const ACursor : TCursor);
    procedure Restart(const AWithRender : boolean=true);
    procedure CreateImages;
    function GetGenotypeForImage(const AImage : TImage32) : TGenotype;
    procedure DoGodMode;
    procedure DoZoom;
  public
    { Public declarations }
    procedure RenderGeneration;
    procedure RenderGenotype(const AGenotype : TGenotype;
      const ABitmap32 : TBitmap32; const ARenderSkip : integer=1); overload;

    procedure SaveImage(const ABitmap32 : TBitmap32);

    procedure RenderGenotype(const AGenotype : TGenotype;
      const AImage32 : TImage32; const ARenderSkip : integer=1); overload;
  end;

var
  frmGeneticArt: TfrmGeneticArt;

implementation

uses
  (*uImageUtils,*)
  fZoomImage, fGodMode, uNEATToXML, fFavorites,
  fAddToFavorites;

{$R *.dfm}

procedure TfrmGeneticArt.Image32_Click(Sender: TObject);
begin
  if FWantsGodMode then
  begin
    FWantsGodMode := false;
    SetCursor(crDefault);
    DoGodMode;
  end

  else if FWantsZoom then
  begin
    FWantsZoom := false;
    SetCursor(crDefault);
    DoZoom;
  end else
    TImage32(Sender).Tag := TImage32(Sender).Tag + 1;
end;

procedure TfrmGeneticArt.FormCreate(Sender: TObject);
begin
  FNoise := TNoise.Create(1024);
  Randomize;
  RegisterNewTransferFuntion('Misc', 'Gaussian', TTransferFunctionGaussian.Create);
  // RegisterNewTransferFuntion('Misc', 'Multiply', TTransferFunctionMUL.Create);
  // RegisterNewTransferFuntion('Misc', 'Max', TTransferFunctionMax.Create);
  // RegisterNewTransferFuntion('Misc', 'Min', TTransferFunctionMin.Create);

  FImages := TObjectList.Create;
  CreateImages;

  NEATPopulation.CreateFitnessMonitor;
  Show;
  Restart;
end;

procedure TfrmGeneticArt.Button_RestartClick(Sender: TObject);
begin
  Restart;
  Button_NewGeneration.SetFocus;
end;

procedure TfrmGeneticArt.Restart(const AWithRender : boolean);
var
  i,j  : integer;
  Connect : TConnect;
  NewNode: TNode;
begin
  CreateImages;

  with NEATPopulation do
  begin
    FitnessMonitor.ResetForNewRun;
    FitnessMonitor.SetRunState(true);

    // Clear out all old innovations
    DeleteAllConnectInnovations;

    CreatePopulation;

    for i := 0 to NEATPopulation.PopulationSize-1 do
      with NEATPopulation.Genotypes[i] do
      begin
        for j := 0 to NodeList.Count-1 do
          NodeList[j].MutateFunction;

        Connect := GetRandomActiveConnect;
        if Assigned(Connect) then
        begin
          NewNode := Connect.MutateSplit;

          if Assigned(NewNode) then
            AddConnection(
              NewNode.NodeID,
              NewNode.NodeID,
              (random*2-1)/10);
        end;

        AddConnection(
          OutputNodes[0].NodeID,
          OutputNodes[0].NodeID,
          (random*2-1)/10);

        {for j := 0 to 5 do
          Mutate;//}
        PreparePhenotype;
      end;//}

    if AWithRender then
      RenderGeneration;
  end;
end;

procedure TfrmGeneticArt.RenderGeneration;
var
  i : integer;
begin
  Screen.Cursor := crHourGlass;
  try
    for i := 0 to FImages.Count-1 do
    begin
      with TImage32(FImages[i]) do
      begin
        Bitmap.Width := Width;
        Bitmap.Height := Height;
        Bitmap.Clear(clBlack);
      end;

      RenderGenotype(NEATPopulation.Genotypes[i], TImage32(FImages[i]).Bitmap, 4);
      TImage32(FImages[i]).Refresh;
    end;

    for i := 0 to FImages.Count-1 do
    begin
      RenderGenotype(NEATPopulation.Genotypes[i], TImage32(FImages[i]).Bitmap);
      TImage32(FImages[i]).Refresh;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function mclean(x : single) : single;
begin
  while abs(x)>100 do
    x := x/100;

  while x>1 do
    x:=2-x;

  while x<-1 do
    x:=2+x;

  result := abs(x);//}
end;

procedure TfrmGeneticArt.RenderGenotype(const AGenotype: TGenotype;
  const ABitmap32: TBitmap32; const ARenderSkip : integer);
var
  x, y, dx, dy : integer;
  varx, vary, wdiv, hdiv, xmove, ymove, perlinOffset : single;
  p0, p1, p2, p3 : single;
  r,g,b : single;
  c : byte;
  q : single;
  d, MaxRad : single;
  RenderWidth, RenderHeight : integer;
  procedure FillNetwork;
  begin
    AGenotype.Flush;
    AGenotype.SetNextInputValue(1);

    if cbCoords.Checked then
    begin
      AGenotype.SetNextInputValue(varx);
      AGenotype.SetNextInputValue(vary);
      AGenotype.SetNextInputValue(d);
    end;

    if cbPerlinNoise.Checked then
    begin
      AGenotype.SetNextInputValue(p0);
      AGenotype.SetNextInputValue(p1);
      AGenotype.SetNextInputValue(p2);
      AGenotype.SetNextInputValue(p3);
    end;
  end;
begin
  AGenotype.Flush;
  wdiv := 2/ABitmap32.Width;
  hdiv := 2/ABitmap32.Height;
  MaxRad := 1/Sqrt(2);

  Q := ABitmap32.Width / ABitmap32.Height;

  RenderWidth := ABitmap32.Width div ARenderSkip;
  RenderHeight := ABitmap32.Height div ARenderSkip;

  xmove := (ARenderSkip/ABitmap32.Width);
  ymove := (ARenderSkip/ABitmap32.Height);

  AGenotype.InputNodes[0].BiasNode := true;

  if cbModifyPerlin.Checked then
    perlinOffset := AGenotype.ConnectList[0].Weight+1
  else
    perlinOffset := 1;


  for x := 0 to RenderWidth-1 do
    for y := 0 to RenderHeight-1 do
    begin
      varx := (x*ARenderSkip*wdiv-1+xmove)*q;
      vary := ARenderSkip*y*hdiv-1+ymove;

      if cbPerlinNoise.Checked then
      begin
        p0 := FNoise.Noise(varx*1+(perlinOffset-1)*10, vary*1+(perlinOffset-1)*10, 0);
        p1 := FNoise.Noise(varx*2+perlinOffset*100, vary*2+perlinOffset*100, perlinOffset*100);
        p2 := FNoise.Noise(varx*4+perlinOffset*600, vary*4+perlinOffset*600, perlinOffset*100);
        p3 := FNoise.Noise(varx*8+perlinOffset*700, vary*8+perlinOffset*700, perlinOffset*100);
      end;

      if cbCoords.Checked then
        d := sqrt(sqr(varx)+sqr(vary))/MaxRad;

      if not cbUseColor.Checked then
      begin
        FillNetwork;
        AGenotype.Iterate(-1);
        c := trunc(mclean(AGenotype.Outputs[0]) * 255);

        for dx := x*ARenderSkip to (x+1)*ARenderSkip-1 do
          for dy := y*ARenderSkip to (y+1)*ARenderSkip-1 do
            ABitmap32.Pixel[dx,dy] := Color32(c,c,c);//}
      end else
      begin
        FillNetwork;
        AGenotype.Inputs[AGenotype.InputNodes.Count-1] := 1;
        AGenotype.Inputs[AGenotype.InputNodes.Count-2] := 0;
        AGenotype.Inputs[AGenotype.InputNodes.Count-3] := 0;
        AGenotype.Iterate(-1);
        r := AGenotype.Outputs[0];

        FillNetwork;
        AGenotype.Inputs[AGenotype.InputNodes.Count-1] := 0;
        AGenotype.Inputs[AGenotype.InputNodes.Count-2] := 1;
        AGenotype.Inputs[AGenotype.InputNodes.Count-3] := 0;
        AGenotype.Iterate(-1);
        g := AGenotype.Outputs[0];

        FillNetwork;
        AGenotype.Inputs[AGenotype.InputNodes.Count-1] := 0;
        AGenotype.Inputs[AGenotype.InputNodes.Count-2] := 0;
        AGenotype.Inputs[AGenotype.InputNodes.Count-3] := 1;
        AGenotype.Iterate(-1);
        b := AGenotype.Outputs[0];

        for dx := x*ARenderSkip to (x+1)*ARenderSkip-1 do
          for dy := y*ARenderSkip to (y+1)*ARenderSkip-1 do
            ABitmap32.Pixel[dx,dy] :=
              Color32(
                trunc(mclean(r) * 255),
                trunc(mclean(g) * 255),
                trunc(mclean(b) * 255));//}
      end;
    end;
end;

procedure TfrmGeneticArt.Button_NewGenerationClick(Sender: TObject);
begin
  NEATPopulation.CalculateFitnesses;
  NEATPopulation.CreateNewGeneration;

  RenderGeneration;
end;

function TfrmGeneticArt.NEATPopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): Double;
begin
  result := TImage32(FImages[NEATPopulation.CurrentGenotypeID]).Tag;
  Genotype.Fitness := result;
  TImage32(FImages[NEATPopulation.CurrentGenotypeID]).Tag :=0 ;
end;

procedure TfrmGeneticArt.CreateImages;
var
  Image : TImage32;
  Columns, Rows, Row, Col : integer;
begin
  Columns := (Panel_Images.Width) div (cImageWidth+8);
  Rows := (Panel_Images.Height) div (cImageHeight+8);

  FImages.Clear;
  for Col := 0 to Columns-1 do
    for Row := 0 to Rows-1 do
    begin
      Image := TImage32.Create(Panel_Images);
      Image.Parent := Panel_Images;
      Image.OnClick := Image32_Click;
      Image.OnDblClick := Image32_Click;
      Image.Width := cImageWidth;
      Image.Height := cImageHeight;
      Image.Left := 8 + Col*(cImageWidth+8);
      Image.Top := 8 + Row*(cImageHeight+8);
      Image.Hint := 'Click on this image to raise it''s fitness.';
      Image.PopupMenu := PopupMenu_Image;
      Image.OnMouseMove := Image32_MouseMove;

      FImages.Add(Image);
    end;

  Panel_Images.Refresh;

  NEATPopulation.ClearPopulation;
  NEATPopulation.SwapPopulations;
  NEATPopulation.ClearPopulation;
  NEATPopulation.InputNodeCount := 1;

  if cbCoords.Checked then
    NEATPopulation.InputNodeCount := NEATPopulation.InputNodeCount+3;
  if cbPerlinNoise.Checked then
    NEATPopulation.InputNodeCount := NEATPopulation.InputNodeCount+4;
  if cbUseColor.Checked then
    NEATPopulation.InputNodeCount := NEATPopulation.InputNodeCount+3;


  NEATPopulation.PopulationSize := FImages.Count;
end;

procedure TfrmGeneticArt.cbUseColorClick(Sender: TObject);
begin
  Restart;
end;

procedure TfrmGeneticArt.FormResize(Sender: TObject);
begin
  // Caption := Format('%d:%d', [Width, Height]);
end;

procedure TfrmGeneticArt.cbCoordsClick(Sender: TObject);
begin
  // RenderGeneration;
  Application.ProcessMessages;
  Restart(not FWorkingWithFavorites);
end;

procedure TfrmGeneticArt.RaiseFitness1Click(Sender: TObject);
begin
  TImage32(Sender).Tag := TImage32(Sender).Tag + 1;
end;

procedure TfrmGeneticArt.LowerFitness1Click(Sender: TObject);
begin
  TImage32(Sender).Tag := TImage32(Sender).Tag - 1;
  if TImage32(Sender).Tag<0 then
    TImage32(Sender).Tag := 0;
end;

procedure TfrmGeneticArt.DoZoom;
begin
  with TfrmZoomImage.Create(Application) do
    ShowAndRender(GetGenotypeForImage(FCurrentImage));
end;

procedure TfrmGeneticArt.ZoomImage1Click(Sender: TObject);
begin
  DoZoom;
end;

function TfrmGeneticArt.GetGenotypeForImage(
  const AImage: TImage32): TGenotype;
var
  idx : integer;
begin
  idx := FImages.IndexOf(AImage);
  result := NEATPopulation.Genotypes[idx];
end;

procedure TfrmGeneticArt.Image32_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FCurrentImage := TImage32(Sender);
end;

procedure TfrmGeneticArt.DoGodMode;
begin
  with TfrmGodMode.Create(Application) do
    ShowAndModify(GetGenotypeForImage(FCurrentImage));

  RenderGenotype(GetGenotypeForImage(FCurrentImage), FCurrentImage.Bitmap);
  FCurrentImage.Repaint;
end;

procedure TfrmGeneticArt.GodMode1Click(Sender: TObject);
begin
  DoGodMode;
end;

procedure TfrmGeneticArt.Button_ZoomClick(Sender: TObject);
begin
  FWantsGodMode := False;
  FWantsZoom := true;
  SetCursor(crHandPoint);
  Button_NewGeneration.SetFocus;
end;

procedure TfrmGeneticArt.FormKeyPress(Sender: TObject; var Key: Char);
begin
  FWantsZoom := false;
  FWantsGodMode := false;
  SetCursor(crDefault);
end;

procedure TfrmGeneticArt.Button_GodModeClick(Sender: TObject);
begin
  FWantsGodMode := true;
  FWantsZoom := false;
  SetCursor(crHandPoint);
  Button_NewGeneration.SetFocus;
end;

procedure TfrmGeneticArt.SetCursor(const ACursor: TCursor);
var
  i : integer;
begin
  Screen.Cursor := ACursor;
  Cursor := ACursor;
  Panel_Images.Cursor := ACursor;
  Shape1.Cursor := ACursor;

  for i := 0 to FImages.Count-1 do
    TImage32(FImages[i]).Cursor := ACursor;
end;

procedure TfrmGeneticArt.SaveGenotype1Click(Sender: TObject);
begin
  with TfrmAddToFavorites.Create(Application) do
    try
      AddGenotype(GetGenotypeForImage(FCurrentImage));
    finally
      Free;
    end;
end;

procedure TfrmGeneticArt.Button_FavoritesClick(Sender: TObject);
begin
  Button_NewGeneration.SetFocus;
  
  with TfrmFavorites.Create(Application) do
    try
      FWorkingWithFavorites := true;
      ShowModal;
    finally
      FWorkingWithFavorites := false;
      Free;
    end;
end;

procedure TfrmGeneticArt.RenderGenotype(const AGenotype: TGenotype;
  const AImage32: TImage32; const ARenderSkip: integer);
begin
  Screen.Cursor := crHourGlass;
  try
    AImage32.Bitmap.SetSize(AImage32.ClientWidth, AImage32.ClientHeight);
    AImage32.Bitmap.Clear(clBlack);

    frmGeneticArt.RenderGenotype(AGenotype, AImage32.Bitmap, ARenderSkip);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGeneticArt.SaveImage(const ABitmap32: TBitmap32);
var
  ext : string;

  procedure SaveToJpeg;
  var
    jpg : TJPegImage;
    bmp : TBitmap;
  begin
    jpg := nil;
    Bmp := TBitmap.Create;

    try
      Bmp.Assign(ABitmap32);

      jpg := TJPegImage.Create;

      jpg.Assign(Bmp);
      jpg.Compress;
      jpg.SaveToFile(SaveDialog_SaveImage.FileName);
     finally
      FreeAndNil(jpg);
      FreeAndNil(bmp);
    end;
  end;
begin
  SaveDialog_SaveImage.InitialDir := '';
  SaveDialog_SaveImage.FileName := '*.jpg';

  if SaveDialog_SaveImage.Execute then
  begin
    ext := LowerCase(ExtractFileExt(SaveDialog_SaveImage.FileName));

    if (ext = '.jpg') or (ext = '.jpeg') then
    begin
      // Create a bitmap
      SaveToJpeg;
    end else
      ABitmap32.SaveToFile(SaveDialog_SaveImage.FileName);
  end;
end;

procedure TfrmGeneticArt.SaveImage1Click(Sender: TObject);
begin
  SaveImage(FCurrentImage.Bitmap);
end;

end.
