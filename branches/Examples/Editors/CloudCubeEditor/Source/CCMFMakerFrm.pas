unit CCMFMakerFrm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  GR32,
  GR32_Layers,
  GR32_Image,
  GLFileTGA,
  uHeightmapClasses;

type
  TMFMakerForm = class(TForm)
    Image32_Terrain: TImage32;
    Panel1: TPanel;
    SaveBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    Button_GO: TSpeedButton;
    EdgesBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    GoTB: TTrackBar;
    SizeRG: TRadioGroup;
    Memo_Octaves: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBar_Radius: TScrollBar;
    ScrollBar_Depth: TScrollBar;
    CheckBox_Box: TCheckBox;
    SaveDialog1: TSaveDialog;
    CheckBox_RenderWater: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExitBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SizeRGClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure Button_GOClick(Sender: TObject);
    procedure EdgesBtnClick(Sender: TObject);
    procedure Image32_TerrainMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure Image32_TerrainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HeightMap : THeightMap;
    FirstTime:Boolean;
    procedure DoRender;
    //procedure SetStatus(s : string);
  end;

var
  MFMakerForm: TMFMakerForm;
  StartPath : string;
const
  BASE_DIR = 'Output\';

implementation

uses
  StrFunctions,CCEditorFrm;

{$R *.DFM}

procedure TMFMakerForm.FormCreate(Sender: TObject);
begin
  FirstTime:=True;

end;
procedure TMFMakerForm.FormShow(Sender: TObject);
begin
  If FirstTime then begin
  FirstTime:=False;
  Image32_Terrain.Bitmap.SetSize(Image32_Terrain.Width, Image32_Terrain.Height);
  Heightmap := THeightMap.Create(Image32_Terrain.Width, Image32_Terrain.Height);
  Button_GO.Click;
  //StartPath := ExtractFilePath(ParamStr(0));
  StartPath := GetCurrentDir+'\';

  DoRender;
  end;
end;
procedure TMFMakerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Heightmap.Destroy;
end;

procedure TMFMakerForm.ExitBtnClick(Sender: TObject);
begin
close;
end;

procedure TMFMakerForm.HelpBtnClick(Sender: TObject);
begin
ShellExecute(
Application.Handle, // handle to parent window
'open',             // pointer to string that specifies operation to perform
PChar(ExtractFilePath(ParamStr(0))+'MFClouds.htm'),// pointer to filename or folder name string
'',// pointer to string that specifies executable-file parameters
                                  //+'help'
 PChar(ExtractFilePath(ParamStr(0))),// pointer to string that specifies default directory
SW_SHOWNORMAL);
end;

procedure TMFMakerForm.SizeRGClick(Sender: TObject);
begin
  Case SizeRG.ItemIndex of
  0: Begin Image32_Terrain.Width:=64; Image32_Terrain.Height:=64;  ClearBtnClick(Self);End;
  1: Begin Image32_Terrain.Width:=128; Image32_Terrain.Height:=128;  ClearBtnClick(Self);End;
  2: Begin Image32_Terrain.Width:=256; Image32_Terrain.Height:=256;  ClearBtnClick(Self);End;
  end;
  Image32_Terrain.Bitmap.SetSize(Image32_Terrain.Width, Image32_Terrain.Height);
  Heightmap.Destroy;
  Heightmap := THeightMap.Create(Image32_Terrain.Width, Image32_Terrain.Height);
  ClearBtnClick(Self);
end;

procedure TMFMakerForm.ClearBtnClick(Sender: TObject);
begin
  Heightmap.Height.Clear;
  DoRender;
end;

procedure TMFMakerForm.Button_GOClick(Sender: TObject);
var
  s : string;
  i : integer;
  f, a : single;
  x,y : integer;
  maxd : single;
  d : single;
begin                 //Hidden under the panel...
  for i := 0 to Memo_Octaves.Lines.Count-1 do
  begin
    s := Memo_Octaves.Lines[i];

    if s='clear' then
    begin
      Heightmap.Height.Clear;
      Continue;
    end;

    if s='rescale' then
    begin
      HeightMap.Rescale(0,1);
      Continue;
    end;

    if s='bubble' then
    begin
      maxd := sqrt(sqr(HeightMap.SizeX/2));//sqr(HeightMap.SizeY/2));
      for x := 0 to HeightMap.SizeX-1 do
        for y := 0 to HeightMap.SizeY-1 do
        begin
          d := sqrt(sqr(x-HeightMap.SizeX/2)+sqr(y-HeightMap.SizeY/2));

          if d>MaxD then
            HeightMap.Height[x,y] := 0
          else
            HeightMap.Height[x,y] := 2-2*d/maxd;
        end;

      //HeightMap.Rescale(0,1);
      Continue;
    end;


    if GetBefore('|', s)='s' then
    begin
      s := GetAfter('|',s);
      f := StrToFloat(GetBefore('|', s));
      a := StrToFloat(GetAfter('|', s));
      Heightmap.Subdivide(trunc(f),a);
      continue;
    end;

    if GetBefore('|', s)='n' then
    begin
      s := GetAfter('|',s);
      f := StrToFloat(GetBefore('|', s));
      a := StrToFloat(GetAfter('|', s));
      Heightmap.AddNoise(f,a);
      continue;
    end;

    if GetBefore('|', s)='mi' then
    begin
      Heightmap.MakeIsland(StrToFloat(GetAfter('|', s)));
      continue;
    end;
  end;
  DoRender;
end;

procedure TMFMakerForm.EdgesBtnClick(Sender: TObject);
begin
  HeightMap.Rescale(0,1);
  HeightMap.MakeIsland(0.1);
  HeightMap.Rescale(0,1);
  //HeightMap.ClampToLevel(0.5);
  DoRender;
end;

procedure TMFMakerForm.Image32_TerrainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  Mult : integer;
begin
  if ssRight in Shift then
    Mult := 1
  else
    Mult := -1;

  if CheckBox_Box.Checked then
    HeightMap.BoxLower(x,y, Mult*ScrollBar_Depth.Position/100, ScrollBar_Radius.Position)
  else
    HeightMap.CircleLower(x,y, Mult*ScrollBar_Depth.Position/100, ScrollBar_Radius.Position);

  DoRender;
end;

var
  oldX : integer=-100;
  oldY : integer=-100;
  

procedure TMFMakerForm.Image32_TerrainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Mult : integer;
//  r : integer;
begin
  if ssRight in Shift then
    Mult := 1
  else
    Mult := -1;

  if (ssLeft in Shift) or (ssRight in Shift) then
  begin
    HeightMap.CircleLower(x,y, Mult*ScrollBar_Depth.Position/100, ScrollBar_Radius.Position);
    DoRender;
  end else
  begin
//    r := ScrollBar_Radius.Position;
  end;
end;


procedure TMFMakerForm.SaveBtnClick(Sender: TObject);
var
  fName : String;
  tga : TTGAImage;
begin
//  Image1
//  SaveDialog1.Filter := 'MF Cloud image (*.bmp)|*.bmp';
  SaveDialog1.Filter := 'MF Cloud image (*.bmp;*.tga)|*.bmp;*.tga';
  SaveDialog1.InitialDir:=ImagePath;//ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt:='bmp';
  SaveDialog1.Filename:='';//'*.bmp' ;
  If SaveDialog1.Execute then
  begin
  //Image1.picture.bitmap.SaveToFile(SaveDialog1.FileName);
  //Image32_Terrain.Bitmap.SaveToFile(SaveDialog1.FileName);
      fName:=SaveDialog1.FileName;
      if ExtractFileExt(fName)='' then fName:=fName+'.bmp' else
      if ExtractFileExt(fName)='.bmp' then begin {do nothing} end
         else changefileext(fName,'.tga');
      if LowerCase(ExtractFileExt(fName))='.tga' then begin
         tga:=TTGAImage.Create;
         try
            tga.Assign(Image32_Terrain.Bitmap{bmp}{pic.Bitmap});
            tga.SaveToFile(fName)
         finally
            tga.Free;
         end;
      end else //bmp.SaveToFile(fName);
      //Saved2.SaveToFile(SaveDialog1.FileName);
      Image32_Terrain.Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;


procedure TMFMakerForm.DoRender;
begin
  Heightmap.RenderTo(Image32_Terrain.Bitmap,
  CheckBox_RenderWater.Checked,
  (GoTB.Position/10)
  {0.2});
end;



{procedure TMFMakerForm.SetStatus(s: string);
begin
//  StatusBar1.SimpleText := s;
end;}




end.
