{Procedural Texture Demo / Tobias Peirick }
//The Original is at GLScene
//Greatly changed..altered by Ivan Lee Herring.

unit CCProceduralCloudsFrm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,//help display
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Imaging.Jpeg,

  GLScene,
  GLObjects,
  GLTexture,
  GLTextureFormat,
  GLHUDObjects,
  GLCadencer,
  GLWin32Viewer,
  GLFileTGA,
  GLMaterial,
  GLGraphics,//for bitmap saving
  GLProcTextures,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TProceduralCloudsForm = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    Panel1: TPanel;
    Label1: TLabel;
    CBFormat: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    CBCompression: TComboBox;
    Label5: TLabel;
    RBDefault: TRadioButton;
    RBDouble: TRadioButton;
    LAUsedMemory: TLabel;
    RBQuad: TRadioButton;
    LARGB32: TLabel;
    LACompression: TLabel;
    GLCadencer1: TGLCadencer;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    Label6: TLabel;
    CheckBox2: TCheckBox;
    GLPlane1: TGLPlane;
    TileTrackBar: TTrackBar;
    Timer1: TTimer;
    CloudRandomSeedUsedEdit: TEdit;
    CloudImageSizeUsedEdit: TEdit;
    UseCloudFileCB: TCheckBox;
    CloudFileOpenBtn: TSpeedButton;
    CloudFileUsedEdit: TEdit;
    MakeAndSaveCloudNoiseFile: TSpeedButton;
    Label61: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveImageBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    HelpBtn: TSpeedButton;
    MinCutTrackBar: TTrackBar;
    MinCutLabel: TLabel;
    SharpnessLabel: TLabel;
    SharpnessTrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TileTrackBarChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
    procedure CloudFileOpenBtnClick(Sender: TObject);
    procedure MakeAndSaveCloudNoiseFileClick(Sender: TObject);
    procedure SaveImageBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    newSelection : Boolean;
    FirstTime:Boolean;
  end;

var
  ProceduralCloudsForm: TProceduralCloudsForm;

implementation

{$R *.DFM}

uses
  CCEditorFrm;

procedure TProceduralCloudsForm.FormCreate(Sender: TObject);
begin
  FirstTime:=True;
end;
procedure TProceduralCloudsForm.FormShow(Sender: TObject);
begin
  If FirstTime then begin
  FirstTime:=False;
  Timer1.Enabled:=True;
  GLCadencer1.Enabled:=True;
  CBFormat.ItemIndex:=1;//3;
  CBCompression.ItemIndex:=0;
  CBFormatChange(Sender);
  end;
end;
procedure TProceduralCloudsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Timer1.Enabled:=False;
  GLCadencer1.Enabled:=False;
end;
procedure TProceduralCloudsForm.HelpBtnClick(Sender: TObject);
begin
ShellExecute(
Application.Handle, // handle to parent window
'open',             // pointer to string that specifies operation to perform
PChar(ExtractFilePath(ParamStr(0))+'ProceduralClouds.htm'),// pointer to filename or folder name string
'',// pointer to string that specifies executable-file parameters
                                  //+'help'
 PChar(ExtractFilePath(ParamStr(0))),// pointer to string that specifies default directory
SW_SHOWNORMAL);
end;

procedure TProceduralCloudsForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if CheckBox1.Checked then
     TGLProcTextureNoise(GLPlane1.Material.Texture.Image).NoiseAnimate(deltaTime);
end;

procedure TProceduralCloudsForm.TileTrackBarChange(Sender: TObject);
begin
  GLPlane1.XTiles:= TileTrackBar.Position;
  GLPlane1.YTiles:= TileTrackBar.Position;
  {EnvColor clrLightBlue   TextureMode Blend}
end;

procedure TProceduralCloudsForm.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TProceduralCloudsForm.GLSceneViewer1AfterRender(Sender: TObject);
var
   rgb : Integer;
begin
   // update compression stats, only the 1st time after a new selection
   if newSelection then with GLPlane1.Material.Texture do begin
      rgb:=Image.Width*Image.Height*4;
      LARGB32.Caption:=Format('RGBA 32bits would require %d kB', [rgb div 1024]);
      LAUsedMemory.Caption:=Format('Required memory : %d kB',
                                   [TextureImageRequiredMemory div 1024]);
      LACompression.Caption:=Format('Compression ratio : %d %%',
                                    [100-100*TextureImageRequiredMemory div rgb]);
      newSelection:=False;
   end;
end;

procedure TProceduralCloudsForm.CBFormatChange(Sender: TObject);
var
  aPERM: array [0..255] of Byte;
  outfile:Textfile;
  s:string;
   i : Integer;
begin
   // adjust settings from selection and reload the texture map
   with GLPlane1.Material.Texture do begin
      If (UseCloudFileCB.Checked and (FileExists(CloudFileUsedEdit.Text)))then
      begin
      Try
        AssignFile(outfile, CloudFileUsedEdit.Text);   { File selected in dialog box }
        Reset(outfile);
        Readln(outfile, s{'Cloud Base V1.0'});
        For I := 0 to 255 do
        begin
          Readln(outfile, s);
          aPERM[I]:=strtoint(s);
        end;
      Finally
        CloseFile(outfile);
      End;
      TGLProcTextureNoise(Image).SetPermFromData(aPERM);
      end else TGLProcTextureNoise(Image).SetPermToDefault;
      TextureFormat:=TGLTextureFormat(Integer(tfRGB)+CBFormat.ItemIndex);
      Compression:=TGLTextureCompression(Integer(tcNone)+CBCompression.ItemIndex);
      TGLProcTextureNoise(Image).MinCut := MinCutTrackBar.Position;
      TGLProcTextureNoise(Image).NoiseSharpness := SharpnessTrackBar.Position /100;
      TGLProcTextureNoise(Image).Height :=strtoint(CloudImageSizeUsedEdit.Text);
      TGLProcTextureNoise(Image).Width :=strtoint(CloudImageSizeUsedEdit.Text);
      TGLProcTextureNoise(Image).NoiseRandSeed :=strtoint(CloudRandomSeedUsedEdit.Text); ;
      TGLProcTextureNoise(Image).Seamless := CheckBox2.Checked;
      SharpnessLabel.Caption:=Inttostr(SharpnessTrackBar.Position);
      MinCutLabel.Caption:=Inttostr(MinCutTrackBar.Position);

      if RBDefault.Checked then begin
         GLPlane1.Width:= 50;
         GLPlane1.Height:=50;
      end else if RBDouble.Checked then begin
         GLPlane1.Width:=100;
         GLPlane1.Height:=100;
      end else begin
         GLPlane1.Width:=400;
         GLPlane1.Height:=400;
      end;
   end;
   newSelection:=True;
end;





procedure TProceduralCloudsForm.CloudFileOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog1.Filename:='*.clb' ;
  if OpenDialog1.execute then
  begin
    CloudFileUsedEdit.Text := OpenDialog1.Filename;
  end;
end;

procedure TProceduralCloudsForm.MakeAndSaveCloudNoiseFileClick(Sender: TObject);
var
  aPERM: array [0..255] of Byte;
  outfile:Textfile;
  i:Integer;
Procedure RandomPerm;
var Idiot,Count,More, Less,again:Integer;
begin
  MakeAndSaveCloudNoiseFile.Caption:=inttostr(0);
  Application.ProcessMessages;
  For Idiot := 0 to 255 do
  begin
    aPERM[Idiot]:=Random(256);
    //Label61.Caption:= inttostr(Idiot);
    //Application.ProcessMessages;
  end;
  Count:=0;
  repeat
    again:=0;
    Less:= Random(256);
    For Idiot := 0 to Count do
    begin
      more:= aPERM[Idiot];
      If (Less = more) then  inc(again);
    end;
      Label61.Caption:= inttostr(again); //these can be removed.. just for debugging
      Application.ProcessMessages;
      If (again = 0) then
      begin
        aPERM[Count+1]:=Less;
        inc(Count);
        MakeAndSaveCloudNoiseFile.Caption:=
        inttostr(Less)+','+inttostr(Count);
        Application.ProcessMessages;
      end;
  until Count = 255
end;
begin
  SaveDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  SaveDialog1.InitialDir:=ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt:='rnd';
  SaveDialog1.Filename:='*.clb' ;
  if (SaveDialog1.Execute) then
  begin
    if UpperCase(ExtractFileExt(SaveDialog1.FileName)) = '.CLB' then
    begin
      Application.ProcessMessages;
      Randomize;
      RandomPerm;
      Try
        AssignFile(outfile, SaveDialog1.FileName);   { File selected in dialog box }
        Rewrite(outfile);
        Writeln(outfile, 'Cloud Base V1.0');
        For I := 0 to 255 do
          Writeln(outfile, inttostr(aPERM[I]));
      Finally
        CloseFile(outfile);
      End;
      Label61.Caption:='Done';
      MakeAndSaveCloudNoiseFile.Caption:='';
    end;
  end;
end;


procedure TProceduralCloudsForm.SaveImageBtnClick(Sender: TObject);
var
  fName : String;
  Saved:TGLBitmap32;
  Saved2:TBitmap;
  tga : TTGAImage;
begin
  //SaveDialog1.Filter := 'Cloud image (*.bmp)|*.bmp';
  SaveDialog1.Filter := 'Cloud image (*.bmp;*.tga)|*.bmp;*.tga';
  SaveDialog1.InitialDir:=ImagePath;//ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt:='bmp';
  SaveDialog1.Filename:='';//'*.bmp' ;
  If SaveDialog1.Execute then
  begin
    if (GLPlane1.Material.Texture.Image is TGLProcTextureNoise) then
    begin
      Saved:=TGLProcTextureNoise(GLPlane1.Material.Texture.Image).GetBitmap32(); //(1)
      Saved2:=Saved.Create32BitsBitmap;
      fName:=SaveDialog1.FileName;
      if ExtractFileExt(fName)='' then fName:=fName+'.bmp' else
      if ExtractFileExt(fName)='.bmp' then begin {do nothing} end
         else changefileext(fName,'.tga');
      if LowerCase(ExtractFileExt(fName))='.tga' then begin
         tga:=TTGAImage.Create;
         try
            tga.Assign(Saved2{bmp}{pic.Bitmap});
            tga.SaveToFile(fName)
         finally
            tga.Free;
         end;
      end else //bmp.SaveToFile(fName);
      Saved2.SaveToFile(SaveDialog1.FileName);
    end;
  end;
end;







end.
