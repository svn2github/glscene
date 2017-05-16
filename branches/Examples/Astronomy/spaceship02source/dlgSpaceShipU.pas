unit dlgSpaceShipU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls, Spin, IniFiles;

type
  TdlgSpaceShip = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lbMissions: TListBox;
    GroupBox2: TGroupBox;
    mmMission: TMemo;
    Memo1: TMemo;
    mmControls: TMemo;
    mmFlightSchool: TMemo;
    GroupBox3: TGroupBox;
    tbPitchInertia: TTrackBar;
    Label2: TLabel;
    lblPitchInertia: TLabel;
    Label3: TLabel;
    tbYawInertia: TTrackBar;
    lblYawInertia: TLabel;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label4: TLabel;
    seAsteroids: TSpinEdit;
    Panel1: TPanel;
    btStart: TBitBtn;
    btExit: TBitBtn;
    tbLOD: TTrackBar;
    Label5: TLabel;
    procedure lbMissionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbPitchInertiaChange(Sender: TObject);
    procedure tbYawInertiaChange(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
     
  public
     
  end;

var
  dlgSpaceShip: TdlgSpaceShip;

implementation

{$R *.dfm}

procedure TdlgSpaceShip.lbMissionsClick(Sender: TObject);
begin
  case lbMissions.ItemIndex of
  0: mmMission.Lines.LoadFromFile('AsteroidField.txt');
  end;//case
end;

procedure TdlgSpaceShip.FormCreate(Sender: TObject);
var
  IniF	:TIniFile;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  mmMission.Lines.LoadFromFile('AsteroidField.txt');
  mmControls.Lines.LoadFromFile('Controls.txt');
  mmFlightSchool.Lines.LoadFromFile('FlightSchool.txt');
  lbMissions.ItemIndex:=0;

  IniF:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'SpaceShip.ini');
  try
    tbLOD.Position:=Round(11-IniF.ReadFloat('Environment','CloseAngle',3));
    tbPitchInertia.Position:=Round((1-IniF.ReadFloat('Ship','PitchInertia',1))*10);
    tbYawInertia.Position:=Round((1-IniF.ReadFloat('Ship','YawInertia',1))*10);
  finally
    IniF.Free;
  end;//finally
end;

procedure TdlgSpaceShip.tbPitchInertiaChange(Sender: TObject);
begin
  lblPitchInertia.Caption:=Format('%.1f',[1+tbPitchInertia.Position/10]);
end;

procedure TdlgSpaceShip.tbYawInertiaChange(Sender: TObject);
begin
  lblYawInertia.Caption:=Format('%.1f',[1+tbYawInertia.Position/10]);
end;

procedure TdlgSpaceShip.btStartClick(Sender: TObject);
var
  IniF	:TIniFile;
begin
  IniF:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'SpaceShip.ini');
  try
    IniF.WriteFloat('Environment','CloseAngle',11-tbLOD.Position);
    IniF.WriteFloat('Environment','FarAngle',(11-tbLOD.Position)/2);
    IniF.WriteInteger('Environment','Asteroids',seAsteroids.Value);
    IniF.WriteFloat('Ship','PitchInertia',tbPitchInertia.Position/10+1);
    IniF.WriteFloat('Ship','YawInertia',tbYawInertia.Position/10+1);
  finally
    IniF.Free;
    case lbMissions.ItemIndex of
    0: WinExec(PAnsiChar('AsteroidField.exe'),SW_SHOWNORMAL)
    end;//case
    Application.Terminate;
  end;//finally
end;

end.
