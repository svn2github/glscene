unit nFrenzy;
{Birds eye
Maze pole
Centers inn
Fly renzy
Feed renzy
Predatorenzy}
{Bye
Awk
Fly
Frenzy
Feed
Food
}
{Hummingbird
Flicker
Yellow Finch
Red Finch
Cardinals
Mockingbird
Woodpecker
Blue Jay
Crow
Duck
Owl
Goose
Hawk
Eagle}
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, Buttons;

type
  TFrenzyForm = class(TForm)
    FrenzyNameEdit: TEdit;
    FrenzyAddBtn: TBitBtn;
    FrenzyLB: TListBox;
    FrenzyLoadBtn: TBitBtn;
    FrenzySaveBtn: TBitBtn;
    HelpBtn: TBitBtn;
    FrenzyDeletebirdBtn: TBitBtn;
    FileListBox1: TFileListBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ClearBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FrenzyAddBtnClick(Sender: TObject);
    procedure FrenzyDeletebirdBtnClick(Sender: TObject);
    procedure FrenzyLoadBtnClick(Sender: TObject);
    procedure FrenzySaveBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
     
  public
     
  end;

var
  FrenzyForm: TFrenzyForm;

implementation
uses nUGlobal, nUBirdDX;
{$R *.DFM}

procedure TFrenzyForm.FormCreate(Sender: TObject);
begin
  top := FrenzyFormY;
  left := FrenzyFormX;
  FrenzyFilesLoaded := False;
  FrenzyLB.Clear;
end;

procedure TFrenzyForm.FormShow(Sender: TObject);
var FStr: string;
begin
  FStr := ExtractFileDrive(LifeDir);
  if length(FStr) = 1 then DriveComboBox1.Drive := FStr[1];
end;

procedure TFrenzyForm.ReallyClose;
begin
  Close;
end;

procedure TFrenzyForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  FrenzyFormY := FrenzyForm.top;
  FrenzyFormX := FrenzyForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TFrenzyForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(8100);
end;

procedure TFrenzyForm.ClearBtnClick(Sender: TObject);
begin
  FrenzyLB.Clear;
  FrenzyFilesLoaded := False;
end;

procedure TFrenzyForm.FrenzyAddBtnClick(Sender: TObject);
begin
  if FileListBox1.Itemindex > -1 then begin
    FrenzyLB.Items.Add(ExtractFileName(FileListBox1.Items.Strings[FileListBox1.Itemindex]));
  end else showmessage('Select a file first');
  FrenzyFilesLoaded := True;
end;

procedure TFrenzyForm.FrenzyDeletebirdBtnClick(Sender: TObject);
begin
  if FrenzyLB.Itemindex > -1 then begin
    FrenzyLB.Items.Delete(FrenzyLB.Itemindex);
  end else showmessage('Select a file first');
  if (FrenzyForm.FrenzyLB.Items.Count = 0) then
    FrenzyFilesLoaded := False;
end;

procedure TFrenzyForm.FrenzyLoadBtnClick(Sender: TObject);
var
  F: Textfile;
  FStr: string;
begin
  OpenDialog1.Title := 'Frenzy Files';
  OpenDialog1.Filter := 'Frenzy (*.fnz)|*.fnz';
  OpenDialog1.InitialDir := LifeDir;
  OpenDialog1.Filename := FrenzyNameEdit.Text;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.fnz'))
      then
    begin
      FrenzyNameEdit.Text := ExtractFileName(OpenDialog1.Filename);
      FrenzyFilesName := FrenzyNameEdit.Text;
      AssignFile(F, OpenDialog1.Filename);
      Reset(F);
      while (not eof(F)) do
      begin
        Readln(F, FStr);
        FrenzyLB.Items.Add(FStr);
      end;
      CloseFile(F);
      FrenzyFilesLoaded := True;
    end;
  end;
end;

procedure TFrenzyForm.FrenzySaveBtnClick(Sender: TObject);
var
  F: Textfile;
  i: Integer;
  FStr: string;
begin
  if (FrenzyLB.Items.Count > 0) then
  begin
    SaveDialog1.Title := 'Frenzy Files';
    SaveDialog1.Filter := 'Frenzy (*.fnz)|*.fnz';
    SaveDialog1.InitialDir := LifeDir;
    SaveDialog1.Filename := FrenzyNameEdit.Text;
    if SaveDialog1.Execute then begin
      if ((lowercase(ExtractFileExt(SaveDialog1.Filename)) = '.fnz'))
        then
      begin {Load}
        AssignFile(F, SaveDialog1.Filename);
        Rewrite(F);
        for i := 0 to (FrenzyLB.Items.Count - 1) do begin
          FStr := FrenzyLB.Items.Strings[i];
          writeln(F, FStr);
        end;
        CloseFile(F);
        FrenzyFilesLoaded := True;
        FrenzyFilesName := FrenzyNameEdit.Text;
      end;
    end;
  end;
end;







end.
