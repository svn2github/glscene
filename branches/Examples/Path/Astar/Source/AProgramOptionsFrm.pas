unit AProgramOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TAProgramOptionsForm = class(TForm)
    EnemyPositionsColorBtn: TBitBtn;
    EnemyUnitGoalColorBtn: TBitBtn;
    EnemyUnitBaseColorBtn: TBitBtn;
    Target1ColorBtn: TBitBtn;
    Target2ColorBtn: TBitBtn;
    Target3ColorBtn: TBitBtn;
    Target4ColorBtn: TBitBtn;
    Target5ColorBtn: TBitBtn;
    Base5ColorBtn: TBitBtn;
    Base4ColorBtn: TBitBtn;
    Base3ColorBtn: TBitBtn;
    Base2ColorBtn: TBitBtn;
    Base1ColorBtn: TBitBtn;
    SPathColorBtn: TBitBtn;
    GroundColorBtn: TBitBtn;
    ObstacleColorBtn: TBitBtn;
    GridColorBtn: TBitBtn;
    AStarDataDisplayArrowsColorRG: TRadioGroup;
    OpenaffBtn: TSpeedButton;
    FileAffEdit: TEdit;
    SaveaffBtn: TSpeedButton;
    ColorDialog1: TColorDialog;
    OpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
procedure SetColors;
Procedure makecolor(btn:TBitBtn;color:TColor;Shape:Integer);

    procedure OpenaffBtnClick(Sender: TObject);
    procedure SaveaffBtnClick(Sender: TObject);
    procedure GridColorBtnClick(Sender: TObject);
    procedure ObstacleColorBtnClick(Sender: TObject);
    procedure GroundColorBtnClick(Sender: TObject);
    procedure SPathColorBtnClick(Sender: TObject);
    procedure Base1ColorBtnClick(Sender: TObject);
    procedure Target1ColorBtnClick(Sender: TObject);
    procedure EnemyUnitBaseColorBtnClick(Sender: TObject);
    procedure EnemyUnitGoalColorBtnClick(Sender: TObject);
    procedure EnemyPositionsColorBtnClick(Sender: TObject);
    procedure AStarDataDisplayArrowsColorRGClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AProgramOptionsForm: TAProgramOptionsForm;

implementation
uses AStarGlobals;
{$R *.DFM}

procedure TAProgramOptionsForm.FormCreate(Sender: TObject);
begin
//  left := AProjectOptionsFormX;
//  top := AProjectOptionsFormY;
end;

procedure TAProgramOptionsForm.FormShow(Sender: TObject);
begin
//
end;

procedure TAProgramOptionsForm.FormActivate(Sender: TObject);
begin
  SetColors;
end;

procedure TAProgramOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//  AProjectOptionsFormX := AProjectOptionsForm.left;
//  AProjectOptionsFormY := AProjectOptionsForm.top;
end;

procedure TAProgramOptionsForm.SetColors;
Begin
  makecolor(GroundColorBtn,BackGroundColor,1);
  makecolor(GridColorBtn,GridColor,1);
  makecolor(ObstacleColorBtn,ObstacleColor,1);
  makecolor(SPathColorBtn,PathColor,2);
  makecolor(Base1ColorBtn,BaseStartColorArray[1],1);
  makecolor(Base2ColorBtn,BaseStartColorArray[2],1);
  makecolor(Base3ColorBtn,BaseStartColorArray[3],1);
  makecolor(Base4ColorBtn,BaseStartColorArray[4],1);
  makecolor(Base5ColorBtn,BaseStartColorArray[5],1);
  makecolor(Target1ColorBtn,TargetGoalColorArray[1],2);
  makecolor(Target2ColorBtn,TargetGoalColorArray[2],2);
  makecolor(Target3ColorBtn,TargetGoalColorArray[3],2);
  makecolor(Target4ColorBtn,TargetGoalColorArray[4],2);
  makecolor(Target5ColorBtn,TargetGoalColorArray[5],2);
  makecolor(EnemyPositionsColorBtn,EnemyPositionsColor,1);
  makecolor(EnemyUnitBaseColorBtn,EnemyUnitBaseColor,1);
  makecolor(EnemyUnitGoalColorBtn,EnemyUnitGoalTargetColor,2);
  AStarDataDisplayArrowsColorRG.ItemIndex:=AStarDataDisplayArrowsColor;
End;

{********************** MakeColor ***************}
//Cloned from Maze Generator
Procedure TAProgramOptionsForm.makecolor(btn:TBitBtn;color:TColor;Shape:Integer);
var
  mybitmap:TBitmap;
Begin
  mybitmap:=tbitmap.create;
  mybitmap.width:=btn.glyph.width;
  mybitmap.height:=btn.glyph.height;
  with btn, mybitmap do
  Begin
    canvas.Brush.color:=color;
    case Shape of
    1:canvas.rectangle(1,1,glyph.width-1,glyph.height-1);
    2:canvas.Ellipse(1,1,glyph.width-1,glyph.height-1);
    end;
    glyph.assign(mybitmap);
  end;
  mybitmap.free;
End;

procedure TAProgramOptionsForm.OpenaffBtnClick(Sender: TObject);
begin
  OpenDialog.Filter:= 'astar Colors.aff|*.aff';
  OpenDialog.InitialDir:=ProjectDirectory;
  OpenDialog.FileName:='*.aff';
  if OpenDialog.Execute then
  begin
  Application.ProcessMessages;
  FileAffEdit.Text:= OpenDialog.FileName;
//  ProjectDirectory:=ExtractFilePath(OpenDialog.FileName);
  LoadColorsFile(OpenDialog.FileName);
  SetColors;//Sets Colors
  end;
end;

procedure TAProgramOptionsForm.SaveaffBtnClick(Sender: TObject);
var
  F: TextFile;
  S: string;
Begin
  SaveDialog1.Filter:= 'astar Colors.aff|*.aff';
  SaveDialog1.InitialDir:=ProjectDirectory;
  SaveDialog1.FileName:='';//'myTerrainData.aof';
  if savedialog1.execute then
  Begin
    if uppercase(extractfileext(savedialog1.filename))='.AFF' then
    begin
      Application.ProcessMessages;
      AssignFile(F, savedialog1.filename);
      Rewrite(F);
      Writeln(F,'AStar Colors File');
      S:=Inttostr(1); Writeln(F,S);
      S:=Inttostr(GridColor); Writeln(F,S);
      S:=Inttostr(ObstacleColor); Writeln(F,S);
      S:=Inttostr(BackGroundColor); Writeln(F,S);
      S:=Inttostr(PathColor); Writeln(F,S);
      S:=Inttostr(AStarDataDisplayArrowsColor); Writeln(F,S);
      S:=Inttostr(BaseStartColorArray[1]); Writeln(F,S);
      S:=Inttostr(BaseStartColorArray[2]); Writeln(F,S);
      S:=Inttostr(BaseStartColorArray[3]); Writeln(F,S);
      S:=Inttostr(BaseStartColorArray[4]); Writeln(F,S);
      S:=Inttostr(BaseStartColorArray[5]); Writeln(F,S);
      S:=Inttostr(TargetGoalColorArray[1]); Writeln(F,S);
      S:=Inttostr(TargetGoalColorArray[2]); Writeln(F,S);
      S:=Inttostr(TargetGoalColorArray[3]); Writeln(F,S);
      S:=Inttostr(TargetGoalColorArray[4]); Writeln(F,S);
      S:=Inttostr(TargetGoalColorArray[5]); Writeln(F,S);
      S:=Inttostr(EnemyPositionsColor); Writeln(F,S);
      S:=Inttostr(EnemyUnitBaseColor); Writeln(F,S);
      S:=Inttostr(EnemyUnitGoalTargetColor); Writeln(F,S);
      //Future Proofing
      //If Version > 1 then       begin       end;
      CloseFile(F);
    end;
  End;
end;


procedure TAProgramOptionsForm.GridColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    GridColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.ObstacleColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    ObstacleColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.GroundColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    BackGroundColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.SPathColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,2);
    PathColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.Base1ColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    BaseStartColorArray[TComponent(Sender).Tag]:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.Target1ColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,2);
    TargetGoalColorArray[TComponent(Sender).Tag]:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.EnemyUnitBaseColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    EnemyUnitBaseColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.EnemyUnitGoalColorBtnClick(Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,2);
    EnemyUnitGoalTargetColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.EnemyPositionsColorBtnClick(
  Sender: TObject);
begin
  If colordialog1.Execute then
  Begin
    makecolor(TBitBtn(sender),colordialog1.color,1);
    EnemyPositionsColor:=colordialog1.Color;
  end;
end;

procedure TAProgramOptionsForm.AStarDataDisplayArrowsColorRGClick(
  Sender: TObject);
begin
  AStarDataDisplayArrowsColor:=AStarDataDisplayArrowsColorRG.ItemIndex;

end;

end.
