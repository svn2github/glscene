unit FXCollectionEditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, System.Actions, FMX.ActnList, FMX.Menus, FMX.ListView,

 //GLS
  GLCrossPlatform, XCollection;

type
  TXCollectionEditor = class(TForm)
    ToolBar1: TToolBar;
    TBAdd: TButton;
    ListView: TListView;
    PMListView: TPopupMenu;
    PMToolBar: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  XCollectionEditor: TXCollectionEditor;

implementation

{$R *.fmx}

procedure TXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

procedure TXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

procedure TXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

end.
