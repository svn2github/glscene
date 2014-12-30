//
// This unit is part of the GLScene Project, http://glscene.org
//
// FXCollectionEditor
{ : Egg<p>

  Edits a TXCollection<p>

  <b>History : </b><font size=-1><ul>
  <li>18/12/14 - PW - Upgraded to support FMX
  <li>20/01/11 - DanB - Collection items are now grouped by ItemCategory
  <li>16/06/10 - YP - Fixed IDE exception when item removed
  <li>05/10/08 - DanB - removed Kylix support + some other old ifdefs
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>03/07/04 - LR - Make change for Linux
  <li>12/07/03 - DanB - Fixed crash when owner deleted
  <li>27/02/02 - Egg - Fixed crash after item deletion
  <li>11/04/00 - Egg - Fixed crashes in IDE
  <li>06/04/00 - Egg - Creation
  </ul></font>
}
unit FXCollectionEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, System.Actions, FMX.ActnList, FMX.Menus, FMX.ListView,

  
  GLS.CrossPlatform, XCollection;

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
