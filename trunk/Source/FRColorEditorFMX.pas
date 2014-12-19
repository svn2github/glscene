//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRColorEditorFMX<p>

   RGB+Alpha color editor.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>10/08/14 - PW - Upgraded to support FireMonkey controls
      <li>24/04/09 - DanB - removed some ifdef MSWINDOWS, which were actually for Kylix
      <li>05/09/08 - DanB - Removed Kylix support
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>03/07/04 - LR - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRColorEditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Colors, FMX.Edit, FMX.Controls.Presentation;

type
  TRColorEditor = class(TFrame)
    Panel: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    ColorBox1: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
