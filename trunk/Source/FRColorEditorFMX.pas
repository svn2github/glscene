//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRColorEditor<p>

   RGB+Alpha color editor.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>10/08/14 - PW - Upgraded to support FireMonkey controls
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRColorEditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Colors, FMX.Edit;

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
