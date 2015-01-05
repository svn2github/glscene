//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : FRFaceEditor<p>

  Editor frame for a TGLFaceProperties.<p>

  <b>History : </b><font size=-1><ul>
  <li>06/01/15 - PW - Converted to FMX
  <li>05/09/08 - DanB - Removed Kylix support
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>19/12/06 - DaStr - TRFaceEditor.SetGLFaceProperties bugfixed - Shiness and
  PoligonMode are now updated when FaceProperties are assigned
  <li>03/07/04 - LR  - Make change for Linux
  <li>06/02/00 - Egg - Creation
  </ul></font>
}
unit FRFaceEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FRTrackBarEdit, FRColorEditor;

type
  TRFaceEditor = class(TFrame)
    Label1: TLabel;
    TRTrackBarEdit1: TRTrackBarEdit;
    TabControl: TTabControl;
    TIAmbient: TTabItem;
    TIDiffuse: TTabItem;
    TIEmission: TTabItem;
    TISpecular: TTabItem;
    CEAmbiant: TRColorEditor;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
