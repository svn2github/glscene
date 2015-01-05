//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS.FRColorEditor<p>

   RGB+Alpha color editor.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>10/08/14 - PW - Converted to FMX
      <li>24/04/09 - DanB - removed some ifdef MSWINDOWS, which were actually for Kylix
      <li>05/09/08 - DanB - Removed Kylix support
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>03/07/04 - LR - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRColorEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Colors, FMX.Edit, FMX.Controls.Presentation,

  GLS.VectorGeometry, GLS.Color, GLS.Texture, GLS.VectorTypes;

type
  TRColorEditor = class(TFrame)
    Panel: TPanel;
    RedEdit: TEdit;
    GreenEdit: TEdit;
    BlueEdit: TEdit;
    AlphaEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ColorBox: TColorBox;
    procedure RedEditChange(Sender: TObject);
    procedure GreenEditChange(Sender: TObject);
    procedure BlueEditChange(Sender: TObject);
    procedure AlphaEditChange(Sender: TObject);
  private
    { Private declarations }
    FOnChange : TNotifyEvent;
    updating : Boolean;
    WorkBitmap : tBitmap;
    RedValue : Integer;
    GreenValue : integer;
    BlueValue : integer;
    AlphaVAlue : integer;
    DraggingValue : (None,Red,Green,Blue,Alpha);
    procedure SetColor(const val : THomogeneousFltVector);
    function GetColor : THomogeneousFltVector;
    Procedure DrawContents;
    Procedure DragColorSliderToPosition(XPos : integer);
    Procedure ContentsChanged;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

const
  MaxColorValue = 255;
  MaxAlphaValue = 1000;

  ColorSliderLeft = 40;
  ColorSliderWidth = 128;
  ColorSliderHeight = 16;
  ColorViewHeight = 7;
  ColorSliderMaxValue = ColorSliderWidth - 2;

  RTop = 8;
  GTop = 30;
  BTop = 52;
  ATop = 74;

  PreviewPanelLeft = 216;
  PreviewPanelTop = 10;
  PreviewPanelWidth = 65;
  PreviewPanelHeight = 74;

  AlphaCheckSize = 9;
  AlphaChecksHigh = 4;
  AlphaChecksWide = 7;

procedure TRColorEditor.RedEditChange(Sender: TObject);
var
  IntValue : integer;
begin
  IntValue := StrToIntDef(RedEdit.Text,-1);

  If (IntValue < 0) or (IntValue > MaxColorValue) then
  begin
    RedEdit.Color:= TColors.clRed;
  end
  else
  begin
    RedEdit.Color:=TColors.clWindow;
    RedValue := IntValue;
    ContentsChanged;
  end;
end;

procedure TRColorEditor.GreenEditChange(Sender: TObject);
var
  IntValue : integer;
begin
  IntValue := StrToIntDef(GreenEdit.Text,-1);

  If (IntValue < 0) or (IntValue > MaxColorValue) then
  begin
    GreenEdit.Color:=TColors.clRed;
  end
  else
  begin
    GreenEdit.Color:=TColors.clWindow;
    GreenValue := IntValue;
    ContentsChanged;
  end;
end;

procedure TRColorEditor.BlueEditChange(Sender: TObject);
var
  IntValue : integer;
begin
  IntValue := StrToIntDef(BlueEdit.Text,-1);

  If (IntValue < 0) or (IntValue > MaxColorValue) then
  begin
    BlueEdit.Color:=TColors.clRed;
  end
  else
  begin
    BlueEdit.Color:=TColors.clWindow;
    BlueValue := IntValue;
    ContentsChanged;
  end;
end;

procedure TRColorEditor.AlphaEditChange(Sender: TObject);
var
  IntValue : integer;
begin
  IntValue := StrToIntDef(AlphaEdit.Text,-1);

  If (IntValue < 0) or (IntValue > MaxAlphaValue) then
  begin
    AlphaEdit.Color:=TColors.clRed;
  end
  else
  begin
    AlphaEdit.Color:=TColors.clWindow;
    AlphaValue := IntValue;
    ContentsChanged;
  end;
end;

end.
