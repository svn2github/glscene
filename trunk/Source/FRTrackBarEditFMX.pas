//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRTrackBarEdit<p>

   Frame combining a TrackBar and an Edit.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>10/08/14 - PW - Upgraded to support FireMonkey platform
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRTrackBarEditFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit;

type
  TRTrackBarEdit = class(TFrame)
    TrackBar: TTrackBar;
    Edit: TEdit;
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetValue(const val : Single);
    function GetValue : Single;
    procedure SetValueMin(const val : Single);
    function GetValueMin : Single;
    procedure SetValueMax(const val : Single);
    function GetValueMax : Single;
  public
    { Public declarations }
    property Value : Single read GetValue write SetValue;
    property ValueMin : Single read GetValueMin write SetValueMin;
    property ValueMax : Single read GetValueMax write SetValueMax;
  end;

implementation

{$R *.fmx}

procedure TRTrackBarEdit.TrackBarChange(Sender: TObject);
begin
   Edit.Text:=FloatToStr(TrackBar.Value);
end;

procedure TRTrackBarEdit.EditChange(Sender: TObject);
var
   I : Integer;
begin
   try
      I := StrToInt(Edit.Text);
      TrackBar.Value := I;
   except
      // ignore
   end;
end;


// SetValue
//
procedure TRTrackBarEdit.SetValue(const val: Single);
begin
   TrackBar.Value:=val;
   TrackBarChange(Self);
end;

// GetValue
//
function TRTrackBarEdit.GetValue: Single;
begin
   Result:=TrackBar.Value;
end;

// SetValueMax
//
procedure TRTrackBarEdit.SetValueMax(const val: Single);
begin
   TrackBar.Max:=val;
   TrackBarChange(Self);
end;

// GetValueMax
//
function TRTrackBarEdit.GetValueMax: Single;
begin
   Result:=TrackBar.Max;
end;

// SetValueMin
//
procedure TRTrackBarEdit.SetValueMin(const val: Single);
begin
   TrackBar.Min:=val;
   TrackBarChange(Self);
end;

// GetValueMin
//
function TRTrackBarEdit.GetValueMin: Single;
begin
   Result:=TrackBar.Min;
end;

end.
