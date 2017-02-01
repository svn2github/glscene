{: ProgressForm<p>

  Progress form for the ManuCAD program<p>

  <b>History :</b><font size=-1><ul>
    <li>13/10/02 - DA - Unit creation
  </ul></font>
}
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TProgress = class(TForm)
    LMessage: TLabel;
    ProgressBar: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }

    //: update the form contents
    procedure UpdateProgress(const Msg: String; ActualPosition, PositionMax: Longint);
  end;

var
  Progress: TProgress;

implementation

{$R *.dfm}

{ TProgress }

// UpdateProgress
//
{: @param Msg                   A message to show
   @param ActualPosition        The actual progress position
   @param PositionMax           The maximum progress position }
procedure TProgress.UpdateProgress(const Msg: String; ActualPosition,
  PositionMax: Integer);
begin
  LMessage.Caption := Msg;
  ProgressBar.Max := PositionMax;
  ProgressBar.Position := ActualPosition;
  Application.ProcessMessages;
end;

end.
