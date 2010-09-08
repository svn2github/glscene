//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialCode<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialCode;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_DELPHI_OR_CPPB}
  Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
{$IFDEF FPC}
  LResources, LMessages,
{$ENDIF}
  Dialogs, StdCtrls;

type
  TMaterialCodeForm = class(TForm)
    CodePad: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
{$IFDEF FPC}
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
{$ELSE}
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
{$ENDIF}
  public
    { Public declarations }
    NotifyClose: TNotifyEvent;
    Docked: Boolean;
    LastChangePosByMainForm: Boolean;
  end;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {.$R *.lfm}
{$ENDIF}

{$IFDEF FPC}
procedure TMaterialCodeForm.WMMove(var Message: TLMMove);
{$ELSE}
procedure TMaterialCodeForm.WMMove(var Message: TWMMove);
{$ENDIF}
begin
  if LastChangePosByMainForm then
    LastChangePosByMainForm := False
  else
    Docked := False;
  inherited;
end;

procedure TMaterialCodeForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  NotifyClose(Self);
end;

end.
