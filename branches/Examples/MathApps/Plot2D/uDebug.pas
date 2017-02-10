unit uDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDebugForm = class(TForm)
    DebugMemo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DebugForm: TDebugForm;

implementation

{$R *.dfm}

end.
