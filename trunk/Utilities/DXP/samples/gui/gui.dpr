program gui;

uses Forms, Dialogs {$IFDEF FPC}, Interfaces{$ENDIF};

var
   frm : TForm;
begin
   Application.Initialize;

   frm:=TForm.Create(nil);
   frm.Caption:='Hello World!';
   frm.Position:=poScreenCenter;

   frm.ShowModal;

   frm.Free;
end.
