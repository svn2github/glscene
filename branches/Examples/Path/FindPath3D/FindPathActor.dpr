// * Xeno(C)2005 mail: glscene@nm.ru
// * http://glscene.nm.ru

program FindPathActor;

uses
  Forms,
  UnitMain888 in 'UnitMain888.pas' {FormShoot};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormShoot, FormShoot);
  Application.Run;
end.
