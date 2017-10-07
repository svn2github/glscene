{
  Создание впадин на freeform'е без его деформации

  Управление:
  ASWDZX - движение камеры
  ЛКМ - вращение камеры
  ПКМ - выкопать яму
  Shift - разместить маркер
}
program GroundDigging;

uses
  Forms,
  main in 'main.pas' {main_form};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tmain_form, main_form);
  Application.Run;
end.
