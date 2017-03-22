unit uGameObjects;

interface

uses
  Contnrs,

  uGameObject, uSpaceFighter, uFighterControl.User, uBoomAccum;

{С этим объектом работаю классы миссий, они заполняют его, берут оттуда данные
 Класс MainGame разносит эту информацию по соответствующим объектам

 Технически, с этой записью работает как класс TdfMainGame}
var
  dfGameObjects: record
    Player: TdfSpaceFighter;
    UserControl: TdfUserControl;
    GameObjects: TObjectList; //Полный список игровых объектов

    BigBoom: TdfBoomAccum;
  end;

implementation

end.
