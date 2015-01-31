{ : Tweener interpolation demo.<p>

  Original Tweener (caurina.transitions.Tweener) is a Class used to create tweenings
  and other transitions via ActionScript code for projects built on the Flash platform.

  Current unit is a demonstration project of the GLScene's delphi implementation of
  the tweener library : AnimationUtils.pas

  <b>History : </b><font size=-1><ul>
  <li>11/10/12 - YP - Created by Yann Papouin
  </ul>
}

program interpolation;

uses
  Forms,
  Unit1 in 'Unit1.pas' { Form1 } ;
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
