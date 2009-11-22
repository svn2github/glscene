//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneRegisterWinOnlyLCL<p>

   Contains registration for Design-Time Lazarus Windows-Only units.<p>
   Because of Lazarus'es limitations, these need to be separated from the main
   GLSceneRegisterLCL.pas

   <b>History :</b><font size=-1><ul>
      <li>22/11/09 - DaStr - Initial version (by Predator)
   </ul></font>
}

unit GLSceneRegisterWinOnlyLCL;
{$IFDEF UNIX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

interface

uses

   Classes, ComponentEditors, PropEdits, LResources;
procedure Register;

implementation

uses
   GLLCLFullScreenViewer,
   GLAVIRecorder, Joystick, ScreenSaver;


procedure Register;
begin
   RegisterComponents('GLScene',
                      [
                       TGLFullScreenViewer
                      ]);

   RegisterComponents('GLScene Utils',
                      [TAVIRecorder,  TJoystick, TScreenSaver
                      ]);

end;

initialization

   {$I nonGLSceneLCL.lrs}

finalization
end.
