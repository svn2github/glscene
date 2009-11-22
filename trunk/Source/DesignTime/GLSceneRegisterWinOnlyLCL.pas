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

{$IFDEF UNIX}{$Message Error 'Unit not supported'}{$ENDIF}

interface

uses
   GLLCLFullScreenViewer,
   GLAVIRecorder, Joystick, ScreenSaver, GLSMWaveOut;


procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLSMWaveOut,
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