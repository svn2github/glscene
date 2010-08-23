//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLViewer<p>

   Platform independant viewer.<p>

    History:
      <li>23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
      <li>30/04/10 - Yar - Added vertical synchronization cntrol for Linux (by Rustam Asmandiarov aka Predato) 
      <li>17/09/07 - DaStr - Replaced $IFNDEF KYLIX to $IFDEF MSWINDOWS in 
                              SetupVSync() because wgl* functions are Windows-specific
      <li>12/09/07 - DaStr - Fixed SetupVSync() function (Bugtracker ID = 1786279)
                             Made cross-platform code easier to read
      <li>12/07/07 - DaStr - Added SetupVSync
      <li>30/03/07 - DaStr - Another update after the previous fix (removed class())
                             Added TVSyncMode type and constants.
      <li>24/03/07 - DaStr - Update for Windows after the previous fix
      <li>21/03/07 - DaStr - Improved Cross-Platform compatibility
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
      <li>24/01/02 -  EG   - Initial version
}

unit GLViewer;

interface

{$I GLScene.inc}

uses
  OpenGLTokens, GLContext,
  {$IFDEF GLS_DELPHI_OR_CPPB} GLWin32Viewer; {$ENDIF}
  {$IFDEF FPC}
               GLLCLViewer;   {$ENDIF}
type
{$IFDEF FPC}
  TGLSceneViewer = GLLCLViewer.TGLSceneViewer;
{$ELSE}
  TGLSceneViewer = GLWin32Viewer.TGLSceneViewer;
{$ENDIF FPC}

procedure SetupVSync(const AVSyncMode : TVSyncMode);

implementation

uses
  OpenGLAdapter;

procedure SetupVSync(const AVSyncMode : TVSyncMode);
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  if WGL_EXT_swap_control then
  begin
    I := wglGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then wglSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then wglSwapIntervalEXT(0);
    else
       Assert(False);
    end;
  end;
end;
{$ELSE}
{$IFDEF Linux}
begin
  if GLX_SGI_swap_control then
  begin
    case AVSyncMode of
      vsmSync  : glXSwapIntervalSGI(GL_True);
      vsmNoSync: glXSwapIntervalSGI(GL_False);
    else
       Assert(False);
    end;
  end;
end;
{$ELSE}
begin
   Assert(False, 'Not implemented for UNIX!')
end;
{$ENDIF}
{$ENDIF}

end.
