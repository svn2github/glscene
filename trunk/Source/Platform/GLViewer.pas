//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLViewer<p>

   Platform independant viewer.<p>

    History:
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>24/01/02 -  EG   - Initial version
}

unit GLViewer;

interface

{$I GLScene.inc}

uses
  {$IFDEF LINUX}GLLinuxViewer; {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}GLWin32Viewer; {$ENDIF MSWINDOWS}

type
{$IFDEF UNIX}
  TGLSceneViewer = class(TGLLinuxSceneViewer);
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
  TGLSceneViewer = class(TGLWin32SceneViewer);
{$ENDIF MSWINDOWS}

implementation

end.
 
