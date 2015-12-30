//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{: DGLViewer<p>

      Platform independant viewer.<p><p>

 <b>Historique : </b><font size=-1><ul>
      <li>21/12/15 - JD -  Imported From GLScene
 </ul></font>
}

unit DGLViewer;

interface

{$I DGLEngine.inc}

uses
  DGLTypes,
  DGLContext,
  DGLWin32Viewer;

type
  TDGLSceneViewer = DGLWin32Viewer.TDGLSceneViewer;

procedure SetupVSync(const AVSyncMode : TVSyncMode);

implementation

uses
  dglOpenGL;

procedure SetupVSync(const AVSyncMode : TVSyncMode);
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  if WGL_EXT_swap_control then
  begin
    I := WGLGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then WGLSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then WGLSwapIntervalEXT(0);
    else
       Assert(False);
    end;
  end;
end;
{$ENDIF}
{$IFDEF Linux}
begin
  if GLX_SGI_swap_control then
  begin
    case AVSyncMode of
      vsmSync  : GLXSwapIntervalSGI(GL_True);
      vsmNoSync: GLXSwapIntervalSGI(GL_False);
    else
       Assert(False);
    end;
  end;
end;
{$ENDIF}
{$IFDEF DARWIN}
var ctx: TAGLContext;
const ISync: Integer = 0;
      INoSync: Integer = 1;
begin
    ctx := GetCurrentContext();
    if Assigned(ctx) then
      case AVSyncMode of
        vsmSync  : GLaSetInteger(ctx, AGL_SWAP_INTERVAL, @ISync);
        vsmNoSync: GLaSetInteger(ctx, AGL_SWAP_INTERVAL, @INoSync);
      else
         Assert(False);
      end;
end;
{$ENDIF}

end.
