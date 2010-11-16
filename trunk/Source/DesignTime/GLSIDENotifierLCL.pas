
// This unit is part of the GLScene Project, http://glscene.org

{: GLSIDENotifierLCL<p>

   The notifier tracking project opening and closing.<p>
   Now only MaterialManager take a notifying about this events.<p>


  <b>History : </b><font size=-1><ul>
      <li>07/09/10 - Yar - Creation
  </ul></font>
}

unit GLSIDENotifierLCL;

interface

{$I GLScene.inc}

procedure Register;

implementation

uses
  LazIDEIntf, ProjectIntf, IDEMsgIntf, Forms, Controls, BaseClasses, GLSLog;

type
  TGLSIDENotifier = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    function OnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    function OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    function OnProjectBuilding(Sender: TObject): TModalResult;
  end;

var
  GLSIDENotifier: TGLSIDENotifier = nil;

procedure Register;
begin
  if not Assigned(GLSIDENotifier) and Assigned(LazarusIDE) then
    GLSIDENotifier := TGLSIDENotifier.Create;
end;

constructor TGLSIDENotifier.Create;
begin
  LazarusIDE.AddHandlerOnProjectOpened(OnProjectOpened);
  LazarusIDE.AddHandlerOnProjectClose(OnProjectClose);
  LazarusIDE.AddHandlerOnProjectBuilding(OnProjectBuilding);
  GLSLogger.Log('GLScene IDE Notifier Created');
end;

destructor TGLSIDENotifier.Destroy;
begin
  if Assigned(LazarusIDE) then
  begin
    LazarusIDE.RemoveHandlerOnProjectOpened(OnProjectOpened);
    LazarusIDE.RemoveHandlerOnProjectClose(OnProjectClose);
    LazarusIDE.RemoveHandlerOnProjectBuilding(OnProjectBuilding);
    GLSLogger.Log('GLScene IDE Notifier Destroyed');
  end;
end;

function TGLSIDENotifier.OnProjectOpened(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  NotifyGLSceneManagersProjectOpened;
  Result := mrOk;
end;

function TGLSIDENotifier.OnProjectClose(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  NotifyGLSceneManagersProjectClosed;
  Result := mrOk;
end;

function TGLSIDENotifier.OnProjectBuilding(Sender: TObject): TModalResult;
begin
  NotifyGLSceneManagersBeforeCompile;
  Result := mrOk;
end;

initialization

finalization
  GLSIDENotifier.Free;
  GLSIDENotifier := nil;

end.

