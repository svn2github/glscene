//
// This unit is part of the GLScene Project, http://glscene.org
//
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
  LazIDEIntf, ProjectIntf, Forms, GL3xMaterial;

type
  TGLSIDENotifier = class(TObject)
    constructor Create;
    destructor Destroy; override;
    function OnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    function OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    function OnProjectBuilding(Sender: TObject): TModalResult;
  end;

var
  GLSIDENotifier: TGLSIDENotifier;

procedure Register;
begin
  GLSIDENotifier := TGLSIDENotifier.Create;
end;

constructor TGLSIDENotifier.Create;
begin
  LazarusIDE.AddHandlerOnProjectOpened(OnProjectOpened);
  LazarusIDE.AddHandlerOnProjectClose(OnProjectClose);
  LazarusIDE.AddHandlerOnProjectBuilding(OnProjectBuilding);
end;

destructor TGLSIDENotifier.Destroy;
begin
  LazarusIDE.RemoveHandlerOnProjectOpened(OnProjectOpened);
  LazarusIDE.RemoveHandlerOnProjectClose(OnProjectClose);
  LazarusIDE.RemoveHandlerOnProjectBuilding(OnProjectBuilding);
end;

function TGLSIDENotifier.OnProjectOpened(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  MaterialManager.NotifyProjectOpened;
  Result := 0;
end;

function TGLSIDENotifier.OnProjectClose(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  MaterialManager.NotifyProjectClosed;
  Result := 0;
end;

function TGLSIDENotifier.OnProjectBuilding(Sender: TObject): TModalResult;
begin
  // UpdateResource called by IDE itself
  Result := 0;
end;

initialization

finalization
  GLSIDENotifier.Free;
  GLSIDENotifier := nil;

end.

