//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSIDENotifier<p>

   The notifier tracking project opening and closing.<p>
   Now only MaterialManager take a notifying about this events.<p>


	<b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
	</ul></font>
}

unit GLSIDENotifier;

interface

procedure Register;

implementation

uses
  SysUtils,
  TypInfo,
  ToolsAPI,
  GL3xMaterial;

type
  TGLSIDENotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  end;

var
  NotifierIndex: Integer;

procedure Register;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  NotifierIndex := Services.AddNotifier(TGLSIDENotifier.Create);
end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  if NotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    Services.RemoveNotifier(NotifierIndex);
  end;
end;

function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;

procedure TGLSIDENotifier.AfterCompile(Succeeded: Boolean);
begin
  //MsgServices.AddTitleMessage('After Compile');
end;

procedure TGLSIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  //MsgServices.AddTitleMessage('Before Compile');
end;

procedure TGLSIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);

  function IsProject: Boolean;
  var
    ext: string;
  begin
    ext := UpperCase(ExtractFileExt(FileName));
    System.Delete(ext, 1, 1);
    Result := ext = 'DPROJ';
  end;

begin
  if (NotifyCode = ofnFileOpened)
    and IsProject then
  begin
    MaterialManager.NotifyProjectOpened;
  end
  else if (NotifyCode = ofnFileClosing)
    and IsProject then
  begin
    MaterialManager.NotifyProjectClosed;
  end;
end;

initialization

finalization
  RemoveNotifier;

end.

