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
  Classes,
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
var
  ResList: TStringList;
  I: Integer;
  Editor: IOTAEditor;
  Project: IOTAProject;
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  Dest: PByte;
  rName: PChar;
begin
  ResList := MaterialManager.GetResourceList;
  if Assigned(ResList) then
  begin
    Project := GetActiveProject;
    if not Assigned(Project) then
    begin
      ResList.Destroy;
      exit;
    end;

    Resource := nil;
    for I := 0 to Project.GetModuleFileCount - 1 do
    begin
      Editor := Project.GetModuleFileEditor(I);
      if Supports(Editor, IOTAProjectResource, Resource) then
        Break;
    end;

    if Assigned(Resource) then
    begin
      Dest := nil;
      for I := 0 to Resource.GetEntryCount - 1 do
      begin
        ResourceEntry := Resource.GetEntry(I);
        rName := ResourceEntry.GetResourceName;
        if Cardinal(rName)>$1000 then
          if StrComp(rName, PChar(glsMaterialManagerData)) = 0 then
          begin
            ResourceEntry.DataSize := Size;
            Dest := ResourceEntry.GetData;
            break;
          end;
      end;
      if Dest = nil then
      begin
        // Need to create resource
        ResourceEntry := Resource.CreateEntry(GLS_RC_String_Type, PChar(glsMaterialManagerData), 4112, 1033, 0, 0, 0);
        if Assigned(ResourceEntry) then
        begin
          ResourceEntry.DataSize := Size;
          Dest := ResourceEntry.GetData;
        end
        else begin
          MsgServices.AddTitleMessage('Can''t create resource entry');
          ResList.Destroy;
          exit;
        end;
      end;
      Move(ResList.Text, Dest^, Size);
      ResList.Destroy;
      MsgServices.AddTitleMessage('MaterialManager updated resource list');
    end;
  end;
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

