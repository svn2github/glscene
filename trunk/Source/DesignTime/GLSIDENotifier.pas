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
  BaseClasses,
  GLSCrossXML,
  GLStrings,
  ApplicationFileIO;

type
  TGLSIDENotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    FFirstOpen: Boolean;
    function GetProjectResource(const AProject: IOTAProject): IOTAProjectResource;
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

function TGLSIDENotifier.GetProjectResource(const AProject: IOTAProject): IOTAProjectResource;
var
  I: Integer;
  Editor: IOTAEditor;
begin
  for I := 0 to AProject.GetModuleFileCount - 1 do
  begin
    Editor := AProject.GetModuleFileEditor(I);
    if Supports(Editor, IOTAProjectResource, Result) then
      exit;
  end;
  Result := nil;
end;

procedure TGLSIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  ResList: TStringList;
  mStream: TMemoryStream;
  I: Integer;
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  Dest: PByte;
  rName: PChar;
  msg: string;
begin
  NotifyGLSceneManagersBeforeCompile;
  MsgServices.AddTitleMessage('GLScene: resources saved');

  if UpdateGLSceneManagersResourceList then
  begin
    Resource := GetProjectResource(Project);

    if Assigned(Resource) then
    begin
      mStream := TMemoryStream.Create;
      ResList := TStringList.Create;
      ResList.Text := vManagersResourceList;
      ResList.SaveToStream(mStream);
      ResList.Destroy;
      Dest := nil;
      try
        for I := 0 to Resource.GetEntryCount - 1 do
        begin
          ResourceEntry := Resource.GetEntry(I);
          rName := ResourceEntry.GetResourceName;
          if Cardinal(rName) > $1000 then
            if StrComp(rName, PChar(glsResourceInfo)) = 0 then
            begin
              ResourceEntry.DataSize := mStream.Size;
              Dest := ResourceEntry.GetData;
              if not Assigned(Dest) then
              begin
                msg := 'Can''t get address of resource ' + glsResourceInfo;
                Abort;
              end;
              break;
            end;
        end;
        if Dest = nil then
        begin
          // Need to create resource
          ResourceEntry := Resource.CreateEntry(GLS_RC_String_Type, PChar(glsResourceInfo), 4112, 1033, 0, 0, 0);
          if Assigned(ResourceEntry) then
          begin
            ResourceEntry.DataSize := mStream.Size;
            Dest := ResourceEntry.GetData;
          end
          else
          begin
            msg := 'Can''t create resource entry for' + glsResourceInfo;
            Abort;
          end;
        end;
        // Update resource
        Move(mStream.Memory^, Dest^, mStream.Size);
        mStream.Destroy;
        MsgServices.AddTitleMessage('GLScene updated application resource list');
      except
        MsgServices.AddTitleMessage(msg);
        mStream.Destroy;
        exit;
      end;
    end;
  end;
end;

procedure TGLSIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);

  var
    lFileName: string;

  function IsProject: Boolean;
  var
    ext: string;
  begin
    ext := UpperCase(ExtractFileExt(FileName));
    System.Delete(ext, 1, 1);
    Result := ext = 'DPROJ';
  end;

  function IsPackage: Boolean;
  var
    pak: string;
  begin
    pak := UpperCase(ExtractFileName(FileName));
    Result := Pos('GLSCENE_DESIGNTIME', pak) > 0;
  end;

  procedure LoadResourceList;
  var
    XMLDoc: GLSXMLDocument;
    XMLProject: GLSXMLNode;
  begin
    XMLDoc := GLSNewXMLDocument;
    try
      XMLDoc.LoadFromFile(lFileName);
      XMLProject := XMLDoc.DocumentElement;
      if not GetXMLAttribute(XMLProject, 'GLSceneResources', vManagersResourceList) then
        vManagersResourceList := '';
    except
      MsgServices.AddTitleMessage('GLScene: can''t load project resource list');
    end;
  end;

  procedure SaveResourceList;
  var
    XMLDoc: GLSXMLDocument;
    XMLProject: GLSDOMNode;
  begin
    XMLDoc := GLSNewXMLDocument;
    XMLDoc.LoadFromFile(FileName);
    XMLProject := XMLDoc.DOMDocument.DocumentElement;
    SetXMLAttribute(XMLProject, 'GLSceneResources', vManagersResourceList);
    XMLDoc.SaveToFile(FileName);
  end;

begin
  if (NotifyCode = ofnPackageInstalled)
    and IsPackage then
  begin
    FFirstOpen := True;
  end
  else if (NotifyCode = ofnFileOpened)
    and (IsProject or FFirstOpen) then
  begin
    if FFirstOpen then
      lFileName := GetActiveProject.FileName
    else
      lFileName := FileName;
    LoadResourceList;
    NotifyGLSceneManagersProjectOpened;
    FFirstOpen := False;
  end
  else if (NotifyCode = ofnFileClosing)
    and IsProject then
  begin
    if UpdateGLSceneManagersResourceList then
      SaveResourceList;
    NotifyGLSceneManagersProjectClosed;
  end;
end;

initialization

finalization
  RemoveNotifier;

end.

