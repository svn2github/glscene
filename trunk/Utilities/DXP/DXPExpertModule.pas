// DXPExpertModule
{
   Provides MenuItems, common resources, and event handling for the DXP Expert.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPExpertModule;

interface

uses
  Windows, SysUtils, Forms, Classes, Menus, ToolsAPI, Dialogs, ActnList, ImgList,
  Graphics, Controls, DXPFPCConfig;

type
  TDMDXPExpertModule = class(TDataModule)
    PMFreePascal: TPopupMenu;
    MIExecute: TMenuItem;
    N2: TMenuItem;
    MICompile: TMenuItem;
    MIBuild: TMenuItem;
    ActionList: TActionList;
    ACFPCExecute: TAction;
    ACFPCBuild: TAction;
    ACFPCCompile: TAction;
    ACFPCOptions: TAction;
    PMDXP: TPopupMenu;
    MenuItem1: TMenuItem;
    ACDXPOptions: TAction;
    N1: TMenuItem;
    View1: TMenuItem;
    MICompilerMessages: TMenuItem;
    ACViewCompilerMessages: TAction;
    N3: TMenuItem;
    Options1: TMenuItem;
    procedure ACDXPOptionsExecute(Sender: TObject);
    procedure ACFPCCompileExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ACViewCompilerMessagesExecute(Sender: TObject);
    procedure ACFPCExecuteExecute(Sender: TObject);
    procedure ACFPCBuildExecute(Sender: TObject);
    procedure ACFPCOptionsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    { Déclarations privées }
    FFPCConfig : TDXPFPCConfig;
    FFPCConfigFileName : String;

    procedure AddMenuInIDE(popup : TPopupMenu; const aDelphiMenu : String);
    function GetProjectGroup : IOTAProjectGroup;
    function GetProject : IOTAProject;
    function GetModule(const moduleName : String) : IOTAModule;
    function ProjectBinaryName : String;

    function FPCConfig : TDXPFPCConfig;
    function FPCCommandLine(const extraOptions : String = '') : String;
    function FPCErrorFile : String;
    //: Returns True if compilation succeeded
    function FPCCompile(const extraOptions : String = '') : Boolean;

  public
    { Déclarations publiques }
    FOTAServices : IOTAServices;
    FNTAServices : INTAServices;

    procedure HookIDE;
    procedure UnHookIDE;

    procedure WarpTo(const moduleName : String; col, line : Integer);
  end;

implementation

{$R *.dfm}

uses DXPGlobals, FDXPOptions, DXPUtils, FDXPCompileLog, FDXPFPCOptions;

procedure TDMDXPExpertModule.DataModuleCreate(Sender: TObject);
begin
   FFPCConfig:=nil; // initialized upon request
end;

procedure TDMDXPExpertModule.DataModuleDestroy(Sender: TObject);
begin
   FFPCConfig.Free;
end;

procedure TDMDXPExpertModule.AddMenuInIDE(popup : TPopupMenu; const aDelphiMenu : String);
var
   k, i : Integer;
   mm : TMainMenu;
begin
   mm:=FNTAServices.MainMenu;
   k:=-1;
   if aDelphiMenu<>'' then for i:=0 to mm.Items.Count-1 do begin
      if mm.Items[i].Name=aDelphiMenu then begin
         k:=i;
         Break;
      end;
   end;
   if k>=0 then
      mm.Items.Insert(k+1, popup.Items)
   else mm.Items.Add(popup.Items);
   popup.Items.Caption:=Copy(popup.Name, 3, MaxInt);
   popup.Images:=mm.Images;
end;

procedure TDMDXPExpertModule.HookIDE;
begin
   ActionList.Images:=FNTAServices.MainMenu.Images;
   AddMenuInIDE(PMFreePascal, 'RunMenu');
   AddMenuInIDE(PMDXP, '');
end;

procedure TDMDXPExpertModule.UnHookIDE;
var
   mm : TMainMenu;
begin
   mm:=FNTAServices.MainMenu;
   mm.Items.Remove(PMFreePascal.Items);
   mm.Items.Remove(PMDXP.Items);
end;

function TDMDXPExpertModule.GetProjectGroup : IOTAProjectGroup;
var
   IModuleServices : IOTAModuleServices;
   i : Integer;
begin
   Result:=nil;
   IModuleServices:=BorlandIDEServices as IOTAModuleServices;
   for i:=0 to IModuleServices.ModuleCount-1 do
      if Supports(IModuleServices.Modules[i], IOTAProjectGroup, Result) then
         Break;
end;

function TDMDXPExpertModule.GetProject : IOTAProject;
var
   grp : IOTAProjectGroup;
begin
   grp:=GetProjectGroup;
   if grp<>nil then
      Result:=grp.ActiveProject
   else Result:=nil;
end;

function TDMDXPExpertModule.ProjectBinaryName : String;
var
   prj : IOTAProject;
begin
   prj:=GetProject;
   if Assigned(prj) then
      Result:=ChangeFileExt(prj.FileName, '.exe')
   else Result:='';
end;

function TDMDXPExpertModule.GetModule(const moduleName : String) : IOTAModule;
var
   i : Integer;
   modules : IOTAModuleServices;
begin
   modules:=(BorlandIDEServices as IOTAModuleServices);
   Result:=nil;
   for i:=0 to modules.ModuleCount-1 do begin
      if CompareText(ExtractFileName(modules.Modules[i].FileName), moduleName)=0 then begin
         Result:=modules.Modules[i];
         Break;
      end;
   end;
end;

procedure TDMDXPExpertModule.WarpTo(const moduleName : String; col, line : Integer);
var
   i, j : Integer;
   module : IOTAModule;
   editor : IOTASourceEditor;
   editPos : TOTAEditPos;
   view : IOTAEditView;
   fileName : String;
begin
   module:=GetModule(moduleName);
   if not Assigned(module) then begin
      fileName:=FindFileInPaths(moduleName, vFPC_SourcePaths);
      if fileName<>'' then begin
         (BorlandIDEServices as IOTAActionServices).OpenFile(fileName);
         module:=GetModule(moduleName);
      end;
   end;
   if Assigned(module) then begin
      for i:=0 to module.ModuleFileCount-1 do begin
         module.ModuleFileEditors[i].QueryInterface(IOTASourceEditor, editor);
         if Assigned(editor) then begin
            editor:=(module.ModuleFileEditors[i] as IOTASourceEditor);
            editor.Show;
            editPos.Col:=col;
            editPos.Line:=line;
            for j:=0 to editor.EditViewCount-1 do begin
               view:=editor.EditViews[j];
               view.CursorPos:=editPos;
               view.MoveViewToCursor;
               view.Paint;
            end;
         end;
      end;
   end;
end;

function TDMDXPExpertModule.FPCConfig : TDXPFPCConfig;
var
   cfgFileName : String;
begin
   cfgFileName:=ChangeFileExt(GetProject.FileName, '.fpc-cfg');
   if cfgFileName<>FFPCConfigFileName then
      FreeAndNil(FFPCConfig);
   if not Assigned(FFPCConfig) then begin
      FFPCConfig:=TDXPFPCConfig.Create;
      FFPCConfigFileName:=cfgFileName;
      if FileExists(FFPCConfigFileName) then
         FFPCConfig.LoadFromFile(FFPCConfigFileName);
   end;
   Result:=FFPCConfig;
end;

function TDMDXPExpertModule.FPCCommandLine(const extraOptions : String = '') : String;
var
   i : Integer;
   prj : IOTAProject;
   paths : TStringList;
   configOptions : String;
   config : TDXPFPCConfig;
begin
   Result:='';
   prj:=GetProject;
   if not Assigned(prj) then Exit;
   configOptions:='';
   config:=FPCConfig;
   for i:=0 to config.Options.Count-1 do
      configOptions:=configOptions+' '+config.Options[i];
   Result:= vFPC_BinaryPath+'\fpc.exe '+extraOptions
           +' -Sd'+configOptions
           +' -Fe'+FPCErrorFile;
   paths:=TStringList.Create;
   try
      paths.Delimiter:=';';
      paths.CommaText:=vFPC_SourcePaths;
      for i:=0 to paths.Count-1 do
         Result:=Result+' -Fu"'+paths[i]+'"';
   finally
      paths.Free;
   end;
   Result:=Result+' "'+prj.FileName+'"';
end;

function TDMDXPExpertModule.FPCErrorFile : String;
begin
   Result:='c:\dxp.tmp';//GetTemporaryFilesPath+'dxp.tmp';
end;

function TDMDXPExpertModule.FPCCompile(const extraOptions : String = '') : Boolean;
var
   res : Integer;
   cmdLine, verbose : String;
begin
   Result:=False;
   LoadDXPGlobals;
   cmdLine:=FPCCommandLine(extraOptions);
   if cmdLine='' then Exit;
   Screen.Cursor:=crHourGlass;
   try
      verbose:=FPCErrorFile;
      DeleteFile(verbose);
      res:=ExecuteAndWait(cmdLine, SW_SHOW, vFPC_TimeOut, True);
      if res=-1 then
         ShowMessage('Failed to start compiler'#13#10+cmdLine)
      else begin
         if res=0 then
            Result:=True;
         DXPCompileLog.ExecuteOnFPC(verbose, Self);
         with DXPCompileLog.MERaw.Lines do begin
            Insert(0, cmdLine);
            Insert(1, '');
         end;
      end;
   finally
      DeleteFile(verbose);
      Screen.Cursor:=crDefault;
   end;
end;

procedure TDMDXPExpertModule.ACFPCExecuteExecute(Sender: TObject);
begin
   if FPCCompile then
      WinExec(PChar(ProjectBinaryName), SW_SHOW)
   else ShowMessage('Compilation failed!');
end;

procedure TDMDXPExpertModule.ACFPCCompileExecute(Sender: TObject);
begin
   (BorlandIDEServices as IOTAModuleServices).SaveAll;
   if FPCCompile then
      ShowMessage('Compilation successful!')
   else ShowMessage('Compilation failed!');
end;

procedure TDMDXPExpertModule.ACFPCBuildExecute(Sender: TObject);
begin
   (BorlandIDEServices as IOTAModuleServices).SaveAll;
   if FPCCompile('-B') then
      ShowMessage('Build successful!')
   else ShowMessage('Build failed!');
end;

procedure TDMDXPExpertModule.ACDXPOptionsExecute(Sender: TObject);
begin
   LoadDXPGlobals;
   with TDXPOptions.Create(nil) do begin
      try
         if Execute then
            StoreDXPGlobals;
      finally
         Free;
      end;
   end;
end;

procedure TDMDXPExpertModule.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
   gotProject : Boolean;
begin
   gotProject:=(GetProject<>nil);
   ACFPCCompile.Enabled:=gotProject;
   ACFPCBuild.Enabled:=gotProject;
   ACFPCExecute.Enabled:=gotProject;
   ACFPCOptions.Enabled:=gotProject;
   ACViewCompilerMessages.Checked:=DXPCompileLogVisible;
   Handled:=True;
end;

procedure TDMDXPExpertModule.ACViewCompilerMessagesExecute(
  Sender: TObject);
begin
   if DXPCompileLog.Visible then
      DXPCompileLog.Hide
   else DXPCompileLog.Show;
end;

procedure TDMDXPExpertModule.ACFPCOptionsExecute(Sender: TObject);
var
   config : TDXPFPCConfig;
begin
   with TDXPFPCOptions.Create(nil) do begin
      config:=FPCConfig;
      if Execute(config.Options) then
         config.SaveToFile(FFPCConfigFileName);
      Free;
   end;
end;

end.
