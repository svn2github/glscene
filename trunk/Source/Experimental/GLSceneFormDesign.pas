//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneFormDesign<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GLSceneFormDesign;

interface

{$I GLScene.inc}

uses
  Windows,
  Classes,
  ToolsAPI;

type

  // TGLSceneFormWizard
  //

  TGLSceneFormWizard = class(
      TNotifierObject,
      IOTAWizard,
      IOTARepositoryWizard,
      IOTAFormWizard,
      IOTACreator,
      IOTAModuleCreator,
      IOTARepositoryWizard60
{$IFDEF GLS_DELPHI_8_UP}, IOTARepositoryWizard80{$ENDIF}
{$IFDEF GLS_DELPHI_2010_UP}, IOTAProjectWizard100{$ENDIF})
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  protected
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard / IOTAFormWizard methods
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    // IOTACreator methods
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator methods
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent,
      AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent,
      AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);

    { IOTARepositoryWizard60 }
    function GetDesigner: string;

{$IFDEF GLS_DELPHI_8_UP}
    { IOTARepositoryWizard80 }
    function GetGalleryCategory: IOTAGalleryCategory; virtual;
    function GetPersonality: string; virtual;
{$ENDIF}

{$IFDEF GLS_DELPHI_2010_UP}
    { IOTAProjectWizard100 }
    function IsVisible(Project: IOTAProject): Boolean;
{$ENDIF}

{$IFDEF GLS_DELPHI_8_UP}
    property Personality: string read GetPersonality;
{$ENDIF}
  end;

procedure Register;

implementation

{$R *.res}

uses
  Forms,
  GLScene,
  GLSceneForm,
  GLCadencer,
  SysUtils,
  DesignIntf,
  DesignEditors;

type

  TBaseFile = class(TInterfacedObject)
  private
    FModuleName: string;
    FFormName: string;
    FAncestorName: string;
  public
    constructor Create(const ModuleName, FormName, AncestorName: string);
  end;

  TUnitFile = class(TBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TFormFile = class(TBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

procedure Register;
begin
  RegisterCustomModule(TGLSceneForm, TCustomModule);
  RegisterPackageWizard(TGLSceneFormWizard.Create);
end;

// TBaseFile
//

constructor TBaseFile.Create(const ModuleName, FormName, AncestorName: string);
begin
  inherited Create;
  FModuleName := ModuleName;
  FFormName := FormName;
  FAncestorName := AncestorName;
end;

// TUnitFile
//

function TUnitFile.GetSource: string;
const
  sSource =
    'unit %0:s;' + #13#10 +
    '' + #13#10 +
    'interface' + #13#10 +
    '' + #13#10 +
    'uses' + #13#10 +
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,' + #13#10 +
    '  GLScene, GLSceneForm, GLCadencer;' + #13#10 +
    '' + #13#10 +
    'type' + #13#10 +
    '  T%1:s = class(TGLSceneForm)' + #13#10 +
    '    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + #13#10 +
    '  private' + #13#10 +
    '    { Private declarations }' + #13#10 +
    '  public' + #13#10 +
    '    { Public declarations }' + #13#10 +
    '  end;' + #13#10 +
    '' + #13#10 +
    'var' + #13#10 +
    '  %1:s : T%1:s;' + #13#10 +
    '' + #13#10 +
    'implementation' + #13#10 +
    '' + #13#10 +
    '{$R *.dfm}' + #13#10 +
    '' + #13#10 +
    'procedure T%1:s.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);' + #13#10 +
    'begin' + #13#10 +
    '  Invalidate;' + #13#10 +
    'end;' + #13#10 +
    '' + #13#10 +
    'end.';
begin
  Result := Format(sSource, [FModuleName, FFormName]);
end;

function TUnitFile.GetAge: TDateTime;
begin
  Result := -1;
end;

// TFormFile
//

function TFormFile.GetSource: string;
const
  FormText =
    'object %0:s: T%0:s' + #13#10 +
    ' Buffer.BackgroundColor = 2064383' + #13#10 +
    ' Camera = GLCamera1' + #13#10 +
    ' object GLScene1: TGLScene' + #13#10 +
    '  Left = 24' + #13#10 +
    '  Top = 24' + #13#10 +
    '  object GLLightSource1: TGLLightSource' + #13#10 +
    '    ConstAttenuation = 1.000000000000000000' + #13#10 +
    '    Position.Coordinates = {000000400000803F000080400000803F}' + #13#10 +
    '    SpotCutOff = 180.000000000000000000' + #13#10 +
    '  end' + #13#10 +
    '  object GLCamera1: TGLCamera' + #13#10 +
    '    DepthOfView = 100.000000000000000000' + #13#10 +
    '    FocalLength = 50.000000000000000000' + #13#10 +
    '    TargetObject = GLDummyCube1' + #13#10 +
    '    Position.Coordinates = {0000803F00000040000040400000803F}' + #13#10 +
    '  end' + #13#10 +
    '  object GLDummyCube1: TGLDummyCube' + #13#10 +
    '  end ' + #13#10 +
    ' end' + #13#10 +
    ' object GLCadencer1: TGLCadencer' + #13#10 +
    '   Scene = GLScene1' + #13#10 +
    '   OnProgress = GLCadencer1Progress' + #13#10 +
    '   Left = 64' + #13#10 +
    '   Top = 24' + #13#10 +
    ' end' + #13#10 +
    'end';
begin
  Result := Format(FormText, [FFormName]);
end;

function TFormFile.GetAge: TDateTime;
begin
  Result := -1;
end;

// TGLSceneFormWizard
//

function TGLSceneFormWizard.GetIDString: string;
begin
  Result := 'GLScene.GLSceneForm';
end;

function TGLSceneFormWizard.GetName: string;
begin
  Result := 'GLScene Form Wizard';
end;

function TGLSceneFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TGLSceneFormWizard.Execute;
begin
  FClassName := 'GLMainForm';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    'Unit', FUnitIdent, FClassName, FFileName);
  (BorlandIDEServices as IOTAModuleServices).CreateModule(Self);
end;

function TGLSceneFormWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(hInstance, 'GLSCENEFORMGLYPH');
end;

function TGLSceneFormWizard.GetPage: string;
begin
  Result := 'GLScene';
end;

function TGLSceneFormWizard.GetAuthor: string;
begin
  Result := 'YarUnderoaker';
end;

function TGLSceneFormWizard.GetComment: string;
begin
  Result := 'Creates a new GLScene form.'
end;

function TGLSceneFormWizard.GetCreatorType: string;
begin
  Result := sForm;
end;

function TGLSceneFormWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

{$IFDEF GLS_COMPILER_2010_UP}

function TGLSceneFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory('Borland.Delphi.New.Expert');
end;

function TGLSceneFormWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;
{$ENDIF}

{$IFDEF GLS_COMPILER_2010_UP}

function TGLSceneFormWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;
{$ENDIF}

function TGLSceneFormWizard.GetExisting: Boolean;
begin
  Result := False;
end;

function TGLSceneFormWizard.GetFileSystem: string;
begin
  Result := '';
end;

function TGLSceneFormWizard.GetOwner: IOTAModule;
var
  IModuleServices: IOTAModuleServices;
  IModule, INewModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Pred(IModuleServices.ModuleCount) do
  begin
    IModule := IModuleServices.Modules[I];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup.ActiveProject;
      Break;
    end
    else if IModule.QueryInterface(IOTAProject, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
{
  Result := nil;
  IModuleServices := (BorlandIDEServices as IOTAModuleServices);
  IModule := IModuleServices.CurrentModule;

  if IModule <> nil then
  begin
    if IModule.QueryInterface(IOTAProject, INewModule) = S_OK then
      Result := INewModule

    else if IModule.OwnerModuleCount > 0 then
    begin
      INewModule := IModule.OwnerModules[0];
      if INewModule <> nil then
        if INewModule.QueryInterface(IOTAProject, Result) <> S_OK then
          Result := nil;
    end;
  end;   }
end;

function TGLSceneFormWizard.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TGLSceneFormWizard.GetAncestorName: string;
begin
  Result := 'GLSceneForm';
end;

function TGLSceneFormWizard.GetImplFileName: string;
var
  CurrDir: array[0..MAX_PATH] of Char;
begin
  // Note: full path name required!
  GetCurrentDirectory(SizeOf(CurrDir), CurrDir);
  Result := Format('%s\%s.pas', [CurrDir, FUnitIdent]);
end;

function TGLSceneFormWizard.GetIntfFileName: string;
begin
  Result := '';
end;

function TGLSceneFormWizard.GetFormName: string;
begin
  Result := FClassName;
end;

function TGLSceneFormWizard.GetMainForm: Boolean;
begin
  Result := True;
end;

function TGLSceneFormWizard.GetShowForm: Boolean;
begin
  Result := True;
end;

function TGLSceneFormWizard.GetShowSource: Boolean;
begin
  Result := True;
end;

function TGLSceneFormWizard.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TFormFile.Create('', FormIdent, AncestorIdent);
end;

function TGLSceneFormWizard.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TGLSceneFormWizard.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TGLSceneFormWizard.FormCreated(const FormEditor: IOTAFormEditor);
var
  OContainer: IOTAComponent;
  NContainer: INTAComponent;
  Component, Component1, Component2: TComponent;

  procedure RefClean;
  begin
    Component := nil;
    NContainer := nil;
    OContainer := nil;
  end;
begin
  // Form Setup
  RefClean;
  OContainer := FormEditor.GetRootComponent;
  OContainer.QueryInterface(INTAComponent, NContainer);
  Component := NContainer.GetComponent;
  with (Component as TForm) do
  begin
    BorderStyle := bsSizeable;
    Caption := 'GLScene';
    ClientHeight := 512;
    ClientWidth := 512;
    Position := poMainFormCenter;
  end;
  RefClean;
end;

end.

