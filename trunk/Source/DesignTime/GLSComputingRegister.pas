//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSComputingRegister<p>

   Registration unit for GLScene Computing package.<p>

	<b>History : </b><font size=-1><ul>
      <li>09/06/10 - Yar - Added dropdown list ProjectModule for TGLSCUDACompiler
      <li>19/03/10 - Yar - Creation
	</ul></font>
}
unit GLSComputingRegister;

interface

uses
  Classes, SysUtils,
  DesignIntf, DesignEditors, STREDIT, ToolsAPI;

procedure Register;

type

  TGLSCUDAEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLSCUDACompilerEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLSCUDACompilerSourceProperty = class(TStringProperty)
  private
    FModuleList: TStringList;
    procedure RefreshModuleList;
  public
    { Public Declarations }
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;

  TCUDAModuleCodeEditor = class(TStringListProperty)
  public
    { Public Declarations }
    procedure Edit; override;
  end;

implementation

uses
  GLSCUDAEditor,
  GLSCUDAContext, GLSCUDA, GLSCUDACompiler, GLSCUDAFFTPlan, GLSCUDAGraphics;

procedure Register;
begin
  RegisterComponents('GLScene Computing', [TGLSCUDA, TGLSCUDADevice,
    TGLSCUDACompiler, TGLSCUDAFactory]);
  RegisterComponentEditor(TGLSCUDA, TGLSCUDAEditor);
  RegisterComponentEditor(TGLSCUDACompiler, TGLSCUDACompilerEditor);
	RegisterPropertyEditor(TypeInfo(TStringList), TCUDAModule, 'Code',
    TCUDAModuleCodeEditor);
  RegisterPropertyEditor(TypeInfo(string), TGLSCUDACompiler, 'ProjectModule',
    TGLSCUDACompilerSourceProperty);
  RegisterNoIcon([TCUDAModule, TCUDAMemData, TCUDAFunction, TCUDATexture,
    TCUDAFFTPlan, TCUDAGLImageResource, TCUDAGLGeometryResource]);
end;

// ------------------
// ------------------ TGLSCUDAEditor ------------------
// ------------------

procedure TGLSCUDAEditor.Edit;
begin
  with GLSCUDAEditorForm do
  begin
    SetCUDAEditorClient(TGLSCUDA(Self.Component), Self.Designer);
    Show;
  end;
end;

procedure TGLSCUDAEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSCUDAEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show CUDA Items Editor';
  end;
end;

function TGLSCUDAEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TGLSCUDACompilerEditor ------------------
// ------------------

procedure TGLSCUDACompilerEditor.Edit;
var
  CUDACompiler: TGLSCUDACompiler;
  i: Integer;
  Task: PDesignerTask;
  obj: TCUDABaseItem;
begin
  CUDACompiler := TGLSCUDACompiler(Self.Component);
  CUDACompiler.Compile;
  // Ugly hack for design time dynamicaly creating components
  if CUDACompiler.DesignerTaskList <> nil then
  begin
    for i := 0 to CUDACompiler.DesignerTaskList.Count - 1 do
    begin
      Task := CUDACompiler.DesignerTaskList.Items[i];
      if Task.Creating then
      begin
        obj := TCUDABaseItem(Designer.CreateComponent(Task.ItemClass,
          Task.Owner, 0,
          0, 0, 0));
        obj.Master := Task.Owner;
        if Task.ItemClass = TCUDAFunction then
          TCUDAFunction(obj).KernelName := Task.KernelName
        else if Task.ItemClass = TCUDATexture then
          TCUDATexture(obj).KernelName := Task.KernelName;
      end
      else
      begin
        Designer.SelectComponent(Task.Owner);
        Designer.DeleteSelection(true);
      end;
      Dispose(Task);
    end;
    CUDACompiler.DesignerTaskList.Free;
    CUDACompiler.DesignerTaskList := nil;
    Designer.Modified;
  end;
end;

procedure TGLSCUDACompilerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSCUDACompilerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Compile Module';
  end;
end;

function TGLSCUDACompilerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TGLSCUDACompilerSourceProperty ------------------
// ------------------

constructor TGLSCUDACompilerSourceProperty.Create(
  const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited;
  FModuleList := TStringList.Create;
end;

destructor TGLSCUDACompilerSourceProperty.Destroy;
begin
  FModuleList.Free;
  inherited;
end;

procedure TGLSCUDACompilerSourceProperty.RefreshModuleList;
var
  proj: IOTAProject;
  I: Integer;
  module: IOTAModuleInfo;
  name: string;
begin
  FModuleList.Clear;
  FModuleList.Add('none');
  proj := GetActiveProject;
  if proj <> nil then
  begin
    for I := 0 to proj.GetModuleCount - 1 do
    begin
      module := proj.GetModule(I);
      name := UpperCase(ExtractFileExt(module.FileName));
      if name = '.CU' then
        FModuleList.Add(module.FileName);
    end;
  end;
end;

function TGLSCUDACompilerSourceProperty.GetAttributes;
begin
  Result := [paValueList];
end;

// GetValue
//
function TGLSCUDACompilerSourceProperty.GetValue : String;
var
  I : Integer;
begin
  RefreshModuleList;
  Result := FModuleList[0];
  for I := 1 to FModuleList.Count - 1 do
  begin
    if GetStrValue = ExtractFileName(FModuleList[I]) then
    begin
      Result := GetStrValue;
      exit;
    end;
  end;
  Result := FModuleList[1];
end;

// GetValues
//
procedure TGLSCUDACompilerSourceProperty.GetValues(Proc: TGetStrProc);
var
   I : Integer;
begin
  RefreshModuleList;
  for I := 0 to FModuleList.Count - 1 do
      Proc(ExtractFileName(FModuleList[I]));
end;

// SetValue
//
procedure TGLSCUDACompilerSourceProperty.SetValue(const Value: String);
var
  I, J: Integer;
  Correct: Boolean;
begin
  RefreshModuleList;
  Correct := False;
  for I := 1 to FModuleList.Count - 1 do
    if Value = ExtractFileName(FModuleList[I]) then
    begin
      J := I;
      Correct := True;
      Break;
    end;

  if Correct then
  begin
    TGLSCUDACompiler(GetComponent(0)).CodeSourceFile := FModuleList[J];
    SetStrValue(Value);
  end
  else
  begin
    SetStrValue('none');
  end;
	Modified;
end;

// ------------------
// ------------------ TCUDAModuleCodeEditor ------------------
// ------------------

procedure TCUDAModuleCodeEditor.Edit;
var
  i: Integer;
  module: TCUDAModule;
  TaskList: TList;
  Task: PDesignerTask;
  obj: TCUDABaseItem;
begin
  inherited Edit;
  module := TCUDAModule(GetComponent(0));
  TaskList := module.GetDesignerTaskList;
  if Assigned(TaskList) then
  begin
    for i := 0 to TaskList.Count - 1 do
    begin
      Task := TaskList.Items[i];
      if Task.Creating then
      begin
        obj := TCUDABaseItem(Designer.CreateComponent(Task.ItemClass,
          Task.Owner, 0,
          0, 0, 0));
        obj.Master := Task.Owner;
        if Task.ItemClass = TCUDAFunction then
          TCUDAFunction(obj).KernelName := Task.KernelName
        else if Task.ItemClass = TCUDATexture then
          TCUDATexture(obj).KernelName := Task.KernelName;
      end
      else
      begin
        Designer.SelectComponent(Task.Owner);
        Designer.DeleteSelection(true);
      end;
      Dispose(Task);
    end;
    TaskList.Free;
    Designer.Modified;
  end;
end;

end.

