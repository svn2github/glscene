//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSComputingRegister<p>

   Registration unit for GLScene Computing package.<p>

	<b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
	</ul></font>
}
unit GLSComputingRegister;

interface

uses
  Classes, DesignIntf, DesignEditors, STREDIT, Dialogs;

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
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TCUDAModuleCodeEditor = class(TStringListProperty)
  public
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
  RegisterPropertyEditor(TypeInfo(string), TGLSCUDACompiler, 'CodeSourceFile',
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

function TGLSCUDACompilerSourceProperty.GetAttributes;
begin
  Result := [paDialog];
end;

procedure TGLSCUDACompilerSourceProperty.Edit;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    if Dialog.Execute then
      SetStrValue(Dialog.FileName);
  finally
    Dialog.Free;
  end;
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

