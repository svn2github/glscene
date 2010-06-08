//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDACompiler <p>

   Component allows to compile the CUDA-source (*.cu) file.<p>
   in design- and runtime.<p>
   To work requires the presence of CUDA Toolkit 3.0 and MS Visual Studio C++.<p>

   <b>History : </b><font size=-1><ul>
      <li>08/06/10 - Yar - Added ProjectModule property
      <li>19/03/10 - Yar - Creation
   </ul></font><p>
}
unit GLSCUDACompiler;

interface

{$I cuda.inc}

uses
  Classes, Forms;

type
  TGLSCUDACompilerOutput = (codeUndefined, codePtx, codeCubin, codeGpu);
  TGLSCUDACompilerArch = (compute_10, compute_11, compute_12, compute_13,
    sm_10, sm_11, sm_12, sm_13);

  TGLSCUDACompiler = class(TComponent)
  private
    { Private declarations }
    FNVCCPath: string;
    FCppCompilerPath: string;
    FCode: TStringList;
    FProjectModule: string;
    FCodeSourceFile: string;
    FOutputCodeType: TGLSCUDACompilerOutput;
    FArchitecture: TGLSCUDACompilerArch;
    FMaxRegisterCount: Integer;
    FDesignerTaskList: TList;
    procedure setMaxRegisterCount(Value: Integer);
    procedure setOutputCodeType(const Value: TGLSCUDACompilerOutput);
    function StoreProjectModule: Boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function Compile: Boolean;
    property Code: TStringList read fCode write fCode;

    property DesignerTaskList: TList read fDesignerTaskList write
      fDesignerTaskList;
    property CodeSourceFile: string read FCodeSourceFile write FCodeSourceFile;
  published
    { Published declarations }
    {: NVidia CUDA Compiler}
    property NVCCPath: string read fNVCCPath write fNVCCPath;
    {: Microsoft Visual Studio Compiler.
       Pascal compiler is still not done. }
    property CppCompilerPath: string read fCppCompilerPath write
      fCppCompilerPath;
    {: Disign-time only property.
       Make choose of one of the Project module as CUDA kernel source }
    property ProjectModule: string read FProjectModule write FProjectModule stored StoreProjectModule;
    {: Output code type for module kernel
       - Ptx - Parallel Thread Execution
       - Cubin - CUDA Binary}
    property OutputCodeType: TGLSCUDACompilerOutput read fOutputCodeType write
      setOutputCodeType default codePtx;
    property Architecture: TGLSCUDACompilerArch read fArchitecture write
      fArchitecture default sm_10;
    property MaxRegisterCount: Integer read fMaxRegisterCount write
      setMaxRegisterCount default 32;
  end;

implementation

uses
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  SysUtils, Dialogs,
  ShellAPI, TlHelp32;

function IsProcessRunning(const processName: string): Boolean;
var
  hSnapshoot: THandle;
  pe32: TProcessEntry32;
  nvccHandle: THandle;
begin
  Result := false;
  hSnapshoot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshoot = THandle(-1)  then
    exit;

  pe32.dwSize := SizeOf(TProcessEntry32);
  if (Process32First(hSnapshoot, pe32)) then
  begin
    repeat
      if (pe32.szExeFile = processName) then
      begin
        nvccHandle := pe32.th32ProcessID;
        if nvccHandle <> 0 then
        begin
          Result := true;
          break;
        end;
      end;
    until not Process32Next(hSnapshoot, pe32);
  end;
  CloseHandle(hSnapshoot);
end;

// ------------------
// ------------------ TGLSCUDACompiler ------------------
// ------------------

constructor TGLSCUDACompiler.Create(AOwner: TComponent);
var
  path: string;
begin
  inherited Create(AOwner);
  fOutputCodeType := codePtx;
  fArchitecture := sm_10;
  fMaxRegisterCount := 32;
  fNVCCPath := '';
  path := GetEnvironmentVariable('CUDA_BIN_PATH');
  if Length(path) > 0 then
  begin
    path := IncludeTrailingPathDelimiter(path);
    if FileExists(path + 'nvcc.exe') then
      fNVCCPath := path;
  end;
  path := 'C:\Program Files\Microsoft Visual Studio 9.0\VC\bin\';
  if FileExists(path + 'cl.exe') then
    fCppCompilerPath := path
  else
  begin
    path := 'C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\';
    if FileExists(path + 'cl.exe') then
      fCppCompilerPath := path
    else
      fCppCompilerPath := '';
  end;
  FProjectModule := 'none';
end;

procedure TGLSCUDACompiler.Assign(Source: TPersistent);
var
  compiler: TGLSCUDACompiler;
begin
  if Source is TGLSCUDACompiler then
  begin
    compiler := TGLSCUDACompiler(Source);
    FCodeSourceFile := compiler.FCodeSourceFile;
    fOutputCodeType := compiler.fOutputCodeType;
    fArchitecture := compiler.fArchitecture;
  end;
  inherited Assign(Source);
end;

function TGLSCUDACompiler.Compile: Boolean;
var
  tepmPath, tempFile, tempFileExt: string;
  commands, nvcc, pathfile: string;
  CallResult: Cardinal;
  CodeSource: TStrings;
begin
  Result := false;
  if not FileExists(FCodeSourceFile) then
  begin
    if csDesigning in ComponentState then
      MessageDlg('Source file not found', mtError, [mbOk], 0);
    exit;
  end;
  CodeSource := TStringList.Create;
  CodeSource.LoadFromFile(FCodeSourceFile);

  if FileExists(fNVCCPath + 'nvcc.exe')
    and FileExists(fCppCompilerPath + 'cl.exe')
    and Assigned(fCode) then
  begin
    tepmPath := GetEnvironmentVariable('TEMP');
    tepmPath := IncludeTrailingPathDelimiter(tepmPath);
    tempFile := tepmPath + 'temp';
    CodeSource.SaveToFile(tempFile + '.cu');
    commands := '"' + tempFile + '.cu" ';
    commands := commands + '-arch ';
    case fArchitecture of
      compute_10: commands := commands + 'compute_10 ';
      compute_11: commands := commands + 'compute_11 ';
      compute_12: commands := commands + 'compute_12 ';
      compute_13: commands := commands + 'compute_13 ';
      sm_10: commands := commands + 'sm_10 ';
      sm_11: commands := commands + 'sm_11 ';
      sm_12: commands := commands + 'sm_12 ';
      sm_13: commands := commands + 'sm_13 ';
    end;
    commands := commands + '-ccbin ';
    pathfile := Copy(fCppCompilerPath, 1, Length(fCppCompilerPath) - 1);
    commands := commands + '"' + pathfile + '" ';
    commands := commands + '-Xcompiler "/EHsc /W3 /nologo /O2 /Zi /MT " ';
    commands := commands + '-maxrregcount=' + IntToStr(fMaxRegisterCount) + ' ';
    commands := commands + '-m32 ';
    case fOutputCodeType of
      codePtx:
        begin
          commands := commands + '--ptx ';
          tempFileExt := 'ptx';
        end;
      codeCubin:
        begin
          commands := commands + '--cubin ';
          tempFileExt := 'cubin';
        end;
      codeGpu:
        begin
          commands := commands + '--gpu ';
          tempFileExt := 'gpu';
        end;
    end;
    commands := commands + '-o "' + tempFile + '.' + tempFileExt + '" ';
    commands := commands + #00;
    nvcc := fNVCCPath + 'nvcc.exe' + #00;
    CallResult := ShellExecute(0, nil, PChar(nvcc), PChar(commands), nil,
      SW_HIDE);
    if CallResult > 32 then
    begin
      repeat
        Sleep(100);
      until not IsProcessRunning('nvcc.exe');

      pathfile := tempFile + '.' + tempFileExt;
      if FileExists(pathfile) then
      begin
        fCode.LoadFromFile(pathfile);
        if csDesigning in ComponentState then
          fCode.OnChange(Self);
        SysUtils.DeleteFile(pathfile);
        Result := true;
        if csDesigning in ComponentState then
          MessageDlg('Successful compilation', mtInformation, [mbOk], 0);
      end
      else
      begin
        if csDesigning in ComponentState then
          MessageDlg('NVCC failed to compile', mtError, [mbOk], 0);
      end;
    end;
    pathfile := tempFile + '.cu';
    SysUtils.DeleteFile(pathfile);
  end;
  CodeSource.Free;
end;

procedure TGLSCUDACompiler.SetMaxRegisterCount(Value: Integer);
begin
  if Value <> fMaxRegisterCount then
  begin
    Value := 4 * (Value div 4);
    if Value < 4 then
      Value := 4;
    if Value > 128 then
      Value := 128;
    fMaxRegisterCount := Value;
  end;
end;

procedure TGLSCUDACompiler.SetOutputCodeType(const Value:
  TGLSCUDACompilerOutput);
begin
  if Value = codeUndefined then
    exit;
  fOutputCodeType := Value;
end;

function TGLSCUDACompiler.StoreProjectModule: Boolean;
begin
  Result := FProjectModule <> 'none';
end;

end.

