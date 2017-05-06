//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//

{
  Base classes and logic for DelphiWebScriptII enabled objects 
    
}
unit VKS.DwsObjects;

interface

uses
  System.Classes, System.SysUtils, 
  DwsComp, DwsExprs, DwsSymbols,
  VKS.Scene, VKS.XCollection, VKS.ScriptDws, VKS.BaseClasses, VKS.Manager;

type
  // TVKDwsActiveBehaviour
  //
  { A DelphiWebScriptII enabled behaviour. This behaviour also calls
    on the OnProgress and OnBeginProgram procedures in the script if
    they are found. Once compiled and executed the program remains
    active until killed, deactivated or the script is invalidated. }
  TVKDwsActiveBehaviour = class (TVKBehaviour)
    private
      FActive : Boolean;
      FScript : TStringList;
      FDwsProgram : TProgram;
      FCompiler : TVKDelphiWebScriptII;
      FCompilerName : String;

      procedure SetActive(const Value : Boolean);
      procedure SetScript(const Value : TStringList);
      procedure SetCompiler(const Value : TVKDelphiWebScriptII);

      procedure CompileProgram;
      procedure BeginProgram;
      procedure EndProgram;
      procedure KillProgram;

    protected
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;

    public
      constructor Create(AOwner : TVKXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      procedure DoProgress(const ProgressTimes : TProgressTimes); override;
      procedure InvalidateScript;

      property DwsProgram : TProgram read FDwsProgram;

    published
      property Active : Boolean read FActive write SetActive;
      property Script : TStringList read FScript write SetScript;
      property Compiler : TVKDelphiWebScriptII read FCompiler write SetCompiler;
  end;

procedure Register;

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
implementation
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

// ----------
// ---------- Miscellaneous ----------
// ----------

procedure Register;
begin
  RegisterClasses([TVKDwsActiveBehaviour]);
end;


// ----------
// ---------- TVKDwsActiveBehaviour ----------
// ----------

// Create
//
constructor TVKDwsActiveBehaviour.Create(AOwner: TVKXCollection);
begin
  inherited;
  FScript:=TStringList.Create;
end;

// Destroy
//
destructor TVKDwsActiveBehaviour.Destroy;
begin
  KillProgram;
  FScript.Free;
  inherited;
end;

// FriendlyName
//
class function TVKDwsActiveBehaviour.FriendlyName: String;
begin
  Result:='Dws Active Script';
end;

// DoProgress
//
procedure TVKDwsActiveBehaviour.DoProgress(const ProgressTimes: TProgressTimes);
var
  Symbol : TSymbol;
begin
  inherited;
  if Assigned(FDwsProgram) then begin
    if FDwsProgram.ProgramState = psRunning then begin
      Symbol:=DwsProgram.Table.FindSymbol('OnProgress');
      if Assigned(Symbol) then
        if Symbol is TFuncSymbol then
          DwsProgram.Info.Func['OnProgress'].Call([ProgressTimes.newTime, ProgressTimes.deltaTime]);
    end;
  end;
end;

// Loaded
//
procedure TVKDwsActiveBehaviour.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TVKDelphiWebScriptII, FCompilerName);
    if Assigned(temp) then
      Compiler:=TVKDelphiWebScriptII(temp);
    FCompilerName:='';
    CompileProgram;
    if Active then BeginProgram;
  end;
end;

// ReadFromFiler
//
procedure TVKDwsActiveBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Active:=ReadBoolean;
    FCompilerName:=ReadString;
    Script.Text:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TVKDwsActiveBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteBoolean(FActive);
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else WriteString('');
    WriteString(Script.Text);
  end;
end;

// CompileProgram
//
procedure TVKDwsActiveBehaviour.CompileProgram;
begin
  if Assigned(Compiler) then begin
    KillProgram;
    FDwsProgram:=Compiler.Compile(Script.Text);
    if Active then
      BeginProgram;
  end;
end;

// BeginProgram
//
procedure TVKDwsActiveBehaviour.BeginProgram;
var
  Symbol : TSymbol;
  ObjectID : Variant;
  Obj : TVKBaseSceneObject;
begin
  if Assigned(DwsProgram) then begin
    if DwsProgram.ProgramState = psReadyToRun then begin
      DwsProgram.BeginProgram;
      if FDwsProgram.ProgramState = psRunning then begin
        Symbol:=DwsProgram.Table.FindSymbol('OnBeginProgram');
        if Assigned(Symbol) then
          if Symbol is TFuncSymbol then begin
            Obj:=OwnerBaseSceneObject;
            if Assigned(Obj) then begin
              ObjectID:=DwsProgram.Info.RegisterExternalObject(Obj, False, False);
              DwsProgram.Info.Func['OnBeginProgram'].Call([ObjectID]);
            end;
          end;
      end;
    end;
  end;
end;

// EndProgram
//
procedure TVKDwsActiveBehaviour.EndProgram;
begin
  if Assigned(DwsProgram) then begin
    if DwsProgram.ProgramState = psRunning then
      DwsProgram.EndProgram;
  end;
end;

// KillProgram
//
procedure TVKDwsActiveBehaviour.KillProgram;
begin
  if Assigned(DwsProgram) then begin
    EndProgram;
    FreeAndNil(FDwsProgram);
  end;
end;

// InvalidateScript
//
procedure TVKDwsActiveBehaviour.InvalidateScript;
begin
  KillProgram;
  CompileProgram;
end;

// SetActive
//
procedure TVKDwsActiveBehaviour.SetActive(const Value: Boolean);
begin
  if Value<>FActive then begin
    EndProgram;
    FActive:=Value;
    if Active then
      BeginProgram;
  end;
end;

// SetScript
//
procedure TVKDwsActiveBehaviour.SetScript(const Value: TStringList);
begin
  if Assigned(Value) then begin
    KillProgram;
    FScript.Assign(Value);
    if Assigned(Compiler) then begin
      CompileProgram;
      if Active then BeginProgram;
    end;
  end;
end;

// SetCompiler
//
procedure TVKDwsActiveBehaviour.SetCompiler(const Value: TVKDelphiWebScriptII);
begin
  if Value<>FCompiler then begin
    if Assigned(FCompiler) then
      KillProgram;
    FCompiler:=Value;
    if Assigned(FCompiler) then begin
      RegisterManager(FCompiler);
      CompileProgram;
      if Active then BeginProgram;
    end;
  end;
end;


// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
initialization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  RegisterXCollectionItemClass(TVKDwsActiveBehaviour);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TVKDwsActiveBehaviour);

end.
