//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//

{
  Base classes and logic for DelphiWebScript enabled objects 
    
}
unit VXS.DwsObjects;

interface

uses
  System.Classes, 
  System.SysUtils, 
  DwsComp, 
  DwsExprs, 
  DwsSymbols,
  VXS.Scene, 
  VXS.XCollection, 
  VXS.ScriptDws, 
  VXS.BaseClasses, 
  VXS.Manager;

type
  { A DelphiWebScript enabled behaviour. This behaviour also calls
    on the OnProgress and OnBeginProgram procedures in the script if
    they are found. Once compiled and executed the program remains
    active until killed, deactivated or the script is invalidated. }
  TVXDwsActiveBehaviour = class (TVXBehaviour)
    private
      FActive : Boolean;
      FScript : TStringList;
      FDwsProgram : TProgram;
      FCompiler : TVXDelphiWebScriptII;
      FCompilerName : String;
      procedure SetActive(const Value : Boolean);
      procedure SetScript(const Value : TStringList);
      procedure SetCompiler(const Value : TVXDelphiWebScript);
      procedure CompileProgram;
      procedure BeginProgram;
      procedure EndProgram;
      procedure KillProgram;
    protected
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      procedure DoProgress(const ProgressTimes : TProgressTimes); override;
      procedure InvalidateScript;
      property DwsProgram : TProgram read FDwsProgram;
    published
      property Active : Boolean read FActive write SetActive;
      property Script : TStringList read FScript write SetScript;
      property Compiler : TVXDelphiWebScriptII read FCompiler write SetCompiler;
  end;

procedure Register;

// --------------------------------------------------
implementation
// --------------------------------------------------

// ----------
// ---------- Miscellaneous ----------
// ----------

procedure Register;
begin
  RegisterClasses([TVXDwsActiveBehaviour]);
end;


// ----------
// ---------- TVXDwsActiveBehaviour ----------
// ----------

constructor TVXDwsActiveBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FScript:=TStringList.Create;
end;

destructor TVXDwsActiveBehaviour.Destroy;
begin
  KillProgram;
  FScript.Free;
  inherited;
end;

class function TVXDwsActiveBehaviour.FriendlyName: String;
begin
  Result:='DWS Active Script';
end;

procedure TVXDwsActiveBehaviour.DoProgress(const ProgressTimes: TProgressTimes);
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

procedure TVXDwsActiveBehaviour.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TVXDelphiWebScript, FCompilerName);
    if Assigned(temp) then
      Compiler:=TVXDelphiWebScript(temp);
    FCompilerName:='';
    CompileProgram;
    if Active then BeginProgram;
  end;
end;

procedure TVXDwsActiveBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Active:=ReadBoolean;
    FCompilerName:=ReadString;
    Script.Text:=ReadString;
  end;
end;

procedure TVXDwsActiveBehaviour.WriteToFiler(writer: TWriter);
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

procedure TVXDwsActiveBehaviour.CompileProgram;
begin
  if Assigned(Compiler) then begin
    KillProgram;
    FDwsProgram:=Compiler.Compile(Script.Text);
    if Active then
      BeginProgram;
  end;
end;

procedure TVXDwsActiveBehaviour.BeginProgram;
var
  Symbol : TSymbol;
  ObjectID : Variant;
  Obj : TVXBaseSceneObject;
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

procedure TVXDwsActiveBehaviour.EndProgram;
begin
  if Assigned(DwsProgram) then begin
    if DwsProgram.ProgramState = psRunning then
      DwsProgram.EndProgram;
  end;
end;

procedure TVXDwsActiveBehaviour.KillProgram;
begin
  if Assigned(DwsProgram) then begin
    EndProgram;
    FreeAndNil(FDwsProgram);
  end;
end;

procedure TVXDwsActiveBehaviour.InvalidateScript;
begin
  KillProgram;
  CompileProgram;
end;

procedure TVXDwsActiveBehaviour.SetActive(const Value: Boolean);
begin
  if Value<>FActive then begin
    EndProgram;
    FActive:=Value;
    if Active then
      BeginProgram;
  end;
end;

procedure TVXDwsActiveBehaviour.SetScript(const Value: TStringList);
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

procedure TVXDwsActiveBehaviour.SetCompiler(const Value: TVXDelphiWebScriptII);
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
initialization
// --------------------------------------------------

  RegisterXCollectionItemClass(TVXDwsActiveBehaviour);

// --------------------------------------------------
finalization
// --------------------------------------------------

  UnregisterXCollectionItemClass(TVXDwsActiveBehaviour);

end.
