//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
   DelphiWebScriptII implementation for the GLScene scripting layer
    
}
unit VKS.ScriptDWS2;

interface

uses
  System.Classes, System.SysUtils, 
  dws2Comp, dws2Exprs, dws2Symbols, 

  VKS.XCollection, VKS.ScriptBase, VKS.Manager;

type
  // TVKDelphiWebScriptII
  //
  { This class only adds manager registration logic to the TDelphiWebScriptII
     class to enable the XCollection items (ie. TVKScriptDWS2) retain it's
     assigned compiler from design to run -time. }
  TVKDelphiWebScriptII = class(TDelphiWebScriptII)
    public
      constructor Create(AOnwer : TComponent); override;
      destructor Destroy; override;
  end;

  // VKS.ScriptDWS2
  //
  { Implements DelphiWebScriptII scripting functionality through the
     abstracted VKS.ScriptBase . }
  TVKScriptDWS2 = class(TVKScriptBase)
    private
      { Private Declarations }
      FDWS2Program : TProgram;
      FCompiler : TVKDelphiWebScriptII;
      FCompilerName : String;

    protected
      { Protected Declarations }
      procedure SetCompiler(const Value : TVKDelphiWebScriptII);

      procedure ReadFromFiler(reader : TReader); override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      function GetState : TVKScriptState; override;

    public
      { Public Declarations }
      destructor Destroy; override;

      procedure Assign(Source: TPersistent); override;

      procedure Compile; override;
      procedure Start; override;
      procedure Stop; override;
      procedure Execute; override;
      procedure Invalidate; override;
      function Call(aName : String;
        aParams : array of Variant) : Variant; override;

      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;

      property DWS2Program : TProgram read FDWS2Program;

    published
      { Published Declarations }
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

// ---------------
// --------------- Miscellaneous ---------------
// ---------------

// Register
//
procedure Register;
begin
  RegisterClasses([TVKDelphiWebScriptII, TVKScriptDWS2]);
  RegisterComponents('GLScene DWS2', [TVKDelphiWebScriptII]);
end;


// ----------
// ---------- TVKDelphiWebScriptII ----------
// ----------

// Create
//
constructor TVKDelphiWebScriptII.Create(AOnwer : TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

// Destroy
//
destructor TVKDelphiWebScriptII.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TVKScriptDWS2 ---------------
// ---------------

// Destroy
//
destructor TVKScriptDWS2.Destroy;
begin
  Invalidate;
  inherited;
end;

// Assign
//
procedure TVKScriptDWS2.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKScriptDWS2 then begin
    Compiler:=TVKScriptDWS2(Source).Compiler;
  end;
end;

// ReadFromFiler
//
procedure TVKScriptDWS2.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FCompilerName:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TVKScriptDWS2.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do begin
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else
      WriteString('');
  end;
end;

// Loaded
//
procedure TVKScriptDWS2.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TVKDelphiWebScriptII, FCompilerName);
    if Assigned(temp) then
      Compiler:=TVKDelphiWebScriptII(temp);
    FCompilerName:='';
  end;
end;

// Notification
//
procedure TVKScriptDWS2.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Compiler) and (Operation = opRemove) then
    Compiler:=nil;
end;

// FriendlyName
//
class function TVKScriptDWS2.FriendlyName : String;
begin
  Result:='VKS.ScriptDWS2';
end;

// FriendlyDescription
//
class function TVKScriptDWS2.FriendlyDescription : String;
begin
  Result:='DelphiWebScriptII script';
end;

// ItemCategory
//
class function TVKScriptDWS2.ItemCategory : String;
begin
  Result:='';
end;

// Compile
//
procedure TVKScriptDWS2.Compile;
begin
  Invalidate;
  if Assigned(Compiler) then
    FDWS2Program:=Compiler.Compile(Text.Text)
  else
    raise Exception.Create('No compiler assigned!');
end;

// Execute
//
procedure TVKScriptDWS2.Execute;
begin
  if (State = ssUncompiled) then
    Compile
  else if (State = ssRunning) then
    Stop;
  if (State = ssCompiled) then
    FDWS2Program.Execute;
end;

// Invalidate
//
procedure TVKScriptDWS2.Invalidate;
begin
  if (State <> ssUncompiled) or Assigned(FDWS2Program) then begin
    Stop;
    FreeAndNil(FDWS2Program);
  end;
end;

// Start
//
procedure TVKScriptDWS2.Start;
begin
  if (State = ssUncompiled) then
    Compile;
  if (State = ssCompiled) then
    FDWS2Program.BeginProgram(False);
end;

// Stop
//
procedure TVKScriptDWS2.Stop;
begin
  if (State = ssRunning) then
    FDWS2Program.EndProgram;
end;

// Call
//
function TVKScriptDWS2.Call(aName: String;
  aParams: array of Variant) : Variant;
var
  Symbol : TSymbol;
  Output : IInfo;
begin
  if (State <> ssRunning) then
    Start;
  if State = ssRunning then begin
    Symbol:=FDWS2Program.Table.FindSymbol(aName);
    if Assigned(Symbol) then begin
      if Symbol is TFuncSymbol then begin
        Output:=FDWS2Program.Info.Func[aName].Call(aParams);
        if Assigned(Output) then
          Result:=Output.Value;
      end else
        raise Exception.Create('Expected TFuncSymbol but found '+Symbol.ClassName+' for '+aName);
    end else
      raise Exception.Create('Symbol not found for '+aName);
  end;
end;

// SetCompiler
//
procedure TVKScriptDWS2.SetCompiler(const Value : TVKDelphiWebScriptII);
begin
  if Value<>FCompiler then begin
    FCompiler:=Value;
    Invalidate;
  end;
end;

// GetState
//
function TVKScriptDWS2.GetState : TVKScriptState;
begin
  Result:=ssUncompiled;
  if Assigned(FDWS2Program) then begin
    case FDWS2Program.ProgramState of
      psReadyToRun : Result:=ssCompiled;
      psRunning : Result:=ssRunning;
    else
      if FDWS2Program.Msgs.HasErrors then begin
        if FDWS2Program.Msgs.HasCompilerErrors then
          Result:=ssCompileErrors
        else if FDWS2Program.Msgs.HasExecutionErrors then
          Result:=ssRunningErrors;
        Errors.Text:=FDWS2Program.Msgs.AsInfo;
      end;
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

  RegisterXCollectionItemClass(TVKScriptDWS2);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TVKScriptDWS2);

end.