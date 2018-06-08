//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   DelphiWebScript implementation
    
}
unit VXS.ScriptDWS;

interface

uses
  System.Classes, 
  System.SysUtils, 
  DwsComp, 
  DwsExprs, 
  DwsSymbols, 

  VXS.XCollection, 
  VXS.ScriptBase, 
  VXS.Manager;

type
  { This class only adds manager registration logic to the TDelphiWebScriptII
     class to enable the XCollection items (ie. TVXScriptDWS) retain it's
     assigned compiler from design to run -time. }
  TVXDelphiWebScriptII = class(TDelphiWebScriptII)
    public
      constructor Create(AOnwer : TComponent); override;
      destructor Destroy; override;
  end;

  { Implements DelphiWebScriptII scripting functionality through the
     abstracted VXS.ScriptBase . }
  TVXScriptDWS = class(TVXScriptBase)
    private
      
      FDWS2Program : TProgram;
      FCompiler : TVXDelphiWebScriptII;
      FCompilerName : String;
    protected
      procedure SetCompiler(const Value : TVXDelphiWebScriptII);
      procedure ReadFromFiler(reader : TReader); override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      function GetState : TVXScriptState; override;
    public
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
      property Compiler : TVXDelphiWebScriptII read FCompiler write SetCompiler;

  end;

procedure Register;

// --------------------------------------------------
implementation
// --------------------------------------------------

// ---------------
// --------------- Miscellaneous ---------------
// ---------------

procedure Register;
begin
  RegisterClasses([TVXDelphiWebScriptII, TVXScriptDWS]);
  RegisterComponents('VXScene DWS2', [TVXDelphiWebScriptII]);
end;


// ----------
// ---------- TVXDelphiWebScriptII ----------
// ----------

constructor TVXDelphiWebScriptII.Create(AOnwer : TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

destructor TVXDelphiWebScriptII.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TVXScriptDWS ---------------
// ---------------

destructor TVXScriptDWS.Destroy;
begin
  Invalidate;
  inherited;
end;

procedure TVXScriptDWS.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVXScriptDWS then begin
    Compiler:=TVXScriptDWS(Source).Compiler;
  end;
end;

procedure TVXScriptDWS.ReadFromFiler(reader : TReader);
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

procedure TVXScriptDWS.WriteToFiler(writer : TWriter);
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

procedure TVXScriptDWS.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TVXDelphiWebScript, FCompilerName);
    if Assigned(temp) then
      Compiler:=TVXDelphiWebScript(temp);
    FCompilerName:='';
  end;
end;

procedure TVXScriptDWS.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Compiler) and (Operation = opRemove) then
    Compiler:=nil;
end;

class function TVXScriptDWS.FriendlyName : String;
begin
  Result:='VXS.ScriptDWS';
end;

class function TVXScriptDWS.FriendlyDescription : String;
begin
  Result:='DelphiWebScript script';
end;

class function TVXScriptDWS.ItemCategory : String;
begin
  Result:='';
end;

procedure TVXScriptDWS.Compile;
begin
  Invalidate;
  if Assigned(Compiler) then
    FDWS2Program:=Compiler.Compile(Text.Text)
  else
    raise Exception.Create('No compiler assigned!');
end;

procedure TVXScriptDWS.Execute;
begin
  if (State = ssUncompiled) then
    Compile
  else if (State = ssRunning) then
    Stop;
  if (State = ssCompiled) then
    FDWS2Program.Execute;
end;

procedure TVXScriptDWS.Invalidate;
begin
  if (State <> ssUncompiled) or Assigned(FDWSProgram) then begin
    Stop;
    FreeAndNil(FDWSProgram);
  end;
end;

procedure TVXScriptDWS.Start;
begin
  if (State = ssUncompiled) then
    Compile;
  if (State = ssCompiled) then
    FDWS2Program.BeginProgram(False);
end;

procedure TVXScriptDWS.Stop;
begin
  if (State = ssRunning) then
    FDWS2Program.EndProgram;
end;

function TVXScriptDWS.Call(aName: String; aParams: array of Variant) : Variant;
var
  Symbol : TSymbol;
  Output : IInfo;
begin
  if (State <> ssRunning) then
    Start;
  if State = ssRunning then begin
    Symbol:=FDWSProgram.Table.FindSymbol(aName);
    if Assigned(Symbol) then begin
      if Symbol is TFuncSymbol then begin
        Output:=FDWSProgram.Info.Func[aName].Call(aParams);
        if Assigned(Output) then
          Result:=Output.Value;
      end else
        raise Exception.Create('Expected TFuncSymbol but found '+Symbol.ClassName+' for '+aName);
    end else
      raise Exception.Create('Symbol not found for '+aName);
  end;
end;

procedure TVXScriptDWS.SetCompiler(const Value : TVXDelphiWebScript);
begin
  if Value<>FCompiler then begin
    FCompiler:=Value;
    Invalidate;
  end;
end;

function TVXScriptDWS.GetState : TVXScriptState;
begin
  Result:=ssUncompiled;
  if Assigned(FDWSProgram) then begin
    case FDWSProgram.ProgramState of
      psReadyToRun : Result:=ssCompiled;
      psRunning : Result:=ssRunning;
    else
      if FDWSProgram.Msgs.HasErrors then begin
        if FDWSProgram.Msgs.HasCompilerErrors then
          Result:=ssCompileErrors
        else if FDWS2Program.Msgs.HasExecutionErrors then
          Result:=ssRunningErrors;
        Errors.Text:=FDWS2Program.Msgs.AsInfo;
      end;
    end;
  end;
end;

// --------------------------------------------------
initialization
// --------------------------------------------------

  RegisterXCollectionItemClass(TVXScriptDWS);

// --------------------------------------------------
finalization
// --------------------------------------------------

  UnregisterXCollectionItemClass(TVXScriptDWS);

end.