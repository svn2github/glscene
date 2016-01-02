//
// This unit is part of the GLScene Project   
//
// VKS.ScriptPython
{: Python implementation for the GLScene scripting layer.<p>

   This unit is experimental.<p>

   <b>History : </b><font size=-1><ul>
      <li>11/11/2004 - SG - Creation
   </ul></font>
}
unit VKS.ScriptPython;

interface

uses
  System.Classes, System.SysUtils,

  VKS.XCollection, VKS.ScriptBase, VKS.Manager,

  // Python
  PythonEngine;

type
  // TVKPythonEngine
  //
  {: This class only adds manager registration logic to the TPythonEngine
     class to enable the XCollection items (ie. TVKScriptPython) retain it's
     assigned compiler from design to run -time. }
  TVKPythonEngine = class(TPythonEngine)
    public
      constructor Create(AOnwer : TComponent); override;
      destructor Destroy; override;
  end;

  // VKS.ScriptPython
  //
  {: Implements Python scripting functionality through the
     abstracted VKS.ScriptBase. }
  TVKScriptPython = class(TVKScriptBase)
    private
      { Private Declarations }
      FEngine : TVKPythonEngine;
      FEngineName : String;
      FCompiled,
      FStarted : Boolean;

    protected
      { Protected Declarations }
      procedure SetEngine(const Value : TVKPythonEngine);

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

    published
      { Published Declarations }
      property Engine : TVKPythonEngine read FEngine write SetEngine;

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
  RegisterClasses([TVKPythonEngine, TVKScriptPython]);
  RegisterComponents('GLScene Python', [TVKPythonEngine]);
end;


// ----------
// ---------- TVKPythonEngine ----------
// ----------

// Create
//
constructor TVKPythonEngine.Create(AOnwer : TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

// Destroy
//
destructor TVKPythonEngine.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TVKScriptPython ---------------
// ---------------

// Destroy
//
destructor TVKScriptPython.Destroy;
begin
  Invalidate;
  inherited;
end;

// Assign
//
procedure TVKScriptPython.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKScriptPython then begin
    Engine:=TVKScriptPython(Source).Engine;
  end;
end;

// ReadFromFiler
//
procedure TVKScriptPython.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FEngineName:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TVKScriptPython.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do begin
    if Assigned(FEngine) then
      WriteString(FEngine.GetNamePath)
    else
      WriteString('');
  end;
end;

// Loaded
//
procedure TVKScriptPython.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FEngineName<>'' then begin
    temp:=FindManager(TVKPythonEngine, FEngineName);
    if Assigned(temp) then
      Engine:=TVKPythonEngine(temp);
    FEngineName:='';
  end;
end;

// Notification
//
procedure TVKScriptPython.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Engine) and (Operation = opRemove) then
    Engine:=nil;
end;

// FriendlyName
//
class function TVKScriptPython.FriendlyName : String;
begin
  Result:='TVKScriptPython';
end;

// FriendlyDescription
//
class function TVKScriptPython.FriendlyDescription : String;
begin
  Result:='Python script';
end;

// ItemCategory
//
class function TVKScriptPython.ItemCategory : String;
begin
  Result:='';
end;

// Compile
//
procedure TVKScriptPython.Compile;
begin
  Invalidate;
  if Assigned(Engine) then begin
    Engine.ExecStrings(Text);
    FCompiled:=True;
    FStarted:=False;
  end else
    raise Exception.Create('No engine assigned!');
end;

// Execute
//
procedure TVKScriptPython.Execute;
begin
  Compile;
end;

// Invalidate
//
procedure TVKScriptPython.Invalidate;
begin
  FStarted:=False;
  FCompiled:=False;
end;

// Start
//
procedure TVKScriptPython.Start;
begin
  Compile;
  FStarted:=True;
end;

// Stop
//
procedure TVKScriptPython.Stop;
begin
  FStarted:=False;
end;

// Call
//
function TVKScriptPython.Call(aName: String;
  aParams: array of Variant) : Variant;
var
  func : PPyObject;
  args : array of TVarRec;
  i : Integer;
begin
  if State = ssUncompiled then
    Start;
  if State = ssRunning then begin
    func:=Engine.FindFunction('__main__', aName);
    if Assigned(func) then
      if Length(aParams)>0 then begin
        SetLength(args, Length(aParams));
        for i:=0 to Length(aParams)-1 do begin
          args[i].VType:=vtVariant;
          args[i].VVariant:=@aParams[i];
        end;
        Result:=Engine.EvalFunction(func, args);
      end else
        Result:=Engine.EvalFunctionNoArgs(func);
  end;
end;

// SetCompiler
//
procedure TVKScriptPython.SetEngine(const Value : TVKPythonEngine);
begin
  if Value<>FEngine then begin
    FEngine:=Value;
    Invalidate;
  end;
end;

// GetState
//
function TVKScriptPython.GetState : TVKScriptState;
begin
  Result:=ssUncompiled;
  if Assigned(Engine) and FCompiled and FStarted then
    Result:=ssRunning;
end;

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
initialization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  RegisterXCollectionItemClass(TVKScriptPython);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TVKScriptPython);

end.