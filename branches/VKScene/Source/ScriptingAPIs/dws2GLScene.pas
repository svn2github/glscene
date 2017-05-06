//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   DelphiWebScriptII symbol creation for base GLScene classes. 

   This unit is dependent on dws2Classes and dws2VectorGeometry.
   These components must be associated with the same compiler
   for the GLScene classes to inherit from. 
}
unit dws2GLScene;

interface

uses
  System.Classes, System.SysUtils,
  dws2Exprs, dws2Symbols, dws2Comp, dws2CompStrings, dws2Stack, 
  dws2Functions, dws2HelperFunc, VKS.Scene, VKS.VectorGeometry,
  VKS.Coordinates;

type
  Tdws2GLSceneUnit = class(Tdws2UnitComponent)
    private

      procedure AddClassTVKCoordinates(SymbolTable : TSymbolTable);
      procedure AddClassTVKBaseSceneObject(SymbolTable : TSymbolTable);

    protected
      procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;

    public
      constructor Create(AOwner: TComponent); override;

  end;

procedure Register;

implementation

// ----------
// ---------- Internal class method class declarations ----------
// ----------

type

  // TVKCoordinates

  TVKCoordinatesSetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetPointMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetToZeroMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesSetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesGetAsStringMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesTranslateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesAddScaledVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesRotateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesNormalizeMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesInvertMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKCoordinatesEqualsMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;


  // TVKBaseSceneObject

  TVKBaseSceneObjectSetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectInvAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectSetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectGetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectPitchMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectTurnMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectRollMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectMoveMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVKBaseSceneObjectAddChildMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;


// ----------
// ---------- Vector/Matrix to/from IInfo helper functions ----------
// ----------

// GetVectorFromInfo
//
function GetVectorFromInfo(Info : IInfo) : TVector;
begin
  Result:=VectorMake(Info.Element([0]).Value,
                     Info.Element([1]).Value,
                     Info.Element([2]).Value,
                     Info.Element([3]).Value);
end;

// SetInfoFromVector
//
procedure SetInfoFromVector(Info : IInfo; vec : TVector);
var
  i : Integer;
begin
  for i:=0 to 3 do
    Info.Element([i]).Value:=vec[i];
end;

// GetMatrixFromInfo
//
function GetMatrixFromInfo(Info : IInfo) : TMatrix;
var
  i : Integer;
begin
  for i:=0 to 3 do
    Result[i]:=VectorMake(Info.Element([i]).Element([0]).Value,
                          Info.Element([i]).Element([1]).Value,
                          Info.Element([i]).Element([2]).Value,
                          Info.Element([i]).Element([3]).Value);
end;

// SetInfoFromMatrix
//
procedure SetInfoFromMatrix(Info : IInfo; mat : TMatrix);
var
  i,j : Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      Info.Element([i]).Element([j]).Value:=mat[i][j];
end;


// ----------
// ---------- Internal class method execute procedures ----------
// ----------

// TVKCoordinates internal class methods

// TVKCoordinates.X write access
procedure TVKCoordinatesSetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).X:=Info['Value'];
end;

// TVKCoordinates.X read access
procedure TVKCoordinatesGetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  Info.Result:=TVKCoordinates(ExternalObject).X;
end;

// TVKCoordinates.Y write access
procedure TVKCoordinatesSetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).Y:=Info['Value'];
end;

// TVKCoordinates.Y read access
procedure TVKCoordinatesGetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  Info.Result:=TVKCoordinates(ExternalObject).Y;
end;

// TVKCoordinates.Z write access
procedure TVKCoordinatesSetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).Z:=Info['Value'];
end;

// TVKCoordinates.Z read access
procedure TVKCoordinatesGetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  Info.Result:=TVKCoordinates(ExternalObject).Z;
end;

// TVKCoordinates.W write access
procedure TVKCoordinatesSetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).W:=Info['Value'];
end;

// TVKCoordinates.W read access
procedure TVKCoordinatesGetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  Info.Result:=TVKCoordinates(ExternalObject).W;
end;

// TVKCoordinates.SetVector
procedure TVKCoordinatesSetVectorMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).SetVector(Info['x'],Info['y'],Info['z'],Info['w']);
end;

// TVKCoordinates.SetPoint
procedure TVKCoordinatesSetPointMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).SetPoint(Info['x'],Info['y'],Info['z']);
end;

// TVKCoordinates.AsVector write access
procedure TVKCoordinatesSetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=GetVectorFromInfo(Info.Vars['Value']);
  TVKCoordinates(ExternalObject).AsVector:=v;
end;

// TVKCoordinates.AsVector read access
procedure TVKCoordinatesGetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=TVKCoordinates(ExternalObject).AsVector;
  SetInfoFromVector(Info.Vars['Result'], v);
end;

// TVKCoordinates.AsString read access
procedure TVKCoordinatesGetAsStringMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  Info.Result:=TVKCoordinates(ExternalObject).AsString;
end;

// TVKCoordinates.Translate
procedure TVKCoordinatesTranslateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TVKCoordinates(ExternalObject).Translate(v);
end;

// TVKCoordinates.AddScaledVector
procedure TVKCoordinatesAddScaledVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TVKCoordinates(ExternalObject).AddScaledVector(Info['factor'],v);
end;

// TVKCoordinates.Rotate
procedure TVKCoordinatesRotateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=GetVectorFromInfo(Info.Vars['anAxis']);
  TVKCoordinates(ExternalObject).Rotate(v, Info['anAngle']);
end;

// TVKCoordinates.Normalize
procedure TVKCoordinatesNormalizeMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).Normalize;
end;

// TVKCoordinates.Invert
procedure TVKCoordinatesInvertMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).Invert;
end;

// TVKCoordinates.Scale
procedure TVKCoordinatesScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).Scale(Info['factor']);
end;

// TVKCoordinates.Equals
procedure TVKCoordinatesEqualsMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  v:=GetVectorFromInfo(Info.Vars['aVector']);
  Info.Result:=TVKCoordinates(ExternalObject).Equals(v);
end;

// TVKCoordinates.SetToZero
procedure TVKCoordinatesSetToZeroMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKCoordinates);
  TVKCoordinates(ExternalObject).SetToZero;
end;


// TVKBaseSceneObject internal class methods

// TVKBaseSceneObject.SetVisible
procedure TVKBaseSceneObjectSetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Visible:=Info.Vars['Value'].Value;
end;

// TVKBaseSceneObject.GetVisible
procedure TVKBaseSceneObjectGetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=TVKBaseSceneObject(ExternalObject).Visible;
end;

// TVKBaseSceneObject.SetMatrix
procedure TVKBaseSceneObjectSetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Matrix:=GetMatrixFromInfo(Info.Vars['Value']);
end;

// TVKBaseSceneObject.GetMatrix
procedure TVKBaseSceneObjectGetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).Matrix);
end;

// TVKBaseSceneObject.AbsoluteMatrix
procedure TVKBaseSceneObjectAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).AbsoluteMatrix);
end;

// TVKBaseSceneObject.InvAbsoluteMatrix
procedure TVKBaseSceneObjectInvAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).InvAbsoluteMatrix);
end;

// TVKBaseSceneObject.SetAbsolutePosition
procedure TVKBaseSceneObjectSetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).AbsolutePosition:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVKBaseSceneObject.GetAbsolutePosition
procedure TVKBaseSceneObjectGetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).AbsolutePosition);
end;

// TVKBaseSceneObject.SetAbsoluteUp
procedure TVKBaseSceneObjectSetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).AbsoluteUp:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVKBaseSceneObject.GetAbsoluteUp
procedure TVKBaseSceneObjectGetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).AbsoluteUp);
end;

// TVKBaseSceneObject.SetAbsoluteDirection
procedure TVKBaseSceneObjectSetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).AbsoluteDirection:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVKBaseSceneObject.GetAbsoluteDirection
procedure TVKBaseSceneObjectGetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVKBaseSceneObject(ExternalObject).AbsoluteDirection);
end;

// TVKBaseSceneObject.Position write access
procedure TVKBaseSceneObjectSetPositionMethod.Execute(var ExternalObject: TObject);
var
  Value : TVKCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Value:=TVKCoordinates(Info.GetExternalObjForVar('Value'));
  TVKBaseSceneObject(ExternalObject).Position:=Value;
end;

// TVKBaseSceneObject.Position read access
procedure TVKBaseSceneObjectGetPositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVKBaseSceneObject(ExternalObject).Position);
end;

// TVKBaseSceneObject.Direction write access
procedure TVKBaseSceneObjectSetDirectionMethod.Execute(var ExternalObject: TObject);
var
  Value : TVKCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Value:=TVKCoordinates(Info.GetExternalObjForVar('Value'));
  TVKBaseSceneObject(ExternalObject).Direction:=Value;
end;

// TVKBaseSceneObject.Direction read access
procedure TVKBaseSceneObjectGetDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVKBaseSceneObject(ExternalObject).Direction);
end;

// TVKBaseSceneObject.Up write access
procedure TVKBaseSceneObjectSetUpMethod.Execute(var ExternalObject: TObject);
var
  Value : TVKCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Value:=TVKCoordinates(Info.GetExternalObjForVar('Value'));
  TVKBaseSceneObject(ExternalObject).Up:=Value;
end;

// TVKBaseSceneObject.Up read access
procedure TVKBaseSceneObjectGetUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVKBaseSceneObject(ExternalObject).Up);
end;

// TVKBaseSceneObject.Scale write access
procedure TVKBaseSceneObjectSetScaleMethod.Execute(var ExternalObject: TObject);
var
  Value : TVKCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Value:=TVKCoordinates(Info.GetExternalObjForVar('Value'));
  TVKBaseSceneObject(ExternalObject).Scale:=Value;
end;

// TVKBaseSceneObject.Scale read access
procedure TVKBaseSceneObjectGetScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVKBaseSceneObject(ExternalObject).Scale);
end;

// TVKBaseSceneObject.PitchAngle write access
procedure TVKBaseSceneObjectSetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).PitchAngle:=Info.Vars['Value'].Value;
end;

// TVKBaseSceneObject.PitchAngle read access
procedure TVKBaseSceneObjectGetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=TVKBaseSceneObject(ExternalObject).PitchAngle;
end;

// TVKBaseSceneObject.TurnAngle write access
procedure TVKBaseSceneObjectSetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).TurnAngle:=Info.Vars['Value'].Value;
end;

// TVKBaseSceneObject.TurnAngle read access
procedure TVKBaseSceneObjectGetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=TVKBaseSceneObject(ExternalObject).TurnAngle;
end;

// TVKBaseSceneObject.RollAngle write access
procedure TVKBaseSceneObjectSetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).RollAngle:=Info.Vars['Value'].Value;
end;

// TVKBaseSceneObject.RollAngle read access
procedure TVKBaseSceneObjectGetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  Info.Result:=TVKBaseSceneObject(ExternalObject).RollAngle;
end;

// TVKBaseSceneObject.Pitch
procedure TVKBaseSceneObjectPitchMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Pitch(Info['angle']);
end;

// TVKBaseSceneObject.Turn
procedure TVKBaseSceneObjectTurnMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Turn(Info['angle']);
end;

// TVKBaseSceneObject.Roll
procedure TVKBaseSceneObjectRollMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Roll(Info['angle']);
end;

// TVKBaseSceneObject.Move
procedure TVKBaseSceneObjectMoveMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  TVKBaseSceneObject(ExternalObject).Move(Info['ADistance']);
end;

// TVKBaseSceneObject.AddChild
procedure TVKBaseSceneObjectAddChildMethod.Execute(var ExternalObject: TObject);
var
  AChild : TObject;
begin
  ValidateExternalObject(ExternalObject, TVKBaseSceneObject);
  AChild:=Info.GetExternalObjForVar('AChild');
  if not Assigned(AChild) then raise Exception.Create('AChild parameter is unassigned.');
  if not (AChild is TVKBaseSceneObject) then Exception.Create('AChild parameter is not inheriting from TVKBaseSceneObject.');
  TVKBaseSceneObject(ExternalObject).AddChild(TVKBaseSceneObject(AChild));
end;


// ----------
// ---------- Global procedures/functions ----------
// ----------

procedure Register;
begin
  RegisterComponents('VKScene DWS', [TDwsVKSceneUnit]);
end;


// ----------
// ---------- Tdws2GLSceneUnit ----------
// ----------

// Create
//
constructor Tdws2GLSceneUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName:='VKScene';
  with FDependencies do begin
    Add('Classes');
    Add('VKS.VectorGeometry');
  end;
end;

// AddClassTVKCoordinates
//
procedure Tdws2GLSceneUnit.AddClassTVKCoordinates(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TVKCoordinates', 'TPersistent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetX')) then
    TVKCoordinatesSetXMethod.Create(mkProcedure, [], 0, 'SetX', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetX')) then
    TVKCoordinatesGetXMethod.Create(mkFunction, [], 0, 'GetX', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetY')) then
    TVKCoordinatesSetYMethod.Create(mkProcedure, [], 0, 'SetY', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetY')) then
    TVKCoordinatesGetYMethod.Create(mkFunction, [], 0, 'GetY', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetZ')) then
    TVKCoordinatesSetZMethod.Create(mkProcedure, [], 0, 'SetZ', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetZ')) then
    TVKCoordinatesGetZMethod.Create(mkFunction, [], 0, 'GetZ', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetW')) then
    TVKCoordinatesSetWMethod.Create(mkProcedure, [], 0, 'SetW', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetW')) then
    TVKCoordinatesGetWMethod.Create(mkFunction, [], 0, 'GetW', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetVector')) then
    TVKCoordinatesSetVectorMethod.Create(mkProcedure, [], 0, 'SetVector', ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetPoint')) then
    TVKCoordinatesSetPointMethod.Create(mkProcedure, [], 0, 'SetPoint', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetToZero')) then
    TVKCoordinatesSetToZeroMethod.Create(mkProcedure, [], 0, 'SetToZero', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAsVector')) then
    TVKCoordinatesSetAsVectorMethod.Create(mkProcedure, [], 0, 'SetAsVector', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsVector')) then
    TVKCoordinatesGetAsVectorMethod.Create(mkFunction, [], 0, 'GetAsVector', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsString')) then
    TVKCoordinatesGetAsStringMethod.Create(mkFunction, [], 0, 'GetAsString', [], 'String', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Translate')) then
    TVKCoordinatesTranslateMethod.Create(mkProcedure, [], 0, 'Translate', ['translationVector', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddScaledVector')) then
    TVKCoordinatesAddScaledVectorMethod.Create(mkProcedure, [], 0, 'AddScaledVector', ['factor', 'Float', 'translationVector', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Rotate')) then
    TVKCoordinatesRotateMethod.Create(mkProcedure, [], 0, 'Rotate', ['anAxis', 'TVector', 'anAngle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Normalize')) then
    TVKCoordinatesNormalizeMethod.Create(mkProcedure, [], 0, 'Normalize', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Invert')) then
    TVKCoordinatesInvertMethod.Create(mkProcedure, [], 0, 'Invert', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Scale')) then
    TVKCoordinatesScaleMethod.Create(mkProcedure, [], 0, 'Scale', ['factor', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Equals')) then
    TVKCoordinatesEqualsMethod.Create(mkFunction, [], 0, 'Equals', ['aVector', 'TVector'], 'Boolean', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('X', 'Float', 'GetX', 'SetX', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Y', 'Float', 'GetY', 'SetY', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Z', 'Float', 'GetZ', 'SetZ', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsVector', 'TVector', 'GetAsVector', 'SetAsVector', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsString', 'String', 'GetAsString', '', '', False, ClassSym, SymbolTable);
end;

// AddClassTVKBaseSceneObject
//
procedure Tdws2GLSceneUnit.AddClassTVKBaseSceneObject(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TVKBaseSceneObject', 'TComponent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetVisible')) then
    TVKBaseSceneObjectSetVisibleMethod.Create(mkProcedure, [], 0, 'SetVisible', ['Value', 'Boolean'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetVisible')) then
    TVKBaseSceneObjectGetVisibleMethod.Create(mkFunction, [], 0, 'GetVisible', [], 'Boolean', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetMatrix')) then
    TVKBaseSceneObjectSetMatrixMethod.Create(mkProcedure, [], 0, 'SetMatrix', ['Value', 'TMatrix'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetMatrix')) then
    TVKBaseSceneObjectGetMatrixMethod.Create(mkFunction, [], 0, 'GetMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AbsoluteMatrix')) then
    TVKBaseSceneObjectAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'AbsoluteMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('InvAbsoluteMatrix')) then
    TVKBaseSceneObjectInvAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'InvAbsoluteMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsolutePosition')) then
    TVKBaseSceneObjectSetAbsolutePositionMethod.Create(mkProcedure, [], 0, 'SetAbsolutePosition', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsolutePosition')) then
    TVKBaseSceneObjectGetAbsolutePositionMethod.Create(mkFunction, [], 0, 'GetAbsolutePosition', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteUp')) then
    TVKBaseSceneObjectSetAbsoluteUpMethod.Create(mkProcedure, [], 0, 'SetAbsoluteUp', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteUp')) then
    TVKBaseSceneObjectGetAbsoluteUpMethod.Create(mkFunction, [], 0, 'GetAbsoluteUp', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteDirection')) then
    TVKBaseSceneObjectSetAbsoluteDirectionMethod.Create(mkProcedure, [], 0, 'SetAbsoluteDirection', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteDirection')) then
    TVKBaseSceneObjectGetAbsoluteDirectionMethod.Create(mkFunction, [], 0, 'GetAbsoluteDirection', [], 'TVector', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPosition')) then
    TVKBaseSceneObjectSetPositionMethod.Create(mkProcedure, [], 0, 'SetPosition', ['Value', 'TVKCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPosition')) then
    TVKBaseSceneObjectGetPositionMethod.Create(mkFunction, [], 0, 'GetPosition', [], 'TVKCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetDirection')) then
    TVKBaseSceneObjectSetDirectionMethod.Create(mkProcedure, [], 0, 'SetDirection', ['Value', 'TVKCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetDirection')) then
    TVKBaseSceneObjectGetDirectionMethod.Create(mkFunction, [], 0, 'GetDirection', [], 'TVKCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetUp')) then
    TVKBaseSceneObjectSetUpMethod.Create(mkProcedure, [], 0, 'SetUp', ['Value', 'TVKCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetUp')) then
    TVKBaseSceneObjectGetUpMethod.Create(mkFunction, [], 0, 'GetUp', [], 'TVKCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetScale')) then
    TVKBaseSceneObjectSetScaleMethod.Create(mkProcedure, [], 0, 'SetScale', ['Value', 'TVKCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetScale')) then
    TVKBaseSceneObjectGetScaleMethod.Create(mkFunction, [], 0, 'GetScale', [], 'TVKCoordinates', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPitchAngle')) then
    TVKBaseSceneObjectSetPitchAngleMethod.Create(mkProcedure, [], 0, 'SetPitchAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPitchAngle')) then
    TVKBaseSceneObjectGetPitchAngleMethod.Create(mkFunction, [], 0, 'GetPitchAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetTurnAngle')) then
    TVKBaseSceneObjectSetTurnAngleMethod.Create(mkProcedure, [], 0, 'SetTurnAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetTurnAngle')) then
    TVKBaseSceneObjectGetTurnAngleMethod.Create(mkFunction, [], 0, 'GetTurnAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetRollAngle')) then
    TVKBaseSceneObjectSetRollAngleMethod.Create(mkProcedure, [], 0, 'SetRollAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetRollAngle')) then
    TVKBaseSceneObjectGetRollAngleMethod.Create(mkFunction, [], 0, 'GetRollAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Pitch')) then
    TVKBaseSceneObjectPitchMethod.Create(mkProcedure, [], 0, 'Pitch', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Turn')) then
    TVKBaseSceneObjectTurnMethod.Create(mkProcedure, [], 0, 'Turn', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Roll')) then
    TVKBaseSceneObjectRollMethod.Create(mkProcedure, [], 0, 'Roll', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Move')) then
    TVKBaseSceneObjectMoveMethod.Create(mkProcedure, [], 0, 'Move', ['ADistance', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddChild')) then
    TVKBaseSceneObjectAddChildMethod.Create(mkProcedure, [], 0, 'AddChild', ['AChild', 'TVKBaseSceneObject'], '', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('Visible', 'Boolean', 'GetVisible', 'SetVisible', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Matrix', 'TMatrix', 'GetMatrix', 'SetMatrix', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsolutePosition', 'TVector', 'GetAbsolutePosition', 'SetAbsolutePosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteUp', 'TVector', 'GetAbsoluteUp', 'SetAbsoluteUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteDirection', 'TVector', 'GetAbsoluteDirection', 'SetAbsoluteDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Position', 'TVKBaseSceneObject', 'GetPosition', 'SetPosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Direction', 'TVKBaseSceneObject', 'GetDirection', 'SetDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Up', 'TVKBaseSceneObject', 'GetUp', 'SetUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Scale', 'TVKBaseSceneObject', 'GetScale', 'SetScale', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('PitchAngle', 'Float', 'GetPitchAngle', 'SetPitchAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('TurnAngle', 'Float', 'GetTurnAngle', 'SetTurnAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('RollAngle', 'Float', 'GetRollAngle', 'SetRollAngle', '', False, ClassSym, SymbolTable);
end;

// AddUnitSymbols
//
procedure Tdws2GLSceneUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // Forward class declaration
  AddForwardDeclaration('TVKCoordinates', SymbolTable);
  AddForwardDeclaration('TVKBaseSceneObject', SymbolTable);

  // Class types
  AddClassTVKCoordinates(SymbolTable);
  AddClassTVKBaseSceneObject(SymbolTable);
end;

end.
