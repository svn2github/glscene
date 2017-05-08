//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   DelphiWebScript symbol creation for base GLScene classes. 

   This unit is dependent on DwsClasses and DwsVectorGeometry.
   These components must be associated with the same compiler
   for the GLScene classes to inherit from. 
}
unit DwsVXScene;

interface

uses
  System.Classes, 
  System.SysUtils,
  DwsExprs, 
  DwsSymbols, 
  DwsComp, 
  DwsCompStrings, 
  DwsStack, 
  DwsFunctions, 
  DwsHelperFunc, 
  VXS.Scene, 
  VXS.VectorGeometry,
  VXS.Coordinates;

type
  TDwsVXSceneUnit = class(TDwsUnitComponent)
    private
    procedure AddClassTVXCoordinates(SymbolTable : TSymbolTable);
      procedure AddClassTVXBaseSceneObject(SymbolTable : TSymbolTable);
    protected
      procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
    public
      constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

//============================================
implementation
//============================================

// ----------
// ---------- Internal class method class declarations ----------
// ----------

type

  
  TVXCoordinatesSetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetXMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetYMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetZMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetWMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetPointMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetToZeroMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesSetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetAsVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesGetAsStringMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesTranslateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesAddScaledVectorMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesRotateMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesNormalizeMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesInvertMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXCoordinatesEqualsMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;


  // TVXBaseSceneObject

  TVXBaseSceneObjectSetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetVisibleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectInvAbsoluteMatrixMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetAbsolutePositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetAbsoluteUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetAbsoluteDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetPositionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetDirectionMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetUpMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetScaleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetPitchAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetTurnAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectSetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectGetRollAngleMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectPitchMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectTurnMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectRollMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectMoveMethod = class(TInternalMethod)
    public
      procedure Execute(var ExternalObject: TObject); override;
  end;

  TVXBaseSceneObjectAddChildMethod = class(TInternalMethod)
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

// TVXCoordinates internal class methods

// TVXCoordinates.X write access
procedure TVXCoordinatesSetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).X:=Info['Value'];
end;

// TVXCoordinates.X read access
procedure TVXCoordinatesGetXMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  Info.Result:=TVXCoordinates(ExternalObject).X;
end;

// TVXCoordinates.Y write access
procedure TVXCoordinatesSetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).Y:=Info['Value'];
end;

// TVXCoordinates.Y read access
procedure TVXCoordinatesGetYMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  Info.Result:=TVXCoordinates(ExternalObject).Y;
end;

// TVXCoordinates.Z write access
procedure TVXCoordinatesSetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).Z:=Info['Value'];
end;

// TVXCoordinates.Z read access
procedure TVXCoordinatesGetZMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  Info.Result:=TVXCoordinates(ExternalObject).Z;
end;

// TVXCoordinates.W write access
procedure TVXCoordinatesSetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).W:=Info['Value'];
end;

// TVXCoordinates.W read access
procedure TVXCoordinatesGetWMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  Info.Result:=TVXCoordinates(ExternalObject).W;
end;

// TVXCoordinates.SetVector
procedure TVXCoordinatesSetVectorMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).SetVector(Info['x'],Info['y'],Info['z'],Info['w']);
end;

// TVXCoordinates.SetPoint
procedure TVXCoordinatesSetPointMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).SetPoint(Info['x'],Info['y'],Info['z']);
end;

// TVXCoordinates.AsVector write access
procedure TVXCoordinatesSetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=GetVectorFromInfo(Info.Vars['Value']);
  TVXCoordinates(ExternalObject).AsVector:=v;
end;

// TVXCoordinates.AsVector read access
procedure TVXCoordinatesGetAsVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=TVXCoordinates(ExternalObject).AsVector;
  SetInfoFromVector(Info.Vars['Result'], v);
end;

// TVXCoordinates.AsString read access
procedure TVXCoordinatesGetAsStringMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  Info.Result:=TVXCoordinates(ExternalObject).AsString;
end;

// TVXCoordinates.Translate
procedure TVXCoordinatesTranslateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TVXCoordinates(ExternalObject).Translate(v);
end;

// TVXCoordinates.AddScaledVector
procedure TVXCoordinatesAddScaledVectorMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=GetVectorFromInfo(Info.Vars['translationVector']);
  TVXCoordinates(ExternalObject).AddScaledVector(Info['factor'],v);
end;

// TVXCoordinates.Rotate
procedure TVXCoordinatesRotateMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=GetVectorFromInfo(Info.Vars['anAxis']);
  TVXCoordinates(ExternalObject).Rotate(v, Info['anAngle']);
end;

// TVXCoordinates.Normalize
procedure TVXCoordinatesNormalizeMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).Normalize;
end;

// TVXCoordinates.Invert
procedure TVXCoordinatesInvertMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).Invert;
end;

// TVXCoordinates.Scale
procedure TVXCoordinatesScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).Scale(Info['factor']);
end;

// TVXCoordinates.Equals
procedure TVXCoordinatesEqualsMethod.Execute(var ExternalObject: TObject);
var
  v : TVector;
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  v:=GetVectorFromInfo(Info.Vars['aVector']);
  Info.Result:=TVXCoordinates(ExternalObject).Equals(v);
end;

// TVXCoordinates.SetToZero
procedure TVXCoordinatesSetToZeroMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXCoordinates);
  TVXCoordinates(ExternalObject).SetToZero;
end;


// TVXBaseSceneObject internal class methods

// TVXBaseSceneObject.SetVisible
procedure TVXBaseSceneObjectSetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Visible:=Info.Vars['Value'].Value;
end;

// TVXBaseSceneObject.GetVisible
procedure TVXBaseSceneObjectGetVisibleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=TVXBaseSceneObject(ExternalObject).Visible;
end;

// TVXBaseSceneObject.SetMatrix
procedure TVXBaseSceneObjectSetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Matrix:=GetMatrixFromInfo(Info.Vars['Value']);
end;

// TVXBaseSceneObject.GetMatrix
procedure TVXBaseSceneObjectGetMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).Matrix);
end;

// TVXBaseSceneObject.AbsoluteMatrix
procedure TVXBaseSceneObjectAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).AbsoluteMatrix);
end;

// TVXBaseSceneObject.InvAbsoluteMatrix
procedure TVXBaseSceneObjectInvAbsoluteMatrixMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromMatrix(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).InvAbsoluteMatrix);
end;

// TVXBaseSceneObject.SetAbsolutePosition
procedure TVXBaseSceneObjectSetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).AbsolutePosition:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVXBaseSceneObject.GetAbsolutePosition
procedure TVXBaseSceneObjectGetAbsolutePositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).AbsolutePosition);
end;

// TVXBaseSceneObject.SetAbsoluteUp
procedure TVXBaseSceneObjectSetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).AbsoluteUp:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVXBaseSceneObject.GetAbsoluteUp
procedure TVXBaseSceneObjectGetAbsoluteUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).AbsoluteUp);
end;

// TVXBaseSceneObject.SetAbsoluteDirection
procedure TVXBaseSceneObjectSetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).AbsoluteDirection:=GetVectorFromInfo(Info.Vars['Value']);
end;

// TVXBaseSceneObject.GetAbsoluteDirection
procedure TVXBaseSceneObjectGetAbsoluteDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  SetInfoFromVector(Info.Vars['Result'], TVXBaseSceneObject(ExternalObject).AbsoluteDirection);
end;

// TVXBaseSceneObject.Position write access
procedure TVXBaseSceneObjectSetPositionMethod.Execute(var ExternalObject: TObject);
var
  Value : TVXCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Value:=TVXCoordinates(Info.GetExternalObjForVar('Value'));
  TVXBaseSceneObject(ExternalObject).Position:=Value;
end;

// TVXBaseSceneObject.Position read access
procedure TVXBaseSceneObjectGetPositionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVXBaseSceneObject(ExternalObject).Position);
end;

// TVXBaseSceneObject.Direction write access
procedure TVXBaseSceneObjectSetDirectionMethod.Execute(var ExternalObject: TObject);
var
  Value : TVXCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Value:=TVXCoordinates(Info.GetExternalObjForVar('Value'));
  TVXBaseSceneObject(ExternalObject).Direction:=Value;
end;

// TVXBaseSceneObject.Direction read access
procedure TVXBaseSceneObjectGetDirectionMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVXBaseSceneObject(ExternalObject).Direction);
end;

// TVXBaseSceneObject.Up write access
procedure TVXBaseSceneObjectSetUpMethod.Execute(var ExternalObject: TObject);
var
  Value : TVXCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Value:=TVXCoordinates(Info.GetExternalObjForVar('Value'));
  TVXBaseSceneObject(ExternalObject).Up:=Value;
end;

// TVXBaseSceneObject.Up read access
procedure TVXBaseSceneObjectGetUpMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVXBaseSceneObject(ExternalObject).Up);
end;

// TVXBaseSceneObject.Scale write access
procedure TVXBaseSceneObjectSetScaleMethod.Execute(var ExternalObject: TObject);
var
  Value : TVXCoordinates;
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Value:=TVXCoordinates(Info.GetExternalObjForVar('Value'));
  TVXBaseSceneObject(ExternalObject).Scale:=Value;
end;

// TVXBaseSceneObject.Scale read access
procedure TVXBaseSceneObjectGetScaleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=Info.RegisterExternalObject(TVXBaseSceneObject(ExternalObject).Scale);
end;

// TVXBaseSceneObject.PitchAngle write access
procedure TVXBaseSceneObjectSetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).PitchAngle:=Info.Vars['Value'].Value;
end;

// TVXBaseSceneObject.PitchAngle read access
procedure TVXBaseSceneObjectGetPitchAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=TVXBaseSceneObject(ExternalObject).PitchAngle;
end;

// TVXBaseSceneObject.TurnAngle write access
procedure TVXBaseSceneObjectSetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).TurnAngle:=Info.Vars['Value'].Value;
end;

// TVXBaseSceneObject.TurnAngle read access
procedure TVXBaseSceneObjectGetTurnAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=TVXBaseSceneObject(ExternalObject).TurnAngle;
end;

// TVXBaseSceneObject.RollAngle write access
procedure TVXBaseSceneObjectSetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).RollAngle:=Info.Vars['Value'].Value;
end;

// TVXBaseSceneObject.RollAngle read access
procedure TVXBaseSceneObjectGetRollAngleMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  Info.Result:=TVXBaseSceneObject(ExternalObject).RollAngle;
end;

// TVXBaseSceneObject.Pitch
procedure TVXBaseSceneObjectPitchMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Pitch(Info['angle']);
end;

// TVXBaseSceneObject.Turn
procedure TVXBaseSceneObjectTurnMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Turn(Info['angle']);
end;

// TVXBaseSceneObject.Roll
procedure TVXBaseSceneObjectRollMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Roll(Info['angle']);
end;

// TVXBaseSceneObject.Move
procedure TVXBaseSceneObjectMoveMethod.Execute(var ExternalObject: TObject);
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  TVXBaseSceneObject(ExternalObject).Move(Info['ADistance']);
end;

// TVXBaseSceneObject.AddChild
procedure TVXBaseSceneObjectAddChildMethod.Execute(var ExternalObject: TObject);
var
  AChild : TObject;
begin
  ValidateExternalObject(ExternalObject, TVXBaseSceneObject);
  AChild:=Info.GetExternalObjForVar('AChild');
  if not Assigned(AChild) then raise Exception.Create('AChild parameter is unassigned.');
  if not (AChild is TVXBaseSceneObject) then Exception.Create('AChild parameter is not inheriting from TVXBaseSceneObject.');
  TVXBaseSceneObject(ExternalObject).AddChild(TVXBaseSceneObject(AChild));
end;


// ----------
// ---------- Global procedures/functions ----------
// ----------

procedure Register;
begin
  RegisterComponents('VXScene Dws', [TDwsVXSceneUnit]);
end;


// ----------
// ---------- TDwsVXSceneUnit ----------
// ----------

constructor TDwsVXSceneUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName:='VXScene';
  with FDependencies do begin
    Add('Classes');
    Add('VXS.VectorGeometry');
  end;
end;

procedure TDwsVXSceneUnit.AddClassTVXCoordinates(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TVXCoordinates', 'TPersistent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetX')) then
    TVXCoordinatesSetXMethod.Create(mkProcedure, [], 0, 'SetX', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetX')) then
    TVXCoordinatesGetXMethod.Create(mkFunction, [], 0, 'GetX', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetY')) then
    TVXCoordinatesSetYMethod.Create(mkProcedure, [], 0, 'SetY', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetY')) then
    TVXCoordinatesGetYMethod.Create(mkFunction, [], 0, 'GetY', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetZ')) then
    TVXCoordinatesSetZMethod.Create(mkProcedure, [], 0, 'SetZ', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetZ')) then
    TVXCoordinatesGetZMethod.Create(mkFunction, [], 0, 'GetZ', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetW')) then
    TVXCoordinatesSetWMethod.Create(mkProcedure, [], 0, 'SetW', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetW')) then
    TVXCoordinatesGetWMethod.Create(mkFunction, [], 0, 'GetW', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetVector')) then
    TVXCoordinatesSetVectorMethod.Create(mkProcedure, [], 0, 'SetVector', ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetPoint')) then
    TVXCoordinatesSetPointMethod.Create(mkProcedure, [], 0, 'SetPoint', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetToZero')) then
    TVXCoordinatesSetToZeroMethod.Create(mkProcedure, [], 0, 'SetToZero', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAsVector')) then
    TVXCoordinatesSetAsVectorMethod.Create(mkProcedure, [], 0, 'SetAsVector', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsVector')) then
    TVXCoordinatesGetAsVectorMethod.Create(mkFunction, [], 0, 'GetAsVector', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAsString')) then
    TVXCoordinatesGetAsStringMethod.Create(mkFunction, [], 0, 'GetAsString', [], 'String', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Translate')) then
    TVXCoordinatesTranslateMethod.Create(mkProcedure, [], 0, 'Translate', ['translationVector', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddScaledVector')) then
    TVXCoordinatesAddScaledVectorMethod.Create(mkProcedure, [], 0, 'AddScaledVector', ['factor', 'Float', 'translationVector', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Rotate')) then
    TVXCoordinatesRotateMethod.Create(mkProcedure, [], 0, 'Rotate', ['anAxis', 'TVector', 'anAngle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Normalize')) then
    TVXCoordinatesNormalizeMethod.Create(mkProcedure, [], 0, 'Normalize', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Invert')) then
    TVXCoordinatesInvertMethod.Create(mkProcedure, [], 0, 'Invert', [], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Scale')) then
    TVXCoordinatesScaleMethod.Create(mkProcedure, [], 0, 'Scale', ['factor', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Equals')) then
    TVXCoordinatesEqualsMethod.Create(mkFunction, [], 0, 'Equals', ['aVector', 'TVector'], 'Boolean', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('X', 'Float', 'GetX', 'SetX', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Y', 'Float', 'GetY', 'SetY', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Z', 'Float', 'GetZ', 'SetZ', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsVector', 'TVector', 'GetAsVector', 'SetAsVector', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AsString', 'String', 'GetAsString', '', '', False, ClassSym, SymbolTable);
end;

// AddClassTVXBaseSceneObject
//
procedure TDwsVXSceneUnit.AddClassTVXBaseSceneObject(
  SymbolTable: TSymbolTable);
var
  ClassSym : TClassSymbol;
begin
  ClassSym:=TClassSymbol(AddClassSymbol(SymbolTable, 'TVXBaseSceneObject', 'TComponent'));

  // Methods
  if not Assigned(ClassSym.Members.FindLocal('SetVisible')) then
    TVXBaseSceneObjectSetVisibleMethod.Create(mkProcedure, [], 0, 'SetVisible', ['Value', 'Boolean'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetVisible')) then
    TVXBaseSceneObjectGetVisibleMethod.Create(mkFunction, [], 0, 'GetVisible', [], 'Boolean', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetMatrix')) then
    TVXBaseSceneObjectSetMatrixMethod.Create(mkProcedure, [], 0, 'SetMatrix', ['Value', 'TMatrix'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetMatrix')) then
    TVXBaseSceneObjectGetMatrixMethod.Create(mkFunction, [], 0, 'GetMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AbsoluteMatrix')) then
    TVXBaseSceneObjectAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'AbsoluteMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('InvAbsoluteMatrix')) then
    TVXBaseSceneObjectInvAbsoluteMatrixMethod.Create(mkFunction, [], 0, 'InvAbsoluteMatrix', [], 'TMatrix', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsolutePosition')) then
    TVXBaseSceneObjectSetAbsolutePositionMethod.Create(mkProcedure, [], 0, 'SetAbsolutePosition', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsolutePosition')) then
    TVXBaseSceneObjectGetAbsolutePositionMethod.Create(mkFunction, [], 0, 'GetAbsolutePosition', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteUp')) then
    TVXBaseSceneObjectSetAbsoluteUpMethod.Create(mkProcedure, [], 0, 'SetAbsoluteUp', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteUp')) then
    TVXBaseSceneObjectGetAbsoluteUpMethod.Create(mkFunction, [], 0, 'GetAbsoluteUp', [], 'TVector', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetAbsoluteDirection')) then
    TVXBaseSceneObjectSetAbsoluteDirectionMethod.Create(mkProcedure, [], 0, 'SetAbsoluteDirection', ['Value', 'TVector'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetAbsoluteDirection')) then
    TVXBaseSceneObjectGetAbsoluteDirectionMethod.Create(mkFunction, [], 0, 'GetAbsoluteDirection', [], 'TVector', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPosition')) then
    TVXBaseSceneObjectSetPositionMethod.Create(mkProcedure, [], 0, 'SetPosition', ['Value', 'TVXCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPosition')) then
    TVXBaseSceneObjectGetPositionMethod.Create(mkFunction, [], 0, 'GetPosition', [], 'TVXCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetDirection')) then
    TVXBaseSceneObjectSetDirectionMethod.Create(mkProcedure, [], 0, 'SetDirection', ['Value', 'TVXCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetDirection')) then
    TVXBaseSceneObjectGetDirectionMethod.Create(mkFunction, [], 0, 'GetDirection', [], 'TVXCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetUp')) then
    TVXBaseSceneObjectSetUpMethod.Create(mkProcedure, [], 0, 'SetUp', ['Value', 'TVXCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetUp')) then
    TVXBaseSceneObjectGetUpMethod.Create(mkFunction, [], 0, 'GetUp', [], 'TVXCoordinates', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetScale')) then
    TVXBaseSceneObjectSetScaleMethod.Create(mkProcedure, [], 0, 'SetScale', ['Value', 'TVXCoordinates'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetScale')) then
    TVXBaseSceneObjectGetScaleMethod.Create(mkFunction, [], 0, 'GetScale', [], 'TVXCoordinates', ClassSym, SymbolTable);

  if not Assigned(ClassSym.Members.FindLocal('SetPitchAngle')) then
    TVXBaseSceneObjectSetPitchAngleMethod.Create(mkProcedure, [], 0, 'SetPitchAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetPitchAngle')) then
    TVXBaseSceneObjectGetPitchAngleMethod.Create(mkFunction, [], 0, 'GetPitchAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetTurnAngle')) then
    TVXBaseSceneObjectSetTurnAngleMethod.Create(mkProcedure, [], 0, 'SetTurnAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetTurnAngle')) then
    TVXBaseSceneObjectGetTurnAngleMethod.Create(mkFunction, [], 0, 'GetTurnAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('SetRollAngle')) then
    TVXBaseSceneObjectSetRollAngleMethod.Create(mkProcedure, [], 0, 'SetRollAngle', ['Value', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('GetRollAngle')) then
    TVXBaseSceneObjectGetRollAngleMethod.Create(mkFunction, [], 0, 'GetRollAngle', [], 'Float', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Pitch')) then
    TVXBaseSceneObjectPitchMethod.Create(mkProcedure, [], 0, 'Pitch', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Turn')) then
    TVXBaseSceneObjectTurnMethod.Create(mkProcedure, [], 0, 'Turn', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Roll')) then
    TVXBaseSceneObjectRollMethod.Create(mkProcedure, [], 0, 'Roll', ['angle', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('Move')) then
    TVXBaseSceneObjectMoveMethod.Create(mkProcedure, [], 0, 'Move', ['ADistance', 'Float'], '', ClassSym, SymbolTable);
  if not Assigned(ClassSym.Members.FindLocal('AddChild')) then
    TVXBaseSceneObjectAddChildMethod.Create(mkProcedure, [], 0, 'AddChild', ['AChild', 'TVXBaseSceneObject'], '', ClassSym, SymbolTable);

  // Properties
  AddPropertyToClass('Visible', 'Boolean', 'GetVisible', 'SetVisible', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Matrix', 'TMatrix', 'GetMatrix', 'SetMatrix', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsolutePosition', 'TVector', 'GetAbsolutePosition', 'SetAbsolutePosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteUp', 'TVector', 'GetAbsoluteUp', 'SetAbsoluteUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('AbsoluteDirection', 'TVector', 'GetAbsoluteDirection', 'SetAbsoluteDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Position', 'TVXBaseSceneObject', 'GetPosition', 'SetPosition', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Direction', 'TVXBaseSceneObject', 'GetDirection', 'SetDirection', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Up', 'TVXBaseSceneObject', 'GetUp', 'SetUp', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('Scale', 'TVXBaseSceneObject', 'GetScale', 'SetScale', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('PitchAngle', 'Float', 'GetPitchAngle', 'SetPitchAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('TurnAngle', 'Float', 'GetTurnAngle', 'SetTurnAngle', '', False, ClassSym, SymbolTable);
  AddPropertyToClass('RollAngle', 'Float', 'GetRollAngle', 'SetRollAngle', '', False, ClassSym, SymbolTable);
end;

// AddUnitSymbols
//
procedure TDwsVXSceneUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // Forward class declaration
  AddForwardDeclaration('TVXCoordinates', SymbolTable);
  AddForwardDeclaration('TVXBaseSceneObject', SymbolTable);

  // Class types
  AddClassTVXCoordinates(SymbolTable);
  AddClassTVXBaseSceneObject(SymbolTable);
end;

end.
