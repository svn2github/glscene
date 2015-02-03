//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS.RegisterXCollection<p>

	Register TXCollection property editor<p>

	<b>History : </b><font size=-1><ul>
      <li>20/05/10 - Yar - Fixes for Linux x64
      <li>11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>03/07/04 - LR - Removed ..\ from the GLScene.inc
      <li>16/04/00 - Egg - Creation
	</ul></font>
}
unit GLS.RegisterXCollection;

interface

{$i GLScene.inc}

uses
  System.Classes, System.TypInfo,

  //in FMX DesignEditors and DesignIntf not exist

  GLS.XCollection;

type

  TPropertyAttribute = (paValueList, paSubProperties, paDialog, paMultiSelect,
    paAutoUpdate, paSortList, paReadOnly, paRevertable, paFullWidthName,
    paVolatileSubProperties, paVCL, paNotNestable, paDisplayReadOnly,
    paCustomDropDown, paValueEditable);

  TPropertyAttributes = set of TPropertyAttribute;

  TClassProperty = class  //class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; //override;  <- not found in base class
    procedure GetProperties(Proc: TGetChildProc);
      //in VCL -> (Proc: TGetPropProc) //override; <- not found in base class
    function GetValue: string; //override;  <- not found in base class
  end;

	// TGLXCollectionProperty
	//
	TXCollectionProperty = class(TClassProperty)
		public
      function GetAttributes: TPropertyAttributes; //override;  <- not found in base class
			procedure Edit; //override;  <- not found in base class

	end;


function GetOrdValueAt(Index: Integer): Longint;
function GetOrdValue: Longint;
procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  FXCollectionEditor;

function GetOrdValueAt(Index: Integer): Longint;
var
  FPropList: PInstPropList;
begin
  with FPropList^[Index] do Result := GetOrdProp(Instance, PropInfo);
end;

function GetOrdValue: Longint;
begin
  Result := GetOrdValueAt(0);
end;


procedure Register;
begin
  { TODO : E2003 Undeclared identifier: 'RegisterPropertyEditor' }
  (*
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
  *)
end;

//----------------- TXCollectionProperty ------------------------------------

// GetAttributes
//
function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

// Edit
//
procedure TXCollectionProperty.Edit;
begin
   with XCollectionEditor do
   begin
      { TODO : E2003 Undeclared identifier: 'Designer' }
      (*SetXCollection(TXCollection(GetOrdValue), Self.Designer);*)
      Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
{ TClassProperty }

function TClassProperty.GetAttributes: TPropertyAttributes;
begin

end;

procedure TClassProperty.GetProperties(Proc: TGetChildProc);
begin
  inherited;

end;

function TClassProperty.GetValue: string;
begin

end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   
end.
