//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Register TGLXCollection property editor 
 
}
unit GLX.XCollectionRegister;

interface

{$I VKScene.inc}

uses
  System.Classes, System.TypInfo,
  //in FMX DesignEditors and DesignIntf do not exist
  GLX.XCollection;

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
	TGLXCollectionProperty = class(TClassProperty)
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
  RegisterPropertyEditor(TypeInfo(TGLXCollection), nil, '', TGLXCollectionProperty);
  *)
end;

//----------------- TGLXCollectionProperty ------------------------------------

// GetAttributes
//
function TGLXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

// Edit
//
procedure TGLXCollectionProperty.Edit;
begin
   with XCollectionEditor do
   begin
      { TODO : E2003 Undeclared identifier: 'Designer' }
      (*SetXCollection(TGLXCollection(GetOrdValue), Self.Designer);*)
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
