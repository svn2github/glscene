//
// This unit is part of the GLScene Project, http://glscene.org
//
{RegisterXCollection 

	Register TXCollection property editor 

	 History :  
       20/05/10 - Yar - Fixes for Linux x64
       11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       03/07/04 - LR - Removed ..\ from the GLScene.inc
       16/04/00 - Egg - Creation
	 
}
unit RegisterXCollection;

interface

{$i GLScene.inc}

uses
  System.Classes,
  XCollection,

  DesignEditors, DesignIntf;

type

	// TGLXCollectionProperty
	//
	TXCollectionProperty = class(TClassProperty)
		public
			{ Public Declarations }
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

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


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
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
   with XCollectionEditor do begin
     SetXCollection(TXCollection(GetOrdValue), Self.Designer);
     Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations

end.
