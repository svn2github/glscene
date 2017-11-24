//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Register TGLXCollection property editor 
  
   History :  
       16/04/00 - Egg - Creation
	   The whole history is logged in previous version of the unit
	 
}
unit GLXCollectionRegister;

interface

{$I GLScene.inc}

uses
  System.Classes,
  GLXCollection,

  DesignEditors, DesignIntf;

type
	TGLXCollectionProperty = class(TClassProperty)
		public
			
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
uses
  FXCollectionEditor;


//----------------- TGLXCollectionProperty ------------------------------------

function TGLXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

procedure TGLXCollectionProperty.Edit;
begin
   with GLXCollectionEditorForm do begin
     SetXCollection(TGLXCollection(GetOrdValue), Self.Designer);
     Show;
   end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGLXCollection), nil, '', TGLXCollectionProperty);
end;


// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations

end.
