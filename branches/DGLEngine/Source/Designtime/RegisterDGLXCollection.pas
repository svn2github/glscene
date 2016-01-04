//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ @HTML ( RegisterXCollection<p>

	Register TDGLXCollection property editor<p>

	<b>History : </b><font size=-1><ul>
      <li>16/04/00 - JD - Imported from GLScene
	</ul></font>
}
unit RegisterDGLXCollection;

interface

{$i DGLEngine.inc}

uses
  System.Classes,
  DGLXCollection,

  DesignEditors, DesignIntf;

type

	// TGLXCollectionProperty
	//
	TDGLXCollectionProperty = class(TClassProperty)
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
  FDGLXCollectionEditor;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TDGLXCollection), nil, '', TDGLXCollectionProperty);
end;

//----------------- TXCollectionProperty ------------------------------------

// GetAttributes
//
function TDGLXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TDGLXCollectionProperty.Edit;
begin
   with DGLXCollectionEditor do begin
     SetXCollection(TDGLXCollection(GetOrdValue), Self.Designer);
     Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization


	// class registrations

end.
