// GLROAMPatch
{: Class for managing a ROAM (square) patch.<p>

	<b>History : </b><font size=-1><ul>
      <li>10/09/01 - EG - Creation
	</ul></font>
}
unit GLROAMPatch;

interface

uses GLHeightData;

type

	// TGLROAMPatch
	//
	TGLROAMPatch = class (TObject)
	   private
	      { Private Declarations }
         FHeightData : THeightData; // Referred, not owned

	   protected
	      { Protected Declarations }
         
	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         property HeightData : THeightData read FHeightData write FHeightData; 
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLROAMPatch ------------------
// ------------------

// Create
//
constructor TGLROAMPatch.Create;
begin
	inherited Create;
//	...
end;

// Destroy
//
destructor TGLROAMPatch.Destroy;
begin
//	...
	inherited Destroy;
end;

end.
