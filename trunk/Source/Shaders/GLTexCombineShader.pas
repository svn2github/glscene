{: GLTexCombineShader<p>

   A shader that allows texture combiner setup.<p>

   <b>History : </b><font size=-1><ul>
      <li>16/05/03 - EG - Creation
   </ul></font>
}
unit GLTexCombineShader;

interface

uses Classes, GLTexture;

type

   // TGLTexCombineShader
   //
   {: A shader that can setup the texture combiner.<p> }
   TGLTexCombineShader = class (TGLShader)
	   private
	      { Protected Declarations }
         FCombiners : TStrings;
         FCombinerIsValid : Boolean; // to avoid reparsing invalid stuff
         FDesignTimeEnabled : Boolean;

	   protected
			{ Protected Declarations }
         procedure SetCombiners(const val : TStrings);
         procedure SetDesignTimeEnabled(const val : Boolean);

         procedure DoInitialize; override;
         procedure DoApply(var rci : TRenderContextInfo); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
         procedure DoFinalize; override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
			procedure NotifyChange(Sender : TObject); override;

      published
	      { Published Declarations }
         property Combiners : TStrings read FCombiners write SetCombiners;
         property DesignTimeEnabled : Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;
   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLTextureCombiners;

procedure Register;
begin
	RegisterComponents('GLScene Shaders', [TGLTexCombineShader]);
end;

// ------------------
// ------------------ TGLTexCombineShader ------------------
// ------------------

// Create
//
constructor TGLTexCombineShader.Create(AOwner : TComponent);
begin
	inherited;
   ShaderStyle:=ssLowLevel;
   FCombiners:=TStringList.Create;
   TStringList(FCombiners).OnChange:=NotifyChange;
   FCombinerIsValid:=True;
end;

// Destroy
//
destructor TGLTexCombineShader.Destroy;
begin
	inherited;
   FCombiners.Free;
end;

// NotifyChange
//
procedure TGLTexCombineShader.NotifyChange(Sender : TObject);
begin
   FCombinerIsValid:=True;
   inherited NotifyChange(Sender);
end;

// DoInitialize
//
procedure TGLTexCombineShader.DoInitialize;
begin
end;

// DoApply
//
procedure TGLTexCombineShader.DoApply(var rci : TRenderContextInfo);
begin
   if FCombinerIsValid and (FDesignTimeEnabled or (not (csDesigning in ComponentState)))  then  begin
      try
         SetupTextureCombiners(FCombiners.Text);
      except
         FCombinerIsValid:=False;
         raise;
      end;
   end;
end;

// DoUnApply
//
function TGLTexCombineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   Result:=False;
end;

// DoFinalize
//
procedure TGLTexCombineShader.DoFinalize;
begin
end;

// SetCombiners
//
procedure TGLTexCombineShader.SetCombiners(const val : TStrings);
begin
   if val<>FCombiners then begin
      FCombiners.Assign(val);
      NotifyChange(Self);
   end;
end;

// SetDesignTimeEnabled
//
procedure TGLTexCombineShader.SetDesignTimeEnabled(const val : Boolean);
begin
   if val<>FDesignTimeEnabled then begin
      FDesignTimeEnabled:=val;
      NotifyChange(Self);
   end;
end;

end.
