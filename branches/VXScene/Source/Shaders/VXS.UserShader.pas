//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   A shader that passes control of the DoApply and DoUnApply
   methods through published events. This component is 
   designed to make it a little easier to implement a 
   customized shader. Be sure to keep the shader balanced
   by returning the OpenGL state to how you found it. 
               
}
unit VXS.UserShader;

interface

uses
  System.Classes, 
  
  VXS.Material, VXS.RenderContextInfo;

type
  TOnDoApplyEvent = procedure (Sender : TObject; var rci : TVXRenderContextInfo) of Object;
  TOnDoUnApplyEvent = procedure (Sender : TObject; Pass:Integer; var rci : TVXRenderContextInfo; var Continue : Boolean) of Object;
  
  TVXUserShader = class(TVXShader)
    private
      FPass : Integer;
      FOnDoApply : TOnDoApplyEvent;
      FOnDoUnApply : TOnDoUnApplyEvent;
    protected
      procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TVXRenderContextInfo) : Boolean; override;
    published
      property OnDoApply : TOnDoApplyEvent read FOnDoApply write FOnDoApply;
      property OnDoUnApply : TOnDoUnApplyEvent read FOnDoUnApply write FOnDoUnApply;
      property ShaderStyle;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVXUserShader ------------------
// ------------------

// DoApply
//
procedure TVXUserShader.DoApply(var rci: TVXRenderContextInfo; Sender : TObject);
begin
  FPass:=1;
  if Assigned(FOnDoApply) and (not (csDesigning in ComponentState)) then
    FOnDoApply(Self,rci);
end;

// DoUnApply
//
function TVXUserShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  Result:=False;
  if Assigned(FOnDoUnApply) and (not (csDesigning in ComponentState)) then begin
    FOnDoUnApply(Self,FPass,rci,Result);
    Inc(FPass);
  end;
end;

end.
