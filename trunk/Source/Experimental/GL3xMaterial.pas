unit GL3xMaterial;

interface

{$I GLScene.inc}

uses
  Classes, BaseClasses, OpenGL1x, GLContext, GLRenderContextInfo,
  GLState, GL3xShadersManager, GLVBOManagers, GLSLShader;

type

  // TGL3xMaterial
  //
  TGL3xMaterial = class(TPersistent)
  private
    { Private Declarations }
    FOwner: TComponent;
    FShader: TGLSLShader;
    procedure SetShader(Value: TGLSLShader);
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function GetAttributes(out AttrArray: TGLSLAttributeArray): Boolean;
    procedure Apply(var rci: TRenderContextInfo);
    function UnApply(var rci: TRenderContextInfo): Boolean;
  published
    { Published Declarations }
    property Shader: TGLSLShader read FShader write SetShader;
  end;

implementation

uses
  GLStrings;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xMaterial'}{$ENDIF}
// ------------------
// ------------------ TGL3xMaterial ------------------
// ------------------

constructor TGL3xMaterial.Create(AOwner: TComponent);
begin
  inherited Create;
  FShader := nil;
  FOwner :=  AOwner;
end;

destructor TGL3xMaterial.Destroy;
begin
  Shader := nil;
  inherited;
end;

procedure TGL3xMaterial.SetShader(Value: TGLSLShader);
begin
  if FShader <> nil then
    FShader.RemoveFreeNotification(FOwner);
  FShader := Value;
  if FShader <> nil then
    FShader.FreeNotification(FOwner);
end;

function TGL3xMaterial.GetAttributes(out AttrArray: TGLSLAttributeArray): Boolean;
var
  i, j: Integer;
  Attribs: TGLActiveAttribArray;
  AttribPresent: Boolean;
begin
  Result := false;
  if FShader = nil then
    exit;

  Attribs := FShader.GetActiveAttribs;
  AttribPresent := Length(Attribs)>0;
  j := 0;
  for i := 0 to GLS_VERTEX_ATTR_NUM-1 do
    if (i<=High(Attribs)) and AttribPresent then
    begin
      if GetGLSLAttribute(Attribs[i].Name, AttrArray[j]) then
      begin
        case Attribs[i].AType of
          GL_FLOAT: AttrArray[i].DataType := GLSLType1F;
          GL_FLOAT_VEC2: AttrArray[i].DataType := GLSLType2F;
          GL_FLOAT_VEC3: AttrArray[i].DataType := GLSLType3F;
          GL_FLOAT_VEC4: AttrArray[i].DataType := GLSLType4F;
          GL_INT: AttrArray[i].DataType := GLSLType1I;
          GL_INT_VEC2: AttrArray[i].DataType := GLSLType2I;
          GL_INT_VEC3: AttrArray[i].DataType := GLSLType3I;
          GL_INT_VEC4: AttrArray[i].DataType := GLSLType4I;
          GL_BOOL: AttrArray[i].DataType := GLSLType1I;
          GL_BOOL_VEC2: AttrArray[i].DataType := GLSLType2I;
          GL_BOOL_VEC3: AttrArray[i].DataType := GLSLType3I;
          GL_BOOL_VEC4: AttrArray[i].DataType := GLSLType4I;
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
        Inc(j);
      end
      else AttrArray[i].DataType := GLSLTypeUndefined;
    end
    else AttrArray[i].DataType := GLSLTypeUndefined;

  if j>0 then
    Result := true;
end;

procedure TGL3xMaterial.Apply(var rci: TRenderContextInfo);
begin
  if Assigned(FShader) then
  begin
    FShader.Apply(rci, Self);
  end;
end;

function TGL3xMaterial.UnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(FShader) then
  begin
    Result := FShader.UnApply(rci);
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

end.
