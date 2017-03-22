unit uDebugInfo;

interface

uses
  System.SysUtils,
  GLScene,
  GLHUDObjects,
  GLVectorGeometry;


const

  C_PRECISION = 6;
  C_DIGITS = 3;
type
  TdfDebugRec = record
    sCaption, sParam: String;
    bVisible: Boolean;
  end;

  TdfDebugInfo = class(TGLHUDText)
  private
    FDebugs: array of TdfDebugRec;
    procedure ReconstructText();
  public
    function AddNewString(aCaption: String): Integer;
    procedure ShowString(aIndex: Integer);
    procedure HideString(aIndex: Integer);
    procedure UpdateParam(aIndex: Integer; aParam: Single); overload;
    procedure UpdateParam(aIndex: Integer; aParam: Integer); overload;
    procedure UpdateParam(aIndex: Integer; aParam: String); overload;
    procedure UpdateParam(aIndex: Integer; aParam: TAffineVector); overload;
    procedure UpdateParam(aIndex: Integer; aParam: TVector); overload;

    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    destructor Destroy; override;
  end;

var
  dfDebugInfo: TdfDebugInfo;

implementation

{ TdfDebugInfo }

function TdfDebugInfo.AddNewString(aCaption: String): Integer;
var
  i: Integer;
begin
  i := Length(FDebugs);
  SetLength(FDebugs, i + 1);
  FDebugs[i].sCaption := aCaption;
  FDebugs[i].sParam := '';
  FDebugs[i].bVisible := True;
  Result := i;
end;

procedure TdfDebugInfo.ReconstructText;
var
  i: Integer;
  sText: String;
begin
//  Text :=
  sText := '';
  for i := 0 to Length(FDebugs) - 1 do
    if FDebugs[i].bVisible then
      sText := sText + FDebugs[i].sCaption + ': ' + FDebugs[i].sParam + ';' + #13#10;

  Text := sText;
end;

procedure TdfDebugInfo.ShowString(aIndex: Integer);
begin
  FDebugs[aIndex].bVisible := True;
end;

constructor TdfDebugInfo.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  SetLength(FDebugs, 0);
end;

destructor TdfDebugInfo.Destroy;
begin
  SetLength(FDebugs, 0);
  inherited;
end;

procedure TdfDebugInfo.HideString(aIndex: Integer);
begin
  FDebugs[aIndex].bVisible := False;
end;

procedure TdfDebugInfo.UpdateParam(aIndex, aParam: Integer);
begin
  UpdateParam(aIndex, IntToStr(aParam));
end;

procedure TdfDebugInfo.UpdateParam(aIndex: Integer; aParam: Single);
begin
  UpdateParam(aIndex, FloatToStrF(aParam, ffGeneral, C_PRECISION, C_DIGITS));
end;

procedure TdfDebugInfo.UpdateParam(aIndex: Integer; aParam: TVector);
begin
  UpdateParam(aIndex,
    '['+FloatToStrF(aParam.X, ffGeneral, C_PRECISION, C_DIGITS) + '] [' +
        FloatToStrF(aParam.Y, ffGeneral, C_PRECISION, C_DIGITS) + '] [' +
        FloatToStrF(aParam.Z, ffGeneral, C_PRECISION, C_DIGITS) + ']');
end;

procedure TdfDebugInfo.UpdateParam(aIndex: Integer; aParam: TAffineVector);
begin
  UpdateParam(aIndex,
    '['+FloatToStrF(aParam.X, ffGeneral, C_PRECISION, C_DIGITS) + '] [' +
    '['+FloatToStrF(aParam.Y, ffGeneral, C_PRECISION, C_DIGITS) + '] [' +
    '['+FloatToStrF(aParam.Z, ffGeneral, C_PRECISION, C_DIGITS) + ']');
end;

procedure TdfDebugInfo.UpdateParam(aIndex: Integer; aParam: String);
begin
  if FDebugs[aIndex].sParam <> aParam then
  begin
    FDebugs[aIndex].sParam := aParam;
    ReconstructText();
  end;
end;

end.
