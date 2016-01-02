//
// This unit is part of the GLScene Project   
//
{: VKS.ShaderCombiner<p>

    Allows to combine shaders in different sequences.
    Note, that can't just take 2 random shaders and combine them, because
    shaders often override the objects material and vertex data with a total
    disregard to what existed before it. But in some cases, especially with
    multipass shaders, this unit does magic and allows to reuse and upgrade
    previously written shaders.<p>


	<b>History : </b><font size=-1><ul>
      <li>23/02/07 - DaStr - Initial version (contributed to GLScene)


    Previous version history:
      v1.0  02 November    '2006  Creation
}
unit VKS.ShaderCombiner;

interface

{$I VKScene.inc}

uses
  System.Classes,
  //VKS
  VKS.Material, VKS.Scene, VKS.VectorGeometry, 
  VKS.Strings, VKS.RenderContextInfo;

type
  {: MP - multipass, SP-singlepass, AP - anypass (single or multi)
     One-Two or Two-One determines the order of how the shaders should be applied
     For example, sctTwoMPOneSP means that first one will be applied Shader Two,
     which can be a multipass shader, then Shader One is applied, which should be
     a singlepass shader.

     sctOneMPTwoSP and sctTwoMPOneSP modes are actualy quite Str@nge,
                                       because... well look at the code yourself

     TODO: Add more modes here, including sctOneAPTwoAP, which should be the
           default one.

     By the way, I admit - the names do look stupid, and if someone gives them
     proper names, I will be only glad.
  }
  TVKShaderCombinerType = (sctOneSPTwoAP, sctTwoSPOneAP,
                           sctOneMPTwoSP, sctTwoMPOneSP
                           );

  TVKCustomShaderCombiner = class(TVKShader)
  private
    FCurrentPass: Integer;
    FCombinerType: TVKShaderCombinerType;
    FShaderOne: TVKShader;
    FShaderTwo: TVKShader;
    procedure SetShaderOne(const Value: TVKShader);
    procedure SetShaderTwo(const Value: TVKShader);
  protected
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property CombinerType: TVKShaderCombinerType read FCombinerType write FCombinerType default sctOneSPTwoAP;
    property ShaderOne: TVKShader read FShaderOne write SetShaderOne;
    property ShaderTwo: TVKShader read FShaderTwo write SetShaderTwo;
    property CurrentPass : Integer read FCurrentPass stored False;
  public
    constructor Create(AOwner: TComponent); override;
    function ShaderSupported: Boolean; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TVKShaderCombiner = class(TVKCustomShaderCombiner)
  published
    property CombinerType;
    property ShaderOne;
    property ShaderTwo;
    property ShaderStyle;
  end;

implementation


{ TVKCustomShaderCombiner }

procedure TVKCustomShaderCombiner.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKCustomShaderCombiner then
  begin
    SetShaderOne(TVKCustomShaderCombiner(Source).FShaderOne);
    SetShaderTwo(TVKCustomShaderCombiner(Source).FShaderTwo);
  end;
end;

constructor TVKCustomShaderCombiner.Create(AOwner: TComponent);
begin
  inherited;
  FCombinerType := sctOneSPTwoAP;
end;

procedure TVKCustomShaderCombiner.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  if (csDesigning in ComponentState) then Exit;
  Assert((FShaderOne <> nil) and (FShaderTwo <> nil));

  FCurrentPass:=1;
  case FCombinerType of
    sctOneMPTwoSP:
      begin
        FShaderOne.Apply(rci, Self);
        FShaderTwo.Apply(rci, Self);
      end;

    sctTwoMPOneSP:
      begin
        FShaderTwo.Apply(rci, Self);
        FShaderOne.Apply(rci, Self);
      end;

    sctOneSPTwoAP:
      begin
        FShaderOne.Apply(rci, Self);
      end;

    sctTwoSPOneAP:
      begin
        FShaderTwo.Apply(rci, Self);
      end;
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

function TVKCustomShaderCombiner.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  case FCombinerType of
    sctOneMPTwoSP:
      begin
        if FShaderOne.UnApply(rci) then
          Result := True
        else
          Result := FShaderTwo.UnApply(rci);
      end;

    sctTwoMPOneSP:
      begin
        if FShaderTwo.UnApply(rci) then
          Result := True
        else
          Result := FShaderOne.UnApply(rci);
      end;

    sctOneSPTwoAP:
      begin
        if FCurrentPass = 1 then
        begin
          FShaderOne.UnApply(rci);
          FShaderTwo.Apply(rci, Self);
          Result := True;
        end
        else
          Result := FShaderTwo.UnApply(rci);
      end;

    sctTwoSPOneAP:
      begin
        if FCurrentPass = 1 then
        begin
          FShaderTwo.UnApply(rci);
          FShaderOne.Apply(rci, Self);
          Result := True;
        end
        else
          Result := FShaderOne.UnApply(rci);
      end;
  else
    begin
      Result := False;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
  Inc(FCurrentPass);
end;

procedure TVKCustomShaderCombiner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FShaderOne then
      FShaderOne := nil
    else if AComponent = FShaderTwo then
      FShaderTwo := nil;
  end;
end;

procedure TVKCustomShaderCombiner.SetShaderOne(const Value: TVKShader);
begin
  if FShaderOne <> nil then
    FShaderOne.RemoveFreeNotification(Self);
  FShaderOne := Value;
  if FShaderOne <> nil then
    FShaderOne.FreeNotification(Self);
end;

procedure TVKCustomShaderCombiner.SetShaderTwo(const Value: TVKShader);
begin
  if FShaderTwo <> nil then
    FShaderTwo.RemoveFreeNotification(Self);
  FShaderTwo := Value;
  if FShaderTwo <> nil then
    FShaderTwo.FreeNotification(Self);
end;

function TVKCustomShaderCombiner.ShaderSupported: Boolean;
begin
  Result := (FShaderOne <> nil) and (FShaderTwo <> nil) and
             FShaderOne.ShaderSupported and FShaderTwo.ShaderSupported;
end;

initialization
  RegisterClasses([TVKCustomShaderCombiner, TVKShaderCombiner]);

end.

