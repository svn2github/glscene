//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    Allows to combine shaders in different sequences.
    Note, that can't just take 2 random shaders and combine them, because
    shaders often override the objects material and vertex data with a total
    disregard to what existed before it. But in some cases, especially with
    multipass shaders, this unit does magic and allows to reuse and upgrade
    previously written shaders. 
            
}
unit VXS.ShaderCombiner;

interface

{$I VXScene.inc}

uses
  System.Classes,
  
  VXS.Material, VXS.Scene, VXS.VectorGeometry, 
  VXS.Strings, VXS.RenderContextInfo;

type
  { MP - multipass, SP-singlepass, AP - anypass (single or multi)
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
  TVXShaderCombinerType = (sctOneSPTwoAP, sctTwoSPOneAP,
                           sctOneMPTwoSP, sctTwoMPOneSP
                           );

  TVXCustomShaderCombiner = class(TVXShader)
  private
    FCurrentPass: Integer;
    FCombinerType: TVXShaderCombinerType;
    FShaderOne: TVXShader;
    FShaderTwo: TVXShader;
    procedure SetShaderOne(const Value: TVXShader);
    procedure SetShaderTwo(const Value: TVXShader);
  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property CombinerType: TVXShaderCombinerType read FCombinerType write FCombinerType default sctOneSPTwoAP;
    property ShaderOne: TVXShader read FShaderOne write SetShaderOne;
    property ShaderTwo: TVXShader read FShaderTwo write SetShaderTwo;
    property CurrentPass : Integer read FCurrentPass stored False;
  public
    constructor Create(AOwner: TComponent); override;
    function ShaderSupported: Boolean; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TVXShaderCombiner = class(TVXCustomShaderCombiner)
  published
    property CombinerType;
    property ShaderOne;
    property ShaderTwo;
    property ShaderStyle;
  end;

implementation


{ TVXCustomShaderCombiner }

procedure TVXCustomShaderCombiner.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVXCustomShaderCombiner then
  begin
    SetShaderOne(TVXCustomShaderCombiner(Source).FShaderOne);
    SetShaderTwo(TVXCustomShaderCombiner(Source).FShaderTwo);
  end;
end;

constructor TVXCustomShaderCombiner.Create(AOwner: TComponent);
begin
  inherited;
  FCombinerType := sctOneSPTwoAP;
end;

procedure TVXCustomShaderCombiner.DoApply(var rci: TVXRenderContextInfo;
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
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

function TVXCustomShaderCombiner.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
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
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;
  Inc(FCurrentPass);
end;

procedure TVXCustomShaderCombiner.Notification(AComponent: TComponent;
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

procedure TVXCustomShaderCombiner.SetShaderOne(const Value: TVXShader);
begin
  if FShaderOne <> nil then
    FShaderOne.RemoveFreeNotification(Self);
  FShaderOne := Value;
  if FShaderOne <> nil then
    FShaderOne.FreeNotification(Self);
end;

procedure TVXCustomShaderCombiner.SetShaderTwo(const Value: TVXShader);
begin
  if FShaderTwo <> nil then
    FShaderTwo.RemoveFreeNotification(Self);
  FShaderTwo := Value;
  if FShaderTwo <> nil then
    FShaderTwo.FreeNotification(Self);
end;

function TVXCustomShaderCombiner.ShaderSupported: Boolean;
begin
  Result := (FShaderOne <> nil) and (FShaderTwo <> nil) and
             FShaderOne.ShaderSupported and FShaderTwo.ShaderSupported;
end;

initialization
  RegisterClasses([TVXCustomShaderCombiner, TVXShaderCombiner]);

end.

