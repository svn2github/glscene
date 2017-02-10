unit uTransferFunctionClasses;

interface

uses
  Math,uNEATClasses;

type
  // TTransferFunctionSigmoid is the most commonly used transfer function
  TTransferFunctionSigmoid=class(TTransferFunction)
   function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionGaussian implements a bell-curve transfer function
  TTransferFunctionGaussian=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionCAP only allows values from -1 to +1
  TTransferFunctionCAP=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionMUL
  TTransferFunctionMUL=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionONE
  TTransferFunctionONE=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionADD
  TTransferFunctionADD=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionMAX
  TTransferFunctionMAX=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionOR
  TTransferFunctionOR=class(TTransferFunctionMAX)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionMIN = AND!
  TTransferFunctionMIN=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionAND
  TTransferFunctionAND=class(TTransferFunctionMIN)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionIF
  TTransferFunctionIF=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunctionTHRESHOLD
  TTransferFunctionTHRESHOLD=class(TTransferFunction)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunction1DelaySigmoid is a one step delayed sigmoid
  TTransferFunction1DelaySigmoid=class(TTransferFunctionSigmoid)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunction2DelaySigmoid is a two step delayed sigmoid
  TTransferFunction2DelaySigmoid=class(TTransferFunctionSigmoid)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunction1DelayGaussian is a one step delayed gaussian
  TTransferFunction1DelayGaussian=class(TTransferFunctionGaussian)
    function CalculateValue(Node : TNode) : double;override;
  end;

  // TTransferFunction2DelayGaussian is a two step delayed gaussian
  TTransferFunction2DelayGaussian=class(TTransferFunctionGaussian)
    function CalculateValue(Node : TNode) : double;override;
  end;

implementation

{ TTransferFunctionSigmoid }

function TTransferFunctionSigmoid.CalculateValue(Node: TNode): double;
var
  NodeSum : double;
begin
  // Calculate the node sum
  NodeSum := GetSum(Node);

  // Apply the adjusted sigmoid
  result := AdjustedSigmoid(NodeSum);
end;

{ TTransferFunctionGaussian }

function TTransferFunctionGaussian.CalculateValue(Node: TNode): double;
begin
  result := AdjustedGaussian(GetSum(Node));
end;

{ TTransferFunctionCAP }

function TTransferFunctionCAP.CalculateValue(Node: TNode): double;
begin
  result := Min(Max(GetSum(Node),-1),1);
end;

{ TTransferFunctionADD }

function TTransferFunctionADD.CalculateValue(Node: TNode): double;
begin
  result := GetSum(Node);
end;

{ TTransferFunctionMUL }

function TTransferFunctionMUL.CalculateValue(Node: TNode): double;
begin
  result := GetMul(Node);
end;

{ TTransferFunctionONE }

function TTransferFunctionONE.CalculateValue(Node: TNode): double;
begin
  // ONLY USED FOR DEBUGGING!
  result := 1;
end;

{ TTransferFunctionMAX }

function TTransferFunctionMAX.CalculateValue(Node: TNode): double;
var
  i : integer;
  m : double;
  Connect : TConnect;
begin
  if Node.EnabledConnectList.Count=0 then
  begin
    result := 0;
    exit;
  end;

  m := 0;

  for i := 0 to Node.EnabledConnectList.Count-1 do
  begin
    Connect := Node.EnabledConnectList[i];

    if i = 0 then
      m := Connect.GetValue
    else
      m := max(m, Connect.GetValue);
  end;

  result := m;
end;

{ TTransferFunctionMIN }

function TTransferFunctionMIN.CalculateValue(Node: TNode): double;
var
  i : integer;
  m : double;
  Connect : TConnect;
begin
  if Node.EnabledConnectList.Count=0 then
  begin
    result := 0;
    exit;
  end;

  m := 0;

  for i := 0 to Node.EnabledConnectList.Count-1 do
  begin
    Connect := Node.EnabledConnectList[i];

    if i = 0 then
      m := Connect.GetValue
    else
      m := min(m, Connect.GetValue);
  end;

  result := m;
end;

{ TTransferFunctionIF }

function TTransferFunctionIF.CalculateValue(Node: TNode): double;
var
  i : integer;
  sum : double;
  Connect : TConnect;
  val : double;
begin
  if Node.EnabledConnectList.Count=0 then
  begin
    result := 0;
    exit;
  end;

  with Node.EnabledConnectList[0] do
    val := GetValue;

  if Val<=0 then
  begin
    // First value is considered the "false" value, just return that
    if Node.EnabledConnectList.Count>1 then
      with Node.EnabledConnectList[1] do
      begin
        result := GetValue;
        exit;
      end
    else
      result := -1;
  end else
  begin
    // Sum up all the nodes from 2 and up!
    if Node.EnabledConnectList.Count>2 then
    begin
      sum := 0;

      for i := 2 to Node.EnabledConnectList.Count-1 do
      begin
        Connect := Node.EnabledConnectList[i];

        if Connect.Enabled then
          sum := sum + Connect.GetValue;
      end;

      result := sum;
    end else
      result := 1;
  end;
end;

{ TTransferFunctionTHRESHOLD }

function TTransferFunctionTHRESHOLD.CalculateValue(Node: TNode): double;
begin
  if GetSum(Node)>0 then
    result := 1
  else
    result := -1;
end;

{ TTransferFunctionOR }

function TTransferFunctionOR.CalculateValue(Node: TNode): double;
var
  f : double;
begin
  f := inherited CalculateValue(Node);

  result := BoolToFloat(FloatToBool(f));
end;

{ TTransferFunctionAND }

function TTransferFunctionAND.CalculateValue(Node: TNode): double;
var
  f : double;
begin
  f := inherited CalculateValue(Node);

  result := BoolToFloat(FloatToBool(f));
end;

{ TTransferFunction1DelaySigmoid }

function TTransferFunction1DelaySigmoid.CalculateValue(
  Node: TNode): double;
begin
  result := Node.ComingValue[0];

  Node.ComingValue[0] := inherited CalculateValue(Node);
end;

{ TTransferFunction2DelaySigmoid }

function TTransferFunction2DelaySigmoid.CalculateValue(
  Node: TNode): double;
begin
  result := Node.ComingValue[0];
  Node.ComingValue[0] := Node.ComingValue[1];
  Node.ComingValue[1] := inherited CalculateValue(Node);
end;

{ TTransferFunction1DelayGaussian }

function TTransferFunction1DelayGaussian.CalculateValue(
  Node: TNode): double;
begin
  result := Node.ComingValue[0];

  Node.ComingValue[0] := inherited CalculateValue(Node);
end;


{ TTransferFunction2DelayGaussian }

function TTransferFunction2DelayGaussian.CalculateValue(
  Node: TNode): double;
begin
  result := Node.ComingValue[0];
  Node.ComingValue[0] := Node.ComingValue[1];
  Node.ComingValue[1] := inherited CalculateValue(Node);
end;

initialization
  // RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create);
{  RegisterNewTransferFuntion('Main','IF',TTransferFunctionIF.Create);
  RegisterNewTransferFuntion('Main','Cap',TTransferFunctionCAP.Create);
  RegisterNewTransferFuntion('Main','Mul',TTransferFunctionMUL.Create);
  RegisterNewTransferFuntion('Main','1/-1',TTransferFunctionTHRESHOLD.Create);
  RegisterNewTransferFuntion('Main','Add',TTransferFunctionADD.Create);//}
{  RegisterNewTransferFuntion('Main','AND',TTransferFunctionAND.Create);
  RegisterNewTransferFuntion('Main','OR',TTransferFunctionOR.Create);//}
 { RegisterNewTransferFuntion('Main','Max',TTransferFunctionMAX.Create);
  RegisterNewTransferFuntion('Main','Min',TTransferFunctionMIN.Create);//}
end.
