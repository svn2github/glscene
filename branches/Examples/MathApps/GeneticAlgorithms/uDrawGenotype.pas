unit uDrawGenotype;

interface

uses
  uNEATClasses, graphics;

const
  cWEIGHT_WIDTH_MULTIPLIER = 1;
  cXWIDTH = 400;

  procedure DrawGenotype(
    Genotype : TGenotype;
    Canvas : TCanvas;
    DrawNodeNumbers : boolean = true;
    DrawConnectNumbers : boolean = true;
    FullValues : boolean = false;
    AAddRandom : boolean=true);

implementation

uses
  SysUtils;

procedure DrawGenotype(Genotype: TGenotype; Canvas: TCanvas; DrawNodeNumbers : boolean = true; DrawConnectNumbers : boolean = true; FullValues : boolean = false; AAddRandom : boolean = true);
  function tX( x : double) : integer;
  begin
    result := trunc(x)+20;
  end;

  function tY( y : double) : integer;
  begin
    result := trunc(y*2)+20;
  end;

  procedure DrawNode(Node : TNode);
  var
    x, y : integer;
  begin
    x := tX(Node.XPos);
    y := tY(Node.YPos);

    if Node.BiasNode then
      Canvas.Brush.Color := clYellow
    else if Node.IsInputNode then
      Canvas.Brush.Color := clBlue
    else if Node.IsOutputNode then
      Canvas.Brush.Color := clGreen
    else
      Canvas.Brush.Color := clWhite;

    Canvas.Ellipse(x-5,y-5,x+5,y+5);

    Canvas.Brush.Color := clWhite;

    if DrawNodeNumbers then
      Canvas.TextOut(x+5, y-5, IntToStr(Node.NodeID));

    if FullValues then
    begin
      Canvas.TextOut(x+5, y-5+13, Format('%1.2f',[Node.Value]));
      Canvas.TextOut(x+5, y-5+13*2, Format('%1.2f',[Node.OldValue]));
    end;
  end;

  procedure DrawConnect(Connect : TConnect);
  var
    NodeFrom, NodeTo : TNode;
  begin
    if not Connect.Enabled then
      exit;

    try
      if Connect.Weight>0 then
        Canvas.Pen.Color := clBlue
      else
        Canvas.Pen.Color := clRed;

      NodeFrom := Genotype.GetNodeByID(Connect.TailNodeID);
      NodeTo := Genotype.GetNodeByID(Connect.HeadNodeID);

      Assert(Assigned(NodeFrom),Format('TailNodeID (%d) could not be located!',[Connect.TailNodeID])+#13#10+Genotype.SaveToString);
      Assert(Assigned(NodeTo),Format('HeadNodeID (%d) could not be located!',[Connect.HeadNodeID])+#13#10+Genotype.SaveToString);

      // psDashDot doesn't work with widths' different than 1
      {if NodeFrom.BiasNode then
        Canvas.Pen.Style := psDashDot;//}

      Canvas.Pen.Width := trunc(abs(Connect.Weight)*cWEIGHT_WIDTH_MULTIPLIER)+1;

      Canvas.MoveTo(tx(NodeFrom.XPos), ty(NodeFrom.YPos));
      Canvas.LineTo(tx(NodeTo.XPos), ty(NodeTo.YPos));


      if DrawConnectNumbers then
        Canvas.TextOut(tx((NodeFrom.XPos+NodeTo.XPos)/2)-6, ty((NodeFrom.YPos+NodeTo.YPos)/2)-6, IntToStr(Connect.InnovationID));

      if FullValues then
      begin
        Canvas.TextOut(tx((NodeFrom.XPos+NodeTo.XPos)/2), ty((NodeFrom.YPos+NodeTo.YPos)/2), Format('%1.2f',[Connect.Weight]));
      end;
    finally
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psSolid;
    end;
  end;
var
  Level, i : integer;
  LastYPos, CurrentYPos, CurrentXPos : double;
  NodesOnThisLevel : integer;
  dx : double;
begin
  Canvas.FillRect(Canvas.ClipRect);
  // Reset all xpos
  for i := 0 to Genotype.NodeList.Count-1 do
    Genotype.NodeList[i].XPos := 0;

  LastYPos := -1;

  for Level := 0 to Genotype.MaxNodeDepth-1 do
  begin
    CurrentYPos := 100;

    // Find the next ypos
    for i := 0 to Genotype.NodeList.Count-1 do
      if (Genotype.NodeList[i].YPos > LastYPos) and
         (Genotype.NodeList[i].YPos < CurrentYPos) then
        CurrentYPos := Genotype.NodeList[i].YPos;

    CurrentXPos := 0;


    // Calculate how many nodes are on this level
    NodesOnThisLevel := 0;
    for i := 0 to Genotype.NodeList.Count-1 do
      if Genotype.NodeList[i].YPos = CurrentYPos then
        inc(NodesOnThisLevel);

    // We've got the ypos, setup their exposes
    dx := cXWIDTH/(NodesOnThisLevel+1);
    for i := 0 to Genotype.NodeList.Count-1 do
      if Genotype.NodeList[i].YPos = CurrentYPos then
      begin
        Genotype.NodeList[i].XPos := dx + CurrentXPos * dx;

        if (Level<>0) and (Level<>Genotype.MaxNodeDepth-1) and AAddRandom then
          Genotype.NodeList[i].XPos := Genotype.NodeList[i].XPos + random(30)-15;

        CurrentXPos := CurrentXPos + 1;
      end;
    LastYPos := CurrentYPos;
  end;


  // Draw all connects
  for i := 0 to Genotype.ConnectList.Count-1 do
    DrawConnect(Genotype.ConnectList[i]);

  // Draw all nodes
  for i := 0 to Genotype.NodeList.Count-1 do
    DrawNode(Genotype.NodeList[i]);
end;
end.
 