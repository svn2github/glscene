{

This unit uses a freeware version of an XML parser called sdXmlDocuments.
To purchase a real version, please visit http://www.simdesign.nl/xml.html.

}

unit uNEATToXML;

interface

uses
  System.SysUtils,
  sdXmlDocuments,
  uNeatClasses;

  procedure AddGenotypeToXMLFile(const AGenotype : TGenotype; const AName, AFileName : string);
  procedure SaveGenotypeToXMLNode(const AGenotype : TGenotype; const AXMLNode : TXMLNode);
  procedure LoadGenotypeFromXMLNode(const AGenotype : TGenotype; const AXMLNode : TXMLNode);

implementation

uses
  Math;

procedure AddGenotypeToXMLFile(const AGenotype : TGenotype; const AName, AFileName : string);
var
  Doc : TsdXmlDocument;
  GenotypeNode : TXMLNode;
begin
  Doc := TsdXmlDocument.Create;
  try
    if FileExists(AFileName) then
      Doc.LoadFromFile(AFileName)
    else
      Doc.CreateName('Genotypes');

    GenotypeNode := Doc.Root.NodeNew('Genotype');
    GenotypeNode.WriteString('Name', AName, 'Name');
    SaveGenotypeToXMLNode(AGenotype, GenotypeNode);

    Doc.XmlFormat := xfReadable;
    Doc.SaveToFile(AFileName);
  finally
    FreeAndNil(Doc);
  end;
end;

procedure SaveGenotypeToXMLNode(const AGenotype : TGenotype; const AXMLNode : TXMLNode);
var
  i : integer;
  Node : TNode;
  Connect : TConnect;
begin
  AXMLNode.Name := 'Genotype';
  AXMLNode.AttributeAdd('MaxNodeDepth', IntToStr(AGenotype.MaxNodeDepth));
  AXMLNode.WriteFloat('Fitness', AGenotype.Fitness, AGenotype.Fitness);

  for i := 0 to AGenotype.NodeList.Count-1 do
    with AXMLNode.NodeNew('Node') do
    begin
      Node := AGenotype.NodeList[i];

      AttributeAdd('ID', IntToStr(Node.NodeID));
      AttributeAdd('TransferFunction', Node.TransferFunction.TransferFunctionName);

      WriteFloat('XPos', Node.XPos, Node.XPos);
      WriteFloat('YPos', Node.YPos, Node.YPos);

      if Node.IsInputNode then
        AttributeAdd('NodeType', 'input')

      else if Node.IsOutputNode then
        AttributeAdd('NodeType', 'output')

      else
        AttributeAdd('NodeType', 'hidden');
     end;

  for i := 0 to AGenotype.ConnectList.Count-1 do
  begin
    Connect := AGenotype.ConnectList[i];
    if not Connect.Enabled then continue;

    with AXMLNode.NodeNew('Link') do
    begin
      AttributeAdd('InnovationID', IntToStr(Connect.InnovationID));
      WriteFloat('Weight', Connect.Weight, Connect.Weight);
      WriteInteger('HeadNodeID', Connect.HeadNodeID, Connect.HeadNodeID);
      WriteInteger('TailNodeID', Connect.TailNodeID, Connect.TailNodeID);
    end;
  end;
end;


procedure LoadGenotypeFromXMLNode(const AGenotype : TGenotype; const AXMLNode : TXMLNode);
var
  i : integer;
  XNode : TXMLNode;
  NodeID : integer;
  Node : TNode;
  Connect : TConnect;
  idx : integer;
begin
  AGenotype.CacheNodesAndConnects;

  for i := 0 to AXMLNode.NodeCount-1 do
  begin
    XNode := AXMLNode[i];
    if SameText(XNode.Name, 'Node') then
    begin
      NodeID := StrToInt(XNode.AttributeByName('ID'));
      NodeID := AGenotype.AddNewNode(NodeID);
      Node := AGenotype.GetNodeByID(NodeID);

      idx := TransferFunctionList.IndexOf(XNode.AttributeByName('TransferFunction'));
      Node.TransferFunction := TTransferFunction(TransferFunctionList.Objects[idx]);

      Node.XPos := XNode.ReadFloat('XPos', 0);
      Node.YPos := XNode.ReadFloat('YPos', 0);

      if SameText(XNode.AttributeByName('NodeType'), 'input') then
        Node.IsInputNode := true

      else if SameText(XNode.AttributeByName('NodeType'), 'output') then
        Node.IsOutputNode := true;

      AGenotype.NEATPopulation.NodeInnovationCounter := max(AGenotype.NEATPopulation.NodeInnovationCounter, NodeID+1);
    end
    else if SameText(XNode.Name, 'Link') then
    begin
      Connect := CacheHandler.GetOrCreateConnect(AGenotype);
      Connect.HeadNodeID := Xnode.ReadInteger('HeadNodeID', 0);
      Connect.TailNodeID := Xnode.ReadInteger('TailNodeID', 0);
      Connect.Weight := XNode.ReadFloat('Weight', 0);
      Connect.InnovationID := StrToInt(XNode.AttributeByName('InnovationID'));
      Connect.Enabled := true;
      AGenotype.ConnectList.Add(Connect);
      AGenotype.NEATPopulation.ConnectInnovationCounter := max(AGenotype.NEATPopulation.ConnectInnovationCounter, Connect.InnovationID+1);
    end;
  end;

  AGenotype.MaxNodeDepth := StrToInt(AXMLNode.AttributeByName('MaxNodeDepth'));
  AGenotype.PreparePhenotype;//}
end;
end.
