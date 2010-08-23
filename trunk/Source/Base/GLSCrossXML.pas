//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCrossXML<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GLSCrossXML;

interface

uses
{$IFNDEF FPC}
  Variants,
  XMLIntf,
  XMLDoc,
  XMLDom;
{$ELSE}
  DOM,
  XMLRead,
  XMLWrite;
{$ENDIF}

{$IFNDEF FPC}
type
  GLSXMLDocument = IXMLDocument;
  GLSXMLNode = IXMLNode;
  GLSDOMNode = IDOMNode;
{$ENDIF}

{$IFDEF FPC}
type
  GLSXMLDocument = TXMLDocument;
  GLSXMLNode = TDOMNode;
  GLSDOMNode = TDOMNode;
{$ENDIF}
function GLSNewXMLDocument: GLSXMLDocument;
function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean; overload;
function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode; overload;
procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string); overload;
procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string); overload;
function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;

implementation

{$IFNDEF FPC}

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := NewXMLDocument();
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  attr: OleVariant;
begin
  attr := 0;
  attr := XMLNode.Attributes[AttrName];
  Result := not VarIsNull(attr);
  if Result then
    Value := attr;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
begin
  XMLNode.Attributes[AttrName] := Value;
end;

procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string);
var
  E: IDOMElement;
begin
  E := DOMNode as IDOMElement;
  E.SetAttribute(AttrName, Value);
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
begin
  ChildNode := ParentNode.ChildNodes.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): Boolean;
begin
  AText := XMLNode.Text;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
begin
  Result := XMLNode.AttributeNodes.Count;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
begin
  Result := XMLNode.AttributeNodes[Idx];
end;

{$ENDIF}

{$IFDEF FPC}

function GLSNewXMLDocument: GLSXMLDocument;
begin
  Result := TXMLDocument.Create;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; out Value: string): Boolean;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Value := E[AttrName];
  Result := Length(Value) > 0;
end;

procedure SetXMLAttribute(const XMLNode: GLSXMLNode; const AttrName: string; const Value: string);
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  E[AttrName] := Value;
end;

procedure SetXMLAttribute(const DOMNode: GLSDOMNode; const AttrName: string; const Value: string);
var
  E: TDOMElement;
begin
  E := DOMNode as TDOMElement;
  E[AttrName] := Value;
end;

function FindXMLNode(const ParentNode: GLSXMLNode; const NodeName: string; out ChildNode: GLSXMLNode): Boolean;
var
  E: TDOMElement;
begin
  E := ParentNode as TDomElement;
  ChildNode := E.FindNode(NodeName);
  Result := Assigned(ChildNode);
end;

function CreateDOMNode(const ParentNode: GLSDOMNode; const NodeName: string): GLSDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

function GetXMLText(const XMLNode: GLSXMLNode; out AText: string): string;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  AText := E.TextContent;
  Result := Length(AText)>0;
end;

function GetXMLAttributeCount(const XMLNode: GLSXMLNode): Integer;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes.Length;
end;

function GetXMLAttribute(const XMLNode: GLSXMLNode; Idx: Integer): GLSXMLNode;
var
  E: TDOMElement;
begin
  E := XMLNode as TDOMElement;
  Result := E.Attributes[Idx];
end;

{$ENDIF}

end.
