//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VRMLParser<p>

   VRML file format parser.<p>

   <b>History :</b><font size=-1><ul>
      <li>14/01/05 - SG - Added to CVS
   </ul></font>
}
unit VRMLParser;

interface

uses
  Classes, SysUtils, VectorTypes, VectorLists, GLUtils;

type
  TVRMLNode = class
    private
      FNodes : TList;
      FParent : TVRMLNode;
      FName,
      FDefName : String;

      function GetNode(index : Integer) : TVRMLNode;

    public
      constructor Create; virtual;
      constructor CreateOwned(AParent : TVRMLNode);
      destructor Destroy; override;

      function Count : Integer;
      procedure Clear;
      procedure Add(node : TVRMLNode);
      procedure Remove(node : TVRMLNode);
      procedure Delete(index : Integer);

      property Nodes[index : Integer] : TVRMLNode read GetNode; default;
      property Parent : TVRMLNode read FParent;
      property Name : String read FName write FName;
      property DefName : String read FDefName write FDefName;
  end;

  TVRMLSingleArray = class (TVRMLNode)
    private
      FValues : TSingleList;
    public
      constructor Create; override;
      destructor Destroy; override;
      property Values : TSingleList read FValues;
  end;

  TVRMLIntegerArray = class (TVRMLNode)
    private
      FValues : TIntegerList;
    public
      constructor Create; override;
      destructor Destroy; override;
      property Values : TIntegerList read FValues;
  end;

  TVRMLMaterial = class (TVRMLNode)
    private
      FDiffuseColor,
      FAmbientColor,
      FSpecularColor,
      FEmissiveColor : TVector3f;
      FTransparency,
      FShininess : Single;
      FHasDiffuse,
      FHasAmbient,
      FHasSpecular,
      FHasEmissive,
      FHasTransparency,
      FHasShininess : Boolean;

    public
      constructor Create; override;

      property DiffuseColor : TVector3f read FDiffuseColor write FDiffuseColor;
      property AmbientColor : TVector3f read FAmbientColor write FAmbientColor;
      property SpecularColor : TVector3f read FSpecularColor write FSpecularColor;
      property EmissiveColor : TVector3f read FEmissiveColor write FEmissiveColor;
      property Transparency : Single read FTransparency write FTransparency;
      property Shininess : Single read FShininess write FShininess;

      property HasDiffuse : Boolean read FHasDiffuse write FHasDiffuse;
      property HasAmbient : Boolean read FHasAmbient write FHasAmbient;
      property HasSpecular : Boolean read FHasSpecular write FHasSpecular;
      property HasEmissive : Boolean read FHasEmissive write FHasEmissive;
      property HasTransparency : Boolean read FHasTransparency write FHasTransparency;
      property HasShininess : Boolean read FHasShininess write FHasShininess;
  end;

  TVRMLUse = class (TVRMLNode)
    private
      FValue : String;
    public
      property Value : String read FValue write FValue;
  end;

  TVRMLTransform = class (TVRMLNode)
    private
      FCenter : TVector3f;
      FRotation : TVector4f;
      FScaleFactor : TVector3f;
    public
      constructor Create; override;

      property Center : TVector3f read FCenter write FCenter;
      property Rotation : TVector4f read FRotation write FRotation;
      property ScaleFactor : TVector3f read FScaleFactor write FScaleFactor;
  end;

  TVRMLParser = class
    private
      FCursor : Integer;
      FTokens : TStringList;
      FRootNode : TVRMLNode;
      FCurrentNode : TVRMLNode;
      FAllowUnknownNodes : Boolean;
      FDefines : TList;

    protected
      function ReadToken : String;
      function ReadSingle : Single;
      function ReadVector3f : TVector3f;
      function ReadVector4f : TVector4f;

      procedure ReadUnknownArray(defname : String = '');
      procedure ReadUnknownHeirachy(defname : String = '');
      procedure ReadUnknown(unknown_token : String; defname : String = '');
      procedure ReadPointArray(defname : String = '');
      procedure ReadCoordIndexArray(defname : String = '');
      procedure ReadCoordinate3(defname : String = '');
      procedure ReadMaterial(defname : String = '');
      procedure ReadIndexedFaceSet(defname : String = '');
      procedure ReadTransform(defname : String = '');
      procedure ReadSeparator(defname : String = '');
      procedure ReadGroup(defname : String = '');
      procedure ReadDef;
      procedure ReadUse;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Parse(Text : String);

      property RootNode : TVRMLNode read FRootNode;
      property AllowUnknownNodes : Boolean read FAllowUnknownNodes write FAllowUnknownNodes;

  end;

implementation

// ---------------
// --------------- TVRMLNode ---------------
// ---------------

// Create
//
constructor TVRMLNode.Create;
begin
  FNodes:=TList.Create;
end;

// CreateOwned
//
constructor TVRMLNode.CreateOwned(AParent : TVRMLNode);
begin
  Create;
  if Assigned(AParent) then
    AParent.Add(Self);
end;

// Destroy
//
destructor TVRMLNode.Destroy;
begin
  Clear;
  FNodes.Free;
  inherited;
end;

// GetNode
//
function TVRMLNode.GetNode(index : Integer) : TVRMLNode;
begin
  Result:=TVRMLNode(FNodes[index]);
end;

// Count
//
function TVRMLNode.Count : Integer;
begin
  Result:=FNodes.Count;
end;

// Clear
//
procedure TVRMLNode.Clear;
begin
  while FNodes.Count>0 do
    Delete(0);
end;

// Add
//
procedure TVRMLNode.Add(node : TVRMLNode);
begin
  if not Assigned(node) then exit;
  if Assigned(node.Parent) then
    node.Parent.FNodes.Remove(node);
  FNodes.Add(node);
  node.FParent:=Self;
end;

// Remove
//
procedure TVRMLNode.Remove(node : TVRMLNode);
begin
  if not Assigned(node) then exit;
  FNodes.Remove(node);
  node.Free;
end;

// Delete
//
procedure TVRMLNode.Delete(index : Integer);
begin
  if (index<0) or (index>=Count) then exit;
  Nodes[index].Free;
  FNodes.Delete(index);
end;


// ---------------
// --------------- TVRMLSingleArray ---------------
// ---------------

// Create
//
constructor TVRMLSingleArray.Create;
begin
  inherited;
  FValues:=TSingleList.Create;
end;

// Destroy
//
destructor TVRMLSingleArray.Destroy;
begin
  FValues.Free;
  inherited;
end;


// ---------------
// --------------- TVRMLIntegerArray ---------------
// ---------------

// Create
//
constructor TVRMLIntegerArray.Create;
begin
  inherited;
  FValues:=TIntegerList.Create;
end;

// Destroy
//
destructor TVRMLIntegerArray.Destroy;
begin
  FValues.Free;
  inherited;
end;


// ---------------
// --------------- TVRMLMaterial ---------------
// ---------------

// Create
//
constructor TVRMLMaterial.Create;
begin
  inherited;
  // Default shininess value
  FHasDiffuse:=False;
  FHasAmbient:=False;
  FHasSpecular:=False;
  FHasEmissive:=False;
  FHasTransparency:=False;
  FHasShininess:=False;
end;


// ---------------
// --------------- TVRMLTransform ---------------
// ---------------

// Create
//
constructor TVRMLTransform.Create;
begin
  inherited;
  FScaleFactor[0]:=1;
  FScaleFactor[1]:=1;
  FScaleFactor[2]:=1;
end;


// ---------------
// --------------- TVRMLParser ---------------
// ---------------

// Create
//
constructor TVRMLParser.Create;
begin
  FDefines:=TList.Create;
  FRootNode:=TVRMLNode.Create;
  FRootNode.Name:='Root';
  FAllowUnknownNodes:=False;
end;

// Destroy
//
destructor TVRMLParser.Destroy;
begin
  FDefines.Free;
  FRootNode.Free;
  inherited;
end;

// ReadToken
//
function TVRMLParser.ReadToken : String;
begin
  if FCursor<FTokens.Count then begin
    Result:=LowerCase(FTokens[FCursor]);
    Inc(FCursor);
  end else
    Result:='';
end;

// ReadUnknownArray
//
procedure TVRMLParser.ReadUnknownArray;
var
  token : String;
begin
  if AllowUnknownNodes then begin
    FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
    FCurrentNode.Name:='Unknown array';
  end;
  repeat
    token:=ReadToken;
    if token = '' then
      exit;
  until token = ']';
  if AllowUnknownNodes then
    FCurrentNode:=FCurrentNode.Parent;
end;

// ReadUnknownHeirachy
//
procedure TVRMLParser.ReadUnknownHeirachy;
var
  token : String;
begin
  if AllowUnknownNodes then begin
    FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
    FCurrentNode.Name:='Unknown heirachy';
  end;
  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else
      ReadUnknown(token);
  until token = '}';
  if AllowUnknownNodes then
    FCurrentNode:=FCurrentNode.Parent;
end;

// ReadUnknown
//
procedure TVRMLParser.ReadUnknown(unknown_token : String; defname : String);
begin
  if unknown_token = '{' then
    ReadUnknownHeirachy
  else if unknown_token = '[' then
    ReadUnknownArray
  else if (unknown_token<>'}') and (unknown_token<>']') and AllowUnknownNodes then begin
    TVRMLNode.CreateOwned(FCurrentNode).Name:='UNKNOWN['+unknown_token+']';
  end;
end;

// ReadSingle
//
function TVRMLParser.ReadSingle : Single;
begin
  Result:=StrToFloatDef(ReadToken);
end;

// ReadVector3f
//
function TVRMLParser.ReadVector3f : TVector3f;
begin
  Result[0]:=ReadSingle;
  Result[1]:=ReadSingle;
  Result[2]:=ReadSingle;
end;

// ReadVector4f
//
function TVRMLParser.ReadVector4f : TVector4f;
begin
  Result[0]:=ReadSingle;
  Result[1]:=ReadSingle;
  Result[2]:=ReadSingle;
  Result[3]:=ReadSingle;
end;

// ReadPointArray
//
procedure TVRMLParser.ReadPointArray;
var
  token : String;
begin
  FCurrentNode:=TVRMLSingleArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='PointArray';

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '[';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLSingleArray(FCurrentNode).Values.Add(StrToFloatDef(token));
  until token = ']';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadCoordIndexArray
//
procedure TVRMLParser.ReadCoordIndexArray(defname : String = '');
var
  token : String;
begin
  FCurrentNode:=TVRMLIntegerArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='CoordIndexArray';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '[';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLIntegerArray(FCurrentNode).Values.Add(StrToInt(token));
  until token = ']';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadMaterial
//
procedure TVRMLParser.ReadMaterial(defname : String);
var
  token : String;
begin
  FCurrentNode:=TVRMLMaterial.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='Material';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  with TVRMLMaterial(FCurrentNode) do begin
    repeat
      token:=ReadToken;
      if token = '' then
        exit
      else if token = 'diffusecolor' then begin
        DiffuseColor:=ReadVector3f;
        HasDiffuse:=True;
      end else if token = 'ambientcolor' then begin
        AmbientColor:=ReadVector3f;
        HasAmbient:=True;
      end else if token = 'specularcolor' then begin
        SpecularColor:=ReadVector3f;
        HasSpecular:=True;
      end else if token = 'emissivecolor' then begin
        EmissiveColor:=ReadVector3f;
        HasEmissive:=True;
      end else if token = 'transparency' then begin
        Transparency:=ReadSingle;
        HasTransparency:=True;
      end else if token = 'shininess' then begin
        Shininess:=ReadSingle;
        HasShininess:=True;
      end else if token<>'}' then
        ReadUnknown(token);
    until token = '}';
  end;

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadCoordinate3
//
procedure TVRMLParser.ReadCoordinate3(defname : String = '');
var
  token : String;
begin
  FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='Coordinate3';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token = 'point' then
      ReadPointArray
    else if token<>'}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadIndexedFaceSet
//
procedure TVRMLParser.ReadIndexedFaceSet(defname : String = '');
var
  token : String;
begin
  FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='IndexedFaceSet';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token = 'coordindex' then
      ReadCoordIndexArray
    else if token<>'}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadTransform
//
procedure TVRMLParser.ReadTransform(defname : String);
var
  token : String;
begin
  FCurrentNode:=TVRMLTransform.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='Transform';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  with TVRMLTransform(FCurrentNode) do begin
    repeat
      token:=ReadToken;
      if token = '' then
        exit
      else if token = 'rotation' then
        Rotation:=ReadVector4f
      else if token = 'center' then
        Center:=ReadVector3f
      else if token = 'scalefactor' then
        ScaleFactor:=ReadVector3f
      else if token<>'}' then
        ReadUnknown(token);
    until token = '}';
  end;

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadSeparator
//
procedure TVRMLParser.ReadSeparator(defname : String = '');
var
  token : String;
begin
  FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='Separator';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token = 'def' then
      ReadDef
    else if token = 'group' then
      ReadGroup
    else if token = 'separator' then
      ReadSeparator
    else if token = 'use' then
      ReadUse
    else if token = 'transform' then
      ReadTransform
    else if token = 'material' then
      ReadMaterial
    else if token = 'coordinate3' then
      ReadCoordinate3
    else if token = 'indexedfaceset' then
      ReadIndexedFaceSet
    else if token<>'}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadGroup
//
procedure TVRMLParser.ReadGroup(defname : String = '');
var
  token : String;
begin
  FCurrentNode:=TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name:='Group';
  FCurrentNode.DefName:=defname;

  repeat
    token:=ReadToken;
    if token = '' then exit;
  until token = '{';

  repeat
    token:=ReadToken;
    if token = '' then
      exit
    else if token = 'def' then
      ReadDef
    else if token = 'group' then
      ReadGroup
    else if token = 'separator' then
      ReadSeparator
    else if token = 'use' then
      ReadUse
    else if token = 'transform' then
      ReadTransform
    else if token = 'material' then
      ReadMaterial
    else if token = 'coordinate3' then
      ReadCoordinate3
    else if token = 'indexedfaceset' then
      ReadIndexedFaceSet
    else if token<>'}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode:=FCurrentNode.Parent;
end;

// ReadDef
//
procedure TVRMLParser.ReadDef;
var
  defname, token : String;
begin
  defname:=ReadToken;
  token:=ReadToken;
  if token = 'group' then
    ReadGroup(defname)
  else if token = 'separator' then
    ReadSeparator(defname)
  else if token = 'transform' then
    ReadTransform(defname)
  else if token = 'material' then
    ReadMaterial(defname)
  else if token = 'coordinate3' then
    ReadCoordinate3(defname)
  else if token = 'indexedfaceset' then
    ReadIndexedFaceSet(defname)
  else
    ReadUnknown(token);
end;

// ReadUse
//
procedure TVRMLParser.ReadUse;
begin
  with TVRMLUse.CreateOwned(FCurrentNode) do begin
    name:='Use';
    Value:=ReadToken;
  end;
end;

// Parse
//
procedure TVRMLParser.Parse(Text : String);
var
  token : String;
begin
  FTokens:=TStringList.Create;
  FTokens.CommaText:=Text;
  FCursor:=0;
  FCurrentNode:=FRootNode;
  try
    repeat
      token:=ReadToken;
      if token = 'def' then
        ReadDef
      else if token = 'group' then
        ReadGroup
      else if token = 'separator' then
        ReadSeparator
      else if token = 'use' then
        ReadUse
      else if token = 'transform' then
        ReadTransform
      else if token = 'material' then
        ReadMaterial
      else if token = 'coordinate3' then
        ReadCoordinate3
      else if token = 'indexedfaceset' then
        ReadIndexedFaceSet
      else
        ReadUnknown(token);
    until FCursor>=FTokens.Count;
  finally
    FTokens.Free;
  end;
end;

end.
