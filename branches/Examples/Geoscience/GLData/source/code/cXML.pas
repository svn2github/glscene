{** helper functions to reduce the verbosity of the main form}
unit cXML;

interface

uses
  classes,sysutils, XDOM_3_1, cGLGridImportFn,cGLCoordinateAxes,GLObjects,
  cUtilities;

procedure CreateBooleanElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;bBool:boolean);
procedure CreateFloatElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;dFloat:double);
procedure CreateIntegerElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;iInteger:integer);
procedure CreateStringElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;sString:string);

procedure ObtainGridProperties(rNode:TDOMNode;var glgrid:TGLContourgridData);
procedure ObtainGridFileNameAndBlankSub(rNode:TDOMNode;var sFilePath:string;
  var dBlankSub:double);
procedure ObtainAxesProperties(rNode:TDOMNode;var aaxes:TGLCoordinateAxes;
  var dc:TGLDummyCube);

procedure SaveAxesProperties(dd:TDOMDocument;RElement:TDOMElement;
  aaxes:TGLCoordinateAxes;dc:TGLDummyCube);
procedure SaveGridProperties(dd:TDOMDocument;RElement:TDOMElement;
  iType:integer;sName:string;glGrid:TGLContourGriddata);

implementation
// ----- CreateBooleanElement --------------------------------------------------
procedure CreateBooleanElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;bBool:boolean);

var
  e : TDOMElement;

begin
  e := DD.createElement(sName);
  p.appendChild(e);

  if bBool then
    e.appendChild(DD.createTextNode('true'))
  else
    e.appendChild(DD.createTextNode('false'));
end;
// ----- CreateFloatElement ----------------------------------------------------
procedure CreateFloatElement(DD:TDOMDocument;p:TDOMElement;
 sName:string;dFloat:double);

var
  e : TDOMElement;

begin
  e := DD.createElement(sName);
  p.appendChild(e);
  e.appendChild(DD.createTextNode(FloatToStr(dFloat)));
end;
// ----- CreateIntegerElement --------------------------------------------------
procedure CreateIntegerElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;iInteger:integer);

var
  e : TDOMElement;

begin
  e := DD.createElement(sName);
  p.appendChild(e);
  e.appendChild(DD.createTextNode(InttoStr(iInteger)));
end;
// ----- CreateStringElement ---------------------------------------------------
procedure CreateStringElement(DD:TDOMDocument;p:TDOMElement;
  sName:string;sString:string);

var
  e : TDOMElement;

begin
  e := DD.createElement(sName);
  p.appendChild(e);
  e.appendChild(DD.createTextNode(sString));
end;
// ----- ObtainGridFileNameAndBlankSub -----------------------------------------
procedure ObtainGridFileNameAndBlankSub(rNode:TDOMNode;var sFilePath:string;
  var dBlankSub:double);

var
  aElement:TDOMElement;

procedure CheckElement;

begin
  if (aElement.tagName = 'filepath')  and (aElement.hasChildNodes) then
    sFilePath := aElement.FirstChild.nodeValue;;
  if (aElement.tagName = 'blanksub')  and (aElement.hasChildNodes) then
    dBlankSub := strToFloat(aElement.firstChild.NodeValue);
end;

begin
  sFilePath := '';
  dBlankSub := 0.0;
  aElement := rNode.findFirstChildElement;
  CheckElement;
  while (aElement.FindNextSiblingElement <> nil) do
  begin
    aElement := aElement.findNextSiblingElement;
    CheckElement;
  end;
end;
// ----- ObtainGridProperties --------------------------------------------------
procedure ObtainGridProperties(rNode:TDOMNode;var glgrid:TGLContourgridData);

var
  aElement:TDOMElement;

procedure CheckElement;

begin
  if (aElement.tagName = 'polygonmode')  and (aElement.hasChildNodes) then
    glGrid.PolygonMode := StrToInt(aElement.firstChild.nodeValue);
  if (aElement.tagName = 'colourmode')  and (aElement.hasChildNodes) then
    glGrid.ColourMode := StrToInt(aElement.firstChild.NodeValue);
  if (aElement.tagName = 'alpha')  and (aElement.hasChildNodes) then
    glGrid.Alpha := StrToFloat(aElement.firstChild.NodeValue);
  if (aElement.tagName = 'twosided') and (aElement.hasChildNodes) then
    glGrid.TwoSided := (LowerCase(aElement.firstChild.NodeValue)='true');
  if (aElement.tagName = 'enablebasemap')  and (aElement.hasChildNodes) then
    glGrid.EnableBaseMap := (LowerCase(aElement.firstChild.NodeValue)='true');
  if (aElement.tagName = 'basemappath') and (aElement.hasChildNodes) then
    glGrid.baseMapPath:= aElement.firstChild.NodeValue;
  if (aElement.tagName = 'gridname') and (aElement.hasChildNodes) then
    glGrid.gridname:= aElement.firstChild.NodeValue;
  if (aElement.TagName = 'visible') and (aElement.HasChildNodes) then
    glGrid.Visible := LowerCase(aElement.FirstChild.NodeValue) = 'true';
end;

begin
  aElement := rNode.findFirstChildElement;
  CheckElement;
  while (aElement.FindNextSiblingElement <> nil) do
  begin
    aElement := aElement.findNextSiblingElement;
    CheckElement;
  end;
end;
// ----- SaveGridProperties ----------------------------------------------------
procedure SaveGridProperties(dd:TDOMDocument;RElement:TDOMElement;
  iType:integer;sName:string;glGrid:TGLContourGridData);

var
  PElement:TDOMElement;

begin
  case iType of
    0: PElement := DD.createElement('surfergrid');
    1: PElement := DD.createElement('arcinfogrid');
  end;
  RElement.AppendChild(PElement);
  CreateStringElement(DD,PElement,'filepath',sName);
  with glGrid do
  begin
    CreateBooleanElement(DD,PElement,'visible',Visible);
    CreateIntegerElement(DD,PElement,'polygonmode',PolygonMode);
    CreateIntegerElement(DD,PElement,'colourmode',ColourMode);
    CreateFloatElement(DD,PElement,'alpha',Alpha);
    CreateBooleanElement(DD,PElement,'twosided',TwoSided);
    CreateBooleanElement(DD,PElement,'enablebasemap',EnableBaseMap);
    CreateStringElement(DD,PElement,'basemappath',BaseMapPath);
    CreateIntegerElement(DD,PElement,'TileX',TileX);
    CreateIntegerElement(DD,PElement,'TileY',TileY);
    CreateStringElement(DD,PElement,'gridname',GridName);
    CreateFloatElement(DD,PElement,'blanksub',BlankSub);
  end;
end;
// ----- ObtainAxesProperties --------------------------------------------------
procedure ObtainAxesProperties(rNode:TDOMNode;var aaxes:TGLCoordinateAxes;var
  dc:TGLDummyCube);

var
  aElement:TDOMElement;

  procedure CheckElement;

  var
    sVal :string;

  begin
    if aElement.HasChildNodes then
    begin
      sVal := aElement.FirstChild.NodeValue;

// origins
      if (aElement.TagName = 'xorigin') then
        dc.Position.X := StrToFloat(sVal)
      else if (aElement.TagName = 'yorigin') then
        dc.Position.Y := StrToFloat(sVal)
      else if (aElement.TagName = 'zorigin') then
        dc.Position.Z := StrToFloat(sVal)

// visibility
      else if (aElement.TagName = 'visible') then
      begin
        if (LowerCase(sVal)='true') then
          aaxes.ShowAxes(true)
        else
          aAxes.ShowAxes(false);
      end
      else if (aElement.TagName = 'xlabelvisible') then
        aaxes.XLabelVisible := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'ylabelvisible') then
        aaxes.YLabelVisible := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'zlabelvisible') then
        aaxes.ZLabelVisible := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'xygridvisible') then
        aaxes.XYGrid.Visible :=(LowerCase(sVal)='true')
      else if (aElement.TagName = 'yzgridvisible') then
        aaxes.YZGrid.Visible :=(LowerCase(sVal)='true')
      else if (aElement.TagName = 'xzgridvisible') then
        aaxes.XZGrid.Visible :=(LowerCase(sVal)='true')

// axes components
      else if (aElement.TagName = 'xn') then
        aaxes.XN := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'xp') then
        aaxes.XP := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'yn') then
        aaxes.YN := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'yp') then
        aaxes.YP := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'zn') then
        aaxes.ZN := (LowerCase(sVal)='true')
      else if (aElement.TagName = 'zp') then
        aaxes.ZP := (LowerCase(sVal)='true')

// lengths
      else if (aElement.TagName = 'xlength') then
        aaxes.XLength := StrToFloat(sVal)
      else if (aElement.TagName = 'ylength') then
        aaxes.YLength := StrToFloat(sVal)
      else if (aElement.TagName = 'zlength') then
        aaxes.ZLength := StrToFloat(sVal)

// radii
      else if (aElement.TagName = 'xradius') then
        aAxes.XRadius := StrtoFloat(sVal)
      else if (aElement.TagName = 'yradius') then
        aAxes.YRadius := StrtoFloat(sVal)
      else if (aElement.TagName = 'zradius') then
        aAxes.ZRadius := StrtoFloat(sVal)

// axes colours
      else if (aElement.TagName = 'xaxiscolour') then
        aAxes.XAxisColour := HexToColor(sVal)
      else if (aElement.TagName = 'yaxiscolour') then
        aAxes.YAxisColour := HexToColor(sVal)
      else if (aElement.TagName = 'zaxiscolour') then
        aAxes.ZAxisColour := HexToColor(sVal)

// label colours
      else if (aElement.tagName = 'xlabelcolour') then
        aAxes.XLabelColour := HexToColor(sVal)
      else if (aElement.tagName = 'ylabelcolour') then
        aAxes.YLabelColour := HexToColor(sVal)
      else if (aElement.tagName = 'zlabelcolour') then
        aAxes.ZLabelColour := HexToColor(sVal)

// x label positions
      else if (aElement.TagName = 'xlabelstart') then
        aaxes.XLabelStart := StrToFloat(sVal)
      else if (aElement.TagName = 'xlabelstep') then
        aaxes.XLabelStep := StrToFloat(sVal)
      else if (aElement.TagName = 'xlabelstop') then
        aaxes.XLabelStop := StrToFloat(sVal)

// y label positions
      else if (aElement.TagName = 'ylabelstart') then
        aaxes.YLabelStart := StrToFloat(sVal)
      else if (aElement.TagName = 'ylabelstep') then
        aaxes.YLabelStep := StrToFloat(sVal)
      else if (aElement.TagName = 'ylabelstop') then
        aaxes.YLabelStop := StrToFloat(sVal)

// z label positions
      else if (aElement.TagName = 'zlabelstart') then
        aaxes.ZLabelStart := StrToFloat(sVal)
      else if (aElement.TagName = 'zlabelstep') then
        aaxes.ZLabelStep := StrToFloat(sVal)
      else if (aElement.TagName = 'zlabelstop') then
        aaxes.ZLabelStop := StrToFloat(sVal);
    end;
  end;

begin
  aElement := rNode.findFirstChildElement;
  CheckElement;
  while (aElement.FindNextSiblingElement <> nil) do
  begin
    aElement := aElement.findNextSiblingElement;
    CheckElement;
  end;
end;
// ----- SaveAxesProperties ----------------------------------------------------
procedure SaveAxesProperties(dd:TDOMDocument;RElement:TDOMElement;
  aaxes:TGLCoordinateAxes;dc:TGLDummyCube);

var
  PElement : TDOMElement;

begin
// axes settings
  PElement := DD.CreateElement('axes');
  RElement.AppendCHild(PElement);

  CreateBooleanElement(DD,PElement,'visible',aaxes.IsVisible);
  CreateBooleanElement(DD,PElement,'xlabelvisible',aaxes.XLabelVisible);
  CreateBooleanElement(DD,PElement,'ylabelvisible',aaxes.YLabelVisible);
  CreateBooleanElement(DD,PElement,'zlabelvisible',aaxes.ZLabelVisible);

  CreateBooleanElement(DD,PElement,'xn',aAxes.XN);
  CreateBooleanElement(DD,PElement,'xp',aAxes.XP);
  CreateBooleanElement(DD,PElement,'yn',aAxes.YN);
  CreateBooleanElement(DD,PElement,'yp',aAxes.YP);
  CreateBooleanElement(DD,PElement,'zn',aAxes.ZN);
  CreateBooleanElement(DD,PElement,'zp',aAxes.ZP);

  CreateBooleanElement(DD,PElement,'xygridvisible',aaxes.XYGrid.Visible);
  CreateBooleanElement(DD,PElement,'yzgridvisible',aaxes.YZGrid.Visible);
  CreateBooleanElement(DD,PElement,'xzgridvisible',aaxes.XZGrid.Visible);

  CreateFloatElement(DD,PElement,'xorigin',dc.Position.X);
  CreateFloatElement(DD,PElement,'yorigin',dc.Position.Y);
  CreateFloatElement(DD,PElement,'zorigin',dc.Position.Z);

  CreateFloatElement(DD,PElement,'xlength',aaxes.xlength);
  CreateFloatElement(DD,PElement,'ylength',aaxes.ylength);
  CreateFloatElement(DD,PElement,'zlength',aaxes.zlength);

  CreateFloatElement(DD,PElement,'xradius',aaxes.XRadius);
  CreateFloatElement(DD,PElement,'yradius',aaxes.YRadius);
  CreateFloatElement(DD,PElement,'zradius',aaxes.ZRadius);

  CreateStringElement(DD,Pelement,'xaxiscolour',ColorToHex(aaxes.XAxisColour));
  CreateStringElement(DD,Pelement,'yaxiscolour',ColorToHex(aaxes.YAxisColour));
  CreateStringElement(DD,Pelement,'zaxiscolour',ColorToHex(aaxes.ZAxisColour));


  CreateStringElement(DD,PElement,'xlabelcolour',ColorToHex(aaxes.XLabelColour));
  CreateFloatElement(DD,PElement,'xlabelstart',aaxes.XLabelStart);
  CreateFloatElement(DD,PElement,'xlabelstep',aaxes.XLabelStep);
  CreateFloatElement(DD,PElement,'xlabelstop',aaxes.XLabelStop);

  CreateStringElement(DD,PElement,'ylabelcolour',ColorToHex(aaxes.YLabelColour));
  CreateFloatElement(DD,PElement,'ylabelstart',aaxes.yLabelStart);
  CreateFloatElement(DD,PElement,'ylabelstep',aaxes.yLabelStep);
  CreateFloatElement(DD,PElement,'ylabelstop',aaxes.yLabelStop);

  CreateStringElement(DD,PElement,'zlabelcolour',ColorToHex(aaxes.ZLabelColour));
  CreateFloatElement(DD,PElement,'zlabelstart',aaxes.zLabelStart);
  CreateFloatElement(DD,PElement,'zlabelstep',aaxes.zLabelStep);
  CreateFloatElement(DD,PElement,'zlabelstop',aaxes.zLabelStop);
end;
// =============================================================================
end.






