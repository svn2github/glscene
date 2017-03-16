{-------------------------------------------------------------------------------
 Unit Name: cGLCoordinateAxes
 Author:    HochwimmerA
 Purpose:   Helper class for laying out coordinate axis and flat text labels
 $Id: cGLCoordinateAxes.pas,v 1.9 2004/07/08 09:54:53 hochwimmera Exp $
-------------------------------------------------------------------------------}
unit cGLCoordinateAxes;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.StdCtrls,

   
  GLVectorgeometry, GLBitmapFont, GLGraph, GLGeomObjects, GLGraphics, GLObjects,
  GLTexture, GLWindowsFont, GLCrossPlatform, GLColor;


type
	TAxesPart  = (apXN,apXP,apYN,apYP,apZN,apZP);
	TAxesParts = set of TAxesPart;

  TGLCoordinateAxes = class(TObject)
  private

    fDummyCube : TGLDummyCube;
    fLabelFont:TGLWindowsBitMapFont;
    fParts : TAxesParts;
    fXYGrid : TGLXYZGrid;
    fXZGrid : TGLXYZGrid;
    fYZGrid : TGLXYZGrid;

    fXArrow : TGLArrowLine;
    fXLabelColour : TColor;
    fXLabelList : TStringList;
    fXLabelStart : double;
    fXLabelStep : double;
    fXLabelStop : double;
    fXLabelVisible : boolean;
    fXLength : double;
    fXRadius : double;

    fYArrow : TGLArrowLine;
    fYLabelColour : TColor;
    fYLabelList : TStringList;
    fYLabelStart : double;
    fYLabelStep : double;
    fYLabelStop : double;
    fYLabelVisible : boolean;
    fYLength : double;
    fYRadius : double;

    fZArrow : TGLArrowLine;
    fZLabelColour : TColor;
    fZLabelList : TStringList;
    fZLabelStart : double;
    fZLabelStep : double;
    fZLabelStop : double;
    fZLabelVisible : boolean;
    fZLength : double;
    fZRadius : double;

    fScaleX:double;
    fScaleY:double;
    fScaleZ:double;
    fVisible:boolean;
  protected
    procedure Initialise;
    procedure UpdateXYGrid;
    procedure UpdateYZGrid;
    procedure UpdateXZGrid;

    procedure ClearXLabels;
    procedure ClearYLabels;
    procedure ClearZLabels;

    function GetXN : boolean;
    function GetXP : boolean;
    function GetYN : boolean;
    function GetYP : boolean;
    function GetZN : boolean;
    function GetZP : boolean;

    procedure SetXN(bValue:boolean);
    procedure SetXP(bValue:boolean);
    procedure SetYN(bValue:boolean);
    procedure SetYP(bValue:boolean);
    procedure SetZN(bValue:boolean);
    procedure SetZP(bValue:boolean);

    procedure Render;
    procedure SetParts(aParts:TAxesParts);

    procedure SetXAxisColour(aCol : TColor);
    function GetXAxisColour : TColor;

    procedure SetYAxisColour(aCol : TColor);
    function GetYAxisColour : TColor;

    procedure SetZAxisColour(aCol : TColor);
    function GetZAxisColour : TColor;

    procedure SetXLabelColour(aCol:TColor);
    procedure SetXLabelVisible(bVisible:boolean);
    procedure SetYLabelColour(aCol:TColor);
    procedure SetYLabelVisible(bVisible:boolean);
    procedure SetZLabelColour(aCol:TColor);
    procedure SetZLabelVisible(bVisible:boolean);

    procedure SetXLength(dLength:double);
    procedure SetXRadius(dRadius:double);

    procedure SetYLength(dLength:double);
    procedure SetYRadius(dRadius:double);

    procedure SetZLength(dLength:double);
    procedure SetZRadius(dRadius:double);

    procedure SetScaleX(dScale:double);
    procedure SetScaleY(dScale:double);
    procedure SetScaleZ(dScale:double);

  public
// dummy cube is situated @ origin
    constructor Create(aDummyCube:TGLDummyCube);
    destructor Destroy;override;
    procedure GenerateXLabels;
    procedure GenerateYLabels;
    procedure GenerateZLabels;
    procedure ShowAxes(bshow:boolean);
    function IsVisible:boolean;

    property XAxisColour : TColor read GetXAxisColour write SetXAxisColour;
    property YAxisColour : TColor read GetYAxisColour write SetYAxisColour;
    property ZAxisColour : TColor read GetZAxisColour write SetZAxisColour;

    property LabelFont:TGLWindowsBitMapFont read fLabelFont write fLabelFont;
    property Parts:TAxesParts read fParts write SetParts;

    property XN : boolean read GetXN write SetXN;
    property XP : boolean read GetXP write SetXP;
    property YN : boolean read GetYN write SetYN;
    property YP : boolean read GetYP write SetYP;
    property ZN : boolean read GetZN write SetZN;
    property ZP : boolean read GetZP write SetZP;

// hide these objects...
    property XYGrid : TGLXYZGrid read fXYgrid write fXYGrid;
    property YZGrid : TGLXYZGrid read fYZgrid write fYZgrid;
    property XZGrid : TGLXYZGrid read fXZgrid write fXZgrid;

    property XLabelStart : double read fXLabelStart write fXLabelStart;
    property XLabelStep : double read fXLabelStep write fXLabelStep;
    property XLabelStop : double read fXLabelStop write fXLabelStop;
    property XLabelColour : TColor read fXlabelColour write SetXLabelColour;
    property XLabelVisible : boolean read fXLabelVisible write SetXLabelVisible;

    property XLength:double read fXLength write SetXLength;
    property YLength:double read fYLength write SetYLength;
    property ZLength:double read fZLength write SetZLength;

    property XRadius:double read fXRadius write SetXRadius;
    property YRadius:double read fYRadius write SetYRadius;
    property ZRadius:double read fZRadius write SetZRadius;

    property YLabelStart : double read fYLabelStart write fYLabelStart;
    property YLabelStep : double read fYLabelStep write fYLabelStep;
    property YLabelStop : double read fYLabelStop write fYLabelStop;
    property YLabelColour : TColor read fYLabelColour write SetYLabelColour;
    property YLabelVisible : boolean read fYLabelVisible write SetYLabelVisible;


    property ZlabelStart : double read fzLabelStart write fzLabelStart;
    property ZLabelStep : double read fzLabelStep write fzLabelStep;
    property ZLabelStop : double read fzLabelStop write fzLabelStop;
    property ZLabelColour : TColor read fZLabelColour write SetZLabelColour;
    property ZLabelVisible : boolean read fZLabelVisible write SetZLabelVisible;
    
    property ScaleX:double read fScaleX write SetScaleX;
    property ScaleY:double read fScaleY write SetScaleY;
    property ScaleZ:double read fScaleZ write SetScaleZ;
  end;

//--------------------------------------------------------------------
//--------------------------------------------------------------------
//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------
//--------------------------------------------------------------------
//--------------------------------------------------------------------

// ----- TGLCoordinateAxes.SetScaleX -------------------------------------------
procedure TGLCoordinateAxes.SetScaleX(dScale:double);

begin
  fScaleX := dScale;
  GenerateXLabels;
end;
// ----- TGLCoordinateAxes.SetScaleY -------------------------------------------
procedure TGLCoordinateAxes.SetScaleY(dScale:double);

begin
  fScaleY := dScale;
  GenerateyLabels;
end;

// ----- TGLCoordinateAxes.SetScaleZ -------------------------------------------
procedure TGLCoordinateAxes.SetScaleZ(dScale:double);

begin
  fScaleZ := dScale;
  GenerateZLabels;
end;
// ----- TGLCoordinateAxes.ClearXLabels ----------------------------------------
procedure TGLCoordinateAxes.ClearXLabels;

begin
  while (fXLabelList.Count > 0) do
  begin
    TGLFlatText(fXLabelList.Objects[0]).Free;
    fXLabelList.Delete(0);
  end;
  fXLabelList.Clear;
end;
// ----- TGLCoordinateAxes.ClearYLabels ----------------------------------------
procedure TGLCoordinateAxes.ClearYLabels;

begin
  while (fYLabelList.Count > 0) do
  begin
    TGLFlatText(fYLabelList.Objects[0]).Free;
    fYLabelList.Delete(0);
  end;
  fYLabelList.Clear;
end;
// ----- TGLCoordinateAxes.ClearZLabels ----------------------------------------
procedure TGLCoordinateAxes.ClearZLabels;

begin
  while (fzLabelList.Count > 0) do
  begin
    TGLFlatText(fzLabelList.Objects[0]).Free;
    fzLabelList.Delete(0);
  end;
  fzLabelList.Clear;
end;
// ----- TGLCoordinateAxes.GetXN -----------------------------------------------
function TGLCoordinateAxes.GetXN:boolean;

begin
  result := apXN in Parts;
end;
// ----- TGLCoordinateAxes.GetXP -----------------------------------------------
function TGLCoordinateAxes.GetXP:boolean;

begin
  result := apXP in Parts;
end;
// ----- TGLCoordinateAxes.GetYN -----------------------------------------------
function TGLCoordinateAxes.GetYN:boolean;

begin
  result := apYN in Parts;
end;
// ----- TGLCoordinateAxes.GetYP -----------------------------------------------
function TGLCoordinateAxes.GetYP:boolean;

begin
  result := apYP in Parts;
end;
// ----- TGLCoordinateAxes.GetZN -----------------------------------------------
function TGLCoordinateAxes.GetZN:boolean;

begin
  result := apZN in Parts;
end;
// ----- TGLCoordinateAxes.GetZP -----------------------------------------------
function TGLCoordinateAxes.GetZP:boolean;

begin
  result := apZP in Parts;
end;
// ----- TGLCoordinateAxes.SetXN -----------------------------------------------
procedure TGLCoordinateAxes.SetXN(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apXn]
  else
    Parts := Parts - [apxn];
end;
// ----- TGLCoordinateAxes.SetXP -----------------------------------------------
procedure TGLCoordinateAxes.SetXP(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apXP]
  else
    Parts := Parts - [apxP];
end;

// ----- TGLCoordinateAxes.SetYN -----------------------------------------------
procedure TGLCoordinateAxes.SetYN(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apYn]
  else
    Parts := Parts - [apYn];
end;
// ----- TGLCoordinateAxes.SetYP -----------------------------------------------
procedure TGLCoordinateAxes.SetYP(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apYP]
  else
    Parts := Parts - [apYP];
end;
// ----- TGLCoordinateAxes.SetZN -----------------------------------------------
procedure TGLCoordinateAxes.SetZN(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apZn]
  else
    Parts := Parts - [apZn];
end;
// ----- TGLCoordinateAxes.SetZP -----------------------------------------------
procedure TGLCoordinateAxes.SetZP(bValue:boolean);

begin
  if bValue then
    Parts := Parts + [apZP]
  else
    Parts := Parts - [apZP];
end;
// ----- TGLCoordinateAxes.Render ----------------------------------------------
procedure TGLCoordinateAxes.Render;

begin
// constructing X arrow
  if (apXN in Parts) and (apXP in Parts) then
  begin
    fXArrow.Position.X := 0;
    fXArrow.Height := fXLength;
    fXArrow.Parts := [alLine, alTopArrow, alBottomArrow];
  end else if (apXN in Parts) and not (apXP in Parts) then
  begin
    fXArrow.Position.X := 0.5*fXLength;
    fXArrow.Height := fXLength;
    fXArrow.Parts := [alLine, alTopArrow];
  end else if not (apXN in Parts) and (apXP in Parts) then
  begin
    fXArrow.Position.X := -0.5*fXLength;
    fXArrow.Height := fXLength;
    fXArrow.Parts := [alLine, alBottomArrow]
  end else
    fXArrow.Parts := [];

// constructing Y arrow
  if (apYN in Parts) and (apYP in Parts) then
  begin
    fYArrow.Position.Y := 0;
    fYArrow.Height := fYLength;
    fYArrow.Parts := [alLine, alTopArrow, alBottomArrow];
  end else if (apYN in Parts) and not (apYP in Parts) then
  begin
    fYArrow.Position.Y := 0.5*fYLength;
    fYArrow.Height := fYLength;
    fYArrow.Parts := [alLine, alTopArrow];
  end else if not (apYN in Parts) and (apYP in Parts) then
  begin
    fYArrow.Position.Y := -0.5*fYLength;
    fYArrow.Height := fYLength;
    fYArrow.Parts := [alLine, alBottomArrow]
  end else
    fYArrow.Parts := [];

// constructing z arrow - inverted
  if (apZN in Parts) and (apZP in Parts) then
  begin
    fZArrow.Position.Z := 0;
    fZArrow.Height := fZLength;
    fZArrow.Parts := [alLine, alTopArrow, alBottomArrow];
  end else if (apZN in Parts) and not (apZP in Parts) then
  begin
    fZArrow.Position.Z := -0.5*fZLength;
    fZArrow.Height := fZLength;
    fZArrow.Parts := [alLine, alTopArrow];
  end else if not (apZN in Parts) and (apZP in Parts) then
  begin
    fZArrow.Position.Z := 0.5*fZLength;
    fZArrow.Height := fzLength;
    fZArrow.Parts := [alLine, alBottomArrow]
  end else
    fZArrow.Parts := [];
end;
// ----- TGLCoordinateAxes.SetParts --------------------------------------------
procedure TGLCoordinateAxes.SetParts(aParts:TAxesParts);

begin
  if fParts <> aParts then
  begin
    fParts := aParts;
    Render;
  end;
end;
// ----- TGLCoordinateAxes.GetXAxisColour --------------------------------------
function TGLCoordinateAxes.GetXAxisColour:TColor;

begin
  result := fXArrow.Material.FrontProperties.Emission.AsWinColor;
end;
// ----- TGLCoordinateAxes.SetAxisColour ---------------------------------------
procedure TGLCoordinateAxes.SetXAxisColour(aCol : TColor);

begin
  fXArrow.Material.FrontProperties.Emission.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.GetYAxisColour --------------------------------------
function TGLCoordinateAxes.GetYAxisColour:TColor;

begin
  result := fYArrow.Material.FrontProperties.Emission.AsWinColor;
end;
// ----- TGLCoordinateAxes.SetYAxisColour --------------------------------------
procedure TGLCoordinateAxes.SetYAxisColour(aCol : TColor);

begin
  fYArrow.Material.FrontProperties.Emission.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.GetZAxisColour --------------------------------------
function TGLCoordinateAxes.GetZAxisColour:TColor;

begin
  result := fZArrow.Material.FrontProperties.Emission.AsWinColor;
end;
// ----- TGLCoordinateAxes.SetZAxisColour --------------------------------------
procedure TGLCoordinateAxes.SetZAxisColour(aCol : TColor);

begin
  fZArrow.Material.FrontProperties.Emission.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.SetXLabelColour ------------------------------------
procedure TGLCoordinateAxes.SetXLabelColour(aCol:TColor);

var
  i:integer;

begin
  fXLabelColour := aCol;
  for i:=0 to fXlabelList.Count-1 do
    TGLFlatText(fXLabelList.Objects[i]).ModulateColor.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.SetXLabelVisible ------------------------------------
procedure TGLCoordinateAxes.SetXLabelVisible(bVisible:boolean);

var
  i:integer;

begin
  fXLabelVisible := bVisible;
  for i:=0 to fXLabelList.Count-1 do
    TGLFlatText(fXLabelList.Objects[i]).Visible := bVisible;
end;

// ----- TGLCoordinateAxes.SetYLabelColour ------------------------------------
procedure TGLCoordinateAxes.SetYLabelColour(aCol:TColor);

var
  i:integer;

begin
  fYLabelColour := aCol;
  for i:=0 to fYlabelList.Count-1 do
    TGLFlatText(fYLabelList.Objects[i]).ModulateColor.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.SetYLabelVisible ------------------------------------
procedure TGLCoordinateAxes.SetYLabelVisible(bVisible:boolean);

var
  i:integer;

begin
  fYLabelVisible := bVisible;
  for i:=0 to fYLabelList.Count-1 do
    TGLFlatText(fYLabelList.Objects[i]).Visible := bVisible;
end;
// ----- TGLCoordinateAxes.SetZLabelColour ------------------------------------
procedure TGLCoordinateAxes.SetZLabelColour(aCol:TColor);

var
  i:integer;

begin
  fZLabelColour := aCol;
  for i:=0 to fZlabelList.Count-1 do
    TGLFlatText(fZLabelList.Objects[i]).ModulateColor.AsWinColor := aCol;
end;
// ----- TGLCoordinateAxes.SetZLabelVisible ------------------------------------
procedure TGLCoordinateAxes.SetZLabelVisible(bVisible:boolean);

var
  i:integer;

begin
  fZLabelVisible := bVisible;
  for i:=0 to fZLabelList.Count-1 do
    TGLFlatText(fZLabelList.Objects[i]).Visible := bVisible;
end;
// ----- TGLCoordinateAxes.SetXLength ------------------------------------------
procedure TGLCoordinateAxes.SetXLength(dLength:double);

begin
  fxLength := dLength;
  Render;
end;
// ----- TGLCoordinateAxes.SetXRadius ------------------------------------------
procedure TGLCoordinateAxes.SetXRadius(dRadius:double);

begin
  fXRadius := dRadius;
  with fXArrow do
  begin
    TopRadius := fXRadius;
    BottomRadius := fXRadius;
    BottomArrowHeadRadius := 2*fXRadius;
    TopArrowHeadRadius := 2*fxRadius;
    BottomArrowHeadHeight := 5*fXRadius;
    TopArrowHeadHeight := 5*fxRadius;
    GenerateXLabels;
  end;
end;
// ----- TGLCoordinateAxes.SetYLength ------------------------------------------
procedure TGLCoordinateAxes.SetYLength(dLength:double);

begin
  fYLength := dLength;
  Render;
end;
// ----- TGLCoordinateAxes.SetYRadius ------------------------------------------
procedure TGLCoordinateAxes.SetYRadius(dRadius:double);

begin
  fYRadius := dRadius;
  with fYArrow do
  begin
    TopRadius := fYRadius;
    BottomRadius := fYRadius;
    BottomArrowHeadRadius := 2*fYRadius;
    TopArrowHeadRadius := 2*fYRadius;
    BottomArrowHeadHeight := 5*fYRadius;
    TopArrowHeadHeight := 5*fYRadius;
    GenerateYLabels;
  end;
end;
// ----- TGLCoordinateAxes.SetZLength ------------------------------------------
procedure TGLCoordinateAxes.SetZLength(dLength:double);

begin
  fZLength := dLength;
  Render;
end;
// ----- TGLCoordinateAxes.SetZRadius ------------------------------------------
procedure TGLCoordinateAxes.SetZRadius(dRadius:double);

begin
  fZRadius := dRadius;
  with fZArrow do
  begin
    TopRadius := fzRadius;
    BottomRadius := fzRadius;
    BottomArrowHeadRadius := 2*fzRadius;
    TopArrowHeadRadius := 2*fzRadius;
    BottomArrowHeadHeight := 5*fzRadius;
    TopArrowHeadHeight := 5*fzRadius;
    GeneratezLabels;
  end;
end;
// ----- TGLCoordinateAxes.Initialise ------------------------------------------
// initialises some default properties - note this all gets set later...
procedure TGLCoordinateAxes.Initialise;

begin

// set arrows
  fXArrow.Visible := false;
  fXArrow.Material.FrontProperties.Emission.AsWinColor := clRed;
  fXLabelColour := clRed;
  
  fYArrow.Visible := false;
  fYArrow.Material.FrontProperties.Emission.AsWinColor := clGreen;
  fYLabelColour := clGreen;

  fZArrow.Visible := false;
  fZArrow.Material.FrontProperties.Emission.AsWinColor := clBlue;
  fZLabelColour := clRed;

// prep the XY grid!
  with fXYGrid do
  begin
    Visible := false;
    LineColor.Color := clrGray;
    XSamplingScale.Origin := 0;
    XSamplingScale.Min := -5;
    XSamplingScale.Max := 5;
    XSamplingScale.Step := 1;
    YSamplingScale.Origin := 0;
    YSamplingScale.Min := -5;
    YSamplingScale.Max := 5;
    YSamplingScale.Step := 1;
  end;
// prep the XY grid!
  with fYZGrid do
  begin
    Visible := false;
    LineColor.Color := clrGray;
    XSamplingScale.Origin := 0;
    XSamplingScale.Min := -5;
    XSamplingScale.Max := 5;
    XSamplingScale.Step := 1;
    YSamplingScale.Origin := 0;
    YSamplingScale.Min := -5;
    YSamplingScale.Max := 5;
    YSamplingScale.Step := 1;
  end;
// prep the XZ grid!
  with fXZGrid do
  begin
    Visible := false;
    LineColor.Color := clrGray;
    XSamplingScale.Origin := 0;
    XSamplingScale.Min := -5;
    XSamplingScale.Max := 5;
    XSamplingScale.Step := 1;
    YSamplingScale.Origin := 0;
    YSamplingScale.Min := -5;
    YSamplingScale.Max := 5;
    YSamplingScale.Step := 1;
  end;

  fParts := [apXN,apXP,apYN,apYP,apZN,apZP];
  fXLength := 10;
  fYLength := 10;
  fZLength := 10;

  fXLabelStart := -5;
  fXLabelStop := 5;
  fXLabelStep := 1;
  fXLabelVisible := false;

  fyLabelStart := -5;
  fyLabelStop := 5;
  fyLabelStep := 1;
  fYLabelVisible := false;

  fzLabelStart := -5;
  fzLabelStop := 5;
  fzLabelStep := 1;
  fZLabelVisible := false;
end;
// ----- TGLCoordinateAxes.UpdateXYGrid ----------------------------------------
procedure TGLCoordinateAxes.UpdateXYGrid;

begin
// adjust for scale-space
  with fXYGrid.XSamplingScale do
  begin
    Min := fxlabelstart*fScaleX - fDummyCube.Position.X;
    Max := fxLabelStop*fScaleX - fDummyCube.Position.X;
    Origin := Min;
    Step := fxLabelStep*fScaleX;
  end;
// adjust for scale-space
  with fXYGrid.YSamplingScale do
  begin
    Min := fyLabelStart*fScaleY - fDummyCube.Position.Y;
    Max := fYLabelStop*fScaleY - fDummyCube.Position.Y;
    Origin := Min;
    Step := fYLabelStep*fScaleY;
  end;
end;
// ----- TGLCoordinateAxes.UpdateYZGrid ----------------------------------------
procedure TGLCoordinateAxes.UpdateYZGrid;

begin
// adjust for scale-space
  with fYZGrid.YSamplingScale do
  begin
    Min := fYLabelStart*fScaleY - fDummyCube.Position.Y;
    Max := fYLabelStop*fScaleY - fDummyCube.Position.Y;
    Origin := Min;
    Step := fYLabelStep*fScaleY;
  end;
// adjust for scale-space
  with fYZGrid.ZSamplingScale do
  begin
    Min := fZLabelStart*fScaleZ - fDummyCube.Position.Z;
    Max := fZLabelStop*fScaleZ - fDummyCube.Position.Z;
    Origin := Min;
    Step := fZLabelStep*fScaleZ;
  end;
end;
// ----- TGLCoordinateAxes.UpdateXZGrid ----------------------------------------
procedure TGLCoordinateAxes.UpdateXZGrid;

begin
// adjust for scale-space
  with fXZGrid.XSamplingScale do
  begin
    Min := fXLabelStart*fScaleX - fDummyCube.Position.X;
    Max := fXLabelStop*fScaleX - fDummyCube.Position.X;
    Origin := Min;
    Step := fXLabelStep*fScaleX;
  end;
// adjust for scale-space
  with fXZGrid.ZSamplingScale do
  begin
    Min := fZLabelStart*fScaleZ - fDummyCube.Position.Z;
    Max := fZLabelStop*fscaleZ - fDummyCube.Position.Z;
    Origin := Min;
    Step := fZLabelStep*fScaleZ;
  end;
end;

// ----- TGLCoordinateAxes.Create ----------------------------------------------
constructor TGLCoordinateAxes.Create(aDummyCube:TGLDummyCube);

begin
  inherited Create;

  fDummyCube := aDummyCube;

  fXArrow := TGLArrowLine(aDummyCube.AddNewChild(TGLArrowLine));
  fXArrow.Direction.AsVector := XHmgVector;
  fXRadius := 0.1;

  fYArrow := TGLArrowLine(aDummyCube.AddNewChild(TGLArrowLine));
  fYArrow.Direction.AsVector := YHmgVector;
  fYRadius := 0.1;

  fZArrow := TGLArrowLine(aDummyCube.AddNewChild(TGLArrowLine));
  fZArrow.Direction.AsVector := ZHmgVector;
  fZArrow.Direction.Z := -fZArrow.Direction.Z;
  fZRadius := 0.1;

  fXYGrid := TGLXYZGrid(aDummyCube.AddNewChild(TGLXYZGrid));
  fXYGrid.Parts := [gpX,gpY];
  fXYGRid.Up.Y := -1;
  fXYGrid.Direction.Z := 1;

  fYZGrid := TGLXYZGrid(aDummyCube.AddNewChild(TGLXYZGrid));
  fYZGrid.Parts := [gpY,gpZ];
  fYZGRid.Up.Y := -1;
  fYZGrid.Direction.Z := 1;

  fXZGrid := TGLXYZGrid(aDummyCube.AddNewChild(TGLXYZGrid));
  fXZGrid.Parts := [gpX,gpZ];
  fXZGRid.Up.Y := -1;
  fXZGrid.Direction.Z := 1;

  fXLabelList := TStringList.Create;
  fYLabelList := TStringList.Create;
  fZLabelList := Tstringlist.create;

  fScaleX := 1;
  fScaleY := 1;
  fScaleZ := 1;
  
  Initialise;
  Render;
end;
// ----- TGLCoordinateAxes.Destroy ---------------------------------------------
destructor TGLCoordinateAxes.Destroy;

begin
  fXArrow.Free;
  fYArrow.Free;
  fZArrow.Free;

  ClearXLabels;
  fXLabelList.Free;

  ClearYLabels;
  fYLabelList.Free;

  ClearzLabels;
  fzLabelList.Free;

  fXYGrid.Free;
  fYZGrid.Free;
  fXZGrid.Free;
end;
// ----- TGLCoordinateAxes.GenerateXLabels -------------------------------------
procedure TGLCoordinateAxes.GenerateXLabels;

// note the dummy cube position is already in scale-space

var
  dX,dScale : double; // current X;
  ft : TGLFlatText;

begin
// 40% of increment?
  dScale := (fxLabelStep*fScaleX*0.4)/fLabelFont.Font.Size;
  ClearXLabels;

// offset relative to the X position of the parent dummy cube
  dx := fxLabelStart-fDummyCube.Position.X/fscalex;
  while dx<=(fxLabelStop-fDummyCube.Position.X/fscalex) do
  begin
    ft := TGLFlatText(fDummyCube.AddNewChild(TGLFlatText));
    with ft do
    begin
      BitMapfont := fLabelfont;
      Alignment := taLeftJustify;
      Direction.AsVector := zHmgVector;
      Up.AsVector := XHmgVector;
      Up.X := -1;
      Layout := tlCenter;
      Scale.X := dScale;
      Scale.Y := dScale;
      Scale.Z := dScale;
      Options := Options + [ftoTwoSided];
      ModulateColor.AswinColor := fXLabelColour;
      Position.X := -dx*fScaleX;
      Position.Y := 2.5*fXRadius;
      Position.Z := 0;
      Visible := fXLabelVisible;
      Text := FloatToStr(dx+fDummyCube.Position.X/fScaleX);
    end;
    fXLabelList.AddObject(FloatToStr(dx),ft);
    dx := dx + fxLabelStep;
  end;
  UpdateXYGrid;
  UpdateXZGrid;
end;
  // ----- TGLCoordinateAxes.GenerateYLabels -----------------------------------
procedure TGLCoordinateAxes.GenerateYLabels;

var
  dY,dScale : double; // current Y;
  ft : TGLFlatText;

begin
// 40% of increment?
  dScale := (fYLabelStep*fScaleY*0.4)/fLabelFont.Font.Size;
  ClearYLabels;

// offset is relative to the Y position of the parent dummy cube
  dy := fyLabelStart-fDummyCube.Position.Y/fscaley;
  while dy<=(fyLabelStop-fDummyCube.Position.Y/fscaley) do
  begin
    ft := TGLFlatText(fDummyCube.AddNewChild(TGLFlatText));
    with ft do
    begin
      BitMapfont := fLabelfont;
      Alignment := taRightJustify;
      Direction.AsVector := zHmgVector;
      Up.AsVector := yHmgVector;
      Up.Y := -1;
      Layout := tlCenter;
      Scale.X := dScale;
      Scale.Y := dScale;
      Scale.Z := dScale;
      Options := Options + [ftoTwoSided];
      ModulateColor.AsWinColor := fYLabelColour;
      Position.X := 2.5*fYRadius;
      Position.Y := -dy*fScaleY; // dy has the cube position offset already.
      Position.Z := 0;
      Visible := fYLabelVisible;
      Text := FloatToStr(dy+fDummyCube.Position.Y/fscaleY);
    end;
    fyLabelList.AddObject(FloatToStr(dy),ft);
    dy := dy + fyLabelStep;
  end;
  UpdateXYGrid;
  UpdateYZGrid;
end;
// ----- TGLCoordinateAxes.GeneratezLabels -------------------------------------
procedure TGLCoordinateAxes.GenerateZLabels;

var
  dz,dScale : double; // current Y;
  ft : TGLFlatText;

begin
// 40% of increment?
  dScale := (fzLabelStep*fScalez*0.4)/fLabelFont.Font.Size;
  ClearzLabels;
  dz := fzLabelStart;

  while dz<=(fzLabelStop-fDummyCube.Position.Z/fscalez) do
  begin
    ft := TGLFlatText(fDummyCube.AddNewChild(TGLFlatText));
    with ft do
    begin
      BitMapfont := fLabelfont;
      Alignment := taRightJustify;
      Direction.X := 0;
      Direction.Y := 1;
      Direction.Z := 0;
      Up.AsVector := zHmgVector;
      Layout := tlCenter;
      Scale.X := dScale;
      Scale.Y := dScale;
      Scale.Z := dScale;
      Options := Options + [ftoTwoSided];
      ModulateColor.AsWinColor := fZLabelColour;
      Position.X := 2.5*fZRadius;
      Position.Y := 0;
      Position.Z := dz*fScaleZ;
      Visible := fZLabelVisible;
      Text := FloatToStr(dz+fDummyCube.Position.Z/fScaleZ);
    end;
    fzLabelList.AddObject(FloatToStr(dz),ft);
    dz := dz + fzLabelStep;
  end;
  UpdateYZGrid;
  UpdateXZGrid;
end;
// ----- TGLCoordinateAxes.ShowAxes --------------------------------------------
procedure TGLCoordinateAxes.ShowAxes(bshow:boolean);

begin
  fXArrow.Visible := bShow;
  fYArrow.Visible := bShow;
  fZArrow.Visible := bShow;
end;
// ----- TGLCoordinateAxes.IsVisible -------------------------------------------
function TGLCoordinateAxes.IsVisible:boolean;

begin
  result := fXArrow.Visible;
end;
// =============================================================================
end.
