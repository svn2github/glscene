unit GLSGraphStructure;

interface

uses
  Classes,
  BaseClasses,
  VectorTypes,
  VectorGeometry;

type
  TBaseGraphStructure = class;

  TBaseGraphVertex = class(TGLUpdateAbleObject)
  private
    { Private declarations }
    FDeletable: Boolean;
  protected
    { Protected declarations }
    FRect: TRectangle;
    FHighlight: Boolean;
    FInScreen: Boolean;
    FScaleOffset: TVector4f; // Shader variable
    FChanged: Boolean;
    function GetLeft: Integer; virtual;
    function GetTop: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    procedure SetWidth(Value: Integer); virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetLeft(Value: Integer); virtual;
    procedure SetTop(Value: Integer); virtual;

    function GetCount: Integer; virtual; abstract;
    procedure SetLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer); virtual; abstract;
    function IsJointCanLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean; virtual; abstract;
    function IsStartLinkNode(jointIndex: Integer): Boolean; virtual; abstract;
    function IsJointLinkedWith(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean; virtual; abstract;
  public
    { Public declarations }
    procedure NotifyChange(Sender: TObject); override;

    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Count: Integer read GetCount;
    property Deletable: Boolean read FDeletable write FDeletable;
  end;

  TBaseGraphVertexClass = class of TBaseGraphVertex;

  TBaseGraphStructure = class(TGLUpdateAbleComponent)
  protected
    { Protected declarations }
    FScreenCenter: TVector2s;
    FScreenSize: TVector2s;
    FCursorCoords: TVector2s;
    FScreenZoom: Single;
    FNodeList: TList;
    FNeedPackList: Boolean;
    FPulling: Boolean;
    FSelectedJointNode1: TBaseGraphVertex;
    FSelectedJointIndex1: Integer;
    FSelectedJointNode2: TBaseGraphVertex;
    FSelectedJointIndex2: Integer;
    FChanged: Boolean;

    procedure DeleteNode(const i: Integer);
    function GetNode(const i: Integer): TBaseGraphVertex; overload;
    procedure SetPullingMode(Value: Boolean); inline;
    function AddNode(const VertexClass: TBaseGraphVertexClass;
      const x, y, w, h: Integer): TBaseGraphVertex; overload;

    procedure Save; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Render; virtual; abstract;
  public
    procedure NotifyChange(Sender: TObject); override;

    procedure PackLists;
    procedure EraseNode(Value: TBaseGraphVertex);
    procedure DeleteSelected;
    procedure CreateLink;
    procedure BreakLink;
    procedure BreakAllLinks;

    function Pick(const GroupSelection: Boolean): TPersistent;
    procedure MoveSelectedNodes(const Offset: TVector2s);
    procedure SetCursorCoords(const Position: TVector2s); inline;

    function GetSelectedNodeNumber: Integer;
    function IsCursorOverJoint: Boolean;
    function GetScreenSpaceCoords(const Position: TVector2s): TVector2f;
    property PullingMode: Boolean read FPulling write SetPullingMode;
    property ScreenCenter: TVector2s read FScreenCenter write FScreenCenter;
    property ScreenSize: TVector2s read FScreenSize write FScreenSize;
    property ScreenZoom: Single read FScreenZoom write FScreenZoom;
  end;

implementation

procedure TBaseGraphStructure.NotifyChange(Sender: TObject);
begin
  FChanged := True;
  inherited;
end;

function TBaseGraphStructure.AddNode(const VertexClass: TBaseGraphVertexClass;
  const x, y, w, h: Integer): TBaseGraphVertex;
begin
  Result := VertexClass.Create(Self);
  PackLists;
  FNodeList.Add(Result);
  Result.Left := x;
  Result.Top := y;
  Result.Width := w;
  Result.Height := h;
end;

procedure TBaseGraphStructure.PackLists;
begin
  if FNeedPackList then
  begin
    FNodeList.Pack;
    FNeedPackList := false;
  end;
end;

procedure TBaseGraphStructure.SetPullingMode(Value: Boolean);
begin
  FPulling := Value;
end;

procedure TBaseGraphStructure.CreateLink;
var
  GiverNode, TakerNode: TBaseGraphVertex;
  GiverIndex, TakerIndex: Integer;
begin
  if Assigned(FSelectedJointNode1)
    and Assigned(FSelectedJointNode2)
    and (FSelectedJointNode1 <> FSelectedJointNode2)
    and FSelectedJointNode1.IsJointCanLink(
      FSelectedJointIndex1, FSelectedJointNode2, FSelectedJointIndex2) then
  begin
    if FSelectedJointNode1.IsStartLinkNode(FSelectedJointIndex1) then
    begin
      GiverNode := FSelectedJointNode1;
      TakerNode := FSelectedJointNode2;
      GiverIndex := FSelectedJointIndex1;
      TakerIndex := FSelectedJointIndex2;
    end
    else
    begin
      GiverNode := FSelectedJointNode2;
      TakerNode := FSelectedJointNode1;
      GiverIndex := FSelectedJointIndex2;
      TakerIndex := FSelectedJointIndex1;
    end;
    TakerNode.SetLink(TakerIndex, GiverNode, GiverIndex);
  end;
end;

procedure TBaseGraphStructure.BreakLink;
var
  vNode: TBaseGraphVertex;
  i, j: Integer;
begin
  if Assigned(FSelectedJointNode1) then
  begin
    if FSelectedJointNode1.IsStartLinkNode(FSelectedJointIndex1) then
    begin
      // start link joints have more than one outcoming link
      PackLists;
      for I := 0 to FNodeList.Count - 1 do
      begin
        vNode := GetNode(I);
        for j := 0 to vNode.Count - 1 do
          if vNode.IsJointLinkedWith(J, FSelectedJointNode1, FSelectedJointIndex1) then
            vNode.SetLink(J, nil, 0);
      end;
    end
    else
    begin
      // end link joints have one incoming link
      FSelectedJointNode1.SetLink(FSelectedJointIndex1, nil, 0);
    end;
  end;
end;

procedure TBaseGraphStructure.BreakAllLinks;
var
  vNode: TBaseGraphVertex;
  i, j: Integer;
begin
  PackLists;
  for I := 0 to FNodeList.Count - 1 do
  begin
    vNode := GetNode(I);
    if vNode.FHighlight then
    begin
      FSelectedJointNode1 := vNode;
      for j := 0 to vNode.Count - 1 do
      begin
        FSelectedJointIndex1 := j;
        BreakLink;
      end;
    end;
  end;
end;

procedure TBaseGraphStructure.DeleteNode(const i: Integer);
begin
  if GetNode(I) <> nil then
  begin
    GetNode(I).Destroy;
    FNodeList[i] := nil;
    FNeedPackList := True;
  end;
end;

procedure TBaseGraphStructure.EraseNode(Value: TBaseGraphVertex);
var
  i: Integer;
begin
  i := FNodeList.IndexOf(Value);
  if i > -1 then
  begin
    FNodeList.Items[i] := nil;
    FNeedPackList := true;
  end;
end;

procedure TBaseGraphStructure.DeleteSelected;
var
  vNode: TBaseGraphVertex;
  i: Integer;
begin
  BreakAllLinks;
  for I := 0 to FNodeList.Count - 1 do
  begin
    vNode := GetNode(I);
    if vNode.FHighlight and vNode.Deletable then
      DeleteNode(I);
  end;
end;

function TBaseGraphStructure.Pick(const GroupSelection:
  Boolean): TPersistent;
var
  vNode: TBaseGraphVertex;
  n: Integer;
begin
  PackLists;
  if not GroupSelection then
    for n := 0 to FNodeList.Count - 1 do
    begin
      vNode := GetNode(n);
      vNode.FHighlight := false;
    end;

  Result := nil;
  for n := 0 to FNodeList.Count - 1 do
  begin
    vNode := GetNode(n);
    if ((FCursorCoords[0] >= vNode.Left)
      and (FCursorCoords[0] <= vNode.Left + vNode.Width)
      and (FCursorCoords[1] >= vNode.Top)
      and (FCursorCoords[1] <= vNode.Top + vNode.Height)) then
    begin
      vNode.FHighlight := not vNode.FHighlight;
      if vNode.FHighlight then
        exit(vNode);
    end;
  end;
end;

procedure TBaseGraphStructure.MoveSelectedNodes(const Offset: TVector2s);
var
  vNode: TBaseGraphVertex;
  n: Integer;
begin
  PackLists;
  for n := 0 to FNodeList.Count - 1 do
  begin
    vNode := GetNode(n);
    if vNode.FHighlight then
    begin
      vNode.Left := vNode.Left - Offset[0];
      vNode.Top := vNode.Top - Offset[1];
    end;
  end;
end;

function TBaseGraphStructure.GetSelectedNodeNumber: Integer;
var
  n: Integer;
begin
  Result := 0;
  PackLists;
  for n := 0 to FNodeList.Count - 1 do
    if GetNode(n).FHighlight then
      Inc(Result);
end;

procedure TBaseGraphStructure.SetCursorCoords(const Position: TVector2s);
begin
  FCursorCoords := Position;
end;

function TBaseGraphStructure.GetNode(const i: Integer): TBaseGraphVertex;
begin
  if i < FNodeList.Count then
  begin
    Result := TBaseGraphVertex(FNodeList.Items[i]);
  end
  else
    Result := nil;
end;

function TBaseGraphStructure.IsCursorOverJoint: Boolean;
begin
  Result := Assigned(FSelectedJointNode1);
end;

function TBaseGraphStructure.GetScreenSpaceCoords(const Position: TVector2s):
  TVector2f;
begin
  Result[0] := 2 * (Position[0] - FScreenCenter[0]) / FScreenSize[0] /
    FScreenZoom;
  Result[1] := 2 * (Position[1] - FScreenCenter[1]) / FScreenSize[1] /
    FScreenZoom;
end;

//////////////////////////////////////////////////////////////////////////////////

function TBaseGraphVertex.GetLeft: Integer;
begin
  Result := FRect.Left;
end;

function TBaseGraphVertex.GetTop: Integer;
begin
  Result := FRect.Top;
end;

function TBaseGraphVertex.GetWidth: Integer;
begin
  Result := FRect.Width;
end;

function TBaseGraphVertex.GetHeight: Integer;
begin
  Result := FRect.Height;
end;

procedure TBaseGraphVertex.SetWidth(Value: Integer);
begin
  FRect.Width := Value;
end;

procedure TBaseGraphVertex.SetHeight(Value: Integer);
begin
  FRect.Height := Value;
end;

procedure TBaseGraphVertex.SetLeft(Value: Integer);
begin
  FRect.Left := Value;
end;

procedure TBaseGraphVertex.SetTop(Value: Integer);
begin
  FRect.Top := Value;
end;

procedure TBaseGraphVertex.NotifyChange;
begin
  inherited;
  FChanged := True;
end;

end.

