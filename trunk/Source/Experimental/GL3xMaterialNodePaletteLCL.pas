//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialNodePaletteLCL<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialNodePaletteLCL;

{$mode DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, GLExpandPanels, LMessages,
  GL3xMaterialGraph;

type

  { TNodePaletteForm }

  TNodePaletteForm = class(TForm)
    ConstantRollOut: TMyRollOut;
    VectorsRollOut: TMyRollOut;
    CoordinateRollOut: TMyRollOut;
    MathRollOut: TMyRollOut;
    TextureRollOut: TMyRollOut;
    UtilityRollOut: TMyRollOut;

    ConstantPanel: TPanel;
    ConstantVector2Panel: TPanel;
    ConstantVector3Panel: TPanel;
    ConstantVector4Panel: TPanel;
    VertexColorPanel: TPanel;

    WorldPositionPanel: TPanel;
    WorldNormalPanel: TPanel;
    WorldCameraPositionPanel: TPanel;
    TextureCoordinatePanel: TPanel;
    PannerPanel: TPanel;
    RotatorPanel: TPanel;
    ScreenPositionPanel: TPanel;

    AddPanel: TPanel;
    SubtractPanel: TPanel;
    MultiplyPanel: TPanel;
    DividePanel: TPanel;
    NormalizePanel: TPanel;
    PowerPanel: TPanel;
    DotProductPanel: TPanel;
    SinePanel: TPanel;
    CosinePanel: TPanel;
    FloorPanel: TPanel;
    AbsPanel: TPanel;
    OneMinusPanel: TPanel;
    SquareRootPanel: TPanel;

    WorldNormalPanel2: TPanel;
    LightVectorPanel: TPanel;
    CameraVectorPanel: TPanel;
    ReflectionVectorPanel: TPanel;

    TextureSamplerPanel: TPanel;

    TimerPanel: TPanel;
    ComponentMaskPanel: TPanel;

    ScrollBox1: TScrollBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure PanelStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  protected
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
  private
    { Private declarations }
    LastPanel: TPanel;
  public
    { Public declarations }
    NotifyClose: TNotifyEvent;
    Docked: boolean;
    LastChangePosByMainForm: boolean;
    NodeClass: TCustomMaterialGraphNodeClass;
  end;

var
  NodePaletteForm: TNodePaletteForm;

implementation

{$R *.lfm}

const
  cNodeClasses: array[0..32] of TCustomMaterialGraphNodeClass = (
    TCustomMaterialGraphNode,
    TConstantNode,
    TConstant2fNode,
    TConstant3fNode,
    TConstant4fNode,
    TVertexColorNode,

    TCustomMaterialGraphNode,
    TCustomMaterialGraphNode,
    TCustomMaterialGraphNode,
    TTextureCoordinateNode,
    TPannerNode,
    TRotatorNode,
    TScreenPositionNode,

    TAddNode,
    TSubtractNode,
    TMultiplyNode,
    TDivideNode,
    TNormalizeNode,
    TPowerNode,
    TDotProductNode,
    TSineNode,
    TCosineNode,
    TFloorNode,
    TAbsNode,
    TOneMinusNode,
    TSquareRootNode,

    TWorldNormalNode,
    TLightVectorNode,
    TCameraVectorNode,
    TReflectionVectorNode,

    TTextureSamplerNode,

    TTimerNode,
    TComponentMaskNode);


procedure TNodePaletteForm.FormCreate(Sender: TObject);
begin
  ConstantPanel.Tag := 1;
  ConstantVector2Panel.Tag := 2;
  ConstantVector3Panel.Tag := 3;
  ConstantVector4Panel.Tag := 4;
  VertexColorPanel.Tag := 5;

  WorldPositionPanel.Tag := 6;
  WorldNormalPanel.Tag := 7;
  WorldCameraPositionPanel.Tag := 8;
  TextureCoordinatePanel.Tag := 9;
  PannerPanel.Tag := 10;
  RotatorPanel.Tag := 11;
  ScreenPositionPanel.Tag := 12;

  AddPanel.Tag := 13;
  SubtractPanel.Tag := 14;
  MultiplyPanel.Tag := 15;
  DividePanel.Tag := 16;
  NormalizePanel.Tag := 17;
  PowerPanel.Tag := 18;
  DotProductPanel.Tag := 19;
  SinePanel.Tag := 20;
  CosinePanel.Tag := 21;
  FloorPanel.Tag := 22;
  AbsPanel.Tag := 23;
  OneMinusPanel.Tag := 24;
  SquareRootPanel.Tag := 25;

  WorldNormalPanel2.Tag := 26;
  LightVectorPanel.Tag := 27;
  CameraVectorPanel.Tag := 28;
  ReflectionVectorPanel.Tag := 29;

  TextureSamplerPanel.Tag := 30;

  TimerPanel.Tag := 31;
  ComponentMaskPanel.Tag := 32;
end;

procedure TNodePaletteForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  NotifyClose(Self);
end;

procedure TNodePaletteForm.FormMouseLeave(Sender: TObject);
begin
  if Assigned(LastPanel) then
  begin
    LastPanel.Color := clCream;
    LastPanel := nil;
  end;
end;

procedure TNodePaletteForm.PanelStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  DragedPanel: TPanel;
begin
  if Sender is TPanel then
  begin
    DragedPanel := TPanel(Sender);
    NodeClass := cNodeClasses[DragedPanel.Tag];
  end
  else
    NodeClass := TCustomMaterialGraphNode;
end;

procedure TNodePaletteForm.PanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if Active and (LastPanel <> Sender) then
  begin
    if Assigned(LastPanel) then
      LastPanel.Color := clCream;
    LastPanel := TPanel(Sender);
    LastPanel.Color := $0010AFEF;
  end;
end;

procedure TNodePaletteForm.WMMove(var Message: TLMMove);
begin
  if LastChangePosByMainForm then
    LastChangePosByMainForm := False
  else
    Docked := False;
  inherited;
end;

end.

