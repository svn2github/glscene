//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialNodePalette<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialNodePalette;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, ButtonGroup, ExtCtrls, ActnList,
  GLSGraphStructure, GL3xMaterialGraph;

type
  TNodePaletteForm = class(TForm)
    CategoryPanelGroup1: TCategoryPanelGroup;
    ConstantsPanel: TCategoryPanel;
    CoordinatesPanel: TCategoryPanel;
    TexturePanel: TCategoryPanel;
    ConstantGroup: TButtonGroup;
    CoordinateGroup: TButtonGroup;
    TextureGroup: TButtonGroup;
    MathPanel: TCategoryPanel;
    MathGroup: TButtonGroup;
    UtilityPanel: TCategoryPanel;
    UtilityGroup: TButtonGroup;
    VectorsPanel: TCategoryPanel;
    VectorsGroup: TButtonGroup;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CommonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

  public
    { Public declarations }
    NotifyClose: TNotifyEvent;
    Docked: Boolean;
    LastChangePosByMainForm: Boolean;
    NodeClass: TCustomMaterialGraphNodeClass;
  end;

implementation

{$R *.dfm}

const
  cConstantClasses: array[0..4] of TCustomMaterialGraphNodeClass =
    (TConstantNode, TConstant2fNode, TConstant3fNode, TConstant4fNode,
     TVertexColorNode);
  cCoordinateClasses: array[0..7] of TCustomMaterialGraphNodeClass =
    (TObjectPositionNode, TWorldPositionNode, TWorldNormalNode,
     TCameraWorldPositionNode,
     TTextureCoordinateNode, TPannerNode, TRotatorNode, TScreenPositionNode);
  cMathClasses: array[0..15] of TCustomMaterialGraphNodeClass =
    (TAddNode, TSubtractNode, TMultiplyNode, TDivideNode,
     TNormalizeNode, TPowerNode, TDotProductNode, TSineNode, TCosineNode,
     TFloorNode, TAbsNode, TFractNode, TOneMinusNode, TSquareRootNode, TSignNode,
     TSmoothStepNode);
  cTextureClasses: array[0..1] of TCustomMaterialGraphNodeClass =
    (TTextureSamplerNode, TCustomMaterialGraphNode);
  cUtilityClasses: array[0..4] of TCustomMaterialGraphNodeClass =
    (TTimerNode, TComponentMaskNode, TClampNode, TConstClampNode, TAppendVectorNode);
  cVectorsClasses: array[0..3] of TCustomMaterialGraphNodeClass =
    (TWorldNormalNode, TLightVectorNode, TCameraVectorNode, TReflectionVectorNode);

procedure TNodePaletteForm.FormCreate(Sender: TObject);
begin
  NodeClass := TCustomMaterialGraphNode;
end;

procedure TNodePaletteForm.CommonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BG: TButtonGroup;
  I: Integer;
begin
  NodeClass := TCustomMaterialGraphNode;
  BG := TButtonGroup(Sender);
  I := BG.IndexOfButtonAt(X, Y);
  if I<0 then
    exit;
  if BG = ConstantGroup then
    NodeClass := cConstantClasses[I]
  else if BG = CoordinateGroup then
    NodeClass := cCoordinateClasses[I]
  else if BG = MathGroup then
    NodeClass := cMathClasses[I]
  else if BG = TextureGroup then
    NodeClass := cTextureClasses[I]
  else if BG = UtilityGroup then
    NodeClass := cUtilityClasses[I]
  else if BG = VectorsGroup then
    NodeClass := cVectorsClasses[I]
end;

procedure TNodePaletteForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NotifyClose(Self);
end;

procedure TNodePaletteForm.WMMove(var Message: TWMMove);
begin
  if LastChangePosByMainForm then
    LastChangePosByMainForm := False
  else
    Docked := False;
  inherited;
end;

end.
