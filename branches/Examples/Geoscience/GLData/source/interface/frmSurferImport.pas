unit frmSurferImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, geFloatEdit;

type
  TformSurferImport = class(TForm)
    pnlGridBounds: TPanel;
    lblXField: TLabel;
    lblYField: TLabel;
    lblZField: TLabel;
    lblMinimum: TLabel;
    ebMinX: TEdit;
    ebMinY: TEdit;
    ebMinZ: TEdit;
    lblMaximum: TLabel;
    ebMaxX: TEdit;
    ebMaxY: TEdit;
    ebMaxZ: TEdit;
    lblSpacing: TLabel;
    ebSpacingX: TEdit;
    ebSpacingY: TEdit;
    ebTotalNo: TEdit;
    ebNoY: TEdit;
    ebNoX: TEdit;
    lblNoNodes: TLabel;
    lblTotalNodes: TLabel;
    pnlBottom: TPanel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    pnlBlankedNodes: TPanel;
    lblBlankedNodes: TLabel;
    lblBlankedValue: TLabel;
    ebNoBlanks: TEdit;
    pnlGridName: TPanel;
    lblGridName: TLabel;
    ebGridName: TEdit;
    geBlankedValue: TGEFloatEdit;
  private

  public

  end;

implementation

{$R *.dfm}
// =============================================================================
end.
