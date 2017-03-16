unit frmBlock;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  //GLData
  geFloatEdit;

type
  TformBlock = class(TForm)
    lebName: TLabeledEdit;
    lebLayer: TLabeledEdit;
    pnlBottom: TPanel;
    bCancel: TBitBtn;
    bOK: TBitBtn;
    lebNX: TLabeledEdit;
    lebNY: TLabeledEdit;
    cbxActiveBlock: TCheckBox;
    lblPorosity: TLabel;
    lblPermeabilityX: TLabel;
    lblPermeabilityY: TLabel;
    lblPermeabilityZ: TLabel;

    gePorosity: TGEFloatEdit;
    gePermX: TGEFloatEdit;
    gePermY: TGEFloatEdit;
    gePermZ: TGEFloatEdit;
    lebPressure: TLabeledEdit;
    lebTemperature: TLabeledEdit;
    lebSaturation: TLabeledEdit;
  private
     
  public
     
  end;

implementation

{$R *.dfm}

end.
