// FDXPOptions
{
   General DXP options.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit FDXPOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TDXPOptions = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TSFreePascal: TTabSheet;
    BUOk: TButton;
    BUCancel: TButton;
    Label1: TLabel;
    EDFPCBinary: TEdit;
    BUFPCBinary: TButton;
    Label2: TLabel;
    EDFPCSourcePaths: TEdit;
    Button1: TButton;
    procedure BUFPCBinaryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute : Boolean;
  end;

implementation

{$R *.dfm}

uses DXPGlobals, FDXPDirectoryDialog, FDXPPathsDialog;

// Execute
//
function TDXPOptions.Execute : Boolean;
begin
   // prepare the dialog

   // FPC
   EDFPCBinary.Text:=vFPC_BinaryPath;
   EDFPCSourcePaths.Text:=vFPC_SourcePaths;

   Result:=(ShowModal=mrOk);

   if Result then begin
      // store values

      // FPC
      vFPC_BinaryPath:=EDFPCBinary.Text;
      vFPC_SourcePaths:=EDFPCSourcePaths.Text;
   end;
end;

procedure TDXPOptions.BUFPCBinaryClick(Sender: TObject);
begin
   DXPDirectoryDialog(EDFPCBinary);
end;

procedure TDXPOptions.Button1Click(Sender: TObject);
var
   buf : String;
begin
   buf:=EDFPCSourcePaths.Text;
   if DXPPathsDialog(buf) then
      EDFPCSourcePaths.Text:=buf;
end;

end.
