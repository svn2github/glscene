object frmGameHistory: TfrmGameHistory
  Left = 556
  Top = 104
  Caption = 'Game Actions'
  ClientHeight = 530
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    249
    530)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Moves'
  end
  object ListBox_Actions: TListBox
    Left = 8
    Top = 24
    Width = 242
    Height = 513
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    ExplicitWidth = 212
  end
end
