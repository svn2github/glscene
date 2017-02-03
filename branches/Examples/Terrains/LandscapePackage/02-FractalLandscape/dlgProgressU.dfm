object dlgProgress: TdlgProgress
  Left = 698
  Top = 678
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 63
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object ggTaskProgress: TGauge
    Left = 8
    Top = 32
    Width = 305
    Height = 20
    Progress = 0
  end
  object lblTask: TLabel
    Left = 8
    Top = 8
    Width = 305
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object timTask: TTimer
    Enabled = False
    OnTimer = timTaskTimer
    Left = 8
  end
end
