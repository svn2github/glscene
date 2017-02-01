object Progress: TProgress
  Left = 516
  Top = 444
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Op'#233'rations en cours...'
  ClientHeight = 68
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LMessage: TLabel
    Left = 8
    Top = 8
    Width = 3
    Height = 13
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 32
    Width = 273
    Height = 25
    Position = 50
    Smooth = True
    TabOrder = 0
  end
end
