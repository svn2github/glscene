object MaterialCodeForm: TMaterialCodeForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Material GLSL Code'
  ClientHeight = 200
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object CodePad: TMemo
    Left = 0
    Top = 0
    Width = 640
    Height = 200
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
