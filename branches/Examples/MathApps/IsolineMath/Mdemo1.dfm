object DemoForm: TDemoForm
  Left = -4
  Top = -4
  Caption = 'Isolines'
  ClientHeight = 541
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object TabSet1: TTabSet
    Left = 0
    Top = 0
    Width = 792
    Height = 17
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Tabs.Strings = (
      'Contours')
    TabIndex = 0
  end
end
