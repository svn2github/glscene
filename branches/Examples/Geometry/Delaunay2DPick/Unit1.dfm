object Form1: TForm1
  Left = 217
  Top = 172
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 668
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 24
    Width = 129
    Height = 121
    Visible = False
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 72
  end
end
