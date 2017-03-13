object Form1: TForm1
  Left = 227
  Top = 110
  Caption = 'Form1'
  ClientHeight = 291
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 56
    Top = 8
    Width = 473
    Height = 257
    Camera = GLCamera1
    FieldOfView = 137.477478027343800000
    TabOrder = 0
  end
  object Button1: TButton
    Left = 456
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000000000C0400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLLines1: TGLLines
      Nodes = <
        item
          X = -1.000000000000000000
        end
        item
          Y = 2.000000000000000000
        end
        item
          X = 1.000000000000000000
          Y = -1.000000000000000000
        end
        item
          X = 2.000000000000000000
          Y = 1.200000047683716000
        end>
      Options = []
    end
  end
end
