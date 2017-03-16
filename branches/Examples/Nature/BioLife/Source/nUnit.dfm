object Form1: TForm1
  Left = 191
  Top = 117
  Width = 544
  Height = 375
  Caption = 'Form1'
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
    Left = 129
    Top = 0
    Width = 407
    Height = 348
    Camera = GLCamera1
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 348
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 144
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000204100002041000020410000803F}
      Left = 256
      Top = 160
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
      end
    end
  end
end
