object Form1: TForm1
  Left = 221
  Top = 109
  Width = 355
  Height = 381
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Generate Anim Frames'
    TabOrder = 0
    OnClick = Button1Click
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 48
    Width = 329
    Height = 297
    Camera = GLCamera1
  end
  object CBAnimate: TCheckBox
    Left = 200
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Animate'
    Enabled = False
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 56
    object Cube1: TGLCube
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00007041000020410000E0400000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 120
      TargetObject = Cube1
      Position.Coordinates = {0000A04000008040000040400000803F}
      Left = 208
      Top = 160
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 48
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 88
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 136
  end
end
