object Form1: TForm1
  Left = 203
  Top = 102
  Width = 219
  Height = 369
  BorderWidth = 5
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 201
    Height = 313
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 313
    Width = 201
    Height = 17
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = '???.? FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      TargetObject = DummyCube1
      Position.Coordinates = {0000A04100002041000020410000803F}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 40
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 72
  end
end
