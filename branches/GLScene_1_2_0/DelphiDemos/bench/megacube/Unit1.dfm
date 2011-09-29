object Form1: TForm1
  Left = 198
  Top = 107
  Caption = 'Form1'
  ClientHeight = 264
  ClientWidth = 328
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
    Width = 328
    Height = 264
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 138.507843017578100000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object DummyCube1: TGLDummyCube
      ObjectsSorting = osNone
      CubeSize = 10.000000000000000000
      VisibleAtRunTime = True
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000C842000048420000C8420000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000C8410000C8420000803F}
    end
  end
  object Timer1: TTimer
    Interval = 4000
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
