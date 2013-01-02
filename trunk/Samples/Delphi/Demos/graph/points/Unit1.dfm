object Form1: TForm1
  Left = 239
  Top = 88
  Width = 418
  Height = 394
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
    Top = 25
    Width = 402
    Height = 331
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 146.379318237304700000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 25
    Align = alTop
    TabOrder = 1
    object CBPointParams: TCheckBox
      Left = 8
      Top = 4
      Width = 97
      Height = 17
      Caption = 'PointParameters'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBPointParamsClick
    end
    object CBAnimate: TCheckBox
      Left = 176
      Top = 4
      Width = 97
      Height = 17
      Caption = 'Animate'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBAnimateClick
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 40
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLPoints1: TGLPoints
        NoZWrite = True
        Static = False
        size = 10.000000000000000000
        Style = psSmoothAdditive
        PointParameters.Enabled = True
        PointParameters.DistanceAttenuation.Coordinates = {0000C0BF3333333F0000000000000000}
      end
      object GLPoints2: TGLPoints
        NoZWrite = True
        Static = False
        size = 20.000000000000000000
        Style = psSmoothAdditive
        PointParameters.Enabled = True
        PointParameters.DistanceAttenuation.Coordinates = {0000C0BF3333333F0000000000000000}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000A04000008040000040400000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 80
  end
end
