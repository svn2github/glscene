object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Points'
  ClientHeight = 551
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 31
    Width = 649
    Height = 520
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 158.228942871093800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 649
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 354
      Top = 7
      Width = 22
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'FPS'
    end
    object CBPointParams: TCheckBox
      Left = 10
      Top = 5
      Width = 119
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'PointParameters'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBPointParamsClick
    end
    object CBAnimate: TCheckBox
      Left = 217
      Top = 5
      Width = 119
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
        Size = 10.000000000000000000
        Style = psSmoothAdditive
        PointParameters.Enabled = True
        PointParameters.DistanceAttenuation.Coordinates = {0000C0BF3333333F0000000000000000}
      end
      object GLPoints2: TGLPoints
        NoZWrite = True
        Static = False
        Size = 20.000000000000000000
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
