object Form1: TForm1
  Left = 239
  Top = 88
  Caption = 'Form1'
  ClientHeight = 367
  ClientWidth = 410
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
    Width = 410
    Height = 342
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 147.402404785156300000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 410
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
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 40
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLPoints1: TGLPoints
        Static = False
        NoZWrite = True
        Size = 10.000000000000000000
        Style = psSmoothAdditive
      end
      object GLPoints2: TGLPoints
        Static = False
        NoZWrite = True
        Size = 20.000000000000000000
        Style = psSmoothAdditive
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
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end>
    Left = 24
    Top = 136
  end
end
