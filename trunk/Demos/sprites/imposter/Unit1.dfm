object Form1: TForm1
  Left = 179
  Top = 113
  Width = 463
  Height = 226
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
    Width = 328
    Height = 197
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 328
    Top = 0
    Width = 127
    Height = 197
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 8
      Top = 80
      Width = 58
      Height = 13
      Caption = 'Sample Size'
    end
    object CBShowTeapot: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Show Teapot'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBShowTeapotClick
    end
    object CBShowImposter: TCheckBox
      Left = 8
      Top = 52
      Width = 97
      Height = 17
      Caption = 'Show Imposter'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBShowImposterClick
    end
    object CBSampleSize: TComboBox
      Left = 72
      Top = 76
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 2
      Text = '64'
      OnChange = CBSampleSizeChange
      Items.Strings = (
        '8'
        '16'
        '32'
        '64'
        '128'
        '256')
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLSkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15
        end
        item
          StartAngle = 15
          StopAngle = 90
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end
        item
          StartAngle = -90
          StartColor.Color = {0000000000000000000000000000803F}
          StopColor.Color = {0000803F0000803F0000803F0000803F}
        end>
      Stars = <>
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
    end
    object GLTeapot1: TGLTeapot
      Scale.Coordinates = {00000040000000400000004000000000}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00004842000034420000F0410000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 70
      TargetObject = GLDirectOpenGL1
      Position.Coordinates = {00004040000080400000A0400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 64
  end
end
