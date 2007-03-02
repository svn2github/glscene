object MainForm: TMainForm
  Left = 278
  Top = 272
  Width = 511
  Height = 357
  BorderWidth = 2
  Caption = 'TGLPostEffect demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 497
    Top = 33
    Width = 2
    Height = 293
    Align = alRight
    AutoSize = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 499
    Height = 33
    Align = alTop
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 10
      Width = 49
      Height = 13
      Caption = 'PostEffect'
    end
    object ComboBox1: TComboBox
      Left = 64
      Top = 6
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'pepNone'
      OnChange = ComboBox1Change
      Items.Strings = (
        'pepNone'
        'pepGray'
        'pepNegative'
        'pepWeird'
        'pepRedNoise')
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 33
    Width = 497
    Height = 293
    Camera = GLCamera1
    Buffer.BackgroundColor = 16776176
    FieldOfView = 142.310775756835900000
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 80
    object GLActor1: TGLActor
      Material.Texture.TextureMode = tmReplace
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Interval = 100
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLPostEffect1: TGLPostEffect
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLActor1
      Position.Coordinates = {0000A04000008040000040400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 80
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 104
    Top = 80
  end
end
