object Form1: TForm1
  Left = 293
  Top = 145
  Caption = 'Form1'
  ClientHeight = 502
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    644
    502)
  PixelsPerInch = 96
  TextHeight = 13
  object LFogStart: TLabel
    Left = 8
    Top = 312
    Width = 38
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'fog start'
  end
  object LFogEnd: TLabel
    Left = 8
    Top = 344
    Width = 36
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'fog end'
  end
  object LFogColor: TLabel
    Left = 344
    Top = 288
    Width = 41
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'fog color'
  end
  object SFogColor: TShape
    Left = 392
    Top = 288
    Width = 57
    Height = 16
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    OnMouseDown = SFogColorMouseDown
  end
  object LFogDensity: TLabel
    Left = 344
    Top = 336
    Width = 144
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'fog density (for fmExp/fmExp2)'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 2
    Top = 2
    Width = 520
    Height = 273
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    FieldOfView = 139.764404296875000000
    Anchors = [akLeft, akTop, akRight]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CBFogEnable: TCheckBox
    Left = 8
    Top = 288
    Width = 73
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'fog on/off'
    TabOrder = 1
    OnClick = CBFogEnableClick
  end
  object RGFogDistance: TRadioGroup
    Left = 120
    Top = 288
    Width = 105
    Height = 73
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '[ fog mode ]'
    ItemIndex = 0
    Items.Strings = (
      'fdDefault'
      'fdEyePlane'
      'fdEyeRadial')
    TabOrder = 2
    OnClick = RGFogModeClick
  end
  object RGFogMode: TRadioGroup
    Left = 232
    Top = 288
    Width = 105
    Height = 73
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '[ fog mode ]'
    ItemIndex = 0
    Items.Strings = (
      'fmLinear'
      'fmExp'
      'fmExp2')
    TabOrder = 3
    OnClick = RGFogModeClick
  end
  object CBApplyToBackground: TCheckBox
    Left = 376
    Top = 304
    Width = 105
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'background too ?'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CBApplyToBackgroundClick
  end
  object GBTexture: TGroupBox
    Left = 120
    Top = 368
    Width = 217
    Height = 41
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '[ texture ]'
    TabOrder = 5
    object CBTextureEnabled: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBTextureEnabledClick
    end
    object CBTextureIgnoreFog: TCheckBox
      Left = 80
      Top = 16
      Width = 73
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'ignore fog'
      TabOrder = 1
      OnClick = CBTextureIgnoreFogClick
    end
  end
  object EFogStart: TEdit
    Left = 56
    Top = 312
    Width = 57
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 6
    Text = '-30'
    OnChange = EFogStartChange
  end
  object EFogEnd: TEdit
    Left = 56
    Top = 344
    Width = 57
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 7
    Text = '30'
    OnChange = EFogStartChange
  end
  object EFogDensity: TEdit
    Left = 392
    Top = 352
    Width = 57
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 8
    Text = '100'
    OnChange = EFogStartChange
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000096430000FA430000C8430000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000C04000008040000000410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 48
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Left = 80
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 112
    Top = 16
  end
end
