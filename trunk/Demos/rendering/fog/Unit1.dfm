object Form1: TForm1
  Left = 192
  Top = 119
  Width = 510
  Height = 450
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    502
    416)
  PixelsPerInch = 96
  TextHeight = 13
  object LFogStart: TLabel
    Left = 8
    Top = 312
    Width = 38
    Height = 13
    Caption = 'fog start'
  end
  object LFogEnd: TLabel
    Left = 8
    Top = 344
    Width = 36
    Height = 13
    Caption = 'fog end'
  end
  object LFogColor: TLabel
    Left = 344
    Top = 288
    Width = 41
    Height = 13
    Caption = 'fog color'
  end
  object SFogColor: TShape
    Left = 392
    Top = 288
    Width = 57
    Height = 16
    OnMouseDown = SFogColorMouseDown
  end
  object LFogDensity: TLabel
    Left = 344
    Top = 336
    Width = 144
    Height = 13
    Caption = 'fog density (for fmExp/fmExp2)'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 502
    Height = 274
    Camera = GLCamera1
    Anchors = [akLeft, akTop, akRight]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CBFogEnable: TCheckBox
    Left = 8
    Top = 288
    Width = 73
    Height = 17
    Caption = 'fog on/off'
    TabOrder = 1
    OnClick = CBFogEnableClick
  end
  object RGFogDistance: TRadioGroup
    Left = 120
    Top = 288
    Width = 105
    Height = 73
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
    Caption = '[ texture ]'
    TabOrder = 5
    object CBTextureEnabled: TCheckBox
      Left = 8
      Top = 16
      Width = 65
      Height = 17
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
      Caption = 'ignore fog'
      TabOrder = 1
      OnClick = CBTextureIgnoreFogClick
    end
  end
  object EFogStart: TEdit
    Left = 56
    Top = 312
    Width = 57
    Height = 21
    TabOrder = 6
    Text = '-30'
    OnChange = EFogStartChange
  end
  object EFogEnd: TEdit
    Left = 56
    Top = 344
    Width = 57
    Height = 21
    TabOrder = 7
    Text = '30'
    OnChange = EFogStartChange
  end
  object EFogDensity: TEdit
    Left = 392
    Top = 352
    Width = 57
    Height = 21
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
