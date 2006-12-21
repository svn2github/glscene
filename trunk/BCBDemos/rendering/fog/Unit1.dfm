object Form1: TForm1
  Left = 293
  Top = 145
  Width = 652
  Height = 546
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object LFogStart: TLabel
    Left = 10
    Top = 384
    Width = 47
    Height = 16
    Caption = 'fog start'
  end
  object LFogEnd: TLabel
    Left = 10
    Top = 423
    Width = 45
    Height = 16
    Caption = 'fog end'
  end
  object LFogColor: TLabel
    Left = 423
    Top = 354
    Width = 52
    Height = 16
    Caption = 'fog color'
  end
  object SFogColor: TShape
    Left = 482
    Top = 354
    Width = 71
    Height = 20
    OnMouseDown = SFogColorMouseDown
  end
  object LFogDensity: TLabel
    Left = 423
    Top = 414
    Width = 181
    Height = 16
    Caption = 'fog density (for fmExp/fmExp2)'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 2
    Top = 2
    Width = 640
    Height = 337
    Camera = GLCamera1
    FieldOfView = 146.944976806641
    Anchors = [akLeft, akTop, akRight]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CBFogEnable: TCheckBox
    Left = 10
    Top = 354
    Width = 90
    Height = 21
    Caption = 'fog on/off'
    TabOrder = 1
    OnClick = CBFogEnableClick
  end
  object RGFogDistance: TRadioGroup
    Left = 148
    Top = 354
    Width = 129
    Height = 90
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
    Left = 286
    Top = 354
    Width = 129
    Height = 90
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
    Left = 463
    Top = 374
    Width = 129
    Height = 21
    Caption = 'background too ?'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CBApplyToBackgroundClick
  end
  object GBTexture: TGroupBox
    Left = 148
    Top = 453
    Width = 267
    Height = 50
    Caption = '[ texture ]'
    TabOrder = 5
    object CBTextureEnabled: TCheckBox
      Left = 10
      Top = 20
      Width = 80
      Height = 21
      Caption = 'enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBTextureEnabledClick
    end
    object CBTextureIgnoreFog: TCheckBox
      Left = 98
      Top = 20
      Width = 90
      Height = 21
      Caption = 'ignore fog'
      TabOrder = 1
      OnClick = CBTextureIgnoreFogClick
    end
  end
  object EFogStart: TEdit
    Left = 69
    Top = 384
    Width = 70
    Height = 24
    TabOrder = 6
    Text = '-30'
    OnChange = EFogStartChange
  end
  object EFogEnd: TEdit
    Left = 69
    Top = 423
    Width = 70
    Height = 24
    TabOrder = 7
    Text = '30'
    OnChange = EFogStartChange
  end
  object EFogDensity: TEdit
    Left = 482
    Top = 433
    Width = 71
    Height = 24
    TabOrder = 8
    Text = '100'
    OnChange = EFogStartChange
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {000096430000FA430000C8430000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
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
    Ctl3D = True
    Left = 80
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 112
    Top = 16
  end
end
