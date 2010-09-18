object Form1: TForm1
  Left = 171
  Top = 95
  Width = 468
  Height = 359
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
    Width = 368
    Height = 321
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 145.393615722656300000
    Align = alClient
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 368
    Top = 0
    Width = 84
    Height = 321
    Align = alRight
    Caption = 'Mode'
    ItemIndex = 1
    Items.Strings = (
      'Sleepy'
      'Gentle'
      'Average'
      'Restless'
      'Angry'
      'Inferno')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCVolcano: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        58434F4C02010201060B54474C42496E65727469610200060E53696D706C6520
        496E657274696102000200050000000000000080FF3F02000805000000000000
        00B4054005000000000000000000000500000000000000000000090200080200
        08}
      EffectsData = {
        58434F4C02010201061254474C536F757263655046584566666563740201060A
        504658566F6C63616E6F050000000000000080FF3F0206020009000000000000
        40400000000000000000020008020008050000000000000080FF3F0500000000
        00CDCCCCFC3F0500000000008FC2F5F83F020002000905000000000000000000
        00080200}
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003F0000803F}
        Position.Coordinates = {0000000000000040000000400000803F}
        Radius = 0.300000011920929000
        Slices = 12
        Stacks = 12
        EffectsData = {
          58434F4C02010201061254474C536F7572636550465845666665637402010607
          504658426C7565050000000000000080FF3F0206020008020008020008050000
          000000CDCCCCFB3F050000000000CDCCCCFB3F050000000000CDCCCCFB3F0200
          0200090500000000000000000000080200}
      end
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCCamera: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 30.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DCCamera
        Position.Coordinates = {00002041000000410000C0400000803F}
      end
    end
  end
  object PFXVolcano: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Acceleration.Coordinates = {00000000000080BF0000000000000000}
    Friction = 1.000000000000000000
    NbSides = 7
    ParticleSize = 0.250000000000000000
    ColorOuter.Color = {0000803F000000000000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F000000000000000000000000}
        LifeTime = 7.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 56
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 136
    Top = 16
  end
  object PFXBlue: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Friction = 1.000000000000000000
    ParticleSize = 0.550000011920929000
    ColorInner.Color = {00000000000000000000803F0000803F}
    ColorOuter.Color = {00000000000000001283203F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 56
    Top = 72
  end
end
