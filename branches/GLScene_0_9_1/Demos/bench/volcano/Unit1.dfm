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
    Width = 376
    Height = 330
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object RadioGroup1: TRadioGroup
    Left = 376
    Top = 0
    Width = 84
    Height = 330
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
      CubeSize = 1
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        080500000000000000B405400500000000000000000000050000000000000000
        000009020008020008}
      EffectsData = {
        0201061254474C536F757263655046584566666563740200060A504658566F6C
        63616E6F02010200090000000000004040000000000000000002000805000000
        0000000080FF3F050000000000CDCCCCFC3F0500000000008FC2F5F83F020002
        00}
      object Sphere1: TGLSphere
        Position.Coordinates = {0000000000000040000000400000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000008180003F0000803F}
        Radius = 0.300000011920929
        Slices = 12
        Stacks = 12
        EffectsData = {
          0201061254474C536F7572636550465845666665637402000607504658426C75
          650201020008020008050000000000CDCCCCFB3F050000000000CDCCCCFB3F05
          0000000000CDCCCCFB3F02000200}
      end
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000204100002041000000000000803F}
      SpotCutOff = 180
    end
    object DCCamera: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 30
        FocalLength = 50
        TargetObject = DCCamera
        Position.Coordinates = {00002041000000410000C0400000803F}
      end
    end
  end
  object PFXVolcano: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Acceleration.Coordinates = {00000000000080BF0000000000000000}
    NbSides = 7
    ParticleSize = 0.25
    ColorOuter.Color = {0000803F000000000000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F000000000000000000000000}
        LifeTime = 7
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
    ParticleSize = 0.55
    ColorInner.Color = {00000000000000000000803F0000803F}
    ColorOuter.Color = {00000000000000001283203F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 3
      end>
    Left = 56
    Top = 56
  end
end
