object Form1: TForm1
  Left = 395
  Top = 115
  Width = 228
  Height = 208
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 220
    Height = 179
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.DepthTest = False
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLParticles1: TGLParticles
      ObjectsSorting = osNone
      CubeSize = 1
      ParticlePoolSize = 10
      OnActivateParticle = GLParticles1ActivateParticle
      object DummyCube1: TDummyCube
        OnProgress = DummyCube1Progress
        CubeSize = 1
        BehavioursData = {
          0201060B54474C42496E657274696102000200050000000000000080FF3F0200
          0805000000000000000000000500000000000000000000050000000000000000
          000008020008020008}
        object Sprite1: TSprite
          Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          Width = 0.100000001490116
          Height = 0.100000001490116
          NoZWrite = False
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 25
      TargetObject = GLParticles1
      Position.Coordinates = {0000204100004040000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 40
    Top = 8
  end
end
