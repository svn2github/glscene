object Form1: TForm1
  Left = 284
  Top = 105
  Width = 280
  Height = 280
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 272
    Height = 248
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
      Scale.Coordinates = {00000040000000400000004000000000}
      CubeSize = 1
      OnActivateParticle = GLParticles1ActivateParticle
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        080500000000000000F003400500000000000000000000050000000000000000
        000008020008020008}
      object Sprite1: TGLSprite
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.BlendingMode = bmAdditive
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '..\..\media\Flare1.bmp'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfIntensity
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Material.Texture.Disabled = False
        OnProgress = Sprite1Progress
        Width = 1
        Height = 1
        NoZWrite = False
        MirrorU = False
        MirrorV = False
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = GLParticles1
      Position.Coordinates = {0000704100000000000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    Left = 8
    Top = 48
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 48
    Top = 8
  end
end
