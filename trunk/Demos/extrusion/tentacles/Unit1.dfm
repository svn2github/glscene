object Form1: TForm1
  Left = 227
  Top = 109
  Width = 468
  Height = 369
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
    Width = 460
    Height = 340
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCBase: TGLDummyCube
      Position.Coordinates = {00000000000000C0000000000000803F}
      CubeSize = 1
      object Sphere1: TGLSphere
        Scale.Coordinates = {000000400000003F0000004000000000}
        Material.FrontProperties.Ambient.Color = {00000000CDCC4C3E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {0000000039B4483E000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 1
        Slices = 32
        Stacks = 8
      end
      object Pipe1: TGLPipe
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Nodes = <>
        Radius = 1
      end
      object Pipe2: TGLPipe
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Nodes = <>
        Radius = 1
      end
      object Pipe3: TGLPipe
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Nodes = <>
        Radius = 1
      end
      object Pipe4: TGLPipe
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Nodes = <>
        Radius = 1
      end
      object Pipe5: TGLPipe
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Nodes = <>
        Radius = 1
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180
    end
    object DCTarget: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DCTarget
      Position.Coordinates = {0000C0400000A040000080400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 48
  end
end
