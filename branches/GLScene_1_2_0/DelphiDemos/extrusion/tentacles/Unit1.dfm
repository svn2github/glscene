object Form1: TForm1
  Left = 218
  Top = 106
  Caption = 'Form1'
  ClientHeight = 331
  ClientWidth = 452
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
    Width = 452
    Height = 331
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 146.379318237304700000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCBase: TGLDummyCube
      Position.Coordinates = {00000000000000C0000000000000803F}
      CubeSize = 1.000000000000000000
      object Sphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {00000000CDCC4C3E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {0000000039B4483E000000000000803F}
        Scale.Coordinates = {000000400000003F0000004000000000}
        Radius = 1.000000000000000000
        Slices = 32
        Stacks = 8
      end
      object Pipe1: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe2: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe3: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe4: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe5: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
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
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'PipeMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end>
    Left = 16
    Top = 104
  end
end
