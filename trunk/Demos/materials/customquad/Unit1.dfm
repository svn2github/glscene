object Form1: TForm1
  Left = 191
  Top = 101
  Width = 452
  Height = 368
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
    Width = 444
    Height = 339
    Camera = GLCamera1
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        080500000000000000B405400500000000000000000000050000000000000000
        000009020008020008}
      object DirectOpenGL1: TGLDirectOpenGL
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        UseBuildList = False
        OnRender = DirectOpenGL1Render
      end
    end
    object Torus1: TGLTorus
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      MajorRadius = 1.29999995231628
      MinorRadius = 0.100000001490116
      Rings = 36
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        08050000000000000000000005000000000000000000000500000000000000F0
        034009020008020008}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000A0410000A0410000A0410000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 75
      TargetObject = DummyCube1
      Position.Coordinates = {00004040000000400000803F0000803F}
      Left = 200
      Top = 136
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <>
    Left = 8
    Top = 40
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 8
    Top = 72
  end
end
