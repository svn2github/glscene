object Form1: TForm1
  Left = 191
  Top = 107
  Width = 438
  Height = 369
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
    Width = 430
    Height = 340
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object BUBind: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Bind Shader'
    TabOrder = 1
    OnClick = BUBindClick
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00002041000000410000E0400000803F}
      SpotCutOff = 180
    end
    object Torus1: TGLTorus
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      MajorRadius = 2.5
      MinorRadius = 1.5
    end
    object Sphere1: TGLSphere
      ShowAxes = True
      Radius = 0.5
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Torus1
      Position.Coordinates = {0000E0400000A040000040400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 16
    Top = 48
  end
end
