object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Terrain Editor'
  ClientHeight = 630
  ClientWidth = 804
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 804
    Height = 630
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 161.961349487304700000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLDirectOpenGL1: TGLDirectOpenGL
        UseBuildList = False
        OnRender = GLDirectOpenGL1Render
        Blend = False
        object GLCone1: TGLCone
          Material.FrontProperties.Ambient.Color = {E7E6663FE7E6663F000000000000803F}
          Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
          Position.Coordinates = {000000000000003E000000000000803F}
          Up.Coordinates = {00000000000080BF0000008000000000}
          BottomRadius = 0.050000000745058060
          Height = 0.250000000000000000
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000004040000080400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'TerrainMat'
        Tag = 0
      end>
    Left = 104
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 24
    Top = 92
  end
end
