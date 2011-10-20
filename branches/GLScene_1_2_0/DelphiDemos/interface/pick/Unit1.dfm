object Form1: TForm1
  Left = 227
  Top = 96
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 191
  ClientWidth = 280
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
    Left = 5
    Top = 5
    Width = 270
    Height = 182
    Camera = GLCamera1
    FieldOfView = 122.426765441894500000
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    object Line: TGLLines
      LineColor.Color = {0000003F0000003F0000003F0000803F}
      LineWidth = 3.000000000000000000
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'LibMaterial6'
      Direction.Coordinates = {00000000000000800000803F00000000}
      Nodes = <
        item
          Y = -1.000000000000000000
        end
        item
          X = 0.800000011920928900
        end
        item
          X = -1.000000000000000000
          Y = 2.000000000000000000
        end
        item
          Y = 3.000000000000000000
        end>
      NodesAspect = lnaInvisible
      Division = 20
      SplineMode = lsmBezierSpline
      Options = []
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000C842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Sphere: TGLSphere
      Material.MaterialLibrary = GLMaterialLibraryEx1
      Material.LibMaterialName = 'DefaultMaterial'
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'DefaultMaterial'
      Position.Coordinates = {00000040000080BE000000000000803F}
      Radius = 1.000000000000000000
    end
    object Cylinder: TGLCylinder
      Material.MaterialLibrary = GLMaterialLibraryEx1
      Material.LibMaterialName = 'DefaultMaterial'
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'DefaultMaterial'
      Position.Coordinates = {000000C0000080BE000000000000803F}
      BottomRadius = 1.000000000000000000
      Height = 1.500000000000000000
      TopRadius = 1.000000000000000000
    end
    object Torus: TGLTorus
      Material.MaterialLibrary = GLMaterialLibraryEx1
      Material.LibMaterialName = 'DefaultMaterial'
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'DefaultMaterial'
      Direction.Coordinates = {F604B5BEF304353F70C41C3F00000000}
      Position.Coordinates = {000000C0CDCC0C40000000000000803F}
      Up.Coordinates = {F604B53EF304353F70C41CBF00000000}
      MajorRadius = 0.699999988079071100
      MinorRadius = 0.200000002980232200
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
    end
    object Cone: TGLCone
      Material.MaterialLibrary = GLMaterialLibraryEx1
      Material.LibMaterialName = 'DefaultMaterial'
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'DefaultMaterial'
      Direction.Coordinates = {00000000F28384BEEA46773F00000000}
      Position.Coordinates = {0000004000002040000000000000803F}
      Up.Coordinates = {00000000EA46773FF283843E00000000}
      BottomRadius = 1.000000000000000000
      Height = 1.500000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000000000008040000020410000803F}
    end
  end
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'DefaultMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.LineProperties.Enabled = True
        FixedFunction.LineProperties.Width = 3.000000000000000000
        FixedFunction.LineProperties.StipplePattern = 65535
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end
      item
        Name = 'HighlightMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
        FixedFunction.LineProperties.Enabled = True
        FixedFunction.LineProperties.Width = 3.000000000000000000
        FixedFunction.LineProperties.StipplePattern = 65535
        FixedFunction.Texture.Enabled = False
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end
      item
        Name = 'HighlightMaterial2'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.FrontProperties.Emission.Color = {9A99193FCDCC4C3FACC8483E0000803F}
        FixedFunction.LineProperties.Enabled = True
        FixedFunction.LineProperties.Width = 3.000000000000000000
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
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 10
    OnProgress = GLCadencer1Progress
    Top = 48
  end
end
