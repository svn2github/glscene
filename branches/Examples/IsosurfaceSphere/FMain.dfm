object FrmMain: TFrmMain
  Left = 192
  Top = 107
  Caption = 'IsoSurface Sphere'
  ClientHeight = 520
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 153
    Top = 0
    Width = 599
    Height = 520
    Camera = glcMainCamera
    Buffer.BackgroundColor = clMoneyGreen
    Buffer.ShadeModel = smSmooth
    FieldOfView = 137.924972534179700000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object PUSerInterface: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 520
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 173
      Width = 38
      Height = 11
      Caption = 'Vertices'
    end
    object Label2: TLabel
      Left = 10
      Top = 189
      Width = 43
      Height = 11
      Caption = 'Triangles'
    end
    object lblVertices: TLabel
      Left = 82
      Top = 171
      Width = 57
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object lblTriangles: TLabel
      Left = 82
      Top = 190
      Width = 57
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object Label3: TLabel
      Left = 8
      Top = 60
      Width = 20
      Height = 11
      Caption = 'Size'
    end
    object Label4: TLabel
      Left = 10
      Top = 115
      Width = 41
      Height = 11
      Caption = 'IsoValue'
    end
    object Label5: TLabel
      Left = 12
      Top = 6
      Width = 91
      Height = 13
      Caption = 'Number of Spheres'
    end
    object RGAlgorithm: TRadioGroup
      Left = 2
      Top = 213
      Width = 145
      Height = 71
      Caption = 'Algorithm'
      ItemIndex = 1
      Items.Strings = (
        'March. Tetrahedra'
        'March. Cubes')
      TabOrder = 0
      OnClick = RGAlgorithmClick
    end
    object RAWireFrameFill: TRadioGroup
      Left = 2
      Top = 292
      Width = 145
      Height = 71
      Caption = 'Fill / Wire '
      ItemIndex = 1
      Items.Strings = (
        'Fill'
        'Wireframe')
      TabOrder = 1
      OnClick = RGAlgorithmClick
    end
    object tbSize: TTrackBar
      Left = 2
      Top = 76
      Width = 145
      Height = 31
      Max = 100
      Min = 1
      Frequency = 5
      Position = 25
      TabOrder = 2
      OnChange = tbSizeChange
    end
    object tbIsoValue: TTrackBar
      Left = 2
      Top = 134
      Width = 145
      Height = 31
      Max = 255
      Frequency = 10
      Position = 128
      TabOrder = 3
      OnChange = RGAlgorithmClick
    end
    object rbgShading: TRadioGroup
      Left = 2
      Top = 373
      Width = 145
      Height = 71
      Caption = 'Shading '
      ItemIndex = 1
      Items.Strings = (
        'Flat'
        'Smooth')
      TabOrder = 4
      OnClick = rbgShadingClick
    end
    object TrackBar1: TTrackBar
      Left = 2
      Top = 23
      Width = 145
      Height = 31
      Max = 5
      Min = 1
      Position = 1
      TabOrder = 5
      OnChange = TrackBar1Change
    end
    object rbgInterpolation: TRadioGroup
      Left = 2
      Top = 450
      Width = 145
      Height = 71
      Caption = 'Interpolation Mode'
      ItemIndex = 1
      Items.Strings = (
        'Rugged'
        'Polished')
      TabOrder = 6
      OnClick = RGAlgorithmClick
    end
  end
  object GLScene: TGLScene
    Left = 312
    Top = 56
    object DCDummyCube: TGLDummyCube
      Direction.Coordinates = {70C41C3F0000003F70C41C3F00000000}
      PitchAngle = 30.000000000000000000
      Position.Coordinates = {000000BF000000BF000000BF0000803F}
      ShowAxes = True
      TurnAngle = 45.000000000000000000
      Up.Coordinates = {F304B5BED7B35D3FF304B5BE00000000}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {0000000000000000000000000000803F}
      VisibleAtRunTime = True
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100002041000020410000803F}
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object ffFreeForm: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {52B85E3FE17A143F48E17A3F0000803F}
        Material.PolygonMode = pmLines
        Scale.Coordinates = {00000040000000400000004000000000}
      end
    end
    object glcMainCamera: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DCDummyCube
      Position.Coordinates = {0000000000000000000048C20000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 432
    Top = 56
  end
end
