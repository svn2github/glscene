object Form1: TForm1
  Left = 275
  Top = 166
  Caption = 'GLSL Lattice & Glass'
  ClientHeight = 566
  ClientWidth = 712
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 396
    Height = 566
    Camera = GLCamera1
    Buffer.BackgroundColor = 4276545
    FieldOfView = 151.655319213867200000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 396
    Top = 0
    Width = 316
    Height = 566
    Align = alRight
    BevelOuter = bvNone
    Color = 6316128
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 32
      Top = 17
      Width = 258
      Height = 169
      Caption = ' Lattice '
      TabOrder = 0
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 27
        Height = 13
        Caption = 'Scale'
      end
      object Label4: TLabel
        Left = 8
        Top = 88
        Width = 47
        Height = 13
        Caption = 'Threshold'
      end
      object trScaleX: TTrackBar
        Left = 32
        Top = 32
        Width = 209
        Height = 20
        Max = 300
        Frequency = 10
        Position = 100
        TabOrder = 0
        TabStop = False
        ThumbLength = 14
        OnChange = DepthTrackBar2Change
      end
      object trScaleY: TTrackBar
        Left = 32
        Top = 64
        Width = 209
        Height = 20
        Max = 300
        Frequency = 10
        Position = 100
        TabOrder = 1
        TabStop = False
        ThumbLength = 14
        OnChange = DepthTrackBar2Change
      end
      object trTresholdX: TTrackBar
        Left = 40
        Top = 104
        Width = 209
        Height = 20
        Max = 100
        Frequency = 10
        Position = 13
        TabOrder = 2
        TabStop = False
        ThumbLength = 14
        OnChange = DepthTrackBar2Change
      end
      object trTresholdY: TTrackBar
        Left = 40
        Top = 136
        Width = 209
        Height = 20
        Max = 100
        Frequency = 10
        Position = 13
        TabOrder = 3
        TabStop = False
        ThumbLength = 14
        OnChange = DepthTrackBar2Change
      end
    end
    object GroupBox3: TGroupBox
      Left = 6
      Top = 192
      Width = 298
      Height = 121
      Caption = ' Glass '
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 29
        Height = 13
        Caption = 'Depth'
      end
      object Label2: TLabel
        Left = 24
        Top = 56
        Width = 16
        Height = 13
        Caption = 'Mix'
      end
      object DepthTrackBar2: TTrackBar
        Left = 40
        Top = 24
        Width = 209
        Height = 20
        Max = 100
        Position = 1
        TabOrder = 0
        TabStop = False
        ThumbLength = 14
        OnChange = DepthTrackBar2Change
      end
      object MixTrackBar1: TTrackBar
        Left = 40
        Top = 56
        Width = 209
        Height = 20
        Max = 200
        Frequency = 10
        Position = 199
        TabOrder = 1
        TabStop = False
        ThumbLength = 14
      end
      object MixEdit1: TEdit
        Left = 247
        Top = 52
        Width = 50
        Height = 21
        TabStop = False
        ReadOnly = True
        TabOrder = 2
        Text = '0.00'
      end
      object DepthEdit1: TEdit
        Left = 247
        Top = 20
        Width = 50
        Height = 21
        TabStop = False
        ReadOnly = True
        TabOrder = 3
        Text = '0.00'
      end
      object btnColorTint1: TButton
        Left = 46
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Color Tint'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = btnColorTint1Click
      end
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {D8B3DD3E0000003F0000403F00000000}
      PitchAngle = 30.000000000000000000
      ShowAxes = True
      TurnAngle = 30.000000000000000000
      Up.Coordinates = {000080BED7B35D3FD7B3DDBE00000000}
      CubeSize = 1.000000000000000000
      object GLPlane1: TGLPlane
        Material.MaterialLibrary = MatLib1
        Material.LibMaterialName = 'planeMap'
        Position.Coordinates = {0000000000000000000080BF0000803F}
        Height = 2.000000000000000000
        Width = 2.000000000000000000
      end
      object GLCube1: TGLCube
        Material.MaterialLibrary = MatLib1
        Material.LibMaterialName = 'LibMaterial'
      end
      object GLSphere1: TGLSphere
        Material.MaterialLibrary = MatLib1
        Material.LibMaterialName = 'envMap'
        Position.Coordinates = {0000000000000000000000400000803F}
        Radius = 0.500000000000000000
      end
    end
    object Init1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = Init1Render
      Blend = False
    end
    object Render1: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      Blend = False
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000404000008040000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      SceneScale = 2.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {000000000000803F000080400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
  object LatticeShader1: TGLUserShader
    OnDoApply = LatticeShader1DoApply
    OnDoUnApply = LatticeShader1DoUnApply
    Left = 80
    Top = 8
  end
  object MatLib1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'envMap'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = GlassShader1
      end
      item
        Name = 'refractionMap'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {9F9E9E3E8B8A0A3FCDCC4C3F0000803F}
        Shader = LatticeShader1
      end
      item
        Name = 'planeMap'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 8
    Top = 40
  end
  object GlassShader1: TGLUserShader
    OnDoApply = GlassShader1DoApply
    OnDoUnApply = GlassShader1DoUnApply
    Left = 120
    Top = 8
  end
  object ColorDialog1: TColorDialog
    Color = clLime
    Left = 536
    Top = 280
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 8
    Top = 72
  end
end
