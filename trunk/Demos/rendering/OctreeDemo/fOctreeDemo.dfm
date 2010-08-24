object frmOctreeDemo: TfrmOctreeDemo
  Left = 309
  Top = 207
  Width = 778
  Height = 580
  Caption = 'Octree Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  DesignSize = (
    762
    542)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 722
    Top = 8
    Width = 32
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'Label1'
  end
  object Label3: TLabel
    Left = 16
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Leaf Threshold'
  end
  object Label2: TLabel
    Left = 120
    Top = 8
    Width = 388
    Height = 13
    Caption = 
      '(Green = Colliding with other object, Red = inside query box/sph' +
      'ere, Yellow = both)'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 56
    Width = 750
    Height = 489
    Camera = GLCamera1
    Buffer.BackgroundColor = clWhite
    Buffer.FaceCulling = False
    FieldOfView = 145.893371582031300000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object TrackBar_LeafThreshold: TTrackBar
    Left = 8
    Top = 24
    Width = 150
    Height = 25
    Max = 20
    Min = 3
    Position = 10
    TabOrder = 1
    TickStyle = tsNone
    OnChange = TrackBar_LeafThresholdChange
  end
  object Button_ResetOctreeSize: TButton
    Left = 160
    Top = 24
    Width = 105
    Height = 21
    Caption = 'Reset Octree Size'
    TabOrder = 2
    OnClick = Button_ResetOctreeSizeClick
  end
  object GLScene1: TGLScene
    Left = 96
    Top = 64
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCCCC3E}
      Material.BlendingMode = bmTransparency
      CubeSize = {0000A0400000A04000000041}
    end
    object GLSphere1: TGLSphere
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCC4C3E}
      Material.BlendingMode = bmTransparency
      Position.Coordinates = {0000C0400000C0400000C0400000803F}
      Radius = 2.500000000000000000
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCC4C3E}
      Material.BlendingMode = bmTransparency
      Direction.Coordinates = {0000000000000080000080BF00000000}
      Position.Coordinates = {0000000000000000000020410000803F}
      Up.Coordinates = {6A41323FD9BD373F0000000000000000}
      Visible = False
      Height = 40.000000000000000000
      Width = 40.000000000000000000
    end
    object GLLines1: TGLLines
      Nodes = <>
      Options = []
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000404100008040000000410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 112
  end
end
