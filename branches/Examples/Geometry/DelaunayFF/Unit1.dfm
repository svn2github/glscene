object Form1: TForm1
  Left = 219
  Top = 197
  Caption = 'Delaunay Triangulation'
  ClientHeight = 504
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
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
  object Panel1: TPanel
    Left = 512
    Top = 0
    Width = 278
    Height = 504
    Align = alRight
    BevelOuter = bvNone
    Color = 6316128
    TabOrder = 0
    object TimeLabel: TLabel
      Left = 16
      Top = 216
      Width = 21
      Height = 13
      Caption = '       '
    end
    object GroupBox1: TGroupBox
      Left = 7
      Top = 8
      Width = 265
      Height = 121
      Caption = 'Subdivide'
      TabOrder = 0
      object Label3: TLabel
        Left = 16
        Top = 24
        Width = 66
        Height = 13
        Caption = 'Smooth factor'
      end
      object SmoothTB: TTrackBar
        Left = 16
        Top = 40
        Width = 209
        Height = 20
        Max = 50
        Frequency = 10
        Position = 10
        TabOrder = 0
        TabStop = False
        ThumbLength = 14
      end
      object SubdivideBtn: TButton
        Left = 16
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Subdivide'
        TabOrder = 1
        OnClick = SubdivideBtnClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 7
      Top = 144
      Width = 265
      Height = 57
      Caption = 'Options'
      TabOrder = 1
      object WireframeCB: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Wireframe'
        TabOrder = 0
        OnClick = WireframeCBClick
      end
      object TexturedCB: TCheckBox
        Left = 120
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Textured'
        TabOrder = 1
        OnClick = TexturedCBClick
      end
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 504
    Camera = Camera
    Buffer.BackgroundColor = 16492697
    Buffer.FaceCulling = False
    FieldOfView = 136.711135864257800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 1
  end
  object GLScene: TGLScene
    Left = 24
    Top = 16
    object CamH: TGLDummyCube
      Direction.Coordinates = {0100003FD7B35D3F0000000000000000}
      Position.Coordinates = {00000000000000000000F0410000803F}
      TurnAngle = -30.000000000000000000
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object CamV: TGLDummyCube
        Direction.Coordinates = {00000000EA46773FEE83843E00000000}
        PitchAngle = 15.000000000000000000
        Up.Coordinates = {00000000EE8384BEEA46773F00000000}
        Visible = False
        CubeSize = 1.000000000000000000
        VisibleAtRunTime = True
        object Camera: TGLCamera
          DepthOfView = 10000.000000000000000000
          FocalLength = 100.000000000000000000
          NearPlaneBias = 0.100000001490116100
          TargetObject = CamH
          Position.Coordinates = {000000000000FAC3000000000000803F}
          Direction.Coordinates = {000000000000803F0000000000000000}
          Up.Coordinates = {00000000000000000000803F00000000}
        end
      end
    end
    object Windrose: TGLFreeForm
      Visible = False
      MaterialLibrary = MatLib
    end
    object ffTerrain: TGLFreeForm
      MaterialLibrary = MatLib
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000004842000000000000803F}
      LightStyle = lsParallel
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {0000803F0000803F0000803F00000000}
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 80
    Top = 16
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 80
    Top = 80
  end
  object MatLib: TGLMaterialLibrary
    Left = 24
    Top = 80
  end
end
