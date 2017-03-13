object frmBuggy: TfrmBuggy
  Left = 363
  Top = 130
  Caption = 'Buggy Drivin'#39
  ClientHeight = 424
  ClientWidth = 681
  Color = clBtnFace
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
  DesignSize = (
    681
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 572
    Top = 8
    Width = 100
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Steering : Arrow keys'
  end
  object Label2: TLabel
    Left = 572
    Top = 24
    Width = 54
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Flip car : '#39'X'#39
  end
  object Label3: TLabel
    Left = 572
    Top = 40
    Width = 91
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Handbrake : space'
  end
  object Label_Friction: TLabel
    Left = 576
    Top = 64
    Width = 34
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Friction'
  end
  object Label4: TLabel
    Left = 576
    Top = 336
    Width = 82
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Frames / Second'
  end
  object Label_FPS: TLabel
    Left = 576
    Top = 352
    Width = 37
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '(none)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 576
    Top = 104
    Width = 90
    Height = 13
    Caption = 'Shadow "strength"'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 569
    Height = 424
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 129.493667602539100000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object TrackBar_Friction: TTrackBar
    Left = 576
    Top = 80
    Width = 102
    Height = 17
    Anchors = [akTop, akRight]
    Max = 100
    Position = 14
    TabOrder = 1
    TabStop = False
    ThumbLength = 10
    TickStyle = tsManual
    OnChange = TrackBar_FrictionChange
  end
  object Button_AddBall: TButton
    Left = 584
    Top = 144
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&Add ball'
    TabOrder = 2
    OnClick = Button_AddBallClick
  end
  object CheckBox_Shadows: TCheckBox
    Left = 576
    Top = 399
    Width = 73
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Shadows'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox_ShadowsClick
  end
  object CheckBox_StencilBuffer: TCheckBox
    Left = 576
    Top = 383
    Width = 89
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Stencil Buffer'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox_StencilBufferClick
  end
  object CheckBox_Headlights: TCheckBox
    Left = 576
    Top = 367
    Width = 73
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = '&Headlights'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox_HeadlightsClick
  end
  object TrackBar_ShadowStrength: TTrackBar
    Left = 576
    Top = 120
    Width = 102
    Height = 17
    Anchors = [akTop, akRight]
    Max = 100
    Position = 50
    TabOrder = 6
    TabStop = False
    ThumbLength = 10
    TickStyle = tsManual
    OnChange = TrackBar_ShadowStrengthChange
  end
  object CheckBox_ScissorTest: TCheckBox
    Left = 576
    Top = 311
    Width = 73
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Scissors'
    TabOrder = 7
    OnClick = CheckBox_ScissorTestClick
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 10
    OnProgress = GLCadencer1Progress
    Left = 168
    Top = 72
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 48
    object HeightField1: TGLHeightField
      Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      XSamplingScale.Min = -3.000000000000000000
      XSamplingScale.Max = 3.000000000000000000
      XSamplingScale.Step = 0.250000000000000000
      YSamplingScale.Min = -3.000000000000000000
      YSamplingScale.Max = 3.000000000000000000
      YSamplingScale.Step = 0.250000000000000000
      OnGetHeight = HeightField1GetHeight
    end
    object DC_LightHolder: TGLDummyCube
      Position.Coordinates = {0000000000000000333333400000803F}
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {CDCC4C3E000000000000003F0000803F}
        SpotCutOff = 180.000000000000000000
        object Cube1: TGLCube
          Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
          Position.Coordinates = {CDCC4CBE00000000000000000000803F}
          Visible = False
          CubeSize = {CDCC4C3ECDCC4C3ECDCC4C3E}
        end
        object Sphere1: TGLSphere
          Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Position.Coordinates = {CDCCCCBD00000000000000000000803F}
          Radius = 0.050000000745058060
        end
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Marble'
      Height = 30.000000000000000000
      Width = 30.000000000000000000
      XTiles = 16
      YTiles = 16
      Style = [psTileTexture]
      ShadowingObject = DC_Shadowing
      ShadowedLight = GLLightSource1
      ShadowOptions = [spoUseStencil]
    end
    object DC_Shadowing: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DC_Shadowing
      Position.Coordinates = {0000C0400000C0400000C0400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Wheel'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Marble'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Solstickan'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.MappingSCoordinates.Coordinates = {000000000000803F0000000000000000}
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {0000403F0000803F0000803F00000000}
        Texture2Name = 'Solstickan'
      end
      item
        Name = 'Arrow'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 320
    Top = 128
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 120
    Top = 16
  end
end
