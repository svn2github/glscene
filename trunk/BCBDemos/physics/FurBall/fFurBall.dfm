object frmFurBall: TfrmFurBall
  Left = 192
  Top = 114
  Width = 743
  Height = 512
  Caption = 'Fur Ball'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 110
  TextHeight = 16
  object Label1: TLabel
    Left = 402
    Top = 2
    Width = 195
    Height = 16
    Caption = '(Steer with A/D W/S UP/DOWN)'
  end
  object Label_FPS: TLabel
    Left = 746
    Top = 2
    Width = 129
    Height = 16
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'FPS'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 10
    Top = 49
    Width = 862
    Height = 536
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 150.723449707031
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox_LockBall: TCheckBox
    Left = 10
    Top = 0
    Width = 80
    Height = 21
    Caption = '&Lock Ball'
    TabOrder = 1
  end
  object CheckBox_FurGravity: TCheckBox
    Left = 98
    Top = 0
    Width = 100
    Height = 21
    Caption = 'Fur &Gravity'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox_FurGravityClick
  end
  object CheckBox_WindResistence: TCheckBox
    Left = 197
    Top = 0
    Width = 129
    Height = 21
    Caption = '&Wind Resistence'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox_WindResistenceClick
  end
  object CheckBox_Bald: TCheckBox
    Left = 335
    Top = 0
    Width = 60
    Height = 21
    Caption = '&Bald'
    TabOrder = 4
    OnClick = CheckBox_BaldClick
  end
  object CheckBox_Shadows: TCheckBox
    Left = 610
    Top = 0
    Width = 80
    Height = 21
    Caption = '&Shadows'
    TabOrder = 5
    OnClick = CheckBox_ShadowsClick
  end
  object CheckBox_Inertia: TCheckBox
    Left = 10
    Top = 20
    Width = 80
    Height = 21
    Caption = 'Fur &Inertia'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CheckBox_InertiaClick
  end
  object TrackBar_WindForce: TTrackBar
    Left = 197
    Top = 20
    Width = 129
    Height = 21
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 7
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = TrackBar_WindForceChange
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.05
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 104
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 48
    object DC_LightHolder: TGLDummyCube
      Up.Coordinates = {000000000000803F0000008000000000}
      OnProgress = DC_LightHolderProgress
      CubeSize = 1
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        Position.Coordinates = {00000000000000000000A0400000803F}
        SpotCutOff = 180
        object Sphere1: TGLSphere
          Material.BackProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
          Material.BackProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
          Radius = 0.100000001490116
        end
      end
    end
    object DCShadowCaster: TGLDummyCube
      CubeSize = 1
      object FurBall: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.TextureMode = tmModulate
        Position.Coordinates = {000000000000C0BF000040400000803F}
        Radius = 0.25
      end
    end
    object GLShadowPlane_Floor: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Direction.Coordinates = {00000000B6C8CB3DC2BA7E3F00000000}
      Up.Coordinates = {00000000C2BA7E3FB6C8CBBD00000000}
      Height = 8
      Width = 8
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      NoZWrite = False
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Wall: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {000080C0000080400000803F0000803F}
      Up.Coordinates = {0000000000000000FFFF7FBF00000000}
      Height = 4
      Width = 16
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      NoZWrite = False
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Floor2: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
      Direction.Coordinates = {00000000B6C8CBBDC2BA7E3F00000000}
      Position.Coordinates = {000000000000F040000000000000803F}
      Up.Coordinates = {00000000C2BA7E3FB5C8CB3D00000000}
      Height = 10
      Width = 8
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      NoZWrite = False
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLLines1: TGLLines
      Nodes = <>
      Options = []
    end
    object GLShadowPlane_Wall2: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000000080C00000803F0000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 4
      Width = 8
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      NoZWrite = False
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLShadowPlane_Wall3: TGLShadowPlane
      Material.BackProperties.Diffuse.Color = {000000000000803F000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Position.Coordinates = {00000000000040410000803F0000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 4
      Width = 8
      XTiles = 10
      YTiles = 10
      Style = [psTileTexture]
      NoZWrite = False
      ShadowingObject = DCShadowCaster
      ShadowOptions = [spoUseStencil]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 70
      TargetObject = FurBall
      Position.Coordinates = {0000004100004040000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 136
    Top = 80
  end
end
