object frmQuadtreeVisCulling: TfrmQuadtreeVisCulling
  Left = 297
  Top = 155
  Width = 696
  Height = 497
  Caption = 'Quadtree Visibility Culling'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    688
    463)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 376
    Top = 10
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 32
    Width = 688
    Height = 433
    Camera = GLCamera1
    Buffer.FogEnvironment.FogStart = 3000.000000000000000000
    Buffer.FogEnvironment.FogEnd = 3950.000000000000000000
    Buffer.BackgroundColor = clWhite
    Buffer.Lighting = False
    FieldOfView = 2.687657594680786000
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object cbUseQuadtree: TCheckBox
    Left = 8
    Top = 8
    Width = 89
    Height = 17
    Caption = 'Use Quadtree'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 264
    Top = 264
    Width = 185
    Height = 49
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 82
      Height = 13
      Caption = 'Generating Trees'
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      TabOrder = 0
    end
  end
  object cbShowQuadtree: TCheckBox
    Left = 248
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Show &Quadtree'
    TabOrder = 3
    OnClick = cbShowQuadtreeClick
  end
  object cbUseExtendedFrustum: TCheckBox
    Left = 104
    Top = 8
    Width = 137
    Height = 17
    Caption = 'Use E&xtended Frustum'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object GLScene1: TGLScene
    Left = 92
    Top = 40
    object GLSkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object GLTerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = '1'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00000042000000420000004000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 32
      TilesPerTexture = 8.000000000000000000
    end
    object queryVisible: TGLDirectOpenGL
      UseBuildList = False
      OnRender = queryVisibleRender
      Blend = False
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      Blend = False
    end
    object trees: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000804000008040000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Text = '0'
      Alignment = taLeftJustify
      Layout = tlTop
    end
    object tree: TGLSprite
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageAlpha = tiaSuperBlackTransparent
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Width = 280.000000000000000000
      Height = 300.000000000000000000
      NoZWrite = False
      MirrorU = False
      MirrorV = False
    end
    object GLDirectOpenGL2: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = GLDirectOpenGL2Render
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 4000.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 180
    Top = 84
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = '1'
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Tag = 0
        Texture2Name = '2'
      end
      item
        Name = '2'
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 80
    Top = 156
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 136
    Top = 32
  end
  object GLNavigator1: TGLNavigator
    MoveUpWhenMovingForward = False
    InvertHorizontalSteeringWhenUpsideDown = False
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLCamera1
    UseVirtualUp = True
    AutoUpdateObject = True
    AngleLock = False
    Left = 292
    Top = 160
  end
  object GLUserInterface1: TGLUserInterface
    InvertMouse = False
    MouseSpeed = 12.000000000000000000
    GLNavigator = GLNavigator1
    Left = 276
    Top = 64
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 260
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 224
    Top = 200
  end
end
