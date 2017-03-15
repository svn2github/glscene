object dlgFracArchip: TdlgFracArchip
  Left = 241
  Top = 123
  Caption = 'Fractal Archipelago'
  ClientHeight = 501
  ClientWidth = 618
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    618
    501)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 184
    Width = 625
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Landscape initialisation in progress. Please wait...'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 618
    Height = 476
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {FCA9313F9CC4603F91ED7C3F0000803F}
    Buffer.FogEnvironment.FogStart = 700.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1200.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clSkyBlue
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 156.271224975585900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 476
    Width = 618
    Height = 25
    Align = alBottom
    TabOrder = 0
    object lblDebug: TLabel
      Left = 8
      Top = 4
      Width = 3
      Height = 13
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 160
    Top = 24
    object GLDummyCube1: TGLDummyCube
      Scale.Coordinates = {0000E0400000E0400000E04000000000}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 800.000000000000000000
        FocalLength = 50.000000000000000000
        Direction.Coordinates = {00000000000000000000803F00000000}
        Left = 264
        Top = 160
      end
      object TerrainRenderer1: TGLTerrainRenderer
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        TileSize = 32
        TilesPerTexture = 1.000000000000000000
        MaterialLibrary = GLMaterialLibrary1
        QualityDistance = 20.000000000000000000
        CLODPrecision = 20
        ContourWidth = 0
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 48
    Top = 24
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 48
    Top = 88
  end
  object GLAsyncTimer1: TGLAsyncTimer
    OnTimer = GLAsyncTimer1Timer
    Left = 160
    Top = 88
  end
end
