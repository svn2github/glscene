object frmMain: TfrmMain
  Left = 196
  Top = 118
  Caption = 'GLS Terrain LOD'
  ClientHeight = 585
  ClientWidth = 712
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
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 712
    Height = 544
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 159.167938232421900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 712
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 120
      Top = 12
      Width = 142
      Height = 13
      Caption = 'keys WASD QE or arrow keys'
    end
    object chkWireFrame: TCheckBox
      Left = 16
      Top = 12
      Width = 91
      Height = 17
      TabStop = False
      Caption = 'WireFrame'
      TabOrder = 0
      OnClick = chkWireFrameClick
    end
    object Button1: TButton
      Left = 286
      Top = 6
      Width = 131
      Height = 25
      Caption = 'Quality Distance TileSize'
      TabOrder = 1
      TabStop = False
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 422
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Use TR scale'
      TabOrder = 2
      TabStop = False
      OnClick = Button2Click
    end
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 64
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      SpotCutOff = 180.000000000000000000
    end
    object GLTerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'plain'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000080000080BF00000000}
      HeightDataSource = GLCustomHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      MaterialLibrary = GLMaterialLibrary1
      QualityDistance = 100.000000000000000000
      CLODPrecision = 50
      OnGetTerrainBounds = GLTerrainRenderer1GetTerrainBounds
      ContourWidth = 0
    end
    object cam: TGLDummyCube
      Position.Coordinates = {0000000000004842000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = cam
        Position.Coordinates = {0000000000000040000020410000803F}
      end
    end
    object dccentre: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
    object GLCube1: TGLCube
      Position.Coordinates = {000000430000A041000000C30000803F}
      ShowAxes = True
      CubeSize = {0000A0400000A0400000A040}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'plain'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {A9A5253FB1A8283EB1A8283E0000803F}
        Material.Texture.Disabled = False
        TextureOffset.Coordinates = {0000000000000000000000000000803F}
      end
      item
        Name = 'tex1'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 34
    Top = 144
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 132
    Top = 64
  end
  object GLCustomHDS1: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDS1StartPreparingData
    Left = 134
    Top = 144
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 236
    Top = 144
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Left = 236
    Top = 64
  end
end
