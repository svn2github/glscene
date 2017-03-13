object frmBuoyancy: TfrmBuoyancy
  Left = 359
  Top = 199
  Caption = 'Buoyancy'
  ClientHeight = 693
  ClientWidth = 913
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
  DesignSize = (
    913
    693)
  PixelsPerInch = 96
  TextHeight = 13
  object Label_FPS: TLabel
    Left = 8
    Top = 8
    Width = 23
    Height = 13
    Caption = '0 fps'
  end
  object Label_Density: TLabel
    Left = 504
    Top = 0
    Width = 65
    Height = 13
    Caption = 'Water density'
  end
  object Label_Submerged: TLabel
    Left = 272
    Top = 32
    Width = 63
    Height = 13
    Caption = '% submerged'
  end
  object Label_DragConstant: TLabel
    Left = 616
    Top = 0
    Width = 50
    Height = 13
    Caption = 'Drag coeff'
  end
  object Label1: TLabel
    Left = 728
    Top = 0
    Width = 47
    Height = 13
    Caption = 'Tilt ocean'
  end
  object Label_Comment: TLabel
    Left = 8
    Top = 32
    Width = 110
    Height = 13
    Caption = 'No comment for this file'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 48
    Width = 894
    Height = 640
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.AmbientColor.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 162.238677978515600000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object ScrollBar_WaterDensity: TScrollBar
    Left = 504
    Top = 16
    Width = 105
    Height = 17
    Max = 1000
    PageSize = 0
    Position = 300
    TabOrder = 1
    OnChange = ScrollBar_WaterDensityChange
  end
  object ScrollBar_Drag: TScrollBar
    Left = 616
    Top = 16
    Width = 105
    Height = 17
    Max = 1000
    PageSize = 0
    Position = 150
    TabOrder = 2
    OnChange = ScrollBar_DragChange
  end
  object Button_Lift: TButton
    Left = 272
    Top = 8
    Width = 57
    Height = 21
    Caption = 'Lift body'
    TabOrder = 3
    OnClick = Button_LiftClick
  end
  object ScrollBar_TiltOcean: TScrollBar
    Left = 728
    Top = 16
    Width = 105
    Height = 17
    Min = -100
    PageSize = 0
    TabOrder = 4
    OnChange = ScrollBar_TiltOceanChange
  end
  object CheckBox_StormySeas: TCheckBox
    Left = 840
    Top = 8
    Width = 57
    Height = 17
    Caption = '&Waves'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox_StormySeasClick
  end
  object Button_LoadBobbyFile: TButton
    Left = 336
    Top = 8
    Width = 57
    Height = 21
    Caption = 'Load File'
    TabOrder = 6
    OnClick = Button_LoadBobbyFileClick
  end
  object Button_Clear: TButton
    Left = 393
    Top = 8
    Width = 32
    Height = 21
    Caption = 'Clear'
    TabOrder = 7
    OnClick = Button_ClearClick
  end
  object Button_RainCubes: TButton
    Left = 432
    Top = 8
    Width = 65
    Height = 21
    Caption = 'Rain cubes'
    TabOrder = 8
    OnClick = Button_RainCubesClick
  end
  object CheckBox_StepFast: TCheckBox
    Left = 840
    Top = 24
    Width = 73
    Height = 17
    Caption = '&QuickStep'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = CheckBox_StormySeasClick
  end
  object CheckBox_Shadows: TCheckBox
    Left = 432
    Top = 32
    Width = 65
    Height = 14
    Caption = '&Shadows'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = CheckBox_ShadowsClick
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 168
    Top = 72
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 64
    object GLShadowVolume1: TGLShadowVolume
      Lights = <
        item
          LightSource = GLLightSource1
        end>
      Occluders = <
        item
          Caster = GLCube1
        end
        item
          Caster = GLCube2
        end
        item
          Caster = GLCube3
        end
        item
          Caster = GLCube4
        end>
      Options = [svoScissorClips, svoWorldScissorClip, svoDesignVisible]
      DarkeningColor.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
      object DC_MirrorAndReflect: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCube1: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'WallMaterialName'
          Position.Coordinates = {00008040000000000000803E0000803F}
          CubeSize = {CDCC4C3E0000004100002040}
        end
        object GLCube2: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'WallMaterialName'
          Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
          Position.Coordinates = {00000000000080400000803E0000803F}
          RollAngle = 90.000000000000000000
          Up.Coordinates = {FFFF7FBF2CBD3BB30000000000000000}
          CubeSize = {CDCC4C3E0000004100002040}
        end
        object GLCube3: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'WallMaterialName'
          Position.Coordinates = {000080C0000000000000803E0000803F}
          CubeSize = {CDCC4C3E0000004100002040}
        end
        object GLCube4: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'WallMaterialName'
          Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
          Position.Coordinates = {00000000000080C00000803E0000803F}
          RollAngle = 90.000000000000000000
          Up.Coordinates = {FFFF7FBF2CBD3BB30000000000000000}
          CubeSize = {CDCC4C3E0000004100002040}
        end
      end
      object OceanFloor: TGLPlane
        Material.FrontProperties.Ambient.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
        Material.FrontProperties.Diffuse.Color = {000000001283803E1283803E0000803F}
        Material.FrontProperties.Emission.Color = {000000000000000000000000CDCC4C3E}
        Position.Coordinates = {0000000000000000000080BF0000803F}
        Height = 16.000000000000000000
        Width = 16.000000000000000000
        XTiles = 6
        YTiles = 6
        Style = [psTileTexture]
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Position.Coordinates = {0000A0420000A042000048430000803F}
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object OceanLevel: TGLPlane
      Material.FrontProperties.Ambient.Color = {000000000000000077BE1F3F0000803F}
      Material.FrontProperties.Diffuse.Color = {0C02EB3E0C02EB3E0000803F0000003F}
      Material.FrontProperties.Shininess = 1
      Material.FrontProperties.Specular.Color = {3D0A173F85EBD13E52B89E3E0000803F}
      Material.BlendingMode = bmTransparency
      Material.FaceCulling = fcNoCull
      Position.Coordinates = {00000000000000000000003F0000803F}
      Up.Coordinates = {000000000000803F0000008000000000}
      Height = 8.000000000000000000
      Width = 8.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 9.999999848243207E30
      FocalLength = 50.000000000000000000
      TargetObject = DC_MirrorAndReflect
      Position.Coordinates = {0000E040000000000000A0400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 304
    Top = 64
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Ballmaterial'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {A69BC43EA69BC43EA69BC43E0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'WallMaterialName'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.FrontProperties.Diffuse.Color = {1283003F1283003F000000000000803F}
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
      end>
    Left = 168
    Top = 120
  end
  object OpenDialog_BobbyFile: TOpenDialog
    DefaultExt = '*.bob'
    FileName = '*.bob'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 64
    Top = 120
  end
end
