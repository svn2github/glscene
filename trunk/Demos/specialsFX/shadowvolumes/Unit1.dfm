object Form1: TForm1
  Left = 70
  Top = 58
  Width = 703
  Height = 485
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 560
    Height = 456
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
  end
  object Panel1: TPanel
    Left = 560
    Top = 0
    Width = 135
    Height = 456
    Align = alRight
    BevelInner = bvLowered
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 120
      Height = 18
      Caption = 'Shadow Volumes'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CBShowVolumes: TCheckBox
      Left = 16
      Top = 192
      Width = 97
      Height = 17
      Caption = 'Show Volumes'
      TabOrder = 0
      OnClick = CBShowVolumesClick
    end
    object RBZFail: TRadioButton
      Left = 16
      Top = 88
      Width = 97
      Height = 17
      Caption = 'Z-Fail (capped)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RBZFailClick
    end
    object RBZPass: TRadioButton
      Left = 16
      Top = 112
      Width = 73
      Height = 17
      Caption = 'Z-Pass'
      TabOrder = 2
      OnClick = RBZFailClick
    end
    object RBNoShadows: TRadioButton
      Left = 16
      Top = 56
      Width = 97
      Height = 17
      Caption = 'No shadows'
      TabOrder = 3
      OnClick = RBZFailClick
    end
    object RBDarkening: TRadioButton
      Left = 16
      Top = 144
      Width = 105
      Height = 17
      Caption = 'Darkening (faked)'
      TabOrder = 4
      OnClick = RBZFailClick
    end
    object CBMainLight: TCheckBox
      Left = 16
      Top = 232
      Width = 97
      Height = 17
      Caption = 'Main Light'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CBMainLightClick
    end
    object CBBlueLight: TCheckBox
      Left = 16
      Top = 256
      Width = 97
      Height = 17
      Caption = 'Blue Light'
      TabOrder = 6
      OnClick = CBBlueLightClick
    end
    object CBRedLight: TCheckBox
      Left = 16
      Top = 280
      Width = 97
      Height = 17
      Caption = 'Red Light'
      TabOrder = 7
      OnClick = CBRedLightClick
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCLight1Turn: TGLDummyCube
      CubeSize = 1
      object DCLight1Pitch: TGLDummyCube
        CubeSize = 1
        object GLLightSource1: TGLLightSource
          Ambient.Color = {9A99193F9A99193F9A99193F0000803F}
          ConstAttenuation = 1
          Diffuse.Color = {9A99193F9A99193F9A99193F0000803F}
          Position.Coordinates = {0000000000008040000000000000803F}
          SpotCutOff = 180
          object GLSphere1: TGLSphere
            ShowAxes = True
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
            Material.MaterialOptions = [moNoLighting]
            Radius = 0.150000005960464
            Slices = 11
            Stacks = 11
          end
        end
      end
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object GLShadowVolume: TGLShadowVolume
      Casters = <
        item
          Caster = GLLightSource1
          Capping = svcAlways
        end
        item
          Caster = GLLightSource2
        end
        item
          Caster = GLLightSource3
        end>
      object GLPlane1: TGLPlane
        Position.Coordinates = {0000000000000000000020C10000803F}
        Material.FrontProperties.Ambient.Color = {00000000000000001283003F0000803F}
        Height = 20
        Width = 20
        XTiles = 9
        YTiles = 9
        Style = [psTileTexture]
      end
      object GLPlane2: TGLPlane
        Direction.Coordinates = {0000803F000000000000000000000000}
        Position.Coordinates = {000020C100000000000000000000803F}
        Up.Coordinates = {00000000FFFF7F3F0000008000000000}
        Material.FrontProperties.Ambient.Color = {D7A3703E00000000000000000000803F}
        Height = 20
        Width = 20
        XTiles = 9
        YTiles = 9
        Style = [psTileTexture]
      end
      object GLPlane3: TGLPlane
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000000020C1000000000000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Material.FrontProperties.Ambient.Color = {000000001283803E000000000000803F}
        Height = 20
        Width = 20
        XTiles = 9
        YTiles = 9
        Style = [psTileTexture]
      end
      object GLFreeForm: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {295C8F3D295C8F3D295C8F3D00000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
      end
    end
    object DCLight2: TGLDummyCube
      Direction.Coordinates = {00000000431DAF3EB28F703F00000000}
      PitchAngle = 10
      Up.Coordinates = {00000000B38F703F441DAFBE00000000}
      CubeSize = 1
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1
        Diffuse.Color = {00000000000000000000803F0000803F}
        QuadraticAttenuation = 0.00999999977648258
        Position.Coordinates = {00000000000000000000A0C00000803F}
        LightStyle = lsOmni
        Shining = False
        SpotCutOff = 180
        object GLSphere2: TGLSphere
          Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
          Material.MaterialOptions = [moNoLighting]
          Radius = 0.150000005960464
          Slices = 9
          Stacks = 9
        end
      end
    end
    object DCLight3: TGLDummyCube
      Direction.Coordinates = {00000000D3D031BE5D1C7C3F00000000}
      PitchAngle = -5
      Up.Coordinates = {000000005C1C7C3FD3D0313E00000000}
      CubeSize = 1
      object GLLightSource3: TGLLightSource
        ConstAttenuation = 1
        Diffuse.Color = {0000803F00000000000000000000803F}
        QuadraticAttenuation = 0.00999999977648258
        Position.Coordinates = {000000000000A040000000000000803F}
        Shining = False
        SpotCutOff = 180
        object GLSphere3: TGLSphere
          Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
          Material.MaterialOptions = [moNoLighting]
          Radius = 0.150000005960464
        end
      end
    end
    object DCSpheres: TGLDummyCube
      CubeSize = 1
    end
    object GLCamera: TGLCamera
      DepthOfView = 1.00000001504747E30
      FocalLength = 50
      TargetObject = GLDummyCube1
      CameraStyle = csInfinitePerspective
      Position.Coordinates = {0000E0400000A040000080400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 96
    Top = 16
  end
end
