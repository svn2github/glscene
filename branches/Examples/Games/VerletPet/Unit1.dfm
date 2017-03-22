object Form1: TForm1
  Left = 135
  Top = 171
  Caption = 'GLVerlet Simulaci'#243'n'
  ClientHeight = 390
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 57
    Width = 568
    Height = 333
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.AmbientColor.Color = {00000000000000000000000000000000}
    FieldOfView = 146.569946289062500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 57
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 112
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 112
      Top = 29
      Width = 121
      Height = 13
      Caption = 'Nivel de energia del aura:'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Desclavar Tela'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 456
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Mostrar estructura'
      TabOrder = 1
    end
    object TrackBar1: TTrackBar
      Left = 232
      Top = 22
      Width = 185
      Height = 30
      Max = 50
      TabOrder = 2
      OnChange = TrackBar1Change
    end
    object CheckBox2: TCheckBox
      Left = 456
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Ver '#193'rboles'
      TabOrder = 3
      OnClick = CheckBox2Click
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 8
    object GLDisk1: TGLDisk
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      OuterRadius = 250.000000000000000000
      SweepAngle = 360.000000000000000000
    end
    object DummyActor: TGLDummyCube
      Position.Coordinates = {0000000000000C42000000000000803F}
      CubeSize = 1.000000000000000000
      object Actor1: TGLActor
        Direction.Coordinates = {000000000000803F0000000000000000}
        RollAngle = 90.000000000000000000
        Up.Coordinates = {2EBD3BB300000000000080BF00000000}
        AnimationMode = aamLoop
        Interval = 40
        MaterialLibrary = GLMaterialLibrary1
      end
      object Tela: TGLActor
        Material.FaceCulling = fcNoCull
        Position.Coordinates = {0000000000005C42000000000000803F}
        Interval = 100
        object GLDirectOpenGL1: TGLDirectOpenGL
          UseBuildList = False
          Blend = False
        end
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000003F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000003F}
        Material.FrontProperties.Emission.Color = {0000000000000000000000000000003F}
        Material.FrontProperties.Specular.Color = {0000000000000000000000000000003F}
        Material.BlendingMode = bmAdditive
        Material.Texture.ImageAlpha = tiaLuminance
        Material.Texture.Disabled = False
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000960640050000000000
          0000960640050000000000000096064009020008020008}
      end
    end
    object ODECube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object CentroCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object DummyCamara: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 1500.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyActor
        Position.Coordinates = {000048430000C8420000C8C10000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 40
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 48
    Top = 40
  end
  object MaterialArbol: TGLMaterialLibrary
    Left = 16
    Top = 80
  end
end
