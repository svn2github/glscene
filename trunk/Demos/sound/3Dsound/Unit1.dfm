object Form1: TForm1
  Left = 220
  Top = 90
  Width = 443
  Height = 387
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 66
    Width = 435
    Height = 265
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aaNone
    Align = alClient
  end
  object TrackBar: TTrackBar
    Left = 0
    Top = 331
    Width = 435
    Height = 27
    Align = alBottom
    Max = 180
    Min = -180
    Orientation = trHorizontal
    PageSize = 45
    Frequency = 45
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBoth
    TickStyle = tsAuto
    OnChange = TrackBarChange
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 33
    Width = 435
    Height = 33
    Align = alTop
    Max = 50
    Min = -50
    Orientation = trHorizontal
    PageSize = 45
    Frequency = 10
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmBoth
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 435
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      Left = 0
      Top = 8
      Width = 82
      Height = 13
      Caption = 'Sound Manager :'
    end
    object RBBass: TRadioButton
      Left = 96
      Top = 8
      Width = 57
      Height = 17
      Caption = 'BASS'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBFMODClick
    end
    object RBFMOD: TRadioButton
      Left = 152
      Top = 8
      Width = 57
      Height = 17
      Caption = 'FMOD'
      TabOrder = 1
      OnClick = RBFMODClick
    end
    object Button1: TButton
      Left = 352
      Top = 4
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Chimes (WAV)'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 264
      Top = 4
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Howl (MP3)'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object GLSMFMOD: TGLSMFMOD
    MasterVolume = 1
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Left = 8
    Top = 144
  end
  object GLSMBASS: TGLSMBASS
    Active = True
    MaxChannels = 32
    MasterVolume = 1
    Listener = Mickey
    Sources = <>
    Cadencer = GLCadencer1
    Algorithm3D = algFull
    Left = 40
    Top = 144
  end
  object GLSoundLibrary: TGLSoundLibrary
    Samples = <>
    Left = 8
    Top = 104
  end
  object GLScene: TGLScene
    Left = 8
    Top = 64
    object DummyCube: TGLDummyCube
      CubeSize = 1
      object Torus1: TGLTorus
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Scale.Coordinates = {0000803F0000803F0000003F00000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {000000008180803E8180003F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        MajorRadius = 5
        MinorRadius = 0.100000001490116
        Rings = 16
        Sides = 3
      end
      object Mickey: TGLSphere
        Position.Coordinates = {000000000000003F000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
        Slices = 9
        Stacks = 9
        object Sphere2: TGLSphere
          Position.Coordinates = {CDCCCC3ECDCC4C3E000000000000803F}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          Radius = 0.300000011920929
          Slices = 6
          Stacks = 6
        end
        object Sphere3: TGLSphere
          Position.Coordinates = {CDCCCCBECDCC4C3E000000000000803F}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          Radius = 0.300000011920929
          Slices = 6
          Stacks = 6
        end
        object Cone1: TGLCone
          Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
          Position.Coordinates = {00000000000000000000003F0000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          BottomRadius = 0.300000011920929
          Height = 0.5
          Slices = 8
          Stacks = 2
          Parts = [coSides]
        end
      end
      object Plane1: TGLPlane
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000000BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Height = 10
        Width = 0.5
      end
    end
    object Sphere: TGLSphere
      Position.Coordinates = {000000400000003F000000000000803F}
      OnProgress = SphereProgress
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {8180003F00000000000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.5
      Slices = 9
      Stacks = 9
      BehavioursData = {
        0201061054474C42536F756E64456D6974746572020002000200020005000000
        0000000080FF3F050000000000000080FF3F0500000000000000C80540050000
        0000000000B407400500000000000000B407400500000000000000000000060E
        474C536F756E644C696272617279060C6472756D6C6F6F702E776176080803E7
        0309}
      object Disk1: TGLDisk
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Position.Coordinates = {00000000000080BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        OuterRadius = 0.5
        Slices = 12
        SweepAngle = 360
      end
    end
    object GLLightSource: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Plane1
      Position.Coordinates = {000000400000A040000020410000803F}
      Left = 256
      Top = 160
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    SleepLength = 1
    Left = 8
    Top = 184
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 8
    Top = 224
  end
end
