object Form1: TForm1
  Left = 250
  Top = 176
  Width = 725
  Height = 539
  Caption = 'Quake3 Actor Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 717
    Height = 73
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Legs Animation'
    end
    object Label2: TLabel
      Left = 200
      Top = 16
      Width = 76
      Height = 13
      Caption = 'Torso Animaiton'
    end
    object Label3: TLabel
      Left = 380
      Top = 8
      Width = 70
      Height = 13
      Caption = 'Torso direction'
    end
    object Label4: TLabel
      Left = 496
      Top = 8
      Width = 26
      Height = 13
      Caption = 'Head'
    end
    object Label5: TLabel
      Left = 616
      Top = 20
      Width = 21
      Height = 13
      Caption = 'Skin'
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 32
      Width = 169
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object ComboBox2: TComboBox
      Left = 200
      Top = 32
      Width = 145
      Height = 21
      AutoComplete = False
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBox2Change
    end
    object TrackBar1: TTrackBar
      Left = 372
      Top = 24
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object TrackBar2: TTrackBar
      Left = 372
      Top = 48
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object TrackBar3: TTrackBar
      Left = 488
      Top = 24
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object TrackBar4: TTrackBar
      Left = 488
      Top = 48
      Width = 109
      Height = 21
      Max = 90
      Min = -90
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 5
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object ComboSkin: TComboBox
      Left = 612
      Top = 36
      Width = 85
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = 'Default'
      OnChange = ComboSkinChange
      Items.Strings = (
        'Default'
        'Red'
        'Blue')
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 73
    Width = 717
    Height = 439
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 80
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object GLLightSource1: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1
        Position.Coordinates = {0000204100000000000020410000803F}
        LightStyle = lsOmni
        SpotCutOff = 180
      end
      object GLCamera1: TGLCamera
        DepthOfView = 1000
        FocalLength = 400
        TargetObject = DummyCube1
        Position.Coordinates = {00009041000080410000C0400000803F}
        Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object ModelCube: TGLDummyCube
      CubeSize = 1
      object Legs: TGLActor
        Interval = 100
        MaterialLibrary = MatLib
        object Torso: TGLActor
          Interval = 100
          MaterialLibrary = MatLib
          object Head: TGLActor
            Interval = 100
            MaterialLibrary = MatLib
          end
          object Weapon: TGLActor
            Interval = 100
            MaterialLibrary = MatLib
            object GunSmoke: TGLDummyCube
              CubeSize = 1
              EffectsData = {
                0201061254474C536F7572636550465845666665637402000617474C506F696E
                744C696768745046584D616E61676572310201020008020008050000000000CD
                CCCCFA3F050000000000CDCCCCFA3F050000000000CDCCCCFA3F02000200}
            end
          end
        end
      end
    end
    object GLShadowPlane1: TGLShadowPlane
      Position.Coordinates = {0000000000000000000080BF0000803F}
      Height = 10
      Width = 10
      ShadowingObject = ModelCube
      ShadowedLight = GLLightSource1
    end
    object GLParticleFXRenderer1: TGLParticleFXRenderer
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 112
  end
  object MatLib: TGLMaterialLibrary
    Left = 40
    Top = 112
  end
  object GLPointLightPFXManager1: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Acceleration.Coordinates = {0000000000000000CDCC4C3E00000000}
    ColorMode = scmInner
    ParticleSize = 0.2
    ColorInner.Color = {0000403F0000403F0000403F0000403F}
    ColorOuter.Color = {0000403F0000403F0000403F00000000}
    LifeColors = <
      item
        LifeTime = 3
      end>
    Left = 72
    Top = 112
  end
end
