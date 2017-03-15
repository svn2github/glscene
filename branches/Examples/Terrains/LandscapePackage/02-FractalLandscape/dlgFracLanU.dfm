object dlgFracLan: TdlgFracLan
  Left = 242
  Top = 106
  Caption = 'FractalLandscape'
  ClientHeight = 496
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DummyTrackbar: TTrackBar
    Left = 240
    Top = 192
    Width = 150
    Height = 45
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 351
    Width = 685
    Height = 120
    ActivePage = TabSheet3
    Align = alBottom
    HotTrack = True
    TabOrder = 0
    OnChange = PageControl1Change
    ExplicitTop = 356
    ExplicitWidth = 651
    object TabSheet1: TTabSheet
      Caption = 'Topography'
      ExplicitWidth = 643
      object GroupBox1: TGroupBox
        Left = 193
        Top = 0
        Width = 173
        Height = 92
        Align = alLeft
        Caption = 'Sea parameters'
        TabOrder = 0
        object Label2: TLabel
          Left = 24
          Top = 40
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Caption = '&Sea level:'
          FocusControl = tbRoughness
        end
        object Label21: TLabel
          Left = 3
          Top = 64
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = '&Transparency:'
          FocusControl = tbRoughness
        end
        object tbSeaLevel: TTrackBar
          Left = 72
          Top = 40
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 3
          TabOrder = 0
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
        object ckSea: TCheckBox
          Left = 16
          Top = 16
          Width = 73
          Height = 17
          Hint = 'Every vertices below sea level are levelled up'
          Alignment = taLeftJustify
          Caption = 'Sea sur&face'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = OnTopographyChanged
        end
        object tbTransparency: TTrackBar
          Left = 72
          Top = 64
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 5
          TabOrder = 2
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 193
        Height = 92
        Align = alLeft
        Caption = 'Fractal parameters'
        TabOrder = 1
        object Label3: TLabel
          Left = 8
          Top = 16
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = '&Depth:'
          FocusControl = seDepth
        end
        object Label4: TLabel
          Left = 7
          Top = 40
          Width = 57
          Height = 13
          Alignment = taRightJustify
          Caption = '&Roughness:'
          FocusControl = tbRoughness
        end
        object Label11: TLabel
          Left = 108
          Top = 16
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Seed:'
          FocusControl = seSeed
        end
        object Label13: TLabel
          Left = 15
          Top = 64
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Caption = '&Amplitude:'
          FocusControl = tbAmplitude
        end
        object seDepth: TSpinEdit
          Left = 40
          Top = 16
          Width = 57
          Height = 22
          Hint = 
            'Resolution of the height-field (parameter of the fractal algorit' +
            'hm)'
          MaxValue = 10
          MinValue = 3
          TabOrder = 0
          Value = 6
          OnChange = OnTopographyChanged
        end
        object tbRoughness: TTrackBar
          Left = 64
          Top = 40
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 4
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
        object seSeed: TSpinEdit
          Left = 136
          Top = 16
          Width = 49
          Height = 22
          Hint = 
            'Seed of the random generator. Each seed produces a different lan' +
            'dscape'
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 4
          OnChange = OnTopographyChanged
        end
        object tbAmplitude: TTrackBar
          Left = 64
          Top = 64
          Width = 97
          Height = 25
          HelpContext = 45
          Max = 200
          Min = 1
          Frequency = 10
          Position = 60
          TabOrder = 3
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
      end
      object GroupBox12: TGroupBox
        Left = 366
        Top = 0
        Width = 171
        Height = 92
        Align = alLeft
        Caption = 'Tile parameters'
        TabOrder = 2
        object Label18: TLabel
          Left = 8
          Top = 32
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale &X:'
          FocusControl = tbRoughness
        end
        object lblScaleY: TLabel
          Left = 8
          Top = 48
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale &Y:'
          Enabled = False
          FocusControl = tbRoughness
        end
        object lblScaleZ: TLabel
          Left = 8
          Top = 64
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale &Z:'
          Enabled = False
          FocusControl = tbRoughness
        end
        object tbScaleX: TTrackBar
          Left = 48
          Top = 32
          Width = 121
          Height = 25
          HelpContext = 45
          Max = 20
          Min = 1
          Position = 4
          TabOrder = 0
          ThumbLength = 10
          OnChange = OnScaleChanged
        end
        object ckCyclic: TCheckBox
          Left = 16
          Top = 16
          Width = 49
          Height = 17
          Hint = 'The landscape wrap on itself endlessly'
          Alignment = taLeftJustify
          Caption = '&Cyclic'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = OnTopographyChanged
        end
        object tbScaleY: TTrackBar
          Left = 48
          Top = 48
          Width = 121
          Height = 25
          HelpContext = 45
          Enabled = False
          Max = 20
          Min = 1
          Position = 4
          TabOrder = 2
          ThumbLength = 10
          OnChange = OnScaleChanged
        end
        object tbScaleZ: TTrackBar
          Left = 48
          Top = 64
          Width = 121
          Height = 25
          HelpContext = 45
          Enabled = False
          Max = 20
          Min = 1
          Position = 4
          TabOrder = 3
          ThumbLength = 10
          OnChange = OnScaleChanged
        end
        object ckIsometric: TCheckBox
          Left = 96
          Top = 16
          Width = 65
          Height = 17
          Alignment = taLeftJustify
          Caption = '&Isometric'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = ckIsometricClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Erosion'
      ImageIndex = 2
      ExplicitWidth = 643
      object GroupBox5: TGroupBox
        Left = 0
        Top = 0
        Width = 169
        Height = 92
        Align = alLeft
        Caption = 'Sea erosion'
        TabOrder = 0
        object Label8: TLabel
          Left = 6
          Top = 48
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = '&Beach height:'
          FocusControl = tbBeachHeight
        end
        object ckSeaErosion: TCheckBox
          Left = 3
          Top = 16
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnTopographyChanged
        end
        object tbBeachHeight: TTrackBar
          Left = 72
          Top = 48
          Width = 89
          Height = 25
          HelpContext = 45
          Min = 1
          Position = 2
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
      end
      object GroupBox6: TGroupBox
        Left = 169
        Top = 0
        Width = 184
        Height = 92
        Align = alLeft
        Caption = 'Rain erosion'
        TabOrder = 1
        object Label9: TLabel
          Left = 29
          Top = 40
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = '&Erosion rate:'
          FocusControl = tbErosionRate
        end
        object Label10: TLabel
          Left = 14
          Top = 64
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = '&Deposition rate:'
          FocusControl = tbDepositionRate
        end
        object ckRainErosion: TCheckBox
          Left = 8
          Top = 16
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnTopographyChanged
        end
        object tbErosionRate: TTrackBar
          Left = 88
          Top = 40
          Width = 89
          Height = 25
          HelpContext = 45
          Position = 2
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
        object tbDepositionRate: TTrackBar
          Left = 88
          Top = 64
          Width = 89
          Height = 25
          HelpContext = 45
          Position = 2
          TabOrder = 2
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
      end
      object GroupBox7: TGroupBox
        Left = 353
        Top = 0
        Width = 104
        Height = 92
        Align = alLeft
        Caption = 'Life erosion'
        TabOrder = 2
        object Label12: TLabel
          Left = 13
          Top = 40
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = '&Robustness:'
          FocusControl = tbRobustness
        end
        object ckLifeErosion: TCheckBox
          Left = 8
          Top = 16
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnTopographyChanged
        end
        object tbRobustness: TTrackBar
          Left = 7
          Top = 56
          Width = 90
          Height = 25
          HelpContext = 45
          Max = 5
          Min = -2
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnTopographyChanged
        end
      end
      object GroupBox14: TGroupBox
        Left = 457
        Top = 0
        Width = 220
        Height = 92
        Align = alClient
        Caption = 'Steps'
        TabOrder = 3
        ExplicitWidth = 186
        object Label26: TLabel
          Left = 6
          Top = 40
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'Step count:'
          FocusControl = seStepCount
        end
        object ckStepped: TCheckBox
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Hint = 'Build a stepped landscape'
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = OnTopographyChanged
        end
        object seStepCount: TSpinEdit
          Left = 60
          Top = 40
          Width = 49
          Height = 22
          MaxValue = 100
          MinValue = 3
          TabOrder = 1
          Value = 10
          OnChange = OnTopographyChanged
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Lighting'
      ImageIndex = 1
      ExplicitWidth = 643
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 169
        Height = 92
        Align = alLeft
        Caption = 'Shadows'
        TabOrder = 0
        object Label1: TLabel
          Left = 10
          Top = 64
          Width = 54
          Height = 13
          Alignment = taRightJustify
          Caption = 'Sun &height:'
          FocusControl = tbSunHeight
        end
        object ckLighting: TCheckBox
          Left = 8
          Top = 16
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnLightChanged
        end
        object ckShadows: TCheckBox
          Left = 8
          Top = 40
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Shadows'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = OnLightChanged
        end
        object tbSunHeight: TTrackBar
          Left = 64
          Top = 64
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 3
          TabOrder = 2
          ThumbLength = 10
          OnChange = OnLightChanged
        end
      end
      object GroupBox4: TGroupBox
        Left = 169
        Top = 0
        Width = 152
        Height = 92
        Align = alLeft
        Caption = 'Light'
        TabOrder = 1
        object Label5: TLabel
          Left = 6
          Top = 24
          Width = 42
          Height = 13
          Alignment = taRightJustify
          Caption = 'Intensity:'
          FocusControl = tbIntensity
        end
        object Label6: TLabel
          Left = 7
          Top = 48
          Width = 41
          Height = 13
          Alignment = taRightJustify
          Caption = '&Ambient:'
          FocusControl = tbAmbient
        end
        object Label7: TLabel
          Left = 21
          Top = 72
          Width = 27
          Height = 13
          Alignment = taRightJustify
          Caption = 'Color:'
          FocusControl = tbAmbient
          Visible = False
        end
        object shColor: TShape
          Left = 54
          Top = 70
          Width = 81
          Height = 17
          Pen.Style = psClear
          Visible = False
          OnMouseUp = shColorMouseUp
        end
        object tbIntensity: TTrackBar
          Left = 48
          Top = 24
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 10
          TabOrder = 0
          ThumbLength = 10
          OnChange = OnLightChanged
        end
        object tbAmbient: TTrackBar
          Left = 48
          Top = 48
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 5
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnLightChanged
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Texture'
      ImageIndex = 3
      ExplicitWidth = 643
      object GroupBox8: TGroupBox
        Left = 0
        Top = 0
        Width = 177
        Height = 92
        Align = alLeft
        Caption = 'Land-cover'
        TabOrder = 0
        object Label14: TLabel
          Left = 42
          Top = 40
          Width = 30
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale:'
          FocusControl = tbTextureScale
        end
        object ckTexture: TCheckBox
          Left = 16
          Top = 16
          Width = 73
          Height = 17
          Hint = 
            'Compute a land-cover for each pixel according to its elevation a' +
            'nd slope'
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnLightChanged
        end
        object tbTextureScale: TTrackBar
          Left = 72
          Top = 40
          Width = 97
          Height = 25
          Hint = 'Resolution of the land-cover'
          HelpContext = 45
          Max = 4
          PageSize = 1
          Position = 2
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnLightChanged
        end
      end
      object grpStrata: TGroupBox
        Left = 177
        Top = 0
        Width = 177
        Height = 92
        Align = alLeft
        Caption = 'Topography strata'
        TabOrder = 1
        object Label15: TLabel
          Left = 5
          Top = 24
          Width = 67
          Height = 13
          Alignment = taRightJustify
          Caption = 'Snow altitude:'
          FocusControl = tbSnow
        end
        object Label16: TLabel
          Left = 13
          Top = 48
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Steep slope:'
          FocusControl = tbSteep
        end
        object tbSnow: TTrackBar
          Left = 72
          Top = 24
          Width = 97
          Height = 25
          HelpContext = 45
          Position = 9
          TabOrder = 0
          ThumbLength = 10
          OnChange = OnLightChanged
        end
        object tbSteep: TTrackBar
          Left = 72
          Top = 48
          Width = 97
          Height = 25
          HelpContext = 45
          Max = 90
          Frequency = 5
          Position = 55
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnLightChanged
        end
      end
      object rgTextureQuality: TRadioGroup
        Left = 354
        Top = 0
        Width = 103
        Height = 92
        Align = alLeft
        Caption = 'Land-cover quality'
        ItemIndex = 3
        Items.Strings = (
          'Basic'
          'Flashy'
          'Slopes'
          'Realistic')
        TabOrder = 2
        OnClick = OnLightChanged
      end
      object grpDefaultTexture: TGroupBox
        Left = 457
        Top = 0
        Width = 192
        Height = 92
        Align = alLeft
        Caption = 'Default texture'
        TabOrder = 3
        Visible = False
        object Label27: TLabel
          Left = 9
          Top = 16
          Width = 45
          Height = 13
          Caption = 'Filename:'
          FocusControl = btDefaultTexture
        end
        object edDefaultTexture: TEdit
          Left = 8
          Top = 32
          Width = 153
          Height = 21
          ReadOnly = True
          TabOrder = 0
          Text = 'nature073-Terre+Herbe.jpg'
          OnDblClick = btDefaultTextureClick
        end
        object btDefaultTexture: TButton
          Left = 168
          Top = 32
          Width = 17
          Height = 17
          Caption = '...'
          TabOrder = 1
          OnClick = btDefaultTextureClick
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'View'
      ImageIndex = 4
      ExplicitWidth = 643
      object GroupBox11: TGroupBox
        Left = 0
        Top = 0
        Width = 177
        Height = 92
        Align = alLeft
        Caption = 'Camera'
        TabOrder = 0
        object Label19: TLabel
          Left = 11
          Top = 24
          Width = 61
          Height = 13
          Alignment = taRightJustify
          Caption = 'Focal length:'
          FocusControl = tbFocalLength
        end
        object Label20: TLabel
          Left = 3
          Top = 48
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Caption = 'Depth of &view:'
          FocusControl = tbDepthOfView
        end
        object tbFocalLength: TTrackBar
          Left = 72
          Top = 24
          Width = 97
          Height = 25
          HelpContext = 45
          Max = 100
          Min = 20
          Frequency = 10
          Position = 50
          TabOrder = 0
          ThumbLength = 10
          OnChange = OnCameraChanged
        end
        object tbDepthOfView: TTrackBar
          Left = 72
          Top = 48
          Width = 97
          Height = 25
          HelpContext = 45
          Max = 30
          Min = 1
          Frequency = 5
          Position = 15
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnCameraChanged
        end
      end
      object GroupBox10: TGroupBox
        Left = 177
        Top = 0
        Width = 177
        Height = 92
        Align = alLeft
        Caption = 'Fog'
        TabOrder = 1
        object Label17: TLabel
          Left = 47
          Top = 40
          Width = 25
          Height = 13
          Alignment = taRightJustify
          Caption = '&Start:'
          FocusControl = tbFogStart
        end
        object ckFog: TCheckBox
          Left = 16
          Top = 16
          Width = 73
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = OnCameraChanged
        end
        object tbFogStart: TTrackBar
          Left = 72
          Top = 40
          Width = 97
          Height = 25
          HelpContext = 45
          Min = 1
          Position = 5
          TabOrder = 1
          ThumbLength = 10
          OnChange = OnCameraChanged
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Credits'
      ImageIndex = 5
      ExplicitWidth = 643
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 265
        Height = 92
        Align = alLeft
        Lines.Strings = (
          '(c) by Alexandre Hirzel, 2003'
          'Freeware and open-source, can be freely redistributed.'
          'Based on GLScene, by Eric Grange at '
          'www.glscene.org'
          'Textures from Laurent ANTOINE "Lemog" at '
          'www.3dtextures.fr.st'
          'Many thanks to all GLSceners.')
        TabOrder = 0
      end
      object GroupBox13: TGroupBox
        Left = 265
        Top = 0
        Width = 176
        Height = 92
        Align = alLeft
        Caption = 'Miscellaneous'
        TabOrder = 1
        object Label22: TLabel
          Left = 32
          Top = 16
          Width = 40
          Height = 13
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          Alignment = taRightJustify
          Caption = 'Effect 1:'
          FocusControl = TrackBar2
        end
        object Label23: TLabel
          Left = 32
          Top = 32
          Width = 40
          Height = 13
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          Alignment = taRightJustify
          Caption = 'Effect 2:'
          FocusControl = TrackBar1
        end
        object Label24: TLabel
          Left = 32
          Top = 64
          Width = 40
          Height = 13
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          Alignment = taRightJustify
          Caption = 'Effect 4:'
          FocusControl = TrackBar4
        end
        object Label25: TLabel
          Left = 32
          Top = 48
          Width = 40
          Height = 13
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          Alignment = taRightJustify
          Caption = 'Effect 3:'
          FocusControl = TrackBar3
        end
        object TrackBar1: TTrackBar
          Left = 72
          Top = 32
          Width = 97
          Height = 25
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          HelpContext = 45
          Max = 30
          Min = 1
          Frequency = 5
          Position = 15
          TabOrder = 0
          ThumbLength = 10
          OnChange = TrackBar2Change
        end
        object TrackBar2: TTrackBar
          Left = 72
          Top = 16
          Width = 97
          Height = 17
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          HelpContext = 45
          Max = 100
          Min = 20
          Frequency = 10
          Position = 50
          TabOrder = 1
          ThumbLength = 10
          OnChange = TrackBar2Change
        end
        object TrackBar3: TTrackBar
          Left = 72
          Top = 48
          Width = 97
          Height = 25
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          HelpContext = 45
          Max = 100
          Min = 20
          Frequency = 10
          Position = 50
          TabOrder = 2
          ThumbLength = 10
          OnChange = TrackBar2Change
        end
        object TrackBar4: TTrackBar
          Left = 72
          Top = 64
          Width = 97
          Height = 25
          Hint = 'Useless trackbars, but I had to beat Ivan'#39's trackbar density'
          HelpContext = 45
          Max = 30
          Min = 1
          Frequency = 5
          Position = 15
          TabOrder = 3
          ThumbLength = 10
          OnChange = TrackBar2Change
        end
      end
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 685
    Height = 351
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {FCA9313F9CC4603F91ED7C3F0000803F}
    Buffer.FogEnvironment.FogStart = 400.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1200.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clSkyBlue
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 148.195465087890600000
    OnMouseEnter = GLSceneViewer1MouseEnter
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 2
    ExplicitHeight = 350
  end
  object Panel1: TPanel
    Left = 0
    Top = 471
    Width = 685
    Height = 25
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 476
    ExplicitWidth = 651
    DesignSize = (
      685
      25)
    object lblDebug: TLabel
      Left = 8
      Top = 4
      Width = 3
      Height = 13
    end
    object btApply: TBitBtn
      Left = 461
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btApplyClick
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 88
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object TerrainRenderer1: TGLTerrainRenderer
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        TileSize = 32
        TilesPerTexture = 1.000000000000000000
        QualityDistance = 100.000000000000000000
        CLODPrecision = 20
        ContourWidth = 0
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1200.000000000000000000
      FocalLength = 50.000000000000000000
      Left = 264
      Top = 160
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 16
    Top = 56
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 120
  end
  object AsyncTimer1: TGLAsyncTimer
    Interval = 1
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpIdle
    Left = 16
    Top = 152
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 16
    Top = 184
  end
end
