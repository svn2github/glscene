object ACloudDemoForm: TACloudDemoForm
  Left = 41
  Top = 23
  Caption = 'Cloud Cube Editor'
  ClientHeight = 545
  ClientWidth = 723
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF006666
    6666666666666666666666666666666666666666666666666666666666666666
    66F666666666F6666666666666666666666F6666FF6F6666F6FFF66666666666
    6F666F6666F6F6F6666F6F666666666666F6FF66F6FF6FF6F6FFFFFF66666666
    6FF6F6666F666666FF6F6FF6F666666666F6F666FF66666FFFF6FF6666666666
    FFF66666F6FFF6FF66666F6F6F66666666FF666FFFFFFFF6FFF6666666666666
    F6F66666F66FFFFFFFF6F6666F66F6666FFF66F6F6FFFFFF66FFFF6F66666666
    66FF6FFF6FFFFFFFF66FFF666666FF66FFFFFFFFFF6666FFFF6FFFF6FF666F66
    66FFFF6FFF66666FF6FF6FFF6F66FF6666FFF6FFF666666FFF66666FF6666FF6
    666FF66FF666666FFF66F666FF666F6666FF666FF666666FF6FFF6666F666FFF
    66F6FFFFFF6666FFFF6FFFF66FF6666F6F6F6F66FFFFFFFF6F6F6F66F6F6666F
    F66FFFF66FFFFFFFF6FFFFF6F6666666FFF6FFFFF6FFFF6FFFFFFFF6F6666666
    6FFFFF6F6FF666FFFFF6FF666666666666FFF6FFFFFFF6FFFF6FFFF666666666
    666F6F6F6FF6FFFFF6F6F6F6666666666666FFF6666FF6F6FF66FFF666666666
    666666FF6FFFFF6FF666FF6666666666F66FF666FFF6F66F6666F6F666666F6F
    FFF6FFFFFFFF666F66FFFFF66666666FF6FF6FFFF66666666FFF666666666666
    6666666666666666F6F666666666666666666666666666666666666666660000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Scn: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 530
    Height = 526
    Camera = GLCamera1
    FieldOfView = 158.471450805664100000
    Align = alClient
    OnMouseDown = ScnMouseDown
    OnMouseMove = ScnMouseMove
    OnMouseUp = ScnMouseUp
    TabOrder = 0
    ExplicitWidth = 512
    ExplicitHeight = 505
  end
  object AAUIHolderPanel: TPanel
    Left = 530
    Top = 0
    Width = 193
    Height = 526
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 512
    ExplicitHeight = 505
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 193
      Height = 526
      ActivePage = TabSheet1
      Align = alClient
      MultiLine = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ExplicitHeight = 505
      object TabSheet1: TTabSheet
        Caption = 'Clouds'
        ExplicitHeight = 477
        object AddBtn: TSpeedButton
          Left = 8
          Top = 64
          Width = 49
          Height = 22
          Hint = 'at Center'
          Caption = 'Add'
          ParentShowHint = False
          ShowHint = True
          OnClick = AddBtnClick
        end
        object TotalCubesLabel: TLabel
          Left = 136
          Top = 40
          Width = 6
          Height = 13
          Hint = 'Total Cubes'
          Caption = '0'
        end
        object AddBlueBtn: TSpeedButton
          Left = 8
          Top = 88
          Width = 49
          Height = 22
          Hint = 'at Blue Cube'
          Caption = 'Add'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = AddBlueBtnClick
        end
        object AddAtLastBtn: TSpeedButton
          Left = 64
          Top = 88
          Width = 49
          Height = 22
          Hint = 'at Latest Cube'
          Caption = 'Add'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = AddAtLastBtnClick
        end
        object Label3: TLabel
          Left = 8
          Top = 176
          Width = 37
          Height = 13
          Caption = 'Position'
        end
        object Label2: TLabel
          Left = 96
          Top = 176
          Width = 40
          Height = 13
          Caption = 'Rotation'
        end
        object Label1: TLabel
          Left = 56
          Top = 176
          Width = 20
          Height = 13
          Caption = 'Size'
        end
        object NewBtn: TSpeedButton
          Left = 2
          Top = 8
          Width = 41
          Height = 22
          Hint = 'New Clear'
          Caption = 'New'
          OnClick = NewBtnClick
        end
        object LoadBtn: TSpeedButton
          Left = 47
          Top = 8
          Width = 41
          Height = 22
          Caption = 'Open'
          OnClick = LoadBtnClick
        end
        object SaveBtn: TSpeedButton
          Left = 91
          Top = 8
          Width = 41
          Height = 22
          Caption = 'Save'
          OnClick = SaveBtnClick
        end
        object Label15: TLabel
          Left = 8
          Top = 40
          Width = 120
          Height = 13
          Caption = 'Cloud Cubes [1..200]'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object GenerateBtn: TSpeedButton
          Left = 88
          Top = 302
          Width = 92
          Height = 22
          Caption = 'Generate'
          OnClick = GenerateBtnClick
        end
        object DeleteBtn: TSpeedButton
          Left = 136
          Top = 64
          Width = 41
          Height = 22
          Hint = 'Delete the LAST Cube'
          Caption = 'Delete'
          OnClick = DeleteBtnClick
        end
        object ResetBtn: TSpeedButton
          Left = 136
          Top = 88
          Width = 41
          Height = 22
          Hint = 'Text to Current Cube'
          Caption = 'Reset'
          OnClick = ResetBtnClick
        end
        object ExitBtn: TSpeedButton
          Left = 136
          Top = 8
          Width = 41
          Height = 22
          Caption = 'Exit'
          OnClick = ExitBtnClick
        end
        object Label31: TLabel
          Left = 16
          Top = 264
          Width = 68
          Height = 13
          Caption = 'Cube Group'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object CurrentCubesLabel: TLabel
          Left = 80
          Top = 64
          Width = 6
          Height = 13
          Hint = 'Current Cube'
          Caption = '0'
        end
        object RegenerateBtn: TSpeedButton
          Left = 88
          Top = 346
          Width = 92
          Height = 22
          Caption = 'Regenerate'
          OnClick = RegenerateBtnClick
        end
        object HelpBtn: TSpeedButton
          Left = 157
          Top = 479
          Width = 25
          Height = 22
          Hint = 'Help'
          Caption = '?'
          OnClick = HelpBtnClick
        end
        object Label109: TLabel
          Left = 144
          Top = 176
          Width = 31
          Height = 13
          Caption = 'Speed'
        end
        object SingleStepBtn: TSpeedButton
          Left = 164
          Top = 440
          Width = 15
          Height = 33
          Hint = 'Single Step'
          OnClick = SingleStepBtnClick
        end
        object PositionXEdit: TEdit
          Left = 8
          Top = 192
          Width = 37
          Height = 21
          Hint = 'X'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '0'
        end
        object PositionYEdit: TEdit
          Left = 8
          Top = 216
          Width = 37
          Height = 21
          Hint = 'Y'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '0'
        end
        object PositionZEdit: TEdit
          Left = 8
          Top = 240
          Width = 37
          Height = 21
          Hint = 'Z'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Text = '0'
        end
        object RollEdit: TEdit
          Left = 99
          Top = 216
          Width = 37
          Height = 21
          Hint = 'Turn'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '0'
        end
        object TurnEdit: TEdit
          Left = 99
          Top = 240
          Width = 37
          Height = 21
          Hint = 'Roll'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Text = '0'
        end
        object PitchEdit: TEdit
          Left = 99
          Top = 192
          Width = 37
          Height = 21
          Hint = 'Pitch'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Text = '0'
        end
        object WidthEdit: TEdit
          Left = 53
          Top = 192
          Width = 37
          Height = 21
          Hint = 'Width'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Text = '1.0'
        end
        object DepthYEdit: TEdit
          Left = 53
          Top = 216
          Width = 37
          Height = 21
          Hint = 'Depth'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Text = '1.0'
        end
        object HeightZEdit: TEdit
          Left = 53
          Top = 240
          Width = 37
          Height = 21
          Hint = 'Height'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          Text = '1.0'
        end
        object SpritesRG: TRadioGroup
          Left = 10
          Top = 296
          Width = 63
          Height = 73
          Caption = 'Sprites'
          ItemIndex = 0
          Items.Strings = (
            'Off'
            'On'
            'And')
          TabOrder = 9
          OnClick = SpritesRGClick
        end
        object CurrentCubeUpDown: TUpDown
          Left = 104
          Top = 58
          Width = 16
          Height = 24
          Hint = 'Current Cube Text'
          Max = 200
          TabOrder = 10
        end
        object GroupColorPanel: TPanel
          Left = 144
          Top = 264
          Width = 37
          Height = 21
          Hint = 'Group Color'
          Color = clSilver
          TabOrder = 11
          OnClick = GroupColorPanelClick
        end
        object GroupColorEdit: TEdit
          Left = 99
          Top = 264
          Width = 37
          Height = 21
          Hint = 'Depth'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
          Text = '1'
        end
        object ImageCaptureRG: TRadioGroup
          Left = 18
          Top = 425
          Width = 89
          Height = 33
          Hint = 
            'Save Image of Display : Pick a Size : NA, As is, 32bit, x2, 512 ' +
            'width'
          Caption = 'Image Capture'
          Columns = 5
          ItemIndex = 0
          Items.Strings = (
            ' None'
            ' Asis'
            ' 32x1'
            ' 32x2'
            ' 32x3')
          TabOrder = 13
          OnClick = ImageCaptureRGClick
        end
        object GridOffCB: TCheckBox
          Left = 16
          Top = 392
          Width = 68
          Height = 17
          Hint = 'Grid Off'
          Caption = 'Grid Off'
          TabOrder = 14
          OnClick = GridOffCBClick
        end
        object SpeedXEdit: TEdit
          Left = 144
          Top = 192
          Width = 37
          Height = 21
          ParentShowHint = False
          ShowHint = True
          TabOrder = 15
          Text = '1.0'
        end
        object SpeedYEdit: TEdit
          Left = 144
          Top = 216
          Width = 37
          Height = 21
          ParentShowHint = False
          ShowHint = True
          TabOrder = 16
          Text = '0.1'
        end
        object SpeedZEdit: TEdit
          Left = 144
          Top = 240
          Width = 37
          Height = 21
          ParentShowHint = False
          ShowHint = True
          TabOrder = 17
          Text = '0'
        end
        object ShapeProgressBar: TProgressBar
          Left = 88
          Top = 326
          Width = 76
          Height = 17
          Hint = 'Shape Progress'
          TabOrder = 18
        end
        object ShapeSizeSpriteCB: TCheckBox
          Left = 166
          Top = 324
          Width = 17
          Height = 17
          Hint = 'Use input Sprite Size for Shape'
          TabOrder = 19
          OnClick = TimeOnCBClick
        end
        object TimeOnCB: TCheckBox
          Left = 148
          Top = 456
          Width = 17
          Height = 17
          Hint = 'FPS Timer and Sun Movements'
          TabOrder = 20
          OnClick = TimeOnCBClick
        end
        object Panel1: TPanel
          Left = 8
          Top = 112
          Width = 169
          Height = 57
          TabOrder = 21
          object AddShapeBtn: TSpeedButton
            Left = 1
            Top = 1
            Width = 105
            Height = 22
            Hint = 'Load Shape to Fill'
            Caption = 'Add Open File ...'
            OnClick = AddShapeBtnClick
          end
          object ShapeSizeCubeCB: TCheckBox
            Left = 108
            Top = 18
            Width = 17
            Height = 17
            Hint = 'Use Input Size for Shape Cube else Default : 1'
            TabOrder = 0
            OnClick = TimeOnCBClick
          end
          object ShapeShowObjectCB: TCheckBox
            Left = 108
            Top = 3
            Width = 17
            Height = 17
            Hint = 'Show Shape object'
            TabOrder = 1
            OnClick = TimeOnCBClick
          end
          object ShapeUpxyzRG: TRadioGroup
            Left = 0
            Top = 24
            Width = 105
            Height = 33
            Caption = 'Shape Up'
            Columns = 3
            ItemIndex = 2
            Items.Strings = (
              'X'
              'Y'
              'Z')
            TabOrder = 2
          end
          object ShapeUpNegativeCB: TCheckBox
            Left = 108
            Top = 36
            Width = 17
            Height = 17
            Hint = 'Shape Up Negative'
            TabOrder = 3
            OnClick = TimeOnCBClick
          end
          object ShapeResolutionEdit: TEdit
            Left = 128
            Top = 32
            Width = 29
            Height = 21
            Hint = 'Resolution : 3... 6x6x6=216 Sprites'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            Text = '6'
          end
        end
        object ScnOnCB: TCheckBox
          Left = 148
          Top = 440
          Width = 17
          Height = 17
          Hint = 'FPS Frame Active'
          TabOrder = 22
        end
        object BlueCubeOffCB: TCheckBox
          Left = 90
          Top = 392
          Width = 92
          Height = 17
          Hint = 'Blue Cube Off'
          Caption = 'Blue Cube Off'
          TabOrder = 23
          OnClick = BlueCubeOffCBClick
        end
        object ViewerBackgroundColorPanel: TPanel
          Left = 20
          Top = 464
          Width = 88
          Height = 18
          Hint = 'Viewer Background Color'
          Caption = 'Viewer Color'
          Color = clSilver
          TabOrder = 24
          OnClick = ViewerBackgroundColorPanelClick
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Cubes'
        ImageIndex = 1
        object PageControl3: TPageControl
          Left = 0
          Top = 0
          Width = 185
          Height = 498
          ActivePage = TabSheet15
          Align = alClient
          MultiLine = True
          TabOrder = 0
          object TabSheet12: TTabSheet
            Caption = 'Size'
            object Label13: TLabel
              Left = 8
              Top = 8
              Width = 132
              Height = 13
              Caption = 'Density [1..5..30..100] '
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label10: TLabel
              Left = 48
              Top = 24
              Width = 46
              Height = 13
              Caption = '# of Parts'
            end
            object Label12: TLabel
              Left = 88
              Top = 88
              Width = 40
              Height = 13
              Caption = 'Rot Max'
            end
            object Label11: TLabel
              Left = 32
              Top = 88
              Width = 37
              Height = 13
              Caption = 'Rot Min'
            end
            object PartsEdit: TEdit
              Left = 48
              Top = 40
              Width = 49
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Text = '12'
            end
            object RotationMaxEdit: TEdit
              Left = 86
              Top = 104
              Width = 49
              Height = 21
              Hint = 'TurnAngle: 180'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              Text = '18'
            end
            object RotationMinEdit: TEdit
              Left = 32
              Top = 104
              Width = 49
              Height = 21
              Hint = 'TurnAngle: -180'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              Text = '0'
            end
            object SIZERANGEGroupBox: TGroupBox
              Left = 24
              Top = 152
              Width = 121
              Height = 145
              Caption = 'Part Size Range'
              TabOrder = 3
              object Label9: TLabel
                Left = 8
                Top = 96
                Width = 24
                Height = 13
                Caption = 'ZMin'
              end
              object Label8: TLabel
                Left = 64
                Top = 96
                Width = 27
                Height = 13
                Caption = 'ZMax'
              end
              object Label7: TLabel
                Left = 8
                Top = 56
                Width = 24
                Height = 13
                Caption = 'YMin'
              end
              object Label6: TLabel
                Left = 64
                Top = 56
                Width = 27
                Height = 13
                Caption = 'YMax'
              end
              object Label5: TLabel
                Left = 8
                Top = 16
                Width = 24
                Height = 13
                Caption = 'XMin'
              end
              object Label4: TLabel
                Left = 64
                Top = 16
                Width = 27
                Height = 13
                Caption = 'XMax'
              end
              object ZMinEdit: TEdit
                Left = 8
                Top = 112
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                Text = '0.3'
              end
              object ZMaxEdit: TEdit
                Left = 64
                Top = 112
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 1
                Text = '0.7'
              end
              object YMinEdit: TEdit
                Left = 8
                Top = 72
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 2
                Text = '0.3'
              end
              object YMaxEdit: TEdit
                Left = 64
                Top = 72
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 3
                Text = '0.7'
              end
              object XMinEdit: TEdit
                Left = 8
                Top = 32
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 4
                Text = '0.3'
              end
              object XMaxEdit: TEdit
                Left = 64
                Top = 32
                Width = 49
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 5
                Text = '0.7'
              end
            end
          end
          object TabSheet3: TTabSheet
            Caption = 'Type'
            ImageIndex = 3
            object CloudTypeRG: TRadioGroup
              Left = 24
              Top = 2
              Width = 121
              Height = 80
              Caption = 'Cloud Level Height'
              ItemIndex = 0
              Items.Strings = (
                'Stratus (Low)'
                'Alto   (Medium)'
                'Cirrus (High)'
                'Cumulus (Vertical)')
              TabOrder = 0
            end
            object CloudShapeRG: TRadioGroup
              Left = 24
              Top = 84
              Width = 121
              Height = 60
              Caption = 'Cloud Shape'
              ItemIndex = 0
              Items.Strings = (
                'Stratus [spread]'
                'Cumulus [Pile]'
                'Nimbus [Rain]')
              TabOrder = 1
            end
            object CloudPatternRG: TRadioGroup
              Left = 24
              Top = 146
              Width = 121
              Height = 100
              Caption = 'Cloud Pattern'
              ItemIndex = 0
              Items.Strings = (
                'Random  Full'
                'Linear XorY [___]'
                'Flat XxY  [_]'
                'Rounded Mass [_]'
                'Open File Shape...')
              TabOrder = 2
            end
            object SpriteTypeRG: TRadioGroup
              Left = 24
              Top = 250
              Width = 121
              Height = 200
              Caption = 'Generation Method'
              ItemIndex = 0
              Items.Strings = (
                'Individual Sprites'
                'Particle System'
                'Open File Shapes...'
                'PolyPFX Fog'
                'Image Sprites'
                'PL Mask [only 1]'
                'CSprite [only 1]'
                'Imposter [1?]'
                'Niniane Clouds'
                'Real [Slow]')
              TabOrder = 3
            end
          end
          object TabSheet15: TTabSheet
            Caption = 'Image'
            ImageIndex = 3
            object PageControl2: TPageControl
              Left = 0
              Top = 0
              Width = 177
              Height = 470
              ActivePage = TabSheet10
              Align = alClient
              MultiLine = True
              RaggedRight = True
              TabOrder = 0
              object TabSheet10: TTabSheet
                Caption = 'All'
                ImageIndex = 8
                object ValidateAndSetTexturesBtn: TSpeedButton
                  Left = 8
                  Top = 16
                  Width = 145
                  Height = 22
                  Caption = 'Validate and Set Textures'
                  OnClick = ValidateAndSetTexturesBtnClick
                end
                object PFX16SetGroupBox: TGroupBox
                  Left = 4
                  Top = 64
                  Width = 97
                  Height = 121
                  Caption = 'PFX 16 Set'
                  TabOrder = 0
                  object Label136: TLabel
                    Left = 9
                    Top = 32
                    Width = 8
                    Height = 13
                    Caption = '1'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label137: TLabel
                    Left = 9
                    Top = 51
                    Width = 8
                    Height = 13
                    Caption = '2'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label138: TLabel
                    Left = 9
                    Top = 71
                    Width = 8
                    Height = 13
                    Caption = '3'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label139: TLabel
                    Left = 9
                    Top = 90
                    Width = 8
                    Height = 13
                    Caption = '4'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label140: TLabel
                    Left = 24
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '1'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label141: TLabel
                    Left = 40
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '2'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label142: TLabel
                    Left = 56
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '3'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label143: TLabel
                    Left = 72
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '4'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Texture16_13CB: TCheckBox
                    Left = 24
                    Top = 90
                    Width = 17
                    Height = 17
                    TabOrder = 0
                  end
                  object Texture16_14CB: TCheckBox
                    Left = 40
                    Top = 90
                    Width = 17
                    Height = 17
                    TabOrder = 1
                  end
                  object Texture16_15CB: TCheckBox
                    Left = 56
                    Top = 90
                    Width = 17
                    Height = 17
                    TabOrder = 2
                  end
                  object Texture16_16CB: TCheckBox
                    Left = 72
                    Top = 90
                    Width = 17
                    Height = 17
                    TabOrder = 3
                  end
                  object Texture16_12CB: TCheckBox
                    Left = 72
                    Top = 71
                    Width = 17
                    Height = 17
                    TabOrder = 4
                  end
                  object Texture16_11CB: TCheckBox
                    Left = 56
                    Top = 71
                    Width = 17
                    Height = 17
                    TabOrder = 5
                  end
                  object Texture16_10CB: TCheckBox
                    Left = 40
                    Top = 71
                    Width = 17
                    Height = 17
                    TabOrder = 6
                  end
                  object Texture16_9CB: TCheckBox
                    Left = 24
                    Top = 71
                    Width = 17
                    Height = 17
                    TabOrder = 7
                  end
                  object Texture16_5CB: TCheckBox
                    Left = 24
                    Top = 51
                    Width = 17
                    Height = 17
                    TabOrder = 8
                  end
                  object Texture16_6CB: TCheckBox
                    Left = 40
                    Top = 51
                    Width = 17
                    Height = 17
                    TabOrder = 9
                  end
                  object Texture16_7CB: TCheckBox
                    Left = 56
                    Top = 51
                    Width = 17
                    Height = 17
                    TabOrder = 10
                  end
                  object Texture16_8CB: TCheckBox
                    Left = 72
                    Top = 51
                    Width = 17
                    Height = 17
                    TabOrder = 11
                  end
                  object Texture16_4CB: TCheckBox
                    Left = 72
                    Top = 32
                    Width = 17
                    Height = 17
                    TabOrder = 12
                  end
                  object Texture16_3CB: TCheckBox
                    Left = 56
                    Top = 32
                    Width = 17
                    Height = 17
                    TabOrder = 13
                  end
                  object Texture16_2CB: TCheckBox
                    Left = 40
                    Top = 32
                    Width = 17
                    Height = 17
                    TabOrder = 14
                  end
                  object Texture16_1CB: TCheckBox
                    Left = 24
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 15
                  end
                end
                object PFX64SetGroupBox: TGroupBox
                  Left = 4
                  Top = 200
                  Width = 161
                  Height = 193
                  Caption = 'PFX 64 Set'
                  TabOrder = 1
                  object Label120: TLabel
                    Left = 24
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '1'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label121: TLabel
                    Left = 40
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '2'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label122: TLabel
                    Left = 56
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '3'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label123: TLabel
                    Left = 72
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '4'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label124: TLabel
                    Left = 88
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '5'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label125: TLabel
                    Left = 104
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '6'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label126: TLabel
                    Left = 120
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '7'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label127: TLabel
                    Left = 136
                    Top = 20
                    Width = 8
                    Height = 13
                    Caption = '8'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label128: TLabel
                    Left = 9
                    Top = 168
                    Width = 8
                    Height = 13
                    Caption = '8'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label129: TLabel
                    Left = 9
                    Top = 149
                    Width = 8
                    Height = 13
                    Caption = '7'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label130: TLabel
                    Left = 9
                    Top = 129
                    Width = 8
                    Height = 13
                    Caption = '6'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label131: TLabel
                    Left = 9
                    Top = 110
                    Width = 8
                    Height = 13
                    Caption = '5'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label132: TLabel
                    Left = 9
                    Top = 90
                    Width = 8
                    Height = 13
                    Caption = '4'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label133: TLabel
                    Left = 9
                    Top = 71
                    Width = 8
                    Height = 13
                    Caption = '3'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label134: TLabel
                    Left = 9
                    Top = 51
                    Width = 8
                    Height = 13
                    Caption = '2'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label135: TLabel
                    Left = 9
                    Top = 32
                    Width = 8
                    Height = 13
                    Caption = '1'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Texture64_1CB: TCheckBox
                    Left = 24
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 0
                  end
                  object Texture64_2CB: TCheckBox
                    Left = 40
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 1
                  end
                  object Texture64_3CB: TCheckBox
                    Left = 56
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 2
                  end
                  object Texture64_4CB: TCheckBox
                    Left = 72
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 3
                  end
                  object Texture64_5CB: TCheckBox
                    Left = 88
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 4
                  end
                  object Texture64_6CB: TCheckBox
                    Left = 104
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 5
                  end
                  object Texture64_7CB: TCheckBox
                    Left = 120
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 6
                  end
                  object Texture64_8CB: TCheckBox
                    Left = 136
                    Top = 32
                    Width = 15
                    Height = 17
                    TabOrder = 7
                  end
                  object Texture64_9CB: TCheckBox
                    Left = 24
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 8
                  end
                  object Texture64_10CB: TCheckBox
                    Left = 40
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 9
                  end
                  object Texture64_11CB: TCheckBox
                    Left = 56
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 10
                  end
                  object Texture64_12CB: TCheckBox
                    Left = 72
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 11
                  end
                  object Texture64_13CB: TCheckBox
                    Left = 88
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 12
                  end
                  object Texture64_14CB: TCheckBox
                    Left = 104
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 13
                  end
                  object Texture64_15CB: TCheckBox
                    Left = 120
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 14
                  end
                  object Texture64_16CB: TCheckBox
                    Left = 136
                    Top = 51
                    Width = 15
                    Height = 17
                    TabOrder = 15
                  end
                  object Texture64_17CB: TCheckBox
                    Left = 24
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 16
                  end
                  object Texture64_18CB: TCheckBox
                    Left = 40
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 17
                  end
                  object Texture64_19CB: TCheckBox
                    Left = 56
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 18
                  end
                  object Texture64_20CB: TCheckBox
                    Left = 72
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 19
                  end
                  object Texture64_21CB: TCheckBox
                    Left = 88
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 20
                  end
                  object Texture64_22CB: TCheckBox
                    Left = 104
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 21
                  end
                  object Texture64_23CB: TCheckBox
                    Left = 120
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 22
                  end
                  object Texture64_24CB: TCheckBox
                    Left = 136
                    Top = 71
                    Width = 15
                    Height = 17
                    TabOrder = 23
                  end
                  object Texture64_25CB: TCheckBox
                    Left = 24
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 24
                  end
                  object Texture64_26CB: TCheckBox
                    Left = 40
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 25
                  end
                  object Texture64_27CB: TCheckBox
                    Left = 56
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 26
                  end
                  object Texture64_28CB: TCheckBox
                    Left = 72
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 27
                  end
                  object Texture64_29CB: TCheckBox
                    Left = 88
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 28
                  end
                  object Texture64_30CB: TCheckBox
                    Left = 104
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 29
                  end
                  object Texture64_31CB: TCheckBox
                    Left = 120
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 30
                  end
                  object Texture64_32CB: TCheckBox
                    Left = 136
                    Top = 90
                    Width = 15
                    Height = 17
                    TabOrder = 31
                  end
                  object Texture64_33CB: TCheckBox
                    Left = 24
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 32
                  end
                  object Texture64_34CB: TCheckBox
                    Left = 40
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 33
                  end
                  object Texture64_35CB: TCheckBox
                    Left = 56
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 34
                  end
                  object Texture64_36CB: TCheckBox
                    Left = 72
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 35
                  end
                  object Texture64_37CB: TCheckBox
                    Left = 88
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 36
                  end
                  object Texture64_38CB: TCheckBox
                    Left = 104
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 37
                  end
                  object Texture64_39CB: TCheckBox
                    Left = 120
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 38
                  end
                  object Texture64_40CB: TCheckBox
                    Left = 136
                    Top = 110
                    Width = 15
                    Height = 17
                    TabOrder = 39
                  end
                  object Texture64_41CB: TCheckBox
                    Left = 24
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 40
                  end
                  object Texture64_42CB: TCheckBox
                    Left = 40
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 41
                  end
                  object Texture64_43CB: TCheckBox
                    Left = 56
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 42
                  end
                  object Texture64_44CB: TCheckBox
                    Left = 72
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 43
                  end
                  object Texture64_45CB: TCheckBox
                    Left = 88
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 44
                  end
                  object Texture64_46CB: TCheckBox
                    Left = 104
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 45
                  end
                  object Texture64_47CB: TCheckBox
                    Left = 120
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 46
                  end
                  object Texture64_48CB: TCheckBox
                    Left = 136
                    Top = 129
                    Width = 15
                    Height = 17
                    TabOrder = 47
                  end
                  object Texture64_49CB: TCheckBox
                    Left = 24
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 48
                  end
                  object Texture64_50CB: TCheckBox
                    Left = 40
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 49
                  end
                  object Texture64_51CB: TCheckBox
                    Left = 56
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 50
                  end
                  object Texture64_52CB: TCheckBox
                    Left = 72
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 51
                  end
                  object Texture64_53CB: TCheckBox
                    Left = 88
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 52
                  end
                  object Texture64_54CB: TCheckBox
                    Left = 104
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 53
                  end
                  object Texture64_55CB: TCheckBox
                    Left = 120
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 54
                  end
                  object Texture64_56CB: TCheckBox
                    Left = 136
                    Top = 149
                    Width = 15
                    Height = 17
                    TabOrder = 55
                  end
                  object Texture64_57CB: TCheckBox
                    Left = 24
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 56
                  end
                  object Texture64_58CB: TCheckBox
                    Left = 40
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 57
                  end
                  object Texture64_59CB: TCheckBox
                    Left = 56
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 58
                  end
                  object Texture64_60CB: TCheckBox
                    Left = 72
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 59
                  end
                  object Texture64_61CB: TCheckBox
                    Left = 88
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 60
                  end
                  object Texture64_62CB: TCheckBox
                    Left = 104
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 61
                  end
                  object Texture64_63CB: TCheckBox
                    Left = 120
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 62
                  end
                  object Texture64_64CB: TCheckBox
                    Left = 136
                    Top = 168
                    Width = 15
                    Height = 17
                    TabOrder = 63
                  end
                end
              end
              object TabSheet11: TTabSheet
                Caption = 'Type'
                ImageIndex = 5
                object TextureTypeRG: TRadioGroup
                  Left = 0
                  Top = 0
                  Width = 161
                  Height = 97
                  Caption = 'Texture Combinations'
                  ItemIndex = 0
                  Items.Strings = (
                    '1 Per (24) File(s)   [ANY]'
                    '4 Per (24) File(s)   [PFX+]'
                    '16 Set per (1) file  [PFX+]'
                    '16 random (1) file  [PFX+]'
                    '64 Set per (1) file  [PFX+]'
                    '64 random (1) file  [PFX+]')
                  TabOrder = 0
                end
                object ImageTypeGetSettingsRG: TRadioGroup
                  Left = 2
                  Top = 98
                  Width = 25
                  Height = 303
                  Caption = 'Get'
                  ItemIndex = 0
                  Items.Strings = (
                    'NA'
                    '1'
                    '2'
                    '3'
                    '4'
                    '5'
                    '6'
                    '7'
                    '8'
                    '9'
                    '10'
                    '11'
                    '12'
                    '13'
                    '14'
                    '15'
                    '16'
                    '17'
                    '18'
                    '19'
                    '20'
                    '21'
                    '22'
                    '23'
                    '24')
                  TabOrder = 1
                  OnClick = ImageTypeGetSettingsRGClick
                end
                object ImageTypeSetSettingsRG: TRadioGroup
                  Left = 30
                  Top = 98
                  Width = 43
                  Height = 303
                  Caption = 'Set it'
                  ItemIndex = 0
                  Items.Strings = (
                    'NA'
                    '1'
                    '2'
                    '3'
                    '4'
                    '5'
                    '6'
                    '7'
                    '8'
                    '9'
                    '10'
                    '11'
                    '12'
                    '13'
                    '14'
                    '15'
                    '16'
                    '17'
                    '18'
                    '19'
                    '20'
                    '21'
                    '22'
                    '23'
                    '24')
                  TabOrder = 2
                  OnClick = ImageTypeSetSettingsRGClick
                end
              end
              object TabSheet13: TTabSheet
                Caption = 'Alpha'
                ImageIndex = 6
                object ImageImageAlphaRG: TRadioGroup
                  Left = 0
                  Top = 280
                  Width = 169
                  Height = 129
                  Caption = 'Image Alpha [9]'
                  ItemIndex = 0
                  Items.Strings = (
                    'tiaAlphaFromIntensity'
                    'tiaLuminance'
                    'tiaLuminanceSqrt'
                    'tiaInverseLuminance'
                    'tiaInverseLuminanceSqrt'
                    'tiaOpaque'
                    'tiaSuperBlackTransparent'
                    'tiaTopLeftPointColorTransparent'
                    'tiaDefault')
                  TabOrder = 0
                end
                object ImageAlphaGetSettingsRG: TRadioGroup
                  Left = 98
                  Top = 0
                  Width = 25
                  Height = 305
                  Caption = 'Get'
                  ItemIndex = 0
                  Items.Strings = (
                    'NA'
                    '1'
                    '2'
                    '3'
                    '4'
                    '5'
                    '6'
                    '7'
                    '8'
                    '9'
                    '10'
                    '11'
                    '12'
                    '13'
                    '14'
                    '15'
                    '16'
                    '17'
                    '18'
                    '19'
                    '20'
                    '21'
                    '22'
                    '23'
                    '24')
                  TabOrder = 1
                  OnClick = ImageAlphaGetSettingsRGClick
                end
                object ImageAlphaSetSettingsRG: TRadioGroup
                  Left = 126
                  Top = 0
                  Width = 43
                  Height = 305
                  Caption = 'Set it'
                  ItemIndex = 0
                  Items.Strings = (
                    'NA'
                    '1'
                    '2'
                    '3'
                    '4'
                    '5'
                    '6'
                    '7'
                    '8'
                    '9'
                    '10'
                    '11'
                    '12'
                    '13'
                    '14'
                    '15'
                    '16'
                    '17'
                    '18'
                    '19'
                    '20'
                    '21'
                    '22'
                    '23'
                    '24')
                  TabOrder = 2
                  OnClick = ImageAlphaSetSettingsRGClick
                end
                object ImageTextureFormatRG: TRadioGroup
                  Left = 0
                  Top = 174
                  Width = 97
                  Height = 103
                  Caption = 'Texture Format [7]'
                  ItemIndex = 0
                  Items.Strings = (
                    'RGBA'
                    'Alpha'
                    'RGBA16'
                    'Intensity'
                    'Luminance'
                    'LuminanceAlpha'
                    'Default')
                  TabOrder = 3
                end
                object ImageTextureModeRG: TRadioGroup
                  Left = 0
                  Top = 100
                  Width = 97
                  Height = 67
                  Caption = 'Texture Mode [4]'
                  ItemIndex = 0
                  Items.Strings = (
                    'Replace'
                    'Modulate'
                    'Decal'
                    'Blend')
                  TabOrder = 4
                end
                object ImageBlendingModeRG: TRadioGroup
                  Left = 0
                  Top = 0
                  Width = 97
                  Height = 97
                  Caption = 'Blending Mode [6]'
                  ItemIndex = 0
                  Items.Strings = (
                    'Transparency'
                    'Additive'
                    'AlphaTest50'
                    'AlphaTest100'
                    'Modulate'
                    'Opaque')
                  TabOrder = 5
                end
              end
              object TabSheet17: TTabSheet
                Caption = 'Mask'
                ImageIndex = 7
                PopupMenu = PLMaskPopupMenu
                object Label118: TLabel
                  Left = 114
                  Top = 299
                  Width = 35
                  Height = 13
                  Caption = 'Interval'
                end
                object Label115: TLabel
                  Left = 52
                  Top = 334
                  Width = 22
                  Height = 13
                  Caption = 'Turn'
                end
                object Label114: TLabel
                  Left = 28
                  Top = 334
                  Width = 18
                  Height = 13
                  Caption = 'Roll'
                end
                object Label113: TLabel
                  Left = 2
                  Top = 334
                  Width = 24
                  Height = 13
                  Hint = 'Angle'
                  Caption = 'Pitch'
                end
                object Label111: TLabel
                  Left = 2
                  Top = 299
                  Width = 46
                  Height = 13
                  Caption = 'Character'
                end
                object Label112: TLabel
                  Left = 52
                  Top = 299
                  Width = 29
                  Height = 13
                  Caption = 'Depth'
                end
                object Label117: TLabel
                  Left = 82
                  Top = 299
                  Width = 27
                  Height = 13
                  Caption = 'Scale'
                end
                object ZImage: TImage
                  Left = 36
                  Top = 208
                  Width = 20
                  Height = 20
                  AutoSize = True
                end
                object YImage: TImage
                  Left = 36
                  Top = 104
                  Width = 20
                  Height = 20
                  AutoSize = True
                end
                object PlLoadXBtn: TSpeedButton
                  Left = 2
                  Top = 2
                  Width = 23
                  Height = 22
                  OnClick = PlLoadXBtnClick
                end
                object XImage: TImage
                  Left = 36
                  Top = 2
                  Width = 20
                  Height = 20
                  AutoSize = True
                end
                object LabelX: TLabel
                  Left = 154
                  Top = 248
                  Width = 8
                  Height = 13
                  Caption = '3'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object LabelY: TLabel
                  Left = 153
                  Top = 264
                  Width = 8
                  Height = 13
                  Caption = '3'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object LabelZ: TLabel
                  Left = 153
                  Top = 280
                  Width = 8
                  Height = 13
                  Caption = '3'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object LabelImageSize: TLabel
                  Left = 84
                  Top = 248
                  Width = 15
                  Height = 13
                  Caption = '20'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object Label116: TLabel
                  Left = 114
                  Top = 334
                  Width = 30
                  Height = 13
                  Caption = 'P Size'
                end
                object Label119: TLabel
                  Left = 82
                  Top = 334
                  Width = 27
                  Height = 13
                  Caption = 'P Life'
                end
                object PLRegenerateDupedBtn: TSpeedButton
                  Left = 88
                  Top = 376
                  Width = 73
                  Height = 22
                  Caption = 'Regenerate'
                  OnClick = RegenerateBtnClick
                end
                object PLResetDupedBtn: TSpeedButton
                  Left = 42
                  Top = 376
                  Width = 41
                  Height = 22
                  Hint = 'Text to Current Cube'
                  Caption = 'Reset'
                  OnClick = ResetBtnClick
                end
                object PLAddDupedBtn: TSpeedButton
                  Left = 2
                  Top = 376
                  Width = 33
                  Height = 22
                  Hint = 'at Center'
                  Caption = 'Add'
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = AddBtnClick
                end
                object PLPitchEdit: TEdit
                  Left = 2
                  Top = 349
                  Width = 24
                  Height = 21
                  Hint = 'Z is up here'
                  TabOrder = 0
                  Text = '-90'
                end
                object PLRollEdit: TEdit
                  Left = 28
                  Top = 349
                  Width = 24
                  Height = 21
                  Hint = 'Stagger Lee'
                  TabOrder = 1
                  Text = '0'
                end
                object PLTurnEdit: TEdit
                  Left = 54
                  Top = 349
                  Width = 24
                  Height = 21
                  Hint = 'Front'
                  TabOrder = 2
                  Text = '0'
                end
                object PLPIntervalEdit: TEdit
                  Left = 112
                  Top = 312
                  Width = 41
                  Height = 21
                  Hint = 'Sparkle time'
                  TabOrder = 3
                  Text = '0.003'
                end
                object PLCharEdit: TEdit
                  Left = 2
                  Top = 312
                  Width = 24
                  Height = 21
                  Hint = 'Alphabet'
                  TabOrder = 4
                  Text = 'A'
                end
                object PLDepthEdit: TEdit
                  Left = 55
                  Top = 312
                  Width = 24
                  Height = 21
                  Hint = 'Layers away'
                  TabOrder = 5
                  Text = '3'
                end
                object PLCharacterSizeEdit: TEdit
                  Left = 29
                  Top = 312
                  Width = 24
                  Height = 21
                  Hint = 'Font size'
                  TabOrder = 6
                  Text = '16'
                end
                object PLScaleEdit: TEdit
                  Left = 82
                  Top = 312
                  Width = 24
                  Height = 21
                  Hint = 'Size Enlargement Device'
                  TabOrder = 7
                  Text = '1.0'
                end
                object PLMaskTypeRG: TRadioGroup
                  Left = 0
                  Top = 264
                  Width = 97
                  Height = 33
                  Hint = 'Use: Alphabet, Loaded Image, Menu maker'
                  Columns = 3
                  ItemIndex = 0
                  Items.Strings = (
                    'A'
                    'I'
                    'M')
                  TabOrder = 8
                end
                object PLMaskMaskXOffsetTB: TTrackBar
                  Left = 104
                  Top = 248
                  Width = 50
                  Height = 15
                  Hint = 'Image Height - 1..50'
                  Max = 50
                  Min = 1
                  PageSize = 10
                  Frequency = 10
                  Position = 3
                  TabOrder = 9
                  ThumbLength = 13
                  OnChange = PLMaskMaskXOffsetTBChange
                end
                object PLMaskMaskYOffsetTB: TTrackBar
                  Left = 104
                  Top = 264
                  Width = 50
                  Height = 15
                  Hint = 'Line Width'
                  Max = 50
                  Min = 1
                  PageSize = 10
                  Frequency = 10
                  Position = 3
                  TabOrder = 10
                  ThumbLength = 13
                  OnChange = PLMaskMaskYOffsetTBChange
                end
                object PLMaskMaskZOffsetTB: TTrackBar
                  Left = 104
                  Top = 280
                  Width = 50
                  Height = 15
                  Hint = 'Object Width'
                  Max = 50
                  Min = 1
                  PageSize = 10
                  Frequency = 10
                  Position = 3
                  TabOrder = 11
                  ThumbLength = 13
                  OnChange = PLMaskMaskZOffsetTBChange
                end
                object PLPSizeEdit: TEdit
                  Left = 112
                  Top = 349
                  Width = 33
                  Height = 21
                  Hint = 'Sparkle size'
                  TabOrder = 12
                  Text = '0.3'
                end
                object PLPLifeEdit: TEdit
                  Left = 80
                  Top = 349
                  Width = 24
                  Height = 21
                  Hint = 'Sparkle size'
                  TabOrder = 13
                  Text = '1.0'
                end
                object PLColorPanel: TPanel
                  Left = 150
                  Top = 349
                  Width = 16
                  Height = 21
                  Color = clAqua
                  TabOrder = 14
                  OnClick = PLColorPanelClick
                end
                object PLMaskImageSizeTB: TTrackBar
                  Left = 0
                  Top = 248
                  Width = 80
                  Height = 25
                  Hint = 'Image size: 20..100 MAX'
                  Max = 100
                  Min = 20
                  PageSize = 10
                  Frequency = 10
                  Position = 20
                  TabOrder = 15
                  ThumbLength = 17
                  OnChange = PLMaskImageSizeTBChange
                end
              end
              object TabSheet5: TTabSheet
                Caption = '1..6'
                object Label21: TLabel
                  Left = 40
                  Top = 0
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label20: TLabel
                  Left = 104
                  Top = 0
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn1: TSpeedButton
                  Left = 8
                  Top = 40
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn1Click
                end
                object Label23: TLabel
                  Left = 40
                  Top = 64
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label24: TLabel
                  Left = 104
                  Top = 64
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn2: TSpeedButton
                  Left = 8
                  Top = 104
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn2Click
                end
                object Label26: TLabel
                  Left = 40
                  Top = 128
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label25: TLabel
                  Left = 104
                  Top = 128
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn3: TSpeedButton
                  Left = 8
                  Top = 168
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn3Click
                end
                object Label28: TLabel
                  Left = 40
                  Top = 192
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label27: TLabel
                  Left = 104
                  Top = 192
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn4: TSpeedButton
                  Left = 8
                  Top = 232
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn4Click
                end
                object Label30: TLabel
                  Left = 40
                  Top = 256
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label29: TLabel
                  Left = 104
                  Top = 256
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn5: TSpeedButton
                  Left = 8
                  Top = 296
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn5Click
                end
                object Label32: TLabel
                  Left = 40
                  Top = 320
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label33: TLabel
                  Left = 104
                  Top = 320
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn6: TSpeedButton
                  Left = 8
                  Top = 360
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn6Click
                end
                object TextureMinEdit1: TEdit
                  Left = 40
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 0
                  Text = '1'
                end
                object TextureMaxEdit1: TEdit
                  Left = 104
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 1
                  Text = '5'
                end
                object TextureFilename1: TEdit
                  Left = 40
                  Top = 40
                  Width = 121
                  Height = 21
                  TabOrder = 2
                end
                object TextureMinEdit2: TEdit
                  Left = 40
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 3
                  Text = '1'
                end
                object TextureMaxEdit2: TEdit
                  Left = 104
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 4
                  Text = '5'
                end
                object TextureFilename2: TEdit
                  Left = 40
                  Top = 104
                  Width = 121
                  Height = 21
                  TabOrder = 5
                end
                object TextureMinEdit3: TEdit
                  Left = 40
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 6
                  Text = '1'
                end
                object TextureMaxEdit3: TEdit
                  Left = 104
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 7
                  Text = '5'
                end
                object TextureFilename3: TEdit
                  Left = 40
                  Top = 168
                  Width = 121
                  Height = 21
                  TabOrder = 8
                end
                object TextureMinEdit4: TEdit
                  Left = 40
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 9
                  Text = '1'
                end
                object TextureMaxEdit4: TEdit
                  Left = 104
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 10
                  Text = '5'
                end
                object TextureFilename4: TEdit
                  Left = 40
                  Top = 232
                  Width = 121
                  Height = 21
                  TabOrder = 11
                end
                object TextureMinEdit5: TEdit
                  Left = 40
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 12
                  Text = '1'
                end
                object TextureMaxEdit5: TEdit
                  Left = 104
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 13
                  Text = '5'
                end
                object TextureFilename5: TEdit
                  Left = 40
                  Top = 296
                  Width = 121
                  Height = 21
                  TabOrder = 14
                end
                object TextureCB1: TCheckBox
                  Left = 10
                  Top = 16
                  Width = 15
                  Height = 17
                  TabOrder = 15
                end
                object TextureCB2: TCheckBox
                  Left = 10
                  Top = 80
                  Width = 15
                  Height = 17
                  TabOrder = 16
                end
                object TextureCB3: TCheckBox
                  Left = 10
                  Top = 144
                  Width = 15
                  Height = 17
                  TabOrder = 17
                end
                object TextureCB4: TCheckBox
                  Left = 10
                  Top = 208
                  Width = 15
                  Height = 17
                  TabOrder = 18
                end
                object TextureCB5: TCheckBox
                  Left = 10
                  Top = 272
                  Width = 15
                  Height = 17
                  TabOrder = 19
                end
                object TextureCB6: TCheckBox
                  Left = 10
                  Top = 336
                  Width = 15
                  Height = 17
                  TabOrder = 20
                end
                object TextureMinEdit6: TEdit
                  Left = 40
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 21
                  Text = '1'
                end
                object TextureMaxEdit6: TEdit
                  Left = 104
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 22
                  Text = '5'
                end
                object TextureFilename6: TEdit
                  Left = 40
                  Top = 360
                  Width = 121
                  Height = 21
                  TabOrder = 23
                end
              end
              object TabSheet6: TTabSheet
                Caption = '..12'
                ImageIndex = 1
                object Label34: TLabel
                  Left = 40
                  Top = 0
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label35: TLabel
                  Left = 104
                  Top = 0
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn7: TSpeedButton
                  Left = 8
                  Top = 40
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn7Click
                end
                object Label40: TLabel
                  Left = 40
                  Top = 64
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label41: TLabel
                  Left = 104
                  Top = 64
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn8: TSpeedButton
                  Left = 8
                  Top = 104
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn8Click
                end
                object Label42: TLabel
                  Left = 40
                  Top = 128
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label43: TLabel
                  Left = 104
                  Top = 128
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn9: TSpeedButton
                  Left = 8
                  Top = 168
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn9Click
                end
                object Label44: TLabel
                  Left = 40
                  Top = 192
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label45: TLabel
                  Left = 104
                  Top = 192
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn10: TSpeedButton
                  Left = 8
                  Top = 232
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn10Click
                end
                object Label46: TLabel
                  Left = 40
                  Top = 256
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label47: TLabel
                  Left = 104
                  Top = 256
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn11: TSpeedButton
                  Left = 8
                  Top = 296
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn11Click
                end
                object Label48: TLabel
                  Left = 40
                  Top = 320
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label49: TLabel
                  Left = 104
                  Top = 320
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn12: TSpeedButton
                  Left = 8
                  Top = 360
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn12Click
                end
                object TextureCB7: TCheckBox
                  Left = 10
                  Top = 16
                  Width = 15
                  Height = 17
                  TabOrder = 0
                end
                object TextureMaxEdit7: TEdit
                  Left = 104
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 1
                  Text = '5'
                end
                object TextureMinEdit7: TEdit
                  Left = 40
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 2
                  Text = '1'
                end
                object TextureFilename7: TEdit
                  Left = 40
                  Top = 40
                  Width = 121
                  Height = 21
                  TabOrder = 3
                end
                object TextureCB8: TCheckBox
                  Left = 10
                  Top = 80
                  Width = 15
                  Height = 17
                  TabOrder = 4
                end
                object TextureMaxEdit8: TEdit
                  Left = 104
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 5
                  Text = '5'
                end
                object TextureMinEdit8: TEdit
                  Left = 40
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 6
                  Text = '1'
                end
                object TextureFilename8: TEdit
                  Left = 40
                  Top = 104
                  Width = 121
                  Height = 21
                  TabOrder = 7
                end
                object TextureCB9: TCheckBox
                  Left = 10
                  Top = 144
                  Width = 15
                  Height = 17
                  TabOrder = 8
                end
                object TextureMaxEdit9: TEdit
                  Left = 104
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 9
                  Text = '5'
                end
                object TextureMinEdit9: TEdit
                  Left = 40
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 10
                  Text = '1'
                end
                object TextureFilename9: TEdit
                  Left = 40
                  Top = 168
                  Width = 121
                  Height = 21
                  TabOrder = 11
                end
                object TextureCB10: TCheckBox
                  Left = 10
                  Top = 208
                  Width = 15
                  Height = 17
                  TabOrder = 12
                end
                object TextureMaxEdit10: TEdit
                  Left = 104
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 13
                  Text = '5'
                end
                object TextureMinEdit10: TEdit
                  Left = 40
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 14
                  Text = '1'
                end
                object TextureFilename10: TEdit
                  Left = 40
                  Top = 232
                  Width = 121
                  Height = 21
                  TabOrder = 15
                end
                object TextureCB11: TCheckBox
                  Left = 10
                  Top = 272
                  Width = 15
                  Height = 17
                  TabOrder = 16
                end
                object TextureMaxEdit11: TEdit
                  Left = 104
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 17
                  Text = '5'
                end
                object TextureMinEdit11: TEdit
                  Left = 40
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 18
                  Text = '1'
                end
                object TextureFilename11: TEdit
                  Left = 40
                  Top = 296
                  Width = 121
                  Height = 21
                  TabOrder = 19
                end
                object TextureCB12: TCheckBox
                  Left = 10
                  Top = 336
                  Width = 15
                  Height = 17
                  TabOrder = 20
                end
                object TextureMaxEdit12: TEdit
                  Left = 104
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 21
                  Text = '5'
                end
                object TextureMinEdit12: TEdit
                  Left = 40
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 22
                  Text = '1'
                end
                object TextureFilename12: TEdit
                  Left = 40
                  Top = 360
                  Width = 121
                  Height = 21
                  TabOrder = 23
                end
              end
              object TabSheet7: TTabSheet
                Caption = '..18'
                ImageIndex = 2
                object Label36: TLabel
                  Left = 40
                  Top = 0
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label37: TLabel
                  Left = 104
                  Top = 0
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn13: TSpeedButton
                  Left = 8
                  Top = 40
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn13Click
                end
                object Label50: TLabel
                  Left = 40
                  Top = 64
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label51: TLabel
                  Left = 104
                  Top = 64
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn14: TSpeedButton
                  Left = 8
                  Top = 104
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn14Click
                end
                object Label52: TLabel
                  Left = 40
                  Top = 128
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label53: TLabel
                  Left = 104
                  Top = 128
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn15: TSpeedButton
                  Left = 8
                  Top = 168
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn15Click
                end
                object Label54: TLabel
                  Left = 40
                  Top = 192
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label55: TLabel
                  Left = 104
                  Top = 192
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn16: TSpeedButton
                  Left = 8
                  Top = 232
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn16Click
                end
                object Label56: TLabel
                  Left = 40
                  Top = 256
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label57: TLabel
                  Left = 104
                  Top = 256
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn17: TSpeedButton
                  Left = 8
                  Top = 296
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn17Click
                end
                object Label58: TLabel
                  Left = 40
                  Top = 320
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label59: TLabel
                  Left = 104
                  Top = 320
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn18: TSpeedButton
                  Left = 8
                  Top = 360
                  Width = 23
                  Height = 22
                  OnClick = TextureBtn18Click
                end
                object TextureCB13: TCheckBox
                  Left = 10
                  Top = 16
                  Width = 15
                  Height = 17
                  TabOrder = 0
                end
                object TextureMaxEdit13: TEdit
                  Left = 104
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 1
                  Text = '5'
                end
                object TextureMinEdit13: TEdit
                  Left = 40
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 2
                  Text = '1'
                end
                object TextureFilename13: TEdit
                  Left = 40
                  Top = 40
                  Width = 121
                  Height = 21
                  TabOrder = 3
                end
                object TextureCB14: TCheckBox
                  Left = 10
                  Top = 80
                  Width = 15
                  Height = 17
                  TabOrder = 4
                end
                object TextureMaxEdit14: TEdit
                  Left = 104
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 5
                  Text = '5'
                end
                object TextureMinEdit14: TEdit
                  Left = 40
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 6
                  Text = '1'
                end
                object TextureFilename14: TEdit
                  Left = 40
                  Top = 104
                  Width = 121
                  Height = 21
                  TabOrder = 7
                end
                object TextureCB15: TCheckBox
                  Left = 10
                  Top = 144
                  Width = 15
                  Height = 17
                  TabOrder = 8
                end
                object TextureMaxEdit15: TEdit
                  Left = 104
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 9
                  Text = '5'
                end
                object TextureMinEdit15: TEdit
                  Left = 40
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 10
                  Text = '1'
                end
                object TextureFilename15: TEdit
                  Left = 40
                  Top = 168
                  Width = 121
                  Height = 21
                  TabOrder = 11
                end
                object TextureCB16: TCheckBox
                  Left = 10
                  Top = 208
                  Width = 15
                  Height = 17
                  TabOrder = 12
                end
                object TextureMaxEdit16: TEdit
                  Left = 104
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 13
                  Text = '5'
                end
                object TextureMinEdit16: TEdit
                  Left = 40
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 14
                  Text = '1'
                end
                object TextureFilename16: TEdit
                  Left = 40
                  Top = 232
                  Width = 121
                  Height = 21
                  TabOrder = 15
                end
                object TextureCB17: TCheckBox
                  Left = 10
                  Top = 272
                  Width = 15
                  Height = 17
                  TabOrder = 16
                end
                object TextureMaxEdit17: TEdit
                  Left = 104
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 17
                  Text = '5'
                end
                object TextureMinEdit17: TEdit
                  Left = 40
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 18
                  Text = '1'
                end
                object TextureFilename17: TEdit
                  Left = 40
                  Top = 296
                  Width = 121
                  Height = 21
                  TabOrder = 19
                end
                object TextureCB18: TCheckBox
                  Left = 10
                  Top = 336
                  Width = 15
                  Height = 17
                  TabOrder = 20
                end
                object TextureMaxEdit18: TEdit
                  Left = 104
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 21
                  Text = '5'
                end
                object TextureMinEdit18: TEdit
                  Left = 40
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 22
                  Text = '1'
                end
                object TextureFilename18: TEdit
                  Left = 40
                  Top = 360
                  Width = 121
                  Height = 21
                  TabOrder = 23
                end
              end
              object TabSheet8: TTabSheet
                Caption = '..24'
                ImageIndex = 3
                object Label38: TLabel
                  Left = 40
                  Top = 0
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label39: TLabel
                  Left = 104
                  Top = 0
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn19: TSpeedButton
                  Left = 0
                  Top = 40
                  Width = 41
                  Height = 22
                  Caption = 'PFX4'
                  OnClick = TextureBtn19Click
                end
                object Label60: TLabel
                  Left = 40
                  Top = 64
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label61: TLabel
                  Left = 104
                  Top = 64
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn20: TSpeedButton
                  Left = 0
                  Top = 104
                  Width = 41
                  Height = 22
                  Caption = 'PFX4'
                  OnClick = TextureBtn20Click
                end
                object Label62: TLabel
                  Left = 40
                  Top = 128
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label63: TLabel
                  Left = 104
                  Top = 128
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn21: TSpeedButton
                  Left = 0
                  Top = 168
                  Width = 41
                  Height = 22
                  Caption = 'PFX16'
                  OnClick = TextureBtn21Click
                end
                object Label64: TLabel
                  Left = 40
                  Top = 192
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label65: TLabel
                  Left = 104
                  Top = 192
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn22: TSpeedButton
                  Left = 0
                  Top = 232
                  Width = 41
                  Height = 22
                  Caption = 'PFX16'
                  OnClick = TextureBtn22Click
                end
                object Label66: TLabel
                  Left = 40
                  Top = 256
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label67: TLabel
                  Left = 104
                  Top = 256
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn23: TSpeedButton
                  Left = 0
                  Top = 296
                  Width = 41
                  Height = 22
                  Caption = 'PFX64'
                  OnClick = TextureBtn23Click
                end
                object Label68: TLabel
                  Left = 40
                  Top = 320
                  Width = 28
                  Height = 13
                  Caption = '% Min'
                end
                object Label69: TLabel
                  Left = 104
                  Top = 320
                  Width = 31
                  Height = 13
                  Caption = '% Max'
                end
                object TextureBtn24: TSpeedButton
                  Left = 0
                  Top = 360
                  Width = 41
                  Height = 22
                  Caption = 'PFX64'
                  OnClick = TextureBtn24Click
                end
                object TextureCB19: TCheckBox
                  Left = 10
                  Top = 16
                  Width = 15
                  Height = 17
                  TabOrder = 0
                end
                object TextureMaxEdit19: TEdit
                  Left = 104
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 1
                  Text = '5'
                end
                object TextureMinEdit19: TEdit
                  Left = 40
                  Top = 16
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 2
                  Text = '1'
                end
                object TextureFilename19: TEdit
                  Left = 40
                  Top = 40
                  Width = 121
                  Height = 21
                  TabOrder = 3
                end
                object TextureCB20: TCheckBox
                  Left = 10
                  Top = 80
                  Width = 15
                  Height = 17
                  TabOrder = 4
                end
                object TextureMaxEdit20: TEdit
                  Left = 104
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 5
                  Text = '5'
                end
                object TextureMinEdit20: TEdit
                  Left = 40
                  Top = 80
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 6
                  Text = '1'
                end
                object TextureFilename20: TEdit
                  Left = 40
                  Top = 104
                  Width = 121
                  Height = 21
                  TabOrder = 7
                end
                object TextureCB21: TCheckBox
                  Left = 10
                  Top = 144
                  Width = 15
                  Height = 17
                  TabOrder = 8
                end
                object TextureMaxEdit21: TEdit
                  Left = 104
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 9
                  Text = '5'
                end
                object TextureMinEdit21: TEdit
                  Left = 40
                  Top = 144
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 10
                  Text = '1'
                end
                object TextureFilename21: TEdit
                  Left = 40
                  Top = 168
                  Width = 121
                  Height = 21
                  TabOrder = 11
                end
                object TextureCB22: TCheckBox
                  Left = 10
                  Top = 208
                  Width = 15
                  Height = 17
                  TabOrder = 12
                end
                object TextureMaxEdit22: TEdit
                  Left = 104
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 13
                  Text = '5'
                end
                object TextureMinEdit22: TEdit
                  Left = 40
                  Top = 208
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 14
                  Text = '1'
                end
                object TextureFilename22: TEdit
                  Left = 40
                  Top = 232
                  Width = 121
                  Height = 21
                  TabOrder = 15
                end
                object TextureCB23: TCheckBox
                  Left = 10
                  Top = 272
                  Width = 15
                  Height = 17
                  TabOrder = 16
                end
                object TextureMaxEdit23: TEdit
                  Left = 104
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 17
                  Text = '5'
                end
                object TextureMinEdit23: TEdit
                  Left = 40
                  Top = 272
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 18
                  Text = '1'
                end
                object TextureFilename23: TEdit
                  Left = 40
                  Top = 296
                  Width = 121
                  Height = 21
                  TabOrder = 19
                end
                object TextureCB24: TCheckBox
                  Left = 10
                  Top = 336
                  Width = 15
                  Height = 17
                  TabOrder = 20
                end
                object TextureMaxEdit24: TEdit
                  Left = 104
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 21
                  Text = '5'
                end
                object TextureMinEdit24: TEdit
                  Left = 40
                  Top = 336
                  Width = 49
                  Height = 21
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 22
                  Text = '1'
                end
                object TextureFilename24: TEdit
                  Left = 40
                  Top = 360
                  Width = 121
                  Height = 21
                  TabOrder = 23
                end
              end
            end
          end
          object TabSheet14: TTabSheet
            Caption = 'Albedo'
            ImageIndex = 2
            object Label19: TLabel
              Left = 16
              Top = 384
              Width = 63
              Height = 13
              Caption = 'Dissipation'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label16: TLabel
              Left = 16
              Top = 344
              Width = 56
              Height = 13
              Caption = 'Formation'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label18: TLabel
              Left = 16
              Top = 304
              Width = 68
              Height = 13
              Caption = 'Initial Alpha'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label71: TLabel
              Left = 48
              Top = 272
              Width = 76
              Height = 13
              Caption = 'Cloud Bottom'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label70: TLabel
              Left = 16
              Top = 16
              Width = 131
              Height = 13
              Caption = 'R       G         B        A'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label14: TLabel
              Left = 48
              Top = 0
              Width = 59
              Height = 13
              Caption = 'Cloud Top'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label110: TLabel
              Left = 96
              Top = 304
              Width = 64
              Height = 13
              Caption = 'Alpha Rate'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object AlphaDissipationEdit: TEdit
              Left = 16
              Top = 400
              Width = 49
              Height = 21
              Hint = 'Dissipation Alpha'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Text = '0.7'
            end
            object AlphaFormationEdit: TEdit
              Left = 16
              Top = 360
              Width = 49
              Height = 21
              Hint = 'Formation Alpha'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              Text = '0.1'
            end
            object AlphaInitialEdit: TEdit
              Left = 16
              Top = 320
              Width = 49
              Height = 21
              Hint = 'Starting Alpha'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              Text = '0.3'
            end
            object AlbedoPanel5: TPanel
              Left = 8
              Top = 224
              Width = 153
              Height = 14
              Color = clInactiveCaption
              TabOrder = 3
              OnClick = AlbedoPanel5Click
            end
            object AlbedoPanel4: TPanel
              Left = 8
              Top = 176
              Width = 153
              Height = 14
              TabOrder = 4
              OnClick = AlbedoPanel4Click
            end
            object AlbedoPanel3: TPanel
              Left = 8
              Top = 128
              Width = 153
              Height = 14
              Color = clMenu
              TabOrder = 5
              OnClick = AlbedoPanel3Click
            end
            object AlbedoPanel2: TPanel
              Left = 8
              Top = 80
              Width = 153
              Height = 14
              Color = clInfoBk
              TabOrder = 6
              OnClick = AlbedoPanel2Click
            end
            object AlbedoPanel1: TPanel
              Left = 8
              Top = 32
              Width = 153
              Height = 14
              Color = clWhite
              TabOrder = 7
              OnClick = AlbedoPanel1Click
            end
            object Albedo5AEdit: TEdit
              Left = 128
              Top = 240
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 8
              Text = '0.2'
            end
            object Albedo5BEdit: TEdit
              Left = 85
              Top = 240
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 9
              Text = '0.5'
            end
            object Albedo5GEdit: TEdit
              Left = 43
              Top = 240
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 10
              Text = '0.5'
            end
            object Albedo5REdit: TEdit
              Left = 0
              Top = 240
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 11
              Text = '0.5'
            end
            object Albedo4REdit: TEdit
              Left = 0
              Top = 192
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 12
              Text = '0.7'
            end
            object Albedo4GEdit: TEdit
              Left = 43
              Top = 192
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 13
              Text = '0.7'
            end
            object Albedo4BEdit: TEdit
              Left = 85
              Top = 192
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 14
              Text = '0.7'
            end
            object Albedo4AEdit: TEdit
              Left = 128
              Top = 192
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 15
              Text = '0.4'
            end
            object Albedo3AEdit: TEdit
              Left = 128
              Top = 144
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 16
              Text = '0.6'
            end
            object Albedo3BEdit: TEdit
              Left = 85
              Top = 144
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 17
              Text = '0.8'
            end
            object Albedo3GEdit: TEdit
              Left = 43
              Top = 144
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 18
              Text = '0.8'
            end
            object Albedo3REdit: TEdit
              Left = 0
              Top = 144
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 19
              Text = '0.8'
            end
            object Albedo2REdit: TEdit
              Left = 0
              Top = 96
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 20
              Text = '0.9'
            end
            object Albedo2GEdit: TEdit
              Left = 43
              Top = 96
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 21
              Text = '0.9'
            end
            object Albedo2BEdit: TEdit
              Left = 85
              Top = 96
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 22
              Text = '0.9'
            end
            object Albedo2AEdit: TEdit
              Left = 128
              Top = 96
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 23
              Text = '0.8'
            end
            object Albedo1AEdit: TEdit
              Left = 128
              Top = 48
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 24
              Text = '1.0'
            end
            object Albedo1BEdit: TEdit
              Left = 85
              Top = 48
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 25
              Text = '1.0'
            end
            object Albedo1GEdit: TEdit
              Left = 43
              Top = 48
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 26
              Text = '1.0'
            end
            object Albedo1REdit: TEdit
              Left = 0
              Top = 48
              Width = 37
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 27
              Text = '1.0'
            end
            object AlphaRateEdit: TEdit
              Left = 96
              Top = 320
              Width = 49
              Height = 21
              Hint = 'Alpha Rate of change'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 28
              Text = '0.003'
            end
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Global'
        ImageIndex = 2
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 185
          Height = 498
          ActivePage = TabSheet9
          Align = alClient
          TabOrder = 0
          object TabSheet9: TTabSheet
            Caption = 'Sun'
            object SunGroupBox: TGroupBox
              Left = 0
              Top = 8
              Width = 177
              Height = 281
              Caption = 'Sun Data'
              TabOrder = 0
              object Label17: TLabel
                Left = 8
                Top = 16
                Width = 59
                Height = 13
                Caption = 'Sun X,Y,Z'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object SunTimeLabel: TLabel
                Left = 8
                Top = 80
                Width = 40
                Height = 13
                Caption = '5:15 PM'
              end
              object Label107: TLabel
                Left = 56
                Top = 144
                Width = 10
                Height = 13
                Caption = 'R'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label104: TLabel
                Left = 56
                Top = 168
                Width = 10
                Height = 13
                Caption = 'G'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label105: TLabel
                Left = 56
                Top = 192
                Width = 9
                Height = 13
                Caption = 'B'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label106: TLabel
                Left = 56
                Top = 213
                Width = 9
                Height = 13
                Caption = 'A'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object Label108: TLabel
                Left = 8
                Top = 232
                Width = 64
                Height = 13
                Caption = 'Alpha Rate'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object SunXEdit: TEdit
                Left = 8
                Top = 32
                Width = 49
                Height = 21
                Hint = 'X'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                Text = '0'
              end
              object SunYEdit: TEdit
                Left = 64
                Top = 32
                Width = 49
                Height = 21
                Hint = 'Y'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 1
                Text = '0'
              end
              object SunZEdit: TEdit
                Left = 120
                Top = 32
                Width = 49
                Height = 21
                Hint = 'Z'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 2
                Text = '0'
              end
              object SunMovingCB: TCheckBox
                Left = 8
                Top = 56
                Width = 65
                Height = 17
                Caption = 'Moving'
                TabOrder = 3
              end
              object DawnPanel: TPanel
                Left = 72
                Top = 104
                Width = 41
                Height = 25
                Caption = 'Dawn'
                Color = clYellow
                TabOrder = 4
                OnClick = DawnPanelClick
              end
              object DuskPanel: TPanel
                Left = 8
                Top = 104
                Width = 41
                Height = 25
                Caption = 'Dusk'
                Color = clFuchsia
                TabOrder = 5
                OnClick = DuskPanelClick
              end
              object SunLensFlareCB: TCheckBox
                Left = 104
                Top = 56
                Width = 66
                Height = 17
                Caption = 'LensFlare'
                TabOrder = 6
                OnClick = SunLensFlareCBClick
              end
              object SunDawnREdit: TEdit
                Left = 72
                Top = 136
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 7
                Text = '0.85'
              end
              object SunDawnGEdit: TEdit
                Left = 72
                Top = 160
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 8
                Text = '0.85'
              end
              object SunDawnBEdit: TEdit
                Left = 72
                Top = 184
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 9
                Text = '0.1'
              end
              object SunDawnAEdit: TEdit
                Left = 72
                Top = 208
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 10
                Text = '1.0'
              end
              object SunDuskAEdit: TEdit
                Left = 8
                Top = 208
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 11
                Text = '1.0'
              end
              object SunDuskBEdit: TEdit
                Left = 8
                Top = 184
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 12
                Text = '0.69'
              end
              object SunDuskGEdit: TEdit
                Left = 8
                Top = 160
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 13
                Text = '0.8'
              end
              object SunDuskREdit: TEdit
                Left = 8
                Top = 136
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 14
                Text = '0.96'
              end
              object LightsOnCB: TCheckBox
                Left = 68
                Top = 56
                Width = 32
                Height = 17
                Hint = 'Shining On'
                Caption = 'On'
                TabOrder = 15
                OnClick = LightsOnCBClick
              end
              object Edit1: TEdit
                Left = 8
                Top = 248
                Width = 49
                Height = 21
                Hint = 'Starting Alpha'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 16
                Text = '0.003'
              end
            end
          end
          object TabSheet16: TTabSheet
            Caption = 'Dome'
            ImageIndex = 1
            object CirrusFilenameOpenBtn: TSpeedButton
              Left = 2
              Top = 150
              Width = 23
              Height = 22
              Hint = 'Open Image or Cirrus Cloud File'
              OnClick = CirrusFilenameOpenBtnClick
            end
            object CirrusFilenameSaveBtn: TSpeedButton
              Left = 146
              Top = 150
              Width = 23
              Height = 22
              Hint = 'Save CCF'
              OnClick = CirrusFilenameSaveBtnClick
            end
            object DomeGroupBox: TGroupBox
              Left = 0
              Top = 2
              Width = 177
              Height = 65
              TabOrder = 0
              object ProDomeFilenameBtn: TSpeedButton
                Left = 80
                Top = 38
                Width = 23
                Height = 22
                OnClick = ProDomeFilenameBtnClick
              end
              object SkyDomeFilenameBtn: TSpeedButton
                Left = 80
                Top = 14
                Width = 23
                Height = 22
                OnClick = SkyDomeFilenameBtnClick
              end
              object ProDomeCB: TCheckBox
                Left = 8
                Top = 38
                Width = 66
                Height = 17
                Hint = 'Toggle Display'
                Caption = 'ProDome'
                TabOrder = 0
                OnClick = ProDomeCBClick
              end
              object SkyDomeCB: TCheckBox
                Left = 8
                Top = 14
                Width = 66
                Height = 17
                Hint = 'Toggle Display'
                Caption = 'SkyDome'
                TabOrder = 1
                OnClick = SkyDomeCBClick
              end
              object SkyDomeFilenameEdit: TEdit
                Left = 104
                Top = 14
                Width = 65
                Height = 21
                Hint = 'File Name'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 2
                Text = 'FileName'
              end
              object ProDomeFilenameEdit: TEdit
                Left = 104
                Top = 38
                Width = 65
                Height = 21
                Hint = 'File Name'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 3
                Text = 'FileName'
              end
            end
            object CirrusRG: TRadioGroup
              Left = 0
              Top = 72
              Width = 177
              Height = 73
              Hint = 'Filename Already Exists FOR the selection'
              Caption = 'Cirrus Cloud Cover'
              ItemIndex = 0
              Items.Strings = (
                'None'
                'Static Image'
                'Moving Image'
                'Procedural')
              TabOrder = 1
              OnClick = CirrusRGClick
            end
            object CirrusFilenameEdit: TEdit
              Left = 32
              Top = 148
              Width = 113
              Height = 21
              Hint = 'Cirrus Cloud File Name'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              Text = 'FileName'
            end
            object GroupBox1: TGroupBox
              Left = 0
              Top = 176
              Width = 177
              Height = 241
              Caption = 'Procedural Clouds'
              TabOrder = 3
              object Label73: TLabel
                Left = 56
                Top = 64
                Width = 36
                Height = 13
                Caption = 'Min Cut'
              end
              object Label72: TLabel
                Left = 56
                Top = 40
                Width = 52
                Height = 13
                Caption = 'Image Size'
              end
              object Label74: TLabel
                Left = 56
                Top = 88
                Width = 50
                Height = 13
                Hint = 'Noise Sharpness'
                Caption = 'Sharpness'
              end
              object Label75: TLabel
                Left = 96
                Top = 112
                Width = 25
                Height = 13
                Hint = 'Cloud Random Noise Seed'
                Caption = 'Seed'
              end
              object CloudFileOpenBtn: TSpeedButton
                Left = 10
                Top = 150
                Width = 23
                Height = 22
                Hint = 'Load Cloud File'
                OnClick = CloudFileOpenBtnClick
              end
              object CloudChangeMoreBtn: TSpeedButton
                Left = 56
                Top = 184
                Width = 12
                Height = 22
                Caption = '+'
                OnClick = CloudChangeMoreBtnClick
              end
              object CloudChangeLessBtn: TSpeedButton
                Left = 72
                Top = 184
                Width = 12
                Height = 22
                Caption = '-'
                OnClick = CloudChangeLessBtnClick
              end
              object Label76: TLabel
                Left = 104
                Top = 192
                Width = 67
                Height = 13
                Caption = 'Cloud Change'
              end
              object CloudsAnimeFasterBtn: TSpeedButton
                Left = 56
                Top = 208
                Width = 12
                Height = 22
                Caption = '+'
                OnClick = CloudsAnimeFasterBtnClick
              end
              object CloudsAnimeSlowerBtn: TSpeedButton
                Left = 72
                Top = 208
                Width = 12
                Height = 22
                Caption = '-'
                OnClick = CloudsAnimeSlowerBtnClick
              end
              object CloudsAnimeMuchSlowerBtn: TSpeedButton
                Left = 88
                Top = 208
                Width = 12
                Height = 22
                Caption = '--'
                OnClick = CloudsAnimeMuchSlowerBtnClick
              end
              object Label77: TLabel
                Left = 120
                Top = 216
                Width = 46
                Height = 13
                Hint = 'Cloud Animation'
                Caption = 'Animation'
              end
              object UseCloudsCB: TCheckBox
                Left = 8
                Top = 11
                Width = 105
                Height = 17
                Caption = 'Clouds Active Or :'
                TabOrder = 0
                OnClick = UseCloudsCBClick
              end
              object UseCloudsSettingCB: TCheckBox
                Left = 112
                Top = 11
                Width = 17
                Height = 17
                Hint = 'Use Clouds Parameters'
                Checked = True
                State = cbChecked
                TabOrder = 1
              end
              object CloudImageSizeUsedEdit: TEdit
                Left = 10
                Top = 32
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 2
                Text = '128'
              end
              object CloudMinUsedEdit: TEdit
                Left = 10
                Top = 56
                Width = 41
                Height = 21
                Hint = '98 [0..100]'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 3
                Text = '0'
              end
              object CloudMinRangeUsedEdit: TEdit
                Left = 96
                Top = 56
                Width = 33
                Height = 21
                Hint = 'Range'
                TabOrder = 4
                Text = '24'
              end
              object UseCloudsRangeCB: TCheckBox
                Left = 144
                Top = 59
                Width = 17
                Height = 17
                Hint = 'Change iaw Range'
                TabOrder = 5
              end
              object CloudSharpUsedEdit: TEdit
                Left = 10
                Top = 80
                Width = 41
                Height = 21
                ParentShowHint = False
                ShowHint = True
                TabOrder = 6
                Text = '0.99'
              end
              object CloudSharpRangeUsedEdit: TEdit
                Left = 112
                Top = 80
                Width = 33
                Height = 21
                Hint = 'Range'
                TabOrder = 7
                Text = '0.1'
              end
              object UseCloudsSRangeCB: TCheckBox
                Left = 152
                Top = 83
                Width = 17
                Height = 17
                Hint = 'Change iaw Range'
                TabOrder = 8
              end
              object CloudRandomSeedUsedEdit: TEdit
                Left = 10
                Top = 104
                Width = 79
                Height = 21
                Hint = '0 or Seeded'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 9
                Text = '1643093486'
              end
              object UseCloudFileCB: TCheckBox
                Left = 8
                Top = 131
                Width = 73
                Height = 17
                Hint = 'Use File'
                Caption = 'Cloud File'
                TabOrder = 10
              end
              object CloudFileUsedEdit: TEdit
                Left = 35
                Top = 148
                Width = 134
                Height = 21
                Hint = 'Procedural File Name'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 11
                Text = 'FileName'
              end
              object CloudChangeAmountEdit: TEdit
                Left = 10
                Top = 184
                Width = 41
                Height = 21
                Hint = 'Cloud Change Amount'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 12
                Text = '0.0001'
              end
              object CloudAnimeEdit: TEdit
                Left = 10
                Top = 208
                Width = 41
                Height = 21
                Hint = 'Cloud Animation Speed'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 13
                Text = '6'
              end
            end
          end
          object TabSheet20: TTabSheet
            Caption = 'Makers'
            ImageIndex = 3
            object MFxMakerBtn: TSpeedButton
              Left = 2
              Top = 16
              Width = 52
              Height = 22
              Caption = 'MF'
              OnClick = MFxMakerBtnClick
            end
            object NanxMakerBtn: TSpeedButton
              Left = 60
              Top = 16
              Width = 52
              Height = 22
              Caption = 'Nan'
              OnClick = NanxMakerBtnClick
            end
            object PCxMakerBtn: TSpeedButton
              Left = 120
              Top = 16
              Width = 52
              Height = 22
              Caption = 'PC'
              OnClick = PCxMakerBtnClick
            end
            object TexEditorBtn: TSpeedButton
              Left = 2
              Top = 40
              Width = 80
              Height = 22
              Caption = 'Texture Editor'
              OnClick = TexEditorBtnClick
            end
            object TexCombineBtn: TSpeedButton
              Left = 84
              Top = 40
              Width = 89
              Height = 22
              Caption = 'Texture Combiner'
              OnClick = TexCombineBtnClick
            end
          end
        end
      end
      object TabSheet19: TTabSheet
        Caption = 'Niniane'
        ImageIndex = 3
        object Label22: TLabel
          Left = 8
          Top = 20
          Width = 70
          Height = 13
          Caption = 'Ring Radius'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RingRadiusEdit: TEdit
          Left = 80
          Top = 12
          Width = 33
          Height = 21
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '14'
        end
        object RingRadiusFileNameEdit: TEdit
          Left = 120
          Top = 12
          Width = 53
          Height = 21
          Hint = 'File Name'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'FileName'
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 526
    Width = 723
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    ExplicitTop = 505
    ExplicitWidth = 705
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 24
    object ImposterDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLSkyDome1: TGLSkyDome
      Visible = False
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
        end
        item
          StartAngle = -10.000000000000000000
          StartColor.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
          StopColor.Color = {0000803F0000803F0000803F0000803F}
        end>
      Stars = <>
    end
    object GLEarthSkyDome1: TGLEarthSkyDome
      Visible = False
      Bands = <>
      Stars = <>
      SunElevation = 75.000000000000000000
      Turbidity = 15.000000000000000000
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object ImposterDirectOGL: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = ImposterDirectOGLRender
      Blend = False
    end
    object GLXYZGrid1: TGLXYZGrid
      Tag = 1
      LineColor.Color = {6666263F6666263F6666263F0000803F}
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 0.500000000000000000
      YSamplingScale.Min = -10.000000000000000000
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 0.500000000000000000
      ZSamplingScale.Min = -10.000000000000000000
      ZSamplingScale.Max = 10.000000000000000000
      ZSamplingScale.Step = 0.500000000000000000
    end
    object CloudSphere: TGLSphere
      Material.BackProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.BackProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.BackProperties.Emission.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageClassName = 'TGLProcTextureNoise'
      Material.Texture.Image.MinCut = 0
      Material.Texture.Image.NoiseSharpness = 0.990000009536743200
      Material.Texture.Image.Seamless = False
      Material.Texture.Image.NoiseRandSeed = 625017788
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      NormalDirection = ndInside
      Bottom = -10
      Radius = 111.000000000000000000
    end
    object GLSunSphere: TGLSphere
      Material.FrontProperties.Ambient.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      Material.FrontProperties.Emission.Color = {8FC2753FCDCC4C3FD7A3303F0000803F}
      Position.Coordinates = {0000A0C00000A0C00000A0400000803F}
      Radius = 0.500000000000000000
      object GLHiddenColorCube: TGLCube
        Up.Coordinates = {00000000000080BF0000000000000000}
        CubeSize = {CDCCCC3DCDCCCC3DCDCCCC3D}
      end
      object GLLensFlare1: TGLLensFlare
        Seed = 1465
        FlareIsNotOccluded = False
        Position.Coordinates = {000000000000003F000000000000803F}
        Visible = False
      end
      object GLLightSource1: TGLLightSource
        Tag = 1
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {000000000000803F000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCameraTargetDC: TGLDummyCube
      Tag = 1
      ShowAxes = True
      CubeSize = 4.000000000000000000
    end
    object GLGitzmoDC: TGLDummyCube
      Tag = 1
      Visible = False
      CubeSize = 1.000000000000000000
    end
    object GLCloudCube: TGLCube
      Material.FrontProperties.Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
      Material.FrontProperties.Diffuse.Color = {EBE0E03E9A93133FE4DB5B3F0000803F}
      Material.FrontProperties.Emission.Color = {EBE0E03E9A93133FE4DB5B3F0000803F}
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
    end
    object CloudCubeDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object SpriteCubeDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLFreeFormx: TGLFreeForm
      Material.FaceCulling = fcNoCull
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      UseMeshMaterials = False
    end
    object FreeFormDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLParticles1: TGLParticles
      CubeSize = 1.000000000000000000
      object ParticleSprite: TGLSprite
        Material.BlendingMode = bmAdditive
        Material.Texture.ImageAlpha = tiaAlphaFromIntensity
        Width = 1.000000000000000000
        Height = 1.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object PFXCubeDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object PFXRenderer: TGLParticleFXRenderer
      BlendingMode = bmTransparency
    end
    object GLCamera1: TGLCamera
      DepthOfView = 222.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCameraTargetDC
      Position.Coordinates = {0000000000001041000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 44
    Top = 24
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 474
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 392
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 432
    Top = 8
  end
  object GLCloudsMatLib: TGLMaterialLibrary
    Left = 112
    Top = 24
  end
  object PFXCSpriteManager: TGLCustomSpritePFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    OnCreateParticle = PFXCSpriteManagerCreateParticle
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    ColorMode = scmNone
    ParticleSize = 1.000000000000000000
    ColorInner.Color = {0000803F0000803F0000803F0000803F}
    LifeColors = <
      item
        ColorOuter.Color = {0000000000000000000000000000003F}
        LifeTime = 1.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 144
    Top = 24
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 80
    Top = 24
  end
  object PFXPolyFogManager: TGLPolygonPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    NbSides = 7
    ParticleSize = 0.699999988079071100
    ColorInner.Color = {0000803F0000803F0000803F0000003F}
    ColorOuter.Color = {CDCC4C3FCDCC4C3FCDCC4C3F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 1.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 144
    Top = 56
  end
  object PFXPLMaskManager: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = PFXRenderer
    OnCreateParticle = PFXPLMaskManagerCreateParticle
    Friction = 1.000000000000000000
    BlendingMode = bmTransparency
    TexMapSize = 3
    ColorMode = scmFade
    ParticleSize = 0.699999988079071100
    ColorInner.Color = {0000803F0000803F0000803F0000003F}
    ColorOuter.Color = {77BE3F3FFED4583FFED4583F00000000}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F00000000}
        ColorOuter.Color = {0000803F0000803F0000803F00000000}
        LifeTime = 1.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 144
    Top = 88
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'XMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end
      item
        Name = 'YMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end
      item
        Name = 'ZMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end>
    Left = 208
    Top = 88
  end
  object GLEParticleMasksManager1: TGLEParticleMasksManager
    ParticleMasks = <
      item
        Scale.Coordinates = {0000A0400000A0400000A04000000000}
        Name = 'mask'
        MaterialLibrary = MatLib
        XMask = 'XMask'
        YMask = 'YMask'
        ZMask = 'ZMask'
        BackgroundColor = clBlack
        MaskColor = clWhite
      end>
    Left = 176
    Top = 88
  end
  object WinFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 240
    Top = 88
  end
  object PLMaskPopupMenu: TPopupMenu
    Left = 464
    Top = 80
    object Sphere1: TMenuItem
      Tag = 1
      Caption = 'Sphere'
      OnClick = PLMaskPopupMenu1Click
    end
    object Torus1: TMenuItem
      Tag = 2
      Caption = 'Torus'
      OnClick = PLMaskPopupMenu1Click
    end
    object Cube1: TMenuItem
      Tag = 3
      Caption = 'Cube'
      OnClick = PLMaskPopupMenu1Click
    end
    object Tube1: TMenuItem
      Tag = 4
      Caption = 'Tube'
      OnClick = PLMaskPopupMenu1Click
    end
    object TV1: TMenuItem
      Tag = 5
      Caption = 'TV'
      OnClick = PLMaskPopupMenu1Click
    end
    object Star1: TMenuItem
      Tag = 6
      Caption = 'Star'
      OnClick = PLMaskPopupMenu1Click
    end
  end
end
