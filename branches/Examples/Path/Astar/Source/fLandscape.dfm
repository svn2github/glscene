object frmLandscape: TfrmLandscape
  Left = 61
  Top = 15
  Caption = 'Landscape Sculptor'
  ClientHeight = 515
  ClientWidth = 678
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777877777777777
    7777877777777777777788777777887777778777777777077777787777778887
    7777887777777077777778877777088887778877778708877777788777777008
    887778777800700888877887777787700F888887807777000888778777778777
    700FFF88077700EEE008878877778777777700F07770EEEEEEE0888887778877
    77777707700EEEEEEEEE008F88778887787777770EEEEEEEEEEEEE00F8708887
    88778800EEEEEEEEEEEEEEEE0F0E0888877800EEEEEEEEEEEEEEEEEEE0EEE0FF
    8880EEEEEEEEEEE0EEEEEEEEEEEEEE0FF00EEEEEEE0EEE000EE0EEEEEEEEEEE0
    0EEEEEEEEEE000EEE00EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
    EEEEEEEEEEEEEEEEEEEEEEEFFFFFEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEFFEEEE
    EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEBBBEEEEEEEEFFFFFFFFEEEEEEEFEEEEBBB
    BBEEEEEEEEEEFFFFFFFEFFFFFFEEEBBBBBEEEEEEEEEEEFFFEEEFEFFEEEEEEBBB
    BBEEEEEEEEEEEEEEEEEEEEEEEEEEEEBBBEEEEEEEEEEEEEEEEEEEEEEEEEEE0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 678
    Height = 496
    ActivePage = TerrainTabSheet
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 503
    object TerrainTabSheet: TTabSheet
      Caption = 'Terrain'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TerrainPanel: TPanel
        Left = 0
        Top = 0
        Width = 105
        Height = 468
        Align = alLeft
        TabOrder = 0
        ExplicitHeight = 475
        object CheckBox_RenderWater: TCheckBox
          Left = 8
          Top = 272
          Width = 89
          Height = 17
          Caption = 'Render water'
          TabOrder = 0
        end
        object Button_RenderNormal: TButton
          Left = 53
          Top = 288
          Width = 50
          Height = 21
          Caption = 'Standard'
          TabOrder = 1
          OnClick = Button_RenderNormalClick
        end
        object Button_Rescale: TButton
          Left = 2
          Top = 336
          Width = 50
          Height = 21
          Caption = 'Rescale'
          TabOrder = 2
          OnClick = Button_RescaleClick
        end
        object Button_MakeIsland: TButton
          Left = 2
          Top = 312
          Width = 50
          Height = 21
          Caption = 'w/Island'
          TabOrder = 3
          OnClick = Button_MakeIslandClick
        end
        object Button_GO: TButton
          Left = 53
          Top = 88
          Width = 50
          Height = 21
          Caption = 'GO'
          TabOrder = 4
          OnClick = Button_GOClick
        end
        object Memo_Octaves: TMemo
          Left = 8
          Top = 112
          Width = 89
          Height = 137
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            'clear'
            'cup'
            'n|4|0.5'
            'n|8|0.25'
            'n|16|0.125'
            'n|32|0.061'
            'n|64|0.0305'
            'n|128|0.0150'
            'rescale')
          ParentFont = False
          TabOrder = 5
          WordWrap = False
        end
        object Button_Clear: TButton
          Left = 2
          Top = 88
          Width = 50
          Height = 21
          Caption = 'Clear'
          TabOrder = 6
          OnClick = Button_ClearClick
        end
        object Button_LoadTerrain: TButton
          Left = 2
          Top = 58
          Width = 50
          Height = 21
          Caption = 'Load'
          TabOrder = 7
          OnClick = Button_LoadTerrainClick
        end
        object Button_SaveTerrain: TButton
          Left = 53
          Top = 58
          Width = 50
          Height = 21
          Caption = 'Save'
          TabOrder = 8
          OnClick = Button_SaveTerrainClick
        end
        object Button_SubDivide: TButton
          Left = 53
          Top = 336
          Width = 50
          Height = 21
          Caption = 'Subdivide'
          TabOrder = 9
          OnClick = Button_SubDivideClick
        end
        object GroupBox1: TGroupBox
          Left = 2
          Top = 360
          Width = 100
          Height = 113
          Caption = 'Height altering'
          TabOrder = 10
          object Label1: TLabel
            Left = 8
            Top = 16
            Width = 33
            Height = 13
            Caption = 'Radius'
          end
          object Label2: TLabel
            Left = 8
            Top = 72
            Width = 29
            Height = 13
            Caption = 'Depth'
          end
          object DepthLabel: TLabel
            Left = 48
            Top = 72
            Width = 6
            Height = 13
            Caption = '7'
          end
          object RadiusLabel: TLabel
            Left = 48
            Top = 16
            Width = 12
            Height = 13
            Caption = '20'
          end
          object ScrollBar_Radius: TScrollBar
            Left = 4
            Top = 32
            Width = 93
            Height = 17
            Min = 1
            PageSize = 0
            Position = 20
            TabOrder = 0
            OnChange = ScrollBar_RadiusChange
          end
          object ScrollBar_Depth: TScrollBar
            Left = 4
            Top = 88
            Width = 93
            Height = 17
            Min = 1
            PageSize = 0
            Position = 7
            TabOrder = 1
            OnChange = ScrollBar_DepthChange
          end
          object CheckBox_Box: TCheckBox
            Left = 50
            Top = 54
            Width = 39
            Height = 17
            Caption = 'Box'
            TabOrder = 2
          end
        end
        object LandscapeSizeRG: TRadioGroup
          Left = 2
          Top = 2
          Width = 95
          Height = 55
          Caption = 'Landscape  Size'
          ItemIndex = 0
          Items.Strings = (
            '256'
            '512'
            '1024')
          TabOrder = 11
          OnClick = LandscapeSizeRGClick
        end
        object ResetDefaultMemoBtn: TButton
          Left = 2
          Top = 250
          Width = 101
          Height = 21
          Caption = 'Reset Default Memo'
          TabOrder = 12
          OnClick = ResetDefaultMemoBtnClick
        end
        object WaterEdit: TEdit
          Left = 8
          Top = 288
          Width = 33
          Height = 21
          TabOrder = 13
          Text = '0.2'
        end
        object IslandEdit: TEdit
          Left = 56
          Top = 312
          Width = 33
          Height = 21
          TabOrder = 14
          Text = '0.1'
        end
      end
      object TerrainScrollBox: TScrollBox
        Left = 105
        Top = 0
        Width = 565
        Height = 468
        Align = alClient
        TabOrder = 1
        ExplicitHeight = 475
        object Image32_Terrain: TImage32
          Left = 0
          Top = 0
          Width = 256
          Height = 256
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baTopLeft
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
          OnMouseDown = Image32_TerrainMouseDown
          OnMouseMove = Image32_TerrainMouseMove
        end
      end
    end
    object TSlopeTab: TTabSheet
      Caption = 'Slope'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TSlopeScrollBox: TScrollBox
        Left = 121
        Top = 0
        Width = 549
        Height = 468
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 475
        object Image32_Misc: TImage32
          Left = 0
          Top = 0
          Width = 512
          Height = 512
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baTopLeft
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
        end
      end
      object TSlopePanel: TPanel
        Left = 0
        Top = 0
        Width = 121
        Height = 468
        Align = alLeft
        TabOrder = 1
        ExplicitHeight = 475
        object ConvertSlopesBtn: TSpeedButton
          Left = 8
          Top = 224
          Width = 106
          Height = 22
          Caption = 'Save Slope.aef'
          OnClick = ConvertSlopesBtnClick
        end
        object SlopeMaxLabel: TLabel
          Left = 16
          Top = 176
          Width = 15
          Height = 13
          Caption = '0.3'
        end
        object SlopeMinLabel: TLabel
          Left = 16
          Top = 160
          Width = 15
          Height = 13
          Caption = '0.3'
        end
        object Label4: TLabel
          Left = 56
          Top = 136
          Width = 48
          Height = 13
          Caption = '0.01 .. 0.3'
        end
        object SaveMiscBitmapBtn: TButton
          Left = 8
          Top = 196
          Width = 106
          Height = 21
          Caption = 'Save bitmap'
          TabOrder = 0
          OnClick = SaveMiscBitmapBtnClick
        end
        object Button_Shadow: TButton
          Left = 24
          Top = 384
          Width = 75
          Height = 21
          Caption = 'Shadow'
          TabOrder = 1
          OnClick = Button_ShadowClick
        end
        object Button_Shaded: TButton
          Left = 24
          Top = 333
          Width = 75
          Height = 21
          Caption = 'Shaded'
          TabOrder = 2
          OnClick = Button_ShadedClick
        end
        object SlopeEdit: TEdit
          Left = 16
          Top = 136
          Width = 33
          Height = 21
          TabOrder = 3
          Text = '0.01'
        end
        object Button_Slopes: TButton
          Left = 16
          Top = 112
          Width = 75
          Height = 21
          Caption = 'Slopes'
          TabOrder = 4
          OnClick = Button_SlopesClick
        end
        object TSlopeSizeRG: TRadioGroup
          Left = 16
          Top = 16
          Width = 73
          Height = 89
          Caption = 'Slope  Size'
          ItemIndex = 1
          Items.Strings = (
            '256'
            '512'
            '1024'
            '2048'
            '4096')
          TabOrder = 5
          OnClick = TSlopeSizeRGClick
        end
        object Button_AATerrain: TButton
          Left = 8
          Top = 358
          Width = 50
          Height = 21
          Caption = 'AA Basic'
          TabOrder = 6
          OnClick = Button_AATerrainClick
        end
        object Button_AASweep: TButton
          Left = 59
          Top = 358
          Width = 56
          Height = 21
          Caption = 'AASweep'
          TabOrder = 7
          OnClick = Button_AASweepClick
        end
        object SlopeCheckBox: TCheckBox
          Left = 16
          Top = 312
          Width = 97
          Height = 17
          Caption = 'Slope or Texture'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object Slope2ColorPanel: TPanel
          Left = 2
          Top = 248
          Width = 16
          Height = 22
          Caption = '<2'
          TabOrder = 9
          OnClick = Slope2ColorPanelClick
        end
        object Slope2Edit: TEdit
          Left = 2
          Top = 272
          Width = 16
          Height = 21
          TabOrder = 10
          Text = '2'
        end
        object Slope15Edit: TEdit
          Left = 38
          Top = 272
          Width = 25
          Height = 21
          TabOrder = 11
          Text = '15'
        end
        object Slope15ColorPanel: TPanel
          Left = 38
          Top = 248
          Width = 25
          Height = 22
          Caption = '<15'
          TabOrder = 12
          OnClick = Slope15ColorPanelClick
        end
        object Slope45Edit: TEdit
          Left = 66
          Top = 272
          Width = 25
          Height = 21
          TabOrder = 13
          Text = '45'
        end
        object Slope45ColorPanel: TPanel
          Left = 66
          Top = 248
          Width = 25
          Height = 22
          Caption = '<45'
          TabOrder = 14
          OnClick = Slope45ColorPanelClick
        end
        object Slope99Edit: TEdit
          Left = 94
          Top = 272
          Width = 25
          Height = 21
          TabOrder = 15
          Text = '99'
        end
        object Slope99ColorPanel: TPanel
          Left = 94
          Top = 248
          Width = 25
          Height = 22
          Caption = '>45'
          TabOrder = 16
          OnClick = Slope99ColorPanelClick
        end
        object Slope7Edit: TEdit
          Left = 20
          Top = 272
          Width = 16
          Height = 21
          TabOrder = 17
          Text = '7'
        end
        object Slope7ColorPanel: TPanel
          Left = 20
          Top = 248
          Width = 16
          Height = 22
          Caption = '<7'
          TabOrder = 18
          OnClick = Slope7ColorPanelClick
        end
      end
    end
    object TextureTabSheet: TTabSheet
      Caption = 'Texture'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TextureScrollBox: TScrollBox
        Left = 169
        Top = 0
        Width = 501
        Height = 468
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 475
        object Image_Bitmap: TImage32
          Left = 0
          Top = 0
          Width = 512
          Height = 512
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baTopLeft
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
          OnMouseMove = Image_BitmapMouseMove
        end
      end
      object TexturePanel: TPanel
        Left = 0
        Top = 0
        Width = 169
        Height = 468
        Align = alLeft
        TabOrder = 1
        ExplicitHeight = 475
        object SlopeBtn: TSpeedButton
          Left = 8
          Top = 352
          Width = 65
          Height = 22
          Caption = 'Slope'
          OnClick = SlopeBtnClick
        end
        object SnowBtn: TSpeedButton
          Left = 8
          Top = 304
          Width = 65
          Height = 22
          Caption = 'Snow'
          OnClick = SnowBtnClick
        end
        object RockBtn: TSpeedButton
          Left = 8
          Top = 256
          Width = 65
          Height = 22
          Caption = 'Rock'
          OnClick = RockBtnClick
        end
        object GrassBtn: TSpeedButton
          Left = 8
          Top = 160
          Width = 65
          Height = 22
          Caption = 'Grass'
          OnClick = GrassBtnClick
        end
        object SandBtn: TSpeedButton
          Left = 8
          Top = 112
          Width = 73
          Height = 22
          Caption = 'Sand'
          OnClick = SandBtnClick
        end
        object Label5: TLabel
          Left = 2
          Top = 40
          Width = 38
          Height = 13
          Caption = 'Ambient'
        end
        object ForestBtn: TSpeedButton
          Left = 8
          Top = 208
          Width = 65
          Height = 22
          Caption = 'Forest'
          OnClick = ForestBtnClick
        end
        object GridPaintBtn: TSpeedButton
          Left = 8
          Top = 384
          Width = 73
          Height = 22
          Caption = 'Paint Grid'
          OnClick = GridPaintBtnClick
        end
        object SandEdit1: TEdit
          Left = 8
          Top = 136
          Width = 33
          Height = 21
          TabOrder = 0
          Text = '-2'
        end
        object SandEdit2: TEdit
          Left = 48
          Top = 136
          Width = 33
          Height = 21
          TabOrder = 1
          Text = '-1'
        end
        object SandEdit3: TEdit
          Left = 88
          Top = 136
          Width = 33
          Height = 21
          TabOrder = 2
          Text = '0.2'
        end
        object SandEdit4: TEdit
          Left = 128
          Top = 136
          Width = 33
          Height = 21
          TabOrder = 3
          Text = '0.3'
        end
        object SandPanel: TPanel
          Left = 120
          Top = 112
          Width = 41
          Height = 22
          TabOrder = 4
          OnClick = SandPanelClick
        end
        object ImageTextureSizeRG: TRadioGroup
          Left = 64
          Top = 8
          Width = 97
          Height = 57
          Caption = 'Texture Size'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            '512'
            '1024'
            '2048'
            '4096')
          TabOrder = 5
          OnClick = ImageTextureSizeRGClick
        end
        object Button_SaveBitmap: TButton
          Left = 4
          Top = 80
          Width = 55
          Height = 21
          Caption = 'Save...'
          TabOrder = 6
          OnClick = Button_SaveBitmapClick
        end
        object Button_CreateBitmap: TButton
          Left = 4
          Top = 2
          Width = 55
          Height = 21
          Caption = 'Create'
          TabOrder = 7
          OnClick = Button_CreateBitmapClick
        end
        object CheckBox_Shaded: TCheckBox
          Left = 4
          Top = 24
          Width = 57
          Height = 17
          Caption = 'Shaded'
          TabOrder = 8
        end
        object Edit_Ambient: TEdit
          Left = 2
          Top = 56
          Width = 55
          Height = 21
          TabOrder = 9
          Text = '0.15'
        end
        object GrassEdit3: TEdit
          Left = 88
          Top = 184
          Width = 33
          Height = 21
          TabOrder = 10
          Text = '0.4'
        end
        object GrassEdit4: TEdit
          Left = 128
          Top = 184
          Width = 33
          Height = 21
          TabOrder = 11
          Text = '0.5'
        end
        object GrassEdit1: TEdit
          Left = 8
          Top = 184
          Width = 33
          Height = 21
          TabOrder = 12
          Text = '0.2'
        end
        object GrassEdit2: TEdit
          Left = 48
          Top = 184
          Width = 33
          Height = 21
          TabOrder = 13
          Text = '0.3'
        end
        object GrassPanel: TPanel
          Left = 120
          Top = 160
          Width = 41
          Height = 22
          TabOrder = 14
          OnClick = GrassPanelClick
        end
        object RockEdit3: TEdit
          Left = 88
          Top = 280
          Width = 33
          Height = 21
          TabOrder = 15
          Text = '0.9'
        end
        object RockEdit4: TEdit
          Left = 128
          Top = 280
          Width = 33
          Height = 21
          TabOrder = 16
          Text = '0.95'
        end
        object RockPanel: TPanel
          Left = 120
          Top = 256
          Width = 41
          Height = 22
          TabOrder = 17
          OnClick = RockPanelClick
        end
        object SnowPanel: TPanel
          Left = 120
          Top = 304
          Width = 41
          Height = 22
          TabOrder = 18
          OnClick = SnowPanelClick
        end
        object RockEdit1: TEdit
          Left = 8
          Top = 280
          Width = 33
          Height = 21
          TabOrder = 19
          Text = '0.7'
        end
        object RockEdit2: TEdit
          Left = 48
          Top = 280
          Width = 33
          Height = 21
          TabOrder = 20
          Text = '0.8'
        end
        object SnowEdit1: TEdit
          Left = 8
          Top = 328
          Width = 33
          Height = 21
          TabOrder = 21
          Text = '0.9'
        end
        object SnowEdit2: TEdit
          Left = 48
          Top = 328
          Width = 33
          Height = 21
          TabOrder = 22
          Text = '0.95'
        end
        object SnowEdit3: TEdit
          Left = 88
          Top = 328
          Width = 33
          Height = 21
          TabOrder = 23
          Text = '2'
        end
        object SnowEdit4: TEdit
          Left = 128
          Top = 328
          Width = 33
          Height = 21
          TabOrder = 24
          Text = '2'
        end
        object ForestPanel: TPanel
          Left = 120
          Top = 208
          Width = 41
          Height = 22
          TabOrder = 25
          OnClick = ForestPanelClick
        end
        object ForestEdit1: TEdit
          Left = 8
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 26
          Text = '0.4'
        end
        object ForestEdit2: TEdit
          Left = 48
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 27
          Text = '0.5'
        end
        object ForestEdit3: TEdit
          Left = 88
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 28
          Text = '0.7'
        end
        object ForestEdit4: TEdit
          Left = 128
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 29
          Text = '0.8'
        end
        object GridSizeRG: TRadioGroup
          Left = 88
          Top = 376
          Width = 73
          Height = 92
          Caption = 'Grid Size'
          ItemIndex = 4
          Items.Strings = (
            '64x64'
            '32x32'
            '20x20'
            '16x16'
            '10x10'
            '8x8')
          TabOrder = 30
        end
        object GridColorPanel: TPanel
          Left = 8
          Top = 408
          Width = 73
          Height = 14
          TabOrder = 31
          OnClick = GridColorPanelClick
        end
        object FlipnSaveBtn: TButton
          Left = 72
          Top = 80
          Width = 83
          Height = 21
          Caption = 'Flip n Save...'
          TabOrder = 32
          OnClick = FlipnSaveBtnClick
        end
        object LoadTextureButton: TButton
          Left = 10
          Top = 426
          Width = 50
          Height = 21
          Caption = 'Load'
          TabOrder = 33
          OnClick = LoadTextureButtonClick
        end
      end
    end
    object TerrainAttributesTabSheet: TTabSheet
      Caption = 'Terrain Attributes'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TerrainAttributesScrollBox: TScrollBox
        Left = 209
        Top = 0
        Width = 461
        Height = 468
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 475
        object ImageTA32: TImage32
          Left = 0
          Top = 0
          Width = 512
          Height = 512
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baTopLeft
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
          OnMouseDown = ImageTA32MouseDown
          OnMouseMove = ImageTA32MouseMove
          OnMouseUp = ImageTA32MouseUp
        end
      end
      object TerrainAttributesPanel: TPanel
        Left = 0
        Top = 0
        Width = 209
        Height = 468
        Align = alLeft
        TabOrder = 1
        ExplicitHeight = 475
        object AttributeSetBtn: TSpeedButton
          Left = 166
          Top = 96
          Width = 36
          Height = 22
          Caption = 'Set'
          OnClick = AttributeSetBtnClick
        end
        object AttributeImageSizeRG: TRadioGroup
          Left = 78
          Top = 2
          Width = 65
          Height = 90
          Caption = 'Image Size'
          ItemIndex = 1
          Items.Strings = (
            '256'
            '512'
            '1024'
            '2048'
            '4096')
          TabOrder = 0
          OnClick = AttributeImageSizeRGClick
        end
        object AttributeNewImageBtn: TButton
          Left = 4
          Top = 2
          Width = 34
          Height = 17
          Caption = 'New'
          TabOrder = 1
          OnClick = AttributeNewImageBtnClick
        end
        object AttributeSaveImageBtn: TButton
          Left = 4
          Top = 72
          Width = 69
          Height = 17
          Caption = 'Save Map...'
          TabOrder = 2
          OnClick = AttributeSaveImageBtnClick
        end
        object AttributeLoadBackgroundBtn: TButton
          Left = 4
          Top = 20
          Width = 69
          Height = 17
          Caption = 'Background'
          TabOrder = 3
          OnClick = AttributeLoadBackgroundBtnClick
        end
        object AttributeEdit: TEdit
          Left = 120
          Top = 96
          Width = 33
          Height = 21
          TabOrder = 4
          Text = '65'
        end
        object AttributeColorPanel: TPanel
          Left = 80
          Top = 96
          Width = 34
          Height = 22
          BevelOuter = bvNone
          Color = clBlue
          TabOrder = 5
          OnClick = AttributeColorPanelClick
        end
        object TerrainAttributesTileSizeRG: TRadioGroup
          Left = 142
          Top = 2
          Width = 65
          Height = 90
          Caption = 'Tile Size'
          ItemIndex = 5
          Items.Strings = (
            '32x32'
            '16x16'
            '8x8'
            '4x4'
            '2x2'
            '1x1')
          TabOrder = 6
          OnClick = TerrainAttributesTileSizeRGClick
        end
        object LayerAlphaTB: TTrackBar
          Left = 2
          Top = 112
          Width = 64
          Height = 15
          Max = 254
          PageSize = 20
          Frequency = 25
          Position = 254
          TabOrder = 7
          ThumbLength = 11
          OnChange = LayerAlphaTBChange
        end
        object PaintMapCB: TCheckBox
          Left = 4
          Top = 96
          Width = 70
          Height = 17
          Caption = 'Paint Map'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object AttributeSaveBackgroundBtn: TButton
          Left = 4
          Top = 37
          Width = 69
          Height = 17
          Caption = 'Save Back...'
          TabOrder = 9
          OnClick = AttributeSaveBackgroundBtnClick
        end
        object PageControl3: TPageControl
          Left = 2
          Top = 352
          Width = 73
          Height = 121
          ActivePage = TabSheet24
          MultiLine = True
          TabOrder = 10
          object TabSheet1: TTabSheet
            Caption = '**G Cost**'
            ImageIndex = 8
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object agfSaveBtn: TSpeedButton
              Left = 0
              Top = 32
              Width = 57
              Height = 18
              Caption = 'Save .agf'
              OnClick = agfSaveBtnClick
            end
            object agfOpenBtn: TSpeedButton
              Tag = 8
              Left = 0
              Top = 0
              Width = 57
              Height = 18
              Caption = 'Open .agf'
              OnClick = agfOpenBtnClick
            end
            object agfEdit: TEdit
              Left = 0
              Top = 16
              Width = 55
              Height = 18
              TabOrder = 0
            end
          end
          object TabSheet4: TTabSheet
            Caption = '**MAG B**'
            ImageIndex = 11
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object abfOpenBtn: TSpeedButton
              Tag = 8
              Left = 0
              Top = 0
              Width = 57
              Height = 18
              Caption = 'Open .abf'
              OnClick = abfOpenBtnClick
            end
            object abfSaveBtn: TSpeedButton
              Left = 0
              Top = 32
              Width = 57
              Height = 18
              Caption = 'Save .abf'
              OnClick = abfSaveBtnClick
            end
            object abfEdit: TEdit
              Left = 0
              Top = 16
              Width = 55
              Height = 18
              TabOrder = 0
            end
          end
          object TabSheet24: TTabSheet
            Caption = '**Map Att**'
            ImageIndex = 11
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object amfOpenBtn: TSpeedButton
              Tag = 4
              Left = 0
              Top = 0
              Width = 60
              Height = 18
              Caption = 'Open .amf'
              OnClick = amfOpenBtnClick
            end
            object amfSaveBtn: TSpeedButton
              Left = 0
              Top = 32
              Width = 60
              Height = 18
              Caption = 'Save .amf'
              OnClick = amfSaveBtnClick
            end
            object amfEdit: TEdit
              Left = 1
              Top = 16
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
        end
        object TAGroupBox: TGroupBox
          Left = 2
          Top = 128
          Width = 76
          Height = 164
          Caption = '  Big Brush'
          TabOrder = 11
          object TAConvertBtn: TSpeedButton
            Left = 28
            Top = 104
            Width = 44
            Height = 22
            Caption = 'Convert'
            OnClick = TAConvertBtnClick
          end
          object TALabel: TLabel
            Left = 8
            Top = 16
            Width = 59
            Height = 39
            Caption = 'Paint ONLY Conversion Required'
            WordWrap = True
          end
          object Label3: TLabel
            Left = 32
            Top = 72
            Width = 33
            Height = 13
            Caption = 'Radius'
          end
          object TARadiusLabel: TLabel
            Left = 32
            Top = 88
            Width = 12
            Height = 13
            Caption = '20'
          end
          object TARePaintBtn: TSpeedButton
            Left = 28
            Top = 128
            Width = 44
            Height = 22
            Caption = 'RePaint'
            OnClick = TARePaintBtnClick
          end
          object TABigBrushCB: TCheckBox
            Left = 32
            Top = 56
            Width = 17
            Height = 17
            TabOrder = 0
          end
          object TAScrollBar: TScrollBar
            Left = 4
            Top = 60
            Width = 16
            Height = 100
            Kind = sbVertical
            Min = 1
            PageSize = 0
            Position = 20
            TabOrder = 1
            OnChange = TAScrollBarChange
          end
        end
        object AttributeLoadImageBtn: TButton
          Left = 4
          Top = 55
          Width = 69
          Height = 17
          Caption = 'Load Map...'
          TabOrder = 12
          OnClick = AttributeLoadImageBtnClick
        end
        object AttributeNewBaseImageBtn: TButton
          Left = 38
          Top = 2
          Width = 34
          Height = 17
          Caption = 'Base'
          TabOrder = 13
          OnClick = AttributeNewBaseImageBtnClick
        end
        object AttributeRG: TRadioGroup
          Left = 80
          Top = 120
          Width = 121
          Height = 353
          Caption = 'Map Attributes'
          ItemIndex = 18
          Items.Strings = (
            'x-Base'
            'T-Hiway'
            'T-Road'
            'T-Street'
            'T-Dirt Trail'
            'S-Bare'
            'S-Plowed field'
            'S-Rocky Boulders'
            'S-Muck'
            'S-Swamp'
            'S-Swamp Lake'
            'S-Sand  hard pack'
            'S-Sand Dunes'
            'V-Grass'
            'V-Mixed'
            'V-Shrub'
            'V-Forest'
            'V-Jungle'
            'H-Stream fordable'
            'H-River'
            'H-Lake'
            'H-Snow<1 ft'
            'H-Snow>1 ft'
            'H-Ice'
            'U-Housing'
            'U-Urban areas'
            'O-NoGo areas')
          TabOrder = 14
          OnClick = AttributeRGClick
        end
        object LayerTwixTB: TTrackBar
          Left = 38
          Top = 294
          Width = 42
          Height = 15
          Max = 254
          PageSize = 20
          Frequency = 25
          TabOrder = 15
          ThumbLength = 11
          OnChange = LayerTwixTBChange
        end
        object AttributeLoadTwixBtn: TButton
          Left = 4
          Top = 294
          Width = 34
          Height = 17
          Caption = 'Twix'
          TabOrder = 16
          OnClick = AttributeLoadTwixBtnClick
        end
        object AttributeLoadUnitBtn: TButton
          Left = 4
          Top = 318
          Width = 34
          Height = 17
          Caption = 'Units'
          TabOrder = 17
          OnClick = AttributeLoadUnitBtnClick
        end
        object LayerUnitTB: TTrackBar
          Left = 38
          Top = 318
          Width = 42
          Height = 15
          Max = 254
          PageSize = 20
          Frequency = 25
          TabOrder = 18
          ThumbLength = 11
          OnChange = LayerUnitTBChange
        end
        object LayerUnitSizeTB: TTrackBar
          Left = 38
          Top = 334
          Width = 42
          Height = 15
          Max = 100
          Min = 1
          PageSize = 20
          Frequency = 25
          Position = 10
          TabOrder = 19
          ThumbLength = 11
          OnChange = LayerUnitTBChange
        end
      end
    end
    object ToDoTabsheet: TTabSheet
      Caption = 'To Do: Import data into Terrain Layers '
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ToDoPanel: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 468
        Align = alLeft
        TabOrder = 0
        ExplicitHeight = 475
        object ToDoPageControl: TPageControl
          Left = 2
          Top = 56
          Width = 73
          Height = 409
          ActivePage = TabSheet3
          MultiLine = True
          TabOrder = 0
          object TabSheet7: TTabSheet
            Caption = '**G Cost**'
            ImageIndex = 8
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object SaveagfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 57
              Height = 22
              Caption = 'Save .agf'
            end
            object OpenagfBtn: TSpeedButton
              Tag = 8
              Left = 0
              Top = 0
              Width = 57
              Height = 22
              Caption = 'Open .agf'
            end
            object FileAgfEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
          object TabSheet9: TTabSheet
            Caption = 'NO GO'
            ImageIndex = 10
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenanfBtn: TSpeedButton
              Tag = 9
              Left = 0
              Top = 8
              Width = 57
              Height = 22
              Caption = 'Open .anf'
            end
            object SaveanfBtn: TSpeedButton
              Left = 0
              Top = 56
              Width = 57
              Height = 22
              Caption = 'Save .anf'
            end
            object FileAnfEdit: TEdit
              Left = 0
              Top = 32
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
          object TabSheet10: TTabSheet
            Caption = '**MAG B**'
            ImageIndex = 11
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenabfBtn: TSpeedButton
              Tag = 8
              Left = 0
              Top = 0
              Width = 57
              Height = 22
              Caption = 'Open .abf'
            end
            object SaveabfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 57
              Height = 22
              Caption = 'Save .abf'
            end
            object FileAbfEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
          object TabSheet18: TTabSheet
            Caption = '-Elevation-'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenaefBtn: TSpeedButton
              Tag = 1
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .aef'
            end
            object SaveaefBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .aef'
            end
            object FileAefEdit: TEdit
              Left = 1
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
          object TabSheet19: TTabSheet
            Caption = 'Vegetation'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenavfBtn: TSpeedButton
              Tag = 4
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .avf'
            end
            object SaveavfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .avf'
            end
            object FileAvfEdit: TEdit
              Left = 1
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox1: TCheckBox
              Left = 0
              Top = 72
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet20: TTabSheet
            Caption = 'Soils'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenasfBtn: TSpeedButton
              Tag = 2
              Left = 0
              Top = 0
              Width = 57
              Height = 22
              Caption = 'Open .asf'
            end
            object SaveasfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .asf'
            end
            object FileAsfEdit: TEdit
              Left = 1
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox7: TCheckBox
              Left = 0
              Top = 72
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet21: TTabSheet
            Caption = 'Hydrology'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenahfBtn: TSpeedButton
              Tag = 3
              Left = 0
              Top = 0
              Width = 57
              Height = 22
              Caption = 'Open .ahf'
            end
            object SaveahfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .ahf'
            end
            object Label59: TLabel
              Left = 0
              Top = 72
              Width = 3
              Height = 13
            end
            object FileAhfEdit: TEdit
              Left = 1
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox6: TCheckBox
              Left = 8
              Top = 80
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet6: TTabSheet
            Caption = 'Transport'
            ImageIndex = 4
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenatfBtn: TSpeedButton
              Tag = 5
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .atf'
            end
            object SaveatfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .atf'
            end
            object FileAtfEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox5: TCheckBox
              Left = 0
              Top = 72
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet22: TTabSheet
            Caption = 'Urban'
            ImageIndex = 5
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenaafBtn: TSpeedButton
              Tag = 5
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .aaf'
            end
            object SaveaafBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .aaf'
            end
            object FileAafEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox4: TCheckBox
              Left = 0
              Top = 70
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet23: TTabSheet
            Caption = 'Obstacles'
            ImageIndex = 6
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenaofBtn: TSpeedButton
              Tag = 6
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .aof'
            end
            object SaveaofBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .aof'
            end
            object FileAofEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox3: TCheckBox
              Left = 0
              Top = 70
              Width = 17
              Height = 17
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
          object TabSheet5: TTabSheet
            Caption = 'Influence'
            ImageIndex = 7
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenaifBtn: TSpeedButton
              Tag = 7
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .aif'
            end
            object SaveaifBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .aif'
            end
            object FileAifEdit: TEdit
              Left = 0
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
          end
          object TabSheet3: TTabSheet
            Caption = '**Map Att**'
            ImageIndex = 11
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object OpenamfBtn: TSpeedButton
              Tag = 4
              Left = 0
              Top = 0
              Width = 60
              Height = 22
              Caption = 'Open .amf'
            end
            object SaveamfBtn: TSpeedButton
              Left = 0
              Top = 48
              Width = 60
              Height = 22
              Caption = 'Save .amf'
            end
            object FileAmfEdit: TEdit
              Left = 1
              Top = 24
              Width = 55
              Height = 21
              TabOrder = 0
            end
            object CheckBox2: TCheckBox
              Left = 0
              Top = 70
              Width = 17
              Height = 17
              TabOrder = 1
            end
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 496
    Width = 678
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Panels = <
      item
        Alignment = taCenter
        Text = 'Layer Inactive'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'X: 1200 Y: 1200'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'X: 120 Y: 120'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'Hints'
        Width = 50
      end>
    UseSystemFont = False
    ExplicitTop = 503
  end
  object OpenDialogData: TOpenDialog
    Left = 408
    Top = 32
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Left = 456
    Top = 30
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 497
    Top = 34
  end
  object OpenDialog: TOpenPictureDialog
    Left = 336
    Top = 32
  end
end
