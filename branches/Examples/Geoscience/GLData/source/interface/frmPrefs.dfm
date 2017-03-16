object formPreferences: TformPreferences
  Left = 233
  Top = 239
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'glData Preferences'
  ClientHeight = 373
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 373
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 0
    object tvPrefs: TTreeView
      Left = 1
      Top = 1
      Width = 159
      Height = 314
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HotTrack = True
      Images = ImageList
      Indent = 19
      ParentFont = False
      TabOrder = 0
      OnClick = tvPrefsClick
      Items.NodeData = {
        03070000002C0000000000000001000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107470065006E006500720061006C003000000000000000010000
        00FFFFFFFFFFFFFFFF000000000000000000000000010949006E007400650072
        0066006100630065002C0000000000000001000000FFFFFFFFFFFFFFFF000000
        000000000005000000010744006900730070006C006100790026000000000000
        0001000000FFFFFFFFFFFFFFFF00000000000000000000000001044100780065
        0073002A0000000000000001000000FFFFFFFFFFFFFFFF000000000000000000
        0000000106430061006D00650072006100240000000000000001000000FFFFFF
        FFFFFFFFFF00000000000000000000000001034800550044002E000000000000
        0001000000FFFFFFFFFFFFFFFF00000000000000000000000001084C00690067
        006800740069006E006700280000000000000001000000FFFFFFFFFFFFFFFF00
        000000000000000000000001055300630061006C006500340000000000000001
        000000FFFFFFFFFFFFFFFF000000000000000004000000010B5300630061006C
        006100720020004400610074006100280000000000000001000000FFFFFFFFFF
        FFFFFF00000000000000000000000001054C0069006E00650073002C00000000
        00000001000000FFFFFFFFFFFFFFFF00000000000000000000000001074D0061
        0072006B00650072007300260000000000000001000000FFFFFFFFFFFFFFFF00
        0000000000000000000000010450006900700065003200000000000000000000
        00FFFFFFFFFFFFFFFF000000000000000000000000010A5300630061006C0061
        007200200042006F007800340000000000000000000000FFFFFFFFFFFFFFFF00
        0000000000000001000000010B56006500630074006F00720020004400610074
        006100320000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        000000010A56006500630074006F007200200042006F00780028000000000000
        0001000000FFFFFFFFFFFFFFFF00000000000000000200000001054700720069
        00640073002A0000000000000001000000FFFFFFFFFFFFFFFF00000000000000
        00000000000106530075007200660065007200360000000000000001000000FF
        FFFFFFFFFFFFFF000000000000000000000000010C4500530052004900200041
        007200630049006E0066006F00320000000000000000000000FFFFFFFFFFFFFF
        FF000000000000000000000000010A470065006F0074006800650072006D0061
        006C00}
    end
    object bCancel: TBitBtn
      Left = 3
      Top = 345
      Width = 75
      Height = 25
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = bkCancel
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 1
      OnClick = bCancelClick
    end
    object bOK: TBitBtn
      Left = 3
      Top = 317
      Width = 75
      Height = 25
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = bkOK
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 2
      OnClick = bOKClick
    end
    object bUpdate: TBitBtn
      Left = 80
      Top = 317
      Width = 75
      Height = 25
      Caption = 'Update'
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 3
      OnClick = bUpdateClick
    end
  end
  object pnlOptions: TPanel
    Left = 161
    Top = 0
    Width = 383
    Height = 373
    Align = alClient
    TabOrder = 1
    object pcPreferences: TPageControl
      Left = 1
      Top = 1
      Width = 381
      Height = 352
      ActivePage = tsDisplay
      Align = alClient
      TabOrder = 0
      object tsGeneral: TTabSheet
        Caption = 'General'
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbxAutoProcess: TCheckBox
          Left = 8
          Top = 8
          Width = 226
          Height = 17
          Caption = 'Automatically Process on Import'
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
      end
      object tsDisplay: TTabSheet
        Caption = 'Display'
        ImageIndex = 1
        TabVisible = False
        object lblColourPoint: TLabel
          Left = 6
          Top = 8
          Width = 90
          Height = 13
          Caption = 'Background Colour'
        end
        object lblAntialiasing: TLabel
          Left = 8
          Top = 68
          Width = 54
          Height = 13
          Caption = 'Antialiasing'
        end
        object lblShadeModel: TLabel
          Left = 8
          Top = 92
          Width = 61
          Height = 13
          Caption = 'Shade Model'
        end
        object pnlBackgroundColour: TPanel
          Left = 102
          Top = 2
          Width = 25
          Height = 25
          Hint = 'Click to change background colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clWhite
          TabOrder = 0
          OnClick = pnlBackgroundColourClick
        end
        object cbxTwoSideLighting: TCheckBox
          Left = 6
          Top = 38
          Width = 231
          Height = 17
          Caption = 'Two Side Lighting'
          TabOrder = 1
          OnClick = UpdatedModifiedOnly
        end
        object cbAntialiasing: TComboBox
          Left = 90
          Top = 64
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 1
          TabOrder = 2
          Text = 'Default'
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'None'
            'Default'
            '2x'
            '2x HQ'
            '4x'
            '4x HQ')
        end
        object cbShadeModel: TComboBox
          Left = 90
          Top = 88
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 3
          Text = 'Default'
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'Default'
            'Flat'
            'Smooth')
        end
      end
      object tsSurfer: TTabSheet
        Caption = 'Surfer'
        ImageIndex = 2
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblTileSurferX: TLabel
          Left = 18
          Top = 117
          Width = 77
          Height = 13
          Caption = 'Tile Texture in X'
        end
        object lblTileSurferY: TLabel
          Left = 150
          Top = 117
          Width = 77
          Height = 13
          Caption = 'Tile Texture in Y'
        end
        object lblSurferBaseMap: TLabel
          Left = 5
          Top = 75
          Width = 54
          Height = 13
          Caption = 'Base Map'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblSurferPalette: TLabel
          Left = 5
          Top = 137
          Width = 41
          Height = 13
          Caption = 'Palette'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblSurferSingleColour: TLabel
          Left = 180
          Top = 167
          Width = 62
          Height = 13
          Caption = 'Single Colour'
        end
        object lblSurferMinColour: TLabel
          Left = 192
          Top = 194
          Width = 50
          Height = 13
          Caption = 'Min Colour'
        end
        object lblSurferMaxColour: TLabel
          Left = 188
          Top = 221
          Width = 54
          Height = 13
          Caption = 'Max Colour'
        end
        object lblSurferBlankedColour: TLabel
          Left = 171
          Top = 247
          Width = 71
          Height = 13
          Caption = 'Blanked Colour'
        end
        object lblSurferCLRFile: TLabel
          Left = 5
          Top = 276
          Width = 38
          Height = 13
          Caption = 'CLR File'
        end
        object lblSurferAlpha: TLabel
          Left = 133
          Top = 5
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblSurferPolygonMode: TLabel
          Left = 221
          Top = 5
          Width = 67
          Height = 13
          Caption = 'Polygon Mode'
        end
        object lblImportSurfer: TLabel
          Left = 5
          Top = 34
          Width = 40
          Height = 13
          Caption = 'Import'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbxPromptTexSurfer: TCheckBox
          Left = 16
          Top = 93
          Width = 247
          Height = 17
          Caption = 'Prompt for a basemap texture on import'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object rgSurferOptions: TRadioGroup
          Left = 5
          Top = 152
          Width = 153
          Height = 117
          Caption = 'Options'
          ItemIndex = 0
          Items.Strings = (
            'Single Colour'
            'Simple Range'
            'Rainbow'
            'Inverse Rainbow '
            'Palette .clr File'
            'Inverse Palette .clr File')
          TabOrder = 1
          OnClick = UpdatedSurferModified
        end
        object pnlSurferColour: TPanel
          Left = 251
          Top = 161
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGreen
          ParentShowHint = False
          ShowHint = False
          TabOrder = 2
          OnClick = pnlSurferColourClick
        end
        object pnlSurferMinColour: TPanel
          Left = 251
          Top = 187
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          ParentShowHint = False
          ShowHint = False
          TabOrder = 3
          OnClick = pnlSurferMinColourClick
        end
        object pnlSurferMaxColour: TPanel
          Left = 251
          Top = 214
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clFuchsia
          ParentShowHint = False
          ShowHint = False
          TabOrder = 4
          OnClick = pnlSurferMaxColourClick
        end
        object pnlSurferBlankedColour: TPanel
          Left = 251
          Top = 241
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clSilver
          ParentShowHint = False
          ShowHint = False
          TabOrder = 5
          OnClick = pnlSurferBlankedColourClick
        end
        object ebSurferCLRFile: TEdit
          Left = 28
          Top = 294
          Width = 345
          Height = 21
          TabOrder = 6
          OnChange = ebSurferCLRFileChange
        end
        object bLoadSurferCLR: TButton
          Left = 5
          Top = 294
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 7
          OnClick = bLoadSurferCLRClick
        end
        object cbSurferColourMode: TComboBox
          Left = 4
          Top = 1
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          ItemIndex = 3
          TabOrder = 8
          Text = 'Emission'
          OnChange = UpdatedSurferModified
          Items.Strings = (
            'Ambient'
            'Ambient & Diffuse'
            'Diffuse'
            'Emission'
            'None')
        end
        object cbSurferPolygonMode: TComboBox
          Left = 295
          Top = 1
          Width = 75
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 9
          OnChange = UpdatedSurferModified
          Items.Strings = (
            'Fill'
            'Wireframe'
            'Points')
        end
        object cbxSurferTwoSided: TCheckBox
          Left = 271
          Top = 26
          Width = 99
          Height = 17
          Caption = 'Two Sided Mesh'
          Checked = True
          State = cbChecked
          TabOrder = 10
          OnClick = UpdatedModifiedOnly
        end
        object cbxSurferSilentImport: TCheckBox
          Left = 16
          Top = 54
          Width = 88
          Height = 17
          Caption = 'Silent Import'
          Checked = True
          State = cbChecked
          TabOrder = 11
          OnClick = UpdatedModifiedOnly
        end
        object cbxSurferSilentLoad: TCheckBox
          Left = 118
          Top = 54
          Width = 121
          Height = 17
          Caption = 'Silent MetaFile Load'
          Checked = True
          State = cbChecked
          TabOrder = 12
          OnClick = UpdatedModifiedOnly
        end
        object geTileSurferX: TGEIntegerEdit
          Left = 100
          Top = 113
          Width = 45
          Height = 21
          Text = '0'
          MinValue = 1
          MaxValue = 1
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 13
          OnExit = UpdatedSurferModified
        end
        object geTileSurferY: TGEIntegerEdit
          Left = 235
          Top = 113
          Width = 45
          Height = 21
          Text = '0'
          MinValue = 1
          MaxValue = 1
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 14
          OnExit = UpdatedSurferModified
        end
        object geSurferAlpha: TGEFloatEdit
          Left = 166
          Top = 1
          Width = 45
          Height = 21
          Text = '0'
          MaxValue = 1.000000000000000000
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = True
          Validate = vdSilent
          TabOrder = 15
          OnExit = UpdatedSurferModified
        end
        object cbxSurferCreateVisible: TCheckBox
          Left = 271
          Top = 54
          Width = 99
          Height = 17
          Caption = 'Create Visible'
          Checked = True
          State = cbChecked
          TabOrder = 16
          OnClick = UpdateModifiedOnly
        end
      end
      object tsAxes: TTabSheet
        Caption = 'tsAxes'
        ImageIndex = 3
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbxDisplayAxes: TCheckBox
          Left = 8
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Display Axes'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
      end
      object tsMarkers: TTabSheet
        Caption = 'tsMarkers'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblPointRadius: TLabel
          Left = 17
          Top = 231
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = 'Marker Radius'
        end
        object lblPointColour: TLabel
          Left = 18
          Top = 256
          Width = 67
          Height = 13
          Caption = 'Marker Colour'
        end
        object lblPointStyle: TLabel
          Left = 191
          Top = 32
          Width = 51
          Height = 13
          Caption = 'Point Style'
        end
        object cbxDisplayMarkers: TCheckBox
          Left = 10
          Top = 204
          Width = 97
          Height = 17
          Caption = 'Display Markers'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object pnlColourSetting: TPanel
          Left = 95
          Top = 250
          Width = 25
          Height = 25
          Hint = 'Click to change point colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clRed
          TabOrder = 1
          OnClick = pnlColourSettingClick
        end
        object cbxPointWireFrame: TCheckBox
          Left = 215
          Top = 229
          Width = 97
          Height = 17
          Caption = 'Wire Frame'
          TabOrder = 2
          OnClick = UpdatedModifiedOnly
        end
        object cbxAutoCenterPoints: TCheckBox
          Left = 133
          Top = 255
          Width = 239
          Height = 17
          Caption = 'Auto Center Markers on Process'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = UpdatedModifiedOnly
        end
        object cbxDisplayPoints: TCheckBox
          Left = 7
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Display Points'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = UpdatedModifiedOnly
        end
        object cbxRenderNullPoints: TCheckBox
          Left = 30
          Top = 30
          Width = 142
          Height = 17
          Caption = 'Render Null Value Points'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = UpdatedModifiedOnly
        end
        object geMarkerRadius: TGEFloatEdit
          Left = 95
          Top = 227
          Width = 100
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 6
          OnExit = UpdatedModifiedOnly
        end
        object cbPointStyle: TComboBox
          Left = 250
          Top = 28
          Width = 118
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          ItemIndex = 3
          TabOrder = 7
          Text = 'Square'
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'Round'
            'Smooth'
            'Smooth Additive'
            'Square'
            'Square Additive')
        end
      end
      object tsLines: TTabSheet
        Caption = 'Lines'
        ImageIndex = 5
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblColourLine: TLabel
          Left = 40
          Top = 65
          Width = 53
          Height = 13
          Caption = 'Line Colour'
        end
        object lblLineMode: TLabel
          Left = 43
          Top = 43
          Width = 48
          Height = 13
          Caption = 'Line Mode'
        end
        object cbxDisplayLine: TCheckBox
          Left = 8
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Display Line'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object pnlColourLine: TPanel
          Left = 97
          Top = 60
          Width = 25
          Height = 25
          Hint = 'Click to change line colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = pnlColourLineClick
        end
        object cbLineMode: TComboBox
          Left = 97
          Top = 37
          Width = 89
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          ItemIndex = 1
          TabOrder = 2
          Text = 'Lines'
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'Cubic Spline'
            'Lines')
        end
      end
      object tsCamera: TTabSheet
        Caption = 'Camera'
        ImageIndex = 6
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblCameraDepthOfView: TLabel
          Left = 71
          Top = 53
          Width = 67
          Height = 13
          Alignment = taRightJustify
          Caption = 'Depth of View'
        end
        object lblCameraStyle: TLabel
          Left = 8
          Top = 8
          Width = 64
          Height = 13
          Caption = 'Camera Style'
        end
        object lblFocalLength: TLabel
          Left = 77
          Top = 31
          Width = 61
          Height = 13
          Alignment = taRightJustify
          Caption = 'Focal Length'
        end
        object lblCameraNearPlaneBias: TLabel
          Left = 64
          Top = 74
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = 'Near Plane Bias'
        end
        object cbCameraStyle: TComboBox
          Left = 84
          Top = 4
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          ItemIndex = 2
          TabOrder = 0
          Text = 'Perspective'
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'Orthogonal'
            'Orthogonal 2D'
            'Perspective')
        end
        object geFocalLength: TGEFloatEdit
          Left = 144
          Top = 27
          Width = 85
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = False
          UseMaxBound = False
          Validate = vdNone
          TabOrder = 1
          OnExit = UpdatedModifiedOnly
        end
        object geDepthOfView: TGEFloatEdit
          Left = 144
          Top = 49
          Width = 85
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = False
          UseMaxBound = False
          Validate = vdNone
          TabOrder = 2
          OnExit = UpdatedModifiedOnly
        end
        object geCameraNearPlaneBias: TGEFloatEdit
          Left = 144
          Top = 70
          Width = 85
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 3
          OnExit = UpdatedModifiedOnly
        end
      end
      object tsScale: TTabSheet
        Caption = 'Scale'
        ImageIndex = 7
        TabVisible = False
        object lblScaleZ: TLabel
          Left = 24
          Top = 51
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale Factor Z'
        end
        object lblScaleY: TLabel
          Left = 24
          Top = 28
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale Factor Y'
        end
        object lblScaleX: TLabel
          Left = 24
          Top = 6
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scale Factor X'
        end
        object bResetScale: TButton
          Left = 209
          Top = 22
          Width = 75
          Height = 25
          Caption = 'Reset 1:1:1'
          TabOrder = 0
          OnClick = bResetScaleClick
        end
        object geScaleX: TGEFloatEdit
          Left = 100
          Top = 2
          Width = 100
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 1
          OnExit = UpdatedModifiedOnly
        end
        object geScaleY: TGEFloatEdit
          Left = 100
          Top = 24
          Width = 100
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 2
          OnExit = UpdatedModifiedOnly
        end
        object geScaleZ: TGEFloatEdit
          Left = 100
          Top = 47
          Width = 100
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 3
          OnExit = UpdatedModifiedOnly
        end
      end
      object tsLighting: TTabSheet
        Caption = 'Lighting'
        ImageIndex = 8
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbxLightingShining: TCheckBox
          Left = 4
          Top = 4
          Width = 205
          Height = 17
          Caption = 'Light 1 Shining'
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object cbxLighting2Shining: TCheckBox
          Left = 4
          Top = 28
          Width = 205
          Height = 17
          Caption = 'Light 2 Shining'
          TabOrder = 1
          OnClick = UpdatedModifiedOnly
        end
      end
      object tsPipes: TTabSheet
        Caption = 'Pipes'
        ImageIndex = 9
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblPipeRadius: TLabel
          Left = 31
          Top = 31
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Radius'
        end
        object cbxShowPipes: TCheckBox
          Left = 8
          Top = 4
          Width = 97
          Height = 17
          Caption = 'Show Pipe'
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object gePipeRadius: TGEFloatEdit
          Left = 70
          Top = 27
          Width = 121
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 1
          OnExit = UpdatedModifiedOnly
        end
      end
      object tsArcInfo: TTabSheet
        Caption = 'ArcInfo'
        ImageIndex = 10
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblArcInfoPalette: TLabel
          Left = 5
          Top = 137
          Width = 41
          Height = 13
          Caption = 'Palette'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblArcInfoCLRFile: TLabel
          Left = 5
          Top = 276
          Width = 38
          Height = 13
          Caption = 'CLR File'
        end
        object lblArcInfoBlankedColour: TLabel
          Left = 171
          Top = 247
          Width = 71
          Height = 13
          Caption = 'Blanked Colour'
        end
        object lblArcInfoMaxColour: TLabel
          Left = 188
          Top = 221
          Width = 54
          Height = 13
          Caption = 'Max Colour'
        end
        object lblArcInfoMinColour: TLabel
          Left = 192
          Top = 194
          Width = 50
          Height = 13
          Caption = 'Min Colour'
        end
        object lblArcInfoSingleColour: TLabel
          Left = 180
          Top = 167
          Width = 62
          Height = 13
          Caption = 'Single Colour'
        end
        object lblTileArcInfoY: TLabel
          Left = 150
          Top = 117
          Width = 77
          Height = 13
          Caption = 'Tile Texture in Y'
        end
        object lblTileArcInfoX: TLabel
          Left = 18
          Top = 117
          Width = 77
          Height = 13
          Caption = 'Tile Texture in X'
        end
        object lblArcInfoBaseMap: TLabel
          Left = 5
          Top = 75
          Width = 54
          Height = 13
          Caption = 'Base Map'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblArcInfoPolygonMode: TLabel
          Left = 221
          Top = 5
          Width = 67
          Height = 13
          Caption = 'Polygon Mode'
        end
        object Label2: TLabel
          Left = 133
          Top = 5
          Width = 27
          Height = 13
          Caption = 'Alpha'
        end
        object lblImportArcInfo: TLabel
          Left = 5
          Top = 34
          Width = 40
          Height = 13
          Caption = 'Import'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object rgArcInfoOptions: TRadioGroup
          Left = 5
          Top = 152
          Width = 153
          Height = 117
          Caption = 'Options'
          ItemIndex = 0
          Items.Strings = (
            'Single Colour'
            'Simple Range'
            'Rainbow'
            'Inverse Rainbow '
            'Palette .clr File'
            'Inverse Palette .clr File')
          TabOrder = 0
          OnClick = UpdateModifiedOnly
        end
        object bLoadArcInfoCLR: TButton
          Left = 5
          Top = 294
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = bLoadArcInfoCLRClick
        end
        object ebArcInfoCLRFile: TEdit
          Left = 28
          Top = 294
          Width = 345
          Height = 21
          TabOrder = 2
          OnChange = ebArcInfoCLRFileChange
        end
        object pnlArcInfoColour: TPanel
          Left = 251
          Top = 161
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGreen
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = pnlArcInfoColourClick
        end
        object pnlArcInfoMinColour: TPanel
          Left = 251
          Top = 187
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          ParentShowHint = False
          ShowHint = False
          TabOrder = 4
          OnClick = pnlArcInfoMinColourClick
        end
        object pnlArcInfoMaxColour: TPanel
          Left = 251
          Top = 214
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clFuchsia
          ParentShowHint = False
          ShowHint = False
          TabOrder = 5
          OnClick = pnlArcInfoMaxColourClick
        end
        object pnlArcInfoBlankedColour: TPanel
          Left = 251
          Top = 241
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clSilver
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = pnlArcInfoBlankedColourClick
        end
        object cbxPromptTexArcInfo: TCheckBox
          Left = 16
          Top = 93
          Width = 247
          Height = 17
          Caption = 'Prompt for a basemap texture on import'
          Checked = True
          State = cbChecked
          TabOrder = 7
          OnClick = UpdatedModifiedOnly
        end
        object cbArcInfoColourMode: TComboBox
          Left = 4
          Top = 1
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          ItemIndex = 3
          TabOrder = 8
          Text = 'Emission'
          OnChange = UpdateModifiedOnly
          Items.Strings = (
            'Ambient'
            'Ambient & Diffuse'
            'Diffuse'
            'Emission'
            'None')
        end
        object cbArcInfoPolygonMode: TComboBox
          Left = 295
          Top = 1
          Width = 75
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 9
          OnChange = UpdateModifiedOnly
          Items.Strings = (
            'Fill'
            'Wireframe'
            'Points')
        end
        object cbxArcInfoTwoSided: TCheckBox
          Left = 271
          Top = 26
          Width = 99
          Height = 17
          Caption = 'Two Sided Mesh'
          Checked = True
          State = cbChecked
          TabOrder = 10
          OnClick = cbxArcInfoTwoSidedClick
        end
        object geTileArcInfoX: TGEIntegerEdit
          Left = 100
          Top = 113
          Width = 45
          Height = 21
          Text = '0'
          MinValue = 1
          MaxValue = 1
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 11
          OnExit = UpdateModifiedOnly
        end
        object geTileArcInfoY: TGEIntegerEdit
          Left = 235
          Top = 113
          Width = 45
          Height = 21
          Text = '0'
          MinValue = 1
          MaxValue = 1
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 12
          OnExit = UpdateModifiedOnly
        end
        object geArcInfoAlpha: TGEFloatEdit
          Left = 166
          Top = 1
          Width = 45
          Height = 21
          Text = '0'
          MaxValue = 1.000000000000000000
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = True
          Validate = vdSilent
          TabOrder = 13
          OnExit = UpdateModifiedOnly
        end
        object cbxArcInfoSilentImport: TCheckBox
          Left = 16
          Top = 54
          Width = 88
          Height = 17
          Caption = 'Silent Import'
          Checked = True
          State = cbChecked
          TabOrder = 14
          OnClick = UpdatedModifiedOnly
        end
        object cbxArcInfoSilentLoad: TCheckBox
          Left = 118
          Top = 54
          Width = 121
          Height = 17
          Caption = 'Silent MetaFile Load'
          Checked = True
          State = cbChecked
          TabOrder = 15
          OnClick = UpdatedModifiedOnly
        end
      end
      object tsGrids: TTabSheet
        Caption = 'Grids'
        ImageIndex = 11
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbxAutoCenterGrids: TCheckBox
          Left = 8
          Top = 10
          Width = 155
          Height = 17
          Caption = 'Auto Center Grid on Import'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
      end
      object tsData: TTabSheet
        Caption = 'Data'
        ImageIndex = 12
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblPointNullColour: TLabel
          Left = 171
          Top = 34
          Width = 80
          Height = 13
          Caption = 'Null Value Colour'
        end
        object lblPointMinColour: TLabel
          Left = 177
          Top = 86
          Width = 74
          Height = 13
          Caption = 'Minimum Colour'
        end
        object lblPointMaxColour: TLabel
          Left = 173
          Top = 111
          Width = 78
          Height = 13
          Caption = 'Maximum Colour'
        end
        object lblScalarOptions: TLabel
          Left = 5
          Top = 5
          Width = 115
          Height = 13
          Caption = 'Scalar Value Options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblDataCLRFile: TLabel
          Left = 4
          Top = 143
          Width = 38
          Height = 13
          Caption = 'CLR File'
        end
        object lblPointSingleColour: TLabel
          Left = 189
          Top = 59
          Width = 62
          Height = 13
          Caption = 'Single Colour'
        end
        object rgPointOptions: TRadioGroup
          Left = 5
          Top = 21
          Width = 153
          Height = 117
          Caption = 'Options'
          ItemIndex = 0
          Items.Strings = (
            'Single Colour'
            'Simple Range'
            'Rainbow'
            'Inverse Rainbow '
            'Palette .clr File'
            'Inverse Palette .clr File')
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object pnlMaxPointColour: TPanel
          Left = 258
          Top = 106
          Width = 25
          Height = 25
          Hint = 'Click to change point colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clRed
          TabOrder = 1
          OnClick = pnlMaxPointColourClick
        end
        object pnlMinPointColour: TPanel
          Left = 258
          Top = 80
          Width = 25
          Height = 25
          Hint = 'Click to change point colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          TabOrder = 2
          OnClick = pnlMinPointColourClick
        end
        object pnlNullPointColour: TPanel
          Left = 258
          Top = 29
          Width = 25
          Height = 25
          Hint = 'Click to change point colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlack
          TabOrder = 3
          OnClick = pnlNullPointColourClick
        end
        object bLoadDataCLR: TButton
          Left = 2
          Top = 161
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 4
          OnClick = bLoadDataCLRClick
        end
        object ebDataCLRFile: TEdit
          Left = 28
          Top = 161
          Width = 345
          Height = 21
          TabOrder = 5
          OnChange = ebDataCLRFileChange
        end
        object pnlSinglePointColour: TPanel
          Left = 258
          Top = 54
          Width = 25
          Height = 25
          Hint = 'Click to change point colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGreen
          TabOrder = 6
          OnClick = pnlSinglePointColourClick
        end
      end
      object tsInterface: TTabSheet
        Caption = 'Interface'
        ImageIndex = 13
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbxFocusStatusBar: TCheckBox
          Left = 8
          Top = 37
          Width = 157
          Height = 17
          Caption = 'Focus Status Bar'
          TabOrder = 0
        end
        object cbxInvertMouseWheel: TCheckBox
          Left = 8
          Top = 10
          Width = 157
          Height = 17
          Caption = 'Invert Mouse Wheel (zoom)'
          TabOrder = 1
        end
        object cbxCameraStatusBar: TCheckBox
          Left = 8
          Top = 64
          Width = 157
          Height = 17
          Caption = 'Camera Status Bar'
          TabOrder = 2
        end
      end
      object tsHUD: TTabSheet
        Caption = 'HUD'
        ImageIndex = 14
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblColourScaleAlpha: TLabel
          Left = 236
          Top = 155
          Width = 27
          Height = 13
          Alignment = taRightJustify
          Caption = 'Alpha'
        end
        object lblScaleLabelFormat: TLabel
          Left = 176
          Top = 204
          Width = 90
          Height = 13
          Caption = 'Scale Label Format'
        end
        object cbxShowHUDScale: TCheckBox
          Left = 10
          Top = 6
          Width = 185
          Height = 17
          Caption = 'Show scale head up display'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object cbxShowColourScale: TCheckBox
          Left = 10
          Top = 128
          Width = 257
          Height = 17
          Caption = 'Show colour scale head up display'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = UpdatedModifiedOnly
        end
        object cbxColourScaleContinous: TCheckBox
          Left = 30
          Top = 153
          Width = 140
          Height = 17
          Caption = 'Continuous colour grade'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = UpdatedModifiedOnly
        end
        object cbxColourScaleBorder: TCheckBox
          Left = 30
          Top = 202
          Width = 97
          Height = 17
          Caption = 'Draw border'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = UpdatedModifiedOnly
        end
        object cbxColourScaleShowContours: TCheckBox
          Left = 30
          Top = 177
          Width = 149
          Height = 17
          Caption = 'Show contour points'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = UpdatedModifiedOnly
        end
        object cbColourScaleType: TComboBox
          Left = 224
          Top = 175
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 5
          OnChange = UpdatedModifiedOnly
          Items.Strings = (
            'Data Points'
            'Vector Field'
            'Surfer Grid(s)'
            'ArcInfo Grid(s)'
            'Geothermal Grid')
        end
        object ebScaleLabelFormat: TEdit
          Left = 269
          Top = 200
          Width = 100
          Height = 21
          TabOrder = 6
          OnExit = UpdatedModifiedOnly
        end
        object geColourScaleAlpha: TGEFloatEdit
          Left = 269
          Top = 151
          Width = 100
          Height = 21
          Text = '0'
          MaxValue = 1.000000000000000000
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = True
          Validate = vdSilent
          TabOrder = 7
          OnExit = UpdatedModifiedOnly
        end
        object cbxHUDScaleSteps: TCheckBox
          Left = 30
          Top = 231
          Width = 140
          Height = 17
          Caption = 'Fixed Number of Points'
          Checked = True
          State = cbChecked
          TabOrder = 8
          OnClick = UpdatedModifiedOnly
        end
        object geHUDPoints: TGEIntegerEdit
          Left = 68
          Top = 254
          Width = 43
          Height = 21
          Text = '0'
          MinValue = 2
          MaxValue = 0
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 9
        end
      end
      object tsVectorData: TTabSheet
        Caption = 'Vector Data'
        ImageIndex = 15
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblVectorOptions: TLabel
          Left = 5
          Top = 2
          Width = 117
          Height = 13
          Caption = 'Vector Value Options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorMaxColour: TLabel
          Left = 170
          Top = 59
          Width = 78
          Height = 13
          Caption = 'Maximum Colour'
        end
        object lblVectorMinColour: TLabel
          Left = 174
          Top = 35
          Width = 74
          Height = 13
          Caption = 'Minimum Colour'
        end
        object lblVectorSingleColour: TLabel
          Left = 186
          Top = 11
          Width = 62
          Height = 13
          Caption = 'Single Colour'
        end
        object lblVectorDataCLRFile: TLabel
          Left = 5
          Top = 142
          Width = 38
          Height = 13
          Caption = 'CLR File'
        end
        object lblVectorScaleOptions: TLabel
          Left = 5
          Top = 192
          Width = 76
          Height = 13
          Caption = 'Arrow Length'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorMinLength: TLabel
          Left = 5
          Top = 216
          Width = 40
          Height = 13
          Caption = 'Minimum'
        end
        object lblVectorMaxLength: TLabel
          Left = 5
          Top = 238
          Width = 44
          Height = 13
          Caption = 'Maximum'
        end
        object lblVectorMinArrowRadius: TLabel
          Left = 171
          Top = 285
          Width = 40
          Height = 13
          Caption = 'Minimum'
        end
        object lblVectorMaxArrowRadius: TLabel
          Left = 171
          Top = 308
          Width = 44
          Height = 13
          Caption = 'Maximum'
        end
        object lblVectorArrowHeadRadius: TLabel
          Left = 171
          Top = 264
          Width = 103
          Height = 13
          Caption = 'Arrowhead Radius'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorMinRadius: TLabel
          Left = 171
          Top = 216
          Width = 40
          Height = 13
          Caption = 'Minimum'
        end
        object lblVectorMaxRadius: TLabel
          Left = 171
          Top = 238
          Width = 44
          Height = 13
          Caption = 'Maximum'
        end
        object Label4: TLabel
          Left = 171
          Top = 192
          Width = 75
          Height = 13
          Caption = 'Arrow Radius'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorMaxArrowLength: TLabel
          Left = 5
          Top = 308
          Width = 44
          Height = 13
          Caption = 'Maximum'
        end
        object lblVectorMinArrowLength: TLabel
          Left = 5
          Top = 285
          Width = 40
          Height = 13
          Caption = 'Minimum'
        end
        object Label5: TLabel
          Left = 5
          Top = 262
          Width = 104
          Height = 13
          Caption = 'Arrowhead Length'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorSlices: TLabel
          Left = 222
          Top = 98
          Width = 26
          Height = 13
          Caption = 'Slices'
        end
        object lblVectorStacks: TLabel
          Left = 217
          Top = 118
          Width = 31
          Height = 13
          Caption = 'Stacks'
        end
        object rgVectorDataOptions: TRadioGroup
          Left = 5
          Top = 18
          Width = 143
          Height = 117
          Caption = 'Options'
          ItemIndex = 0
          Items.Strings = (
            'Single Colour'
            'Simple Range'
            'Rainbow'
            'Inverse Rainbow '
            'Palette .clr File'
            'Inverse Palette .clr File')
          TabOrder = 0
          OnClick = UpdatedModifiedOnly
        end
        object pnlSingleVectorColour: TPanel
          Left = 257
          Top = 5
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGreen
          TabOrder = 1
          OnClick = pnlSingleVectorColourClick
        end
        object pnlMinVectorColour: TPanel
          Left = 257
          Top = 29
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          TabOrder = 2
          OnClick = pnlMinVectorColourClick
        end
        object pnlMaxVectorColour: TPanel
          Left = 257
          Top = 53
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clRed
          TabOrder = 3
          OnClick = pnlMaxVectorColourClick
        end
        object ebVectorDataCLRFile: TEdit
          Left = 30
          Top = 161
          Width = 343
          Height = 21
          TabOrder = 5
          OnChange = ebVectorDataCLRFileChange
        end
        object bLoadVectorDataCLR: TButton
          Left = 5
          Top = 160
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 4
          OnClick = bLoadVectorDataCLRClick
        end
        object bArrowLengthSync: TButton
          Left = 120
          Top = 220
          Width = 35
          Height = 25
          Caption = 'Sync'
          TabOrder = 6
          OnClick = bArrowLengthSyncClick
        end
        object bArrowHeadRadiusSync: TButton
          Left = 288
          Top = 290
          Width = 35
          Height = 25
          Caption = 'Sync'
          TabOrder = 7
          OnClick = bArrowHeadRadiusSyncClick
        end
        object bArrowRadiusSync: TButton
          Left = 291
          Top = 220
          Width = 35
          Height = 25
          Caption = 'Sync'
          TabOrder = 8
          OnClick = bArrowRadiusSyncClick
        end
        object bArrowHeadLengthSync: TButton
          Left = 120
          Top = 290
          Width = 35
          Height = 25
          Caption = 'Sync'
          TabOrder = 9
          OnClick = bArrowHeadLengthSyncClick
        end
        object geVectorMinLength: TGEFloatEdit
          Left = 51
          Top = 213
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 10
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMaxLength: TGEFloatEdit
          Left = 51
          Top = 234
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 11
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMinRadius: TGEFloatEdit
          Left = 222
          Top = 213
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 12
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMaxRadius: TGEFloatEdit
          Left = 222
          Top = 234
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 13
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMaxArrowLength: TGEFloatEdit
          Left = 52
          Top = 304
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 14
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMinArrowLength: TGEFloatEdit
          Left = 52
          Top = 281
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 15
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMinArrowRadius: TGEFloatEdit
          Left = 221
          Top = 281
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 16
          OnExit = UpdatedModifiedOnly
        end
        object geVectorMaxArrowRadius: TGEFloatEdit
          Left = 221
          Top = 304
          Width = 60
          Height = 21
          Text = '0'
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 17
          OnExit = UpdatedModifiedOnly
        end
        object geVectorSlices: TGEIntegerEdit
          Left = 257
          Top = 92
          Width = 60
          Height = 21
          Text = '0'
          MinValue = 2
          MaxValue = 0
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 18
          OnExit = UpdatedModifiedOnly
        end
        object geVectorStacks: TGEIntegerEdit
          Left = 257
          Top = 114
          Width = 60
          Height = 21
          Text = '0'
          MinValue = 1
          MaxValue = 0
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = False
          Validate = vdSilent
          TabOrder = 19
          OnExit = UpdatedModifiedOnly
        end
      end
      object tsGeothermal: TTabSheet
        Caption = 'Geothermal'
        ImageIndex = 16
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblGeothermalCLR: TLabel
          Left = 2
          Top = 127
          Width = 38
          Height = 13
          Caption = 'CLR File'
        end
        object lblGeothermalMaxColour: TLabel
          Left = 185
          Top = 70
          Width = 54
          Height = 13
          Caption = 'Max Colour'
        end
        object lblGeothermalMinColour: TLabel
          Left = 189
          Top = 45
          Width = 50
          Height = 13
          Caption = 'Min Colour'
        end
        object lblGeothermalSingleColour: TLabel
          Left = 177
          Top = 18
          Width = 62
          Height = 13
          Caption = 'Single Colour'
        end
        object rgGeothermalOptions: TRadioGroup
          Left = 2
          Top = 3
          Width = 153
          Height = 117
          Caption = 'Options'
          ItemIndex = 0
          Items.Strings = (
            'Single Colour'
            'Simple Range'
            'Rainbow'
            'Inverse Rainbow '
            'Palette .clr File'
            'Inverse Palette .clr File')
          TabOrder = 0
          OnClick = UpdateGeothermalModified
        end
        object bLoadGeothermalCLR: TButton
          Left = 2
          Top = 145
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = bLoadGeothermalCLRClick
        end
        object ebGeothermalCLRFile: TEdit
          Left = 25
          Top = 145
          Width = 345
          Height = 21
          TabOrder = 2
          OnChange = ebGeothermalCLRFileChange
        end
        object pnlGeothermalMaxColour: TPanel
          Left = 248
          Top = 63
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clFuchsia
          ParentShowHint = False
          ShowHint = False
          TabOrder = 3
          OnClick = pnlGeothermalMaxColourClick
        end
        object pnlGeothermalMinColour: TPanel
          Left = 248
          Top = 38
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clBlue
          ParentShowHint = False
          ShowHint = False
          TabOrder = 4
          OnClick = pnlGeothermalMinColourClick
        end
        object pnlGeothermalColour: TPanel
          Left = 248
          Top = 13
          Width = 25
          Height = 25
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGreen
          ParentShowHint = False
          ShowHint = False
          TabOrder = 5
          OnClick = pnlGeothermalColourClick
        end
      end
      object tsScalarBox: TTabSheet
        Caption = 'tsScalarBox'
        ImageIndex = 17
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblScalarBoundingColour: TLabel
          Left = 21
          Top = 34
          Width = 53
          Height = 13
          Caption = 'Line Colour'
        end
        object lblScalarBB: TLabel
          Left = 4
          Top = 8
          Width = 76
          Height = 13
          Caption = 'Bounding Box'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblScalarBoxLinePattern: TLabel
          Left = 125
          Top = 34
          Width = 58
          Height = 13
          Caption = 'Line Pattern'
        end
        object pnlScalarBoundingColour: TPanel
          Left = 81
          Top = 28
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGray
          TabOrder = 0
          OnClick = pnlScalarBoundingColourClick
        end
        object cbxScalarAA: TCheckBox
          Left = 21
          Top = 65
          Width = 108
          Height = 17
          Caption = 'Anti-Aliasing'
          TabOrder = 1
          OnClick = UpdatedModifiedOnly
        end
        object cbxScalarSmooth: TCheckBox
          Left = 182
          Top = 65
          Width = 108
          Height = 17
          Caption = 'Smoothing'
          TabOrder = 2
          OnClick = UpdatedModifiedOnly
        end
        object geScalarLinePattern: TGEIntegerEdit
          Left = 190
          Top = 30
          Width = 77
          Height = 21
          Text = '0'
          MinValue = 0
          MaxValue = 65535
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = True
          Validate = vdSilent
          TabOrder = 3
          OnExit = UpdatedModifiedOnly
        end
      end
      object tsVectorBox: TTabSheet
        Caption = 'tsVectorBox'
        ImageIndex = 18
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblVectorBoxLineColour: TLabel
          Left = 21
          Top = 34
          Width = 53
          Height = 13
          Caption = 'Line Colour'
        end
        object lblVectorBB: TLabel
          Left = 4
          Top = 8
          Width = 76
          Height = 13
          Caption = 'Bounding Box'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVectorBoxLinePattern: TLabel
          Left = 125
          Top = 34
          Width = 58
          Height = 13
          Caption = 'Line Pattern'
        end
        object pnlVectorBoundingColour: TPanel
          Left = 81
          Top = 28
          Width = 25
          Height = 25
          Hint = 'Click to change colour'
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Color = clGray
          TabOrder = 0
          OnClick = pnlVectorBoundingColourClick
        end
        object cbxVectorAA: TCheckBox
          Left = 21
          Top = 65
          Width = 108
          Height = 17
          Caption = 'Anti-Aliasing'
          TabOrder = 1
          OnClick = UpdatedModifiedOnly
        end
        object cbxVectorSmooth: TCheckBox
          Left = 182
          Top = 65
          Width = 108
          Height = 17
          Caption = 'Smoothing'
          TabOrder = 2
          OnClick = UpdatedModifiedOnly
        end
        object geVectorLinePattern: TGEIntegerEdit
          Left = 190
          Top = 30
          Width = 77
          Height = 21
          Text = '0'
          MinValue = 0
          MaxValue = 65535
          ReturnStop = False
          UseMinBound = True
          UseMaxBound = True
          Validate = vdSilent
          TabOrder = 3
          OnExit = UpdatedModifiedOnly
        end
      end
    end
    object sbModified: TStatusBar
      Left = 1
      Top = 353
      Width = 381
      Height = 19
      Panels = <
        item
          Alignment = taCenter
          Text = 'Modified'
          Width = 50
        end
        item
          Alignment = taCenter
          Text = 'Surfer Modified'
          Width = 100
        end
        item
          Alignment = taCenter
          Text = 'ArcInfo Modified'
          Width = 100
        end
        item
          Alignment = taCenter
          Text = 'Geothermal Modified'
          Width = 50
        end>
    end
  end
  object ColourDialog: TColorDialog
    Color = clWhite
    Left = 128
    Top = 6
  end
  object OpenDialogCLR: TOpenDialog
    DefaultExt = 'clr'
    Filter = 'Colour Spectrum File|*.clr|All Files|*.*'
    Title = 'Open CLR Colour Spectrum File'
    Left = 128
    Top = 34
  end
  object ImageList: TImageList
    Left = 127
    Top = 63
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      80008080800000000000000000000000000000000000BF000000BF000000BF00
      0000BF000000BF000000BF000000BF000000BF000000BF000000BF000000BF00
      0000BF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      80008080800000000000000000000000000000000000BF000000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000BF0000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800000000000000000000000000000000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BF0000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800000000000000000000000000000000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BF0000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800000000000000000000000000000000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800000000000000000000000000000000000BF000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00808080000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008080800000000000000000000000000000000000BF000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000080808000C0C0C000C0C0
      C0000000000000000000C0C0C000C0C0C000C0C0C00000000000000000000000
      000000000000000000000000000000000000000000008080800000FF000000FF
      0000000000000000000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      80008080800000000000000000000000000000000000BF000000BF000000BF00
      0000BF000000BF000000BF000000BF000000BF000000BF000000BF0000000000
      000000FFFF0080808000000000000000000080808000C0C0C000000000000000
      0000000000000000000000000000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000008080800000FF0000000000000000
      000000000000000000000000000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000080808000FFFFFF008080
      800080808000FFFFFF008080800080808000FFFFFF008080800080808000FFFF
      FF008080800000000000000000000000000000000000BF000000FFFFFF00BF00
      0000BF000000FFFFFF00BF000000BF000000FFFFFF00BF000000BF0000000000
      000000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      80008080800000000000000000000000000000000000BF000000BF000000BF00
      0000BF000000BF000000BF000000BF000000BF000000BF000000BF000000BF00
      00000000000000FFFF0080808000000000000000000000000000000000000000
      000000000000000000000000000000000000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0080808000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BF00BF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008080800000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BF00BF00BF00BF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      800000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFF80078007FFFFFFFF
      80078007F9FFF9FFBFF7BFD7F0FFF0FFA497A487F0FFF0FFBFF7BFC7E07FE07F
      A497A483C07FC07FBFF7BFCB843F843F800780011E3F1E3F80078005FE1FFE1F
      80078000FF1FFF1FFFFFFFF0FF8FFF8FFFFFFFF8FFC7FFC7FFFFFFF8FFE3FFE3
      FFFFFFFCFFF8FFF8FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
