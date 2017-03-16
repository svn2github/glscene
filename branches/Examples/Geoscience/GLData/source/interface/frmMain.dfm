object formMain: TformMain
  Left = 107
  Top = 131
  Caption = 'glData'
  ClientHeight = 556
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
    00000000CCCCCCC0000000000000000000000CCCCCCCCCCCCC00000000000000
    000CCCCCCCCCCCCCCCCC00000000000000CCCCCCCCCCCCCCCCCCC00000000000
    0CCCCCCCCCCCCCCCCCCCCC0000000000CCCCCCCCCCCCCCCCCCCCCCC00000000C
    CCCCCCCCCFFFFFCCCCCCCCCC000000CCCCCCCCCCCFFFFFCCCCCCCCCCC00000CC
    CCCCCCCCCFFFFFCCCCCCCCCCC0000CCCCCCCCCCCCFFFFFCCCCCCCCCCCC000CCC
    CCCCCCCCCFFFFFCCCCCCCCCCCC000CCCCCCCCCCCCFFFFFCCCCCCCCCCCC00CCCC
    CCCCCCCCCFFFFFCCCCCCCCCCCCC0CCCCCCFFFFFFFFFFFFFFFFFFFCCCCCC0CCCC
    CCFFFFFFFFFFFFFFFFFFFCCCCCC0CCCCCCFFFFFFFFFFFFFFFFFFFCCCCCC0CCCC
    CCFFFFFFFFFFFFFFFFFFFCCCCCC0CCCCCCFFFFFFFFFFFFFFFFFFFCCCCCC0CCCC
    CCCCCCCCCFFFFFCCCCCCCCCCCCC00CCCCCCCCCCCCFFFFFCCCCCCCCCCCC000CCC
    CCCCCCCCCFFFFFCCCCCCCCCCCC000CCCCCCCCCCCCFFFFFCCCCCCCCCCCC0000CC
    CCCCCCCCCFFFFFCCCCCCCCCCC00000CCCCCCCCCCCFFFFFCCCCCCCCCCC000000C
    CCCCCCCCCFFFFFCCCCCCCCCC00000000CCCCCCCCCCCCCCCCCCCCCCC000000000
    0CCCCCCCCCCCCCCCCCCCCC000000000000CCCCCCCCCCCCCCCCCCC00000000000
    000CCCCCCCCCCCCCCCCC00000000000000000CCCCCCCCCCCCC00000000000000
    00000000CCCCCCC0000000000000FFFFFFFFFFF01FFFFF8003FFFE0000FFFC00
    007FF800003FF000001FE000000FC0000007C000000780000003800000038000
    0003000000010000000100000001000000010000000100000001000000018000
    00038000000380000003C0000007C0000007E000000FF000001FF800003FFC00
    007FFE0000FFFF8003FFFFF01FFF280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000CCCCCF00000000C
    CCCCCCCCF00000CCCCCCCCCCCF000CCCCCCFFCCCCCF00CCCCCCFFCCCCCC0CCCC
    CCCFFCCCCCCFCCCCCCCFFCCCCCCCCCCFFFFFFFFFFCCCCCCFFFFFFFFFFCCCCCCC
    CCCFFCCCCCCCCCCCCCCFFCCCCCCC0CCCCCCFFCCCCCC00CCCCCCFFCCCCCC000CC
    CCCCCCCCCC00000CCCCCCCCCC00000000CCCCCC00000F83F0000E00F0000C007
    0000800300008001000000010000000000000000000000000000000000000000
    00008001000080010000C0030000E0070000F81F0000}
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splittermain: TSplitter
    Left = 404
    Top = 23
    Height = 533
    Color = clSkyBlue
    ParentColor = False
    ExplicitTop = 24
    ExplicitHeight = 546
  end
  object pnlData: TPanel
    Left = 0
    Top = 23
    Width = 404
    Height = 533
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoa'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object pcMain: TPageControl
      Left = 1
      Top = 1
      Width = 402
      Height = 531
      ActivePage = tsGeothermal
      Align = alClient
      DockSite = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MultiLine = True
      ParentFont = False
      TabOrder = 0
      OnChange = acImportCSVExecute
      object tsOriginalData: TTabSheet
        Caption = 'Raw Data'
        object ActionToolBar_Original: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 26
          ActionManager = ActionManager1
          Caption = 'ActionToolBar_Original'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
        end
        object dbgData1: TDBGrid
          Left = 0
          Top = 26
          Width = 394
          Height = 458
          Align = alClient
          BorderStyle = bsNone
          DataSource = dsData1
          Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          Columns = <
            item
              Expanded = False
              FieldName = 'u'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'v'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'w'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Value'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Uu'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Uv'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Uw'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end>
        end
        object sbOriginal: TStatusBar
          Left = 0
          Top = 484
          Width = 394
          Height = 19
          Panels = <
            item
              Alignment = taCenter
              Width = 50
            end
            item
              Width = 50
            end>
        end
      end
      object tsProcessed: TTabSheet
        Caption = 'Processed'
        ImageIndex = 1
        object dbgData2: TDBGrid
          Left = 0
          Top = 26
          Width = 394
          Height = 458
          Align = alClient
          BorderStyle = bsNone
          DataSource = dsData2
          Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          OnDblClick = dbgData2DblClick
          Columns = <
            item
              Expanded = False
              FieldName = 'x'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'y'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'z'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Value'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Magnitude'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Ux'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Uy'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Uz'
              Title.Alignment = taCenter
              Width = 90
              Visible = True
            end>
        end
        object ActionToolBar_Processed: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 26
          ActionManager = ActionManager1
          Caption = 'ActionToolBar_Processed'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
        end
        object sbProcessed: TStatusBar
          Left = 0
          Top = 484
          Width = 394
          Height = 19
          Panels = <
            item
              Alignment = taCenter
              Width = 50
            end
            item
              Width = 50
            end>
        end
      end
      object tsStructureGrids: TTabSheet
        Caption = 'Grids (experimental)'
        ImageIndex = 3
        TabVisible = False
        object memGridDetails: TMemo
          Left = 0
          Top = 23
          Width = 394
          Height = 250
          Align = alTop
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object ActionTool_Grids: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 23
          ActionManager = ActionManager1
          Caption = 'ActionTool_Grids'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Spacing = 0
        end
        object cbxGridBoundingBox: TCheckBox
          Left = 4
          Top = 280
          Width = 97
          Height = 17
          Caption = 'Show Outline'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = cbxGridBoundingBoxClick
        end
        object cbxCutXY: TCheckBox
          Left = 4
          Top = 312
          Width = 97
          Height = 17
          Caption = 'XY Cut Plane'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = cbxCutXYClick
        end
        object cbxCutXZ: TCheckBox
          Left = 4
          Top = 364
          Width = 97
          Height = 17
          Caption = 'XZ Cut Plane'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = cbxCutXZClick
        end
        object cbCutYZ: TCheckBox
          Left = 4
          Top = 416
          Width = 97
          Height = 17
          Caption = 'YZ Cut Plane'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = cbxCutXZClick
        end
      end
      object tsSURFER: TTabSheet
        Caption = 'Surfer'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object splitterSurfer: TSplitter
          Left = 0
          Top = 150
          Width = 394
          Height = 3
          Cursor = crVSplit
          Align = alTop
          Beveled = True
          Color = clSkyBlue
          ParentColor = False
        end
        object clbSurferGrids: TCheckListBox
          Left = 0
          Top = 26
          Width = 394
          Height = 124
          OnClickCheck = clbSurferGridsClickCheck
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
          OnClick = clbSurferGridsClick
        end
        object ActionToolBarSurfer: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 26
          ActionManager = ActionManager1
          Caption = 'ActionToolBarSurfer'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Spacing = 0
        end
        object pcSurferDetails: TPageControl
          Left = 0
          Top = 153
          Width = 394
          Height = 350
          ActivePage = tsSurferDisplay
          Align = alClient
          TabOrder = 2
          TabPosition = tpBottom
          OnChange = pcSurferDetailsChange
          object tsNoSurfer: TTabSheet
            Caption = 'No Surfer'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object pnlNoSurfer: TPanel
              Left = 0
              Top = 0
              Width = 386
              Height = 324
              Align = alClient
              BevelOuter = bvLowered
              Caption = 'No Surfer Grid Selected.'
              TabOrder = 0
            end
          end
          object tsSurferGridInfo: TTabSheet
            Caption = 'Grid Info'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object dbSurferGridInfo: TDBGrid
              Left = 0
              Top = 0
              Width = 386
              Height = 324
              Align = alClient
              DataSource = dsSurferGridInfo
              ReadOnly = True
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -11
              TitleFont.Name = 'Tahoma'
              TitleFont.Style = []
              Columns = <
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Property'
                  Title.Alignment = taCenter
                  Width = 120
                  Visible = True
                end
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Integer'
                  Title.Alignment = taCenter
                  Visible = True
                end
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Float'
                  Title.Alignment = taCenter
                  Width = 160
                  Visible = True
                end>
            end
          end
          object tsSurferDisplay: TTabSheet
            Caption = 'Display'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lblSurferPolygonMode: TLabel
              Left = 132
              Top = 8
              Width = 67
              Height = 13
              Caption = 'Polygon Mode'
            end
            object lblSurferAlpha: TLabel
              Left = 45
              Top = 31
              Width = 27
              Height = 13
              Caption = 'Alpha'
            end
            object lblTileSurferX: TLabel
              Left = 1
              Top = 121
              Width = 77
              Height = 13
              Caption = 'Tile Texture in X'
            end
            object lblTileSurferY: TLabel
              Left = 192
              Top = 121
              Width = 77
              Height = 13
              Caption = 'Tile Texture in Y'
            end
            object lblSurferGridName: TLabel
              Left = 4
              Top = 218
              Width = 49
              Height = 13
              Caption = 'Grid Name'
            end
            object cbSurferPolygonMode: TComboBox
              Left = 204
              Top = 4
              Width = 75
              Height = 21
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbSurferPolygonModeChange
              Items.Strings = (
                'Fill'
                'Wireframe'
                'Points')
            end
            object cbSurferColourMode: TComboBox
              Left = 2
              Top = 4
              Width = 121
              Height = 21
              Style = csDropDownList
              ItemIndex = 3
              TabOrder = 1
              Text = 'Emission'
              OnChange = cbSurferColourModeChange
              Items.Strings = (
                'Ambient'
                'Ambient & Diffuse'
                'Diffuse'
                'Emission'
                'None')
            end
            object cbxSurferTwoSided: TCheckBox
              Left = 283
              Top = 6
              Width = 99
              Height = 17
              Caption = 'Two Sided Mesh'
              Checked = True
              State = cbChecked
              TabOrder = 2
              OnClick = cbxSurferTwoSidedClick
            end
            object ebSurferBaseMap: TEdit
              Left = 23
              Top = 144
              Width = 345
              Height = 21
              TabOrder = 3
              OnChange = ebSurferBaseMapChange
            end
            object bLoadSurferBaseMap: TButton
              Left = 1
              Top = 144
              Width = 21
              Height = 21
              Caption = '...'
              TabOrder = 4
              OnClick = bLoadSurferBaseMapClick
            end
            object cbxSurferBaseMap: TCheckBox
              Left = 30
              Top = 168
              Width = 117
              Height = 17
              Caption = 'Enable Base Map'
              TabOrder = 5
              OnClick = cbxSurferBaseMapClick
            end
            object ebSurferGridName: TEdit
              Left = 3
              Top = 235
              Width = 365
              Height = 21
              TabOrder = 6
              OnExit = ebSurferGridNameExit
            end
          end
        end
      end
      object tsArcInfo: TTabSheet
        Caption = 'Arc/Info'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object splitterArcInfo: TSplitter
          Left = 0
          Top = 150
          Width = 394
          Height = 3
          Cursor = crVSplit
          Align = alTop
          Beveled = True
          Color = clSkyBlue
          ParentColor = False
        end
        object ActionToolBarArcInfo: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 26
          ActionManager = ActionManager1
          Caption = 'ActionToolBarSurfer'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Spacing = 0
        end
        object clbArcInfoGrids: TCheckListBox
          Left = 0
          Top = 26
          Width = 394
          Height = 124
          OnClickCheck = clbArcInfoGridsClickCheck
          Align = alTop
          ItemHeight = 13
          TabOrder = 1
          OnClick = clbArcInfoGridsClick
        end
        object pcArcInfoDetails: TPageControl
          Left = 0
          Top = 153
          Width = 394
          Height = 350
          ActivePage = tsNoArcInfo
          Align = alClient
          TabOrder = 2
          TabPosition = tpBottom
          object tsNoArcInfo: TTabSheet
            Caption = 'No Arc/Info'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object pnlNoArcInfo: TPanel
              Left = 0
              Top = 0
              Width = 386
              Height = 324
              Align = alClient
              BevelOuter = bvLowered
              Caption = 'No Arc/Info Grid Selected.'
              TabOrder = 0
            end
          end
          object tsArcInfoGridInfo: TTabSheet
            Caption = 'Grid Info'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object dbArcInfoGridInfo: TDBGrid
              Left = 0
              Top = 0
              Width = 386
              Height = 324
              Align = alClient
              DataSource = dsArcInfoGridInfo
              ReadOnly = True
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -11
              TitleFont.Name = 'Tahoma'
              TitleFont.Style = []
              Columns = <
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Property'
                  Title.Alignment = taCenter
                  Width = 120
                  Visible = True
                end
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Integer'
                  Title.Alignment = taCenter
                  Visible = True
                end
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'Float'
                  Title.Alignment = taCenter
                  Width = 160
                  Visible = True
                end>
            end
          end
          object tsArcInfoDisplay: TTabSheet
            Caption = 'Display'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lblArcInfoAlpha: TLabel
              Left = 45
              Top = 30
              Width = 27
              Height = 13
              Caption = 'Alpha'
            end
            object lblArcInfoPolygonMode: TLabel
              Left = 132
              Top = 7
              Width = 67
              Height = 13
              Caption = 'Polygon Mode'
            end
            object lblArcInfoTileX: TLabel
              Left = 6
              Top = 121
              Width = 77
              Height = 13
              Caption = 'Tile Texture in X'
            end
            object lblArcInfoTileY: TLabel
              Left = 192
              Top = 121
              Width = 77
              Height = 13
              Caption = 'Tile Texture in Y'
            end
            object lblArcInfoGridName: TLabel
              Left = 4
              Top = 218
              Width = 49
              Height = 13
              Caption = 'Grid Name'
            end
            object cbArcInfoColourMode: TComboBox
              Left = 2
              Top = 3
              Width = 121
              Height = 21
              Style = csDropDownList
              ItemIndex = 3
              TabOrder = 0
              Text = 'Emission'
              OnChange = cbArcInfoColourModeChange
              Items.Strings = (
                'Ambient'
                'Ambient & Diffuse'
                'Diffuse'
                'Emission'
                'None')
            end
            object cbArcInfoPolygonMode: TComboBox
              Left = 204
              Top = 3
              Width = 75
              Height = 21
              Style = csDropDownList
              TabOrder = 1
              OnChange = cbArcInfoPolygonModeChange
              Items.Strings = (
                'Fill'
                'Wireframe'
                'Points')
            end
            object cbxArcInfoTwoSided: TCheckBox
              Left = 283
              Top = 6
              Width = 99
              Height = 17
              Caption = 'Two Sided Mesh'
              Checked = True
              State = cbChecked
              TabOrder = 2
              OnClick = cbxArcInfoTwoSidedClick
            end
            object bLoadArcInfoBaseMap: TButton
              Left = 1
              Top = 144
              Width = 21
              Height = 21
              Caption = '...'
              TabOrder = 3
              OnClick = bLoadArcInfoBaseMapClick
            end
            object ebArcInfoBaseMap: TEdit
              Left = 24
              Top = 144
              Width = 345
              Height = 21
              TabOrder = 4
              OnChange = ebArcInfoBaseMapChange
            end
            object cbxArcInfoBaseMap: TCheckBox
              Left = 30
              Top = 168
              Width = 117
              Height = 17
              Caption = 'Enable Base Map'
              TabOrder = 5
              OnClick = cbxArcInfoBaseMapClick
            end
            object ebArcInfoGridName: TEdit
              Left = 3
              Top = 235
              Width = 365
              Height = 21
              TabOrder = 6
              OnExit = ebArcInfoGridNameExit
            end
          end
        end
      end
      object tsGeothermal: TTabSheet
        Caption = 'Geothermal'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object ActionToolBarGeothermal: TActionToolBar
          Left = 0
          Top = 0
          Width = 394
          Height = 26
          ActionManager = ActionManager1
          Caption = 'ActionToolBarSurfer'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Spacing = 0
        end
        object pnlGeoTop: TPanel
          Left = 0
          Top = 26
          Width = 394
          Height = 48
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 1
          object cbDisplay: TComboBox
            Left = 5
            Top = 3
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = 'Rock Types'
            OnChange = cbDisplayChange
            Items.Strings = (
              'Rock Types'
              'Porosity'
              'Permeability-X'
              'Permeability-Y'
              'Permeability-Z'
              'Pressure'
              'Temperature'
              'Saturation')
          end
          object cbxGeoAutoScale: TCheckBox
            Left = 5
            Top = 27
            Width = 76
            Height = 17
            Caption = 'Auto Scale'
            TabOrder = 1
            OnClick = cbxGeoAutoScaleClick
          end
          object ebgeoMinValue: TEdit
            Left = 93
            Top = 25
            Width = 110
            Height = 21
            TabOrder = 2
            Text = '0.0'
            OnExit = ebgeoMinValueExit
          end
          object ebgeoMaxValue: TEdit
            Left = 204
            Top = 24
            Width = 110
            Height = 21
            TabOrder = 3
            Text = '1.0'
            OnExit = ebgeoMaxValueExit
          end
          object bGeoSyncLimits: TButton
            Left = 319
            Top = 21
            Width = 40
            Height = 25
            Caption = 'Sync'
            TabOrder = 4
            OnClick = bGeoSyncLimitsClick
          end
        end
        object dbgGeothermalLayers: TDBGrid
          Left = 0
          Top = 74
          Width = 394
          Height = 222
          Align = alClient
          DataSource = dsGeothermalLayers
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -9
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          OnDrawColumnCell = dbgGeothermalLayersDrawColumnCell
          Columns = <
            item
              Alignment = taCenter
              Color = clSkyBlue
              Expanded = False
              FieldName = 'Layer'
              ReadOnly = True
              Title.Alignment = taCenter
              Title.Color = clSkyBlue
              Visible = True
            end
            item
              Alignment = taCenter
              Color = clSkyBlue
              Expanded = False
              FieldName = 'Elevation'
              ReadOnly = True
              Title.Alignment = taCenter
              Title.Color = clSkyBlue
              Visible = True
            end
            item
              Alignment = taCenter
              Color = clSkyBlue
              Expanded = False
              FieldName = 'Thickness'
              ReadOnly = True
              Title.Alignment = taCenter
              Title.Color = clSkyBlue
              Visible = True
            end
            item
              Alignment = taCenter
              Expanded = False
              FieldName = 'Show Block'
              PickList.Strings = (
                'True'
                'False')
              Title.Alignment = taCenter
              Width = 55
              Visible = True
            end
            item
              Alignment = taCenter
              Expanded = False
              FieldName = 'Block Top'
              PickList.Strings = (
                'True'
                'False')
              Title.Alignment = taCenter
              Title.Caption = 'Top'
              Width = 40
              Visible = True
            end
            item
              Alignment = taCenter
              Expanded = False
              FieldName = 'Block Bottom'
              PickList.Strings = (
                'True'
                'False')
              Title.Alignment = taCenter
              Title.Caption = 'Bottom'
              Width = 40
              Visible = True
            end
            item
              Alignment = taCenter
              Expanded = False
              FieldName = 'Block Outside'
              PickList.Strings = (
                'True'
                'False')
              Title.Alignment = taCenter
              Title.Caption = 'Outside'
              Width = 40
              Visible = True
            end
            item
              Alignment = taCenter
              Expanded = False
              FieldName = 'Alpha'
              Title.Alignment = taCenter
              Width = 40
              Visible = True
            end>
        end
        object PageControl1: TPageControl
          Left = 0
          Top = 296
          Width = 394
          Height = 207
          ActivePage = tsRowsCols
          Align = alBottom
          TabOrder = 3
          object tsRowsCols: TTabSheet
            Caption = 'Rows/Columns'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object clbColumns: TCheckListBox
              Left = 187
              Top = 0
              Width = 199
              Height = 179
              OnClickCheck = clbColumnsClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
            object clbRows: TCheckListBox
              Left = 0
              Top = 0
              Width = 187
              Height = 179
              OnClickCheck = clbRowsClickCheck
              Align = alLeft
              ItemHeight = 13
              TabOrder = 1
            end
          end
          object tsIsoSurfaces: TTabSheet
            Caption = 'Isosurfaces'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object tbIso: TTrackBar
              Left = 4
              Top = 51
              Width = 380
              Height = 45
              Max = 500
              Position = 200
              TabOrder = 0
              OnChange = tbIsoChange
            end
          end
          object tsBlockSettings: TTabSheet
            Caption = 'Block Options'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object cbxInActiveBlocks: TCheckBox
              Left = 16
              Top = 15
              Width = 139
              Height = 17
              Caption = 'Show InActive Blocks'
              TabOrder = 0
              OnClick = cbxInActiveBlocksClick
            end
          end
        end
      end
    end
  end
  object pnlGLScene: TPanel
    Left = 407
    Top = 23
    Width = 385
    Height = 533
    Align = alClient
    TabOrder = 1
    object Image1: TImage
      Left = 278
      Top = 472
      Width = 105
      Height = 105
    end
    object GLSceneViewer: TGLSceneViewer
      Left = 1
      Top = 27
      Width = 383
      Height = 475
      Camera = GLCamera
      Buffer.BackgroundColor = clWhite
      FieldOfView = 150.733886718750000000
      OnMouseEnter = GLSceneViewerMouseEnter
      Align = alClient
      OnMouseDown = GLSceneViewerMouseDown
      OnMouseMove = GLSceneViewerMouseMove
      OnMouseUp = GLSceneViewerMouseUp
      TabOrder = 0
    end
    object sbCamera: TStatusBar
      Left = 1
      Top = 517
      Width = 383
      Height = 15
      Hint = 'Double click to hide'
      Color = clSkyBlue
      Panels = <
        item
          Alignment = taRightJustify
          Bevel = pbNone
          Text = 'Camera XYZ'
          Width = 65
        end
        item
          Alignment = taRightJustify
          Text = 'x'
          Width = 75
        end
        item
          Alignment = taRightJustify
          Text = 'z'
          Width = 75
        end
        item
          Alignment = taRightJustify
          Text = 'y'
          Width = 75
        end
        item
          Bevel = pbNone
          Width = 50
        end>
      ParentShowHint = False
      ShowHint = False
      SimpleText = 'Camera: '
      SizeGrip = False
      Visible = False
    end
    object sbFocus: TStatusBar
      Left = 1
      Top = 502
      Width = 383
      Height = 15
      Hint = 'Double click to hide'
      Color = clSilver
      Panels = <
        item
          Alignment = taRightJustify
          Bevel = pbNone
          Text = 'Focus XYZ'
          Width = 65
        end
        item
          Alignment = taRightJustify
          Text = 'x'
          Width = 75
        end
        item
          Alignment = taRightJustify
          Text = 'z'
          Width = 75
        end
        item
          Alignment = taRightJustify
          Text = 'y'
          Width = 75
        end
        item
          Bevel = pbNone
          Width = 50
        end>
      ParentShowHint = False
      ShowHint = False
      SimpleText = 'Camera: '
      SizeGrip = False
      Visible = False
    end
    object ActionToolBar_Modes: TActionToolBar
      Left = 1
      Top = 1
      Width = 383
      Height = 26
      ActionManager = ActionManager1
      Caption = 'ActionToolBar_Modes'
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Spacing = 0
    end
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 792
    Height = 23
    UseSystemFont = False
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Spacing = 0
  end
  object GLScene: TGLScene
    Left = 437
    Top = 83
    object LightCube: TGLDummyCube
      Direction.Coordinates = {0000000000000000000080BF00000000}
      CubeSize = 1.000000000000000000
      object GLLightSource: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsParallel
        SpotCutOff = 180.000000000000000000
      end
      object GLLightSourceReverse: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsParallel
        SpotCutOff = 180.000000000000000000
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object GLDummyCube: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {00000000000000000000000000000000}
      object GLCamera: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube
        Position.Coordinates = {000020C1000020410000A0400000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object GLDummyCubeObjects: TGLDummyCube
      ObjectsSorting = osRenderBlendedLast
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {00000000000000000000000000000000}
      object GLPoints: TGLPoints
        Visible = False
        NoZWrite = False
        Static = False
        Size = 5.000000000000000000
        Style = psRound
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
        Visible = False
        Radius = 0.050000000745058060
        Slices = 6
        Stacks = 6
      end
      object glBoundingBox: TGLXYZGrid
        Visible = False
        LineColor.Color = {BEC0403FBEC0403FBEC0403F0000803F}
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpX, gpY, gpZ]
      end
      object glBoundingBoxVector: TGLXYZGrid
        Visible = False
        LineColor.Color = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpX, gpY, gpZ]
      end
      object GLLines: TGLLines
        LineColor.Color = {00000000000000000000803F0000803F}
        Nodes = <>
        NodesAspect = lnaInvisible
        NodeSize = 0.050000000745058060
        Options = []
      end
      object GLPipe: TGLPipe
        Visible = False
        Nodes = <>
        SplineMode = lsmBezierSpline
        Radius = 0.009999999776482582
        NodesColorMode = pncmDiffuse
      end
      object GLDAxes: TGLDummyCube
        Up.Coordinates = {00000000000080BF0000008000000000}
        CubeSize = 1.000000000000000000
      end
      object GLDVectorData: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
      object GLDCubeGrid: TGLDummyCube
        CubeSize = 1.000000000000000000
        EdgeColor.Color = {00000000000000000000000000000000}
      end
      object GLDGeoSim: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
      object GLDSurferGrids: TGLDummyCube
        ObjectsSorting = osRenderBlendedLast
        CubeSize = 1.000000000000000000
      end
      object GLDArcInfoGrids: TGLDummyCube
        ObjectsSorting = osRenderBlendedLast
        CubeSize = 1.000000000000000000
      end
      object glsLocator: TGLSphere
        Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FC9761E3F}
        Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000003F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000003333333F}
        Material.BlendingMode = bmTransparency
        Visible = False
        Radius = 0.500000000000000000
      end
      object GLHUDColourScale: TGLHUDSprite
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.Image.Picture.Data = {
          0A544A504547496D61676582170000FFD8FFE000104A46494600010201004800
          480000FFED0C0C50686F746F73686F7020332E30003842494D03ED0A5265736F
          6C7574696F6E0000000010004800000002000200480000000200023842494D04
          0D18465820476C6F62616C204C69676874696E6720416E676C65000000000400
          0000783842494D041912465820476C6F62616C20416C74697475646500000000
          040000001E3842494D03F30B5072696E7420466C616773000000090000000000
          00000001003842494D040A0E436F7079726967687420466C6167000000000100
          003842494D2710144A6170616E657365205072696E7420466C61677300000000
          0A000100000000000000023842494D03F517436F6C6F722048616C66746F6E65
          2053657474696E677300000048002F66660001006C6666000600000000000100
          2F6666000100A1999A0006000000000001003200000001005A00000006000000
          000001003500000001002D000000060000000000013842494D03F817436F6C6F
          72205472616E736665722053657474696E6773000000700000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF03E800000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFF03E800000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF03E800000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF03E800003842494D040806477569646573000000001000000001000002
          4000000240000000003842494D041E0D55524C206F7665727269646573000000
          04000000003842494D041A06536C696365730000000075000000060000000000
          0000000000009D0000004F0000000A0055006E007400690074006C0065006400
          2D00310000000100000000000000000000000000000000000000010000000000
          0000000000004F0000009D000000000000000000000000000000000000000000
          000000000000000000000000003842494D04111149434320556E746167676564
          20466C61670000000101003842494D0414174C617965722049442047656E6572
          61746F72204261736500000004000000033842494D040C154E65772057696E64
          6F7773205468756D626E61696C00000864000000010000003800000070000000
          A8000049800000084800180001FFD8FFE000104A464946000102010048004800
          00FFEE000E41646F626500648000000001FFDB0084000C08080809080C09090C
          110B0A0B11150F0C0C0F1518131315131318110C0C0C0C0C0C110C0C0C0C0C0C
          0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C010D0B0B0D0E0D100E0E
          10140E0E0E14140E0E0E0E14110C0C0C0C0C11110C0C0C0C0C0C110C0C0C0C0C
          0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0CFFC000110800700038
          03012200021101031101FFDD00040004FFC4013F000001050101010101010000
          0000000000030001020405060708090A0B010001050101010101010000000000
          0000010002030405060708090A0B1000010401030204020507060805030C3301
          0002110304211231054151611322718132061491A1B14223241552C162333472
          82D14307259253F0E1F163733516A2B283264493546445C2A3743617D255E265
          F2B384C3D375E3F3462794A485B495C4D4E4F4A5B5C5D5E5F55666768696A6B6
          C6D6E6F637475767778797A7B7C7D7E7F7110002020102040403040506070706
          05350100021103213112044151617122130532819114A1B14223C152D1F03324
          62E1728292435315637334F1250616A2B283072635C2D2449354A31764455536
          7465E2F2B384C3D375E3F34694A485B495C4D4E4F4A5B5C5D5E5F55666768696
          A6B6C6D6E6F62737475767778797A7B7C7FFDA000C03010002110311003F00A6
          E3EE1A72EF3D3EE5B9D121DEC988D61619049690635D64F3FC95D0F442D6B1A6
          21D3AA30F999A7B3D2D46C60036B456621F0E3CFE6BB6A9EF7924319046D3B6C
          D0905C5A7E8FEEEDDCA55125827491D90DA1C1B2DB43C6843CBCE80B87E6FBDA
          EFCEF7A69DCAD0AF5ECD84FA6411BF583036901BB9BFCE3B7EEFCC4EDB6CDED0
          5A3638C480E980DDE5DFE77B149EE7B5CDDCF6B492E024ED99FE6DBB5DBB7398
          EFCE4275B6369863DA5C4CB5E5C1C0FF00235F7FF2FE87FC17FC22556A793EBB
          924E5D81E62B97C3CC8036BB6B757EDDBB9255FA9DCEBAE78374BE47D070904F
          BBE957E9FF0038D0FD8CFF00CF89293F495FA2FF00FFD0A9EDDEC9FDEF69F02B
          A0E9459EC8F15CFF00BA5B0247E778AD9E8662C01C74274461BB349EC693ED10
          AA87E2ECDC18E683539B2D74C56482E688DFBB6EEFEBFD3AD59A1CC2D002667D
          A4346E602F0D1C4346E9F76C737F91FC84D3B95AB1750FDCD771639A609264BD
          BECEFECF62AF96FF0052A7B1C1DBDFFA3717B9A488DE37347B777D37FE91AAD0
          FB48B3806B2E702468430FF36EFE539BF47DDFF4D64F5AEA37E3D6C63AB871D0
          BA01E5ADDD327F9BDEEFF028C37F251791C96D75DEF2F7173C06D85C1DA38020
          55EDFA2CFF000692183941BEE6B67638FE6CEF924360433D37CA49C9E8FF00FF
          D1A84365BBB99D349D56BF4EF4C3DBB5F33DBBAC970D5BEDDDAC4F82B784E2DB
          D91DCC231D19A5B3DBE0B81688F9286DC615EDF55C186BDA43DB236C87B6C7B4
          ECF7ED7FD3FE5FA8A5D398E0C0E3A020429544443A8DA5ADFE6DAD248DC7F48C
          F7FE89FF00BFF4909EEB42435B2E96B9C49DC0B5C40105C3DBE8B88FA0FF00A6
          B97EB1978B65F6305806C8680D616B44177B8346EFDE5D3086D847A0369711BE
          0B800D1FA2B76FF29BEDFD1FBD7079B631D9D6D9552D697DA58F2D040986EEED
          FA3FDDDF6230EAA43EAB49768EF64EFF00691B63F7BFCD4908BDCF6EE7D1A961
          330E899DAEA9ED6FE93DC92297FFD2AC43B768E813C4FF00AFF9AACE290DBD85
          DC03AC2AE768B20F3B8771E7B745671C7E95BE467EED53EB5653B3DC6099C669
          1A18D27F0FDE526D57C8DD6C374900B89F69DFF49FFBFEE63BFB0ABE1E4D6CA4
          122181B26489D0B990C6FF008476E67E6AB272AA00CCCC1207891F9BCFD27264
          BE6280C2F7594E3BCBAD034743883327E8C4EEDBB1BEDFA16FFE895C2E6B6D39
          761AACDD492D226013AFE95BA35BF4FF00D26F5D47D64EA2CAB0856C05DBDFB4
          B841FA3D9927DDBA57276E5321CF01CE870691A0D48DDF9C53A3B2BAAC197076
          B6489719E7424398DDA7F73DCDDDB924BED1506BDC7706D606E2E11F4BB7BBDA
          922A7FFFD3143F7081ED91E1C46A8D43B658D7F81942DADDC3533220703408A0
          278642F69D35FEA51B8EA7CD5977AF2F2D82D81B040267DBB8FF0067F4AB3FA2
          BE686B7F92AE5CCA9CF76F2E1B981B0D6C9D4BB606BF5FE5FE85327BA86CE3FD
          67BF2EAF4D81AC7D6F2F81A4803739AEE59EF77E8D7303ED2EDDBDA1AF734998
          07DD2DF4F7C177E62D3EBB914D9D45F51B096B2D1ED78D3739A7754CDDF45966
          EFA0B35ADADA3D26B9EC686103F35A369D8F76E86FBF7BBDE9E06810C7F5C86F
          B6B923DC48EF3F9DEEFCDFEDFF00E9449DB531AEDFBE26091B61B14FB5DB3FAA
          DF6FF5124E53FFD4891FA4FA33A8F77C915A1476FB84BA0488130741FF009CA2
          80A55D6F49D04CD6238022568DB26C70148B0EC03507513AB3768CECDFD1FD35
          95F57CF65A990E2C2F77AAD600C1ED2E2DD67F93F47D4DCDFD237F4CA39EE91B
          3C667BAC767DBEA522B7BAE89735C4103F99B5B1F4363457F4F67F61018D96B5
          8FAA2402E10368DC77387BC9FA3F4DE8B936D965EFBECC96FB9C1CE25D32D1EC
          B59FBACFF8C43631FB4B858373039AE3B9CF6033BA5DBCB3E837D8A4A5B68E5A
          030BAB01A43B717020005DEEFA5F41AEDBEB7BD2457EE0FD2C6896BC09711AFE
          F7A706BFD0FEF7D3491A55BFFFD5210DDE343C8E0E83F7744501287EED3E8C88
          3A71FF0054A6029C045BB1F578CD8E1FBA2568F55A98EC0C8738BB5601A3881F
          BBF9BFD6FCE597D089195039234074D55DEAEFCB1D39DB9AC973EB6C0D349DCF
          3F4DDFF55FF6E28E43D61783E979B7D154C86C685A63490E32E69FED281C7ADD
          BA665C5CE99EEEFA5FEAF468B37BF791B67D83BC4BB9FECEC4FB5494B2D01A18
          7E949FA5A4E8771DFEE1FD677B12478491A55BFFD93842494D04211A56657273
          696F6E20636F6D7061746962696C69747920696E666F00000000550000000101
          0000000F00410064006F00620065002000500068006F0074006F00730068006F
          00700000001300410064006F00620065002000500068006F0074006F00730068
          006F007000200036002E003000000001003842494D04060C4A50454720517561
          6C69747900000000070001010100010100FFEE002141646F6265006480000000
          0103001003020306000000000000000000000000FFDB0084000C08080809080C
          09090C110B0A0B11150F0C0C0F1518131315131318110C0C0C0C0C0C110C0C0C
          0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C010D0B0B0D0E0D
          100E0E10140E0E0E14140E0E0E0E14110C0C0C0C0C11110C0C0C0C0C0C110C0C
          0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0CFFC200110800
          9D004F03012200021101031101FFC400A9000002030101000000000000000000
          0000020601030504000100030100000000000000000000000000000103021000
          0201040104020203010000000000000102030011120405102131134114223220
          4223061100020103020403040609050100000000010211002112312241324203
          51526210617213718182A2B2232091B192C2D2334353A1E26383B30412000202
          010501000000000000000000001001002011502131516171FFDA000C03010102
          110311000000E3BA9BD5D9B632F677390139BF44C072A833A8D0E7E9E7EDCED8
          B6F137353AA47D3731654184BDB383506DAEC9ED9F7D6D837383029B803CDD18
          58DD74ED8745058D6FB0A9B4EE76C78E4E57B7937672D13569D85049E8B8A639
          1919B266568CE69F4383D69375909877362A3499E990893C75CDB5FAA82086CC
          FC48B9BD3DAC5DF55BCF36B3917D7551070C22F101B32D30A5AF9FA1C32D277B
          DEB6626CA80EC836BDBB8BB08DCC3DE5E9EB07DD235C575DF2C82F10A74B3FB8
          6C0BEC0B52D724CCDB03E2F0BFFFDA0008010200010500FECFE3A0FD40EEFE3A
          350A61DA945C9EE476A3FAD276E96EE7F5A51DA8F93FAD01DA80EC7C0F3D3B51
          F0BE7A0A3E13CD5A85AC7C47E7A7FFDA00080103000105003E0751E5BC0EAB46
          874343C1AF9A3DFAFCD7CF4F9FE1F3FC079357ABD5E81EEDE2F57E8BE5BC74FF
          DA0008010100010500637AD4232D042C239829320557D8087DF193F605B959ED
          A6F30799EC469AE6DC6C78A88D1958C799F5BD32465A3F538E625D6104AD1ACE
          C463A23F3D0624C62CB2490E4A216323448480A39B975E3533AB3936AD1C6B8E
          20BA1BAC8F0868DF5D99444C99A03CEC3ED6B4203582C0F81E2AED518B2B1833
          65850236B96D9975D61E73790EC9935BD2476D7C4D7172E350485848F0165313
          BB98633CB88C433463330458B5826AB466B8F78C1D56B891E10C8DAE593D4C9C
          C6FEB26C6D4E9919D423D82C6463C5B9CF481A76843BA42A11B5C1E706A7DC69
          D326935BD2C40A4B5B8A179B5ADEB95A124CBAEB520D744E4DB5FED96815447A
          C4B05C92D6E359966D43FE4D044C445184DF78A087912BB3308630AA8AB4D6CD
          2D8F183FDF4E60CABB3130F7C76FFA3E45150CF1C8CBB70B033C411C2DD2C578
          C2A26D48A378E45862565D751CF4BA0D2E707B6FAEAED269C94701496C75DB19
          78D6BC72E015E7841E765D7976A38A02A5A0B8101538A940318C7E7C637E3363
          EB9E481619362092491A31522C4CCA902D3E20A018A0FCB893DA6C7D7C9EC6BC
          1A8C20A8F0618C2597D45D802C80155EC7867FC66C7D7C8BEA0D50DAA599D298
          479054C5B1A5EE00AE1CF79CA08F9FD98224630288CC21818D5C7A053E149895
          02B85BB48CA187390C2BB4D121630C64FA101586352710CA0580AE21B19DB6A0
          53CCEC21DB560E2D56AB51F5E4B8D857197FB07EBDF92F47DB18D76AED5DABFF
          DA0008010202063F007F622A739F62A2B30DDD94128E9B047BAAC067FFDA0008
          010302063F00D4BFFFDA0008010101063F0024499F0D6948B0B40A526B0C1B2E
          1A5E674DDE86A93ADB6DA775947EF5364AC318BED8B9C57AAA14E4F00E0089BF
          D78D0211A197307681169D5BD54BDC5901E23ED72CD0713B5C4F1363E5596FB3
          466E0EBC2942DA749A5526AE2FE22C6DEA1BB8D14F96C1ED0C3193174DC5BFE3
          CBF329838C480A5A4C18E65DCA7A5A8F6D81DCB1326E14F8CFAE885C8481724C
          959251B2CB2F352A063B340328D90413E9DEBBE9302D83BCB3AB020F8E4EE73D
          AA9D144B1B713A5230B2C08A028538704142A0B69AE9BC1C97FA951190EE2823
          292081F17972A56C4E25710EA76858C8C437953A6B687ED13200189981905DDF
          331EAC299A1B94B95B8B1B4156C7FC5C9C941F124819A88936B32FC6BCB449A5
          F081143D8F9A4C598C086207CCC62776DF352A817298AA93231BDB6B32FF006E
          800A02C901481ADC3AFE3A809DC2E92402D3C064777731E575A1DC209536643E
          0A72933E566E8A56542665027D24B7717127E5F4D49B0A52BA5A281A15DD9532
          04770CDA085CB8F96B3225BB40B4CCB46EE2DBBCD4891BB9932B9B96630C72F2
          73533BCA9C598C486DBB5F729FE2AEDF6D7253752B2BE9B1DDEA46D943B83228
          A6F04C82DBB5CB9B7635A4FBA94C40D60D0142BB81C1110AE45A4363CC54FBFA
          AAC9FD459CC81B976FDAEAEAA47C240E5EE08200866F1CB1C2B02194B02A158C
          F8BB5D599BF33F828319254983919B9C9A6FBAB1C4052722A2C247A68C891A10
          2B7DA7435B6E07B1F3498B31810C40F998C4EEDBE6A0003915854CA410325C79
          9BB7D14062144901081A8C95B4FB55F28862518892D37E49DCFBBAB0FF00B2B2
          55669306C041240EA2BE3D35F31811DBB6E3110746B1CA8E5A5BF6D02BA563EC
          EE928CC408EE41B41032EAF285A2C449EDA93324B443754E5E6A540B2C065DB0
          D73C790B13FE3F82BB8E10A1556C94710861F2C4E2D4A8CA72631062C4627CDE
          A5E4ACF167EDC8C803201B155C4B7AB956A0F1D3DFC2AD6AF1A1029C9462548C
          8A9824C809B5595FE0CA9CE39620066B190D8C6F63EAEBA3DC200F96B20DC300
          67EDAFA569E4923BA0855DCDB7D1AB52300C46455181372773756FDCB5840521
          8EC989303A41C5BF2EAE09316A11A1B8A0178EB401F0A965C8CC8C8923F53561
          88C6D637D34FD9458A89611971B7AF9B85076918E90C674C6ED35846D9903C0F
          A7CB463A8C9BCDE8D8930623C26F4234811409D2408FA4E3481149C9431D2C0E
          9949FC352A491008201BC96165E6E8A99918979F00313FC749DA5CA0EE2C0183
          38E3FF00A500AD248CB8E96FE6A2C4950049C811685FE7A0F94A9120804D86BA
          51C81263C62DD5C6811306F7A190B483F583950104285C60122DE060EEA920DE
          6E099B07EE734E5E7A6389855CCC48205A38AE3FD2FB9410CAB259099008409D
          CCBF06FA2C80A95842B11CC500389E5A3DAC4824EB0624E118B74FF6E9433CE0
          6C65AC440C8B0FFD2A5A46DD64E942348B529F7D0A0597282001EF6FCBE3F1D3
          B3036ED92E2C44296DA7E187F452F73B62430214A12012A0AC6A3A32C2862A7B
          7B8C026F2A416EA7FF001537740CF15C8917100FEEE5F97F72B909DA5829BDAD
          9A059E9654D9526640B7D142348B50FA69456E05848B03066463C57AA9B61844
          C49B5812DDBF96D7C9B7AD2B7282C4F6C488931D28CDFE4A93DAC8AB468B6624
          798F5E55BB716210AF891F98B97C3CD46D0149ED953A6E20C63CBBF2A394E9AC
          C71A11A45A852D6E05848B03066463C57AA9DC2B649DB810740334C75E9DF481
          50BAC9F96D33057A54BB65FDBF8289C4AE4C6CC64965F0BB79295940DED21D62
          E44EBF673A2A01BB1693A12BB1B8F455C498B47D3C2810201BDFD8047D75B816
          122C0C19918F15EAA65EE21C42849F092DDB5EDB5F26DEB48541492420CBC4A9
          C71463D4FCB527B249568D16433627CDD59D2962254C2DF891E1E6C69980742A
          4291324CC6D065B1F999735491323FD38D4FB00ADE09591A18BCCF34AE3FBD4A
          98B2BF6C2C09B472C7361B3E65103B6CDF261AF72A203DB36C97E1A6EDC15C77
          366D2068FC5DBCDCD442AC31600900092467F7577515C5802D025899397CB2C3
          779DEB26074B5FDE3DF431D3D91E15067EA247E1A18A010BFB4E7F8A89C44B08
          6F78F579A9891397303241D3A397A6A45A08200D01030FC1B6890398C9993A1C
          B8FA8D5C198807F57BEADED2A5F72EA0027DFC2BBC667E50DD1EE1FEDA244882
          56FE20C7E81CA72E313156D3D8B1472F133338CC365FF1F2E79D7FF54FBFE66B
          11BB4FBFC95B6224E9E33BBEF7E87FFFD9}
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.MaterialLibrary = GLMaterialLibrary
        Position.Coordinates = {0000F0410000F041000000000000803F}
        Visible = False
        Width = 50.000000000000000000
        Height = 600.000000000000000000
        Rotation = 0.000000000000000000
      end
      object GLHUDMovePosition: TGLHUDText
        Position.Coordinates = {0000A0400000A041000000000000803F}
        Visible = False
        BitmapFont = GLWindowsBitmapFont
        Text = 'Hello'
        Rotation = 0.000000000000000000
        ModulateColor.Color = {0000000000000000000000000000803F}
      end
      object GLHUDTextScale: TGLHUDText
        Position.Coordinates = {0000A0400000A040000000000000803F}
        BitmapFont = GLWindowsBitmapFont
        Text = 'Scale [ X = 1.0, Y = 1.0, Z = 1.0 ]'
        Rotation = 0.000000000000000000
        ModulateColor.Color = {0000000000000000000000000000803F}
      end
    end
  end
  object dsData1: TDataSource
    Left = 453
    Top = 436
  end
  object dsData2: TDataSource
    Left = 578
    Top = 436
  end
  object ImageList: TImageList
    Left = 683
    Top = 299
    Bitmap = {
      494C010116001800200010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000008400
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000063313100633131006331310063313100633131009C6363009C6363009C63
      6300633131000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C6363009C63630052525200CE63
      6300CE9C9C00CE9C9C00CE9C9C00CE9C9C000000000000000000000000000000
      0000000000009C63630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C63630052525200CE9C
      0000CE9C9C0000639C0000000000CE9C9C00CE9C9C00CE9C9C009C6363000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C636300525252009C9C
      6300CE9C9C00000000000031CE0000000000CE9C9C009C636300000000005252
      520000000000633131009C636300000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C63630052525200CE9C
      0000CE9C9C00CE9C9C00CE9C9C00CE9C9C009C6363009C63630000000000FF9C
      9C00FFFF6300CE6363009C636300000000000000000000000000840000000000
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C636300525252009C9C
      630000000000000000000000000000000000000000009C63630000000000FF9C
      9C00FFFF6300FFCE630063313100000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C63630052525200CE9C
      000063313100CE9C9C00CE9C9C00CE9C9C00CE9C9C009C63630000000000FF9C
      0000FFCE6300FF9C000063313100000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C63630052525200FF9C
      9C0063313100CE9C9C009C9C6300FFFF3100000000000000000084848400CE00
      630000000000633131009C6363009C6363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF9C9C009C636300FF9C9C000000
      000063313100CE9C9C0063313100633131006331310063313100000000008484
      8400313163002929290000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C636300FF9C9C00000000009C63
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000009C63630000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000636331009C636300CE9C
      9C00CE9C9C00CE9C9C00CE9C9C00CE9C9C00CE9C9C00CE9C9C00CE9C9C00CE9C
      9C00848484000000000000000000000000000000000000000000840000000000
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6E7E700FFEF
      CE00CE9C9C00CE9C9C0000000000639C9C00639C9C00639C9C00639C9C009C63
      6300000000000000000000000000000000000000000084000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF9C
      9C00FF9C9C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00C6C6C600C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000008484
      8400C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000000000FFFF000000
      000084848400C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      000000000000000000000000000000000000FFFFFF00FFFFFF000000000000FF
      FF000000000084848400C6C6C600C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF000000000000000000000000000084848400C6C6
      C600C6C6C60000000000C6C6C600C6C6C600C6C6C60000000000000000008484
      840000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      000000FFFF0000FFFF000000000084848400C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF00000000000000000000C6C6C600000000000000
      0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      00000000000000FFFF0000FFFF000000000084848400C6C6C600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0084000000840000008400000000000000FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      000000FFFF0000FFFF0000FFFF0000FFFF00000000008484840084848400C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF000000000000000000000000000000000000FFFF
      FF00FFFFFF00FF00000084000000840000008400000084000000FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000008484
      8400C6C6C600FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      0000FFFFFF008400000084000000840000000000000084000000FFFFFF000000
      000000000000848484000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      000084848400C6C6C600FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      0000000000008400000084000000840000008400000000000000000000000000
      000084848400000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C6008484
      840000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      000084848400FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF000000000084848400FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000007B0000007B0000007B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000007B0000007B00FFFF
      FF00FFFFFF00FFFFFF0000007B00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000007B000000
      7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      7B0000007B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000007B0000007B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000084840000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000007B00000000000000
      00000000000000007B0000007B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000084840000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD0000007B0000007B000000
      7B0000007B0000007B0000007B00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      000000000000000000000000000000FFFF000084840000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000848400000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD007B0000007B0000007B00
      0000BDBDBD007B0000007B0000007B000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00008484000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00000000007B000000BDBD
      BD00BDBDBD00BDBDBD007B00000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      000000FFFF000084840000000000000000000000000000FFFF0000FFFF000084
      8400000000000000000000000000000000000000000084000000840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD007B0000007B00
      00007B0000007B0000007B000000BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      000000FFFF00008484000000000000000000000000000000000000FFFF000084
      8400000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00000000007B00
      0000BDBDBD007B00000000000000BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      000000FFFF0000FFFF0000848400000000000000000000FFFF0000FFFF000084
      8400000000000000000000000000000000000000000084000000840000008400
      00008400000084000000FFFFFF00FFFFFF00FFFFFF0084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD007B00
      00007B0000007B000000BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008484000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD000000
      00007B00000000000000BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      00008400000084000000FFFFFF00FFFFFF00FFFFFF0084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B000000BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD0000000000BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000000000000000
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
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00000000000000000000000000FFFF
      FF00840000008400000084000000840000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFF
      FF00840000008400000084000000840000000000000000848400848484000084
      8400848484000084840084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484000000000000000000000000000000000000000000840000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400848484000084
      84000000000000FFFF00000000000000000000FFFF0000000000848484000084
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B000000000000000000000000007B7B7B00000000000000
      FF00000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000000000FF000000
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B000000000000000000000000007B7B7B000000000000FFFF007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000FF00000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B00000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF00000000007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD00FFFFFF00BDBDBD00FFFFFF007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B00FFFFFF00BDBD
      BD00FFFFFF000000FF00FFFFFF00BDBDBD00FFFFFF007B7B7B007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00BDBDBD00FFFF
      FF00BDBDBD000000FF00BDBDBD00FFFFFF00BDBDBD007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF0000000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF000000
      FF000000FF000000FF000000FF000000FF00FFFFFF007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00BDBDBD00FFFF
      FF00BDBDBD000000FF00BDBDBD00FFFFFF00BDBDBD007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B00FFFFFF00BDBD
      BD00FFFFFF000000FF00FFFFFF00BDBDBD00FFFFFF007B7B7B007B7B7B000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD00FFFFFF00BDBDBD00FFFFFF007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B000000000000000000000000007B7B7B000000000000000000000000000000
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
      0000000000000000000000000000000000006B6B6B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D6E7FF000018520000185200000000000000
      0000000000000000000000000000000000000000000000000000C6C6C60000FF
      FF00C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000315200E7E7E70039393900001852000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00C6C6
      C60000FFFF00C6C6C60000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000315200B5B5B500E7E7E700CECECE00525252000018
      5200000000000000000000000000000000000000000000FFFF00C6C6C60000FF
      FF00C6C6C6000084840000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B5C6FF0021212100E7E7E700E7E7E70063636300525252003131
      31000018520000000000000000000000000000000000FFFFFF00000000000000
      0000000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000217300F7F7F700B5B5B500E7E7E7006BC6FF00CECECE00B5B5
      B500ADADAD00001852000000000000000000000000000000000000FFFF00C6C6
      C60000FFFF000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000084848400848484000000
      000000000000000000000000000000000000000000000000000000000000CECE
      CE00004A7300F7F7F70052525200E7E7E700E7E7E700CECECE00ADADAD00C6C6
      C600B5B5B500A5A5A50000185200000000000000000000FFFF00C6C6C60000FF
      FF00C6C6C60000FFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF0000000000848484008484
      8400000000000000000000000000000000000000000000000000000000009C9C
      9C00B5B5B500DEDEDE00B5B5B500B5B5B500E7E7E700B5B5B50000315200C6C6
      C600B5B5B500003152006B6B6B0000000000000000000000000000FFFF00C6C6
      C60000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00000000008484
      8400848484000000000000000000000000000000000000000000FFFFFF000031
      5200FFFFFF00C6C6C600E7E7E700CECECE00ADADAD00848484008CD6FF00C6C6
      C6009494940021212100000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF000000
      00008484840084848400848484000000000000000000F7F7F700004A7300FFFF
      FF00E7E7E700F7F7F700C6C6C600DEDEDE00B5B5B500CECECE00CECECE005252
      52000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF000000000084848400000000000000000000000000007BBD00004A7300FFFF
      FF0052525200F7F7F700F7F7F700E7E7E700E7E7E700DEDEDE00C6C6C6000063
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00848484000000000000000000000000000000000031313100004A
      73003131310021212100B5B5B50094949400E7E7E700CECECE00001852004ABD
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      00006B6B6B00525252003131310021212100E7E7E70000185200000000000000
      00002121210000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000008484840084848400000000000000000000000000000000000000
      0000F7F7F700A5A5A50031313100003152000031520000000000000000000000
      0000003194000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF00FFFFA80000000000FFFFAEFB00000000
      8003CCFB000000000003FEFB000000000003FCFB0000000000018EFB00000000
      0000D8000000000000009EFB000000000000DCFB000000000000FEFB00000000
      0001FCFB0000000000038EFB000000008003D80000000000C007AEFB00000000
      E3FFDCFB00000000FFFFFEFB00000000FFFFFCFF0000FFFF0001F07F0000FEFF
      0001E01F0000FEFF000180070000FEFF1FF100030000FEFF1DF180010000FC7F
      1CF100000000FABF1C7100010000F6DF1C318001000000011C71C0030000F6DF
      1CF1A0070000FABF1DF1D00F0000FC7F1FF1503F0000FEFF000127FB0000FEFF
      000187C70000FEFF0001E01F0000FEFFFFFFFFFFFFFFFC3FFFFF8001FE7FE00F
      EFFD8015FC3FC007C7FF8001FC3F8003C3FB8023FE7F8001E3F78001FC3F0001
      F1E7B9F7FC3F0000F8CF0000FC3F0000FC1F0000FC1F0000FE3F0000F20F0000
      FC1F4100E1070001F8CF0000E1870001E1E72200E0078001C3F30000F00FC003
      C7FD1400F81FE007FFFF0000FFFFF01FFFFFFFFFFFFFFFFFFFFFF9FFFFFFFC00
      FFFFF6CFFE008000FFFFF6B7FE000000FFFFF6B7FE000000FFF7F8B780000000
      C1F7FE8F80000001C3FBFE3F80000003C7FBFF7F80000003CBFBFE3F80010003
      DCF7FEBF80030003FF0FFC9F80070FC3FFFFFDDF807F0003FFFFFDDF80FF8007
      FFFFFDDF81FFF87FFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFF8FFE3FFFFFFFF
      FFF1FFC3FFFF8823FFE3FB83FFFFBEFBFFC7F907FF03BEFBE08FF80FFF87FFFF
      C01FF01FFF0FBEFB803FF03FB8DFBEFB001FE01FC7FF8823001FE00F93FFBEFB
      001FC07FFCFFBEFB001FC1FFFF2FFFFF001F87FFFF87BEFB803F9FFFFF03BEFB
      C07FFFFFFFFF8823E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FC7FFFFFFFFFF
      FE3F0000FFFFFFFFFC1F0000FFBF807FFC0F0000FF9F803FF8070000FFAF801F
      F0030000C037800FE0010000DFFBC007E0010000DFFBE003C0030000C037F001
      80070000FFAFF801800F0000FF9FFC01C00F0000FFBFFE01F0370000FFFFFF01
      F0770000FFFFFFFFFFF7FFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmap|*.bmp|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Bitmap'
    Left = 685
    Top = 376
  end
  object OpenSurfer: TOpenDialog
    DefaultExt = 'grd'
    Filter = 'Surfer Grid File|*.grd|All Files|*.*'
    Title = 'Open a Surfer Grid File'
    Left = 448
    Top = 319
  end
  object GLWindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 530
    Top = 155
  end
  object ActionManager1: TActionManager
    ActionBars.Customizable = False
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = acImportCSV
                Caption = 'C&omma Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acImportSSV
                Caption = 'S&pace Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acImportFixedFormat
                Caption = '&Fixed Format (Space Separated) Value File'
                ImageIndex = 1
              end
              item
                Action = acImportTSV
                Caption = '&Tab Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acImportClipboard
                Caption = '&Clipboard'
                ImageIndex = 0
              end>
            Action = acImport
            Caption = '&Import'
            ImageIndex = 0
          end
          item
            Action = acEmptyOriginal
            Caption = '&Empty'
            ImageIndex = 3
          end
          item
            Action = acSortOriginal
            Caption = '&Sort'
            ImageIndex = 13
          end
          item
            Caption = '-'
          end
          item
            Action = acProcess
            Caption = '&Process'
            ImageIndex = 2
          end>
        ActionBar = ActionToolBar_Original
      end
      item
        Items = <
          item
            Items = <
              item
                Items = <
                  item
                    Action = acSnapShot
                    Caption = '&Snapshot...'
                  end
                  item
                    Action = acRenderBitmap
                    Caption = '&Render to Bitmap...'
                  end
                  item
                    Action = acRenderBitmap2
                    Caption = 'R&ender to Bitmap (x2)...'
                  end
                  item
                    Action = acRenderBitmap300
                    Caption = 'Re&nder to Bitmap (300 dpi)...'
                  end
                  item
                    Action = acRenderBitmap600
                    Caption = 'Ren&der to Bitmap (600 dpi)...'
                  end>
                Caption = '&Render'
              end
              item
                Action = acLoadGridMeta
                Caption = '&Load Grid Metafile...'
              end
              item
                Action = acSaveGridMeta
                Caption = '&Save Grid MetaFile...'
              end
              item
                Caption = '-'
              end
              item
                Action = acExit
                Caption = 'E&xit'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = EditUndo1
                ImageIndex = 8
                ShortCut = 16474
              end
              item
                Action = EditCut1
                ImageIndex = 9
                ShortCut = 16472
              end
              item
                Action = EditCopy1
                ImageIndex = 10
                ShortCut = 16451
              end
              item
                Action = EditPaste1
                ImageIndex = 11
                ShortCut = 16470
              end
              item
                Action = EditSelectAll1
                ShortCut = 16449
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Items = <
                  item
                    Action = acImportCSV
                    Caption = '&Comma Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acImportSSV
                    Caption = '&Space Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acImportTSV
                    Caption = '&Tab Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acImportFixedFormat
                    Caption = '&Fixed Format (Space Separated) Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acImportClipboard
                    Caption = 'C&lipboard'
                    ImageIndex = 0
                  end>
                Caption = '&Import Raw Data'
                ImageIndex = 0
              end
              item
                Action = acEmptyOriginal
                Caption = 'E&mpty Raw Data'
                ImageIndex = 3
              end
              item
                Action = acSortOriginal
                Caption = '&Sort Raw Data'
                ImageIndex = 13
              end
              item
                Action = acProcess
                Caption = '&Process Raw Data'
                ImageIndex = 2
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = acExportProcessedCSV
                    Caption = '&Comma Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acExportProcessedSSV
                    Caption = '&Space Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acExportProcessedTSV
                    Caption = '&Tab Separated Value File'
                    ImageIndex = 1
                  end
                  item
                    Action = acExportProcessedLaTeX
                    Caption = '&LaTeX File Segment'
                    ImageIndex = 1
                  end
                  item
                    Action = acExportProcessedXML
                    Caption = '&XML File'
                    ImageIndex = 1
                  end
                  item
                    Action = acExportProcessedClipboard
                    Caption = 'Cl&ipboard'
                    ImageIndex = 0
                  end>
                Caption = '&Export Processed'
                ImageIndex = 0
              end
              item
                Action = acEmptyProcessed
                Caption = 'Emp&ty Processed'
                ImageIndex = 3
              end
              item
                Action = acSortProcessed
                Caption = 'S&ort Processed'
                ImageIndex = 13
              end
              item
                Action = acFocusProcessed
                Caption = '&Focus on Processed'
                ImageIndex = 17
              end>
            Caption = '&Data'
          end
          item
            Items = <
              item
                Action = acCenterOnPoints
                Caption = '&Center View on Points'
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Action = acPreferences
                Caption = '&Preferences...'
                ShortCut = 113
              end
              item
                Action = acColourTool
                Caption = '&Colour Tool'
              end
              item
                Caption = '-'
              end
              item
                Action = acAxes
                Caption = '&Axes'
              end
              item
                Action = acBenchmark
                Caption = '&Benchmark'
              end
              item
                Action = acLocator
                Caption = '&Locator'
              end>
            Caption = 'T&ools'
          end
          item
            Items = <
              item
                Action = acHelp
                Caption = '&Help'
                ImageIndex = 14
                ShortCut = 112
              end
              item
                Action = acTipOfDay
                Caption = '&Tip of the Day...'
                ImageIndex = 14
              end
              item
                Action = acOpenGLContext
                Caption = '&OpenGL Information...'
              end
              item
                Caption = '-'
              end
              item
                Action = acAbout
                Caption = '&About glData...'
              end>
            Caption = '&Help'
          end>
        ActionBar = ActionMainMenuBar1
      end
      item
        Items = <
          item
            Action = acViewMode
            Caption = '&View'
            ImageIndex = 4
          end
          item
            Action = acPickMode
            Caption = '&Pick'
            ImageIndex = 5
          end>
        ActionBar = ActionToolBar_Modes
      end
      item
        Items = <
          item
            Action = acProcess
            Caption = '&Process'
            ImageIndex = 2
          end>
      end
      item
        Items = <
          item
            Items = <
              item
                Action = acExportProcessedCSV
                Caption = '&Comma Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportProcessedSSV
                Caption = '&Space Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportProcessedTSV
                Caption = '&Tab Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportProcessedLaTeX
                Caption = '&LaTeX File Segment'
                ImageIndex = 1
              end
              item
                Action = acExportProcessedXML
                Caption = '&XML File'
                ImageIndex = 1
              end
              item
                Action = acExportProcessedClipboard
                Caption = 'Cl&ipboard'
                ImageIndex = 0
              end>
            Action = acExport
            Caption = 'E&xport'
            ImageIndex = 0
          end
          item
            Action = acEmptyProcessed
            Caption = '&Empty'
            ImageIndex = 3
          end
          item
            Action = acSortProcessed
            Caption = '&Sort'
            ImageIndex = 13
          end
          item
            Caption = '-'
          end
          item
            Action = acFocusProcessed
            Caption = '&Focus'
            ImageIndex = 17
          end
          item
            Caption = '-'
          end
          item
            Action = acBoundingBox
            Caption = 'S&calar'
            ImageIndex = 7
          end
          item
            Action = acBoundingBoxVector
            Caption = '&Vector'
            ImageIndex = 7
          end>
        ActionBar = ActionToolBar_Processed
      end
      item
        Items = <
          item
            Action = acOpenVTK
            Caption = '&Open VTK Grid'
          end>
        ActionBar = ActionTool_Grids
      end
      item
        Items = <
          item
            Action = acImportSurfer
            Caption = '&Import'
            ImageIndex = 0
          end
          item
            Action = acFocusSurfer
            Caption = '&Focus'
            ImageIndex = 17
          end
          item
            Items = <
              item
                Action = acSurferDefaultAll
                Caption = '&Default All'
                ImageIndex = 8
              end>
            Action = acSurferDefault
            Caption = 'D&efault'
            ImageIndex = 8
          end
          item
            Items = <
              item
                Action = acSurferDeleteAll
                Caption = '&Delete All'
                ImageIndex = 3
              end>
            Action = acSurferDelete
            Caption = '&Delete'
            ImageIndex = 3
          end
          item
            Caption = '-'
          end
          item
            Action = acSurferAnimation
            Caption = '&Animate'
            ImageIndex = 16
          end>
        ActionBar = ActionToolBarSurfer
      end
      item
        Items = <
          item
            Action = acImportArcInfo
            Caption = '&Import'
            ImageIndex = 0
          end
          item
            Action = acFocusOnArcInfoGrid
            Caption = '&Focus'
            ImageIndex = 17
          end
          item
            Items = <
              item
                Action = acArcInfoDefaultAll
                Caption = '&Default All'
                ImageIndex = 8
              end>
            Action = acArcInfoDefault
            Caption = 'D&efault'
            ImageIndex = 8
          end
          item
            Items = <
              item
                Action = acArcInfoDeleteAll
                Caption = '&Delete All'
                ImageIndex = 3
              end>
            Action = acArcInfoDelete
            Caption = '&Delete'
            ImageIndex = 3
          end
          item
            Caption = '-'
          end
          item
            Action = acArcInfoAnimation
            Caption = '&Animate'
            ImageIndex = 16
          end>
        ActionBar = ActionToolBarArcInfo
      end
      item
        Items = <
          item
            Action = acGeothermalGrid
            Caption = '&TOUGH'
            ImageIndex = 0
          end
          item
            Action = acGeothermalTETRAD
            Caption = 'T&ETRAD'
            ImageIndex = 0
          end
          item
            Action = acSortGeoLayers
            Caption = '&Sort'
            ImageIndex = 13
          end>
        ActionBar = ActionToolBarGeothermal
      end
      item
        Items = <
          item
            Caption = '-'
          end
          item
            Caption = '-'
          end>
      end
      item
      end
      item
        Items = <
          item
            Caption = '-'
          end
          item
            Caption = '-'
          end>
      end>
    Images = ImageList
    Left = 582
    Top = 313
    StyleName = 'XP Style'
    object acImport: TAction
      Category = 'Original'
      Caption = 'Import'
      Hint = 'Import from CSV file'
      ImageIndex = 0
      OnExecute = acImportExecute
      OnHint = acImportHint
    end
    object acImportCSV: TAction
      Category = 'Original'
      Caption = 'Comma Separated Value File'
      Hint = 'Import from CSV file'
      ImageIndex = 1
      OnExecute = acImportCSVExecute
      OnUpdate = acImportCSVUpdate
    end
    object acImportClipboard: TAction
      Category = 'Original'
      Caption = 'Clipboard'
      Hint = 'Import from clipboard'
      ImageIndex = 0
      OnExecute = acImportClipboardExecute
      OnUpdate = acImportClipboardUpdate
    end
    object acEmptyOriginal: TAction
      Category = 'Original'
      Caption = 'Empty'
      Hint = 'Empty original table'
      ImageIndex = 3
      OnExecute = acEmptyOriginalExecute
      OnUpdate = acEmptyOriginalUpdate
    end
    object acViewMode: TAction
      Category = 'Modes'
      Caption = 'View'
      Checked = True
      Hint = 'View mode'
      ImageIndex = 4
      OnExecute = acViewModeExecute
    end
    object acPickMode: TAction
      Category = 'Modes'
      Caption = 'Pick'
      Hint = 'Pick mode'
      ImageIndex = 5
      OnExecute = acPickModeExecute
    end
    object acMoveMode: TAction
      Category = 'Modes'
      Caption = 'Move'
      Hint = 'Move mode'
      ImageIndex = 6
      OnExecute = acMoveModeExecute
    end
    object acPreferences: TAction
      Category = 'Tools'
      Caption = 'Preferences...'
      ShortCut = 113
      OnExecute = acPreferencesExecute
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 8
      ShortCut = 16474
    end
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 9
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 10
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 11
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object acImportSurfer: TAction
      Category = 'Surfer'
      Caption = 'Import'
      ImageIndex = 0
      OnExecute = acImportSurferExecute
      OnUpdate = acImportSurferUpdate
    end
    object acCenterOnPoints: TAction
      Category = 'View'
      Caption = 'Center View on Points'
      OnExecute = acCenterOnPointsExecute
    end
    object acSnapShot: TAction
      Category = 'File'
      Caption = 'Snapshot...'
      OnExecute = acSnapShotExecute
    end
    object acRenderBitmap: TAction
      Category = 'File'
      Caption = 'Render to Bitmap...'
      OnExecute = acRenderBitmapExecute
    end
    object acRenderBitmap2: TAction
      Category = 'File'
      Caption = 'Render to Bitmap (x2)...'
      OnExecute = acRenderBitmap2Execute
    end
    object acRenderBitmap300: TAction
      Category = 'File'
      Caption = 'Render to Bitmap (300 dpi)...'
      OnExecute = acRenderBitmap300Execute
    end
    object acRenderBitmap600: TAction
      Category = 'File'
      Caption = 'Render to Bitmap (600 dpi)...'
      OnExecute = acRenderBitmap600Execute
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = acExitExecute
    end
    object acProcess: TAction
      Category = 'Original'
      Caption = 'Process'
      ImageIndex = 2
      OnExecute = acProcessExecute
      OnUpdate = acProcessUpdate
    end
    object acEmptyProcessed: TAction
      Category = 'Processed'
      Caption = 'Empty'
      Hint = 'Empty the processed table'
      ImageIndex = 3
      OnExecute = acEmptyProcessedExecute
      OnUpdate = acEmptyProcessedUpdate
    end
    object acFocusProcessed: TAction
      Category = 'Processed'
      Caption = 'Focus'
      Hint = 'Focus on selected record'
      ImageIndex = 17
      OnExecute = acFocusProcessedExecute
      OnUpdate = acFocusProcessedUpdate
    end
    object acSortProcessed: TAction
      Category = 'Processed'
      Caption = 'Sort'
      Hint = 'Sort the processed table'
      ImageIndex = 13
      OnExecute = acSortProcessedExecute
      OnUpdate = acSortProcessedUpdate
    end
    object acExport: TAction
      Category = 'Processed'
      Caption = 'Export'
      ImageIndex = 0
      OnExecute = acExportExecute
      OnHint = acExportHint
      OnUpdate = acExportUpdate
    end
    object acSortOriginal: TAction
      Category = 'Original'
      Caption = 'Sort'
      Hint = 'Sort original table'
      ImageIndex = 13
      OnExecute = acSortOriginalExecute
      OnUpdate = acSortOriginalUpdate
    end
    object acOpenVTK: TAction
      Category = 'Grids'
      Caption = 'Open VTK Grid'
      OnExecute = acOpenVTKExecute
    end
    object acImportTSV: TAction
      Category = 'Original'
      Caption = 'Tab Separated Value File'
      ImageIndex = 1
      OnExecute = acImportTSVExecute
      OnUpdate = acImportTSVUpdate
    end
    object acImportSSV: TAction
      Category = 'Original'
      Caption = 'Space Separated Value File'
      ImageIndex = 1
      OnExecute = acImportSSVExecute
      OnUpdate = acImportSSVUpdate
    end
    object acImportFixedFormat: TAction
      Category = 'Original'
      Caption = 'Fixed Format (Space Separated) Value File'
      ImageIndex = 1
      OnExecute = acImportFixedFormatExecute
      OnUpdate = acImportFixedFormatUpdate
    end
    object acExportProcessedCSV: TAction
      Category = 'Processed'
      Caption = 'Comma Separated Value File'
      ImageIndex = 1
      OnExecute = acExportProcessedCSVExecute
      OnUpdate = acExportProcessedCSVUpdate
    end
    object acExportProcessedTSV: TAction
      Category = 'Processed'
      Caption = 'Tab Separated Value File'
      ImageIndex = 1
      OnExecute = acExportProcessedTSVExecute
      OnUpdate = acExportProcessedTSVUpdate
    end
    object acExportProcessedSSV: TAction
      Category = 'Processed'
      Caption = 'Space Separated Value File'
      ImageIndex = 1
      OnExecute = acExportProcessedSSVExecute
      OnUpdate = acExportProcessedSSVUpdate
    end
    object acExportProcessedLaTeX: TAction
      Category = 'Processed'
      Caption = 'LaTeX File Segment'
      ImageIndex = 1
      OnExecute = acExportProcessedLaTeXExecute
      OnUpdate = acExportProcessedLaTeXUpdate
    end
    object acExportProcessedXML: TAction
      Category = 'Processed'
      Caption = 'XML File'
      ImageIndex = 1
      OnExecute = acExportProcessedXMLExecute
      OnUpdate = acExportProcessedXMLUpdate
    end
    object acExportProcessedClipboard: TAction
      Category = 'Processed'
      Caption = 'Clipboard'
      ImageIndex = 0
      OnExecute = acExportProcessedClipboardExecute
      OnUpdate = acExportProcessedClipboardUpdate
    end
    object acHelp: TAction
      Category = 'Help'
      Caption = 'Help'
      ImageIndex = 14
      ShortCut = 112
      OnExecute = acHelpExecute
      OnUpdate = acHelpUpdate
    end
    object acTipOfDay: TAction
      Category = 'Help'
      Caption = 'Tip of the Day...'
      ImageIndex = 14
      OnExecute = acTipOfDayExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About glData...'
      ImageIndex = 15
      OnExecute = acAboutExecute
    end
    object acSurferAnimation: TAction
      Category = 'Surfer'
      Caption = 'Animate'
      ImageIndex = 16
      OnExecute = acSurferAnimationExecute
      OnUpdate = acSurferAnimationUpdate
    end
    object acSurferDelete: TAction
      Category = 'Surfer'
      Caption = 'Delete'
      ImageIndex = 3
      OnExecute = acSurferDeleteExecute
      OnUpdate = acSurferDeleteUpdate
    end
    object acFocusSurfer: TAction
      Category = 'Surfer'
      Caption = 'Focus'
      ImageIndex = 17
      OnExecute = acFocusSurferExecute
      OnUpdate = acFocusSurferUpdate
    end
    object acSurferDeleteAll: TAction
      Category = 'Surfer'
      Caption = 'Delete All'
      ImageIndex = 3
      OnExecute = acSurferDeleteAllExecute
      OnUpdate = acSurferDeleteAllUpdate
    end
    object acImportArcInfo: TAction
      Category = 'ArcInfo'
      Caption = 'Import'
      ImageIndex = 0
      OnExecute = acImportArcInfoExecute
    end
    object acFocusOnArcInfoGrid: TAction
      Category = 'ArcInfo'
      Caption = 'Focus'
      ImageIndex = 17
      OnExecute = acFocusOnArcInfoGridExecute
      OnUpdate = acFocusOnArcInfoGridUpdate
    end
    object acSaveGridMeta: TAction
      Category = 'File'
      Caption = 'Save Grid MetaFile...'
      OnExecute = acSaveGridMetaExecute
      OnUpdate = acSaveGridMetaUpdate
    end
    object acLoadGridMeta: TAction
      Category = 'File'
      Caption = 'Load Grid Metafile...'
      OnExecute = acLoadGridMetaExecute
    end
    object acSurferDefault: TAction
      Category = 'Surfer'
      Caption = 'Default'
      ImageIndex = 8
      OnExecute = acSurferDefaultExecute
      OnUpdate = acSurferDefaultUpdate
    end
    object acSurferDefaultAll: TAction
      Category = 'Surfer'
      Caption = 'Default All'
      ImageIndex = 8
      OnExecute = acSurferDefaultAllExecute
      OnUpdate = acSurferDefaultAllUpdate
    end
    object acArcInfoDeleteAll: TAction
      Category = 'ArcInfo'
      Caption = 'Delete All'
      ImageIndex = 3
      OnExecute = acArcInfoDeleteAllExecute
      OnUpdate = acArcInfoDeleteAllUpdate
    end
    object acArcInfoDelete: TAction
      Category = 'ArcInfo'
      Caption = 'Delete'
      ImageIndex = 3
      OnExecute = acArcInfoDeleteExecute
      OnUpdate = acArcInfoDeleteUpdate
    end
    object acArcInfoDefault: TAction
      Category = 'ArcInfo'
      Caption = 'Default'
      ImageIndex = 8
      OnExecute = acArcInfoDefaultExecute
      OnUpdate = acArcInfoDefaultUpdate
    end
    object acArcInfoDefaultAll: TAction
      Category = 'ArcInfo'
      Caption = 'Default All'
      ImageIndex = 8
      OnExecute = acArcInfoDefaultAllExecute
      OnUpdate = acArcInfoDefaultAllUpdate
    end
    object acArcInfoAnimation: TAction
      Category = 'ArcInfo'
      Caption = 'Animate'
      Enabled = False
      ImageIndex = 16
      OnExecute = acArcInfoAnimationExecute
      OnUpdate = acArcInfoAnimationUpdate
    end
    object acGeothermalGrid: TAction
      Category = 'Geothermal'
      Caption = 'TOUGH'
      ImageIndex = 0
      OnExecute = acGeothermalGridExecute
    end
    object acSortGeoLayers: TAction
      Category = 'Geothermal'
      Caption = 'Sort'
      ImageIndex = 13
      OnExecute = acSortGeoLayersExecute
      OnUpdate = acSortGeoLayersUpdate
    end
    object acColourTool: TAction
      Category = 'Tools'
      Caption = 'Colour Tool'
      OnExecute = acColourToolExecute
    end
    object acBenchmark: TAction
      Category = 'Tools'
      Caption = 'Benchmark'
      OnExecute = acBenchmarkExecute
      OnUpdate = acBenchmarkUpdate
    end
    object acOpenGLContext: TAction
      Category = 'Help'
      Caption = 'OpenGL Information...'
      OnExecute = acOpenGLContextExecute
    end
    object acBoundingBox: TAction
      Category = 'Processed'
      AutoCheck = True
      Caption = 'Scalar'
      ImageIndex = 7
      OnExecute = acBoundingBoxExecute
    end
    object acBoundingBoxVector: TAction
      Category = 'Processed'
      AutoCheck = True
      Caption = 'Vector'
      ImageIndex = 7
      OnExecute = acBoundingBoxVectorExecute
    end
    object acLocator: TAction
      Category = 'Tools'
      Caption = 'Locator'
      OnExecute = acLocatorExecute
      OnUpdate = acLocatorUpdate
    end
    object acAxes: TAction
      Category = 'Tools'
      Caption = 'Axes'
      OnExecute = acAxesExecute
      OnUpdate = acAxesUpdate
    end
    object acWorldView: TAction
      Category = 'Tools'
      Caption = 'World View'
      OnExecute = acWorldViewExecute
      OnUpdate = acWorldViewUpdate
    end
    object acGeothermalTETRAD: TAction
      Category = 'Geothermal'
      Caption = 'TETRAD'
      ImageIndex = 0
      OnExecute = acGeothermalTETRADExecute
    end
  end
  object OpenVTK: TOpenDialog
    DefaultExt = 'vtk'
    Filter = 'Visualisation Toolkit|*.vtk|All Files|*.*'
    Title = 'Open a VTK File'
    Left = 444
    Top = 159
  end
  object OpenArcInfo: TOpenDialog
    DefaultExt = 'grd'
    Filter = 'ESRI ArcInfo Grid File|*.grd|All Files|*.*'
    Title = 'Open an ESRI ArcInfo Grid File'
    Left = 445
    Top = 231
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Left = 518
    Top = 83
  end
  object dsSurferGridInfo: TDataSource
    Left = 577
    Top = 381
  end
  object TimerAnimationSurfer: TTimer
    Enabled = False
    OnTimer = TimerAnimationSurferTimer
    Left = 703
    Top = 149
  end
  object dsArcInfoGridInfo: TDataSource
    Left = 451
    Top = 381
  end
  object SaveDialogGridMeta: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files|*.xml|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Metafile'
    Left = 689
    Top = 228
  end
  object OpenDialogGridMeta: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML Files|*.xml|All Files|*.*'
    Title = 'Save Metafile'
    Left = 571
    Top = 228
  end
  object dsGeothermalLayers: TDataSource
    Left = 683
    Top = 433
  end
  object AxesFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 702
    Top = 87
  end
  object GLAVIRecorder: TGLAVIRecorder
    Width = 320
    Height = 200
    Left = 615
    Top = 87
  end
end
