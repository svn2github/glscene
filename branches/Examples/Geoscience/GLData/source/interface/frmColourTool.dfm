object formColourEdit: TformColourEdit
  Left = 420
  Top = 435
  Width = 648
  Height = 342
  Caption = 'Colour Tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ActionToolBarColourEdit: TActionToolBar
    Left = 0
    Top = 0
    Width = 640
    Height = 26
    ActionManager = ActionManager
    Caption = 'ActionToolBarColourEdit'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 26
    Width = 640
    Height = 289
    ActivePage = tsHex
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
    object tsHex: TTabSheet
      Caption = 'Hexidecimal'
      ImageIndex = 1
      object dbgHex: TDBGrid
        Left = 0
        Top = 0
        Width = 632
        Height = 261
        Align = alClient
        DataSource = dsHex
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDrawColumnCell = dbgHexDrawColumnCell
        OnDblClick = dbgHexDblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'Identifier'
            Title.Alignment = taCenter
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Hex'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end>
      end
    end
    object tsRGB: TTabSheet
      Caption = 'RGBA'
      object dbgRGBA: TDBGrid
        Left = 0
        Top = 0
        Width = 632
        Height = 261
        Align = alClient
        DataSource = dsRGBA
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDrawColumnCell = dbgRGBADrawColumnCell
        OnDblClick = dbgRGBADblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'Identifier'
            Title.Alignment = taCenter
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Red'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Green'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Blue'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Alpha'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Hex'
            Title.Alignment = taCenter
            Width = 75
            Visible = True
          end>
      end
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
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
                Action = acImportFixed
                Caption = '&Fixed Format (Space Separated) Value File'
                ImageIndex = 1
              end
              item
                Action = acImportClipboard
                Caption = 'C&lipboard'
                ImageIndex = 0
              end>
            Action = acImport
            Caption = '&Import'
            ImageIndex = 0
          end
          item
            Items = <
              item
                Action = acExportCSV
                Caption = '&Comma Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportSSV
                Caption = '&Space Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportTSV
                Caption = '&Tab Separated Value File'
                ImageIndex = 1
              end
              item
                Action = acExportLatex
                Caption = '&LaTeX File Segment'
                ImageIndex = 1
              end
              item
                Action = acExportXML
                Caption = '&XML File'
                ImageIndex = 1
              end
              item
                Action = acExportClipboard
                Caption = 'Cl&ipboard'
                ImageIndex = 0
              end>
            Action = acExport
            Caption = '&Export'
            ImageIndex = 0
          end
          item
            Caption = '-'
          end
          item
            Action = acDone
            Caption = '&Done'
            ImageIndex = 2
          end>
        ActionBar = ActionToolBarColourEdit
      end>
    Images = ImageList
    Left = 445
    Top = 147
    StyleName = 'XP Style'
    object acImport: TAction
      Caption = 'Import'
      ImageIndex = 0
      OnExecute = acImportExecute
    end
    object acExport: TAction
      Category = 'Export'
      Caption = 'Export'
      ImageIndex = 0
      OnExecute = acExportExecute
      OnUpdate = acExportUpdate
    end
    object acImportCSV: TAction
      Caption = 'Comma Separated Value File'
      ImageIndex = 1
      OnExecute = acImportCSVExecute
      OnUpdate = acImportCSVUpdate
    end
    object acImportTSV: TAction
      Caption = 'Tab Separated Value File'
      ImageIndex = 1
      OnExecute = acImportTSVExecute
      OnUpdate = acImportTSVUpdate
    end
    object acImportSSV: TAction
      Caption = 'Space Separated Value File'
      ImageIndex = 1
      OnExecute = acImportSSVExecute
      OnUpdate = acImportSSVUpdate
    end
    object acImportFixed: TAction
      Caption = 'Fixed Format (Space Separated) Value File'
      ImageIndex = 1
      OnExecute = acImportFixedExecute
      OnUpdate = acImportFixedUpdate
    end
    object acImportClipboard: TAction
      Caption = 'Clipboard'
      ImageIndex = 0
      OnExecute = acImportClipboardExecute
      OnUpdate = acImportClipboardUpdate
    end
    object acExportCSV: TAction
      Category = 'Export'
      Caption = 'Comma Separated Value File'
      ImageIndex = 1
      OnExecute = acExportCSVExecute
      OnUpdate = acExportCSVUpdate
    end
    object acExportSSV: TAction
      Category = 'Export'
      Caption = 'Space Separated Value File'
      ImageIndex = 1
      OnExecute = acExportSSVExecute
      OnUpdate = acExportSSVUpdate
    end
    object acExportTSV: TAction
      Category = 'Export'
      Caption = 'Tab Separated Value File'
      ImageIndex = 1
      OnExecute = acExportTSVExecute
      OnUpdate = acExportTSVUpdate
    end
    object acExportLatex: TAction
      Category = 'Export'
      Caption = 'LaTeX File Segment'
      ImageIndex = 1
      OnExecute = acExportLatexExecute
      OnUpdate = acExportLatexUpdate
    end
    object acExportXML: TAction
      Category = 'Export'
      Caption = 'XML File'
      ImageIndex = 1
      OnExecute = acExportXMLExecute
      OnUpdate = acExportXMLUpdate
    end
    object acExportClipboard: TAction
      Category = 'Export'
      Caption = 'Clipboard'
      ImageIndex = 0
      OnExecute = acExportClipboardExecute
      OnUpdate = acExportClipboardUpdate
    end
    object acDone: TAction
      Caption = 'Done'
      ImageIndex = 2
      OnExecute = acDoneExecute
    end
  end
  object geImportFile: TGEImportFile
    DefaultFileType = ifCSV
    Destination = kbmHex
    GroupDelimiters = False
    ImportFileType = ifCSV
    RegPath = '\Software\glData\ColourTool\'
    Left = 326
    Top = 146
  end
  object geExportFile: TGEExportFile
    DefaultFileType = efCSV
    ExportFileType = efCSV
    RegPath = '\Software\glData\ColourTool\'
    Precision = 0
    SubRegPath = 'Export'
    Left = 355
    Top = 146
  end
  object dsRGBA: TDataSource
    DataSet = kbmRGBA
    Left = 385
    Top = 146
  end
  object dsHex: TDataSource
    DataSet = kbmHex
    Left = 415
    Top = 146
  end
  object kbmRGBA: TkbmMemTable
    Active = True
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'Identifier'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'Red'
        DataType = ftFloat
      end
      item
        Name = 'Green'
        DataType = ftFloat
      end
      item
        Name = 'Blue'
        DataType = ftFloat
      end
      item
        Name = 'Alpha'
        DataType = ftFloat
      end
      item
        Name = 'Hex'
        DataType = ftString
        Size = 6
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '4.02'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    BeforePost = kbmRGBABeforePost
    Left = 326
    Top = 176
  end
  object kbmHex: TkbmMemTable
    Active = True
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'Identifier'
        DataType = ftString
        Size = 32
      end
      item
        Name = 'Hex'
        DataType = ftString
        Size = 6
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '4.02'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 355
    Top = 175
  end
  object ImageList: TImageList
    Left = 324
    Top = 106
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
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
      00000000000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000315200E7E7E70039393900001852000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00C6C6
      C60000FFFF00C6C6C60000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000315200B5B5B500E7E7E700CECECE00525252000018
      5200000000000000000000000000000000000000000000FFFF00C6C6C60000FF
      FF00C6C6C6000084840000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B5C6FF0021212100E7E7E700E7E7E70063636300525252003131
      31000018520000000000000000000000000000000000FFFFFF00000000000000
      0000000000000084840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000217300F7F7F700B5B5B500E7E7E7006BC6FF00CECECE00B5B5
      B500ADADAD00001852000000000000000000000000000000000000FFFF00C6C6
      C60000FFFF000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF0000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CECE
      CE00004A7300F7F7F70052525200E7E7E700E7E7E700CECECE00ADADAD00C6C6
      C600B5B5B500A5A5A50000185200000000000000000000FFFF00C6C6C60000FF
      FF00C6C6C60000FFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF0000000000000000008080800000FF000000FF
      0000000000000000000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009C9C
      9C00B5B5B500DEDEDE00B5B5B500B5B5B500E7E7E700B5B5B50000315200C6C6
      C600B5B5B500003152006B6B6B0000000000000000000000000000FFFF00C6C6
      C60000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000FF0000000000000000
      000000000000000000000000000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000031
      5200FFFFFF00C6C6C600E7E7E700CECECE00ADADAD00848484008CD6FF00C6C6
      C6009494940021212100000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F7F700004A7300FFFF
      FF00E7E7E700F7F7F700C6C6C600DEDEDE00B5B5B500CECECE00CECECE005252
      52000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007BBD00004A7300FFFF
      FF0052525200F7F7F700F7F7F700E7E7E700E7E7E700DEDEDE00C6C6C6000063
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000031313100004A
      73003131310021212100B5B5B50094949400E7E7E700CECECE00001852004ABD
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008080800000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006B6B6B00525252003131310021212100E7E7E70000185200000000000000
      00002121210000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      800000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7F7F700A5A5A50031313100003152000031520000000000000000000000
      0000003194000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
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
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FF7FC7FFFFFF0000
      FE3F0000F9FF0000FC1F0000F0FF0000FC0F0000F0FF0000F8070000E07F0000
      F0030000C07F0000E0010000843F0000E00100001E3F0000C0030000FE1F0000
      80070000FF1F0000800F0000FF8F0000C00F0000FFC70000F0370000FFE30000
      F0770000FFF80000FFF7FFFFFFFF0000}
  end
  object ColorDialog: TColorDialog
    Left = 236
    Top = 152
  end
end
