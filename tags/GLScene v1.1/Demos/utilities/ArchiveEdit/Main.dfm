object Form1: TForm1
  Left = 268
  Top = 248
  Width = 555
  Height = 299
  Caption = 'GLScene Pak Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    539
    241)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 545
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object TreeView: TTreeView
    Left = 8
    Top = 8
    Width = 161
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    Images = ImageList1
    Indent = 19
    ShowRoot = False
    TabOrder = 0
    ToolTips = False
    OnChange = TreeViewChange
    OnCollapsing = TreeViewCollapsing
    OnKeyDown = TreeViewKeyDown
  end
  object ListView: TListView
    Left = 176
    Top = 8
    Width = 365
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Size'
        Width = 70
      end>
    Enabled = False
    Items.Data = {
      370000000100000000000000FFFFFFFFFFFFFFFF00000000000000001A4F7065
      6E206F72206372656174652070616B2066696C652E2E2E}
    MultiSelect = True
    SmallImages = ImageList1
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnClick = ListViewClick
    OnDblClick = ListViewDblClick
    OnKeyDown = ListViewKeyDown
  end
  object MainMenu1: TMainMenu
    Left = 184
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New...'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Createfolder1: TMenuItem
        Caption = 'Create folder...'
        OnClick = Createfolder1Click
      end
      object Addfiles1: TMenuItem
        Caption = 'Add file(s)...'
        OnClick = Addfiles1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Compression1: TMenuItem
        Caption = 'Compression'
        object None1: TMenuItem
          Caption = 'None'
          Checked = True
          GroupIndex = 2
          OnClick = None1Click
        end
        object Fast1: TMenuItem
          Tag = 1
          Caption = 'Fast'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Default1: TMenuItem
          Tag = 2
          Caption = 'Default'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Max1: TMenuItem
          Tag = 3
          Caption = 'Max'
          GroupIndex = 2
          OnClick = None1Click
        end
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Deleteselectedfile1: TMenuItem
        Caption = 'Delete selected file(s)'
        GroupIndex = 2
        OnClick = Deleteselectedfile1Click
      end
      object Deleteselectedfolder1: TMenuItem
        Caption = 'Delete selected folder'
        GroupIndex = 2
        OnClick = Deleteselectedfolder1Click
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Extractselectedfiles1: TMenuItem
        Caption = 'Extract selected file(s)'
        GroupIndex = 2
        OnClick = Extractselectedfiles1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 184
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'glp'
    Filter = 'All (Pak, Zlib)|*.pak;*.zlib|PAK files|*.pak|ZLib Files|*.zlib'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 216
    Top = 64
  end
  object ImageList1: TImageList
    Left = 216
    Top = 32
  end
end
