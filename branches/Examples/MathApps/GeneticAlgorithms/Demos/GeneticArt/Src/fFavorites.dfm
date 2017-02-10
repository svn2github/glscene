object frmFavorites: TfrmFavorites
  Left = 375
  Top = 129
  Caption = 'Favorites'
  ClientHeight = 421
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    534
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 176
    Width = 51
    Height = 13
    Caption = 'Collections'
  end
  object Label2: TLabel
    Left = 240
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Images'
  end
  object Shape1: TShape
    Left = 8
    Top = 24
    Width = 225
    Height = 145
    Brush.Color = clBlack
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object lvCollections: TListView
    Left = 8
    Top = 192
    Width = 225
    Height = 186
    Anchors = [akLeft, akTop, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end>
    DragMode = dmAutomatic
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = PopupMenu_Collections
    ShowHint = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnDragDrop = lvCollectionsDragDrop
    OnDragOver = lvCollectionsDragOver
    OnSelectItem = lvCollectionsSelectItem
    ExplicitHeight = 225
  end
  object lvImages: TListView
    Left = 240
    Top = 24
    Width = 274
    Height = 330
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Coords'
      end
      item
        Caption = 'P. Noise'
        Width = 60
      end>
    DragMode = dmAutomatic
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = PopupMenu_Image
    ShowHint = True
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = lvImagesSelectItem
    ExplicitWidth = 290
    ExplicitHeight = 369
  end
  object Image: TImage32
    Left = 16
    Top = 32
    Width = 210
    Height = 128
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    PopupMenu = PopupMenu_Image
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 2
  end
  object Button_EvolveImage: TButton
    Left = 240
    Top = 358
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '&Evolve Image'
    TabOrder = 3
    OnClick = Button_EvolveImageClick
    ExplicitTop = 397
  end
  object PopupMenu_Image: TPopupMenu
    Left = 80
    Top = 72
    object RenameImage1: TMenuItem
      Caption = '&Rename Image'
      OnClick = RenameImage1Click
    end
    object DeleteImage1: TMenuItem
      Caption = '&Delete Image'
      OnClick = DeleteImage1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object EvolveImage1: TMenuItem
      Caption = '&Evolve Image'
      OnClick = EvolveImage1Click
    end
    object SaveImage1: TMenuItem
      Caption = '&Save Image'
      OnClick = SaveImage1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ZoomImage1: TMenuItem
      Caption = '&Zoom Image'
      OnClick = ZoomImage1Click
    end
    object KensGodMode1: TMenuItem
      Caption = 'Ken'#39's &God Mode'
      OnClick = KensGodMode1Click
    end
  end
  object PopupMenu_Collections: TPopupMenu
    Left = 80
    Top = 232
    object Addcollection1: TMenuItem
      Caption = '&Add collection'
      OnClick = Addcollection1Click
    end
    object DeleteCollection1: TMenuItem
      Caption = '&Delete Collection'
      OnClick = DeleteCollection1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object RenameCollection1: TMenuItem
      Caption = '&Rename Collection'
      OnClick = RenameCollection1Click
    end
  end
end
