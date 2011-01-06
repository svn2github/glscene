object MaterialEditorForm: TMaterialEditorForm
  Left = 309
  Top = 140
  AlphaBlendValue = 192
  BiDiMode = bdLeftToRight
  BorderStyle = bsSizeToolWin
  Caption = 'Material Editor'
  ClientHeight = 554
  ClientWidth = 640
  Color = clBtnFace
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  ParentBiDiMode = False
  PopupMenu = PopupMenu1
  Position = poDesigned
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDragDrop = FormDragDrop
  OnDragOver = FormDragOver
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseLeave = FormMouseLeave
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 24
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object CloseItem: TMenuItem
        Caption = 'Close'
        OnClick = CloseItemClick
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object NodePaletteMenuItem: TMenuItem
        Caption = 'Node Palette'
        Checked = True
        OnClick = ACViewNodePalleteExecute
      end
      object MaterialPreviewMenuItem: TMenuItem
        Caption = 'Material Preview'
        Checked = True
        OnClick = ACViewMaterialPreviewExecute
      end
      object MaterialCodeMenuItem: TMenuItem
        Caption = 'Material GLSL Code'
        Checked = True
        OnClick = ACViewMaterialCodeExecute
      end
      object RealTimePreviewMenuItem: TMenuItem
        Caption = 'Real Time Expression Preview'
        Checked = True
      end
      object TransparensyMenuItem: TMenuItem
        Caption = 'Transparensy'
        OnClick = TransparensyMenuItemClick
      end
      object RefreshAllMenuItem: TMenuItem
        Caption = 'Refresh All'
        OnClick = ACRefreshAllExecute
      end
    end
    object Model1: TMenuItem
      Caption = 'Model'
      object PlaneMenuItem: TMenuItem
        AutoCheck = True
        Caption = 'Plane'
        GroupIndex = 1
        RadioItem = True
        OnClick = ModelMenuItemClick
      end
      object CubeMenuItem: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Cube'
        GroupIndex = 1
        RadioItem = True
        OnClick = ModelMenuItemClick
      end
      object SphereMenuItem: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Sphere'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = ModelMenuItemClick
      end
      object FreeformMenuItem: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'Free form'
        GroupIndex = 1
        RadioItem = True
        OnClick = ModelMenuItemClick
      end
    end
  end
  object PopupMenu1: TPopupMenu
    AutoPopup = False
    OnPopup = PopupMenu1Popup
    Left = 104
    Top = 32
    object DeleteSelected: TMenuItem
      Caption = 'Delete Selected'
      OnClick = DeleteSelectedClick
    end
    object BreakLink: TMenuItem
      Caption = 'Break Link'
      OnClick = BreakLinkClick
    end
    object BreakAllLinks: TMenuItem
      Caption = 'Break All Links'
      OnClick = BreakAllLinksClick
    end
    object Cancel1: TMenuItem
      Caption = 'Cancel'
    end
  end
  object Cadencer: TTimer
    Enabled = False
    Interval = 25
    OnTimer = Cadence
    Left = 104
    Top = 88
  end
end
