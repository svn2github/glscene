unit DelphiImageList;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TDataModule1 = class(TDataModule)
    ImageList1: TImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

end.
(*
inherited AppBuilder: TAppBuilder
  Left = 336
  Top = 465
  HorzScrollBar.Increment = 48
  VertScrollBar.Increment = 6
  AutoScroll = False
  Caption = ' '
  ClientHeight = 117
  ClientWidth = 786
  Menu = MainMenu1
  Scaled = False
  OnActivate = FormActivate
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCloseQuery = WindowCloseQuery
  OnConstrainedResize = FormConstrainedResize
  OnMouseWheel = FormMouseWheel
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 633
    Height = 57
    BevelEdges = []
    BevelInner = bvNone
    PopupMenu = ToolbarsPopup
    TabOrder = 0
    OnBandDrag = ControlBar1BandDrag
    OnBandInfo = ControlBar1BandInfo
    OnBandMove = ControlBar1BandMove
    OnBandPaint = ControlBar1BandPaint
    OnDockOver = ControlBar1DockOver
    OnGetSiteInfo = ControlBar1GetSiteInfo
    object StandardToolBar: TDockToolBar
      Left = 87
      Top = 2
      Width = 194
      Height = 22
      HelpContext = 6100
      Caption = 'Standard'
      Constraints.MinHeight = 22
      Constraints.MinWidth = 23
      DockSite = True
      Images = ImageList1
      ParentShowHint = False
      PopupMenu = ToolbarsPopup
      ShowHint = True
      TabOrder = 0
      OnEndDock = ToolBarEndDock
      OnGetSiteInfo = ToolBarGetSiteInfo
      OnStartDock = ToolBarStartDock
      object ToolButton21: TToolButton
        Left = 0
        Top = 0
        Action = FileNewCommand
        OnEndDock = ToolButtonEndDock
      end
      object CommandButton2: TToolButton
        Left = 23
        Top = 0
        Action = FileOpenCommand
        DropdownMenu = ClosedFilesPopup
        Style = tbsDropDown
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton4: TToolButton
        Left = 59
        Top = 0
        Action = FileSaveCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton8: TToolButton
        Left = 82
        Top = 0
        Width = 8
        Caption = 'ToolButton8'
        ImageIndex = 20
        Style = tbsSeparator
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton2: TToolButton
        Left = 90
        Top = 0
        Action = FileSaveAllCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton1: TToolButton
        Left = 113
        Top = 0
        Action = FileOpenProjectCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton7: TToolButton
        Left = 136
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 0
        Style = tbsSeparator
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton5: TToolButton
        Left = 144
        Top = 0
        Action = ProjectAddCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton6: TToolButton
        Left = 167
        Top = 0
        Action = ProjectRemoveCommand
        OnEndDock = ToolButtonEndDock
      end
    end
    object ViewToolBar: TDockToolBar
      Left = 11
      Top = 28
      Width = 100
      Height = 22
      HelpContext = 6101
      Caption = 'View'
      Constraints.MinHeight = 22
      Constraints.MinWidth = 23
      DockSite = True
      Images = ImageList1
      ParentShowHint = False
      PopupMenu = ToolbarsPopup
      ShowHint = True
      TabOrder = 1
      OnEndDock = ToolBarEndDock
      OnGetSiteInfo = ToolBarGetSiteInfo
      OnStartDock = ToolBarStartDock
      object ToolButton9: TToolButton
        Left = 0
        Top = 0
        Action = ViewUnitCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton10: TToolButton
        Left = 23
        Top = 0
        Action = ViewFormCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton11: TToolButton
        Left = 46
        Top = 0
        Action = ViewToggleFormCommand
        OnEndDock = ToolButtonEndDock
      end
    end
    object PaletteBar: TDockPanel
      Left = 330
      Top = 2
      Width = 64
      Height = 48
      Caption = 'Component Palette'
      DragMode = dmManual
      BevelEdges = [beBottom]
      BevelOuter = bvNone
      PopupMenu = PaletteMenu
      Visible = False
      OnEndDock = PaletteBarEndDock
      object TabControl: TComponentPaletteTabControl
        Left = 0
        Top = 0
        Width = 64
        Height = 47
        Align = alClient
        Constraints.MinWidth = 20
        HotTrack = True
        PopupMenu = PaletteMenu
        TabOrder = 0
        TabStop = False
        OnChange = TabControlChange
        OnDragDrop = TabControlDragDrop
        OnDragOver = TabControlDragOver
        OnEndDrag = TabControlEndDrag
        OnMouseDown = TabControlMouseDown
        OnMouseMove = TabControlMouseMove
        OnStartDrag = TabControlStartDrag
        BorderStyle = bsNone
        OnHelpRequest = ComponentPaletteHelpRequest
        object PageScroller1: TPageScroller
          Left = 32
          Top = 6
          Width = 31
          Height = 39
          Align = alClient
          AutoScroll = True
          TabOrder = 0
          OnScroll = PageScroller1Scroll
        end
        object Panel2: TPanel
          Left = 4
          Top = 6
          Width = 28
          Height = 39
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
          object SelectorButton: TSpeedButton
            Left = 0
            Top = 0
            Width = 28
            Height = 28
            GroupIndex = 1
            Down = True
            Flat = True
          end
        end
      end
    end
    object MenuBar: TActionMainMenuBar
      Left = 11
      Top = 2
      Width = 63
      Height = 22
      ActionManager = ActionList1
      AnimationStyle = asNone
      Caption = 'Menu bar'
      Color = clBtnFace
      ColorMap.FontColor = clWindowText
      ColorMap.HighlightColor = clBlack
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 15660791
      DragKind = dkDock
      EdgeBorders = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMenuText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = True
      ParentColor = False
      ParentFont = False
      PersistentHotKeys = False
      Spacing = 4
      VertMargin = 0
      OnExitMenuLoop = MenuBarExitMenuLoop
      OnPopup = MenuBarPopup
    end
    object CustomToolBar: TDockToolBar
      Left = 294
      Top = 2
      Width = 23
      Height = 22
      HelpContext = 6103
      Caption = 'Custom'
      Constraints.MinHeight = 22
      Constraints.MinWidth = 23
      DockSite = True
      Images = ImageList1
      ParentShowHint = False
      PopupMenu = ToolbarsPopup
      ShowHint = True
      TabOrder = 4
      OnEndDock = ToolBarEndDock
      OnGetSiteInfo = ToolBarGetSiteInfo
      OnStartDock = ToolBarStartDock
      object ToolButton16: TToolButton
        Left = 0
        Top = 0
        Action = HelpContentsCommand
        ParentShowHint = False
        ShowHint = True
        OnEndDock = ToolButtonEndDock
      end
    end
    object DebugToolBar: TDockToolBar
      Left = 124
      Top = 28
      Width = 114
      Height = 22
      HelpContext = 6102
      Caption = 'Debug'
      Constraints.MinHeight = 22
      Constraints.MinWidth = 23
      DockSite = True
      Images = ImageList1
      ParentShowHint = False
      PopupMenu = ToolbarsPopup
      ShowHint = True
      TabOrder = 5
      OnEndDock = ToolBarEndDock
      OnGetSiteInfo = ToolBarGetSiteInfo
      OnStartDock = ToolBarStartDock
      object CommandButton1: TToolButton
        Left = 0
        Top = 0
        Action = RunRunCommand
        DropdownMenu = ProcessMenu
        ParentShowHint = False
        ShowHint = True
        Style = tbsDropDown
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton3: TToolButton
        Left = 36
        Top = 0
        Action = RunPauseCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton14: TToolButton
        Left = 59
        Top = 0
        Width = 8
        Caption = 'ToolButton14'
        ImageIndex = 28
        Style = tbsSeparator
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton13: TToolButton
        Left = 67
        Top = 0
        Action = RunTraceIntoCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton15: TToolButton
        Left = 90
        Top = 0
        Action = RunStepOverCommand
        OnEndDock = ToolButtonEndDock
      end
    end
    object DesktopToolBar: TDockToolBar
      Left = 407
      Top = 2
      Width = 187
      Height = 22
      HelpContext = 6104
      Caption = 'Desktop'
      DockSite = True
      Images = ImageList1
      ParentShowHint = False
      PopupMenu = ToolbarsPopup
      ShowHint = True
      TabOrder = 6
      OnEndDock = ToolBarEndDock
      OnGetSiteInfo = ToolBarGetSiteInfo
      OnStartDock = ToolBarStartDock
      object cbDesktop: TDesktopComboBox
        Left = 0
        Top = 0
        Width = 129
        Height = 21
        Hint = 'Desktop speedsetting'
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = cbDesktopClick
        OnDropDown = cbDesktopDropDown
        OnKeyPress = cbDesktopKeyPress
      end
      object ToolButton17: TToolButton
        Left = 129
        Top = 0
        Width = 11
        Caption = 'ToolButton17'
        Style = tbsSeparator
      end
      object ToolButton18: TToolButton
        Left = 140
        Top = 0
        Action = ViewSaveDesktopCommand
        OnEndDock = ToolButtonEndDock
      end
      object ToolButton19: TToolButton
        Left = 163
        Top = 0
        Hint = 'Set runtime desktop'
        Action = ViewRuntimeDesktopCommand
        ParentShowHint = False
        ShowHint = True
        OnEndDock = ToolButtonEndDock
      end
    end
  end
  object Panel1: TPanel
    Left = 360
    Top = 88
    Width = 57
    Height = 25
    TabOrder = 1
    Visible = False
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    OnChange = MainMenu1Change
    Left = 101
    Top = 87
    object FileMenu: TMenuItem
      Caption = '&File'
      HelpContext = 1000
      object New1: TMenuItem
        Caption = '&New'
        HelpContext = 1001
        object FileNewUnitItem: TMenuItem
          Action = FileNewUnitCommand
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object FileNewItem: TMenuItem
          Action = FileNewCommand
        end
      end
      object FileOpenItem: TMenuItem
        Action = FileOpenCommand
      end
      object FileOpenProjectItem: TMenuItem
        Action = FileOpenProjectCommand
      end
      object FileClosedFilesItem: TMenuItem
        Caption = '&Reopen'
        Enabled = False
        HelpContext = 1013
        Hint = 'Reopen'
      end
      object FileSaveSeparater: TMenuItem
        Caption = '-'
      end
      object FileSaveItem: TMenuItem
        Action = FileSaveCommand
      end
      object FileSaveAsItem: TMenuItem
        Action = FileSaveAsCommand
      end
      object FileSaveProjectAs: TMenuItem
        Action = FileSaveProjectAsCommand
      end
      object FileSaveAllItem: TMenuItem
        Action = FileSaveAllCommand
      end
      object FileCloseItem: TMenuItem
        Action = FileCloseCommand
      end
      object FileCloseAllItem: TMenuItem
        Action = FileCloseAllCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object FilePrintItem: TMenuItem
        Action = FilePrintCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Action = FileExitCommand
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      GroupIndex = 1
      HelpContext = 1025
      OnClick = EditMenuClick
      object EditUndoItem: TMenuItem
        Action = EditUndoCommand
      end
      object EditRedoItem: TMenuItem
        Action = EditRedoCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object EditCutItem: TMenuItem
        Action = EditCutCommand
      end
      object EditCopyItem: TMenuItem
        Action = EditCopyCommand
      end
      object EditPasteItem: TMenuItem
        Action = EditPasteCommand
      end
      object EditDeleteItem: TMenuItem
        Action = EditDeleteCommand
      end
      object EditSelectAll: TMenuItem
        Action = EditSelectAllCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object EditAlignGridItem: TMenuItem
        Action = EditAlignGridCommand
      end
      object EditFrontItem: TMenuItem
        Action = EditFrontCommand
      end
      object EditBackItem: TMenuItem
        Action = EditBackCommand
      end
      object EditAlignItem: TMenuItem
        Action = EditAlignCommand
      end
      object EditSizeItem: TMenuItem
        Action = EditSizeCommand
      end
      object EditScaleItem: TMenuItem
        Action = EditScaleCommand
      end
      object EditTabOrderItem: TMenuItem
        Action = EditTabOrderCommand
      end
      object EditCreationOrderItem: TMenuItem
        Action = EditCreationOrderCommand
      end
      object EditFlipChildrenItem: TMenuItem
        Caption = 'Flip C&hildren'
        HelpContext = 1045
        object EditFlipChildrenAllItem: TMenuItem
          Action = EditFlipChildrenAllCommand
        end
        object EditFlipChildrenSelectedItem: TMenuItem
          Action = EditFlipChildrenSelectedCommand
        end
      end
      object EditLockControlsItem: TMenuItem
        Action = EditLockControlsCommand
      end
    end
    object SearchMenu: TMenuItem
      Caption = '&Search'
      GroupIndex = 1
      HelpContext = 1050
      object SearchFindItem: TMenuItem
        Action = SearchFindCommand
      end
      object SearchFileFindItem: TMenuItem
        Action = SearchFileFindCommand
      end
      object SearchReplaceItem: TMenuItem
        Action = SearchReplaceCommand
      end
      object SearchAgainItem: TMenuItem
        Action = SearchAgainCommand
      end
      object SearchIncrementalItem: TMenuItem
        Action = SearchIncrementalCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object SearchGoToItem: TMenuItem
        Action = SearchGoToCommand
      end
    end
    object ViewsMenu: TMenuItem
      Caption = '&View'
      GroupIndex = 2
      HelpContext = 1075
      object ViewPrjMgrItem: TMenuItem
        Action = ViewPrjMgrCommand
      end
      object ViewObjInspItem: TMenuItem
        Action = ViewObjInspCommand
      end
      object ViewObjTreeViewItem: TMenuItem
        Action = ViewObjTreeViewCommand
      end
      object ViewAlignItem: TMenuItem
        Action = ViewAlignPaletteCommand
      end
      object ViewCompListItem: TMenuItem
        Action = ViewCompListCommand
      end
      object ViewWindowListItem: TMenuItem
        Action = ViewWindowListCommand
      end
      object ViewDebugItem: TMenuItem
        Caption = '&Debug Windows'
        HelpContext = 1074
        object ViewBreakpointsItem: TMenuItem
          Action = DebugBreakPointsCommand
        end
        object ViewCallStackItem: TMenuItem
          Action = DebugCallStackCommand
        end
        object ViewWatchesItem: TMenuItem
          Action = DebugWatchesCommand
        end
        object ViewThreadsItem: TMenuItem
          Action = DebugThreadsCommand
        end
        object ViewCPUItem: TMenuItem
          Action = DebugCPUCommand
        end
        object AltViewCallStackItem: TMenuItem
          Caption = 'AltViewCallStackItem'
          Visible = False
          OnClick = DebugCallStackCommandClick
        end
        object AltViewCPUItem: TMenuItem
          Caption = 'AltViewCPUItem'
          Visible = False
          OnClick = DebugCPUCommandClick
        end
      end
      object ViewDesktopsMenu: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Des&ktops'
        HelpContext = 1115
        OnClick = ViewDesktopsMenuClick
        object SaveDesktop1: TMenuItem
          Action = ViewSaveDesktopCommand
        end
        object DeleteDesktop: TMenuItem
          Action = ViewDeleteDesktopCommand
        end
        object SetDebugDesktop1: TMenuItem
          Action = ViewRuntimeDesktopCommand
        end
      end
      object TMenuItem
        Caption = '-'
      end
      object ViewToggleFormItem: TMenuItem
        Action = ViewToggleFormCommand
      end
      object ViewUnitItem: TMenuItem
        Action = ViewUnitCommand
      end
      object ViewFormItem: TMenuItem
        Action = ViewFormCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object ViewNewEditorItem: TMenuItem
        Action = ViewNewEditorCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object ViewToolbarsItem: TMenuItem
        Caption = 'Toolba&rs'
        HelpContext = 1071
        OnClick = ViewToolbarsMenuClick
        object TMenuItem
        end
      end
      object ViewSwapSourceFormItem: TMenuItem
        Action = ViewSwapSourceFormCommand
        Visible = False
      end
      object ViewNextWindowItem: TMenuItem
        Action = ViewNextWindow
        Visible = False
      end
    end
    object ProjectMenu: TMenuItem
      Caption = '&Project'
      GroupIndex = 2
      HelpContext = 1100
      object ProjectAddItem: TMenuItem
        Action = ProjectAddCommand
        GroupIndex = 2
      end
      object ProjectRemoveItem: TMenuItem
        Action = ProjectRemoveCommand
        GroupIndex = 2
      end
      object ProjectImportTypeLibraryItem: TMenuItem
        Action = ProjectImportTypeLibraryCommand
        GroupIndex = 2
      end
      object ProjectAddRepositoryItem: TMenuItem
        Action = ProjectAddRepositoryCommand
        GroupIndex = 2
      end
      object ViewPrjSourceItem: TMenuItem
        Action = ProjectViewSourceCommand
        GroupIndex = 2
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ProjectAddNewProjectItem: TMenuItem
        Action = ProjectAddNewProjectCommand
        GroupIndex = 2
      end
      object ProjectAddExistingProjectItem: TMenuItem
        Action = ProjectAddExistingProjectCommand
        GroupIndex = 2
      end
      object ProjectCompileSeparator: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ProjectBuildItem: TMenuItem
        Action = ProjectBuildCommand
        GroupIndex = 2
      end
      object ProjectInformationItem: TMenuItem
        Action = ProjectInformationCommand
        GroupIndex = 2
      end
      object ProjectClearUnitCacheItem: TMenuItem
        Caption = 'Clear Unit Cache'
        GroupIndex = 2
        Visible = False
        OnClick = ProjectClearUnitCacheItemClick
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ProjectCompileAllItem: TMenuItem
        Action = ProjectCompileAllCommand
        GroupIndex = 2
      end
      object ProjectBuildAllItem: TMenuItem
        Action = ProjectBuildAllCommand
        GroupIndex = 2
      end
      object ProjectSeparator2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object ProjectOptionsItem: TMenuItem
        Action = ProjectOptionsCommand
        GroupIndex = 2
      end
    end
    object RunMenu: TMenuItem
      Caption = '&Run'
      GroupIndex = 2
      HelpContext = 1125
      object RunRunItem: TMenuItem
        Action = RunRunCommand
      end
      object RunRunNoDebugItem: TMenuItem
        Action = RunRunNoDebugCommand
      end
      object RunParametersItem: TMenuItem
        Action = RunParametersCommand
      end
      object RunParametersSeparator: TMenuItem
        Caption = '-'
      end
      object RunStepOverItem: TMenuItem
        Action = RunStepOverCommand
      end
      object RunTraceIntoItem: TMenuItem
        Action = RunTraceIntoCommand
      end
      object RunTraceToSourceItem: TMenuItem
        Action = RunTraceToSourceCommand
      end
      object RunGotoCursorItem: TMenuItem
        Action = RunGotoCursorCommand
      end
      object RunUntilReturnItem: TMenuItem
        Action = RunUntilReturnCommand
      end
      object RunShowCSIPItem: TMenuItem
        Action = RunShowCSIPCommand
      end
      object RunPauseItem: TMenuItem
        Action = RunPauseCommand
      end
      object RunResetItem: TMenuItem
        Action = RunResetCommand
      end
      object RunDetachItem: TMenuItem
        Action = RunDetachCommand
      end
      object RunInspectSeparator: TMenuItem
        Caption = '-'
      end
      object RunEvalModItem: TMenuItem
        Action = RunEvalModCommand
      end
      object RunAddWatchItem: TMenuItem
        Action = RunAddWatchCommand
      end
      object RunAddBreakItem: TMenuItem
        Caption = 'Add &Breakpoint'
        HelpContext = 1134
        object RunAddSourceBreakpointItem: TMenuItem
          Action = RunAddSourceBreakpointCommand
        end
        object RunAddAddressBreakpointItem: TMenuItem
          Action = RunAddAddressBreakpointCommand
        end
      end
    end
    object ComponentMenu: TMenuItem
      Caption = '&Component'
      GroupIndex = 2
      HelpContext = 1140
      object ComponentNewItem: TMenuItem
        Action = ComponentNewCommand
      end
      object ComponentAddtoPackage: TMenuItem
        Action = ComponentAddtoPackageCommand
      end
      object ComponentImportAXCItem: TMenuItem
        Action = ComponentImportAXCCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object ComponentInstallCompositeItem: TMenuItem
        Action = ComponentInstallCompositeCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object ComponentInstallPackagesItem: TMenuItem
        Action = ComponentInstallPackagesCommand
      end
      object ComponentPaletteItem: TMenuItem
        Action = ComponentPaletteCommand
      end
    end
    object ToolsMenu: TMenuItem
      Caption = '&Tools'
      GroupIndex = 2
      HelpContext = 1160
      object ToolsOptionsItem: TMenuItem
        Action = ToolsOptionsCommand
      end
      object ToolsEditorOptionsItem: TMenuItem
        Action = ToolsEditorOptionsCommand
      end
      object ToolsDebuggerOptionsItem: TMenuItem
        Action = ToolsDebuggerOptionsCommand
      end
      object ToolsGalleryItem: TMenuItem
        Action = ToolsGalleryCommand
      end
      object ToolsToolsItem: TMenuItem
        Action = ToolsToolsCommand
      end
    end
    object WindowsMenu: TMenuItem
      Caption = '&Window'
      GroupIndex = 2
      HelpContext = 1175
      OnClick = WindowsMenuClick
      object Temp1: TMenuItem
        Caption = 'Temp'
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      GroupIndex = 2
      HelpContext = 1200
      OnClick = HelpCommandsUpdate
      object HelpContentsItem: TMenuItem
        Action = HelpContentsCommand
      end
      object HelpWinSDKItem: TMenuItem
        Action = HelpWinSDKCommand
      end
      object TMenuItem
        Caption = '-'
      end
      object HelpInprisePage: TMenuItem
        Action = HelpInprisePageCommand
      end
      object HelpCommunityPage: TMenuItem
        Action = HelpCommunityPageCommand
      end
      object HelpProgGuideSeparater: TMenuItem
        Caption = '-'
        Visible = False
      end
    end
  end
  object OpenFileDialog: TOpenDialog
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 70
    Top = 88
  end
  object SaveFileDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing]
    Left = 38
    Top = 88
  end
  object PaletteMenu: TPopupActionBar
    HelpContext = 4745
    Images = ImageList1
    OnPopup = PaletteLocalPopup
    Left = 168
    Top = 88
    object TabsItem: TMenuItem
      Caption = '&Tabs'
      OnClick = TabsItemClick
      object Testing1: TMenuItem
        Caption = 'Testing'
      end
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object ShowPaletteHints: TMenuItem
      Caption = '&Show Hints'
      HelpContext = 2302
      OnClick = TogglePaletteHints
    end
    object PaletteHideItem: TMenuItem
      Caption = '&Hide'
      HelpContext = 2301
      OnClick = PaletteHideItemClick
    end
    object PaletteHelpItem: TMenuItem
      Caption = 'H&elp'
      HelpContext = 2303
      OnClick = PaletteHelp
    end
    object PaletteOrientItem: TMenuItem
      Caption = '&Orientation'
      Visible = False
      object PaletteTopItem: TMenuItem
        Caption = '&Top'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = PaletteTabPosClick
      end
      object PaletteBottomItem: TMenuItem
        Tag = 1
        Caption = '&Bottom'
        GroupIndex = 1
        RadioItem = True
        OnClick = PaletteTabPosClick
      end
      object PaletteLeftItem: TMenuItem
        Tag = 2
        Caption = '&Left'
        GroupIndex = 1
        RadioItem = True
        Visible = False
        OnClick = PaletteTabPosClick
      end
      object PaletteRightItem: TMenuItem
        Tag = 3
        Caption = '&Right'
        GroupIndex = 1
        RadioItem = True
        Visible = False
        OnClick = PaletteTabPosClick
      end
    end
    object TMenuItem
      Caption = '-'
    end
    object Configure2: TMenuItem
      Caption = 'P&roperties'
      HelpContext = 2300
      OnClick = ConfigurePalette
    end
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 6
    Top = 88
  end
  object ImageList1: TImageList
    AllocBy = 128
    Left = 328
    Top = 88
    Bitmap = {
      494C01017D008000800010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000000002000001002000000000000000
      020000000000000000000000000000000000666666004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B00666666000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000081818100C5C5C500C1C1C100B4B4
      B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4B400B4B4
      B4004B4B4B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000081818100DADADA00CDCDCD00CDCD
      CD00C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C1C1C100B4B4
      B4004B4B4B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000081818100E6E6E600E6E6E600CDCD
      CD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00001FFF00CDCDCD0000BF0000B4B4
      B400595959000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008E8E8E0000000000000000000000
      0000000000000000000000000000E6E6E600E6E6E600E6E6E600DADADA00CDCD
      CD00666666000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9A00A7A7A700A7A7A700A7A7
      A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7
      A700A7A7A7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001FFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001FFF0000000000000000000000
      0000A8A8A8004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B00A7A7A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001FFF000000000000000000001F
      FF009A9A9A0099F8FF0099F8FF0099F8FF0055DFFF00AADFD50055DFFF0055DF
      FF00AADFD50055DFFF0055C0D4004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000001FFF00000000000000
      0000001FFF00A9FFFF00A9FFFF00A9FFFF0099F7FF00A9FFFF0098F7FF0098F7
      FF0054DFFF00AADFD50055DFFF004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000001FFF00001F
      FF00001FFF00001FFF00A9FFFF00A9FFFF00A9FFFF0099F8FF0099F8FF0055DF
      FF0098F7FF0055DFFF00AADFD5004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000001FFF0000000000A9FFFF00A9FFFF00A9FFFF00A9FFFF0099F7FF0099F7
      FF0055DFFF0099F8FF0055DFFF00595959000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000001F
      FF00C1C1C100000000000000000000000000A9FFFF009A9A9A009A9A9A009A9A
      9A0055BFD30054BFD30055BFD300737373000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A7A7A700B4B4B4009A9A9A009A9A9A009A9A9A0055DFFF0055DFFF0055DF
      FF0055DFFF0055DFFF0054BFD300818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B4B4B400A9FFFF0000F2FF0000F1FF0055BFD3008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1C1C1009B9B9B008D8D8D008D8D8D009B9B9B00B4B4B400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A8A8A80073737300553F2A00553F2A00553F2A00553F2A00553F2A00553F
      2A00553F2A00553F2A00553F2A00A7A7A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008F6E1C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000303030003030300030303000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000092929200AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00553F2A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000081610000FF9F2A008F6F1D000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000303030003030300000A0AB0000A0AB0000A0AB00303030000000
      0000000000000000000000000000000000000000000000000000000000000000
      000091919100AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF00553F2A008181810059595900595959005959
      5900595959005959590059595900595959005959590059595900595959005959
      590059595900595959004B4B4B0081818100A8A8A80073737300553F2A00553F
      2A003D3D3D003D3D3D00B4B4B40080610000FF9F2A00D9A77D00FF9F2A008F6F
      1C0073737300747474003D3D3D00A7A7A7000000000000000000000000003030
      30003030300000A0AB0000A0AB0000F2FF0000F2FF0000F2FF0000A0AB003030
      3000000000000000000000000000000000000000000000000000000000000000
      000091919100AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00553F2A004B4B4B00D9A77D00CB8C4400CB8C
      4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C
      4400CB8C4400CB8C4400CB8C44004B4B4B0091919100A9FFFF0099F8FF0099F8
      FF0055DFFF0055BFD30080610000D9A77D00D9A77D00D9A77D00D9A77D00FF9F
      2A008F6F1D0055DFFF0055C0D4003D3D3D0000000000303030003030300000A0
      AB0000A0AB0000F2FF0000F2FF00633600006336000000A0AB0000F2FF0000A0
      AB00303030000000000000000000000000000000000000000000000000000000
      000090909000AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF00AAFFFF0099F8FF00AAFFFF00767676007F5B0000FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B0091919100A9FFFF00A9FFFF00A9FF
      FF00A9FFFF0080610000D9A77D00FFFFC300FFFFC300D9A77D00D9A77D00D9A7
      7D00FF9F2A008F6F1D0055DFFF003D3D3D006336000000A0AB0000A0AB0000F2
      FF0000F2FF00633600007F5B0000D9A77D00D9A77D006336000000A0AB0000F2
      FF0000A0AB003030300000000000000000000000000000000000000000000000
      000091919100AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00808080007F5B0000FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B0091919100A9FFFF00A9FFFF00A9FF
      FF00E1BE78008F6F1C008F6F1C008F6E1C00FFFFC300FFFFC400D9A77D008F6F
      1D008F6F1C008F6F1C00E1BE7800737373006336000000F2FF0000F2FF006336
      00007F5B0000BC720000BC720000BC720000BC720000D9A77D006336000000A0
      AB0000F2FF0000A0AB0030303000000000000000000000000000000000000000
      00009191910098F7FF00AAFFFF0099F8FF00AAFFFF0099F8FF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF008D8D8D007F5B0000FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B0090909000A9FFFF00A9FFFF00A9FF
      FF00A9FFFF00A9FFFF00A9FFFF009D7C3000FFFFC300FFFFC300FFF1AB009D7C
      3000B4B4B40099F8FF0055DFFF003D3D3D00633600006336000063360000BC72
      0000BC720000BC72000000F2FF00BC720000BC720000BC720000D9A77D006336
      000000A0AB0000F2FF0000A0AB00303030000000000000000000000000000000
      0000C1C1C100C1C1C10098F7FF00AAFFFF0099F8FF00AAFFFF0099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF009B9B9B007F5B0000FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B009191910000000000A9FFFF00A9FF
      FF00A9FFFF00A9FFFF00A9FFFF00AB8A4000FFFFC300FFFFC300D9A77D00FBD7
      910098F7FF0055DFFF00AADFD5003D3D3D00633600007F5B0000A3760000BC72
      0000BC720000BC72000068F5FF00BC720000BC720000BC720000BC720000D9A7
      7D006336000000A0AB0030303000000000000000000000000000EFAD0000A377
      00007F5B0000EFAD0000C1C1C10098F7FF0098F7FF0098F7FF0098F7FF0098F7
      FF0098F7FF0098F7FF0098F7FF00A7A7A7007F5B0000FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B0091919100A9FFFF00A9FFFF00A9FF
      FF00A9FFFF00A9FFFF00B9974F00D9A77D00FFF0AA00FFFFC300AB8A400099F7
      FF0099F7FF0099F8FF0055DFFF00595959000000000063360000FF9F2A00D9A7
      7D00BC720000BC720000BC720000BC72000000F2FF0000F2FF00BC720000BC72
      0000D9A77D0063360000303030000000000000000000000000007F5B0000D9A7
      7D00D9A77D007F5B0000C1C1C100C1C1C10098F7FF0098F7FF0098F7FF0098F7
      FF0055DFFF0055DFFF0054BFD300737373007F5B000000000000D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D00CB8C44004B4B4B00A8A8A80000000000A9FFFF000000
      0000A9FFFF00A8FEFF00B9974E00FFE49E00FFF0AA00E1BE7700FBD79000A9FF
      FF00A9FFFF00A9FFFF00A9FFFF0066666600000000000000000063360000FF9F
      2A00D9A77D00BC720000BC720000BC720000BC72000068F5FF0000F2FF0000F2
      FF00BC720000D9A77D003030300000000000EFAD0000A2760000A2760000D9A7
      7D00D9A77D00A37700007F5B0000EFAD0000C1C1C10098F7FF0098F7FF00B4B4
      B400A0A0A0007373730073737300B4B4B4007F5B00000000000000000000FFFF
      CC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFF
      CC00FFFFCC00FFFFCC00D9A77D004B4B4B00B4B4B400A9FFFF0000000000A9FF
      FF00A8FEFF00AB893F00D9A77D00FBD79000E1BE7800A9FFFF00A9FFFF00A9FF
      FF00A9FFFF00A9FFFF00A9FFFF00737373000000000000000000000000006336
      0000FFFF7F00D9A77D00BC72000068F5FF00BC720000BC72000000F2FF0000F2
      FF00BC720000BC720000D9A77D0030303000AA7F0000F6CF6D00D9A77D00D9A7
      7D00D9A77D00D9A77D00FF9F2A007F5B0000C1C1C10098F7FF0098F7FF00A1A1
      A100E6E6E600DADADA00DADADA00B4B4B4007F5B0000A3760000A3760000A376
      0000A3760000A3760000A3760000A3760000A3760000A3760000A3760000A376
      0000A3760000A3760000A37600004B4B4B00C1C1C10000000000A9FFFF000000
      0000D4B16A00D4B26B00D4B26B00D4B26B00EECB8400C6C6C600C6C6C600AADF
      D50055BFD30054BFD30055BFD300818181000000000000000000000000000000
      000063360000FFFF7F00D9A77D00BC72000068F5FF0000F2FF0000F2FF00BC72
      0000D9A77D00FF9F2A007F5B000063360000AA7F0000FFFFCB00FFFFCB00F6CF
      6D00D9A77D00F6CF6D00F6CF6D00A3770000C1C1C10098F7FF0098F7FF00A7A7
      A700AAFFFF00E7E7E700B4B4B400000000007F5B0000AA9F000000000000A376
      00009DA900009DA90000AA9F00009DA900009DA900009DA90000A37600000000
      0000A376000000000000A37600004B4B4B00CCCCCC00B5B5B500B4B4B400B4B4
      B400CBCBCB00AADFD500AADFD50055DFFF0055DFFF0055DFFF0055DFFF0055DF
      FF0055DFFF0055DFFF0054BFD300C1C1C1000000000000000000000000000000
      00000000000063360000FFFF7F00D9A77D00BC720000BC720000D9A77D00FF9F
      2A007F5B0000633600000000000000000000F7D06C00E5B72600E2B62900FFFF
      6600F6CF6D00AA7F0000AA7F0000F7CF6C0000000000AAFFFF00E6E6E6008D8D
      8D0000000000B4B4B40000000000000000007F5B0000AA9F0000AA9F0000AA9F
      0000AA9F0000AA9F0000AA9F0000AA9F0000AA9F0000AA9F0000AA9F0000AA9F
      0000AA9F0000AA9F0000AA9F00004B4B4B0000000000DADADA0000F2FF0000F2
      FF0000F1FF0000F1FF0055BFD300E6E6E6008D8D8D008D8D8D008D8D8D008D8D
      8D008D8D8D008D8D8D008D8D8D00000000000000000000000000000000000000
      0000000000000000000063360000FFFF7F00FFFF7F00FFFF7F007F5B00006336
      0000000000000000000000000000000000000000000000000000F1BF2B00FFFF
      9900FFFF9900AA7F0000B4B4B400CDCDCD00C1C1C100B4B4B4008D8D8D008D8D
      8D00C1C1C100000000000000000000000000A37600007F5B00007F5B00007F5B
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      00007F5B00007F5B00004B4B4B0081818100000000009B9B9B0054FFFF0067F4
      FF0067F4FF0067F4FF0000F1FF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000633600006336000063360000000000000000
      0000000000000000000000000000000000000000000000000000F8D06D00FFC8
      2D00FDC83100F7CF6C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9B008D8D
      8D008D8D8D008D8D8D009B9B9B00000000000000000000000000000000000000
      000000000000000000000000000000000000A8A8A80073737300553F2A00553F
      2A00553F2A00553F2A00553F2A00553F2A00553F2A00A7A7A700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      AA000000000000000000A8A8A80073737300553F2A00553F2A00553F2A00553F
      2A00553F2A00553F2A00553F2A00A7A7A7008181810059595900595959005959
      590059595900595959005959590059595900595959004B4B4B00818181000000
      00000000000000000000000000000000000000000000A7A7A700737373007373
      7300595959004B4B4B004B4B4B003D3D3D00303030003030300030303000A7A7
      A7000000000000000000000000000000000092929200AAFFFF00AAFFFF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00AAFFFF0099F8FF00553F2A00000000000000
      00000000000000000000000000000000000000000000000000000000AA000000
      0000000000000000000073737300AAFFFF00AAFFFF00AAFFFF0099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00553F2A004B4B4B00D9A77D00CB8C4400CB8C
      4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C4400CB8C44004B4B4B000000
      000000000000000000000000000000000000A7A7A70059595900CDCDCD00E6E6
      E600C1C1C100CDCDCD00F0F0F000EDEDED00E6E6E600A7A7A700333333003030
      3000A7A7A70000000000000000000000000091919100AAFFFF00AAFFFF00AAFF
      FF00AAFFFF0099F8FF00AAFFFF0099F8FF00AAFFFF00553F2A00000000000000
      00000000000000000000000000000000000000000000000000000000AA000000
      0000000000000000000073737300AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF00553F2A0073737300FFFF9900D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C44004B4B4B000000
      0000000000000000000000000000000000004B4B4B0059595900DADADA00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600DADADA00737373002929
      29003030300000000000000000000000000091919100AAFFFF00AAFFFF00AAFF
      FF00AAFFFF00AAFFFF0099F8FF00AAFFFF0099F8FF00553F2A00A7A7A7007373
      7300553F2A00553F2A00553F2A00A7A7A700000000000000AA000000AA000000
      AA00000000000000000073737300AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00553F2A0073737300FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C44004B4B4B000000
      0000000000000000000000000000000000004B4B4B0059595900D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00737373003636
      3600303030009A9A9A00A7A7A7000000000091919100AAFFFF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF0099F8FF00AAFFFF00553F2A0099F8FF0098F7
      FF0099F8FF00AAFFFF0099F8FF00553F2A0000000000000000000000AA000000
      0000000000000000000073737300AAFFFF00AAFFFF0099F8FF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF008D8D8D007373730000000000D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C44004B4B4B000000
      0000000000000000000000000000000000004B4B4B0059595900D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00737373004A4A
      4A00303030007373730030303000A7A7A70091919100AAFFFF00AAFFFF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00AAFFFF0099F8FF0073737300AAFFFF0098F7
      FF00AAFFFF0099F8FF00AAFFFF00737373000000000000000000000000000000
      000000000000000000007373730000000000AAFFFF0098F7FF0098F7FF0098F7
      FF0099F8FF0099F8FF0099F8FF00A7A7A7007373730000000000D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C44004B4B4B000000
      0000000000000000000000000000000000005959590059595900D9A77D007373
      7300737373007373730073737300737373007373730073737300737373005353
      53004B4B4B007373730029292900303030009292920000000000AAFFFF0098F7
      FF0098F7FF0098F7FF0099F8FF0099F8FF0099F8FF0073737300AAFFFF0098F7
      FF0099F8FF00AAFFFF0099F8FF00553F2A007373730073737300737373007373
      730081818100C1C1C1007373730000000000AAFFFF0098F7FF0098F7FF0098F7
      FF0055DFFF0055DFFF0054BFD300737373007373730000000000000000000000
      0000000000000000000000000000FFFFCC00FFFF9900D9A77D004B4B4B00C1C1
      C10059595900595959004B4B4B00818181005959590059595900D9A77D00AAFF
      FF00AAFFFF00AAFFFF00AAFFFF0099F8FF0099F8FF0099F8FF009A9A9A005656
      56004B4B4B007373730059595900303030009090900000000000AAFFFF0098F7
      FF0098F7FF0098F7FF0055DFFF0055DFFF0054BFD30073737300AAFFFF0098F7
      FF00AAFFFF0099F8FF00AAFFFF008D8D8D0073737300D9A77D00CB8C4400CB8C
      4400CB8C4400CB8C44008181810000000000AAFFFF0098F7FF0098F7FF00B4B4
      B400A0A0A0007373730073737300B4B4B40073737300A3760000A3760000A376
      0000A3760000A3760000A3760000A3760000A3760000A37600004B4B4B00AA9F
      2A00AABF2A009DA900009DA900004B4B4B006666660059595900AAFFFF00AAFF
      FF00A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700AAFFFF0099F8FF007171
      71005959590073737300595959003D3D3D009191910000000000AAFFFF0098F7
      FF0098F7FF00B4B4B400A0A0A0007373730073737300B4B4B40099F8FF0098F7
      FF0099F8FF00AAFFFF0099F8FF009B9B9B0073737300FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D008E8E8E0000000000AAFFFF00AAFFFF0098F7FF00A1A1
      A100E6E6E600DADADA00DADADA00B4B4B40073737300A3760000A3760000A376
      0000A3760000A3760000A3760000A3760000A3760000A37600004B4B4B00AA9F
      2A00D9A77D00D9A77D009DA900004B4B4B007373730098989800AAFFFF00AAFF
      FF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8FF009898
      98005959590073737300595959004B4B4B009090900000000000AAFFFF00AAFF
      FF0098F7FF00A1A1A100E6E6E600DADADA00DADADA00A1A1A10098F7FF0098F7
      FF0099F8FF0099F8FF0099F8FF00A7A7A70073737300FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D009A9A9A0000000000AAFFFF00AAFFFF0098F7FF00A7A7
      A700AAFFFF00E7E7E700B4B4B400000000008181810081818100818181008181
      810081818100818181008181810081818100818181004B4B4B0081818100AA9F
      2A00D9A77D00D9A77D009DA900004B4B4B0073737300A6A6A60000000000AAFF
      FF00CB8C4400CB8C4400A7A7A700A7A7A700A7A7A70099F8FF00AAFFFF00A6A6
      A600666666008E8E8E00595959004B4B4B009191910000000000AAFFFF00AAFF
      FF0098F7FF00A7A7A700AAFFFF00E7E7E700A1A1A10098F7FF0098F7FF0098F7
      FF0055DFFF0055DFFF0054BFD3007373730073737300FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00A7A7A700000000000000000000000000AAFFFF008D8D
      8D0000000000B4B4B40000000000000000000000000000000000000000000000
      000000000000C1C1C100D9A77D00C8CC7A00C8CC7A00C8CC7A00C8CC7A00D9A7
      7D00D9A77D00D9A77D009DA900004B4B4B0073737300B1B1B100000000000000
      0000AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00B1B1
      B100737373008E8E8E00666666004B4B4B009191910000000000000000000000
      0000AAFFFF008D8D8D0000000000A1A1A100AAFFFF0098F7FF0098F7FF00B4B4
      B400A0A0A0007373730073737300B4B4B40073737300FFFFCC00D9A77D00D9A7
      7D00D9A77D00D9A77D00C1C1C100737373007373730073737300737373008D8D
      8D00C1C1C10000000000000000000000AA000000000000000000000000000000
      00000000000073737300FFFFCC00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D009DA900004B4B4B00A7A7A70081818100595959005959
      5900595959005959590059595900595959005959590059595900666666008181
      8100A7A7A7008E8E8E007373730059595900B4B4B400B4B4B400A7A7A700A7A7
      A7008D8D8D008D8D8D00A1A1A10000000000AAFFFF0099F8FF0098F7FF00A1A1
      A100E6E6E600DADADA00DADADA00B4B4B40073737300FFFFCC00FFFFCC00FFFF
      CC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00AADF7F00C1C1C1000000
      00000000AA0000000000000000000000AA000000000000000000000000000000
      00000000000073737300FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFF
      CC00FFFFCC00FFFFCC00AABF2A004B4B4B0000000000A7A7A7008E8E8E008E8E
      8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E009A9A
      9A00DADADA008E8E8E0081818100666666000000000000000000000000000000
      00000000000000000000C1C1C1000000000099F8FF0099F8FF0098F7FF00A7A7
      A700AAFFFF00E7E7E700B4B4B4000000000073737300A3760000A3760000A376
      0000A3760000A3760000A3760000A3760000A3760000A3760000C1C1C1000000
      AA000000AA000000AA000000AA00000000000000000000000000000000000000
      00000000000073737300A3760000A3760000A3760000A3760000A3760000A376
      0000A3760000A3760000A37600004B4B4B00000000000000000000000000C1C1
      C100B1B1B100DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADA
      DA00DADADA00DADADA00B1B1B100737373000000000000000000000000000000
      000000000000000000008D8D8D00AAFFFF00AAFFFF00AAFFFF00AAFFFF008D8D
      8D0000000000B4B4B400000000000000000073737300A3760000A3760000A376
      0000A3760000A3760000A3760000A3760000A3760000A3760000818181000000
      00000000AA000000000000000000000000000000000000000000000000000000
      00000000000073737300A3760000A3760000A3760000A3760000A3760000A376
      0000A3760000A3760000A37600004B4B4B00000000000000000000000000A7A7
      A70081818100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1
      C100C1C1C100C1C1C10081818100A7A7A7000000000000000000000000000000
      00000000000000000000B4B4B4008D8D8D008D8D8D008D8D8D008D8D8D008D8D
      8D00C1C1C1000000000000000000000000008181810081818100818181008181
      8100818181008181810081818100818181008181810081818100818181000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008181810081818100818181008181810081818100818181008181
      8100818181008181810073737300818181000000000000000000000000000000
      0000A7A7A7008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E
      8E008E8E8E008E8E8E009A9A9A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      84008484840000000000000000000000000000000000BDBDBD007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400000000000000000000000000BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B00000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000084848400000000000000
      00000000000084848400FFFFFF000000000000000000BDBDBD00BDBDBD007B7B
      7B00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B00BDBDBD007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000084848400848484008484840000000000BDBDBD00BDBDBD007B7B
      7B00FFFFFF00FFFFFF00FFFFFF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B7B7B00BDBDBD007B7B7B00000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000000000000000000000000000000000000BDBDBD00BDBDBD007B7B
      7B00FFFFFF00FFFFFF00FFFFFF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B00000000000000000000000000000000000000
      00000000000000000000848484000000FF000000FF0084848400000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000084848400000000000000000000000000BDBDBD00BDBDBD007B7B
      7B00FFFFFF00FFFFFF00FFFFFF00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B00BDBDBD007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000084848400FFFFFF000000000000000000BDBDBD00BDBDBD007B7B
      7B00FFFFFF00FFFFFF00FFFFFF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B7B7B00BDBDBD007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000FFFFFF00FFFFFF0000000000000000000000
      00000000000084848400848484008484840000000000BDBDBD00BDBDBD007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00FFFFFF000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000000000000BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF00FFFF
      FF0000000000000000000000000000000000000000000000000084848400FFFF
      FF000000000000000000000000000000000000000000BDBDBD007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00FFFFFF000000
      8400000000000000000000000000000000000000000000000000848484008484
      84008484840000000000000000000000000000000000BDBDBD00FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFF
      FF00FF000000FFFFFF007B7B7B00000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
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
      0000626262000000000000000000000000000000000000000000D9A77D000000
      0000D9A77D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B00000000003D3D3D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003D3D3D00000000000000000000000000000000000000
      000062626200626262000000000000000000FFFF9900D9A77D00D9A77D000000
      0000D9A77D00D9A77D00FFFF990000000000000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600848484008484840084848400848484008484840084848400848484008484
      84008484840084848400848484004B4B4B0000000000B1B1B100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B1B1B10000000000000000000000000062626200626262006262
      620062626200626262006262620000000000D9A77D00FFFF9900D9A77D000000
      0000D9A77D00FFFF9900D9A77D00000000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B000000000000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B00000000000000000000000000000000000000000062626200000000000000
      0000626262006262620000000000D9A77D00D9A77D00D9A77D00FFFF99000000
      0000FFFF9900D9A77D00D9A77D00D9A77D00000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B0000000000000000004B4B4B00DDC9
      B800DDC9B800DDC9B800DDC9B800DDC9B800DDC9B800DDC9B800DDC9B800DDC9
      B8004B4B4B000000000000000000000000000000000062626200000000000000
      0000626262000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C6000000FF000000FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B0000000000000000004B4B4B00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D004B4B4B000000000000000000000000000000000062626200000000000000
      0000000000000000000000000000D9A77D00D9A77D00D9A77D00FFFF99000000
      0000FFFF9900D9A77D00D9A77D00D9A77D00000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C6000000000000000000000000004B4B4B00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D004B4B4B000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9A77D00FFFF9900D9A77D000000
      0000D9A77D00FFFF9900D9A77D00000000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B4B4B00D9A7
      7D00737373007373730073737300737373007373730073737300737373007373
      73004B4B4B000000000000000000000000006262620062626200626262006262
      620062626200626262006262620062626200FFFF9900D9A77D00D9A77D000000
      0000D9A77D00D9A77D00FFFF990000000000000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF00000000008484840000FF
      000000FF0000000000000000000000000000000000004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B000000000000000000000000000000
      0000000000004B4B4B0000000000000000003D3D3D0000000000B1B1B100D9A7
      7D00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8FF0099F8FF0099F8FF009A9A
      9A00B1B1B10000000000B1B1B1003D3D3D0062626200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00626262000000000000000000D9A77D000000
      0000D9A77D000000000000000000000000000000000000FFFF00000000000000
      00000000000000000000000000000000000000000000848484008484840000FF
      000000FF00000000000000000000000000004B4B4B00DFFFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF00DFFFFF004B4B4B0000000000000000000000
      00004B4B4B004B4B4B004B4B4B000000000000000000000000004B4B4B00AAFF
      FF00AAFFFF00A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700AAFFFF0099F8
      FF004B4B4B0000000000000000000000000062626200FFFFFF00626262006262
      62006262620062626200FFFFFF00626262000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF008484840000FF000000FF000000FF
      000000FF000000FF000000FF0000000000004B4B4B0080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0060DFDF0080FFFF004B4B4B0000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B0000000000000000004B4B4B00AAFF
      FF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8
      FF004B4B4B0000000000000000000000000062626200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00626262000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF008484840000FF000000FF000000FF
      000000FF000000FF000000FF0000000000004B4B4B0080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF004B4B4B0000000000000000000000
      0000000000004B4B4B00000000000000000000000000000000004B4B4B00FFFF
      FF00AAFFFF00CB8C4400CB8C4400A7A7A700A7A7A700A7A7A70099F8FF00AAFF
      FF004B4B4B0000000000000000000000000062626200FFFFFF00626262006262
      62006262620062626200FFFFFF00626262000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00848484008484840000FF
      000000FF00000000000000000000000000004B4B4B0000FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF004B4B4B0000000000000000000000
      0000000000004B4B4B00000000000000000000000000000000004B4B4B00FFFF
      FF00FFFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF004B4B4B0000000000000000000000000062626200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00626262000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008484840000FF
      000000FF00000000000000000000000000004B4B4B004B4B4B004B4B4B004B4B
      4B0010BFCF0080FFFF0080FFFF00DFFFFF004B4B4B0000000000000000000000
      0000000000004B4B4B0000000000000000000000000000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B000000000000000000000000000000000062626200FFFFFF0062626200FFFF
      FF00626262006262620062626200626262000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000004B4B4B0000FFFF00FFFFFF0000CF
      CF004B4B4B004B4B4B004B4B4B004B4B4B00000000004B4B4B004B4B4B004B4B
      4B004B4B4B0084848400000000000000000000000000B1B1B100000000000000
      0000000000000000000000000000B1B1B1000000000000000000000000000000
      000000000000B1B1B100000000000000000062626200FFFFFF00FFFFFF00FFFF
      FF0062626200FFFFFF0062626200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B4B4B004B4B4B004B4B
      4B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D3D3D0000000000000000000000
      00000000000000000000000000003D3D3D000000000000000000000000000000
      000000000000000000003D3D3D00000000006262620062626200626262006262
      6200626262006262620000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000626262006262620062626200626262006262620062626200626262006262
      620062626200626262000000000000000000000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      6200FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF006262620000000000000000000000000000FFFF000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      620062626200FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF006262620000000000000000000000000000FFFF000000
      0000000000008484840084848400848484008484840084848400848484008484
      840084848400FFFFFF00FFFFFF00848484000000000000000000000000000000
      0000000000000000000084848400FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      62006262620000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0062626200000000000000000000FFFF000000000000FF
      FF0084848400FFFFFF0084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000006262
      620000CFCF006262620000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF0062626200000000000000000000FFFF000000
      000000FFFF0084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484008484840084848400000000000000000000000000000000000000
      000000000000000000008484840084848400848484008484840000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000000000000000000000008484840000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      6200FFFFFF00D9A77D0062626200626262006262620062626200626262006262
      6200626262006262620062626200626262000000000000FFFF000000000000FF
      FF0000FFFF0000FFFF0084848400848484008484840084848400848484008484
      8400FFFFFF00FFFFFF00FFFFFF00848484000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00848484008484
      840084848400848484008484840000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      620000FFFF00D9A77D00FFFF9900FFFF9900FFFF9900FFFF9900FFFF9900FFFF
      9900FFFF9900D9A77D000000000000000000000000000000000000FFFF000000
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      0000848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400000000000000
      00000000000000000000000000000000000084848400000000008484840000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      6200FFFFFF00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D0000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000FFFF0084848400FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      620000FFFF00BBF3F30000FFFF00FFFFFF0000FFFF00FFFFFF003535F100FFFF
      FF00626262000E0ED9000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00848484008484
      84008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      6200BBF3F30000FFFF00B3EBEB00626262006262620062626200626262003535
      F100000000000E0ED900000000003535F1000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000FFFF0084848400FFFF
      FF0000FFFF008484840084848400848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      620000FFFF00ABE3E30000FFFF00626262000000000000000000000000000000
      0000000000000E0ED9000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF0000FFFF008484
      840084848400FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000626262006262620062626200000000000000000000000000000000000E0E
      D9000E0ED900000000000E0ED9000E0ED9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      00000000000000000000000000000000000084848400848484008484840000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E0ED90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF0000FFFF008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003535
      F100000000000E0ED900000000003535F1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535F1000000
      0000000000000E0ED900000000000000000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      000084848400000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000C6C6C60084848400848484008484
      8400848484008484840084848400848484008484840084848400000000000000
      00000000000000000000000000000000000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848484000000000000000000C6C6
      C60000000000000000008484840000000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60084848400000000000000
      00000000000000000000000000000000000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008484840000000000FFFF0000FFFF0000FFFF
      FF00FFFF0000FF0000000000000084848400FF000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      000000000000000000000000000000000000C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400000000000000
      00000000000000000000000000000000000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF0000FFFF00008484
      8400FF000000FFFF0000FF00000000000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      000000000000000000000000000000000000C6C6C600FFFFFF00848484008484
      8400848484008484840084848400FFFFFF00C6C6C60084848400000000000000
      00000000000000000000000000000000000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF000084848400C6C6
      C60084848400FF000000FFFF000000000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      000000000000000000000000000000000000C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400FFFF0000FFFF
      FF00FFFF0000848484008484840000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000C6C6C600FFFFFF00848484008484
      8400848484008484840084848400FFFFFF00C6C6C60084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFFFF00FFFF
      0000FFFFFF00FFFF00000000000000000000FF000000FFFFFF00FF0000008400
      00008400000084000000840000008400000084000000FF000000FFFFFF00FF00
      000000000000000000000000000000000000C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00000000000000000000000000000000000000
      FF00000084000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000084848400848484008484840084848400C6C6C600FFFFFF00848484008484
      8400848484008484840084848400FFFFFF00C6C6C60084848400C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C6000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000084000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      000000000000000000000000000084848400FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF000000000000000000FFFFFF000000FF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      000000000000000000000000000084848400C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00848484008484
      8400848484008484840084848400FFFFFF008484840084848400C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C6000000000000000000FFFFFF000000FF000000
      FF000000FF000000FF000000FF00000084000000000000000000000000000000
      00008400000084000000000000000000000000000000FFFFFF000000FF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000000000000000000000000000
      0000840000008400000000000000000000000000000084000000000000000000
      000084000000000000000000000084848400FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60084848400FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF000000000000000000FFFFFF000000FF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000000000000000000000000000
      00008400000084000000000000000000000000000000FFFFFF000000FF000000
      FF000000FF000000FF000000FF00000084000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000840000000000
      000084000000840000000000000084848400C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000FFFFFF000000
      FF000000FF000000FF000000FF00000000000000000000000000840000008400
      00008400000084000000840000008400000000000000FFFFFF000000FF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000000000008400
      0000840000008400000084000000848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000FFFFFF000000
      FF000000FF000000FF000000FF00000000000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      000084000000840000000000000084848400FFFFFF0084848400C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000008484
      8400840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000840000000000000000000000848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000008484
      8400FFFFFF008400000084000000840000008400000084000000840000008400
      0000FFFFFF0084000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000C6C6C600848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000084840000FFFF000084840000FFFF000084840000FFFF000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000000000000084
      840000FFFF000084840000FFFF00FFFFFF0000000000FFFFFF0000FFFF000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000000000000000000000084
      840000FFFF00FFFFFF0000FFFF0000000000000000000000000000FFFF00FFFF
      FF0000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF000000000000000000C6C6C600FFFFFF000000
      0000FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C6008484840000000000000000000000000000000000FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      00008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000C6C6C600000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      00008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000C6C6C60000000000FFFF
      FF00000000000000000084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000008400000084000000848484008484840084848400840000008400
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000000000000000000000000000C6C6
      C600000000000000000000000000000000008400000084000000000000000000
      00008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000848484008484840084848400FFFF0000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000084848400000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF000000000000000000C6C6C600FFFFFF000000
      0000FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000C6C6C600000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000C6C6C60000000000FFFF
      FF00000000000000000084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00000000000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF008484840084848400848484008484840084848400FFFFFF008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484008484
      84000000000000000000000000000000000000000000C6C6C600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C6008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000084848400FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000084848400C6C6C6000000
      0000848484008484840084848400848484008484840084848400848484008484
      840084848400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000084848400FFFFFF000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000084848400C6C6C6000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000084848400FFFFFF000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000000000000000000000000000C6C6
      C600000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000084848400C6C6C6000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400FFFFFF000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000000000000000000000000000C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000084848400FFFFFF000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400C6C6C60000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000084848400C6C6C6000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60084848400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF008484840084848400848484008484840084848400848484008484
      8400FFFFFF00C6C6C60084848400000000000000000084848400FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C6000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000C6C6
      C60000000000000000000000000000000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF008484840084848400848484008484840084848400FFFFFF008484
      8400848484008484840084848400000000000000000084848400840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF008484840000000000000000000000000084848400FFFFFF008400
      000084000000840000008400000084000000840000008400000084000000FFFF
      FF0084000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600848484000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400848484008484840000000000C6C6
      C60084848400C6C6C60000000000C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400848484008484840084848400FFFF
      FF0084848400FFFFFF0084848400FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C6000000000000000000A37600007F5B00007F5B00007F5B
      00004B4B4B004B4B4B00848484008484840084848400FFFFFF00FFFFFF00FFFF
      FF007F5B00007F5B00007F5B0000A3760000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C6000000000000000000000000000000000084848400FFFF
      FF0084000000840000008400000084000000C6C6C60000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A37600004B4B4B004B4B4B0084848400FFFFFF00FFFFFF00FFFF
      FF007F5B0000000000000000000000000000C6C6C60084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60000000000000000008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF008484000084840000848400008484000084840000848400008484
      0000FFFFFF00C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007F5B0000000000000000000000000000C6C6C6008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF0084848400FFFF
      FF0084000000840000008400000084000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007F5B0000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C60000000000000000000000000084848400FFFF
      FF00FFFFFF008484000084840000848400008484000084840000848400008484
      0000FFFFFF00C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFFDF00FFFF8000F7EF7000FFFF
      00007F5B0000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF0084848400FFFF
      FF0084000000840000008400000084000000C6C6C600C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C60000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFF8000FFFF8000FFFF8000FFFF
      80007F5B0000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C60000000000000000000000000084848400FFFF
      FF00FFFFFF008484000084840000848400008484000084840000848400008484
      0000FFFFFF00C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFF8000FFFF8000FFFF8000FFFF
      80007F5B0000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF00848484008484
      84008484840084848400848484008484840084848400C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C60000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000007F5B
      0000A3760000A3760000A37600004B4B4B00FFFF0000FFFF8000FFFF8000FFFF
      DF007F5B0000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000008484840000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0084848400C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C60000000000000000000000000084848400FFFF
      FF00FFFFFF008484000084840000848400008484000084840000848400008484
      0000FFFFFF00C6C6C6000000000000000000000000000000000000000000A376
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      0000A3760000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000008484840084848400848484008484
      840084848400848484008484840084848400FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C60000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C6008484840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C60000000000000000000000000084848400FFFF
      FF00FFFFFF008484000084840000848400008484000084840000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006F6F6F004B4B4B004B4B4B004B4B4B004B4B4B006F6F6F000000
      000000000000000000000000000000000000C6C6C60084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00C6C6C60000000000000000000000000000000000848484008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000C6C6C60000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600C6C6
      C600FFFFFF008484840000000000000000000000000000000000000000000000
      0000000000004B4B4B0000DF000000DF000000DF000000DF00004B4B4B000000
      000000000000000000000000000000000000C6C6C60084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      840084848400C6C6C60000000000000000000000000000000000848484008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000C6C6C60000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600848484000000000000000000000000000000000000000000000000000000
      0000000000006F6F6F004B4B4B004B4B4B004B4B4B004B4B4B006F6F6F000000
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840000000000000000000000000000000000000000007F5B0000000000007F5B
      00007F5B0000000000007F5B00007F5B0000000000007F5B00007F5B00000000
      00007F5B00007F5B00007F5B0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000FF000000FF000000FF00000000007F5B0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007F5B0000000000007F5B0000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000008400000084000000000000008400000084000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000007F000000000000000000000000000000000000000000000000
      00007F5B00007F5B00007F5B0000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000FFFF000000000000000000FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      840000000000000000000000FF0000000000000000007F5B0000000000000000
      000000000000007F0000007F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      8400000000000000FF000000FF000000FF00000000007F5B0000000000000000
      000000000000007F000000FF0000007F00000000000000000000000000000000
      0000848484007F5B000084848400000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      840000000000000000000000FF0000000000000000007F5B0000000000000000
      000000000000007F000000DF000000FF0000007F000000000000000000000000
      00007F5B00007F5B00007F5B0000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF0000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      840000000000000000000000FF00000000000000000000000000000000000000
      000000000000007F000000FF000000DF000000FF0000007F0000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      000000000000FFFFFF00000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      840000000000000000000000000000000000000000007F5B0000000000000000
      000000000000007F000000DF000000FF000000DF0000007F0000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000008484
      840000000000000000000000FF0000000000000000007F5B0000000000000000
      000000000000007F000000FF000000DF0000007F000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000C6C6
      C60000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000008400000084000000000000008400000084000000000000008484
      8400000000000000000000000000000000007F5B00007F5B00007F5B00000000
      000000000000007F000000FF0000007F00000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840000000000000000000000FF0000000000848484007F5B0000848484000000
      000000000000007F0000007F0000000000000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000007F5B00007F5B00007F5B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007F5B000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F5B0000000000007F5B00000000
      00007F5B00007F5B0000000000007F5B00007F5B0000000000007F5B00007F5B
      0000000000007F5B000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F5B00007F5B00007F5B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000081818100818181008181
      8100818181008181810081818100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000FF000000FF00000000000000000000000000
      000000000000FF0000000000000000000000818181007F5B00007F5B00007F5B
      00007F5B00007F5B000081818100818181000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00000000008484840084848400FF000000FF000000000000000000
      0000FF000000FF00000000000000000000007F5B0000FFFFCC00FFFFCC00FFFF
      CC00FFFFCC00FFFFCC007F5B0000818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF00FFFFFF0000000000FF000000FF000000FF00
      0000FF0000000000000000000000000000007F5B0000FFFFCC00FFFFFF00FFFF
      FF00FFFFFF00FFFFCC007F5B0000818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FF000000FF00
      0000000000000000000000000000000000007F5B0000FFFFCC00FFFFFF00FFFF
      FF00FFFFFF00FFFFCC007F5B0000818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000084848400848484008484840000000000FF000000FF00
      0000FF0000000000000000000000000000007F5B0000FFFFCC00FFFFFF00FFFF
      FF00FFFFFF00FFFFCC007F5B0000818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FF000000FF0000000000
      0000FF000000FF00000000000000000000007F5B0000FFFFCC00FFFFCC00FFFF
      CC00FFFFCC00FFFFCC007F5B0000818181000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000008484840084848400848484008484840000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000000000000000000000000000FF000000000000000000
      000000000000FF000000FF00000000000000818181007F5B00007F5B00007F5B
      00007F5B00007F5B000081818100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007F5B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007F5B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007F5B0000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400000000000000
      0000848484008484840084848400000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007F5B
      000000000000000000000000000000000000000000007F5B00007F5B00000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000084848400000000000000000084000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007F5B0000000000007F5B0000000000007F5B00007F5B00007F5B
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000000000
      0000848484000000000084848400000000008400000084000000840000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007F5B00007F5B00000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000008484
      8400000000000000000000000000848484008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007F5B0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007F0000007F
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      00004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007F000000FF
      2A00007F00000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C600000000000000000000000000000000000000
      00000000000000000000C6C6C600000000000000000000000000000000000000
      000084000000FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00840000000000000000000000000000000000000000000000000000000000
      0000C6C6C6008484840084848400848484008484840084848400848484008484
      840084848400848484004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007F000000DF
      000000FF2A00007F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      000084000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600840000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600848484004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007F000000FF
      2A0000DF000000FF2A00007F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00840000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C6000000FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600848484004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007F000000DF
      000000FF2A0000DF000000FF2A00007F00000000000000000000000000000000
      00000000000000000000C6C6C60000000000000000000000000000000000C6C6
      C600000000000000000000000000000000000000000000000000FF000000FF00
      000084000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C6004B4B4B0000000000818181004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B0081818100000000000000000000000000007F000000FF
      2A0000DF000000FF2A00007F0000000000000000000000000000000000000000
      00000000000000000000C6C6C60000000000000000000000000000000000C6C6
      C600000000000000000000000000000000000000000000000000FF000000FFFF
      FF0084000000C6C6C60084000000840000008400000084000000840000008400
      000084000000000000000000000000000000848484004B4B4B004B4B4B004B4B
      4B004B4B4B008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004B4B4B00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D004B4B4B00000000000000000000000000007F000000FF
      2A0000FF2A00007F000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000004B4B4B00D9A77D00D9A77D00D9A7
      7D00D9A77D004B4B4B0000000000000000000000000000000000000000000000
      0000000000004B4B4B0000000000000000004B4B4B00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D004B4B4B004B4B4B008181810000000000007F000000FF
      2A00007F000000000000000000000000000000000000C6C6C60000FFFF000000
      000000FFFF000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      0000000000000000000000000000000000004B4B4B00D9A77D00D9A77D00D9A7
      7D00D9A77D004B4B4B004B4B4B00848484000000000000000000000000000000
      00004B4B4B004B4B4B004B4B4B00000000004B4B4B00D9A77D00D9A77D00D9A7
      7D00D9A77D00D9A77D004B4B4B00D9A77D004B4B4B0000000000007F0000007F
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF00C6C6C60000FFFF00C6C6C600000000000000000000FFFF00000000000000
      0000000000000000000000000000000000008400000084000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      0000000000000000000000000000000000004B4B4B00A3760000A3760000A376
      0000A37600004B4B4B00D9A77D004B4B4B000000000000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B007F5B00007F5B00007F5B
      00007F5B00007F5B00004B4B4B00D9A77D004B4B4B004B4B4B00007F00000000
      00000000000000000000000000000000000000000000C6C6C60000FFFF00C6C6
      C60000FFFF008400000000FFFF000000000000000000C6C6C600000000000000
      00000000000000000000000000000000000084000000C6C6C600FF000000C6C6
      C600FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000840000008400000084000000840000004B4B4B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004B4B4B00D9A77D004B4B4B004B4B4B0084848400000000000000
      0000000000004B4B4B0000000000000000004B4B4B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF004B4B4B00D9A77D004B4B4B00D9A77D004B4B4B000000
      0000000000000000000000000000000000000000000000FFFF00C6C6C60000FF
      FF008400000000FFFF00C6C6C600000000000000000000FFFF00000000000000
      00000000000000000000000000000000000084000000FFFFFF00FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      000084000000840000008400000000000000848484004B4B4B004B4B4B004B4B
      4B004B4B4B0084848400A37600004B4B4B00D9A77D004B4B4B00000000000000
      0000000000004B4B4B000000000000000000818181004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B00818181007F5B00004B4B4B00D9A77D004B4B4B000000
      00000000000000000000000000000000000000000000C6C6C60000FFFF008400
      000000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C600000000000000
      00000000000000000000000000000000000084000000C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C6008400000000000000000000000000
      00008400000084000000840000008484840000000000000000004B4B4B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF004B4B4B00D9A77D004B4B4B00000000000000
      0000000000004B4B4B00000000000000000000000000000000004B4B4B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004B4B4B00D9A77D004B4B4B000000
      0000000000000000000000000000000000000000000000FFFF00C6C6C60000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00000000000000
      00000000000000000000000000000000000084000000FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF008400000000000000000000000000
      0000840000000000000000000000840000000000000000000000848484004B4B
      4B004B4B4B004B4B4B004B4B4B0084848400A37600004B4B4B00000000004B4B
      4B004B4B4B004B4B4B0000000000000000000000000000000000818181004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B00818181007F5B00004B4B4B000000
      00000000000000000000000000000000000000000000C6C6C60000FFFF008484
      8400000000000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000084000000C6C6C600840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000084848400840000000000000000000000000000000000
      00004B4B4B00FFFFFF00FFFFFF00FFFFFF00FFFFFF004B4B4B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004B4B4B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004B4B4B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000848484004B4B4B004B4B4B004B4B4B004B4B4B0084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000818181004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B00818181000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484000000
      0000848484008484840084848400000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00848484000000000000000000000000008484
      8400FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840084848400848484008484840084848400C6C6C600848484008484
      84008484840084848400C6C6C600848484000000000000000000000000000000
      000084848400FFFFFF008484840000000000FF000000FF000000FF0000000000
      000084848400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000084000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      000084848400FFFFFF0000000000FF000000FFFFFF00FF000000FF000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000084000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000848484000000
      0000848484000000000000000000000000008484840084848400000000000000
      0000000000008484840084848400000000000000000000000000000000000000
      000084848400FFFFFF0000000000FFFFFF00FF000000FF000000FF000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000084848400000000008484
      8400FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FF000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008484840000000000000000000000
      000084848400FFFFFF008484840000000000FFFFFF00FF000000FF0000000000
      000084848400FFFFFF00000000000000000000000000FF000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000FFFFFF000000
      00000000000000000000C6C6C6000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600FFFFFF00FFFFFF000000000000000000C6C6C600FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00848484000000000000000000000000008484
      8400FFFFFF00FFFFFF00000000000000000000000000FF000000FF000000FF00
      00000000000000FFFF0000FFFF0000FFFF000000000000000000840000008400
      0000000000000000000084000000840000000000000000000000FFFFFF000000
      00000000000000000000C6C6C6000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000084848400000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FF000000FF000000FF00
      00000000000000FFFF0000FFFF0000FFFF000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      8400000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000084848400000000008484
      8400C6C6C600000000000000000084848400FFFFFF00FFFFFF00FFFFFF008484
      8400848484008484840000000000000000000000000000000000FF000000FF00
      00000000000000FFFF0000FFFF00000000000000000000000000000000008400
      000084000000840000008400000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      000084848400C6C6C60000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008484
      8400FFFFFF00000000000000000000000000000000000000FF00000000000000
      0000000000000000000000000000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484
      840000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000084000000840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000084848400848484008484
      840000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000084000000840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000084000000840000000000000000000000000000000000000000
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
      0000000000008484840084000000840000008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000008400000084000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000840000008400000084000000840000000000FF000000
      FF000000FF000000FF000000000000000000000000000000FF00FFFFFF000000
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000008484840000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000840000008400000084000000840000000000FF000000
      FF000000FF0000000000000000000000000000000000000000000000FF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000848484000000
      000084848400C6C6C60000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848484008400000084000000840000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF00FFFFFF000000000000000000000000000000000084848400000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00C6C6C600FFFFFF00840000008400000084000000840000008400
      0000FFFFFF00C6C6C60000000000000000000000000084848400000000008484
      8400C6C6C6000000000000000000FFFFFF008484000000000000000000008484
      8400000000000000000084848400000000000000000000000000000000000000
      00000000000084848400000000000000000000000000848484000000FF000000
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000000000000000
      00000000FF0000000000FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C6000000000000000000000000008484840000000000C6C6
      C600848484000000000084848400FFFFFF008484000084840000848400000000
      0000848484008484840000000000848484000000000000000000000000000000
      00008484840000000000FFFFFF00FFFFFF00FFFFFF00000000000000FF000000
      FF00FFFFFF00C6C6C600FFFFFF00000000008484840000000000000000000000
      000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF008400000084000000840000008400000084000000840000008400
      0000FFFFFF00C6C6C60000000000000000000000000000000000000000008484
      84000000000084848400FFFFFF0084840000FFFF0000FFFF0000848400008484
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00C6C6C600FFFFFF00C6C6C6000000000000000000000000008484840000FF
      FF0000000000FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF000000000000FF
      FF0000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      000000000000FFFFFF0084840000FFFF00008484000084840000FFFF0000FFFF
      0000848400008484000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF00C6C6C600FFFFFF000000000000000000FFFFFF00000000008484
      840000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600000000008484
      840000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00C6C6C600FFFFFF00840000008400000084000000840000008400
      0000FFFFFF00C6C6C6000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFF0000FFFFFF00FFFF000084840000FFFF0000848400008484
      0000FFFF0000FFFF000000000000848484000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008484
      8400848484008484840084848400848484000000000000FFFF000000000000FF
      FF008484840000000000C6C6C600FFFFFF00C6C6C600000000008484840000FF
      FF0084848400000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      000000000000FFFF000084848400FFFFFF00FFFFFF00FFFF000084840000FFFF
      000084840000FFFF000000000000848484000000000000000000000000000000
      00000000FF000000000000000000FFFFFF00FFFFFF0000000000848484000000
      00000000000000000000000000000000000000000000FFFFFF00848484000000
      000000FFFF00848484000000000000000000000000008484840000FFFF008484
      840000FFFF00000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00C6C6C600FFFFFF00840000008400000084000000840000008400
      0000FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      000000000000FFFF0000848484008484840084848400FFFFFF00FFFF0000FFFF
      0000FFFF00000000000084848400FFFFFF000000000000000000000000000000
      FF00FFFFFF000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF000000
      00008484840000FFFF008484840000FFFF008484840000FFFF008484840000FF
      FF008484840000FFFF000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      000000000000FFFF00008484840000000000000000000000000000000000FFFF
      0000000000008484840084848400FFFFFF0000000000000000000000FF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF000000000000000000000000008484840000FFFF008484840000FFFF008484
      840000FFFF00848484000000000000000000000000000000000084848400FFFF
      FF00FFFFFF008400000084000000840000008400000084000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF000084848400000000000000000000000000000000000000
      00008484840084848400FFFFFF0000000000000000000000FF00FFFFFF000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600C6C6
      C600FFFFFF008484840000000000000000000000000000000000000000000000
      000000000000FFFF000084848400000000000000000000000000848484000000
      000084848400FFFFFF00000000000000000000000000FFFFFF00000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600848484000000000000000000000000000000000000000000000000000000
      000000000000FFFF00008484840000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      00000000000000000000000000000084840000FFFF0000848400000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C60000000000C6C6
      C60000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000084840000FFFF000084840000FFFF000084840000FFFF000084
      840000000000000000000000000000000000000000000000000000000000C6C6
      C600000000000000000000000000C6C6C60000000000C6C6C60000000000C6C6
      C60000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF00000084000000FF000000
      84000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000008400000084000000000000000000000000000000000000000084
      840000FFFF000084840000FFFF00FFFFFF0000000000FFFFFF0000FFFF000084
      840000FFFF00008484000000000000000000000000000000000000000000C6C6
      C600C6C6C60000000000C6C6C600C6C6C60000000000C6C6C60000000000C6C6
      C60000000000C6C6C60000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      000000000000840000008400000000000000000000000000000000FFFF000084
      840000FFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF0000FFFF000084840000FFFF0000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      000000000000840000008400000000000000000000000000000000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C6000000FF0000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000008400000084000000000000000000000000000000840000008400
      000000000000000000000000000084848400000000000000000000000000C6C6
      C600C6C6C60000000000C6C6C600C6C6C60000000000C6C6C600000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C6000000FF0000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008400
      0000840000000000000000000000848484008484840084848400000000000000
      000084000000840000000000000084848400000000000000000000000000C6C6
      C6000000000000000000C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000840000008400000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      000000000000FFFF0000FFFF0000848484008484840084848400FFFF0000FFFF
      000000000000000000008484840000000000000000000000000000000000C6C6
      C600C6C6C60000000000C6C6C600C6C6C60000000000C6C6C600000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000008400
      0000840000000000000000000000000000008400000084000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000FFFF0000FFFF0000FFFF0000000000000000
      0000FFFFFF00FFFFFF008484840000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008400
      000084000000840000008400000084000000840000008400000084000000FFFF
      FF0084000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400000000000000000000000000000000000000
      00000000000084848400C6C6C600C6C6C6000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FF
      FF00C6C6C60000FFFF000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000000000000000000000000
      00000000000000000000C6C6C600C6C6C6008400000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00C6C6C60000FFFF0000000000000000000000
      000000FFFF00C6C6C6000000000000000000000000008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600FFFFFF008484840000000000000000000000
      00000000000000000000C6C6C600C6C6C6008400000084000000000000000000
      0000FFFFFF0084848400848484008484840084848400FFFFFF0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000848484000000
      000000000000FFFFFF00C6C6C60000FFFF00C6C6C60000FFFF000000000000FF
      FF00C6C6C60000FFFF000000000000000000000000008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600FFFFFF008484840000000000848484000000
      0000000000000000000000000000848484008400000084000000840000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000FFFF00C6C6C60000FFFF00C6C6C60000000000C6C6
      C60000FFFF00C6C6C6000000000000000000000000008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600FFFFFF008484840000000000000000008484
      8400000000000000000084848400000000008400000084000000840000000000
      00000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF0000000000FF00
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00C6C6C60000FFFF00C6C6C600000000000000000000FF
      FF00C6C6C60000FFFF000000000000000000000000008484840000000000C6C6
      C600C6C6C600C6C6C600C6C6C600FFFFFF008484840000000000000000000000
      0000000000000000000000000000000000008400000084000000FF00FF000000
      00000000FF000000FF000000FF000000FF000000FF000000FF0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000000000000840000000000000000000000848484000000000000000000FFFF
      FF0000000000FFFFFF0000FFFF00C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000848484000000
      0000000000000000000000000000848484000000000000000000000000000000
      00000000000000000000000000000000000084000000FF00FF00FF00FF000000
      0000FFFFFF008484840084848400FFFFFF00FFFFFF00FFFFFF0000000000FF00
      FF00FF00FF000000000000000000FF00FF000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000008484
      8400848484008484840084848400000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FF00
      FF00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000FFFF0000000000000000000000000000FFFF00C6C6C60000FFFF00C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000084000000
      FF0000008400000000000000000000000000FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FF00FF00FF00
      FF00FF00FF000000000000000000FF00FF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000FFFF00C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000FF000000FF000000
      FF000000FF00000084000000000000000000FF00FF00FF00FF00FF00FF000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000FF00FF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000084848400000000000000
      000084848400000000000000000000000000FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000FF00FF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00C6C6C6000000
      00000000000000000000C6C6C60000FFFF00C6C6C60000FFFF00000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      000000000000848484000000000000000000FFFFFF000000FF000000FF000000
      FF000000FF00000084000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00C6C6
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000FF00FF000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000008484
      8400FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000008484
      8400C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000008484840000000000000000008484
      8400FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFF00008484
      8400C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C6000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000FF000000000084848400FFFFFF00FFFF0000FFFFFF00FFFF
      0000FFFFFF00FFFF0000FFFFFF00000000008400000084000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00000000000000000084848400FFFF0000FFFFFF008484
      8400FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000000000000000000000000000084848400FFFF000000000000000000000000
      00000000000000000000FFFF0000000000008400000084000000840000000000
      000084848400FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFF00008484
      8400C6C6C600FFFFFF00C6C6C600FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000000000000
      000084848400000000000000000084848400FFFFFF00FFFF0000FFFFFF00FFFF
      0000FFFFFF00FFFF0000FFFFFF00000000008400000084000000840000000000
      000084848400FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000084848400FFFF0000FFFFFF008484
      8400FFFFFF00C6C6C600FFFFFF00C6C6C60000000000C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00C6C6C60084848400FFFF000000000000000000000000
      00000000000000000000FFFF0000000000008400000084000000000000000000
      000084848400FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFF00008484
      8400C6C6C600FFFFFF00C6C6C600FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400C6C6C600FFFFFF0084848400FFFFFF00FFFF0000FFFFFF00FFFF
      0000FFFFFF00FFFF0000FFFFFF00000000008400000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFF0000FFFFFF008484
      8400848484008484840084848400848484008484840000000000000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      000084848400FFFFFF00C6C6C60084848400FFFF0000FFFFFF00FFFF0000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFF0000FFFF
      FF00FFFF000000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      000084848400C6C6C600FFFFFF0084848400FFFFFF00FFFF0000FFFFFF00FFFF
      000000000000C6C6C60000000000000000000000000000000000000000000000
      000084848400FFFFFF0084848400848484008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFF0000FFFFFF00FFFF
      0000FFFFFF0000000000000000000000000000000000000000000000FF000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      000084848400FFFFFF00C6C6C60084848400FFFF0000FFFFFF00FFFF0000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000084848400C6C6C600FFFFFF00848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00C6C6C600FFFFFF00C6C6C60000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000084848400C6C6C600FFFFFF00C6C6C600FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF00FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600848484008484840084848400848484008484840084848400848484008484
      84008484840084848400848484004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF00000000000000000000FF
      FF00FFFFFF00000000000000000000FFFF000000000000000000000000000000
      000000000000FFFFFF0084000000FFFFFF0084000000FFFFFF00848484000000
      0000840000008400000084000000840000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00848484000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000C6C6C6000000FF000000FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600848484004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00848484000000
      00000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00848484000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00848484000000
      0000000000000000FF000000FF0000000000000000004B4B4B004B4B4B004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B000000000000000000000000000000
      0000000000004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF008400000000000000000000000000
      000000000000FFFFFF0084000000FFFFFF0084000000FFFFFF00848484000000
      0000000000000000FF000000FF00000000004B4B4B00DFFFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF00DFFFFF004B4B4B0000000000000000000000
      00004B4B4B004B4B4B004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000004B4B4B0080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0060DFDF0080FFFF004B4B4B0000000000000000004B4B
      4B004B4B4B004B4B4B004B4B4B004B4B4B000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF008400000084000000840000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000FF000000FF00000000004B4B4B0080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF004B4B4B0000000000000000000000
      0000000000004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004B4B4B0000FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF004B4B4B0000000000000000000000
      0000000000004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF00000000004B4B4B004B4B4B004B4B4B004B4B
      4B0010BFCF0080FFFF0080FFFF00DFFFFF004B4B4B0000000000000000000000
      0000000000004B4B4B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004B4B4B0000FFFF00FFFFFF0000CF
      CF004B4B4B004B4B4B004B4B4B004B4B4B00000000004B4B4B004B4B4B004B4B
      4B004B4B4B008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000004B4B4B004B4B4B004B4B
      4B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000008484840000FFFF000000000084848400C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      000000000000848484000000000000000000C6C6C60000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C600000000000000000000000000000000008484
      84000000000000000000FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000084848400840000008400000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000FFFFFF000000
      00000000000000000000C6C6C60000FFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000084848400000000000000
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF00000000000000000000848484000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      FF00FF000000FFFF0000FF000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084848400000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      00008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000008484840000FFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF0000FF000000FFFF0000000000000000000000000000000000000000
      0000000000000000000084848400840000008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      FF00FF000000FFFF0000FF000000000000000000000000000000000000000000
      0000000000000000000084848400840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF0000FF000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000848484008400000084000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000FFFF00000000FF008484840084848400FFFFFF0084848400848484000000
      FF00FF000000FFFF0000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      0000848484000000FF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF00000000
      FF0084848400FF000000FFFF0000000000000000000000000000000000000000
      0000000000000000000084000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00C6C6C600000000000000000000000000848484008484
      8400FFFFFF00FFFF00000000FF00FFFF0000FFFFFF00FFFF00000000FF00FFFF
      0000FFFFFF008484840084848400000000000000000000000000000000000000
      0000000000000000000084848400840000008400000084000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C600000000000000000000000000FFFF0000FFFF
      FF00FFFF0000FFFFFF00FFFF00000000FF000000FF000000FF00FFFF0000FFFF
      FF00FFFF0000FFFFFF00FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000084848400000000000000
      0000FFFFFF00FFFF00000000FF00FFFFFF00FFFF0000FFFFFF000000FF00FFFF
      0000FFFFFF000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000848484008400000084000000848484000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000FFFFFF0084000000FFFFFF00000000000000000000000000000000008484
      84000000000000000000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      000000000000848484000000000000000000FFFF000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008400000084000000848484000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000000000
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
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF008484840000000000848484000000000000FFFF0000000000848484000000
      000084848400FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF0084000000840000008400000084000000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00C6C6
      C6000000000000FFFF00848484000000000000FFFF00000000008484840000FF
      FF0000000000C6C6C600FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000848484000000000084848400000000008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000848484008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008484
      84008484840000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000848484000000000000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00000000000000000084848400FFFFFF0084848400FFFF
      FF0084000000840000008400000084000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000FFFFFF008484
      8400000000000000000000FFFF0084848400000000008484840000FFFF000000
      00000000000084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF0000000000FFFFFF0000FFFF000000
      000000FFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000848484008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000FFFFFF000000
      000000FFFF0000FFFF0000FFFF0000000000FFFFFF000000000000FFFF0000FF
      FF0000FFFF0000000000FFFFFF00000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000FF000000FF000000000000000000FFFFFF0000FFFF000000
      000000FFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000000084848400FFFFFF0084848400FFFF
      FF0084000000840000008400000084000000C6C6C600C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C600000000000000000000000000FFFFFF008484
      8400000000000000000000FFFF0084848400000000008484840000FFFF000000
      00000000000084848400FFFFFF00000000000000000000000000000000008400
      0000FFFF0000FFFF00000000FF00FFFF0000FFFF000084000000000000000000
      0000000000000000FF000000FF000000000000000000FFFFFF0000FFFF000000
      000000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840000FFFF0084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000FFFFFF00FFFF
      FF00848484008484840000FFFF0000FFFF0000FFFF0000FFFF0000FFFF008484
      840084848400FFFFFF00FFFFFF00000000000000000000000000000000008400
      0000FFFF0000FFFF00000000FF00FFFF0000FFFF000084000000000000000000
      0000000000000000FF000000FF00000000008484840000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00848484008484
      84008484840084848400848484008484840084848400C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C600000000000000000000000000FFFFFF00FFFF
      FF000000000000FFFF00848484000000000000FFFF00000000008484840000FF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000008400
      0000FFFF0000FFFF00000000FF00FFFF0000FFFF000084000000000000000000
      FF000000FF000000FF0084848400000000000000000084848400000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0084848400C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000FFFFFF00FFFF
      FF008484840000000000848484000000000000FFFF0000000000848484000000
      000084848400FFFFFF00FFFFFF00000000000000000000000000000000008400
      0000FFFF0000FFFF00000000FF00FFFF0000FFFF000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600C6C6C600000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000848484008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000084848400840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000C6C6C600000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000008484840084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000C6C6C600000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000000000000000000000000000
      000000000000848484000000000000000000C6C6C60000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      00000000000000FFFF00FF000000FF000000FF000000FF000000FF00000000FF
      FF00FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000084848400848484008484840084848400848484008484
      84008484840084848400FFFFFF00848484000000000000000000000000008484
      84000000000000000000FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000084848400000000000000
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF00000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FF000000FF000000FF000000FF000000FF00000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF000000FF00C6C6C600FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      FF00FF000000FFFF0000FF00000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF000000FF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF0000FF000000FFFF000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF000000FF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000FFFFFF00FFFF0000FF0000000000
      FF00FF000000FFFF0000FF00000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF000000FF000000FF000000FF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000FFFF00000000FF00FFFF0000FFFF0000C6C6C600FF000000FFFF00000000
      FF00FFFF0000FF000000FFFF000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF000000FF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000FFFF00000000FF008484840084848400FFFFFF0084848400848484000000
      00000000000000000000FF00000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF000000FF00FFFFFF000000
      FF00FFFFFF0084848400FFFFFF00848484000000000000000000FFFF0000FFFF
      0000848484000000FF00FFFF0000FFFFFF00FFFF0000FFFFFF008484840000FF
      000000FF000000000000FFFF000000000000000000000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00C6C6C6000000FF00C6C6
      C600FFFFFF008484840084848400848484000000000000000000848484008484
      8400FFFFFF00FFFF00000000FF00FFFF0000FFFFFF00848484008484840000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000FFFF0000FFFF
      FF00FFFF0000FFFFFF00FFFF00000000FF008484840000FF000000FF000000FF
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      000000000000000000008484840000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000FFFFFF008484840084848400848484008484
      8400848484008484840000000000000000000000000084848400000000000000
      0000FFFFFF00FFFF00000000FF00FFFFFF008484840000FF000000FF000000FF
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000000000000000FFFFFF00FFFF000084848400848484008484840000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848484000000000000000000FFFF0000000000008484840000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      00000000000000FFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF0000FFFF0084848400C6C6C600FFFFFF00C6C6
      C60000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF0084848400FFFFFF00C6C6C6000000
      00000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00000000000000000000000000000000000084848400C6C6C600FFFFFF000000
      0000FFFFFF000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF000000000084848400C6C6C600FFFFFF00C6C6
      C60000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF0000000000FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00C6C6C6000000
      00000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF000000000084848400C6C6C600FFFFFF000000
      0000FFFFFF000000000000000000C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF000000
      000000000000000000000000000000FFFF0084848400C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C6000000000000000000000000000000000000000000FFFF
      FF0084000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FFFFFF000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0084000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FFFFFF00000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFF
      FF00FF000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000FFFFFF000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      840084848400848484008484840000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000626262000000000000000000000000000000000000000000D9A77D000000
      0000D9A77D000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000062626200626262000000000000000000FFFF9900D9A77D00D9A77D000000
      0000D9A77D00D9A77D00FFFF990000000000000000000000000084848400FFFF
      FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000062626200626262006262
      620062626200626262006262620000000000D9A77D00FFFF9900D9A77D000000
      0000D9A77D00FFFF9900D9A77D0000000000000000000000000084848400FFFF
      FF008484840084848400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C6008484840084848400C6C6C600000000000000000000000000000000008484
      840000000000000000000000000000000000FFFFFF00C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000062626200000000000000
      0000626262006262620000000000D9A77D00D9A77D00D9A77D00FFFF99000000
      0000FFFF9900D9A77D00D9A77D00D9A77D00000000000000000084848400FFFF
      FF008484840084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008484840084848400FFFFFF00000000000000000000000000000000008484
      840000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000062626200000000000000
      0000626262000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000062626200000000000000
      0000000000000000000000000000D9A77D00D9A77D00D9A77D00FFFF99000000
      0000FFFF9900D9A77D00D9A77D00D9A77D00000000000000000084848400FFFF
      FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      000000000000FFFF0000FFFF0000FFFF00000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9A77D00FFFF9900D9A77D000000
      0000D9A77D00FFFF9900D9A77D0000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF9900D9A77D00D9A77D000000
      0000D9A77D00D9A77D00FFFF9900000000000000000084000000C6C6C6008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400848484000000000000000000000000008484
      840000000000000000000000000000000000FFFFFF00C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000008484840000000000FFFFFF00FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000008484840062626200626262006262
      6200626262006262620062626200626262008484840000000000D9A77D000000
      0000D9A77D000000000000000000000000008400000084000000840000000000
      0000000000000000000084848400000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF0084840000FFFF
      0000FFFF0000FFFF000084848400000000000000000000000000000000000000
      00000000000000000000000000000000000062626200DFFFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF00DFFFFF006262620000000000000000000000
      0000000000000000000000000000000000008400000000000000840000008400
      0000000000000000000000000000848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484000084840000FFFF
      0000FFFF0000FFFF000084848400FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000006262620080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0060DFDF0080FFFF006262620000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084840000FFFFFF00FFFF
      0000FFFF0000FFFF000084848400848484008484840084848400848484000000
      0000000000000000000000000000000000006262620080FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF006262620000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00848400008484
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000006262620000FFFF0080FFFF0080FF
      FF0080FFFF0080FFFF0080FFFF0080FFFF006262620000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000008484840000000000848400008484
      0000FFFFFF00FFFF0000FFFF0000FFFF00000000000084848400000000000000
      0000000000000000000000000000000000006262620062626200626262006262
      620029D8E80080FFFF0080FFFF00DFFFFF006262620000000000000000000000
      0000000000000000000000000000000000008400000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000008484840000000000FFFF
      FF00848400008484000000000000000000008484840000000000000000000000
      0000000000000000000084848400000000006262620000FFFF00FFFFFF0019E8
      E800626262006262620062626200626262008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000062626200626262006262
      6200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000084000000FF00000084000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484000000
      0000000000000000000084848400000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C6000000000000000000C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00000000008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00C6C6C6000000000000000000FFFF
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF0000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C600FFFFFF00C6C6C6000000000000000000C6C6
      C600FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF0084848400000000000000000000FF
      FF00FFFFFF008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000848484000000
      0000000000000000000084848400000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF0084848400FFFFFF00000000000000
      00000000000084848400FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF0000008400000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF0084848400848484008484840000FF
      FF00FFFFFF008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000FF000000FF000000FF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084848400FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      000084848400FFFFFF000000FF000000FF000000FF0000000000FFFFFF0000FF
      FF00FFFFFF0084848400848484008484840000000000FFFFFF00C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000084000000840000008400000084000000000000000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000000000000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000084000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D9A77D00A376
      0000A3760000A3760000D9A77D000000000000000000D9A77D00A3760000A376
      0000A3760000D9A77D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004B4B4B004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A3760000D9A7
      7D00CB8C4400CB8C4400A37600000000000000000000A3760000FF9F7F00CB8C
      4400CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000CB8C4400A65400006336000063360000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CB8C4400A65400006336000063360000A654000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000000DF000000BF2A004B4B4B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FF9F
      7F00D9A77D00CB8C4400A37600000000000000000000AA7F5500FF9F7F00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00CB8C4400CB8C4400CB8C44006336
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00CB8C4400CB8C4400CB8C440063360000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000000DF000000DF000000BF2A004B4B4B0000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00CB8C4400CB8C4400A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00CB8C4400CB8C4400A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000000DF000000DF000000DF000000BF2A004B4B4B00000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00D9A77D00CB8C4400A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00D9A77D00CB8C4400A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000000DF000000FF090000DF000000DF000000BF2A004B4B4B000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00D9A77D00D9A77D00A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00D9A77D00D9A77D00A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000000FF2A0000DF000000FF090000DF000000DF000000BF2A004B4B
      4B00000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      0000000000000000000000000000A3760000FFFFCC00FFFFCC00FFFFCC00FFFF
      CC00A37600000000000000000000000000000000000000000000000000000000
      000000000000A3760000FFFFCC00FFFFCC00FFFFCC00FFFFCC00A37600000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000055FF000000FF2A0000DF000000FF090000DF000000DF000000BF
      2A004B4B4B000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3760000A3760000A3760000A376
      0000CB8C44000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000A3760000A3760000A3760000CB8C44000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F000055FFAA0000FF090000FF2A0000DF000000FF090000DF000000DF
      0000007F00000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000001FFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000001FFF000000000000000000000000000000000000000000000000000000
      0000007F000055FFAA0000FF090000FF090000FF2A0000DF000055FFAA00007F
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000001FFF00001FFF00001F
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000001F
      FF00001FFF00001FFF0000000000000000000000000000000000000000000000
      0000007F0000AAFF2A0000FF090000FF090000FF090000FF2A00007F00000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001FFF00001FFF00001FFF00001F
      FF00001FFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000001FFF00001F
      FF00001FFF00001FFF00001FFF00000000000000000000000000000000000000
      0000007F0000AAFF2A0000FF090000FF0900AAFF2A00007F0000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000001FFF000000
      00000000000000000000000000000000000000000000001FFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000001FFF000000000000000000000000000000000000000000000000000000
      0000007F0000AAFFAA0000FF0900AAFF2A00007F000000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007F0000AAFFAA00AAFFAA00007F00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00D9A77D00CB8C4400A37600000000000000000000AA7F5500FFFFCC00D9A7
      7D00CB8C4400A376000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000001FFF00000000000000
      0000000000000000000000000000000000000000000000000000001FFF000000
      000000000000000000000000000000000000000000000000000000000000001F
      FF00000000000000000000000000000000000000000000000000000000000000
      0000007F0000AAFFAA00007F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7F5500FFFF
      CC00FFFFCC00D9A77D00A37600000000000000000000AA7F5500FFFFCC00FFFF
      CC00FF9F7F00A376000000000000000000000000000000000000000000000000
      000000000000001FFF0000000000001FFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000001FFF000000000000000000001FFF0000000000001FFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D9A77D00AA7F
      5500AA7F5500AA7F5500D9A77D000000000000000000D9A77D00AA7F5500AA7F
      5500AA7F5500D9A77D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C600000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000084848400FFFFFF00C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C6000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000FFFFFF00C6C6
      C600000000000000000000000000000000000000000084848400FFFFFF00C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C6000000000084848400FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000FFFFFF00C6C6
      C60000000000C6C6C600C6C6C600000000000000000084848400FFFFFF00C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C6000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000FFFFFF00C6C6
      C60000000000FFFFFF00C6C6C600000000000000000084848400FFFFFF00C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6
      C600000000000000000000000000000000008484840000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C6000000000084848400FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000FFFFFF00C6C6
      C60000000000FFFFFF00C6C6C600000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C6000000000000000000000000000000000084848400FFFFFF00C6C6C600C6C6
      C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C6C6C6000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00C6C6
      C60000000000FFFFFF00C6C6C600000000000000000084848400840000008400
      000084000000840000008400000084000000840000008400000000000000C6C6
      C60000000000C6C6C600000000000000000084848400FFFFFF00C6C6C600C6C6
      C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000084848400FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C600000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400FFFFFF0000000000FFFFFF00FFFFFF00C6C6
      C60000000000FFFFFF00C6C6C600000000000000000084848400848484008484
      840084848400848484008484840084848400848484008484840000000000C6C6
      C60000000000C6C6C600000000000000000084848400FFFFFF00C6C6C600C6C6
      C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084848400FFFFFF00000000000000000084848400FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFF
      FF00C6C6C600FFFFFF00000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008484840000000000FFFFFF0000000000000000000000
      000000000000FFFFFF00C6C6C600000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C600000000000000000084848400FFFFFF00C6C6C600C6C6
      C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008484840000000000000000000000000084848400FFFFFF00FFFFFF00C6C6
      C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6C600FFFFFF00C6C6
      C600FFFFFF00C6C6C60000000000000000008484840084848400848484008484
      8400848484008484840084848400FFFFFF00FFFFFF0084848400FFFFFF000000
      0000FFFFFF00FFFFFF00C6C6C600000000000000000000000000000000008484
      8400840000008400000084000000840000008400000084000000840000008400
      000000000000C6C6C600000000000000000084848400FFFFFF00C6C6C600C6C6
      C600C6C6C600C6C6C60084848400848484008484840084848400848484008484
      84008484840000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      840000000000C6C6C600000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000008484840084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400FFFFFF00FFFF
      FF0084848400FFFFFF0000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00840000008400
      000084000000840000008400000084000000840000008400000084000000FFFF
      FF0084000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484000000000000000000000000000000000000000000000000000000
      0000000000008484840084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008484840084000000840000008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A8A8A800666666003D3D3D003D3D
      3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D
      3D003D3D3D00A7A7A7000000000000000000A8A8A800666666003D3D3D003D3D
      3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D
      3D003D3D3D00A7A7A70000000000000000000000000000000000000000000000
      0000000000000000000000000000303030003030300030303000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006666660055DFD4003D3D3D0055DF
      FF0055DFFF0055DFFF0055DFFF0055DFFF0055DFD40055DFFF0055DFD40055DF
      FF0055C0D4003D3D3D0000000000000000006666660055DFD4003D3D3D0055DF
      FF0055DFFF0055DFFF0055DFFF0055DFFF0055DFD40055DFFF0055DFD40055DF
      FF0055C0D4003D3D3D0000000000000000000000000000000000000000000000
      000000000000303030003030300000A0AB0000A0AB0000A0AB00303030000000
      000000000000000000000000000000000000C6C6C60084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000006666660000F2FF0055DFD4003D3D
      3D00A9FFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DF
      D40055DFFF0055DFD4003D3D3D00000000006666660000F2FF0055DFD4003D3D
      3D00A9FFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DFFF0055DF
      D40055DFFF0055DFD4003D3D3D00000000000000000000000000000000003030
      30003030300000A0AB0000A0AB0000F2FF0000F2FF0000F2FF0000A0AB003030
      300000000000000000000000000000000000FFFFFF00C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400000000000000000000000000000000007373730054FFFF0000F2FF0055DF
      D4003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D
      3D003D3D3D003D3D3D003D3D3D003D3D3D007373730054FFFF0000F2FF0055DF
      D4003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D3D003D3D
      3D003D3D3D003D3D3D003D3D3D003D3D3D0000000000303030003030300000A0
      AB0000A0AB0000F2FF0000F2FF00633600006336000000A0AB0000F2FF0000A0
      AB0030303000000000000000000000000000FFFFFF00C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400000000000000000000000000000000007373730054FFFF0054FFFF0000F2
      FF0055DFD40000F2FF0055DFD40099F8FF0099F8FF0099F8FF0099F8FF0099F8
      FF0099F8FF0099F8FF00B4B4B400000000007373730054FFFF0054FFFF0000F2
      FF0055DFD40000F2FF0055DFD400FFFFFF0098F7FF0099F7FF0098F7FF0098F7
      FF0098F7FF0098F7FF0098F7FF00B4B4B4006336000000A0AB0000A0AB0000F2
      FF0000F2FF00633600007F5B0000D9A77D00D9A77D006336000000A0AB0000F2
      FF0000A0AB00303030000000000000000000FFFFFF00C6C6C6000000FF000000
      FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6008484
      84000000000000000000000000000000000081818100A9FFFF0054FFFF0054FF
      FF0000F2FF00EFAD00007F5B0000EFAD0000AAFFFF0099F8FF00AAFFFF0099F8
      FF00AAFFFF0099F8FF00767676000000000081818100A9FFFF0054FFFF0054FF
      FF0000F2FF0055DFD400B4B4B400FFFFFF0099F8FF00AAFFFF0099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF00B4B4B4006336000000F2FF0000F2FF006336
      00007F5B0000BC720000BC720000BC720000BC720000D9A77D006336000000A0
      AB0000F2FF0000A0AB003030300000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6
      C600000000000000000000000000000000008181810054FFFF00A9FFFF0054FF
      FF0054FFFF007F5B0000D9A77D007F5B0000FFFFFF00AAFFFF0099F8FF00AAFF
      FF0099F8FF00AAFFFF0076767600000000008181810054FFFF00A9FFFF008181
      8100737373005959590059595900595959005959590073737300AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF0076767600633600006336000063360000BC72
      0000BC720000BC72000000F2FF00BC720000BC720000BC720000D9A77D006336
      000000A0AB0000F2FF0000A0AB00303030000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9A00A9FFFF0054FFFF00EFAD
      0000A2760000A2760000D9A77D00A37700007F5B0000EFAD0000AAFFFF0099F8
      FF00AAFFFF0099F8FF0076767600000000009A9A9A00A9FFFF0054FFFF008181
      8100005FFF00001FFF00001FFF00001FFF00001FFF005959590099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF0076767600633600007F5B0000A3760000BC72
      0000BC720000BC72000068F5FF00BC720000BC720000BC720000BC720000D9A7
      7D006336000000A0AB0030303000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9A0054FFFF00A9FFFF00AA7F
      0000FFFFCC00D9A77D00D9A77D00D9A77D00D9A77D007F5B0000AAFFFF00AAFF
      FF0099F8FF00AAFFFF0081818100000000009A9A9A0054FFFF00A9FFFF008181
      81001D94F7001D94F700005FFF00001FFF00001FFF0059595900AAFFFF0099F8
      FF00AAFFFF0099F8FF00AAFFFF00818181000000000063360000FF9F2A00D9A7
      7D00BC720000BC720000BC720000BC72000000F2FF0000F2FF00BC720000BC72
      0000D9A77D006336000030303000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000000000A7A7A700A7A7A70055C0D400F7D0
      6C00E5B72600E2B62900F6CF6D00AA7F0000AA7F0000F7CF6C00AAFFFF0099F8
      FF00AAFFFF0099F8FF008E8E8E0000000000A7A7A700A7A7A70000F2FF00A7A7
      A7009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A00A7A7A70099F8FF00AAFF
      FF0099F8FF00AAFFFF0099F8FF008E8E8E00000000000000000063360000FF9F
      2A00D9A77D00BC720000BC720000BC720000BC72000068F5FF0000F2FF0000F2
      FF00BC720000D9A77D0030303000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000000000000000A7A7A70000F2FF0000F2
      FF0000F1FF00F1BF2B00FFFFCC00AA7F0000AAFFFF00AAFFFF00AAFFFF0055DF
      FF0055DFFF0055C0D4009A9A9A000000000000000000A7A7A70000F2FF0000F2
      FF0000F2FF0000F2FF00B4B4B400FFFFFF00AAFFFF00AAFFFF00AAFFFF0099F8
      FF0055DFFF0055DFFF0055C0D4009A9A9A000000000000000000000000006336
      0000FFFF7F00D9A77D00BC72000068F5FF00BC720000BC72000000F2FF0000F2
      FF00BC720000BC720000D9A77D00303030000000000000000000848484000000
      000000000000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000000000000000009B9B9B0054FFFF0067F4
      FF0067F4FF00F8D06D00FDC83100F7CF6C00AAFFFF00AAFFFF00B4B4B400A0A0
      A0008D8D8D0081818100A7A7A70000000000000000009B9B9B0054FFFF0067F4
      FF0067F4FF0067F4FF0091919100FFFFFF00AAFFFF00AAFFFF00AAFFFF00B4B4
      B400A0A0A0008D8D8D0081818100A7A7A7000000000000000000000000000000
      000063360000FFFF7F00D9A77D00BC72000068F5FF0000F2FF0000F2FF00BC72
      0000D9A77D00FF9F2A007F5B0000633600008484840000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000000000000000000000009B9B9B008D8D
      8D008D8D8D00B4B4B40090909000FFFFFF00AAFFFF00FFFFFF00A1A1A100E6E6
      E600DADADA00DADADA00B4B4B4000000000000000000000000009B9B9B008D8D
      8D008D8D8D00B4B4B40090909000FFFFFF00FFFFFF00AAFFFF00AAFFFF00A1A1
      A100E6E6E600DADADA00DADADA00B4B4B4000000000000000000000000000000
      00000000000063360000FFFF7F00D9A77D00BC720000BC720000D9A77D00FF9F
      2A007F5B00006336000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000091919100FFFFFF00FFFFFF00AAFFFF00A7A7A700FFFF
      FF00E7E7E700B4B4B40000000000000000000000000000000000000000000000
      0000000000000000000091919100FFFFFF00AAFFFF00FFFFFF00AAFFFF00A7A7
      A700FFFFFF00E7E7E700B4B4B400000000000000000000000000000000000000
      0000000000000000000063360000FFFF7F00FFFF7F00FFFF7F007F5B00006336
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000091919100FFFFFF00FFFFFF00FFFFFF008D8D8D00FFFF
      FF00B4B4B4000000000000000000000000000000000000000000000000000000
      0000000000000000000091919100FFFFFF00FFFFFF00AAFFFF00FFFFFF008D8D
      8D00FFFFFF00B4B4B40000000000000000000000000000000000000000000000
      0000000000000000000000000000633600006336000063360000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B4B4B4009A9A9A009A9A9A008E8E8E0081818100C1C1
      C100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B4B4B400A7A7A700A7A7A7009A9A9A008E8E8E008181
      8100C1C1C1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700737373007373
      7300595959004B4B4B004B4B4B003D3D3D00303030003030300030303000A7A7
      A700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      00000000000000000000000000000000000000000000000000007F5B00007F5B
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      00007F5B0000000000000000000000000000A7A7A70059595900CDCDCD00E6E6
      E600C1C1C100CDCDCD00F0F0F000EDEDED00E6E6E600A7A7A700333333003030
      3000A7A7A7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000000000000000000000000000000000007F5B0000E3C1A400E3C1
      A400E3C1A400E3C1A400E3C1A400E3C1A400E3C1A400E3C1A400E3C1A4007F5B
      0000D9A77D007F5B000000000000000000004B4B4B0059595900DADADA00E6E6
      E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600DADADA00737373002929
      2900303030000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000007F5B00007F5B00007F5B00007F5B
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      00007F5B0000D9A77D007F5B0000000000004B4B4B0059595900D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00737373003636
      3600303030009A9A9A00A7A7A700000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000840000008400
      000084000000000000000000000000000000000000000000000084848400C6C6
      C600FFFFFF008484840000000000840000000000000000000000000000000000
      0000000084000000000000000000000000007F5B0000E3C1A400E3C1A400E3C1
      A400E3C1A400E3C1A400E3C1A400FFFF9900FFFF9900FFFF9900E3C1A400E3C1
      A4007F5B00007F5B00007F5B0000000000004B4B4B0059595900D9A77D00D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00737373004A4A
      4A00303030007373730030303000A7A7A7000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600FFFFFF008484840084000000840000008400
      0000000000000000000000000000000000000000000084848400C6C6C600C6C6
      C600C6C6C600FFFFFF0084848400000000000000000000000000000000000000
      8400000084000000000000000000000000007F5B0000E3C1A400E3C1A400E3C1
      A400E3C1A400E3C1A400E3C1A400D9A77D00D9A77D00D9A77D00E3C1A400E3C1
      A4007F5B0000D9A77D007F5B0000000000005959590059595900D9A77D007373
      7300737373007373730073737300737373007373730073737300737373005353
      53004B4B4B00737373002929290030303000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF0084848400000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600FFFFFF00000000000000000000000000000084000000
      8400000084000000840000008400000000007F5B00007F5B00007F5B00007F5B
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      00007F5B0000E3C1A400D9A77D007F5B00005959590059595900D9A77D00AAFF
      FF00AAFFFF00AAFFFF00AAFFFF0099F8FF0099F8FF0099F8FF009A9A9A005656
      56004B4B4B007373730059595900303030000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF00000000000000
      00000000000000000000000000000000000000000000C6C6C600FFFFFF00FFFF
      0000C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      8400000084000000000000000000000084007F5B0000E3C1A400E3C1A400E3C1
      A400E3C1A400E3C1A400E3C1A400E3C1A400E3C1A400E3C1A400D9A77D00E3C1
      A4007F5B00007F5B0000E3C1A4007F5B00006666660059595900AAFFFF00AAFF
      FF00A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700AAFFFF0099F8FF007171
      71005959590073737300595959003D3D3D0000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00C6C6C600C6C6C60084848400000000000000000000000000000000000000
      000000008400000000000000000000008400000000007F5B00007F5B00007F5B
      00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B0000D9A7
      7D00E3C1A4007F5B00007F5B00007F5B00007373730098989800AAFFFF00AAFF
      FF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF0099F8FF009898
      98005959590073737300595959004B4B4B0000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C6008484840000000000000000000000000000000000000000000000
      00000000000000000000000000000000840000000000000000007F5B0000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F5B
      0000D9A77D00E3C1A400D9A77D007F5B000073737300A6A6A60000000000AAFF
      FF00CB8C4400CB8C4400A7A7A700A7A7A700A7A7A70099F8FF00AAFFFF00A6A6
      A600666666008E8E8E00595959004B4B4B0000000000C6C6C600FFFFFF00FFFF
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000007F5B
      0000FFFFFF007F5B00007F5B00007F5B00007F5B00007F5B0000FFFFFF007F5B
      00007F5B00007F5B00007F5B00000000000073737300B1B1B100000000000000
      0000AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00B1B1
      B100737373008E8E8E00666666004B4B4B0000000000C6C6C600FFFFFF00FFFF
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000000000000000000000000000000000000000000000007F5B
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007F5B0000000000000000000000000000A7A7A70081818100595959005959
      5900595959005959590059595900595959005959590059595900666666008181
      8100A7A7A7008E8E8E0073737300595959000000000000000000FFFFFF00FFFF
      FF00FFFF0000FFFF0000C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      00007F5B0000FFFFFF007F5B00007F5B00007F5B00007F5B00007F5B0000FFFF
      FF007F5B000000000000000000000000000000000000A7A7A7008E8E8E008E8E
      8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E009A9A
      9A00DADADA008E8E8E008181810066666600000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00C6C6C600C6C6C600C6C6C60084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      00007F5B0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F5B00000000000000000000000000000000000000000000C1C1
      C100B1B1B100DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADA
      DA00DADADA00DADADA00B1B1B100737373000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      0000000000007F5B00007F5B00007F5B00007F5B00007F5B00007F5B00007F5B
      00007F5B00007F5B00000000000000000000000000000000000000000000A7A7
      A70081818100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1
      C100C1C1C100C1C1C10081818100A7A7A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A7A7A7008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E8E008E8E
      8E008E8E8E008E8E8E009A9A9A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A7A7A7007373
      73007373730059595900595959004B4B4B004B4B4B003D3D3D00303030003030
      300030303000A7A7A70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A70063360000CDCD
      CD00E6E6E600C1C1C100C1C1C100CDCDCD00F0F0F000EDEDED00E6E6E600A7A7
      A7003333330030303000A7A7A7000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000633600006336000063360000DADA
      DA00E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600E6E6E600DADA
      DA00CB8C440063360000303030003030300000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000063360000CB8C440063360000D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C4400CB8C4400CB8C4400CB8C
      4400CB8C4400633600007F5B00003030300000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400C6C6C600C6C6C6008484
      84000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000063360000D9A77D0063360000D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C4400CB8C4400CB8C
      4400CB8C440063360000CB8C44003030300000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000084848400C6C6C600C6C6C600FFFF00008484
      84008484840000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000063360000D9A77D0063360000D9A7
      7D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00D9A77D00CB8C4400CB8C
      4400CB8C440063360000CB8C44003D3D3D0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400C6C6C60000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000063360000D9A77D0063360000AA3F
      2A00633600006336000063360000633600006336000063360000633600006336
      0000CB8C440063360000CB8C44004B4B4B0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C6C6C600FFFF0000C6C6C600C6C6C6008484
      8400C6C6C60000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000000000000000000000000000063360000D9A77D00633600009A9A
      9A00AAFFFF0099F8FF0099F8FF0099F8FF0099F8FF0099F8FF0099F8FF0099F8
      FF006336000063360000CB8C44004B4B4B0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000084848400FFFF0000FFFF0000C6C6C6008484
      84008484840000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000C6C6C60000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      00008400000084000000000000000000000063360000D9A77D0063360000AAFF
      FF00CDCDCD00A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700C1C1
      C10099F8FF0063360000CB8C44004B4B4B0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400C6C6C600C6C6C6008484
      84000000000000000000000000000000000000000000FFFFFF0000000000C6C6
      C60000000000FFFFFF0000000000C6C6C60000000000C6C6C600000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000084000000840000000000000063360000D9A77D007F5B0000AAFF
      FF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF0099F8FF007F5B0000CB8C44004B4B4B0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000C6C6C60000000000C6C6C60000000000C6C6C60000000000C6C6C600C6C6
      C600C6C6C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000063360000D9A77D007F5B0000AAFF
      FF00CDCDCD00A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700A7A7A700C1C1
      C10099F8FF007F5B0000CB8C44004B4B4B0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000063360000D9A77D0098989800AAFF
      FF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF0099F8FF0098989800CB8C44005959590000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6C6C60000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084000000840000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      00000000000084000000840000008400000063360000D9A77D00A6A6A6000000
      0000CDCDCD00CB8C4400CB8C4400A7A7A700A7A7A700A7A7A700A7A7A700C1C1
      C10099F8FF00A6A6A600D9A77D006666660000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      000084000000840000008400000000000000A7A7A700633600007F5B00000000
      000000000000AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFFFF00AAFF
      FF00AAFFFF007F5B000063360000A7A7A70000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000A7A7A7006336
      0000633600006336000063360000633600006336000063360000633600006336
      0000633600009A9A9A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000005B7000005B7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000005B7000005B7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000005B7000005B700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000005B7000005B7000005
      B700000000000000000000000000000000000000000000000000000000000000
      00000005B7000005B70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000005B7000005B6000005
      B7000005B7000000000000000000000000000000000000000000000000000005
      B7000005B700000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000006D7000005
      BA000005B7000005B700000000000000000000000000000000000005B7000005
      B70000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000005B7000005B7000005B600000000000005B6000005B7000005B7000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000005B6000006C7000006C7000006CE000005B400000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000000000000000000000006C1000005C1000006DA0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000005B6000006D7000006CE000006DA000006E900000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      00000006E5000006DA000006D30000000000000000000006E5000006EF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000008400000000000000000000000000000000000000000000000006
      F8000006DA000006EF00000000000000000000000000000000000006F8000006
      F60000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000006F6000006
      F6000006F8000000000000000000000000000000000000000000000000000006
      F6000006F600000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000006F6000006F6000006
      F600000000000000000000000000000000000000000000000000000000000000
      0000000000000006F6000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000006F6000006F6000006F6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000006F6000006F600000000000000
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
      000000000000000000000000000000000000000000000000000097300200A338
      0100A03601009A32020000000000000000000000000097300200A03601009D35
      0100952E02000000000000000000000000000000000000000000000000000000
      000000000000A0756E0074434200744342007443420074434200744342007443
      4200744342007443420074434200000000000000000000000000000000001B88
      CC001B88CC008E5D59008E5D59008E5D59008E5D59008E5D59008E5D59008E5D
      59008E5D59008E5D59008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009E3501000000
      000000000000932D0200952D020000000000952D020099300200000000000000
      0000972F02000000000000000000000000000000000000000000000000000000
      000000000000A0756E00FFF8E500F7EDD900F7EBD500F4E9D100F4E9D000F4E7
      CF00F6EAD000EEDDC400754443000000000000000000000000001B88CC0074C7
      E90067C5EB00BAB7AA00FBE7D300F8EEDC00F6EDD700F4E9D300F4E9D000F4E7
      D000F4E6CF00F6E7CE008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009E3501000000
      00000000000000000000952D020000000000952E020000000000000000000000
      0000962E02000000000000000000000000000000000000000000000000000000
      000000000000A0756E00F7EDDC00F2D9BF00F2D7BB00F0D5BA00EFD4B500EED3
      B200EED9BF00E5D0BA007544430000000000000000001B88CC008CE1F6007FEF
      FF0072EEFF00BAB7AA00F3DCCF00F3DDC500F3D5B600F2D4B500F0D3B200F0D1
      AD00EED4B600EBDAC2008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009E350100912C
      02000000000000000000932D020000000000952E020000000000000000008A27
      0300962F02000000000000000000000000000000000000000000000000000000
      000000000000A0756E00FAEFDE00FCC59100FCC59100FCC59100FCC59100FCC5
      9100FCC59100E3D1BC007544430000000000000000001B88CC008AE0F6007AE7
      FF006BE6FF00BAB7AA00F6E1D500F7DCC000F7D0AB00F7D0AB00F7D0AB00F6CE
      A500F2D3B100EBDCC5008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A138
      01009C330200A13601009F330100000000009D3201009F350100962F0200952E
      02000000000000000000000000000000000000000000A0756E00744342007443
      420074434200A0756E00FCF4E700F6D9BA00F7D7B600F6D4B500F6D4B200F4D1
      AD00F0DCC200E6D3C00081524C0000000000000000001B88CC0097E2F6008BED
      FF007DEBFF00BAB7AA00F7E5DC00F7DEC600F7D3B100F7D4B100F6D3B000F4D0
      AB00F2D5B600EEDECA008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099310100A02F0000882F0500692D1500882D06009F2E0000972F02000000
      00000000000000000000000000000000000000000000A0756E00FFF8E500F7ED
      D900F7EBD500A0756E00FEF6EB00F8DABC00F8D9B800F8D8B700F7D5B600F7D4
      B200F3DEC700E7D7C50081524D0000000000000000001B88CC00A1E5F6009AEF
      FF008CEEFF00BAB7AA00F8EAE200F8E7D400F8DDC200F7DDC100F7DABF00F6D8
      BB00F2DCC200EFE1D0008E5D5900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000006848390098868700952E020000000000000000000000
      00000000000000000000000000000000000000000000A0756E00F7EDDC00F2D9
      BF00F2D7BB00A0756E00FEFAF200FCC59100FCC59100FCC59100FCC59100FCC5
      9100FCC59100EBDDCF008F5F5A0000000000000000001B88CC00ADE6F600ACF2
      FF009CF0FF00BAB7AA00FBF0EB00FBE1C500FBD0A900FAD1AA00F8D0A900F8CE
      A400F6D8B800F4E9DA008E5D5900000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000004B3F3500B19D9C00BDA3A1006E59510055433900000000000000
      00000000000000000000000000000000000000000000A0756E00FAEFDE00FCC5
      9100FCC59100A0756E00FFFCFA00FCE3CC00FBE0C700FADEC600F8DEC400FCE2
      C600FCF0DE00E1D7CE008F5E590000000000000000001B88CC00B7E9F600BCF4
      FF00ACF3FF00BAB7AA00FEF4F000FEF4EB00FAEBDD00FAEADA00F8E7D700F8E9
      D800F7EBDD00E1DAD3008E5D5900000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      00000000000088766E00E6D4D70077635900AC8F8C00997B77004B3B30000000
      00000000000000000000000000000000000000000000A0756E00FCF4E700F6D9
      BA00F7D7B600A0756E00FFFFFF00FEFFFF00FBFBFB00FAF8F700FAFAF600E5D5
      D000C6B1AF00A79395009E675A0000000000000000001B88CC00C0EAF600D0FA
      FF00BFF7FF00BAB7AA00FEF6F200FFFFFF00FEFFFE00FBF8F700FAFAF700EBE0
      D900C7ADA300B59A8B008E5D5900000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      000055413500E2D9D800AD9D98004B372A00A0868000C6A4A500664F44000000
      00000000000000000000000000000000000000000000A0756E00FEF6EB00F8DA
      BC00F8D9B800A0756E00FFFFFF00FFFFFF00FFFEFE00FFFCF800FFFEFA00A075
      6E00A0756E00A0756E00A0756E0000000000000000001B88CC00C7EBF600E2FC
      FF00D0FAFF00BAB7AA00FFF8F600FFFFFF00FFFFFF00FFFFFF00FFFFFF00D4BB
      B500D7925500F47A410000000000000000000000000000000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000009175
      6F0090827900E7E5E2005D4A3D00000000005E4A3E00C0A09F0091756F009175
      6F000000000000000000000000000000000000000000A0756E00FEFAF200FCC5
      9100FCC59100A0756E00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A075
      6E00E5A15400B6735D000000000000000000000000001B88CC00CFEDF600F6FF
      FF00E2FEFF00BAB7AA00FBEBE200FBF2ED00FBF0EB00FAEFEB00FAF0ED00DAB4
      A700B79A6F001B88CC0000000000000000000000000000000000840000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000005642
      3600CCC5C10097898000000000000000000000000000826B6300B29592005642
      36000000000000000000000000000000000000000000A0756E00FFFCFA00FCE3
      CC00FBE0C700A0756E00A0756E00A0756E00A0756E00A0756E00A0756E00A075
      6E00AA6D6800000000000000000000000000000000001B88CC00D0EBF600FFFF
      FF00F2FFFF00BAB7AA00BAB7AA00BAB7AA00BAB7AA00BAB7AA00BAB7AA00BAB7
      AA006BB4CF001B88CC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000006B59
      4D00B0A59E00513E3100000000000000000000000000513E3100997F7A006D57
      4D000000000000000000000000000000000000000000A0756E00FFFFFF00FEFF
      FF00FBFBFB00FAF8F700FAFAF600E5D5D000C6B1AF00A79395009E675A000000
      000000000000000000000000000000000000000000001B88CC00D3EDF700F4F2
      F0009CB7BC0093B4BC0091B4BB0090B4BB008FB4BB008BB1B8009FC4CA00D5FC
      FE006FCEF2001B88CC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006C5B
      4E00705F530000000000000000000000000000000000000000006B5449006C56
      4B000000000000000000000000000000000000000000A0756E00FFFFFF00FFFF
      FF00FFFEFE00FFFCF800FFFEFA00A0756E00A0756E00A0756E00A0756E000000
      000000000000000000000000000000000000000000001B88CC00DCF6FF00D5BC
      B100A98C8000C1B0AA00C1B0A900C1B0A900C1B0A900C0ACA400A6877900DEE1
      DA0078D0F4001B88CC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005441
      3300000000000000000000000000000000000000000000000000000000005541
      35000000000000000000000000000000000000000000A0756E00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00A0756E00E5A15400B6735D00000000000000
      00000000000000000000000000000000000000000000000000001B88CC00A0C5
      D8007E878800D9CCC600F8F7F600F7F6F400F7F6F400C2B5AD006A868F0073C1
      E0001B88CC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0756E00A0756E00A075
      6E00A0756E00A0756E00A0756E00A0756E00AA6D680000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001B88
      CC001B88CC0088786F0088786F0088786F0088786F0088786F001B88CC001B88
      CC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000000200000100010000000000001000000000000000000000
      000000000000000000000000FFFFFF0000070000000000000007000000000000
      000700000000000000070000000000007E070000000000000007000000000000
      7FFF00000000000070000000000000006000000000000000B000000000000000
      C000000000000000F400000000000000E700000000000000F000000000000000
      F000000000000000F03F000000000000FFFFF000FFFFFFBFFE3FF000FFFFFF1F
      F81FF00000000000E00FF000000000008007F000000000000003F00000000000
      0001F000000000000000F000000040000001C000000000008001C00040005000
      C001000060002000E000000000005000F000000120140000F803008B00008001
      FC0FC007000080FFFE3FC3FFFFFFC1FF003FEC00001F800F003FDC00001F0007
      003FDC00001F000700008C00001F00010000DC00401F00000000FD00401F0000
      400001007E000000400001000000000040000100000000004000010100002000
      400001CBF800300072000006F800000001000016F8008000FD010001F800E000
      FC0B0017F800E000FC07001FF800F001FF7FFFFFFFFF8FFFFF47FFFFFE7F400F
      FF078000FC3F400FFF478000FC3F400FFF788000FE7F400FF8208000FFFF800F
      F0288000FE7FC00FF00F8000FC3FC00FF0088000FC3FC00FF0008000FC3FC007
      E0288000FC3FC00BC0478000FC3FC00D83078000FC3FFFBE07C78000FC3FFFDE
      0FFF8000FE7FFFEE9FFFFFFFFFFFFFF1FFFFFFFFFFFFF7D7C07FE0017FFDF311
      953FE000BFFB81112A9FF000E00FB210555FF000C007B7FF2A9FF000C007BE10
      555FFFFDC007FF112A83FFFFC0070011554380FB400400D700010071C00700FF
      00000060C00700FF0000007BC00700FF8001007BC00700FFC043007BE00F00FF
      FFE70083BEFB01FFFFFF8FFF7EFD03FFFFFFFC00EDB6FFFFC07FFC00EAAAF003
      9531FC00EAAAE0032000FC00EDB6E0015000FC00FFFFE0012001FC00C01FE000
      5000FC00C01FE0002000FC01C019E0035551FE3F0010E003001FFFF30019E003
      001FFFE10019E00A001F00730039E0FB803F3F730061F1E4C07F3947007FFFFB
      FFFF007F00FFFFEAFFFF007F9FFFFFDB7445FFC7000F003F3555FF01000F003F
      1555FE00000F003F3555FE00000F003F7445FE00000F003FFFFFFE00000F0000
      E3FFFF01000F000080FFE3830000000080FF80EFBE000000007F80FFBE000000
      00730073B600000000730073D200000080C00040E000E00080C080C0F200E000
      E3F380F3F600E000FFF3E3F3FFFFE000AC00F000FFFFF80F07FFF0008001E003
      AFFFF0008001C00107FF70008001C001AFFF30008001C001FFFF10008001C001
      C7C710008001C001C7C730008001E003C38770008001B80FC007F00080011E3F
      C007F00080014FFFC007F0008001E7FFC007F0018001BFFFC007F00380031FFF
      F39FF00780074FFFF39FFFFF800FE7FFFFFFFFFFB6E7FFFFC0018001B76BFE49
      C00180018427FE49C0018001B76BFFFFC0018001CEE7FFFFC0018001FFFFC7C7
      C0018001C7C7C7C7C0018001C7C7C387C0018001C387C007C0018001C007C007
      C0018001C007C007C0018001C007C007C0018001C007C007C0038001C007F39F
      C0078001F39FF39FC00FFFFFF39FF39FFFFFFFFFFFFFFFFF0022FFFFC03FC001
      0000FFFFC03FC00100000003003FC001E00700030000C001E00700030000C001
      E00700030000C001E00700030000C001E00700030000C001E00700030000C001
      E00700030000C001E00700030000C001FFFF0003C000C001F81F0003C000C003
      F81F0003C000C007F81F0003C000C00FFFFFE00FFFFFFFFF8001EFEFA491F00F
      8001EFE8BFF5F0088001E92FFBF1F00F8001EEEDB9FFF00D8001EEE8B8F1F008
      8001EEEDB871F00D8001EEEDF83BF00D8001EEEFB83BF00F8001EEEDB87FF00D
      8001E92F18FBD00F80016FED19FBCFFD8001200FFBFFC7FF80011FFD1FFBCFFD
      C0FF3FFF524BDFFFFFFF7FFF1FFFFFFFF00081FFFFFFFFFFF67A00FF8001C001
      F43200FFBFFDDFFDF48600FFB24DD80DF7CE00FFB24DDFFDF44600FFBFFDC001
      F41200FFB24DD861F7B801FFB24DDFE3F7FEDFFFBFFDC007F000FFFFA005FFFF
      83FFDFFFA005FE31E3FFFFBFA005F400C3FFEF9FA005ED8C8BFFFA8FBFFDEC84
      1BFFFF9F8001E8003FFFFFBFFFFFF3F9FFFFFFFFFFFFFFCFF8F8F007F003FFC7
      F8F8F007F001FFC3F870F007F801FFC1F800F007F801FFC0F800C007FFF901C1
      F800C00703FF01C30000C00703FB00470000C01F00F1004F0013001F00E0001F
      00130010003B001F00130011003B001F001F0070C03BC01F001F0076C023C01F
      001F007CF03FF01F8FFF007DF03FF01FFFFFF001FFF3FFFFFFFFF001FFF3FFFF
      FE11F001FFFF07C1F000F001FFF307C1EE00F001FFF307C1D001F001FFF10101
      900380013E780001B0030001007C0001B0032001004C0001A003F00100408003
      8FE780010061C107FFFF0003007FC107FFFF2007007FE38FFFFFF18F007FE38F
      FFFFF8FF007FE38FFFFFFDFFC1FFFFFFF83F9FFFFFFFFDFFF83F0FFFC001F0FF
      F80307FFC001E07FF80783FFC001C03FF800C03FC001800DF800E01FC0018000
      F000000FC0018000F0000007C0018000F0000007C001C000F0000003C001F800
      E01F0003C001F800C03F0001C001F98083FF0001C001F98107FF0003C003F9C3
      0FFF000FC007F9E79FFF000FC00FF9FFC001FFFFFFFFFE3FC001FFFFFF93F80F
      C001FF07F339E003C001FE03E7398000C0017C01E7398000C0013800E7398000
      C0011800C3938000C0010800E7FF8000C0011800FFFF8000C0013800F27FC001
      C0017C01E73FC001C001FE03E73FE003C001FF07E73FE003C001FFFFE73FF007
      C001FFFFF27FF007FFFFFFFFFFFFF80FFFFFF001E1C0E000AAAAF001C0F06000
      FFFFF00180702000BFC2C00180400000FFE3C00180610000BFC2C00180730000
      FFCB0001C0FF0000803E0001E1870000803F0007FF030000803E0007CE010000
      803F000786010000803E001F02010000803F001FCE010000802A001FCF030000
      FFFF8FFFC3870000FFFFFFFFFFFF0000FFF9E00FFFFFF000FFF8E00FC3FFF000
      FFD1E00FE3FFF0004941000FC20070007FC0000F8A003000F0C1000FBE001000
      70C3000FB000100070C7001FF0003000F0CF003FF00070007FDF007DF000F000
      43FF00FDF001F000C3FF01D1F003F00043DF03C3F007F00143DFFFC7F00FF003
      FFFFFFC3F01FF007249FFFFFF03FFFFFF01FFFFFFFFFF800F01FE001FFFFF800
      F01FE000FFFFF800F010F000FFDDF800F01FF000FFCDF800F019F0008001F800
      F010FFFDFFCDF800F019FFFFFFDDF000F01980FBBBFFF00070190071B3FFE000
      301F00608001E0001019007BB3FFC07F101F007BBBFF81FF3FF9007BFFFF07FF
      7FFF0083FFFF0FFFFFF98FFFFFFF9FFF8000FE3FFFFFB0008000F80FFE1FB000
      8000E003FC5F800080008000FC7FB00080008000FC3F800080008000FC3FF800
      80008000FC3FF80080008000FE1FF00080008000FF1FF00080008000FD1FE000
      80008000FC1FE00080008000FFFFC07F80008000FE1F81FF8000E003FE1F07FF
      8000F80FFE1F0FFFFFFFFE3FFFFF9FFFEDB6FFFFFFFF8000EAAAFFFFC03F8000
      EAAAFFFFC03F8000EDB6FFFF003F8000FFFF83F100008000FFF903F100008000
      FFF0000000008000E039000000008000E039000000008000E03903FF00008000
      E02183FF00008000E03FFFFF00008000C03FFFFFC00080009FFFFFFFC0008001
      3FFFFFFFC00080037FFFFFFFC0008007FFFFF800FF00FE3FFFFFF800FF00F80F
      FCF9F800FC00E003FDFDF800FC008000BDFDF800FC0080009D55F800FC008000
      8DFDF800FC00800085FDF000FC0080008DFDF000FC0080009DFDE000FC008000
      BDFDE000FC008000FDFDC07FE0038000FCF981FFF0038000FFFF07FFE1FFE001
      FFFF0FFFC5FFF803FFFF9FFF0FFFFE27FFFFC003F800F8000001C003F800F800
      0001C003F800F8000001C003F800F8000001C003F800F8000001C003F800F800
      0001C003F800F8000001C003F000F0000001C003F000F0000001C003E000E000
      0001C003E000E0000001C003C07FC07F0001C00381FF81FF0001C00307FF07FF
      0001C0030FFF0FFFFFFFC0039FFF9FFFEDB6F7D7C000FFFFEAAAF311C000CE01
      EAAA8111C000E601EDB6B210C000E601FFFFB7FFC000CE01F07FBE10C000FFFF
      C03FFF11C000C601801FFF118000CE01001F00571C0FF6010019007F4E1FCE01
      0010007FE7FFFFFF0019007FB3FFEE010019007F1FFFEE010039007F4FFFEE01
      8061007FE7FFEE01C1FF8FFFF3FFFFFFE0FFFFFFF800EDB6C07FFC01F800EAAA
      803FC801F800EAAA001FD801F800EDB6001FD801F800FFFF001FD001F800FFFF
      001FC7F3F800E01F001FFFFFF000E01F803FFFFFF000E01FC07FFFFFE0000019
      E0F3FFF3E0000010FFF3FFF3C07F0019FFC0FFC081FF0019FFC0FFC007FF00F9
      FFF3FFF30FFF00E1FFF3FFF39FFF00FFFFFFC183FFFFFFFFF9FFC183FC1FF07F
      F0FFC183FC0FF03FF07FC183FC07F01FF03FC183FC07F01FF01FC183FC07F01F
      F00FC183FE07F81FF007C183FF07FC1FF007C183FFDFFFF7F00FC183FF8FFFE3
      F01FC183FF07FFC1F03FC183FFDFBFF7F07FC183FFFFFFFFF0FFC183FFBFDFEF
      F1FFC183FAFFF6BFFBFFC183FFFFFFFF003FFFFFFC00FFFF003F801FEC00FFFF
      0007801FDC0000010007801FDC000001000080078C00000100008007DC000001
      0000800700000001000080010000000100008001000000010000800100010001
      0000E001000300010000E00100070001E000E001001E0001E001F801001A0001
      FC03F80100110001FC07F801001BFFFFFFFFFFFF00030003FE3F000700030003
      F81F000700010001E00F00070000000080070007000100000003000700010000
      0001000F000100000000FFFF0001000000013F010001000080013E0000010000
      C001360080018000E000120080018000F0000000C001C000F803F200FC03FC01
      FC0FF601FC07FC03FE3FFF1FFC0FFC07FFF3FFFFFFFF800FFFE9FF3FC0070007
      FFD1FE3F80030007FFA3C07F00010001F04780F700010000C00F00E700010000
      801F00C100000000801F00E600000000000F00F680000000000F81FEC0002000
      000FC3BFE0013000000FFFB7E0070000801FFFB3F0078000801FFFC1F003E000
      C03FFFF3F803E000F0FFFFF7FFFFF001FFFFFFFFFFFFFFFFC003000C000FF9FF
      80010008000FF9FF00000001000FF3C700000003000F73C700000003000F27FF
      00000003000F07C700000003000F00C700000003000F01E300000007000403F1
      0000000F000006380000000F00000E380000000FF8001E381000001FFC003F01
      1800003FFE047F83C003007FFFFFFFFFFFFFFFFCFFFFFFFFFFFF9FF9FFFFFFFF
      FFFF8FF3C007001FFFFF87E7C007000FFFFFC3CFC0070007EFFFF11FC0070003
      EF83F83FC0070001DFC3FC7FC0070000DFE3F83FC007001FDFD3F19FC007001F
      EF3BE3CFC007001FF0FFC7E7C0078FF1FFFF8FFBC00FFFF9FFFF1FFFC01FFF75
      FFFF3FFFC03FFF8FFFFFFFFFFFFFFFFFC387F801E001FFFFD937F801C001FFFF
      DD77F8018001FFFFCD67F8018001FFFFE10F80018001FFFFF01F80018001FFF7
      FC7F80018001C1F7F83F80018001C3FBF81F80018001C7FBF01F80018003CBFB
      E10F80038003DCF7E38F80078003FF0FE38F801F8003FFFFE7CF801F8003FFFF
      EFEF803FC007FFFFFFFF807FE00FFFFF}
  end
  object ActionList1: TActionManager
    ActionBars = <
      item
        Items.AutoHotKeys = False
        Items = <>
        ActionBar = MenuBar
        AutoSize = False
      end>
    Images = ImageList1
    Left = 264
    Top = 88
    StyleName = 'XP Style'
    object RunRunCommand: TPopupAction
      Category = 'Run'
      Caption = '&Run'
      DropdownMenu = ProcessMenu
      HelpContext = 1126
      Hint = 'Run'
      ImageIndex = 24
      OnExecute = RunRun
      OnUpdate = RunCommandsUpdate
    end
    object FileNewCommand: TAction
      Category = 'File'
      Caption = '&Other...'
      HelpContext = 1181
      Hint = 'New items'
      ImageIndex = 121
      OnExecute = FileNew
      OnUpdate = FileNewCommandUpdate
    end
    object FileNewUnitCommand: TAction
      Category = 'File'
      Caption = '&Unit'
      HelpContext = 1019
      Hint = 'New Unit'
      ImageIndex = 103
      OnExecute = FileNewUnit
      OnUpdate = FileNewCommandUpdate
    end
    object FileOpenCommand: TPopupAction
      Category = 'File'
      Caption = '&Open...'
      DropdownMenu = ClosedFilesPopup
      HelpContext = 1009
      Hint = 'Open'
      ImageIndex = 123
      OnExecute = FileOpen
    end
    object FileOpenProjectCommand: TAction
      Category = 'File'
      Caption = 'Open Pro&ject...'
      HelpContext = 1002
      Hint = 'Open Project'
      ImageIndex = 124
      OnExecute = ProjectOpen
    end
    object FileSaveCommand: TAction
      Category = 'File'
      Caption = '&Save'
      HelpContext = 1010
      Hint = 'Save'
      ImageIndex = 8
      OnExecute = FileSave
      OnUpdate = FileSaveActionUpdate
    end
    object FileSaveAsCommand: TAction
      Category = 'File'
      Caption = 'Save &As...'
      HelpContext = 1011
      Hint = 'Save As'
      ImageIndex = 110
      OnExecute = FileSaveAs
      OnUpdate = FileSaveAsActionUpdate
    end
    object FileSaveProjectAsCommand: TAction
      Category = 'File'
      Caption = 'Sav&e Project As...'
      HelpContext = 1004
      Hint = 'Save Project As'
      ImageIndex = 109
      OnExecute = ProjectSaveAs
      OnUpdate = FileSaveProjectAsActionUpdate
    end
    object FileSaveAllCommand: TAction
      Category = 'File'
      Caption = 'Sa&ve All'
      HelpContext = 1003
      Hint = 'Save All'
      ImageIndex = 15
      OnExecute = FileSaveAll
      OnUpdate = FileSaveAllActionUpdate
    end
    object FileCloseCommand: TAction
      Category = 'File'
      Caption = '&Close'
      HelpContext = 1012
      Hint = 'Close'
      ImageIndex = 111
      OnExecute = FileClose
      OnUpdate = FileSaveAsActionUpdate
    end
    object FileCloseAllCommand: TAction
      Category = 'File'
      Caption = 'C&lose All'
      HelpContext = 1005
      Hint = 'Close All'
      ImageIndex = 33
      OnExecute = FileCloseAll
      OnUpdate = FileCloseAllActionUpdate
    end
    object FilePrintCommand: TAction
      Category = 'File'
      Caption = '&Print...'
      HelpContext = 1016
      Hint = 'Print'
      ImageIndex = 14
      OnExecute = FilePrint
      OnUpdate = FilePrintCommandUpdate
    end
    object FileExitCommand: TAction
      Category = 'File'
      Caption = 'E&xit'
      HelpContext = 1017
      Hint = 'Exit'
      ImageIndex = 88
      OnExecute = FileExit
    end
    object EditUndoCommand: TAction
      Category = 'Edit'
      Caption = '&Undo'
      HelpContext = 1026
      Hint = 'Undo'
      ImageIndex = 3
      OnExecute = EditUndo
      OnUpdate = EditUndoCommandUpdate
    end
    object EditRedoCommand: TAction
      Category = 'Edit'
      Caption = '&Redo'
      HelpContext = 1027
      Hint = 'Redo'
      ImageIndex = 4
      OnExecute = EditRedo
      OnUpdate = EditRedoCommandUpdate
    end
    object EditCutCommand: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      HelpContext = 1028
      Hint = 'Cut'
      ImageIndex = 0
      OnExecute = EditCut
      OnUpdate = EditCutCommandUpdate
    end
    object EditCopyCommand: TAction
      Category = 'Edit'
      Caption = '&Copy'
      HelpContext = 1029
      Hint = 'Copy'
      ImageIndex = 1
      OnExecute = EditCopy
      OnUpdate = EditCopyCommandUpdate
    end
    object EditPasteCommand: TAction
      Category = 'Edit'
      Caption = '&Paste'
      HelpContext = 1030
      Hint = 'Paste'
      ImageIndex = 2
      OnExecute = EditPaste
      OnUpdate = EditPasteCommandUpdate
    end
    object EditDeleteCommand: TAction
      Category = 'Edit'
      Caption = '&Delete'
      HelpContext = 1031
      Hint = 'Delete'
      ImageIndex = 5
      OnExecute = EditDelete
      OnUpdate = EditDeleteCommandUpdate
    end
    object EditSelectAllCommand: TAction
      Category = 'Edit'
      Caption = 'Se&lect All'
      HelpContext = 1042
      Hint = 'Select All'
      OnExecute = EditSelectAllClick
      OnUpdate = EditSelectAllCommandUpdate
    end
    object EditAlignGridCommand: TAction
      Category = 'Edit'
      Caption = 'Align to &Grid'
      HelpContext = 1036
      Hint = 'Align To Grid'
      OnExecute = EditAlignToGrid
      OnUpdate = EditCommandUpdate
    end
    object EditFrontCommand: TAction
      Category = 'Edit'
      Caption = 'Bring to &Front'
      HelpContext = 1032
      Hint = 'Bring To Front'
      ImageIndex = 58
      OnExecute = EditBringToFront
      OnUpdate = EditCommandUpdate
    end
    object EditBackCommand: TAction
      Category = 'Edit'
      Caption = 'Send to &Back'
      HelpContext = 1033
      Hint = 'Send To Back'
      ImageIndex = 57
      OnExecute = EditSendToBack
      OnUpdate = EditCommandUpdate
    end
    object EditAlignCommand: TAction
      Category = 'Edit'
      Caption = '&Align...'
      HelpContext = 1034
      Hint = 'Align'
      ImageIndex = 54
      OnExecute = EditAlign
      OnUpdate = EditCommandUpdate
    end
    object EditSizeCommand: TAction
      Category = 'Edit'
      Caption = '&Size...'
      HelpContext = 1035
      Hint = 'Size'
      ImageIndex = 60
      OnExecute = EditSize
      OnUpdate = EditCommandUpdate
    end
    object EditScaleCommand: TAction
      Category = 'Edit'
      Caption = 'Scal&e...'
      HelpContext = 1038
      Hint = 'Scale'
      ImageIndex = 56
      OnExecute = EditScale
      OnUpdate = EditCommandUpdate
    end
    object EditTabOrderCommand: TAction
      Category = 'Edit'
      Caption = 'Tab &Order...'
      HelpContext = 1039
      Hint = 'Tab Order'
      ImageIndex = 61
      OnExecute = EditTabOrder
      OnUpdate = EditCommandUpdate
    end
    object EditFlipChildrenAllCommand: TAction
      Category = 'Edit'
      Caption = '&All'
      HelpContext = 1045
      Hint = 'Flip All Children'
      OnExecute = EditFlipChildrenAll
      OnUpdate = EditCommandUpdate
    end
    object EditFlipChildrenSelectedCommand: TAction
      Category = 'Edit'
      Caption = '&Selected'
      HelpContext = 1045
      Hint = 'Flip Selected Children'
      OnExecute = EditFlipChildrenSelected
      OnUpdate = EditCommandUpdate
    end
    object EditCreationOrderCommand: TAction
      Category = 'Edit'
      Caption = 'Creatio&n Order...'
      HelpContext = 1041
      Hint = 'Creation Order'
      ImageIndex = 35
      OnExecute = EditCreationOrder
      OnUpdate = EditCommandUpdate
    end
    object EditLockControlsCommand: TAction
      Category = 'Edit'
      Caption = 'Loc&k Controls'
      HelpContext = 1043
      Hint = 'Lock Controls'
      ImageIndex = 45
      OnExecute = EditLockControls
      OnUpdate = EditCommandUpdate
    end
    object SearchFindCommand: TAction
      Category = 'Search'
      Caption = '&Find...'
      Enabled = False
      HelpContext = 1051
      Hint = 'Find'
      ImageIndex = 75
      OnExecute = SearchFind
      OnUpdate = SearchCommandUpdate
    end
    object SearchFileFindCommand: TAction
      Category = 'Search'
      Caption = 'Fin&d in Files...'
      HelpContext = 1059
      Hint = 'Find in Files'
      ImageIndex = 76
      OnExecute = SearchFileFind
      OnUpdate = SearchFileFindCommandUpdate
    end
    object SearchReplaceCommand: TAction
      Category = 'Search'
      Caption = '&Replace...'
      HelpContext = 1052
      Hint = 'Replace'
      ImageIndex = 94
      OnExecute = SearchReplace
      OnUpdate = SearchCommandUpdate
    end
    object SearchAgainCommand: TAction
      Category = 'Search'
      Caption = '&Search Again'
      HelpContext = 1053
      Hint = 'Search Again'
      ImageIndex = 95
      OnExecute = SearchSearchAgain
      OnUpdate = SearchCommandUpdate
    end
    object SearchIncrementalCommand: TAction
      Category = 'Search'
      Caption = '&Incremental Search'
      HelpContext = 1054
      Hint = 'Incremental Search'
      ImageIndex = 96
      OnExecute = SearchIncrementalClick
      OnUpdate = SearchCommandUpdate
    end
    object SearchGoToCommand: TAction
      Category = 'Search'
      Caption = '&Go to Line Number...'
      HelpContext = 1055
      Hint = 'Go to Line Number'
      ImageIndex = 97
      OnExecute = SearchGoToItemClick
      OnUpdate = SearchCommandUpdate
    end
    object ViewPrjMgrCommand: TAction
      Category = 'View'
      Caption = '&Project Manager'
      HelpContext = 1083
      Hint = 'View Project Manager'
      ImageIndex = 69
      OnExecute = ViewProjectManager
    end
    object ViewObjInspCommand: TAction
      Category = 'View'
      Caption = '&Object Inspector'
      HelpContext = 1088
      Hint = 'View Object Inspector'
      ImageIndex = 68
      OnExecute = ViewsPropInsp
    end
    object ViewObjTreeViewCommand: TAction
      Category = 'View'
      Caption = '&Object TreeView'
      HelpContext = 8116
      Hint = 'View Object TreeView'
      ImageIndex = 112
      OnExecute = ViewsObjTreeView
    end
    object ViewCompListCommand: TAction
      Category = 'View'
      Caption = '&Component List'
      HelpContext = 1084
      Hint = 'View Component List'
      OnExecute = ViewComponentList
    end
    object ViewWindowListCommand: TPopupAction
      Category = 'View'
      Caption = '&Window List...'
      DropdownMenu = WindowListPopup
      HelpContext = 1090
      Hint = 'View Window List'
      ImageIndex = 77
      OnExecute = ViewWindowList
    end
    object ViewToggleFormCommand: TAction
      Category = 'View'
      Caption = 'To&ggle Form/Unit'
      HelpContext = 1092
      Hint = 'Toggle Form/Unit'
      ImageIndex = 117
      OnExecute = ViewToggleForm
      OnUpdate = ViewToggleFormCommandUpdate
    end
    object ViewUnitCommand: TAction
      Category = 'View'
      Caption = '&Units...'
      HelpContext = 1085
      Hint = 'View Unit'
      ImageIndex = 116
      OnExecute = ViewModule
      OnUpdate = ViewUnitFormCommandUpdate
    end
    object ViewFormCommand: TAction
      Category = 'View'
      Caption = '&Forms...'
      HelpContext = 1086
      Hint = 'View Form'
      ImageIndex = 118
      OnExecute = ViewForm
      OnUpdate = ViewUnitFormCommandUpdate
    end
    object ViewNewEditorCommand: TAction
      Category = 'View'
      Caption = 'New &Edit Window'
      HelpContext = 1176
      Hint = 'New Edit Window'
      OnExecute = ViewNewEditor
      OnUpdate = ViewNewEditorCommandUpdate
    end
    object ViewSwapSourceFormCommand: TAction
      Category = 'View'
      Caption = '&View as Text'
      HelpContext = 1094
      Hint = 'View Form As Text'
      OnExecute = ViewSwapSourceFormItemClick
      OnUpdate = ViewSwapSourceFormCommandUpdate
    end
    object ViewCustomizeCommand: TAction
      Category = 'View'
      Caption = '&Customize...'
      HelpContext = 1072
      Hint = 'Customize Toolbars'
      OnExecute = ViewCustomizeCommandExecute
    end
    object ViewAlignPaletteCommand: TAction
      Category = 'View'
      Caption = '&Alignment Palette'
      HelpContext = 1078
      Hint = 'View Alignment Palette'
      ImageIndex = 64
      OnExecute = ViewsAlignPalette
    end
    object ProjectAddCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Add to Project...'
      HelpContext = 1105
      Hint = 'Add file to project'
      ImageIndex = 18
      OnExecute = ProjectAddFile
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectRemoveCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Remove from Project...'
      HelpContext = 1106
      Hint = 'Remove file from project'
      ImageIndex = 19
      OnExecute = ProjectRemoveFile
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectImportTypeLibraryCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Import Type &Library...'
      HelpContext = 1454
      Hint = 'Import Type Library'
      ImageIndex = 42
      OnExecute = ImportTypeLibraryItemClick
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectAddRepositoryCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Add &to Repository...'
      HelpContext = 1107
      Hint = 'Add to Repository'
      ImageIndex = 108
      OnExecute = ProjectAddRepository
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectAddNewProjectCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Add &New Project...'
      HelpContext = 1111
      Hint = 'Add New Project'
      ImageIndex = 107
      OnExecute = ProjectCreatetarget
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectAddExistingProjectCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Add E&xisting Project...'
      HelpContext = 1112
      Hint = 'Add Existing Project'
      ImageIndex = 105
      OnExecute = ProjectOpenTarget
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectBuildCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Build'
      HelpContext = 1102
      Hint = 'Build'
      ImageIndex = 32
      OnExecute = ProjectBuild
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectInformationCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Information...'
      HelpContext = 1103
      Hint = 'Information'
      ImageIndex = 50
      OnExecute = ProjectInformation
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectCompileAllCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Compil&e All Projects'
      HelpContext = 1113
      Hint = 'Compile All Projects'
      ImageIndex = 106
      OnExecute = ProjectCompileAll
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectBuildAllCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'B&uild All Projects'
      HelpContext = 1114
      Hint = 'Build All Projects'
      ImageIndex = 31
      OnExecute = ProjectBuildAll
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectOptionsCommand: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Options...'
      HelpContext = 1151
      Hint = 'Project Options'
      ImageIndex = 84
      OnExecute = ProjectOptions
      OnUpdate = ProjectCommandsUpdate
    end
    object ProjectViewSourceCommand: TAction
      Category = 'Project'
      Caption = '&View Source'
      HelpContext = 1091
      Hint = 'View Project Source'
      ImageIndex = 70
      OnExecute = ViewPrjSourceItemClick
      OnUpdate = ProjectCommandsUpdate
    end
    object RunParametersCommand: TAction
      Category = 'Run'
      Caption = '&Parameters...'
      HelpContext = 1127
      Hint = 'Parameters'
      ImageIndex = 86
      OnExecute = RunParametersItemClick
      OnUpdate = RunCommandsUpdate
    end
    object RunStepOverCommand: TAction
      Category = 'Run'
      Caption = '&Step Over'
      HelpContext = 1128
      Hint = 'Step over'
      ImageIndex = 27
      OnExecute = RunStepOver
      OnUpdate = RunCommandsUpdate
    end
    object RunTraceIntoCommand: TAction
      Category = 'Run'
      Caption = '&Trace Into'
      HelpContext = 1129
      Hint = 'Trace into'
      ImageIndex = 26
      OnExecute = RunStepInto
      OnUpdate = RunCommandsUpdate
    end
    object RunTraceToSourceCommand: TAction
      Category = 'Run'
      Caption = 'Trace to &Next Source Line'
      HelpContext = 1137
      Hint = 'Trace to next Source Line'
      ImageIndex = 87
      OnExecute = RunTraceToSource
      OnUpdate = RunCommandsUpdate
    end
    object RunGotoCursorCommand: TAction
      Category = 'Run'
      Caption = 'Run to &Cursor'
      HelpContext = 1130
      Hint = 'Run to Cursor'
      ImageIndex = 85
      OnExecute = RunGotoCursor
      OnUpdate = RunCommandsUpdate
    end
    object RunShowCSIPCommand: TAction
      Category = 'Run'
      Caption = 'S&how Execution Point'
      HelpContext = 1136
      Hint = 'Show Execution Point'
      ImageIndex = 59
      OnExecute = RunShowCSIPItemClick
      OnUpdate = RunCommandsUpdate
    end
    object RunPauseCommand: TAction
      Category = 'Run'
      Caption = 'Pro&gram Pause'
      HelpContext = 1131
      Hint = 'Pause'
      ImageIndex = 25
      OnExecute = RunPause
      OnUpdate = RunCommandsUpdate
    end
    object RunResetCommand: TAction
      Category = 'Run'
      Caption = 'Program R&eset'
      HelpContext = 1132
      Hint = 'Program Reset'
      ImageIndex = 89
      OnExecute = ProgramReset
      OnUpdate = RunCommandsUpdate
    end
    object RunDetachCommand: TAction
      Category = 'Run'
      Caption = '&Detach From Program'
      HelpContext = 8000
      Hint = 'Detach From Program'
      ImageIndex = 115
      OnExecute = ProgramDetach
      OnUpdate = RunCommandsUpdate
    end
    object RunAddWatchCommand: TAction
      Category = 'Run'
      Caption = 'Add &Watch...'
      HelpContext = 1133
      Hint = 'Add Watch'
      ImageIndex = 29
      OnExecute = RunAddWatch
      OnUpdate = RunCommandsUpdate
    end
    object RunAddSourceBreakpointCommand: TAction
      Category = 'Run'
      Caption = '&Source Breakpoint...'
      HelpContext = 1560
      Hint = 'Add Source Breakpoint'
      ImageIndex = 28
      OnExecute = RunAddSourceBreakpoint
    end
    object RunAddAddressBreakpointCommand: TAction
      Category = 'Run'
      Caption = '&Address Breakpoint...'
      HelpContext = 5610
      Hint = 'Add Address Breakpoint'
      ImageIndex = 100
      OnExecute = RunAddAddressBreakpoint
    end
    object RunEvalModCommand: TAction
      Category = 'Run'
      Caption = 'E&valuate/Modify...'
      HelpContext = 1135
      Hint = 'Evaluate/Modify'
      ImageIndex = 82
      OnExecute = RunEvalModify
      OnUpdate = RunCommandsUpdate
    end
    object ComponentNewCommand: TAction
      Category = 'Component'
      Caption = '&New Component...'
      HelpContext = 1141
      Hint = 'New Component'
      ImageIndex = 47
      OnExecute = ComponentNew
      OnUpdate = ComponentCommandsUpdate
    end
    object ComponentAddtoPackageCommand: TAction
      Category = 'Component'
      Caption = '&Install Component...'
      HelpContext = 1143
      Hint = 'Install Component'
      ImageIndex = 43
      OnExecute = AddtoPackage1Click
      OnUpdate = ComponentCommandsUpdate
    end
    object ComponentImportAXCCommand: TAction
      Category = 'Component'
      Caption = 'Import Active&X Control...'
      HelpContext = 1453
      Hint = 'Import ActiveX Control'
      ImageIndex = 80
      OnExecute = ComponentImportAXC
      OnUpdate = ComponentCommandsUpdate
    end
    object ComponentInstallCompositeCommand: TAction
      Category = 'Component'
      Caption = 'Create Component &Template...'
      HelpContext = 1144
      Hint = 'Create Component'
      OnExecute = ComponentInstallComposite
      OnUpdate = ComponentCommandsUpdate
    end
    object ComponentInstallPackagesCommand: TAction
      Category = 'Component'
      Caption = 'Install &Packages...'
      HelpContext = 1145
      Hint = 'Install Packages'
      ImageIndex = 49
      OnExecute = InstallPackagesClick
      OnUpdate = ComponentCommandsUpdate
    end
    object ComponentPaletteCommand: TAction
      Category = 'Component'
      Caption = '&Configure Palette...'
      HelpContext = 1142
      Hint = 'Configure Palette'
      OnExecute = ConfigurePalette
      OnUpdate = ComponentCommandsUpdate
    end
    object ToolsOptionsCommand: TAction
      Category = 'Tools'
      Caption = 'Environment &Options...'
      HelpContext = 1152
      Hint = 'Environment Options'
      ImageIndex = 36
      OnExecute = ToolsOptions
    end
    object ToolsEditorOptionsCommand: TAction
      Category = 'Tools'
      Caption = '&Editor Options...'
      HelpContext = 1159
      Hint = 'Editor Options'
      ImageIndex = 98
      OnExecute = ToolsEditorOptions
    end
    object ToolsDebuggerOptionsCommand: TAction
      Category = 'Tools'
      Caption = '&Debugger Options...'
      HelpContext = 1158
      Hint = 'Debugger Options'
      ImageIndex = 99
      OnExecute = ToolsDebuggerOptionsCommandExecute
    end
    object ToolsGalleryCommand: TAction
      Category = 'Tools'
      Caption = '&Repository...'
      HelpContext = 1156
      Hint = 'Repository'
      ImageIndex = 104
      OnExecute = ToolsGallery
    end
    object ToolsToolsCommand: TAction
      Category = 'Tools'
      Caption = 'Configure &Tools...'
      HelpContext = 1155
      Hint = 'Configure Tools'
      ImageIndex = 34
      OnExecute = ToolsTools
    end
    object HelpContentsCommand: TAction
      Category = 'Help'
      Caption = 'Delphi Help'
      HelpContext = 1201
      Hint = 'Help contents'
      ImageIndex = 16
      OnExecute = HelpContents
      OnUpdate = HelpCommandsUpdate
    end
    object HelpWhatsNewCommand: TAction
      Category = 'Help'
      Caption = '&What'#39's New'
      HelpContext = 1207
      Hint = 'What'#39's New'
      ImageIndex = 16
      OnExecute = HelpWhatsNewClick
      OnUpdate = HelpCommandsUpdate
    end
    object HelpGettingStartedCommand: TAction
      Category = 'Help'
      Caption = '&Getting Started'
      HelpContext = 1208
      Hint = 'Getting Started'
      ImageIndex = 16
      OnExecute = HelpGettingStartedClick
      OnUpdate = HelpCommandsUpdate
    end
    object HelpUsingPascalCommand: TAction
      Category = 'Help'
      Caption = '&Using Object Pascal'
      HelpContext = 1209
      Hint = 'Using Object Pascal'
      ImageIndex = 16
      OnExecute = HelpUsingPascalClick
      OnUpdate = HelpCommandsUpdate
    end
    object HelpDevelopingAppsCommand: TAction
      Category = 'Help'
      Caption = '&Developing Applications'
      HelpContext = 1210
      Hint = 'Developing Applications'
      ImageIndex = 16
      OnExecute = HelpDevelopingAppsClick
      OnUpdate = HelpCommandsUpdate
    end
    object HelpObjCompRefCommand: TAction
      Category = 'Help'
      Caption = '&Object and Component Reference'
      HelpContext = 1211
      Hint = 'Object and Component Reference'
      ImageIndex = 16
      OnExecute = HelpObjCompRefClick
      OnUpdate = HelpCommandsUpdate
    end
    object HelpInprisePageCommand: TAction
      Category = 'Help'
      Caption = '&Borland Home Page'
      HelpContext = 1212
      Hint = 'Borland Home Page'
      OnExecute = HelpInprisePageCommandExecute
      OnUpdate = HelpCommandsUpdate
    end
    object HelpCommunityPageCommand: TAction
      Category = 'Help'
      Caption = 'B&orland Developer Network'
      HelpContext = 1218
      Hint = 'Borland Community Page'
      OnExecute = HelpCommunityPageCommandExecute
      OnUpdate = HelpCommandsUpdate
    end
    object HelpProgGuideCommand: TAction
      Category = 'Help'
      Caption = '&Programmer'#39's Guide'
      Hint = 'Programmer'#39's Guide'
      ImageIndex = 16
      OnExecute = ProgGuide
      OnUpdate = HelpCommandsUpdate
    end
    object HelpVclRefCommand: TAction
      Category = 'Help'
      Caption = '&VCL Reference'
      Hint = 'VCL Reference'
      ImageIndex = 16
      OnExecute = VclRef
      OnUpdate = HelpCommandsUpdate
    end
    object HelpRtlRefCommand: TAction
      Category = 'Help'
      Caption = '&RTL Reference'
      Hint = 'RTL Reference'
      ImageIndex = 16
      OnExecute = RtlRef
      OnUpdate = HelpCommandsUpdate
    end
    object HelpUsingHelpCommand: TAction
      Category = 'Help'
      Caption = '&How to Use Help'
      HelpContext = 1203
      Hint = 'How to Use Help'
      ImageIndex = 74
      Visible = False
      OnExecute = HelpUsingHelp
      OnUpdate = HelpCommandsUpdate
    end
    object HelpAPICommand: TAction
      Category = 'Help'
      Caption = '&Windows API'
      HelpContext = 1206
      Hint = 'Windows API Help'
      ImageIndex = 74
      OnExecute = HelpWinAPI
      OnUpdate = HelpCommandsUpdate
    end
    object DebugBreakPointsCommand: TAction
      Category = 'Debug'
      Caption = '&Breakpoints'
      HelpContext = 1080
      Hint = 'View Breakpoints'
      ImageIndex = 65
      OnExecute = DebugBreakPointsCommandClick
    end
    object DebugCallStackCommand: TAction
      Category = 'Debug'
      Caption = 'Call &Stack'
      HelpContext = 1081
      Hint = 'View Call Stack'
      ImageIndex = 66
      OnExecute = DebugCallStackCommandClick
    end
    object DebugWatchesCommand: TAction
      Category = 'Debug'
      Caption = '&Watches'
      HelpContext = 1079
      Hint = 'View Watches'
      ImageIndex = 72
      OnExecute = DebugWatchesCommandClick
    end
    object DebugThreadsCommand: TAction
      Category = 'Debug'
      Caption = '&Threads'
      HelpContext = 1093
      Hint = 'View Threads'
      ImageIndex = 71
      OnExecute = DebugThreadsCommandClick
    end
    object DebugCPUCommand: TAction
      Category = 'Debug'
      Caption = '&CPU'
      HelpContext = 1097
      Hint = 'View CPU'
      ImageIndex = 67
      OnExecute = DebugCPUCommandClick
      OnUpdate = DebugCPUCommandUpdate
    end
    object RunUntilReturnCommand: TAction
      Category = 'Run'
      Caption = 'Run &Until Return'
      HelpContext = 1117
      Hint = 'Run Until Return'
      ImageIndex = 81
      OnExecute = RunUntilReturnCommandExecute
      OnUpdate = RunCommandsUpdate
    end
    object ViewSaveDesktopCommand: TAction
      Category = 'View'
      Caption = '&Save Desktop...'
      HelpContext = 6015
      Hint = 'Save current desktop'
      ImageIndex = 78
      OnExecute = ViewSaveDesktopCommandExecute
    end
    object ViewRuntimeDesktopCommand: TAction
      Category = 'View'
      Caption = 'Set &Debug Desktop...'
      HelpContext = 6016
      Hint = 'Set debug desktop'
      ImageIndex = 79
      OnExecute = ViewRuntimeDesktopCommandExecute
      OnUpdate = ViewRuntimeDesktopCommandUpdate
    end
    object ViewDeleteDesktopCommand: TAction
      Category = 'View'
      Caption = 'Dele&te...'
      HelpContext = 6017
      Hint = 'Delete Desktop'
      OnExecute = ViewDeleteDesktopCommandExecute
      OnUpdate = ViewRuntimeDesktopCommandUpdate
    end
    object ViewNextWindow: TAction
      Category = 'View'
      Caption = 'Next Window'
      Hint = 'Next Window'
      ImageIndex = 102
      OnExecute = ViewNextWindowExecute
    end
    object HelpWinSDKCommand: TAction
      Category = 'Help'
      Caption = '&Windows SDK'
      HelpContext = 1217
      Hint = 'Windows SDK Help'
      ImageIndex = 74
      OnExecute = HelpWinSDK
      OnUpdate = HelpCommandsUpdate
    end
    object RunRunNoDebugCommand: TPopupAction
      Category = 'Run'
      Caption = 'Run Without &Debugging'
      DropdownMenu = ProcessMenu
      Hint = 'Run Without Debugging'
      ImageIndex = 114
      Visible = False
      OnExecute = RunRunNoDebugCommandExecute
      OnUpdate = RunCommandsUpdate
    end
  end
  object ProcessMenu: TPopupMenu
    Images = ImageList1
    OnPopup = ProcessMenuPopup
    Left = 232
    Top = 88
    object NoProcesses1: TMenuItem
      Caption = '<No Processes>'
    end
  end
  object ToolbarsPopup: TPopupActionBar
    Images = ImageList1
    OnPopup = ToolbarsPopupPopup
    Left = 200
    Top = 88
    object N5: TMenuItem
      Caption = '-'
      Visible = False
    end
    object Docking1: TMenuItem
      Caption = 'Docking'
      object Active1: TMenuItem
        Caption = 'Active'
      end
      object AutoHide1: TMenuItem
        Caption = 'AutoHide'
      end
      object Alwaysontop1: TMenuItem
        Caption = 'Always on top'
      end
      object Animation1: TMenuItem
        Caption = 'Animation'
      end
      object DockArea1: TMenuItem
        Caption = 'Dock area'
        object Top1: TMenuItem
          Caption = 'Top'
        end
        object Left1: TMenuItem
          Caption = 'Left'
        end
        object Right1: TMenuItem
          Caption = 'Right'
        end
        object Bottom1: TMenuItem
          Caption = 'Bottom'
        end
        object Float1: TMenuItem
          Caption = 'Float'
        end
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Customize1: TMenuItem
      Action = ViewCustomizeCommand
    end
  end
  object ToolbarActionList: TActionList
    Left = 298
    Top = 88
    object ViewStandardCommand: TAction
      Category = 'View'
      Caption = '&Standard'
      Checked = True
      HelpContext = 1071
      OnExecute = ViewStandardCommandExecute
      OnUpdate = ViewStandardCommandUpdate
    end
    object ViewViewCommand: TAction
      Category = 'View'
      Caption = '&View'
      Checked = True
      HelpContext = 1071
      OnExecute = ViewViewCommandExecute
      OnUpdate = ViewViewCommandUpdate
    end
    object ViewDebugCommand: TAction
      Category = 'View'
      Caption = '&Debug'
      Checked = True
      HelpContext = 1071
      OnExecute = ViewDebugCommandExecute
      OnUpdate = ViewDebugCommandUpdate
    end
    object ViewCustomCommand: TAction
      Category = 'View'
      Caption = 'Cus&tom'
      Checked = True
      HelpContext = 1071
      OnExecute = ViewCustomCommandExecute
      OnUpdate = ViewCustomCommandUpdate
    end
    object ViewPaletteCommand: TAction
      Category = 'View'
      Caption = 'Component &Palette'
      Checked = True
      HelpContext = 1089
      OnExecute = ViewPaletteCommandExecute
      OnUpdate = ViewPaletteCommandUpdate
    end
    object ViewDesktopCommand: TAction
      Category = 'View'
      Caption = 'Des&ktops'
      Checked = True
      OnExecute = ViewDesktopCommandExecute
      OnUpdate = ViewDesktopCommandUpdate
    end
  end
  object ClosedFilesPopup: TPopupMenu
    Images = ImageList1
    OnPopup = ClosedFilesPopupPopup
    Left = 136
    Top = 88
  end
  object ApplicationEvents: TApplicationEvents
    OnActivate = ApplicationActivated
    OnException = ShowException
    OnIdle = Idle
    OnMessage = IsDesignMsg
    Left = 424
    Top = 88
  end
  object WindowListPopup: TPopupMenu
    HelpContext = 1175
    Images = ImageList1
    OnPopup = WindowListPopupPopup
    Left = 456
    Top = 88
    object Temp2: TMenuItem
      Caption = 'Temp'
    end
  end
end
*)
