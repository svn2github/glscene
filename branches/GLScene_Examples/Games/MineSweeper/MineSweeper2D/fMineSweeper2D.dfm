object frmMineSweeper2D: TfrmMineSweeper2D
  Left = 369
  Top = 107
  Caption = 'Mines'
  ClientHeight = 271
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MinesPanel: TPanel
    Left = 7
    Top = 86
    Width = 159
    Height = 169
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object PanelStatus: TPanel
    Left = 0
    Top = 0
    Width = 181
    Height = 81
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 406
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 179
      Height = 79
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BiDiMode = bdRightToLeft
      Caption = 'Status'
      ParentBiDiMode = False
      TabOrder = 0
      ExplicitWidth = 426
      DesignSize = (
        179
        79)
      object Label_GameState: TLabel
        Left = 8
        Top = 48
        Width = 161
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '(none)'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        OnClick = Label_GameStateClick
        OnMouseEnter = Label_GameStateMouseEnter
        OnMouseLeave = Label_GameStateMouseLeave
        ExplicitWidth = 349
      end
      object Label_GameTime: TLabel
        Left = 136
        Top = 16
        Width = 34
        Height = 24
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = '000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 324
      end
      object Label_MinesLeft: TLabel
        Left = 8
        Top = 16
        Width = 34
        Height = 24
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = '000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object Timer_GameTimer: TTimer
    Interval = 150
    OnTimer = Timer_GameTimerTimer
    Left = 72
    Top = 104
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 208
    object File1: TMenuItem
      Caption = '&Game'
      object Restart1: TMenuItem
        Action = Action_Restart
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Beginner1: TMenuItem
        Action = Action_BeginnerGame
      end
      object Intermediate1: TMenuItem
        Action = Action_Intermediate
      end
      object Advanced1: TMenuItem
        Action = Action_AdvancedGame
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Showmoves1: TMenuItem
        Action = Action_ShowMoves
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = Action_Exit
      end
    end
  end
  object ActionList1: TActionList
    Left = 72
    Top = 152
    object Action_BeginnerGame: TAction
      Category = 'GameTypes'
      Caption = '&Beginner'
      OnExecute = Action_BeginnerGameExecute
    end
    object Action_Exit: TAction
      Caption = '&Exit'
      OnExecute = Action_ExitExecute
    end
    object Action_Intermediate: TAction
      Category = 'GameTypes'
      Caption = '&Intermediate'
      OnExecute = Action_IntermediateExecute
    end
    object Action_Restart: TAction
      Caption = '&Restart'
      ShortCut = 113
      OnExecute = Action_RestartExecute
    end
    object Action_AdvancedGame: TAction
      Category = 'GameTypes'
      Caption = '&Advanced'
      OnExecute = Action_AdvancedGameExecute
    end
    object Action_ShowMoves: TAction
      Caption = 'Show moves'
      OnExecute = Action_ShowMovesExecute
    end
  end
end
