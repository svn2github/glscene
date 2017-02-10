object frmPoleBalancer: TfrmPoleBalancer
  Left = 640
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Pole balancer'
  ClientHeight = 662
  ClientWidth = 632
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
  DesignSize = (
    632
    662)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 272
    Top = 200
    Width = 59
    Height = 13
    Caption = 'Description :'
  end
  object Label_Description: TLabel
    Left = 336
    Top = 200
    Width = 37
    Height = 13
    Caption = '(none)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 200
    Width = 29
    Height = 13
    Caption = 'Time :'
  end
  object LabelTime: TLabel
    Left = 40
    Top = 200
    Width = 37
    Height = 13
    Caption = '(none)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 88
    Top = 200
    Width = 61
    Height = 13
    Caption = 'Generation : '
  end
  object Label_Generation: TLabel
    Left = 160
    Top = 200
    Width = 8
    Height = 13
    Alignment = taRightJustify
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 176
    Top = 200
    Width = 39
    Height = 13
    Caption = 'Fitness :'
  end
  object Label_Fitness: TLabel
    Left = 216
    Top = 200
    Width = 37
    Height = 13
    Caption = '(none)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 616
    Height = 185
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      616
      185)
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 600
      Height = 169
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = True
    end
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 224
    Width = 617
    Height = 428
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Best individual'
      object Memo_Individual: TMemo
        Left = 0
        Top = 0
        Width = 609
        Height = 400
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Info'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_Info: TMemo
        Left = 0
        Top = 0
        Width = 486
        Height = 400
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object CheckBox5: TCheckBox
    Left = 145
    Top = 224
    Width = 105
    Height = 17
    Action = Action_OnlyDrawWinners
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 257
    Top = 224
    Width = 73
    Height = 17
    Action = Action_NoDraw
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object Button1: TButton
    Left = 536
    Top = 216
    Width = 81
    Height = 21
    Action = Action_SkipAnimation
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object CheckBox_AllowGaussian: TCheckBox
    Left = 320
    Top = 224
    Width = 97
    Height = 17
    Caption = 'Allow Gaussian'
    TabOrder = 5
    OnClick = CheckBox_AllowGaussianClick
  end
  object ActionManager1: TActionManager
    Left = 280
    Top = 96
    StyleName = 'XP Style'
    object Action_OnlyDrawWinners: TAction
      Category = 'Draw'
      Caption = 'Only draw winners'
      OnExecute = Action_InvertChecked
    end
    object Action_SkipAnimation: TAction
      Category = 'Draw'
      Caption = 'S&kip animation'
      Enabled = False
      OnExecute = Action_SkipAnimationExecute
    end
    object Action_NoDraw: TAction
      Category = 'Draw'
      Caption = '&No draw'
      OnExecute = Action_InvertChecked
    end
    object Action_FitnessLine: TAction
      Category = 'Draw'
      Caption = 'Fitness monitor'
    end
  end
  object MainMenu1: TMainMenu
    Left = 272
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Draw1: TMenuItem
      Caption = '&Draw'
      object Onlydrawwinners1: TMenuItem
        Action = Action_OnlyDrawWinners
      end
      object Stopanimation1: TMenuItem
        Action = Action_SkipAnimation
      end
      object Nodraw1: TMenuItem
        Action = Action_NoDraw
      end
      object Fitnessline1: TMenuItem
        Action = Action_FitnessLine
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
      end
    end
  end
  object NEATPopulation: TNEATPopulation
    SpeciesTargetCount = 0
    ReproduceMutateRate = 0.250000000000000000
    MinSpeciesRelatedness = 2.500000000000000000
    InputNodeCount = 3
    OutputNodeCount = 2
    GenerationsToRun = 1000
    TargetFitness = 1.000000000000000000
    ConnectionWeightMutationChance = 0.800000000000000000
    ConnectionMutationBigChangeChance = 0.100000000000000000
    ActivationIterations = 2
    SurvivalThreshold = 0.200000000000000000
    ConnectionSplitChance = 0.030000000000000000
    ConnectionAddChance = 0.050000000000000000
    WeightUnrelatednessFactor = 0.400000005960464400
    WeightPeturbationFactor = 2.500000000000000000
    InitialWeightMagnitude = 1.000000000000000000
    MaxLinkWeight = 3.000000000000000000
    OnCalculateFitness = NEATPopulationCalculateFitness
    OnShowBestIndividual = NEATPopulationShowBestIndividual
    Left = 40
    Top = 32
  end
end
