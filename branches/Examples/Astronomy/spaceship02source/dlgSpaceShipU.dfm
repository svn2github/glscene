object dlgSpaceShip: TdlgSpaceShip
  Left = 242
  Top = 162
  Caption = 'Space Ship'
  ClientHeight = 441
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 443
    Height = 400
    ActivePage = TabSheet1
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Mission selection'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 435
        Height = 137
        Align = alTop
        Caption = 'Mission selection'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 20
          Width = 79
          Height = 13
          Caption = 'Select a mission:'
        end
        object lbMissions: TListBox
          Left = 2
          Top = 38
          Width = 431
          Height = 97
          Align = alBottom
          ItemHeight = 13
          Items.Strings = (
            'Rally the space station in the middle of an asteroid field')
          TabOrder = 0
          OnClick = lbMissionsClick
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 137
        Width = 435
        Height = 217
        Align = alClient
        Caption = 'Mission description'
        TabOrder = 1
        object mmMission: TMemo
          Left = 2
          Top = 15
          Width = 431
          Height = 200
          Align = alClient
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Flight school'
      ImageIndex = 5
      object mmFlightSchool: TMemo
        Left = 0
        Top = 0
        Width = 435
        Height = 354
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Controls'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmControls: TMemo
        Left = 0
        Top = 0
        Width = 443
        Height = 366
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Physic parameters'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 443
        Height = 89
        Align = alTop
        Caption = 'Gyroscopic forces'
        TabOrder = 0
        object Label2: TLabel
          Left = 9
          Top = 24
          Width = 95
          Height = 13
          Hint = 'Inertia as a fraction of roll inertia'
          Alignment = taRightJustify
          Caption = 'Pitch relative inertia:'
        end
        object lblPitchInertia: TLabel
          Left = 256
          Top = 24
          Width = 15
          Height = 13
          Caption = '1.0'
        end
        object Label3: TLabel
          Left = 12
          Top = 56
          Width = 92
          Height = 13
          Hint = 'Inertia as a fraction of roll inertia'
          Alignment = taRightJustify
          Caption = 'Yaw relative inertia:'
        end
        object lblYawInertia: TLabel
          Left = 256
          Top = 56
          Width = 15
          Height = 13
          Caption = '1.0'
        end
        object tbPitchInertia: TTrackBar
          Left = 104
          Top = 16
          Width = 150
          Height = 33
          Hint = 'Inertia as a fraction of roll inertia'
          Max = 5
          Min = -5
          TabOrder = 0
          ThumbLength = 15
          TickMarks = tmBoth
          OnChange = tbPitchInertiaChange
        end
        object tbYawInertia: TTrackBar
          Left = 104
          Top = 48
          Width = 150
          Height = 33
          Hint = 'Inertia as a fraction of roll inertia'
          Max = 5
          Min = -5
          TabOrder = 1
          ThumbLength = 15
          TickMarks = tmBoth
          OnChange = tbYawInertiaChange
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Display parameters'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox4: TGroupBox
        Left = 0
        Top = 65
        Width = 443
        Height = 64
        Align = alTop
        Caption = 'Image complexity'
        TabOrder = 0
        object Label5: TLabel
          Left = 11
          Top = 24
          Width = 69
          Height = 13
          Hint = 'Inertia as a fraction of roll inertia'
          Alignment = taRightJustify
          Caption = 'Level of detail:'
        end
        object tbLOD: TTrackBar
          Left = 80
          Top = 16
          Width = 150
          Height = 33
          Hint = 'Inertia as a fraction of roll inertia'
          Min = 1
          Position = 3
          TabOrder = 0
          ThumbLength = 15
          TickMarks = tmBoth
          OnChange = tbPitchInertiaChange
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 0
        Width = 443
        Height = 65
        Align = alTop
        Caption = 'Environment'
        TabOrder = 1
        object Label4: TLabel
          Left = 27
          Top = 24
          Width = 77
          Height = 13
          Hint = 'Inertia as a fraction of roll inertia'
          Alignment = taRightJustify
          Caption = 'Asteroid density:'
        end
        object seAsteroids: TSpinEdit
          Left = 104
          Top = 24
          Width = 65
          Height = 22
          MaxValue = 500
          MinValue = 50
          TabOrder = 0
          Value = 200
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'About'
      ImageIndex = 3
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 435
        Height = 354
        Align = alClient
        Lines.Strings = (
          'Technical information'
          '====================='
          '- First person camera'
          
            '- Realistic physics and behaviour (but no gyroscopic effect yet ' +
            'implemented)'
          '- Fractal asteroids'
          
            '- Two-steps level-of-detail, triggered by object aspect as seen ' +
            'from the camera'
          
            '- Collision detection between spaceship and other objects (rayca' +
            'sting and octree '
          'method)'
          
            '- Runs at 20 FPS on my machine with 200 asteroids and LOD thresh' +
            'olds set at 4'#176' and '
          '2'#176'. '
          'Should probably be considered a minimum! Tell me what you get.'
          ''
          'Planned improvements'
          '===================='
          
            '- Damages when hitting an asteroid (so far, the ship is just bou' +
            'ncing wildly)'
          '- Add various indicators on the HUD: speed, acceleration, etc.'
          '- Improve the external environment'
          
            '- Other missions: flying in a gravity field, landing and take-of' +
            'f from'
          
            'flat asteroids, piloting in the vicinity of a black-hole, space ' +
            'rendez-'
          'vous, etc.'
          ''
          'Credits'
          '======='
          '- Conceived and developped by Alexandre Hirzel, (c) 2003'
          '- Developped with Eric Grange'#39's GLScene, Delphi 6'
          
            '- Benefited greatly from the help of the much friendly GLScene c' +
            'ommunity.'
          '- Freeware and open-source'
          '')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 400
    Width = 443
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      443
      41)
    object btStart: TBitBtn
      Left = 344
      Top = 8
      Width = 99
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Start mission'
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btStartClick
    end
    object btExit: TBitBtn
      Left = 240
      Top = 8
      Width = 99
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Exit'
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
