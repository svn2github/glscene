object Form1: TForm1
  Left = 214
  Top = 276
  Caption = 'Form1'
  ClientHeight = 386
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  DesignSize = (
    652
    386)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object Label2: TLabel
    Left = 160
    Top = 8
    Width = 64
    Height = 13
    Caption = 'Render mode'
  end
  object Label3: TLabel
    Left = 360
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Panel1: TPanel
    Left = 8
    Top = 64
    Width = 633
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Panel1'
    TabOrder = 0
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 1
      Width = 631
      Height = 311
      Camera = Cam
      Buffer.BackgroundColor = clBlack
      FieldOfView = 114.510925292968800000
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseUp = GLSceneViewer1MouseUp
      TabOrder = 0
    end
  end
  object ScrollBar1: TScrollBar
    Left = 8
    Top = 32
    Width = 121
    Height = 17
    Max = 4
    Min = 1
    PageSize = 0
    Position = 4
    TabOrder = 1
    OnChange = ScrollBar1Change
  end
  object RadioButton1: TRadioButton
    Tag = 1
    Left = 256
    Top = 32
    Width = 65
    Height = 17
    Caption = 'Tri Strips'
    TabOrder = 2
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 160
    Top = 32
    Width = 89
    Height = 17
    Caption = 'Compiled Array'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton1Click
  end
  object FPSScrollBar: TScrollBar
    Left = 416
    Top = 32
    Width = 217
    Height = 17
    Min = 1
    PageSize = 0
    Position = 25
    TabOrder = 4
    Visible = False
    OnChange = FPSScrollBarChange
  end
  object CheckBox1: TCheckBox
    Left = 352
    Top = 8
    Width = 161
    Height = 17
    Caption = 'Use Frame rate from AVI file'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object PosScrollBar: TScrollBar
    Left = 8
    Top = 376
    Width = 633
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 0
    PageSize = 0
    SmallChange = 1000
    TabOrder = 6
    OnScroll = PosScrollBarScroll
  end
  object GLScene1: TGLScene
    Left = 104
    Top = 80
    object CamH: TGLDummyCube
      CubeSize = 1.000000000000000000
      object CamV: TGLDummyCube
        CubeSize = 1.000000000000000000
        object Cam: TGLCamera
          DepthOfView = 150.000000000000000000
          FocalLength = 100.000000000000000000
          TargetObject = CamH
          Position.Coordinates = {0000000000000000000000C10000803F}
          object GLLightSource1: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            SpotCutOff = 180.000000000000000000
          end
        end
      end
    end
    object GLCube1: TGLCube
      Position.Coordinates = {0000000000000000CDCC0C3F0000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 152
    Top = 152
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 96
  end
  object MainMenu1: TMainMenu
    Left = 176
    Top = 72
    object File1: TMenuItem
      Caption = 'File'
      object Openavi1: TMenuItem
        Caption = 'Open avi..'
        OnClick = Openavi1Click
      end
      object Quit1: TMenuItem
        Caption = 'Quit'
        OnClick = Quit1Click
      end
    end
  end
end
