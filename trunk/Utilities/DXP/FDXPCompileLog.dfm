object DXPCompileLog: TDXPCompileLog
  Left = 222
  Top = 118
  Width = 568
  Height = 337
  Caption = 'DXPCompileLog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 560
    Height = 308
    ActivePage = TSCooked
    Align = alClient
    TabOrder = 0
    object TSCooked: TTabSheet
      Caption = 'Compiler Messages'
      object LVMessages: TListView
        Left = 0
        Top = 0
        Width = 552
        Height = 280
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Module'
            Width = 100
          end
          item
            Caption = 'Line, Col'
            Width = 70
          end
          item
            AutoSize = True
            Caption = 'Message'
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = LVMessagesDblClick
      end
    end
    object TSRaw: TTabSheet
      Caption = 'Raw Output'
      ImageIndex = 1
      object MERaw: TMemo
        Left = 0
        Top = 0
        Width = 512
        Height = 281
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
end
