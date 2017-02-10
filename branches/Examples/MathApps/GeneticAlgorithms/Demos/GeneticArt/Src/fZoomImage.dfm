object frmZoomImage: TfrmZoomImage
  Left = 299
  Top = 134
  Width = 599
  Height = 476
  Caption = 'Zoom Image'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  DesignSize = (
    591
    442)
  PixelsPerInch = 96
  TextHeight = 13
  object Button_Redraw: TButton
    Left = 504
    Top = 416
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = '&Redraw'
    TabOrder = 0
    OnClick = Button_RedrawClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 577
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      577
      401)
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 577
      Height = 401
      Align = alClient
      Brush.Color = clBlack
    end
    object ScrollBox1: TScrollBox
      Left = 8
      Top = 8
      Width = 561
      Height = 385
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      object Image: TImage32
        Left = 0
        Top = 0
        Width = 561
        Height = 385
        BitmapAlign = baTopLeft
        PopupMenu = PopupMenu_Zoomed
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
      end
    end
  end
  object PopupMenu_Zoomed: TPopupMenu
    Left = 176
    Top = 136
    object SetResolution1: TMenuItem
      Caption = 'Set Resolution'
      object N640x4801: TMenuItem
        Caption = '640x480'
        OnClick = SetResolution_Click
      end
      object N800x6001: TMenuItem
        Caption = '800x600'
        OnClick = SetResolution_Click
      end
      object N1024x7681: TMenuItem
        Caption = '1024x768'
        OnClick = SetResolution_Click
      end
      object N1280x10241: TMenuItem
        Caption = '1280x1024'
        OnClick = SetResolution_Click
      end
    end
    object SaveImage1: TMenuItem
      Caption = '&Save Image'
      OnClick = SaveImage1Click
    end
  end
end
