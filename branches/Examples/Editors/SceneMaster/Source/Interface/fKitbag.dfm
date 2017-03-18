object FormKitbag: TFormKitbag
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Kitbag'
  ClientHeight = 429
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object twBasicGeometry: TTreeView
    Left = 0
    Top = 41
    Width = 258
    Height = 112
    Align = alTop
    AutoExpand = True
    Images = DMImages.ilBasicGeometry
    Indent = 19
    TabOrder = 0
    Items.NodeData = {
      03010000003A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
      0003000000010E420061007300690063002000470065006F006D006500740072
      007900260000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
      000000010443007500620065002A0000000100000000000000FFFFFFFFFFFFFF
      FF00000000000000000000000001065300700068006500720065003400000002
      00000000000000FFFFFFFFFFFFFFFF000000000000000000000000010B540065
      0074007200610068006500640072006F006E00}
    ExplicitTop = 47
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 258
    Height = 41
    Align = alTop
    Caption = 'Objects'
    TabOrder = 1
  end
  object TreeView2: TTreeView
    Left = 0
    Top = 168
    Width = 258
    Height = 113
    AutoExpand = True
    Images = DMImages.ilMeshObjects
    Indent = 19
    TabOrder = 2
    Items.NodeData = {
      030100000036000000FFFFFFFF00000000FFFFFFFFFFFFFFFF00000000000000
      0002000000010C4D0065007300680020004F0062006A00650063007400730028
      0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000001
      054100630074006F0072002E0000000100000000000000FFFFFFFFFFFFFFFF00
      00000000000000000000000108460072006500650046006F0072006D00}
  end
end
