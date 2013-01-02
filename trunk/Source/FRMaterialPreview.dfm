object RMaterialPreview: TRMaterialPreview
  Left = 0
  Top = 0
  Width = 202
  Height = 203
  AutoSize = True
  TabOrder = 0
  object CBObject: TComboBox
    Left = 0
    Top = 0
    Width = 60
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = CBObjectChange
    Items.Strings = (
      'Cube'
      'Sphere'
      'Cone'
      'Teapot')
  end
  object CBBackground: TComboBox
    Left = 60
    Top = 0
    Width = 142
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CBBackgroundChange
    Items.Strings = (
      'on a pattern background'
      'on a white background'
      'on a black background'
      'on a blue background'
      'on a red background'
      'on a green background')
  end
end
