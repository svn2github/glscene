object RTrackBarEdit: TRTrackBarEdit
  Left = 0
  Top = 0
  Width = 193
  Height = 20
  AutoSize = True
  TabOrder = 0
  object TrackBar: TTrackBar
    Left = 0
    Top = 0
    Width = 150
    Height = 20
    Max = 255
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 32
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 0
    ThumbLength = 10
    TickMarks = tmTopLeft
    TickStyle = tsAuto
    OnChange = TrackBarChange
  end
  object Edit: TEdit
    Left = 152
    Top = 0
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = EditChange
    OnExit = TrackBarChange
  end
end
