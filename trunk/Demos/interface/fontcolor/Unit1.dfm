object Form1: TForm1
  Left = 219
  Top = 105
  Width = 370
  Height = 299
  Caption = 'Bitmap Text with Color Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 362
    Height = 272
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 72
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000004000000040000000400000803F}
      SpotCutOff = 180
    end
    object Teapot1: TTeapot
      Material.FrontProperties.Diffuse.Color = {9D9C1C3FB3B2323F0000803F0000803F}
    end
    object HUDText1: THUDText
      Position.Coordinates = {000034430000F042000000000000803F}
      BitmapFont = BitmapFont
      Text = 'FADING OUT'
      Alignment = taCenter
      Layout = tlTop
      ModulateColor.Color = {0000803F0000803F0000803F3333333F}
    end
    object HUDText2: THUDText
      Position.Coordinates = {000034430000DC42000000000000803F}
      BitmapFont = BitmapFont
      Text = 'THE END'
      Alignment = taCenter
      Layout = tlTop
      ModulateColor.Color = {0000803FF8FEFE3E000000003333333F}
    end
    object HUDText3: THUDText
      Position.Coordinates = {000034430000A041000000000000803F}
      BitmapFont = BitmapFont
      Text = 'RED RED'
      Alignment = taCenter
      Layout = tlTop
      ModulateColor.Color = {0000803F00000000000000000000803F}
    end
    object HUDText4: THUDText
      Position.Coordinates = {0000344300002043000000000000803F}
      BitmapFont = BitmapFont
      Text = 'TRANSPARENT'
      Alignment = taCenter
      Layout = tlTop
      ModulateColor.Color = {9A99593F9A99593FCDCCCC3DCDCCCC3E}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Teapot1
      Position.Coordinates = {0000803F0000003F0000803F0000803F}
    end
  end
  object BitmapFont: TBitmapFont
    GlyphsIntervalX = 7
    GlyphsIntervalY = 0
    Ranges = <
      item
        StartASCII = 'A'
        StopASCII = 'H'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'I'
        StopASCII = 'P'
        StartGlyphIdx = 8
      end
      item
        StartASCII = 'Q'
        StopASCII = 'X'
        StartGlyphIdx = 16
      end
      item
        StartASCII = '!'
        StopASCII = '!'
        StartGlyphIdx = 27
      end>
    CharWidth = 27
    CharHeight = 32
    HSpace = 0
    Left = 8
    Top = 104
  end
  object GLTimeEventsMGR1: TGLTimeEventsMGR
    Cadencer = GLCadencer1
    Events = <
      item
        Name = 'Event0'
        StartTime = 1.5
        EndTime = 3
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events0Event
      end
      item
        Name = 'Event1'
        OnEvent = GLTimeEventsMGR1Events1Event
      end
      item
        Name = 'Event2'
        StartTime = 2
        EndTime = 10
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events2Event
      end>
    Left = 8
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 8
    Top = 40
  end
end
