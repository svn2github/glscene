object Form1: TForm1
  Left = 209
  Top = 106
  Width = 511
  Height = 346
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 503
    Height = 317
    Camera = Camera1
    Buffer.BackgroundColor = clSilver
    Align = alClient
  end
  object GLScene1: TGLScene
    Left = 464
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object Cube1: TGLCube
      Position.Coordinates = {000040C000000000000000000000803F}
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
      Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
    end
    object Cube2: TGLCube
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
      Material.FrontProperties.Emission.Color = {000000000000803F000000000000803F}
    end
    object Cube3: TGLCube
      Position.Coordinates = {0000404000000000000000000000803F}
      Scale.Coordinates = {0000803F0000A0400000803F00000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
    end
    object Camera1: TGLCamera
      DepthOfView = 1000000
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000000000000000000020410000803F}
      Direction.Coordinates = {000080BF000000000000000000000000}
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 40
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 464
    Top = 72
  end
  object GLTimeEventsMGR1: TGLTimeEventsMGR
    Cadencer = GLCadencer1
    FreeEventOnEnd = True
    Events = <
      item
        Name = 'Event0'
        StartTime = 2
        EndTime = 5
        EventType = etContinuous
        OnEvent = GLTimeEventsMGR1Events0Event
      end
      item
        Name = 'Event1'
        StartTime = 5
        EndTime = 10
        Period = 0.01
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events1Event
      end
      item
        Name = 'Event2'
        StartTime = 10
        OnEvent = GLTimeEventsMGR1Events2Event
      end
      item
        Name = 'Event3'
        StartTime = 15
        EndTime = -1
        Period = 0.5
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events3Event
      end
      item
        Name = 'Event4'
        StartTime = 15
        EndTime = -1
        Period = 0.1
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events4Event
      end
      item
        Name = 'Event5'
        StartTime = 15
        EndTime = -1
        Period = 0.01
        EventType = etPeriodic
        OnEvent = GLTimeEventsMGR1Events5Event
      end>
    Left = 464
    Top = 104
  end
end
