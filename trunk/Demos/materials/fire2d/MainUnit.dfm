object Form1: TForm1
  Left = 92
  Top = 62
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  AutoSize = True
  BorderStyle = bsDialog
  Caption = '2D Fire Demo'
  ClientHeight = 392
  ClientWidth = 664
  Color = clBtnFace
  Constraints.MinHeight = 256
  Constraints.MinWidth = 256
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 241
    Height = 14
    Caption = 'The 2D fire animation below is a pure 2D animated'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 230
    Height = 14
    Caption = 'texture using Graphics32 (http://www.g32.org)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 227
    Height = 13
    Caption = 'It is refreshed every 40 ms and used to update'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 192
    Height = 13
    Caption = 'the cube'#39's texture you see on the right.'
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 206
    Height = 19
    Caption = 'Dynamic Textures : 2D Fire'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 272
    Top = 0
    Width = 392
    Height = 392
    Camera = GLCamera1
    Buffer.BackgroundColor = 3881787
    Buffer.Lighting = False
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
  end
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 136
    Width = 256
    Height = 256
    TabOrder = 1
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
  end
  object AsyncTimer1: TAsyncTimer
    Interval = 80
    OnTimer = AsyncTimer1Timer
    Left = 8
    Top = 144
  end
  object GLScene1: TGLScene
    Left = 288
    Top = 48
    object Cube1: TGLCube
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Compression = tcNone
      Material.Texture.Disabled = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      SceneScale = 1.5
      TargetObject = Cube1
      Position.Coordinates = {0000804000000000000000400000803F}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 288
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 288
    Top = 88
  end
end
