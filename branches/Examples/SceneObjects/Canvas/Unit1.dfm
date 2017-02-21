object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 596
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 650
    Height = 596
    Camera = GLCamera1
    PostRender = GLSceneViewer1PostRender
    FieldOfView = 89.616752624511720000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
      Material.Texture.TextureMode = tmReplace
    end
    object DCBlueLight: TGLDummyCube
      Direction.Coordinates = {3ACD133F3ACD133F3ACD133F00000000}
      Up.Coordinates = {EB05D1BEEB05513FEB05D1BE00000000}
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {00000000000000000000803F0000803F}
        Position.Coordinates = {0000A04100000000000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 300.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {0000404000000040000000400000803F}
    end
  end
end
