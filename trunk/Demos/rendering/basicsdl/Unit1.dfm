object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 248
  Top = 107
  Height = 150
  Width = 215
  object GLScene1: TGLScene
    Left = 16
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180
    end
    object Teapot1: TGLTeapot
      Material.Texture.ImageClassName = 'TGLCubeMapImage'
      Material.Texture.TextureMode = tmReplace
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = Teapot1
      Position.Coordinates = {0000404000000040000000400000803F}
    end
  end
  object GLSDLViewer1: TGLSDLViewer
    Camera = GLCamera1
    Width = 640
    Height = 480
    Buffer.BackgroundColor = clBlack
    Caption = 'GLScene SDL Test'
    OnResize = GLSDLViewer1Resize
    OnEventPollDone = GLSDLViewer1EventPollDone
    Left = 96
    Top = 8
  end
end
