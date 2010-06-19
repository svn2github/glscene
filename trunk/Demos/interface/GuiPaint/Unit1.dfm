object Form1: TForm1
  Left = 182
  Top = 91
  Caption = 'Gui Paint'
  ClientHeight = 446
  ClientWidth = 499
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 499
    Height = 446
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 154.724838256835900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GuiRoot: TGLBaseControl
      Autosize = False
      RedrawAtOnce = False
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 10000.000000000000000000
      Height = 10000.000000000000000000
      object GLForm1: TGLForm
        Autosize = False
        RedrawAtOnce = False
        GuiLayout = GLGuiLayout1
        GuiLayoutName = 'form'
        AlphaChannel = 0.500000000000000000
        NoZWrite = False
        DoChangesOnProgress = False
        Width = 300.000000000000000000
        Height = 300.000000000000000000
        Left = 100.000000000000000000
        Top = 100.000000000000000000
        Position.Coordinates = {0000C8420000C842000000000000803F}
        BitmapFont = WindowsBitmapFont1
        DefaultColor = clMaroon
        Caption = 'Paint'
        TitleColor = clWhite
        OnMoving = GLForm1Moving
        TitleOffset = 2.000000000000000000
        object PenButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 40.000000000000000000
          Height = 40.000000000000000000
          Left = 10.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000DC4200000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 1
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Pen'
          Pressed = True
          OnButtonClick = PenButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 32.000000000000000000
          LogicHeight = 34.000000000000000000
          YOffset = 1.000000000000000000
          AllowUp = True
        end
        object BrushButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 40.000000000000000000
          Height = 40.000000000000000000
          Left = 50.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000164300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 1
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Brush'
          Pressed = False
          OnButtonClick = BrushButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 32.000000000000000000
          LogicHeight = 34.000000000000000000
          YOffset = 1.000000000000000000
          AllowUp = True
        end
        object GLPanel1: TGLPanel
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 280.000000000000000000
          Height = 214.000000000000000000
          Left = 10.000000000000000000
          Top = 76.000000000000000000
          Position.Coordinates = {0000DC4200003043000000000000803F}
          object GLCanvas: TGLCustomControl
            Autosize = False
            RedrawAtOnce = False
            GuiLayout = GLGuiLayout1
            GuiLayoutName = 'button'
            NoZWrite = False
            DoChangesOnProgress = False
            Width = 274.000000000000000000
            Height = 208.000000000000000000
            Left = 3.000000000000000000
            Top = 3.000000000000000000
            Position.Coordinates = {0000E24200003343000000000000803F}
            OnMouseDown = GLCanvasMouseDown
            OnMouseMove = GLCanvasMouseMove
            OnMouseUp = GLCanvasMouseUp
            OnAcceptMouseQuery = GLCanvasAcceptMouseQuery
            BitmapFont = WindowsBitmapFont1
            DefaultColor = clBlack
            Focused = False
            FocusedColor = clBlack
            OnRender = GLCanvasRender
            Centered = False
            MaxInvalidRenderCount = 0
          end
        end
        object WhiteButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 90.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {00003E4300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'White'
          Pressed = False
          OnButtonClick = WhiteButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object BlackButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 110.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000524300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Black'
          Pressed = True
          OnButtonClick = BlackButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object RedButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 130.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000664300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Red'
          Pressed = False
          OnButtonClick = RedButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object GreenButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 150.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {00007A4300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Green'
          Pressed = False
          OnButtonClick = GreenButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object BlueButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 170.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000874300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Blue'
          Pressed = False
          OnButtonClick = BlueButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000A04000004040000080400000803F}
      Left = 240
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 80
  end
  object WindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 56
    Top = 48
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
    end
    object Font1: TMenuItem
      Caption = 'Font'
      object WindowsFont1: TMenuItem
        Caption = 'Set New Font'
        OnClick = WindowsFont1Click
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 112
    Top = 16
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = WindowsBitmapFont1
    Material.MaterialLibrary = GLMaterialLibrary1
    Material.LibMaterialName = 'Gui'
    GuiComponents = <
      item
        Elements = <
          item
            TopLeft.Coordinates = {00004041000084420000000000000000}
            BottomRight.Coordinates = {000018420000B8420000000000000000}
            Align = GLAlCenter
            Name = 'center1'
          end
          item
            BottomRight.Coordinates = {000088410000F0410000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {00008841000000000000000000000000}
            BottomRight.Coordinates = {0000E0420000F0410000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {000000000000F8410000000000000000}
            BottomRight.Coordinates = {000000410000DA420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {0000E042000000000000000000000000}
            BottomRight.Coordinates = {000000430000F0410000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {0000F0420000F0410000000000000000}
            BottomRight.Coordinates = {000000430000DE420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {000000000000E4420000000000000000}
            BottomRight.Coordinates = {00006041000000430000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {000070410000F0420000000000000000}
            BottomRight.Coordinates = {0000E242000000430000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {0000E4420000E4420000000000000000}
            BottomRight.Coordinates = {00000043000000430000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'form'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {00004041000084420000000000000000}
            BottomRight.Coordinates = {00001C420000BA420000000000000000}
            Align = GLAlCenter
            Name = 'center1'
          end
          item
            TopLeft.Coordinates = {0000104100007C420000000000000000}
            BottomRight.Coordinates = {00005041000086420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {0000304100007C420000000000000000}
            BottomRight.Coordinates = {00002042000084420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {00001041000082420000000000000000}
            BottomRight.Coordinates = {000040410000BC420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {0000184200007C420000000000000000}
            BottomRight.Coordinates = {00002842000086420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00001C42000082420000000000000000}
            BottomRight.Coordinates = {000028420000BA420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {000010410000B8420000000000000000}
            BottomRight.Coordinates = {000050410000C0420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {000030410000BA420000000000000000}
            BottomRight.Coordinates = {000020420000C0420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {000018420000B8420000000000000000}
            BottomRight.Coordinates = {000028420000C0420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'panel'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000024420000F8410000000000000000}
            BottomRight.Coordinates = {00003442000018420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {000034420000F8410000000000000000}
            BottomRight.Coordinates = {00005442000014420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {000054420000F8410000000000000000}
            BottomRight.Coordinates = {00006442000018420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00002442000018420000000000000000}
            BottomRight.Coordinates = {00003442000030420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00003442000014420000000000000000}
            BottomRight.Coordinates = {0000544200002C420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {00005442000018420000000000000000}
            BottomRight.Coordinates = {00006442000030420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {00002442000030420000000000000000}
            BottomRight.Coordinates = {00003442000044420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00003442000030420000000000000000}
            BottomRight.Coordinates = {00005442000044420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {00005442000030420000000000000000}
            BottomRight.Coordinates = {00006442000044420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'button'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000068420000F8410000000000000000}
            BottomRight.Coordinates = {00007842000014420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {000078420000F8410000000000000000}
            BottomRight.Coordinates = {00008C42000014420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {00008C420000F8410000000000000000}
            BottomRight.Coordinates = {00009442000014420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00006842000014420000000000000000}
            BottomRight.Coordinates = {00007842000030420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00007842000018420000000000000000}
            BottomRight.Coordinates = {00008C42000030420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {00008C42000014420000000000000000}
            BottomRight.Coordinates = {00009442000030420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {00006842000030420000000000000000}
            BottomRight.Coordinates = {00007842000044420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00007842000030420000000000000000}
            BottomRight.Coordinates = {00008C42000044420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {00008C42000030420000000000000000}
            BottomRight.Coordinates = {00009442000044420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'buttonpressed'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {0000B44200003C420000000000000000}
            BottomRight.Coordinates = {0000D042000074420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end>
        Name = 'checkbox'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {0000B4420000F8410000000000000000}
            BottomRight.Coordinates = {0000D042000034420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end>
        Name = 'checkboxchecked'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {0000964200003C420000000000000000}
            BottomRight.Coordinates = {0000B242000074420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end>
        Name = 'radiobutton'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000096420000F8410000000000000000}
            BottomRight.Coordinates = {0000B242000034420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end>
        Name = 'radiobuttonchecked'
      end>
    FileName = '..\..\media\default.layout'
    Left = 176
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Gui'
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '..\..\media\defaultskin.bmp'
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Tag = 0
      end
      item
        Name = 'Brush'
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '..\..\media\brush.bmp'
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Tag = 0
      end
      item
        Name = 'Pen'
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '..\..\media\pen.bmp'
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Tag = 0
      end
      item
        Name = 'White'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmReplace
        Tag = 0
      end
      item
        Name = 'Black'
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.Texture.TextureMode = tmReplace
        Tag = 0
      end
      item
        Name = 'Red'
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Tag = 0
      end
      item
        Name = 'Green'
        Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
        Tag = 0
      end
      item
        Name = 'Blue'
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Tag = 0
      end>
    Left = 144
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps|*.bmp'
    Title = 'Open Bitmap'
    Left = 16
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps|*.bmp'
    Title = 'Save Bitmap'
    Left = 16
    Top = 176
  end
end
