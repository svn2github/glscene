�
 TFORM1 0@  TPF0TForm1Form1Left� Top5Width�Height�CaptionMoving Objects with MouseColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameVerdana
Font.Style 
KeyPreview	OldCreateOrderPositionpoScreenCenter
OnKeyPressFormKeyPressOnKeyUp	FormKeyUpOnMouseWheelFormMouseWheel
PixelsPerInch`
TextHeight
 TGLSceneViewerScnLeft� Top Width�Height�Camera	GLCamera1AfterRenderScnAfterRenderBuffer.BackgroundColorclBlackAlignalClientOnMouseDownScnMouseDownOnMouseMoveScnMouseMove  TPanelPanel1Left Top Width� Height�AlignalLeft
BevelOuterbvNoneColor--- TabOrder TLabelLabel2LeftTopgWidth� HeightACaption}Select and move with the mouse any of the two cubes. Default movement is on the XY plane. Shift + Drag moves on the XZ plane.Font.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameVerdana
Font.Style 
ParentFont
ShowAccelCharWordWrap	  TLabelLabel1LeftTopWidth� Height4Captionc   © Author: Rado Stoyanov
radostoyanov@softhome.net
Based on the GLScene 
Camera & Pick examples.Font.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameVerdana
Font.Style 
ParentFont
ShowAccelCharWordWrap	  TButtonButton1LeftjTopWidth� HeightCaptionNear: (0,0) Eye -> ObjTabOrder    TGLSceneGLScene1Left� Top TCubeFloorPosition.Coordinates
           \���  �?&Material.FrontProperties.Diffuse.Color
   ���>���>��?�;?CubeSize
      @   @
�#<  TGLLightSource	TopLight1
Ambient.Color
      ?   ?   ?  �?ConstAttenuation     ����?
Diffuse.Color
   ��i?��i?��i?  �?Position.Coordinates
     �A  PA  @A  �?
SpotCutOff       �@  TCubeCube1Direction.Coordinates
     �?            Position.Coordinates
   ���=���=fff�  �?Scale.Coordinates
   ��L>��L>��L>    %Material.BackProperties.Diffuse.Color
   ��?��?��0?��c?&Material.FrontProperties.Diffuse.Color
   ��?��?��0?m�[?Material.BlendingModebmTransparencyMaterial.Texture.ImageAlphatiaAlphaFromIntensityMaterial.Texture.TextureMode
tmModulate  TCubeCube2Position.Coordinates
   ��̾���>   �  �?CubeSize
     �>  �>  �>  
TDummyCube
DummyCube1Position.Coordinates
     �?  �?   ?  �?TransformationModetmParentWithPosCubeSize     ����?EdgeColor.Color
   ��]?��]?��h?  �?  
TArrowLineXArrowDirection.Coordinates
     �?            Position.Coordinates
   ���=  ��  ��  �?Up.Coordinates
       .�;�  �?    BottomRadius     
ף�?Height     �̌ @	TopRadius     
ף�?TopArrowHeadHeight     ����?TopArrowHeadRadius     ����?BottomArrowHeadHeight       ��?BottomArrowHeadRadius     ����?  
TArrowLineYArrowDirection.Coordinates
   $�L2  �?,�;3    Position.Coordinates
     �����=  ��  �?Up.Coordinates
   .�;�,�;�  �?    BottomRadius     
ף�?Height     �̌ @	TopRadius     
ף�?TopArrowHeadHeight     ����?TopArrowHeadRadius     ����?BottomArrowHeadHeight       ��?BottomArrowHeadRadius     ����?  
TArrowLineZArrowPosition.Coordinates
     ��  �����=  �?VisibleBottomRadius     
ף�?Height     �̌ @	TopRadius     
ף�?TopArrowHeadHeight     ����?TopArrowHeadRadius     ����?BottomArrowHeadHeight       ��?BottomArrowHeadRadius     ����?  
TSpaceTextTxtXDirection.Coordinates
   �5��5�c��    Position.Coordinates
   ���?  ��  ��  �?Scale.Coordinates
   ���>���>���>    Up.Coordinates
   �j�&.�;�  �?    	Extrusion     ����?Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style TextXCharacterRangestcrAlphaNum  
TSpaceTextTxtYDirection.Coordinates
   �5��5�b��    Position.Coordinates
     �����?  ��  �?Scale.Coordinates
   ���>���>���>    Up.Coordinates
    `.�;�  �?    	Extrusion     ����?Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style TextYCharacterRangestcrAlphaNum  	TGLCamera	GLCamera1DepthOfView       �@FocalLength       �@TargetObject
DummyCube1Position.Coordinates
     �A  �A  �A  �?Direction.Coordinates
   .�d�.��>        Up.Coordinates
             �?        