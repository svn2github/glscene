// Info
{: Information sur le driver OpenGL courant<p>

	<b>Historique : </b><font size=-1><ul>
      <li>24/08/01 - Egg - Compatibility with new Buffer classes
		<li>17/04/00 - Egg - Creation of header, minor layout changes
	</ul></font>
}
unit Info;

interface

uses Windows, Forms, GLScene, Classes, Controls, Buttons, StdCtrls, ComCtrls,
     CommCtrl, ExtCtrls, Graphics;

type

  TInfoForm = class(TForm)
    CloseButton: TSpeedButton;
    PageControl: TPageControl;
    Sheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Sheet2: TTabSheet;
    Sheet3: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    VendorLabel: TLabel;
    AccLabel: TLabel;
    VersionLabel: TLabel;
    CopyLabel: TLabel;
    DoubleLabel: TLabel;
    Label7: TLabel;
    StereoLabel: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    ColorLabel: TLabel;
    DepthLabel: TLabel;
    StencilLabel: TLabel;
    AuxLabel: TLabel;
    AccumLabel: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    ClipLabel: TLabel;
    EvalLabel: TLabel;
    ListLabel: TLabel;
    LightLabel: TLabel;
    Label23: TLabel;
    ModelLabel: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    NameLabel: TLabel;
    PixelLabel: TLabel;
    ProjLabel: TLabel;
    TexStackLabel: TLabel;
    TexSizeLabel: TLabel;
    Label35: TLabel;
    ViewLabel: TLabel;
    SubLabel: TLabel;
    Label37: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label18: TLabel;
    OverlayLabel: TLabel;
    UnderlayLabel: TLabel;
    Label20: TLabel;
    TabSheet1: TTabSheet;
    Extensions: TMemo;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    procedure GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
  end;

implementation

uses OpenGL12, SysUtils;

{$R *.DFM}
{$R Info.res}

//------------------------------------------------------------------------------

procedure TInfoForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;

// GetInfoFrom
//
procedure TInfoForm.GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
const
   DRIVER_MASK = PFD_GENERIC_FORMAT or PFD_GENERIC_ACCELERATED;
var
   pfd            : TPixelformatDescriptor;
	i, pixelFormat : Integer;
	extStr         : String;

   procedure IntLimitToLabel(const aLabel : TLabel; const aLimit : TLimitType);
   begin
      aLabel.Caption:=IntToStr(aSceneBuffer.LimitOf[aLimit]);
   end;

begin
	Caption:=Caption+' (current context in '+(aSceneBuffer.Owner as TComponent).Name+')';
	with aSceneBuffer do begin
      // common properties
      VendorLabel.Caption:=StrPas(PChar(glGetString(GL_VENDOR)));
      PixelFormat:=GetPixelFormat(Canvas.Handle);
      DescribePixelFormat(Canvas.Handle,PixelFormat,SizeOf(pfd), PFD);
      // figure out the driver type
      if (DRIVER_MASK and pfd.dwFlags) = 0 then AccLabel.Caption:='Installable Client Driver'
        else if (DRIVER_MASK and pfd.dwFlags ) = DRIVER_MASK then AccLabel.Caption:='Mini-Client Driver'
          else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then AccLabel.Caption:='Generic Software Driver';
      VersionLabel.Caption:=StrPas(PChar(glGetString(GL_VERSION)));
      ExtStr:=PChar(glGetString(GL_EXTENSIONS));
      Extensions.Clear;
      while Length(ExtStr) > 0 do begin
        I:=Pos(' ',ExtStr);
        if I = 0 then I:=255;
        Extensions.Lines.Add(Copy(ExtStr,1,I-1));
        Delete(ExtStr,1,I);
      end;
      if DoubleBuffered then begin
        DoubleLabel.Caption:='yes';
        CopyLabel.Caption:='';
        if (pfd.dwFlags and PFD_SWAP_EXCHANGE) > 0 then CopyLabel.Caption:='exchange';
        if Length(CopyLabel.Caption) > 0 then CopyLabel.Caption:=CopyLabel.Caption+', ';
        if (pfd.dwFlags and PFD_SWAP_COPY) > 0 then CopyLabel.Caption:=CopyLabel.Caption+'copy';
        if Length(CopyLabel.Caption) = 0 then CopyLabel.Caption:='no info available';
      end else begin
        DoubleLabel.Caption:='no';
        CopyLabel.Caption:='n/a';
      end;
      if (pfd.dwFlags and PFD_STEREO) > 0 then
         StereoLabel.Caption:='yes'
      else StereoLabel.Caption:='no';
      // buffer and pixel depths
      ColorLabel.Caption:=Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
                                 [LimitOf[limRedBits], LimitOf[limGreenBits],
                                  LimitOf[limBlueBits], LimitOf[limAlphaBits]]);
      DepthLabel.Caption:=Format('%d bits', [LimitOf[limDepthBits]]);
      StencilLabel.Caption:=Format('%d bits', [LimitOf[limStencilBits]]);
      AccumLabel.Caption:=Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
                                 [LimitOf[limAccumRedBits],LimitOf[limAccumGreenBits],
                                  LimitOf[limAccumBlueBits],LimitOf[limAccumAlphaBits]]);
      IntLimitToLabel(AuxLabel, limAuxBuffers);
      IntLimitToLabel(SubLabel, limSubpixelBits);
      OverlayLabel.Caption:=IntToStr(pfd.bReserved and 7);
      UnderlayLabel.Caption:=IntToStr(pfd.bReserved shr 3);

      // Maximum values
      IntLimitToLabel(ClipLabel, limClipPlanes);
      IntLimitToLabel(EvalLabel, limEvalOrder);
      IntLimitToLabel(LightLabel, limLights);
      IntLimitToLabel(ListLabel, limListNesting);
      IntLimitToLabel(ModelLabel, limModelViewStack);
      IntLimitToLabel(NameLabel, limNameStack);
      IntLimitToLabel(PixelLabel, limPixelMapTable);
      IntLimitToLabel(ProjLabel, limProjectionStack);
      IntLimitToLabel(TexSizeLabel, limTextureSize);
      IntLimitToLabel(TexStackLabel, limTextureStack);
      IntLimitToLabel(ViewLabel, limViewportDims);
   end;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormCreate(Sender: TObject);

// quite much code just to display a background image in the
// page control with correct windows colors

var I         : Integer;
	 NewR,
	 NewG,
    NewB      : Integer;
    r,g,b     : Byte;
    X, Y      : Integer;
    BM        : TBitmap;
	 OldColors,
    NewColors  : array[Byte] of TColor;

begin
  PageControl.ActivePage:=Sheet1;
  r:=GetRValue(ColorToRGB(clBtnFace));
  g:=GetGValue(ColorToRGB(clBtnFace));
  b:=GetBValue(ColorToRGB(clBtnFace));
  BM:=TBitmap.Create;
  // translate bitmap colors from gray-ramp to a clBtnFace-ramp 
  try
    for I:=0 to 255 do
    begin
      OldColors[I]:=RGB(I,I,I);
      // 175 instead of 255 to make the image a bit lighter
      NewR:=Round(I*r/175); if NewR > r then NewR:=r;
      NewG:=Round(I*g/175); if NewG > g then NewG:=g;
      NewB:=Round(I*b/175); if NewB > b then NewB:=b;
      NewColors[I]:=RGB(NewR,NewG,NewB);
    end;
    BM.Handle:=CreateMappedRes(HInstance,'INFO_BACK',OldColors,NewColors);
	 for Y:=0 to Image1.Height div BM.Height do
      for X:=0 to Image1.Width div BM.Width do Image1.Canvas.Draw(X*BM.Width,Y*BM.Height,BM);
    Image2.Picture:=Image1.Picture;
    Image3.Picture:=Image1.Picture;
  finally
    BM.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then Close;
end;

//------------------------------------------------------------------------------

end.

