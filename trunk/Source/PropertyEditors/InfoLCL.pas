// Info
{: Informations on OpenGL driver.<p>

	<b>History : </b><font size=-1><ul>

	</ul></font>
}
unit InfoLCL;

interface

{$i GLScene.inc}


uses
  lresources,
  {$IFDEF MSWINDOWS} Windows,{$ENDIF}  Forms, GLScene, Classes, Controls, Buttons, StdCtrls, ComCtrls,
  ExtCtrls, Graphics, Menus;


type

  TInfoForm = class(TForm)
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
    Label18: TLabel;
    OverlayLabel: TLabel;
    UnderlayLabel: TLabel;
    Label20: TLabel;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    TexUnitsLabel: TLabel;
    Extensions: TListBox;
    Label13: TLabel;
    RendererLabel: TLabel;
    PMWebLink: TPopupMenu;
    MIRegistryLink: TMenuItem;
    MIDelphi3D: TMenuItem;
    TabSheet2: TTabSheet;
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label30: TLabel;
    VersionLbl: TLabel;
    WebsiteLbl: TLabel;
    CloseButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExtensionsDblClick(Sender: TObject);
    procedure ExtensionsClick(Sender: TObject);
    procedure ExtensionsKeyPress(Sender: TObject; var Key: Char);
    procedure MIDelphi3DClick(Sender: TObject);
    procedure WebsiteLblClick(Sender: TObject);
  public
    procedure GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
  end;

implementation

uses
  OpenGL1x, SysUtils, GLCrossPlatform;


// ShowInfoForm
//
procedure ShowInfoForm(aSceneBuffer : TGLSceneBuffer; Modal : boolean);
var
   infoForm: TInfoForm;
begin
   infoForm:=TInfoForm.Create(nil);
   try
      infoForm.GetInfoFrom(aSceneBuffer);
      with infoForm do if Modal then ShowModal else Show;
   except
      infoForm.Free;
      raise;
   end;
end;

// CloseButtonClick
//
procedure TInfoForm.CloseButtonClick(Sender: TObject);
begin
   Close;
end;

// GetInfoFrom
//
procedure TInfoForm.GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
{$IFDEF MSWINDOWS}
const
   DRIVER_MASK = PFD_GENERIC_FORMAT or PFD_GENERIC_ACCELERATED;
{$ENDIF}
var
{$IFDEF MSWINDOWS}
   pfd            : TPixelformatDescriptor;
   pixelFormat    : Integer;
{$ENDIF}
   i              : Integer;
   ExtStr         : String;

   procedure IntLimitToLabel(const aLabel : TLabel; const aLimit : TLimitType);
   begin
      aLabel.Caption:=IntToStr(aSceneBuffer.LimitOf[aLimit]);
   end;

begin
	Caption:=Caption+' (current context in '+(aSceneBuffer.Owner as TComponent).Name+')';
	with aSceneBuffer do begin
      // common properties
      VendorLabel.Caption:=String(glGetString(GL_VENDOR));
      RendererLabel.Caption:=String(glGetString(GL_RENDERER));
      {$IFDEF MSWINDOWS}
      PixelFormat:=GetPixelFormat(Canvas.Handle);
      DescribePixelFormat(Canvas.Handle,PixelFormat,SizeOf(pfd), PFD);
      // figure out the driver type
      if (DRIVER_MASK and pfd.dwFlags) = 0 then AccLabel.Caption:='Installable Client Driver'
        else if (DRIVER_MASK and pfd.dwFlags ) = DRIVER_MASK then AccLabel.Caption:='Mini-Client Driver'
          else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then AccLabel.Caption:='Generic Software Driver';
      {$ENDIF}
      VersionLabel.Caption:=String(glGetString(GL_VERSION));
      ExtStr:=String(glGetString(GL_EXTENSIONS));
      Extensions.Clear;
      while Length(ExtStr) > 0 do begin
        I:=Pos(' ',ExtStr);
        if I = 0 then I:=255;
        Extensions.Items.Add(Copy(ExtStr,1,I-1));
        Delete(ExtStr,1,I);
      end;
      {$IFDEF MSWINDOWS}
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
      {$ENDIF}
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
      {$IFDEF MSWINDOWS}
      OverlayLabel.Caption:=IntToStr(pfd.bReserved and 7);
      UnderlayLabel.Caption:=IntToStr(pfd.bReserved shr 3);
      {$ENDIF}

      // Maximum values
      IntLimitToLabel(ClipLabel, limClipPlanes);
      IntLimitToLabel(EvalLabel, limEvalOrder);
      IntLimitToLabel(LightLabel, limLights);
      IntLimitToLabel(ListLabel, limListNesting);
      IntLimitToLabel(ModelLabel, limModelViewStack);
      IntLimitToLabel(ViewLabel, limViewportDims);

      IntLimitToLabel(NameLabel, limNameStack);
      IntLimitToLabel(PixelLabel, limPixelMapTable);
      IntLimitToLabel(ProjLabel, limProjectionStack);
      IntLimitToLabel(TexSizeLabel, limTextureSize);
      IntLimitToLabel(TexStackLabel, limTextureStack);
      IntLimitToLabel(TexUnitsLabel, limNbTextureUnits);
   end;
   VersionLbl.Caption := GLSCENE_VERSION;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then Close;
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Release;
end;

procedure TInfoForm.ExtensionsDblClick(Sender: TObject);
var
   p : Integer;
   url, buf : String;
begin
   with Extensions do begin
      if ItemIndex<0 then Exit;
      url:=Items[ItemIndex];
   end;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:=Copy(url, p+1, 255);
   if (buf<>'GL') and (buf<>'WGL') and (buf<>'GLX') then Exit;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:= 'http://www.opengl.org/registry/specs/'
        +buf+'/'+Copy(url, p+1, 255)+'.txt';
   ShowHTMLUrl(url);
end;

procedure TInfoForm.MIDelphi3DClick(Sender: TObject);
var
   url : String;
begin
   with Extensions do begin
      if ItemIndex<0 then Exit;
      url:='http://www.delphi3d.net/hardware/extsupport.php?extension='+Items[ItemIndex];
   end;
   ShowHTMLUrl(url);
end;

procedure TInfoForm.ExtensionsClick(Sender: TObject);
var
   extName : String;
begin
   if Extensions.ItemIndex<0 then
      Extensions.PopupMenu:=nil
   else begin
      Extensions.PopupMenu:=PMWebLink;
      extName:=Extensions.Items[Extensions.ItemIndex];
      MIRegistryLink.Caption:='View OpenGL Extension Registry for '+extName;
      MIDelphi3D.Caption:='View Delphi3D Hardware Registry for '+extName;
   end;
end;

procedure TInfoForm.ExtensionsKeyPress(Sender: TObject; var Key: Char);
begin
   ExtensionsClick(Sender);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TInfoForm.WebsiteLblClick(Sender: TObject);
begin
  ShowHTMLUrl(WebsiteLbl.Caption);
end;

initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

  {$i InfoLCL.lrs}

   RegisterInfoForm(ShowInfoForm);

end.




