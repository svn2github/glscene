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

  { TInfoForm }

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
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExtensionsDblClick(Sender: TObject);
    procedure ExtensionsClick(Sender: TObject);
    procedure ExtensionsKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure MIDelphi3DClick(Sender: TObject);
    procedure WebsiteLblClick(Sender: TObject);
  public
    procedure GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
  end;

implementation

uses
  {$IFDEF Linux}xlib,{$ENDIF}OpenGL1x, SysUtils, GLCrossPlatform;


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
   dc             : HDC;
{$ENDIF}
   i              : Integer;
   ExtStr         : String;

   procedure IntLimitToLabel(const aLabel : TLabel; const aLimit : TLimitType);
   begin
      aLabel.Caption:=IntToStr(aSceneBuffer.LimitOf[aLimit]);
   end;

begin
	Caption:=Caption+' (current context in '+(aSceneBuffer.Owner as TComponent).Name+')';
        aSceneBuffer.RenderingContext.Activate;
	with aSceneBuffer do begin
      // common properties
      VendorLabel.Caption:=String(glGetString(GL_VENDOR));
      RendererLabel.Caption:=String(glGetString(GL_RENDERER));

      {$IFDEF MSWINDOWS}
      Label2.Show;
      AccLabel.Show;
      dc := wglGetCurrentDC();
      PixelFormat:=GetPixelFormat(dc);
      DescribePixelFormat(dc,PixelFormat,SizeOf(pfd), PFD);
      // figure out the driver type
      if (DRIVER_MASK and pfd.dwFlags) = 0 then AccLabel.Caption:='Installable Client Driver'
        else if (DRIVER_MASK and pfd.dwFlags ) = DRIVER_MASK then AccLabel.Caption:='Mini-Client Driver'
          else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then AccLabel.Caption:='Generic Software Driver';
      {$ENDIF}
      VersionLabel.Caption:=
      {$IFDEF Linux}
      'GLX '+String(glXQueryServerString(glXGetCurrentDisplay(),
                                          XDefaultScreen(glXGetCurrentDisplay())
                                          ,GLX_VERSION))+', OpenGL '+
      {$ENDIF}
      String(glGetString(GL_VERSION));

      ExtStr:=String(glGetString(GL_EXTENSIONS));
      Extensions.Clear;
      while Length(ExtStr) > 0 do begin
        I:=Pos(' ',ExtStr);
        if I = 0 then I:=255;
        Extensions.Items.Add(Copy(ExtStr,1,I-1));
        Delete(ExtStr,1,I);
      end;

      if LimitOf[limDoubleBuffer] = GL_TRUE then
        DoubleLabel.Caption:='yes'
      else
        DoubleLabel.Caption:='no';

      if LimitOf[limStereo] = GL_TRUE then
        StereoLabel.Caption:='yes'
      else
        StereoLabel.Caption:='no';

      {$IFDEF MSWINDOWS}
      Label6.Show;
      CopyLabel.Show;
      // Include WGL extensions
      if WGL_ARB_extensions_string then
      begin
        ExtStr:=String(wglGetExtensionsStringARB(dc));
        while Length(ExtStr) > 0 do begin
          I:=Pos(' ',ExtStr);
          if I = 0 then I:=255;
          Extensions.Items.Add(Copy(ExtStr,1,I-1));
          Delete(ExtStr,1,I);
        end;
      end;

      // Some extra info about the double buffer mode
      if (pfd.dwFlags and PFD_DOUBLEBUFFER)=PFD_DOUBLEBUFFER then begin
        CopyLabel.Caption:='';
        if (pfd.dwFlags and PFD_SWAP_EXCHANGE) > 0 then CopyLabel.Caption:='exchange';
        if (pfd.dwFlags and PFD_SWAP_COPY) > 0 then
        begin
          if Length(CopyLabel.Caption) > 0 then CopyLabel.Caption:=CopyLabel.Caption+', ';
          CopyLabel.Caption:=CopyLabel.Caption+'copy';
        end;
        if Length(CopyLabel.Caption) = 0 then CopyLabel.Caption:='no info available';
      end else begin
        CopyLabel.Caption:='n/a';
      end;
      {$ENDIF}
      {$IFDEF Linux}
       // Include GLX extensions
       ExtStr:=String(glXQueryExtensionsString(glXGetCurrentDisplay(),0));
        while Length(ExtStr) > 0 do begin
          I:=Pos(' ',ExtStr);
          if I = 0 then I:=255;
          Extensions.Items.Add(Copy(ExtStr,1,I-1));
          Delete(ExtStr,1,I);
        end;
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
      Label18.Show;
      OverlayLabel.Show;
      Label20.Show;
      UnderlayLabel.Show;
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
   aSceneBuffer.RenderingContext.DeActivate;
   VersionLbl.Caption := GLSCENE_VERSION;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then Close;
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action:= caFree;
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
   Key:=' ';
end;

procedure TInfoForm.FormShow(Sender: TObject);
begin
   PageControl.ActivePageIndex := 0;
   {$IFDEF Linux}
   PageControl.TabPosition := tpRight;
   Top := 272;
   Width := 616;
   {$ENDIF}
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



