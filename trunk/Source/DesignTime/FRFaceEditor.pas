{: FRFaceEditor<p>

   Editor fram for a TGLFaceProperties.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>03/07/04 - LR - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRFaceEditor;

interface

{$i GLScene.inc}

{$IFDEF MSWINDOWS}
uses
  Windows, Forms, ComCtrls, FRTrackBarEdit, StdCtrls, FRColorEditor, ImgList, Controls,
  Classes, GLTexture;
{$ENDIF}
{$IFDEF LINUX}
uses 
  QForms, QComCtrls, FRTrackBarEdit, QStdCtrls, FRColorEditor, QImgList, QControls, 
  Classes, GLTexture; 
{$ENDIF}


type
  TRFaceEditor = class(TFrame)
    PageControl: TPageControl;
    TSAmbient: TTabSheet;
    TSDiffuse: TTabSheet;
    TSEmission: TTabSheet;
    TSSpecular: TTabSheet;
    CEAmbiant: TRColorEditor;
    Label1: TLabel;
    TBEShininess: TRTrackBarEdit;
    ImageList: TImageList;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    Label2: TLabel;
    CBPolygonMode: TComboBox;
    procedure TBEShininessTrackBarChange(Sender: TObject);
    procedure CBPolygonModeChange(Sender: TObject);

  private
    { Déclarations privées }
    FOnChange : TNotifyEvent;
    updating : Boolean;
    FFaceProperties : TGLFaceProperties;
    procedure SetGLFaceProperties(const val : TGLFaceProperties);
    procedure OnColorChange(Sender : TObject);

  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
	 property FaceProperties : TGLFaceProperties read FFaceProperties write SetGLFaceProperties;

  end;

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}


uses
{$IFDEF MSWINDOWS}
  Graphics; 
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, Types;
{$ENDIF}


constructor TRFaceEditor.Create(AOwner : TComponent);
begin
   inherited;
   FFaceProperties:=TGLFaceProperties.Create(nil);
   CEAmbiant.OnChange:=OnColorChange;
   CEDiffuse.OnChange:=OnColorChange;
   CEEmission.OnChange:=OnColorChange;
   CESpecular.OnChange:=OnColorChange;
   {$IFDEF MSWINDOWS}
   PageControl.DoubleBuffered:=True;
   {$ENDIF}
end;

destructor TRFaceEditor.Destroy;
begin
   FFaceProperties.Free;
   inherited;
end;

procedure TRFaceEditor.OnColorChange(Sender : TObject);
var
   bmp : TBitmap;
   bmpRect : TRect;

   procedure AddBitmapFor(ce : TRColorEditor);
   begin
      with bmp.Canvas do begin
         Brush.Color:=ce.PAPreview.Color;
         FillRect(bmpRect);
      end;
      ImageList.Add(bmp, nil);
   end;

begin
   if not updating then begin
      // Update imageList
      bmp:=TBitmap.Create;
      try
         bmp.Width:=16;
         bmp.Height:=16;
         bmpRect:=Rect(0, 0, 16, 16);
         ImageList.Clear;
         AddBitmapFor(CEAmbiant);
         FFaceProperties.Ambient.Color:=CEAmbiant.Color;
         AddBitmapFor(CEDiffuse);
         FFaceProperties.Diffuse.Color:=CEDiffuse.Color;
         AddBitmapFor(CEEmission);
         FFaceProperties.Emission.Color:=CEEmission.Color;
         AddBitmapFor(CESpecular);
         FFaceProperties.Specular.Color:=CESpecular.Color;
      finally
         bmp.Free;
      end;
      // Trigger onChange
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TRFaceEditor.TBEShininessTrackBarChange(Sender: TObject);
begin
   if not updating then begin
      TBEShininess.TrackBarChange(Sender);
      FFaceProperties.Shininess:=TBEShininess.Value;
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TRFaceEditor.CBPolygonModeChange(Sender: TObject);
begin
   if not updating then begin
      FFaceProperties.PolygonMode:=TPolygonMode(CBPolygonMode.ItemIndex);
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

// SetGLFaceProperties
//
procedure TRFaceEditor.SetGLFaceProperties(const val : TGLFaceProperties);
begin
   updating:=True;
   try
      CEAmbiant.Color:=val.Ambient.Color;
      CEDiffuse.Color:=val.Diffuse.Color;
      CEEmission.Color:=val.Emission.Color;
      CESpecular.Color:=val.Specular.Color;
      TBEShininess.Value:=val.Shininess;
      CBPolygonMode.ItemIndex:=Integer(val.PolygonMode);
      FFaceProperties.PolygonMode:=val.PolygonMode;
   finally
      updating:=False;
   end;
   OnColorChange(Self);
end;

end.



