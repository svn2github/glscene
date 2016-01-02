//
// This unit is part of the GLScene Project   
//
{ : FRFaceEditor<p>

  Editor frame for a TVKFaceProperties.<p>

  <b>History : </b><font size=-1><ul>
  <li>06/01/15 - PW - Converted to FMX
  <li>05/09/08 - DanB - Removed Kylix support
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>19/12/06 - DaStr - TRFaceEditor.SetGLFaceProperties bugfixed - Shiness and
  PoligonMode are now updated when FaceProperties are assigned
  <li>03/07/04 - LR  - Make change for Linux
  <li>06/02/00 - Egg - Creation
  </ul></font>
}
unit FRFaceEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl,

  //VKS
  FRTrackBarEdit, FRColorEditor, VKS.Material;

type
  TRFaceEditor = class(TFrame)
    TabControl: TTabControl;
    TIAmbient: TTabItem;
    TIDiffuse: TTabItem;
    TIEmission: TTabItem;
    TISpecular: TTabItem;
    CEAmbiant: TRColorEditor;
    Label1: TLabel;
    TBEShininess: TRTrackBarEdit;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    procedure TBEShininessTrackBarChange(Sender: TObject);
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    Updating: Boolean;
    FFaceProperties: TVKFaceProperties;
    procedure SetGLFaceProperties(const val: TVKFaceProperties);
    procedure OnColorChange(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property FaceProperties: TVKFaceProperties read FFaceProperties
      write SetGLFaceProperties;
  end;

implementation

{$R *.fmx}

{ TRFaceEditor }

constructor TRFaceEditor.Create(AOwner: TComponent);
begin
  inherited;
  FFaceProperties := TVKFaceProperties.Create(nil);
  CEAmbiant.OnChange := OnColorChange;
  CEDiffuse.OnChange := OnColorChange;
  CEEmission.OnChange := OnColorChange;
  CESpecular.OnChange := OnColorChange;
  { TODO : E2003 Undeclared identifier: 'DoubleBuffered' }
  (*TabControl.DoubleBuffered := True;*)
end;

destructor TRFaceEditor.Destroy;
begin
  FFaceProperties.Free;
  inherited;
end;

procedure TRFaceEditor.OnColorChange(Sender: TObject);
var
  bmp: TBitmap;
  bmpRect: TRectF;

  procedure AddBitmapFor(ce: TRColorEditor);
  begin
    with bmp.Canvas do
    begin
      Fill.Color := ce.PAPreview.Color;
      FillRect(bmpRect,20,40,AllCorners,100);
    end;
    { TODO : E2003 Undeclared identifier: 'ImageList', to be replaced }
    (*ImageList.Add(bmp, nil);*)
  end;

begin
  if not updating then
  begin
    // Update imageList
    bmp := TBitmap.Create;
    try
      bmp.Width := 16;
      bmp.Height := 16;
      bmpRect := TRectF.Create(0, 0, 16, 16);
    { TODO : E2003 Undeclared identifier: 'ImageList', to be replaced }
      (*ImageList.Clear;*)
      bmp.Canvas.BeginScene;
      AddBitmapFor(CEAmbiant);
      bmp.Canvas.EndScene;
      FFaceProperties.Ambient.Color := CEAmbiant.Color;
      AddBitmapFor(CEDiffuse);
      FFaceProperties.Diffuse.Color := CEDiffuse.Color;
      AddBitmapFor(CEEmission);
      FFaceProperties.Emission.Color := CEEmission.Color;
      AddBitmapFor(CESpecular);
      FFaceProperties.Specular.Color := CESpecular.Color;
    finally
      bmp.Free;
    end;
    // Trigger onChange
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TRFaceEditor.TBEShininessTrackBarChange(Sender: TObject);
begin
  if not Updating then
  begin
    TBEShininess.TrackBarChange(Sender);
    FFaceProperties.Shininess := Round(TBEShininess.Value);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

// SetGLFaceProperties
//
procedure TRFaceEditor.SetGLFaceProperties(const val: TVKFaceProperties);
begin
  Updating := True;
  try
    CEAmbiant.Color := val.Ambient.Color;
    CEDiffuse.Color := val.Diffuse.Color;
    CEEmission.Color := val.Emission.Color;
    CESpecular.Color := val.Specular.Color;
    TBEShininess.Value := val.Shininess;
  finally
    Updating := False;
  end;
  OnColorChange(Self);
  TBEShininessTrackBarChange(Self);
end;

end.
