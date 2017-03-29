unit dImages;

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Controls;

type
  TDMImages = class(TDataModule)
    ilAdvancedGeometry: TImageList;
    ilBasicGeometry: TImageList;
    ilMenubar: TImageList;
    ilMeshObjects: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMImages: TDMImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
