unit fFavorites;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, GR32_Image, ExtCtrls, sdXmlDocuments,
  uNEATClasses, Menus;

type
  TfrmFavorites = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lvCollections: TListView;
    lvImages: TListView;
    Shape1: TShape;
    Image: TImage32;
    Label3: TLabel;
    Button_EvolveImage: TButton;
    PopupMenu_Image: TPopupMenu;
    ZoomImage1: TMenuItem;
    KensGodMode1: TMenuItem;
    N1: TMenuItem;
    EvolveImage1: TMenuItem;
    RenameImage1: TMenuItem;
    N2: TMenuItem;
    PopupMenu_Collections: TPopupMenu;
    RenameCollection1: TMenuItem;
    DeleteCollection1: TMenuItem;
    DeleteImage1: TMenuItem;
    Addcollection1: TMenuItem;
    N3: TMenuItem;
    SaveImage1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure lvImagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Button_EvolveImageClick(Sender: TObject);
    procedure ZoomImage1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure KensGodMode1Click(Sender: TObject);
    procedure EvolveImage1Click(Sender: TObject);
    procedure lvCollectionsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvCollectionsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure lvCollectionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RenameImage1Click(Sender: TObject);
    procedure RenameCollection1Click(Sender: TObject);
    procedure DeleteCollection1Click(Sender: TObject);
    procedure DeleteImage1Click(Sender: TObject);
    procedure Addcollection1Click(Sender: TObject);
    procedure SaveImage1Click(Sender: TObject);
  private
    { Private declarations }
    FDoc : TsdXMLDocument;
    FGenotype : TGenotype;
    FUserAcceptedLossOfGeneration : boolean;

    procedure SaveDocument;
    function LoadFavoriteToGenotype(const AGenotype : TGenotype) : boolean;
    function GetCurrentCollection: TXMLNode;
    function GetCurrentImage: TXMLNode;
  public
    { Public declarations }
    procedure RefreshImageList;
    procedure RefreshCollectionList;
    property CurrentCollection : TXMLNode read GetCurrentCollection;
    property CurrentImage : TXMLNode read GetCurrentImage;
  end;

var
  frmFavorites: TfrmFavorites;

implementation

uses fGeneticArt, uNEATToXML, fZoomImage, fGodMode;

{$R *.dfm}

procedure TfrmFavorites.RefreshImageList;
var
  i : integer;
  CollectionNode : TXmlNode;
begin
  lvImages.Items.Clear;

  if not Assigned(lvCollections.Selected) then exit;

  CollectionNode := TXmlNode(lvCollections.Selected.Data);

  for i := 0 to CollectionNode.NodeCount-1 do
    if SameText(CollectionNode.Nodes[i].Name, 'Genotype') then
    begin
      with lvImages.Items.Add do
      begin
        Caption := CollectionNode[i].ReadString('Name', '');
        if CollectionNode[i].ReadBool('Coords', True) then
          SubItems.Add('x')
        else
          SubItems.Add('');

        if CollectionNode[i].ReadBool('PerlinNoise', True) then
          SubItems.Add('x')
        else
          SubItems.Add('');

        Data := CollectionNode[i];
      end;
    end;

  {if lvImages.Items.Count>0 then
    lvImages.Selected := lvImages.Items[0];//}
end;

procedure TfrmFavorites.FormCreate(Sender: TObject);
var
  FileName, GenotypeName  : string;
begin
  FDoc := TsdXmlDocument.Create;

  FileName := GetCurrentDir+'\Images.xml';
  if FileExists(FileName) then
    FDoc.LoadFromFile(FileName)
  else
    FDoc.CreateName('Genotypes');

  FGenotype := TGenotype.Create(frmGeneticArt.NEATPopulation);

  RefreshCollectionList;
end;

procedure TfrmFavorites.RefreshCollectionList;
var
  i : integer;
begin
  lvCollections.Items.Clear;

  for i := 0 to FDoc.Root.NodeCount-1 do
    if SameText(FDoc.Root.Nodes[i].Name, 'Collection') then
    begin
      with lvCollections.Items.Add do
      begin
        Caption := FDoc.Root.Nodes[i].AttributeByName('Name');
        Data := FDoc.Root.Nodes[i];
      end;
    end;

  if lvCollections.Items.Count>0 then
    lvCollections.Selected := lvCollections.Items[0];
end;

procedure TfrmFavorites.lvImagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if (Item<>nil) and Selected then
  begin
    if LoadFavoriteToGenotype(FGenotype) then
    begin
      frmGeneticArt.RenderGenotype(FGenotype, Image, 4);
      Image.Repaint;
      frmGeneticArt.RenderGenotype(FGenotype, Image, 1);
    end;
  end;
end;

function TfrmFavorites.LoadFavoriteToGenotype(const AGenotype: TGenotype) : boolean;
var
  GenotypeNode : TXMLNode;
begin
  result := false;

  GenotypeNode := TXMLNode(lvImages.Selected.Data);

  // Prepare the neat settings for this
  if ((frmGeneticArt.cbCoords.Checked <> GenotypeNode.ReadBool('Coords', True)) or
   (frmGeneticArt.cbPerlinNoise.Checked <> GenotypeNode.ReadBool('PerlinNoise', True))) then
  begin
    if FUserAcceptedLossOfGeneration or (MessageDlg('This means you will loose the generation of images you''re currently viewing. '+#13+#10+''+#13+#10+'Are you sure you want to do this?', mtWarning, [mbOK, mbCancel], 0) = mrOk) then
    begin
      FUserAcceptedLossOfGeneration := true;
      frmGeneticArt.cbCoords.Checked := GenotypeNode.ReadBool('Coords', True);
      frmGeneticArt.cbPerlinNoise.Checked := GenotypeNode.ReadBool('PerlinNoise', True);
    end else
      exit;
  end;

  LoadGenotypeFromXMLNode(AGenotype, GenotypeNode);
  result := true;
end;

procedure TfrmFavorites.Button_EvolveImageClick(Sender: TObject);
begin
  if LoadFavoriteToGenotype(frmGeneticArt.NEATPopulation.Genotypes[0]) then
  begin
    FUserAcceptedLossOfGeneration := false;
    frmGeneticArt.RenderGeneration;
    Close;
  end;
end;

procedure TfrmFavorites.ZoomImage1Click(Sender: TObject);
begin
  with TfrmZoomImage.Create(Application) do
    ShowAndRender(FGenotype);
end;

procedure TfrmFavorites.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FUserAcceptedLossOfGeneration then
    frmGeneticArt.RenderGeneration;
end;

procedure TfrmFavorites.KensGodMode1Click(Sender: TObject);
begin
  with TfrmGodMode.Create(Application) do
    ShowAndModify(FGenotype);
end;

procedure TfrmFavorites.EvolveImage1Click(Sender: TObject);
begin
  Button_EvolveImageClick(Sender);
end;

procedure TfrmFavorites.lvCollectionsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(lvImages.Selected) and Assigned(lvCollections.GetItemAt(x,y));
end;

procedure TfrmFavorites.lvCollectionsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  ImageNode, CollectionNode : TXMLNode;
begin
  ImageNode := TXMLNode(lvImages.Selected.Data);
  CollectionNode := TXMLNode(lvCollections.GetItemAt(x,y).Data);
  CollectionNode.NodeAdd(ImageNode);
  SaveDocument;
  RefreshImageList;
end;

procedure TfrmFavorites.lvCollectionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  RefreshImageList;
end;

procedure TfrmFavorites.SaveDocument;
begin
  FDoc.XmlFormat := xfReadable;
  FDoc.SaveToFile(GetCurrentDir+'\Images.xml');
end;

procedure TfrmFavorites.RenameImage1Click(Sender: TObject);
var
  NewName : string;
begin
  if Assigned(CurrentImage) then
  begin
    NewName := CurrentImage.ReadString('Name', '');

    if InputQuery('New name', 'New name', NewName) then
    begin
      CurrentImage.WriteString('Name', NewName, 'Name');
      SaveDocument;
      lvImages.Selected.Caption := NewName;
    end;
  end;
end;

function TfrmFavorites.GetCurrentCollection: TXMLNode;
begin
  if Assigned(lvCollections.Selected) then
    result := TXMLNode(lvCollections.Selected.Data)
  else
    result := nil;
end;

function TfrmFavorites.GetCurrentImage: TXMLNode;
begin
  if Assigned(lvImages.Selected) then
    result := TXMLNode(lvImages.Selected.Data)
  else
    result := nil;
end;

procedure TfrmFavorites.RenameCollection1Click(Sender: TObject);
var
  NewName : string;
begin
  if Assigned(CurrentCollection) then
  begin
    NewName := CurrentCollection.AttributeByName('Name');

    if InputQuery('New name', 'New name', NewName) then
    begin
      (*CurrentCollection.AttributeByName('Name') := NewName;*)
      CurrentCollection.AttributeByName(NewName);
      SaveDocument;
      lvCollections.Selected.Caption := NewName;
    end;
  end;
end;

procedure TfrmFavorites.DeleteCollection1Click(Sender: TObject);
begin
  if Assigned(CurrentCollection) and (MessageDlg('Are you sure?', mtConfirmation, [mbYes, mbNo], 0)=mrYES) then
  begin
    CurrentCollection.Parent.NodeRemove(CurrentCollection);
    SaveDocument;
    lvCollections.Selected.Free;
    RefreshImageList;
  end;
end;

procedure TfrmFavorites.DeleteImage1Click(Sender: TObject);
begin
  if Assigned(CurrentImage) and (MessageDlg('Are you sure?', mtConfirmation, [mbYes, mbNo], 0)=mrYES) then
  begin
    CurrentImage.Parent.NodeRemove(CurrentImage);
    SaveDocument;
    lvImages.Selected.Free;
    RefreshImageList;
  end;
end;

procedure TfrmFavorites.Addcollection1Click(Sender: TObject);
var
  CollectionName : string;
begin
  CollectionName := '';
  if InputQuery('Collection name', 'Collection name', CollectionName) then
  begin
    Assert(CollectionName<>'', 'You must specify a real name!');

    with FDoc.Root.NodeNew('Collection') do
      AttributeAdd('Name', CollectionName);

    SaveDocument;
    RefreshCollectionList;
  end;
end;

procedure TfrmFavorites.SaveImage1Click(Sender: TObject);
begin
  frmGeneticArt.SaveImage(Image.Bitmap);
end;

end.
