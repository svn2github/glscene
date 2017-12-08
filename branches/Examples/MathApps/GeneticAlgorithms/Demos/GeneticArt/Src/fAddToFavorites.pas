unit fAddToFavorites;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uNEATClasses, sdXmlDocuments, uNEATToXML;

type
  TfrmAddToFavorites = class(TForm)
    cbo_Collection: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit_Name: TEdit;
    Button1: TButton;
    Button_Cancel: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
     
    FDoc : TsdXmlDocument;
    FGenotype : TGenotype;
  public
     
    procedure AddGenotype(const AGenotype : TGenotype);
    procedure FillCollectionList;
  end;

var
  frmAddToFavorites: TfrmAddToFavorites;

implementation

uses fGeneticArt;

var
  LastCollection : string = '';

{$R *.dfm}

{ TfrmAddToFavorites }

procedure TfrmAddToFavorites.AddGenotype(const AGenotype: TGenotype);
var
  FileName : string;
begin
  FileName := GetCurrentDir+'\Images.xml';
  if FileExists(FileName) then
    FDoc.LoadFromFile(FileName)
  else
    FDoc.CreateName('Genotypes');

  FillCollectionList;

  FGenotype := AGenotype;
  ShowModal;
end;


procedure TfrmAddToFavorites.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  GenotypeNode : TXMLNode;
  CollectionNode : TXMLNode;
begin
  if ModalResult <> mrOK then exit;

  Assert(Edit_Name.Text<>'', 'You must specify a name for your genotype!');

  if cbo_Collection.ItemIndex>-1 then
    CollectionNode := TXMLNode(cbo_Collection.Items.Objects[cbo_Collection.ItemIndex])
  else
  begin
    Assert(cbo_Collection.Text<>'', 'You must specify a name for your collection!');
    CollectionNode := FDoc.Root.NodeNew('Collection');
    CollectionNode.AttributeAdd('Name', cbo_Collection.Text);
  end;

  LastCollection := cbo_Collection.Text;

  GenotypeNode := CollectionNode.NodeNew('Genotype');
  GenotypeNode.WriteString('Name', Edit_Name.Text, 'Name');
  GenotypeNode.WriteBool('Coords',  frmGeneticArt.cbCoords.Checked, True);
  GenotypeNode.WriteBool('PerlinNoise', frmGeneticArt.cbPerlinNoise.Checked, True);
  SaveGenotypeToXMLNode(FGenotype, GenotypeNode);

  FDoc.XmlFormat := xfReadable;
  FDoc.SaveToFile(GetCurrentDir+'\Images.xml');
  FDoc.Clear;
end;

procedure TfrmAddToFavorites.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDoc);
end;

procedure TfrmAddToFavorites.FormCreate(Sender: TObject);
begin
  FDoc := TsdXmlDocument.Create;
end;

procedure TfrmAddToFavorites.FillCollectionList;
var
  i : integer;
begin
  cbo_Collection.Items.Clear;

  for i := 0 to FDoc.Root.NodeCount-1 do
    if SameText(FDoc.Root.Nodes[i].Name, 'Collection') then
    begin
      cbo_Collection.Items.AddObject(
        FDoc.Root.Nodes[i].AttributeByName('Name'),
        FDoc.Root.Nodes[i]);

      if FDoc.Root.Nodes[i].AttributeByName('Name') = LastCollection then
        cbo_Collection.ItemIndex := i;
    end;

  if (cbo_Collection.Items.Count>0) and (cbo_Collection.ItemIndex = -1) then
    cbo_Collection.ItemIndex := 0;
end;

procedure TfrmAddToFavorites.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then
    ModalResult := mrCancel;
end;
end.
