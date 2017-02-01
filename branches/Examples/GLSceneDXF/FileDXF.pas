{: FileDXF<p>

  DXF file loader<p>

  <b>History :</b><font size=-1><ul>
    <li>26/01/03 - DA - SaveToStream is now correct
    <li>21/01/03 - DA - Supported DXF entities are now stored in an array
    <li>07/01/03 - DA - Update the progress bar with a logarithmic scale when reading entities
    <li>06/12/02 - SJ - Added block reading
    <li>30/11/02 - DA - Added header and tables support,
                        Moved TDXFEntitiesList to the TypesDXF unit
    <li>27/11/02 - DA - Support for Vertices and Text
    <li>23/11/02 - DA - Moved base DXF IO functions to the TDXFIOObject
    <li>16/11/02 - DA - Entities are now created
    <li>20/10/02 - SJ - Uses TDXFEntity of TypesDXF unit
    <li>13/10/02 - DA - Entity reading base and progress information
    <li>05/10/02 - DA - Unit creation
  </ul></font>
}
unit FileDXF;

interface

uses
  Classes,
  Contnrs, // TObjectList

  TypesDXF,
  CodesValuesDXF,
  ObjectsDXF
  ;

type

  // TDXFProgressFunc
  //
  {: Function to call when loading or saving from/to DXF }
  TDXFProgressFunc = procedure(const Msg: String; ActualPosition,
    PositionMax: Longint) of object;

  // TFileDXF
  //
  {: TFileDXF is the  main class and supplies the user with all available data
     from a specific DXF file. }
  TFileDXF = class(TDXFIOObject)
  private
    FHeader: TDXFHeader;
    FEntities: TDXFEntitiesList;
    FLayers: TDXFLayersList;
    FBlocks: TDXFBlocksList;
    FOnProgress: TDXFProgressFunc;

    //: go to the specified section
    function GoToSection(const ASectionName: String): boolean;
    //: call the load/save progress function
    procedure DoProgress(const AMsg: string; APos, AMax: LongInt);

  protected
    //: Read the header section
    procedure ReadHeader;
    //: Read the tables section
    procedure ReadTables;
    //: Read the blocks section
    procedure ReadBlocks;    
    //: Read the entities section
    procedure ReadEntities;

    //: Write the header section
    procedure WriteHeader;
    //: Write the tables section
    procedure WriteTables;
    //: Write the blocks section
    procedure WriteBlocks;
    //: Write the entities section
    procedure WriteEntities;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    //: load from a stream
    procedure LoadFromStream(AStream: TStream); override;
    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

    //: DXF header
    property Header: TDXFHeader read FHeader;
    //: list of entities in the DXF file
    property Entities: TDXFEntitiesList read FEntities;
    //: list of layers in the DXF file
    property Layers: TDXFLayersList read FLayers;
    //: list of blocks in the DXF file
    property Blocks: TDXFBlocksList read FBlocks;
    //: function called on load/save progress
    property OnProgress: TDXFProgressFunc read FOnProgress write FOnProgress;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
  SysUtils,
  WinProcs, // OutputDebugString()
  Math
  ;

{ TFileDXF }

// Create
//
constructor TFileDXF.Create;
begin
  inherited;

  FEntities := TDXFEntitiesList.Create;
  FLayers := TDXFLayersList.Create;
  FBlocks := TDXFBlocksList.Create;
  FHeader := TDXFHeader.Create;
end;

// Destroy
//
destructor TFileDXF.Destroy;
begin
  FEntities.Free;
  FLayers.Free;
  FBlocks.Free;
  FHeader.Free;

  inherited;
end;

// DoProgress
//
{: @param AMsg          Message to show
   @param APos          Actual load/save position
   @param AMax          Maximum position }
procedure TFileDXF.DoProgress(const AMsg: string; APos, AMax: Integer);
begin
  if Assigned(FOnProgress) then FOnProgress(AMsg, APos, AMax);
end;

// GoToSection
//
{: @param ASectionName  Name of the section where to go
   @return True if success, False otherwise }
function TFileDXF.GoToSection(const ASectionName: String): boolean;
var
  Code: integer;
  Value: string;
begin
  // return to the beginning
  DXFStream.Position := 0;

  try
                                           
    repeat

      // search a section
      repeat
        ReadCodes(Code, Value);
      until (Code = DXF_START) and (Value = DXFV_SECTION);

      // check if this section is the one we are searching
      ReadCodes(Code, Value);
      Result := (Code = DXF_NAME) and (Value = ASectionName);

    until Result;

  except
    Result := False;
  end;
end;

// LoadFromStream
//
procedure TFileDXF.LoadFromStream(AStream: TStream);
begin
  try
    inherited;
  except
  end;

  // read the header section
  DoProgress('Lecture de l''entête...', 1, 100);
  if GoToSection(DXFV_HEADER) then begin
    ReadHeader;
  end;

  // read the tables section
  DoProgress('Lecture des tables...', 10, 100);
  if GoToSection(DXFV_TABLES) then begin
    ReadTables;
  end;

  // read the blocks section
  DoProgress('Lecture des blocs...', 20, 100);
  if GoToSection(DXFV_BLOCKS) then begin
    ReadBlocks;
  end;

  // read the entities section
  DoProgress('Lecture des entitées...', 30, 100);
  if GoToSection(DXFV_ENTITIES) then begin
    ReadEntities;
  end;

  DoProgress('Terminé', 100, 100);
  Sleep(25);
end;

// ReadBlocks
//
procedure TFileDXF.ReadBlocks;
var
  Code: integer;
  Value: string;
  EndSec: boolean;
  Block: TDXFBlock;
begin
  repeat
    Block := nil;
    
    ReadCodes(Code, Value);

    // end of section ?
    EndSec := (Code = DXF_START) and (Value = DXFV_ENDSEC);

    // create entities
    if (not EndSec) and (Code = DXF_START) then begin
      if Value = DXFV_BLOCK then Block := TDXFBlock.Create(FLayers, FBlocks) else
      ;
    end;

    // if a new entity was read then ...
    if Assigned(Block) then begin

      // the entity read itself
      Block.LoadFromStream(DXFStream);

      // add it to the layers list
      FBlocks.AddBlock(Block);
    end;
  until EndSec;
end;

// ReadEntities
//
procedure TFileDXF.ReadEntities;
var
  Code: integer;
  Value: string;
  EndSec : boolean;
  Entity : TDXFEntity;
  SuppEntIdx: Integer; // supported entity index
  EntityClass: TDXFEntityClass; // class of the entity
begin
  repeat
    Entity := nil;
    
    ReadCodes(Code, Value);

    // end of section ?
    EndSec := (Code = DXF_START) and (Value = DXFV_ENDSEC);

    // create entities
    if (not EndSec) and (Code = DXF_START) then begin

      with SupportedDXFEntities do begin

        // search a DXF entity with this name in the supported DXF entities list
        if Find(Value, SuppEntIdx) then begin

          // get the entity class
          EntityClass := TDXFEntityClass(Objects[SuppEntIdx]);

          // check the entity classes
          if EntityClass.InheritsFrom(TDXFInsert) then begin
            // inserts need the block list too
            Entity := TDXFInsertClass(EntityClass).Create(FLayers, FBlocks);
          end else begin
            Entity := EntityClass.Create(FLayers); // create the entity
          end;

        end
        else OutputDebugString(PWideChar('Unsuported entity type ' + Value));
      end;
      
    end;

    // if a new entity was read then ...
    if Assigned(Entity) then begin

      // the entity read itself
      Entity.LoadFromStream(DXFStream);

      // add it to the entities list
      FEntities.AddEntity(Entity);

      // update the progress bar with a logarithmic scale
      DoProgress('Ajout de l''entitée ' + IntToStr(FEntities.Count) + '...', 30 + Trunc(Ln(FEntities.Count)*7), 100);
    end;
  until EndSec;
end;

// ReadHeader
//
procedure TFileDXF.ReadHeader;
begin
  FHeader.LoadFromStream(DXFStream);
end;

// ReadTables
//
procedure TFileDXF.ReadTables;
var
  Code: integer;
  Value: string;
  EndSec: boolean;
  Layer: TDXFLayer;
begin
  repeat
    Layer := nil;
    
    ReadCodes(Code, Value);

    // end of section ?
    EndSec := (Code = DXF_START) and (Value = DXFV_ENDSEC);

    // create entities
    if (not EndSec) and (Code = DXF_START) then begin
      if Value = DXFV_LAYER then Layer := TDXFLayer.Create else
      ;
    end;

    // if a new entity was read then ...
    if Assigned(Layer) then begin

      // the entity read itself
      Layer.LoadFromStream(DXFStream);

      // add it to the layers list
      FLayers.AddLayer(Layer);
    end;
  until EndSec;
end;

// SaveToStream
//
procedure TFileDXF.SaveToStream(AStream: TStream);
begin
  inherited;

  // write the header section
  DoProgress('Ecriture de l''entête...', 1, 100);
  WriteHeader;

  // write the tables section
  DoProgress('Ecriture des tables...', 10, 100);
  WriteTables;

  // write the blocks section
  DoProgress('Ecriture des blocs...', 20, 100);
  WriteBlocks;

  // write the entities section
  DoProgress('Ecriture des entitées...', 30, 100);
  WriteEntities;

  DoProgress('Terminé', 100, 100);
  Sleep(25);
end;


// WriteBlocks
//
procedure TFileDXF.WriteBlocks;
var
  i: Integer;
begin
  WriteCodes(DXF_START, DXFV_SECTION); // open section
  WriteCodes(DXF_NAME, DXFV_BLOCKS); // write section name

  // write blocks
  for i := 0 to FBlocks.Count - 1 do
    FBlocks.Block[i].SaveToStream(DXFStream);

  WriteCodes(DXF_START, DXFV_ENDSEC); // close section
end;

// WriteEntities
//
procedure TFileDXF.WriteEntities;
var
  i: Integer;
begin
  WriteCodes(DXF_START, DXFV_SECTION); // open section
  WriteCodes(DXF_NAME, DXFV_ENTITIES); // write section name

  // write entities
  for i := 0 to FEntities.Count - 1 do
    FEntities.Entity[i].SaveToStream(DXFStream);

  WriteCodes(DXF_START, DXFV_ENDSEC); // close section    
end;

// WriteHeader
//
procedure TFileDXF.WriteHeader;
begin
  FHeader.SaveToStream(DXFStream);
end;

// WriteTables
//
procedure TFileDXF.WriteTables;
var
  i: Integer;
begin
  WriteCodes(DXF_START, DXFV_SECTION); // open section
  WriteCodes(DXF_NAME, DXFV_TABLES); // write section name

  WriteCodes(DXF_START, DXFV_TABLE); // open the layer tables

  // write layers
  for i := 0 to FLayers.Count - 1 do
    FLayers.Layer[i].SaveToStream(DXFStream);

  WriteCodes(DXF_START, DXFV_ENDTAB); // close the layer tables

  WriteCodes(DXF_START, DXFV_ENDSEC); // close section
end;

end.
