{: ObjectsDXF<p>

  DXF Objects<p>

  <b>History :</b><font size=-1><ul>
    <li>26/01/03 - DA - Entities are now saving to a stream correctly
    <li>25/01/03 - DA - Inserts into blocks are now supported,
                        Implemented GetEntityName methods for entities,
                        Implemented SaveToStream methods for entities
    <li>21/01/03 - DA - Supported DXF entities are now stored in an array
    <li>07/01/03 - DA - Blocks can now begin with the INSERT keyword instead of BLOCK,
                        Added Bulge to the Vertex entity
    <li>07/12/02 - SJ - Added insert entity
    <li>06/12/02 - SJ - Added blocklist
            		 - DA - Added 3DFace entity
    <li>02/12/02 - DA - Unit creation from TypesDXF
  </ul></font>
}
unit ObjectsDXF;

{ TODO : Gerer trace }
{ TODO : blocks : rotation }
{ TODO : couleur du texte }
{ TODO : quotes }

interface

uses
  // Delphi
  Classes,
  windows,

  // DXF
  CodesValuesDXF,
  TypesDXF
  ;

type

  // TDXFPoint
  //
  {: The DXF Point type. }
  TDXFPoint = class(TDXFEntity)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;

  public
    Primary : T3DPoint;   // 10 20 30
    Thickness : Single;           // 39
    Extrusion : T3DPoint;    // 210 220 230

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;
  end;

  // TDXFVertexFlag
  //
  {: Vertex flags:<br>
    1 = Extra vertex created by curve-fitting<br>
    2 = Curve-fit tangent defined for this vertex.<br>
        A curve-fit tangent direction of 0 may be omitted from DXF output but is
        significant if this bit is set.
    4 = Not used<br>
    8 = GLSpline vertex created by GLSpline-fitting<br>
    16 = GLSpline frame control point<br>
    32 = 3D polyline vertex<br>
    64 = 3D polygon mesh<br>
    128 = Polyface mesh vertex }
  TDXFVertexFlag = (vtExtra = 1, vtTangent = 2, { unused = 4, }
    vtSpline = 8, vt3DPolyline = 16, vt3DPolygon = 32, vt3DPolygonMesh = 64,
    vt3DPolyfaceMesh = 128);

  // TDXFVertex
  //
  {: The DXF vertex type. }
  TDXFVertex = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;

  public
    Flag: Integer; // 70
    Bulge: Double; // 42

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;
  end;

  // TDXFCircle
  //
  {: The DXF Circle type. }
  TDXFCircle = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;    

  public
    Radius : Single;              // 40

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;
  end;

  // TDXFArc
  //
  {: The DXF Arc type. }
  TDXFArc = class(TDXFCircle)
  private
    FStartAngle: Single;          // 50
    FEndAngle: Single;            // 51

  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;    

  public
    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

    property StartAngle: Single read FStartAngle write FStartAngle;
    property EndAngle: Single read FEndAngle write FEndAngle;
  end;

  // TDXFLine
  //
  {: The DXF Line type. }
  TDXFLine = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;       

  public
    EndPoint : T3DPoint;     // 11 21 31

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;
  end;

  // TDXFPolylineFlag
  //
  {: Polyline flag (bit-coded); default is 0:<br>
    1 = This is a closed polyline (or a polygon mesh closed in the M direction).<br>
    2 = Curve-fit vertices have been added.<br>
    4 = GLSpline-fit vertices have been added.<br>
    8 = This is a 3D polyline.<br>
    16 = This is a 3D polygon mesh.<br>
    32 = The polygon mesh is closed in the N direction.<br>
    64 = The polyline is a polyface mesh.<br>
    128 = The linetype pattern is generated continuously around the vertices of this polyline.
  }
  TDXFPolylineFlag = (plClosed = 1, plCurveFit = 2, plSplineFit = 4,
    pl3DPolyline = 8, pl3DPolygon = 16, plPolyMeshClosed = 32, plPolyface = 64,
    plContinuousLinetype = 128);

  // TDXFPolyline
  //
  {: The DXF Polyline type. }
  TDXFPolyline = class(TDXFPoint)
  private
    FSeqEnd: boolean; // end of section

  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;
    //: says if this code pair means the end of this entity
    function EndOfPart(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;        

  public
    Flag: Integer;               // 70
    PointList : array of TDXFVertex;

    destructor Destroy; override;

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;    
  end;

  // TDXFText
  //
  {: The DXF Text type. }
  TDXFText = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;

  public
    Value : String;               // 1
    Style : String;               // 7
    Height : Single;              // 40
    Rotation : Single;            // 50
    GenerationFlag : Integer;     // 71
    JustificationH : Integer;         // 72
    JustificationV : Integer;         // 73

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;       
  end;

  // TDXF3DFace
  //
  {: The DXF 3D face. }
  TDXF3DFace = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;    

  public
    Second, Third, Fourth: T3DPoint;

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;        
  end;

  // TDXFSolid
  //
  {: The DXF solid }
  TDXFSolid = class(TDXF3DFace)
  protected
    //: returns the entity's DXF name
    function GetEntityName: string; override;
  end;

  // forward declaration
  TDXFBlocksList = class;
  
  // TDXFBlock
  //
  {: The DXF Block type. }
  TDXFBlock = class(TDXFEntity)
  private
    FEntities: TDXFEntitiesList;
    FBlocks : TDXFBlocksList;

  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;
    //: says if this code pair means the end of this part
    function EndOfPart(Code: Integer; Value: String): boolean; override;
    
  public
    Primary : T3DPoint;   // 10 20 30
    BlockName : String;           // 2 3
    Flag : Integer;               // 70

    constructor Create(Layers : TDXFLayersList; ABlocksList : TDXFBlocksList); reintroduce;
    
    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

    //: list of entities in the block
    property Entities: TDXFEntitiesList read FEntities;

  end;

  // TDXFBlocksList
  //
  {: Contains all blocks entities of a DXF file }
  TDXFBlocksList = class
  private
    FBlocks: TDXFEntitiesList;

    function GetCount: Integer;
    function GetBlock(Index: Integer): TDXFBlock;
    function GetBlockByName(Name: string): TDXFBlock;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    //: add a block to the list
    procedure AddBlock(const ABlock: TDXFBlock);

    //: the number of block
    property Count : Integer read GetCount;
    //: use this to access a block
    property Block[Index: Integer]: TDXFBlock read GetBlock;
    //: use this to access a block by its block name
    property BlockByName[Name: String]: TDXFBlock read GetBlockByName;

  end;

  // TDXFInsert
  //
  {: The DXF insert type}
  TDXFInsert = class(TDXFPoint)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; override;       

  public
    Blocks : TDXFBlocksList;
      
    BlockName : String;           // 2
    Scale : T3DPoint;             // 41 42 43
    Rotation : Single;            // 50

    constructor Create(ALayersList : TDXFLayersList; ABlocksList : TDXFBlocksList);

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;          
  end;

  // TDXFInsertClass
  //
  TDXFInsertClass = class of TDXFInsert;

var
  //: list of supported entities and classes
  SupportedDXFEntities : TStringList;  

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
  SysUtils
  ;

{ TDXFBlock }

// Create
//
constructor TDXFBlock.Create(Layers: TDXFLayersList; ABlocksList : TDXFBlocksList);
begin
  inherited Create(Layers);

  FBlocks := ABlocksList;
  FEntities := TDXFEntitiesList.Create;
end;

// EndOfPart
//
function TDXFBlock.EndOfPart(Code: Integer; Value: String): boolean;
begin
  Result := (Code = DXF_START) and ((Value = DXFV_ENDBLK) {or (Value = DXFV_INSERT)});
end;

// InterpretCodes
//
function TDXFBlock.InterpretCodes(Code: Integer; Value: String): boolean;

  // Read a block entity
  //
  procedure ReadEntity;
  var
    Entity : TDXFEntity;
    SuppEntIdx: Integer; // supported entity index
    EntityClass: TDXFEntityClass; // class of the entity
  begin
    Entity := nil;

    // create entities
    with SupportedDXFEntities do begin

      // search a DXF entity with this name in the supported DXF entities list
      if Find(Value, SuppEntIdx) then begin

        // get the entity class
        EntityClass := TDXFEntityClass(Objects[SuppEntIdx]);

        // check the entity classes
        if EntityClass.InheritsFrom(TDXFInsert) then begin
          // inserts need the block list too
          Entity := TDXFInsertClass(EntityClass).Create(Layers, FBlocks);
        end else begin
          Entity := EntityClass.Create(Layers); // create the entity
        end;

      end
      else
        OutputDebugString(PWideChar('Unsuported entity type ' + Value));
    end;

    // if a new entity was read then ...
    if Assigned(Entity) then begin
      Entity.LoadFromStream(DXFStream); // the entity read itself
      FEntities.AddEntity(Entity); // add it to the entities list
    end;
  end;

begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_PRIMARY_X     : Primary.X := StrToFloat(Value); // X coordinates
      DXF_PRIMARY_Y     : Primary.Y := StrToFloat(Value); // Y coordinates
      DXF_PRIMARY_Z     : Primary.Z := StrToFloat(Value); // Z coordinates
      DXF_START         : if Value <> DXFV_ENDBLK then ReadEntity;
      DXF_NAME          : BlockName := Value; // The block name
      DXF_70FLAG        : Flag := StrToInt(Value); // Block flag
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFBlock.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  DXFStream := AStream; // save the stream in an instance variable

  WriteCodes(DXF_START, DXFV_BLOCK); // start the block

  WriteCodes(DXF_NAME, BlockName); // The block name
  Write3DPoint(Primary); // coordinates
  if Flag <> 0 then WriteCodes(DXF_70FLAG, IntToStr(Flag)); // Block flag

  // write entities
  for i := 0 to FEntities.Count - 1 do
    FEntities.Entity[i].SaveToStream(DXFStream);

  WriteCodes(DXF_START, DXFV_ENDBLK); // end of block
end;

{ TDXFBlocksList }

// AddBlock
//
procedure TDXFBlocksList.AddBlock(const ABlock: TDXFBlock);
begin
  FBlocks.AddEntity(ABlock);
end;

// Create
//
constructor TDXFBlocksList.Create;
begin
  FBlocks := TDXFEntitiesList.Create;
end;

// Destroy
//
destructor TDXFBlocksList.Destroy;
begin
  FBlocks.Free;

  inherited;
end;

// GetBlock
//
function TDXFBlocksList.GetBlock(Index: Integer): TDXFBlock;
begin
  Result := TDXFBlock(FBlocks.Entity[Index]);
end;

// GetBlockByName
//
function TDXFBlocksList.GetBlockByName(Name: string): TDXFBlock;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do begin
//    OutputDebugString(PAnsiChar(GetBlock(i).BlockName + ' ' + Name));
    if GetBlock(i).BlockName = Name then begin
      Result := GetBlock(i);
      exit;
    end;
  end;
end;

// GetCount
//
function TDXFBlocksList.GetCount: Integer;
begin
  Result := FBlocks.Count;
end;

//--------------------------
//--------------------------{ TDXFPoint }------------------------------------
//--------------------------

// GetEntityName
//
function TDXFPoint.GetEntityName: string;
begin
  Result := DXFV_POINT;
end;

// InterpretCodes
//
function TDXFPoint.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_PRIMARY_X       : Primary.X := StrToFloat(Value); // X coordinates
      DXF_PRIMARY_Y       : Primary.Y := StrToFloat(Value); // Y coordinates
      DXF_PRIMARY_Z       : Primary.Z := StrToFloat(Value); // Z coordinates
      DXF_THICKNESS       : Thickness := StrToFloat(Value); // thickness
      DXF_EXTRUSIONX      : Extrusion.X := StrToFloat(Value); // X extrusion
      DXF_EXTRUSIONY      : Extrusion.Y := StrToFloat(Value); // Y extrusion
      DXF_EXTRUSIONZ      : Extrusion.Z := StrToFloat(Value); // Z extrusion
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFPoint.SaveToStream(AStream: TStream);
begin
  inherited;

  Write3DPoint(Primary); // coordinates
  if Thickness <> 0 then WriteCodes(DXF_THICKNESS, FloatToStr(Thickness)); // thickness
  Write3DPoint(Extrusion, DXF_EXTRUSIONX); // extrusion
end;

{ TDXFLine }

// GetEntityName
//
function TDXFLine.GetEntityName: string;
begin
  Result := DXFV_LINE;
end;

// InterpretCodes
//
function TDXFLine.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_OTHER_X_1       : EndPoint.X := StrToFloat(Value); // endpoint X coordinates
      DXF_OTHER_Y_1       : EndPoint.Y := StrToFloat(Value); // endpoint Y coordinates
      DXF_OTHER_Z_1       : EndPoint.Z := StrToFloat(Value); // endpoint Z coordinates
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFLine.SaveToStream(AStream: TStream);
begin
  inherited;

  Write3DPoint(EndPoint, DXF_OTHER_X_1); // endpoint coordinates
end;


{ TDXFCircle }

// GetEntityName
//
function TDXFCircle.GetEntityName: string;
begin
  Result := DXFV_CIRCLE;
end;

// InterpretCodes
//
function TDXFCircle.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_FLOATVAL        : Radius := StrToFloat(Value); // radius
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFCircle.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_FLOATVAL, FloatToStr(Radius)); // circle radius
end;

{ TDXFArc }

// GetEntityName
//
function TDXFArc.GetEntityName: string;
begin
  Result := DXFV_ARC;
end;

// InterpretCodes
//
function TDXFArc.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_ANGLE1        : StartAngle := StrToFloat(Value); // start angle
      DXF_ANGLE2        : EndAngle := StrToFloat(Value); // end angle
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFArc.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_ANGLE1, FloatToStr(StartAngle)); // start angle
  WriteCodes(DXF_ANGLE2, FloatToStr(EndAngle)); // end angle
end;

{ TDXFPolyline }

// Destroy
//
destructor TDXFPolyline.Destroy;
var
  i: integer;
begin
  // free points
  for i:=0 to Length(PointList) - 1 do
    PointList[i].Free;

  inherited;
end;

// EndOfPart
//
function TDXFPolyline.EndOfPart(Code: Integer; Value: String): boolean;
begin
  Result := (Code = DXF_START) and FSeqEnd;
end;

// GetEntityName
//
function TDXFPolyline.GetEntityName: string;
begin
  Result := DXFV_POLYLINE;
end;

// InterpretCodes
//
function TDXFPolyline.InterpretCodes(Code: Integer;
  Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_START         : begin
          // end of section ?
          if Value = DXFV_SEQEND then FSeqEnd := True else
          // or vertex ?
          if Value = DXFV_VERTEX then begin
            // yes it is a vertex, add it to the list
            SetLength(PointList, Length(PointList) + 1);
            PointList[Length(PointList) - 1] := TDXFVertex.Create(Layers);
            PointList[Length(PointList) - 1].LoadFromStream(DXFStream);
          end;
        end;
      DXF_70FLAG        : Flag := StrToInt(Value); // Polyline flag
      DXF_71FLAG        : ; // Polygon mesh M vertex count (optional; default = 0)
      DXF_72FLAG        : ; // Polygon mesh N vertex count (optional; default = 0)
      DXF_73FLAG        : ; // Smooth surface M density (optional; default = 0)
      DXF_74FLAG        : ; // Smooth surface N density (optional; default = 0)
      DXF_75FLAG        : ; // Curves and smooth surface type (optional; default = 0);
      DXF_FLOATVAL      : ; // Default start width
      DXF_FLOATVALS1    : ; // Default end width
      DXF_ENTITIES_FLG  : ; // vertices just follow
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFPolyline.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  inherited;

  if Flag <> 0 then WriteCodes(DXF_70FLAG, IntToStr(Flag)); // Polyline flag

  // write vertices
  for i := 0 to Length(PointList) - 1 do
    PointList[i].SaveToStream(DXFStream);

  WriteCodes(DXF_START, DXFV_SEQEND); // close the polyline
end;

{ TDXFVertex }

// GetEntityName
//
function TDXFVertex.GetEntityName: string;
begin
  Result := DXFV_VERTEX;
end;

// InterpretCodes
//
function TDXFVertex.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_70FLAG        : Flag := StrToInt(Value); // Vertex flags
      DXF_FLOATVALS2    : Bulge := StrToFloat(Value); // Bulge (optional; default is 0)
      DXF_ANGLE1        : ; // Curve fit tangent direction
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFVertex.SaveToStream(AStream: TStream);
begin
  inherited;

  if Flag <> 0 then WriteCodes(DXF_70FLAG, IntToStr(Flag)); // Vertex flags
  if Bulge <> 0 then WriteCodes(DXF_FLOATVALS2, FloatToStr(Bulge)); // Bulge (optional; default is 0)
end;

{ TDXFText }

// GetEntityName
//
function TDXFText.GetEntityName: string;
begin
  Result := DXFV_TEXT;
end;

// InterpretCodes
//
function TDXFText.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_TEXT_DEF      : Self.Value := Value; // Default value (the string itself)
      DXF_TEXT_STYLE    : Style := Value; // Text style name (optional, default = STANDARD)
      DXF_FLOATVAL      : Height := StrToFloat(Value); // Text height
      DXF_ANGLE1        : Rotation := StrToFloat(Value); // Text rotation (optional; default = 0)
      DXF_71FLAG        : GenerationFlag := StrToInt(Value); // Text generation flags (optional, default = 0)
      DXF_72FLAG        : JustificationH := StrToInt(Value); // Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
      DXF_73FLAG        : JustificationV := StrToInt(Value); // Vertical text justification type (optional, default = 0) integer codes (not bit-coded)
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFText.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_TEXT_DEF, Value); // Default value (the string itself)
  if Style <>  '' then WriteCodes(DXF_TEXT_STYLE, Style); // Text style name (optional, default = STANDARD)
  WriteCodes(DXF_FLOATVAL, FloatToStr(Height)); // Text height
  if Rotation <> 0 then WriteCodes(DXF_ANGLE1, FloatToStr(Rotation)); // Text rotation (optional; default = 0)
  if GenerationFlag <> 0 then WriteCodes(DXF_71FLAG, IntToStr(GenerationFlag)); // Text generation flags (optional, default = 0)
  if JustificationH <> 0 then WriteCodes(DXF_72FLAG, IntToStr(JustificationH)); // Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
  if JustificationV <> 0 then WriteCodes(DXF_73FLAG, IntToStr(JustificationV)); // Vertical text justification type (optional, default = 0) integer codes (not bit-coded)
end;

{ TDXF3DFace }

// GetEntityName
//
function TDXF3DFace.GetEntityName: string;
begin
  Result := DXFV_3DFACE;
end;

// InterpretCodes
//
function TDXF3DFace.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_OTHER_X_1     : Second.X := StrToFloat(Value);
      DXF_OTHER_Y_1     : Second.Y := StrToFloat(Value);
      DXF_OTHER_Z_1     : Second.Z := StrToFloat(Value);
      DXF_OTHER_X_2     : Third.X := StrToFloat(Value);
      DXF_OTHER_Y_2     : Third.Y := StrToFloat(Value);
      DXF_OTHER_Z_2     : Third.Z := StrToFloat(Value);
      DXF_OTHER_X_3     : Fourth.X := StrToFloat(Value);
      DXF_OTHER_Y_3     : Fourth.Y := StrToFloat(Value);
      DXF_OTHER_Z_3     : Fourth.Z := StrToFloat(Value);
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXF3DFace.SaveToStream(AStream: TStream);
begin
  inherited;

  Write3DPoint(Second, DXF_OTHER_X_1); // 2nd point
  Write3DPoint(Third, DXF_OTHER_X_2); // 3rd point
  Write3DPoint(Fourth, DXF_OTHER_X_3); // 4th point
end;

{ TDXFInsert }

// Create
//
constructor TDXFInsert.Create(ALayersList: TDXFLayersList;
  ABlocksList: TDXFBlocksList);
begin
  inherited Create(ALayersList);

  Blocks := ABlocksList;

  // initialize the scale
  Scale.X := 1;
  Scale.Y := 1;
  Scale.Z := 1;
end;

// GetEntityName
//
function TDXFInsert.GetEntityName: string;
begin
  Result := DXFV_INSERT;
end;

// InterpretCodes
//
function TDXFInsert.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_NAME          : BlockName := Value; // The block to insert
      DXF_FLOATVALS1    : Scale.X := StrToFloat(Value); // X scale factor (optional; default = 1)
      DXF_FLOATVALS2    : Scale.Y := StrToFloat(Value); // Y scale factor (optional; default = 1)
      DXF_FLOATVALS3    : Scale.Z := StrToFloat(Value); // Z scale factor (optional; default = 1)
      DXF_ANGLE1        : Rotation := StrToFloat(Value); // Rotation angle (optional; default = 0)
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFInsert.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_NAME, BlockName); // The block to insert
  if Scale.X <> 1 then WriteCodes(DXF_FLOATVALS1, FloatToStr(Scale.X)); // X scale factor (optional; default = 1)
  if Scale.Y <> 1 then WriteCodes(DXF_FLOATVALS2, FloatToStr(Scale.Y)); // Y scale factor (optional; default = 1)
  if Scale.Z <> 1 then WriteCodes(DXF_FLOATVALS3, FloatToStr(Scale.Z)); // Z scale factor (optional; default = 1)
  if Rotation <> 1 then WriteCodes(DXF_ANGLE1, FloatToStr(Rotation)); // Rotation angle (optional; default = 0)
end;

{ TDXFSolid }

// GetEntityName
//
function TDXFSolid.GetEntityName: string;
begin
  Result := DXFV_SOLID;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

  // create the list of supported entities
  SupportedDXFEntities := TStringList.Create;


  with SupportedDXFEntities do begin
    // the list need to be sorted in order to do searches
    Sorted := True;

    // add supported entities and classes
    AddObject(DXFV_ARC,         TObject(TDXFArc));
    AddObject(DXFV_BLOCK,       TObject(TDXFBlock));
    AddObject(DXFV_CIRCLE,      TObject(TDXFCircle));
    AddObject(DXFV_LINE,        TObject(TDXFLine));
    AddObject(DXFV_POINT,       TObject(TDXFPoint));
    AddObject(DXFV_POLYLINE,    TObject(TDXFPolyline));
    AddObject(DXFV_TEXT,        TObject(TDXFText));
    AddObject(DXFV_VERTEX,      TObject(TDXFVertex));
    AddObject(DXFV_3DFACE,      TObject(TDXF3DFace));
    AddObject(DXFV_SOLID,       TObject(TDXFSolid));
//    AddObject(DXFV_SPLINE,      TObject(TDXFSpline)); // not yet supported

    AddObject(DXFV_INSERT,      TObject(TDXFInsert)); // insert is special
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
finalization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

  // frees the list of supported entities
  SupportedDXFEntities.Free;

end.
