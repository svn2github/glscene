{: TypesDXF<p>

  Types for DXF reading and writing<p>

  <b>History :</b><font size=-1><ul>
    <li>08/02/03 - DA - Fixed a bug, not detecting that the color was set in TDXFEntity
    <li>25/01/03 - DA - Moved InterpretCodes and EndOfPart methods in the TDXFIOObject object,
                        Moved TDXFIOObject here,
                        Refactored some parts of the class hierarchy,
                        Added more SaveToStream implementations
    <li>22/01/03 - DA - Changes on the color storage,
                        Completed TDXFEntity and TDXFHeader SaveToStream method
    <li>21/01/03 - DA - Added the TDXFEntityClass type in order to make a supported entities' classes list
    <li>02/12/02 - DA - Moved objects to the ObjectsDXF unit
    <li>01/12/02 - DA - Entitie knows their layer now and use its color 
    <li>30/11/02 - DA - Now reading base information from the dxf header,
                        Moved TDXFEntitiesList here, Added TDXFLayersList
    <li>29/11/02 - DA - Support for Polylines, Vertices and Text
    <li>27/11/02 - DA - Now reading Points, Lines, Circles, Arcs
    <li>23/11/02 - DA - Removed TypeName, added TDXFEntity.InterpretCodes
    <li>16/11/02 - DA - TDXFEntity.LoadFromStream method is now virtual
    <li>20/10/02 - SJ - Added LoadFromStream method to TDXFEntity
    <li>12/10/02 - SJ - Unit creation, added main types
  </ul></font>
}

unit TypesDXF;

interface

uses

  // Delphi
  Classes,
  Contnrs,
  Graphics,

   cene
  GLVectorGeometry,
  GLVectorTypes,

  // DXF
  CodesValuesDXF
  ;

const

  //: DXF standard colours
  Def_Cols = 12;
  DXF_Layer_Colours : array[0..Def_Cols] of TColor = (clBlack, // zero - not used
    clRed,    clYellow, clLime,   clAqua,   clBlue,   clPurple, clWhite,
    clOlive,  clFuchsia,clTeal,   clGray,   clDkGray);

type

  // T3DPoint
  //
  T3DPoint = TAffineVector;

  // forward declarations
  TDXFLayer = class;
  TDXFLayersList = class;

  // TDXFIOObject
  //
  {: Object which can read and write to DXF }
  TDXFIOObject = class
  private
    FStream: TStream;

    function ReadChar: Char;
    function ReadLn: String;
    procedure WriteLn(Line: string);
  protected
    //: Read a code/value pair
    procedure ReadCodes(var Code: Integer; var Value: String);
    //: Write a code/value pair
    procedure WriteCodes(Code: Integer; Value: String);

    //: read a 3D point in the DXF file
    procedure Read3DPoint(var V3DPoint: T3DPoint);
    //: write a 3D point to a DXF file
    procedure Write3DPoint(A3DPoint: T3DPoint; ABaseCode: Integer = DXF_PRIMARY_X);

    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; virtual;

    //: says if this code pair means the end of this DXF part
    function EndOfPart(Code: Integer; Value: String): boolean; virtual;

    //: DXF stream
    property DXFStream: TStream read FStream write FStream;

  public
    //: load from a stream
    procedure LoadFromStream(AStream: TStream); virtual;
    //: save to a stream
    procedure SaveToStream(AStream: TStream); virtual;
  end;

  // TDXFEntity
  //
  {: The base DXF entity type. }
  TDXFEntity = class(TDXFIOObject)
  private
    FColorSet: boolean; // true if the color was set
    FColorNum: Integer;            // 62
    {
    FOCStoWCS: TMatrix4d;
    FOCSAxis: T3DPoint;
    }

    function GetColor: T3DPoint;
    procedure SetColorNum(Value: integer);

  protected
    Layers: TDXFLayersList; // list of layers in the DXF file

    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

    //: returns the entity's DXF name
    function GetEntityName: string; virtual;

    {
    //: transformation matrix for OCS to WCS coordinates conversion
    property OCStoWCS: TMatrix4d read FOCSto"WCS write FOCStoWCS;
    //: OCS coordinates axis
    property OCSAxis: T3DPoint read FOCSAxis write FOCSAxis;
    }

  public
    Layer: TDXFLayer; // 8, the entity's layer
    Visible: Boolean;            // 60
    LineTypeName: string;         // 6

    //: constructor
    constructor Create(ALayersList: TDXFLayersList); reintroduce;

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

    //: the color property
    property Color: T3DPoint read GetColor;
    //: the colornum property
    property ColorNum: integer read FColorNum write SetColorNum;
  end;

  // TDXFEntityClass
  //
  TDXFEntityClass = class of TDXFEntity;

  // TDXFHeader
  //
  {: The DXF header type. }
  TDXFHeader = class(TDXFIOObject)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

  public
    ExtMin, ExtMax: T3DPoint;

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

  end;

  // TDXFLayer
  //
  {: The DXF layer type. }
  TDXFLayer = class(TDXFIOObject)
  protected
    //: interpret a code pair
    function InterpretCodes(Code: Integer; Value: String): boolean; override;

  public
    LayerName: string; // 2
    ColorNum: Integer;            // 62

    //: save to a stream
    procedure SaveToStream(AStream: TStream); override;

  end;

  // TDXFEntitiesList
  //
  {: Contains all (supported) entities of a DXF file }
  TDXFEntitiesList = class
  private
    FEntitiesList : TObjectList;

    function GetEntitiesCount: Integer;
    function GetEntity(Index: Integer): TDXFEntity;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    //: add an entity to the list
    procedure AddEntity(const AEntity: TDXFEntity);

    //: the number of entities
    property Count : Integer read GetEntitiesCount;
    //: use this to access an entity
    property Entity[Index: Integer]: TDXFEntity read GetEntity;
  end;

  // TDXFLayersList
  //
  {: Contains all layers entities of a DXF file }
  TDXFLayersList = class
  private
    FLayers: TDXFEntitiesList;

    function GetCount: Integer;
    function GetLayer(Index: Integer): TDXFLayer;
    function GetLayerByName(Name: string): TDXFLayer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    //: add a layer to the list
    procedure AddLayer(const ALayer: TDXFLayer);

    //: the number of layer
    property Count : Integer read GetCount;
    //: use this to access a layer
    property Layer[Index: Integer]: TDXFLayer read GetLayer;
    //: use this to access a layer by its layer name
    property LayerByName[Name: String]: TDXFLayer read GetLayerByName; 

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
  Windows
  ;

const
  cCR = $0D;
  cLF = $0A;
  cEOF = $1A;


{ TDXFIOObject }

// EndOfPart
//
function TDXFIOObject.EndOfPart(Code: Integer; Value: String): boolean;
begin
  Result := (Code = DXF_START);
end;

// InterpretCodes
//
{: @param Code       Code to interpret
   @param Value      Value to interpret

   @return True if the code was interpreted, false otherwise }
function TDXFIOObject.InterpretCodes(Code: Integer;
  Value: String): boolean;
begin
  Result := False;
end;

// LoadFromStream
//
procedure TDXFIOObject.LoadFromStream(AStream: TStream);
var
  Code: integer;
  Value: string;
  EndEntity: boolean;
  PosInStream: Int64;
begin
  FStream := AStream; // save the stream in an instance variable

  repeat
    // save the stream position
    PosInStream := DXFStream.Position;

    // read codes
    ReadCodes(Code, Value);

    // check if this is the end of this entity
    EndEntity := EndOfPart(Code, Value);

    // interpret codes
    if not InterpretCodes(Code, Value) and not EndEntity then
      // if there is unsuported codes then write it
//      OutputDebugString(PAnsiChar(Format('Unsupported codes %d %s', [Code, Value])))
      ;

  until EndEntity;

  // revert to the last stream position
  DXFStream.Seek(PosInStream, soBeginning);
end;

// Read3DPoint
//
{: @param V3DPoint       The point to read }
procedure TDXFIOObject.Read3DPoint(var V3DPoint: T3DPoint);
var
  Code: integer;
  Value: string;
  EndEntity: boolean;
  PosInStream: Int64;
begin
  EndEntity := False;

  repeat
    // save the stream position
    PosInStream := DXFStream.Position;

    // read codes
    ReadCodes(Code, Value);

    // interpret codes
    case Code of
      DXF_PRIMARY_X       : V3DPoint.X := StrToFloat(Value); // X coordinates
      DXF_PRIMARY_Y       : V3DPoint.Y := StrToFloat(Value); // Y coordinates
      DXF_PRIMARY_Z       : V3DPoint.Z := StrToFloat(Value); // Z coordinates
      else EndEntity := True;
    end;

  until EndEntity;

  // revert to the last stream position
  DXFStream.Seek(PosInStream, soBeginning);
end;

// ReadChar
//
{: @return The readed char or cEOF if end of file }
function TDXFIOObject.ReadChar: Char;
begin
  if FStream.Read(Result, Sizeof(Char)) = 0 then Result := Char(cEOF);
end;

// ReadCodes
//
{: @param Code          Will contains the code
   @param Value         Will contains the value }
procedure TDXFIOObject.ReadCodes(var Code: Integer; var Value: String);
var
  Line: string;
begin
  // skip blanks line in the DXF files
  repeat
    Line := ReadLn;
  until (Length(Trim(Line)) > 0) or (Value = 'EOF') or (DXFStream.Position >= DXFStream.Size);
  Code := StrToInt(Line);

  Value := ReadLn;
end;

// ReadLn
//
{: @return The line read or cEOF if end of file }
function TDXFIOObject.ReadLn: String;
var
  bufsize, charsRead: Integer;
  ch: Char;
begin
  bufsize := 256;
  charsRead := 0;
  SetLength(Result, bufsize);
  Repeat
    ch := ReadChar;
    Case ch Of
      #13: Begin { end of line }
          If ReadChar <> #10 Then
            FStream.Seek( -1, soFromCurrent );
        End; { Case #13 }
      #10, #26: Begin { alternative end of line }
        ch := #13
      End
      Else Begin
        Inc(charsRead);
        If charsRead > bufsize Then Begin
          Inc(bufsize, 128);
          SetLength(Result, bufsize );
        End; { If }
        Result[charsRead]:= ch;
      End; { Else }
    End; { Case }
  Until ch = #13;
  SetLength(Result, charsRead );
End;


// SaveToStream
//
procedure TDXFIOObject.SaveToStream(AStream: TStream);
begin
  FStream := AStream; // save the stream in an instance variable
end;

// Write3DPoint
//
{: @param A3DPoint  The point to write
   @param ABaseCode Base code for the point (ex: 10 for DXF_PRIMARY_X), default is DXF_PRIMARY_X }
procedure TDXFIOObject.Write3DPoint(A3DPoint: T3DPoint; ABaseCode: Integer);
begin
  if A3DPoint.X <> 0 then WriteCodes(ABaseCode, FloatToStr(A3DPoint.X));
  if A3DPoint.Y <> 0 then WriteCodes(ABaseCode + 10, FloatToStr(A3DPoint.Y));
  if A3DPoint.Z <> 0 then WriteCodes(ABaseCode + 20, FloatToStr(A3DPoint.Z));
end;

// WriteCodes
//
{: @param Code          Contains the code
   @param Value         Contains the value }
procedure TDXFIOObject.WriteCodes(Code: Integer; Value: String);
begin
  // write the code
  WriteLn(IntToStr(Code));
  // write the value
  WriteLn(Value);
end;

// WriteLn
//
{: @param Line          The line to write }
procedure TDXFIOObject.WriteLn(Line: string);
begin
  // add a carrier return
  Line := Line + #13#10;

  // write the line
  FStream.Write(Line[1], Length(Line));
end;


//--------------------------
//--------------------------{ TDXFEntity }------------------------------------
//--------------------------

// Create
//
{: @param ALayersList   List of layers in the DXF files }
constructor TDXFEntity.Create(ALayersList: TDXFLayersList);
begin
  inherited Create;

  Layers := ALayersList;
end;

// GetColor
//
function TDXFEntity.GetColor: T3DPoint;
var
  WinColor : TColor;
begin
  WinColor := ColorToRGB(DXF_Layer_Colours[ColorNum mod (Def_Cols + 1)]);
  Result.X := GetRValue(WinColor) / 256;
  Result.Y := GetGValue(WinColor) / 256;
  Result.Z := GetBValue(WinColor) / 256;
  // write non-standard colors
  //if StrToInt(Value) > Def_Cols then OutputDebugString(PAnsiChar('Color='+Value));
end;

// GetEntityName
//
function TDXFEntity.GetEntityName: string;
begin
  Result := '';
end;

// InterpretCodes
//
{: @param Code       Code to interpret
   @param Value      Value to interpret

   @return True if the code was interpreted, false otherwise }
function TDXFEntity.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  Result := True;

  case Code of
    DXF_COLORNUM        : begin
                            ColorNum := StrToInt(Value); // entity's color number
                            FColorSet := True; // the color is set
                          end;
    DXF_LAYER_NAME      : begin
                            Layer := Layers.GetLayerByName(Value); // set the layer
                            if (not FColorSet) and Assigned(Layer) then
                              ColorNum := Layer.ColorNum; // set the color to the layer color
                          end;
    DXF_VISIBLE         : Visible := StrToInt(Value) = 0; // visibility (0=visible, 1=invisible)
    DXF_LINE_TYPE       : LineTypeName := Value; // Linetype name (present if not BYLAYER). The special name BYBLOCK indicates a floating linetype (optional).
  else Result := False; // codes unknown
  end;
end;

// SaveToStream
//
procedure TDXFEntity.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_START, GetEntityName); // write the entity name

  if FColorSet then WriteCodes(DXF_COLORNUM, IntToStr(ColorNum)); // color
  if Assigned(Layer) then WriteCodes(DXF_LAYER_NAME, Layer.LayerName); // layer name
  if Visible then WriteCodes(DXF_VISIBLE, '1') else WriteCodes(DXF_VISIBLE, '0'); // visibility
  if LineTypeName <> '' then WriteCodes(DXF_LINE_TYPE, LineTypeName); // line type
end;

// SetColorNum
//
procedure TDXFEntity.SetColorNum(Value: integer);
begin
  FColorNum := Value;
  FColorSet := True; // the color was set
end;

{ TDXFHeader }

// InterpretCodes
//
function TDXFHeader.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  Result := True;

  case Code of
    DXF_VAR_NAME        : begin
        if Value = DXFV_EXTMAX then Read3DPoint(ExtMax) else
        if Value = DXFV_EXTMIN then Read3DPoint(ExtMin) else
        if Value = DXFV_CECOLOR then { TODO : read the color here } else
        Result := False; // codes unknown
      end;
  else Result := False; // codes unknown
  end;
end;

// SaveToStream
//
procedure TDXFHeader.SaveToStream(AStream: TStream);
begin
  inherited;
  
  WriteCodes(DXF_START, DXFV_SECTION); // open section
  WriteCodes(DXF_NAME, DXFV_HEADER); // write section name

  WriteCodes(DXF_VAR_NAME, DXFV_EXTMAX); // extmax
  Write3DPoint(ExtMax);
  WriteCodes(DXF_VAR_NAME, DXFV_EXTMIN); // extmin
  Write3DPoint(ExtMin);

  WriteCodes(DXF_START, DXFV_ENDSEC); // close section
end;

{ TDXFLayer }

// InterpretCodes
//
function TDXFLayer.InterpretCodes(Code: Integer; Value: String): boolean;
begin
  // handle generic codes in generic entities
  Result := inherited InterpretCodes(Code, Value);

  // handle our specific codes here
  if not Result then begin
    Result := True;

    case Code of
      DXF_NAME          : LayerName := Value; // Layer name
      DXF_COLORNUM      : ColorNum := StrToInt(Value); // entity's color number
    else Result := False; // codes unknown
    end;
  end;
end;

// SaveToStream
//
procedure TDXFLayer.SaveToStream(AStream: TStream);
begin
  inherited;

  WriteCodes(DXF_START, DXFV_LAYER); // start the layer

  if LayerName <> '' then WriteCodes(DXF_NAME, LayerName); // Layer name
  WriteCodes(DXF_COLORNUM, IntToStr(ColorNum)); // color
end;

{ TDXFEntitiesList }

// AddEntity
//
{: @param AEntity       The entity to add }
procedure TDXFEntitiesList.AddEntity(const AEntity: TDXFEntity);
begin
  FEntitiesList.Add(AEntity);
end;

// Create
//
constructor TDXFEntitiesList.Create;
begin
  inherited;

  // the list own the entities
  FEntitiesList := TObjectList.Create(True);
end;

// Destroy
//
destructor TDXFEntitiesList.Destroy;
begin
  // frees entities
  FEntitiesList.Free;

  inherited;
end;

// GetEntitiesCount
//
{: @return The number of entities in the list }
function TDXFEntitiesList.GetEntitiesCount: Integer;
begin
  Result := FEntitiesList.Count;
end;

// GetEntity
//
{: @param Index         The index of the entity to retrieve
   @return The entity whith this index }
function TDXFEntitiesList.GetEntity(Index: Integer): TDXFEntity;
begin
  Result := TDXFEntity(FEntitiesList[Index]);
end;


{ TDXFLayersList }

// AddLayer
//
procedure TDXFLayersList.AddLayer(const ALayer: TDXFLayer);
begin
  FLayers.AddEntity(TDXFEntity(ALayer));
end;

// Create
//
constructor TDXFLayersList.Create;
begin
  inherited;
  FLayers := TDXFEntitiesList.Create;
end;

// Destroy
//
destructor TDXFLayersList.Destroy;
begin
  FLayers.Free;
  
  inherited;
end;

// GetCount
//
function TDXFLayersList.GetCount: Integer;
begin
  Result := FLayers.Count;
end;

// GetLayer
//
function TDXFLayersList.GetLayer(Index: Integer): TDXFLayer;
begin
  Result := TDXFLayer(FLayers.GetEntity(Index));
end;

// GetLayerByName
//
function TDXFLayersList.GetLayerByName(Name: string): TDXFLayer;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if GetLayer(i).LayerName = Name then begin
      Result := GetLayer(i);
      exit;
    end;
end;

end.

