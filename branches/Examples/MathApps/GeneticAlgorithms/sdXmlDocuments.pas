{ unit sdXmlDocuments

  This is a small-footprint implementation to read and write XML documents.

  You can use this code to read XML documents from files, streams or strings.
  The load routine generates events that can be used to display load progress
  on the fly.

  Author: Nils Haeck M.Sc. (n.haeck@simdesign.nl)
  Version: 1.43
  Last Modified: 21-Sep-2004
  Original date: 01-Apr-2003

  Latest changes:
  11-Mar-2004
    Added property BinaryString which can be used to easily add binary data (from
    e.g. blobfields: ANode.BinaryString := AField.AsString)
  28-May-2004
    Added method FindNodes, that allows finding all nodes below the node with
    specific name.
  29-May-2004
    Added "ShouldRecurse" parameter to function TXmlNode.NodeByAttributeName.
    Added procedure TXmlNode.NodeInsert()
    Added procedure TXmlNode.NodeNewAtIndex()
    Moved examples out of source code into documentation
  09-Jun-2004
    Added ExtraNodes[] array that holds any nodes in the XML document that
    are not part of the root but still need to be preserved, like <?mapping ...?>
    Added xeDoctype extra node type <!DOCTYPE...]>
    Added xeExclam extra node type <!...>
    Added xeQuestion extra node type <?...?>
  15-Jul-2004
    Fixed bug in FindNodes method
  23-Jul-2004
    Added property UseFullNodes to TXmlDocument
  06-Aug-2004
    Removed unit Dialogs in registered version (required only for trial reminder
    message), using defines.
  08-Aug-2004
    Added methods TXmlNode.NodeRemove, NodeIndexOf and Delete.
  16-Aug-2004
    Added public function DateTimeToString
    Made TXmlNode.WriteToString public method
    Changed name of TXmlDocument to TsdXmlDocument, and changed name of unit to
    sdXmlDocuments. WARNING this change may impact the application code! Solution:
    simply rename all the occurances of above object class and unit name.
  17-Aug-2004
    Removed bug with <!DOCTYPE...> declaration
  27-Aug-2004
    Added support for unicode. This is done by allowing strings in the document
    and nodes to be encoded as UTF8 (see property TXmlDocument.Utf8Encoded).
    Added support for loading UTF8 files.
    Added TXmlNode methods ValueAsWidestring, ToWidestring, FromWidestring,
    ToAnsiString, FromAnsiString, ReadWidestring, WriteWidestring.
    Added TXmlNode method AttributeDelete.
  01-Sep-2004
    Added version string constant sdXmlDocumentsVersion.
    Added conversion of character references (&#...; and &#x...;).
  02-Sep-2004
    Renamed DateTimeToString and DateTimeFromStringDefault to sdDateTimeToString
    and sdDateTimeFromStringDefault to avoid naming conflict with SysUtils unit.
  17-Sep-2004
    Made compatible with Delphi 2, 3 and 4.
  20-Sep-2004
    Added method TXmlNode.ReadAttributeString
  21-Sep-2004
    Enhancements to stream converters to make writing faster when no conversion
    is neccesary.
  22-Sep-2004
    Fixed bug in AttributeByName
  25-Sep-2004
    Added methods for buffered reading/writing to streams, this immensely speeds up
    disk-based access. See classes TsdBufferedReadStream/TsdBufferedWriteStream.
    Added option TsdXmlDocument.DropCommentsOnParse. This provides for easier
    processing of XML files with occasional comments in them. By default this
    option is off.

  Copyright (c) 2002-2004 Simdesign, Nils Haeck

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.

  Note: any external encoding (ANSI, UTF16, etc) is converted to an internal
  encoding that is ANSI or UTF8. When the loaded document is ANSI based,
  the encoding will be ANSI, in other cases (UTF8, UTF16) the encoding
  will be UTF8.
}

{$DEFINE USEGRAPHICS}                             // uncomment if you do not want to include the Graphics unit.

// Delphi and BCB versions

// BCB 4
{$IFDEF VER125}
{$DEFINE D4UP}
{$ENDIF}
// Delphi 4
{$IFDEF VER120}
{$DEFINE D4UP}
{$ENDIF}
// Delphi and BCB 5
{$IFDEF VER130}
{$DEFINE D4UP}
{$DEFINE D5UP}
{$ENDIF}
//Delphi and BCB 6
{$IFDEF VER140}
{$DEFINE D4UP}
{$DEFINE D5UP}
{$ENDIF}
//Delphi and BCB 7
{$IFDEF VER150}
{$DEFINE D4UP}
{$DEFINE D5UP}
{$ENDIF}

unit sdXmlDocuments;

interface

uses
    Windows,
    Classes,
{$IFDEF USEGRAPHICS}
    Graphics,
{$ENDIF}
    SysUtils ;

const

    // Current version of the XmlDocuments unit
    sdXMLDocumentsVersion = '1.43' ;

    // Delphi 3 and below stubs
{$IFNDEF D4UP}
type
    TReplaceFlags = set of ( rfReplaceAll, rfIgnoreCase ) ;
function StringReplace( const S, OldPattern, NewPattern : string ;
    Flags : TReplaceFlags ) : string ;
function Max( A, B : integer ) : integer ;
function Min( A, B : integer ) : integer ;
{$ENDIF}
// Delphi 4 stubs

{$IFNDEF D5UP}
type
    widestring = string ;
function AnsiPos( const Substr, S : string ) : Integer ;
function AnsiQuotedStr( const S : string ; Quote : Char ) : string ;
function AnsiExtractQuotedStr( var Src : PChar ; Quote : Char ) : string ;
procedure FreeAndNil( var Obj ) ;
{$ENDIF}

type

    // Note on TsdXmlDocument.Format:
    // - xfReadable (default) to be able to read the xml file with a standard editor.
    // - xfCompact to save the xml fully compliant and at smallest size
    TXmlFormatType = (
        xfReadable,                               // Save in readable format with CR-LF and indents
        xfCompact                                 // Save without any control chars except LF after declarations
        ) ;

    // TXmlElementType enumerates the different kinds of elements that can be found
    // in the XML document.
    TXmlElementType = (
        xeNormal,                                 // Normal element <name {attr}>[value][sub-elements]</name>
        xeClosing,                                // Closing element </name>
        xeDirect,                                 // Direct element <name {attr}/>
        xeComment,                                // Comment <!--{comment}-->
        xeCData,                                  // literal data <![CDATA[{data}]]>
        xeDeclaration,                            // XML declaration <?xml{declaration}?>
        xeStylesheet,                             // Stylesheet <?xml-stylesheet{stylesheet}?>
        xeDoctype,                                // DOCTYPE DTD declaration <!DOCTYPE{spec}>
        xeExclam,                                 // Any <!data>
        xeQuestion,                               // Any <?data?>
        xeUnknown                                 // Any <data>
        ) ;

    // Choose what kind of binary encodig will be used when calling
    // TXmlNode BufferRead and BufferWrite.
    TBinaryEncodingType = (
        xbeBinHex, { With this encoding, each byte is stored as a hexadecimal
        number, e.g. 0 = 00 and 255 = FF.                        }
        xbeBase64, { With this encoding, each group of 3 bytes are stored as 4
        characters, requiring 64 different characters.}
        xbeSixBit                                 // Present for backwards compatibility, will be removed in v3.0
        ) ;

    // Definition of different methods of string encoding.
    TStringEncodingType = (
        se8Bit,                                   // General 8 bit encoding, encoding must be determined from encoding declaration
        seUCS4BE,                                 // UCS-4 Big Endian
        seUCS4LE,                                 // UCS-4 Little Endian
        seUCS4_2143,                              // UCS-4 unusual octet order (2143)
        seUCS4_3412,                              // UCS-4 unusual octet order (3412)
        se16BitBE,                                // General 16 bit Big Endian, encoding must be determined from encoding declaration
        se16BitLE,                                // General 16 bit Little Endian, encoding must be determined from encoding declaration
        seUTF8,                                   // UTF-8
        seUTF16BE,                                // UTF-16 Big Endian
        seUTF16LE,                                // UTF-16 Little Endian
        seEBCDIC                                  // EBCDIC flavour
        ) ;

var

    // XML Defaults

    cDefaultEncodingString : string = 'windows-1252' ;
    cDefaultExternalEncoding : TStringEncodingType = se8bit ;
    cDefaultVersionString : string = '1.0' ;
    cDefaultXmlFormat : TXmlFormatType = xfCompact ;
    cDefaultWriteOnDefault : boolean = True ;
    cDefaultBinaryEncoding : TBinaryEncodingType = xbeBase64 ;
    cDefaultUtf8Encoded : boolean = False ;

type

    TXmlNode = class ;
    TsdXmlDocument = class ;
    TsdCodecStream = class ;

    // An event that is based on the TXmlNode object Node.
    TXmlNodeEvent = procedure( Sender : TObject ; Node : TXmlNode ) of object ;

    // An event that is used to indicate load or save progress.
    TXmlProgressEvent = procedure( Sender : TObject ; Size : integer ) of object ;

    // The TXmlNode represents an element in the XML file. Each TsdXmlDocument holds
    // one Root element. Under ths root element, sub-elements can be nested (there
    // is no limit on how deep). Property ElementType defines what kind of element
    // this node is.
    TXmlNode = class( TPersistent )
    private
        FAttributes : TStrings ;                  // List with attributes
        FDocument : TsdXmlDocument ;              // Pointer to parent XmlDocument
        FElementType : TXmlElementType ;          // The type of element
        FName : string ;                          // The element name
        FNodes : TList ;                          // These are the child elements
        FParent : TXmlNode ;                      // Pointer to parent element
        FTag : integer ;                          // A value the developer can use
        FValue : string ;                         // The *escaped* value
        function GetValueAsString : string ;
        procedure SetAttributeName( Index : integer ; const Value : string ) ;
        procedure SetAttributeValue( Index : integer ; const Value : string ) ;
        procedure SetValueAsString( const AValue : string ) ;
        function GetIndent : string ;
        function GetLineFeed : string ;
        function GetTreeDepth : integer ;
        function GetAttributeCount : integer ;
        function GetAttributePair( Index : integer ) : string ;
        function GetAttributeName( Index : integer ) : string ;
        function GetAttributeValue( Index : integer ) : string ;
        function GetWriteOnDefault : boolean ;
        function GetBinaryEncoding : TBinaryEncodingType ;
        function GetCascadedName : string ;
        function QualifyAsDirectNode : boolean ;
        procedure SetName( const Value : string ) ;
        function GetFullPath : string ;
        procedure SetBinaryEncoding( const Value : TBinaryEncodingType ) ;
        function GetBinaryString : string ;
        procedure SetBinaryString( const Value : string ) ;
        function UseFullNodes : boolean ;
        function GetValueAsWidestring : widestring ;
        procedure SetValueAsWidestring( const Value : widestring ) ;
    protected
        function GetNodes( Index : integer ) : TXmlNode ; virtual ;
        function GetNodeCount : integer ; virtual ;
        procedure ParseTag( const AValue : string ; TagStart, TagClose : integer ) ;
        procedure ReadFromStream( S : TStream ) ; virtual ;
        procedure ReadFromString( const AValue : string ) ; virtual ;
        function ScanTag( const AValue : string ; var Start, Close, Next : integer ; var ATag : TXmlElementType ) : boolean ;
        function UnescapeString( const AValue : string ) : string ; virtual ;
        function Utf8Encoded : boolean ;
        function WriteInnerTag : string ; virtual ;
        procedure WriteToStream( S : TStream ) ; virtual ;
    public
        // Create a new TXmlNode object. ADocument must be the TsdXmlDocument that is
        // going to hold this new node.
        constructor Create( ADocument : TsdXmlDocument ) ; virtual ;
        // \Create a new TXmlNode with name AName. ADocument must be the TsdXmlDocument
        // that is going to hold this new node.
        constructor CreateName( ADocument : TsdXmlDocument ; const AName : string ) ; virtual ;
        // \Create a new TXmlNode with name AName and string value AValue. ADocument
        // must be the TsdXmlDocument that is going to hold this new node.
        constructor CreateNameValue( ADocument : TsdXmlDocument ; const AName, AValue : string ) ; virtual ;
        // \Create a new TXmlNode with XML element type AType. ADocument must be the
        // TsdXmlDocument that is going to hold this new node.
        constructor CreateType( ADocument : TsdXmlDocument ; AType : TXmlElementType ) ; virtual ;
        procedure Assign( Source : TPersistent ) ; override ;
        // Call Delete to delete this node completely from the parent node list. This
        // call only succeeds if the node has a parent. It has no effect when called for
        // the root node.
        procedure Delete ; virtual ;
        // \Delete all nodes that are empty (this means, which have no subnodes, no
        // attributes, and no value assigned). This procedure works recursively.
        procedure DeleteEmptyNodes ;
        // Destroy a TXmlNode object. This will free the child node list automatically.
        // Never call this method directly. All TXmlNodes in the document will be
        // recursively freed when TsdXmlDocument.Free is called.
        destructor Destroy ; override ;
{$IFDEF D4UP}
        // Use this method to add an integer attribute to the node.
        procedure AttributeAdd( const AName : string ; AValue : integer ) ; overload ;
{$ENDIF}
        // Use this method to add a string attribute with value AValue to the node.
        procedure AttributeAdd( const AName, AValue : string ) ; 
{$IFDEF D4UP} overload ;
{$ENDIF}
        // This function returns the attribute value for the attribute that has name AName.
        function AttributeByName( const AName : string ) : string ;
        // Use this method to delete the attribute at Index in the list. Index must be
        // equal or greater than 0, and smaller than AttributeCount. Using an index
        // outside of that range has no effect.
        procedure AttributeDelete( Index : integer ) ;
        // Use this method to find the index of an attribute with name AName.
        function AttributeIndexByname( const AName : string ) : integer ;
        // \Clear all attributes from the current node.
        procedure AttributesClear ; virtual ;
        // Use this method to read binary data from the node into Buf with a length of Size.
        procedure BufferRead( var Buf ; ASize : integer ) ; virtual ;
        // Use this method to write binary data in Buf with a length of ASize to the
        // current node. The data will appear as text using either BinHex or Base64
        // method) in the final XML document.
        // Notice that XmlDocument does only support up to 2Gb bytes of data per file,
        // so do not use this option for huge files. The binary encoding method (converting
        // binary data into text) can be selected using property BinaryEncoding.
        // xbeBase64 is most efficient, but slightly slower. Always use identical methods
        // for reading and writing.
        procedure BufferWrite( const Buf ; ASize : integer ) ; virtual ;
        // Returns the length of the data in the buffer, once it would be decoded by
        // method xbeBinHex or xbeBase64. If BinaryEncoding is xbeSixBits, this function
        // cannot be used. The length of the unencoded data is determined from the
        // length of the encoded data. For xbeBinHex this is trivial (just half the
        // length), for xbeBase64 this is more difficult (must use the padding characters)
        function BufferLength : integer ; virtual ;
        // Clear all child nodes and attributes, and the name and value of the current
        // XML node. However, the node is not deleted. Call Delete instead for that.
        procedure Clear ; virtual ;
        // Find the first node which has name NodeName. Contrary to the NodeByName
        // function, this function will search the whole subnode tree, using the
        // DepthFirst method. It is possible to search for a full path too, e.g.
        // FoundNode := MyNode.FindNode('/Root/SubNode1/SubNode2/ThisNode');
        function FindNode( const NodeName : string ) : TXmlNode ;
        // Find all nodes which have name NodeName. Contrary to the NodesByName
        // function, this function will search the whole subnode tree
        procedure FindNodes( const NodeName : string ; const AList : TList ) ;
        // Use FromAnsiString to convert a normal ANSI string to a string for the node
        // (name, value, attributes). If the TsdXmlDocument property UtfEncoded is True,
        // the ANSI characters are encoded into UTF8. Use this function if you work
        // with special codebases (characters in the range $7F-$FF) and want to produce
        // unicode or UTF8 XML documents.
        function FromAnsiString( const s : string ) : string ;
        // Use FromWidestring to convert widestring to a string for the node (name, value,
        // attributes). If the TsdXmlDocument property UtfEncoded is True, all
        // character codes higher than $FF are preserved.
        function FromWidestring( const W : widestring ) : string ;
        // Use HasAttribute to determine if the node has an attribute with name AName.
        function HasAttribute( const AName : string ) : boolean ; virtual ;
        // This function returns the index of this node in the parent's node list.
        // If Parent is not assigned, this function returns -1.
        function IndexInParent : integer ;
        // This function returns True if the node has no subnodes and no attributes,
        // and if the node Name and value are empty.
        function IsClear : boolean ; virtual ;
        // This function returns True if the node has no subnodes and no attributes,
        // and if the node value is empty.
        function IsEmpty : boolean ; virtual ;
        // Add the node ANode as a new subelement in the nodelist. The node will be
        // added in position NodeCount (which will be returned).
        function NodeAdd( ANode : TXmlNode ) : integer ; virtual ;
        // This function returns a pointer to the first subnode that has an attribute with
        // name AttribName and value AttribValue. If ShouldRecurse = True (default), the
        // function works recursively, using the depthfirst method.
        function NodeByAttributeValue( const NodeName, AttribName, AttribValue : string ;
            ShouldRecurse : boolean{$IFDEF D4UP} = True{$ENDIF} ) : TXmlNode ;
        // Return a pointer to the first subnode in the nodelist that has name AName.
        // If no subnodes with AName are found, the function returns nil.
        function NodeByName( const AName : string ) : TXmlNode ; virtual ;
        // \Delete the subnode at Index. The node will also be freed, so do not free the
        // node in the application.
        procedure NodeDelete( Index : integer ) ; virtual ;
        // Switch position of the nodes at Index1 and Index2.
        procedure NodeExchange( Index1, Index2 : integer ) ;
        // Extract the node ANode from the subnode list. The node will no longer appear
        // in the subnodes list, so the application is responsible for freeing ANode later.
        function NodeExtract( ANode : TXmlNode ) : TXmlNode ; virtual ;
        // Find the index of the first subnode with name AName.
        function NodeIndexByName( const AName : string ) : integer ; virtual ;
        // Find the index of the first subnode with name AName that appears after or on
        // the index AFrom. This function can be used in a loop to retrieve all nodes
        // with a certain name, without using a helper list. See also NodesByName.
        function NodeIndexByNameFrom( const AName : string ; AFrom : integer ) : integer ; virtual ;
        // Call NodeIndexOf to get the index for ANode in the Nodes array. The first
        // node in the array has index 0, the second item has index 1, and so on. If
        // a node is not in the list, NodeIndexOf returns -1.
        function NodeIndexOf( ANode : TXmlNode ) : integer ;
        // Insert the node ANode at location Index in the list.
        procedure NodeInsert( Index : integer ; ANode : TXmlNode ) ; virtual ;
        // \Create a new node with AName, add it to the subnode list, and return a
        // pointer to it.
        function NodeNew( const AName : string ) : TXmlNode ; virtual ;
        // \Create a new node with AName, and insert it into the subnode list at location
        // Index, and return a pointer to it.
        function NodeNewAtIndex( Index : integer ; const AName : string ) : TXmlNode ; virtual ;
        // Call NodeRemove to remove a specific node from the Nodes array when its index
        // is unknown. The value returned is the index of the item in the Nodes array
        // before it was removed. After an item is removed, all the items that follow
        // it are moved up in index position and the NodeCount is reduced by one.
        function NodeRemove( ANode : TxmlNode ) : integer ;
        // This function returns a pointer to the first node with AName. If this node
        // is not found, then it creates a new node with AName and returns its pointer.
        function NodeReplace( const AName : string ) : TXmlNode ; virtual ;
        // \Clear (and free) the complete list of subnodes.
        procedure NodesClear ; virtual ;
        // Use this procedure to retrieve all nodes that have name AName. Pointers to
        // these nodes are added to the list in AList. AList must be initialized
        // before calling this procedure.
        procedure NodesByName( const AName : string ; const AList : TList ) ;
        // Find the attribute with AName, and convert its value to an integer. If the
        // attribute is not found, or cannot be converted, the default ADefault will
        // be returned.
        function ReadAttributeInteger( const AName : string ; ADefault : integer{$IFDEF D4UP} = 0{$ENDIF} ) : integer ; virtual ;
        function ReadAttributeString( const AName : string ; const ADefault : string{$IFDEF D4UP} = ''{$ENDIF} ) : string ; virtual ;
        // Read the subnode with AName and convert it to a boolean value. If the
        // subnode is not found, or cannot be converted, the boolean ADefault will
        // be returned.
        function ReadBool( const AName : string ; ADefault : boolean{$IFDEF D4UP} = False{$ENDIF} ) : boolean ; virtual ;
{$IFDEF USEGRAPHICS}
        // Read the properties Color and Style for the TBrush object ABrush from the
        // subnode with AName.
        procedure ReadBrush( const AName : string ; ABrush : TBrush ) ; virtual ;
        // Read the subnode with AName and convert its value to TColor. If the
        // subnode is not found, or cannot be converted, ADefault will be returned.
        function ReadColor( const AName : string ; ADefault : TColor{$IFDEF D4UP} = clBlack{$ENDIF} ) : TColor ; virtual ;
        // Read the properties \Name, Color, Size and Style for the TFont object AFont
        // from the subnode with AName.
        procedure ReadFont( const AName : string ; AFont : TFont ) ; virtual ;
        // Read the properties Color, Mode, Style and Width for the TPen object APen
        // from the subnode with AName.
        procedure ReadPen( const AName : string ; APen : TPen ) ; virtual ;
{$ENDIF}
        // Read the subnode with AName and convert its value to TDateTime. If the
        // subnode is not found, or cannot be converted, ADefault will be returned.
        function ReadDateTime( const AName : string ; ADefault : TDateTime{$IFDEF D4UP} = 0{$ENDIF} ) : TDateTime ; virtual ;
        // Read the subnode with AName and convert its value to a double. If the
        // subnode is not found, or cannot be converted, ADefault will be returned.
        function ReadFloat( const AName : string ; ADefault : double{$IFDEF D4UP} = 0.0{$ENDIF} ) : double ; virtual ;
        // Read the subnode with AName and convert its value to an integer. If the
        // subnode is not found, or cannot be converted, ADefault will be returned.
        function ReadInteger( const AName : string ; ADefault : integer{$IFDEF D4UP} = 0{$ENDIF} ) : integer ; virtual ;
        // Read the subnode with AName and return its string value. If the subnode is
        // not found, ADefault will be returned.
        function ReadString( const AName : string ; const ADefault : string{$IFDEF D4UP} = ''{$ENDIF} ) : string ; virtual ;
        // Read the subnode with AName and return its widestring value. If the subnode is
        // not found, ADefault will be returned.
        function ReadWidestring( const AName : string ; const ADefault : widestring{$IFDEF D4UP} = ''{$ENDIF} ) : widestring ; virtual ;
        // Use ToAnsiString to convert any string from the node (name, value, attributes)
        // to a normal ANSI string. If the TsdXmlDocument property UtfEncoded is True,
        // you may loose characters with codes higher than $FF. To prevent this, use
        // widestrings in your application and use ToWidestring instead.
        function ToAnsiString( const s : string ) : string ;
        // Use ToWidestring to convert any string from the node (name, value, attributes)
        // to a widestring. If the TsdXmlDocument property UtfEncoded is True, all
        // character codes higher than $FF are preserved.
        function ToWidestring( const s : string ) : widestring ;
        // Convert the node's value to boolean and return the result. If this conversion
        // fails, or no value is found, then the function returns ADefault.
        function ValueAsBoolDef( ADefault : boolean ) : boolean ; virtual ;
        // Convert the node's value to a TDateTime and return the result. If this conversion
        // fails, or no value is found, then the function returns ADefault.
        function ValueAsDateTimeDef( ADefault : TDateTime ) : TDateTime ; virtual ;
        // Convert the node's value to a double and return the result. If this conversion
        // fails, or no value is found, then the function returns ADefault.
        function ValueAsFloatDef( ADefault : double ) : double ; virtual ;
        // Convert the node's value to integer and return the result. If this conversion
        // fails, or no value is found, then the function returns ADefault.
        function ValueAsIntegerDef( ADefault : integer ) : integer ; virtual ;
        // If the attribute with name AName exists, then set its value to the integer
        // AValue. If it does not exist, then create a new attribute AName with the
        // integer value converted to a quoted string. If ADefault = AValue, and
        // WriteOnDefault = False, no attribute will be added.
        procedure WriteAttributeInteger( const AName : string ; AValue : integer ; ADefault : integer{$IFDEF D4UP} = 0{$ENDIF} ) ; virtual ;
        // If the attribute with name AName exists, then set its value to the string
        // AValue. If it does not exist, then create a new attribute AName with the
        // value AValue. If ADefault = AValue, and WriteOnDefault = False, no attribute
        // will be added.
        procedure WriteAttributeString( const AName : string ; const AValue : string ; const ADefault : string{$IFDEF D4UP} = ''{$ENDIF} ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the boolean
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteBool( const AName : string ; AValue : boolean ; ADefault : boolean{$IFDEF D4UP} = False{$ENDIF} ) ; virtual ;
{$IFDEF USEGRAPHICS}
        // Write properties Color and Style of the TBrush object ABrush to the subnode
        // with AName. If AName does not exist, it will be created.
        procedure WriteBrush( const AName : string ; ABrush : TBrush ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the TColor
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteColor( const AName : string ; AValue : TColor ; ADefault : TColor{$IFDEF D4UP} = clBlack{$ENDIF} ) ; virtual ;
        // Write properties \Name, Color, Size and Style of the TFont object AFont to
        // the subnode with AName. If AName does not exist, it will be created.
        procedure WriteFont( const AName : string ; AFont : TFont ) ; virtual ;
        // Write properties Color, Mode, Style and Width of the TPen object APen to
        // the subnode with AName. If AName does not exist, it will be created.
        procedure WritePen( const AName : string ; APen : TPen ) ; virtual ;
{$ENDIF}
        // Add or replace the subnode with AName and set its value to represent the TDateTime
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        // The XML format used is compliant with W3C's specification of date and time.
        procedure WriteDateTime( const AName : string ; AValue : TDateTime ; ADefault : TDateTime{$IFDEF D4UP} = 0{$ENDIF} ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the double
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteFloat( const AName : string ; AValue : double ; ADefault : double{$IFDEF D4UP} = 0.0{$ENDIF} ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the hexadecimal representation of
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteHex( const AName : string ; AValue : integer ; Digits : integer ; ADefault : integer{$IFDEF D4UP} = 0{$ENDIF} ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the integer
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteInteger( const AName : string ; AValue : integer ; ADefault : integer{$IFDEF D4UP} = 0{$ENDIF} ) ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the string
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteString( const AName, AValue : string ; const ADefault : string{$IFDEF D4UP} = ''{$ENDIF} ) ; virtual ;
        // Call WriteToString to save the XML node to a string. This method can be used to store
        // individual nodes instead of the complete XML document.
        function WriteToString : string ; virtual ;
        // Add or replace the subnode with AName and set its value to represent the widestring
        // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
        procedure WriteWidestring( const AName : string ; const AValue : widestring ; const ADefault : widestring{$IFDEF D4UP} = ''{$ENDIF} ) ; virtual ;
        // Returns the number of attributes in the current node.
        property AttributeCount : integer read GetAttributeCount ;
        // Read this property to get the name of the attribute at Index. Note that Index
        // is zero-based: Index goes from 0 to AttributeCount - 1
        property AttributeName[ Index : integer ] : string read GetAttributeName write SetAttributeName ;
        // Read this property to get the Attribute \Name and Value pair at index Index.
        // This is a string with \Name and Value separated by a TAB character (#9).
        property AttributePair[ Index : integer ] : string read GetAttributePair ;
        // Read this property to get the string value of the attribute at index Index.
        // Write to it to set the string value.
        property AttributeValue[ Index : integer ] : string read GetAttributeValue write SetAttributeValue ;
        // BinaryEncoding reflects the same value as the BinaryEncoding setting of the parent
        // Document.
        property BinaryEncoding : TBinaryEncodingType read GetBinaryEncoding write SetBinaryEncoding ;
        // Use BinaryString to add/extract binary data in an easy way to/from the node. Internally the
        // data gets stored as Base64-encoded data. Do not use this method for normal textual
        // information, it is better to use ValueAsString in that case (adds less overhead).
        property BinaryString : string read GetBinaryString write SetBinaryString ;
        // This property returns the name and index and all predecessors with underscores
        // to separate, in order to get a unique reference that can be used in filenames.
        property CascadedName : string read GetCascadedName ;
        // Pointer to parent XmlDocument, or Nil if none.
        property Document : TsdXmlDocument read FDocument write FDocument ;
        // ElementType contains the type of element that this node holds.
        property ElementType : TXmlElementType read FElementType write FElementType ;
        // Fullpath will return the complete path of the node from the root, e.g.
        // /Root/SubNode1/SubNode2/ThisNode
        property FullPath : string read GetFullPath ;
        // Read Name to get the name of the element, and write Name to set the name.
        // This is the full name and may include a namespace. (Namespace:Name)
        property Name : string read FName write SetName ;
        // Parent points to the parent node of the current XML node.
        property Parent : TXmlNode read FParent write FParent ;
        // NodeCount is the number of child nodes that this node holds. In order to
        // loop through all child nodes, use a construct like this:
        // <CODE>
        // with MyNode do
        //   for i := 0 to NodeCount - 1 do
        //     with Nodes[i] do
        //     ..processing here
        // </CODE>
        property NodeCount : integer read GetNodeCount ;
        // Use Nodes to access the child nodes of the current XML node by index. Note
        // that the list is zero-based, so Index is valid from 0 to NodeCount - 1.
        property Nodes[ Index : integer ] : TXmlNode read GetNodes ; default ;
        // Tag is an integer value the developer can use in any way. Tag does not get
        // saved to the XML. Tag is often used to point to a GUI element (and is then
        // cast to a pointer).
        property Tag : integer read FTag write FTag ;
        // Read TreeDepth to find out many nested levels there are for the current XML
        // node. Root has a TreeDepth of zero.
        property TreeDepth : integer read GetTreeDepth ;
        // ValueAsString returns the unescaped version of ValueDirect. All neccesary
        // characters in ValueDirect must be escaped (e.g. "&" becomes "&amp;") but
        // ValueAsString returns them in original format. Always use ValueAsString to
        // set the text value of a node, to make sure all neccesary charaters are
        // escaped.
        property ValueAsString : string read GetValueAsString write SetValueAsString ;
        // ValueAsWidestring returns the unescaped version of ValueDirect as a widestring.
        // Always use ValueAsWidestring to set the text value of a node, to make sure all
        // neccesary charaters are escaped. Character codes bigger than $FF are preserved
        // if the document is set to Utf8Encoded.
        property ValueAsWidestring : widestring read GetValueAsWidestring write SetValueAsWidestring ;
        // ValueDirect is the exact text value as was parsed from the stream. If multiple
        // text elements are encountered, they are added to ValueDirect with a CR to
        // separate them.
        property ValueDirect : string read FValue write FValue ;
        // WriteOnDefault reflects the same value as the WriteOnDefault setting of the parent
        // Document.
        property WriteOnDefault : boolean read GetWriteOnDefault ;
    end ;

    // TsdXmlDocument is the XML document holder. Create a TsdXmlDocument and then use
    // methods LoadFromFile, LoadFromStream or ReadFromString to load an XML document
    // into memory. Or start from scratch and use Root.NodeNew to add nodes and
    // eventually SaveToFile and SaveToStream to save the results as an XML document.
    // Use property Xmlformat = xfReadable to ensure that indented (readable) output
    // is produced.
    TsdXmlDocument = class( TPersistent )
    private
        FBinaryEncoding : TBinaryEncodingType ;   // HexChars or SixbitChars
        FCodecStream : TsdCodecStream ;           // Temporary stream used to read encoded files
        FComments : TStrings ;                    // An owned list of comment strings  in the root <!--{comment}-->
        FEncodingString : string ;                // Encoding string (e.g. UTF-8 or UTF-16)
        FExternalEncoding : TStringEncodingType ;
        FExtraNodes : TList ;                     // Extra nodes in the document (e.g. DOCTYPE)
        FRoot : TXmlNode ;                        // Contains the actual XML elements
        FStyleSheetString : string ;              // Use to set/read the stylesheet associated with the XML document: <?xml-stylesheet THISONE?>
        FTempRoot : TXmlNode ;                    // Temporary root for reading from stream
        FUseFullNodes : boolean ;                 // If true, nodes are never written in short notation.
        FUtf8Encoded : boolean ;                  // If true, all internal strings are UTF-8 encoded
        FVersionString : string ;                 // Xml version (usually "1.0")
        FWriteOnDefault : boolean ;               // Set this option to "False" to only write values <> default value (default = true)
        FXmlFormat : TXmlFormatType ;             // xfReadable, xfCompact
        FOnNodeNew : TXmlNodeEvent ;              // Called after a node is added
        FOnNodeLoaded : TXmlNodeEvent ;           // Called after a node is loaded completely
        FOnProgress : TXmlProgressEvent ;         // Called after a node is loaded/saved, with the current position in the file
        FOnUnicodeLoss : TNotifyEvent ;
        FDropCommentsOnParse : boolean ;          // This event is called when there is a warning for unicode conversion loss when reading unicode
        procedure DoNodeNew( Node : TXmlNode ) ;
        procedure DoNodeLoaded( Node : TXmlNode ) ;
        procedure DoUnicodeLoss( Sender : TObject ) ;
        function GetCommentString : string ;
        procedure SetCommentString( const Value : string ) ;
        function GetExtraNodeCount : integer ;
        function GetExtraNodes( Index : integer ) : TXmlNode ;
    protected
        procedure CopyFrom( Source : TsdXmlDocument ) ; virtual ;
        procedure DoProgress( Size : integer ) ;
        function LineFeed : string ; virtual ;
        procedure ReadFromStream( S : TStream ) ; virtual ;
        procedure WriteToStream( S : TStream ) ; virtual ;
        procedure SetDefaults ; virtual ;
    public
        // Create a new XmlDocument which can then be used to read or write XML files.
        // A document that is created with Create must later be freed using Free.
        // Example:
        // <Code>
        // var
        //   ADoc: TsdXmlDocument;
        // begin
        //   ADoc := TsdXmlDocument.Create;
        //   try
        //     ADoc.LoadFromFile('c:\\temp\\myxml.xml');
        //     {do something with the document here}
        //   finally
        //     ADoc.Free;
        //   end;
        // end;
        // </Code>
        constructor Create ; virtual ;
        // Use CreateName to Create a new Xml Document that will automatically
        // contain a root element with name ARootName.
        constructor CreateName( const ARootName : string ) ; virtual ;
        // Destroy will free all data in the TsdXmlDocument object. This includes the
        // root node and all subnodes under it. Do not call Destroy directly, call
        // Free instead.
        destructor Destroy ; override ;
        // When calling Assign with a Source object that is a TsdXmlDocument, will cause
        // it to copy all data from Source.
        procedure Assign( Source : TPersistent ) ; override ;
        // Call Clear to remove all data from the object, and restore all defaults.
        procedure Clear ; virtual ;
        // Clear the list with extra nodes. ExtraNodes is a list of nodes like
        // <!DOCTYPE...> etcetera.
        procedure ExtraNodesClear ; virtual ;
        // Function IsEmpty returns true if the root is clear, or in other words, the
        // root contains no value, no name, no subnodes and no attributes.
        function IsEmpty : boolean ; virtual ;
        // Load an XML document from the TStream object in Stream. The LoadFromStream
        // procedure will raise an exception of type EFilerError when it encounters
        // non-wellformed XML. This method can be used with any TStream descendant.
        // See also LoadFromFile and ReadFromString.
        procedure LoadFromStream( Stream : TStream ) ; virtual ;
        // Call procedure LoadFromFile to load an XML document from the filename
        // specified. See Create for an example. The LoadFromFile procedure will raise
        // an exception of type EFilerError when it encounters non-wellformed XML.
        procedure LoadFromFile( const FileName : string ) ; virtual ;
        // Call procedure ReadFromString to load an XML document from the string AValue.
        // The ReadFromString procedure will raise an exception of type EFilerError
        // when it encounters non-wellformed XML.
        procedure ReadFromString( const AValue : string ) ; virtual ;
        // Call SaveToStream to save the XML document to the Stream. Stream
        // can be any TStream descendant. Set XmlFormat to xfReadable if you want
        // the stream to contain indentations to make the XML more human-readable. This
        // is not the default and also not compliant with the XML specification.
        procedure SaveToStream( Stream : TStream ) ; virtual ;
        // Call SaveToFile to save the XML document to a file with FileName. If the
        // filename exists, it will be overwritten without warning. If the file cannot
        // be created, a standard I/O exception will be generated. Set XmlFormat to
        // xfReadable if you want the file to contain indentations to make the XML
        // more human-readable. This is not the default and also not compliant with
        // the XML specification.
        procedure SaveToFile( const FileName : string ) ; virtual ;
        // Call WriteToString to save the XML document to a string. Set XmlFormat to
        // xfReadable if you want the string to contain indentations to make the XML
        // more human-readable. This is not the default and also not compliant with
        // the XML specification.
        function WriteToString : string ; virtual ;
        // Choose what kind of binary encoding will be used when calling TXmlNode.BufferRead
        // and TXmlNode.BufferWrite. Default value is xbBase64.
        property BinaryEncoding : TBinaryEncodingType read FBinaryEncoding write FBinaryEncoding ;
        // Comments that are found above or below the root element are stored in this
        // stringlist. Add comments to this stringlist to insert multiple comments in
        // the XML document. The comments should not include the <!-- -->, these are added
        // by the component.
        property Comments : TStrings read FComments ;
        // A comment string above the root element \<!--{comment}--\> is stored in ths
        // property. \Assign a comment to this property to add it to the XML document.
        // Use property Comments if there are multiple comments.
        property CommentString : string read GetCommentString write SetCommentString ;
        // Set DropCommentsOnParse if you're not interested in any comment nodes in your object
        // model data. All comments encountered during parsing will simply be skipped and
        // not added as a node with ElementType = xeComment (which is default). Note that
        // when you set this option, you cannot later reconstruct an XML file with the comments
        // back in place.
        property DropCommentsOnParse : boolean read FDropCommentsOnParse write FDropCommentsOnParse ;
        // Encoding string (e.g. UTF-8 or UTF-16). This encoding string is stored in
        // the header.
        // Example: In order to get this header:
        // <?xml version="1.0" encoding="UTF-16" ?>
        // enter this code:
        // <CODE>MyXmlDocument.EncodingString := 'UTF-16';</CODE>
        // When reading a file, EncodingString will contain the encoding used.
        property EncodingString : string read FEncodingString write FEncodingString ;
        // ExternalEncoding defines in which format XML files are saved. Set ExternalEncoding
        // to se8bit to save as plain text files, to seUtf8 to save as UTF8 files (with
        // Byte Order Mark #EF BB FF) and to seUTF16LE to save as unicode (Byte Order
        // Mark #FF FE). When reading an XML file, the value of ExternalEncoding will
        // be set according to the byte order mark and/or encoding declaration found.
        property ExternalEncoding : TStringEncodingType read FExternalEncoding write FExternalEncoding ;
        // ExtraNodes is a list of nodes like <!DOCTYPE...>, all <?..?> and all
        // <!...> nodes.
        property ExtraNodes[ Index : integer ] : TXmlNode read GetExtraNodes ;
        // ExtraNodeCount returns the number of nodes in the ExtraNodes array.
        property ExtraNodeCount : integer read GetExtraNodeCount ;
        // Root is the topmost element in the XML document. Access Root to read any
        // child elements. When creating a new XML document, you can automatically
        // include a Root node, by creating using CreateName.
        property Root : TXmlNode read FRoot write FRoot ;
        // Set or get the stylesheet string used for this XML document.
        property StyleSheetString : string read FStyleSheetString write FStyleSheetString ;
        // Set UseFullNodes to True before saving the XML document to ensure that all
        // nodes are represented by <Node>...</Node> instead of the short version
        // <Node/>. UseFullNodes is False by default.
        property UseFullNodes : boolean read FUseFullNodes write FUseFullNodes ;
        // When Utf8Encoded is True, all strings inside the document represent
        // UTF-8 encoded strings. Use function ToWidestring to convert strings to
        // widestring (without loss) or ToAnsiString to convert to ANSI string
        // (with loss). When Utf8Encoded is False (default), all strings represent
        // normal ANSI strings. Set Utf8Encoded to True before adding info to the XML
        // file to ensure internal strings are all UTF-8. Use methods FromWidestring,
        // sdAnsiToUTF8 or sdUnicodeToUtf8 before setting any strings in that case.
        property Utf8Encoded : boolean read FUtf8Encoded write FUtf8Encoded ;
        // After reading, this property contains the XML version (usually "1.0").
        property VersionString : string read FVersionString write FVersionString ;
        // Set WriteOnDefault to False if you do not want to write default values to
        // the XML document. This option can avoid creating huge documents with
        // redundant info, and will speed up writing.
        property WriteOnDefault : boolean read FWriteOnDefault write FWriteOnDefault ;
        // XmlFormat by default is set to xfCompact. This setting is compliant to the spec,
        // and XmlDocument will only generate XML files with #$0A as control character.
        // By setting XmlFormat to xfReadable, you can generate easily readable XML
        // files that contain indentation and carriage returns after each element.
        property XmlFormat : TXmlFormatType read FXmlFormat write FXmlFormat ;
        // This event is called whenever the parser has encountered a new node.
        property OnNodeNew : TXmlNodeEvent read FOnNodeNew write FOnNodeNew ;
        // This event is called when the parser has finished parsing the node, and
        // has created its complete contents in memory.
        property OnNodeLoaded : TXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded ;
        // OnProgress is called during loading and saving of the XML document. The
        // Size parameter contains the position in the stream. This event can be used
        // to implement a progress indicator during loading and saving. The event is
        // called after each node that is read or written.
        property OnProgress : TXmlProgressEvent read FOnProgress write FOnProgress ;
        // This event is called if there is a warning for unicode conversion loss,
        // when reading from Unicode streams or files.
        property OnUnicodeLoss : TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss ;
    end ;

    // This enumeration defines the conversion stream access mode.
    TsdStreamModeType = (
        umUnknown,                                // The stream access mode is not yet known
        umRead,                                   // UTF stream opened for reading
        umWrite                                   // UTF stream opened for writing
        ) ;

    // TsdBufferedReadStream is a buffered stream that takes another TStream
    // and reads only buffer-wise from it, and reads to the stream are first
    // done from the buffer. This stream type can only support reading.
    TsdBufferedReadStream = class( TStream )
    private
        FStream : TStream ;
        FBuffer : PByteArray ;
        FPage : integer ;
        FBufPos : integer ;
        FBufSize : integer ;
        FPosition : longint ;
        FOwned : boolean ;
        FMustCheck : boolean ;
    protected
        procedure CheckPosition ;
    public
        // Create the buffered reader stream by passing the source stream in AStream,
        // this source stream must already be initialized. If Owned is set to True,
        // the source stream will be freed by TsdBufferedReadStream.
        constructor Create( AStream : TStream ; Owned : boolean{$IFDEF D4UP} = False{$ENDIF} ) ;
        destructor Destroy ; override ;
        function Read( var Buffer ; Count : Longint ) : Longint ; override ;
        function Write( const Buffer ; Count : Longint ) : Longint ; override ;
        function Seek( Offset : Longint ; Origin : Word ) : Longint ; override ;
    end ;

    // TsdBufferedWriteStream is a buffered stream that takes another TStream
    // and writes only buffer-wise to it, and writes to the stream are first
    // done to the buffer. This stream type can only support writing.
    TsdBufferedWriteStream = class( TStream )
    private
        FStream : TStream ;
        FBuffer : PByteArray ;
        FBufPos : integer ;
        FPosition : longint ;
        FOwned : boolean ;
    protected
        procedure Flush ;
    public
        // Create the buffered writer stream by passing the destination stream in AStream,
        // this destination stream must already be initialized. If Owned is set to True,
        // the destination stream will be freed by TsdBufferedWriteStream.
        constructor Create( AStream : TStream ; Owned : boolean{$IFDEF D4UP} = False{$ENDIF} ) ;
        destructor Destroy ; override ;
        function Read( var Buffer ; Count : Longint ) : Longint ; override ;
        function Write( const Buffer ; Count : Longint ) : Longint ; override ;
        function Seek( Offset : Longint ; Origin : Word ) : Longint ; override ;
    end ;

    // TsdCodecStream is the base codec class for reading and writing encoded files.
    // See TsdAnsiStream and TsdUtf8Stream for more information.
    TsdCodecStream = class( TStream )
    private
        FBuffer : string ;                        // Buffer that holds temporary utf8 characters
        FBufferPos : integer ;                    // Current character in buffer
        FEncoding : TstringEncodingType ;         // Type of string encoding used for the external stream
        FMode : TsdStreamModeType ;               // Access mode of this UTF stream, determined after first read/write
        FPosMin1 : integer ;                      // Position for seek(-1)
        FPosMin2 : integer ;                      // Position for seek(-2)
        FStream : TStream ;                       // Referenced stream
        FSwapByteOrder : boolean ;
        FWarningUnicodeLoss : boolean ;           // There was a warning for a unicode conversion loss
        FWriteBom : boolean ;
        FOnUnicodeLoss : TNotifyEvent ;           // This event is called if there is a warning for unicode conversion loss
    protected
        function ReadByte : byte ; virtual ;
        procedure StorePrevPositions ; virtual ;
        procedure WriteByte( const B : byte ) ; virtual ;
        procedure WriteBuf( const Buffer ; Count : longint ) ; virtual ;
    public
        // Call Create to create a new TsdCodectream based on an input or output stream
        // in AStream. After the first Read, the input streamtype will be determined,
        // and the Encoding property will be set accordingly. When using Write to
        // write data to the referenced stream, the Encoding property must be set prior
        // to this, indicating what kind of stream to produce.
        constructor Create( AStream : TStream ) ; virtual ;
        // Read Count bytes from the referenced stream, and put them in Buffer. The function
        // returns the actual number of bytes read. The codec stream can only read
        // one byte at the time!
        function Read( var Buffer ; Count : Longint ) : Longint ; override ;
        // Seek to a new position in the stream, with Origin as a reference. The codec
        // stream can not seek when writing, and when reading can only go back one
        // character, or return a position. Position returned is the position
        // in the referenced stream.
        function Seek( Offset : Longint ; Origin : Word ) : Longint ; override ;
        // Write Count bytes from Buffer to the referenced stream, The function
        // returns the actual number of bytes written.
        function Write( const Buffer ; Count : Longint ) : Longint ; override ;
        // Set Encoding when writing to the preferred encoding of the output stream,
        // or read Encoding after reading the output stream to determine encoding type.
        property Encoding : TstringEncodingType read FEncoding write FEncoding ;
        // Read this value after loading an XML file. It will be True if there was a
        // warning for a unicode conversion loss.
        property WarningUnicodeLoss : boolean read FWarningUnicodeLoss ;
        // This event is called if there is a warning for unicode conversion loss.
        property OnUnicodeLoss : TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss ;
    end ;

    // TsdAnsiStream is a conversion stream that will load ANSI, UTF8 or
    // Unicode files and convert them into ANSI only. The stream can
    // also save ANSI data as UTF8 or Unicode. When there is a conversion
    // problem, the conversion routine gives proper warnings through
    // WarningUnicodeLoss and OnUnicodeLoss.
    TsdAnsiStream = class( TsdCodecStream )
    protected
        function ReadByte : byte ; override ;
        procedure WriteByte( const B : byte ) ; override ;
        procedure WriteBuf( const Buffer ; Count : longint ) ; override ;
    end ;

    // TsdUdf8tream is a conversion stream that will load ANSI, UTF8 or
    // Unicode files and convert them into UTF8 only. The stream can
    // also save UTF8 data as Ansi, UTF8 or Unicode.
    TsdUtf8Stream = class( TsdCodecStream )
    private
    protected
        function ReadByte : byte ; override ;
        procedure WriteByte( const B : byte ) ; override ;
        procedure WriteBuf( const Buffer ; Count : longint ) ; override ;
    end ;

    // string functions

    // Escape all required characters in string AValue.
function EscapeString( const AValue : string ) : string ;

// Replace all escaped characters in string AValue by their original. This includes
// character references using &#...; and &#x...;
function UnEscapeStringUTF8( const AValue : string ) : string ;

// Replace all escaped characters in string AValue by their original. This includes
// character references using &#...; and &#x...;, however, character codes above
// 255 are not replaced.
function UnEscapeStringANSI( const AValue : string ) : string ;

// Enclose the string AValue in quotes.
function QuoteString( const AValue : string ) : string ;
// Remove the quotes from string AValue.
function UnQuoteString( const AValue : string ) : string ;

// This function adds control characters Chars repeatedly after each Interval
// of characters to string Value.
function AddControlChars( const AValue : string ; const Chars : string ; Interval : integer ) : string ;

// This function removes control characters from string AValue (Tab, CR, LF and Space)
function RemoveControlChars( const AValue : string ) : string ;

// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
function sdDateTimeFromStringDefault( const ADate : string ; ADefault : TDateTime ) : TDateTime ;

// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
function sdDateTimeToString( ADate : TDateTime ) : string ;

// Conversion between Ansi, UTF8 and Unicode

// Convert a widestring to a UTF8 encoded string
function sdUnicodeToUtf8( const W : widestring ) : string ;

// Convert a normal ansi string to a UTF8 encoded string
function sdAnsiToUtf8( const S : string ) : string ;

// Convert a UTF8 encoded string to a widestring
function sdUtf8ToUnicode( const S : string ) : widestring ;

// Convert a UTF8 encoded string to a normal ansi string
function sdUtf8ToAnsi( const S : string ) : string ;

// parse functions

// Find SubString within string S, only process characters between Start and Close.
// First occurrance is reported in APos. If something is found, function returns True.
function FindString( const SubString : string ; const S : string ; Start, Close : integer ; var APos : integer ) : boolean ;

// Detect if the SubString matches the characters in S from position Start. S may be
// actually longer than SubString, only length(SubString) characters are checked.
function MatchString( const SubString : string ; const S : string ; Start : integer ) : boolean ;

// Find all Name="Value" pairs in string AValue (from Start to Close - 1), and put
// the resulting attributes in stringlist Attributes. This stringlist must already
// be initialized when calling this function.
procedure ParseAttributes( const AValue : string ; Start, Close : integer ; Attributes : TStrings ) ;

// Trim the string AValue between Start and Close - 1 (remove whitespaces at start
// and end), not by adapting the string but by adjusting the Start and Close indices.
// If the resulting string still has a length > 0, the function returns True.
function TrimPos( const AValue : string ; var Start, Close : integer ) : boolean ;

{$IFDEF D4UP}
resourcestring
{$ELSE}
const
{$ENDIF}

    sxeCannotDetermineStreamLength = 'Cannot determine stream length' ;
    sxeErrorCalcStreamLength = 'Error while calculating streamlength' ;
    sxeMissingDataInBinaryStream = 'Missing data in binary stream' ;
    sxeMissingElementName = 'Missing element name' ;
    sxeMissingCloseTag = 'Missing close tag in element %s' ;
    sxeMissingDataAfterGreaterThan = 'Missing data after "<" in element %s' ;
    sxeMissingLessThanInCloseTag = 'Missing ">" in close tag of element %s' ;
    sxeIncorrectCloseTag = 'Incorrect close tag in element %s' ;
    sxeIllegalCharInNodeName = 'Illegal character in node name "%s"' ;
    sxeMoreThanOneRootElement = 'More than one root element found in xml' ;
    sxeNoRootElement = 'No root element found in xml' ;
    sxeMultiStylesheetsNotSupp = 'Multiple stylesheets in root not supported' ;
    sxeCDATAInRoot  = 'No CDATA allowed in root' ;
    sxeRootElementNotDefined = 'XML root element not defined.' ;
    sxeCodecStreamNotAssigned = 'Encoding stream unassigned' ;
    sxeUnsupportedEncoding = 'Unsupported string encoding' ;
    sxeCannotReadCodecForWriting = 'Cannot read from a conversion stream opened for writing' ;
    sxeCannotWriteCodecForReading = 'Cannot write to an UTF stream opened for reading' ;
    sxeCannotReadMultipeChar = 'Cannot read multiple chars from conversion stream at once' ;
    sxeCannotPerformSeek = 'Cannot perform seek on codec stream' ;
    sxeCannotSeekBeforeReadWrite = 'Cannot seek before reading or writing in conversion stream' ;
    sxeCannotSeek   = 'Cannot perform seek in conversion stream' ;
    sxeCannotWriteToOutputStream = 'Cannot write to output stream' ;
    sxeXmlNodeNotAssigned = 'XML Node is not assigned' ;

implementation

uses
{$IFDEF TRIALXML}
    Dialogs,
{$ENDIF}
    Math ;

type

    // Internal type
    TTagType = record
        FStart : string ;
        FClose : string ;
        FStyle : TXmlElementType ;
    end ;
    PByte = ^byte ;

    TBomInfo = packed record
        BOM : array[ 0..3 ] of char ;
        Len : integer ;
        Enc : TStringEncodingType ;
        HasBOM : boolean ;
    end ;

const

    // Count of different escape characters
    cEscapeCount    = 5 ;

    // These are characters that must be escaped. Note that "&" is first since
    // when another would be replaced first (eg ">" by "&lt;") this could
    // cause the new "&" in "&lt;" to be replaced by "&amp;";
    cEscapes        : array[ 0..cEscapeCount - 1 ] of string =
        ( '&', '<', '>', '''', '"' ) ;

    // These are the strings that replace the escape strings - in the same order
    cReplaces       : array[ 0..cEscapeCount - 1 ] of string =
        ( '&amp;', '&lt;', '&gt;', '&apos;', '&quot;' ) ;

    cQuoteChars     : set of char = [ '"', '''' ] ;
    cControlChars   : set of char = [ #9, #10, #13, #32 ] ; {Tab, LF, CR, Space}

    // Count of different XML tags
    cTagCount       = 9 ;

    cTags           : array[ 0..cTagCount - 1 ] of TTagType = (
        // The order is important here; the items are searched for in appearing order
        ( FStart : '<![CDATA[' ; FClose : ']]>' ; FStyle : xeCData ),
        ( FStart : '<!DOCTYPE' ; FClose : '>' ; FStyle : xeDoctype ),
        ( FStart : '<?xml-stylesheet' ; FClose : '?>' ; FStyle : xeStylesheet ),
        ( FStart : '<?xml' ; FClose : '?>' ; FStyle : xeDeclaration ),
        ( FStart : '<!--' ; FClose : '-->' ; FStyle : xeComment ),
        ( FStart : '<!' ; FClose : '>' ; FStyle : xeExclam ),
        ( FStart : '<?' ; FClose : '?>' ; FStyle : xeQuestion ),
        ( FStart : '</' ; FClose : '>' ; FStyle : xeClosing ),
        ( FStart : '<' ; FClose : '>' ; FStyle : xeNormal ) ) ;
    // direct tags are derived from Normal tags by checking for the />

  // These constant are used when generating hexchars from buffer data
    cHexChar        : array[ 0..15 ] of char = '0123456789ABCDEF' ;
    cHexCharLoCase  : array[ 0..15 ] of char = '0123456789abcdef' ;

    // These characters are used when generating sixbitchars from buffer data
    cSixbitChar     : array[ 0..63 ] of char =
        '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*-' ;
    cBase64Char     : array[ 0..63 ] of char =
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/' ;
    cBase64PadChar  : char = '=' ;

    // The amount of bytes to allocate with each increase of the value buffer
    cNodeValueBuf   = 2048 ;

    // byte order marks for strings
    // Unicode text files should contain $FFFE as first character to identify such a file clearly. Depending on the system
    // where the file was created on this appears either in big endian or little endian style.

const
    cBomInfoCount   = 15 ;
const
    cBomInfo        : array[ 0..cBomInfoCount - 1 ] of TBomInfo =
        ( ( BOM : #$00#$00#$FE#$FF ; Len : 4 ; Enc : seUCS4BE ; HasBOM : true ),
        ( BOM : #$FF#$FE#$00#$00 ; Len : 4 ; Enc : seUCS4LE ; HasBOM : true ),
        ( BOM : #$00#$00#$FF#$FE ; Len : 4 ; Enc : seUCS4_2143 ; HasBOM : true ),
        ( BOM : #$FE#$FF#$00#$00 ; Len : 4 ; Enc : seUCS4_3412 ; HasBOM : true ),
        ( BOM : #$FE#$FF#$00#$00 ; Len : 2 ; Enc : seUTF16BE ; HasBOM : true ),
        ( BOM : #$FF#$FE#$00#$00 ; Len : 2 ; Enc : seUTF16LE ; HasBOM : true ),
        ( BOM : #$EF#$BB#$BF#$00 ; Len : 3 ; Enc : seUTF8 ; HasBOM : true ),
        ( BOM : #$00#$00#$00#$3C ; Len : 4 ; Enc : seUCS4BE ; HasBOM : false ),
        ( BOM : #$3C#$00#$00#$00 ; Len : 4 ; Enc : seUCS4LE ; HasBOM : false ),
        ( BOM : #$00#$00#$3C#$00 ; Len : 4 ; Enc : seUCS4_2143 ; HasBOM : false ),
        ( BOM : #$00#$3C#$00#$00 ; Len : 4 ; Enc : seUCS4_3412 ; HasBOM : false ),
        ( BOM : #$00#$3C#$00#$3F ; Len : 4 ; Enc : seUTF16BE ; HasBOM : false ),
        ( BOM : #$3C#$00#$3F#$00 ; Len : 4 ; Enc : seUTF16LE ; HasBOM : false ),
        ( BOM : #$3C#$3F#$78#$6D ; Len : 4 ; Enc : se8bit ; HasBOM : false ),
        ( BOM : #$4C#$6F#$A7#$94 ; Len : 4 ; Enc : seEBCDIC ; HasBOM : false )
        ) ;

    // Delphi 3 and below stubs
{$IFNDEF D4UP}

function StringReplace( const S, OldPattern, NewPattern : string ;
    Flags : TReplaceFlags ) : string ;
var
    SearchStr, Patt, NewStr : string ;
    Offset          : Integer ;
begin
    if rfIgnoreCase in Flags then begin
        SearchStr := UpperCase( S ) ;
        Patt := UpperCase( OldPattern ) ;
    end
    else begin
        SearchStr := S ;
        Patt := OldPattern ;
    end ;
    NewStr := S ;
    Result := '' ;
    while SearchStr <> '' do begin
        Offset := Pos( Patt, SearchStr ) ;
        if Offset = 0 then begin
            Result := Result + NewStr ;
            Break ;
        end ;
        Result := Result + Copy( NewStr, 1, Offset - 1 ) + NewPattern ;
        NewStr := Copy( NewStr, Offset + Length( OldPattern ), MaxInt ) ;
        if not ( rfReplaceAll in Flags ) then begin
            Result := Result + NewStr ;
            Break ;
        end ;
        SearchStr := Copy( SearchStr, Offset + Length( Patt ), MaxInt ) ;
    end ;
end ;

function Max( A, B : integer ) : integer ;
begin
    if A > B then
        Result := A
    else
        Result := B ;
end ;

function Min( A, B : integer ) : integer ;
begin
    if A < B then
        Result := A
    else
        Result := B ;
end ;
{$ENDIF}

// Delphi 4 stubs
{$IFNDEF D5UP}

function AnsiPos( const Substr, S : string ) : Integer ;
begin
    Result := Pos( Substr, S ) ;
end ;

function AnsiQuotedStr( const S : string ; Quote : Char ) : string ;
var
    P, Src, Dest    : PChar ;
    AddCount        : Integer ;
begin
    AddCount := 0 ;
    P := StrScan( PChar( S ), Quote ) ;
    while P <> nil do begin
        Inc( P ) ;
        Inc( AddCount ) ;
        P := StrScan( P, Quote ) ;
    end ;
    if AddCount = 0 then begin
        Result := Quote + S + Quote ;
        Exit ;
    end ;
    SetLength( Result, Length( S ) + AddCount + 2 ) ;
    Dest := Pointer( Result ) ;
    Dest^ := Quote ;
    Inc( Dest ) ;
    Src := Pointer( S ) ;
    P := StrScan( Src, Quote ) ;
    repeat
        Inc( P ) ;
        Move( Src^, Dest^, P - Src ) ;
        Inc( Dest, P - Src ) ;
        Dest^ := Quote ;
        Inc( Dest ) ;
        Src := P ;
        P := StrScan( Src, Quote ) ;
    until P = nil ;
    P := StrEnd( Src ) ;
    Move( Src^, Dest^, P - Src ) ;
    Inc( Dest, P - Src ) ;
    Dest^ := Quote ;
end ;

function AnsiExtractQuotedStr( var Src : PChar ; Quote : Char ) : string ;
var
    P, Dest         : PChar ;
    DropCount       : Integer ;
begin
    Result := '' ;
    if ( Src = nil ) or ( Src^ <> Quote ) then
        Exit ;
    Inc( Src ) ;
    DropCount := 1 ;
    P := Src ;
    Src := StrScan( Src, Quote ) ;
    while Src <> nil do begin
        Inc( Src ) ;
        if Src^ <> Quote then
            Break ;
        Inc( Src ) ;
        Inc( DropCount ) ;
        Src := StrScan( Src, Quote ) ;
    end ;
    if Src = nil then
        Src := StrEnd( P ) ;
    if ( ( Src - P ) <= 1 ) then
        Exit ;
    if DropCount = 1 then
        SetString( Result, P, Src - P - 1 )
    else begin
        SetLength( Result, Src - P - DropCount ) ;
        Dest := PChar( Result ) ;
        Src := StrScan( P, Quote ) ;
        while Src <> nil do begin
            Inc( Src ) ;
            if Src^ <> Quote then
                Break ;
            Move( P^, Dest^, Src - P ) ;
            Inc( Dest, Src - P ) ;
            Inc( Src ) ;
            P := Src ;
            Src := StrScan( Src, Quote ) ;
        end ;
        if Src = nil then
            Src := StrEnd( P ) ;
        Move( P^, Dest^, Src - P - 1 ) ;
    end ;
end ;

procedure FreeAndNil( var Obj ) ;
var
    P               : TObject ;
begin
    P := TObject( Obj ) ;
    TObject( Obj ) := nil ;
    P.Free ;
end ;
{$ENDIF}

function EscapeString( const AValue : string ) : string ;
var
    i               : integer ;
begin
    Result := AValue ;
    for i := 0 to cEscapeCount - 1 do
        Result := StringReplace( Result, cEscapes[ i ], cReplaces[ i ], [ rfReplaceAll ] ) ;
end ;

function UnEscapeStringUTF8( const AValue : string ) : string ;
var
    SearchStr, Reference, Replace : string ;
    i, Offset, Code : Integer ;
    W               : word ;
begin
    SearchStr := AValue ;
    Result := '' ;
    while SearchStr <> '' do begin
        // find '&'
        Offset := AnsiPos( '&', SearchStr ) ;
        if Offset = 0 then begin
            // Nothing found
            Result := Result + SearchStr ;
            Break ;
        end ;
        Result := Result + Copy( SearchStr, 1, Offset - 1 ) ;
        SearchStr := Copy( SearchStr, Offset, MaxInt ) ;
        // find next ';'
        Offset := AnsiPos( ';', SearchStr ) ;
        if Offset = 0 then begin
            // Error: encountered a '&' but not a ';'.. we will ignore, just return
            // the unmodified value
            Result := Result + SearchStr ;
            Break ;
        end ;
        // Reference
        Reference := copy( SearchStr, 1, Offset ) ;
        SearchStr := Copy( SearchStr, Offset + 1, MaxInt ) ;
        Replace := Reference ;
        // See if it is a character reference
        if copy( Reference, 1, 2 ) = '&#' then begin
            Reference := copy( Reference, 3, length( Reference ) - 3 ) ;
            if length( Reference ) > 0 then begin
                if lowercase( Reference[ 1 ] ) = 'x' then
                    // Hex notation
                    Reference[ 1 ] := '$' ;
                Code := StrToIntDef( Reference, -1 ) ;
                if ( Code >= 0 ) and ( Code < $FFFF ) then begin
                    W := Code ;
{$IFDEF D5UP}
                    Replace := sdUnicodeToUtf8( WideChar( W ) ) ;
{$ELSE}
                    Replace := char( W and $FF ) ;
{$ENDIF}
                end ;
            end ;
        end
        else begin
            // Look up default escapes
            for i := 0 to cEscapeCount - 1 do
                if Reference = cReplaces[ i ] then begin
                    // Replace
                    Replace := cEscapes[ i ] ;
                    Break ;
                end ;
        end ;
        // New result
        Result := Result + Replace ;
    end ;
end ;

function UnEscapeStringANSI( const AValue : string ) : string ;
var
    SearchStr, Reference, Replace : string ;
    i, Offset, Code : Integer ;
    B               : byte ;
begin
    SearchStr := AValue ;
    Result := '' ;
    while SearchStr <> '' do begin
        // find '&'
        Offset := AnsiPos( '&', SearchStr ) ;
        if Offset = 0 then begin
            // Nothing found
            Result := Result + SearchStr ;
            Break ;
        end ;
        Result := Result + Copy( SearchStr, 1, Offset - 1 ) ;
        SearchStr := Copy( SearchStr, Offset, MaxInt ) ;
        // find next ';'
        Offset := AnsiPos( ';', SearchStr ) ;
        if Offset = 0 then begin
            // Error: encountered a '&' but not a ';'.. we will ignore, just return
            // the unmodified value
            Result := Result + SearchStr ;
            Break ;
        end ;
        // Reference
        Reference := copy( SearchStr, 1, Offset ) ;
        SearchStr := Copy( SearchStr, Offset + 1, MaxInt ) ;
        Replace := Reference ;
        // See if it is a character reference
        if copy( Reference, 1, 2 ) = '&#' then begin
            Reference := copy( Reference, 3, length( Reference ) - 3 ) ;
            if length( Reference ) > 0 then begin
                if lowercase( Reference[ 1 ] ) = 'x' then
                    // Hex notation
                    Reference[ 1 ] := '$' ;
                Code := StrToIntDef( Reference, -1 ) ;
                if ( Code >= 0 ) and ( Code < $FF ) then begin
                    B := Code ;
                    Replace := char( B ) ;
                end ;
            end ;
        end
        else begin
            // Look up default escapes
            for i := 0 to cEscapeCount - 1 do
                if Reference = cReplaces[ i ] then begin
                    // Replace
                    Replace := cEscapes[ i ] ;
                    Break ;
                end ;
        end ;
        // New result
        Result := Result + Replace ;
    end ;
end ;

function QuoteString( const AValue : string ) : string ;
var
    AQuoteChar      : char ;
begin
    AQuoteChar := '"' ;
    if Pos( '"', AValue ) > 0 then
        AQuoteChar := '''' ;
    Result := AnsiQuotedStr( AValue, AQuoteChar ) ;
end ;

function UnQuoteString( const AValue : string ) : string ;
var
    P               : PChar ;
begin
    if Length( AValue ) < 2 then begin
        Result := AValue ;
        exit ;
    end ;
    if AValue[ 1 ] in cQuoteChars then begin
        P := @AValue[ 1 ] ;
        Result := AnsiExtractQuotedStr( P, AValue[ 1 ] ) ;
    end ;
end ;

function AddControlChars( const AValue : string ; const Chars : string ; Interval : integer ) : string ;
// Insert Chars in AValue at each Interval chars
var
    i, j, ALength   : integer ;
    // local
    procedure InsertControlChars ;
    var
        k           : integer ;
    begin
        for k := 1 to Length( Chars ) do begin
            Result[ j ] := Chars[ k ] ;
            inc( j ) ;
        end ;
    end ;
    // main
begin
    if ( Length( Chars ) = 0 ) or ( Interval <= 0 ) then begin
        Result := AValue ;
        exit ;
    end ;

    // Calculate length based on original length and total extra length for control chars
    ALength := Length( AValue ) + ( ( Length( AValue ) - 1 ) div Interval + 3 ) * Length( Chars ) ;
    SetLength( Result, ALength ) ;

    // Copy and insert
    j := 1 ;
    for i := 1 to Length( AValue ) do begin
        if ( i mod Interval ) = 1 then begin
            // Insert control chars
            InsertControlChars ;
        end ;
        Result[ j ] := AValue[ i ] ;
        inc( j ) ;
    end ;
    InsertControlChars ;

    // Adjust length
    dec( j ) ;
    if ALength > j then
        SetLength( Result, j ) ;
end ;

function RemoveControlChars( const AValue : string ) : string ;
// Remove control characters from string in AValue
var
    i, j            : integer ;
begin
    Setlength( Result, Length( AValue ) ) ;
    i := 1 ;
    j := 1 ;
    while i <= Length( AValue ) do
        if AValue[ i ] in cControlChars then
            inc( i )
        else begin
            Result[ j ] := AValue[ i ] ;
            inc( i ) ;
            inc( j ) ;
        end ;
    // Adjust length
    if i <> j then
        SetLength( Result, j - 1 ) ;
end ;

function FindString( const SubString : string ; const S : string ; Start, Close : integer ; var APos : integer ) : boolean ;
// Check if the Substring matches the string S in any position in interval Start to Close - 1
// and returns found positon in APos. Result = True if anything is found.
// Note: this funtion is case-insensitive
var
    CharIndex       : integer ;
begin
    Result := False ;
    APos := 0 ;
    for CharIndex := Start to Close - Length( SubString ) do
        if MatchString( SubString, S, CharIndex ) then begin
            APos := CharIndex ;
            Result := True ;
            exit ;
        end ;
end ;

function MatchString( const SubString : string ; const S : string ; Start : integer ) : boolean ;
// Check if the Substring matches the string S at position Start.
// Note: this funtion is case-insensitive
var
    CharIndex       : integer ;
begin
    Result := False ;
    // Check range just in case
    if ( Length( S ) - Start + 1 ) < Length( Substring ) then
        exit ;

    CharIndex := 0 ;
    while CharIndex < Length( SubString ) do
        if Upcase( SubString[ CharIndex + 1 ] ) = Upcase( S[ Start + CharIndex ] ) then
            inc( CharIndex )
        else
            exit ;
    // All chars were the same, so we succeeded
    Result := True ;
end ;

procedure ParseAttributes( const AValue : string ; Start, Close : integer ; Attributes : TStrings ) ;
// Convert the attributes string AValue in [Start, Close - 1] to the attributes stringlist
var
    i               : integer ;
    InQuotes        : boolean ;
    AQuoteChar      : char ;
    AText           : string ;
begin
    InQuotes := False ;
    AQuoteChar := '"' ;
    if not assigned( Attributes ) then
        exit ;
    if not TrimPos( AValue, Start, Close ) then
        exit ;

    // Working copy
    AText := copy( AValue, Start, Close - Start ) ;
    for i := Start to Close - 1 do begin

        // In quotes?
        if InQuotes then begin
            if AValue[ i ] = AQuoteChar then
                InQuotes := False ;
        end
        else begin
            if AValue[ i ] in cQuoteChars then begin
                InQuotes := True ;
                AQuoteChar := AValue[ i ] ;
            end ;
        end ;

        // Replace all spaces by LF when not in quotes
        if not InQuotes then
            if AValue[ i ] in [ #9, ' ' ] then
                AText[ i - Start + 1 ] := #10 ;
    end ;

    // Assign to attributes
    Attributes.Text := AText ;

    // Remove empty lines
    i := Attributes.Count - 1 ;
    while i >= 0 do begin
        Attributes[ i ] := Trim( Attributes[ i ] ) ;
        if Length( Attributes[ i ] ) = 0 then
            Attributes.Delete( i )
        else
            dec( i ) ;
    end ;

    // First-char "=" signs should append to previous
    for i := Attributes.Count - 1 downto 1 do
        if Attributes[ i ][ 1 ] = '=' then begin
            Attributes[ i - 1 ] := Attributes[ i - 1 ] + Attributes[ i ] ;
            Attributes.Delete( i ) ;
        end ;
    // First-char quotes should append to previous
    for i := Attributes.Count - 1 downto 1 do
        if Attributes[ i ][ 1 ] in cQuoteChars then begin
            Attributes[ i - 1 ] := Attributes[ i - 1 ] + Attributes[ i ] ;
            Attributes.Delete( i ) ;
        end ;
end ;

function TrimPos( const AValue : string ; var Start, Close : integer ) : boolean ;
// Trim the string in AValue in [Start, Close - 1] by adjusting Start and Close variables
begin
    // Checks
    Start := Max( 1, Start ) ;
    Close := Min( Length( AValue ) + 1, Close ) ;
    if Close <= Start then begin
        Result := False ;
        exit ;
    end ;

    // Trim left
    while
        ( Start < Close ) and
        ( AValue[ Start ] in cControlChars ) do
        inc( Start ) ;

    // Trim right
    while
        ( Start < Close ) and
        ( AValue[ Close - 1 ] in cControlChars ) do
        dec( Close ) ;

    // Do we have a string left?
    Result := Close > Start ;
end ;

procedure WriteStringToStream( S : TStream ; const AString : string ) ;
var
    ALength         : integer ;
begin
    ALength := length( AString ) ;
    if ALength > 0 then
        S.Write( AString[ 1 ], ALength ) ;
end ;

function ReadOpenTag( S : TStream ) : integer ;
// Try to read the type of open tag from S
var
    AIndex, i       : integer ;
    Found           : boolean ;
    Ch              : char ;
    Candidates      : array[ 0..cTagCount - 1 ] of boolean ;
begin
    Result := cTagCount - 1 ;
    //  Setlength(Candidates, cTagCount);
    for i := 0 to cTagCount - 1 do
        Candidates[ i ] := True ;
    AIndex := 1 ;
    repeat
        Found := False ;
        inc( AIndex ) ;
        S.Read( Ch, 1 ) ;
        for i := cTagCount - 1 downto 0 do
            if Candidates[ i ] and ( length( cTags[ i ].FStart ) >= AIndex ) then begin
                if cTags[ i ].FStart[ AIndex ] = Ch then begin
                    Found := True ;
                    if length( cTags[ i ].FStart ) = AIndex then
                        Result := i ;
                end
                else
                    Candidates[ i ] := False ;
            end ;
    until Found = False ;
    // Decrease position again
    S.Seek( -1, soFromCurrent ) ;
end ;

function ReadStringFromStreamUntil( S : TStream ; const ASearch : string ;
    var AValue : string ) : boolean ;
var
    AIndex          : integer ;
    Ch              : char ;
begin
    AIndex := 1 ;
    AValue := '' ;
    Result := False ;
    while AIndex <= length( ASearch ) do begin
        if S.Read( Ch, 1 ) = 0 then
            exit ;
        AValue := AValue + Ch ;
        if ASearch[ AIndex ] = Ch then
            inc( AIndex )
        else
            AIndex := 1 ;
    end ;
    Result := True ;
    AValue := copy( AValue, 1, length( AValue ) - length( ASearch ) ) ;
end ;

// StrSwapByteOrder was kindly borrowed from Mike Lischke's Unicode unit

procedure StrSwapByteOrder( Str : PWideChar ) ;
// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
asm
         PUSH ESI
         PUSH EDI
         MOV ESI, EAX
         MOV EDI, ESI
         XOR EAX, EAX  // clear high order byte to be able to use 32bit operand below
@@1:     LODSW
         OR EAX, EAX
         JZ @@2
         XCHG AL, AH
         STOSW
         JMP @@1

@@2:     POP EDI
         POP ESI
end ;

function sdDateTimeFromStringDefault( const ADate : string ; ADefault : TDateTime ) : TDateTime ;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
var
    AYear, AMonth, ADay, AHour, AMin, ASec, AMSec : word ;
begin
    try
        AYear := StrToInt( copy( ADate, 1, 4 ) ) ;
        AMonth := StrToInt( copy( ADate, 6, 2 ) ) ;
        ADay := StrToInt( copy( ADate, 9, 2 ) ) ;
        if Length( ADate ) > 16 then begin        // Suggestion JH
            AHour := StrToInt( copy( ADate, 12, 2 ) ) ;
            AMin := StrToInt( copy( ADate, 15, 2 ) ) ;
            ASec := StrToIntDef( copy( ADate, 18, 2 ), 0 ) ; // They might be omitted, so default to 0
            AMSec := StrToIntDef( copy( ADate, 21, 3 ), 0 ) ; // They might be omitted, so default to 0
        end
        else begin
            AHour := 0 ;
            AMin := 0 ;
            ASec := 0 ;
            AMSec := 0 ;
        end ;
        Result :=
            EncodeDate( AYear, AMonth, ADay ) +
            EncodeTime( AHour, AMin, ASec, AMSec ) ;
    except
        Result := ADefault ;
    end ;
end ;

function sdDateTimeToString( ADate : TDateTime ) : string ;
// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
var
    AYear, AMonth, ADay, AHour, AMin, ASec, AMSec : word ;
begin
    DecodeDate( ADate, AYear, AMonth, ADay ) ;
    DecodeTime( ADate, AHour, AMin, ASec, AMSec ) ;
    if frac( ADate ) = 0 then begin
        Result := Format( '%.4d-%.2d-%.2d', [ AYear, AMonth, ADay ] ) ;
    end
    else begin
        Result := Format( '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
            [ AYear, AMonth, ADay, AHour, AMin, ASec, AMSec ] ) ;
    end ;
end ;

function PtrUnicodeToUtf8( Dest : PChar ; MaxDestBytes : Cardinal ; Source : PWideChar ; SourceChars : Cardinal ) : Cardinal ;
var
    i, count        : Cardinal ;
    c               : Cardinal ;
begin
    Result := 0 ;
    if not assigned( Source ) or not assigned( Dest ) then
        exit ;

    count := 0 ;
    i := 0 ;

    while ( i < SourceChars ) and ( count < MaxDestBytes ) do begin
        c := Cardinal( Source[ i ] ) ;
        Inc( i ) ;
        if c <= $7F then begin
            Dest[ count ] := Char( c ) ;
            Inc( count ) ;
        end
        else if c > $7FF then begin
            if count + 3 > MaxDestBytes then
                break ;
            Dest[ count ] := Char( $E0 or ( c shr 12 ) ) ;
            Dest[ count + 1 ] := Char( $80 or ( ( c shr 6 ) and $3F ) ) ;
            Dest[ count + 2 ] := Char( $80 or ( c and $3F ) ) ;
            Inc( count, 3 ) ;
        end
        else begin                                //  $7F < Source[i] <= $7FF
            if count + 2 > MaxDestBytes then
                break ;
            Dest[ count ] := Char( $C0 or ( c shr 6 ) ) ;
            Dest[ count + 1 ] := Char( $80 or ( c and $3F ) ) ;
            Inc( count, 2 ) ;
        end ;
    end ;
    if count >= MaxDestBytes then
        count := MaxDestBytes - 1 ;
    Dest[ count ] := #0 ;
    Result := count + 1 ;                         // convert zero based index to byte count
end ;

function PtrUtf8ToUnicode( Dest : PWideChar ; MaxDestChars : Cardinal ; Source : PChar ;
    SourceBytes : Cardinal ) : Cardinal ;
var
    i, count        : Cardinal ;
    c               : Byte ;
    wc              : Cardinal ;
begin
    if not assigned( Dest ) or not assigned( Source ) then begin
        Result := 0 ;
        Exit ;
    end ;
    Result := Cardinal( -1 ) ;
    count := 0 ;
    i := 0 ;
    while ( i < SourceBytes ) and ( count < MaxDestChars ) do begin
        wc := Cardinal( Source[ i ] ) ;
        Inc( i ) ;
        if ( wc and $80 ) <> 0 then begin
            if i >= SourceBytes then
                // incomplete multibyte char
                Exit ;
            wc := wc and $3F ;
            if ( wc and $20 ) <> 0 then begin
                c := Byte( Source[ i ] ) ;
                Inc( i ) ;
                if ( c and $C0 ) <> $80 then
                    // malformed trail byte or out of range char
                    Exit ;
                if i >= SourceBytes then
                    // incomplete multibyte char
                    Exit ;
                wc := ( wc shl 6 ) or ( c and $3F ) ;
            end ;
            c := Byte( Source[ i ] ) ;
            Inc( i ) ;
            if ( c and $C0 ) <> $80 then
                // malformed trail byte
                Exit ;
            Dest[ count ] := WideChar( ( wc shl 6 ) or ( c and $3F ) ) ;
        end
        else
            Dest[ count ] := WideChar( wc ) ;
        Inc( count ) ;
    end ;

    if count >= MaxDestChars then
        count := MaxDestChars - 1 ;

    Dest[ count ] := #0 ;
    Result := count + 1 ;
end ;

function sdUnicodeToUtf8( const W : widestring ) : string ;
var
    L               : integer ;
    Temp            : string ;
begin
    Result := '' ;
    if W = '' then
        Exit ;
    SetLength( Temp, Length( W ) * 3 ) ;          // SetLength includes space for null terminator

    L := PtrUnicodeToUtf8( PChar( Temp ), Length( Temp ) + 1, PWideChar( W ), Length( W ) ) ;
    if L > 0 then
        SetLength( Temp, L - 1 )
    else
        Temp := '' ;
    Result := Temp ;
end ;

function sdAnsiToUtf8( const S : string ) : string ;
begin
    Result := sdUnicodeToUtf8( S ) ;
end ;

function sdUtf8ToUnicode( const S : string ) : widestring ;
var
    L               : Integer ;
    Temp            : WideString ;
begin
    Result := '' ;
    if S = '' then
        Exit ;
    SetLength( Temp, Length( S ) ) ;

    L := PtrUtf8ToUnicode( PWideChar( Temp ), Length( Temp ) + 1, PChar( S ), Length( S ) ) ;
    if L > 0 then
        SetLength( Temp, L - 1 )
    else
        Temp := '' ;
    Result := Temp ;
end ;

function sdUtf8ToAnsi( const S : string ) : string ;
begin
    Result := sdUtf8ToUnicode( S ) ;
end ;

{ TXmlNode }

procedure TXmlNode.Assign( Source : TPersistent ) ;
var
    OldFormat       : TXmlFormatType ;
begin
    if Source is TXmlNode then begin
        // Clear first
        Clear ;
        // The mother of all tricks: instead of doing the grunt work, we will simply
        // write source to a string and read ourself from it. A typical example of
        // "Use the source, Luke!". Note: make sure to use xfCompact since otherwise
        // spurious whitespaces may get introduced.
        if assigned( Document ) then begin
            OldFormat := Document.XmlFormat ;
            Document.XmlFormat := xfCompact ;
        end
        else
            OldFormat := xfCompact ;              // avoid compiler warning
        ReadFromString( TXmlNode( Source ).WriteToString ) ;
        if assigned( Document ) then
            Document.XmlFormat := OldFormat ;
    end
    else
        inherited ;
end ;

procedure TXmlNode.AttributeAdd( const AName, AValue : string ) ;
var
    Attr            : string ;
begin
    Attr := Format( '%s=%s', [ AName, QuoteString( EscapeString( AValue ) ) ] ) ;
    FAttributes.Add( Attr ) ;
end ;

{$IFDEF D4UP}

procedure TXmlNode.AttributeAdd( const AName : string ; AValue : integer ) ;
begin
    AttributeAdd( AName, IntToStr( AValue ) ) ;
end ;

{$ENDIF}

function TXmlNode.AttributeByName( const AName : string ) : string ;
var
    i               : integer ;
begin
    Result := '' ;
    for i := 0 to AttributeCount - 1 do
        if AnsiCompareText( AttributeName[ i ], AName ) = 0 then begin
            Result := AttributeValue[ i ] ;
            exit ;
        end ;
end ;

procedure TXmlNode.AttributeDelete( Index : integer ) ;
begin
    if ( Index >= 0 ) and ( Index < AttributeCount ) then
        FAttributes.Delete( Index ) ;
end ;

function TXmlNode.AttributeIndexByname( const AName : string ) : integer ;
// Return the index of the attribute with name AName, or -1 if not found
var
    i               : integer ;
begin
    Result := -1 ;
    for i := 0 to AttributeCount - 1 do
        if AnsiCompareText( AttributeName[ i ], AName ) = 0 then begin
            Result := i ;
            exit ;
        end ;
end ;

procedure TXmlNode.AttributesClear ;
begin
    FAttributes.Clear ;
end ;

function TXmlNode.BufferLength : integer ;
var
    AData           : string ;
    APos            : integer ;
begin
    AData := RemoveControlChars( FValue ) ;
    case BinaryEncoding of
        xbeBinHex : begin
                Result := length( AData ) div 2 ;
                if Result * 2 <> length( AData ) then
                    raise EFilerError.Create( sxeErrorCalcStreamLength ) ;
            end ;
        xbeBase64 : begin
                Result := length( AData ) div 4 ;
                if Result * 4 <> length( AData ) then
                    raise EFilerError.Create( sxeErrorCalcStreamLength ) ;
                Result := Result * 3 ;
                // Check padding chars
                APos := length( AData ) ;
                if ( APos > 0 ) and ( AData[ APos ] = cBase64PadChar ) then begin
                    dec( APos ) ;
                    dec( Result ) ;
                    if ( APos > 0 ) and ( AData[ APos ] = cBase64PadChar ) then
                        dec( Result ) ;
                end ;
            end ;
    else
        raise EFilerError.Create( sxeCannotDetermineStreamLength ) ;
    end ;
end ;

procedure TXmlNode.BufferRead( var Buf ; ASize : integer ) ;
// Read data from XML binhex to the buffer
var
    i, j            : integer ;
    ADataSize       : integer ;
    AData           : string ;
    APos, ACore     : integer ;
    ALong           : cardinal ;
    D               : PByte ;
    Map             : array[ Char ] of byte ;
begin
    AData := RemoveControlChars( FValue ) ;
    case BinaryEncoding of
        xbeBinHex : begin
                ADataSize := Min( Length( AData ) div 2, ASize ) ;
                if ADataSize < ASize then
                    raise EFilerError.Create( sxeMissingDataInBinaryStream ) ;

                // Prepare map (from ASCII to hex value)
                Fillchar( Map, SizeOf( Map ), 0 ) ;
                for i := 0 to 15 do begin
                    Map[ cHexChar[ i ] ] := i ;
                    Map[ cHexCharLoCase[ i ] ] := i ;
                end ;

                // Data from binhex to buffer
                for i := 0 to ADataSize - 1 do
                    TByteArray( Buf )[ i ] := Map[ AData[ i * 2 + 1 ] ] * 16 + Map[ AData[ i * 2 + 2 ] ] ;
            end ;
        xbeSixBit :
            {// This method is here for backwards compatibility! It will be removed in v3.0}begin
                // Core * 4 is the number of chars to read - check length
                ACore := Length( AData ) div 4 ;
                if ASize > ACore * 3 then
                    raise EFilerError.Create( sxeMissingDataInBinaryStream ) ;

                // Prepare map
                for i := 0 to 63 do
                    Map[ cSixBitChar[ i ] ] := i ;
                D := @Buf ;

                // Do this ACore times
                for i := 0 to ACore - 1 do begin
                    ALong := 0 ;
                    // Unroll the characters
                    for j := 0 to 3 do
                        ALong := ALong shl 6 + Map[ AData[ i * 4 + 4 - j ] ] ;
                    // and unroll the bytes
                    for j := 2 downto 0 do begin
                        // Check overshoot
                        if integer( D ) - integer( @Buf ) >= ASize then
                            exit ;
                        D^ := ALong shr ( j * 8 ) and $FF ;
                        inc( D ) ;
                    end ;
                end ;
            end ;
        xbeBase64 : begin
                // Core * 4 is the number of chars to read - check length
                ACore := Length( AData ) div 4 ;
                if ASize > ACore * 3 then
                    raise EFilerError.Create( sxeMissingDataInBinaryStream ) ;

                // Prepare map
                for i := 0 to 63 do
                    Map[ cBase64Char[ i ] ] := i ;
                D := @Buf ;

                // Check for final padding, and replace with "zeros". There can be
                // at max two pad chars ('=')
                APos := length( AData ) ;
                if ( APos > 0 ) and ( AData[ APos ] = cBase64PadChar ) then begin
                    AData[ APos ] := cBase64Char[ 0 ] ;
                    dec( APos ) ;
                    if ( APos > 0 ) and ( AData[ APos ] = cBase64PadChar ) then
                        AData[ APos ] := cBase64Char[ 0 ] ;
                end ;

                // Do this ACore times
                for i := 0 to ACore - 1 do begin
                    ALong := 0 ;
                    // Unroll the characters
                    for j := 0 to 3 do
                        ALong := ALong shl 6 + Map[ AData[ i * 4 + j + 1 ] ] ;
                    // and unroll the bytes
                    for j := 2 downto 0 do begin
                        // Check overshoot
                        if integer( D ) - integer( @Buf ) >= ASize then
                            exit ;
                        D^ := ALong shr ( j * 8 ) and $FF ;
                        inc( D ) ;
                    end ;
                end ;
            end ;
    end ;                                         //case
end ;

procedure TXmlNode.BufferWrite( const Buf ; ASize : integer ) ;
// Write data from the buffer to XML in binhex format
var
    AData           : string ;
    i, j            : integer ;
    ACore           : integer ;
    ALong           : cardinal ;
    S               : PByte ;
begin
    case BinaryEncoding of
        xbeBinHex : begin
                // Data length
                SetLength( AData, ASize * 2 ) ;
                // Data to binhex buffer
                for i := 0 to ASize - 1 do begin
                    AData[ i * 2 + 1 ] := cHexChar[ TByteArray( Buf )[ i ] shr 4 ] ;
                    AData[ i * 2 + 2 ] := cHexChar[ TByteArray( Buf )[ i ] and $0F ] ;
                end ;
            end ;
        xbeBase64 : begin
                // Make sure ASize is always a multiple of 3, and this multiple
                // gets saved as 4 characters
                ACore := ( ASize + 2 ) div 3 ;

                // Set the length of the string that stores encoded characters
                SetLength( AData, ACore * 4 ) ;
                S := @Buf ;
                // Do the loop ACore times
                for i := 0 to ACore - 1 do begin
                    ALong := 0 ;
                    for j := 0 to 2 do begin
                        ALong := ALong shl 8 + S^ ;
                        inc( S ) ;
                    end ;
                    for j := 0 to 3 do begin
                        AData[ i * 4 + 4 - j ] := cBase64Char[ ALong and $3F ] ;
                        ALong := ALong shr 6 ;
                    end ;
                end ;
                // For comformity to Base64, we must pad the data instead of zero out
                // if the size is not an exact multiple of 3
                case ACore * 3 - ASize of
                    0 : ;                         // nothing to do
                    1 :                           // pad one byte
                        AData[ ACore * 4 ] := cBase64PadChar ;
                    2 :                           {// pad two bytes} begin
                            AData[ ACore * 4 ] := cBase64PadChar ;
                            AData[ ACore * 4 - 1 ] := cBase64PadChar ;
                        end ;
                end ;                             //case
            end ;
    end ;

    // For comformity with Base64, we must add linebreaks each 76 characters
    FValue := AddControlChars( AData, GetLineFeed + GetIndent, 76 ) ;
end ;

procedure TXmlNode.Clear ;
begin
    // Name + value
    FName := '' ;
    FValue := '' ;
    // Clear attributes and nodes
    AttributesClear ;
    NodesClear ;
end ;

constructor TXmlNode.Create( ADocument : TsdXmlDocument ) ;
begin
    inherited Create ;
    FDocument := ADocument ;
    FAttributes := TStringList.Create ;
    FNodes := TList.Create ;
end ;

constructor TXmlNode.CreateName( ADocument : TsdXmlDocument ;
    const AName : string ) ;
begin
    Create( ADocument ) ;
    Name := AName ;
end ;

constructor TXmlNode.CreateNameValue( ADocument : TsdXmlDocument ; const AName,
    AValue : string ) ;
begin
    Create( ADocument ) ;
    Name := AName ;
    ValueAsString := AValue ;
end ;

constructor TXmlNode.CreateType( ADocument : TsdXmlDocument ;
    AType : TXmlElementType ) ;
begin
    Create( ADocument ) ;
    FElementType := AType ;
end ;

procedure TXmlNode.Delete ;
begin
    if assigned( Parent ) then
        Parent.NodeRemove( Self ) ;
end ;

procedure TXmlNode.DeleteEmptyNodes ;
var
    i               : integer ;
    ANode           : TXmlNode ;
begin
    for i := NodeCount - 1 downto 0 do begin
        ANode := Nodes[ i ] ;
        // Recursive call
        ANode.DeleteEmptyNodes ;
        // Check if we should delete child node
        if IsEmpty then
            NodeDelete( i ) ;
    end ;
end ;

destructor TXmlNode.Destroy ;
begin
    NodesClear ;
    FNodes.Free ;
    FNodes := nil ;
    FAttributes.Free ;
    FAttributes := nil ;
    inherited ;
end ;

function TXmlNode.FindNode( const NodeName : string ) : TXmlNode ;
// Find the first node which has name NodeName. Contrary to the NodeByName
// function, this function will search the whole subnode tree, using the
// DepthFirst method.
var
    i               : integer ;
    ANode           : TXmlNode ;
begin
    Result := nil ;
    // Loop through all subnodes
    for i := 0 to NodeCount - 1 do begin
        ANode := Nodes[ i ] ;
        if ( AnsiCompareText( ANode.Name, NodeName ) = 0 ) or
            ( AnsiCompareText( ANode.FullPath, NodeName ) = 0 ) then begin
            // If the subnode has name NodeName then we have a result, exit
            Result := ANode ;
            break ;
        end
        else begin
            // If not, we will search the subtree of this node
            ANode := ANode.FindNode( NodeName ) ;
            if assigned( ANode ) then begin
                Result := ANode ;
                break ;
            end ;
        end ;
    end ;
end ;

procedure TXmlNode.FindNodes( const NodeName : string ; const AList : TList ) ;
// local
    procedure FindNodesRecursive( ANode : TXmlNode ; AList : TList ) ;
    var
        i           : integer ;
    begin
        with ANode do
            for i := 0 to NodeCount - 1 do begin
                if ( AnsiCompareText( Nodes[ i ].Name, NodeName ) = 0 ) then
                    AList.Add( Nodes[ i ] ) ;
                FindNodesRecursive( Nodes[ i ], AList ) ;
            end ;
    end ;
    // main
begin
    AList.Clear ;
    FindNodesRecursive( Self, AList ) ;
end ;

function TXmlNode.FromAnsiString( const s : string ) : string ;
begin
    if Utf8Encoded then
        Result := sdAnsiToUtf8( s )
    else
        Result := s ;
end ;

function TXmlNode.FromWidestring( const W : widestring ) : string ;
begin
    if Utf8Encoded then
        Result := sdUnicodeToUtf8( W )
    else
        Result := W ;
end ;

function TXmlNode.GetAttributeCount : integer ;
begin
    Result := FAttributes.Count ;
end ;

function TXmlNode.GetAttributeName( Index : integer ) : string ;
begin
    if ( Index >= 0 ) and ( Index < AttributeCount ) then
        Result := FAttributes.Names[ Index ] ;
end ;

function TXmlNode.GetAttributePair( Index : integer ) : string ;
begin
    if ( Index >= 0 ) and ( Index < AttributeCount ) then
        Result := FAttributes[ Index ] ;
end ;

function TXmlNode.GetAttributeValue( Index : integer ) : string ;
begin
    if assigned( FAttributes ) then
        with FAttributes do
            Result := UnEscapeString( UnQuoteString( Values[ Names[ Index ] ] ) ) ;
end ;

function TXmlNode.GetBinaryEncoding : TBinaryEncodingType ;
begin
    Result := xbeBinHex ;
    if assigned( Document ) then
        Result := Document.BinaryEncoding ;
end ;

function TXmlNode.GetBinaryString : string ;
// Get the binary contents of this node as Base64 and return it as a string
var
    OldEncoding     : TBinaryEncodingType ;
begin
    // Set to base64
    OldEncoding := BinaryEncoding ;
    try
        BinaryEncoding := xbeBase64 ;
        // Set resulting stringlength
        SetLength( Result, BufferLength ) ;
        // And fill the buffer
        if length( Result ) > 0 then
            BufferRead( Result[ 1 ], length( Result ) ) ;
    finally
        BinaryEncoding := OldEncoding ;
    end ;
end ;

function TXmlNode.GetCascadedName : string ;
// Return the name+index and all predecessors with underscores to separate, in
// order to get a unique reference that can be used in filenames
var
    AName           : string ;
begin
    AName := Format( '%s%.4d', [ Name, StrToIntDef( AttributeByName( 'Index' ), 0 ) ] ) ;
    if assigned( Parent ) then
        Result := Format( '%s_%s', [ Parent.CascadedName, AName ] )
    else
        Result := AName ;
end ;

function TXmlNode.GetFullPath : string ;
// GetFullpath will return the complete path of the node from the root, e.g.
// /Root/SubNode1/SubNode2/ThisNode
begin
    Result := '/' + Name ;
    if assigned( Parent ) then
        // Recursive call
        Result := Parent.GetFullPath + Result ;
end ;

function TXmlNode.GetIndent : string ;
var
    i               : integer ;
begin
    if assigned( Document ) then
        case Document.XmlFormat of
            xfCompact : Result := '' ;
            xfReadable :
                for i := 0 to TreeDepth - 1 do
                    Result := Result + '  ' ;
        end
    else
        Result := ''
end ;

function TXmlNode.GetLineFeed : string ;
begin
    if assigned( Document ) then
        case Document.XmlFormat of
            xfCompact : Result := '' ;
            xfReadable : Result := #13#10 ;
        else
            Result := #10 ;
        end
    else
        Result := '' ;
end ;

function TXmlNode.GetNodeCount : integer ;
begin
    Result := 0 ;
    if Assigned( FNodes ) then
        Result := FNodes.Count ;
end ;

function TXmlNode.GetNodes( Index : integer ) : TXmlNode ;
begin
    Result := nil ;
    if ( Index >= 0 ) and ( Index < NodeCount ) then
        Result := TXmlNode( FNodes[ Index ] ) ;
end ;

function TXmlNode.GetTreeDepth : integer ;
begin
    Result := 0 ;
    if assigned( Parent ) and assigned( Document ) and ( Parent <> Document.FTempRoot ) then
        Result := Parent.TreeDepth + 1 ;
end ;

function TXmlNode.GetValueAsString : string ;
begin
    Result := UnEscapeString( FValue ) ;
end ;

function TXmlNode.GetValueAsWidestring : widestring ;
begin
    Result := ToWidestring( ValueAsString ) ;
end ;

function TXmlNode.GetWriteOnDefault : boolean ;
begin
    Result := True ;
    if assigned( Document ) then
        Result := Document.WriteOnDefault ;
end ;

function TXmlNode.HasAttribute( const AName : string ) : boolean ;
var
    i               : integer ;
begin
    Result := False ;
    for i := 0 to AttributeCount - 1 do
        if AnsiCompareText( AName, AttributeName[ i ] ) = 0 then begin
            Result := True ;
            exit ;
        end ;
end ;

function TXmlNode.IndexInParent : integer ;
// Retrieve our index in the parent's nodelist
var
    i               : integer ;
begin
    Result := -1 ;
    if assigned( Parent ) then
        for i := 0 to Parent.NodeCount - 1 do
            if Self = Parent.Nodes[ i ] then begin
                Result := i ;
                exit ;
            end ;
end ;

function TXmlNode.IsClear : boolean ;
begin
    Result := ( Length( FName ) = 0 ) and IsEmpty ;
end ;

function TXmlNode.IsEmpty : boolean ;
begin
    Result := ( Length( FValue ) = 0 ) and ( NodeCount = 0 ) and ( AttributeCount = 0 ) ;
end ;

function TXmlNode.NodeAdd( ANode : TXmlNode ) : integer ;
begin
    Result := -1 ;
    if assigned( ANode ) and assigned( FNodes ) then begin
        ANode.Parent := Self ;
        Result := FNodes.Add( ANode ) ;
    end ;
end ;

function TXmlNode.NodeByAttributeValue( const NodeName, AttribName, AttribValue : string ;
    ShouldRecurse : boolean ) : TXmlNode ;
// This function returns a pointer to the first subnode that has an attribute with
// name AttribName and value AttribValue.
var
    i               : integer ;
    ANode           : TXmlNode ;
begin
    Result := nil ;
    // Find all nodes that are potential results
    for i := 0 to NodeCount - 1 do begin
        ANode := Nodes[ i ] ;
        if ( AnsiCompareText( ANode.Name, NodeName ) = 0 ) and
            ANode.HasAttribute( AttribName ) and
            ( AnsiCompareText( ANode.AttributeByName( AttribName ), AttribValue ) = 0 ) then begin
            Result := ANode ;
            exit ;
        end ;
        // Recursive call
        if ShouldRecurse then
            Result := ANode.NodeByAttributeValue( NodeName, AttribName, AttribValue, True ) ;
        if assigned( Result ) then
            exit ;
    end ;
end ;

function TXmlNode.NodeByName( const AName : string ) : TXmlNode ;
var
    i               : integer ;
begin
    Result := nil ;
    for i := 0 to NodeCount - 1 do
        if AnsiCompareText( Nodes[ i ].Name, AName ) = 0 then begin
            Result := Nodes[ i ] ;
            exit ;
        end ;
end ;

procedure TXmlNode.NodeDelete( Index : integer ) ;
begin
    if ( Index >= 0 ) and ( Index < NodeCount ) then begin
        TXmlNode( FNodes[ Index ] ).Free ;
        FNodes.Delete( Index ) ;
    end ;
end ;

procedure TXmlNode.NodeExchange( Index1, Index2 : integer ) ;
begin
    if ( Index1 >= 0 ) and ( Index1 < Nodecount ) and
        ( Index2 >= 0 ) and ( Index2 < Nodecount ) then
        FNodes.Exchange( Index1, Index2 ) ;
end ;

function TXmlNode.NodeExtract( ANode : TXmlNode ) : TXmlNode ;
var
    AIndex          : integer ;
begin
    //  Result := TXmlNode(FNodes.Extract(ANode));
      // Compatibility with Delphi4
    Result := nil ;
    AIndex := FNodes.IndexOf( ANode ) ;
    if AIndex >= 0 then begin
        Result := ANode ;
        FNodes.Delete( AIndex ) ;
    end ;
end ;

function TXmlNode.NodeIndexByName( const AName : string ) : integer ;
begin
    Result := 0 ;
    while Result < NodeCount do begin
        if AnsiCompareText( Nodes[ Result ].Name, AName ) = 0 then
            exit ;
        inc( Result ) ;
    end ;
    if Result = NodeCount then
        Result := -1 ;
end ;

function TXmlNode.NodeIndexByNameFrom( const AName : string ;
    AFrom : integer ) : integer ;
begin
    Result := AFrom ;
    while Result < NodeCount do begin
        if AnsiCompareText( Nodes[ Result ].Name, AName ) = 0 then
            exit ;
        inc( Result ) ;
    end ;
    if Result = NodeCount then
        Result := -1 ;
end ;

function TXmlNode.NodeIndexOf( ANode : TXmlNode ) : integer ;
begin
    Result := -1 ;
    if assigned( ANode ) and assigned( FNodes ) then
        Result := FNodes.IndexOf( ANode ) ;
end ;

procedure TXmlNode.NodeInsert( Index : integer ; ANode : TXmlNode ) ;
// Insert the node ANode at location Index in the list.
begin
    if assigned( FNodes ) and assigned( ANode ) and ( Index >= 0 ) and ( Index <= NodeCount ) then begin
        ANode.Parent := Self ;
        FNodes.Insert( Index, ANode ) ;
    end ;
end ;

function TXmlNode.NodeNew( const AName : string ) : TXmlNode ;
// Add a new child node and return its pointer
begin
    Result := Nodes[ NodeAdd( TXmlNode.CreateName( Document, AName ) ) ] ;
end ;

function TXmlNode.NodeNewAtIndex( Index : integer ;
    const AName : string ) : TXmlNode ;
// Create a new node with AName, and insert it into the subnode list at location
// Index, and return a pointer to it.
begin
    Result := nil ;
    if assigned( FNodes ) and ( Index >= 0 ) and ( Index <= NodeCount ) then begin
        Result := TXmlNode.CreateName( Document, AName ) ;
        NodeInsert( Index, Result ) ;
    end ;
end ;

function TXmlNode.NodeRemove( ANode : TxmlNode ) : integer ;
begin
    Result := NodeIndexOf( ANode ) ;
    if Result >= 0 then
        NodeDelete( Result ) ;
end ;

function TXmlNode.NodeReplace( const AName : string ) : TXmlNode ;
// Find the node with AName, and if not found, add new one
begin
    Result := NodeByName( AName ) ;
    if not assigned( Result ) then
        Result := NodeNew( AName ) ;
end ;

procedure TXmlNode.NodesByName( const AName : string ; const AList : TList ) ;
// Fill AList with nodes that have name AName
var
    i               : integer ;
begin
    if not assigned( AList ) then
        exit ;
    AList.Clear ;
    for i := 0 to NodeCount - 1 do
        if AnsiCompareText( Nodes[ i ].Name, AName ) = 0 then
            AList.Add( Nodes[ i ] ) ;
end ;

procedure TXmlNode.NodesClear ;
var
    i               : integer ;
begin
    for i := 0 to NodeCount - 1 do
        TXmlNode( FNodes[ i ] ).Free ;
    FNodes.Clear ;
end ;

procedure TXmlNode.ParseTag( const AValue : string ; TagStart,
    TagClose : integer ) ;
var
    FItems          : TStringList ;
begin
    // Create a list to hold string items
    FItems := TStringList.Create ;
    try
        ParseAttributes( AValue, TagStart, TagClose, FItems ) ;

        // Determine name, attributes or value for each element type
        case ElementType of
            xeDeclaration :
                FName := 'xml' ;
            xeStyleSheet : begin
                    FName := 'xml-stylesheet' ;
                    // We also set this as the value for use in "StyleSheetString"
                    ValueDirect := trim( copy( AValue, TagStart, TagClose - TagStart ) ) ;
                end ;
        else
            // First item is the name - is it there?
            if FItems.Count = 0 then
                raise EFilerError.Create( sxeMissingElementName ) ;

            // Set the name - using the element instead of property for speed
            FName := FItems[ 0 ] ;
            FItems.Delete( 0 ) ;
        end ;                                     //case

        // Any attributes?
        if FItems.Count > 0 then
            FAttributes.Assign( FItems ) ;

    finally
        FItems.Free ;
    end ;
end ;

function TXmlNode.QualifyAsDirectNode : boolean ;
// If this node qualifies as a direct node when writing, we return True.
// A direct node may have attributes, but no value or subnodes
begin
    Result := ( Length( FValue ) = 0 ) and ( NodeCount = 0 ) and not UseFullNodes ;
end ;

function TXmlNode.ReadAttributeInteger( const AName : string ;
    ADefault : integer ) : integer ;
begin
    Result := StrToIntDef( AttributeByName( AName ), ADefault ) ;
end ;

function TXmlNode.ReadAttributeString( const AName : string ; const ADefault : string ) : string ;
begin
    Result := AttributeByName( AName ) ;
    if length( Result ) = 0 then
        Result := ADefault ;
end ;

function TXmlNode.ReadBool( const AName : string ;
    ADefault : boolean ) : boolean ;
var
    AIndex          : integer ;
begin
    Result := ADefault ;
    AIndex := NodeIndexByName( AName ) ;
    if AIndex >= 0 then
        Result := Nodes[ AIndex ].ValueAsBoolDef( ADefault ) ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.ReadBrush( const AName : string ; ABrush : TBrush ) ;
var
    AChild          : TXmlNode ;
begin
    AChild := NodeByName( AName ) ;
    if assigned( AChild ) then
        with AChild do begin
            // Read values
            ABrush.Color := ReadColor( 'Color', clWhite ) ;
            ABrush.Style := TBrushStyle( ReadInteger( 'Style', integer( bsSolid ) ) ) ;
        end
    else begin
        // Defaults
        ABrush.Bitmap := nil ;
        ABrush.Color := clWhite ;
        ABrush.Style := bsSolid ;
    end ;
end ;

function TXmlNode.ReadColor( const AName : string ; ADefault : TColor ) : TColor ;
var
    AIndex          : integer ;
begin
    Result := ADefault ;
    AIndex := NodeIndexByName( AName ) ;
    if AIndex >= 0 then
        Result := StrToInt( Nodes[ AIndex ].ValueAsString ) ;
end ;
{$ENDIF}

function TXmlNode.ReadDateTime( const AName : string ;
    ADefault : TDateTime ) : TDateTime ;
// Date MUST always be written in this format:
// YYYY-MM-DD (if just date) or
// YYYY-MM-DDThh:mm:ss.sssZ (if date and time. The Z stands for universal time
// zone. Since Delphi's TDateTime does not give us a clue about the timezone,
// this is the easiest solution)
// This format SHOULD NOT be changed, to avoid all kinds of
// conversion errors in future.
// This format is compatible with the W3C date/time specification as found here:
// http://www.w3.org/TR/NOTE-datetime
begin
    Result := sdDateTimeFromStringDefault( ReadString( AName, '' ), ADefault ) ;
end ;

function TXmlNode.ReadFloat( const AName : string ; ADefault : double ) : double ;
var
    AIndex          : integer ;
begin
    Result := ADefault ;
    AIndex := NodeIndexByName( AName ) ;
    if AIndex >= 0 then
        Result := Nodes[ AIndex ].ValueAsFloatDef( ADefault ) ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.ReadFont( const AName : string ; AFont : TFont ) ;
var
    AChild          : TXmlNode ;
begin
    AChild := NodeByName( AName ) ;
    AFont.Style := [ ] ;
    if assigned( AChild ) then
        with AChild do begin
            // Read values
            AFont.Name := ReadString( 'Name', 'Arial' ) ;
            AFont.Color := ReadColor( 'Color', clBlack ) ;
            AFont.Size := ReadInteger( 'Size', 14 ) ;
            if ReadBool( 'Bold', False ) then
                AFont.Style := AFont.Style + [ fsBold ] ;
            if ReadBool( 'Italic', False ) then
                AFont.Style := AFont.Style + [ fsItalic ] ;
            if ReadBool( 'Underline', False ) then
                AFont.Style := AFont.Style + [ fsUnderline ] ;
            if ReadBool( 'Strikeout', False ) then
                AFont.Style := AFont.Style + [ fsStrikeout ] ;
        end
    else begin
        // Defaults
        AFont.Name := 'Arial' ;
        AFont.Color := clBlack ;
        AFont.Size := 14 ;
    end ;
end ;
{$ENDIF}

procedure TXmlNode.ReadFromStream( S : TStream ) ;
// Read the node from the starting "<" until the closing ">" from the stream in S.
// This procedure also calls OnNodeNew and OnNodeLoaded events
var
    Ch              : Char ;
    ATagIndex       : integer ;
    AValue          : string ;
    ALength         : integer ;
    ANode           : TXmlNode ;
    ANodeValue      : string ;
    AValuePos, AValueLength : integer ;
    AClose          : integer ;
    HasCR           : boolean ;
    HasSubtags      : boolean ;
    // Local
    function ReadCharSkipBlanks : boolean ;
    begin
        Result := False ;
        repeat
            // Read character, exit if none available
            if S.Read( Ch, 1 ) = 0 then
                exit ;
            // Skip if in controlchars
            if not ( Ch in cControlchars ) then
                break ;
        until False ;
        Result := True ;
    end ;
    // Main
begin
    // Initial reserve: just 80 characters which is OK for most short values
    AValuePos := 0 ;
    AValueLength := 80 ;
    SetLength( ANodeValue, AValueLength ) ;
    HasCR := False ;
    HasSubTags := False ;
    // Trailing blanks/controls chars?
    if not ReadCharSkipBlanks then
        exit ;

    // What is it?
    if Ch = '<' then begin
        // A tag - which one?
        ATagIndex := ReadOpenTag( S ) ;
        if ATagIndex >= 0 then begin
            try
                ElementType := cTags[ ATagIndex ].FStyle ;
                if ElementType in [ xeNormal, xeDirect, xeDeclaration, xeStylesheet ] then begin
                    // These tags we will process
                    ReadStringFromStreamUntil( S, cTags[ ATagIndex ].FClose, AValue ) ;
                    ALength := length( AValue ) ;
                    // Is it a direct tag?
                    if ( ElementType = xeNormal ) and ( ALength > 0 ) and ( AValue[ ALength ] = '/' ) then begin
                        dec( ALength ) ;
                        ElementType := xeDirect ;
                    end ;
                    ParseTag( AValue, 1, ALength + 1 ) ;
                    // Here we know our name so good place to call OnNodeNew event
                    if assigned( Document ) then
                        Document.DoNodeNew( Self ) ;
                    // Now the tag can be a direct close - in that case we're finished
                    if ElementType in [ xeDirect, xeDeclaration, xeStyleSheet ] then
                        exit ;
                    repeat
                        // Process rest of tag
                        if S.Read( Ch, 1 ) <> 1 then
                            raise EFilerError.CreateFmt( sxeMissingCloseTag, [ Name ] ) ;

                        // Is there a subtag?
                        if Ch = '<' then begin
                            if not ReadCharSkipBlanks then
                                raise EFilerError.CreateFmt( sxeMissingDataAfterGreaterThan, [ Name ] ) ;
                            if Ch = '/' then begin

                                // This seems our closing tag
                                if not ReadStringFromStreamUntil( S, '>', AValue ) then
                                    raise EFilerError.CreateFmt( sxeMissingLessThanInCloseTag, [ Name ] ) ;
                                if AnsiCompareText( trim( AValue ), Name ) <> 0 then
                                    raise EFilerError.CreateFmt( sxeIncorrectCloseTag, [ Name ] ) ;
                                break ;

                            end
                            else begin

                                // Reset the HasCR flag if we add node, we only want to detect
                                // the CR after last subnode
                                HasCR := False ;

                                // This is a subtag... so create it and let it process
                                HasSubTags := True ;
                                S.Seek( -2, soFromCurrent ) ;
                                ANode := TXmlNode.Create( Document ) ;
                                NodeAdd( ANode ) ;
                                ANode.ReadFromStream( S ) ;

                                // Check for dropping comments
                                if assigned( Document ) and Document.DropCommentsOnParse and
                                    ( ANode.ElementType = xeComment ) then
                                    NodeDelete( NodeIndexOf( ANode ) ) ;

                            end ;
                        end
                        else begin

                            // If we detect a CR we will set the flag. This will signal the fact
                            // that this XML file was saved with xfReadable
                            if Ch = #13 then
                                HasCR := True ;

                            // Add the character to the node value buffer. We do not abide to
                            // the XML spec here, since we only allow the value to precede
                            // the subtags. However, it simplifies the object model considerably
                            if ( not HasSubTags ) then begin
                                inc( AValuePos ) ;
                                if AValuePos > AValueLength then begin
                                    inc( AValueLength, cNodeValueBuf ) ;
                                    SetLength( ANodeValue, AValueLength ) ;
                                end ;
                                ANodeValue[ AValuePos ] := Ch ;
                            end ;

                        end ;
                    until False ;

                    AClose := AValuePos ;

                    // In case we had subtags, and we were in xfReadable mode we must
                    // remove one CRLF + Indent from the end of the string
                    if HasSubtags and HasCR then
                        while ( AClose > 0 ) do begin
                            dec( AClose ) ;
                            if ANodeValue[ AClose + 1 ] = #13 then
                                break ;
                        end ;
                    ValueDirect := copy( ANodeValue, 1, AClose ) ;

                end
                else begin
                    case ElementType of
                        xeComment : Name := 'Comment' ;
                        xeCData : Name := 'CData' ;
                        xeExclam : Name := 'Special' ;
                        xeQuestion : Name := 'Special' ;
                        xeDoctype : Name := 'DTD' ;
                    else
                        Name := 'Unknown' ;
                    end ;

                    // Here we know our name so good place to call OnNodeNew
                    if assigned( Document ) then
                        Document.DoNodeNew( Self ) ;

                    // In these cases just get all data up till the closing tag
                    ReadStringFromStreamUntil( S, cTags[ ATagIndex ].FClose, AValue ) ;
                    ValueDirect := AValue ;
                end ;
            finally
                // Call the OnNodeLoaded and OnProgress events
                if assigned( Document ) then begin
                    Document.DoNodeLoaded( Self ) ;
                    Document.DoProgress( S.Position ) ;
                end ;
            end ;
        end ;
    end ;

end ;

procedure TXmlNode.ReadFromString( const AValue : string ) ;
var
    M               : TMemoryStream ;
begin
    if length( AValue ) > 0 then begin
        M := TMemoryStream.Create ;
        try
            M.Write( AValue[ 1 ], length( AValue ) ) ;
            M.Position := 0 ;
            ReadFromStream( M ) ;
        finally
            M.Free ;
        end ;
    end ;
end ;

function TXmlNode.ReadInteger( const AName : string ;
    ADefault : integer ) : integer ;
var
    AIndex          : integer ;
begin
    Result := ADefault ;
    AIndex := NodeIndexByName( AName ) ;
    if AIndex >= 0 then
        Result := Nodes[ AIndex ].ValueAsIntegerDef( ADefault ) ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.ReadPen( const AName : string ; APen : TPen ) ;
var
    AChild          : TXmlNode ;
begin
    AChild := NodeByName( AName ) ;
    if assigned( AChild ) then
        with AChild do begin
            // Read values
            APen.Color := ReadColor( 'Color', clBlack ) ;
            APen.Mode := TPenMode( ReadInteger( 'Mode', integer( pmCopy ) ) ) ;
            APen.Style := TPenStyle( ReadInteger( 'Style', integer( psSolid ) ) ) ;
            APen.Width := ReadInteger( 'Width', 1 ) ;
        end
    else begin
        // Defaults
        APen.Color := clBlack ;
        APen.Mode := pmCopy ;
        APen.Style := psSolid ;
        APen.Width := 1 ;
    end ;
end ;
{$ENDIF}

function TXmlNode.ReadString( const AName : string ;
    const ADefault : string ) : string ;
var
    AIndex          : integer ;
begin
    Result := ADefault ;
    AIndex := NodeIndexByName( AName ) ;
    if AIndex >= 0 then
        Result := Nodes[ AIndex ].ValueAsString ;
end ;

function TXmlNode.ReadWidestring( const AName : string ;
    const ADefault : widestring ) : widestring ;
begin
    Result := ToWidestring( ReadString( AName, FromWidestring( ADefault ) ) ) ;
end ;

function TXmlNode.ScanTag( const AValue : string ; var Start, Close,
    Next : integer ; var ATag : TXmlElementType ) : boolean ;
var
    TagIndex        : integer ;
    TagStart        : integer ;
begin
    // when starting:
    // <?xml{data}?>
    // ^                                    ^
    // Start                                Close

    Result := False ;
    if not TrimPos( AValue, Start, Close ) then
        exit ;

    // Try to detect any tag
    TagIndex := 0 ;
    repeat
        while TagIndex < cTagCount do
            if MatchString( cTags[ TagIndex ].FStart, AValue, Start ) then
                break
            else
                inc( TagIndex ) ;

        // Nothing found?
        if TagIndex = cTagCount then
            exit ;

        inc( Start, Length( cTags[ TagIndex ].FStart ) ) ;

        // Try to find the closing tag
        if FindString( cTags[ TagIndex ].FClose, AValue, Start, Close, TagStart ) then
            break ;

        // Not found .. try more tag records - but restore start pos
        inc( TagIndex ) ;
        dec( Start, Length( cTags[ TagIndex ].FStart ) ) ;

    until False ;

    // We did find it so update results and return True
    Result := True ;
    ATag := cTags[ TagIndex ].FStyle ;
    Close := TagStart ;
    Next := Close + Length( cTags[ TagIndex ].FClose ) ;

    if ATag = xeNormal then
        // Check for direct
        if AValue[ Close - 1 ] = '/' then begin
            dec( Close ) ;
            ATag := xeDirect ;
        end ;
end ;

procedure TXmlNode.SetAttributeName( Index : integer ; const Value : string ) ;
begin
    if ( Index >= 0 ) and ( Index < AttributeCount ) then
        FAttributes[ Index ] := Format( '%s=%s', [ Value, AttributeValue[ Index ] ] ) ;
end ;

procedure TXmlNode.SetAttributeValue( Index : integer ; const Value : string ) ;
begin
    if ( Index >= 0 ) and ( Index < AttributeCount ) then
        FAttributes[ Index ] := Format( '%s=%s', [ AttributeName[ Index ],
            QuoteString( EscapeString( Value ) ) ] ) ;
end ;

procedure TXmlNode.SetBinaryEncoding( const Value : TBinaryEncodingType ) ;
begin
    if assigned( Document ) then
        Document.BinaryEncoding := Value ;
end ;

procedure TXmlNode.SetBinaryString( const Value : string ) ;
var
    OldEncoding     : TBinaryEncodingType ;
begin
    // Set to base64
    OldEncoding := BinaryEncoding ;
    try
        BinaryEncoding := xbeBase64 ;
        if length( Value ) = 0 then begin
            ValueAsString := '' ;
            exit ;
        end ;
        // fill the buffer
        BufferWrite( Value[ 1 ], length( Value ) ) ;
    finally
        BinaryEncoding := OldEncoding ;
    end ;
end ;

procedure TXmlNode.SetName( const Value : string ) ;
var
    i               : integer ;
begin
    if FName <> Value then begin
        // Check if the name abides the rules. We will be very forgiving here and
        // just accept any name that at least does not contain control characters
        for i := 1 to length( Value ) do
            if Value[ i ] in cControlChars then
                raise Exception.Create( Format( sxeIllegalCharInNodeName, [ Value ] ) ) ;
        FName := Value ;
    end ;
end ;

procedure TXmlNode.SetValueAsString( const AValue : string ) ;
begin
    FValue := EscapeString( AValue ) ;
end ;

procedure TXmlNode.SetValueAsWidestring( const Value : widestring ) ;
begin
    ValueAsString := FromWidestring( Value ) ;
end ;

function TXmlNode.ToAnsiString( const s : string ) : string ;
begin
    if Utf8Encoded then
        Result := sdUtf8ToAnsi( s )
    else
        Result := s ;
end ;

function TXmlNode.ToWidestring( const s : string ) : widestring ;
begin
    if Utf8Encoded then
        Result := sdUtf8ToUnicode( s )
    else
        Result := s ;
end ;

function TXmlNode.UnescapeString( const AValue : string ) : string ;
begin
    if Utf8Encoded then
        Result := UnescapeStringUTF8( AValue )
    else
        Result := UnescapeStringAnsi( AValue ) ;
end ;

function TXmlNode.UseFullNodes : boolean ;
begin
    Result := False ;
    if assigned( Document ) then
        Result := Document.UseFullNodes ;
end ;

function TXmlNode.Utf8Encoded : boolean ;
begin
    Result := False ;
    if assigned( Document ) then
        Result := Document.Utf8Encoded ;
end ;

function TXmlNode.ValueAsBoolDef( ADefault : boolean ) : boolean ;
var
    AValue          : string ;
begin
    Result := ADefault ;
    AValue := Uppercase( FValue ) ;
    if Length( Avalue ) = 0 then
        exit ;
    if ( AValue = 'TRUE' ) or ( AValue[ 1 ] = 'T' ) or ( AValue[ 1 ] = 'Y' ) then begin
        Result := True ;
        exit ;
    end ;
    if ( AValue = 'FALSE' ) or ( AValue[ 1 ] = 'F' ) or ( AValue[ 1 ] = 'N' ) then begin
        Result := False ;
        exit ;
    end ;
end ;

function TXmlNode.ValueAsDateTimeDef( ADefault : TDateTime ) : TDateTime ;
begin
    Result := sdDateTimeFromStringDefault( ValueAsString, ADefault ) ;
end ;

function TXmlNode.ValueAsFloatDef( ADefault : double ) : double ;
var
    Code            : integer ;
begin
    try
        val( StringReplace( FValue, ',', '.', [ ] ), Result, Code ) ;
        if Code > 0 then
            Result := ADefault ;
    except
        Result := ADefault ;
    end ;
end ;

function TXmlNode.ValueAsIntegerDef( ADefault : integer ) : integer ;
begin
    Result := StrToIntDef( FValue, ADefault ) ;
end ;

procedure TXmlNode.WriteAttributeInteger( const AName : string ; AValue : integer ; ADefault : integer ) ;
var
    AIndex          : integer ;
begin
    AIndex := AttributeIndexByName( AName ) ;
    if AIndex >= 0 then
        AttributeValue[ AIndex ] := IntToStr( AValue )
    else
        AttributeAdd( AName, IntToStr( AValue ) ) ;
end ;

procedure TXmlNode.WriteAttributeString( const AName, AValue,
    ADefault : string ) ;
var
    AIndex          : integer ;
begin
    AIndex := AttributeIndexByName( AName ) ;
    if AIndex >= 0 then
        AttributeValue[ AIndex ] := AValue
    else
        AttributeAdd( AName, AValue ) ;
end ;

procedure TXmlNode.WriteBool( const AName : string ; AValue : boolean ; ADefault : boolean ) ;
const
    cBoolValues     : array[ boolean ] of string = ( 'False', 'True' ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        with NodeReplace( AName ) do
            ValueAsString := cBoolValues[ AValue ] ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.WriteBrush( const AName : string ; ABrush : TBrush ) ;
begin
    with NodeReplace( AName ) do begin
        WriteColor( 'Color', ABrush.Color, clBlack ) ;
        WriteInteger( 'Style', integer( ABrush.Style ), 0 ) ;
    end ;
end ;

procedure TXmlNode.WriteColor( const AName : string ; AValue, ADefault : TColor ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        WriteHex( AName, ColorToRGB( AValue ), 8, 0 ) ;
end ;
{$ENDIF}

procedure TXmlNode.WriteDateTime( const AName : string ; AValue,
    ADefault : TDateTime ) ;
// Date MUST always be written in this format:
// YYYY-MM-DD (if just date) or
// YYYY-MM-DDThh:mm:ss.sssZ (if date and time. The Z stands for universal time
// zone. Since Delphi's TDateTime does not give us a clue about the timezone,
// this is the easiest solution)
// This format SHOULD NOT be changed, to avoid all kinds of
// conversion errors in future.
// This format is compatible with the W3C date/time specification as found here:
// http://www.w3.org/TR/NOTE-datetime
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        WriteString( AName, sdDateTimeToString( AValue ), '' ) ;
end ;

procedure TXmlNode.WriteFloat( const AName : string ; AValue : double ; ADefault : double ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        with NodeReplace( AName ) do
            ValueAsString := FloatToStr( AValue ) ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.WriteFont( const AName : string ; AFont : TFont ) ;
begin
    with NodeReplace( AName ) do begin
        WriteString( 'Name', AFont.Name, 'Arial' ) ;
        WriteColor( 'Color', AFont.Color, clBlack ) ;
        WriteInteger( 'Size', AFont.Size, 14 ) ;
        WriteBool( 'Bold', fsBold in AFont.Style, False ) ;
        WriteBool( 'Italic', fsItalic in AFont.Style, False ) ;
        WriteBool( 'Underline', fsUnderline in AFont.Style, False ) ;
        WriteBool( 'Strikeout', fsStrikeout in AFont.Style, False ) ;
    end ;
end ;
{$ENDIF}

procedure TXmlNode.WriteHex( const AName : string ; AValue, Digits : integer ; ADefault : integer ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        with NodeReplace( AName ) do
            ValueAsString := '$' + IntToHex( AValue, Digits ) ;
end ;

function TXmlNode.WriteInnerTag : string ;
// Write the inner part of the tag, the one that contains the attributes
var
    i               : integer ;
begin
    // Attributes
    for i := 0 to AttributeCount - 1 do begin
        Result := Result + ' ' + AttributePair[ i ] ;
    end ;
    // End of tag - direct nodes get an extra "/"
    if QualifyAsDirectNode then
        Result := Result + '/' ;
end ;

procedure TXmlNode.WriteInteger( const AName : string ; AValue : integer ; ADefault : integer ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        with NodeReplace( AName ) do
            ValueAsString := IntToStr( AValue ) ;
end ;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.WritePen( const AName : string ; APen : TPen ) ;
begin
    with NodeReplace( AName ) do begin
        WriteColor( 'Color', APen.Color, clBlack ) ;
        WriteInteger( 'Mode', integer( APen.Mode ), 0 ) ;
        WriteInteger( 'Style', integer( APen.Style ), 0 ) ;
        WriteInteger( 'Width', APen.Width, 0 ) ;
    end ;
end ;
{$ENDIF}

procedure TXmlNode.WriteString( const AName, AValue : string ; const ADefault : string ) ;
begin
    if WriteOnDefault or ( AValue <> ADefault ) then
        with NodeReplace( AName ) do
            ValueAsString := AValue ;
end ;

procedure TXmlNode.WriteToStream( S : TStream ) ;
var
    i               : integer ;
    AIndent         : string ;
    ALineFeed       : string ;
    ALine           : string ;
begin
    AIndent := GetIndent ;
    ALineFeed := GetLineFeed ;

    // Write indent
    ALine := AIndent ;

    // Write the node - distinguish node type
    case ElementType of
        xeComment : begin
                // Comment <!--{comment}-->
                ALine := ALine + Format( '<!--%s-->', [ ValueDirect ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeCData : begin
                // literal data <![CDATA[{data}]]>
                ALine := ALine + Format( '<![CDATA[%s]]>', [ ValueDirect ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeDeclaration : begin
                // XML declaration <?xml{declaration}?>
                ALine := ALine + Format( '<?xml%s?>', [ WriteInnerTag ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeStylesheet : begin
                // Stylesheet <?xml-stylesheet{stylesheet}?>
                ALine := ALine + Format( '<?xml-stylesheet%s?>', [ WriteInnerTag ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeExclam :                                {// Any <!data>} begin
                ALine := ALine + Format( '<!%s>', [ ValueDirect ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeQuestion :                              {// Any <?data?>} begin
                ALine := ALine + Format( '<?%s?>', [ ValueDirect ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
        xeUnknown :                               {// Any <data>} begin
                // This cannot happen normally because we are not preserving them at read
                // time. But they can exist if the the user sets it. In that case, just
                // output the ValueDirect inbetween brackets
                ALine := ALine + Format( '<%s>', [ ValueDirect ] ) ;
                WriteStringToStream( S, ALine ) ;
            end ;
    else
        // All other, namely xeNormal, xeDirect

        // Write tag
        ALine := ALine + Format( '<%s%s>', [ FName, WriteInnerTag ] ) ;

        // Write value (if any)
        ALine := ALine + FValue ;
        if ( NodeCount > 0 ) then
            // ..and a linefeed
            ALine := ALine + ALineFeed ;

        WriteStringToStream( S, ALine ) ;

        // Write child elements
        for i := 0 to NodeCount - 1 do begin
            Nodes[ i ].WriteToStream( S ) ;
            WriteStringToStream( S, ALineFeed ) ;
        end ;

        // Write end tag
        ALine := '' ;
        if not QualifyAsDirectNode then begin
            if NodeCount > 0 then
                ALine := ALine + AIndent ;
            ALine := ALine + Format( '</%s>', [ FName ] ) ;
        end ;
        WriteStringToStream( S, ALine ) ;

    end ;                                         //case

    // Call the onprogress
    if assigned( Document ) then
        Document.DoProgress( S.Position ) ;
end ;

function TXmlNode.WriteToString : string ;
var
    M               : TMemoryStream ;
begin
    // We will simply call WriteToStream and collect the result as string using
    // a memory stream
    M := TMemoryStream.Create ;
    try
        WriteToStream( M ) ;
        SetLength( Result, M.Size ) ;
        Move( M.Memory^, Result[ 1 ], M.Size ) ;
    finally
        M.Free ;
    end ;
end ;

procedure TXmlNode.WriteWidestring( const AName : string ;
    const AValue : widestring ; const ADefault : widestring ) ;
begin
    WriteString( AName, FromWidestring( AValue ), ADefault ) ;
end ;

{ TsdXmlDocument }

procedure TsdXmlDocument.Assign( Source : TPersistent ) ;
begin
    if Source is TsdXmlDocument then
        CopyFrom( Source as TsdXmlDocument )
    else
        inherited ;
end ;

procedure TsdXmlDocument.Clear ;
begin
    // Reset defaults
    SetDefaults ;
    // Clear root
    FRoot.Clear ;
    // Clear extra nodes
    ExtraNodesClear ;
end ;

procedure TsdXmlDocument.CopyFrom( Source : TsdXmlDocument ) ;
var
    S               : TStream ;
begin
    Clear ;
    if not assigned( Source ) then
        exit ;
    S := TMemoryStream.Create ;
    try
        Source.WriteToStream( S ) ;
        S.Position := 0 ;
        ReadFromStream( S ) ;
    finally
        S.Free ;
    end ;
end ;

constructor TsdXmlDocument.Create ;
begin
    inherited Create ;
    FComments := TStringList.Create ;
    // Defaults
    SetDefaults ;
    // Root node
    FRoot := TXmlNode.Create( Self ) ;
    // Extra nodes
    FExtraNodes := TList.Create ;
end ;

constructor TsdXmlDocument.CreateName( const ARootName : string ) ;
begin
    Create ;
    Root.Name := ARootName ;
end ;

destructor TsdXmlDocument.Destroy ;
begin
    FRoot.Free ;
    FRoot := nil ;
    FComments.Free ;
    FComments := nil ;
    ExtraNodesClear ;
    FExtraNodes.Free ;
    FExtraNodes := nil ;
    inherited ;
end ;

procedure TsdXmlDocument.DoNodeLoaded( Node : TXmlNode ) ;
begin
    if assigned( FOnNodeLoaded ) then
        FOnNodeLoaded( Self, Node ) ;
end ;

procedure TsdXmlDocument.DoNodeNew( Node : TXmlNode ) ;
begin
    if assigned( FOnNodeNew ) then
        FOnNodeNew( Self, Node ) ;
end ;

procedure TsdXmlDocument.DoProgress( Size : integer ) ;
begin
    if assigned( FOnProgress ) then
        FOnProgress( Self, Size ) ;
end ;

procedure TsdXmlDocument.DoUnicodeLoss( Sender : TObject ) ;
begin
    if assigned( FOnUnicodeLoss ) then
        FOnUnicodeLoss( Self ) ;
end ;

procedure TsdXmlDocument.ExtraNodesClear ;
// Clear extra nodes
var
    i               : integer ;
begin
    if not assigned( FExtraNodes ) then
        exit ;
    for i := 0 to ExtraNodeCount - 1 do
        TXmlNode( FExtraNodes[ i ] ).Free ;
    FExtraNodes.Clear ;
end ;

function TsdXmlDocument.GetCommentString : string ;
begin
    Result := FComments.Text ;
end ;

function TsdXmlDocument.GetExtraNodeCount : integer ;
begin
    Result := 0 ;
    if assigned( FExtraNodes ) then
        Result := FExtraNodes.Count ;
end ;

function TsdXmlDocument.GetExtraNodes( Index : integer ) : TXmlNode ;
begin
    Result := nil ;
    if ( Index >= 0 ) and ( Index < ExtraNodeCount ) then
        Result := TXmlNode( FExtraNodes[ Index ] ) ;
end ;

function TsdXmlDocument.IsEmpty : boolean ;
begin
    Result := True ;
    if assigned( FRoot ) then
        Result := FRoot.IsClear ;
end ;

function TsdXmlDocument.LineFeed : string ;
begin
    case XmlFormat of
        xfReadable : Result := #13#10 ;
        xfCompact : Result := #10 ;
    else
        Result := #10 ;
    end ;
end ;

procedure TsdXmlDocument.LoadFromFile( const FileName : string ) ;
var
    S               : TStream ;
begin
    S := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite ) ;
    try
        LoadFromStream( S ) ;
    finally
        S.Free ;
    end ;
end ;

procedure TsdXmlDocument.LoadFromStream( Stream : TStream ) ;
var
    B               : TsdBufferedReadStream ;
begin
    // Create buffer filter. Since we read from the original stream a buffer at a
    // time, this speeds up the reading process for disk-based files.
    B := TsdBufferedReadStream.Create( Stream, False ) ;
    try
        // We will create a conversion stream as intermediate
        if Utf8Encoded then
            FCodecStream := TsdUtf8Stream.Create( B )
        else
            FCodecStream := TsdAnsiStream.Create( B ) ;
        try
            // Connect events
            FCodecStream.OnUnicodeLoss := DoUnicodeLoss ;
            // Read from stream
            ReadFromStream( FCodecStream ) ;
            // Set our external encoding
            FExternalEncoding := FCodecStream.Encoding ;
            // Set internal encoding
            if ( ExternalEncoding = seUtf8 ) or ( EncodingString = 'UTF-8' ) then
                FUtf8Encoded := True ;
        finally
            FreeAndNil( FCodecStream ) ;
        end ;
    finally
        B.Free ;
    end ;
end ;

procedure TsdXmlDocument.ReadFromStream( S : TStream ) ;
var
    i               : integer ;
    ARoot           : TXmlNode ;
    ANode           : TXmlNode ;
begin
    // Clear the old document
    Clear ;
    // A temporary node for reading
    FTempRoot := TXmlNode.Create( Self ) ;
    try
        with FTempRoot do begin
            DoProgress( 0 ) ;
            repeat
                ANode := NodeNew( '' ) ;
                ANode.ReadFromStream( S ) ;
                // XML declaration
                if ANode.ElementType = xeDeclaration then begin
                    if ANode.HasAttribute( 'version' ) then
                        VersionString := ANode.AttributeByName( 'version' ) ;
                    if ANode.HasAttribute( 'encoding' ) then
                        EncodingString := ANode.AttributeByName( 'encoding' ) ;
                    // Check encoding
                    if assigned( FCodecStream ) then begin
                        if EncodingString = 'UTF-8' then
                            FCodecStream.Encoding := seUTF8 ;
                    end ;
                    NodeDelete( 0 ) ;
                    continue ;
                end ;
                if Length( Nodes[ NodeCount - 1 ].Name ) = 0 then
                    NodeDelete( NodeCount - 1 ) ;
            until S.Position >= S.Size ;
            DoProgress( S.Size ) ;

            // Find root node
            ARoot := nil ;
            for i := 0 to NodeCount - 1 do begin
                // We will no longer point to the temp parent
                Nodes[ i ].Parent := nil ;
                // A normal node, this is our root
                if Nodes[ i ].ElementType in [ xeNormal, xeDirect ] then begin
                    // More than one root?
                    if assigned( ARoot ) then
                        raise EFilerError.Create( sxeMoreThanOneRootElement ) ;
                    ARoot := Nodes[ i ] ;
                end ;
            end ;
            // No root found
            if not assigned( ARoot ) then
                raise EFilerError.Create( sxeNoRootElement ) ;

            // Now we can adjust our root, remove old and assign new
            FreeAndNil( FRoot ) ;
            FRoot := NodeExtract( ARoot ) ;

            // Other important data
            while NodeCount > 0 do begin
                ANode := Nodes[ 0 ] ;
                // XML declaration
                if ANode.ElementType = xeDeclaration then begin
                    if ANode.HasAttribute( 'version' ) then
                        VersionString := ANode.AttributeByName( 'version' ) ;
                    if ANode.HasAttribute( 'encoding' ) then
                        EncodingString := ANode.AttributeByName( 'encoding' ) ;
                    NodeDelete( 0 ) ;
                    continue ;
                end ;

                // XML stylesheet
                if ANode.ElementType = xeStylesheet then begin
                    if Length( StyleSheetString ) = 0 then
                        StyleSheetString := ANode.ValueAsString
                    else
                        raise EFilerError.Create( sxeMultiStylesheetsNotSupp ) ;
                    // To do: parse stylesheet
                    NodeDelete( 0 ) ;
                    continue ;
                end ;

                // Comment - just one comment in the root supported
                if ElementType = xeComment then begin
                    FComments.Add( ANode.ValueAsString ) ;
                    NodeDelete( 0 ) ;
                    continue ;
                end ;

                // CData
                if ElementType = xeCData then
                    raise EFilerError.Create( sxeCDATAInRoot ) ;

                // Other nodes, e.g. xeQuestion/xeExclam/xeDoctype
                FExtraNodes.Add( NodeExtract( ANode ) ) ;
            end ;

        end ;                                     //with
    finally
        FreeAndNil( FTempRoot ) ;
    end ;
end ;

procedure TsdXmlDocument.ReadFromString( const AValue : string ) ;
var
    S               : TStream ;
begin
    S := TMemoryStream.Create ;
    try
        WriteStringToStream( S, AValue ) ;
        S.Position := 0 ;
        ReadFromStream( S ) ;
    finally
        S.Free ;
    end ;
end ;

procedure TsdXmlDocument.SaveToFile( const FileName : string ) ;
var
    S               : TStream ;
begin
    S := TFileStream.Create( FileName, fmCreate ) ;
    try
        SaveToStream( S ) ;
    finally
        S.Free ;
    end ;
end ;

procedure TsdXmlDocument.SaveToStream( Stream : TStream ) ;
var
    B               : TsdBufferedWriteStream ;
begin
    // Create buffer filter. Since we write a buffer at a time to the destination
    // stream, this speeds up the writing process for disk-based files.
    B := TsdBufferedWriteStream.Create( Stream, False ) ;
    try
        // Create conversion stream
        if Utf8Encoded then
            FCodecStream := TsdUtf8Stream.Create( B )
        else
            FCodecStream := TsdAnsiStream.Create( B ) ;
        try
            // Set External encoding
            FCodecStream.Encoding := FExternalEncoding ;
            WriteToStream( FCodecStream ) ;
        finally
            FCodecStream.Free ;
        end ;
    finally
        B.Free ;
    end ;
end ;

procedure TsdXmlDocument.SetCommentString( const Value : string ) ;
begin
    FComments.Text := Value ;
end ;

procedure TsdXmlDocument.SetDefaults ;
begin
    // Defaults
    FComments.Clear ;
    ;
    FStyleSheetString := '' ;
    FEncodingString := cDefaultEncodingString ;
    FExternalEncoding := cDefaultExternalEncoding ;
    FVersionString := cDefaultVersionString ;
    FXmlFormat := cDefaultXmlFormat ;
    FWriteOnDefault := cDefaultWriteOnDefault ;
    FBinaryEncoding := cDefaultBinaryEncoding ;
    FUtf8Encoded := cDefaultUtf8Encoded ;
end ;

procedure TsdXmlDocument.WriteToStream( S : TStream ) ;
var
    i               : integer ;
    ALine           : string ;
begin
    if not assigned( Root ) then
        raise EFilerError.Create( sxeRootElementNotDefined ) ;

    DoProgress( 0 ) ;
    // Version info
    if ( Length( VersionString ) > 0 ) or ( Length( EncodingString ) > 0 ) then begin
        ALine := '<?xml ' ;
        if VersionString <> '' then
            ALine := ALine + Format( 'version=%s', [ QuoteString( EscapeString( VersionString ) ) ] ) ;
        if EncodingString <> '' then
            ALine := ALine + ' ' + Format( 'encoding=%s', [ QuoteString( EscapeString( EncodingString ) ) ] ) ;
        ALine := ALine + '?>' + LineFeed ;
    end ;

    // Stylesheet string
    if Length( StyleSheetString ) > 0 then
        ALine := ALine + Format( '<?xml-stylesheet %s?>', [ StyleSheetString ] ) + LineFeed ;

    // Comments
    for i := 0 to FComments.Count - 1 do
        if Length( FComments[ i ] ) > 0 then
            ALine := ALine + Format( '<!--%s-->', [ EscapeString( FComments[ i ] ) ] ) + LineFeed ;

    // Preserve extra nodes
    for i := 0 to ExtraNodeCount - 1 do
        with ExtraNodes[ i ] do begin
            case ElementType of
                xeDoctype :
                    ALine := ALine + Format( '<!DOCTYPE%s>', [ ValueDirect ] ) + LineFeed ;
                xeQuestion :
                    ALine := ALine + Format( '<?%s?>', [ ValueDirect ] ) + LineFeed ;
                xeExclam :
                    ALine := ALine + Format( '<!%s>', [ ValueDirect ] ) + LineFeed ;
                xeUnknown :
                    ALine := ALine + Format( '<%s>', [ ValueDirect ] ) + LineFeed ;
            end ;
        end ;
    // Write header to the stream
    WriteStringToStream( S, ALine ) ;

    // write the root
    Root.WriteToStream( S ) ;
    DoProgress( S.Size ) ;
end ;

function TsdXmlDocument.WriteToString : string ;
var
    S               : TStream ;
begin
    S := TMemoryStream.Create ;
    try
        WriteToStream( S ) ;
        S.Position := 0 ;
        SetLength( Result, S.Size ) ;
        S.Read( Result[ 1 ], S.Size ) ;
    finally
        S.Free ;
    end ;
end ;

{ TsdCodecStream }

constructor TsdCodecStream.Create( AStream : TStream ) ;
begin
    inherited Create ;
    FStream := AStream ;
end ;

function TsdCodecStream.Read( var Buffer ; Count : LongInt ) : Longint ;
// Read from FStream and pass back data
var
    i, j            : integer ;
    BOM             : array[ 0..3 ] of char ;
    BytesRead       : integer ;
    Found           : boolean ;
begin
    Result := 0 ;
    if FMode = umUnknown then begin
        FMode := umRead ;
        // Check FStream
        if not assigned( FStream ) then
            raise EStreamError.Create( sxeCodecStreamNotAssigned ) ;

        // Determine encoding
        FEncoding := se8bit ;
        BytesRead := FStream.Read( BOM[ 0 ], 4 ) ;
        for i := 0 to cBomInfoCount - 1 do begin
            Found := True ;
            for j := 0 to min( BytesRead, cBomInfo[ i ].Len ) - 1 do begin
                if BOM[ j ] <> cBomInfo[ i ].BOM[ j ] then begin
                    Found := False ;
                    break ;
                end ;
            end ;
            if Found then
                break ;
        end ;
        if Found then begin
            FEncoding := cBomInfo[ i ].Enc ;
            FWriteBom := cBomInfo[ i ].HasBOM ;
        end
        else begin
            // Unknown.. default to this
            FEncoding := se8bit ;
            FWriteBom := False ;
        end ;

        // Some encodings are not supported (yet)
        if FEncoding in [ seUCS4BE, seUCS4_2143, seUCS4_2143, seUCS4_3412, seEBCDIC ] then
            raise EStreamError.Create( sxeUnsupportedEncoding ) ;

        // Correct stream to start position
        if FWriteBom then
            FStream.Seek( cBomInfo[ i ].Len - BytesRead, soFromCurrent )
        else
            FStream.Seek( -BytesRead, soFromCurrent ) ;

        // Check if we must swap byte order
        if FEncoding in [ se16BitBE, seUTF16BE ] then
            FSwapByteOrder := True ;

    end ;

    // Check mode
    if FMode <> umRead then
        raise EStreamError.Create( sxeCannotReadCodecForWriting ) ;

    // Check count
    if Count <> 1 then
        raise EStreamError.Create( sxeCannotReadMultipeChar ) ;

    // Now finally read
    Byte( Buffer ) := ReadByte ;
    if Byte( Buffer ) <> 0 then
        Result := 1 ;
end ;

function TsdCodecStream.ReadByte : byte ;
begin
    // default does nothing
    Result := 0 ;
end ;

function TsdCodecStream.Seek( Offset : Longint ; Origin : Word ) : Longint ;
begin
    Result := 0 ;
    if FMode = umUnknown then
        raise EStreamError.Create( sxeCannotSeekBeforeReadWrite ) ;

    if Origin = soFromCurrent then begin
        if Offset = 0 then begin
            // Position
            Result := FStream.Position ;
            exit ;
        end ;
        if ( FMode = umRead ) and ( ( Offset = -1 ) or ( Offset = -2 ) ) then begin
            FBuffer := '' ;
            case Offset of
                -1 : FStream.Seek( FPosMin1, soFromBeginning ) ;
                -2 : FStream.Seek( FPosMin2, soFromBeginning ) ;
            end ;
            exit ;
        end ;
    end ;
    if ( Origin = soFromEnd ) and ( Offset = 0 ) then begin
        // Size
        Result := FStream.Size ;
        exit ;
    end ;
    // Ignore set position from beginning (used in Size command)
    if Origin = soFromBeginning then
        exit ;
    // Arriving here means we cannot do it
    raise EStreamError.Create( sxeCannotPerformSeek ) ;
end ;

procedure TsdCodecStream.StorePrevPositions ;
begin
    FPosMin2 := FPosMin1 ;
    FPosMin1 := FStream.Position ;
end ;

function TsdCodecStream.Write( const Buffer ; Count : Longint ) : Longint ;
var
    i               : integer ;
begin
    if FMode = umUnknown then begin
        FMode := umWrite ;

        // Some encodings are not supported (yet)
        if FEncoding in [ seUCS4BE, seUCS4_2143, seUCS4_2143, seUCS4_3412, seEBCDIC ] then
            raise EStreamError.Create( sxeUnsupportedEncoding ) ;

        // Find correct encoding info
        for i := 0 to cBomInfoCount - 1 do
            if cBomInfo[ i ].Enc = FEncoding then begin
                FWriteBom := cBomInfo[ i ].HasBOM ;
                break ;
            end ;

        // Write BOM
        if FWriteBom then
            FStream.WriteBuffer( cBomInfo[ i ].BOM, cBomInfo[ i ].Len ) ;

        // Check if we must swap byte order
        if FEncoding in [ se16BitBE, seUTF16BE ] then
            FSwapByteOrder := True ;
    end ;

    if FMode <> umWrite then
        raise EStreamError.Create( sxeCannotWriteCodecForReading ) ;

    WriteBuf( Buffer, Count ) ;
    Result := Count ;
end ;

procedure TsdCodecStream.WriteBuf( const Buffer ; Count : Integer ) ;
var
    i               : integer ;
begin
    // Default just writes out bytes one by one. We override this in descendants
    // to provide faster writes for some modes
    for i := 0 to Count - 1 do
        WriteByte( TByteArray( Buffer )[ i ] ) ;
end ;

procedure TsdCodecStream.WriteByte( const B : byte ) ;
begin
    // default does nothing
end ;

{ TsdAnsiStream }

function TsdAnsiStream.ReadByte : byte ;
var
    B               : byte ;
    W               : word ;
begin
    StorePrevPositions ;

    case FEncoding of
        se8Bit, seUTF8 : begin
                // Just a flat read of one byte. UTF8 is not converted back, when UTF8
                // encoding is detected, the document will set Utf8Encoded to True.
                B := 0 ;
                FStream.Read( B, 1 ) ;
                Result := B ;
            end ;
        se16BitBE, se16BitLE, seUTF16BE, seUTF16LE : begin
                // Read two bytes
                W := 0 ;
                FStream.Read( W, 2 ) ;
                // Swap byte order
                if FSwapByteOrder then
                    W := swap( W ) ;
                // Unicode warning loss
                if ( ( W and $FF00 ) > 0 ) and not FWarningUnicodeLoss then begin
                    FWarningUnicodeLoss := True ;
                    if assigned( FOnUnicodeLoss ) then
                        FOnUnicodeLoss( Self ) ;
                end ;
                // ignore high order bytes
                Result := W and $FF ;
            end ;
    else
        raise EStreamError.Create( sxeUnsupportedEncoding ) ;
    end ;
end ;

procedure TsdAnsiStream.WriteBuf( const Buffer ; Count : Integer ) ;
begin
    case FEncoding of
        se8bit : begin
                // one on one
                if FStream.Write( Buffer, Count ) <> Count then
                    raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
            end ;
    else
        inherited ;
    end ;
end ;

procedure TsdAnsiStream.WriteByte( const B : byte ) ;
var
    SA, SU          : string ;
    W               : word ;
begin
    case FEncoding of
        se8Bit : begin
                // Just a flat write of one byte
                FStream.Write( B, 1 ) ;
            end ;
        seUTF8 : begin
                // Convert Ansi to UTF8
                SA := char( B ) ;
                SU := sdAnsiToUTF8( SA ) ;
                // write out
                if FStream.Write( SU[ 1 ], length( SU ) ) = 0 then
                    raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
            end ;
        se16BitBE, se16BitLE, seUTF16BE, seUTF16LE : begin
                // Convert Ansi to Unicode
                W := B ;
                // Swap byte order
                if FSwapByteOrder then
                    W := swap( W ) ;
                // write out
                if FStream.Write( W, 2 ) = 0 then
                    raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
            end ;
    else
        raise EStreamError.Create( sxeUnsupportedEncoding ) ;
    end ;
end ;

{ TsdUtf8Stream }

function TsdUtf8Stream.ReadByte : byte ;
var
    B, B1, B2, B3   : byte ;
    W               : word ;
    SA              : string ;
begin
    Result := 0 ;

    // New character?
    if ( Length( FBuffer ) = 0 ) or ( FBufferPos > length( FBuffer ) ) then begin
        StorePrevPositions ;
        FBufferPos := 1 ;
        // Read another char and put in buffer
        case FEncoding of
            se8Bit : begin
                    // read one byte
                    B := 0 ;
                    FStream.Read( B, 1 ) ;
                    SA := char( B ) ;
                    // Convert to UTF8
                    FBuffer := sdAnsiToUtf8( SA ) ;
                end ;
            seUTF8 : begin
                    // Read one, two or three bytes in the buffer
                    B1 := 0 ;
                    FStream.Read( B1, 1 ) ;
                    FBuffer := char( B1 ) ;
                    if ( B1 and $80 ) > 0 then begin
                        if ( B1 and $20 ) <> 0 then begin
                            B2 := 0 ;
                            FStream.Read( B2, 1 ) ;
                            FBuffer := FBuffer + char( B2 ) ;
                        end ;
                        B3 := 0 ;
                        FStream.Read( B3, 1 ) ;
                        FBuffer := FBuffer + char( B3 ) ;
                    end ;
                end ;
            se16BitBE, se16BitLE, seUTF16BE, seUTF16LE : begin
                    // Read two bytes
                    W := 0 ;
                    FStream.Read( W, 2 ) ;
                    // Swap byte order
                    if FSwapByteOrder then
                        W := swap( W ) ;
                    // Convert to UTF8 in buffer
{$IFDEF D5UP}
                    FBuffer := sdUnicodeToUtf8( widechar( W ) ) ;
{$ELSE}
                    FBuffer := sdUnicodeToUtf8( char( W and $FF ) ) ;
{$ENDIF}
                end ;
        else
            raise EStreamError.Create( sxeUnsupportedEncoding ) ;
        end ;
    end ;

    // Now we have the buffer, so read
    if ( FBufferPos > 0 ) and ( FBufferPos <= length( FBuffer ) ) then
        Result := byte( FBuffer[ FBufferPos ] ) ;
    inc( FBufferPos ) ;
end ;

procedure TsdUtf8Stream.WriteBuf( const Buffer ; Count : Integer ) ;
begin
    case FEncoding of
        seUtf8 : begin
                // one on one
                if FStream.Write( Buffer, Count ) <> Count then
                    raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
            end
    else
        inherited ;
    end ;
end ;

procedure TsdUtf8Stream.WriteByte( const B : byte ) ;
var
    SA              : string ;
    SW              : widestring ;
    MustWrite       : boolean ;
begin
    case FEncoding of
        se8Bit, se16BitBE, se16BitLE, seUTF16BE, seUTF16LE : begin
                MustWrite := True ;
                case Length( FBuffer ) of
                    0 : begin
                            FBuffer := char( B ) ;
                            if ( B and $80 ) <> 0 then
                                MustWrite := False ;
                        end ;
                    1 : begin
                            FBuffer := FBuffer + char( B ) ;
                            if ( byte( FBuffer[ 1 ] ) and $20 ) <> 0 then
                                MustWrite := False ;
                        end ;
                    2 : FBuffer := FBuffer + char( B ) ;
                end ;
                if MustWrite then begin
                    if FEncoding = se8Bit then begin
                        // Convert to ansi
                        SA := sdUtf8ToAnsi( FBuffer ) ;
                        // write out
                        if length( SA ) = 1 then
                            if FStream.Write( SA[ 1 ], 1 ) = 0 then
                                raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
                    end
                    else begin
                        // Convert to unicode
                        SW := sdUtf8ToUnicode( FBuffer ) ;
                        // write out
                        if length( SW ) = 1 then
                            if FStream.Write( SW[ 1 ], 2 ) = 0 then
                                raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
                    end ;
                    FBuffer := '' ;
                end ;
            end ;
        seUTF8 : begin
                // Just a flat write of one byte
                if FStream.Write( B, 1 ) = 0 then
                    raise EStreamError.Create( sxeCannotWriteToOutputStream ) ;
            end ;
    else
        raise EStreamError.Create( sxeUnsupportedEncoding ) ;
    end ;
end ;

{ TsdBufferedReadStream }

const

    cMaxBufferSize  = $1000 ;                     // 4096 bytes in the buffer

procedure TsdBufferedReadStream.CheckPosition ;
var
    NewPage         : integer ;
    FStartPos       : longint ;
begin
    // Page and buffer position
    NewPage := FPosition div cMaxBufferSize ;
    FBufPos := FPosition mod cMaxBufferSize ;

    // Read new page if required
    if ( NewPage <> FPage ) then begin
        // New page and buffer
        FPage := NewPage ;

        // Start position in stream
        FStartPos := FPage * cMaxBufferSize ;
        FBufSize := Min( cMaxBufferSize, FStream.Size - FStartPos ) ;

        FStream.Seek( FStartPos, soFromBeginning ) ;
        if FBufSize > 0 then
            FStream.Read( FBuffer^, FBufSize ) ;
    end ;
    FMustCheck := False ;
end ;

constructor TsdBufferedReadStream.Create( AStream : TStream ; Owned : boolean ) ;
begin
    inherited Create ;
    FStream := AStream ;
    FOwned := Owned ;
    FMustCheck := True ;
    FPage := -1 ;                                 // Set to invalid number to force an update on first read
    ReallocMem( FBuffer, cMaxBufferSize ) ;
end ;

destructor TsdBufferedReadStream.Destroy ;
begin
    if FOwned then
        FreeAndNil( FStream ) ;
    ReallocMem( FBuffer, 0 ) ;
    inherited ;
end ;

function TsdBufferedReadStream.Read( var Buffer ; Count : Integer ) : Longint ;
var
    Packet          : PByte ;
    PacketCount     : integer ;
begin
    // Set the right page
    if FMustCheck then
        CheckPosition ;

    // Special case - read one byte, most often
    if ( Count = 1 ) and ( FBufPos < FBufSize - 1 ) then begin
        byte( Buffer ) := FBuffer^[ FBufPos ] ;
        inc( FBufPos ) ;
        inc( FPosition ) ;
        Result := 1 ;
        exit ;
    end ;

    // general case
    Packet := @Buffer ;
    Result := 0 ;
    while Count > 0 do begin
        PacketCount := min( FBufSize - FBufPos, Count ) ;
        if PacketCount <= 0 then
            exit ;
        Move( FBuffer^[ FBufPos ], Packet^, PacketCount ) ;
        dec( Count, PacketCount ) ;
        inc( Packet, PacketCount ) ;
        inc( Result, PacketCount ) ;
        inc( FPosition, PacketCount ) ;
        inc( FBufPos, PacketCount ) ;
        if FBufPos >= FBufSize then
            CheckPosition ;
    end ;
end ;

function TsdBufferedReadStream.Seek( Offset : Integer ; Origin : Word ) : Longint ;
begin
    case Origin of
        soFromBeginning :
            FPosition := Offset ;
        soFromCurrent : begin
                // no need to check in this case - it is the GetPosition command
                if Offset = 0 then begin
                    Result := FPosition ;
                    exit ;
                end ;
                FPosition := FPosition + Offset ;
            end ;
        soFromEnd :
            FPosition := FStream.Size + Offset ;
    end ;                                         //case
    Result := FPosition ;
    FMustCheck := True ;
end ;

function TsdBufferedReadStream.Write( const Buffer ; Count : Integer ) : Longint ;
begin
    raise EStreamError.Create( sxeCannotWriteCodecForReading ) ;
end ;

{ TsdBufferedWriteStream }

constructor TsdBufferedWriteStream.Create( AStream : TStream ;
    Owned : boolean ) ;
begin
    inherited Create ;
    FStream := AStream ;
    FOwned := Owned ;
    ReallocMem( FBuffer, cMaxBufferSize ) ;
end ;

destructor TsdBufferedWriteStream.Destroy ;
begin
    Flush ;
    if FOwned then
        FreeAndNil( FStream ) ;
    ReallocMem( FBuffer, 0 ) ;
    inherited ;
end ;

procedure TsdBufferedWriteStream.Flush ;
begin
    // Write the buffer to the stream
    if FBufPos > 0 then begin
        FStream.Write( FBuffer^, FBufPos ) ;
        FBufPos := 0 ;
    end ;
end ;

function TsdBufferedWriteStream.Read( var Buffer ; Count : Integer ) : Longint ;
begin
    raise EStreamError.Create( sxeCannotReadCodecForWriting ) ;
end ;

function TsdBufferedWriteStream.Seek( Offset : Integer ;
    Origin : Word ) : Longint ;
begin
    case Origin of
        soFromBeginning :
            if Offset = FPosition then begin
                Result := FPosition ;
                exit ;
            end ;
        soFromCurrent : begin
                // GetPosition command
                if Offset = 0 then begin
                    Result := FPosition ;
                    exit ;
                end ;
            end ;
        soFromEnd :
            if Offset = 0 then begin
                Result := FPosition ;
                exit ;
            end ;
    end ;                                         //case
    raise EStreamError.Create( sxeCannotPerformSeek ) ;
end ;

function TsdBufferedWriteStream.Write( const Buffer ;
    Count : Integer ) : Longint ;
var
    Packet          : PByte ;
    PacketCount     : integer ;
begin
    // Special case - read less bytes than would fill buffersize
    if ( FBufPos + Count < cMaxBufferSize ) then begin
        Move( Buffer, FBuffer^[ FBufPos ], Count ) ;
        inc( FBufPos, Count ) ;
        inc( FPosition, Count ) ;
        Result := Count ;
        exit ;
    end ;

    // general case that wraps buffer
    Packet := @Buffer ;
    Result := 0 ;
    while Count > 0 do begin
        PacketCount := min( cMaxBufferSize - FBufPos, Count ) ;
        if PacketCount <= 0 then
            exit ;
        Move( Packet^, FBuffer^[ FBufPos ], PacketCount ) ;
        dec( Count, PacketCount ) ;
        inc( Result, PacketCount ) ;
        inc( FPosition, PacketCount ) ;
        inc( Packet, PacketCount ) ;
        inc( FBufPos, PacketCount ) ;
        if FBufPos = cMaxBufferSize then
            Flush ;
    end ;
end ;

initialization

{$IFDEF TRIALXML}
    ShowMessage(
        'This is the unregistered version of XmlDocuments.pas'#13#13 +
        'Please visit http://www.simdesign.nl/xml.html to buy the'#13 +
        'registered version for $29.95 (source included).' ) ;
{$ENDIF}

end.

