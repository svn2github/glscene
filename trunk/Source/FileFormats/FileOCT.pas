{: FileOCT<p>

	Loader for FSRad OCT files.<p>

	<b>History : </b><font size=-1><ul>
	   <li>30/01/03 - Egg - Creation
	</ul></font>
}
unit FileOCT;

interface

uses Classes, Geometry;

type

   TOCTHeader = record
		numVerts : Integer;
		numFaces : Integer;
		numTextures : Integer;
		numLightmaps : Integer;
		numLights : Integer;
	end;

	TOCTVertex = record
		tv : TTexPoint;      // texture coordinates
		lv : TTexpoint;      // lightmap coordinates
		pos : TAffineVector; // vertex position
   end;

	TOCTFace = record
		start : Integer;     // first face vert in vertex array
		num : Integer;			// number of verts in the face
		id : Integer;			// texture index into the texture array
		lid : Integer;			// lightmap index into the lightmap array
		p : THmgPlane;
	end;
   POCTFace = ^TOCTFace;

	TOCTTexture = record
		id : Integer;				      // texture id
		Name : array [0..63] of Char;	// texture name
   end;

	TOCTLightmap = record
		id : Integer;				         // lightmaps id
		map : array [0..49151] of Byte;	// 128 x 128 raw RGB data
   end;
   POCTLightmap = ^TOCTLightmap;

	TOCTLight = record
		pos : TAffineVector;		      // Position
		color : TAffineVector;			// Color (RGB)
		intensity : Integer;			   // Intensity
	end;

   // TOCTFile
   //
   TOCTFile = class (TObject)
      public
         { Public Declarations }
         Header         : TOCTHeader;
         Vertices       : array of TOCTVertex;
         Faces          : array of TOCTFace;
         Textures       : array of TOCTTexture;
         Lightmaps      : array of TOCTLightmap;
         Lights         : array of TOCTLight;
         PlayerPos      : TAffineVector;

         constructor Create; overload;
         constructor Create(octStream : TStream); overload;

         {: Saves content to stream in OCT format.<p>
            The Header is automatically prepared before streaming. }
         procedure SaveToStream(aStream : TStream);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TOCTFile ------------------
// ------------------

// Create
//
constructor TOCTFile.Create;
begin
   inherited Create;
end;

// Create
//
constructor TOCTFile.Create(octStream : TStream);
begin
   inherited Create;
   
   // Read in the header
   octStream.Read(Header, SizeOf(Header));

   // then the rest of the stuff

   SetLength(Vertices, Header.numVerts);
   octStream.Read(Vertices[0], Header.numVerts*SizeOf(TOCTVertex));

   SetLength(Faces, Header.numFaces);
   octStream.Read(Faces[0], Header.numFaces*SizeOf(TOCTFace));

   SetLength(Textures, Header.numTextures);
   octStream.Read(Textures[0], Header.numTextures*SizeOf(TOCTTexture));

   SetLength(Lightmaps, Header.numLightmaps);
   octStream.Read(Lightmaps[0], Header.numLightmaps*SizeOf(TOCTLightmap));

   SetLength(Lights, Header.numLights);
   octStream.Read(Lights[0], Header.numLights*SizeOf(TOCTLight));

   octStream.Read(PlayerPos, SizeOf(PlayerPos))
end;

// SaveToStream
//
procedure TOCTFile.SaveToStream(aStream : TStream);
begin
   with Header, aStream do begin
      numVerts:=Length(Vertices);
      numFaces:=Length(Faces);
      numTextures:=Length(Textures);
      numLightmaps:=Length(Lightmaps);
      numLights:=Length(Lights);

      Write(Header, SizeOf(Header));
      Write(Vertices[0], numVerts*SizeOf(TOCTVertex));
      Write(Faces[0], numFaces*SizeOf(TOCTFace));
      Write(Textures[0], numTextures*SizeOf(TOCTTexture));
      Write(Lightmaps[0], numLightmaps*SizeOf(TOCTLightmap));
      Write(Lights[0], numLights*SizeOf(TOCTLight));
      Write(PlayerPos, SizeOf(PlayerPos))
   end;
end;

end.
