//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLWideMultiBitmapFont<p>

	<b>History : </b><font size=-1><ul>
      <li>30/23/10 - Yar - Initial version (contributed to GLScene by Lampogolovii)
	</ul></font>
}

unit GLWideMultiBitmapFont;

interface

uses
  Classes,
  GLContext, GLRenderContextInfo, GLColor, GLMaterial, GLScene, GLCanvas,
  VectorGeometry, GLBitmapFont, GLCrossPlatform, ApplicationFileIO;

type

  // TUnicodeChar
  //
  TUnicodeChar = class
  public
    fUnicodeIndex: Integer;
    fCharRect: TVector;
    fCharWidth, fCharHeight: Integer;
    fMaterialIndex: Integer;
  end;

  // TGLWideMultiBitmapFont
  //
  TGLWideMultiBitmapFont = class(TGLCustomBitmapFont)
  protected
    fChars: array [0 .. 65535] of TUnicodeChar;
    fMaterials: TList;
    fGlyphsInterval: TVector; // between symbols in Materials
    fSpaces: TVector; // between symbols at rendering
  protected
    Function GetMaterialByIndex(const aIndex: Integer): TGlMaterial;
    Function CalcTextWidth(const aText: WideString): Integer;
    Function SizeOfChar: Int64;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure ClearMaterialsList;
    Procedure SaveCharsToFile(aFileName: String);
    Procedure LoadCharsFromFile(aFileName: String);
    Procedure LoadCharsFromStream(const aStream: TStream);
    Procedure AddSymbol(aSymbol: TUnicodeChar);
    Procedure FreeCharByIndex(aIndex: Integer);
    Procedure AddMaterial(aMaterial: TGlMaterial);
    Procedure RenderString(var rci: TRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment; layout: TGLTextLayout;
      const aColor: TColorVector; aPosition: PVector = nil;
      aReverseY: Boolean = False); override;

    function IsContainChar(const aChar: WideChar): Boolean;
    function CalcStringWidth(const aText: UnicodeString): Integer; override;
  end;

implementation

uses
  SysUtils, OpenGLTokens, GLTextureFormat, GlState;

// ------------------
// ------------------ TGLWideMultiBitmapFont ------------------
// ------------------

// Create
//

Constructor TGLWideMultiBitmapFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMaterials := TList.Create;
end;

Destructor TGLWideMultiBitmapFont.Destroy;
var
  i: Integer;
begin
  inherited;
  for i := 0 to 65535 do
    fChars[i].Free;
  fMaterials.Destroy;
end;

// GetMaterialByIndex
//

Function TGLWideMultiBitmapFont.GetMaterialByIndex(const aIndex: Integer): TGlMaterial;
begin
  result := TGlMaterial(fMaterials[aIndex]);
end;

// CalcTextWidth
//

Function TGLWideMultiBitmapFont.CalcTextWidth(const aText: WideString): Integer;
var
  i: Integer;
begin
  if aText <> '' then
  begin
    result := Round(Length(aText) * (fSpaces[0] - 1));
    for i := 1 to Length(aText) do
      if fChars[Word(aText[i])] <> nil then
        result := result + fChars[Word(aText[i])].fCharWidth;
  end
  else
    result := 0;
end;

// ClearMaterialsList
//

Procedure TGLWideMultiBitmapFont.ClearMaterialsList;
begin
  fMaterials.Clear;
end;

// SizeOfChar
//

Function TGLWideMultiBitmapFont.SizeOfChar: Int64;
var
  ch: TUnicodeChar;
begin
  with ch do
    result := sizeOf(fUnicodeIndex) + sizeOf(fCharRect) + sizeOf(fCharWidth)
      + sizeOf(fCharHeight) + sizeOf(fMaterialIndex);
end;

// SaveCharsToFile
//

Procedure TGLWideMultiBitmapFont.SaveCharsToFile(aFileName: String);
var
  i: Integer;
  fFile: TStream;
begin
  fFile := CreateFileStream(aFileName, fmCreate);
  try
    for i := 0 to 65535 do
      if fChars[i] <> nil then
        with fChars[i] do
        begin
          fFile.Write(i, sizeOf(fUnicodeIndex));
          fFile.Write(fCharRect, sizeOf(fCharRect));
          fFile.Write(fCharWidth, sizeOf(fCharWidth));
          fFile.Write(fChars[i].fCharHeight, sizeOf(fChars[i].fCharHeight));
          fFile.Write(fMaterialIndex, sizeOf(fMaterialIndex));
        end;
  finally
    fFile.Free;
  end;
end;

// LoadCharsFromFile
//

Procedure TGLWideMultiBitmapFont.LoadCharsFromFile(aFileName: String);
var
  fFile: TStream;
begin
  fFile := CreateFileStream(aFileName, fmOpenRead);
  try
    LoadCharsFromStream(fFile);
  finally
    fFile.Free;
  end;
end;

// LoadCharsFromStream
//

Procedure TGLWideMultiBitmapFont.LoadCharsFromStream(const aStream: TStream);
var
  i, count: Integer;
  ch: TUnicodeChar;
begin
  count := aStream.Size div SizeOfChar;
  for i := 0 to count - 1 do
  begin
    ch := TUnicodeChar.Create;
    with ch do
    begin
      aStream.Read(fUnicodeIndex, sizeOf(fUnicodeIndex));
      aStream.Read(fCharRect, sizeOf(fCharRect));
      aStream.Read(fCharWidth, sizeOf(fCharWidth));
      aStream.Read(ch.fCharHeight, sizeOf(ch.fCharHeight));
      aStream.Read(fMaterialIndex, sizeOf(fMaterialIndex));
      if ch.fCharHeight > CharHeight then
        CharHeight := ch.fCharHeight;
    end;
    AddSymbol(ch);
  end;
end;

// AddSymbol
//

procedure TGLWideMultiBitmapFont.AddSymbol(aSymbol: TUnicodeChar);
begin
  if fChars[aSymbol.fUnicodeIndex] <> nil then
    FreeCharByIndex(aSymbol.fUnicodeIndex);
  fChars[aSymbol.fUnicodeIndex] := aSymbol;
end;

// FreeCharByIndex
//

Procedure TGLWideMultiBitmapFont.FreeCharByIndex(aIndex: Integer);
begin
  FreeAndNil(fChars[aIndex]);
end;

// AddMaterial
//

Procedure TGLWideMultiBitmapFont.AddMaterial(aMaterial: TGlMaterial);
begin
  fMaterials.Add(aMaterial);
end;

// RenderString
//

Procedure TGLWideMultiBitmapFont.RenderString(var rci: TRenderContextInfo;
  const aText: UnicodeString; aAlignment: TAlignment; layout: TGLTextLayout;
  const aColor: TColorVector; aPosition: PVector = nil;
  aReverseY: Boolean = False);

  function AlignmentAdjustement(p: Integer): Single;
  var
    i: Integer;
  begin
    i := 0;
    while (p <= Length(aText)) and (aText[p] <> #13) do
    begin
      Inc(p);
      Inc(i);
    end;
    case aAlignment of
      taLeftJustify:
        result := 0;
      taRightJustify:
        result := -CalcTextWidth(Copy(aText, p - i, i))
      else // taCenter
        result := Round(-CalcTextWidth(Copy(aText, p - i, i)) * 0.5);
    end;
  end;

  function LayoutAdjustement: Single;
  var
    i, n: Integer;
  begin
    n := 1;
    for i := 1 to Length(aText) do
      if aText[i] = #13 then
        Inc(n);
    case TGLTextLayout(layout) of
      tlTop:
        result := -CharHeight;
      tlBottom:
        result := (n * (CharHeight + VSpace) - VSpace);
    else // tlCenter
      result := Round((n * (CharHeight + VSpace) - VSpace) * 0.5);
    end;
  end;

var
  wChar: WideChar;
  i: Integer;
  BindedIndex: Integer;
  UniChar: TUnicodeChar;
  vTopLeft, vBottomRight: TVector; // screen coords
  TopLeft, BottomRight: TTexPoint; // Material coords
begin
  BindedIndex := -1;

  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

  if Assigned(aPosition) then
    MakePoint(vTopLeft, aPosition[0] + AlignmentAdjustement(1),
      aPosition[1] + LayoutAdjustement, 0)
  else
    MakePoint(vTopLeft, AlignmentAdjustement(1), LayoutAdjustement, 0);

  vBottomRight[2] := 0;
  vBottomRight[3] := 1;
  GL.Begin_(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    wChar := aText[i];
    UniChar := fChars[Word(wChar)];
    if UniChar <> nil then
    begin
      if (BindedIndex <> fChars[Word(wChar)].fMaterialIndex) then
      begin
        GL.End_;
        if (BindedIndex >= 0) and (BindedIndex < fMaterials.count) then
          GetMaterialByIndex(BindedIndex).UnApply(rci);

        BindedIndex := fChars[Word(wChar)].fMaterialIndex;
        with GetMaterialByIndex(BindedIndex) do
        begin
          FrontProperties.Diffuse.Alpha := aColor[3];
          Apply(rci);
        end;
        GL.Begin_(GL_QUADS);
      end;

      { from TGLCustomBitmapFont.RenderString Start }
      // copy values of Material coordinates
      TopLeft.S := UniChar.fCharRect[0];
      TopLeft.T := UniChar.fCharRect[1];
      BottomRight.S := UniChar.fCharRect[2];
      BottomRight.T := UniChar.fCharRect[3];

      vBottomRight[0] := vTopLeft[0] + UniChar.fCharWidth;
      vBottomRight[1] := vTopLeft[1] + UniChar.fCharHeight;

      GL.TexCoord2fv(@TopLeft);
      GL.Vertex2f(vTopLeft[0], vTopLeft[1]);
      GL.TexCoord2f(BottomRight.S, TopLeft.T);
      GL.Vertex2f(vBottomRight[0], vTopLeft[1]);
      GL.TexCoord2fv(@BottomRight);
      GL.Vertex2f(vBottomRight[0], vBottomRight[1]);
      GL.TexCoord2f(TopLeft.S, BottomRight.T);
      GL.Vertex2f(vTopLeft[0], vBottomRight[1]);
      // increase offset
      vTopLeft[0] := vTopLeft[0] + UniChar.fCharWidth + fSpaces[0];
    end;
  end;
  GL.End_;

  if (BindedIndex >= 0) and (BindedIndex < fMaterials.count) then
    GetMaterialByIndex(BindedIndex).UnApply(rci);
end;

// CalcStringWidth
//

function TGLWideMultiBitmapFont.CalcStringWidth(const aText: UnicodeString): Integer;
var
  i: Integer;
  wChar: WideChar;
  UniChar: TUnicodeChar;
begin
  result := 0;
  for i := 1 to Length(aText) do
  begin
    wChar := aText[i];
    UniChar := fChars[Word(wChar)];
    if UniChar <> nil then
      result := result + UniChar.fCharWidth;
  end;
end;

// IsContainChar
//

function TGLWideMultiBitmapFont.IsContainChar(const aChar: WideChar): Boolean;
begin
  result := fChars[Word(aChar)] <> nil;
end;

end.
