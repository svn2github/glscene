//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   A shader that allows texture combiner setup. 
               
}
unit GLS.TexCombineShader;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  //GLS
  GLS.Texture,
  GLS.Material,
  GLS.RenderContextInfo,
  GLS.TextureCombiners,
  GLS.OpenGLTokens,
  GLS.XOpenGL,
  GLS.Context,
  GLS.CrossPlatform,
  GLS.Utils;

type

  // TVKTexCombineShader
  //
  { A shader that can setup the texture combiner.  }
  TVKTexCombineShader = class(TVKShader)
  private
    { Protected Declarations }
    FCombiners: TStringList;
    FCommandCache: TCombinerCache;
    FCombinerIsValid: Boolean; // to avoid reparsing invalid stuff
    FDesignTimeEnabled: Boolean;

    FMaterialLibrary: TVKMaterialLibrary;
    FLibMaterial3Name: TVKLibMaterialName;
    currentLibMaterial3: TVKLibMaterial;
    FLibMaterial4Name: TVKLibMaterialName;
    currentLibMaterial4: TVKLibMaterial;

    FApplied3, FApplied4: Boolean;

  protected
    { Protected Declarations }
    procedure SetCombiners(const val: TStringList);
    procedure SetDesignTimeEnabled(const val: Boolean);
    procedure SetMaterialLibrary(const val: TVKMaterialLibrary);
    procedure SetLibMaterial3Name(const val: TVKLibMaterialName);
    procedure SetLibMaterial4Name(const val: TVKLibMaterialName);

    procedure NotifyLibMaterial3Destruction;
    procedure NotifyLibMaterial4Destruction;

    procedure DoInitialize(var rci: TVKRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TVKRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChange(Sender: TObject); override;

  published
    { Published Declarations }
    property Combiners: TStringList read FCombiners write SetCombiners;
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;

    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterial3Name: TVKLibMaterialName read FLibMaterial3Name write SetLibMaterial3Name;
    property LibMaterial4Name: TVKLibMaterialName read FLibMaterial4Name write SetLibMaterial4Name;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKTexCombineShader ------------------
// ------------------

// Create
//

constructor TVKTexCombineShader.Create(AOwner: TComponent);
begin
  inherited;
  ShaderStyle := ssLowLevel;
  FCombiners := TStringList.Create;
  TStringList(FCombiners).OnChange := NotifyChange;
  FCombinerIsValid := True;
  FCommandCache := nil;
end;

// Destroy
//

destructor TVKTexCombineShader.Destroy;
begin
  if Assigned(currentLibMaterial3) then
    currentLibMaterial3.UnregisterUser(Self);
  if Assigned(currentLibMaterial4) then
    currentLibMaterial4.UnregisterUser(Self);
  inherited;
  FCombiners.Free;
end;

// Notification
//

procedure TVKTexCombineShader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (FMaterialLibrary = AComponent) and (Operation = opRemove) then
  begin
    NotifyLibMaterial3Destruction;
    NotifyLibMaterial4Destruction;
    FMaterialLibrary := nil;
  end;
  inherited;
end;

// NotifyChange
//

procedure TVKTexCombineShader.NotifyChange(Sender: TObject);
begin
  FCombinerIsValid := True;
  FCommandCache := nil;
  inherited NotifyChange(Sender);
end;

// NotifyLibMaterial3Destruction
//

procedure TVKTexCombineShader.NotifyLibMaterial3Destruction;
begin
  FLibMaterial3Name := '';
  currentLibMaterial3 := nil;
end;

// NotifyLibMaterial4Destruction
//

procedure TVKTexCombineShader.NotifyLibMaterial4Destruction;
begin
  FLibMaterial4Name := '';
  currentLibMaterial4 := nil;
end;

// SetMaterialLibrary
//

procedure TVKTexCombineShader.SetMaterialLibrary(const val: TVKMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterial3Name(LibMaterial3Name);
  SetLibMaterial4Name(LibMaterial4Name);
end;

// SetLibMaterial3Name
//

procedure TVKTexCombineShader.SetLibMaterial3Name(const val: TVKLibMaterialName);
var
  newLibMaterial: TVKLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(val)
  else
    newLibMaterial := nil;
  FLibMaterial3Name := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial3 then
  begin
    // unregister from old
    if Assigned(currentLibMaterial3) then
      currentLibMaterial3.UnregisterUser(Self);
    currentLibMaterial3 := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial3) then
      currentLibMaterial3.RegisterUser(Self);
    NotifyChange(Self);
  end;
end;

// SetLibMaterial4Name
//

procedure TVKTexCombineShader.SetLibMaterial4Name(const val: TVKLibMaterialName);
var
  newLibMaterial: TVKLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(val)
  else
    newLibMaterial := nil;
  FLibMaterial4Name := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial4 then
  begin
    // unregister from old
    if Assigned(currentLibMaterial4) then
      currentLibMaterial4.UnregisterUser(Self);
    currentLibMaterial4 := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial4) then
      currentLibMaterial4.RegisterUser(Self);
    NotifyChange(Self);
  end;
end;

// DoInitialize
//

procedure TVKTexCombineShader.DoInitialize(var rci: TVKRenderContextInfo; Sender: TObject);
begin
end;

// DoApply
//

procedure TVKTexCombineShader.DoApply(var rci: TVKRenderContextInfo; Sender: TObject);
var
  n, units: Integer;
begin
  if not GL.ARB_multitexture then
    Exit;
  FApplied3 := False;
  FApplied4 := False;
  if FCombinerIsValid and (FDesignTimeEnabled or (not (csDesigning in ComponentState))) then
  begin
    try
      if Assigned(currentLibMaterial3) or Assigned(currentLibMaterial4) then
      begin
        glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @n);
        units := 0;
        if Assigned(currentLibMaterial3) and (n >= 3) then
        begin
          with currentLibMaterial3.Material.Texture do
          begin
            if Enabled then
            begin
              if currentLibMaterial3.TextureMatrixIsIdentity then
                ApplyAsTextureN(3, rci)
              else
                ApplyAsTextureN(3, rci, @currentLibMaterial3.TextureMatrix.V[0].V[0]);
              //                     ApplyAsTextureN(3, rci, currentLibMaterial3);
              Inc(units, 4);
              FApplied3 := True;
            end;
          end;
        end;
        if Assigned(currentLibMaterial4) and (n >= 4) then
        begin
          with currentLibMaterial4.Material.Texture do
          begin
            if Enabled then
            begin
              if currentLibMaterial4.TextureMatrixIsIdentity then
                ApplyAsTextureN(4, rci)
              else
                ApplyAsTextureN(4, rci, @currentLibMaterial4.TextureMatrix.V[0].V[0]);
              //                     ApplyAsTextureN(4, rci, currentLibMaterial4);
              Inc(units, 8);
              FApplied4 := True;
            end;
          end;
        end;
        if units > 0 then
          xgl.MapTexCoordToArbitraryAdd(units);
      end;

      if Length(FCommandCache) = 0 then
        FCommandCache := GetTextureCombiners(FCombiners);
      for n := 0 to High(FCommandCache) do
      begin
        rci.GLStates.ActiveTexture := FCommandCache[n].ActiveUnit;
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        glTexEnvi(GL_TEXTURE_ENV, FCommandCache[n].Arg1, FCommandCache[n].Arg2);
      end;
      rci.GLStates.ActiveTexture := 0;
    except
      on E: Exception do
      begin
        FCombinerIsValid := False;
        InformationDlg(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

// DoUnApply
//

function TVKTexCombineShader.DoUnApply(var rci: TVKRenderContextInfo): Boolean;
begin
  if FApplied3 then
    with currentLibMaterial3.Material.Texture do
      UnApplyAsTextureN(3, rci, (not currentLibMaterial3.TextureMatrixIsIdentity));
  if FApplied4 then
    with currentLibMaterial4.Material.Texture do
      UnApplyAsTextureN(4, rci, (not currentLibMaterial4.TextureMatrixIsIdentity));
  Result := False;
end;

// DoFinalize
//

procedure TVKTexCombineShader.DoFinalize;
begin
end;

// SetCombiners
//

procedure TVKTexCombineShader.SetCombiners(const val: TStringList);
begin
  if val <> FCombiners then
  begin
    FCombiners.Assign(val);
    NotifyChange(Self);
  end;
end;

// SetDesignTimeEnabled
//

procedure TVKTexCombineShader.SetDesignTimeEnabled(const val: Boolean);
begin
  if val <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := val;
    NotifyChange(Self);
  end;
end;

end.

