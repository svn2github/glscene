//
// This unit is part of the GLScene Project   
//
{: VKS.OutlineShader<p>

   A simple shader that adds an outline to an object. <p>

   Limitations: <br>
     <li> 1. Object can be transparent (color alpha < 1) if it doesn't
                   overlap itself. Texture transparency doesn't work.
     <li> 2. Doesn't work with objects (e.g. TVKFreeForm) having it's own
                   color array.
     <li> 3. Doesn't Works with visible backfaces.<p>

   <b>History : </b><font size=-1><ul>
      <li>12/02/11 - Yar - Added skipping shader when enabled stencil test to avvoid conflict with shadow volume
      <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after VKS.State revision
      <li>05/03/10 - DanB - More state added to TVKStateCache
      <li>06/06/07 - DaStr - Added $I GLScene.inc
                             Added VKS.Color to uses (BugtrackerID = 1732211)
      <li>25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>05/06/04 - NelC - Fixed bug with textured object
      <li>14/12/03 - NelC - Removed BlendLine, automatically determine if blend
      <li>20/10/03 - NelC - Removed unnecessary properties. Shader now honors
                            rci.ignoreMaterials.
      <li>04/09/03 - NelC - Converted into a component from the TOutlineShader
                            in the multipass demo.
   </ul></font>
}
unit VKS.OutlineShader;

interface

{$I VKScene.inc}

uses
  System.Classes,
  //VKS
  VKS.Material, VKS.CrossPlatform, VKS.Color, VKS.RenderContextInfo,
  VKS.OpenGLTokens, VKS.Context, VKS.State, VKS.TextureFormat;

type

  // TVKOutlineShader
  //
  TVKOutlineShader = class(TVKShader)
  private
    { Private Declarations }
    FPassCount: integer;
    FLineColor: TVKColor;
    FOutlineSmooth: Boolean;
    FOutlineWidth: Single;

    procedure SetOutlineWidth(v: single);
    procedure SetOutlineSmooth(v: boolean);

  protected
    { Protected Declarations }
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property LineColor: TVKColor read FLineColor write FLineColor;
    {: Line smoothing control }
    property LineSmooth: Boolean read FOutlineSmooth write SetOutlineSmooth
      default false;
    property LineWidth: Single read FOutlineWidth write SetOutlineWidth;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKOutlineShader ------------------
// ------------------

// Create
//

constructor TVKOutlineShader.Create(AOwner: TComponent);
begin
  inherited;
  FOutlineSmooth := False;
  FOutLineWidth := 2;
  FLineColor := TVKColor.CreateInitialized(Self, clrBlack);
  ShaderStyle := ssLowLevel;
end;

// Destroy
//

destructor TVKOutlineShader.Destroy;
begin
  FLineColor.Free;
  inherited;
end;

// DoApply
//

procedure TVKOutlineShader.DoApply(var rci: TRenderContextInfo; Sender:
  TObject);
begin
  // We first draw the object as usual in the first pass. This allows objects
  // with color alpha < 1 to be rendered correctly with outline.
  FPassCount := 1;
end;

// DoUnApply
//

function TVKOutlineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if rci.ignoreMaterials or (stStencilTest in rci.GLStates.States) then
  begin
    Result := False;
    Exit;
  end;
  case FPassCount of
    1:
      with rci.GLStates do
      begin
        // Now set up to draw the outline in the second pass

        Disable(stLighting);

        if FOutlineSmooth then
        begin
          LineSmoothHint := hintNicest;
          Enable(stLineSmooth);
        end
        else
          Disable(stLineSmooth);

        if FOutlineSmooth or (FlineColor.Alpha < 1) then
        begin
          Enable(stBlend);
          SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
        end
        else
          Disable(stBlend);

        GL.Color4fv(FlineColor.AsAddress);
        LineWidth := FOutlineWidth;
        Disable(stLineStipple);
        PolygonMode := pmLines;
        CullFaceMode := cmFront;
        DepthFunc := cfLEqual;
        ActiveTextureEnabled[ttTexture2D] := False;

        FPassCount := 2;
        Result := True; // go for next pass
      end;
    2:
      with rci.GLStates do
      begin
        // Restore settings
        PolygonMode := pmFill;
        CullFaceMode := cmBack;
        DepthFunc := cfLequal;
        Result := False; // we're done
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

// SetOutlineWidth
//

procedure TVKOutlineShader.SetOutlineWidth(v: single);
begin
  if FOutlineWidth <> v then
  begin
    FOutlineWidth := v;
    NotifyChange(self);
  end;
end;

// SetOutlineSmooth
//

procedure TVKOutlineShader.SetOutlineSmooth(v: boolean);
begin
  if FOutlineSmooth <> v then
  begin
    FOutlineSmooth := v;
    NotifyChange(self);
  end;
end;

end.

