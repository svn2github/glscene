unit GL3xFactory;

interface

{$I GLScene.inc}

uses
  Classes, BaseClasses, OpenGL1x, VectorLists, GLContext, GLRenderContextInfo,
  GLState, GLShadersManager, GLVBOManagers, GL3xMaterial, GLSLShader;

type

  TGLFactoryLaunching = (flSingle, flOnePerAtttribute);

  TGLFactoryState = (pfsProducing, pfsMapped);
  TGLFactoryStates = set of TGLFactoryState;

  // TGL3xBaseFactory
  //
  TGL3xBaseFactory = class(TComponent)
  private
    { Protected Declarations }
    FActive: Boolean;
    FLaunching: TGLFactoryLaunching;
    FStates: TGLFactoryStates;
    procedure SetActive(Value: Boolean);
    procedure SetLaunching(Value: TGLFactoryLaunching);
  protected
    { Protected Declarations }
    FProducedAttribute: TGLSLAttribute;
    procedure DoProduce(Sender: TObject); virtual; abstract;

    property Active: Boolean read FActive write SetActive default false;
    property Launching: TGLFactoryLaunching read FLaunching
      write SetLaunching default flSingle;
    property ProducedAttribute: TGLSLAttribute read FProducedAttribute;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Produce(Sender: TObject);
  end;

implementation

uses
  GLStrings;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xBaseFactory'}{$ENDIF}
// ------------------
// ------------------ TGL3xBaseFactory ------------------
// ------------------

// Create
//

constructor TGL3xBaseFactory.Create(AOwner: TComponent);
begin
  inherited;
  FActive := false;
  FProducedAttribute := nil;
end;

// Destroy
//

destructor TGL3xBaseFactory.Destroy;
begin
  inherited;
end;

// Produre
//

procedure TGL3xBaseFactory.Produce(Sender: TObject);
begin
  if FActive then
  begin
    Include(FStates, pfsProducing);
    DoProduce(Sender);
    Exclude(FStates, pfsProducing);
  end;
end;

// SetLaunching
//

procedure TGL3xBaseFactory.SetLaunching(Value: TGLFactoryLaunching);
begin
  if pfsProducing in FStates then
    exit;
  if Value<>FLaunching then
  begin
    FLaunching := Value;
  end;
end;

// SetActive
//

procedure TGL3xBaseFactory.SetActive(Value: Boolean);
begin
  if Value<>FActive then
  begin
    FActive := Value;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

end.
