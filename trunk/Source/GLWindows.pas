  // In GL windows management
{: In GL windows management classes and structures<p>

	<b>History : </b><font size=-1><ul>
      <li>24/05/02 - JAJ - Base Unit built on basis of Jan Horn's demo at http://www.sulaco.co.za (http://www.sulaco.co.za/opengl/windows.zip)
      <li>01/06/02 - JAJ - After not having received Jan Horn's blessing, the system have been revised all parts have been rewritten.
	</ul></font>
}

unit GLWindows;

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, GLMisc, GLScene, GLHudObjects, GLTexture, OpenGL12, GLBitmapFont, GLWindowsFont, stdctrls, geometry, controls, GLGui;

type

  TGLBaseComponent = class(THUDSprite)
  private
    FGUIRedraw : Boolean;
    FGuiLayout     : TGLGuiLayout;
    FGuiLayoutName : TGLGuiComponentName;
    FGuiComponent  : TGLGuiComponent;
    FReBuildGui    : Boolean;
    FRedrawAtOnce : Boolean;
    MoveX, MoveY : TGLFloat;
    FRenderStatus  : TGUIDrawResult;

    procedure SetGUIRedraw(value : Boolean);
  protected
    Procedure RenderHeader(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
    Procedure RenderFooter(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

    procedure SetGuiLayout(NewGui : TGLGuiLayout); Virtual;
    procedure SetGuiLayoutName(NewName : TGLGuiComponentName);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public

    procedure NotifyChange(Sender : TObject); override;
    Procedure DoChanges; virtual;
    Procedure MoveGUI(XRel, YRel : Single);
    procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    Procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); Virtual;
    property  GUIRedraw : Boolean read FGUIRedraw write SetGUIRedraw;
    property  ReBuildGui : Boolean read FReBuildGui write FReBuildGui;
  published
    property  RedrawAtOnce : Boolean read FRedrawAtOnce write FRedrawAtOnce;
    property  GuiLayout     : TGLGuiLayout           read FGuiLayout     write SetGuiLayout;
    property  GuiLayoutName : TGLGuiComponentName    read FGuiLayoutName write SetGuiLayoutName;
  End;

  TGLFocusControl = class;

  TGLBaseControl = class(TGLBaseComponent)
  private
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp  : TMouseEvent;
    FKeepMouseEvents  : Boolean;
    FActiveControl    : TGLBaseControl;
    FFocusedControl   : TGLFocusControl;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); Virtual;
    procedure InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); Virtual;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); Virtual;
    Procedure SetActiveControl(NewControl : TGLBaseControl);
    Procedure SetFocusedControl(NewControl : TGLFocusControl);
  public
    Function  MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Function  MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Function  MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Procedure KeyPress(Sender: TObject; var Key: Char); virtual;
    Procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    Procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    property  ActiveControl : TGLBaseControl  read FActiveControl write SetActiveControl;
  published
    property  FocusedControl  : TGLFocusControl read FFocusedControl  write SetFocusedControl;
    property  OnMouseDown     : TMouseEvent     read FOnMouseDown     write FOnMouseDown;
    property  OnMouseMove     : TMouseMoveEvent read FOnMouseMove     write FOnMouseMove;
    property  OnMouseUp       : TMouseEvent     read FOnMouseUp       write FOnMouseUp;
    property  KeepMouseEvents : Boolean         read FKeepMouseEvents write FKeepMouseEvents default false;
  End;

  TGLBaseFontControl = class(TGLBaseControl)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FDefaultColor    : TColorVector;
  protected
    Function  GetDefaultColor : TColor;
    procedure SetDefaultColor(value : TColor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetBitmapFont(NewFont : TGLCustomBitmapFont);
    Procedure WriteTextAt(Const X,Y : TGLFloat; Const Data : String; const Color : TColorVector); overload;
    Procedure WriteTextAt(Const X1,Y1,X2,Y2 : TGLFloat; Const Data : String; const Color : TColorVector); overload;
    Function  GetFontHeight : Integer;
  public
    Destructor Destroy; override;
  published
    property BitmapFont : TGLCustomBitmapFont read FBitmapFont write SetBitmapFont;
    property DefaultColor : TColor read GetDefaultColor write SetDefaultColor;
  end;

  TGLBaseTextControl = class(TGLBaseFontControl)
  private
    FCaption  : String;
  protected
    Procedure SetCaption(NewCaption : String);
  public
  published
    property  Caption : String read FCaption write SetCaption;
  end;

  TGLFocusControl = class(TGLBaseTextControl)
  private
    FRootControl      : TGLBaseControl;
    FFocused          : Boolean;
    FOnKeyDown        : TKeyEvent;
    FOnKeyUp          : TKeyEvent;
    FOnKeyPress       : TKeyPressEvent;
    FShiftState       : TShiftState;
  protected
    Procedure InternalKeyPress(var Key: Char); virtual;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); virtual;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); virtual;
    Procedure SetFocused(Value :Boolean); virtual;
    Function  GetRootControl : TGLBaseControl;
  public
    Procedure SetFocus;
    Procedure PrevControl;
    Procedure NextControl;
    Procedure KeyPress(Sender: TObject; var Key: Char); override;
    Procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    Procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  published
    property  RootControl    : TGLBaseControl   read GetRootControl;
    property  Focused        : Boolean          read FFocused    write SetFocused;
    property  OnKeyDown      : TKeyEvent        read FOnKeyDown  write FOnKeyDown;
    property  OnKeyUp        : TKeyEvent        read FOnKeyUp    write FOnKeyUp;
    property  OnKeyPress     : TKeyPressEvent   read FOnKeyPress write FOnKeyPress;
  End;

  TGLFormCanRequest   = procedure (Sender: TObject; var Can: Boolean) of Object;
  TGLFormCloseOptions = (co_Hide, co_Ignore, co_Destroy);
  TGLFormCanClose     = procedure (Sender: TObject; var CanMove: TGLFormCloseOptions) of Object;

  TGLForm = class(TGLBaseTextControl)
  private
    FOnCanMove     : TGLFormCanRequest;
    FOnCanResize   : TGLFormCanRequest;
    FOnCanClose    : TGLFormCanClose;
    Moving         : Boolean;
    Resized        : Boolean;
    OldX           : Integer;
    OldY           : Integer;
    FTitleColor    : TColorVector;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    Function  GetTitleColor : TColor;
    procedure SetTitleColor(value : TColor);
  public
    Constructor Create(AOwner : TComponent); override;

    Function  MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    Function  MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property  TitleColor    : TColor                 read GetTitleColor  write SetTitleColor;
    property  OnCanMove     : TGLFormCanRequest      read FOnCanMove     write FOnCanMove;
    property  OnCanResize   : TGLFormCanRequest      read FOnCanResize   write FOnCanResize;
    property  OnCanClose    : TGLFormCanClose        read FOnCanClose    write FOnCanClose;
  end;

  TGLPanel = class(TGLBaseComponent)
  end;

  TGLCheckBox = class(TGLBaseControl)
  private
    FChecked : Boolean;
    FOnChange : TNotifyEvent;
    FGuiLayoutNameChecked : TGLGuiComponentName;
    FGuiCheckedComponent  : TGLGuiComponent;
  protected
    Procedure SetChecked(NewChecked : Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    procedure SetGuiLayoutNameChecked(newName : TGLGuiComponentName);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property  Checked : Boolean read FChecked write SetChecked;
    property  OnChange : TNotifyEvent read FOnChange write FOnChange;
    property  GuiLayoutNameChecked : TGLGuiComponentName read FGuiLayoutNameChecked Write SetGuiLayoutNameChecked;
  end;

  TGLButton = class(TGLFocusControl)
  private
    FPressed : Boolean;
    FOnButtonClick : TNotifyEvent;
    FGuiLayoutNamePressed : TGLGuiComponentName;
    FGuiPressedComponent  : TGLGuiComponent;
  protected
    Procedure SetPressed(NewPressed : Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    Procedure SetFocused(Value :Boolean); override;
    procedure SetGuiLayoutNamePressed(newName : TGLGuiComponentName);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property Pressed : Boolean read FPressed write SetPressed;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property  GuiLayoutNamePressed : TGLGuiComponentName read FGuiLayoutNamePressed Write SetGuiLayoutNamePressed;
  end;

  TGLEdit = class(TGLFocusControl)
  private
    FOnChange : TNotifyEvent;
    FSelStart  : Integer;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer); override;
    Procedure InternalKeyPress(var Key: Char); override;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    Procedure SetFocused(Value :Boolean); override;
    Procedure SetSelStart(Value : Integer);
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property  OnChange : TNotifyEvent read FOnChange write FOnChange;
    property  SelStart : Integer read FSelStart write SetSelStart;
  end;

  TGLLabel = class(TGLBaseTextControl)
  private
  protected
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
  end;

  TGLAdvancedLabel = class(TGLFocusControl)
  private
  protected
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
  end;

Const
  NormalColor : TColorVector = (1,    1,    1,    1);
  FocusedColor : TColorVector = (1,        1,        0,        1);


implementation

uses GLObjects;

procedure TGLBaseComponent.SetGUIRedraw(value : Boolean);

Begin
  FGUIRedraw := Value;
  If Value then
  Begin
    if FRedrawAtOnce then
    Begin
      FGUIRedraw := False;
      StructureChanged;
    End;
  End;
End;


Procedure TGLBaseComponent.RenderHeader(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  f : Single;
Begin
  	Material.Apply(rci);
   if AlphaChannel<>1 then
      SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
   // Prepare matrices
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
   if rci.renderDPI=96 then
      f:=1
   else f:=rci.renderDPI/96;
   glScalef(f*2/rci.viewPortSize.cx, f*2/rci.viewPortSize.cy, 1);
   glTranslatef(f*Position.X-rci.viewPortSize.cx*0.5,
                rci.viewPortSize.cy*0.5-f*Position.Y, Position.Z);
   if Rotation<>0 then
      glRotatef(Rotation, 0, 0, 1);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_DEPTH_TEST);
   glDepthMask(False);
End;

Procedure TGLBaseComponent.RenderFooter(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
   glDepthMask(True);
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
   Material.UnApply(rci);
End;


procedure TGLBaseComponent.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  If FGuiLayout <> NewGui then
  Begin
    If Assigned(FGuiLayout) then
    Begin
      FGuiLayout.RemoveGuiComponent(Self);
    End;
    FGuiComponent := Nil;
    FGuiLayout := NewGui;
    If Assigned(FGuiLayout) then
    Begin
      FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      If Assigned(FGuiLayout) then
      Begin
        FGuiLayout.AddGuiComponent(Self);
      End;
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLBaseComponent.SetGuiLayoutName(NewName : TGLGuiComponentName);

Begin
  If FGuiLayoutName <> NewName then
  Begin
    FGuiComponent := Nil;
    FGuiLayoutName := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLBaseComponent.Notification(AComponent: TComponent; Operation: TOperation);

Begin
  inherited;
End;

procedure TGLBaseComponent.NotifyChange(Sender : TObject);

Begin
  If Sender = FGuiLayout then
  Begin
    If (FGuiLayoutName <> '') and (GuiLayout <> Nil) then
    Begin
      FGuiComponent := GuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      ReBuildGui := True;
      GUIRedraw := True;
    End else
    Begin
      FGuiComponent := Nil;
      ReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
  inherited;
End;

Procedure TGLBaseComponent.MoveGUI(XRel, YRel : Single);

Var
  XC : Integer;

Begin
  If RedrawAtOnce then
  Begin
    BeginUpdate;
    try
      MoveX := MoveX + XRel;
      MoveY := MoveY + YRel;
      For XC := 0 to Count -1 do
      If Children[XC] is TGLBaseComponent then
      Begin
        (Children[XC] as TGLBaseComponent).MoveGUI(XRel,YRel);
      End;
      GUIRedraw := True;
      DoChanges;
    finally
      Endupdate;
    End;
  End else
  Begin
    MoveX := MoveX + XRel;
    MoveY := MoveY + YRel;
    For XC := 0 to Count -1 do
    If Children[XC] is TGLBaseComponent then
    Begin
      (Children[XC] as TGLBaseComponent).MoveGUI(XRel,YRel);
    End;
    GUIRedraw := True;
  End;
End;

Procedure TGLBaseComponent.DoChanges;

Var
  XC : Integer;

Begin
  If GUIRedraw then
  Begin
    GUIRedraw := False;
    BeginUpdate;
    try
      If MoveX <> 0 then Position.X := Position.X + MoveX;
      If MoveY <> 0 then Position.Y := Position.Y + MoveY;
      MoveX := 0;
      MoveY := 0;

      For XC := 0 to Count-1 do
      If Children[XC] is TGLBaseComponent then
      Begin
        (Children[XC] as TGLBaseComponent).DoChanges;
      End;
    finally
      EndUpdate;
    End;
  End else
  Begin
    For XC := 0 to Count-1 do
    If Children[XC] is TGLBaseComponent then
    Begin
      (Children[XC] as TGLBaseComponent).DoChanges;
    End;
  End;
End;

Procedure TGLBaseComponent.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
  If Assigned(FGuiComponent) then
  Begin
    FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
  End;
End;

procedure TGLBaseComponent.DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
  If RenderSelf then
  Begin
    RenderHeader(rci,renderSelf,renderChildren);

    InternalRender(rci,RenderSelf,RenderChildren);

    RenderFooter(rci,renderSelf,renderChildren);
    FReBuildGui := False;
  End;

  If renderChildren then
  if Count>0 then
    Self.RenderChildren(0, Count-1, rci);
End;

procedure TGLBaseControl.InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Begin
  If Assigned(FOnMouseDown) then FOnMouseDown(Self,Button,Shift,X,Y);
End;

procedure TGLBaseControl.InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Begin
  If Assigned(FOnMouseUp) then FOnMouseUp(Self,Button,Shift,X,Y);
End;

procedure TGLBaseControl.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

Begin
  If Assigned(FOnMouseMove) then FOnMouseMove(Self,Shift,X,Y);
End;

Procedure TGLBaseControl.SetActiveControl(NewControl : TGLBaseControl);

Begin
  FActiveControl := NewControl;
End;

Procedure TGLBaseControl.SetFocusedControl(NewControl : TGLFocusControl);

Begin
  If Assigned(FFocusedControl) then
    FFocusedControl.Focused := False;
  FFocusedControl := NewControl;
  If Assigned(FFocusedControl) then
    FFocusedControl.Focused := True;
End;

Function  TGLBaseControl.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
Begin
  Result := False;
  If (Position.X <= X) and (Position.X+Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y) then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseDown(Sender,Button,Shift,X,Y) then Exit;

      For XC := 0 to count-1 do
      If FActiveControl <> Children[XC] then
      Begin
        If Children[XC] is TGLBaseControl then
        Begin
          If (Children[XC] as TGLBaseControl).MouseDown(Sender,button,shift,x,y) then Exit;
        End;
      End;
    End;
    InternalMouseDown(Shift,Button,X,Y);
  End;
End;

Function  TGLBaseControl.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
Begin
  Result := False;
  If (Position.X <= X) and (Position.X+Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y) then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseUp(Sender,button,shift,x,y) then Exit;

      For XC := 0 to count-1 do
      If FActiveControl <> Children[XC] then
      Begin
        If Children[XC] is TGLBaseControl then
        Begin
          If (Children[XC] as TGLBaseControl).MouseUp(Sender,button,shift,x,y) then Exit;
        End;
      End;
    End;
    InternalMouseUp(Shift,Button,X,Y);
  End;
End;

Function  TGLBaseControl.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
Begin
  Result := False;
  If (Position.X <= X) and (Position.X+Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y) then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseMove(Sender,shift,x,y) then Exit;

      For XC := 0 to count-1 do
      If FActiveControl <> Children[XC] then
      Begin
        If Children[XC] is TGLBaseControl then
        Begin
          If (Children[XC] as TGLBaseControl).MouseMove(Sender,shift,x,y) then
            Exit;
        End;
      End;
    End;
    InternalMouseMove(Shift,X,Y);
  End;
End;

Procedure   TGLBaseControl.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyDown(Sender,Key,Shift);
  End;
End;

Procedure   TGLBaseControl.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyUp(Sender,Key,Shift);
  End;
End;

Procedure   TGLBaseControl.KeyPress(Sender: TObject; var Key: Char);

Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyPress(Sender,Key);
  End;
End;

Procedure TGLFocusControl.InternalKeyPress(var Key: Char);
Begin
  if assigned(FOnKeyPress) then FOnKeyPress(Self,Key);
End;

Procedure TGLFocusControl.InternalKeyDown(var Key: Word; Shift: TShiftState);
Begin
  if assigned(FOnKeyDown) then FOnKeyDown(Self,Key,shift);
End;

Procedure TGLFocusControl.InternalKeyUp(var Key: Word; Shift: TShiftState);
Begin
  if assigned(FOnKeyUp) then FOnKeyUp(Self,Key,shift);
End;

Procedure TGLFocusControl.SetFocused(Value :Boolean);
Begin
  If Value <> FFocused then
  Begin
    FFocused := Value;
    GUIRedraw :=True;
  End;
End;

Function  TGLFocusControl.GetRootControl : TGLBaseControl;

Var
  TmpRoot : TGLBaseComponent;

Begin
  if Assigned(FRootControl) then
  Begin
    Result := FRootControl;
  End else
  Begin
    FRootControl := Self;

    TmpRoot := Self;
    While (TmpRoot is TGLBaseComponent) do
    Begin
      If Assigned(TmpRoot.parent) then
      Begin
        If TmpRoot.parent is TGLBaseComponent then
        Begin
          TmpRoot := TmpRoot.parent as TGLBaseComponent;
          If TmpRoot is TGLBaseControl then FRootControl := TmpRoot as TGLBaseControl;
        End else Break;
      End else Break;
    End;
    Result := FRootControl;
  End;
End;

Procedure TGLFocusControl.SetFocus;

Begin
  RootControl.FocusedControl := Self;
End;

Procedure TGLFocusControl.NextControl;

Var
  Host : TGLBaseComponent;
  Index : Integer;

Begin
  If Parent is TGLBaseComponent then
  Begin
    Host := Parent as TGLBaseComponent;
    Index := Host.IndexOfChild(Self);
    While true do
    Begin
      If Index > 0 then
      Begin
        Dec(Index);
        If Host.Children[Index] is TGLFocusControl then
        Begin
          (Host.Children[Index] as TGLFocusControl).SetFocus;
          Exit;
        End else
        Begin
          If Host.Children[Index] is TGLBaseComponent then
          Begin
            Host := Host.Children[Index] as TGLBaseComponent;
            Index := Host.Count;
          End;
        End;
      End else
      Begin
        If Host.Parent is TGLBaseComponent then
        Begin
          Index := Host.Parent.IndexOfChild(Host);
          Host := Host.Parent as TGLBaseComponent;
        End else
        Begin
          Index := Host.Count;
        End;
      End;
    End;
  End;
End;

Procedure TGLFocusControl.PrevControl;

Var
  Host : TGLBaseComponent;
  Index : Integer;

Begin
  If Parent is TGLBaseComponent then
  Begin
    Host := Parent as TGLBaseComponent;
    Index := Host.IndexOfChild(Self);
    While true do
    Begin
      Inc(Index);

      If Index < Host.Count then
      Begin
        If Host.Children[Index] is TGLFocusControl then
        Begin
          (Host.Children[Index] as TGLFocusControl).SetFocus;
          Exit;
        End else
        Begin
          If Host.Children[Index] is TGLBaseComponent then
          Begin
            Host := Host.Children[Index] as TGLBaseComponent;
            Index := -1;
          End;
        End;
      End else
      Begin
        If Host.Parent is TGLBaseComponent then
        Begin
          Index := Host.Parent.IndexOfChild(Host);
          Host := Host.Parent as TGLBaseComponent;
        End else
        Begin
          Index := -1;
        End;
      End;
    End;
  End;
End;

Procedure TGLFocusControl.KeyPress(Sender: TObject; var Key: Char);

Begin
  InternalKeyPress(Key);
  If Key = #9 then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;
End;

Procedure TGLFocusControl.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  FShiftState := Shift;
  InternalKeyDown(Key,Shift);
  If Key = vk_tab then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;
End;

Procedure TGLFocusControl.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  FShiftState := Shift;
  InternalKeyUp(Key,Shift);
  If Key = vk_tab then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;

End;

{ base font control }

Destructor TGLBaseFontControl.Destroy;
Begin
  BitmapFont := Nil;
  inherited;
End;

Procedure TGLBaseFontControl.SetBitmapFont(NewFont : TGLCustomBitmapFont);

Begin
   if NewFont<>FBitmapFont then begin
      if Assigned(FBitmapFont) then
      Begin
         FBitmapFont.RemoveFreeNotification(Self);
         FBitmapFont.UnRegisterUser(Self);
      End;
      FBitmapFont:=NewFont;
      if Assigned(FBitmapFont) then begin
         FBitmapFont.RegisterUser(Self);
         FBitmapFont.FreeNotification(Self);
      end;
      GUIRedraw := True;
   end;
End;

Function  TGLBaseFontControl.GetDefaultColor : TColor;

Begin
  Result := ConvertColorVector(FDefaultColor);
End;

procedure TGLBaseFontControl.SetDefaultColor(value : TColor);

Begin
  FDefaultColor := ConvertWinColor(value);
End;

procedure TGLBaseFontControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
   if (Operation=opRemove) and (AComponent=FBitmapFont) then
      BitmapFont:=nil;
end;

{ GLWindow }

Procedure TGLBaseTextControl.SetCaption(NewCaption : String);

Begin
  FCaption := NewCaption;
  GuiRedraw := True;
End;

Procedure TGLBaseFontControl.WriteTextAt(Const X,Y : TGLFloat; Const Data : String; Const Color : TColorVector);

Var
  Position : TVector;
Begin
  If Assigned(FBitmapFont) then
  Begin
    Position[0] := X;
    Position[1] := Y;
    Position[2] := 0;
    Position[3] := 0;
    FBitmapFont.RenderString(Data,taLeftJustify,tlTop,Color, @Position);
  End;
End;

Procedure TGLBaseFontControl.WriteTextAt(Const X1,Y1,X2,Y2 : TGLFloat; Const Data : String; const Color : TColorVector);
var
  Position : TVector;
Begin
  If Assigned(FBitmapFont) then
  Begin
    Position[0] := ((X2+X1-FBitmapFont.CalcStringWidth(Data)) / 2);
    Position[1] := -((Y2+Y1-GetFontHeight) / 2);
    Position[2] := 0;
    Position[3] := 0;
    FBitmapFont.RenderString(Data,taLeftJustify,tlTop,Color,@Position);
  End;
End;


Function  TGLBaseFontControl.GetFontHeight : Integer;

Begin
  If Assigned(FBitmapFont) then
    If FBitmapFont is TWindowsBitmapFont then
      Result := Abs((FBitmapFont as TWindowsBitmapFont).Font.Height)
    else
      Result := FBitmapFont.CharHeight
  else Result := -1;
End;

procedure TGLForm.InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Var
  CanMove : Boolean;
  YHere : TGLFloat;

Begin
  YHere := Y - Position.Y;
  If YHere < FRenderStatus[GLALTop].X2 then
  Begin
    If Button = mbLeft then
    Begin
{      If contains(Width-22,Width-6,XHere) and contains(8,24,YHere) then
      Begin
        HowClose := co_hide;
        If Assigned(FOnCanClose) then FOnCanClose(Self,HowClose);
        Case HowClose of
          co_hide   : Visible := False;
          co_ignore : ;
          co_Destroy : Free;
        End;
      End else{}
      Begin
        CanMove := True;
        If Assigned(FOnCanMove) then FOnCanMove(Self,CanMove);
        If CanMove then
        Begin
          OldX := X;
          OldY := Y;
          Moving := True;
          If Parent is TGLBaseControl then
          Begin
            (Parent as TGLBaseControl).ActiveControl := Self;
          End;
        End;
      End;
    End;
  End else inherited;
End;

procedure TGLForm.InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Begin
  If (Button = mbLeft) and Moving then
  Begin
    Moving := False;
    If Parent is TGLBaseControl then
    Begin
      (Parent as TGLBaseControl).ActiveControl := Nil;
    End;
    Exit;
  End;

  If Y - Position.Y < 27 then
  Begin
  End else inherited;
End;

procedure TGLForm.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

Var
  XRel, YRel : Integer;

Begin
  If Moving then
  Begin
    If (X <> OldX) or (Y <> OldY) then
    Begin
      XRel := X - OldX;
      YRel := Y - OldY;
      MoveGUI(XRel,YRel);

      OldX := X;
      OldY := Y;
    End;
  End else
  If Y - Position.Y < 27 then
  Begin

  End else inherited;
End;

Function  TGLForm.GetTitleColor : TColor;

Begin
  Result := ConvertColorVector(FTitleColor);
End;

procedure TGLForm.SetTitleColor(value : TColor);

Begin
  FTitleColor := ConvertWinColor(value);
End;

Constructor TGLForm.Create(AOwner : TComponent);

Begin
  inherited;
  Resized := True;
End;


Function  TGLForm.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (Button = mbLeft) and (Moving) then
  Begin
    Result := True;
    InternalMouseUp(Shift,Button,X,Y);
  End else Result := Inherited MouseUp(Sender,Button,Shift,X,Y);
End;

Function  TGLForm.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (Moving) then
  Begin
    Result := True;
    InternalMouseMove(Shift,X,Y);
  End else Result := Inherited MouseMove(Sender,Shift,X,Y);
End;

procedure TGLForm.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
Begin
  If Assigned(FGuiComponent) then
  Begin
    FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    WriteTextAt(FRenderStatus[GLALTop].X1,FRenderStatus[GLALTop].Y1,FRenderStatus[GLALTop].X2,FRenderStatus[GLALTop].Y2,Caption,FTitleColor);
  End;
End;

Procedure TGLCheckBox.SetChecked(NewChecked : Boolean);

Begin
  If NewChecked <> FChecked then
  begin
    FChecked := NewChecked;
    FReBuildGui := True;
    GUIRedraw := True;
    if Assigned(FOnChange) then FOnChange(Self);
  End;
End;

procedure TGLCheckBox.InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);
Begin
  Checked := Not Checked;
  inherited;
End;

procedure TGLCheckBox.InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Begin
  inherited;
End;

procedure TGLCheckBox.SetGuiLayoutNameChecked(newName : TGLGuiComponentName);

Begin
  If FGuiLayoutNameChecked <> NewName then
  Begin
    FGuiCheckedComponent := Nil;
    FGuiLayoutNameChecked := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiCheckedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLCheckBox.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  FGuiCheckedComponent := Nil;
  inherited;
  If Assigned(FGuiLayout) then
  Begin
    FGuiCheckedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

procedure TGLCheckBox.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
Begin
  If Checked then
  Begin
    If Assigned(FGuiCheckedComponent) then
    Begin
      FGuiCheckedComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  End else
  Begin
    If Assigned(FGuiComponent) then
    Begin
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  End;
End;


Procedure TGLButton.SetPressed(NewPressed : Boolean);

Begin
  If FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    If FPressed then
    if Assigned(FOnButtonClick) then FOnButtonClick(Self);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

procedure TGLButton.InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);
Begin
  SetFocus;
  inherited;
  If Button = mbLeft then
  Pressed := True;
End;

procedure TGLButton.InternalMouseUp(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);

Begin
  If Button = mbLeft then
  Pressed := False;
  inherited;
End;

Procedure TGLButton.InternalKeyDown(var Key: Word; Shift: TShiftState);

Begin
  inherited;
  If Key = vk_space then
  Begin
    Pressed := True;
  End;
  If Key = vk_Return then
  Begin
    Pressed := True;
  End;
End;

Procedure TGLButton.InternalKeyUp(var Key: Word; Shift: TShiftState);

Begin
  If Key = vk_space then
  Begin
    Pressed := False;
  End;
  If Key = vk_Return then
  Begin
    Pressed := False;
  End;
  inherited;
End;

Procedure TGLButton.SetFocused(Value :Boolean);
Begin
  Pressed := False;
  inherited;
End;

procedure TGLButton.SetGuiLayoutNamePressed(newName : TGLGuiComponentName);

Begin
  If FGuiLayoutNamePressed <> NewName then
  Begin
    FGuiPressedComponent := Nil;
    FGuiLayoutNamePressed := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiPressedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLButton.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  FGuiPressedComponent := Nil;
  inherited;
  If Assigned(FGuiLayout) then
  Begin
    FGuiPressedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

procedure TGLButton.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
  if Pressed then
  begin
    If Assigned(FGuiPressedComponent) then
    Begin
      FGuiPressedComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  end else
  begin
    If Assigned(FGuiComponent) then
    Begin
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  end;

   If Assigned(FBitmapFont) then
   Begin
     If FFocused then
     Begin
       WriteTextAt(FRenderStatus[GLALCenter].X1,FRenderStatus[GLALCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Caption,FocusedColor);
     End else
     Begin
       WriteTextAt(FRenderStatus[GLALCenter].X1,FRenderStatus[GLALCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Caption,NormalColor);
     End;
   End;
End;

procedure TGLEdit.InternalMouseDown(Shift: TShiftState; Button: TMouseButton; X, Y: Integer);
Begin
  SetFocus;
  inherited;
End;

Procedure TGLEdit.InternalKeyPress(var Key: Char);
Begin
  inherited;

  Case Key of
    #8 :
    Begin
      If FSelStart > 1 then
      Begin
        system.Delete(FCaption,FSelStart-1,1);
        Dec(FSelStart);
        GUIRedraw := True;
      End;
    End;
    else
    Begin
      If Key >= #32 then
      Begin
        system.Insert(Key,FCaption,SelStart);
        inc(FSelStart);
        GUIRedraw := True;
      End;
    End;
  End;
End;

Procedure TGLEdit.InternalKeyDown(var Key: Word; Shift: TShiftState);

Begin
  inherited;

  Case Key of
    VK_DELETE :
    Begin
      If FSelStart <= Length(Caption) then
      Begin
        System.Delete(FCaption,FSelStart,1);
        GUIRedraw := True;
      End;
    End;
    VK_LEFT   :
    Begin
      If FSelStart > 1 then
      Begin
        Dec(FSelStart);
        GUIRedraw := True;
      End;
    End;
    VK_Right   :
    Begin
      If FSelStart < Length(Caption)+1 then
      Begin
        Inc(FSelStart);
        GUIRedraw := True;
      End;
    End;
    VK_Home   :
    Begin
      If FSelStart > 1 then
      Begin
        FSelStart := 1;
        GUIRedraw := True;
      End;
    End;
    VK_End   :
    Begin
      If FSelStart < Length(Caption)+1 then
      Begin
        FSelStart := Length(Caption)+1;
        GUIRedraw := True;
      End;
    End;
  End;

End;

Procedure TGLEdit.InternalKeyUp(var Key: Word; Shift: TShiftState);

Begin
  inherited;
End;

Procedure TGLEdit.SetFocused(Value :Boolean);

Begin
  Inherited;
  If Value then
  SelStart := Length(Caption)+1;
End;

Procedure TGLEdit.SetSelStart(Value : Integer);

Begin
  FSelStart := Value;
  GUIRedraw := True;
End;

procedure TGLEdit.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
var
        Tekst : String;
Begin
  If Assigned(FGuiComponent) then
  Begin
    FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
  End;

  If Assigned(FBitmapFont) then
  Begin
    Tekst := Caption;
    If FFocused then
    Begin
      system.insert('*',Tekst,SelStart);
      WriteTextAt(FRenderStatus[GLALCenter].X1,FRenderStatus[GLALCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Tekst,FocusedColor);
    End else
    Begin
      WriteTextAt(FRenderStatus[GLALCenter].X1,FRenderStatus[GLALCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Tekst,NormalColor);
    End;
  End;
End;

procedure TGLLabel.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  TekstPos : TVector;
  Tekst : String;

Begin
  If Assigned(FBitmapFont) then
  Begin
    SetVector(TekstPos,8,-((Height-GetFontHeight) / 2)+1,0);
    Tekst := Caption;
    FBitmapFont.RenderString(Tekst,taLeftJustify,tlTop, FDefaultColor, @TekstPos);
  End;
End;

procedure TGLAdvancedLabel.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
   If Assigned(FBitmapFont) then
   Begin
     If Focused then
     Begin
       WriteTextAt(8,-((Height-GetFontHeight) / 2)+1,Caption,FocusedColor);
     End else
     Begin
       WriteTextAt(8,-((Height-GetFontHeight) / 2)+1,Caption,NormalColor);
     End;
   End;
End;

initialization
   RegisterClasses([TGLBaseControl,TGLForm,TGLPanel,TGLButton,TGLCheckBox,TGLEdit,TGLLabel,TGLAdvancedLabel]);
end.
