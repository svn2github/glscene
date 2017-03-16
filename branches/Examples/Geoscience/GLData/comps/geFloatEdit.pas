{-----------------------------------------------------------------------------
 Unit Name: geFloatEdit
 Author:    HochwimmerA
 Purpose:   Edit Box for floats - incomplete...
 $Id: geFloatEdit.pas,v 1.7 2003/09/11 05:25:52 hochwimmera Exp $
-----------------------------------------------------------------------------}
unit geFloatEdit;

interface

uses
  Classes, Controls, Forms, Messages, StdCtrls, SysUtils, Dialogs, Variants,
  Math, Windows;

type
  TGEValidateOption = (vdNone,vdSilent,vdWarning);

  TGEFloatEdit = class(TCustomEdit)
  private
    fFormatString : string;
    fIsNull : boolean;
    fLastText : string;
    fMaxValue : extended;
    fMinValue : extended;
    fReturnStop : boolean;
    fUseMinBound : boolean;
    fUseMaxBound : boolean;
    fValidate : TGEValidateOption;
    fValue : extended;
  protected
    procedure Loaded;override;
    function ConvertText(sValue:string):extended;
    procedure DoEnter;override;
		function GetText: string;
		procedure KeyPress(VAR Key: Char); override;
		procedure Refresh; virtual;
    procedure SetFormatString(sFormat:string);
		procedure SetReturnStop(bValue:boolean);
		procedure SetText(sValue: String);
    procedure SetValidate(aGE:TGEValidateOption);
    procedure SetValue(aValue:extended);
  	procedure WMKillFocus(VAR Message: TWMSetFocus); message WM_KILLFOCUS;
  public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    property IsNull : boolean read fIsNull;
    property Value : extended read fValue write SetValue;
  published
    property FormatString : string read fFormatString write SetFormatString;
		property Text : string read GetText write SetText;
    property MinValue : extended read fMinValue write fMinValue;
    property MaxValue : extended read fMaxValue write fMaxValue;
		property ReturnStop:Boolean read fReturnStop write SetReturnStop;
    property UseMinBound : boolean read fUseMinBound write fUseMinBound;
    property UseMaxBound : boolean read fUseMaxBound write fUseMaxBound;
    property Validate : TGEValidateOption read fValidate write SetValidate;

// expose from TCustomEdit;
    property Align;

    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  procedure Register;

implementation

// ----- TGEFloatEdit.Loaded ---------------------------------------------------
procedure TGEFloatEdit.Loaded;

begin
  inherited Loaded;
  Refresh;
end;
// ----- TGEFloatEdit.ConvertText ----------------------------------------------
function TGEFloatEdit.ConvertText(sValue:string):extended;

var
  s:string;

begin
// premultiply the number if no mantissa present
  s := svalue;
  if Pos('E',Uppercase(s)) = 1 then insert('1',s,1);
  if Pos('-E',UpperCase(s)) = 1 then insert('1',s,2);
  if (sValue = '') then
  begin
    fIsNull := true;
    result := 0.0
  end else
  begin
    try
      result := StrToFloat(s);
      fIsNull := false;
    except on EConvertError do
      begin
        result := fValue; // revert back to old value
//        result := 0.0;
        fIsNull := false;
      end;
    end;
  end;
end;
// ------ TGEFloatEdit.DoEnter -------------------------------------------------
procedure TGEFloatEdit.DoEnter;

begin
  if ISNull then
    inherited Text := ''
  else
    inherited Text := FloatToStr(value); // full precision
  inherited DoEnter;
end;
// ----- TGEFloatEdit.GetText --------------------------------------------------
function TGEFloatEdit.GetText: string;

begin
  result := inherited Text;
end;
// ----- TGEFloatEdit.KeyPress -------------------------------------------------
procedure TGEFloatEdit.KeyPress(VAR Key: Char);

var
 dTemp:extended;
 sTemp:string;

// only allow numbers, 0..9, e,E
function CharIsNumber(const C: AnsiChar): Boolean;

begin
  Result := (C in ['0'..'9']) or
    (C in ['-','+','e','E']) or (C in ['.',',']); //Last - DecimalSeparators
end;

begin
// control characters
//  if Key = #8 then
//    Key := #0;
  if (Ord(Key) = Ord(' ')) then
    Key := #0
  else if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    exit;
  end else
  if (Key = Char(VK_RETURN)) then
  begin
    if ReturnStop <> true then
    begin
      stemp:=inherited Text;
      dtemp := ConvertText(stemp);
      if not IsNull then
      begin
        Value := dtemp;
        SelectAll;
      end else
      begin
//     if (InputWarning) then
//       AlreadyWarned:=true;
//       MessageDlg(strtemp + ' is invalid!', mtInformation, [mbOK], 0);
        Refresh;
      end;

    end else
// return stop - ie a tabstop
    begin
{ do an equivalent tab out - use the screen global var ie if the key is pressed
then the parent on the must be focused I think}
     Key := #0;
     Screen.ActiveForm.Perform(WM_NEXTDLGCTL, 0, 0);  //method of form
    end;
	end else
  begin
// if not a screen number return null
    if not CharIsNumber(AnsiChar(Key)) then
      Key := #0;
  end;
  inherited KeyPress(Key);
end;
// ----- TGEFloatEdit.Refresh --------------------------------------------------
procedure TGEFloatEdit.Refresh;

begin
  if IsNull then
    inherited Text := ''
  else
  begin
    if (FormatString <> '') then
    begin
      try
        inherited Text := Format(FormatString,[value]);
      except
        inherited Text := FloatToStr(Value);
      end;
    end else
      inherited Text := FloatToStr(Value);
  end;
  fLastText := inherited Text;
end;
// ----- TGEFloatEdit.SetFormatString ------------------------------------------
procedure TGEFloatEdit.SetFormatString(sFormat:string);

begin
  if (fFormatString <> sFormat) then
  begin
    fFormatString := sFormat;
    Refresh;
  end;
end;
// ----- TGEFloatEdit.SetReturnStop --------------------------------------------
procedure TGEFloatEdit.SetReturnStop(bValue:boolean);
// only allow this if tabstop is true
begin
 if bValue <> fReturnStop then fReturnStop:= (bValue and TabStop);
end;
// ----- TGEFloatEdit.SetText --------------------------------------------------
procedure TGEFloatEdit.SetText(sValue: String);

begin
	if not (csLoading in ComponentState) then
  begin
    if (sValue <> '') then
      Value := ConvertText(sValue)
    else
    begin
      Value := 0.0;
      fIsNull := true;
    end;
  end;
  if IsNull then
    inherited Text := ''
  else
    inherited Text := FloatToStr(Value);
  fLastText := inherited Text;
end;
// ----- TGEFloatEdit.SetValidate ----------------------------------------------
procedure TGEFloatEdit.SetValidate(aGE:TGEValidateOption);

begin
  if (aGE <> fValidate) then
    fValidate := aGE;
end;
// ----- TGEFloatEdit.SetValue -------------------------------------------------
procedure TGEFloatEdit.SetValue(aValue:extended);
var
  bValid : boolean;

begin
// check for NAN
  if IsNan(aValue) then
  begin
    fValue := 0.0;
    fIsNull := true;
  end else
  begin


// check for validity
    bValid := true;
    if (fValidate = vdSilent) or (fValidate = vdWarning) then
    begin
      if (AValue < fMinValue) and fUseMinBound then
        bValid := false;
      if (AValue > fMaxValue) and fUseMaxBound then
        bValid := false;
    end;

    if bvalid then
    begin
      fIsNull := false;
      fValue := AValue;
    end else
    begin

    end;
  end;
  Refresh;
end;
// ----- TGEFloatEdit.WMKillFocus ----------------------------------------------
procedure TGEFloatEdit.WMKillFocus(Var Message: TWMSetFocus);

var
 dtemp:extended;
 stemp:string;

begin
  try
    stemp:=inherited Text;
    if (stemp <> fLastText) then
    begin
      dtemp := ConvertText(sTemp);
      if not IsNull then
        Value := dtemp
      else begin
//       if (InputWarning) and not (AlreadyWarned) then
//         MessageDlg(strtemp+ ' is invalid!', mtInformation, [mbOK], 0);
        Refresh;
      end;
    end;
    inherited;
  except
    if inherited Text <> '' then
    begin
      Self.SelectAll;
      Self.SetFocus;
    end else
    begin
      Value := 0.0;
      fIsNull := true;
      inherited;
    end;
  end;
end;
// ----- TGEFloatEdit.Create ---------------------------------------------------
constructor TGEFloatEdit.Create(AOwner: TComponent);

begin
	inherited Create(AOwner);
  fValidate := vdNone;
	fMinValue := 0.0;
	fMaxValue := 0.0;
  fUseMinBound := false;
  fUseMaxBound := false;
  fFormatString := ''; // set this to a format property - e.g. %8.2f
  fIsNull := false;
  fValue := 0.0;
  fLastText := inherited Text;
end;
// ----- TGEFloatEdit.Destroy --------------------------------------------------
destructor TGEFloatEdit.Destroy;

begin
	inherited Destroy;
end;

// ======== Register =======================================================
procedure Register;

begin
  RegisterComponents('Graphic Edge IO',[TGEFloatEdit]);
end;


end.
