//
// VKScene project, http://glscene.sourceforge.net
//
{
   "Alternate" OpenGL functions to handle multi-texturing.
   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.
   Before using any of the xglTexCoordXxxx fonctions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.
   This unit is Open-Source under MPL

}
unit VKS.XOpenGL;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  VKS.Context;

type
  TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond,
    mtcmArbitrary);

  TVKMultitextureCoordinator = class(TAbstractMultitextureCoordinator)
  private
    FMapTexCoordMode: TMapTexCoordMode;
    FSecondTextureUnitForbidden: Boolean;

    FUpdCount: Integer;
    FUpdNewMode: TMapTexCoordMode;
    FStateStack: array of TMapTexCoordMode;
    FComplexMapping: array of Cardinal;
    FComplexMappingN: Integer;
  public
    // Explicit texture coordinates specification
    TexCoord2f: procedure(s, t: GLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexCoord2fv: procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexCoord3f: procedure(s, t, r: GLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexCoord3fv: procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexCoord4f: procedure(s, t, r, q: GLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexCoord4fv: procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}

    // TexGen texture coordinates specification
    TexGenf: procedure(coord, pname: GLEnum; param: GLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexGenfv: procedure(coord, pname: GLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexGeni: procedure(coord, pname: GLEnum; param: GLint);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    TexGeniv: procedure(coord, pname: GLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}

    // Vertex Arrays texture coordinates specification
    TexCoordPointer: procedure(size: GLint; atype: GLEnum; stride: GLsizei;
      data: pointer);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    EnableClientState: procedure(aarray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    DisableClientState: procedure(aarray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}

    // Misc
    Enable: procedure(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}
    Disable: procedure(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}{$IFDEF unix} cdecl;{$ENDIF}

    constructor Create(AOwner: TVKContext); override;

    { TexCoord functions will be ignored. }
    procedure MapTexCoordToNull;
    { TexCoord functions will define the main texture coordinates. }
    procedure MapTexCoordToMain;
    { TexCoord functions will define the second texture unit coordinates. }
    procedure MapTexCoordToSecond;
    { TexCoord functions will define the two first texture units coordinates. }
    procedure MapTexCoordToDual;
    { TexCoord functions will define the specified texture units coordinates. }
    procedure MapTexCoordToArbitrary(const units: array of Cardinal); overload;
    procedure MapTexCoordToArbitrary(const bitWiseUnits: Cardinal); overload;
    procedure MapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);

    { Defers Map calls execution until EndUpdate is met.
       Calls to Begin/EndUpdate may be nested. }
    procedure BeginUpdate;
    { Applies Map calls if there were any since BeginUpdate was invoked. 
       Calls to Begin/EndUpdate may be nested. }
    procedure EndUpdate;

    { Saves VKS.XOpenGL State on the stack. }
    procedure PushState;
    { Restores VKS.XOpenGL State from the stack. }
    procedure PopState;

    { Whenever called, 2nd texture units changes will be forbidden to . 
       Use this function when you're using the 2nd texture unit for your own
       purposes and don't want VKS.XOpenGL to alter it. }
    procedure ForbidSecondTextureUnit;
    { Allow VKS.XOpenGL to use the second texture unit again. }
    procedure AllowSecondTextureUnit;
    { Returns the complex mapping in bitwise form. }
    function GetBitWiseMapping: Cardinal;

    property MapTexCoordMode: TMapTexCoordMode read FMapTexCoordMode write FMapTexCoordMode;
    property SecondTextureUnitForbidden: Boolean read FSecondTextureUnitForbidden;
  end;

function xgl(): TVKMultitextureCoordinator;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

threadvar
  vMTC : TVKMultitextureCoordinator;

function xgl(): TVKMultitextureCoordinator;
var
  RC: TVKContext;
begin
  RC := SafeCurrentGLContext;
  if not Assigned(vMTC) or (vMTC.FOwner <> RC) then
  begin
    vMTC := TVKMultitextureCoordinator(RC.MultitextureCoordinator);
  end;
  Result := vMTC;
end;

  // ------------------------------------------------------------------
  // Multitexturing coordinates duplication functions
  // ------------------------------------------------------------------

  // --------- Complex (arbitrary) mapping

procedure TexCoord2f_Arbitrary(s, t: GLfloat);{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord2f(xgl.FComplexMapping[i], s, t);
end;

procedure TexCoord2fv_Arbitrary(v: PGLfloat);{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord2fv(xgl.FComplexMapping[i], v);
end;

procedure TexCoord3f_Arbitrary(s, t, r: GLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord3f(xgl.FComplexMapping[i], s, t, r);
end;

procedure TexCoord3fv_Arbitrary(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord3fv(xgl.FComplexMapping[i], v);
end;

procedure TexCoord4f_Arbitrary(s, t, r, q: GLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord4f(xgl.FComplexMapping[i], s, t, r, q);
end;

procedure TexCoord4fv_Arbitrary(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
    glMultiTexCoord4fv(xgl.FComplexMapping[i], v);
end;

procedure TexGenf_Arbitrary(coord, pname: GLEnum; param: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glTexGenf(coord, pname, param);
  end;
end;

procedure TexGenfv_Arbitrary(coord, pname: GLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glTexGenfv(coord, pname, params);
  end;
end;

procedure TexGeni_Arbitrary(coord, pname: GLEnum; param: GLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glTexGeni(coord, pname, param);
  end;
end;

procedure TexGeniv_Arbitrary(coord, pname: GLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glTexGeniv(coord, pname, params);
  end;
end;

procedure Enable_Arbitrary(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glEnable(cap);
  end;
end;

procedure Disable_Arbitrary(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := xgl.FComplexMapping[i];
    glDisable(cap);
  end;
end;

procedure TexCoordPointer_Arbitrary(size: GLint; atype: GLEnum; stride:
  GLsizei; data: pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    glClientActiveTexture(xgl.FComplexMapping[i]);
    glTexCoordPointer(size, atype, stride, data);
  end;
end;

procedure EnableClientState_Arbitrary(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    glClientActiveTexture(xgl.FComplexMapping[i]);
    glEnableClientState(aArray);
  end;
end;

procedure DisableClientState_Arbitrary(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to xgl.FComplexMappingN do
  begin
    glClientActiveTexture(xgl.FComplexMapping[i]);
    glDisableClientState(aArray);
  end;
end;

// --------- Second unit Texturing

procedure TexCoord2f_Second(s, t: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure TexCoord2fv_Second(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure TexCoord3f_Second(s, t, r: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure TexCoord3fv_Second(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure TexCoord4f_Second(s, t, r, q: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure TexCoord4fv_Second(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glMultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure TexGenf_Second(coord, pname: GLEnum; param: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glTexGenf(coord, pname, param);
end;

procedure TexGenfv_Second(coord, pname: GLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glTexGenfv(coord, pname, params);
end;

procedure TexGeni_Second(coord, pname: GLEnum; param: GLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glTexGeni(coord, pname, param);
end;

procedure TexGeniv_Second(coord, pname: GLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glTexGeniv(coord, pname, params);
end;

procedure Enable_Second(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glEnable(cap);
end;

procedure Disable_Second(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  glDisable(cap);
end;

procedure TexCoordPointer_Second(size: GLint; atype: GLEnum; stride:
  GLsizei; data: pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure EnableClientState_Second(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glEnableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure DisableClientState_Second(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glClientActiveTexture(GL_TEXTURE1);
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

// --------- Dual Texturing

procedure TexCoord2f_Dual(s, t: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord2f(s, t);
  glMultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure TexCoord2fv_Dual(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord2fv(v);
  glMultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure TexCoord3f_Dual(s, t, r: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord3f(s, t, r);
  glMultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure TexCoord3fv_Dual(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord3fv(v);
  glMultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure TexCoord4f_Dual(s, t, r, q: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord4f(s, t, r, q);
  glMultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure TexCoord4fv_Dual(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord4fv(v);
  glMultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure TexGenf_Dual(coord, pname: GLEnum; param: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glTexGenf(coord, pname, param);
    ActiveTexture := 1;
    glTexGenf(coord, pname, param);
  end;
end;

procedure TexGenfv_Dual(coord, pname: GLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glTexGenfv(coord, pname, params);
    ActiveTexture := 1;
    glTexGenfv(coord, pname, params);
  end;
end;

procedure TexGeni_Dual(coord, pname: GLEnum; param: GLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glTexGeni(coord, pname, param);
    ActiveTexture := 1;
    glTexGeni(coord, pname, param);
  end;
end;

procedure TexGeniv_Dual(coord, pname: GLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glTexGeniv(coord, pname, params);
    ActiveTexture := 1;
    glTexGeniv(coord, pname, params);
  end;
end;

procedure Enable_Dual(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glEnable(cap);
    ActiveTexture := 1;
    glEnable(cap);
  end;
end;

procedure Disable_Dual(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    glDisable(cap);
    ActiveTexture := 1;
    glDisable(cap);
  end;
end;

procedure TexCoordPointer_Dual(size: GLint; atype: GLEnum; stride:
  GLsizei; data: pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE1);
  glTexCoordPointer(size, atype, stride, data);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure EnableClientState_Dual(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glEnableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE1);
  glEnableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

procedure DisableClientState_Dual(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE1);
  glDisableClientState(aArray);
  glClientActiveTexture(GL_TEXTURE0);
end;

// --------- Null Texturing

procedure TexCoord2f_Null(s, t: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoord2fv_Null(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoord3f_Null(s, t, r: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoord3fv_Null(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoord4f_Null(s, t, r, q: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoord4fv_Null(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexGenf_Null(coord, pname: GLEnum; param: GLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexGenfv_Null(coord, pname: GLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexGeni_Null(coord, pname: GLEnum; param: GLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexGeniv_Null(coord, pname: GLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure Enable_Null(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure Disable_Null(cap: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure TexCoordPointer_Null(size: GLint; atype: GLEnum; stride:
  GLsizei; data: pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure EnableClientState_Null(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure DisableClientState_Null(aArray: GLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------

// BeginUpdate
//

procedure TVKMultitextureCoordinator.BeginUpdate;
begin
  if FUpdCount = 0 then
  begin
    FUpdCount := 1;
    FUpdNewMode := MapTexCoordMode;
  end
  else
    Inc(FUpdCount);
end;

// EndUpdate
//

procedure TVKMultitextureCoordinator.EndUpdate;
begin
  Dec(FUpdCount);
  if (FUpdCount = 0) and (FUpdNewMode <> MapTexCoordMode) then
  begin
    case FUpdNewMode of
      mtcmNull: MapTexCoordToNull;
      mtcmMain: MapTexCoordToMain;
      mtcmDual: MapTexCoordToDual;
      mtcmSecond: MapTexCoordToSecond;
      mtcmArbitrary: MapTexCoordToArbitrary(FComplexMapping);
    else
      Assert(False);
    end;
  end;
end;

// PushState
//

procedure TVKMultitextureCoordinator.PushState;
var
  i: Integer;
begin
  Assert(FUpdCount = 0);
  i := Length(FStateStack);
  SetLength(FStateStack, i + 1);
  FStateStack[i] := MapTexCoordMode;
end;

// PopState
//

procedure TVKMultitextureCoordinator.PopState;
var
  i: Integer;
begin
  Assert(FUpdCount = 0);
  i := Length(FStateStack) - 1;
  Assert(i >= 0);
  case FStateStack[i] of
    mtcmNull: MapTexCoordToNull;
    mtcmMain: MapTexCoordToMain;
    mtcmDual: MapTexCoordToDual;
    mtcmSecond: MapTexCoordToSecond;
    mtcmArbitrary: MapTexCoordToArbitrary(FComplexMapping);
  else
    Assert(False);
  end;
  SetLength(FStateStack, i);
end;

// ForbidSecondTextureUnit
//

procedure TVKMultitextureCoordinator.ForbidSecondTextureUnit;
begin
  FSecondTextureUnitForbidden := True;
end;

// AllowSecondTextureUnit
//

procedure TVKMultitextureCoordinator.AllowSecondTextureUnit;
begin
  FSecondTextureUnitForbidden := False;
end;

constructor TVKMultitextureCoordinator.Create(AOwner: TVKContext);
begin
  inherited Create(AOwner);
  FMapTexCoordMode := mtcmUndefined;
  MapTexCoordToNull;
end;

// MapTexCoordToNull
//

procedure TVKMultitextureCoordinator.MapTexCoordToNull;
begin
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmNull
  else if MapTexCoordMode <> mtcmNull then
  begin
    MapTexCoordMode := mtcmNull;

    TexCoord2f := TexCoord2f_Null;
    TexCoord2fv := TexCoord2fv_Null;
    TexCoord3f := TexCoord3f_Null;
    TexCoord3fv := TexCoord3fv_Null;
    TexCoord4f := TexCoord4f_Null;
    TexCoord4fv := TexCoord4fv_Null;

    TexGenf := TexGenf_Null;
    TexGenfv := TexGenfv_Null;
    TexGeni := TexGeni_Null;
    TexGeniv := TexGeniv_Null;

    TexCoordPointer := TexCoordPointer_Null;
    EnableClientState := EnableClientState_Null;
    DisableClientState := DisableClientState_Null;

    Enable := Enable_Null;
    Disable := Disable_Null;
  end;
end;

// TexCoordMapToMain
//

procedure TVKMultitextureCoordinator.MapTexCoordToMain;
begin
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmMain
  else if MapTexCoordMode <> mtcmMain then
  begin
    MapTexCoordMode := mtcmMain;

    TexCoord2f := glTexCoord2f;
    TexCoord2fv := glTexCoord2fv;
    TexCoord3f := glTexCoord3f;
    TexCoord3fv := glTexCoord3fv;
    TexCoord4f := glTexCoord4f;
    TexCoord4fv := glTexCoord4fv;

    TexGenf := glTexGenf;
    TexGenfv := glTexGenfv;
    TexGeni := glTexGeni;
    TexGeniv := glTexGeniv;

    TexCoordPointer := glTexCoordPointer;
    EnableClientState := glEnableClientState;
    DisableClientState := glDisableClientState;

    Enable := glEnable;
    Disable := glDisable;
  end;
end;

// TexCoordMapToSecond
//

procedure TVKMultitextureCoordinator.MapTexCoordToSecond;
begin
  if FSecondTextureUnitForbidden then
  begin
    MapTexCoordToNull;
    Exit;
  end;
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmSecond
  else if MapTexCoordMode <> mtcmSecond then
  begin
    MapTexCoordMode := mtcmSecond;
///    Assert(GL_ARB_multitexture);

    TexCoord2f := TexCoord2f_Second;
    TexCoord2fv := TexCoord2fv_Second;
    TexCoord3f := TexCoord3f_Second;
    TexCoord3fv := TexCoord3fv_Second;
    TexCoord4f := TexCoord4f_Second;
    TexCoord4fv := TexCoord4fv_Second;

    TexGenf := TexGenf_Second;
    TexGenfv := TexGenfv_Second;
    TexGeni := TexGeni_Second;
    TexGeniv := TexGeniv_Second;

    TexCoordPointer := TexCoordPointer_Second;
    EnableClientState := EnableClientState_Second;
    DisableClientState := DisableClientState_Second;

    Enable := Enable_Second;
    Disable := Disable_Second;
  end;
end;

// TexCoordMapToDual
//

procedure TVKMultitextureCoordinator.MapTexCoordToDual;
begin
  if FSecondTextureUnitForbidden then
  begin
    MapTexCoordToMain;
    Exit;
  end;
  if FUpdCount <> 0 then
    FUpdNewMode := mtcmDual
  else if MapTexCoordMode <> mtcmDual then
  begin
    MapTexCoordMode := mtcmDual;
///    Assert(GL_ARB_multitexture);

    TexCoord2f := TexCoord2f_Dual;
    TexCoord2fv := TexCoord2fv_Dual;
    TexCoord3f := TexCoord3f_Dual;
    TexCoord3fv := TexCoord3fv_Dual;
    TexCoord4f := TexCoord4f_Dual;
    TexCoord4fv := TexCoord4fv_Dual;

    TexGenf := TexGenf_Dual;
    TexGenfv := TexGenfv_Dual;
    TexGeni := TexGeni_Dual;
    TexGeniv := TexGeniv_Dual;

    TexCoordPointer := TexCoordPointer_Dual;
    EnableClientState := EnableClientState_Dual;
    DisableClientState := DisableClientState_Dual;

    Enable := Enable_Dual;
    Disable := Disable_Dual;
  end;
end;

// MapTexCoordToArbitrary (array)
//

procedure TVKMultitextureCoordinator.MapTexCoordToArbitrary(const units: array of Cardinal);
var
  i, j, n: Integer;
begin
  n := Length(units);
  SetLength(FComplexMapping, n);
  j := 0;
  FComplexMappingN := n - 1;
  for i := 0 to FComplexMappingN do
  begin
    if (not FSecondTextureUnitForbidden) or (units[i] <> GL_TEXTURE1) then
    begin
      FComplexMapping[j] := units[i];
      Inc(j);
    end;
  end;

  if FUpdCount <> 0 then
    FUpdNewMode := mtcmArbitrary
  else if MapTexCoordMode <> mtcmArbitrary then
  begin

    MapTexCoordMode := mtcmArbitrary;
///    Assert(GL_ARB_multitexture);

    TexCoord2f := TexCoord2f_Arbitrary;
    TexCoord2fv := TexCoord2fv_Arbitrary;
    TexCoord3f := TexCoord3f_Arbitrary;
    TexCoord3fv := TexCoord3fv_Arbitrary;
    TexCoord4f := TexCoord4f_Arbitrary;
    TexCoord4fv := TexCoord4fv_Arbitrary;

    TexGenf := TexGenf_Arbitrary;
    TexGenfv := TexGenfv_Arbitrary;
    TexGeni := TexGeni_Arbitrary;
    TexGeniv := TexGeniv_Arbitrary;

    TexCoordPointer := TexCoordPointer_Arbitrary;
    EnableClientState := EnableClientState_Arbitrary;
    DisableClientState := DisableClientState_Arbitrary;

    Enable := Enable_Arbitrary;
    Disable := Disable_Arbitrary;
  end;
end;

// MapTexCoordToArbitrary (bitwise)
//

procedure TVKMultitextureCoordinator.MapTexCoordToArbitrary(const bitWiseUnits: Cardinal);
var
  i, n: Integer;
  units: array of Cardinal;
begin
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
      Inc(n);
  end;
  SetLength(units, n);
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
    begin
      units[n] := GL_TEXTURE0 + i;
      Inc(n);
    end;
  end;
  MapTexCoordToArbitrary(units);
end;

// MapTexCoordToArbitrary (bitwise)
//

procedure TVKMultitextureCoordinator.MapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);
var
  n: Cardinal;
begin
  n := GetBitWiseMapping;
  MapTexCoordToArbitrary(n or bitWiseUnits);
end;

// GetBitWiseMapping
//

function TVKMultitextureCoordinator.GetBitWiseMapping: Cardinal;
var
  i, n: Cardinal;
  mode: TMapTexCoordMode;
begin
  if FUpdCount > 0 then
    mode := FUpdNewMode
  else
    mode := MapTexCoordMode;
  n := 0;
  case mode of
    mtcmMain: n := 1;
    mtcmDual: n := 3;
    mtcmSecond: n := 2;
    mtcmArbitrary:
      begin
        for i := 0 to FComplexMappingN do
          n := n or (1 shl (FComplexMapping[i] - GL_TEXTURE0));
      end;
  else
    Assert(False);
  end;
  Result := n;
end;

initialization

  // Register class
  vMultitextureCoordinatorClass := TVKMultitextureCoordinator;

end.