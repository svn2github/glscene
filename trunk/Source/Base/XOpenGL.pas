// XOpenGL
{: "Alternate" OpenGL functions to handle multi-texturing.<p>

   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.<p>

   Before using any of the xglTexCoordXxxx fonctions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.<p>

   This unit is Open-Source under MPL<br>
   Copyright 2001 - Eric Grange (egrange@glscene.org)<br>
   http://glscene.org<p>

   <b>History :</b><ul>
      <li>01/02/03 - EG - Added State stack
      <li>01/07/02 - EG - Added mtcmUndefined, fixed initial state
      <li>03/01/02 - EG - Added xglDisableClientState
      <li>26/01/02 - EG - Added xglBegin/EndUpdate mechanism
      <li>21/12/01 - EG - Fixed xglTexCoordPointer and xglEnableClientState
      <li>18/12/01 - EG - Added xglEnableClientState
      <li>24/08/01 - EG - Now supports MULTITHREADOPENGL (same as OpenGL12)
      <li>17/08/01 - EG - Made declarations Kylix compatible (cdecl vs stdcall) 
      <li>16/08/01 - EG - Renamed xglMapTextCoordMode to xglMapTexCoordMode
      <li>14/08/01 - EG - Added xglMapTexCoordToSecond
      <li>21/02/01 - EG - Added TexGen and vertex arrays mappings
   </ul>
}
unit XOpenGL;

interface

{.$define MULTITHREADOPENGL}

uses OpenGL12;

type
   TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond);

{: xglTexCoord functions will be ignored. }
procedure xglMapTexCoordToNull;
{: xglTexCoord functions will define the main texture coordinates. }
procedure xglMapTexCoordToMain;
{: xglTexCoord functions will define the second texture unit coordinates. }
procedure xglMapTexCoordToSecond;
{: xglTexCoord functions will define the two first texture units coordinates. }
procedure xglMapTexCoordToDual;

{: Defers xglMap calls execution until xglEndUpdate is met.<p>
   Calls to xglBegin/EndUpdate may be nested. }
procedure xglBeginUpdate;
{: Applies xglMap calls if there were any since xglBeginUpdate was invoked.<p>
   Calls to xglBegin/EndUpdate may be nested. }
procedure xglEndUpdate;

{: Saves XOpenGL State on the stack. }
procedure xglPushState;
{: Restores XOpenGL State from the stack. }
procedure xglPopState;

{$ifdef MULTITHREADOPENGL}
threadvar
{$else}
var
{$endif}

   xglMapTexCoordMode : TMapTexCoordMode;

   // Explicit texture coordinates specification
   xglTexCoord2f: procedure(s, t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexCoord2fv: procedure(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexCoord3f: procedure(s, t, r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexCoord3fv: procedure(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexCoord4f: procedure(s, t, r, q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexCoord4fv: procedure(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

   // TexGen texture coordinates specification
   xglTexGenf: procedure(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexGeni: procedure(coord, pname: TGLEnum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

   // Vertex Arrays texture coordinates specification
   xglTexCoordPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglEnableClientState: procedure(aarray: TGLEnum); {$ifdef Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglDisableClientState: procedure(aarray: TGLEnum); {$ifdef Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

   // Misc
   xglEnable: procedure(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
   xglDisable: procedure(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vUpdCount : Integer;
   vUpdNewMode : TMapTexCoordMode;
   vStateStack : array of TMapTexCoordMode;

// ------------------------------------------------------------------
// Multitexturing coordinates duplication functions
// ------------------------------------------------------------------

// --------- Second unit Texturing

procedure glTexCoord2f_Second(s, t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord2fARB(GL_TEXTURE1_ARB, s, t);
end;

procedure glTexCoord2fv_Second(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord2fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord3f_Second(s, t, r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord3fARB(GL_TEXTURE1_ARB, s, t, r);
end;

procedure glTexCoord3fv_Second(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord3fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord4f_Second(s, t, r, q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord4fARB(GL_TEXTURE1_ARB, s, t, r, q);
end;

procedure glTexCoord4fv_Second(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glMultiTexCoord4fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexGenf_Second(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenf(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGenfv_Second(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenfv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeni_Second(coord, pname: TGLEnum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeni(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Second(coord, pname: TGLEnum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeniv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glEnable_Second(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glEnable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glDisable_Second(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glDisable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglTexCoordPointer_Second(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glTexCoordPointer(size, atype, stride, data);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglEnableClientState_Second(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glEnableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglDisableClientState_Second(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glDisableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

// --------- Dual Texturing

procedure glTexCoord2f_Dual(s, t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord2f(s, t);
   glMultiTexCoord2fARB(GL_TEXTURE1_ARB, s, t);
end;

procedure glTexCoord2fv_Dual(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord2fv(v);
   glMultiTexCoord2fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord3f_Dual(s, t, r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord3f(s, t, r);
   glMultiTexCoord3fARB(GL_TEXTURE1_ARB, s, t, r);
end;

procedure glTexCoord3fv_Dual(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord3fv(v);
   glMultiTexCoord3fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexCoord4f_Dual(s, t, r, q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord4f(s, t, r, q);
   glMultiTexCoord4fARB(GL_TEXTURE1_ARB, s, t, r, q);
end;

procedure glTexCoord4fv_Dual(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoord4fv(v);
   glMultiTexCoord4fvARB(GL_TEXTURE1_ARB, v);
end;

procedure glTexGenf_Dual(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexGenf(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenf(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGenfv_Dual(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexGenfv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGenfv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeni_Dual(coord, pname: TGLEnum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexGeni(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeni(coord, pname, param);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glTexGeniv_Dual(coord, pname: TGLEnum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexGeniv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glTexGeniv(coord, pname, params);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glEnable_Dual(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glEnable(cap);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glEnable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure glDisable_Dual(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glDisable(cap);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glDisable(cap);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglTexCoordPointer_Dual(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glTexCoordPointer(size, atype, stride, data);
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glTexCoordPointer(size, atype, stride, data);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglEnableClientState_Dual(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glEnableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glEnableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure xglDisableClientState_Dual(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin
   glDisableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE1_ARB);
   glDisableClientState(aArray);
   glClientActiveTextureARB(GL_TEXTURE0_ARB);
end;

// --------- Null Texturing

procedure glTexCoord2f_Null(s, t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexCoord2fv_Null(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexCoord3f_Null(s, t, r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexCoord3fv_Null(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexCoord4f_Null(s, t, r, q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexCoord4fv_Null(v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexGenf_Null(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexGenfv_Null(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexGeni_Null(coord, pname: TGLEnum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glTexGeniv_Null(coord, pname: TGLEnum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glEnable_Null(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure glDisable_Null(cap: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure xglTexCoordPointer_Null(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure xglEnableClientState_Null(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

procedure xglDisableClientState_Null(aArray: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
begin end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------

// xglBeginUpdate
//
procedure xglBeginUpdate;
begin
   if vUpdCount=0 then begin
      vUpdCount:=1;
      vUpdNewMode:=xglMapTexCoordMode;
   end else Inc(vUpdCount);
end;

// xglEndUpdate
//
procedure xglEndUpdate;
begin
   Dec(vUpdCount);
   if (vUpdCount=0) and (vUpdNewMode<>xglMapTexCoordMode) then begin
      case vUpdNewMode of
         mtcmNull   : xglMapTexCoordToNull;
         mtcmMain   : xglMapTexCoordToMain;
         mtcmDual   : xglMapTexCoordToDual;
         mtcmSecond : xglMapTexCoordToSecond;
      else
         Assert(False);
      end;
   end;
end;

// xglPushState
//
procedure xglPushState;
var
   i : Integer;
begin
   Assert(vUpdCount=0);
   i:=Length(vStateStack);
   SetLength(vStateStack, i+1);
   vStateStack[i]:=xglMapTexCoordMode;
end;

// xglPopState
//
procedure xglPopState;
var
   i : Integer;
begin
   Assert(vUpdCount=0);
   i:=Length(vStateStack)-1;
   Assert(i>=0);
   case vStateStack[i] of
      mtcmNull : xglMapTexCoordToNull;
      mtcmMain : xglMapTexCoordToMain;
      mtcmDual : xglMapTexCoordToDual;
      mtcmSecond : xglMapTexCoordToSecond;
   end;
   SetLength(vStateStack, i);
end;

// xglMapTexCoordToNull
//
procedure xglMapTexCoordToNull;
begin
   if vUpdCount<>0 then
      vUpdNewMode:=mtcmNull
   else if xglMapTexCoordMode<>mtcmNull then begin
      xglMapTexCoordMode:=mtcmNull;

      xglTexCoord2f:=glTexCoord2f_Null;
      xglTexCoord2fv:=glTexCoord2fv_Null;
      xglTexCoord3f:=glTexCoord3f_Null;
      xglTexCoord3fv:=glTexCoord3fv_Null;
      xglTexCoord4f:=glTexCoord4f_Null;
      xglTexCoord4fv:=glTexCoord4fv_Null;

      xglTexGenf:=glTexGenf_Null;
      xglTexGenfv:=glTexGenfv_Null;
      xglTexGeni:=glTexGeni_Null;
      xglTexGeniv:=glTexGeniv_Null;

      xglTexCoordPointer:=xglTexCoordPointer_Null;
      xglEnableClientState:=xglEnableClientState_Null;
      xglDisableClientState:=xglDisableClientState_Null;

      xglEnable:=glEnable_Null;
      xglDisable:=glDisable_Null;
   end;
end;

// xglTexCoordMapToMain
//
procedure xglMapTexCoordToMain;
begin
   if vUpdCount<>0 then
      vUpdNewMode:=mtcmMain
   else if xglMapTexCoordMode<>mtcmMain then begin
      xglMapTexCoordMode:=mtcmMain;

      xglTexCoord2f:=glTexCoord2f;
      xglTexCoord2fv:=glTexCoord2fv;
      xglTexCoord3f:=glTexCoord3f;
      xglTexCoord3fv:=glTexCoord3fv;
      xglTexCoord4f:=glTexCoord4f;
      xglTexCoord4fv:=glTexCoord4fv;

      xglTexGenf:=glTexGenf;
      xglTexGenfv:=glTexGenfv;
      xglTexGeni:=glTexGeni;
      xglTexGeniv:=glTexGeniv;

      xglTexCoordPointer:=glTexCoordPointer;
      xglEnableClientState:=glEnableClientState;
      xglDisableClientState:=glDisableClientState;

      xglEnable:=glEnable;
      xglDisable:=glDisable;
   end;
end;

// xglTexCoordMapToSecond
//
procedure xglMapTexCoordToSecond;
begin
   if vUpdCount<>0 then
      vUpdNewMode:=mtcmSecond
   else if xglMapTexCoordMode<>mtcmSecond then begin
      xglMapTexCoordMode:=mtcmSecond;
      Assert(GL_ARB_multitexture);

      xglTexCoord2f:=glTexCoord2f_Second;
      xglTexCoord2fv:=glTexCoord2fv_Second;
      xglTexCoord3f:=glTexCoord3f_Second;
      xglTexCoord3fv:=glTexCoord3fv_Second;
      xglTexCoord4f:=glTexCoord4f_Second;
      xglTexCoord4fv:=glTexCoord4fv_Second;

      xglTexGenf:=glTexGenf_Second;
      xglTexGenfv:=glTexGenfv_Second;
      xglTexGeni:=glTexGeni_Second;
      xglTexGeniv:=glTexGeniv_Second;

      xglTexCoordPointer:=xglTexCoordPointer_Second;
      xglEnableClientState:=xglEnableClientState_Second;
      xglDisableClientState:=xglDisableClientState_Second;

      xglEnable:=glEnable_Second;
      xglDisable:=glDisable_Second;
   end;
end;

// xglTexCoordMapToDual
//
procedure xglMapTexCoordToDual;
begin
   if vUpdCount<>0 then
      vUpdNewMode:=mtcmDual
   else if xglMapTexCoordMode<>mtcmDual then begin
      xglMapTexCoordMode:=mtcmDual;
      Assert(GL_ARB_multitexture);

      xglTexCoord2f:=glTexCoord2f_Dual;
      xglTexCoord2fv:=glTexCoord2fv_Dual;
      xglTexCoord3f:=glTexCoord3f_Dual;
      xglTexCoord3fv:=glTexCoord3fv_Dual;
      xglTexCoord4f:=glTexCoord4f_Dual;
      xglTexCoord4fv:=glTexCoord4fv_Dual;

      xglTexGenf:=glTexGenf_Dual;
      xglTexGenfv:=glTexGenfv_Dual;
      xglTexGeni:=glTexGeni_Dual;
      xglTexGeniv:=glTexGeniv_Dual;

      xglTexCoordPointer:=xglTexCoordPointer_Dual;
      xglEnableClientState:=xglEnableClientState_Dual;
      xglDisableClientState:=xglDisableClientState_Dual;

      xglEnable:=glEnable_Dual;
      xglDisable:=glDisable_Dual;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   xglMapTexCoordMode:=mtcmUndefined;
   xglMapTexCoordToNull;

end.
