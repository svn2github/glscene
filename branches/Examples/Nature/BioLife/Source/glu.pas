(*
  Delphi 3 translation of the Win32 version of GLU.H for OpenGL Version 1.1
  Copyright (c) 1998 Classic Software
  classicsw@classicsw.com
*)

(*++ BUILD Version: 0004    // Increment this if a change has global effects

Copyright (c) 1985-95, Microsoft Corporation

Module Name:

    glu.h

Abstract:

    Procedure declarations, constant definitions and macros for the OpenGL
    Utility Library.

--*)

(*
** Copyright 1991-1993, Silicon Graphics, Inc.
** All Rights Reserved.
** 
** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
** the contents of this file may not be disclosed to third parties, copied or
** duplicated in any form, in whole or in part, without the prior written
** permission of Silicon Graphics, Inc.
** 
** RESTRICTED RIGHTS LEGEND:
** Use, duplication or disclosure by the Government is subject to restrictions
** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
** rights reserved under the Copyright Laws of the United States.
*)

unit GLU;

interface

uses GL;

const
{$IFDEF SGI_OPENGL}
  GLU32DLL = 'GLU.DLL';
{$ELSE}
  GLU32DLL = 'GLU32.DLL';
{$ENDIF}

{type
  Array3d   = Array[0..2] of GLdouble;
  Array4f   = Array[0..3] of GLfloat;
  Array4i   = Array[0..3] of GLint;
  Array4p   = Array[0..3] of Pointer;
  Array4_4d = Array[0..3, 0..3] of GLdouble;
  Array4_4f = Array[0..3, 0..3] of GLfloat;
}

(*
** Return the error string associated with a particular error code.
** This will return 0 for an invalid error code.
**
** The generic function prototype that can be compiled for ANSI or Unicode
** is defined as follows:
**
** LPCTSTR APIENTRY gluErrorStringWIN (GLenum errCode);
*)
{$IFDEF UNICODE}
  function gluErrorStringWIN(errCode: GLenum): PWideChar;
{$ELSE}
  function gluErrorStringWIN(errCode: GLenum): PGLubyte;
{$ENDIF}

function gluErrorString(errCode: GLenum): PGLubyte; stdcall; external GLU32DLL;
function gluErrorUnicodeStringEXT(errCode: GLenum): PWideChar; stdcall; external GLU32DLL;

function gluGetString(name: GLenum): PGLubyte; stdcall; external GLU32DLL;

procedure gluOrtho2D(
    left: GLdouble;
    right: GLdouble;
    bottom: GLdouble;
    top: GLdouble); stdcall; external GLU32DLL;

procedure gluPerspective(
    fovy: GLdouble;
    aspect: GLdouble;
    zNear: GLdouble;
    zFar: GLdouble); stdcall; external GLU32DLL;

procedure gluPickMatrix(
    x: GLdouble;
    y: GLdouble;
    width: GLdouble;
    height: GLdouble;
    viewport: Array4i); stdcall; external GLU32DLL;

procedure gluLookAt(
    eyex: GLdouble;
    eyey: GLdouble;
    eyez: GLdouble;
    centerx: GLdouble;
    centery: GLdouble;
    centerz: GLdouble;
    upx: GLdouble;
    upy: GLdouble;
    upz: GLdouble); stdcall; external GLU32DLL;

function gluProject(
    objx: GLdouble;
    objy: GLdouble;
    objz: GLdouble;
    const modelMatrix: Array4_4d;
    const projMatrix: Array4_4d;
    const viewport: Array4i;
    winx: PGLdouble;
    winy: PGLdouble;
    winz: PGLdouble): GLint; stdcall; external GLU32DLL;

function gluUnProject(
    winx: GLdouble;
    winy: GLdouble;
    winz: GLdouble;
    const modelMatrix: Array4_4d;
    const projMatrix: Array4_4d;
    const viewport: Array4i;
    objx: PGLdouble;
    objy: PGLdouble;
    objz: PGLdouble): GLint; stdcall; external GLU32DLL;

function gluScaleImage(
    format: GLenum;
    widthin: GLint;
    heightin: GLint;
    typein: GLenum;
    const datain: Pointer;
    widthout: GLint;
    heightout: GLint;
    typeout: GLenum;
    dataout: Pointer): GLint; stdcall; external GLU32DLL;

function gluBuild1DMipmaps(
    target: GLenum;
    components: GLint;
    width: GLint;
    format: GLenum;
    atype: GLenum;
    const data: Pointer): GLint; stdcall; external GLU32DLL;

function gluBuild2DMipmaps(
    target: GLenum;
    components: GLint;
    width: GLint;
    height: GLint;
    format: GLenum;
    atype: GLenum;
    const data: Pointer): GLint; stdcall; external GLU32DLL;

type
  GLUnurbs            = record end;
  GLUquadric          = record end;
  GLUtesselator       = record end;

  PGLUnurbs           = ^GLUnurbs;
  PGLUquadric         = ^GLUquadric;
  PGLUtesselator      = ^GLUtesselator;

(* backwards compatibility: *)
  GLUnurbsObj         = GLUnurbs;
  GLUquadricObj       = GLUquadric;
  GLUtesselatorObj    = GLUtesselator;
  GLUtriangulatorObj  = GLUtesselator;

  PGLUnurbsObj        = PGLUnurbs;
  PGLUquadricObj      = PGLUquadric;
  PGLUtesselatorObj   = PGLUtesselator;
  PGLUtriangulatorObj = PGLUtesselator;

(****           Callback function prototypes    ****)

(* gluQuadricCallback *)
  TGLUQuadricErrorProc = procedure(errorCode: GLenum); stdcall;

(* gluTessCallback *)
  TGLUtessCallbackProc = Pointer;
  TGLUtessBeginProc    = procedure(atype: GLenum); stdcall;
  TGLUtessEdgeFlagProc = procedure(flag: GLboolean); stdcall;
  TGLUtessVertexProc   = procedure(vertex_data: Pointer); stdcall;
  TGLUtessEndProc      = procedure; stdcall;
  TGLUtessErrorProc    = procedure(errno: GLenum); stdcall;
  TGLUtessCombineProc  = procedure(coords: Array3d;
                                   vertex_data: Array4p;
                                   weight: Array4f;
                                   var outData: PGLvoid); stdcall;
  TGLUtessBeginDataProc    = procedure(atype: GLenum; user_data: PGLvoid); stdcall;
  TGLUtessEdgeFlagDataProc = procedure(flag: GLboolean; user_data: PGLvoid); stdcall;
  TGLUtessVertexDataProc   = procedure(vertex_data: PGLvoid; user_data: PGLvoid); stdcall;
  TGLUtessEndDataProc      = procedure(user_data: PGLvoid); stdcall;
  TGLUtessErrorDataProc    = procedure(errno: GLenum; user_data: PGLvoid); stdcall;
  TGLUtessCombineDataProc  = procedure(coords: Array3d;
                                       vertex_data: Array4p;
                                       weight: Array4f;
                                       var outData: PGLvoid;
                                       user_data: PGLvoid); stdcall;

(* gluNurbsCallback *)
  TGLUNurbsErrorProc = procedure(errorCode: GLenum); stdcall;

function gluNewQuadric: PGLUquadric; stdcall; external GLU32DLL;

procedure gluDeleteQuadric(
    state: PGLUquadric); stdcall; external GLU32DLL;

procedure gluQuadricNormals(
    quadObject: PGLUquadric;
    normals: GLenum); stdcall; external GLU32DLL;

procedure gluQuadricTexture(
    quadObject: PGLUquadric;
    textureCoords: GLboolean); stdcall; external GLU32DLL;

procedure gluQuadricOrientation(
    quadObject: PGLUquadric;
    orientation: GLenum); stdcall; external GLU32DLL;

procedure gluQuadricDrawStyle(
    quadObject: PGLUquadric;
    drawStyle: GLenum); stdcall; external GLU32DLL;

procedure gluCylinder(
    quadObject: PGLUquadric;
    baseRadius: GLdouble;
    topRadius: GLdouble;
    height: GLdouble;
    slices: GLint;
    stacks: GLint); stdcall; external GLU32DLL;

procedure gluDisk(
    quadObject: PGLUquadric;
    innerRadius: GLdouble;
    outerRadius: GLdouble;
    slices: GLint;
    loops: GLint); stdcall; external GLU32DLL;

procedure gluPartialDisk(
    quadObject: PGLUquadric;
    innerRadius: GLdouble;
    outerRadius: GLdouble;
    slices: GLint;
    loops: GLint;
    startAngle: GLdouble;
    sweepAngle: GLdouble); stdcall; external GLU32DLL;

procedure gluSphere(
    quadObject: PGLUquadric;
    radius: GLdouble;
    slices: GLint;
    stacks: GLint); stdcall; external GLU32DLL;

procedure gluQuadricCallback (
    quadObject: PGLUquadric;
    which: GLenum;
    fn: TGLUQuadricErrorProc); stdcall; external GLU32DLL;

function gluNewTess: PGLUtesselator; stdcall; external GLU32DLL;

procedure gluDeleteTess(
    tess: PGLUtesselator); stdcall; external GLU32DLL;

procedure gluTessBeginPolygon(
    tess: PGLUtesselator;
    polygon_data: Pointer); stdcall; external GLU32DLL;

procedure gluTessBeginContour(
    tess: PGLUtesselator); stdcall; external GLU32DLL;

procedure gluTessVertex(
    tess: PGLUtesselator;
    coords: Array3d;
    data: Pointer); stdcall; external GLU32DLL;

procedure gluTessEndContour(
    tess: PGLUtesselator); stdcall; external GLU32DLL;

procedure gluTessEndPolygon(
    tess: PGLUtesselator); stdcall; external GLU32DLL;

procedure gluTessProperty(
    tess: PGLUtesselator;
    which: GLenum;
    value: GLdouble); stdcall; external GLU32DLL;

procedure gluTessNormal(
    tess: PGLUtesselator;
    x: GLdouble;
    y: GLdouble;
    z: GLdouble); stdcall; external GLU32DLL;

procedure gluTessCallback(
    tess: PGLUtesselator;
    which: GLenum;
    fn: TGLUTessCallbackProc); stdcall; external GLU32DLL;

procedure gluGetTessProperty(
    tess: PGLUtesselator;
    which: GLenum;
    value: PGLdouble); stdcall; external GLU32DLL;
 
function gluNewNurbsRenderer: PGLUnurbs; stdcall; external GLU32DLL;

procedure gluDeleteNurbsRenderer(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluBeginSurface(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluBeginCurve(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluEndCurve(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluEndSurface(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluBeginTrim(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluEndTrim(
    nobj: PGLUnurbs); stdcall; external GLU32DLL;

procedure gluPwlCurve(
    nobj: PGLUnurbs;
    count: GLint;
    points: PGLfloat;
    stride: GLint;
    atype: GLenum); stdcall; external GLU32DLL;

procedure gluNurbsCurve(
    nobj: PGLUnurbs;
    nknots: GLint;
    knot: PGLfloat;
    stride: GLint;
    ctlarray: PGLfloat;
    order: GLint;
    atype: GLenum); stdcall; external GLU32DLL;

procedure gluNurbsSurface(
    nobj: PGLUnurbs;
    sknot_count: GLint;
    sknot: PGLfloat;
    tknot_count: GLint;
    tknot: PGLfloat;
    s_stride: GLint;
    t_stride: GLint;
    ctlarray: PGLfloat;
    sorder: GLint;
    torder: GLint;
    atype: GLenum); stdcall; external GLU32DLL;

procedure gluLoadSamplingMatrices(
    nobj: PGLUnurbs;
    const modelMatrix: Array4_4f;
    const projMatrix: Array4_4f;
    const viewport: Array4i); stdcall; external GLU32DLL;

procedure gluNurbsProperty(
    nobj: PGLUnurbs;
    aproperty: GLenum;
    value: GLfloat); stdcall; external GLU32DLL;

procedure gluGetNurbsProperty(
    nobj: PGLUnurbs;
    aproperty: GLenum;
    value: PGLfloat); stdcall; external GLU32DLL;

procedure gluNurbsCallback(
    nobj: PGLUnurbs;
    which: GLenum;
    fn: TGLUNurbsErrorProc); stdcall; external GLU32DLL;

(****           Generic constants               ****)
const

(* Version *)
  GLU_VERSION_1_1                 = 1;
  GLU_VERSION_1_2                 = 1;

(* Errors: (return value 0 = no error) *)
  GLU_INVALID_ENUM                = 100900;
  GLU_INVALID_VALUE               = 100901;
  GLU_OUT_OF_MEMORY               = 100902;
  GLU_INCOMPATIBLE_GL_VERSION     = 100903;

(* StringName *)
  GLU_VERSION                     = 100800;
  GLU_EXTENSIONS                  = 100801;

(* Boolean *)
  GLU_TRUE                        = GL_TRUE;
  GLU_FALSE                       = GL_FALSE;


(****           Quadric constants               ****)

(* QuadricNormal *)
  GLU_SMOOTH                      = 100000;
  GLU_FLAT                        = 100001;
  GLU_NONE                        = 100002;

(* QuadricDrawStyle *)
  GLU_POINT                       = 100010;
  GLU_LINE                        = 100011;
  GLU_FILL                        = 100012;
  GLU_SILHOUETTE                  = 100013;

(* QuadricOrientation *)
  GLU_OUTSIDE                     = 100020;
  GLU_INSIDE                      = 100021;

(* Callback types: *)
(*      GLU_ERROR               100103 *)


(****           Tesselation constants           ****)

  GLU_TESS_MAX_COORD              = 1.0e150;

(* TessProperty *)
  GLU_TESS_WINDING_RULE           = 100140;
  GLU_TESS_BOUNDARY_ONLY          = 100141;
  GLU_TESS_TOLERANCE              = 100142;

(* TessWinding *)
  GLU_TESS_WINDING_ODD            = 100130;
  GLU_TESS_WINDING_NONZERO        = 100131;
  GLU_TESS_WINDING_POSITIVE       = 100132;
  GLU_TESS_WINDING_NEGATIVE       = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO    = 100134;

(* TessCallback *)
  GLU_TESS_BEGIN          = 100100; (* TGLUtessBeginProc        *)
  GLU_TESS_VERTEX         = 100101; (* TGLUtessVertexProc       *)
  GLU_TESS_END            = 100102; (* TGLUtessEndProc          *)
  GLU_TESS_ERROR          = 100103; (* TGLUtessErrorProc        *)
  GLU_TESS_EDGE_FLAG      = 100104; (* TGLUtessEdgeFlagProc     *)
  GLU_TESS_COMBINE        = 100105; (* TGLUtessCombineProc      *)
  GLU_TESS_BEGIN_DATA     = 100106; (* TGLUtessBeginDataProc    *)
  GLU_TESS_VERTEX_DATA    = 100107; (* TGLUtessVertexDataProc   *)
  GLU_TESS_END_DATA       = 100108; (* TGLUtessEndDataProc      *)
  GLU_TESS_ERROR_DATA     = 100109; (* TGLUtessErrorDataProc    *)
  GLU_TESS_EDGE_FLAG_DATA = 100110; (* TGLUtessEdgeFlagDataProc *)
  GLU_TESS_COMBINE_DATA   = 100111; (* TGLUtessCombineDataProc  *)

(* TessError *)
  GLU_TESS_ERROR1     = 100151;
  GLU_TESS_ERROR2     = 100152;
  GLU_TESS_ERROR3     = 100153;
  GLU_TESS_ERROR4     = 100154;
  GLU_TESS_ERROR5     = 100155;
  GLU_TESS_ERROR6     = 100156;
  GLU_TESS_ERROR7     = 100157;
  GLU_TESS_ERROR8     = 100158;

  GLU_TESS_MISSING_BEGIN_POLYGON  = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR  = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON    = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR    = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE        = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK  = GLU_TESS_ERROR6;

(****           NURBS constants                 ****)

(* NurbsProperty *)
  GLU_AUTO_LOAD_MATRIX            = 100200;
  GLU_CULLING                     = 100201;
  GLU_SAMPLING_TOLERANCE          = 100203;
  GLU_DISPLAY_MODE                = 100204;
  GLU_PARAMETRIC_TOLERANCE        = 100202;
  GLU_SAMPLING_METHOD             = 100205;
  GLU_U_STEP                      = 100206;
  GLU_V_STEP                      = 100207;

(* NurbsSampling *)
  GLU_PATH_LENGTH                 = 100215;
  GLU_PARAMETRIC_ERROR            = 100216;
  GLU_DOMAIN_DISTANCE             = 100217;


(* NurbsTrim *)
  GLU_MAP1_TRIM_2                 = 100210;
  GLU_MAP1_TRIM_3                 = 100211;

(* NurbsDisplay *)
(*      GLU_FILL                100012 *)
  GLU_OUTLINE_POLYGON             = 100240;
  GLU_OUTLINE_PATCH               = 100241;

(* NurbsCallback *)
(*      GLU_ERROR               100103 *)

(* NurbsErrors *)
  GLU_NURBS_ERROR1                = 100251;
  GLU_NURBS_ERROR2                = 100252;
  GLU_NURBS_ERROR3                = 100253;
  GLU_NURBS_ERROR4                = 100254;
  GLU_NURBS_ERROR5                = 100255;
  GLU_NURBS_ERROR6                = 100256;
  GLU_NURBS_ERROR7                = 100257;
  GLU_NURBS_ERROR8                = 100258;
  GLU_NURBS_ERROR9                = 100259;
  GLU_NURBS_ERROR10               = 100260;
  GLU_NURBS_ERROR11               = 100261;
  GLU_NURBS_ERROR12               = 100262;
  GLU_NURBS_ERROR13               = 100263;
  GLU_NURBS_ERROR14               = 100264;
  GLU_NURBS_ERROR15               = 100265;
  GLU_NURBS_ERROR16               = 100266;
  GLU_NURBS_ERROR17               = 100267;
  GLU_NURBS_ERROR18               = 100268;
  GLU_NURBS_ERROR19               = 100269;
  GLU_NURBS_ERROR20               = 100270;
  GLU_NURBS_ERROR21               = 100271;
  GLU_NURBS_ERROR22               = 100272;
  GLU_NURBS_ERROR23               = 100273;
  GLU_NURBS_ERROR24               = 100274;
  GLU_NURBS_ERROR25               = 100275;
  GLU_NURBS_ERROR26               = 100276;
  GLU_NURBS_ERROR27               = 100277;
  GLU_NURBS_ERROR28               = 100278;
  GLU_NURBS_ERROR29               = 100279;
  GLU_NURBS_ERROR30               = 100280;
  GLU_NURBS_ERROR31               = 100281;
  GLU_NURBS_ERROR32               = 100282;
  GLU_NURBS_ERROR33               = 100283;
  GLU_NURBS_ERROR34               = 100284;
  GLU_NURBS_ERROR35               = 100285;
  GLU_NURBS_ERROR36               = 100286;
  GLU_NURBS_ERROR37               = 100287;

(****           Backwards compatibility for old tesselator           ****)

procedure gluBeginPolygon(tess: PGLUtesselator); stdcall; external GLU32DLL;

procedure gluNextContour(tess: PGLUtesselator;
                         atype: GLenum); stdcall; external GLU32DLL;

procedure gluEndPolygon(tess: PGLUtesselator); stdcall; external GLU32DLL;

(* Contours types -- obsolete! *)
const
  GLU_CW                  = 100120;
  GLU_CCW                 = 100121;
  GLU_INTERIOR            = 100122;
  GLU_EXTERIOR            = 100123;
  GLU_UNKNOWN             = 100124;

(* Names without "TESS_" prefix *)
  GLU_BEGIN               = GLU_TESS_BEGIN;
  GLU_VERTEX              = GLU_TESS_VERTEX;
  GLU_END                 = GLU_TESS_END;
  GLU_ERROR               = GLU_TESS_ERROR;
  GLU_EDGE_FLAG           = GLU_TESS_EDGE_FLAG;

implementation

{$IFDEF UNICODE}
function gluErrorStringWIN(errCode: GLenum): PWideChar;
begin
  Result := gluErrorUnicodeStringEXT(errCode);
end;
{$ELSE}
function gluErrorStringWIN(errCode: GLenum): PGLubyte;
begin
  Result := gluErrorString(errCode);
end;
{$ENDIF}

end.
