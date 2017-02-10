unit GraphObjects;

{
  Graphical objects and modification routines as part of my Voronoi algorithm
  Copyright (C) 2002 Christian Huettig
  Released: 08/08/2002
  Restrictions: None
  Contact: snakers@gmx.net

  This source is free; you can redistribute it and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
  This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along with this program;
  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
}

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics;

type
  TRGB = packed record // Varianten record to mix colors fast
    case boolean of
      true:
        (Color: LongWord); // 32bit Color value (like TColor)
      false:
        (R, G, B, A: Byte); // 8bit RGBA split, alpha isn't used
  end;

  TGLineState = (TwoPoint, OnePoint, Vector);
  // |------|, |----->, <---|------|----> (Vector isn't the right name, its an endless line in both directions marked through two point)

  // Graphical Objects
  TGraphObject = class // Base object for all drawable elements + management
  private
    orig_index: integer;
    // Index of list at create time (movetolist and clonetolist don't change this index, only set on create
    index: integer; // Index of current list
    List: TList;
    // Current list, can only be changed through movetolist or clonetolist
    Canvas: TCanvas;
  protected
    constructor Create(L: TList; C: TCanvas);
  public
    Color: TRGB;
    procedure MoveToList(L: TList);
    procedure CopyToList(L: TList);
    // same as move, but now the object is on more than one list. careful, because index is left on old list !
    function CloneToList(L: TList): TGraphObject;
    // Creates a new object and moves it to L
    function GetIndex: integer;
    function GetOrigIndex: integer;
    // returns the index of the list were it was created
    procedure Delete(orig: boolean);
    // orig=true means that the object is in its "original" list. now reindexes also orig_index
    procedure SetCanvas(C: TCanvas);
    function GetCanvas: TCanvas;
    procedure ReIndex(orig: boolean); overload;
    // slow reindex by searching the list for "self"
    procedure ReIndex(orig: boolean; i: integer); overload;
    // fast reindex with validation
    procedure Draw; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Clone: TGraphObject; virtual; abstract;
  end;

  TGPoint = class; // forward declaration, with seperate units impossible !

  TGLine = class(TGraphObject)
  private
    state: TGLineState; // see above
    d, dx, dy: extended; // d=distance, dx,dy=delta
    ix, iy, t, R: extended;
    // ex,ey=unity vector, ix,iy=crosspoint(set after Intersect), t,r=distances to ipoint
    procedure initialize; // evaluates all constants if line has changed
  public
    p1, p2: TGPoint;
    ex, ey: extended; // Unity vector of the line
    BisectorOf: array [1 .. 2] of integer;
    // The orig_index of the points from which this line is the bisector. -1 if none
    constructor Create(p1_, p2_: TGPoint; s: TGLineState; L: TList; C: TCanvas);
    function Clone: TGraphObject; override;

    procedure Draw; override;
    procedure Clear; override;

    function GetState: TGLineState;
    function Intersect(ln: TGLine): boolean;
    procedure GetCurrentIPoint(var x, y: extended); overload;
    // copies ix and iy. only valid after intersect() call !
    procedure GetCurrentIPoint(var p: TGPoint); overload;
    // copies ix and iy to a point. only valid after intersect() call !
    procedure CutRight(ln: TGLine); // Cuts the line right on ln
    procedure CutLeft(ln: TGLine);
    procedure CutBoth(ln: TGLine);
  end;

  TGPoint = class(TGraphObject)
  private
    x, y: extended;
  public
    closeDist: extended;
    // distance to point for MatchPoint=true (0=exact match)
    constructor Create(x_, y_: extended; L: TList; C: TCanvas);
    function Clone: TGraphObject; override;

    procedure Draw; override;
    procedure Clear; override;

    function getX: extended;
    function getY: extended;
    function DistanceTo(p: TGPoint): extended; overload;
    function DistanceTo(x_, y_: extended): extended; overload;
    procedure MoveTo(x_, y_: extended);
    function Match(p: TGPoint): boolean; overload;
    function Match(x_, y_: extended): boolean; overload;
    function Angle(p: TGPoint): extended;
    // required for the convex hull (preparata-hong)
    function IsRightTurn(p1, p2: TGPoint): boolean;
    // required for Graham scan (discarded, but left for further use)
    function areCollinear(A, B: TGPoint): boolean;
    function Bisector(p: TGPoint): TGLine;
    // Creates a line and sets BisectorOf[1..2]
    function CircleCenter(A, B: TGPoint): TGPoint; // never used
  end;

implementation

uses
  main;

// to keep me from getting insane, i splitted this giant.
// Hint: Press Ctrl+Enter while the cursor is on the filename to open it.

{$I GraphObject.pas}
{$I Line.pas}
{$I Point.pas}

end.
