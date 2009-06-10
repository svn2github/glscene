{: GLLinGTKContext<p>

   GTK specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>10/06/09 - DanB - Added to main GLScene CVS repository (from GLScene-Lazarus).
      <li>14/01/05 - CU - Creation
   </ul></font>
}
unit GLLinGTKContext;

interface

{$i GLScene.inc}

uses
  xlib, Classes, sysutils, GLCrossPlatform, GLContext, OpenGL1x,
  x, xutil, GLGLXContext, GTKProc,
{$ifdef LCLGTK2}
  gtk2, gdk2, gdk2x, gtkdef;
{$else}
  gtk, gtkdef, gdk;
{$endif}

type
   // TGLGTKContext
   //
   {: A context driver for GLX, retrieving X handles from GTK. }
   TGLLinGTKContext = class (TGLGLXContext)
      protected
         { Protected Declarations }
         procedure DoGetHandles(outputDevice: Cardinal; out XDisp: Pointer; out XWin: LongInt); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure TGLLinGTKContext.DoGetHandles(outputDevice: Cardinal; out XDisp: Pointer; out XWin: LongInt);
var
  FGTKWidget : PGTKWidget;
begin
    fGTKWidget := GetFixedWidget(TGtkDeviceContext(outputDevice).Widget);
    // Dirty workaround: force realize
    gtk_widget_realize(FGTKWidget);

    // GTK1 -- old
    {$ifdef LCLGTK2}
    gtk_widget_set_double_buffered(FGTKWidget, False);
    XDisp:=GDK_WINDOW_XDISPLAY (PGdkDrawable(fGTKWidget^.window));
    XWin:=GDK_WINDOW_XWINDOW (PGdkDrawable(fGTKWidget^.window));
    {$else}
    XDisp:=GDK_WINDOW_XDISPLAY (PGdkWindowPrivate(fGTKWidget^.window));
    XWin:=GDK_WINDOW_XWINDOW (PGdkWindowPrivate(fGTKWidget^.window));
    {$endif}
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLLinGTKContext);

end.
