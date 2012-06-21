package com.pascal.lcltest;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.*;
import android.graphics.*;
import android.text.*;
import android.view.*;
import android.view.inputmethod.*;
import android.content.res.Configuration;
//import android.content.Intent;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
//import java.util.*;

import javax.microedition.khronos.egl.*;
import javax.microedition.khronos.opengles.GL;

public class LCLActivity extends Activity implements SensorEventListener
{
  // -------------------------------------------
  // Input connection to get character events
  // -------------------------------------------
  private class LCLInputConnection extends BaseInputConnection
  {
    private SpannableStringBuilder _editable;
    View _lclView;

    public LCLInputConnection(View targetView, boolean fullEditor)
    {
      super(targetView, fullEditor);
      _lclView = (View) targetView;
    }

/*    public Editable getEditable()
    {
      if (_editable == null)
      {
        _editable = (SpannableStringBuilder) Editable.Factory.getInstance()
        .newEditable("Placeholder");
      }
      return _editable;
    } This crashes in HTC */

    // This method sends a text to be added at the current cursor position
    @Override public boolean commitText(CharSequence text, int newCursorPosition)
    {
      //if (_editable != null) _editable.append(text);
      Log.v("lclproject", "LCLInputConnection.commitText =" + text + " newCursorPosition=" + Integer.toString(newCursorPosition));

      // Send each character of the string
      int eventResult, i;
      for (i = 0; i<text.length(); i++)
      {
        eventResult = LCLOnKey(-1, 0, null, (int) text.charAt(i));
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }
      return true;
    }

    @Override public boolean deleteSurroundingText(int leftLength, int rightLength)
    {
      Log.v("lclproject", "LCLInputConnection.deleteSurroundingText left=" + Integer.toString(leftLength) + " right=" + Integer.toString(rightLength));

      // For each left surrounding text deletion, send a backspace key
      int eventResult, i;
      for (i = 0; i<leftLength; i++)
      {
        eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
        eventResult = LCLOnKey(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }

      // For each right surrounding text deletion, send a del key
      // KEYCODE_FORWARD_DEL Since: API Level 11
      /*for (i = 0; i<leftLength; i++)
      {
        eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_FORWARD_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
        eventResult = LCLOnKey(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_FORWARD_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }*/

      return super.deleteSurroundingText(leftLength, rightLength);
    }
  }

  // -------------------------------------------
  // Our drawing surface
  // -------------------------------------------
  private class LCLSurface extends SurfaceView  implements SurfaceHolder.Callback 
  {
	SurfaceHolder aholder;
	    
    public LCLSurface(Context context)
    {
      super(context);
     // SetFullScreen(false);
     // glsrender= new EglHelper();
      init();
      // Allows View.postInvalidate() to work
      setWillNotDraw(false);
      // We already double buffer, so no need for a second one
      setWillNotCacheDrawing(true);
      // Set focus on us to get keyboard events
      requestFocus();
      setFocusableInTouchMode(true);
    }
    
    private void init() {
        // Install a SurfaceHolder.Callback so we get notified when the
        // underlying surface is created and destroyed
        aholder = getHolder();
        aholder.addCallback(this);
        aholder.setSizeFromLayout();
        if (LOG_EGL) {
            Log.v("EglHelper","init()" );
        }
        // setFormat is done by SurfaceView in SDK 2.3 and newer. Uncomment
        // this statement if back-porting to 2.2 or older:
        // holder.setFormat(PixelFormat.RGB_565);
        //
        // setType is not needed for SDK 2.0 or newer. Uncomment this
        // statement if back-porting this code to older SDKs.
        // holder.setType(SurfaceHolder.SURFACE_TYPE_GPU);
    }

	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int width,
			int height) {
        if (LOG_EGL) {
            Log.v("EglHelper","surfaceChanged()" );
        }
        Log.v("LCLActivity", "surfaceChanged: width"+width+"height"+height);
      LCLOnSurfaceChanged();  
		// TODO Auto-generated method stub
		
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder) {
        if (LOG_EGL) {
            Log.v("EglHelper","surfaceCreated()" );
        }
        
            // TODO ВНИМАНИЕ!!! Если возникнет необходимость введения PBufferа, LCLDoStartEGL 
            //нужно переместить в OnCreane 
            //FinishGL в onDestroy однако, событие onSurfaceCreated может возникнуть с задержкой и появится после
            //onDestroy тем самым вызвав GLError, по этому нужно более детально обдумать вызов метода.
            LCLDoStartEGL();
            LCLOnSurfaceCreated();
		// TODO Auto-generated method stub
	//	glsrender.start();
    //	gl = (GL10) glsrender.createSurface(holder);
	//  glsrender.purgeBuffers();
        //glsrender.start();
       // glsrender.createcontext();
      //  glsrender.createSurface(holder);
      //  glsrender.purgeBuffers();
		
	//	((GL10) gl).glViewport(0, 0, 100, 100);
        // define the color we want to be displayed as the "clipping wall"
    //    ((GL10) gl).glClearColor(_red, _green, _blue, 1.0f);
        // clear the color buffer to show the ClearColor we called above...
     //   ((GL10) gl).glClear(GL10.GL_COLOR_BUFFER_BIT);
		
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder) {
        if (LOG_EGL) {
            Log.v("EglHelper","surfaceDestroyed()" );
        }
            LCLOnSurfaceDestroyed(); 
  	        LCLDoFinishEGL();
		// TODO Auto-generated method stub
  
		//glsrender.destroySurface();
		//glsrender.finish();
	}    
    
    
    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (LOG_EGL) {
        Log.i("LCLSurface", "onAttachedToWindow");
        }
    }     

    @Override protected void onDraw(Canvas canvas)
    { if (LOG_EGL) {
      Log.i("lclapp", "onDraw started");
      }
      SendScreenChanges(true);
      //Log.v("lclproject", "LCLSurface.onDraw width=" + Integer.toString(lWidth)
      //  + " height=" + Integer.toString(lHeight));
 
      //Bitmap localbitmap = Bitmap.createBitmap(lWidth, lHeight, Bitmap.Config.ARGB_8888);
      //LCLDrawToBitmap(lWidth, lHeight, localbitmap);
      LCLOnDraw(lclformwidth, lclformheight);
     // canvas.drawBitmap(localbitmap, 0, 0, null);
      //Log.i("lclapp", "onDraw finished");
    }

    @Override public boolean onKeyDown (int keyCode, KeyEvent event)
    {
      //Log.v("lclproject", "LCLSurface.onKeyDown");
      super.onKeyDown(keyCode, event);
      int eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, keyCode, event, (char) 0);
      if ((eventResult & 1) != 0) postInvalidate();
      return true;
    }

    @Override public boolean onKeyUp (int keyCode, KeyEvent event)
    {
      // First handle the KeyUp event
      int eventResult = LCLOnKey(KeyEvent.ACTION_UP, keyCode, event, event.getUnicodeChar());
      if ((eventResult & 1) != 0) postInvalidate();

      // Handling of the Back hardware key
      super.onKeyUp(keyCode, event);
      if ((eventResult & 2) != 0)
      {
        //Log.v("lclproject", "BackKey going to home");
        finish();
        return false; // From the docs it seams that only returning false should do it, but calling finish() is really necessary
      }
      else
      {
        //Log.v("lclproject", "BackKey not going to home");
        return true;
      }
    }

    @Override public boolean onTouchEvent (MotionEvent event)
    {
      int eventResult = LCLOnTouch(event.getX(), event.getY(), event.getAction());
      if ((eventResult | 1) != 0) postInvalidate();
      return true;
    }

    @Override public InputConnection onCreateInputConnection(EditorInfo outAttrs)
    {
      outAttrs.actionLabel = null;
      outAttrs.label = "Test text";
      outAttrs.inputType = InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS;
      outAttrs.imeOptions = EditorInfo.IME_ACTION_DONE;
      return new LCLInputConnection(this, true);
    }

    @Override public boolean onCheckIsTextEditor()
    {
      return true;
    }
  }

  // Global objects
  LCLSurface lclsurface;
  SensorManager localSensorManager;

  // Utility routines
  public static double[] convertFloatsToDoubles(float[] input)
  {
    if (input == null) return null;
    double[] output = new double[input.length];
    for (int i = 0; i < input.length; i++)
    {  output[i] = input[i]; }
    return output;
  }

  public void ProcessEventResult(int eventResult)
  {
    if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
    //if ((eventResult | 2) != 0) reserved for BACK key handling
  }
  
  // -------------------------------------------
  // Activity Events
  // -------------------------------------------

  /** Called when the activity is first created. */
  @Override public void onCreate(Bundle savedInstanceState)
  {
    super.onCreate(savedInstanceState);
    Log.v("LCLActivity", "Activity onCreate");
    
 
   
    lclsurface = new LCLSurface(this);
   // GLSRenderer glsrenderer;
   // glsrenderer = new GLSRenderer();
   // lclsurface.setRenderer(glsrenderer);

    
   
    // Tell the LCL that an OnCreate has happened and what is our instance
  //предварительное сохранение параметров экрана без вызова собятия
    SendScreenChanges(false); 
    LCLOnCreate(this);
    
    setContentView(lclsurface);
    lclsurface.postInvalidate();

  }
  
  @Override protected void onDestroy()
  {
	  super.onDestroy();
	    Log.v("LCLActivity", "Activity onDestroy");
  }
  @Override protected void onStart()
  {
	  super.onStart(); 
	  Log.v("LCLActivity", "Activity onStart"); 
	  SendScreenChanges(true);
  }

  @Override public void onConfigurationChanged (Configuration newConfig)
  {
    super.onConfigurationChanged(newConfig);
    
    Log.v("LCLActivity", "onConfigurationChanged");
    
    
    SendScreenChanges(true);
    // Don't call LCLOnConfigurationChanged, wait for a onDraw instead
    //lclsurface.postInvalidate();
    //Log.i("lclapp", "onConfigurationChanged finished");
    
  }

  @Override
  protected void onResume() {
    super.onResume();
	  Log.v("LCLActivity", "Activity onResume"); 
	  lclsurface.setVisibility(1);
   // lclsurface.onResume();
  }

  @Override
  protected void onPause() {
    super.onPause();
	  Log.v("LCLActivity", "Activity onPause"); 
	  lclsurface.setVisibility(0);
   // lclsurface.onPause();
  }


  // -------------------------------------------
  // JNI table of Pascal functions
  // -------------------------------------------
  public native int LCLOnDraw(int width, int height);
  public native int LCLOnTouch(float x, float y, int action);
  public native int LCLOnCreate(LCLActivity lclactivity);
  public native int LCLOnMessageBoxFinished(int Result);
  public native int LCLOnKey(int kind, int keyCode, KeyEvent event, int AChar);
  public native int LCLOnTimer(Runnable timerid);
  public native int LCLOnConfigurationChanged(int ANewxDPI,int ANewyDPI, int ANewWidth,int ANewHeight);
  public native int LCLOnSensorChanged(int ASensorKind, double[] AValues);
  
  public native void LCLOnSurfaceCreated();
  public native void LCLOnSurfaceDestroyed(); 
  public native void LCLOnSurfaceChanged();   
  
  

  // -------------------------------------------
  // Functions exported to the Pascal side
  // -------------------------------------------

  // input: String lcltext, int lcltextsize
  // output: int lclwidth, int lclheight, int lclascent, etc
  public void LCLDoGetTextBounds()
  {
    Paint localpaint = new Paint();
    Rect localbounds = new Rect();
    localpaint.setTextSize(lcltextsize);
    localpaint.getTextBounds(lcltext, 0, lcltext.length(), localbounds);
    lclwidth = localbounds.width();
    // Painter.getTextBounds consistently gives us a too small size, so work around that
    lclwidth = lclwidth + (3 * lcltextsize) / 16;
    // Don't use just localbounds.height() from the source text
    // because it will calculate the minimum necessary height,
    // but we can't easily use that to draw text because it draws relative to the baseline
    localpaint.getTextBounds("Íqg", 0, 3, localbounds);
    lclheight = localbounds.height();
    // Also get some measures
    lcltextascent = (int) localpaint.getFontMetrics().ascent;
    lcltextbottom = (int) localpaint.getFontMetrics().bottom;
    lcltextdescent = (int) localpaint.getFontMetrics().descent;
    lcltextleading = (int) localpaint.getFontMetrics().leading;
    lcltexttop = (int) localpaint.getFontMetrics().top;
  }

  // input: String lcltext, int lclmaxwidth, int lcltextsize
  // output: int lclmaxcount
  public void LCLDoGetTextPartialWidths()
  {
    Paint localpaint = new Paint();
    localpaint.setTextSize(lcltextsize);

    float localmaxwidth = (float) lclmaxwidth;
    //Log.i("lclapp", "[LCLDoGetTextPartialWidths] lcltext="+lcltext+" localmaxwidth="+Float.toString(localmaxwidth));
    lclmaxcount = localpaint.breakText(lcltext, true, localmaxwidth, lclpartialwidths);
  }

  // input: String lcltext, int lclwidth, int lclheight
  // output: lclbitmap
  public void LCLDoDrawText(int ATextColor)
  {
    lclbitmap = Bitmap.createBitmap(lclwidth, lclheight, Bitmap.Config.ARGB_8888);
    Canvas localcanvas = new Canvas(lclbitmap);
    Paint localpaint = new Paint();
    localpaint.setColor(ATextColor);
    localpaint.setTextSize(lcltextsize);
    localpaint.setFlags(Paint.ANTI_ALIAS_FLAG);
    localcanvas.drawColor(Color.TRANSPARENT); // TRANSPARENT
    // The Y coordinate is the lower baseline of letters like "abc"
    // see http://code.google.com/p/android/issues/detail?id=393
    localcanvas.drawText(lcltext, 0, lclheight - lcltextbottom, localpaint);
  }

  // LCLType definitions

  private final int idButtonBase = 0x00000000;
  private final int idButtonOk = 0x00000001;
  private final int idButtonCancel = 0x00000002;
  private final int idButtonHelp = 0x00000003;
  private final int idButtonYes = 0x00000004;
  private final int idButtonNo = 0x00000005;
  private final int idButtonClose = 0x00000006;
  private final int idButtonAbort = 0x00000007;
  private final int idButtonRetry = 0x00000008;
  private final int idButtonIgnore = 0x00000009;
  private final int idButtonAll = 0x0000000A;
  private final int idButtonYesToAll = 0x0000000B;
  private final int idButtonNoToAll = 0x0000000C;
  private final int idButtonOpen = 0x0000000D;
  private final int idButtonSave = 0x0000000E;
  private final int idButtonShield = 0x0000000F;

  // input: String lcltext, String lcltitle, int lclconfig (buttons)
  // output: nothing, but calles LCLOnMessageBoxFinished
  public void LCLDoShowMessageBox()
  {
    DialogInterface.OnClickListener dialogClickListener = new DialogInterface.OnClickListener()
    {
      @Override
      public void onClick(DialogInterface dialog, int which)
      {
        switch (which)
        {
        case DialogInterface.BUTTON_POSITIVE:
          LCLOnMessageBoxFinished(lclbutton1);
          break;
        case DialogInterface.BUTTON_NEUTRAL:
          LCLOnMessageBoxFinished(lclbutton2);
          break;
        case DialogInterface.BUTTON_NEGATIVE:
          LCLOnMessageBoxFinished(lclbutton3);
          break;
        };
      }
    };

    DialogInterface.OnCancelListener dialogCancelListener = new DialogInterface.OnCancelListener()
    {
      @Override
      public void onCancel(DialogInterface dialog)
      {
        // The Cancel button number matches for LCLIntf.MessageBox and LCLIntf.PromptDialog
        LCLOnMessageBoxFinished(idButtonCancel);
      }
    };

    AlertDialog.Builder builder = new AlertDialog.Builder(this);
    builder.setMessage(lcltext);
    builder.setTitle(lcltitle);
    if (lclbutton1 >= 0) builder.setPositiveButton(lclbutton1str, dialogClickListener);
    if (lclbutton2 >= 0) builder.setNeutralButton(lclbutton2str, dialogClickListener);
    if (lclbutton3 >= 0) builder.setNegativeButton(lclbutton3str, dialogClickListener);
    builder.show().setOnCancelListener(dialogCancelListener);
  };

  private Handler LocalHandler = new Handler();

  private class LCLRunnable implements Runnable
  {
    public boolean Destroyed = false;

    public void run()
    {
      int eventResult = LCLOnTimer(this);
      ProcessEventResult(eventResult);
      if (this.Destroyed == false) LocalHandler.postDelayed(this, lcltimerinterval);
    }
  };
    
  void SendScreenChanges(boolean sendchanges)
  {	  	
	  int statusBarHeight = 0;  
	  int titleBarHeight = 0;  
	  
  	  Window win = getWindow();
  	  
	    if (sendchanges==true)
	    {
	    	Rect rect = new Rect();
	    	win.getDecorView().getWindowVisibleDisplayFrame(rect);
	    	statusBarHeight = rect.top;
	    	int contentViewTop =
	    			win.findViewById(Window.ID_ANDROID_CONTENT).getTop();	
	    	titleBarHeight = contentViewTop - statusBarHeight;  
	    }
	  
	  
	    DisplayMetrics metrics = new DisplayMetrics();
	    getWindowManager().getDefaultDisplay().getMetrics(metrics);
	    lclxdpi = (int) metrics.xdpi;
	    lclydpi = (int) metrics.ydpi;
	    
	    int lWidth=0;
	    int lHeight=0;
	    
	     WindowManager.LayoutParams winParams = win.getAttributes();
	     int flag = WindowManager.LayoutParams.FLAG_FULLSCREEN; 
	     
	     //Если выставлен режим фуллскрина то лучше размеры не менять
	     //если же такого нету то из полного размера экрана нуно вычесть размеры титл бара и строки событий
	     //ну там где часики батарейка и прочая чюшь.
	    if ((winParams.flags&flag)==flag)
	    {
    	    lWidth = metrics.widthPixels;
    	    lHeight = metrics.heightPixels;      	     	
	    } else
	    {
	    
    	/*	lWidth = metrics.widthPixels;
    		lHeight = metrics.heightPixels;    
	    	int ot = getResources().getConfiguration().orientation;
	    	switch(ot)
	    	{
	    	case  Configuration.ORIENTATION_PORTRAIT:
	    		lWidth = metrics.widthPixels -statusBarHeight-titleBarHeight;
	    		lHeight = metrics.heightPixels;
	    		break;
	    	case Configuration.ORIENTATION_LANDSCAPE:*/
	    		lWidth = metrics.widthPixels ;
	    		lHeight = metrics.heightPixels -statusBarHeight-titleBarHeight;
	    /*		break;	 

	    	}*/
	    }
	    
	    int oldlclformwidth = lclformwidth;
	    int oldlclformheight = lclformheight; 
	    
	    lclformwidth = lWidth;
	    lclformheight = lHeight;
	    lclscreenwidth = lclformwidth;
	    lclscreenheight = lclformheight;

	      // Check if we rotated in the draw event, OnConfigurationChanged can't return the new form width =(
	      // see http://stackoverflow.com/questions/2524683/how-to-get-new-width-height-of-root-layout-in-onconfigurationchanged
	    if (sendchanges==true)
	    {
	      if ((lWidth != oldlclformwidth)||(lHeight != oldlclformheight)) LCLOnConfigurationChanged(lclxdpi,lclydpi, lWidth, lHeight); // we send xdpi because thats what the LCL uses for Screen.PixelsPerInch
	    }
  }
  

  // input:  int lcltimerinterval in milliseconds
  // output:  Runnable lcltimerid
  public void LCLDoCreateTimer()
  {
    lcltimerid = new LCLRunnable();

    LocalHandler.removeCallbacks(lcltimerid);
    LocalHandler.postDelayed(lcltimerid, lcltimerinterval);
  };

  // input: Runnable lcltimerid
  public void LCLDoDestroyTimer()
  {
    LocalHandler.removeCallbacks(lcltimerid);
    ((LCLRunnable) lcltimerid).Destroyed = true;
  };

  public void LCLDoHideVirtualKeyboard()
  {
    InputMethodManager localInputManager = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
    localInputManager.hideSoftInputFromWindow(lclsurface.getWindowToken(), 0);
  };

  public void LCLDoShowVirtualKeyboard()
  {
    InputMethodManager localInputManager = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
    localInputManager.showSoftInput(lclsurface, 0);
  };
  
  
  public void LCLDoInvalidate()
  {  
	    lclsurface.postInvalidate();
  };
  
  public void LCLDoCreateHolder()
  {  
	    lclsurface.setVisibility(1);
  };  
  
  public void LCLDoDestroyHolder()
  {  
	    lclsurface.setVisibility(0);
  }; 
  
  public void LCLDoRecreateHolder()
  {  
	    lclsurface.setVisibility(0);
	    lclsurface.setVisibility(1);	    
  };  
  
  public boolean LCLisHolderCreated()
  {  
	  return  lclsurface.aholder.isCreating();
  }; 
  
  public void LCLSetTitleBar(String str)
  {  
      if (LOG_EGL) {
		  Log.w("EglHelper", "LCLSetTitleBar("+str+") tid=" + Thread.currentThread().getId());
	  }	 
	  this.setTitle(str);
  }; 
  
  //переход в фуллскрин
  public void SetFullScreen(boolean on) {
      if (LOG_EGL) {
		  Log.w("EglHelper", "SetFullScreen("+on+") tid=" + Thread.currentThread().getId());
	  }	 
      Window win = getWindow();
      WindowManager.LayoutParams winParams = win.getAttributes();
      final int bits = WindowManager.LayoutParams.FLAG_FULLSCREEN;
      if (on) {
              winParams.flags |=  bits;
             requestWindowFeature(Window.FEATURE_NO_TITLE);
      } else {
              winParams.flags &= ~bits;
              requestWindowFeature(Window.FEATURE_OPTIONS_PANEL);
      }
      win.setAttributes(winParams);
  }
  
  //Смена режимов экрана
  public boolean SetScreenOrientation(int Orientation) {
      if (LOG_EGL) {
		  Log.w("EglHelper", "ScreenOrientation("+Orientation+") tid=" + Thread.currentThread().getId());
	  }	
      /*
      soDefault   -1
      soLandscape  0
      soPortrait   1
      soUser       2
      soBehind     3
      soSensor     4
      soNoSensor   5
      */
      this.setRequestedOrientation(Orientation-1);
      
      
     return true; 
  }
  
  //Динамическая загрузка библиотеки
  //При повторной загрзке выдаст ошибку мол приложение уже загружено
  public boolean LCLLoadLibrary(String lib) {
      if (LOG_EGL) {
		  Log.i("EglHelper", "LCLLoadLibrary: Trying to load "+lib+") tid=" + Thread.currentThread().getId());
	  }	  
      String s=lib.substring(3,lib.indexOf("."
    		  ));
        try 
        {
          System.loadLibrary(s);       
        }
        catch(UnsatisfiedLinkError ule) 
        {
        
          Log.e("lclapp", "WARNING: Could not load "+ s);
          ule.printStackTrace();
          return false;  
        }     
        
     return true; 
  }
 
  
  void LCLDoStartEGL()
  {   
	  //Starting on Create Activity
      if (LOG_EGL) {
		  Log.w("EglHelper", "start() tid=" + Thread.currentThread().getId());
	  }
	  /*
	   * Get an EGL instance
	   */
	  mEgl = (EGL10) EGLContext.getEGL();

	  /*
	   * Get to the default display.
	   */
	  mEglDisplay = mEgl.eglGetDisplay(EGL10.EGL_DEFAULT_DISPLAY);

	  if (mEglDisplay == EGL10.EGL_NO_DISPLAY) {
		  throw new RuntimeException("eglGetDisplay failed");
	  }
	  lclholder = lclsurface.aholder;
	  
	  /*
	   * We can now initialize EGL for that display
	   */
	  int[] version = new int[2];
	  if(!mEgl.eglInitialize(mEglDisplay, version)) {
		  throw new RuntimeException("eglInitialize failed");
	  }
	 
	  //Версия egl
	  String s = mEgl.eglQueryString(mEglDisplay, EGL10.EGL_VERSION);
	  if (s.indexOf("1.")>=0)
	  {	  
	    s = s.substring(s.indexOf("1."),s.indexOf("1.")+3);
	    lclmajorversion = Integer.parseInt( s.substring(0,1));
	    lclminorversion = Integer.parseInt( s.substring(2,3));
	  }else
	  {
		  lclmajorversion = 1;
		  lclminorversion = 0;  
	  }
	 
      //Информация о платформе
 
	  lclplatformversion = android.os.Build.VERSION.RELEASE;
	  lclplatformapi = android.os.Build.VERSION.SDK;
	  lclplatformdevice = android.os.Build.DEVICE;
	  
	  s ="\n Debug-infos:";
	  s += "\n OS Version: " + android.os.Build.VERSION.RELEASE;
	  s += "\n OS API Level: " + android.os.Build.VERSION.SDK;
	  s += "\n Device: " + android.os.Build.DEVICE;

	  Log.v("EglHelper", s);      
	  Log.v("EglHelper", "MajorVersion=" + lclmajorversion + " MinorVersion=" + lclminorversion);
	  
  };
  
  void LCLDoFinishEGL()
  { 
      if (LOG_EGL) {
          Log.w("EglHelper", "finish() tid=" + Thread.currentThread().getId());
      }
      if (mEglDisplay != null) {
          mEgl.eglTerminate(mEglDisplay);
          mEglDisplay = null;
      }
  };
  public EGLContext LCLDoCreateContext(EGLConfig config,EGLContext shareRC)
  {   if (LOG_EGL) {
        Log.v("LCLActivity", "LCLDoCreateContext() tid=" + Thread.currentThread().getId());
      }
	  int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
      int[] attrib_list = {EGL_CONTEXT_CLIENT_VERSION, mEGLContextClientVersion,
              EGL10.EGL_NONE };
      
      mEglConfig = config;
      
      mEglContext= mEgl.eglCreateContext(mEglDisplay, config, EGL10.EGL_NO_CONTEXT,
    		  mEGLContextClientVersion != 0 ? attrib_list : null);
      
      if (mEglContext == null || mEglContext == EGL10.EGL_NO_CONTEXT) {
          mEglContext = null;
          throw new IllegalArgumentException("LCLDoCreateContext failed");
      }	
      if (LOG_EGL) {
        Log.v("DefaultContextFactory", "display:" + mEglDisplay + " context: " + mEglContext);
      }
      return mEglContext;
	// return lclsurface.glsrender.createcontext();
  };
  
  public int LCLDoDestroyContext(EGLContext context)
  {   if (LOG_EGL) {
        Log.v("LCLActivity", "LCLDoDestroyContext() tid=" + Thread.currentThread().getId());
      }
      if (context != null) {
          if (!mEgl.eglDestroyContext(mEglDisplay, context)) {
              if (LOG_EGL) {
              Log.e("DefaultContextFactory", "display:" + mEglDisplay + " context: " + context);
              }
              throw new RuntimeException("eglDestroyContext failed: ");
          }
          mEglContext = null;
      }
      
	//  lclsurface.glsrender.destroycontext();
      return 1;
  };  
  public EGLSurface LCLDoCreateSurface(EGLConfig config)
  {
      if (LOG_EGL) {
          Log.v("Activity", "createSurface()  tid=" + Thread.currentThread().getId());
      }
      mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE,
              EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_CONTEXT);
      mEglSurface = mEgl.eglCreateWindowSurface(mEglDisplay, config, lclsurface.aholder, null);
      
      if (mEglSurface == null || mEglSurface == EGL10.EGL_NO_SURFACE) {
          int error = mEgl.eglGetError();
          if (error == EGL10.EGL_BAD_NATIVE_WINDOW) {
              Log.e("EglHelper", "createWindowSurface returned EGL_BAD_NATIVE_WINDOW.");
          }
          return null;
      }
      return mEglSurface;
      
      
	 // lclsurface.glsrender.createSurface(lclsurface.aholder);
  };
  public void LCLDoDestroySurface(EGLSurface surface)
  {
      if (LOG_EGL) {
          Log.v("EglHelper", "destroySurface()  tid=" + Thread.currentThread().getId());
      }
      if (surface != null && surface != EGL10.EGL_NO_SURFACE) {
    	  mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE,
                  EGL10.EGL_NO_SURFACE,
                  EGL10.EGL_NO_CONTEXT);
    	  mEgl.eglDestroySurface(mEglDisplay, surface);
          mEglSurface = null;
      } 
	 // lclsurface.glsrender.destroySurface();
  };  
  public void LCLDoPurgeBuffers(EGLSurface surface, EGLContext context)
  {
	  mEgl.eglMakeCurrent(mEglDisplay,
    		  surface, surface,
    		  context);
	  //lclsurface.glsrender.purgeBuffers();
  };  
  public void LCLDoClearBuffers()
  {
	  mEgl.eglMakeCurrent(mEglDisplay,
              EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
              EGL10.EGL_NO_CONTEXT);
      
	  //lclsurface.glsrender.clearBuffers();
  };  
  public boolean LCLDoSwapBuffers(EGLSurface surface)
  {
      if (! mEgl.eglSwapBuffers(mEglDisplay, mEglSurface)) {

          /*
           * Check for EGL_CONTEXT_LOST, which means the context
           * and all associated data were lost (For instance because
           * the device went to sleep). We need to sleep until we
           * get a new surface.
           */
          int error = mEgl.eglGetError();
          switch(error) {
          case EGL11.EGL_CONTEXT_LOST:
              return false;
          case EGL10.EGL_BAD_NATIVE_WINDOW:
              // The native window is bad, probably because the
              // window manager has closed it. Ignore this error,
              // on the expectation that the application will be closed soon.
              Log.e("EglHelper", "eglSwapBuffers returned EGL_BAD_NATIVE_WINDOW. tid=" + Thread.currentThread().getId());
              break;
          default:
        	  throw new RuntimeException("eglSwapBuffers"+ error);
          }
      }
      return true;
	 // lclsurface.glsrender.swap();
  }; 
  public int LCLeglGetError()
  {
	  return mEgl.eglGetError();
	 
  };  
  

  protected int[] FiAttribs;
  EGLConfig[] configs;
  
  // Get All configs
  public int LCLGetConfigs() {
      if (LOG_EGL) {
      Log.v("LCLActivity", "LCLGetConfigs() tid=" + Thread.currentThread().getId());
      }
      int[] num_config = new int[1];
	  FiAttribs=null;
	      
	  
      if (!mEgl.eglChooseConfig(mEglDisplay, FiAttribs, null, 0,
              num_config)) {
          throw new IllegalArgumentException("eglChooseConfig failed");
      }
 
      int numConfigs = num_config[0];

      if (numConfigs <= 0) {
          throw new IllegalArgumentException(
                  "No configs match configSpec");
      }

      configs = new EGLConfig[numConfigs];
      if (!mEgl.eglChooseConfig(mEglDisplay, FiAttribs, configs, numConfigs,
              num_config)) {
          throw new IllegalArgumentException("eglChooseConfig#2 failed");
      }
      FiAttribs = new int[1];
      if (LOG_EGL) {
        Log.v("LCLActivity", "LCLGetConfigs() tid=" + Thread.currentThread().getId());
      }
      return 1;
  }
  
  public int LCLGetFixedAttribute(int Attrib, int Param)
  {
    if (LOG_EGL) {  
    Log.v("LCLActivity", "LCLGetFixedAttribute() tid=" + Thread.currentThread().getId());
    }

    int Result, J, OverRes;
    int[] Res= new int[1];
    /* Appointment of a function to look for equal or approximate values
       of attributes from the list glx.
      If you just ask all the attributes
      that the user can put it out of ignorance
      Access Violation could appear as the list will be empty. 
    */
    OverRes = -1;
    Result = -1;
    J = 0;
    for (int i = 0; i < configs.length; i++) { 
      if (mEgl.eglGetConfigAttrib(mEglDisplay, configs[i], Attrib, Res) != true)
      {
        continue;
      }
      if (Res[0] > 0 && Res[0] <= Param)
      {
        Result = Res[0];
      }
      if (Res[0] > Param && OverRes < Res[0])
      {
        OverRes = Res[0];
      }
      J = i;
    }
    if (Result == -1 && J == configs.length)
    {
      return OverRes;
    }
    if (LOG_EGL) {
    Log.v("LCLActivity", "LCLGetFixedAttribute() tid=" + Thread.currentThread().getId());
    }
    return Result;
  } 
  
  public void LCLAddIAttrib(int attrib, int value)
  {
	  /*  int n;
    n := Length(FiAttribs);
    SetLength(FiAttribs, n + 2);
    FiAttribs[n - 1] := attrib;
    FiAttribs[n] := value;
    FiAttribs[n + 1] := EGL_NONE;
    */
    if (LOG_EGL) {  
	Log.v("LCLActivity", "LCLAddIAttrib() tid=" + Thread.currentThread().getId());
    }
	  int len = FiAttribs.length;
	  int[] newConfigSpec = new int[len + 2];
	  System.arraycopy(FiAttribs, 0, newConfigSpec, 0, len-1);
	  newConfigSpec[len-1] = attrib;
	  newConfigSpec[len] = value;
	  newConfigSpec[len+1] = EGL10.EGL_NONE;
	  FiAttribs= newConfigSpec;
    if (LOG_EGL) {
	 Log.v("LCLActivity", "LCLAddIAttrib() tid=" + Thread.currentThread().getId());
    }
  }   
  
  //chooseConfig
  
  public EGLConfig LCLChooseConfig() {
      if (LOG_EGL) {
      Log.v("LCLActivity", "LCLChooseConfig() tid=" + Thread.currentThread().getId());
      }
	   if (mEGLContextClientVersion == 2) {
	          int len = FiAttribs.length;
	          int[] newConfigSpec = new int[len + 2];
	          System.arraycopy(FiAttribs, 0, newConfigSpec, 0, len-1);
	          newConfigSpec[len-1] = EGL10.EGL_RENDERABLE_TYPE;
	          newConfigSpec[len] = 4; // EGL_OPENGL_ES2_BIT 
	          newConfigSpec[len+1] = EGL10.EGL_NONE;
	          FiAttribs= newConfigSpec;
	      }
      
      int[] num_config = new int[1];
      
      if (!mEgl.eglChooseConfig(mEglDisplay, FiAttribs, configs, 1,
              num_config)) {
          throw new IllegalArgumentException("eglChooseConfig failed");
      }
      if (LOG_EGL) {
      Log.v("LCLActivity", "LCLChooseConfig() config:" + configs[0]);
      }
      return configs[0];
  }
  
  // SensorEventListener overrides

  @Override public void onSensorChanged(SensorEvent event)
  {
      Log.v("LCLActivity", "onSensorChanged");
      
      /*  double[] eventValues = convertFloatsToDoubles(event.values);
    int eventKind = event.sensor.getType();
    int eventResult = LCLOnSensorChanged(eventKind, eventValues);
    if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
    */
  }

  @Override public void onAccuracyChanged(Sensor sensor, int accuracy)
  {
  }
  

  public void LCLDoStartReadingAccelerometer()
  {
    localSensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
    localSensorManager.registerListener(this,
      localSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),
      SensorManager.SENSOR_DELAY_NORMAL);
  };

  public void LCLDoStopReadingAccelerometer()
  {
    localSensorManager.unregisterListener(this);
  };

  /*
   //SMS и GPS подключение для сцены не нужно но на всякий оставим
  // input: String lcldestination, String lcltext (Body)
  public void LCLDoSendMessage()
  {
    if (lclkind == 1)
    {
      PendingIntent sentPI = PendingIntent.getBroadcast(this, 0,
        new Intent("SMS_SENT"), 0);

      PendingIntent deliveredPI = PendingIntent.getBroadcast(this, 0,
        new Intent("SMS_DELIVERED"), 0);

      //---when the SMS has been sent---
      registerReceiver(new BroadcastReceiver()
      {
        @Override public void onReceive(Context arg0, Intent arg1)
        {
          double[] statusArray = new double[1];
          int eventResult = 0;
          switch (getResultCode())
          {
            case Activity.RESULT_OK:
              statusArray[0] = 1.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
              statusArray[0] = 2.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_NO_SERVICE:
              statusArray[0] = 3.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_NULL_PDU:
              statusArray[0] = 2.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_RADIO_OFF:
              statusArray[0] = 5.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
          }
          ProcessEventResult(eventResult);
        }
      }, new IntentFilter("SMS_SENT"));

      //---when the SMS has been delivered---
      registerReceiver(new BroadcastReceiver()
      {
        @Override public void onReceive(Context arg0, Intent arg1)
        {
          double[] statusArray = new double[1];
          int eventResult = 0;
          switch (getResultCode())
          {
            case Activity.RESULT_OK:
              statusArray[0] = 10.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case Activity.RESULT_CANCELED:
              statusArray[0] = 11.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
          }
          ProcessEventResult(eventResult);
        }
      }, new IntentFilter("SMS_DELIVERED"));

      // SMS sending seams to cause an awful lot of exceptions
      // See: http://stackoverflow.com/questions/4580952/why-do-i-get-nullpointerexception-when-sending-an-sms-on-an-htc-desire-or-what
      // See: http://code.google.com/p/android/issues/detail?id=3718
      try
      {
        SmsManager sms = SmsManager.getDefault();
        Log.i("lclapp", "[LCLDoSendMessage] lcldestination="+lcldestination+" lcltext="+lcltext);
        ArrayList<String> parts = sms.divideMessage(lcltext);
        //sms.sendMultipartTextMessage(lcldestination, null, parts, sentPI, deliveredPI);
        sms.sendTextMessage(lcldestination, null, lcltext, sentPI, deliveredPI);
      }
      catch (Exception e)
      {
      }
    }
  };
  
  // LocationListener overrides

  @Override public void onLocationChanged(Location loc)
  {
    if (loc != null)
    {
      double[] positionArray = new double[6];
      positionArray[0] = loc.getLatitude();
      positionArray[1] = loc.getLongitude();
      positionArray[2] = loc.getAltitude();
      positionArray[3] = (double)loc.getAccuracy();
      positionArray[4] = (double)loc.getSpeed();
      positionArray[5] = (double)loc.getTime();
      int eventResult = LCLOnSensorChanged(-10, positionArray);
      if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
    }
  }

  @Override public void onProviderDisabled(String provider)
  {
  }

  @Override public void onProviderEnabled(String provider)
  {
  }

  @Override public void onStatusChanged(String provider, int status, Bundle extras)
  {
  }

  // input:  int lclkind
  public void LCLDoRequestPositionInfo()
  {
    LocationManager mlocManager = (LocationManager)getSystemService(Context.LOCATION_SERVICE);
    switch (lclkind)
    {
      case 1: mlocManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);
      case 2: mlocManager.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 0, 0, this);
      default: Log.i("lclapp", "[LCLDoRequestPositionInfo] Wrong lclkind parameter");
    }
  }
*/

  // -------------------------------------------
  // Fields exported to the Pascal side for easier data communication
  // -------------------------------------------
  public String lcltext;
  public String lcltitle;
  public String lclbutton1str;
  public String lclbutton2str;
  public String lclbutton3str;
  //
  public int lclwidth;
  public int lclheight;
  public int lclbutton1;
  public int lclbutton2;
  public int lclbutton3;
  public Bitmap lclbitmap;
  //
  public int lcltextsize;
  public int lcltextascent;
  public int lcltextbottom;
  public int lcltextdescent;
  public int lcltextleading;
  public int lcltexttop;
  public int lclmaxwidth;
  public int lclmaxcount;
  public float[] lclpartialwidths;
  //
  public int lcltimerinterval;
  public Runnable lcltimerid;
  //
  public int lclxdpi;
  public int lclydpi;
  public int lclformwidth;
  public int lclformheight;
  public int lclscreenwidth;
  public int lclscreenheight;
  // for LazDeviceAPIs
  public String lcldestination;
  public int lclkind;
  
  public int lclmajorversion;
  public int lclminorversion;
  GL gl;
  EGL10 mEgl;
  EGLDisplay mEglDisplay;
  EGLSurface mEglSurface;
  EGLConfig mEglConfig;
  EGLContext mEglContext;
  private final static boolean LOG_EGL = true;
  public int mEGLContextClientVersion;
  public SurfaceHolder lclholder;
  
  public String lclplatformversion;
  public String lclplatformapi;
  public String lclplatformdevice;

  
  
  static
  {
    try 
    {
      Log.i("lclapp", "Trying to load libglues.so");
      System.loadLibrary("glues");       
    //  Log.i("lclapp", "Trying to load libz.so");
    //  System.loadLibrary("z");  
      //  Log.i("lclapp", "Trying to load libpng.so");
      //   System.loadLibrary("png");   
      Log.i("lclapp", "Trying to load liblclapp.so");
      System.loadLibrary("lclapp");
    }
    catch(UnsatisfiedLinkError ule) 
    {
      Log.e("lclapp", "WARNING: Could not load liblclapp.so");
      ule.printStackTrace();
    }
  }
}
