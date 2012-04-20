package com.pascal.lcltest;

import java.io.Writer;

import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGL11;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLContext;
import javax.microedition.khronos.egl.EGLDisplay;
import javax.microedition.khronos.egl.EGLSurface;
import javax.microedition.khronos.opengles.GL;
import javax.microedition.khronos.opengles.GL10;

import android.content.Context;
import android.opengl.GLDebugHelper;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
/*
import android.opengl.GLSurfaceView;


public class GLSRenderer implements GLSurfaceView.Renderer {
    private static final String LOG_TAG = GLSRenderer.class.getSimpleName();

    private float _red = 0.9f;
    private float _green = 0.2f;
    private float _blue = 0.2f;

    @Override
    public void onSurfaceCreated(GL10 gl, EGLConfig config) {
        // Do nothing special.
    }

    @Override
    public void onSurfaceChanged(GL10 gl, int w, int h) {
        gl.glViewport(0, 0, w, h);
    }

    @Override
    public void onDrawFrame(GL10 gl) {
        // define the color we want to be displayed as the "clipping wall"
        gl.glClearColor(_red, _green, _blue, 1.0f);
        // clear the color buffer to show the ClearColor we called above...
        gl.glClear(GL10.GL_COLOR_BUFFER_BIT);
    }
}
*/

public class GLSSurfaceView extends SurfaceView implements SurfaceHolder.Callback {
    private final static boolean LOG_EGL = true;
    EglHelper glsrender;
    GL gl;
    private float _red = 0.9f;
    private float _green = 0.2f;
    private float _blue = 0.2f;
    SurfaceHolder holder;
    public int lclmajorversion;
    public int lclminorversion;
    
	public GLSSurfaceView(Context context) {
		super(context);
		// TODO Auto-generated constructor stub
		glsrender= new EglHelper();
        init();
	}
	
	
    private void init() {
        // Install a SurfaceHolder.Callback so we get notified when the
        // underlying surface is created and destroyed
        holder = getHolder();
        holder.addCallback(this);
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
		// TODO Auto-generated method stub
		
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder) {
		// TODO Auto-generated method stub
	//	glsrender.start();
    //	gl = (GL10) glsrender.createSurface(holder);
	//  glsrender.purgeBuffers();
		
	//	((GL10) gl).glViewport(0, 0, 100, 100);
        // define the color we want to be displayed as the "clipping wall"
    //    ((GL10) gl).glClearColor(_red, _green, _blue, 1.0f);
        // clear the color buffer to show the ClearColor we called above...
     //   ((GL10) gl).glClear(GL10.GL_COLOR_BUFFER_BIT);
		
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder) {
		// TODO Auto-generated method stub
		//glsrender.destroySurface();
		//glsrender.finish();
	}
	
   private abstract class BaseConfigChooser
            {
   public BaseConfigChooser(int[] configSpec) {
       mConfigSpec = filterConfigSpec(configSpec);
   }

   public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display) {
       int[] num_config = new int[1];
       if (!egl.eglChooseConfig(display, mConfigSpec, null, 0,
               num_config)) {
           throw new IllegalArgumentException("eglChooseConfig failed");
       }

       int numConfigs = num_config[0];

       if (numConfigs <= 0) {
           throw new IllegalArgumentException(
                   "No configs match configSpec");
       }

       EGLConfig[] configs = new EGLConfig[numConfigs];
       if (!egl.eglChooseConfig(display, mConfigSpec, configs, numConfigs,
               num_config)) {
           throw new IllegalArgumentException("eglChooseConfig#2 failed");
       }
       EGLConfig config = chooseConfig(egl, display, configs);
       if (config == null) {
           throw new IllegalArgumentException("No config chosen");
       }
       return config;
   }

   abstract EGLConfig chooseConfig(EGL10 egl, EGLDisplay display,
           EGLConfig[] configs);

   protected int[] mConfigSpec;

   private int[] filterConfigSpec(int[] configSpec) {
       if (mEGLContextClientVersion != 2) {
           return configSpec;
       }
       /* We know none of the subclasses define EGL_RENDERABLE_TYPE.
        * And we know the configSpec is well formed.
        */
       int len = configSpec.length;
       int[] newConfigSpec = new int[len + 2];
       System.arraycopy(configSpec, 0, newConfigSpec, 0, len-1);
       newConfigSpec[len-1] = EGL10.EGL_RENDERABLE_TYPE;
       newConfigSpec[len] = 4; /* EGL_OPENGL_ES2_BIT */
       newConfigSpec[len+1] = EGL10.EGL_NONE;
       return newConfigSpec;
   }
}

/**
* Choose a configuration with exactly the specified r,g,b,a sizes,
* and at least the specified depth and stencil sizes.
*/
private class ComponentSizeChooser extends BaseConfigChooser {
   public ComponentSizeChooser(int redSize, int greenSize, int blueSize,
           int alphaSize, int depthSize, int stencilSize) {
       super(new int[] {
               EGL10.EGL_RED_SIZE, redSize,
               EGL10.EGL_GREEN_SIZE, greenSize,
               EGL10.EGL_BLUE_SIZE, blueSize,
               EGL10.EGL_ALPHA_SIZE, alphaSize,
               EGL10.EGL_DEPTH_SIZE, depthSize,
               EGL10.EGL_STENCIL_SIZE, stencilSize,
               EGL10.EGL_NONE});
       mValue = new int[1];
       mRedSize = redSize;
       mGreenSize = greenSize;
       mBlueSize = blueSize;
       mAlphaSize = alphaSize;
       mDepthSize = depthSize;
       mStencilSize = stencilSize;
  }

   @Override
   public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display,
           EGLConfig[] configs) {
       for (EGLConfig config : configs) {
           int d = findConfigAttrib(egl, display, config,
                   EGL10.EGL_DEPTH_SIZE, 0);
           int s = findConfigAttrib(egl, display, config,
                   EGL10.EGL_STENCIL_SIZE, 0);
           if ((d >= mDepthSize) && (s >= mStencilSize)) {
               int r = findConfigAttrib(egl, display, config,
                       EGL10.EGL_RED_SIZE, 0);
               int g = findConfigAttrib(egl, display, config,
                        EGL10.EGL_GREEN_SIZE, 0);
               int b = findConfigAttrib(egl, display, config,
                         EGL10.EGL_BLUE_SIZE, 0);
               int a = findConfigAttrib(egl, display, config,
                       EGL10.EGL_ALPHA_SIZE, 0);
               if ((r == mRedSize) && (g == mGreenSize)
                       && (b == mBlueSize) && (a == mAlphaSize)) {
                   return config;
               }
           }
       }
       return null;
   }

   private int findConfigAttrib(EGL10 egl, EGLDisplay display,
           EGLConfig config, int attribute, int defaultValue) {

       if (egl.eglGetConfigAttrib(display, config, attribute, mValue)) {
           return mValue[0];
       }
       return defaultValue;
   }

   private int[] mValue;
   // Subclasses can adjust these values:
   protected int mRedSize;
   protected int mGreenSize;
   protected int mBlueSize;
   protected int mAlphaSize;
   protected int mDepthSize;
   protected int mStencilSize;
   }

/**
* This class will choose a RGB_565 surface with
* or without a depth buffer.
*
*/
private class SimpleEGLConfigChooser extends ComponentSizeChooser {
	public SimpleEGLConfigChooser(boolean withDepthBuffer) {
		super(5, 6, 5, 0, withDepthBuffer ? 16 : 0, 0);
	}
}

static class LogWriter extends Writer {

    @Override public void close() {
        flushBuilder();
    }

    @Override public void flush() {
        flushBuilder();
    }

    @Override public void write(char[] buf, int offset, int count) {
        for(int i = 0; i < count; i++) {
            char c = buf[offset + i];
            if ( c == '\n') {
                flushBuilder();
            }
            else {
                mBuilder.append(c);
            }
        }
    }

    private void flushBuilder() {
        if (mBuilder.length() > 0) {
            Log.v("GLSurfaceView", mBuilder.toString());
            mBuilder.delete(0, mBuilder.length());
        }
    }

    private StringBuilder mBuilder = new StringBuilder();
}

public class EglHelper {
    private static final int DEBUG_CHECK_GL_ERROR = 0;
	private static final int DEBUG_LOG_GL_CALLS = 0;
	private int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
    
    public EglHelper() {

    }

    public EGLContext createContext(EGL10 egl, EGLDisplay display, EGLConfig config) {
        int[] attrib_list = {EGL_CONTEXT_CLIENT_VERSION, mEGLContextClientVersion,
                EGL10.EGL_NONE };

        return egl.eglCreateContext(display, config, EGL10.EGL_NO_CONTEXT,
                mEGLContextClientVersion != 0 ? attrib_list : null);
    }

    public void destroyContext(EGL10 egl, EGLDisplay display,
            EGLContext context) {
        if (!egl.eglDestroyContext(display, context)) {
            Log.e("DefaultContextFactory", "display:" + display + " context: " + context);
          //  if (LOG_THREADS) {
           //     Log.i("DefaultContextFactory", "tid=" + Thread.currentThread().getId());
          //  }
          //  throw new RuntimeException("eglDestroyContext failed: "
            //        + EGLLogWrapper.getErrorString(egl.eglGetError()));
        }
    } 
    public EGLSurface createWindowSurface(EGL10 egl, EGLDisplay display,
            EGLConfig config, Object nativeWindow) {
        EGLSurface result = null;
        try {
            result = egl.eglCreateWindowSurface(display, config, nativeWindow, null);
        } catch (IllegalArgumentException e) {
            // This exception indicates that the surface flinger surface
            // is not valid. This can happen if the surface flinger surface has
            // been torn down, but the application has not yet been
            // notified via SurfaceHolder.Callback.surfaceDestroyed.
            // In theory the application should be notified first,
            // but in practice sometimes it is not. See b/4588890
            Log.e("GLSurfaceView", "eglCreateWindowSurface", e);
        }
        return result;
    }

    public void destroySurface(EGL10 egl, EGLDisplay display,
            EGLSurface surface) {
        egl.eglDestroySurface(display, surface);
    }  
    
    public void start() {
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

        /*
         * We can now initialize EGL for that display
         */
        int[] version = new int[2];
        if(!mEgl.eglInitialize(mEglDisplay, version)) {
            throw new RuntimeException("eglInitialize failed");
        }
        
        lclmajorversion = version[0];
        lclminorversion = version[1];
        
        Log.v("EglHelper", "MajorVersion=" + lclmajorversion + " MinorVersion=" + lclminorversion);
           	
    }
    
    
    /**
     * Initialize EGL for a given configuration spec.
     * @param configSpec
     */
    public EGLContext createcontext() {
        if (LOG_EGL) {
            Log.w("EglHelper", "createcontext() tid=" + Thread.currentThread().getId());
        }
       
        
        mEGLConfigChooser= new SimpleEGLConfigChooser(true);
        mEglConfig = mEGLConfigChooser.chooseConfig(mEgl, mEglDisplay);

        /*
        * Create an EGL context. We want to do this as rarely as we can, because an
        * EGL context is a somewhat heavy object.
        */
        mEglContext = createContext(mEgl, mEglDisplay, mEglConfig);
        if (mEglContext == null || mEglContext == EGL10.EGL_NO_CONTEXT) {
            mEglContext = null;
            throwEglException("createContext");
        }
        if (LOG_EGL) {
            Log.w("EglHelper", "createContext " + mEglContext + " tid=" + Thread.currentThread().getId());
        }
        mEglSurface = null;
        return mEglContext;
    }

    /*
     * React to the creation of a new surface by creating and returning an
     * OpenGL interface that renders to that surface.
     */
    public GL createSurface(SurfaceHolder holder) {
        if (LOG_EGL) {
            Log.v("EglHelper", "createSurface()  tid=" + Thread.currentThread().getId());
        }
        /*
         * Check preconditions.
         */
        if (mEgl == null) {
            throw new RuntimeException("egl not initialized");
        }
        if (mEglDisplay == null) {
            throw new RuntimeException("eglDisplay not initialized");
        }
        if (mEglConfig == null) {
            throw new RuntimeException("mEglConfig not initialized");
        }
        /*
         *  The window size has changed, so we need to create a new
         *  surface.
         */
        if (mEglSurface != null && mEglSurface != EGL10.EGL_NO_SURFACE) {

            /*
             * Unbind and destroy the old EGL surface, if
             * there is one.
             */
            mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE,
                    EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_CONTEXT);
            destroySurface(mEgl, mEglDisplay, mEglSurface);
        }

        /*
         * Create an EGL surface we can render into.
         */
        mEglSurface = createWindowSurface(mEgl,
                mEglDisplay, mEglConfig, holder);

        if (mEglSurface == null || mEglSurface == EGL10.EGL_NO_SURFACE) {
            int error = mEgl.eglGetError();
            if (error == EGL10.EGL_BAD_NATIVE_WINDOW) {
                Log.e("EglHelper", "createWindowSurface returned EGL_BAD_NATIVE_WINDOW.");
            }
            return null;
        }

        /*
         * Before we can issue GL commands, we need to make sure
         * the context is current and bound to a surface.
         */
        if (!mEgl.eglMakeCurrent(mEglDisplay, mEglSurface, mEglSurface, mEglContext)) {
            throwEglException("eglMakeCurrent");
        }

        GL gl = mEglContext.getGL();
       // if (mGLWrapper != null) {
       //     gl = mGLWrapper.wrap(gl);
       // }


		if ((mDebugFlags & (DEBUG_CHECK_GL_ERROR | DEBUG_LOG_GL_CALLS)) != 0) {
            int configFlags = 0;
            Writer log = null;
            if ((mDebugFlags & DEBUG_CHECK_GL_ERROR) != 0) {
                configFlags |= GLDebugHelper.CONFIG_CHECK_GL_ERROR;
            }
            if ((mDebugFlags & DEBUG_LOG_GL_CALLS) != 0) {
                log = new LogWriter();
            }
            gl = GLDebugHelper.wrap(gl, configFlags, log);
        }
        return gl;
    }

    public void purgeBuffers() {
       // mEgl.eglMakeCurrent(mEglDisplay,
       //         EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
        //        EGL10.EGL_NO_CONTEXT);
        mEgl.eglMakeCurrent(mEglDisplay,
                mEglSurface, mEglSurface,
                mEglContext);
    }
    
    public void clearBuffers() {
        mEgl.eglMakeCurrent(mEglDisplay,
                EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
                EGL10.EGL_NO_CONTEXT);
    }

    /**
     * Display the current render surface.
     * @return false if the context has been lost.
     */
    public boolean swap() {
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
                throwEglException("eglSwapBuffers", error);
            }
        }
        return true;
    }

    public void destroySurface() {
        if (LOG_EGL) {
            Log.w("EglHelper", "destroySurface()  tid=" + Thread.currentThread().getId());
        }
        if (mEglSurface != null && mEglSurface != EGL10.EGL_NO_SURFACE) {
            mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE,
                    EGL10.EGL_NO_SURFACE,
                    EGL10.EGL_NO_CONTEXT);
            destroySurface(mEgl, mEglDisplay, mEglSurface);
            mEglSurface = null;
        }
    }

    public void destroycontext(EGLContext aEglContext) {
        if (LOG_EGL) {
            Log.w("EglHelper", "destroycontext() tid=" + Thread.currentThread().getId());
        }
        if (mEglContext != null) {
            destroyContext(mEgl, mEglDisplay, aEglContext);
            mEglContext = null;
        }
        if (LOG_EGL) {
            Log.w("EglHelper", "contextdestroyd() tid=" + Thread.currentThread().getId());
        }        
    }
    
    public void finish() {
        if (LOG_EGL) {
            Log.w("EglHelper", "finish() tid=" + Thread.currentThread().getId());
        }
        if (mEglDisplay != null) {
            mEgl.eglTerminate(mEglDisplay);
            mEglDisplay = null;
        }
    }
    

    private void throwEglException(String function) {
        throwEglException(function, mEgl.eglGetError());
    }

    private void throwEglException(String function, int error) {
        String message = function + " failed: " ;//+ EGLLogWrapper.getErrorString(error);
       // if (LOG_THREADS) {
       //     Log.e("EglHelper", "throwEglException tid=" + Thread.currentThread().getId() + " " + message);
       // }
        throw new RuntimeException(message);
    }

    EGL10 mEgl;
    EGLDisplay mEglDisplay;
    EGLSurface mEglSurface;
    EGLConfig mEglConfig;
    EGLContext mEglContext;

}

private SimpleEGLConfigChooser mEGLConfigChooser;
//private EGLContextFactory mEGLContextFactory;
//private EGLWindowSurfaceFactory mEGLWindowSurfaceFactory;
private int mEGLContextClientVersion;
int mDebugFlags;
	
}
