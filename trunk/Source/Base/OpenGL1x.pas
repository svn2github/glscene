//
// This unit is part of the GLScene Project, http://glscene.org
//
{: OpenGL1x<p>

	OpenGL 1.x import unit for GLScene. Unit remains "general purpose", but with
   a more "pragmatic" approach :)<p>

   This unit is based on OpenGL12.pas orginally written by Mike Lischke,
   please refer to OpenGL12.pas header.<p>

	<b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Moved tokens part to OpenGLTokens.pas
      <li>22/07/10 - Yar - Added GL_ARB_debug_output constant
      <li>01/06/10 - Yar - Fixes for Linux x64
      <li>31/05/10 - Yar - Added WGL_NV_gpu_affinity
      <li>12/05/10 - Yar - Added GL_ARB_texture_compression_bptc
      <li>04/05/10 - Yar - Added GL_S3_s3tc extension (thanks to Rustam Asmandiarov aka Predator)
      <li>01/05/10 - DanB - Fixed glGetTransformFeedbackVarying params
      <li>16/04/10 - Yar - Added Graphics Remedy's Extensions
      <li>28/03/10 - DanB - Added missing OpenGL 3.1/3.2 function lookups +
                            added bindless graphics extensions
      <li>18/03/10 - Yar - Added more GLX extensions
                          (thanks to Rustam Asmandiarov aka Predator)
      <li>12/03/10 - DanB - OpenGL 3.3/4.0 support (new ARB extensions), removed
                            _ARB suffix from functions/procedures in
                            GL_ARB_draw_buffers_blend + GL_ARB_sample_shading
      <li>04/03/10 - DanB - Organised core into relevant + deprecated sections,
                            fixed a couple of function params + misc changes.
      <li>12/02/10 - Yar -  Added GL_AMD_vertex_shader_tessellator
      <li>07/02/10 - Yar -  Added GL_NV_primitive_restart
      <li>21/01/10 - DaStr - Bugfixed wglChoosePixelFormatARB() and
                              wglCreatePbufferARB() parameters
      <li>07/01/10 - DaStr - Added WGL_COLOR_SAMPLES_NV (thanks YarUndeoaker)
      <li>25/12/09 - DaStr - Added GL_NV_copy_image, GL_LUMINANCE_INTEGER,
                              GL_LUMINANCE_ALPHA_INTEGER extentions and constants
                             Re-added $region declarations (thanks YarUndeoaker)
      <li>13/12/09 - DaStr - Added missing stdcall/cdecl modifiers
      <li>25/10/09 - DaStr - Added some texture compression extensions and updated
                              glTransformFeedbackVaryings()(thanks YarUndeoaker)
      <li>28/09/09 - DaStr - Added some NVidia-specific extensions (thanks YarUndeoaker)
      <li>30/08/09 - DanB - GLsync changed to NativeInt, fixes to glBindBufferRange calls
      <li>14/08/09 - DanB - Added missing GL_ARB_framebuffer_object extension check + fixed typo
      <li>04/08/09 - DanB - OpenGL 3.1/3.2 support + new ARB extensions added
      <li>28/07/09 - DaStr - Added GL_GEOMETRY_PROGRAM_NV and related extensions
      <li>20/01/08 - DanB - Fix due to Delphi6 not containing UInt64
      <li>05/10/08 - DanB - Moved error handling code here from GLContext.pas
                            OpenGL 3.0 support, new core features + ARB extensions
      <li>23/03/08 - DanB - Added more Vendor/EXT extensions
      <li>17/03/08 - mrqzzz - uncommented some constants "GL_NORMAL_MAP_EXT,..."
                              to keep compatibility with dws2OpenGL1x.
      <li>16/03/08 - DanB - Major rewrite of unit, including:
                            OpenGL 1.3, 1.4, 1.5, 2.0, 2.1 support.
                            removed TRCOptions (not used).
                            moved MRT_BUFFERS constant to GLContext.pas (isn't core openGL).
                            several new ARB extensions added.
                            several new Vendor/EXT exensions added.
                            new function IsOpenGLVersionMet added.
                            restructured unit so extensions are in numerical order.
      <li>17/06/07 - LC - Added GL_ARB_pixel_buffer_object, GL_EXT_pixel_buffer_object
      <li>22/03/07 - DaStr - Removed GetTextureRectangle (had many useless checks)
      <li>16/03/07 - DaStr - Dropped Kylix support in favor of FPC
                             (thanks Burkhard Carstens) (BugTracekrID=1681585)
      <li>09/03/07 - DaStr - Added GL_ARB_draw_buffers (thanks riz)
      <li>03/03/07 - DaStr - Added GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT
      <li>02/03/07 - DaStr - Added GL_[ARB/EXT]_texture_rectangle
                             Added GetTextureRectangle
      <li>10/01/07 - LC - Added GL_EXT_framebuffer_object
      <li>11/09/06 - NC - Added GL_ARB_texture_float, GL_ARB_texture_non_power_of_two
      <li>13/10/04 - NC - Added GL_ATI_draw_buffers
      <li>08/10/04 - LR - Added const in the prototype of the following function for compatibility :
                              TGLUTessCombineProc, TGLUTessCombineDataProc, gluPickMatrix
      gluProject, gluUnProject, gluTessVertex, gluLoadSamplingMatrices
      <li>04/10/04 - NC - Added GL_ATI_texture_float, WGL_ATI_pixel_format_float,
                          WGL_NV_float_buffer, GL_NV_float_buffer
      <li>08/07/04 - LR - Change case for Linux
      <li>05/07/04 - LR - Corrections for Linux. Now glX function are directly load
                          by external action (like for Windows). So i suppress
                          the function LoadLinuxOpenGL.
      <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
      <li>24/06/04 - SG - Added GL_ARB_fragment_program
      <li>17/05/04 - EG - Dropped EXT_vertex_array (assumed as standard)
      <li>06/04/04 - EG - Added GL_ARB_shader_objects, GL_ARB_vertex_shader
                          and GL_ARB_fragment_shader, dropped a few oldies
      <li>13/02/04 - EG - Added GL_NV_texture_rectangle
      <li>18/11/03 - EG - Fixed binding of core extensions, added GL_ARB_depth_texture
                          and GL_ARB_shadow support
      <li>20/09/03 - EG - Added GL_NV_occlusion_query, dropped some more oldies
      <li>09/09/03 - EG - Added GL_ARB_vertex_buffer_object, dropped some oldies
      <li>04/09/03 - EG - Added GL_ARB_vertex_program
      <li>23/07/03 - EG - Creation from OpenGL12.pas "morph": classic OpenGL
                          calls made static, obsolete/rare extensions support
                          dropped
   </ul></font>
}
unit OpenGL1x;

interface

{$i GLScene.inc}

 // DaStr: MULTITHREADOPENGL is defined in GLScene.inc, but you can override it
 // manually here, though I would not reccomend it. This is because other units
 // may depend on this option too. So if you need this option, please use the
 // GLS_MULTITHREAD define in GLScene.inc.
{.$define MULTITHREADOPENGL}

uses
  OpenGLTokens, VectorTypes, SysUtils,
  {$IFDEF MSWINDOWS}
    Windows
  {$ENDIF }

  {$IFDEF unix}
    {Libc,}Types, LCLType, X, Xlib, XUtil, dynlibs
  {$ENDIF}
  ;
{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL extension feature checks'} {$ENDIF}

{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}
   // supported version checks
   GL_VERSION_1_0,
   GL_VERSION_1_1,
   GL_VERSION_1_2,
   GL_VERSION_1_3,
   GL_VERSION_1_4,
   GL_VERSION_1_5,
   GL_VERSION_2_0,
   GL_VERSION_2_1,
   GL_VERSION_3_0,
   GL_VERSION_3_1,
   GL_VERSION_3_2,
   GL_VERSION_3_3,
   GL_VERSION_4_0,
   GLU_VERSION_1_1,
   GLU_VERSION_1_2,
   GLU_VERSION_1_3: Boolean;

   // ARB approved OpenGL extension checks
   GL_ARB_blend_func_extended,
   GL_ARB_color_buffer_float,
   GL_ARB_compatibility,
   GL_ARB_copy_buffer,
   GL_ARB_depth_buffer_float,
   GL_ARB_depth_clamp,
   GL_ARB_depth_texture,
   GL_ARB_draw_buffers,
   GL_ARB_draw_buffers_blend,
   GL_ARB_draw_elements_base_vertex,
   GL_ARB_draw_indirect,
   GL_ARB_draw_instanced,
   GL_ARB_explicit_attrib_location,
   GL_ARB_fragment_coord_conventions,
   GL_ARB_fragment_program,
   GL_ARB_fragment_program_shadow,
   GL_ARB_fragment_shader,
   GL_ARB_framebuffer_object,
   GL_ARB_framebuffer_sRGB,
   GL_ARB_geometry_shader4,
   GL_ARB_gpu_shader_fp64,
   GL_ARB_gpu_shader5,
   GL_ARB_half_float_pixel,
   GL_ARB_half_float_vertex,
   GL_ARB_imaging,
   GL_ARB_instanced_arrays,
   GL_ARB_map_buffer_range,
   GL_ARB_matrix_palette,
   GL_ARB_multisample,
   GL_ARB_multitexture,
   GL_ARB_occlusion_query,
   GL_ARB_occlusion_query2,
   GL_ARB_pixel_buffer_object,
   GL_ARB_point_parameters,
   GL_ARB_point_sprite,
   GL_ARB_provoking_vertex,
   GL_ARB_sample_shading,
   GL_ARB_sampler_objects,
   GL_ARB_seamless_cube_map,
   GL_ARB_shader_bit_encoding,
   GL_ARB_shader_subroutine,
   GL_ARB_shader_texture_lod,
   GL_ARB_shading_language_100,
   GL_ARB_shadow,
   GL_ARB_shadow_ambient,
   GL_ARB_shader_objects,
   GL_ARB_sync,
   GL_ARB_tessellation_shader,
   GL_ARB_texture_border_clamp,
   GL_ARB_texture_buffer_object,
   GL_ARB_texture_buffer_object_rgb32,
   GL_ARB_texture_compression,
   GL_ARB_texture_compression_rgtc,
   GL_ARB_texture_cube_map,
   GL_ARB_texture_cube_map_array,
   GL_ARB_texture_env_add,
   GL_ARB_texture_env_combine,
   GL_ARB_texture_env_crossbar,
   GL_ARB_texture_env_dot3,
   GL_ARB_texture_float,
   GL_ARB_texture_gather,
   GL_ARB_texture_mirrored_repeat,
   GL_ARB_texture_multisample,
   GL_ARB_texture_non_power_of_two,
   GL_ARB_texture_query_lod,
   GL_ARB_texture_rectangle,
   GL_ARB_texture_rg,
   GL_ARB_texture_rgb10_a2ui,
   GL_ARB_texture_swizzle,
   GL_ARB_timer_query,
   GL_ARB_transform_feedback2,
   GL_ARB_transform_feedback3,
   GL_ARB_transpose_matrix,
   GL_ARB_uniform_buffer_object,
   GL_ARB_vertex_array_bgra,
   GL_ARB_vertex_array_object,
   GL_ARB_vertex_blend,
   GL_ARB_vertex_buffer_object,
   GL_ARB_vertex_program,
   GL_ARB_vertex_shader,
   GL_ARB_vertex_type_2_10_10_10_rev,
   GL_ARB_window_pos,
   GL_ARB_texture_compression_bptc,

   // Vendor/EXT OpenGL extension checks
   GL_3DFX_multisample,
   GL_3DFX_tbuffer,
   GL_3DFX_texture_compression_FXT1,

   GL_ATI_draw_buffers,
   GL_ATI_texture_compression_3dc,
   GL_ATI_texture_float,
   GL_ATI_texture_mirror_once,

   GL_S3_s3tc,

   GL_EXT_abgr,
   GL_EXT_bgra,
   GL_EXT_bindable_uniform,
   GL_EXT_blend_color,
   GL_EXT_blend_equation_separate,
   GL_EXT_blend_func_separate,
   GL_EXT_blend_logic_op,
   GL_EXT_blend_minmax,
   GL_EXT_blend_subtract,
   GL_EXT_Cg_shader,
   GL_EXT_clip_volume_hint,
   GL_EXT_compiled_vertex_array,
   GL_EXT_copy_texture,
   GL_EXT_depth_bounds_test,
   GL_EXT_draw_buffers2,
   GL_EXT_draw_instanced,
   GL_EXT_draw_range_elements,
   GL_EXT_fog_coord,
   GL_EXT_framebuffer_blit,
   GL_EXT_framebuffer_multisample,
   GL_EXT_framebuffer_object,
   GL_EXT_framebuffer_sRGB,
   GL_EXT_geometry_shader4,
   GL_EXT_gpu_program_parameters,
   GL_EXT_gpu_shader4,
   GL_EXT_multi_draw_arrays,
   GL_EXT_multisample,
   GL_EXT_packed_depth_stencil,
   GL_EXT_packed_float,
   GL_EXT_packed_pixels,
   GL_EXT_paletted_texture,
   GL_EXT_pixel_buffer_object,
   GL_EXT_polygon_offset,
   GL_EXT_rescale_normal,
   GL_EXT_secondary_color,
   GL_EXT_separate_specular_color,
   GL_EXT_shadow_funcs,
   GL_EXT_shared_texture_palette,
   GL_EXT_stencil_clear_tag,
   GL_EXT_stencil_two_side,
   GL_EXT_stencil_wrap,
   GL_EXT_texture3D,
   GL_EXT_texture_array,
   GL_EXT_texture_buffer_object,
   GL_EXT_texture_compression_latc,
   GL_EXT_texture_compression_rgtc,
   GL_EXT_texture_compression_s3tc,
   GL_EXT_texture_cube_map,
   GL_EXT_texture_edge_clamp,
   GL_EXT_texture_env_add,
   GL_EXT_texture_env_combine,
   GL_EXT_texture_env_dot3,
   GL_EXT_texture_filter_anisotropic,
   GL_EXT_texture_integer,
   GL_EXT_texture_lod,
   GL_EXT_texture_lod_bias,
   GL_EXT_texture_mirror_clamp,
   GL_EXT_texture_object,
   GL_EXT_texture_rectangle,
   GL_EXT_texture_sRGB,
   GL_EXT_texture_shared_exponent,
   GL_EXT_timer_query,
   GL_EXT_transform_feedback,
   GL_EXT_vertex_array,

   GL_HP_occlusion_test,

   GL_IBM_rasterpos_clip,

   GL_KTX_buffer_region,

   GL_MESA_resize_buffers,

   GL_NV_blend_square,
   GL_NV_conditional_render,
   GL_NV_copy_image,
   GL_NV_depth_buffer_float,
   GL_NV_fence,
   GL_NV_float_buffer,
   GL_NV_fog_distance,
   GL_NV_geometry_program4,
   GL_NV_light_max_exponent,
   GL_NV_multisample_filter_hint,
   GL_NV_occlusion_query,
   GL_NV_point_sprite,
   GL_NV_primitive_restart,
   GL_NV_register_combiners,
   GL_NV_shader_buffer_load,
   GL_NV_texgen_reflection,
   GL_NV_texture_compression_vtc,
   GL_NV_texture_env_combine4,
   GL_NV_texture_rectangle,
   GL_NV_texture_shader,
   GL_NV_texture_shader2,
   GL_NV_texture_shader3,
   GL_NV_transform_feedback,
   GL_NV_vertex_array_range,
   GL_NV_vertex_array_range2,
   GL_NV_vertex_buffer_unified_memory,
   GL_NV_vertex_program,

   GL_SGI_color_matrix,

   GL_SGIS_generate_mipmap,
   GL_SGIS_multisample,
   GL_SGIS_texture_border_clamp,
   GL_SGIS_texture_color_mask,
   GL_SGIS_texture_edge_clamp,
   GL_SGIS_texture_lod,

   GL_SGIX_depth_texture,
   GL_SGIX_shadow,
   GL_SGIX_shadow_ambient,

   GL_AMD_vertex_shader_tessellator,

   GL_WIN_swap_hint,

   // ARB approved WGL extension checks
   WGL_ARB_buffer_region,
   WGL_ARB_create_context,
   WGL_ARB_create_context_profile,
   WGL_ARB_extensions_string,
   WGL_ARB_framebuffer_sRGB,
   WGL_ARB_make_current_read,
   WGL_ARB_multisample,
   WGL_ARB_pbuffer,
   WGL_ARB_pixel_format,
   WGL_ARB_pixel_format_float,
   WGL_ARB_render_texture,

   // Vendor/EXT WGL extension checks
   WGL_ATI_pixel_format_float,

   WGL_EXT_framebuffer_sRGB,
   WGL_EXT_pixel_format_packed_float,
   WGL_EXT_swap_control,
   WGL_NV_gpu_affinity,

   // GLX extension checks
   GLX_VERSION_1_1,
   GLX_VERSION_1_2,
   GLX_VERSION_1_3,
   GLX_VERSION_1_4,
   GLX_ARB_create_context,
   GLX_ARB_create_context_profile,
   GLX_ARB_framebuffer_sRGB,
   GLX_ARB_multisample,
   GLX_EXT_framebuffer_sRGB,
   GLX_EXT_fbconfig_packed_float,

   GLX_SGIS_multisample,
   GLX_EXT_visual_info,
   GLX_SGI_swap_control,
   GLX_SGI_video_sync,
   GLX_SGI_make_current_read,
   GLX_SGIX_video_source,
   GLX_EXT_visual_rating,
   GLX_EXT_import_context,
   GLX_SGIX_fbconfig,
   GLX_SGIX_pbuffer,
   GLX_SGI_cushion,
   GLX_SGIX_video_resize,
   GLX_SGIX_dmbuffer,
   GLX_SGIX_swap_group,
   GLX_SGIX_swap_barrier,
   GLX_SGIS_blended_overlay,
   GLX_SGIS_shared_multisample,
   GLX_SUN_get_transparent_index,
   GLX_3DFX_multisample,
   GLX_MESA_copy_sub_buffer,
   GLX_MESA_pixmap_colormap,
   GLX_MESA_release_buffers,
   GLX_MESA_set_3dfx_mode,
   GLX_SGIX_visual_select_group,
   GLX_SGIX_hyperpipe,

   // Graphics Remedy's Extensions
   GL_GREMEDY_frame_terminator,
   GL_GREMEDY_string_marker,

   // OpenGL Utility (GLU) extension checks
   GLU_EXT_object_space_tess,
   GLU_EXT_nurbs_tessellator,
   GLU_EXT_Texture: Boolean;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL v1.1 core functions and procedures'} {$ENDIF}
   procedure glBindTexture(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClear(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearColor(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearDepth(depth: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearStencil(s: TGLint ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glColorMask(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glCopyTexImage1D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexImage2D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage1D(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage2D(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCullFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDeleteTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthFunc(func: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthMask(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthRange(zNear, zFar: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDisable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDrawArrays(mode: TGLEnum; first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawElements(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   

   procedure glEnable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFinish; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glFlush; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFrontFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGenTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetBooleanv(pname: TGLEnum; params: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetDoublev(pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glGetError: TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetFloatv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetIntegerv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetPointerv(pname: TGLEnum; var params); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   
   procedure glGetTexImage(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameterfv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameteriv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glHint(target, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glIsEnabled(cap: TGLEnum): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glIsTexture(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glLineWidth(width: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glLogicOp(opcode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glPixelStoref(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPixelStorei(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPointSize(size: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonMode(face, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonOffset(factor, units: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glReadBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glReadPixels(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glScissor(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilFunc(func: TGLEnum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilOp(fail, zfail, zpass: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glTexImage1D(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,
                          atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexImage2D(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;
                          format, atype: TGLEnum; Pixels:Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage1D(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;
                             pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage2D(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,
                             atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glViewport(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.1 deprecated'} {$ENDIF}
   procedure glAccum(op: TGLuint; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glAreTexturesResident(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glArrayElement(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBegin(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBitmap(width: TGLsizei; height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallList(list: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallLists(n: TGLsizei; atype: TGLEnum; lists: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearAccum(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearIndex(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColor3b(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3d(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3f(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3i(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3s(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ub(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ui(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3us(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4b(red, green, blue, alpha: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4d(red, green, blue, alpha: TGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4f(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4i(red, green, blue, alpha: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4s(red, green, blue, alpha: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4sv(v: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ub(red, green, blue, alpha: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ui(red, green, blue, alpha: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4us(red, green, blue, alpha: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColorMaterial(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColorPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCopyPixels(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDeleteLists(list: TGLuint; range: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDisableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDrawPixels(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glEdgeFlag(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagPointer(stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagv(flag: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnd; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEndList; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1d(u: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1f(u: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2d(u: TGLdouble; v: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2f(u, v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh1(mode: TGLEnum; i1, i2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh2(mode: TGLEnum; i1, i2, j1, j2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint1(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint2(i, j: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glFeedbackBuffer(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogiv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFrustum(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glGenLists(range: TGLsizei): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapdv(target, query: TGLEnum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapfv(target, query: TGLEnum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapiv(target, query: TGLEnum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapfv(map: TGLEnum; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapuiv(map: TGLEnum; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapusv(map: TGLEnum; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glIndexMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexd(c: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexdv(c: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexf(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexfv(c: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexi(c: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexiv(c: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexs(c: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexsv(c: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexub(c: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexubv(c: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInitNames; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInterleavedArrays(format: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glIsList(list: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeli(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeliv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightf(light, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLighti(light, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLineStipple(factor: TGLint; pattern: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glListBase(base: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadIdentity; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glMap1d(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap1f(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2d(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,
                     vorder: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2f(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,
                     vorder: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1d(un: TGLint; u1, u2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1f(un: TGLint; u1, u2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2d(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2f(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMateriali(face, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMatrixMode(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNewList(list: TGLuint; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3b(nx, ny, nz: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3d(nx, ny, nz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3f(nx, ny, nz: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3i(nx, ny, nz: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3s(nx, ny, nz: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormalPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glOrtho(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPassThrough(token: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapfv(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapuiv(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapusv(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelTransferi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelZoom(xfactor, yfactor: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopClientAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopName; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPrioritizeTextures(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushClientAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glRasterPos2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2s(x, y: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectd(x1, y1, x2, y2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectdv(v1, v2: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectf(x1, y1, x2, y2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectfv(v1, v2: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRecti(x1, y1, x2, y2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectiv(v1, v2: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRects(x1, y1, x2, y2: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectsv(v1, v2: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glRenderMode(mode: TGLEnum): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotated(angle, x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotatef(angle, x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glScaled(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glScalef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glSelectBuffer(size: TGLsizei; buffer: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glShadeModel(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1d(s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1f(s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1i(s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1s(s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2d(s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2f(s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2i(s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2s(s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3d(s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3f(s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3i(s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3s(s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4d(s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4f(s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4i(s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4s(s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoordPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvi(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGend(coord, pname: TGLEnum; param: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenf(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeni(coord, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslated(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslatef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glVertex2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2s(x, y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertexPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL utility (GLU) functions and procedures'} {$ENDIF}
   function  gluErrorString(errCode: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluOrtho2D(left, right, bottom, top: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPickMatrix(x, y, width, height: TGLdouble; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluProject(objx, objy, objz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                        winx, winy, winz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluUnProject(winx, winy, winz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                          objx, objy, objz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluScaleImage(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,
                           heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild1DMipmaps(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild2DMipmaps(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewQuadric: PGLUquadric; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteQuadric(state: PGLUquadric); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricNormals(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluCylinder(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,
                         stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
                            startAngle, sweepAngle: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricCallback(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewTess: PGLUtesselator; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteTess(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessVertex(tess: PGLUtesselator; const coords: TVector3d; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessProperty(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessNormal(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessCallback(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetTessProperty(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewNurbsRenderer: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPwlCurve(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; const modelMatrix: TMatrix4f; const projMatrix: TMatrix4f; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCallback(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNextContour(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   function wglGetProcAddress(ProcName: PGLChar): Pointer; stdcall; external opengl32;
   function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall; external opengl32;
   function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
   function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall; external opengl32;
   function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
   function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
   function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
   function wglGetCurrentDC: HDC; stdcall; external opengl32;
   function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall; external opengl32;
   function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall; external opengl32;
   function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD; stdcall; external opengl32;
   function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesA (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesW (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name 'wglUseFontBitmapsA';
   function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name 'wglUseFontOutlinesA';
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Extension to the X Window System (GLX) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   // GLX 1.0
   function glXChooseVisual(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
   function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
   procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
   function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl; external opengl32;
   procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
   function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo; pixmap: GLXPixmap): GLXPixmap; cdecl; external opengl32;
   procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
   function glXQueryExtension(dpy: PDisplay; errorb: PGLInt; event: PGLInt): TGLboolean; cdecl; external opengl32;
   function glXQueryVersion(dpy: PDisplay; maj: PGLInt; min: PGLINT): TGLboolean; cdecl; external opengl32;
   function glXIsDirect(dpy: PDisplay; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
   function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
   procedure glXWaitGL; cdecl; external opengl32;
   procedure glXWaitX; cdecl; external opengl32;
   procedure glXUseXFont(font: XFont; first: TGLInt; count: TGLInt; list: TGLint); cdecl; external opengl32;

   // GLX 1.1 and later
   function glXQueryExtensionsString(dpy: PDisplay; screen: TGLInt): PGLChar; cdecl; external opengl32;
   function glXQueryServerString(dpy: PDisplay; screen: TGLInt; name: TGLInt): PGLChar; cdecl; external opengl32;
   function glXGetClientString(dpy: PDisplay; name: TGLInt): PGLChar; cdecl; external opengl32;

   // GLX 1.2 and later
   function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL extension function/procedure definitions'} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.2'} {$ENDIF}
   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.2 Core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor: procedure(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation: procedure(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements: procedure(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;
                                  indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D: procedure(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;
                           border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage3D: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint;  width, height, depth: TGLsizei;
                              format: TGLEnum; Atype: TGLEnum; pixels: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.2 deprecated'} {$ENDIF}
   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
                           table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyColorTable: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTable: procedure(target, format, Atype: TGLEnum; table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable: procedure(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyColorSubTable: procedure(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
     image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;
     image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameteri: procedure(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyConvolutionFilter1D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyConvolutionFilter2D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionFilter: procedure(target, internalformat, Atype: TGLEnum; image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetSeparableFilter: procedure(target, format, Atype: TGLEnum; row, column, span: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSeparableFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,
     column: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetHistogramParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetHistogramParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmax: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmaxParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmaxParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glHistogram: procedure(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMinmax: procedure(target, internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glResetHistogram: procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glResetMinmax: procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.3 Core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture: procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage2D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage1D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage3D: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage2D: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage1D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCompressedTexImage: procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.3 deprecated'} {$ENDIF}
   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glClientActiveTexture: procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1d: procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1dV: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1f: procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1fV: procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1i: procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1iV: procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1s: procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1sV: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2d: procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2f: procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2i: procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2s: procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3d: procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3f: procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3i: procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3s: procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4d: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4f: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4i: procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4s: procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glLoadTransposeMatrixd: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultTransposeMatrixf: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultTransposeMatrixd: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.4 Core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElements: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterfv: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameteri: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameteriv: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.4 deprecated'} {$ENDIF}
   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf: procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordfv: procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordd: procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoorddv: procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordPointer: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b: procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3bv: procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3d: procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3dv: procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3f: procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3fv: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3i: procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3iv: procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3s: procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3sv: procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ub: procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ubv: procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ui: procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3uiv: procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3us: procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3usv: procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColorPointer: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d: procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2dv: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2f: procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2fv: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2i: procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2iv: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2s: procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2sv: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3d: procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3dv: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3f: procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3fv: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3i: procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3iv: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3s: procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3sv: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.5 Core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteQueries: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsQuery:  function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginQuery: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndQuery: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryiv: procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectiv: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectuiv: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer: procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteBuffers: procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsBuffer: function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferData: procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferSubData: procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferSubData: procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMapBuffer: function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnmapBuffer: function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.0 Core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate: procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers: procedure(n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate: procedure(face, sfail, dpfail, dppass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glStencilFuncSeparate: procedure(face, func: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glStencilMaskSeparate: procedure(face: TGLenum; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader: procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindAttribLocation: procedure(_program: TGLuint; index: TGLuint; const name: PGLChar);
   glCompileShader: procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateProgram: function(): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateShader: function(_type: TGLenum): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteShader: procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDetachShader: procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableVertexAttribArray: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableVertexAttribArray: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveAttrib: procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniform: procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttachedShaders: procedure(_program: TGLuint; maxCount: TGLsizei; count: PGLSizei; obj: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttribLocation: function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramiv: procedure(_program: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramInfoLog: procedure(_program: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderiv: procedure(shader: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderInfoLog: procedure(shader: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderSource: procedure(shader:TGLuint; bufSize: TGLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformLocation: function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformfv: procedure(_program: TGLuint; location: TGLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformiv: procedure(_program: TGLuint; location: TGLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdv: procedure(index:TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfv: procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribiv: procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointerv: procedure(index: TGLuint; pname: TGLenum; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgram: function(_program: TGLuint):TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsShader: function(shader: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLinkProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glShaderSource: procedure(shader: TGLuint; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUseProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1f: procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1i: procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glValidateProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1d: procedure(index:TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dv: procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1f: procedure(index:TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fv: procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1s: procedure(index:TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sv: procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2d: procedure(index:TGLuint; x,y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dv: procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2f: procedure(index:TGLuint; x,y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fv: procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2s: procedure(index:TGLuint; x,y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sv: procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3d: procedure(index:TGLuint; x,y,z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dv: procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3f: procedure(index:TGLuint; x,y,z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fv: procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3s: procedure(index:TGLuint; x,y,z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sv: procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nbv: procedure(index:TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Niv: procedure(index:TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nsv: procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nub: procedure(index:TGLuint; x,y,z,w: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nubv: procedure(index:TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nuiv: procedure(index:TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nusv: procedure(index:TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4bv: procedure(index:TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4d: procedure(index:TGLuint; x,y,z,w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dv: procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4f: procedure(index:TGLuint; x,y,z,w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fv: procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4iv: procedure(index:TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4s: procedure(index:TGLuint; x,y,z,w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sv: procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubv: procedure(index:TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4uiv: procedure(index:TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4usv: procedure(index:TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointer: procedure(index:TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride:TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.1 Core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)

   // promoted to core v2.1 from GL_EXT_texture_sRGB (#315)
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x2fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2x4fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x2fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x4fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x3fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.0 Core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i: procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2i: procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3i: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4i: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1ui: procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4bv: procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4sv: procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ubv: procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4usv: procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribIPointer: procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                                stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIiv: procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIuiv: procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1ui: procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformuiv: procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFragDataLocation: procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFragDataLocation: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender: procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndConditionalRender: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor: procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi: procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   //glClearColorIui: procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIiv: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIuiv: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIiv: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIuiv: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski: procedure(index: TGLuint; r: TGLboolean; g: TGLboolean;
                            b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBooleani_v: procedure(target: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegeri_v: procedure(target: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnablei: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisablei: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsEnabledi: function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   //promoted to core v3.0 from GL_EXT_transform_feedback
   glBindBufferRange: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBase: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedback: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedback: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryings: procedure(_program: TGLuint; count: TGLsizei;
                                      const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVarying: procedure(_program: TGLuint; index: TGLuint;
     bufSize: TGLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // New commands in OpenGL 3.0
   glClearBufferiv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferuiv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferfv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferfi: procedure(buffer: TGLenum; drawbuffer: TGLint; depth: TGLfloat; stencil: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetStringi: function(name: TGLenum; index: TGLuint): PGLChar;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.1 Core
   //  ###########################################################

   glDrawArraysInstanced: procedure(mode: TGLenum; first: TGLint; count: TGLsizei; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstanced: procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexBuffer: procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrimitiveRestartIndex: procedure(index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.2'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.2 Core
   //  ###########################################################

   glGetInteger64i_v: procedure(target: TGLenum; index: TGLuint; data: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameteri64v: procedure(target: TGLenum; pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameteri: procedure(_program: TGLuint; pname: TGLenum; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture: procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
//   glFramebufferTextureFace: procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; face: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // OpenGL 3.2 also reuses entry points from these extensions:
   // GL_ARB_draw_elements_base_vertex
   // GL_ARB_provoking_vertex
   // GL_ARB_sync
   // GL_ARB_texture_multisample

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.3'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.3 Core
   //  ###########################################################

   // OpenGL 3.3 reuses entry points from these extensions:
   // GL_ARB_blend_func_extended (ARB #78)
   // GL_ARB_explicit_attrib_location (ARB #79) (none)
   // GL_ARB_occlusion_query2 (ARB #80)
   // GL_ARB_sampler_objects (ARB #81)
   // GL_ARB_shader_bit_encoding (ARB #82)
   // GL_ARB_texture_rgb10_a2ui (ARB #83)
   // GL_ARB_texture_swizzle (ARB #84)
   // GL_ARB_timer_query (ARB #85)
   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 4.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 4.0 Core
   //  ###########################################################

   // OpenGL 4.0 uses entry points from these extensions:
   // GL_ARB_draw_indirect (ARB #87)
   // GL_ARB_gpu_shader5 (ARB #88) (none)
   // GL_ARB_gpu_shader_fp64 (ARB #89)
   // GL_ARB_shader_subroutine (ARB #90)
   // GL_ARB_tessellation_shader (ARB #91)
   // GL_ARB_texture_buffer_object_rgb32 (ARB #92) (none)
   // GL_ARB_transform_feedback2 (ARB #93)
   // GL_ARB_transform_feedback3 (ARB #94)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Utility (GLU) function/procedure definitions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                     GLU extensions
   //  ###########################################################

   // GLU extensions
   gluNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   gluNewNurbsTessellatorEXT: function: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   gluDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) function/procedure definitions for ARB approved extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB: function(DC: HDC; iLayerPlane: Integer; uType: TGLenum) : Integer; stdcall;
   wglDeleteBufferRegionARB: procedure(hRegion: Integer); stdcall;
   wglSaveBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
   wglRestoreBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer;
     xSrc, ySrc: Integer): BOOL; stdcall;

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB: function(DC: HDC): PGLChar; stdcall;

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
     const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
   wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
     const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
   wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
     nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB: function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
   wglGetCurrentReadDCARB: function(): HDC; stdcall;

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB: function(DC: HDC; iPixelFormat: GLInt; iWidth, iHeight : GLInt;
     const piAttribList: PGLint) : HPBUFFERARB; stdcall;
   wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
   wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
   wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
   wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
     piValue: PGLint) : BOOL; stdcall;

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
   wglReleaseTexImageARB: function(hpBuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
   wglSetPbufferAttribARB: function(hpBuffer: HPBUFFERARB; const piAttribList: PGLint): BOOL; stdcall;

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB: function(DC: HDC; hShareContext: HGLRC;
				     attribList: PGLint):HGLRC; stdcall;
   // WGL_NV_gpu_affinity
   wglEnumGpusNV: function(iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean;
   wglEnumGpuDevicesNV: function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPUDevice): Boolean;
   wglCreateAffinityDCNV: function(hGpuList: PHGPUNV): HDC;
   wglEnumGpusFromAffinityDCNV: function(hAffinityDC: HDC; iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean;
   wglDeleteDCNV: function(hdc: HDC): Boolean;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;
   wglGetSwapIntervalEXT: function : Integer; stdcall;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'GLX function/procedure definitions for ARB approved extensions'} {$ENDIF}
 {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved GLX extensions
   //  ###########################################################

   // GLX 1.3 and later
   glXChooseFBConfig: function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfig; cdecl;
   glXGetFBConfigAttrib: function(dpy: PDisplay; config: GLXFBConfig; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
   glXGetFBConfigs: function(dpy: PDisplay; screen: TGLInt; nelements: PGLInt): GLXFBConfig; cdecl;
   glXGetVisualFromFBConfig: function(dpy: PDisplay; config: GLXFBConfig): PXVisualInfo; cdecl;
   glXCreateWindow: function(dpy: PDisplay; config: GLXFBConfig; win: GLXWindow; const attribList: PGLInt): GLXWindow; cdecl;
   glXDestroyWindow: procedure(dpy: PDisplay; window: GLXWindow); cdecl;
   glXCreatePixmap: function(dpy: PDisplay; config: GLXFBConfig; pixmap: GLXPixmap; attribList: PGLInt): GLXPixmap; cdecl;
   glXDestroyPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
   glXCreatePbuffer: function(dpy: PDisplay; config: GLXFBConfig; attribList: PGLInt): GLXPBuffer; cdecl;
   glXDestroyPbuffer: procedure(dpy: PDisplay; pbuf: GLXPBuffer); cdecl;
   glXQueryDrawable: procedure(dpy: PDisplay; draw: GLXDrawable; attribute: TGLInt; value: PGLuint); cdecl;
   glXCreateNewContext: function(dpy: PDisplay; config: GLXFBConfig; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
   glXMakeContextCurrent: function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
   glXGetCurrentReadDrawable: function: GLXDrawable; cdecl;
   glXQueryContext: function(dpy: PDisplay; ctx: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
   glXSelectEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
   glXGetSelectedEvent: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
   glXBindTexImageARB: function(dpy: PDisplay; pbuffer: GLXPbuffer; buffer: TGLInt): TGLboolean; cdecl;
   glXReleaseTexImageARB: function(dpy: PDisplay; pbuffer: GLXPbuffer; buffer: TGLint): TGLboolean; cdecl;
   glxDrawableAttribARB: function(dpy: PDisplay; draw: GLXDrawable; const attribList:PGLInt): TGLboolean; cdecl;

   //GLX 1.4
   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB: function(dpy: PDisplay; config: GLXFBConfig;
		    share_context: GLXContext; direct: TGLBoolean;
		    attrib_list: PGLint): GLXContext; cdecl;
   glXGetProcAddress: function(const name: PAnsiChar): pointer; cdecl;
   glXGetProcAddressARB: function (const name: PAnsiChar): pointer; cdecl;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'GLX function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT GLX extensions
   //  ###########################################################

   // GLX_SGI_swap_control (EXT #40)
   glXSwapIntervalSGI: function(interval: TGLint): TGLint; cdecl;
   glXGetVideoSyncSGI: function(count: PGLuint): TGLInt; cdecl;
   glXWaitVideoSyncSGI: function(divisor: TGLInt; remainder: TGLInt; count: PGLuint): TGLInt; cdecl;
   glXFreeContextEXT: procedure(dpy: PDisplay; context: GLXContext); cdecl;
   glXGetContextIDEXT: function(const context: GLXContext): GLXContextID; cdecl;
   glXGetCurrentDisplayEXT: function: PDisplay; cdecl;
   glXImportContextEXT: function(dpy: PDisplay; contextID: GLXContextID): GLXContext; cdecl;
   glXQueryContextInfoEXT: function(dpy: PDisplay; context: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
   glXCopySubBufferMESA: procedure(dpy: PDisplay; drawable: GLXDrawable; x: TGLInt; y: TGLInt; width: TGLInt; height: TGLInt); cdecl;
   glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;
   glXReleaseBuffersMESA: function(dpy: PDisplay; d: GLXDrawable): TGLboolean; cdecl;
   glXSet3DfxModeMESA: function(mode: TGLint): TGLboolean; cdecl;

   glXBindTexImageEXT: procedure(dpy: PDisplay; drawable: GLXDrawable; buffer: GLint; const attrib_list: PGLint); cdecl;
   glXReleaseTexImageEXT: procedure(dpy: PDisplay; drawable: GLXDrawable; buffer: GLint); cdecl;

   //GLX 1.4
   glXMakeCurrentReadSGI: function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
   glXGetCurrentReadDrawableSGI: function: GLXDrawable; cdecl;
   glXGetFBConfigAttribSGIX: function(dpy: PDisplay; config: GLXFBConfigSGIX; attribute: TGLInt; value: PGLInt):TGLInt; cdecl;
   glXChooseFBConfigSGIX: function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfigSGIX; cdecl;
   glXCreateGLXPixmapWithConfigSGIX: function(dpy: PDisplay; config:GLXFBConfigSGIX;  pixmap: GLXPixmap): GLXPixmap; cdecl;
   glXCreateContextWithConfigSGIX: function(dpy: PDisplay; config: GLXFBConfigSGIX; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
   glXGetVisualFromFBConfigSGIX: function(dpy: PDisplay; config: GLXFBConfigSGIX): PXVisualInfo; cdecl;
   glXGetFBConfigFromVisualSGIX: function(dpy: PDisplay; vis: PXVisualInfo): GLXFBConfigSGIX; cdecl;
   glXCreateGLXPbufferSGIX: function(dpy: PDisplay; config: GLXFBConfigSGIX; width:PGLuint;  height: PGLuint; attribList: PGLInt): GLXPBufferSGIX; cdecl;
   glXDestroyGLXPbufferSGIX: procedure(dpy: PDisplay; pbuf: GLXFBConfigSGIX); cdecl;
   glXQueryGLXPbufferSGIX: function(dpy: PDisplay; pbuf: GLXFBConfigSGIX; attribute: PGLInt; value: PGLuint): TGLInt; cdecl;
   glXSelectEventSGIX: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: PGLuint64); cdecl;
   glXGetSelectedEventSGIX: procedure(dpy: PDisplay; drawable: GLXDrawable; mask: PGLuint64); cdecl;
   glXCushionSGI: procedure(dpy: PDisplay; window: TWindow; cushion: TGLfloat); cdecl;
   glXBindChannelToWindowSGIX: function(dpy: PDisplay; screen: TGLInt; channel: TGLInt; window: TWindow): TGLInt; cdecl;
   glXChannelRectSGIX: function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; x, y, w, h: TGLInt): TGLInt; cdecl;
   glXQueryChannelRectSGIX: function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; dx, dy, dw, dh: TGLInt): TGLInt; cdecl;
   glXQueryChannelDeltasSGIX: function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; x, y, w, h: TGLInt): TGLInt; cdecl;
   glXChannelRectSyncSGIX: function (dpy: PDisplay; screen: TGLInt; channel: TGLInt; synctype: TGLEnum): TGLInt; cdecl;
   glXJoinSwapGroupSGIX: procedure (dpy: PDisplay; drawable: GLXDrawable; member: GLXDrawable); cdecl;
   glXBindSwapBarrierSGIX: procedure (dpy: PDisplay; drawable: GLXDrawable; barrier: TGLInt); cdecl;
   glXQueryMaxSwapBarriersSGIX: procedure (dpy: PDisplay; screen: TGLInt; max: TGLInt); cdecl;
   glXQueryHyperpipeNetworkSGIX: function (dpy: PDisplay; npipes:PGLint): PGLXHyperpipeNetworkSGIX; cdecl;
   glXHyperpipeConfigSGIX: function(dpy: PDisplay; networkId, npipes: PGLint; cfg: PGLXHyperpipeConfigSGIX; hpId: PGLInt): TGLint; cdecl;
   glXQueryHyperpipeConfigSGIX: function(dpy: PDisplay; hpId: TGLInt; npipes: PGLInt): PGLXHyperpipeConfigSGIX; cdecl;
   glXDestroyHyperpipeConfigSGIX: function(dpy: PDisplay; hpId:TGLint): PGLInt; cdecl;
   glXBindHyperpipeSGIX: function(dpy: PDisplay; hpId: PGLint): PGLInt; cdecl;
   glXQueryHyperpipeBestAttribSGIX: function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; attribList: TGLint; returnAttribList: TGLint): TGLint; cdecl;
   glXHyperpipeAttribSGIX: function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; attribList: TGLint): TGLint; cdecl;
   glXQueryHyperpipeAttribSGIX: function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; returnAttribList: TGLint): TGLint; cdecl;
   glXGetAGPOffsetMESA: function(param: Pointer): PGLInt;cdecl;
   glXEnumerateVideoDevicesNV: function(dpy: PDisplay; screen: TGLInt; nelements: PGLint): PGLuint; cdecl;
   glXBindVideoDeviceNV: function(dpy: PDisplay; video_slot: TGLInt; video_device: TGLInt; attrib_list: PGLint): TGLint; cdecl;
   GetVideoDeviceNV: function(dpy: PDisplay; screen: TGLInt; numVideoDevices: TGLInt; pVideoDevice: GLXVideoDeviceNV): TGLInt; cdecl;

   glXAllocateMemoryNV: procedure( size: TGLsizei; readFrequency: TGLfloat; writeFrequency: TGLfloat; priority: TGLfloat ); cdecl;
   glXFreeMemoryNV: procedure ( GLvoid: pointer ); cdecl;

   glXReleaseVideoDeviceNV: function(dpy: PDisplay; screen: TGLInt; VideoDevice: GLXVideoDeviceNV): TGLuint; cdecl;
   glXBindVideoImageNV: function(dpy: PDisplay; VideoDevice: GLXVideoDeviceNV; pbuf: GLXPbuffer; iVideoBuffer: TGLInt): TGLuint; cdecl;
   glXReleaseVideoImageNV: function(dpy: PDisplay; pbuf: GLXPbuffer): TGLInt; cdecl;
   glXSendPbufferToVideoNV: function(dpy: PDisplay; pbuf: GLXPbuffer; iBufferType: TGLInt; pulCounterPbuffer: TGLuint64; bBlock: TGLboolean): TGLInt; cdecl;
   glXGetVideoInfoNV: function(dpy: PDisplay; screen: TGLInt; VideoDevice: GLXVideoDeviceNV; pulCounterOutputPbuffer: TGLuInt64; pulCounterOutputVideo: TGLuInt64): TGLInt; cdecl;
   glXJoinSwapGroupNV: function(dpy: PDisplay; drawable: GLXDrawable; group: TGLuint): TGLBoolean; cdecl;
   glXBindSwapBarrierNV: function(dpy: PDisplay; group: TGLuint; barrier: TGLuint): TGLboolean; cdecl;
   glXQuerySwapGroupNV: function(dpy: PDisplay; drawable: GLXDrawable; group: PGLuint; barrier: PGLuint): TGLBoolean; cdecl;
   glXQueryMaxSwapGroupsNV: function(dpy: PDisplay; screen: TGLInt; maxGroups: TGLuInt; maxBarriers: TGLuInt): TGLBoolean; cdecl;
   glXQueryFrameCountNV: function(dpy: PDisplay; screen: TGLInt; count: TGLuint): TGLBoolean; cdecl;
   glXResetFrameCountNV: function(dpy: PDisplay; screen: TGLInt): TGLBoolean; cdecl;
   glXBindVideoCaptureDeviceNV: function(dpy: PDisplay; video_capture_slot: TGLuint; device: GLXVideoCaptureDeviceNV): TGLint; cdecl;
   glXEnumerateVideoCaptureDevicesNV: function(dpy: PDisplay; screen: TGLInt; nelements: PGLint): GLXVideoCaptureDeviceNV; cdecl;
   glxLockVideoCaptureDeviceNV: procedure (dpy: PDisplay; device: GLXVideoCaptureDeviceNV); cdecl;
   glXQueryVideoCaptureDeviceNV: function(dpy: PDisplay; device: GLXVideoCaptureDeviceNV; attribute:TGLint; value: PGLint): TGLint; cdecl;
   glXReleaseVideoCaptureDeviceNV: procedure(dpy: PDisplay; device: GLXVideoCaptureDeviceNV); cdecl;
   glXSwapIntervalEXT: function(dpy: PDisplay; drawable: GLXDrawable; interval:TGLint): TGLint; cdecl;
   glXCopyImageSubDataNV: procedure(dpy: PDisplay; srcCtx: GLXContext; srcName: TGLuint; srcTarget: TGLenum;
                         srcLevel: TGLuint; srcX: TGLuint;
                         srcY: TGLuint; srcZ: TGLuint;
                         dstCtx:GLXContext; dstName:TGLuint; dstTarget: TGLenum; dstLevel: TGLint;
                         dstX: TGLint; dstY: TGLint; dstZ: TGLint; width: GLsizei; height: GLsizei;
                         depth: GLsizei); cdecl;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL function/procedure definitions for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                  ARB approved extensions
   //  ###########################################################

   // unknown ARB extension
   glSamplePassARB: procedure(pass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   
   // GL_ARB_multitexture (ARB #1)
   glActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClientActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1dVARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1fARB: procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1fVARB: procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1iVARB: procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1sARB: procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1sVARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2dARB: procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2fARB: procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2iARB: procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2sARB: procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3dARB: procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3fARB: procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3iARB: procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3sARB: procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4dARB: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4fARB: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4iARB: procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4sARB: procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_transpose_matrix (ARB #3)
   glLoadTransposeMatrixfARB: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLoadTransposeMatrixdARB: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultTransposeMatrixfARB: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultTransposeMatrixdARB: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   
   // GL_ARB_multisample (ARB #5)
   glSampleCoverageARB: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_compression (ARB #12)
   glCompressedTexImage3DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage2DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage1DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage3DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage2DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage1DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCompressedTexImageARB: procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_point_parameter (ARB #14)
   glPointParameterfARB: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterfvARB: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_blend (ARB #15) {deprecated?}
   glWeightbvARB: procedure(size: TGLint; weights: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightsvARB: procedure(size: TGLint; weights: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightivARB: procedure(size: TGLint; weights: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightfvARB: procedure(size: TGLint; weights: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightdvARB: procedure(size: TGLint; weights: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightubvARB: procedure(size: TGLint; weights: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightusvARB: procedure(size: TGLint; weights: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightuivARB: procedure(size: TGLint; weights: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightPointerARB: procedure(size: TGLint; _type: TGLenum; stride:TGLsizei;
                                 _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexBlendARB: procedure(count: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_matrix_palette (ARB #16) {deprecated?}
   glCurrentPaletteMatrixARB: procedure(index: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexubvARB: procedure(size: TGLint; indices: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexusvARB: procedure(size: TGLint; indices: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexuivARB: procedure(size: TGLint; indices: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexPointerARB: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_window_pos (ARB #25)
   glWindowPos2dARB: procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2dvARB: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2fARB: procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2fvARB: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2iARB: procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2ivARB: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2sARB: procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2svARB: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3dARB: procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3dvARB: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3fARB: procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3fvARB: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3iARB: procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3ivARB: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3sARB: procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3svARB: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_program (ARB #26)
   glVertexAttrib1dARB: procedure(index: GLuint; x: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fARB: procedure(index: GLuint; x: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sARB: procedure(index: GLuint; x: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sARB: procedure(index: GLuint; x: GLshort; y: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NbvARB: procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NivARB: procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NsvARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NubARB: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NubvARB: procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NuivARB: procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NusvARB: procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4bvARB: procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ivARB: procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubvARB: procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4uivARB: procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4usvARB: procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointerARB: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableVertexAttribArrayARB: procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableVertexAttribArrayARB: procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramStringARB: procedure(target: GLenum; format: GLenum; len: GLsizei; const _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindProgramARB: procedure(target: GLenum; _program: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgramsARB: procedure(n: GLsizei; const programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenProgramsARB: procedure(n: GLsizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramEnvParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramEnvParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramLocalParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramLocalParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramStringARB: procedure(target: GLenum; pname: GLenum; _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdvARB: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfvARB: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribivARB: procedure(index: GLuint; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointervARB: procedure(index: GLuint; pname: GLenum; _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgramARB: function(_program: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_buffer_object (ARB #28)
   glBindBufferARB: procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteBuffersARB: procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenBuffersARB: procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsBufferARB: function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferDataARB: procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferSubDataARB: procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferSubDataARB: procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMapBufferARB: function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnmapBufferARB: function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameterivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferPointervARB: procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_occlusion_query (ARB #29)
   glGenQueriesARB: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteQueriesARB: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsQueryARB:  function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginQueryARB: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndQueryARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryivARB: procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectivARB: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectuivARB: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_shader_objects (ARB #30)
   glDeleteObjectARB: procedure(obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetHandleARB: function(pname: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDetachObjectARB: procedure(containerObj: GLhandleARB; attachedObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateShaderObjectARB: function(shaderType: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glShaderSourceARB: procedure(shaderObj: GLhandleARB; count: GLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompileShaderARB: procedure(shaderObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateProgramObjectARB: function(): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glAttachObjectARB: procedure(containerObj: GLhandleARB; obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLinkProgramARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUseProgramObjectARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glValidateProgramARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fARB: procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1iARB: procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2iARB: procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetObjectParameterfvARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetObjectParameterivARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetInfoLogARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttachedObjectsARB: procedure(containerObj: GLhandleARB; maxCount: GLsizei; count: PGLsizei; obj: PGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformLocationARB: function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformfvARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformivARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderSourceARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_shader (ARB #31)
   glBindAttribLocationARB: procedure(programObj: GLhandleARB; index: GLuint; const name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveAttribARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttribLocationARB: function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_DrawBuffers (ARB #37)
   glDrawBuffersARB: procedure (n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_color_buffer_float (ARB #39)
   glClampColorARB: procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB: procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
            primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedARB: procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
            indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_framebuffer_object (ARB #45)         
   glIsRenderbuffer: function(renderbuffer: TGLuint): TGLBoolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindRenderbuffer: procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteRenderbuffers: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenRenderbuffers: procedure(n: TGLSizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorage: procedure(target: TGLenum; internalformat: TGLenum;
			      width: TGLsizei;  height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorageMultisample: procedure(target: TGLenum; samples: TGLsizei;
					internalformat: TGLenum;
				  width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetRenderbufferParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFramebuffer: function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFramebuffer: procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFramebuffers: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenFramebuffers: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCheckFramebufferStatus: function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture1D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture2D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture3D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint;
			      level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayer: procedure(target: TGLenum; attachment: TGLenum;
				 texture: TGLuint; level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferRenderbuffer: procedure(target: TGLenum; attachment: TGLenum;
				 renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFramebufferAttachmentParameteriv: procedure(target: TGLenum; attachment: TGLenum;
					     pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlitFramebuffer: procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
			 dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
			 mask: TGLbitfield; filter: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenerateMipmap: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB: procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayerARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureFaceARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB: procedure(index: TGLuint; divisor: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange: function(target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr};
	            access: TGLbitfield ): Pointer;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFlushMappedBufferRange: procedure( target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr} );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB: procedure(target: TGLenum; internalformat: TGLEnum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray: procedure(_array: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteVertexArrays: procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenVertexArrays: procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsVertexArray: function(_array: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices: procedure(_program: TGLuint; uniformCount: TGLsizei; uniformNames: PGLPCharArray; uniformIndices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformsiv: procedure(_program: TGLuint; uniformCount: TGLsizei; uniformIndices: PGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformName: procedure(_program: TGLuint; uniformIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformBlockIndex: function(_program: TGLuint; uniformBlockName: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformBlockiv: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformBlockName: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformBlockName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformBlockBinding: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; uniformBlockBinding: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData: procedure(readTarget: TGLenum; writeTarget: TGLenum;
          readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex: procedure(mode: TGLenum; count: TGLsizei;
          _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawRangeElementsBaseVertex: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint;
          count: TGLsizei; _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedBaseVertex: procedure(mode: TGLenum; count: TGLsizei;
          _type: TGLenum; indices: PGLvoid; primcount: TGLsizei; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElementsBaseVertex: procedure(mode: TGLenum; count: PGLsizei;
          _type: TGLenum; var indices; primcount: TGLsizei; basevertex: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex: procedure(mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_sync (ARB #66)
   glFenceSync: function(condition: TGLenum; flags: TGLbitfield): TGLsync;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsSync: function(sync: TGLsync): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteSync: procedure(sync: TGLsync);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClientWaitSync: function(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64): TGLenum;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWaitSync: procedure(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetInteger64v: procedure(pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSynciv: procedure(sync: TGLsync; pname: TGLenum; bufSize: TGLsizei; length: PGLsizei; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample: procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                               width: TGLsizei; height: TGLsizei;
                               fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexImage3DMultisample: procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                               width: TGLsizei; height: TGLsizei; depth: TGLsizei;
                               fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetMultisamplefv: procedure(pname: TGLenum; index: TGLuint; val: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSampleMaski: procedure(index: TGLuint; mask: TGLbitfield);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationi: procedure(buf: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendEquationSeparatei: procedure(buf: TGLuint; modeRGB: TGLenum; modeAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendFunci: procedure(buf: TGLuint; src: TGLenum; dst: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendFuncSeparatei: procedure(buf: TGLuint; srcRGB: TGLenum; dstRGB: TGLenum;
                               srcAlpha: TGLenum; dstAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShading: procedure(value: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_blend_func_extended (ARB #78)
   glBindFragDataLocationIndexed: procedure (_program: TGLuint; colorNumber: TGLuint; index: TGLuint; const name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFragDataIndex: function (_program: TGLuint; const name: PGLchar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_sampler_objects (ARB #81)
   glGenSamplers: procedure(count: TGLsizei; samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteSamplers: procedure(count: TGLsizei; const samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsSampler: function(sampler: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindSampler: procedure(_unit: TGLuint; sampler: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameteri: procedure(sampler: TGLuint; pname: TGLenum; param: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameteriv: procedure(sampler: TGLuint; pname: TGLenum; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameterf: procedure(sampler: TGLuint; pname: TGLenum; param: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameterfv: procedure(sampler: TGLuint; pname: TGLenum; const params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameterIiv: procedure(sampler: TGLuint; pname: TGLenum; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplerParameterIuiv: procedure(sampler: TGLuint; pname: TGLenum; const params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSamplerParameteriv: procedure(sampler: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSamplerParameterIiv: procedure(sampler: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSamplerParameterfv: procedure(sampler: TGLuint; pname: TGLenum; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSamplerParameterIfv: procedure(sampler: TGLuint; pname: TGLenum; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_timer_query (ARB #85)
   glQueryCounter: procedure(id: TGLuint; target: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjecti64v: procedure(id: TGLuint; pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectui64v: procedure(id: TGLuint; pname: TGLenum; params: PGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
   glVertexP2ui: procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexP2uiv: procedure(_type: TGLenum; const value: PGLuint );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexP3ui: procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexP3uiv: procedure(_type: TGLenum; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexP4ui: procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexP4uiv: procedure(_type: TGLenum; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP1ui: procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP1uiv: procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP2ui: procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP2uiv: procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP3ui: procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP3uiv: procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP4ui: procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordP4uiv: procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP1ui: procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP1uiv: procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP2ui: procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP2uiv: procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP3ui: procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP3uiv: procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP4ui: procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoordP4uiv: procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glNormalP3ui: procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glNormalP3uiv: procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorP3ui: procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorP3uiv: procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorP4ui: procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorP4uiv: procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColorP3ui: procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColorP3uiv: procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP1ui: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP1uiv: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP2ui: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP2uiv: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP3ui: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP3uiv: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP4ui: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribP4uiv: procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_indirect (ARB #87)
   glDrawArraysIndirect: procedure(mode: TGLenum; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsIndirect: procedure(mode: TGLenum; _type: TGLenum; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_gpu_shader_fp64 (ARB #89)
   glUniform1d: procedure(location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2d: procedure(location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3d: procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4d: procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1dv: procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2dv: procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3dv: procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4dv: procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2x3dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2x4dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x2dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x4dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x2dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x3dv: procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformdv: procedure(_program: TGLuint; location: TGLint; params : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // glProgramUniformXXX only valid if EXT_direct_state_access is available
   glProgramUniform1dEXT: procedure(_program: TGLuint; location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform2dEXT: procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform3dEXT: procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform4dEXT: procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform1dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform2dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform3dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniform4dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix2dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix3dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix4dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix2x3dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix2x4dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix3x2dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix3x4dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix4x2dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformMatrix4x3dvEXT: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_shader_subroutine (ARB #90)
   glGetSubroutineUniformLocation: function(_program: TGLuint; shadertype: TGLenum; const name: PGLchar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSubroutineIndex: function(_program: TGLuint; shadertype: TGLenum; const name: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveSubroutineUniformiv: procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; pname: TGLenum; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveSubroutineUniformName: procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; bufsize: TGLsizei; length: PGLsizei; name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveSubroutineName: procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; bufsize: TGLsizei; length: PGLsizei; name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformSubroutinesuiv: procedure(shadertype: TGLenum; count: TGLsizei; const indices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformSubroutineuiv: procedure(shadertype: TGLenum; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramStageiv: procedure(_program: TGLuint; shadertype: TGLenum; pname: TGLenum; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_tessellation_shader (ARB #91)
   glPatchParameteri: procedure(pname: TGLenum; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPatchParameterfv: procedure(pname: TGLenum; const values: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_transform_feedback2 (ARB #93)
   glBindTransformFeedback: procedure(target: TGLenum; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteTransformFeedbacks: procedure(n: TGLsizei; const ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenTransformFeedbacks: procedure(n: TGLsizei; ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsTransformFeedback: function(id: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPauseTransformFeedback: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glResumeTransformFeedback: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawTransformFeedback: procedure(mode: TGLenum; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_transform_feedback3 (ARB #94)
   glDrawTransformFeedbackStream: procedure(mode: TGLenum; id: TGLuint; stream: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginQueryIndexed: procedure(target: TGLenum; index: TGLuint; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndQueryIndexed: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryIndexediv: procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // Unknown Vendor/EXT functions
   glArrayElementArrayEXT: procedure(mode: TGLEnum; count: TGLsizei; pi: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN: procedure(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_color (EXT #2)
   glBlendColorEXT: procedure(red, green, blue: TGLclampf; alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_polygon_offset (EXT #3)
   glPolygonOffsetEXT: procedure(factor, bias: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture3D (EXT #6)
   glTexImage3DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width, height, depth: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_subtexture (EXT #9)
   glTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_copy_texture (EXT #10)
   glCopyTexImage1DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexImage2DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture_object (EXT #20)
   glGenTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindTextureEXT: procedure(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrioritizeTexturesEXT: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glAreTexturesResidentEXT: function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsTextureEXT: function(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_SGIS_multisample (EXT #25)
   glSampleMaskSGIS: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplePatternSGIS: procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_minmax (EXT #37)
   glBlendEquationEXT: procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_paletted_texture (EXT #78)
   glColorTableEXT: procedure(target, internalFormat: TGLEnum; width: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorSubTableExt: procedure(target: TGLEnum; start, count: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableEXT: procedure(target, format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

//   glGetColorTableParameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
//   glGetColorTableParameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_index_material (EXT #94)
   glIndexMaterialEXT: procedure(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_index_func (EXT #95)
   glIndexFuncEXT: procedure(func: TGLEnum; ref: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_compiled_vertex_array (EXT #97)
   glLockArraysEXT: procedure(first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnlockArraysEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_range_elements (EXT #112)
   glDrawRangeElementsEXT: procedure(mode: TGLenum; start, Aend: TGLuint; Count: TGLsizei; Atype: TGLenum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_scene_marker (EXT #120)
   glBeginSceneEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndSceneEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_secondary_color (EXT #145)
   glSecondaryColor3bEXT: procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3bvEXT: procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3dEXT: procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3dvEXT: procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3fEXT: procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3fvEXT: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3iEXT: procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ivEXT: procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3sEXT: procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3svEXT: procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ubEXT: procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ubvEXT: procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3uiEXT: procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3uivEXT: procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3usEXT: procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3usvEXT: procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColorPointerEXT: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_multi_draw_arrays (EXT #148)
   glMultiDrawArraysEXT: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElementsEXT: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_fog_coord (EXT #149)
   glFogCoordfEXT: procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoordfvEXT: procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoorddEXT: procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoorddvEXT: procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoordPointerEXT: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_func_separate (EXT #173)
   glBlendFuncSeparateEXT: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_vertex_array_range (EXT #190)
   glFlushVertexArrayRangeNV: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexArrayRangeNV: procedure(Size: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   wglAllocateMemoryNV: function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   wglFreeMemoryNV: procedure(ptr: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_register_combiners (EXT #191)
   glCombinerParameterfvNV: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameterfNV: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameterivNV: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerInputNV: procedure(stage, portion, variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerOutputNV: procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: TGLenum; abDotProduct, cdDotProduct, muxSum: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFinalCombinerInputNV: procedure(variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerInputParameterfvNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerInputParameterivNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFinalCombinerInputParameterfvNV: procedure(variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFinalCombinerInputParameterivNV: procedure(variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_MESA_resize_buffers (EXT #196)
   glResizeBuffersMESA: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_3DFX_tbuffer (EXT #208)
   glTbufferMask3DFX: procedure(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_multisample (EXT #209)
   glSampleMaskEXT: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplePatternEXT: procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_SGIS_texture_color_mask (EXT #214)
   glTextureColorMaskSGIS: procedure(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_fence (EXT #222)
   glGenFencesNV: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFencesNV: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSetFenceNV: procedure(fence: TGLuint; condition: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTestFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFinishFenceNV: procedure(fence: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFenceivNV: procedure(fence: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_vertex_program (EXT #233)
   glAreProgramsResidentNV: procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindProgramNV: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glExecuteProgramNV: procedure(target: TGLenum; id: TGLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramParameterdvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramParameterfvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramivNV: procedure (id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramStringNV: procedure (id: TGLuint; pname: TGLenum; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTrackMatrixivNV: procedure (target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribivNV: procedure (index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointervNV: procedure (index: TGLuint; pname: TGLenum; pointer: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgramNV: function (id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLoadProgramNV: procedure (target: TGLenum; id: TGLuint; len: TGLSizei; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4dNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4dvNV: procedure (target: TGLenum; index: TGLuint; v: PGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4fNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4fvNV: procedure (target: TGLenum; index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameters4dvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameters4fvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRequestResidentProgramsNV: procedure (n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTrackMatrixNV: procedure (target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointerNV: procedure (index: TGLuint; fsize: TGLint; vertextype: TGLenum; stride: TGLSizei; pointer: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dNV: procedure (index: TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fNV: procedure (index: TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sNV: procedure (index: TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fvNV: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubvNV: procedure (index: TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4ubvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_occlusion_query (EXT #261)
   glGenOcclusionQueriesNV: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteOcclusionQueriesNV: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsOcclusionQueryNV: function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginOcclusionQueryNV: procedure(id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndOcclusionQueryNV: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetOcclusionQueryivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetOcclusionQueryuivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_point_sprite (EXT #262)
   glPointParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterivNV: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_stencil_two_side (EXT #268)
   glActiveStencilFaceEXT: procedure(face: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ATI_draw_buffers (EXT #277)
   glDrawBuffersATI: procedure(n: GLsizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_primitive_restart (EXT #285)
   glPrimitiveRestartNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrimitiveRestartIndexNV: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_depth_bounds_test (EXT #297)
   glDepthBoundsEXT: procedure(zmin: TGLclampd; zmax: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_equation_separate (EXT #299)
   glBlendEquationSeparateEXT: procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_object (EXT #310)
   glIsRenderbufferEXT: function(renderbuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindRenderbufferEXT: procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteRenderbuffersEXT: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenRenderbuffersEXT: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorageEXT: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetRenderbufferParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFramebufferEXT: function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFramebufferEXT: procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFramebuffersEXT: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenFramebuffersEXT: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCheckFramebufferStatusEXT: function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture1DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture2DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture3DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; zoffset: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferRenderbufferEXT: procedure(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFramebufferAttachmentParameterivEXT: procedure(target: TGLenum; attachment: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenerateMipmapEXT: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT: procedure(stencilTagBits: TGLsizei; stencilClearTag: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT: procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
                            dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
                            mask: TGLbitfield; filter: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT: procedure(target: TGLenum; samples: TGLsizei;
            internalformat: TGLenum; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT: procedure(id: TGLuint; pname: TGLenum; params: PGLint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectui64vEXT: procedure(id: TGLuint; pname: TGLenum; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_gpu_program_parameters (EXT #320)
   glProgramEnvParameters4fvEXT:   procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                     const params:PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameters4fvEXT: procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                     const params:PGLFloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_geometry_program4 (EXT #323)
   glProgramVertexLimitNV: procedure (target: TGLenum; limit: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_geometry_shader4 (EXT #324)
   glProgramParameteriEXT: procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayerEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureFaceEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_gpu_shader4 (EXT #326)
   glVertexAttribI1iEXT: procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uiEXT: procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4bvEXT: procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4svEXT: procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ubvEXT: procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4usvEXT: procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribIPointerEXT: procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                                stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIivEXT: procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIuivEXT: procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uiEXT: procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformuivEXT: procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFragDataLocationEXT: procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFragDataLocationEXT: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT: procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
            primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedEXT: procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
            indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_packed_float (#328)
   // WGL_EXT_pixel_format_packed_float
   // GLX_EXT_fbconfig_packed_float


   // GL_EXT_texture_array (#329)
   //glFramebufferTextureLayerEXT: procedure(target: TGLenum; attachment: TGLenum;
   //                                texture: TGLuint; level: TGLint; layer: TGLint);


   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT: procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT: procedure(buf: TGLuint; r: TGLboolean; g: TGLboolean;
                            b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBooleanIndexedvEXT: procedure(value: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegerIndexedvEXT: procedure(value: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableIndexedEXT: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableIndexedEXT: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsEnabledIndexedEXT: function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                  offset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferOffsetNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                   offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBaseNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackAttribsNV: procedure(count: TGLsizei; attribs: PGLint;
                                           bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryingsNV: procedure(_program: TGLuint; count: TGLsizei;
                                            locations: PGLint;
                                            bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedbackNV: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedbackNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   glGetVaryingLocationNV: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveVaryingNV: procedure(_program: TGLuint; index: TGLuint;
                                   bufSize: TGLsizei; length: PGLsizei; size: PGLsizei;
                                   _type: TGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glActiveVaryingNV: procedure(_program: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVaryingNV: procedure(_program: TGLuint; index: TGLuint;
                                              location: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}


   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT: procedure(_program: TGLUint; location: TGLint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformBufferSizeEXT: function(_program: TGLuint; location: TGLint): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformOffsetEXT: function(_program: TGLuint; location: TGLint): PGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT: procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearColorIuiEXT: procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIuivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIuivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV: procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndConditionalRenderNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferOffsetEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBaseEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedbackEXT: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedbackEXT: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryingsEXT: procedure(_program: TGLuint; count: TGLsizei;
                                      const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVaryingEXT: procedure(_program: TGLuint; index: TGLuint;
                                        bufSize: TGLsizei; length: PGLsizei;
                                        size: PGLsizei; _type: PGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_AMD_vertex_shader_tessellator (#363)
   glTessellationFactorAMD: procedure(factor: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTessellationModeAMD: procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV: procedure(
     srcName: GLuint; srcTarget: GLenum; srcLevel: GLint;
     srcX: GLint; srcY: GLint; srcZ: GLint;
     dstName: GLuint; dstTarget: GLenum; dstLevel: GLint;
     dstX: GLint; dstY: GLint; dstZ: GLint;
     width: GLsizei; height: GLsizei; depth: GLsizei);  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_shader_buffer_load (#379)
   glMakeBufferResidentNV: procedure(target: TGLenum; access: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMakeBufferNonResidentNV: procedure(target: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsBufferResidentNV: function(target: TGLenum): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMakeNamedBufferResidentNV: procedure(buffer: TGLuint; access: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMakeNamedBufferNonResidentNV: procedure(buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsNamedBufferResidentNV: function (buffer: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameterui64vNV: procedure(target: TGLenum; pname: TGLenum; params: PGLuint64EXT );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetNamedBufferParameterui64vNV: procedure(buffer: TGLuint; pname: TGLenum; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegerui64vNV: procedure(value: TGLenum; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformui64NV: procedure(location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformui64vNV: procedure(location: GLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformui64vNV: procedure(_program: TGLuint; location: TGLint; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformui64NV: procedure(_program: TGLuint; location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramUniformui64vNV: procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_vertex_buffer_unified_memory (#380)
   glBufferAddressRangeNV: procedure(pname: TGLenum; index: TGLuint; address: TGLuint64EXT; length: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexFormatNV: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glNormalFormatNV: procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorFormatNV: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIndexFormatNV: procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexCoordFormatNV: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEdgeFlagFormatNV: procedure(stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColorFormatNV: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoordFormatNV: procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribFormatNV: procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribIFormatNV: procedure(index: TGLuint; size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegerui64i_vNV: procedure(value: TGLenum; index: TGLuint; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // Special Gremedy debugger extension
   glFrameTerminatorGREMEDY: procedure(); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glStringMarkerGREMEDY: procedure(len: GLsizei; str: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}


//------------------------------------------------------------------------------
function GLLibGetProcAddress(ProcName: PGLChar): Pointer;
function GLGetProcAddress(ProcName: PGLChar): Pointer;
procedure ReadExtensions;
procedure ReadImplementationProperties;
{$IFDEF SUPPORT_WGL}
procedure ReadWGLExtensions;
procedure ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
procedure ReadGLXExtensions;
procedure ReadGLXImplementationProperties;
{$ENDIF}

procedure CloseOpenGL;
function InitOpenGL : Boolean;
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
function IsOpenGLInitialized: Boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL : Boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
function IsOpenGLLoaded : Boolean;

function IsMesaGL : Boolean;
procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
function IsVersionMet(MajorVersion,MinorVersion, actualMajorVersion, actualMinorVersion:Integer): boolean;
function IsOpenGLVersionMet(MajorVersion,MinorVersion: Integer): boolean;

type
  EOpenGLError = class(Exception);

{: Gets the oldest error from OpenGL engine and tries to clear the error queue.<p> }
procedure CheckOpenGLError;
{: Clears all pending OpenGL errors. }
procedure ClearGLError;
{: Raises an EOpenGLError with 'msg' error string. }
procedure RaiseOpenGLError(const msg : String);

var
   vIgnoreOpenGLErrors : Boolean = false;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ************** Windows specific ********************
{$IFDEF MSWINDOWS}
const
   INVALID_MODULEHANDLE = 0;

var
   GLHandle: HINST;
   GLUHandle: HINST;

function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := wglGetProcAddress(ProcName);
end;

{$ENDIF}

// ************** UNIX specific ********************
{$IFDEF UNIX}
const
   INVALID_MODULEHANDLE = 0;//nil;

var
   GLHandle: TLibHandle = 0;//Pointer;
   GLUHandle: TLibHandle = 0;//Pointer;
   
function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  if @glXGetProcAddress<>nil then
    result := glXGetProcAddress(ProcName);

  if result<> nil then exit;

  if @glXGetProcAddressARB<>nil then
    result := glXGetProcAddressARB(ProcName);

  if result<> nil then exit;

  result := GetProcAddress(GLHandle, ProcName);
end;
{$ENDIF}
function GLLibGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := GetProcAddress(GLHandle, ProcName);
end;

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError : LongWord;
	Count : Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
      if not vIgnoreOpenGLErrors then
   		raise EOpenGLError.Create(String(gluErrorString(GLError)));
	end;
end;

// ClearGLError
//
procedure ClearGLError;
var
   n : Integer;
begin
   n:=0;
   while (glGetError<>GL_NO_ERROR) and (n<6) do Inc(n);
end;

// RaiseOpenGLError
//
procedure RaiseOpenGLError(const msg : String);
begin
   raise EOpenGLError.Create(msg);
end;

// ************** Extensions ********************

// ReadExtensions
//
procedure ReadExtensions;
   // To be used in an active rendering context only!
begin

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.2'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.2 core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor := GLGetProcAddress('glBlendColor');

   //promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation := GLGetProcAddress('glBlendEquation');

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements := GLGetProcAddress('glDrawRangeElements');

   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable := GLGetProcAddress('glColorTable');
   glColorTableParameterfv := GLGetProcAddress('glColorTableParameterfv');
   glColorTableParameteriv := GLGetProcAddress('glColorTableParameteriv');
   glCopyColorTable := GLGetProcAddress('glCopyColorTable');
   glGetColorTable := GLGetProcAddress('glGetColorTable');
   glGetColorTableParameterfv := GLGetProcAddress('glGetColorTableParameterfv');
   glGetColorTableParameteriv := GLGetProcAddress('glGetColorTableParameteriv');

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable := GLGetProcAddress('glColorSubTable');
   glCopyColorSubTable := GLGetProcAddress('glCopyColorSubTable');

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D := GLGetProcAddress('glConvolutionFilter1D');
   glConvolutionFilter2D := GLGetProcAddress('glConvolutionFilter2D'); 
   glConvolutionParameterf := GLGetProcAddress('glConvolutionParameterf');
   glConvolutionParameterfv := GLGetProcAddress('glConvolutionParameterfv');
   glConvolutionParameteri := GLGetProcAddress('glConvolutionParameteri'); 
   glConvolutionParameteriv := GLGetProcAddress('glConvolutionParameteriv');
   glCopyConvolutionFilter1D := GLGetProcAddress('glCopyConvolutionFilter1D');
   glCopyConvolutionFilter2D := GLGetProcAddress('glCopyConvolutionFilter2D');
   glGetConvolutionFilter := GLGetProcAddress('glGetConvolutionFilter');
   glGetConvolutionParameterfv := GLGetProcAddress('glGetConvolutionParameterfv');
   glGetConvolutionParameteriv := GLGetProcAddress('glGetConvolutionParameteriv');
   glGetSeparableFilter := GLGetProcAddress('glGetSeparableFilter');
   glSeparableFilter2D := GLGetProcAddress('glSeparableFilter2D');

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram := GLGetProcAddress('glGetHistogram');
   glGetHistogramParameterfv := GLGetProcAddress('glGetHistogramParameterfv');
   glGetHistogramParameteriv := GLGetProcAddress('glGetHistogramParameteriv');
   glGetMinmax := GLGetProcAddress('glGetMinmax');
   glGetMinmaxParameterfv := GLGetProcAddress('glGetMinmaxParameterfv');
   glGetMinmaxParameteriv := GLGetProcAddress('glGetMinmaxParameteriv');
   glHistogram := GLGetProcAddress('glHistogram');
   glMinmax := GLGetProcAddress('glMinmax');
   glResetHistogram := GLGetProcAddress('glResetHistogram');
   glResetMinmax := GLGetProcAddress('glResetMinmax');

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D := GLGetProcAddress('glTexImage3D');
   glTexSubImage3D := GLGetProcAddress('glTexSubImage3D');

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D := GLGetProcAddress('glCopyTexSubImage3D');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.3 core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture := GLGetProcAddress('glActiveTexture');
   glClientActiveTexture := GLGetProcAddress('glClientActiveTexture');
   glMultiTexCoord1d := GLGetProcAddress('glMultiTexCoord1d');
   glMultiTexCoord1dV := GLGetProcAddress('glMultiTexCoord1dV');
   glMultiTexCoord1f := GLGetProcAddress('glMultiTexCoord1f');
   glMultiTexCoord1fV := GLGetProcAddress('glMultiTexCoord1fV');
   glMultiTexCoord1i := GLGetProcAddress('glMultiTexCoord1i');
   glMultiTexCoord1iV := GLGetProcAddress('glMultiTexCoord1iV');
   glMultiTexCoord1s := GLGetProcAddress('glMultiTexCoord1s');
   glMultiTexCoord1sV := GLGetProcAddress('glMultiTexCoord1sV'); 
   glMultiTexCoord2d := GLGetProcAddress('glMultiTexCoord2d');
   glMultiTexCoord2dv := GLGetProcAddress('glMultiTexCoord2dv');
   glMultiTexCoord2f := GLGetProcAddress('glMultiTexCoord2f');
   glMultiTexCoord2fv := GLGetProcAddress('glMultiTexCoord2fv');
   glMultiTexCoord2i := GLGetProcAddress('glMultiTexCoord2i');
   glMultiTexCoord2iv := GLGetProcAddress('glMultiTexCoord2iv');
   glMultiTexCoord2s := GLGetProcAddress('glMultiTexCoord2s'); 
   glMultiTexCoord2sv := GLGetProcAddress('glMultiTexCoord2sv');
   glMultiTexCoord3d := GLGetProcAddress('glMultiTexCoord3d');
   glMultiTexCoord3dv := GLGetProcAddress('glMultiTexCoord3dv'); 
   glMultiTexCoord3f := GLGetProcAddress('glMultiTexCoord3f');
   glMultiTexCoord3fv := GLGetProcAddress('glMultiTexCoord3fv');
   glMultiTexCoord3i := GLGetProcAddress('glMultiTexCoord3i');
   glMultiTexCoord3iv := GLGetProcAddress('glMultiTexCoord3iv'); 
   glMultiTexCoord3s := GLGetProcAddress('glMultiTexCoord3s'); 
   glMultiTexCoord3sv := GLGetProcAddress('glMultiTexCoord3sv');
   glMultiTexCoord4d := GLGetProcAddress('glMultiTexCoord4d'); 
   glMultiTexCoord4dv := GLGetProcAddress('glMultiTexCoord4dv');
   glMultiTexCoord4f := GLGetProcAddress('glMultiTexCoord4f');
   glMultiTexCoord4fv := GLGetProcAddress('glMultiTexCoord4fv');
   glMultiTexCoord4i := GLGetProcAddress('glMultiTexCoord4i');
   glMultiTexCoord4iv := GLGetProcAddress('glMultiTexCoord4iv');
   glMultiTexCoord4s := GLGetProcAddress('glMultiTexCoord4s');
   glMultiTexCoord4sv := GLGetProcAddress('glMultiTexCoord4sv');

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf := GLGetProcAddress('glLoadTransposeMatrixf');
   glLoadTransposeMatrixd := GLGetProcAddress('glLoadTransposeMatrixd');
   glMultTransposeMatrixf := GLGetProcAddress('glMultTransposeMatrixf');
   glMultTransposeMatrixd := GLGetProcAddress('glMultTransposeMatrixd');

   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage := GLGetProcAddress('glSampleCoverage');

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D := GLGetProcAddress('glCompressedTexImage3D');
   glCompressedTexImage2D := GLGetProcAddress('glCompressedTexImage2D');
   glCompressedTexImage1D := GLGetProcAddress('glCompressedTexImage1D');
   glCompressedTexSubImage3D := GLGetProcAddress('glCompressedTexSubImage3D');
   glCompressedTexSubImage2D := GLGetProcAddress('glCompressedTexSubImage2D');
   glCompressedTexSubImage1D := GLGetProcAddress('glCompressedTexSubImage1D');
   glGetCompressedTexImage := GLGetProcAddress('glGetCompressedTexImage');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.4 core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate := GLGetProcAddress('glBlendFuncSeparate');

   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf := GLGetProcAddress('glFogCoordf');
   glFogCoordfv := GLGetProcAddress('glFogCoordfv');
   glFogCoordd := GLGetProcAddress('glFogCoordd');
   glFogCoorddv := GLGetProcAddress('glFogCoorddv');
   glFogCoordPointer := GLGetProcAddress('glFogCoordPointer');

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays := GLGetProcAddress('glMultiDrawArrays');
   glMultiDrawElements := GLGetProcAddress('glMultiDrawElements');

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf := GLGetProcAddress('glPointParameterf');
   glPointParameterfv := GLGetProcAddress('glPointParameterfv');
   glPointParameteri := GLGetProcAddress('glPointParameteri');
   glPointParameteriv := GLGetProcAddress('glPointParameteriv');

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b := GLGetProcAddress('glSecondaryColor3b');
   glSecondaryColor3bv := GLGetProcAddress('glSecondaryColor3bv');
   glSecondaryColor3d := GLGetProcAddress('glSecondaryColor3d');
   glSecondaryColor3dv := GLGetProcAddress('glSecondaryColor3dv');
   glSecondaryColor3f := GLGetProcAddress('glSecondaryColor3f');
   glSecondaryColor3fv := GLGetProcAddress('glSecondaryColor3fv');
   glSecondaryColor3i := GLGetProcAddress('glSecondaryColor3i');
   glSecondaryColor3iv := GLGetProcAddress('glSecondaryColor3iv');
   glSecondaryColor3s := GLGetProcAddress('glSecondaryColor3s');
   glSecondaryColor3sv := GLGetProcAddress('glSecondaryColor3sv');
   glSecondaryColor3ub := GLGetProcAddress('glSecondaryColor3ub');
   glSecondaryColor3ubv := GLGetProcAddress('glSecondaryColor3ubv');
   glSecondaryColor3ui := GLGetProcAddress('glSecondaryColor3ui');
   glSecondaryColor3uiv := GLGetProcAddress('glSecondaryColor3uiv');
   glSecondaryColor3us := GLGetProcAddress('glSecondaryColor3us');
   glSecondaryColor3usv := GLGetProcAddress('glSecondaryColor3usv');
   glSecondaryColorPointer := GLGetProcAddress('glSecondaryColorPointer');

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d := GLGetProcAddress('glWindowPos2d');
   glWindowPos2dv := GLGetProcAddress('glWindowPos2dv');
   glWindowPos2f := GLGetProcAddress('glWindowPos2f');
   glWindowPos2fv := GLGetProcAddress('glWindowPos2fv');
   glWindowPos2i := GLGetProcAddress('glWindowPos2i');
   glWindowPos2iv := GLGetProcAddress('glWindowPos2iv');
   glWindowPos2s := GLGetProcAddress('glWindowPos2s');
   glWindowPos2sv := GLGetProcAddress('glWindowPos2sv');
   glWindowPos3d := GLGetProcAddress('glWindowPos3d');
   glWindowPos3dv := GLGetProcAddress('glWindowPos3dv');
   glWindowPos3f := GLGetProcAddress('glWindowPos3f');
   glWindowPos3fv := GLGetProcAddress('glWindowPos3fv');
   glWindowPos3i := GLGetProcAddress('glWindowPos3i');
   glWindowPos3iv := GLGetProcAddress('glWindowPos3iv');
   glWindowPos3s := GLGetProcAddress('glWindowPos3s');
   glWindowPos3sv := GLGetProcAddress('glWindowPos3sv');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.5 core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries := GLGetProcAddress('glGenQueries');
   glDeleteQueries := GLGetProcAddress('glDeleteQueries');
   glIsQuery := GLGetProcAddress('glIsQuery');
   glBeginQuery := GLGetProcAddress('glBeginQuery');
   glEndQuery := GLGetProcAddress('glEndQuery');
   glGetQueryiv := GLGetProcAddress('glGetQueryiv');
   glGetQueryObjectiv := GLGetProcAddress('glGetQueryObjectiv');
   glGetQueryObjectuiv := GLGetProcAddress('glGetQueryObjectuiv');


   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer := GLGetProcAddress('glBindBuffer');
   glDeleteBuffers := GLGetProcAddress('glDeleteBuffers');
   glGenBuffers := GLGetProcAddress('glGenBuffers');
   glIsBuffer := GLGetProcAddress('glIsBuffer');
   glBufferData := GLGetProcAddress('glBufferData');
   glBufferSubData := GLGetProcAddress('glBufferSubData');
   glGetBufferSubData := GLGetProcAddress('glGetBufferSubData');
   glMapBuffer := GLGetProcAddress('glMapBuffer');
   glUnmapBuffer := GLGetProcAddress('glUnmapBuffer');
   glGetBufferParameteriv := GLGetProcAddress('glGetBufferParameteriv');
   glGetBufferPointerv := GLGetProcAddress('glGetBufferPointerv');

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.0 core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate := GLGetProcAddress('glBlendEquationSeparate');

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers := GLGetProcAddress('glDrawBuffers');

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate := GLGetProcAddress('glStencilOpSeparate');
   glStencilFuncSeparate := GLGetProcAddress('glStencilFuncSeparate');
   glStencilMaskSeparate := GLGetProcAddress('glStencilMaskSeparate');

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader := GLGetProcAddress('glAttachShader');
   glBindAttribLocation := GLGetProcAddress('glBindAttribLocation');
   glCompileShader := GLGetProcAddress('glCompileShader');
   glCreateProgram := GLGetProcAddress('glCreateProgram');
   glCreateShader := GLGetProcAddress('glCreateShader');
   glDeleteProgram := GLGetProcAddress('glDeleteProgram');
   glDeleteShader := GLGetProcAddress('glDeleteShader');
   glDetachShader := GLGetProcAddress('glDetachShader');
   glDisableVertexAttribArray := GLGetProcAddress('glDisableVertexAttribArray');
   glEnableVertexAttribArray := GLGetProcAddress('glEnableVertexAttribArray');
   glGetActiveAttrib := GLGetProcAddress('glGetActiveAttrib');
   glGetActiveUniform := GLGetProcAddress('glGetActiveUniform');
   glGetAttachedShaders := GLGetProcAddress('glGetAttachedShaders');
   glGetAttribLocation := GLGetProcAddress('glGetAttribLocation');
   glGetProgramiv := GLGetProcAddress('glGetProgramiv');
   glGetProgramInfoLog := GLGetProcAddress('glGetProgramInfoLog');
   glGetShaderiv := GLGetProcAddress('glGetShaderiv');
   glGetShaderInfoLog := GLGetProcAddress('glGetShaderInfoLog');
   glGetShaderSource := GLGetProcAddress('glGetShaderSource');
   glGetUniformLocation := GLGetProcAddress('glGetUniformLocation');
   glGetUniformfv := GLGetProcAddress('glGetUniformfv');
   glGetUniformiv := GLGetProcAddress('glGetUniformiv');
   glGetVertexAttribdv := GLGetProcAddress('glGetVertexAttribdv');
   glGetVertexAttribfv := GLGetProcAddress('glGetVertexAttribfv');
   glGetVertexAttribiv := GLGetProcAddress('glGetVertexAttribiv');
   glGetVertexAttribPointerv := GLGetProcAddress('glGetVertexAttribPointerv');
   glIsProgram := GLGetProcAddress('glIsProgram');
   glIsShader := GLGetProcAddress('glIsShader');
   glLinkProgram := GLGetProcAddress('glLinkProgram');
   glShaderSource := GLGetProcAddress('glShaderSource');
   glUseProgram := GLGetProcAddress('glUseProgram');
   glUniform1f := GLGetProcAddress('glUniform1f');
   glUniform2f := GLGetProcAddress('glUniform2f');
   glUniform3f := GLGetProcAddress('glUniform3f');
   glUniform4f := GLGetProcAddress('glUniform4f');
   glUniform1i := GLGetProcAddress('glUniform1i');
   glUniform2i := GLGetProcAddress('glUniform2i');
   glUniform3i := GLGetProcAddress('glUniform3i');
   glUniform4i := GLGetProcAddress('glUniform4i');
   glUniform1fv := GLGetProcAddress('glUniform1fv');
   glUniform2fv := GLGetProcAddress('glUniform2fv');
   glUniform3fv := GLGetProcAddress('glUniform3fv');
   glUniform4fv := GLGetProcAddress('glUniform4fv');
   glUniform1iv := GLGetProcAddress('glUniform1iv');
   glUniform2iv := GLGetProcAddress('glUniform2iv');
   glUniform3iv := GLGetProcAddress('glUniform3iv');
   glUniform4iv := GLGetProcAddress('glUniform4iv');
   glUniformMatrix2fv := GLGetProcAddress('glUniformMatrix2fv');
   glUniformMatrix3fv := GLGetProcAddress('glUniformMatrix3fv');
   glUniformMatrix4fv := GLGetProcAddress('glUniformMatrix4fv');
   glValidateProgram := GLGetProcAddress('glValidateProgram');
   glVertexAttrib1d := GLGetProcAddress('glVertexAttrib1d');
   glVertexAttrib1dv := GLGetProcAddress('glVertexAttrib1dv');
   glVertexAttrib1f := GLGetProcAddress('glVertexAttrib1f');
   glVertexAttrib1fv := GLGetProcAddress('glVertexAttrib1fv');
   glVertexAttrib1s := GLGetProcAddress('glVertexAttrib1s');
   glVertexAttrib1sv := GLGetProcAddress('glVertexAttrib1sv');
   glVertexAttrib2d := GLGetProcAddress('glVertexAttrib2d');
   glVertexAttrib2dv := GLGetProcAddress('glVertexAttrib2dv');
   glVertexAttrib2f := GLGetProcAddress('glVertexAttrib2f');
   glVertexAttrib2fv := GLGetProcAddress('glVertexAttrib2fv');
   glVertexAttrib2s := GLGetProcAddress('glVertexAttrib2s');
   glVertexAttrib2sv := GLGetProcAddress('glVertexAttrib2sv');
   glVertexAttrib3d := GLGetProcAddress('glVertexAttrib3d');
   glVertexAttrib3dv := GLGetProcAddress('glVertexAttrib3dv');
   glVertexAttrib3f := GLGetProcAddress('glVertexAttrib3f');
   glVertexAttrib3fv := GLGetProcAddress('glVertexAttrib3fv');
   glVertexAttrib3s := GLGetProcAddress('glVertexAttrib3s');
   glVertexAttrib3sv := GLGetProcAddress('glVertexAttrib3sv');
   glVertexAttrib4Nbv := GLGetProcAddress('glVertexAttrib4Nbv');
   glVertexAttrib4Niv := GLGetProcAddress('glVertexAttrib4Niv');
   glVertexAttrib4Nsv := GLGetProcAddress('glVertexAttrib4Nsv');
   glVertexAttrib4Nub := GLGetProcAddress('glVertexAttrib4Nub');
   glVertexAttrib4Nubv := GLGetProcAddress('glVertexAttrib4Nubv');
   glVertexAttrib4Nuiv := GLGetProcAddress('glVertexAttrib4Nuiv');
   glVertexAttrib4Nusv := GLGetProcAddress('glVertexAttrib4Nusv');
   glVertexAttrib4bv := GLGetProcAddress('glVertexAttrib4bv');
   glVertexAttrib4d := GLGetProcAddress('glVertexAttrib4d');
   glVertexAttrib4dv := GLGetProcAddress('glVertexAttrib4dv');
   glVertexAttrib4f := GLGetProcAddress('glVertexAttrib4f');
   glVertexAttrib4fv := GLGetProcAddress('glVertexAttrib4fv');
   glVertexAttrib4iv := GLGetProcAddress('glVertexAttrib4iv');
   glVertexAttrib4s := GLGetProcAddress('glVertexAttrib4s');
   glVertexAttrib4sv := GLGetProcAddress('glVertexAttrib4sv');
   glVertexAttrib4ubv := GLGetProcAddress('glVertexAttrib4ubv');
   glVertexAttrib4uiv := GLGetProcAddress('glVertexAttrib4uiv');
   glVertexAttrib4usv := GLGetProcAddress('glVertexAttrib4usv');
   glVertexAttribPointer := GLGetProcAddress('glVertexAttribPointer');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.1 core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)
   
   // promoted to core v2.1 from GL_EXT_texture_sRGB
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv := GLGetProcAddress('glUniformMatrix2x3fv');
   glUniformMatrix3x2fv := GLGetProcAddress('glUniformMatrix3x2fv');
   glUniformMatrix2x4fv := GLGetProcAddress('glUniformMatrix2x4fv');
   glUniformMatrix4x2fv := GLGetProcAddress('glUniformMatrix4x2fv');
   glUniformMatrix3x4fv := GLGetProcAddress('glUniformMatrix3x4fv');
   glUniformMatrix4x3fv := GLGetProcAddress('glUniformMatrix4x3fv');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.0 core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i := GLGetProcAddress('glVertexAttribI1i');
   glVertexAttribI2i := GLGetProcAddress('glVertexAttribI2i');
   glVertexAttribI3i := GLGetProcAddress('glVertexAttribI3i');
   glVertexAttribI4i := GLGetProcAddress('glVertexAttribI4i');
   glVertexAttribI1ui := GLGetProcAddress('glVertexAttribI1ui');
   glVertexAttribI2ui := GLGetProcAddress('glVertexAttribI2ui');
   glVertexAttribI3ui := GLGetProcAddress('glVertexAttribI3ui');
   glVertexAttribI4ui := GLGetProcAddress('glVertexAttribI4ui');
   glVertexAttribI1iv := GLGetProcAddress('glVertexAttribI1iv');
   glVertexAttribI2iv := GLGetProcAddress('glVertexAttribI2iv');
   glVertexAttribI3iv := GLGetProcAddress('glVertexAttribI3iv');
   glVertexAttribI4iv := GLGetProcAddress('glVertexAttribI4iv');
   glVertexAttribI1uiv := GLGetProcAddress('glVertexAttribI1uiv');
   glVertexAttribI2uiv := GLGetProcAddress('glVertexAttribI2uiv');
   glVertexAttribI3uiv := GLGetProcAddress('glVertexAttribI3uiv');
   glVertexAttribI4uiv := GLGetProcAddress('glVertexAttribI4uiv');
   glVertexAttribI4bv := GLGetProcAddress('glVertexAttribI4bv');
   glVertexAttribI4sv := GLGetProcAddress('glVertexAttribI4sv');
   glVertexAttribI4ubv := GLGetProcAddress('glVertexAttribI4ubv');
   glVertexAttribI4usv := GLGetProcAddress('glVertexAttribI4usv');
   glVertexAttribIPointer := GLGetProcAddress('glVertexAttribIPointer');
   glGetVertexAttribIiv := GLGetProcAddress('glGetVertexAttribIiv');
   glGetVertexAttribIuiv := GLGetProcAddress('glGetVertexAttribIuiv');
   glUniform1ui := GLGetProcAddress('glUniform1ui');
   glUniform2ui :=  GLGetProcAddress('glUniform2ui');
   glUniform3ui := GLGetProcAddress('glUniform3ui');
   glUniform4ui := GLGetProcAddress('glUniform4ui');
   glUniform1uiv := GLGetProcAddress('glUniform1uiv');
   glUniform2uiv := GLGetProcAddress('glUniform2uiv');
   glUniform3uiv := GLGetProcAddress('glUniform3uiv');
   glUniform4uiv := GLGetProcAddress('glUniform4uiv');
   glGetUniformuiv := GLGetProcAddress('glGetUniformuiv');
   glBindFragDataLocation := GLGetProcAddress('glBindFragDataLocation');
   glGetFragDataLocation := GLGetProcAddress('glGetFragDataLocation');

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender := GLGetProcAddress('glBeginConditionalRender');
   glEndConditionalRender := GLGetProcAddress('glEndConditionalRender');
   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor := GLGetProcAddress('glClampColor');
   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi := GLGetProcAddress('glClearColorIi');
   //glClearColorIui := GLGetProcAddress('glClearColorIui');
   glTexParameterIiv := GLGetProcAddress('glTexParameterIiv');
   glTexParameterIuiv := GLGetProcAddress('glTexParameterIuiv');
   glGetTexParameterIiv := GLGetProcAddress('glGetTexParameterIiv');
   glGetTexParameterIuiv := GLGetProcAddress('glGetTexParameterIuiv');

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski := GLGetProcAddress('glColorMaski');
   glGetBooleani_v := GLGetProcAddress('glGetBooleani_v');
   glGetIntegeri_v := GLGetProcAddress('glGetIntegeri_v');
   glEnablei := GLGetProcAddress('glEnablei');
   glDisablei := GLGetProcAddress('glDisablei');
   glIsEnabledi := GLGetProcAddress('glIsEnabledi');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRange := GLGetProcAddress('glBindBufferRange');
   glBindBufferBase := GLGetProcAddress('glBindBufferBase');
   glBeginTransformFeedback := GLGetProcAddress('glBeginTransformFeedback');
   glEndTransformFeedback := GLGetProcAddress('glEndTransformFeedback');
   glTransformFeedbackVaryings := GLGetProcAddress('glTransformFeedbackVaryings');
   glGetTransformFeedbackVarying := GLGetProcAddress('glGetTransformFeedbackVarying');

   // New commands in OpenGL 3.0
   glClearBufferiv := GLGetProcAddress('glClearBufferiv');
   glClearBufferuiv := GLGetProcAddress('glClearBufferuiv');
   glClearBufferfv := GLGetProcAddress('glClearBufferfv');
   glClearBufferfi := GLGetProcAddress('glClearBufferfi');
   glGetStringi := GLGetProcAddress('glGetStringi');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 3.1'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.1 core
   //  ###########################################################

   glDrawArraysInstanced := GLGetProcAddress('glDrawArraysInstanced');
   glDrawElementsInstanced := GLGetProcAddress('glDrawElementsInstanced');
   glTexBuffer := GLGetProcAddress('glTexBuffer');
   glPrimitiveRestartIndex := GLGetProcAddress('glPrimitiveRestartIndex');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 3.2'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.2 core
   //  ###########################################################

   glGetInteger64i_v := GLGetProcAddress('glGetInteger64i_v');
   glGetBufferParameteri64v := GLGetProcAddress('glGetBufferParameteri64v');
   glProgramParameteri := GLGetProcAddress('glProgramParameteri');
   glFramebufferTexture := GLGetProcAddress('glFramebufferTexture');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                     GLU extensions
   //  ###########################################################

   gluNurbsCallbackDataEXT := GLGetProcAddress('gluNurbsCallbackDataEXT');
   gluNewNurbsTessellatorEXT := GLGetProcAddress('gluNewNurbsTessellatorEXT');
   gluDeleteNurbsTessellatorEXT := GLGetProcAddress('gluDeleteNurbsTessellatorEXT');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                  ARB approved extensions
   //  ###########################################################

   // GL_ARB_multitexture (#1)
   glActiveTextureARB := GLGetProcAddress('glActiveTextureARB');
   glClientActiveTextureARB := GLGetProcAddress('glClientActiveTextureARB');
   glMultiTexCoord1dARB := GLGetProcAddress('glMultiTexCoord1dARB');
   glMultiTexCoord1dVARB := GLGetProcAddress('glMultiTexCoord1dVARB');
   glMultiTexCoord1fARB := GLGetProcAddress('glMultiTexCoord1fARB');
   glMultiTexCoord1fVARB := GLGetProcAddress('glMultiTexCoord1fVARB');
   glMultiTexCoord1iARB := GLGetProcAddress('glMultiTexCoord1iARB');
   glMultiTexCoord1iVARB := GLGetProcAddress('glMultiTexCoord1iVARB');
   glMultiTexCoord1sARB := GLGetProcAddress('glMultiTexCoord1sARB');
   glMultiTexCoord1sVARB := GLGetProcAddress('glMultiTexCoord1sVARB');
   glMultiTexCoord2dARB := GLGetProcAddress('glMultiTexCoord2dARB');
   glMultiTexCoord2dvARB := GLGetProcAddress('glMultiTexCoord2dvARB');
   glMultiTexCoord2fARB := GLGetProcAddress('glMultiTexCoord2fARB');
   glMultiTexCoord2fvARB := GLGetProcAddress('glMultiTexCoord2fvARB');
   glMultiTexCoord2iARB := GLGetProcAddress('glMultiTexCoord2iARB');
   glMultiTexCoord2ivARB := GLGetProcAddress('glMultiTexCoord2ivARB');
   glMultiTexCoord2sARB := GLGetProcAddress('glMultiTexCoord2sARB');
   glMultiTexCoord2svARB := GLGetProcAddress('glMultiTexCoord2svARB');
   glMultiTexCoord3dARB := GLGetProcAddress('glMultiTexCoord3dARB');
   glMultiTexCoord3dvARB := GLGetProcAddress('glMultiTexCoord3dvARB');
   glMultiTexCoord3fARB := GLGetProcAddress('glMultiTexCoord3fARB');
   glMultiTexCoord3fvARB := GLGetProcAddress('glMultiTexCoord3fvARB');
   glMultiTexCoord3iARB := GLGetProcAddress('glMultiTexCoord3iARB');
   glMultiTexCoord3ivARB := GLGetProcAddress('glMultiTexCoord3ivARB');
   glMultiTexCoord3sARB := GLGetProcAddress('glMultiTexCoord3sARB');
   glMultiTexCoord3svARB := GLGetProcAddress('glMultiTexCoord3svARB');
   glMultiTexCoord4dARB := GLGetProcAddress('glMultiTexCoord4dARB');
   glMultiTexCoord4dvARB := GLGetProcAddress('glMultiTexCoord4dvARB');
   glMultiTexCoord4fARB := GLGetProcAddress('glMultiTexCoord4fARB');
   glMultiTexCoord4fvARB := GLGetProcAddress('glMultiTexCoord4fvARB');
   glMultiTexCoord4iARB := GLGetProcAddress('glMultiTexCoord4iARB');
   glMultiTexCoord4ivARB := GLGetProcAddress('glMultiTexCoord4ivARB');
   glMultiTexCoord4sARB := GLGetProcAddress('glMultiTexCoord4sARB');
   glMultiTexCoord4svARB := GLGetProcAddress('glMultiTexCoord4svARB');

   // GL_ARB_transpose_matrix (#3)
   glLoadTransposeMatrixfARB := GLGetProcAddress('glLoadTransposeMatrixfARB');
   glLoadTransposeMatrixdARB := GLGetProcAddress('glLoadTransposeMatrixdARB');
   glMultTransposeMatrixfARB := GLGetProcAddress('glMultTransposeMatrixfARB');
   glMultTransposeMatrixdARB := GLGetProcAddress('glMultTransposeMatrixdARB');

   // GL_ARB_multisample (#5)
   glSampleCoverageARB := GLGetProcAddress('glSampleCoverageARB');

   // GL_ARB_texture_compression (#12)
   glCompressedTexImage3DARB := GLGetProcAddress('glCompressedTexImage3DARB');
   glCompressedTexImage2DARB := GLGetProcAddress('glCompressedTexImage2DARB');
   glCompressedTexImage1DARB := GLGetProcAddress('glCompressedTexImage1DARB');
   glCompressedTexSubImage3DARB := GLGetProcAddress('glCompressedTexSubImage3DARB');
   glCompressedTexSubImage2DARB := GLGetProcAddress('glCompressedTexSubImage2DARB');
   glCompressedTexSubImage1DARB := GLGetProcAddress('glCompressedTexSubImage1DARB');
   glGetCompressedTexImageARB := GLGetProcAddress('glGetCompressedTexImageARB');

   // GL_ARB_point_parameter (#14)
   glPointParameterfARB := GLGetProcAddress('glPointParameterfARB');
   glPointParameterfvARB := GLGetProcAddress('glPointParameterfvARB');

   // GL_ARB_vertex_blend (#15) {deprecated?}
   glWeightbvARB := GLGetProcAddress('glWeightbvARB');
   glWeightsvARB := GLGetProcAddress('glWeightsvARB');
   glWeightivARB := GLGetProcAddress('glWeightivARB');
   glWeightfvARB := GLGetProcAddress('glWeightfvARB');
   glWeightdvARB := GLGetProcAddress('glWeightdvARB');
   glWeightubvARB := GLGetProcAddress('glWeightubvARB');
   glWeightusvARB := GLGetProcAddress('glWeightusvARB');
   glWeightuivARB := GLGetProcAddress('glWeightuivARB');
   glWeightPointerARB := GLGetProcAddress('glWeightPointerARB');
   glVertexBlendARB := GLGetProcAddress('glVertexBlendARB');

   // GL_ARB_matrix_palette (#16) {deprecated?}
   glCurrentPaletteMatrixARB := GLGetProcAddress('glCurrentPaletteMatrixARB');
   glMatrixIndexubvARB := GLGetProcAddress('glMatrixIndexubvARB');
   glMatrixIndexusvARB := GLGetProcAddress('glMatrixIndexusvARB');
   glMatrixIndexuivARB := GLGetProcAddress('glMatrixIndexuivARB');
   glMatrixIndexPointerARB := GLGetProcAddress('glMatrixIndexPointerARB');

   // GL_ARB_window_pos (#25)
   glWindowPos2dARB := GLGetProcAddress('glWindowPos2dARB');
   glWindowPos2dvARB := GLGetProcAddress('glWindowPos2dvARB');
   glWindowPos2fARB := GLGetProcAddress('glWindowPos2fARB');
   glWindowPos2fvARB := GLGetProcAddress('glWindowPos2fvARB');
   glWindowPos2iARB := GLGetProcAddress('glWindowPos2iARB');
   glWindowPos2ivARB := GLGetProcAddress('glWindowPos2ivARB');
   glWindowPos2sARB := GLGetProcAddress('glWindowPos2sARB');
   glWindowPos2svARB := GLGetProcAddress('glWindowPos2svARB');
   glWindowPos3dARB := GLGetProcAddress('glWindowPos3dARB');
   glWindowPos3dvARB := GLGetProcAddress('glWindowPos3dvARB');
   glWindowPos3fARB := GLGetProcAddress('glWindowPos3fARB');
   glWindowPos3fvARB := GLGetProcAddress('glWindowPos3fvARB');
   glWindowPos3iARB := GLGetProcAddress('glWindowPos3iARB');
   glWindowPos3ivARB := GLGetProcAddress('glWindowPos3ivARB');
   glWindowPos3sARB := GLGetProcAddress('glWindowPos3sARB');
   glWindowPos3svARB := GLGetProcAddress('glWindowPos3svARB');

   // GL_ARB_vertex_program (#26)
   glVertexAttrib1dARB := GLGetProcAddress('glVertexAttrib1dARB');
   glVertexAttrib1dvARB := GLGetProcAddress('glVertexAttrib1dvARB');
   glVertexAttrib1fARB := GLGetProcAddress('glVertexAttrib1fARB');
   glVertexAttrib1fvARB := GLGetProcAddress('glVertexAttrib1fvARB');
   glVertexAttrib1sARB := GLGetProcAddress('glVertexAttrib1sARB');
   glVertexAttrib1svARB := GLGetProcAddress('glVertexAttrib1svARB');
   glVertexAttrib2dARB := GLGetProcAddress('glVertexAttrib2dARB');
   glVertexAttrib2dvARB := GLGetProcAddress('glVertexAttrib2dvARB');
   glVertexAttrib2fARB := GLGetProcAddress('glVertexAttrib2fARB');
   glVertexAttrib2fvARB := GLGetProcAddress('glVertexAttrib2fvARB');
   glVertexAttrib2sARB := GLGetProcAddress('glVertexAttrib2sARB');
   glVertexAttrib2svARB := GLGetProcAddress('glVertexAttrib2svARB');
   glVertexAttrib3dARB := GLGetProcAddress('glVertexAttrib3dARB');
   glVertexAttrib3dvARB := GLGetProcAddress('glVertexAttrib3dvARB');
   glVertexAttrib3fARB := GLGetProcAddress('glVertexAttrib3fARB');
   glVertexAttrib3fvARB := GLGetProcAddress('glVertexAttrib3fvARB');
   glVertexAttrib3sARB := GLGetProcAddress('glVertexAttrib3sARB');
   glVertexAttrib3svARB := GLGetProcAddress('glVertexAttrib3svARB');
   glVertexAttrib4NbvARB := GLGetProcAddress('glVertexAttrib4NbvARB');
   glVertexAttrib4NivARB := GLGetProcAddress('glVertexAttrib4NivARB');
   glVertexAttrib4NsvARB := GLGetProcAddress('glVertexAttrib4NsvARB');
   glVertexAttrib4NubARB := GLGetProcAddress('glVertexAttrib4NubARB');
   glVertexAttrib4NubvARB := GLGetProcAddress('glVertexAttrib4NubvARB');
   glVertexAttrib4NuivARB := GLGetProcAddress('glVertexAttrib4NuivARB');
   glVertexAttrib4NusvARB := GLGetProcAddress('glVertexAttrib4NusvARB');
   glVertexAttrib4bvARB := GLGetProcAddress('glVertexAttrib4bvARB');
   glVertexAttrib4dARB := GLGetProcAddress('glVertexAttrib4dARB');
   glVertexAttrib4dvARB := GLGetProcAddress('glVertexAttrib4dvARB');
   glVertexAttrib4fARB := GLGetProcAddress('glVertexAttrib4fARB');
   glVertexAttrib4fvARB := GLGetProcAddress('glVertexAttrib4fvARB');
   glVertexAttrib4ivARB := GLGetProcAddress('glVertexAttrib4ivARB');
   glVertexAttrib4sARB := GLGetProcAddress('glVertexAttrib4sARB');
   glVertexAttrib4svARB := GLGetProcAddress('glVertexAttrib4svARB');
   glVertexAttrib4ubvARB := GLGetProcAddress('glVertexAttrib4ubvARB');
   glVertexAttrib4uivARB := GLGetProcAddress('glVertexAttrib4uivARB');
   glVertexAttrib4usvARB := GLGetProcAddress('glVertexAttrib4usvARB');
   glVertexAttribPointerARB := GLGetProcAddress('glVertexAttribPointerARB');
   glEnableVertexAttribArrayARB := GLGetProcAddress('glEnableVertexAttribArrayARB');
   glDisableVertexAttribArrayARB := GLGetProcAddress('glDisableVertexAttribArrayARB');
   glProgramStringARB := GLGetProcAddress('glProgramStringARB');
   glBindProgramARB := GLGetProcAddress('glBindProgramARB');
   glDeleteProgramsARB := GLGetProcAddress('glDeleteProgramsARB');
   glGenProgramsARB := GLGetProcAddress('glGenProgramsARB');
   glProgramEnvParameter4dARB := GLGetProcAddress('glProgramEnvParameter4dARB');
   glProgramEnvParameter4dvARB := GLGetProcAddress('glProgramEnvParameter4dvARB');
   glProgramEnvParameter4fARB := GLGetProcAddress('glProgramEnvParameter4fARB');
   glProgramEnvParameter4fvARB := GLGetProcAddress('glProgramEnvParameter4fvARB');
   glProgramLocalParameter4dARB := GLGetProcAddress('glProgramLocalParameter4dARB');
   glProgramLocalParameter4dvARB := GLGetProcAddress('glProgramLocalParameter4dvARB');
   glProgramLocalParameter4fARB := GLGetProcAddress('glProgramLocalParameter4fARB');
   glProgramLocalParameter4fvARB := GLGetProcAddress('glProgramLocalParameter4fvARB');
   glGetProgramEnvParameterdvARB := GLGetProcAddress('glGetProgramEnvParameterdvARB');
   glGetProgramEnvParameterfvARB := GLGetProcAddress('glGetProgramEnvParameterfvARB');
   glGetProgramLocalParameterdvARB := GLGetProcAddress('glGetProgramLocalParameterdvARB');
   glGetProgramLocalParameterfvARB := GLGetProcAddress('glGetProgramLocalParameterfvARB');
   glGetProgramivARB := GLGetProcAddress('glGetProgramivARB');
   glGetProgramStringARB := GLGetProcAddress('glGetProgramStringARB');
   glGetVertexAttribdvARB := GLGetProcAddress('glGetVertexAttribdvARB');
   glGetVertexAttribfvARB := GLGetProcAddress('glGetVertexAttribfvARB');
   glGetVertexAttribivARB := GLGetProcAddress('glGetVertexAttribivARB');
   glGetVertexAttribPointervARB := GLGetProcAddress('glGetVertexAttribPointervARB');
   glIsProgramARB := GLGetProcAddress('glIsProgramARB');

   // GL_ARB_vertex_buffer_object (#28)
   glBindBufferARB := GLGetProcAddress('glBindBufferARB');
   glDeleteBuffersARB := GLGetProcAddress('glDeleteBuffersARB');
   glGenBuffersARB := GLGetProcAddress('glGenBuffersARB');
   glIsBufferARB := GLGetProcAddress('glIsBufferARB');
   glBufferDataARB := GLGetProcAddress('glBufferDataARB');
   glBufferSubDataARB := GLGetProcAddress('glBufferSubDataARB');
   glGetBufferSubDataARB := GLGetProcAddress('glGetBufferSubDataARB');
   glMapBufferARB := GLGetProcAddress('glMapBufferARB');
   glUnmapBufferARB := GLGetProcAddress('glUnmapBufferARB');
   glGetBufferParameterivARB := GLGetProcAddress('glGetBufferParameterivARB');
   glGetBufferPointervARB := GLGetProcAddress('glGetBufferPointervARB');

   // GL_ARB_occlusion_query (#29)
   glGenQueriesARB := GLGetProcAddress('glGenQueriesARB');
   glDeleteQueriesARB := GLGetProcAddress('glDeleteQueriesARB');
   glIsQueryARB := GLGetProcAddress('glIsQueryARB');
   glBeginQueryARB := GLGetProcAddress('glBeginQueryARB');
   glEndQueryARB := GLGetProcAddress('glEndQueryARB');
   glGetQueryivARB := GLGetProcAddress('glGetQueryivARB');
   glGetQueryObjectivARB := GLGetProcAddress('glGetQueryObjectivARB');
   glGetQueryObjectuivARB := GLGetProcAddress('glGetQueryObjectuivARB');

   // GL_ARB_shader_objects (#30)
   glDeleteObjectARB := GLGetProcAddress('glDeleteObjectARB');
   glGetHandleARB := GLGetProcAddress('glGetHandleARB');
   glDetachObjectARB := GLGetProcAddress('glDetachObjectARB');
   glCreateShaderObjectARB := GLGetProcAddress('glCreateShaderObjectARB');
   glShaderSourceARB := GLGetProcAddress('glShaderSourceARB');
   glCompileShaderARB := GLGetProcAddress('glCompileShaderARB');
   glCreateProgramObjectARB := GLGetProcAddress('glCreateProgramObjectARB');
   glAttachObjectARB := GLGetProcAddress('glAttachObjectARB');
   glLinkProgramARB := GLGetProcAddress('glLinkProgramARB');
   glUseProgramObjectARB := GLGetProcAddress('glUseProgramObjectARB');
   glValidateProgramARB := GLGetProcAddress('glValidateProgramARB');
   glUniform1fARB := GLGetProcAddress('glUniform1fARB');
   glUniform2fARB := GLGetProcAddress('glUniform2fARB');
   glUniform3fARB := GLGetProcAddress('glUniform3fARB');
   glUniform4fARB := GLGetProcAddress('glUniform4fARB');
   glUniform1iARB := GLGetProcAddress('glUniform1iARB');
   glUniform2iARB := GLGetProcAddress('glUniform2iARB');
   glUniform3iARB := GLGetProcAddress('glUniform3iARB');
   glUniform4iARB := GLGetProcAddress('glUniform4iARB');
   glUniform1fvARB := GLGetProcAddress('glUniform1fvARB');
   glUniform2fvARB := GLGetProcAddress('glUniform2fvARB');
   glUniform3fvARB := GLGetProcAddress('glUniform3fvARB');
   glUniform4fvARB := GLGetProcAddress('glUniform4fvARB');
   glUniform1ivARB := GLGetProcAddress('glUniform1ivARB');
   glUniform2ivARB := GLGetProcAddress('glUniform2ivARB');
   glUniform3ivARB := GLGetProcAddress('glUniform3ivARB');
   glUniform4ivARB := GLGetProcAddress('glUniform4ivARB');
   glUniformMatrix2fvARB := GLGetProcAddress('glUniformMatrix2fvARB');
   glUniformMatrix3fvARB := GLGetProcAddress('glUniformMatrix3fvARB');
   glUniformMatrix4fvARB := GLGetProcAddress('glUniformMatrix4fvARB');
   glGetObjectParameterfvARB := GLGetProcAddress('glGetObjectParameterfvARB');
   glGetObjectParameterivARB := GLGetProcAddress('glGetObjectParameterivARB');
   glGetInfoLogARB := GLGetProcAddress('glGetInfoLogARB');
   glGetAttachedObjectsARB := GLGetProcAddress('glGetAttachedObjectsARB');
   glGetUniformLocationARB := GLGetProcAddress('glGetUniformLocationARB');
   glGetActiveUniformARB := GLGetProcAddress('glGetActiveUniformARB');
   glGetUniformfvARB := GLGetProcAddress('glGetUniformfvARB');
   glGetUniformivARB := GLGetProcAddress('glGetUniformivARB');
   glGetShaderSourceARB := GLGetProcAddress('glGetShaderSourceARB');

   // GL_ARB_vertex_shader (#31)
   glBindAttribLocationARB := GLGetProcAddress('glBindAttribLocationARB');
   glGetActiveAttribARB := GLGetProcAddress('glGetActiveAttribARB');
   glGetAttribLocationARB := GLGetProcAddress('glGetAttribLocationARB');

   // GL_ARB_draw_buffers (#37)
   glDrawBuffersARB := GLGetProcAddress('glDrawBuffersARB');

   // GL_ARB_color_buffer_float (#39)
   glClampColorARB := GLGetProcAddress('glClampColorARB');

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB := GLGetProcAddress('glDrawArraysInstancedARB');
   glDrawElementsInstancedARB := GLGetProcAddress('glDrawElementsInstancedARB');

   // GL_ARB_framebuffer_object (ARB #45)
   glIsRenderbuffer := GLGetProcAddress('glIsRenderbuffer');
   glBindRenderbuffer := GLGetProcAddress('glBindRenderbuffer');
   glDeleteRenderbuffers := GLGetProcAddress('glDeleteRenderbuffers');
   glGenRenderbuffers := GLGetProcAddress('glGenRenderbuffers');
   glRenderbufferStorage := GLGetProcAddress('glRenderbufferStorage');
   glRenderbufferStorageMultisample := GLGetProcAddress('glRenderbufferStorageMultisample');
   glGetRenderbufferParameteriv := GLGetProcAddress('glGetRenderbufferParameteriv');
   glIsFramebuffer := GLGetProcAddress('glIsFramebuffer');
   glBindFramebuffer := GLGetProcAddress('glBindFramebuffer');
   glDeleteFramebuffers := GLGetProcAddress('glDeleteFramebuffers');
   glGenFramebuffers := GLGetProcAddress('glGenFramebuffers');
   glCheckFramebufferStatus := GLGetProcAddress('glCheckFramebufferStatus');
   glFramebufferTexture1D := GLGetProcAddress('glFramebufferTexture1D');
   glFramebufferTexture2D := GLGetProcAddress('glFramebufferTexture2D');
   glFramebufferTexture3D := GLGetProcAddress('glFramebufferTexture3D');
   glFramebufferTextureLayer := GLGetProcAddress('glFramebufferTextureLayer');
   glFramebufferRenderbuffer := GLGetProcAddress('glFramebufferRenderbuffer');
   glGetFramebufferAttachmentParameteriv := GLGetProcAddress('glGetFramebufferAttachmentParameteriv');
   glBlitFramebuffer := GLGetProcAddress('glBlitFramebuffer');
   glGenerateMipmap := GLGetProcAddress('glGenerateMipmap');

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB := GLGetProcAddress('glProgramParameteriARB');
   glFramebufferTextureARB := GLGetProcAddress('glFramebufferTextureARB');
   glFramebufferTextureLayerARB := GLGetProcAddress('glFramebufferTextureLayerARB');
   glFramebufferTextureFaceARB := GLGetProcAddress('glFramebufferTextureFaceARB');

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB := GLGetProcAddress('glVertexAttribDivisorARB');

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange := GLGetProcAddress('glMapBufferRange');
   glFlushMappedBufferRange := GLGetProcAddress('glFlushMappedBufferRange');

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB := GLGetProcAddress('glTexBufferARB');

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray := GLGetProcAddress('glBindVertexArray');
   glDeleteVertexArrays := GLGetProcAddress('glDeleteVertexArrays');
   glGenVertexArrays := GLGetProcAddress('glGenVertexArrays');
   glIsVertexArray := GLGetProcAddress('glIsVertexArray');

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices := GLGetProcAddress('glGetUniformIndices');
   glGetActiveUniformsiv := GLGetProcAddress('glGetActiveUniformsiv');
   glGetActiveUniformName := GLGetProcAddress('glGetActiveUniformName');
   glGetUniformBlockIndex := GLGetProcAddress('glGetUniformBlockIndex');
   glGetActiveUniformBlockiv := GLGetProcAddress('glGetActiveUniformBlockiv');
   glGetActiveUniformBlockName := GLGetProcAddress('glGetActiveUniformBlockName');
   glUniformBlockBinding := GLGetProcAddress('glUniformBlockBinding');

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData := GLGetProcAddress('glCopyBufferSubData');

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex := GLGetProcAddress('glDrawElementsBaseVertex');
   glDrawRangeElementsBaseVertex := GLGetProcAddress('glDrawRangeElementsBaseVertex');
   glDrawElementsInstancedBaseVertex := GLGetProcAddress('glDrawElementsInstancedBaseVertex');
   glMultiDrawElementsBaseVertex := GLGetProcAddress('glMultiDrawElementsBaseVertex');

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex := GLGetProcAddress('glProvokingVertex');

   // GL_ARB_sync commands (ARB #66)
   glFenceSync := GLGetProcAddress('glFenceSync');
   glIsSync := GLGetProcAddress('glIsSync');
   glDeleteSync := GLGetProcAddress('glDeleteSync');
   glClientWaitSync := GLGetProcAddress('glClientWaitSync');
   glWaitSync := GLGetProcAddress('glWaitSync');
   glGetInteger64v := GLGetProcAddress('glGetInteger64v');
   glGetSynciv := GLGetProcAddress('glGetSynciv');

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample := GLGetProcAddress('glTexImage2DMultisample');
   glTexImage3DMultisample := GLGetProcAddress('glTexImage3DMultisample');
   glGetMultisamplefv := GLGetProcAddress('glGetMultisamplefv');
   glSampleMaski := GLGetProcAddress('glSampleMaski');

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationi := GLGetProcAddress('glBlendEquationi');
   glBlendEquationSeparatei := GLGetProcAddress('glBlendEquationSeparatei');
   glBlendFunci := GLGetProcAddress('glBlendFunci');
   glBlendFuncSeparatei := GLGetProcAddress('glBlendFuncSeparatei');

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShading := GLGetProcAddress('glMinSampleShading');

   // GL_ARB_blend_func_extended (ARB #78)
   glBindFragDataLocationIndexed := GLGetProcAddress('glBindFragDataLocationIndexed');
   glGetFragDataIndex := GLGetProcAddress('glGetFragDataIndex');

   // GL_ARB_sampler_objects (ARB #81)
   glGenSamplers := GLGetProcAddress('glGenSamplers');
   glDeleteSamplers := GLGetProcAddress('glDeleteSamplers');
   glIsSampler := GLGetProcAddress('glIsSampler');
   glBindSampler := GLGetProcAddress('glBindSampler');
   glSamplerParameteri := GLGetProcAddress('glSamplerParameteri');
   glSamplerParameteriv := GLGetProcAddress('glSamplerParameteriv');
   glSamplerParameterf := GLGetProcAddress('glSamplerParameterf');
   glSamplerParameterfv := GLGetProcAddress('glSamplerParameterfv');
   glSamplerParameterIiv := GLGetProcAddress('glSamplerParameterIiv');
   glSamplerParameterIuiv := GLGetProcAddress('glSamplerParameterIuiv');
   glGetSamplerParameteriv := GLGetProcAddress('glGetSamplerParameteriv');
   glGetSamplerParameterIiv := GLGetProcAddress('glGetSamplerParameterIiv');
   glGetSamplerParameterfv := GLGetProcAddress('glGetSamplerParameterfv');
   glGetSamplerParameterIfv := GLGetProcAddress('glGetSamplerParameterIfv');

   // GL_ARB_timer_query (ARB #85)
   glQueryCounter := GLGetProcAddress('glQueryCounter');
   glGetQueryObjecti64v := GLGetProcAddress('glGetQueryObjecti64v');
   glGetQueryObjectui64v := GLGetProcAddress('glGetQueryObjectui64v');

   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
   glVertexP2ui := GLGetProcAddress('glVertexP2ui');
   glVertexP2uiv := GLGetProcAddress('glVertexP2uiv');
   glVertexP3ui := GLGetProcAddress('glVertexP3ui');
   glVertexP3uiv := GLGetProcAddress('glVertexP3uiv');
   glVertexP4ui := GLGetProcAddress('glVertexP4ui');
   glVertexP4uiv := GLGetProcAddress('glVertexP4uiv');
   glTexCoordP1ui := GLGetProcAddress('glTexCoordP1ui');
   glTexCoordP1uiv := GLGetProcAddress('glTexCoordP1uiv');
   glTexCoordP2ui := GLGetProcAddress('glTexCoordP2ui');
   glTexCoordP2uiv := GLGetProcAddress('glTexCoordP2uiv');
   glTexCoordP3ui := GLGetProcAddress('glTexCoordP3ui');
   glTexCoordP3uiv := GLGetProcAddress('glTexCoordP3uiv');
   glTexCoordP4ui := GLGetProcAddress('glTexCoordP4ui');
   glTexCoordP4uiv := GLGetProcAddress('glTexCoordP4uiv');
   glMultiTexCoordP1ui := GLGetProcAddress('glMultiTexCoordP1ui');
   glMultiTexCoordP1uiv := GLGetProcAddress('glMultiTexCoordP1uiv');
   glMultiTexCoordP2ui := GLGetProcAddress('glMultiTexCoordP2ui');
   glMultiTexCoordP2uiv := GLGetProcAddress('glMultiTexCoordP2uiv');
   glMultiTexCoordP3ui := GLGetProcAddress('glMultiTexCoordP3ui');
   glMultiTexCoordP3uiv := GLGetProcAddress('glMultiTexCoordP3uiv');
   glMultiTexCoordP4ui := GLGetProcAddress('glMultiTexCoordP4ui');
   glMultiTexCoordP4uiv := GLGetProcAddress('glMultiTexCoordP4uiv');
   glNormalP3ui := GLGetProcAddress('glNormalP3ui');
   glNormalP3uiv := GLGetProcAddress('glNormalP3uiv');
   glColorP3ui := GLGetProcAddress('glColorP3ui');
   glColorP3uiv := GLGetProcAddress('glColorP3uiv');
   glColorP4ui := GLGetProcAddress('glColorP4ui');
   glColorP4uiv := GLGetProcAddress('glColorP4uiv');
   glSecondaryColorP3ui := GLGetProcAddress('glSecondaryColorP3ui');
   glSecondaryColorP3uiv := GLGetProcAddress('glSecondaryColorP3uiv');
   glVertexAttribP1ui := GLGetProcAddress('glVertexAttribP1ui');
   glVertexAttribP1uiv := GLGetProcAddress('glVertexAttribP1uiv');
   glVertexAttribP2ui := GLGetProcAddress('glVertexAttribP2ui');
   glVertexAttribP2uiv := GLGetProcAddress('glVertexAttribP2uiv');
   glVertexAttribP3ui := GLGetProcAddress('glVertexAttribP3ui');
   glVertexAttribP3uiv := GLGetProcAddress('glVertexAttribP3uiv');
   glVertexAttribP4ui := GLGetProcAddress('glVertexAttribP4ui');
   glVertexAttribP4uiv := GLGetProcAddress('glVertexAttribP4uiv');

   // GL_ARB_draw_indirect (ARB #87)
   glDrawArraysIndirect := GLGetProcAddress('glDrawArraysIndirect');
   glDrawElementsIndirect := GLGetProcAddress('glDrawElementsIndirect');

   // GL_ARB_gpu_shader_fp64 (ARB #89)
   glUniform1d := GLGetProcAddress('glUniform1d');
   glUniform2d := GLGetProcAddress('glUniform2d');
   glUniform3d := GLGetProcAddress('glUniform3d');
   glUniform4d := GLGetProcAddress('glUniform4d');
   glUniform1dv := GLGetProcAddress('glUniform1dv');
   glUniform2dv := GLGetProcAddress('glUniform2dv');
   glUniform3dv := GLGetProcAddress('glUniform3dv');
   glUniform4dv := GLGetProcAddress('glUniform4dv');
   glUniformMatrix2dv := GLGetProcAddress('glUniformMatrix2dv');
   glUniformMatrix3dv := GLGetProcAddress('glUniformMatrix3dv');
   glUniformMatrix4dv := GLGetProcAddress('glUniformMatrix4dv');
   glUniformMatrix2x3dv := GLGetProcAddress('glUniformMatrix2x3dv');
   glUniformMatrix2x4dv := GLGetProcAddress('glUniformMatrix2x4dv');
   glUniformMatrix3x2dv := GLGetProcAddress('glUniformMatrix3x2dv');
   glUniformMatrix3x4dv := GLGetProcAddress('glUniformMatrix3x4dv');
   glUniformMatrix4x2dv := GLGetProcAddress('glUniformMatrix4x2dv');
   glUniformMatrix4x3dv := GLGetProcAddress('glUniformMatrix4x3dv');
   glGetUniformdv := GLGetProcAddress('glGetUniformdv');
   glProgramUniform1dEXT := GLGetProcAddress('glProgramUniform1dEXT');
   glProgramUniform2dEXT := GLGetProcAddress('glProgramUniform2dEXT');
   glProgramUniform3dEXT := GLGetProcAddress('glProgramUniform3dEXT');
   glProgramUniform4dEXT := GLGetProcAddress('glProgramUniform4dEXT');
   glProgramUniform1dvEXT := GLGetProcAddress('glProgramUniform1dvEXT');
   glProgramUniform2dvEXT := GLGetProcAddress('glProgramUniform2dvEXT');
   glProgramUniform3dvEXT := GLGetProcAddress('glProgramUniform3dvEXT');
   glProgramUniform4dvEXT := GLGetProcAddress('glProgramUniform4dvEXT');
   glProgramUniformMatrix2dvEXT := GLGetProcAddress('glProgramUniformMatrix2dvEXT');
   glProgramUniformMatrix3dvEXT := GLGetProcAddress('glProgramUniformMatrix3dvEXT');
   glProgramUniformMatrix4dvEXT := GLGetProcAddress('glProgramUniformMatrix4dvEXT');
   glProgramUniformMatrix2x3dvEXT := GLGetProcAddress('glProgramUniformMatrix2x3dvEXT');
   glProgramUniformMatrix2x4dvEXT := GLGetProcAddress('glProgramUniformMatrix2x4dvEXT');
   glProgramUniformMatrix3x2dvEXT := GLGetProcAddress('glProgramUniformMatrix3x2dvEXT');
   glProgramUniformMatrix3x4dvEXT := GLGetProcAddress('glProgramUniformMatrix3x4dvEXT');
   glProgramUniformMatrix4x2dvEXT := GLGetProcAddress('glProgramUniformMatrix4x2dvEXT');
   glProgramUniformMatrix4x3dvEXT := GLGetProcAddress('glProgramUniformMatrix4x3dvEXT');

   // GL_ARB_shader_subroutine (ARB #90)
   glGetSubroutineUniformLocation := GLGetProcAddress('glGetSubroutineUniformLocation');
   glGetSubroutineIndex := GLGetProcAddress('glGetSubroutineIndex');
   glGetActiveSubroutineUniformiv := GLGetProcAddress('glGetActiveSubroutineUniformiv');
   glGetActiveSubroutineUniformName := GLGetProcAddress('glGetActiveSubroutineUniformName');
   glGetActiveSubroutineName := GLGetProcAddress('glGetActiveSubroutineName');
   glUniformSubroutinesuiv := GLGetProcAddress('glUniformSubroutinesuiv');
   glGetUniformSubroutineuiv := GLGetProcAddress('glGetUniformSubroutineuiv');
   glGetProgramStageiv := GLGetProcAddress('glGetProgramStageiv');

   // GL_ARB_tessellation_shader (ARB #91)
   glPatchParameteri := GLGetProcAddress('glPatchParameteri');
   glPatchParameterfv := GLGetProcAddress('glPatchParameterfv');

   // GL_ARB_transform_feedback2 (ARB #93)
   glBindTransformFeedback := GLGetProcAddress('glBindTransformFeedback');
   glDeleteTransformFeedbacks := GLGetProcAddress('glDeleteTransformFeedbacks');
   glGenTransformFeedbacks := GLGetProcAddress('glGenTransformFeedbacks');
   glIsTransformFeedback := GLGetProcAddress('glIsTransformFeedback');
   glPauseTransformFeedback := GLGetProcAddress('glPauseTransformFeedback');
   glResumeTransformFeedback := GLGetProcAddress('glResumeTransformFeedback');
   glDrawTransformFeedback := GLGetProcAddress('glDrawTransformFeedback');

   // GL_ARB_transform_feedback3 (ARB # 94)
   glDrawTransformFeedbackStream := GLGetProcAddress('glDrawTransformFeedbackStream');
   glBeginQueryIndexed := GLGetProcAddress('glBeginQueryIndexed');
   glEndQueryIndexed := GLGetProcAddress('glEndQueryIndexed');
   glGetQueryIndexediv := GLGetProcAddress('glGetQueryIndexediv');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // functions/procedures belonging to unknown extensions
   glSamplePassARB := GLGetProcAddress('glSamplePassARB');
   glArrayElementArrayEXT := GLGetProcAddress('glArrayElementArrayEXT');

   // WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN := GLGetProcAddress('glAddSwapHintRectWIN');

   // GL_EXT_blend_color (#2)
   glBlendColorEXT := GLGetProcAddress('glBlendColorEXT');

   // GL_EXT_polygon_offset (#3)
   glPolygonOffsetEXT := GLGetProcAddress('glPolygonOffsetEXT');

   // GL_EXT_texture3D (#6)
   glTexImage3DEXT := GLGetProcAddress('glTexImage3DEXT');

   // GL_EXT_subtexture (#9)
   glTexSubImage1dEXT := GLGetProcAddress('glTexSubImage1DEXT');
   glTexSubImage2dEXT := GLGetProcAddress('glTexSubImage2DEXT');
   glTexSubImage3dEXT := GLGetProcAddress('glTexSubImage3DEXT');

   // GL_EXT_copy_texture (#10)
   glCopyTexImage1DEXT := GLGetProcAddress('glCopyTexImage1DEXT');
   glCopyTexImage2DEXT := GLGetProcAddress('glCopyTexImage2DEXT');
   glCopyTexSubImage1DEXT := GLGetProcAddress('glCopyTexSubImage1DEXT');
   glCopyTexSubImage2DEXT := GLGetProcAddress('glCopyTexSubImage2DEXT');
   glCopyTexSubImage3DEXT := GLGetProcAddress('glCopyTexSubImage3DEXT');

   // GL_EXT_texture_object (#20)
   glGenTexturesEXT := GLGetProcAddress('glGenTexturesEXT');
   glDeleteTexturesEXT := GLGetProcAddress('glDeleteTexturesEXT');
   glBindTextureEXT := GLGetProcAddress('glBindTextureEXT');
   glPrioritizeTexturesEXT := GLGetProcAddress('glPrioritizeTexturesEXT');
   glAreTexturesResidentEXT := GLGetProcAddress('glAreTexturesResidentEXT');
   glIsTextureEXT := GLGetProcAddress('glIsTextureEXT');

   // GL_SGIS_multisample (#25)
   glSampleMaskSGIS := GLGetProcAddress('glSampleMaskSGIS');
   glSamplePatternSGIS := GLGetProcAddress('glSamplePatternSGIS');

   // GL_EXT_blend_minmax (#37)
   glBlendEquationEXT := GLGetProcAddress('glBlendEquationEXT');

   // GL_EXT_paletted_texture (#78)
   glColorTableEXT := GLGetProcAddress('glColorTableEXT');
   glColorSubTableEXT := GLGetProcAddress('glColorSubTableEXT');
   glGetColorTableEXT := GLGetProcAddress('glGetColorTableEXT');
   glGetColorTableParameterivEXT := GLGetProcAddress('glGetColorTableParameterivEXT');
   glGetColorTableParameterfvEXT := GLGetProcAddress('glGetColorTableParameterfvEXT');

   // GL_EXT_index_material (#94)
   glIndexMaterialEXT := GLGetProcAddress('glIndexMaterialEXT');

   // GL_EXT_index_func (#95)
   glIndexFuncEXT := GLGetProcAddress('glIndexFuncEXT');

   // EXT_compiled_vertex_array (#97)
   glLockArraysEXT := GLGetProcAddress('glLockArraysEXT');
   glUnlockArraysEXT := GLGetProcAddress('glUnlockArraysEXT');
   
   // GL_EXT_draw_range_elements (#112)
   glDrawRangeElementsEXT := GLGetProcAddress('glDrawRangeElementsEXT');

   // GL_EXT_secondary_color (#145)
   glSecondaryColor3bEXT := GLGetProcAddress('glSecondaryColor3bEXT');
   glSecondaryColor3bvEXT := GLGetProcAddress('glSecondaryColor3bvEXT');
   glSecondaryColor3dEXT := GLGetProcAddress('glSecondaryColor3dEXT');
   glSecondaryColor3dvEXT := GLGetProcAddress('glSecondaryColor3dvEXT');
   glSecondaryColor3fEXT := GLGetProcAddress('glSecondaryColor3fEXT');
   glSecondaryColor3fvEXT := GLGetProcAddress('glSecondaryColor3fvEXT');
   glSecondaryColor3iEXT := GLGetProcAddress('glSecondaryColor3iEXT');
   glSecondaryColor3ivEXT := GLGetProcAddress('glSecondaryColor3ivEXT');
   glSecondaryColor3sEXT := GLGetProcAddress('glSecondaryColor3sEXT');
   glSecondaryColor3svEXT := GLGetProcAddress('glSecondaryColor3svEXT');
   glSecondaryColor3ubEXT := GLGetProcAddress('glSecondaryColor3ubEXT');
   glSecondaryColor3ubvEXT := GLGetProcAddress('glSecondaryColor3ubvEXT');
   glSecondaryColor3uiEXT := GLGetProcAddress('glSecondaryColor3uiEXT');
   glSecondaryColor3uivEXT := GLGetProcAddress('glSecondaryColor3uivEXT');
   glSecondaryColor3usEXT := GLGetProcAddress('glSecondaryColor3usEXT');
   glSecondaryColor3usvEXT := GLGetProcAddress('glSecondaryColor3usvEXT');
   glSecondaryColorPointerEXT := GLGetProcAddress('glSecondaryColorPointerEXT');

   // GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArraysEXT := GLGetProcAddress('glMultiDrawArraysEXT');
   glMultiDrawElementsEXT := GLGetProcAddress('glMultiDrawElementsEXT');

   // GL_EXT_fog_coord (#149)
   glFogCoordfEXT := GLGetProcAddress('glFogCoordfEXT'); 
   glFogCoordfvEXT := GLGetProcAddress('glFogCoordfvEXT'); 
   glFogCoorddEXT := GLGetProcAddress('glFogCoorddEXT');
   glFogCoorddvEXT := GLGetProcAddress('glFogCoorddvEXT'); 
   glFogCoordPointerEXT := GLGetProcAddress('glFogCoordPointerEXT'); 

   // GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparateEXT := GLGetProcAddress('glBlendFuncSeparateEXT');

   // GL_NV_vertex_array_range (#190)
   glFlushVertexArrayRangeNV := GLGetProcAddress('glFlushVertexArrayRangeNV'); 
   glVertexArrayRangeNV := GLGetProcAddress('glVertexArrayRangeNV');
   wglAllocateMemoryNV := GLGetProcAddress('wglAllocateMemoryNV'); 
   wglFreeMemoryNV := GLGetProcAddress('wglFreeMemoryNV'); 

   // GL_NV_register_combiners (#191)
   glCombinerParameterfvNV := GLGetProcAddress('glCombinerParameterfvNV'); 
   glCombinerParameterfNV := GLGetProcAddress('glCombinerParameterfNV');
   glCombinerParameterivNV := GLGetProcAddress('glCombinerParameterivNV'); 
   glCombinerParameteriNV := GLGetProcAddress('glCombinerParameteriNV'); 
   glCombinerInputNV := GLGetProcAddress('glCombinerInputNV');
   glCombinerOutputNV := GLGetProcAddress('glCombinerOutputNV'); 
   glFinalCombinerInputNV := GLGetProcAddress('glFinalCombinerInputNV');
   glGetCombinerInputParameterfvNV := GLGetProcAddress('glGetCombinerInputParameterfvNV');
   glGetCombinerInputParameterivNV := GLGetProcAddress('glGetCombinerInputParameterivNV'); 
   glGetCombinerOutputParameterfvNV := GLGetProcAddress('glGetCombinerOutputParameterfvNV');
   glGetCombinerOutputParameterivNV := GLGetProcAddress('glGetCombinerOutputParameterivNV');
   glGetFinalCombinerInputParameterfvNV := GLGetProcAddress('glGetFinalCombinerInputParameterfvNV'); 
   glGetFinalCombinerInputParameterivNV := GLGetProcAddress('glGetFinalCombinerInputParameterivNV');

   // GL_MESA_resize_buffers (#196)
   glResizeBuffersMESA := GLGetProcAddress('glResizeBuffersMESA');

   // GL_3DFX_tbuffer (#208)
   glTbufferMask3DFX := GLGetProcAddress('glTbufferMask3DFX');

   // GL_EXT_multisample (#209)
   glSampleMaskEXT := GLGetProcAddress('glSampleMaskEXT');
   glSamplePatternEXT := GLGetProcAddress('glSamplePatternEXT');

   // GL_SGIS_texture_color_mask (#214)
   glTextureColorMaskSGIS := GLGetProcAddress('glTextureColorMaskSGIS');

   // GL_NV_fence (#222)
   glGenFencesNV := GLGetProcAddress('glGenFencesNV');
   glDeleteFencesNV := GLGetProcAddress('glDeleteFencesNV');
   glSetFenceNV := GLGetProcAddress('glSetFenceNV');
   glTestFenceNV := GLGetProcAddress('glTestFenceNV');
   glFinishFenceNV := GLGetProcAddress('glFinishFenceNV');
   glIsFenceNV := GLGetProcAddress('glIsFenceNV');
   glGetFenceivNV := GLGetProcAddress('glGetFenceivNV');

   // GL_NV_vertex_program (#233)
   glAreProgramsResidentNV := GLGetProcAddress('glAreProgramsResidentNV');
   glBindProgramNV := GLGetProcAddress('glBindProgramNV');
   glDeleteProgramsNV := GLGetProcAddress('glDeleteProgramsNV');
   glExecuteProgramNV := GLGetProcAddress('glExecuteProgramNV');
   glGenProgramsNV := GLGetProcAddress('glGenProgramsNV');
   glGetProgramParameterdvNV := GLGetProcAddress('glGetProgramParameterdvNV');
   glGetProgramParameterfvNV := GLGetProcAddress('glGetProgramParameterfvNV');
   glGetProgramivNV := GLGetProcAddress('glGetProgramivNV');
   glGetProgramStringNV := GLGetProcAddress('glGetProgramStringNV');
   glGetTrackMatrixivNV := GLGetProcAddress('glGetTrackMatrixivNV');
   glGetVertexAttribdvNV:= GLGetProcAddress('glGetVertexAttribdvNV');
   glGetVertexAttribfvNV:= GLGetProcAddress('glGetVertexAttribfvNV');
   glGetVertexAttribivNV:= GLGetProcAddress('glGetVertexAttribivNV');
   glGetVertexAttribPointervNV := GLGetProcAddress ('glGetVertexAttribPointervNV');
   glIsProgramNV := GLGetProcAddress('glIsProgramNV');
   glLoadProgramNV := GLGetProcAddress('glLoadProgramNV');
   glProgramParameter4dNV := GLGetProcAddress('glProgramParameter4dNV');
   glProgramParameter4dvNV := GLGetProcAddress('glProgramParameter4dvNV');
   glProgramParameter4fNV := GLGetProcAddress('glProgramParameter4fNV');
   glProgramParameter4fvNV := GLGetProcAddress('glProgramParameter4fvNV');
   glProgramParameters4dvNV := GLGetProcAddress ('glProgramParameters4dvNV');
   glProgramParameters4fvNV := GLGetProcAddress ('glProgramParameters4fvNV');
   glRequestResidentProgramsNV := GLGetProcAddress ('glRequestResidentProgramsNV');
   glTrackMatrixNV := GLGetProcAddress('glTrackMatrixNV');
   glVertexAttribPointerNV := GLGetProcAddress('glVertexAttribPointerNV');
   glVertexAttrib1dNV := GLGetProcAddress('glVertexAttrib1dNV');
   glVertexAttrib1dvNV := GLGetProcAddress('glVertexAttrib1dvNV');
   glVertexAttrib1fNV := GLGetProcAddress('glVertexAttrib1fNV');
   glVertexAttrib1fvNV := GLGetProcAddress('glVertexAttrib1fvNV');
   glVertexAttrib1sNV := GLGetProcAddress('glVertexAttrib1sNV');
   glVertexAttrib1svNV := GLGetProcAddress('glVertexAttrib1svNV');
   glVertexAttrib2dNV := GLGetProcAddress('glVertexAttrib2dNV');
   glVertexAttrib2dvNV := GLGetProcAddress('glVertexAttrib2dvNV');
   glVertexAttrib2fNV := GLGetProcAddress('glVertexAttrib2fNV');
   glVertexAttrib2fvNV := GLGetProcAddress('glVertexAttrib2fvNV');
   glVertexAttrib2sNV := GLGetProcAddress('glVertexAttrib2sNV');
   glVertexAttrib2svNV := GLGetProcAddress('glVertexAttrib2svNV');
   glVertexAttrib3dNV := GLGetProcAddress('glVertexAttrib3dNV');
   glVertexAttrib3dvNV := GLGetProcAddress('glVertexAttrib3dvNV');
   glVertexAttrib3fNV := GLGetProcAddress('glVertexAttrib3fNV');
   glVertexAttrib3fvNV := GLGetProcAddress('glVertexAttrib3fvNV');
   glVertexAttrib3sNV := GLGetProcAddress('glVertexAttrib3sNV');
   glVertexAttrib3svNV := GLGetProcAddress('glVertexAttrib3svNV');
   glVertexAttrib4dNV := GLGetProcAddress('glVertexAttrib4dNV');
   glVertexAttrib4dvNV := GLGetProcAddress('glVertexAttrib4dvNV');
   glVertexAttrib4fNV := GLGetProcAddress('glVertexAttrib4fNV');
   glVertexAttrib4fvNV := GLGetProcAddress('glVertexAttrib4fvNV');
   glVertexAttrib4sNV := GLGetProcAddress('glVertexAttrib4sNV');
   glVertexAttrib4svNV := GLGetProcAddress('glVertexAttrib4svNV');
   glVertexAttrib4ubvNV := GLGetProcAddress('glVertexAttrib4ubvNV');
   glVertexAttribs1dvNV := GLGetProcAddress('glVertexAttribs1dvNV');
   glVertexAttribs1fvNV := GLGetProcAddress('glVertexAttribs1fvNV');
   glVertexAttribs1svNV := GLGetProcAddress('glVertexAttribs1svNV');
   glVertexAttribs2dvNV := GLGetProcAddress('glVertexAttribs2dvNV');
   glVertexAttribs2fvNV := GLGetProcAddress('glVertexAttribs2fvNV');
   glVertexAttribs2svNV := GLGetProcAddress('glVertexAttribs2svNV');
   glVertexAttribs3dvNV := GLGetProcAddress('glVertexAttribs3dvNV');
   glVertexAttribs3fvNV := GLGetProcAddress('glVertexAttribs3fvNV');
   glVertexAttribs3svNV := GLGetProcAddress('glVertexAttribs3svNV');
   glVertexAttribs4dvNV := GLGetProcAddress('glVertexAttribs4dvNV');
   glVertexAttribs4fvNV := GLGetProcAddress('glVertexAttribs4fvNV');
   glVertexAttribs4svNV := GLGetProcAddress('glVertexAttribs4svNV');
   glVertexAttribs4ubvNV := GLGetProcAddress('glVertexAttribs4ubvN');

   // GL_NV_occlusion_query (#261)
   glGenOcclusionQueriesNV := GLGetProcAddress('glGenOcclusionQueriesNV');
   glDeleteOcclusionQueriesNV := GLGetProcAddress('glDeleteOcclusionQueriesNV');
   glIsOcclusionQueryNV := GLGetProcAddress('glIsOcclusionQueryNV');
   glBeginOcclusionQueryNV := GLGetProcAddress('glBeginOcclusionQueryNV');
   glEndOcclusionQueryNV := GLGetProcAddress('glEndOcclusionQueryNV');
   glGetOcclusionQueryivNV := GLGetProcAddress('glGetOcclusionQueryivNV');
   glGetOcclusionQueryuivNV := GLGetProcAddress('glGetOcclusionQueryuivNV');

   // GL_NV_point_sprite (#262)
   glPointParameteriNV := GLGetProcAddress('glPointParameteriNV');
   glPointParameterivNV := GLGetProcAddress('glPointParameterivNV');

   // GL_EXT_stencil_two_side (#268)
   glActiveStencilFaceEXT := GLGetProcAddress('glActiveStencilFaceEXT');

   // GL_ATI_draw_buffers (#277)
   glDrawBuffersATI := GLGetProcAddress('glDrawBuffersATI');

   // GL_NV_primitive_restart (#285)
   glPrimitiveRestartNV := GLGetProcAddress('glPrimitiveRestartNV');
   glPrimitiveRestartIndexNV := GLGetProcAddress('glPrimitiveRestartIndexNV');
   glPrimitiveRestartIndex := GLGetProcAddress('glPrimitiveRestartIndex');
   if Addr(glPrimitiveRestartIndex) = nil then
    glPrimitiveRestartIndex := glPrimitiveRestartIndexNV;

   // GL_EXT_depth_bounds_test (#297)
   glDepthBoundsEXT := GLGetProcAddress('glDepthBoundsEXT');

   // GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparateEXT := GLGetProcAddress('glBlendEquationSeparateEXT');

   // GL_EXT_framebuffer_object (#310)
   glIsRenderbufferEXT := GLGetProcAddress('glIsRenderbufferEXT');
   glBindRenderbufferEXT := GLGetProcAddress('glBindRenderbufferEXT');
   glDeleteRenderbuffersEXT := GLGetProcAddress('glDeleteRenderbuffersEXT');
   glGenRenderbuffersEXT := GLGetProcAddress('glGenRenderbuffersEXT');
   glRenderbufferStorageEXT := GLGetProcAddress('glRenderbufferStorageEXT');
   glGetRenderbufferParameterivEXT := GLGetProcAddress('glGetRenderbufferParameterivEXT');
   glIsFramebufferEXT := GLGetProcAddress('glIsFramebufferEXT');
   glBindFramebufferEXT := GLGetProcAddress('glBindFramebufferEXT');
   glDeleteFramebuffersEXT := GLGetProcAddress('glDeleteFramebuffersEXT');
   glGenFramebuffersEXT := GLGetProcAddress('glGenFramebuffersEXT');
   glCheckFramebufferStatusEXT := GLGetProcAddress('glCheckFramebufferStatusEXT');
   glFramebufferTexture1DEXT := GLGetProcAddress('glFramebufferTexture1DEXT');
   glFramebufferTexture2DEXT := GLGetProcAddress('glFramebufferTexture2DEXT');
   glFramebufferTexture3DEXT := GLGetProcAddress('glFramebufferTexture3DEXT');
   glFramebufferRenderbufferEXT := GLGetProcAddress('glFramebufferRenderbufferEXT');
   glGetFramebufferAttachmentParameterivEXT := GLGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
   glGenerateMipmapEXT := GLGetProcAddress('glGenerateMipmapEXT');

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT := GLGetProcAddress('glStencilClearTagEXT');

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT := GLGetProcAddress('glBlitFramebufferEXT');

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT := GLGetProcAddress('glRenderbufferStorageMultisampleEXT');

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT := GLGetProcAddress('glGetQueryObjecti64vEXT');
   glGetQueryObjectui64vEXT := GLGetProcAddress('glGetQueryObjectui64vEXT');

   // GL_EXT_gpu_program_parameters (#320)
   glProgramEnvParameters4fvEXT := GLGetProcAddress('glProgramEnvParameters4fvEXT');
   glProgramLocalParameters4fvEXT := GLGetProcAddress('glProgramLocalParameters4fvEXT');

   // GL_NV_geometry_program4 (#323)
   glProgramVertexLimitNV := GLGetProcAddress('glProgramVertexLimitNV');

   // GL_EXT_geometry_shader4 (#324)
   glProgramParameteriEXT := GLGetProcAddress('glProgramParameteriEXT');
   glFramebufferTextureEXT := GLGetProcAddress('glFramebufferTextureEXT');
   glFramebufferTextureLayerEXT := GLGetProcAddress('glFramebufferTextureLayerEXT');
   glFramebufferTextureFaceEXT := GLGetProcAddress('glFramebufferTextureFaceEXT');

   // GL_EXT_gpu_shader4 (#326)
   glVertexAttribI1iEXT := GLGetProcAddress('glVertexAttribI1iEXT');
   glVertexAttribI2iEXT := GLGetProcAddress('glVertexAttribI2iEXT');
   glVertexAttribI3iEXT := GLGetProcAddress('glVertexAttribI3iEXT');
   glVertexAttribI4iEXT := GLGetProcAddress('glVertexAttribI4iEXT');

   glVertexAttribI1uiEXT := GLGetProcAddress('glVertexAttribI1uiEXT');
   glVertexAttribI2uiEXT := GLGetProcAddress('glVertexAttribI2uiEXT');
   glVertexAttribI3uiEXT := GLGetProcAddress('glVertexAttribI3uiEXT');
   glVertexAttribI4uiEXT := GLGetProcAddress('glVertexAttribI4uiEXT');

   glVertexAttribI1ivEXT := GLGetProcAddress('glVertexAttribI1ivEXT');
   glVertexAttribI2ivEXT := GLGetProcAddress('glVertexAttribI2ivEXT');
   glVertexAttribI3ivEXT := GLGetProcAddress('glVertexAttribI3ivEXT');
   glVertexAttribI4ivEXT := GLGetProcAddress('glVertexAttribI4ivEXT');

   glVertexAttribI1uivEXT := GLGetProcAddress('glVertexAttribI1uivEXT');
   glVertexAttribI2uivEXT := GLGetProcAddress('glVertexAttribI2uivEXT');
   glVertexAttribI3uivEXT := GLGetProcAddress('glVertexAttribI3uivEXT');
   glVertexAttribI4uivEXT := GLGetProcAddress('glVertexAttribI4uivEXT');

   glVertexAttribI4bvEXT := GLGetProcAddress('glVertexAttribI4bvEXT');
   glVertexAttribI4svEXT := GLGetProcAddress('glVertexAttribI4svEXT');
   glVertexAttribI4ubvEXT := GLGetProcAddress('glVertexAttribI4ubvEXT');
   glVertexAttribI4usvEXT := GLGetProcAddress('glVertexAttribI4usvEXT');

   glVertexAttribIPointerEXT := GLGetProcAddress('glVertexAttribIPointerEXT');

   glGetVertexAttribIivEXT := GLGetProcAddress('glGetVertexAttribIivEXT');
   glGetVertexAttribIuivEXT := GLGetProcAddress('glGetVertexAttribIuivEXT');

   glUniform1uiEXT := GLGetProcAddress('glUniform1uiEXT');
   glUniform2uiEXT := GLGetProcAddress('glUniform2uiEXT');
   glUniform3uiEXT := GLGetProcAddress('glUniform3uiEXT');
   glUniform4uiEXT := GLGetProcAddress('glUniform4uiEXT');

   glUniform1uivEXT := GLGetProcAddress('glUniform1uivEXT');
   glUniform2uivEXT := GLGetProcAddress('glUniform2uivEXT');
   glUniform3uivEXT := GLGetProcAddress('glUniform3uivEXT');
   glUniform4uivEXT := GLGetProcAddress('glUniform4uivEXT');

   glGetUniformuivEXT := GLGetProcAddress('glGetUniformuivEXT');

   glBindFragDataLocationEXT := GLGetProcAddress('glBindFragDataLocationEXT');
   glGetFragDataLocationEXT := GLGetProcAddress('glGetFragDataLocationEXT');

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT := GLGetProcAddress('glDrawArraysInstancedEXT');
   glDrawElementsInstancedEXT := GLGetProcAddress('glDrawElementsInstancedEXT');

   // GL_EXT_texture_array (#329)
//   glFramebufferTextureLayerEXT:= GLGetProcAddress('glFramebufferTextureLayerEXT');

   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT := GLGetProcAddress('glTexBufferEXT');

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT := GLGetProcAddress('glColorMaskIndexedEXT');
   glGetBooleanIndexedvEXT := GLGetProcAddress('glGetBooleanIndexedvEXT');
   glGetIntegerIndexedvEXT:= GLGetProcAddress('glGetIntegerIndexedvEXT');
   glEnableIndexedEXT:= GLGetProcAddress('glEnableIndexedEXT');
   glDisableIndexedEXT:= GLGetProcAddress('glDisableIndexedEXT');
   glIsEnabledIndexedEXT:= GLGetProcAddress('glIsEnabledIndexedEXT');

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV := GLGetProcAddress('glBindBufferRangeNV');
   glBindBufferOffsetNV := GLGetProcAddress('glBindBufferOffsetNV');
   glBindBufferBaseNV := GLGetProcAddress('glBindBufferBaseNV');
   glTransformFeedbackAttribsNV := GLGetProcAddress('glTransformFeedbackAttribsNV');
   glTransformFeedbackVaryingsNV := GLGetProcAddress('glTransformFeedbackVaryingsNV');
   glBeginTransformFeedbackNV := GLGetProcAddress('glBeginTransformFeedbackNV');
   glEndTransformFeedbackNV := GLGetProcAddress('glEndTransformFeedbackNV');
   glGetVaryingLocationNV := GLGetProcAddress('glGetVaryingLocationNV');
   glGetActiveVaryingNV := GLGetProcAddress('glGetActiveVaryingNV');
   glActiveVaryingNV := GLGetProcAddress('glActiveVaryingNV');
   glGetTransformFeedbackVaryingNV := GLGetProcAddress('glGetTransformFeedbackVaryingNV');

   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT := GLGetProcAddress('glUniformBufferEXT');
   glGetUniformBufferSizeEXT := GLGetProcAddress('glGetUniformBufferSizeEXT');
   glGetUniformOffsetEXT := GLGetProcAddress('glGetUniformOffsetEXT');

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT := GLGetProcAddress('glClearColorIiEXT');
   glClearColorIuiEXT := GLGetProcAddress('glClearColorIuiEXT');
   glTexParameterIivEXT := GLGetProcAddress('glTexParameterIivEXT');
   glTexParameterIuivEXT := GLGetProcAddress('glTexParameterIuivEXT');
   glGetTexParameterIivEXT := GLGetProcAddress('glGetTexParameterIivEXT');
   glGetTexParameterIuivEXT := GLGetProcAddress('glGetTexParameterIuivEXT');

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV := GLGetProcAddress('glBeginConditionalRenderNV');
   glEndConditionalRenderNV := GLGetProcAddress('glEndConditionalRenderNV');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT := GLGetProcAddress('glBindBufferRangeEXT');
   glBindBufferOffsetEXT := GLGetProcAddress('glBindBufferOffsetEXT');
   glBindBufferBaseEXT := GLGetProcAddress('glBindBufferBaseEXT');
   glBeginTransformFeedbackEXT := GLGetProcAddress('glBeginTransformFeedbackEXT');
   glEndTransformFeedbackEXT := GLGetProcAddress('glEndTransformFeedbackEXT');
   glTransformFeedbackVaryingsEXT := GLGetProcAddress('glTransformFeedbackVaryingsEXT');
   glGetTransformFeedbackVaryingEXT:= GLGetProcAddress('glGetTransformFeedbackVaryingEXT');

   // GL_AMD_vertex_shader_tesselator (#363)
   glTessellationFactorAMD := GLGetProcAddress('glTessellationFactorAMD');
   glTessellationModeAMD := GLGetProcAddress('glTessellationModeAMD');

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV := GLGetProcAddress('glCopyImageSubDataNV');

   // GL_NV_shader_buffer_load (#379)
   glMakeBufferResidentNV := GLGetProcAddress('glMakeBufferResidentNV');
   glMakeBufferNonResidentNV := GLGetProcAddress('glMakeBufferNonResidentNV');
   glIsBufferResidentNV := GLGetProcAddress('glIsBufferResidentNV');
   glMakeNamedBufferResidentNV := GLGetProcAddress('glMakeNamedBufferResidentNV');
   glMakeNamedBufferNonResidentNV := GLGetProcAddress('glMakeNamedBufferNonResidentNV');
   glIsNamedBufferResidentNV := GLGetProcAddress('glIsNamedBufferResidentNV');
   glGetBufferParameterui64vNV := GLGetProcAddress('glGetBufferParameterui64vNV');
   glGetNamedBufferParameterui64vNV := GLGetProcAddress('glGetNamedBufferParameterui64vNV');
   glGetIntegerui64vNV := GLGetProcAddress('glGetIntegerui64vNV');
   glUniformui64NV := GLGetProcAddress('glUniformui64NV');
   glUniformui64vNV := GLGetProcAddress('glUniformui64vNV');
   glGetUniformui64vNV := GLGetProcAddress('glGetUniformui64vNV');
   glProgramUniformui64NV := GLGetProcAddress('glProgramUniformui64NV');
   glProgramUniformui64vNV := GLGetProcAddress('glProgramUniformui64vNV');

   // GL_NV_vertex_buffer_unified_memory (#380)
   glBufferAddressRangeNV := GLGetProcAddress('glBufferAddressRangeNV');
   glVertexFormatNV := GLGetProcAddress('glVertexFormatNV');
   glNormalFormatNV := GLGetProcAddress('glNormalFormatNV');
   glColorFormatNV := GLGetProcAddress('glColorFormatNV');
   glIndexFormatNV := GLGetProcAddress('glIndexFormatNV');
   glTexCoordFormatNV := GLGetProcAddress('glTexCoordFormatNV');
   glEdgeFlagFormatNV := GLGetProcAddress('glEdgeFlagFormatNV');
   glSecondaryColorFormatNV := GLGetProcAddress('glSecondaryColorFormatNV');
   glFogCoordFormatNV := GLGetProcAddress('glFogCoordFormatNV');
   glVertexAttribFormatNV := GLGetProcAddress('glVertexAttribFormatNV');
   glVertexAttribIFormatNV := GLGetProcAddress('glVertexAttribIFormatNV');
   glGetIntegerui64i_vNV := GLGetProcAddress('glGetIntegerui64i_vNV');

   // Special Gremedy debugger extensions
   glFrameTerminatorGREMEDY := GLGetProcAddress('glFrameTerminatorGREMEDY');
   glStringMarkerGREMEDY := GLGetProcAddress('glStringMarkerGREMEDY');

{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for Windows OpenGL (WGL) extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   ReadWGLExtensions;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for GLX extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   ReadGLXExtensions;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

end;

{$IFDEF SUPPORT_WGL}
// ReadWGLExtensions
//
procedure ReadWGLExtensions;
begin
   // ARB wgl extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB := GLGetProcAddress('wglCreateBufferRegionARB');
   wglDeleteBufferRegionARB := GLGetProcAddress('wglDeleteBufferRegionARB');
   wglSaveBufferRegionARB := GLGetProcAddress('wglSaveBufferRegionARB');
   wglRestoreBufferRegionARB := GLGetProcAddress('wglRestoreBufferRegionARB');

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB := GLGetProcAddress('wglGetExtensionsStringARB');

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB := GLGetProcAddress('wglGetPixelFormatAttribivARB');
   wglGetPixelFormatAttribfvARB := GLGetProcAddress('wglGetPixelFormatAttribfvARB');
   wglChoosePixelFormatARB := GLGetProcAddress('wglChoosePixelFormatARB');

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB := GLGetProcAddress('wglMakeContextCurrentARB');
   wglGetCurrentReadDCARB := GLGetProcAddress('wglGetCurrentReadDCARB');

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB := GLGetProcAddress('wglCreatePbufferARB');
   wglGetPbufferDCARB := GLGetProcAddress('wglGetPbufferDCARB');
   wglReleasePbufferDCARB := GLGetProcAddress('wglReleasePbufferDCARB');
   wglDestroyPbufferARB := GLGetProcAddress('wglDestroyPbufferARB');
   wglQueryPbufferARB := GLGetProcAddress('wglQueryPbufferARB');

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB := GLGetProcAddress('wglBindTexImageARB');
   wglReleaseTexImageARB := GLGetProcAddress('wglReleaseTexImageARB');
   wglSetPbufferAttribARB := GLGetProcAddress('wglSetPbufferAttribARB');

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB := GLGetProcAddress('wglCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT := GLGetProcAddress('wglSwapIntervalEXT');
   wglGetSwapIntervalEXT := GLGetProcAddress('wglGetSwapIntervalEXT');

   // WGL_NV_gpu_affinity
   wglEnumGpusNV := GLGetProcAddress('wglEnumGpusNV');
   wglEnumGpuDevicesNV := GLGetProcAddress('wglEnumGpuDevicesNV');
   wglCreateAffinityDCNV := GLGetProcAddress('wglCreateAffinityDCNV');
   wglEnumGpusFromAffinityDCNV := GLGetProcAddress('wglEnumGpusFromAffinityDCNV');
   wglDeleteDCNV := GLGetProcAddress('wglDeleteDCNV');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXExtensions
//
procedure ReadGLXExtensions;
begin
   // ARB glx extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved GLX extensions
   //  ###########################################################

   //loading first!
   glXGetProcAddress := GLLibGetProcAddress('glXGetProcAddress');
   glXGetProcAddressARB := GLLibGetProcAddress('glXGetProcAddressARB');

   //GLX 1.3 and later
   glXChooseFBConfig := GLGetProcAddress('glXChooseFBConfig');
   glXGetFBConfigAttrib := GLGetProcAddress('glXGetFBConfigAttrib');
   glXGetFBConfigs := GLGetProcAddress('glXGetFBConfigs');
   glXGetVisualFromFBConfig := GLGetProcAddress('glXGetVisualFromFBConfig');
   glXCreateWindow := GLGetProcAddress('glXCreateWindow');
   glXDestroyWindow := GLGetProcAddress('glXDestroyWindow');
   glXCreatePixmap := GLGetProcAddress('glXCreatePixmap');
   glXDestroyPixmap := GLGetProcAddress('glXDestroyPixmap');
   glXCreatePbuffer := GLGetProcAddress('glXCreatePbuffer');
   glXDestroyPbuffer := GLGetProcAddress('glXDestroyPbuffer');
   glXQueryDrawable := GLGetProcAddress('glXQueryDrawable');
   glXCreateNewContext := GLGetProcAddress('glXCreateNewContext');
   glXMakeContextCurrent := GLGetProcAddress('glXMakeContextCurrent');
   glXGetCurrentReadDrawable := GLGetProcAddress('glXGetCurrentReadDrawable');
   glXQueryContext := GLGetProcAddress('glXQueryContext');
   glXSelectEvent := GLGetProcAddress('glXSelectEvent');
   glXGetSelectedEvent := GLGetProcAddress('glXGetSelectedEvent');
   glXBindTexImageARB := GLGetProcAddress('glXBindTexImageARB');
   glXReleaseTexImageARB := GLGetProcAddress('glXReleaseTexImageARB');
   glxDrawableAttribARB := GLGetProcAddress('glxDrawableAttribARB');

   //GLX 1.4
   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB := GLGetProcAddress('glXCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
    glXSwapIntervalSGI := GLGetProcAddress('glXSwapIntervalSGI');
    glXGetVideoSyncSGI := GLGetProcAddress('glXGetVideoSyncSGI');
    glXWaitVideoSyncSGI := GLGetProcAddress('glXWaitVideoSyncSGI');
    glXFreeContextEXT := GLGetProcAddress('glXFreeContextEXT');
    glXGetContextIDEXT := GLGetProcAddress('glXGetContextIDEXT');
    glXGetCurrentDisplayEXT := GLGetProcAddress('glXGetCurrentDisplayEXT');
    glXImportContextEXT := GLGetProcAddress('glXImportContextEXT');
    glXQueryContextInfoEXT := GLGetProcAddress('glXQueryContextInfoEXT');
    glXCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
    glXCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
    glXReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
    glXSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

    glXBindTexImageEXT := GLGetProcAddress('glXBindTexImageEXT');
    glXReleaseTexImageEXT := GLGetProcAddress('glXReleaseTexImageEXT');

    //GLX 1.4
    glXMakeCurrentReadSGI := GLGetProcAddress('glXMakeCurrentReadSGI');
    glXGetCurrentReadDrawableSGI := GLGetProcAddress('glXGetCurrentReadDrawableSGI');
    glXGetFBConfigAttribSGIX := GLGetProcAddress('glXGetFBConfigAttribSGIX');
    glXChooseFBConfigSGIX := GLGetProcAddress('glXChooseFBConfigSGIX');
    glXCreateGLXPixmapWithConfigSGIX := GLGetProcAddress('glXCreateGLXPixmapWithConfigSGIX');
    glXCreateContextWithConfigSGIX := GLGetProcAddress('glXCreateContextWithConfigSGIX');
    glXGetVisualFromFBConfigSGIX := GLGetProcAddress('glXGetVisualFromFBConfigSGIX');
    glXGetFBConfigFromVisualSGIX := GLGetProcAddress('glXGetFBConfigFromVisualSGIX');
    glXCreateGLXPbufferSGIX := GLGetProcAddress('glXCreateGLXPbufferSGIX');
    glXDestroyGLXPbufferSGIX := GLGetProcAddress('glXDestroyGLXPbufferSGIX');
    glXQueryGLXPbufferSGIX := GLGetProcAddress('glXQueryGLXPbufferSGIX');
    glXSelectEventSGIX := GLGetProcAddress('glXSelectEventSGIX');
    glXGetSelectedEventSGIX := GLGetProcAddress('glXGetSelectedEventSGIX');
    glXCushionSGI := GLGetProcAddress('glXCushionSGI');
    glXBindChannelToWindowSGIX := GLGetProcAddress('glXBindChannelToWindowSGIX');
    glXChannelRectSGIX := GLGetProcAddress('glXChannelRectSGIX');
    glXQueryChannelRectSGIX := GLGetProcAddress('glXQueryChannelRectSGIX');
    glXQueryChannelDeltasSGIX := GLGetProcAddress('glXQueryChannelDeltasSGIX');
    glXChannelRectSyncSGIX := GLGetProcAddress('glXChannelRectSyncSGIX');
    glXJoinSwapGroupSGIX := GLGetProcAddress('glXJoinSwapGroupSGIX');
    glXBindSwapBarrierSGIX := GLGetProcAddress('glXBindSwapBarrierSGIX');
    glXQueryMaxSwapBarriersSGIX := GLGetProcAddress('glXQueryMaxSwapBarriersSGIX');
    glXQueryHyperpipeNetworkSGIX := GLGetProcAddress('glXQueryHyperpipeNetworkSGIX');


    glXHyperpipeConfigSGIX := GLGetProcAddress('glXHyperpipeConfigSGIX');
    glXQueryHyperpipeConfigSGIX := GLGetProcAddress('glXQueryHyperpipeConfigSGIX');
    glXDestroyHyperpipeConfigSGIX := GLGetProcAddress('glXDestroyHyperpipeConfigSGIX');
    glXBindHyperpipeSGIX := GLGetProcAddress('glXBindHyperpipeSGIX');
    glXQueryHyperpipeBestAttribSGIX := GLGetProcAddress('glXQueryHyperpipeBestAttribSGIX');
    glXHyperpipeAttribSGIX := GLGetProcAddress('glXHyperpipeAttribSGIX');
    glXQueryHyperpipeAttribSGIX := GLGetProcAddress('glXQueryHyperpipeAttribSGIX');
    glXGetAGPOffsetMESA := GLGetProcAddress('glXGetAGPOffsetMESA');
    glXEnumerateVideoDevicesNV := GLGetProcAddress('glXEnumerateVideoDevicesNV');
    glXBindVideoDeviceNV := GLGetProcAddress('glXBindVideoDeviceNV');
    GetVideoDeviceNV := GLGetProcAddress('GetVideoDeviceNV');
    glXCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
    glXReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
    glXCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
    glXSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

    glXAllocateMemoryNV := GLGetProcAddress('glXAllocateMemoryNV');
    glXFreeMemoryNV := GLGetProcAddress('glXFreeMemoryNV');

    glXReleaseVideoDeviceNV := GLGetProcAddress('glXReleaseVideoDeviceNV');
    glXBindVideoImageNV := GLGetProcAddress('glXBindVideoImageNV');
    glXReleaseVideoImageNV := GLGetProcAddress('glXReleaseVideoImageNV');
    glXSendPbufferToVideoNV := GLGetProcAddress('glXSendPbufferToVideoNV');
    glXGetVideoInfoNV := GLGetProcAddress('glXGetVideoInfoNV');
    glXJoinSwapGroupNV := GLGetProcAddress('glXJoinSwapGroupNV');
    glXBindSwapBarrierNV := GLGetProcAddress('glXBindSwapBarrierNV');
    glXQuerySwapGroupNV := GLGetProcAddress('glXQuerySwapGroupNV');
    glXQueryMaxSwapGroupsNV := GLGetProcAddress('glXQueryMaxSwapGroupsNV');
    glXQueryFrameCountNV := GLGetProcAddress('glXQueryFrameCountNV');
    glXResetFrameCountNV := GLGetProcAddress('glXResetFrameCountNV');
    glXBindVideoCaptureDeviceNV := GLGetProcAddress('glXBindVideoCaptureDeviceNV');
    glXEnumerateVideoCaptureDevicesNV := GLGetProcAddress('glXEnumerateVideoCaptureDevicesNV');
    glxLockVideoCaptureDeviceNV := GLGetProcAddress('glxLockVideoCaptureDeviceNV');
    glXQueryVideoCaptureDeviceNV := GLGetProcAddress('glXQueryVideoCaptureDeviceNV');
    glXReleaseVideoCaptureDeviceNV := GLGetProcAddress('glXReleaseVideoCaptureDeviceNV');
    glXSwapIntervalEXT := GLGetProcAddress('glXSwapIntervalEXT');
    glXCopyImageSubDataNV := GLGetProcAddress('glXCopyImageSubDataNV');
end;
{$ENDIF}

// TrimAndSplitVersionString
//
procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".
var
  Separator: Integer;
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', Buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < Length(Buffer)) and (AnsiChar(Buffer[Separator - 1]) in ['0'..'9']) and
      (AnsiChar(Buffer[Separator + 1]) in ['0'..'9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator);
      // Find last non-numeric character before version number.
      while (Separator > 0) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Dec(Separator);
      // Delete leading characters which do not belong to the version string.
      Delete(Buffer, 1, Separator);
      Separator := Pos('.', Buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= Length(Buffer)) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(Buffer, Separator, 255);
      // Now translate the numbers.
      Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
      Max := StrToInt(Copy(Buffer, 1, Separator - 1));
      Min := StrToInt(Copy(Buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    Min := 0;
    Max := 0;
  end;
end;

function IsVersionMet(MajorVersion,MinorVersion,actualMajorVersion, actualMinorVersion:Integer): boolean;
begin
  Result:=(actualMajorVersion>MajorVersion)or
          ((actualMajorVersion=MajorVersion)and(actualMinorVersion>=MinorVersion));
end;

// ReadImplementationProperties
//
procedure ReadImplementationProperties;
var
   Buffer : String;
   MajorVersion, MinorVersion: Integer;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // determine OpenGL versions supported
   buffer:=String(glGetString(GL_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GL_VERSION_1_0:=True;
   GL_VERSION_1_1:=IsVersionMet(1,1,majorVersion,minorVersion);
   GL_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GL_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);
   GL_VERSION_1_4:=IsVersionMet(1,4,majorVersion,minorVersion);
   GL_VERSION_1_5:=IsVersionMet(1,5,majorVersion,minorVersion);
   GL_VERSION_2_0:=IsVersionMet(2,0,majorVersion,minorVersion);
   GL_VERSION_2_1:=IsVersionMet(2,1,majorVersion,minorVersion);
   GL_VERSION_3_0:=IsVersionMet(3,0,majorVersion,minorVersion);
   GL_VERSION_3_1:=IsVersionMet(3,1,majorVersion,minorVersion);
   GL_VERSION_3_2:=IsVersionMet(3,2,majorVersion,minorVersion);
   GL_VERSION_3_3:=IsVersionMet(3,3,majorVersion,minorVersion);
   GL_VERSION_4_0:=IsVersionMet(4,0,majorVersion,minorVersion);

   // determine GLU versions met
   buffer:=String(gluGetString(GLU_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GLU_VERSION_1_1:=True; // won't load without at least GLU 1.1
   GLU_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GLU_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);

   // check supported OpenGL extensions
   Buffer := String(glGetString(GL_EXTENSIONS));
   // check ARB approved OpenGL extensions
   GL_ARB_blend_func_extended := CheckExtension('GL_ARB_blend_func_extended');
   GL_ARB_color_buffer_float := CheckExtension('GL_ARB_color_buffer_float');
   GL_ARB_compatibility := CheckExtension('GL_ARB_compatibility');
   GL_ARB_copy_buffer := CheckExtension('GL_ARB_copy_buffer');
   GL_ARB_depth_buffer_float := CheckExtension('GL_ARB_depth_buffer_float');
   GL_ARB_depth_clamp := CheckExtension('GL_ARB_depth_clamp');
   GL_ARB_depth_texture := CheckExtension('GL_ARB_depth_texture');
   GL_ARB_draw_buffers := CheckExtension('GL_ARB_draw_buffers');
   GL_ARB_draw_buffers_blend := CheckExtension('GL_ARB_draw_buffers_blend');
   GL_ARB_draw_elements_base_vertex := CheckExtension('GL_ARB_draw_elements_base_vertex');
   GL_ARB_draw_indirect := CheckExtension('GL_ARB_draw_indirect');
   GL_ARB_draw_instanced := CheckExtension('GL_ARB_draw_instanced');
   GL_ARB_explicit_attrib_location := CheckExtension('GL_ARB_explicit_attrib_location');
   GL_ARB_fragment_coord_conventions := CheckExtension('GL_ARB_fragment_coord_conventions');
   GL_ARB_fragment_program := CheckExtension('GL_ARB_fragment_program');
   GL_ARB_fragment_program_shadow := CheckExtension('GL_ARB_fragment_program_shadow');
   GL_ARB_fragment_shader := CheckExtension('GL_ARB_fragment_shader');
   GL_ARB_framebuffer_object := CheckExtension('GL_ARB_framebuffer_object');
   GL_ARB_framebuffer_sRGB := CheckExtension('GL_ARB_framebuffer_sRGB');
   GL_ARB_geometry_shader4 := CheckExtension('GL_ARB_geometry_shader4');
   GL_ARB_gpu_shader_fp64 := CheckExtension('GL_ARB_gpu_shader_fp64');
   GL_ARB_gpu_shader5 := CheckExtension('GL_ARB_gpu_shader5');
   GL_ARB_half_float_pixel := CheckExtension('GL_ARB_half_float_pixel');
   GL_ARB_half_float_vertex := CheckExtension('GL_ARB_half_float_vertex');
   GL_ARB_imaging := CheckExtension('GL_ARB_imaging');
   GL_ARB_instanced_arrays := CheckExtension('GL_ARB_instanced_arrays');
   GL_ARB_map_buffer_range := CheckExtension('GL_ARB_map_buffer_range');
   GL_ARB_matrix_palette  := CheckExtension('GL_ARB_matrix_palette');
   GL_ARB_multisample := CheckExtension(' GL_ARB_multisample'); // ' ' to avoid collision with WGL variant
   GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture');
   GL_ARB_occlusion_query := CheckExtension('GL_ARB_occlusion_query');
   GL_ARB_occlusion_query2 := CheckExtension('GL_ARB_occlusion_query2');
   GL_ARB_pixel_buffer_object := CheckExtension('GL_ARB_pixel_buffer_object');
   GL_ARB_point_parameters := CheckExtension('GL_ARB_point_parameters');
   GL_ARB_point_sprite := CheckExtension('GL_ARB_point_sprite');
   GL_ARB_provoking_vertex := CheckExtension('GL_ARB_provoking_vertex');
   GL_ARB_sample_shading := CheckExtension('GL_ARB_sample_shading');
   GL_ARB_sampler_objects := CheckExtension('GL_ARB_sampler_objects');
   GL_ARB_seamless_cube_map := CheckExtension('GL_ARB_seamless_cube_map');
   GL_ARB_shader_bit_encoding := CheckExtension('GL_ARB_shader_bit_encoding');
   GL_ARB_shader_objects := CheckExtension('GL_ARB_shader_objects');
   GL_ARB_shader_subroutine := CheckExtension('GL_ARB_shader_subroutine');
   GL_ARB_shader_texture_lod := CheckExtension('GL_ARB_shader_texture_lod');
   GL_ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
   GL_ARB_shadow := CheckExtension('GL_ARB_shadow');
   GL_ARB_shadow_ambient := CheckExtension('GL_ARB_shadow_ambient');
   GL_ARB_sync := CheckExtension('GL_ARB_sync');
   GL_ARB_tessellation_shader := CheckExtension('GL_ARB_tessellation_shader');
   GL_ARB_texture_border_clamp := CheckExtension('GL_ARB_texture_border_clamp');
   GL_ARB_texture_buffer_object := CheckExtension('GL_ARB_texture_buffer_object');
   GL_ARB_texture_buffer_object_rgb32 := CheckExtension('GL_ARB_texture_buffer_object_rgb32');
   GL_ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
   GL_ARB_texture_compression_rgtc := CheckExtension('GL_ARB_texture_compression_rgtc');
   GL_ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
   GL_ARB_texture_cube_map_array := CheckExtension('GL_ARB_texture_cube_map_array');
   GL_ARB_texture_env_add := CheckExtension('GL_ARB_texture_env_add');
   GL_ARB_texture_env_combine := CheckExtension('GL_ARB_texture_env_combine');
   GL_ARB_texture_env_crossbar := CheckExtension('GL_ARB_texture_env_crossbar');
   GL_ARB_texture_env_dot3 := CheckExtension('GL_ARB_texture_env_dot3');
   GL_ARB_texture_float := CheckExtension('GL_ARB_texture_float');
   GL_ARB_texture_gather := CheckExtension('GL_ARB_texture_gather');
   GL_ARB_texture_mirrored_repeat := CheckExtension('GL_ARB_texture_mirrored_repeat');
   GL_ARB_texture_multisample := CheckExtension('GL_ARB_texture_multisample');
   GL_ARB_texture_non_power_of_two := CheckExtension('GL_ARB_texture_non_power_of_two');
   GL_ARB_texture_query_lod := CheckExtension('GL_ARB_texture_query_lod');
   GL_ARB_texture_rectangle := CheckExtension('GL_ARB_texture_rectangle');
   GL_ARB_texture_rg := CheckExtension('GL_ARB_texture_rg');
   GL_ARB_texture_rgb10_a2ui := CheckExtension('GL_ARB_texture_rgb10_a2ui');
   GL_ARB_texture_swizzle := CheckExtension('GL_ARB_texture_swizzle');
   GL_ARB_timer_query := CheckExtension('GL_ARB_timer_query');
   GL_ARB_transform_feedback2 := CheckExtension('GL_ARB_transform_feedback2');
   GL_ARB_transform_feedback3 := CheckExtension('GL_ARB_transform_feedback3');
   GL_ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
   GL_ARB_uniform_buffer_object := CheckExtension('GL_ARB_uniform_buffer_object');
   GL_ARB_vertex_array_bgra := CheckExtension('GL_ARB_vertex_array_bgra');
   GL_ARB_vertex_array_object := CheckExtension('GL_ARB_vertex_array_object');
   GL_ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');
   GL_ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
   GL_ARB_vertex_program := CheckExtension('GL_ARB_vertex_program');
   GL_ARB_vertex_shader := CheckExtension('GL_ARB_vertex_shader');
   GL_ARB_vertex_type_2_10_10_10_rev := CheckExtension('GL_ARB_vertex_type_2_10_10_10_rev');
   GL_ARB_window_pos := CheckExtension('GL_ARB_window_pos');
   GL_ARB_texture_compression_bptc := CheckExtension('GL_ARB_texture_compression_bptc');

   // check Vendor/EXT OpenGL extensions
   GL_3DFX_multisample := CheckExtension('GL_3DFX_multisample');
   GL_3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
   GL_3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');
   GL_ATI_draw_buffers := CheckExtension('GL_ATI_draw_buffers');
   GL_ATI_texture_compression_3dc := CheckExtension('GL_ATI_texture_compression_3dc');
   GL_ATI_texture_float := CheckExtension('GL_ATI_texture_float');
   GL_ATI_texture_mirror_once := CheckExtension('GL_ATI_texture_mirror_once');

   GL_S3_s3tc := CheckExtension('GL_S3_s3tc');

   GL_EXT_abgr := CheckExtension('GL_EXT_abgr');
   GL_EXT_bgra := CheckExtension('GL_EXT_bgra');
   GL_EXT_bindable_uniform := CheckExtension('GL_EXT_bindable_uniform');   
   GL_EXT_blend_color := CheckExtension('GL_EXT_blend_color');
   GL_EXT_blend_equation_separate := CheckExtension('GL_EXT_blend_equation_separate');
   GL_EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
   GL_EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
   GL_EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
   GL_EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
   GL_EXT_Cg_shader := CheckExtension('GL_EXT_Cg_shader');
   GL_EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
   GL_EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
   GL_EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
   GL_EXT_depth_bounds_test := CheckExtension('GL_EXT_depth_bounds_test');
   GL_EXT_draw_buffers2 := CheckExtension('GL_EXT_draw_buffers2');
   GL_EXT_draw_instanced := CheckExtension('GL_EXT_draw_instanced');
   GL_EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
   GL_EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
   GL_EXT_framebuffer_blit := CheckExtension('GL_EXT_framebuffer_blit');
   GL_EXT_framebuffer_multisample := CheckExtension('GL_EXT_framebuffer_multisample');
   GL_EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
   GL_EXT_framebuffer_sRGB := CheckExtension('GL_EXT_framebuffer_sRGB');
   GL_EXT_geometry_shader4 := CheckExtension('GL_EXT_geometry_shader4');
   GL_EXT_gpu_program_parameters := CheckExtension('GL_EXT_gpu_program_parameters');
   GL_EXT_gpu_shader4 := CheckExtension('GL_EXT_gpu_shader4');
   GL_EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
   GL_EXT_multisample := CheckExtension('GL_EXT_multisample');
   GL_EXT_packed_depth_stencil := CheckExtension('GL_EXT_packed_depth_stencil');
   GL_EXT_packed_float := CheckExtension('GL_EXT_packed_float');
   GL_EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
   GL_EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
   GL_EXT_pixel_buffer_object := CheckExtension('GL_EXT_pixel_buffer_object');
   GL_EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
   GL_EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
   GL_EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
   GL_EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
   GL_EXT_shadow_funcs := CheckExtension('GL_EXT_shadow_funcs');
   GL_EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
   GL_EXT_stencil_clear_tag := CheckExtension('GL_EXT_stencil_clear_tag');
   GL_EXT_stencil_two_side := CheckExtension('EXT_stencil_two_side');
   GL_EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
   GL_EXT_texture3D := CheckExtension('GL_EXT_texture3D');
   GL_EXT_texture_array := CheckExtension('GL_EXT_texture_array');
   GL_EXT_texture_buffer_object := CheckExtension('GL_EXT_texture_buffer_object');
   GL_EXT_texture_compression_latc := CheckExtension('GL_EXT_texture_compression_latc');
   GL_EXT_texture_compression_rgtc := CheckExtension('GL_EXT_texture_compression_rgtc');
   GL_EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
   GL_EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
   GL_EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
   GL_EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
   GL_EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
   GL_EXT_texture_env_dot3 := CheckExtension('GL_EXT_texture_env_dot3');
   GL_EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic');
   GL_EXT_texture_integer := CheckExtension('GL_EXT_texture_integer');
   GL_EXT_texture_lod := CheckExtension('GL_EXT_texture_lod');
   GL_EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
   GL_EXT_texture_mirror_clamp := CheckExtension('GL_EXT_texture_mirror_clamp');
   GL_EXT_texture_object := CheckExtension('GL_EXT_texture_object');
   GL_EXT_texture_rectangle := CheckExtension('GL_EXT_texture_rectangle');
   GL_EXT_texture_sRGB := CheckExtension('GL_EXT_texture_sRGB');
   GL_EXT_texture_shared_exponent := CheckExtension('GL_EXT_texture_shared_exponent');
   GL_EXT_timer_query := CheckExtension('GL_EXT_timer_query');
   GL_EXT_transform_feedback := CheckExtension('GL_EXT_transform_feedback');
   GL_EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');

   GL_HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');

   GL_IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');

   GL_KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');

   GL_MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');

   GL_NV_blend_square := CheckExtension('GL_NV_blend_square');
   GL_NV_conditional_render := CheckExtension('GL_NV_conditional_render');
   GL_NV_copy_image := CheckExtension('GL_NV_copy_image');
   GL_NV_depth_buffer_float := CheckExtension('GL_NV_depth_buffer_float');
   GL_NV_fence := CheckExtension('GL_NV_fence');
   GL_NV_float_buffer := CheckExtension('GL_NV_float_buffer');
   GL_NV_fog_distance := CheckExtension('GL_NV_fog_distance');
   GL_NV_geometry_program4 := CheckExtension('GL_NV_geometry_program4');
   GL_NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
   GL_NV_multisample_filter_hint  := CheckExtension('GL_NV_multisample_filter_hint');
   GL_NV_occlusion_query := CheckExtension('GL_NV_occlusion_query');
   GL_NV_point_sprite := CheckExtension('GL_NV_point_sprite');
   GL_NV_primitive_restart := CheckExtension('GL_NV_primitive_restart');
   GL_NV_register_combiners := CheckExtension('GL_NV_register_combiners');
   GL_NV_shader_buffer_load := CheckExtension('GL_NV_shader_buffer_load');
   GL_NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
   GL_NV_texture_compression_vtc := CheckExtension('GL_NV_texture_compression_vtc');
   GL_NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
   GL_NV_texture_rectangle := CheckExtension('GL_NV_texture_rectangle');
   GL_NV_texture_shader := CheckExtension('GL_NV_texture_shader');
   GL_NV_texture_shader2 := CheckExtension('GL_NV_texture_shader2');
   GL_NV_texture_shader3 := CheckExtension('GL_NV_texture_shader3');
   GL_NV_transform_feedback := CheckExtension('GL_NV_transform_feedback');
   GL_NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
   GL_NV_vertex_array_range2 := CheckExtension('GL_NV_vertex_array_range2');
   GL_NV_vertex_buffer_unified_memory := CheckExtension('GL_NV_vertex_buffer_unified_memory');
   GL_NV_vertex_program := CheckExtension('GL_NV_vertex_program');

   GL_SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');

   GL_SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
   GL_SGIS_multisample := CheckExtension('GL_SGIS_multisample');
   GL_SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
   GL_SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
   GL_SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
   GL_SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');

   GL_SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
   GL_SGIX_shadow := CheckExtension('GL_SGIX_shadow'); 
   GL_SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');

   GL_AMD_vertex_shader_tessellator := CheckExtension('GL_AMD_vertex_shader_tessellator');

   GL_WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');

   GL_GREMEDY_frame_terminator := CheckExtension('GL_GREMEDY_frame_terminator');
   GL_GREMEDY_string_marker := CheckExtension('GL_GREMEDY_string_marker');

   // check supported GLU extensions
   Buffer := String(gluGetString(GLU_EXTENSIONS));
   GLU_EXT_nurbs_tessellator := CheckExtension('GLU_EXT_nurbs_tessellator');
   GLU_EXT_object_space_tess := CheckExtension('GLU_EXT_object_space_tess');
   GLU_EXT_TEXTURE := CheckExtension('GLU_EXT_TEXTURE');

   {$IFDEF SUPPORT_WGL}
   //check supported WGL extensions
   ReadWGLImplementationProperties;
   {$ENDIF}

   {$IFDEF SUPPORT_GLX}
   //check supported GLX extensions
   ReadGLXImplementationProperties;
   {$ENDIF}
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties
//
procedure ReadWGLImplementationProperties;
var
   Buffer: string;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // ARB wgl extensions
   if Assigned(wglGetExtensionsStringARB) then
      Buffer:=String(wglGetExtensionsStringARB(wglGetCurrentDC))
   else Buffer:='';
   WGL_ARB_buffer_region:=CheckExtension('WGL_ARB_buffer_region');
   WGL_ARB_create_context := CheckExtension('WGL_ARB_create_context');
   WGL_ARB_create_context_profile := CheckExtension('WGL_ARB_create_context_profile');
   WGL_ARB_extensions_string:=CheckExtension('WGL_ARB_extensions_string');
   WGL_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
   WGL_ARB_make_current_read:=CheckExtension('WGL_ARB_make_current_read');
   WGL_ARB_multisample:=CheckExtension('WGL_ARB_multisample');
   WGL_ARB_pbuffer:=CheckExtension('WGL_ARB_pbuffer');
   WGL_ARB_pixel_format:=CheckExtension('WGL_ARB_pixel_format');
   WGL_ARB_pixel_format_float:=CheckExtension('WGL_ARB_pixel_format_float');
   WGL_ARB_render_texture:=CheckExtension('WGL_ARB_render_texture');
   // Vendor/EXT wgl extensions
   WGL_ATI_pixel_format_float := CheckExtension('WGL_ATI_pixel_format_float');
   WGL_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
   WGL_EXT_pixel_format_packed_float := CheckExtension('WGL_EXT_pixel_format_packed_float');
   WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
   WGL_NV_gpu_affinity := CheckExtension('WGL_NV_gpu_affinity');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties
//
procedure ReadGLXImplementationProperties;
var
   Buffer: string;
   MajorVersion, MinorVersion: Integer;
   Dpy: PDisplay;
   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;
begin
   Dpy:=glXGetCurrentDisplay();
   buffer:=String(glXQueryServerString(Dpy, XDefaultScreen(Dpy), GLX_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GLX_VERSION_1_1:=IsVersionMet(1,1,majorVersion,minorVersion);
   GLX_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GLX_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);
   GLX_VERSION_1_4:=IsVersionMet(1,4,majorVersion,minorVersion);

   // This procedure will probably need changing, as totally untested
   // This might only work if GLX functions/procedures are loaded dynamically
   if Assigned(glXQueryExtensionsString) then
     Buffer := glXQueryExtensionsString(Dpy, 0)  //guess at a valid screen
   else
     Buffer:='';
   GLX_ARB_create_context := CheckExtension('GLX_ARB_create_context');
   GLX_ARB_create_context_profile := CheckExtension('GLX_ARB_create_context_profile');
   GLX_ARB_framebuffer_sRGB := CheckExtension('GLX_ARB_framebuffer_sRGB');
   GLX_EXT_framebuffer_sRGB := CheckExtension('GLX_EXT_framebuffer_sRGB');
   GLX_EXT_fbconfig_packed_float := CheckExtension('GLX_EXT_fbconfig_packed_float');
   GLX_SGI_swap_control := CheckExtension('GLX_SGI_swap_control');
   GLX_ARB_multisample := CheckExtension('GLX_ARB_multisample');

   GLX_SGIS_multisample	 := CheckExtension('GLX_SGIS_multisample');
   GLX_EXT_visual_info	 := CheckExtension('GLX_EXT_visual_info');
   GLX_SGI_video_sync := CheckExtension('GLX_SGI_video_sync');
   GLX_SGI_make_current_read := CheckExtension('GLX_SGI_make_current_read');
   GLX_SGIX_video_source := CheckExtension('GLX_SGIX_video_source');
   GLX_EXT_visual_rating := CheckExtension('GLX_EXT_visual_rating');
   GLX_EXT_import_context := CheckExtension('GLX_EXT_import_context');
   GLX_SGIX_fbconfig := CheckExtension('GLX_SGIX_fbconfig');
   GLX_SGIX_pbuffer := CheckExtension('GLX_SGIX_pbuffer');
   GLX_SGI_cushion := CheckExtension('GLX_SGI_cushion');
   GLX_SGIX_video_resize := CheckExtension('GLX_SGIX_video_resize');
   GLX_SGIX_dmbuffer := CheckExtension('GLX_SGIX_dmbuffer');
   GLX_SGIX_swap_group := CheckExtension('GLX_SGIX_swap_group');
   GLX_SGIX_swap_barrier := CheckExtension('GLX_SGIX_swap_barrier');
   GLX_SGIS_blended_overlay := CheckExtension('GLX_SGIS_blended_overlay');
   GLX_SGIS_shared_multisample	 := CheckExtension('GLX_SGIS_shared_multisample');
   GLX_SUN_get_transparent_index := CheckExtension('GLX_SUN_get_transparent_index');
   GLX_3DFX_multisample	 := CheckExtension('GLX_3DFX_multisample');
   GLX_MESA_copy_sub_buffer := CheckExtension('GLX_MESA_copy_sub_buffer');
   GLX_MESA_pixmap_colormap := CheckExtension('GLX_MESA_pixmap_colormap');
   GLX_MESA_release_buffers := CheckExtension('GLX_MESA_release_buffers');
   GLX_MESA_set_3dfx_mode := CheckExtension('GLX_MESA_set_3dfx_mode');
   GLX_SGIX_visual_select_group	 := CheckExtension('GLX_SGIX_visual_select_group');
   GLX_SGIX_hyperpipe  := CheckExtension('GLX_SGIX_hyperpipe');
end;
{$ENDIF}

// CloseOpenGL
//
procedure CloseOpenGL;
begin
   if GLHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(Cardinal(GLHandle));
      GLHandle:=INVALID_MODULEHANDLE;
   end;

   if GLUHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(Cardinal(GLUHandle));
      GLUHandle:=INVALID_MODULEHANDLE;
   end;
end;

// InitOpenGL
//
function InitOpenGL : Boolean;
begin
   if (GLHandle=INVALID_MODULEHANDLE) or (GLUHandle=INVALID_MODULEHANDLE) then
      Result:=InitOpenGLFromLibrary(opengl32, glu32)
   else Result:=True;
end;

// InitOpenGLFromLibrary
//
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
begin
   Result := False;
   CloseOpenGL;

   GLHandle:=LoadLibrary(PChar(GLName));
   GLUHandle:=LoadLibrary(PChar(GLUName));

   if (GLHandle<>INVALID_MODULEHANDLE) and (GLUHandle<>INVALID_MODULEHANDLE) then
     Result:=True
   else begin
      if GLHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(Cardinal(GLHandle));
      if GLUHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(Cardinal(GLUHandle));
   end;
end;

// IsOpenGLInitialized
//
function IsOpenGLInitialized: Boolean;
begin
   Result:=(GLHandle<>INVALID_MODULEHANDLE);
end;

// compatibility routines

// UnloadOpenGL
//
procedure UnloadOpenGL;
begin
   CloseOpenGL;
end;

// LoadOpenGL
//
function LoadOpenGL: Boolean;
begin
   Result := InitOpenGL;
end;

// LoadOpenGLFromLibrary
//
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
begin
   Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

// IsOpenGLLoaded
//
function IsOpenGLLoaded: Boolean;
begin
  Result := IsOpenGLInitialized();
end;

// IsMesaGL
//
function IsMesaGL : Boolean;
begin
  Result:=GLGetProcAddress('glResizeBuffersMESA')<>nil;
end;

// IsOpenGLVersionMet
//
function IsOpenGLVersionMet(MajorVersion, MinorVersion: Integer): boolean;
var
  Buffer : String;
  GLMajorVersion, GLMinorVersion: Integer;
begin
  buffer:=String(glGetString(GL_VERSION));
  TrimAndSplitVersionString(buffer, GLMajorVersion, GLMinorVersion);
  Result:=IsVersionMet(MajorVersion,MinorVersion,GLMajorVersion,GLMinorVersion);
end;

initialization

   Set8087CW($133F);

finalization

   CloseOpenGL;

end.

